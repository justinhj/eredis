;; eredis... A simple emacs interface to redis
;; See for info on the protocol http://redis.io/topics/protocol

;; (C)2011 Justin Heyes-Jones
;; This is released under the Gnu License v3. See http://www.gnu.org/licenses/gpl.txt

;; I addded support for editing and viewing keys as an org-table
(require 'org-table)

(defvar *redis-process* nil "Current Redis client process")
(defvar *redis-state* nil "Statue of the connection")
(defvar *redis-response* nil "Stores response of last Redis command")
(defvar *redis-timeout* 15 "Timeout on the client in seconds when waiting for Redis")

;; UTILS

(defun eredis-set-timeout(seconds)
  "set how long emacs will wait for a response from redit, pay attention to this if using blocking 
commands like blpop which also have a timeout" 
  (setf *redis-timeout* seconds))

(defun two-lists-to-map(lst1 lst2)
  "take a list of keys LST1 and a list of values LST2 and make a hashmap"
  (let ((retmap (make-hash-table :test 'equal)))
    (mapc (lambda (n) (puthash (car n) (cdr n) retmap))
	  (map 'list (lambda (a b) (cons a b)) lst1 lst2))
    retmap))

(defun flatten-map(m)
  "flatten the key values of map M to a list of the form key1 value1 key2 value2..."
  (let ((key-values nil))
    (maphash (lambda (k v)
	       (push k key-values)
	       (push v key-values))
	     m)
    (reverse key-values)))

(defun eredis-parse-map-or-list-arg(a)
  "handle when an argument can be passed as a hash table or a list of key values"
  (if (hash-table-p a)
      (flatten-map a)
    a))

;; helper function from http://www.emacswiki.org/emacs/ElispCookbook#toc5
(defun chomp (str)
  "Remove leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun insert-map(m)
  "insert a map M of key value pairs into the current buffer"
  (maphash (lambda (a b) (insert (format "%s,%s\n" a b))) m))

(defun convert-numbers-to-strings(item)
  (if (numberp item)
      (number-to-string item)
    item))

(defun eredis-construct-unified-request(command &rest arguments)
  "all redis commands are sent using this protocol"
  (let ((num-args (+ 1 (length arguments))))
    (if (> num-args 0)
	(let ((req (format "*%d\r\n$%d\r\n%s\r\n" num-args (length command) command)))
	  (dolist (item arguments)
	    (setf item (convert-numbers-to-strings item))
	    (setf req (concat req (format "$%d\r\n%s\r\n" (length item) item))))
	  req)
      nil)))

(defun eredis-map-keys(key-expr)
  "take a glob expression like \"user.id.*\" and return the key/values of matching keys"
  (let ((keys (eredis-keys key-expr)))
    (if keys
	(let ((values (eredis-mget keys)))
	  (two-lists-to-map keys values))
      nil)))

(defun eredis-get-response()
  "await response from redis and store it in *redis-response*. If it times out it will return nil"
  (if (accept-process-output *redis-process* *redis-timeout* 0 t)
      *redis-response*
    nil))

(defun eredis-parse-multi-bulk(resp)
  "parse the redis multi bulk response RESP and return the list of results. handles null entries when
length is -1 as per spec"
  (if (null resp) ;; resp is nil if timeout or other problem
      nil
    (if (= ?- (string-to-char resp))
	(error "redis error: %s" (eredis-trim-status-response resp))
      (let ((num-values (string-to-number (subseq resp 1))))
	(if (<= num-values 0)
	    nil
	  (let ((return-list nil)
		(parse-pos (string-match "^\\$" resp)))
	    (dotimes (n num-values)
	      (let ((len (string-to-number (subseq resp (1+ parse-pos)))))
		(string-match "\r\n" resp parse-pos)
		(setf parse-pos (match-end 0))
		(if (= len -1)
		    (setf return-list (cons nil return-list))
		  (unless (and parse-pos (> parse-pos 0))
		    (error "parse error"))
		  (setf return-list (cons (subseq resp parse-pos (+ len parse-pos)) return-list)))
		(setf parse-pos (string-match "^\\$" resp parse-pos))))
	    (reverse return-list)))))))

(defun eredis-command-returning-multibulk(command &rest args)
  "Send a COMMAND that has the multi bulk return type and return a list to the user"
  (if (and *redis-process* (eq *redis-state* 'open))
      (progn 
	(process-send-string *redis-process* (apply #'eredis-construct-unified-request command args))
	(let ((resp (eredis-get-response)))
	  (eredis-parse-multi-bulk resp)))))

(defun eredis-parse-bulk(resp)
  "parse the redis bulk response RESP and return the result"
  (if (null resp)
      nil
    (if (= ?$ (string-to-char resp))
	(let ((count (string-to-number (subseq resp 1))))
	  (if (and (> count 0)
		   (string-match "\r\n" resp))
	      (let ((body-start (match-end 0)))
		(when body-start
		  (subseq resp body-start (+ count body-start))))
	    nil))
      nil)))

(defun eredis-command-returning-bulk(command &rest args)
  "Send a COMMAND that has the bulk return type and return it to the user"
  (if (and *redis-process* (eq *redis-state* 'open))
      (progn 
	(process-send-string *redis-process* (apply #'eredis-construct-unified-request command args))
	(let ((resp (eredis-get-response)))
	  (eredis-parse-bulk resp)))))

(defun eredis-buffer-message(process message)
  "print a message to the redis process buffer"
  (save-excursion 
    (set-buffer (process-buffer process))
    (insert message)))

(defun eredis-sentinel(process event)
  "sentinel function for redis network process which monitors for events"
  (eredis-buffer-message process (format "sentinel event %s" event))
  (cond 
   ((string-match "open" event)
    (setq *redis-state* 'open))))

(defun eredis-filter(process string)
  "filter function for redis network process, which receives output"
  (setq *redis-response* string))

(defun eredis-delete-process()
  (when *redis-process*
    (delete-process *redis-process*)
    (setq *redis-state* 'closed)))

;; Connect and disconnect functionality

(defun eredis-hai(host port &optional no-wait)
  (interactive "sHost: \nsPort: \n")
  (eredis-delete-process)
  (setq *redis-state* 'opening)
  (let ((p 
	 (make-network-process :name "redis"
			       :host host
			       :service port
			       :nowait no-wait
			       :filter #'eredis-filter
			       :keepalive t
			       :linger t
			       :sentinel #'eredis-sentinel
			       :buffer (get-buffer-create "*redis*"))))
    (if p
	(progn
	  ;; When doing a blocking connect set the state to
	  ;; open. A non-nlocking connect will set the state 
	  ;; to open when the connection calls the sentinel
	  (if (null no-wait)
	      (progn
		(when (called-interactively-p)
		  (message "Redis connected"))
		(setf *redis-state* 'open)))
	  (setf *redis-process* p)))))
     
(defun eredis-kthxbye()
  "Close the connection to Redis"
  (interactive)
  (eredis-delete-process))

;; todo handle -blah error 

(defun eredis-status-response-success-p(resp)
  (= ?+ (string-to-char resp)))

(defun eredis-trim-status-response(resp)
  "strip the leading character +/- and the final carriage returns"
  (let ((len (length resp)))
    (subseq resp 1 (- len 2))))

(defun eredis-parse-integer-response(resp)
  "parse integer response type"
  (string-to-number (subseq resp 1)))

(defun eredis-command-returning-integer(command &rest args)
  "Send a command that has the integer return type"
  (if (and *redis-process* (eq *redis-state* 'open))
      (progn 
	(process-send-string *redis-process* (apply #'eredis-construct-unified-request command args))
	(let ((resp (eredis-get-response)))
	  (eredis-parse-integer-response resp)))
    nil))

(defun eredis-command-returning-status(command &rest args)
  "Send a command that has the status code return type"
  (if (and *redis-process* (eq *redis-state* 'open))
      (progn 
	(process-send-string *redis-process* (apply #'eredis-construct-unified-request command args))
	(let ((resp (eredis-get-response)))
	  (let ((ret-val (eredis-trim-status-response resp)))
	    (if (eredis-status-response-success-p resp)
		(progn
		  (when (called-interactively-p)
		    (message ret-val))
		  ret-val)
	      (error "redis error: %s" ret-val)))))
    nil))

(defun eredis-get-map(keys)
  "given a map M of key/value pairs, go to Redis to retrieve the values and set the 
value to whatever it is in Redis (or nil if not found)"
  (let ((num-args (1+ (hash-table-count m))))
    (let ((command (format "*%d\r\n$4\r\nMGET\r\n" num-args))
	  (key-value-string "")))
      (maphash (lambda (k v)
		 (setf key-value-string (concat key-value-string (format "$%d\r\n%s\r\n" (length k) k))))
	       m)
      (process-send-string *redis-process* (concat command key-value-string))
      (eredis-get-response)))

;; key commands 

(defun eredis-del(key &rest keys)
  (apply #'eredis-command-returning-integer "del" key keys))  

(defun eredis-exists(key)
  "Returns 1 if key exists and 0 otherwise"
  (eredis-command-returning-integer "exists" key))

(defun eredis-expire(key seconds)
  "Set timeout on KEY to SECONDS and returns 1 if it succeeds 0 otherwise"
  (eredis-command-returning-integer "expire" key seconds))

(defun eredis-expireat(key unix-time)
  "Set timeout on KEY to SECONDS and returns 1 if it succeeds 0 otherwise"
  (eredis-command-returning-integer "expireat" key unix-time))

; http://redis.io/commands/keys

(defun eredis-keys(pattern)
  "returns a list of keys where the key matches the provided
pattern. see the link for the style of patterns"
  (eredis-command-returning-multibulk "keys" pattern))

(defun eredis-move(key db)
  "moves KEY to DB and returns 1 if it succeeds 0 otherwise"
  (eredis-command-returning-integer "move" key db))

;; http://redis.io/commands/object

(defun eredis-object(subcommand &rest args)
  "inspect the internals of Redis Objects associated with keys, best see the docs fo;r this one"
  (if (eq t (compare-strings "encoding" nil nil subcommand nil nil t))
      (apply #'eredis-command-returning-bulk "object" subcommand args)
    (apply #'eredis-command-returning-integer "object" subcommand args)))

(defun eredis-persist(key)
  "Remove the existing timeout on KEY and returns 1 if it succeeds 0 otherwise"
  (eredis-command-returning-integer "persist" key))

(defun eredis-randomkey()
  "get a random key from the redis db"
  (eredis-command-returning-bulk "randomkey"))

(defun eredis-rename(key newkey)
  "renames KEY as NEWKEY"
  (eredis-command-returning-status "rename" key newkey))

(defun eredis-renamenx(key newkey)
  "renames KEY as NEWKEY only if NEWKEY does not yet exist"
  (eredis-command-returning-status "renamenx" key newkey))

(defun eredis-sort(key &rest args)
  "call the redis sort command with the specified KEY and ARGS"
  (apply #'eredis-command-returning-multibulk "sort" key args))

(defun eredis-ttl(key)
  "Set timeout on KEY to SECONDS and returns 1 if it succeeds 0 otherwise"
  (eredis-command-returning-integer "ttl" key))

(defun eredis-type(key)
  "Get the type of KEY"
  (eredis-command-returning-status "type" key))

;; string commands

(defun eredis-append(key value)
  "Append VALUE to value of KEY"
  (eredis-command-returning-integer "append" key value))

(defun eredis-decr(key)
  "decrement value of KEY"
  (eredis-command-returning-integer "decr" key))

(defun eredis-decrby(key decrement)
  "decrement value of KEY by DECREMENT"
  (eredis-command-returning-integer "decrby" key decrement))

(defun eredis-get(key)
  "redis get"
  (eredis-command-returning-bulk "get" key))

(defun eredis-getbit(key offset)
  "redis getbit"
  (eredis-command-returning-integer "getbit" key offset))

(defun eredis-getrange(key start end)
  "redis getrange"
  (eredis-command-returning-bulk "getrange" key start end))

(defun eredis-getset(key value)
  "redis atomic set and get old value"
  (eredis-command-returning-bulk "getset" key value))

(defun eredis-incr(key)
  "increment value of KEY"
  (eredis-command-returning-integer "incr" key))

(defun eredis-incrby(key increment)
  "increment value of KEY by INCREMENT"
  (eredis-command-returning-integer "incrby" key increment))

; http://redis.io/commands/mget

(defun eredis-mget(keys)
  "return the values of the specified keys, or nil if not present"
  (apply #'eredis-command-returning-multibulk "mget" keys))

(defun eredis-mset(m)
  "set the keys and values of the map M in Redis using mset"
  (apply #'eredis-command-returning-status "mset" (eredis-parse-map-or-list-arg m)))

(defun eredis-msetnx(m)
  "set the keys and values of the map M in Redis using msetnx (only if all are not existing)"
    (apply #'eredis-command-returning-integer "msetnx" (eredis-parse-map-or-list-arg m)))

(defun eredis-set(k v)
  "set the key K and value V in Redis"
  (eredis-command-returning-status "set" k v))

(defun eredis-setbit(key offset value)
  "redis setbit"
  (eredis-command-returning-integer "setbit" key offset value))

(defun eredis-setex(key seconds value)
  "eredis setex"
  (eredis-command-returning-status "setex" key seconds value))

(defun eredis-setnx(k v)
  "set if not exist"
  (eredis-command-returning-integer "setnx" k v))

(defun eredis-setrange(key offset value)
  "redis setrange"
  (eredis-command-returning-integer "setrange" key offset value))

(defun eredis-strlen(key)
  "redis strlen"
  (eredis-command-returning-integer "strlen" key))

;; hash commands

(defun eredis-hget(key field)
  "redis hget"
  (eredis-command-returning-bulk "hget" key field))

(defun eredis-hset(key field value)
  "redis hset"
  (eredis-command-returning-integer "hset" key field value))

(defun eredis-hsetnx(key field value)
  "redis hsetnx"
  (eredis-command-returning-integer "hsetnx" key field value))

(defun eredis-hmget(key field &rest fields)
  "redis hmget"
  (apply #'eredis-command-returning-multibulk "hmget" key field fields))

(defun eredis-hmset(key m)
  "redis hmset set multiple key values on the key KEY using an emacs lisp map M or list of key values"
  (apply #'eredis-command-returning-status "hmset" key (eredis-parse-map-or-list-arg m)))

(defun eredis-hincrby(key field integer)
  "increment FIELD on KEY by INTEGER"
  (eredis-command-returning-integer "hincrby" key field integer))

(defun eredis-hexists(key field)
  "redis hexists"
  (eredis-command-returning-integer "hexists" key field))

(defun eredis-hdel(key field)
  "redis hdel"
  (eredis-command-returning-integer "hdel" key field))

(defun eredis-hlen(key)
  "redis hlen"
  (eredis-command-returning-integer "hlen" key))

(defun eredis-hkeys(key)
  "redis hkeys"
  (eredis-command-returning-multibulk "hkeys" key))

(defun eredis-hvals(key)
  "redis hvals"
  (eredis-command-returning-multibulk "hvals" key))

(defun eredis-hgetall(key)
  "redis hgetall"
  (eredis-command-returning-multibulk "hgetall" key))

;; list commands

(defun eredis-llen(key)
  "length of list"
  (eredis-command-returning-integer "llen" key))

(defun eredis-lpop(key)
  "list pop first element"
  (eredis-command-returning-bulk "lpop" key))

(defun eredis-lpush(key value &rest values)
  "Prepend value(s) to a list stored by KEY"
  (apply #'eredis-command-returning-integer "lpush" key value values))

(defun eredis-rpush(key value &rest values)
  "Append value(s) to a list stored by KEY"
  (apply #'eredis-command-returning-integer "rpush" key value values))

(defun eredis-lpushx(key value)
  "Prepend value(s) to a list stored by KEY if it doesn't exist already"
  (eredis-command-returning-integer "lpushx" key value))

(defun eredis-rpushx(key value)
  "Append value(s) to a list stored by KEY if it doesn't exist already"
  (eredis-command-returning-integer "rpushx" key value))

(defun eredis-lindex(key index)
  "list element INDEX to a list stored by KEY"
  (eredis-command-returning-bulk "lindex" key index))

(defun eredis-blpop(key &rest rest)
  "blocking left pop of multiple lists, rest is actually as many keys as you want and a timeout"
  (apply #'eredis-command-returning-multibulk "blpop" key rest))

(defun eredis-brpop(key &rest rest)
  "blocking right pop of multiple lists, rest is actually as many keys as you want and a timeout"
  (apply #'eredis-command-returning-multibulk "brpop" key rest))

(defun eredis-lrange(key start stop)
  "redis lrange"
  (eredis-command-returning-multibulk "lrange" key start stop))

(defun eredis-linsert(key position pivot value)
  "redis linsert"
  (eredis-command-returning-integer "linsert" key position pivot value))

(defun eredis-brpoplpush(source destination timeout)
  "redis brpoplpush"
  (eredis-command-returning-bulk "brpoplpush" source destination timeout))

(defun eredis-rpoplpush(source destination timeout)
  "redis rpoplpush"
  (eredis-command-returning-bulk "rpoplpush" source destination))

(defun eredis-lrem(key count value)
  "redis lrem"
  (eredis-command-returning-integer "lrem" key count value))

(defun eredis-lset(key index value)
  "redis lset"
  (eredis-command-returning-status "lset" key index value))

(defun eredis-ltrim(key start stop)
  "redis ltrim"
  (eredis-command-returning-status "ltrim" key start stop))

(defun eredis-rpop(key)
  "right pop of list"
  (eredis-command-returning-bulk "rpop" key))



;; connection commands

(defun eredis-auth(password)
  "eredis auth"
  (eredis-command-returning-status "auth" password))

(defun eredis-echo(message)
  "eredis echo"
  (eredis-command-returning-bulk "echo" message))

(defun eredis-ping()
  "redis ping"
  (interactive)
  (eredis-command-returning-status "ping"))

(defun eredis-quit()
  "redis ping"
  (interactive)
  (eredis-command-returning-status "quit"))

(defun eredis-select(index)
  "redis select db with INDEX"
  (interactive)
  (eredis-command-returning-status "select" index))

;; server commands 

(defun eredis-info()
  (eredis-command-returning-bulk "info"))



(defun eredis-mset-region(beg end delimiter) 
  "Parse the current region using DELIMITER to split each line into a key value pair which
is then sent to redis using mset"
  (interactive "*r\nsDelimiter: ")
  (let ((done nil)
	(mset-param (make-hash-table :test 'equal)))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (save-excursion
	(while (not done)
	  (let ((split-line 
		 (split-string  
		  (buffer-substring (point-at-bol) (point-at-eol)) 
		  delimiter)))
	    (let ((key (first split-line))
		  (value (second split-line)))
	      (if (or (null key) (null value))
		  (setf done t)
		(progn
		  (puthash key value mset-param)
		  (next-line))))))))
    (if (> (hash-table-count mset-param) 0)
	(eredis-mset mset-param)
      nil)))

(defun eredis-org-table-from-pattern(pattern)
  "Search Redis for the pattern of keys and create an org table from the results"
  (interactive "sPattern: ")
  (let ((m (eredis-map-keys pattern)))
    (if m
	(org-table-from-map m)
      (message (format "No keys found for pattern %s" pattern)))))

(defun org-table-from-map(m)
  "Create an org-table from a map of key value pairs"
  (let ((beg (point)))
    (if (hash-table-p m)
	(progn
	  (insert-map m)
	  (org-table-convert-region beg (point))))))

(defun eredis-org-table-get-field-clean(col)
  "Get a field in org table at column COL and strip any leading or
trailing whitespace using chomp. Also strip text properties"
  (let ((field (org-table-get-field col)))
    (let ((chomped (chomp field)))
      (set-text-properties 0 (length chomped) nil chomped)
      chomped)))

(defun eredis-org-table-to-map()
  "Walk an org table and convert the first column to keys and the second 
column to values in an elisp map"
  (let ((retmap (make-hash-table :test 'equal)))
    (save-excursion
      (let ((beg (org-table-begin))
	    (end (org-table-end)))
	(goto-char beg)
	(while (> end (point))
	  (let ((key (eredis-org-table-get-field-clean 1))
		(value (eredis-org-table-get-field-clean 2)))
	    (when (and key value)
	      (puthash key value retmap)))
	  (next-line))))
    retmap))

(defun eredis-org-table-row-to-key-value-pair()
  "When point is in an org table convert the first column to a key and the second 
column to a value, returning the result as a dotted pair"
  (let ((beg (org-table-begin))
	(end (org-table-end)))
    (if (and (>= (point) beg)
	     (<= (point) end))
	(let ((key (eredis-org-table-get-field-clean 1))
	      (value (eredis-org-table-get-field-clean 2)))
	  (if (and key value)
	      (cons key value)
	    nil))
      nil)))

(defun eredis-org-table-mset()
  "with point in an org table convert the table to a map and send it to redis with mset"
  (interactive)
  (let ((m (eredis-org-table-to-map)))
    (eredis-mset m)))

(defun eredis-org-table-msetnx()
  "with point in an org table convert the table to a map and send it to redis with msetnx"
  (interactive)
  (let ((m (eredis-org-table-to-map)))
    (eredis-msetnx m)))

(defun eredis-org-table-row-set()
  "with point in an org table set the key and value"
  (interactive)
  (let ((keyvalue (eredis-org-table-row-to-key-value-pair)))
    (eredis-set (car keyvalue) (cdr keyvalue))))

(provide 'eredis)