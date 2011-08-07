;; eredis... A simple emacs interface to redis
;; See for info on the protocol http://redis.io/topics/protocol

;; (C) 2011 Justin Heyes-Jones
;; This is released under the Gnu License v3. See http://www.gnu.org/licenses/gpl.txt

(require 'org-table)
(require 'cl)

(defvar *redis-process* nil "Current Redis client process")
(defvar *redis-state* nil "Statue of the connection")
(defvar *redis-response* nil "Stores response of last Redis command")
(defvar *redis-timeout* 300 "Timeout on the client in seconds when waiting for Redis")

;; UTILS

(defun eredis-set-timeout(seconds)
  "set how long emacs will wait for a response from redit, pay attention to this if using blocking 
commands like blpop which also have a timeout" 
  (setf *redis-timeout* seconds))

(defun two-lists-to-map(lst1 lst2)
  "take a list of keys LST1 and a list of values LST2 and make a hashmap, not particularly efficient
as it first constructs a list of key value pairs then uses that to construct the hashmap"
  (let ((retmap (make-hash-table :test 'equal)))
    (mapc (lambda (n) (puthash (car n) (cdr n) retmap))
	  (map 'list (lambda (a b) (cons a b)) lst1 lst2))
    retmap))

(defun unflatten-map-worker(in keys values)
  (if (null in)
      (two-lists-to-map keys values)
    (unflatten-map-worker (cddr in) (cons (first in) keys) (cons (second in) values))))

(defun unflatten-map(l)
  "take a list of value1 key1 ... valuen keyn and return a map"
  (let ((len (length l)))
    (if (/= (mod len 2) 0)
	(error "list must be even length"))
    (unflatten-map-worker l nil nil)))

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

;; TODO random macro would be nice; dolist with a different body to execute for the first 
;; or last item 
(defun insert-list(l)
  "insert a list L into the current buffer"
  (let ((str (mapconcat #'identity l ",")))
    (insert str)))
;    (insert (subseq str 0 ))))

(defun stringify-numbers-and-symbols(item)
  (cond 
   ((numberp item)
    (number-to-string item))
   ((symbolp item)
    (symbol-name item))
   ((stringp item)
    item)
   (t
    (error "unsupported type: %s"))))

(defun eredis-construct-unified-request(command &rest arguments)
  "all redis commands are sent using this protocol"
  (let ((num-args (+ 1 (length arguments))))
    (if (> num-args 0)
	(let ((req (format "*%d\r\n$%d\r\n%s\r\n" num-args (length command) command)))
	  (dolist (item arguments)
	    (setf item (stringify-numbers-and-symbols item))
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

(defun eredis-get-response(&optional requested-timeout)
  "await response from redis and store it in *redis-response*. If it times out it will return nil"
  (let ((timeout (or requested-timeout *redis-timeout*)))
    (if (accept-process-output *redis-process* timeout 0 t)
	*redis-response*
      nil)))

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
	  (eredis-parse-multi-bulk resp)))
    (error "redis not connected")))

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
      (if (= ?- (string-to-char resp))
	  (error "redis error: %s" (eredis-trim-status-response resp))
	nil))))
      

(defun eredis-command-returning-bulk(command &rest args)
  "Send a COMMAND that has the bulk return type and return it to the user"
  (if (and *redis-process* (eq *redis-state* 'open))
      (progn 
	(process-send-string *redis-process* (apply #'eredis-construct-unified-request command args))
	(let ((resp (eredis-get-response)))
	  (eredis-parse-bulk resp)))
    (error "redis not connected")))

(defun eredis-buffer-message(process message)
  "append a message to the redis process buffer"
  (save-excursion 
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert message)
    (goto-char (point-max))))

(defun eredis-sentinel(process event)
  "sentinel function for redis network process which monitors for events"
  (eredis-buffer-message process (format "sentinel event %s" event))
  (cond 
   ((string-match "open" event)
    (setq *redis-state* 'open))
   ((string-match "connection broken by remote peer" event)
    (progn 
      (setq *redis-state* 'closed)
      (setq *redis-process* nil)))
   (t
    nil)))

(defun eredis-filter(process string)
  "filter function for redis network process, which receives output"
  (setq *redis-response* string))

(defun eredis-delete-process()
  (when *redis-process*
    (delete-process *redis-process*)
    (setq *redis-process* nil)
    (setq *redis-state* 'closed)))

;; Connect and disconnect functionality

(defun eredis-hai(host port &optional no-wait)
  "connect to Redis on HOST PORT. NO-WAIT can be set to true to make the connection asynchronously
but that's not supported on windows and doesn't make much difference"
  (interactive "sHost: \nsPort (usually 6379): \n")
  (eredis-delete-process)
  (setq *redis-state* 'opening)
  (let ((p 
	 (make-network-process :name "redis"
			       :host host
			       :service port
			       :type nil
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

(defun eredis-status-response-success-p(resp)
  (= ?+ (string-to-char resp)))

(defun eredis-trim-status-response(resp)
  "strip the leading character +/- and the final carriage returns"
  (let ((len (length resp)))
    (subseq resp 1 (- len 2))))

(defun eredis-parse-integer-response(resp)
  "parse integer response type"
  (if (= ?: (string-to-char resp))
      (string-to-number (subseq resp 1))
    (if (= ?- (string-to-char resp))
	(error "redis error: %s" (eredis-trim-status-response resp))
      nil)))

(defun eredis-command-returning-integer(command &rest args)
  "Send a command that has the integer return type"
  (if (and *redis-process* (eq *redis-state* 'open))
      (progn 
	(process-send-string *redis-process* (apply #'eredis-construct-unified-request command args))
	(let ((resp (eredis-get-response)))
	  (eredis-parse-integer-response resp)))
    (error "redis not connected")))

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
    (error "redis not connected")))

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

;; set commands

(defun eredis-sadd(key member &rest members)
  "redis add to set"
  (apply #'eredis-command-returning-integer "sadd" key member members))

(defun eredis-scard(key)
  "redis scard"
  (eredis-command-returning-integer "scard" key))

(defun eredis-sdiff(key &rest keys)
  "redis sdiff"
  (apply #'eredis-command-returning-multibulk "sdiff" key keys))

(defun eredis-sdiffstore(destination key &rest keys)
  "redis sdiffstore"
  (apply #'eredis-command-returning-integer "sdiffstore" destination key keys))

(defun eredis-sinter(key &rest keys)
  "redis sinter"
  (apply #'eredis-command-returning-multibulk "sinter" key keys))

(defun eredis-sinterstore(destination key &rest keys)
  "redis sinterstore"
  (apply #'eredis-command-returning-integer "sinterstore" destination key keys))

(defun eredis-sismember(key member)
  "redis sdiffstore"
  (eredis-command-returning-integer "sismember" key member))

(defun eredis-smembers(key)
  "redis smembers"
  (eredis-command-returning-multibulk "smembers" key))

(defun eredis-smove(source destination member)
  "redis smove"
  (eredis-command-returning-integer "smove" source destination member))

(defun eredis-spop(key)
  "redis spop"
  (eredis-command-returning-bulk "spop" key))

(defun eredis-srandmember(key)
  "redis srandmember"
  (eredis-command-returning-bulk "srandmember" key))

(defun eredis-srem(key member &rest members)
  "redis srem"
  (apply #'eredis-command-returning-integer "srem" key member members))

(defun eredis-sunion(key &rest keys)
  "redis sunion"
  (apply #'eredis-command-returning-multibulk "sunion" key keys))

(defun eredis-sunionstore(destination key &rest keys)
  "redis sunionstore"
  (apply #'eredis-command-returning-integer "sunionstore" destination key keys))


;; sorted set commands

(defun eredis-zadd(key score member)
  "redis zadd"
  (eredis-command-returning-integer "zadd" key score member))

(defun eredis-zcard(key)
  "redis zcard"
  (eredis-command-returning-integer "zcard" key))

(defun eredis-zcount(key min max)
  "redis zcount"
  (eredis-command-returning-integer "zcount" key min max))

(defun eredis-zincrby(key increment member)
  "redis zincrby"
  (eredis-command-returning-bulk "zincrby" key increment member))

(defun eredis-zinterstore(destination numkeys key &rest rest)
  "redis zinterstore"
  (apply #'eredis-command-returning-integer "zinterstore" destination numkeys key rest))

(defun eredis-zrange(key start stop &optional withscores)
  "eredis zrange. withscores can be the string \"withscores\", the symbol 'withscores"
  (if (null withscores)
      (eredis-command-returning-multibulk "zrange" key start stop)
    (eredis-command-returning-multibulk "zrange" key start stop withscores)))

(defun eredis-zrangebyscore(key min max &rest rest)
  "eredis zrangebyscore"
  (apply #'eredis-command-returning-multibulk "zrangebyscore" key min max rest))

(defun eredis-zrank(key member)
  "redis zrank"
  (eredis-command-returning-integer "zrank" key member))

(defun eredis-zrem(key member)
  "redis zrem"
  (eredis-command-returning-integer "zrem" key member))

(defun eredis-zremrangebyrank(key start stop)
  "redis zremrangebyrank"
  (eredis-command-returning-integer "zremrangebyrank" key start stop))

(defun eredis-zremrangebyscore(key min max)
  "redis zremrangebyscore"
  (eredis-command-returning-integer "zremrangebyscore" key min max))

(defun eredis-zrevrange(key start stop &optional withscores)
  "eredis zrevrange. withscores can be the string \"withscores\", the symbol 'withscores"
  (if (null withscores)
      (eredis-command-returning-multibulk "zrevrange" key start stop)
    (eredis-command-returning-multibulk "zrevrange" key start stop withscores)))

(defun eredis-zrevrangebyscore(key min max &rest rest)
  "eredis zrevrangebyscore"
  (apply #'eredis-command-returning-multibulk "zrevrangebyscore" key min max rest))

(defun eredis-zrevrank(key member)
  "redis zrevrank"
  (eredis-command-returning-integer "zrevrank" key member))

(defun eredis-zscore(key member)
  "redis zscore"
  (eredis-command-returning-bulk "zscore" key member))

(defun eredis-zunionstore(destination numkeys key &rest rest)
  "redis zunionstore"
  (apply #'eredis-command-returning-integer destination numkeys key rest))

;; pub/sub commands

;; Warning: these aren't working very well yet. Need to write a custom response handler 
;; to handle replies from the publish subscribe commands. They have differences, for 
;; example multiple bulk messages come at once. 

(defun eredis-publish(channel message)
  "eredis publish"
  (eredis-command-returning-integer "publish" channel message))

(defun eredis-subscribe(channel &rest channels)
  "eredis subscribe"
   (apply #'eredis-command-returning-multibulk "subscribe" channel channels))

(defun eredis-psubscribe(pattern &rest patterns)
  "eredis psubscribe"
   (apply #'eredis-command-returning-multibulk "psubscribe" pattern patterns))

(defun eredis-unsubscribe(channel &rest channels)
  "eredis unsubscribe"
   (apply #'eredis-command-returning-multibulk "unsubscribe" channel channels))

(defun eredis-punsubscribe(pattern &rest patterns)
  "eredis punsubscribe"
   (apply #'eredis-command-returning-multibulk "punsubscribe" pattern patterns))

(defun eredis-await-message()
  "Not a redis command. After subscribe or psubscribe, call this  to poll each
message and call unsubscribe or punsubscribe when done. Other commands will fail
with an error until then"
  (let ((resp (eredis-get-response)))
    (eredis-parse-multi-bulk resp)))

;; transaction commands

(defun eredis-discard()
  "eredis discard"
  (eredis-command-returning-status "discard"))

(defun eredis-multi()
  "eredis multi"
  (eredis-command-returning-status "multi"))

;; TODO this returns a multibulk which in turn will contain a sequence of responses to commands
;; executed. Best way to handle this is probably to return a list of responses
;; Also need to fix the parser to handle numeric results in a multibulk response
;; which is the same issue I'm seeing with publish/subscribe results
(defun eredis-exec()
  "eredis exec"
  (eredis-command-returning-multibulk "exec"))

(defun eredis-watch(key &rest keys)
  "redis watch"
  (apply #'eredis-command-returning-status "watch" key keys))

(defun eredis-unwatch()
  "redis unwatch"
  (eredis-command-returning-status "unwatch"))

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

(defun eredis-bgrewriteaof()
  (eredis-command-returning-status "bgrewriteaof"))

(defun eredis-bgsave()
  (eredis-command-returning-status "bgsave"))

(defun eredis-config-get(parameter)
  (eredis-command-returning-multibulk "config" "get" parameter))

(defun eredis-config-set(parameter value)
  (eredis-command-returning-status "config" "set" parameter value))

(defun eredis-config-resetstat()
  (eredis-command-returning-status "config" "resetstat"))

(defun eredis-dbsize()
  (eredis-command-returning-integer "dbsize"))

(defun eredis-debug-object(key)
  (eredis-command-returning-status "debug" "object" key))

(defun eredis-debug-segfault()
  (eredis-command-returning-status "debug" "segfault"))

(defun eredis-flushall()
  (eredis-command-returning-status "flushall"))

(defun eredis-flushdb()
  (eredis-command-returning-status "flushdb"))

;; TODO the response from this is a single bulk response but it could be further parsed into a map
;; It uses : to delimit the keys from values
(defun eredis-info()
  (eredis-command-returning-bulk "info"))

(defun eredis-lastsave()
  (eredis-command-returning-integer "lastsave"))

;; TODO monitor opens up the *redis-buffer* and shows commands streaming 
;; but it does not yet follow along, they just go off the screen, so I need
;; to fix that
(defun eredis-monitor()
  (if (and *redis-process* (eq *redis-state* 'open))
      (unwind-protect
	  (progn
	    (switch-to-buffer "*redis*")
	    (goto-char (point-max))
	    (eredis-buffer-message *redis-process* "C-g to exit\n")
	    (process-send-string *redis-process* "monitor\r\n")
	    (let ((resp nil))
	      (while t
		(redisplay t)
		(sleep-for 1)
		;;(recenter-top-bottom 'top)
		(let ((resp (eredis-get-response 5)))
		  (when resp
		    (eredis-buffer-message *redis-process* resp))))))
	;; when the user hits C-g we send the quit command to exit
	;; monitor mode
	(progn
	  (eredis-quit)
	  (eredis-kthxbye)))))
	

(defun eredis-save()
  (eredis-command-returning-status "save"))

(defun eredis-shutdown()
  "shutdown redis server"
  (interactive)
  ;; Note that this just sends the command and does not wait for or parse the response
  ;; since there shouldn't be one
  (if (and *redis-process* (eq *redis-state* 'open))
      (progn 
	(process-send-string *redis-process* (eredis-construct-unified-request "shutdown"))
	(eredis-kthxbye))))

(defun eredis-slaveof(host port)
  (eredis-command-returning-status "slaveof" host port))

;; This is in the docs but not in the server I'm using 
;; (defun eredis-slowlog-len()
;;   (eredis-command-returning-integer "slowlog" "len"))

(defun eredis-sync()
  (eredis-command-returning-status "sync"))

;; Helpers 

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

(defun eredis-org-table-from-keys(keys)
  "for each of KEYS lookup their type in redis and populate an org table 
containing a row for each one"
  (org-table-from-list  '("Key" "Type" "Values"))
  (dolist (key keys)
    (let ((type (eredis-type key)))	 
      (cond
       ((string= "string" type)
	(eredis-org-table-from-string key))
       ((string= "zset" type)
	(eredis-org-table-from-zset key 'withscores))
       ((string= "hash" type)
	(eredis-org-table-from-hash key))
       ((string= "list" type)
	(eredis-org-table-from-list key))
       ((string= "set" type)
	(eredis-org-table-from-set key))
       ((string= "none" type)
	nil) ; silently skip missing keys
       (t
	(insert (format "| %s | unknown type %s |\n" key type)))))))

(defun eredis-org-table-from-list(key)
  "create an org table populated with the members of the list KEY"
  (let ((items (eredis-lrange key 0 -1)))
    (when items
      (org-table-from-list (apply #' list key "list" items)))))

(defun eredis-org-table-from-zset(key &optional withscores)
  "create an org table populated with the members of the zset KEY"
  (let ((items (eredis-zrange key 0 -1 withscores)))
    (when items
      (org-table-from-list (apply #'list key "zset" items)))))

(defun eredis-org-table-from-set(key)
  "create an org table populated with the members of the set KEY"
  (let ((members (eredis-smembers key)))
    (when members
      (org-table-from-list (apply #'list key "set" members)))))

(defun eredis-org-table-from-hash(key)
  "org table populated with the hash of KEY"
  (let ((m (eredis-hgetall key)))
    (when m
      (setf m (unflatten-map m))
      (org-table-from-map m))))

(defun eredis-org-table-from-string(key)
  "create a small org table from the key, and it's string value"
  (let ((val (eredis-get key)))
    (when val
      (org-table-from-list (list key "string" val)))))

(defun eredis-org-table-from-pattern(pattern)
  "Search Redis for the pattern of keys and create an org table from the results"
  (let ((keys (eredis-keys pattern)))
    (if keys
	(eredis-org-table-from-keys keys))))

(defun org-table-from-list(l)
  "Create an org-table from a list"
  (if (listp l)
      (let ((beg (point)))
    	(insert-list l)
    	(org-table-convert-region beg (point) '(4))
    	(forward-line))))

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