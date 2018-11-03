;;; eredis.el --- eredis, a Redis client in emacs lisp

;; Copyright (C) 2012-2018 --- Justin Heyes-Jones
 
;; Author: Justin Heyes-Jones <justinhj@gmail.com>

;; Version: 0.9
;; Package-Requires: (dash)
;; Keywords: redis, api, tools, org
;; URL: http://github.com/justinhj/eredis/

;; See for info on the protocol http://redis.io/topics/protocol

;;; Commentary:

;; Eredis provides a programmatic API for accessing Redis (in-memory data structure store/database) using emacs lisp.
;; This software is released under the Gnu License v3. See http://www.gnu.org/licenses/gpl.txt

;; Usage:

;; Each redis connection creates a process and has an associated buffer which revieves data from the redis server

;; (setq redis-p1 (eredis-connect "localhost" "6379"))
;; (eredis-set "key" "value" redis-p1) "ok"
;; (eredis-get "key" redis-p1) "value"

;; Earlier versions of redis (pre 0.9) did not support multiple connections/processes. To preserve backwards compatibility you can omit the process argument from commands and an internal variable `eredis--current-process' will track the most recent connection to be used by default. 

;; You can close a connection like so. The process buffer can be closed seperately.
;; (eredis-disconnect redis-p1)

;;; 0.9 Changes

;; Multiple connections to multiple redis servers supported
;; Buffer is used for all output from the process (Redis)
;; Github repo contains an ert test suite
;; Fix for multibyte characters
;; Support for LOLWUT (version 5.0 of Redis and later)

;;; Github contributors

;; justinhj
;; pidu
;; crispy 
;; darksun
;; lujun9972

;;; Future TODO 

;; TODO rethink error reporting... it currently is not distinguishable to the user from a normal response, perhaps return a tuple ...
;;; response type (incomplete, complete, error)
;;; and body
;;; note that this will change the API though
;;; simpler solution is to throw the error
;;; TODO check all private function names have --
;;; TODO check all functionas have eredis-
;;; Everything here https://github.com/bbatsov/emacs-lisp-style-guide
;;; heading comments three semi colons, otherwise two

(require 'cl)
(require 'dash)

(defvar eredis--current-process nil "Current Redis client process, used when the process is not passed in to the request")

;; Util

(defun eredis--two-lists-to-map(key-list value-list)
  "take a list of keys LST1 and a list of values LST2 and make a hashmap, not particularly efficient
as it first constructs a list of key value pairs then uses that to construct the hashmap"
  (let ((retmap (make-hash-table :test 'equal)))
    (cl-mapc (lambda (k v)
               (puthash k v retmap))
             key-list value-list)
    retmap))

(defun eredis--unflatten-map-worker(in keys values)
  (if (null in)
      (eredis--two-lists-to-map keys values)
    (eredis--unflatten-map-worker (cddr in) (cons (first in) keys) (cons (second in) values))))

(defun eredis--unflatten-map(l)
  "take a list of value1 key1 ... valuen keyn and return a map"
  (let ((len (length l)))
    (if (/= (mod len 2) 0)
        (error "list must be even length"))
    (eredis--unflatten-map-worker l nil nil)))

(defun eredis--flatten-map(m)
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
      (eredis--flatten-map a)
    a))

(defun eredis--insert-map(m)
  "insert a map M of key value pairs into the current buffer"
  (maphash (lambda (a b) (insert (format "%s,%s\n" a b))) m))

;; TODO random macro would be nice; dolist with a different body to execute for the first 
;; or last item 
(defun eredis--insert-list(l)
  "insert a list L into the current buffer"
  (let ((str (mapconcat #'identity l ",")))
    (insert str)))
                                        ;    (insert (cl-subseq str 0 ))))

(defun eredis--stringify-numbers-and-symbols(item)
  (cond 
   ((numberp item)
    (number-to-string item))
   ((symbolp item)
    (symbol-name item))
   ((stringp item)
    item)
   (t
    (error "unsupported type: %s" item))))

(defun eredis-build-request(command &rest arguments)
  "Construct a command to send to Redis using the RESP protocol"
  (let ((num-args (+ 1 (length arguments))))
    (if (> num-args 0)
        (let ((req (format "*%d\r\n$%d\r\n%s\r\n" num-args (length command) command)))
          (dolist (item arguments)
            (setf item (eredis--stringify-numbers-and-symbols item))
            (setf req (concat req (format "$%d\r\n%s\r\n" (string-bytes item) item))))
          req)
      nil)))

(defun eredis-map-keys(key-expr)
  "take a glob expression like \"user.id.*\" and return the key/values of matching keys"
  (let ((keys (eredis-keys key-expr)))
    (if keys
        (let ((values (eredis-mget keys)))
          (eredis--two-lists-to-map keys values))
      nil)))

(defun eredis-get-response(process)
  "Given the process we try to get its buffer, and the next response start position (which is stored in the process properties under `response-start', we then identify the message type and parse the response. If we run out of response (maybe it isn't all downloaded yet we return `incomplete' otherwise we return the response, the format of which may depend on the request type"
  (let ((buffer (process-buffer process))
	(response-start (process-get process 'response-start)))
    (with-current-buffer buffer
      (accept-process-output process 3 0 t)
      (pcase-let ((`(,message . ,length)
		   (eredis-parse-response (buffer-substring response-start (point-max)))))
	(if (eq message 'incomplete)
	    (message (format "incomplete message... %d" length))
	  (prog1
	      message
	    (process-put process 'response-start (+ response-start length))))))))
	      
(defun eredis-response-type-of (response)
  "Get the type of RESP response based on the initial character"
  (let ((chr (elt response 0))
        (chr-type-alist '((?- . error)
                          (?* . array)
                          (?$ . single-bulk)
                          (?: . integer)
                          (?+ . status))))
    (cdr (assoc chr chr-type-alist))))

(defun eredis-parse-response (response)
  "Parse the response. Returns a cons of the type and the body. Body will be 'incomplete if it is not yet fully downloaded or corrupted. An error is thrown when parsing an unknown type"
  (let ((response-type (eredis-response-type-of response)))
    (cond ((eq response-type 'error)
           (eredis-parse-error-response response))
          ((eq response-type 'array)
           (eredis-parse-array-response response))
          ((eq response-type 'single-bulk)
           (eredis-parse-bulk-response response))
          ((eq response-type 'integer)
           (eredis-parse-integer-response response))
          ((eq response-type 'status)
           (eredis-parse-status-response response))
          (t (error "Unkown RESP response prefix: %c" (elt response 0))))))

(defun eredis--basic-response-length (resp)
  "Return the length of the response header or fail with nil if it doesn't end wth \r\n"
  (when (and resp (string-match "\r\n" resp))
    (match-end 0)))

(defun eredis-parse-integer-response(resp)
  (let ((len (eredis--basic-response-length resp)))
    (if len	
	`(,(string-to-number (cl-subseq resp 1)) . ,len)
      `(incomplete . 0))))

(defun eredis-parse-error-response (resp)
  (eredis-parse-status-response resp))

(defun eredis-parse-status-response (resp)
  (let ((len (eredis--basic-response-length resp)))
    (if len
	`(,(substring resp 1 (- len 2)) . ,len)
      '(incomplete . 0))))

(defun eredis-parse-bulk-response (resp)
  "Parse the redis bulk response `resp'. Returns the dotted pair of the result and the total length of the message including any line feeds and the header. If the result is incomplete return `incomplete' instead of the message so the caller knows to wait for more data from the process"
  (let ((unibyte (string-as-unibyte resp)))
    (if (string-match "^$\\([\-]*[0-9]+\\)\r\n" unibyte)
	(let* ((body-size (string-to-number (match-string 1 unibyte)))
	       (header-size (+ (length (match-string 1 resp)) 1 2 2))
	       (total-size-bytes (+ header-size body-size))
	       (body-start (match-end 0)))
	  (message (format "body size %d" body-size))
	  (if (< body-size 0)
	      `(,nil . ,(- header-size 2))
	    (if (= body-size 0)
		`("" . ,header-size)
	      (if (< (length unibyte) total-size-bytes)
		  `(incomplete . 0)
		(let ((message (string-as-multibyte
				(substring unibyte body-start (+ body-start body-size)))))
		  `(,message . ,(+ header-size (length message))))))))
      `(incomplete . 0))))

  ;; wip rewriting
(defun eredis-parse-array-response (resp)
  "Parse the redis array response RESP and return the list of results. handles null entries when length is -1 as per spec. handles lists of any type of thing, handles lists of lists etc"
  (if (string-match "^*\\([\-]*[0-9]+\\)\r\n" resp)
      (let ((array-length (string-to-number (match-string 1 resp)))
	    (header-size (+ (length (match-string 1 resp)) 1 2)))
	(message (format "parse array length %d header %d resp %s" array-length header-size resp))
	(case array-length
	  (0
	   `(() . 4))
	  (-1
	   `(nil . 5))
	  (t
	   (let ((things nil)
		 (current-pos header-size))
	     (dotimes (n array-length)
	       (message (format "n %d current-pos %d" n current-pos))
	       (pcase-let ((`(,message . ,length)
			    (eredis-parse-response (substring resp current-pos nil))))
		 (message (format "%s length %d" message length))
		 (incf current-pos length)
		 (!cons message things)))
	     `(,things . ,current-pos)))))
    `(incomplete . 0)))

(defun eredis--util-remove-last(lst) (reverse (cdr (reverse lst))))

(defun eredis-command-returning (command &rest args)
  "Send a command that has the status code return type. If the last argument is a process then that is the process used, otherwise it will use the value of `eredis--current-process'"
  (let* ((last-arg (car (last args)))
	 (process (if (processp last-arg)
		      last-arg
		    eredis--current-process))
	 (command-args (eredis--util-remove-last args)))
    (if (and process (eq (process-status process) 'open))
	(progn 
          (process-send-string process (apply #'eredis-build-request command command-args))
          (let ((ret-val (eredis-get-response process)))
            (when (called-interactively-p 'any)
              (message ret-val))
            ret-val))
      (error "redis not connected"))))

(defun eredis-buffer-message(process message)
  "append a message to the redis process buffer"
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert message)
    (goto-char (point-max))))

(defun eredis-sentinel(process event)
  "sentinel function for redis network process which monitors for events"
  (eredis-buffer-message process (format "sentinel event %s" event))
  (when (eq 'closed (process-status process))
    (delete-process process)
    (setq eredis--current-process nil)))

(defun eredis-filter(process string)
  "filter function for redis network process, which receives output"
  (message (format "received %d bytes at process %s" (length string) eredis--current-process))
  (process-put process 'eredis-response-str (concat (or (process-get process 'eredis-response-str)
                                                    "")
                                                string)))

(defun eredis-delete-process(&optional process)
  (if process
      (prog1 
	  (delete-process process)
	(when (eq eredis--current-process process)
	    (setq eredis--current-process nil)))
    (when eredis--current-process
      (delete-process eredis--current-process)
      (setq eredis--current-process nil))))

;; Create a unique buffer for each connection

(defun eredis--generate-buffer(host port)
  (generate-new-buffer (format "redis-%s-%d" host port)))

;; Connect and disconnect functionality

(defun eredis-connect(host port &optional nowait)
  "Connect to Redis on HOST PORT. `NOWAIT' can be set to non-nil to make the connection asynchronously. That's not supported when you run on Windows"
  (interactive (list (read-string "Host: " "localhost") (read-number "Port: " 6379)))
  (let ((buffer (eredis--generate-buffer host port)))	
    (prog1
	(setq eredis--current-process
              (make-network-process :name (buffer-name buffer)
				    :host host
				    :service port
				    :type nil
				    :nowait nowait
				    :keepalive t
				    :linger t
				    :sentinel #'eredis-sentinel
				    :buffer buffer))
      (process-put eredis--current-process 'response-start 1))))

(defun eredis-clear-buffer(&optional process)
  "Erase the process buffer and reset the `response-start' property to the start"
  (let ((this-process (if (processp process)
			  process
			eredis--current-process)))
    (when (processp this-process)
      (with-current-buffer (process-buffer this-process)
	(erase-buffer)
	(process-put this-process 'response-start 1)
	t))))

(defun eredis-disconnect(&optional process)
  "Close the connection to Redis"
  (interactive)
  (eredis-delete-process process))

;; legacy 'funny' names for connect and disconnect
(defalias 'eredis-hai 'eredis-connect)
(defalias 'eredis-kthxbye 'eredis-disconnect)

;; NOTE I think this is deprecated since it doesn't seem to do anything
;; (defun eredis-get-map(keys)
;;   "Given a map M of key/value pairs, go to Redis to retrieve the values and set the value to whatever it is in Redis (or nil if not found)"
;;   (let* ((m (make-hash-table))
;;          (num-args (1+ (hash-table-count m)))
;;          (command (format "*%d\r\n$4\r\nMGET\r\n" num-args))
;;          (key-value-string ""))
;;     (maphash (lambda (k v)
;;                (setf key-value-string (concat key-value-string (format "$%d\r\n%s\r\n" (length k) k))))
;;              m)
;;     (process-send-string eredis--current-process (concat command key-value-string))
;;     (eredis-get-response)))

;; all the redis commands are documented at http://redis.io/commands
;; key commands

(defun eredis-del(key &rest keys)
  (apply #'eredis-command-returning "del" key keys))  

(defun eredis-exists(key &optional process)
  "Returns 1 if key exists and 0 otherwise"
  (eredis-command-returning "exists" key process))

(defun eredis-expire(key seconds &optional process)
  "Set timeout on KEY to SECONDS and returns 1 if it succeeds 0 otherwise"
  (eredis-command-returning "expire" key seconds process))

(defun eredis-expireat(key unix-time &optional process)
  "Set timeout on KEY to SECONDS and returns 1 if it succeeds 0 otherwise"
  (eredis-command-returning "expireat" key unix-time process))

(defun eredis-keys(pattern &optional process)
  "Returns a list of keys where the key matches the provided
pattern. see the link for the style of patterns"
  (eredis-command-returning "keys" pattern process))

(defun eredis-move(key db &optional process)
  "moves KEY to DB and returns 1 if it succeeds 0 otherwise"
  (eredis-command-returning "move" key db process))

(defun eredis-object(subcommand &rest args)
  "Inspect the internals of Redis Objects associated with keys,
  best see the docs for this one. http://redis.io/commands/object"
  (if (eq t (compare-strings "encoding" nil nil subcommand nil nil t))
      (apply #'eredis-command-returning "object" subcommand args)
    (apply #'eredis-command-returning "object" subcommand args)))

(defun eredis-persist(key &optional process)
  "Remove the existing timeout on KEY and returns 1 if it succeeds 0 otherwise"
  (eredis-command-returning "persist" key process))

(defun eredis-randomkey(&optional process)
  "Get a random key from the redis db"
  (eredis-command-returning "randomkey" process))

(defun eredis-rename(key newkey &optional process)
  "Renames KEY as NEWKEY"
  (eredis-command-returning "rename" key newkey process))

(defun eredis-renamenx(key newkey &optional process)
  "Renames KEY as NEWKEY only if NEWKEY does not yet exist"
  (eredis-command-returning "renamenx" key newkey process))

(defun eredis-sort(key &rest args)
  "Call the redis sort command with the specified KEY and ARGS"
  (apply #'eredis-command-returning "sort" key args))

(defun eredis-ttl(key &optional process)
  "Set timeout on KEY to SECONDS and returns 1 if it succeeds 0 otherwise"
  (eredis-command-returning "ttl" key process))

(defun eredis-type(key &optional process)
  "Get the type of KEY"
  (eredis-command-returning "type" key process))

;; string commands

(defun eredis-append(key value &optional process)
  "Append VALUE to value of KEY"
  (eredis-command-returning "append" key value process))

(defun eredis-decr(key &optional process)
  "Decrement value of KEY"
  (eredis-command-returning "decr" key process))

(defun eredis-decrby(key decrement &optional process)
  "Decrement value of KEY by DECREMENT"
  (eredis-command-returning "decrby" key decrement process))

(defun eredis-get(key &optional process)
  "Get string value"
  (eredis-command-returning "get" key process))

(defun eredis-getbit(key offset &optional process)
  "getbit"
  (eredis-command-returning "getbit" key offset process))

(defun eredis-getrange(key start end &optional process)
  "getrange"
  (eredis-command-returning "getrange" key start end process))

(defun eredis-getset(key value &optional process)
  "Atomic set and get old value"
  (eredis-command-returning "getset" key value process))

(defun eredis-incr(key &optional process)
  "Increment value of KEY"
  (eredis-command-returning "incr" key process))

(defun eredis-incrby(key increment &optional process)
  "Increment value of KEY by INCREMENT"
  (eredis-command-returning "incrby" key increment process))

(defun eredis-mget(keys)
  "Get values of the specified keys, or nil if not present"
  (apply #'eredis-command-returning "mget" keys))

(defun eredis-mset(m)
  "Set the keys and values of the map M in Redis using mset"
  (apply #'eredis-command-returning "mset" (eredis-parse-map-or-list-arg m)))

(defun eredis-msetnx(m)
  "Set the keys and values of the map M in Redis using msetnx (only if all are not existing)"
  (apply #'eredis-command-returning "msetnx" (eredis-parse-map-or-list-arg m)))

(defun eredis-set(k v &optional process)
  "Set the key K and value V in Redis"
  (eredis-command-returning "set" k v process))

(defun eredis-setbit(key offset value &optional process)
  "setbit"
  (eredis-command-returning "setbit" key offset value process))

(defun eredis-setex(key seconds value &optional process)
  "setex"
  (eredis-command-returning "setex" key seconds value process))

(defun eredis-setnx(k v &optional process)
  "set if not exist"
  (eredis-command-returning "setnx" k v process))

(defun eredis-setrange(key offset value &optional process)
  "setrange"
  (eredis-command-returning "setrange" key offset value process))

(defun eredis-strlen(key &optional process)
  "strlen"
  (eredis-command-returning "strlen" key process))

;; hash commands

(defun eredis-hget(key field &optional process)
  "hget"
  (eredis-command-returning "hget" key field process))

(defun eredis-hset(key field value &optional process)
  "hset"
  (eredis-command-returning "hset" key field value process))

(defun eredis-hsetnx(key field value &optional process)
  "hsetnx"
  (eredis-command-returning "hsetnx" key field value process))

(defun eredis-hmget(key field &rest fields)
  "hmget"
  (apply #'eredis-command-returning "hmget" key field fields))

(defun eredis-hmset(key m)
  "hmset set multiple key values on the key KEY using an emacs lisp map M or list of key values"
  (apply #'eredis-command-returning "hmset" key (eredis-parse-map-or-list-arg m)))

(defun eredis-hincrby(key field integer &optional process)
  "increment FIELD on KEY by INTEGER"
  (eredis-command-returning "hincrby" key field integer process))

(defun eredis-hexists(key field &optional process)
  "hexists"
  (eredis-command-returning "hexists" key field process))

(defun eredis-hdel(key field &optional process)
  "hdel"
  (eredis-command-returning "hdel" key field process))

(defun eredis-hlen(key &optional process)
  "hlen"
  (eredis-command-returning "hlen" key process))

(defun eredis-hkeys(key &optional process)
  "redis hkeys"
  (eredis-command-returning "hkeys" key process))

(defun eredis-hvals(key &optional process)
  "redis hvals"
  (eredis-command-returning "hvals" key process))

(defun eredis-hgetall(key &optional process)
  "redis hgetall"
  (eredis-command-returning "hgetall" key process))

;; hyperloglog commands
(defun eredis-pfadd(key value &rest values)
  "add the elements to the named HyperLogLog"
  (eredis-command-returning "pfadd" key value values))

(defun eredis-pfcount(key &rest keys)
  "return the approx cardinality of the HyperLogLog(s)"
  (apply #'eredis-command-returning "pfcount" key keys))

(defun eredis-pfmerge(dest src &rest srcs)
  "merge all source keys into dest HyperLogLog"
  (apply #'eredis-command-returning "pfmerge" dest src srcs))

;; list commands

(defun eredis-llen(key &optional process)
  "length of list"
  (eredis-command-returning "llen" key process))

(defun eredis-lpop(key &optional process)
  "list pop first element"
  (eredis-command-returning "lpop" key process))

(defun eredis-lpush(key value &rest values)
  "Prepend value(s) to a list stored by KEY"
  (apply #'eredis-command-returning "lpush" key value values))

(defun eredis-rpush(key value &rest values)
  "Append value(s) to a list stored by KEY"
  (apply #'eredis-command-returning "rpush" key value values))

(defun eredis-lpushx(key value &optional process)
  "Prepend value(s) to a list stored by KEY if it doesn't exist already"
  (eredis-command-returning "lpushx" key value process))

(defun eredis-rpushx(key value &optional process)
  "Append value(s) to a list stored by KEY if it doesn't exist already"
  (eredis-command-returning "rpushx" key value  process))

(defun eredis-lindex(key index &optional process)
  "list element INDEX to a list stored by KEY"
  (eredis-command-returning "lindex" key index))

(defun eredis-blpop(key &rest rest)
  "blocking left pop of multiple lists, rest is actually as many keys as you want and a timeout"
  (apply #'eredis-command-returning "blpop" key rest))

(defun eredis-brpop(key &rest rest)
  "blocking right pop of multiple lists, rest is actually as many keys as you want and a timeout"
  (apply #'eredis-command-returning "brpop" key rest))

(defun eredis-lrange(key start stop  &optional process)
  "redis lrange"
  (eredis-command-returning "lrange" key start stop process))

(defun eredis-linsert(key position pivot value &optional process)
  "redis linsert"
  (eredis-command-returning "linsert" key position pivot value process))

(defun eredis-brpoplpush(source destination timeout &optional process)
  "redis brpoplpush"
  (eredis-command-returning "brpoplpush" source destination timeout process))

(defun eredis-rpoplpush(source destination timeout &optional process)
  "redis rpoplpush"
  (eredis-command-returning "rpoplpush" source destination process))

(defun eredis-lrem(key count value &optional process)
  "redis lrem"
  (eredis-command-returning "lrem" key count value process))

(defun eredis-lset(key index value &optional process)
  "redis lset"
  (eredis-command-returning "lset" key index value process))

(defun eredis-ltrim(key start stop &optional process)
  "redis ltrim"
  (eredis-command-returning "ltrim" key start stop process))

(defun eredis-rpop(key &optional process)
  "right pop of list"
  (eredis-command-returning "rpop" key process))

;;; set commands

(defun eredis-sadd(key member &rest members)
  "redis add to set"
  (apply #'eredis-command-returning "sadd" key member members))

(defun eredis-scard(key &optional process)
  "redis scard"
  (eredis-command-returning "scard" key process))

(defun eredis-sdiff(key &rest keys)
  "redis sdiff"
  (apply #'eredis-command-returning "sdiff" key keys))

(defun eredis-sdiffstore(destination key &rest keys)
  "redis sdiffstore"
  (apply #'eredis-command-returning "sdiffstore" destination key keys))

(defun eredis-sinter(key &rest keys)
  "redis sinter"
  (apply #'eredis-command-returning "sinter" key keys))

(defun eredis-sinterstore(destination key &rest keys)
  "redis sinterstore"
  (apply #'eredis-command-returning "sinterstore" destination key keys))

(defun eredis-sismember(key member &optional process)
  "redis sdiffstore"
  (eredis-command-returning "sismember" key member process))

(defun eredis-smembers(key &optional process)
  "redis smembers"
  (eredis-command-returning "smembers" key process))

(defun eredis-smove(source destination member &optional process)
  "redis smove"
  (eredis-command-returning "smove" source destination member process))

(defun eredis-spop(key &optional process)
  "redis spop"
  (eredis-command-returning "spop" key process))

(defun eredis-srandmember(key &optional process)
  "redis srandmember"
  (eredis-command-returning "srandmember" key process))

(defun eredis-srem(key member &rest members)
  "redis srem"
  (apply #'eredis-command-returning "srem" key member members))

(defun eredis-sunion(key &rest keys)
  "redis sunion"
  (apply #'eredis-command-returning "sunion" key keys))

(defun eredis-sunionstore(destination key &rest keys)
  "redis sunionstore"
  (apply #'eredis-command-returning "sunionstore" destination key keys))

;;; sorted set commands

(defun eredis-zadd(key score member &optional process)
  "redis zadd"
  (eredis-command-returning "zadd" key score member process))

(defun eredis-zcard(key &optional process)
  "redis zcard"
  (eredis-command-returning "zcard" key process))

(defun eredis-zcount(key min max &optional process)
  "redis zcount"
  (eredis-command-returning "zcount" key min max process))

(defun eredis-zincrby(key increment member &optional process)
  "redis zincrby"
  (eredis-command-returning "zincrby" key increment member process))

(defun eredis-zinterstore(destination numkeys key &rest rest)
  "redis zinterstore"
  (apply #'eredis-command-returning "zinterstore" destination numkeys key rest))

(defun eredis-zrange(key start stop &optional withscores process)
  "eredis zrange. withscores can be the string \"withscores\", the symbol 'withscores"
  (if (null withscores)
      (eredis-command-returning "zrange" key start stop process)
    (eredis-command-returning "zrange" key start stop withscores process)))

(defun eredis-zrangebyscore(key min max &rest rest)
  "eredis zrangebyscore"
  (apply #'eredis-command-returning "zrangebyscore" key min max rest))

(defun eredis-zrank(key member &optional process)
  "redis zrank"
  (eredis-command-returning "zrank" key member process))

(defun eredis-zrem(key member &optional process)
  "redis zrem"
  (eredis-command-returning "zrem" key member process))

(defun eredis-zremrangebyrank(key start stop &optional process)
  "redis zremrangebyrank"
  (eredis-command-returning "zremrangebyrank" key start stop process))

(defun eredis-zremrangebyscore(key min max &optional process)
  "redis zremrangebyscore"
  (eredis-command-returning "zremrangebyscore" key min max process))

(defun eredis-zrevrange(key start stop &optional withscores process)
  "eredis zrevrange. withscores can be the string \"withscores\", the symbol 'withscores"
  (if (null withscores)
      (eredis-command-returning "zrevrange" key start stop  process)
    (eredis-command-returning "zrevrange" key start stop withscores process)))

(defun eredis-zrevrangebyscore(key min max &rest rest)
  "eredis zrevrangebyscore"
  (apply #'eredis-command-returning "zrevrangebyscore" key min max rest))

(defun eredis-zrevrank(key member &optional process)
  "redis zrevrank"
  (eredis-command-returning "zrevrank" key member process))

(defun eredis-zscore(key member &optional process)
  "redis zscore"
  (eredis-command-returning "zscore" key member process))

(defun eredis-zunionstore(destination numkeys key &rest rest)
  "redis zunionstore"
  (apply #'eredis-command-returning destination numkeys key rest))

;;; pub/sub commands

;; Warning: these aren't working very well yet. Need to write a custom response handler 
;; to handle replies from the publish subscribe commands. They have differences, for 
;; example multiple bulk messages come at once. 

(defun eredis-publish(channel message &optional process)
  "eredis publish"
  (eredis-command-returning "publish" channel message process))

(defun eredis-subscribe(channel &rest channels)
  "eredis subscribe"
  (apply #'eredis-command-returning "subscribe" channel channels))

(defun eredis-psubscribe(pattern &rest patterns)
  "eredis psubscribe"
  (apply #'eredis-command-returning "psubscribe" pattern patterns))

(defun eredis-unsubscribe(channel &rest channels)
  "eredis unsubscribe"
  (apply #'eredis-command-returning "unsubscribe" channel channels))

(defun eredis-punsubscribe(pattern &rest patterns)
  "eredis punsubscribe"
  (apply #'eredis-command-returning "punsubscribe" pattern patterns))

(defun eredis-await-message(&optional process)
  "Not a redis command. After subscribe or psubscribe, call this
to poll each message and call unsubscribe or punsubscribe when
done. Other commands will fail with an error until then"
  (eredis-get-response process))

;; transaction commands

(defun eredis-discard(&optional process)
  "eredis discard"
  (eredis-command-returning "discard" process))

(defun eredis-multi(&optional process)
  "eredis multi"
  (eredis-command-returning "multi" process))

;; TODO this returns a multibulk which in turn will contain a sequence of responses to commands
;; executed. Best way to handle this is probably to return a list of responses
;; Also need to fix the parser to handle numeric results in a multibulk response
;; which is the same issue I'm seeing with publish/subscribe results
(defun eredis-exec( &optional process)
  "eredis exec"
  (eredis-command-returning "exec" process))

(defun eredis-watch(key &rest keys)
  "redis watch"
  (apply #'eredis-command-returning "watch" key keys))

(defun eredis-unwatch(&optional process)
  "redis unwatch"
  (eredis-command-returning "unwatch" process))

;; connection commands

(defun eredis-auth(password &optional process)
  "eredis auth"
  (eredis-command-returning "auth" password process))

(defun eredis-echo(message &optional process)
  "eredis echo"
  (eredis-command-returning "echo" message process))

(defun eredis-ping(&optional process)
  "redis ping"
  (interactive)
  (eredis-command-returning "ping" process))

(defun eredis-quit(&optional process)
  "redis ping"
  (interactive)
  (eredis-command-returning "quit" process))

(defun eredis-select(index &optional process)
  "redis select db with INDEX"
  (interactive)
  (eredis-command-returning "select" index process))

;;; server commands 

(defun eredis-bgrewriteaof(&optional process)
  (eredis-command-returning "bgrewriteaof" process))

(defun eredis-bgsave(&optional process)
  (eredis-command-returning "bgsave" process))

(defun eredis-config-get( &optional parameter prcoess)
  (eredis-command-returning "config" "get" parameter process))

(defun eredis-config-set(parameter value &optional process)
  (eredis-command-returning "config" "set" parameter value process))

(defun eredis-config-resetstat(&optional process)
  (eredis-command-returning "config" "resetstat" process))

(defun eredis-dbsize(&optional process)
  (eredis-command-returning "dbsize" process))

(defun eredis-debug-object(key &optional process)
  (eredis-command-returning "debug" "object" key process))

(defun eredis-debug-segfault(&optional process)
  (eredis-command-returning "debug" "segfault" process))

(defun eredis-flushall(&optional process)
  (eredis-command-returning "flushall" process))

(defun eredis-flushdb(&optional process)
  (eredis-command-returning "flushdb" process))

(defun eredis-info(&optional process)
  "Call Redis INFO and return a hash table of key value pairs"
  (->> (eredis-command-returning "info" process)
       (split-string)
       (--reduce-from (let ((keyvalue (split-string it ":")))
			(when (= 2 (length keyvalue))
			  (puthash (first keyvalue) (second keyvalue) acc))
			acc)
		      (make-hash-table :test 'equal))))
    
(defun eredis-lastsave( &optional process)
  (eredis-command-returning "lastsave" process))

;; TODO monitor opens up the *redis-buffer* and shows commands streaming 
;; but it does not yet follow along, they just go off the screen, so I need
;; to fix that.. probably broken
(defun eredis-monitor(&optional process)
  (let ((this-process (if process
			  process
			eredis--current-process)))
    (unwind-protect
        (progn
          (switch-to-buffer (process-buffer this-process))
          (goto-char (point-max))
          (eredis-buffer-message this-process "C-g to exit\n")
          (process-send-string this-process "monitor\r\n")
          (let ((resp nil))
            (while t
              (redisplay t)
              (sleep-for 1)
              ;;(recenter-top-bottom 'top)
              (let ((resp (eredis-get-response 3 5)))
                (when resp
                  (eredis-buffer-message this-process eredis-response))))))
      ;; when the user hits C-g we send the quit command to exit
      ;; monitor mode
      (progn
        (eredis-quit)
        (eredis-kthxbye)))))

(defun eredis-save( &optional process)
  (eredis-command-returning "save" process))

(defun eredis-shutdown()
  "shutdown redis server"
  (interactive)
  ;; Note that this just sends the command and does not wait for or parse the response
  ;; since there shouldn't be one
  (if (and eredis--current-process (eq (process-status eredis--current-process) 'open))
      (progn 
        (process-send-string eredis--current-process (eredis-build-request "shutdown"))
        (eredis-kthxbye))))

(defun eredis-slaveof(host port &optional process)
  (eredis-command-returning "slaveof" host port process))

(defun eredis-slowlog-len(&optional process)
  (eredis-command-returning "slowlog" "len" process))

(defun eredis-slowlog-get(&optional most-recent process)
  (let ((recent (if most-recent
		    most-recent
		  100)))
    (eredis-command-returning "slowlog" "get" recent process)))

(defun eredis-sync(&optional process)
  (eredis-command-returning "sync" process))

(defun eredis-lolwut(&optional process)
  "Returns LOLWUT response (version 5 onwards)"
  (interactive)
  (eredis-command-returning "lolwut" process))

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
                  (forward-line))))))))
    (if (> (hash-table-count mset-param) 0)
        (eredis-mset mset-param)
      nil)))

(defun eredis-org-table-from-keys(keys &optional process)
  "For each of KEYS lookup their type in redis and populate an org table 
containing a row for each one"
  (eredis--org-table-from-list  '("Key" "Type" "Values"))
  (dolist (key keys)
    (let ((type (eredis-type key process)))	 
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
      (eredis--org-table-from-list (apply #' list key "list" items)))))

(defun eredis-org-table-from-zset(key &optional withscores)
  "create an org table populated with the members of the zset KEY"
  (let ((items (eredis-zrange key 0 -1 withscores)))
    (when items
      (eredis--org-table-from-list (apply #'list key "zset" items)))))

(defun eredis-org-table-from-set(key)
  "create an org table populated with the members of the set KEY"
  (let ((members (eredis-smembers key)))
    (when members
      (eredis--org-table-from-list (apply #'list key "set" members)))))

(defun eredis-org-table-from-hash(key)
  "org table populated with the hash of KEY"
  (let ((m (eredis-hgetall key)))
    (when m
      (setf m (eredis--unflatten-map m))
      (eredis--org-table-from-map m))))

(defun eredis-org-table-from-string(key)
  "create a small org table from the key, and it's string value"
  (let ((val (eredis-get key)))
    (when val
      (eredis--org-table-from-list (list key "string" val)))))

(defun eredis-org-table-from-pattern(pattern)
  "Search Redis for the pattern of keys and create an org table from the results"
  (let ((keys (eredis-keys pattern)))
    (if keys
        (eredis-org-table-from-keys keys))))

(defun eredis--org-table-from-list(l)
  "Create an org-table from a list"
  (if (listp l)
      (let ((beg (point)))
        (eredis--insert-list l)
        (org-table-convert-region beg (point) '(4))
        (forward-line))))

(defun eredis--org-table-from-map(m)
  "Create an org-table from a map of key value pairs"
  (let ((beg (point)))
    (if (hash-table-p m)
        (progn
          (eredis--insert-map m)
          (org-table-convert-region beg (point))))))

(defun eredis-org-table-get-field-clean(col)
  "Get a field in org table at column COL and strip any leading or
trailing whitespace using `string-trim'. Also strip text properties"
  (let ((field (org-table-get-field col)))
    (let ((chomped (string-trim field)))
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
          (forward-line))))
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
  "With point in an org table set the key and value"
  (interactive)
  (let ((keyvalue (eredis-org-table-row-to-key-value-pair)))
    (eredis-set (car keyvalue) (cdr keyvalue))))

(provide 'eredis)

;;; eredis.el ends here
