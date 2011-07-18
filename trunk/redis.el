;; eredis... A simple emacs interface to redis
;; http://redis.io/topics/protocol
;; see unified protocol to see what's going on in the command sends
;; basically * + number of strings then the length of each string preceded by $
;; todo generalise command sending 
;; todo data structure representing API and expected response

;; usage examples
;; first connect to a server
; (redis-hai host port nowait) 
; last arg is whether to wait for connection or connect asynchronously. defaults to nil 
; (redis-hai "127.0.0.1" 6379) 
; (redis-hai "127.0.0.1" 6379 t)
; close connection
; (redis-kthxbye) 
; returns true if you can ping redis
; (redis-ping)
; (redis-info)
; (redis-get "user:66971:last-login")
; for reference
; (process-list)
; (list-processes)
; (delete-process (get-process "redis"))
; (network-interface-list)
; (network-interface-info "en1")	  
;(insert (synchronous-ping))

(require 'org-table)

(defvar *redis-process* nil)
(defvar *redis-state* nil)
(defvar *redis-response* nil)
(defvar *redis-timeout* 10) ; seconds 

;; UTILS

(defun two-lists-to-map(lst1 lst2)
  "take a list of keys LST1 and a list of values LST2 and make a hashmap"
  (let ((retmap (make-hash-table :test 'equal)))
    (mapc (lambda (n) (puthash (car n) (cdr n) retmap))
	  (map 'list (lambda (a b) (cons a b)) lst1 lst2))
    retmap))

;; helper function from http://www.emacswiki.org/emacs/ElispCookbook#toc5
(defun chomp (str)
  "chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun insert-map(m)
  "insert a map M of key value pairs into the buffer"
  (maphash (lambda (a b) (insert (format "%s,%s\n" a b))) m))

(defun redis-map-keys(key-expr)
  "take a glob expression and return the key/values of matching keys"
  (let ((keys (redis-keys key-expr)))
    (if keys
	(let ((values (redis-mget keys)))
	  (two-lists-to-map keys values))
      nil)))

(defun redis-buffer-message(process message)
  "print a message to the redis process buffer"
  (save-excursion 
    (set-buffer (process-buffer process))
    (insert message)))

(defun redis-sentinel(process event)
  "sentinal function for redis network process which monitors for events"
  (redis-buffer-message process (format "sentinel event %s" event))
  (cond 
   ((string-match "open" event)
    (setq *redis-state* 'open))))

(defun redis-filter(process string)
  "filter function for redis network process, which receives output"
  (setq *redis-response* string))

(defun redis-delete-process()
  (when *redis-process*
    (delete-process *redis-process*)
    (setq *redis-state* 'closed)))

(defun redis-hai(host port &optional no-wait)
  (redis-delete-process)
  (setq *redis-state* 'opening)
  (let ((p 
	 (make-network-process :name "redis"
			       :host host
			       :service port
			       :nowait no-wait
			       :filter #'redis-filter
			       :keepalive t
			       :linger t
			       :sentinel #'redis-sentinel
			       :buffer (get-buffer-create "*redis*"))))
    (if p
	(progn
	  (if (null no-wait)
	      (setf *redis-state* 'open))
	  (setf *redis-process* p)))))
     
(defun redis-kthxbye()
  (redis-delete-process))

(defun redis-ping()
  (if (and *redis-process* (eq *redis-state* 'open))
      (progn 
	(process-send-string *redis-process* "PING\r\n")
	(let ((resp (get-redis-response)))
	  (if (string-match "+PONG" resp)
	    t
	    nil)))))

(defun redis-get(key)
  (if (and *redis-process* (eq *redis-state* 'open))
      (process-send-string *redis-process* (format "GET %s\r\n" key))))

(defun redis-info()
  (if (and *redis-process* (eq *redis-state* 'open))
      (progn
	(process-send-string *redis-process* "INFO\r\n")
	(let ((resp (get-redis-response)))
	  (insert resp))))) ; todo need to handle bulk reply (simpler version of multi bulk)

(defun get-redis-response()
  "await response from redis and store it"
  (if (accept-process-output *redis-process* *redis-timeout* 0 t)
      *redis-response*
    nil))

(defun redis-parse-multi-bulk(resp)
  "parse the redis multi bulk response RESP and return the list of results"
  (if (< (length resp) 2)
      nil
    (let ((elements (split-string resp "\r\n" t)))
      (let ((count (string-to-number (subseq (first elements) 1)))
	    (return-list nil))
	(if (> count 0)
	    (dolist (item (rest elements))
	      (if (/= ?$ (string-to-char item))
		  (setf return-list (cons item return-list)))))
	(reverse
	 return-list)))))

; http://redis.io/commands/keys

(defun redis-keys(pattern)
  "returns a list of keys where the key matches the provided
pattern. see the link for the style of patterns"
  (process-send-string *redis-process* (concat "KEYS " pattern "\r\n"))
  (let ((r (get-redis-response)))
    (redis-parse-multi-bulk r)))

(defun redis-mget(keys)
  "return the values of the specified keys, or nil if not present"
  (process-send-string *redis-process* (concat "MGET " (mapconcat 'identity keys " ") "\r\n"))
  (let ((r (get-redis-response)))
    (redis-parse-multi-bulk r)))

(defun redis-get-map(keys)
  "given a map M of key/value pairs, go to Redis to retrieve the values and set the 
value to whatever it is in Redis (or nil if not found)"
  (let ((num-args (1+ (hash-table-count m))))
    (let ((command (format "*%d\r\n$4\r\nMGET\r\n" num-args))
	  (key-value-string "")))
      (maphash (lambda (k v)
		 (setf key-value-string (concat key-value-string (format "$%d\r\n%s\r\n" (length k) k))))
	       m)
      (process-send-string *redis-process* (concat command key-value-string))
      (get-redis-response)))

(defun redis-mset(m)
  "set the keys and values of the map M in Redis"
  (let ((num-args (1+ (* 2 (hash-table-count m)))))
    (let ((command (format "*%d\r\n$4\r\nMSET\r\n" num-args))
	  (key-value-string ""))
      (maphash (lambda (k v)
		 (setf key-value-string (concat key-value-string (format "$%d\r\n%s\r\n$%d\r\n%s\r\n" (length k) k (length v) v))))
	       m)
      (process-send-string *redis-process* (concat command key-value-string))
      (let ((resp (get-redis-response)))
	(if (string-match "+OK" resp)
	    t
	  nil)))))

(defun redis-mset-region(beg end delimiter) 
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
	      (if (or 
		   (null key)
		   (null value))
		  (setf done t)
		(progn
		  (puthash key value mset-param)
		  (next-line))))))))
    (if (> (hash-table-count mset-param) 0)
	(redis-mset mset-param)
      nil)))

(defun redis-org-table-from-pattern(pattern)
  (interactive "sPattern: ")
  (let ((m (redis-map-keys pattern)))
    (if m
	(org-table-from-map m)
      (message (format "No keys found for pattern %s" pattern)))))

(defun org-table-from-map(m)
  (let ((beg (point)))
    (if (hash-table-p m)
	(progn
	  (insert-map m)
	  (org-table-convert-region beg (point))))))

(defun redis-org-table-get-field-clean(col)
  "Get a field in org table at column COL and strip any leading or
trailing whitespace using chomp. Also strip text properties"
  (let ((field (org-table-get-field col)))
    (let ((chomped (chomp field)))
      (set-text-properties 0 (length chomped) nil chomped)
      chomped)))

(defun redis-org-table-to-map()
  "Walk an org table and convert the first column to keys and the second 
column to values in an elisp map"
  (let ((retmap (make-hash-table :test 'equal)))
    (save-excursion
      (let ((beg (org-table-begin))
	    (end (org-table-end)))
	(goto-char beg)
	(while (> end (point))
	  (let ((key (redis-org-table-get-field-clean 1))
		(value (redis-org-table-get-field-clean 2)))
	    (when (and key value)
	      (puthash key value retmap)))
	  (next-line))))
    retmap))

(defun redis-org-table-mset()
  "with point in an org table convert the table to a map and send it to redis with mset"
  (interactive)
  (let ((m (redis-org-table-to-map)))
    (redis-mset m)))

(provide 'eredis)