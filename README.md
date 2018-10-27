# eredis - An Emacs client API for Redis

## Introduction

eredis is a programming API so you can connect to Redis servers and have access to most of the API via emacs lisp. It also has some functionality to load keys into an org table, where you can edit the values and then push them back to the server.

Currently this package does not support Windows Redis servers due to differences in newline handling. This is an open issue and pull requests are welcome.

## Installation

### From source

Download the latest version via svn or from the downloads page. Add the path you downloaded it to in your .emacs file:

```
(add-to-list 'load-path "~/eredis/")

(require 'eredis)
```

### From Melpa

[Melpa](https://melpa.org/)

    M-x package-install eredis

## Usage

First connect to the server. Some of the commands are interactive but all work from the emacs lisp REPL. To get that:

`M-x ielm`

A redis session begins by connecting to a server:

`(eredis-connect "localhost" "6379")`

and ends by closing the connection:

`(eredis-disconnect)`

Most commands receive a string response from Redis such as eredis-ping, which returns "PONG". Some return lists of strings, and others return maps of key value pairs. I've tried to follow the principle of least surprise when implementing each call.

Some examples of basic usage:

`(eredis-set "key" "value")`
"ok"

`(eredis-get "key")` 
"value"

## Monitor Mode

All commands work as you would expect, except for monitor. Monitor starts to show all the commands going to the Redis server, and as such does not fit with a single asynchronous API call. What I do instead is to switch you to the redis buffer where you can watch the commands. Hit C-g to exit this mode. You will need to reconnect to the Redis server once you are done, because the QUIT command that exits the client from monitor mode seems to close the connection too.

`(eredis-monitor)`

## Org table mode

This is the stuff I'm most excited about that sets the emacs client apart from other client libs for Redis. Interacting with the data in Redis in ad hoc, super easy to create tables, seems like a potentially very powerful and fun tool.

Support so far includes creating org tables from various data types:

And sending data from an org table (assumes string types only for now but will be expanded)

```
(eredis-org-table-from-set "s1")

(eredis-org-table-from-zset "z1")

(eredis-org-table-from-zset "z1" 'withscores)

(eredis-org-table-from-hash "testhash1")

(eredis-org-table-from-list "l1")

(eredis-org-table-from-pattern "configparam..server1")

(defun eredis-org-table-mset()

"with point in an org table convert the table to a map and send it to redis with mset"

(defun eredis-org-table-msetnx()

"with point in an org table convert the table to a map and send it to redis with msetnx"

(defun eredis-org-table-row-set()

"with point in an org table set the key and value"
```

More to come

## More caveats

Almost the entire Redis API works no problem, except for the publish subscribe commands, which I have a little more work to do on. Same goes for MULTI and EXEC. These commands work, but the output of the final EXEC is not correctly parsed yet.

Supported Commands

Use emacs function help `C-h f eredis- TAB` should give you a good idea what you can do...

```
;; key commands 
(defun eredis-del(key &rest keys)
(defun eredis-exists(key)
(defun eredis-expire(key seconds)
(defun eredis-expireat(key unix-time)
(defun eredis-keys(pattern)
(defun eredis-move(key db)
;; http://redis.io/commands/object'>http://redis.io/commands/object
(defun eredis-object(subcommand &rest args)
(defun eredis-persist(key)
(defun eredis-randomkey()
(defun eredis-rename(key newkey)
(defun eredis-renamenx(key newkey)
(defun eredis-sort(key &rest args)
(defun eredis-ttl(key)
(defun eredis-type(key)
;; string commands
(defun eredis-append(key value)
(defun eredis-decr(key)
(defun eredis-decrby(key decrement)
(defun eredis-get(key)
(defun eredis-getbit(key offset)
(defun eredis-getrange(key start end)
(defun eredis-getset(key value)
(defun eredis-incr(key)
(defun eredis-incrby(key increment)
(defun eredis-mget(keys)
(defun eredis-mset(m)
(defun eredis-msetnx(m)
(defun eredis-set(k v)
(defun eredis-setbit(key offset value)
(defun eredis-setex(key seconds value)
(defun eredis-setnx(k v)
(defun eredis-setrange(key offset value)
(defun eredis-strlen(key)
;; hash commands
(defun eredis-hget(key field)
(defun eredis-hset(key field value)
(defun eredis-hsetnx(key field value)
(defun eredis-hmget(key field &rest fields)
(defun eredis-hmset(key m)
(defun eredis-hincrby(key field integer)
(defun eredis-hexists(key field)
(defun eredis-hdel(key field)
(defun eredis-hlen(key)
(defun eredis-hkeys(key)
(defun eredis-hvals(key)
(defun eredis-hgetall(key)
;; list commands
(defun eredis-llen(key)
(defun eredis-lpop(key)
(defun eredis-lpush(key value &rest values)
(defun eredis-rpush(key value &rest values)
(defun eredis-lpushx(key value)
(defun eredis-rpushx(key value)
(defun eredis-lindex(key index)
(defun eredis-blpop(key &rest rest)
(defun eredis-brpop(key &rest rest)
(defun eredis-lrange(key start stop)
(defun eredis-linsert(key position pivot value)
(defun eredis-brpoplpush(source destination timeout)
(defun eredis-rpoplpush(source destination timeout)
(defun eredis-lrem(key count value)
(defun eredis-lset(key index value)
(defun eredis-ltrim(key start stop)
(defun eredis-rpop(key)
;; set commands
(defun eredis-sadd(key member &rest members)
(defun eredis-scard(key)
(defun eredis-sdiff(key &rest keys)
(defun eredis-sdiffstore(destination key &rest keys)
(defun eredis-sinter(key &rest keys)
(defun eredis-sinterstore(destination key &rest keys)
(defun eredis-sismember(key member)
(defun eredis-smembers(key)
(defun eredis-smove(source destination member)
(defun eredis-spop(key)
(defun eredis-srandmember(key)
(defun eredis-srem(key member &rest members)
(defun eredis-sunion(key &rest keys)
(defun eredis-sunionstore(destination key &rest keys)
;; sorted set commands
(defun eredis-zadd(key score member)
(defun eredis-zcard(key)
(defun eredis-zcount(key min max)
(defun eredis-zincrby(key increment member)
(defun eredis-zinterstore(destination numkeys key &rest rest)
(defun eredis-zrange(key start stop &optional withscores)
(defun eredis-zrangebyscore(key min max &rest rest)
(defun eredis-zrank(key member)
(defun eredis-zrem(key member)
(defun eredis-zremrangebyrank(key start stop)
(defun eredis-zremrangebyscore(key min max)
(defun eredis-zrevrange(key start stop &optional withscores)
(defun eredis-zrevrangebyscore(key min max &rest rest)
(defun eredis-zrevrank(key member)
(defun eredis-zscore(key member)
(defun eredis-zunionstore(destination numkeys key &rest rest)
;; pub/sub commands
;; Warning: these aren't working very well yet. Need to write a custom response handler 
;; to handle replies from the publish subscribe commands. They have differences, for 
;; example multiple bulk messages come at once. 
(defun eredis-publish(channel message)
(defun eredis-subscribe(channel &rest channels)
(defun eredis-psubscribe(pattern &rest patterns)
(defun eredis-unsubscribe(channel &rest channels)
(defun eredis-punsubscribe(pattern &rest patterns)
(defun eredis-await-message()
;; transaction commands
(defun eredis-discard()
(defun eredis-multi()
;; TODO this returns a multibulk which in turn will contain a sequence of responses to commands
;; executed. Best way to handle this is probably to return a list of responses
;; Also need to fix the parser to handle numeric results in a multibulk response
;; which is the same issue I'm seeing with publish/subscribe results
(defun eredis-exec()
(defun eredis-watch(key &rest keys)
(defun eredis-unwatch()
;; connection commands
(defun eredis-auth(password)
(defun eredis-echo(message)
(defun eredis-ping()
(defun eredis-quit()
(defun eredis-select(index)
;; server commands 
(defun eredis-bgrewriteaof()
(defun eredis-bgsave()
(defun eredis-config-get(parameter)
(defun eredis-config-set(parameter value)
(defun eredis-config-resetstat()
(defun eredis-dbsize()
(defun eredis-debug-object(key)
(defun eredis-debug-segfault()
(defun eredis-flushall()
(defun eredis-flushdb()
;; TODO the response from this is a single bulk response but it could be further parsed into a map
;; It uses : to delimit the keys from values
(defun eredis-info()
(defun eredis-lastsave()
;; TODO this needs a bit of work. This will get only the first command. Need to enter a loop 
;; and let the user terminate monitor mode. Also should handle repeated status type 
;; responses until monitor is done. Dumping to a buffer may make sense here
;; and with the subcribe/publish stuff
(defun eredis-monitor()
(defun eredis-save()
;; TODO this returns the last response again. Should handle the connection 
;; termination correctly
(defun eredis-shutdown()
(defun eredis-slaveof(host port)
;; This is in the docs but not in the server I'm using 
;; (defun eredis-slowlog-len()
;; (eredis-command-returning-integer "slowlog" "len"))
(defun eredis-sync()
```

