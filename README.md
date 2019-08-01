# eredis - An Emacs client API for Redis

## Introduction

eredis is a redis client written in Emacs Lisp

Melpa Status

* MELPA ![https://melpa.org/#/request](https://melpa.org/#/request) |melpa-badge|
* MELPA Stable ![https://stable.melpa.org/#/request](https://stable.melpa.org/#/request) |melpa-stable-badge|

Features

* Almost everything implemented 
* Redis 5.0 support including LOLWUT
* Multiple connections to multiple Redis servers
* Integration with org-mode
* Handles multibyte characters correctly
* Buffer per connection shows all Redis output

Currently this package does not support Windows Redis servers due to differences in newline handling. This is an open issue and pull requests are welcome. I do not soil my hands with Windows machines, if I can avoid it.

## Installation

### From source

Download the latest version via svn or from the downloads page. Add the path you downloaded it to in your .emacs file:

```
(add-to-list 'load-path "~/eredis/")

(require 'eredis)
```

You can find a test suite in the tests folder that uses ert.

### From Melpa

[Melpa](https://melpa.org/)

    M-x package-install eredis

## Usage

First connect to the server. Some of the commands are interactive but all work from the emacs lisp REPL. To get that:

`M-x ielm`

A redis session begins by connecting to a server. It returns a process which you can use for further operations.

`(setq p1 (eredis-connect "localhost" "6379"))`

and ends by closing the connection:

`(eredis-disconnect p1)`

Most commands receive a string response from Redis such as eredis-ping, which returns "PONG". Some return lists of strings, and others return maps of key value pairs. I've tried to follow the principle of least surprise when implementing each call.

Some examples of basic usage:

`(eredis-set "key" "value")`
"ok"

`(eredis-get "key")` 
"value"

`(eredis-info)`

Returns a hash table of the key values that represent the Redis info response.

## Monitor Mode

All commands work as you would expect, except for monitor. Monitor starts to show all the commands going to the Redis server, and as such does not fit with a single asynchronous API call. What I do instead is to switch you to the redis buffer where you can watch the commands. Hit C-g to exit this mode. You will need to reconnect to the Redis server once you are done, because the QUIT command that exits the client from monitor mode seems to close the connection too.

`(eredis-monitor)`

## Org table mode

You can pull a range of values from the Redis server and create an org table, which can be further manipulated, values changed and so on...

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

## License etc

See the source code in `eredis.el` for licensing information, contributor thanks etc

