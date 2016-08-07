# eredis.el, a low level redis command library in elisp.

eredis isn't a comint style redis command line. Rather it's a
programming to to write code that accesses data stored in redis. It's
useful for monitoring and management tools if you build products that
use redis.

## Commands

All current commands should be implemented including pub/sub (with
some caveat) Currently there's no support (or interest) in sentinel
support.

## History and Contact

This is a pre-closing-of-google-code fork of work done by Justin
Heyes-Jones that has been modified and extended somewhat. Some of the
issues, noteably around pub/sub haven't been dealt with due to my lack
of an itch.

The original is now on [github](https://github.com/justinhj/eredis)
but seems to be moribund.
