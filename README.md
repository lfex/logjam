# logjam

*A Logging Library for LFE*


[![][logo]][logo-large]

[logo]: resources/images/logjam-crop-small.png
[logo-large]: resources/images/logjam.jpg


## Table of Contents

* [Introduction](#introduction-)
* [Installation](#installation-)
* [Setup](#setup-)
  * [Basic Configuration](#basic-configuration-)
  * [Colour Support](#colour-support-)
  * [Starting Logjam](#starting-logjam-)
* [Usage](#usage-)
  * [As Includes](#as-includes-)
  * [Via mod func](#via-mod-func-)
  * [Log-level Functions](#log-level-functions-)
  * [Dynamically Updating Log Levels](#dynamically-updating-log-levels-)


## Introduction [&#x219F;](#table-of-contents)

The preferred logging library in Erlang is
[lager](https://github.com/basho/lager). However, it doesn't work
out of the box with LFE, due to the fact that it uses parse transforms (the LFE
compiler uses Core Erlang and does not generate Erlang abstract terms, which
are how Erlang parse transforms work).

As such, we needed a way to easily use lager from LFE. So here you have it: a
a pile of logs for the LFE community, in a river of LFE code ...

[![][screenshot]][screenshot]

[screenshot]: resources/images/screenshot.png


## Installation [&#x219F;](#table-of-contents)

Just add it to your ``rebar.config`` deps:

```erlang
  {deps, [
    ...
    {logjam, ".*",
      {git, "git@github.com:lfex/logjam.git", "master"}}
      ]}.
```

And then do the usual:

```bash
    $ make compile
```


## Setup [&#x219F;](#table-of-contents)


### Basic Configuration [&#x219F;](#table-of-contents)

First things first, make sure you have an ``lfe.config`` file with the
appropriate lager configuration options set. For instance:

```cl
#(logging (
   #(log-level info)
   #(backend lager)
   #(options (#(lager_console_backend info)
              #(lager_file_backend (
                #(file "log/error.log")
                #(level error)
                #(size 10485760)
                #(date "$D0")
                #(count 5)))
              #(lager_file_backend (
                #(file "log/console.log")
                #(level info)
                #(size 10485760)
                #(date "$D0")
                #(count 5)))))))
```

Any legal lager configuration will work (as long as you translate it into LFE
syntax first!).

If you'd like to use use the custom log formatter (modeled after logging formats common in Clojure applications), you can configure your handler like the following:

```cl
#(logging (
   #(log-level debug)
   #(colored true)
   #(backend lager)
   #(options (#(lager_console_backend (
                debug
                #(logjam-formatter
                  (date " " time " [" pid "] [" severity "] " message "\n"))))
               ...))))

```


### Color Support [&#x219F;](#table-of-contents)

Logjam supports colourd logging -- you just need to enable it in your
project's ``lfe.config`` file. See the logjam ``lfe.config`` for example
configuration.

To not have ANSI colors as part of the output, be sure to use ``#(colored false)``.

If you'd like to use your own colors, you can start with this template:

```cl
#(logging (
   #(log-level debug)
   #(colored true)
   #(colors (#(timestamp (color green))
             #(process (color cyan))
             #(date (color green))
             #(time (color green))
             #(modfunc (color yellow))
             #(message (color green))
             #(debug (color greenb))
             #(info (color blue))
             #(notice (color cyan))
             #(warning (color yellow))
             #(error (color red))
             #(critical (color yellowb))
             #(alert (color magentab))
             #(emergency (color redb))))
  ...))

The confguration value for the color is a list of module and func that will be called to wrap color around the log text indicated by the configuration's key (e.g., timestamp, debug, etc.)


### Starting Logjam [&#x219F;](#table-of-contents)

Next, start the logjam application (which will perform additional setup and start lager):

```cl
> (logjam:start)
ok
2015-12-17 13:08:04.163 [<0.7.0>] [info] Application lager started on ...
```

ou may or may not see a message logged to the console, depending upon your log-level setting in ``lfe.config``.


## Usage [&#x219F;](#table-of-contents)

The ``logjam`` module includes all the logging functions you may be used to in other logging frameworks. These allow you do make calls like the following:

```cl
(logjam:info "An informative message")
(logjam:info "A ~p message" '(great))
(logjam:info 'my-mod 'my-func/2 "Message from function ...")
(logjam:info 'my-mod 'my-func/2 "Message from a ~p function ..." '(simple))
```

If you would like to have your module and function populated in the log more or less automatically, you can call logjam log functions with the "caller" tuple:

```cl
(logjam:info `#(c ,(logjam:caller)) "An informative message")
(logjam:info `#(c ,(logjam:caller)) "A ~p message" '(great))
```

More details on the logging functions are given below.


### Log-level Functions [&#x219F;](#table-of-contents)

Now you'll be able to use logjam. The following log types are defined:
 * ``debug``
 * ``info``
 * ``notice``
 * ``warning``
 * ``error`` (supported by both sets of ``error`` and ``err`` functions)
 * ``critical``
 * ``alert``
 * ``emergency``

Each of these has arity 1, 2, 3, and 4 functions of the same name:
* arity 1: pass a message
* arity 2: pass an ``(io_lib:format ...)`` format string and arguments for the
  format string OR pass a caller tuple and a message
* arity 3: pass a module, a function, and a message OR pass a caller tuple, a format string, and args
* arity 4: pass a module, a function, an ``(io_lib:format ...)`` format string,
  and arguments for the format string

Examples:

```cl
> (logjam:info "wassup?")
ok
> 2015-12-17 13:44:06.912 [<0.46.0>] [info] wassup?
```

```cl
> (logjam:critical "~s~shey!" '("a " "critical thing, hey-"))
ok
> 2015-12-17 13:44:31.412 [<0.46.0>] [critical] a critical thing, hey-hey!
```

```cl
> (logjam:notice `#(c ,(logjam:caller)) "You better check this out ...")
ok
> 2015-12-17 13:49:10.235 [<0.46.0>] [notice] lists:map/2 You better check this out ...

```

```cl
> (logjam:notice (MODULE) 'my-func "You better check this out ...")
ok
> 2015-12-17 13:45:12.158 [<0.46.0>] [notice] -no-module:my-func You better check this out ...
```

```cl
> (logjam:alert `#(c ,(logjam:caller)) "~s~shey!" '("whoa! " "red alert, "))
ok
> 2015-12-17 13:48:08.277 [<0.46.0>] [alert] lists:map/2 whoa! red alert, hey!
```

```cl
> (logjam:alert (MODULE) 'my-func "~s~shey!" '("whoa! " "red alert, "))
ok
> 2015-12-17 13:45:31.196 [<0.46.0>] [alert] -no-module:my-func whoa! red alert, hey!
```


### Dynamically Updating Log Levels [&#x219F;](#table-of-contents)

logjam provides the following wrappers for this same functionality in lager:
 * ``logjam:set-level/1`` - set the level of the console backend
 * ``logjam:set-level/2`` - set the log level of a given backend
 * ``logjam:set-level/3`` - set the log level of a given backend's' logfile

Examples:

```cl
> (logjam:set-level 'debug)
ok
 ```

```cl
> (logjam:set-level 'lager_console_backend 'debug)
ok
```

```cl
> (logjam:set-level 'lager_file_backend "log/console.log" 'debug)
21:32:03.894 [notice] Changed loglevel of log/console.log to debug
ok
```

```cl
(logjam:set-level 'lager_file_backend "log/error.log" 'warning)
21:34:32.131 [notice] Changed loglevel of log/error.log to warning
ok
```
