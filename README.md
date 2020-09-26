# logjam

[![Build Status][gh-actions-badge]][gh-actions] [![LFE Versions][lfe badge]][lfe] [![Erlang Versions][erlang badge]][versions] [![Tags][github tags badge]][github tags]

[![Project Logo][logo]][logo-large]

*A custom formatter for the Erlang logger application that produces human-readable output*

## Why?

The default formatter for Erlang is very difficult to read at a glance making troubleshooting and debugging of applications via log output during development phases rather cumbersome or even difficult. 

With logjam, you get something nice and easy to read:

[![][screenshot]][screenshot]

## About

Previous versions of logjam wrapped older versions of the lager logging library. As of Erlang 21.0, Erlang has a new (and excellent) logging library, yet the same output is used (and thus the same problems regarding developer readability persist). The [flatlog][flatlog] library was created to provide structured logging support for the new logger library, and this project was used as the basis of logjam 1.0. Logjam provides additional configuration on top of the code inherited from flatlog, and while it does support much of the same structured logging that flatlog provides, its goals are completely different.

## Usage

It is recommended that if you are providing a library, you do not add this
project as a dependency. A code formatter of this kind should be added to a
project in its release repository as a top-level final presentational concern.

### Configuration

Once the project is added, replace the formatter of the default handler (or add a custom handler) for structured logging to your `sys.config` file:

```erlang
[
  {kernel, [
      {logger, [
          {handler, default, logger_std_h,
          #{level => info,
            formatter => {logjam,
              #{colored => true,
                time_designator => $\s,
                time_offset => "",
                time_unit => second,
                strip_tz => true,
                level_capitalize => true
              }
            }
           }
          }
      ]}
  ]}
].
```

This configuration was used to produce the screenshot above.

Note that if you are building a release, you will need to manually add
the `logjam` dependency to your `relx` configuration, since it is
technically not a direct dependency of any application in your system.

### Logging Calls

If you call the logger functions directly, your log output will not have reference to the module, function, arity, or line number where the logging call was made. For this info, you need to use the `logger` or `logjam` macros.

Include the `logger` calls with either:

```lisp
(include-lib "kernel/include/logger.hrl")
```

or

```erlang
-include_lib("kernel/include/logger.hrl").
```

This will let you call the logging macros such as `(LOG_DEBUG "my message")` or `?LOG_DEBUG("my message").`

If you'd like to use the `logjam` macros, use these instead`:

```lisp
(include-lib "logjam/include/logjam.hrl")
```

or

```erlang
-include_lib("logjam/include/logjam.hrl").
```

This will let you call the logging macros such as `(log-debug "my message")` or `?'log-debug'("my message").`

Here is some example LFE usage:

```lisp
(log-debug "This is a debug-level message")
(log-info "This is an info-level message")
(log-notice "This is a notice-level message")
(log-warn "This is a warning-level message")
(log-error "This is an error-level message")
(log-crit "This is a critical-level message")
(log-alert "This is an alert-level message")
(log-emergency "This is an emergency-level message")
(log-info #m(some "structured" logging "examples" might "be useful too")))
```

## Test

```bash
$ rebar3 check
```

<!-- Named page links below: /-->

[logo]: priv/images/logjam-crop-small.png
[logo-large]: priv/images/logjam.jpg
[screenshot]: priv/images/screenshot.png
[org]: https://github.com/lfex
[github]: https://github.com/lfex/logjam
[gitlab]: https://gitlab.com/lfex/logjam
[gh-actions-badge]: https://github.com/lfex/logjam/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfex/logjam/actions
[lfe]: https://github.com/rvirding/lfe
[lfe badge]: https://img.shields.io/badge/lfe-1.3.0-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-17.5%20to%2022.0-blue.svg
[versions]: https://github.com/lfex/logjam/blob/master/.travis.yml
[github tags]: https://github.com/lfex/logjam/tags
[github tags badge]: https://img.shields.io/github/tag/lfex/logjam.svg
[github downloads]: https://img.shields.io/github/downloads/lfex/logjam/total.svg
[flatlog]: https://github.com/ferd/flatlog
