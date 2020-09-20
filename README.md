# logjam

[![Build Status][gh-actions-badge]][gh-actions] [![LFE Versions][lfe badge]][lfe] [![Erlang Versions][erlang badge]][versions] [![Tags][github tags badge]][github tags]

[![Project Logo][logo]][logo-large]

*A custom formatter for the Erlang logger application that produces human-readable output*

## Why?

The default formatter for Erlang is very difficult to read at a glance making troubleshooting and debugging of applications via log output during development phases rather cumbersome or even difficult. 

With logjam, you get something nice and easy to read:

[![][screenshot]][screenshot]

## Usage

It is recommended that if you are providing a library, you do not add this
project as a dependency. A code formatter of this kind should be added to a
project in its release repository as a top-level final presentational concern.

Once the project is added, replace the formatter of the default handler (or add a custom handler) for structured logging to your `sys.config` file:

```erlang
[
 {kernel, [
    {logger, [
        {handler, default, logger_std_h,
         #{formatter => {logjam, #{
            colored => true,
            time_designator => $\s,
            time_offset => "",
            time_unit => second,
            strip_tz => true,
            level_capitalize => true
          }}}
        }
    ]},
    {logger_level, info}
 ]}
].
```

This configuration was used to produce the screenshot above.

Note that if you are building a release, you will need to manually add
the `logjam` dependency to your `relx` configuration, since it is
technically not a direct dependency of any application in your system.

Test
----

    $ rebar3 check

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
