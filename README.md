# lumberjack

*A Logging Library for LFE*

[logo forth-coming ...]


## Introduction

The preferred logging library in Erlang is
[lager](https://github.com/basho/lager). However, it doesn't work
out of the box with LFE, due to the fact that it uses parse transforms (the LFE
compiler uses Core Erlang and does not generate Erlang abstract terms, which
are how Erlang parse transforms work).

As such, we needed a way to easily use lager from LFE. So here you have it: a
lumberjack you do *not* want to cross sticks with ... unless you really know
what you're doing.


## Installation

Just add it to your ``rebar.config`` deps:

```erlang
  {deps, [
    ...
    {lumberjack, ".*",
      {git, "git@github.com:oubiwann/lumberjack.git", "master"}}
      ]}.
```

And then do the usual:

```bash
    $ make compile
```


## Usage

TBD
