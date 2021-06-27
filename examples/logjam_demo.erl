-module(logjam_demo).

-export([run/0]).

-include_lib("logjam/include/logjam.hrl").

run() ->
    io:format("~n*** Using logjam's macros from Erlang ***~n~n"),
    ?'log-debug'("This is a debug-level message"),
    ?'log-info'("This is a info-level message"),
    ?'log-notice'("This is a notice-level message"),
    ?'log-warn'("This is a warning-level message"),
    ?'log-err'("This is a error-level message"),
    ?'log-crit'("This is a critical-level message"),
    ?'log-alert'("This is a alert-level message"),
    ?'log-emergency'("This is a emergency-level message"),
    ?'log-info'(#{some => "structured",
                  logging => "examples",
                  might => "be useful too"}).
