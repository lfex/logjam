%%%-----------------------------------------------------------------
%%% Convenience definitions
%%% 
-define(BLACK, "\e[0;30m").
-define(BLACKB, "\e[1;30m").
-define(BLACK_ON_GOLD, "\e[30;43m").
-define(BLUE, "\e[0;34m").
-define(BLUEB, "\e[1;34m").
-define(CYAN, "\e[0;36m").
-define(CYANB, "\e[1;36m").
-define(GOLD, "\e[0;33m").
-define(GOLDB, "\e[1;33m").
-define(GOLDB_ON_RED, "\e[1;33;41m").
-define(GREEN, "\e[0;32m").
-define(GREENB, "\e[1;32m").
-define(GREY, "\e[0;37m").
-define(GREYB, "\e[1;37m").
-define(MAGENTA, "\e[0;35m").
-define(MAGENTAB, "\e[1;35m").
-define(RED, "\e[0;31m").
-define(REDB, "\e[1;31m").
-define(COLOR_END, "\e[0m").

%%%-----------------------------------------------------------------
%%% Copied from kernel/include/logger.hrl and adapted for LFE
%%% 
-ifndef(LOGJAM_HRL).
-define(LOGJAM_HRL,true).
-define('log-emergency'(A),?DO_LOG(emergency,[A])).
-define('log-emergency'(A,B),?DO_LOG(emergency,[A,B])).
-define('log-emergency'(A,B,C),?DO_LOG(emergency,[A,B,C])).

-define('log-alert'(A),?DO_LOG(alert,[A])).
-define('log-alert'(A,B),?DO_LOG(alert,[A,B])).
-define('log-alert'(A,B,C),?DO_LOG(alert,[A,B,C])).

-define('log-critical'(A),?DO_LOG(critical,[A])).
-define('log-critical'(A,B),?DO_LOG(critical,[A,B])).
-define('log-critical'(A,B,C),?DO_LOG(critical,[A,B,C])).

-define('log-crit'(A),?DO_LOG(critical,[A])).
-define('log-crit'(A,B),?DO_LOG(critical,[A,B])).
-define('log-cril'(A,B,C),?DO_LOG(critical,[A,B,C])).

-define('log-error'(A),?DO_LOG(error,[A])).
-define('log-error'(A,B),?DO_LOG(error,[A,B])).
-define('log-error'(A,B,C),?DO_LOG(error,[A,B,C])).

-define('log-err'(A),?DO_LOG(error,[A])).
-define('log-err'(A,B),?DO_LOG(error,[A,B])).
-define('log-err'(A,B,C),?DO_LOG(error,[A,B,C])).

-define('log-warning'(A),?DO_LOG(warning,[A])).
-define('log-warning'(A,B),?DO_LOG(warning,[A,B])).
-define('log-warning'(A,B,C),?DO_LOG(warning,[A,B,C])).

-define('log-warn'(A),?DO_LOG(warning,[A])).
-define('log-warn'(A,B),?DO_LOG(warning,[A,B])).
-define('log-warn'(A,B,C),?DO_LOG(warning,[A,B,C])).

-define('log-notice'(A),?DO_LOG(notice,[A])).
-define('log-notice'(A,B),?DO_LOG(notice,[A,B])).
-define('log-notice'(A,B,C),?DO_LOG(notice,[A,B,C])).

-define('log-info'(A),?DO_LOG(info,[A])).
-define('log-info'(A,B),?DO_LOG(info,[A,B])).
-define('log-info'(A,B,C),?DO_LOG(info,[A,B,C])).

-define('log-debug'(A),?DO_LOG(debug,[A])).
-define('log-debug'(A,B),?DO_LOG(debug,[A,B])).
-define('log-debug'(A,B,C),?DO_LOG(debug,[A,B,C])).

-define('log'(L,A),?DO_LOG(L,[A])).
-define('log'(L,A,B),?DO_LOG(L,[A,B])).
-define('log'(L,A,B,C),?DO_LOG(L,[A,B,C])).

-define(LOCATION,#{mfa=>{?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY},
                   line=>?LINE,
                   file=>?FILE}).

%%%-----------------------------------------------------------------
%%% Internal, i.e. not intended for direct use in code - use above
%%% macros instead!
-define(DO_LOG(Level,Args),
        case logger:allow(Level,?MODULE) of
            true ->
                apply(logger,macro_log,[?LOCATION,Level|Args]);
            false ->
                ok
        end).
-endif.

%% This function is for display purpses when used in the LFE REPL
%% and needs to be the last function in the include file.
-define('|-- loaded include: logjam --|'(),
  ok).
