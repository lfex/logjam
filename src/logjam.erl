%%% @doc
%%% This is the main module that exposes custom formatting to the OTP
%%% logger library (part of the `kernel' application since OTP-21).
%%%
%%% The module honors the standard configuration of the kernel's default
%%% logger formatter regarding: max depth, templates.
%%% @end
-module(logjam).

%% API exports
-export([format/2]).

%% Wrapper exports
-export([debug/1]).

-ifdef(TEST).
-export([format_msg/2, to_string/2]).
-endif.

-include_lib("kernel/include/logger.hrl").

-type template() :: [metakey() | {metakey(), template(), template()} | string()].
-type metakey() :: atom() | [atom()].

-define(BLACK, "\e[0;30m").
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
-define(RED, "\e[0;31m").
-define(REDB, "\e[1;31m").
-define(COLOR_END, "\e[0m").
%%====================================================================
%% API functions
%%====================================================================
-spec format(LogEvent, Config) -> unicode:chardata() when
      LogEvent :: logger:log_event(),
      Config :: logger:formatter_config().
format(Map = #{msg := {report, #{label := {error_logger, _}, format := Format, args := Terms}}}, UsrConfig) ->
    format(Map#{msg := {report, #{text => format_to_binary(Format, Terms)}}}, UsrConfig);
format(#{level:=Level, msg:={report, Msg}, meta:=Meta}, UsrConfig) when is_map(Msg) ->
    Config = apply_defaults(UsrConfig),
    NewMeta = maps:merge(Meta, #{level => Level
                                ,colored_start => Level
                                ,colored_end => "\e[0m"
                                }),
    format_log(maps:get(template, Config), Config, Msg, NewMeta);
format(Map = #{msg := {report, KeyVal}}, UsrConfig) when is_list(KeyVal) ->
    %io:format("KeyVal: ~p", [KeyVal]),
    format(Map#{msg := {report, maps:from_list(KeyVal)}}, UsrConfig);
format(Map = #{msg := {string, String}}, UsrConfig) ->
    %io:format("String: ~p", [String]),
    format(Map#{msg := {report, #{text => string_to_binary(String)}}}, UsrConfig);
format(Map = #{msg := {Format, Terms}}, UsrConfig) ->
    %io:format("Format: ~p", [Format]),
    format(Map#{msg := {report, #{text => format_to_binary(Format, Terms)}}}, UsrConfig).

%%====================================================================
%% Internal functions
%%====================================================================
apply_defaults(Map) ->
    maps:merge(
      #{term_depth => undefined,
        map_depth => -1,
        time_offset => 0,
        time_unit => second,
        time_designator => $T,
        strip_tz => false,
        level_capitalize => false,
        level_length => -1,
        colored => false,
        colored_debug =>     ?BLUEB,
        colored_info =>      ?CYAN,
        colored_notice =>    ?GREENB,
        colored_warning =>   ?GOLDB,
        colored_error =>     ?REDB,
        colored_critical =>  ?RED,
        colored_alert =>     ?BLACK_ON_GOLD,
        colored_emergency => ?GOLDB_ON_RED,
        template => [time, " ", colored_start, level, colored_end, " ",
                     {id, [" id=", id], ""}, {parent_id, [" parent_id=", parent_id], ""},
                     {correlation_id, [" correlation_id=", correlation_id], ""},
                     {pid, ["", ?BLUEB, pid, ?COLOR_END], ""}, " [", ?GOLD, mfa, ":", line, ?COLOR_END, "] ", ?CYANB, "▸ ", ?COLOR_END, ?GREENB, msg, ?COLOR_END, "\n"]
       },
      Map
    ).


-spec format_log(template(), Config, Msg, Meta) -> unicode:chardata() when
      Config :: logger:formatter_config(),
      Msg :: Data,
      Meta :: Data,
      Data :: #{string() | binary() | atom() => term()}.
format_log(Tpl, Config, Msg, Meta) -> format_log(Tpl, Config, Msg, Meta, []).

format_log([], _Config, _Msg, _Meta, Acc) ->
    lists:reverse(Acc);
format_log([msg | Rest], Config, Msg, Meta, Acc) ->
    format_log(Rest, Config, Msg, Meta, [format_msg(Msg, Config) | Acc]);
format_log([Key | Rest], Config, Msg, Meta, Acc) when is_atom(Key)
                                                 orelse is_atom(hd(Key)) -> % from OTP
    case maps:find(Key, Meta) of
        error ->
            format_log(Rest, Config, Msg, Meta, Acc);
        {ok, Val} ->
            format_log(Rest, Config, Msg, Meta, [format_val(Key, Val, Config) | Acc])
    end;
format_log([{Key, IfExists, Else} | Rest], Config, Msg, Meta, Acc) ->
    case maps:find(Key, Meta) of
        error ->
            format_log(Rest, Config, Msg, Meta, [Else | Acc]);
        {ok, Val} ->
            format_log(Rest, Config, Msg, Meta,
                       [format_log(IfExists, Config, Msg, #{Key => Val}, []) | Acc])
    end;
format_log([Term | Rest], Config, Msg, Meta, Acc) when is_list(Term) ->
    format_log(Rest, Config, Msg, Meta, [Term | Acc]).

format_msg(Data, Config) -> format_msg("", Data, Config).

format_msg(Parents, Data, Config=#{map_depth := 0}) when is_map(Data) ->
    to_string(truncate_key(Parents), Config)++"=... ";
format_msg(Parents, Data, Config = #{map_depth := Depth}) when is_map(Data) ->
    maps:fold(
      fun(K, V, Acc) when is_map(V) ->
        [format_msg(Parents ++ to_string(K, Config) ++ "_",
                    V,
                    Config#{map_depth := Depth-1}) | Acc]
      ;  (K, V, Acc) ->
        [Parents ++ to_string(K, Config), $=,
         to_string(V, Config), $\s | Acc]
      end,
      [],
      Data
    ).


format_val(time, Time, Config) ->
    format_time(Time, Config);
format_val(mfa, MFA, Config) ->
    escape(format_mfa(MFA, Config));
format_val(level, Level, Config) ->
    format_level(Level, Config);
format_val(colored_end, _EOC, #{colored := false}) -> "";
format_val(colored_end, EOC,  #{colored := true}) -> EOC;
format_val(colored_start, _Level,    #{colored := false}) -> "";
format_val(colored_start, debug,     #{colored := true, colored_debug     := BOC}) -> BOC;
format_val(colored_start, info,      #{colored := true, colored_info      := BOC}) -> BOC;
format_val(colored_start, notice,    #{colored := true, colored_notice    := BOC}) -> BOC;
format_val(colored_start, warning,   #{colored := true, colored_warning   := BOC}) -> BOC;
format_val(colored_start, error,     #{colored := true, colored_error     := BOC}) -> BOC;
format_val(colored_start, critical,  #{colored := true, colored_critical  := BOC}) -> BOC;
format_val(colored_start, alert,     #{colored := true, colored_alert     := BOC}) -> BOC;
format_val(colored_start, emergency, #{colored := true, colored_emergency := BOC}) -> BOC;
format_val(_Key, Val, Config) ->
    to_string(Val, Config).

format_level(Level, Config) when is_atom(Level) ->
    format_level(atom_to_list(Level), Config);
format_level(Level, #{level_capitalize := Is_cap, level_length := Lvl_len}) ->
    L2 = case Is_cap of
        true -> string:to_upper(Level);
        _ -> Level
    end,
    case Lvl_len > 0 of
        true -> lists:sublist(L2, Lvl_len);
        _ -> L2
    end.

format_time(N, #{time_offset := O,
                 time_unit := U,
                 time_designator := D,
                 strip_tz := Strip}) when is_integer(N) ->
    N2 = case U of
             second -> round(N / 1000000);
             millisecond -> round(N / 1000);
             _ -> N
    end,
    Time = calendar:system_time_to_rfc3339(N2, [{unit, U},
                                                {offset, O},
                                                {time_designator, D}]),
    case Strip of
        true -> lists:sublist(Time, 1, length(Time) - 6);
        _ -> Time
    end.

format_mfa({M, F, A}, _) when is_atom(M), is_atom(F), is_integer(A) ->
   [atom_to_list(M), $:, atom_to_list(F), $/, integer_to_list(A)];
format_mfa({M, F, A}, Config) when is_atom(M), is_atom(F), is_list(A) ->
    %% arguments are passed as a literal list ({mod, fun, [a, b, c]})
    format_mfa({M, F, length(A)}, Config);
format_mfa(MFAStr, Config) -> % passing in a pre-formatted string value
    to_string(MFAStr,Config).

to_string(X, _) when is_atom(X) ->
    escape(atom_to_list(X));
to_string(X, _) when is_integer(X) ->
    integer_to_list(X);
to_string(X, _) when is_pid(X) ->
    pid_to_list(X);
to_string(X, _) when is_reference(X) ->
    ref_to_list(X);
to_string(X, C = #{colored := IsColored}) when is_binary(X) ->
    BeginColor = case IsColored of
        true -> ?GREEN;
        _ -> ""
    end,
    EndColor = case IsColored of
        true -> ?COLOR_END;
        _ -> ""
    end,
    String = case unicode:characters_to_list(X) of
        {_, _, _} -> % error or incomplete
            escape(format_str(C, X));
        List ->
            case io_lib:printable_list(List) of
                true -> escape(List);
                _ -> escape(format_str(C, X))
            end
    end,
    BeginColor ++ String ++ EndColor;
to_string(X, C) when is_list(X) ->
    case io_lib:printable_list(X) of
        true -> escape(X);
        _ -> escape(format_str(C, X))
    end;
to_string(X, C) ->
    escape(format_str(C, X)).

format_str(#{term_depth := undefined}, T) ->
    io_lib:format("~0tp", [T]);
format_str(#{term_depth := D}, T) ->
    io_lib:format("~0tP", [T, D]).

escape(Str) ->
    case needs_escape(Str) of
        false ->
            case needs_quoting(Str) of
                true -> [$", Str, $"];
                false -> Str
            end;
        true ->
            [$", do_escape(Str), $"]
    end.

needs_quoting(Str) ->
    string:find(Str, " ") =/= nomatch orelse
    string:find(Str, "=") =/= nomatch.

needs_escape(Str) ->
    string:find(Str, "\"") =/= nomatch orelse
    string:find(Str, "\\") =/= nomatch orelse
    string:find(Str, "\n") =/= nomatch.

do_escape([]) ->
    [];
do_escape(Str) ->
    case string:next_grapheme(Str) of
        [$\n | Rest] -> [$\\, $\n | do_escape(Rest)];
        ["\r\n" | Rest] -> [$\\, $\r, $\\, $\n | do_escape(Rest)];
        [$" | Rest] -> [$\\, $" | do_escape(Rest)];
        [$\\ | Rest] -> [$\\, $\\ | do_escape(Rest)];
        [Grapheme | Rest] -> [Grapheme | do_escape(Rest)]
    end.

truncate_key([]) -> [];
truncate_key("_") -> "";
truncate_key([H|T]) -> [H | truncate_key(T)].

debug(Msg) ->
    ?LOG_DEBUG(Msg).

string_to_binary(String) ->
    %% Remove any ANSI colors
    T1 = re:replace(String, "\e\[[0-9;]*m", ""),
    unicode:characters_to_binary(T1).

format_to_binary(Format, Terms) ->
    String = io_lib:format(Format, Terms),
    string_to_binary(String).
