-module(logjam_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [term_depth, map_depth, unstructured, colored, demo].

term_depth() ->
    [{docs, "Once a term is too deep, it gets continued with `...'"}].
term_depth(_) ->
    ?assertEqual(
       "\"[\\\"01234567890123456789\\\",abc,[d,e|...]]\"",
        lists:flatten(logjam_formatter:to_string(
          ["01234567890123456789",abc,[d,e,[f,g,h]]]
          , #{term_depth => 6}
        ))
    ),
    ok.

map_depth() ->
    [{docs, "A max number of nesting in maps can be provided"}].
map_depth(_) ->
    %% Use custom templates to drop metadata/templates
    Template = [msg],
    Map = #{a => #{b => #{c => #{d => x}},
                   f => g},
            1 => #{2 => #{3 => x}}},
    ?assertEqual(
        "a_f=g a_b_c=... 1_2_3=x ",
        lists:flatten(
          logjam:format(#{level => info, msg => {report, Map}, meta => #{}},
                         #{template => Template,
                           map_depth => 3})
        )
    ),
    ?assertEqual(
        "a=... 1=... ",
        lists:flatten(
          logjam:format(#{level => info, msg => {report, Map}, meta => #{}},
                         #{template => Template,
                           map_depth => 1})
        )
    ),

    ok.

unstructured() ->
    [{docs, "logs that aren't structured get passed through with a re-frame"}].
unstructured(_) ->
    ?assertEqual(
       "text=abc ",
       lists:flatten(
         logjam:format(#{level => info, msg => {string, "abc"}, meta => #{}},
                        #{template => [msg]})
       )
    ),
    ?assertEqual(
       "text=abc ",
       lists:flatten(
         logjam:format(#{level => info, msg => {string, [<<"abc">>]}, meta => #{}},
                        #{template => [msg]})
       )
    ),
    ?assertEqual(
       "text=\"hello world\" ",
       lists:flatten(
         logjam:format(#{level => info, msg => {"hello ~s", ["world"]}, meta => #{}},
                        #{template => [msg]})
       )
    ),
    ok.

colored() ->
    [{docs, "colored output logs"}].
colored(_) ->
    ?assertEqual(
       " \e[0;36minfo\e[0m  [\e[0;33m:\e[0m] \e[1;36m▸ \e[0m\e[1;32mhi=there \e[0m\n",
        lists:flatten(
          logjam:format(#{level => info, msg => {report, #{hi => there}}, meta => #{}},
                         #{colored => true})
        )
    ),
    ?assertEqual(
       " \e[30;43malert\e[0m  [\e[0;33m:\e[0m] \e[1;36m▸ \e[0m\e[1;32mtext=\e[0;32mabc\e[0m \e[0m\n",
       lists:flatten(
         logjam:format(#{level => alert, msg => {string, "abc"}, meta => #{}},
                        #{colored => true})
       )
    ),
    ok.

demo() ->
    [{docs, "just going for text coverage ..."}].
demo(_) ->
    logjam_demo:run(),
    'logjam-demo':run().
