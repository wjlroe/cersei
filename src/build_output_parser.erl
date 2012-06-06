-module(build_output_parser).
-export([parse_build_output/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(REGEXES, 
        [
         {"(?<EXAMPLES>[0-9]+) examples, (?<FAILURES>[0-9]+) failures", [tests, bugs]},
         {"([0-9]+) tests,.+, ([0-9]) failures", [tests, bugs]}
        ]).

% ============================== parse_build_output ============================

-ifdef(TEST).
parse_simple_rspec_fail_test() ->
    ?assertEqual(
       {fail, {bugs, 32}, {tests, 59}}, 
       parse_build_output("59 examples, 32 failures, 3 pending")).
parse_simple_rspec_pass_test() ->
    ?assertEqual(
       {pass, {bugs, 0}, {tests, 7}},
       parse_build_output("7 tests, 10 assertions, 0 failures, 0 errors, 0 skips")).
-endif.

parse_build_output(Output) ->
    case lists:sort(group_counts(Output)) of
        [{bugs, BugCount}, {tests, TestCount}] when BugCount > 0 ->
            {fail, {bugs, BugCount}, {tests, TestCount}};
        [{bugs, BugCount}, {tests, TestCount}] ->
            {pass, {bugs, BugCount}, {tests, TestCount}};
        Error -> {error, Error}
    end.

% ============================= group_counts ===================================

-ifdef(TEST).
group_count_simple_test() ->
    ?assertEqual(
       [{bugs, 32}, {tests, 59}],
       lists:sort(group_counts("59 examples, 32 failures, 3 pending"))).
-endif.

group_counts(Output) ->
    RegexChecks = lists:zip(?REGEXES, 
                            lists:duplicate(length(?REGEXES), Output)),
    lists:foldl(fun regex_match_to_integers/2,
                [],
                RegexChecks).

% ============================= regex_match_to_integers ========================

-ifdef(TEST).
regex_match_simple_bug_test() ->
    ?assertEqual(
       [{bugs, 3}, {tests, 10}],
       lists:sort(
         regex_match_to_integers(
           {{"([0-9]+) tests and ([0-9]+) failures", [tests, bugs]},
            "10 tests and 3 failures"}, 
           []))),
    ?assertEqual(
       [{bugs, 4}, {tests, 8}],
       lists:sort(
         regex_match_to_integers(
           {{"([0-9]+) tests and ([0-9]+) failures", [tests, bugs]}, 
            "5 tests and 1 failures"}, 
           [{tests, 3}, {bugs, 3}]))).
-endif.

regex_match_to_integers({{Regex, Groups}, Output}, Acc) ->
    case re:run(Output, Regex, [{capture, all, list}]) of
        {match, [_|Matches]} ->
            MatchInts = lists:map(fun list_to_integer/1, Matches),
            io:format("MatchInts: ~p~n", [MatchInts]),
            MatchedGroups = lists:zip(Groups, MatchInts) ++ Acc,
            io:format("MAtchGroups: ~p~n", [MatchedGroups]),
            %% [{bugs, 3}, {bugs, 0}, {tests, 10}, {tests, 1}]
            lists:map(fun(Key) -> 
                              {Key, 
                               lists:sum(proplists:get_all_values(Key, MatchedGroups))}
                      end,
                      proplists:get_keys(MatchedGroups));
        _ ->
            Acc
    end.
