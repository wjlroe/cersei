-module(build_output_parser).
-export([parse_build_output/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

%% TODO: Configurable, dynamically settable
-define(REGEXES, 
        [
         {"(?<EXAMPLES>[0-9]+) examples, (?<FAILURES>[0-9]+) failures", [tests, bugs]},
         {"([0-9]+) tests,.+, ([0-9]) failures", [tests, bugs]},
         {"FAILURE: ([0-9]+) facts were not confirmed.+But ([0-9]+) were", [bugs, passes]},
         {"All claimed facts \\(([0-9]+)\\) have been confirmed", [tests]}
        ]).

% ============================== parse_build_output ============================

parse_build_output(Output) ->
    case lists:sort(group_counts(Output)) of
        [{bugs, BugCount} | Tests] when BugCount > 0 ->
            {fail, {bugs, BugCount}, test_summary(Tests, BugCount)};
        [{bugs, BugCount} | Tests] ->
            {pass, {bugs, BugCount}, test_summary(Tests, BugCount)};
        [{tests, TestNum}] ->
            {pass, {bugs, 0}, {tests, TestNum}};
        _Error -> 
            {error, no_regex_matched}
    end.

test_summary(Tests, BugCount) ->
    case Tests of
        [{tests, Num}] ->
            {tests, Num};
        [{passes, Num}] ->
            {tests, Num + BugCount}
    end.

% ============================= group_counts ===================================

group_counts(Output) ->
    RegexChecks = lists:zip(?REGEXES, 
                            lists:duplicate(length(?REGEXES), Output)),
    lists:foldl(fun regex_match_to_integers/2,
                [],
                RegexChecks).

% ============================= regex_match_to_integers ========================

regex_match_to_integers({{Regex, Groups}, Output}, Acc) ->
    case re:run(Output, Regex, [{capture, all, list}]) of
        {match, [_|Matches]} ->
            MatchInts = lists:map(fun list_to_integer/1, Matches),
            io:format("MatchInts: ~p~n", [MatchInts]),
            MatchedGroups = lists:zip(Groups, MatchInts) ++ Acc,
            io:format("MatchGroups: ~p~n", [MatchedGroups]),
            %% [{bugs, 3}, {bugs, 0}, {tests, 10}, {tests, 1}]
            lists:map(fun(Key) -> 
                              {Key, 
                               lists:sum(proplists:get_all_values(Key, MatchedGroups))}
                      end,
                      proplists:get_keys(MatchedGroups));
        _ ->
            Acc
    end.
