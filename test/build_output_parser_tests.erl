-module(build_output_parser_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

parse_simple_rspec_fail_test() ->
    ?assertEqual(
       {fail, [{bugs, 32}, {tests, 59}]}, 
       build_output_parser:parse_build_output("59 examples, 32 failures, 3 pending")).
parse_simple_rspec_pass_test() ->
    ?assertEqual(
       {pass, [{bugs, 0}, {tests, 7}]},
       build_output_parser:parse_build_output("7 tests, 10 assertions, 0 failures, 0 errors, 0 skips")).
parse_general_crap_test() ->
    ?assertEqual(
        {error, no_regex_matched},
        build_output_parser:parse_build_output("general crap")).
parse_clojure_midje_passing_test() ->
    ?assertEqual(
       {pass, [{bugs, 0}, {tests, 11}]},
       build_output_parser:parse_build_output("All claimed facts (11) have been confirmed.")).
parse_clojure_midje_failing_test() ->
    ?assertEqual(
       {fail, [{bugs, 3}, {tests, 28}]},
       build_output_parser:parse_build_output("FAILURE: 3 facts were not confirmed. (But 25 were.)")).
parse_eunit_passing_test() ->
    ?assertEqual(
       {pass, [{bugs, 0}, {tests, 18}]},
       build_output_parser:parse_build_output("All 18 tests passed.")).
parse_eunit_failed_test() ->
    ?assertEqual(
       {fail, [{bugs, 1}, {tests, 19}]},
       build_output_parser:parse_build_output("Failed: 1.  Skipped: 0.  Passed: 18.")).

group_count_simple_test() ->
    ?assertEqual(
       [{bugs, 32}, {tests, 59}],
       lists:sort(build_output_parser:group_counts("59 examples, 32 failures, 3 pending"))).

group_counts_clojure_passing_test() ->
    ?assertEqual(
       [{tests, 11}],
       lists:sort(build_output_parser:group_counts("All claimed facts (11) have been confirmed."))).
no_matches_group_count_test() ->
    ?assertEqual(
        [],
        build_output_parser:group_counts("herpa derpa la de da")).

regex_match_simple_bug_test() ->
    ?assertEqual(
       [{bugs, 3}, {tests, 10}],
       lists:sort(
         build_output_parser:regex_match_to_integers(
           {{"([0-9]+) tests and ([0-9]+) failures", [tests, bugs]},
            "10 tests and 3 failures"}, 
           []))).
regex_match_add_to_existing_test() ->
    ?assertEqual(
       [{bugs, 4}, {tests, 8}],
       lists:sort(
         build_output_parser:regex_match_to_integers(
           {{"([0-9]+) tests and ([0-9]+) failures", [tests, bugs]}, 
            "5 tests and 1 failures"}, 
           [{tests, 3}, {bugs, 3}]))).
regex_match_nothing_test() ->
    ?assertEqual(
        [],
        build_output_parser:regex_match_to_integers(
            {{"([0-9]+) tests and ([0-9]+) failures", [tests, bugs]}, 
            "blah blah nothing nothing"},
            [])).

