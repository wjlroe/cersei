-module(build_filter_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

groups_include_project_name_test() ->
    ?assertEqual({ok, ["ProjectName"]}, build_filter:groups_for_project("ProjectName")).
