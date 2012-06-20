-module(jenkins_build_info_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(setup(F), {setup, fun start_tests/0, fun stop_tests/1, F}).

start_tests() ->
    application:start(inets).

stop_tests(_) ->
    application:stop(inets).

fetch_job_exists(_) ->
    %% The fixture for this call should be checked into SCM
    Response = meck_cassettes:use_cassette(
                 "on_topic_job_4",
                 fun() ->
                         jenkins_build_info:fetch_job_details("http://localhost:8080/job/on_topic/4/consoleText")
                 end),
    ?assertMatch({ok, _}, Response),
    {ok, Body} = Response,
    BinaryBody = list_to_binary(Body),
    Matches = binary:matches(BinaryBody, <<"28 tests">>),
    [?_assert(is_list(Matches)),
     ?_assertEqual(1, length(Matches))].

bad_url_fetch_job(_) ->
    ?_assertEqual({error, no_scheme}, jenkins_build_info:fetch_job_details("sdhfkshdfkhsfd")).

fetch_job_details_test_() ->
    [{"A build output can be fetched from Jenkins",
      ?setup(fun fetch_job_exists/1)},
     {"An error is returned when the url is junk",
      ?setup(fun bad_url_fetch_job/1)}].


console_url_no_env_test() ->
    ?assertEqual({error, {config_missing, jenkins_url}},
                 jenkins_build_info:console_url("woot", "1")).
console_url_bad_url_test() ->
    application:set_env(jenkins_listener, jenkins_url, "ci.somebody.com"),
    ?assertEqual({error, no_scheme},
                 jenkins_build_info:console_url("woot", "1")).
console_url_correct_format_test() ->
    application:set_env(jenkins_listener, jenkins_url, "https://ci.somebody.com"),
    {ok, Url} = jenkins_build_info:console_url("woot", "1"),
    ?assertMatch(<<"https://ci.somebody.com:443/job/woot/1/consoleText">>,
                 list_to_binary(Url)).
