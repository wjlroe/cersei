-module(jenkins_build_info).
-export([build_received/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% decode json from websocket and fetch build info
%% example json: "{\"number\":37,\"project\":\"woot\",\"result\":\"SUCCESS\"}"
build_received(Json) ->
    case mochijson2:decode(Json) of
        {struct, JsonData} ->
            {struct, BuildNumber} = proplists:get_value(<<"number">>,  JsonData),
            {struct, Project}     = proplists:get_value(<<"project">>, JsonData),
            {struct, _Result}     = proplists:get_value(<<"result">>,  JsonData),
            {ok, Url} = console_url(Project, BuildNumber),
            fetch_job_details(Url);
        Error ->
            {error, Error}
    end.

auth_header(User, Pass) ->
    Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
    {"Authorization","Basic " ++ Encoded}.


% ================================== fetch_job_details =============================

-ifdef(TEST).
fetch_job_exists_test() ->
    application:start(inets),
    %% The fixture for this call should be checked into SCM
    Response = meck_cassettes:use_cassette("on_topic_job_4",
                                           fun() ->
                                                   fetch_job_details("http://localhost:8080/job/on_topic/4/consoleText")
                                           end),
    application:stop(inets),
    ?assertMatch({ok, _}, Response),
    {ok, Body} = Response,
    BinaryBody = list_to_binary(Body),
    Matches = binary:matches(BinaryBody, <<"28 tests">>),
    ?assert(is_list(Matches)),
    ?assertEqual(1, length(Matches)).
-endif.

%% @doc Returns the consoleText of a jenkins build
fetch_job_details(Url) ->
    Headers = [auth_header("jenkins_user", "jenkins_pass")],
    case httpc:request(get, {Url, Headers}, [], []) of
        {ok, {{_, _StatusCode, _}, _Headers, Body}} ->
            {ok, Body};
        {error, Reason} ->
            {error, Reason}
    end.

% ================================== console_url ===================================

-ifdef(TEST).
console_url_no_env_test() ->
    ?assertEqual({error, {config_missing, jenkins_url}}, console_url("woot", "1")).
console_url_bad_url_test() ->
    application:set_env(jenkins_listener, jenkins_url, "ci.somebody.com"),
    ?assertEqual({error, no_scheme}, console_url("woot", "1")).
console_url_correct_format_test() ->
    application:set_env(jenkins_listener, jenkins_url, "https://ci.somebody.com"),
    {ok, Url} = console_url("woot", "1"),
    ?assertMatch(<<"https://ci.somebody.com:443/job/woot/1/consoleText">>,
                 list_to_binary(Url)).
-endif.

%% @doc Returns a URL for downloading the console output of a Jenkins build
console_url(Project, BuildNumber) ->
    case application:get_env(jenkins_listener, jenkins_url) of
        {ok, Url} ->
            case http_uri:parse(Url) of
                {Scheme, _UserInfo, Host, Port, _Path, _Query} ->
                    {ok, io_lib:format("~p://~s:~B/job/~s/~s/consoleText",
                                       [Scheme, Host, Port, Project, BuildNumber])};
                Error ->
                    Error
            end;
        undefined -> 
            {error, {config_missing, jenkins_url}}
    end.
