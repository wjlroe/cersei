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
            fetch_job_details(Project, BuildNumber);
        Error ->
            {error, Error}
    end.

auth_header(User, Pass) ->
    Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
    {"Authorization","Basic " ++ Encoded}.

fetch_job_details(Project, BuildNumber) ->
    {ok, Url} = console_url(Project, BuildNumber),
    Headers = [auth_header("jenkins_user", "jenkins_pass")],
    case httpc:request(get, {Url, Headers}, [], []) of
        {ok, {_, _StatusCode, _}, _Headers, Body} ->
            {ok, Body};
        {error, Reason} ->
            {error, Reason}
    end.


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
