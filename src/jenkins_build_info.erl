-module(jenkins_build_info).
-export([build_received/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

%% decode json from websocket and fetch build info
%% example json: "{\"number\":37,\"project\":\"woot\",\"result\":\"SUCCESS\"}"
build_received(Json) ->
    case mochijson2:decode(Json) of
        {struct, JsonData} ->
            BuildNumber = proplists:get_value(<<"number">>,  JsonData),
            Project     = binary_to_list(proplists:get_value(<<"project">>, JsonData)),
            Result      = proplists:get_value(<<"result">>,  JsonData),
            {ok, Url}   = console_url(Project, BuildNumber),
            case fetch_job_details(Url) of
                {ok, ConsoleText} ->
                    case build_output_parser:parse_build_output(ConsoleText) of
                        {error, Error} ->
                            io:format("Error parsing build output: ~p~n", [Error]);
                        BuildOutcome ->
                            notify_groups(Project, BuildNumber, Result, BuildOutcome)
                    end;
                {error, Error} ->
                    io:format("Error fetching job details for Url: ~p Error: ~p~n", 
                              [Url, Error])
            end;
        Error ->
            {error, Error}
    end.

notify_groups(Project, BuildNumber, Result, BuildOutcome) ->
    case build_filter:groups_for_project(Project) of
        {ok, Groups} ->
            lists:foreach(
              fun(Group) ->
                      group_stats:update_stats(Group, 
                                               Project, 
                                               BuildNumber, 
                                               Result, 
                                               BuildOutcome)
              end,
              Groups);
        {error, Error} ->
            io:format("Error getting groups for Project: ~p. Error: ~p~n", 
                      [Project, Error])
    end.

auth_header(User, Pass) ->
    Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
    {"Authorization","Basic " ++ Encoded}.


% ================================== fetch_job_details =============================

%% @doc Returns the consoleText of a jenkins build
fetch_job_details(Url) ->
    io:format("Url: ~p~n", [Url]),
    case application:get_env(jenkins_listener, jenkins_user) of
        {ok, Username} ->
            case application:get_env(jenkins_listener, jenkins_pass) of
                {ok, Password} ->
                    Headers = [auth_header(Username, Password)],
                    case httpc:request(get, {Url, Headers}, [], []) of
                        {ok, {{_, StatusCode, _}, _Headers, Body}} ->
                            case StatusCode of
                                200 ->
                                    {ok, Body};
                                _ ->
                                    {error, StatusCode}
                            end;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                undefined ->
                    {error, {config_missing, jenkins_pass}}
            end;
        undefined ->
            {error, {config_missing, jenkins_user}}
    end.

% ================================== console_url ===================================

%% @doc Returns a URL for downloading the console output of a Jenkins build
console_url(Project, BuildNumber) ->
    case application:get_env(jenkins_listener, jenkins_url) of
        {ok, Url} ->
            case http_uri:parse(Url) of
                {Scheme, _UserInfo, Host, Port, _Path, _Query} ->
                    {ok, lists:flatten(io_lib:format("~s://~s:~B/job/~s/~B/consoleText",
                                                     [Scheme, Host, Port, Project, BuildNumber]))};
                Error ->
                    Error
            end;
        undefined -> 
            {error, {config_missing, jenkins_url}}
    end.
