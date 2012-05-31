-module(jenkins_websocket_client).
-behaviour(websocket_client).
-export([start/0]).
%% websocket specific callbacks
-export([ws_onmessage/1,ws_onopen/0,ws_onclose/0,ws_close/0,ws_send/1]).

ws_send(Data) ->
    websocket_client:write(Data).

start(Endpoint) ->
    case http_uri:parse(Endpoint) of
        {ok, {Scheme, UserInfo, Host, Port, Path, Query}} ->
            websocket_client:start(Host,Port,Path,?MODULE);
        {error, Reason} ->
            io:format("Error parsing endpoint: ~s: ~p~n", [Endpoint, Reason])
    end.

%% Handle incoming messages here
ws_onmessage(Data) ->
    io:format("Got some data:: ~p~n",[Data]).

ws_onclose() ->
    io:format("Connection closed~n").

ws_onopen() ->
    io:format("Connection open~n"),
    ws_send("client-connected").

ws_close() ->
    websocket_client:close().
