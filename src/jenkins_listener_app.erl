-module(jenkins_listener_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(inets),
    Dispatch = [{'_', [
                       {'_', build_group_websocket_server, []}
                      ]}],
    cowboy:start_listener(builds_websocket, 100,
                          cowboy_tcp_transport, [{port, 10100}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]
                         ),
    jenkins_listener_sup:start_link().

stop(_State) ->
    ok.
