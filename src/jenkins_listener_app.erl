-module(jenkins_listener_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(inets),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(cowboy),
    application:start(gproc),
    application:start(jenkins_listener).

start(_StartType, _StartArgs) ->
    application:start(inets),
    Dispatch = [{'_', [
                       {[<<"websocket">>], build_group_websocket_handler, []},
                       {'_', build_group_http_handler, []}
                      ]}],
    cowboy:start_listener(builds_websocket, 100,
                          cowboy_tcp_transport, [{port, 10100}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]
                         ),
    jenkins_listener_sup:start_link().

stop(_State) ->
    ok.
