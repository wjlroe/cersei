-module(meck_cassettes).
-behaviour(gen_server).
-export([use_cassette/2,mock_request/4]).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,terminate/2,init/1]).

use_cassette(_Name, _Fun) ->
    meck:new(httpc, [unstick]),
    %% If cassette doesn't exist:
    %%  record request parameters and return from httpc
    %%  save to cassette file
    %%  return passthrough from httpc
    %% otherwise cassette exists:
    %%  read file, check parameters to file
    %%  return the response from the file
    %%  anything doesn't match? error
    meck:expect(httpc, request).

mock_request(Method, Request, HTTPOptions, Options) ->
    Args = [Method, Request],
    Fixture = "test/fixtures/cassettes/mock_request",
    case file:read_file_info(Fixture) of
        {error, _} ->
            Response = httpc:request(Method, Request, HTTPOptions, Options),
            RequestCall = [{request, Args},
                           {response, Response}],
            ok = file:write_file(Fixture, term_to_binary(RequestCall)),
            Response;
         _ ->
            case file:read_file(Fixture) of
                {ok, ResponseBinary} ->
                    ResponseCall = binary_to_term(ResponseBinary),
                    proplists:get_value(response, ResponseCall);
                {error, Something} ->
                    {error, Something}
            end
    end.
            

%% @hidden
code_change(_OldVsn, S, _Extra) -> {ok, S}.

%% @hidden
handle_call(_Something, _from, S) ->
    {noreply, S}.

%% @hidden
handle_cast(_Msg, S)  ->
    {noreply, S}.

%% @hidden
handle_info(_Info, S) -> {noreply, S}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
init([]) ->
    process_flag(trap_exit, true),
    {ok, []}.
