-module(models).
-behaviour(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

-export([add_project_to_group/2, remove_project_from_group/2, create_group/2, get_group/1, get_project/1]).

-record(state, {
          database :: atom()
         }).

add_project_to_group(_Project, _Group) ->
    ok.

remove_project_from_group(_Project, _Group) ->
    ok.

create_group(_Group, _Projects) ->
    ok.

get_group(_Group) ->
    ok.

get_project(_Project) ->
    ok.

%% ===================== gen_server callbacks ====================

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
init([Database]) ->
    case Database of
        redis ->
            {ok, _Client} = eredis:start_link();
        ets ->
            create_ets_tables
    % group_name -> [project]
    % project -> [group]
    end,

    State = #state{database = Database},    
    {ok, State}.
