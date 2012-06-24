-module(group_stats).
-export([update_stats/5]).

update_stats(Group, Project, BuildNumber, Result, {BuildOutcome, BuildResults}) ->
    Msg = mochijson2:encode({struct, [
                                      {group, list_to_binary(Group)},
                                      {project, list_to_binary(Project)},
                                      {build_number, BuildNumber},
                                      {result, Result},
                                      {build_outcome, BuildOutcome},
                                      {build_results,
                                       {struct, BuildResults}}
                                     ]}),
    build_group_websocket_handler:send_message(Msg).
