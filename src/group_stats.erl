-module(group_stats).
-export([update_stats/5]).
-export([send_random_message/0]).

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

send_random_message() ->
    RandBuildNum = random:uniform(120),
    RandBugNum   = random:uniform(10) - 1,
    RandTestNum  = random:uniform(50),
    Result = io_lib:format("There were ~B bugs with a total of ~B tests.", [RandBugNum, RandTestNum]),
    BuildOutcome = case RandBugNum of
                       0 ->
                           pass;
                       _ ->
                           fail
                   end,
    BuildResults = {BuildOutcome, [{bugs, RandBugNum}, {tests, RandTestNum}]},
    update_stats("blah", "blah-frontend", RandBuildNum, list_to_binary(Result), BuildResults).
