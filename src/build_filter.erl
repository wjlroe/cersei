-module(build_filter).
-export([groups_for_project/1]).

%% Needs to map project -> group in a many<->many relationship

groups_for_project(Project) ->
    {ok, [Project]}.
