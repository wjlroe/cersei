-module(redis_persistence).
-compile(export_all).

connect() ->
    eredis:start_link().

