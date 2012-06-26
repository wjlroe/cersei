-module(build_group_http_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    Path = case cowboy_http_req:raw_path(Req) of
               {BinaryPath, _} -> binary_to_list(BinaryPath);
               _ -> error
           end,
    Filename = case Path of
                   "/" ++ NotAbsolute -> NotAbsolute;
                   _ -> Path
               end,
    RewrittenFilename = rewrite_path(Filename),
    io:format("Filename: ~p~n", [RewrittenFilename]),
    FileLocation = filename:join(["./public", RewrittenFilename]),
    io:format("FileLocation: ~p~n", [FileLocation]),
    ContentTypeHeader = content_type(RewrittenFilename),
    {ok, Req2} = case filelib:is_regular(FileLocation) of
                     true ->
                         {ok, Body} = file:read_file(FileLocation),
                         cowboy_http_req:reply(200, [ContentTypeHeader], Body, Req);
                     false ->
                         cowboy_http_req:reply(404, [], <<"No such file">>, Req)
                 end,
    {ok, Req2, State}.

rewrite_path(Filename) ->
    case Filename of
        "" ->
            "index.html";
        _ ->
            Filename
    end.

terminate(_Req, _State) ->
    ok.

content_type(Filename) ->
    ContentType = mime_type(Filename),
    {'Content-Type', ContentType}.

mime_type(FileName) ->
    "." ++ Extension = filename:extension(FileName),
    MimeTypes = mime_types(),
    proplists:get_value(Extension, MimeTypes).

mime_types() ->
    MimeTypesFile = filename:join(code:lib_dir(inets),
                                  "examples/server_root/conf/mime.types"),
    {ok, MimeTypes} = httpd_conf:load_mime_types(MimeTypesFile),
    MimeTypes.
