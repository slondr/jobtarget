-module(jt).
-import(file,[native_name_encoding/0]).
-export([start/0]).

read(Filename) ->
    file:read_file(Filename).

start() ->
    io:fwrite("Read mode is ~w~n",[ native_name_encoding()]).
