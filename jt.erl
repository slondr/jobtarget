-module(jt).
-import(file,[native_name_encoding/0]).
-export([start/0]).

read(Filename) ->
    {ok, Cwd} = file:get_cwd(),
    AbsoluteFilename = Cwd ++ "/" ++ Filename,
    file:read_file(AbsoluteFilename).

start() ->
    io:fwrite("Read mode is ~w~n", [native_name_encoding()]),
    {ok, ContentString} = read("triangle.txt"),
    ContentStrings = string:split(ContentString, "\r\n", all),
    ContentArray = lists:map(fun(X) -> string:lexemes(X, " ") end, ContentStrings),
    NumericArray = lists:map(fun(X) -> lists:map(fun(Y) -> {I, _} = string:to_integer(Y), I end, X) end, ContentArray),
    %% NumericArray now contains the triangle as a list of lists where each member list is 1 line of the triangle
    NumericArray.
