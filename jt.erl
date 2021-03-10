-module(jt).
-import(file,[native_name_encoding/0]).
-compile(export_all).

-define(MAXLINE, 100).

%% @doc Reads a file with the given name in the current working directory
%% @param Filename The relative name of the file to read
%% @returns string containing the contents of the file
read(Filename) ->
    {ok, Cwd} = file:get_cwd(),
    AbsoluteFilename = Cwd ++ "/" ++ Filename,
    file:read_file(AbsoluteFilename).

%% @doc parses the string version of the triangle into a list of lists of numbers
parse(S) ->
    ContentStrings = string:split(S, "\r\n", all),
    ContentArray = lists:map(fun(X) -> string:lexemes(X, " ") end, ContentStrings),
    lists:map(fun(X) -> lists:map(fun(Y) -> {I, _} = string:to_integer(Y), I end, X) end, ContentArray).

%% @doc Sets up the process for guarding access to the data structure being parsed
create_triangle() ->
    register(triangle, self()),
    {ok, ContentString} = read("triangle.txt"),
    ets:new(triangle, [set, named_table]),
    Triangle = parse(ContentString),
    triangle_recv(Triangle).

%% @doc Event loop for accessing the triangle. Memoized!
triangle_recv(Triangle) ->
    receive
	{value, From, Ref, Line, Index} ->
	    case ets:lookup(triangle, {Line, Index}) of
		[{{Line, Index}, RetVal}] ->
		    From!{Ref, RetVal};
		[] ->
		    %% This address has never been visited yet, so memoize it
		    Value = lists:nth(Index, lists:nth(Line, Triangle)),
		    From!{Ref, Value}
	    end
    end,
    triangle_recv(Triangle).
    
%% @doc Sets up the process for the subproblem memoizer
create_memo() ->
    register(memo, self()),
    ets:new(lookup, [set, named_table]),
    memo_recv().

%% @doc Event loop for accessing and inserting values into the subproblem memoizer
memo_recv() ->
    receive
	{query, From, Ref, Key} ->
	    case ets:lookup(lookup, Key) of 
		[{Key, RetVal}] -> 
		    From!{response, ok, Ref, RetVal},
		    memo_recv();
		[] ->
		    From!{response, not_found, Ref},
		    memo_recv()
	    end;
	{insertion, Key, StoreVal} ->
	    ets:insert(lookup, {Key, StoreVal}),
	    memo_recv()
    end.
	    


%% @doc The algorithmic unit function; receives a line number and index representing the current node in the tree.
%% If the current node is a leaf, computation ends and the current number is returned.
%% If the current node is not a leaf, computation splits; the value of the current node is added to the return
%% value of the algorithm for left and right nodes respectively.
f(I, Pos) ->
    Ref = make_ref(),
    whereis(triangle)!{value, self(), Ref, I, Pos},
    receive {Ref, At} ->
	    if 
		I == ?MAXLINE ->
		    %% We are a leaf
		    At;
		true ->
		    %% We are not a leaf
		    
		    %% The left branch changes the line number but not the position in the line
		    %% The right branch changes both the line number and line position by 1
		    %% first, check the memo
		    LeftRef = make_ref(),
		    whereis(memo)!{query, self(), LeftRef, {I + 1, Pos}},
		    LeftVal = receive
				  {response, ok, LeftRef, RetVal} ->
				      %% The left child has already been visited
				      RetVal;
				  {response, not_found, LeftRef} ->
				      %% Left child has not been visited yet; compute it
				      LeftSubVal = f(I + 1, Pos),
				      whereis(memo)!{insertion, {I + 1, Pos}, LeftSubVal},
				      LeftSubVal
			      end,
		    
		    %% compute the right child
		    RightRef = make_ref(),
		    whereis(memo)!{query, self(), RightRef, {I + 1, Pos + 1}},
		    RightVal = receive
				   {response, ok, RightRef, RightRetVal} ->
				       %% right child was already memoized
				       RightRetVal;
				   {response, not_found, RightRef} ->
				       %% right child was never visited, so compute it
				       RightSubVal = f(I + 1, Pos + 1),
				       whereis(memo)!{inserction, {I + 1, Pos + 1}, RightSubVal},
				       RightSubVal
			       end,
		    %% finally, return the maximum of the left and right children
		    lists:max([At + LeftVal, At + RightVal])
	    end
    end.

%% @doc The setup function for the main process.
%% Calls `f' and prints out the result
%% Note that if this function is spawned instead of called, the result will be printed async so you'll still have a prompt
start_server() ->
    MaxValue = f(1, 1),
    io:fwrite("The maximum path is ~w~n", [MaxValue]).

%% Entry point
start() ->
    spawn(fun create_triangle/0),
    spawn(fun create_memo/0),
    spawn(fun start_server/0).
