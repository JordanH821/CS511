-module(ex2).
-compile(export_all).

server() ->
    receive
        {start, PID} ->
            S = spawn(?MODULE, servlet, [""]),
            PID!{S},
            server()
    end.

servlet(Str) ->
    receive
        {add, S} ->
            servlet(Str ++ S);
        {done, PID} ->
            PID!{Str}
    end.


