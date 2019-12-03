-module(ex3).
-compile(export_all).

server(Count) ->
    receive
        {counter} ->
            io:format("Received 'continue' ~w times~n", [Count]),
            server(0);
        {continue} ->
            server(Count+1)
    end.
