-module(ex7).
-compile(export_all).

start(W, M) ->
    S = spawn(?MODULE, server, [0, 0, false]),
    [spawn(?MODULE, woman, [S]) || _ <- lists:seq(1, W)],
    [spawn(?MODULE, man, [S]) || _ <- lists:seq(1, M)],
    spawn(?MODULE, itGotLate, [3000, S]).

itGotLate(Time, S) ->
    timer:sleep(Time),
    S!{itGotLate}.

woman(S) ->
    S!{woman}.

man(S) ->
    S!{man, self()},
    receive
        {enter} ->
            io:format("Man: ~w entering~n", [self()])
    end.

server(Women, Men, false) ->
    receive
        {man, Pid} when Women >= 2 ->
            Pid!{enter},
            server(Women - 2, Men + 1, false);
        {woman} ->
            io:fwrite("Woman entering~n"),
            server(Women + 1, Men, false);
        {itGotLate} ->
            io:fwrite("ItGotLate~n"),
            server(Women, Men, true)
    end;
server(Women, Men, true) ->
    receive
        {man, Pid} ->
            Pid!{enter},
            server(Women - 2, Men + 1, true);
        {woman} ->
            server(Women + 1, Men, true)
    end.