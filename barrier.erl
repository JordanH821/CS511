-module(barrier).
-compile(export_all).

barrier(0, N, L) ->
    lists:map(fun(P) -> P!{self(), ok} end, L),
    barrier(N, N, []);
barrier(M,N,L) when M>0 ->
    receive
        {PID, reached} ->
            barrier(M - 1, N, L ++ [PID])
    end.



pass_barrier(B) ->
    B!{self(), reached},
    receive {B, ok} ->
        ok
    end.

client(B, Letter, Number) ->
    io:format("~p ~s ~n", [self(), Letter]),
    pass_barrier(B),
    io:format("~p ~w ~n", [self(), Number]).

start(N) ->
    %% M is the number of pids at barrier
    %% N is the capacity of the barrier
    %% L is the pids to send to
    B = spawn(?MODULE, barrier, [N, N, []]),
    spawn(?MODULE, client, [B, "a", 1]),
    spawn(?MODULE, client, [B, "b", 2]),
    spawn(?MODULE, client, [B, "c", 3]).
