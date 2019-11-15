-module(chain).
-compile(export_all).

chain(S, 0) ->
    S!self(),
    receive
        ok ->
            exit(normal)
    end;
chain(S, N) when N>0 ->
     spawn_link(?MODULE, chain, [S, N-1]),
     receive
         ok ->
             exit(normal)
    end.

start() ->
    spawn_link(?MODULE, chain, [self(), 5]).

restarter() ->
    process_flag(trap_exit, true),
    spawn_link(?MODULE, critic, []),
    register(critic, C),
    receive
        {'EXIT', PID, normal} ->
            ok;
        {'EXIT', PID, shutdown} ->
            ok; 
        {'EXIT', PID, Reason} ->
            restarter()
    end.