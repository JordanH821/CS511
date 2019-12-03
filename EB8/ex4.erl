-module(ex4).
-compile(export_all).

timer(Time, Pids) ->
    timer:sleep(Time),
    lists:map(fun(Pid) -> Pid!{tick} end, Pids),
    timer(Time, Pids).

listener() ->
    receive
        {tick} ->
            io:format("Received 'tick' at ~w~n", [self()]),
            listener()
    end.


start(Time, Num_Listeners) ->
    Listeners = [spawn(?MODULE, listener, []) || _ <- lists:seq(1, Num_Listeners)],
    spawn(?MODULE, timer, [Time, Listeners]).