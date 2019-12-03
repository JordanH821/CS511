-module(ex5).
-compile(export_all).

timer(Time, Pids) ->
    receive
        {register, PID} ->
            NewPids = Pids ++ [PID]
        after 0 ->
            NewPids = Pids
    end,
    timer:sleep(Time),
    lists:map(fun(Pid) -> Pid!{tick} end, NewPids),
    timer(Time, NewPids).

listener() ->
    receive
        {tick} ->
            io:format("Received 'tick' at ~w~n", [self()]),
            listener()
    end.


start(Time) ->
    % Listeners = [spawn(?MODULE, listener, []) || _ <- lists:seq(1, Num_Listeners)],
    spawn(?MODULE, timer, [Time, []]).