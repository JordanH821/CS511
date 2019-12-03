-module(ex9).
-compile(export_all).

sensor(Temp, Neighs) ->
    receive
        {direct, Value} ->
            io:format("Direct reading ~w at sensor ~w~n", [Value, self()]),
            sensor(Value, Neighs);
        {update} ->
            io:format("Updating sensor ~w~n", [self()]),
            lists:map(fun(P) -> P!{update, Temp} end, Neighs),
            sensor(update(Neighs, 0, 0), Neighs);
        {register, Pid} ->
            sensor(Temp, Neighs ++ [Pid])
    end.

timer(Time, Pids) ->
    timer:sleep(Time),
    lists:map(fun(P) -> P!{update} end, Pids),
    timer(Time, Pids).

update(Neighs, Num_Read, Value) ->
    if
        length(Neighs) == Num_Read ->
            Ret = Value / Num_Read,
            io:format("Updating to value ~w~n", [Ret]),
            Ret;
        true ->
            receive
                {update, Temp} ->
                    update(Neighs, Num_Read + 1, Value + Temp)
            end
    end.

start() ->
    S1 = spawn(?MODULE, sensor, [rand:uniform(1000), []]),
    S2 = spawn(?MODULE, sensor, [rand:uniform(1000), [S1]]),
    S1!{register, S2},
    spawn(?MODULE, timer, [rand:uniform(1000), [S1, S2]]).
    
