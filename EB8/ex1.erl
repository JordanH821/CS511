-module(ex1).
-compile(export_all).

turnstile(0, Counter) ->
    Counter!{done, self()};
turnstile(N, Counter) ->
    timer:sleep(rand:uniform(100)),
    Counter!{plus},
    turnstile(N - 1, Counter).


counter(N) ->
    receive
        {plus} ->
            counter(N+1);
        {done, PID} ->
            io:format("Turnstile ~w finished, count at ~w~n", [PID, N]),
            counter(N)
    end.

start(Num_T, Turn_Count) ->
    Counter = spawn(?MODULE, counter, [0]),
    [spawn(?MODULE, turnstile, [Turn_Count, Counter]) || _ <- lists:seq(1, Num_T)].
