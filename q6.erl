-module(q6).
-compile(export_all).

dryCleaner(Clean,Dirty) -> %% Clean, Dirty are counters todo.
    receive
        {Sender, Ref, pickUpOverall} when Clean > 0 ->
            Sender!{self(), Ref, overall},
            dryCleaner(Clean-1, Dirty);
        {Sender, Ref, pickUpOverall} ->
            Sender!{self(), Ref, wait},
            dryCleaner(Clean, Dirty);
        {_Sender, _Ref, dropOffOverall} ->
            dryCleaner(Clean, Dirty+1);
        {Sender, Ref, dryCleanItem} when Dirty > 0 ->
            Sender!{self(), Ref, wait},
            dryCleaner(Clean, Dirty);
        {Sender, Ref, dryCleanItem} ->
            Sender!{self(), Ref, wash},
            dryCleaner(Clean+1, Dirty-1)
    end.

employee(DC) -> % drop off overall, then pick up a clean one (if none % is available , wait), and end
    Ref = make_ref(),
    DC!{dropOffOverall},
    waitLoop(DC, Ref).

waitLoop(DC, Ref) ->
    DC!{self(), Ref, pickUpOverall},
    receive
        {DC, Ref, wait} ->
            waitLoop(DC, Ref);
        {DC, Ref, overall} ->
            io:format("Picked up overall")
    end.

dryCleanMachine(DC) -> % dry clean item (if none are available, wait),
    Ref = make_ref(),
    DC!{self(), Ref, dryCleanItem},
    receive
        {DC, Ref, wait} ->
            dryCleanMachine(DC);
        {DC, Ref, wash} ->
            timer:sleep(1000),
            dryCleanMachine(DC)
    end.


start(E,M) ->
    DC=spawn(?MODULE ,dryCleaner ,[0,0]),
    [spawn (?MODULE, employee, [DC]) || _ <- lists:seq(1, E)],
    [spawn (?MODULE, dryCleanMachine, [DC]) || _ <- lists:seq(1, M)].