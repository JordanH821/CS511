-module(server).
-compile(export_all).


echo() ->
    receieve
        {echo, From, Msg} ->
            From!{Msg},
            echo();
        {stop} ->
            ok
    end.