-module(ex8).
-compile(export_all).

server() ->
    receive 
        {From, Ref, start} ->
            Servlet = spawn(?MODULE, start_servlet, [Ref]),
            From!{Servlet},
            server()
    end.

servlet_loop(Ref, Num) ->
    receive
        {Pid, Ref, Number} ->
            if
                Number == Num ->
                    io:format("GotIt: ~w~n", [Number]),
                    Pid!{Ref, gotIt};
                true ->
                    io:format("Wrong guess: ~w~n", [Number]),
                    Pid!{Ref, tryAgain}
            end
    end,
    servlet_loop(Ref, Num).

start_servlet(Ref) ->
    Number = rand:uniform(10),
    servlet_loop(Ref, Number).

client(Server) ->
    Ref = make_ref(),
    Server!{self(), Ref, start},
    receive
        {Servlet} ->
            client_loop(Ref, Servlet)
    end.

client_loop(Ref, Servlet) ->
    Guess = rand:uniform(10),
    Servlet!{self(), Ref, Guess},
    receive
        {Ref, gotIt} ->
            gotIt;
        {Ref, tryAgain} ->
            client_loop(Ref, Servlet)
    end.
