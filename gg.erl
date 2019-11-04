-module(gg).
-compile(export_all).

server() ->
    receive
        {From, Ref, start} ->
            S=spawn(?MODULE, servlet, [From, rand:uniform(20)]),
            From!{self(),Ref,S},
            server()
    end.

servlet(C, Number) ->
    receive
        {Cl, Ref, guess, N} when N==Number ->
            Cl!{self(), Ref, gotIt};
        {Cl, Ref, guess, N} ->
            Cl!{self(), Ref, tryAgain},
            servlet(cl, Number)
    end.


client(S) ->
    R = make_ref(),
    S!{self(), R, start},
    receive
        {S, R, Servlet} ->
            client_loop(Servlet, 0)
    end.

client_loop(Servlet, C) ->
    Servlet!{self(), make_ref, guess, rand:uniform(20)},
    receive
        {Servlet, R, gotIt} ->
            exit(gotIt);
        {Servlet, R, tryAgain} ->
            client_loop(Servlet, C + 1)
    end.

start() ->
    spawn(?MODULE, server, []).
