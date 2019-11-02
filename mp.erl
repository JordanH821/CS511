-module(mp).
-compile(export_all).


echo() ->
    receive
	{echo,From, Ref, Msg} ->
	    From ! {self(),Ref,Msg},
	    echo();
	{stop} ->
	    ok
end.

fact(0) ->
    1;
fact(N) when N>0 ->
    N*fact(N-1).

fact_loop(S) ->
    receive 
	{request,From,Ref,N} ->
	    From!{self(),Ref,fact(N)},
	    fact_loop(S+1);
	{query,From,Ref} ->
	    From!{self(),Ref,S},
	    fact_loop(S);
	{stop} ->
	    ok
    end.

gen_server(State,F) ->
    receive 
	{request,From,Ref,Input} ->
	    case (catch(F(State,Input))) of
		{'EXIT',Reason} ->
		    From!{self(),Ref,error,Reason},
		    gen_server(State,F);
		{NewState,Result} ->
		    From!{self(),Ref,ok,Result},
		    gen_server(NewState,F);
		ThrowException ->
		    From!{self(),Ref,error,ThrowException},
		    gen_server(State,F)
	    end;	
	{update,From,Ref,G} ->
	    From!{self(),Ref,ok},
	    gen_server(State,G);
	{query,From,Ref} ->
	    From!{self(),Ref,State},
	    gen_server(State,F);
	{stop} ->
	    ok
    end.


sem(0) -> 		  
    receive
	{release,_From} ->
	    sem(1)
    end;	 
sem(P) when P>0 ->
    receive 
	{acquire,From} ->
	    From!{self(),ok},
	    sem(P-1);
	{release,_From} ->
	    sem(P+1)
    end.

release(S) ->
	S!{release}.

acquire(S) ->
	R = make_ref(),
	S!{acquire, self(), R},
	receive
		{S,R,ok} ->
			ok
	end.


client1(S) ->
    io:format("A~n"),
    S!{release,self()}.

client2(S) ->
    S!{acquire,self()},
    receive 
	{S,ok} ->
	    io:format("B~n")
    end.
    
    
start() ->
     S = spawn(?MODULE,sem,[0]),
    spawn(?MODULE,client2,[S]),
    spawn(?MODULE,client1,[S]).



%%turnstiles using message passing
counter_loop(S) ->
	receive
		{bump} -> 
			counter_loop(S+1);
		{read, From, Ref} ->
			From!{self(), Ref, S},
			counter_loop(S);
		{stop} ->
			ok
	end.

turnstile(0, _C) ->
	ok;
turnstile(N, C) when N>0 ->
	C!{bump},
	turnstile(N-1, C).

startT(N) ->
	C = spawn(?MODULE, counter_loop, [0]),
	spawn(?MODULE, turnstile, [N, C]),
	spawn(?MODULE, turnstile, [N, C]),
	C.