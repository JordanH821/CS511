-module(pc).
-compile(export_all).

buffer(Size, Consumers, Producers, Capacity) ->
    receive 
        {PID, startProduce} when Size + Producers < Capacity ->
            PID!{self(), ok},
            buffer(Size, Consumers, Producers + 1, Capacity);
        {_PID, stopProduce} ->
            buffer(Size + 1, Consumers, Producers - 1, Capacity);
        {PID, startConsume} when Size - Consumers > 0 ->
            PID!{self(), ok},
            buffer(Size, Consumers + 1, Producers, Capacity);
        {_PID, stopConsume} ->
            buffer(Size - 1, Consumers - 1, Producers, Capacity)
    end.

producer(B) ->
    B!{self(), startProduce},
    receive
        {B, ok} ->
            io:format("~p done producing ~n", [self()]),
            produce,
            B!{self(), stopProduce}
    end.

consumer(B) ->
    B!{self(), startConsume},
    receive
        {B, ok} ->
            io:format("~p done consuming ~n", [self()]),
            consume,
            B!{self(), stopConsume}
    end.

start(NC, NP, Capacity) ->
    B = spawn(?MODULE, buffer, [0,0,0,Capacity]),
    [spawn(?MODULE, producer, [B]) || _ <- lists:seq(1, NP) ],
    [spawn(?MODULE, consumer, [B]) || _ <- lists:seq(1, NC) ].
