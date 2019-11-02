-module(main).
-author("Elliot Wasem").
-export([start/0,start/1]).

-spec start() -> _.
-spec start(_NumClients) -> _.

%% Start 2 clients
start() ->
    %% spawn server
    spawn(server, start_server , []),
    %% spawn two guis and their clients
    spawn(gui, start_gui, []),
    spawn(gui, start_gui, []).

%% Start `NumClients` clients
start(NumClients) ->
    %% spawn server
    spawn(server, start_server , []),
    %% spawn guis and their clients
    [ spawn(gui, start_gui, []) || _ <- lists:seq(1,max(NumClients,1)) ].
