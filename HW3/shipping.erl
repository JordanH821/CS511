-module(shipping).
-compile(export_all).
-include_lib("./shipping.hrl").

get_ship(Shipping_State, Ship_ID) ->
    ShipList = lists:filter(fun (Ship) -> Ship#ship.id == Ship_ID end, Shipping_State#shipping_state.ships),
    if
        length(ShipList) > 0 ->
            hd(ShipList);
        true ->
            error
    end.

    

get_container(Shipping_State, Container_ID) ->
    ContainerList = lists:filter(fun (Container) -> Container#container.id == Container_ID end, Shipping_State#shipping_state.containers),
    if
        length(ContainerList) > 0 ->
            hd(ContainerList);
        true ->
            error
    end.
    

get_port(Shipping_State, Port_ID) ->
        PortList = lists:filter(fun (Port) -> Port#port.id == Port_ID end, Shipping_State#shipping_state.ports),
        if
            length(PortList) > 0 ->
                hd(PortList);
            true ->
                error
        end.

get_occupied_docks(Shipping_State, Port_ID) ->
    OccupiedDocksAtPort = lists:filter(fun({P, _D, _S}) -> P == Port_ID end, Shipping_State#shipping_state.ship_locations),
    [D || {_P, D, _S} <- OccupiedDocksAtPort].


get_ship_location(Shipping_State, Ship_ID) ->
    Location = lists:filter(fun({_P, _D, S}) -> S == Ship_ID end, Shipping_State#shipping_state.ship_locations),
    if
        length(Location) > 0 ->
            LocTup = hd(Location),
            {element(1, LocTup), element(2, LocTup)};
        true ->
            error
    end.  

get_container_weight(Shipping_State, Container_IDs) ->
    lists:foldr(fun(X, Sum) -> X#container.weight + Sum end, 0, lists:filter(fun(Container) -> lists:member(Container#container.id, Container_IDs) end, Shipping_State#shipping_state.containers)).

get_ship_weight(Shipping_State, Ship_ID) ->
    case get_ship(Shipping_State, Ship_ID) of
        error -> error;
        _Good -> get_container_weight(Shipping_State, maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory))
    end.


load_ship(Shipping_State, Ship_ID, Container_IDs) ->
    Ship = get_ship(Shipping_State, Ship_ID),
    case Ship of
        error -> error;
        _Good ->
            {Port_ID, _Dock} = (get_ship_location(Shipping_State, Ship_ID)),
            PortInventory = maps:get(Port_ID, Shipping_State#shipping_state.port_inventory),
            ContainersAtPort = lists:filter(fun(ID) -> lists:member(ID, PortInventory) end, Container_IDs),
            ShipContCap = Ship#ship.container_cap,
            Curr_Ship_Inventory = maps:get(Ship#ship.id, Shipping_State#shipping_state.ship_inventory),
            ShipCurrCont = length(Curr_Ship_Inventory),
            if
                ShipCurrCont + length(ContainersAtPort) > ShipContCap ->
                    error;
                true ->
                    if
                        length(ContainersAtPort) =/= length(Container_IDs) ->
                            io:format("Not all containers at port...\n"),
                            error;
                        true ->
                            load_ship(Shipping_State, Ship_ID, Port_ID, ContainersAtPort)
                    end
            end
    end.

load_ship(Shipping_State, Ship_ID, Port_ID, ContainersAtPort) ->
    NewShipInv = maps:map(fun(K, V) -> if
                K == Ship_ID -> V ++ ContainersAtPort;
                true -> V
                end
            end, Shipping_State#shipping_state.ship_inventory),
    NewPortInv = maps:map(fun(K, V) -> if
                K == Port_ID -> V -- ContainersAtPort;
                true -> V
                end
            end, Shipping_State#shipping_state.port_inventory),
    {ok, #shipping_state{ships = Shipping_State#shipping_state.ships,
                    containers = Shipping_State#shipping_state.containers,
                    ports = Shipping_State#shipping_state.ports,
                    ship_locations = Shipping_State#shipping_state.ship_locations,
                    ship_inventory = NewShipInv,
                    port_inventory = NewPortInv}}.


unload_ship_all(Shipping_State, Ship_ID) ->
    Ship = get_ship(Shipping_State, Ship_ID),
    case Ship of
        error -> error;
        _Good ->
            {Port_ID, _Dock} = (get_ship_location(Shipping_State, Ship_ID)),
            PortCap = (get_port(Shipping_State, Port_ID))#port.container_cap,
            PortInventory = maps:get(Port_ID, Shipping_State#shipping_state.port_inventory),
            ShipInventories = Shipping_State#shipping_state.ship_inventory,
            ShipInventory = maps:get(Ship_ID, ShipInventories),
            Curr_Ship_Inventory = maps:get(Ship#ship.id, Shipping_State#shipping_state.ship_inventory),
            ShipCurrCont = length(Curr_Ship_Inventory),
            if
                ShipCurrCont + length(PortInventory) > PortCap ->
                    error;
                true ->
                    NewShipInv = maps:map(fun(K, V) -> if
                                K == Ship_ID -> [];
                                true -> V
                                end
                            end, ShipInventories),
                    NewPortInventory = maps:map(fun(K, V) -> if
                                K == Port_ID -> V ++ ShipInventory;
                                true -> V
                                end
                            end, Shipping_State#shipping_state.port_inventory),
                    {ok, #shipping_state{ships = Shipping_State#shipping_state.ships,
                                    containers = Shipping_State#shipping_state.containers,
                                    ports = Shipping_State#shipping_state.ports,
                                    ship_locations = Shipping_State#shipping_state.ship_locations,
                                    ship_inventory = NewShipInv,
                                    port_inventory = NewPortInventory}}
            end
        end.

unload_ship(Shipping_State, Ship_ID, Container_IDs) ->
    Ship = get_ship(Shipping_State, Ship_ID),
    case Ship of
        error -> error;
        _Good ->
            {Port_ID, _Dock} = (get_ship_location(Shipping_State, Ship_ID)),
            PortCap = (get_port(Shipping_State, Port_ID))#port.container_cap,
            CurrPortInventory = length(maps:get(Port_ID, Shipping_State#shipping_state.port_inventory)),
            ShipInventories = Shipping_State#shipping_state.ship_inventory,
            ShipInventory = maps:get(Ship_ID, ShipInventories),
            ContainersOnShip = lists:filter(fun(C) -> lists:member(C, ShipInventory) end, Container_IDs),
            if
                length(ContainersOnShip) + CurrPortInventory > PortCap ->
                    error;
                length(ContainersOnShip) /= length(Container_IDs) ->
                    io:format("The given conatiners are not all on the same ship...\n"),
                    error;
                true ->
                    NewShipInv = maps:map(fun(K, V) -> if
                                K == Ship_ID -> V -- ContainersOnShip;
                                true -> V
                                end
                            end, ShipInventories),
                    NewPortInventory = maps:map(fun(K, V) -> if
                                K == Port_ID -> V ++ ContainersOnShip;
                                true -> V
                                end
                            end, Shipping_State#shipping_state.port_inventory),
                    {ok, #shipping_state{ships = Shipping_State#shipping_state.ships,
                                    containers = Shipping_State#shipping_state.containers,
                                    ports = Shipping_State#shipping_state.ports,
                                    ship_locations = Shipping_State#shipping_state.ship_locations,
                                    ship_inventory = NewShipInv,
                                    port_inventory = NewPortInventory}}
            end
        end.

set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
    Ship = get_ship(Shipping_State, Ship_ID),
    IsRealPort = get_port(Shipping_State, Port_ID),
    case {IsRealPort, Ship} of
        {error, _Else} -> error;
        {_Else, error} -> error;
        {_, _} ->
            IsRealDock = lists:member(Dock, (get_port(Shipping_State, Port_ID))#port.docks),
            IsOccupiedDock = lists:member(Dock, get_occupied_docks(Shipping_State, Port_ID)),
            case {IsRealDock, IsOccupiedDock} of
                {true, false} ->
                    NewLocations = lists:map(
                        fun({P, D, S}) -> if
                                            S == Ship_ID -> {Port_ID, Dock, Ship_ID};
                                            true -> {P, D, S}
                                          end
                                        end, Shipping_State#shipping_state.ship_locations),
                    {ok, #shipping_state{ships = Shipping_State#shipping_state.ships,
                            containers = Shipping_State#shipping_state.containers,
                            ports = Shipping_State#shipping_state.ports,
                            ship_locations = NewLocations,
                            ship_inventory = Shipping_State#shipping_state.ship_inventory,
                            port_inventory = Shipping_State#shipping_state.port_inventory}};
                {_, _} -> error
            end
    end.
                                

                                                            



%% Determines whether all of the elements of Sub_List are also elements of Target_List
%% @returns true is all elements of Sub_List are members of Target_List; false otherwise
is_sublist(Target_List, Sub_List) ->
    lists:all(fun (Elem) -> lists:member(Elem, Target_List) end, Sub_List).




%% Prints out the current shipping state in a more friendly format
print_state(Shipping_State) ->
    io:format("--Ships--~n"),
    _ = print_ships(Shipping_State#shipping_state.ships, Shipping_State#shipping_state.ship_locations, Shipping_State#shipping_state.ship_inventory, Shipping_State#shipping_state.ports),
    io:format("--Ports--~n"),
    _ = print_ports(Shipping_State#shipping_state.ports, Shipping_State#shipping_state.port_inventory).


%% helper function for print_ships
get_port_helper([], _Port_ID) -> error;
get_port_helper([ Port = #port{id = Port_ID} | _ ], Port_ID) -> Port;
get_port_helper( [_ | Other_Ports ], Port_ID) -> get_port_helper(Other_Ports, Port_ID).


print_ships(Ships, Locations, Inventory, Ports) ->
    case Ships of
        [] ->
            ok;
        [Ship | Other_Ships] ->
            {Port_ID, Dock_ID, _} = lists:keyfind(Ship#ship.id, 3, Locations),
            Port = get_port_helper(Ports, Port_ID),
            {ok, Ship_Inventory} = maps:find(Ship#ship.id, Inventory),
            io:format("Name: ~s(#~w)    Location: Port ~s, Dock ~s    Inventory: ~w~n", [Ship#ship.name, Ship#ship.id, Port#port.name, Dock_ID, Ship_Inventory]),
            print_ships(Other_Ships, Locations, Inventory, Ports)
    end.

print_containers(Containers) ->
    io:format("~w~n", [Containers]).

print_ports(Ports, Inventory) ->
    case Ports of
        [] ->
            ok;
        [Port | Other_Ports] ->
            {ok, Port_Inventory} = maps:find(Port#port.id, Inventory),
            io:format("Name: ~s(#~w)    Docks: ~w    Inventory: ~w~n", [Port#port.name, Port#port.id, Port#port.docks, Port_Inventory]),
            print_ports(Other_Ports, Inventory)
    end.
%% This functions sets up an initial state for this shipping simulation. You can add, remove, or modidfy any of this content. This is provided to you to save some time.
%% @returns {ok, shipping_state} where shipping_state is a shipping_state record with all the initial content.
shipco() ->
    Ships = [#ship{id=1,name="Santa Maria",container_cap=20},
              #ship{id=2,name="Nina",container_cap=20},
              #ship{id=3,name="Pinta",container_cap=20},
              #ship{id=4,name="SS Minnow",container_cap=20},
              #ship{id=5,name="Sir Leaks-A-Lot",container_cap=20}
             ],
    Containers = [
                  #container{id=1,weight=200},
                  #container{id=2,weight=215},
                  #container{id=3,weight=131},
                  #container{id=4,weight=62},
                  #container{id=5,weight=112},
                  #container{id=6,weight=217},
                  #container{id=7,weight=61},
                  #container{id=8,weight=99},
                  #container{id=9,weight=82},
                  #container{id=10,weight=185},
                  #container{id=11,weight=282},
                  #container{id=12,weight=312},
                  #container{id=13,weight=283},
                  #container{id=14,weight=331},
                  #container{id=15,weight=136},
                  #container{id=16,weight=200},
                  #container{id=17,weight=215},
                  #container{id=18,weight=131},
                  #container{id=19,weight=62},
                  #container{id=20,weight=112},
                  #container{id=21,weight=217},
                  #container{id=22,weight=61},
                  #container{id=23,weight=99},
                  #container{id=24,weight=82},
                  #container{id=25,weight=185},
                  #container{id=26,weight=282},
                  #container{id=27,weight=312},
                  #container{id=28,weight=283},
                  #container{id=29,weight=331},
                  #container{id=30,weight=136}
                 ],
    Ports = [
             #port{
                id=1,
                name="New York",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=2,
                name="San Francisco",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=3,
                name="Miami",
                docks=['A','B','C','D'],
                container_cap=200
               }
            ],
    %% {port, dock, ship}
    Locations = [
                 {1,'B',1},
                 {1, 'A', 3},
                 {3, 'C', 2},
                 {2, 'D', 4},
                 {2, 'B', 5}
                ],
    Ship_Inventory = #{
      1=>[14,15,9,2,6],
      2=>[1,3,4,13],
      3=>[],
      4=>[2,8,11,7],
      5=>[5,10,12]},
    Port_Inventory = #{
      1=>[16,17,18,19,20],
      2=>[21,22,23,24,25],
      3=>[26,27,28,29,30]
     },
    #shipping_state{ships = Ships, containers = Containers, ports = Ports, ship_locations = Locations, ship_inventory = Ship_Inventory, port_inventory = Port_Inventory}.
