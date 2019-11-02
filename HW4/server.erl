-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
    io:format("server:do_join(...): IMPLEMENT ME~n"),
    State.

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
    io:format("server:do_leave(...): IMPLEMENT ME~n"),
    State.

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
    IsUnique = lists:any(fun({_P, N}) -> N = NewNick end, maps:to_list(State#serv_st.nicks)),
	if
		IsUnique -> 
			maps:update(ClientPID, NewNick, State#serv_st.nicks),
			% UserRooms = maps:filter(fun(CR) -> lists:member(ClientPID, maps:values(CR#chat_st.registrations)) end, State#serv_st.chatrooms),
			ChatRooms = maps:filter(fun(Name, Clients) -> not lists:member(ClientPID, Clients) end, State#serv_st.registrations),
			ChatRoomPids = maps:filter(fun(Name, Pid) -> not lists:member(Name, maps:keys(ChatRooms)) end, ChatRooms),
			maps:map(fun({Name, Pid}) -> 
				Pid!{self(), Ref, update_nick, ClientPID, NewNick}
				end, ChatRoomPids),
			ClientPID!{self(), Ref, ok_nick};
		true ->
			ClientPID!{self(), Ref, err_nick_used}
	end.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
    io:format("server:do_client_quit(...): IMPLEMENT ME~n"),
    State.
