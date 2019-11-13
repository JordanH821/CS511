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
	%% If the chatroom does not exist, create it
	ChatRooms = State#serv_st.chatrooms,
	Registrations = State#serv_st.registrations,
	case maps:is_key(ChatName, ChatRooms) of
		true ->
			ChatRoomPID = maps:get(ChatName, ChatRooms),
			NewChatRooms = ChatRooms,
			NewRegistration = maps:update(ChatName, lists:append([ClientPID], maps:get(ChatName, Registrations)), Registrations);

		false ->
			ChatRoomPID = spawn(chatroom, start_chatroom, [ChatName]),
			NewChatRooms = maps:put(ChatName, ChatRoomPID, ChatRooms),
			NewRegistration = maps:put(ChatName, [ClientPID], Registrations)
	end,
	
	ClientNick = maps:get(ClientPID, State#serv_st.nicks),

	%% Send register request to chatroom
	ChatRoomPID!{self(), Ref, register, ClientPID, ClientNick},

	%% return new state
	#serv_st{
	 nicks = State#serv_st.nicks, %% nickname map. client_pid => "nickname"
	 registrations = NewRegistration, %% registration map. "chat_name" => [client_pids]
	 chatrooms = NewChatRooms %% chatroom map. "chat_name" => chat_pid
	}.

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
    ChatroomPID = maps:get(ChatName, State#serv_st.chatrooms),
	Registrations = State#serv_st.registrations,
	ChatroomClientsPIDs = maps:get(ChatName, State#serv_st.registrations),
	NewRegistrations = maps:update(ChatName, ChatroomClientsPIDs -- [ClientPID], Registrations),
	ChatroomPID!{self(), Ref, unregister, ClientPID},
	ClientPID!{self(), Ref, ack_leave},
	#serv_st{
		nicks = State#serv_st.nicks,
		registrations = NewRegistrations,
		chatrooms = State#serv_st.chatrooms
	}.

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
    IsUnique = not lists:member(NewNick, maps:values(State#serv_st.nicks)),
	case IsUnique of
		false ->
			NewNicks = ClientPID!{self(), Ref, err_nick_used};
		true -> 
			NewNicks = maps:update(ClientPID, NewNick, State#serv_st.nicks),
			%	nicks: a map of client pids to their registered nicknames
			%	registrations: a map of chatroom names to lists containing all client pids registered in that chatroom
			%	chatrooms: a map of chatroom names to that chatroom's pid
			Registrations = State#serv_st.registrations,
			Chatrooms = State#serv_st.chatrooms,
			ChatroomNames = maps:keys(maps:filter(fun(_ChatName, ClientPIDs) -> not lists:member(ClientPID, ClientPIDs) end, Registrations)),
			% ChatoomPids = maps:filter(fun(Name, _Pid) -> not lists:member(Name, maps:keys(Chatrooms)) end, Chatrooms),
			ChatroomPIDs = maps:filter(fun(ChatName, _ChatPID) -> not lists:member(ChatName, ChatroomNames) end, Chatrooms),
			maps:map(fun(_Name, Pid) -> 
							% o:format("Updating chatroom: ~p~n", [pid]),
							Pid!{self(), Ref, update_nick, ClientPID, NewNick}
							end, ChatroomPIDs),
			ClientPID!{self(), Ref, ok_nick}
	end,
	#serv_st{
		nicks = NewNicks,
		registrations = State#serv_st.registrations,
		chatrooms = State#serv_st.chatrooms
	}.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
	Registrations = State#serv_st.registrations,
	Chatrooms = State#serv_st.chatrooms,
	ChatroomNames = maps:keys(maps:filter(fun(_ChatName, ClientPIDs) -> not lists:member(ClientPID, ClientPIDs) end, Registrations)),
	ChatroomPIDs = maps:filter(fun(ChatName, _ChatPID) -> not lists:member(ChatName, ChatroomNames) end, Chatrooms),
	maps:map(fun(_Name, Pid) -> 
				Pid!{self(), Ref, unregister, ClientPID}
				end, ChatroomPIDs),
	NewRegistrations = maps:map(fun(_ChatName, ClientPIDs) -> lists:delete(ClientPID, ClientPIDs) end, Registrations),
	ClientPID!{self(), Ref, ack_quit},
	#serv_st{
		nicks = State#serv_st.nicks,
		registrations = NewRegistrations,
		chatrooms = State#serv_st.chatrooms
	}.
