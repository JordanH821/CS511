-module(gui).
-export([start_gui/0, init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").

%% File with records definitions to be used as state for the client
-include_lib("./defs.hrl").

-define(SYSTEM, 0).
-define(CMDLINE, 1).
-define(NOTEBOOK, 2).
-define(MAX_CONNECTIONS, 100000).

-define(SYSTEM_NAME, "System").

-spec do_join(_ClientPID, _ClientName, _ClientID, _Ref, _ChatName) -> _.
-spec start_gui() -> _.
-spec init(_Server) -> _.
-spec do_init(_Server) -> _.
-spec handle_event(_WX, _State) -> _.
-spec handle_info(_Msg, _State) -> _.
-spec do_send_msg(_Ref, _ClientName, _ClientPID, _Message) -> _.
-spec do_whoami(_St, _Ref, _ClientPID) -> _.
-spec do_new_nick(_St, _Ref, _ClientPID, _Nick) -> _.
-spec do_leave_chatroom(_ClientName, _ChatName, _Ref, _ClientPID) -> _.
-spec do_quit(_ClientPID, _Ref) -> _.

% This record defines the structure of the
% client process.
%
% It contains the fields:
%
% parent: it stores the top window (used with operations
%         related to display widgets in the GUI.
% gui: it stores the name the GUI process.
% client: it stores the name of the client process.
-record(state,
	{
	 parent,
	 client,
	 gui,
	 clientid % unique part of name, as integer
	}).

start_gui() ->
    Server = wx:new(),
    wx_object:start_link(?MODULE, Server, []).


init(Server) ->
    wx:batch(fun () -> do_init(Server) end).

do_init(Server) ->

    % It creates a unique name for the client and gui processes
    {ClientName, ClientID} = find_unique_name("client_", ?MAX_CONNECTIONS),
    {GUIName,_ }           = find_unique_name("gui_", ?MAX_CONNECTIONS),

    % If any of the name choosen above are taken at this point, everything crashes!
    register(to_atom(GUIName), self()),
    {Nickname, _} = find_unique_name("user", ?MAX_CONNECTIONS),
%%    helper:start(to_atom(ClientName), client:initial_state(Nickname, GUIName) , fun client:main/1),
    %% Spawn and register client
    Pid = spawn(fun() -> apply(fun client:main/1, [client:initial_state(Nickname, GUIName)]) end),
    catch(unregister(to_atom(ClientName))),
    register(to_atom(ClientName), Pid),


    %% Starting GUI
    Frame = wxFrame:new(Server, -1, "Chat", []),
    Parent = Frame,
    Panel = wxPanel:new(Parent, []),


    %% Widgets: command line and system tab
    Cmd  = wxTextCtrl:new(Panel, -1, [{value, ""}, {style, ?wxTE_PROCESS_ENTER}]),
    label(ClientID, ?CMDLINE, Cmd),

    Ntbk = wxAuiNotebook:new(Panel,[{style,?wxAUI_NB_DEFAULT_STYLE}]),
    label(ClientID, ?NOTEBOOK, Ntbk),

    Tab = create_tab(ClientName, ?SYSTEM_NAME, "Welcome to CCHAT v. 0.2"),
    label(ClientID, ?SYSTEM, Tab),

    %% Sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(MainSizer, Ntbk, [{flag, ?wxEXPAND}, {proportion,1}]),
    wxSizer:addSpacer(MainSizer,10),
    wxSizer:add(MainSizer, Cmd, [{flag, ?wxEXPAND}]),
    wxPanel:setSizer(Panel, MainSizer),

    wxFrame:show(Frame),

    focus(with_label(ClientName, ?CMDLINE)),

    wxTextCtrl:connect(Cmd, command_text_enter),

    wxAuiNotebook:connect(Ntbk, command_auinotebook_button),

    {Panel, #state{parent=Panel, client=ClientName, clientid=ClientID, gui=GUIName}}.

%% Join a chatroom
do_join(ClientPID, ClientName, ClientID, Ref, ChatName) ->
    %% tell client we want to join chatroom with name ChatName
    ClientPID!{request, self(), Ref, {join, ChatName}},
    receive
	{result, ClientPID, Ref, {dummy_target, Response}} ->
	    io:format("GUI received dummy response '~s'~n", [Response]);
	{result, ClientPID, Ref, Response} ->
	    case Response of
		%% Cannot join, as client is already in chatroom
		err ->
		    %% tell system tab that client is already in chatroom
		    write_chatroom(with_label(ClientName, ?SYSTEM), "+ Already in chatroom " ++ ChatName);
		%% If not err, then response is the history of the chatroom
		History ->
		    %% We write in the system tabl that we have joined the chatroom
		    write_chatroom(with_label(ClientName, ?SYSTEM), "+ Joined " ++ ChatName),
		    %% Create a new tab, the name of which is ChatName
		    Tab = create_tab(ClientName, ChatName, "* Chatroom " ++ ChatName),
		    %% create a label for it? Not quite sure, this is a relic from Chalmers
		    label(ClientID, chatroom_id(ChatName), Tab),
		    %% goes through history, writing out each message
		    lists:foreach(
		      fun({Nickname, Message}) ->
			      write_chatroom(with_label(ClientName, chatroom_id(ChatName)), Nickname ++ ": " ++ Message)
		      end,
		      History
		     )
	    end
    end.

%% Leave a chatroom
do_leave_chatroom(ClientName, ChatName, Ref, ClientPID) ->
    %% tell client we want to leave chatroom with name ChatName
    ClientPID!{request, self(), Ref, {leave, ChatName}},
    receive
	{result, ClientPID, Ref, {dummy_target, Response}} ->
	    io:format("GUI received dummy response '~s'~n", [Response]);
	{result, ClientPID, Ref, Response} ->
	    case Response of
		%% client has successfully left chat
		ok ->
		    %% close the tab of that chatroom
		    close_tab(with_label(ClientName, ?NOTEBOOK), ChatName),
		    %% write in the system tab that we left the chatroom
	            write_chatroom(with_label(ClientName, ?SYSTEM), "+ Left chatroom " ++ ChatName);
		%% client could not leave as client was not in that chatroom
		err ->
		    %% write in chatroom saying we cannot leave
		    write_chatroom(with_label(ClientName, ?SYSTEM), "+ Cannot leave chatroom " ++ ChatName ++ ". not in chatroom!")
	    end
    end.

%% executes quit protocol
do_quit(ClientPID, Ref) ->
    %% tells client we want to quit
    ClientPID!{request, self(), Ref, quit},
    %% waits for client to tell us we're welcome to quit
    receive
	{result, ClientPID, Ref, {dummy_target, Response}} ->
	    io:format("GUI received dummy response '~s'~n", [Response]);
	{ClientPID, Ref, ack_quit} -> 
	    exit(normal)
    end.

%% executes send new message protocol
do_send_msg(Ref, ClientName, ClientPID, Message) ->
    %% get name of chatroom from the active tab
    ChatName = active_chatroom(with_label(ClientName, ?NOTEBOOK)),
    %% check if tab is system tab
    case ChatName of
	%% if it IS the system tab, then we error
	"System" -> write_chatroom(with_label(ClientName, ?SYSTEM), "+ Cannot send messages to " ++ ChatName);
	%% if not system tab, continue with protocol
	_ ->
	    %% tell client we'd like to send the following message to the name chatroom
	    ClientPID!{request, self(), Ref, {outgoing_msg, ChatName, Message}},
	    %% we wait to receive an ack from the client, including the
	    %% client's current nickname
	    receive
		{result, ClientPID, Ref, {dummy_target, Response}} ->
		    io:format("GUI received dummy response '~s'~n", [Response]);
		{result, ClientPID, Ref, {_, Nickname}} ->
		    %% write to chatroom
		    write_chatroom(with_label(ClientName, chatroom_id(ChatName)), Nickname ++ ": " ++ Message)
	    end
    end.

%% executes whoami protocol
do_whoami(St, Ref, ClientPID) ->
    %% ask client what current nickname is
    ClientPID!{request, self (), Ref, whoami},
    %% wait for client to tell us
    receive
	{result, ClientPID, Ref, {dummy_target, Response}} ->
	    io:format("GUI received dummy response '~s'~n", [Response]);
	{result, ClientPID, Ref, Nickname} ->
	    %% tell user through system tab what nickname is
	    write_chatroom(with_label(St#state.client, ?SYSTEM), "+ Nickname is \"" ++ Nickname ++ "\".")
    end.

%% executes nick protocol to change user nickname
do_new_nick(St, Ref, ClientPID, Nick) ->
    %% tell client we would like to change nickname
    ClientPID!{request, self (), Ref, {nick, Nick}},
    %% listen for client's response
    receive
	{result, ClientPID, Ref, {dummy_target, Response}} ->
	    io:format("GUI received dummy response '~s'~n", [Response]);
	{result, ClientPID, Ref, Response} ->
	    %% checks type of response
	    case Response of
		%% nickname is same as current nickname
		err_same -> write_chatroom(with_label(St#state.client, ?SYSTEM), "+ Error: Could not change nickname. Requested nickname same as current nickname!");
		%% nickname is used by someone else
		err_nick_used -> write_chatroom(with_label(St#state.client, ?SYSTEM), "+ Error: Could not change nickname. Requested nickname already used by another client!");
		%% we successfully changed nickname
		ok_nick -> write_chatroom(with_label(St#state.client, ?SYSTEM), "+ Nickname updated! New nickname: \"" ++ Nick ++ "\".")
	    end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event(#wx{ event = #wxCommand{type = command_text_enter, cmdString = Item} },
             St = #state{ parent = _Panel, client = ClientName, clientid = _ClientID }) ->

    clear_text(with_label(ClientName, ?CMDLINE)),
    Cmd = lexgrm:parse_cmd(Item),
    Ref = make_ref(),
    ClientPID = whereis(list_to_atom(ClientName)),
    ClientID = St#state.clientid,
    _Panel = St#state.parent,
    case Cmd  of

	%% Disconnect from the server
	quit ->
	    do_quit(ClientPID, Ref);

	%% Joining a new chatroom
	{join, ChatName}   ->
	    do_join(ClientPID, ClientName, ClientID, Ref, ChatName);

        % /leave
	leave -> 
	    ChatName = active_chatroom(with_label(ClientName, ?NOTEBOOK)),
	    do_leave_chatroom(ClientName, ChatName, Ref, ClientPID);
	% /leave #chatroom
	{leave, ChatName} -> 
	    do_leave_chatroom(ClientName, ChatName, Ref, ClientPID);
	%% Sending a message
	{msg, Message}     ->
	    do_send_msg(Ref, ClientName, ClientPID, Message);

	%% /whoami
	whoami -> 
            do_whoami(St, Ref, ClientPID);

	%% /nick nickname
	{nick, Nick} -> 
            do_new_nick(St, Ref, ClientPID, Nick);

	%% The given command was wrong
	{ignore, Line}    ->
	    write_chatroom(with_label(St#state.client, ?SYSTEM), "+ Command not recognized: " ++ Line)
    end,
    focus(with_label(ClientName, ?CMDLINE)),
    {noreply, St};


% Clicking the X on a tab
handle_event(#wx{ event = #wxAuiNotebook{type = command_auinotebook_button, selection = _TabPos} },
             St = #state{ parent = _Panel, client = ClientName }) ->
    Ref = make_ref(),
    ChatName = active_chatroom(with_label(ClientName, ?NOTEBOOK)),
    ClientPID = whereis(list_to_atom(ClientName)),
    do_leave_chatroom(ClientName, ChatName, Ref, ClientPID),
    {noreply, St};

handle_event(WX = #wx{}, State = #state{}) ->
    io:format("#wx: ~p~n",[WX]),
    io:format("#state: ~p~n",[State]),
    {noreply, State}.

%% Callbacks handled as normal gen_server callbacks (not used)
handle_info(_Msg, State) ->
    {noreply, State}.

handle_call(shutdown, _From, State) ->
    {stop, normal, ok, State};

%% Here, the GUI receives a message from the client process!
handle_call({msg_to_GUI, Chatroom, CliNick, Msg}, _From, State = #state{ client = ClientName }) ->
    write_chatroom( with_label(ClientName, chatroom_id(Chatroom)), CliNick ++ ": " ++ Msg),
    {reply, ok, State};

%% Here, the GUI receives a message to the system tab
handle_call({msg_to_SYSTEM, Msg}, _From, State = #state{ client = ClientName }) ->
    write_chatroom(with_label(ClientName, ?SYSTEM), "* "++Msg),
    {reply, ok, State} ;

handle_call(_Msg, _From, State) ->
    {reply, {error,nyi}, State}.

handle_cast(_Msg, State) ->
    {noreply,State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _State) ->
    ok.


%% Auxiliary functions

%% Finding a unique name
find_unique_name(Prefix,N) ->
    Num = rand:uniform(N),
    MStr = integer_to_list(Num),
    Name = Prefix++MStr,
    case whereis(to_atom(Name)) of
        undefined -> {Name, Num} ;
        _         -> find_unique_name(Prefix,N)
    end.

%% GUI
clear_text(ID) ->
    CmdLine = typed_search(ID, wxTextCtrl),
    wxTextCtrl:setValue(CmdLine, "").

create_tab(ClientName, Title, Init) ->
    Ntbk = typed_search(with_label(ClientName, ?NOTEBOOK), wxAuiNotebook),
    NtbkPanel = wxPanel:new(Ntbk, []),
    Msgs = wxTextCtrl:new(NtbkPanel, -1,
			  [{value, Init},
			   {style, ?wxTE_MULTILINE}]),
    wxTextCtrl:setEditable(Msgs, false),
    wxTextCtrl:setInsertionPointEnd(Msgs),
    NtbkSizer  = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:addSpacer(NtbkSizer, 10),
    wxSizer:add(NtbkSizer, Msgs, [{flag, ?wxEXPAND}, {proportion,1}]),
    wxPanel:setSizer(NtbkPanel, NtbkSizer),
    wxAuiNotebook:addPage(Ntbk,NtbkPanel,Title),
    wxWindow:setFocus(NtbkPanel),
    Msgs.

active_chatroom(ID) ->
    Ntbk = typed_search(ID, wxAuiNotebook),
    PageNumber = wxAuiNotebook:getSelection(Ntbk),
    Title = wxAuiNotebook:getPageText(Ntbk, PageNumber),
    Title.

close_tab(NotebookID, TabName) ->
    Ntbk = typed_search(NotebookID, wxAuiNotebook),
    Max  = wxAuiNotebook:getPageCount(Ntbk),
    Tabs = [ {wxAuiNotebook:getPageText(Ntbk,N), N} || N <- lists:seq(0,Max-1) ],
    {_, PageNumber} = lists:keyfind(TabName, 1, Tabs),
    Page = wxAuiNotebook:getPage(Ntbk,PageNumber),
    wxWindow:destroyChildren(Page),
    wxAuiNotebook:removePage(Ntbk, PageNumber).

write_chatroom(ID, String) ->
    DMesg = typed_search(ID,wxTextCtrl),
    wxTextCtrl:writeText(DMesg, "\n"++String).

%% Labels
focus(ID) ->
    W = wxWindow:findWindowById(ID),
    wxWindow:setFocus(W).

typed_search(ID, no_cast) ->
    Result = wxWindow:findWindowById(ID),
    Result ;

typed_search(ID, Cast) ->
    Result = wxWindow:findWindowById(ID),
    {F1, F2, _, F3} = Result,
    {F1,F2,Cast,F3}.

label(ClientID, ID, Widget) ->
    Label = join_ids(ClientID, ID),
    _Result = wxWindow:setId(Widget, Label),
    ok.

with_label(ClientName, Id) ->
    CIds = lists:sublist(ClientName,8,5),
    S = lists:flatten(io_lib:format("~s~p", [CIds, Id])),
    {N, _} = string:to_integer(S),
    N.

join_ids(ClientId, Id) ->
    S = lists:flatten(io_lib:format("~p~p", [ClientId, Id])),
    {N, _} = string:to_integer(S),
    N.

chatroom_id(ChatroomName) ->
    S = lists:foldl(fun(S, Acc) -> io_lib:format("~p", [S])++Acc end, "", ChatroomName),
    list_to_integer(lists:flatten(S)).

to_atom(String) ->
    list_to_atom(String).
