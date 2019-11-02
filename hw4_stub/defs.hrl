% This record defines the structure of the client process.
% It contains the following fields:
%	gui: the name (or Pid) of the GUI process.
%	nick: nick of the GUI process	
%	con_ch: channel clients is connected to

-record(cl_st, {gui, nick, con_ch}).

% This record defines the structure of the server process.
% It contains the following fields:
%	nicks: a map of client pids to their registered nicknames
%	registrations: a map of chatroom names to lists containing all client pids registered in that chatroom
%	chatrooms: a map of chatroom names to that chatroom's pid

-record(serv_st, {nicks, registrations, chatrooms}).

% This record defines the structure of the chatroom process.
% It contains the following fields:
%	name: that chatroom's name
%	registrations: a map from a client's PID to a client's nickname. The map represents all registered clients
%       history: chat history since the beginning of that chatroom

-record(chat_st, {name, registrations, history}).
