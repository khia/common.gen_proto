%% DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
%% Copyright (c) 2009-2011, ILYA Khlopotov. All rights reserved.
%%
%% The contents of this file are subject to the terms of either the GNU
%% General Public License ("GPL") version 2, or any later version,
%% or the Common Development and Distribution License ("CDDL") 
%% (collectively, the "License"). 
%% You may not use this file except in compliance with the License. 
%% 
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see http://www.gnu.org/licenses/
%%
%% You should have received a copy of the 
%% Common Development and Distribution License along with this program.  
%% If not, see http://www.opensource.org/licenses/

-module(common.gen_proto.transport).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("debug.hrl").
-include("utils.hrl").
-include("gen_proto.hrl").
-include("internal.hrl").

%%--------------------------------------------------------------------
%% Import libraries
%%--------------------------------------------------------------------
-import(lists).
-import(proplists).
-import(proc_lib).
-import(inet).
-import(gen_tcp).


%%--------------------------------------------------------------------
%% API

%% Public API
-export([listen/2, accept/3, session_id/2, controlling_process/3, 
	 close/2, shutdown/3, send/3, fix_opts/1, check_opts/1, connect/3,
	 session_local_id/1, session_remote_id/1, update_socket/1,
	 session_matchspec_local_id/1, session_matchspec_remote_id/1
	]).
%% TODO
connect(tcp, Opts, Timeout) ->
    ?debug("About to ~p.",[Opts],connect),
    Addr = proplists:get_value(address, Opts, []),
    ?assert(undefined =/= Addr),
    Port = proplists:get_value(port, Opts),
    ?assert(undefined =/= Port),
    Extra_Opts = proplists:get_value(opts, Opts, []),
    Fixed = lists:flatten([binary, {active, true},
			   Extra_Opts]),
    ?debug("About to gen_tcp:connect(~p, ~p).",[Port, Fixed],connect),
    gen_tcp:connect(Addr, Port, Fixed, Timeout);
connect(ssl, Opts, Timeout) ->
    Addr = proplists:get_value(address, Opts, []), 
    Port = proplists:get_value(port, Opts),
    ?assert(undefined =/= Port),
    Extra_Opts = proplists:get_value(opts, Opts, []),
    SSL_Opts = proplists:get_value(ssl, Opts, []),
    Fixed = lists:flatten([Addr, binary, {active, true}, SSL_Opts, Extra_Opts]),
    ssl:connect(Port, Fixed, Timeout);
connect(Transport, Opts, Timeout) ->
    Transport:connect(Transport, Opts, Timeout).
    
listen(tcp, Opts) ->
    ?debug("About to ~p.",[Opts],listen),
    Addr = case proplists:get_value(address, Opts, []) of
	       [] -> [];
	       IP -> {ip, IP}
	   end,
    Port = proplists:get_value(port, Opts),
    ?assert(undefined =/= Port),
    Extra_Opts = proplists:get_value(opts, Opts, []),
    Fixed = lists:flatten([Addr, binary, {active, true}, Extra_Opts]),
    ?debug("About to gen_tcp:listen(~p, ~p).",[Port, Fixed],listen),
    gen_tcp:listen(Port, Fixed);
listen(ssl, Opts) ->
    Addr = proplists:get_value(address, Opts, []), 
    Port = proplists:get_value(port, Opts),
    ?assert(undefined =/= Port),
    Extra_Opts = proplists:get_value(opts, Opts, []),
    SSL_Opts = proplists:get_value(ssl, Opts, []),
    Fixed = lists:flatten([Addr, binary, {active, true}, SSL_Opts, Extra_Opts]),
    ssl:listen(Port, Fixed);
listen(Transport, Opts) ->
    Transport:listen(Transport, Opts).

accept(tcp, Listen_Socket, _Timeout) -> catch gen_tcp:accept(Listen_Socket);
accept(ssl, Listen_Socket, Timeout) -> 
    try ssl:transport_accept(Listen_Socket, Timeout) of 
	{ok, SSL_Socket} ->
	    case ssl:ssl_accept(SSL_Socket) of 
		ok -> {ok, SSL_Socket};
		{error, _Reason} = Error -> Error
	    end
    catch
	_:Reason ->
	    {error, {exception, Reason}}
    end;
accept(Transport, Listen_Socket, Timeout) ->
    catch Transport:accept(Transport, Listen_Socket, Timeout).

session_id(tcp, Socket) ->
    case ip(Socket) of
	{Local_Address, Local_Port, Remote_Address, Remote_Port} ->
	    {tcp, Local_Address, Local_Port, Remote_Address, Remote_Port};
	Error -> 
	    ?debug("Cannot determine session id ~p.", [Error], session_id),
	    undefined
    end;
session_id(ssl, Socket) ->
    case ip(Socket) of
	{Local_Address, Local_Port, Remote_Address, Remote_Port} ->
	    {ssl, Local_Address, Local_Port, Remote_Address, Remote_Port};
	Error -> 
	    ?debug("Cannot determine session id ~p.", [Error], session_id),
	    undefined
    end;
session_id(Transport, Socket) ->
    catch Transport:session_id(Transport, Socket).

session_local_id({_Transport, 
		  Local_Address, Local_Port, _Remote_Address, _Remote_Port}) -> 
    {Local_Address, Local_Port}.
session_remote_id({_Transport, 
		  _Local_Address, _Local_Port, Remote_Address, Remote_Port}) ->
    {Remote_Address, Remote_Port}.

session_matchspec_local_id({_Transport, 
		  Local_Address, Local_Port, _Remote_Address, _Remote_Port}) -> 
    {'_', Local_Address, Local_Port, '_', '_'}.
session_matchspec_remote_id({_Transport, 
		  _Local_Address, _Local_Port, Remote_Address, Remote_Port}) -> 
    {'_', '_', '_', Remote_Address, Remote_Port}.

update_socket(#session{id = Id, 
		       socket = #socket{mode = listener} = Socket} = Session) ->
    {Remote_Address, Remote_Port} = session_remote_id(Id),
    Session#session{socket = Socket#socket{remote_ip = Remote_Address,
					   remote_port = Remote_Port}};
update_socket(#session{id = Id, 
		       socket = #socket{mode = connector} = Socket} = Session) ->
    {Local_Address, Local_Port} = session_local_id(Id),
    Session#session{socket = Socket#socket{local_ip = Local_Address,
					   local_port = Local_Port}}.

controlling_process(tcp, Socket, Session) ->
    catch gen_tcp:controlling_process(Socket, Session);
controlling_process(ssl, Socket, Session) ->
    catch ssl:controlling_process(Socket, Session);
controlling_process(Transport, Socket, Session) ->
    catch Transport:controlling_process(Transport, Socket, Session).

close(tcp, Socket) -> catch gen_tcp:close(Socket);
close(ssl, Socket) -> catch ssl:close(Socket); %% FIX ME
close(Transport, Socket) -> catch Transport:close(Transport, Socket).
    
shutdown(tcp, Socket, How) -> catch gen_tcp:shutdown(Socket, How);
shutdown(ssl, Socket, How) -> catch ssl:shutdown(Socket, How); %% FIX ME
shutdown(Transport, Socket, How) -> 
    catch Transport:shutdown(Transport, Socket, How).

send(tcp, Socket, Packet) -> catch gen_tcp:send(Socket, Packet);
send(ssl, Socket, Packet) -> catch ssl:send(Socket, Packet); %% FIX ME
send(Transport, Socket, Packet) -> 
    catch Transport:send(Transport, Socket, Packet).

ip(Socket) ->
    case inet:peername(Socket) of
	{ok, {Remote_Address, Remote_Port}} -> 
	    case inet:sockname(Socket) of
		{ok, {Local_Address, Local_Port}} ->
		    {Local_Address, Local_Port, Remote_Address, Remote_Port};
		Local_Error -> {error, {sockname, Local_Error}}
	    end;
	Remote_Error -> {error, {peername, Remote_Error}}
    end.

-spec(is_ip/1 :: (IP :: ip()) -> boolean()).
is_ip({_,_,_,_}) -> true;
is_ip({_,_,_,_,_,_,_,_}) -> true;
is_ip(_) -> false.

check_opts(#session{socket = #socket{mode = listener}} = Handler) ->
    check_listener_opts(Handler);
check_opts(#session{socket = #socket{mode = connector}} = Handler) ->
    check_connector_opts(Handler).

check_listener_opts(#session{socket = #socket{
			       local_ip = Addr,
			       local_port = Port,
			       opts = Opts
			      }}) when is_integer(Port), is_list(Opts) ->
    is_ip(Addr);
check_listener_opts(_) -> false.
check_connector_opts(#session{socket = #socket{
			       remote_ip = Addr,
			       remote_port = Port,
			       opts = Opts
			      }}) when is_integer(Port), is_list(Opts) ->
    is_ip(Addr);
check_connector_opts(_) -> false.

fix_opts(#session{socket = #socket{timeout = undefined} = Socket} = H) ->
    fix_opts(H#session{socket = Socket#socket{timeout = 5000}}); %% FIXME
fix_opts(#session{socket = #socket{opts = undefined} = Socket} = H) -> 
    fix_opts(H#session{socket = Socket#socket{opts = []}}); 
fix_opts(Socket) -> Socket.
