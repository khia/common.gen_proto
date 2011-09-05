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

-module(common.gen_proto.connect).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("debug.hrl").
-include("gen_proto.hrl").
-include("internal.hrl").

%%--------------------------------------------------------------------
%% Import libraries
%%--------------------------------------------------------------------
-import(proc_lib).

%%--------------------------------------------------------------------
%% API

%% Public API
-export([start_link/2, stop/1, cleanup/1]).

%% OTP callbacks
-export([init/1]).

%% Test functions
%%-export([test/1]).

start_link(Client_Pid, Handler) -> 
    %% async start
    proc_lib:spawn_link(?MODULE, init, [{Client_Pid, Handler}]).

stop(Pid) -> catch exit(Pid, normal).

init({Client_Pid, #session{socket = #socket{
			     transport = Transport, 
			     remote_ip = Address,
			     remote_port = Port,
			     opts = Opts,
			     timeout = Timeout}} = Handler0}) ->
    ?debug("About to init ~p",[Handler0], init), 
    Transport_Opts = [{address, Address}, {port, Port}, {opts, Opts}],
    case transport:connect(Transport, Transport_Opts, Timeout) of
	{ok, Socket} ->
	    Id = transport:session_id(Transport, Socket),
	    Handler1 = transport:update_socket(Handler0#session{id = Id}),
            %% Send the cast message to the client process 
	    case mgr:session_start(Socket, Handler1) of
		{ok, #session{ext = #ext{session = Session, 
					 sup = Sup} = Ext} = Handler2} ->
		    client:create(Client_Pid, Socket, Handler2),
		    transport:controlling_process(Transport, Socket, Session),
		    ?debug("Socket = ~p Session = ~p.", 
			   [Socket, Session], init),
		    session:connect(Ext),
		    .erlang:unlink(Sup);
		Error -> 
		    ?error("Cannot start session: ~p.", [Error], init),
		    Error
	    end;
	{error, Reason} ->
	    ?error("Connect failed error: ~p.", [Reason], init),
	    client:error(Client_Pid, {connect_failed, Reason})
    end.

cleanup(Session) ->
    mgr:change_status(Session, connecting).
