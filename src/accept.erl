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

-module(common.gen_proto.accept).

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
-import(proc_lib).


%%--------------------------------------------------------------------
%% API

%% Public API
-export([start_link/3, stop/1]).

%% OTP callbacks
-export([init/1]).

%% Test functions
%%-export([test/1]).

start_link(Listen_Socket, Listen_Pid, Handler) ->
    proc_lib:spawn_link(?MODULE, init, [{Listen_Socket, Listen_Pid, Handler}]).

stop(Pid) -> catch exit(Pid, normal).

init({Listen_Socket, Listen_Pid, #session{socket = #socket{
					    transport = Transport,
					    local_ip = _Address,
					    local_port = _Port,
					    timeout = Timeout
					   }} = Handler0}) ->
    case transport:accept(Transport, Listen_Socket, Timeout) of
	{ok, Socket} ->
            %% Send the cast message to the listener process 
	    %% to create a new acceptor
	    server:create(Listen_Pid, self()),
	    Id = transport:session_id(Transport, Socket),
	    Handler1 = transport:update_socket(Handler0#session{id = Id}),
	    case mgr:session_start(Socket, Handler1) of
		{ok, #session{ext = #ext{session = Session, sup = Sup} = Ext}} ->
		    transport:controlling_process(Transport, Socket, Session),
		    session:connect(Ext),
		    .erlang:unlink(Sup);
		Error -> 
		    ?error("Cannot start session: ~p.", [Error], init),
		    Error
	    end;
	Else ->
	    ?error("Accept failed error: ~p.", [Else], init),
	    exit({error, accept_failed})
    end.
