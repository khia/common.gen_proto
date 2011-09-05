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

-module(common.gen_proto.session_sup).

-behaviour(supervisor).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("debug.hrl").
-include("gen_proto.hrl").
-include("internal.hrl").

%%--------------------------------------------------------------------
%% Imported libraries
%%--------------------------------------------------------------------
-import(proplists).
-import(supervisor).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/2,
	 start_fsm/3,
	 stop/1
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 init/1
        ]).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/1
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Socket, #session{fsm = FSM_Module, pdu = PDU_Module} = Handler) ->
    ?debug("HANDLER: ~p",[Handler], start_link),
    ?assert(FSM_Module =/= undefined),
    ?assert(PDU_Module =/= undefined),
    case supervisor:start_link(?MODULE, []) of
	{ok, Sup} ->
	    ?debug("SUP: ~p",[Sup], start_link),
	    case start_session(Sup, Socket, Handler) of
		{ok, Session} ->
		    ?debug("SESSION: ~p",[Session], start_link),
		    case start_fsm(Sup, FSM_Module, PDU_Module) of
			{ok, FSM} -> 
			        ?debug("FSM: ~p",[FSM], start_link),
			    {ok, Sup, {Session, FSM}};
			FSM_Error -> 
			    stop_session(Sup),
			    FSM_Error
		    end;
		Session_Error -> Session_Error
	    end;
	Error -> Error
    end.

start_session(Sup, Socket, Handler) ->
    Session_Spec = {session,
	       {?session, start_link, [Socket, Handler]},
	       temporary,
	       5000,
	       worker,
	       [?session]},
    ?debug("About to start_session ~p", [Session_Spec], start_session),
    supervisor:start_child(Sup, Session_Spec).
    
stop_session(Sup) -> 
    supervisor:terminate_child(Sup, session).
    
start_fsm(Sup, FSM, PDU) ->
    Handler = {fsm,
 	       {FSM, start_link, [PDU]},
 	       temporary,
 	       5000,
 	       worker,
 	       [FSM]},
    ?debug("About to start_fsm ~p", [Handler],start_fsm),
    supervisor:start_child(Sup, Handler).
    

%%--------------------------------------------------------------------
%% Function: stop/1
%% Description: Stops the supervisor
%%--------------------------------------------------------------------
stop(__Pid) ->
    %% TODO
    %%    supervisor:stop(?MODULE, {FSM}).
    ok.

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init([]) ->
%%    {ok,{{one_for_all,10,5}, []}}.
    {ok,{{one_for_all,0,1}, []}}. %% terminate supervisor if session or fsm died

%%====================================================================
%% Internal functions
%%====================================================================
