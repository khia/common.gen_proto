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

-module(common.gen_proto.mgr_sup).

-behaviour(supervisor).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("debug.hrl").
-include("gen_proto.hrl").
-include("internal.hrl").

-define(session_sup, common.gen_proto.super).
%%--------------------------------------------------------------------
%% Imported libraries
%%--------------------------------------------------------------------
-import(proplists).
-import(supervisor).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/0,
	 start_link/1,
	 stop/1
        ]).

-export([add_session/2, add_session/3, 
	 terminate_session/1, terminate_session/2, 
	 delete_session/1, delete_session/2]).
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
start_link() -> start_link([]).
start_link(Args) -> start_link(?MODULE, Args).    
start_link(Name, Args) -> 
    ?debug("About to start supervisor ~p.", [Name], start_link),
    supervisor:start_link({local,Name}, ?MODULE, Args).

add_session(Socket, Session) ->
    add_session(?MODULE, Socket, Session).
add_session(Sup, Socket, #session{id = Id} = Session) ->
     ?debug("About to add session ~p to supervisor ~p", [Id, Sup], add_session),
     Handler = {Id,
 	       {?session_sup,start_link,[Socket, Session]},
 	       temporary, %% temporary - child process should never be restarted
 	       5000,
 	       worker, %% to force process die
 	       [?session_sup]},
    supervisor:start_child(Sup, Handler).	     

terminate_session(Id) -> terminate_session(?MODULE, Id).
terminate_session(Sup, Id) -> supervisor:terminate_child(Sup, Id).
delete_session(Id) -> delete_session(?MODULE, Id).
delete_session(Sup, Id) -> supervisor:delete_child(Sup, Id).
    
%%--------------------------------------------------------------------
%% Function: stop/1
%% Description: Stops the supervisor
%%--------------------------------------------------------------------
stop(_Pid) ->
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
init(Args) ->
    ?debug("About to start supervisor with args ~p.", [Args], init),
    MGR = {mgr,
 	       {?mgr,start_link,[]},
 	       transient,
 	       5000,
 	       supervisor,
 	       [?mgr]},
    {ok,{{one_for_one,10,5}, [MGR]}}.

%%====================================================================
%% Internal functions
%%====================================================================
