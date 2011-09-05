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

-module(common.gen_proto.sup).

-behaviour(supervisor).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("debug.hrl").
-include("utils.hrl").
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
	 start_link/0,
	 start_link/1,
	 start_link/2,
	 stop/1
        ]).

-export([add_listener/1, add_listener/2, add_connector/1, add_connector/2]).
%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 init/1
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    start_link([]).
start_link(Args) ->
    start_link(?MODULE, Args).    
start_link(Name, Args) -> 
    ?debug("About to start supervisor ~p.", [Name], start_link),
    supervisor:start_link({local,Name}, ?MODULE, Args).

add_listener(Session) -> add_listener(?MODULE, Session).
add_listener(Sup, #session{id = Id} = Session) ->
    ?debug("About to add listener ~p to supervisor ~p (~p)", 
	   [Id, Sup, whereis(Sup)], add_listener),
    Handler = {Id,
	       {?server, start_link, [Session]},
	       transient,
	       5000,
	       worker,
	       [?server]},
    supervisor:start_child(Sup, Handler).
%%    {ok, self()}.

add_connector(Session) -> add_connector(?MODULE, Session).
add_connector(Sup, #session{id = Id} = Session) ->
    ?debug("About to add connector ~p to supervisor ~p",[Id, Sup],add_connector),
    Handler = {Id,
	       {?client, start_link, [Session]},
	       transient,
	       5000,
	       worker,
	       [?client]},
    supervisor:start_child(Sup, Handler).
%%    {ok, self()}.

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
    MGR = {mgr_sup,
 	       {?mgr_sup,start_link,[]},
 	       temporary,
 	       5000,
 	       supervisor,
 	       [?mgr_sup]},
    {ok,{{one_for_one,10,5}, [MGR]}}.

%%====================================================================
%% Internal functions
%%====================================================================
