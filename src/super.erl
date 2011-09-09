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

-module(common.gen_proto.super).

-behaviour(gen_server). 


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
%%-import(lists).
-import(proplists).
-import(gen_server).

%%--------------------------------------------------------------------
%% API

%% Public API
-export([]).

%% Server Control
-export([stop/1, start_link/2, status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2,
	 terminate/2, code_change/3]).

%% Test functions
-export([test/1]).

-record(state, {fsm_module :: atom(),
		fsm :: pid(),
		session :: pid()}).

%%--------------------------------------------------------------------
%% @doc
%%    Starts server.
%% @end
-spec(start_link/2 :: (Socket :: term(), Session :: #session{}) ->
	    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Socket, Handler) ->
    case gen_server:start_link(?MODULE, {Socket, Handler}, []) of
	{ok, Sup} ->
	    case status(Sup) of
		{ok, Sup, State} ->
		    Session = proplists:get_value(session, State),
		    FSM =  proplists:get_value(fsm, State),
		    ?debug("STATE ~p",[State],start_link),
		    {ok, Sup, {Session, FSM}};
		Error_Status -> Error_Status
	    end;
	Error -> Error
    end.

%% @doc
%%     Stops server.
%% @end
-spec(stop/1 :: (Pid :: pid()) -> ok).
stop(Pid) ->
    (catch gen_server:call(Pid, stop)),
    ok.

%% @doc
%%    Returns the state of server.
%% @end
-spec(status/1 :: (Pid :: atom() | pid()) -> 
	     {ok, Pid, #state{}} | {ok, not_running, Pid} 
		 | {error, Reason :: term()}). 
status(Pid) ->
    try gen_server:call(Pid, status) of
	Result->Result
	catch
 	  _:_Reason -> {ok, not_running, Pid}
	end.

%%==================================================
%% gen_server callbacks
%%==================================================

%%--------------------------------------------------------------------
%% @doc
%%    Initiates the server. Callback function that will be invoked by gen_server.
%% @end
%%--------------------------------------------------------------------
-type(reason() :: term()).
-spec(init/1 :: (Args :: term()) -> {ok, #state{}} 
					| {ok, #state{}, Timeout :: integer()}
					| {ok, #state{}, hibernate}
					| {stop, reason()} 
					| ignore).
init({Socket, #session{fsm = FSM_Module, pdu = PDU_Module} = Handler}) -> 
    process_flag(trap_exit, true),
    ?debug("HANDLER: ~p",[Handler], start_link),
    ?assert(FSM_Module =/= undefined),
    case ?session:start_link(Socket, Handler) of
	{ok, Session} ->
	    ?debug("SESSION: ~p",[Session], start_link),
	    case FSM_Module:start_link(PDU_Module) of
		{ok, FSM} -> 
		    ?debug("FSM: ~p",[FSM], start_link),
		    {ok, #state{fsm_module = FSM_Module, 
				fsm = FSM, session = Session}};
		FSM_Error -> {stop, FSM_Error}
	    end;
	Session_Error -> {stop, Session_Error}
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Handling call messages. Callback function that will be invoked by gen_server.
%% @end
-spec(handle_call/3 :: 
      (status, From :: pid(), #state{}) -> {reply, Reply :: term(), #state{}};
      (stop, From :: pid(), #state{}) -> {stop, normal, #state{}};
      (Request :: term(), From :: pid(), #state{}) -> 
	     {reply, {error, {unhandled_call, Request :: term(), From :: pid()}},
	      #state{}};
      (M :: term(), From :: pid(), #state{}) -> 
	     {reply, Reply :: term(), #state{}};
      (M :: term(), From :: pid(), #state{}) -> 
	     {reply, Reply :: term(), #state{}, Timeout :: integer()};
      (M :: term(), From :: pid(), #state{}) -> {noreply, #state{}};
      (M :: term(), From :: pid(), #state{}) -> 
	     {noreply, #state{}, Timeout :: integer()};
      (M :: term(), From :: pid(), #state{}) -> 
	     {stop, Reason :: term(), Reply :: term(), #state{}};
      (M :: term(), From :: pid(), #state{}) -> 
	     {stop, Reason :: term(), #state{}}).
%%--------------------------------------------------------------------

handle_call(status, _From, State) ->
    KVs = ?record_to_keyval(state, State),
    Reply={ok, self(), KVs},
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    {stop, normal, State};

handle_call(Request, From, State) ->
    ?debug("Got unhandled call ~p from ~p.", [Request, From], handle_call),
    {reply, {error, {unhandled_call, Request, From}}, State}.


%%--------------------------------------------------------------------
%% @doc
%%  Handling cast messages. Callback function that will be invoked by gen_server.
%% @end
-spec(handle_cast/2 :: 
      (status, #state{}) -> {reply, Reply :: term(), #state{}};
      (Request :: term(), #state{}) -> 
	     {reply, {error, {unhandled_call, Request :: term(), From :: pid()}},
	      #state{}};
      (M :: term(), #state{}) -> {noreply, #state{}};
      (M :: term(), #state{}) -> {noreply, #state{}, Timeout :: integer()};
      (M :: term(), #state{}) -> {noreply, #state{}, hibernate};
      (M :: term(), #state{}) -> {stop, Reason :: term(), #state{}}).
%%--------------------------------------------------------------------

handle_cast(Message, State) ->
    ?debug("Got unhandled info message ~p.", [Message], handle_info),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%%     Handling all non call/cast messages. 
%%     Callback function that will be invoked by gen_server.
%% @end
%%--------------------------------------------------------------------
-spec(handle_info/2 :: 
      ({'EXIT', Pid :: pid(), normal}, State :: #state{}) -> {noreply, #state{}};
      (M :: term(), #state{}) -> {noreply, #state{}};
      (M :: term(), #state{}) -> {noreply, #state{}, Timeout :: integer()};
      (M :: term(), #state{}) -> {noreply, #state{}, hibernate};
      (M :: term(), #state{}) -> {stop, Reason :: term(), #state{}}).

%% Take care of terminated children.
handle_info({'EXIT', _Pid, _Reason}, 
	    #state{fsm = FSM, session = Session} = State) ->
    shutdown(Session, brutal_kill),
    shutdown(FSM, brutal_kill),
    %% Send message to mgr???
    {stop, shutdown, State};

handle_info(Message, State) ->
    ?debug("Got unhandled info message ~p.", [Message], handle_info),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%%    This function is called by a gen_server when it is about to
%%    terminate. It should be the opposite of Module:init/1 and do any necessary
%%    cleaning up. When it returns, the gen_server terminates with Reason.
%%    The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec(terminate/2 :: (Reason :: term(), #state{}) -> ok).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc 
%%    Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec(code_change/3 :: (OldVsn :: term(), #state{}, Extra :: term()) -> 
	     {ok, #state{}}).
code_change(_OldVsn, State, _Extra) -> %% FIXME
    {ok, State}.

%% parent(Parent) ->
%%     case process_info(Parent, registered_name) of
%% 	{registered_name,Name} -> Name;
%% 	undefined -> Parent
%%     end.

-define(match_spec(Name, KeyVal),
	lists:to_record(Name, KeyVal, record_info(fields, Name), '_')).

%%% copy/paste from otp supervisor
%%-----------------------------------------------------------------
%% Shutdowns a child. We must check the EXIT value 
%% of the child, because it might have died with another reason than
%% the wanted. In that case we want to report the error. We put a 
%% monitor on the child an check for the 'DOWN' message instead of 
%% checking for the 'EXIT' message, because if we check the 'EXIT' 
%% message a "naughty" child, who does unlink(Sup), could hang the 
%% supervisor. 
%% Returns: ok | {error, OtherReason}  (this should be reported)
%%-----------------------------------------------------------------
shutdown(Pid, brutal_kill) ->
    case monitor_child(Pid) of
	ok ->
	    exit(Pid, kill),
	    receive
		{'DOWN', _MRef, process, Pid, killed} ->
		    ok;
		{'DOWN', _MRef, process, Pid, OtherReason} ->
		    {error, OtherReason}
	    end;
	{error, Reason} ->      
	    {error, Reason}
    end.

%% Help function to shutdown/2 switches from link to monitor approach
monitor_child(Pid) ->
    %% Do the monitor operation first so that if the child dies 
    %% before the monitoring is done causing a 'DOWN'-message with
    %% reason noproc, we will get the real reason in the 'EXIT'-message
    %% unless a naughty child has already done unlink...
    erlang:monitor(process, Pid),
    unlink(Pid),

    receive
	%% If the child dies before the unlik we must empty
	%% the mail-box of the 'EXIT'-message and the 'DOWN'-message.
	{'EXIT', Pid, Reason} -> 
	    receive 
		{'DOWN', _, process, Pid, _} ->
		    {error, Reason}
	    end
    after 0 -> 
	    %% If a naughty child did unlink and the child dies before
	    %% monitor the result will be that shutdown/2 receives a 
	    %% 'DOWN'-message with reason noproc.
	    %% If the child should die after the unlink there
	    %% will be a 'DOWN'-message with a correct reason
	    %% that will be handled in shutdown/2. 
	    ok   
    end.

test(_) ->
    unknown.
