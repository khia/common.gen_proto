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

-module(common.gen_proto.mgr).
-behaviour(gen_server).
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
-import(common.utils.lists).
-import(gen_server).
-import(ets).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
%% Public API
-export([session_start/2, sessions/1, session_stop/1, sessions/2, info/2,
	change_status/2, session_register/1]).

%% Server Control
-export([stop/0, stop/1, start_link/0, start_link/1, status/0, status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2,
	 terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
%% Test functions
-export([test/1]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {sessions :: est:tid()}).
%%====================================================================
%% External functions
%%====================================================================
%% @hidden
session_start(Socket, #session{id = _Id} = Handler0) -> 
    case mgr_sup:add_session(Socket, Handler0) of
	{ok, Sup, {Session, FSM}} ->
	    Handler1 = Handler0#session{
			 ext = #ext{sup = Sup, fsm = FSM, session = Session}},
	    change_status(Handler1, active),
	    {ok, Handler1};
	Error -> 
	    ?debug("Cannot register session ~p",[Error], session_start),
	    Error
    end.

session_stop(Id) -> 
    session_unregister(Id),
    mgr_sup:terminate_session(Id).  

sessions(Query) -> sessions(?MODULE, Query).
sessions(Pid, Query) -> gen_server:call(Pid, {sessions, Query}).

info(Id, Info) -> info(?MODULE, Id, Info).
info(Pid, Id, Info) -> gen_server:call(Pid, {info, Id, Info}).

session_register(Session) -> session_register(?MODULE, Session).
session_register(Pid, Session) ->
    ?debug("About to register ~p",[Session],session_register),
    gen_server:call(Pid, {register, Session}).
session_unregister(Session) -> session_unregister(?MODULE, Session).
session_unregister(Pid, Session) -> gen_server:call(Pid, {unregister, Session}).

change_status(Session, Status) -> change_status(?MODULE, Session, Status).
change_status(Pid, Session, Status) ->
    gen_server:call(Pid, {change_status, Session, Status}).

%%--------------------------------------------------------------------
%% @doc
%%    Starts server.
%% @end
-spec(start_link/0 :: () -> {ok, Pid :: pid()} | {error, Reason :: term()}).
start_link() -> start_link(?MODULE).
-spec(start_link/1 :: (Name :: atom()) -> 
	     {ok, Pid :: pid()} | {error, Reason :: term()}).
start_link(Name) -> gen_server:start_link({local, Name}, ?MODULE, {}, []).

%% @doc
%%     Stops server.
%% @end
stop() -> stop(?MODULE).
-spec(stop/1 :: (Pid :: pid()) -> ok).
stop(Pid) ->
    (catch gen_server:call(Pid, stop)),
    ok.

%% @doc
%%    Returns the state of server.
%% @end
status() -> status(?MODULE).
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
-type(reason() :: term()).
-spec(init/1 :: (Args :: [term()]) -> {ok, #state{}} 
					  | {ok, #state{}, Timeout :: integer()}
					  | {ok, #state{}, hibernate}
					  | {stop, reason()} 
					  | ignore).
%%                               {reuseaddr, true},
init({}) -> 
    ?debug("Started",[],init),
    %% TO THINK HOW WE CAN MOVE ETS TO SUPERVISOR TO AVOID LOST WHEN DIE
    {ok, #state{sessions = ets:new(sessions, [{keypos, 2}, private, set])}}.

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
handle_call({sessions, Query}, _From, State) ->
    Reply = handle_sessions(Query, State),
    {reply, Reply, State};
handle_call({register, Session}, _From, State) ->
    Reply = handle_register(Session, State),
    {reply, Reply, State};
handle_call({unregister, Session}, _From, State) ->
    Reply = handle_unregister(Session, State),
    {reply, Reply, State};
handle_call({change_status, Session, Status}, _From, State) ->
    Reply = handle_change_status(Session, Status, State),
    {reply, Reply, State};
handle_call({info, Id, Info}, _From, State) ->
    Reply = handle_info(Id, Info, State),
    {reply, Reply, State};
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
terminate(_Reason, #state{}) -> ok.

%%--------------------------------------------------------------------
%% @doc 
%%    Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec(code_change/3 :: (OldVsn :: term(), #state{}, Extra :: term()) -> 
	     {ok, #state{}}).
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
-define(match_spec(Name, KeyVal),
	lists:to_record(Name, KeyVal, record_info(fields, Name), '_')).
handle_sessions(Query, #state{sessions = Sessions} = _State) ->
    Match_Spec = ?match_spec(session, Query),
    {ok, ets:match_object(Sessions, Match_Spec)}.

handle_session_termination(Session_Sup, #state{sessions = Sessions} = _State) ->
    Ext = ?match_spec(ext, [{sup, Session_Sup}]),
    Match_Spec = ?match_spec(session, [{ext, Ext}]),
    Id = case ets:match_object(Sessions, Match_Spec) of
	     [] -> unknown;
	     [Session] -> 
		 ets:insert(Sessions, Session#session{status = stopped}),
		 client:connect(),
		 Session#session.id
	 end,
    {ok, Id}.    

handle_register(Session0, #state{sessions = Sessions} = _State) ->
    Id = api:id(Session0),
    ets:insert(Sessions, clear_ro_fields(Session0)).

handle_unregister(Session, #state{sessions = Sessions} = _State) ->
    ets:delete(Sessions, Session).

handle_change_status(#session{} = Session0, 
		     active, #state{sessions = Sessions} = State) ->
    Id = api:id(Session0),
    case handle_sessions([{id, Id}], State) of
	{ok, [#session{stat = Statistic} = Session1]} -> 
	    ets:delete(Sessions, Session1),
	    Session2 = update_statistic(Session0#session{id = Id, 
							 status = active},
					Statistic),
	    ets:insert(Sessions, Session2);
	{ok, []} -> 
	    ets:delete(Sessions, Session0),
	    ets:insert(Sessions, Session0#session{id = Id, status = active})
    end;
handle_change_status(#session{id = Id0} = Session0, connecting, 
		     #state{sessions = Sessions} = State) ->
    Id = api:id(Session0),
    case handle_sessions([{id, Id}], State) of
	{ok, [Session1]} -> 
	    ets:delete(Sessions, Session1),
	    ets:insert(Sessions, Session1#session{status = connecting});
	{ok, []} -> {error, {unknown_session, Id0}}
    end.

handle_info(Id, Info, State) ->
    case handle_sessions([{id, Id}], State) of
	{ok, [Session]} -> get_info(Session, Info, State);
	{ok, []} -> {error, {unknown_session, Id}}
    end.

get_info(#session{ext = #ext{session = Session}}, session, _State) ->
    {ok, Session};
get_info(#session{ext = #ext{session = Session}}, session_status, _State) ->
    session:status(Session);
get_info(#session{ext = #ext{fsm = FSM}}, handler, _State) -> {ok, FSM};
get_info(#session{fsm = Module, ext = #ext{fsm = FSM}}, handler_status,_State) ->
    Module:status(FSM);
get_info(_Session, Info, _State) -> {error, {unsupported, Info}}.
    
%% TODO make it more generic
clear_ro_fields(#session{} = Session) -> Session#session{status = unknown}.

update_statistic(#session{} = Session, Statistic) -> 
    Session#session{stat = Statistic}. %% FIXME

%%====================================================================
%% Test functions
%%====================================================================
test(_) -> todo.
