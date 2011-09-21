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

-module(common.gen_proto.client).

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
-import(lists).
-import(proplists).
-import(gen_server).
-import(timer).

-define(RECONNECT_TIMEOUT, 5000).  %% 5 sec

%%--------------------------------------------------------------------
%% API

%% Public API
-export([create/3, connect/1, error/2]).

%% Server Control
-export([stop/1, start_link/1, status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2,
	 terminate/2, code_change/3]).

%% Test functions
-export([test/1]).

-record(state, {connect_socket, session, connector, transport}).

%% Send message to cause a new connector to be created
create(ClientPid, Socket, Handler) ->
    gen_server:cast(ClientPid, {create, Socket, Handler}).

error(ClientPid, Reason) -> gen_server:cast(ClientPid, {error, Reason}).

%%--------------------------------------------------------------------
%% @doc
%%    Starts server.
%% @end
-spec(start_link/1 :: (Session :: #session{}) -> 
	     {ok, Pid :: pid()} | {error, Reason :: term()}).
start_link(Session) ->
    case gen_server:start_link(?MODULE, Session, []) of
	{ok, Pid} -> connect(Pid), {ok, Pid};
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

connect(Pid) -> gen_server:call(Pid, connect).

%%==================================================
%% gen_server callbacks
%%==================================================

%%--------------------------------------------------------------------
%% @doc
%%    Initiates the server. Callback function that will be invoked by gen_server.
%% @end
%%--------------------------------------------------------------------
-type(reason() :: term()).
-spec(init/1 :: (Session :: #session{}) -> {ok, #state{}} 
					  | {ok, #state{}, Timeout :: integer()}
					  | {ok, #state{}, hibernate}
					  | {stop, reason()} 
					  | ignore).
%%                               {reuseaddr, true},
init(#session{socket = #socket{transport = Transport}} = Handler) -> 
    process_flag(trap_exit, true),
    ?debug("About to init, session ~p.",[Handler], init),
    Fixed = transport:fix_opts(Handler),
    ?assert(transport:check_opts(Fixed)),
    ?assert(session:check_opts(Fixed)),
    {ok, #state{transport = Transport, session = Fixed}}.

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
handle_call(stop, _From, State)->
    {stop, normal, State};
handle_call(connect, _From, State) ->
    case handle_connect(State) of
	{error, _Reason} = Error -> {reply, Error, State};
	New_State -> {noreply, New_State}
    end;
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

%% Called by gen_server framework when the cast message from create/3 is received
handle_cast({create, Socket, #session{ext = #ext{sup = Sup}} = Handler}, 
	    #state{session = _Session} = State) ->
    link(Sup), %% link to receive messages about child's death
    {noreply, State#state{connect_socket = Socket, session = Handler}};
handle_cast({error, _Reason}, #state{session = #session{} = Session} = State) ->
    connect:cleanup(Session),
    schedule_next_attempt(State),
    {noreply, State};
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

handle_info({'EXIT', Pid, _Reason}, 
	    #state{session = #session{ext = #ext{sup = Pid}} = Session}
	    = State) ->
    ?debug("Got EXIT ~p(~p).", [Pid, _Reason], handle_info),
    connect:cleanup(Session),
    schedule_next_attempt(State),
    {noreply, State};
handle_info({'EXIT', Pid, _Reason}, State) -> 
    ?debug("Ignore EXIT ~p(~p).", [Pid,State], handle_info),
    {noreply, State}; %% ignore
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
terminate(_Reason, #state{transport = _Transport, connect_socket = _Socket,
			  connector = undefined}) ->
    ok;
terminate(_Reason, #state{transport = _Transport, connect_socket = undefined,
			  connector = Connector}) ->
    connect:stop(Connector),
    ok;
terminate(_Reason, #state{transport = Transport, connect_socket = Socket,
			  connector = Connector}) ->
    connect:stop(Connector),
    case transport:close(Transport, Socket) of 
	ok -> normal;
	{error, Reason} -> 
	    ?debug("Cannot close socket (~p) trying to shutdown it.", 
		   [Socket], handle_call),
	    transport:shutdown(Transport, Socket, read_write),
	    {error, {close_socket, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc 
%%    Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec(code_change/3 :: (OldVsn :: term(), #state{}, Extra :: term()) -> 
	     {ok, #state{}}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_connect(#state{session = Session} = State) ->
    case connect:start_link(self(), Session) of
	{error, Reason} -> {error, {listen, Reason}};
	Pid when is_pid(Pid)-> State#state{connector = Pid}
    end.

schedule_next_attempt(#state{session = #session{opts = Options}}) ->
    Timeout = proplists:get_value(reconnect, Options, ?RECONNECT_TIMEOUT),
    timer:apply_after(Timeout, ?MODULE, connect, [self()]).

test(_) -> unknown.
