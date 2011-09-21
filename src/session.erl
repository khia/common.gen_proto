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

-module(common.gen_proto.session).

-define(SESSION_TIMEOUT, 30000). %% 30 sec

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

%%--------------------------------------------------------------------
%% API

%% Public API
-export([check_opts/1, send/2, connect/1]).
%% Server Control
-export([stop/1, start_link/2, start/4, status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2,
	 terminate/2, code_change/3]).

%% Test functions
-export([test/1]).

-record(state, {fsm, fsm_pid, pdu, data = <<>>, id, socket, transport, sup,
	       timeout}).

%% @doc 
%%    Starts server
%% @end
-spec(start/4 :: (Transport :: atom(), Socket :: transport:socket(), 
%%		  Id :: term(), Opts :: proplist()) -> 
		  Id :: term(), Opts :: term()) -> 
	     {ok, Pid :: pid()} | {error, Reason :: term()}).
start(Transport, Socket, Id, Opts)->
    gen_server:start(?MODULE, [Transport, Socket, Id, Opts], []).


%% @doc
%%     Stops dli feedback collector server.
%% @end
-spec(stop/1 :: (Pid :: pid()) -> ok).
stop(Pid) ->
    (catch gen_server:call(Pid, stop)),
    ok.

send(Pid, Packet) -> gen_server:cast(Pid, {send, Packet}).

%% @doc
%%    Returns the state of server.
%% @end
-spec(status/1 :: (Pid :: atom() | pid()) -> 
	     {ok, Pid, #state{}} | {ok, not_running, Pid} 
		 | {error, Reason :: term()}). 
status(Pid)->
    try gen_server:call(Pid, status) of
	Result->Result
	catch
 	  _:_Reason -> {ok, not_running, Pid}
	end.

%%--------------------------------------------------------------------
%% @doc
%%    Starts server.
%% @end
-spec(start_link/2 :: (Socket :: transport:socket(), 
		  Handler :: #session{}) -> 
	     {ok, Pid :: pid()} | {error, Reason :: term()}).
start_link(Socket, Handler) ->
    gen_server:start_link(?MODULE, {Socket, Handler}, []).

connect(#ext{session = Pid} = Ext) ->
    gen_server:call(Pid, {connect, Ext}).

%%==================================================
%% gen_server callbacks
%%==================================================

%%--------------------------------------------------------------------
%% @doc
%%    Initiates the server. Callback function that will be invoked by gen_server.
%% @end
%%--------------------------------------------------------------------
-type(reason() :: term()).
-spec(init/1 :: (Args :: [term()]) -> {ok, #state{}} 
					  | {ok, #state{}, Timeout :: integer()}
					  | {ok, #state{}, hibernate}
					  | {stop, reason()} 
					  | ignore).
init({Socket, #session{fsm = FSM_Module, pdu = PDU, id = Id, opts = Options,
		       socket = #socket{transport = Transport}} = Session}) ->
    ?assert(check_opts(Session)), %% FIX ME how to do it only once
    Timeout = proplists:get_value(timeout, Options, ?SESSION_TIMEOUT),  
    {ok, #state{socket = Socket, id = Id, fsm = FSM_Module, 
		pdu = PDU, transport = Transport, timeout = Timeout}}.

%%--------------------------------------------------------------------
%% @doc
%%     Handling call messages. Callback function that will be invoked by gen_server.
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
	     {reply, Reply :: term(), #state{},Timeout :: integer()};
      (M :: term(), From :: pid(), #state{}) -> {noreply, #state{}};
      (M :: term(), From :: pid(), #state{}) -> 
	     {noreply, #state{}, Timeout :: integer()};
      (M :: term(), From :: pid(), #state{}) -> 
	     {stop, Reason :: term(), Reply :: term(), #state{}};
      (M :: term(), From :: pid(), #state{}) -> 
	     {stop, Reason :: term(), #state{}}).
%%--------------------------------------------------------------------
handle_call({connect, #ext{sup = Sup, fsm = FSM}}, _From, 
	    #state{fsm = FSM_Module, pdu = PDU, id = Id, socket = Socket, data = Data} = State) ->
    Reply = FSM_Module:session_event(FSM, PDU, {ctrl, {start, Id, self()}}),
    self() ! {data, Socket, Data},
    {reply, Reply, State#state{fsm_pid = FSM, sup = Sup}};
handle_call(status, _From, State) ->
    KVs = ?record_to_keyval(state, State),
    Reply={ok, self(), KVs},
    {reply, Reply, State};
handle_call(stop, _From, State)->
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
handle_cast({send, Packet}, 
	    #state{socket = Socket, 
		   transport = Transport, timeout = Timeout} = State) ->
    %% TODO Add errors handling
    transport:send(Transport, Socket, Packet),
    {noreply, State, Timeout};
handle_cast(Message, State) ->
    ?debug("Got unhandled info message ~p.", [Message], handle_cast),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%%     Handling all non call/cast messages. 
%%     Callback function that will be invoked by gen_server.
%% @end
%%--------------------------------------------------------------------
-spec(handle_info/2 :: 
      (M :: term(), #state{}) -> {noreply, #state{}};
      (M :: term(), #state{}) -> {noreply, #state{}, Timeout :: integer()};
      (M :: term(), #state{}) -> {noreply, #state{}, hibernate};
      (M :: term(), #state{}) -> {stop, Reason :: term(), #state{}}).

%% TODO find another way to route tcp 
handle_info({tcp, Socket, Data}, #state{id = _Id, fsm_pid = undefined, data = Bin} = State) ->
    Packet = <<Bin/binary, Data/binary>>,
    {noreply, State#state{data = Packet}};
handle_info({tcp, Socket, Data}, #state{id = _Id} = State) ->
    handle_info({data, Socket, Data}, State);
handle_info({tcp_closed, Socket}, State) ->
    handle_info({session_closed, Socket}, State);
handle_info({tcp_error, Socket, Reason}, State) ->
    handle_info({session_error, Socket, Reason}, State);

handle_info({data, _Socket, <<>>}, State) ->
    {noreply, State};

handle_info({data, _Socket, Data}, 
	    #state{fsm = FSM_Module, 
		   fsm_pid = FSM,
		   pdu = PDU,
		   data = Bin, id = _Id} = State ) ->
    Packet = <<Bin/binary, Data/binary>>,
    New_State = case PDU:match_packet(Packet) of
		    %% incomplete packet, we need to wait next segment
		    incomplete -> 
			?debug("Incomplete packet waiting next segment.",[],
			       handle_info),
			State#state{data = Packet};
		    false -> 
			?debug("Cannot match_packet, so skip.",[], handle_info),
			State;
		    true -> 
			FSM_Module:session_event(FSM, PDU, Packet),
			State#state{data = <<>>}
		end,
    {noreply, New_State};

handle_info({session_closed, Socket},
	    #state{fsm = FSM_Module, fsm_pid = FSM,
		   pdu = PDU, id = Id} = State ) ->
    ?debug("~p: Session was closed: ~p.", [now(), Socket], handle_info),
    FSM_Module:session_event(FSM, PDU, {ctrl, {closed, Id, self()}}),
    {stop, normal, State};
handle_info({session_error, Socket, Reason}, 
	    #state{fsm = FSM_Module, fsm_pid = FSM,
		   pdu = PDU, id = Id} = State ) -> 
    ?debug("Session get an error: ~p ~p.", [Socket, Reason], handle_info),
    FSM_Module:session_event(FSM, PDU, {ctrl, {error, Id, self(), Reason}}),
    {noreply, State};

%% workaround to detect session termination because tcp_closed doesn't received
handle_info(timeout, #state{fsm = FSM_Module, fsm_pid = FSM,
		   pdu = PDU, id = Id} = State ) -> 
    FSM_Module:session_event(FSM, PDU, {ctrl, {error, Id, self(), timeout}}),
    {stop, normal, State};
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
-spec(terminate/2 :: (Reason :: term(), #state{}) -> normal).
terminate(Reason, #state{socket = Socket, transport = Transport} = _State) ->
    ?debug("session was terminated ~p", [Reason], terminate),
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

test(_) ->
    unknown.

check_opts(#session{fsm = FSM, pdu = PDU}) 
  when undefined =/= FSM, undefined =/= PDU -> true;
check_opts(_) -> false.
