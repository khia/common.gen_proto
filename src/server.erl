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

-module(common.gen_proto.server).

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
-import(gen_server).

%%--------------------------------------------------------------------
%% API

%% Public API
-export([create/2]).

%% Server Control
-export([stop/1, start_link/1, status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2,
	 terminate/2, code_change/3]).


%% Test functions
-export([test/1]).


-record(state, {listen_socket, acceptor, session, transport}).

%% Send message to cause a new acceptor to be created
create(ServerPid, Pid) ->
    gen_server:cast(ServerPid, {create, Pid}).

%%--------------------------------------------------------------------
%% @doc
%%    Starts server.
%% @end
-spec(start_link/1 :: (Handler :: #session{}) -> 
	     {ok, Pid :: pid()} | {error, Reason :: term()}).
start_link(Handler) ->
    gen_server:start_link(?MODULE, Handler, []).

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
status(Pid)->
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
-spec(init/1 :: (Args :: [term()]) -> {ok, #state{}} 
					  | {ok, #state{}, Timeout :: integer()}
					  | {ok, #state{}, hibernate}
					  | {stop, reason()} 
					  | ignore).
%%                               {reuseaddr, true},
init(#session{socket = #socket{
		local_ip = Address,
		local_port = Port,
		transport = Transport,
		opts = Socket_Opts0}, opts = _Session_Opts} = Handler) -> 
    ?debug("About to init, args ~p.",[Handler], init),
    Fixed = transport:fix_opts(Handler),
    mgr:session_register(Fixed),
    ?assert(transport:check_opts(Fixed)),
    ?assert(session:check_opts(Fixed)),
    Socket_Opts1 = [{reuseaddr, true}|Socket_Opts0],
    Transport_Opts = [{address, Address}, {port, Port}, {opts, Socket_Opts1}],
    case transport:listen(Transport, Transport_Opts) of
	{ok, Socket} -> 
	    Pid = accept:spawn_link(Socket, self(), Fixed),
	    State = #state{listen_socket = Socket,
			   acceptor = Pid, 
			   transport = Transport,
			   session = Fixed},
	    ?debug("~p state = ~p.", [self(), State], init),
	    {ok, State};
	{error, Reason} -> {error, {listen, Reason}}
    end.

%%    gen_tcp:accept(ListenSocket, Timeout);

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
handle_call(stop, _From,
	    State)->
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

%% Called by gen_server framework when the cast message from create/2 is received
handle_cast({create, _Pid}, #state{listen_socket = Socket, 
				   session = Handler} = State) ->
    New_pid = accept:spawn_link(Socket, self(), Handler),
    {noreply, State#state{acceptor=New_pid}};

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

handle_info({'EXIT', Pid, normal}, #state{acceptor=Pid} = State) ->
    {noreply, State};

%% The current acceptor has died, wait a little and try again
handle_info({'EXIT', Pid, _Abnormal}, 
	    #state{acceptor=Pid, listen_socket = Socket, 
		   session = Handler} = State) ->
    timer:sleep(2000),
    New_pid = accept:spawn_link(Socket, self(), Handler),
    {noreply, State#state{acceptor=New_pid}};

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
terminate(_Reason, #state{transport = Transport, listen_socket = Socket,
			  acceptor = Acceptor}) ->
    accept:stop(Acceptor),
    case transport:close(Transport, Socket) of 
	ok -> normal;
	{error, Reason} -> 
	    ?debug("Cannot close socket (~p) trying to shutdown it.", 
		   [Socket], handle_call),
	    transport:shutdown(Transport, Socket, read_write),
	    {error, {close_socket, Reason}}
    end,
    ok.

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
