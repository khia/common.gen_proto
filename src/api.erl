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

-module(common.gen_proto.api).

-include("debug.hrl").
-include("utils.hrl").
-include("gen_proto.hrl").
-include("internal.hrl").

-export([
	 behaviour_info/1
	 ]).

%% API
-export([
	 add_handlers/1, 
	 send/2, 
	 info/2, 
	 session_stop/1,
	 sessions/1, 
	 id/1, 
	 id_matchspec/1, 
	 local_id/1, 
	 remote_id/1
	]).

-spec(behaviour_info/1 :: (atom()) -> 'undefined' | [{atom(), byte()}]).

behaviour_info(callbacks) ->
    [
     {init,1},
     {handle_call,3},
     {handle_cast,2},
     {handle_info,2},
     {terminate,2},
     {code_change,3}
    ];
behaviour_info(_Other) ->
    undefined.

%% API dispatch
add_handlers([]) -> ok;
add_handlers([#session{} = H | Rest]) -> 
    case add_handler(H) of
	{ok, _Pid} -> 
	    ?debug("Port handler was successfully added to supervisor.",
		   [], add_handlers),
	    ok;
	Error ->
	    ?debug("Port handler cannot be added to supervisor, error ~p.",
		   [Error], add_handlers),
	    skip
    end,
    add_handlers(Rest);
add_handlers([H | Rest]) ->
    ?debug("Wrong handler specification ~p, will be skipped", [H], add_handlers),
    add_handlers(Rest).    

add_handler(#session{id = undefined} = Handler) ->
    Id = id(Handler),
    add_handler(Handler#session{id = Id});
add_handler(#session{socket = #socket{mode = listener}} = Handler) ->
    sup:add_listener(Handler);
add_handler(#session{socket = #socket{mode = connector}} = Handler) ->
    sup:add_connector(Handler).

send(Pid, Packet) -> 
    session:send(Pid, Packet).

id(#session{socket = #socket{mode = listener, 
			     local_ip = Address, local_port = Port}}) ->
    {Address, Port};
id(#session{socket = #socket{mode = connector,
			     remote_ip = Address, remote_port = Port}})->
    {Address, Port}.

id_matchspec(#session{id = Id, socket = #socket{mode = listener}}) ->
    transport:session_matchspec_local_id(Id);
id_matchspec(#session{id = Id, socket = #socket{mode = connector}}) ->
    transport:session_matchspec_remote_id(Id).    

local_id(Id) -> transport:session_local_id(Id). 
remote_id(Id) -> transport:session_remote_id(Id).

info(Id, Info) -> mgr:info(Id, Info).
    
session_stop(Id) -> mgr:session_stop(Id).

sessions(Query) -> mgr:sessions(Query).
    
