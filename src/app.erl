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

-module(common.gen_proto.app).
-include("debug.hrl").
-behaviour(application). 

-import(proplists).
-import(application).

-export([start/2, stop/1]).

%%%----------------------------------------------------------------------
%%% Application callbacks
%%%----------------------------------------------------------------------

-spec(start/2 :: (
	      Type :: normal | {takeover, node()} | {failover, node()}, 
	      Args :: term()) -> 
	     {ok, Pid :: pid()} 
		 | {ok, Pid :: pid(), State :: term()} 
		 | {error, Reason :: term()}).
start(_Type, Args) -> 
    case sup:start_link(Args) of
	{ok, Pid} -> 
	    ?debug("Application ~p was successfully started.", [?MODULE], start),
	    {ok, Pid};
	Error -> 
	    ?debug("Cannot start application, reason ~p.", [Error], start),
	    Error
    end.

-type(ignored() :: term()).
-spec(stop/1 :: (State :: term()) -> ignored()). 
stop(_State) -> 
    ok.


