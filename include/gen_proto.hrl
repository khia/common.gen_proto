-type(word() :: 0..65535).
-type(ip() :: 
      {byte(), byte(), byte(), byte()}
      | {word(), word(), word(), word(), word(), word(), word(), word()}).
-type(proplist() :: [atom() | {atom(), term()}]).
-record(socket, {
 	  remote_ip :: ip(),
 	  remote_port :: integer(),
	  local_ip :: ip(),	  
 	  local_port :: integer(),
 	  transport :: tcp | ssl | atom(),
 	  mode :: listener | connector,
 	  timeout :: integer(), %% connection timeout
	  opts = [] :: proplist()
 	 }).

-record(session, {
 	  id :: term(),
 	  socket :: #socket{},
 	  protocol :: atom(),
 	  fsm :: atom(),
 	  pdu :: atom(),
	  status :: unknown | active | stopped | connecting,
	  ext :: term(),
	  stat :: term(),
	  opts = [] :: proplist() 
		  %% reconnect - timeout before next attempt to connect will happens
		  %% timeout -  send timeout before session will be terminated 
	 }).
