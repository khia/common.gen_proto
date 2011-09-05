-record(ext, {sup, fsm, session}).

%%--------------------------------------------------------------------
%% Macros for referencing internal modules
%%--------------------------------------------------------------------
-define(api, common.gen_proto.api).
-define(accept, common.gen_proto.accept).
-define(sup, common.gen_proto.sup).
-define(client, common.gen_proto.client).
-define(server, common.gen_proto.server).
-define(session, common.gen_proto.session).
-define(transport, common.gen_proto.transport).
-define(mgr, common.gen_proto.mgr).
-define(mgr_sup, common.gen_proto.mgr_sup).
