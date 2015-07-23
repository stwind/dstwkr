-module(dstwkr_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(VERBOSE, true).

-define(LOCALHOST, '127.0.0.1').

start_nodes(Num) ->
    _ = net_kernel:start(['s3_test0@127.0.0.1',longnames]),
    [start_node(N) || N <- lists:seq(1, Num)].

stop_nodes(Nodes) ->
    [stop_node(Node) || Node <- Nodes],
    {ok, Cwd} = file:get_cwd(),
    DataDir = filename:join([Cwd,"data"]),
    os:cmd("rm -rf " ++ DataDir),
    ok.

start_apps(N) ->
    application:load(lager),
    application:load(sasl),
    application:load(riak_core),
    application:set_env(sasl, sasl_error_logger, false),
    maybe_verbose(),
    application:set_env(lager, colored, true),
    application:set_env(riak_core, cluster_name, "dstwkr_ct"),
    DataDir = "data/" ++ integer_to_list(N),
    application:set_env(riak_core, platform_data_dir, DataDir),
    RingDir = DataDir ++ "/ring",
    application:set_env(riak_core, ring_state_dir, RingDir),
    application:set_env(riak_core, vnode_management_timer, 1000),
    application:set_env(riak_core, broadcast_exchange_timer, 1000),
    application:set_env(riak_core, gossip_limit, {5, 1000}),
    application:set_env(riak_core, ring_creation_size, 4),
    application:set_env(riak_core, handoff_port, 9310 + N),
    {ok, _} = application:ensure_all_started(dstwkr),
    ok.

start_node(N) ->
    Args = "-env ERL_LIBS ../deps -pa ../ebin",
    {ok, Node} = slave:start(?LOCALHOST, nodname(N), Args),
    ok = rpc:call(Node, ?MODULE, start_apps, [N]),
    ?debugFmt("node ~p started", [Node]),
    Node.

stop_node(Node) ->
    ok = slave:stop(Node),
    ?debugFmt("node ~p stopped", [Node]).

nodname(N) ->
    list_to_atom("s3_test" ++ integer_to_list(N)).

join(From, To) ->
    ok = rpc:call(From, riak_core, staged_join, [To]),
    timer:sleep(500),
    {ok, _, _} = rpc:call(From, riak_core_claimant, plan, []),
    ok = rpc:call(From, riak_core_claimant, commit, []),
    timer:sleep(1500).

maybe_verbose() ->
    case ?VERBOSE of
        true ->
            application:set_env(lager, handlers, [debug_handler()]);
        _ ->
            application:set_env(lager, handlers, [])
    end.

debug_handler() ->
    {lager_console_backend, 
     [debug, 
      {stout, [{time, yellow}," ",
               {severity, [upper, color, {format, "~s"}]}," ",
               {node, [{format, "~s "},magentab]},
               {application, [{format, "(~s) "}, blueb]},
               message, " ",
               {pid, cyan}," ",
               {module,[{format,"~s"},blackb]},
               {line,[{format,":~b"},blackb]},
               "\n"]}]}.
