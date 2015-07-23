-module(dstwkr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% =============================================================================
%% Public
%% =============================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = dstwkr_sup:start_link(),
    ok = riak_core:register(dstwkr, [{vnode_module, dstwkr_vnode}]),
    ok = riak_core_node_watcher:service_up(dstwkr, self()),
    ok = riak_core_ring_events:add_guarded_handler(
           dstwkr_ring_event_handler, []),
    ok = riak_core_node_watcher_events:add_guarded_handler(
           dstwkr_node_event_handler, []),
    ok = dstwkr_registry:init(),
    register_metrics(),
    {ok, Sup}.

stop(_State) ->
    ok.

%% =============================================================================
%% Private
%% =============================================================================

register_metrics() ->
    exometer:new([dstwkr, req_fsm, duration], duration),
    exometer:new([dstwkr, req_fsm, num], counter),
    exometer:new([dstwkr, req_fsm, request, duration], duration),
    exometer:new([dstwkr, worker, duration], duration),
    exometer:new([dstwkr, worker, num], counter),
    exometer:new([dstwkr, worker, transfer], counter),
    exometer:new([dstwkr, vnode, handoff, count], counter),
    exometer:new([dstwkr, vnode, handoff, forward], counter),
    exometer:new([dstwkr, vnode, handoff, duration], duration),
    ok.
