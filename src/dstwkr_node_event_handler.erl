-module(dstwkr_node_event_handler).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%% =============================================================================
%% Public
%% =============================================================================

init([]) ->
    {ok, #state{}}.

handle_event({service_update, [dstwkr]}, State) ->
    maybe_remove_down_nodes(),
    {ok, State};

handle_event(_, State) ->
    {ok, State}.

handle_call(_Event, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Private
%% =============================================================================

maybe_remove_down_nodes() ->
    UpNodes = riak_core_node_watcher:nodes(dstwkr),
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    AllMembers = riak_core_ring:all_members(Ring),
    remove_down_nodes(AllMembers, UpNodes).

is_up(Node, UpNodes) ->
    lists:member(Node, UpNodes).

remove_down_nodes([], _UpNodes) ->
    ok;
remove_down_nodes([Node | Rest], UpNodes) ->
    case is_up(Node, UpNodes) of
        true ->
            ok;
        false ->
            lager:warning("removing down node ~p", [Node]),
            riak_core:remove(Node)
    end,
    remove_down_nodes(Rest, UpNodes).
