-module(main_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 60).

-define(with_nodes(N, Cases), 
        {setup, 
         fun() -> dstwkr_test:start_nodes(N) end, 
         fun dstwkr_test:stop_nodes/1, 
         fun(Nodes) -> 
                 {inorder, 
                  [{timeout, ?TIMEOUT, ?_test(?MODULE:Case(Nodes))} 
                   || Case <- Cases]}
         end}).

%% =============================================================================
%% EUnit
%% =============================================================================

main_test_() ->
    {setup, fun setup/0, fun cleanup/1, 
     {inorder, 
      [
       % {"single node", 
       %  ?with_nodes(1, [
       %                  simple_worker,
       %                  two_vnode
       %                 ])},
       {"two nodes", 
        ?with_nodes(2, [
                        % location_change,
                        worker_transfer
                       ])}
      ]}}.

setup() ->
    ok.

cleanup(_) ->
    ok.

%% =============================================================================
%% Test cases
%% =============================================================================

simple_worker([N]) ->
    Id = 324234,
    {ok, _} = rpc:call(N, dstwkr, start_worker, [Id, worker_ping]),
    pong = rpc:call(N, dstwkr, command, [Id, ping]),
    ok = rpc:call(N, dstwkr, stop_worker, [Id]),
    {error, not_found} = rpc:call(N, dstwkr, command, [Id, ping]).

two_vnode([N]) ->
    {Id1, Id2} = {12, 821},
    {ok, _} = rpc:call(N, dstwkr, start_worker, [Id1, worker_ping]),
    {ok, _} = rpc:call(N, dstwkr, start_worker, [Id2, worker_ping]),
    {_, [{Par1, _}]} = rpc:call(N, dstwkr_util, get_location, [Id1]),
    {_, [{Par2, _}]} = rpc:call(N, dstwkr_util, get_location, [Id2]),
    ?assert(Par1 /= Par2).

location_change([N1, N2]) ->
    {Id1, Id2} = {123, 123722},
    {_, [{_, N1}]} = rpc:call(N1, dstwkr_util, get_location, [Id1]),
    {_, [{_, N1}]} = rpc:call(N1, dstwkr_util, get_location, [Id2]),
    dstwkr_test:join(N2, N1),
    {_, [{_, N1}]} = rpc:call(N1, dstwkr_util, get_location, [Id1]),
    {_, [{_, N2}]} = rpc:call(N1, dstwkr_util, get_location, [Id2]),
    ok.

worker_transfer([N1, N2]) ->
    Id = 123722,
    {ok, _} = rpc:call(N1, dstwkr, start_worker, [Id, worker_ping]),
    {ok, Pid1, _} = rpc:call(N1, dstwkr, get_state, [Id]),
    ?assertMatch(N1, node(Pid1)),
    dstwkr_test:join(N2, N1),
    {ok, Pid2, _} = rpc:call(N1, dstwkr, get_state, [Id]),
    ?assertMatch(N2, node(Pid2)).

%% =============================================================================
%% Private
%% =============================================================================
