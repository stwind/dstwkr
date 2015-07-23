-module(dstwkr_registry).

-include("dstwkr.hrl").

-export([init/0]).
-export([reg/4]).
-export([unreg/1]).
-export([find/1]).
-export([find_all/1]).

-define(TBL, dstwkr_workers).

%% =============================================================================
%% Public
%% =============================================================================

init() ->
    ets:new(?TBL,[public,named_table,
                  {write_concurrency,true},{read_concurrency,true}]),
    log_init(),
    ok.

reg(Partition, Id, Mod, Pid) ->
    ets:insert(?TBL,{Id, Partition, Mod, Pid}),
    log_reg(Partition, Id, Pid),
    ok.

unreg(Id) ->
    ets:delete(?TBL, Id),
    log_unreg(Id),
    ok.

find(Id) ->
    case ets:lookup(?TBL, Id) of
        [{Id, _, _, Pid}] ->
            log_lookup(Id, Pid),
            {ok, Pid};
        [] ->
            {error, not_found}
    end.

find_all(Partition) ->
    Results = ets:match(?TBL, {'$1', Partition, '$2', '$3'}),
    [{Id, {Mod, Pid}} || [Id, Mod, Pid] <- Results].

%% =============================================================================
%% Private
%% =============================================================================

log_init() ->
    Meta = [{part, registry}, {action, init}],
    lager:debug(Meta, "[registry] init", []).

log_reg(Partition, Id, Pid) ->
    Meta = [{part, registry}, {action, reg}, 
            {worker_pid, Pid}, {worker_id, Id}, {partition, Partition}],
    lager:debug(Meta, "[registry] registered ~p as ~p", [Pid, Id]).

log_unreg(Id) ->
    Meta = [{part, registry}, {action, unreg}, {worker_id, Id}],
    lager:debug(Meta, "[registry] unregistered ~p", [Id]).

log_lookup(Id, Pid) ->
    Meta = [{part, registry}, {action, lookup}, {worker_id, Id}, {worker_pid, Pid}],
    lager:debug(Meta, "[registry] lookup ~p => ~p", [Id, Pid]).
