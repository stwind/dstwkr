-module(dstwkr_vnode).

-behaviour(riak_core_vnode).

-include("dstwkr.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([request/4]).
-export([vnode_status/1]).

-export([
         start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3
        ]).

-record(state, {
          node :: node(), 
          partition :: integer(),
          forward_reqs = [] :: [any()]
         }).

%% =============================================================================
%% Public
%% =============================================================================

request(Preflist, From, Id, Request) ->
    riak_core_vnode_master:command(Preflist, {worker_req, Id, Request, From}, 
                                   {fsm, undefined, self()},
                                   dstwkr_vnode_master).
vnode_status(PrefList) ->
    ReqId = erlang:phash2({self(), os:timestamp()}),
    %% Get the status of each vnode
    riak_core_vnode_master:command(PrefList,
                                   get_vnode_status,
                                   {raw, ReqId, self()},
                                   dstwkr_vnode_master),
    wait_for_vnode_status_results(PrefList, ReqId, []).

%% =============================================================================
%% riak_core_vnode
%% =============================================================================

start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state{node=node(), partition=Partition}}.

handle_command(get_vnode_status, _Sender, #state{partition = Index} = State) ->
    {reply, {vnode_status, Index, [ok]}, State};

handle_command({worker_req, Id, {create, Mod}, {ReqId, _}}, _Sender, State) ->
    Resp = start_worker(Id, Mod, State),
    {reply, {response, ReqId, Resp}, State};

handle_command({worker_req, Id, stop, From}, _Sender, State) ->
    _ = dstwkr_worker:stop(Id, From),
    {noreply, State};

handle_command({worker_req, Id, get_state, From}, _Sender, State) ->
    _ = dstwkr_worker:get_state(Id, From),
    {noreply, State};

handle_command({worker_req, Id, {command, Cmd}, From}, _Sender, State) ->
    dstwkr_worker:command(Id, From, Cmd),
    {noreply, State};

handle_command(Message, _Sender, State) ->
    lager:debug("unhandled_command ~p", [Message]),
    {noreply, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

handoff_starting({_HOType, TargetNode}, State) ->
    exometer:update([dstwkr, vnode, handoff,  duration], timer_start),
    exometer:update([dstwkr, vnode, handoff, count], 1),
    log(State, "handing off to ~p", [TargetNode]),
    {true, State}.

handoff_cancelled(State) ->
    log(State, "handoff canceled"),
    {ok, State}.

handoff_finished(TargetNode, State) ->
    exometer:update([dstwkr, vnode, handoff,  duration], timer_end),
    log(State, "handoff finished ~p", [TargetNode]),
    {ok, send_forward_reqs(TargetNode, State)}.

handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->
    Acc = dict:fold(Fun, Acc0, get_all_workers(State)),
    {reply, Acc, State};
handle_handoff_command(Request, _Sender, State) ->
    exometer:update([dstwkr, vnode, handoff, forward], 1),
    {noreply, save_forward_request(Request, State)}.

handle_handoff_data(Data, State) ->
    {Id, {Mod, Old}} = binary_to_term(Data),
    %% TODO: handle start error
    {ok, New} = start_worker(Id, Mod, State),
    ok = dstwkr_worker:transfer(New, Old),
    {reply, ok, State}.

encode_handoff_item(Id, Meta) ->
    term_to_binary({Id, Meta}).

is_empty(State) ->
    {false, State}.

delete(State) ->
    log(State, "deleting"),
    {ok, State}.

terminate(_Reason, State) ->
    log(State, "terminating"),
    ok.

%% =============================================================================
%% Private
%% =============================================================================

log(State, Fmt) ->
    log(State, Fmt, []).

log(#state{node=Node,partition=Partition}, Fmt, Args) ->
    lager:debug("vnode :: ~p/~p " ++ Fmt, [Node, Partition | Args]).

start_worker(Id, Mod, #state{partition = Partition}) ->
    dstwkr_worker:create(Id, Mod, Partition).

get_all_workers(#state{partition = Partition}) ->
    Workers = dstwkr_registry:find_all(Partition),
    dict:from_list(Workers).

wait_for_vnode_status_results([], _ReqId, Acc) ->
    Acc;
wait_for_vnode_status_results(PrefLists, ReqId, Acc) ->
    receive
        {ReqId, {vnode_status, Index, Status}} ->
            UpdPrefLists = proplists:delete(Index, PrefLists),
            wait_for_vnode_status_results(UpdPrefLists,
                                          ReqId,
                                          [{Index, Status} | Acc]);
         _ ->
            wait_for_vnode_status_results(PrefLists, ReqId, Acc)
    end.

save_forward_request(Request, #state{forward_reqs = Reqs} = State) ->
    State#state{forward_reqs = [Request | Reqs]}.

send_forward_reqs(TargetNode, #state{forward_reqs = Reqs} = State) ->
    Reqs1 = lists:reverse(Reqs),
    log(State, "resending ~b forward reqs", [length(Reqs1)]),
    lists:foreach(
      fun(Req) -> 
              riak_core_vnode_master:command(TargetNode, Req, dstwkr_vnode_master)
      end, Reqs1),
    State#state{forward_reqs = []}.
