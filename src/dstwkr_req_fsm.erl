-module(dstwkr_req_fsm).

-behaviour(gen_fsm).

-include("dstwkr.hrl").

-export([
         start_link/4, 
         stop/1, 
         create/2,
         get_state/1,
         command/2
        ]).

-export([
         init/1,
         code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         terminate/3
        ]).

-export([
         prepare/2,
         execute/2,
         waiting/2
        ]).

-define(TIMEOUT, 5000).

-record(state, {
          preflist :: riak_core_apl:preflist(),
          req_id :: integer(),
          from :: pid(),
          id :: term(),
          request :: any(),
          responses = 0 :: integer()}).

%% =============================================================================
%% Public
%% =============================================================================

start_link(ReqId, From, Id, Request) ->
    gen_fsm:start_link(?MODULE, [ReqId, From, Id, Request], []).

create(Id, Mod) ->
    call_fsm(Id, {create, Mod}).

stop(Id) ->
    call_fsm(Id, stop).

get_state(Id) ->
    call_fsm(Id, get_state).

command(Id, Cmd) ->
    call_fsm(Id, {command, Cmd}).

%% =============================================================================
%% gen_fsm
%% =============================================================================

init([ReqId, From, Id, Request]) ->
    State = #state{req_id=ReqId,from=From,id=Id,request=Request},
    exometer:update([dstwkr, req_fsm, num], 1),
    exometer:update([dstwkr, req_fsm, duration], timer_start),
    {ok, prepare, State, 0}.

handle_info(_Info, _StateName, StateData) ->
    {stop, badmsg, StateData}.

handle_event(_Event, _StateName, StateData) ->
    {stop, badmsg, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop, badmsg, StateData}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(_Reason, _SN, _SD) ->
    exometer:update([dstwkr, req_fsm, num], -1),
    exometer:update([dstwkr, req_fsm, duration], timer_end),
    ok.

%% =============================================================================
%% States
%% =============================================================================

%% @doc Prepare request by retrieving the preflist.
prepare(timeout, #state{id=Id}=State) ->
    {_DocIdx, Preflist = [_]} = dstwkr_util:get_location(Id),
    {next_state, execute, State#state{preflist=Preflist}, 0}.

%% @doc Execute the request.
execute(timeout, #state{preflist = Preflist, req_id = ReqId,
                        id = Id, request = Request} = State) ->
    dstwkr_vnode:request(Preflist, {ReqId, self()}, Id, Request),
    {next_state, waiting, State}.

%% @doc Attempt to write to every single node responsible for this
%%      group.
waiting({response, ReqId, Response}, #state{from=From}=State) ->
    From ! {ReqId, Response},
    {stop, normal, State}.

%% =============================================================================
%% Private
%% =============================================================================

call_fsm(Id, Request) ->
    ReqId = dstwkr_util:mk_reqid(),
    exometer:update([dstwkr, req_fsm, request, duration], timer_start),
    {ok, _} = dstwkr_req_fsm_sup:start_child([ReqId, self(), Id, Request]),
    receive
        {ReqId, Response} ->
            exometer:update([dstwkr, req_fsm, request, duration], timer_end),
            Response
    after 
        ?TIMEOUT ->
            {error, timeout}
    end.
