-module(dstwkr_worker).

-include("dstwkr.hrl").

-export([start_link/3]).
-export([create/3]).
-export([get_state/2]).
-export([set_state/3]).
-export([command/3]).
-export([stop/2]).

-export([transfer/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% TODO: more formal behaviour spec
-callback init(term()) -> 
    {ok, ModState :: term()} | {error, Reason :: term()}.

-callback handle_command(term(), term()) -> 
    {Resp :: term(), ModState :: term()}.

-callback handle_info(term(), term()) -> ModState :: term().

-callback transferred(pid(), term()) -> {ok, ModState :: term()}.

-callback terminate(Reason :: term(), ModState :: term()) -> ok.

-record(state, {
          id :: term(),
          partition :: integer(),
          mod :: module(),
          modstate :: term()
         }).

%% =============================================================================
%% Public
%% =============================================================================

start_link(Id, Mod, Partition) ->
    gen_server:start_link(?MODULE, [Id, Mod, Partition], []).

create(Id, Mod, Partition) ->
    case dstwkr_registry:find(Id) of
        {ok, Pid} ->
            {ok, Pid};
        {error, not_found} ->
            dstwkr_worker_sup:start_child(Id, Mod, Partition)
    end.

stop(Id, From) ->
    maybe_cast(Id, From, stop).

get_state(Id, From) ->
    maybe_cast(Id, From, get_state).

set_state(Id, From, State) ->
    maybe_cast(Id, From, {set_state, State}).

command(Id, From, Cmd) ->
    maybe_cast(Id, From, {command, Cmd}).

transfer(New, Old) ->
    gen_server:cast(New, {transfer_from, Old}).

%% =============================================================================
%% gen_server
%% =============================================================================

init([Id, Mod, Partition]) ->
    log_init(Id),
    exometer:update([dstwkr, worker, duration], timer_start),
    exometer:update([dstwkr, worker, num], 1),
    dstwkr_registry:reg(Partition, Id, Mod, self()),
    {ok, #state{id = Id, mod = Mod, partition = Partition}, 0}.

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};

handle_call({transfer_to, NewPid}, _From, State) ->
    {reply, {ok, State}, transferred_to(NewPid, State)};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({get_state, From}, #state{modstate = ModState} = State) ->
    {noreply, send_reply({ok, self(), ModState}, From, State)};

handle_cast({{set_state, ModState}, From}, State) ->
    {noreply, send_reply(ModState, From, State#state{modstate = ModState})};

handle_cast({{command, Cmd}, From}, State) ->
    exometer:update([dstwkr, worker, command], 1),
    mod_command(Cmd, From, State);

handle_cast({stop, From}, State) ->
    {stop, normal, send_reply(ok, From, State)};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({transfer_from, Old}, State) ->
    exometer:update([dstwkr, worker, transfer], 1),
    lager:debug("~p transfering state from ~p", [self(), Old]),
    case gen_server:call(Old, {transfer_to, self()}) of
        {ok, OldState} ->
            ok = gen_server:cast(Old, stop),
            {noreply, merge_state(OldState, State)};
        {error, Reason} ->
            {stop, {transfer_fail, Reason}, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    {noreply, mod_init(State)};

handle_info(Event, State) ->
    mod_event(Event, State).

terminate(Reason, #state{id = Id} = State) ->
    mod_terminate(Reason, State),
    dstwkr_registry:unreg(Id),
    log_terminate(Id),
    exometer:update([dstwkr, worker, duration], timer_end),
    exometer:update([dstwkr, worker, num], -1),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Private
%% =============================================================================

mod_init(#state{id = Id, mod = Mod} = State) ->
    %% TODO: handle init error
    {ok, ModState} = Mod:init(Id),
    State#state{modstate = ModState}.

mod_command(Cmd, From, #state{mod = Mod, modstate = ModState} = State) ->
    %% TODO: handle mod crash
    case Mod:handle_command(Cmd, ModState) of
        {reply, Resp, ModState1} ->
            State1 = send_reply(Resp, From, State#state{modstate = ModState1}),
            {noreply, State1};
        {stop, Reason, ModState1} ->
            {stop, Reason, State#state{modstate = ModState1}}
    end.

mod_event(Cmd, #state{mod = Mod, modstate = ModState} = State) ->
    %% TODO: handle mod crash
    case Mod:handle_info(Cmd, ModState) of
        {ok, ModState1} ->
            {noreply, State#state{modstate = ModState1}};
        {stop, ModState1} ->
            {stop, normal, State#state{modstate = ModState1}};
        {stop, Reason, ModState1} ->
            {stop, Reason, State#state{modstate = ModState1}}
    end.

send_reply(Resp, From, State) ->
    do_reply(Resp, From),
    State.

mod_terminate(Reason, #state{mod = Mod, modstate = ModState} = State) ->
    Mod:terminate(Reason, ModState),
    State.

mod_transferred(NewPid, #state{mod = Mod, modstate = ModState} = State) ->
    {ok, ModState1} = Mod:transferred(NewPid, ModState),
    State#state{modstate = ModState1}.

transferred_to(NewPid, State) ->
    mod_transferred(NewPid, State).

do_reply(Error, {ReqId, Sender}) ->
    gen_fsm:send_event(Sender, {response, ReqId, Error}),
    ok.

maybe_cast(Id, From, Msg) -> 
    case dstwkr_registry:find(Id) of
        {ok, Pid} ->
            gen_server:cast(Pid, {Msg, From});
        {error, _} = Error ->
            do_reply(Error, From)
    end.

log_init(Id) ->
    Meta = [{part, worker}, {action, init}, {worker_id, Id}],
    lager:debug(Meta, "[worker] ~p init", [Id]).

log_terminate(Id) ->
    Meta = [{part, worker}, {action, terminate}, {worker_id, Id}],
    lager:debug(Meta, "[worker] ~p terminate", [Id]).

merge_state(Old, _New) ->
    Old.
