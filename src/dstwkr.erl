-module(dstwkr).

-include("dstwkr.hrl").

-export([start_worker/2]).
-export([stop_worker/1]).
-export([get_state/1]).
-export([command/2]).

%% =============================================================================
%% Public
%% =============================================================================

-spec start_worker(any(), module()) -> {ok, pid()}.
start_worker(Id, Mod) ->
    dstwkr_req_fsm:create(Id, Mod).

-spec stop_worker(any()) -> ok.
stop_worker(Id) ->
    dstwkr_req_fsm:stop(Id).

-spec get_state(any()) -> any().
get_state(Id) ->
    dstwkr_req_fsm:get_state(Id).

-spec command(any(), any()) -> any().
command(Id, Cmd) ->
    dstwkr_req_fsm:command(Id, Cmd).

%% =============================================================================
%% Private
%% =============================================================================
