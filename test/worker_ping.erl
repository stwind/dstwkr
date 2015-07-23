-module(worker_ping).

-behaviour(dstwkr_worker).

-export([init/1]).
-export([handle_command/2]).
-export([handle_info/2]).
-export([terminate/2]).

-record(state, { }).

%% =============================================================================
%% Public
%% =============================================================================

init(_Id) ->
    {ok, #state{}}.

handle_command(ping, State) ->
    {reply, pong, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% =============================================================================
%% Private
%% =============================================================================
