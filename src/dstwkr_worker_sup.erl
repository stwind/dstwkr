-module(dstwkr_worker_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_child/3]).
-export([init/1]).

%% =============================================================================
%% Public
%% =============================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Id, Mod, Partition) ->
    supervisor:start_child(?MODULE, [Id, Mod, Partition]).

init([]) ->
    {ok, 
     {{simple_one_for_one, 10, 10}, 
      [{dstwkr_worker, {dstwkr_worker, start_link, []},
        temporary, 300000, worker, dynamic}]}}.
