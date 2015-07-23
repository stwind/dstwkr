-module(dstwkr_req_fsm_sup).

-behaviour(supervisor).

-export([start_child/1]).
-export([start_link/0]).
-export([init/1]).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    ReqFsmSpec = {undefined,
                  {dstwkr_req_fsm, start_link, []},
                  temporary, 5000, worker, [dstwkr_req_fsm]},

    {ok, {{simple_one_for_one, 10, 10}, [ReqFsmSpec]}}.
