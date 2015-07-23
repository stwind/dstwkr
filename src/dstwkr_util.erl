-module(dstwkr_util).

-include("dstwkr.hrl").

-export([mk_reqid/0]).
-export([get_location/1]).

%% =============================================================================
%% Public
%% =============================================================================

mk_reqid() ->
    erlang:phash2(os:timestamp()).

get_location(Id) ->
    DocIdx = riak_core_util:chash_key({<<"procs">>, Id}),
    Preflist0 = riak_core_apl:get_primary_apl(DocIdx, ?N, dstwkr),
    Preflist = [{Index, Node} || {{Index, Node}, _Type} <- Preflist0],
    {DocIdx, Preflist}.
