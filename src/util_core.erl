-module(util_core).
-export([match_maps/2, seconds_from/1, ns_from/1, ms_from/1, flatten/1, explode/1]).

flatten(M) -> lists:flatten(flatten([], M)).

flatten(Pk, M) when is_map(M) ->
	[ flatten(Pk++[K], V) || {K, V} <- maps:to_list(M) ];
flatten(K, V) ->
	{K, V}.

explode(L) -> lists:foldl(fun({K,V}, M) -> explode(K, V, M) end, #{}, L).
explode([K], V, M) -> M#{ K => V };
explode([K|Rest], V, M) -> M#{ K => explode(Rest, V, maps:get(K, M, #{})) }.

match_maps(_, undefined) -> false;
match_maps(Inner, Outer) ->
	L = flatten(Inner) -- flatten(Outer),
	case erlang:length(L) of
		0 -> true;
		_N -> false
	end.

seconds_from(Ts) ->
	erlang:convert_time_unit(erlang:monotonic_time() - Ts, native, second).

ms_from(Ts) ->
	erlang:convert_time_unit(erlang:monotonic_time() - Ts, native, millisecond).

ns_from(Ts) ->
	erlang:convert_time_unit(erlang:monotonic_time() - Ts, native, microsecond).
