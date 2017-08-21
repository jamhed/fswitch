-module(util_core).
-export([match_maps/2, seconds_from/1, ns_from/1, ms_from/1]).

match_maps(_, undefined) -> false;
match_maps(Inner, Outer) ->
	L = maps:to_list(Inner) -- maps:to_list(Outer),
	case erlang:length(L) of
		0 -> true;
		_N -> false
	end.

seconds_from(Ts) ->
	erlang:round(timer:now_diff(erlang:timestamp(), Ts)/1000000).

ms_from(Ts) ->
	erlang:round(timer:now_diff(erlang:timestamp(), Ts)/1000).

ns_from(Ts) -> timer:now_diff(erlang:timestamp(), Ts).
