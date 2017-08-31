-module(gen_safe).
-include_lib("stdlib/include/qlc.hrl").
-export([call/3, call/4, cast/3, info/3]).
-export([subscribe/1, unsubscribe/1, subscriptions/1, subscriptions/2]).

call(Id, _PidF, Msg) when is_pid(Id) -> gen_server:call(Id, Msg);
call(Id, PidF, Msg) ->
	case PidF(Id) of
		undefined -> {error, no_pid, Id};
		Pid -> gen_server:call(Pid, Msg)
	end.

call(Id, _PidF, Msg, Timeout) when is_pid(Id) -> gen_server:call(Id, Msg, Timeout);
call(Id, PidF, Msg, Timeout) ->
	case PidF(Id) of
		undefined -> {error, no_pid, Id};
		Pid -> gen_server:call(Pid, Msg, Timeout)
	end.

cast(Id, _Pid, Msg) when is_pid(Id) -> gen_server:cast(Id, Msg);
cast(Id, PidF, Msg) ->
	case PidF(Id) of
		undefined -> {error, no_pid, Id};
		Pid -> gen_server:cast(Pid, Msg)
	end.

info(Id, _PidF, Msg) when is_pid(Id) -> gen_server:info(Id, Msg);
info(Id, PidF, Msg) ->
	case PidF(Id) of
		undefined -> {error, no_pid, Id};
		Pid -> Pid ! Msg
	end.

% allow multiple calls to subscribe from the same pid

subscribe(K) ->
	case subscriptions(K, self()) of
		[] -> gproc:reg({p, l, K}, subscribe);
		_ -> skip
	end.

unsubscribe(K) ->
	Self = self(),
	case subscriptions(K, Self) of
		[Self] -> gproc:unreg({p, l, K});
		_ -> skip
	end.

subscriptions(K) ->
	Q = qlc:q([ Pid || {{p, l, Ks}, Pid, subscribe} <- gproc:table({l, p}), Ks =:= K ]),
	qlc:e(Q).

subscriptions(K, P) ->
	Q = qlc:q([ Pid || {{p, l, Ks}, Pid, subscribe} <- gproc:table({l, p}), Ks =:= K, Pid =:= P ]),
	qlc:e(Q).
