-module(gen_safe).
-export([call/3, call/4, cast/3, info/3]).

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