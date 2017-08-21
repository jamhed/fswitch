-module(call_sup).
-behaviour(gen_server).

% accept and distribute to proper processes fs calls, either inbound or outbound

-export([
	start_link/0, start_link/1, create_uuid/0, originate/3, originate/2, originate/1, wait_call/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	jobs,
	template
}).

start_link() -> start_link("~s").
start_link(Template) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Template], []).

originate(Url, Exten, Opts) -> gen_server:call(?MODULE, {originate, Url, Exten, Opts}).
originate(Url, Opts) -> originate(Url, self_exten(), Opts).
originate(Url) -> originate(Url, []).

self_exten() -> io_lib:format("&erlang('~s:! ~s')", [?MODULE, node()]).

wait_call() ->
	call:subscribe(event, <<"SYNC">>),
	receive
		{call, _UUID, Map} -> Map
	after
		5000 -> erlang:error(timeout)
	end.

init([Template]) ->
	{ok, #state{jobs=#{}, template=Template}}.

handle_info({freeswitch_sendmsg, Str}, #state{}=S) ->
	UUID = erlang:list_to_binary(Str),
	lager:info("incoming message, uuid:~s", [UUID]),
	{ok, _Pid} = call:start_link(UUID),
	{noreply, S};

handle_info({get_pid, Str, Ref, From}, #state{}=S) ->
	UUID = erlang:list_to_binary(Str),
	lager:info("call control request, uuid:~s", [UUID]),
	CallPid =
		case call:pid(UUID) of
			undefined -> {ok, Pid} = call:start_link(UUID), Pid;
			Pid -> Pid
		end,
	From ! {Ref, CallPid},
	{noreply, S};

handle_info({bgerror, Job, _}, S=#state{jobs=J}) ->
	case maps:get(Job, J) of
		{badkey, _} -> skip;
		UUID -> call:notify_uuid(UUID, <<"BGERROR">>)
	end,
	{noreply, S#state{jobs=maps:remove(Job, J)}};

handle_info({bgok, Job, _}, S=#state{jobs=J}) ->
	{noreply, S#state{jobs=maps:remove(Job, J)}};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_call({originate, URL, Exten, Opts}, _From, S=#state{template=T, jobs=J}) ->
	UUID = create_uuid(),
	Opts1 = apply_opts(Opts, [
		{fun with_uuid/2, [UUID]},
		{fun with_timeout/2, [proplists:get_value(originate_timeout, Opts, 5)]}
	]),
	{ok, Job} = fswitch:bgapi("originate ~s~s ~s", [fswitch:stringify_opts(Opts1, curly), template(T, URL), Exten]),
	{reply, UUID, S#state{ jobs = J#{ Job => UUID } }};

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S) ->
	lager:info("terminate, reason:~p", [_Reason]),
	ok.

code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

with_uuid(Opts, UUID) -> [{origination_uuid, UUID}|Opts].
with_timeout(Opts, Timeout) -> [{originate_timeout, Timeout}|Opts].

apply_opts(Opts, []) -> Opts;
apply_opts(Opts, [{F, Args}|Fs]) -> apply_opts(erlang:apply(F, [Opts|Args]), Fs).

template(T, N) -> io_lib:format(T, [N]).

create_uuid() ->
	{ok, UUID} = fswitch:api(create_uuid),
	UUID.
