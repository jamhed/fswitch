-module(call).
-behaviour(gen_server).
-include_lib("fswitch/include/fswitch.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([
	start_link/2, pid/1, tuple/1, alive/1, link_process/2, wait_hangup/1,
	subscribe/2, subscribe/3, unsubscribe/2, unsubscribe/3,
	vars/1, variables/1,
	hangup/1, answer/1, park/1, break/1,
	deflect/2, display/3, getvar/2, hold/1, hold/2, setvar/2, setvar/3, setvars/2, broadcast/2, displace/3,
	send_dtmf/2, recv_dtmf/2,
	execute/3, playback/2, tone_detect/4, detect_tone/2, stop_detect_tone/1,
	bridge/2, transfer/2, transfer/3, transfer/4, record/3,
	wait/1, wait_event/2, wait_event/3, wait_event_now/2, wait_event_now/3,
	active/0, hupall/0, stop/0, answer/0, match_for/1, notify_uuid/2, stop/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	fs, % fs id (erlang node)
	call_state,
	uuid, % we silently assume these to be unique event among several fs instances
	wait_hangup = [],
	event_log
}).

active() ->
	Q = qlc:q([ UUID || {{n,l,{?MODULE,UUID}}, _Pid, _} <- gproc:table({l, n}) ]),
	qlc:e(Q).

hupall() -> [ hangup(UUID) || UUID <- active() ].
answer() -> [ answer(UUID) || UUID <- active() ].
stop() -> [ stop(UUID) || UUID <- active() ].

match_for(Map) when is_map(Map) ->
	Q = qlc:q([ UUID || {{n,l,{?MODULE,UUID}}, _Pid, M} <- gproc:table({l, n}), util_core:match_maps(Map, M) ]),
	qlc:e(Q);
match_for(Key) ->
	Q = qlc:q([ {UUID, maps:get(Key, M)} || {{n,l,{?MODULE,UUID}}, _Pid, M=#{}} <- gproc:table({l, n}), maps:is_key(Key, M) ]),
	qlc:e(Q).

start_link(Id, UUID) ->
	gen_server:start_link(?MODULE, [Id, UUID], []).

% gproc api
tuple(UUID) -> {?MODULE, UUID}.
pid({?MODULE, UUID}) -> pid(UUID);
pid(UUID) -> gproc:whereis_name({n, l, {?MODULE, UUID}}).
wait(UUID) -> gproc:await({n, l, {?MODULE, UUID}}, 5000), UUID.

subscribe(uuid, UUID) -> gen_safe:subscribe({?MODULE, uuid, UUID});
subscribe(event, L) when is_list(L) -> [ subscribe(event, Ev) || Ev <- L ];
subscribe(event, Event) -> gen_safe:subscribe({?MODULE, event, Event}).
subscribe(both, UUID, L) when is_list(L) -> [ subscribe(both, UUID, Ev) || Ev <- L ];
subscribe(both, UUID, Event) -> gen_safe:subscribe({?MODULE, both, UUID, Event}).
unsubscribe(uuid, UUID) -> gen_safe:unsubscribe({?MODULE, uuid, UUID});
unsubscribe(event, L) when is_list(L) -> [ unsubscribe(event, Ev) || Ev <- L ];
unsubscribe(event, Event) -> gen_safe:unsubscribe({?MODULE, event, Event}).
unsubscribe(both, UUID, L) when is_list(L) -> [ unsubscribe(both, UUID, Ev) || Ev <- L ];
unsubscribe(both, UUID, Event) -> gen_safe:unsubscribe({?MODULE, both, UUID, Event}).

vars(Id) -> gen_safe:call(Id, fun pid/1, vars).
variables(Id) -> gen_safe:call(Id, fun pid/1, variables).

hangup(Id) -> gen_safe:cast(Id, fun pid/1, hangup).
stop(Id) -> gen_safe:cast(Id, fun pid/1, stop).
answer(Id) -> gen_safe:cast(Id, fun pid/1, answer).
park(Id) -> gen_safe:cast(Id, fun pid/1, park).
break(Id) -> gen_safe:cast(Id, fun pid/1, break).
alive(Id) -> gen_safe:cast(Id, fun pid/1, alive).
tone_detect(Id, EvName, Tone, Timeout) -> gen_safe:cast(Id, fun pid/1, {tone_detect, EvName, Tone, Timeout}).
detect_tone(Id, ToneName) -> gen_safe:cast(Id, fun pid/1, {detect_tone, ToneName}).
stop_detect_tone(Id) -> gen_safe:cast(Id, fun pid/1, stop_detect_tone).
broadcast(Id, Path) -> gen_safe:cast(Id, fun pid/1, {broadcast, Path}).
displace(Id, Cmd, Path) -> gen_safe:cast(Id, fun pid/1, {displace, Cmd, Path}).
execute(Id, Cmd, Args) -> gen_safe:cast(Id, fun pid/1, {execute, Cmd, Args}).
playback(Id, Tone) -> gen_safe:cast(Id, fun pid/1, {playback, Tone}).
bridge(Id, UUID) -> gen_safe:cast(Id, fun pid/1, {bridge, UUID}).

link_process(Id, Pid) -> gen_safe:call(Id, fun pid/1, {link_process, Pid}).

wait_hangup(Id) -> gen_safe:call(Id, fun pid/1, {wait_hangup}).
deflect(Id, Target) -> gen_safe:call(Id, fun pid/1, {deflect, Target}).
display(Id, Name, Number) -> gen_safe:call(Id, fun pid/1, {display, Name, Number}).
getvar(Id, Name) -> gen_safe:call(Id, fun pid/1, {getvar, Name}).
hold(Id) -> gen_safe:call(Id, fun pid/1, {hold}).
hold(Id, off) -> gen_safe:call(Id, fun pid/1, {hold, off});
hold(Id, toggle) -> gen_safe:call(Id, fun pid/1, {hold, toggle}).
send_dtmf(Id, DTMF) -> gen_safe:call(Id, fun pid/1, {send_dtmf, DTMF}).
recv_dtmf(Id, DTMF) -> gen_safe:call(Id, fun pid/1, {recv_dtmf, DTMF}).
setvar(Id, Name) -> gen_safe:call(Id, fun pid/1, {setvar, Name}).
setvar(Id, Name, Value) -> gen_safe:call(Id, fun pid/1, {setvar, Name, Value}).
setvars(Id, Vars) -> gen_safe:call(Id, fun pid/1, {setvars, Vars}).
transfer(Id, Target) -> gen_safe:call(Id, fun pid/1, {transfer, Target}).
transfer(Id, Target, Dialplan) -> gen_safe:call(Id, fun pid/1, {transfer, Target, Dialplan}).
transfer(Id, Target, Dialplan, Context) -> gen_safe:call(Id, fun pid/1, {transfer, Target, Dialplan, Context}).
record(Id, Action=start, Path) -> gen_safe:call(Id, fun pid/1, {record, Action, Path});
record(Id, Action=stop, Path) -> gen_safe:call(Id, fun pid/1, {record, Action, Path});
record(Id, Action=mask, Path) -> gen_safe:call(Id, fun pid/1, {record, Action, Path});
record(Id, Action=unmask, Path) -> gen_safe:call(Id, fun pid/1, {record, Action, Path}).

event_match(Ev) when is_binary(Ev) -> #{ <<"Event-Name">> => Ev };
event_match(M) when is_map(M) -> M.

wait_event(Id, Match) -> wait_event(Id, Match, 5000).
wait_event(Id, Match, Timeout) ->
	gen_safe:call(Id, fun pid/1, {wait_event, event_match(Match)}, Timeout).

wait_event_now(Id, Match) -> wait_event_now(Id, Match, 5000).
wait_event_now(Id, Match, Timeout) ->
	gen_safe:call(Id, fun pid/1, {wait_event_now, event_match(Match)}, Timeout).

sync_state(Pid) when is_pid(Pid) -> Pid ! sync_state.

init([Id, UUID]) ->
	lager:info("~s start, fs:~p", [UUID, Id]),
	gproc:reg({n, l, {?MODULE, UUID}}),
	sync_state(self()),
	{ok, EvLog} = event_log:start_link(),
	{ok, #state{fs=Id, uuid = UUID, event_log = EvLog}}.

handle_cast(answer, S=#state{fs=Id, uuid=UUID}) -> fswitch:api(Id, "uuid_answer ~s", [UUID]), {noreply, S};
handle_cast(park, S=#state{fs=Id, uuid=UUID}) -> fswitch:api(Id, "uuid_park ~s", [UUID]), {noreply, S};
handle_cast(break, S=#state{fs=Id, uuid=UUID}) -> fswitch:api(Id, "uuid_break ~s", [UUID]), {noreply, S};
handle_cast(alive, S=#state{fs=Id, uuid=UUID}) ->
	{ok, "true"} = fswitch:api(Id, "uuid_exists ~s", [UUID]),
	{noreply, S};
handle_cast(hangup, S=#state{fs=Id, uuid=UUID}) ->
	fswitch:api(Id, "uuid_kill ~s", [UUID]),
	{noreply, S};
handle_cast(stop, S=#state{}) ->
	{stop, normal, S};

% doesn't work?
handle_cast({tone_detect, Name, Tone, Timeout}, S=#state{fs=Id, uuid=UUID}) ->
	fswitch:api(Id, "tone_detect ~s ~s ~s r +~p stop_tone_detect '' 1", [UUID, Name, Tone, Timeout]),
	{noreply, S};
handle_cast({detect_tone, ToneName}, S=#state{fs=Id, uuid=UUID}) ->
	fswitch:api(Id, "spandsp_start_tone_detect ~s ~s", [UUID, ToneName]),
	{noreply, S};
handle_cast(stop_detect_tone, S=#state{fs=Id, uuid=UUID}) ->
	fswitch:api(Id, "spandsp_stop_tone_detect ~s", [UUID]),
	{noreply, S};

handle_cast({broadcast, Path}, S=#state{fs=Id, uuid=UUID}) ->
	fswitch:api(Id, "uuid_broadcast ~s ~s", [UUID, Path]),
	{noreply, S};
handle_cast({displace, Cmd, Path}, S=#state{fs=Id, uuid=UUID}) ->
	fswitch:api(Id, "uuid_displace ~s ~s ~s", [UUID, Cmd, Path]),
	{noreply, S};
handle_cast({execute, Cmd, Path}, S=#state{fs=Id, uuid=UUID}) ->
	fswitch:execute(Id, UUID, Cmd, Path),
	{noreply, S};
handle_cast({playback, Tone}, S=#state{fs=Id, uuid=UUID}) ->
	fswitch:execute(Id, UUID, "playback", Tone),
	{noreply, S};
handle_cast({bridge, Peer}, S=#state{fs=Id, uuid=UUID}) ->
	fswitch:api(Id, "uuid_bridge ~s ~s", [UUID, Peer]),
	{noreply, S};

handle_cast(_Msg, S=#state{uuid=_UUID}) ->
	lager:error("~s unhandled cast:~p", [_UUID, _Msg]),
	{noreply, S}.

% these messages come directly from fs, convert uuid to binary

handle_info({call_event, {event,[UUID|Pairs]}}, S=#state{}) when is_list(UUID) ->
	handle_info({call_event, {event,[erlang:list_to_binary(UUID)|Pairs]}}, S);

handle_info({call_event, {event,[UUID|Pairs]}}, S=#state{uuid=UUID}) ->
	{Vars, Variables} = fswitch:parse_uuid_dump(Pairs),
	handle_event(Vars, Variables, S);

handle_info({call, {event,[UUID|Pairs]}}, S=#state{}) when is_list(UUID) ->
	handle_info({call, {event,[erlang:list_to_binary(UUID)|Pairs]}}, S);

handle_info({call, {event,[UUID|Pairs]}}, S=#state{uuid=UUID}) ->
	{Vars, Variables} = fswitch:parse_uuid_dump(Pairs),
	handle_event(Vars, Variables, S);

handle_info({call_hangup, UUID}, S=#state{}) when is_list(UUID) ->
	handle_info({call_hangup, erlang:list_to_binary(UUID)}, S);

handle_info({call_hangup, UUID}, S=#state{uuid=UUID, event_log=EvLog}) ->
	case event_log:search(EvLog, event_match(<<"CHANNEL_HANGUP">>)) of
		no_match ->
			% sometimes this event happens before hangup event, and no events follow, so synthesize it
			[Vars] = call_sup:vars(UUID),
			handle_event(Vars#{ <<"Event-Name">> => <<"CHANNEL_HANGUP">> }, #{}, S);
		_ -> skip
	end,
	{stop, normal, S};

handle_info(sync_state, S=#state{fs=Id, uuid=UUID}) ->
	maybe_sync_state(fswitch:api(Id, "uuid_dump ~s", [UUID]), S);

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, S=#state{uuid=_UUID}) ->
	lager:info("~s owner is dead, pid:~p reason:~p", [_UUID, _Pid, _Reason]),
	{stop, normal, S};

handle_info(_Info, S=#state{uuid=_UUID}) ->
	lager:error("~s unhandled info:~p", [_UUID, _Info]),
	{noreply, S}.

handle_call({link_process, Pid}, _From, S=#state{}) ->
	erlang:monitor(process, Pid),
	{reply, self(), S};

handle_call(vars, _From, S=#state{uuid=UUID}) -> {reply, call_sup:vars(UUID), S};
handle_call(variables, _From, S=#state{uuid=UUID}) -> {reply, call_sup:variables(UUID), S};

handle_call({deflect, Target}, _From, S=#state{fs=Id, uuid=UUID}) -> {reply, fswitch:api(Id, "uuid_deflect ~s ~s", [UUID, Target]), S};
handle_call({display, Name, Number}, _From, S=#state{fs=Id, uuid=UUID}) -> {reply, fswitch:api(Id, "uuid_display ~s ~s|~s", [UUID, Name, Number]), S};
handle_call({getvar, Name}, _From, S=#state{fs=Id, uuid=UUID}) -> {reply, fswitch:api(Id, "uuid_getvar ~s ~s", [UUID, Name]), S};
handle_call({hold}, _From, S=#state{fs=Id, uuid=UUID}) -> {reply, fswitch:api(Id, "uuid_hold ~s", [UUID]), S};
handle_call({hold, Cmd}, _From, S=#state{fs=Id, uuid=UUID}) -> {reply, fswitch:api(Id, "uuid_hold ~s ~s", [Cmd, UUID]), S};
handle_call({send_dtmf, DTMF}, _From, S=#state{fs=Id, uuid=UUID}) -> {reply, fswitch:api(Id, "uuid_send_dtmf ~s ~s", [UUID, DTMF]), S};
handle_call({recv_dtmf, DTMF}, _From, S=#state{fs=Id, uuid=UUID}) -> {reply, fswitch:api(Id, "uuid_recv_dtmf ~s ~s", [UUID, DTMF]), S};
handle_call({setvar, Name, Value}, _From, S=#state{fs=Id, uuid=UUID}) -> {reply, fswitch:api(Id, "uuid_setvar ~s ~s ~s", [UUID, Name, Value]), S};
handle_call({setvar, Name}, _From, S=#state{fs=Id, uuid=UUID}) -> {reply, fswitch:api(Id, "uuid_setvar ~s ~s", [UUID, Name]), S};
handle_call({transfer, Target}, _From, S=#state{fs=Id, uuid=UUID}) -> {reply, fswitch:api(Id, "uuid_transfer ~s ~s", [UUID, Target]), S};
handle_call({transfer, Target, Dialplan}, _From, S=#state{fs=Id, uuid=UUID}) -> {reply, fswitch:api(Id, "uuid_transfer ~s ~s ~s", [UUID, Target, Dialplan]), S};
handle_call({transfer, Target, Dialplan, Context}, _From, S=#state{fs=Id, uuid=UUID}) ->
	{reply, fswitch:api(Id, "uuid_transfer ~s ~s ~s ~s", [UUID, Target, Dialplan, Context]), S};
handle_call({record, Action, Path}, _From, S=#state{fs=Id, uuid=UUID}) -> {reply, fswitch:api(Id, "uuid_record ~s ~s ~s", [UUID, Action, Path]), S};
% just wait process to die
handle_call({wait_hangup}, From, S=#state{wait_hangup=WaitList}) -> {noreply, S#state{wait_hangup=[From|WaitList]}};

handle_call({setvars, Vars}, _From, S=#state{fs=Id, uuid=UUID}) -> 
	{reply, fswitch:api(Id, "uuid_setvar_multi ~s ~s", [UUID, fswitch:stringify_opts(Vars, none, ";")]), S};

handle_call({wait_event, Match}, From, S=#state{ event_log=EvLog }) ->
	case event_log:wait(EvLog, Match, From) of
		no_match -> {noreply, S};
		{match, _Ts, _} = Re -> {reply, Re, S}
	end;

handle_call({wait_event_now, Match}, From, S=#state{ event_log=EvLog }) ->
	event_log:wait_now(EvLog, Match, From),
	{noreply, S};

handle_call(_Request, _From, S=#state{uuid=_UUID}) ->
	lager:error("~s unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S=#state{fs=Id, uuid=UUID, wait_hangup=WaitList}) ->
	lager:info("~s terminate, reason:~p", [UUID, _Reason]),
	fswitch:api(Id, "uuid_kill ~s", [UUID]),
	call_sup:on_hangup(UUID),
	[ gen_server:reply(Caller, ok) || Caller <- WaitList ],
	ok.

code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

handle_event(Vars = #{ <<"Event-Name">> := Ev }, Variables, S=#state{uuid=UUID, event_log=EvLog}) ->
	maybe_debug(Ev, UUID, Vars),
	notify_uuid(UUID, Ev),
	notify_event(UUID, Ev, Vars),
	case event_log:add(EvLog, Vars) of
		{match, Caller, {Ts, Msg}} -> gen_server:reply(Caller, {match, Ts, Msg});
		_ -> skip
	end,
	call_sup:set(UUID, Vars, Variables),
	{noreply, set_call_state(Vars, S)}.

notify_uuid(UUID, Ev) ->
	gproc:send({p, l, {?MODULE, uuid, UUID}}, #call_event{uuid=UUID, event=Ev}).

notify_event(UUID, Ev, Vars) ->
	gproc:send({p, l, {?MODULE, both, UUID, Ev}}, #call_event{uuid=UUID, event=Ev, vars=Vars}),
	gproc:send({p, l, {?MODULE, event, Ev}}, #call_event{uuid=UUID, event=Ev, vars=Vars}).

set_call_state(#{ <<"Channel-Call-State">> := State }, S) -> S#state{ call_state = State };
set_call_state(_, S) -> S.

maybe_debug(<<"CHANNEL_STATE">>=Ev, UUID, Vars) -> lager:debug("~s ~s ~s", [UUID, Ev, maps:get(<<"Channel-State">>, Vars)]);
maybe_debug(<<"CHANNEL_HANGUP">>=Ev, UUID, _Vars) -> lager:debug("~s ~s", [UUID, Ev]);
maybe_debug(Ev, UUID, Vars) ->
	lager:debug("~s ~s destination-number:~s", [UUID, Ev, maps:get(<<"Caller-Destination-Number">>, Vars, <<"undefined">>)]).

maybe_sync_state({ok, Dump}, S) ->
	Pairs = fswitch:parse_uuid_dump_string(Dump),
	{Vars, Variables} = fswitch:parse_uuid_dump(Pairs),
	handle_event(Vars#{ <<"Event-Name">> => <<"SYNC">> }, Variables, S);
maybe_sync_state(_Err, S) ->
	lager:info("fs channel is dead already"),
	{stop, normal, S}.
