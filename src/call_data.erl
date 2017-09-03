-module(call_data).
-behaviour(gen_server).

% store call variables
-define(GRACE, 5). % in seconds
-define(CHECK, 1000).

-export([
	start_link/0, on_hangup/2, set/4, get/2, vars/2, variables/2
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	tid,
	on_hangup = []
}).

start_link() -> gen_server:start_link(?MODULE, [], []).
set(Pid, UUID, Vars, Variables) -> gen_server:call(Pid, {set, UUID, Vars, Variables}).
get(Pid, UUID) -> gen_server:call(Pid, {get, UUID}).
vars(Pid, UUID) -> gen_server:call(Pid, {vars, UUID}).
variables(Pid, UUID) -> gen_server:call(Pid, {variables, UUID}).
on_hangup(Pid, UUID) -> gen_server:call(Pid, {on_hangup, UUID}).

init([]) ->
	Tid = ets:new(?MODULE, [set,private,compressed]),
	self() ! clear_table,
	{ok, #state{tid=Tid}}.

handle_call({set, UUID, Vars, Variables}, _From, S=#state{tid=Tid}) when map_size(Variables) =:= 0 ->
	ets:update_element(Tid, UUID, {2, Vars}),
	{reply, ok, S};

handle_call({set, UUID, Vars, Variables}, _From, S=#state{tid=Tid}) ->
	ets:insert(Tid, {UUID, Vars, Variables}),
	{reply, ok, S};

handle_call({get, UUID}, _From, S=#state{tid=Tid}) ->
	Re = case ets:lookup(Tid, UUID) of
		[{UUID, Vars, Variables}] -> [Vars, Variables];
		_ -> []
	end,
	{reply, Re, S};

handle_call({vars, UUID}, _From, S=#state{tid=Tid}) ->
	Re = case ets:lookup(Tid, UUID) of
		[{UUID, Vars, _}] -> [Vars];
		_ -> []
	end,
	{reply, Re, S};

handle_call({variables, UUID}, _From, S=#state{tid=Tid}) ->
	Re = case ets:lookup(Tid, UUID) of
		[{UUID, _, Variables}] -> [Variables];
		_ -> []
	end,
	{reply, Re, S};

handle_call({on_hangup, UUID}, _From, S=#state{on_hangup=List}) ->
	{reply, ok, S#state{on_hangup=[{UUID, erlang:monotonic_time(second)}|List]}};

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, {error, not_implemented}, S}.

handle_info(clear_table, S=#state{tid=Tid, on_hangup=List}) ->
	[ ets:delete(Tid, UUID) || {UUID, Ts} <- List, erlang:monotonic_time(second)-Ts > ?GRACE ],
	erlang:send_after(?CHECK, self(), clear_table),
	{noreply, S};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

terminate(_Reason, _S) ->
	lager:info("terminate, reason:~p", [_Reason]),
	ok.

code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.
