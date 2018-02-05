-module(fswitch).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").

-export([
	start_link/1, stop/1, alive/0, pid/1,
	api/3, api/2, bgapi/3, bgapi/2, execute/4,
	parse_uuid_dump/1, parse_uuid_dump_string/1,
	stringify_opts/1, stringify_opts/2, stringify_opts/3, subscribe/0, unsubscribe/0
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PING_REPEAT, 1000).

-record(state, {
	node,
	jobs
}).

start_link(Node) -> gen_server:start_link(?MODULE, [Node], []).
stop(Id) -> gen_server:cast(Id, {stop}).

api(Id, Cmd, Args) -> gen_server:call(pid(Id), {api, Cmd, Args}).
api(Id, Cmd) -> gen_server:call(pid(Id), {api, Cmd, []}).
bgapi(Id, Cmd, Args) -> gen_server:call(pid(Id), {bgapi, Cmd, Args}).
bgapi(Id, Cmd) -> gen_server:call(pid(Id), {bgapi, Cmd, []}).
execute(Id, UUID, Command, Args) -> gen_server:call(pid(Id), {execute, UUID, Command, Args}).

pid(Pid) when is_pid(Pid) -> Pid;
pid(Id) when is_list(Id) -> pid(erlang:list_to_atom(Id));
pid(Id) -> gproc:whereis_name({n, l, {?MODULE, Id}}).

subscribe() -> gproc:reg({p, l, ?MODULE}, subscribe).
unsubscribe() -> gproc:unreg({p, l, ?MODULE}).

alive() ->
	Q = qlc:q([ Id || {{n,l,{?MODULE, Id}}, _Pid, _} <- gproc:table({l, n}) ]),
	qlc:e(Q).

init([Node]) ->
	lager:notice("start, node:~p", [Node]),
	gproc:reg({n, l, {?MODULE, Node}}),
	self() ! node_check,
	{ok, #state{jobs=#{}, node=Node}}.

handle_cast({stop}, S=#state{}) -> {stop, normal, S};

handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_info({Re, Job, _}=Msg, S=#state{jobs=J}) when Re =:= bgok; Re =:= bgerror ->
	case maps:get(Job, J) of
		{badkey, _} -> skip;
		Pid -> Pid ! Msg
	end,
	{noreply, S#state{jobs=maps:remove(Job, J)}};

handle_info(node_check, S=#state{node=Node}) ->
	case net_adm:ping(Node) of
		pong ->
			lager:notice("freeswitch node up:~p", [Node]),
			gproc:send({p, l, ?MODULE}, {node_up, Node}),
			erlang:monitor_node(Node, true);
		_ ->
			lager:info("freeswitch node is still down:~p", [Node]),
			erlang:send_after(?PING_REPEAT, self(), node_check)
		end,
		{noreply, S};

handle_info({nodedown, Node}, #state{}=S) ->
	lager:error("freeswitch node down:~p", [Node]),
	gproc:send({p, l, ?MODULE}, {node_down, Node}),
	self() ! node_check,
	{noreply, S};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call({api, Cmd, Args}, _From, S=#state{node=Fs}) ->
	{reply, freeswitch:fapi(Fs, Cmd, Args), S};

handle_call({bgapi, Cmd, Args}, {Pid, _Ref}, S=#state{node=Fs, jobs=J}) ->
	{ok, Job} = freeswitch:fbgapi(Fs, Cmd, Args),
	{reply, {ok, Job}, S#state{jobs=J#{ Job => Pid }}};

handle_call({execute, UUID, Cmd, Args}, _From, S=#state{node=Fs}) ->
	{reply, freeswitch:sendmsg(Fs, UUID, [
		{"call-command", "execute"},
		{"execute-app-name", Cmd},
		{"execute-app-arg", Args}
	]), S};

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S) ->
	lager:notice("terminate, reason:~p", [_Reason]),
	ok.

code_change(_OldVsn, S=#state{}, _Extra) ->
	{ok, S}.

is_variable({K, _}) -> is_variable(K);
is_variable(<<"variable_", _Var/binary>>) -> true;
is_variable(_X) -> false.

variable(<<"variable_", Var/binary>>) -> Var.

parse_uuid_dump_string(Str) ->
	Tokens = binary:split(Str, <<"\n">>, [global]),
	Pairs = [ binary:split(Token, <<": ">>) || Token <- Tokens ],
	[ erlang:list_to_tuple([K, V]) || [K,V] <- Pairs ].

parse_uuid_dump(Pairs) ->
	{Variables, Vars} = lists:partition(fun is_variable/1, to_bin_list(Pairs)),
	VarMap = lists:foldl(fun({K,V}, M) -> M#{ to_bin(K) => to_bin(V) } end, #{}, Vars),
	VariableMap = lists:foldl(fun({K,V}, M) -> M#{ to_bin(variable(K)) => to_bin(V) } end, #{}, Variables),
	{VarMap, VariableMap}.

to_bin_list(L) -> [{to_bin(K), to_bin(V)} || {K,V} <- L ].

to_bin(B) when is_list(B) -> erlang:list_to_binary(B);
to_bin(X) -> X.

format_opt(K,V) when is_number(V) -> io_lib:format("~s=~p", [K,V]);
format_opt(K,V) when is_boolean(V) -> io_lib:format("~s=~p", [K,V]);
format_opt(K,V) -> io_lib:format("~s='~s'", [K,V]).

stringify_opts(Opts) -> stringify_opts(Opts, none, ",").
stringify_opts(Opts, Brackets) -> stringify_opts(Opts, Brackets, ",").

stringify_opts([], _, _) -> "";
stringify_opts(Opts, Brackets, Joiner) when is_map(Opts) -> stringify_opts(maps:to_list(Opts), Brackets, Joiner);
stringify_opts(Opts, Brackets, Joiner) when is_list(Opts) ->
	Str = string:join([ format_opt(K, V) || {K, V} <- Opts ], Joiner),
	brackets(Brackets, Str).

brackets(curly, Str) -> io_lib:format("{~s}", [Str]);
brackets(square, Str) -> io_lib:format("[~s]", [Str]);
brackets(none, Str) -> io_lib:format("~s", [Str]).
