-module(fswitch).
-behaviour(gen_server).

-export([
	start_link/1,
	api/2, api/1, bgapi/2, bgapi/1, execute/3,
	parse_uuid_dump/1, parse_uuid_dump_string/1,
	stringify_opts/1, stringify_opts/2
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	drone,
	jobs
}).

start_link(Drone) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Drone], []).

api(Cmd, Args) -> gen_server:call(?MODULE, {api, Cmd, Args}).
api(Cmd) -> gen_server:call(?MODULE, {api, Cmd, []}).
bgapi(Cmd, Args) -> gen_server:call(?MODULE, {bgapi, Cmd, Args}).
bgapi(Cmd) -> gen_server:call(?MODULE, {bgapi, Cmd, []}).
execute(UUID, Command, Args) -> gen_server:call(?MODULE, {execute, UUID, Command, Args}).

init([FsDrone]) ->
	Drone = erlang:list_to_atom(FsDrone),
	lager:notice("start, drone:~p", [Drone]),
	net_adm:ping(Drone),
	erlang:monitor_node(Drone, true),
	{ok, #state{jobs=#{}, drone=Drone}}.

handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_info({Re, Job, _}=Msg, S=#state{jobs=J}) when Re =:= bgok; Re =:= bgerror ->
	case maps:get(Job, J) of
		{badkey, _} -> skip;
		Pid -> Pid ! Msg
	end,
	{noreply, S#state{jobs=maps:remove(Job, J)}};

handle_info({nodedown, Node}, #state{}=S) ->
	lager:error("freeswitch node down:~p", [Node]),
	{noreply, S};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call({api, Cmd, Args}, _From, S=#state{drone=Fs}) ->
	{reply, freeswitch:fapi(Fs, Cmd, Args), S};

handle_call({bgapi, Cmd, Args}, {Pid, _Ref}, S=#state{drone=Fs, jobs=J}) ->
	{ok, Job} = freeswitch:fbgapi(Fs, Cmd, Args),
	{reply, {ok, Job}, S#state{jobs=J#{ Job => Pid }}};

handle_call({execute, UUID, Cmd, Args}, _From, S=#state{drone=Fs}) ->
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
format_opt(K,V) -> io_lib:format("~s=~s", [K,V]).

stringify_opts(Opts) -> stringify_opts(Opts, none).

stringify_opts([], _) -> "";
stringify_opts(Opts, Brackets) when is_map(Opts) -> stringify_opts(maps:to_list(Opts), Brackets);
stringify_opts(Opts, Brackets) when is_list(Opts) ->
	Str = string:join([ format_opt(K, V) || {K, V} <- Opts ], ","),
	brackets(Brackets, Str).

brackets(curly, Str) -> io_lib:format("{~s}", [Str]);
brackets(square, Str) -> io_lib:format("[~s]", [Str]);
brackets(none, Str) -> io_lib:format("~s", [Str]).
