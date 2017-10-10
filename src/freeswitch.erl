-module(freeswitch).

-define(TIMEOUT, 5000).

-export([
	send/2,
	api/2, api/3,
	bgapi/3,
	sapi/3, sbgapi/3, fapi/2, fapi/3, fbgapi/2, fbgapi/3,
	event/2,
	session_event/2,
	nixevent/2,
	session_nixevent/2,
	noevents/1,
	session_noevents/1,
	close/1,
	setevent/2,
	session_setevent/2,
	get_event_header/2,
	get_event_body/1,
	get_event_name/1,
	getpid/1,
	sendmsg/3,
	sendevent/3,
	sendevent_custom/3,
	handlecall/2,
	handlecall/3,
	start_fetch_handler/5,
	start_log_handler/4,
	start_event_handler/4,
	fetch_reply/3
]).

%% @doc Return the value for a specific header in an event or `{error,notfound}'.
get_event_header([], _Needle) -> {error, notfound};
get_event_header({event, Headers}, Needle) when is_list(Headers) -> get_event_header(Headers, Needle);
get_event_header([undefined | Headers], Needle) -> get_event_header(Headers, Needle);
get_event_header([UUID | Headers], Needle) when is_list(UUID) ->get_event_header(Headers, Needle);
get_event_header([{Key,Value} | Headers], Needle) ->
	case Key of
		Needle -> Value;
		_ -> get_event_header(Headers, Needle)
	end.

get_event_name(Event) ->
	get_event_header(Event, "Event-Name").

get_event_body(Event) ->
	get_event_header(Event, "body").

send(Node, Term) ->
	{send, Node} ! Term,
	receive
		Response -> Response
	after
		?TIMEOUT -> timeout
	end.

fetch_reply(Node, FetchID, Reply) ->
	{send, Node} ! {fetch_reply, FetchID, Reply},
	receive
		{ok, FetchID} -> ok;
		{error, FetchID, Reason} -> {error, Reason}
	after
		?TIMEOUT -> timeout
	end.

api(Node, Cmd) -> api(Node, Cmd, "").

api(Node, Cmd, Args) ->
	maybe_log(Node, Cmd, Args),
	{api, Node} ! {api, Cmd, Args},
	receive
		{ok, X} -> 
			{ok, erlang:list_to_binary(X)};
		{error, X} ->
			maybe_log_err(X, Cmd, Args),
			{error, X}
	after ?TIMEOUT ->
		lager:error("fs api timeout ~s ~s", [Cmd, Args]),
		timeout
	end.

maybe_log(Node, uuid_kill=Cmd, Args) -> lager:info("fs api ~s ~s ~s", [Node, Cmd, Args]);
maybe_log(Node, Cmd, Args) -> lager:info("fs api ~s ~s ~s", [Node, Cmd, Args]).
maybe_log_err(X, uuid_kill=Cmd, Args) -> lager:debug("fs api error ~p ~s ~s", [X, Cmd, Args]);
maybe_log_err(X, Cmd, Args) -> lager:info("fs api error ~p ~s ~s", [X, Cmd, Args]).

fmt(Format, Args) -> lists:flatten(io_lib:format(Format, Args)).

sapi(Node, Cmd, Args) ->
	api(Node, Cmd, string:join([ lists:flatten(io_lib:format("~s", [Arg])) || Arg <- Args], " ")).

sbgapi(Node, Cmd, Args) ->
	bgapi(Node, Cmd, string:join([ lists:flatten(io_lib:format("~s", [Arg])) || Arg <- Args], " ")).

fapi(Node, Format) -> fapi(Node, Format, []).
fapi(Node, Format, Args) ->
	[Cmd|Rest] = string:tokens(fmt(Format, Args), " "),
	api(Node, erlang:list_to_atom(Cmd), string:join(Rest, " ")).

fbgapi(Node, Format) -> fbgapi(Node, Format, []).
fbgapi(Node, Format, Args) ->
	[Cmd|Rest] = string:tokens(fmt(Format, Args), " "),
	bgapi(Node, erlang:list_to_atom(Cmd), string:join(Rest, " ")).

bgapi(Node, Cmd, Args) ->
	lager:info("fs bgapi ~p ~s", [Cmd, Args]),
	Self = self(),
	spawn(fun() -> bgapi_handler(Self, Node, Cmd, Args) end),
	receive
		{api, X} -> X
	end.

bgapi_handler(ParentPid, Node, Cmd, Args) ->
	{bgapi, Node} ! {bgapi, Cmd, Args},
	receive
		{error, Reason} ->
			lager:info("fs bgapi error:~p", [Reason]),
			ParentPid ! {api, {error, Reason}};
		{ok, JobID} ->
			lager:debug("fs bgapi success, job id:~p", [JobID]),
			ParentPid ! {api, {ok, l2b(JobID)}},
			receive % wait for the job's reply
				{bgok, JobID, Reply} ->
					lager:debug("fs bgapi job success, job id:~p reply:~p", [JobID, Reply]),
					ParentPid ! {bgok, l2b(JobID), l2b(Reply)};
				{bgerror, JobID, Reply} ->
					lager:info("fs bgapi job error, job id:~p reply:~p", [JobID, Reply]),
					ParentPid ! {bgerror, l2b(JobID), l2b(Reply)}
			end
		after
			?TIMEOUT ->
				lager:error("fs bgapi timeout"),
				ParentPid ! {api, timeout}
	end.

event(Node, Events) when is_list(Events) ->
	{event, Node} ! list_to_tuple(lists:append([event], Events)),
	receive
		ok -> ok;
		{error, Reason} -> {error, Reason}
	after ?TIMEOUT ->
			timeout
	end;
event(Node, Event) when is_atom(Event) ->
	event(Node, [Event]).

session_event(Node, Events) when is_list(Events) ->
	{session_event, Node} ! list_to_tuple([session_event | Events]),
	receive
		ok -> ok;
		{error, Reason} -> {error, Reason}
	after ?TIMEOUT ->
			timeout
	end;
session_event(Node, Event) when is_atom(Event) ->
	session_event(Node, [Event]).

%% @doc Stop receiving any events in the list `Events' from `Node'.
nixevent(Node, Events) when is_list(Events) ->
	{nixevent, Node} ! list_to_tuple(lists:append([nixevent], Events)),
	receive
		ok -> ok;
		{error, Reason} -> {error, Reason}
	after ?TIMEOUT ->
			timeout
	end;
nixevent(Node, Event) when is_atom(Event) ->
	nixevent(Node, [Event]).

session_nixevent(Node, Events) when is_list(Events) ->
	{session_nixevent, Node} ! list_to_tuple([session_nixevent | Events]),
	receive
		ok -> ok;
		{error, Reason} -> {error, Reason}
	after ?TIMEOUT ->
			timeout
	end;
session_nixevent(Node, Event) when is_atom(Event) ->
	session_nixevent(Node, [Event]).

%% @doc Receive only listed events from `Node'.
setevent(Node, Events) when is_list(Events) ->
	{setevent, Node} ! list_to_tuple(lists:append([setevent], Events)),
	receive
		ok -> ok;
		{error, Reason} -> {error, Reason}
	after ?TIMEOUT ->
			timeout
	end;
setevent(Node, Event) when is_atom(Event) ->
	setevent(Node, [Event]).

session_setevent(Node, Events) when is_list(Events) ->
	{session_setevent, Node} ! list_to_tuple([session_setevent | Events]),
	receive
		ok -> ok;
		{error, Reason} -> {error, Reason}
	after ?TIMEOUT ->
			timeout
	end;
session_setevent(Node, Event) when is_atom(Event) ->
	session_setevent(Node, [Event]).

%% @doc Stop receiving any events from `Node'.
noevents(Node) ->
	{noevents, Node} ! noevents,
	receive
		ok -> ok;
		{error, Reason} -> {error, Reason}
	after ?TIMEOUT ->
			timeout
	end.

session_noevents(Node) ->
	{session_noevents, Node} ! session_noevents,
	receive
		ok -> ok;
		{error, Reason} -> {error, Reason}
	after ?TIMEOUT ->
			timeout
	end.

%% @doc Close the connection to `Node'.
close(Node) ->
	{close, Node} ! exit,
	receive
		ok -> ok
	after ?TIMEOUT ->
			timeout
	end.

%% @doc Send an event to FreeSWITCH. `EventName' is the name of the event and
%% `Headers' is a list of `{Key, Value}' string tuples. See the mod_event_socket
%% documentation for more information.
sendevent(Node, EventName, Headers) ->
	{sendevent, Node} ! {sendevent, EventName, Headers},
	receive
		ok -> ok;
		{error, Reason} -> {error, Reason}
	after ?TIMEOUT ->
			timeout
	end.

%% @doc Send a CUSTOM event to FreeSWITCH. `SubClassName' is the name of the event
%% subclass and `Headers' is a list of `{Key, Value}' string tuples. See the
%% mod_event_socket documentation for more information.
sendevent_custom(Node, SubClassName, Headers) ->
	{sendevent, Node} ! {sendevent, 'CUSTOM',  SubClassName, Headers},
	receive
		ok -> ok;
		{error, Reason} -> {error, Reason}
	after ?TIMEOUT ->
			timeout
	end.

%% @doc Send a message to the call identified by `UUID'. `Headers' is a list of
%% `{Key, Value}' string tuples.
sendmsg(Node, UUID, Headers) ->
	lager:info("fs sendmsg uuid:~p headers:~p", [UUID, Headers]),
	{sendmsg, Node} ! {sendmsg, UUID, Headers},
	receive
		ok ->
			ok;
		{error, Reason} ->
			lager:info("fs sendmsg error:~p", [Reason]),
			{error, Reason}
	after ?TIMEOUT ->
			timeout
	end.

getpid(Node) ->
	{getpid, Node} ! getpid,
	receive
		{ok, Pid} when is_pid(Pid) -> {ok, Pid}
	after ?TIMEOUT ->
			timeout
	end.

%% @doc Request that FreeSWITCH send any events pertaining to call `UUID' to
%% `Process' where process is a registered process name.
handlecall(Node, UUID, Process) ->
	{handlecall, Node} ! {handlecall, UUID, Process},
	receive
		ok -> ok;
		{error, Reason} -> {error, Reason}
	after ?TIMEOUT ->
			timeout
	end.

%% @doc Request that FreeSWITCH send any events pertaining to call `UUID' to
%% the calling process.
handlecall(Node, UUID) ->
	{handlecall, Node} ! {handlecall, UUID},
	receive
		ok -> ok;
		{error, Reason} -> {error, Reason}
	after ?TIMEOUT ->
			timeout
	end.

event_handler(Self, Node, Type, Module, Function, State) ->
	monitor_node(Node, true),
	{foo, Node} ! Type,
	receive
		ok ->
			Self ! {Type, {ok, self()}},
			apply(Module, Function, [Node, State]);
		{error,Reason} ->
			Self ! {Type, {error, Reason}}
		after ?TIMEOUT ->
			Self ! {Type, timeout}
	end.

start_handler(Node, Type, Module, Function, State) ->
	Self = self(),
	spawn(fun() -> event_handler(Self, Node, Type, Module, Function, State) end),
	receive
		{Type, X} -> X
	end.

start_log_handler(Node, Module, Function, State) ->
	start_handler(Node, register_log_handler, Module, Function, State).

start_event_handler(Node, Module, Function, State) ->
	start_handler(Node, register_event_handler, Module, Function, State).

start_fetch_handler(Node, Section, Module, Function, State) ->
	start_handler(Node, {bind, Section}, Module, Function, State).

l2b(L) when is_list(L) -> erlang:list_to_binary(L).