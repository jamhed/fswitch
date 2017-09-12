-module(event_log).
-behaviour(gen_server).

-export([start_link/0, add/2, search/2, wait/3, wait/4, wait_now/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	wait,
	log = []
}).

start_link() -> gen_server:start_link(?MODULE, [], []).
add(Id, Msg) -> gen_server:call(Id, {add, Msg}).
wait(Id, Match, Caller) -> wait(Id, Match, Caller, 2).
wait(Id, Match, Caller, Depth) -> gen_server:call(Id, {wait, Match, Caller, Depth}).
wait_now(Id, Match, Caller) -> gen_server:call(Id, {wait_now, Match, Caller}).
search(Id, Match) -> gen_server:call(Id, {search, Match}).

init([]) -> {ok, #state{
	log = []
}}.

handle_call({add, Msg}, _, S=#state{ wait = undefined }) ->
	{reply, ok, add_msg(Msg, S)};

handle_call({add, Msg}, _, S=#state{ wait = {Match, Caller} }) ->
	case util_core:match_maps(Match, Msg) of
		true -> {reply, {match, Caller, {erlang:monotonic_time(), Msg}}, (add_msg(Msg, S))#state{ wait = undefined }};
		false -> {reply, no_match, add_msg(Msg, S)}
	end;

handle_call({search, Match}, _, S=#state{log=Log}) ->
	{reply, lookback(Match, Log), S};

handle_call({wait, Match, Caller, Depth}, _, S=#state{ log = Log }) ->
	case lookback(Depth, Match, Log) of
		{match, _, _}=Msg -> {reply, Msg, S};
		_ -> {reply, no_match, S#state{ wait = {Match,Caller} } }
	end;

handle_call({wait_now, Match, Caller}, _, S=#state{}) ->
	{reply, no_match, S#state{ wait = {Match,Caller} } };

handle_call(_Request, _From, S=#state{}) -> {reply, {error, not_handled}, S}.

handle_cast(_Msg, S=#state{}) -> {noreply, S}.
handle_info(_Info, S=#state{}) -> {noreply, S}.
terminate(_Reason, _S) -> ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

lookback(0, _, _) -> no_match;
lookback(_, _, []) -> no_match;
lookback(Depth, Match, [{Ts,Msg} | Log]) ->
	case util_core:match_maps(Match, Msg) of
		true -> {match, Ts, Msg};
		false -> lookback(Depth-1, Match, Log)
	end.

lookback(_, []) -> no_match;
lookback(Match, [{Ts,Msg} | Log]) ->
	case util_core:match_maps(Match, Msg) of
		true -> {match, Ts, Msg};
		false -> lookback(Match, Log)
	end.


add_msg(Msg, S=#state{log=Log}) -> S#state{ log = [ {erlang:monotonic_time(), Msg} | Log ] }.
