%% Copyright (c) 2013, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(alien).

%% Probes.
-export([start_probe/4]).
-export([stop_probe/1]).
-export([list_probes/0]).
-export([probe/3]).

%% Relays.
-export([start_relay/4]).
-export([stop_relay/1]).
-export([list_relays/0]).

%% Events.
-export([event/2]).
-export([relay/3]).

%% Internal.
-export([sync_event/3]).

-define(TAB, alien).

-type ref() :: any().
-type filters() :: []. %[fun((any()) -> ok | drop)].
-type route() :: {msg, atom() | pid() | port()}
	| {msg, node(), atom()}
	| {udp, inet:hostname(), inet:port_number(), 0..16#ffff}.
%	| {tcp, inet:hostname(), inet:port_number(), [gen_tcp:option()]} %% opts?
%	| {ssl, inet:hostname(), inet:port_number(), [ssl:connect_option()]}. %% opts?
-type probe_spec() :: inline | {process, module(), any()}.
-type probe() :: inline | {process, module(), any(), pid()}.
-type probe_list() :: [{ref(), probe, filters(), route(), probe()}].
-type relay_list() :: [{ref(), relay, module(), any(), route()}].
-type event() :: any().

-export_type([ref/0]).
-export_type([event/0]).

%% Probes.

-spec start_probe(ref(), filters(), route(), probe_spec()) -> ok.
start_probe(Ref, Filters, Route, {process, Module, Opts}) ->
	{ok, Pid} = supervisor:start_child(alien_probes_sup,
		{Ref, {Module, start_link, [Ref, Opts]},
			permanent, 5000, worker, [Module]}),
	_ = ets:insert(?TAB, {Ref, probe, Filters, Route, {process, Module, Opts, Pid}}),
	ok;
start_probe(Ref, Filters, Route, Type) ->
	_ = ets:insert(?TAB, {Ref, probe, Filters, Route, Type}),
	ok.

-spec stop_probe(ref()) -> ok | {error, badarg | notfound}.
stop_probe(Ref) ->
	case ets:lookup(?TAB, Ref) of
		[{_, probe, _, _, Type}] ->
			stop_probe(Ref, Type);
		[] ->
			{error, notfound};
		_ ->
			{error, badarg}
	end.

stop_probe(Ref, {process, _, _, _}) ->
	ok = supervisor:terminate_child(alien_probes_sup, Ref),
	ok = supervisor:delete_child(alien_probes_sup, Ref),
	_ = ets:delete(?TAB, Ref),
	ok;
stop_probe(Ref, _) ->
	_ = ets:delete(?TAB, Ref),
	ok.

-spec list_probes() -> probe_list().
list_probes() ->
	[E || E <- ets:tab2list(?TAB), element(2, E) =:= probe].

%% Ondemand probes always send an event.
-spec probe(ref(), module(), route()) -> ok.
probe(Ref, Module, Route) ->
	send(Ref, Module:probe(Ref), Route).

%% Relays.

-spec start_relay(ref(), module(), any(), route()) -> {ok, pid()}.
start_relay(Ref, Module, Opts, Route) ->
	{ok, Pid} = supervisor:start_child(alien_relays_sup,
		{Ref, {Module, start_link, [Ref, Opts]},
			permanent, 5000, worker, [Module]}),
	_ = ets:insert(?TAB, {Ref, relay, Module, Opts, Route}),
	{ok, Pid}.

-spec stop_relay(ref()) -> ok | {error, badarg | notfound}.
stop_relay(Ref) ->
	case ets:lookup(?TAB, Ref) of
		[{_, relay, _, _, _}] ->
			ok = supervisor:terminate_child(alien_relays_sup, Ref),
			ok = supervisor:delete_child(alien_relays_sup, Ref),
			_ = ets:delete(?TAB, Ref),
			ok;
		[] ->
			{error, notfound};
		_ ->
			{error, badarg}
	end.

-spec list_relays() -> relay_list().
list_relays() ->
	[E || E <- ets:tab2list(?TAB), element(2, E) =:= relay].

%% Events.

-spec event(ref(), event()) -> ok.
event(Ref, Event) ->
	_ = spawn(?MODULE, sync_event, [Ref, Event, self()]),
	ok.

sync_event(Ref, Event, FromPid) ->
	case ets:lookup(?TAB, Ref) of
		%% Drop events that have no corresponding probe.
		%% Should almost only happen with inline events.
		[] ->
			ok;
		%% Process all inline events with a corresponding probe.
		[{_, probe, Filters, Route, inline}] ->
			maybe_send(Ref, Event, Route, Filters);
		%% Process all process events called from the right pid.
		[{_, probe, Filters, Route, {process, _, _, Pid}}] when Pid =:= FromPid ->
			maybe_send(Ref, Event, Route, Filters)
		%% Crash on everything else.
	end.

-spec relay(ref(), ref(), event()) -> ok.
relay(RelayRef, Ref, Event) ->
	case ets:lookup(?TAB, RelayRef) of
		%% Drop events with no corresponding relay.
		[] ->
			ok;
		%% Process events with corresponding relay unconditionally.
		[{_, relay, _, _, Route}] ->
			send(Ref, Event, Route)
		%% Crash on everything else.
	end.

maybe_send(Ref, Event, Route, Filters) ->
	case filter(Filters, Event) of
		ok ->
			send(Ref, Event, Route)
%		drop ->
%			ok
	end.

filter([], _) ->
	ok.
%filter([Filter|Tail], Event) ->
%	case Filter(Event) of
%		ok ->
%			filter(Tail, Event);
%		drop ->
%			drop
%	end.

send(Ref, Event, {msg, Dest}) ->
	send_msg(Dest, {?MODULE, Ref, Event});
send(Ref, Event, {msg, Node, LocalName}) ->
	send_msg({LocalName, Node}, {?MODULE, Ref, Event});
send(_, Event, {udp, Host, Port, ID}) ->
	case gen_udp:open(0) of
		{ok, Socket} ->
			try
				BinEvent = term_to_binary(Event, [compressed]),
				Packet = << ID:16, BinEvent/binary >>,
				_ = gen_udp:send(Socket, Host, Port, Packet),
				ok
			after
				gen_udp:close(Socket)
			end;
		_ ->
			ok
	end.
%send(_Ref, _Event, _Route={tcp, _, _, _}) ->
%	todo;
%send(_Ref, _Event, _Route={ssl, _, _, _}) ->
%	todo.

send_msg(Dest, Msg) ->
	try erlang:send(Dest, Msg, [noconnect]) of
		noconnect ->
			spawn(erlang, send, [Dest, Msg]);
		_ -> ok
	catch _:_ ->
		ok
	end.
