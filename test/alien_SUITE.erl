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

-module(alien_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% alien.
-export([inline/1]).
-export([ondemand/1]).
-export([process/1]).
-export([relay/1]).
-export([node_route/1]).
-export([udp_route/1]).

%% @todo Test filters and the other route types.

%% ct.

all() ->
	[inline, ondemand, process, relay, node_route, udp_route].

init_per_suite(Config) ->
	ok = application:start(alien),
	Config.

end_per_suite(_) ->
	application:stop(alien),
	ok.

%% alien.

inline(_) ->
	Ref = inline,
	Self = self(),
	ok = alien:start_probe(Ref, [], {msg, Self}, inline),
	[{Ref, probe, [], {msg, Self}, inline}] = alien:list_probes(),
	Event1 = {my_other_event, lists:seq(1, 100)},
	Event2 = {my_event, 42, <<"some message">>},
	Event3 = <<"just a message">>,
	alien:event(Ref, Event1),
	alien:event(Ref, Event2),
	alien:event(Ref, Event3),
	[{alien, Ref, Event1}, {alien, Ref, Event2}, {alien, Ref, Event3}] = [
		receive Msg -> Msg after 500 -> error(timeout) end
		|| _ <- [1, 2, 3]],
	alien:stop_probe(Ref),
	[] = alien:list_probes(),
	ok.

%% @todo inline filters

ondemand(_) ->
	Ref = ondemand,
	Self = self(),
	OTPRelease = erlang:system_info(otp_release),
	alien:probe(Ref, otp_release_probe, {msg, Self}),
	receive {alien, Ref, OTPRelease} ->
		ok
	after 500 ->
		error(timeout)
	end,
	ok.

process(_) ->
	Ref = process,
	Self = self(),
	ok = alien:start_probe(Ref, [], {msg, Self}, {process, timed_probe, []}),
	[{Ref, probe, [], {msg, Self}, {process, timed_probe, [], Pid}}]
		= alien:list_probes(),
	true = is_process_alive(Pid),
	[{alien, Ref, 0}, {alien, Ref, 1}, {alien, Ref, 2}, {alien, Ref, 3}] = [
		receive Msg -> Msg after 500 -> error(timeout) end
		|| _ <- [1, 2, 3, 4]],
	alien:stop_probe(Ref),
	[] = alien:list_probes(),
	false = is_process_alive(Pid),
	ok.

relay(_) ->
	Ref = relay,
	RelayRef = relay_relay,
	Self = self(),
	{ok, Pid} = alien:start_relay(RelayRef, identity_relay, [], {msg, Self}),
	[{RelayRef, relay, identity_relay, [], {msg, Self}}]
		= alien:list_relays(),
	true = is_process_alive(Pid),
	ok = alien:start_probe(Ref, [], {msg, Pid}, inline),
	[{Ref, probe, [], {msg, Pid}, inline}]
		= alien:list_probes(),
	Event1 = {my_other_event, lists:seq(1, 100)},
	Event2 = {my_event, 42, <<"some message">>},
	Event3 = <<"just a message">>,
	alien:event(Ref, Event1),
	alien:event(Ref, Event2),
	alien:event(Ref, Event3),
	[{alien, Ref, Event1}, {alien, Ref, Event2}, {alien, Ref, Event3}] = [
		receive Msg -> Msg after 500 -> error(timeout) end
		|| _ <- [1, 2, 3]],
	alien:stop_probe(Ref),
	[] = alien:list_probes(),
	true = is_process_alive(Pid),
	alien:stop_relay(RelayRef),
	[] = alien:list_relays(),
	false = is_process_alive(Pid),
	ok.

node_route(_C) ->
	Ref = node_route,
	Node = node_route_node,
	%% The node needs to be able to load this module for running the funs.
	{ok, NodeName} = ct_slave:start(Node, [{erl_flags, "-pa "
		++ filename:absname(filename:dirname(code:which(?MODULE)))}]),
	%% Create a process that will forward all messages to us
	%% wrapped in a tuple so we know it reached the destination.
	ok = rpc:call(NodeName, node_route_node, start, [self()]),
	receive remote_ready -> ok after 5000 -> error(remote_timeout) end,
	ok = alien:start_probe(Ref, [],
		{msg, NodeName, node_route_node_local_name}, inline),
	Event1 = {my_other_event, lists:seq(1, 100)},
	Event2 = {my_event, 42, <<"some message">>},
	Event3 = <<"just a message">>,
	alien:event(Ref, Event1),
	alien:event(Ref, Event2),
	alien:event(Ref, Event3),
	[
		{remote, {alien, Ref, Event1}},
		{remote, {alien, Ref, Event2}},
		{remote, {alien, Ref, Event3}}
	] = [
		receive Msg -> Msg after 500 -> error(timeout) end
		|| _ <- [1, 2, 3]],
	alien:stop_probe(Ref),
	{ok, NodeName} = ct_slave:stop(Node),
	ok.

udp_route(_) ->
	Ref = udp_route,
	ID = 12345,
	{ok, Socket} = gen_udp:open(0, [binary, {active, true}]),
	{ok, Port} = inet:port(Socket),
	ok = alien:start_probe(Ref, [], {udp, "127.0.0.1", Port, ID}, inline),
	Event1 = {my_other_event, lists:seq(1, 100)},
	Event2 = {my_event, 42, <<"some message">>},
	Event3 = <<"just a message">>,
	alien:event(Ref, Event1),
	alien:event(Ref, Event2),
	alien:event(Ref, Event3),
	[
		{udp, Socket, _, _, << ID:16, Packet1/binary >>},
		{udp, Socket, _, _, << ID:16, Packet2/binary >>},
		{udp, Socket, _, _, << ID:16, Packet3/binary >>}
	] = [
		receive Msg -> Msg after 500 -> error(timeout) end
		|| _ <- [1, 2, 3]],
	[Event1, Event2, Event3] = lists:sort([
		binary_to_term(Packet1),
		binary_to_term(Packet2),
		binary_to_term(Packet3)
	]),
	alien:stop_probe(Ref),
	gen_udp:close(Socket),
	ok.
