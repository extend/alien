%% Feel free to use, reuse and abuse the code in this file.

-module(node_route_node).

-export([start/1]).
-export([init/1]).
-export([loop/1]).

start(DestPid) ->
	spawn(?MODULE, init, [DestPid]),
	ok.

init(DestPid) ->
	register(node_route_node_local_name, self()),
	DestPid ! remote_ready,
	loop(DestPid).

loop(DestPid) ->
	receive
		Msg ->
			DestPid ! {remote, Msg},
			loop(DestPid)
	end.
