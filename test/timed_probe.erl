%% Feel free to use, reuse and abuse the code in this file.

-module(timed_probe).
-behaviour(alien_process_probe).

-export([start_link/2]).

start_link(Ref, _Opts = []) ->
	{ok, spawn_link(fun() ->
		%% Wait before the first event because the first few events might
		%% get discarded while the probe initializes.
		receive after 100 -> ok end,
		alien:event(Ref, 0),
		receive after 100 -> ok end,
		alien:event(Ref, 1),
		receive after 100 -> ok end,
		alien:event(Ref, 2),
		receive after 100 -> ok end,
		alien:event(Ref, 3),
		receive after infinity -> ok end
	end)}.
