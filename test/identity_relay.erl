%% Feel free to use, reuse and abuse the code in this file.

-module(identity_relay).
-behaviour(alien_relay).

-export([start_link/2]).
-export([loop/1]).

start_link(RelayRef, _Opts = []) ->
	{ok, spawn_link(?MODULE, loop, [RelayRef])}.

loop(RelayRef) ->
	receive
		{alien, Ref, Event} ->
			alien:relay(RelayRef, Ref, Event),
			loop(RelayRef)
	end.
