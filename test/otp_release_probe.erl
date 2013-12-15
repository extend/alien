%% Feel free to use, reuse and abuse the code in this file.

-module(otp_release_probe).
-behaviour(alien_ondemand_probe).

-export([probe/1]).

probe(_) ->
	erlang:system_info(otp_release).
