-module(test).
-author('jocke@gleipnir.com').
-export([decode/2]).

decode(State, Payload) ->
	error_logger:info_msg("TEST: Payload received ~p", [Payload]),
	P = {response, [102012]},
	error_logger:info_msg("TEST: Encoded Response ~p", [P]),
	{true, 60000, State, P}.