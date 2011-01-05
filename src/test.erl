-module(test).
-author('jocke@gleipnir.com').
-export([decode/2]).

decode(State, Payload) ->
	log4erl:debug("TEST: Payload received ~p", [Payload]),
	P = {response, [102012]},
	log4erl:debug("TEST: Encoded Response ~p", [P]),
	{true, 60000, State, P}.