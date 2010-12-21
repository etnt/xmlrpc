-module(test).
-author('jocke@gleipnir.com').
-export([decode/2]).

decode(State, _Payload) ->
	log4erl:debug("TEST"),
	{true, 60000, State, "OK"}.