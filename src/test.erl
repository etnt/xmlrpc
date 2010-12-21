-module(test).
-author('jocke@gleipnir.com').
-export([decode/2]).

decode(_Payload, _State) ->
	log4erl:debug("TEST"),
	ok.