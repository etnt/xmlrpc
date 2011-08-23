%% Copyright (C) 2003 Joakim Grebenö <jocke@gleipnir.com>.
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met: 
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer. 
%% 2. Redistributions in binary form must reproduce the above
%%    copyright notice, this list of conditions and the following
%%    disclaimer in the documentation and/or other materials provided
%%    with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
%% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(xmlrpc).
-author('palfrey@tevp.net').
-export([call/3]).

-export([cbs_new/0, cbs_record/1,
	 cbs_ip/1, cbs_ip/2, 
	 cbs_port/1, cbs_port/2, 
	 cbs_opaque/1, cbs_opaque/2
	]).

%% Exported: call/3

call(Host, URI, Payload) ->
	inets:start(),
    case xmlrpc_encode:payload(Payload) of
	{ok, EncodedPayload} ->
		case http:request(post, {"http://"++Host++URI, [], "text/xml", lists:flatten(EncodedPayload)}, [], []) of
		{ok, {{_, 200, _}, _, Body}} ->
			case xmlrpc_decode:payload(Body) of
			{ok, {response, {fault, Code, String}}} -> 
				{error, {Code, String}};
			{ok, {response, [Data]}} ->
				QuoteData = re:replace(Data, "\\\\\"", "\"", [{return, list},global]),
				{ok, xmerl_scan:string(QuoteData)};
			{error, Reason} ->
				{error, Reason}
			end;
		{ok, StatusLine} -> {error, StatusLine}
		end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%% This record can be used by the client to be able to
%%% retrieve internal info (such as Ip,Port) and at the
%%% same time preserve the clients own state (opaque).
%%% NB: No include file is provided, use the access functions!
-record(cback_state, {
	  ip,
	  port,
	  opaque
	 }).

cbs_record(#cback_state{}) -> true;
cbs_record(_)              -> false.

cbs_new() -> 
    #cback_state{}.

cbs_ip(#cback_state{}=C) ->
    C#cback_state.ip.
cbs_ip(#cback_state{}=C, Ip) ->
    C#cback_state{ip = Ip}.

cbs_port(#cback_state{}=C) ->
    C#cback_state.port.
cbs_port(#cback_state{}=C, Port) ->
    C#cback_state{port = Port}.

cbs_opaque(#cback_state{}=C) ->
    C#cback_state.opaque.
cbs_opaque(#cback_state{}=C, Opaque) ->
    C#cback_state{opaque = Opaque}.
