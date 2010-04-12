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
-export([call/2]).

%% Exported: call/2

call(URL, Payload) ->
	inets:start(),
    case xmlrpc_encode:payload(Payload) of
	{ok, EncodedPayload} ->
		case http:request(post, {URL, [], "text/xml", lists:flatten(EncodedPayload)}, [], []) of
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
