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

-module(xmlrpc_http).
-author('jocke@gleipnir.com').
-export([handler/4, split_header_field/1]).

-include("log.hrl").

-record(header, {
	  %% int()
	  content_length,
	  %% string()
	  content_type,
	  %% string()
	  user_agent,
	  %% close | undefined
	  connection
	 }).

%% Exported: handler/3

handler(Socket, Timeout, Handler, State) ->
	log4erl:debug("XMLRPC: http handler ~p", [Handler]),	
    case parse_request(Socket, Timeout) of
	{ok, Header} ->
	    ?DEBUG_LOG({header, Header}),
		log4erl:debug("XMLRPC: .. Header ~p", [Header]),	
	    handle_payload(Socket, Timeout, Handler, State, Header);
	{status, StatusCode} ->
		log4erl:debug("XMLRPC: .. Status ~p", [StatusCode]),	
   	    send(Socket, StatusCode),
	    handler(Socket, Timeout, Handler, State);
	{error, Reason} -> 
		log4erl:debug("XMLRPC: .. Error ~p", [Reason]),	
		{error, Reason}
    end.

parse_request(Socket, Timeout) ->
    inet:setopts(Socket, [{packet, line}]),
	log4erl:debug("XMLRPC: http parse_request ~p", [Socket]),	
    case gen_tcp:recv(Socket, 0, Timeout) of
	{ok, RequestLine} ->
	    case string:tokens(RequestLine, " \r\n") of
		["POST", _, "HTTP/1.0"] ->
		    ?DEBUG_LOG({http_version, "1.0"}),
			log4erl:debug("XMLRPC: HTTP 1.0"),	
		    parse_header(Socket, Timeout, #header{connection = close});
		["POST", _, "HTTP/1.1"] ->
		    ?DEBUG_LOG({http_version, "1.1"}),
			log4erl:debug("XMLRPC: HTTP 1.1"),	
		    parse_header(Socket, Timeout);
		[_UnknownMethod, _, "HTTP/1.1"] -> {status, 501};
		["POST", _, _UnknownVersion] -> {status, 505};
		_ -> {status, 400}
	    end;
	{error, Reason} -> {error, Reason}
    end.

parse_header(Socket, Timeout) -> parse_header(Socket, Timeout, #header{}).

parse_header(Socket, Timeout, Header) ->
	log4erl:debug("XMLRPC: http parse header"),	
    case gen_tcp:recv(Socket, 0, Timeout) of
	{ok, "\r\n"} when Header#header.content_length == undefined ->
		log4erl:debug("XMLRPC: http status 411"),	
	    {status, 411};
	{ok, "\r\n"} when Header#header.content_type == undefined ->
		log4erl:debug("XMLRPC: http status 400"),	
	    {status, 400};
	{ok, "\r\n"} when Header#header.user_agent == undefined ->
		log4erl:debug("XMLRPC: http status 400"),	
	    {status, 400};
	{ok, "\r\n"} -> {ok, Header};
	{ok, HeaderField} ->
		LowerHeaderField=string:to_lower(HeaderField),
		log4erl:debug("XMLRPC: http header ok ~p", [LowerHeaderField]),
		Res=split_header_field(LowerHeaderField),
		log4erl:debug("XMLRPC: http header analyze ~p", [Res]),
		case Res of
		{"content-length:", ContentLength} ->
			log4erl:debug("XMLRPC: http header content length ~p", [ContentLength]),	
		    try
				N = list_to_integer(ContentLength),
			    parse_header(Socket, Timeout,
					 Header#header{content_length = N})
			catch
				_ -> {status, 400}
			end;
		{"content-type:", "text/xml"} ->
			log4erl:debug("XMLRPC: http header content type text/xml"),	
		    parse_header(Socket, Timeout,
				 Header#header{content_type = "text/xml"});
		{"content-type:", _UnknownContentType} -> 
			log4erl:debug("XMLRPC: http header content type unknown"),	
			{status, 415};
		{"user-agent:", UserAgent} ->
			log4erl:debug("XMLRPC: http header content type user agent ~p", [UserAgent]),	
		    parse_header(Socket, Timeout,
				 Header#header{user_agent = UserAgent});
		{"connection:", "close"} ->
			log4erl:debug("XMLRPC: http header connection close"),	
		    parse_header(Socket, Timeout,
				 Header#header{connection = close});
		{"connection:", [_,$e,$e,$p,$-,_,$l,$i,$v,$e]} ->
			log4erl:debug("XMLRPC: http header connection"),	
		    parse_header(Socket, Timeout,
				 Header#header{connection = undefined});
		_ ->
			log4erl:debug("XMLRPC: http header other"),				
		    ?DEBUG_LOG({skipped_header, HeaderField}),
		    parse_header(Socket, Timeout, Header)
	    end; 
	{error, Reason} -> 
		log4erl:debug("XMLRPC: http header error ~p", [Reason]),	
		{error, Reason}
    end.

split_header_field(HeaderField) -> split_header_field(HeaderField, []).

split_header_field([], Name) -> {Name, ""};
split_header_field([$ |Rest], Name) -> {lists:reverse(Name), Rest -- "\r\n"};
split_header_field([C|Rest], Name) -> split_header_field(Rest, [C|Name]).

handle_payload(Socket, Timeout, Handler, State,
	       #header{connection = Connection} = Header) ->
	log4erl:debug("XMLRPC: http handle payload"),	
    case get_payload(Socket, Timeout, Header#header.content_length) of
	{ok, Payload} ->
	    ?DEBUG_LOG({encoded_call, Payload}),
		log4erl:debug("XMLRPC: http call payload decoder"),
	    case xmlrpc_decode:payload(Payload) of
		{ok, DecodedPayload} ->
			log4erl:debug("XMLRPC: Decoded Payload ~p", [DecodedPayload]),
		    ?DEBUG_LOG({decoded_call, DecodedPayload}),
		    eval_payload(Socket, Timeout, Handler, State, Connection,
				 DecodedPayload);
		{error, Reason} when Connection == close ->
			log4erl:error("XMLRPC: Error Payload ~p", [Reason]),
   		    ?ERROR_LOG({xmlrpc_decode, payload, Payload, Reason}),
		    send(Socket, 400);
		{error, Reason} ->
			log4erl:error("XMLRPC: Error Payload ~p", [Reason]),
		    ?ERROR_LOG({xmlrpc_decode, payload, Payload, Reason}),
		    send(Socket, 400),
		    handler(Socket, Timeout, Handler, State)
	    end;
	{error, Reason} -> {error, Reason}
    end.

get_payload(Socket, Timeout, ContentLength) ->
    inet:setopts(Socket, [{packet, raw}]),
    Payload=gen_tcp:recv(Socket, ContentLength, Timeout),
	log4erl:debug("XMLRPC: get payload ~p", [Payload]),
	Payload.

eval_payload(Socket, Timeout, {M, F} = Handler, State, Connection, Payload) ->
    case catch M:F(State, Payload) of
	{'EXIT', Reason} when Connection == close ->
	    ?ERROR_LOG({M, F, {'EXIT', Reason}}),
	    send(Socket, 500, "Connection: close\r\n");
	{'EXIT', Reason} ->
	    ?ERROR_LOG({M, F, {'EXIT', Reason}}),
	    send(Socket, 500),
	    handler(Socket, Timeout, Handler, State);
	{error, Reason} when Connection == close ->
	    ?ERROR_LOG({M, F, Reason}),
	    send(Socket, 500, "Connection: close\r\n");
	{error, Reason} ->
	    ?ERROR_LOG({M, F, Reason}),
	    send(Socket, 500),
	    handler(Socket, Timeout, Handler, State);
	{false, ResponsePayload} ->
	    encode_send(Socket, 200, "Connection: close\r\n", ResponsePayload);
	{true, _NewTimeout, _NewState, ResponsePayload} when
	      Connection == close ->
	    encode_send(Socket, 200, "Connection: close\r\n", ResponsePayload);
	{true, NewTimeout, NewState, ResponsePayload} ->
	    encode_send(Socket, 200, "", ResponsePayload),
	    handler(Socket, NewTimeout, Handler, NewState)
    end.

encode_send(Socket, StatusCode, ExtraHeader, Payload) ->
    ?DEBUG_LOG({decoded_response, Payload}),
    case xmlrpc_encode:payload(Payload) of
	{ok, EncodedPayload} ->
	    ?DEBUG_LOG({encoded_response, lists:flatten(EncodedPayload)}),
	    send(Socket, StatusCode, ExtraHeader, EncodedPayload);
	{error, Reason} ->
	    ?ERROR_LOG({xmlrpc_encode, payload, Payload, Reason}),
	    send(Socket, 500)
    end.

send(Socket, StatusCode) -> send(Socket, StatusCode, "", "").

send(Socket, StatusCode, ExtraHeader) ->
    send(Socket, StatusCode, ExtraHeader, "").

send(Socket, StatusCode, ExtraHeader, Payload) ->
    Response =
	["HTTP/1.1 ", integer_to_list(StatusCode), " ",
	 reason_phrase(StatusCode), "\r\n",
	 "Content-Length: ", integer_to_list(lists:flatlength(Payload)),
	 "\r\n",
	 "Server: Erlang/1.13\r\n",
	 ExtraHeader, "\r\n",
	 Payload],
    gen_tcp:send(Socket, Response).

reason_phrase(200) -> "OK";
reason_phrase(400) -> "Bad Request";
reason_phrase(411) -> "Length required";
reason_phrase(415) -> "Unsupported Media Type";     
reason_phrase(500) -> "Internal Server Error";
reason_phrase(501) -> "Not Implemented";
reason_phrase(505) -> "HTTP Version not supported".
