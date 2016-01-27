%% -*- coding: latin-1 -*-
%% Copyright (C) 2003 Joakim Grebenö <jocke@tail-f.com>.
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

%% @author Joakim Grebenö <jocke@tail-f.com>
%% @author Torbjörn Törnkvist <etnt@redhoterlang.com>
%% @copyright 2003 Joakim Grebenö

%% TODO: Document the callback state functions
%% TODO: SOAP
%% TODO: Integration into either the inets, jnets or yaws web server.
%% TODO: Implementation of the function inspection protocol.

-module(xmlrpc).

-export([call/3, call/4, call/5, call/6]).
-export([start_link/1, start_link/5, start_link/6, start_link/7, stop/1]).

-export([ssl_call/3, ssl_call/4, ssl_call/5, ssl_call/6]).

-export([cbs_new/0, cbs_record/1,
	 cbs_ip/1, cbs_ip/2, 
	 cbs_port/1, cbs_port/2, 
	 cbs_opaque/1, cbs_opaque/2
	]).


-include("log.hrl").

-record(header, {
	  %% int()
	  content_length,
	  %% close | undefined
	  connection
	 }).

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


%% Type definitions

-type socket() :: term().

-type ip_address() :: inet:ip_address().

-type host() :: string() | ip_address().

-type iso8601date() :: {date, string()}.

-type base64() :: {base64, string()}.

-type uri() :: string().

-type xmlrpc_value() ::  integer() | float() | string() | boolean()
  | iso8601date() | base64() | xmlrpc_struct() | xmlrpc_array().

-type xmlrpc_struct() :: {struct, [{Key::atom(), Value::xmlrpc_value()}]}.

-type xmlrpc_array() :: {array, [xmlrpc_value()]}.

-type response_payload() :: {response, [xmlrpc_value()]}
                          | {response, {fault, Code::integer(),
                                        Message::string()}}.

-type call_result() :: {ok, response_payload()}
                     | {ok, socket(), response_payload()}
                     | {error, Reason::term()}
                     | {error, socket(), Reason::term()}.


%%%
%%% Quick and dirty solution for adding SSL support.
%%%
-define(SSL, ssl).

-spec ssl_call(Socket, URI, Payload) -> call_result()
 when Socket :: socket(),
      URI :: uri(),
      Payload :: {call, Method::atom(), Arguments::[xmlrpc_value()]}.

%% @equiv ssl_call(Socket, URI, Payload, false, 60000)
%% @see ssl_call/5

ssl_call(Socket, URI, Payload) ->
    put(proto, ?SSL),
    call(Socket, URI, Payload).

-spec ssl_call(Host, Port, URI, Payload) -> call_result()
 when Host :: host(),
      Port :: integer(),
      URI :: uri(),
      Payload :: {call, Method::atom(), Arguments::[xmlrpc_value()]}.

%% @equiv ssl_call(Host, Port, URI, Payload, false, 60000)
%% @see ssl_call/6

ssl_call(Host, Port, URI, Payload) ->
    put(proto, ?SSL),
    call(Host, Port, URI, Payload).

-spec ssl_call(Socket, URI, Payload, KeepAlive, Timeout) -> call_result()
 when Socket :: socket(),
      URI :: uri(),
      Payload :: {call, Method::atom(), Arguments::[xmlrpc_value()]},
      KeepAlive :: boolean(),
      Timeout :: integer().

%% @doc Calls an XML-RPC server on an open SSL connection.
%% @see ssl_call/6

ssl_call(Socket, URI, Payload, KeepAlive, Timeout) ->
    put(proto, ?SSL),
    call(Socket, URI, Payload, KeepAlive, Timeout).

-spec ssl_call(Host, Port, URI, Payload, KeepAlive, Timeout) -> call_result()
 when Host :: host(),
      Port :: integer(),
      URI :: uri(),
      Payload :: {call, Method::atom(), Arguments::[xmlrpc_value()]},
      KeepAlive :: boolean(),
      Timeout :: integer().

%% @doc Calls an SSL XML-RPC server listening on `Host:Port' over.
%% @see call/6

ssl_call(Host, Port, URI, Payload, KeepAlive, Timeout) ->
    put(proto, ?SSL),
    call(Host, Port, URI, Payload, KeepAlive, Timeout).



%% Exported: call/{3,4,5,6}

-spec call(Host, Port, URI, Payload) -> call_result()
 when Host :: host(),
      Port :: integer(),
      URI :: uri(),
      Payload :: {call, Method::atom(), Arguments::[xmlrpc_value()]}.

%% @equiv call(Host, Port, URI, Payload, false, 60000)

call(Host, Port, URI, Payload) ->
    call(Host, Port, URI, Payload, false, 60000).

-spec call(Host, Port, URI, Payload, KeepAlive, Timeout) -> call_result()
 when Host :: host(),
      Port :: integer(),
      URI :: uri(),
      Payload :: {call, Method::atom(), Arguments::[xmlrpc_value()]},
      KeepAlive :: boolean(),
      Timeout :: integer().

%% @doc Calls an XML-RPC server listening on `Host:Port'. The arguments
%% `URI' and `Payload' are used in the HTTP POST request sent to the server.
%% The `Arguments' are converted to XML for the request body.
%%
%% If `KeepAlive' is `true', a socket is returned. The socket can be used
%% to send several calls on the same connection in accordance with HTTP 1.1.
%% If no server response is received within `Timeout' milliseconds
%% `{error, timeout}' or `{error, Socket, timeout}' is returned.
%%
%% `KeepAlive' and `Timeout' default to `false' and `60000' milliseconds,
%% respectively.
%%
%% @see ssl_call/6

call(Host, Port, URI, Payload, KeepAlive, Timeout) ->
    case connect(Host, Port, [{active, false}]) of
	{ok, Socket} -> call(Socket, {Host,URI}, Payload, KeepAlive, Timeout);
	{error, Reason} when KeepAlive == false -> {error, Reason};
	{error, Reason} -> {error, undefined, Reason}
    end.

-spec call(Socket, URI, Payload) -> call_result()
 when Socket :: socket(),
      URI :: uri(),
      Payload :: {call, Method::atom(), Arguments::[xmlrpc_value()]}.

%% @equiv call(Socket, URI, Payload, false, 60000)

call(Socket, URI, Payload) ->
    call(Socket, URI, Payload, false, 60000).

-spec call(Socket, URI, Payload, KeepAlive, Timeout) -> call_result()
 when Socket :: socket(),
      URI :: uri() | {host(), uri()},
      Payload :: {call, Method::atom(), Arguments::[xmlrpc_value()]},
      KeepAlive :: boolean(),
      Timeout :: integer().

%% @doc Calls an XML-RPC server on an open connection.
%% @see call/6

call(Socket, URI, Payload, KeepAlive, Timeout) ->
    ?DEBUG_LOG({decoded_call, Payload}),
    case xmlrpc_encode:payload(Payload) of
	{ok, EncodedPayload} ->
	    ?DEBUG_LOG({encoded_call, EncodedPayload}),
	    case send(Socket, URI, KeepAlive, EncodedPayload) of
		ok ->
		    case parse_response(Socket, Timeout) of
			{ok, Header} ->
			    handle_payload(Socket, KeepAlive, Timeout, Header);
			{error, Reason} when KeepAlive == false ->
			    close(Socket),
			    {error, Reason};
			{error, Reason} -> {error, Socket, Reason}
		    end;
		{error, Reason} when KeepAlive == false ->
		    close(Socket),
		    {error, Reason};
		{error, Reason} ->
		    {error, Socket, Reason}
	    end;
	{error, Reason} when KeepAlive == false ->
	    close(Socket),
	    {error, Reason};
	{error, Reason} -> {error, Socket, Reason}
    end.

send(Socket, URI, false, Payload) ->
    send(Socket, URI, "Connection: close\r\n", Payload);
send(Socket, URI, true, Payload) -> send(Socket, URI, "", Payload);
send(Socket, {Host,URI}, Header, Payload) ->
    Request =
	["POST ", URI, " HTTP/1.1\r\n",
	 "Content-Length: ", integer_to_list(lists:flatlength(Payload)),
	 "\r\n",
	 "User-Agent: Erlang XML-RPC Client 1.13\r\n",
	 "Content-Type: text/xml\r\n",
	 "Host: ", Host, "\r\n",
	 Header, "\r\n",
	 Payload],
    send(Socket, Request);
send(Socket, URI, Header, Payload) ->
    Request =
	["POST ", URI, " HTTP/1.1\r\n",
	 "Content-Length: ", integer_to_list(lists:flatlength(Payload)),
	 "\r\n",
	 "User-Agent: Erlang XML-RPC Client 1.13\r\n",
	 "Content-Type: text/XML\r\n",
	 Header, "\r\n",
	 Payload],
    send(Socket, Request).

parse_response(Socket, Timeout) ->
    setopts(Socket, [{packet, line}]),
    case recv(Socket, 0, Timeout) of
	{ok, "HTTP/1.1 200 OK\r\n"} -> parse_header(Socket, Timeout);
	{ok, StatusLine} -> {error, StatusLine};
	{error, Reason} -> {error, Reason}
    end.

parse_header(Socket, Timeout) -> parse_header(Socket, Timeout, #header{}).

parse_header(Socket, Timeout, Header) ->
    case recv(Socket, 0, Timeout) of
	{ok, "\r\n"} when Header#header.content_length == undefined ->
	    {error, missing_content_length};
	{ok, "\r\n"} -> {ok, Header};
	{ok, HeaderField} ->
	    case string:tokens(string:to_lower(HeaderField), " \r\n") of
		["content-length:", ContentLength] ->
		    try
			Value = list_to_integer(ContentLength),
			parse_header(Socket, Timeout,
				     Header#header{content_length = Value})
		    catch
			_ -> {error, {invalid_content_length, ContentLength}}
		    end;
		["connection:", "close"] ->
		    parse_header(Socket, Timeout,
				 Header#header{connection = close});
		_ ->
		    parse_header(Socket, Timeout, Header)
	    end;
	{error, Reason} -> {error, Reason}
    end.

handle_payload(Socket, KeepAlive, Timeout, Header) ->
    case get_payload(Socket, Timeout, Header#header.content_length) of
	{ok, Payload} ->
	    ?DEBUG_LOG({encoded_response, Payload}),
	    case xmlrpc_decode:payload(Payload) of
		{ok, DecodedPayload} when KeepAlive == false ->
		    ?DEBUG_LOG({decoded_response, DecodedPayload}),
		    close(Socket),
		    {ok, DecodedPayload};
		{ok, DecodedPayload} when KeepAlive == true,
					  Header#header.connection == close ->
		    ?DEBUG_LOG({decoded_response, DecodedPayload}),
		    close(Socket),
		    {ok, Socket, DecodedPayload};
		{ok, DecodedPayload} ->
		    ?DEBUG_LOG({decoded_response, DecodedPayload}),
		    {ok, Socket, DecodedPayload};
		{error, Reason} when KeepAlive == false ->
		    close(Socket),
		    {error, Reason};
		{error, Reason} when KeepAlive == true,
				     Header#header.connection == close ->
		    close(Socket),
		    {error, Socket, Reason};
		{error, Reason} ->
		    {error, Socket, Reason}
	    end;
	{error, Reason} when KeepAlive == false ->
	    close(Socket),
	    {error, Reason};
	{error, Reason} when KeepAlive == true,
			     Header#header.connection == close ->
	    close(Socket),
	    {error, Socket, Reason};
	{error, Reason} -> {error, Socket, Reason}
    end.

get_payload(Socket, Timeout, ContentLength) ->
    setopts(Socket, [{packet, raw}]),
    recv(Socket, ContentLength, Timeout).

-define(TIMEOUT, 6000).

%% Exported: start_link/{1,5,6}

-spec start_link(Handler) -> {ok, pid()} | {error, Reason::term()}
 when Handler :: {Module::atom(), Function::atom()}.

%% @equiv start_link(4567, 1000, 60000, Handler, undefined)

start_link(Handler) ->
    start_link(4567, 1000, ?TIMEOUT, Handler, undefined).

-spec start_link(Port, MaxSessions, Timeout, Handler, State) ->
          {ok, pid()} | {error, Reason::term()}
 when Port :: integer(),
      MaxSessions :: integer(),
      Timeout :: integer(),
      Handler :: {Module::atom(), Function::atom()},
      State :: term().

%% @equiv start_link(all, Port, MaxSessions, Timeout, Handler, State)

start_link(Port, MaxSessions, Timeout, Handler, State) ->
    start_link(all, Port, MaxSessions, Timeout, Handler, State).

-spec start_link(IP, Port, MaxSessions, Timeout, Handler, State) ->
          {ok, pid()} | {error, Reason::term()}
 when IP :: ip_address() | all,
      Port :: integer(),
      MaxSessions :: integer(),
      Timeout :: integer(),
      Handler :: {Module::atom(), Function::atom()},
      State :: term().

%% @doc Starts an XML-RPC server listening on `IP:Port'. If no IP address is
%% given, the server listens on `Port' for all available IP addresses.
%% `MaxSessions' is used to restrict the number of concurrent connections.
%% If `MaxSessions' is reached, the server accepts no new connections for 5
%% seconds, i.e., blocks new connect attempts.
%%
%% `Handler' is a callback, implemented by `Module:Function/2', which is
%% used to instantiate an XML-RPC server. The `Timeout' value is used if the
%% handler is keepalive-oriented. `State' is the initial state given to
%% `Module:Function/2'. The resulting pid can be used as input to {@link
%% xmlrpc:stop/1}.

start_link(IP, Port, MaxSessions, Timeout, Handler, State) ->
    start_link(undefined, IP, Port, MaxSessions, Timeout, Handler, State).

start_link(Register, IP, Port, MaxSessions, Timeout, Handler, State) ->
    OptionList = [{active, false}, {reuseaddr, true}|ip(IP)],
    SessionHandler = {xmlrpc_http, handler, [Timeout, Handler, State]}, 
    xmlrpc_tcp_serv:start_link([Port, MaxSessions, OptionList, SessionHandler],
                        ?TIMEOUT, Register).

ip(all) -> [];
ip(IP) when is_tuple(IP) -> [{ip, IP}].

%% Exported: stop/1

-spec stop(Pid::pid()) -> ok.

%% @doc Stops a running XML-RPC server.

stop(Pid) -> xmlrpc_tcp_serv:stop(Pid).


%%%
%%% Switch on bearer protocol to be used
%%%
connect(Host, Port, Opts) ->
    connect(get(proto), Host, Port, Opts).

connect(?SSL, Host, Port, Opts) -> ssl:connect(Host, Port, Opts);
connect(_,    Host, Port, Opts) -> gen_tcp:connect(Host, Port, Opts).


close(Socket) ->
    close(get(proto), Socket).

close(?SSL, Socket) -> ssl:close(Socket);
close(_   , Socket) -> gen_tcp:close(Socket).


send(Socket, Request) ->
    send(get(proto), Socket, Request).

send(?SSL, Socket, Request) -> ssl:send(Socket, Request);
send(_   , Socket, Request) -> gen_tcp:send(Socket, Request).


recv(Socket, Length, Timeout) ->
    recv(get(proto), Socket, Length, Timeout).

recv(?SSL, Socket, Length, Timeout) -> ssl:recv(Socket, Length, Timeout);
recv(_   , Socket, Length, Timeout) -> gen_tcp:recv(Socket, Length, Timeout).



setopts(Socket, Opts) ->
    setopts(get(proto), Socket, Opts).

setopts(?SSL, Socket, Opts) -> ssl:setopts(Socket, Opts);
setopts(_,    Socket, Opts) -> inet:setopts(Socket, Opts).




