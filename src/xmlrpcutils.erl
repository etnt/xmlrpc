%%
%% Copyright (c) 2009 John Wright <john@dryfish.org>
%%
%% Permission to use, copy, modify, and distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%

-module(xmlrpcutils).
-author(john@dryfish.org).

-export([get_member/2, new_server/3, call/3]).

% Deep struct/array helper.
% eg. get_member(Response, [one, 2, three]).

get_member(Data, []) ->
    Data;
get_member({struct, Struct}, [Head | Tail]) ->
    get_member(element(2, lists:keyfind(Head, 1, Struct)), Tail);
get_member({array, Array}, [Head | Tail]) ->
    get_member(lists:nth(Head, Array), Tail);
get_member({response, [Response]}, Addr) ->
    get_member(Response, Addr).

% Keep alive helper.

-record(aServer, {host, port, path, sock}).

new_server(Host, Port, Path) ->
    #aServer{host=Host, port=Port, path=Path, sock=0}.

call_result(Server, _Call, {ok, Socket, Response}) ->
    {ok, Server#aServer{sock=Socket}, Response};
call_result(Server, _Call, {ok, Response}) ->
    {ok, Server#aServer{sock=0}, Response};
call_result(Server, Call, {error, _Socket, closed}) ->
    call(Server#aServer{sock=0}, Call);
call_result(Server, _Call, {error, Message}) ->
    {error, Server, Message}.

call(Server, Method, Args) ->
    call(Server, {call, Method, Args}).

call(Server, Call) ->
    case Server#aServer.sock of
        0 ->
            call_result(Server, Call, xmlrpc:call(
                    Server#aServer.host, 
                    Server#aServer.port, 
                    Server#aServer.path, 
                    Call,
                    true,
                    2000));
        Sock ->
            call_result(Server, Call, xmlrpc:call(
                    Sock,
                    Server#aServer.path, 
                    Call,
                    true,
                    1000))
    end.
