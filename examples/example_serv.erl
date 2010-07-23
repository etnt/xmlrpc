-module(example_serv).
-author('jocke@gleipnir.com').
-export([start_link/2]).
-export([foo/1, bar/3]).
-export([init/2]).

-record(state, {}).

%%
%% API
%%

%% Exported: start_link/2

start_link(Args, Timeout) ->
    Pid = spawn_link(?MODULE, init, [self(), Args]),
    receive
	{Pid, started} -> {ok, Pid};
	{Pid, Reason} -> Reason
    after Timeout -> {error, timeout}
    end.

%% Exported: foo/1

foo(Pid) -> foo ! Pid.

%% Exported: bar/3

bar(Pid, Timeout, Message) ->
    Pid !!  


    {bar, Message, self()} ! Pid,
    receive
	{Pid, Result} -> Result
    after Timeout -> {error, timeout}
    end.

%%
%% Server
%%

%% Exported: init/2

init(Parent, Args) ->
    Parent ! {self(), started},
    loop(#state{}).

loop(State) ->
    receive
	stop ->
	    stop;
	
	{bar, Message, From} ->
	    {self(), Message},
	    loop(State);
	UnknownMessage ->
	    io:format("Unknown message: ~p~n", [UnknownMessage]),
	    loop(State)
    end.
