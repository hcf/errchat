-module(errchat_server).

-behaviour(gen_server).


%% API.
-export([start_link/0]).
-export([start/0]).

%% errchat server api

-record(state, {connections = []}).

-export([register/1]).
-export([unregister/1]).
-export([me/2]).
-export([new_message/2]).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(SERVER, ?MODULE).

%% Used during development
start() ->
	ok = application:start(compiler),
	ok = application:start(syntax_tools),
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(lager),
	ok = application:start(errchat_server).

start_link() ->

	lager:debug("Starting errchat_server link"),

	Dispatch = cowboy_router:compile([
        {'_', [
            {'_', communication_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_http(http, 100, 
        [{port, 10100}],
        [{env, [{dispatch, Dispatch}]}]),

    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% chat server api
%% Uses the generic servers cast ability in order to use the same processing logic

register(Pid) -> 
	lager:debug("register"),

	gen_server:cast(?SERVER, {register, Pid}).

unregister(Pid) ->
	lager:debug("unregister"),

	gen_server:cast(?SERVER, {unregister, Pid}).

me(Pid, Message) -> 
	lager:debug("me"),

	gen_server:cast(?SERVER, {me, Pid, Message}).

new_message(Pid, Message) ->
	lager:debug("new_message"),

	gen_server:cast(?SERVER, {new_message, Pid, Message}).


%% gen_server
init([]) -> 
	lager:debug("Starting chat server"),
	{ok, #state{}}.

%% Not really interested in synchronous calls, just ACKING
handle_call(_Request, _From, State) ->
	lager:debug("Synchronous call received, just acking"),
	{reply, ok, State}.

%% Handles asynchronous calls, for our defined api (register, unregister, new_message), and
%% catches all other data and logs.
handle_cast({register, Pid}, #state{connections = Connections} = State) ->
	lager:debug("cast register"),

	Connections2 = [{Pid, no_name} | Connections],
	State2 = State#state{connections = Connections2},
	{noreply, State2};

handle_cast({me, Pid, Message}, #state{connections = Connections} = State) ->
	lager:debug("cast me"),

	%Connections2 = lists:keyreplace(Pid, 1, Connections, Message),
	%State2 = State#state{connections = Connections2},
	{noreply, State};

handle_cast({unregister, Pid}, #state{connections = Connections} = State) ->
	lager:debug("cast unregister"),

	Connections2 = lists:keydelete(Pid, 1, Connections),
	State2 = State#state{connections = Connections2},
	{noreply, State2};

handle_cast({new_message, Pid, Message}, #state{connections = Connections} = State) ->
	lager:debug("cast new_message"),

	[ Cid ! {new_message, Pid, Message} || { Cid, _} <- Connections],
	{noreply, State};

handle_cast(_Data, State) ->
	lager:debug("Received unknown cast message"),
	{noreply, State}.

handle_info(_Info, State) ->
	lager:debug("Received info message"),
	{noreply, State}.

%% Terminating connection
terminate(_Reason, _State) ->
	lager:debug("Terminating"),
	ok.

%% Handling code change
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.