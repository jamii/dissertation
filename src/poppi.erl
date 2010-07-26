-module(poppi).

-export([default/1, start/0, start/1, start_many/2, start_star/1]).

-behaviour(ctmc).
-export([init/1, events/1, handle_event/2, handle_interrupt/2]).

-include(poppi).

% event constants

-define(FORGET, 0.01).
-define(BUMP, 0.01).
-define(PULL, 0.01).
-define(PUSH, 0.01).
-define(CACHE, 0.01).

% api

default_rates() ->
    #rates{forget=0.01,
	   pull=0.01,
	   push=0.01,
	   cache=0.01}.

start() ->
    start(#poppi{peers=[], rates=default_rates()}).

start(Poppi) ->
    true = lists:all(fun is_pid/1, Poppi),
    ctmc:start(?MODULE, [Poppi]).

start_many(N, Poppi) ->
    Nodes = [start(Poppi) || _ <- lists:seq(1,N)],
    {ok, [Node || {ok, Node} <- Nodes]}.

start_star(N, Rates) ->
    {ok, Hub} = start(#poppi{peers=[], rates=Rates}),
    Spoke = #poppi{peers=[Hub], rates=Rates},
    {ok, Spokes} = start_many(N, Spoke),
    {ok, Hub, Spokes}.

% ctmc callbacks

init(#poppi{}=Poppi) ->
    Poppi.

events(#poppi{peers=Peers,
	      rates=#rates{forget=Forget,
			   pull=Pull,
			   push=Push,
			   cache=Cache}}) ->
    lists:flatten([
      [{{forget, Peer}, Forget} || Peer <- Peers],
      {cache, Cache},
      [{{bump, Peer}, Bump} || Peer <- Peers],
      [{{pull, Peer}, Pull} || Peer <- Peers],
      [{{push, Peer}, Push} || Peer <- Peers]]).

handle_event(#poppi{peers=Peers}=Poppi, {forget, Peer}) when is_pid(Peer) ->
    log("Forgetting ~w from ~w~n", [Peer, self()]),
    Poppi#poppi{peers=lists:delete(Peer,Peers)};

handle_event(Poppi, cache) ->
    log("Cache not implemented yet~n", []),
    Poppi;

handle_event(Poppi, {bump, Peer}) when is_pid(Peer) ->
    log("Sending bump from ~w to ~w~n", [self(), Peer]),
    ctmc:interrupt(Peer, {bump, self()}),
    Poppi;

handle_event(Poppi, {pull, Peer}) when is_pid(Peer) ->
    log("Sending pull from ~w to ~w~n", [self(), Peer]),
    ctmc:interrupt(Peer, {pull, self()}),
    Poppi;

handle_event(#poppi{peers=Peers}=Poppi, {push, Peer1}) when is_pid(Peer1) ->
    case choice(lists:delete(Peer1,Peers)) of
        none ->
            log("No candidate for push from ~w to ~w~n", [Peer1, self()]);
        {choice, Peer2} ->
            log("Sending push from ~w to ~w with ~w~n", [self(), Peer1, Peer2]),
            ctmc:interrupt(Peer1, {push, Peer2})
    end,
    Poppi.

handle_interrupt(#poppi{peers=Peers}=Poppi, {bump, Peer}) when is_pid(Peer) ->
    log("Receiving bump from ~w to ~w~n", [Peer, self()]),
    Poppi#poppi{peers=[Peer | lists:delete(Peer, Peers)]};

handle_interrupt(#poppi{peers=Peers}=Poppi, {pull, Peer1}) when is_pid(Peer1) ->
    log("Receiving pull from ~w to ~w~n", [Peer1, self()]),
    case choice(Peers) of
        none -> 
            log("No candidate for pull from ~w to ~w~n", [Peer1, self()]);
        {choice, Peer2} ->
            log("Sending push from ~w to ~w with ~w~n in response to pull", [self(), Peer1, Peer2]),
            ctmc:interrupt(Peer1, {push, Peer2})
    end,
    Poppi;

handle_interrupt(#poppi{peers=Peers}=Poppi, {push, Peer}) when is_pid(Peer) ->
    log("Receiving push from ~w to ~w~n", [Peer, self()]),
    Poppi#poppi{peers=[Peer | lists:delete(Peer, Peers)]}.

% internal functions

log(Format, Args) ->
    io:format(Format, Args).

choice(List) ->
    case length(List) of
        0 -> none;
        N -> {choice, lists:nth(random:uniform(N),List)}
    end.
             
