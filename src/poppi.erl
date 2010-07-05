-module(poppi).

-export([start/0, start/1, start_many/2, start_star/1]).

-behaviour(ctmc).
-export([init/1, events/1, handle_event/2, handle_interrupt/2]).

-record(poppi, {peers}).

% event constants

-define(FORGET, 0.01).
-define(BUMP, 0.01).
-define(PULL, 0.01).
-define(PUSH, 0.01).
-define(CACHE, 0.01).

% api

start() ->
    start([]).

start(Peers) when is_list(Peers) ->
    true = lists:all(fun is_pid/1, Peers),
    ctmc:start(?MODULE, [Peers]).

start_many(N, Peers) ->
    Nodes = [start(Peers) || _ <- lists:seq(1,N)],
    {ok, [Node || {ok, Node} <- Nodes]}.

start_star(N) ->
    {ok, Hub} = start(),
    {ok, Spokes} = start_many(N, [Hub]),
    {ok, Hub, Spokes}.

% ctmc callbacks

init([Peers]) ->
    #poppi{peers=Peers}.

events(#poppi{peers=Peers}) ->
    lists:flatten([
      [{{forget, Peer}, ?FORGET} || Peer <- Peers],
      {cache, ?CACHE},
      [{{bump, Peer}, ?BUMP} || Peer <- Peers],
      [{{pull, Peer}, ?PULL} || Peer <- Peers],
      [{{push, Peer}, ?PUSH} || Peer <- Peers]]).

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
        Peer2 ->
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
        N -> lists:nth(random:uniform(N),List)
    end.
             
