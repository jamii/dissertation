-module(poppi).

-export([start/0, start/1]).

-behaviour(ctmc).
-export([init/1, events/1, handle_event/2, handle_interrupt/2]).

-record(poppi, {peers}).

% api

start() ->
    start([]).

start(Peers) when is_list(Peers) ->
    ctmc:start(?MODULE, [Peers]).

% ctmc callbacks

init([Peers]) ->
    #poppi{peers=Peers}.

events(#poppi{peers=Peers}) ->
    [{{push_self, Peer}, 1} || Peer <- Peers].

handle_event(Poppi, {push_self, Peer}) ->
    % io:format("Sending push from ~w to ~w~n", [self(), Peer]),
    ctmc:interrupt(Peer, {push_self, self()}),
    Poppi.

handle_interrupt(#poppi{peers=Peers}=Poppi, {push_self, Peer}) ->
    % io:format("Receiving push from ~w to ~w~n", [Peer, self()]),
    Peers2 = [Peer | lists:delete(Peer, Peers)],
    Poppi#poppi{peers=Peers2}.
