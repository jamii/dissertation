-module(poppi).

-export([start/0, start/1, start_experiment/2]).

-behaviour(ctmc).
-export([init/1, events/1, handle_event/2, handle_call/2]).

-record(poppi, {node,root,known_roots,log}).

% event constants

-define(LAMBDA, 0.1).
-define(MU, 0.001).

% api

start() ->
    start([]).

start(KnownRoots) ->
    start(KnownRoots, fun (_) -> ok end).

start(KnownRoots, Log) ->
    Poppi = #poppi{node=none, root=none, known_roots=KnownRoots, log=Log},
    ctmc:start(?MODULE, [Poppi]).

start_experiment(N, K) ->
    KnownRoots = [Root || {ok, Root} <- [start() ||  _ <- lists:seq(1,K)]],
    _Nodes = [Node || {ok, Node} <- [start(KnownRoots) || _ <- lists:seq(1,N-1)]],
    {ok, _ExperimentNode} = start(KnownRoots, fun log_sample/1).

% ctmc callbacks

init([#poppi{known_roots=KnownRoots}=Poppi]) ->
    Node = choose_root(KnownRoots),
    Root = choose_root(KnownRoots),
    Poppi#poppi{node=Node, root=Root}.

events(#poppi{}) ->
    lists:flatten([{msg_root, ?LAMBDA}, {msg_known_root, ?MU}]).

handle_event(#poppi{node=Node}=Poppi, msg_root) ->
    %log("Messaging root ~w from ~w~n", [Node, self()]),
    sample(Poppi, Node);

handle_event(#poppi{known_roots=KnownRoots}=Poppi, msg_known_root) ->
    Root= choose_root(KnownRoots),
    %log("Messaging known root ~w from ~w~n", [Root, self()]),
    sample(Poppi, Root).
    
handle_call(#poppi{root=Root}=Poppi, {sample, Node}) ->
    %log("Sending sample to ~w from ~w~n", [Node, self()]),
    {Poppi#poppi{root=Node}, Root}.
    
% internal functions

sample(#poppi{root=Root, known_roots=KnownRoots, log=Log}=Poppi, Target) ->
    if
	Target == self() ->
	    Log(Root),
	    Poppi#poppi{node=Root, root=self()};
	true ->
	    case ctmc:call(Target, {sample, self()}) of
		{ok, Node2} ->
		    Log(Node2),
		    Poppi#poppi{node=Node2};
		timeout ->
		    log("Timeout at ~w~n", [self()]),
		    Node2 = choose_root(KnownRoots),
		    Log(Node2),
		    Poppi#poppi{node=Node2}
	    end
    end.

log(Format, Args) ->
    io:format(Format, Args).

log_sample(Sample) ->
    log("Sample ~w~n", [Sample]).

choice(List) ->
    case length(List) of
        0 -> none;
        N -> {choice, lists:nth(random:uniform(N),List)}
    end.

choose_root(Roots) ->
    case choice(Roots) of
	none -> self();
	{choice, Root} -> Root
    end.
             
