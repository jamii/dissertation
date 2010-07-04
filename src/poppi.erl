-module(poppi).

-export([start/0, start/1]).

-export([run/1]).

start() ->
    start([]).

start(Peers) when is_list(Peers) ->
    spawn( 
      fun () ->
	      random:seed(now()),
	      run(Peers)
      end).

run(Peers) ->
    Interval = 
	case length(Peers) of
	    0 -> infinity;
	    N -> round(exponential(0.001 * N))
	end,
    receive
	{push, Peer} ->
	    io:format("Receiving push from ~w to ~w~n", [Peer, self()]),
	    NewPeers = [Peer | lists:delete(Peer, Peers)],
	    poppi:run(NewPeers)
    after Interval ->
	    {choice, Peer} = choose(Peers),
	    io:format("Sending push from ~w to ~w~n", [self(), Peer]),
	    Peer ! {push, self()},
	    poppi:run(Peers)
    end.

choose(Choices) when is_list(Choices) ->
    N = random:uniform(length(Choices)),
    case N of
	0 -> none;
	_ -> {choice, lists:nth(N, Choices)}
    end.

exponential(Lambda) ->
    P = random:uniform(),
    (-math:log(P) / Lambda).
