-module(stats).

-export([]).

% write results to file
% separate creation from sampling so can run multiple loggers

view_size(File, N, Rates) ->
    {ok, Hub, Spokes} = poppi:start_star(N, Rates),
    Spoke = lists:nth(random:uniform(N), Spokes),
    Sample = fun (#poppi{peers=Peers}) ->
		     io:format(File, "~B~n", [lists:length(Peers)])
	     end,
    ctmc:poll_state(Spoke, 1000, Sample).
		   
		    
