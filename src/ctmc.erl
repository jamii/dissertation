-module(ctmc).

-export([start/2, call/2]).
-export([behaviour_info/1]).

-behaviour(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

-record(ctmc, {module, state, next_event}).
-define(SERVER, ?MODULE).

behaviour_info(callbacks) ->
    [{init,1},{events,1},{handle_event,2},{handle_call,2}];

behaviour_info(_Other) ->
    undefined.

% api

start(Module, Args) ->
    gen_server:start(?MODULE, [Module, Args], []).

call(Ctmc, Call) ->
    try
	{ok, gen_server:call(Ctmc, Call, 1000)}
    catch
	_:{timeout,_} -> timeout
    end.

% gen_server callbacks

init([Module, Args]) ->
    random:seed(now()),
    State = Module:init(Args),
    {Next_event, Timeout} = next_event(Module, State),
    {ok, #ctmc{module=Module, state=State, next_event=Next_event}, Timeout}.

handle_call(Call, _From, #ctmc{module=Module, state=State}=Ctmc) ->
    {State2, Reply} = Module:handle_call(State, Call),
    {Next_event, Timeout} = next_event(Module, State2),
    {reply, Reply, Ctmc#ctmc{state=State2, next_event=Next_event}, Timeout}.

handle_cast(_Cast, _Ctmc) ->
    ok.

handle_info(timeout, #ctmc{module=Module, state=State, next_event=Next_event}=Ctmc) ->
    State2 = Module:handle_event(State, Next_event),
    {Next_event2, Timeout} = next_event(Module, State2),
    {noreply, Ctmc#ctmc{state=State2, next_event=Next_event2}, Timeout};

handle_info(_, #ctmc{module=Module, state=State}=Ctmc) ->
    {Next_event, Timeout} = next_event(Module, State),
    {noreply, Ctmc#ctmc{next_event=Next_event}, Timeout}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% internal functions

next_event(Module, State) ->
    random:seed(now()),
    Events = [{Event, Rate} || {Event, Rate} <- Module:events(State), Rate > 0],
    Total = lists:sum([Rate || {_Event, Rate} <- Events]),
    case choose_event(Total * random:uniform(), Events) of
	no_event -> {no_event, infinity};
	{event, Event} -> {Event, round(1000 * exponential(Total))}
    end.

% rounding errors may occasionally cause spurious no_event
% normally should only happen when the event list is empty
choose_event(_P, []) ->
    no_event;
choose_event(P, [{Event, Rate}|Events]) ->
    New_p = P - Rate,
    if 
	New_p =< 0 -> {event, Event};
	true -> choose_event(New_p, Events)
    end.

exponential(Lambda) ->
    P = random:uniform(),
    (-math:log(P) / Lambda).
