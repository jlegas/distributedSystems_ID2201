%%%-------------------------------------------------------------------
%%% @author justi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. sep 2017 12:40
%%%-------------------------------------------------------------------
-module(worker).
-author("justi").

%% API
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
  Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
  random:seed(Seed, Seed, Seed),
  receive
    {peers, Peers} ->
      IntTime = time:zero(),
      loop(Name, Log, Peers, Sleep, Jitter, IntTime);
    stop ->
      ok
  end.

peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, IntClock)->
  Wait = random:uniform(Sleep),
  receive
    {msg, Time, Msg} ->
      %% increment at reception
      Tick = time:inc(Name, time:merge(IntClock, Time)),
      Log ! {log, Name, Tick, {received, Msg}},
      loop(Name, Log, Peers, Sleep, Jitter, Tick);
    stop ->
      ok;
    Error ->
      Log ! {log, Name, IntClock, {error, Error}}
  after Wait ->
    Selected = select(Peers),
    %% increment before sending
    Tick = time:inc(Name, IntClock),
    Message = {hello, random:uniform(100)},
    Selected ! {msg, Tick, Message},
    jitter(Jitter),
    Log ! {log, Name, Tick, {sending, Message}},
    loop(Name, Log, Peers, Sleep, Jitter, Tick)
  end.

select(Peers) ->
  lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;

jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).

