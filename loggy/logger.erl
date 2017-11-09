%%%-------------------------------------------------------------------
%%% @author justi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. sep 2017 12:38
%%%-------------------------------------------------------------------
-module(logger).
-author("justi").

%% API
-export([start/1, stop/1]).

start(Nodes) ->
  spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  loop(time:clock(Nodes), []).

loop(Clock, Queue) ->
  receive
    {log, From, Time, Msg} ->
      Tick = time:update(From, Time, Clock),
      NewQueue = check(Tick, [{From, Time, Msg} | Queue], []),
      loop(Tick, lists:keysort(2, NewQueue));
    stop ->
      ok
  end.

check(_, [], Acc) -> Acc;
check(Clock, [H|T], Acc) ->
  {From, Time, Msg} = H,
  case time:safe(Time, Clock) of
    true ->
      log(From, Time, Msg),
      check(Clock, T, Acc);
    false -> check(Clock, T, [H|Acc])
  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
