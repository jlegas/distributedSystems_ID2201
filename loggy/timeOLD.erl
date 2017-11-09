%%%-------------------------------------------------------------------
%%% @author justi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% This is the generator of Lamport timestamps
%%% {Name, Timestamp}
%%% @end
%%% Created : 27. sep 2017 12:43
%%%-------------------------------------------------------------------
-module(time).
-author("justi").

%% API
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

%% initial value
zero() -> 0.

%% return incremented T
inc(Name, T) ->
  T + 1.

%% merge timestamps
merge(Ti, Tj) ->
  case leq(Ti, Tj) of
    true -> Tj;
    false -> Ti
  end.

%% Ti <= Tj
leq(Ti, Tj) ->
  case (Ti =< Tj) of
    true -> true;
    false -> false
  end.

clock(Nodes) ->
  [{Node, zero()} || Node <- Nodes].

update(Node, Time, Clock) ->
  lists:keysort(2, lists:keyreplace(Node, 1, Clock, {Node, Time})).

safe(Time, Clock) ->
  lists:all(fun({_N, T}) -> leq(Time, T) end, Clock).
