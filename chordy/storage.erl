%%%-------------------------------------------------------------------
%%% @author justi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. okt 2017 01:19
%%%-------------------------------------------------------------------
-module(storage).
-author("justi").

%% API
-export([create/0, add/3, lookup/2, split/3, merge/2]).

create() ->
  [].

add(Key, Value, Store) ->
  lists:keystore(Key, 1, Store, {Key, Value}).


lookup(Key, Store) ->
  lists:keyfind(Key, 1, Store).

%% return {ExtractedElements, Rest}
split(From, To, Store) ->
  lists:partition(fun({K,_}) -> key:between(K, From, To) end, Store).

merge(Entries, Store) ->
  lists:keymerge(1, Entries, Store).
