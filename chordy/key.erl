%%%-------------------------------------------------------------------
%%% @author justi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. okt 2017 22:56
%%%-------------------------------------------------------------------
-module(key).
-author("justi").

%% API
-export([generate/0, between/3, gen/0]).

generate() ->
  rand:uniform(1000000000).

%% Check if Key in (From; To] with regard to circular structure
between(Key, From, To) ->
  if
    (From < To) and (To >= Key) and (Key > From) ->
      true;
    (From > To) and ((To >= Key) or (Key > From)) ->
      true;
    (From == To) ->
      true;
    true ->
      false
end.

gen() ->
  io:format("~w~n", [generate()]),
  gen().