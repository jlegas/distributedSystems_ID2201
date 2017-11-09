%%%-------------------------------------------------------------------
%%% @author justi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. sep 2017 19:15
%%%-------------------------------------------------------------------
-module(hist).
-author("justi").

%% API
-export([new/1, update/3]).

new(Name) ->
  [{Name, inf}].

update(Node, N, History) ->
  case lists:keyfind(Node, 1, History) of
    {Node, Value} -> case Value >= N of
                       true -> old;
                       false ->
                         {new, [{Node, N}] ++ lists:keydelete(Node, 1, History)}
                     end;
    false ->
      {new, History ++ [{Node, N}]}
  end.