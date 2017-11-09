%%%-------------------------------------------------------------------
%%% @author justi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. sep 2017 17:02
%%%-------------------------------------------------------------------
-module(map).
-author("justi").

%% API
-export([new/0, update/3, reachable/2, all_nodes/1]).


new() -> [].

update(Node, Links, Map) ->
  case lists:keyfind(Node, 1, Map) of
    {Node, _} -> [{Node, Links}|lists:keydelete(Node, 1, Map)];
    false -> [{Node, Links}|Map]
  end.


reachable(Node, Map) ->
  case lists:keyfind(Node, 1, Map) of
    {Node, Links} -> Links;
    false -> []
end.

all_nodes(Map) ->
  lists:usort(lists:foldl(fun({Node, Links}, Nodes) -> Nodes ++ Links ++ [Node] end, [], Map)).


