%%%-------------------------------------------------------------------
%%% @author justi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. sep 2017 17:32
%%%-------------------------------------------------------------------
-module(dijkstra).
-author("justi").

%% API
-compile(export_all).
%%-export([table/2, route/2]).


%%dijkstra(Name, Gateways, Map, RTable) ->
%%.

entry(Node, Sorted) ->
  case lists:keyfind(Node, 1, Sorted) of
    false -> 0;
    {Node, N, _} -> N
  end.

replace(Node, N, Gateway, Sorted) ->
  Replaced = lists:keydelete(Node, 1, Sorted) ++ [{Node, N, Gateway}],
  lists:sort(fun({_, A, _}, {_, B, _}) -> A =< B end, Replaced).

update(Node, N, Gateway, Sorted) ->
  case entry(Node, Sorted) > N of
    false -> Sorted;
    true -> replace(Node, N, Gateway, Sorted)
  end.

iterate(Sorted, Map, Table) ->
  case Sorted of
    [] -> Table;
    [{_, inf, _} | _] -> Table;
    [{Node, N, Gateway} | TailSorted] ->
      case map:reachable(Node, Map) of
        [] ->
          iterate(TailSorted, Map, [{Node, Gateway} | Table]);
        Nodes ->
          NewSorted = lists:foldl(fun(X, Acc) ->
            update(X, N + 1, Gateway, Acc) end, TailSorted, Nodes),
          NewTable = [{Node, Gateway} | Table],
          iterate(NewSorted, Map, NewTable)
      end
  end.


construct(Gateways) ->
  fun(X, Acc) ->
    case lists:member(X, Gateways) of
      true ->
        [{X, 0, X} | Acc];
      false ->
        [{X, inf, unknown} | Acc]
    end
  end.

table(Gateways, Map) ->
  NodesList = lists:foldl(construct(Gateways), [], map:all_nodes(Map)),
  Sorted = lists:keysort(2, NodesList),
  iterate(Sorted, Map, []).

%%table(Gateways, Map) ->
%%  Nodes = map:all_nodes(Map),
%%  %%'Sorted' gateway-gateway with 0 distance
%%  Gateway0Gateway = lists:map(fun(Gateway) -> {Gateway, 0, Gateway} end, Gateways),
%%  %%'Sorted' of all nodes with inf distance
%%  EndNodes = Nodes -- Gateways,
%%  NodeInfNode = lists:map(fun(Node) -> {Node, inf, unknown} end, EndNodes),
%%  %%
%%  iterate(Gateway0Gateway ++ NodeInfNode, Map, []).

route(Node, Table) ->
  case lists:keyfind(Node, 1, Table) of
    false -> notfound;
    {Node, Gateway} -> {ok, Gateway}
  end.

