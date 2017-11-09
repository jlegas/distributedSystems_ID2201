%%%-------------------------------------------------------------------
%%% @author justi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. sep 2017 19:15
%%%-------------------------------------------------------------------
-module(intf).
-author("justi").

%% API
-compile(export_all).


new() -> [].

add(Name, Ref, Pid, Intf) ->
  [{Name,Ref,Pid}|Intf].
%%  case lookup(Name, Intf) of
%%    notfound -> Intf ++ [{Name, Ref, Pid}];
%%    {ok, _} -> error
%%  end.

remove(Name, Intf) ->
case lookup(Name, Intf) of
  {ok, _} -> lists:keydelete(Name, 1, Intf);
  false -> error
end.

lookup(Name, Intf) ->
  case lists:keyfind(Name, 1, Intf) of
    {Name, _, Pid} -> {ok, Pid};
    false -> notfound
  end.

ref(Name, Intf) ->
  case lists:keyfind(Name, 1, Intf) of
    {Name, Ref, _} -> {ok, Ref};
    false -> notfound
  end.

name(Ref, Intf) ->
  case lists:keyfind(Ref, 2, Intf) of
    {Name, Ref, _} -> {ok, Name};
    false -> notfound
end.

list(Intf) ->
  lists:foldl(fun({Name, _Ref, _Pid}, Acc) -> [Name | Acc] end, [], Intf).

broadcast(Message, Intf) ->
  lists:map(fun({_,_,Pid}) -> Pid ! Message end, Intf).
%%broadcast(Message, Intf) ->
%%  lists:map(fun({Name,_,_}) -> Name ! Message end, Intf).
