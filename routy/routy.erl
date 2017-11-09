%%%-------------------------------------------------------------------
%%% @author justi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. sep 2017 18:37
%%%-------------------------------------------------------------------

%%erl -name sweden@192.168.1.2 -sname sweden -setcookie routy -connect_all false

-module(routy).
-author("justi").

%% API
-export([start/1, start/2, stop/1, status/1]).

start(Name) ->
  start(Name, Name).

start(Reg, Name) ->
  register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
  Node ! stop,
  unregister(Node).

init(Name) ->
  Intf = intf:new(),
  Map = map:new(),
  Table = dijkstra:table(Intf, Map),
  Hist = hist:new(Name),
  router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
  receive
    {add, Node, Pid} ->
      Ref = erlang:monitor(process, Pid),
      Intf1 = intf:add(Node, Ref, Pid, Intf),
      router(Name, N, Hist, Intf1, Table, Map);
    {remove, Node} ->
      {ok, Ref} = intf:ref(Node, Intf),
      erlang:demonitor(Ref),
      Intf1 = intf:remove(Node, Intf),
      router(Name, N, Hist, Intf1, Table, Map);
    {'DOWN', Ref, process, _, _} ->
      {ok, Down} = intf:name(Ref, Intf),
      io:format("~w: exit recived from ~w~n", [Name, Down]),
      Intf1 = intf:remove(Down, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

    {status, From} ->
      From ! {status, {Name, N, Hist, Intf, Table, Map}},
      router(Name, N, Hist, Intf, Table, Map);

    {links, Node, R, Links} ->
      case hist:update(Node, R, Hist) of
        {new, Hist1} ->
          intf:broadcast({links, Node, R, Links}, Intf),
          Map1 = map:update(Node, Links, Map),
          router(Name, N, Hist1, Intf, Table, Map1);
        old ->
          router(Name, N, Hist, Intf, Table, Map)
      end;

    update ->
      Table1 = dijkstra:table(intf:list(Intf), Map),
      router(Name, N, Hist, Intf, Table1, Map);

    broadcast ->
      Message = {links, Name, N, intf:list(Intf)},
      intf:broadcast(Message, Intf),
      router(Name, N + 1, Hist, Intf, Table, Map);

    {route, Name, From, Message} ->
      io:format("~w: received message ~w~n from: ~w~n", [Name, Message, From]),
      router(Name, N, Hist, Intf, Table, Map);

    {route, To, From, Message} ->
      io:format("~w: routing message (~w)", [Name, Message]),
      case dijkstra:route(To, Table) of
        {ok, Gw} ->
          case intf:lookup(Gw, Intf) of
            {ok, Pid} ->
              Pid ! {route, To, From, Message};
            notfound ->
              ok
          end;
        notfound ->
          ok
      end,
      router(Name, N, Hist, Intf, Table, Map);

    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, Hist, Intf, Table, Map);

    stop -> ok
  end.


status(Pid) ->
  Pid ! {status, self()},
  receive
    {status, {Name, N, Hist, Intf, Table, Map}} ->
      io:format("Status: Name, N, Hist, Intf, Table, Map ~n"),
      io:format("Name: ~w~n", [Name]),
      io:format("N: ~w~n", [N]),
      io:format("Hist: ~w~n", [Hist]),
      io:format("Intf: ~w~n", [Intf]),
      io:format("Table: ~w~n", [Table]),
      io:format("Map: ~w~n", [Map]),
      ok;
    true ->
      io:format("  got something: ~n")
  end.
