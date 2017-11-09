%%%-------------------------------------------------------------------
%%% @author justi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. okt 2017 20:51
%%%-------------------------------------------------------------------
-module(gms4).
-author("justi").

%% API
%% -export([]).
-compile(export_all).
-define(arghh, 100).
-define(timeout, 2000).

%% master
start(Id) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun() -> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  leader(Id, Master, 0, [], [Master]).

%% slave
start(Id, Grp) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun() -> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    {view, N, [Leader | Slaves], Group} ->
      Master ! {view, Group},
      random:seed(Rnd, Rnd, Rnd),
      slave(Id, Master, Leader, N + 1, {view, N, [Leader | Slaves], Group}, Slaves, Group)
  after ?timeout ->
    Master ! {error, "no reply from leader"}
  end.

leader(Id, Master, N, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      bcast(Id, {msg, N, Msg}, Slaves),
      Master ! Msg,
      leader(Id, Master, N + 1, Slaves, Group);
    {join, Wrk, Peer} ->
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, N, [self() | Slaves2], Group2}, Slaves2),
      Master ! {view, Group2},
      leader(Id, Master, N + 1, Slaves2, Group2);
    stop ->
      ok
  end.

%%bcast(_, _, []) ->
%%  done;
%%bcast(Id, Msg, [H | T]) ->
%%  H ! Msg,
%%  bcast(Id, Msg, T).

bcast(Id, Msg, Nodes) ->
  lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
  case random:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~w: crash~n", [Id]),
      exit(no_luck);
    _ ->
      ok
  end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
  erlang:monitor(process, Leader),
  receive
    {mcast, Msg} ->
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {join, Wrk, Peer} ->
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, I, _} when I < N ->
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, Nr, Msg} ->
      Master ! Msg,
      slave(Id, Master, Leader, Nr + 1, {msg, Nr, Msg}, Slaves, Group);
    {view, Nr, [Leader | Slaves2], Group2} ->
      Master ! {view, Group2},
      slave(Id, Master, Leader, Nr + 1, Last, Slaves2, Group2);
    %% {'DOWN', Ref, process, Pid2, Reason}
    {'DOWN', _Ref, process, Leader, _Reason} ->
%%      [_|T] = Slaves,
%%      election(Id, Master, N, Last, T, Group);
      election(Id, Master, N, Last, Slaves, Group);
    stop ->
      ok
  end.

election(Id, Master, N, Last, Slaves, Group) ->
  io:format("Election ~w ongoing...~n", [Id]),
  Self = self(),
  case Slaves of
  [Self | Rest] ->
    io:format("Leader ~w ~n", [Id]),
    io:format("Slaves ~w ~n", [Slaves]),
    bcast(Id, {view, Slaves, Group}, Rest),
    bcast(Id, Last, Rest),
    Master ! {view, Group},
    leader(Id, Master, N + 1, Rest, Group);
  [Leader | Rest] ->
    io:format("Leader ~w ~n", [Leader]),
    slave(Id, Master, Leader, N, Last, Rest, Group)
end.