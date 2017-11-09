%%%-------------------------------------------------------------------
%%% @author justi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. okt 2017 22:55
%%%-------------------------------------------------------------------
-module(node3).
-author("justi").

%% API
-export([start/1, start/2, test/0]).

-define(Timeout, 10000).
-define(Stabilize, 1000).

start(Id) ->
  start(Id, nil).

start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

%% Predecessor nil, connect to Successor
init(Id, Peer) ->
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  Storage = storage:create(),
  node(Id, Predecessor, Successor, nil, Storage).

connect(Id, nil) ->
  {ok, {Id, nil, self()}};
connect(_Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      Sref = erlang:monitor(process, Peer),
      {ok, {Skey, Sref, Peer}}
  after ?Timeout ->
    io:format("Time out: no response from ~w~n",[Peer])
  end.

%% start monitoring other nodes
monitorStart(Predecessor, Successor) ->
  if
    Predecessor /= nil ->
      {_, PredPid} = Predecessor,
      PredMon = erlang:monitor(process, PredPid);
    true ->
      PredMon = nil
  end,
  if
    Successor /= nil ->
      {_, SuccPid} = Successor,
      SuccMon = erlang:monitor(process, SuccPid);
    true ->
      SuccMon = nil
  end,
  {PredMon, SuccMon}.

%% stop monitoring
monitorStop({PredMon, SuccMon}) ->
  if
    PredMon /= nil ->
      erlang:demonitor(PredMon);
    true ->
      ok
  end,
  if
    SuccMon /= nil ->
      erlang:demonitor(SuccMon);
    true ->
      ok
  end.





node(Id, Predecessor, Successor, Next, Storage) ->
  %%monitorStop(Monitor),
  %%NewMonitor = monitorStart(Predecessor, Successor),
  receive
    %% Peer needs our key
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Next, Storage);

    %% Peer let's us know it exists
    %% Peer becomes our Predecessor or not
    %% Handover data
    {notify, New} ->
      {Pred, Store} = notify(New, Id, Predecessor, Storage),
      node(Id, Pred, Successor, Next, Store);

    %% Our predecessor asks for our predecessor
    %% response: Peer ! {status, {Pkey, Ppid}}
    {request, Peer} ->
      request(Peer, Predecessor, Next),
      node(Id, Predecessor, Successor, Next, Storage);

    %% Our successor tells us about its predecessor
    %% it either stays successor, or its predecessors become our successors
    {status, Pred, NewNext} ->
      {Succ, ProperNext} = stabilize(Pred, NewNext, Id, Successor),
      node(Id, Predecessor, Succ, ProperNext, Storage);

    %% add scheduled stabilization and probing
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Next, Storage);
    %% probing is to go around the chord and measure roundtrip
    %% start probing
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Next, Storage);
    %% probe made the roundtrip
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Next, Storage);
    %% forward probe
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Next, Storage);

    %% add storage
    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client,
        Id, Predecessor, Successor, Storage),
      node(Id, Predecessor, Successor, Next, Added);
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Storage),
      node(Id, Predecessor, Successor, Next, Storage);
    {handover, Elements} ->
      Merged = storage:merge(Storage, Elements),
      node(Id, Predecessor, Successor, Next, Merged);

    {'DOWN', Ref, process, _, _} ->
      io:format("Node Down ~n", []),
      {NewPred, NewSucc, Nxt} = crash(Ref, Predecessor, Successor, Next),
      node(Id, NewPred, NewSucc, Nxt, Storage);


      status ->
      io:format("Node Id ~w Pred ~w Succ ~w Next ~w Content ~w ~n", [Id, Predecessor, Successor, Next, Storage]),
      node(Id, Predecessor, Successor, Next, Storage);
    stop ->
      ok;
    _Msg ->
      io:format("Node ~w received undetermined msg ~w ~n", [Id, _Msg])
  end.

crash(Ref, {_, Ref, _}, Successor, Next) ->
  {nil, Successor, Next};
crash(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
  Nref = erlang:monitor(process, Npid),
  {Predecessor, {Nkey, Nref, Npid}, nil}.

%% add key-value pair, return updated storage
add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Storage) ->
  case (key:between(Key, Pkey, Id)) of
    true ->
      Client ! {Qref, ok},
      storage:add(Key, Value, Storage);
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Storage
  end.


%% return {Key, Value} or false
lookup(Key, Qref, Client, Id, {Pkey, _, _}, Successor, Storage) ->
  case (key:between(Key, Pkey, Id)) of
    true ->
      Result = storage:lookup(Key, Storage),
      Client ! {Qref, Result};
    false ->
      {_, _, Spid} = Successor,
      Spid ! {lookup, Key, Qref, Client}
  end.

handover(Id, Store, Nkey, Npid) ->
  {Keep, Rest} = storage:split(Id, Nkey, Store),
  Npid ! {handover, Rest},
  Keep.


notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      Nref = erlang:monitor(process, Npid),
      {{Nkey, Nref, Npid}, Keep};
    {Pkey, Pref, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Nkey, Npid),
          erlang:demonitor(Pref),
          Nref = erlang:monitor(process, Npid),
          {{Nkey, Nref, Npid}, Keep};
        false ->
          {Predecessor, Store}
      end
end.

request(Peer, Predecessor, Next) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil, Next};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}, Next}
  end.

stabilize({_, _, Spid}) ->
  Spid ! {request, self()}.

stabilize(Pred, Next, Id, Successor) ->
  {Skey, Sref, Spid} = Successor,
  case Pred of
    %% have no successor, get a new one
    nil ->
      Spid ! {notify, {Id, self()}},
      {Successor, Next};
    %% all alone in the ring
    {Id, _} ->
      {Successor, Next};
    %% everything fine
    {Skey, _} ->
      Spid ! {notify, {Id, self()}},
      {Successor, Next};
    %% new node
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        %% new successor!
        true ->
          Xpid ! {request, self()},
          erlang:demonitor(Sref),
          Xref = erlang:monitor(process, Xpid),
          {{Xkey, Xref, Xpid}, Successor};
        %% We should be our successors predecessor!
        %% they have a wrong predecessor now
        false ->
          Spid ! {notify, {Id, self()}},
          {Successor, Next}
      end
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

create_probe(Id, Successor) ->
  {_, Spid} = Successor,
  Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

%% Ref - original sender, T - time of origin
forward_probe(Ref, T, Nodes, Id, Successor) ->
  {_, Spid} = Successor,
  Spid ! {probe, Ref, [Id|Nodes], T}.

remove_probe(T, Nodes) ->
  Rtt = erlang:system_time(micro_seconds) - T,
  io:format("Probe roundtrip ~w through ~w ~n", [Rtt, Nodes]).

test() ->
  Peer1 = test:start(node3),
  timer:sleep(2000),
  test:start(node3, 4, Peer1),
  timer:sleep(8000),
  Peer1 ! probe,
  timer:sleep(1000),
  Keys = test:keys(1000),
  timer:sleep(1000),
  test:add(Keys, Peer1),
  timer:sleep(5000),
  Peer1 ! status,
  test:check(Keys, Peer1),
  timer:sleep(5000),
  PeerExtra = start(nodeExtra, Peer1),
  timer:sleep(8000),
  Peer1 ! probe,
  %%PeerExtra ! stop,
  timer:sleep(8000),
  Peer1 ! probe,
  Peer1.






