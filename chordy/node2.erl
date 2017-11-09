%%%-------------------------------------------------------------------
%%% @author justi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. okt 2017 22:55
%%%-------------------------------------------------------------------
-module(node2).
-author("justi").

%% API
-export([start/1, start/2, test1/0, test2/0]).

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
  node(Id, Predecessor, Successor, Storage).

connect(Id, nil) ->
  {ok, {Id, self()}};
connect(_Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey, Peer}}
  after ?Timeout ->
    io:format("Time out: no response from ~w~n",[Peer])
  end.

node(Id, Predecessor, Successor, Storage) ->
  receive
    %% Peer needs our key
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Storage);

    %% Peer let's us know it exists
    %% Peer becomes our Predecessor or not
    %% Handover data
    {notify, New} ->
      {Pred, Store} = notify(New, Id, Predecessor, Storage),
      node(Id, Pred, Successor, Store);

    %% Our predecessor asks for our predecessor
    %% response: Peer ! {status, {Pkey, Ppid}}
    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor, Storage);

    %% Our successor tells us about its predecessor
    %% it either stays successor, or its predecessors become our successors
    {status, Pred} ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ, Storage);

    %% add scheduled stabilization and probing
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Storage);
    %% probing is to go around the chord and measure roundtrip
    %% start probing
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Storage);
    %% probe made the roundtrip
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Storage);
    %% forward probe
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Storage);

    %% add storage
    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client,
        Id, Predecessor, Successor, Storage),
      node(Id, Predecessor, Successor, Added);
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Storage),
      node(Id, Predecessor, Successor, Storage);
    {handover, Elements} ->
      Merged = storage:merge(Storage, Elements),
      node(Id, Predecessor, Successor, Merged);

    status ->
      io:format("Node Id ~w Pred ~w Succ ~w Content ~w ~n", [Id, Predecessor, Successor, Storage]),
      node(Id, Predecessor, Successor, Storage);
    _Msg ->
      io:format("Node ~w received undetermined msg ~w ~n", [Id, _Msg])
  end.

%% add key-value pair, return updated storage
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Storage) ->
  case (key:between(Key, Pkey, Id)) of
    true ->
      Client ! {Qref, ok},
      storage:add(Key, Value, Storage);
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Storage
  end.


%% return {Key, Value} or false
lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Storage) ->
  case (key:between(Key, Pkey, Id)) of
    true ->
      Result = storage:lookup(Key, Storage),
      Client ! {Qref, Result};
    false ->
      {_, Spid} = Successor,
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
      {{Nkey, Npid}, Keep};
    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Nkey, Npid),
          {{Nkey, Npid}, Keep};
        false ->
          {Predecessor, Store}
      end
end.

request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.

stabilize({_, Spid}) ->
  Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
    %% have no successor, get a new one
    nil ->
      Spid ! {notify, {Id, self()}},
      Successor;
    %% all alone in the ring
    {Id, _} ->
      Successor;
    %% everything fine
    {Skey, _} ->
      Spid ! {notify, {Id, self()}},
      Successor;
    %% new node
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        %% new successor!
        true ->
          Xpid ! {request, self()},
          Pred;
        %% We should be our successors predecessor!
        %% they have a wrong predecessor now
        false ->
          Spid ! {notify, {Id, self()}},
          Successor
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

test1() ->
  Peer1 = test:start(node2),
  timer:sleep(3000),
  test:start(node2, 4, Peer1),
  timer:sleep(8000),
  Peer1 ! probe,
  timer:sleep(1000),
  Keys = test:keys(5000),
  timer:sleep(1000),
  test:add(Keys, Peer1),
  timer:sleep(5000),
  Peer1 ! status,
  test:check(Keys, Peer1),
  timer:sleep(5000),
  Peer1.

test2() ->
  Peer1 = test:start(node2),
  timer:sleep(4000),
  Peer1 ! probe,
  timer:sleep(1000),
  Keys = test:keys(5000),
  timer:sleep(3000),
  test:add(Keys, Peer1),
  timer:sleep(5000),
  Peer1 ! status,
  test:check(Keys, Peer1),
  timer:sleep(5000),
  Peer1.





