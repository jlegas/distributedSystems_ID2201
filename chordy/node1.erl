%%%-------------------------------------------------------------------
%%% @author justi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. okt 2017 22:55
%%%-------------------------------------------------------------------
-module(node1).
-author("justi").

%% API
-export([start/1, start/2]).

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
  node(Id, Predecessor, Successor).

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

node(Id, Predecessor, Successor) ->
  receive
    %% Peer needs our key
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor);

    %% Peer let's us know it exists
    %% Peer becomes our Predecessor or not
    {notify, New} ->
      Pred = notify(New, Id, Predecessor),
      node(Id, Pred, Successor);

    %% Our predecessor asks for our predecessor
    %% response: Peer ! {status, {Pkey, Ppid}}
    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor);

    %% Our successor tells us about its predecessor
    %% it either stays successor, or its predecessors become our successors
    {status, Pred} ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ);

    %% add scheduled stabilization and probing
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor);
    %% probing is to go around the chord and measure roundtrip
    %% start probing
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor);
    %% probe made the roundtrip
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor);
    %% forward probe
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor);

    status ->
      io:format("Node Id ~w Pred ~w Succ ~w ~n", [Id, Predecessor, Successor]),
      node(Id, Predecessor, Successor);
    _Msg ->
      io:format("Node ~w received undetermined msg ~w ~n", [Id, _Msg])
  end.




%% (New, Id, Predecessor)
%% suggestion from other node that they are our predecessor
notify({Nkey, Npid}, Id, Predecessor) ->
  case Predecessor of
    nil ->
      {Nkey, Npid};
    {Pkey, _} ->
      %% we do have a new predecessor
      %% if Nkey in (Pkey; Id]:
      case key:between(Nkey, Pkey, Id) of
        true ->
          {Nkey, Npid};
        false ->
          Predecessor
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

