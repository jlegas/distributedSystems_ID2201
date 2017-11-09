%%%-------------------------------------------------------------------
%%% @author justi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Sep 2017 4:09 PM
%%%-------------------------------------------------------------------
-module(rudy).
-author("justi").

%% API
-export([start/1, stop/0,init/1,handler/1,request/1,reply/1]).

%% Task 2
init(Port) ->
  %% list Received Packet is delivered as a list.
  %% {active, true} everything received from the socket is sent as messages to the receiving process
  %% {reuseaddr, Boolean} Allows or disallows local reuse of port numbers. By default, reuse is disallowed.
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      handler(Listen),
      gen_tcp:close(Listen),
      ok;
    {error, Error} ->
      io:format("rudy: init error: ~w~n", [Error])
  end.

handler(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      request(Client),
      %% task 2.2 handler calls itself recursively after request
      handler(Listen),
      ok;
    {error, Error} ->
      io:format("rudy: handler error: ~w~n", [Error])
  end.

request(Client) ->
  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} ->
      Request = http:parse_request(Str),
      Response = reply(Request),
      gen_tcp:send(Client, Response);
    {error, Error} ->
      io:format("rudy: request error: ~w~n", [Error])
  end,
  gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
  %% 40ms delay for benchmarks
  %timer:sleep(120),
  http:ok(URI).

%% Task 2.2
start(Port) ->
  register(rudy, spawn(fun() -> init(Port) end)).
stop() ->
  exit(whereis(rudy), "time to die").
