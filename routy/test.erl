%%%-------------------------------------------------------------------
%%% @author justi
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. sep 2017 11:15
%%%-------------------------------------------------------------------
-module(test).
-author("justi").

%% API
-compile(export_all).

start() ->
  io:format("Start cities... Stockholm, Umeå, Luleå ~n"),
  routy:start(stockholm),
  routy:start(lund),
  routy:start(uppsala),
  routy:start(goteborg),
  routy:start(malmo),

  stockholm ! {add, lund, {lund, node()}},
  stockholm ! {add, malmo, {malmo, node()}},
  lund ! {add, uppsala, {uppsala, node()}},
  uppsala ! {add, stockholm, {stockholm, node()}},
  goteborg ! {add, uppsala, {uppsala, node()}},
  malmo ! {add, goteborg, {goteborg, node()}},
  stockholm ! broadcast,
  timer:sleep(1000),
  lund ! broadcast,
  timer:sleep(1000),
  uppsala ! broadcast,
  timer:sleep(1000),
  malmo ! broadcast,
  timer:sleep(1000),
  goteborg ! broadcast,
  timer:sleep(1000),
  stockholm ! update,
  timer:sleep(1000),
  lund ! update,
  timer:sleep(1000),
  uppsala ! update,
  timer:sleep(1000),
  malmo ! update,
  timer:sleep(1000),
  goteborg ! update,
  timer:sleep(1000).

stop() ->
  stockholm ! stop,
  umea ! stop,
  lulea ! stop.

status() ->
  routy:status(stockholm),
  routy:status(malmo),
  routy:status(goteborg).