-module(crawler).
-export([start/0, stop/0, parse/1, parse/2]).

start() ->
    {ok, _} = application:ensure_all_started(?MODULE).

stop() ->
  Apps = [sync, crawler, poolboy],
  [application:stop(App) || App <- Apps],
  ok.

parse(Link) -> parse(Link, unlimited).
parse(Link, Limit) ->
  supervisor:start_child(crawler_server_sup, [[Link, Limit]]).
