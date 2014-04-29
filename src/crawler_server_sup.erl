-module(crawler_server_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
  ok, _ = supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) ->
  {ok, {{simple_one_for_one, 5, 10},
  [?CHILD(crawler_server, worker)]}}.