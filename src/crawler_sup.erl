-module(crawler_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  ok, _ = supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
  {ok, MaxWorkers} = application:get_env(crawler, workers),
  % PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
  %                           PoolArgs = [{name, {local, Name}},
  %                             {worker_module, crawler_worker}] ++ SizeArgs,
  %                           poolboy:child_spec(Name, PoolArgs, WorkerArgs)
  %                       end, Pools),
  Workers = ?CHILD(crawler_worker, worker, MaxWorkers),
  Servers = ?CHILD(crawler_server_sup, supervisor),
  {ok, {{one_for_one, 5, 10}, [Workers, Servers]}}.