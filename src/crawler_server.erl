-module(crawler_server).

-behaviour(gen_server).

-export([start_link/1]).
-export([parse/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Link) ->
  ok, _ = gen_server:start_link(?MODULE, [Link], []).

init(Link) ->
    Set = sets:new(),
    self() ! {links, Link},
    {ok, Set}.

parse(Link) ->
    crawler_worker:process_link(self(), Link).

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, N)  -> {noreply, N}.

handle_info({processed, Link, "404"}, State)  ->
    io:format("~s - \e[1;31m 404 \e[0;37m~n", [Link]),
    {noreply, State};

handle_info({processed, Link, badlink}, State)  ->
    io:format("~s - \e[1;31m badlink \e[0;37m~n", [Link]),
    {noreply, State};

handle_info({processed, Link, Status}, State)  ->
    io:format("~s - \e[1;32m ~s\e[0;37m~n", [Link, Status]),
    {noreply, State};

handle_info({links, Links}, State)  ->
    [parse(Link) || Link <- Links, not sets:is_element(Link, State)],
    NewState = add_elements(Links, State),
    {noreply, NewState};

handle_info(Info, State)  ->
    erlang:display(Info),
    % erlang:display(N),
    {noreply, State}.

terminate(_Reason, _N) ->
    io:format("~p stopping~n",[?MODULE]),
    ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

add_elements([], Set) -> Set;
add_elements([Link | T], Set) ->
    ProcessedSet = sets:add_element(Link, Set),
    add_elements(T, ProcessedSet).

