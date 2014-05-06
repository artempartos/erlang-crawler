-module(crawler_server).

-behaviour(gen_server).

-export([start_link/1]).
-export([parse/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {links=sets:new(), limit=0}).

start_link(Args) ->
  ok, _ = gen_server:start_link(?MODULE, Args, []).

init([Link , Limit]) ->
    process_flag(trap_exit, true),
    self() ! {links, [Link]},
    {ok, #state{limit=Limit}}.

parse(Link) ->
    crawler_worker:process_link(self(), Link).

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, N)  -> {noreply, N}.

handle_info({processed, Link, Status}, #state{links=Set,limit=0}=State)  ->
    io:format("\e[1;31mlimit is over \e[0;37m~n"),
    {stop, normal, State};

handle_info({processed, Link, "404"}, #state{links=Set,limit=unlimited}=State)  ->
    #state{limit=Limit, links=Set} = State,
    io:format("~s - \e[1;31m 404 \e[0;37m~n", [Link]),
    {noreply, #state{links=Set, limit=unlimited}};

handle_info({processed, Link, badlink}, #state{links=Set,limit=unlimited}=State)  ->
    #state{limit=Limit, links=Set} = State,
    io:format("~s - \e[1;31m badlink \e[0;37m~n", [Link]),
    {noreply, #state{links=Set, limit=unlimited}};

handle_info({processed, Link, Status}, #state{links=Set,limit=unlimited}=State)  ->
    #state{limit=Limit, links=Set} = State,
    io:format("~s - \e[1;32m ~s\e[0;37m~n", [Link, Status]),
    {noreply, #state{links=Set, limit=unlimited}};

handle_info({processed, Link, "404"}, State)  ->
    #state{limit=Limit, links=Set} = State,
    io:format("~s - \e[1;31m 404 \e[0;37m~n", [Link]),
    {noreply, #state{links=Set, limit=Limit - 1}};

handle_info({processed, Link, badlink}, State)  ->
    #state{limit=Limit, links=Set} = State,
    io:format("~s - \e[1;31m badlink \e[0;37m~n", [Link]),
    {noreply, #state{links=Set, limit=Limit - 1}};

handle_info({processed, Link, Status}, State)  ->
    #state{limit=Limit, links=Set} = State,
    io:format("~s - \e[1;32m ~s\e[0;37m~n", [Link, Status]),
    {noreply, #state{links=Set, limit=Limit - 1}};


handle_info({links, Links}, State)  ->
    #state{links=Set,limit=Limit} = State,
    [parse(Link) || Link <- Links, not sets:is_element(Link, Set)],
    NewSet = add_elements(Links, Set),
    {noreply, #state{links=NewSet, limit=Limit}};

handle_info(Info, State)  ->
    erlang:display(Info),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("~p stopping~n",[?MODULE]),
    ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

add_elements([], Set) -> Set;
add_elements([Link | T], Set) ->
    ProcessedSet = sets:add_element(Link, Set),
    add_elements(T, ProcessedSet).

