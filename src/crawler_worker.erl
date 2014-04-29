-module(crawler_worker).

% -behaviour(poolboy_worker).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([process_link/2]).

start_link(Concurrency) ->
  ok, _ = gen_server:start_link({local, ?MODULE}, ?MODULE, Concurrency, []).

init(Concurrency) ->
  process_flag(trap_exit, true),
  Dict = dict:new(),
  {ok, {Dict, Concurrency}}.

process_link(Pid, Link) ->
    gen_server:cast(crawler_worker, {process, Link, Pid}).

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast({process, Link, From}, State)  ->
    {Dict , Concurrency } = State,
    % erlang:display(Concurrency),
    case ibrowse:send_req(Link, [], get, [], [{stream_to, self()}]) of
        {ibrowse_req_id, RequestID} ->
            Value = { From, Link, "", [] },
            NewDict = dict:append(RequestID, Value, Dict),
            NewState = {NewDict , Concurrency-1},
            {noreply, NewState};
        {error, Error} ->
            erlang:display(Error),
            {noreply, State}
    end;

handle_cast(Other, State)  ->
    erlang:display(Other),
    {noreply, State}.

handle_info({ibrowse_async_headers, RequestID, Status, _Headers}, State)  ->
    {Dict , Concurrency } = State,
    [ { From, Link, _, _ } | T ] = dict:fetch(RequestID, Dict),
    DelDict = dict:erase(RequestID, Dict),
    Value = { From, Link, Status, [] },
    NewDict = dict:append(RequestID, Value, DelDict),
    NewState = {NewDict , Concurrency},
    {noreply, NewState};

handle_info({ibrowse_async_response, RequestID, Body}, State)  ->
    {Dict , Concurrency } = State,
    [ { From, Link, Status, _ } | T ] = dict:fetch(RequestID, Dict),
    DelDict = dict:erase(RequestID, Dict),
    Value = { From, Link, Status, Body },
    NewDict = dict:append(RequestID, Value, DelDict),
    NewState = {NewDict , Concurrency},
    {noreply, NewState};

handle_info({ibrowse_async_response_end, RequestID}, State)  ->
    {Dict , Concurrency } = State,
    % erlang:display(Dict),
    [ { From, Link, Status, Body } | T ] = dict:fetch(RequestID, Dict),
    DelDict = dict:erase(RequestID, Dict),
    NewState = {DelDict , Concurrency + 1},

    case http_uri:parse(Link) of
        {ok,{http,[],Domain ,_Port,_Rel,[]}} ->
            From ! {processed, Link, Status},
            Links = extract_links(Domain, Body),
            From ! {links, Links};
        _Other ->
            erlang:display("Bad link"),
            []
    end,

    % erlang:display(Links),
    {noreply, NewState};

handle_info(Msg, State)  ->
    {noreply, State}.

terminate(_Reason, _N) ->
    io:format("~p stopping~n",[?MODULE]),
    ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

extract_links(Domain,Body) ->
    RE = io_lib:format("href=\"(http://)?(/)?(www\.)?(~s)?/(?<href>.+?)\"", [Domain]),
    FinalRE = lists:flatten(RE),
    case re:run(Body, FinalRE, [global, {capture, [href], list}]) of
        nomatch ->
            [];
        {match, Res} ->
            [normalize(Domain, Link) || Link <- Res ]
    end.

normalize(Domain, Relative) ->
    DefaultScheme = "http://",
    UrlIOList = io_lib:format("~s~s/~s", [DefaultScheme, Domain, Relative]),
    lists:flatten(UrlIOList).
