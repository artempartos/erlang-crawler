-module(crawler).
-export([start/0, parse/2, parse/4, extract_links/2]).

start() ->
    {ok, _} = application:ensure_all_started(?MODULE).

parse(Link, Limit) -> 
    inets:start(),
    Set = sets:new(),
    spawn_link(?MODULE, parse, [self(), [Link], Set, Limit]).

parse(Pid, _, _, 0) -> Pid ! limitover;
parse(Pid, [], _, _) -> Pid ! gameover;
parse(Pid, [Link | T], Set, Limit) ->
    case sets:is_element(Link, Set) of 
        true ->
            parse(Pid, T, Set, Limit);
        false ->            
            NewLinks = process_link(Pid, Link),
            ProcessedSet = sets:add_element(Link, Set),
            parse(Pid, T ++ NewLinks, ProcessedSet, Limit - 1)
    end.
    
extract_links(Domain,Body) -> 
    RE = io_lib:format("href=\"(http://)?(/)?(www\.)?(~s)?/(?<href>.+?)\"", [Domain]),
    FinalRE = lists:flatten(RE),
    case re:run(Body, FinalRE, [global, {capture, [href], list}]) of
        nomatch ->
            [];
        {match, Res} ->
            lists:map(fun(X) -> normalize(Domain, X) end, Res)
    end.

normalize(Domain, Relative) ->
    DefaultScheme = "http://",
    UrlIOList = io_lib:format("~s~s/~s", [DefaultScheme, Domain, Relative]),
    lists:flatten(UrlIOList).

process_link(Pid, Link) ->
    case httpc:request(Link) of 
        {ok, {{_Version, 200, ReasonPhrase}, _Headers, Body}} -> 
            case http_uri:parse(Link) of
                {ok,{http,[],Domain ,_Port,_Rel,[]}} ->
                    Pid ! {Link, 200, ReasonPhrase},
                    extract_links(Domain, Body);
                _Other ->
                    Pid ! {Link, badlink, _Other}
            end;
        {ok, {{_Version, Code, ReasonPhrase}, _Headers, _Body}} ->
            Pid ! {Link, Code, ReasonPhrase},
            [];
        _Other ->
            []
    end.

