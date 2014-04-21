-module(crawler).
-export([start/0, parse/2, parse/4, extract_links/2]).

start() ->
    {ok, _} = application:ensure_all_started(?MODULE).

parse(Link, Limit) -> 
    inets:start(),
    Set = sets:new(),
    Pid = spawn_link(?MODULE, parse, [self(), [Link], Set, Limit]),
    process_flag(trap_exit, true),
    wait(Pid).

wait(Pid) ->
    receive
        {'EXIT', Pid, Reason} ->
            io:format("Parser die with reason: \e[1;31m ~s\e[0;37m~n", [Reason]);
        {Link, 404, ReasonPhrase} ->
            io:format("~s - \e[1;31m 404 ~s\e[0;37m~n", [Link, ReasonPhrase]),
            wait(Pid);
        {Link, Status, ReasonPhrase} ->
            io:format("~s - \e[1;32m ~p ~s\e[0;37m~n", [Link, Status, ReasonPhrase]),
            wait(Pid)
    end.

parse(_, _, _, 0) -> exit("limit is over");
parse(_, [], _, _) -> exit("game over");
parse(Pid, [Link | T], Set, Limit) ->
    % erlang:display(Link),
    case sets:is_element(Link, Set) of 
        true ->
            parse(Pid, T, Set, Limit);
        false ->            
            case httpc:request(Link) of 
                {ok, {{_Version, 200, ReasonPhrase}, _Headers, Body}} -> 
                    ProcessSet = sets:add_element(Link, Set),
                    case http_uri:parse(Link) of
                        {ok,{http,[],Domen ,_Port,_Rel,[]}} ->
                            Links = extract_links(Domen, Body),
                            Pid ! {Link, 200, ReasonPhrase},
                            parse(Pid, Links ++ T, ProcessSet, Limit - 1);
                        _Other ->
                            % erlang:display(Other),
                            ProcessSet = sets:add_element(Link, Set),
                            parse(Pid, T, ProcessSet, Limit - 1)
                    end;
                {ok, {{_Version, Code, ReasonPhrase}, _Headers, _Body}} ->
                    Pid ! {Link, Code, ReasonPhrase},
                    ProcessSet = sets:add_element(Link, Set),
                    parse(Pid, T, ProcessSet, Limit - 1);
                _Other ->
                    % erlang:display(Other),
                    ProcessSet = sets:add_element(Link, Set),
                    parse(Pid, T, ProcessSet, Limit - 1)
            end
    end.
    
    
extract_links(Domen,Body) -> 
    RE = io_lib:format("href=\"(http://)?(/)?(www\.)?(~s)?/(?<href>.+?)\"", [Domen]),
    FinalRE = lists:flatten(RE),
    % erlang:display(FinalRE),

    case re:run(Body, FinalRE, [global, {capture, [href], list}]) of
        nomatch ->
            [];
        {match, Res} ->
            % erlang:display(Res),
            lists:map(fun(X) -> normalize(Domen, X) end, Res)
    end.

normalize(Domen, Relative) ->
    DefaultScheme = "http://",
    UrlIOList = io_lib:format("~s~s/~s", [DefaultScheme, Domen, Relative]),
    lists:flatten(UrlIOList).
