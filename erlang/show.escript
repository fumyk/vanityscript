#!/usr/bin/env escript
%% -*- erlang -*-

main([File]) -> 
    {ok, B} = file:read_file(File),
    Forms = scan(erl_scan:tokens([],binary_to_list(B),1),[]),
    F = fun(X) -> {ok,Y} = erl_parse:parse_form(X), Y end,
    Abstract = [F(X) || X <- Forms],
    io:format("~p~n", [Abstract]).

scan({done, {ok, T, N}, S}, Res) ->
    scan(erl_scan:tokens([], S, N), [T | Res]);
scan(_, Res) ->
    lists:reverse(Res).
