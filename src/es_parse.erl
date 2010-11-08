-module(es_parse).
-export([file/2]).
-include("es.hrl").

file(F,R) -> {ok, Binary} = file:read_file(F),
             line(string:tokens(erlang:binary_to_list(Binary), "\n"), #c{},R).

line([], LD, R)       -> line([io:get_line('')], LD,R);
line([eof], _, _)     -> halt(0);
line(["q\n"], LD, _)  -> LD;
line([Line|L], LD, R) -> line(L, R(LD#c{in=parse(Line,[],[])}), R).

parse([],[],List)      -> lists:reverse(List);
parse([],Acc,List)     -> parse([],[],[padd(Acc)|List]);
parse([10|T],Acc,List) -> parse(T,Acc,List);
parse([32|T],[],List)  -> parse(T,[],List);
parse([32|T],Acc,List) -> parse(T,[],[padd(Acc)|List]);
parse([34|T],[],List)  -> {Str,Rest} = pstr(T,[]), parse(Rest,[],[Str|List]);
parse([34|T],Acc,List) -> {Str,Rest} = pstr(T,[]), parse(Rest,[],[Str,padd(Acc)|List]);
parse([F|T],Acc,List)  -> parse(T,[F|Acc],List).

padd(Acc) -> try list_to_integer(lists:reverse(Acc))
	     catch _:_ -> list_to_atom(lists:reverse(Acc)) end.

pstr([34|T],Acc) -> {lists:reverse(Acc), T};
pstr([H|T],Acc)  -> pstr(T,[H|Acc]).
