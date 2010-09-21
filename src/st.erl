-module(st).
-import(lists,[sublist/2,nthtail/2,reverse/1,map/2,reverse/1]).
-export([run/0,test/0,st/4]).
%%
% Ws = Words, D = Dictionary, S = Stack, C = Context state interpret/compile => in/comp
% T = Top of stack, Nextw = Next word in program,
%%-----------------------------------------------------------------------------
st([],D,S,C) -> {D,S,C};
st(['#'  | _], D,        S, in) -> st([], D, S, in);
st(['<<' |Ws], D,        S, in) -> st(reverse(Ws), D, S, in);
st(['+'  |Ws], D, [T,S2|S], in) -> st(Ws, D, [T+S2|S], in);
st(['++' |Ws], D, [T,S2|S], in) -> st(Ws, D, [T++S2|S], in);
st([swap |Ws], D,  [A,B|S], in) -> st(Ws, D, [B,A|S], in);
st([dup  |Ws], D,    [T|S], in) -> st(Ws, D, [T,T|S], in);
st([drop |Ws], D,    [_|S], in) -> st(Ws, D, S, in);
st([len  |Ws], D,    [T|S], in) -> st(Ws, D, [length(T),T|S], in);
st([list |Ws], D,    [T|S], in) -> st(Ws, D, [sublist(S, T)|nthtail(T, S)], in);
st([tuple|Ws], D,    [T|S], in) -> st(Ws, D, [list_to_tuple(sublist(S, T))|nthtail(T, S)], in);
st([l2t  |Ws], D,    [T|S], in) -> st(Ws, D, [list_to_tuple(T)|S], in);
st([t2l  |Ws], D,    [T|S], in) -> st(Ws, D, [tuple_to_list(T)|S], in);
st([rev  |Ws], D,    [T|S], in) -> st(Ws, D, [reverse(T)|S], in);
st([exp  |Ws], D,    [T|S], in) -> st(Ws, D, lists:append(T,S), in);
st([hd   |Ws], D,    [T|S], in) -> st(Ws, D, [hd(T)|S], in);
st([tl   |Ws], D,    [T|S], in) -> st(Ws, D, [tl(T)|S], in);
st(['.'  |Ws], D,    [T|S], in) -> io:format("~p", [T]), st(Ws, D, S, in);
st(['.'  |Ws], D,       [], in) -> st(Ws, D, [], in);
st(['.s' |Ws], D,        S,  C) -> io:format("s~p ~s~n", [S, C]), print_dic(D), st(Ws, D, S, C);
st(['.w' |Ws], D,        S,  C) -> io:format("w~p ~s~n", [Ws, C]), st(Ws, D, S, C);
st([size |Ws], D,        S, in) -> st(Ws, D, [length(S)|S], in);
st([mark |Ws], D,        S,  C) -> st(Ws, D, S, C);
st([halt |_], _,        _,  _) -> halt(0);

st([dd   |Ws], [W|D],     S, C) -> st(Ws, D, [W|S], C); % dictionary delete
st([cc   |Ws],     D, [T|S], C) -> st(Ws, [T|D], S, C); % create dictionary

st(['i[' |Ws], D, S,  _) -> st(Ws, D, S, in);
st([']i' |Ws], D, S,  _) -> st(Ws, D, S, comp);
st([create |Ws], D, [T|S], in)                 -> 
    io:format("create: t:~p s:~p~n",[T,S]),
    st(Ws, [{T,[]}|D], S, in);
st(['IMM'  |Ws], [{W,Def}|D], S, in)           -> st(Ws ,[{W,{im,Def}}|D], S, in);
st([word   |Ws], D,     S, in)                 -> {Here,Ws1} = here(Ws,hd(Ws),[]), st(Ws1, [{Here,[]}|D], S, in);
st([','    |Ws], [{W,Def}|D], [T|S], in)       -> st(Ws,   [{W,Def++[T]}|D],   S, in);
st(['\'',   Nextw|Ws],           D,   S, in)   -> st(Ws,            D, [Nextw|S], in);
st(['[\']', Nextw|Ws], [{W,Def}|D],   S, comp) -> st(Ws, [{W,Def++[Nextw]}|D], S, comp);

% Compile anonymous function
st([pop_word|Ws], [{Type,Comp}|D], S, in)  -> st(Ws, D, [Comp|S], in);
st([apply   |Ws], D,[Body|S], in)         -> st(Body++Ws, D, S, in);

st(['fun'  |Ws], D, [T|S], in) -> st(Ws, D, [fun() -> {_, S1, _}=st(T, D, [], in),S1 end|S], in);
st(['1fun' |Ws], D, [T|S], in) -> st(Ws, D, [fun(Arg) -> {_ , S1, _} = st(T, D, [Arg], in), S1 end| S], in);
st([call   |Ws], D, [Fun|S], in) -> Res = apply(Fun, []), st(Ws, D, Res++S, in);
st([argcall|Ws], D, [Fun,Args|S], in) -> Res = apply(Fun, Args), st(Ws, D, Res++S, in);
st([mcall  |Ws], D, [Fun,Mod,Args|S], in) -> st(Ws, D, [apply(Fun, Mod, Args)|S], in);

%% 1. force execution of word, no compilning into word on dic,
%%    when done go back to compiling
%% 2. normal case, compiling to top of dictionary
st([W|Ws], [{Name,Def}|D], S, comp) ->
    case proplists:lookup(W,D) of
	{_, {im, Word}} ->
%            io:format("IMM-> ~p ~p im: ~p~n",[Name,W,Word++Ws]),
            st(Word++Ws, [{Name,Def}|D], S, in);
	_ ->
	    % io:format("comp-> ~p ~p~n",[Name,W]),
	    st(Ws, [{Name,Def++[W]}|D], S, comp)
    end;

st([W|Ws], D, S, in) ->
    case proplists:lookup(W, D) of
	none -> st(Ws, D, [W|S], in);              % Add to stack
	{_, {im, Word}} -> st(Word++Ws, D, S, in); % same as next line
	{_, Word} ->
	    % io:format("in-> ~p~n",[Word]),
	    st(Word++[mark|Ws], D, S, in) % lookup word, add in front of
    end.

%%=============================================================================

run() ->
    {ok, Binary} = file:read_file("init.st"),
    InitStack = string:tokens(erlang:binary_to_list(Binary), "\n"),
    run(InitStack, {[], [], in}).
run([], ErlStack)        ->  run([io:get_line('')], ErlStack);
run([eof], _)     ->  halt(0);
run(["q\n"], _)   ->  ok;
run([Line|L], {D, S, C}) -> run(L, st(parse(Line,[],[]) ,D ,S ,C)).


parse([],[],List)      -> reverse(List);
parse([],Acc,List)     -> parse([],[],[padd(Acc)|List]);
parse([10|T],Acc,List) -> parse(T,Acc,List);
parse([32|T],[],List)  -> parse(T,[],List);
parse([32|T],Acc,List) -> parse(T,[],[padd(Acc)|List]);
parse([34|T],[],List)  -> {Str,Rest} = pstr(T,[]), parse(Rest,[],[Str|List]);
parse([34|T],Acc,List) -> {Str,Rest} = pstr(T,[]), parse(Rest,[],[Str,padd(Acc)|List]);
parse([F|T],Acc,List)  -> parse(T,[F|Acc],List).

padd(Acc) -> try list_to_integer(reverse(Acc))
	     catch _:_ -> list_to_atom(reverse(Acc)) end.

pstr([34|T],Acc) -> {reverse(Acc), T};
pstr([H|T],Acc)  -> pstr(T,[H|Acc]).

%%-----------------------------------------------------------------------------


print_dic(D) -> map(fun(X) -> io:format("    ~p~n",[X]) end, sublist(D,3)).

here([],First,Acc)      -> {First,tl(reverse(Acc))};
here([mark,W|Ws],_,Acc) -> {W,Acc++Ws};
here([W|Ws],PassOn,Acc) -> here(Ws,PassOn,[W|Acc]).


test() -> 0.
