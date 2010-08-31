-module(st).
-import(lists,[sublist/2,nthtail/2,reverse/1,map/2,reverse/1]).
-export([run/0,test/0,st/4]).
% Ws = Words, D = Dictionary, S = Stack, C = Context state interpret/compile => in/comp
% T = Top of stack, Nextw = Next word in program,
st([],D,S,C) -> {[],D,S,C};

st(['<<' |Ws], D,        S, in) -> st(reverse(Ws), D, S, in);
st(['+'  |Ws], D, [T,S2|S], in) -> st(Ws, D, [T+S2|S], in);
st([list |Ws], D,    [T|S], in) -> st(Ws, D, [sublist(S, T)|nthtail(T, S)], in);
st([rev  |Ws], D,    [T|S], in) -> st(Ws, D, [reverse(T)|S], in);
st([swap |Ws], D,  [A,B|S], in) -> st(Ws, D, [B,A|S], in);
st([size |Ws], D,        S, in) -> st(Ws, D, [length(S)|S], in);
st([drop |Ws], D,    [_|S], in) -> st(Ws, D, S, in);
st([mark |Ws], D,        S,  C) -> st(Ws, D, S, C);
st([l2t  |Ws], D,    [T|S],  C) -> st(Ws, D, [list_to_tuple(T)|S], C);
st(['#'  | _], D,        S, in) -> st([], D, S, in);
st(['.'  |Ws], D,    [T|S], in) -> io:format("~p", [T]), st(Ws, D, S, in);
st(['.s' |Ws], D,        S,  C) -> io:format("s~p ~s~n", [S, C]), print_dic(D), st(Ws, D, S, C);
st(['.w' |Ws], D,        S,  C) -> io:format("w~p ~s~n", [Ws, C]), st(Ws, D, S, C);

st([create |Ws], D, [T|S], in) -> st(Ws, [{T,[]}|D], S, in);
st([word   |Ws], D,     S, in) -> {Here,Ws1} = here(Ws,hd(Ws),[]), st(Ws1, [{Here,[]}|D], S, in);
st([','      |Ws],     [{W,Def}|D], [T|S], in) -> st(Ws,   [{W,Def++[T]}|D],   S, in);
st(['\'',   Nextw|Ws],           D,     S, in) -> st(Ws,            D, [Nextw|S], in);
st(['[\']', Nextw|Ws], [{W,Def}|D],   S, comp) -> st(Ws, [{W,Def++[Nextw]}|D], S, comp);

st(['.[' |Ws], D, S,  _) -> st(Ws, D, S, in);
st(['].' |Ws], D, S,  _) -> st(Ws, D, S, comp);

st(['IMM'  |Ws], [{W,Def}|D], S, in) -> st(Ws ,[{W,{im,Def}}|D], S, in);

% Compile anonymous function
st([pop_word|Ws], [{tmp,Comp}|D], S, in) -> st(Ws, D, [Comp|S], in);
st([apply   |Ws], D,[Body|S], in)          -> st(Body++Ws, D, S, in);
st([call    |Ws], D,[Fun,Mod,Args|S], in)  -> st(Ws, D, [apply(Fun, Mod, Args)|S], in);
%% 1. force execution of word, no compilning into word on dic,
%%    when done go back to compiling
%% 2. normal case, compiling to top of dictionary
st([W|Ws], [{Name,Def}|D], S, comp) ->
    case proplists:lookup(W,D) of
	{_, {im, Word}} ->
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

run() -> {ok, Binary} = file:read_file("init.st"),
         InitStack = string:tokens(erlang:binary_to_list(Binary), "\n"),
         run(InitStack, [], [], in).
run([], D, S, C) -> case io:get_line('') of
                        eof -> halt(0);
                        Line -> run([Line], D, S, C)
                    end;
run([Line|L], D, S, C) ->  Ws = parse(Line),
                           {_ ,D1 ,S1 ,C1} = st(Ws ,D ,S ,C),
                           run(L, D1 ,S1 ,C1).
parse(Line) ->
    NoLineBreak = re:replace(Line, "\n", "", [{return,list}]),
    Toks        = string:tokens(NoLineBreak, " "),
    AtomToks    = map(fun(X) -> list_to_atom(X) end, Toks),
    Ws          = map(fun(X) -> try list_to_integer(atom_to_list(X))
                                catch _:_ -> X end end, AtomToks),
    Ws.

test() -> 0.

print_dic(D) -> map(fun(X) -> io:format("    ~p~n",[X]) end, D).

here([],First,Acc) -> {First,tl(reverse(Acc))};
here([mark,W|Ws],_,Acc) -> {W,Acc++Ws};
here([W|Ws],PassOn,Acc) -> here(Ws,PassOn,[W|Acc]).

% 99%
%create_next ;; ]. ['] .[ .[ IMM
%create_next :: ]. create_next ['] ]. ;;
%:: hej 1 ;;

