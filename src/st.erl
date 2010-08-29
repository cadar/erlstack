-module(st).
-import(lists,[sublist/2,nthtail/2,reverse/1,map/2]).
-export([run/0,test/0,st/4]).
% Ws = Words, D = Dictionary, S = Stack, C = Context state interpret/compile => in/comp
st([],D,S,C) -> {[],D,S,C};

st(['<<' |Ws], D,          S, in) -> st(reverse(Ws), D, S, in);
st(['+'  |Ws], D, [Top,S2|S], in) -> st(Ws, D, [Top+S2|S], in);
st([list |Ws], D,    [Top|S], in) -> st(Ws, D, [sublist(S, Top)|nthtail(Top, S)], in);
st([drop |Ws], D,      [_|S], in) -> st(Ws, D, S, in);
st(['.'  |Ws], D,    [Top|S], in) -> io:format("~p", [Top]), st(Ws, D, S, in);
st(['.s' |Ws], D,          S,  C) -> io:format("s~p~n", [S]), st(Ws, D, S, C);
st(['.d' |Ws], D,          S,  C) -> io:format("d(~p): ~p~n", [C, D]), st(Ws, D, S, C);

st([create|Ws],                  D, [Top|S], in) -> st(Ws, [{Top,[]}|D],         S, in);
st([','   |Ws],        [{W,Def}|D], [Top|S], in) -> st(Ws, [{W,Def++[Top]}|D],   S, in);
st(['\'',   Nextw|Ws],           D,       S, in) -> st(Ws,  D, [Nextw|S], in);
st(['[\']', Nextw|Ws], [{W,Def}|D],     S, comp) -> st(Ws, [{W,Def++[Nextw]}|D], S, comp);

st(['[[' |Ws], D,          S,  _) -> st(Ws, D, S, in);
st([']]' |Ws], D,          S,  _) -> st(Ws, D, S, comp);

%; = : ; ['] [[ [[ IMM
% : IMM
st([':',  W|Ws], D ,S , in)          -> st([create,']]'|Ws], D, [W|S], in);
st([';'|Ws], D ,S , comp)          -> st(['[['|Ws], D, S, comp);
st(['IMM'  |Ws], [{W,Def}|D], S, in) -> st(Ws ,[{W,{im,Def}}|D], S, in);

% Compile anonymous function
st(['{'  |Ws], D, S, in)                -> st([tup,create,']]'|Ws], D, S, in);
st(['}'  |Ws], [{tup,Comp}|D], S, comp) -> st(['[['|Ws], D, [list_to_tuple(Comp)|S], comp);
st(['['  |Ws], D, S, _)                 -> st([tmp,create,']]'|Ws], D, S, in);
st([']'  |Ws], [{tmp,Comp}|D], S, comp) -> st(['[['|Ws], D, [Comp|S], comp);
st([apply|Ws], D,[Body|S], in)          -> st(Body++Ws, D, S, in);
st([call |Ws], D,[Fun,Mod,Args|S], in)  -> Res = apply(Fun, Mod, Args),
                                           st(Ws, D, [Res|S], in);
%% 1. force execution of word, no compilning into word on dic, 
%%    when done go back to compiling
%% 2. normal case, compiling to top of dictionary
st([W|Ws], [{Name,Def}|D], S, comp) ->
    case proplists:lookup(W,D) of
	{_, {im, Word}} -> 
            st(Word++Ws, [{Name,Def}|D], S, in);
	_ -> st(Ws, [{Name,Def++[W]}|D], S, comp)
    end;

st([W|Ws], D, S, in) ->
    case proplists:lookup(W, D) of 
	none -> st(Ws, D, [W|S], in);              % Add to stack
	{_, {im, Word}} -> 
            io:format("-> ~p~n",[Word++Ws]),
            st(Word++Ws, D, S, in); % same as next line
	{_, Word} -> st(Word++Ws, D, S, in)        % lookup word, add in front of
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


d(S) -> io:format("~s~n",[S]).
