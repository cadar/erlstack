-module(st).
-export([run/0,test/0,st/4]).
% Ws = Words, D = Dictionary, S = Stack, C = Context state interpret/compile => in/comp
st(S,D,[],C) -> {S,D,[],C};

st([Top,S2|S], D, ['+'|Ws],in)   -> st([Top+S2|S],D,Ws,in);
st(S, D,          ['<-'|Ws],in)  -> st(S,D,lists:reverse(Ws),in);
st([Top|S], D,    [list|Ws],in) -> st([lists:sublist(S,Top)|lists:nthtail(Top,S)],D,Ws,in);
st([_|S], D,      [drop|Ws],in) -> st(S,D,Ws,in);
st(S, [_|D],      [dd|Ws],in)   -> st(S,D,Ws,in);

st([Top|S], D, ['.'|Ws],in) -> io:format("~p",[Top]),st(S,D,Ws,in);
st([], D,      ['.'|Ws],in) -> st([],D,Ws,in);

st(_, D, [clear|Ws],in) -> st([],D,Ws,in);
st(_, _, [halt|_],_)   -> halt(0);
st(S,D,['.s'|Ws],C)    -> io:format("s~p~n",[lists:reverse(S)]),st(S,D,Ws,C);
st(S,D,['.d'|Ws],C)    -> io:format("d(~p): ~p~n",[C,D]),st(S,D,Ws,C);

st([Top|S], D, [create|Ws],in)             -> st(S,[{Top,[]}|D],Ws,in);
st([Top|S], [{W,Def}|D], [','|T],in)       -> st(S,[{W,Def++[Top]}|D],T,in);
st(S,[{W,Def}|D],['\'',Nextw|Ws],in)     -> st(S,[{W,Def++[Nextw]}|D],Ws,in);
st(S,[{W,Def}|D],['[\']',Nextw|Ws],comp) -> st(S,[{W,Def++[Nextw]}|D],Ws,comp);

st(S,D, ['[['|Ws],_) -> st(S,D,Ws,in);
st(S,D, [']]'|Ws],_) -> st(S,D,Ws,comp);

% : ; IMM
st(S,D,[':',W|Ws],in) -> st([W|S],D,[create,']]'|Ws],in);
st(S,D,[';'|Ws],comp) -> st(S,D,['[['|Ws],comp);
st(S,[{W,Def}|D],['IMM'|Ws],in) -> st(S,[{W,{im,Def}}|D],Ws,in);

% Compile anonymous function
st(S,D,             ['['|Ws],in)    -> st(S,D,[tmp,create,']]'|Ws],in);
st(S,[{tmp,Comp}|D],[']'|Ws],comp)  -> st([Comp|S],D,['[['|Ws],comp);
st([Body|S],D,[apply|Ws],in)        -> st(S,D,Body++Ws,in);
st([Fun,Mod,Args|S],D,[call|Ws],in) -> Res = apply(Fun,Mod,Args),
                                       st([Res|S],D,Ws,in);
st(S,[{Name,Def}|D],[W|Ws],comp) ->
    case proplists:lookup(W,D) of
        %% force execution of word, no compilning into word on dic.
	{_,{im,ImWord}} -> io:format("IM! ~p => ~p -> ~p~n",[W,ImWord,[Ws]]),
                           st(S,[{Name,Def}|D],ImWord++[']]'|Ws],in);
	%% normal case, compiling to top of dictionary
	_ -> st(S,[{Name,Def++[W]}|D],Ws,comp)
    end;

st(S,D,[W|Ws],in) ->
    case proplists:lookup(W,D) of
	none -> st([W|S],D,Ws,in);            % Add to stack
	{_,{im,Word}} -> st(S,D,Word++Ws,in); % same as next line
	{_,Word} -> st(S,D,Word++Ws,in)       % lookup word, add in front of
    end.
     
run()-> run([],[],in).
run(S,D,C)->
    case io:get_line('') of
	eof -> halt(0);
	Line ->
	    NoLineBreak =  re:replace(Line,"\n","",[{return,list}]),
	    Toks = string:tokens(NoLineBreak," "),
	    AtomToks = lists:map(fun(X) -> list_to_atom(X) end, Toks),
	    Ws = lists:map(fun(X) -> try list_to_integer(atom_to_list(X)) 
                                     catch _:_ -> X end end, AtomToks),
            {S1,D1,_,C1} = st(S,D,Ws,C), 
	    run(S1,D1,C1)
    end.

test() -> 0.
