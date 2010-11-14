-module(es).
-export([start/0, eat/1]).
-import(lists,[reverse/1,sublist/2,nthtail/2]).
-include("es.hrl").

start() -> es_parse:file("es_init",fun eat/1).
eat(LD=#c{ws=[], in=[I|Is]})                            -> peat(LD#c{ws=[I],in=Is});
eat(LD=#c{ws=['create'|T], c=0, di=D, in=[I|Is]})       -> peat(LD#c{di=[{I,[]}|D],ws=T,in=Is});
eat(LD=#c{ws=['IMM'|T], di=[{N,Def}|D]})                -> peat(LD#c{di=[{{i,N},Def}|D],ws=T});
eat(LD=#c{ws=['word'|Ws], di=D, in=[H|T]})              -> peat(LD#c{di=[{H,[]}|D],ws=Ws,in=T});
eat(LD=#c{ws=['postpone'|Ws], di=[{N,F}|D], in=[I|Is]}) -> peat(LD#c{di=[{N,F++[I]}|D],ws=Ws,in=Is});
eat(LD=#c{ws=['\''|Ws], c=0, st=Ss,in=[I|Is]})           -> peat(LD#c{st=[I|Ss],ws=Ws,in=Is});
eat(LD=#c{ws=['[\']',W|Ws], c=1, st=S})         -> peat(LD#c{st=[W|S],ws=Ws});
eat(LD=#c{ws=[','|Ws], st=[H|S], di=[{N,F}|D]})         -> peat(LD#c{st=S,di=[{N,F++[H]}|D],ws=Ws});
eat(LD=#c{ws=['!'|Ws], st=[P,Da|S], di=D})         -> peat(LD#c{st=S,di=[{{p,P},[Da]}|D],ws=Ws});
eat(LD=#c{ws=['execute'|Ws], st=[H|S]})         -> peat(LD#c{st=S,ws=[H|Ws]});
eat(LD=#c{ws=['pop'|Ws], st=Ss, di=[{_,Def}|Ds]})       -> peat(LD#c{st=[Def|Ss],di=Ds,ws=Ws});
eat(LD=#c{ws=['swap'|Ws], st=[A,B|Ss]})                 -> peat(LD#c{st=[B,A|Ss],ws=Ws});
eat(LD=#c{ws=['dup'|Ws], st=[A|Ss]})                    -> peat(LD#c{st=[A,A|Ss],ws=Ws});
eat(LD=#c{ws=['2dup'|Ws], st=[A,B|Ss]})                 -> peat(LD#c{st=[A,B,A,B|Ss],ws=Ws});
eat(LD=#c{ws=['s'|Ws]})                                 -> seat(LD#c{ws=Ws});
eat(LD=#c{ws=['w'|Ws]})                                 -> w(LD),eat(LD#c{ws=Ws});
eat(LD=#c{ws=['.'|Ws],st=[S|_]})                       -> io:format("~p",[S]),eat(LD#c{ws=Ws});
eat(LD=#c{ws=[']'|T]})                                  -> peat(LD#c{c=1,ws=T}); % comp
eat(LD=#c{ws=['['|T]})                                  -> peat(LD#c{c=0,ws=T}); % immediate
eat(LD=#c{ws=['#'|_]})                                  -> peat(LD#c{ws=[],in=[]}); 
eat(LD=#c{debug=0,ws=['debug'|Ws]})                     -> peat(LD#c{debug=1,ws=Ws}); 
eat(LD=#c{debug=1,ws=['debug'|Ws]})                     -> peat(LD#c{debug=0,ws=Ws}); 

eat(LD=#c{ws=['list'|Ws], st=[S|Ss]})                   -> peat(LD#c{st=[sublist(Ss, S)|nthtail(S, Ss)],ws=Ws});
eat(LD=#c{ws=['tuple'|Ws], st=[S|Ss]})                  -> peat(LD#c{st=[list_to_tuple(sublist(Ss, S))|nthtail(S, Ss)],ws=Ws});
eat(LD=#c{ws=['length'|Ws], st=[S|Ss]})                 -> peat(LD#c{st=[length(S)|Ss],ws=Ws});
eat(LD=#c{ws=['l2t'|Ws], st=[S|Ss]})                    -> peat(LD#c{st=[list_to_tuple(S)|Ss],ws=Ws});
eat(LD=#c{ws=['rev'|Ws], st=[S|Ss]})                    -> peat(LD#c{st=[reverse(S)|Ss],ws=Ws});
eat(LD=#c{ws=['drop'|Ws], st=[_|Ss]})                   -> peat(LD#c{st=Ss,ws=Ws});
eat(LD=#c{ws=['mcall'|Ws], st=[M,F,A|Ss]})              -> peat(LD#c{st=[apply(M,F,A)|Ss],ws=Ws});
eat(LD=#c{ws=['call'|Ws], st=[F|Ss]})                   -> peat(LD#c{st=[apply(F,[])|Ss],ws=Ws});


eat(LD=#c{ws=[{'dump', [] }|Ws], st=S})                     -> peat(LD#c{st=['?'|S], ws=Ws});
eat(LD=#c{ws=[{'dump', [{Match,D}|_] }|Ws], st=[Match|Ss]}) -> peat(LD#c{st=[D|Ss],ws=Ws});
eat(LD=#c{ws=[{'dump', [_|D] }|Ws]})                        -> peat(LD#c{ws=[{'dump', D}|Ws]});
eat(LD=#c{ws=['dump'|Ws], di=D})                            -> peat(LD#c{ws=[{'dump', D}|Ws]});

eat(LD=#c{ws=[{'@', [] }|Ws], st=S})                     -> peat(LD#c{st=['?'|S], ws=Ws});
eat(LD=#c{ws=[{'@', [{{p,Match},[D]}|_] }|Ws], st=[Match|Ss]}) -> peat(LD#c{st=[D|Ss],ws=Ws});
eat(LD=#c{ws=[{'@', [_|D] }|Ws]})                        -> peat(LD#c{ws=[{'dump', D}|Ws]});
eat(LD=#c{ws=['@'|Ws], c=0, di=D})                            -> peat(LD#c{ws=[{'@', D}|Ws]});


eat(LD=#c{ws=[{'if',Xs},'then'|Ws], st=[S|Ss]}) when S==true -> peat(LD#c{st=Ss,ws=reverse(Xs)++Ws});
eat(LD=#c{ws=[{'if',Xs},X|Ws], st=[S|Ss]}) when S==true      -> peat(LD#c{st=[S|Ss],ws=[{'if',[X|Xs]}|Ws]});
eat(LD=#c{ws=['if',X|Ws], st=[S|Ss]}) when S==true           -> peat(LD#c{st=[S|Ss],ws=[{'if',[X]}|Ws]});
eat(LD=#c{ws=['if','then'|Ws], st=[S|Ss]}) when S==false     -> peat(LD#c{st=Ss,ws=Ws});
eat(LD=#c{ws=['if',_|Ws], st=[S|Ss]}) when S==false          -> peat(LD#c{st=[S|Ss],ws=['if'|Ws]});

eat(LD=#c{ws=['fun'|Ws], st=[T|S],di=D}) -> peat(LD#c{st=[fun() -> R=eat(LD#c{st=[],ws=T,di=D,in=[]}),R#c.st end|S],ws=Ws});

eat(LD=#c{st=[A,B|T],ws=['/='|Ws]}) -> peat(LD#c{st=[A/=B|T],ws=Ws});
eat(LD=#c{st=[A,B|T],ws=['=='|Ws]}) -> peat(LD#c{st=[A==B|T],ws=Ws});
eat(LD=#c{st=[A,B|T],ws=['++'|Ws]}) -> peat(LD#c{st=[A++B|T],ws=Ws});
eat(LD=#c{st=[A,B|T],ws=['+'|Ws]})  -> peat(LD#c{st=[A+B|T],ws=Ws});
eat(LD=#c{in=[],ws=[]})             -> LD;
eat(LD=#c{c=1})                     -> compile_or_imm(LD,LD);
eat(LD)                             -> exec_or_stack(LD,LD).

%% Find word, if no add to stack, else exec word.
exec_or_stack(L,#c{di=[{EQL,Def}|_],ws=[EQL|Ws]})     -> ieat(L#c{ws=Def++Ws}); %Found exec
exec_or_stack(L,#c{di=[{{i,EQL},Def}|_],ws=[EQL|Ws]}) -> ieat(L#c{ws=Def++Ws}); %Immediate exec
exec_or_stack(L,LD = #c{di=[{_,_}|D],ws=Ws})          -> exec_or_stack(L,LD#c{di=D,ws=Ws}); %Search
exec_or_stack(L,#c{st=S,di=[],ws=[W|Ws]})             -> ieat(L#c{st=[W|S],ws=Ws}). %Stack!
%%                                                            --------
  
%% Find imm, if no imm, compile to current word.
compile_or_imm(L,#c{di=[{{i,EQL},Def}|_],ws=[EQL|Ws]}) -> ieat(L#c{ws=Def++Ws}); %Immediate exec
compile_or_imm(L,LD = #c{di=[{_,_}|D],ws=Ws})          -> compile_or_imm(L,LD#c{di=D,ws=Ws}); %Search
compile_or_imm(L = #c{di=[{N,Def}|D],ws=[W|Ws]},_)     -> ieat(L#c{di=[{N,Def++[W]}|D],ws=Ws}).%Comp!
%%                                                             -------------------

seat(LD)   -> p("stack:",LD), eat(LD).
peat(LD=#c{debug=1})   -> p("p",LD), eat(LD);
peat(LD)   -> eat(LD).
ieat(LD=#c{debug=1})   -> p("i",LD), eat(LD);
ieat(LD)   -> eat(LD).
w(Out) -> 
    lists:map(fun(X) -> {N,Ds} = X,
                        case N of
                            {i,C} -> io:format("  :*~p* ",[C]);
                            {p,C} -> io:format("  : ~p->",[C]);
                            C -> io:format("  : ~p ",[C])
                        end,                            
                        lists:map(fun(W) -> 
                                          case W of
                                              '\'' -> io:format(" ' ");
                                              _ -> io:format("~p ",[W]) 
                                          end
                                  end, Ds),   
                        io:format(";~n")
              end, lists:sublist(Out#c.di,5)),
    io:format("~n").
p(Str,Out) -> 
    io:format("~s ~p ~p   exec~p",[Str,Out#c.c,Out#c.st,Out#c.ws]),
    io:format("<-~p",[Out#c.in]),
    io:format("~n").

%% create : postpone word postpone ]
%% : ;  postpone [ [ IMM
%% : var  create  0 , ;     
%% : const create , ;     
