-module(es).
-export([start/0, eat/1]).
-import(lists,[reverse/1]).
-include("es.hrl").

start() -> es_parse:file("es_init",fun eat/1).
eat(LD=#c{ws=[], in=[I|Is]})                            -> peat(LD#c{ws=[I],in=Is});
eat(LD=#c{ws=['create'|T], c=0, di=D, in=[I|Is]})       -> peat(LD#c{di=[{I,[]}|D],ws=T,in=Is});
eat(LD=#c{ws=['IMM'|T], di=[{N,Def}|D]})                -> peat(LD#c{di=[{{i,N},Def}|D],ws=T});
eat(LD=#c{ws=['word'|Ws], di=D, in=[H|T]})              -> peat(LD#c{di=[{H,[]}|D],ws=Ws,in=T});
eat(LD=#c{ws=['postpone'|Ws], di=[{N,F}|D], in=[I|Is]}) -> peat(LD#c{di=[{N,F++[I]}|D],ws=Ws,in=Is});
eat(LD=#c{ws=['\''|Ws], c=0, st=S, in=[H|T]})           -> peat(LD#c{st=[H|S],ws=Ws,in=T});
eat(LD=#c{ws=['[\']'|Ws], c=1, st=S, in=[H|T]})         -> peat(LD#c{st=[H|S],ws=Ws,in=T});
eat(LD=#c{ws=[','|Ws], st=[H|S], di=[{N,F}|D]})         -> peat(LD#c{st=S,di=[{N,F++[H]}|D],ws=Ws});
eat(LD=#c{ws=['pop'|Ws], st=Ss, di=[{_,Def}|Ds]})       -> peat(LD#c{st=[Def|Ss],di=Ds,ws=Ws});
eat(LD=#c{ws=[']'|T]})                                  -> peat(LD#c{c=1,ws=T}); % comp
eat(LD=#c{ws=['['|T]})                                  -> peat(LD#c{c=0,ws=T}); % immediate


eat(LD=#c{st=[A,B|T],ws=['+'|Ws]})                      -> peat(LD#c{st=[A+B|T],ws=Ws});
eat(LD=#c{in=[],ws=[]})                                 -> LD;
eat(LD=#c{c=1})                                         -> compile_or_imm(LD,LD);
eat(LD)                                                 -> exec_or_stack(LD,LD).

%% Find word, if no add to stack, else exec word.
exec_or_stack(L,#c{di=[{EQL,Def}|_],ws=[EQL|Ws]})     -> ieat(L#c{ws=Def++Ws}); %Found exec
exec_or_stack(L,#c{di=[{{i,EQL},Def}|_],ws=[EQL|Ws]}) -> ieat(L#c{ws=Def++Ws}); %Immediate exec
exec_or_stack(L,LD = #c{di=[{_,_}|D],ws=Ws})          -> exec_or_stack(L,LD#c{di=D,ws=Ws}); %Search
exec_or_stack(L,#c{st=S,di=[],ws=[W|Ws]})             -> ieat(L#c{st=[W|S],ws=Ws}). %Stack!
%%                                                            --------
  
%% Find imm, if no imm, compile to current word.
compile_or_imm(L,#c{di=[{{i,EQL},Def}|_],ws=[EQL|Ws]}) -> ceat(L#c{ws=Def++Ws}); %Immediate exec
compile_or_imm(L,LD = #c{di=[{_,_}|D],ws=Ws})          -> compile_or_imm(L,LD#c{di=D,ws=Ws}); %Search
compile_or_imm(L = #c{di=[{N,Def}|D],ws=[W|Ws]},_)     -> ceat(L#c{di=[{N,Def++[W]}|D],ws=Ws}).%Comp!
%%                                                             -------------------

ceat(LD)   -> p("c",LD), eat(LD).
peat(LD)   -> p("p",LD), eat(LD).
ieat(LD)   -> p("i",LD), eat(LD).
p(Str,Out) -> 
    io:format("~s ~p ~p   exec~p",[Str,Out#c.c,Out#c.st,Out#c.ws]),
    io:format("<-~p    dic",[Out#c.in]),
    lists:map(fun(X) -> {N,Ds} = X,
                        case N of
                            {i,C} -> io:format(":*~p* ",[C]);
                            C -> io:format(":~p ",[C])
                        end,                            
                        lists:map(fun(W) -> io:format("~p ",[W]) 
                                  end, Ds),
                        io:format(";   ")
              end, lists:sublist(Out#c.di,3)),
    io:format("~n").

%% create : postpone word postpone ]
%% : ;  postpone [ [ IMM
%% : var  create  0 , ;     
%% : const create , ;     
