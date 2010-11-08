-module(es).
-export([start/0, eat/1]).
-import(lists,[reverse/1]).
-include("es.hrl").

start() -> es_parse:file("es_init",fun eat/1).
eat(LD = #c{     ws=[],          in=[I|Is]})    -> peat(LD#c{            ws=[I],in=Is});
eat(LD = #c{di=D,ws=['create'|T],in=[I|Is]})    -> peat(LD#c{di=[{I,[]}|D],ws=T,in=Is});
eat(LD = #c{st=[H|S],di=[{N,Ws}|D],ws=[','|T]}) -> peat(LD#c{st=S,di=[{N,Ws++[H]}|D],ws=T});
eat(LD = #c{st=S,ws=['\''|Ws],in=[H|T]})        -> peat(LD#c{st=[H|S],ws=Ws,in=T});

eat(LD = #c{ws=['comp'|T]})                     -> peat(LD#c{s=comp,ws=T});
eat(LD = #c{ws=['int'|T]})                      -> peat(LD#c{s=int,ws=T});
eat(LD = #c{di=[{N,Def}|D],ws=['IMM'|T]})       -> eat(LD#c{di=[{{i,N},Def}|D],ws=T});
eat(LD = #c{in=[],ws=[]})                       -> LD;

eat(LD = #c{s=comp,di=[{N,Def}|D],ws=[W|Ws]})   -> peat(LD#c{di=[{N,Def++[W]}|D],ws=Ws}); 
eat(LD)                                         -> next(LD,LD).

next(L,#c{di=[{EQL,Def}|_],ws=[EQL|Ws]})     -> heat(L#c{ws=Def++Ws});      %Found
next(L,#c{di=[{{i,EQL},Def}|_],ws=[EQL|Ws]}) -> heat(L#c{s=int,ws=Def++[L#c.s]++Ws}); 
next(L,LD = #c{di=[{_,_}|D],ws=Ws})          -> next(L,LD#c{di=D,ws=Ws}); %Search
next(L=#c{s=int}, #c{st=S,di=[],ws=[W|Ws]})  -> seat(L#c{st=[W|S],ws=Ws}). 

peat(LD)   -> p("tra",LD), eat(LD).
heat(LD)   -> p("found",LD), eat(LD).
seat(LD)   -> p("miss",LD), eat(LD).
p(Str,Out) -> 
    io:format("~s ~p ~p   exec~p    dic",[Str,Out#c.s,Out#c.st,Out#c.ws]),
    lists:map(fun(X) -> {N,Ds} = X,
                        io:format(":~p",[N]),
                        lists:map(fun(C) -> io:format(" ~p",[C]) 
                                  end, Ds),
                        io:format(" ;")
              end, lists:sublist(Out#c.di,3)),
    io:format("~n").
