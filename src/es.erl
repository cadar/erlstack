-module(es).
-export([start/0, eat/1]).
-import(lists,[reverse/1]).
-include("es.hrl").

start() -> es_parse:file("es_init",fun eat/1).
eat(LD = #c{     ws=[],          in=[I|Is]})    -> peat(LD#c{            ws=[I],in=Is});
eat(LD = #c{st=[H|S],di=D,ws=['create'|T]})     -> peat(LD#c{st=S,di=[{H,[]}|D],ws=T});
eat(LD = #c{di=D,ws=['word'|T],in=[I|Is]})      -> peat(LD#c{di=[{I,[]}|D],ws=T,in=Is});
eat(LD = #c{st=[H|S],di=[{N,Ws}|D],ws=[','|T]}) -> peat(LD#c{st=S,di=[{N,Ws++[H]}|D],ws=T});
eat(LD = #c{st=S,ws=['\''|Ws],in=[H|T]})        -> peat(LD#c{st=[H|S],ws=Ws,in=T});
eat(LD = #c{di=D,ws=[':'|Ws],in=[H|T]})         -> peat(LD#c{s=comp,di=[{H,[]}|D],ws=Ws,in=T});
eat(LD = #c{ws=[';'|Ws]})                       -> peat(LD#c{ws=[int|Ws]});
eat(LD = #c{st=Ss,di=[{N,Def}|Ds],ws=['dd'|Ws]})-> peat(LD#c{st=[Def|Ss],di=Ds,ws=Ws});
eat(LD = #c{st=[A,B|T],ws=['+'|Ws]})            -> peat(LD#c{st=[A+B|T],ws=Ws});

eat(LD = #c{ws=[']'|T]})                        -> peat(LD#c{s=comp,ws=T}); % comp
eat(LD = #c{ws=[comp|T]})                       -> peat(LD#c{s=comp,ws=T}); % comp
eat(LD = #c{ws=['['|T]})                        -> peat(LD#c{s=int,ws=T}); % immediate
eat(LD = #c{ws=[int|T]})                        -> peat(LD#c{s=int,ws=T}); % immediate
eat(LD = #c{di=[{N,Def}|D],ws=['IMM'|T]})       -> peat(LD#c{di=[{{i,N},Def}|D],ws=T});
eat(LD = #c{in=[],ws=[]})                       -> LD;

eat(LD = #c{s=comp})                            -> compile_or_imm(LD,LD);
eat(LD)                                         -> exec_or_stack(LD,LD).

%% Find word, if no add to stack, else exec word.
exec_or_stack(L,#c{di=[{EQL,Def}|_],ws=[EQL|Ws]})     -> ieat(L#c{ws=Def++Ws});      %Found
exec_or_stack(L,#c{di=[{{i,EQL},Def}|_],ws=[EQL|Ws]}) -> ieat(L#c{ws=Def++Ws}); 
exec_or_stack(L,LD = #c{di=[{_,_}|D],ws=Ws})          -> exec_or_stack(L,LD#c{di=D,ws=Ws}); %Search
exec_or_stack(L,#c{st=S,di=[],ws=[W|Ws]})             -> ieat(L#c{st=[W|S],ws=Ws}).
%%                                                            --------
  
%% Find imm, if no imm, compile to current word.
compile_or_imm(L,#c{di=[{{i,EQL},Def}|_],ws=[EQL|Ws]}) -> ceat(L#c{s=int,ws=Def++[comp]++Ws}); 
compile_or_imm(L,LD = #c{di=[{_,_}|D],ws=Ws})          -> compile_or_imm(L,LD#c{di=D,ws=Ws}); %Search
compile_or_imm(L = #c{di=[{N,Def}|D],ws=[W|Ws]},_)     -> ceat(L#c{di=[{N,Def++[W]}|D],ws=Ws}).
%%                                                           -------------------

ceat(LD)   -> p("c",LD), eat(LD).
aeat(LD)   -> p("a",LD), eat(LD).
peat(LD)   -> p("p",LD), eat(LD).
ieat(LD)   -> p("i",LD), eat(LD).
seat(LD)   -> p("s",LD), eat(LD).
p(Str,Out) -> 
    io:format("~s ~p ~p   exec~p    dic",[Str,Out#c.s,Out#c.st,Out#c.ws]),
    lists:map(fun(X) -> {N,Ds} = X,
                        case N of
                            {i,C} -> io:format("_~p_",[N]);
                            C -> io:format(":~p",[N])
                        end,                            
                        lists:map(fun(C) -> io:format(" ~p",[C]) 
                                  end, Ds),
                        io:format(" ;")
              end, lists:sublist(Out#c.di,3)),
    io:format("~n").
