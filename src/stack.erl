%%% File    : stack.erl
%%% Author  : Mats Westin <mats.westin@klarna.com>
%%% Description : A stack repl (see forth).
%%% Created :  4 Aug 2010 by Mats Westin <mats.westin@klarna.com>

-module(stack).
-export([run/5,eat/2,test/0]).
dump(Stck,Dic,Code)-> io:format("Stack: ~p~nDic  : ~p~nCode : ~p~n",[lists:reverse(Stck),Dic,Code]).

eat(0,[[]|Code]) -> Code;
eat(0,Code) -> Code;
eat(N,[[]|Code]) -> eat(N,Code);
eat(N,[[_H|T]|Code]) -> eat(N-1,[T|Code]).

run(comp, Stck,_Dic, [[';'|T]|Code], Words)-> {[T|Code],Words,Stck};
run(comp, Stck,_Dic, [[';IMM'|T]|Code], Words)-> {[T|Code],{im,Words},Stck};
run(comp, Stck, Dic, [['['|T]|Code], Words)-> run(exec,Stck,Dic,[T|Code],Words);
run(comp, Stck, Dic, [[H|T]|Code], Words)-> run(comp,Stck,Dic,[T|Code],Words++[H]);


run(exec, Stck, Dic, [[branch,N|T]|Code], Words)-> run(exec,Stck,Dic,eat(N,[T|Code]),Words);
run(exec, [H|Stck], Dic, [[zbranch,N|T]|Code], Words)-> C = case H == 0 of
								true -> eat(N,[T|Code]);
								_ ->  [T|Code]
							    end,
							run(exec,Stck,Dic,C,Words);



run(exec, Stck, Dic, [],_Words)-> dump(Stck,Dic,[]);
run(exec, Stck, Dic, [[]],_Words)-> dump(Stck,Dic,[]);
run(exec, Stck, Dic, [[]|Code], Words)->  run(exec, Stck, Dic, Code, Words);
run(exec, [A,B|Stck], Dic, [['+'|T]|Code], Words)-> run(exec, [A+B|Stck], Dic, [T|Code], Words);
run(exec, [H|Stck], Dic, [[']'|T]|Code], Words)-> dump(Stck,Dic,[T|Code]),
                   run(comp, Stck, Dic, [T|Code], Words++[H]);
run(exec, Stck, Dic, [[':',Word|T]|Code], Words)->
    {NewCode, Defined, NewStck} = run(comp, Stck, Dic, [T|Code], []),
    run(exec, NewStck, [{Word,Defined}|Dic], NewCode, Words);

run(imm, [T|Stck], Dic, [['branch',_ToBranch|T]|Code], Words)->
    run(exec, Stck, Dic, [T|Code], Words); 

run(exec, Stck, Dic, [[H|T]|Code], Words)->
    case proplists:get_value(H,Dic) of
	undefined -> run(exec, [H|Stck], Dic, [T|Code], Words);
	{im,Word} -> io:format("found imm: ~p~n",[H]),
		     run(exec, Stck, Dic, [Word|Code], Words);
	Word -> io:format("found: ~p~n",[H]),
		     run(exec, Stck, Dic, [Word,T|Code], Words)
    end.

test() -> [
	   stack:run(exec, [], [], [[':',iff,q,zbranch,0,';'],
				    [':',do,iff,1,then,';'],
				    [0,do,0]
				   ],[])
	   ].
	   
