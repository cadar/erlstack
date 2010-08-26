%%% File    : stack.erl
%%% Author  : Mats Westin <mats.westin@klarna.com>
%%% Description : A stack repl (see forth).
%%% Created :  4 Aug 2010 by Mats Westin <mats.westin@klarna.com>

-module(stack).
-export([run/5,eat/2,test/0]).
dump(Stck,Dic,Code)-> io:format("Dic  : ~p~nStack: ~p~nCode : ~p~n",[lists:reverse(Dic),lists:reverse(Stck),Code]).

eat(0,[[]|Code]) -> Code;
eat(0,Code) -> Code;
eat(N,[[]|Code]) -> eat(N,Code);
eat(N,[[_H|T]|Code]) -> eat(N-1,[T|Code]).

run(comp, Stck,_Dic, [[';'|T]|Code], Words)-> {[T|Code],Words,Stck};
run(comp, Stck,_Dic, [[';IMM'|T]|Code], Words)-> {[T|Code],{im,Words},Stck};
run(comp, Stck, Dic, [['['|T]|Code], Words)-> run(exec,Stck,Dic,[T|Code],Words);
run(comp, Stck, Dic, [['stack'|T]|Code], Words)-> run(comp,[],Dic,[T|Code],Words++Stck);
run(Stat, [H|Stck], Dic, [['qhead'|T]|Code], Words)-> run(Stat,Stck,Dic,[T|Code],Words++[H]);
%run(Stat, Stck, Dic, [['quote',H|T]|Code], Words)->   run(Stat,Stck,Dic,[T|Code],Words++[{Stat,H}]);
run(comp, Stck, Dic, [[H|T]|Code], Words)->
    case proplists:get_value(H,Dic) of
	undefined -> 
		     run(comp,Stck,Dic,[T|Code],Words++[H]);
	{im,Word} -> 
		     {S,D,W} = run(exec,Stck,Dic,[Word],Words),
		     run(comp,S,D,[T|Code],W);
	_Word -> 
	    run(comp,Stck,Dic,[T|Code],Words++[H])
    end;
run(exec, Stck, Dic, [[branch,N|T]|Code], Words)-> run(exec,Stck,Dic,eat(N,[T|Code]),Words);
run(exec, [H|Stck], Dic, [[zbranch,N|T]|Code], Words)-> C = case H == 0 of
								true -> eat(N,[T|Code]);
								_ ->  [T|Code]
							    end,
							run(exec,Stck,Dic,C,Words);
run(exec, Stck, Dic, [], Words)-> {Stck,Dic, Words};
run(exec, Stck, Dic, [[]], Words)-> {Stck, Dic, Words};
run(exec, Stck, Dic, [[]|Code], Words)->  run(exec, Stck, Dic, Code, Words);
run(exec, [A,B|Stck], Dic, [['+'|T]|Code], Words)-> run(exec, [A+B|Stck], Dic, [T|Code], Words);
run(exec, Stck, Dic, [['code'|T]|Code], Words)-> run(exec, [T,Code|Stck], Dic, [T|Code], Words);
run(exec, [H|Stck], Dic, [['eat'|T]|Code], Words)-> NCode = eat(H,[T|Code]),
						    run(exec, [T|Stck], Dic, NCode, Words);
run(exec, [H|Stck], Dic, [[']'|T]|Code], Words)-> run(comp, Stck, Dic, [T|Code], Words++[H]);
run(exec, Stck, Dic, [[':',Word|T]|Code], Words)->
    {NewCode, Defined, NewStck} = run(comp, Stck, Dic, [T|Code], []),
    run(exec, NewStck, [{Word,Defined}|Dic], NewCode, Words);

run(exec, Stck, Dic, [[H|T]|Code], Words)->
    case proplists:get_value(H,Dic) of
	undefined -> run(exec, [H|Stck], Dic, [T|Code], Words);
	{im,_Word} -> exit(error_no_imm_words_in_repl_mode);
	Word -> io:format("found: ~p~n",[H]),
		     run(exec, Stck, Dic, [Word,T|Code], Words)
    end.

test() -> [
	   stack:run(exec, [], [], [[':',a,aa,';'],
				    [':',b,bb,';'],
				    [':',c,cc,';'],
				    [a1,b2],
				    [':',iff,t1,code,';IMM'],
				    [':',then,a,b,c,';IMM'],
				    [':',do,0,iff,n,g,then,then,do_end,';'],
				    [v]
				   ],[])
	   ].
