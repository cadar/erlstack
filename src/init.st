# Define system
; create ]i        ['] i[ i[ IMM
: create ]i word   ['] ]i ;
: [ ' comp create  ['] ]i ; IMM
: { ' comp create  ['] ]i ; IMM
: ] pop_word       ['] i[ ; IMM
: } pop_word l2t   ['] i[ ; IMM

: 2format 2 list format io mcall drop ;
"*** print functions~n" 0 list swap 2format 
: 1f 1 list format io mcall drop ;
: 1p 1 list "~p" 2format ;  
: l "~n" 1f ; 
: 1pl 1p l ; 
: 1sl 1f l ; 

"*** defining mnesia helpers" 1sl
: 0node 0 list ['] node erlang mcall ; 
: 1create_schema 1 list create_schema mnesia mcall ; 
: 0mnesia-start 0 list start mnesia mcall ; 
: 2create_table 2 list create_table mnesia mcall ; 
: 1write 1 list write mnesia ' mcall ;
: 1read  1 list read mnesia ' mcall ;
: 1match 1 list match_object mnesia ' mcall ;
: 1tran  1 list transaction mnesia mcall ;
: f1tran rev fun 1tran ;
: 2map 1fun 2 list map lists mcall ; 

"*** list helpers" 1sl
: append 1 list ++ ; 
: cons 1 list ++ ; 
: clean size list drop ; 
: th t2l tl hd ;
: thh t2l tl hd hd ;
: thhh t2l tl hd hd hd ;
: concat swap ++ ; 
: flat 1 list flatten lists mcall ; 

: rapply rev apply ;
0node 1 list 1create_schema
0mnesia-start
: fields length name 2 list ;
: att fields attributes 2 tuple ;
: disc 0node 1 list disc_copies 2 tuple ; 
: arg2 att disc 2 list ;
: arg1 person ;
arg2 arg1 2create_table
: rec person append ; 
drop drop drop 
: lambda-write rec l2t 1write 4 list rev fun ; 

"*** write to mnesia" 1sl
190 Cadar 2 list lambda-write 1tran drop
175 Dylan 2 list lambda-write 1tran drop
175 Willy 2 list lambda-write 1tran drop
175 Billy 2 list lambda-write 1tran drop

"*** test read from mnesia" 1sl
: lambda-read 1read 4 list rev fun ;
Cadar person 2 tuple lambda-read 1tran thhh 1pl
Dylan person 2 tuple 1read 4 list f1tran thhh 1pl
99
175 _ person 3 tuple 1match 4 list f1tran thh

l
"--- Stack and Dictionary ---" 1sl .s
