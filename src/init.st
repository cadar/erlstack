# Define system
; create ]. ['] .[ .[ IMM
: create ]. word ['] ]. ;
: [ tmp create ['] ]. ;
: ] pop_word ['] .[ ; IMM
: { tmp create ['] ]. ;
: } pop_word l2t ['] .[ ; IMM


: 0node 0 list ['] node erlang call ; 
: 1create_schema 1 list create_schema mnesia call ; 
: 0mnesia-start 0 list start mnesia call ; 
: 2create_table 2 list create_table mnesia call ; 
: 1write 1 list write mnesia ' call ;
: 1read  1 list read mnesia ' call ;
: 1match 1 list match_object mnesia ' call ;
: 1tran  1 list transaction mnesia call ;
: f1tran rev fun 1tran ;
: 2format 2 list format io call drop ;
: p 0 list swap 2format ; 
: 1p 1 list "~p" 2format ;  
: lf "~n" ;
: pl lf p ; 

: append 1 list ++ ; 
: clean size list drop ; 
: atomdrop t2l tl hd hd ;
