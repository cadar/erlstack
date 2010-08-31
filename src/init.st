# Define system
create ; ]. ['] .[ .[ IMM
create : ]. create_next ['] ]. ;
: [ create tmp ['] ]. ;
: ] pop_word ['] .[ ; IMM
: { create tmp ['] ]. ;
: } pop_word l2t ['] .[ ; IMM
.s
: jo 3 ;
: hej 2 2 jo 2 2 ;
: j 1 1 1 hej 1 1 1 ;
j
.s
