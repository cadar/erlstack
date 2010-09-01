# Define system
; create ]. ['] .[ .[ IMM
: create ]. word ['] ]. ;
: [ tmp create ['] ]. ;
: ] pop_word ['] .[ ; IMM
: { tmp create ['] ]. ;
: } pop_word l2t ['] .[ ; IMM
