\ hf.f -- hiForth "protolude", implementing non-microcoded ops
\ Copyright (C) 2017-2019 Austin Seipp. See Copyright Notice in COPYING

\ Commentary:

  \ NOTE: See the corresponding hf.S for more information, including changes.
  \
  \ TODO FIXME: Lorem ipsum...

\ ------------------------------------------------------------------------------

: TRUE  1 ;
: FALSE 0 ;

: TRUE?  ( bool -- bool ) TRUE = ;
: FALSE? ( bool -- bool ) FALSE = ;

: NOT ( bool -- bool ) 0 = ;
: <> ( x y -- bool ) = NOT ;

: B>C ( bool -- char )
  0BRANCH 20
  84
  BRANCH 12
  70
  ;

: '\n' 10 ;

\ TODO FIXME: use this implementation?
: >CFA2 ( a -- codeword )
  4+ 4+        \ skip length chain
  1+           \ skip flags byte
  DUP @        \ grab length         STACK: addr len
  SWAP 1+      \ skip length byte    STACK: len addr
  +            \ add                 STACK: addr

  3 +          \ round to 4-byte boundary
  3 INVERT AND
  ;

: DFA> ( codeword -- body ) >CFA 4+ ;

: FIND ( a len -- w ) ;

: PROTOMAIN ( -- result )
  TRUE FALSE <>   \ dumb test

  0BRANCH 16      \ conditional jump
  TRUE
  BRANCH 8        \ unconditional
  FALSE

  B>C EMIT '\n' EMIT \ spit out result
  0 \ result
  ;

\ ------------------------------------------------------------------------------
\ -- El fin --------------------------------------------------------------------

\ Local Variables:
\ mode: forth
\ fill-column: 80
\ indent-tabs-mode: nil
\ c-basic-offset: 2
\ buffer-file-coding-system: utf-8-unix
\ End:

\ ------------------------------- hf.f ends here -------------------------------
