/*
** decode/epilogue.sail: close scattered defs, helpers
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */

/* -- Close mapping clauses with illegal fallthrough case -- */

union clause ast =   ILLEGAL : bits(32)
union clause ast = C_ILLEGAL : bits(16)
mapping clause encdec_base =   ILLEGAL(s) <-> s
mapping clause encdec_comp = C_ILLEGAL(s) <-> s
function clause print_insn   ILLEGAL(s) =   "illegal " ^-^ bits_str(s)
function clause print_insn C_ILLEGAL(s) = "c.illegal " ^-^ bits_str(s)

/* -- Close all the scattered definitions -- */

end print_insn
end encdec_comp
end encdec_base
end ast

/* Utility functions that wrap the encode/decode mapping */

val cast insn_to_string : ast -> string
function insn_to_string i = print_insn(i)

val decode_base : bits(32) -> ast effect pure
function decode_base bv = encdec_base(bv)

val decode_comp : bits(16) -> ast effect pure
function decode_comp bv = encdec_comp(bv)

val encode_base : ast -> bits(32) effect pure
function encode_base bv = encdec_base(bv)

val encode_comp : ast -> bits(16) effect pure
function encode_comp bv = encdec_comp(bv)

/* -------------------------------------------------------------------------- */
/* -- El Fin (decode/epilogue.sail) ----------------------------------------- */
