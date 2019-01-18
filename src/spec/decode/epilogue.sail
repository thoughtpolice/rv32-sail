/*
** decode/epilogue.sail: close scattered defs, helpers
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */
/* -- Close scattered defs with an exception clause ------------------------- */

/* -- Close all the scattered definitions -- */

end encdec_comp
end encdec_base
end ast

/* Utility functions that wrap the encode/decode mapping */

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
