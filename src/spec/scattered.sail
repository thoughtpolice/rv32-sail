/*
** scattered.sail: end module that closes scattered definitions, etc.
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */
/* -- Close scattered defs with an exception clause ------------------------- */

/* -- Close all the scattered definitions -- */

end execute
end encdec_comp
end encdec_base
end assembly
end ast

val cast print_insn : ast -> string
function print_insn i = assembly(i)

val decode_base : bits(32) -> ast effect pure
function decode_base bv = encdec_base(bv)

val decode_comp : bits(16) -> ast effect pure
function decode_comp bv = encdec_comp(bv)

/* -------------------------------------------------------------------------- */
/* -- El Fin (scattered.sail) ----------------------------------------------- */
