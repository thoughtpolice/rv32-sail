/*
** preamble/util.sail: misc definitions
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */

val exts : forall 'n 'm , 'm >= 'n . bits('n) -> bits('m)
function exts v = sign_extend(v, sizeof('m))

val extz : forall 'n 'm , 'm >= 'n . bits('n) -> bits('m)
function extz v = zero_extend(v, sizeof('m))

val to_bits : forall 'l, 'l >= 0 .(atom('l), int) -> bits('l)
function to_bits (l, n) = get_slice_int(l, n, 0)

/* -------------------------------------------------------------------------- */
/* -- El Fin (preamble/util.sail) ------------------------------------------- */
