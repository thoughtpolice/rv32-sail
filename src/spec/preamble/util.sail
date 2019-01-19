/*
** preamble/util.sail: misc definitions
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */

val EXTS : forall 'n 'm , 'm >= 'n . bits('n) -> bits('m)
function EXTS v = sign_extend(v, sizeof('m))

val EXTZ : forall 'n 'm , 'm >= 'n . bits('n) -> bits('m)
function EXTZ v = zero_extend(v, sizeof('m))

val to_bits : forall 'l, 'l >= 0 .(atom('l), int) -> bits('l)
function to_bits (l, n) = get_slice_int(l, n, 0)

/* -------------------------------------------------------------------------- */
/* -- El Fin (preamble/util.sail) ------------------------------------------- */
