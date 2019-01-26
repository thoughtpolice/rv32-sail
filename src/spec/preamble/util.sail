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

val cast bool_to_bits : bool -> bits(1)
function bool_to_bits x = if x then 0b1 else 0b0

val cast bit_to_bool : bit -> bool
function bit_to_bool b = match b {
  bitone  => true,
  bitzero => false
}

val sra32 : (bits(32), bits(5)) -> bits(32)
function sra32 (v, shift) =
    let v64 : bits(64) = exts(v) in
    shiftright_bits(v64,shift)[31..0]

/* -------------------------------------------------------------------------- */
/* -- El Fin (preamble/util.sail) ------------------------------------------- */
