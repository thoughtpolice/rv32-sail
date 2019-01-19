/*
** preamble/asm.sail: asm clause utilities
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */

val spc : unit <-> string
val opt_spc : unit <-> string
val def_spc : unit <-> string

val hex_bits    : forall 'n . (atom('n), bits('n)) <-> string
val hex_bits_16 : bits(16) <-> string
val hex_bits_32 : bits(32) <-> string

val n_leading_spaces : string -> nat
function n_leading_spaces s =
  match s {
    "" => 0,
    _ => match string_take(s, 1) {
      " " => 1 + n_leading_spaces(string_drop(s, 1)),
      _ => 0
    }
  }

val spc_forwards : unit -> string
function spc_forwards () = " "
val spc_backwards : string -> unit
function spc_backwards s = ()
val spc_matches_prefix : string -> option((unit, nat))
function spc_matches_prefix s = {
  let n = n_leading_spaces(s);
  match n {
    0 => None(),
    _ => Some((), n)
  }
}

/* -------------------------------------------------------------------------- */
/* -- El Fin (preamble/asm.sail) -------------------------------------------- */
