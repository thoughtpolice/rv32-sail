/*
** preamble.sail: basic, reusable sail imports, types, and definitions.
** See Copyright Notice in LICENSE.txt
*/

/* TODO FIXME: describe */
default Order dec

/* -------------------------------------------------------------------------- */
/* -- Imports --------------------------------------------------------------- */

$include <prelude.sail>
$include <elf.sail>
$include <smt.sail>
$include <string.sail>
$include <trace.sail>

/* -------------------------------------------------------------------------- */

/* SAIL BUG: apparently fixity does not import properly from sail $include
** directives, so we have to redeclare it here.
*/

infixl 9 ^-^

/* -------------------------------------------------------------------------- */
/* -- FFI C Bindings -------------------------------------------------------- */

val get_verbosity = { c: "sail_get_verbosity" } : unit -> bits(64)
function get_verbosity () = 0x0000000000000000

val "get_cycle_count"     : unit -> int
val "get_time_ns"         : unit -> int
val "cycle_count"         : unit -> unit
val "cycle_limit_reached" : unit -> bool
function cycle_limit_reached () = false

/* -------------------------------------------------------------------------- */
/* -- Assembler clause utilities -------------------------------------------- */

// TODO FIXME: why aren't... all of these in the prelude? too new?

val spc : unit <-> string
val opt_spc : unit <-> string
val def_spc : unit <-> string

val hex_bits    : forall 'n . (atom('n), bits('n)) <-> string
val hex_bits_16 : bits(16) <-> string
val hex_bits_32 : bits(32) <-> string

val "string_length"     : string -> nat
val "string_startswith" : (string, string) -> bool
val "string_drop"       : (string, nat) -> string
val "string_take"       : (string, nat) -> string
val "string_length"     : string -> nat
val maybe_int_of_prefix = "maybe_int_of_prefix" : string -> option((int, nat))
val maybe_nat_of_prefix = "maybe_nat_of_prefix" : string -> option((nat, nat))
val maybe_int_of_string = "maybe_int_of_string" : string -> option(int)

val string_append = { c: "concat_str", _: "string_append" } : (string, string) -> string

// TODO: why isn't eq_string overloaded for this in prelude, at least?
overload operator == = {eq_int, eq_bit, eq_bool, eq_unit, eq_vec, eq_string, eq_real}

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

val "decimal_string_of_bits" : forall 'n. bits('n) -> string

/* -------------------------------------------------------------------------- */
/* -- Utility functions ----------------------------------------------------- */

val to_bits : forall 'l, 'l >= 0 .(atom('l), int) -> bits('l)
function to_bits (l, n) = get_slice_int(l, n, 0)

// why so long? :(
val print_ln : string -> unit
val prerr_ln : string -> unit
function print_ln s = print_endline(s)
function prerr_ln s = prerr_endline(s)

/* -------------------------------------------------------------------------- */
/* -- RAM Primitives -------------------------------------------------------- */

val "write_ram" : forall 'n 'm.
  (atom('m), atom('n), bits('m), bits('m), bits(8 * 'n)) -> unit effect { wmem }

val "read_ram" : forall 'n 'm.
  (atom('m), atom('n), bits('m), bits('m)) -> bits(8 * 'n) effect { rmem }

/* -------------------------------------------------------------------------- */
/* -- El Fin (preamble.sail) ------------------------------------------------ */
