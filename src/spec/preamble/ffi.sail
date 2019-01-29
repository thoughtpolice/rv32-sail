/*
** preamble/ffi.sail: RTS FFI imports
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */
/* -- FFI C Bindings -------------------------------------------------------- */

// -- strings

val eq_string = {lem: "eq", _: "eq_string"} : (string, string) -> bool
val string_append = { c: "concat_str", _: "string_append" } : (string, string) -> string
val "string_length"     : string -> nat
val "string_startswith" : (string, string) -> bool
val "string_drop"       : (string, nat) -> string
val "string_take"       : (string, nat) -> string
val "string_length"     : string -> nat

val "dec_str" : int -> string

val "hex_str" : int -> string

val bits_str = "string_of_bits" : forall 'n. bits('n) -> string

// -- bits functions

val "sign_extend" : forall 'n 'm, 'm >= 'n. (bits('n), atom('m)) -> bits('m)
val "zero_extend" : forall 'n 'm, 'm >= 'n. (bits('n), atom('m)) -> bits('m)

val "maybe_int_of_prefix" : string -> option((int, nat))
val "maybe_nat_of_prefix" : string -> option((nat, nat))
val "maybe_int_of_string" : string -> option(int)

// -- RAM/memory

val "write_ram" : forall 'n 'm.
  (atom('m), atom('n), bits('m), bits('m), bits(8 * 'n)) -> unit effect { wmem }

val "read_ram" : forall 'n 'm.
  (atom('m), atom('n), bits('m), bits('m)) -> bits(8 * 'n) effect { rmem }

// -- vectors

val shiftleft_int = "shiftl" : forall 'n ('ord : Order).
    (vector('n, 'ord, bit), int) -> vector('n, 'ord, bit) effect pure

val shiftright_int = "shiftr" : forall 'n ('ord : Order).
    (vector('n, 'ord, bit), int) -> vector('n, 'ord, bit) effect pure

val shiftleft_bits  = "shift_bits_left"  : forall 'n 'm. (bits('n), bits('m)) -> bits('n)
val shiftright_bits = "shift_bits_right" : forall 'n 'm. (bits('n), bits('m)) -> bits('n)

val add_vec = {c: "sub_bits", _: "sub_vec"} : forall 'n. (bits('n), bits('n)) -> bits('n)
val sub_vec = {c: "sub_bits", _: "sub_vec"} : forall 'n. (bits('n), bits('n)) -> bits('n)
val xor_vec = {c: "xor_bits", _: "xor_vec"} : forall 'n. (bits('n), bits('n)) -> bits('n)
val and_vec = {c: "and_bits", _: "and_vec"} : forall 'n. (bits('n), bits('n)) -> bits('n)
val or_vec  = {c: "or_bits",  _: "or_vec"}  : forall 'n. (bits('n), bits('n)) -> bits('n)

val add_vec_int = {c: "add_bits_int",  _: "add_vec_int"} : forall 'n. (bits('n), bits('n)) -> bits('n)
val sub_vec_int = {c: "sub_bits_int",  _: "sub_vec_int"} : forall 'n. (bits('n), bits('n)) -> bits('n)

// -- ints, atoms, etc

val "pow2" : forall 'n. atom('n) -> atom(2 ^ 'n)
val quot_round_zero = {ocaml: "quot_round_zero", lem: "hardware_quot", c: "tdiv_int"} : (int, int) -> int
val rem_round_zero = {ocaml: "rem_round_zero", lem: "hardware_mod", c: "tmod_int"} : (int, int) -> int

// -- rts functions

val putchar = { c:"sail_putchar", _:"putchar" } : int -> unit

val print = "print" : string -> unit
val prerr = "prerr" : string -> unit
val print_ln = "print_endline" : string -> unit
val prerr_ln = "prerr_endline" : string -> unit

val get_verbosity = { c: "sail_get_verbosity" } : unit -> bits(64)
function get_verbosity () = 0x0000000000000000

val "cycle_limit_reached" : unit -> bool
function cycle_limit_reached () = false

val "get_time_ns" : unit -> int

val elf_entry = {
  ocaml: "Elf_loader.elf_entry",
  lem: "elf_entry",
  c: "elf_entry"
} : unit -> int

/* -------------------------------------------------------------------------- */
/* -- El Fin (preamble/ffi.sail) -------------------------------------------- */
