/*
** setup.sail: basic, reusable sail imports, types, and definitions.
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
/* -- Basic Types and Operators --------------------------------------------- */

infixr 1 -->
type operator -->('p: Bool, 'q: Bool) -> Bool = not('p) | 'q

infix 1 <-->
type operator <-->('p: Bool, 'q: Bool) -> Bool = ('p --> 'q) & ('q --> 'p)

/* -------------------------------------------------------------------------- */
/* -- FFI C Bindings -------------------------------------------------------- */

val get_verbosity = { c: "sail_get_verbosity" } : unit -> bits(64)

val "get_cycle_count"     : unit -> int
val "get_time_ns"         : unit -> int
val "cycle_count"         : unit -> unit
val "cycle_limit_reached" : unit -> bool

/* -------------------------------------------------------------------------- */
/* -- Utility functions ----------------------------------------------------- */

val to_bits : forall 'l, 'l >= 0 .(atom('l), int) -> bits('l)
function to_bits (l, n) = get_slice_int(l, n, 0)

/* -------------------------------------------------------------------------- */
/* -- RAM Primitives -------------------------------------------------------- */

val "write_ram" : forall 'n 'm.
  (atom('m), atom('n), bits('m), bits('m), bits(8 * 'n)) -> unit effect {wmem}

val "read_ram" : forall 'n 'm.
  (atom('m), atom('n), bits('m), bits('m)) -> bits(8 * 'n) effect {rmem}

/* -- El Fin (setup.sail) --------------------------------------------------- */
