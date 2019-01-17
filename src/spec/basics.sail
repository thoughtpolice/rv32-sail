/*
** basics.sail: basic project-specific definitions.
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */
/* -- Basic Types ----------------------------------------------------------- */

type xlen_t = bits(32)

type xword_t = bits(32)
type xhalf_t = bits(16)

type regno ('n : Int), 0 <= 'n < 32 = atom('n)

type regbits = bits(5) /* log2(32), would be nice to have clog in sail? */

val cast regbits_to_regno : bits(5) -> {'n, 0 <= 'n < 32. regno('n)}
function regbits_to_regno b =
  let r as atom(_) = unsigned(b)
  in r

/* -------------------------------------------------------------------------- */
/* -- Register file reads/writes -------------------------------------------- */

register PC     : xlen_t
register NextPC : xlen_t

/* GP regs */
register Xs : vector(32, dec, xlen_t)

/* read gp reg */
val rX : forall 'n, 0 <= 'n < 32. regno('n) -> xlen_t effect {rreg}
function rX 0            = 0x00000000
and      rX (r if r > 0) = Xs[r]

/* write gp reg(r, val) */
val wX : forall 'n, 0 <= 'n < 32. (regno('n), xlen_t) -> unit effect {wreg}
function wX (r, v) =
  if (r != 0) then { Xs[r] = v; }

/* overload read/write ops to 'X' */
overload X = { rX, wX }

/* -------------------------------------------------------------------------- */
/* -- Memory reads/writes --------------------------------------------------- */

val __RISCV_read : forall 'n. (xlen_t, atom('n)) -> bits(8 * 'n) effect {rmem}
function __RISCV_read(addr, width) =
  read_ram(32, width, 0x0000_0000, addr)

val __RISCV_write : forall 'n. (xlen_t, atom('n), bits(8 * 'n)) -> unit effect {wmem}
function __RISCV_write(addr, width, data) =
  let _ = write_ram(32, width, 0x0000_0000, addr, data)
  in ()

val MEMr : forall 'n. (xlen_t, atom('n)) -> bits(8 * 'n) effect {rmem}
function MEMr (addr, width) = __RISCV_read(addr, width)

/* overload read/write ops to 'MEM' */
overload MEM = { MEMr, MEMw }

/* -------------------------------------------------------------------------- */
/* -- AST, Assembly, Decode, and Execute signatures ------------------------- */

scattered union ast

val assembly : ast <-> string
scattered mapping assembly

val encdec_base : ast <-> bits(32)
scattered mapping encdec_base

val encdec_comp : ast <-> bits(16)
scattered mapping encdec_comp

val execute : ast -> unit
scattered function execute

/* -------------------------------------------------------------------------- */
/* -- Exceptions ------------------------------------------------------------ */

union exception = {
  EInvalidInstr : unit
}

val raise_exc : forall ('o : Type). exception -> 'o effect {escape}
function raise_exc exc = {
  match exc {
    _ => ()
  };

  throw exc
}

/* -------------------------------------------------------------------------- */
/* -- El Fin (basics.sail) -------------------------------------------------- */
