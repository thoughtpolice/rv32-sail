/*
** basics.sail: basic project-specific definitions.
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */
/* -- Basic Types ----------------------------------------------------------- */

let xlen = 32
type xlen_t = bits(32)

type xword_t = bits(32)
type xhalf_t = bits(16)

let xlen_max_unsigned =     2 ^  xlen      - 1
let xlen_max_signed   =     2 ^ (xlen - 1) - 1
let xlen_min_signed   = 0 - 2 ^ (xlen - 1)

type regno ('n : Int), 0 <= 'n < 32 = atom('n)

type regbits = bits(5) /* log2(32), would be nice to have clog in sail? */

type csregno = bits(12)
type csreg_t = bits(32)

/* -------------------------------------------------------------------------- */
/* -- Register file reads/writes -------------------------------------------- */

register PC     : xlen_t
register NextPC : xlen_t

/* GP regs */
register Xs : vector(32, dec, xlen_t)

/* read gp reg */
val rX : forall 'n, 0 <= 'n < 32. regno('n) -> xlen_t effect { rreg }
function rX r = match r {
  0 => 0x00000000,
  v => Xs[v]
}
/* -- this apparently triggers a compiler bug?
function rX 0            = 0x00000000
and      rX (r if r > 0) = Xs[r]
*/

/* write gp reg(r, val) */
val wX : forall 'n, 0 <= 'n < 32. (regno('n), xlen_t) -> unit effect { wreg }
function wX (r, v) =
  if (r != 0) then { Xs[r] = v; }

/* overload read/write ops to 'X' */
overload X = { rX, wX }

val cast regbits_to_regno : regbits -> {'n, 0 <= 'n < 32. regno('n)}
function regbits_to_regno b =
  let r as atom(_) = unsigned(b)
  in r

val cast gp_regbits_to_string : regbits -> string
function gp_regbits_to_string b =
  let r = regbits_to_regno(b)
  in match r {
    0  => "zero",
    1  => "ra",
    2  => "sp",
    3  => "gp",
    4  => "tp",
    5  => "t0",
    6  => "t1",
    7  => "t2",
    8  => "fp",
    9  => "s1",
    10 => "a0",
    11 => "a1",
    12 => "a2",
    13 => "a3",
    14 => "a4",
    15 => "a5",
    16 => "a6",
    17 => "a7",
    18 => "s2",
    19 => "s3",
    20 => "s4",
    21 => "s5",
    22 => "s6",
    23 => "s7",
    24 => "s8",
    25 => "s9",
    26 => "s10",
    27 => "s11",
    28 => "t3",
    29 => "t4",
    30 => "t5",
    31 => "t6"
  }

/* -------------------------------------------------------------------------- */
/* -- Memory reads/writes --------------------------------------------------- */

val __RISCV_read : forall 'n. (xlen_t, atom('n)) -> bits(8 * 'n) effect { rmem }
function __RISCV_read(addr, width) =
  read_ram(32, width, 0x0000_0000, addr)

val __RISCV_write : forall 'n. (xlen_t, atom('n), bits(8 * 'n)) -> unit effect { wmem }
function __RISCV_write(addr, width, data) =
  let _ = write_ram(32, width, 0x0000_0000, addr, data)
  in ()

/* overload read/write ops to 'MEM' */
overload MEM = { __RISCV_read, __RISCV_write }

/* -------------------------------------------------------------------------- */
/* -- Exceptions ------------------------------------------------------------ */

union exception = {
  EInvalidInstrBase : bits(32),
  EInvalidInstrComp : bits(16),
  EInvalidCSR       : csregno,
  EMisalignedPC     : bits(32),
  EFeatureDisabled  : (string,string),
  ENotImplemented   : string,
  EInvalidECall     : xlen_t,
  EImpossible       : string,
}

val nih : forall ('a : Type). string -> 'a effect {escape}
function nih m = throw(ENotImplemented(m))

val impossible : forall ('a : Type). string -> 'a effect {escape}
function impossible m = throw(EImpossible(m))

/* -------------------------------------------------------------------------- */
/* -- Control Status Registers (CSRs) --------------------------------------- */

union csr_priv =
  { CSR_PRIV_USER       : unit
  , CSR_PRIV_SUPERVISOR : unit
  , CSR_PRIV_RESERVED   : unit
  , CSR_PRIV_MACHINE    : unit
  }

union csr_mode =
  { CSR_MODE_READONLY  : unit
  , CSR_MODE_READWRITE : unit
  }

mapping csr_priv_bits : bits(2) <-> csr_priv = {
  0b00 <-> CSR_PRIV_USER(),
  0b01 <-> CSR_PRIV_SUPERVISOR(),
  0b10 <-> CSR_PRIV_RESERVED(),
  0b11 <-> CSR_PRIV_MACHINE()
}

mapping csr_mode_bits : bits(2) <-> csr_mode = {
  0b00 <-> CSR_MODE_READWRITE(),
  0b01 <-> CSR_MODE_READWRITE(),
  0b10 <-> CSR_MODE_READWRITE(),
  0b11 <-> CSR_MODE_READONLY()
}

bitfield misa_t : bits(32) = {
  MXL  : 31 .. 30,

  Z    : 25,
  Y    : 24,
  X    : 23,
  W    : 22,
  V    : 21,
  U    : 20,
  T    : 19,
  S    : 18,
  R    : 17,
  Q    : 16,
  P    : 15,
  O    : 14,
  N    : 13,
  M    : 12,
  L    : 11,
  K    : 10,
  J    : 9,
  I    : 8,
  H    : 7,
  G    : 6,
  F    : 5,
  E    : 4,
  D    : 3,
  C    : 2,
  B    : 1,
  A    : 0
}
register __csr_misa : misa_t

val __CSR_read : csregno -> csreg_t effect { escape, rreg }
function __CSR_read (mode : bits(2) @ priv : bits(2) @ regno : bits(8)) =
  match (csr_mode_bits(mode), csr_priv_bits(priv), regno) {
    /* ----------------------------- User CSRs ------------------------------ */

    /* ---------------------------- Machine CSRs ---------------------------- */

    // MISA reads: always RV32 (0b01), with 4-bit empty field, and MISA regs
    (CSR_MODE_READWRITE(), CSR_PRIV_MACHINE(), 0x01) =>
     (0b01 @ 0xF @ __csr_misa.bits()[25..0]),

    (CSR_MODE_READONLY(), CSR_PRIV_MACHINE(), 0x11) => 0x00000000, // mvendorid
    (CSR_MODE_READONLY(), CSR_PRIV_MACHINE(), 0x12) => 0x00000000, // marchid
    (CSR_MODE_READONLY(), CSR_PRIV_MACHINE(), 0x13) => 0x00000000, // mimpid
    (CSR_MODE_READONLY(), CSR_PRIV_MACHINE(), 0x14) => 0x00000000, // mhartid

    /* ----------------------------- Error case ----------------------------- */
    // TODO FIXME: illegal instr exception
    _ => { throw(ENotImplemented("__CSR_read")) }
  }

val __CSR_write : (csregno, xlen_t) -> unit effect { escape, rreg, wreg }
function __CSR_write (mode : bits(2) @ priv : bits(2) @ regno : bits(8), v) =
  match (csr_mode_bits(mode), csr_priv_bits(priv), regno) {
    /* ----------------------------- User CSRs ------------------------------ */

    /* ---------------------------- Machine CSRs ---------------------------- */

    // MISA reads: always RV32 (0b01), with 36-bit empty field, and MISA regs
    (CSR_MODE_READWRITE(), CSR_PRIV_MACHINE(), 0x01) => {
      let old = __csr_misa.bits();
      __csr_misa->bits() = 0b01 @ 0xF @ v[25..0]
    },

    /* ----------------------------- Error case ----------------------------- */
    // TODO FIXME: illegal instr exception
    _ => { throw(ENotImplemented("__CSR_write")) }
  }

overload CSR = { __CSR_read, __CSR_write }

/* -------------------------------------------------------------------------- */
/* -- El Fin (basics.sail) -------------------------------------------------- */
