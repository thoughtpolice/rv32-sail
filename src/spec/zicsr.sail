/*
** zicsr.sail: Control and Status Register mappings
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */
/* -- Types, mappings ------------------------------------------------------- */

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

/* -------------------------------------------------------------------------- */
/* -- Mutable CSR decls ----------------------------------------------------- */

register __csr_misa : misa_t

/* -------------------------------------------------------------------------- */
/* -- Read/Write ops -------------------------------------------------------- */

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
/* -- El Fin (zicsr.sail) --------------------------------------------------- */
