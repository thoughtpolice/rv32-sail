/*
** decode/prologue.sail: RISC-V instruction decoder setup
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */

mapping bool_not_bits : bool <-> bits(1) = {
  true   <-> 0b0,
  false  <-> 0b1
}

/* "RISC-V User-Level ISA V2.2, pg 104" */

type frm   = bits(3) // F rounding mode
type imm5  = bits(5)
type imm7  = bits(7)
type imm12 = bits(12)
type imm13 = bits(13)
type imm20 = bits(20)
type imm21 = bits(21)

type Rtype  = (regbits, regbits, regbits)
type Itype  = (imm12, regbits, regbits)
type Stype  = (imm12, regbits, regbits)
type Btype  = (imm12, regbits, regbits)
type Utype  = (imm20, regbits)
type Jtype  = (imm21, regbits)
type Mtype  = (bits(4), bits(4)) // fences

type R4type = (regbits, regbits, regbits, frm, regbits)

type A0type = (bits(1), bits(1), regbits, regbits)
type A1type = (bits(1), bits(1), regbits, regbits, regbits)

/* -------------------------------------------------------------------------- */

// NOTE: do _NOT_ flip the argument/types in this mapping around. it apparently
// drives the compiler up the wall, causing an internal compiler error(?)
// TODO FIXME: submit a bug about this?
mapping uj_imm : imm21 <-> imm20 = {
  (imm_19 @ imm_7_0 @ imm_8 @ imm_18_13 @ imm_12_9 @ 0b0) <-> (imm_19 : bits(1) @ imm_18_13 : bits(6) @ imm_12_9 : bits(4) @ imm_8 : bits(1) @ imm_7_0 : bits(8))
}

mapping sb_imm : imm13 <-> imm12 = {
  (imm7_6 @ imm5_0 @ imm7_5_0 @ imm5_4_1 @ 0b0) <-> (imm7_6 : bits(1) @ imm7_5_0 : bits(6) @ imm5_4_1 : bits(4) @ imm5_0 : bits(1))
}

/* -------------------------------------------------------------------------- */

/* Scattered AST definitions */
scattered union ast

/* Mapping for base instruction sets */
val encdec_base : ast <-> bits(32)
scattered mapping encdec_base

/* Mapping for compressed instruction sets */
val encdec_comp : ast <-> bits(16)
scattered mapping encdec_comp

/* Function for printing things. TODO: Replace with <-> string mapping. */
val print_insn : ast -> string
scattered function print_insn

/* -------------------------------------------------------------------------- */
/* -- El Fin (decode/prologue.sail) ----------------------------------------- */
