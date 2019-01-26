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
type imm20 = bits(20)
type imm21 = bits(21)

type Rtype  = (regbits, regbits, regbits)
type Itype  = (imm12, regbits, regbits)
type Stype  = (imm7, regbits, regbits, imm5)
type Btype  = (imm7, regbits, regbits, imm5)
type Utype  = (imm20, regbits)
type Jtype  = (imm21, regbits)
type Mtype  = (bits(4), bits(4)) // fences

type R4type = (regbits, regbits, regbits, frm, regbits)

type A0type = (bits(1), bits(1), regbits, regbits)
type A1type = (bits(1), bits(1), regbits, regbits, regbits)

/* -------------------------------------------------------------------------- */

mapping uj_imm : bits(21) <-> bits(20) = {
  (imm_19 @ imm_7_0 @ imm_8 @ imm_18_13 @ imm_12_9 @ 0b0) <-> (imm_19 : bits(1) @ imm_18_13 : bits(6) @ imm_12_9 : bits(4) @ imm_8 : bits(1) @ imm_7_0 : bits(8))
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

/* TODO FIXME: no compressed ones yet, so fake one */
union clause ast = C_NOP : unit
mapping clause encdec_comp = C_NOP() <-> 0b000 @ 0b0 @ 0b00000 @ 0b00000 @ 0b01
function clause print_insn C_NOP() = "c.nop"

/* -------------------------------------------------------------------------- */
/* -- El Fin (decode/prologue.sail) ----------------------------------------- */
