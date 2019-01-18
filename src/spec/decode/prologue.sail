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

type imm5  = bits(5)
type imm7  = bits(7)
type imm12 = bits(12)
type imm20 = bits(20)

type Rtype = (regbits, regbits, regbits)
type Itype = (imm12, regbits, regbits)
type Stype = (imm7, regbits, regbits, imm5)
type Btype = (imm7, regbits, regbits, imm5)
type Utype = (imm20, regbits)
type Jtype = (imm20, regbits)

/* Scattered AST definitions */
scattered union ast

/* Mapping for base instruction sets */
val encdec_base : ast <-> bits(32)
scattered mapping encdec_base

/* Mapping for compressed instruction sets */
val encdec_comp : ast <-> bits(16)
scattered mapping encdec_comp

/* TODO FIXME: no compressed ones yet, so fake one */

union clause ast = C_NOP : unit
mapping clause encdec_comp = C_NOP() <-> 0b000 @ 0b0 @ 0b00000 @ 0b00000 @ 0b01

/* -------------------------------------------------------------------------- */
/* -- El Fin (decode/prologue.sail) ----------------------------------------- */
