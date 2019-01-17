/*
** rv32.sail: RISC-V instruction decoder/execute engine
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */

mapping bool_not_bits : bool <-> bits(1) = {
  true   <-> 0b0,
  false  <-> 0b1
}

/* ---------------------------------- */
/* -- DIV --------------------------- */

union clause ast = DIV : (regbits, regbits, regbits, bool)

mapping clause encdec_base =
  DIV(rs2, rs1, rd, s) <-> 0b0000001 @ rs2 @ rs1 @ 0b10 @ bool_not_bits(s) @ rd @ 0b0110011

function clause execute DIV(rs2, rs1, rd, s) = ()

/* ---------------------------------- */
/* -- NOP --------------------------- */

union clause ast = C_NOP : unit

mapping clause assembly = C_NOP() <-> "c.nop"
mapping clause encdec_comp = C_NOP() <-> 0b000 @ 0b0 @ 0b00000 @ 0b00000 @ 0b01

function clause execute C_NOP() = ()

/* ---------------------------------- */
/* -- ILLEGAL ----------------------- */

union clause ast =   ILLEGAL : xword_t
union clause ast = C_ILLEGAL : xhalf_t

//mapping clause assembly =   ILLEGAL(s) <-> "illegal"   ^ spc() ^ hex_bits_32(s)
//mapping clause assembly = C_ILLEGAL(s) <-> "c.illegal" ^ spc() ^ hex_bits_16(s)

mapping clause encdec_base =   ILLEGAL(s) <-> s
mapping clause encdec_comp = C_ILLEGAL(s) <-> s

function clause execute (  ILLEGAL(_)) = ()
function clause execute (C_ILLEGAL(_)) = ()

/* -------------------------------------------------------------------------- */
/* -- El Fin (rv32.sail) -------------------------------------------------- */

