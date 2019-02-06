/*
** decode/comp.sail: temporary, bunk RISC-V compressed instruction decoder
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */

// TODO FIXME: no compressed decoder yet, so fake one. this avoids a compiler
// error for mapping functions/clauses that are declared, but empty

union clause ast = C_NOP : unit
mapping clause encdec_comp = C_NOP() <-> 0b000 @ 0b0 @ 0b00000 @ 0b00000 @ 0b01
function clause print_insn C_NOP() = "c.nop"

/* -------------------------------------------------------------------------- */
/* -- El Fin (decode/comp.sail) --------------------------------------------- */
