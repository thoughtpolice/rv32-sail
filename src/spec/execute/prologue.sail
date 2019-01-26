/*
** execute/prologue.sail: RISC-V execution engine setup
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */

val execute : ast -> unit effect { escape, rreg, wreg }
scattered function execute

/* TODO FIXME: no compressed ones yet, so fake one */
function clause execute C_NOP() = ()

val __rv32_store : forall 'n, 0 < 'n <= 4. (imm12, regbits, regbits, atom('n)) -> unit effect { wmem, rreg }
function __rv32_store (imm, rs2, rs1, width) = {
  let ptr : xlen_t = X(rs1) + exts(imm);
  MEM(ptr, width) = X(rs2)[ ((8 * 'n) - 1) .. 0 ];
}

val __rv32_load : forall 'n, 0 < 'n <= 4. (imm12, regbits, atom('n)) -> bits(8 * 'n) effect { rmem, rreg }
function __rv32_load (imm, rs1, width) = {
  let ptr : xlen_t = X(rs1) + exts(imm);
  MEM(ptr, width)
}

/* -------------------------------------------------------------------------- */
/* -- El Fin (execute/prologue.sail) ---------------------------------------- */
