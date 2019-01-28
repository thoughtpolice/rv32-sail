/*
** execute/prologue.sail: RISC-V execution engine setup
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */

val execute : ast -> unit effect { escape, rreg, wreg, wmem, rmem }
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

/*
** assert that a given target PC address has proper alignment
*/
val assert_aligned_pc : xlen_t -> unit effect { escape }
function assert_aligned_pc pc = {
  // NOTE: the RISC-V ISA Manual, V2.2, pg 16, "Control Transfer Instructions"
  // says:
  //
  //     "Instruction fetch misaligned exceptions are not possible on machines
  //      that support extensions with 16-bit aligned instructions, such as the
  //      compressed instruction set extension, C."

  if not_bool(__cfg_enableExtensionC) then {
    // check for 4 byte alignment
    if not_bool(pc[1..0] == 0b00) then {
      throw(EMisalignedPC(pc))
    }
  };
}

/* -------------------------------------------------------------------------- */
/* -- El Fin (execute/prologue.sail) ---------------------------------------- */
