/*
** execute/base.sail: base instruction set executions
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */
/* -- Primary execute clause defns ------------------------------------------ */

function clause execute LUI(imm, rd) = {
  let off : xlen_t = exts(imm @ 0x000);
  X(rd) = off;
}

function clause execute AUIPC(imm, rd) = {
  let off : xlen_t = exts(imm @ 0x000);
  X(rd) = PC + off;
}

function clause execute JAL(imm, rd) = {
  let npc : xlen_t = exts(imm) + PC;

  // TODO FIXME: handle misaligned npc
  X(rd) = NextPC;
  PC = npc;
}

function clause execute JALR(imm, rs1, rd) = {
  let npc0 : xlen_t = exts(imm) + X(rs1);
  let npc  : xlen_t = npc0[31..1] @ 0b0;

  // TODO FIXME: handle misaligned npc
  X(rd) = NextPC;
  PC = npc;
}

function clause execute ADD(rs2, rs1, rd)    = X(rd) = X(rs1) + X(rs2)
function clause execute SUB(rs2, rs1, rd)    = X(rd) = X(rs1) - X(rs2)
function clause execute ADDI(imm, rs1, rd)   = X(rd) = X(rs1) + exts(imm)

function clause execute XOR(rs2, rs1, rd)    = X(rd) = X(rs1) ^ X(rs2)
function clause execute XORI(imm, rs1, rd)   = X(rd) = X(rs1) ^ exts(imm)

function clause execute ORI(imm, rs1, rd)    = X(rd) = X(rs1) | exts(imm)
function clause execute OR(rs2, rs1, rd)     = X(rd) = X(rs1) | X(rs2)

function clause execute ANDI(imm, rs1, rd)   = X(rd) = X(rs1) & exts(imm)
function clause execute AND(rs2, rs1, rd)    = X(rd) = X(rs1) & X(rs2)

function clause execute SLL(rs2, rs1, rd)    = X(rd) = X(rs1) << X(rs2)[4..0]
function clause execute SLLI(shamt, rs1, rd) = X(rd) = X(rs1) << shamt[4..0]

function clause execute SRL(rs2, rs1, rd)    = X(rd) = X(rs1) >> X(rs2)[4..0]
function clause execute SRLI(shamt, rs1, rd) = X(rd) = X(rs1) >> shamt[4..0]

function clause execute SRA(rs2, rs1, rd)    = X(rd) = sra32(X(rs1), X(rs2)[4..0])
function clause execute SRAI(shamt, rs1, rd) = X(rd) = sra32(X(rs1), shamt[4..0])

function clause execute SLT(rs2, rs1, rd)  = {
  let res = signed(X(rs1)) < signed(X(rs2));
  X(rd) = extz(res);
}

function clause execute SLTI(imm, rs1, rd) = {
  let _imm : xlen_t = exts(imm);
  let res = signed(X(rs1)) < signed(_imm);
  X(rd) = extz(res);
}

function clause execute SLTU(rs2, rs1, rd) = {
  let res = unsigned(X(rs1)) < unsigned(X(rs2));
  X(rd) = extz(res);
}

function clause execute SLTIU(imm, rs1, rd) = {
  let _imm : xlen_t = exts(imm);
  let res = unsigned(X(rs1)) < unsigned(_imm);
  X(rd) = extz(res);
}

/* -------------------------------------------------------------------------- */
/* -- El Fin (execute/base.sail) -------------------------------------------- */
