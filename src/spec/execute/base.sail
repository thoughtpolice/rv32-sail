/*
** execute/base.sail: base instruction set executions
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */
/* -- Primary execute clause defns ------------------------------------------ */

// -------------------------------------
// ---- Immediate loads, etc

function clause execute LUI(imm, rd) = {
  let off : xlen_t = exts(imm @ 0x000);
  X(rd) = off;
}

function clause execute AUIPC(imm, rd) = {
  let off : xlen_t = exts(imm @ 0x000);
  X(rd) = PC + off;
}

// -------------------------------------
// ---- Branches

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

// -------------------------------------
// ---- Fundamental R/R & R/I ops

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

// -------------------------------------
// ---- Loads and stores

function clause execute SB(imm, rs2, rs1) = {
  let ptr : xlen_t = X(rs1) + exts(imm);
  MEM(ptr, 1) = X(rs2)[7..0];
}

function clause execute SH(imm, rs2, rs1) = {
  let ptr : xlen_t = X(rs1) + exts(imm);
  MEM(ptr, 2) = X(rs2)[15..0];
}

function clause execute SW(imm, rs2, rs1) = {
  let ptr : xlen_t = X(rs1) + exts(imm);
  MEM(ptr, 4) = X(rs2)[31..0];
}

function clause execute LB(imm, rs1, rd) = {
  let ptr : xlen_t = X(rs1) + exts(imm);
  X(rd) = exts(MEM(ptr, 1));
}

function clause execute LH(imm, rs1, rd) = {
  let ptr : xlen_t = X(rs1) + exts(imm);
  X(rd) = exts(MEM(ptr, 2));
}

function clause execute LW(imm, rs1, rd) = {
  let ptr : xlen_t = X(rs1) + exts(imm);
  X(rd) = MEM(ptr, 4);
}

function clause execute LBU(imm, rs1, rd) = {
  let ptr : xlen_t = X(rs1) + exts(imm);
  X(rd) = extz(MEM(ptr, 1));
}

function clause execute LHU(imm, rs1, rd) = {
  let ptr : xlen_t = X(rs1) + exts(imm);
  X(rd) = extz(MEM(ptr, 2));
}

// -------------------------------------
// ---- ECALL/EBREAK

function clause execute ECALL()  = { () }
function clause execute EBREAK() = { () }

// -------------------------------------
// ---- Fences

function clause execute FENCE(_, _) = { () }
function clause execute FENCE_I() = { () }

// -------------------------------------
// ---- CSRs

function clause execute CSRRW(csr, rs1, rd) = { () }
function clause execute CSRRS(csr, rs1, rd) = { () }
function clause execute CSRRC(csr, rs1, rd) = { () }

function clause execute CSRRWI(csr, zimm, rd) = { () }
function clause execute CSRRSI(csr, zimm, rd) = { () }
function clause execute CSRRCI(csr, zimm, rd) = { () }

/* -------------------------------------------------------------------------- */
/* -- El Fin (execute/base.sail) -------------------------------------------- */
