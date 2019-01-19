/*
** execute/base.sail: base instruction set executions
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */
/* -- Close scattered defs -------------------------------------------------- */

//function clause execute SLLI(rs2, rs1, rd) = X(rd) =
//function clause execute SRLI(rs2, rs1, rd) = X(rd) =
//function clause execute SRAI(rs2, rs1, rd) = X(rd) =

function clause execute ADD(rs2, rs1, rd)  = X(rd) = X(rs1) + X(rs2)
function clause execute SUB(rs2, rs1, rd)  = X(rd) = sub_vec(X(rs1), X(rs2))
//function clause execute SLL(rs2, rs1, rd)  = X(rd) =
//function clause execute SLT(rs2, rs1, rd)  = X(rd) =
//function clause execute SLTU(rs2, rs1, rd) = X(rd) =
function clause execute XOR(rs2, rs1, rd)  = X(rd) = xor_vec(X(rs1), X(rs2))
//function clause execute SRL(rs2, rs1, rd)  = X(rd) =
//function clause execute SRA(rs2, rs1, rd)  = X(rd) =
//function clause execute OR(rs2, rs1, rd)   = X(rd) =
//function clause execute AND(rs2, rs1, rd)  = X(rd) =

/* -------------------------------------------------------------------------- */
/* -- El Fin (execute/base.sail) -------------------------------------------- */
