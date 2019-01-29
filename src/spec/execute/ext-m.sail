/*
** execute/ext-m.sail: multiplication and division instruction implementations
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */

val check_ext_m : string -> unit effect { escape }
function check_ext_m name = {
  if not_bool(__cfg_enableExtensionM) then
    throw(EFeatureDisabled("M", "encountered instruction: " ^ name));

  throw(ENotImplemented("-M extension is not implemented; encountered: " ^ name));
}

// -------------------------------------
// ---- Multiplication operations

function clause execute MUL(rs2, rs1, rd)    = { check_ext_m(MUL(rs2,rs1,rd))    }
function clause execute MULH(rs2, rs1, rd)   = { check_ext_m(MULH(rs2,rs1,rd))   }
function clause execute MULHSU(rs2, rs1, rd) = { check_ext_m(MULHSU(rs2,rs1,rd)) }
function clause execute MULHU(rs2, rs1, rd)  = { check_ext_m(MULHU(rs2,rs1,rd))  }

// -------------------------------------
// ---- Division operations

function clause execute DIV(rs2, rs1, rd)  = { check_ext_m(DIV(rs2,rs1,rd))  }
function clause execute DIVU(rs2, rs1, rd) = { check_ext_m(DIVU(rs2,rs1,rd)) }
function clause execute REM(rs2, rs1, rd)  = { check_ext_m(REM(rs2,rs1,rd))  }
function clause execute REMU(rs2, rs1, rd) = { check_ext_m(REMU(rs2,rs1,rd)) }

/* -------------------------------------------------------------------------- */
/* -- El Fin (execute/ext-m.sail) ------------------------------------------- */
