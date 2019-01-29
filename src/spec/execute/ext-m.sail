/*
** execute/ext-m.sail: multiplication and division instruction implementations
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */

val check_ext_m : string -> unit effect { escape }
function check_ext_m name = {
  if not_bool(__cfg_enableExtensionM) then
    throw(EFeatureDisabled("M", "encountered instruction: " ^ name));
}

// -------------------------------------
// ---- Multiplication operations

val ex_mul : (regbits, regbits, regbits, bool, bool, bool) -> unit effect { rreg, wreg }
function clause ex_mul (rs2, rs1, rd, top, signed1, signed2) = {
  let r1 = X(rs1);
  let r2 = X(rs2);

  let i1 : int = if signed1 then signed(r1) else unsigned(r1);
  let i2 : int = if signed2 then signed(r2) else unsigned(r2);

  let multr = to_bits(64, i1 * i2);
  let result = if top then multr[63..32] else multr[31..0];
  X(rd) = result;
}

function clause execute MUL(rs2, rs1, rd)    = { check_ext_m(MUL(rs2,rs1,rd))    ; ex_mul(rs2, rs1, rd, false, true,  true)  }
function clause execute MULH(rs2, rs1, rd)   = { check_ext_m(MULH(rs2,rs1,rd))   ; ex_mul(rs2, rs1, rd, true,  true,  true)  }
function clause execute MULHSU(rs2, rs1, rd) = { check_ext_m(MULHSU(rs2,rs1,rd)) ; ex_mul(rs2, rs1, rd, true,  true,  false) }
function clause execute MULHU(rs2, rs1, rd)  = { check_ext_m(MULHU(rs2,rs1,rd))  ; ex_mul(rs2, rs1, rd, true,  false, false) }

// -------------------------------------
// ---- Division operations


val ex_divmod : (regbits, regbits, regbits, bool, bool) -> unit effect { rreg, wreg }
function clause ex_divmod (rs2, rs1, rd, divop, s) = {
  let r1 = X(rs1);
  let r2 = X(rs2);

  let i1 : int = if s then signed(r1) else unsigned(r1);
  let i2 : int = if s then signed(r2) else unsigned(r2);

  if divop then {
    // div case: check for signed overflow
    let q : int = if i2 == 0 then -1 else quot_round_zero(i1, i2);
    let q': int = if s & q > xlen_max_signed then xlen_min_signed else q;
    X(rd) = to_bits(xlen, q');
  } else {
    // rem case: signed overflow case returns zero naturally as required due to -1 divisor
    let r : int = if i2 == 0 then i1 else rem_round_zero(i1, i2);
    X(rd) = to_bits(xlen, r);
  }
}

function clause execute DIV(rs2, rs1, rd)  = { check_ext_m(DIV(rs2,rs1,rd))  ; ex_divmod(rs2, rs1, rd, true, true)  }
function clause execute DIVU(rs2, rs1, rd) = { check_ext_m(DIVU(rs2,rs1,rd)) ; ex_divmod(rs2, rs1, rd, true, false) }
function clause execute REM(rs2, rs1, rd)  = { check_ext_m(REM(rs2,rs1,rd))  ; ex_divmod(rs2, rs1, rd, false, true) }
function clause execute REMU(rs2, rs1, rd) = { check_ext_m(REMU(rs2,rs1,rd)) ; ex_divmod(rs2, rs1, rd, false, false) }

/* -------------------------------------------------------------------------- */
/* -- El Fin (execute/ext-m.sail) ------------------------------------------- */
