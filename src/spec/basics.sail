/*
** basics.sail: basic project-specific definitions.
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */
/* -- Basic Types ----------------------------------------------------------- */

type xlen_t = bits(32)

type xword_t = bits(32)
type xhalf_t = bits(16)

type regno ('n : Int), 0 <= 'n < 32 = atom('n)

type regbits = bits(5) /* log2(32), would be nice to have clog in sail? */

val cast regbits_to_regno : regbits -> {'n, 0 <= 'n < 32. regno('n)}
function regbits_to_regno b =
  let r as atom(_) = unsigned(b)
  in r

val cast gp_regbits_to_string : regbits -> string
function gp_regbits_to_string b =
  let r = regbits_to_regno(b)
  in match r {
    0  => "zero",
    1  => "ra",
    2  => "sp",
    3  => "gp",
    4  => "tp",
    5  => "t0",
    6  => "t1",
    7  => "t2",
    8  => "fp",
    9  => "s1",
    10 => "a0",
    11 => "a1",
    12 => "a2",
    13 => "a3",
    14 => "a4",
    15 => "a5",
    16 => "a6",
    17 => "a7",
    18 => "s2",
    19 => "s3",
    20 => "s4",
    21 => "s5",
    22 => "s6",
    23 => "s7",
    24 => "s8",
    25 => "s9",
    26 => "s10",
    27 => "s11",
    28 => "t3",
    29 => "t4",
    30 => "t5",
    31 => "t6"
  }

/* -------------------------------------------------------------------------- */
/* -- Register file reads/writes -------------------------------------------- */

register PC     : xlen_t
register NextPC : xlen_t

/* GP regs */
register Xs : vector(32, dec, xlen_t)

/* read gp reg */
val rX : forall 'n, 0 <= 'n < 32. regno('n) -> xlen_t effect {rreg}
function rX 0            = 0x00000000
and      rX (r if r > 0) = Xs[r]

/* write gp reg(r, val) */
val wX : forall 'n, 0 <= 'n < 32. (regno('n), xlen_t) -> unit effect {wreg}
function wX (r, v) =
  if (r != 0) then { Xs[r] = v; }

/* overload read/write ops to 'X' */
overload X = { rX, wX }

/* -------------------------------------------------------------------------- */
/* -- Memory reads/writes --------------------------------------------------- */

val __RISCV_read : forall 'n. (xlen_t, atom('n)) -> bits(8 * 'n) effect {rmem}
function __RISCV_read(addr, width) =
  read_ram(32, width, 0x0000_0000, addr)

val __RISCV_write : forall 'n. (xlen_t, atom('n), bits(8 * 'n)) -> unit effect {wmem}
function __RISCV_write(addr, width, data) =
  let _ = write_ram(32, width, 0x0000_0000, addr, data)
  in ()

/* overload read/write ops to 'MEM' */
overload MEM = { __RISCV_read, __RISCV_write }

/* -------------------------------------------------------------------------- */
/* -- Exceptions ------------------------------------------------------------ */

union exception = {
  EInvalidInstr : unit
}

val raise_exc : forall ('o : Type). exception -> 'o effect {escape}
function raise_exc exc = {
  match exc {
    _ => ()
  };

  throw exc
}

/* -------------------------------------------------------------------------- */
/* -- {List,Set}Config ------------------------------------------------------ */

val __PrintConfig : (string, string, string, option(string)) -> unit
val __ListConfig  : unit -> unit
val __SetConfig   : (string, int) -> unit effect pure

function __PrintConfig (cfg, ty, value, desc) = {
  prerr_ln(cfg ^-^ " (" ^-^ ty ^-^ ") default = " ^-^ value);
  match (desc) {
    Some(d) => { prerr_ln("    " ^-^ d) },
    _ => { () }
  }
}

function __ListConfig () = {
  prerr_ln("");
  __PrintConfig("cpu.f", "boolean", "FALSE", Some("Enable -F Floating Point Extension"));
  ()
}

function __SetConfig (arg, value) = {
  match () {
    () if arg == "cpu.f" => {
      prerr_ln("[Sail] Enabling RV32 -F extension");
    },

    _ => {
      let v = dec_str(value);
      let q = "'" ^-^ arg ^-^ "' = '" ^-^ v ^-^ "'";
      prerr_ln("[Sail] ERROR: Unrecognized config: " ^-^ q ^-^ "; ignoring...");
    }
  }
}

/* -------------------------------------------------------------------------- */
/* -- El Fin (basics.sail) -------------------------------------------------- */
