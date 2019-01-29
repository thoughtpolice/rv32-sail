/*
** config.sail: emulator configuration settings
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */
/* -- Emulator Configuration Settings --------------------------------------- */

register configuration __cfg_disassembleOps : bool = false
register configuration __cfg_dumpStatsCycleRate : int = 10000000

register configuration __cfg_enableExtensionM : bool = true
register configuration __cfg_enableExtensionA : bool = false
register configuration __cfg_enableExtensionF : bool = false
register configuration __cfg_enableExtensionD : bool = false
register configuration __cfg_enableExtensionC : bool = false

val __CfgDisassembleOps : bool -> unit effect { configuration }
function __CfgDisassembleOps enabled = { __cfg_disassembleOps = enabled }

val __CfgExtensionM : bool -> unit effect { configuration }
function __CfgExtensionM enabled = { __cfg_enableExtensionM = enabled }

val __CfgExtensionA : bool -> unit effect { configuration }
function __CfgExtensionA enabled = { __cfg_enableExtensionA = enabled }

val __CfgExtensionF : bool -> unit effect { configuration }
function __CfgExtensionF enabled = { __cfg_enableExtensionF = enabled }

val __CfgExtensionD : bool -> unit effect { configuration }
function __CfgExtensionD enabled = { __cfg_enableExtensionD = enabled }

val __CfgExtensionC : bool -> unit effect { configuration }
function __CfgExtensionC enabled = { __cfg_enableExtensionC = enabled }

/* -------------------------------------------------------------------------- */
/* -- {List,Set}Config ------------------------------------------------------ */

val __PrintConfig : (string, string, string, option(string)) -> unit
val __ListConfig  : unit -> unit
val __SetConfig   : (string, int) -> unit effect { configuration }

function __PrintConfig (cfg, ty, value, desc) = {
  prerr_ln(cfg ^ " (" ^ ty ^ ") default = " ^ value);
  match (desc) {
    Some(d) => { prerr_ln("    " ^ d) },
    _ => { () }
  }
}

function __ListConfig () = {
  prerr_ln("");
  __PrintConfig("debug.disasm_ops", "boolean", "FALSE", Some("Disassemble fetched/executed opcodes"));
  __PrintConfig("debug.stat_dump_rate", "integer", "10000000", Some("How often (in cycles) to dump emulator info. Only in effect when (verbosity & 0b1)"));

  prerr_ln("");
  __PrintConfig("cpu.has_ext_m", "boolean", "FALSE", Some("Enable -M Mult/Div Extension"));
  __PrintConfig("cpu.has_ext_a", "boolean", "FALSE", Some("Enable -A Atomic Extension"));
  __PrintConfig("cpu.has_ext_f", "boolean", "FALSE", Some("Enable -F SP Floating Point Extension"));
  __PrintConfig("cpu.has_ext_d", "boolean", "FALSE", Some("Enable -D DP Floating Point Extension"));
  __PrintConfig("cpu.has_ext_c", "boolean", "FALSE", Some("Enable -C Compressed Instruction Set"));
  ()
}

function __SetConfig (arg, value) = {
  match () {
    () if arg == "debug.disasm_ops" => {
      __CfgDisassembleOps(value == 1);
    },
    () if arg == "debug.stat_dump_rate" => {
      __cfg_dumpStatsCycleRate = value;
    },

    // Extension features
    () if arg == "cpu.has_ext_m" => {
       __CfgExtensionM(value == 1);
    },
    () if arg == "cpu.has_ext_a" => {
      prerr_ln("[Sail] ERROR: -A extension is not implemented! Ignoring...");
    },
    () if arg == "cpu.has_ext_f" => {
      prerr_ln("[Sail] ERROR: -F extension is not implemented! Ignoring...");
    },
    () if arg == "cpu.has_ext_d" => {
      prerr_ln("[Sail] ERROR: -D extension is not implemented! Ignoring...");
    },
    () if arg == "cpu.has_ext_c" => {
      prerr_ln("[Sail] ERROR: -C extension is not implemented! Ignoring...");
    },

    // Default case
    _ => {
      let v = dec_str(value);
      let q = "'" ^ arg ^ "' = '" ^ v ^ "'";
      prerr_ln("[Sail] ERROR: Unrecognized config: " ^ q ^ "; ignoring...");
    }
  }
}

/* -------------------------------------------------------------------------- */
/* -- El Fin (config.sail) -------------------------------------------------- */
