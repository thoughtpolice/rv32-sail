/*
** elfmain.sail: entry point and ELF loader for CHERI-RISCV simulator.
** See Copyright Notice in LICENSE.txt
*/

/* -------------------------------------------------------------------------- */
/* -- Top-level declarations for the ELF loader ----------------------------- */

/* -- Basic register that keeps track of the instruction count -- */
register InstrCount : int

/* -------------------------------------------------------------------------- */
/* -- Fetch driver ---------------------------------------------------------- */

union FetchResult = {
  FetchedBase       : xword_t,
  FetchedCompressed : xhalf_t,
  FetchError        : (exception, xlen_t)
}

/* Non-compressed instructions have low-half LSBs of 0x00 */
function isCompressed(h : xhalf_t) -> bool = not_bool(h[1..0] == 0b11)

/*
** Prefetch a RISC-V instruction -- importantly, predecode the given
** PC to determine if we have a compressed instruction or not.
*/
val elf_prefetch : xlen_t -> FetchResult effect { rmem, rreg, wreg }
function elf_prefetch (pc) = {
  InstrCount = InstrCount + 1;

  let ilo : xhalf_t = MEM(pc, 2);

  if isCompressed(ilo) then {
    if (not_bool(__cfg_enableExtensionC)) then {
      FetchError(EFeatureDisabled("-C (compressed ISA) extension"), pc)
    } else {
      FetchError(ENotImplemented("-C (compressed ISA) extension"), pc)
      // FetchedCompressed(ilo)
    }
  }
  else {
    let ihi  : xhalf_t = MEM(pc + 2, 2);
    let insn : xword_t = append(ihi, ilo);
    FetchedBase(insn);
  }
}

/*
** Take a fetched instruction from memory, decode it, and handle any decoding
** errors.
*/
val elf_fetch : unit -> ast effect { escape, rmem, rreg, wreg }
function elf_fetch () = {
  let instr : ast = match elf_prefetch(PC) {
    FetchError(ex, _) => { throw(ex) },

    FetchedCompressed(h) => {
      match decode_comp(h) {
        C_ILLEGAL(c) => { throw(EInvalidInstrComp(c)); },
        i => { NextPC = NextPC + 2; i }
      }
    },

    FetchedBase(w) => {
      match decode_base(w) {
        ILLEGAL(c) => { throw(EInvalidInstrBase(c)); },
        i => { NextPC = NextPC + 4; i }
      }
    }
  };

  if (__cfg_disassembleOps) then {
    prerr_ln("[Sail] Fetch - " ^ bits_str(PC) ^ ": " ^ instr);
  };

  instr
}

/* -------------------------------------------------------------------------- */
/* -- Decode, Execute driver ------------------------------------------------ */

/*
** take a fetched instruction and dispatch the execution function, as well as
** increasing the cycle count. returns 'true' if the cycle limit has NOT been
** reached; returns false if it has.
*/
val elf_execute : ast -> bool effect { escape, wmem, rmem, wreg, rreg }
function elf_execute ast = {
  execute(ast);
  PC = NextPC;
  not_bool (cycle_limit_reached());
}

/* -------------------------------------------------------------------------- */
/* -- Entry point and driver for the ELF loader ----------------------------- */

/*
** initialize_regs(pc): initialize the register file, and set the initial
** program load point to the specified address.
*/
val initialize_regs : xlen_t -> unit effect { wreg }
function initialize_regs(pc) = {
  PC = pc;
  NextPC = pc;
  InstrCount = 0;
}

/*
** churn(): fetch and execute instructions. if a cycle limit has been specified
** and it's been reached, this function returns false. otherwise, it always
** returns true.
*/
val churn : unit -> bool effect { escape, rmem, wmem, rreg, wreg }
function churn () = {
  let ast = elf_fetch();
  elf_execute(ast)
}

/*
** print_stats(start_time): Print some execution stats about the current
** CPU, given the time when the CPU started up.
*/
val print_stats : int -> unit effect { rreg }
function print_stats(start_time) = {
  end_time = get_time_ns();
  elapsed  = end_time - start_time;
  inst_1e9 = InstrCount * 1000000000;
  ips = inst_1e9 / elapsed;

  print_int("[Sail] Executed Instructions: ", InstrCount);
  print_int("[Sail] Nanoseconds Elapsed:   ", elapsed);
  print_int("[Sail] Approximate IPS:       ", ips);
  ()
}

/*
** dump_info(verbose, start_time): Dump information about the CPU occasionally,
** if ${verbose} is true, at a rate specified by the dumpStatCycleRate
** configuration register. ${start_time} should be when the CPU started.
*/
val dump_info : (bool, int) -> unit effect { rreg }
function dump_info(verbose, start_time) = {
  if (mod(InstrCount, __cfg_dumpStatsCycleRate) == 0) & verbose then {
    print_ln("");
    print_stats(start_time);
  };
}

/*
** print_regs(): dump the register set at the end of emulation.
*/
val print_regs : unit -> unit effect { rreg }
function print_regs () = {
  prerr_ln("[Sail] Register dump:");

  foreach (i from 0 to 31) {
    let idx = to_bits(5, i);
    let r = if (idx == 0) then "x0" else gp_regbits_to_string(idx);
    let v = X(idx);
    prerr(r ^ ":\t" ^ bits_str(v) ^ " ");
    if (mod(i, 4) == 3) then { prerr("\n"); }
  };
  prerr_ln("")
}

/*
** main(): entry point for the ELF loader. also prints per-iteration stats
** if asked. The arguments for the executable will already be parsed by this
** point in the runtime system.
*/
val main : unit -> unit effect { escape, rmem, wmem, rreg, wreg }
function main() = {
  let verbosity  = get_verbosity();
  let verbose0   = verbosity[0] == bitone;

  let elf_pc     = elf_entry();
  let start_time = get_time_ns();

  initialize_regs(to_bits(32, elf_pc));
  print_ln("[Sail] ELF Initial PC: " ^ hex_str(elf_pc));
  print_ln("[Sail] Executing reset vector...");

  try {
    while (churn ()) do { dump_info(verbose0, start_time) };
  } catch {
    EInvalidInstrBase(w) => { prerr_ln("[Sail] ERROR: Invalid instruction (base encoding) found: " ^ bits_str(w)) },
    EInvalidInstrComp(c) => { prerr_ln("[Sail] ERROR: Invalid instruction (compressed encoding) found: " ^ bits_str(c)) },
    EFeatureDisabled(f)  => { prerr_ln("[Sail] ERROR: Feature Disabled - '" ^ f ^ "'") },
    ENotImplemented(f)   => { prerr_ln("[Sail] ERROR: Not Invented Here - '" ^ f ^ "'") }
  };

  print_ln("\n[Sail] Finished!");
  print_regs();
  print_stats(start_time);
}

/* -------------------------------------------------------------------------- */
/* -- El Fin (elfmain.sail) ------------------------------------------------- */
