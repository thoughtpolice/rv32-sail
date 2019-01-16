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

val elf_fetch : unit -> unit
function elf_fetch () = {
  PC = NextPC;
  // NextPC = PC + 4;
  InstrCount = InstrCount + 1;

  let iaddr = MEM(PC, 4);
  let instr = decode(iaddr);

  match instr {
    Some(d) => (),
    None()  => {
      /* decode() should never return none; see scattered.sail */
      print_endline("elf_fetch(): impossible case! Decoding returned None?!");
      
    }
  };
  ();
}

/* -------------------------------------------------------------------------- */
/* -- Decode, Execute driver ------------------------------------------------ */

val elf_execute : unit -> bool
function elf_execute () = {
  let pc_v = PC;

  // test
  // print_endline("test PC:" ^-^ hex_str(pc_v));

  not_bool (cycle_limit_reached());
}

/* -------------------------------------------------------------------------- */
/* -- Entry point and driver for the ELF loader ----------------------------- */

/* initialize_regs(pc): initialize the register file, and set the initial
** program load point to the specified address.
*/
val initialize_regs : xlen_t -> unit
function initialize_regs(pc) = {
  NextPC = pc;
  InstrCount = 0;
}

/*
** churn(): fetch and execute instructions. if a cycle limit has been specified
** and it's been reached, this function returns false. otherwise, it always
** returns true.
*/
val churn : unit -> bool
function churn () = {
  elf_fetch(); elf_execute();
}

/*
** print_stats(start_time): Print some execution stats about the current
** CPU, given the time when the CPU started up.
*/
val print_stats : int -> unit
function print_stats(start_time) = {
  end_time = get_time_ns();
  elapsed  = end_time - start_time;
  inst_1e9 = InstrCount * 1000000000;
  ips = inst_1e9 / elapsed;

  print_int("[Sail] Executed Instructions: ", InstrCount);
  print_int("[Sail] Nanoseconds Elapsed:   ", elapsed);
  print_int("[Sail] Approximate IPS:       ", ips);
}

/*
** dump_info(verbose, rate, start_time): Dump information about the
** CPU occasionally, if 'verbose' is true, at a rate of every 'rate'
** instructions executed. start_time should be when the CPU started
*/
val dump_info : (bool, int, int) -> unit
function dump_info(verbose, dump_rate, start_time) = {
  if (mod(InstrCount, dump_rate) == 0) & verbose then {
    print_endline("");
    print_stats(start_time);
  };
}

/*
** main(): entry point for the ELF loader. also prints per-iteration stats
** if asked. The arguments for the executable will already be parsed by this
** point in the runtime system.
*/
val main : unit -> unit
function main() = {
  let verbosity  = get_verbosity();
  let elf_pc     = elf_entry();
  let start_time = get_time_ns();
  let verbose0   = verbosity[0] == bitone;

  print_endline("[Sail] ELF Initial PC: " ^-^ hex_str(elf_pc));
  initialize_regs(to_bits(64, elf_pc));

  print_endline("[Sail] Executing...");
  while (churn ()) do { dump_info(verbose0, 10000000, start_time) };

  print_endline("\n[Sail] Finished!");
  print_stats(start_time);
}

/* -------------------------------------------------------------------------- */
/* -- El Fin (elfmain.sail) ------------------------------------------------- */
