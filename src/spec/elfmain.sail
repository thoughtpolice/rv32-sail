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

/* Prefetch a RISC-V instruction -- importantly, predecode the given
** PC to determine if we have a compressed instruction or not.
*/
val elf_prefetch : xlen_t -> FetchResult effect { rmem, rreg, wreg }
function elf_prefetch (pc) = {
  InstrCount = InstrCount + 1;

  let ilo : xhalf_t = MEM(pc, 2);

  if isCompressed(ilo) then FetchedCompressed(ilo)
  else {
    let ihi  : xhalf_t = MEM(pc + 2, 2);
    let insn : xword_t = append(ihi, ilo);
    FetchedBase(insn);
  }
}

val elf_fetch : unit -> unit effect { rmem, rreg, wreg }
function elf_fetch () = {
  match elf_prefetch(NextPC) {
    FetchError(ex, epc) => {
      print_endline("fetch erro?");
      ()
    },

    FetchedCompressed(h) => {
      match decode_comp(h) {
        None() => {
          print_endline("illegal compressed instruction?");
          ()
        },
        Some(_) => {
          NextPC = NextPC + 2;
          ()
        }
      }
    },

    FetchedBase(w) => {
      match decode_base(w) {
        None() => {
          print_endline("illegal base instruction?");
          ()
        },
        Some(i) => {
          print_endline(i);
          NextPC = NextPC + 4;
          ()
        }
      }
    }
  }
}

/* -------------------------------------------------------------------------- */
/* -- Decode, Execute driver ------------------------------------------------ */

val elf_execute : unit -> bool
function elf_execute () = {

  not_bool (cycle_limit_reached());
}

/* -------------------------------------------------------------------------- */
/* -- Entry point and driver for the ELF loader ----------------------------- */

/* initialize_regs(pc): initialize the register file, and set the initial
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

val churn : unit -> bool effect { rmem, rreg, wreg }
function churn () = {
  elf_fetch(); elf_execute();
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
** dump_info(verbose, rate, start_time): Dump information about the
** CPU occasionally, if 'verbose' is true, at a rate of every 'rate'
** instructions executed. start_time should be when the CPU started
*/
val dump_info : (bool, int, int) -> unit effect { rreg }
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
val main : unit -> unit effect { rmem, rreg, wreg }
function main() = {
  let verbosity  = get_verbosity();
  let verbose0   = verbosity[0] == bitone;

  let elf_pc     = elf_entry();
  let start_time = get_time_ns();

  initialize_regs(to_bits(32, elf_pc));
  print_endline("[Sail] ELF Initial PC: " ^-^ hex_str(elf_pc));
  print_endline("[Sail] Executing...");

  while (churn ()) do { dump_info(verbose0, 10000000, start_time) };

  print_endline("\n[Sail] Finished!");
  print_stats(start_time);
}

/* -------------------------------------------------------------------------- */
/* -- El Fin (elfmain.sail) ------------------------------------------------- */
