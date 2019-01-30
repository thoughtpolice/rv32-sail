// main.S -- glue for the sail emulator

//
// We use the C ABI for the main entry point and the boot shim to start here.
// Currently we use the putchar primitive, but this could be removed to avoid
// a libfirm dep...
//
.pushsection .text

defextern __hiforth_putchar
  ccall putchar
  jr R_JA
endextern __hiforth_putchar

defextern main
  add sp, sp, -16   // Stack space for ret value from __hiforth_main
  add sp, sp, -32*4 // Slots for register values

#define STORE(r) sw x##r, r*4(sp)
  STORE(0)
  STORE(1)
  STORE(2)
  STORE(3)
  STORE(4)
  STORE(5)
  STORE(6)
  STORE(7)
  STORE(8)
  STORE(9)
  STORE(10)
  STORE(11)
  STORE(12)
  STORE(13)
  STORE(14)
  STORE(15)
  STORE(16)
  STORE(17)
  STORE(18)
  STORE(19)
  STORE(20)
  STORE(21)
  STORE(22)
  STORE(23)
  STORE(24)
  STORE(25)
  STORE(26)
  STORE(27)
  STORE(28)
  STORE(29)
  STORE(30)
  STORE(31)
#undef STORE

  jal ra, __hiforth_main
  sw a0, 32*4(sp) // save return value

#define LOAD(r) lw x##r, r*4(sp)
  LOAD(0)
  LOAD(1)
  LOAD(2)
  LOAD(3)
  LOAD(4)
  LOAD(5)
  LOAD(6)
  LOAD(7)
  LOAD(8)
  LOAD(9)
  LOAD(10)
  LOAD(11)
  LOAD(12)
  LOAD(13)
  LOAD(14)
  LOAD(15)
  LOAD(16)
  LOAD(17)
  LOAD(18)
  LOAD(19)
  LOAD(20)
  LOAD(21)
  LOAD(22)
  LOAD(23)
  LOAD(24)
  LOAD(25)
  LOAD(26)
  LOAD(27)
  LOAD(28)
  LOAD(29)
  LOAD(30)
  LOAD(31)
#undef LOAD

  // un-clobber return value
  lw a0, 32*4(sp)

  // Return to libc
  add sp, sp, 32*4 // Register space
  add sp, sp, 16   // Stack space
  jr ra            // Done
endextern main

.popsection //.text

// Local Variables:
// mode: asm
// fill-column: 80
// indent-tabs-mode: nil
// c-basic-offset: 2
// buffer-file-coding-system: utf-8-unix
// End:

