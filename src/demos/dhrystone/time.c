long time()
{
  int cycles;
  __asm__ __volatile__("rdcycle %0" : "=r"(cycles));
  return cycles;
}

long insn()
{
  int insns;
  __asm__ __volatile__("rdinstret %0" : "=r"(insns));
  return insns;
}
