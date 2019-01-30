static char heap_memory[1024];
static int heap_memory_used = 0;

char *malloc(int size)
{
  char *p = heap_memory + heap_memory_used;
  heap_memory_used += size;
  
  if (heap_memory_used > 1024)
    __asm__ __volatile__("ebreak");
  return p;
}

