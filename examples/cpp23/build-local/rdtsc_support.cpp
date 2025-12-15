#include <cstddef>

extern "C" void
serialize(void)
{
#if defined(__amd64__)
  asm volatile("cpuid" ::: "%rax", "%rbx", "%rcx", "%rdx");
#elif defined(__aarch64__)
  asm volatile("isb" : : : "memory");
#endif
}

extern "C" unsigned long long
rdtsc(void)
{
#if defined(__amd64__)
  unsigned int high, low;
  asm volatile("rdtsc" : "=a"(low), "=d"(high));
  return (static_cast<unsigned long long>(high) << 32) | low;
#elif defined(__aarch64__)
  unsigned long long value;
  asm volatile("mrs %0, cntvct_el0" : "=r"(value));
  return value;
#else
  return 0;
#endif
}
