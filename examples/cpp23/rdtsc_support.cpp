#include <cstdint>

extern "C" {

void serialize(void) {
#if defined(__amd64__)
  __asm__ volatile("cpuid" ::: "%rax", "%rbx", "%rcx", "%rdx");
#elif defined(__aarch64__)
  __asm__ volatile("isb" : : : "memory");
#endif
}

unsigned long long int rdtsc(void) {
#if defined(__amd64__)
  unsigned long long int a, d;
  __asm__ volatile("rdtsc" : "=a"(a), "=d"(d));
  return (d << 32) | a;
#elif defined(__aarch64__)
  unsigned long long int a;
  __asm__ volatile("mrs %0, cntvct_el0" : "=r"(a));
  return a;
#else
  return 0;
#endif
}

} // extern "C"
