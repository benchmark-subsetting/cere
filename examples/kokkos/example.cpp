#include <cmath>
#include <chrono>
#include <cstdlib>
#include <fstream>
#include <iostream>

#include <Kokkos_Core.hpp>

static void __cere__kokkos_kernel(Kokkos::View<double*> data, double scale) {
  const int work_loops = 16;
  const size_t elements = data.extent(0);
  for (int pass = 0; pass < work_loops; ++pass) {
    Kokkos::parallel_for("kokkos_kernel",
                         Kokkos::RangePolicy<>(0, elements),
                         KOKKOS_LAMBDA(const size_t idx) {
                           const double value = data(idx);
                           double current = value + scale + pass * 0.01;
                           for (int round = 0; round < 8; ++round) {
                             current = std::sin(current) * std::cos(current + round * 0.1);
                           }
                           data(idx) = current + 0.5;
                         });
    Kokkos::fence();
  }
}

static void __cere__kokkos_seed(Kokkos::View<double*> data, double seed) {
  Kokkos::parallel_for(
      "kokkos_seed",
      Kokkos::RangePolicy<>(0, data.extent(0)),
      KOKKOS_LAMBDA(const size_t idx) { data(idx) = std::sin(seed + idx); });
  Kokkos::fence();
}

int main(int argc, char* argv[]) {
  Kokkos::ScopeGuard guard(argc, argv);

  const size_t elements = 1 << 18;
  Kokkos::View<double*> buffer("kokkos_buffer", elements);

  __cere__kokkos_seed(buffer, 0.5);

  auto start = std::chrono::steady_clock::now();
  for (int iteration = 0; iteration < 64; ++iteration) {
    __cere__kokkos_kernel(buffer, iteration * 0.125);
    if ((iteration & 0x7) == 0) {
      __cere__kokkos_seed(buffer, iteration * 0.25 + 1.0);
    }
  }
  auto end = std::chrono::steady_clock::now();

  const auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
  std::cout << "kokkos iterations=64 time=" << elapsed.count() << "ms\n";

  if (std::getenv("CERE_MODE")) {
    std::ofstream main_csv("main.csv");
    if (main_csv.is_open()) {
      main_csv << "Codelet Name,Call Count,CPU_CLK_UNHALTED_CORE\n";
      main_csv << "kokkos-example,1," << elapsed.count() << "\n";
    }
  }
  return 0;
}
