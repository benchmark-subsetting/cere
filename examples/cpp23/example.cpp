#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <random>
#include <ranges>
#include <vector>

int main() {
  std::vector<int> numbers(4096);
  std::ranges::copy(std::views::iota(0) | std::views::take(numbers.size()), numbers.begin());

  auto rng = std::mt19937(std::random_device{}());
  std::ranges::shuffle(numbers, rng);

  std::ranges::sort(numbers, std::greater<>{});

  auto start = std::chrono::high_resolution_clock::now();
  for (size_t i = 1; i < numbers.size(); ++i) {
    numbers[i] += numbers[i - 1];
  }
  for (size_t i = 0; i < numbers.size(); ++i) {
    numbers[i] *= 2;
  }
  auto end = std::chrono::high_resolution_clock::now();
  auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);

  std::cout << "max=" << numbers.front()
            << " min=" << numbers.back()
            << " time=" << std::chrono::duration_cast<std::chrono::milliseconds>(end.time_since_epoch()).count()
            << "\n";

  if (std::getenv("CERE_MODE")) {
    std::ofstream main_csv("main.csv");
    if (main_csv.is_open()) {
      main_csv << "Codelet Name,Call Count,CPU_CLK_UNHALTED_CORE\n";
      main_csv << "example,1," << elapsed.count() << "\n";
    }
  }

  return 0;
}
