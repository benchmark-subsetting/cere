#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <random>
#include <ranges>
#include <thread>
#include <vector>

static int
__cere__compute_region(std::vector<int>& numbers)
{
  int work = 0;
  for (size_t passes = 0; passes < 3; ++passes) {
    for (size_t i = 0; i < numbers.size(); ++i) {
      const int value = numbers[i];
      work += (value * (int(i) + 1)) ^ (passes + 1);
      numbers[i] = ((value + passes) * 13) ^ ((int(i) + 7) >> 1);
    }
  }
  return work;
}

static int
__cere__combine_region(const std::vector<int>& numbers)
{
  int sum = 0;
  for (size_t window = 1; window < numbers.size(); window += 11) {
    for (size_t idx = window; idx < std::min(numbers.size(), window + 64);
         ++idx) {
      sum += numbers[idx] * (int(idx) ^ 0x15);
    }
  }
  return sum;
}

static void
__cere__randomize(std::vector<int>& numbers)
{
  std::mt19937 rng(std::random_device{}());
  std::uniform_int_distribution<int> dist(1, 128);
  for (size_t i = 0; i < numbers.size(); ++i) {
    numbers[i] = dist(rng);
  }
}

int
main()
{
  std::vector<int> numbers(8192);
  std::ranges::copy(
      std::views::iota(0) | std::views::take(numbers.size()), numbers.begin());
  __cere__randomize(numbers);

  auto start = std::chrono::steady_clock::now();
  int work = 0;
  int aggregate = 0;
  const auto target_duration = std::chrono::seconds(3);
  auto deadline = start + target_duration;
  int rounds = 0;
  while (std::chrono::steady_clock::now() < deadline) {
    work += __cere__compute_region(numbers);
    aggregate += __cere__combine_region(numbers);
    if ((rounds & 0x7) == 0) {
      __cere__randomize(numbers);
      std::this_thread::sleep_for(std::chrono::microseconds(50));
    }
    ++rounds;
  }
  auto end = std::chrono::steady_clock::now();
  auto elapsed =
      std::chrono::duration_cast<std::chrono::milliseconds>(end - start);

  std::cout << "max=" << numbers.front() << " min=" << numbers.back()
            << " work=" << work << " aggregate=" << aggregate
            << " time=" << elapsed.count() << "\n";

  if (std::getenv("CERE_MODE")) {
    std::ofstream main_csv("main.csv");
    if (main_csv.is_open()) {
      main_csv << "Codelet Name,Call Count,CPU_CLK_UNHALTED_CORE\n";
      main_csv << "example,1," << elapsed.count() << "\n";
    }
  }

  return 0;
}
