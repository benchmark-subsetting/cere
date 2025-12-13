#include <iostream>
#include <vector>

int
__cere__compute_region(std::vector<int>& numbers)
{
  int work = 0;
  for (size_t i = 0; i < numbers.size(); ++i) {
    work += numbers[i] * (int(i) + 1);
  }
  return work;
}

int
main()
{
  std::vector<int> numbers = {1, 2, 3, 4, 5};
  int result = __cere__compute_region(numbers);
  std::cout << "Result: " << result << std::endl;
  return 0;
}
