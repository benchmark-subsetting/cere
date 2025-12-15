#include <iostream>
#include <vector>
#include <cstdio>

// Déclarations des fonctions de libcere_dump
extern "C" {
    void dump_init(const char* func_name);
    void dump(char* loop_name, char* invocations_str, int arg_count, ...);
    void after_dump(char* loop_name);
}

int compute(std::vector<int>& numbers) {
    int work = 0;
    for (size_t i = 0; i < numbers.size(); ++i) {
        work += numbers[i] * (int(i) + 1);
    }
    return work;
}

int main() {
    std::cerr << "[TEST C++] Starting test\n";
    std::cerr.flush();

    std::cerr << "[TEST C++] Calling dump_init\n";
    std::cerr.flush();
    dump_init("main");

    std::cerr << "[TEST C++] After dump_init, calling dump\n";
    std::cerr.flush();

    std::vector<int> numbers = {1, 2, 3, 4, 5};
    dump(const_cast<char*>("__cere__cpp_compute"), const_cast<char*>("1"), 1, numbers.data());

    std::cerr << "[TEST C++] After dump, calling after_dump\n";
    std::cerr.flush();
    after_dump(const_cast<char*>("__cere__cpp_compute"));

    std::cerr << "[TEST C++] Computing result\n";
    std::cerr.flush();
    int result = compute(numbers);

    std::cerr << "[TEST C++] Test completed, result=" << result << "\n";
    std::cerr.flush();
    return 0;
}
