cmake_minimum_required(VERSION 3.19...3.21)
project(
        cpp_result
        VERSION 0.1.0
        LANGUAGES CXX
        DESCRIPTION "A C++17 implementation of the 'value or error' type Result<T, E>."
        HOMEPAGE_URL "https://github.com/Dragonrun1/result"
)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Catch2 3 REQUIRED)

add_library(cpp_result INTERFACE src/result.hpp)

add_executable(tests ./test/result.cpp)
target_link_libraries(tests PRIVATE Catch2::Catch2WithMain cpp_result)

enable_testing()
add_test(NAME tests COMMAND tests)
