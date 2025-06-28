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
option(BUILD_TESTING "" OFF)
include(CTest)
add_library(${PROJECT_NAME} INTERFACE
        "src/result.hpp"
)

if (BUILD_TESTING)
    message(STATUS "Building tests")

    include(FetchContent)

    FetchContent_Declare(
            Catch2
            GIT_REPOSITORY https://github.com/catchorg/Catch2.git
            GIT_TAG 2b60af89e23d28eefc081bc930831ee9d45ea58b # v3.8.1
            FIND_PACKAGE_ARGS NAMES Catch2
    )
    FetchContent_MakeAvailable(Catch2)

    find_package(Catch2 3.4.0 REQUIRED)

    # Add test executable
    add_executable("${PROJECT_NAME}_tests"
            "./test/result.cpp"
    )
    set_target_properties("${PROJECT_NAME}_tests" PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/test")
    target_link_libraries("${PROJECT_NAME}_tests" PRIVATE Catch2::Catch2WithMain ${PROJECT_NAME})

    enable_testing()
    # Add test command
    add_test(NAME test COMMAND "${PROJECT_NAME}_tests")

endif () # BUILD_TESTING
