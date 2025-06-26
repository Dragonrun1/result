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
    # Try finding Git package only if BUILD_TESTING is ON
    find_package(Git QUIET)

    if (GIT_FOUND AND EXISTS "${PROJECT_SOURCE_DIR}/.git")
        # Update submodules as needed
        option(GIT_SUBMODULE "Check submodules during build" ON)
        if (GIT_SUBMODULE)
            message(STATUS "Submodule update")
            execute_process(
                    COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive
                    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                    RESULT_VARIABLE GIT_SUBMODULE_RESULT
            )
            if (NOT GIT_SUBMODULE_RESULT EQUAL "0")
                message(FATAL_ERROR "git submodule update --init --recursive failed with ${GIT_SUBMODULE_RESULT}, please checkout submodules")
            endif ()
        endif ()
    endif ()

    # First try finding testing dependency Catch2 package in the
    # test/lib/Catch2/ directory
    find_package(Catch2 3 REQUIRED
        PATHS "${PROJECT_SOURCE_DIR}/test/lib/Catch2"
        NO_DEFAULT_PATH
    )
    find_package(Catch2 3 REQUIRED)

    # Enable adding tests by finding CTest package
#    find_package(CTest REQUIRED)

    # Add test executable
    add_executable("${PROJECT_NAME}_tests"
            "./test/result.cpp"
    )
    set_target_properties("${PROJECT_NAME}_tests" PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/test")
    target_link_libraries("${PROJECT_NAME}_tests" PRIVATE Catch2::Catch2WithMain ${PROJECT_NAME})

    # Add test command
    add_test(NAME test COMMAND "${PROJECT_NAME}_tests")

endif ()


enable_testing()
