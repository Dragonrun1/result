message(STATUS "Building tests")

include(FetchContent)

FetchContent_Declare(
    Catch2
    GIT_REPOSITORY https://github.com/catchorg/Catch2.git
    GIT_TAG 2b60af89e23d28eefc081bc930831ee9d45ea58b # v3.8.1
    FIND_PACKAGE_ARGS NAMES Catch2
)
set(INSTALL_Catch2 OFF)
FetchContent_MakeAvailable(Catch2)

find_package(Catch2 3.4.0 REQUIRED)

# Add test executable
add_executable("result_tests")
target_sources("result_tests"
    PRIVATE "${CMAKE_SOURCE_DIR}/test/result.cpp"
)
set_target_properties("result_tests" PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/test")
target_link_libraries("result_tests" PRIVATE Catch2::Catch2WithMain result::lib)

# Add test command
add_test(NAME test COMMAND "result_tests")
