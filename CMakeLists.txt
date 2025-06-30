cmake_minimum_required(VERSION 3.21)
project(
        cpp_result
        VERSION 0.2.2
        LANGUAGES CXX
        DESCRIPTION "A C++17 implementation of the 'value or error' type Result<T, E>."
        HOMEPAGE_URL "https://github.com/Dragonrun1/result"
)

set(VENDOR_NAME "Michael Cummings")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
option(BUILD_TESTING "" OFF)

if ("${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")
    message(FATAL_ERROR "In-source builds are not allowed. Use a separate build directory")
endif ()

add_library(${PROJECT_NAME} INTERFACE
    "${CMAKE_SOURCE_DIR}/include/${PROJECT_NAME}/result.hpp"
)
# Add library alias so the project can be uses with add_subdirectory
add_library(${PROJECT_NAME}::${PROJECT_NAME} ALIAS ${PROJECT_NAME})

include(GNUInstallDirs)

# Adding the install interface generator expression makes sure that the include
# files are installed to the proper location (provided by GNUInstallDirs)
target_include_directories(
    ${PROJECT_NAME}
    INTERFACE
    $<BUILD_INTERFACE:${${PROJECT_NAME}_SOURCE_DIR}/include/${PROJECT_NAME}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

target_compile_features(${PROJECT_NAME} INTERFACE cxx_std_17)

if (BUILD_TESTING)
    include(CTest)
    enable_testing()
    add_subdirectory("${PROJECT_SOURCE_DIR}/test")
endif () # BUILD_TESTING

# Common install related config and settings
include("common/install/CMakeLists.txt")

# Common CPack related config and settings
include("common/cpack/CMakeLists.txt")

if (CMAKE_SYSTEM_NAME STREQUAL "Windows")
    include("platform/windows/CMakeLists.txt")
elseif (CMAKE_SYSTEM_NAME STREQUAL "Linux")
    include("platform/linux/CMakeLists.txt")
else ()
    message(FATAL_ERROR "${CMAKE_SYSTEM_NAME} is NOT a currently supported platform!")
endif ()

include(CPack)