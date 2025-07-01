# Copyright (c) 2025 Michael Cummings
# SPDX-License-Identifier: MIT

# set some project metadata for the installer
set(CPACK_PACKAGE_CONTACT "mgcummings@yahoo.com")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY ${CMAKE_PROJECT_DESCRIPTION})
set(CPACK_PACKAGE_DIRECTORY distribution)
set(CPACK_PACKAGE_INSTALL_DIRECTORY ${CPACK_PACKAGE_NAME})
set(CPACK_PACKAGE_NAME ${CMAKE_PROJECT_NAME})
set(CPACK_PACKAGE_VENDOR "Michael Cummings")
set(CPACK_PACKAGE_VERSION_MAJOR ${CMAKE_PROJECT_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${CMAKE_PROJECT_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${CMAKE_PROJECT_VERSION_PATCH})
set(CPACK_RESOURCE_FILE_LICENSE "../LICENSE")
set(CPACK_SOURCE_IGNORE_FILES
    "/.git/"
    "/.idea/"
    "/build/"
    "/cmake-build-debug/"
    "/cmake-build-release/"
    "/CMakeCache.txt"
    "/CMakeFiles/"
    "/cmake_install.cmake"
    "/CPackConfig.cmake"
)
set(CPACK_VERBATIM_VARIABLES YES)

# Probably unneeded for single file INTERFACE library but shouldn't cause harm
set(CPACK_COMPONENTS_GROUPING IGNORE)
