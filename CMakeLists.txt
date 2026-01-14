cmake_minimum_required(VERSION 3.12)
project(cpp_simd_detector
    LANGUAGES CXX)


# --- Set `CMAKE_BUILD_TYPE` the user hasn't already specified it ---

# Multi-config build generators ignore `CMAKE_BUILD_TYPE`, so we won't be able to set a default
if(CMAKE_CONFIGURATION_TYPES)
    message(STATUS "\
        NOTE: You are on a multi-configuration generator (VSCode, XCode, etc). This means\n\
        the build type (Debug, Release, etc) should be set from within the IDE itself, because\n\
        multi-configuration generators ignore the CMAKE_BUILD_TYPE variable)."
    )
endif()

# Default `CMAKE_BUILD_TYPE` to Release if not set
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(STATUS "NOTE: Setting build type to 'Release' as none was specified.")
    set(CMAKE_BUILD_TYPE "Release" CACHE
        STRING "Choose the type of build." FORCE)
    # Set the possible values of build type for cmake-gui
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
      "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()


# --- Add `cpp_simd_detector` as an interface (header-only) library ---
add_library(cpp_simd_detector INTERFACE)


# --- Add compiler options and target properties --- 

# `cpp_simd_detector` requires C++23
target_compile_features(cpp_simd_detector INTERFACE cxx_std_23)
# Also, disable the use of all non-standard C++ extensions in building `cpp_simd_detector`
set_target_properties(cpp_simd_detector PROPERTIES CXX_EXTENSIONS OFF)


# --- Designate include directories for `cpp_simd_detector` ---

# The only include directory for the `cpp_simd_detector` library will simply be `include/`.
# `INTERFACE` tells CMake to propagate that to any target linking to `cpp_simd_detector`
# so they can directly include "simd_detector.h"
target_include_directories(cpp_simd_detector INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)


# --- Add custom build options ---

# If the option `CPP_SIMD_DETECTOR_BUILD_EXAMPLES` is enabled, then we will build the example
# usage of `cpp_simd_detector` presented in the `examples/` directory.
option(CPP_SIMD_DETECTOR_BUILD_EXAMPLES "Build example programs" ON)

# More specifically, if `CPP_SIMD_DETECTOR_BUILD_EXAMPLES` is enabled, then we will have CMake also
# execute the `CMakeLists.txt` file present in `examples/`, which will build the example programs.
if (CPP_SIMD_DETECTOR_BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()