cmake_minimum_required(VERSION 3.12)
project(cpp_simd_detector
    LANGUAGES CXX)


# --- Set the `CMAKE_BUILD_TYPE` variable if the user has not specified it already ---

# Notify user if their build generator is multi-configuration, because this prevents
# us from setting a default `CMAKE_BUILD_TYPE` (as multi-config generators ignore
# the `CMAKE_BUILD_TYPE` variable).
if(CMAKE_CONFIGURATION_TYPES)
    message(STATUS "\
        NOTE: You are on a multi-configuration generator (VSCode, XCode, etc). This means\n\
        the build type (Debug, Release, etc) should be set from within the IDE itself, because\n\
        multi-configuration generators ignore the CMAKE_BUILD_TYPE variable)."
    )
endif()

# Set the default build type to Release if `CMAKE_BUILD_TYPE` has not been set previously,
# and if the build generator is single-configuration (because if it is multi-config, then
# it ignores `CMAKE_BUILD_TYPE`). From https://www.kitware.com/cmake-and-the-default-build-type/.
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(STATUS "NOTE: Setting build type to 'Release' as none was specified.")
    set(CMAKE_BUILD_TYPE "Release" CACHE
        STRING "Choose the type of build." FORCE)
    # Set the possible values of build type for cmakde-gui
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
      "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()


# --- Add `cpp_simd_detector` as an interface (header-only) library ---

# In CMake, an "interface library" is a header-only library which produces no build artifacts, but
# which has certain usage requirements (e.g. specific compiler options) that must be PROPAGATED to
# any target which wishes to consume (link against) it. Interface libraries are CMake's abstraction
# over header-only libraries, and we therefore add `cpp_simd_detector` as an interface library here.
# This is done by passing the `INTERFACE` keyword to the regular `add_library` command .
add_library(cpp_simd_detector INTERFACE)


# --- Add compiler options and target properties --- 

# `cpp_simd_detector` requires C++23. Thus, we will require C++23 for any target that consumes
# (links against) `cpp_simd_detector`. This is achieved by passing the `INTERFACE` keyword to the
# `target_compile_features` command, since `INTERFACE` is used to indicate that a given property or
# dependency is meant to be used by consumers of a target, rather than the target itself. 
target_compile_features(cpp_simd_detector INTERFACE cxx_std_23)

# Additionally, disable the use of all non-standard C++ extensions in building `cpp_simd_detector`.
set_target_properties(cpp_simd_detector PROPERTIES CXX_EXTENSIONS OFF)

# Add the `-mxsave` option when compiling on GCC or Clang to enable the use of their `_xgetbv`
# intrinsic (without `-mxsave`, attempting to call `_xgetbv` will result in a compilation error).
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")

    # Using the `INTERFACE` keyword here tells CMake to PROPAGATE the specified compiler options
    # to any target that links to it.
    target_compile_options(cpp_simd_detector INTERFACE -mxsave)
endif()


# --- Designate include directories for `cpp_simd_detector` ---

# The only include directory for the `cpp_simd_detector` library will simply be `include/`,
# which we pass to `target_include_directories`. Here, the `INTERFACE` keyword tells CMake
# to propagate these include directories to any target which links to `cpp_simd_detector`;
# as a result, those targets will be able to directly include "simd_detector.h", rather than
# something like `cpp_simd_detector/include/simd_detector.h".
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