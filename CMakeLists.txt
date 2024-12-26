cmake_minimum_required(VERSION 3.12)
project(cpp_simd_detector
    LANGUAGES CXX)

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

# Enumerate the source files for `cpp_simd_detector`...
set(CPP_SIMD_DETECTOR_SOURCES
    src/main.cpp
)

# ...and add the executable, which also has the name `cpp_simd_detector`.
add_executable(cpp_simd_detector ${CPP_SIMD_DETECTOR_SOURCES})

# Require C++23 for `cpp_simd_detector` (and because I use PUBLIC, also for all targets that link to
# `cpp_simd_detector`), and also avoid having extensions being added.
target_compile_features(cpp_simd_detector PUBLIC cxx_std_23)
set_target_properties(cpp_simd_detector PROPERTIES CXX_EXTENSIONS OFF)

# Add the `-mxsave` option when compiling on GCC or Clang to enable the use of their `_xgetbv`
# intrinsic (without `-mxsave`, attempting to call `_xgetbv` will result in a compilation error).
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_compile_options(cpp_simd_detector PRIVATE -mxsave)
endif()

# Use `include/` as an include directory for building the `cpp_simd_detector` executable
target_include_directories(cpp_simd_detector PRIVATE ${CMAKE_SOURCE_DIR}/include)