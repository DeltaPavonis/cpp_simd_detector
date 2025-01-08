# This `CMakeLists.txt` simply adds an executable corresponding to the usage examples for
# `cpp_simd_detector`; it is run iff the `CPP_SIMD_DETECTOR_BUILD_EXAMPLES` option is found
# to be enabled in the top-level `CMakeLists.txt`.

# Add an executable to be built from the source files in the `examples/` directory (which is just
# `main.cpp`); we name it `cpp_simd_detector_examples`.
add_executable(cpp_simd_detector_examples main.cpp)

# Then, have CMake link the `cpp_simd_detector_examples` executable against the `cpp_simd_detector`
# library (which was added in the top-level `CMakeLists.txt`) using the `target_link_libraries`
# command. This will cause all usage requirements specified for `cpp_simd_detector` (that is, every
# property we set using the `INTERFACE` keyword) to be PROPAGATED to the current executable. In
# turn, `examples/main.cpp` can freely use `#include "cpp_simd_detector.h"`, because the include
# directories for `cpp_simd_detector` will be propagated to `cpp_simd_detector_examples` as well.
target_link_libraries(cpp_simd_detector_examples PRIVATE cpp_simd_detector)