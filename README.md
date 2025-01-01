# cpp_simd_detector
A single-header, cross-platform C++23 utility which detects supported SIMD instruction sets on the host CPU and operating system.

- **Portable**; tested on Windows, MacOS, and Linux.
- **Cross-ISA**; Supports both x86-64 and ARM64.
- **No external dependencies**; written in pure C++23.
- **Correctly detects OS support** as well as processor support for SIMD instruction sets; this is necessary to ensure that SIMD registers are properly saved and restored upon context switches.

## Sample Output
For an Intel Raptor Lake processor, `cpp_simd_detector` outputs
```
128-bit SIMD Instruction Sets:
SSE                  = true
SSE2                 = true
SSE3                 = true
SSSE3                = true
SSE4.1               = true
SSE4.2               = true

256-bit SIMD Instruction Sets:
AVX                  = true
AVX2                 = true

512-bit SIMD Instruction Sets:
AVX512_F             = false
AVX512_DQ            = false
AVX512_IFMA          = false
AVX512_PF            = false
AVX512_ER            = false
AVX512_CD            = false
AVX512_BW            = false
AVX512_VL            = false
AVX512_VBMI          = false
AVX512_VBMI2         = false
AVX512_VNNI          = false
AVX512_BITALG        = false
AVX512_VPOPCNTDQ     = false
AVX512_4VNNIW        = false
AVX512_4FMAPS        = false
AVX512_VP2INTERSECT  = false
AVX512_FP16          = false

ARM Instruction Sets:
NEON                 = false

x86 Extensions:
PCLMULQDQ            = true
FMA3                 = true
```

## General Usage
`cpp_simd_detector` provides two methods for general use:
1. `simd_detector::is_supported` checks if a given `InstructionSet` is supported on the host CPU and operating system. This is useful for creating basic conditional logic based on host SIMD support.
2. `simd_detector::detect_supported_instruction_sets` returns a 64-bit bitmask containing support information for every `InstructionSet`; this is useful for verifying more complex support requirements.

`InstructionSet` is an enumeration which lists all instruction sets/extensions detectable by `cpp_simd_detector`. Currently, `InstructionSet` includes
- **SSE instruction sets**: Up to SSE4.2, including SSSE3
- **AVX instruction sets**: Up to AVX2
- **AVX-512**: AVX-512 Foundation, and all standard AVX-512 features
- **x86 Extensions**: x86 Bit Manipulation Instruction sets, FMA3, and Carryless Multiplication instructions
- **ARM instruction sets**: NEON

## How to Build
This project uses CMake as its build system, so it follows the usual CMake workflow:
1. From the project directory, create a build directory (`mkdir build`).
2. From `build`, run `cmake ..` to configure the build.
3. Finally, build the project; if the previous step generated Makefiles, for instance, then use the command `make`.
