/*
@file simd_detector.h
@brief Enumerates all instruction sets detectable by `cpp_simd_detector`, and defines free functions
for detecting instruction set support on the current instruction set architecture.

This file includes the following custom data types/free functions:
- Instruction sets: `enum class InstructionSet`
- Instruction set support functions: `get_supported_instruction_sets`, `is_supported`
*/

#ifndef SIMD_DETECTOR_H
#define SIMD_DETECTOR_H

#include <cstdint>
#include <array>
#include <utility>


/* SIMD detection on x86-64 (aka AMD64) requires intrinsics for the CPUID/XGETBV instructions. */
#if defined(__x86_64__) || defined(_M_AMD64)

    // Where to get those intrinsics depends on the compiler.
    #if defined(_MSC_VER) && !defined(__INTEL_COMPILER)
        // If MSVC (or clang-cl), "intrin.h" must exist on supported x86/x64 target platforms
        #if __has_include(<intrin.h>)
            #include <intrin.h>
        #else
            #error "cpp_simd_detector expects MSVC to provide <intrin.h> on x86-64"
        #endif

    #elif (defined(__GNUC__) || defined(__clang__)) && !defined(_MSC_VER)
        // If GCC/Clang, check <cpuid.h> and <immintrin.h> for `__get_cpuid_count` and `_xgetbv`,
        // respectively. These two intrinsics execute CPUID and XGETBV.
        // Note: clang-cl defines __clang__ and _MSC_VER, which is why we exclude the latter above

        #if __has_include(<cpuid.h>)
            #include <cpuid.h>
        #else
            #define GCC_OR_CLANG_LACKS_CPUID
        #endif

        // Note: `_xgetbv` is actually defined in <xsaveintrin.h>, but both GCC/Clang disallow
        // including that. GCC/Clang instead recommend including <x86gprintrin.h> and <immintrin.h>
        // so we'll just use the latter, since it's guaranteed to include the former.
        #if __has_include(<immintrin.h>)  // This should always be true on GCC/Clang; just in case
            #include <immintrin.h>
        #else 
            #define GCC_OR_CLANG_LACKS_XGETBV
        #endif
    #else
        #error "Unsupported compiler; cpp_simd_detector only supports GCC/Clang/MSVC"
    #endif
    
#endif

namespace simd_detector {  // Begin namespace `simd_detector`

/* `InstructionSet` enumerates the SIMD instruction sets and extensions that `cpp_simd_detector`
is capable of detecting. */
enum class InstructionSet : unsigned {
    /* SSE instruction sets */
    SSE,                 // Streaming SIMD Extensions
    SSE2,                // Streaming SIMD Extensions 2
    SSE3,                // Streaming SIMD Extensions 3
    SSSE3,               // Supplemental Streaming SIMD Extensions 3
    SSE4_1,              // Streaming SIMD Extensions 4.1
    SSE4_2,              // Streaming SIMD Extensions 4.2

    /* Bit Manipulation instruction sets */
    BMI1,                // Bit Manipulation Instruction Set 1
    BMI2,                // Bit Manipulation Instruction Set 2

    /* AVX instruction sets */
    AVX,                 // Advanced Vector Extensions
    AVX2,                // Advanced Vector Extensions 2
    AVX512_F,            // AVX-512 Foundation Instructions
    AVX512_DQ,           // AVX-512 Doubleword and Quadword Instructions
    AVX512_IFMA,         // AVX-512 Integer Fused Multiply-Add Instructions
    AVX512_PF,           // AVX-512 Prefetch Instructions
    AVX512_ER,           // AVX-512 Exponential and Reciprocal Instructions
    AVX512_CD,           // AVX-512 Conflict Detection Instructions
    AVX512_BW,           // AVX-512 Byte and Word Instructions
    AVX512_VL,           // AVX-512 Vector Length Extensions for 128- and 256-bit vectors
    AVX512_VBMI,         // AVX-512 Vector Bit Manipulation Instructions
    AVX512_VBMI2,        // AVX-512 Vector Bit Manipulation Instructions 2
    AVX512_VNNI,         // AVX-512 Vector Neural Network Instructions
    AVX512_BITALG,       // AVX-512 Bit Algorithm Instructions
    AVX512_VPOPCNTDQ,    // AVX-512 Population Count for Doubleword/Quadword Instructions
    AVX512_4VNNIW,       // AVX-512 4-register Neural Network Instructions
    AVX512_4FMAPS,       // AVX-512 4-register Fused Multiply-Add Packed Single Instructions
    AVX512_VP2INTERSECT, // AVX-512 Vector Pairwise Intersect Instructions
    AVX512_FP16,         // AVX-512 Half-Precision Floating-Point Instructions

    /* x86 instruction set extensions */
    PCLMULQDQ,           // Carry-less multiplication instructions
    FMA3,                // Three-operand Fused Multiply-Add Instructions

    /* ARM instruction sets */
    NEON,                // ARM NEON SIMD Instructions

    /* Ending sentinel value, used to determine the total number of `InstructionSet` values */
    Sentinel,
};

/* Make sure `uint64_t` can hold all instruction set support information */
static_assert(std::to_underlying(InstructionSet::Sentinel) < 64);

/* Define `get_supported_instruction_sets` depending on the current ISA (x86-64 or ARM64). */
#if defined(__x86_64__) || defined(_M_AMD64)

namespace internal {  // Begin namespace `simd_detector::internal`

/* `CPUIDDestinationRegister` = destination registers of the `cpuid` instruction, used to
specify which register contains the bit corresponding to a given instruction set. */
enum class CPUIDDestinationRegister : unsigned { EAX, EBX, ECX, EDX };

/* `InstructionSetBitLocation` specifies the exact location of the bit that indicates support
for a given instruction set. */
struct InstructionSetBitLocation {
    int cpuid_leaf;                // Leaf passed to CPUID
    CPUIDDestinationRegister reg;  // Which of %e[b,c,d]x contains the desired bit
    unsigned bit_position;         // Position of the desired bit in `reg`
};


using enum CPUIDDestinationRegister;

/* `instruction_set_bit_locations` maps `InstructionSet`s to `InstructionSetBitLocation`s. */
inline constexpr auto instruction_set_bit_locations = std::to_array<InstructionSetBitLocation>({
    /* SSE instruction sets */
    {1, EDX, 25},  // SSE
    {1, EDX, 26},  // SSE2
    {1, ECX, 0},   // SSE3
    {1, ECX, 9},   // SSSE3
    {1, ECX, 19},  // SSE4_1
    {1, ECX, 20},  // SSE4_2

    /* Bit Manipulation Instruction Sets */
    {7, EBX, 3},   // BMI1
    {7, EBX, 8},   // BMI2

    /* AVX instruction sets */
    {1, ECX, 28},  // AVX
    {7, EBX, 5},   // AVX2
    {7, EBX, 16},  // AVX512_F
    {7, EBX, 17},  // AVX512_DQ
    {7, EBX, 21},  // AVX512_IFMA
    {7, EBX, 26},  // AVX512_PF
    {7, EBX, 27},  // AVX512_ER
    {7, EBX, 28},  // AVX512_CD
    {7, EBX, 30},  // AVX512_BW
    {7, EBX, 31},  // AVX512_VL
    {7, ECX, 1},   // AVX512_VBMI
    {7, ECX, 6},   // AVX512_VBMI2
    {7, ECX, 11},  // AVX512_VNNI
    {7, ECX, 12},  // AVX512_BITALG
    {7, ECX, 14},  // AVX512_VPOPCNTDQ
    {7, EDX, 2},   // AVX512_4VNNIW
    {7, EDX, 3},   // AVX512_4FMAPS
    {7, EDX, 8},   // AVX512_VP2INTERSECT
    {7, EDX, 23},  // AVX512_FP16

    /* x86 instruction set extensions */
    {1, ECX, 1},   // PCLMULQDQ
    {1, ECX, 12},  // FMA3

    /* ARM instruction sets; but because this array is used in the context of the x86-specific
    `CPUID` instruction, ARM instruction sets are irrelevant to it, and so are given an invalid
    bit location. */
    {-1, EAX, 0},

    /* Sentinel (set to an invalid bit location as well). */
    {-1, EAX, 0}
});

/* One-to-one-correspondence between `InstructionSetBitLocation`/`InstructionSet`s */
static_assert(instruction_set_bit_locations.size() ==
              std::to_underlying(InstructionSet::Sentinel) + 1);

/* --- IMPLEMENT `cpuid` AND `xgetbv` INSTRUCTION WRAPPERS --- */

/* Executes the `CPUID` instruction given `eax`/`ebx`/`ecx`/`edx`. Works across GCC/Clang/MSVC. */
inline void cpuid(unsigned *eax, unsigned *ebx, unsigned *ecx, unsigned *edx) {

    /* If MSVC, use `__cpuidex` from <intrin.h> */
    #if defined(_MSC_VER) && !defined(__clang__) && !defined(__INTEL_COMPILER)

        /* `__cpuidex` takes leaf/subleaf in `*eax`/`*ecx`. Note that `__cpuid` exists but doesn't
        have a parameter for the subleaf, which we need here. */
        int cpu_info[4];
        __cpuidex(cpu_info, *eax, *ecx);

        /* `__cpuidex` writes to an output array (`cpu_info`) */
        *eax = static_cast<unsigned>(cpu_info[0]);
        *ebx = static_cast<unsigned>(cpu_info[1]);
        *ecx = static_cast<unsigned>(cpu_info[2]);
        *edx = static_cast<unsigned>(cpu_info[3]);

        return;

    /* Otherwise, if GCC or Clang, either use `__get_cpuid_count` (if "cpuid.h" was found) or else
    fall back to inline assembly. */
    #elif !defined(GCC_OR_CLANG_LACKS_CPUID)

        /* `__get_cpuid_count` takes leaf/subleaf in `*eax` and `*ecx`. */
        if (!__get_cpuid_count(*eax, *ecx, eax, ebx, ecx, edx)) {
            // 0 will be returned if the leaf `*eax` is unsupported. This may happen on very old
            // processors; if it does, we set all outputs to 0 to indicate no SIMD support.
            *eax = *ebx = *ecx = *edx = 0;
        }

        return;

    #else  // Fall back to inline assembly

        /* Inputs are `*eax`/`*ecx`, and we direct outputs to be written to `*e[a,b,c,d]x`. */
        asm volatile("cpuid\n\t"
            : "=a" (*eax), "=b" (*ebx), "=c" (*ecx), "=d" (*edx)  // Specify outputs
            : "0"  (*eax), "2"  (*ecx)  // Inputs (0 = %eax, 2 = %ecx; leaf and subleaf)
        );

        // Alternatively, change "=[a,c]" to "+[a, c]", which means the outputs %e[a,c]x are
        // also to be treated as inputs, which is what we need here.

    #endif
}

/* Define `read_xcr0`, which reads XCR0 using `xgetbv(0)`. Works across GCC/Clang/MSVC. */
#if defined(_MSC_VER) && !defined(__clang__) && !defined(__INTEL_COMPILER)
    /* If MSVC, just use `_xgetbv` from <intrin.h> */
    inline auto read_xcr0() -> uint64_t { return _xgetbv(0); }
#elif !defined(GCC_OR_CLANG_LACKS_XGETBV)
    /* If GCC or Clang, either use `_xgetbv` from <immintrin.h> or fall back to inline assembly.
    IMPORTANT: This requires enabling the `xsave` target feature (either by using a function-level
    target attribute, as we do, or by adding the `-mxsave` flag to the compiler; we choose the
    former since the `-mxsave` flag only exists on x86).
    IMPORTANT: We force `read_xcr0` to not be inlined to prevent `_xgetbv` from being inlined into
    other code (which may cause target-specific option mismatches, if it gets inlined into code
    which doesn't have `__attribute__((target("xsave")))` on it. The compiler might be smart enough
    to know never to do this, but just in case). */
    __attribute__((target("xsave"), noinline))
    inline auto read_xcr0() -> uint64_t {
        return static_cast<uint64_t>(_xgetbv(0)); // Cast b.c. GCC/Clang `_xgetbv` returns `int64_t`
    }
#else  // Fall back to inline assembly
    inline auto read_xcr0() -> uint64_t {
        /* `xgetbv(0)` writes the upper/lower 32 bits of XCR0 to `%e[d,a]x` */
        uint32_t xcr0_low_32_bits, xcr0_high_32_bits;
        asm volatile("xgetbv\n\t"
            : "=a" (xcr0_low_32_bits), "=d" (xcr0_high_32_bits)  // Specify outputs
            : "c"  (0)  // Inputs (c = %ecx)
        );

        // Stitch `xcr0_[low,high]_32_bits` together into a `uint64_t`
        return (static_cast<uint64_t>(xcr0_high_32_bits) << 32) | xcr0_low_32_bits;
    }
#endif

};  // End namespace `simd_detector::internal`

/* Returns a bitmask representing instruction set support on the current machine */
inline auto get_supported_instruction_sets() {
    using namespace internal;

    uint64_t supported_instruction_sets = 0;

    /* We make two calls to `CPUID` (leaf 1 and leaf 7) */
    std::array<unsigned, 4> registers_for_leaf_1, registers_for_leaf_7;
    auto &[eax_leaf_1, ebx_leaf_1, ecx_leaf_1, edx_leaf_1] = registers_for_leaf_1;
    auto &[eax_leaf_7, ebx_leaf_7, ecx_leaf_7, edx_leaf_7] = registers_for_leaf_7;

    // Leaf 1
    eax_leaf_1 = 1;
    ecx_leaf_1 = 0;
    cpuid(&eax_leaf_1, &ebx_leaf_1, &ecx_leaf_1, &edx_leaf_1);

    // Leaf 7
    eax_leaf_7 = 7;
    ecx_leaf_7 = 0;
    cpuid(&eax_leaf_7, &ebx_leaf_7, &ecx_leaf_7, &edx_leaf_7);

    /* Sets the bit for one instruction set in `supported_instruction_sets` */
    auto set_bit_for_instruction_set = [&](InstructionSet instruction_set) {
    
        auto const index = std::to_underlying(instruction_set);

        /* Now, read the bit returned from `CPUID` corresponding to the given instruction set.
        First go to `instruction_set_bit_locations` to determine where that bit is */
        auto const [cpuid_leaf, reg, bit_pos] = instruction_set_bit_locations[index];

        /* Read the returned registers with the leaf `cpuid_leaf`, in the register `reg`, at the
        bit position `bit_pos`, and set that in `supported_instruction_sets` */
        const auto &registers = (cpuid_leaf == 1 ? registers_for_leaf_1 : registers_for_leaf_7);
        auto const register_index = std::to_underlying(reg);

        supported_instruction_sets |= ((registers[register_index] >> bit_pos) & 1) << index;
    };

    using enum InstructionSet;

    /* Read XCR0 and use it to verify OS-level support for SIMD instruction sets. */

    /* Before reading XCR0, check support for all instruction sets that don't require it */
    for (auto xcr0_independent_instruction_set : {PCLMULQDQ, BMI1, BMI2}) {
        set_bit_for_instruction_set(xcr0_independent_instruction_set);
    }

    /* Read XCR0 using `XGETBV`. Before that, we need to check if `XGETBV` itself is supported.
    Thankfully, that's given in the result of executing `CPUID` with a leaf of 1. */
    bool const has_xsave_support = (ecx_leaf_1 >> 26) & 1;
    bool const has_osxsave_support = (ecx_leaf_1 >> 27) & 1;
    if (!(has_xsave_support && has_osxsave_support)) {  // No support for `XGETBV`; assume no SIMD
        return supported_instruction_sets;
    }

    /* `XGETBV` is supported, use it to read XCR0 */
    auto const xcr0 = read_xcr0();

    /* Bit 1/2/[5,6,7] in XCR0 indicates OS support for SSE/AVX/AVX-512. */
    if (!((xcr0 >> 1) & 1)) { return supported_instruction_sets; }
    for (auto const sse_instruction_set : {SSE, SSE2, SSE3, SSSE3, SSE4_1, SSE4_2}) {
        set_bit_for_instruction_set(sse_instruction_set);
    }

    if (!((xcr0 >> 2) & 1)) { return supported_instruction_sets; }
    for (auto const avx_instruction_set : {AVX, AVX2, FMA3}) {
        set_bit_for_instruction_set(avx_instruction_set);
    }

    if (auto const avx512_xcr0_mask = 0b0111'0000U; (xcr0 & avx512_xcr0_mask) != avx512_xcr0_mask) {
        return supported_instruction_sets;
    }

    for (auto const avx512_instruction_set : {
        AVX512_F, AVX512_DQ, AVX512_IFMA, AVX512_PF, AVX512_ER, AVX512_CD, AVX512_BW, AVX512_VL,
        AVX512_VBMI, AVX512_VBMI2, AVX512_VNNI, AVX512_BITALG, AVX512_VPOPCNTDQ, AVX512_4VNNIW,
        AVX512_4FMAPS, AVX512_VP2INTERSECT, AVX512_FP16
    }) {
        set_bit_for_instruction_set(avx512_instruction_set);
    }

    /* Having checked processor/OS support for SSE/AVX/AVX-512 and everything else, we're done */
    return supported_instruction_sets;
}

/* If the ISA's ARM64, our job is easy; ARM64 is guaranteed to support Neon, and that's the only
ARM instruction set we have in `InstructionSet`. */
#elif defined(__aarch64__) || defined(_M_ARM64)

/* Returns a bitmask representing instruction set support on the current machine. */
inline auto get_supported_instruction_sets() {
    return uint64_t{1} << std::to_underlying(InstructionSet::NEON);
}

#else

#warning "Unsupported ISA; cpp_simd_detector only supports x86-64 and ARM64"

/* Returns a bitmask representing instruction set support on the current machine */
inline auto get_supported_instruction_sets() -> uint64_t { return 0; }

#endif

/* Returns whether the given `instruction_set` is supported. */
inline bool is_supported(InstructionSet const instruction_set) {
    /* Call `get_supported_instruction_sets` once */
    static auto const supported_instruction_sets = get_supported_instruction_sets();
    return (supported_instruction_sets >> std::to_underlying(instruction_set)) & 1;
}

};  // End namespace `simd_detector`


// Undefine any macros we defined
#ifdef GCC_OR_CLANG_LACKS_CPUID
    #undef GCC_OR_CLANG_LACKS_CPUID
#endif

#ifdef GCC_OR_CLANG_LACKS_XGETBV
    #undef GCC_OR_CLANG_LACKS_XGETBV
#endif

#endif
