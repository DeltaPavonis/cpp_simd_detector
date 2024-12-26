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
#include <utility>  // For `std::to_underlying`

/* If the current instruction set architecture is x86-64, then we case on the current compiler
to `#include` the intrinsics we need (for the `CPUID` and `XGETBV` instructions). */
#if defined(__x86_64__) || defined(_M_AMD64)

    /* Case on the current compiler, and `#include` any available compiler intrinsics for the
    `CPUID` and `XGETBV` instructions. */
    #if defined(__GNUC__) || defined(__clang__)
        /* If our current compiler is GCC or clang, then we check for the existence of two headers:
        - "cpuid.h", which contains the `__get_cpuid_count` intrinsic (which executes the `CPUID`
        instruction); and,
        - "immintrin.h", which contains the `_xgetbv` intrinsic (which executes the `XGETBV`
        instruction). */

        /* Check for the existence of "cpuid.h", which contains the `__get_cpuid_count` intrinsic */
        #if __has_include("cpuid.h")
            #include "cpuid.h"
        #else
            #define GCC_OR_CLANG_LACKS_CPUID_INTRINSIC
        #endif

        /* Then, check for the existence of "immintrin.h", which contains the `_xgetbv` intrinsic.
        Note: Although the actual definition of the `_xgetbv` intrinsic resides in "xsaveintrin.h",
        neither GCC nor Clang allow directly including "xsaveintrin.h". Instead, GCC recommends
        using "x86gprintrin.h", and Clang recommends "immintrin.h". For simplicity, we consistently
        use "immintrin.h", which is supported by both GCC and Clang, and which is also guaranteed
        to be a superset of "x86gprintrin.h". */
        #if __has_include("immintrin.h")
            #include "immintrin.h"
        #else 
            #define GCC_OR_CLANG_LACKS_XGETBV_INTRINSIC
        #endif
        
    #elif defined(_MSC_VER)
        /* Otherwise, if our current compiler is MSVC, then we can directly include the "intrin.h"
        header, which is guaranteed to exist on supported x86/x64 target platforms. */
        #include "intrin.h"
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

/* `get_supported_instruction_sets` currently returns a 64-bit bitmask, which requires the
implicit assumption that `InstructionSet` enumerates at most 64 instruction sets. We verify
that here. */
static_assert(std::to_underlying(InstructionSet::Sentinel) < 64);

/* Now, to define `get_supported_instruction_sets`, we case on the current instruction set
architecture (x86-64 or ARM64). If we are on x86-64 (detected by the macro `__x86_64__` on
GCC/Clang) and `_M_AMD64` on MSVC), then we will detect supported instruction sets using
the x86 `CPUID` and `XGETBV` instructions in conjunction. */
#if defined(__x86_64__) || defined(_M_AMD64)

namespace {  // Begin unnamed namespace

/* `CPUIDDestinationRegister` enumerates the destination registers of the `cpuid` instruction.
This is used to specify which register contains the bit corresponding to a given instruction
set. */
enum class CPUIDDestinationRegister : unsigned {
    /* The destination registers for `cpuid` are %eax, %ebx, %ecx, and %edx, which we represent
    using the values EAX, EBX, ECX, and EDX. We further set the underlying values of EAX/EBX/ECX/EDX=
    to 0/1/2/3, because those underlying values will be later used as indexes into an array
    containing the four registers [%eax, %ebx, %ecx, %edx]. */
    EAX,
    EBX,
    ECX,
    EDX
};

/* `InstructionSetBitLocation` specifies the exact location of the bit that indicates support
for a given instruction set. */
struct InstructionSetBitLocation {
    /* `cpuid_leaf` = Whichever leaf (input parameter) was passed to `cpuid` to query support
    for the current instruction set. This allows us to determine which call to `cpuid` (and thus,
    which specific SET of destination registers) contains the bit corresponding to the current
    instruction set. */
    int cpuid_leaf;

    /* `reg` = Whichever destination register (%ebx, %ecx, or %edx) contains the bit corresponding
    to the current instruction set. */
    CPUIDDestinationRegister reg;

    /* Finally, `bit_position` represents the position of the desired bit within that destination
    register. */
    unsigned bit_position;
};

/* Bring the values of `CPUIDDestinationRegister` into scope to make the below initialization of
`instruction_set_bit_locations` less verbose. Note that because we are in an unnamed `namespace`,
this does not pollute the global scope namespace of including files, as all the `enum` values will
still have internal linkage. */
using enum CPUIDDestinationRegister;

/* Now, `instruction_set_bit_locations` maps every `InstructionSet` to a corresponding
`InstructionSetBitLocation`. The index of an `InstructionSet` in this array is given by
its underlying `InstructionSet` value. */
constexpr auto instruction_set_bit_locations = std::to_array<InstructionSetBitLocation>({
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

/* Verify that `instruction_set_bit_locations` has the same size as the `InstructionSet` enum; that
there is a one-to-one-correspondence between `InstructionSetBitLocation`s and `InstructionSet`s. */
static_assert(instruction_set_bit_locations.size() ==
              std::to_underlying(InstructionSet::Sentinel) + 1);

/* --- IMPLEMENT `cpuid` AND `xgetbv` INSTRUCTION WRAPPERS --- */

/* `cpuid` executes the x86 `CPUID` instruction, treating `eax`/`ebx`/`ecx`/`edx` as pointers to
the `%eax`, `%ebx`, `%ecx`, and `%edx` registers, respectively. More specifically:
- `eax` is treated as a pointer to `%eax`, which is used by `CPUID` as both an input and an output
register. More specifically, the initial value of `%eax` is called the "leaf", and it determines
the main category of information returned by `CPUID` (ex: a leaf of 1 returns feature information).
- `ebx` is treated as a pointer to `%ebx`, which is used solely as an output register by `CPUID`.
- `ecx` is treated as a pointer to `%ecx`, which is used by `CPUID` as both an input and an output
register. More specifically, the initial value of `%ecx` is called the "subleaf", and it provides
finer-grained control over information within an existing leaf.
- `edx` is treated as a pointer to `%edx`, which is used solely as an output register by `CPUID`.

This function works across GCC, Clang, and MSVC.
- For MSVC, it delegates to the `__cpuidex` intrinsic, which is provided as part of `<immintrin.h>`.
- For GCC and Clang, it delegates to the `__get_cpuid_count` intrinsic if the `<cpuid.h>` header
exists, and otherwise falls back to inline assembly.

The caller is responsible for ensuring that `eax`, `ebx`, `ecx`, and `edx` are all valid pointers,
and that the desired leaf and subleaf values are properly stored in `eax` and `ecx` before calling.
*/
void cpuid(unsigned *eax, unsigned *ebx, unsigned *ecx, unsigned *edx) {

    /* If our current compiler is MSVC, we delegate to the `__cpuidex` instrinic, which is provided
    as part of the "intrin.h" header. */
    #if defined(_MSC_VER) && !defined(__clang__) && !defined(__INTEL_COMPILER)
        /* ^^ Note: See Stack Overflow (https://tinyurl.com/5ffxjvnn) for why we check that
        __clang__ and __INTEL_COMPILER are not defined when determining if the current compiler
        is MSVC. */

        /* `__cpuidex` executes the `CPUID` instruction with the given leaf and subleaf (which are
        stored in `*eax` and `*ecx`, respectively), and writes the values of the output registers
        `%eax`/`%ebx`/`%ecx`/`%edx` to the given output array (`cpu_info` here). Note that we use
        the `__cpuidex` intrinsic instead of `__cpuid`, because the latter does not allow specifying
        the subleaf. Source: the MSVC documentation: https://tinyurl.com/5h4nym4f). */
        int cpu_info[4];
        __cpuidex(cpu_info, *eax, *ecx);

        /* Copy the values of the output registers back to `eax`/`ebx`/`ecx`/`edx`. */
        *eax = static_cast<unsigned>(cpu_info[0]);
        *ebx = static_cast<unsigned>(cpu_info[1]);
        *ecx = static_cast<unsigned>(cpu_info[2]);
        *edx = static_cast<unsigned>(cpu_info[3]);

        return;

    /* Otherwise, if our current compiler is GCC or Clang, we case on whether or not the "cpuid.h"
    header was successfully `#include`d. If it was, then we delegate to the `__get_cpuid_count`
    intrinsic it provides. */
    #elif !defined(GCC_OR_CLANG_LACKS_CPUID_INTRINSIC)

        /* `__get_cpuid_count` executes the `CPUID` instruction with the leaf and subleaf (`*eax`
        and `*ecx` here) given in its first two parameters, and writes the values of the output
        registers to the pointers passed in.  */
        if (!__get_cpuid_count(*eax, *ecx, eax, ebx, ecx, edx)) {
            /* `__get_cpuid_count` will return 0 if the given leaf (`*eax`) is unsupported. This
            will only happen on very old processors, but if it does happen, we will conservatively
            set all of the output registers to 0 (indicating that nothing is supported) to avoid
            any false positives. */
            *eax = *ebx = *ecx = *edx = 0;
        }

        return;

    /* Finally, if our current compiler is GCC or Clang but we do NOT have the "cpuid.h" header,
    then we fall back to inline assembly. */
    #else

        /* Use inline assembly to execute the `CPUID` instruction. The input register values are
        given in `*eax` and `*ecx`, and the output registers' values are directed to be written
        to  `*eax`/`*ebx`/`*ecx`/`*edx`. */
        asm volatile("cpuid\n\t"
                /* This line specifies the output registers of the instruction; the output registers
                are %eax/%ebx/%ecx/%edx (represented by "=a"/"=b"/"=c"/"=d"), whose values we write
                back to `*eax`/`*ebx`/`*ecx`/`*edx`. */
                : "=a" (*eax), "=b" (*ebx), "=c" (*ecx), "=d" (*edx)
                /* This line specifies the input registers to the instruction; the input registers
                are %eax and %ecx (represented by "0" and "2"), which will contain the leaf (`*eax`)
                and subleaf (`*ecx`), respectively. */
                : "0"  (*eax), "2"  (*ecx));
        
        /* Note: Alternatively, we would change "=a" and "=c" to "+a" and "+c". This signals that
        the output registers `%eax` and `%ecx` are also to be treated as input registers, which is
        what we need here. */

    #endif
}

/* `read_xcr0` returns the value of Extended Control Register 0 (XCR0), which it reads using the
x86 `XGETBV` instruction. This function works across GCC, Clang, and MSVC. */
auto read_xcr0() -> uint64_t {

    /* Reading XCR0 is done by executing the `XGETBV` instruction with an input of 0. Now, if
    our current compiler is MSVC, we execute `XGETBV` by delegating to the `_xgetbv` intrinsic,
    which is provided by the "intrin.h" header. */
    #if defined(_MSC_VER) && !defined(__clang__) && !defined(__INTEL_COMPILER)
        /* ^^ Note: See Stack Overflow (https://tinyurl.com/5ffxjvnn) for why we check that
        __clang__ and __INTEL_COMPILER are not defined when determining if the current compiler
        is MSVC. */

        /* To read XCR0, we use an input value of 0 to `_xgetbv`. */
        return _xgetbv(0);
    
    /* Otherwise, if our current compiler is GCC or Clang, we case on whether or not the
    "x86gprintrin.h" header was successfully `#include`d. If it was, then we delegate to
    the `_xgetbv` intrinsic it provides. */
    #elif !defined(GCC_OR_CLANG_LACKS_XGETBV_INTRINSIC)

        /* Once again, to read XCR0, we pass in 0 as the input to `_xgetbv` . Unlike MSVC's
        `_xgetbv`, GCC/Clang's `_xgetbv` returns a signed 64-bit integer, so we cast it before
        returning. */
        return static_cast<uint64_t>(_xgetbv(0));
    
    /* Finally, if our current compiler is GCC or Clang but we do NOT have the "x86gprintrin.h"
    header, then we fall back to inline assembly. */
    #else

        /* Use inline assembly to execute the `XGETBV` instruction with an input of 0; this will
        cause the higher-order 32 bits of XCR0 to be written to `%edx`, and the lower-order 32
        bits to be written to `%eax`. It therefore remains to assemble those two pieces back
        into the final 64-bit value. */
        uint32_t xcr0_low_32_bits, xcr0_high_32_bits;
        asm volatile("xgetbv\n\t"
                    /* This line specifies the output registers of the instruction, which are
                    `%eax` and `%edx` (represented by "=a" and "=d"). We direct the values of those
                    output registers to be written to `xcr0_low_32_bits` and `xcr0_high_32_bits`,
                    respectively. */
                    : "=a" (xcr0_low_32_bits), "=d" (xcr0_high_32_bits)
                    /* This line specifies the input register(s) to the instruction, which is
                    only `%ecx`, which we set to 0 (in order to read XCR0). */
                    : "c"  (0));

        /* Return the full 64-bit value of XCR0. */
        return (static_cast<uint64_t>(xcr0_high_32_bits) << 32) | xcr0_low_32_bits;
    #endif
}

};  // End unnamed namespace

/* Returns a bitmask representing instruction set support on the current machine. More specifically,
support for every `InstructionSet` is indicated by the bit with position equal to the underlying
value of that `InstructionSet`. */
auto get_supported_instruction_sets() {
    uint64_t supported_instruction_sets = 0;

    /* Currently, we make two different calls to `cpuid` to determine instruction set support:
    one with leaf 1 and one with leaf 7. Thus, we maintain two separate sets of the four registers
    %eax/%ebx/%ecx/%edx, which we store in arrays to enable convenient access via indexing. */
    std::array<unsigned, 4> registers_for_leaf_1, registers_for_leaf_7;
    auto &[eax_leaf_1, ebx_leaf_1, ecx_leaf_1, edx_leaf_1] = registers_for_leaf_1;
    auto &[eax_leaf_7, ebx_leaf_7, ecx_leaf_7, edx_leaf_7] = registers_for_leaf_7;

    /* Execute `cpuid` with a leaf of 1 and a subleaf of 0; this will write processor info and
    feature bits back to the `registers_for_leaf1`. */
    eax_leaf_1 = 1;
    ecx_leaf_1 = 0;
    cpuid(&eax_leaf_1, &ebx_leaf_1, &ecx_leaf_1, &edx_leaf_1);

    /* Execute `cpuid` with a leaf of 7 and a subleaf of 0; this will write extended feature
    information back to the `registers_for_leaf7`. */
    eax_leaf_7 = 7;
    ecx_leaf_7 = 0;
    cpuid(&eax_leaf_7, &ebx_leaf_7, &ecx_leaf_7, &edx_leaf_7);

    /* `set_bit_for_instruction_set` sets the bit corresponding to the given `instruction_set`
    in the `supported_instruction_sets` bitmask. */
    auto set_bit_for_instruction_set = [&](InstructionSet instruction_set) {
        
        /* The key idea: The position of the bit corresponding to an instruction set in the
        returned bitmask will simply be its underlying `InstructionSet` value. */
        auto index = std::to_underlying(instruction_set);

        /* Now, read the bit returned from `CPUID` corresponding to the given instruction set.
        First, we query `instruction_set_bit_locations` to determine the exact location of that
        bit in the `CPUID` registers; the desired bit is located in the set of registers returned
        from the call to `cpuid` with the given `cpuid_leaf`, in the register `reg`, at the
        bit position `bit_pos`. */
        auto [cpuid_leaf, reg, bit_pos] = instruction_set_bit_locations[index];

        /* Again, `cpuid_leaf` tells us which set of registers the current instruction set's bit
        is located in. */
        const auto &registers = (cpuid_leaf == 1 ? registers_for_leaf_1 : registers_for_leaf_7);

        /* Then, recall that the index of a register `reg` within a `registers` array is given
        by its underlying `CPUIDDestinationRegister` value. */
        auto register_index = std::to_underlying(reg);

        /* Finally, we set the `index`-position bit in `supported_instruction_sets` to the
        `bit_pos`-position bit in the register `reg`. */
        supported_instruction_sets |= ((registers[register_index] >> bit_pos) & 1) << index;
    };

    using enum InstructionSet;

    /* [Shortened] Read XCR0 and use it to verify OS-level support for SIMD instruction sets. */

    /* [1] First, we check support for all XCR0-INDEPENDENT instruction sets. */
    for (auto xcr0_independent_instruction_set : {PCLMULQDQ, BMI1, BMI2}) {
        set_bit_for_instruction_set(xcr0_independent_instruction_set);
    }
    
    /* [2] Then, attempt to read the XCR0 extended control register. */

    /* Reading the XCR0 control register is done by executing the `XGETBV` instruction with an input
    of 0. However, before executing `XGETBV`, we will need to verify that both the processor and the
    OS support it. This is done by inspecting bits 26 and 27 in the `%ecx` register returned from
    the call to `CPUID` with a leaf of 1; if both bits are true, then `XGETBV` is supported. */
    bool has_xsave_support = (ecx_leaf_1 >> 26) & 1, has_osxsave_support = (ecx_leaf_1 >> 27) & 1;
    if (!(has_xsave_support && has_osxsave_support)) {
        /* If we lack processor and/or OS-level support for `XGETBV`, then we will be unable to
        read XCR0, and so we will conservatively assume that there is no OS support for any of the
        remaining instruction sets. As a result, we will return early in that case. */
        return supported_instruction_sets;
    }

    /* Otherwise, if `XGETBV` has both processor-level and OS-level support, we will use it to read
    the value of the XCR0 extended control register. */
    auto xcr0 = read_xcr0();

    /* [3] Now, using the bits in the XCR0 register, we will check whether the SIMD instruction sets
    are fully supported (supported by the current OS as well as the current processor). We will
    specifically check the SSE, AVX, and AVX-512 instruction sets in that order. */

    /* [3][a] Bit 1 in XCR0 indicates OS support for SSE. If this is false, then we return
    immediately, as a lack of OS support for SSE implies a lack of OS support for AVX and AVX-512
    as well. */
    if (!((xcr0 >> 1) & 1)) return supported_instruction_sets;
    
    /* Otherwise, we will update `supported_instruction_sets` with all the SSE instruction sets. */
    for (auto sse_instruction_set : {SSE, SSE2, SSE3, SSSE3, SSE4_1, SSE4_2}) {
        set_bit_for_instruction_set(sse_instruction_set);
    }

    /* [3][b] Then, bit 2 in XCR0 indicates OS support for the AVX instruction set. If this is
    false, then we again return immediately, as a lack of OS support for AVX implies a lack of OS
    support for AVX-512 as well. */
    if (!((xcr0 >> 2) & 1)) return supported_instruction_sets;

    /* Otherwise, we will update `supported_instruction_sets` with all the AVX instruction sets
    and extensions. */
    for (auto avx_instruction_set : {AVX, AVX2, FMA3}) {
        set_bit_for_instruction_set(avx_instruction_set);
    }

    /* [3][c] Finally, OS support for AVX-512 is indicated by bits 5, 6, and 7 in XCR0. If any of
    these three bits are false, then we return immediately once more. */
    if (auto avx512_xcr0_mask = 0b0111'0000U; (xcr0 & avx512_xcr0_mask) != avx512_xcr0_mask) {
        return supported_instruction_sets;
    }

    /* Otherwise, we update `supported_instruction_sets` with all the AVX-512 instruction sets.
    Afterwards, we are done. */
    for (auto avx512_instruction_set : {
        AVX512_F, AVX512_DQ, AVX512_IFMA, AVX512_PF, AVX512_ER, AVX512_CD, AVX512_BW, AVX512_VL,
        AVX512_VBMI, AVX512_VBMI2, AVX512_VNNI, AVX512_BITALG, AVX512_VPOPCNTDQ, AVX512_4VNNIW,
        AVX512_4FMAPS, AVX512_VP2INTERSECT, AVX512_FP16
    }) {
        set_bit_for_instruction_set(avx512_instruction_set);
    }

    return supported_instruction_sets;
}

/* If our current instruction set architecture is ARM64, then `get_supported_instruction_sets` will
simply return a bitmask with only the bit for `InstructionSet::NEON` set. This is because 64-bit
ARM is guaranteed to support NEON (sources: the ARM documentation (https://tinyurl.com/mbkzxpa8)
and Wikipedia (https://tinyurl.com/mvcdk559)), and as every other `InstructionSet` is currently
x86-specific. */
#elif defined(__aarch64__) || defined(_M_ARM64)

/* Returns a bitmask representing instruction set support on the current machine. More specifically,
support for every `InstructionSet` is indicated by the bit with position equal to the underlying
value of that `InstructionSet`. */
auto get_supported_instruction_sets() {
    /* On ARM64, the only supported `InstructionSet` will be NEON. */
    return uint64_t{1} << std::to_underlying(InstructionSet::NEON);
}

/* Fallback case; `cpp_simd_detector` currently handles only the x86-64 and ARM64 ISAs. */
#else

/* Returns a bitmask representing instruction set support on the current machine. More specifically,
support for every `InstructionSet` is indicated by the bit with position equal to the underlying
value of that `InstructionSet`. */
auto get_supported_instruction_sets() -> uint64_t {
    return 0;
}

#endif

/* Returns `true` iff the given `instruction_set` is supported. */
bool is_supported(InstructionSet instruction_set) {
    /* Call `get_supported_instruction_sets` once and cache its value... */
    static auto supported_instruction_sets = get_supported_instruction_sets();

    /* ...then simply read and return the bit corresponding to the given `instruction_set`. */
    return (supported_instruction_sets >> std::to_underlying(instruction_set)) & 1;
}

};  // End namespace `simd_detector`

#endif