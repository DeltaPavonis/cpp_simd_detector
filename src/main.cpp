#include "simd_detector.h"
#include <print>

int main()
{
    using namespace simd_detector;
    using enum InstructionSet;

    std::println("128-bit SIMD Instruction Sets:");
    std::println("{:<20} = {}", "SSE", is_supported(SSE));
    std::println("{:<20} = {}", "SSE2", is_supported(SSE2));
    std::println("{:<20} = {}", "SSE3", is_supported(SSE3));
    std::println("{:<20} = {}", "SSSE3", is_supported(SSSE3));
    std::println("{:<20} = {}", "SSE4.1", is_supported(SSE4_1));
    std::println("{:<20} = {}", "SSE4.2", is_supported(SSE4_2));
    std::println();

    std::println("256-bit SIMD Instruction Sets:");
    std::println("{:<20} = {}", "AVX", is_supported(AVX));
    std::println("{:<20} = {}", "AVX2", is_supported(AVX2));
    std::println();

    std::println("512-bit SIMD Instruction Sets:");
    std::println("{:<20} = {}", "AVX512_F", is_supported(AVX512_F));
    std::println("{:<20} = {}", "AVX512_DQ", is_supported(AVX512_DQ));
    std::println("{:<20} = {}", "AVX512_IFMA", is_supported(AVX512_IFMA));
    std::println("{:<20} = {}", "AVX512_PF", is_supported(AVX512_PF));
    std::println("{:<20} = {}", "AVX512_ER", is_supported(AVX512_ER));
    std::println("{:<20} = {}", "AVX512_CD", is_supported(AVX512_CD));
    std::println("{:<20} = {}", "AVX512_BW", is_supported(AVX512_BW));
    std::println("{:<20} = {}", "AVX512_VL", is_supported(AVX512_VL));
    std::println("{:<20} = {}", "AVX512_VBMI", is_supported(AVX512_VBMI));
    std::println("{:<20} = {}", "AVX512_VBMI2", is_supported(AVX512_VBMI2));
    std::println("{:<20} = {}", "AVX512_VNNI", is_supported(AVX512_VNNI));
    std::println("{:<20} = {}", "AVX512_BITALG", is_supported(AVX512_BITALG));
    std::println("{:<20} = {}", "AVX512_VPOPCNTDQ", is_supported(AVX512_VPOPCNTDQ));
    std::println("{:<20} = {}", "AVX512_4VNNIW", is_supported(AVX512_4VNNIW));
    std::println("{:<20} = {}", "AVX512_4FMAPS", is_supported(AVX512_4FMAPS));
    std::println("{:<20} = {}", "AVX512_VP2INTERSECT", is_supported(AVX512_VP2INTERSECT));
    std::println("{:<20} = {}", "AVX512_FP16", is_supported(AVX512_FP16));
    std::println();

    std::println("ARM Instruction Sets:");
    std::println("{:<20} = {}", "NEON", is_supported(NEON));
    std::println();

    std::println("x86 Extensions:");
    std::println("{:<20} = {}", "PCLMULQDQ", is_supported(PCLMULQDQ));
    std::println("{:<20} = {}", "FMA3", is_supported(FMA3));

    return 0;
}