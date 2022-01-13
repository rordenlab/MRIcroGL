CloudFlare zlib 29 Sept 2020
Compiled By Chris Rorden
https://github.com/cloudflare/zlib

./configure
edit makefile for "-mmacosx-version-min=10.8"
 CFLAGS= -mmacosx-version-min=10.8 -DHAS_PCLMUL -mpclmul -DHAS_SSE42 -msse4.2 -DADLER32_SIMD_SSSE3 -mssse3 -DINFLATE_CHUNK_SIMD_SSE2 -msse2 -DINFLATE_CHUNK_READ_64LE -O3  -DHAVE_HIDDEN
make

