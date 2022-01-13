CloudFlare zlib 29 Sept 2020
Compiled By Chris Rorden
https://github.com/cloudflare/zlib
gcc version 8.1.0 

gcc -O3  -c -o adler32.o adler32.c
gcc -O3  -c -o crc32.o crc32.c
gcc -O3  -c -o deflate.o deflate.c  -msse4
gcc -O3  -c -o inffast.o inffast.c
gcc -O3  -c -o inflate.o inflate.c
gcc -O3  -c -o inftrees.o inftrees.c
gcc -O3  -c -o trees.o trees.c
gcc -O3  -c -o zutil.o zutil.c
gcc -O3  -c -o adler32_simd.o adler32_simd.c
gcc -O3  -c -o crc32_simd.o crc32_simd.c
gcc -O3  -c -o inffast_chunk.o inffast_chunk.c


gcc -O3  -c -o adler32.o adler32.c -DHAS_PCLMUL -mpclmul -DHAS_SSE42 -msse4.2 -DADLER32_SIMD_SSSE3 -mssse3 -DINFLATE_CHUNK_SIMD_SSE2 -msse2 -DINFLATE_CHUNK_READ_64LE -O3  -DHAVE_HIDDEN
gcc -O3  -c -o crc32.o crc32.c -DHAS_PCLMUL -mpclmul -DHAS_SSE42 -msse4.2 -DADLER32_SIMD_SSSE3 -mssse3 -DINFLATE_CHUNK_SIMD_SSE2 -msse2 -DINFLATE_CHUNK_READ_64LE -O3  -DHAVE_HIDDEN
gcc -O3  -c -o deflate.o deflate.c  -msse4  -DHAS_PCLMUL -mpclmul -DHAS_SSE42 -msse4.2 -DADLER32_SIMD_SSSE3 -mssse3 -DINFLATE_CHUNK_SIMD_SSE2 -msse2 -DINFLATE_CHUNK_READ_64LE -O3  -DHAVE_HIDDEN
gcc -O3  -c -o inffast.o inffast.c -DHAS_PCLMUL -mpclmul -DHAS_SSE42 -msse4.2 -DADLER32_SIMD_SSSE3 -mssse3 -DINFLATE_CHUNK_SIMD_SSE2 -msse2 -DINFLATE_CHUNK_READ_64LE -O3  -DHAVE_HIDDEN
gcc -O3  -c -o inflate.o inflate.c -DHAS_PCLMUL -mpclmul -DHAS_SSE42 -msse4.2 -DADLER32_SIMD_SSSE3 -mssse3 -DINFLATE_CHUNK_SIMD_SSE2 -msse2 -DINFLATE_CHUNK_READ_64LE -O3  -DHAVE_HIDDEN
gcc -O3  -c -o inftrees.o inftrees.c -DHAS_PCLMUL -mpclmul -DHAS_SSE42 -msse4.2 -DADLER32_SIMD_SSSE3 -mssse3 -DINFLATE_CHUNK_SIMD_SSE2 -msse2 -DINFLATE_CHUNK_READ_64LE -O3  -DHAVE_HIDDEN
gcc -O3  -c -o trees.o trees.c -DHAS_PCLMUL -mpclmul -DHAS_SSE42 -msse4.2 -DADLER32_SIMD_SSSE3 -mssse3 -DINFLATE_CHUNK_SIMD_SSE2 -msse2 -DINFLATE_CHUNK_READ_64LE -O3  -DHAVE_HIDDEN
gcc -O3  -c -o zutil.o zutil.c -DHAS_PCLMUL -mpclmul -DHAS_SSE42 -msse4.2 -DADLER32_SIMD_SSSE3 -mssse3 -DINFLATE_CHUNK_SIMD_SSE2 -msse2 -DINFLATE_CHUNK_READ_64LE -O3  -DHAVE_HIDDEN
gcc -O3  -c -o adler32_simd.o adler32_simd.c -DHAS_PCLMUL -mpclmul -DHAS_SSE42 -msse4.2 -DADLER32_SIMD_SSSE3 -mssse3 -DINFLATE_CHUNK_SIMD_SSE2 -msse2 -DINFLATE_CHUNK_READ_64LE -O3  -DHAVE_HIDDEN
gcc -O3  -c -o crc32_simd.o crc32_simd.c -DHAS_PCLMUL -mpclmul -DHAS_SSE42 -msse4.2 -DADLER32_SIMD_SSSE3 -mssse3 -DINFLATE_CHUNK_SIMD_SSE2 -msse2 -DINFLATE_CHUNK_READ_64LE -O3  -DHAVE_HIDDEN
gcc -O3  -c -o inffast_chunk.o inffast_chunk.c -DHAS_PCLMUL -mpclmul -DHAS_SSE42 -msse4.2 -DADLER32_SIMD_SSSE3 -mssse3 -DINFLATE_CHUNK_SIMD_SSE2 -msse2 -DINFLATE_CHUNK_READ_64LE -O3  -DHAVE_HIDDEN
