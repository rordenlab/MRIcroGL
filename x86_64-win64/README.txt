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
