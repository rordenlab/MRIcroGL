unit sse_testu;

{$mode delphi}
{$H+}
(*
BSD-2-Clause License
https://opensource.org/licenses/BSD-2-Clause

Copyright (c) 2020  J. Gareth "Kit" Moreton and Chris Rorden. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of the copyright owner nor the name of this project
   (MRIcroGL) may be used to endorse or promote products derived from this
   software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT OWNER ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO
EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

interface

uses
  Classes, SysUtils;

type
    TUInt8s = array of uint8;
    TInt16s = array of int16;
    TFloat32s = array of single;

{$IFDEF CPUX86_64}
	{$DEFINE USE_SSE}
{$ENDIF}
procedure flt2byte(flts: TFloat32s; byts: TUInt8s; lMin, lMax: single; skipVx: int64);
procedure int2byte(lMin, lMax: single; ints: TInt16s; byts: TUInt8s; skipVx: int64; SSE: boolean = true);

implementation



{$IFDEF USE_SSE}
{$ASMMODE INTEL}

procedure int2byteSSE(mn, mx: single; ints: pointer; byts: pointer; skipVx, nVx: int64); assembler; nostackframe;
const
  Single_255: Single = 255.0;
  Negate: LongWord = $80008000;
  Single_Negate: Single = 32768.0;
asm
//volatile registers XMM0..XMM5, rax,rcx,rdx,r8,r9,r10,r11 
//Unix rdi,rsi,rdx,r8,r9,r10,r11
//Windows rcx,rdx,r8,r9
{unix, win
   xmm0, xmm0          = mn
   xmm1, xmm1          = mx
   rdi , r8            = pointer to ints
   rsi , r9            = pointer to byts
   rdx , [rsp+28]->rdx = skipvx
   rcx,	 [rsp+30]->rcx = nvx     
   xmm0 = min
   xmm1 = initially max, later scale (255.0 / (mx-mn))
   xmm2 = zeros
   xmm3 = negate
   xmm4 = temp0
   xmm5 = temp1 
}
 {$ifdef win64}
  mov rdx, [rsp+$28]; //skipvx
  mov rcx, [rsp+$30]; //nvx
 {$else}
 mov r8, rdi
 mov r9, rsi
 {$endif}
  test    r9,  r9 //exit if output array is null (empty)
  jz      @exit
  shl     rdx,  1 //multiply skipvx by 2 (the size of a int16)
  add     r8,  rdx //skip ahead by skipvx bytes on the input pointer
  pxor xmm4, xmm4 //all int zeros
  movss   xmm4, [rip + single_255] //255
  subss   xmm1, xmm0  // xmm1 now contains mx-mn = range 
  divss   xmm4, xmm1  //compute scale factor
  movdqa xmm1, xmm4 // xmm1, now contains the scale factor
  //
  pxor xmm3, xmm3 //all int zeros
  movss   xmm3, [rip + single_negate]
  addps xmm0, xmm3
  pxor xmm2, xmm2 //zeros
  pxor xmm3, xmm3 //all int zeros
  movss   xmm3, [rip + negate]
  //broadcast
  shufps  xmm0, xmm0, $00 //mn (bias)
  shufps  xmm1, xmm1, $00 //scale
  shufps  xmm3, xmm3, $00 //negate  
  //loop 
  @processloop:
  prefetchnta [r8+64] //cache input
  prefetcht0 [r9+32] //cache output 8*uint8     
  //load 8*int16
  movdqa xmm4, [r8] //move 8*int16 to xmm4
  paddw xmm4, xmm3 //add 0x8000
  movdqa xmm5, xmm4
  punpcklwd xmm4, xmm2 //xmm3 = zeros (same representation for floats and ints)
  cvtdq2ps xmm4, xmm4
  punpckhwd xmm5, xmm2 //xmm3 = zeros
  cvtdq2ps xmm5, xmm5
  //transform 4*float32
  subps   xmm4, xmm0 //subtract mn (bias)
  mulps   xmm4, xmm1 //multiply by scaling factor
  //transform 4*float32
  subps   xmm5, xmm0 //subtract mn (bias)
  mulps   xmm5, xmm1 //multiply by scaling factor
  //convert 8*float32->8*uint8 
  cvtps2dq xmm4, xmm4 //float32->int32
  cvtps2dq xmm5, xmm5 //float32->int32
  packssdw xmm4, xmm5 //int32->int16 mm_packs_epi32
  packuswb xmm4, xmm4 //int16->uint8 mm_packus_epi16 packed signed 16-bit integers from a and b to packed 8-bit integers 
  movq [r9],xmm4 //store
  add r9, 8  //r9= output 8*uint8
  add r8, 16  //r8 = input 8*uint16
  sub rcx, 8  //rcx= number of elements
  ja      @processloop
@exit:
end;

 {$IFNDEF WIN64}
procedure flt2byte(flts: TFloat32s; byts: TUInt8s; lMin, lMax: single; skipVx: int64); assembler; nostackframe;
const
  Single_255: Single = 255.0;
asm
  { RDI         = pointer to flts
    RSI         = pointer to byts
    XMM0        = iMin
    XMM1        = iMax
    RDX         = skipVX
  }
  PUSH    RBP
  MOV     RBP,  RSP
  LEA     RSP,  [RSP - $18]
  AND     RSP,  $FFFFFFFFFFFFFFF0 { Ensure stack is aligned to a 16-byte boundary }
  MOVAPS  [RSP], XMM6

  STMXCSR [RSP + $10]
  XOR     R10D, R10D
  MOV     EAX,  [RSP + $10]
  TEST    EAX,  $6000
  JZ      @SkipStateSet   { Skip over setting the state if possible, as it's pretty expensive }
  MOV     R10D, EAX       { Preserve previous value of MXCSR register }
  AND     EAX,  $FFFF9FFF { Zero the $2000 and $4000 bits to set rounding mode to 'nearest' }
  MOV     [RSP + $10], EAX
  LDMXCSR [RSP + $10]
@SkipStateSet:

  { Create the scale factor (255 / (lMax - lMin)) and store in XMM2, interspersed
    with other initialisation to take advantage of the pipeline }
  MOVSS   XMM2, [RIP + Single_255]
  TEST    RSI,  RSI         { Test to see if the array is actually empty or not }
  JZ      @Exit
  MOVSS   XMM3, XMM1        { XMM3 now contains lMax }
  MOV     R8,   [RSI - $8]  { Length of byts - 1 }
  SUBSS   XMM1, XMM0
  SHL     RDX,  2           { Multiply skipVx by 4 (the size of a Single) }
  ADD     R8,   1
  DIVSS   XMM2, XMM1        { XMM2, Now contains the scale factor }
  ADD     RDI,  RDX         { Skip ahead by skipVx bytes on the input pointer }

  { Broadcast the scale, lMin and lMax into the rest of the registers }
  SHUFPS  XMM2, XMM2, $00
  SHUFPS  XMM3, XMM3, $00
  SHUFPS  XMM0, XMM0, $00

  PREFETCHT0    [RSI]

  { Test the alignment of the input buffer }
  TEST    RDI,  $4
  JZ      @SkipLeadUnit

  { Deal with float that isn't aligned }
  MOVSS   XMM1, [RDI]
  MINSS   XMM1, XMM3 { max cap to lMax }
  MAXSS   XMM1, XMM0 { min cap to lMin }
  SUBSS   XMM1, XMM0 { subtract lMin }
  MULSS   XMM1, XMM2 { multiply by scaling factor }
  LEA     RSI,  [RSI + $1]
  ADD     RDI,  $4
  CVTSS2SI EAX, XMM1 { Convert to byte }
  SUB     R8,   1
  MOV     [RSI - $1], AL

@SkipLeadUnit:
  TEST    RDI,  $8
  JZ      @SkipLeadPair

  { Deal with 2 floats that are not aligned }
  MOVSS   XMM1, [RDI]
  MOVSS   XMM4, [RDI + $4]

  MINSS   XMM1, XMM3 { max cap to lMax }
  MINSS   XMM4, XMM3

  MAXSS   XMM1, XMM0 { min cap to lMin }
  MAXSS   XMM4, XMM0

  SUBSS   XMM1, XMM0 { subtract lMin }
  SUBSS   XMM4, XMM0

  MULSS   XMM1, XMM2 { multiply by scaling factor }
  MULSS   XMM4, XMM2

  CVTSS2SI EAX, XMM1 { Convert results into DWords }
  CVTSS2SI R11D,XMM4

  LEA     RSI,  [RSI + $2]
  SUB     R8,   2
  SHL     R11D, 8
  ADD     RDI,  $8
  OR      EAX,  R11D { Merge the least-significant bytes of the results into a single Word }

  MOV     [RSI - $2], AX

@SkipLeadPair:
  MOV     RCX,  R8
  SHR     R8,   4    { Divide by 16, as the loop handles blocks of 16 }
  JZ      @BlockLoopEnd

  XOR     RDX,  RDX
  AND     RCX,  $F
  TEST    RSI,  $F
  JNZ     @BlockLoopUnaligned

  ALIGN   16
@BlockLoop:
  MOVAPS  XMM1, [RDI + RDX*4]
  MOVAPS  XMM4, [RDI + RDX*4 + $10]
  MOVAPS  XMM5, [RDI + RDX*4 + $20]
  MOVAPS  XMM6, [RDI + RDX*4 + $30]

  SUBPS   XMM1, XMM0 { subtract lMin }
  SUBPS   XMM4, XMM0

  PREFETCHNTA   [RDI + RDX*4 + $100]

  MULPS   XMM1, XMM2 { multiply by scaling factor }
  MULPS   XMM4, XMM2

  { Use AGU to keep ALUs free }
  LEA     RDX,  [RDX + $10]

  { Take advantage of the free ALUs while multiplications are in progress, hence
    the interspersing of instructions }
  SUBPS   XMM5, XMM0 { subtract lMin }
  SUBPS   XMM6, XMM0

  MULPS   XMM5, XMM2 { multiply by scaling factor }

  CVTPS2DQ XMM1, XMM1 { Convert results into DWords }
  CVTPS2DQ XMM4, XMM4
  PACKSSDW XMM1, XMM4

  MULPS   XMM6, XMM2

  CVTPS2DQ XMM5, XMM5 { Convert results into DWords }
  CVTPS2DQ XMM6, XMM6

  SUB     R8,   1

  PACKSSDW XMM5, XMM6

  PACKUSWB XMM1, XMM5

  MOVNTDQ [RSI + RDX - $10],  XMM1
  JA      @BlockLoop
  JMP     @BlockLoopEnd

  ALIGN   16
@BlockLoopUnaligned:
  MOVAPS  XMM1, [RDI + RDX*4]
  MOVAPS  XMM4, [RDI + RDX*4 + $10]
  MOVAPS  XMM5, [RDI + RDX*4 + $20]
  MOVAPS  XMM6, [RDI + RDX*4 + $30]

  SUBPS   XMM1, XMM0 { subtract lMin }
  SUBPS   XMM4, XMM0

  PREFETCHNTA   [RDI + RDX*4 + $100]
  PREFETCHW     [RSI + RDX + $40]

  MULPS   XMM1, XMM2 { multiply by scaling factor }
  MULPS   XMM4, XMM2

  { Use AGU to keep ALUs free }
  LEA     RDX,  [RDX + $10]

  { Take advantage of the free ALUs while multiplications are in progress, hence
    the interspersing of instructions }
  SUBPS   XMM5, XMM0 { subtract lMin }
  SUBPS   XMM6, XMM0

  MULPS   XMM5, XMM2 { multiply by scaling factor }

  CVTPS2DQ XMM1, XMM1 { Convert results into DWords }
  CVTPS2DQ XMM4, XMM4
  PACKSSDW XMM1, XMM4

  MULPS   XMM6, XMM2

  CVTPS2DQ XMM5, XMM5 { Convert results into DWords }
  CVTPS2DQ XMM6, XMM6

  SUB     R8,   1

  PACKSSDW XMM5, XMM6

  PACKUSWB XMM1, XMM5

  MOVDQU  [RSI + RDX - $10],  XMM1
  JA      @BlockLoopUnaligned

@BlockLoopEnd:

  { Check for leftovers }

  { Though RCX contains the original count minus the lead units, all blocks of
    16 have been handled by now, so it can be treated as of it's equal to
    RCX and $F in the context of all the TEST comparisons below }

  { Check if there are 8 or more first, since we can use half of the above
    block loop for that }

  TEST    CL,   $8
  JZ      @SkipOctLeftover

  MOVAPS  XMM1, [RDI + RDX*4]
  MOVAPS  XMM4, [RDI + RDX*4 + $10]

  SUBPS   XMM1, XMM0 { subtract lMin }
  SUBPS   XMM4, XMM0

  MULPS   XMM1, XMM2 { multiply by scaling factor }
  MULPS   XMM4, XMM2

  LEA     RDX,  [RDX + 8]

  CVTPS2DQ XMM1, XMM1 { Convert results into DWords }
  CVTPS2DQ XMM4, XMM4

  PACKSSDW XMM1, XMM4
  PACKUSWB XMM1, XMM1

  MOVQ     [RSI + RDX - 8],  XMM1

@SkipOctLeftover:
  { Check if there are 4 or more next, since we can do this with an aligned move }
  TEST    CL,   $4
  JZ      @SkipQuadLeftover

  MOVAPS  XMM1, [RDI + RDX*4]

  SUBPS   XMM1, XMM0 { subtract lMin }
  MULPS   XMM1, XMM2 { multiply by scaling factor }

  ADD     RDX,  4

  CVTPS2DQ XMM1, XMM1 { Convert results into DWords }
  PACKSSDW XMM1, XMM1
  PACKUSWB XMM1, XMM1

  MOVD    [RSI + RDX - 4],  XMM1

@SkipQuadLeftover:
  TEST    CL,   $2
  JZ      @SkipPairLeftover

  { Handle two floats }
  MOVSS   XMM1, [RDI + RDX*4]
  MOVSS   XMM4, [RDI + RDX*4 + $4]

  MINSS   XMM1, XMM3 { max cap to lMax }
  MINSS   XMM4, XMM3

  MAXSS   XMM1, XMM0 { min cap to lMin }
  MAXSS   XMM4, XMM0

  SUBSS   XMM1, XMM0 { subtract lMin }
  SUBSS   XMM4, XMM0

  MULSS   XMM1, XMM2 { multiply by scaling factor }
  MULSS   XMM4, XMM2

  CVTSS2SI EAX, XMM1 { Convert results into DWords }
  CVTSS2SI R9D, XMM4

  SHL     R9D,  8
  ADD     RDX,  2
  OR      EAX,  R9D { Merge the least-significant bytes of the results into a single Word }

  MOV     [RSI + RDX - $2], AX

@SkipPairLeftover:
  TEST    CL,   $1
  JZ      @SkipUnitLeftover

  { Handle lone float }
  MOVSS   XMM1, [RDI + RDX*4]
  MINSS   XMM1, XMM3 { max cap to lMax }
  MAXSS   XMM1, XMM0 { min cap to lMin }
  SUBSS   XMM1, XMM0 { subtract lMin }
  MULSS   XMM1, XMM2 { multiply by scaling factor }
  CVTSS2SI EAX, XMM1 { Convert to byte }
  MOV     [RSI + RDX], AL

@SkipUnitLeftover:

@Exit:
  { This works because if R10D was never set to equal the MXCSR register (and
    hence is just zero), then the bits we wanted to change were already at the
    values we wanted (both zero), so MXCSR wasn't modified.  Similarly, if the
    MXCSR register was equal to all zeroes, then we didn't need to change anything }
  TEST    R10D, R10D
  JZ      @SkipStateRestore
  MOV     [RSP + $10], R10D
  LDMXCSR [RSP + $10] { Restore previous value of MXCSR register }
@SkipStateRestore:

  MOVAPS  XMM6, [RSP]
  MOV     RSP,  RBP
  POP     RBP
end;
{$ELSE}
procedure flt2byte(flts: TFloat32s; byts: TUInt8s; lMin, lMax: single; skipVx: int64); assembler; nostackframe;
const
  Single_255: Single = 255.0;
asm
  { RCX  = pointer to flts
    RDX  = pointer to byts
    XMM2 = iMin
    XMM3 = iMax
    [RSP + $40] = skipVX (after stack adjustment)
  }
  LEA     RSP,  [RSP - $18] { -$18 ensures the stack is aligned }
.seh_stackalloc $18
  MOVAPS  [RSP], XMM6
.seh_savexmm XMM6, 0
.seh_endprologue

  STMXCSR [RSP + $10]
  XOR     R10D, R10D
  MOV     EAX,  [RSP + $10]
  TEST    AX,   $6000
  JZ      @SkipStateSet   { Skip over setting the state if possible, as it's pretty expensive }
  MOV     R10D, EAX{ Preserve previous value of MXCSR register }
  AND     EAX,  $FFFF9FFF { Zero the $2000 and $4000 bits to set rounding mode to 'nearest' }
  MOV     [RSP + $10], EAX
  LDMXCSR [RSP + $10]
@SkipStateSet:

  { Create the scale factor (255 / (lMax - lMin)) and store in XMM0, interspersed
    with other initialisation to take advantage of the pipeline }
  MOVSS   XMM0, [RIP + Single_255]
  TEST    RDX,  RDX  { Test to see if the array is actually empty or not }
  JZ      @Exit
  MOVSS   XMM1, XMM3 { XMM1 now contains lMax }
  MOV     R8,   [RDX - $8]  { Length of byts - 1 }
  SUBSS   XMM3, XMM2
  MOV     RAX,  [RSP + $40] { Move skipVx into a register }
  ADD     R8,   1
  DIVSS   XMM0, XMM3 { XMM0, Now contains the scale factor }
  SHL     RAX,  2    { Multiply skipVx by 4 (the size of a Single) }
  ADD     RCX,  RAX  { Skip ahead by skipVx bytes on the input pointer }

  { Broadcast the scale, lMin and lMax into the rest of the registers }
  SHUFPS  XMM0, XMM0, $00
  SHUFPS  XMM1, XMM1, $00
  SHUFPS  XMM2, XMM2, $00

  PREFETCHW     [RDX]

  { Test the alignment of the input buffer (use CL instead of RCX to make the instruction smaller) }
  TEST    RCX,  $4
  JZ      @SkipLeadUnit

  { Deal with float that isn't aligned }
  MOVSS   XMM3, [RCX]
  MINSS   XMM3, XMM1 { max cap to lMax }
  MAXSS   XMM3, XMM2 { min cap to lMin }
  SUBSS   XMM3, XMM2 { subtract lMin }
  MULSS   XMM3, XMM0 { multiply by scaling factor }
  LEA     RDX,  [RDX + $1]
  ADD     RCX,  $4
  CVTSS2SI EAX, XMM3 { Convert to byte }
  SUB     R8,   1
  MOV     [RDX - $1], AL

@SkipLeadUnit:
  TEST    RCX,  $8
  JZ      @SkipLeadPair

  { Deal with 2 floats that are not aligned }
  MOVSS   XMM3, [RCX]
  MOVSS   XMM4, [RCX + $4]

  MINSS   XMM3, XMM1 { max cap to lMax }
  MINSS   XMM4, XMM1

  MAXSS   XMM3, XMM2 { min cap to lMin }
  MAXSS   XMM4, XMM2

  SUBSS   XMM3, XMM2 { subtract lMin }
  SUBSS   XMM4, XMM2

  MULSS   XMM3, XMM0 { multiply by scaling factor }
  MULSS   XMM4, XMM0

  CVTSS2SI EAX, XMM3 { Convert results into DWords }
  CVTSS2SI R11D,XMM4

  LEA     RDX,  [RDX + $2]
  SUB     R8,   2
  SHL     R11D, 8
  ADD     RCX,  $8
  OR      EAX,  R11D { Merge the least-significant bytes of the results into a single Word }

  MOV     [RDX - $2], AX

@SkipLeadPair:
  MOV     R9,   R8
  SHR     R8,   4    { Divide by 16, as the loop handles blocks of 16 }
  JZ      @BlockLoopEnd

  XOR     EAX,  EAX
  AND     R9,   $F   { To allow smaller and faster testing of R9B later }
  TEST    RDX,  $F   { Test alignment of destination buffer (use DL instead of RDX to shrink the instruction }
  JNZ     @BlockLoopUnaligned

  ALIGN   16
@BlockLoop:
  MOVAPS  XMM3, [RCX + RAX*4]
  MOVAPS  XMM4, [RCX + RAX*4 + $10]
  MOVAPS  XMM5, [RCX + RAX*4 + $20]
  MOVAPS  XMM6, [RCX + RAX*4 + $30]

  SUBPS   XMM3, XMM2 { subtract lMin }
  SUBPS   XMM4, XMM2

  PREFETCHNTA   [RCX + RAX*4 + $100]

  MULPS   XMM3, XMM0 { multiply by scaling factor }
  MULPS   XMM4, XMM0

  { Use AGU to keep ALUs free }
  LEA     RAX,  [RAX + $10]

  { Take advantage of the free ALU while multiplications are in progress }
  SUBPS   XMM5, XMM2 { subtract lMin }
  SUBPS   XMM6, XMM2

  MULPS   XMM5, XMM0 { multiply by scaling factor }

  CVTPS2DQ XMM3, XMM3 { Convert results into DWords }
  CVTPS2DQ XMM4, XMM4

  MULPS   XMM6, XMM0

  PACKSSDW XMM3, XMM4

  CVTPS2DQ XMM5, XMM5 { Convert results into DWords }
  CVTPS2DQ XMM6, XMM6

  SUB     R8,   1

  PACKSSDW XMM5, XMM6

  PACKUSWB XMM3, XMM5

  MOVNTDQ [RDX + RAX - $10], XMM3
  JA      @BlockLoop
  JMP     @BlockLoopEnd

  ALIGN   16
@BlockLoopUnaligned:
  MOVAPS  XMM3, [RCX + RAX*4]
  MOVAPS  XMM4, [RCX + RAX*4 + $10]
  MOVAPS  XMM5, [RCX + RAX*4 + $20]
  MOVAPS  XMM6, [RCX + RAX*4 + $30]

  SUBPS   XMM3, XMM2 { subtract lMin }
  SUBPS   XMM4, XMM2

  PREFETCHNTA   [RCX + RAX*4 + $100]
  PREFETCHW     [RDX + RAX + $40]

  MULPS   XMM3, XMM0 { multiply by scaling factor }
  MULPS   XMM4, XMM0

  { Use AGU to keep ALUs free }
  LEA     RAX,  [RAX + $10]

  { Take advantage of the free ALU while multiplications are in progress }
  SUBPS   XMM5, XMM2 { subtract lMin }
  SUBPS   XMM6, XMM2

  MULPS   XMM5, XMM0 { multiply by scaling factor }

  CVTPS2DQ XMM3, XMM3 { Convert results into DWords }
  CVTPS2DQ XMM4, XMM4

  MULPS   XMM6, XMM0

  PACKSSDW XMM3, XMM4

  CVTPS2DQ XMM5, XMM5 { Convert results into DWords }
  CVTPS2DQ XMM6, XMM6

  SUB     R8,   1

  PACKSSDW XMM5, XMM6

  PACKUSWB XMM3, XMM5

  MOVDQU  [RDX + RAX - $10], XMM3
  JA      @BlockLoopUnaligned

@BlockLoopEnd:

  { Check for leftovers }

  { Though R9 contains the original count minus the lead units, all blocks of
    16 have been handled by now, so it can be treated as if it's equal to
    R9 and $F in the context of all the TEST comparisons below }

  { Check if there are 8 or more first, since we can use half of the above
    block loop for that }
  TEST    R9B,  $8
  JZ      @SkipOctLeftover

  MOVAPS  XMM3, [RCX + RAX*4]
  MOVAPS  XMM4, [RCX + RAX*4 + $10]

  SUBPS   XMM3, XMM2 { subtract lMin }
  SUBPS   XMM4, XMM2

  MULPS   XMM3, XMM0 { multiply by scaling factor }
  MULPS   XMM4, XMM0

  ADD     RAX,  8

  CVTPS2DQ XMM3, XMM3 { Convert results into DWords }
  CVTPS2DQ XMM4, XMM4

  PACKSSDW XMM3, XMM4
  PACKUSWB XMM3, XMM3

  MOVQ    [RDX + RAX - 8],  XMM3

@SkipOctLeftover:
  { Check if there are 4 or more next, since we can do this with an aligned move }
  TEST    R9B,  $4
  JZ      @SkipQuadLeftover

  MOVAPS  XMM3, [RCX + RAX*4]

  SUBPS   XMM3, XMM2 { subtract lMin }
  MULPS   XMM3, XMM0 { multiply by scaling factor }

  ADD     RAX,  4

  CVTPS2DQ XMM3, XMM3 { Convert results into DWords and pack down into bytes }
  PACKSSDW XMM3, XMM3
  PACKUSWB XMM3, XMM3

  MOVD    [RDX + RAX - 4],  XMM3

@SkipQuadLeftover:
  TEST    R9B,  $2
  JZ      @SkipPairLeftover

  { Handle two floats }
  MOVSS   XMM3, [RCX + RAX*4]
  MOVSS   XMM4, [RCX + RAX*4 + $4]

  MINSS   XMM3, XMM1 { max cap to lMax }
  MINSS   XMM4, XMM1

  MAXSS   XMM3, XMM2 { min cap to lMin }
  MAXSS   XMM4, XMM2

  SUBSS   XMM3, XMM2 { subtract lMin }
  SUBSS   XMM4, XMM2

  MULSS   XMM3, XMM0 { multiply by scaling factor }
  MULSS   XMM4, XMM0

  CVTSS2SI R8D, XMM3 { Convert results into DWords }
  CVTSS2SI R11D,XMM4

  SHL     R11D, 8
  ADD     RAX,  $2
  OR      R8D,  R11D { Merge the least-significant bytes of the results into a single Word }

  MOV     [RDX + RAX - $2], R8W

@SkipPairLeftover:
  TEST    R9B,  $1
  JZ      @SkipUnitLeftover

  { Handle lone float }
  MOVSS   XMM3, [RCX + RAX*4]
  MINSS   XMM3, XMM1 { max cap to lMax }
  MAXSS   XMM3, XMM2 { min cap to lMin }
  SUBSS   XMM3, XMM2 { subtract lMin }
  MULSS   XMM3, XMM0 { multiply by scaling factor }
  CVTSS2SI R8D, XMM3 { Convert to byte }
  MOV     [RDX + RAX], R8B

@SkipUnitLeftover:

@Exit:
  { This works because if R10D was never set to equal the MXCSR register (and
    hence is just zero), then the bits we wanted to change were already at the
    values we wanted (both zero), so MXCSR wasn't modified.  Similarly, if the
    MXCSR register was equal to all zeroes, then we didn't need to change anything }
  TEST    R10D, R10D
  JZ      @SkipStateRestore
  MOV     [RSP + $10], R10D
  LDMXCSR [RSP + $10] { Restore previous value of MXCSR register }
@SkipStateRestore:
  MOVAPS  XMM6, [RSP]
  LEA     RSP, [RSP + $18]
end;
{$ENDIF WIN64}
 {$ENDIF} //SSE

Type
  ByteRA0 = array [0..MAXINT] of byte;
  Bytep0 = ^ByteRA0;
  IntRA0 = array [0..MAXINT] of int16;
  Intp0 = ^IntRA0;
  FltRA0 = array [0..MAXINT] of single;
  Fltp0 = ^FltRA0;


procedure int2byteSISD(lMin, lMax: single; ints: Intp0; byts: Bytep0; skipVx, nVx: int64);
var
   i: int64;
   slope: single;
begin
  slope := abs(lMax - lMin);
  if (slope > 0) then
    slope := 255.0/slope;
  for i := 0 to (nVx - 1) do begin
    if (ints[skipVx+i] >= lMax) then
       byts[i] := 255
    else if  (ints[skipVx+i] <= lMin) then
        byts[i] := 0
    else
       byts[i] := round((ints[skipVx+i] - lMin) * slope);
  end;
end;

procedure int2byte(lMin, lMax: single; ints: TInt16s; byts: TUInt8s; skipVx: int64; SSE: boolean = true);
//SSE-vs-Scalar choice
{$IFDEF USE_SSE}
const
  kAlign = 16; //SSE expects values aligned to 16-byte boundaries
  kInBytes = 2;
  kVectorSize = 8; //SSE will retire 8 values at a time 
var
   inAlign: PtrUint;
   head: int64 = 0;
   tail: int64 = 0;
   nVox: int64;
{$ENDIF}
begin
	{$IFDEF USE_SSE}
	nVox := length(byts);
	//check output alignment (required)
	inAlign := PtrUint(@byts[0]) mod kAlign;
	if (lMin >= lMax) or (inAlign <> 0) or (nVox < (kVectorSize * 3)) then
		SSE := false;
	if (SSE) then begin //check input alignment, scalar required items to align 
		inAlign := PtrUint(@ints[skipVx]) mod kAlign;
		if (inAlign <> 0) and ((inAlign mod kInBytes) <> 0) then
			SSE := false
		else if (inAlign <> 0) then begin
			head := (kAlign - inAlign) div kInBytes;
			int2byteSISD(lMin,lMax, @ints[0],@byts[0], skipVx, head);
			skipVx += head;
			nVox -= head;
		end;
	end;
	if (SSE) and ((nVox mod kVectorSize) <> 0) then begin
		tail := (nVox mod kVectorSize);
		//writeln('>>>', nVox, 'x', tail, ':',ints[skipVx+nVox-tail]);
		int2byteSISD(lMin,lMax, @ints[0],@byts[nVox-tail+head], skipVx+nVox-tail, tail);
		//byts[nVox-tail] := 127;
		nVox -= tail;
	end;
	if SSE then
		int2byteSSE(lMin,lMax,@ints[0],@byts[head],  skipVx, nVox)
	else
	{$ENDIF}
		int2byteSISD(lMin,lMax, @ints[0],@byts[0], skipVx, length(byts));	
end;

{$IFNDEF USE_SSE}
procedure flt2byte(flts: TFloat32s; byts: TUInt8s; lMin, lMax: single; skipVx: int64);
var
   i, nVx: int64;
   slope: single;
begin
  slope := abs(lMax - lMin);
  if (slope > 0) then
  	slope := 255.0/slope;
  nVx :=  length(byts);
  for i := 0 to (nVx - 1) do begin
    if (flts[skipVx+i] >= lMax) then
       byts[i] := 255
    else if  (flts[skipVx+i] <= lMin) then
        byts[i] := 0
    else
       byts[i] := round((flts[skipVx+i] - lMin) * slope);
  end;
end;
{$ENDIF}

end.
