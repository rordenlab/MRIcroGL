unit sse2;

{$mode delphi}
{$H+}
{$WARN 7105 off : Use of -offset(%esp), access may cause a crash or value may be lost}

interface

uses
  //math, //TODO
  Classes, SysUtils, SimdUtils;

(*type
    TUInt8s = array of uint8;
    TFloat32s = array of single;*)

//procedure float2byte2(flts: TFloat32s; byts: TUInt8s; lMin, lMax: single; SSE: boolean = true);
procedure float2byte(flts: TFloat32s; byts: TUInt8s; lMin, lMax: single; skipVx: integer; SSE: boolean = true);

implementation

{$IFDEF CPUX86_64}
{$ASMMODE INTEL}
{$IFDEF UNIX}
 procedure float2byteKitSSE(flts: TFloat32s; byts: TUInt8s; lMin, lMax: single; skipVx: int64); assembler; nostackframe;
const
  Single_255: Single = 255.0;
  Single_0_5: Single = 0.5;
asm
  { RDI         = pointer to flts
    RSI         = pointer to byts
    XMM0        = iMin
    XMM1        = iMax
    RDX         = skipVX
  }

  LEA     RSP,  [RSP - $8]
  STMXCSR [RSP]
  XOR     R10D, R10D
  MOV     EAX,  [RSP]
  TEST    EAX,  $6000
  JZ      @SkipStateSet   { Skip over setting the state if possible, as it's pretty expensive }
  MOV     R10D, EAX       { Preserve previous value of MXCSR register }
  AND     EAX,  $FFFF9FFF { Zero the $2000 and $4000 bits to set rounding mode to 'nearest' }
  MOV     [RSP], EAX
  LDMXCSR [RSP]
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
  MOV     RCX,   R8
  AND     R8,   $7   { Keep track of any leftover floats for later }
  XOR     EAX,  EAX
  SHR     RCX,   3    { Divide by 8, as the loop handles blocks of 8 }
  JZ      @BlockLoopEnd

@BlockLoop:
  PREFETCHNTA   [RDI + RDX*4 + $80]
  PREFETCHT0    [RSI + RDX + $20]

  MOVAPS  XMM1, [RDI + RDX*4]
  MOVAPS  XMM4, [RDI + RDX*4 + $10]

  MINPS   XMM1, XMM3 { max cap to lMax }
  MINPS   XMM4, XMM3

  MAXPS   XMM1, XMM0 { min cap to lMin }
  MAXPS   XMM4, XMM0

  SUBPS   XMM1, XMM0 { subtract lMin }
  SUBPS   XMM4, XMM0

  MULPS   XMM1, XMM2 { multiply by scaling factor }
  MULPS   XMM4, XMM2

  CVTPS2PI MM0, XMM1 { Convert results into DWords }
  CVTPS2PI MM2, XMM4

  MOVHLPS XMM1, XMM1
  MOVHLPS XMM4, XMM4

  CVTPS2PI MM1, XMM1 { Convert results into DWords }
  CVTPS2PI MM3, XMM4

  PACKSSDW MM0, MM1
  PACKSSDW MM2, MM3

  PACKUSWB MM0, MM2

  ADD     RDX,  8
  SUB     RCX,  1

  MOVQ    [RSI + RDX - 8],  MM0
  JA      @BlockLoop

@BlockLoopEnd:

  { Check for leftovers }

  { Check if there are 4 or more first, since we can do this with an aligned move }
  TEST    R8,   $4
  JZ      @SkipQuadLeftover

  MOVAPS  XMM1, [RDI + RDX*4]

  MINPS   XMM1, XMM3 { max cap to lMax }
  MAXPS   XMM1, XMM0 { min cap to lMin }
  SUBPS   XMM1, XMM0 { subtract lMin }
  MULPS   XMM1, XMM2 { multiply by scaling factor }
  CVTPS2PI MM0, XMM1 { Convert results into DWords }
  MOVHLPS XMM1, XMM1
  CVTPS2PI MM1, XMM1
  PACKSSDW MM0, MM1
  PACKUSWB MM0, MM0

  ADD     RDX,  4

  MOVD    [RSI + RDX - 4],  MM0

@SkipQuadLeftover:
  TEST    R8,   $2
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

  CVTSS2SI ECX, XMM1 { Convert results into DWords }
  CVTSS2SI R9D, XMM4

  SHL     R9D,  8
  ADD     RDX,  2
  OR      ECX,  R9D { Merge the least-significant bytes of the results into a single Word }

  MOV     [RSI + RDX - $2], CX

@SkipPairLeftover:
  TEST    R8,   $1
  JZ      @SkipUnitLeftover

  { Handle lone float }
  MOVSS   XMM1, [RDI + RDX*4]
  MINSS   XMM1, XMM3 { max cap to lMax }
  MAXSS   XMM1, XMM0 { min cap to lMin }
  SUBSS   XMM1, XMM0 { subtract lMin }
  MULSS   XMM1, XMM2 { multiply by scaling factor }
  CVTSS2SI ECX, XMM1 { Convert to byte }
  MOV     [RSI + RDX], CL

@SkipUnitLeftover:

@Exit:
  { This works because if R10D was never set to equal the MXCSR register (and
    hence is just zero), then the bits we wanted to change were already at the
    values we wanted (both zero), so MXCSR wasn't modified.  Similarly, if the
    MXCSR register was equal to all zeroes, then we didn't need to change anything }
  TEST    R10D, R10D
  JZ      @SkipStateRestore
  MOV     [RSP], R10D
  LDMXCSR [RSP] { Restore previous value of MXCSR register }
@SkipStateRestore:
  LEA     RSP,  [RSP + $8]
  EMMS
end;
{$ELSE}
   procedure float2byteKitSSE(flts: TFloat32s; byts: TUInt8s; lMin, lMax: single; skipVx: int64); assembler; nostackframe;
   const
     Single_255: Single = 255.0;
     Single_0_5: Single = 0.5;
   asm
     { RCX         = pointer to flts
       RDX         = pointer to byts
       XMM2        = iMin
       XMM3        = iMax
       [RSP + $28] = skipVX
     }

     STMXCSR [RSP + $8] { RSP + $8 is in the shadow space and is otherwise available for use }
     XOR     R10D, R10D
     MOV     EAX,  [RSP + $8]
     TEST    EAX,  $6000
     JZ      @SkipStateSet   { Skip over setting the state if possible, as it's pretty expensive }
     MOV     R10D, EAX       { Preserve previous value of MXCSR register }
     AND     EAX,  $FFFF9FFF { Zero the $2000 and $4000 bits to set rounding mode to 'nearest' }
     MOV     [RSP + $8], EAX
     LDMXCSR [RSP + $8]
   @SkipStateSet:

     { Create the scale factor (255 / (lMax - lMin)) and store in XMM0, interspersed
       with other initialisation to take advantage of the pipeline }
     MOVSS   XMM0, [RIP + Single_255]
     TEST    RDX,  RDX         { Test to see if the array is actually empty or not }
     JZ      @Exit
     MOVSS   XMM1, XMM3        { XMM1 now contains lMax }
     MOV     R8,   [RDX - $8]  { Length of byts - 1 }
     SUBSS   XMM3, XMM2
     MOV     RAX,  [RSP + $28] { Move skipVx into a register }
     ADD     R8,   1
     DIVSS   XMM0, XMM3        { XMM0, Now contains the scale factor }
     SHL     RAX,  2           { Multiply skipVx by 4 (the size of a Single) }
     ADD     RCX,  RAX         { Skip ahead by skipVx bytes on the input pointer }

     { Broadcast the scale, lMin and lMax into the rest of the registers }
     SHUFPS  XMM0, XMM0, $00
     SHUFPS  XMM1, XMM1, $00
     SHUFPS  XMM2, XMM2, $00

     PREFETCHT0    [RDX]

     { Test the alignment of the input buffer }
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
     AND     R8,   $7   { Keep track of any leftover floats for later }
     XOR     EAX,  EAX
     SHR     R9,   3    { Divide by 8, as the loop handles blocks of 8 }
     JZ      @BlockLoopEnd

   @BlockLoop:
     PREFETCHNTA   [RCX + RAX*4 + $80]
     PREFETCHT0    [RDX + RAX + $20]

     MOVAPS  XMM3, [RCX + RAX*4]
     MOVAPS  XMM4, [RCX + RAX*4 + $10]

     MINPS   XMM3, XMM1 { max cap to lMax }
     MINPS   XMM4, XMM1

     MAXPS   XMM3, XMM2 { min cap to lMin }
     MAXPS   XMM4, XMM2

     SUBPS   XMM3, XMM2 { subtract lMin }
     SUBPS   XMM4, XMM2

     MULPS   XMM3, XMM0 { multiply by scaling factor }
     MULPS   XMM4, XMM0

     CVTPS2PI MM0, XMM3 { Convert results into DWords }
     CVTPS2PI MM2, XMM4

     MOVHLPS XMM3, XMM3
     MOVHLPS XMM4, XMM4

     CVTPS2PI MM1, XMM3 { Convert results into DWords }
     CVTPS2PI MM3, XMM4

     PACKSSDW MM0, MM1
     PACKSSDW MM2, MM3

     PACKUSWB MM0, MM2

     ADD     RAX,  8
     SUB     R9,   1

     MOVQ    [RDX + RAX - 8],  MM0
     JA      @BlockLoop

   @BlockLoopEnd:

     { Check for leftovers }

     { Check if there are 4 or more first, since we can do this with an aligned move }
     TEST    R8,   $4
     JZ      @SkipQuadLeftover

     MOVAPS  XMM3, [RCX + RAX*4]

     MINPS   XMM3, XMM1 { max cap to lMax }
     MAXPS   XMM3, XMM2 { min cap to lMin }
     SUBPS   XMM3, XMM2 { subtract lMin }
     MULPS   XMM3, XMM0 { multiply by scaling factor }
     CVTPS2PI MM0, XMM3 { Convert results into DWords }
     MOVHLPS XMM3, XMM3
     CVTPS2PI MM1, XMM3
     PACKSSDW MM0, MM1
     PACKUSWB MM0, MM0

     ADD     RAX,  4

     MOVD    [RDX + RAX - 4],  MM0

   @SkipQuadLeftover:
     TEST    R8,   $2
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

     CVTSS2SI R9D, XMM3 { Convert results into DWords }
     CVTSS2SI R11D,XMM4

     SHL     R11D, 8
     ADD     RAX,  $2
     OR      R9D,  R11D { Merge the least-significant bytes of the results into a single Word }

     MOV     [RDX + RAX - $2], R9W

   @SkipPairLeftover:
     TEST    R8,   $1
     JZ      @SkipUnitLeftover

     { Handle lone float }
     MOVSS   XMM3, [RCX + RAX*4]
     MINSS   XMM3, XMM1 { max cap to lMax }
     MAXSS   XMM3, XMM2 { min cap to lMin }
     SUBSS   XMM3, XMM2 { subtract lMin }
     MULSS   XMM3, XMM0 { multiply by scaling factor }
     CVTSS2SI R9D, XMM3 { Convert to byte }
     MOV     [RDX + RAX], R9B

   @SkipUnitLeftover:

   @Exit:
     { This works because if R10D was never set to equal the MXCSR register (and
       hence is just zero), then the bits we wanted to change were already at the
       values we wanted (both zero), so MXCSR wasn't modified.  Similarly, if the
       MXCSR register was equal to all zeroes, then we didn't need to change anything }
     TEST    R10D, R10D
     JZ      @SkipStateRestore
     MOV     [RSP + $8], R10D
     LDMXCSR [RSP + $8] { Restore previous value of MXCSR register }
   @SkipStateRestore:
     EMMS
   end;

{$ENDIF}

{$ENDIF}
procedure float2byteSISD(flts: TFloat32s; byts: TUInt8s; lMin, lMax: single; skipVx: integer);
var
   i,vx: integer;
   slope: single;
begin
  slope := 255.0/(lMax - lMin);
  vx := length(byts);
  for i := 0 to (vx - 1) do begin
    if (flts[skipVx+i] >= lMax) then
       byts[i] := 255
    else if  (flts[skipVx+i] <= lMin) then
        byts[i] := 0
    else
       byts[i] := round((flts[skipVx+i] - lMin) * slope);
  end;
end;


procedure float2byte(flts: TFloat32s; byts: TUInt8s; lMin, lMax: single; skipVx: integer; SSE: boolean = true);
//SSE-vs-x87 choice
(*const
  kAlign = 16; //SSE expects values aligned to 16-byte boundaries
var
   inAlign: PtrUint;*)
begin
  //writeln(format('skip %d vx8 %d vx32 %d min %g max%g', [skipVx, length(byts), length(flts), lMin, lMax]));
  {$IFDEF CPUX86_64}
  //float2byteKitSSE(flts,byts, lMin,lMax, skipVx);
  float2byteSISD(flts,byts, lMin,lMax, skipVx);
  {$ELSE}
  float2byteSISD(flts,byts, lMin,lMax, skipVx);
  {$ENDIF}
  (*{$IFDEF CPUX86_64}
  inAlign := PtrUint(@flts[skipVx]) mod kAlign;
  if inAlign <> 0 then
       SSE := false;
  inAlign := PtrUint(@byts[0]) mod kAlign;
  if inAlign <> 0 then
       SSE := false;
  if SSE then
     float2byteSIMD(flts,byts, lMin,lMax, skipVx)
  else
  {$ENDIF}
    float2byteSISD(flts,byts, lMin,lMax, skipVx);*)
end;

end.

