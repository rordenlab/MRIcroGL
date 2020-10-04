unit ssei16;

{$mode delphi}
{$H+}

interface

uses
  Classes, SysUtils, SimdUtils;

(*type
    TUInt8s = array of uint8;
    TInt16s = array of int16; *)

{$DEFINE SIGNED}

procedure int2byte(ints: TInt16s; byts: TUInt8s; lMin, lMax: single; skipVx: integer; SSE: boolean = true);

implementation

{$IFDEF CPUX86_64}
{$ASMMODE INTEL}
Type
  ByteRA0 = array [0..0] of byte;
  Bytep0 = ^ByteRA0;
  IntRA0 = array [0..0] of int16;
  Intp0 = ^IntRA0;

  ByteRA = array [1..1] of byte;
  Bytep = ^ByteRA;
  IntRA = array [1..1] of int16;
  Intp = ^IntRA;
  UIntRA = array [1..1] of uint16;
  UIntp = ^UIntRA;
  SingleRA = array [1..1] of single; //DWord
  SingleP = ^SingleRA;

(*procedure int2byteSIMDX(ints: Intp0; byts: Bytep0; lMin, lMax: single; skipVx, nVx: integer); assembler; nostackframe;
const
  Single_255: Single = 255.0;
  Single_0_5: Single = 0.5;
  Negate:     array[0..7] of LongWord = ($80000000, $80000000, $80000000, $80000000, $80000000, $80000000, $80000000, $80000000);
  Shuffle:    array[0..7] of LongWord = (0, 4, 1, 5, 0, 0, 0, 0);
asm
//Windows: Parameters beyond the fourth are passed on stack
//Unix rdi,rsi,rdx,r8,r9,r10,r11
//  https://en.wikipedia.org/wiki/X86_calling_conventions#System_V_AMD64_ABI
  {Unix, Win
   RDI , RCX          = pointer to flts
   RSI , RDX          = pointer to byts
   XMM0, XMM2         = iMin
   XMM1, XMM3         = iMax
   RDX , [RSP+28]     = skipVX
   R8				  = nVx
  }

  { Broadcast the scale, lMin and lMax into the rest of the registers }
  SHUFPS  XMM0, XMM0, $00 //Min
  SHUFPS  XMM1, XMM1, $00 //Max
  SHUFPS  XMM0, XMM0, $00
end;*)

procedure int2byteSIMD(ints: Intp0; byts: Bytep0; lMin, lMax: single; skipVx, nVx: integer);
//https://stackoverflow.com/questions/9161807/sse-convert-short-integer-to-float
const
  kAlign = 16; //SSE expects values aligned to 16-byte boundaries
var
     t : integer;
     tail : integer;
     parameters: singleP;
     parunalign: singleP;
     parametersU16: UIntp;
     localIn: Intp;
     localOut: byteP;
     lMod: single;
     lMaxByte: byte = 255;
     fMaxByte: single;
     lSz : uint64;
  begin
    lMod := 255/(lMax-lMin);
    {Get a multiple-of-eight number of voxels}
    //lSz := length(flts);
    lSz := nVx;
    tail := lSz and 7;
    //lSz := lSz and $fffffff8;
    lSz := lSz and $ffffffffffffff8; //32bit->$fffffff8;
    localIn := Intp(@ints[skipVx]);
    localOut := ByteP(@byts[0]);
    if tail <> 0 then begin
	  for t := 1 to (tail) do begin //1402
		  if localIn[t+lSz] > lMax then
			 localOut[t+lSz] := lMaxByte
		  else if localIn[t+lSz] < lMin then
			   localOut[t+lSz] := 0
		  else
			  localOut[t+lSz] := round((localIn[t+lSz]-lMin)* lMod);
	  end;
	end;
        //https://bugs.freepascal.org/view.php?id=34031
        GetMem(parunalign, 64+kAlign);
        parameters := System.Align(parunalign, kAlign); //aligned
        parametersU16 := System.Align(parunalign, kAlign); //aligned
        lMin := lMin - ((lMax-lMin)/510);  //shift 0.5 units: SSE uses trunc, we want round
        //paralign := singleP($fffffff0 and (integer(parameters)+15));
	//paralign := singleP($fffffff0 and (pinteger(parameters)+15));
        //paralign := singleP(@parameters[1]);
        fMaxByte := lMaxByte;
	for t := 1 to 4 do begin
	  parameters[t]    := lMod;//scale factors
	  {$IFDEF SIGNED}
	  parameters[t+4]  := lMin+32768.0;//bias + 0x8000
	  {$ELSE}
	  parameters[t+4]  := lMin;//bias
	  {$ENDIF}
	  parameters[t+8] := fMaxByte;//MaxByte
	  //12
	  parametersU16[t+24] := $8000; //UINT16 0x8000
	  parametersU16[t+28] := $8000; //UINT16 0x8000
    end;
   {Real problem here is getting the pointers into the right places
    It gives complete nonsense unless you copy the passed parameters
    into local variables before running}
     //http://www.cs.tufts.edu/comp/181/x64_cheatsheet.pdf
    asm
       mov rax, [parameters];  //mov -> movaps
       mov rbx, [localIn];
       mov rcx, [lSz];
       mov rdx, [localOut];
       //MOVAPS =move packed single, MOVDQA
       movaps xmm7, [rax] //xmm7 is the scale factors
       movaps xmm6, [rax+16] //xmm6 is the bias
       //movaps xmm5, [rax+32] //xmm5 is zeroes
       movaps xmm4, [rax+32] //xmm4 MaxBytes (255)
       {$IFDEF SIGNED}
       movdqa xmm8, [rax+48] // xmm8 is uint16 $8000
       {$ENDIF}
       movdqa xmm5, xmm4
       pxor xmm5, xmm5 //all int zeros
       @ProcessLoop:
       prefetchnta [rbx+64] //db $0f, $18, $83, $80, $00, $00, $00      // prefetchnta [ebx+64]
       prefetcht0 [rdx+32] //db $0f, $18, $4a, $20                     // prefetcht0 [edx+32]
       // https://stackoverflow.com/questions/9161807/sse-convert-short-integer-to-float
       movdqa xmm0, [rbx] //move 8 16-bit integers to xmm0
       {$IFDEF SIGNED}
       paddw xmm0, xmm8 //add 0x8000
       {$ENDIF}
       movdqa xmm1, xmm0
       punpcklwd xmm0, xmm5 //xmm5 = zeros (same representation for floats and ints)
       cvtdq2ps xmm0, xmm0
       punpckhwd xmm1, xmm5 //xmm5 = zeros
       cvtdq2ps xmm1, xmm1
       subps xmm0, xmm6 //db $0f, $5c, $c6                          // subps xmm0, xmm6 -- bias
       mulps xmm0, xmm7 //db $0f, $59, $c7                          // mulps xmm0, xmm7 -- scale
       subps xmm1, xmm6 //db $0f, $5c, $ce                          // subps xmm1, xmm6
       mulps xmm1, xmm7 //db $0f, $59, $cf                          // mulps xmm1, xmm7
       //x64 adds registers XMM8 through XMM15
       minps xmm0, xmm4 //db $0f, $5d, $c4                          // minps xmm0, xmm4 -- chop left
       maxps xmm0, xmm5 //db $0f, $5f, $c5                          // maxps xmm0, xmm5 -- chop right
       minps xmm1, xmm4 //db $0f, $5d, $cc                          // minps xmm1, xmm4
       maxps xmm1, xmm5 //db $0f, $5f, $cd                          // maxps xmm1, xmm5

       cvttps2pi mm0,xmm0 //db $0f, $2c, $c0                          // cvttps2pi mm0,xmm0
       movhlps xmm0,xmm0 //db $0f, $12, $c0                          // movhlps xmm0,xmm0
       cvttps2pi mm1,xmm0 //db $0f, $2c, $c8                          // cvttps2pi mm1,xmm0
       cvttps2pi mm2,xmm1 //db $0f, $2c, $d1                          // cvttps2pi mm2,xmm1
       movhlps xmm1,xmm1 //db $0f, $12, $c9                          // movhlps xmm1,xmm1
       cvttps2pi mm3,xmm1 //db $0f, $2c, $d9                          // cvttps2pi mm3,xmm1

       packssdw mm0, mm1 //db $0f, $6b, $c1                          // packssdw mm0, mm1
       packssdw mm2, mm3 //db $0f, $6b, $d3                          // packssdw mm2, mm3
       packuswb mm0, mm2 //db $0f, $67, $c2                          // packuswb mm0, mm2
       movq [rdx],mm0 //db $0f, $7f, $42, $00                     // movq [edx],mm0
       add rbx, 16 //rbx= in-put array 32-bit floats,
       //add rbx, 32 //rbx= in-put array 32-bit floats,
       add rdx, 8  //rdx= out-put array 8-bit integers
       sub rcx, 8  //rcx= number of elements
       jne @ProcessLoop
       emms //db $0f, $77                               // emms
    end;
    freemem(parunalign);
  end;
{$ENDIF}

procedure int2byteSISD(ints: Intp0; byts: Bytep0; lMin, lMax: single; skipVx, nVx: integer);
var
   i: integer;
   slope: single;
begin
  slope := 255.0/(lMax - lMin);
  for i := 0 to (nVx - 1) do begin
    if (ints[skipVx+i] >= lMax) then
       byts[i] := 255
    else if  (ints[skipVx+i] <= lMin) then
        byts[i] := 0
    else
       byts[i] := round((ints[skipVx+i] - lMin) * slope);
  end;
end;

(*procedure int2byteSISDX(ints: Intp0; byts: Bytep0; lMin, lMax: single; skipVx, nVx: integer);
var
   i: integer;
   slope: single;
begin
  slope := 255.0/(lMax - lMin);
  for i := 0 to (nVx - 1) do begin
    byts[i] := 129

  end;
end;*)

procedure int2byte(ints: TInt16s; byts: TUInt8s; lMin, lMax: single; skipVx: integer; SSE: boolean = true);
//SSE-vs-x87 choice
const
  kAlign = 16; //SSE expects values aligned to 16-byte boundaries
  kInBytes = 2;
  kInPerVector = kAlign div kInBytes; //e.g. of 16-bytes vector, we can process 8 values of 2 bytes
var
   inAlign: PtrUint;
   head: integer = 0;
   tail: integer = 0;
   nVox: integer;
begin
	nVox := length(byts);
	{$IFDEF CPUX86_64}
	//check output alignment (required)
	inAlign := PtrUint(@byts[0]) mod kAlign;
	if (inAlign <> 0) or (nVox < (kInPerVector * 3)) then
		SSE := false;
	if (SSE) then begin //check input alignment, scalar required items to align
		inAlign := PtrUint(@ints[skipVx]) mod kAlign;
		if (inAlign <> 0) and ((inAlign mod kInBytes) <> 0) then
			SSE := false
		else if (inAlign <> 0) then begin
			head := (kAlign - inAlign) div kInBytes;
			int2byteSISD(@ints[0],@byts[0], lMin,lMax, skipVx, head);
			skipVx += head;
			nVox -= head;
		end;
	end;
	if (SSE) and ((nVox mod kInPerVector) <> 0) then begin
		tail := (nVox mod kInPerVector);
		//writeln('>>>', nVox, 'x', tail, ':',ints[skipVx+nVox-tail]);
		int2byteSISD(@ints[0],@byts[nVox-tail+head], lMin,lMax, skipVx+nVox-tail, tail);
		//byts[nVox-tail] := 127;
		nVox -= tail;
	end;
	if SSE then
		int2byteSIMD(@ints[0],@byts[head], lMin,lMax, skipVx, nVox)
	else
	{$ENDIF}
		int2byteSISD(@ints[0],@byts[0], lMin,lMax, skipVx, length(byts));

end;

end.

