unit sse2;

{$mode delphi}
{$H+}

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
Type
  ByteRA = array [1..1] of byte;
  Bytep = ^ByteRA;
  SingleRA = array [1..1] of single; //DWord
  SingleP = ^SingleRA;
//https://en.wikibooks.org/wiki/X86_Assembly/SSE
// 4 32-bit floating points (single-precision)
// 8 16-bit integers
// 16 8-bit characters (bytes)
procedure float2byteSIMD(flts: TFloat32s; byts: TUInt8s; lMin, lMax: single; skipVx: integer);
//procedure SSEScale(var lMod,lMin,lMax : single; lMaxByte: byte; var liSz: integer; var lDataIn: TFloat32s; var lDataOut: TUInt8s);
const
  kAlign = 16; //SSE expects values aligned to 16-byte boundaries
var
     t : integer;
     tail : integer;
     parameters: singleP;
     parunalign: singleP;
     localIn: singleP;
     localOut: byteP;
     lMod: single;
     lMaxByte: byte = 255;
     fMaxByte: single;
     lSz : uint64;
  begin
    lMod := 255/(lMax-lMin);
    {Get a multiple-of-eight number of voxels}
    //lSz := length(flts);
    lSz := length(byts);
    tail := lSz and 7;
    //lSz := lSz and $fffffff8;
    lSz := lSz and $ffffffffffffff8; //32bit->$fffffff8;
    localIn := SingleP(@flts[skipVx]);
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
        GetMem(parunalign, 80+kAlign);
        parameters := System.Align(parunalign, kAlign); //aligned
        lMin := lMin - ((lMax-lMin)/510);  //shift 0.5 units: SSE uses trunc, we want round
        //paralign := singleP($fffffff0 and (integer(parameters)+15));
	//paralign := singleP($fffffff0 and (pinteger(parameters)+15));
        //paralign := singleP(@parameters[1]);
        fMaxByte := lMaxByte;
	for t := 1 to 4 do begin
	  parameters[t]    := lMod;//scale factors
	  parameters[t+4]  := lMin;//bias
	  parameters[t+8]  := 0; //zeroes
          parameters[t+12] := fMaxByte;//MaxByte
        end;
   {Real problem here is getting the pointers into the right places
    It gives complete nonsense unless you copy the passed parameters
    into local variables before running}
     //http://www.cs.tufts.edu/comp/181/x64_cheatsheet.pdf
    asm
       //mov eax, paralign
       //http://www.cs.tufts.edu/comp/181/x64_cheatsheet.pdf
       //eax ->  rax
       //ebx -> rbx
       //ecx -> rcx
       //edx -> rdx
       mov rax, [parameters];  //mov -> movaps
       mov rbx, [localIn];
       mov rcx, [lSz];
       mov rdx, [localOut];
       //MOVAPS =move packed single, MOVDQA
       movaps xmm7, [rax] //db $0f, $28, $78, $00     // movaps xmm7, [eax]    - X7 is the scale factors
       movaps xmm6, [rax+16] //	   db $0f, $28, $70, $10     // movaps xmm6, [eax+16] - X6 is the bias
       movaps xmm5, [rax+32] //db $0f, $28, $68, $20     // movaps xmm5, [eax+32] - X5 is zeroes
       movaps xmm4, [rax+48] //db $0f, $28, $60, $30     // movaps xmm4, [eax+48] - X4 is MaxBytes
       @ProcessLoop:
       prefetchnta [rbx+128] //db $0f, $18, $83, $80, $00, $00, $00      // prefetchnta [ebx+128]
       prefetcht0 [rdx+32] //db $0f, $18, $4a, $20                     // prefetcht0 [edx+32]
       // https://stackoverflow.com/questions/9161807/sse-convert-short-integer-to-float
       // CVTPI2PS : convert 32-bit integer to float
       //  CVTDQ2PS
       //floats 1,2,3,4
       movaps xmm0, [rbx] //db $0f, $28, $43, $00                     // movaps xmm0, [ebx]
       //floats 5,6,7,8
       movaps xmm1, [rbx+16] //db $0f, $28, $4b, $10                     // movaps xmm1, [ebx+16]
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
       //MOVQ move quad word
       movq [rdx],mm0 //db $0f, $7f, $42, $00                     // movq [edx],mm0
       add rbx, 32 //rbx= in-put array 32-bit floats,
       add rdx, 8  //rdx= out-put array 8-bit integers
       sub rcx, 8  //rcx= number of elements
       jne @ProcessLoop
       emms //db $0f, $77                               // emms
    end;

    freemem(parunalign);
  end;
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
const
  kAlign = 16; //SSE expects values aligned to 16-byte boundaries
var
   inAlign: PtrUint;
begin
  {$IFDEF CPUX86_64}
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
    float2byteSISD(flts,byts, lMin,lMax, skipVx);
end;

end.
