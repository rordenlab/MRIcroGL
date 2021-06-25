unit neon;

{$mode delphi}
{$H+}
(*
BSD-2-Clause License
https://opensource.org/licenses/BSD-2-Clause

Copyright (c) 2021  Chris Rorden. All rights reserved.

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
  Classes, SysUtils, SimdUtils;


procedure flt2byte(flts: TFloat32s; byts: TUInt8s; lMin, lMax: single; skipVx: int64);
procedure int2byte(lMin, lMax: single; ints: TInt16s; byts: TUInt8s; skipVx: int64; SSE: boolean = true);

implementation

{$L ./aarch64-darwin/scale2uint8.o}
function f32_i8sse(in32: pointer; out8: pointer; n: int64; mn, mx: single): Integer; external name '__Z9f32_i8ssePfPhxff';
function i16_i8sse(in16: pointer; out8: pointer; n: int64; mn, mx: single): Integer; external name '__Z9i16_i8ssePsPhxff';


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
  if slope > 0 then
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
const
  kAlign = 16; //Neon expects values aligned to 16-byte boundaries
  kInBytes = 2;
  kVectorSize = 16; //Neon will retire 128 bytes (16x8-bit) values at a time
var
   inAlign: PtrUint;
   head: int64 = 0;
   tail: int64 = 0;
   nVox: int64;
begin
	nVox := length(byts);
	if (lMin = lMax) then lMin -= 1E-9;
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
		int2byteSISD(lMin,lMax, @ints[0],@byts[nVox-tail+head], skipVx+nVox-tail, tail);
		nVox -= tail;
	end;
	//function i16_i8sse(in16: pointer; out8: pointer; n: int64; mn, mx: single): Integer; external name '__Z9f32_i8ssePfPhxff';
        if SSE then
		i16_i8sse(@ints[skipVx],@byts[head],nVox, lMin,lMax)
	else
		int2byteSISD(lMin,lMax, @ints[0],@byts[0], skipVx, length(byts));
end;

procedure flt2byteSISD(lMin, lMax: single; flts: Fltp0; byts: Bytep0; skipVx, nVx: int64);
var
   i: int64;
   slope: single;
begin
  slope := abs(lMax - lMin);
  if slope > 0 then
     slope := 255.0/slope;
  for i := 0 to (nVx - 1) do begin
    if (flts[skipVx+i] >= lMax) then
       byts[i] := 255
    else if  (flts[skipVx+i] <= lMin) then
        byts[i] := 0
    else
       byts[i] := round((flts[skipVx+i] - lMin) * slope);
  end;
end;

procedure flt2byte(flts: TFloat32s; byts: TUInt8s; lMin, lMax: single; skipVx: int64);
const
  kAlign = 16; //Neon expects values aligned to 16-byte boundaries
  kInBytes = 2;
  kVectorSize = 16; //Neon will retire 128 bytes (16x8-bit) values at a time
var
   inAlign: PtrUint;
   SSE : boolean = true;
   head: int64 = 0;
   tail: int64 = 0;
   nVox: int64;
begin
	nVox := length(byts);
	if (lMin = lMax) then lMin -= 1E-9;
	//check output alignment (required)
	inAlign := PtrUint(@byts[0]) mod kAlign;
	if (lMin >= lMax) or (inAlign <> 0) or (nVox < (kVectorSize * 3)) then
		SSE := false;
	if (SSE) then begin //check input alignment, scalar required items to align
		inAlign := PtrUint(@flts[skipVx]) mod kAlign;
		if (inAlign <> 0) and ((inAlign mod kInBytes) <> 0) then
			SSE := false
		else if (inAlign <> 0) then begin
			head := (kAlign - inAlign) div kInBytes;
			flt2byteSISD(lMin,lMax, @flts[0],@byts[0], skipVx, head);
			skipVx += head;
			nVox -= head;
		end;
	end;
	if (SSE) and ((nVox mod kVectorSize) <> 0) then begin
		tail := (nVox mod kVectorSize);
		//writeln('>>>', nVox, 'x', tail, ':',ints[skipVx+nVox-tail]);
		flt2byteSISD(lMin,lMax, @flts[0],@byts[nVox-tail+head], skipVx+nVox-tail, tail);
		//byts[nVox-tail] := 127;
		nVox -= tail;
	end;
        if SSE then begin
		f32_i8sse(@flts[skipVx],@byts[head],nVox, lMin,lMax)
	end else
		flt2byteSISD(lMin,lMax, @flts[0],@byts[0], skipVx, length(byts));		
end;

end.

