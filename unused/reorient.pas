unit reorient;
{$D-,O+,Q-,R-,S-}  //Delphi L- Y-
{$mode delphi}
interface

uses
  SysUtils, dialogs, nifti_types, clipbrd, math, SimdUtils;

function ShrinkLarge(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; lMaxDim: integer): boolean;

//function ReorientCore(var lHdr: TNIFTIhdr; lBufferIn: bytep): boolean;
//procedure ShrinkLarge(var lHdr: TNIFTIhdr; var lBuffer: bytep; lMaxDim: integer);
//procedure ShrinkOrEnlarge(var lHdr: TNIFTIhdr; var lBuffer: bytep; lFilter: integer; lScale: single); overload;
//procedure ShrinkOrEnlarge(var lHdr: TNIFTIhdr; var lBuffer: bytep; lFilter: integer; lScaleX, lScaleY, lScaleZ : single); overload;
//function EnlargeIsotropic(var lHdr: TNIFTIhdr; var lBuffer: bytep; lFilter: integer): boolean;
implementation
//  uses mainunit;

(*type
    ByteRA = array [1..1] of byte;
    Bytep = ^ByteRA;
    SingleRA = array [1..1] of Single;
    Singlep = ^SingleRA;
    SmallIntRA = array [1..1] of SmallInt;
    SMallIntp = ^SmallIntRA; *)



procedure Zoom(var lHdr: TNIFTIhdr; xScale, yScale, zScale: single);
//if we have a 256x256x256 pixel image with scale of 0.5, output is 128x128x128
//if we have a 1x1x1mm pixel image with a scale of 2.0, output is 2x2x2mm
var
   i: integer;
   scale: array[1..3] of single;
begin
     //showmessage(format('%g -> %g %g %g; %g %g %g; %g %g %g',[iScale, lHdr.srow_x[0],lHdr.srow_y[0],lHdr.srow_z[0], lHdr.srow_x[1],lHdr.srow_y[1],lHdr.srow_z[1], lHdr.srow_x[2],lHdr.srow_y[2],lHdr.srow_z[2]]));
     for i := 1 to 3 do begin
         if i = 1 then
                 scale[i] := xScale
         else if i = 2 then
                 scale[i] := yScale
         else
             scale[i] := zScale;
         if (round(lHdr.dim[i] * scale[i]) < 1) then begin
              scale[i] := 1/ lHdr.dim[i]  //e.g. for reducing 2D images, Z dimension does not change
         end;
         lHdr.dim[i] := round(lHdr.dim[i] * scale[i]);

         lHdr.pixdim[i] := lHdr.pixdim[i] / scale[i];
         //fx(lHdr.srow_x[i] ,lHdr.srow_y[i] ,lHdr.srow_z[i] );
     end;
     //showmessage(format('%g %g %g', [scale[1], scale[2], scale[3]]));
     for i :=0 to 2 do begin

         lHdr.srow_x[i] := lHdr.srow_x[i]/ scale[i+1];
         lHdr.srow_y[i] := lHdr.srow_y[i]/ scale[i+1];
         lHdr.srow_z[i] := lHdr.srow_z[i]/ scale[i+1];
     end;
end;

// Extends image shrink code by Anders Melander, anders@melander.dk
// Here's some additional copyrights for you:
//
// The algorithms and methods used in this library are based on the article
// "General Filtered Image Rescaling" by Dale Schumacher which appeared in the
// book Graphics Gems III, published by Academic Press, Inc.
// From filter.c:
// The authors and the publisher hold no copyright restrictions
// on any of these files; this source code is public domain, and
// is freely available to the entire computer graphics community
// for study, use, and modification.  We do request that the
// comment at the top of each file, identifying the original
// author and its original publication in the book Graphics
// Gems, be retained in all programs that use these files.



// Box filter
// a.k.a. "Nearest Neighbour" filter
// anme: I have not been able to get acceptable
//       results with this filter for subsampling.

function BoxFilter(Value: Single): Single;
begin
  if (Value > -0.5) and (Value <= 0.5) then
    Result := 1.0
  else
    Result := 0.0;
end;

// Hermite filter

function HermiteFilter(Value: Single): Single;
begin
  // f(t) = 2|t|^3 - 3|t|^2 + 1, -1 <= t <= 1
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
    Result := (2.0 * Value - 3.0) * Sqr(Value) + 1.0
  else
    Result := 0.0;
end;

// Triangle filter
// a.k.a. "Linear" or "Bilinear" filter

function TriangleFilter(Value: Single): Single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
    Result := 1.0 - Value
  else
    Result := 0.0;
end;

// Bell filter

function BellFilter(Value: Single): Single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 0.5) then
    Result := 0.75 - Sqr(Value)
  else if (Value < 1.5) then
  begin
    Value := Value - 1.5;
    Result := 0.5 * Sqr(Value);
  end
  else
    Result := 0.0;
end;

// B-spline filter

function SplineFilter(Value: Single): Single;
var
  tt: single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
  begin
    tt := Sqr(Value);
    Result := 0.5 * tt * Value - tt + 2.0 / 3.0;
  end
  else if (Value < 2.0) then
  begin
    Value := 2.0 - Value;
    Result := 1.0 / 6.0 * Sqr(Value) * Value;
  end
  else
    Result := 0.0;
end;

// Lanczos3 filter

function Lanczos3Filter(Value: Single): Single;

function SinC(Value: Single): Single;
  begin
    if (Value <> 0.0) then
    begin
      Value := Value * Pi;
      Result := sin(Value) / Value
    end
    else
      Result := 1.0;
  end;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 3.0) then
    Result := SinC(Value) * SinC(Value / 3.0)
  else
    Result := 0.0;
end;

function MitchellFilter(Value: Single): Single;
const
  B = (1.0 / 3.0);
  C = (1.0 / 3.0);
var
  tt: single;
begin
  if (Value < 0.0) then
    Value := -Value;
  tt := Sqr(Value);
  if (Value < 1.0) then
  begin
    Value := (((12.0 - 9.0 * B - 6.0 * C) * (Value * tt))
      + ((-18.0 + 12.0 * B + 6.0 * C) * tt)
      + (6.0 - 2 * B));
    Result := Value / 6.0;
  end
  else if (Value < 2.0) then
  begin
    Value := (((-1.0 * B - 6.0 * C) * (Value * tt))
      + ((6.0 * B + 30.0 * C) * tt)
      + ((-12.0 * B - 48.0 * C) * Value)
      + (8.0 * B + 24 * C));
    Result := Value / 6.0;
  end
  else
    Result := 0.0;
end;

type
  // Contributor for a pixel
  TFilterProc = function(Value: Single): Single;
  TContributor = record
    pixel: integer; // Source pixel
    weight: single; // Pixel weight
  end;
  TContributorList = array[0..0] of TContributor;
  PContributorList = ^TContributorList;
  // List of source pixels contributing to a destination pixel
  TCList = record
    n: integer;
    p: PContributorList;
  end;
  TCListList = array[0..0] of TCList;
  PCListList = ^TCListList;

procedure SetContrib(out contrib: PCListList; SrcPix, DstPix, Delta: integer; xscale, fwidth: single; filter: TFilterProc);
var
  i,j,k: integer;
  width, fscale: single;
  sum, center, weight: single; // Filter calculation variables
  left, right: integer; // Filter calculation variables
begin
  if (DstPix < 1) or (xscale <= 0) then exit;
  if (xscale < 1) then
  	fscale := 1.0 / xscale
  else
  	fscale := 1.0;
  width := fwidth * fscale;
  GetMem(contrib, DstPix * sizeof(TCList));
  for i := 0 to DstPix - 1 do begin
      contrib^[i].n := 0;
      GetMem(contrib^[i].p, trunc(width * 2.0 + 1) * sizeof(TContributor));
      center := i / xscale;
      left := floor(center - width);
      left := max(left,0);
      right := ceil(center + width);
      right := min(right, SrcPix - 1);
      sum := 0.0;
      for j := left to right do begin
        weight := filter((center - j) / fscale) / fscale;
        if (weight = 0.0) then
          continue;
        sum := sum + weight;
        k := contrib^[i].n;
        contrib^[i].n := contrib^[i].n + 1;
        contrib^[i].p^[k].pixel := j * Delta;
        contrib^[i].p^[k].weight := weight;
      end;
      for k := 0 to contrib^[i].n - 1 do
          contrib^[i].p^[k].weight := contrib^[i].p^[k].weight/sum;
      (*showmessage(format('n=%d l=%d r=%d c=%g sum=%g',[contrib^[i].n, left, right, center, sum]));
      for k := 0 to contrib^[i].n - 1 do
          showmessage(format('%d %g',[contrib^[i].p^[k].pixel, contrib^[i].p^[k].weight])); *)
    end;
end;

procedure Resize8(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; xScale, yScale, zScale, fwidth: single; filter: TFilterProc);
//rescales images with any dimension larger than lMaxDim to have a maximum dimension of maxdim...
label
  666;
var
  sum, mx, mn: single;
  lineStart, x,y,z, lXo,lYo,lZo,lXi,lYi,lZi, outBytes, i,j: integer;
  contrib: PCListList;
  finalImg, tempImgX, tempImgY, tempImgZ: TFloat32s;
begin
  lXi := lHdr.dim[1]; //input X
  lYi := lHdr.dim[2]; //input Y
  lZi := lHdr.dim[3]; //input Z
  lXo := lXi; lYo := lYi; lZo := lZi; //output initially same as input
  //inBytes := lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3]*bytesPerVox;
  //find min/max values
  mn := lBuffer[0];
  mx := mn;
  for i := 0 to ((lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3])-1) do begin
      if lBuffer[i] < mn then mn := lBuffer[i];
      if lBuffer[i] > mx then mx := lBuffer[i];
  end;
  Zoom(lHdr,xScale, yScale, zScale);
  //shrink in 1st dimension : do X as these are contiguous = faster, compute slower dimensions at reduced resolution
  lXo := lHdr.dim[1]; //input X
  SetLength( tempImgX,lXo*lYi*lZi); //8
  SetContrib(contrib, lXi, lXo, 1, xScale, fwidth, filter);
  i := 0;
  for z := 0 to (lZi - 1) do begin
    for y := 0 to (lYi-1) do begin
        lineStart :=  (lXi * y)+((lXi*lYi) * z);
        for x := 0 to (lXo - 1) do begin
            sum := 0.0;
            for j := 0 to contrib^[x].n - 1 do begin
              sum := sum + (contrib^[x].p^[j].weight * lBuffer[lineStart +contrib^[x].p^[j].pixel]);
            end;
            tempImgX[i] := sum;
            i := i + 1;
        end; //for X
    end; //for Y
  end; //for Z
  for i := 0 to lXo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  lBuffer := nil;
  //{$DEFINE XONLY}
  {$IFDEF XONLY}
  finalImg := tempImgX;
  goto 666;
  {$ENDIF}
  if ((lYi = lHdr.dim[2]) and (lZi = lHdr.dim[3])) then begin
     finalImg := tempImgX;
     goto 666; //e.g. 1D image
  end;
  //shrink in 2nd dimension
  lYo := lHdr.dim[2]; //reduce Y output
  Setlength( tempImgY,lXo*lYo*lZi); //8
  SetContrib(contrib, lYi, lYo, lXo, yScale, fwidth, filter);
  i := 0;
  for z := 0 to (lZi - 1) do begin
      for y := 0 to (lYo - 1) do begin
          for x := 0 to (lXo-1) do begin
            lineStart :=  x+((lXo*lYi) * z);
            sum := 0.0;
            for j := 0 to contrib^[y].n - 1 do begin
              //sum := sum + (contrib^[y].p^[j].weight * sourceLine^[contrib^[y].p^[j].pixel]);
              sum := sum + (contrib^[y].p^[j].weight * tempImgX[lineStart +contrib^[y].p^[j].pixel] );
            end;
            tempImgY[i] := sum;
            i := i + 1;
        end; //for X
    end; //for Y
  end; //for Z
  for i := 0 to lYo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  tempImgX := nil;
  //{$DEFINE YONLY}
  {$IFDEF YONLY}
    finalImg := tempImgY;
    goto 666;
  {$ENDIF}
  if (lZi = lHdr.dim[3]) then begin
     finalImg := tempImgY;
     goto 666; //e.g. 2D image
  end;
  //shrink the 3rd dimension
  lZo := lHdr.dim[3]; //reduce Z output
  SetLength( tempImgZ,lXo*lYo*lZo); //8
  SetContrib(contrib, lZi, lZo, (lXo*lYo), zScale, fwidth, filter);
  i := 0;
  for z := 0 to (lZo - 1) do begin
      for y := 0 to (lYo - 1) do begin
          for x := 0 to (lXo-1) do begin
            lineStart :=  x+(lXo * y);
            sum := 0.0;
            for j := 0 to contrib^[z].n - 1 do begin
              sum := sum + (contrib^[z].p^[j].weight * tempImgY[lineStart +contrib^[z].p^[j].pixel] );
            end;
            tempImgZ[i] := sum;
            i := i + 1;
        end; //for X
    end; //for Y
  end; //for Z
  for i := 0 to lZo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  tempImgY := nil;
  finalImg := tempImgZ;
666:
  lHdr.dim[1] := lXo;
  lHdr.dim[2] := lYo;
  lHdr.dim[3] := lZo;
  outBytes := lHdr.dim[1] * lHdr.dim[2] * lHdr.dim[3]*sizeof(byte);
  Setlength( lBuffer,outBytes); //8
  for i := 1 to ((lXo*lYo*lZo)-1) do begin
      //check image range - some interpolation can cause ringing
      // e.g. if input range 0..1000 do not create negative values!
      if finalImg[i] > mx then finalImg[i] := mx;
      if finalImg[i] < mn then finalImg[i] := mn;
      lBuffer[i] := round(finalImg[i]);
  end;
  finalImg := nil;
end; //ShrinkLarge8()

procedure Resize16(var lHdr: TNIFTIhdr; var lBuffer: TUint8s; xScale, yScale, zScale, fwidth: single; filter: TFilterProc);
//rescales images with any dimension larger than lMaxDim to have a maximum dimension of maxdim...
label
  666;
var
  sum, mx, mn: single;
  lineStart, x,y,z,  lXo,lYo,lZo,lXi,lYi,lZi, outBytes, i,j: integer;
  contrib: PCListList;
  lImg16: TInt16s;
  finalImg, tempImgX, tempImgY, tempImgZ: TFloat32s;
begin
  lXi := lHdr.dim[1]; //input X
  lYi := lHdr.dim[2]; //input Y
  lZi := lHdr.dim[3]; //input Z
  lXo := lXi; lYo := lYi; lZo := lZi; //output initially same as input
  //inBytes := lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3]*bytesPerVox;
  //find min/max values
  lImg16 := TInt16s(lBuffer);
  mn := lImg16[0];
  mx := mn;
  for i := 0 to ((lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3])-1) do begin
      if lImg16[i] < mn then mn := lImg16[i];
      if lImg16[i] > mx then mx := lImg16[i];
  end;
  Zoom(lHdr,xScale, yScale, zScale);
  //shrink in 1st dimension : do X as these are contiguous = faster, compute slower dimensions at reduced resolution
  lXo := lHdr.dim[1]; //input X
  Setlength( tempImgX,lXo*lYi*lZi); //8
  SetContrib(contrib, lXi, lXo, 1, xScale, fwidth, filter);
  i := 0;
  for z := 0 to (lZi - 1) do begin
    for y := 0 to (lYi-1) do begin
        lineStart := (lXi * y)+((lXi*lYi) * z);
        for x := 0 to (lXo - 1) do begin
            sum := 0.0;
            for j := 0 to contrib^[x].n - 1 do begin
              sum := sum + (contrib^[x].p^[j].weight * lImg16[lineStart +contrib^[x].p^[j].pixel]);
            end;
            tempImgX[i] := sum;
            i := i + 1;
        end; //for X
    end; //for Y
  end; //for Z
  for i := 0 to lXo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  lBuffer := nil;
  //{$DEFINE XONLY}
  {$IFDEF XONLY}
  finalImg := tempImgX;
  goto 666;
  {$ENDIF}
  if ((lYi = lHdr.dim[2]) and (lZi = lHdr.dim[3])) then goto 666; //e.g. 1D image
  //shrink in 2nd dimension
  lYo := lHdr.dim[2]; //reduce Y output
  Setlength( tempImgY,lXo*lYo*lZi); //8
  SetContrib(contrib, lYi, lYo, lXo, yScale, fwidth, filter);
  i := 0;
  for z := 0 to (lZi - 1) do begin
      for y := 0 to (lYo - 1) do begin
          for x := 0 to (lXo-1) do begin
            lineStart :=  x+((lXo*lYi) * z);
            sum := 0.0;
            for j := 0 to contrib^[y].n - 1 do begin
              //sum := sum + (contrib^[y].p^[j].weight * sourceLine^[contrib^[y].p^[j].pixel]);
              sum := sum + (contrib^[y].p^[j].weight * tempImgX[lineStart +contrib^[y].p^[j].pixel] );
            end;
            tempImgY[i] := sum;
            i := i + 1;
        end; //for X
    end; //for Y
  end; //for Z
  for i := 0 to lYo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  tempImgX := nil;
  //{$DEFINE YONLY}
  {$IFDEF YONLY}
    finalImg := tempImgY;
    goto 666;
  {$ENDIF}
  if (lZi = lHdr.dim[3]) then begin
     finalImg := tempImgY;
     goto 666; //e.g. 2D image
  end;
  //shrink the 3rd dimension
  lZo := lHdr.dim[3]; //reduce Z output
  Setlength( tempImgZ,lXo*lYo*lZo); //8
  SetContrib(contrib, lZi, lZo, (lXo*lYo), zScale, fwidth, filter);
  i := 0;
  for z := 0 to (lZo - 1) do begin
      for y := 0 to (lYo - 1) do begin
          for x := 0 to (lXo-1) do begin
            lineStart :=  x+(lXo * y);
            sum := 0.0;
            for j := 0 to contrib^[z].n - 1 do begin
              sum := sum + (contrib^[z].p^[j].weight * tempImgY[lineStart +contrib^[z].p^[j].pixel] );
            end;
            tempImgZ[i] := sum;
            i := i + 1;
        end; //for X
    end; //for Y
  end; //for Z
  for i := 0 to lZo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  tempImgY := nil;
  finalImg := tempImgZ;
666:
  lHdr.dim[1] := lXo;
  lHdr.dim[2] := lYo;
  lHdr.dim[3] := lZo;
  outBytes := lHdr.dim[1] * lHdr.dim[2] * lHdr.dim[3]*sizeof(SmallInt);
  Setlength( lBuffer,outBytes);
  lImg16 := TInt16s(lBuffer);
  for i := 0 to ((lXo*lYo*lZo)-1) do begin
      //check image range - some interpolation can cause ringing
      // e.g. if input range 0..1000 do not create negative values!
      if finalImg[i] > mx then finalImg[i] := mx;
      if finalImg[i] < mn then finalImg[i] := mn;
      lImg16[i] := round(finalImg[i]);
  end;
  finalImg := nil;
end; //ShrinkLarge16()

(*procedure Resize24(var lHdr: TNIFTIhdr; var lBuffer: bytep; xScale,yScale, zScale, fwidth: single; filter: TFilterProc);
//rescales images with any dimension larger than lMaxDim to have a maximum dimension of maxdim...
//this is done as three passes: once for red, green and blue
// it might be a little faster to compute as one pass.
// however, shrinklarge is designed for huge images (~2Gb) that will overwhelm graphics cards
// since we use 32-bit floats, computing 3 passes requires less RAM
var
   iHdr: TNIFTIhdr;
   lXi, lYi, lZi, nVxi, nVxo, i, j, k: integer;
  imgo, img1: bytep;
begin
  lXi := lHdr.dim[1]; //input X
  lYi := lHdr.dim[2]; //input Y
  lZi := lHdr.dim[3]; //input Z
  nVxi := lXi * lYi * lZi;
  iHdr := lHdr;
  //GLForm1.IntensityBox.Caption := floattostr(yScale);
  for k := 1 to 3 do begin
    GetMem( img1,nVxi);
    j := k;
    for i := 1 to nVxi do begin
        img1[i] := lBuffer[j];
        j := j + 3;
    end;
    lHdr := iHdr;
    Resize8(lHdr, img1, xScale, yScale, zScale, fwidth, filter);
    if (k = 1) then begin
       nVxo := lHdr.dim[1] * lHdr.dim[2] * lHdr.dim[3];
       getmem(imgo,nVxo * 3);
    end;
    j := k;
    for i := 1 to nVxo do begin
        imgo[j] := img1[i];
        j := j + 3;
    end;
    FreeMem( img1);
  end;
  freemem(lBuffer);
  lBuffer := imgo;
end; //ShrinkLarge24()
  *)

procedure Resize32(var lHdr: TNIFTIhdr; var lBuffer: TUint8s; xScale, yScale, zScale, fwidth: single; filter: TFilterProc);
//rescales images with any dimension larger than lMaxDim to have a maximum dimension of maxdim...
label
  666;
var
  sum, mx, mn: single;
  lineStart, x,y,z,  lXo,lYo,lZo,lXi,lYi,lZi, i,j: integer;
  contrib: PCListList;
  lImg32,finalImg, tempImgX, tempImgY, tempImgZ: TFloat32s;
begin
  lXi := lHdr.dim[1]; //input X
  lYi := lHdr.dim[2]; //input Y
  lZi := lHdr.dim[3]; //input Z
  lXo := lXi; lYo := lYi; lZo := lZi; //output initially same as input
  //inBytes := lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3]*bytesPerVox;
  //find min/max values
  lImg32 := TFloat32s(lBuffer);
  mn := lImg32[0];
  mx := mn;
  for i := 0 to ((lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3])-1) do begin
      if lImg32[i] < mn then mn := lImg32[i];
      if lImg32[i] > mx then mx := lImg32[i];
  end;

  Zoom(lHdr,xScale, yScale, zScale);
  //shrink in 1st dimension : do X as these are contiguous = faster, compute slower dimensions at reduced resolution
  lXo := lHdr.dim[1]; //input X
  Setlength( tempImgX,lXo*lYi*lZi); //8
  SetContrib(contrib, lXi, lXo, 1, xScale, fwidth, filter);
  i := 0;
  for z := 0 to (lZi - 1) do begin
    for y := 0 to (lYi-1) do begin
        lineStart := (lXi * y)+((lXi*lYi) * z);
        for x := 0 to (lXo - 1) do begin
            sum := 0.0;
            for j := 0 to contrib^[x].n - 1 do begin
              sum := sum + (contrib^[x].p^[j].weight * lImg32[lineStart +contrib^[x].p^[j].pixel]);
            end;
            tempImgX[i] := sum;
            i := i + 1;
        end; //for X
    end; //for Y
  end; //for Z
  for i := 0 to lXo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  lBuffer := nil;
  //{$DEFINE XONLY}
  {$IFDEF XONLY}
  finalImg := tempImgX;
  goto 666;
  {$ENDIF}
  if ((lYi = lHdr.dim[2]) and (lZi = lHdr.dim[3])) then goto 666; //e.g. 1D image
  //shrink in 2nd dimension
  lYo := lHdr.dim[2]; //reduce Y output
  Setlength( tempImgY,lXo*lYo*lZi); //8
  SetContrib(contrib, lYi, lYo, lXo, yScale, fwidth, filter);
  i := 0;
  for z := 0 to (lZi - 1) do begin
      for y := 0 to (lYo - 1) do begin
          for x := 0 to (lXo-1) do begin
            lineStart :=  x+((lXo*lYi) * z);
            sum := 0.0;
            for j := 0 to contrib^[y].n - 1 do begin
              //sum := sum + (contrib^[y].p^[j].weight * sourceLine^[contrib^[y].p^[j].pixel]);
              sum := sum + (contrib^[y].p^[j].weight * tempImgX[lineStart +contrib^[y].p^[j].pixel] );
            end;
            tempImgY[i] := sum;
            i := i + 1;
        end; //for X
    end; //for Y
  end; //for Z
  for i := 0 to lYo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  tempImgX := nil;
  //{$DEFINE YONLY}
  {$IFDEF YONLY}
    finalImg := tempImgY;
    goto 666;
  {$ENDIF}
  if (lZi = lHdr.dim[3]) then begin
     finalImg := tempImgY;
     goto 666; //e.g. 2D image
  end;
  //shrink the 3rd dimension
  lZo := lHdr.dim[3]; //reduce Z output
  Setlength( tempImgZ,lXo*lYo*lZo); //8
  SetContrib(contrib, lZi, lZo, (lXo*lYo), zScale, fwidth, filter);
  i := 0;
  for z := 0 to (lZo - 1) do begin
      for y := 0 to (lYo - 1) do begin
          for x := 0 to (lXo-1) do begin
            lineStart :=  x+(lXo * y);
            sum := 0.0;
            for j := 0 to contrib^[z].n - 1 do begin
              sum := sum + (contrib^[z].p^[j].weight * tempImgY[lineStart +contrib^[z].p^[j].pixel] );
            end;
            tempImgZ[i] := sum;
            i := i + 1;
        end; //for X
    end; //for Y
  end; //for Z
  for i := 0 to lZo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  tempImgY := nil;
  finalImg := tempImgZ;
666:
  lHdr.dim[1] := lXo;
  lHdr.dim[2] := lYo;
  lHdr.dim[3] := lZo;
  Setlength( lBuffer, (lXo*lYo*lZo) * sizeof(Single));
  lImg32 := TFloat32s(lBuffer);
  for i := 0 to ((lXo*lYo*lZo)-1) do begin
      //check image range - some interpolation can cause ringing
      // e.g. if input range 0..1000 do not create negative values!
      if finalImg[i] > mx then finalImg[i] := mx;
      if finalImg[i] < mn then finalImg[i] := mn;
      lImg32[i] := (finalImg[i]);
  end;
  finalImg := nil;
end; //ShrinkLarge16()

FUNCTION specialsingle (var s:single): boolean;
//returns true if s is Infinity, NAN or Indeterminate
CONST kSpecialExponent = 255 shl 23;
VAR Overlay: LongInt ABSOLUTE s;
BEGIN
 IF ((Overlay AND kSpecialExponent) = kSpecialExponent) THEN
   RESULT := true
 ELSE
   RESULT := false;
END; //specialsingle()

(*function EnlargeIsotropic(var lHdr: TNIFTIhdr; var lBuffer: bytep; lFilter: integer): boolean;
var
  mmMx, mmMn, xScale, yScale, zScale, fwidth: single;
  filter: TFilterProc;
begin
 result := false;
 mmMx := max(max(lHdr.pixdim[1],lHdr.pixdim[2]),lHdr.pixdim[3]);
 mmMn := min(min(lHdr.pixdim[1],lHdr.pixdim[2]),lHdr.pixdim[3]);
 if (mmMn = 0) or (specialsingle(mmMn)) or (specialsingle(mmMx)) or (mmMn = mmMx) then exit;
 xScale :=  lHdr.pixdim[1]/ mmMn;
 yScale :=  lHdr.pixdim[2]/ mmMn;
 zScale :=  lHdr.pixdim[3]/ mmMn;
 //upsample, so use Mitchell by default
 if lFilter = 0 then begin
    filter := @BoxFilter; fwidth := 0.5;
 end else if lFilter = 1 then begin
      filter := @TriangleFilter; fwidth := 1;
 end else if lFilter = 2 then begin
      filter := @HermiteFilter; fwidth := 1;
 end else if lFilter = 3 then begin
     filter := @BellFilter; fwidth := 1.5;
 end else if lFilter = 4 then begin
     filter := @SplineFilter; fwidth := 2;
 end else if lFilter = 5 then begin
     filter := @Lanczos3Filter; fwidth := 3;
 end else begin
     filter := @MitchellFilter; fwidth := 2;
 end;
 if lHdr.datatype = kDT_UNSIGNED_CHAR then
    Resize8(lHdr, lBuffer, xScale, yScale, zScale, fwidth, @filter)
 else if lHdr.datatype = kDT_SIGNED_SHORT then
    Resize16(lHdr, lBuffer, xScale, yScale, zScale, fwidth, @filter)
 else if lHdr.datatype = kDT_FLOAT then
    Resize32(lHdr, lBuffer, xScale, yScale, zScale, fwidth, @filter)
 else if lHdr.datatype = kDT_RGB then
    Resize24(lHdr, lBuffer, xScale, yScale, zScale, fwidth, @filter);
 result := true;
end;   *)

function ShrinkLarge(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; lMaxDim: integer): boolean;
//rescales images with any dimension larger than lMaxDim to have a maximum dimension of maxdim...
var
   imx: integer;
   scale, fwidth: single;
   filter: TFilterProc;
begin
  result := false;
  imx := max(max(lHdr.dim[1], lHdr.dim[2]), lHdr.dim[3]);
  if (imx <= lMaxDim) or (lMaxDim < 1) then exit;
  scale := lMaxDim/imx;
  //showmessage(format('%g %d %d %d',[scale, lHdr.dim[1], lHdr.dim[2], lHdr.dim[3]]));
  //n.b. can also be used to upsize or downsize data:
  // xscale := 0.5; // 50%
  // xscale := 1.5; //150%
  //filter := @BoxFilter; fwidth := 0.5;
  //filter := @TriangleFilter; fwidth := 1;
  //filter := @Hermite; fwidth := 1;
  //filter := @BellFilter; fwidth := 1.5;
  //filter := @SplineFilter; fwidth := 2;

  filter := @Lanczos3Filter; fwidth := 3;
  //filter := @MitchellFilter; fwidth := 2;
  if lHdr.datatype = kDT_UNSIGNED_CHAR then
     Resize8(lHdr, lBuffer, scale, scale, scale, fwidth, @filter)
  else if lHdr.datatype = kDT_SIGNED_SHORT then
     Resize16(lHdr, lBuffer, scale, scale, scale, fwidth, @filter)
  else if lHdr.datatype = kDT_FLOAT then
     Resize32(lHdr, lBuffer, scale, scale, scale, fwidth, @filter);(*
  else if lHdr.datatype = kDT_RGB then
     Resize24(lHdr, lBuffer, scale, scale, scale, fwidth, @filter)*);
  result := true;
end;



end.
