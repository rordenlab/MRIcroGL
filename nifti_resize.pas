unit nifti_resize;

{$mode delphi}
{$H+}

{$D-,O+,Q-,R-,S-}  //Delphi L- Y-

interface

uses
  {$IFDEF UNIX} DateUtils,{$ENDIF}
  Classes, SysUtils, nifti_types, SimdUtils, dialogs;

//function EnlargeIsotropic(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s): boolean;
function ShrinkLargeMb(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; lMaxTexMb: integer; isAntiAlias, isLabels: boolean): boolean;
//function ShrinkLarge(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; lMaxDim: integer; isLabels: boolean = false): boolean;
function ShrinkOrEnlarge(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; lFilter: integer; lScale: single): boolean; overload;
function ShrinkOrEnlarge(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; lFilter: integer; lScaleX, lScaleY, lScaleZ: single; outDatatype : integer = -1): boolean; overload;

implementation

uses math;


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
  i,j,k: int64;
  width, fscale: single;
  sum, center, weight: single; // Filter calculation variables
  left, right: integer; // Filter calculation variables
begin
  if (DstPix < 1) or (xscale <= 0) then exit;
  if (xscale < 1)and (fwidth > 0.6) then //not for nearest neighbor!
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

procedure Zoom(var lHdr: TNIFTIhdr; xScale, yScale, zScale: single);
//if we have a 256x256x256 pixel image with scale of 0.5, output is 128x128x128
//if we have a 1x1x1mm pixel image with a scale of 2.0, output is 2x2x2mm
var
   i, inDim: int64;
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
         inDim := lHdr.dim[i];
         lHdr.dim[i] := round(lHdr.dim[i] * scale[i]);
         scale[i] := lHdr.dim[i] / inDim; //e.g. rounding error
         lHdr.pixdim[i] := lHdr.pixdim[i] / scale[i];
         //fx(lHdr.srow_x[i] ,lHdr.srow_y[i] ,lHdr.srow_z[i] );
     end;
     for i :=0 to 2 do begin
         lHdr.srow_x[i] := lHdr.srow_x[i]/ scale[i+1];
         lHdr.srow_y[i] := lHdr.srow_y[i]/ scale[i+1];
         lHdr.srow_z[i] := lHdr.srow_z[i]/ scale[i+1];
     end;
end;

procedure Resize32(var lHdr: TNIFTIhdr; var lImg8: TUInt8s; xScale, yScale, zScale, fwidth: single; filter: TFilterProc);
//rescales images with any dimension larger than lMaxDim to have a maximum dimension of maxdim...
label
  666;
const
  kIdx1 = 0; //0=arrays indexed from 0, 1= arrays indexed from 1
  kEnd = 1 - kIdx1;
var
  //sum, mx, mn: single;
  sum, mx, mn: double;
  lineStart, x,y,z, lXo,lYo,lZo,lXi,lYi,lZi, i,j: int64;
  contrib: PCListList;
  finalImg, img8x, img8y, img8z: TUInt8s;
  inImg, tempImgX, tempImgY, tempImgZ, out32: TFloat32s;
begin
  //bytesPerVox := 4;
  lXi := lHdr.dim[1]; //input X
  lYi := lHdr.dim[2]; //input Y
  lZi := lHdr.dim[3]; //input Z
  lXo := lXi; lYo := lYi; lZo := lZi; //output initially same as input
  //inBytes := lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3]*bytesPerVox;
  inImg := TFloat32s(lImg8);
  //find min/max values
  mn := inImg[0];
  mx := mn;
  for i := 0 to (lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3])-kEnd do begin
      if inImg[i] < mn then mn := inImg[i];
      if inImg[i] > mx then mx := inImg[i];
  end;
  Zoom(lHdr,xScale, yScale, zScale);
  //shrink in 1st dimension : do X as these are contiguous = faster, compute slower dimensions at reduced resolution
  lXo := lHdr.dim[1]; //input X
  setlength(img8x,lXo*lYi*lZi*4);
  tempImgX := TFloat32s(img8x);
  SetContrib(contrib, lXi, lXo, 1, xScale, fwidth, filter);
  i := kIdx1;
  for z := 0 to (lZi - 1) do begin
    for y := 0 to (lYi-1) do begin
        lineStart := kIdx1+ (lXi * y)+((lXi*lYi) * z);
        for x := 0 to (lXo - 1) do begin
            sum := 0.0;
            for j := 0 to contrib^[x].n - 1 do begin
              sum := sum + (contrib^[x].p^[j].weight * inImg[lineStart +contrib^[x].p^[j].pixel]);
            end;
            tempImgX[i] := sum;
            i := i + 1;
        end; //for X
    end; //for Y
  end; //for Z
  for i := 0 to lXo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  setlength( lImg8, 0);
  //{$DEFINE XONLY}
  finalImg := img8x;
  out32 := tempImgX;
  {$IFDEF XONLY}
  goto 666;
  {$ENDIF}
  if ((lYi = lHdr.dim[2]) and (lZi = lHdr.dim[3])) then goto 666; //e.g. 1D image
  //shrink in 2nd dimension
  lYo := lHdr.dim[2]; //reduce Y output
  setlength(img8y,lXo*lYo*lZi*4);
  tempImgY := TFloat32s(img8y);
  //SetLength( tempImgY,lXo*lYo*lZi); //8
  SetContrib(contrib, lYi, lYo, lXo, yScale, fwidth, filter);
  i := kIdx1;
  for z := 0 to (lZi - 1) do begin
      for y := 0 to (lYo - 1) do begin
          for x := 0 to (lXo-1) do begin
            lineStart :=  kIdx1+x+((lXo*lYi) * z);
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
  SetLength( img8x,0);
  //{$DEFINE YONLY}
  finalImg := img8y;
  out32 := tempImgY;
  {$IFDEF YONLY}
    goto 666;
  {$ENDIF}
  if (lZi = lHdr.dim[3]) then goto 666; //e.g. 2D image
  //shrink the 3rd dimension
  lZo := lHdr.dim[3]; //reduce Z output
  setlength(img8z,lXo*lYo*lZo*4);
  tempImgZ := TFloat32s(img8z);
  //SetLength( tempImgZ,lXo*lYo*lZo); //8
  SetContrib(contrib, lZi, lZo, (lXo*lYo), zScale, fwidth, filter);
  i := kIdx1;
  for z := 0 to (lZo - 1) do begin
      for y := 0 to (lYo - 1) do begin
          for x := 0 to (lXo-1) do begin
            lineStart :=  kIdx1+x+(lXo * y);
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
  Setlength( img8y,0);
  finalImg := img8z;
  out32 := tempImgZ;

666:
  lHdr.dim[1] := lXo;
  lHdr.dim[2] := lYo;
  lHdr.dim[3] := lZo;
  for i := kIdx1 to ((lXo*lYo*lZo)-kEnd) do begin
      //check image range - some interpolation can cause ringing
      // e.g. if input range 0..1000 do not create negative values!
      if out32[i] > mx then out32[i] := mx;
      if out32[i] < mn then out32[i] := mn;
  end;
  lImg8 := finalImg;
end; //Resize32()

{$DEFINE RAWRESIZE} //native->FLOAT->FLOAT->Native
{$IFDEF RAWRESIZE}
//Native Resizing requires less RAM

{$IFDEF TEMPFLOAT} //native->float->native else native->native->native
procedure Resize8(var lHdr: TNIFTIhdr; var lImg8: TUInt8s; xScale, yScale, zScale, fwidth: single; filter: TFilterProc);
//rescales images with any dimension larger than lMaxDim to have a maximum dimension of maxdim...
label
  666;
const
  kIdx1 = 0; //0=arrays indexed from 0, 1= arrays indexed from 1
  kEnd = 1 - kIdx1;
var
  sum: double;
  mx, mn: byte;
  lineStart, x,y,z, lXo,lYo,lZo,lXi,lYi,lZi, i,j: int64;
  contrib: PCListList;
  finalImg,
  img8x, img8y, img8z: TUInt8s;
  tempImgX, tempImgY, tempImgZ, out32: TFloat32s;
begin
  //bytesPerVox := 4;
  lXi := lHdr.dim[1]; //input X
  lYi := lHdr.dim[2]; //input Y
  lZi := lHdr.dim[3]; //input Z
  lXo := lXi; lYo := lYi; lZo := lZi; //output initially same as input
  //inBytes := lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3]*bytesPerVox;
  //find min/max values
  mn := lImg8[0];
  mx := mn;
  for i := 0 to (lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3])-kEnd do begin
      if lImg8[i] < mn then mn := lImg8[i];
      if lImg8[i] > mx then mx := lImg8[i];
  end;
  //{$IFDEF UNIX}writeln(format('int8 range %d..%d', [mn, mx]));{$ENDIF}
  Zoom(lHdr,xScale, yScale, zScale);
  //shrink in 1st dimension : do X as these are contiguous = faster, compute slower dimensions at reduced resolution
  lXo := lHdr.dim[1]; //input X
  try
     setlength(img8x,lXo*lYi*lZi*4);
  except
      {$IFDEF UNIX}writeln('Heap memory exhausted: reduce MaxVox');{$ENDIF}
  end;
  tempImgX := TFloat32s(img8x);
  SetContrib(contrib, lXi, lXo, 1, xScale, fwidth, filter);
  i := kIdx1;
  for z := 0 to (lZi - 1) do begin
    for y := 0 to (lYi-1) do begin
        lineStart := kIdx1+ (lXi * y)+((lXi*lYi) * z);
        for x := 0 to (lXo - 1) do begin
            sum := 0.0;
            for j := 0 to contrib^[x].n - 1 do begin
              sum := sum + (contrib^[x].p^[j].weight * lImg8[lineStart +contrib^[x].p^[j].pixel]);
            end;
            tempImgX[i] := sum;
            i := i + 1;
        end; //for X
    end; //for Y
  end; //for Z
  for i := 0 to lXo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  setlength( lImg8, 0);
  //{$DEFINE XONLY}
  finalImg := img8x;
  out32 := tempImgX;
  {$IFDEF XONLY}
  goto 666;
  {$ENDIF}
  if ((lYi = lHdr.dim[2]) and (lZi = lHdr.dim[3])) then goto 666; //e.g. 1D image
  //shrink in 2nd dimension
  lYo := lHdr.dim[2]; //reduce Y output
  setlength(img8y,lXo*lYo*lZi*4);
  tempImgY := TFloat32s(img8y);
  //SetLength( tempImgY,lXo*lYo*lZi); //8
  SetContrib(contrib, lYi, lYo, lXo, yScale, fwidth, filter);
  i := kIdx1;
  for z := 0 to (lZi - 1) do begin
      for y := 0 to (lYo - 1) do begin
          for x := 0 to (lXo-1) do begin
            lineStart :=  kIdx1+x+((lXo*lYi) * z);
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
  SetLength( img8x,0);
  //{$DEFINE YONLY}
  finalImg := img8y;
  out32 := tempImgY;
  {$IFDEF YONLY}
    goto 666;
  {$ENDIF}
  if (lZi = lHdr.dim[3]) then goto 666; //e.g. 2D image
  //shrink the 3rd dimension
  lZo := lHdr.dim[3]; //reduce Z output
  setlength(img8z,lXo*lYo*lZo*4);
  tempImgZ := TFloat32s(img8z);
  //SetLength( tempImgZ,lXo*lYo*lZo); //8
  SetContrib(contrib, lZi, lZo, (lXo*lYo), zScale, fwidth, filter);
  i := kIdx1;
  for z := 0 to (lZo - 1) do begin
      for y := 0 to (lYo - 1) do begin
          for x := 0 to (lXo-1) do begin
            lineStart :=  kIdx1+x+(lXo * y);
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
  Setlength( img8y,0);
  finalImg := img8z;
  out32 := tempImgZ;

666:
  lHdr.dim[1] := lXo;
  lHdr.dim[2] := lYo;
  lHdr.dim[3] := lZo;
  setlength(lImg8,  lXo * lYo * lZo);
  for i := kIdx1 to ((lXo*lYo*lZo)-kEnd) do begin
      //check image range - some interpolation can cause ringing
      // e.g. if input range 0..1000 do not create negative values!
      if out32[i] > mx then lImg8[i] := mx
      else if out32[i] < mn then lImg8[i] := mn
      else lImg8[i] := round(out32[i])
  end;
  finalImg := nil;
end; //Resize8()

procedure Resize16(var lHdr: TNIFTIhdr; var lImg8: TUInt8s; xScale, yScale, zScale, fwidth: single; filter: TFilterProc);
//rescales images with any dimension larger than lMaxDim to have a maximum dimension of maxdim...
label
  666;
const
  kIdx1 = 0; //0=arrays indexed from 0, 1= arrays indexed from 1
  kEnd = 1 - kIdx1;
var
  sum: double;
  mx, mn: Int16;
  lineStart, x,y,z, lXo,lYo,lZo,lXi,lYi,lZi, i,j: int64;
  contrib: PCListList;
  finalImg,
  img8x, img8y, img8z: TUInt8s;
  img16: TInt16s;
  tempImgX, tempImgY, tempImgZ, out32: TFloat32s;
begin
  //bytesPerVox := 4;
  lXi := lHdr.dim[1]; //input X
  lYi := lHdr.dim[2]; //input Y
  lZi := lHdr.dim[3]; //input Z
  lXo := lXi; lYo := lYi; lZo := lZi; //output initially same as input
  //inBytes := lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3]*bytesPerVox;
  img16 := TInt16s(lImg8);
  //find min/max values
  mn := img16[0];
  mx := mn;
  for i := 0 to (lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3])-kEnd do begin
      if img16[i] < mn then mn := img16[i];
      if img16[i] > mx then mx := img16[i];
  end;
  //{$IFDEF UNIX}writeln(format('>int16 range %d..%d', [mn, mx]));{$ENDIF}
  Zoom(lHdr,xScale, yScale, zScale);
  //shrink in 1st dimension : do X as these are contiguous = faster, compute slower dimensions at reduced resolution
  lXo := lHdr.dim[1]; //input X
  //{$IFDEF UNIX}writeln(format('>resize x %d', [lXo*lYi*lZi*4]));{$ENDIF}
  try
     setlength(img8x,lXo*lYi*lZi*4);
  except
      {$IFDEF UNIX}On E: EOutOfMemory do writeln(format('X Heap memory exhausted: reduce MaxVox (%dx%dx%d)',[lXo,lYi,lZi]));{$ENDIF}
  end;
  tempImgX := TFloat32s(img8x);
  SetContrib(contrib, lXi, lXo, 1, xScale, fwidth, filter);
  i := kIdx1;
  for z := 0 to (lZi - 1) do begin
    for y := 0 to (lYi-1) do begin
        lineStart := kIdx1+ (lXi * y)+((lXi*lYi) * z);
        for x := 0 to (lXo - 1) do begin
            sum := 0.0;
            for j := 0 to contrib^[x].n - 1 do begin
              sum := sum + (contrib^[x].p^[j].weight * img16[lineStart +contrib^[x].p^[j].pixel]);
            end;
            tempImgX[i] := sum;
            i := i + 1;
        end; //for X
    end; //for Y
  end; //for Z
  for i := 0 to lXo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  setlength( lImg8, 0);
  finalImg := img8x;
  out32 := tempImgX;
  {$IFDEF XONLY}
  goto 666;
  {$ENDIF}
  if ((lYi = lHdr.dim[2]) and (lZi = lHdr.dim[3])) then goto 666; //e.g. 1D image
  //shrink in 2nd dimension
  lYo := lHdr.dim[2]; //reduce Y output
  //{$IFDEF UNIX}writeln(format('>resize y %d', [lXo*lYo*lZi*4]));{$ENDIF}
  try
      setlength(img8y,lXo*lYo*lZi*4);
  except
      {$IFDEF UNIX}writeln(format('Y Heap memory exhausted: reduce MaxVox (%dx%dx%d)',[lXo,lYo,lZi]));{$ENDIF}
  end;

  tempImgY := TFloat32s(img8y);
  //SetLength( tempImgY,lXo*lYo*lZi); //8
  SetContrib(contrib, lYi, lYo, lXo, yScale, fwidth, filter);
  i := kIdx1;
  for z := 0 to (lZi - 1) do begin
      for y := 0 to (lYo - 1) do begin
          for x := 0 to (lXo-1) do begin
            lineStart :=  kIdx1+x+((lXo*lYi) * z);
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
  SetLength( img8x,0);
  //{$DEFINE YONLY}
  finalImg := img8y;
  out32 := tempImgY;
  {$IFDEF YONLY}
    goto 666;
  {$ENDIF}
  if (lZi = lHdr.dim[3]) then goto 666; //e.g. 2D image
  //shrink the 3rd dimension
  lZo := lHdr.dim[3]; //reduce Z output
  //{$IFDEF UNIX}writeln(format('>resize z %d', [lXo*lYo*lZo*4]));{$ENDIF}
  try
     setlength(img8z,lXo*lYo*lZo*4);
  except
      {$IFDEF UNIX}writeln(format('Z Heap memory exhausted: reduce MaxVox (%dx%dx%d)',[lXo,lYo,lZo]));{$ENDIF}
  end;
  tempImgZ := TFloat32s(img8z);
  //SetLength( tempImgZ,lXo*lYo*lZo); //8
  SetContrib(contrib, lZi, lZo, (lXo*lYo), zScale, fwidth, filter);
  i := kIdx1;
  for z := 0 to (lZo - 1) do begin
      for y := 0 to (lYo - 1) do begin
          for x := 0 to (lXo-1) do begin
            lineStart :=  kIdx1+x+(lXo * y);
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
  Setlength( img8y,0);
  finalImg := img8z;
  out32 := tempImgZ;
666:
  lHdr.dim[1] := lXo;
  lHdr.dim[2] := lYo;
  lHdr.dim[3] := lZo;
  setlength(lImg8,  2 * lXo * lYo * lZo);
  img16:= TInt16s(lImg8);
  for i := kIdx1 to ((lXo*lYo*lZo)-kEnd) do begin
      //check image range - some interpolation can cause ringing
      // e.g. if input range 0..1000 do not create negative values!
      if out32[i] > mx then img16[i] := mx
      else if out32[i] < mn then img16[i] := mn
      else img16[i] := round(out32[i])
  end;
  finalImg := nil;
  //{$IFDEF UNIX}writeln(format('>resize mn/mx %d %d', [mn, mx]));{$ENDIF}
end; //Resize16()
{$ELSE} //if native->float->native else native->native->native

function Round8(v: double; mn, mx: UInt8): UInt8;
begin
     if (v <= mn) then exit(mn);
     if (v >= mx) then exit(mx);
     result := round(v);
end;

procedure Resize8(var lHdr: TNIFTIhdr; var lImg8: TUInt8s; xScale, yScale, zScale, fwidth: single; filter: TFilterProc);
//rescales images with any dimension larger than lMaxDim to have a maximum dimension of maxdim...
label
  666;
const
  kIdx1 = 0; //0=arrays indexed from 0, 1= arrays indexed from 1
  kEnd = 1 - kIdx1;
var
  sum: double;
  mn, mx, mnT, mxT: byte;
  lineStart, x,y,z, lXo,lYo,lZo,lXi,lYi,lZi, i,j: int64;
  contrib: PCListList;
  finalImg, img8x, img8y, img8z: TUInt8s;
begin
  //bytesPerVox := 4;
  lXi := lHdr.dim[1]; //input X
  lYi := lHdr.dim[2]; //input Y
  lZi := lHdr.dim[3]; //input Z
  lXo := lXi; lYo := lYi; lZo := lZi; //output initially same as input
  //inBytes := lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3]*bytesPerVox;
  //find min/max values
  mn := lImg8[0];
  mx := mn;
  mnT := 0;
  mxT := 255;
  for i := 0 to (lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3])-kEnd do begin
      if lImg8[i] < mn then mn := lImg8[i];
      if lImg8[i] > mx then mx := lImg8[i];
  end;
  //{$IFDEF UNIX}writeln(format('int8 range %d..%d', [mn, mx]));{$ENDIF}
  Zoom(lHdr,xScale, yScale, zScale);
  //shrink in 1st dimension : do X as these are contiguous = faster, compute slower dimensions at reduced resolution
  lXo := lHdr.dim[1]; //input X
  try
     setlength(img8x,lXo*lYi*lZi);
  except
      {$IFDEF UNIX}writeln('Heap memory exhausted: reduce MaxVox');{$ENDIF}
  end;
  if ((lYi = lHdr.dim[2]) and (lZi = lHdr.dim[3])) then begin //final pass: bound by input extremes to avoid ringing
     mnT := mn;
     mxT := mx;
  end;
  SetContrib(contrib, lXi, lXo, 1, xScale, fwidth, filter);
  i := kIdx1;
  for z := 0 to (lZi - 1) do begin
    for y := 0 to (lYi-1) do begin
        lineStart := kIdx1+ (lXi * y)+((lXi*lYi) * z);
        for x := 0 to (lXo - 1) do begin
            sum := 0.0;
            for j := 0 to contrib^[x].n - 1 do begin
              sum := sum + (contrib^[x].p^[j].weight * lImg8[lineStart +contrib^[x].p^[j].pixel]);
            end;
            img8x[i] := round8(sum, mnT, mxT);
            i := i + 1;
        end; //for X
    end; //for Y
  end; //for Z
  for i := 0 to lXo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  setlength( lImg8, 0);
  //{$DEFINE XONLY}
  finalImg := img8x;
  {$IFDEF XONLY}
  goto 666;
  {$ENDIF}
  if ((lYi = lHdr.dim[2]) and (lZi = lHdr.dim[3])) then goto 666; //e.g. 1D image
  //shrink in 2nd dimension
  lYo := lHdr.dim[2]; //reduce Y output
  setlength(img8y,lXo*lYo*lZi);
  if (lZi = lHdr.dim[3]) then begin //final pass: bound by input extremes to avoid ringing
     mnT := mn;
     mxT := mx;
  end;
  //SetLength( tempImgY,lXo*lYo*lZi); //8
  SetContrib(contrib, lYi, lYo, lXo, yScale, fwidth, filter);
  i := kIdx1;
  for z := 0 to (lZi - 1) do begin
      for y := 0 to (lYo - 1) do begin
          for x := 0 to (lXo-1) do begin
            lineStart :=  kIdx1+x+((lXo*lYi) * z);
            sum := 0.0;
            for j := 0 to contrib^[y].n - 1 do begin
              //sum := sum + (contrib^[y].p^[j].weight * sourceLine^[contrib^[y].p^[j].pixel]);
              sum := sum + (contrib^[y].p^[j].weight * img8x[lineStart +contrib^[y].p^[j].pixel] );
            end;
            img8y[i] := round8(sum,mn,mx);
            i := i + 1;
        end; //for X
    end; //for Y
  end; //for Z
  for i := 0 to lYo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  SetLength( img8x,0);
  //{$DEFINE YONLY}
  finalImg := img8y;
  {$IFDEF YONLY}
    goto 666;
  {$ENDIF}
  if (lZi = lHdr.dim[3]) then goto 666; //e.g. 2D image
  //shrink the 3rd dimension
  lZo := lHdr.dim[3]; //reduce Z output
  setlength(img8z,lXo*lYo*lZo);
  //SetLength( tempImgZ,lXo*lYo*lZo); //8
  SetContrib(contrib, lZi, lZo, (lXo*lYo), zScale, fwidth, filter);
  i := kIdx1;
  for z := 0 to (lZo - 1) do begin
      for y := 0 to (lYo - 1) do begin
          for x := 0 to (lXo-1) do begin
            lineStart :=  kIdx1+x+(lXo * y);
            sum := 0.0;
            for j := 0 to contrib^[z].n - 1 do begin
              sum := sum + (contrib^[z].p^[j].weight * img8y[lineStart +contrib^[z].p^[j].pixel] );
            end;
            img8z[i] := round8(sum,mn,mx);
            i := i + 1;
        end; //for X
    end; //for Y
  end; //for Z
  for i := 0 to lZo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  Setlength( img8y,0);
  finalImg := img8z;
666:
  lHdr.dim[1] := lXo;
  lHdr.dim[2] := lYo;
  lHdr.dim[3] := lZo;
  lImg8 := finalImg;
  //range check
  (*for i := 0 to (lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3])-kEnd do begin
      if lImg8[i] < mn then lImg8[i] := mn;
      if lImg8[i] > mx then lImg8[i] := mx;
  end; *)
end; //Resize8()

function Round16(v: double; mn, mx: Int16): Int16;
begin
     if (v <= mn) then exit(mn);
     if (v >= mx) then exit(mx);
     result := round(v);
end;

procedure Resize16(var lHdr: TNIFTIhdr; var lImg8: TUInt8s; xScale, yScale, zScale, fwidth: single; filter: TFilterProc);
//rescales images with any dimension larger than lMaxDim to have a maximum dimension of maxdim...
label
  666;
const
  kIdx1 = 0; //0=arrays indexed from 0, 1= arrays indexed from 1
  kEnd = 1 - kIdx1;
var
  sum: double;
  mn, mx, mnT, mxT: Int16;
  lineStart, x,y,z, lXo,lYo,lZo,lXi,lYi,lZi, i,j: int64;
  contrib: PCListList;
  finalImg,
  img8x, img8y, img8z: TUInt8s;
  img16: TInt16s;
  tempImgX, tempImgY, tempImgZ: TInt16s;
begin
  //bytesPerVox := 4;
  lXi := lHdr.dim[1]; //input X
  lYi := lHdr.dim[2]; //input Y
  lZi := lHdr.dim[3]; //input Z
  lXo := lXi; lYo := lYi; lZo := lZi; //output initially same as input
  //inBytes := lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3]*bytesPerVox;
  img16 := TInt16s(lImg8);
  //find min/max values
  mnT := -32768;
  mxT := 32767;
  mn := img16[0];
  mx := mn;
  for i := 0 to (lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3])-kEnd do begin
      if img16[i] < mn then mn := img16[i];
      if img16[i] > mx then mx := img16[i];
  end;
  //{$IFDEF UNIX}writeln(format('>int16 range %d..%d', [mn, mx]));{$ENDIF}
  Zoom(lHdr,xScale, yScale, zScale);
  //shrink in 1st dimension : do X as these are contiguous = faster, compute slower dimensions at reduced resolution
  lXo := lHdr.dim[1]; //input X
  //{$IFDEF UNIX}writeln(format('>resize x %d', [lXo*lYi*lZi*4]));{$ENDIF}
  try
     setlength(img8x,lXo*lYi*lZi*2);
  except
      {$IFDEF UNIX}On E: EOutOfMemory do writeln(format('X Heap memory exhausted: reduce MaxVox (%dx%dx%d)',[lXo,lYi,lZi]));{$ENDIF}
  end;
  if ((lYi = lHdr.dim[2]) and (lZi = lHdr.dim[3])) then begin //final pass: bound by input extremes to avoid ringing
     mnT := mn;
     mxT := mx;
  end;
  tempImgX := TInt16s(img8x);
  SetContrib(contrib, lXi, lXo, 1, xScale, fwidth, filter);
  i := kIdx1;
  for z := 0 to (lZi - 1) do begin
    for y := 0 to (lYi-1) do begin
        lineStart := kIdx1+ (lXi * y)+((lXi*lYi) * z);
        for x := 0 to (lXo - 1) do begin
            sum := 0.0;
            for j := 0 to contrib^[x].n - 1 do begin
              sum := sum + (contrib^[x].p^[j].weight * img16[lineStart +contrib^[x].p^[j].pixel]);
            end;
            tempImgX[i] := round16(sum,mnT,mxT);
            i := i + 1;
        end; //for X
    end; //for Y
  end; //for Z
  for i := 0 to lXo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  setlength( lImg8, 0);
  finalImg := img8x;
  //out16 := tempImgX;
  {$IFDEF XONLY}
  goto 666;
  {$ENDIF}
  if ((lYi = lHdr.dim[2]) and (lZi = lHdr.dim[3])) then goto 666; //e.g. 1D image
  //shrink in 2nd dimension
  lYo := lHdr.dim[2]; //reduce Y output
  //{$IFDEF UNIX}writeln(format('>resize y %d', [lXo*lYo*lZi*4]));{$ENDIF}
  try
      setlength(img8y,lXo*lYo*lZi*2);
  except
      {$IFDEF UNIX}writeln(format('Y Heap memory exhausted: reduce MaxVox (%dx%dx%d)',[lXo,lYo,lZi]));{$ENDIF}
  end;
  if (lZi = lHdr.dim[3]) then begin //final pass: bound by input extremes to avoid ringing
     mnT := mn;
     mxT := mx;
  end;
  tempImgY := TInt16s(img8y);
  //SetLength( tempImgY,lXo*lYo*lZi); //8
  SetContrib(contrib, lYi, lYo, lXo, yScale, fwidth, filter);
  i := kIdx1;
  for z := 0 to (lZi - 1) do begin
      for y := 0 to (lYo - 1) do begin
          for x := 0 to (lXo-1) do begin
            lineStart :=  kIdx1+x+((lXo*lYi) * z);
            sum := 0.0;
            for j := 0 to contrib^[y].n - 1 do begin
              //sum := sum + (contrib^[y].p^[j].weight * sourceLine^[contrib^[y].p^[j].pixel]);
              sum := sum + (contrib^[y].p^[j].weight * tempImgX[lineStart +contrib^[y].p^[j].pixel] );
            end;
            tempImgY[i] := round16(sum,mnT,mxT);
            i := i + 1;
        end; //for X
    end; //for Y
  end; //for Z
  for i := 0 to lYo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  SetLength( img8x,0);
  //{$DEFINE YONLY}
  finalImg := img8y;
  //out16 := tempImgY;
  {$IFDEF YONLY}
    goto 666;
  {$ENDIF}
  if (lZi = lHdr.dim[3]) then goto 666; //e.g. 2D image
  //shrink the 3rd dimension
  lZo := lHdr.dim[3]; //reduce Z output
  //{$IFDEF UNIX}writeln(format('>resize z %d', [lXo*lYo*lZo*4]));{$ENDIF}
  try
     setlength(img8z,lXo*lYo*lZo*2);
  except
      {$IFDEF UNIX}writeln(format('Z Heap memory exhausted: reduce MaxVox (%dx%dx%d)',[lXo,lYo,lZo]));{$ENDIF}
  end;
  tempImgZ := TInt16s(img8z);
  SetContrib(contrib, lZi, lZo, (lXo*lYo), zScale, fwidth, filter);
  i := kIdx1;
  for z := 0 to (lZo - 1) do begin
      for y := 0 to (lYo - 1) do begin
          for x := 0 to (lXo-1) do begin
            lineStart :=  kIdx1+x+(lXo * y);
            sum := 0.0;
            for j := 0 to contrib^[z].n - 1 do begin
              sum := sum + (contrib^[z].p^[j].weight * tempImgY[lineStart +contrib^[z].p^[j].pixel] );
            end;
            tempImgZ[i] := round16(sum,mn,mx);
            i := i + 1;
        end; //for X
    end; //for Y
  end; //for Z
  for i := 0 to lZo - 1 do
     FreeMem(contrib^[i].p);
  FreeMem(contrib);
  Setlength( img8y,0);
  finalImg := img8z;
  //out16 := tempImgZ;
666:
  lHdr.dim[1] := lXo;
  lHdr.dim[2] := lYo;
  lHdr.dim[3] := lZo;
  lImg8 := finalImg;
  //range check
  (*img16:= TInt16s(lImg8);
  for i := 0 to (lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3])-kEnd do begin
      if img16[i] < mn then img16[i] := mn;
      if img16[i] > mx then img16[i] := mx;
  end; *)
  //{$IFDEF UNIX}writeln(format('>resize mn/mx %d %d', [mn, mx]));{$ENDIF}
end; //Resize16()
{$ENDIF} //if native->float->native else native->native->native
{$ELSE}
procedure Resize8(var lHdr: TNIFTIhdr; var lImg8: TUInt8s; xScale, yScale, zScale, fwidth: single; filter: TFilterProc);
var
   i, vx: int64;
   v: TFloat32s;
begin
  vx := lHdr.dim[1] * lHdr.dim[2] * lHdr.dim[3]; //input Z
  setlength(v, vx);
  for i := 0 to vx-1 do
     v[i] := lImg8[i];
  Resize32(lHdr, TUInt8s(v), xScale, yScale, zScale, fwidth, @filter);
  vx := lHdr.dim[1] * lHdr.dim[2] * lHdr.dim[3]; //input Z
  setlength(lImg8, vx);
  for i := 0 to vx-1 do
      lImg8[i] := round(v[i]);
  setlength(v, 0);
end;

procedure Resize16(var lHdr: TNIFTIhdr; var lImg8: TUInt8s; xScale, yScale, zScale, fwidth: single; filter: TFilterProc);
var
   i, vx: int64;
   v: TFloat32s;
   in16, out16 : TInt16s;
begin
  vx := lHdr.dim[1] * lHdr.dim[2] * lHdr.dim[3]; //input Z
  setlength(v, vx);
  in16 := TInt16s(lImg8);
  for i := 0 to vx-1 do
     v[i] := in16[i];
  Resize32(lHdr, TUInt8s(v), xScale, yScale, zScale, fwidth, @filter);
  vx := lHdr.dim[1] * lHdr.dim[2] * lHdr.dim[3]; //input Z
  setlength(lImg8, vx * 2);
  out16 := TInt16s(lImg8);
  for i := 0 to vx-1 do
      out16[i] := round(v[i]);
  setlength(v, 0);
end;
{$ENDIF}

procedure ResizeU16(var lHdr: TNIFTIhdr; var lImg8: TUInt8s; xScale, yScale, zScale, fwidth: single; filter: TFilterProc);
//change datatype kDT_UINT16 ->  kDT_SIGNED_SHORT
var
   i, vx: int64;
   u16: TUInt16s;
   i16: TInt16s;
begin
     {$IFDEF UNIX}
     writeln('Resize requires converting UInt16 -> Int16');
     {$ENDIF}
     vx := lHdr.dim[1] * lHdr.dim[2] * lHdr.dim[3]; //input Z
     u16 := TUInt16s(lImg8);
     i16 := TInt16s(lImg8);
     for i := 0 to vx-1 do
         i16[i] := smallint(u16[i]- 32768);
     lHdr.datatype := kDT_SIGNED_SHORT;
     lHdr.scl_inter:= (32768 * lHdr.scl_slope) + lHdr.scl_inter;
     Resize16(lHdr, lImg8, xScale, yScale, zScale, fwidth, filter);
end;

function ScaleToMb(var lHdr: TNIFTIhdr; lMaxTexMb: integer): double;
const
  kBytesPerMb = 1048576;
var
   mb: double;
   scaleVol, scale: double;
   i : integer;
   outdim: array [1..3] of integer;
begin
  mb := (lHdr.dim[1] * lHdr.dim[2] * lHdr.dim[3] * 4)/kBytesPerMb; //RGBA8 is 4 bytes per voxel
  //scale :=  power(scaleVol, 1/3);
  if (mb <= 0) or (mb < lMaxTexMb) or (lMaxTexMb < 1) then exit(1.0);
  {$IFDEF UNIX}writeln(format('%dx%dx%d texture would require %5.3f mb', [lHdr.dim[1], lHdr.dim[2], lHdr.dim[3], mb])); {$ENDIF}
  scaleVol := lMaxTexMb/mb;
  scale :=  exp(ln(scaleVol) / 3); //power(scaleVol, 1/3);
  for i := 1 to 3 do begin
      outdim[i] := round(lHdr.dim[i] * scale);
      outdim[i] := max(outdim[i],1);
  end;
  mb := (outdim[1] * outdim[2] * outdim[3] * 4)/kBytesPerMb; //RGBA8 is 4 bytes per voxel
  if (mb > lMaxTexMb) then //round down, not up
     scale := (max(outdim[1],max(outdim[2],outdim[3]))-1)/max(lHdr.dim[1],max(lHdr.dim[2],lHdr.dim[3]));
  result := scale;
end;

procedure ShrinkFastLinear16(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; lScale: single);
var
   out8: TUInt8s;
   out16, in16: TInt16s;
   //out24, in24: TRGBs;
   //out32, in32: TInt32s;
   x,y,z,v, nx,ny,nz, nv, inx, iny, inz, inxy: integer;
   xlo,ylo,zlo,xhi,yhi,zhi, vo: array of integer;
   xfl,yfl,zfl,xfh,yfh,zfh: array of single;
   i: int64;
   fv, f: single;
begin
     inx := lHdr.dim[1];
     iny := lHdr.dim[2];
     inz := lHdr.dim[3];
     inxy := inx * iny;
     Zoom(lHdr,lScale, lScale, lScale);
     nx := max(lHdr.dim[1], 1);
     ny := max(lHdr.dim[2], 1);
     nz := max(lHdr.dim[3], 1);
     nv := max(lHdr.dim[4], 1) * max(lHdr.dim[5], 1) * max(lHdr.dim[6], 1) * max(lHdr.dim[7], 1);
     setlength(out8, nx*ny*nz*nv*(lHdr.bitpix div 8));
     //set scaled x
     setlength(xlo, nx);
     setlength(xhi, nx);
     setlength(xfl, nx);
     setlength(xfh, nx);
     f := inx/lHdr.dim[1];
     for i := 0 to (nx-1) do begin
     	 fv := max(((i+0.5)*f)-0.5, 0); //fv := f * i;
     	 xlo[i] := trunc(fv);
         xhi[i] := min(ceil(fv),inx-1);
         xfh[i] := frac(fv);
         xfl[i] := 1.0 - xfh[i];
     end;
     //set scaled y
     setlength(ylo, ny);
     setlength(yhi, ny);
     setlength(yfl, ny);
     setlength(yfh, ny);
     f := iny/lHdr.dim[2];
     for i := 0 to (ny-1) do begin
     	 fv := max(((i+0.5)*f)-0.5, 0); //fv := f * i;
     	 ylo[i] := trunc(fv) * inx;
         yhi[i] := min(ceil(fv),iny-1) * inx;
         yfh[i] := frac(fv);
         yfl[i] := 1.0 - yfh[i];
     end;
     //set scaled z
     setlength(zlo, nz);
     setlength(zhi, nz);
     setlength(zfl, nz);
     setlength(zfh, nz);
     f := inz/lHdr.dim[3];
     for i := 0 to (nz-1) do begin
     	 fv := max(((i+0.5)*f)-0.5, 0); //fv := f * i;
     	 zlo[i] := trunc(fv) * inxy;
         zhi[i] := min(ceil(fv),inz-1) * inxy ;
         zfh[i] := frac(fv);
         zfl[i] := 1.0 - zfh[i];
     end;
     //4th dimension (volume) is not scaled
     setlength(vo, nv);
     for i := 0 to (nv-1) do
     	 vo[i] := i * inx * iny * inz;
     //fill output
     i := 0;
  	 //if (lHdr.bitpix = 16) then begin
     	in16 := TInt16s(lBuffer);
        out16 := TInt16s(out8);
        for v := 0 to (nv-1) do
            for z := 0 to (nz-1) do
                for y := 0 to (ny-1) do begin
                    for x := 0 to (nx-1) do begin
                  	   out16[i] := round(
                                 (xfl[x]*yfl[y]*zfl[z]*in16[xlo[x]+ylo[y]+zlo[z]+vo[v]]){---}
                                +(xfl[x]*yfl[y]*zfh[z]*in16[xlo[x]+ylo[y]+zhi[z]+vo[v]]){--+}
                                +(xfl[x]*yfh[y]*zfl[z]*in16[xlo[x]+yhi[y]+zlo[z]+vo[v]]){-+-}
                                +(xfl[x]*yfh[y]*zfh[z]*in16[xlo[x]+yhi[y]+zhi[z]+vo[v]]){-++}
                      			+(xfh[x]*yfl[y]*zfl[z]*in16[xhi[x]+ylo[y]+zlo[z]+vo[v]]){+--}
                                +(xfh[x]*yfl[y]*zfh[z]*in16[xhi[x]+ylo[y]+zhi[z]+vo[v]]){+-+}
                                +(xfh[x]*yfh[y]*zfl[z]*in16[xhi[x]+yhi[y]+zlo[z]+vo[v]]){++-}
                                +(xfh[x]*yfh[y]*zfh[z]*in16[xhi[x]+yhi[y]+zhi[z]+vo[v]]){+++}
                       );
                        i += 1;
                    end;
                end;
     //end;
     setlength( lBuffer, 0);
     lBuffer := out8;
end;

procedure ShrinkFastLinearU16(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; lScale: single);
var
   out8: TUInt8s;
   out16, in16: TUInt16s;
   //out24, in24: TRGBs;
   //out32, in32: TInt32s;
   x,y,z,v, nx,ny,nz, nv, inx, iny, inz, inxy: integer;
   xlo,ylo,zlo,xhi,yhi,zhi, vo: array of integer;
   xfl,yfl,zfl,xfh,yfh,zfh: array of single;
   i: int64;
   fv, f: single;
begin
     inx := lHdr.dim[1];
     iny := lHdr.dim[2];
     inz := lHdr.dim[3];
     inxy := inx * iny;
     Zoom(lHdr,lScale, lScale, lScale);
     nx := max(lHdr.dim[1], 1);
     ny := max(lHdr.dim[2], 1);
     nz := max(lHdr.dim[3], 1);
     nv := max(lHdr.dim[4], 1) * max(lHdr.dim[5], 1) * max(lHdr.dim[6], 1) * max(lHdr.dim[7], 1);
     setlength(out8, nx*ny*nz*nv*(lHdr.bitpix div 8));
     //set scaled x
     setlength(xlo, nx);
     setlength(xhi, nx);
     setlength(xfl, nx);
     setlength(xfh, nx);
     f := inx/lHdr.dim[1];
     for i := 0 to (nx-1) do begin
     	 fv := max(((i+0.5)*f)-0.5, 0); //fv := f * i;
     	 xlo[i] := trunc(fv);
         xhi[i] := min(ceil(fv),inx-1);
         xfh[i] := frac(fv);
         xfl[i] := 1.0 - xfh[i];
     end;
     //set scaled y
     setlength(ylo, ny);
     setlength(yhi, ny);
     setlength(yfl, ny);
     setlength(yfh, ny);
     f := iny/lHdr.dim[2];
     for i := 0 to (ny-1) do begin
     	 fv := max(((i+0.5)*f)-0.5, 0); //fv := f * i;
     	 ylo[i] := trunc(fv) * inx;
         yhi[i] := min(ceil(fv),iny-1) * inx;
         yfh[i] := frac(fv);
         yfl[i] := 1.0 - yfh[i];
     end;
     //set scaled z
     setlength(zlo, nz);
     setlength(zhi, nz);
     setlength(zfl, nz);
     setlength(zfh, nz);
     f := inz/lHdr.dim[3];
     for i := 0 to (nz-1) do begin
     	 fv := max(((i+0.5)*f)-0.5, 0); //fv := f * i;
     	 zlo[i] := trunc(fv) * inxy;
         zhi[i] := min(ceil(fv),inz-1) * inxy ;
         zfh[i] := frac(fv);
         zfl[i] := 1.0 - zfh[i];
     end;
     //4th dimension (volume) is not scaled
     setlength(vo, nv);
     for i := 0 to (nv-1) do
     	 vo[i] := i * inx * iny * inz;
     //fill output
     i := 0;
  	 //if (lHdr.bitpix = 16) then begin
     	in16 := TUInt16s(lBuffer);
        out16 := TUInt16s(out8);
        for v := 0 to (nv-1) do
            for z := 0 to (nz-1) do
                for y := 0 to (ny-1) do begin
                    for x := 0 to (nx-1) do begin
                  	   out16[i] := round(
                                 (xfl[x]*yfl[y]*zfl[z]*in16[xlo[x]+ylo[y]+zlo[z]+vo[v]]){---}
                                +(xfl[x]*yfl[y]*zfh[z]*in16[xlo[x]+ylo[y]+zhi[z]+vo[v]]){--+}
                                +(xfl[x]*yfh[y]*zfl[z]*in16[xlo[x]+yhi[y]+zlo[z]+vo[v]]){-+-}
                                +(xfl[x]*yfh[y]*zfh[z]*in16[xlo[x]+yhi[y]+zhi[z]+vo[v]]){-++}
                      			+(xfh[x]*yfl[y]*zfl[z]*in16[xhi[x]+ylo[y]+zlo[z]+vo[v]]){+--}
                                +(xfh[x]*yfl[y]*zfh[z]*in16[xhi[x]+ylo[y]+zhi[z]+vo[v]]){+-+}
                                +(xfh[x]*yfh[y]*zfl[z]*in16[xhi[x]+yhi[y]+zlo[z]+vo[v]]){++-}
                                +(xfh[x]*yfh[y]*zfh[z]*in16[xhi[x]+yhi[y]+zhi[z]+vo[v]]){+++}
                       );
                        i += 1;
                    end;
                end;
     //end;
     setlength( lBuffer, 0);
     lBuffer := out8;
end;

procedure ShrinkFastLinearU8(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; lScale: single);
var
   in8, out8: TUInt8s;
   x,y,z,v, nx,ny,nz, nv, inx, iny, inz, inxy: integer;
   xlo,ylo,zlo,xhi,yhi,zhi, vo: array of integer;
   xfl,yfl,zfl,xfh,yfh,zfh: array of single;
   i: int64;
   fv, f: single;
begin
     inx := lHdr.dim[1];
     iny := lHdr.dim[2];
     inz := lHdr.dim[3];
     inxy := inx * iny;
     Zoom(lHdr,lScale, lScale, lScale);
     nx := max(lHdr.dim[1], 1);
     ny := max(lHdr.dim[2], 1);
     nz := max(lHdr.dim[3], 1);
     nv := max(lHdr.dim[4], 1) * max(lHdr.dim[5], 1) * max(lHdr.dim[6], 1) * max(lHdr.dim[7], 1);
     setlength(out8, nx*ny*nz*nv*(lHdr.bitpix div 8));
     //set scaled x
     setlength(xlo, nx);
     setlength(xhi, nx);
     setlength(xfl, nx);
     setlength(xfh, nx);
     f := inx/lHdr.dim[1];
     for i := 0 to (nx-1) do begin
     	 fv := max(((i+0.5)*f)-0.5, 0); //fv := f * i;
     	 xlo[i] := trunc(fv);
         xhi[i] := min(ceil(fv),inx-1);
         xfh[i] := frac(fv);
         xfl[i] := 1.0 - xfh[i];
     end;
     //set scaled y
     setlength(ylo, ny);
     setlength(yhi, ny);
     setlength(yfl, ny);
     setlength(yfh, ny);
     f := iny/lHdr.dim[2];
     for i := 0 to (ny-1) do begin
     	 fv := max(((i+0.5)*f)-0.5, 0); //fv := f * i;
     	 ylo[i] := trunc(fv) * inx;
         yhi[i] := min(ceil(fv),iny-1) * inx;
         yfh[i] := frac(fv);
         yfl[i] := 1.0 - yfh[i];
     end;
     //set scaled z
     setlength(zlo, nz);
     setlength(zhi, nz);
     setlength(zfl, nz);
     setlength(zfh, nz);
     f := inz/lHdr.dim[3];
     for i := 0 to (nz-1) do begin
     	 fv := max(((i+0.5)*f)-0.5, 0); //fv := f * i;
     	 zlo[i] := trunc(fv) * inxy;
         zhi[i] := min(ceil(fv),inz-1) * inxy ;
         zfh[i] := frac(fv);
         zfl[i] := 1.0 - zfh[i];
     end;
     //4th dimension (volume) is not scaled
     setlength(vo, nv);
     for i := 0 to (nv-1) do
     	 vo[i] := i * inx * iny * inz;
     //fill output
     i := 0;
    in8 := TUInt8s(lBuffer);
    for v := 0 to (nv-1) do
        for z := 0 to (nz-1) do
            for y := 0 to (ny-1) do begin
                for x := 0 to (nx-1) do begin
                   out8[i] := round(
                             (xfl[x]*yfl[y]*zfl[z]*in8[xlo[x]+ylo[y]+zlo[z]+vo[v]]){---}
                            +(xfl[x]*yfl[y]*zfh[z]*in8[xlo[x]+ylo[y]+zhi[z]+vo[v]]){--+}
                            +(xfl[x]*yfh[y]*zfl[z]*in8[xlo[x]+yhi[y]+zlo[z]+vo[v]]){-+-}
                            +(xfl[x]*yfh[y]*zfh[z]*in8[xlo[x]+yhi[y]+zhi[z]+vo[v]]){-++}
                      		+(xfh[x]*yfl[y]*zfl[z]*in8[xhi[x]+ylo[y]+zlo[z]+vo[v]]){+--}
                            +(xfh[x]*yfl[y]*zfh[z]*in8[xhi[x]+ylo[y]+zhi[z]+vo[v]]){+-+}
                            +(xfh[x]*yfh[y]*zfl[z]*in8[xhi[x]+yhi[y]+zlo[z]+vo[v]]){++-}
                            +(xfh[x]*yfh[y]*zfh[z]*in8[xhi[x]+yhi[y]+zhi[z]+vo[v]]){+++}
                   );
                    i += 1;
                end;
            end;
     //for i := 0 to ((nx * ny * nz)-1) do out8[i] := random(123);
     setlength( lBuffer, 0);
     lBuffer := out8;
end; //U8

procedure ShrinkFastLinearF32(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; lScale: single);
var
   out8: TUInt8s;
   out32, in32: TFloat32s;
   x,y,z,v, nx,ny,nz, nv, inx, iny, inz, inxy: integer;
   xlo,ylo,zlo,xhi,yhi,zhi, vo: array of integer;
   xfl,yfl,zfl,xfh,yfh,zfh: array of single;
   i: int64;
   fv, f: single;
begin
     inx := lHdr.dim[1];
     iny := lHdr.dim[2];
     inz := lHdr.dim[3];
     inxy := inx * iny;
     Zoom(lHdr,lScale, lScale, lScale);
     nx := max(lHdr.dim[1], 1);
     ny := max(lHdr.dim[2], 1);
     nz := max(lHdr.dim[3], 1);
     nv := max(lHdr.dim[4], 1) * max(lHdr.dim[5], 1) * max(lHdr.dim[6], 1) * max(lHdr.dim[7], 1);
     setlength(out8, nx*ny*nz*nv*(lHdr.bitpix div 8));
     //set scaled x
     setlength(xlo, nx);
     setlength(xhi, nx);
     setlength(xfl, nx);
     setlength(xfh, nx);
     f := inx/lHdr.dim[1];
     for i := 0 to (nx-1) do begin
     	 fv := max(((i+0.5)*f)-0.5, 0); //fv := f * i;
     	 xlo[i] := trunc(fv);
         xhi[i] := min(ceil(fv),inx-1);
         xfh[i] := frac(fv);
         xfl[i] := 1.0 - xfh[i];
     end;
     //set scaled y
     setlength(ylo, ny);
     setlength(yhi, ny);
     setlength(yfl, ny);
     setlength(yfh, ny);
     f := iny/lHdr.dim[2];
     for i := 0 to (ny-1) do begin
     	 fv := max(((i+0.5)*f)-0.5, 0); //fv := f * i;
     	 ylo[i] := trunc(fv) * inx;
         yhi[i] := min(ceil(fv),iny-1) * inx;
         yfh[i] := frac(fv);
         yfl[i] := 1.0 - yfh[i];
     end;
     //set scaled z
     setlength(zlo, nz);
     setlength(zhi, nz);
     setlength(zfl, nz);
     setlength(zfh, nz);
     f := inz/lHdr.dim[3];
     for i := 0 to (nz-1) do begin
     	 fv := max(((i+0.5)*f)-0.5, 0); //fv := f * i;
     	 zlo[i] := trunc(fv) * inxy;
         zhi[i] := min(ceil(fv),inz-1) * inxy ;
         zfh[i] := frac(fv);
         zfl[i] := 1.0 - zfh[i];
     end;
     //4th dimension (volume) is not scaled
     setlength(vo, nv);
     for i := 0 to (nv-1) do
     	 vo[i] := i * inx * iny * inz;
     //fill output
     i := 0;
  	 //if (lHdr.bitpix = 16) then begin
     	in32 := TFloat32s(lBuffer);
        out32 := TFloat32s(out8);
        for v := 0 to (nv-1) do
            for z := 0 to (nz-1) do
                for y := 0 to (ny-1) do begin
                    for x := 0 to (nx-1) do begin
                  	   out32[i] :=
                                 (xfl[x]*yfl[y]*zfl[z]*in32[xlo[x]+ylo[y]+zlo[z]+vo[v]]){---}
                                +(xfl[x]*yfl[y]*zfh[z]*in32[xlo[x]+ylo[y]+zhi[z]+vo[v]]){--+}
                                +(xfl[x]*yfh[y]*zfl[z]*in32[xlo[x]+yhi[y]+zlo[z]+vo[v]]){-+-}
                                +(xfl[x]*yfh[y]*zfh[z]*in32[xlo[x]+yhi[y]+zhi[z]+vo[v]]){-++}
                      			+(xfh[x]*yfl[y]*zfl[z]*in32[xhi[x]+ylo[y]+zlo[z]+vo[v]]){+--}
                                +(xfh[x]*yfl[y]*zfh[z]*in32[xhi[x]+ylo[y]+zhi[z]+vo[v]]){+-+}
                                +(xfh[x]*yfh[y]*zfl[z]*in32[xhi[x]+yhi[y]+zlo[z]+vo[v]]){++-}
                                +(xfh[x]*yfh[y]*zfh[z]*in32[xhi[x]+yhi[y]+zhi[z]+vo[v]]){+++}
                       ;
                        i += 1;
                    end;
                end;
     //end;
     setlength( lBuffer, 0);
     lBuffer := out8;
end;



procedure ShrinkFastLinear(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; lScale: single);
begin
	 if lHdr.datatype = kDT_UINT8 then
     	ShrinkFastLinearU8(lHdr, lBuffer, lScale)
	 else if lHdr.datatype = kDT_UINT16 then
     	ShrinkFastLinearU16(lHdr, lBuffer, lScale)
     else if lHdr.datatype = kDT_INT16 then
     	ShrinkFastLinear16(lHdr, lBuffer, lScale)
     else if lHdr.datatype = kDT_FLOAT then
     	ShrinkFastLinearF32(lHdr, lBuffer, lScale)
     else //e.g. kDT_RGB use slow method
     	 ShrinkOrEnlarge(lHdr,lBuffer, 1, lScale);
end;

procedure ShrinkFastNearest(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; lScale: single);
var
   out8: TUInt8s;
   out16, in16: TInt16s;
   out24, in24: TRGBs;
   out32, in32: TInt32s;
   x,y,z,v, nx,ny,nz, nv, inx, iny, inz, sl: integer;
   xo,yo,zo, vo: array of integer;
   i: int64;
   f: double;
begin
	 if (lHdr.datatype <> kDT_UINT8) and (lHdr.datatype <> kDT_INT8)
        and (lHdr.datatype <> kDT_UINT16) and (lHdr.datatype <> kDT_INT16)
        and (lHdr.datatype <> kDT_UINT32) and (lHdr.datatype <> kDT_INT32)
        and (lHdr.datatype <> kDT_FLOAT) and (lHdr.datatype <> kDT_RGB) then begin
        	ShrinkOrEnlarge(lHdr,lBuffer, 0, lScale);
        	exit;
     end;
     inx := lHdr.dim[1];
     iny := lHdr.dim[2];
     inz := lHdr.dim[3];
     Zoom(lHdr,lScale, lScale, lScale);
     nx := max(lHdr.dim[1], 1);
     ny := max(lHdr.dim[2], 1);
     nz := max(lHdr.dim[3], 1);
     nv := max(lHdr.dim[4], 1) * max(lHdr.dim[5], 1) * max(lHdr.dim[6], 1) * max(lHdr.dim[7], 1);
     setlength(out8, nx*ny*nz*nv*(lHdr.bitpix div 8));
     //set scaled x
     setlength(xo, nx);
     f := inx/lHdr.dim[1];
     for i := 0 to (nx-1) do
     	 xo[i] := round(f * i);
     //set scaled y
     setlength(yo, ny);
     f := iny/lHdr.dim[2];
     for i := 0 to (ny-1) do
     	 yo[i] := round(f * i) * inx;
     //set scaled z
     setlength(zo, nz);
     f := inz/lHdr.dim[3];
     for i := 0 to (nz-1) do
     	 zo[i] := round(f * i) * inx * iny;
     //4th dimension (volume) is not scaled
     setlength(vo, nv);
     for i := 0 to (nv-1) do
     	 vo[i] := i * inx * iny * inz;
     //fill output
     i := 0;
  	 if (lHdr.bitpix = 32) then begin
        in32 := TInt32s(lBuffer);
        out32 := TInt32s(out8);
        for v := 0 to (nv-1) do
           for z := 0 to (nz-1) do
               for y := 0 to (ny-1) do begin
                     sl := yo[y]+zo[z]+vo[v];
                   for x := 0 to (nx-1) do begin
                     out32[i] := in32[xo[x]+sl];
                       i += 1;
                   end;
               end;
      end else if (lHdr.bitpix = 24) then begin
     	in24 := TRGBs(lBuffer);
        out24 := TRGBs(out8);
        for v := 0 to (nv-1) do
            for z := 0 to (nz-1) do
                for y := 0 to (ny-1) do begin
                	   sl := yo[y]+zo[z]+vo[v];
                    for x := 0 to (nx-1) do begin
                  	   out24[i] := in24[xo[x]+sl];
                        i += 1;
                    end;
                end;
       end else if (lHdr.bitpix = 16) then begin
     	in16 := TInt16s(lBuffer);
        out16 := TInt16s(out8);
        for v := 0 to (nv-1) do
            for z := 0 to (nz-1) do
                for y := 0 to (ny-1) do begin
                	   sl := yo[y]+zo[z]+vo[v];
                    for x := 0 to (nx-1) do begin
                  	   out16[i] := in16[xo[x]+sl];
                        i += 1;
                    end;
                end;
     end else begin
       for v := 0 to (nv-1) do
           for z := 0 to (nz-1) do
               for y := 0 to (ny-1) do begin
               	   sl := yo[y]+zo[z]+vo[v];
                   for x := 0 to (nx-1) do begin
                 	   out8[i] := lBuffer[xo[x]+sl];
                       i += 1;
                   end;
               end;
     end;
     setlength( lBuffer, 0);
     lBuffer := out8;
end;

function ShrinkLargeMb(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; lMaxTexMb: integer; isAntiAlias, isLabels: boolean): boolean;
var
   scale: double;
   {$IFDEF UNIX}StartTime: TDateTime;{$ENDIF}
begin
  scale := ScaleToMb(lHdr, lMaxTexMb);
  if scale >= 1.0 then exit(false);
  {$IFDEF UNIX}
  writeln(format('Dimensions (%dx%dx%d) exceeds MaxTexMb (%d): resizing x%.3g', [lHdr.dim[1], lHdr.dim[2], lHdr.dim[3], lMaxTexMb, scale]));
  startTime := now;
  {$ENDIF}
  if isAntiAlias then begin
     if isLabels then
        result := ShrinkOrEnlarge(lHdr,lBuffer, 0, scale)
     else
     	 result := ShrinkOrEnlarge(lHdr,lBuffer, -1, scale);
     exit;
  end;
  if isLabels then
     //result := ShrinkOrEnlarge(lHdr,lBuffer, 0, scale)
  	 ShrinkFastNearest(lHdr,lBuffer, scale)
  else
      //result := ShrinkOrEnlarge(lHdr,lBuffer, -1, scale);
      ShrinkFastLinear(lHdr,lBuffer, scale);
  {$IFDEF UNIX}
  writeln(format('Resizing (%dx%dx%d) time %d',[lHdr.dim[1], lHdr.dim[2], lHdr.dim[3], MilliSecondsBetween(Now,startTime)]));
  {$ENDIF}
  result := true;
end;

(*function ShrinkLargeMb(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; lMaxTexMb: integer; isLabels: boolean = false): boolean;
const
  kBytesPerMb = 1048576;
var
   mb: double;
   scaleVol, scale: double;
   i : integer;
   outdim: array [1..3] of integer;
   {$IFDEF UNIX}StartTime: TDateTime;{$ENDIF}
begin
  mb := (lHdr.dim[1] * lHdr.dim[2] * lHdr.dim[3] * 4)/kBytesPerMb; //RGBA8 is 4 bytes per voxel
  //scale :=  power(scaleVol, 1/3);
  if (mb < lMaxTexMb) or (lMaxTexMb < 1) then exit(false);
  scaleVol := lMaxTexMb/mb;
  scale :=  exp(ln(scaleVol) / 3); //power(scaleVol, 1/3);
  writeln(format('-->> %g mb, max %dmb scaleVox %.2f scale %.2f',[mb, lMaxTexMb, scaleVol, scale]));
  for i := 1 to 3 do begin
      outdim[i] := round(lHdr.dim[i] * scale);
      outdim[i] := max(outdim[i],1);
  end;
  mb := (outdim[1] * outdim[2] * outdim[3] * 4)/kBytesPerMb; //RGBA8 is 4 bytes per voxel
  writeln(format('++>> %g mb',[mb]));
  if (mb > lMaxTexMb) then begin //round down, not up
     scale := (max(outdim[1],max(outdim[2],outdim[3]))-1)/max(lHdr.dim[1],max(lHdr.dim[2],lHdr.dim[3]));
  end;
  writeln(format('-->> %g mb, max %dmb scaleVox %.2f scale %.2f',[mb, lMaxTexMb, scaleVol, scale]));
  {$IFDEF UNIX}
  writeln(format('Dimensions (%dx%dx%d) exceeds MaxTexMb (%d): resizing x%.3g', [lHdr.dim[1], lHdr.dim[2], lHdr.dim[3], lMaxTexMb, scale]));
  startTime := now;
  {$ENDIF}
  if isLabels then
     result := ShrinkOrEnlarge(lHdr,lBuffer, 0, scale)
  else
      result := ShrinkOrEnlarge(lHdr,lBuffer, -1, scale);
  {$IFDEF UNIX}
  writeln(format('Resizing (%dx%dx%d) time %d',[lHdr.dim[1], lHdr.dim[2], lHdr.dim[3], MilliSecondsBetween(Now,startTime)]));
  {$ENDIF}
end;*)

function ShrinkLarge(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; lMaxDim: integer; isLabels: boolean = false): boolean;
//rescales images with any dimension larger than lMaxDim to have a maximum dimension of maxdim...
var
   imx: integer;
   scale: single;
   {$IFDEF UNIX}StartTime: TDateTime;{$ENDIF}
begin
  imx := max(max(lHdr.dim[1], lHdr.dim[2]), lHdr.dim[3]);
  if (imx <= lMaxDim) or (lMaxDim < 1) then exit(false); //false is critical - see: if ShrinkLarge(fHdr,fRawVolBytes, MaxVox) then fVolumesLoaded := 1;
  scale := lMaxDim/imx;
  {$IFDEF UNIX}
  writeln(format('Dimensions (%dx%dx%d) exceeds MaxVox (%d): resizing x%.3g', [lHdr.dim[1], lHdr.dim[2], lHdr.dim[3], lMaxDim, scale]));
  startTime := now;
  {$ENDIF}
  if isLabels then
     result := ShrinkOrEnlarge(lHdr,lBuffer, 0, scale)
  else
      result := ShrinkOrEnlarge(lHdr,lBuffer, -1, scale);
  {$IFDEF UNIX}
  writeln(format('Resizing (%dx%dx%d) time %d',[lHdr.dim[1], lHdr.dim[2], lHdr.dim[3], MilliSecondsBetween(Now,startTime)]));
  {$ENDIF}
end;

(*function EnlargeIsotropic(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s): boolean;
var
  mmMx, mmMn, xScale, yScale, zScale: single;
begin
 result := false;
 mmMx := max(max(lHdr.pixdim[1],lHdr.pixdim[2]),lHdr.pixdim[3]);
 mmMn := min(min(lHdr.pixdim[1],lHdr.pixdim[2]),lHdr.pixdim[3]);
 //if (mmMn = 0) or (specialsingle(mmMn)) or (specialsingle(mmMx)) or (mmMn = mmMx) then exit;
 if (mmMn = 0) or (mmMn = mmMx) then exit;
 xScale :=  lHdr.pixdim[1]/ mmMn;
 yScale :=  lHdr.pixdim[2]/ mmMn;
 zScale :=  lHdr.pixdim[3]/ mmMn;
 result := ShrinkOrEnlarge(lHdr,lBuffer, -1, xScale, yScale, zScale);
end;*)

procedure forceDataType(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; const outDatatype: integer);
var
   vx, i: int64;
   i16 : TInt16s;
   f32, in32 : TFloat32s;
   scale, mn, mx, dtMn, dtMx, dtRng, rng : double;
begin
     if lHdr.datatype <> kDT_FLOAT then showmessage('Catastrophic error: input data not float32');
     if outDatatype = lHdr.datatype then exit; //already in correct format
     vx := lHdr.dim[1] * lHdr.dim[2] * lHdr.dim[3];
     lHdr.datatype := outDatatype;
     setlength(f32, vx);
     in32 := TFloat32s(lBuffer);
     mn := in32[0];
     mx := in32[0];
     for i := 0 to (vx-1) do begin
         f32[i] := in32[i];
         if (f32[i] < mn) then mn := f32[i];
         if (f32[i] > mx) then mx := f32[i];
     end;
     scale := 1.0;
     dtMn := 0;
     dtMx := 255;
     dtRng := dtMx - dtMn;
     if lHdr.datatype = kDT_UNSIGNED_CHAR then begin
        lHdr.bitpix := 8;
        setlength(lBuffer, vx);
        if mx > mn then
           scale := dtRng/(mx-mn);
        for i := 0 to vx -1 do
            lBuffer[i] := round( (f32[i]-mn) * scale );
     end else if outDatatype = kDT_SIGNED_SHORT then begin
         if mn < 0.0 then
            //showmessage(
            dtMn := -32767 //true dtMinimum is -32768: make 0 in center
         else
             dtMn := 0;
         dtMx := 32767;
         dtRng := dtMx - dtMn;
         lHdr.bitpix := 16;
         setlength(lBuffer, vx * 2);
         //showmessage(format('%d %d %d -> %d', [lHdr.dim[1], lHdr.dim[2], lHdr.dim[3],  length(lBuffer)]));
         i16 := TInt16s(lBuffer);
         //lHdr.scl_inter := mn;
         //lHdr.scl_slope := ((mx-mn)/32767);
         if mx > mn then
            scale := dtRng/(mx-mn);
         for i := 0 to vx -1 do
             i16[i] := round( (f32[i]-mn) * scale + dtMn) ;
     end else
         showmessage('Catastrophic error: unsupported output format');
     f32 := nil;
     //compute new scale slope and intercept: output data scaled to dtMin..dtMax
     if lHdr.scl_slope = 0 then lHdr.scl_slope := 1;
     mn := (mn * lHdr.scl_slope) + lHdr.scl_inter;
     mx := (mx * lHdr.scl_slope) + lHdr.scl_inter;
     rng := mx - mn;
     dtRng := dtMx - dtMn;
     if (rng <= 0) or (dtRng <= 0) then exit;
     lHdr.scl_slope := rng / dtRng;
     lHdr.scl_inter := (-dtMn* lHdr.scl_slope)+mn;
end;

function forceFloat32(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s): boolean;
var
   vx, i: int64;
   i8 : TUInt8s;
   i16 : TInt16s;
   o32 : TFloat32s;
begin
  if lHdr.datatype = kDT_FLOAT then exit(true);
  if (lHdr.datatype <> kDT_UNSIGNED_CHAR) and (lHdr.datatype <> kDT_SIGNED_SHORT) then begin
     {$IFDEF UNIX}
     writeln(format('Warning: Unsupported data type for forced resizing: %d', [lHdr.datatype]));
     {$ENDIF}
     exit(false);
  end;
  vx := lHdr.dim[1] * lHdr.dim[2] * lHdr.dim[3];
  setlength(i8, length(lBuffer));
  i8 := copy(lBuffer, 0, maxint);
  i16 := TInt16s(i8);
  setlength(lBuffer, vx * 4);
  o32 := TFloat32s(lBuffer);
  if lHdr.datatype = kDT_UNSIGNED_CHAR then begin
    for i := 0 to vx-1 do
        o32[i] := i8[i];
  end else begin   //kDT_SIGNED_SHORT
   for i := 0 to vx-1 do
       o32[i] := i16[i];
  end;
  i8 := nil;
  lHdr.datatype := kDT_FLOAT; //lBuffer now has type float
  lHdr.bitpix := 32;
  exit(true);
end;

function ShrinkOrEnlarge(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; lFilter: integer; lScale: single): boolean; overload;
begin
  result := ShrinkOrEnlarge(lHdr, lBuffer, lFilter, lScale, lScale, lScale);
end;

function ShrinkOrEnlargeRGB(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; lFilter: integer; lScaleX, lScaleY, lScaleZ: single): boolean; overload;
var
   fwidth, z : single;
   filter : TFilterProc;
   lHdr8: TNIFTIhdr;
   c, i, j, vxIn, vxOut: int64;
   lBuffer8: array [0..2] of TUInt8s;
begin
     result := false;
     if (lScaleX <= 0.0) or (lScaleY <= 0.0) or (lScaleZ <= 0.0) then exit;
     if (lScaleX = 1.0) and (lScaleY = 1.0) and (lScaleZ = 1.0) then exit;
     z := lScaleX * lScaleY * lScaleZ;
     if ((lFilter < 0) or (lFilter > 6)) and (z < 1) then
        lFilter := 5; //Lanczos nice for downsampling
     if ((lFilter < 0) or (lFilter > 6))  and (z >= 1) then
        lFilter := 6; //Mitchell nice for upsampling
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
     result := true;
     vxIn := lHdr.dim[1] * lHdr.dim[2] * lHdr.dim[3];
     //load 8 bit buffers
     for c := 0 to 2 do begin //channel R,G,B
         setlength(lBuffer8[c], vxIn);
         j := 0;
         for i := 0 to (vxIn-1) do begin
             lBuffer8[c][i] := lBuffer[j+c];
             j := j + 3;
         end;
     end;
     lBuffer := nil; //free;
     //resize each color channel
     for c := 0 to 2 do begin //channel R,G,B
         lHdr8 := lHdr;
         lHdr8.datatype := kDT_UNSIGNED_CHAR;
         lHdr8.bitpix := 8;
         Resize8(lHdr8, lBuffer8[c], lScaleX, lScaleY, lScaleZ, fwidth, @filter);
         if c = 0 then begin
           //vxOut := length(lBuffer8[c]);
           vxOut := lHdr8.dim[1] * lHdr8.dim[2] * lHdr8.dim[3];
           setlength(lBuffer, vxOut * 3);
         end;
         j := 0;
         for i := 0 to (vxOut-1) do begin
             lBuffer[j+c] := lBuffer8[c][i];
             j := j + 3;
         end;
     end;
     //showmessage(format('%d %d %d = %d', [lHdr8.dim[1], lHdr8.dim[2], lHdr8.dim[3], vxOut]));
     //free color channels
     for c := 0 to 2 do
         lBuffer8[c] := nil;
     //reset header
     lHdr := lHdr8;
     lHdr.datatype := kDT_RGB;
     lHdr.bitpix := 24;
end;

function ShrinkOrEnlarge(var lHdr: TNIFTIhdr; var lBuffer: TUInt8s; lFilter: integer; lScaleX, lScaleY, lScaleZ: single; outDatatype : integer = -1): boolean; overload;
var
   fwidth, z : single;
   filter : TFilterProc;
begin
  result := false;
  if (lScaleX <= 0.0) or (lScaleY <= 0.0) or (lScaleZ <= 0.0) then exit;
  if (lScaleX = 1.0) and (lScaleY = 1.0) and (lScaleZ = 1.0) and (outDatatype = lHdr.datatype) then exit; //no resize
  if (lHdr.datatype = kDT_RGB) then begin
     result := ShrinkOrEnlargeRGB(lHdr, lBuffer, lFilter, lScaleX, lScaleY, lScaleZ);
     exit;
  end;
  if (lScaleX = 1.0) and (lScaleY = 1.0) and (lScaleZ = 1.0) then begin //change datatype only
    forceFloat32(lHdr, lBuffer);
    forceDataType(lHdr, lBuffer, outDatatype);
    exit (true);
  end;
  z := lScaleX * lScaleY * lScaleZ;
  if ((lFilter < 0) or (lFilter > 6)) and (z < 1) then
     lFilter := 5; //Lanczos nice for downsampling
  if ((lFilter < 0) or (lFilter > 6))  and (z > 1) then
     lFilter := 6; //Mitchell nice for upsampling
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
  result := true;
  //showmessage(format('%d %g', [lFilter, fwidth])); exit;
  if (outDatatype < 0) or (outDatatype = lHdr.datatype) then begin
     if lHdr.datatype = kDT_UNSIGNED_CHAR then
       Resize8(lHdr, lBuffer, lScaleX, lScaleY, lScaleZ, fwidth, @filter)
    else if lHdr.datatype = kDT_UINT16 then
       ResizeU16(lHdr, lBuffer, lScaleX, lScaleY, lScaleZ, fwidth, @filter)
    else if lHdr.datatype = kDT_SIGNED_SHORT then
       Resize16(lHdr, lBuffer, lScaleX, lScaleY, lScaleZ, fwidth, @filter)
    else if lHdr.datatype = kDT_FLOAT then
       Resize32(lHdr, TUInt8s(lBuffer), lScaleX, lScaleY, lScaleZ, fwidth, @filter)
    else begin
       {$IFDEF UNIX}
       writeln(format('Warning: Unsupported data type for resizing: %d', [lHdr.datatype]));
       {$ENDIF}
        result := false;
    end;
  end else begin
      if not forceFloat32(lHdr, lBuffer) then exit;
      Resize32(lHdr, (lBuffer), lScaleX, lScaleY, lScaleZ, fwidth, @filter);
      forceDataType(lHdr, lBuffer, outDatatype);
  end;
  //showmessage(format('%d %d %d', [lHdr.dim[1], lHdr.dim[2], lHdr.dim[3]]));
end;

end.


