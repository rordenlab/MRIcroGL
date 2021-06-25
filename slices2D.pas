unit slices2D;
//describes 3D volume as 2D slices for display
// library agnostic: call from OpenGL/Vulkan/Metal

{$mode delphi}{$H+}
interface
{$include opts.inc} //for  DEFINE MOSAICS

uses
  VectorMath, SimdUtils, Classes, SysUtils, dialogs, math, sdffont;
const //slice displays
  kEmptyOrient = 0;
  kAxialOrient = 1;
  kCoronalOrient = 2;
  kSagRightOrient = 4;
  kSagLeftOrient = 8;
  kAxCorSagOrient4 = 12;
  kAxCorSagOrient3 = 16;
  kMax2DOrient = kAxCorSagOrient3;
  kMosaicOrient = 32;
  kRenderOrient = 64;
type
  TVertex2D = record //vertex position/texture coordinates for drawing using GPU: six per bitmap
    position: TVec2;
    //metal requires we align each vertex attribute on 16 byte boundries
    // here these values do double duty to reveal location of mouse clicks
    unused: single;
    orient: int32;
    textureCoord: TVec4;
  end;
  TVertex2Ds = array of TVertex2D;
  {$IFDEF MOSAIC}
  TMosaicRender = record
  	Left,Bottom,Width,Height: single;
  	Orient: integer; //
  end;
  TMosaicRenders = array of TMosaicRender;
  {$ENDIF}
  //TSlices2DView = (Axial, Coronal, Sagittal, AxCorSag);
  TSlices2D = class
  private
    sliceVerts: TVertex2Ds;
    {$IFDEF MOSAIC}
    mosRenders: TMosaicRenders;
    {$ENDIF}
    numSliceVerts, numLineVerts, numMosRender: integer;
    lineVerts: TVertex2Ds;
    lineWid: single;
    lineXGap: single;
    lineClr: TVec4;
    fontScale,viewPixelHeight: single;
    //zoom: TZoom2D;
    fZoomScale: single;
    fZoomCenter: TVec3;
    sliceFrac2D: TVec3;
    txt: TSDFFont;
    newLines, newSlices, isRadiological, isLabelOrient: boolean;
    //procedure AddQuad(L,B, W,H, tL, tT, tR, TB, tZ: single; orient: integer);
    //procedure AddQuadX(L,B, W,H, tZ: single; orient: integer; m: TMat4);
    procedure AddQuad(L,B, W,H, tZ: single; iOrient: integer; m: TMat4);
    procedure DrawAx(L,B, W,H, ZFrac: single);
    procedure DrawCor(L,B, W,H, YFrac: single);
    procedure DrawSag(L,B, W,H, XFrac: single);
    procedure DrawTri(V1, V2, V3: TVec2; clr : TVec4);
    procedure DrawCropMask(L,B,W,H, tZ: single; orient: integer);
    procedure DrawArrow(L,B,W,H: single; orient: integer);
    //function Frac2Pix(frac: TVec3; L,B,W,H: single; orient: integer): TVec2;
    procedure DrawDistanceLine(L,B,W,H: single; iOrient: integer; tLT, tLB, tRB: TVec4);
    procedure DrawSagMirror(L,B, W,H, XFrac: single);
    procedure DrawLine(startX,startY,endX,endY: single);
    procedure DrawBar(left,bottom,width,height: single; clr : TVec4);
    procedure DrawLineLBWH(left,bottom,width,height: single);
    procedure DrawCross(L,B, W,H, Xfrac,Yfrac: single);
    procedure DrawCrossX(L,B, W,H, Xfrac,Yfrac: single);
    procedure DrawRender(L,B, W, H, Slice: single; Orient: integer);
    procedure TextLabelLeft(X,Y: single; Caption: string);
    procedure TextLabelTop(X,Y: single; Caption: string);
  public
    axCorSagOrient4XY: TVec3i;
    distanceLineStart, distanceLineEnd: TVec3;//TVertex2D;
    distanceLineOrient: integer;
    cropMask:TVec6;
    isOrientationTriangles: boolean;
    procedure DrawOutLine(L,T,R,B: single);
    property ZoomScale: single read fZoomScale write fZoomScale;
    property ZoomCenter: TVec3 read fZoomCenter write fZoomCenter;
    property RadiologicalConvention: boolean read isRadiological write isRadiological; //radiologists view from patient's feet (L on R)
    property LabelOrient: boolean read isLabelOrient write isLabelOrient;
    property NumberOfLineVertices : integer read numLineVerts;
    property HasNewLines: boolean read newLines write newLines; //flag indicates new lines for GP
    property HasNewSlices: boolean read newSlices write newSlices; //flag indicates new quads for GPU
    property LineVertices: TVertex2Ds read lineVerts;
    property LineWidth: single read lineWid write lineWid;
    property LineColor: TVec4 read lineClr write lineClr;
    property LineGap: single read lineXGap write lineXGap;
    property SliceFrac : TVec3 read sliceFrac2D write sliceFrac2D;
    property NumberOfVertices : integer read numSliceVerts;
    property NumberOfMosaicRenders : integer read numMosRender;
    property SliceVertices: TVertex2Ds read sliceVerts;
    {$IFDEF MOSAIC}
    property MosaicRenders: TMosaicRenders read mosRenders;
    {$ENDIF}
    constructor Create(sdffont: TSDFFont);
    destructor Destroy; override;
    //constructor Create();
    function Update(volScale: TVec3; w,h: single; Orient: integer; actualH : integer = -1): single;
    function GetSlice2DFrac2D(mouseX, mouseY: integer; var oOrient: integer): TVec3;
    function GetSlice2DMaxXY2D(mouseX, mouseY: integer; var Lo: TPoint): TPoint;
    function FracMM(Mat: TMat4; Dim: TVec3i; out Vox: TVec3i): TVec3; overload;
    function FracMM(inFrac: TVec3; Mat: TMat4; Dim: TVec3i): TVec3; overload;

    procedure SetFontColor(c: TVec4);
    {$IFDEF MOSAIC}
    procedure MosaicScale(lMosaicString: string; Mat, InvMat: TMat4; Dim: TVec3i; volScale: TVec3; out w, h: integer);
    procedure UpdateMosaic(lMosaicString: string; Mat, InvMat: TMat4; Dim: TVec3i; volScale: TVec3; w,h: single);
    {$ENDIF}
  end;

implementation

//uses mainunit;
const
  //kBlockSzQ = 256; //expand "quad2Ds" buffer by chunks of 256 quads
  kBlockSz = 256 * 6; //expand "sliceVerts" and "lineVerts" by chunks

procedure TSlices2D.SetFontColor(c: TVec4);
begin
  txt.FontColor := c;
  //TextColor := c;
end;

//{$DEFINE VX}
{$IFDEF VX}
function TSlices2D.GetSlice2DMaxXY2D(mouseX, mouseY: integer; var Lo: TPoint): TPoint;
begin
     result.X := 0;
     result.Y := 0;

end;
{$ELSE}
function TSlices2D.GetSlice2DMaxXY2D(mouseX, mouseY: integer; var Lo: TPoint): TPoint;
var
  xy1, xy2: TVec2;
  x, y: single;
  i: integer;
begin
  result.x := 0;
  result.y := 0;
  lo := result;
     //orient := -1;
     x := mouseX;
     y := viewPixelHeight - mouseY; //flip vertical axis: mouse coordinates y increase as we go down the screen
     i := 2;
     while (i < numSliceVerts) do begin
           xy1 := sliceVerts[i-1].position;
           xy2 := sliceVerts[i].position;
           i := i + 1;
           if (x < xy1.x) or (x > xy2.x) or (y < xy1.y) or (y > xy2.y) then continue;
           if (xy2.x <= xy1.x) or (xy2.y <= xy1.y)  then continue; //should never happen: avoid divide by zero
           lo.x := ceil(xy1.x);
           result.y := trunc(viewPixelHeight - xy1.y);
           result.x := trunc(xy2.x);
           lo.y := ceil(viewPixelHeight - xy2.y);
           exit;
     end;
end;
{$ENDIF}

//{$DEFINE AZ}
{$IFDEF AZ}

function TSlices2D.GetSlice2DFrac2D(mouseX, mouseY: integer; var oOrient: integer): TVec3;
begin
  result.x :=  0;
  result.y :=  0;
  result.z :=  0;
  oOrient := 0;
end;

{$ELSE}

function TSlices2D.GetSlice2DFrac2D(mouseX, mouseY: integer; var oOrient: integer): TVec3;
//each quad composed of two triangles = six vertices 0..5: position 1=L,B, position 2=R,T
// order 0=LT,1=LB,2=RT,3=RB,4=RT,5=LB
var
  lt,lb,rb: TVertex2D;
  up, right: TVec4;
  x, y, xFrac, yFrac: single;
  i: integer;
begin
     oOrient := -1;
     x := mouseX;
     y := viewPixelHeight - mouseY; //flip vertical axis: mouse coordinates y increase as we go down the screen
     result := Vec3(-1,-1,-1); //assume no hit
     i := 3;
     while (i < numSliceVerts) do begin
           lt := sliceVerts[i-3];
           lb := sliceVerts[i-2];
           //v2 := sliceVerts[i-1];
           rb := sliceVerts[i];
           i := i + 6; //skip to next quad: each quad composed of two triangles = 6 vertices
           xFrac := (x-lb.position.x)/ (rb.position.x - lb.position.x);
           if (xFrac < 0) or (xFrac > 1) then continue;
           yFrac := (y-lb.position.y)/ (lt.position.y - lb.position.y);
           if (yFrac < 0) or (yFrac > 1) then continue;
           up := lt.textureCoord - lb.textureCoord;
           right := rb.textureCoord - lb.textureCoord;
           result.x :=  lb.textureCoord.x + xFrac * right.x + yFrac * up.x;
           result.y :=  lb.textureCoord.y + xFrac * right.y + yFrac * up.y;
           result.z :=  lb.textureCoord.z + xFrac * right.z + yFrac * up.z;
           oOrient := lb.orient;
           exit;
     end;
end;
{$ENDIF}

  function Vertex2D(constref position: TVec2; constref coord: TVec4; orient: int32): TVertex2D;
  begin
    result.position := position; //pixel position X,Y
    //result.texDepth := texDepth; //used for mouse click to give 3rd dimension
    result.orient := orient;
    result.textureCoord := coord;
  end;

constructor TSlices2D.Create(sdffont: TSDFFont);
//constructor TSlices2D.Create();
begin
  distanceLineOrient := 0;
  sliceFrac2D := Vec3(0.5, 0.5, 0.5);
  txt := sdffont;
  fontScale := 1;
  fZoomScale := 1;
  isOrientationTriangles := false;
  fZoomCenter := Vec3(0.5, 0.5, 0.5);// ? ? Z
  cropMask := Vec6(-1, 0, 0, 0, 0, 0);
  //zoom.sliceFrac2D := Vec3(0.5, 0.5, 0.5);
  isLabelOrient := true;
  isRadiological := false;
  numLineVerts := 0;
  numMosRender := 0;
  //numQuads := 0;
  numSliceVerts := 0;
  lineWid := 1;
  lineXGap := 8;
  sliceVerts := nil;
  //quad2Ds := nil;
  lineVerts := nil;
  newLines := false;
  newSlices := false;
   lineClr := Vec4(0.5, 0.5, 0.7, 1.0);
end;

procedure TSlices2D.DrawLineLBWH(left,bottom,width,height: single);
begin
     DrawLine(left,bottom,left+width, bottom+height);
end;

procedure TSlices2D.DrawLine(startX,startY,endX,endY: single);
var
  i: integer;
  nx,ny, len: single;
begin
  ny :=  (startX-endX);
  nx :=  (startY-endY);
  len := sqrt(sqr(nx)+sqr(ny));
  if (len = 0) then exit;
  newLines := true;
  nx := 0.5*lineWid*nx/len;
  ny := 0.5*lineWid*ny/len;
  if (numLineVerts+6) > length(lineVerts) then
     setlength(lineVerts, length(lineVerts)+kBlockSz);
  for i := 0 to 5 do
      lineVerts[numLineVerts +i].textureCoord := lineClr;
  lineVerts[numLineVerts+0].position := Vec2(startX+nx,startY-ny);
  lineVerts[numLineVerts+1].position := Vec2(startX-nx,startY+ny);
  lineVerts[numLineVerts+2].position := Vec2(endX+nx,endY-ny);
  lineVerts[numLineVerts+3].position := lineVerts[numLineVerts+1].position;
  lineVerts[numLineVerts+4].position := lineVerts[numLineVerts+2].position;
  lineVerts[numLineVerts+5].position := Vec2(endX-nx,endY+ny);
  numLineVerts := numLineVerts + 6;
end;

procedure TSlices2D.DrawOutLine(L,T,R,B: single);
begin
  DrawLine(L,T,L,B);
  DrawLine(L,B,R,B);
  DrawLine(R,B,R,T);
  DrawLine(L,T,R,T);
end;

procedure TSlices2D.DrawCross(L,B, W,H,Xfrac,Yfrac: single);
var
  X, Y, T, R: single;
begin
  T := B + H; //top
  R := L + W; //right
  X := L+(W * Xfrac);
  Y := B+(H * Yfrac);
  //for no gap:
  // DrawLine(L,Y,L+W,Y);
  // DrawLine(X, B, X, B+H);
  DrawLine(X, B, X, max(Y-lineXGap, B));
  DrawLine(X, T, X, min(Y+lineXGap,T));
  DrawLine(L,Y,max(X-lineXGap,L),Y);
  DrawLine(R,Y,min(X+lineXGap,R),Y);
end;

procedure TSlices2D.DrawCrossX(L,B, W,H,Xfrac,Yfrac: single);
var
  X, Y, T, R: single;
begin
  T := B + H; //top
  R := L + W; //right
  X := L+(W * Xfrac);
  Y := B+(H * Yfrac);
  if (Xfrac >= 0) and (Xfrac <= 1) and (Yfrac >= 0) and (Yfrac <= 1) then begin
     //Both horizontal and vertical lines in bitmap
     DrawLine(X, B, X, max(Y-lineXGap, B));
     DrawLine(X, T, X, min(Y+lineXGap,T));
     DrawLine(L,Y,max(X-lineXGap,L),Y);
     DrawLine(R,Y,min(X+lineXGap,R),Y);
     exit;
  end;
  if (Xfrac >= 0) and (Xfrac <= 1) then begin
     //vertical line in bitmap
     DrawLine(X, B, X, T);
  end;
  if (Yfrac >= 0) and (Yfrac <= 1) then begin
     //horizontal line in bitmap
     DrawLine(L,Y,R,Y);
  end;
end;

procedure TSlices2D.DrawTri(v1, v2, v3: TVec2; clr : TVec4);
begin
  newLines := true;
  if (numLineVerts+3) > length(lineVerts) then
     setlength(lineVerts, length(lineVerts)+kBlockSz);
  lineVerts[numLineVerts +0].textureCoord := clr;
  lineVerts[numLineVerts +1].textureCoord := clr;
  lineVerts[numLineVerts +2].textureCoord := clr;
  lineVerts[numLineVerts+0].position := v1;
  lineVerts[numLineVerts+1].position := v2;
  lineVerts[numLineVerts+2].position := v3;
  numLineVerts := numLineVerts + 3;
end;

procedure TSlices2D.DrawRender(L,B, W,H, Slice: single; Orient: integer);
begin
  {$IFDEF MOSAIC}
  if (length(mosRenders) < (numMosRender + 1)) then
     setlength(mosRenders, (numMosRender + kBlockSz));
  mosRenders[numMosRender].Left:= L;
  mosRenders[numMosRender].Bottom:= B;
  mosRenders[numMosRender].Width:= W;
  mosRenders[numMosRender].Height:= H;
  if slice < 0 then
     Orient := -Orient;
  mosRenders[numMosRender].Orient := Orient;
  numMosRender := numMosRender + 1;
  {$ENDIF}
end;

procedure TSlices2D.TextLabelLeft(X,Y: single; Caption: string);
var
  lH: single;
begin
    //lW := txt.TextWidth(FontScale,Caption) * 0.5;
    lH := txt.BaseHeight*FontScale*0.5;
    txt.TextOut(x+2,y-lH,FontScale,Caption);
end;

procedure TSlices2D.TextLabelTop(X,Y: single; Caption: string);
var
  lW,lH: single;
begin
  lH := txt.BaseHeight*FontScale;
    lW := txt.TextWidth(FontScale,Caption) * 0.5;
    txt.TextOut(x-lW,y-lH-2,FontScale,Caption);
end;

function LerpZero(lo, hi, val: single): single;
//if lo= 0 hi=3 and val=1 then returns 0.33 (val is 1/3 of way between lo and hi
var
  rng: single;
begin
     rng := hi - lo;
     if (rng = 0) then exit(-1.0);
     result := (val - lo)/rng;
end;

procedure SwapClr(var a,b: TVec4);
var
  c: TVec4;
begin
  c := a;
  a := b;
  b := c;
end;

procedure TSlices2D.DrawBar(left,bottom,width,height: single; clr : TVec4);
var
  i: integer;
begin
  if (numLineVerts+6) > length(lineVerts) then
     setlength(lineVerts, length(lineVerts)+kBlockSz);
  for i := 0 to 5 do
      lineVerts[numLineVerts +i].textureCoord := clr;
  lineVerts[numLineVerts+0].position := Vec2(left,bottom+height);
  lineVerts[numLineVerts+1].position := Vec2(left,bottom);
  lineVerts[numLineVerts+2].position := Vec2(left+width,bottom+height);
  lineVerts[numLineVerts+3].position := lineVerts[numLineVerts+1].position;
  lineVerts[numLineVerts+4].position := lineVerts[numLineVerts+2].position;
  lineVerts[numLineVerts+5].position := Vec2(left+width,bottom);
  numLineVerts := numLineVerts + 6;

end;

procedure TSlices2D.DrawCropMask(L,B,W,H, tZ: single; orient: integer);
var
  clr : TVec4;
  cL,cB,cW, cH: single;
  msk: TVec6;
begin
  clr := vec4(0.8, 0.2, 0.2, 0.4);
  //if (orient <> kCoronalOrient) then exit;
  msk := cropMask;
  if (orient = kAxialOrient) and ((tZ < msk.zLo) or (tZ > msk.zHi)) then exit;
  if (orient = kCoronalOrient) and ((tZ < msk.yLo) or (tZ > msk.yHi)) then exit;
  if ((orient = kSagRightOrient) or (orient = kSagLeftOrient)) and ((tZ < msk.xLo) or (tZ > msk.xHi)) then exit;
  if isRadiological then begin
     msk.xLo := 1.0 - cropMask.xHi;
     msk.xHi := 1.0 - cropMask.xLo;
  end;
  if (orient = kAxialOrient) or (orient = kCoronalOrient) then begin//x = L>>R
    cL := L + (msk.xLo * w);
    cW := ((msk.xHi - msk.xLo) * w);
  end else begin //sagittal x = A>>P
    if (orient = kSagLeftOrient) then begin
       msk.yLo := 1.0 - cropMask.yHi;
       msk.yHi := 1.0 - cropMask.yLo;
    end;
    cL := L + (msk.yLo * w);
    cW := ((msk.yHi - msk.yLo) * w);
  end;
  if (orient <> kAxialOrient) then begin//y = I>>S
    cB := B + (msk.zLo * H);
    cH := ((msk.zHi - msk.zLo) * H);
  end else begin //axial y = A>>P
  cB := B + (msk.yLo * H);
  cH := ((msk.yHi - msk.yLo) * H);
  end;
  DrawBar(cL, cB, cW, cH, clr );
end;

procedure TSlices2D.DrawArrow(L,B,W,H: single; orient: integer);
const
  kFrac = 0.07;
var
  T,R: single;
  x,y, midX, midY: single;
  Tclr, Lclr, Rclr, Bclr : TVec4;
begin
  T := B + H;
  R := L + W;
  midX := L + W/2;
  midY := B + H/2;

  x := (W * kFrac);
  y := (H * kFrac);
  if orient = kAxialOrient then begin
     Tclr := Vec4(1.0, 0.0, 1.0, 1.0); //anterior = purple
     Bclr := Vec4(0.0, 0.0, 1.0, 1.0); //posterior=blue
  end else begin
      Tclr := Vec4(1.0, 1.0, 0.0, 1.0); //superior = yellow
      Bclr := Vec4(1.0, 0.65, 0.0, 1.0); //inferiro = orange
  end;
  if (orient = kAxialOrient) or (orient = kCoronalOrient) then begin
     Lclr := Vec4(1.0, 0.0, 0.0, 1.0);  //left=red
     Rclr := Vec4(0.0, 1.0, 0.0, 1.0);  //right=green
     if isRadiological then
        SwapClr(Lclr, Rclr); //<- don't think we do this: attempting to determine if displayed correctly
  end else begin
    Lclr := Vec4(1.0, 0.0, 1.0, 1.0); //anterior = purple
    Rclr := Vec4(0.0, 0.0, 1.0, 1.0); //posterior = blue
    if orient = kSagRightOrient then
       SwapClr(Lclr, Rclr);
  end;
  //top arrow
  DrawTri(Vec2(midX, T), Vec2(midX-x,T-y), Vec2(midX+X, T-y),   Tclr);
  //bottom arrow
  DrawTri(Vec2(midX, B), Vec2(midX-x,B+y), Vec2(midX+X, B+y),   Bclr);
  //left arrow
  DrawTri(Vec2(L, midY), Vec2(L+x,midY+y), Vec2(L+X, midY-y),   Lclr);
  //right arrow
  DrawTri(Vec2(R, midY), Vec2(R-x,midY+y), Vec2(R-X, midY-y),   Rclr);
end;

function frac2pix(frac: TVec3; L,B,W,H: single; orient: integer; tLT, tLB, tRB: TVec4): TVec2;
begin
  if (orient <> kSagRightOrient) and (orient <> kSagLeftOrient) then
  	 result.x := LerpZero(tLB.x, tRB.x, frac.x) //for axial and coronal images screen X is texture X
  else
      result.x := LerpZero(tLB.y, tRB.y, frac.y); //for sagitall screen x is texture y
  if orient <> kAxialOrient then
     result.y := LerpZero(tLB.z, tLT.z, frac.z)   //for sagittal and coronal screen y is texture z
  else
      result.y := LerpZero(tLB.y, tLT.y, frac.y); //for axial scans, screen y is texture y
  result.x := L+result.x * W;
  result.y := B+result.y * H;
end;

procedure TSlices2D.DrawDistanceLine(L,B,W,H: single; iOrient: integer; tLT, tLB, tRB: TVec4);
var
  st, en: TVec2;
  wid: single;
begin
  if distanceLineOrient <> iOrient then exit;
  wid := LineWid;
  LineWid := max(LineWid, 1.0);
  st := frac2pix(distanceLineStart, L,B,W,H, iOrient, tLT, tLB, tRB);
  en := frac2pix(distanceLineEnd, L,B,W,H, iOrient, tLT, tLB, tRB);
  DrawLine(st.x,st.y, en.x, en.y);
  LineWid := wid;
end;

procedure TSlices2D.AddQuad(L,B, W,H, tZ: single; iOrient: integer; m: TMat4);
var
  mZoom:TMat4;
  x,y, scale: single;
  pivot1, pivot, tLT, tRT, tLB, tRB,tD: TVec4;
  mx: TMat4;
begin
  mZoom := TMat4.Diag(1,1,1);
  //  kAxialOrient = 1;
  //  kCoronalOrient = 2;
  //  kSagRightOrient = 4;
  //  kSagLeftOrient = 8;
  scale := 1;
  if (fZoomScale > 0) then scale := 1.0 / fZoomScale;
  if (iOrient <> kSagLeftOrient) and (iOrient <> kSagRightOrient) then
     mZoom.m[0,0] := scale;
  if iOrient <> kCoronalOrient then
     mZoom.m[1,1] := scale;
  if iOrient <> kAxialOrient then
     mZoom.m[2,2] := scale;
  pivot := Vec4(0.5, 0.5, 0.5, 0.0);
  tD :=  m * Vec4(0.0, 0.0, tZ, 0.0); //depth
  pivot1 := pivot;
  //fZoomCenter := Vec3(0.5, 0.5, 0.5);
  if (iOrient = kSagLeftOrient) then
      pivot1.x := 0.5 - fZoomScale * (fZoomCenter.y-0.5) //Sagittal: horizontal is Y texture
  else if (iOrient = kAxialOrient) or (iOrient = kCoronalOrient) then begin
       if isRadiological then
         pivot1.x := 0.5 - fZoomScale * (fZoomCenter.x-0.5)
       else
           pivot1.x := 0.5 + fZoomScale * (fZoomCenter.x-0.5)
  end else
      pivot1.x := 0.5 + fZoomScale * (fZoomCenter.y-0.5); //Sagittal: horizontal is Y texture
  if (iOrient <> kAxialOrient) then //for Sagitall and Coronal: Screen Up/Down based on Z
     pivot1.y := 0.5 + fZoomScale * (fZoomCenter.z-0.5)
  else
      pivot1.y := 0.5 + fZoomScale * (fZoomCenter.y-0.5);
  mx :=  (mZoom * m);
  tLB := (mx * (Vec4(0.0, 0.0, 0.0, 0.1) - pivot1)) + pivot + tD;
  tRB := (mx * (Vec4(1.0, 0.0, 0.0, 0.1) - pivot1)) + pivot + tD;
  tLT := (mx * (Vec4(0.0, 1.0, 0.0, 0.1) - pivot1)) + pivot + tD;
  tRT := (mx * (Vec4(1.0, 1.0, 0.0, 0.1) - pivot1)) + pivot + tD;
  //GLForm1.LayerBox.Caption := format('%f %f %f', [tRB.X,tRB.Y,tRB.Z]);
  //GLForm1.LayerBox.Refresh;
  if (length(sliceVerts) < (numSliceVerts + 6)) then
     setlength(sliceVerts, (numSliceVerts + kBlockSz));
  newSlices := true;
  sliceVerts[numSliceVerts+0] := Vertex2D(V2(L, B+H),   V4(tLT.x, tLT.y, tLT.z, 1), iOrient); //LT
  sliceVerts[numSliceVerts+1] := Vertex2D(V2(L, B),     V4(tLB.x, tLB.y, tLB.z, 1), iOrient);  //LB
  sliceVerts[numSliceVerts+2] := Vertex2D(V2(L+W, B+H), V4(tRT.x, tRT.y, tRT.z, 1), iOrient); //RT
  sliceVerts[numSliceVerts+3] := Vertex2D(V2(L+W, B),   V4(tRB.x, tRB.y, tRB.z, 1), iOrient); //RB
  sliceVerts[numSliceVerts+4] := sliceVerts[numSliceVerts+2];
  sliceVerts[numSliceVerts+5] := sliceVerts[numSliceVerts+1];
  numSliceVerts := numSliceVerts + 6;
  if CropMask.xLo >= 0 then
     DrawCropMask(L,B,W,H,tZ, iOrient);
  if isOrientationTriangles then
      DrawArrow(L,B,W,H, iOrient);
  DrawDistanceLine(L,B,W,H, iOrient, tLT, tLB, tRB);
  if lineWid <= 0 then exit;
  if (iOrient <> kSagRightOrient) and (iOrient <> kSagLeftOrient) then
   x := LerpZero(tLB.x, tRB.x, sliceFrac2D.x) //for axial and coronal images screen X is texture X
  else
      x := LerpZero(tLB.y, tRB.y, sliceFrac2D.y); //for sagitall screen x is texture y
  if iOrient <> kAxialOrient then
     y := LerpZero(tLB.z, tLT.z, sliceFrac2D.z)   //for sagittal and coronal screen y is texture z
  else
      y := LerpZero(tLB.y, tLT.y, sliceFrac2D.y); //for axial scans, screen y is texture y
  //if (x < 0) or (x > 1) or (y < 0) or (y > 1) then exit;
  DrawCrossX(L,B,W,H,x, y);
    if isOrientationTriangles then
      DrawArrow(L,B,W,H, iOrient);
end; //AddQuad()


(*procedure TSlices2D.AddQuad(L,B, W,H, tZ: single; orient: integer; m: TMat4);
var
  mZoom:TMat4;
  x,y, scale: single;
  pivot, tLT, tRT, tLB, tRB,tD: TVec4;
  mx: TMat4;
begin
  mZoom := TMat4.Diag(1,1,1);
  //  kAxialOrient = 1;
  //  kCoronalOrient = 2;
  //  kSagRightOrient = 4;
  //  kSagLeftOrient = 8;
  scale := 1;
  if (zoom > 0) then scale := 1.0 / zoom;
  if (orient <> kSagRightOrient) and (orient <> kSagRightOrient) then
   mZoom.m[0,0] := scale;
  if orient <> kCoronalOrient then
     mZoom.m[1,1] := scale;
  if orient <> kAxialOrient then
     mZoom.m[2,2] := scale;
  pivot := Vec4(0.5, 0.5, 0.5, 0.0);
  //ZoomCenter := Vec3(0.5, 0.5, 0.5);// ? ? Z
  //pivot := Vec4(ZoomCenter.x, ZoomCenter.y, ZoomCenter.z, 0.0);
  //pivot := Vec4(0.2, 0.0, 0.0, 0.0);
  tD :=  m * Vec4(0.0, 0.0, tZ, 0.1) + pivot; //depth

  mx := (mZoom * m);
  tLB := (mx * Vec4(0.0, 0.0, 0.0, 0.1) )- pivot  + tD;
  tRB := (mx * Vec4(1.0, 0.0, 0.0, 0.1) )- pivot  + tD;
  tLT := (mx * Vec4(0.0, 1.0, 0.0, 0.1) )- pivot  + tD;
  tRT := (mx * Vec4(1.0, 1.0, 0.0, 0.1) )- pivot  + tD;


  //ZoomCenter := Vec3(0.5, 0.5, 0.5);

  if (length(sliceVerts) < (numSliceVerts + 6)) then
     setlength(sliceVerts, (numSliceVerts + kBlockSz));
  newSlices := true;
  sliceVerts[numSliceVerts+0] := Vertex2D(V2(L, B+H),   V4(tLT.x, tLT.y, tLT.z, 1), orient); //LT
  sliceVerts[numSliceVerts+1] := Vertex2D(V2(L, B),     V4(tLB.x, tLB.y, tLB.z, 1), orient);  //LB
  sliceVerts[numSliceVerts+2] := Vertex2D(V2(L+W, B+H), V4(tRT.x, tRT.y, tRT.z, 1), orient); //RT
  sliceVerts[numSliceVerts+3] := Vertex2D(V2(L+W, B),   V4(tRB.x, tRB.y, tRB.z, 1), orient); //RB
  sliceVerts[numSliceVerts+4] := sliceVerts[numSliceVerts+2];
  sliceVerts[numSliceVerts+5] := sliceVerts[numSliceVerts+1];
  numSliceVerts := numSliceVerts + 6;
  if lineWid <= 0 then exit;
  if (orient <> kSagRightOrient) and (orient <> kSagRightOrient) then
   x := LerpZero(tLB.x, tRB.x, sliceFrac2D.x) //for axial and coronal images screen X is texture X
  else
      x := LerpZero(tLB.y, tRB.y, sliceFrac2D.y); //for sagitall screen x is texture y
  if orient <> kAxialOrient then
     y := LerpZero(tLB.z, tLT.z, sliceFrac2D.z)   //for sagittal and coronal screen y is texture z
  else
      y := LerpZero(tLB.y, tLT.y, sliceFrac2D.y); //for axial scans, screen y is texture y
  if (x < 0) or (x > 1) or (y < 0) or (y > 1) then exit;
  DrawCross(L,B,W,H,x, y);
end;  *)

procedure TSlices2D.DrawAx(L,B, W,H, ZFrac: single);
var
  rot:TMat4=(m:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0)));
begin
  if isRadiological then begin
     rot.m[0,0] := -1;
     //rot.m[3,0] := -0;

  end;
  AddQuad(L,B, W, H, ZFrac, kAxialOrient, rot);
  if lineWid <= 0 then exit;
  (*if isRadiological then begin
     DrawCross(L,B,W,H,1.0-sliceFrac2D.x,sliceFrac2D.y)
  end else begin
      DrawCross(L,B,W,H,sliceFrac2D.x,sliceFrac2D.y);
  end;*)
  if FontScale <= 0.0 then exit;
  if isRadiological then
     TextLabelLeft(L,B+(H * 0.5),'R')
  else
      TextLabelLeft(L,B+(H * 0.5),'L');
  TextLabelTop(L+(W * 0.5),B+H,'A');
end;

procedure TSlices2D.DrawCor(L,B, W,H, YFrac: single);
var
//  rot:TMat4=(m:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0)));
  rot:TMat4=(m:((1.0,0.0,0,0.0),(0.0,0.0,1.0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,0,1.0)));
begin
  if isRadiological then
     rot.m[0,0] := -1;;
  AddQuad(L,B, W, H, YFrac, kCoronalOrient, rot);
  if lineWid <= 0 then exit;
  (*if isRadiological then
     DrawCross(L,B,W,H,1.0-sliceFrac2D.x,sliceFrac2D.z)
  else
      DrawCross(L,B,W,H,sliceFrac2D.x,sliceFrac2D.z);*)
  if FontScale <= 0.0 then exit;
  if isRadiological then
     TextLabelLeft(L,B+(H * 0.5),'R')
  else
      TextLabelLeft(L,B+(H * 0.5),'L');
  TextLabelTop(L+(W * 0.5),B+H,'S');
end;

procedure TSlices2D.DrawSag(L,B, W,H, XFrac: single);
var
   rot:TMat4=(m:((0.0,1.0,0,0.0),(0.0,0.0,1.0,0.0),(1.0,0.0,0.0,0.0),(0.0,0.0,0,1.0)));
begin
  AddQuad(L,B, W, H, XFrac, kSagRightOrient, rot);
  //DrawCross(L,B,W,H,sliceFrac2D.y,sliceFrac2D.z);

  if FontScale <= 0.0 then exit;
  TextLabelLeft(L,B+(H * 0.5),'P');
  TextLabelTop(L+(W * 0.5),B+H,'S');
end;


procedure TSlices2D.DrawSagMirror(L,B, W,H, XFrac: single);
var
  rot:TMat4=(m:((0.0,-1.0,0,0.0),(0.0,0.0,1.0,0.0),(1.0,0.0,0.0,0.0),(0.0,0.0,0,1.0)));
begin
  AddQuad(L,B, W, H, XFrac, kSagLeftOrient, rot);
  //DrawCross(L,B,W,H,sliceFrac2D.y,sliceFrac2D.z);
  if FontScale <= 0.0 then exit;
  TextLabelLeft(L,B+(H * 0.5),'A');
  TextLabelTop(L+(W * 0.5),B+H,'S');
end;

{$IFDEF UNUSED}
procedure TSlices2D.DrawCor(L,B, W,H, YFrac: single);
var
  TexL, TexR: single;
begin
  if isRadiological then begin
   TexL := 1;
   TexR := 0;
  end else begin
   TexL := 0;
   TexR := 1;
  end;
  //AddQuad(L,B, W,H, YFrac, kCoronalOrient);
  if (length(sliceVerts) < (numSliceVerts + 6)) then
     setlength(sliceVerts, (numSliceVerts + kBlockSz));
  newSlices := true;
  sliceVerts[numSliceVerts+0] := Vertex2D(V2(L, B+H), V4(TexL, YFrac, 1, 1), kCoronalOrient);
  sliceVerts[numSliceVerts+1] := Vertex2D(V2(L, B), V4(TexL, YFrac, 0, 1), kCoronalOrient);
  sliceVerts[numSliceVerts+2] := Vertex2D(V2(L+W, B+H), V4(TexR, YFrac, 1, 1), kCoronalOrient);
  sliceVerts[numSliceVerts+3] := Vertex2D(V2(L+W, B), V4(TexR, YFrac, 0, 1), kCoronalOrient);
  sliceVerts[numSliceVerts+4] := sliceVerts[numSliceVerts+2];
  sliceVerts[numSliceVerts+5] := sliceVerts[numSliceVerts+1];
  numSliceVerts := numSliceVerts + 6;
  if lineWid <= 0 then exit;
  //DrawLine(L,B+(sliceFrac2D.z*H),L+W,B+(sliceFrac2D.z*H));
  //DrawLine(L+(sliceFrac2D.x*W), B, L+(sliceFrac2D.x*W), B+H);
  if isRadiological then
     DrawCross(L,B,W,H,1.0-sliceFrac2D.x,sliceFrac2D.z)
  else
      DrawCross(L,B,W,H,sliceFrac2D.x,sliceFrac2D.z);
  if FontScale <= 0.0 then exit;
  if isRadiological then
     TextLabelLeft(L,B+(H * 0.5),'R')
  else
      TextLabelLeft(L,B+(H * 0.5),'L');
  TextLabelTop(L+(W * 0.5),B+H,'S');
end;

procedure TSlices2D.DrawSag(L,B, W,H, XFrac: single);
begin
  if (length(sliceVerts) < (numSliceVerts + 6)) then
     setlength(sliceVerts, (numSliceVerts + kBlockSz));
  newSlices := true;
  sliceVerts[numSliceVerts+0] := Vertex2D(V2(L, B+H), V4(XFrac, 0, 1, 1), kSagRightOrient);
  sliceVerts[numSliceVerts+1] := Vertex2D(V2(L, B), V4(XFrac, 0, 0, 1), kSagRightOrient);
  sliceVerts[numSliceVerts+2] := Vertex2D(V2(L+W, B+H), V4(XFrac, 1, 1, 1), kSagRightOrient);
  sliceVerts[numSliceVerts+3] := Vertex2D(V2(L+W, B), V4(XFrac, 1, 0, 1), kSagRightOrient);
  sliceVerts[numSliceVerts+4] := sliceVerts[numSliceVerts+2];
  sliceVerts[numSliceVerts+5] := sliceVerts[numSliceVerts+1];
  numSliceVerts := numSliceVerts + 6;
  if lineWid <= 0 then exit;
  //DrawLine(L,B+(sliceFrac2D.z*H),L+W,B+(sliceFrac2D.z*H));
  //DrawLine(L+(sliceFrac2D.y*W), B, L+(sliceFrac2D.y*W), B+H);
  DrawCross(L,B,W,H,sliceFrac2D.y,sliceFrac2D.z);
end;

procedure TSlices2D.DrawSagMirror(L,B, W,H, XFrac: single);
begin
  //AddQuad(L,B, W,H, XFrac, kSagLeftOrient);
  if (length(sliceVerts) < (numSliceVerts + 6)) then
     setlength(sliceVerts, (numSliceVerts + kBlockSz));
  newSlices := true;
  sliceVerts[numSliceVerts+0] := Vertex2D(V2(L, B+H),   V4(XFrac, 1, 1, 1), kSagLeftOrient);
  sliceVerts[numSliceVerts+1] := Vertex2D(V2(L, B),     V4(XFrac, 1, 0, 1), kSagLeftOrient);
  sliceVerts[numSliceVerts+2] := Vertex2D(V2(L+W, B+H), V4(XFrac, 0, 1, 1), kSagLeftOrient);
  sliceVerts[numSliceVerts+3] := Vertex2D(V2(L+W, B),   V4(XFrac, 0, 0, 1), kSagLeftOrient);
  sliceVerts[numSliceVerts+4] := sliceVerts[numSliceVerts+2];
  sliceVerts[numSliceVerts+5] := sliceVerts[numSliceVerts+1];
  numSliceVerts := numSliceVerts + 6;
  if lineWid <= 0 then exit;
  //DrawLine(L,B+(sliceFrac2D.z*H),L+W,B+(sliceFrac2D.z*H));
  //DrawLine(L+(sliceFrac2D.y*W), B, L+(sliceFrac2D.y*W), B+H);
  DrawCross(L,B,W,H,1-sliceFrac2D.y,sliceFrac2D.z);
end;

{$ENDIF}

function  TSlices2D.Update(volScale: TVec3; w,h: single; orient: integer; actualH : integer = -1): single;
//returns scale factor
var
  texL,texB,texW,texH, texwhratio, whratio, w1row, w2row, w3row, h1row, h2row, h3row: single;
  //scale1col, texwhratio1,
  scale1row, scale2row, scale3row, scale: single;
begin
  result := 0.0;
  if (actualH  > 0) then
     viewPixelHeight := actualH
  else
      viewPixelHeight := h;
  numLineVerts := 0;
  //numQuads := 0;
  numSliceVerts := 0;
  if (volScale.x = 0) or (volScale.y = 0) or (volScale.z = 0) or (w < 0.1) or (h < 0.1) then exit;
  txt.ClearText;
  whratio := w/h;
  if isLabelOrient then
     FontScale := min(w,h)/txt.BaseHeight * 0.03 //font height about 3%
  else
      FontScale := 0.0;
  texL := 0;
  texB := 0;
  texH := h;
  texW := w;
  // sliceFrac2D zoomScale will be 0 for mosaics and autofit, x2, x3 etc for other orients centered on sliceFrac2D
  if (orient = kAxialOrient) then begin //axial
    texwhratio := volScale.x / volScale.y;
    if texwhratio < whratio then
       texW := texH *  texwhratio
    else
        texH := texW * 1/texwhratio;
    DrawAx(texL,texB,texW,texH, sliceFrac2D.z);
    result := fZoomScale * texW/volScale.x;
  end else if (orient = kCoronalOrient) then begin //coronl
    texwhratio := volScale.x / volScale.z;
    if texwhratio < whratio then
      texW := texH *  texwhratio
    else
       texH := texW * 1/texwhratio;
    DrawCor(texL,texB, texW,texH,  sliceFrac2D.y);
    result := fZoomScale * texW/volScale.x;
  end else if (orient = kSagRightOrient) or (orient = kSagLeftOrient) then begin //sagittal
       texwhratio := volScale.y / volScale.z;
       if texwhratio < whratio then
          texW := texH *  texwhratio
       else
           texH := texW * 1/texwhratio;
       if (orient = kSagRightOrient) then
          DrawSag(texL,texB, texW,texH, sliceFrac2D.x)
       else
           DrawSagMirror(texL,texB, texW,texH, sliceFrac2D.x);
       result := fZoomScale * texW/volScale.y;
  end else begin //default to multislice view (orient = kAxCorSagOrient)
    //1-row w = x+x+y, h= max(y,z)
    //2-row w = x+y, h = z + y
    //3-row w = max(x,y), h= z+z+y
    w1row := texw/(volScale.x+volScale.x+volScale.y);
    w2row := texw/(volScale.x+volScale.y);
    w3row := texw/max(volScale.x,volScale.y);
    h1row := texh/max(volScale.y,volScale.z);
    h2row := texh/(volScale.y+volScale.z);
    h3row := texh/(volScale.y+volScale.z+volScale.z);
    //GLForm1.Caption := format('%g %g', [h3row, w3row]);
    scale1row := min(h1row, w1row);
    scale2row := min(h2row, w2row);
    scale3row := min(h3row, w3row);
    //GLForm1.Caption := format('%g %g', [w,h]);
    //GLForm1.Caption := format('%g %g %g', [scale1row,scale2row,scale3row]);
    //GLForm1.Caption := format('%g %g %g', [volScale.x, volScale.y, volScale.z]);
    if (orient = kAxCorSagOrient4) then begin //OKRA force a 2*2 ROW*COL display
     scale1row := 0;
     scale3row := 0;
    end else if (scale3row > scale1row) and (scale3row > scale2row) then begin // 3*1 ROW*COL
       scale1row := 0;
       scale2row := 0;
    end else if (scale1row > scale2row) then begin // 1*3 ROW*COL display
       scale2row := 0;
       scale3row := 0;
    end else begin  //default a 2*2 ROW*COL display
        scale1row := 0;
        scale3row := 0;
    end;
    //GLForm1.Caption := format('%g %g %g', [scale1row,scale2row,scale3row]);
    scale := max(max(scale1row, scale2row), scale3row);
    //axial
    texH := volScale.y * scale;
    texW := volScale.x * scale;
    DrawAx(texL,texB, texW,texH, sliceFrac2D.z);
    result := fZoomScale * texW/volScale.x;
    //coronal
    if (scale1row <> 0) then
       texL := texL + texW
    else
       texB := texB+texH;
    texH := volScale.z * scale;
    DrawCor(texL,texB, texW,texH, sliceFrac2D.y);
    //sagittal
    if (scale3row <> 0) then
        texB := texB+texH
    else
        texL := texL+texW;
    texW := volScale.y * scale;
    DrawSag(texL,texB, texW,texH, sliceFrac2D.x); //L,B, W,H, XFrac
    axCorSagOrient4XY.x := round(texW); //embedded rendering height
    axCorSagOrient4XY.y := round(texW); //embedded rendering width
    axCorSagOrient4XY.z := round(texL); //embedded rendering left offset
    //GLForm1.Caption := format('%g %g %g', [texL,texB,texH]); //OKRA
  end;
end;

procedure  Coord(var lV: TVec4; lMat: TMat4);
//transform X Y Z by matrix
var
  lXi,lYi,lZi: single;
begin
  lXi := lV[0]; lYi := lV[1]; lZi := lV[2];
  lV[0] := (lXi*lMat[0,0]+lYi*lMat[0,1]+lZi*lMat[0,2]+lMat[0,3]);
  lV[1] := (lXi*lMat[1,0]+lYi*lMat[1,1]+lZi*lMat[1,2]+lMat[1,3]);
  lV[2] := (lXi*lMat[2,0]+lYi*lMat[2,1]+lZi*lMat[2,2]+lMat[2,3]);
end;

function SliceMM(vox0: TVec3; Mat: TMat4): TVec3;
var
  v4: TVec4;
begin
  v4 := Vec4(vox0.x, vox0.y, vox0.z, 1.0); //Vec4(X-1,Y-1,Z-1, 1); //indexed from 0, use -1 if indexed from 1
  Coord(v4, Mat);
  result := Vec3(v4.x, v4.y, v4.z);
end;

//{$DEFINE FX}
{$IFDEF FX}
function TSlices2D.FracMM(inFrac: TVec3; Mat: TMat4; Dim: TVec3i): TVec3; overload;
begin
  result.X := 0.5;
  result.Y := 0.5;
  result.Z := 0.5;
end;

function TSlices2D.FracMM(Mat: TMat4; Dim: TVec3i; out vox: TVec3i): TVec3; overload;
begin
  result.X := 0.5;
  result.Y := 0.5;
  result.Z := 0.5;

end;
{$ELSE}
function TSlices2D.FracMM(inFrac: TVec3; Mat: TMat4; Dim: TVec3i): TVec3; overload;
var
  vox0: TVec3;
begin
     //writeln(format('%gx%gx%g %dx%dx%d', [sliceFrac2D.x, sliceFrac2D.y, sliceFrac2D.z, Dim.x, Dim.y, Dim.z]));
     vox0.x := (inFrac.x * Dim.x);
     vox0.y := (inFrac.y * Dim.y);
     vox0.z := (inFrac.z * Dim.z);
     //result := vox0; exit;
     result := SliceMM(vox0, Mat);
end;

function TSlices2D.FracMM(Mat: TMat4; Dim: TVec3i; out vox: TVec3i): TVec3; overload;
var
  vox0: TVec3;
begin
     //writeln(format('%gx%gx%g %dx%dx%d', [sliceFrac2D.x, sliceFrac2D.y, sliceFrac2D.z, Dim.x, Dim.y, Dim.z]));
     vox0.x := trunc(sliceFrac2D.x * Dim.x);
     vox0.y := trunc(sliceFrac2D.y * Dim.y);
     vox0.z := trunc(sliceFrac2D.z * Dim.z);
     vox0.x := min(vox0.x, Dim.x -1); //0..Dim-1, perfect unless sliceFrac2D.x=1
     vox0.y := min(vox0.y, Dim.y -1); //0..Dim-1, perfect unless sliceFrac2D.y=1
     vox0.z := min(vox0.z, Dim.z -1); //0..Dim-1, perfect unless sliceFrac2D.z=1
     //result := vox0; exit;
     result := SliceMM(vox0, Mat);
     vox.X := round(vox0.x);
     vox.Y := round(vox0.y);
     vox.Z := round(vox0.z);
end;
{$ENDIF}

{$IFDEF MOSAIC}
const
  kMaxMosaicDim = 12; //e.g. if 12 then only able to draw up to 12x12 mosaics [=144 slices]
  kOrientMask = 63;
  kRenderSliceOrient = 64;
  kCrossSliceOrient = 128;

type
TPointF = record
     X,Y: single;
end;
  TMosaicSingle = array [1..kMaxMosaicDim, 1..kMaxMosaicDim] of single;
  TMosaicPoint = array [1..kMaxMosaicDim, 1..kMaxMosaicDim] of TPointF;
  TMosaicOrient = array [1..kMaxMosaicDim, 1..kMaxMosaicDim] of byte;
  TMosaicText = array [1..kMaxMosaicDim, 1..kMaxMosaicDim] of boolean;
  TMosaic =record
    Slices: TMosaicSingle;
    Dim,Pos: TMosaicPoint;
    Orient: TMosaicOrient;
    Text: TMosaicText;
    HOverlap,VOverlap, ClrBarSizeFracX: single;
    LeftBorder, BottomBorder, MaxWid,MaxHt, Rows,Cols: integer;
    isMM: boolean;
  end;

procedure mm2Voxel (var X,Y,Z: single; lInvMat: TMat4);
//returns voxels indexed from 1 not 0!
var
   lV: TVec4;
begin
     lV := Vec4(X,Y,Z,1);
     Coord (lV,lInvMat);
     X := lV[0]+1;
     Y := lV[1]+1;
     Z := lV[2]+1;
end;

procedure Voxel2mm(var X,Y,Z: single; Mat: TMat4);
var
   lV: TVec4;
begin
     lV := Vec4(X-1,Y-1,Z-1,1);
     Coord(lV, Mat);
     X := lV.X;
     Y := lV.Y;
     Z := lV.Z;
end;

function FracToSlice (lFrac: single; lSlices : integer): single;
//indexed from 1
begin
    result := round(((lFrac+ (0.5/lSlices))*lSlices)); //e.g. if 6 slices anything greater than 0.167 counted in slice 2
    //result := round(lFrac*lSlices);
    if result > lSLices then //e.g. if lFrac = 1.0, then result is lSlices+1!
      result := lSlices;
    if result < 1 then //This is impossible if lFrac is 0..1, but just in case...
      result := 1;
end;

function SliceMM_NoFlipLR (lSliceFrac: single; lOrient: integer; Dim: TVec3i; Mat: TMat4): single;
var
  X,Y,Z: single;
begin
    lOrient := (lOrient and kOrientMask);
    X := 0.5;
    Y := 0.5;
    Z := 0.5;
    case lOrient of
      kAxialOrient : Z := lSLiceFrac;
      kCoronalOrient : Y := lSliceFrac;
      kSagRightOrient : X := lSliceFrac;
      kSagLeftOrient : X := lSliceFrac;
    end;
    //if gPrefs.FlipLR then X := 1 - X;
    X := FracToSlice(X,Dim.X);
    Y := FracToSlice(Y,Dim.Y);
    Z := FracToSlice(Z,Dim.Z);
    Voxel2mm(X,Y,Z,Mat);
    case lOrient of
      kAxialOrient : result := Z;
      kCoronalOrient : result := Y;
      kSagRightOrient,kSagLeftOrient : result := X;
      else result := 0; //should be impossible - prevents compiler warning
    end;
    //ScriptForm.Memo2.Lines.Add(format('%g -> %g', [lSliceFrac, result]));
end; //SliceMM

function SliceMM2 (lSliceFrac: single; lOrient: integer; Dim: TVec3i; Mat: TMat4): single;
var
  X,Y,Z: single;
begin
    lOrient := (lOrient and kOrientMask);
    X := 0.5;
    Y := 0.5;
    Z := 0.5;
    case lOrient of
      kAxialOrient : Z := lSLiceFrac;
      kCoronalOrient : Y := lSliceFrac;
      kSagRightOrient : X := lSliceFrac;
      kSagLeftOrient : X := lSliceFrac;
    end;
    //x if gPrefs.FlipLR then X := 1 - X;
    X := FracToSlice(X,Dim.X);
    Y := FracToSlice(Y,Dim.Y);
    Z := FracToSlice(Z,Dim.Z);
    Voxel2mm(X,Y,Z,Mat);
    case lOrient of
      kAxialOrient : result := Z;
      kCoronalOrient : result := Y;
      kSagRightOrient,kSagLeftOrient : result := X;
      else result := 0; //should be impossible - prevents compiler warning
    end;
    //ScriptForm.Memo2.Lines.Add(format('%g -> %g', [lSliceFrac, result]));
end; //SliceMM


function SliceToFrac (lSlice: single; lSlices : integer): single;
//indexed from 1
begin
    result := 0.5;
    if lSlices < 1 then
      exit;
    //result := round(((lFrac+ (1/lSlices))*lSlices));
    result := (lSlice-0.5)/lSlices; //-0.5: consider dim=3, slice=2 means frac 0.5
    //bound in range 0..1
    if result < 0 then
      result := 0;
    if result > 1 then
      result := 1;
end;

function SliceFracF (lSliceMM: single; lOrient: integer; lInvMat: TMat4; Dim: TVec3i): single;
var
  X,Y,Z: single;
begin
    //showmessage(format('%gmm %g %g %g', [lSliceMM, X, Y, Z] )); //targa
    lOrient := (lOrient and kOrientMask);
    X := 0;
    Y := 0;
    Z := 0;
    case lOrient of
      kAxialOrient : Z := lSLiceMM;
      kCoronalOrient : Y := lSliceMM;
      kSagRightOrient : X := lSliceMM;
      kSagLeftOrient : X := lSliceMM;
    end;
    mm2Voxel (X,Y,Z, lInvMat);
    case lOrient of
      kAxialOrient : result := SliceToFrac(Z,Dim.z);
      kCoronalOrient : result := SliceToFrac(Y,Dim.y);
      kSagRightOrient,kSagLeftOrient : result := SliceToFrac(X,Dim.x);
      else result := 0; //should be impossible - prevents compiler warning
    end;
end; //SliceMM

procedure StereoTaxicSpaceToFrac (var lMosaic: TMosaic; lInvMat: TMat4; Dim: TVec3i);
var
  lRow,lCol: integer;
  lFrac, lBetween0and1: boolean;
begin
  //lOK := false;
  if (lMosaic.Cols < 1) or (lMosaic.Rows < 1)  then
    exit;
  if not lMosaic.isMM then begin
     lFrac := true;
     lBetween0and1 := false;
     for lRow := 1 to lMosaic.Rows do
         for lCol := 1 to lMosaic.Cols do begin
             if ((lMosaic.Orient[lCol,lRow] and kRenderSliceOrient) = kRenderSliceOrient) then
               continue;
             if (lMosaic.Orient[lCol,lRow] <> kEmptyOrient) and ((lMosaic.Slices[lCol,lRow] < 0) or (lMosaic.Slices[lCol,lRow] > 1)) then
                lFrac := false;
             if (lMosaic.Orient[lCol,lRow] <> kEmptyOrient) and (lMosaic.Slices[lCol,lRow] > 0) and (lMosaic.Slices[lCol,lRow] < 1) then
                lBetween0and1 := true;
         end;
     if (lFrac) and (not lBetween0and1) then //treat '0' or '1' as spatial coordinates, but "0,0.5,1" are fractions
        lFrac := false;
     if lFrac then exit;
  end;
  lMosaic.isMM := true;
  //lInvMat := Hdr2InvMat (gTexture3D.NIftiHdr,lOK);
  for lRow := 1 to lMosaic.Rows do begin
    for lCol := 1 to lMosaic.Cols do begin
      if ((lMosaic.Orient[lCol,lRow] and kRenderSliceOrient) = kRenderSliceOrient) then
           continue;
      if lMosaic.Orient[lCol,lRow] <> kEmptyOrient then
        lMosaic.Slices[lCol,lRow] := SliceFracF (lMosaic.Slices[lCol,lRow], lMosaic.Orient[lCol,lRow], lInvMat, Dim)
    end;//col
  end;//row
end;

function SliceXY(lOrient: integer; volScale: TVec3; scalePix: integer): TPointF;
begin
  lOrient := (lOrient and kOrientMask);
  case lOrient of
    kAxialOrient,kCoronalOrient: result.X := scalePix*volScale.X;//screen L/R corresponds to X
    kSagRightOrient,kSagLeftOrient: result.X := scalePix*volScale.Y;//screen L/R corresponds to Y dimension
    else result.X := 0;
  end;//case
  case lOrient of
    kAxialOrient: result.Y := scalePix*volScale.Y;//screen vert is Y
    kCoronalOrient,kSagRightOrient,kSagLeftOrient: result.Y := scalePix*volScale.Z;//screen vert is Z dimension
    else result.Y := 0;
  end;//case
end;

procedure MosaicSetXY (var lMosaic: TMosaic; volScale: TVec3; Dim: TVec3i);
var
  lRow,lCol, mPix: integer;
  lMaxYDim, lMaxY,lX,Hfrac,Vfrac: single;
  //pxScale: TVec3;
begin
  //GLForm1.Caption := (format('--> %g %g %g %d %d %d', [volScale.X, volScale.Y, volScale.Z, Dim.X, Dim.Y, Dim.Z]));
  //mPix := min(min(Dim.x, Dim.y),Dim.z);
  //mPix := max(max(Dim.x, Dim.y),Dim.z);
  (*pxScale.x := 1;
  pxScale.y := 1;
  pxScale.z := 1;
  if (pxScale.X > 0.0) then pxScale.x := Dim.X / volScale.X;
  if (pxScale.Y > 0.0) then pxScale.y := Dim.Y / volScale.Y;
  if (pxScale.Z > 0.0) then pxScale.y := Dim.Z / volScale.Z;

  mPix := Dim.x;
  if (pxScale.y > pxScale.x) then mPix := Dim.y;
  if (pxScale.z > pxScale.x) and (pxScale.z > pxScale.y) then mPix := Dim.z;  *)
  //Consider 1x1x1mm volume with dim= 207x256x215. volScale will be [0.808, 1, 0.839]
  // the mPix is set to the pixels with the largest volscale, so 256
  // not for non-isotropic data we are typically forced to have interpolation in at least one dimension
  mPix := Dim.x;
  if (volScale.y > volScale.x) then mPix := Dim.y;
  if (volScale.z > volScale.x) and (volScale.z > volScale.y) then mPix := Dim.z;
  //GLForm1.Caption := (format('%g--> %g %g %g %d %d %d', [scalePix, volScale.X, volScale.Y, volScale.Z, Dim.X, Dim.Y, Dim.Z]));

  lMosaic.MaxWid := 0;
  if (lMosaic.Cols < 1) or (lMosaic.Rows < 1)  then
    exit;
  //GLForm1.Caption := (format('--> %g %g %g', [volScale.X, volScale.Y, volScale.Z]));
  for lRow := 1 to lMosaic.Rows do begin
    for lCol := 1 to lMosaic.Cols do begin
      lMosaic.Dim[lCol,lRow] := SliceXY(lMosaic.Orient[lCol,lRow], volScale, mPix);
      //GLForm1.Caption :=(format('--> %g %g', [lMosaic.Dim[lCol,lRow].X, lMosaic.Dim[lCol,lRow].Y]));
    end;//col
  end;//row
  lMaxYDim := 0;
  Hfrac := 1 - abs(lMosaic.HOverlap);
  Vfrac := 1 - abs(lMosaic.VOverlap);
  for lRow := lMosaic.Rows downto 1 do begin
    //find max height for this row
    lMaxY := 0;
    if lRow < lMosaic.Rows then begin
      for lCol := 1 to lMosaic.Cols do
        if lMosaic.Dim[lCol,lRow+1].Y > lMaxY then
          lMaxY := lMosaic.Dim[lCol,lRow+1].Y;
        lMaxY := (lMosaic.Pos[1,lRow+1].Y)+  Vfrac* lMaxY;
    end;
    //now compute width
    lX := 0;
    lMaxYDim := 0;
    for lCol := 1 to lMosaic.Cols do begin
      lMosaic.Pos[lCol,lRow].X := lX;
      lMosaic.Pos[lCol,lRow].Y := lMaxY;
      if (lMosaic.Dim[lCol,lRow].Y > lMaxYDim) then lMaxYDim := lMosaic.Dim[lCol,lRow].Y;
      if lCol < lMosaic.Cols then begin
        if ((lMosaic.Orient[lCol+1,lRow] and kCrossSliceOrient) = kCrossSliceOrient)
         or ((lMosaic.Orient[lCol+1,lRow] and kRenderSliceOrient) = kRenderSliceOrient) then
        	lX := lX +  lMosaic.Dim[lCol,lRow].X
        else
        	lX := lX +  Hfrac*lMosaic.Dim[lCol,lRow].X
      end else
        lX := lX +  lMosaic.Dim[lCol,lRow].X;
    end;//for each column
    if lX > lMosaic.MaxWid then
      lMosaic.MaxWid := ceil(lX);
  end;//for each row
  //lMosaic.MaxHt := ceil(lMosaic.Pos[1,1].Y+lMosaic.Dim[1,1].Y);
  lMosaic.MaxHt := ceil(lMosaic.Pos[1,1].Y+lMaxYDim); //2018: e.g. in case tallest slice not in leftmost position
  //MosaicColorBarXY(lMosaic);
end;

function Str2Mosaic ( lMosaicString: string; lInvMat: TMat4; Dim: TVec3i; volScale: TVec3): TMosaic;
// '0.2 0.4 0.8; 0.9' has 3 columns and 2 rows
// '0.2 0.4; 0.8 0.9' has 2 columns and 2 rows
// '0.2 0.4; 0.8 s 0.5' has 2 columns and 2 rows, with final item a sagittal slice
//INPUTS
// a c s z   --- orientations (axial,coronal, sagittal,sagittal[mirror]
//  numbers, including decimal separator and E (for scientific notation)
//  ;        ---- next row
// v h     ---- vertical and horizontal overlap, e.g. 0 means none, while 0.2 is 20% range is -1..1
//  note v and h must be followed by a number
var
  FS: TFormatSettings;
function S2F (lStr: String): single;//float to string
begin
  try
    result := StrToFloatDef(lStr,1, FS);    // Hexadecimal values are not supported
  except
    //report problem here..
    result := 1;
  end;//try ... except
end; //nested S2F
procedure DummyMosaic;
begin
    result.Rows := 2;
    Result.Cols := 2;
    result.Slices[1,1] := 0.2;
    result.Slices[1,2] := 0.4;
    result.Slices[2,1] := 0.6;
    result.Slices[2,2] := 0.8;
end; //nested proc DummyMosaic
var
  lDone: boolean;
  lCh,lCh2: char;
  lNumStr: string;
  lFloat: single;
  lX,lY,lPos,lLen,lCurrentOrient,lCol, lCrossFlag, lRenderFlag : integer;
  lCurrentText : boolean;
begin
  FS := FormatSettings;
  FS.DecimalSeparator := '.';
  result.isMM := false;
  lCurrentOrient := kAxialOrient;
  lCurrentText := false;
  lCrossFlag := 0; //assume slice is not a cross-section
  lRenderFlag := 0; //assume slice is not a render
  for lX := 1 to kMaxMosaicDim do begin
    for lY := 1 to kMaxMosaicDim do begin
      result.Orient[lX,lY] := kEmptyOrient;
      result.Text[lX,lY] := lCurrentText;
    end;
  end;
  result.HOverlap := 0;
  result.VOverlap := 0;
  result.Cols := 0;
  result.Rows := 0;
  lLen := length(lMosaicString);
  if lLen < 1 then begin
    DummyMosaic;
    exit;
  end;
  lPos := 1;
  lNumStr := '';
  //lFloat := -1;
  lCol := 0;
  result.Rows := 1;
  result.Cols := 0;
  while lPos <= lLen do begin
    lCh := upcase(lMosaicString[lPos]);
    if lCh in ['0'..'9','-', '+','E',',','.'] then begin
      lNumStr := lNumStr + lCh;
      if lCh = '.' then FS.DecimalSeparator := '.';
      if lCh = ',' then FS.DecimalSeparator := ',';
    end;
    if (lPos = lLen) or (not (lCh in ['0'..'9','-', '+','E',',','.'])) then begin //not number

      //first, read previous number
      if lNumStr <> '' then begin
        lCol := lCol + 1;
        if (lRenderFlag  <> 0) then begin
           if (lNumStr[1] = '-') then
             result.Slices[lCol,Result.Rows] := -1 //e.g. inferior axial view
           else
             result.Slices[lCol,Result.Rows] := 1; //e.g. superior axial view
           //GLForm1.Caption := '@<>'+floattostr(result.Slices[lCol,Result.Rows]);

        end else
            result.Slices[lCol,Result.Rows] := S2F(lNumStr);
        //if lMosaic.Slices[lCol,lRows] < 1 then
        //  fx(lMosaic.Slices[lCol,lRows]);
        //showmessage(floattostr(lMosaic.Slices[lCol,lRows])+'  '+lNumStr);
        result.Orient[lCol,result.Rows] := lCurrentOrient+lCrossFlag+lRenderFlag;
        if ((lCrossFlag = kCrossSliceOrient) or ((lRenderFlag = kRenderSliceOrient))) then
        	result.Text[lCol,result.Rows] := false
        else
        	result.Text[lCol,result.Rows] := lCurrentText;
        lCrossFlag := 0;
        lRenderFlag := 0;
      end;//if lNumStr <> ''
      lNumStr := '';
      //next - see if this is some other command, else whitespace
      if lCh = 'A' then
        lCurrentOrient := kAxialOrient;
      if lCh = 'C' then
        lCurrentOrient := kCoronalOrient;
      if lCh = 'S' then
        lCurrentOrient := kSagRightOrient;
      if lCh = 'Z' then
        lCurrentOrient := kSagLeftOrient;
      if lCh = 'R' then
         lRenderFlag := kRenderSliceOrient;
      if lCh = 'X' then
      	lCrossFlag := kCrossSliceOrient;
      if lCh = 'M' then
         result.isMM := true;
      if lCh = 'L' then begin
        lCurrentText := True;
        if (lPos < lLen) and (lMosaicString[lPos+1] = '-') then begin
          lCurrentText := False;
          inc(lPos);
        end else if (lPos < lLen) and (lMosaicString[lPos+1] = '+') then
          inc(lPos);
      end;
      if (lCh = 'V') or (lCh = 'H') then begin
        lDone := false;
        repeat
          lCh2 := upcase(lMosaicString[lPos]);
          if lCh2 in ['0'..'9','-', '+','E',',','.'] then begin
            lNumStr := lNumStr + lCh2;
            if lCh2 = '.' then FS.DecimalSeparator := '.';
            if lCh2 = ',' then FS.DecimalSeparator := ',';
          end else if lNumStr <> '' then begin
            lDone := true;
            dec(lPos);
          end;
          inc(lPos)
        until (lPos > lLen) or lDone;
        lFloat := S2F(lNumStr);
        if (lFloat > 1) or (lFloat < -1) then
          lFloat := 0;
        if lCh = 'V' then
          result.VOverlap := lFloat
        else
          result.HOverlap := lFloat;
        lNumStr := '';
      end; //V or H ... read overlap
      if lCh = ';' then begin
        result.Rows := result.Rows + 1;
        if lCol > result.Cols then begin
          result.Cols := lCol;
        end;
        lCol := 0;
      end;
    end; //not number
    inc(lPos);//read next character
  end; //for each character in string
  //next - last row may not have a ; so check if final line has most columns...
  if lCol > result.Cols then
    result.Cols := lCol;
  StereoTaxicSpaceToFrac (result, lInvMat, Dim);
  MosaicSetXY(result, volScale, Dim);
  //ReportMosaic(result);
end; //proc ReadMosaicStr

function isCross(lOrient: integer): boolean;
//returns true if slice is Cross-Slice that should have lines drawn
begin
     result := (lOrient and kCrossSliceOrient) = kCrossSliceOrient;
end;

function isRender(lOrient: integer): boolean;
//returns true if slice is Render that should have lines drawn
begin
     result := (lOrient and kRenderSliceOrient) = kRenderSliceOrient;
end;

function isSag(lOrient: integer): boolean;
begin
     result := (lOrient = kSagLeftOrient) or (lOrient = kSagRightOrient);
end;

function ComputeDecimals(var lMosaic: TMosaic; Dim: TVec3i; Mat: TMat4): integer;
var
  lRow,lCol: integer;
  lMin,lMax,lmm: single;
begin
  result := 0;
  if (lMosaic.Rows < 1) or (lMosaic.Cols < 1) or (lMosaic.isMM) then
    exit;
  lMin := MaxInt;
  lMax := -MaxInt;
  for lRow := lMosaic.Rows downto 1 do
      for lCol := 1 to lMosaic.Cols do
        if lMosaic.Text[lCol,lRow] then begin
          lmm := SliceMM2(lMosaic.Slices[lCol,lRow],lMosaic.Orient[lCol,lRow], Dim, Mat);
          if lmm < lMin then
            lMin := lmm;
          if lmm > lMax then
            lMax := lmm
        end;
 lmm := lMax - lMin;
 if lmm > 10 then
  result := 0
 else if lmm > 1 then
  result := 1
 else
  result := 2;
end;

procedure TSlices2D.MosaicScale(lMosaicString: string; Mat, InvMat: TMat4; Dim: TVec3i; volScale: TVec3; out w, h: integer);
var
  lMosaic: TMosaic;
begin
 w := -1;
 lMosaic := Str2Mosaic (lMosaicString, InvMat, Dim, volScale);
 //if (gPrefs.SliceView  <> 5) or (gRayCast.MosaicString = '') then exit;
 //lMosaic := Str2Mosaic ( gRayCast.MosaicString);
 if (lMosaic.MaxWid = 0) or (lMosaic.MaxHt= 0) then exit;
 w := lMosaic.MaxWid;
 h := lMosaic.MaxHt;
end;

procedure TSlices2D.UpdateMosaic(lMosaicString: string; Mat, InvMat: TMat4; Dim: TVec3i; volScale: TVec3; w,h: single);
procedure TextLabelXY(X,Y,lSlice,FntScale: single; lOrient,lDec: integer);
  var
    lF, lW,lH: single;
    lS: string;
  begin
       lF := SliceMM_NoFlipLR (lSlice, lOrient, Dim, Mat);
       if lDec = 0 then
          lF := round(lF);
      lS := FloatToStrF(lF, ffFixed,7,lDec);
      lW := txt.TextWidth(FntScale,lS) * 0.5;
      lH := txt.BaseHeight*FntScale;
      txt.TextOut(x-lW,y-lH,FntScale,lS);
end;
  procedure GLCrossLine(lMosaic: TMosaic; scale, lineWid:single);
  var
    i,j, lOrient, lCrossOrient, lColInc,lRow, lRowInc,lCol:integer;
    fh, fw, l,b,w,h: single;
  begin
   if lineWid < 0.5 then exit;
   //glColor4ub(gPrefs.CrosshairColor.rgbRed,gPrefs.CrosshairColor.rgbGreen,gPrefs.CrosshairColor.rgbBlue,gPrefs.CrosshairColor.rgbReserved);
   if lMosaic.HOverlap < 0 then
     lColInc := -1
   else
     lColInc := 1;
   if lMosaic.VOverlap < 0 then begin
      lRow := lMosaic.Rows;
      lRowInc := -1;
    end else begin
      lRow := 1;
      lRowInc := 1;
    end;
      while (lRow >= 1) and (lRow <= lMosaic.Rows) do begin
        if lMosaic.HOverlap < 0 then
          lCol := lMosaic.Cols
        else
          lCol := 1;
        while (lCol >= 1) and (lCol <= lMosaic.Cols) do begin
          //if ((lMosaic.Orient[lCol,lRow] and kCrossSliceOrient) = kCrossSliceOrient) then
            //TextLabelXY(scale*(lMosaic.Pos[lCol,lRow].X+(lMosaic.Dim[lCol,lRow].X/2) ),scale*(lMosaic.Pos[lCol,lRow].Y+lMosaic.Dim[lCol,lRow].Y),lMosaic.Slices[lCol,lRow],lMosaic.Orient[lCol,lRow],lDec);
          if isCross(lMosaic.Orient[lCol,lRow]) then begin
             lCrossOrient := (lMosaic.Orient[lCol,lRow] and kOrientMask);        //kCrossSliceOrient
             l := scale*lMosaic.Pos[lCol,lRow].X;
             b := scale*lMosaic.Pos[lCol,lRow].Y;
             w := scale*lMosaic.dim[lCol,lRow].X;
             h := scale*lMosaic.dim[lCol,lRow].Y;
             for i := 1 to lMosaic.Cols do
                 for j := 1 to lMosaic.Rows do begin
                   if isRender(lMosaic.Orient[i,j]) then continue;
                   if isCross(lMosaic.Orient[i,j]) then continue;
                   lOrient := (lMosaic.Orient[i,j] and kOrientMask);
                   if lOrient = lCrossOrient then continue; //e.g. sagittal slice is not orthogonal to another sagittal
                   if isSag(lOrient) and isSag(lCrossOrient) then continue; //e.g. sagittal left not orthogonal to sagittal right
                   fh := lMosaic.Slices[i,j] * h;
                   //if (gPrefs.FlipLR) and ((lCrossOrient = kCoronalOrient) or (lCrossOrient = kAxialOrient)) then
                   //   fw := (1.0 - lMosaic.Slices[i,j]) * w
                   //else
                       fw := lMosaic.Slices[i,j] * w;
                   if (lOrient = kSagRightOrient) or (lOrient = kSagLeftOrient) then
                      DrawLineLBWH(l+fw,b,0,h);
                   if (lOrient = kAxialOrient) then
                      DrawLineLBWH(l,b+fh,w,0.0);
                   if (lOrient = kCoronalOrient) then begin
                      if (lCrossOrient = kSagLeftOrient) then
                         DrawLineLBWH(l+w-fw,b,0,h)
                      else if (lCrossOrient =  kSagRightOrient) then
                           DrawLineLBWH(l+fw,b,0,h)
                      else //necessarily: if (lCrossOrient =  kAxialOrient) then
                           DrawLineLBWH(l,b+fh,w,0.0);
                   end;
                 end;
                   //lOrient := (lMosaic.Orient[lCol,lRow] and kOrientMask)
          end;
          lCol := lCol + lColInc;
        end;//col
        lRow := lRow+lRowInc;
      end;//row
  end;
var
  lMosaic: TMosaic;
  lZoomScale, lFontScale, lScale, lLineWid: single;
  lZoomCenter: TVec3;
  lRender: boolean;
  lOrient,lRowInc,lColInc,lRow,lCol, lDec:integer;
begin
  lZoomScale := fZoomScale;
  lZoomCenter := fZoomCenter;
  fZoomCenter := Vec3(0.5, 0.5, 0.5);
  fZoomScale := 1; //torn off zoom
  viewPixelHeight := h;
  numLineVerts := 0;
  numSliceVerts := 0;
  numMosRender := 0;
  if (w < 0.1) or (h < 0.1) then exit;
  FontScale := 0.0; //no A/P L/R S/I labels
  lLineWid := LineWid;
  lMosaic := Str2Mosaic (lMosaicString, InvMat, Dim, volScale);
  lDec := ComputeDecimals(lMosaic, Dim,Mat);
  //Form1.Caption := format('%d %d', [lMosaic.MaxWid, lMosaic.MaxHt]);
  txt.ClearText;
  lScale := w/(lMosaic.MaxWid);
  //lFontScale := min(w,h)/txt.BaseHeight * 0.05; //font height about 5%
  lFontScale := (0.25 *(max(w,h)+3.0*min(w,h)))/txt.BaseHeight * 0.05; //font height about 5%
  // https://www.nitrc.org/forum/message.php?msg_id=25621
  //lFontScale := (0.5 *(max(w,h)+min(w,h)))/txt.BaseHeight * 0.05; //font height about 5%
  if (h/(lMosaic.MaxHt)) < lScale then begin
    lScale := h/(lMosaic.MaxHt);
  end;
  //lHasRender := false;
  if lMosaic.HOverlap < 0 then
   lColInc := -1
  else
   lColInc := 1;
  if lMosaic.VOverlap < 0 then begin
   lRow := lMosaic.Rows;
   lRowInc := -1;
  end else begin
   lRow := 1;
   lRowInc := 1;
  end;
  while (lRow >= 1) and (lRow <= lMosaic.Rows) do begin
   if lMosaic.HOverlap < 0 then
     lCol := lMosaic.Cols
   else
     lCol := 1;
   lineWid := 0;
   while (lCol >= 1) and (lCol <= lMosaic.Cols) do begin
     lOrient := (lMosaic.Orient[lCol,lRow] and kOrientMask);
     lRender := (lMosaic.Orient[lCol,lRow] and kRenderSliceOrient) = kRenderSliceOrient;
     if lRender then begin
      //DrawRender (lScale*lMosaic.Pos[lCol,lRow].X,lScale*lMosaic.Pos[lCol,lRow].Y,lScale*lMosaic.dim[lCol,lRow].X,lScale*lMosaic.dim[lCol,lRow].Y, lOrient);
      DrawRender (lScale*lMosaic.Pos[lCol,lRow].X,lScale*lMosaic.Pos[lCol,lRow].Y,lScale*lMosaic.dim[lCol,lRow].X,lScale*lMosaic.dim[lCol,lRow].Y,lMosaic.Slices[lCol,lRow], lOrient);
      //lHasRender := true;
     end else begin
       case lOrient of
         kAxialOrient: DrawAx (lScale*lMosaic.Pos[lCol,lRow].X,lScale*lMosaic.Pos[lCol,lRow].Y,lScale*lMosaic.dim[lCol,lRow].X,lScale*lMosaic.dim[lCol,lRow].Y,lMosaic.Slices[lCol,lRow]);
         kCoronalOrient: DrawCor (lScale*lMosaic.Pos[lCol,lRow].X,lScale*lMosaic.Pos[lCol,lRow].Y,lScale*lMosaic.dim[lCol,lRow].X,lScale*lMosaic.dim[lCol,lRow].Y,lMosaic.Slices[lCol,lRow]);
         kSagRightOrient: DrawSag (lScale*lMosaic.Pos[lCol,lRow].X,lScale*lMosaic.Pos[lCol,lRow].Y,lScale*lMosaic.dim[lCol,lRow].X,lScale*lMosaic.dim[lCol,lRow].Y,lMosaic.Slices[lCol,lRow]);
         kSagLeftOrient: DrawSagMirror (lScale*lMosaic.Pos[lCol,lRow].X,lScale*lMosaic.Pos[lCol,lRow].Y,lScale*lMosaic.dim[lCol,lRow].X,lScale*lMosaic.dim[lCol,lRow].Y,lMosaic.Slices[lCol,lRow]);
       end;
       if lMosaic.Text[lCol,lRow] then
          TextLabelXY(lScale*(lMosaic.Pos[lCol,lRow].X+0.5*lMosaic.dim[lCol,lRow].X),lScale*(lMosaic.Pos[lCol,lRow].Y+lMosaic.dim[lCol,lRow].Y), lMosaic.Slices[lCol,lRow], lFontScale, lOrient,lDec);
       //X,Y,lSlice: single; lOrient,lDec
     end;
     lCol := lCol + lColInc;
   end;//col
   lRow := lRow+lRowInc;
  end;//row
  lineWid := lLineWid;
  GLCrossLine(lMosaic, lScale, lLineWid);
  fZoomScale := lZoomScale; //restore zoomScale
  fZoomCenter := lZoomCenter;
end;
{$ENDIF}

destructor TSlices2D.Destroy;
begin
  lineVerts := nil;
  inherited;
end;
end.

