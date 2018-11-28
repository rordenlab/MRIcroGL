unit slices2D;
//describes 3D volume as 2D slices for display
// library agnostic: call from OpenGL/Vulkan/Metal

{$mode objfpc}{$H+}
interface

{$DEFINE MOSAICS}
uses
  VectorMath, SimdUtils, Classes, SysUtils, dialogs, math, sdffont;
const //slice displays
  kEmptyOrient = 0;
  kAxialOrient = 1;
  kCoronalOrient = 2;
  kSagRightOrient = 4;
  kSagLeftOrient = 8;
  kAxCorSagOrient = 16;
  kMax2DOrient = kAxCorSagOrient;
  kMosaicOrient = 32;
  kRenderOrient = 64;
type
  TVertex2D = record //vertex position/texture coordinates for drawing using GPU: six per bitmap
    position: TVec2;
    //metal requires we align each vertex attribute on 16 byte boundries
    // here these values do double duty to reveal location of mouse clicks
    texDepth: single;
    orient: int32;
    textureCoord: TVec4;
  end;
  TVertex2Ds = array of TVertex2D;
  TMosaicRender = record
  	Left,Bottom,Width,Height: single;
  	Orient: integer; //
  end;
  TMosaicRenders = array of TMosaicRender;
  //TSlices2DView = (Axial, Coronal, Sagittal, AxCorSag);
  TSlices2D = class
  private
    mosRenders: TMosaicRenders;
    sliceVerts: TVertex2Ds;
    numSliceVerts, numLineVerts, numMosRender: integer;
    lineVerts: TVertex2Ds;
    lineWid: single;
    lineXGap: single;
    lineClr: TVec4;
    fontScale,viewPixelHeight: single;
    newLines, newSlices, isRadiological, isLabelOrient: boolean;
    sliceFrac2D: TVec3;
    txt: TSDFFont;
    procedure DrawAx(L,B, W,H, ZFrac: single);
    procedure DrawCor(L,B, W,H, YFrac: single);
    procedure DrawSag(L,B, W,H, XFrac: single);
    procedure DrawSagMirror(L,B, W,H, XFrac: single);

    procedure DrawLine(startX,startY,endX,endY: single);
    procedure DrawLineLBWH(left,bottom,width,height: single);
    procedure DrawCross(L,B, W,H, Xfrac,Yfrac: single);
    procedure DrawRender(L,B, W, H, Slice: single; Orient: integer);
    procedure TextLabelLeft(X,Y: single; Caption: string);
    procedure TextLabelTop(X,Y: single; Caption: string);
  public
    procedure DrawOutLine(L,T,R,B: single);
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
    property MosaicRenders: TMosaicRenders read mosRenders;
    constructor Create(sdffont: TSDFFont);
    destructor Destroy; override;
    //constructor Create();
    procedure Update(volScale: TVec3; w,h: single; Orient: integer; actualH : integer = -1);
    function GetSlice2DFrac(mouseX, mouseY: integer; out Orient: integer): TVec3;
    function FracMM(Mat: TMat4; Dim: TVec3i; out Vox: TVec3i): TVec3;
    {$IFDEF MOSAICS}
    procedure MosaicScale(lMosaicString: string; Mat, InvMat: TMat4; Dim: TVec3i; volScale: TVec3; out w, h: integer);
    procedure UpdateMosaic(lMosaicString: string; Mat, InvMat: TMat4; Dim: TVec3i; volScale: TVec3; w,h: single);
    //Str2Mosaic ( lMosaicString: string; lInvMat: TMat4; var Dim: TVec3i; volScale: TVec3X)
    {$ENDIF}
  end;
  //function Vertex2D(constref position: TVec2; constref coord: TVec4; texDepth: single; orient: int32): TVertex2D;

implementation

uses mainunit;
const
  //kBlockSzQ = 256; //expand "quad2Ds" buffer by chunks of 256 quads
  kBlockSz = 256 * 6; //expand "sliceVerts" and "lineVerts" by chunks

function TSlices2D.GetSlice2DFrac(mouseX, mouseY: integer; out orient: integer): TVec3;
//each quad has six positions 0..5: position 1=L,B, position 2=R,T
var
  xy1, xy2: TVec2;
  x, y, xFrac, yFrac, zFrac: single;
  i: integer;
begin
     orient := -1;
     x := mouseX;

     y := viewPixelHeight - mouseY; //flip vertical axis: mouse coordinates y increase as we go down the screen
     result := Vec3(-1,-1,-1); //assume no hit
     i := 2;
     while (i < numSliceVerts) do begin
           xy1 := sliceVerts[i-1].position;
           xy2 := sliceVerts[i].position;
           i := i + 1;
           if (x < xy1.x) or (x > xy2.x) or (y < xy1.y) or (y > xy2.y) then continue;
           if (xy2.x <= xy1.x) or (xy2.y <= xy1.y)  then continue; //should never happen: avoid divide by zero
           orient := sliceVerts[i-1].orient;
           xFrac := (x-xy1.x)/ (xy2.x - xy1.x);
           yFrac := (y-xy1.y)/ (xy2.y - xy1.y);
           zFrac := sliceVerts[i-1].texDepth;
           if (isRadiological) and ((orient = kAxialOrient) or (orient = kCoronalOrient)) then
              xFrac := 1.0 - xFrac;

           case orient of
             kAxialOrient : result := Vec3(xFrac, yFrac, zFrac); //LR=X, TB=Y, Slice=Z
             kCoronalOrient : result := Vec3(xFrac, zFrac, yFrac); //LR=X, TB=Z, Slice=Y
             kSagRightOrient : result :=  Vec3(zFrac, xFrac, yFrac); //LR=Y, TB=Z, Slice=X
             kSagLeftOrient : result := Vec3(zFrac, 1-xFrac, yFrac); //LR=Y, TB=Z, Slice=X
           end;
           exit;
     end;
     //sliceVerts[numSliceVerts+1] := Vertex2D(V2(L, B),     V4(XFrac, 1, 0, 1), Xfrac, kSagLeftOrient);
     //sliceVerts[numSliceVerts+2] := Vertex2D(V2(L+W, B+H), V4(XFrac, 0, 1, 1), Xfrac, kSagLeftOrient);
end;

  function Vertex2D(constref position: TVec2; constref coord: TVec4; texDepth: single; orient: int32): TVertex2D;
  begin
    result.position := position;
    result.texDepth := texDepth;
    result.orient := orient;
    result.textureCoord := coord;
  end;

constructor TSlices2D.Create(sdffont: TSDFFont);
//constructor TSlices2D.Create();
begin
  txt := sdffont;
  fontScale := 1;
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
  sliceFrac2D := Vec3(0.5, 0.5, 0.5);
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

procedure TSlices2D.DrawRender(L,B, W,H, Slice: single; Orient: integer);
begin
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

procedure TSlices2D.DrawAx(L,B, W,H, ZFrac: single);
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
  if (length(sliceVerts) < (numSliceVerts + 6)) then
     setlength(sliceVerts, (numSliceVerts + kBlockSz));
  newSlices := true;
  sliceVerts[numSliceVerts+0] := Vertex2D(V2(L, B+H), V4(TexL, 1, ZFrac, 1), Zfrac, kAxialOrient);
  sliceVerts[numSliceVerts+1] := Vertex2D(V2(L, B), V4(TexL, 0, ZFrac, 1), Zfrac, kAxialOrient);
  sliceVerts[numSliceVerts+2] := Vertex2D(V2(L+W, B+H), V4(TexR, 1, ZFrac, 1), Zfrac, kAxialOrient);
  sliceVerts[numSliceVerts+3] := Vertex2D(V2(L+W, B), V4(TexR, 0, ZFrac, 1), Zfrac, kAxialOrient);
  sliceVerts[numSliceVerts+4] := sliceVerts[numSliceVerts+2];
  sliceVerts[numSliceVerts+5] := sliceVerts[numSliceVerts+1];
  numSliceVerts := numSliceVerts + 6;
  if lineWid <= 0 then exit;
  //DrawLine(L,B+(sliceFrac2D.y*H),L+W,B+(sliceFrac2D.y*H));
  //DrawLine(L+(sliceFrac2D.x*W), B, L+(sliceFrac2D.x*W), B+H);
  if isRadiological then begin
     DrawCross(L,B,W,H,1.0-sliceFrac2D.x,sliceFrac2D.y)
  end else begin
      DrawCross(L,B,W,H,sliceFrac2D.x,sliceFrac2D.y);
  end;
  if FontScale <= 0.0 then exit;
  if isRadiological then
     TextLabelLeft(L,B+(H * 0.5),'R')
  else
      TextLabelLeft(L,B+(H * 0.5),'L');
  TextLabelTop(L+(W * 0.5),B+H,'A');
end;

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
  sliceVerts[numSliceVerts+0] := Vertex2D(V2(L, B+H), V4(TexL, YFrac, 1, 1), Yfrac, kCoronalOrient);
  sliceVerts[numSliceVerts+1] := Vertex2D(V2(L, B), V4(TexL, YFrac, 0, 1), Yfrac, kCoronalOrient);
  sliceVerts[numSliceVerts+2] := Vertex2D(V2(L+W, B+H), V4(TexR, YFrac, 1, 1), Yfrac, kCoronalOrient);
  sliceVerts[numSliceVerts+3] := Vertex2D(V2(L+W, B), V4(TexR, YFrac, 0, 1), Yfrac, kCoronalOrient);
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
  //AddQuad(L,B, W,H, XFrac, kSagRightOrient);
  if (length(sliceVerts) < (numSliceVerts + 6)) then
     setlength(sliceVerts, (numSliceVerts + kBlockSz));
  newSlices := true;
  sliceVerts[numSliceVerts+0] := Vertex2D(V2(L, B+H), V4(XFrac, 0, 1, 1), Xfrac, kSagRightOrient);
  sliceVerts[numSliceVerts+1] := Vertex2D(V2(L, B), V4(XFrac, 0, 0, 1), Xfrac, kSagRightOrient);
  sliceVerts[numSliceVerts+2] := Vertex2D(V2(L+W, B+H), V4(XFrac, 1, 1, 1), Xfrac, kSagRightOrient);
  sliceVerts[numSliceVerts+3] := Vertex2D(V2(L+W, B), V4(XFrac, 1, 0, 1), Xfrac, kSagRightOrient);
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
  sliceVerts[numSliceVerts+0] := Vertex2D(V2(L, B+H),   V4(XFrac, 1, 1, 1), Xfrac, kSagLeftOrient);
  sliceVerts[numSliceVerts+1] := Vertex2D(V2(L, B),     V4(XFrac, 1, 0, 1), Xfrac, kSagLeftOrient);
  sliceVerts[numSliceVerts+2] := Vertex2D(V2(L+W, B+H), V4(XFrac, 0, 1, 1), Xfrac, kSagLeftOrient);
  sliceVerts[numSliceVerts+3] := Vertex2D(V2(L+W, B),   V4(XFrac, 0, 0, 1), Xfrac, kSagLeftOrient);
  sliceVerts[numSliceVerts+4] := sliceVerts[numSliceVerts+2];
  sliceVerts[numSliceVerts+5] := sliceVerts[numSliceVerts+1];
  numSliceVerts := numSliceVerts + 6;
  if lineWid <= 0 then exit;
  //DrawLine(L,B+(sliceFrac2D.z*H),L+W,B+(sliceFrac2D.z*H));
  //DrawLine(L+(sliceFrac2D.y*W), B, L+(sliceFrac2D.y*W), B+H);
  DrawCross(L,B,W,H,1-sliceFrac2D.y,sliceFrac2D.z);
end;

procedure  TSlices2D.Update(volScale: TVec3; w,h: single; orient: integer; actualH : integer = -1);
var
  texL,texB,texW,texH, texwhratio, whratio: single;
  scale1col, scale1row, scale: single;
begin
  if (actualH  > 0) then
     viewPixelHeight := actualH
  else
      viewPixelHeight := h;
  numLineVerts := 0;
  //numQuads := 0;
  numSliceVerts := 0;
  if (w < 0.1) or (h < 0.1) then exit;
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
  if (orient = kAxialOrient) then begin //axial
    texwhratio := volScale.x / volScale.y;
    if texwhratio < whratio then
       texW := texH *  texwhratio
    else
        texH := texW * 1/texwhratio;
    DrawAx(texL,texB,texW,texH, sliceFrac2D.z);
  end else if (orient = kCoronalOrient) then begin //coronl
    texwhratio := volScale.x / volScale.z;
    if texwhratio < whratio then
      texW := texH *  texwhratio
    else
       texH := texW * 1/texwhratio;
    DrawCor(texL,texB, texW,texH,  sliceFrac2D.y);
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
  end else begin //default to multislice view (orient = kAxCorSagOrient)
    texwhratio := (volScale.x+volScale.y) / (volScale.y+volScale.z);
    scale1row := 0;
    scale1col := 0;
    if texwhratio > whratio then begin
      scale := (texw)/(volScale.x+volScale.y);
      //check for one-column display
      texwhratio := (max(volScale.x,volScale.y)) /(volScale.y+volScale.z+volScale.z);
      if texwhratio > whratio then
         scale1col := (texw)/max(volScale.y,volScale.z)
      else
          scale1col := (texh)/(volScale.y+volScale.z+volScale.z);
      if (scale1col > scale) then
         scale := scale1col
      else
          scale1col := 0;
    end else begin
       scale := (texh)/ (volScale.y+volScale.z);
       //next: check one-row display scaling
       texwhratio := (volScale.x+volScale.x+volScale.y) / max(volScale.y,volScale.z);
       if texwhratio > whratio then
          scale1row := (texw)/(volScale.x+volScale.x+volScale.y)
       else
           scale1row := (texh)/ max(volScale.y,volScale.z);
       if (scale1row > scale) then
          scale := scale1row
       else
           scale1row := 0;
    end;
    //axial
    texH := volScale.y * scale;
    texW := volScale.x * scale;
    DrawAx(texL,texB, texW,texH, sliceFrac2D.z);
    //coronal
    if (scale1row <> 0) then
       texL := texL + texW
    else
       texB := texB+texH;
    texH := volScale.z * scale;
    DrawCor(texL,texB, texW,texH, sliceFrac2D.y);
    //sagittal
    if (scale1col <> 0) then
        texB := texB+texH
    else
        texL := texL+texW;
    texW := volScale.y * scale;
    DrawSag(texL,texB, texW,texH, sliceFrac2D.x);
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

function TSlices2D.FracMM(Mat: TMat4; Dim: TVec3i; out vox: TVec3i): TVec3;
var
  vox0: TVec3;
begin
     //result := sliceFrac2D; exit;
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

{$IFDEF MOSAICS}
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

function SliceMM (lSliceFrac: single; lOrient: integer; Dim: TVec3i; Mat: TMat4): single;
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

function SliceXY(lOrient: integer; volScale: TVec3; pix: integer): TPointF;
begin
  lOrient := (lOrient and kOrientMask);
  case lOrient of
    kAxialOrient,kCoronalOrient: result.X := pix*volScale.X;//screen L/R corresponds to X
    kSagRightOrient,kSagLeftOrient: result.X := pix*volScale.Y;//screen L/R corresponds to Y dimension
    else result.X := 0;
  end;//case
  case lOrient of
    kAxialOrient: result.Y := pix*volScale.Y;//screen vert is Y
    kCoronalOrient,kSagRightOrient,kSagLeftOrient: result.Y := pix*volScale.Z;//screen vert is Z dimension
    else result.Y := 0;
  end;//case
end;

procedure MosaicSetXY (var lMosaic: TMosaic; volScale: TVec3; Dim: TVec3i);
var
  lRow,lCol, mPix: integer;
  lMaxYDim, lMaxY,lX,Hfrac,Vfrac: single;
begin
  mPix := min(min(Dim.x, Dim.y),Dim.z);
  lMosaic.MaxWid := 0;
  if (lMosaic.Cols < 1) or (lMosaic.Rows < 1)  then
    exit;
  for lRow := 1 to lMosaic.Rows do begin
    for lCol := 1 to lMosaic.Cols do begin
      lMosaic.Dim[lCol,lRow] := SliceXY(lMosaic.Orient[lCol,lRow], volScale, mPix);
      //showmessage(format('--> %g %g', [lMosaic.Dim[lCol,lRow].X, lMosaic.Dim[lCol,lRow].Y]));
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
          lmm := SliceMM(lMosaic.Slices[lCol,lRow],lMosaic.Orient[lCol,lRow], Dim, Mat);
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
  lFontScale, lScale, lLineWid: single;
  //lHasRender,
  lRender: boolean;
  lOrient,lRowInc,lColInc,lRow,lCol, lDec:integer;
begin
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
end;
{$ENDIF}

destructor TSlices2D.Destroy;
begin
  lineVerts := nil;
  inherited;
end;
end.

