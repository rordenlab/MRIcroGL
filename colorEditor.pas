unit colorEditor;
//describes 3D volume as 2D slices for display
// library agnostic: call from OpenGL/Vulkan/Metal

{$mode objfpc}{$H+}
interface

{$DEFINE MOSAICS}
uses
  slices2D, VectorMath, SimdUtils, Classes, SysUtils, dialogs, math, colorTable, Graphics, nifti;
type
  TColorEditor = class
  private
    nodeSelected : integer;
    numLineVerts: integer;
    refreshCount: integer;
    lineVerts: TVertex2Ds;
    gridBorderPix: single;
    gridScale: single;
    screenWH: TVec2;
    gridClr: TVec4;
    viewPixelHeight: single;
    newLines: boolean;
    procedure MousePosition(mouseX, mouseY: integer; out gridX, gridY: single);
    procedure DrawLine(startX,startY,endX,endY, lineWid: single; startClr, endClr: TVec4);
    procedure DrawQuad(LB,LT,RB,RT: TVec2;  Clr: TVec4);
  public
    property HasNewLines: boolean read newLines write newLines; //flag indicates new lines for GP
    property NumberOfLineVertices : integer read numLineVerts;
    property LineVertices: TVertex2Ds read lineVerts;
    property GridColor: TVec4 read gridClr write gridClr;
    function ColorEditorMouseDown(mouseX, mouseY: integer; deleteNode, addNode: boolean; vol: TNIfTI): boolean;
    function ColorEditorMouseMove(mouseX, mouseY: integer; vol: TNIfTI): boolean;
    function ColorEditorMouseUp(): boolean;
    function ColorEditorDblClick(vol: TNIfTI): boolean;
    constructor Create();
    procedure Update(w,h: single; vol: TNIfTI);
  end;

implementation

//uses mainunit;
const
  kGridHWPixScale1 = 256;
  kBlockSz = 256 * 6; //expand  "lineVerts" by chunks

function TColorEditor.ColorEditorDblClick(vol: TNIfTI): boolean;
var
  colorDlg: TColorDialog;
  rgba: TRGBA;
  I: byte;
begin
     result := false;
     if (nodeSelected < 0) or (nodeSelected >= vol.CX.FullColorTable.numnodes) then exit;
     screenWH := Vec2(0,0);
     colorDlg := TColorDialog.Create(nil);
     rgba := vol.CX.FullColorTable.nodes[nodeSelected].rgba;
     I := vol.CX.FullColorTable.nodes[nodeSelected].intensity;
     colorDlg.Color:= RGBToColor(rgba.r, rgba.g, rgba.b);
     if not colorDlg.Execute then exit;
     rgba := setRGBA(Red(colorDlg.Color), Green(colorDlg.Color), Blue(colorDlg.Color),rgba.A);
     vol.CX.ChangeNode(nodeSelected, I,rgba.r,rgba.G,rgba.B,rgba.A);
     //clut.ChangeNode(nodeSelected, I,0,255,0,rgba.A);
     result := true;
end;

procedure TColorEditor.MousePosition(mouseX, mouseY: integer; out gridX, gridY: single);
//convert mouse coordinates to location on grid - values less than 0 and greater than 255 are outside the editor
begin
  if (gridScale <= 0) then begin
     gridX := -1;
     gridY := -1;
     exit;
  end;
  //gridHWPix := kGridHWPixScale1 * gridScale;
  gridX := (mouseX-gridBorderPix)/gridScale;
  gridY := (mouseY-gridBorderPix)/gridScale;
  gridY := kGridHWPixScale1 - gridY;
end;

function TColorEditor.ColorEditorMouseUp(): boolean;
begin
  result := nodeSelected >= 0;
  nodeSelected := -1;
end;

function TColorEditor.ColorEditorMouseDown(mouseX, mouseY: integer; deleteNode, addNode: boolean; vol: TNIfTI): boolean;
const
  kCloseHit = 5;
var
  i: integer;
  X,Y: single;
begin
  result := false;
  if (vol.CX.FullColorTable.numnodes < 2) then exit;
  screenWH := Vec2(0,0);
  MousePosition(mouseX, mouseY, X, Y);
  if (X < 0) or (X > kGridHWPixScale1) then exit;
  if (Y < 0) or (Y > kGridHWPixScale1) then exit;
  nodeSelected := -1;
  if (addNode) then begin
     vol.CX.AddNode(round(X),round(Y));
     exit(true);
  end;
  for i := 0 to (vol.CX.FullColorTable.numnodes-1) do begin
      if (abs(X- vol.CX.FullColorTable.nodes[i].intensity) <= kCloseHit) and (abs(Y- vol.CX.FullColorTable.nodes[i].rgba.a) <= kCloseHit) then
         nodeSelected := i;
  end;
  if (deleteNode) then
     exit(vol.CX.DeleteNode(nodeSelected));
  result := nodeSelected > 0;
end;

function TColorEditor.ColorEditorMouseMove(mouseX, mouseY: integer; vol: TNIfTI): boolean;
var
  X,Y: single;
  rgba: TRGBA;
begin
  if (nodeSelected < 0) then exit(false);
  result := true;
  if (vol.CX.FullColorTable.numnodes < 2) then exit;
  screenWH := Vec2(0,0);
  MousePosition(mouseX, mouseY, X, Y);
  if (X < 0) or (X > kGridHWPixScale1) then exit;
  if (Y < 0) or (Y > kGridHWPixScale1) then exit;
  rgba := vol.CX.FullColorTable.nodes[nodeSelected].rgba;
  if (Y > 255) then Y := 255;
  if (X > 255) then X := 255;
  vol.CX.ChangeNode(nodeSelected, round(X),rgba.r,rgba.G,rgba.B,round(Y));
end;

constructor TColorEditor.Create();
begin
     numLineVerts := 0;
     refreshCount := -1;
     screenWH := Vec2(0,0);
     gridScale := 0;
     gridBorderPix := 4;
     lineVerts := nil;
     newLines := false;
     gridClr := Vec4(0.4, 0.0, 0.9, 1);
     nodeSelected := -1;
end;

procedure TColorEditor.DrawLine(startX,startY,endX,endY, lineWid: single; startClr, endClr: TVec4);
var
  nx,ny, len: single;
begin
  ny :=  (startX-endX);
  nx :=  (startY-endY);
  len := sqrt(sqr(nx)+sqr(ny));
  if (len = 0) then exit;
  nx := 0.5*lineWid*nx/len;
  ny := 0.5*lineWid*ny/len;
  if (numLineVerts+6) > length(lineVerts) then
     setlength(lineVerts, length(lineVerts)+kBlockSz);
  lineVerts[numLineVerts +0].textureCoord := startClr;
  lineVerts[numLineVerts +1].textureCoord := startClr;
  lineVerts[numLineVerts +2].textureCoord := endClr;
  lineVerts[numLineVerts +5].textureCoord := endClr;
  lineVerts[numLineVerts+0].position := Vec2(startX+nx,startY-ny);
  lineVerts[numLineVerts+1].position := Vec2(startX-nx,startY+ny);
  lineVerts[numLineVerts+2].position := Vec2(endX+nx,endY-ny);
  lineVerts[numLineVerts+3] := lineVerts[numLineVerts+1];
  lineVerts[numLineVerts+4] := lineVerts[numLineVerts+2];
  lineVerts[numLineVerts+5].position := Vec2(endX-nx,endY+ny);
  numLineVerts := numLineVerts + 6;
end;

procedure TColorEditor.DrawQuad(LB,LT,RB,RT: TVec2;  Clr: TVec4);
var
  i: integer;
begin
  if (numLineVerts+6) > length(lineVerts) then
     setlength(lineVerts, length(lineVerts)+kBlockSz);
  for i := 0 to 5 do
      lineVerts[numLineVerts +i].textureCoord := Clr;
  lineVerts[numLineVerts+0].position := LB;
  lineVerts[numLineVerts+1].position := LT;
  lineVerts[numLineVerts+2].position := RB;
  lineVerts[numLineVerts+3] := lineVerts[numLineVerts+1];
  lineVerts[numLineVerts+4] := lineVerts[numLineVerts+2];
  lineVerts[numLineVerts+5].position := RT;
  numLineVerts := numLineVerts + 6;
end;


procedure  TColorEditor.Update(w,h: single; vol: TNIfTI);
const
  kHistoHt = 48;
var
  histScale, gridHWPix, lineFat,lineWid, minHW, texL,texB,texW,texH,x: single;
  startXY, endXY, startXY2, endXY2: TVec2;
  startClr, endClr: TVec4;
  i: integer;
function clr(n: integer): TVec4;
begin
   result := Vec4(vol.FullColorTable.nodes[n].rgba.r/255, vol.FullColorTable.nodes[n].rgba.g/255, vol.FullColorTable.nodes[n].rgba.b/255, 1);
end;
function xy(n: integer): TVec2;
begin
   result := Vec2(texL+texW * vol.FullColorTable.nodes[n].intensity/255 , texB + texH * vol.FullColorTable.nodes[n].rgba.a/255);
end;
begin
  if (vol.FullColorTable.numnodes < 2) then begin
     numLineVerts := 0;
     exit;
  end;
  if (vol.RefreshCount = refreshCount) and (screenWH.x = w) and (screenWH.y = h) then exit;
  screenWH := Vec2(w,h);
  refreshCount := vol.RefreshCount;
  viewPixelHeight := h;
  minHW := min(w,h);
  numLineVerts := 0;
  if (minHW < 2) then exit;
  newLines := true;
  //gridScale := trunc(minHW / 1024)+1;
  gridScale := trunc( (minHW-1) / (kGridHWPixScale1 * 2.25))+1;
  gridHWPix := gridScale * kGridHWPixScale1;
  texL := gridBorderPix;
  texB := h - gridHWPix-gridBorderPix;
  texH := gridHWPix;
  texW := gridHWPix;
  //draw grid
  lineWid := gridScale;
  for i := 0 to 4 do begin
    x := (i * gridScale * kGridHWPixScale1) * 0.25;
    DrawLine(texL+x,texB,texL+x,texB+texH, lineWid, gridClr, gridClr);
    DrawLine(texL,texB+x,texL+texW,texB+x, lineWid, gridClr, gridClr);
  end;
  //black->white gradient below: dark-bright pixels
  lineWid := gridScale*8;
  DrawLine(texL,texB-gridScale*8,texL+texW,texB-gridScale*8, lineWid, vec4(0,0,0,1), vec4(1,1,1,1));
  //clear->soled gradient to right: alpha opacity
  lineWid := gridScale*8;
  DrawLine(texL+texW+gridScale*8,texB,texL+texW+gridScale*8, texB+texH, lineWid, vec4(gridClr.r,gridClr.b,gridClr.g,0), gridClr);
  //draw color table: node color is table color, horizontal is image intensity, vertical is opacity
  lineWid := gridScale*6;
  startClr := clr(0);
  startXY := xy(0);
  for i := 1 to (vol.FullColorTable.numnodes-1) do begin
      endClr := clr(i);
      endXY := xy(i);
      DrawLine(startXY.X,startXY.Y, endXY.X, endXY.Y, lineWid, startClr, endClr);
      startClr := endClr;
      startXY := endXY;
  end;
  //draw color table: node color is table color, horizontal is image intensity, vertical is opacity
  lineFat := gridScale*10;
  lineWid := gridScale*8;
  for i := 0 to (vol.FullColorTable.numnodes-1) do begin
      endClr := clr(i);
      endXY := xy(i);
      startXY := endXY;
      endXY.x := endXY.x - 0.5 * lineFat;
      startXY.x := startXY.x + 0.5 * lineFat;
      //startXY+= scale;
      //endXY+= scale;
      DrawLine(startXY.X,startXY.Y, endXY.X, endXY.Y, lineFat, gridClr, gridClr);
      endClr := clr(i);
      endXY := xy(i);
      startXY := endXY;
      endXY.x := endXY.x - 0.5 * lineWid;
      startXY.x := startXY.x + 0.5 * lineWid;
      DrawLine(startXY.X,startXY.Y, endXY.X, endXY.Y, lineWid, endClr, endClr);
  end;
    //draw current window min..max as proportion of full range
  if (Vol.VolumeMax <= Vol.VolumeMin) then exit;
  texB := h - gridHWPix-gridBorderPix;
  texB := texB - (gridScale * 18) - (gridScale * kHistoHt);
  texH := (gridScale * (kHistoHt+4));
  x := gridHWPix * (Vol.DisplayMin-Vol.VolumeMin)/(Vol.VolumeMax-Vol.VolumeMin);
  startXY := Vec2(texL+x,texB);
  startXY2 := Vec2(texL+x,texB +texH);
  x := gridHWPix * (Vol.DisplayMax-Vol.VolumeMin)/(Vol.VolumeMax-Vol.VolumeMin);
  endXY := Vec2(texL+x,texB);
  endXY2 := Vec2(texL+x,texB +texH);
  endClr := gridClr;
  endClr.A := 0.3;
  DrawQuad(startXY,startXY2,endXY,endXY2,endClr);
  //draw histogram
  texB := h - gridHWPix-gridBorderPix;
  texB := texB - (gridScale * 16) - (gridScale * kHistoHt);
  texL := gridBorderPix;
  histScale :=  (gridScale * kHistoHt)/255; //each histogram 0..255
  endClr := gridClr * 0.5;
  endClr.A := 0.8;
  startXY := Vec2(texL,texB);
  startXY2 := Vec2(texL,texB +(vol.Histogram[0].A * histScale));
  for i := 1 to (255) do begin
    endXY := Vec2(startXY.X+gridScale,texB);
    endXY2 := Vec2(startXY.X+gridScale,texB + (vol.Histogram[i].A * histScale));
    DrawQuad(startXY,startXY2,endXY,endXY2,endClr);
    startXY := endXY;
    startXY2 := endXY2;
  end;
end; //Update

end.

