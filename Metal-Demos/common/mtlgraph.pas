unit mtlgraph; //OpenGL and Metal differ in only in first 2 lines
{$DEFINE METALAPI} //<- set by ProjectOptions/CompilerOptions/CustomOptions
{$IFNDEF METALAPI}
{$include glopts.inc}
{$ENDIF}
{$mode objfpc}{$H+}
interface

uses
  lazfileutils, SimdUtils, Classes, SysUtils, dialogs, math, strutils, Controls, VectorMath,
{$IFDEF METALAPI}
 MetalUtils, Metal, MetalPipeline, MetalControl, mtllines,  mtlfont;
{$ELSE}
 {$IFDEF LCLCocoa}retinahelper,{$ENDIF}  //required!
 {$IFDEF COREGL}glcorearb,{$ELSE}gl, glext, {$ENDIF} OpenGLContext, gllines, glfont;
{$ENDIF}
const
    kStyleRaw = 0;
    kStyleDeMean = 1;
    kStyleNormalized = 2; //-1..1 (FSLeyes)
    kStyleNormalized01 = 3; //0..1
    kStylePercent = 4;
    kStyleSqrt = 5;
    kStyleLogN = 6; //ln
    kStyleLog10 = 7; //log10
type
    TGPUGraphLine = record
      min, max, minPositive, mean : single;
      caption  : string[128];
      color: TRGBA;
      vals: TFloat32s;
    end;

  TGPUGraph = class
  private
         numLines, numLinesNoOverwrite : integer;
         Lines: array of TGPUGraphLine;
         isText: boolean;
         scrnSz: TPoint;
         graphLT, graphRB: TVec2;
         gpuTxt: TGPUFont;
         gpuLines: TGPULines;
         minXData, maxXData: single;
         XData: TFloat32s;
         procedure DrawLines(w,h: integer);
         procedure ScaledGlobalMinMax(out mn, mx: single);
         function ScaledValue(line, pt: integer): single;
  public
        Style: integer;
        HorizontalSelection: integer;
        TextColor,BackColor,GridColor, MinorLineColor: TRGBA;
        isRedraw, isMarker, isMinorLines: boolean;
    procedure ClearLines();
    procedure CloneLine();
    procedure DarkColorScheme();
    procedure DarkBlueColorScheme();
    procedure BlueColorScheme();
    procedure GrayColorScheme();
    procedure LightColorScheme();
    function HorizontalClickFrac(X: single): single;
    function HorizontalClickString(X: single): string;
    procedure AddLine(newVals: TFloat32s; newCaption: string; isOverwrite: boolean = false; isOverwriteProtect: boolean = false);
    procedure AddXData(newVals: TFloat32s); //e.g. [0 4 8 12] suggests data evenly spaced one sample every 4 seconds
    function AsText(isSaveXData: boolean = false): TStringList;
    function LoadText(filename: string; isKeepOld: boolean = false): boolean;
    function PointsPerLine: integer;
    {$IFDEF METALAPI}
    constructor Create(fromView: TMetalControl);
    procedure Paint(fromView: TMetalControl);
    procedure SaveBmp(filename: string; hasAlpha: boolean = true);
    {$ELSE}
    constructor Create(fromView: TOpenGLControl);
    procedure Paint(fromView: TOpenGLControl);
    {$ENDIF}
    Destructor  Destroy(); override;
  end;

implementation

uses graphticks;

{$IFDEF METALAPI}
procedure TGPUGraph.SaveBmp(filename: string; hasAlpha: boolean);
begin
     if filename = '' then
        MTLWriteTextureToClipboard(hasAlpha)
     else
         MTLWriteTextureToFile(pChar(filename), hasAlpha);
end;
{$ENDIF}

function TGPUGraph.PointsPerLine: integer;
begin
     if (Lines = nil) then exit(0);
     exit(length(Lines[0].vals));
end;

function TGPUGraph.HorizontalClickFrac(X: single): single;
var
  mn,mx: single;
begin
     result := -1;
     mn := min(graphLT.X, graphRB.X);
     mx := max(graphLT.X, graphRB.X);
     if (X < mn) or (X > mx) or (mn >= mx) then exit;
     result := (X-mn) / (mx-mn);
end;

function TGPUGraph.HorizontalClickString(X: single): string;
var
  i, ptsPerLine, iClick: integer;
  frac, xMin, xMax, xClick: single;
begin
     result := '';
     frac := HorizontalClickFrac(X);
     HorizontalSelection := iClick;
     if (frac < 0.0) or (frac > 1.0) then exit;
     if numLines < 1 then exit; //nothing to do
     ptsPerLine := length(Lines[0].vals);
     HorizontalSelection := -1;
     if (ptsPerLine < 2) then exit;
     if length(XData) <>  ptsPerLine then begin
        xMin := 0;
        xMax := ptsPerLine - 1;
     end else begin
         xMin := minXData;
         xMax := maxXData;
     end;
     xClick := ((xMax - xMin) * frac) + xMin;
     if length(XData) <>  ptsPerLine then begin
        xClick := round(xClick);
        iClick := round(xClick);
     end else begin
         //x values may not be evenly spaced: find sample closest to click
         iClick := 0;
         for i := 1 to (ptsPerLine -1) do
         	if abs(XData[i]-xClick) < abs(XData[iClick]-xClick) then
               iClick := i;
         xClick := XData[iClick];
     end;
     HorizontalSelection := iClick;
     //result := format('%f: ', [xClick]);

     result := format('%3.6g: ', [xClick]);
     for i := 0 to (numLines - 1) do begin
         result += format('%3.6g ', [ ScaledValue(i, iClick)]);
     end;

end;


procedure TGPUGraph.DrawLines(w,h: integer);
var
    jScale, minWH, border, gap, gL,gT,gB,gR, vFrac: single;
    i, j, ptsPerLine: integer;
    markerWid, gridLineWid, lineWid: single; //vertical line space
    maxTextWid, ticMin, ticPos, ticStep, minorStep: double;
    ticDecimals: integer;
    translucent : TRGBA;
    gMin, gMax, gRange, hFrac, pxX, fntScale,fntHeight, StWid: single;
    xMin, xMax: single;
    pxXY: TVec2;
    line: TVec2s;
    St: string;
begin
     if (not isRedraw) and (w = scrnSz.x) and (h = scrnSz.y) then exit;
     scrnSz.x := w;
     scrnSz.y := h;
     gpuLines.ClearLines();
     gpuTxt.ClearText();
     //draw background
     gpuLines.LineColor := BackColor;
     gpuLines.AddRectangle(0, h+1, w+1, 0);
     if numLines < 1 then exit; //nothing to do
     ptsPerLine := length(Lines[0].vals);
     if length(XData) <>  ptsPerLine then begin
        xMin := 0;
        xMax := ptsPerLine - 1;
     end else begin
         xMin := minXData;
         xMax := maxXData;
     end;
     if ptsPerLine < 2 then exit;
     minWH := min(w,h);
     if minWH < 5 then exit;
     gridLineWid := floor(minWH / 300) + 1;
     lineWid := floor(minWH / 150) + 1;
     gpuLines.LineWidth := lineWid;
     if (minWH < 32) then exit;
     ScaledGlobalMinMax(gMin, gMax);
     gRange := gMax - gMin;
     if (gRange < 0) then exit;
     SelectTicks(gMin, gMax, ticMin, ticStep, ticDecimals);
     fntScale := 0.05 * ((minWH)/gpuTxt.BaseHeight);
     fntHeight := fntScale * gpuTxt.BaseHeight;
     gpuTxt.TextColor(TextColor.R, TextColor.G, TextColor.B);
     //compute left border - room for text on left side
     ticPos := ticMin;
     maxTextWid := 0;
     while (ticPos <= gMax) do begin
           St := FloatToStrF(ticPos, ffFixed,7,ticDecimals);
           StWid := gpuTxt.TextWidth(fntScale, St);
           if (maxTextWid < StWid) then
              maxTextWid := StWid;
           ticPos := ticPos + ticStep;
     end;
     if (fntHeight < 12) or ((maxTextWid * 2) > w) then begin
        fntHeight := 0;
        maxTextWid := 0;
     end;
     //
     border := 0.03 * minWH;
     gT := h - border;
     gB := border + round(fntHeight)+2;
     gL := border + maxTextWid;
     gR := w - border;
     graphLT.X := gL;
     graphLT.Y := gT;
     graphRB.X := gR;
     graphRB.Y := gB;
     if gR < gL then gR := gL + 100; //very large text on right
     vFrac := (gT-gB)/gRange;
     //draw vertical grid
     SelectTicks(xMin, xMax, ticMin, ticStep, ticDecimals);

     if (isMinorLines) then begin
        ticPos := ticMin;
        gpuLines.LineWidth:= gridLineWid * 0.5;
        gpuLines.LineColor := MinorLineColor;//setRGBA(0,0,0,255);
        if (ticStep = 3) or (ticStep = 5) or (ticStep = 7) or (ticStep = 9) then begin
           //rationale if ticStep is 3, minor ticks should be at 1 and 2 not 1.5
           //might add toleratance, e.g. 3.0001 ~= 3
          minorStep := 1/ticStep;
          if (ticPos-xMin) > minorStep then
             ticPos -= trunc((ticPos-xMin) / minorStep) * minorStep;
          ticPos := ticPos + (minorStep * ticStep);
          i := 1;
          while (ticPos <= xMax) do begin
              pxX := gL + (gR-gL)*((ticPos - xMin) / (xMax - xMin));
              if (i mod round(ticStep)) <> 0 then
                 gpuLines.AddLine(pxX, gT+lineWid, pxX, gB-lineWid);
              i := i + 1;
              ticPos := ticPos + (minorStep * ticStep);
          end;


        end else begin
          if (ticPos-xMin) > (0.5 * ticStep) then
             ticPos -= ticStep;
          ticPos := ticPos + (0.5 * ticStep);
          while (ticPos <= xMax) do begin
              pxX := gL + (gR-gL)*((ticPos - xMin) / (xMax - xMin));
              gpuLines.AddLine(pxX, gT+lineWid, pxX, gB-lineWid);
              ticPos := ticPos + ticStep;
          end;
        end;
     end;
     gpuLines.LineColor := GridColor;//setRGBA(0,0,0,255);
     gpuLines.LineWidth:= gridLineWid;
     //xMin xMax maxH := (ptsPerLine-1);
     ticPos := ticMin;
     //writeln(format('!<<<<<<<< %.2g %.2g: %g %g', [xMin,xMax, ticMin, ticStep]));
     while (ticPos <= xMax) do begin
         pxX := gL + (gR-gL)*((ticPos - xMin) / (xMax - xMin));
         gpuLines.AddLine(pxX, gT+lineWid, pxX, gB-lineWid);
         St := FloatToStrF(ticPos, ffFixed,7,ticDecimals);
         //St := FloatToStrF(length(XData), ffFixed,7,ticDecimals);
         //St := FloatToStrF(ptsPerLine, ffFixed,7,ticDecimals);
         StWid := gpuTxt.TextWidth(fntScale, St);
         if ((pxX + (0.5*StWid)) < W) and (fntHeight > 0) then
            gpuTxt.TextOut(pxX - (0.5*StWid),border,fntScale, St);
         ticPos := ticPos + ticStep;
     end;
     //draw horizontal grid
     SelectTicks(gMin, gMax, ticMin, ticStep, ticDecimals);
     pxXY.x := gR+lineWid;
     if isMinorLines then begin
        gpuLines.LineWidth:= gridLineWid * 0.5;
        gpuLines.LineColor := MinorLineColor;//setRGBA(0,0,0,255);
        ticPos := ticMin + (0.5 * ticStep);
        while (ticPos <= gMax) do begin
              pxXY.y := gB + ((ticPos-gMin) * vFrac);
              gpuLines.AddLine(pxXY.x, pxXY.y, gL-lineWid, pxXY.y);
              ticPos := ticPos + ticStep;
        end;
     end;
     gpuLines.LineColor := GridColor;//setRGBA(0,0,0,255)
     gpuLines.LineWidth:= gridLineWid;
     ticPos := ticMin;
     while (ticPos <= gMax) do begin
           pxXY.y := gB + ((ticPos-gMin) * vFrac);
           gpuLines.AddLine(pxXY.x, pxXY.y, gL-lineWid, pxXY.y);
           St := FloatToStrF(ticPos, ffFixed,7,ticDecimals);
           StWid := gpuTxt.TextWidth(fntScale, St);
           if (fntHeight > 0) then
              gpuTxt.TextOut((border * 0.5) + maxTextWid - StWid,pxXY.y - (0.5 * fntScale * gpuTxt.BaseHeight),fntScale, St);
           ticPos := ticPos + ticStep;
     end;
     //draw horizontal marker showing volume selection
     //HorizontalSelection := 3;
     hFrac := (gR-gL) / (ptsPerLine-1);
     pxXY.y := gT + lineWid;
     if (HorizontalSelection >= 0) and (HorizontalSelection < ptsPerLine) then begin
        gpuLines.LineWidth := lineWid * 3;
        gpuLines.LineColor := SetRGBA(255,0,0,164);
        pxXY.x := gL + (HorizontalSelection * hFrac);
        gpuLines.AddLine(pxXY.x, pxXY.y, pxXY.x, gB - lineWid);
     end;
     //draw graph lines
     hFrac := (gR-gL) / (xMax-xMin);
     markerWid := lineWid * 2;
     gpuLines.LineWidth := lineWid;
     setlength(line, ptsPerLine);
     for i := 0 to (numLines - 1) do begin
         if length(Lines[i].vals) <> ptsPerLine then continue;
         gpuLines.LineColor := Lines[i].Color;
         for j := 0 to (ptsPerLine-1) do begin
             jScale := j;
             if length(XData) = ptsPerLine then
                jScale := XData[j] - xMin;
             line[j].x := gL + (jScale * hFrac);
             line[j].y := gB + ((ScaledValue(i, j)-gMin) * vFrac);
         end;
         gpuLines.AddLine(line);
         if isMarker then begin
            //draw markers
            for j := 0 to (ptsPerLine-1) do begin
                jScale := j;
                if length(XData) = ptsPerLine then
                   jScale := XData[j] - xMin;
                pxXY.x := gL + (jScale * hFrac);
                pxXY.y := gB + ((ScaledValue(i, j)-gMin) * vFrac);
                gpuLines.AddRectangle(pxXY.x- markerWid, pxXY.y- markerWid, pxXY.x+ markerWid, pxXY.y+ markerWid);
            end; //for each point
         end; //if isMarker
     end;
     //draw captions
     maxTextWid := 0;
     for i := 0 to (numLines - 1) do begin
         St := Lines[i].caption;
         StWid := gpuTxt.TextWidth(fntScale, St);
         if (maxTextWid < StWid) then
            maxTextWid := StWid;
     end;
     if (maxTextWid > 0) and (fntHeight > 0) then begin //draw labels
        translucent := BackColor;
        //translucent.R := 0;
        translucent.A := 200;
       gpuLines.LineColor := translucent;
       gap := border * 0.5;
       gpuLines.AddRectangle(gR - maxTextWid - 4 * gap, gT - (fntHeight), gR+gap, gT -gap - ((numLines+1) * (fntHeight+gap)));
       pxXY.x := gR - maxTextWid;
       for i := 0 to (numLines - 1) do begin
           pxXY.y := gT - ((fntHeight+gap) * (i+2));
           St := Lines[i].caption;
           //St := format('%g..%g', [Lines[i].min, Lines[i].max]);
           if (fntHeight > 0) then
                gpuTxt.TextOut(pxXY.x, pxXY.y,fntScale, St);
           gpuLines.LineWidth := lineWid;
           gpuLines.LineColor := Lines[i].Color;
           pxXY.y := pxXY.y + 0.5 * fntHeight;
           gpuLines.AddLine(pxXY.x - gap, pxXY.y, pxXY.x - 3* gap, pxXY.y);
       end;
     end; //draw labels
     isRedraw := false; //graph is now up to date
end;

{$IFDEF METALAPI}
procedure TGPUGraph.Paint(fromView: TMetalControl);
{$ELSE}
procedure TGPUGraph.Paint(fromView: TOpenGLControl);
{$ENDIF}
begin
  if (fromView.ClientWidth < 2) or (fromView.ClientHeight < 3) then exit();
  //   GLForm1.caption := format('%d %d', [fromView.ClientWidth,fromView.ClientHeight]);
  DrawLines(fromView.ClientWidth,fromView.ClientHeight);
  {$IFDEF METALAPI}
  MTLSetClearColor(MTLClearColorMake(BackColor.R/255, BackColor.G/255, BackColor.B/255, 1));
  MTLBeginFrame();
    gpuLines.Draw();
    if isText then
       gpuTxt.DrawText();
  MTLEndFrame;
  {$ELSE}
  glViewPort(0,0,fromView.ClientWidth, fromView.ClientHeight);
  glClearColor(BackColor.R/255, BackColor.G/255, BackColor.B/255, 1.0); //Set gray background
  glClear(GL_COLOR_BUFFER_BIT);
  glDisable(GL_DEPTH_TEST);
  gpuLines.Draw();
  if isText then
     gpuTxt.DrawText();
  fromView.SwapBuffers;
  {$ENDIF}
end;

function Mix(a,b: TRGBA): TRGBA;
begin
     result.R := (a.R + b.R) div 2;
     result.G := (a.G + b.G) div 2;
     result.B := (a.B + b.B) div 2;
     result.A := (a.A + b.A) div 2;
end;

procedure TGPUGraph.GrayColorScheme();
begin
     TextColor := SetRGBA(0,0,0,255);
     BackColor := SetRGBA(214,214,214,255);
     GridColor := SetRGBA(255,255,255,255);
     MinorLineColor := Mix(BackColor,GridColor);
     isRedraw := true;
end;


procedure TGPUGraph.DarkBlueColorScheme();
begin
     BackColor := SetRGBA(20,78,150,255);
     GridColor := SetRGBA(44,96,162,255);
     TextColor := Mix(GridColor, SetRGBA(255,255,255,255));
     MinorLineColor := Mix(BackColor,GridColor);
     isRedraw := true;
end;

procedure TGPUGraph.BlueColorScheme();
begin
     TextColor := SetRGBA(25,157,203,255);
     BackColor := SetRGBA(231,240,243,255);
     GridColor := SetRGBA(25,157,203,255);
     MinorLineColor := Mix(BackColor,GridColor);
     isRedraw := true;
end;

procedure TGPUGraph.DarkColorScheme();
begin
     TextColor := SetRGBA(255,255,255,255);
     BackColor := SetRGBA(0,0,0,255);
     GridColor := SetRGBA(192,208,192,255);
     MinorLineColor := Mix(BackColor,GridColor);
     isRedraw := true;
end;

procedure TGPUGraph.LightColorScheme();
begin
     TextColor := SetRGBA(0,0,0,255);
     BackColor := SetRGBA(255,255,255,255);
     GridColor := SetRGBA(64,96,64,255);
     MinorLineColor := Mix(BackColor,GridColor);
     isRedraw := true;
end;

{$IFDEF METALAPI}
constructor TGPUGraph.Create(fromView: TMetalControl);
{$ELSE}
constructor TGPUGraph.Create(fromView: TOpenGLControl);
{$ENDIF}
begin
     Style := kStyleRaw;
     //Style := kStyleDeMean;
     //Style := kStyleNormalized;
     //Style := kStyleNormalized01;
     //LightColorScheme();
     //DarkColorScheme();
     BlueColorScheme();
     GrayColorScheme();
     isMarker := false;
     isMinorLines := true;
     XData := nil;
     Lines := nil;
     HorizontalSelection := -1;
     numLines := 0;
     numLinesNoOverwrite := 0;
     gpuLines := TGPULines.Create(fromView);
     {$IFDEF METALAPI}
     gpuTxt := TGPUFont.Create(ResourcePath('Roboto', 'png'),  isText, fromView); //<-multi-channel channel fonts glmtext
     {$ELSE}
     gpuTxt := TGPUFont.Create(ResourceFile('Roboto', 'png'),  isText, fromView); //<-multi-channel channel fonts glmtext
     {$ENDIF}
end;

function lineClr(i: integer): TRGBA;
var
    j: integer;
begin
     result.R := 0;
     result.G := 0;
     result.B := 0;
     result.A := 255;
     j := i mod 6;
     if (j = 0) or (j = 3) or (j = 4) then result.R := 255;
     if (j = 1) or (j = 3) or (j = 5) then result.G := 255;
     if (j = 2) or (j = 4) or (j = 5) then result.B := 255;
     if (i > 5) then begin
       result.R := round(result.R * 0.5);
       result.G := round(result.G * 0.5);
       result.B := round(result.B * 0.5);
     end else if (j > 2) then begin
        result.R := round(result.R * 0.75);
        result.G := round(result.G * 0.75);
        result.B := round(result.B * 0.75);
     end else if (255 = result.G) then
         result.G := 222;

     (*if i > 5 then begin
        result.R := random(255);
        result.G := random(255);
        result.B := random(255);

     end;*)
end;

procedure TGPUGraph.CloneLine();
var
    l: integer;
begin
     if numLines < 1 then exit;
     l := numlines;
     numLines := numLines + 1;
     numLinesNoOverwrite := numLinesNoOverwrite + 1;
     setlength(Lines, numLines);
     setlength(Lines[l].vals, length(Lines[l-1].vals));
     Lines[l].vals := copy(Lines[l-1].vals, low(Lines[l-1].vals), high(Lines[l-1].vals));
     Lines[l].min := Lines[l-1].min;
     Lines[l].mean := Lines[l-1].mean;
     Lines[l].max := Lines[l-1].max;
     Lines[l].caption := Lines[l-1].caption;
     lines[l].color := lineClr(l);
     isRedraw := true;
end;

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

procedure TGPUGraph.AddLine(newVals: TFloat32s; newCaption: string; isOverwrite: boolean = false; isOverwriteProtect: boolean = false);
var
  l, n, i: integer;
  v: single;
  sum: double;
begin
     n := length(newVals);
     if (numLines > 0) and (n <> length(Lines[0].vals)) then
        ClearLines();
     if n < 1 then exit;
     l := numLines;
     if (not isOverwrite) or (l = 0) or (numLines = numLinesNoOverwrite) then
        numLines := numLines + 1
     else
         l := l - 1;
     if (isOverwriteProtect) then
       numLinesNoOverwrite := numLinesNoOverwrite + 1;
     setlength(Lines, numLines);
     setlength(Lines[l].vals, n);
     v := newVals[0];
     if specialsingle(v) then
        v := 0;  //e.g. Inf+
     Lines[l].min :=  v;
     Lines[l].max :=  v;
     Lines[l].minPositive := Infinity;
     sum := 0;
     for i := 0 to (n-1) do begin
         v := newVals[i];
         if specialsingle(v) then
            v := 0;  //e.g. Inf+
         Lines[l].vals[i] := v;
         if (v > Lines[l].max) then
            Lines[l].max := v;
         if (v < Lines[l].min) then
            Lines[l].min := v;
         if (v > 0) and (v < Lines[l].minPositive) then
            Lines[l].minPositive := v;
         sum := sum + v;
     end;
     if (Lines[l].minPositive > Lines[l].max) then //no positive values!
     	Lines[l].minPositive := 0.0;
     Lines[l].mean := sum / n;
     Lines[l].caption := newCaption;
     lines[l].color := lineClr(l);
     isRedraw := true;
end;

procedure TGPUGraph.AddXData(newVals: TFloat32s); //e.g. [0 4 8 12] suggests data evenly spaced one sample every 4 seconds
var
   i: integer;
begin
    setlength(XData,0);
    if length(newVals) < 2 then exit;
    setlength(XData, length(newVals));
    minXData := newvals[0];
    maxXData := minXData;
    for i := 0 to (length(XData)-1) do begin
        //XData[i] := i;
        XData[i] := newVals[i];
        if (XData[i] < minXData) then minXData := XData[i];
        if (XData[i] > maxXData) then maxXData := XData[i];
    end;
    //writeln(format('<<<<<<<< %.2g %.2g', [minXData,maxXData]));
end;

function TGPUGraph.LoadText(filename: string; isKeepOld: boolean = false): boolean;
label
  123;
const
    kTypeUnknown = 0;
    kType1stColXData = 1;
    kType1stColYData = 2;
type
  capt = string[128];
var
  str: string;
  strs, strlst : TStringList;
  nCol, nLinesIn, i, nRow, k, c: integer;
  cols: array of TFloat32s;
  flts: TFloat32s;
  typ: integer = kTypeUnknown;
  captions: array of capt;
  ext, fnm: string;
begin
     result := false;
     if not fileexists(filename) then begin
       showmessage('Unable to find file '+ filename);
       exit;
     end;
     strs := TStringList.Create;
     strlst := TStringList.Create;
     setlength(cols, 0);
     setlength(captions, 0);
     nCol := 0;
     strs.LoadFromFile(filename);
     nLinesIn := strs.Count; //may include comments'#'
     if nLinesIn < 1 then begin
        showmessage('Does not have at least one lines of text: '+filename);
       goto 123;
     end;
     //read input lines
     nRow := 0;
     for i := 0 to (nLinesIn -1) do begin
         str := strs.Strings[i];
         if PosEx('#Col',str) = 1 then begin
            str := copy(str,5,length(str));
            strlst.DelimitedText := str;
            if strlst.Count < 2 then continue;
            c := strtointdef(strlst[0],-1);
            if c < 0 then continue;
            if (c+1) > length(captions) then
               setlength(captions, c+1);
            str := strlst[1];
            if (strlst.Count > 2) then
               for k := 2 to (strlst.Count-1) do
                   str := str +' '+ strlst[k];
            captions[c] :=  str;//strlst[1];
            //showmessage(inttostr(c)+':'+ strlst[1]);
            continue;
         end;
         if (length(str) < 1) or (str[1] = '#') then continue;
         strlst.DelimitedText := str;
         if strlst.Count < 1 then continue;
         if nCol < 1 then begin
            if (strlst.Count = 1) then
               typ := kType1stColYData;
            nCol := strlst.Count;
            setlength(cols, nCol);
            for c := 0 to (nCol - 1) do
                setlength(cols[c],nLinesIn);
         end;
         if nCol <> strlst.Count then begin
           showmessage('File corrupted: Not all rows have the same number of columns');
           goto 123;
         end;
         for c := 0 to (nCol - 1) do
             cols[c][nRow] := strtofloatdef(strlst[c],0);
         nRow := nRow + 1;
     end;
     //showmessage(format('%g : %g', [ cols[0][0], cols[0][j-1] ]));
     ext := upcase(ExtractFileExt(filename));
     fnm := upcase(ExtractFileNameOnly(filename));
     //fnm := upcase(ExtractFileNameWithoutExt(filename));
     if (nRow = 1) and (nCol > 1) then begin //transpose
        setlength(flts, nCol);
        for i := 0 to (nCol-1) do begin
            flts[i] := cols[i][0];
            setlength(cols[i],0);
        end;
        setlength(cols, 1);
        setlength(cols[0], length(flts));
        for i := 0 to (nCol-1) do
            cols[0][i] := flts[i];
        flts := nil;
        nRow := nCol;
        nCol := 1;
     end;
     if (nRow < 2) or (nCol < 1) then begin
        showmessage(format('Found %d rows and %d columns. Need at least two lines of text (without "#" comments) %s',[nRow, nCol, filename]));
        goto 123;
     end;
     if (nCol = 1) then
        typ := kType1stColYData;
     if (typ = kTypeUnknown) and (length(captions) > 1) and (captions[0] = 'XData') then
        typ := kType1stColXData;
     if (typ = kTypeUnknown) and (length(captions) > 1) and (captions[0] <> 'XData') then
        typ := kType1stColYData;
     if (typ = kTypeUnknown) and (length(captions) = 0) and (nCol = 6) and (posex('rp_',extractfilename(filename)) = 1) then
        typ := kType1stColYData;
     if (typ = kTypeUnknown) and (ext = '.1D') then
        typ := kType1stColYData;
     if (typ = kTypeUnknown) and (ext = '.PAR') then
        typ := kType1stColYData; //FSL mcflirt creates ".PAR" files, not to be confused with Philips PAR/REC
     if (typ = kTypeUnknown) and (posex('PS_TSPLOT_ZSTAT1_EV', fnm) > 0) then //FSL
        typ := kType1stColYData;
     if (typ = kTypeUnknown) and (posex('PS_TSPLOT_ZSTAT1_EV', fnm) > 0) then //FSL
        typ := kType1stColYData;
     if (typ = kTypeUnknown) and (posex('PS_TSPLOTC_ZSTAT1_EV', fnm) > 0) then //FSL
        typ := kType1stColYData;
     if (typ = kTypeUnknown) and (posex('TSPLOT_ZSTAT', fnm) > 0) then //FSL
        typ := kType1stColYData;
     if (typ = kTypeUnknown) and (posex('TSPLOTC_ZSTAT', fnm) > 0) then //FSL
        typ := kType1stColYData;
     if (typ = kTypeUnknown) then begin
       i := MessageDlg('Are the first column values the X axis data?', mtConfirmation,[mbYes, mbNo], 0);
       if (i = mrYes) then
          typ := kType1stColXData
       else
           typ := kType1stColYData;
     end;
     //build graph
     if (isKeepOld) and (numLines > 0) and (nRow <> length(Lines[0].vals)) then begin
        isKeepOld := false;
        {$IFDEF UNIX}
        writeln('Unable to keep old graph: number of rows differs');
        {$ENDIF}

     end;
     if (isKeepOld) then
         //
     else
         ClearLines();
     str := extractfilename(filename);
     i := 0;
     if (typ = kType1stColXData) then begin
        setlength(cols[i],nRow);
        AddXData(cols[i]);
        i := 1;
     end;
     for c := i to (nCol - 1) do begin
         setlength(cols[c],nRow);
         if c < length(captions) then
            AddLine(cols[c], captions[c])
         else
             AddLine(cols[c], format('%s [%d]',[str, c-i]));
     end;
     result := true;
     numLinesNoOverwrite := numLines;
     123:
     if nCol > 0 then begin
       for i := 0 to (nCol - 1) do
           setlength(cols[i],0);
       setlength(cols, 0);
     end;
     strs.Free;
     strlst.Free;
     setlength(captions, 0);
end;

function SqrtFcn(v: single): single;
var
  sgn: single;
begin
     sgn := sign(v);
     result := sqrt(abs(v)) * sgn;
end;

function logNFcn(v: single): single;
//https://www.freepascal.org/docs-html/rtl/math/log10.html
// If x is less than or equal to 0 an 'invalid fpu operation' error will occur.
//"symlog" is one approach, but requires choosing 'C'
begin
     if (v <= 0.0) then exit(0.0); // !!! log(0)
     result := Lnxp1(v - 1.0) ;
end;

function log10Fcn(v: single): single;
//https://www.freepascal.org/docs-html/rtl/math/log10.html
// If x is less than or equal to 0 an 'invalid fpu operation' error will occur.
//"symlog" is one approach, but requires choosing 'C'
begin
     if (v <= 0.0) then exit(0.0); // !!! log(0)
     result := Log10(v) ;
end;

function fcn(v: single; style: integer): single;
begin
     case (style) of
           kStyleSqrt: result := sqrtFcn(v);
           kStyleLogN: result := logNFcn(v);
           kStyleLog10: result := log10Fcn(v);
           else result := v;
     end;
end;

function TGPUGraph.ScaledValue(line, pt: integer): single;
var
   inter, slope: single;
begin
     result := 0;
     if line >= numLines then exit;
     if pt >= length(Lines[line].vals) then exit;
     result := Lines[line].vals[pt];
     if Style = kStyleRaw then exit;
     if (Lines[line].Max = Lines[line].Min) then exit(Lines[line].Min);
     case Style of
          kStyleDeMean : inter := -Lines[line].Mean;
          kStyleNormalized : inter := -Lines[line].Min-(0.5 *(Lines[line].Max - Lines[line].Min));
          kStyleNormalized01 : inter := -Lines[line].Min;
          kStylePercent : inter := -Lines[line].Mean;
          else inter := 0;
     end;
     case Style of
          kStyleNormalized : slope := 2/(Lines[line].Max - Lines[line].Min);
          kStyleNormalized01 : slope := 1/(Lines[line].Max - Lines[line].Min);
          kStylePercent : begin
              slope := 1;
              if (Lines[line].Mean <> 0) then
                 slope := 100.0/Lines[line].Mean;
          end
          else slope := 1;
     end;
     result := fcn((result+inter)*slope, style);
end;

procedure TGPUGraph.ScaledGlobalMinMax(out mn, mx: single);
var
   inter, slope, mni, mxi, val: single;
   line: integer;
begin
     mn := -1;
     mx := 1;
     if (Style = kStyleNormalized) then exit;
     if (Style = kStyleNormalized01) then begin
       mn := 0;
       exit;
     end;
     if numLines < 1 then exit;
     for line := 0 to (numLines - 1) do begin
         case Style of
              kStyleDeMean : inter := -Lines[line].Mean;
              //kStyleNormalized : inter := -Lines[line].Mean;
              //kStyleNormalized01 : inter := -Lines[line].Min;
              kStylePercent : inter := -Lines[line].Mean;
              else inter := 0;
         end;
         case Style of
              //kStyleNormalized : slope := 2/(Lines[line].Max - Lines[line].Min);
              //kStyleNormalized01 : slope := 1/(Lines[line].Max - Lines[line].Min);
              kStylePercent : begin
                  slope := 1;
                  if (Lines[line].Mean <> 0) then
                     slope := 100.0/Lines[line].Mean;
              end
              else slope := 1;
         end;
      	 mni := fcn((Lines[line].Min+inter)*slope, style);
      	 mxi := fcn((Lines[line].Max+inter)*slope, style);
         if (Lines[line].MinPositive > 0.0) then begin //e.g. with Log scales, <= 0 is undefined, most extreme value is minimum negative
         	val := fcn((Lines[line].MinPositive+inter)*slope, style);
            mxi := max(mxi, val);
            mni := min(mni, val);
         end;
         if (line = 0) or (mni < mn) then
            mn := mni;
         if (line = 0) or (mxi > mx) then
            mx := mxi;
     end;
     if mn = mx then mx := mn + 1;
end;

function TGPUGraph.AsText(isSaveXData: boolean = false): TStringList;
const
    kDelim = chr(9);
var
  s: string;
  i, j, ptsPerLine: integer;
begin
     result := TStringList.Create;
     if numLines < 1 then exit;
     ptsPerLine := length(Lines[0].vals);
     if ptsPerLine < 1 then exit;
     //save descriptives
     j := 0;
     if isSaveXData then begin
       s := format('#Col%d%sXData', [j, kDelim]);
       result.Add(s);
       j := 1;
     end;
     for i := 0 to (numLines - 1) do begin
         s := format('#Col%d%s%s', [i+j,kDelim, Lines[i].caption])  ;
         result.Add(s);
     end;
     //save data
     DefaultFormatSettings.DecimalSeparator := '.';
     for i := 0 to (ptsPerLine - 1) do begin
         s := '';
         if isSaveXData then begin
            if length(XData) <>  ptsPerLine then
               s := inttostr(i)+kDelim
            else
               s := format('%g', [XData[i]])+kDelim
         end;
         for j := 0 to (numLines - 1) do begin
             if j <> 0 then
                s := s + kDelim;
             s := s + format('%g', [ Lines[j].vals[i] ]);
         end; //for each line
         result.Add(s);
     end; //for each column
end;

procedure TGPUGraph.ClearLines();
var
  i: integer;
begin
     if numLines < 1 then exit;
     setlength(XData,0);
     for i := 0 to (numLines -1) do
         setlength(Lines[i].vals, 0);
     setlength(Lines, 0);
     numLines := 0;
     numLinesNoOverwrite := 0;
end;

destructor TGPUGraph.Destroy();
begin
     ClearLines();
     gpuTxt.Free;
     gpuLines.Free;
     inherited;
end;

end.


