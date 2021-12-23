unit mtlclrbar; //OpenGL and Metal differ in only in first 2 lines
{$DEFINE METALAPI}

{$mode objfpc}{$H+}
{$IFDEF METALAPI}
{$modeswitch objectivec1}
{$ENDIF}
interface

uses
  {$IFDEF METALAPI}
  MetalPipeline, MetalUtils, MetalControl, Metal, VectorMath, mtlfont,
  {$ELSE}
  retinahelper,
  glcorearb, gl_core_utils, glfont, OpenGLContext,
  {$ENDIF}
   SimdUtils,
  Classes, SysUtils, Graphics,  math, dialogs;

type
 TLUTminmax = packed record
   LUT : TLUT;
   mn,mx: single;
 end;
const
  kMaxClrBar = 32;
type
  TGPUClrbar = class
  private
         LUTs: array [1..kMaxClrBar] of TLUTminmax;
         nLUTs, scrnW, scrnH, num_vbo_face2d: integer;
         SizeFrac , RulerPix: Single;
         FontClr,BackClr, RulerClr: TRGBA;
         fisVisible, fisVertical, fisTopOrRight, isRedraw, isText: boolean;
         {$IFDEF METALAPI}
         indexBuffer, vertexBuffer: MTLBufferProtocol;
         mtlControl: TMetalControl;
         pipeline: TMetalPipeline;
         {$ELSE}
         uniform_viewportSize: GLint;
         vbo_face2d, vao_point2d, vbo_point, shaderProgram: GLuint;
         glControl: TOpenGLControl;
         {$ENDIF}
         txt: TGPUFont;
         procedure CreateClrbar;
         procedure CreateCmRuler(ClrBarThick: integer);
         procedure ScreenSize(nLUT,Width,Height: integer);
         procedure CreateTicksText(mn,mx: single; BarLength, BarTop, BarThick, fntScale: single);
         procedure SetVertical(isV: boolean);
         procedure SetVisible(isV: boolean);
         procedure SetTopOrRight(isTR: boolean);
         procedure SetBackColor(c: TRGBA);
         procedure SetRulerColor(c: TRGBA);
         procedure SetFontColor(c: TRGBA);
         procedure SetSizeFrac(f: single);
         procedure SetRulerPix(pix: single);
         {$IFDEF METALAPI}
         procedure CreateStrips();
         procedure InitShader();
         {$ENDIF}
  public
    property isVisible : boolean read fisVisible write SetVisible;
    property isVertical : boolean read fisVertical write SetVertical;
    property isTopOrRight : boolean read fisTopOrRight write SetTopOrRight;
    property Number: integer read nLUTs write nLUTs;
    property BackColor : TRGBA read BackClr write SetBackColor;
    property FontColor : TRGBA read FontClr write SetFontColor;
    property RulerColor : TRGBA read RulerClr write SetRulerColor;
    property SizeFraction : single read SizeFrac write SetSizeFrac;
    property RulerPixels : single read RulerPix write SetRulerPix;
    function PanelFraction (): single; //size of all color tables and surrounding border
    procedure Draw(nLUT: integer); overload; //must be called while TOpenGLControl is current context
    procedure Draw(); overload; //must be called while TOpenGLControl is current context
    procedure SetLUT(index: integer; LUT: TLUT; mn,mx: single; isFromZero: boolean = false);
    {$IFDEF METALAPI}
    constructor Create(fromView: TMetalControl);
    {$ELSE}
    constructor Create(fromView: TOpenGLControl);
    {$ENDIF}
    destructor Destroy; override;
  end;

implementation

uses
  graphTicks;

const
    kBlockSz = 8192;

{$IFDEF METALAPI}
type
  TVtxClr = packed Record //each vertex has position and texture coordinates
    vtx   : TVec4; //position coordinates
    clr : TVec4; //texture coordinates
  end;
   TRGBAx = TVec4;
{$ELSE}
type
  TPoint3f = Packed Record
    x,y,z: single;
  end;

TVtxClr = Packed Record
  vtx   : TPoint3f; //vertex coordinates
  clr : TRGBA;
end;
TRGBAx = TRGBA;

const
//the 'flat' in GLSL code below uses nearest neighbor for colorbars: useful for atlases with discrete colors
    kVert2D ='#version 330'
+#10'layout(location = 0) in vec3 Vert;'
+#10'layout(location = 3) in vec4 Clr;'
+#10'flat out vec4 vClr;'
+#10'uniform vec2 ViewportSize;'
+#10'void main() {'
+#10'    vec2 ptx = Vert.xy;'
+#10'    ptx -= (ViewportSize/2.0);'
+#10'    gl_Position = vec4((ptx / (ViewportSize/2)), 0.0, 1.0);'
+#10'    //gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);'
+#10'    vClr = Clr;'
+#10'}';
    kFrag2D = '#version 330'
+#10'flat in vec4 vClr;'
+#10'out vec4 color;'
+#10'void main() {'
+#10'    color = vClr;'
+#10'}';
{$ENDIF}

var
g2Dvnc: array of TVtxClr;
g2Drgba : TRGBAx;
g2DNew: boolean;
gnface: integer;

function TGPUClrbar.PanelFraction (): single;
begin
  result := 0.0;
  if (not isVisible) or (nLUTs < 1) then exit; //nothing to do
  result := sizeFrac*((nLUTs * 2)+0.5);
end;

destructor TGPUClrbar.Destroy;
begin
  g2Dvnc := nil;
  txt.Free;
  inherited;
end;

procedure nglBegin();
begin
     g2DNew := true;
end;

procedure nglColor4ub (r,g,b,a: byte);
begin
{$IFDEF METALAPI}
g2Drgba.r := (r/255 );
g2Drgba.g := (g/255 );
g2Drgba.b := (b/255 );
g2Drgba.a := (a/255 );
{$ELSE}
  g2Drgba.r := r ;
  g2Drgba.g := g ;
  g2Drgba.b := b ;
  g2Drgba.a := a ;
{$ENDIF}
end;

procedure nglVertex3f(x,y,z: single);
var
  i: integer;
begin
  i := gnface; //array indexed from 0 not 1
  gnface := gnface + 1;
  if (gnface+1) > length(g2Dvnc) then
     setlength(g2Dvnc, length(g2Dvnc)+kBlockSz);
   g2Dvnc[i].vtx.X := x;
   g2Dvnc[i].vtx.Y := y;
   g2Dvnc[i].vtx.Z := z;
   g2Dvnc[i].clr := g2Drgba;
   if not g2DNew then exit;
   g2DNew := false;
   g2Dvnc[gnface] := g2Dvnc[i];
   gnface := gnface + 1;
end;

procedure nglVertex2fr(x,y: single);
begin
     nglVertex3f(round(x),round(y), -1);
end;

procedure nglEnd;
var
  i: integer;
begin
     //add tail
     if gnface < 1 then exit;
     i := gnface; //array indexed from 0 not 1
     gnface := gnface + 1;
     if gnface > length(g2Dvnc) then
        setlength(g2Dvnc, length(g2Dvnc)+kBlockSz);
     g2Dvnc[i] := g2Dvnc[i-1];
end;

function isSame(x,y: TRGBA): boolean;
begin
     result := (x.r = y.r) and (x.g = y.g) and (x.b = y.b) and (x.a = y.a);
end;

procedure TGPUClrbar.SetBackColor(c: TRGBA);
begin
     if not isSame(c, BackClr) then isRedraw := true;
     BackClr := c;
end;

procedure TGPUClrbar.SetRulerColor(c: TRGBA);
begin
     if not isSame(c, RulerClr) then isRedraw := true;
     RulerClr := c;
end;

procedure TGPUClrbar.SetFontColor(c: TRGBA);
begin
     if not isSame(c, FontClr) then isRedraw := true;
     FontClr := c;
end;

procedure TGPUClrbar.SetRulerPix(pix: single);
begin
     if (pix <> RulerPix) then isRedraw := true;
     RulerPix := pix;
end;

procedure TGPUClrbar.SetSizeFrac(f: single);
begin
     if (f <> sizeFrac) then isRedraw := true;
     sizeFrac := f;
     if sizeFrac < 0.005 then sizeFrac := 0.005;
     if sizeFrac > 0.25 then sizeFrac := 0.25;
end;

procedure TGPUClrbar.SetTopOrRight(isTR: boolean);
begin
     if (isTR <> fisTopOrRight) then isRedraw := true;
     fisTopOrRight := isTR;
end;

procedure TGPUClrbar.SetVisible(isV: boolean);
begin
     if (isV <> fisVisible) then isRedraw := true;
     fisVisible := isV;
end;

procedure TGPUClrbar.SetVertical(isV: boolean);
begin
     if (isV <> fisVertical) then isRedraw := true;
     fisVertical := isV;
end;

procedure TGPUClrbar.SetLUT(index: integer; LUT: TLUT; mn,mx: single; isFromZero: boolean);
var
  j,k: integer;
  frac: single;
begin
  if (index > kMaxClrBar) or (index < 1) then exit;
  LUTs[index].LUT := LUT;
  if (mn > mx) then begin
    frac := mx;
    mx := mn;
    mn := frac;
  end;
  LUTs[index].mn := mn;
  LUTs[index].mx := mx;
  isRedraw := true;
  if not isFromZero then exit;
  if (mn = mx) then exit;
  if ((mn > 0) or (mx < 0)) then begin //range does not cross zero
    if (mn > 0) then
      frac := mn/mx
    else
       frac := abs(mx)/abs(mn);
    for j := 1 to 255 do begin
       k := round (255 * (frac + ((1-frac) * j/255)));
       LUTs[index].LUT[j] := lut[k];
    end;
  end;
end;

procedure TGPUClrbar.ScreenSize(nLUT,Width,Height: integer);
begin
     if (nLUTs = nLUT) and (Width = scrnW) and (Height = scrnH) then exit;
     scrnW := Width;
     scrnH := Height;
     nLUTs := nLUT;
     isRedraw := true;
end;

{$IFDEF METALAPI}
constructor TGPUClrbar.Create(fromView: TMetalControl);
{$ELSE}
constructor TGPUClrbar.Create(fromView: TOpenGLControl);
const
    kATTRIB_VERT = 0;  //vertex XYZ are positions 0,1,2
    kATTRIB_CLR = 3;   //color RGBA are positions 3,4,5,6
{$ENDIF}
begin
  {$IFDEF METALAPI}
  mtlControl := fromView;
  {$ELSE}
  glControl := fromView;
  {$ENDIF}
  scrnH := 0;
  SizeFrac := 0.05;
  FontClr := setRGBA(255, 255, 255, 255);
  BackClr := setRGBA(0,0,0,156);
  RulerClr := setRGBA(0,255,0,255);
  RulerPix := 0.0;
  fisVisible := true;
  fisVertical := false;
  fisTopOrRight := false;
  isRedraw := true;
  Txt := TGPUFont.Create(ResourceFile('Roboto', 'png'),  isText, fromView); //<-multi-channel channel fonts glmtext
  {$IFDEF METALAPI}

  {$ELSE}
  glControl.MakeCurrent();
  shaderProgram :=  initVertFrag(kVert2D, kFrag2D);
  uniform_viewportSize := glGetUniformLocation(shaderProgram, pAnsiChar('ViewportSize'));
  //
  num_vbo_face2d := 0;
  vbo_point := 0;
  vao_point2d := 0;
  vbo_face2d := 0;
  glGenVertexArrays(1, @vao_point2d);
  glGenBuffers(1, @vbo_face2d);
  glGenBuffers(1, @vbo_point);
  glBindVertexArray(vao_point2d);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  //Vertices
  glVertexAttribPointer(kATTRIB_VERT, 3, GL_FLOAT, GL_FALSE, sizeof(TVtxClr), PChar(0));
  glEnableVertexAttribArray(kATTRIB_VERT);
  //Color
  glVertexAttribPointer(kATTRIB_CLR, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof(TVtxClr), PChar( sizeof(TPoint3f)));
  glEnableVertexAttribArray(kATTRIB_CLR);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
  glFinish;
  glControl.ReleaseContext;
  {$ENDIF}
end;

FUNCTION specialsingle (var s:single): boolean;
//returns true if s is Infinity, NAN or Indeterminate
//4byte IEEE: msb[31] = signbit, bits[23-30] exponent, bits[0..22] mantissa
//exponent of all 1s =   Infinity, NAN or Indeterminate
CONST kSpecialExponent = 255 shl 23;
VAR Overlay: LongInt ABSOLUTE s;
BEGIN
  IF ((Overlay AND kSpecialExponent) = kSpecialExponent) THEN
     RESULT := true
  ELSE
      RESULT := false;
END;

procedure TGPUClrbar.CreateTicksText(mn,mx: single; BarLength, BarTop, BarThick, fntScale: single);
var
  lStep,lRange, t, MarkerSzX,MarkerSzY, lPosX, lPosY, StWid: double;
  isInvert: boolean;
  ticDecimals: integer;
  ticStep: double;
  St: string;
begin
  if (mx = mn) or (BarThick = 0) or (BarLength = 0) then exit;
  if specialsingle(mn) or specialsingle(mx) then exit;
  if (mx < mn) then begin
    t := mx;
    mx := mn;
    mn := t;
  end;
  isInvert :=  (mn < 0) and (mx < 0);
  MarkerSzX := BarThick * 0.2;
  if (MarkerSzX < 1) then MarkerSzX := 1;
  if not fisVertical then begin
     MarkerSzY := MarkerSzX;
     MarkerSzX := 1;
  end else
      MarkerSzY := 1;
  //next: compute increment
  SelectTicks(mn, mx, lStep, ticStep, ticDecimals);
  lRange := abs(mx - mn); //full range, in case mn < 0 and mx > 0
  nglColor4ub (FontClr.r,FontClr.g,FontClr.b,255);//outline
  repeat
        if not fisVertical then begin
           lPosX :=   (lStep-mn)/lRange*BarLength;
           if isInvert   then
              lPosX :=   BarLength - lPosX;
           lPosX := lPosX + BarThick;
           lPosY := BarTop;
        end else begin
           lPosX := BarTop + BarThick;
           lPosY :=  (lStep-mn)/lRange*BarLength;
           if isInvert   then
              lPosY :=   BarLength - lPosY;
           lPosY := lPosY + BarThick;
        end;
        nglColor4ub (FontClr.r,FontClr.b,FontClr.b,255);//outline
        nglBegin();
          nglVertex2fr(lPosX-MarkerSzX,lPosY-MarkerSzY);
          nglVertex2fr(lPosX-MarkerSzX,lPosY+MarkerSzY);
          nglVertex2fr(lPosX+MarkerSzX,lPosY-MarkerSzY);
          nglVertex2fr(lPosX+MarkerSzX,lPosY+MarkerSzY);
        nglEnd;
        if fntScale > 0 then begin
           St := FloatToStrF(lStep, ffFixed,7,ticDecimals);
           StWid := Txt.TextWidth(fntScale, St);
           if not fisVertical then
              Txt.TextOut(lPosX-(StWid*0.5),BarTop-(BarThick*0.82),fntScale, St)
           else
               Txt.TextOut(lPosX+(BarThick*0.82),lPosY-(StWid*0.5),fntScale,90, St)
        end;
        lStep := lStep + ticStep;
  until lStep > (mx+(ticStep*0.01));
end; //CreateTicksText()

procedure Line(L,T,R,B: single);
begin
  nglBegin();
  nglVertex2fr(L,T );
  nglVertex2fr(L,B);
  nglVertex2fr(R,T);
  nglVertex2fr(R,B);
  nglEnd;
end;

procedure TGPUClrbar.CreateCmRuler(ClrBarThick: integer);
const
    kMargin = 0.0075; //border
    kSmallTick = 0.0075;
    kLineThick = 0.00375;
var
   smallTick, bigTick, lineThick: single;
   L,B, tickStep, tickP, tickH: single;
   scrnMn, margin, i: integer;
begin
     if (RulerPix < 20) or (RulerClr.a = 0) then exit;
     if (not fisVertical) and (RulerPix > scrnW) then exit;
     if (fisVertical) and (RulerPix > scrnH) then exit;
     scrnMn := min(scrnW, scrnH);
     margin := round(scrnMn * kMargin);
     if ClrBarThick > 0 then margin := 0;
     //ruler thickness
     lineThick :=  round(scrnMn * kLineThick);
     lineThick := max(lineThick,1);
     smallTick := round(scrnMn * kSmallTick);
     smallTick := max(smallTick,1);
     tickStep := RulerPix / 10;
     bigTick := smallTick * 2;
     nglColor4ub (RulerClr.r, RulerClr.g, RulerClr.b,RulerClr.a);
     if (fisVertical) and ( fisTopOrRight) then begin
        //right
        L := scrnW-margin-ClrBarThick;
        B :=  round((scrnH/2) - (RulerPix/2));
        Line(L,B ,L-LineThick,B+RulerPix+LineThick);
        for i := 0 to 10 do begin
           tickP := i * tickStep;
           tickH := smallTick;
           if (i = 0) or (i = 5) or (i = 10) then
              tickH := bigTick;
           Line(L-lineThick,B+tickP ,L-lineThick-tickH, B+tickP+lineThick );
        end;
     end;
     if (fisVertical) and (not fisTopOrRight) then begin
        //left
        L := margin+ClrBarThick;
        B :=  round((scrnH/2) - (RulerPix/2));
        Line(L,B ,L+LineThick,B+RulerPix+LineThick);
        for i := 0 to 10 do begin
           tickP := i * tickStep;
           tickH := smallTick;
           if (i = 0) or (i = 5) or (i = 10) then
              tickH := bigTick;
           Line(L+lineThick,B+tickP ,L+lineThick+tickH, B+tickP+lineThick );
        end;
     end;
     if (not fisVertical) and (not fisTopOrRight) then begin
       //bottom
       L :=  round((scrnW/2) - (RulerPix/2));
       B := margin+ClrBarThick;
       Line(L,B+lineThick ,L+RulerPix+LineThick,B);
       for i := 0 to 10 do begin
           tickP := i * tickStep;
           tickH := smallTick;
           if (i = 0) or (i = 5) or (i = 10) then
              tickH := bigTick;
           Line(L+tickP,B+lineThick ,L+tickP+lineThick,B+lineThick+tickH);
       end;
     end;
     if (not fisVertical) and ( fisTopOrRight) then begin
       //top
       L :=  round((scrnW/2) - (RulerPix/2));
       B := scrnH - margin-ClrBarThick;
       Line(L,B-lineThick ,L+RulerPix+LineThick,B);
       for i := 0 to 10 do begin
           tickP := i * tickStep;
           tickH := smallTick;
           if (i = 0) or (i = 5) or (i = 10) then
              tickH := bigTick;
           Line(L+tickP,B-lineThick ,L+tickP+lineThick,B-lineThick-tickH);
       end;
     end;
end;

procedure TGPUClrbar.CreateClrbar;
type
  TInts = array of integer;
label
  123;
var
  faces: TInts;
  BGThick, BarLength,BarThick, i,b,  t,tn: integer;
  frac, pos, fntScale: single;
begin
     if (nLUTs < 1) and (RulerClr.a = 0) then begin
       isRedraw := false;
       exit; //nothing to do
     end;
     BGThick := 0;
     txt.ClearText;
     setlength(g2Dvnc, 0);
     gnface := 0;

     if (nLUTs < 1) then goto 123;
     if (not fisVisible) then goto 123;
     if scrnW < scrnH then
        BarThick := round(scrnW * sizeFrac)
     else
         BarThick := round(scrnH * sizeFrac);
     if BarThick < 1 then goto 123;
     if not fisVertical then
        BarLength := ScrnW - BarThick - BarThick
     else
         BarLength := ScrnH - BarThick - BarThick;
     if BarLength < 1 then exit;
     BGThick := round(BarThick*((nLUTs * 2)+0.5));
     //if isText then
     if fisTopOrRight then begin
        if not fisVertical then
              t := scrnH-BGThick
        else
            t := scrnW - BGThick;
     end else
         t := 0;
     fntScale := 0;
     if (BarThick > 9) and (isText) then begin
        fntScale := (BarThick*0.6)/txt.BaseHeight;
        Txt.TextColor(FontClr.R,FontClr.G,FontClr.B);//black
     end;
     nglColor4ub (BackClr.r, BackClr.g, BackClr.b,BackClr.a);
     nglBegin();
     //background
     if not fisVertical then begin
       nglVertex2fr(0,T+BGThick );
       nglVertex2fr(0,T);
       nglVertex2fr(scrnW,T+BGThick);
       nglVertex2fr(scrnW,T);
     end else begin //else vertical
         nglVertex2fr(T+BGThick,0 );
         nglVertex2fr(T,0);
         nglVertex2fr(T+BGThick,scrnH);
         nglVertex2fr(T+0, scrnH);
     end;
     nglEnd;
     frac := BarLength/255;
     for b := 1 to nLUTs do begin
         nglColor4ub (FontClr.R,FontClr.G,FontClr.B,255);//outline
         nglBegin();
         if not fisVertical then begin
             tn := T+BarThick*(((nLUTs - b) * 2)+1);
             nglVertex2fr(BarThick-1,tn+BarThick+1);
             nglVertex2fr(BarThick-1,tn-1);
             nglVertex2fr(BarLength+BarThick+1,tn+BarThick+1);
             nglVertex2fr(BarLength+BarThick+1,tn-1);
         end else begin
             tn := round(T+BarThick*(((b) * 2)-1.5));
             nglVertex2fr(tn+BarThick+1, BarThick-1);
             nglVertex2fr(tn-1, BarThick-1);
             nglVertex2fr(tn+BarThick+1, BarLength+BarThick+1);
             nglVertex2fr(tn-1, BarLength+BarThick+1);
         end;
         nglEnd;
         pos := BarThick;
         nglBegin();
         if LUTs[b].lut[0].a = 0 then
           nglColor4ub (LUTs[b].lut[1].r, LUTs[b].lut[1].g, LUTs[b].lut[1].b,255)
         else
             nglColor4ub (LUTs[b].lut[0].r, LUTs[b].lut[0].g, LUTs[b].lut[0].b,255);
         if not fisVertical then begin
            nglVertex2fr(pos,tn+BarThick );
            nglVertex2fr(pos,tn);
         end else begin
             nglVertex2fr(tn+BarThick,pos );
             nglVertex2fr(tn,pos);
         end;
         for i := 1 to 255 do begin
           pos := pos + frac;
           nglColor4ub (LUTs[b].lut[i].r, LUTs[b].lut[i].g, LUTs[b].lut[i].b,255);
           if not fisVertical then begin
              nglVertex2fr(pos,tn+BarThick);
              nglVertex2fr(pos,tn);
           end else begin
             nglVertex2fr(tn+BarThick,pos);
             nglVertex2fr(tn,pos);
           end;
         end;
         nglEnd;
         CreateTicksText(LUTs[b].mn,LUTs[b].mx, BarLength, tn, BarThick, fntScale);
     end;
     //copy data to GPU
     123:
     CreateCmRuler(BGThick);
     if Length(g2Dvnc) < 1 then begin
       isRedraw := false;
       exit;
     end;
     {$IFDEF METALAPI}
     vertexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@g2Dvnc[0], gnface*SizeOf(TVtxClr), MTLResourceStorageModeShared);
     CreateStrips();
     {$ELSE}
     glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
     glBufferData(GL_ARRAY_BUFFER, Length(g2Dvnc)*SizeOf(TVtxClr), @g2Dvnc[0], GL_STATIC_DRAW);
     glBindBuffer(GL_ARRAY_BUFFER, 0);

     if (gnface > num_vbo_face2d) then begin //only update if we need to create or enlarge this buffer
       glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbo_face2d);
       setlength(faces,gnface);
       for i := 0 to (gnface-1) do
        faces[i] := i;
       glBufferData(GL_ELEMENT_ARRAY_BUFFER, gnface*sizeof(uint32), @faces[0], GL_STATIC_DRAW);
       glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
       setlength(faces, 0 );
       num_vbo_face2d := gnface;
       glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
     end;
     {$ENDIF}
     setlength(g2Dvnc,0);
     isRedraw := false;
end; // CreateClrbar()

{$IFDEF METALAPI}
procedure TGPUClrbar.CreateStrips();
type
  TInts = array of uint16; //if uint32 then MTLIndexTypeUInt32
var
  i: integer;
  faces: TInts;
begin
  if gnface < 1 then exit;
  setlength(faces,gnface);
  for i := 0 to (gnface-1) do
      faces[i] := i;
  indexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@faces[0], sizeof(uint16) * Length(faces), MTLResourceStorageModeShared);
  setlength(faces, 0 );
  setlength(g2Dvnc,0);
end;

procedure TGPUClrbar.InitShader;
var
 options: TMetalPipelineOptions;
 fnm: string;
begin
	if pipeline <> nil then exit;
	options := TMetalPipelineOptions.Default;
        fnm := ResourceDir + pathdelim + 'colorbar.metal';
        if not fileexists(fnm) then
           fnm := ShaderDir + pathdelim +  '_Colorbar.metal';
        options.libraryName := fnm;
        if not fileexists(options.libraryName) then begin
		writeln('Unable to find ' + fnm);
	end;
        options.pipelineDescriptor := MTLCreatePipelineDescriptor;
        options.pipelineDescriptor.colorAttachmentAtIndex(0).setBlendingEnabled(true);
        options.pipelineDescriptor.colorAttachmentAtIndex(0).setRgbBlendOperation(MTLBlendOperationAdd);
        options.pipelineDescriptor.colorAttachmentAtIndex(0).setAlphaBlendOperation(MTLBlendOperationAdd);
        options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceRGBBlendFactor(MTLBlendFactorSourceAlpha);
        options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceAlphaBlendFactor(MTLBlendFactorSourceAlpha);
        options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationRGBBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
        options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationAlphaBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
	pipeline := MTLCreatePipeline(options);
    MTLSetDepthStencil(pipeline, MTLCompareFunctionAlways, true);
end;
{$ENDIF}

procedure TGPUClrbar.Draw(nLUT: integer); overload;
{$IFDEF METALAPI}
type
  TVertUniforms = record //Uniforms for vertex shader
  viewportSize: TVec2;
end;
var
   vertUniforms: TVertUniforms;
{$ENDIF}
var
   Width,Height: integer;
begin
  if (nLUT < 1) and (RulerClr.a = 0) then exit;
  if (not fisVisible) and (RulerClr.a = 0)  then exit;
  {$IFDEF METALAPI}
  InitShader;
  MTLSetCullMode(MTLCullModeNone);
  MTLSetShader(pipeline);
  Width := mtlControl.ClientWidth;
  Height := mtlControl.ClientHeight;
  ScreenSize(nLUT, Width,Height);
  if isRedraw then
   CreateClrbar();
  if gnface < 1 then exit;
  vertUniforms.viewportSize := V2(Width, Height);
  MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
  MTLSetVertexBuffer(vertexBuffer, 0, 0);
  MTLDrawIndexed (MTLPrimitiveTypeTriangleStrip, gnface, MTLIndexTypeUInt16, indexBuffer, 0); //MTLIndexTypeUInt32
  {$ELSE}
  Width := glControl.ClientWidth;
  Height := glControl.ClientHeight;
  ScreenSize(nLUT, Width,Height);
  glUseProgram(shaderProgram);
  if isRedraw then
     CreateClrbar;
  if gnface < 1 then exit;
  //glViewport(0, 0, Width, Height); //required for form resize
  glControl.SetViewport();
  glDisable(GL_DEPTH_TEST); 
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glUniform2f(uniform_viewportSize, Width, Height);
  glBindVertexArray(vao_point2d);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbo_face2d);
  glDrawElements(GL_TRIANGLE_STRIP, gnface, GL_UNSIGNED_INT, nil);
  glBindVertexArray(0);
  glUseProgram(0);
  {$ENDIF}
  if isText then
     Txt.DrawText();
end;// Draw()

procedure TGPUClrbar.Draw(); overload;
begin
  Draw(nLUTS);
end;

end.

