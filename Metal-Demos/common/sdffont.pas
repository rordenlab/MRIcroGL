unit sdffont; //generic signed distance field typeface
{$mode objfpc}{$H+}
interface
{$include glopts.inc} //{ $DEFINE NEWMSDF}

uses
  VectorMath, Classes, SysUtils, math, strutils, Dialogs;

const
    kMaxChar = 2048; //maximum number of characters on screen, if >21845 change TPoint3i to uint32 and set glDrawElements to GL_UNSIGNED_INT
type
    TMetric = Packed Record //each vertex has position and texture coordinates
      UVl,UVr,UVt, UVb, h,l, w, t,xadv   : single; //position coordinates
    end;
    TMetrics = record
      M : array [0..255] of TMetric;
      lineHeight, base: single;
      {$IFDEF NEWMSDF}
      distanceRange: single;
      {$ENDIF}
    end;
    Txyuv = packed Record //each vertex has position and texture coordinates
      x,y : TScalar; //position coordinates
      {$IFDEF NEWMSDF}
      pad0, pad1: TScalar; //padding for Metal alignment
      {$ENDIF}
      u,v : TScalar; //texture coordinates
      {$IFDEF NEWMSDF}
      screenPxRange, pad2: TScalar; //padding for Metal alignment
      {$ENDIF}

    end;
    TRotMat = Packed Record //https://en.wikipedia.org/wiki/Rotation_matrix
      Xx,Xy, Yx,Yy: single;
    end;
    TQuad = array [0..3] of Txyuv; //each character rectangle has 4 vertices
TVertUniforms = record //Uniforms for vertex shader
  viewportSize: TVec2;
end;
TQuads = array[0..(kMaxChar-1)] of TQuad;

  TSDFFont = class
  private
    nChar: integer;
    isRedraw: boolean;
    quads: TQuads;
    metrics: TMetrics;
    fontClr: TVec4;
    procedure CharOut(x,y,scale: single; rx: TRotMat; asci: byte);
  public
    property NumChar: integer read nChar write nChar;
    property Redraw: boolean read isRedraw write isRedraw;
    property FontColor: TVec4 read fontClr write fontClr;
    property QuadVerts: TQuads read quads;
    procedure ClearText; //remove all previous drawn text
    procedure TextOut(x,y,scale: single; s: string); overload; //add line of text
    procedure TextOut(x,y,scale, angle: single; s: string); overload; //add line of text
    procedure TextColor(red,green,blue: byte); overload;
    function BaseHeight: single;
    function LineHeight: single;
    function TextWidth(scale: single; s: string): single;
    constructor Create(fnm : string; out success: boolean);
  end;

implementation


procedure TSDFFont.TextColor(red,green,blue: byte); overload;
begin
  FontClr.r := red/255;
  FontClr.g := green/255;
  FontClr.b := blue/255;
end;

procedure printf(s: string);
begin
{$IFDEF Darwin}
writeln(s);
{$ELSE}
showmessage(s);
{$ENDIF}
end;

{$IFDEF NEWMSDF}
function LoadMetricsJson(fnm: string; out fnt: TMetrics): boolean;
//load JSON format created by
// https://github.com/Jam3/msdf-bmfont
//Identical attributes to Hiero ASCII FNT format, just saved in JSON
const
  idKey = '"unicode"';
var
   id, strBlockStart, strBlockEnd, subBlockStart, subBlockEnd: integer;
   str: string;
   f: textfile;
   size, scaleW, scaleH: single;
function GetFntVal(key: string; blockStart, blockEnd: integer): single;
var
   p, pComma: integer;
begin
  result := 0;
  p := PosEx(key, str, blockStart);
  if (p < 1) or (p > blockEnd) then exit;
  p :=  p + length(key)+1;
  pComma := PosEx(',',str,p);
  if (pComma <= p) or (pComma > blockEnd) then pComma := blockEnd;
  result := strtofloatdef(copy(str,p, pComma-p), 0);
end; //nested GetFntVal()
begin
  if (fnm <> '') then
     fnm := changefileext(fnm,'.json');
  result := false;
  for id := 0 to 255 do begin
      fnt.M[id].UVl := 0;
      fnt.M[id].UVb := 0;
      fnt.M[id].UVr := 0;
      fnt.M[id].UVt := 0;
      fnt.M[id].w := 0;
      fnt.M[id].h := 0;
      fnt.M[id].l := 0;
      fnt.M[id].t := 0;
      fnt.M[id].xadv := 0; //critical to set: fnt format omits non-graphical characters (e.g. DEL): we skip characters whete X-advance = 0
  end;
  if not fileexists(fnm) then begin
    printf('Unable to find '+fnm);
   exit;
  end;
  AssignFile(f, fnm);
  Reset(f);
  ReadLn(f, str);
  CloseFile(f);
  strBlockStart := PosEx('"atlas"',str,1);
  strBlockEnd := PosEx('}',str, strBlockStart);
  if (strBlockStart < 1) or (strBlockEnd < 1) then begin
     printf('Error: no "atlas" section (old msdf format?)'+fnm);
     exit;
  end;
  size := GetFntVal('"size"', strBlockStart, strBlockEnd);
  fnt.distanceRange := GetFntVal('"distanceRange"', strBlockStart, strBlockEnd);
  //fnt.sizeDivDistanceRange := size / fnt.distanceRange;
  scaleW := GetFntVal('"width"', strBlockStart ,strBlockEnd);
  scaleH := GetFntVal('"height"', strBlockStart, strBlockEnd);
  fnt.base := GetFntVal('"size"', strBlockStart, strBlockEnd);
  strBlockStart := PosEx('"metrics"',str,strBlockEnd);
  strBlockEnd := PosEx('}',str, strBlockStart);
  fnt.lineHeight := GetFntVal('"lineHeight"', strBlockStart ,strBlockEnd) * fnt.base;
  //printf(format('%g -> %g %d', [fnt.base, fnt.lineHeight, strBlockEnd]));
  if (fnt.distanceRange <= 0) or (size <= 0) or (scaleW <= 0) or (scaleH <= 0) then begin
     printf('msdf file corrupted: '+fnm);
     exit;
  end;
  strBlockStart := 1;
  repeat
        strBlockStart := PosEx(idKey,str,strBlockStart);
        if strBlockStart < 1 then continue;
        strBlockEnd := PosEx('}',str, strBlockStart);
        if strBlockEnd < strBlockStart then
           break;
        id := round(GetFntVal(idKey, strBlockStart, strBlockEnd));
        if id = 0 then begin
           strBlockStart := strBlockEnd;
           continue;
        end;
        fnt.M[id].xadv := GetFntVal('"advance"', strBlockStart, strBlockEnd) * size;
        //if (id < 35) then writeln(format('%d->%d %d : %g', [strBlockStart, strBlockEnd, id, fnt.M[id].xadv ]));
        //read planeBounds : world coordinate
        subBlockStart := PosEx('planeBounds',str,strBlockStart);
        if (subBlockStart <= 0) or (subBlockStart >= strBlockEnd) then begin
          strBlockStart := strBlockEnd;
          continue;
        end;
        fnt.M[id].l := GetFntVal('"left"', subBlockStart, strBlockEnd);
        fnt.M[id].w := GetFntVal('"right"', subBlockStart, strBlockEnd) - fnt.M[id].l;
        fnt.M[id].t := GetFntVal('"top"', subBlockStart, strBlockEnd);
        fnt.M[id].h := fnt.M[id].t - GetFntVal('"bottom"', subBlockStart, strBlockEnd);
        fnt.M[id].t := GetFntVal('"bottom"', subBlockStart, strBlockEnd);
        fnt.M[id].l *= size;
        fnt.M[id].w *= size;
        fnt.M[id].t *= size;
        fnt.M[id].h *= size;
        //if (id < 35) then writeln(format('a %g::%g, %g::%g', [fnt.M[id].l, fnt.M[id].w,fnt.M[id].t, fnt.M[id].h]));
        subBlockStart := PosEx('atlasBounds',str,strBlockStart);
        subBlockEnd := PosEx('}',str,subBlockStart);
        fnt.M[id].UVl := GetFntVal('"left"', subBlockStart, subBlockEnd);
        fnt.M[id].UVt := GetFntVal('"top"', subBlockStart, subBlockEnd);
        fnt.M[id].UVr := GetFntVal('"right"', subBlockStart, subBlockEnd);
        fnt.M[id].UVb := GetFntVal('"bottom"', subBlockStart, subBlockEnd);
        //if (id = 83) then writeln(format('newMSDF %g::%g, %g::%g', [fnt.M[id].UVl, fnt.M[id].UVr,fnt.M[id].UVb, fnt.M[id].UVt]));
        fnt.M[id].UVl := GetFntVal('"left"', subBlockStart, subBlockEnd) / scaleW;
        fnt.M[id].UVt := 1.0 - (GetFntVal('"bottom"', subBlockStart, subBlockEnd) / scaleH);
        fnt.M[id].UVr := GetFntVal('"right"', subBlockStart, subBlockEnd) / scaleW;
        fnt.M[id].UVb := 1.0 - (GetFntVal('"top"', subBlockStart, subBlockEnd) / scaleH);
        strBlockStart := subBlockEnd;
  until strBlockStart < 1;
  //if (scaleW < 1) or (scaleH < 1) then exit;
  result := true;
end; //LoadMetricsJson()
{$ELSE} //if NEWMSDF else old MSDF JSON format
function LoadMetricsJson(fnm: string; out fnt: TMetrics): boolean;
//load JSON format created by
// https://github.com/Jam3/msdf-bmfont
//Identical attributes to Hiero ASCII FNT format, just saved in JSON
const
  idKey = '"id"';
var
   pages, id, strBlockStart, strBlockEnd: integer;
   str: string;
   scaleW, scaleH: single;
   f: textfile;
function GetFntVal(key: string): single;
var
   p, pComma: integer;
begin
  result := 0;
  p := PosEx(key,str,strBlockStart);
  if (p < 1) or (p > strBlockEnd) then exit;
  p :=  p + length(key)+1;
  pComma := PosEx(',',str,p);
  if (pComma <= p) or (pComma > strBlockEnd) then exit;
  result := strtofloatdef(copy(str,p, pComma-p), 0);
end; //nested GetFntVal()
begin
  if (fnm <> '') then
     fnm := changefileext(fnm,'.json');
  result := false;
  for id := 0 to 255 do begin
      fnt.M[id].UVl := 0;
      fnt.M[id].UVb := 0;
      fnt.M[id].UVr := 0;
      fnt.M[id].UVt := 0;
      fnt.M[id].w := 0;
      fnt.M[id].h := 0;
      fnt.M[id].l := 0;
      fnt.M[id].t := 0;
      fnt.M[id].xadv := 0; //critical to set: fnt format omits non-graphical characters (e.g. DEL): we skip characters whete X-advance = 0
  end;
    if not fileexists(fnm) then begin
       printf('Unable to find '+fnm);
       exit;
    end;
    AssignFile(f, fnm);
    Reset(f);
    ReadLn(f, str);
    CloseFile(f);
  strBlockStart := PosEx('"common"',str,1);
  strBlockEnd := PosEx('}',str, strBlockStart);
  if (strBlockStart < 1) or (strBlockEnd < 1) then begin
     printf('Error: no "common" section');
     exit;
  end;
  fnt.lineHeight := GetFntVal('"lineHeight"');
  fnt.base := GetFntVal('"base"');
  scaleW := GetFntVal('"scaleW"');
  scaleH := GetFntVal('"scaleH"');
  pages := round(GetFntVal('"pages"'));
  if (pages <> 1) then begin
     printf('Only able to read single page fonts');
     exit;
  end;
  strBlockStart := 1;
  repeat
        strBlockStart := PosEx(idKey,str,strBlockStart);
        if strBlockStart < 1 then continue;
        strBlockEnd := PosEx('}',str, strBlockStart);
        if strBlockEnd < strBlockStart then
           break;
        id := round(GetFntVal(idKey));
        if id = 0 then begin
           strBlockStart := strBlockEnd;
           continue;
        end;
        fnt.M[id].UVl := GetFntVal('"x"');
        fnt.M[id].UVb := GetFntVal('"y"');
        fnt.M[id].w := GetFntVal('"width"');
        fnt.M[id].h := GetFntVal('"height"');
        fnt.M[id].l := GetFntVal('"xoffset"');
        fnt.M[id].t := GetFntVal('"yoffset"');
        fnt.M[id].xadv := GetFntVal('"xadvance"');
        strBlockStart := strBlockEnd;
  until strBlockStart < 1;
  if (scaleW < 1) or (scaleH < 1) then exit;
  for id := 0 to 255 do begin //normalize from pixels to 0..1
      fnt.M[id].t := fnt.base - (fnt.M[id].h + fnt.M[id].t);
      fnt.M[id].UVl:=fnt.M[id].UVl/scaleW;
      fnt.M[id].UVb:=fnt.M[id].UVb/scaleH;
      fnt.M[id].UVr := fnt.M[id].UVl + (fnt.M[id].w/scaleW);
      fnt.M[id].UVt := fnt.M[id].UVb + (fnt.M[id].h/scaleH);
  end;
  result := true;
  id := 65;
  id := 83;
end; //LoadMetricsJson()
{$ENDIF}

procedure Rot(xK,yK, x,y: single; r: TRotMat; out Xout, Yout: single);
// rotate points x,y and add to constant offset xK,yK
begin
     Xout := xK + (x * r.Xx) + (y * r.Xy);
     Yout := yK + (x * r.Yx) + (y * r.Yy);
end; //Rot()

procedure TSDFFont.CharOut(x,y,scale: single; rx: TRotMat; asci: byte);
var
  q: TQuad;
  x0,x1,y0,y1, screenPxRange: single;
begin
  if metrics.M[asci].w = 0 then exit; //nothing to draw, e.g. SPACE character
  if nChar > kMaxChar then nChar := 0; //overflow!
  x0 := (scale * metrics.M[asci].l);
  x1 := x0 + (scale * metrics.M[asci].w);
  y0 := (scale * metrics.M[asci].t);
  y1 := y0 + (scale * metrics.M[asci].h);
  Rot(x,y, x0, y0, rx, q[0].x, q[0].y);
  Rot(x,y, x0, y1, rx, q[1].x, q[1].y);
  Rot(x,y, x1, y0, rx, q[2].x, q[2].y);
  Rot(x,y, x1, y1, rx, q[3].x, q[3].y);
  q[0].u := metrics.M[asci].UVl;
  q[1].u := q[0].u;
  q[2].u := metrics.M[asci].UVr;
  q[3].u := q[2].u;
  q[0].v := metrics.M[asci].UVt;
  q[1].v := metrics.M[asci].UVb;
  q[2].v := q[0].v;
  q[3].v := q[1].v;
  {$IFDEF NEWMSDF}
  screenPxRange := scale * metrics.distanceRange;
  //printf(format('dxRange %g scale %g screenPxRange %g', [metrics.distanceRange, scale, screenPxRange])); //to do: compute scale factor!
  screenPxRange := max(screenPxRange, 1.0);
  q[0].screenPxRange := screenPxRange;
  q[1].screenPxRange := screenPxRange;
  q[2].screenPxRange := screenPxRange;
  q[3].screenPxRange := screenPxRange;
  {$ENDIF}
  quads[nChar] := q;
  isRedraw := true;
  nChar := nChar+ 1;
end; //CharOut()

procedure TSDFFont.TextOut(x,y,scale, angle: single; s: string); overload;
var
  i: integer;
  asci: byte;
  rx: TRotMat;
begin
  angle := DegToRad(angle);
  rx.Xx := cos(angle);
  rx.Xy := -sin(angle);
  rx.Yx := sin(angle);
  rx.Yy := cos(angle);
  if length(s) < 1 then exit;
  for i := 1 to length(s) do begin
      asci := ord(s[i]);
      if metrics.M[asci].xadv = 0 then continue; //not in dataset
      CharOut(x,y,scale,rx,asci);
      Rot(x,y, (scale * metrics.M[asci].xadv),0, rx, x, y);
  end;
end; //TextOut()

procedure TSDFFont.TextOut(x,y,scale: single; s: string); overload;
begin
     TextOut(x,y,scale,0,s);
end; //TextOut()

function TSDFFont.BaseHeight: single;
begin
  result := metrics.base;
end; //BaseHeight()

function TSDFFont.LineHeight: single;
begin
     result := metrics.lineHeight;
end; //LineHeight()

function TSDFFont.TextWidth(scale: single; s: string): single;
var
  i: integer;
  asci: byte;
begin
  result := 0;
  if length(s) < 1 then exit;
  for i := 1 to length(s) do begin
      asci := ord(s[i]);
      if metrics.M[asci].xadv = 0 then continue; //not in dataset
      result := result + (scale * metrics.M[asci].xadv);
  end;
end; //TextWidth()

procedure TSDFFont.ClearText;
begin
  nChar := 0;
end; //ClearText()

constructor TSDFFont.Create(fnm : string; out success: boolean);
begin
  fontClr := vec4(1,1,1,1);
  nChar := 0;
  isRedraw := false;
  success := LoadMetricsJson(fnm, metrics);
end; //Create()

end.
