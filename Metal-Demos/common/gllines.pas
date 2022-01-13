unit gllines;
//openGL lines

{$mode objfpc}{$H+}
{$IFNDEF METALAPI}
 {$include glopts.inc}
{$ENDIF}

interface


uses
 {$IFDEF LCLCocoa}retinahelper,{$ENDIF}
 {$IFDEF COREGL}glcorearb, {$ELSE} gl, glext,{$ENDIF}
  gl_core_utils, VectorMath, SimdUtils, Classes, SysUtils, Graphics, OpenGLContext, dialogs;

type
  TGPULines = class
  private
         uniform_viewportSize: GLint;
         {$IFDEF COREGL}
         vaoLine2D, vboLine2D,
         {$ELSE}
         displayLst,
         {$ENDIF}
         shaderProgram: GLuint;
         numVertices: integer;
         LineWid: single;
         LineClr: TRGBA;
         isRedraw: boolean;
         glControl: TOpenGLControl;
  public
    property NumberOfVertices : integer read numVertices;
    property LineWidth : single read LineWid write LineWid;
    property LineColor : TRGBA read LineClr write LineClr;
    procedure AddLine(startX,startY,endX,endY: single); overload;
    procedure AddLine(startXY, endXY: TVec2); overload;
    procedure AddLine(nodes: TVec2s); overload;
    procedure AddRectangle(Left,Top,Right,Bottom: single);
    procedure ClearLines();
    procedure Draw(); //must be called while TOpenGLControl is current context
    constructor Create(fromView: TOpenGLControl);
  end;

implementation

type
  TPoint3f = Packed Record
    x,y,z: single;
  end;

TVtxClr = Packed Record
  vtx   : TPoint3f; //vertex coordinates
  clr : TRGBA;
end;

var
    g2Dvnc: array of TVtxClr;
    const
        kBlockSz = 8192;
{$IFDEF COREGL}
        kVert2D ='#version 330'
    +#10'layout(location = 0) in vec3 Vert;'
    +#10'layout(location = 3) in vec4 Clr;'
    +#10'out vec4 vClr;'
    +#10'uniform vec2 ViewportSize;'
    +#10'void main() {'
    +#10'    vec2 ptx = Vert.xy - 0.5;'
    +#10'    ptx -= (ViewportSize/2.0);'
    +#10'    gl_Position = vec4((ptx / (ViewportSize/2.0)), 0.0, 1.0);'
    +#10'    vClr = Clr;'
    +#10'}';
        kFrag2D = '#version 330'
    +#10'in vec4 vClr;'
    +#10'out vec4 color;'
    +#10'void main() {'
    +#10'    color = vClr;'
    +#10'}';
{$ELSE}
        kVert2D = '#version 120'
    +#10'varying vec4 vClr;'
    +#10'uniform vec2 ViewportSize;'
    +#10'void main() {'
    +#10'    vec2 ptx = gl_Vertex.xy - 0.5;'
    +#10'    ptx -= (ViewportSize/2.0);'
    +#10'    gl_Position = vec4((ptx / (ViewportSize/2.0)), 0.0, 1.0);'
    +#10'    vClr = gl_Color;'
    +#10'}';

    //Simple Fragment Shader
    kFrag2D = '#version 120'
    +#10'varying vec4 vClr;'
    +#10'void main() {'
    +#10'    gl_FragColor = vClr;'
    +#10'}';

{$ENDIF}

constructor TGPULines.Create(fromView: TOpenGLControl);
{$IFDEF COREGL}
const
    kATTRIB_VERT = 0;  //vertex XYZ are positions 0,1,2
    kATTRIB_CLR = 3;   //color RGBA are positions 3,4,5,6
{$ENDIF}
begin
  glControl := fromView;
  LineClr := setRGBA(255, 255, 255, 255);
  LineWid := 10;
  isRedraw := true;
  glControl.MakeCurrent();
  shaderProgram :=  initVertFrag(kVert2D, kFrag2D);
  uniform_viewportSize := glGetUniformLocation(shaderProgram, pAnsiChar('ViewportSize'));
  {$IFDEF UNIX}
  if GLErrorStr <> '' then
     writeln(GLErrorStr);
  {$ENDIF}
  {$IFDEF COREGL}
  //setup VAO for lines
  vboLine2D := 0;
  vaoLine2D := 0;
  glGenVertexArrays(1, @vaoLine2D);
  glGenBuffers(1, @vboLine2D);
  glBindVertexArray(vaoLine2d);
  glBindBuffer(GL_ARRAY_BUFFER, vboLine2D);
  // Vertices
  glVertexAttribPointer(kATTRIB_VERT, 3, GL_FLOAT, GL_FALSE, sizeof(TVtxClr), PChar(0));
  glEnableVertexAttribArray(kATTRIB_VERT);
  // Color
  glVertexAttribPointer(kATTRIB_CLR, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof(TVtxClr), PChar( sizeof(TPoint3f)));
  glEnableVertexAttribArray(kATTRIB_CLR);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);  //required, even if we will bind it next
  {$ELSE}
  displayLst := 0;
  {$ENDIF}
  //done
  glFinish;
  glControl.ReleaseContext;
end;

function pt(x,y: single): TPoint3f;
begin
  result.x := x;
  result.y := y;
end;

procedure TGPULines.ClearLines();
begin
     numVertices := 0;
end;
//{$DEFINE STRIP}
{$IFDEF STRIP}
procedure TGPULines.AddLine(startX,startY,endX,endY: single); overload;
var
  i: integer;
  nx,ny, len: single;
begin
  ny :=  (startX-endX);
  nx :=  (startY-endY);
  len := sqrt(sqr(nx)+sqr(ny));
  if (len = 0) then exit;
  nx := 0.5*lineWid*nx/len;
  ny := 0.5*lineWid*ny/len;
  if (numVertices+6) > length(g2Dvnc) then
     setlength(g2Dvnc, length(g2Dvnc)+kBlockSz);
  for i := 0 to 5 do
      g2Dvnc[numVertices +i].clr := LineClr;
  g2Dvnc[numVertices+0].vtx := pt(startX+nx,startY-ny);
  g2Dvnc[numVertices+1].vtx := g2Dvnc[numVertices+0].vtx;
  g2Dvnc[numVertices+2].vtx := pt(startX-nx,startY+ny);
  g2Dvnc[numVertices+3].vtx := pt(endX+nx,endY-ny);
  g2Dvnc[numVertices+4].vtx := pt(endX-nx,endY+ny);
  g2Dvnc[numVertices+5] := g2Dvnc[numVertices+4];
  numVertices := numVertices + 6;
  isRedraw := true;
end;
{$ELSE}
procedure TGPULines.AddLine(startX,startY,endX,endY: single); overload;
var
  i: integer;
  nx,ny, len: single;
begin
  ny :=  (startX-endX);
  nx :=  (startY-endY);
  len := sqrt(sqr(nx)+sqr(ny));
  if (len = 0) then exit;
  nx := 0.5*lineWid*nx/len;
  ny := 0.5*lineWid*ny/len;
  if (numVertices+6) > length(g2Dvnc) then
     setlength(g2Dvnc, length(g2Dvnc)+kBlockSz);
  for i := 0 to 5 do
      g2Dvnc[numVertices +i].clr := LineClr;
  g2Dvnc[numVertices+0].vtx := pt(startX+nx,startY-ny);
  g2Dvnc[numVertices+1].vtx := pt(startX-nx,startY+ny);
  g2Dvnc[numVertices+2].vtx := pt(endX+nx,endY-ny);
  g2Dvnc[numVertices+3].vtx := g2Dvnc[numVertices+1].vtx;
  g2Dvnc[numVertices+4].vtx := g2Dvnc[numVertices+2].vtx;
  g2Dvnc[numVertices+5].vtx := pt(endX-nx,endY+ny);
  numVertices := numVertices + 6;
  isRedraw := true;
end;
{$ENDIF}

procedure TGPULines.AddLine(startXY, endXY: TVec2); overload;
begin
     AddLine(startXY.X, startXY.Y, endXY.X, endXY.Y);
end;

function lineNormal(startXY, endXY: TVec2): TVec2;
//https://stackoverflow.com/questions/1243614/how-do-i-calculate-the-normal-vector-of-a-line-segment
//  if we define dx=x2-x1 and dy=y2-y1, then the normals are (-dy, dx) and (dy, -dx).
begin
  result.x := (endXY.y-startXY.y);
  result.y := (endXY.x-startXY.x);
  result := result.Normalize;
end;

procedure TGPULines.AddLine(nodes: TVec2s); overload;
//https://forum.libcinder.org/topic/smooth-thick-lines-using-geometry-shader
var
   i, j: integer;
   d, lw: TScalar;
   n: array [0..2] of TVec2; //normals - previous[0], current[1], next[2] line
   m: array [0..1] of TVec2; //miter - link start[0], end[1]
begin
     if length(nodes) < 2 then exit;
     if length(nodes) = 2 then begin
        AddLine(nodes[0],nodes[1]);
        exit;
     end;
     //ensure enough storage
     j := (length(nodes) - 1) * 6; //-1 : fence post problem
     if (numVertices+j) > length(g2Dvnc) then
        setlength(g2Dvnc, length(g2Dvnc)+j+kBlockSz);
     //set color
     for i := 0 to (j-1) do
         g2Dvnc[numVertices +i].clr := LineClr;
     lw := lineWid * 0.5;
     //add each link
     n[0] := lineNormal(nodes[0], nodes[1]); //previous link (does not yet exist)
     n[1] := n[0]; //current link
     m[0] := n[1];
     d := abs(m[0].dot(n[1]));
     if d < 0.0001 then d := 0.0001 ; //angle too acute
     //if d <> 0.0 then
     m[0] := (m[0]*lw)/d;
     for i := 1 to (length(nodes)-1) do begin
         if i < (length(nodes)-1) then
            n[2] := lineNormal(nodes[i], nodes[i+1]); //next link
         m[1] := 0.5 * (n[1] + n[2]); //miter end average of current and next link normals
         d := abs(m[1].dot(n[2]));
         if d < 0.0001 then d := 0.0001 ; //angle too acute
         //if d <> 0.0 then
         m[1] := (m[1]*lw)/d;
         g2Dvnc[numVertices+0].vtx := pt(nodes[i-1].x+m[0].x,nodes[i-1].y-m[0].y);
         g2Dvnc[numVertices+1].vtx := pt(nodes[i-1].x-m[0].x,nodes[i-1].y+m[0].y);
         g2Dvnc[numVertices+2].vtx := pt(nodes[i].x+m[1].x,nodes[i].y-m[1].y);
         g2Dvnc[numVertices+3].vtx := g2Dvnc[numVertices+1].vtx;
         g2Dvnc[numVertices+4].vtx := g2Dvnc[numVertices+2].vtx;
         g2Dvnc[numVertices+5].vtx := pt(nodes[i].x-m[1].x,nodes[i].y+m[1].y);
         numVertices := numVertices + 6;
         for j := 0 to 1 do
             n[j] := n[j+1]; //increment one link
         m[0] := m[1];
     end;
end;

procedure TGPULines.AddRectangle(Left,Top,Right,Bottom: single);
var
  i: integer;
begin
  if (numVertices+6) > length(g2Dvnc) then
     setlength(g2Dvnc, length(g2Dvnc)+kBlockSz);
  for i := 0 to 5 do
      g2Dvnc[numVertices +i].clr := LineClr;
  g2Dvnc[numVertices+0].vtx := pt(Left,Top);
  g2Dvnc[numVertices+1].vtx := pt(Left,Bottom);
  g2Dvnc[numVertices+2].vtx := pt(Right,Top);
  g2Dvnc[numVertices+3].vtx := g2Dvnc[numVertices+1].vtx;
  g2Dvnc[numVertices+4].vtx := g2Dvnc[numVertices+2].vtx;
  g2Dvnc[numVertices+5].vtx := pt(Right,Bottom);
  numVertices := numVertices + 6;
  isRedraw := true;
end;

procedure TGPULines.Draw();
{$IFNDEF COREGL}
var
  i: integer;
{$ENDIF}
begin
  if isRedraw then begin
    {$IFDEF COREGL}glBindBuffer(GL_ARRAY_BUFFER, vboLine2D);
    glBufferData(GL_ARRAY_BUFFER, numVertices*SizeOf(TVtxClr), @g2Dvnc[0], GL_STATIC_DRAW);
    {$ELSE}
           if displayLst <> 0 then
              glDeleteLists(displayLst, 1);
           displayLst := glGenLists(1);
           glNewList(displayLst, GL_COMPILE);
           {$IFDEF STRIP}
           glBegin(GL_TRIANGLE_STRIP);
           {$ELSE}
           glBegin(GL_TRIANGLES);
           {$ENDIF}
           for i := 0 to (numVertices - 1) do begin
               //next line used to cause issues with LLVM: https://gitlab.com/freepascal.org/fpc/source/-/issues/39296#note_657746458
               glColor4ub(g2Dvnc[i].clr.R, g2Dvnc[i].clr.G, g2Dvnc[i].clr.B, g2Dvnc[i].clr.A);
               glVertex3f(g2Dvnc[i].vtx.x, g2Dvnc[i].vtx.y, g2Dvnc[i].vtx.z);
           end;
           glEnd();
           glEndList();
    {$ENDIF}
    isRedraw := false;
  end;
  if numVertices < 1 then exit;
  glDisable(GL_CULL_FACE);
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glUseProgram(shaderProgram);
  glUniform2f(uniform_viewportSize, glControl.ClientWidth, glControl.ClientHeight);
  {$IFDEF COREGL}
  glBindBuffer(GL_ARRAY_BUFFER, vboLine2D);
  glBindVertexArray(vaoLine2d);
  {$IFDEF STRIP}
  glDrawArrays(GL_TRIANGLE_STRIP, 0, numVertices);
  {$ELSE}
  glDrawArrays(GL_TRIANGLES, 0, numVertices);
  {$ENDIF}
  glBindVertexArray(0);
  {$ELSE}
  glCallList(displayLst);
  {$ENDIF}
  glUseProgram(0);
end;

end.

