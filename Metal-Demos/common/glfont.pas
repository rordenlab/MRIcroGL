unit glfont;
//openGL Text using distance field fonts https://github.com/libgdx/libgdx/wiki/Distance-field-fonts
//traditional signed-distance field fonts use a single channel (alpha), here we use multi-channel (red,green,blue)
//This can preserve sharp corners in fonts
//  https://github.com/Chlumsky/msdfgen
//  https://github.com/Jam3/msdf-bmfont
{$mode objfpc}{$H+}
{$IFNDEF METALAPI}
 {$include glopts.inc}
{$ENDIF}
interface

uses
  {$IFDEF UNIX}retinahelper,{$ENDIF}
  {$IFDEF COREGL}glcorearb,
  {$ELSE}gl, glext, {$ENDIF} SimdUtils,
  sdffont, VectorMath,  gl_core_utils,
  Dialogs,Classes, SysUtils, Graphics, OpenGLContext, math;

type
TGPUFont = class (TSDFFont)
  private
      {$IFDEF COREGL}vboVtx, vboIdx, vao,{$ELSE} displayLst, {$ENDIF}
      tex, shaderProgram: GLuint;
      uniform_viewportSize, uniform_clr, uniform_tex: GLint;
      glControl: TOpenGLControl;
    function LoadTex(fnm : string): boolean;
    procedure UpdateVbo;
  public
    procedure DrawText(); //must be called while TOpenGLControl is current context
    constructor Create(fnm : string; out success: boolean; fromView: TOpenGLControl); //overlod;
  end;

implementation

const
{$IFDEF COREGL}
{$IFDEF NEWMSDF}
kVert = '#version 330'
+#10'layout(location = 0) in vec3 point;'
+#10'layout(location = 1) in vec2 uvX;'
+#10'uniform vec2 ViewportSize;'
+#10'out vec2 uv;'
+#10'out float screenPxRange;'
+#10'void main() {'
+#10'    uv = uvX;'
+#10'    vec2 ptx = point.xy;'
+#10'    screenPxRange = point.z;'
+#10'    ptx -= (ViewportSize/2.0);'
+#10'    gl_Position = vec4((ptx / (ViewportSize/2.0)), 0.0, 1.0);'
+#10'    //gl_Position = ModelViewProjectionMatrix * vec4(ptx, -0.5, 1.0);'
+#10'}';
kFrag = '#version 330'
+#10'in vec2 uv;'
+#10'in float screenPxRange;'
+#10'out vec4 color;'
+#10'uniform sampler2D tex;'
+#10'uniform vec4 clr;'
+#10'float median(float r, float g, float b) {'
+#10'    return max(min(r, g), min(max(r, g), b));'
+#10'}'
+#10'void main() {'
+#10'  vec3 msd = texture(tex, uv).rgb;'
+#10'  float sd = median(msd.r, msd.g, msd.b) - 0.5;'
+#10'  //float screenPxDistance = sd/fwidth(sd);//old formula'
+#10'  float screenPxDistance = screenPxRange* sd;//new formula'
+#10'  float opacity = clamp(screenPxDistance + 0.5, 0.0, 1.0);'
+#10'  color = vec4(clr.rgb, clr.a * opacity);'
+#10'}';
{$ELSE}
kVert = '#version 330'
+#10'layout(location = 0) in vec2 point;'
+#10'layout(location = 1) in vec2 uvX;'
+#10'uniform vec2 ViewportSize;'
+#10'out vec2 uv;'
+#10'void main() {'
+#10'    uv = uvX;'
+#10'    vec2 ptx = point;'
+#10'    ptx -= (ViewportSize/2.0);'
+#10'    gl_Position = vec4((ptx / (ViewportSize/2.0)), 0.0, 1.0);'
+#10'}';
    kFrag = '#version 330'
+#10'in vec2 uv;'
+#10'out vec4 color;'
+#10'uniform sampler2D tex;'
+#10'uniform vec4 clr;'
+#10'float median(float r, float g, float b) {'
+#10'    return max(min(r, g), min(max(r, g), b));'
+#10'}'
+#10'void main() {'
+#10'  vec3 sample = 1.0 - texture(tex, uv).rgb;'
+#10'  float sigDist = median(sample.r, sample.g, sample.b) - 0.5;'
+#10'  float opacity = clamp(sigDist/fwidth(sigDist) + 0.5, 0.0, 1.0);'
+#10'  color = vec4(clr.r,clr.g,clr.b,1.0 - opacity);'
+#10'}';
{$ENDIF} //if NEWMSDF
{$ELSE}
{$IFDEF NEWMSDF}
kVert = '#version 120'
+#10'uniform vec2 ViewportSize;'
+#10'varying vec2 uv;'
+#10'varying float screenPxRange;'
+#10'void main() {'
+#10'    uv = gl_Color.rg;'
+#10'    vec2 ptx = gl_Vertex.xy;'
+#10'    screenPxRange = gl_Vertex.z;'
+#10'    ptx -= (ViewportSize/2.0);'
+#10'    gl_Position = vec4((ptx / (ViewportSize/2.0)), 0.0, 1.0);'
+#10'}';
    kFrag = '#version 120'
+#10'varying vec2 uv;'
+#10'varying float screenPxRange;'
+#10'uniform sampler2D tex;'
+#10'uniform vec4 clr;'
+#10'float median(float r, float g, float b) {'
+#10'    return max(min(r, g), min(max(r, g), b));'
+#10'}'
+#10'void main() {'
+#10'  vec3 msd = texture2D(tex, uv).rgb;'
+#10'  float sd = median(msd.r, msd.g, msd.b) - 0.5;'
+#10'  float screenPxDistance = screenPxRange * sd;//new formula'
+#10'  //float screenPxDistance = sd/fwidth(sd);//old formula'
+#10'  float opacity = clamp(screenPxDistance + 0.5, 0.0, 1.0);'
+#10'  gl_FragColor = vec4(clr.rgb, opacity);'
+#10'}';

{$ELSE}
kVert = '#version 120'
+#10'uniform vec2 ViewportSize;'
+#10'varying vec2 uv;'
+#10'void main() {'
+#10'    uv = gl_Color.rg;'
+#10'    vec2 ptx = gl_Vertex.xy;'
+#10'    ptx -= (ViewportSize/2.0);'
+#10'    gl_Position = vec4((ptx / (ViewportSize/2.0)), 0.0, 1.0);'
+#10'}';
    kFrag = '#version 120'
+#10'varying vec2 uv;'
+#10'uniform sampler2D tex;'
+#10'uniform vec4 clr;'
+#10'float median(float r, float g, float b) {'
+#10'    return max(min(r, g), min(max(r, g), b));'
+#10'}'
+#10'void main() {'
+#10'  vec3 sample = 1.0 - texture2D(tex, uv).rgb;'
+#10'  float sigDist = median(sample.r, sample.g, sample.b) - 0.5;'
+#10'  float opacity = clamp(sigDist/fwidth(sigDist) + 0.5, 0.0, 1.0);'
+#10'  gl_FragColor = vec4(clr.r,clr.g,clr.b,1.0 - opacity);'
+#10'}';
{$ENDIF} //if NEWMSDF
{$ENDIF}

constructor TGPUFont.Create(fnm: string; out success: boolean; fromView: TOpenGLControl);
{$IFDEF COREGL}
type
    TPoint3i = Packed Record
      x,y,z   : uint16; //vertex indices: for >65535 indices use uint32 and use GL_UNSIGNED_INT for glDrawElements
    end;
const
    kATTRIB_POINT = 0; //XY position on screen
    kATTRIB_UV = 1; //UV coordinates of texture
var
    faces: array of TPoint3i;
    i,j,k: integer;
{$ENDIF}
begin
  inherited Create(fnm, success);
  if not success then exit;
  glControl := fromView;
  tex := 0;
  shaderProgram := 0;
  uniform_viewportSize := 0;
  uniform_clr := 0;
  uniform_tex := 0;
  glControl.MakeCurrent();
  shaderProgram :=  initVertFrag(kVert, kFrag);
  {$IFDEF UNIX}
  if GLErrorStr <> '' then
     writeln(GLErrorStr);
  {$ENDIF}
  if not LoadTex(fnm) then success := false;
  uniform_clr := glGetUniformLocation(shaderProgram, pAnsiChar('clr'));
  uniform_tex := glGetUniformLocation(shaderProgram, pAnsiChar('tex'));
  uniform_viewportSize := glGetUniformLocation(shaderProgram, pAnsiChar('ViewportSize'));
  {$IFDEF COREGL}
  vboVtx := 0;
  vboIdx := 0;
  vao := 0;
  glGenBuffers(1, @vboVtx);
  glBindBuffer(GL_ARRAY_BUFFER, vboVtx);
  glBufferData(GL_ARRAY_BUFFER, kMaxChar * sizeof(TQuad), nil, GL_DYNAMIC_DRAW); //GL_STATIC_DRAW
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glGenVertexArrays(1, @vao);
  glBindVertexArray(vao);
  glBindBuffer(GL_ARRAY_BUFFER, vboVtx);
  {$IFDEF NEWMSDF}
  glVertexAttribPointer(kATTRIB_POINT, 3, GL_FLOAT, GL_FALSE, sizeof(Txyuv), PChar(0));
  glEnableVertexAttribArray(kATTRIB_POINT);
  glVertexAttribPointer(kATTRIB_UV, 2, GL_FLOAT, GL_FALSE, sizeof(Txyuv), PChar(sizeof(single)*3));
  {$ELSE}
  glVertexAttribPointer(kATTRIB_POINT, 2, GL_FLOAT, GL_FALSE, sizeof(Txyuv), PChar(0));
  glEnableVertexAttribArray(kATTRIB_POINT);
  glVertexAttribPointer(kATTRIB_UV, 2, GL_FLOAT, GL_FALSE, sizeof(Txyuv), PChar(sizeof(single)*2));
  {$ENDIF}
  glEnableVertexAttribArray(kATTRIB_UV);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
  glGenBuffers(1, @vboIdx);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboIdx);
  setlength(faces, kMaxChar * 2 ); //each character composed of 2 triangles
  for i := 0 to ((kMaxChar)-1) do begin
      j := i * 2;
      k := i * 4;
      faces[j].x := 0+k;
      faces[j].y := 1+k;
      faces[j].z := 2+k;
      faces[j+1].x := 2+k;
      faces[j+1].y := 1+k;
      faces[j+1].z := 3+k;
  end;
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(faces)*sizeof(TPoint3i), @faces[0], GL_STATIC_DRAW);
  setlength(faces, 0 );
  {$ELSE}
  displayLst := 0;
  {$ENDIF}
  glFinish;
  glControl.ReleaseContext;
end; //Create()

{$IFDEF COREGL}
procedure TGPUFont.UpdateVbo;
begin
  if (self.NumChar < 1) or (not self.Redraw) then exit;
  glBindBuffer(GL_ARRAY_BUFFER, vboVtx);
  glBufferSubData(GL_ARRAY_BUFFER,0,self.NumChar * sizeof(TQuad),@self.QuadVerts[0]);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  self.Redraw := false;
end; //UpdateVbo()
{$ELSE} //not CoreGL
procedure TGPUFont.UpdateVbo;
var
  z,i: integer;
  q: TQuad;
begin
  if (self.NumChar < 1) or (not self.Redraw) then exit;
  if displayLst <> 0 then
     glDeleteLists(displayLst, 1);
  displayLst := glGenLists(1);
  glNewList(displayLst, GL_COMPILE);
  z := -1;
  glBegin(GL_TRIANGLES);
  for i := 0 to ( self.NumChar-1) do begin
      q := self.QuadVerts[i];
      {$IFDEF NEWMSDF}
      glColor3f(Q[0].u, Q[0].v, 1.0);
      glVertex3f(Q[0].x, Q[0].y, Q[0].screenPxRange);
      glColor3f(Q[1].u, Q[1].v, 1.0);
      glVertex3f(Q[1].x, Q[1].y, Q[1].screenPxRange);
      glColor3f(Q[2].u, Q[2].v, 1.0);
      glVertex3f(Q[2].x, Q[2].y, Q[2].screenPxRange);
      glColor3f(Q[2].u, Q[2].v, 1.0);
      glVertex3f(Q[2].x, Q[2].y, Q[2].screenPxRange);
      glColor3f(Q[1].u, Q[1].v, 1.0);
      glVertex3f(Q[1].x, Q[1].y, Q[1].screenPxRange);
      glColor3f(Q[3].u, Q[3].v, 1.0);
      glVertex3f(Q[3].x, Q[3].y, Q[3].screenPxRange);
      {$ELSE}
      glColor3f(Q[0].u, Q[0].v, 1.0);
      glVertex3f(Q[0].x, Q[0].y, z);
      glColor3f(Q[1].u, Q[1].v, 1.0);
      glVertex3f(Q[1].x, Q[1].y, z);
      glColor3f(Q[2].u, Q[2].v, 1.0);
      glVertex3f(Q[2].x, Q[2].y, z);
      glColor3f(Q[2].u, Q[2].v, 1.0);
      glVertex3f(Q[2].x, Q[2].y, z);
      glColor3f(Q[1].u, Q[1].v, 1.0);
      glVertex3f(Q[1].x, Q[1].y, z);
      glColor3f(Q[3].u, Q[3].v, 1.0);
      glVertex3f(Q[3].x, Q[3].y, z);
      {$ENDIF}
  end;
  glEnd();
  glEndList();
  self.Redraw := false;
end; //UpdateVbo()
{$ENDIF}

procedure printf(s: string);
begin
{$IFDEF Darwin}
writeln(s);
{$ELSE}
showmessage(s);
{$ENDIF}
end;


function TGPUFont.LoadTex(fnm: string): boolean;
var
  px: TPicture;
  is32bit: boolean;
begin
  result := false;
  if not (LoadPng(fnm, px, is32bit)) then exit;
  glGenTextures(1, @tex);
  glBindTexture(GL_TEXTURE_2D,  tex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
  glTexImage2D(GL_TEXTURE_2D, 0,GL_RGBA8, px.Width, px.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, PInteger(px.Bitmap.RawImage.Data));
  px.Free;
  result := true;
end; //LoadTex()

procedure TGPUFont.DrawText();
begin
  if self.NumChar < 1 then exit; //nothing to draw
  {$IFDEF COREGL}
  glControl.SetViewport();  //TODO: is this required?
  {$ENDIF}
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glUseProgram(shaderProgram);
  UpdateVbo;
  glUniform4f(uniform_clr, self.FontColor.r, self.FontColor.g, self.FontColor.b, 1.0);
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, tex);
  glUniform1i(uniform_tex, 1);
  glUniform2f(uniform_viewportSize, glControl.ClientWidth, glControl.ClientHeight);
  {$IFDEF COREGL}
  glBindVertexArray(vao);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,vboIdx);
  glDrawElements(GL_TRIANGLES,  self.NumChar * 2* 3, GL_UNSIGNED_SHORT, nil); //each quad 2 triangles each with 3 indices
  glBindVertexArray(0);
  {$ELSE}
  glCallList(displayLst);
  {$ENDIF}
  glUseProgram(0);
end; //DrawText()

end.

