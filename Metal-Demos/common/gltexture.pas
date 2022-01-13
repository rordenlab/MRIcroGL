unit gltexture;
//OpenGL Texture - draw a bitmap
{$mode objfpc}{$H+}
interface
{$include glopts.inc}//<- defines CORE OpenGL >=3.3, else uses LEGACY OpenGL 2.1
uses
  {$IFDEF LCLCocoa}retinahelper,{$ENDIF}
  {$IFDEF COREGL}glcorearb, {$ELSE} gl, glext,{$ENDIF}
  SimdUtils,
  gl_core_utils,Classes, SysUtils, Graphics, OpenGLContext, dialogs;

type
Txyuv = Packed Record
  x,y   : single; //vertex coordinates
  u,v : single; //texture coordinates
end;
  TGPUTexture = class
  private
         {$IFDEF COREGL}
         vbo, vao,
         {$ELSE}
          displayLst,
         {$ENDIF}
           tex, shaderProgram: GLuint;
         uniform_viewportSize, uniform_tex: GLint;
         bmpHt, bmpWid: integer;
         glControl: TOpenGLControl;
         OffsetX,OffsetY,Zoom: single;
         isVboRequiresUpdate: boolean;
    procedure VboUpdate();
    procedure LoadTex(fnm : string);
  public
    property BitmapHeight: integer read bmpHt;
    property BitmapWidth: integer read bmpWid;
    procedure DrawTex(); //must be called while TOpenGLControl is current context
    procedure SetPosition(xPixel, yPixel, zoomRatio: single);
    constructor Create(fnm : string; fromView: TOpenGLControl); //overlod;
    Destructor  Destroy; override;
  end;

implementation

const
{$IFDEF COREGL}
//vertex shader
    kVert = '#version 330'
+#10'layout(location = 0) in vec2 point;'
+#10'layout(location = 1) in vec2 uvX;'
+#10'uniform vec2 ViewportSize;'
+#10'out vec2 uv;'
+#10'void main() {'
+#10'    uv = uvX;'
+#10'    vec2 ptx = point;'
+#10'    gl_Position = vec4((ptx / (ViewportSize/2)), 0.0, 1.0);'
+#10'    //gl_Position = ModelViewProjectionMatrix * vec4(ptx, -0.5, 1.0);'
+#10'}';
//Simple Fragment Shader
    kFrag = '#version 330'
+#10'in vec2 uv;'
+#10'out vec4 color;'
+#10'uniform sampler2D tex;'
+#10'void main() {'
+#10'    color = texture(tex,uv);'
+#10'}';
{$ELSE} //if core opengl, else legacy shaders
kVert = '#version 120'
+#10'uniform vec2 ViewportSize;'
+#10'varying vec4 uv;'
+#10'void main() {'
+#10'    gl_Position = vec4((gl_Vertex.xy / (ViewportSize/2)), 0.0, 1.0);'
+#10'    uv = gl_Color;'
+#10'}';

const kFrag = '#version 120'
+#10'varying vec4 uv;'
+#10'uniform sampler2D tex;'
+#10'void main() {'
+#10'  gl_FragColor = texture2D(tex, uv.xy);'
+#10'}';
{$ENDIF}

function MakeXYUV(x,y,u,v: single):Txyuv;
begin
     result.x := x;
     result.y := y;
     result.u := u;
     result.v := v;
end;

procedure TGPUTexture.VboUpdate();
var
    Sq : packed array [0..3] of Txyuv;
    ZoomX,ZoomY: single;
    {$IFNDEF COREGL}
     i: integer;
    {$ENDIF}
begin
  if not isVboRequiresUpdate then exit;
  ZoomX := bmpWid * Zoom * 0.5;
  ZoomY := bmpHt * Zoom * 0.5;
  Sq[0] := MakeXYUV(OffsetX - ZoomX, OffsetY + ZoomY, 0, 0);
  Sq[1] := MakeXYUV(OffsetX - ZoomX, OffsetY -ZoomY, 0, 1);
  Sq[2] := MakeXYUV(OffsetX + ZoomX, OffsetY + ZoomY, 1, 0);
  Sq[3] := MakeXYUV(OffsetX + ZoomX, OffsetY -ZoomY, 1, 1);
  {$IFDEF COREGL}
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBufferSubData(GL_ARRAY_BUFFER,0,sizeof(Sq),@Sq[0]);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  {$ELSE}
  if not displayLst <> 0 then
     glDeleteLists(displayLst, 1);
  displayLst := glGenLists(1);
  glNewList(displayLst, GL_COMPILE);
  glBegin(GL_TRIANGLE_STRIP);
  for i := 0 to 3 do begin
      glColor3f(Sq[i].u, Sq[i].v, 1.0);
      glVertex2f(Sq[i].x, Sq[i].y);
      //
      //  x,y   : single; //vertex coordinates
      //u,v : single; //texture coordinates
  end;
  glEnd();
  glEndList();
  {$ENDIF}
  isVboRequiresUpdate:= false;
end;

procedure TGPUTexture.SetPosition(xPixel, yPixel, zoomRatio: single);
begin
  OffsetX := xPixel;
  OffsetY := yPixel;
  Zoom := zoomRatio;
  isVboRequiresUpdate:= true;
end;

procedure printf(s: string);
begin
{$IFDEF Darwin}
writeln(s);
{$ELSE}
showmessage(s);
{$ENDIF}
end;

constructor TGPUTexture.Create(fnm: string; fromView: TOpenGLControl);
const
    kATTRIB_POINT = 0; //XY position on screen
    kATTRIB_UV = 1; //UV coordinates of texture
begin
  glControl := fromView;
  OffsetX := 0;
  OffsetY := 0;
  Zoom := 1;
  tex := 0;
  shaderProgram := 0;
  uniform_tex := 0;
  isVboRequiresUpdate := true;
  if not fileexists(fnm) then exit;
  glControl.MakeCurrent();
  //setup VAO for lines
  {$IFDEF COREGL}
  vbo := 0;
  vao := 0;
  glGenBuffers(1, @vbo);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBufferData(GL_ARRAY_BUFFER, 4 * sizeof(Txyuv), nil, GL_DYNAMIC_DRAW); //GL_STATIC_DRAW
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glGenVertexArrays(1, @vao);
  glBindVertexArray(vao);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glVertexAttribPointer(kATTRIB_POINT, 2, GL_FLOAT, GL_FALSE, sizeof(Txyuv), PChar(0));
  glEnableVertexAttribArray(kATTRIB_POINT);
  glVertexAttribPointer(kATTRIB_UV, 2, GL_FLOAT, GL_FALSE, sizeof(Txyuv), PChar(sizeof(single)*2));
  glEnableVertexAttribArray(kATTRIB_UV);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
  {$ELSE}
  displayLst := 0;
  {$ENDIF}
  //done
  shaderProgram :=  initVertFrag(kVert,  kFrag);
  uniform_viewportSize := glGetUniformLocation(shaderProgram, pAnsiChar('ViewportSize'));
  uniform_tex := glGetUniformLocation(shaderProgram, pAnsiChar('tex'));
  LoadTex(fnm);
  SetPosition(0.0,0.0, 1);
  glFinish;
  glControl.ReleaseContext;
  if GLErrorStr <> '' then printf(GLErrorStr);
end;

procedure TGPUTexture.LoadTex(fnm: string);
var
  px: TPicture;
  is32bit: boolean = true;
begin
  if not (LoadPng(fnm, px, is32bit)) then exit;
  bmpHt := px.Bitmap.Height;
  bmpWid := px.Bitmap.Width;

  glGenTextures(1, @tex);
  glBindTexture(GL_TEXTURE_2D,  tex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
  //For both Darwin and Windows we seem to want BGRA - TODO: check Linux
  //{$IFDEF Darwin}
  if (is32bit) then
  	glTexImage2D(GL_TEXTURE_2D, 0,GL_RGBA8, px.Width, px.Height, 0, GL_BGRA, GL_UNSIGNED_BYTE, PInteger(px.Bitmap.RawImage.Data))
  else
      //glTexImage2D(GL_TEXTURE_2D, 0,GL_RGBA8, px.Width, px.Height, 0, GL_BGRA, GL_UNSIGNED_BYTE, PInteger(px.Bitmap.RawImage.Data))
      glTexImage2D(GL_TEXTURE_2D, 0,GL_RGBA8, px.Width, px.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, PInteger(px.Bitmap.RawImage.Data));
  px.Free;
end;

procedure TGPUTexture.DrawTex();
begin
  VboUpdate();
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glUseProgram(shaderProgram);
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, tex);
  glUniform1i(uniform_tex, 1);
  glUniform2f(uniform_viewportSize, glControl.ClientWidth, glControl.ClientHeight);
  {$IFDEF COREGL}
  glBindVertexArray(vao);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
  glBindVertexArray(0);
  {$ELSE}
  glCallList(displayLst);
  {$ENDIF}
  glUseProgram(0);
end;

destructor TGPUTexture.Destroy;
begin
  //call the parent destructor:
  inherited;
end;

end.


