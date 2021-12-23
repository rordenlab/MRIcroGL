unit glcube; //OpenGL and Metal differ in only in first 2 lines
//{$DEFINE METALAPI}


{$mode objfpc}{$H+}
{$IFDEF METALAPI}
{$modeswitch objectivec1}
{$ENDIF}
{$include glopts.inc}

interface

uses
  {$IFDEF METALAPI}
  MetalPipeline, MetalUtils, MetalControl, Metal,
  {$ELSE}
  {$IFDEF LCLCocoa}retinahelper,{$ENDIF}
  {$IFDEF COREGL}glcorearb,{$ELSE} glext, gl, {$ENDIF} gl_core_utils, OpenGLContext,
  {$ENDIF}
  SimdUtils, VectorMath,
  Classes, SysUtils, Graphics,  math, dialogs;

type
  TGPUCube = class
  private
    {$IFDEF METALAPI}
    vertexBuffer: MTLBufferProtocol;
    shaderPipeline: TMetalPipeline;
    mtlControl: TMetalControl;
    {$ELSE}
    uniform_mtx: GLint;
    {$IFDEF COREGL}
    vbo_point, vao_point2d,
    {$ELSE}
    displayLst,
    {$ENDIF}
    shaderProgram: GLuint;
    {$ENDIF}
    fAzimuth, fElevation, fPitch, SizeFrac : Single;
    scrnW, scrnH, nVtx: integer;
    isText, isRedraw, isTopLeft: boolean;
    procedure SetIsTopLeft(f: boolean);
    procedure SetIsText(f: boolean);
    procedure SetSize(f: single);
    procedure SetAzimuth(f: single);
    procedure SetElevation(f: single);
    procedure  ScreenSize(Width,Height: integer);
    procedure CreateCube(sz: single);
    {$IFDEF METALAPI}
    procedure SetPipeline;
    {$ENDIF}
  public
    property Text : boolean read isText write SetIsText;
    property TopLeft : boolean read isTopLeft write SetIsTopLeft;
    property Azimuth : single read fAzimuth write SetAzimuth;
    property Elevation : single read fElevation write fElevation;
    property Pitch: single read fPitch write fPitch;
    property Size : single read SizeFrac write SetSize;
    procedure Draw(Width,Height: integer); //must be called while TOpenGLControl is current context
    {$IFDEF METALAPI}
    constructor Create(Ctx: TMetalControl);
    {$ELSE}
    constructor Create(Ctx: TOpenGLControl);
    {$ENDIF}
  end;

implementation

type
{$IFDEF METALAPI}
TVertUniforms = record //Uniforms for vertex shader
  modelViewProjectionMatrix: TMat4;
end;
TVtxClr = record
  vtx: TVec3;
  padding: single; // align each vertex attribute on 16 byte boundries
  clr: TVec4;
end;
TRGBAx = TVec4;
{$ELSE}
TVtxClr = Packed Record
  vtx   : TVec3; //vertex coordinates
  clr : TRGBA;
end;
TRGBAx = TRGBA;
{$ENDIF}

TVtxClrRA = array of TVtxClr;

{$IFDEF METALAPI}
procedure TGPUCube.SetPipeline();
var
 options: TMetalPipelineOptions;
 shaderName: string;
begin
     if (shaderPipeline <> nil) then exit; //already set
     options := TMetalPipelineOptions.Default;
     shaderName := ResourceFolderPath + pathdelim + 'cube.metal';
     if not fileexists(shaderName) then
        shaderName := ShaderDir + pathdelim +  '_Cube.metal';
     options.libraryName := shaderName;
     if not fileexists(shaderName) then begin
       writeln('Unable to find ' + shaderName);
     end;
     options.pipelineDescriptor := MTLCreatePipelineDescriptor;
     options.pipelineDescriptor.colorAttachmentAtIndex(0).setBlendingEnabled(true);
     options.pipelineDescriptor.colorAttachmentAtIndex(0).setRgbBlendOperation(MTLBlendOperationAdd);
     options.pipelineDescriptor.colorAttachmentAtIndex(0).setAlphaBlendOperation(MTLBlendOperationAdd);
     options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceRGBBlendFactor(MTLBlendFactorSourceAlpha);
     options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceAlphaBlendFactor(MTLBlendFactorSourceAlpha);
     options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationRGBBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
     options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationAlphaBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
     shaderPipeline := MTLCreatePipeline(options);
     MTLSetDepthStencil(shaderPipeline, MTLCompareFunctionLess, true);
end;
{$ELSE}
 {$IFDEF COREGL}
const
kVert2D ='#version 330'
  +#10'layout(location = 0) in vec3 Vert;'
  +#10'layout(location = 3) in vec4 Clr;'
  +#10'out vec4 vClr;'
  +#10'uniform mat4 ModelViewProjectionMatrix;'
  +#10'void main() {'
  +#10'    gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);'
  +#10'    vClr = Clr;'
  +#10'}';
kFrag2D = '#version 330'
  +#10'in vec4 vClr;'
  +#10'out vec4 color;'
  +#10'void main() {'
  +#10'    color = vClr;'
  +#10'}';
 {$ELSE}
const
 //Simple Vertex Shader
    kVert2D = '#version 120'
+#10'varying vec4 vClr;'
+#10'uniform mat4 ModelViewProjectionMatrix;'
+#10'void main() {'
+#10'    gl_Position = ModelViewProjectionMatrix * vec4(gl_Vertex.xyz, 1.0);'
+#10'    vClr = gl_Color;'
+#10'}';

//Simple Fragment Shader
kFrag2D = '#version 120'
+#10'varying vec4 vClr;'
+#10'void main() {'
+#10'    vec4 fClr = vec4(vClr.r, vClr.g, vClr.b, 1.0);'
+#10'    gl_FragColor = fClr;'
+#10'}';
 {$ENDIF}
{$ENDIF}

{$DEFINE CUBETEXT}
function setMat (a,b,c,d, e,f,g,h, i,j,k,l: single): TMat4;
begin
     result.m[0,0] := a;
     result.m[0,1] := b;
     result.m[0,2] := c;
     result.m[0,3] := d;

     result.m[1,0] := e;
     result.m[1,1] := f;
     result.m[1,2] := g;
     result.m[1,3] := h;

     result.m[2,0] := i;
     result.m[2,1] := j;
     result.m[2,2] := k;
     result.m[2,3] := l;

     result.m[3,0] := 0;
     result.m[3,1] := 0;
     result.m[3,2] := 0;
     result.m[3,3] := 1;
end;

{$IFDEF METALAPI}
function SetRGBA(r,g,b,a: byte): TRGBAx; overload;
begin
     result.r := r/255;
     result.g := g/255;
     result.b := b/255;
     result.a := a/255;

end;
{$ENDIF}

procedure MakeCube(sz: single; var vtxClrs: TVtxClrRA); //draw a cube of size sz
const
{$IFDEF METALAPI}
 kTiny = 0.005;
{$ELSE}
 kTiny = 0.00;
{$ENDIF}
var
  nface: integer;
  clr : TRGBAx;
{$IFDEF CUBETEXT}
  rot : TMat4;
{$ENDIF}
procedure vertex3f(x,y,z: single; rep: boolean = false);
begin
 vtxClrs[nface].vtx.X := x;
 vtxClrs[nface].vtx.Y := y;
 vtxClrs[nface].vtx.Z := z;
 vtxClrs[nface].clr := clr;
 nface := nface + 1;
 if not rep then exit;
 vtxClrs[nface] := vtxClrs[nface-1];
 nface := nface + 1;
end;
{$IFDEF CUBETEXT}
procedure vertex2f(x,y: single; rep: boolean = false);
var
  v: TVec4;
begin
     v.x := x;
     v.y := y;
     v.z := 0;
     v.w := 1;
     v *= rot;
     vertex3f(v.x, v.y, v.z, rep);
end;

procedure drawL();
begin
  setlength(vtxClrs, length(vtxClrs)+ 12);
  vertex2f(0.275, 0.1, true);
  vertex2f(0.275, 0.9);
  vertex2f(0.375, 0.1);
  vertex2f(0.375, 0.9, true);

  vertex2f(0.375,0.1, true);
  vertex2f(0.375,0.2);
  vertex2f(0.725,0.1);
  vertex2f(0.725,0.2, true);
end;

procedure drawR();
begin
  setlength(vtxClrs, length(vtxClrs)+ 30);
  vertex2f(0.275, 0.1, true);
  vertex2f(0.275, 0.9);
  vertex2f(0.375, 0.1);
  vertex2f(0.375, 0.9, true);

  vertex2f(0.375, 0.8, true);
  vertex2f(0.375, 0.9);
  vertex2f(0.725, 0.8);
  vertex2f(0.625, 0.9, true);

  vertex2f(0.625, 0.55, true);
  vertex2f(0.625, 0.8);
  vertex2f(0.725, 0.55);
  vertex2f(0.725, 0.8, true);

  vertex2f(0.375, 0.45, true);
  vertex2f(0.375, 0.55);
  vertex2f(0.625, 0.45);
  vertex2f(0.725, 0.55, true);

  vertex2f(0.625, 0.1, true);
  vertex2f(0.525, 0.45);
  vertex2f(0.725, 0.1);
  vertex2f(0.625, 0.45, true);

end;

procedure drawP();
begin
  setlength(vtxClrs, length(vtxClrs)+ 24);
  vertex2f(0.275, 0.1, true);
  vertex2f(0.275, 0.9);
  vertex2f(0.375, 0.1);
  vertex2f(0.375, 0.9, true);

  vertex2f(0.375, 0.8, true);
  vertex2f(0.375, 0.9);
  vertex2f(0.725, 0.8);
  vertex2f(0.625, 0.9, true);

  vertex2f(0.625, 0.55, true);
  vertex2f(0.625, 0.8);
  vertex2f(0.725, 0.55);
  vertex2f(0.725, 0.8, true);

  vertex2f(0.375, 0.45, true);
  vertex2f(0.375, 0.55);
  vertex2f(0.625, 0.45);
  vertex2f(0.725, 0.55, true);

end;

procedure drawS();
begin
  setlength(vtxClrs, length(vtxClrs)+ 42);
  vertex2f(0.275, 0.2, true);
  vertex2f(0.275, 0.3);
  vertex2f(0.375, 0.1);
  vertex2f(0.375, 0.3, true);

  vertex2f(0.375, 0.1, true);
  vertex2f(0.375, 0.2);
  vertex2f(0.625, 0.1);
  vertex2f(0.725, 0.2, true);

  vertex2f(0.625, 0.1, true);
  vertex2f(0.625, 0.55);
  vertex2f(0.725, 0.2);
  vertex2f(0.725, 0.45, true);

  vertex2f(0.375, 0.45, true);
  vertex2f(0.275, 0.55);
  vertex2f(0.625, 0.45);
  vertex2f(0.625, 0.55, true);

  vertex2f(0.275, 0.55, true);
  vertex2f(0.275, 0.8);
  vertex2f(0.375, 0.55);
  vertex2f(0.375, 0.9, true);

  vertex2f(0.375, 0.8, true);
  vertex2f(0.375, 0.9);
  vertex2f(0.725, 0.8);
  vertex2f(0.625, 0.9, true);

  vertex2f(0.625, 0.7, true);
  vertex2f(0.625, 0.8);
  vertex2f(0.725, 0.7);
  vertex2f(0.725, 0.8, true);
end;

procedure drawA();
begin
 setlength(vtxClrs, length(vtxClrs)+ 18);
 vertex2f(0.275,0.1, true);
 vertex2f(0.475,0.9);
 vertex2f(0.375,0.1);
 vertex2f(0.575,0.9, true);

 vertex2f(0.625,0.1, true);
 vertex2f(0.475,0.9);
 vertex2f(0.725,0.1);
 vertex2f(0.575,0.9, true);

 vertex2f(0.4375,0.35, true);
 vertex2f(0.4625,0.45);
 vertex2f(0.6625,0.35);
 vertex2f(0.6375,0.45, true);
end;

procedure drawI();
begin
  setlength(vtxClrs, length(vtxClrs)+ 6);
  vertex2f(0.45,0.1, true);
  vertex2f(0.45,0.9);
  vertex2f(0.55,0.1);
  vertex2f(0.55,0.9, true);
end;
{$ENDIF}
begin
  setlength(vtxClrs, 36);
  nface := 0;
  //bottom - dark
  clr := setRGBA(72, 72, 72, 255);
  vertex3f(-sz, -sz, -sz, true);
  vertex3f(-sz, sz, -sz);
  vertex3f(sz, -sz, -sz);
  vertex3f(sz, sz, -sz, true);
  //top - bright
  clr := setRGBA(204,204,204,255);
  vertex3f(-sz, -sz, sz, true);
  vertex3f(sz, -sz, sz);
  vertex3f(-sz, sz, sz);
  vertex3f(sz, sz, sz, true);
  //front - blue
  clr := setRGBA(0,0,188,255);
  vertex3f(-sz, sz, -sz, true);
  vertex3f(-sz, sz, sz);
  vertex3f(sz, sz, -sz);
  vertex3f(sz, sz, sz, true);
  //back -purple
  clr := setRGBA(108,0,108,255);
  vertex3f(-sz, -sz, -sz, true);
  vertex3f(sz, -sz, -sz);
  vertex3f(-sz, -sz, sz);
  vertex3f(sz, -sz, sz, true);
  //left - red
  clr := setRGBA(164,0,0,255);
  vertex3f(-sz, -sz, -sz, true);
  vertex3f(-sz, -sz, sz);
  vertex3f(-sz, sz, -sz);
  vertex3f(-sz, sz, sz, true);
  //right - green
  clr := setRGBA(0,128,0,255);
  vertex3f(sz, -sz, -sz, true);
  vertex3f(sz, sz, -sz);
  vertex3f(sz, -sz, sz);
  vertex3f(sz, sz, sz, true);
  {$IFDEF CUBETEXT}
  rot := TMat4.Identity;
  clr := setRGBA(0,0,0,255);
  rot := setMat(sz*2,0,0,-sz, 0,0,0,sz+kTiny, 0,sz*2,0,-sz);
  drawP();
  rot := setMat(-sz*2,0,0,sz, 0,0,0,-sz-kTiny, 0,sz*2,0,-sz);
  drawA();
  rot := setMat(sz*2,0,0,-sz, 0,-sz*2,0,sz, 0,0,0,sz+kTiny);
  drawS();
  rot := setMat(sz*2,0,0,-sz, 0,sz*2,0,-sz, 0,0,0,-sz-kTiny);
  drawI();
  rot := setMat(0,0,0,-sz-kTiny, sz*2,0,0,-sz, 0,sz*2,0,-sz);
  drawL();
  rot := setMat(0,0,0,sz+kTiny, -sz*2,0,0,sz, 0,sz*2,0,-sz);
  drawR();
  {$ENDIF}
end; //MakeCube()

procedure  TGPUCube.CreateCube(sz: single);
{$IFNDEF METALAPI}  {$IFDEF COREGL}
const
    kATTRIB_VERT = 0;  //vertex XYZ are positions 0,1,2
    kATTRIB_CLR = 3;   //color RGBA are positions 3,4,5,6
{$ENDIF}  {$ENDIF}
var
  nface: integer;
  vtxClrs: TVtxClrRA;
  {$IFNDEF METALAPI} {$IFNDEF COREGL}
  i: integer;
  {$ENDIF} {$ENDIF}
begin
  if not isRedraw then exit;
  //writeln('redraw'); //only when scale changed
  isRedraw := false;
  vtxClrs := nil;
  MakeCube(sz, vtxClrs);
  nface := Length(vtxClrs); //each face has 3 vertices
  if nface < 1 then exit;
  {$IFDEF METALAPI}
   vertexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@vtxClrs[0], nface*sizeof(TVtxClr), MTLResourceStorageModeShared);
  {$ELSE}
  {$IFDEF COREGL}
  if vao_point2d <> 0 then
     glDeleteVertexArrays(1,@vao_point2d);
  glGenVertexArrays(1, @vao_point2d);
  vbo_point := 0;
  glGenBuffers(1, @vbo_point);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  glBufferData(GL_ARRAY_BUFFER, nface*SizeOf(TVtxClr), @vtxClrs[0], GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  // Prepare vertrex array object (VAO)
  glBindVertexArray(vao_point2d);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  //Vertices
  glVertexAttribPointer(kATTRIB_VERT, 3, GL_FLOAT, GL_FALSE, sizeof(TVtxClr), PChar(0));
  glEnableVertexAttribArray(kATTRIB_VERT);
  //Color
  glVertexAttribPointer(kATTRIB_CLR, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof(TVtxClr), PChar( sizeof(TVec3)));
  glEnableVertexAttribArray(kATTRIB_CLR);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
  {$ELSE}
  if not displayLst <> 0 then
     glDeleteLists(displayLst, 1);
  displayLst := glGenLists(1);
  glNewList(displayLst, GL_COMPILE);
  glBegin(GL_TRIANGLE_STRIP);
  for i := 0 to nface-1 do begin
      glColor4ub(vtxClrs[i].clr.R, vtxClrs[i].clr.G, vtxClrs[i].clr.B, vtxClrs[i].clr.A);
      glVertex3f(vtxClrs[i].vtx.x, vtxClrs[i].vtx.y, vtxClrs[i].vtx.z);
  end;
  glEnd();
  glEndList();
  {$ENDIF} //if CoreGL else legacy OpenGL
  {$ENDIF} //if Metal else OpenGL
  nVtx := length(vtxClrs);
  setlength(vtxClrs,0);
end;

procedure TGPUCube.SetAzimuth(f: single);
begin
  fAzimuth := f;
end;

procedure TGPUCube.SetElevation(f: single);
begin
  fElevation := f;
end;

procedure TGPUCube.SetIsText(f: boolean);
begin
     if (f <> isText) then isRedraw := true;
     isText := f;
end;

procedure TGPUCube.SetIsTopLeft(f: boolean);
begin
     if (f <> isTopLeft) then isRedraw := true;
     isTopLeft := f;
end;

procedure  TGPUCube.SetSize(f: single);
begin
     if (f <> sizeFrac) then isRedraw := true;
     sizeFrac := f;
     if sizeFrac < 0.005 then sizeFrac := 0.005;
     if sizeFrac > 0.25 then sizeFrac := 0.25;
end;

procedure  TGPUCube.ScreenSize(Width,Height: integer);
begin
     if (Width = scrnW) and (Height = scrnH) then exit;
     scrnW := Width;
     scrnH := Height;
     isRedraw := true;
end;

{$IFDEF METALAPI}
constructor  TGPUCube.Create(Ctx: TMetalControl);
{$ELSE}
constructor  TGPUCube.Create(Ctx: TOpenGLControl);
{$ENDIF}
begin
     scrnH := 0;
     SizeFrac := 0.02;
     isRedraw := true;
     fAzimuth := 30;
     fElevation := -15;
     fPitch := 0;
     isTopLeft := false;
     isText := true;
     {$IFDEF METALAPI}
     mtlControl := Ctx;
     vertexBuffer := nil;
     shaderPipeline := nil;
     {$ELSE}
     {$IFDEF COREGL}
     vao_point2d := 0;
     vbo_point := 0;
     {$ELSE}
     displayLst := 0;
     {$ENDIF}
     Ctx.MakeCurrent();
     shaderProgram :=  initVertFrag(kVert2D, kFrag2D);
     uniform_mtx := glGetUniformLocation(shaderProgram, pAnsiChar('ModelViewProjectionMatrix'));
     glFinish;
     Ctx.ReleaseContext;
     {$ENDIF}
end;

procedure  TGPUCube.Draw(Width,Height: integer);
var
  sz: single;
  {$IFDEF METALAPI}
  vertUniforms: TVertUniforms;
  {$ELSE}
  modelViewProjectionMatrix,
  {$ENDIF}
  projectionMatrix, modelMatrix: TMat4;
begin
  ScreenSize(Width,Height);
  sz := min(ScrnW,ScrnH) * SizeFrac;
  if sz < 5 then exit;
  {$IFDEF METALAPI}
  setPipeline;
  MTLSetShader(shaderPipeline);
  {$ENDIF}
  CreateCube(sz);
  modelMatrix := TMat4.Identity;
  {$IFDEF METALAPI}
  modelMatrix.m[2,2] := -1;
  projectionMatrix := TMat4.Ortho (0, ScrnW,0, ScrnH,-10*sz,10*sz);
  {$ELSE}
  projectionMatrix := TMat4.OrthoGL (0, ScrnW,0, ScrnH,-10*sz,10*sz);
  {$ENDIF}
  projectionMatrix *= TMat4.Translate(0,0,sz*8);
  projectionMatrix *= TMat4.Translate(1.8*sz,1.8*sz,0);
  {$IFDEF METALAPI}
  projectionMatrix *= TMat4.RotateX(DegToRad(90-fElevation));
  {$ELSE}
  projectionMatrix *= TMat4.RotateX(-DegToRad(90-fElevation));
  {$ENDIF}
  projectionMatrix *= TMat4.RotateZ(-DegToRad(fAzimuth));
  projectionMatrix *= TMat4.RotateX(-DegToRad(fPitch));


  {$IFDEF METALAPI}
  vertUniforms.modelViewProjectionMatrix := ( projectionMatrix * modelMatrix);
  MTLSetVertexBuffer(vertexBuffer, 0, 0);
  MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
  if isText then
     MTLDraw(MTLPrimitiveTypeTriangleStrip, 0, nVtx)
  else
      MTLDraw(MTLPrimitiveTypeTriangleStrip, 0, 36);
  {$ELSE}
  modelViewProjectionMatrix := ( projectionMatrix * modelMatrix);
  glEnable(GL_CULL_FACE);
  glDisable(GL_DEPTH_TEST);
  glUseProgram(shaderProgram);
  glUniformMatrix4fv(uniform_mtx, 1, GL_FALSE, @modelViewProjectionMatrix);
  {$IFDEF COREGL}
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  glBindVertexArray(vao_point2d);
  if isText then
     glDrawArrays(GL_TRIANGLE_STRIP, 0, nVtx)
  else
      glDrawArrays(GL_TRIANGLE_STRIP, 0, 36);
  glBindVertexArray(0);
  {$ELSE}
  glCallList(displayLst);
  {$ENDIF}
  glDisable(GL_CULL_FACE);
  glUseProgram(0);
  {$ENDIF}
end;

end.

