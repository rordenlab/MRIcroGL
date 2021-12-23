unit glmesh;
interface
{$DEFINE MATCAP}
{$IFNDEF METALAPI}
 {$include glopts.inc}
{$ENDIF}
uses
  {$IFDEF LCLCocoa}retinahelper,{$ENDIF}
  {$IFDEF COREGL} glcorearb, {$ELSE} gl, glext, {$ENDIF}
  {$IFDEF MATCAP} Graphics, GraphType, FPImage, IntfGraphics, LCLType,{$ENDIF}
  SimdUtils, classes, dialogs,OpenGLContext,mesh, VectorMath, gl_core_utils,SysUtils, Math;
const
  kDefaultDistance = 1.0;
  kMaxDistance = 2;
type
  TGPUMesh = class
      private
        fMeshColor: TRGBA;
        fAzimuth,fElevation: integer;
        fDistance: single;
        fLightPos: TVec4;
        {$IFDEF COREGL} vertex_vao, {$ELSE} vertex_vbo, {$ENDIF}
        index_vbo , shaderProgram: GLuint;
        {$IFDEF MATCAP}
        matCapFnm: string;
        matCapTexture: GLuint;
        {$ENDIF}
        nface: integer;
        //{$IFNDEF COREGL} vertLoc, normLoc, clrLoc: GLint; {$ENDIF}
        uniform_lightPos, uniform_ModelViewProjectionMatrix, uniform_ModelViewMatrix, uniform_NormalMatrix: GLint;
        fPerspective: boolean;
        glControl: TOpenGLControl;
        fMeshName: string;
      private
        procedure Prepare();
      public
        {$IFDEF MATCAP}
        uniform_MatCap: GLint; // >=0 if shader supports matcaps
        function SetMatCap(fnm: string): boolean;
        function MatCapPath(): string;
        {$ENDIF}
        property MeshName: string read fMeshName;
        function ShaderPath(): string;
        property Perspective: boolean read fPerspective write fPerspective;
        property MeshColor: TRGBA read fMeshColor write fMeshColor;
        property Azimuth: integer read fAzimuth write fAzimuth;
        property Elevation: integer read fElevation write fElevation;
        property Distance: single read fDistance write fDistance;
        property LightPosition: TVec4 read fLightPos write fLightPos;
        constructor Create(fromView: TOpenGLControl); overload;
        constructor Create(fromView: TOpenGLControl; InitMeshName: string); overload;
        procedure Paint();
        procedure OpenMesh(Filename: string; isSwapYZ: boolean = true);
        procedure SetShader(shaderName: string);
        {$IFNDEF COREGL}procedure SetVertexAttrib();{$ENDIF}
  end;

implementation

//uses meshForm;


const
  {$IFDEF COREGL}
kVertStr = '#version 330'
+#10'layout(location = 0) in vec3 Vert;'
+#10'layout(location = 3) in vec3 Norm;'
+#10'layout(location = 6) in vec4 Clr;'
+#10'out vec3 vN, vV;'
+#10'out vec4 vClr;'
+#10'uniform mat4 ModelViewProjectionMatrix;'
+#10'uniform mat4 ModelViewMatrix;'
+#10'uniform mat4 NormalMatrix;'
+#10'void main() {'
+#10'    vN = normalize((NormalMatrix * vec4(Norm,1.0)).xyz);'
+#10'    gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);'
+#10'    vV = -vec3(ModelViewMatrix*vec4(Vert,1.0));'
+#10'    vClr = Clr;'
+#10'}';

kFragStr = '#version 330'
+#10'in vec4 vClr;'
+#10'in vec3 vN, vV;'
+#10'out vec4 color;'
+#10'void main() {'
+#10' vec3 n = normalize(vN);'
+#10' vec3 vL = normalize(vec3(0.0, 20.0, 30.0));'
+#10' vec3 h = normalize(vL + normalize(vV));'
+#10' float diffuse = dot(vL, n);'
+#10' float specular =  pow(max(0.0,dot(n,h)),1.0/0.01);'
+#10' color = vec4(vClr.rgb*0.4 + vClr.rgb*diffuse*0.7 +vec3(1.0, 1.0, 1.0)*specular*0.6, 1.0);'
+#10'}';
{$ELSE}
kVertStr = '#version 120'
+#10'attribute vec3 Vert;'
+#10'attribute vec3 Norm;'
+#10'attribute vec4 Clr;'
+#10'varying vec3 vN, vV;'
+#10'varying vec4 vClr;'
+#10'uniform mat4 ModelViewProjectionMatrix;'
+#10'uniform mat4 ModelViewMatrix;'
+#10'uniform mat4 NormalMatrix;'
+#10'void main() {'
+#10'    vN = normalize((NormalMatrix * vec4(Norm.xyz,1.0)).xyz);'
+#10'    gl_Position = ModelViewProjectionMatrix * vec4(Vert.xyz, 1.0);'
+#10'    vV = -vec3(ModelViewMatrix*vec4(Vert.xyz,1.0));'
+#10'    vClr = Clr;'
+#10'}';

kFragStr = '#version 120'
+#10'varying vec3 vN, vV;'
+#10'varying vec4 vClr;'
+#10'void main() {'
+#10' vec3 n = normalize(vN);'
+#10' vec3 vL = normalize(vec3(0.0, 20.0, 30.0));'
+#10' vec3 h = normalize(vL + normalize(vV));'
+#10' float diffuse = dot(vL, n);'
+#10' float specular =  pow(max(0.0,dot(n,h)),1.0/0.01);'
+#10' gl_FragColor = vec4(vClr.rgb*0.4 + vClr.rgb*diffuse*0.7 +vec3(1.0, 1.0, 1.0)*specular* 0.6, 1.0);'
+#10'}';
{$ENDIF}

{$IFDEF COREGL}
type
  TVtxNormClr = Packed Record
    vtx   : TPoint3f; //vertex coordinates
    norm : int32;
    clr : TRGBA;
  end;
{$ELSE}
type
  TVtxNormClr = Packed Record
    vtx   : TPoint3f; //vertex coordinates
    norm : TPoint3f;
    clr : TRGBA;
  end;
{$ENDIF}

procedure printf (lS: AnsiString);
begin
{$IFNDEF WINDOWS} writeln(lS); {$ENDIF}
end;

procedure TGPUMesh.SetShader(shaderName: string);
var
  VertexProgram, FragmentProgram: string;
begin
  glGetError(); //<- ignore prior errors
  glControl.MakeCurrent();
  glUseProgram(0);
  if (shaderProgram <> 0) then glDeleteProgram(shaderProgram);
  loadVertFrag(shaderName, VertexProgram, FragmentProgram);
  if VertexProgram = '' then VertexProgram := kVertStr;
  if FragmentProgram = '' then FragmentProgram := kFragStr;
  shaderProgram :=  initVertFrag(VertexProgram,  FragmentProgram);
  {$IFDEF UNIX}
  if GLErrorStr <> '' then
     printf(GLErrorStr);
  {$ENDIF}
  {$IFNDEF COREGL}
  //vertLoc := glGetAttribLocation(shaderProgram, 'Vert');
  //normLoc := glGetAttribLocation(shaderProgram, 'Norm');
  //clrLoc := glGetAttribLocation(shaderProgram, 'Clr');
  {$ENDIF}
  uniform_ModelViewProjectionMatrix := glGetUniformLocation(shaderProgram, pAnsiChar('ModelViewProjectionMatrix'));
  uniform_ModelViewMatrix := glGetUniformLocation(shaderProgram, pAnsiChar('ModelViewMatrix'));
  uniform_NormalMatrix := glGetUniformLocation(shaderProgram, pAnsiChar('NormalMatrix'));
  uniform_lightPos := glGetUniformLocation(shaderProgram, pAnsiChar('LightPosition'));
  {$IFDEF MATCAP}
  uniform_MatCap := glGetUniformLocation(shaderProgram, pAnsiChar('MatCap'));
  {$ENDIF}
  glFinish;
  {$IFNDEF COREGL}SetVertexAttrib(); {$ENDIF}
  glControl.ReleaseContext;
  if GLErrorStr <> '' then begin
        printf(GLErrorStr);
        showmessage(GLErrorStr);
        GLErrorStr := '';
  end;
  GetError(4, 'Shader');
end;

{$IFDEF WINDOWS}
procedure FlipVertical (var px: TPicture);
var
  p: array of byte;
  i, half, b: integer;
  LoPtr, HiPtr: PInteger;
begin
    if px.Height < 3 then exit;
    half := (px.Height div 2);
    b := px.Bitmap.RawImage.Description.BytesPerLine;
    LoPtr := PInteger(px.Bitmap.RawImage.Data);
    HiPtr := PInteger(px.Bitmap.RawImage.Data+ ((px.Height -1) * b));
    setlength(p, b);
    for i := 1 to half do begin
          System.Move(LoPtr^,p[0],b); //(src, dst,sz)
          System.Move(HiPtr^,LoPtr^,b); //(src, dst,sz)
          System.Move(p[0],HiPtr^,b); //(src, dst,sz)
          Inc(PByte(LoPtr), b );
          Dec(PByte(HiPtr), b);
    end;
end; //FlipVertical()
{$ENDIF}

function TGPUMesh.SetMatCap(fnm: string): boolean;
var
  px: TPicture;
  ifnm: string;
  {$IFNDEF WINDOWS}
  AImage: TLazIntfImage;
  lRawImage: TRawImage;
  {$ENDIF}
begin
  result := false;
  if not fileexists(fnm) then begin
     ifnm := fnm;
     fnm := MatCapPath+fnm+'.jpg';
     if not fileexists(fnm) then begin
        printf(format('LoadTex: unable to find "%s" or "%s"',[ifnm, fnm]));
        exit;
     end;
  end;
  matCapFnm := fnm;
  px := TPicture.Create;
    try
       {$IFDEF WINDOWS}
       px.LoadFromFile(fnm);
       FlipVertical(px);
       {$ELSE}
       //ensure order is GL_RGBA8 - it is with many PNG files, but not JPEG
       lRawImage.Init;
       lRawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(0,0);
       lRawImage.Description.LineOrder := riloBottomToTop; // openGL uses cartesian coordinates
       lRawImage.CreateData(false);
       AImage := TLazIntfImage.Create(0,0);
       try
         AImage.SetRawImage(lRawImage);
         AImage.LoadFromFile(fnm);
         px.Bitmap.LoadFromIntfImage(AImage);
       finally
         AImage.Free;
       end;
       {$ENDIF}
    except
      px.Bitmap.Width:=-1;
    end;
  if ((px.Bitmap.PixelFormat <> pf24bit ) and  (px.Bitmap.PixelFormat <> pf32bit )) or (px.Bitmap.Width < 1) or (px.Bitmap.Height < 1) then begin
     printf(format('LoadTex: unsupported pixel format bpp (%d) or size (%dx%d)',[PIXELFORMAT_BPP[px.Bitmap.PixelFormat], px.Bitmap.Width, px.Bitmap.Height]));
     exit;
  end;
  px.Bitmap.Height;
  px.Bitmap.Width;
  glControl.MakeCurrent(false);
  glGetError(); //<- ignore prior errors
  if matCapTexture <> 0 then
     glDeleteTextures(1,@matCapTexture);
  glGenTextures(1, @matCapTexture);
  glBindTexture(GL_TEXTURE_2D,  matCapTexture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
  {$IFDEF WINDOWS}
  glTexImage2D(GL_TEXTURE_2D, 0,GL_RGBA8, px.Width, px.Height, 0, GL_BGRA, GL_UNSIGNED_BYTE, PInteger(px.Bitmap.RawImage.Data));
  {$ELSE}
  glTexImage2D(GL_TEXTURE_2D, 0,GL_RGBA8, px.Width, px.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, PInteger(px.Bitmap.RawImage.Data));
  {$ENDIF}
  GetError(4, 'Matcap:'+fnm);
  glControl.ReleaseContext;
  px.Free;
  result := true;

end;

function TGPUMesh.MatCapPath(): string;
begin
     result := ResourceDir+pathdelim+'matcap'+pathdelim;
end;


function TGPUMesh.ShaderPath(): string;
begin
     result := ResourceDir+pathdelim+'shader'+pathdelim;
end;

procedure TGPUMesh.Prepare();
begin
  SetShader('');
end;

constructor TGPUMesh.Create(fromView: TOpenGLControl; InitMeshName: string);  overload;
begin
  glControl := fromView;
  fDistance := kDefaultDistance;
  fMeshName := InitMeshName;
  fPerspective := false;
  fAzimuth := 110;
  fElevation := 30;
  fLightPos := Vec4(0,0.707, 0.707, 0.0);
  fMeshColor.r := 210;
  fMeshColor.g := 148;
  fMeshColor.b := 148;
  index_vbo := 0;
  {$IFDEF COREGL}
  vertex_vao := 0;
  {$ELSE}
  vertex_vbo := 0;
  {$ENDIF}
  shaderProgram :=0;
  {$IFDEF MATCAP}
  matCapTexture := 0;
  uniform_MatCap := -1;
  {$ENDIF}
  shaderProgram := 0;
end;

constructor TGPUMesh.Create(fromView: TOpenGLControl);  overload;
begin
  {$IFDEF SSAO}
  fMeshName := ResourceFolderPath+pathdelim+'brain.mz3';
  {$ELSE}
  fMeshName := '';//  ResourceFolderPath+pathdelim+'teapot.ply';
  {$ENDIF}
  Create(fromView, fMeshName);
end;

function Float2Int16(f: single): int16;
begin
     if f > 1 then
        exit(32767);
     if f < -1 then
        exit(-32768);
     if f > 0 then
        result := round(f * 32767)
     else
         result := round(f * 32768);
end;

{$IFDEF COREGL}
function AsGL_INT_2_10_10_10_REV(f: TPoint3f): int32;
//pack 3 32-bit floats as 10 bit signed integers, assumes floats normalized to -1..1
var
   x,y,z: uint16;
begin
     x := uint16(Float2Int16(f.X)) shr 6;
     y := uint16(Float2Int16(f.Y)) shr 6;
     z := uint16(Float2Int16(f.Z)) shr 6;
     result := (z shl 20)+ (y shl 10) + (x shl 0);
end;
{$ENDIF}

{$IFNDEF COREGL}
procedure TGPUMesh.SetVertexAttrib();
//for Legacy OpenGL: vertexattribs must be updated if either vertex_vbo or shader is updated.
//  in contrast, Modern OpenGL supports "location" in shader,  so only update when vertex_vbo is updated
var
  vertLoc, normLoc, clrLoc: GLint;
begin
  if (vertex_vbo = 0) or (shaderProgram = 0) then exit;
  glUseProgram(shaderProgram);
  glBindBuffer(GL_ARRAY_BUFFER, vertex_vbo);
  vertLoc := glGetAttribLocation(shaderProgram, 'Vert');
  normLoc := glGetAttribLocation(shaderProgram, 'Norm');
  clrLoc := glGetAttribLocation(shaderProgram, 'Clr');
  glVertexAttribPointer(vertLoc, 3, GL_FLOAT, GL_FALSE, sizeof(TVtxNormClr), PChar(0));
  glVertexAttribPointer(normLoc, 3, GL_FLOAT, GL_TRUE, sizeof(TVtxNormClr), PChar(sizeof(TPoint3f)));
  glVertexAttribPointer(clrLoc, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof(TVtxNormClr), PChar(sizeof(TPoint3f) + sizeof(TPoint3f)));
  glEnableVertexAttribArray(vertLoc);
  glEnableVertexAttribArray(normLoc);
  glEnableVertexAttribArray(clrLoc);
end;
{$ENDIF}

procedure TGPUMesh.OpenMesh(Filename: string; isSwapYZ: boolean = true);
{$IFDEF COREGL}
const
    kATTRIB_VERT = 0;  //vertex XYZ are positions 0,1,2
    kATTRIB_NORM = 3;  //normal XYZ are positions 3,4,5
    kATTRIB_CLR = 6;   //color RGBA are positions 6,7,8,9
{$ENDIF}
var
  faces: TFaces;
  verts, vNorm: TVertices;
  colors: TVertexRGBA;
  vnc: array of TVtxNormClr;
  {$IFDEF COREGL}
  vertex_vbo : GLuint = 0;
  {$ENDIF}
  i: integer;
begin
  LoadMesh(Filename, faces, verts, vNorm, colors, fMeshColor, isSwapYZ);
  if (length(verts) <> length(vNorm)) or (length(verts) <> length(colors)) then exit;
  glControl.MakeCurrent(false);
  glGetError(); //<- ignore prior errors
  //create VBO that combines vertex, normal and color information
  if (vertex_vbo <> 0) then
     glDeleteBuffers(1, @vertex_vbo);
  setlength(vnc, length(verts));
  for i := 0 to (length(verts) -1) do begin
      vnc[i].vtx := verts[i];
      {$IFDEF COREGL}
      vnc[i].norm :=  AsGL_INT_2_10_10_10_REV(vNorm[i]);
      {$ELSE}
      vnc[i].norm := vNorm[i];
      {$ENDIF}
      vnc[i].clr := colors[i];
  end;
  glGenBuffers(1, @vertex_vbo);
  glBindBuffer(GL_ARRAY_BUFFER, vertex_vbo);
  glBufferData(GL_ARRAY_BUFFER, Length(vnc)*SizeOf(TVtxNormClr), @vnc[0], GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  {$IFDEF COREGL}
  if vertex_vao <> 0 then
     glDeleteVertexArrays(1,@vertex_vao);
  glGenVertexArrays(1, @vertex_vao);
  glBindVertexArray(vertex_vao);
  glBindBuffer(GL_ARRAY_BUFFER, vertex_vbo);
  glVertexAttribPointer(kATTRIB_VERT, 3, GL_FLOAT, GL_FALSE, sizeof(TVtxNormClr), PChar(0));
  glEnableVertexAttribArray(kATTRIB_VERT);
  //Normals typically stored as 3*32 bit floats (96 bytes), but we will pack them as 10-bit integers in a single 32-bit value with GL_INT_2_10_10_10_REV
  //  https://www.opengl.org/wiki/Vertex_Specification_Best_Practices
  //Vertices
  glVertexAttribPointer(kATTRIB_NORM, 4, GL_INT_2_10_10_10_REV, GL_FALSE, sizeof(TVtxNormClr), PChar(sizeof(TPoint3f)));
  glEnableVertexAttribArray(kATTRIB_NORM);
  //Color
  glVertexAttribPointer(kATTRIB_CLR, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof(TVtxNormClr), PChar(sizeof(int32)+ sizeof(TPoint3f)));
  glEnableVertexAttribArray(kATTRIB_CLR);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
  glDeleteBuffers(1, @vertex_vbo);
  {$ENDIF}
  if (index_vbo <> 0) then
     glDeleteBuffers(1, @index_vbo);
  glGenBuffers(1, @index_vbo);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, index_vbo);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(faces)*sizeof(TPoint3i), @faces[0], GL_STATIC_DRAW);
  nface := Length(faces) * 3; //each face has 3 vertices
  GetError(2, 'OpenMesh');
  {$IFNDEF COREGL}SetVertexAttrib(); {$ENDIF}
  glControl.invalidate;
end; //OpenMesh()

procedure TGPUMesh.Paint();
var
  modelViewProjectionMatrix, projectionMatrix, modelMatrix, modelViewMatrix, normalMatrix: TMat4;
  whratio, scale: single;
begin
  if shaderProgram = 0 then
     Prepare();
  if index_vbo = 0 then
     OpenMesh(fMeshName, false);
  if (glControl.width = 0) or (glControl.height = 0) then exit; //avoid divide by zero
  {$IFDEF MATCAP}
  if (uniform_MatCap >= 0) and (matCapTexture = 0) then
     SetMatCap(matCapFnm);
  {$ENDIF}
  //glGetError(); //<- ignore prior errors
  glUseProgram(shaderProgram);
  scale := 0.6*fDistance;
  whratio := glControl.clientwidth/glControl.clientheight;
  //Form1.Caption := format('%g %d', [whratio, fromView.Width]);
  if fPerspective then
     projectionMatrix := TMat4.PerspectiveGL(fDistance/kMaxDistance * 120.0, whratio, 0.01, kMaxDistance)
  else if (whratio > 1) or (whratio = 0) then //Wide window
     projectionMatrix := TMat4.OrthoGL (-scale * whratio, scale * whratio, -scale, scale, 0.01, 5.0)
  else
      projectionMatrix := TMat4.OrthoGL (-scale, scale, -scale/whratio, scale/whratio, 0.01, 5.0);
  modelMatrix := TMat4.Identity;
  scale := 1.0;
  modelMatrix *= TMat4.Scale(0.5/Scale, 0.5/Scale, 0.5/Scale);
  modelMatrix *= TMat4.Translate(0, 0, -Scale*2);
  modelMatrix *= TMat4.RotateX(-DegToRad(90-fElevation));
  modelMatrix *= TMat4.RotateZ(DegToRad(fAzimuth));
  modelViewMatrix := modelMatrix;
  //
  normalMatrix := modelMatrix.Inverse.Transpose;
  modelViewProjectionMatrix := ( projectionMatrix * modelMatrix);
  //normalMatrix := modelMatrix.Inverse.Transpose;
  glViewport(0, 0, glControl.ClientWidth, glControl.ClientHeight); //required for form resize
  //glClearColor( ClearColor.R/255, ClearColor.G/255, ClearColor.B/255, 1.0); //Set blue background
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  //glEnable(GL_CULL_FACE); // <- ignore back face: teapot will appear hollow
  glDisable(GL_CULL_FACE);
  glEnable(GL_DEPTH_TEST);
  glUniformMatrix4fv(uniform_ModelViewProjectionMatrix, 1, GL_FALSE, @modelViewProjectionMatrix); // note model not MVP!
  glUniformMatrix4fv(uniform_ModelViewMatrix, 1, GL_FALSE, @modelViewMatrix);
  //glUniformMatrix4fv(uniform_NormalMatrix, 1, GL_FALSE, @lMatrix);
  glUniformMatrix4fv(uniform_NormalMatrix, 1, GL_FALSE, @normalMatrix);
  //glUniformMatrix3fv(uniform_NormalMatrix, 1, GL_FALSE, @normalMatrix);
  //if (uniform_lightPos > 0) then //shader might not include light position, and compiler will remove clrLoc for depth shader where color is not used
  glUniform3fv(uniform_lightPos , 1, @fLightPos);
  //glVertexAttrib3fv(uniform_lightPos , @fLightPos);
  {$IFDEF MATCAP}
  if (uniform_MatCap >= 0) then begin
     glActiveTexture(GL_TEXTURE1);
     glBindTexture(GL_TEXTURE_2D, matCapTexture);
     glUniform1i(uniform_MatCap, 1);
  end;
  {$ENDIF}
  {$IFDEF COREGL}
  glBindVertexArray(vertex_vao);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_vbo);
  //glDrawElements(GL_TRIANGLES,  nface, GL_UNSIGNED_INT, nil);
  glDrawElements(GL_TRIANGLES,  nface, GL_UNSIGNED_INT, nil);
  glBindVertexArray(0);
  {$ELSE}
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, index_vbo);
  glDrawElements(GL_TRIANGLES,  nface, GL_UNSIGNED_INT, PChar(0));
  {$ENDIF}
  glControl.SwapBuffers;
end;

end.

