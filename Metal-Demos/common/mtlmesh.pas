unit mtlmesh;
//{$DEFINE SSAO}
{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$DEFINE CUBE}
{$DEFINE MATCAP}

interface

uses
    classes,
    {$IFDEF CUBE} mtlcube, {$ENDIF}
    {$IFDEF MATCAP} Graphics, GraphType, FPImage, IntfGraphics, LCLType, strutils,{$ENDIF}
    VectorMath, MetalPipeline, MetalUtils, MetalControl, Metal,mesh,
    SysUtils, Math, SimdUtils;
const
  kDefaultDistance = 1.0;
  kMaxDistance = 2;
type
  TGPUMesh = class
      private
        indexBuffer, vertexBuffer: MTLBufferProtocol;
        fMeshColor: TRGBA;
        fAzimuth,fElevation, numVert, numIdx: integer;
        fDistance: single;
        fLightPos: TVec4;
        meshShader: TMetalPipeline;
        mtlControl: TMetalControl;
        fPerspective: boolean;
        fFracAO: single;
        {$IFDEF MATCAP}
        matCapFnm: string;
        matCapTexture: MTLTextureProtocol;
        {$ENDIF}
        {$IFDEF SSAO}
        screenWidth, screenHeight: integer;
        screenVerts: array[0..3] of TVec4;
        MeshRenderPassDescriptor: MTLRenderPassDescriptor;
        outputColorTexture, outputDepthTexture: MTLTextureProtocol;
        SSAORenderPassDescriptor: MTLRenderPassDescriptor;
        ssaoShader: TMetalPipeline;
        {$ENDIF}
        {$IFDEF CUBE} gCube :TGPUCube; {$ENDIF}
        fMeshName: string;
      private
             procedure Prepare();
      public
        {$IFDEF MATCAP}
        uniform_MatCap: integer; // >=0 if shader supports matcaps
        function SetMatCap(fnm: string): boolean;
        function MatCapPath(): string;
        {$ENDIF}
        property nVert: integer read numVert;
        property MeshName: string read fMeshName;
        property Perspective: boolean read fPerspective write fPerspective;
        //{$IFDEF SSAO}
        property Occlusion: single read fFracAO write fFracAO;
        //{$ENDIF}
        property MeshColor: TRGBA read fMeshColor write fMeshColor;
        property Azimuth: integer read fAzimuth write fAzimuth;
        property Elevation: integer read fElevation write fElevation;
        property Distance: single read fDistance write fDistance;
        property LightPosition: TVec4 read fLightPos write fLightPos;
        constructor Create(fromView: TMetalControl); overload;
        function ShaderPath(): string;
        constructor Create(fromView: TMetalControl; InitMeshName: string); overload;
        procedure Paint();
        procedure OpenMesh(Filename: string; isSwapYZ: boolean = true);
        procedure SetShader(shaderName: string);
        procedure SaveBmp(filename: string);
        destructor Destroy; override;
  end;

implementation

Type
TVertUniforms = record //Uniforms for vertex shader
  modelViewProjectionMatrix: TMat4;
  modelViewMatrix: TMat4;
  normalMatrix: TMat4;
  lightPos: TVec4;
end;

TSSAOFragUniforms = record
  aoRadius, fracAO: TScalar;
end;

TVertVertex = record //Each vertex defines a location and color
  position: TVec3;
  padding: TScalar;
  //padding: array[0..0] of TScalar;
  color: TVec4;
  normal: TVec4;
end;

{$IFDEF MATCAP}
procedure printf (lS: AnsiString);
begin
{$IFNDEF WINDOWS} writeln(lS); {$ENDIF}
end;

function TGPUMesh.SetMatCap(fnm: string): boolean;
var
  px: TPicture;
  AImage: TLazIntfImage;
  lRawImage: TRawImage;
  ifnm, MatCapDir: string;
   pngTexDesc: MTLTextureDescriptor;
 pngRegion: MTLRegion;
begin
  result := false;
  if not fileexists(fnm) then begin
     ifnm := fnm;
     MatCapDir := ExtractFilePath(ShaderDir)+ 'matcap';
     fnm := MatCapDir+pathdelim+fnm+'.jpg';
     if not fileexists(fnm) then begin
        printf(format('LoadTex: unable to find "%s" or "%s"',[ifnm, fnm]));
        exit;
     end;
  end;
  matCapFnm := fnm;
  px := TPicture.Create;
    try
       {$IFDEF X}
       px.LoadFromFile(fnm);
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
  //Metal specific
  pngTexDesc := MTLTextureDescriptor.alloc.init.autorelease;
  pngTexDesc.setTextureType(MTLTextureType2D);
  pngTexDesc.setPixelFormat(MTLPixelFormatBGRA8Unorm);
  //pngTexDesc.setPixelFormat(MTLPixelFormatRGBA8Unorm);
  pngTexDesc.setWidth(px.Bitmap.Width);
  pngTexDesc.setHeight(px.Bitmap.Height);
  pngTexDesc.setDepth(1);
  if (matCapTexture <> nil) then matCapTexture.release;
  matCapTexture := mtlControl.renderView.device.newTextureWithDescriptor(pngTexDesc);
  Fatal(matCapTexture = nil, 'mtltexture: newTextureWithDescriptor failed');
  pngRegion := MTLRegionMake2D(0, 0, px.Bitmap.Width, px.Bitmap.Height);
  matCapTexture.replaceRegion_mipmapLevel_withBytes_bytesPerRow(pngRegion, 0, PInteger(px.Bitmap.RawImage.Data), (px.Bitmap.Width)*4);

  //release
  px.free;
  result := true;
end;

function TGPUMesh.MatCapPath(): string;
begin
     result := ResourceDir+pathdelim+'matcap'+pathdelim;
end;
{$ENDIF}


destructor TGPUMesh.Destroy;
begin
  {$IFDEF CUBE} gCube.free; {$ENDIF}
  inherited;
end;

procedure TGPUMesh.SaveBmp(filename: string);
begin
     MTLWriteTextureToFile(pChar(filename));
end;

procedure TGPUMesh.SetShader(shaderName: string);
var
 options: TMetalPipelineOptions;
begin
  options := TMetalPipelineOptions.Default;
  options.libraryName := shaderName;
  if not fileexists(shaderName) then
     writeln('Unable to find shader ' + shaderName);
  if AnsiContainsText(extractfilename(shaderName), 'matcap') then
    uniform_MatCap := 1
  else
    uniform_MatCap := -1;
  options.pipelineDescriptor := MTLCreatePipelineDescriptor;
  {$IFDEF SSAO}
	options.pipelineDescriptor.colorAttachmentAtIndex(0).setPixelFormat(MTLPixelFormatInvalid);
	options.pipelineDescriptor.colorAttachmentAtIndex(1).setPixelFormat(MTLPixelFormatBGRA8Unorm);
	options.pipelineDescriptor.colorAttachmentAtIndex(2).setPixelFormat(MTLPixelFormatR32Float);
  {$ELSE}
  options.pipelineDescriptor := MTLCreatePipelineDescriptor;
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setBlendingEnabled(true);
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setRgbBlendOperation(MTLBlendOperationAdd);
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setAlphaBlendOperation(MTLBlendOperationAdd);
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceRGBBlendFactor(MTLBlendFactorSourceAlpha);
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceAlphaBlendFactor(MTLBlendFactorSourceAlpha);
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationRGBBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationAlphaBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
  {$ENDIF}
  meshShader := MTLCreatePipeline(options);
  MTLSetDepthStencil(meshShader, MTLCompareFunctionLess, true);
end;

function VertVertex(x, y, z: TScalar; clr: TRGBA; norm: TPoint3f): TVertVertex;
begin
     result.position := V3(x,y,z);
     result.padding := 0.0;;
     result.color := V4(clr.r/255, clr.g/255, clr.b/255, 1);
     //result.normal := V4(norm.Z, norm.X, -norm.Y, 1);
     result.normal := V4(norm.X, norm.y, norm.Z, 0.0);
end;

function TGPUMesh.ShaderPath(): string;
begin
     result := ResourceFolderPath+pathdelim+'shader'+pathdelim;
end;

procedure TGPUMesh.Prepare();
var
    options: TMetalPipelineOptions;
    shaderName: string;
begin
  {$IFDEF SSAO}
  shaderName := ShaderPath()+'SphericalHarmonic.metal';
  SetShader(shaderName);
  shaderName := ShaderPath()+'_SSAO.metal';
  options := TMetalPipelineOptions.Default;
  options.libraryName := shaderName;
  if not fileexists(shaderName) then
     writeln('Unable to find shader ' + shaderName);
  options.pipelineDescriptor := MTLCreatePipelineDescriptor;
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setBlendingEnabled(true);
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setRgbBlendOperation(MTLBlendOperationAdd);
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setAlphaBlendOperation(MTLBlendOperationAdd);
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceRGBBlendFactor(MTLBlendFactorSourceAlpha);
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceAlphaBlendFactor(MTLBlendFactorSourceAlpha);
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationRGBBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationAlphaBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
  ssaoShader := MTLCreatePipeline(options);  //screen space ambient occlusion
  screenWidth := -1;
  screenHeight := -1;
  {$ELSE}
  shaderName := ShaderPath()+'Phong.metal';
  SetShader(shaderName);
  {$ENDIF}
  {$IFDEF CUBE} gCube := TGPUCube.Create(mtlControl); {$ENDIF}
end;

constructor TGPUMesh.Create(fromView: TMetalControl; InitMeshName: string);  overload;
begin
  mtlControl := fromView;
  fDistance := kDefaultDistance;
  fMeshName := InitMeshName;
  fAzimuth := 110;
  fElevation := 30;
  fLightPos := Vec4(0.0 ,0.707, 0.707, 0.0);
  fMeshColor.r := 210;
  fMeshColor.g := 148;
  fMeshColor.b := 148;
  fPerspective := false;
  vertexBuffer := nil;
  meshShader := nil;
  {$IFDEF MATCAP}
  matCapTexture := nil;
  uniform_MatCap := -1;
  matCapFnm := '';
  {$ENDIF}
  {$IFDEF SSAO}
  fFracAO := 0.25;
  screenVerts[0] := V4(-1, -1, 0, 1);
  screenVerts[1] := V4(-1, +1, 0, 1);
  screenVerts[2] := V4(+1, -1, 0, 1);
  screenVerts[3] := V4(+1, +1, 0, 1);
  meshRenderPassDescriptor := nil;
  SSAORenderPassDescriptor := nil;
  {$ENDIF}
end;

constructor TGPUMesh.Create(fromView: TMetalControl);  overload;
begin
  {$IFDEF SSAO}
  fMeshName := ResourceFolderPath+pathdelim+'brain.mz3';
  {$ELSE}
  //fMeshName := ResourceFolderPath+pathdelim+'teapot.ply';
  fMeshName := '';
  {$ENDIF}
  Create(fromView, fMeshName);
end;

procedure TGPUMesh.OpenMesh(Filename: string; isSwapYZ: boolean = true);
var
  faces: TFaces;
  verts, vNorm: TVertices;
  colors: TVertexRGBA;
  i: integer;
  mtlVertices: array of TVertVertex;
begin
  //de-allocate textures https://stackoverflow.com/questions/39158302/how-to-deallocate-a-mtlbuffer-and-mtltexture
  indexBuffer := nil;
  vertexBuffer := nil;
  LoadMesh(Filename, faces, verts, vNorm, colors, fMeshColor, isSwapYZ);
  numIdx := length(faces) * 3;//each face is a triangle with 3 vertices
  indexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@faces[0], sizeof(uint32) * numIdx, MTLResourceStorageModeShared);
  setlength(mtlVertices, length(verts));
  for i := 0 to (length(verts)-1) do begin
    //i0 := i;//min(i+1, length(verts)-1);
    //vNorm[i0] := Vec3(1,0,0);
    //writeln(format('%d %1.2f %1.2f %1.2f', [i0, vNorm[i0].x, vNorm[i0].y, vNorm[i0].z]));
    mtlVertices[i] := VertVertex(verts[i].X, verts[i].Y, verts[i].Z, colors[i], vNorm[i]);
  end;
  vertexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@mtlVertices[0], sizeof(TVertVertex) * Length(mtlVertices), MTLResourceStorageModeShared);
  numVert := length(mtlVertices);
end; //OpenMesh()

procedure TGPUMesh.Paint();
var
  {$IFDEF SSAO}
  colorAttachment: MTLRenderPassColorAttachmentDescriptor;
  ssaofragUniforms: TSSAOFragUniforms;
  {$ENDIF}
  vertUniforms: TVertUniforms;
  projectionMatrix, modelMatrix: TMat4;
  w,h: integer;
  whratio, scale: single;
begin
  if meshShader = nil then
     Prepare();
  if vertexBuffer = nil then
     OpenMesh(fMeshName, false);
  //writeln('draw');
  w := trunc(mtlControl.renderView.drawableSize.width);
  h := trunc(mtlControl.renderView.drawableSize.height);
  if (w = 0) or (h = 0) then exit; //avoid divide by zero
  {$IFDEF MATCAP}
  if (uniform_MatCap >= 0) and (matCapTexture = nil) then
     SetMatCap(matCapFnm);
  {$ENDIF}
  scale := 0.6*fDistance;
  whratio := w/h;
  if fPerspective then
     projectionMatrix := TMat4.Perspective(fDistance/kMaxDistance * 120.0, whratio, 0.01, kMaxDistance)
  else if (whratio > 1) then //Wide window
     projectionMatrix := TMat4.Ortho(-scale * whratio, scale * whratio, -scale, scale, 0.01, kMaxDistance)//5.0)
  else
      projectionMatrix := TMat4.Ortho(-scale, scale, -scale/whratio, scale/whratio, 0.01, kMaxDistance);//, 5.0);
  modelMatrix := TMat4.Identity;
  scale := 1.0;
  modelMatrix *= TMat4.Scale(0.5/Scale, 0.5/Scale, 0.5/Scale);
  modelMatrix *= TMat4.Translate(0, 0, -Scale*2);
  modelMatrix *= TMat4.RotateX(-DegToRad(90-fElevation));
  modelMatrix *= TMat4.RotateZ(DegToRad(fAzimuth));
  vertUniforms.modelViewMatrix := modelMatrix;
  vertUniforms.modelViewProjectionMatrix := ( projectionMatrix * modelMatrix);
  vertUniforms.normalMatrix := modelMatrix.Inverse.Transpose;
  vertUniforms.lightPos := fLightPos;
  //vertUniforms.lightPos := Vec4(random() ,0.1, random(), 0.0);
  {$IFDEF SSAO}
  if (w <> screenWidth) or (h <> screenHeight) then begin
     //outputColorTexture := MTLNewTexture(w, h, MTLTextureType2D, MTLPixelFormatBGRA8Unorm, MTLTextureUsageShaderRead or MTLTextureUsageShaderWrite);
     //outputDepthTexture := MTLNewTexture(w, h, MTLTextureType2D, MTLPixelFormatR32Float, MTLTextureUsageShaderRead or MTLTextureUsageShaderWrite);
     outputColorTexture := MTLNewTexture(w, h, MTLTextureType2D, MTLPixelFormatBGRA8Unorm, MTLTextureUsageShaderRead or MTLTextureUsageRenderTarget);
     outputDepthTexture := MTLNewTexture(w, h, MTLTextureType2D, MTLPixelFormatR32Float, MTLTextureUsageShaderRead or MTLTextureUsageRenderTarget);

     //outputColorTexture := MTLNewTexture(trunc(view.drawableSize.width), trunc(view.drawableSize.height), MTLTextureType2D, options.pipelineDescriptor.colorAttachmentAtIndex(1).pixelFormat, MTLTextureUsageShaderRead or MTLTextureUsageRenderTarget);
     //outputDepthTexture := MTLNewTexture(trunc(view.drawableSize.width), trunc(view.drawableSize.height), MTLTextureType2D, options.pipelineDescriptor.colorAttachmentAtIndex(2).pixelFormat, MTLTextureUsageShaderRead or MTLTextureUsageRenderTarget);

     screenWidth := w;
     screenHeight := h;
     meshRenderPassDescriptor := nil
  end;
  if meshRenderPassDescriptor = nil then begin
     meshRenderPassDescriptor := MTLRenderPassDescriptor.alloc.init;
     meshRenderPassDescriptor.depthAttachment.setTexture(mtlControl.renderView.depthStencilTexture);
     colorAttachment := meshRenderPassDescriptor.colorAttachmentAtIndex(1);
     colorAttachment.setTexture(outputColorTexture);
     colorAttachment.setClearColor(mtlControl.renderView.clearColor);
     colorAttachment.setLoadAction(MTLLoadActionClear);
     colorAttachment.setStoreAction(MTLStoreActionStore);
     colorAttachment := meshRenderPassDescriptor.colorAttachmentAtIndex(2);
     colorAttachment.setTexture(outputDepthTexture);
     colorAttachment.setClearColor(mtlControl.renderView.clearColor);
     colorAttachment.setLoadAction(MTLLoadActionClear);
     colorAttachment.setStoreAction(MTLStoreActionStore);
  end;
    MTLBeginCommand;
       MTLBeginEncoding(meshShader, meshRenderPassDescriptor);
         MTLSetCullMode(MTLCullModeNone);
         MTLSetVertexBuffer(vertexBuffer, 0, 0);
         MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
         MTLDrawIndexed (MTLPrimitiveTypeTriangle, numIdx, MTLIndexTypeUInt32, indexBuffer, 0);
       MTLEndEncoding;
       MTLBeginEncoding(ssaoShader); //composite g-buffer to screen: ambient occlusion
         ssaofragUniforms.fracAO := ffracAO;
         ssaofragUniforms.aoRadius := max( w, h)/(96*fDistance) ;
         MTLSetFragmentBytes(@ssaofragUniforms, sizeof(ssaofragUniforms), 0);
         MTLSetVertexBytes(@screenVerts, sizeof(screenVerts), 0);
         MTLSetFragmentTexture(outputColorTexture, 0);
         MTLSetFragmentTexture(outputDepthTexture, 1);
         MTLDraw(MTLPrimitiveTypeTriangleStrip, 0, 4);
       MTLEndEncoding;
     MTLEndCommand;
    {$ELSE}
    MTLBeginFrame(meshShader);
      {$IFDEF MATCAP}
      if (uniform_MatCap >= 0) and (matCapTexture <> nil) then
         MTLSetFragmentTexture(matCapTexture, 0);
      {$ENDIF}
      MTLSetCullMode(MTLCullModeNone);
      MTLSetVertexBuffer(vertexBuffer, 0, 0);
      MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
      MTLDrawIndexed (MTLPrimitiveTypeTriangle, numIdx, MTLIndexTypeUInt32, indexBuffer, 0);
      {$IFDEF CUBE}
      if true then begin
       gCube.Azimuth:=fAzimuth;
       gCube.Elevation:=-fElevation;
       gCube.Draw(mtlControl.clientwidth, mtlControl.clientheight);
      end;
      {$ENDIF}
    MTLEndFrame;
    {$ENDIF}
end;

end.

