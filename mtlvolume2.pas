unit mtlvolume2;
{$mode objfpc}
{$modeswitch objectivec1}
{$H+}
interface

{$DEFINE GPUGRADIENTS} //Computing volume gradients on the GPU is much faster than using the CPU
{$DEFINE VIEW2D}
{$DEFINE CUBE}
uses
    {$IFDEF CUBE} mtlcube, {$ENDIF}
    mtlfont, mtlclrbar, VectorMath, MetalPipeline, MetalControl, Metal, MetalUtils,
    SysUtils, Math, nifti, niftis, SimdUtils, Classes
    {$IFDEF VIEW2D}, drawvolume, slices2D, colorEditor{$ENDIF};
const
 kDefaultDistance = 2.25;
 kMaxDistance = 40;
type
  TGPUVolume = class
      private
        {$IFDEF VIEW2D}
        colorEditorVertexBuffer, sliceVertexBuffer, renderVertexBuffer, lineVertexBuffer: MTLBufferProtocol;
        shader2D, shaderLines2D: TMetalPipeline;
        MeshRenderPassDescriptor: MTLRenderPassDescriptor;
        slices2D: TSlices2D;
        colorEditor: TColorEditor;
        colorEditorVisible: boolean;
        txt: TGPUFont;
        drawVolTex, drawVolLut: MTLTextureProtocol;
        {$ENDIF}
        RayCastQuality1to10, maxDim, fAzimuth,fElevation: integer;
        shaderPrefs: TShaderPrefs;
        prefValues: array [1..kMaxUniform] of single;
        fDistance, overlayNum: single;
        fLightPos, fClipPlane: TVec4;
        indexBuffer, vertexBuffer: MTLBufferProtocol;
        shader3D: TMetalPipeline;
        volTex, gradTex, overlayVolTex, overlayGradTex: MTLTextureProtocol;
        mtlControl: TMetalControl;
        clrbar: TGPUClrbar;
        {$IFDEF CUBE} gCube :TGPUCube; {$ENDIF}
        procedure LoadCube();
        procedure LoadTexture(var vol: TNIfTI);
        procedure CreateDrawColorTable;//1D texture for drawing
        procedure CreateDrawTex(Dim: TVec3i; Vals: TUInt8s);
        procedure UpdateDraw(Drawing: TDraw);
        procedure CreateOverlayTextures(Dim: TVec3i; volRGBA: TRGBAs);
        procedure CreateGradientVolumeGPU(Xsz,Ysz,Zsz: integer; var inTex, grTex: MTLTextureProtocol);
      public
        {$IFDEF VIEW2D}
        SelectionRect: TVec4;
        property ShowColorEditor: boolean read colorEditorVisible write colorEditorVisible;
        //function ColorEditorMouseDown(mouseX, mouseY: integer): boolean;
        function Slice2Dmm(var vol: TNIfTI; out vox: TVec3i): TVec3;
        //function FracVox: TVec3i;
        property CE: TColorEditor read colorEditor;
        property Slices: Tslices2D read slices2D;
        procedure SetSlice2DFrac(frac : TVec3);
        function GetSlice2DFrac(mouseX, mouseY: integer; out Orient: integer): TVec3;
        function GetSlice2DMaxXY(mouseX, mouseY: integer; var Lo: TPoint): TPoint;
        procedure Paint2D(var vol: TNIfTI; Drawing: TDraw; DisplayOrient: integer);
        procedure PaintMosaicRender(var vol: TNIfTI; lRender: TMosaicRender);
        procedure PaintMosaic2D(var vol: TNIfTI; Drawing: TDraw; MosaicString: string);
        {$ENDIF}
        procedure UpdateOverlays(vols: TNIfTIs);
        property Quality1to10: integer read RayCastQuality1to10 write RayCastQuality1to10;
        property ShaderSliders: TShaderPrefs read shaderPrefs write shaderPrefs;
        property Azimuth: integer read fAzimuth write fAzimuth;
        property Elevation: integer read fElevation write fElevation;
        property Distance: single read fDistance write fDistance;
        property LightPosition: TVec4 read fLightPos write fLightPos;
        property ClipPlane: TVec4 read fClipPlane write fClipPlane;
        procedure Prepare(shaderName: string);
        constructor Create(fromView: TMetalControl);
        procedure Paint(var vol: TNIfTI);
        procedure SetShader(shaderName: string);
        procedure SetShaderSlider(idx: integer; newVal: single);
        procedure SaveBmp(filename: string);
        procedure SetColorBar(fromColorbar: TGPUClrbar);
        procedure  SetTextContrast(clearclr: TRGBA);
        destructor Destroy; override;
  end;

implementation

//uses mainunit;

destructor TGPUVolume.Destroy;
begin
  {$IFDEF VIEW2D}
  slices2D.free;
  colorEditor.free;
  txt.free;
  {$ENDIF}
  {$IFDEF CUBE} gCube.free; {$ENDIF}
  clrbar.free;
  inherited;
end;

type
  TVertVertex = record //Each vertex defines a location and color
    position: TVec3;
    padding: array[0..0] of TScalar;
    color: TVec4;
  end;
  TVertVertexArray =  array of TVertVertex;
  TVertUniforms = record //Uniforms for vertex shader
    modelViewProjectionMatrix: TMat4;
  end;
  TFragUniforms = record //Uniforms for fragment shader
    stepSize, sliceSize, numOverlays, backAlpha: TScalar;
    rayDir: TVec4;
    lightPos: TVec4;
    clipPlane: TVec4;
  end;

procedure  TGPUVolume.SetTextContrast(clearclr: TRGBA);
begin
  if (clearclr.R + clearclr.G + clearclr.B) > 300 then
     txt.FontColor := Vec4(0,0,0,1)
  else
     txt.FontColor := Vec4(1,1,1,1);
end;

procedure TGPUVolume.SetShaderSlider(idx: integer; newVal: single);
begin
    if (idx < 1) or (idx > kMaxUniform) then exit;
    prefValues[idx] :=newVal;
end;

procedure TGPUVolume.SaveBmp(filename: string);
begin
     MTLWriteTextureToFile(pChar(filename));
end;

procedure TGPUVolume.SetShader(shaderName: string);
var
 options: TMetalPipelineOptions;
 i: integer;
begin
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
 shader3D := MTLCreatePipeline(options);
 shaderPrefs := loadShaderPrefs(shaderName);
 if (shaderPrefs.nUniform > 0) and (shaderPrefs.nUniform <= kMaxUniform) then
    for i := 1 to shaderPrefs.nUniform do
        prefValues[i] := shaderPrefs.Uniform[i].DefaultV;
 //MTLSetDepthStencil(shader3D, MTLCompareFunctionLess, true);
end;

procedure TGPUVolume.Prepare(shaderName: string);
var
 {$IFDEF VIEW2D}
 options: TMetalPipelineOptions;
 success: boolean;
 {$ENDIF}
begin
 if (shaderName = '') or (not fileexists(shaderName)) then
    shaderName := ShaderDir+pathdelim+'Default.metal';
 SetShader(shaderName);
 {$IFDEF VIEW2D}
 meshRenderPassDescriptor := nil;
 options := TMetalPipelineOptions.Default;
 shaderName := ShaderDir+pathdelim+'_Texture2D.metal';
 options.libraryName := shaderName;
 if not fileexists(shaderName) then
  writeln('Unable to find ' + shaderName);
 options.pipelineDescriptor := MTLCreatePipelineDescriptor;
 options.pipelineDescriptor.colorAttachmentAtIndex(0).setBlendingEnabled(true);
 options.pipelineDescriptor.colorAttachmentAtIndex(0).setRgbBlendOperation(MTLBlendOperationAdd);
 options.pipelineDescriptor.colorAttachmentAtIndex(0).setAlphaBlendOperation(MTLBlendOperationAdd);
 options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceRGBBlendFactor(MTLBlendFactorSourceAlpha);
 options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceAlphaBlendFactor(MTLBlendFactorSourceAlpha);
 options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationRGBBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
 options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationAlphaBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
 shader2D := MTLCreatePipeline(options);
 shaderName := ShaderDir+pathdelim+'_Line2D.metal';
 options.libraryName := shaderName;
 if not fileexists(shaderName) then
  writeln('Unable to find ' + shaderName);
 shaderLines2D  := MTLCreatePipeline(options);
 //Create(fnm : string; out success: boolean; var fromView: TMetalControl);
 colorEditor := TColorEditor.Create;
 //colorEditorVisible := false;
 Txt := TGPUFont.Create(ResourcePath('Roboto', 'png'),  success, mtlControl); //<-multi-channel channel fonts glmtext
 slices2D := TSlices2D.Create(Txt);
 {$IFDEF CUBE} gCube := TGPUCube.Create(mtlControl); {$ENDIF}
 CreateDrawTex(pti(4,4,4), nil);
 CreateDrawColorTable;
 {$ENDIF}
end;

procedure TGPUVolume.SetColorBar(fromColorbar: TGPUClrbar);
begin
     clrbar := fromColorbar;
end;

constructor TGPUVolume.Create(fromView: TMetalControl);
begin
  mtlControl := fromView;
  clrbar := nil;
  volTex := nil;
  gradTex := nil;
  overlayVolTex := nil;
  overlayGradTex := nil;
  overlayNum := 0;
  fDistance := kDefaultDistance;
  colorEditorVertexBuffer := nil;
  sliceVertexBuffer := nil;
  renderVertexBuffer := nil;
  lineVertexBuffer := nil;
  fAzimuth := 110;
  fElevation := 30;
  RaycastQuality1to10 := 6;
  SelectionRect := Vec4(-1,0,0,0);
  fLightPos := Vec4(0, 0.707, 0.707, 0);
  //fLightPos := Vec4(0, 0.087, 0.996, 0);
  fClipPlane := Vec4(0, 0, 0, -1);
  //fLightPos := Vec4(0,0.0,0.707, 0.0);
  //fClearColor.r := 200;
  //fClearColor.g := 200;
  //fClearColor.b := 255;
  vertexBuffer := nil;
  colorEditorVisible := false;
  shaderPrefs.nUniform:= 0;
end;

function VertVertex(x, y, z: TScalar): TVertVertex;
begin
     result.position := V3(x,y,z);
     result.color := V4(x,y,z, 1);
end;

procedure TGPUVolume.LoadCube();
const
 mtlFaces: array[0..13] of uint16 = (1,0,4,3,7,6,4,5,1,6,2,3,1,0);
 //mtlFaces: array[0..13] of uint16 = (0,1,3,2,6,1,5,4, 6,7,3, 4, 0, 1);
var
  v0,v1,v2, v3,v4,v5, v6, v7:TVertVertex;
  vertices: array of TVertVertex;
begin
 v0 := VertVertex(0,0,0);
 v1 := VertVertex(0,1,0);
 v2 := VertVertex(1,1,0);
 v3 := VertVertex(1,0,0);
 v4 := VertVertex(0,0,1);
 v5 := VertVertex(0,1,1);
 v6 := VertVertex(1,1,1);
 v7 := VertVertex(1,0,1);
 vertices := TVertVertexArray.Create(v0,v1,v2,v3,v4,v5,v6,v7);
 indexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@mtlFaces[0], sizeof(uint16) * Length(mtlFaces), MTLResourceStorageModeShared);
 vertexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@vertices[0], sizeof(TVertVertex) * Length(vertices), MTLResourceStorageModeShared);
end;

procedure TGPUVolume.CreateGradientVolumeGPU(Xsz,Ysz,Zsz: integer; var inTex, grTex: MTLTextureProtocol);
var
  grTexDesc: MTLTextureDescriptor;
  options: TMetalPipelineOptions;
  blurShader, sobelShader : TMetalPipeline;
  threadgroupSize: MTLSize;
  threadgroupCount: MTLSize;
  tempTex: MTLTextureProtocol;
begin
  grTexDesc := MTLTextureDescriptor.alloc.init.autorelease;
  grTexDesc.setTextureType(MTLTextureType3D);
  grTexDesc.setUsage(MTLTextureUsageShaderWrite or MTLTextureUsageShaderRead);
  grTexDesc.setPixelFormat(MTLPixelFormatRGBA8Unorm);
  grTexDesc.setWidth(Xsz);
  grTexDesc.setHeight(Ysz);
  grTexDesc.setDepth(Zsz);
  //grTex := nil; //789
  if (grTex <> nil) then grTex.release;
  grTex := mtlControl.renderView.device.newTextureWithDescriptor(grTexDesc);
  Fatal(grTex = nil, 'CreateGradientVolumeGPU: newTextureWithDescriptor failed');
  tempTex := mtlControl.renderView.device.newTextureWithDescriptor(grTexDesc);
  threadgroupSize := MTLSizeMake(8, 8, 8);
  threadgroupCount.width  := (Xsz  + threadgroupSize.width -  1) div threadgroupSize.width;
  threadgroupCount.height := (Ysz + threadgroupSize.height - 1) div threadgroupSize.height;
  threadgroupCount.depth := (Zsz + threadgroupSize.depth - 1) div threadgroupSize.depth;
  options := TMetalPipelineOptions.Default;
  options.libraryName := ShaderDir+pathdelim+'_Blur3d.metal';
  options.kernelFunction := 'sobelKernel';  //blur kernel
  sobelShader := MTLCreatePipeline(options);
  options.kernelFunction := 'blurKernel';  //blur kernel
  blurShader := MTLCreatePipeline(options);
  MTLBeginCommand;
  MTLBeginEncoding(blurShader);
     MTLSetTexture(inTex, 0); //in
     MTLSetTexture(tempTex, 1);//out
     MTLSetDispatchThreadgroups(threadgroupCount, threadgroupSize);
  MTLEndEncoding;
  MTLBeginEncoding(sobelShader);
       MTLSetTexture(tempTex, 0); //in
       MTLSetTexture(grTex, 1);//out
       MTLSetDispatchThreadgroups(threadgroupCount, threadgroupSize);
   MTLEndEncoding;
  //MTLEndCommand;
  MTLEndCommand(true); //<- syncrhonous: waitUntilCompleted, reduce flicker
  sobelShader.Free;
  blurShader.Free;
  tempTex.release;
  tempTex := nil;
end;

procedure TGPUVolume.CreateDrawColorTable;//1D texture for drawing
var
  volRegion: MTLRegion;
  volTexDesc: MTLTextureDescriptor;
  i: integer;
  colorLut: array [0..255] of TRGBA;
begin
  colorLut[0] := setRGBA(0,0,0,0);
  colorLut[1] := setRGBA(255,0,0,255);//red
  colorLut[2] := setRGBA(0,128,0,255);//green
  colorLut[3] := setRGBA(0,0,255,255);//blue
  colorLut[4] := setRGBA(255,128,0,255);//orange
  colorLut[5] := setRGBA(128,0,255,255);//purple
  colorLut[6] := setRGBA(0,200,200,255);//cyan
  colorLut[7] := setRGBA(160,48,48,255);//brick
  colorLut[8] := setRGBA(32,255,32,255);//lime
  colorLut[9] := setRGBA(128,160,230,255);//lightblue
  for i := 10 to 255 do
      colorLut[i] := colorLut[((i-1) mod 9)+1];
  volTexDesc := MTLTextureDescriptor.alloc.init.autorelease;
  volTexDesc.setTextureType(MTLTextureType1D);
  volTexDesc.setPixelFormat(MTLPixelFormatRGBA8Unorm);//MTLPixelFormatRGBA8Uint
  volTexDesc.setWidth(256);
  volTexDesc.setHeight(1);
  volTexDesc.setDepth(1);
  //drawVolLut := nil; //789
  if (drawVolLut <> nil) then  drawVolLut.release;
  drawVolLut := mtlControl.renderView.device.newTextureWithDescriptor(volTexDesc);
  Fatal(drawVolLut = nil, 'drawVolume: newTextureWithDescriptor failed');
  //i := 256;
  //volRegion := MTLRegionMake1D(i);
  volRegion := MTLRegionMake1D(0, 256);
  drawVolLut.replaceRegion_mipmapLevel_slice_withBytes_bytesPerRow_bytesPerImage(volRegion, 0,0, @colorLut[0], 0, 0);

end;

procedure TGPUVolume.UpdateDraw(Drawing: TDraw);
begin
     if not Drawing.NeedsUpdate then exit;
     if Drawing.IsOpen then
        CreateDrawTex(Drawing.Dim, Drawing.VolRawBytes)
     else
        CreateDrawTex(pti(4,4,4), nil);
     Drawing.NeedsUpdate := false;
end;

procedure TGPUVolume.CreateDrawTex(Dim: TVec3i; Vals: TUInt8s);
var
 volRegion: MTLRegion;
 volTexDesc: MTLTextureDescriptor;
 i, vx: integer;
 v: TUInt8s;
begin
  volTexDesc := MTLTextureDescriptor.alloc.init.autorelease;
  volTexDesc.setTextureType(MTLTextureType3D);
  volTexDesc.setPixelFormat(MTLPixelFormatR8Unorm);//MTLPixelFormatRGBA8Uint
  volTexDesc.setWidth(Dim.X);
  volTexDesc.setHeight(Dim.Y);
  volTexDesc.setDepth(Dim.Z);
  //drawVolTex := nil; //789
  if drawVolTex <> nil then drawVolTex.release;
  drawVolTex := mtlControl.renderView.device.newTextureWithDescriptor(volTexDesc);
  Fatal(drawVolTex = nil, 'DrawTex: newTextureWithDescriptor failed');
  volRegion := MTLRegionMake3D(0, 0, 0, Dim.X, Dim.Y, Dim.Z);
  //GLForm1.caption := format('%d %d %d', [Dim.X, Dim.Y, Dim.Z]);
  if (Vals = nil) then begin
     vx := prod(Dim);
     setlength(v,vx);
     for i := 0 to (vx -1) do
         v[i] := random(3);
     drawVolTex.replaceRegion_mipmapLevel_slice_withBytes_bytesPerRow_bytesPerImage(volRegion, 0,0, @v[0], Dim.X, Dim.X*Dim.Y);
     v := nil;
  end else
      drawVolTex.replaceRegion_mipmapLevel_slice_withBytes_bytesPerRow_bytesPerImage(volRegion, 0,0, @Vals[0], Dim.X, Dim.X*Dim.Y);
end;

procedure TGPUVolume.CreateOverlayTextures(Dim: TVec3i; volRGBA: TRGBAs);
var
  {$IFNDEF GPUGRADIENTS}
  gradRegion: MTLRegion;
  gradData: TRGBAs;
  {$ENDIF}
  volRegion: MTLRegion;
  volTexDesc: MTLTextureDescriptor;
begin
  if (volRGBA = nil) then begin
     Dim := pti(1,1,1);
     setlength(volRGBA,1);
     volRGBA[0] := setrgba(0,0,0,0);
     overlayNum := 0; //perhaps overlays loaded but made transparent
  end;
  volTexDesc := MTLTextureDescriptor.alloc.init.autorelease;
  volTexDesc.setTextureType(MTLTextureType3D);
  volTexDesc.setPixelFormat(MTLPixelFormatRGBA8Unorm);//MTLPixelFormatRGBA8Uint
  volTexDesc.setWidth(Dim.X);
  volTexDesc.setHeight(Dim.Y);
  volTexDesc.setDepth(Dim.Z);
  //overlayVolTex := nil; //789
  if overlayVolTex <> nil then overlayVolTex.release;
  overlayVolTex := mtlControl.renderView.device.newTextureWithDescriptor(volTexDesc);
  Fatal(overlayVolTex = nil, 'volTex: newTextureWithDescriptor failed');
  volRegion := MTLRegionMake3D(0, 0, 0, Dim.X, Dim.Y, Dim.Z);
  //GLForm1.caption := format('%d %d %d', [Dim.X, Dim.Y, Dim.Z]);
  overlayVolTex.replaceRegion_mipmapLevel_slice_withBytes_bytesPerRow_bytesPerImage(volRegion, 0,0, @volRGBA[0], Dim.X*4, Dim.X*Dim.Y*4);
  //if not skipGradientCalc then begin
  {$IFDEF GPUGRADIENTS}
  CreateGradientVolumeGPU(Dim.X, Dim.Y, Dim.Z, overlayVolTex, overlayGradTex);
  {$ELSE}
  //Vol.CreateGradientVolume (intensityData, Dim.X, Dim.Y, Dim.Z, gradData);
  CreateGradientVolumeX (TUInt8s(volRGBA), Dim.X, Dim.Y, Dim.Z, 1, gradData);
  volTexDesc.setUsage(MTLTextureUsageShaderWrite or MTLTextureUsageShaderRead);
  if (overlayGradTex <> nil) then overlayGradTex.release;
  overlayGradTex := mtlControl.renderView.device.newTextureWithDescriptor(volTexDesc);
  Fatal(overlayGradTex = nil, 'newTextureWithDescriptor failed');
  gradRegion := MTLRegionMake3D(0, 0, 0, Dim.X, Dim.Y, Dim.Z);
  overlayGradTex.replaceRegion_mipmapLevel_slice_withBytes_bytesPerRow_bytesPerImage(gradRegion, 0,0, @gradData[0], Dim.X*4, Dim.X*Dim.Y*4);
  gradData := nil;
  {$ENDIF}
  //end;
  volRGBA := nil; //free
end;

procedure TGPUVolume.UpdateOverlays(vols: TNIfTIs);
var
  intensityData: TRGBAs;
  Vol: TNIfTI;
begin
  if not vols.Layer(0,Vol) then exit;
  overlayNum := vols.NumLayers -1; //-1 as we ignore background
  if (overlayNum < 1) then begin //background only
    //GLForm1.LayerBox.Caption := '->>'+inttostr(random(888));
    if (overlayGradTex = nil) or (overlayGradTex.width > 1) then //e.g. update texture after user closes overlays
       CreateOverlayTextures(Vol.Dim, nil); // <- empty overlay texture
    exit; //no overlays
  end;
  if not vols.OverlaysNeedsUpdate then exit;
  intensityData := vols.CreateOverlayVolume;
  CreateOverlayTextures(Vol.Dim, intensityData);
end;

procedure TGPUVolume.LoadTexture(var vol: TNIfTI);
var
 volTexDesc: MTLTextureDescriptor;
 volRegion: MTLRegion;
 {$IFNDEF GPUGRADIENTS}
 //gradTexDesc: MTLTextureDescriptor;
 gradData: TRGBAs;
 gradRegion: MTLRegion;
 {$ENDIF}
begin
 volTex := nil;
 if (Vol.VolRGBA = nil) then exit;
 maxDim := max(Vol.Dim.X,max(Vol.Dim.Y,Vol.Dim.Z));
 volTexDesc := MTLTextureDescriptor.alloc.init.autorelease;
 volTexDesc.setTextureType(MTLTextureType3D);
 volTexDesc.setPixelFormat(MTLPixelFormatRGBA8Unorm);//MTLPixelFormatRGBA8Uint
 volTexDesc.setWidth(Vol.Dim.X);
 volTexDesc.setHeight(Vol.Dim.Y);
 volTexDesc.setDepth(Vol.Dim.Z);
 //volTex := nil; //789
 if volTex <> nil then volTex.release;
 volTex := mtlControl.renderView.device.newTextureWithDescriptor(volTexDesc);
 Fatal(volTex = nil, 'loadTex: newTextureWithDescriptor failed');
 volRegion := MTLRegionMake3D(0, 0, 0, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z);
 volTex.replaceRegion_mipmapLevel_slice_withBytes_bytesPerRow_bytesPerImage(volRegion, 0,0, @Vol.VolRGBA[0], Vol.Dim.X*4, Vol.Dim.X*Vol.Dim.Y*4);
 //compute and load gradients
 {$IFDEF GPUGRADIENTS}
 CreateGradientVolumeGPU(Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, volTex, gradTex);
 {$ELSE}
 (*gradTexDesc := MTLTextureDescriptor.alloc.init.autorelease;
 gradTexDesc.setTextureType(MTLTextureType3D);
 gradTexDesc.setUsage(MTLTextureUsageShaderWrite or MTLTextureUsageShaderRead);
 gradTexDesc.setPixelFormat(MTLPixelFormatRGBA8Unorm);
 gradTexDesc.setWidth(Vol.Dim.X);
 gradTexDesc.setHeight(Vol.Dim.Y);
 gradTexDesc.setDepth(Vol.Dim.Z);
 gradTex := mtlControl.renderView.device.newTextureWithDescriptor(gradTexDesc);*)
 volTexDesc.setUsage(MTLTextureUsageShaderWrite or MTLTextureUsageShaderRead);
 if gradTex <> nil then gradTex.release;
 gradTex := mtlControl.renderView.device.newTextureWithDescriptor(volTexDesc);
 Fatal(gradTex = nil, 'newTextureWithDescriptor failed');
 gradRegion := MTLRegionMake3D(0, 0, 0, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z);
 gradData := Vol.GenerateGradientVolume;
 //CreateGradientVolume (Vol.VolRGBA, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, gradData);
 gradTex.replaceRegion_mipmapLevel_slice_withBytes_bytesPerRow_bytesPerImage(gradRegion, 0,0, @gradData[0], Vol.Dim.X*4, Vol.Dim.X*Vol.Dim.Y*4);
 // volTex.replaceRegion_mipmapLevel_slice_withBytes_bytesPerRow_bytesPerImage( volRegion, 0,0, @gradData[0], Vol.Dim.X*4, Vol.Dim.X*Vol.Dim.Y*4);
 gradData := nil;
 {$ENDIF}
 Vol.GPULoadDone;
 if (overlayVolTex = nil) then CreateOverlayTextures(Vol.Dim, nil); //load blank overlay
end;

procedure addFuzz (var v: TVec4); //avoid shader compile by zero error
const
     kEPS = 0.0001;
begin
   if (abs(v.x) < kEPS) then v.x := kEPS;
   if (abs(v.y) < kEPS) then v.y := kEPS;
   if (abs(v.z) < kEPS) then v.z := kEPS;
   if (abs(v.w) < kEPS) then v.w := kEPS;
end;

function lerp (p1,p2,frac: single): single;
begin
  result := round(p1 + frac * (p2 - p1));
end;//linear interpolation

function ComputeStepSize (Quality1to10, Slices: integer): single;
var
  f: single;
begin
  f := Quality1to10;
  if f <= 1 then
    f := 0.25;
  if f > 10 then
    f := 10;
  f := f/10;
  f := lerp (slices*0.25,slices*2.0,f);
  if f < 10 then
    f := 10;
  result := 1/f;
end;
{$IFDEF VIEW2D}
type
  TVertUniforms2D = record //Uniforms for vertex shader
    viewportSize: TVec2;
  end;
  TFragUniforms2D = record //Uniforms for fragment shader
      backAlpha, drawAlpha, pad1, pad2: TScalar;
  end;

function TGPUVolume.Slice2Dmm(var vol: TNIfTI; out vox: TVec3i): TVec3;
begin
    result := slices2D.FracMM(vol.Mat, vol.Dim, vox);
end;

procedure TGPUVolume.SetSlice2DFrac(frac : TVec3);
begin
     slices2D.sliceFrac := frac;
end;

(*function TGPUVolume.ColorEditorMouseDown(mouseX, mouseY: integer): boolean;
begin
   if not colorEditorVisible then exit(false);
   result := colorEditor.ColorEditorMouseDown(mouseX, mouseY);

end; *)

function TGPUVolume.GetSlice2DMaxXY(mouseX, mouseY: integer; var Lo: TPoint): TPoint;
begin
  result := slices2D.GetSlice2DMaxXY(mouseX, mouseY, lo);
end;

function TGPUVolume.GetSlice2DFrac(mouseX, mouseY: integer; out Orient: integer): TVec3;
begin
    result := slices2D.GetSlice2DFrac(mouseX, mouseY, Orient);
end;

procedure TGPUVolume.Paint2D(var vol: TNIfTI; Drawing: TDraw; DisplayOrient: integer);
var
 vertUniforms: TVertUniforms2D;
 fragUniforms: TFragUniforms2D;
 w,h: single;
begin
  if vertexBuffer = nil then // only once
    LoadCube();
  if (vol.VolRGBA <> nil) then
     LoadTexture(vol);
  if (volTex = nil) then
    exit;
  w := mtlControl.clientwidth;
  h := mtlControl.clientheight;
  if (clrbar <> nil) and (clrbar.PanelFraction < 1.0) and (clrbar.PanelFraction > 0.0) then begin
     if (clrbar.isVertical) then
        w := round(w * (1.0-clrbar.PanelFraction))
     else
         h := round(h * (1.0-clrbar.PanelFraction));
   slices2D.Update(vol.Scale, w, h, DisplayOrient, mtlControl.clientheight);
   w := mtlControl.clientwidth;
   h := mtlControl.clientheight;
  end else
      slices2D.Update(vol.Scale, w, h, DisplayOrient);
  if SelectionRect.x > 0 then
     slices2D.DrawOutLine(SelectionRect.X,h-SelectionRect.Y,SelectionRect.Z,h-SelectionRect.W);

  //w := mtlControl.clientwidth;
  //h := mtlControl.clientheight;

  if slices2D.NumberOfVertices < 3 then exit;
  vertUniforms.viewportSize := V2(w, h);
  if vol.IsLabels then
     fragUniforms.backAlpha :=  1
  else
   fragUniforms.backAlpha := vol.OpacityPercent/100;
  UpdateDraw(Drawing);
  if (not Drawing.IsOpen) or (Drawing.OpacityFraction <= 0.0) then
     fragUniforms.drawAlpha := 0.0
  else
      fragUniforms.drawAlpha := Drawing.OpacityFraction; //<- loaded!
  MTLBeginFrame();
    //draw 2D slices
    MTLSetShader(shader2D);
    MTLSetFragmentTexture(volTex, 0);
    MTLSetFragmentTexture(overlayVolTex, 1);
    MTLSetFragmentTexture(drawVolTex, 2);
    MTLSetFragmentTexture(drawVolLut, 3);
    if slices2D.HasNewSlices then begin
       sliceVertexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@slices2D.SliceVertices[0], slices2D.NumberOfVertices * sizeof(TVertex2D), MTLResourceStorageModeShared);
       slices2D.HasNewSlices := false;
    end;
    MTLSetVertexBuffer(sliceVertexBuffer, 0, 0);
    MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
    MTLSetFragmentBytes(@fragUniforms, sizeof(fragUniforms), 2);
    MTLDraw(MTLPrimitiveTypeTriangle, 0, slices2D.NumberOfVertices);
    //draw lines
    if slices2D.NumberOfLineVertices > 0 then begin
      MTLSetShader(shaderLines2D);
      if slices2D.HasNewLines then begin
         lineVertexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@slices2D.LineVertices[0], slices2D.NumberOfLineVertices * sizeof(TVertex2D), MTLResourceStorageModeShared);
         slices2D.HasNewLines := false;
      end;
      MTLSetVertexBuffer(lineVertexBuffer, 0, 0);
      //MTLSetVertexBytes(@slices2D.LineVertices[0], slices2D.NumberOfLineVertices * sizeof(TVertex2D), 0); //limited to 4096 bytes!
      MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
      MTLDraw(MTLPrimitiveTypeTriangle, 0, slices2D.NumberOfLineVertices);
    end;
    //draw color editor
    if colorEditorVisible then begin
      colorEditor.Update( w, h, vol);
      if colorEditor.NumberOfLineVertices > 2 then begin
        MTLSetShader(shaderLines2D);
        if colorEditor.HasNewLines then begin
           colorEditorVertexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@colorEditor.LineVertices[0], colorEditor.NumberOfLineVertices * sizeof(TVertex2D), MTLResourceStorageModeShared);
           colorEditor.HasNewLines := false;
        end;
        MTLSetVertexBuffer(colorEditorVertexBuffer, 0, 0);
        //MTLSetVertexBytes(@colorEditor.LineVertices[0], colorEditor.NumberOfLineVertices * sizeof(TVertex2D), 0); //limited to 4096 bytes!
        MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
        MTLDraw(MTLPrimitiveTypeTriangle, 0, colorEditor.NumberOfLineVertices);
      end;
    end;
    txt.DrawText();
    if clrbar <> nil then
     clrbar.Draw();
  MTLEndFrame;
  //GLForm1.caption := inttostr(slices2D.NumberOfLineVertices) +' '+inttostr(random(888));
end;

procedure VertVertexColor(var v: TVertVertex; r, g, b: TScalar);
begin
     v.color := V4(r,g,b, 1);
end;


procedure ColorNegative(var v: TVertVertex);
begin
     v.color.r := 1.0-v.color.r;
     v.color.g := 1.0-v.color.g;
     v.color.b := 1.0-v.color.b;
end;

{$DEFINE FX}
{$IFDEF FX}
procedure TGPUVolume.PaintMosaicRender(var vol: TNIfTI; lRender: TMosaicRender);
var
  vertUniforms: TVertUniforms;
  fragUniforms: TFragUniforms;
  projectionMatrix, modelMatrix: TMat4;
  modelLightPos, v, rayDir: TVec4;
  lElevation, lAzimuth: single;
begin
    if vertexBuffer = nil then // only once
      LoadCube();
    if (vol.VolRGBA <> nil) then
       LoadTexture(vol);
    if (volTex = nil) then
      exit;
  case lRender.Orient of
    -kSagLeftOrient, -kSagRightOrient: lAzimuth:=90;
    kSagLeftOrient, kSagRightOrient: lAzimuth:=270;
    kCoronalOrient: lAzimuth:=180;
    -kAxialOrient: lAzimuth:=180;
    else lAzimuth:=0;
  end;
  if (lRender.Orient = -kAxialOrient) then
     lElevation := -90
  else if (lRender.Orient = kAxialOrient) then
       lElevation := 90
  else
      lElevation := 0;
  //unit model matrix for lighting
  modelMatrix := TMat4.Identity;
  modelMatrix *= TMat4.Translate(0.5, 0.5, -1.0);
  modelMatrix *= TMat4.RotateX(-DegToRad(90-lElevation));
  modelMatrix *= TMat4.RotateZ(DegToRad(lAzimuth));
  modelMatrix *= TMat4.Translate(-0.5, -0.5, -0.5); //pivot around 0.5 as cube spans 0..1
  modelLightPos := (modelMatrix.Transpose * fLightPos);
  //model matrix in pixel space
  modelMatrix := TMat4.Identity;
  modelMatrix *= TMat4.Translate(lRender.Left, lRender.Bottom,0);
  modelMatrix *= TMat4.Scale(lRender.Width, lRender.Height,1);
  modelMatrix *= TMat4.Translate(0.5, 0.5, -1.0);
  modelMatrix *= TMat4.RotateX(-DegToRad(90-lElevation));
  modelMatrix *= TMat4.RotateZ(DegToRad(lAzimuth));
  modelMatrix *= TMat4.Translate(-0.5, -0.5, -0.5); //pivot around 0.5 as cube spans 0..1
  projectionMatrix := TMat4.Ortho(0, mtlControl.clientwidth, 0, mtlControl.clientheight, 0.01, 2);
  vertUniforms.modelViewProjectionMatrix := ( projectionMatrix * modelMatrix);
  rayDir.x := 0; RayDir.y := 0; rayDir.z := 1; RayDir.w := 0;
  v := rayDir;
  rayDir := (vertUniforms.modelViewProjectionMatrix.Inverse * v);
  rayDir.w := 0;
  rayDir := rayDir.Normalize;
  addFuzz(rayDir);
  fragUniforms.rayDir.X := rayDir.x; fragUniforms.rayDir.Y := rayDir.y; fragUniforms.rayDir.Z := rayDir.z;
  fragUniforms.lightPos := modelLightPos;
  fragUniforms.numOverlays:= overlayNum;
  if vol.IsLabels then
     fragUniforms.backAlpha :=  1
  else
      fragUniforms.backAlpha:= vol.OpacityPercent/100;
  fragUniforms.clipPlane := fClipPlane;
  fragUniforms.sliceSize := 1/maxDim;
  //fragUniforms.stepSize := 1.0/ ((maxDim*0.25)+ (maxDim*1.75)* (RayCastQuality1to10/10));
  fragUniforms.stepSize := ComputeStepSize (RayCastQuality1to10, maxDim);
  //MTLBeginFrame();
    MTLSetShader(shader3D);
    MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
    MTLSetFragmentTexture(volTex, 0);
    MTLSetFragmentTexture(gradTex, 1);
    MTLSetFragmentTexture(overlayVolTex, 2);
    MTLSetFragmentTexture(overlayGradTex, 3);
    MTLSetVertexBuffer(vertexBuffer, 0, 0);
    MTLSetFragmentBytes(@fragUniforms, sizeof(fragUniforms), 1);
    if ShaderPrefs.nUniform > 1 then
       MTLSetFragmentBytes(@prefValues[1], ShaderPrefs.nUniform *sizeof(single), 2); //ShaderPrefs.nUniform
    MTLSetCullMode(MTLCullModeFront);
    MTLDrawIndexed (MTLPrimitiveTypeTriangleStrip, 14, MTLIndexTypeUInt16, indexBuffer, 0);
    MTLSetCullMode(MTLCullModeNone);
  //MTLEndFrame;
end;

{$ELSE}
procedure TGPUVolume.PaintMosaicRender(var vol: TNIfTI; lRender: TMosaicRender);
var
 quad :  array[0..3] of TVertVertex;
 vertUniforms: TVertUniforms;
  fragUniforms: TFragUniforms;
  projectionMatrix, modelMatrix: TMat4;
  modelLightPos, rayDir: TVec4;
  w, h: single;
  r: TMosaicRender;
begin
  r := lRender;
  quad[0] := VertVertex(r.Left,r.Bottom,0);
  quad[1] := VertVertex(r.Left+r.Width,r.Bottom,0);
  quad[2] := VertVertex(r.Left,r.Bottom+r.Height,0);
  quad[3] := VertVertex(r.Left+r.Width,r.Bottom+r.Height,0);
  modelLightPos := (fLightPos); //correct for axial
  //colors determine orientation
     //GLForm1.Caption := '<>'+inttostr(r.Orient);
  if r.Orient = -kCoronalOrient then begin
      VertVertexColor(quad[0], 0, 0, 0);
      VertVertexColor(quad[1], 1, 0, 0);
      VertVertexColor(quad[2], 0, 0, 1);
      VertVertexColor(quad[3], 1, 0, 1);
      rayDir := Vec4(0,1,0, 0);
      modelLightPos := Vec4(fLightPos.X,-fLightPos.Z,fLightPos.Y, 0.0); //X=L,Y=N/F ,Z=UpDown
  end else if (r.Orient = -kAxialOrient) then begin
    VertVertexColor(quad[0], 1, 0, 0);
    VertVertexColor(quad[1], 0, 0, 0);
    VertVertexColor(quad[2], 1, 1, 0);
    VertVertexColor(quad[3], 0, 1, 0);
    rayDir := Vec4(0,0,1, 0);
    modelLightPos := Vec4(-fLightPos.X,fLightPos.Y,-fLightPos.Z, 0.0);
  end else if (r.Orient = -kSagRightOrient) or (r.Orient = -kSagLeftOrient) then begin //sagittal
    VertVertexColor(quad[0], 0, 1, 0);
    VertVertexColor(quad[1], 0, 0, 0);
    VertVertexColor(quad[2], 0, 1, 1);
    VertVertexColor(quad[3], 0, 0, 1);
    rayDir := Vec4(1,0,0, 0);
    modelLightPos := Vec4(-fLightPos.Z,-fLightPos.X,  fLightPos.Y, 0.0);
  end else if r.Orient = kAxialOrient then begin
    VertVertexColor(quad[0], 0, 0, 1);
    VertVertexColor(quad[1], 1, 0, 1);
    VertVertexColor(quad[2], 0, 1, 1);
    VertVertexColor(quad[3], 1, 1, 1);
    rayDir := Vec4(0,0,-1, 0);
    modelLightPos := Vec4(fLightPos.X,fLightPos.Y,fLightPos.Z, 0.0);
  end else if r.Orient = kCoronalOrient then begin
      VertVertexColor(quad[0], 1, 1, 0);
      VertVertexColor(quad[1], 0, 1, 0);
      VertVertexColor(quad[2], 1, 1, 1);
      VertVertexColor(quad[3], 0, 1, 1);
      rayDir := Vec4(0,-1,0, 0);
      modelLightPos := Vec4(-fLightPos.X,fLightPos.Z,fLightPos.Y, 0.0); //X=L,Y=N/F ,Z=UpDown
  end else begin //sagittal
        VertVertexColor(quad[0], 1, 0, 0);
        VertVertexColor(quad[1], 1, 1, 0);
        VertVertexColor(quad[2], 1, 0, 1);
        VertVertexColor(quad[3], 1, 1, 1);
        rayDir := Vec4(-1,0,0, 0);
        modelLightPos := Vec4(fLightPos.Z,fLightPos.X,  fLightPos.Y, 0.0);
  end;
  w := mtlControl.clientwidth;
  h := mtlControl.clientheight;
  if (vol.VolRGBA <> nil) then
     LoadTexture(vol);
  if (volTex = nil) then
    exit;
  modelMatrix := TMat4.Identity;
  modelMatrix *= TMat4.Translate(0, 0, -fDistance);
  projectionMatrix := TMat4.OrthoMetal (0, w, 0, h, fDistance-1, fDistance+1);
  vertUniforms.modelViewProjectionMatrix := ( projectionMatrix * modelMatrix);
  rayDir := rayDir.Normalize;
  addFuzz(rayDir);
  fragUniforms.rayDir.X := rayDir.x; fragUniforms.rayDir.Y := rayDir.y; fragUniforms.rayDir.Z := rayDir.z;
  fragUniforms.lightPos := modelLightPos;
  fragUniforms.numOverlays:= overlayNum;
  if vol.IsLabels then
     fragUniforms.backAlpha :=  1
  else
      fragUniforms.backAlpha:= vol.OpacityPercent/100;

  fragUniforms.clipPlane := fClipPlane;
  fragUniforms.sliceSize := 1/maxDim;
  //fragUniforms.stepSize := 1.0/ ((maxDim*0.25)+ (maxDim*1.75)* (RayCastQuality1to10/10));
  fragUniforms.stepSize := ComputeStepSize (RayCastQuality1to10, maxDim);
  //MTLBeginFrame();
    MTLSetShader(shader3D);
    MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
    MTLSetFragmentTexture(volTex, 0);
    MTLSetFragmentTexture(gradTex, 1);
    MTLSetFragmentTexture(overlayVolTex, 2);
    MTLSetFragmentTexture(overlayGradTex, 3);
    MTLSetVertexBytes(@quad, sizeof(quad), 0);
    MTLSetFragmentBytes(@fragUniforms, sizeof(fragUniforms), 1);
    if ShaderPrefs.nUniform > 1 then
       MTLSetFragmentBytes(@prefValues[1], ShaderPrefs.nUniform*sizeof(single), 2);
    MTLSetCullMode(MTLCullModeFront);
    MTLDraw(MTLPrimitiveTypeTriangleStrip, 0, 4);
    MTLSetCullMode(MTLCullModeNone);
  //MTLEndFrame;
end; // PaintMosaicRender()
{$ENDIF}

procedure TGPUVolume.PaintMosaic2D(var vol: TNIfTI; Drawing: TDraw; MosaicString: string);
  var
   vertUniforms: TVertUniforms2D;
   fragUniforms: TFragUniforms2D;
   w,h,f: single;
   i: integer;
  begin
    if vertexBuffer = nil then // only once
      LoadCube();
    if (vol.VolRGBA <> nil) then
       LoadTexture(vol);
    if (volTex = nil) then
      exit;
    w := mtlControl.clientwidth;
    h := mtlControl.clientheight;
    //next two lines only difference between Paint2D and PainMosaic2D
    //slices2D.Update(vol.Scale, w, h, DisplayOrient);
    if clrbar <> nil then begin
         f := clrbar.PanelFraction;
         //GLForm1.Caption := format('%d %.2g',[round(h), f]);
         if (f > 0.0) and (f < 0.5) then begin
            if (clrbar.isVertical) then
               w := w - (w * f)
            else
               h := h - (h * f);
         end;
    end;
    slices2D.UpdateMosaic(MosaicString, vol.Mat, vol.InvMat, vol.Dim, vol.Scale, w,h);
    w := mtlControl.clientwidth;
    h := mtlControl.clientheight;
    if (slices2D.NumberOfVertices < 3) and (slices2D.NumberOfMosaicRenders < 1) then exit;
    vertUniforms.viewportSize := V2(w, h);
    if vol.IsLabels then
       fragUniforms.backAlpha :=  1
    else
        fragUniforms.backAlpha:= vol.OpacityPercent/100;
    MTLBeginFrame();
      if (slices2D.NumberOfVertices >= 3) then begin
        //draw 2D slices
        UpdateDraw(Drawing);
        if (not Drawing.IsOpen) or (Drawing.OpacityFraction <= 0.0) then
           fragUniforms.drawAlpha := 0.0
        else
            fragUniforms.drawAlpha := Drawing.OpacityFraction; //<- loaded!
        MTLSetShader(shader2D);
        MTLSetFragmentTexture(volTex, 0);
        MTLSetFragmentTexture(overlayVolTex, 1);
        MTLSetFragmentTexture(drawVolTex, 2);
        MTLSetFragmentTexture(drawVolLut, 3);
        MTLSetVertexBytes(@fragUniforms, sizeof(fragUniforms), 2);
        if slices2D.HasNewSlices then begin
           sliceVertexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@slices2D.SliceVertices[0], slices2D.NumberOfVertices * sizeof(TVertex2D), MTLResourceStorageModeShared);
           slices2D.HasNewSlices := false;
        end;
        MTLSetVertexBuffer(sliceVertexBuffer, 0, 0);
        MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
        MTLSetFragmentBytes(@fragUniforms, sizeof(fragUniforms), 2);
        MTLDraw(MTLPrimitiveTypeTriangle, 0, slices2D.NumberOfVertices);
      end;
      //draw render
      if slices2D.NumberOfMosaicRenders > 0 then
         for i := 0 to (slices2D.NumberOfMosaicRenders -1) do
             PaintMosaicRender(vol, slices2D.MosaicRenders[i]);
      //draw lines
      if slices2D.NumberOfLineVertices > 0 then begin
        //MTLSetCullMode(MTLCullModeNone);
        MTLSetShader(shaderLines2D);
        if slices2D.HasNewLines then begin
           lineVertexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@slices2D.LineVertices[0], slices2D.NumberOfLineVertices * sizeof(TVertex2D), MTLResourceStorageModeShared);
           slices2D.HasNewLines := false;
        end;
        MTLSetVertexBuffer(lineVertexBuffer, 0, 0);
        //MTLSetVertexBytes(@slices2D.LineVertices[0], slices2D.NumberOfLineVertices * sizeof(TVertex2D), 0); //limited to 4096 bytes!
        MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
        MTLDraw(MTLPrimitiveTypeTriangle, 0, slices2D.NumberOfLineVertices);
      end;
      if clrbar <> nil then
       clrbar.Draw();
      //we do not know the background color - might be nice to have black text if background is white!
      txt.DrawText();
    MTLEndFrame;
  end;
{$ENDIF}

procedure TGPUVolume.Paint(var vol: TNIfTI);
var
 {$IFDEF VIEW2D}
 vertUniforms2D: TVertUniforms2D;
 {$ENDIF}
  vertUniforms: TVertUniforms;
  fragUniforms: TFragUniforms;
  projectionMatrix, modelMatrix: TMat4;
  modelLightPos, v, rayDir: TVec4;
  whratio, scale, w, h: single;
begin
  w := mtlControl.clientwidth;
  h := mtlControl.clientheight;
  if vertexBuffer = nil then // only once
    LoadCube();
  if (vol.VolRGBA <> nil) then
     LoadTexture(vol);
  if (volTex = nil) then
    exit;
  modelMatrix := TMat4.Identity;
  modelMatrix *= TMat4.Translate(0, 0, -fDistance);
  modelMatrix *= TMat4.RotateX(-DegToRad(90-fElevation));
  modelMatrix *= TMat4.RotateZ(DegToRad(fAzimuth));
  modelMatrix *= TMat4.Translate(-vol.Scale.X/2, -vol.Scale.Y/2, -vol.Scale.Z/2);
  modelLightPos := (modelMatrix.Transpose * fLightPos);
  modelMatrix *= TMat4.Scale(vol.Scale.X, vol.Scale.Y, vol.Scale.Z); //for volumes that are rectangular not square
  if fDistance = 0 then
     scale := 1
  else
      scale := 0.5 * 1/abs(kDefaultDistance/(fDistance+1.0));
  whratio := w/h;
  if (whratio > 1) or (whratio = 0) then //Wide window
     projectionMatrix := TMat4.Ortho(-scale * whratio, scale * whratio, -scale, scale, fDistance-1, fDistance+1)
  else
      projectionMatrix := TMat4.Ortho(-scale, scale, -scale/whratio, scale/whratio, fDistance-1, fDistance+1);
  vertUniforms.modelViewProjectionMatrix := ( projectionMatrix * modelMatrix);
  rayDir.x := 0; RayDir.y := 0; rayDir.z := 1; RayDir.w := 0;
  v := rayDir;
  rayDir := (vertUniforms.modelViewProjectionMatrix.Inverse * v);
  rayDir.w := 0;
  rayDir := rayDir.Normalize;
  //GLForm1.Caption := format('>> %g %g %g',[rayDir.X, rayDir.y, rayDir.z]);
  addFuzz(rayDir);
  fragUniforms.rayDir.X := rayDir.x; fragUniforms.rayDir.Y := rayDir.y; fragUniforms.rayDir.Z := rayDir.z;
  fragUniforms.lightPos := modelLightPos;
  fragUniforms.clipPlane := fClipPlane;
  fragUniforms.numOverlays:= overlayNum;
  if vol.IsLabels then
     fragUniforms.backAlpha :=  1
  else
      fragUniforms.backAlpha:= vol.OpacityPercent/100;
  //GLForm1.Caption := format('>> %g', [fragUniforms.backAlpha]);
  fragUniforms.sliceSize := 1/maxDim;
  //fragUniforms.stepSize := 1.0/ ((maxDim*0.25)+ (maxDim*1.75)* (RayCastQuality1to10/10));
  fragUniforms.stepSize := ComputeStepSize (RayCastQuality1to10, maxDim);
  MTLBeginFrame();
    MTLSetShader(shader3D);
    MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
    MTLSetFragmentTexture(volTex, 0);
    MTLSetFragmentTexture(gradTex, 1);
    MTLSetFragmentTexture(overlayVolTex, 2);
    MTLSetFragmentTexture(overlayGradTex, 3);
    MTLSetVertexBuffer(vertexBuffer, 0, 0);
    MTLSetFragmentBytes(@fragUniforms, sizeof(fragUniforms), 1);
    if ShaderPrefs.nUniform > 1 then
       MTLSetFragmentBytes(@prefValues[1], ShaderPrefs.nUniform  *sizeof(single), 2); //n.b. kMaxUniform NOT ShaderPrefs.nUniform
    MTLSetCullMode(MTLCullModeFront);
    MTLDrawIndexed (MTLPrimitiveTypeTriangleStrip, 14, MTLIndexTypeUInt16, indexBuffer, 0);
    MTLSetCullMode(MTLCullModeNone);
    {$IFDEF VIEW2D}
    if colorEditorVisible then begin //n.b. depth testing causes issues...
      //MTLSetCullMode(MTLCullModeNone); //added to end of rendering
      vertUniforms2D.viewportSize := V2(w, h);
      colorEditor.Update(w, h, vol);
      if colorEditor.NumberOfLineVertices > 2 then begin
        MTLSetShader(shaderLines2D);
        if colorEditor.HasNewLines then begin
           colorEditorVertexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@colorEditor.LineVertices[0], colorEditor.NumberOfLineVertices * sizeof(TVertex2D), MTLResourceStorageModeShared);
           colorEditor.HasNewLines := false;
        end;
        MTLSetVertexBuffer(colorEditorVertexBuffer, 0, 0);
        //MTLSetVertexBytes(@colorEditor.LineVertices[0], colorEditor.NumberOfLineVertices * sizeof(TVertex2D), 0); //limited to 4096 bytes!
        MTLSetVertexBytes(@vertUniforms2D, sizeof(vertUniforms2D), 1);
        MTLDraw(MTLPrimitiveTypeTriangle, 0, colorEditor.NumberOfLineVertices);
      end;
    end;
    {$ENDIF}
    //draw render
    if clrbar <> nil then
       clrbar.Draw();
    {$IFDEF CUBE}
    if Slices.LabelOrient then begin
       gCube.Azimuth:=fAzimuth;
       gCube.Elevation:=-fElevation;
       gCube.Draw(mtlControl.clientwidth, mtlControl.clientheight);
    end;
    {$ENDIF}
  MTLEndFrame;
end;

end.

