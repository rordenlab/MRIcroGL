unit mtlvolume2;
{$mode objfpc}
{$modeswitch objectivec1}

{$H+}
interface

{$DEFINE GPUGRADIENTS} //Computing volume gradients on the GPU is much faster than using the CPU
{$DEFINE VIEW2D}
{$DEFINE CUBE}
{$DEFINE MATCAP}
{$DEFINE CLRBAR}
{$DEFINE OLDDEPTHPICKER}
{$DEFINE TIMER} //reports GPU gradient time to stdout (Unix only)
{$include opts.inc} //for  DEFINE DEPTHPICKER
uses
    CocoaAll, MacOSAll,
    //CFBase, CGImage, CGDataProvider, CGColorSpace, MetalKit, CocoaAll,
    {$IFDEF MATCAP} intfgraphics, graphtype, Graphics,  {$ENDIF}
    {$IFDEF CUBE} Forms, mtlcube, {$ENDIF}
    {$IFDEF CLRBAR} mtlclrbar, {$ENDIF}
    {$IFDEF TIMER} DateUtils,{$ENDIF}
    mtlfont,  VectorMath, MetalPipeline, MetalControl, Metal, MetalUtils,
    SysUtils, Math, nifti, niftis, SimdUtils, Classes
    {$IFDEF VIEW2D}, drawvolume, slices2D, colorEditor{$ENDIF};
const
 kDefaultDistance = 2.25;
 kMaxDistance = 40;
 kQualityMedium = 3;
 kQualityBest = 5;
type
  TGPUVolume = class
      private
        {$IFDEF VIEW2D}
        colorEditorVertexBuffer, sliceVertexBuffer, renderVertexBuffer, lineVertexBuffer: MTLBufferProtocol;
        {$IFDEF GPUGRADIENTS}blurShader, sobelShader,{$ENDIF}
        shader2D, shader2Dn, shaderLines2D: TMetalPipeline;
        slices2D: TSlices2D;
        colorEditor: TColorEditor;
        isSmooth2D, colorEditorVisible: boolean;
        txt: TGPUFont;
        drawVolTex, drawVolLut: MTLTextureProtocol;
        {$ENDIF}
        RayCastQuality1to5, maxDim, fAzimuth, fElevation, fPitch: integer;
        shaderPrefs: TShaderPrefs;
        prefValues: array [1..kMaxUniform] of single;
        fDistance, overlayNum: single;
        fLightPos, fClipPlane: TVec4;
        indexBuffer, vertexBuffer: MTLBufferProtocol;
        mvp: TMat4;
        viewportXYWH: TVec4;
        {$IFDEF LINE3D}
        line3DBuffer: MTLBufferProtocol;
        {$ENDIF}
        {$IFDEF LINE3D}shaderLine3D, shaderLine3Dalpha, {$ENDIF}{$IFDEF OLDDEPTHPICKER}shaderDepth,{$ENDIF} shader3D, shader3Dbetter : TMetalPipeline;
        volTex, gradTex, overlayVolTex, overlayGradTex: MTLTextureProtocol;
        {$IFDEF MATCAP} matCapTex: MTLTextureProtocol;{$ENDIF}
        mtlControl: TMetalControl;
        {$IFDEF CLRBAR}clrbar: TGPUClrbar; {$ENDIF}
        {$IFDEF CUBE} gCube :TGPUCube; {$ENDIF}
        procedure LoadCube();
        procedure LoadTexture(var vol: TNIfTI; deferGradients: boolean);
        procedure CreateDrawColorTable;//1D texture for drawing
        procedure CreateDrawTex(Dim: TVec3i; Vals: TUInt8s);
        procedure UpdateDraw(Drawing: TDraw);
        procedure CreateOverlayTextures(Dim: TVec3i; volRGBA: TRGBAs);
        procedure CreateGradientVolumeGPU(Xsz,Ysz,Zsz: integer; var inTex, grTex: MTLTextureProtocol);
      public
        ClipThick: single;
        renderBitmapWidth: integer;
        matcapLoc: integer;
        UseDepthShader: boolean;
        {$IFDEF VIEW2D}
        SelectionRect: TVec4;
        property ShowColorEditor: boolean read colorEditorVisible write colorEditorVisible;
        property ShowSmooth2D: boolean read isSmooth2D write isSmooth2D;
        //function ColorEditorMouseDown(mouseX, mouseY: integer): boolean;
        function Slice2Dmm(var vol: TNIfTI; out vox: TVec3i): TVec3;
        //function FracVox: TVec3i;
        property CE: TColorEditor read colorEditor;
        property Slices: Tslices2D read slices2D;
        procedure SetSlice2DFrac(frac : TVec3);
        function GetSlice2DFrac(mouseX, mouseY: integer; out Orient: integer): TVec3;
        function Unproject(mouseX, mouseY, depth: single): TVec3;
        function GetSlice2DMaxXY(mouseX, mouseY: integer; var Lo: TPoint): TPoint;
        procedure Paint2D(var vol: TNIfTI; Drawing: TDraw; DisplayOrient: integer);
        procedure PaintMosaicRender(var vol: TNIfTI; lRender: TMosaicRender);
        procedure PaintMosaic2D(var vol: TNIfTI; Drawing: TDraw; MosaicString: string);
        {$ENDIF}
        procedure UpdateOverlays(vols: TNIfTIs);
        property Quality1to5: integer read RayCastQuality1to5 write RayCastQuality1to5;
        property ShaderSliders: TShaderPrefs read shaderPrefs write shaderPrefs;
        property Azimuth: integer read fAzimuth write fAzimuth;
        property Elevation: integer read fElevation write fElevation;
        property Pitch: integer read fPitch write fPitch;
        property Distance: single read fDistance write fDistance;
        property LightPosition: TVec4 read fLightPos write fLightPos;
        property ClipPlane: TVec4 read fClipPlane write fClipPlane;
        procedure Prepare(shaderName: string);
        constructor Create(fromView: TMetalControl);
        procedure PaintCrosshair3D(var vol: TNIfTI; rgba: TVec4);
        procedure PaintCore(var vol: TNIfTI; widthHeightLeft: TVec3i; clearScreen: boolean = true; isDepthShader: boolean = false);
        procedure Paint(var vol: TNIfTI);
        procedure PaintDepth(var vol: TNIfTI; isAxCorSagOrient4: boolean = false);

        procedure SetShader(shaderName: string; isUpdatePrefs: boolean = true);
        procedure SetShaderSlider(idx: integer; newVal: single);
        procedure SaveBmp(filename: string; hasAlpha: boolean);
        function ReadPixel(x, y: integer): UInt32;
        function ReadDepth(x,y: integer): single;
        {$IFDEF CLRBAR} procedure SetColorBar(fromColorbar: TGPUClrbar); {$ENDIF}
        procedure  SetTextContrast(clearclr: TRGBA);
        {$IFDEF MATCAP} function SetMatCap(fnm: string): boolean; {$ENDIF}
        destructor Destroy; override;
  end;

implementation

//uses mainunit;
function gluUnProject(winXYZ: TVec3; mvp: TMat4; viewportXYWH: TVec4): TVec3;
//viewport[0]=x, viewport[1]=y, viewport[2]=width, viewport[3]=height
//return coordinates in object space
(*vec4 v = vec4(2.0*(gl_FragCoord.x-view.x)/view.z-1.0,
              2.0*(gl_FragCoord.y-view.y)/view.w-1.0,
              2.0*texture2DRect(DepthTex,gl_FragCoord.xy).z-1.0,
              1.0 );
v = gl_ModelViewProjectionMatrixInverse * v;
v /= v.w;*)
//https://community.khronos.org/t/converting-gl-fragcoord-to-model-space/57397
//https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluUnProject.xml
//http://nehe.gamedev.net/article/using_gluunproject/16013/
var
	v: TVec4;
    mvpInv: TMat4;
begin
	v := vec4(2.0*(winXYZ.x-viewportXYWH.x)/viewportXYWH.z-1.0,
    	2.0*(winXYZ.y-viewportXYWH.y)/viewportXYWH.w-1.0,
        2.0*winXYZ.z-1.0,
        1.0);
    mvpInv := mvp.inverse;
    v := mvpInv * v;
    v := v / v.w;
    result := vec3(v.x, v.y, v.z);
end;

function TGPUVolume.Unproject(mouseX, mouseY, depth: single): TVec3;
var
	winXYZ: TVec3;
begin
    winXYZ := vec3(mouseX, mouseY, depth);
    result := gluUnProject(winXYZ, mvp, viewportXYWH);
    //printf(format('windowXYZ %g %g %g', [winXYZ.x, winXYZ.y, winXYZ.z]));
    //printf(format('objXYZ %g %g %g', [result.x, result.y, result.z]));
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
    stepSize, sliceSize, numOverlays, clipThick: TScalar;
    backAlpha, pad1, pad2, pad3: TScalar;
    rayDir: TVec4;
    lightPos: TVec4;
    clipPlane: TVec4;
    normMatrix, modelViewProjectionMatrix: TMat4;
  end;

{$IFDEF LINE3D}
var
 gLines3Dv: array of TVertVertex = nil;
 gLines3DnIdx: integer = 0;
 gLines3DIndexBuffer: MTLBufferProtocol = nil;
{$ENDIF}

destructor TGPUVolume.Destroy;
begin
  {$IFDEF VIEW2D}
  slices2D.free;
  colorEditor.free;
  txt.free;
  {$ENDIF}
  {$IFDEF LINE3D}
  gLines3Dv := nil;
  gLines3DIndexBuffer := nil;
  {$ENDIF}
  {$IFDEF CUBE} gCube.free; {$ENDIF}
  {$IFDEF CLRBAR} clrbar.free; {$ENDIF}
  inherited;
end;

{$IFDEF MATCAP}
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

procedure CreateBmp(var px: TPicture);
Type
  TRGBquad = PACKED RECORD
     rgbBlue,rgbGreen,rgbRed,rgbAlpha: byte;
    end;
  TQuadRA = array [1..1] of TRGBQuad;
  RGBQuadp = ^TQuadRA;
function rgb2quad(R,G,B: Byte): TRGBquad;
begin
  result.rgbRed:= (R);
  result.rgbGreen:= (G);
  result.rgbBlue:= (B);
  result.rgbAlpha:= 0;
end;
const
  x = 256;
  y = 256;
var
  i, j, k: integer;
  lBuff: RGBQuadp;
  Ptr: pointer;
  AImage: TLazIntfImage;
  lRawImage: TRawImage;
begin
  GetMem(lBuff, x*y* sizeof(TRGBQuad));
  k := 1;
  for j := 1 to y do
      for i :=  1 to x do begin
          //lBuff^[k] := rgb2quad(256-i,i-1,256-j);
          lBuff^[k] := rgb2quad(256-j,256-j,256-j);
          k := k + 1;
      end;
  lRawImage.Init;
  //lRawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(0,0);
  lRawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(x,y);
  lRawImage.CreateData(true);
  AImage := TLazIntfImage.Create(x,y);
  AImage.SetRawImage(lRawImage);
  AImage.BeginUpdate;
  i := 1;
  for j := 0 to (y-1) do begin
    ptr := AImage.GetDataLineStart(j);
    Move(lBuff^[i], Ptr^, x * sizeof(TRGBQuad));
    inc(i, x);
  end;
  AImage.EndUpdate;
  px.Bitmap.LoadFromIntfImage(AImage);
  FreeMem(lBuff);
end;


function TGPUVolume.SetMatCap(fnm: string): boolean;
var
  px: TPicture;
  bmpHt, bmpWid: integer;
  isPng : boolean;
  pngTexDesc: MTLTextureDescriptor;
  pngRegion: MTLRegion;
  AImage: TLazIntfImage;
  lRawImage: TRawImage;
  //ifnm, MatCapDir: string;
begin
  result := false;
  if (not fileexists(fnm)) and (fnm <> '') then begin
       //MatCapDir := ExtractFilePath(ShaderDir)+ 'matcap';
       fnm := ExtractFilePath(ShaderDir)+ 'matcap'+pathdelim+fnm+'.jpg';
  end;
  //if (fnm <> '') and (not fileexists(fnm)) then begin
  px := TPicture.Create;
  if not fileexists(fnm) then begin
     if fnm <> '' then
        writeln('Unable to find MatCap "'+fnm+'"');
     CreateBmp(px)
  end else begin
    isPng := upcase(ExtractFileExt(fnm)) = '.PNG';
    try
      if isPng then
          px.LoadFromFile(fnm)
      else begin
        lRawImage.Init;
        //lRawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(0,0);
        lRawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(0,0);
        lRawImage.CreateData(false);
        AImage := TLazIntfImage.Create(0,0);
        try
          AImage.SetRawImage(lRawImage);
          AImage.LoadFromFile(fnm);
          px.Bitmap.LoadFromIntfImage(AImage);
        finally
          AImage.Free;
        end;
      end;
    except
      px.Bitmap.Width:=0;
    end;
  end;
  if (px.Bitmap.PixelFormat <> pf32bit ) or (px.Bitmap.Width < 1) or (px.Bitmap.Height < 1) then begin
     writeln('Error loading 32-bit MatCap '+fnm);
     exit;
  end;
  FlipVertical(px);
  bmpHt := px.Bitmap.Height;
  bmpWid := px.Bitmap.Width;
  if px.Bitmap.PixelFormat <> pf32bit then
     exit; //distance stored in ALPHA field
  pngTexDesc := MTLTextureDescriptor.alloc.init.autorelease;
  pngTexDesc.setTextureType(MTLTextureType2D);
  if isPng then
     pngTexDesc.setPixelFormat(MTLPixelFormatBGRA8Unorm)
  else
      pngTexDesc.setPixelFormat(MTLPixelFormatRGBA8Unorm);
  //pngTexDesc.setPixelFormat(MTLPixelFormatBGRA8Unorm);
  pngTexDesc.setWidth(bmpWid);
  pngTexDesc.setHeight(bmpHt);
  pngTexDesc.setDepth(1);
  if (matCapTex <> nil) then matCapTex.release;
  matCapTex := mtlControl.renderView.device.newTextureWithDescriptor(pngTexDesc);
  Fatal(matCapTex = nil, format('mtlfont: newTextureWithDescriptor failed %dx%d', [bmpHt, bmpWid]));
  pngRegion := MTLRegionMake3D(0, 0, 0, bmpWid, bmpHt, 1);
  matCapTex.replaceRegion_mipmapLevel_withBytes_bytesPerRow(pngRegion, 0, PInteger(px.Bitmap.RawImage.Data), bmpWid*4);
  px.Free;
  result := true;
end;
{$ENDIF}

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

function TGPUVolume.ReadPixel(x, y: integer): UInt32;
begin
	result := MTLReadPixel(x, y);
end;

function TGPUVolume.ReadDepth(x,y: integer): single;
begin
	result := MTLReadDepth(x, y);
end;

procedure TGPUVolume.SaveBmp(filename: string; hasAlpha: boolean);
begin
  if filename = '' then
     MTLWriteTextureToClipboard(hasAlpha)
  else
      MTLWriteTextureToFile(pChar(filename), hasAlpha);
end;

procedure TGPUVolume.SetShader(shaderName: string; isUpdatePrefs: boolean = true);
var
 options: TMetalPipelineOptions;
 i: integer;
 //str: NSString;
 //Attrib: NSDictionary;
 //defaultLibrary: MTLLibraryProtocol;
 libraryOptions : TMetalLibraryOptions;
begin
 if not isUpdatePrefs then exit; //only for opengl switching between "Better"
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
 options.fragmentShader:= 'fragmentShader';
 shader3D := MTLCreatePipeline(options);
 libraryOptions := TMetalLibraryOptions.Default;
 libraryOptions.name := shaderName;
 libraryOptions.preprocessorMacros := NSDictionary.dictionaryWithObject_forKey(NSSTR('CUBIC'), NSSTR('CUBIC'));
 options.shaderLibrary := MTLCreateLibrary(libraryOptions);
 shader3Dbetter := MTLCreatePipeline(options);
 //if not isUpdatePrefs then exit;
 shaderPrefs := loadShaderPrefs(shaderName);
 matcapLoc := -1;
 if (shaderPrefs.nUniform > 0) and (shaderPrefs.nUniform <= kMaxUniform) then
    for i := 1 to shaderPrefs.nUniform do begin
            if (shaderPrefs.Uniform[i].Widget = 3) then begin
               matcapLoc := 1;
               shaderPrefs.nUniform := shaderPrefs.nUniform - 1;
               break;
            end;
            prefValues[i] := shaderPrefs.Uniform[i].DefaultV;
    end;
 MTLSetDepthStencil(shader3D, MTLCompareFunctionLess, true);//TQ
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
 options := TMetalPipelineOptions.Default;
 //default shader
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
 //nearest neighbor shader
 shaderName := ShaderDir+pathdelim+'_Texture2Dn.metal';
 options.libraryName := shaderName;
 if not fileexists(shaderName) then
  writeln('Unable to find ' + shaderName);
 shader2Dn := MTLCreatePipeline(options);
 //line shades
 shaderName := ShaderDir+pathdelim+'_Line2D.metal';
 options.libraryName := shaderName;
 if not fileexists(shaderName) then
  writeln('Unable to find ' + shaderName);
 shaderLines2D  := MTLCreatePipeline(options);
 MTLSetDepthStencil(shaderLines2D, MTLCompareFunctionLess, false);
 //depth picker
 {$IFDEF OLDDEPTHPICKER}
 UseDepthShader := false;
 shaderName := ShaderDir+pathdelim+'_Depth3D.metal';
 options.libraryName := shaderName;
 if not fileexists(shaderName) then
  writeln('Unable to find ' + shaderName);
 shaderDepth  := MTLCreatePipeline(options);
 {$ENDIF}
 options := TMetalPipelineOptions.Default;
 shaderName := ShaderDir+pathdelim+'_Blur3d.metal';
 {$IFDEF GPUGRADIENTS}
 options.libraryName := shaderName;
 if not fileexists(shaderName) then
    writeln('Unable to find ' + shaderName);
 options.kernelFunction := 'sobelKernel';  //blur kernel
 sobelShader := MTLCreatePipeline(options);
 options.kernelFunction := 'blurKernel';  //blur kernel
 blurShader := MTLCreatePipeline(options);
 {$ENDIF}

 //Create(fnm : string; out success: boolean; var fromView: TMetalControl);
 colorEditor := TColorEditor.Create;
 //colorEditorVisible := false;
 Txt := TGPUFont.Create(ResourcePath('Roboto', 'png'),  success, mtlControl); //<-multi-channel channel fonts glmtext
 slices2D := TSlices2D.Create(Txt);
 {$IFDEF CUBE}
 gCube := TGPUCube.Create(mtlControl);
 if (Screen.PixelsPerInch < 100) then
    gCube.Size:= gCube.Size * 1.5; //Check Darwin Retina vs Windows HiDPI
 {$ENDIF}
 CreateDrawTex(pti(4,4,4), nil);
 CreateDrawColorTable;
 {$ENDIF}
 {$IFDEF MATCAP}
 if matCapTex = nil then
    SetMatCap('');
 {$ENDIF}
 {$IFDEF LINE3D}
 options := TMetalPipelineOptions.Default;
 shaderName := ShaderDir+pathdelim+'_Line3D.metal';
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
 shaderLine3D  := MTLCreatePipeline(options);
 MTLSetDepthStencil(shaderLine3D, MTLCompareFunctionLess, true);//TQ
 //alpha shader does not use
 shaderName := ShaderDir+pathdelim+'_Line3Dalpha.metal';
 options.libraryName := shaderName;
 if not fileexists(shaderName) then
  writeln('Unable to find ' + shaderName);

 shaderLine3Dalpha  := MTLCreatePipeline(options);
 MTLSetDepthStencil(shaderLine3Dalpha, MTLCompareFunctionGreaterEqual, true);//TQ
 line3DBuffer := nil;
 {$ENDIF} //LINE3D
end;

{$IFDEF CLRBAR}
procedure TGPUVolume.SetColorBar(fromColorbar: TGPUClrbar);
begin
     clrbar := fromColorbar;
end;
{$ENDIF}

function TGPUVolume.Slice2Dmm(var vol: TNIfTI; out vox: TVec3i): TVec3;
begin
  if (slices2D = nil) then exit;
  result := slices2D.FracMM(vol.Mat, vol.Dim, vox);
end;

procedure TGPUVolume.SetSlice2DFrac(frac : TVec3);
begin

  if (slices2D = nil) or (frac.x < 0.0) or (frac.y < 0.0) or (frac.z < 0.0) then exit;
  slices2D.sliceFrac := frac;
end;

constructor TGPUVolume.Create(fromView: TMetalControl);
begin
  mtlControl := fromView;
  shader3Dbetter := nil;
  {$IFDEF CLRBAR}clrbar := nil;{$ENDIF}
  slices2D := nil;
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
  fPitch := 0;
  RaycastQuality1to5 := 5;
  SelectionRect := Vec4(-1,0,0,0);
  fLightPos := Vec4(0, 0.707, 0.707, 0);
  //fLightPos := Vec4(0, 0.087, 0.996, 0);
  fClipPlane := Vec4(0, 0, 0, -1);
  ClipThick := 1.0;
  matcapLoc := 1;
  //fLightPos := Vec4(0,0.0,0.707, 0.0);
  //fClearColor.r := 200;
  //fClearColor.g := 200;
  //fClearColor.b := 255;
  vertexBuffer := nil;
  colorEditorVisible := false;
  isSmooth2D := true;
  shaderPrefs.nUniform:= 0;
  {$IFDEF MATCAP}
  matCapTex  := nil;
  //SetMatCap(ResourceFolderPath+pathdelim+'matcap'+pathdelim+'RedPlastic.jpg');
  //SetMatCap('');
  {$ENDIF}
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
{$IFDEF GPUGRADIENTS}
var
  grTexDesc: MTLTextureDescriptor;
  threadgroupSize: MTLSize;
  threadgroupCount: MTLSize;
  tempTex: MTLTextureProtocol;
  {$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
begin
  {$IFDEF TIMER}startTime := now;{$ENDIF}
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
  tempTex.release;
  tempTex := nil;
  {$IFDEF TIMER}writeln(format('GPU Gradient time %d',[MilliSecondsBetween(Now,startTime)]));{$ENDIF}
end;
{$ELSE}
begin
     //
end;
{$ENDIF}

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
 i, vx: int64;
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

procedure TGPUVolume.LoadTexture(var vol: TNIfTI; deferGradients: boolean);
var
 volTexDesc: MTLTextureDescriptor;
 volRegion: MTLRegion;
 {$IFNDEF GPUGRADIENTS}
 //gradTexDesc: MTLTextureDescriptor;
 gradData: TRGBAs;
 gradRegion: MTLRegion;
 {$ENDIF}
begin
 {$IFDEF GPUGRADIENTS}
 //see if gradients were previously deferred but now required
 if (Vol.VolRGBA = nil) and (volTex <> nil) and(gradTex = nil) and (not deferGradients) then CreateGradientVolumeGPU(Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, volTex, gradTex);
 {$ENDIF}
 if (Vol.VolRGBA = nil) then exit;
 if gradTex <> nil then gradTex.release;
 gradTex := nil;
 maxDim := max(Vol.Dim.X,max(Vol.Dim.Y,Vol.Dim.Z));
 volTexDesc := MTLTextureDescriptor.alloc.init.autorelease;
 volTexDesc.setTextureType(MTLTextureType3D);
 volTexDesc.setPixelFormat(MTLPixelFormatRGBA8Unorm);//MTLPixelFormatRGBA8Uint
 volTexDesc.setWidth(Vol.Dim.X);
 volTexDesc.setHeight(Vol.Dim.Y);
 volTexDesc.setDepth(Vol.Dim.Z);
 //volTex := nil; //789
 if volTex <> nil then volTex.release;
 volTex := nil;
 volTex := mtlControl.renderView.device.newTextureWithDescriptor(volTexDesc);
 Fatal(volTex = nil, 'loadTex: newTextureWithDescriptor failed');
 volRegion := MTLRegionMake3D(0, 0, 0, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z);
 volTex.replaceRegion_mipmapLevel_slice_withBytes_bytesPerRow_bytesPerImage(volRegion, 0,0, @Vol.VolRGBA[0], Vol.Dim.X*4, Vol.Dim.X*Vol.Dim.Y*4);
 //compute and load gradients
 {$IFDEF GPUGRADIENTS}
 if (not deferGradients) then
 	CreateGradientVolumeGPU(Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, volTex, gradTex);
 {$ELSE}
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

function ComputeStepSize (Quality1to5, Slices: integer): single;
var
  i : integer;
  f: single;
begin
  //if (i = 0) then i := 2; //dynamic (1=0.4, 2=0.55 ... 5=1.0
  i := Quality1to5 - 1;
  i := max(i,0);
  i := min(i,4); //quality 5 is same as 4 but adds cubic sampling
  f := lerp(slices*0.4,slices*1.0, i/4); //0.4..1.0
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

(*function TGPUVolume.ColorEditorMouseDown(mouseX, mouseY: integer): boolean;
begin
   if not colorEditorVisible then exit(false);
   result := colorEditor.ColorEditorMouseDown(mouseX, mouseY);

end; *)

function TGPUVolume.GetSlice2DMaxXY(mouseX, mouseY: integer; var Lo: TPoint): TPoint;
begin
  result := slices2D.GetSlice2DMaxXY2D(mouseX, mouseY, lo);
end;

function TGPUVolume.GetSlice2DFrac(mouseX, mouseY: integer; out Orient: integer): TVec3;
begin
    result := slices2D.GetSlice2DFrac2D(mouseX, mouseY, Orient);
end;

procedure TGPUVolume.Paint2D(var vol: TNIfTI; Drawing: TDraw; DisplayOrient: integer);
var
 vertUniforms: TVertUniforms2D;
 fragUniforms: TFragUniforms2D;
 w,h, scale, rulerPx: single;
 i: integer;
begin
  if vertexBuffer = nil then // only once
    LoadCube();
  //if (vol.VolRGBA <> nil) then
  LoadTexture(vol, DisplayOrient <> kAxCorSagOrient4);
     //LoadTexture(vol, false);
  if (volTex = nil) then
    exit;
  w := mtlControl.clientwidth;
  h := mtlControl.clientheight;
  {$IFDEF CLRBAR}
  if (clrbar <> nil) and (clrbar.PanelFraction < 1.0) and (clrbar.PanelFraction > 0.0) then begin
     if (clrbar.isVertical) then
        w := round(w * (1.0-clrbar.PanelFraction))
     else
         h := round(h * (1.0-clrbar.PanelFraction));
   scale := slices2D.Update(vol.Scale, w, h, DisplayOrient, mtlControl.clientheight);
   w := mtlControl.clientwidth;
   h := mtlControl.clientheight;
  end else   {$ENDIF}
      scale := slices2D.Update(vol.Scale, w, h, DisplayOrient);
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
    if not isSmooth2D then
       MTLSetShader(shader2Dn)
    else
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
    //draw inset rendering
    if (DisplayOrient = kAxCorSagOrient4) then begin
          i := RayCastQuality1to5;
          if (i < kQualityMedium) then
           	RayCastQuality1to5 := kQualityMedium;
          PaintCore(vol, slices2D.axCorSagOrient4XY, false, false);
          RayCastQuality1to5 := i;
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
    rulerPx := (scale)/vol.MaxMM ;//pixels per mm
    rulerPx := rulerPx * 100; //ruler is 10cm = 100mm
    {$IFDEF CLRBAR}
    clrbar.RulerPixels:= rulerPx;
    if clrbar <> nil then
     clrbar.Draw();
    {$ENDIF}
  MTLEndFrame();
  //GLForm1.caption := inttostr(slices2D.NumberOfLineVertices) +' '+inttostr(random(888));
end; //paint2D

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
    //if (vol.VolRGBA <> nil) then
       LoadTexture(vol, false);
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

  fragUniforms.normMatrix := modelMatrix.Inverse.Transpose;
  //
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
  fragUniforms.modelViewProjectionMatrix :=  vertUniforms.modelViewProjectionMatrix;
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
  fragUniforms.clipThick := clipThick;
  fragUniforms.clipPlane := fClipPlane;
  fragUniforms.sliceSize := 1.0/maxDim;
  //fragUniforms.stepSize := 1.0/ ((maxDim*0.25)+ (maxDim*1.75)* (RayCastQuality1to10/10));
  fragUniforms.stepSize := ComputeStepSize (RayCastQuality1to5, maxDim);
  //MTLBeginFrame();
    //if Quality1to5 = kQualityBest then
       MTLSetShader(shader3Dbetter);
    //else
    //    MTLSetShader(shader3D);
    MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
    MTLSetFragmentTexture(volTex, 0);
    MTLSetFragmentTexture(gradTex, 1);
    MTLSetFragmentTexture(overlayVolTex, 2);
    MTLSetFragmentTexture(overlayGradTex, 3);
    {$IFDEF MATCAP}
    if (matcapLoc >= 0) then
       MTLSetFragmentTexture(matcapTex, 4);
    {$ENDIF}
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
    //if (vol.VolRGBA <> nil) then
       LoadTexture(vol, false);
    if (volTex = nil) then
      exit;
    w := mtlControl.clientwidth;
    h := mtlControl.clientheight;
    //next two lines only difference between Paint2D and PainMosaic2D
    //slices2D.Update(vol.Scale, w, h, DisplayOrient);
    {$IFDEF CLRBAR}
    if clrbar <> nil then begin
         f := clrbar.PanelFraction;
         //GLForm1.Caption := format('%d %.2g',[round(h), f]);
         if (f > 0.0) and (f < 0.5) then begin
            if (clrbar.isVertical) then
               w := w - (w * f)
            else
               h := h - (h * f);
         end;
    end; {$ENDIF}
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
        if not isSmooth2D then
           MTLSetShader(shader2Dn)
        else
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
      {$IFDEF CLRBAR}
      clrbar.RulerPixels:= 0;
      if clrbar <> nil then
       clrbar.Draw();
      {$ENDIF}
      //we do not know the background color - might be nice to have black text if background is white!
      txt.DrawText();
    MTLEndFrame;
  end;
{$ENDIF}

procedure TGPUVolume.Paint(var vol: TNIfTI);
var
	widthHeightLeft: TVec3i;
begin
	widthHeightLeft.x := mtlControl.clientwidth;
        widthHeightLeft.y := mtlControl.clientheight;
        widthHeightLeft.z := 0;
        PaintCore(vol, widthHeightLeft);
end;

procedure TGPUVolume.PaintDepth(var vol: TNIfTI; isAxCorSagOrient4: boolean = false);
var
	widthHeightLeft: TVec3i;
begin
	widthHeightLeft.x := mtlControl.clientwidth;
        widthHeightLeft.y := mtlControl.clientheight;
        widthHeightLeft.z := 0;
        if isAxCorSagOrient4 then
        	widthHeightLeft := slices2D.axCorSagOrient4XY;
        //PaintCore(vol, widthHeightLeft, false, true);
        PaintCore(vol, widthHeightLeft, true, true);
end;

{$include cylinder.inc}

procedure TGPUVolume.PaintCrosshair3D(var vol: TNIfTI; rgba: TVec4);
var
    faces: TFaces = nil;
    vertices: TVertices = nil;
    i,nVert: integer;
    radius: TVec3;
begin
  radius := vol.mmAsFrac * (vol.minPixDim * slices2D.LineWidth * 0.5);
  MakeCyl(radius, slices2D.sliceFrac, faces, vertices);
  nVert := length(vertices);
  if length(gLines3Dv) <> nVert then
  	setlength(gLines3Dv, nVert);
  for i := 0 to (nVert - 1) do begin
  	 gLines3Dv[i].position := vertices[i];
     gLines3Dv[i].color := rgba;
  end;
  if (gLines3DnIdx = (3 * length(faces))) then exit;
  gLines3DnIdx := 3 * length(faces);
  gLines3DIndexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@faces[0], sizeof(uint32) * gLines3DnIdx, MTLResourceStorageModeShared);
end;

procedure TGPUVolume.PaintCore(var vol: TNIfTI; widthHeightLeft: TVec3i; clearScreen: boolean = true; isDepthShader: boolean = false);
//procedure TGPUVolume.Paint(var vol: TNIfTI);
var
 {$IFDEF VIEW2D}
 vertUniforms2D: TVertUniforms2D;
 {$ENDIF}
  vertUniforms: TVertUniforms;
  fragUniforms: TFragUniforms;
  projectionMatrix, modelMatrix: TMat4;
  modelLightPos, v, rayDir: TVec4;
  scale, left, bottom, rotateX: single;
begin
  //whratio := widthHeightLeft.x /widthHeightLeft.y;
  if vertexBuffer = nil then // only once
     LoadCube();
  //if (vol.VolRGBA <> nil) then
  LoadTexture(vol, false);
  if (volTex = nil) or ( widthHeightLeft.x < 1) or (widthHeightLeft.y < 1) then
     exit;
  if (Vol.Dim.z < 2) and  (abs(fElevation) < 0.1) then
      rotateX := -1.57254//(-DegToRad(90.1))
  else
      rotateX := -DegToRad(90-fElevation);
  //next lines required if object is translated on screen:
  modelMatrix := TMat4.Identity;
  modelMatrix *= TMat4.Translate(0.5, 0.5, -1.0);
  modelMatrix *= TMat4.RotateX(rotateX);
  modelMatrix *= TMat4.RotateZ(DegToRad(fAzimuth));
  modelMatrix *= TMat4.Translate(-0.5, -0.5, -0.5); //pivot around 0.5 as cube spans 0..1
  modelLightPos := (modelMatrix.Transpose * fLightPos);
  fragUniforms.normMatrix := modelMatrix.Inverse.Transpose;

  modelMatrix := TMat4.Identity;
  modelMatrix *= TMat4.Scale(widthHeightLeft.x / mtlControl.clientwidth, widthHeightLeft.y / mtlControl.clientheight, 1);
  modelMatrix *= TMat4.Translate(0, 0, -fDistance);
  modelMatrix *= TMat4.RotateX(rotateX);
  modelMatrix *= TMat4.RotateZ(DegToRad(fAzimuth));
  modelMatrix *= TMat4.RotateX(-DegToRad(fPitch));
  modelMatrix *= TMat4.Translate(-vol.Scale.X/2, -vol.Scale.Y/2, -vol.Scale.Z/2);
  modelMatrix *= TMat4.Scale(vol.Scale.X, vol.Scale.Y, vol.Scale.Z); //for volumes that are rectangular not square
  //fragUniforms.normMatrix := modelMatrix.Inverse.Transpose; //only if centered on screen, not if translated
  if fDistance = 0 then
     scale := 1
  else
      scale := 0.5 * 1/abs(kDefaultDistance/(fDistance+1.0));

  //whratio := widthHeightLeft.x /widthHeightLeft.y;
  //if (whratio < 1) and (whratio > 0) then //Wide window
  //		scale /= whratio;
  bottom := -widthHeightLeft.y / mtlControl.clientheight * scale;
  if widthHeightLeft.z <= 0 then
	left :=  scale * (widthHeightLeft.x/widthHeightLeft.y)
  else begin
    renderBitmapWidth :=  MTLCurrentDrawableTextureWidth;
	  left :=  2.0 *  ((widthHeightLeft.z + (0.5 * widthHeightLeft.x))  / mtlControl.clientwidth) * scale;
  end;
  projectionMatrix := TMat4.Ortho(0-left, scale * (widthHeightLeft.x/widthHeightLeft.y) * 2 - left, bottom + 0, bottom + 2 * scale, fDistance-1, fDistance+1);
  vertUniforms.modelViewProjectionMatrix := ( projectionMatrix * modelMatrix);
  fragUniforms.modelViewProjectionMatrix := vertUniforms.modelViewProjectionMatrix;
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
  fragUniforms.clipThick := clipThick;
  //GLForm1.Caption := format('>> %g', [fragUniforms.backAlpha]);
  fragUniforms.sliceSize := 1/maxDim;
  //fragUniforms.stepSize := 1.0/ ((maxDim*0.25)+ (maxDim*1.75)* (RayCastQuality1to10/10));
  fragUniforms.stepSize := ComputeStepSize (RayCastQuality1to5, maxDim);
  if (clearScreen) then
  	MTLBeginFrame();
  {$IFDEF OLDDEPTHPICKER}
  //writeln('>>>',isDepthShader);
  //if (isDepthShader) then
  if (UseDepthShader) then
     MTLSetShader(shaderDepth)
  else {$ENDIF}
    if RayCastQuality1to5 = kQualityBest then
       MTLSetShader(shader3Dbetter)
    else
        MTLSetShader(shader3D);
    MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
    MTLSetFragmentTexture(volTex, 0);
    MTLSetFragmentTexture(gradTex, 1);
    MTLSetFragmentTexture(overlayVolTex, 2);
    MTLSetFragmentTexture(overlayGradTex, 3);
    {$IFDEF MATCAP}
    if (matcapLoc >= 0) then
       MTLSetFragmentTexture(matcapTex, 4);
    {$ENDIF}
    MTLSetVertexBuffer(vertexBuffer, 0, 0);
    MTLSetFragmentBytes(@fragUniforms, sizeof(fragUniforms), 1);
    if (ShaderPrefs.nUniform > 1) and (not isDepthShader) then
       MTLSetFragmentBytes(@prefValues[1], ShaderPrefs.nUniform  *sizeof(single), 2); //n.b. kMaxUniform NOT ShaderPrefs.nUniform
    MTLSetCullMode(MTLCullModeFront);
    MTLDrawIndexed (MTLPrimitiveTypeTriangleStrip, 14, MTLIndexTypeUInt16, indexBuffer, 0);
    MTLSetCullMode(MTLCullModeNone);

    //draw crosshair
    {$IFDEF LINE3D}
    if ((widthHeightLeft.z <> 0) and (slices2D.LineWidth > 0.0)) then begin
      MTLSetShader(shaderLine3D);
      MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
      PaintCrosshair3D(vol,  slices2D.LineColor);
      line3DBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@gLines3Dv[0], length(gLines3Dv) *sizeof(TVertVertex), MTLResourceStorageModeShared);
      MTLSetVertexBuffer(line3DBuffer, 0, 0);
      MTLDrawIndexed (MTLPrimitiveTypeTriangle, gLines3DnIdx, MTLIndexTypeUInt32, gLines3DIndexBuffer, 0);
      //draw again with alpha shader
      MTLSetShader(shaderLine3Dalpha);
      MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
      MTLSetVertexBuffer(line3DBuffer, 0, 0);
      MTLDrawIndexed (MTLPrimitiveTypeTriangle, gLines3DnIdx, MTLIndexTypeUInt32, gLines3DIndexBuffer, 0);
    end;
    //MTLDrawIndexed (MTLPrimitiveTypeLine, 14, MTLIndexTypeUInt16, indexBuffer, 0);
    {$ENDIF}

    {$IFDEF VIEW2D}
    if (colorEditorVisible) and (widthHeightLeft.z = 0) then begin //n.b. depth testing causes issues...
      //MTLSetCullMode(MTLCullModeNone); //added to end of rendering
      vertUniforms2D.viewportSize := V2(widthHeightLeft.x, widthHeightLeft.y );
      colorEditor.Update(widthHeightLeft.x, widthHeightLeft.y , vol);
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
    {$IFDEF CLRBAR}
    clrbar.RulerPixels:= 0;
    if (widthHeightLeft.z = 0)  and (clrbar <> nil) then
       clrbar.Draw();
    {$ENDIF}
    {$IFDEF CUBE}
    if (not isDepthShader) and (widthHeightLeft.z = 0) then begin
      if Slices.LabelOrient then begin
         gCube.Azimuth := fAzimuth;
         gCube.Elevation := -fElevation;
         gCube.Pitch := fPitch;
         gCube.Draw(mtlControl.clientwidth, mtlControl.clientheight);
      end;
    end;
    {$ENDIF}
  if (clearScreen) then MTLEndFrame();
end;


end.

