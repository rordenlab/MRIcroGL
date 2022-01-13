unit mtlfont;
//http://metalbyexample.com/translucency-and-transparency/
//Metal Text using distance field fonts
//traditional signed-distance field fonts use a single channel (alpha), here we use multi-channel (red,green,blue)
//This can preserve sharp corners in fonts
//  https://github.com/Chlumsky/msdfgen
//  https://github.com/Jam3/msdf-bmfont
{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$include glopts.inc}//newmsdf
interface

uses
  MetalPipeline,  MetalControl, Metal, SimdUtils, MetalUtils,
  sdffont, VectorMath, LResources, Dialogs,Classes, SysUtils, Graphics, math;
type
TGPUFont = class(TSDFFont)
  private
         indexBuffer, vertexBuffer: MTLBufferProtocol;
         pipeline: TMetalPipeline;
         mtlControl: TMetalControl;
         pngTex: MTLTextureProtocol;
    procedure InitShader;
    function LoadTex(fnm: string): boolean;
  public
    procedure DrawText(); //call between MTLBeginFrame...MTLDrawIndexed
    constructor Create(fnm : string; out success: boolean; var fromView: TMetalControl); //call at end of  MetalControl.OnPrepare
  end;

implementation

function TGPUFont.LoadTex(fnm: string): boolean;
var
  px: TPicture;
  bmpHt, bmpWid: integer;
  pngTexDesc: MTLTextureDescriptor;
  pngRegion: MTLRegion;
  is32bit: boolean;

begin
  result := false;
  if not (LoadPng(fnm, px, is32bit)) then exit;
  bmpHt := px.Bitmap.Height;
  bmpWid := px.Bitmap.Width;
  if px.Bitmap.PixelFormat <> pf32bit then
     exit; //distance stored in ALPHA field
  pngTexDesc := MTLTextureDescriptor.alloc.init.autorelease;
  {$IFDEF NEWMSDF}
  pngTexDesc.setTextureType(MTLTextureType2D);
  {$ELSE}
  pngTexDesc.setTextureType(MTLTextureType3D);
  {$ENDIF}
  pngTexDesc.setPixelFormat(MTLPixelFormatBGRA8Unorm);
  pngTexDesc.setWidth(bmpWid);
  pngTexDesc.setHeight(bmpHt);
  pngTexDesc.setDepth(1);
  if (pngTex <> nil) then pngTex.release;
  pngTex := mtlControl.renderView.device.newTextureWithDescriptor(pngTexDesc);
  Fatal(pngTex = nil, format('mtlfont: newTextureWithDescriptor failed %dx%d', [bmpHt, bmpWid]));
  pngRegion := MTLRegionMake3D(0, 0, 0, bmpWid, bmpHt, 1);
  pngTex.replaceRegion_mipmapLevel_withBytes_bytesPerRow(pngRegion, 0, PInteger(px.Bitmap.RawImage.Data), bmpWid*4);
  px.Free;
  result := true;
end;

constructor TGPUFont.Create(fnm: string; out success: boolean; var fromView: TMetalControl);
//call at end of  MetalControl.OnPrepare
var
   i,j,k,n: integer;
   faces: array of uint16;
begin
  inherited Create(fnm, success);
  if not success then exit;
  //CreateX(fnm, success);
  if not success then exit;
  mtlControl := fromView;
  pngTex := nil;
  pipeline := nil;
  vertexBuffer := nil;
  //set face indices
  n := kMaxChar * 2 * 3; //each character composed of 2 triangles, each triangle has 3 vertices
  setlength(faces, n );
  j := 0;
  for i := 0 to ((kMaxChar)-1) do begin
      k := i * 4;
      faces[j] := 0+k;
      j := j + 1;
      faces[j] := 1+k;
      j := j + 1;
      faces[j] := 2+k;
      j := j + 1;
      faces[j] := 2+k;
      j := j + 1;
      faces[j] := 1+k;
      j := j + 1;
      faces[j] := 3+k;
      j := j + 1;
  end;
  indexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@faces[0], sizeof(uint16) * Length(faces), MTLResourceStorageModeShared);
  if not LoadTex(fnm) then success := false;
end;

procedure TGPUFont.InitShader;
var
 options: TMetalPipelineOptions;
 fnm: string;
begin
  if pipeline <> nil then exit;
  options := TMetalPipelineOptions.Default;
  {$IFDEF NEWMSDF}
  fnm := ShaderDir + pathdelim +  '_sdf.metal';
  {$ELSE}
  fnm := ResourceDir + pathdelim + 'msdf.metal';
  if not fileexists(fnm) then
     fnm := ShaderDir + pathdelim +  '_Msdf.metal';
  {$ENDIF}
  options.libraryName := fnm;
  //options.libraryName := ResourcePath('msdf', 'metal');
  if not fileexists(options.libraryName) then
    writeln('Unable to find ' + fnm);
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
end; //InitShader()

procedure TGPUFont.DrawText(); //call between MTLBeginFrame...MTLDrawIndexed
var
   uniform_vtx: TVertUniforms;
begin
  if self.NumChar < 1 then exit; //nothing to draw
  InitShader;
  if self.Redraw then begin //only update buffer if something has changed
     //vertexBuffer := MetalControl.renderView.device.newBufferWithBytes_length_options(@quads[0], sizeof(Txyuv)*4*nChar, MTLResourceStorageModeShared);
     vertexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@self.QuadVerts[0], sizeof(Txyuv)*4*self.NumChar, MTLResourceStorageModeShared);
     self.Redraw := false;
  end;
  MTLSetShader(pipeline);
  //MTLBeginEncoding(FontShader);
    uniform_vtx.viewportSize := V2(mtlControl.Width,mtlControl.Height);
    MTLSetCullMode(MTLCullModeNone);
    //nb we use MTLSetVertexBuffer not MTLSetVertexBytes for data > 4096 bytes
    //MTLSetVertexBytes(@quads, sizeof(Txyuv)*4*nChar, 0);
    MTLSetVertexBuffer(vertexBuffer, 0, 0);
    MTLSetVertexBytes(@uniform_vtx, sizeof(uniform_vtx), 1);
    MTLSetFragmentTexture(pngTex, 0);
    MTLSetFragmentBytes(@self.FontColor, sizeof(TVec4), 1);
    MTLDrawIndexed (MTLPrimitiveTypeTriangle, 6*self.NumChar, MTLIndexTypeUInt16, indexBuffer, 0);
  //MTLEndEncoding;
end; //DrawText()

end.


