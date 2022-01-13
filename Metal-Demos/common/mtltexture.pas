unit mtltexture;
//Metal Texture - draw a bitmap
{$mode objfpc}{$H+}
{$modeswitch objectivec1}
interface

uses
  MetalPipeline, MetalUtils, MetalControl, Metal, SimdUtils,
  VectorMath, LResources, Dialogs,Classes, SysUtils, Graphics, math;

type
  TGPUTexture = class
  private
    bmpHt, bmpWid: integer;
    OffsetX,OffsetY,Zoom: single;
    pngTex: MTLTextureProtocol;
    shaderPipeline: TMetalPipeline;
    mtlControl: TMetalControl;
    procedure LoadTex(fnm : string);
    procedure InitShader;
  public
    property Pipeline: TMetalPipeline read shaderPipeline;
    property BitmapHeight: integer read bmpHt;
    property BitmapWidth: integer read bmpWid;
    procedure DrawTex(); //must be called while TOpenGLControl is current context
    procedure SetPosition(xPixel, yPixel, zoomRatio: single);
    constructor Create(fnm : string; fromView: TMetalControl); overload;
    constructor Create(fnm : string; fromView: TMetalControl; var existingPipeline: TMetalPipeline); overload;
    Destructor  Destroy; override;
  end;

implementation

procedure TGPUTexture.SetPosition(xPixel, yPixel, zoomRatio: single);
begin
   OffsetX := xPixel;
   OffsetY := yPixel;
   Zoom := zoomRatio;
end;

procedure TGPUTexture.LoadTex(fnm: string);
var
 pngTexDesc: MTLTextureDescriptor;
 pngRegion: MTLRegion;
 px: TPicture;
 is32bit: boolean;
begin
 px := TPicture.Create;
 if (fnm='') or (not fileexists(fnm)) then
    fnm := ResourcePath('texture', 'png');
 if not (LoadPng(fnm, px, is32bit)) then exit;
 bmpWid := px.Bitmap.Width;
 bmpHt := px.Bitmap.Height;
 pngTexDesc := MTLTextureDescriptor.alloc.init.autorelease;
 pngTexDesc.setTextureType(MTLTextureType2D);
 if is32bit then
    pngTexDesc.setPixelFormat(MTLPixelFormatBGRA8Unorm)
 else
 	pngTexDesc.setPixelFormat(MTLPixelFormatRGBA8Unorm);
 //pngTexDesc.setPixelFormat(MTLPixelFormatRGBA8Unorm);
 pngTexDesc.setWidth(bmpWid);
 pngTexDesc.setHeight(bmpHt);
 pngTexDesc.setDepth(1);
 if (pngTex <> nil) then pngTex.release;
 pngTex := mtlControl.renderView.device.newTextureWithDescriptor(pngTexDesc);
 Fatal(pngTex = nil, 'mtltexture: newTextureWithDescriptor failed');
 pngRegion := MTLRegionMake2D(0, 0, bmpWid, bmpHt);
 pngTex.replaceRegion_mipmapLevel_withBytes_bytesPerRow(pngRegion, 0, PInteger(px.Bitmap.RawImage.Data), bmpWid*4);
 px.Free;
end;

constructor TGPUTexture.Create(fnm : string; fromView: TMetalControl); overload;
begin
     shaderPipeline := nil;
     pngTex := nil;
     Create(fnm, fromView, shaderPipeline);
end;


constructor TGPUTexture.Create(fnm : string; fromView: TMetalControl; var existingPipeline: TMetalPipeline); overload;
begin
 mtlControl := fromView;
 OffsetX := 0;
 OffsetY := 0;
 Zoom := 1;
 shaderPipeline := existingPipeline;
 LoadTex(fnm);
end;

procedure TGPUTexture.InitShader;
var
 options: TMetalPipelineOptions;
 shaderName: string;
begin
  if pipeline <> nil then exit;
  options := TMetalPipelineOptions.Default;
  shaderName := ResourceDir + pathdelim + 'texture.metal';
  if not fileexists(shaderName) then
    shaderName := ShaderDir + pathdelim +  '_Texture.metal';
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
  options.pipelineDescriptor.setSampleCount(mtlControl.renderView.sampleCount);
  shaderPipeline := MTLCreatePipeline(options);
end; //InitShader()

type
  TVertUniforms = record //Uniforms for vertex shader
    viewportSize: TVec2;
  end;
  TAAPLVertex = record
    position: TVec2;
    // align each vertex attribute on 16 byte boundries
    padding: TVec2;
    color: TVec4;
  end;

function AAPLVertex(constref position: TVec2; constref color: TVec4): TAAPLVertex;
begin
    result.position := position;
    result.color := color;
end;

procedure TGPUTexture.DrawTex();
var
 verts: array[0..3] of TAAPLVertex;
 vertUniforms: TVertUniforms;
 ZoomX,ZoomY: single;
begin
 InitShader;
  MTLSetShader(shaderPipeline);
  if (pngTex = nil) then
    LoadTex('');
  ZoomX := bmpWid * Zoom * 0.5;
  ZoomY := bmpHt * Zoom * 0.5;
  verts[0] := AAPLVertex(V2(OffsetX - ZoomX, OffsetY + ZoomY), V4(0, 0, 0, 1));
  verts[1] := AAPLVertex(V2(OffsetX - ZoomX, OffsetY -ZoomY), V4(0, 1, 0, 1));
  verts[2] := AAPLVertex(V2(OffsetX + ZoomX, OffsetY + ZoomY), V4(1, 0, 0, 1));
  verts[3] := AAPLVertex(V2(OffsetX + ZoomX, OffsetY -ZoomY), V4(1, 1, 0, 1));
  vertUniforms.viewportSize := V2(mtlControl.Width, mtlControl.Height);
  MTLSetFragmentTexture(pngTex, 0);
  MTLSetVertexBytes(@verts, sizeof(verts), 0);
  MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
  MTLDraw(MTLPrimitiveTypeTriangleStrip, 0, 4);
end;

destructor TGPUTexture.Destroy;
begin
  //call the parent destructor:
  inherited;
end;

end.
