unit mtllines;
//openGL lines

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
interface


uses
  MetalPipeline, MetalUtils, MetalControl, Metal,
  VectorMath,  SimdUtils,
  Classes, SysUtils, Graphics,  dialogs;


type
  TGPULines = class
  private
         LineWid: single;
         LineClr: TRGBA;
         vertexBuffer: MTLBufferProtocol;
         numVertices: integer;
         isRedraw: boolean;
    	 shaderPipeline: TMetalPipeline;
         mtlControl: TMetalControl;
         procedure CreateStrips;
         procedure SetPipeline;
  public
    property NumberOfVertices : integer read numVertices;
    property LineWidth : single read LineWid write LineWid;
    property LineColor : TRGBA read LineClr write LineClr;
    procedure AddLine(startX,startY,endX,endY: single); overload;
    procedure AddLine(startXY, endXY: TVec2); overload;
    procedure AddLine(nodes: TVec2s); overload;
    procedure AddRectangle(Left,Top,Right,Bottom: single);
    procedure ClearLines();
    procedure Draw(); //must be called while TMetalControl is current context
    constructor Create(fromView: TMetalControl);
  end;

implementation

type
  TPoint3f = Packed Record
    x,y,z: single;
  end;

type
  TVertUniforms = record //Uniforms for vertex shader
    viewportSize, viewportSizeDivTwo, TwoDivViewportSize: TVec2;
  end;
  TVtxClr = record
    position: TVec2;
    padding: TVec2; // align each vertex attribute on 16 byte boundries
    color: TVec4;
    //colorU4,pad1,pad2,pad3: TRGBA;
  end;

const
  kBlockSz = 2048;
var
    g2Dvnc: array of TVtxClr;

procedure TGPULines.CreateStrips;
begin
  setlength(g2Dvnc,0);
  isRedraw := false;
end;

procedure TGPULines.SetPipeline();
var
 options: TMetalPipelineOptions;
 shaderName: string;
begin
     if (shaderPipeline <> nil) then exit; //already set
     options := TMetalPipelineOptions.Default;
     shaderName := ResourceFolderPath + pathdelim + 'lines.metal';
     if not fileexists(shaderName) then begin
        if not fileexists(ShaderDir+pathdelim+'_Line2D.metal') then
           writeln('Unable to find ' + shaderName);
        shaderName := ShaderDir+pathdelim+'_Line2D.metal';
     end;
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
end;

constructor TGPULines.Create(fromView: TMetalControl);
begin
     mtlControl := fromView;
     LineClr := setRGBA(255, 255, 255, 255);
     LineWid := 10;
     isRedraw := true;
     numVertices := 0;
     vertexBuffer := nil;
     shaderPipeline := nil;
end;

procedure TGPULines.ClearLines();
begin
     numVertices := 0;
end;

procedure TGPULines.AddLine(startX,startY,endX,endY: single); overload;
var
  i: integer;
  nx,ny, len: single;
  clr: TVec4;
begin
  ny :=  (startX-endX);
  nx :=  (startY-endY);
  len := sqrt(sqr(nx)+sqr(ny));
  if (len = 0) then exit;
  nx := 0.5*lineWid*nx/len;
  ny := 0.5*lineWid*ny/len;
  if (numVertices+6) > length(g2Dvnc) then
     setlength(g2Dvnc, length(g2Dvnc)+kBlockSz);
  clr := Vec4(LineClr.R/255, LineClr.G/255, LineClr.B/255, LineClr.A/255);
  //for i := 0 to 5 do
  //    g2Dvnc[numVertices +i].colorU4 := LineClr;
  for i := 0 to 5 do
      g2Dvnc[numVertices +i].color := clr;
  g2Dvnc[numVertices+0].position := Vec2(startX+nx,startY-ny);
  g2Dvnc[numVertices+1].position := Vec2(startX-nx,startY+ny);
  g2Dvnc[numVertices+2].position := Vec2(endX+nx,endY-ny);
  g2Dvnc[numVertices+3].position := g2Dvnc[numVertices+1].position;
  g2Dvnc[numVertices+4].position := g2Dvnc[numVertices+2].position;
  g2Dvnc[numVertices+5].position := Vec2(endX-nx,endY+ny);
  numVertices := numVertices + 6;
  isRedraw := true;
end;

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
   clr: TVec4;
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
     clr := Vec4(LineClr.R/255, LineClr.G/255, LineClr.B/255, LineClr.A/255);
     for i := 0 to (j-1) do
         g2Dvnc[numVertices +i].color := clr;
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
         g2Dvnc[numVertices+0].position := Vec2(nodes[i-1].x+m[0].x,nodes[i-1].y-m[0].y);
         g2Dvnc[numVertices+1].position := Vec2(nodes[i-1].x-m[0].x,nodes[i-1].y+m[0].y);
         g2Dvnc[numVertices+2].position := Vec2(nodes[i].x+m[1].x,nodes[i].y-m[1].y);
         g2Dvnc[numVertices+3].position := g2Dvnc[numVertices+1].position;
         g2Dvnc[numVertices+4].position := g2Dvnc[numVertices+2].position;
         g2Dvnc[numVertices+5].position := Vec2(nodes[i].x-m[1].x,nodes[i].y+m[1].y);
         numVertices := numVertices + 6;
         for j := 0 to 1 do
             n[j] := n[j+1]; //increment one link
         m[0] := m[1];
     end;
end;

(*procedure TGPULines.AddLine(nodes: TVec2s); overload;
//this version creates errors at joints
var
   i: integer;
begin
     if length(nodes) < 2 then exit;
     if length(nodes) = 2 then begin
        AddLine(nodes[0],nodes[1]);
        exit;
     end;
     for i := 1 to (length(nodes)-1) do
         AddLine(nodes[i-1],nodes[i]);
end;*)

procedure TGPULines.AddRectangle(Left,Top,Right,Bottom: single);
var
  i: integer;
  clr: TVec4;
begin
  if (numVertices+6) > length(g2Dvnc) then
     setlength(g2Dvnc, length(g2Dvnc)+kBlockSz);
  clr := Vec4(LineClr.R/255, LineClr.G/255, LineClr.B/255, LineClr.A/255);
  for i := 0 to 5 do
      g2Dvnc[numVertices +i].color := clr;
  g2Dvnc[numVertices+0].position := Vec2(Left,Top);
  g2Dvnc[numVertices+1].position := Vec2(Left,Bottom);
  g2Dvnc[numVertices+2].position := Vec2(Right,Top);
  g2Dvnc[numVertices+3].position := g2Dvnc[numVertices+1].position;
  g2Dvnc[numVertices+4].position := g2Dvnc[numVertices+2].position;
  g2Dvnc[numVertices+5].position := Vec2(Right,Bottom);
  numVertices := numVertices + 6;
  isRedraw := true;
end;

procedure TGPULines.Draw();
var
vertUniforms: TVertUniforms;
begin
  if numVertices < 1 then exit;
  setPipeline;
  MTLSetShader(shaderPipeline);
  if isRedraw then begin //only update buffer if something has changed
     vertexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@g2Dvnc[0], numVertices*sizeof(TVtxClr), MTLResourceStorageModeShared);
     isRedraw := false;
  end;
  vertUniforms.viewportSize := V2(mtlControl.Width, mtlControl.Height);
  vertUniforms.viewportSizeDivTwo := V2(mtlControl.Width/2.0, mtlControl.Height/2.0);
  vertUniforms.TwoDivViewportSize := V2(2.0/mtlControl.Width, 2.0/mtlControl.Height);
  MTLSetVertexBuffer(vertexBuffer, 0, 0);
  MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
  MTLDraw(MTLPrimitiveTypeTriangle, 0, numVertices);
end;

end.

