unit glvolume2;
{$mode objfpc}
{$H+}
interface
{$include opts.inc} //for  DEFINE FASTGZ
{$DEFINE STRIP} //we can define cube as either a triangle or triangle strip - no implications on performance
{$DEFINE GPUGRADIENTS} //Computing volume gradients on the GPU is much faster than using the CPU
{$DEFINE VIEW2D}
//{$DEFINE LINE3D}
{$DEFINE CUBE}
{$DEFINE MATCAP}
{$DEFINE CLRBAR}
{$DEFINE TIMER} //reports GPU gradient time to stdout (Unix only)
//{$DEFINE DEPTHPICKER_USEFRAMEBUFFER}
{$include ./Metal-Demos/common/glopts.inc}
uses
  {$IFDEF MATCAP} GraphType, FPImage, IntfGraphics, LCLType,{$ENDIF}
  {$IFDEF TIMER} DateUtils,{$ENDIF}
 {$IFDEF CUBE} Forms, glcube, {$ENDIF}
  {$IFDEF CLRBAR}glclrbar,  {$ENDIF}
  {$IFDEF COREGL} glcorearb, {$ELSE} {$IFNDEF Darwin}gl, {macos has macgl }{$ELSE}MacOSAll,{$ENDIF} glext, {$ENDIF}
  {$IFDEF VIEW2D} colorEditor, slices2D, glfont, {$ENDIF}
 retinahelper, niftis, SimdUtils,  gl_core_utils, VectorMath, Classes, SysUtils, Graphics,
    math, OpenGLContext, dialogs, nifti, drawvolume ;
const
 kDefaultDistance = 2.25;
 kMaxDistance = 40;
 kGradientModeGPUFastest = 0;  //no gradient smooth
 kGradientModeGPUFast = 1; //low precision smooth
 kGradientModeGPUSlow = 2; //high precision smooth
 kGradientModeCPUSlowest = 3; //highest precision
 kQualityMedium = 3;
 kQualityBest = 5;
type
  TGPUVolume = class
      private
        {$IFDEF VIEW2D}
        {$IFDEF COREGL}
        vao, vaoTex2D, vboTex2D, vaoLine2D, vboLine2D,
        {$ELSE}
        textureSzLoc : GLint;
        dlTex2D, dlLine2D,  dlBox3D, dlColorEditor,
        {$ENDIF}
        {$IFDEF LINE3D}
        programLine3D,
        {$ENDIF}
        programLine2D, programTex2D: GLuint;
        slices2D: TSlices2D;
        {$IFDEF LINE3D}{$IFDEF COREGL}vaoLine3D,{$ENDIF} vboLine3D, vboLineIdx3D, mvpLine3DLoc, colorLine3DLoc, {$ENDIF}
        uniform_drawTex, uniform_drawLUT, uniform_drawAlpha,
        uniform_texVox, uniform_viewportSizeLine, uniform_viewportSizeTex, uniform_backAlpha,
        uniform_tex, uniform_overlay, uniform_overlaysLoc: GLint;
        colorEditor: TColorEditor;
        isSmooth2D, colorEditorVisible: boolean;
        txt: TGPUFont;
        {$ENDIF}
        {$IFDEF COREGL}vboBox3D: GLuint;{$ENDIF}
        shaderPrefs: TShaderPrefs;
        RayCastQuality1to5, maxDim,fAzimuth,fElevation, fPitch, overlayNum, overlayGradTexWidth: integer;
        fDistance: single;
        fLightPos: TVec4;
        fClipPlane: TVec4;
        {$IFDEF MTX}fModelMatrix: TMat4;{$ENDIF}
        {$IFDEF CLRBAR}clrbar: TGPUClrbar;{$ENDIF}
        glControl: TOpenGLControl;
        prefLoc: array [1..kMaxUniform] of GLint;
        overlayGradientVolLoc, overlayIntensityVolLoc,
        rayDirLoc,intensityVolLoc, gradientVolLoc,mvpLoc,normLoc, lightPositionLoc,clipPlaneLoc, clipThickLoc,
        backAlphaLoc, sliceSizeLoc, stepSizeLoc, overlaysLoc : GLint;
        overlayGradientTexture3D, overlayIntensityTexture3D,
        drawTexture1D, drawTexture3D,
        gradientTexture3D, intensityTexture3D, programRaycast, programRaycastBetter: GLuint;
        {$IFDEF DEPTHPICKER2}
        mvp: TMat4;
        viewportXYWH: TVec4;
        {$ENDIF}
        {$IFDEF MATCAP}
        matcap2D1, matcap2D2: GLuint;
        {$ENDIF}
        {$IFDEF CUBE} gCube :TGPUCube; {$ENDIF}
        {$IFDEF GPUGRADIENTS}programSobel, programBlur: GLuint;
        procedure CreateGradientVolumeGPU(Xsz,Ysz,Zsz: integer; var inTex, grTex: GLuint);
        procedure GenerateGradient(var inTex, grTex: GLuint);
        {$ENDIF}
        procedure LoadCube();
        function LoadTexture(var vol: TNIfTI; deferGradients: boolean): boolean;
        procedure CreateDrawColorTable;//1D texture for drawing
        procedure CreateDrawTex(Dim: TVec3i; Vals: TUInt8s);
        procedure UpdateDraw(Drawing: TDraw);
        procedure CreateOverlayTextures(Dim: TVec3i; volRGBA: TRGBAs);
        {$IFNDEF COREGL}procedure UpdateColorEditorDisplayList; {$ENDIF}
      public
        matcap1Loc, matcap2Loc: GLint;
        gradientMode: integer;
        ClipThick: single;
        {$IFDEF VIEW2D}
        SelectionRect: TVec4;
        property ShowColorEditor: boolean read colorEditorVisible write colorEditorVisible;
        property ShowSmooth2D: boolean read isSmooth2D write isSmooth2D;
        property CE: TColorEditor read colorEditor;
        property Slices: Tslices2D read slices2D;
        function Slice2Dmm(var vol: TNIfTI; out vox: TVec3i): TVec3;
        procedure SetSlice2DFrac(frac : TVec3);
        function GetSlice2DFrac(mouseX, mouseY: integer; var  oOrient: integer): TVec3;
        function Unproject(mouseX, mouseY, depth: single): TVec3;
        function GetSlice2DMaxXY(mouseX, mouseY: integer; var Lo: TPoint): TPoint;
        procedure Paint2D(var vol: TNIfTI; Drawing: TDraw; DisplayOrient: integer);
        {$IFDEF MOSAIC}
        procedure PaintMosaicRender(var vol: TNIfTI; lRender: TMosaicRender);
        procedure PaintMosaic2D(var vol: TNIfTI; Drawing: TDraw; MosaicString: string);
        {$ENDIF} //MOSAIC
        {$ENDIF}
        {$IFDEF MATCAP}procedure SetMatCap(lFilename: string; isPrimary: boolean = true);{$ENDIF}
        //procedure CreateOverlayTextures();
        procedure UpdateOverlays(vols: TNIfTIs);
        property Quality1to5: integer read RayCastQuality1to5 write RayCastQuality1to5;
        property ShaderSliders: TShaderPrefs read shaderPrefs write shaderPrefs;
        procedure SetShaderSlider(idx: integer; newVal: single);
        property Azimuth: integer read fAzimuth write fAzimuth;
        property Elevation: integer read fElevation write fElevation;
        property Pitch: integer read fPitch write fPitch;
        {$IFDEF MTX} property ModelMatrix3D: TMat4 read fModelMatrix write fModelMatrix; {$ENDIF}
        property Distance: single read fDistance write fDistance;
        property LightPosition: TVec4 read fLightPos write fLightPos;
        property ClipPlane: TVec4 read fClipPlane write fClipPlane;
        procedure Prepare(shaderName: string);
        procedure SetGradientMode(newMode: integer);
        constructor Create(fromView: TOpenGLControl);
        function MakeCrosshair3D(var vol: TNIfTI): integer;
        procedure PaintCrosshair3D(rgba: TVec4; nFaces: integer);
        procedure PaintCore(var vol: TNIfTI; widthHeightLeft: TVec3i; clearScreen: boolean = true; isDepthShader: boolean = false);
        procedure Paint(var vol: TNIfTI);
        procedure PaintDepth(var vol: TNIfTI; isAxCorSagOrient4: boolean = false);
        procedure SetShader(shaderName: string);
        {$IFDEF CLRBAR}procedure SetColorBar(fromColorbar: TGPUClrbar);{$ENDIF}
        procedure  SetTextContrast(clearclr: TRGBA);
        destructor Destroy; override;
  end;

implementation

//uses mainunit;
{$IFNDEF COREGL}{$IFDEF LINUX}
  {$DEFINE MESA_HACKS}
{$ENDIF}{$ENDIF}
{$IFDEF LINE3D}{$IFDEF COREGL}
//var gLines3D: array of TVec3;
{$ENDIF}{$ENDIF}

procedure printf (lS: AnsiString);
begin
{$IFNDEF WINDOWS} writeln(lS); {$ENDIF}
end;

{$IFDEF DEPTHPICKER2}
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
{$ELSE}
function TGPUVolume.Unproject(mouseX, mouseY, depth: single): TVec3;
begin
    result := vec3(2.0, 2.0, 2.0);
end;
{$ENDIF}

destructor TGPUVolume.Destroy;
begin
  {$IFDEF VIEW2D}
  slices2D.free;
  colorEditor.free;
  txt.free;
  {$ENDIF}
  {$IFDEF LINE3D}{$IFDEF COREGL}
  //gLines3D := nil;
  {$ENDIF}{$ENDIF}
  {$IFDEF CUBE} gCube.free; {$ENDIF}
  {$IFDEF CLRBAR} clrbar.free; {$ENDIF}
  inherited;
end;

procedure GetErrorAll(p: integer; str: string = '');  //report OpenGL Error
{$IFDEF UNIX}
  var
    Error: GLenum;
    s: string;
  begin
   Error := glGetError();
   if Error = GL_NO_ERROR then exit;
    s := inttostr(p)+'->';
   if Error = GL_INVALID_ENUM then
      s := s+'GL_INVALID_ENUM'
   else if Error = GL_INVALID_VALUE then
      s := s+'GL_INVALID_VALUE' //out of range https://www.khronos.org/registry/OpenGL-Refpages/es2.0/xhtml/glGetError.xml
   else
       s := s + inttostr(Error);
   printf('OpenGL Error (reduce MaxVox) '+str+s);
end;
{$ELSE}
begin
  //
end;
{$ENDIF}

procedure  TGPUVolume.SetTextContrast(clearclr: TRGBA);
begin

  {$IFDEF VIEW2D}
  if (clearclr.R + clearclr.G + clearclr.B) > 300 then
     txt.FontColor := Vec4(0,0,0,1)
  else
     txt.FontColor := Vec4(1,1,1,1);
  slices2D.SetFontColor(txt.FontColor);
  {$ENDIF}
end;

{$IFDEF VIEW2D}
const
{$IFDEF COREGL}
kVertTex2D = '#version 330'
+#10'layout(location = 0) in vec2 point;'
+#10'layout(location = 4) in vec4 texCoordIn;'
+#10'uniform vec2 ViewportSize;'
+#10'out vec4 texCoord;'
+#10'void main() {'
+#10'    texCoord = texCoordIn;'
+#10'    vec2 pixelPosition = point;'
+#10'    pixelPosition -= (ViewportSize/2.0);'
+#10'    gl_Position = vec4((pixelPosition / (ViewportSize/2.0)), 0.0, 1.0);'
+#10'}';

kFragTex2D = '#version 330'
+#10'in vec4 texCoord;'
+#10'out vec4 color;'
+#10'uniform float backAlpha = 1.0;'
+#10'uniform sampler3D tex, drawTex, overlay;'
+#10'uniform sampler1D drawLUT;'
+#10'uniform float drawAlpha = 0.0;'
+#10'uniform int overlays = 0;'
+#10'void main() {'
+#10'    color = texture(tex,texCoord.xyz);'
+#10'    color.a = smoothstep(0.0, 0.01, color.a);'
+#10'    //color.a = smoothstep(0.0, 0.1, color.a);'
+#10'    //color.a = smoothstep(0.0, 0.00001, color.a);'
+#10'    color.a *= backAlpha;'
+#10'    //if ((overlays == 0) && (drawAlpha == 0.0)) color.r = 1.0; //test case where neither overlay or draw is visible'
+#10'    if ((overlays == 0) && (drawAlpha == 0.0)) return;'
+#10'    //if (color.a > 0.0) color.a = backAlpha;'
+#10'    vec4 ocolor = texture(overlay, texCoord.xyz);'
+#10'    //vec4 ocolor = texture(drawLUT, texture(overlay, texCoord.xyz).r).rgba;'
+#10'    //vec4 ocolor = texture(drawLUT, texture(drawTex, texCoord.xyz).r).rgba;'
+#10'    color.rgb = mix(color.rgb, ocolor.rgb, ocolor.a);'
+#10'    color.a = max(color.a, ocolor.a);'
+#10'    if (drawAlpha == 0.0) return;'
+#10'    ocolor = texture(drawLUT, texture(drawTex, texCoord.xyz).r).rgba;'
+#10'    ocolor.a *= drawAlpha;'
+#10'    color.rgb = mix(color.rgb, ocolor.rgb, ocolor.a);'
+#10'    color.a = max(color.a, ocolor.a);'
+#10'}';

//TODO: pixelPosition -= 0.5;??? offset: line at row 1 with width 1 should span 0..1 not 0.5..1.5
kVertLine2D = '#version 330'
+#10'layout(location = 0) in vec2 point;'
+#10'layout(location = 4) in vec4 texCoordIn;'
+#10'uniform vec2 ViewportSize;'
+#10'out vec4 texCoord;'
+#10'void main() {'
+#10'    texCoord = texCoordIn;'
+#10'    vec2 pixelPosition = point;'
+#10'    pixelPosition -= (ViewportSize/2.0);'
+#10'    gl_Position = vec4((pixelPosition / (ViewportSize/2.0)), 0.0, 1.0);'
+#10'}';

kFragLine2D = '#version 330'
+#10'in vec4 texCoord;'
+#10'out vec4 color;'
+#10'void main() {'
+#10'    color = texCoord;'
+#10'}';
{$ELSE}
kVertTex2D = '#version 120'
+#10'uniform vec2 ViewportSize;'
+#10'varying vec4 texCoord;'
+#10'void main() {'
+#10'    vec2 pixelPosition = gl_Vertex.xy;'
+#10'    pixelPosition -= (ViewportSize/2.0);'
+#10'    gl_Position = vec4((pixelPosition / (ViewportSize/2.0)), 0.0, 1.0);'
+#10'    texCoord = gl_Color;'
+#10'}';
 {$IFDEF MESA_HACKS} //required for Mesa running VirtualBox with "Enable 3D Acceleration"
 kMesaKludge =  #10'    if (texCoord.x > 2.0) {'
 +#10'      gl_FragColor = vec4(1.0, 0.0, 0.0, 0.5);'
 +#10'      return;'
 +#10'    }';
 kMesaKuldge2 =  #10'    ocolor = texture1D(drawLUT, texture3D(drawTex, texCoord.xyz).a).rgba;';
 {$ELSE}
kMesaKludge = '';
kMesaKuldge2 = #10'    ocolor = texture1D(drawLUT, texture3D(drawTex, texCoord.xyz).r).rgba;';
{$ENDIF}

kFragTex2D = '#version 120'
+#10'varying vec4 texCoord;'
+#10'uniform float backAlpha = 1.0;'
+#10'uniform sampler3D tex, drawTex, overlay;'
+#10'uniform sampler1D drawLUT;'
+#10'uniform float drawAlpha = 0.5;'
+#10'uniform int overlays = 0;'
+#10'void main() {'
+kMesaKludge
+#10'    vec4 color = texture3D(tex,texCoord.xyz);'
+#10'    color.a = smoothstep(0.0, 0.01, color.a);'
+#10'    //color.a = smoothstep(0.0, 0.1, color.a);'
+#10'    color.a *= backAlpha;'
+#10'    //if ((overlays == 0) && (drawAlpha == 0.0)) color.r = 1.0; //test case where neither overlay or draw is visible'
+#10'    if ((overlays == 0) && (drawAlpha == 0.0)) {'
+#10'        gl_FragColor = color;'
+#10'        return;'
+#10'    }'
+#10'    //if (color.a > 0.0) color.a = backAlpha;'
+#10'    vec4 ocolor = texture3D(overlay, texCoord.xyz);'
+#10'    color.rgb = mix(color.rgb, ocolor.rgb, ocolor.a);'
+#10'    color.a = max(color.a, ocolor.a);'
+#10'    if (drawAlpha <= 0.0) {'
+#10'        gl_FragColor = color;'
+#10'        return;'
+#10'    }'
+kMesaKuldge2
+#10'    ocolor.a *= drawAlpha;'
+#10'    color.rgb = mix(color.rgb, ocolor.rgb, ocolor.a);'
+#10'    color.a = max(color.a, ocolor.a);'
+#10'    gl_FragColor = color;'
+#10'}';

kVertLine2D = '#version 120'
+#10'//akVertLine2D'
+#10'uniform vec2 ViewportSize;'
+#10'varying vec4 texCoord;'
+#10'void main() {'
+#10'    texCoord = gl_Color;'
+#10'    vec2 pixelPosition = gl_Vertex.xy;'
+#10'    pixelPosition -= (ViewportSize/2.0);'
+#10'    gl_Position = vec4((pixelPosition / (ViewportSize/2.0)), 0.0, 1.0);'
+#10'}';

kFragLine2D = '#version 120'
+#10'//akFragLine2D'
+#10'varying vec4 texCoord;'
+#10'void main() {'
+#10'    gl_FragColor = texCoord;'
+#10'}';
{$ENDIF} //IF COREGL else legacy
{$ENDIF} //If View2D
//uses vrForm;
procedure TGPUVolume.SetShaderSlider(idx: integer; newVal: single);
begin
      if (idx < 1) or (idx > kMaxUniform) then exit;
     shaderPrefs.Uniform[idx].DefaultV:=newVal;
end;

{$IFDEF GPUGRADIENTS}
function bindBlankGL(Xsz,Ysz,Zsz, gradientMode: integer): GLuint;
const
     k2Gb = 2147483648;
var
  width, height, depth: GLint;
  isRGBA8: boolean;
  rgbaBytes: int64;
begin //creates an empty texture in VRAM without requiring memory copy from RAM
    //later run glDeleteTextures(1,&oldHandle);
    GetErrorAll(101,'PreBindBlank');
    isRGBA8 := (gradientMode = kGradientModeGPUFast) or (gradientMode = kGradientModeGPUFastest);
    rgbaBytes :=  XSz * YSz * ZSz * 8; //GL_RGBA16 = is 8 bytes per voxel
    if (rgbaBytes >= k2Gb) then
       isRGBA8 := true;
    if isRGBA8 then
       glTexImage3D(GL_PROXY_TEXTURE_3D, 0, GL_RGBA8, XSz, YSz, ZSz, 0, GL_RGBA, GL_UNSIGNED_BYTE, NIL)
    else
       glTexImage3D(GL_PROXY_TEXTURE_3D, 0, GL_RGBA16, XSz, YSz, ZSz, 0, GL_RGBA, GL_UNSIGNED_BYTE, NIL);
    glGetTexLevelParameteriv(GL_PROXY_TEXTURE_3D, 0, GL_TEXTURE_WIDTH, @width);
    glGetTexLevelParameteriv(GL_PROXY_TEXTURE_3D, 0, GL_TEXTURE_HEIGHT, @height);
    glGetTexLevelParameteriv(GL_PROXY_TEXTURE_3D, 0, GL_TEXTURE_DEPTH, @depth);
    //https://www.opengl.org/archives/resources/faq/technical/texture.htm
    printf(format('blur proxy test %dx%dx%d',[width, height, depth]));
    if (width < Xsz) then begin
       printf('Unable to generate gradient texture. Solution: adjust "MaxVox" or press "Reset" button in preferences.');
       exit(0);
    end;
    glGenTextures(1, @result);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glBindTexture(GL_TEXTURE_3D, result);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE); //, GL_CLAMP_TO_BORDER) will wrap
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
    if isRGBA8 then
       glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA8, XSz, YSz, ZSz, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil)
    else
        glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA16, XSz, YSz, ZSz, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
    GetErrorAll(101,'BindBlank');
end;

procedure glUniform1ix(prog: GLuint; name: AnsiString; value: integer);
begin
    glUniform1i(glGetUniformLocation(prog, pAnsiChar(Name)), value) ;
end;

procedure glUniform1fx(prog: GLuint; name: AnsiString; value: single );
begin
    glUniform1f(glGetUniformLocation(prog, pAnsiChar(Name)), value) ;
end;

{$IFDEF COREGL}
const kBlurSobelVert = '#version 330 core'
+#10'layout(location = 0) in vec3 vPos;'
+#10'out vec2 TexCoord;'
+#10'void main() {'
+#10'    TexCoord = vPos.xy;'
+#10'    gl_Position = vec4( (vPos.xy-vec2(0.5,0.5))* 2.0, 0.0, 1.0);'
+#10'//    gl_Position = vec4( (vPos-vec3(0.5,0.5,0.5))* 2.0, 1.0);'
+#10'}';

const kBlurFrag = '#version 330 core'
+#10'in vec2 TexCoord;'
+#10'out vec4 FragColor;'
+#10'uniform float coordZ, dX, dY, dZ;'
+#10'uniform sampler3D intensityVol;'
+#10'void main(void) {'
+#10' vec3 vx = vec3(TexCoord.xy, coordZ);'
+#10' vec4 samp = texture(intensityVol,vx+vec3(+dX,+dY,+dZ));'
+#10' samp += texture(intensityVol,vx+vec3(+dX,+dY,-dZ));'
+#10' samp += texture(intensityVol,vx+vec3(+dX,-dY,+dZ));'
+#10' samp += texture(intensityVol,vx+vec3(+dX,-dY,-dZ));'
+#10' samp += texture(intensityVol,vx+vec3(-dX,+dY,+dZ));'
+#10' samp += texture(intensityVol,vx+vec3(-dX,+dY,-dZ));'
+#10' samp += texture(intensityVol,vx+vec3(-dX,-dY,+dZ));'
+#10' samp += texture(intensityVol,vx+vec3(-dX,-dY,-dZ));'
+#10' FragColor = samp*0.125;'
+#10'}';

const kSobelFrag = '#version 330 core'
+#10'in vec2 TexCoord;'
+#10'out vec4 FragColor;'
+#10'uniform float coordZ, dX, dY, dZ;'
+#10'uniform sampler3D intensityVol;'
+#10'void main(void) {'
+#10'  vec3 vx = vec3(TexCoord.xy, coordZ);'
+#10'  float TAR = texture(intensityVol,vx+vec3(+dX,+dY,+dZ)).a;'
+#10'  float TAL = texture(intensityVol,vx+vec3(+dX,+dY,-dZ)).a;'
+#10'  float TPR = texture(intensityVol,vx+vec3(+dX,-dY,+dZ)).a;'
+#10'  float TPL = texture(intensityVol,vx+vec3(+dX,-dY,-dZ)).a;'
+#10'  float BAR = texture(intensityVol,vx+vec3(-dX,+dY,+dZ)).a;'
+#10'  float BAL = texture(intensityVol,vx+vec3(-dX,+dY,-dZ)).a;'
+#10'  float BPR = texture(intensityVol,vx+vec3(-dX,-dY,+dZ)).a;'
+#10'  float BPL = texture(intensityVol,vx+vec3(-dX,-dY,-dZ)).a;'
+#10'  vec4 gradientSample = vec4 (0.0, 0.0, 0.0, 0.0);'
+#10'  gradientSample.r =   BAR+BAL+BPR+BPL -TAR-TAL-TPR-TPL;'
+#10'  gradientSample.g =  TPR+TPL+BPR+BPL -TAR-TAL-BAR-BAL;'
+#10'  gradientSample.b =  TAL+TPL+BAL+BPL -TAR-TPR-BAR-BPR;'
+#10'  gradientSample.a = (abs(gradientSample.r)+abs(gradientSample.g)+abs(gradientSample.b))*0.29;'
+#10'  gradientSample.rgb = normalize(gradientSample.rgb);'
+#10'  gradientSample.rgb =  (gradientSample.rgb * 0.5)+0.5;'
+#10'  FragColor = gradientSample;'
+#10'}';
{$ELSE}

const kBlurSobelVert = '';

kBlurFrag = '#version 120'
+#10'uniform float coordZ, dX, dY, dZ;'
+#10'uniform sampler3D intensityVol;'
+#10'void main(void) {'
+#10' vec3 vx = vec3(gl_TexCoord[0].xy, coordZ);'
+#10' vec4 samp = texture3D(intensityVol,vx+vec3(+dX,+dY,+dZ));'
+#10' samp += texture3D(intensityVol,vx+vec3(+dX,+dY,-dZ));'
+#10' samp += texture3D(intensityVol,vx+vec3(+dX,-dY,+dZ));'
+#10' samp += texture3D(intensityVol,vx+vec3(+dX,-dY,-dZ));'
+#10' samp += texture3D(intensityVol,vx+vec3(-dX,+dY,+dZ));'
+#10' samp += texture3D(intensityVol,vx+vec3(-dX,+dY,-dZ));'
+#10' samp += texture3D(intensityVol,vx+vec3(-dX,-dY,+dZ));'
+#10' samp += texture3D(intensityVol,vx+vec3(-dX,-dY,-dZ));'
+#10' gl_FragColor = samp*0.125;'
+#10'}';

//this will estimate a Sobel smooth
const kSobelFrag = '#version 120'
+#10'uniform float coordZ, dX, dY, dZ;'
+#10'uniform sampler3D intensityVol;'
+#10'void main(void) {'
+#10'  vec3 vx = vec3(gl_TexCoord[0].xy, coordZ);'
+#10'  float TAR = texture3D(intensityVol,vx+vec3(+dX,+dY,+dZ)).a;'
+#10'  float TAL = texture3D(intensityVol,vx+vec3(+dX,+dY,-dZ)).a;'
+#10'  float TPR = texture3D(intensityVol,vx+vec3(+dX,-dY,+dZ)).a;'
+#10'  float TPL = texture3D(intensityVol,vx+vec3(+dX,-dY,-dZ)).a;'
+#10'  float BAR = texture3D(intensityVol,vx+vec3(-dX,+dY,+dZ)).a;'
+#10'  float BAL = texture3D(intensityVol,vx+vec3(-dX,+dY,-dZ)).a;'
+#10'  float BPR = texture3D(intensityVol,vx+vec3(-dX,-dY,+dZ)).a;'
+#10'  float BPL = texture3D(intensityVol,vx+vec3(-dX,-dY,-dZ)).a;'
+#10'  vec4 gradientSample = vec4 (0.0, 0.0, 0.0, 0.0);'
+#10'  gradientSample.r =   BAR+BAL+BPR+BPL -TAR-TAL-TPR-TPL;'
+#10'  gradientSample.g =  TPR+TPL+BPR+BPL -TAR-TAL-BAR-BAL;'
+#10'  gradientSample.b =  TAL+TPL+BAL+BPL -TAR-TPR-BAR-BPR;'
+#10'  gradientSample.a = (abs(gradientSample.r)+abs(gradientSample.g)+abs(gradientSample.b))*0.29;'
+#10'  gradientSample.rgb = normalize(gradientSample.rgb);'
+#10'  gradientSample.rgb =  (gradientSample.rgb * 0.5)+0.5;'
+#10'  gl_FragColor = gradientSample;'
+#10'}';
{$ENDIF}
procedure TGPUVolume.CreateGradientVolumeGPU(Xsz,Ysz,Zsz: integer; var inTex, grTex: GLuint);
//given 3D input texture inTex (with dimensions Xsz, Ysz, Zsz) generate 3D gradient texture gradTex
//http://www.opengl-tutorial.org/intermediate-tutorials/tutorial-14-render-to-texture/
//http://www.opengl.org/wiki/Framebuffer_Object_Examples
//{$DEFINE SMOOTHGRAD}
var
   i: integer;
   coordZ: single;
   fb , tempTex3D: GLuint;
   isSmoothGrad: boolean;
   {$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
begin
  glFinish();//force update
  {$IFDEF TIMER}startTime := now;{$ENDIF}
  {$IFDEF COREGL}
  glBindVertexArray(vao);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboBox3D);
  glGenFramebuffers(1, @fb);
  glBindFramebuffer(GL_FRAMEBUFFER, fb);
  {$ELSE}
  glGenFramebuffersEXT(1, @fb);
  glBindFramebufferEXT(GL_FRAMEBUFFER, fb);
  {$ENDIF}
  glDisable(GL_CULL_FACE);
  GetErrorAll(96,'CreateGradient');
  //{$IFNDEF COREGL}glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);{$ENDIF}// <- REQUIRED
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
  GetErrorAll(97,'CreateGradient');
  glViewport(0, 0, XSz, YSz);
  {$IFNDEF COREGL}
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho (0, 1,0, 1, -1, 1);  //gluOrtho2D(0, 1, 0, 1);  https://www.opengl.org/sdk/docs/man2/xhtml/gluOrtho2D.xml
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  {$ENDIF}
  //glDisable(GL_TEXTURE_2D); //<- generates error!
  glDisable(GL_BLEND);
  tempTex3D := 0;
  isSmoothGrad :=  gradientMode > kGradientModeGPUFastest;
  if (isSmoothGrad) then begin
    //STEP 1: run smooth program gradientTexture -> tempTex3D
    tempTex3D := bindBlankGL(Xsz,Ysz,Zsz, gradientMode);
    glUseProgram(programBlur);
    glActiveTexture( GL_TEXTURE1);
    //glBindTexture(GL_TEXTURE_3D, gRayCast.gradientTexture3D);//input texture
    glBindTexture(GL_TEXTURE_3D, inTex);//input texture is overlay
    glUniform1ix(programBlur, 'intensityVol', 1);
    glUniform1fx(programBlur, 'dX', 0.7/XSz); //0.5 for smooth - center contributes
    glUniform1fx(programBlur, 'dY', 0.7/YSz);
    glUniform1fx(programBlur, 'dZ', 0.7/ZSz);
    {$IFDEF COREGL}
    glBindVertexArray(vao);
    {$ENDIF}
    for i := 0 to (ZSz-1) do begin
        coordZ := 1/ZSz * (i + 0.5);
        glUniform1fx(programBlur, 'coordZ', coordZ);
        //glFramebufferTexture3D(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0, GL_TEXTURE_3D, tempTex3D, 0, i);//output texture
        //Ext required: Delphi compile on Winodws 32-bit XP with NVidia 8400M
        {$IFDEF COREGL}
        glFramebufferTexture3D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_3D, tempTex3D, 0, i);//output texture
        {$ELSE}
        glFramebufferTexture3DExt(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0, GL_TEXTURE_3D, tempTex3D, 0, i);//output texture
        {$ENDIF}
        glClear(GL_DEPTH_BUFFER_BIT);  // clear depth bit (before render every layer)
        {$IFDEF COREGL}
        {$IFDEF STRIP}
        glDrawElements(GL_TRIANGLE_STRIP, 4, GL_UNSIGNED_INT, nil);
        {$ELSE}
        glDrawElements(GL_TRIANGLES, 2*3, GL_UNSIGNED_INT, nil);
        {$ENDIF}
        {$ELSE}
        glBegin(GL_QUADS);
        glTexCoord2f(0, 0);
        glVertex2f(0, 0);
        glTexCoord2f(1.0, 0);
        glVertex2f(1.0, 0.0);
        glTexCoord2f(1.0, 1.0);
        glVertex2f(1.0, 1.0);
        glTexCoord2f(0, 1.0);
        glVertex2f(0.0, 1.0);
        glEnd();
        {$ENDIF}
    end;
    GetErrorAll(101,'CreateGradient');
    glUseProgram(0);
  end; //isSmoothGrad
  //STEP 2: run sobel program gradientTexture -> tempTex3D
  //glUseProgramObjectARB(gRayCast.glslprogramSobel);
  glUseProgram(programSobel);
  glActiveTexture(GL_TEXTURE1);
  //x glBindTexture(GL_TEXTURE_3D, gRayCast.intensityTexture3D);//input texture
  if isSmoothGrad then
     glBindTexture(GL_TEXTURE_3D, tempTex3D)//input texture
  else
      glBindTexture(GL_TEXTURE_3D, inTex);//input texture is overlay
    glUniform1ix(programSobel, 'intensityVol', 1);
    glUniform1fx(programSobel, 'dX', 1.2/XSz ); //1.0 for SOBEL - center excluded
    glUniform1fx(programSobel, 'dY', 1.2/YSz);
    glUniform1fx(programSobel, 'dZ', 1.2/ZSz);
    {$IFDEF COREGL}
    glBindVertexArray(vao);
    {$ENDIF}
    for i := 0 to (ZSz-1) do begin
        coordZ := 1/ZSz * (i + 0.5);
        glUniform1fx(programSobel, 'coordZ', coordZ);
        {$IFDEF COREGL}
        glFramebufferTexture3D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_3D, grTex, 0, i);//output is background
        {$ELSE}
        glFramebufferTexture3DExt(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0, GL_TEXTURE_3D, grTex, 0, i);//output is background
        {$ENDIF}
        glClear(GL_DEPTH_BUFFER_BIT);
        {$IFDEF COREGL}
        {$IFDEF STRIP}
        glDrawElements(GL_TRIANGLE_STRIP, 4, GL_UNSIGNED_INT, nil);
        {$ELSE}
        glDrawElements(GL_TRIANGLES, 2*3, GL_UNSIGNED_INT, nil);
        {$ENDIF}
        {$ELSE}
        glBegin(GL_QUADS);
        glTexCoord2f(0, 0);
        glVertex2f(0, 0);
        glTexCoord2f(1.0, 0);
        glVertex2f(1.0, 0.0);
        glTexCoord2f(1.0, 1.0);
        glVertex2f(1.0, 1.0);
        glTexCoord2f(0, 1.0);
        glVertex2f(0.0, 1.0);
        glEnd();
        {$ENDIF}
    end;
    glUseProgram(0);
    glFinish();//force update
    GetErrorAll(102,'CreateGradient');
     //clean up:
     if isSmoothGrad then
        glDeleteTextures(1,@tempTex3D);
    {$IFDEF COREGL}
    {$IFDEF LCLgtk3}
    glBindFramebuffer(GL_FRAMEBUFFER, 1);
    {$ELSE}
     glBindFramebuffer(GL_FRAMEBUFFER, 0);
    {$ENDIF}
     glDeleteFramebuffers(1, @fb);
    {$ELSE}
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
    glDeleteFramebuffersEXT(1, @fb);
    {$ENDIF}
    GetErrorAll(103,'CreateGradient');
    glActiveTexture( GL_TEXTURE0 );  //required if we will draw 2d slices next
    {$IFDEF TIMER}printf(format('GPU Gradient time %d',[MilliSecondsBetween(Now,startTime)]));{$ENDIF}
end;
{$ENDIF}
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
//portion of voiCreate that requires OpenGL context
var
   vx,i: int64;
   v: TUInt8s;
begin
  GetErrorAll(96,'CreateDrawTex');
  //printf(format('Creating draw texture %dx%dx%d %d', [Dim.X, Dim.Y, Dim.Z]));
  if (drawTexture3D <> 0) then glDeleteTextures(1,@drawTexture3D);
  glPixelStorei(GL_UNPACK_ALIGNMENT,1);
  glGenTextures(1, @drawTexture3D);
  glBindTexture(GL_TEXTURE_3D, drawTexture3D);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_BORDER);
  if (Vals = nil) then begin
     vx := prod(Dim);//Xsz * Ysz * Zsz;
     setlength(v,vx);
     for i := 0 to (vx -1) do
         v[i] := random(3);
     {$IFDEF MESA_HACKS}
     glTexImage3D(GL_TEXTURE_3D, 0, GL_INTENSITY, Dim.X, Dim.Y, Dim.Z, 0, GL_RED, GL_UNSIGNED_BYTE,@v[0]);
     {$ELSE}
     glTexImage3D(GL_TEXTURE_3D, 0, GL_RED, Dim.X, Dim.Y, Dim.Z, 0, GL_RED, GL_UNSIGNED_BYTE,@v[0]);
     {$ENDIF}
     v := nil;
  end else begin
      {$IFDEF MESA_HACKS}
      glTexImage3D(GL_TEXTURE_3D, 0, GL_INTENSITY, Dim.X, Dim.Y, Dim.Z, 0, GL_RED, GL_UNSIGNED_BYTE,@Vals[0]);
      {$ELSE} //Mesa does not support GL_RED!
      glTexImage3D(GL_TEXTURE_3D, 0, GL_RED, Dim.X, Dim.Y, Dim.Z, 0, GL_RED, GL_UNSIGNED_BYTE,@Vals[0]);
      {$ENDIF}
  end;
  GetErrorAll(102,'CreateDrawTex');
end;
const
 //A common vertex shader for all volume rendering
{$IFDEF COREGL}
kVert = '#version 330 core'
+#10'layout(location = 0) in vec3 vPos;'
+#10'out vec3 TexCoord1;'
+#10'out vec4 vPosition;'
+#10'uniform mat4 ModelViewProjectionMatrix;'
+#10'void main() {'
+#10'  TexCoord1 = vPos;'
+#10'  gl_Position = ModelViewProjectionMatrix * vec4(vPos, 1.0);'
+#10'  vPosition = gl_Position;'
+#10'}';

{$IFDEF LINE3D}
kVertLine3D = '#version 330 core'
+#10'layout(location = 0) in vec3 vPos;'
+#10'uniform mat4 ModelViewProjectionMatrix;'
+#10'void main() {'
+#10'    gl_Position = ModelViewProjectionMatrix * vec4(vPos, 1.0);'
+#10'}';

 kFragLine3D = '#version 330 core'
+#10'uniform vec4 Color;'
+#10'out vec4 FragColor;'
+#10'void main() {'
+#10'    FragColor = Color;'
+#10'}';
{$ENDIF}//line3D


{$ELSE}
kVert = '#version 120'
+#10'varying vec3 TexCoord1;'
+#10'varying vec4 vPosition;'
+#10'uniform mat4 ModelViewProjectionMatrix;'
+#10'void main() {'
+#10'    gl_Position = ModelViewProjectionMatrix * vec4(gl_Vertex.xyz, 1.0);'
+#10'    TexCoord1 = gl_Vertex.rgb;'
+#10'    vPosition = gl_Position;'
+#10'}';

{$IFDEF LINE3D}
kVertLine3D = '#version 120'
+#10'attribute vec3 Vert;'
+#10'uniform mat4 ModelViewProjectionMatrix;'
+#10'void main() {'
+#10'    gl_Position = ModelViewProjectionMatrix * vec4(Vert.xyz, 1.0);'
+#10'}';

 kFragLine3D = '#version 120'
+#10'uniform vec4 Color;'
+#10'void main() {'
+#10'    gl_FragColor = Color;'
+#10'}';
{$ENDIF}//line3D

{$ENDIF}

{$IFDEF MATCAP}

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

function LoadMatCap(fnm: string; var texID: GLuint): boolean;
var
  px: TPicture;
  ifnm, MatCapDir: string;
  {$IFNDEF WINDOWS}
  AImage: TLazIntfImage;
  lRawImage: TRawImage;
  {$ENDIF}
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
  px := TPicture.Create;
    try
       {$IFDEF Windows}
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
  if texID <> 0 then
     glDeleteTextures(1,@texID);
  glGenTextures(1, @texID);
  glBindTexture(GL_TEXTURE_2D,  texID);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
  //glTexImage2D(GL_TEXTURE_2D, 0,GL_RGBA8, px.Width, px.Height, 0, GL_BGRA, GL_UNSIGNED_BYTE, PInteger(px.Bitmap.RawImage.Data));
  {$IFDEF WINDOWS}
  glTexImage2D(GL_TEXTURE_2D, 0,GL_RGBA8, px.Width, px.Height, 0, GL_BGRA, GL_UNSIGNED_BYTE, PInteger(px.Bitmap.RawImage.Data));
  {$ELSE}
  glTexImage2D(GL_TEXTURE_2D, 0,GL_RGBA8, px.Width, px.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, PInteger(px.Bitmap.RawImage.Data));
  {$ENDIF}
  px.Free;
  result := true;
end;

procedure TGPUVolume.SetMatCap(lFilename: string; isPrimary: boolean = true);
begin
  glControl.MakeCurrent();
  if (isPrimary) then
  	LoadMatCap(lFilename, matcap2D1)
  else
  	LoadMatCap(lFilename, matcap2D2);
  glControl.ReleaseContext;
end;
{$ENDIF}

const

{$IFDEF COREGL}
kFragBase = '#version 330'
+#10'in vec3 TexCoord1;'
+#10'out vec4 FragColor;'
+#10'in vec4 vPosition;'
+#10'uniform float stepSize, sliceSize;'
+#10'uniform sampler3D intensityVol, gradientVol;'
+#10'uniform sampler3D intensityOverlay, gradientOverlay;'
+#10'uniform vec3 lightPosition, rayDir;'
+#10'uniform vec4 clipPlane;'
+#10'uniform float clipThick = 1.0;'
+#10'uniform int overlays = 0;'
+#10'uniform float backAlpha = 0.5;'
+#10'uniform mat4 ModelViewProjectionMatrix;'
+#10'void setDepthBuffer(vec3 pos) {'
+#10'	gl_FragDepth = ((ModelViewProjectionMatrix * vec4(pos, 1.0)).z + 1.0) * 0.5;'
+#10'}'
+#10'vec3 GetBackPosition (vec3 startPosition) {'
+#10' vec3 invR = 1.0 / rayDir;'
+#10' vec3 tbot = invR * (vec3(0.0)-startPosition);'
+#10' vec3 ttop = invR * (vec3(1.0)-startPosition);'
+#10' vec3 tmax = max(ttop, tbot);'
+#10' vec2 t = min(tmax.xx, tmax.yz);'
+#10' return startPosition + (rayDir * min(t.x, t.y));'
+#10'}'
+#10'void fastPass (float len, vec3 dir, sampler3D vol, inout vec4 samplePos){'
+#10'	vec4 deltaDir = vec4(dir.xyz * max(stepSize, sliceSize * 1.95), max(stepSize, sliceSize * 1.95));'
+#10'	while  (texture(vol,samplePos.xyz).a == 0.0) {'
+#10'		samplePos += deltaDir;'
+#10'		if (samplePos.a > len) return; //no hit'
+#10'	}'
+#10'	samplePos -= deltaDir;'
+#10'}'
+#10'vec4 applyClip(vec3 dir, inout vec4 samplePos, inout float len) {'
+#10'	float cdot = dot(dir,clipPlane.xyz);'
+#10'	if  ((clipPlane.a > 1.0) || (cdot == 0.0)) return samplePos;'
+#10'	bool frontface = (cdot > 0.0);'
+#10'	float dis = (-clipPlane.a - dot(clipPlane.xyz, samplePos.xyz-0.5)) / cdot;'
+#10'	float  disBackFace = (-(clipPlane.a-clipThick) - dot(clipPlane.xyz, samplePos.xyz-0.5)) / cdot;'
+#10'	if (((frontface) && (dis >= len)) || ((!frontface) && (dis <= 0.0))) {'
+#10'		samplePos.a = len + 1.0;'
+#10'		return samplePos;'
+#10'	}'
+#10'	if (frontface) {'
+#10'		dis = max(0.0, dis);'
+#10'		samplePos = vec4(samplePos.xyz+dir * dis, dis);'
+#10'		len = min(disBackFace, len);'
+#10'	}'
+#10'	if (!frontface) {'
+#10'		len = min(dis, len);'
+#10'		disBackFace = max(0.0, disBackFace);'
+#10'		samplePos = vec4(samplePos.xyz+dir * disBackFace, disBackFace);'
+#10'	}'
+#10'	return samplePos;'
+#10'}'
+#10'vec4 texture2D(sampler2D vol, vec2 coord) {'
+#10'	return texture(vol, coord); //trilinear interpolation'
+#10'}';

// GetBackPosition -> when does ray exit unit cube http://prideout.net/blog/?p=64

kFragFaster = kFragBase
+#10'vec4 texture3Df(sampler3D vol, vec3 coord) {'
+#10'	return texture(vol, coord); //trilinear interpolation'
+#10'}';



kFragBetter = kFragBase
+#10'vec4 texture3Df(sampler3D vol, vec3 coord) {'
+#10'  // shift the coordinate from [0,1] to [-0.5, textureSz-0.5]'
+#10'  vec3 textureSz = textureSize(vol, 0);'
+#10'  vec3 coord_grid = coord * textureSz - 0.5;'
+#10'  vec3 index = floor(coord_grid);'
+#10'  vec3 fraction = coord_grid - index;'
+#10'  vec3 one_frac = 1.0 - fraction;'
+#10'  vec3 w0 = 1.0/6.0 * one_frac*one_frac*one_frac;'
+#10'  vec3 w1 = 2.0/3.0 - 0.5 * fraction*fraction*(2.0-fraction);'
+#10'  vec3 w2 = 2.0/3.0 - 0.5 * one_frac*one_frac*(2.0-one_frac);'
+#10'  vec3 w3 = 1.0/6.0 * fraction*fraction*fraction;'
+#10'  vec3 g0 = w0 + w1;'
+#10'  vec3 g1 = w2 + w3;'
+#10'  vec3 mult = 1.0 / textureSz;'
+#10'  vec3 h0 = mult * ((w1 / g0) - 0.5 + index);  //h0 = w1/g0 - 1, move from [-0.5, textureSz-0.5] to [0,1]'
+#10'  vec3 h1 = mult * ((w3 / g1) + 1.5 + index);  //h1 = w3/g1 + 1, move from [-0.5, textureSz-0.5] to [0,1]'
+#10'  // fetch the eight linear interpolations'
+#10'  // weighting and fetching is interleaved for performance and stability reasons'
+#10'  vec4 tex000 =  texture(vol,h0);'
+#10'  vec4 tex100 =  texture(vol,vec3(h1.x, h0.y, h0.z));'
+#10'  tex000 = mix(tex100, tex000, g0.x);  //weigh along the x-direction'
+#10'  vec4 tex010 =  texture(vol,vec3(h0.x, h1.y, h0.z));'
+#10'  vec4 tex110 =  texture(vol,vec3(h1.x, h1.y, h0.z));'
+#10'  tex010 = mix(tex110, tex010, g0.x);  //weigh along the x-direction'
+#10'  tex000 = mix(tex010, tex000, g0.y);  //weigh along the y-direction'
+#10'  vec4 tex001 =  texture(vol,vec3(h0.x, h0.y, h1.z));'
+#10'  vec4 tex101 =  texture(vol,vec3(h1.x, h0.y, h1.z));'
+#10'  tex001 = mix(tex101, tex001, g0.x);  //weigh along the x-direction'
+#10'  vec4 tex011 =  texture(vol,vec3(h0.x, h1.y, h1.z));'
+#10'  vec4 tex111 =  texture(vol,h1);'
+#10'  tex011 = mix(tex111, tex011, g0.x);  //weigh along the x-direction'
+#10'  tex001 = mix(tex011, tex001, g0.y);  //weigh along the y-direction'
+#10'  return mix(tex001, tex000, g0.z);  //weigh along the z-direction'
+#10'}'
+#10;

(* License applicable to function texture3D():
Copyright (c) 2008-2013, Danny Ruijters. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
*  Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
*  Neither the name of the copyright holders nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are
those of the authors and should not be interpreted as representing official
policies, either expressed or implied.

When using this code in a scientific project, please cite one or all of the
following papers:
*  Daniel Ruijters and Philippe Thévenaz,
   GPU Prefilter for Accurate Cubic B-Spline Interpolation,
   The Computer Journal, vol. 55, no. 1, pp. 15-20, January 2012.
*  Daniel Ruijters, Bart M. ter Haar Romeny, and Paul Suetens,
   Efficient GPU-Based Texture Interpolation using Uniform B-Splines,
   Journal of Graphics Tools, vol. 13, no. 4, pp. 61-69, 2008.
*)

kFrag = kFragFaster
+#10'uniform float ambient = 1.0;'
+#10'uniform float diffuse = 0.3;'
+#10'uniform float specular = 0.25;'
+#10'uniform float shininess = 10.0;'
+#10'uniform float overlayFuzzy = 0.5;'
+#10'uniform float overlayDepth = 0.3;'
+#10'uniform float overlayClip = 0.0;'
+#10'void main() {'
+#10'	#ifdef BETTER_BUT_SLOWER'
+#10'	textureSz = textureSize(intensityVol, 0);'
+#10'	#endif'
+#10' vec3 start = TexCoord1.xyz;'
+#10'	vec3 backPosition = GetBackPosition(start);'
+#10'	vec3 dir = backPosition - start;'
+#10'	float len = length(dir);'
+#10'	dir = normalize(dir);'
+#10'	vec4 deltaDir = vec4(dir.xyz * stepSize, stepSize);'
+#10'	vec4 gradSample, colorSample;'
+#10'	float bgNearest = len;'
+#10'	float overFarthest = len;'
+#10'	vec4 colAcc = vec4(0.0,0.0,0.0,0.0);'
+#10'	vec4 prevGrad = vec4(0.0,0.0,0.0,0.0);'
+#10'	vec4 samplePos;'
+#10'	float noClipLen = len;'
+#10'	samplePos = vec4(start.xyz +deltaDir.xyz* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453)), 0.0);'
+#10'	if (clipPlane.a > -0.5) {'
+#10'		bool frontface = (dot(dir , clipPlane.xyz) > 0.0);'
+#10'		float dis = dot(dir,clipPlane.xyz);'
+#10'		if (dis != 0.0 ) dis = (-clipPlane.a - dot(clipPlane.xyz, start.xyz-0.5)) / dis;'
+#10'		if (((frontface) && (dis >= len)) || ((!frontface) && (dis <= 0.0))) {'
+#10'			samplePos.a = len + 1.0;'
+#10'		} else if ((dis > 0.0) && (dis < len)) {'
+#10'			if (frontface) {'
+#10'				samplePos.a = dis;'
+#10'				samplePos.xyz += dir * dis;'
+#10'			} else {'
+#10'				backPosition = start + dir * (dis);'
+#10'				len = length(backPosition - start);'
+#10'			}'
+#10'		}'
+#10'	}'
+#10'	vec4 clipPos = samplePos;'
+#10'	float stepSizeX2 = samplePos.a + (stepSize * 2.0);'
+#10'	deltaDir = vec4(dir.xyz * max(stepSize, sliceSize * 1.95), max(stepSize, sliceSize * 1.95));'
+#10'	while (samplePos.a <= len) {'
+#10'		if ((texture(intensityVol,samplePos.xyz).a) > 0.0) break;'
+#10'		samplePos += deltaDir;'
+#10'	}'
+#10'	if ((samplePos.a > len) && ( overlays < 1 )) {'
+#10'		FragColor = colAcc;'
+#10'		return;		'
+#10'	}'
+#10'	samplePos -= deltaDir;'
+#10'	if (samplePos.a < clipPos.a)'
+#10'		samplePos = clipPos;'
+#10'	deltaDir = vec4(dir.xyz * stepSize, stepSize);'
+#10'	vec3 defaultDiffuse = vec3(0.5, 0.5, 0.5);'
+#10'	while (samplePos.a <= len) {'
+#10'		colorSample = texture3Df(intensityVol,samplePos.xyz);'
+#10'		colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);'
+#10'		if (colorSample.a > 0.01) {'
+#10'			bgNearest = min(samplePos.a,bgNearest);'
+#10'			if (samplePos.a > stepSizeX2) {'
+#10'				vec3 a = colorSample.rgb * ambient;'
+#10'				gradSample= texture3Df(gradientVol,samplePos.xyz);'
+#10'				gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);'
+#10'				if (gradSample.a < prevGrad.a)'
+#10'					gradSample.rgb = prevGrad.rgb;'
+#10'				prevGrad = gradSample;'
+#10'				'
+#10'				float lightNormDot = dot(gradSample.rgb, lightPosition);'
+#10'				vec3 d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;'
+#10'				float s = specular * pow(max(dot(reflect(lightPosition, gradSample.rgb), dir), 0.0), shininess);'
+#10'				colorSample.rgb = a + d + s;'
+#10'			} else'
+#10'				colorSample.a = clamp(colorSample.a*3.0,0.0, 1.0);'
+#10'			colorSample.rgb *= colorSample.a;'
+#10'			colAcc= (1.0 - colAcc.a) * colorSample + colAcc;'
+#10'			if ( colAcc.a > 0.95 )'
+#10'				break;'
+#10'		}'
+#10'		samplePos += deltaDir;'
+#10'	}'
+#10'	colAcc.a = colAcc.a/0.95;'
+#10'	if ( overlays < 1 ) {'
+#10'		FragColor = colAcc;'
+#10'		return;'
+#10'	}'
+#10'	vec4 overAcc = vec4(0.0,0.0,0.0,0.0);'
+#10'	prevGrad = vec4(0.0,0.0,0.0,0.0);'
+#10'	if (overlayClip > 0)'
+#10'		samplePos = clipPos;'
+#10'	else {'
+#10'		len = noClipLen;'
+#10'		samplePos = vec4(start.xyz +deltaDir.xyz* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453)), 0.0);'
+#10'	}'
+#10'	clipPos = samplePos;'
+#10'	deltaDir = vec4(dir.xyz * max(stepSize, sliceSize), max(stepSize, sliceSize));'
+#10'	while (samplePos.a <= len) {'
+#10'		if ((texture(intensityOverlay,samplePos.xyz).a) > 0.0) break;'
+#10'		samplePos += deltaDir;'
+#10'	}'
+#10'	samplePos -= deltaDir;'
+#10'	if (samplePos.a < clipPos.a)'
+#10'		samplePos = clipPos;'
+#10'	deltaDir = vec4(dir.xyz * stepSize, stepSize);'
+#10'	while (samplePos.a <= len) {'
+#10'		colorSample = texture3Df(intensityOverlay,samplePos.xyz);'
+#10'		if (colorSample.a > 0.00) {'
+#10'			colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);'
+#10'			colorSample.a *= overlayFuzzy;'
+#10'			vec3 a = colorSample.rgb * ambient;'
+#10'			float s = 0;'
+#10'			vec3 d = vec3(0.0, 0.0, 0.0);'
+#10'			overFarthest = samplePos.a;'
+#10'			gradSample = texture3Df(gradientOverlay,samplePos.xyz);'
+#10'			gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);'
+#10'			if (gradSample.a < prevGrad.a)'
+#10'				gradSample.rgb = prevGrad.rgb;'
+#10'			prevGrad = gradSample;'
+#10'			float lightNormDot = dot(gradSample.rgb, lightPosition);'
+#10'			d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;'
+#10'			s = specular * pow(max(dot(reflect(lightPosition, gradSample.rgb), dir), 0.0), shininess);'
+#10'			colorSample.rgb = a + d + s;'
+#10'			colorSample.rgb *= colorSample.a;'
+#10'			overAcc= (1.0 - overAcc.a) * colorSample + overAcc;'
+#10'			if (overAcc.a > 0.95 )'
+#10'				break;'
+#10'		}'
+#10'		samplePos += deltaDir;'
+#10'	}'
+#10'	overAcc.a = overAcc.a/0.95;'
+#10'	colAcc.a *= backAlpha;'
+#10'	float overMix = overAcc.a;'
+#10'	if (((overFarthest) > bgNearest) && (colAcc.a > 0.0)) {'
+#10'		float dx = (overFarthest - bgNearest)/1.73;'
+#10'		dx = colAcc.a * pow(dx, overlayDepth);'
+#10'		overMix *= 1.0 - dx;'
+#10'	}'
+#10'	colAcc.rgb = mix(colAcc.rgb, overAcc.rgb, overMix);'
+#10'		colAcc.a = max(colAcc.a, overAcc.a);'
+#10'	FragColor = colAcc;'
+#10'}';

{$ELSE}
kFragBase = '#version 120'
+#10'varying vec3 TexCoord1;'
+#10'varying vec4 vPosition;'
+#10'uniform float stepSize, sliceSize;'
+#10'uniform sampler3D intensityVol, gradientVol;'
+#10'uniform sampler3D intensityOverlay, gradientOverlay;'
+#10'uniform vec3 lightPosition, rayDir;'
+#10'uniform vec4 clipPlane;'
+#10'uniform float clipThick = 1.0;'
+#10'uniform int overlays = 0;'
+#10'uniform float backAlpha = 0.5;'
+#10'uniform vec3 textureSz = vec3(3.0, 2.0, 1.0);'
+#10'uniform mat4 ModelViewProjectionMatrix;'
+#10'void setDepthBuffer(vec3 pos) {'
+#10'	gl_FragDepth = ((ModelViewProjectionMatrix * vec4(pos, 1.0)).z + 1.0) * 0.5;'
+#10'}'
+#10'vec3 GetBackPosition (vec3 startPosition) {'
+#10' vec3 invR = 1.0 / rayDir;'
+#10' vec3 tbot = invR * (vec3(0.0)-startPosition);'
+#10' vec3 ttop = invR * (vec3(1.0)-startPosition);'
+#10' vec3 tmax = max(ttop, tbot);'
+#10' vec2 t = min(tmax.xx, tmax.yz);'
+#10' return startPosition + (rayDir * min(t.x, t.y));'
+#10'}'
+#10'void fastPass (float len, vec3 dir, sampler3D vol, inout vec4 samplePos){'
+#10'	vec4 deltaDir = vec4(dir.xyz * max(stepSize, sliceSize * 1.95), max(stepSize, sliceSize * 1.95));'
+#10'	while  (texture3D(vol,samplePos.xyz).a == 0.0) {'
+#10'		samplePos += deltaDir;'
+#10'		if (samplePos.a > len) return; //no hit'
+#10'	}'
+#10'	samplePos -= deltaDir;'
+#10'}'
+#10'vec4 applyClip(vec3 dir, inout vec4 samplePos, inout float len) {'
+#10'	float cdot = dot(dir,clipPlane.xyz);'
+#10'	if  ((clipPlane.a > 1.0) || (cdot == 0.0)) return samplePos;'
+#10'	bool frontface = (cdot > 0.0);'
+#10'	float dis = (-clipPlane.a - dot(clipPlane.xyz, samplePos.xyz-0.5)) / cdot;'
+#10'	float  disBackFace = (-(clipPlane.a-clipThick) - dot(clipPlane.xyz, samplePos.xyz-0.5)) / cdot;'
+#10'	if (((frontface) && (dis >= len)) || ((!frontface) && (dis <= 0.0))) {'
+#10'		samplePos.a = len + 1.0;'
+#10'		return samplePos;'
+#10'	}'
+#10'	if (frontface) {'
+#10'		dis = max(0.0, dis);'
+#10'		samplePos = vec4(samplePos.xyz+dir * dis, dis);'
+#10'		len = min(disBackFace, len);'
+#10'	}'
+#10'	if (!frontface) {'
+#10'		len = min(dis, len);'
+#10'		disBackFace = max(0.0, disBackFace);'
+#10'		samplePos = vec4(samplePos.xyz+dir * disBackFace, disBackFace);'
+#10'	}'
+#10'	return samplePos;'
+#10'}';

kFragFaster = kFragBase
 +#10'vec4 texture3Df(sampler3D vol, vec3 coord) {'
 +#10'	return texture3D(vol, coord); //trilinear interpolation'
 +#10'}';

kFragBetter = kFragBase
+#10'vec4 texture3Df(sampler3D vol, vec3 coord) {'
+#10'  // shift the coordinate from [0,1] to [-0.5, textureSz-0.5]'
+#10'  vec3 coord_grid = coord * textureSz - 0.5;'
+#10'  vec3 index = floor(coord_grid);'
+#10'  vec3 fraction = coord_grid - index;'
+#10'  vec3 one_frac = 1.0 - fraction;'
+#10'  vec3 w0 = 1.0/6.0 * one_frac*one_frac*one_frac;'
+#10'  vec3 w1 = 2.0/3.0 - 0.5 * fraction*fraction*(2.0-fraction);'
+#10'  vec3 w2 = 2.0/3.0 - 0.5 * one_frac*one_frac*(2.0-one_frac);'
+#10'  vec3 w3 = 1.0/6.0 * fraction*fraction*fraction;'
+#10'  vec3 g0 = w0 + w1;'
+#10'  vec3 g1 = w2 + w3;'
+#10'  vec3 mult = 1.0 / textureSz;'
+#10'  vec3 h0 = mult * ((w1 / g0) - 0.5 + index);  //h0 = w1/g0 - 1, move from [-0.5, textureSz-0.5] to [0,1]'
+#10'  vec3 h1 = mult * ((w3 / g1) + 1.5 + index);  //h1 = w3/g1 + 1, move from [-0.5, textureSz-0.5] to [0,1]'
+#10'  // fetch the eight linear interpolations'
+#10'  // weighting and fetching is interleaved for performance and stability reasons'
+#10'  vec4 tex000 =  texture3D(vol,h0);'
+#10'  vec4 tex100 =  texture3D(vol,vec3(h1.x, h0.y, h0.z));'
+#10'  tex000 = mix(tex100, tex000, g0.x);  //weigh along the x-direction'
+#10'  vec4 tex010 =  texture3D(vol,vec3(h0.x, h1.y, h0.z));'
+#10'  vec4 tex110 =  texture3D(vol,vec3(h1.x, h1.y, h0.z));'
+#10'  tex010 = mix(tex110, tex010, g0.x);  //weigh along the x-direction'
+#10'  tex000 = mix(tex010, tex000, g0.y);  //weigh along the y-direction'
+#10'  vec4 tex001 =  texture3D(vol,vec3(h0.x, h0.y, h1.z));'
+#10'  vec4 tex101 =  texture3D(vol,vec3(h1.x, h0.y, h1.z));'
+#10'  tex001 = mix(tex101, tex001, g0.x);  //weigh along the x-direction'
+#10'  vec4 tex011 =  texture3D(vol,vec3(h0.x, h1.y, h1.z));'
+#10'  vec4 tex111 =  texture3D(vol,h1);'
+#10'  tex011 = mix(tex111, tex011, g0.x);  //weigh along the x-direction'
+#10'  tex001 = mix(tex011, tex001, g0.y);  //weigh along the y-direction'
+#10'  return mix(tex001, tex000, g0.z);  //weigh along the z-direction'
+#10'}';

kFrag = kFragFaster
+#10'uniform float overlayFuzzy = 0.5;'
+#10'uniform float overlayDepth = 0.3;'
+#10'uniform float overlayClip = 0.0;'
+#10''
+#10'void main() {'
+#10'    vec3 start = TexCoord1.xyz;'
+#10'	vec3 backPosition = GetBackPosition(start);'
+#10'	vec3 dir = backPosition - start;'
+#10'	float len = length(dir);'
+#10'	dir = normalize(dir);'
+#10'	vec4 deltaDir = vec4(dir.xyz * stepSize, stepSize);'
+#10'	vec4 colorSample;'
+#10'	float bgNearest = len; //assume no hit'
+#10'	float overFarthest = len;'
+#10'	vec4 colAcc = vec4(0.0,0.0,0.0,0.0);'
+#10'	vec4 samplePos;'
+#10'	//background pass'
+#10'	float noClipLen = len;'
+#10'	samplePos = vec4(start.xyz +deltaDir.xyz* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453)), 0.0);'
+#10'	vec4 clipPos = applyClip(dir, samplePos, len);'
+#10'	float stepSizeX2 = samplePos.a + (stepSize * 2.0);'
+#10'	while (samplePos.a <= len) {'
+#10'		colorSample = texture3Df(intensityVol,samplePos.xyz);'
+#10'		if (colorSample.a > 0.0) {'
+#10'			colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);'
+#10'			bgNearest = min(samplePos.a,bgNearest);'
+#10'			if (samplePos.a < stepSizeX2)'
+#10'				colorSample.a = clamp(colorSample.a*3.0,0.0, 1.0);'
+#10'			colorSample.rgb *= colorSample.a;'
+#10'			colAcc= (1.0 - colAcc.a) * colorSample + colAcc;'
+#10'			if ( colAcc.a > 0.95 )'
+#10'				break;'
+#10'		}'
+#10'		samplePos += deltaDir;'
+#10'	} //while samplePos.a < len'
+#10'	colAcc.a = colAcc.a/0.95;'
+#10'	colAcc.a *= backAlpha;'
+#10'	#if ( __VERSION__ > 300 )'
+#10'	if ( overlays < 1 ) {'
+#10'		FragColor = colAcc;'
+#10'		return;'
+#10'	}'
+#10'	#else'
+#10'	if ((textureSz.x < 1) || ( overlays < 1 )) {'
+#10'		gl_FragColor = colAcc;'
+#10'		return;'
+#10'	}'
+#10'	#endif'
+#10'	'
+#10'	//overlay pass'
+#10'	vec4 overAcc = vec4(0.0,0.0,0.0,0.0);'
+#10'	if (overlayClip > 0)'
+#10'		samplePos = clipPos;'
+#10'	else {'
+#10'		len = noClipLen;'
+#10'		samplePos = vec4(start.xyz +deltaDir.xyz* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453)), 0.0);'
+#10'	}'
+#10'	while (samplePos.a <= len) {'
+#10'		colorSample = texture3Df(intensityOverlay,samplePos.xyz);'
+#10'		if (colorSample.a > 0.00) {'
+#10'			colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);'
+#10'			colorSample.a *=  overlayFuzzy;'
+#10'			overFarthest = samplePos.a;'
+#10'			colorSample.rgb *= colorSample.a;'
+#10'			overAcc= (1.0 - overAcc.a) * colorSample + overAcc;'
+#10'			if (overAcc.a > 0.95 )'
+#10'				break;'
+#10'		}'
+#10'		samplePos += deltaDir;'
+#10'	} //while samplePos.a < len'
+#10'	overAcc.a = overAcc.a/0.95;'
+#10'	//end ovelay pass clip plane applied to background ONLY...'
+#10'	//if (overAcc.a > 0.0) { //<- conditional not required: overMix always 0 for overAcc.a = 0.0'
+#10'		float overMix = overAcc.a;'
+#10'		if (((overFarthest) > bgNearest) && (colAcc.a > 0.0)) { //background (partially) occludes overlay'
+#10'			float dx = (overFarthest - bgNearest)/1.73;'
+#10'			dx = colAcc.a * pow(dx, overlayDepth);'
+#10'			overMix *= 1.0 - dx;'
+#10'		}'
+#10'		colAcc.rgb = mix(colAcc.rgb, overAcc.rgb, overMix);'
+#10'		colAcc.a = max(colAcc.a, overAcc.a);'
+#10'	//}'
+#10'	#if ( __VERSION__ > 300 )'
+#10'	FragColor = colAcc;'
+#10'	#else'
+#10'	gl_FragColor = colAcc;'
+#10'	#endif'
+#10'}';

{$ENDIF}
procedure TGPUVolume.SetShader(shaderName: string);
var
  VertexProgram, FragmentProgram: string;
  i : integer;
begin
  glControl.MakeCurrent();
  glUseProgram(0);
  if (programRaycastBetter <> 0) then glDeleteProgram(programRaycastBetter);
  loadVertFrag(shaderName, VertexProgram, FragmentProgram, kFragBetter);
  if VertexProgram = '' then VertexProgram := kVert;
  if FragmentProgram = '' then FragmentProgram := kFrag;
  programRaycastBetter :=  initVertFrag(VertexProgram, FragmentProgram);
  if (programRaycast <> 0) then glDeleteProgram(programRaycast);
  loadVertFrag(shaderName, VertexProgram, FragmentProgram, kFragFaster);
  if VertexProgram = '' then VertexProgram := kVert;
  if FragmentProgram = '' then FragmentProgram := kFrag;
  programRaycast :=  initVertFrag(VertexProgram, FragmentProgram);
  //imvLoc := glGetUniformLocation(programRaycast, pAnsiChar('ModelViewMatrixInverse'));
  mvpLoc := glGetUniformLocation(programRaycast, pAnsiChar('ModelViewProjectionMatrix'));
  rayDirLoc := glGetUniformLocation(programRaycast, pAnsiChar('rayDir'));
  sliceSizeLoc := glGetUniformLocation(programRaycast, pAnsiChar('sliceSize'));
  stepSizeLoc := glGetUniformLocation(programRaycast, pAnsiChar('stepSize'));
  backAlphaLoc := glGetUniformLocation(programRaycast, pAnsiChar('backAlpha'));
  matcap1Loc := glGetUniformLocation(programRaycast, pAnsiChar('matcap2D'));
  matcap2Loc := glGetUniformLocation(programRaycast, pAnsiChar('matcap2D2'));
  {$IFDEF MATCAP}
  normLoc := glGetUniformLocation(programRaycast, pAnsiChar('NormalMatrix'));
  //printf(format('%d %s--->matcap @ %d = %d', [normLoc, shaderName, matcapLoc, matcap2D]));
  {$ENDIF}
  {$IFNDEF COREGL}
  textureSzLoc := glGetUniformLocation(programRaycast, pAnsiChar('textureSz'));
  {$ENDIF}
  //loopsLoc := glGetUniformLocation(programRaycast, pAnsiChar('loops'));
  overlaysLoc := glGetUniformLocation(programRaycast, pAnsiChar('overlays'));
  lightPositionLoc := glGetUniformLocation(programRaycast, pAnsiChar('lightPosition'));
  clipPlaneLoc :=  glGetUniformLocation(programRaycast, pAnsiChar('clipPlane'));
  clipThickLoc :=  glGetUniformLocation(programRaycast, pAnsiChar('clipThick'));
  intensityVolLoc := glGetUniformLocation(programRaycast, pAnsiChar('intensityVol'));
  gradientVolLoc := glGetUniformLocation(programRaycast, pAnsiChar('gradientVol'));
  overlayIntensityVolLoc := glGetUniformLocation(programRaycast, pAnsiChar('intensityOverlay'));
  overlayGradientVolLoc := glGetUniformLocation(programRaycast, pAnsiChar('gradientOverlay'));
  shaderPrefs := loadShaderPrefs(shaderName);
  if (shaderPrefs.nUniform > 0) and (shaderPrefs.nUniform <= kMaxUniform) then
     for i := 1 to shaderPrefs.nUniform do
         prefLoc[i] := glGetUniformLocation(programRaycast, pAnsiChar(shaderPrefs.Uniform[i].name));
  if GLErrorStr <>  '' then begin
   glControl.ReleaseContext;
   {$IFDEF UNIX}
   printf(GLErrorStr);
   {$ENDIF}
   showmessage(GLErrorStr);
   GLErrorStr := '';
  end;
end;

procedure TGPUVolume.CreateDrawColorTable;//1D texture for drawing
var
  i: integer;
  colorLut: array [0..255] of TRGBA;
begin
  if (drawTexture1D <> 0) then glDeleteTextures(1,@drawTexture1D);
  glGenTextures(1, @drawTexture1D);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glBindTexture(GL_TEXTURE_1D, drawTexture1D);
  glTexParameterf(GL_TEXTURE_1D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameterf(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameterf(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
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
  glTexImage1D(GL_TEXTURE_1D, 0, GL_RGBA, 256, 0, GL_RGBA, GL_UNSIGNED_BYTE, @colorLut[0]);
end;

//{$DEFINE FX}
procedure TGPUVolume.Prepare(shaderName: string);
var
 success: boolean;
 isMesa: boolean = false;
 max3D: glInt;
{$IFDEF FX}
fnm, VertexProgram, FragmentProgram: string;
{$ENDIF}
begin
  glControl.MakeCurrent();
  if (shaderName = '') or (not fileexists(shaderName)) then
  shaderName := ShaderDir+pathdelim+'Default.glsl';
  if (not fileexists(shaderName)) then
     shaderName := '';
  SetShader(shaderName);
  glGetIntegerv(GL_MAX_3D_TEXTURE_SIZE, @max3D);
  isMesa := (max3D < 257);
  if isMesa then begin
     printf('Compromised GL_MAX_3D_TEXTURE_SIZE detected. Slower gradients will be used and Draw menu functions may not work');
     {$IFDEF GPUGRADIENTS}
     programBlur := 0;
     programSobel := 0;
     {$ENDIF}
  end;
  {$IFDEF GPUGRADIENTS}
  if not isMesa then begin
     programBlur := initVertFrag(kBlurSobelVert,kBlurFrag);
     programSobel := initVertFrag(kBlurSobelVert,kSobelFrag);
  end;
  {$ENDIF}
  {$IFDEF VIEW2D}
  colorEditor := TColorEditor.Create();
  //ShowColorEditor := true;
  //line program
  programLine2D := initVertFrag(kVertLine2D,kFragLine2D);
  //glUseProgram(programLine2D);
  uniform_viewportSizeLine := glGetUniformLocation(programLine2D, pAnsiChar('ViewportSize'));
  {$IFDEF LINE3D}
  programLine3D := initVertFrag(kVertLine3D,kFragLine3D);
  mvpLine3DLoc := glGetUniformLocation(programLine3D, pAnsiChar('ModelViewProjectionMatrix'));
  colorLine3DLoc := glGetUniformLocation(programLine3D, pAnsiChar('Color'));
  {$IFDEF COREGL}
  //setlength(gLines3D,6); //3D crosshair is three lines, six vertices
  {$ENDIF} //COREGL

  {$ENDIF} //LINE3D
  {$IFDEF COREGL}
  //setup VAO for Tex
  vboTex2D := 0;
  vaoTex2D := 0;
  vboBox3D := 0;
  glGenVertexArrays(1, @vaoTex2D);
  vboTex2D := 0;
  glGenBuffers(1, @vboTex2D);
  glBindVertexArray(vaoTex2D);
  glBindBuffer(GL_ARRAY_BUFFER, vboTex2D);
  //Vertices
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(TVertex2D), PChar(0));
  glEnableVertexAttribArray(0);
  //Texture Coordinates
  glVertexAttribPointer(4, 4, GL_FLOAT, GL_FALSE, sizeof(TVertex2D), PChar( 4 * sizeof(single)));
  glEnableVertexAttribArray(4);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);  //required, even if we will bind it next
  {$ELSE}
  dlTex2D := 0; //display list empty
  dlBox3D := 0;
  dlColorEditor := 0;
  {$ENDIF}
  //2D program
  {$IFDEF FX}
  fnm := '/Users/chris/fx.glsl';
  if fileexists(fnm) then begin
    loadVertFrag(fnm, VertexProgram, FragmentProgram);
    writeln('>>>>>>>>');
    programTex2D := initVertFrag(VertexProgram,FragmentProgram);
  end else
  {$ENDIF}
  programTex2D := initVertFrag(kVertTex2D,kFragTex2D);
  //glUseProgram(programTex2D);
  uniform_tex := glGetUniformLocation(programTex2D, pAnsiChar('tex'));
  uniform_overlay := glGetUniformLocation(programTex2D, pAnsiChar('overlay'));
  uniform_drawTex := glGetUniformLocation(programTex2D, pAnsiChar('drawTex'));
  uniform_drawLUT := glGetUniformLocation(programTex2D, pAnsiChar('drawLUT'));
  uniform_drawAlpha := glGetUniformLocation(programTex2D, pAnsiChar('drawAlpha'));
  uniform_backAlpha := glGetUniformLocation(programTex2D, pAnsiChar('backAlpha'));
  uniform_viewportSizeTex := glGetUniformLocation(programTex2D, pAnsiChar('ViewportSize'));
  uniform_texVox := glGetUniformLocation(programTex2D, pAnsiChar('texSize'));
  uniform_overlaysLoc := glGetUniformLocation(programTex2D, pAnsiChar('overlays'));
  CreateDrawTex(pti(4,4,4), nil);
  CreateDrawColorTable;
  {$IFDEF LINE3D}
  vboLine3D := 0;
  vboLineIdx3D := 0;
  {$ENDIF}
  {$IFDEF COREGL}
  //setup VAO for lines
  vboLine2D := 0;
  vaoLine2D := 0;
  glGenVertexArrays(1, @vaoLine2D);
  glGenBuffers(1, @vboLine2D);
  //glBindBuffer(GL_ARRAY_BUFFER, vboLine2D);
  //glBufferData(GL_ARRAY_BUFFER,slices2D.NumberOfLineVertices*SizeOf(TVertex2D), @slices2D.LineVertices[0], GL_STATIC_DRAW);
  //glBindBuffer(GL_ARRAY_BUFFER, 0);
  // Prepare vertrex array object (VAO)
  glBindVertexArray(vaoLine2D);
  glBindBuffer(GL_ARRAY_BUFFER, vboLine2D);
  //Vertices
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(TVertex2D), PChar(0));
  glEnableVertexAttribArray(0);
  //Texture Coordinates
  glVertexAttribPointer(4, 4, GL_FLOAT, GL_FALSE, sizeof(TVertex2D), PChar( 4 * sizeof(single)));
  glEnableVertexAttribArray(4);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);  //required, even if we will bind it next
  {$IFDEF LINE3D}
  //setup VAO for lines
  glGenBuffers(1, @vboLine3D);
  vaoLine3D := 0;
  glGenVertexArrays(1, @vaoLine3D);
  // Prepare vertrex array object (VAO)
  glBindVertexArray(vaoLine3D);
  glBindBuffer(GL_ARRAY_BUFFER, vboLine3D);
  //Vertices
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(TVec3), PChar(0));
  glEnableVertexAttribArray(0);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);  //required, even if we will bind it next

  {$ENDIF}//LINE3D
  {$ELSE}
  dlLine2D := 0;
  {$ENDIF}
  Txt := TGPUFont.Create(ResourceDir+pathdelim+'Roboto.png',  success, glControl); //<-multi-channel channel fonts glmtext
  slices2D := TSlices2D.Create(Txt);
  //slices2D := TSlices2D.Create();
  {$ENDIF}
  LoadCube();
  {$IFDEF CUBE}
  gCube := TGPUCube.Create(glControl);
  if (Screen.PixelsPerInch < 100) then
     gCube.Size:= gCube.Size * 1.5; //Check Darwin Retina vs Windows HiDPI
  {$ENDIF}
  glControl.ReleaseContext;
  if GLErrorStr <> '' then
     showmessage(GLErrorStr);
end;

{$IFDEF CLRBAR}
procedure TGPUVolume.SetColorBar(fromColorbar: TGPUClrbar);
begin
     clrbar := fromColorbar;
end;
{$ENDIF}

procedure TGPUVolume.SetGradientMode(newMode: integer);
begin
  gradientMode := kGradientModeGPUSlow;
  if (newMode >= kGradientModeGPUFastest) and (newMode <= kGradientModeCPUSlowest) then
     gradientMode := newMode;
  //gradientMode := kGradientModeCPUSlowest;
end;

constructor TGPUVolume.Create(fromView: TOpenGLControl);
begin
  glControl := fromView;
  gradientMode := kGradientModeGPUSlow;
  {$IFDEF CLRBAR}clrbar := nil;{$ENDIF}
  {$IFDEF VIEW2D}
  slices2D := nil;
  SelectionRect := Vec4(-1,0,0,0);
  colorEditorVisible := false;
  isSmooth2D := true;
  {$ENDIF}
  fDistance := kDefaultDistance;
  fAzimuth := 110;
  fElevation := 30;
  fPitch := 0;
  {$IFDEF MTX}
  fModelMatrix := TMat4.Identity;
  fModelMatrix *= TMat4.Translate(0, 0, -fDistance);
  fModelMatrix *= TMat4.RotateY(-DegToRad(32));
  fModelMatrix *= TMat4.RotateX(-DegToRad(90-fElevation));
  fModelMatrix *= TMat4.RotateZ(DegToRad(fAzimuth));
  {$ENDIF}
  overlayNum := 0;
  overlayGradTexWidth := 2; //refresh
  RaycastQuality1to5 := 5;
  fLightPos := Vec4(0, 0.707, 0.707, 0);
  ClipThick := 1;
  fClipPlane := Vec4(0, 0, 0, -1);
  {$IFDEF COREGL}
  vao:= 0;
  {$ELSE}
  dlTex2D := 0;
  dlLine2D := 0;
  dlBox3D := 0;
  dlColorEditor := 0;
  {$ENDIF}
  matcap2D1 := 0;
  matcap2D2 := 0;
  overlayGradientTexture3D := 0;
  overlayIntensityTexture3D := 0;
  drawTexture1D := 0;
  drawTexture3D := 0;
  gradientTexture3D := 0;
  intensityTexture3D := 0;
  shaderPrefs.nUniform:= 0;
  programRaycast := 0;
  programRaycastBetter := 0;
end;

procedure TGPUVolume.LoadCube();
var
  vtx : packed array[0..23] of GLfloat = (
      0,0,0,
      0,1,0,
      1,1,0,
      1,0,0,
      0,0,1,
      0,1,1,
      1,1,1,
      1,0,1
      ); //vtx = 8 vertex positions (corners) of cube
  {$IFDEF STRIP}
  //https://stackoverflow.com/questions/28375338/cube-using-single-gl-triangle-strip
  idx : packed array[0..13] of GLuint = (0,1,3,2,6,1,5,4, 6,7,3, 4, 0, 1); //reversed winding
  //idx : packed array[0..13] of GLuint = (1,0,4,3,7,6,4,5,1,6,2,3,1,0);
  {$ELSE}
  idx : packed array[0..35] of GLuint = (
      0,2,1,
      0,3,2,
      4,5,6,
      4,6,7,
      0,1,5,
      0,5,4,
      3,6,2,
      3,7,6,
      1,6,5,
      1,2,6,
      0,4,7,
      0,7,3
      ); //idx = each cube has 6 faces, each composed of two triangles = 12 tri indices
{$ENDIF}
{$IFDEF COREGL}
vbo_point: gluint;
{$ELSE}
i, nface, v: integer;
v3: TVec3;
{$ENDIF}
begin  //vboCube, vaoCube,
  {$IFDEF COREGL}
  vbo_point := 0;
  vao := 0;
  glGenBuffers(1, @vbo_point);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  glBufferData(GL_ARRAY_BUFFER, 8*3*sizeof(GLfloat), @vtx[0], GL_STATIC_DRAW); //cube has 8 vertices, each 3 coordinates X,Y,Z
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glGenVertexArrays(1, @vao);
  // vao like a closure binding 3 buffer object: verlocdat vercoldat and veridxdat
  glBindVertexArray(vao);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(GLfloat), nil);
  glEnableVertexAttribArray(0); // for vertexloc
  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(GLfloat), nil);
  glEnableVertexAttribArray(1); // for vertexcol
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glGenBuffers(1, @vboBox3D);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboBox3D);
  {$IFDEF STRIP}
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, 14*sizeof(GLuint), @idx[0], GL_STATIC_DRAW); //cube is 6 faces, 2 triangles per face, 3 indices per triangle
  {$ELSE}
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, 36*sizeof(GLuint), @idx[0], GL_STATIC_DRAW); //cube is 6 faces, 2 triangles per face, 3 indices per triangle
  {$ENDIF}
  //do not delete the VBOs! http://stackoverflow.com/questions/25167562/how-to-dispose-vbos-stored-in-a-vao
  {$ELSE}
  //Legacy OpenGL:
  if not dlBox3D <> 0 then
     glDeleteLists(dlBox3D, 1);
  dlBox3D := glGenLists(1);
  glNewList(dlBox3D, GL_COMPILE);
  {$IFDEF STRIP}
  glBegin(GL_TRIANGLE_STRIP);
  nface := 14;
  {$ELSE}
  glBegin(GL_TRIANGLES);
  nface := 36;
  {$ENDIF}
  for i := 0 to nface-1 do begin
      v := idx[i];
      v3.x := vtx[v*3];
      v3.y := vtx[(v*3)+1];
      v3.z := vtx[(v*3)+2];
      glVertex3f(v3.x, v3.y, v3.z);
  end;
  glEnd();
  glEndList();
  {$ENDIF}
end;

procedure TGPUVolume.CreateOverlayTextures(Dim: TVec3i; volRGBA: TRGBAs);
var
{$IFDEF DYNRGBA}
gradData: TRGBAs;
{$ELSE}
gradData: TRGBAp = nil;
{$ENDIF}
begin
  //GLForm1.LayerBox.Caption := ':>>'+inttostr(random(888));
  if (overlayIntensityTexture3D <> 0) then glDeleteTextures(1,@overlayIntensityTexture3D);
  if (overlayGradientTexture3D <> 0) then glDeleteTextures(1,@overlayGradientTexture3D);
  GetErrorAll(101,'OverlayTexture');
  if (volRGBA = nil) then begin
    Dim := pti(1,1,1);
    setlength(volRGBA,1);
    volRGBA[0] := setrgba(0,0,0,0);
    overlayNum := 0; //perhaps overlays loaded but made transparent
  end;
  glPixelStorei(GL_UNPACK_ALIGNMENT,1);
 glGenTextures(1, @overlayIntensityTexture3D);
 glBindTexture(GL_TEXTURE_3D, overlayIntensityTexture3D);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_BORDER);
 glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA, Dim.X, Dim.Y, Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE, @volRGBA[0]);
 GetErrorAll(103,'OverlayTexture');
 //if skipGradientCalc then exit; //faster - for real time drawing
 glPixelStorei(GL_UNPACK_ALIGNMENT,1);
 glGenTextures(1, @overlayGradientTexture3D);
 glBindTexture(GL_TEXTURE_3D, overlayGradientTexture3D);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_BORDER);
 //startTime := Now;
  if (Dim.X > 1) then begin
    {$IFDEF GPUGRADIENTS}
    if (gradientMode <> kGradientModeCPUSlowest) and (programSobel <> 0) and (programSobel <> 0) then begin
       {$IFDEF DYNRGBA}
	   SetLength (gradData, Dim.X*Dim.Y*Dim.Z);
       {$ELSE}
	   SetLengthP (gradData, Dim.X*Dim.Y*Dim.Z);
       {$ENDIF}
       glTexImage3D(GL_TEXTURE_3D, 0,GL_RGBA, Dim.X, Dim.Y, Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE,@gradData[0]);
       {$IFDEF DYNRGBA}
	   gradData := nil;
       {$ELSE}
	   Freemem(gradData);
       {$ENDIF}
       CreateGradientVolumeGPU (Dim.X, Dim.Y, Dim.Z, overlayIntensityTexture3D, overlayGradientTexture3D);
    end else begin
    {$ENDIF}
      //Calculate gradients on the CPU
      //Vol.CreateGradientVolume (volRGBA, Dim.X, Dim.Y, Dim.Z, gradData);
      CreateGradientVolumeX (TUInt8s(volRGBA), Dim.X, Dim.Y, Dim.Z, 1, gradData);
      glTexImage3D(GL_TEXTURE_3D, 0,GL_RGBA, Dim.X, Dim.Y, Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE,@gradData[0]);
      {$IFDEF DYNRGBA}
          gradData := nil;
      {$ELSE}
          Freemem(gradData);
      {$ENDIF}
      //Form1.Caption := 'CPU gradients '+inttostr(MilliSecondsBetween(Now,startTime))+' ms ';
    {$IFDEF GPUGRADIENTS}
    end;
    {$ENDIF}
  end else
      glTexImage3D(GL_TEXTURE_3D, 0,GL_RGBA, Dim.X, Dim.Y, Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE,@volRGBA[0]);
 overlayGradTexWidth := Dim.X;
 GetErrorAll(104,'OverlayGradient');
 //GLForm1.LayerBox.Caption := ':>>'+inttostr(random(888));
 volRGBA := nil; //free
end;

procedure TGPUVolume.UpdateOverlays(vols: TNIfTIs);
var
  intensityData: TRGBAs;
  Vol: TNIfTI;
begin
  if not vols.Layer(0,Vol) then exit;
  glControl.MakeCurrent();
  overlayNum := vols.NumLayers -1; //-1 as we ignore background
  if (overlayNum < 1)  then begin //background only
    if (overlayGradientTexture3D = 0) or (overlayGradTexWidth > 1) then //e.g. update texture after user closes overlays
       CreateOverlayTextures(Vol.Dim, nil); // <- empty overlay texture
    exit; //no overlays
  end;
  if not vols.OverlaysNeedsUpdate then exit;
  intensityData := vols.CreateOverlayVolume;
  CreateOverlayTextures(Vol.Dim, intensityData);
end;

{$IFDEF GPUGRADIENTS}
procedure TGPUVolume.GenerateGradient(var inTex, grTex: GLuint);
var
  w, h, d: GLint;
  gradData: TRGBAs;
begin
  if grTex <> 0 then exit;
  if inTex = 0 then exit;
  glBindTexture(GL_TEXTURE_3D, inTex);
  glGetTexLevelParameteriv(GL_TEXTURE_3D, 0, GL_TEXTURE_WIDTH, @w);
  glGetTexLevelParameteriv(GL_TEXTURE_3D, 0, GL_TEXTURE_HEIGHT, @h);
  glGetTexLevelParameteriv(GL_TEXTURE_3D, 0, GL_TEXTURE_DEPTH, @d);
  if (w < 1) or (h < 1) or (d < 1) then exit;
  glPixelStorei(GL_UNPACK_ALIGNMENT,1);
  glGenTextures(1, @grTex);
  glBindTexture(GL_TEXTURE_3D, grTex);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_BORDER);
  glTexImage3D(GL_PROXY_TEXTURE_3D, 0, GL_RGBA, w, h, d, 0, GL_RGBA, GL_UNSIGNED_BYTE, NIL);
  glGetTexLevelParameteriv(GL_PROXY_TEXTURE_3D, 0, GL_TEXTURE_WIDTH, @w);
  glGetTexLevelParameteriv(GL_PROXY_TEXTURE_3D, 0, GL_TEXTURE_HEIGHT, @h);
  glGetTexLevelParameteriv(GL_PROXY_TEXTURE_3D, 0, GL_TEXTURE_DEPTH, @d);
  {$IFDEF UNIX}printf(format('gradientTexture3D proxy test %dx%dx%d',[w, h, d]));{$ENDIF}
   if (w < 1) then begin
      printf(format('Gradient texture error (%dx%dx%d). Solution: adjust "MaxVox" or press "Reset" button in preferences.', [w, h, d]));
      glControl.ReleaseContext;
      {$IFNDEF LCLCocoa}
      showmessage('Image too large. Try adjusting "MaxVox" or press "Reset" button in preferences.');
      {$ENDIF}
      exit;
   end;
  SetLength (gradData, w*h*d);
  //glFinish();//<<
  glTexImage3D(GL_TEXTURE_3D, 0,GL_RGBA, w, h, d, 0, GL_RGBA, GL_UNSIGNED_BYTE,@gradData[0]);
  GetErrorAll(107,'TextureGradient');
  //glFinish();//<<
  gradData := nil;
  CreateGradientVolumeGPU (w, h, d, inTex, grTex);
  GetErrorAll(108,'TextureGradient'); //1286
end;
{$ENDIF}

function TGPUVolume.LoadTexture(var vol: TNIfTI; deferGradients: boolean): boolean;
label
  123;
var
 //i,j: int64;
 width, height, depth: GLint;
 {$IFDEF DYNRGBA}
 gradData: TRGBAs;
 {$ELSE}
  gradData: TRGBAp = nil;
  {$ENDIF}
 {$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
begin
 result := false;
 glControl.MakeCurrent();
 {$IFDEF GPUGRADIENTS}
 //see if gradients were previously deferred but now required
 if (Vol.VolRGBA = nil) and (gradientTexture3D = 0) and (not deferGradients) then GenerateGradient(intensityTexture3D, gradientTexture3D);
 {$ENDIF}
 if (Vol.VolRGBA = nil) then exit;
 if (intensityTexture3D <> 0) then glDeleteTextures(1,@intensityTexture3D);
 if (gradientTexture3D <> 0) then begin
    glDeleteTextures(1,@gradientTexture3D);
    gradientTexture3D := 0;
 end;
 //next: see if our video card can show this intensity texture
 glTexImage3D(GL_PROXY_TEXTURE_3D, 0, GL_RGBA, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE, NIL);
 glGetTexLevelParameteriv(GL_PROXY_TEXTURE_3D, 0, GL_TEXTURE_WIDTH, @width);
 glGetTexLevelParameteriv(GL_PROXY_TEXTURE_3D, 0, GL_TEXTURE_HEIGHT, @height);
 glGetTexLevelParameteriv(GL_PROXY_TEXTURE_3D, 0, GL_TEXTURE_DEPTH, @depth);
 //https://www.opengl.org/archives/resources/faq/technical/texture.htm
 // printf(format('intensityTexture3D proxy test %dx%dx%d',[width, height, depth]));
 if (width < 1) then begin
    printf(format('Unable to create large intensity texture (%dx%dx%d). Solution: adjust "MaxVox" or press "Reset" button in preferences.', [Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z]));
    glControl.ReleaseContext;
    {$IFNDEF LCLCocoa}
    showmessage('Image too large. Try adjusting "MaxVox" or press "Reset" button in preferences.');
    {$ENDIF}
    exit;
 end;
 //next copy the image to the GPU
 glPixelStorei(GL_UNPACK_ALIGNMENT,1);
 glGenTextures(1, @intensityTexture3D);
 glBindTexture(GL_TEXTURE_3D, intensityTexture3D);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_BORDER);
 glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE, @Vol.VolRGBA[0]);
 {$IFDEF GPUGRADIENTS}
 if (gradientMode <> kGradientModeCPUSlowest) and (programSobel <> 0) and (programBlur <> 0) then begin
   if not deferGradients then
      GenerateGradient(intensityTexture3D, gradientTexture3D);
   goto 123;
 end;
 {$ENDIF}
 GetErrorAll(106,'LoadTexture');
 glPixelStorei(GL_UNPACK_ALIGNMENT,1);
 glGenTextures(1, @gradientTexture3D);
 glBindTexture(GL_TEXTURE_3D, gradientTexture3D);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_BORDER);
 //startTime := Now;
 //next: see if our video card can show this gradient texture
 glTexImage3D(GL_PROXY_TEXTURE_3D, 0, GL_RGBA, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE, NIL);
 glGetTexLevelParameteriv(GL_PROXY_TEXTURE_3D, 0, GL_TEXTURE_WIDTH, @width);
 glGetTexLevelParameteriv(GL_PROXY_TEXTURE_3D, 0, GL_TEXTURE_HEIGHT, @height);
 glGetTexLevelParameteriv(GL_PROXY_TEXTURE_3D, 0, GL_TEXTURE_DEPTH, @depth);
 //https://www.opengl.org/archives/resources/faq/technical/texture.htm
 {$IFDEF UNIX}printf(format('gradientTexture3D proxy test %dx%dx%d',[width, height, depth]));{$ENDIF}
 if (width < 1) then begin
    printf(format('Gradient texture error (%dx%dx%d). Solution: adjust "MaxVox" or press "Reset" button in preferences.', [Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z]));
    glControl.ReleaseContext;
    {$IFNDEF LCLCocoa}
    showmessage('Image too large. Try adjusting "MaxVox" or press "Reset" button in preferences.');
    {$ENDIF}
    exit;
 end;
 (*{$IFDEF GPUGRADIENTS}
    if (gradientMode <> kGradientModeCPUSlowest) then begin
       SetLength (gradData, Vol.Dim.X*Vol.Dim.Y*Vol.Dim.Z);
       //glFinish();//<<
       glTexImage3D(GL_TEXTURE_3D, 0,GL_RGBA, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE,@gradData[0]);
       GetErrorAll(107,'TextureGradient');
       //glFinish();//<<
       gradData := nil;
       CreateGradientVolumeGPU (Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, intensityTexture3D, gradientTexture3D);
       GetErrorAll(108,'TextureGradient'); //1286
 end else begin
 {$ENDIF}*)
  {$IFDEF TIMER}startTime := now;{$ENDIF}
   gradData := Vol.GenerateGradientVolume;
   {$IFDEF TIMER}printf(format('CPU Gradient time %d',[MilliSecondsBetween(Now,startTime)]));{$ENDIF}
   //CreateGradientVolume (Vol.VolRGBA, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, gradData);
   glTexImage3D(GL_TEXTURE_3D, 0,GL_RGBA, Vol.Dim.X, Vol.Dim.Y,Vol.Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE,@gradData[0]);
   gradData := nil;
   //Form1.Caption := 'CPU gradients '+inttostr(MilliSecondsBetween(Now,startTime))+' ms ';
 (*{$IFDEF GPUGRADIENTS}
 end;
 {$ENDIF} *)
 GetErrorAll(109,'TextureGradient');
123:
 maxDim := max(Vol.Dim.X,max(Vol.Dim.Y,Vol.Dim.Z));
 Vol.GPULoadDone;
 if (overlayIntensityTexture3D = 0) then CreateOverlayTextures(Vol.Dim, nil); //load blank overlay
 result := true;
end;       //deferGradients

procedure addFuzz (var v: TVec4); //avoid shader divide by zero error
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
  q : integer;
  f: single;
begin
  //if (i = 0) then i := 2; //dynamic (1=0.4, 2=0.55 ... 5=1.0
  q := Quality1to5 - 1;
  q := max(q,0);
  q := min(q,4); //quality 5 is same as 4 but adds cubic sampling
  f := lerp(slices*0.4,slices*1.0, q/4); //0.4..1.0
  //writeln(format('%g %g %g -> %g', [slices*0.4, slices*1.0, q/4.0, f]));
  if f < 10 then
    f := 10;
  //writeln(format('%d %d-> %g', [Quality1to5, Slices, 1/f]));
  result := 1/f;
end;
(*function ComputeStepSize (Quality1to10, Slices: integer): single;
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
end; *)
{$IFDEF VIEW2D}
function TGPUVolume.Slice2Dmm(var vol: TNIfTI; out vox: TVec3i): TVec3;
begin
   if (slices2D = nil) then exit(Vec3(0.0, 0.0, 0.0));
    result := slices2D.FracMM(vol.Mat, vol.Dim, vox);
end;

procedure TGPUVolume.SetSlice2DFrac(frac : TVec3);
begin
     if (slices2D = nil) or (frac.x < 0.0) or (frac.y < 0.0) or (frac.z < 0.0) then exit;
     slices2D.sliceFrac := frac;
end;

function TGPUVolume.GetSlice2DMaxXY(mouseX, mouseY: integer; var Lo: TPoint): TPoint;
begin
  result.x := 0;
  result.y := 0;
  if (slices2D <> nil) then result := slices2D.GetSlice2DMaxXY2D(mouseX, mouseY, lo);
end;

function TGPUVolume.GetSlice2DFrac(mouseX, mouseY: integer; var oOrient: integer): TVec3;
begin
  result.x := 0;
  result.y := 0;
  result.z := 0;
  oOrient := 0;
  if (slices2D <> nil) then  result := slices2D.GetSlice2DFrac2D(mouseX, mouseY, oOrient);
end;

{$IFDEF MOSAIC}
procedure TGPUVolume.PaintMosaicRender(var vol: TNIfTI; lRender: TMosaicRender);
var
  //modelViewProjectionMatrixInverse,
  modelViewProjectionMatrix, projectionMatrix, modelMatrix: TMat4;
  modelLightPos, v, rayDir: TVec4;
  i: integer;
  lElevation, lAzimuth: single;
  {$IFDEF MATCAP}
  normalMatrix: TMat4;
  nMtx: array [0..8] of single;
  {$ENDIF}
begin
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
  glUseProgram(programRaycastBetter);
  {$IFDEF COREGL}
  {$IFDEF LCLgtk3}
  glBindFramebuffer(GL_FRAMEBUFFER, 1);
  {$ELSE}
   glBindFramebuffer(GL_FRAMEBUFFER, 0);
  {$ENDIF}
  {$ELSE}
  //2020 glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
  {$ENDIF}
  glActiveTexture(GL_TEXTURE2);
  glBindTexture(GL_TEXTURE_3D, intensityTexture3D);
  glUniform1i(intensityVolLoc, 2);
  glActiveTexture(GL_TEXTURE3);
  glBindTexture(GL_TEXTURE_3D, gradientTexture3D);
  glUniform1i(gradientVolLoc, 3);
  //bind overlay intensity (color)
  glUniform1i(overlaysLoc, overlayNum); //0 if no overlays
  glActiveTexture(GL_TEXTURE4);
  glBindTexture(GL_TEXTURE_3D, overlayIntensityTexture3D);
  glUniform1i(overlayIntensityVolLoc, 4);
  //bind background gradient  (edges)
  glActiveTexture(GL_TEXTURE5);
  glBindTexture(GL_TEXTURE_3D, overlayGradientTexture3D);
  glUniform1i(overlayGradientVolLoc, 5);
  //other uniforms...
  glUniform1f(stepSizeLoc, ComputeStepSize(kQualityBest, maxDim)) ;
  if vol.IsLabels then
     glUniform1f(backAlphaLoc, 1)
   else
     glUniform1f(backAlphaLoc, vol.OpacityPercent/100);
  glUniform1f(sliceSizeLoc, 1.0/maxDim);
  //glUniform1i(loopsLoc,round(maxDim*2.2));
  {$IFDEF MATCAP}
  //printf(format('>>matcapLoc %d matcap %d',[matcapLoc, matcap2D]));
  if (matcap1Loc >= 0) and (matcap2D1 > 0) then begin
    modelMatrix := TMat4.Identity;
    modelMatrix *= TMat4.Translate(0, 0, -fDistance);
    modelMatrix *= TMat4.RotateX(-DegToRad(90-lElevation));
    modelMatrix *= TMat4.RotateZ(DegToRad(lAzimuth));
    modelMatrix *= TMat4.Translate(-vol.Scale.X/2, -vol.Scale.Y/2, -vol.Scale.Z/2);
    modelLightPos := (modelMatrix.Transpose * fLightPos);
    modelMatrix *= TMat4.Scale(vol.Scale.X, vol.Scale.Y, vol.Scale.Z); //for volumes that are rectangular not square
    glActiveTexture(GL_TEXTURE6);
    glBindTexture(GL_TEXTURE_2D, matcap2D1);
    glUniform1i(matcap1Loc, 6);
    normalMatrix := modelMatrix.Inverse.Transpose;
    nMtx[0] := normalMatrix.m[0,0];
    nMtx[1] := normalMatrix.m[0,1];
    nMtx[2] := normalMatrix.m[0,2];
    nMtx[3] := normalMatrix.m[1,0];
    nMtx[4] := normalMatrix.m[1,1];
    nMtx[5] := normalMatrix.m[1,2];
    nMtx[6] := normalMatrix.m[2,0];
    nMtx[7] := normalMatrix.m[2,1];
    nMtx[8] := normalMatrix.m[2,2];
    glUniformMatrix3fv(normLoc, 1, GL_FALSE, @nMtx);
    if (matcap2Loc >= 0) then begin
        glActiveTexture(GL_TEXTURE7);
        glBindTexture(GL_TEXTURE_2D, matcap2D2);
        glUniform1i(matcap2Loc, 7);
    end;
  end;
  {$ENDIF}
  {$IFNDEF COREGL}
  glUniform3f(textureSzLoc, Vol.Dim.x, Vol.Dim.y, Vol.Dim.z );
  {$ENDIF}
  //unit model matrix for lighting
  modelMatrix := TMat4.Identity;
  modelMatrix *= TMat4.Translate(0.5, 0.5, -1.0);
  modelMatrix *= TMat4.RotateX(-DegToRad(90-lElevation));
  modelMatrix *= TMat4.RotateZ(DegToRad(lAzimuth));
  modelMatrix *= TMat4.Translate(-0.5, -0.5, -0.5); //pivot around 0.5 as cube spans 0..1
  modelLightPos := (modelMatrix.Transpose * fLightPos);
  modelLightPos := modelLightPos.normalize;
  //model matrix in pixel space
  modelMatrix := TMat4.Identity;
  modelMatrix *= TMat4.Translate(lRender.Left, lRender.Bottom,0);
  modelMatrix *= TMat4.Scale(lRender.Width, lRender.Height,1);
  modelMatrix *= TMat4.Translate(0.5, 0.5, -1.0);
  modelMatrix *= TMat4.RotateX(-DegToRad(90-lElevation));
  modelMatrix *= TMat4.RotateZ(DegToRad(lAzimuth));
  modelMatrix *= TMat4.Translate(-0.5, -0.5, -0.5); //pivot around 0.5 as cube spans 0..1

  projectionMatrix := TMat4.OrthoGL (0, glControl.clientwidth, 0, glControl.clientheight, 0.01, 2);
  glUniform3f(lightPositionLoc,modelLightPos.x, modelLightPos.y, modelLightPos.z);
  glUniform4f(clipPlaneLoc, fClipPlane.x, fClipPlane.y, fClipPlane.z, fClipPlane.w);
  glUniform1f(clipThickLoc, ClipThick);
  if (shaderPrefs.nUniform > 0) then
     for i := 1 to shaderPrefs.nUniform do
         glUniform1f(prefLoc[i], shaderPrefs.Uniform[i].DefaultV);
  modelViewProjectionMatrix := ( projectionMatrix * modelMatrix);
  rayDir.x := 0; RayDir.y := 0; rayDir.z := 1; RayDir.w := 0;
  v := rayDir;
  rayDir := (modelViewProjectionMatrix.Inverse * v);
  rayDir.w := 0;
  rayDir := rayDir.Normalize;
  addFuzz(rayDir);
  glUniformMatrix4fv(mvpLoc, 1, GL_FALSE, @modelViewProjectionMatrix);
  //x modelViewProjectionMatrixInverse := modelViewProjectionMatrix.Inverse;
  //x glUniformMatrix4fv(imvLoc, 1, GL_FALSE, @modelViewProjectionMatrixInverse);
  glUniform3f(rayDirLoc,rayDir.x,rayDir.y,rayDir.z);
  //glViewport(0, 0, glControl.ClientWidth, glControl.ClientHeight); //required for form resize
  glControl.SetViewport();
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_CULL_FACE);
  {$IFDEF STRIP}
  glCullFace(GL_BACK);
  {$ELSE}
  glCullFace(GL_FRONT);
  {$ENDIF}
  {$IFDEF COREGL}
  glBindVertexArray(vao);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboBox3D);
  {$IFDEF STRIP}
  glDrawElements(GL_TRIANGLE_STRIP, 14, GL_UNSIGNED_INT, nil);
  {$ELSE}
  glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, nil);
  {$ENDIF}
  {$ELSE}
  glCallList(dlBox3D);
  {$ENDIF}
  glDisable(GL_CULL_FACE);
end;

procedure TGPUVolume.PaintMosaic2D(var vol: TNIfTI; Drawing: TDraw; MosaicString: string);
var
  i: integer;
  w,h, f: single;
  {$IFNDEF COREGL}
  p: TVec2;
  c: TVec4;
  {$ENDIF}
begin
  {$IFDEF COREGL}
  if vao = 0 then // only once
  {$ELSE}
  if dlBox3D = 0 then
  {$ENDIF}
    LoadCube();
  //if (vol.VolRGBA <> nil) then
     LoadTexture(vol, false);
  if (intensityTexture3D = 0) then
    exit;
  UpdateDraw(Drawing);
  {$IFDEF COREGL}
      {$IFDEF LCLgtk3}
    glBindFramebuffer(GL_FRAMEBUFFER, 1);
    {$ELSE}
     glBindFramebuffer(GL_FRAMEBUFFER, 0);
    {$ENDIF}
  {$ELSE}
  //2020  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
  {$ENDIF}
  w := glControl.clientwidth;
  h := glControl.clientheight;
  //load
  {$IFDEF CLRBAR}
  clrbar.RulerPixels:= 0;
  if clrbar <> nil then begin
       f := clrbar.PanelFraction;
       if (f > 0.0) and (f < 0.5) then begin
          if (clrbar.isVertical) then
             w := w - (w * f)
          else
             h := h - (h * f);
       end;
  end;
  {$ENDIF}
  {$IFDEF MOSAIC}
  slices2D.UpdateMosaic(MosaicString, vol.Mat, vol.InvMat, vol.Dim, vol.Scale, w,h);
  {$ENDIF}
  w := glControl.clientwidth;
  h := glControl.clientheight;

  if (slices2D.NumberOfVertices < 3) and (slices2D.NumberOfMosaicRenders < 1) then exit; //nothing to do

  //draw
  //glViewport(0, 0, glControl.ClientWidth, glControl.ClientHeight); //required for form resize
  glControl.SetViewport();
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  {$IFDEF COREGL}
  {$IFDEF LCLgtk3}
  glBindFramebuffer(GL_FRAMEBUFFER, 1);
  {$ELSE}
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  {$ENDIF}
  {$ELSE}
  //2020 glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
  {$ENDIF}
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  if (slices2D.NumberOfVertices >= 3) then begin
    //draw 2D slices
    glUseProgram(programTex2D);
    //background texture
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_3D, intensityTexture3D);
    glUniform1i(uniform_tex, 1);
    //overlay texture
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_3D, overlayIntensityTexture3D);
    glUniform1i(uniform_overlay, 2);
    //draw texture
    glActiveTexture(GL_TEXTURE3);
    glBindTexture(GL_TEXTURE_3D, drawTexture3D);
    glUniform1i(uniform_drawTex, 3);
    //draw lookup table
    glActiveTexture( GL_TEXTURE4);
    glBindTexture(GL_TEXTURE_1D, drawTexture1D);
    glUniform1i(uniform_drawLUT, 4);
     if not isSmooth2D then begin
       glBindTexture(GL_TEXTURE_3D, intensityTexture3D);
       glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
       glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
       glBindTexture(GL_TEXTURE_3D, overlayIntensityTexture3D);
       glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
       glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
     end;
    if (not Drawing.IsOpen) or (Drawing.OpacityFraction <= 0.0) then
       glUniform1f(uniform_drawAlpha, 0.0)
    else
        glUniform1f(uniform_drawAlpha, Drawing.OpacityFraction); //<- loaded!
    glActiveTexture( GL_TEXTURE4);
    glBindTexture(GL_TEXTURE_1D, drawTexture1D);
    glUniform1i(uniform_drawLUT, 4);
    //other uniforms
    glUniform1i(uniform_overlaysLoc, overlayNum); //0 if no overlays
    if vol.IsLabels then
       glUniform1f(uniform_backAlpha, 1)
    else
        glUniform1f(uniform_backAlpha, vol.OpacityPercent/100);
    glUniform2f(uniform_viewportSizeTex, w, h);
    {$IFDEF COREGL}
    glBindBuffer(GL_ARRAY_BUFFER, vboTex2D);
    glBufferData(GL_ARRAY_BUFFER,slices2D.NumberOfVertices*SizeOf(TVertex2D), @slices2D.SliceVertices[0], GL_STATIC_DRAW);
    glBindVertexArray(vaoTex2d);
    glDrawArrays(GL_TRIANGLES, 0, slices2D.NumberOfVertices);
    {$ELSE}
    glBegin(GL_TRIANGLES);
       for i := 0 to (slices2D.NumberOfVertices - 1) do begin
           c := slices2D.SliceVertices[i].textureCoord;
           glColor4f(c.r, c.g, c.b, c.a);
           p := slices2D.SliceVertices[i].position;
           glVertex2f(p.x, p.y);
       end;
    glEnd();
    {$ENDIF}
    if not isSmooth2D then begin
      glBindTexture(GL_TEXTURE_3D, intensityTexture3D);
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glBindTexture(GL_TEXTURE_3D, overlayIntensityTexture3D);
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    end;
  end;
  //draw render
  if slices2D.NumberOfMosaicRenders > 0 then
     for i := 0 to (slices2D.NumberOfMosaicRenders -1) do
         PaintMosaicRender(vol, slices2D.MosaicRenders[i]);
  //draw lines
  if (slices2D.NumberOfLineVertices > 0) then begin //draw 2D lines
    glUseProgram(programLine2D);
    glUniform2f(uniform_viewportSizeLine, w, h);
    {$IFDEF COREGL}
    glBindBuffer(GL_ARRAY_BUFFER, vboLine2D);
    glBufferData(GL_ARRAY_BUFFER,slices2D.NumberOfLineVertices*SizeOf(TVertex2D), @slices2D.LineVertices[0], GL_STATIC_DRAW);
    glBindVertexArray(vaoLine2D);
    glDrawArrays(GL_TRIANGLES, 0, slices2D.NumberOfLineVertices);
    {$ELSE}
    glBegin(GL_TRIANGLES);
       for i := 0 to (slices2D.NumberOfLineVertices - 1) do begin
           c := slices2D.LineVertices[i].textureCoord;
           glColor4f(c.r, c.g, c.b, c.a);
           p := slices2D.LineVertices[i].position;
           glVertex2f(p.x, p.y);
       end;
    glEnd();
    {$ENDIF}
  end;
  {$IFDEF CLRBAR}
  if clrbar <> nil then
   clrbar.Draw();
  {$ENDIF}
   txt.DrawText();
  glControl.SwapBuffers;
end;
{$ENDIF}


{$IFNDEF COREGL}
procedure TGPUVolume.UpdateColorEditorDisplayList;
var
   i: integer;
   p: TVec2;
   c: TVec4;
begin
 if not colorEditor.HasNewLines then exit;
 colorEditor.HasNewLines := false;
 if not dlColorEditor <> 0 then
    glDeleteLists(dlColorEditor, 1);
 dlColorEditor := glGenLists(1);
 glNewList(dlColorEditor, GL_COMPILE);
 glBegin(GL_TRIANGLES);
   for i := 0 to (colorEditor.NumberOfLineVertices - 1) do begin
       c := colorEditor.LineVertices[i].textureCoord;
       glColor4f(c.r, c.g, c.b, c.a);
       p := colorEditor.LineVertices[i].position;
       glVertex2f(p.x, p.y);
   end;
 glEnd();
 glEndList();
end;
{$ENDIF}

procedure TGPUVolume.Paint2D(var vol: TNIfTI; Drawing: TDraw; DisplayOrient: integer);
var
 w,h, scale, rulerPx: single;
 i: integer;
 {$IFNDEF COREGL}
 p: TVec2;
 c: TVec4;
 {$ENDIF}
begin
  {$IFDEF COREGL}
  if vao = 0 then // only once
  {$ELSE}
  if dlBox3D = 0 then
  {$ENDIF}
    LoadCube();
  if (vol.VolRGBA <> nil) then
     LoadTexture(vol, true);
  if (intensityTexture3D = 0) then
    exit;
  UpdateDraw(Drawing);
  if (vol.VolRGBA <> nil) then
     LoadTexture(vol, true);
  if (intensityTexture3D = 0) then
    exit;
  w := glControl.clientwidth;
  h := glControl.clientheight;
  //load
  {$IFDEF CLRBAR}
  if (clrbar <> nil) and (clrbar.PanelFraction < 1.0) and (clrbar.PanelFraction > 0.0) then begin
     if (clrbar.isVertical) then
        w := round(w * (1.0-clrbar.PanelFraction))
     else
         h := round(h * (1.0-clrbar.PanelFraction));
     scale := slices2D.Update(vol.Scale, w, h, DisplayOrient, glControl.clientheight);
     w := glControl.clientwidth;
     h := glControl.clientheight;
  end else {$ENDIF}
      scale := slices2D.Update(vol.Scale, w, h, DisplayOrient);
  if SelectionRect.x > 0 then
     slices2D.DrawOutLine(SelectionRect.X,h-SelectionRect.Y, SelectionRect.Z,h-SelectionRect.W);
  if slices2D.NumberOfVertices < 3 then exit; //nothing to do
  glControl.SetViewport();
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  {$IFDEF COREGL}
  {$IFDEF LCLgtk3}
  glBindFramebuffer(GL_FRAMEBUFFER, 1);
  {$ELSE}
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  {$ENDIF}
  {$ELSE}
  //2020 glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
  {$ENDIF}
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  //draw 2D texture
  glUseProgram(programTex2D);
  //background texture
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_3D, intensityTexture3D);
  if not isSmooth2D then begin
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    end;
  glUniform1i(uniform_tex, 1);
  //overlay texture
  glActiveTexture(GL_TEXTURE2);
  glBindTexture(GL_TEXTURE_3D, overlayIntensityTexture3D);
  if not isSmooth2D then begin
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  end;
  glUniform1i(uniform_overlay, 2);
  //draw texture
  glActiveTexture(GL_TEXTURE3);
  glBindTexture(GL_TEXTURE_3D, drawTexture3D);
  glUniform1i(uniform_drawTex, 3);
  //draw lookup table
  glActiveTexture( GL_TEXTURE4);
  glBindTexture(GL_TEXTURE_1D, drawTexture1D);
  glUniform1i(uniform_drawLUT, 4);
  if (not Drawing.IsOpen) or (Drawing.OpacityFraction <= 0.0) then
     glUniform1f(uniform_drawAlpha, 0.0)
  else
      glUniform1f(uniform_drawAlpha, Drawing.OpacityFraction); //<- loaded!
  //other uniforms
  glUniform1i(uniform_overlaysLoc, overlayNum); //0 if no overlays
  if vol.IsLabels then
     glUniform1f(uniform_backAlpha, 1)
  else
      glUniform1f(uniform_backAlpha, vol.OpacityPercent/100);
  glUniform2f(uniform_viewportSizeTex, w, h);
  {$IFDEF FX}
  glUniform3f(uniform_texVox, vol.Dim.x, vol.Dim.y, vol.Dim.z);
  {$ENDIF}
  {$IFDEF COREGL}
  glBindBuffer(GL_ARRAY_BUFFER, vboTex2D);
  glBufferData(GL_ARRAY_BUFFER,slices2D.NumberOfVertices*SizeOf(TVertex2D), @slices2D.SliceVertices[0], GL_STATIC_DRAW);
  glBindVertexArray(vaoTex2d);
  glDrawArrays(GL_TRIANGLES, 0, slices2D.NumberOfVertices);
  {$ELSE}
  glBegin(GL_TRIANGLES);
     for i := 0 to (slices2D.NumberOfVertices - 1) do begin
         c := slices2D.SliceVertices[i].textureCoord;
         glColor4f(c.r, c.g, c.b, c.a);
         p := slices2D.SliceVertices[i].position;
         glVertex2f(p.x, p.y);
     end;
  glEnd();
  {$ENDIF}
  if (slices2D.NumberOfLineVertices > 0) then begin //draw 2D lines
    glUseProgram(programLine2D);
    glUniform2f(uniform_viewportSizeLine, w, h);
    {$IFDEF COREGL}
    glBindBuffer(GL_ARRAY_BUFFER, vboLine2D);
    glBufferData(GL_ARRAY_BUFFER,slices2D.NumberOfLineVertices*SizeOf(TVertex2D), @slices2D.LineVertices[0], GL_STATIC_DRAW);
    glBindVertexArray(vaoLine2D);
    glDrawArrays(GL_TRIANGLES, 0, slices2D.NumberOfLineVertices);
    {$ELSE}
    glBegin(GL_TRIANGLES);
       for i := 0 to (slices2D.NumberOfLineVertices - 1) do begin
           c := slices2D.LineVertices[i].textureCoord;
           glColor4f(c.r, c.g, c.b, c.a);
           p := slices2D.LineVertices[i].position;
           glVertex2f(p.x, p.y);
       end;
    glEnd();
    {$ENDIF}
  end;
  //reset linear interpolation - much better for rendering and mosaics
  if not isSmooth2D then begin
      glBindTexture(GL_TEXTURE_3D, intensityTexture3D);
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glBindTexture(GL_TEXTURE_3D, overlayIntensityTexture3D);
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  end;
  //Draw rendering: do this BEFORE color editor and colorbar so it is behind them
  if (DisplayOrient = kAxCorSagOrient4) then begin
      i := RayCastQuality1to5;
      if (i < kQualityMedium) then
        RayCastQuality1to5 := kQualityMedium;
      PaintCore(vol, slices2D.axCorSagOrient4XY, false, false);
      RayCastQuality1to5 := i;
      glControl.SetViewport();
  end;
  txt.DrawText(); //D888
  //GLForm1.LayerBox.caption := format('%g %g', [scale, vol.MaxMM]);    //uses mainunit
  rulerPx := (scale)/vol.MaxMM ;//pixels per mm
  rulerPx := rulerPx * 100; //ruler is 10cm = 100mm
  {$IFDEF CLRBAR}
  clrbar.RulerPixels:= rulerPx;
  if clrbar <> nil then
   clrbar.Draw();
  {$ENDIF}
  //draw color editor
  if colorEditorVisible then begin //final step: draw ON TOP of the colorbar and ruler
    colorEditor.Update(w, h, vol);
    if colorEditor.NumberOfLineVertices > 2 then begin
        glUseProgram(programLine2D);
        glUniform2f(uniform_viewportSizeLine, w, h);
        {$IFDEF COREGL}
        glBindBuffer(GL_ARRAY_BUFFER, vboLine2D);
        glBufferData(GL_ARRAY_BUFFER,colorEditor.NumberOfLineVertices*SizeOf(TVertex2D), @colorEditor.LineVertices[0], GL_STATIC_DRAW);
        glBindVertexArray(vaoLine2D);
        glDrawArrays(GL_TRIANGLES, 0, colorEditor.NumberOfLineVertices);
        {$ELSE}
        UpdateColorEditorDisplayList();
        glCallList(dlColorEditor);
        {$ENDIF}
    end;
  end;
  glControl.SwapBuffers;
end;
{$ENDIF}

procedure TGPUVolume.Paint(var vol: TNIfTI);
var
	widthHeightLeft: TVec3i;
begin
	widthHeightLeft.x := glControl.clientwidth;
    widthHeightLeft.y := glControl.clientheight;
    widthHeightLeft.z := 0;
    PaintCore(vol, widthHeightLeft);
end;

{$IFDEF DEPTHPICKER2}
procedure TGPUVolume.PaintDepth(var vol: TNIfTI; isAxCorSagOrient4: boolean = false);
var
	widthHeightLeft: TVec3i;
begin
  widthHeightLeft.x := glControl.clientwidth;
  widthHeightLeft.y := glControl.clientheight;
  widthHeightLeft.z := 0;
  if isAxCorSagOrient4 then
          widthHeightLeft := slices2D.axCorSagOrient4XY;
  PaintCore(vol, widthHeightLeft, false, false);
end;
{$ENDIF}

{$include cylinder.inc}


procedure TGPUVolume.PaintCrosshair3D(rgba: TVec4; nfaces: integer );
begin
  glUniform4f(colorLine3DLoc, rgba.r, rgba.g, rgba.b, rgba.a);
  glDisable(GL_CULL_FACE);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboLineIdx3D);
  glDrawElements(GL_TRIANGLES,  nfaces*3, GL_UNSIGNED_INT, nil);
  {$IFDEF COREGL}
  //glBindVertexArray(0);
  {$ENDIF}

end;

function TGPUVolume.MakeCrosshair3D(var vol: TNIfTI): integer;
var
    vertices: TVertices = nil ;
    faces: TFaces = nil;
    vertLoc: GLint;
    radius: TVec3;
begin
  radius := vol.mmAsFrac * (vol.minPixDim * slices2D.LineWidth * 0.5);
  MakeCyl(radius, slices2D.sliceFrac, faces, vertices);
  {$IFDEF COREGL}
  glBindBuffer(GL_ARRAY_BUFFER, vboLine3D);
  glBufferData(GL_ARRAY_BUFFER, length(vertices)*SizeOf(TVec3), @vertices[0], GL_STATIC_DRAW);
  glBindVertexArray(vaoLine3D);
  if (vboLineIdx3D = 0) then begin
  	glGenBuffers(1, @vboLineIdx3D);
  	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboLineIdx3D);
  	glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(faces)*sizeof(TVec3i), @faces[0], GL_STATIC_DRAW);
  end;
  {$ELSE}
  if (vboLine3D = 0) then begin
    glGenBuffers(1, @vboLine3D);
    glBindBuffer(GL_ARRAY_BUFFER, vboLine3D);
    glBufferData(GL_ARRAY_BUFFER, length(vertices)*SizeOf(TVec3), @vertices[0], GL_STATIC_DRAW);
    vertLoc := glGetAttribLocation(programLine3D, 'Vert');
    glVertexAttribPointer(vertLoc, 3, GL_FLOAT, GL_FALSE, SizeOf(TVec3), PChar(0));
    glEnableVertexAttribArray(vertLoc);
  end else begin
  	glBindBuffer(GL_ARRAY_BUFFER, vboLine3D);
  	glBufferSubData(GL_ARRAY_BUFFER, 0, length(vertices)*SizeOf(TVec3), @vertices[0]);
  end;
  if (vboLineIdx3D = 0) then begin
  	glGenBuffers(1, @vboLineIdx3D);
  	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboLineIdx3D);
  	glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(faces)*sizeof(TVec3i), @faces[0], GL_STATIC_DRAW);
  end;// else
  //	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,vboLineIdx3D);
  {$ENDIF}
  exit(length(faces));
end;

procedure TGPUVolume.PaintCore(var vol: TNIfTI; widthHeightLeft: TVec3i; clearScreen: boolean = true; isDepthShader: boolean = false);
var
 normalMatrix,
  modelViewProjectionMatrix, projectionMatrix, modelMatrix: TMat4;
  nMtx: array [0..8] of single;
  modelLightPos, v, rayDir: TVec4;
  w,h, whratio, scale: single;
  i, nfaces: integer;
begin
  {$IFDEF COREGL}
  if vao = 0 then // only once
  {$ELSE}
  if dlBox3D = 0 then
  {$ENDIF}
  LoadCube();
  LoadTexture(vol, false);
  if (intensityTexture3D = 0) then
    exit;
  if (widthHeightLeft.y < 1) or (widthHeightLeft.x < 1) then
  	exit;
  {$IFDEF MTX}
  modelMatrix := fModelMatrix;
  {$ELSE}
  modelMatrix := TMat4.Identity;
  modelMatrix *= TMat4.Translate(0, 0, -fDistance);
  if (Vol.Dim.z < 2) and  (abs(fElevation) < 0.1) then
    modelMatrix *= TMat4.RotateX(-DegToRad(90.1))
  else
  	modelMatrix *= TMat4.RotateX(-DegToRad(90-fElevation));
  modelMatrix *= TMat4.RotateZ(DegToRad(fAzimuth));
  modelMatrix *= TMat4.RotateX(DegToRad(fPitch));
  {$ENDIF}
  modelMatrix *= TMat4.Translate(-vol.Scale.X/2, -vol.Scale.Y/2, -vol.Scale.Z/2);
  modelLightPos := (modelMatrix.Transpose * fLightPos);
  modelLightPos := modelLightPos.Normalize;
  modelMatrix *= TMat4.Scale(vol.Scale.X, vol.Scale.Y, vol.Scale.Z); //for volumes that are rectangular not square
  if fDistance = 0 then
          scale := 1
  else
      scale := 0.5 * 1/abs(kDefaultDistance/(fDistance+1.0));
  glViewport(widthHeightLeft.z+glControl.tileLeft, glControl.tileBottom, widthHeightLeft.x, widthHeightLeft.y);
  whratio := widthHeightLeft.x /widthHeightLeft.y;
  if (whratio > 1) or (whratio = 0) then //Wide window
     projectionMatrix := TMat4.OrthoGL (-scale * whratio, scale * whratio, -scale, scale, fDistance-1, fDistance+1)
  else
      projectionMatrix := TMat4.OrthoGL (-scale, scale, -scale/whratio, scale/whratio, fDistance-1, fDistance+1);
  modelViewProjectionMatrix := ( projectionMatrix * modelMatrix);
  {$IFDEF DEPTHPICKER2}
  //widthHeightLeftX: TVec3i;
  mvp := modelViewProjectionMatrix;
  viewportXYWH := vec4(widthHeightLeft.z, 0, widthHeightLeft.x, widthHeightLeft.y);
  //printf(format('viewport %g %g %g %g', [viewportXYWH.x, viewportXYWH.y, viewportXYWH.z, viewportXYWH.w]));
  //printf(format('volScale %g %g %g', [vol.Scale.X, vol.Scale.Y, vol.Scale.Z]));
  //viewportXYWH: TVec4;
  {$ENDIF}
  glEnable(GL_DEPTH_TEST);
  if Quality1to5 = kQualityBest then
         glUseProgram(programRaycastBetter)
  else
        glUseProgram(programRaycast);
  {$IFDEF COREGL}
  {$IFDEF LCLgtk3}
  glBindFramebuffer(GL_FRAMEBUFFER, 1);
  {$ELSE}
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  {$ENDIF}
  //2020 glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
  {$ENDIF}
  //bind background intensity (color)
  glActiveTexture(GL_TEXTURE2);
  glBindTexture(GL_TEXTURE_3D, intensityTexture3D);
  //bind overlay intensity (color)
  //glUniform1i(overlaysLoc, overlayIntensityTexture3D); //0 if no overlays
  glActiveTexture(GL_TEXTURE4);
  glBindTexture(GL_TEXTURE_3D, overlayIntensityTexture3D);
  //bind background gradient  (edges)
  glActiveTexture(GL_TEXTURE5);
  glBindTexture(GL_TEXTURE_3D, overlayGradientTexture3D);
  //glUniform1i(loopsLoc,round(maxDim*2.2));
  //glUniform3f(clearColorLoc, fClearColor.r/255, fClearColor.g/255, fClearColor.b/255);
  rayDir.x := 0; RayDir.y := 0; rayDir.z := 1; RayDir.w := 0;
  v := rayDir;
  rayDir := (modelViewProjectionMatrix.Inverse * v);
  rayDir.w := 0;
  rayDir := rayDir.Normalize;
  addFuzz(rayDir);
  //printf(format('%g %g %g', [vol.Scale.X, vol.Scale.Y, vol.Scale.Z]));
  //printMat(modelMatrix);
  //printf(format('a %d e %d = [%g %g %g]', [fAzimuth, fElevation, rayDir.x, rayDir.y, rayDir.z]));
    glUniform1i(intensityVolLoc, 2);
    //bind background gradient (edges)
    glActiveTexture(GL_TEXTURE3);
    glBindTexture(GL_TEXTURE_3D, gradientTexture3D);
    glUniform1i(gradientVolLoc, 3);
    glUniform1i(overlaysLoc, overlayNum); //0 if no overlays
    glUniform1i(overlayIntensityVolLoc, 4);
    glUniform1i(overlayGradientVolLoc, 5);
    if vol.IsLabels then
       glUniform1f(backAlphaLoc, 1)
    else
     glUniform1f(backAlphaLoc, vol.OpacityPercent/100);
    //bind other uniforms
    glUniform1f(stepSizeLoc, ComputeStepSize(Quality1to5, maxDim)) ;
    glUniform1f(sliceSizeLoc, 1/maxDim);
    glUniform3f(lightPositionLoc, modelLightPos.x, modelLightPos.y, modelLightPos.z);
    glUniform4f(clipPlaneLoc, fClipPlane.x, fClipPlane.y, fClipPlane.z, fClipPlane.w);
    glUniform1f(clipThickLoc, ClipThick);
    if (shaderPrefs.nUniform > 0) then
       for i := 1 to shaderPrefs.nUniform do
           glUniform1f(prefLoc[i], shaderPrefs.Uniform[i].DefaultV);
    {$IFNDEF COREGL}
    glUniform3f(textureSzLoc, Vol.Dim.x, Vol.Dim.y, Vol.Dim.z );
    {$ENDIF}
    glUniformMatrix4fv(mvpLoc, 1, GL_FALSE, @modelViewProjectionMatrix);
    glUniform3f(rayDirLoc,rayDir.x,rayDir.y,rayDir.z);
    //bind matcap
    {$IFDEF MATCAP}
    //printf(format('>>matcapLoc %d matcap %d',[matcapLoc, matcap2D]));
    if (matcap1Loc >= 0) and (matcap2D1 > 0) then begin
       glActiveTexture(GL_TEXTURE6);
       glBindTexture(GL_TEXTURE_2D, matcap2D1);
       glUniform1i(matcap1Loc, 6);
       normalMatrix := modelMatrix.Inverse.Transpose;
       nMtx[0] := normalMatrix.m[0,0];
       nMtx[1] := normalMatrix.m[0,1];
       nMtx[2] := normalMatrix.m[0,2];
       nMtx[3] := normalMatrix.m[1,0];
       nMtx[4] := normalMatrix.m[1,1];
       nMtx[5] := normalMatrix.m[1,2];
       nMtx[6] := normalMatrix.m[2,0];
       nMtx[7] := normalMatrix.m[2,1];
       nMtx[8] := normalMatrix.m[2,2];
       glUniformMatrix3fv(normLoc, 1, GL_FALSE, @nMtx);
       if (matcap2Loc >= 0) then begin
              glActiveTexture(GL_TEXTURE7);
              glBindTexture(GL_TEXTURE_2D, matcap2D2);
              glUniform1i(matcap2Loc, 7);
       end;
    end;
    {$ENDIF}

  //glControl.SetViewport(); //OKRA
  if (clearScreen) then glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  //glDisable(GL_DEPTH_TEST);
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_CULL_FACE);
  {$IFDEF STRIP}
  glCullFace(GL_BACK);
  {$ELSE}
  glCullFace(GL_FRONT);
  {$ENDIF}
  {$IFDEF COREGL}
  glBindVertexArray(vao);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboBox3D);
  {$IFDEF STRIP}
  glDrawElements(GL_TRIANGLE_STRIP, 14, GL_UNSIGNED_INT, nil);
  {$ELSE}
  glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, nil);
  {$ENDIF}
  {$ELSE}
  //Legacy OpenGL
  glCallList(dlBox3D);
  {$ENDIF}
  //draw color editor
  {$IFDEF VIEW2D}{$IFDEF LINE3D}
  if ((not isDepthShader) and (widthHeightLeft.z <> 0) and (slices2D.LineWidth > 0.0)) then begin
    glUseProgram(programLine3D);
    glUniformMatrix4fv(mvpLine3DLoc, 1, GL_FALSE, @modelViewProjectionMatrix);
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LEQUAL); //GL_LESS);
    //draw opaque line occluded by volume render
    nfaces := MakeCrosshair3D(vol);
    PaintCrosshair3D(slices2D.LineColor, nfaces);
    glDisable(GL_DEPTH_TEST);
    //draw translucent line regardless of volume render
    PaintCrosshair3D(Vec4(slices2D.LineColor.r,slices2D.LineColor.g,slices2D.LineColor.b,slices2D.LineColor.a * 0.2), nfaces);
    //glDepthFunc(GL_ALWAYS); //always pass test
  end else begin
  	glDepthFunc(GL_LEQUAL); //GL_LESS);
  	glDisable(GL_DEPTH_TEST);
  end;
  {$ENDIF}{$ENDIF} //IFDEF VIEW2D, LINE3D
  glDisable(GL_CULL_FACE);
  {$IFDEF CLRBAR}
  //if (colorEditorVisible) and (widthHeightLeft.z = 0) then begin
  if  (widthHeightLeft.z = 0) then begin
    clrbar.RulerPixels:= 0;
    if clrbar <> nil then
     clrbar.Draw();
  end;
  {$ENDIF}
  {$IFDEF VIEW2D}
  if (colorEditorVisible) and (not isDepthShader) and (widthHeightLeft.z = 0) then begin
     w := glControl.clientwidth;
     h := glControl.clientheight;
    colorEditor.Update(w, h, vol);
    if colorEditor.NumberOfLineVertices > 2 then begin
        glUseProgram(programLine2D);
        glUniform2f(uniform_viewportSizeLine, w, h);
        {$IFDEF COREGL}
        glBindBuffer(GL_ARRAY_BUFFER, vboLine2D);
        glBufferData(GL_ARRAY_BUFFER,colorEditor.NumberOfLineVertices*SizeOf(TVertex2D), @colorEditor.LineVertices[0], GL_STATIC_DRAW);
        glBindVertexArray(vaoLine2D);
        glDrawArrays(GL_TRIANGLES, 0, colorEditor.NumberOfLineVertices);
        {$ELSE}
        UpdateColorEditorDisplayList();
        glCallList(dlColorEditor);
        {$ENDIF}
    end;
  end;
  {$ENDIF}
  {$IFDEF CUBE}{$IFDEF VIEW2D}
  if (not isDepthShader) and (widthHeightLeft.z = 0) then begin
    if Slices.LabelOrient then begin
       {$IFNDEF STRIP}
       glCullFace(GL_BACK);
       {$ENDIF}
       //gCube.Size := 0.02;
       gCube.Azimuth := fAzimuth;
       gCube.Elevation := -fElevation;
       gCube.Pitch := fPitch;
       gCube.Draw(glControl.ClientWidth, glControl.ClientHeight);
    end;
  end;
  {$ENDIF}{$ENDIF}
  if (clearScreen) then glControl.SwapBuffers;
end;


end.

