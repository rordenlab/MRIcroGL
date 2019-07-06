unit glvolume2;
{$mode objfpc}
{$H+}
interface
{$DEFINE STRIP} //we can define cube as either a triangle or triangle strip - no implications on performance
{$DEFINE GPUGRADIENTS} //Computing volume gradients on the GPU is much faster than using the CPU
{$DEFINE VIEW2D}
{$DEFINE CUBE}
{$DEFINE MATCAP}
{$DEFINE TIMER} //reports GPU gradient time to stdout (Unix only)
uses
  {$IFDEF MATCAP} GraphType, FPImage, IntfGraphics, LCLType,{$ENDIF}
  {$IFDEF TIMER} DateUtils,{$ENDIF}
 {$IFDEF CUBE} glcube, {$ENDIF}
 retinahelper,glclrbar, niftis, SimdUtils, glcorearb, gl_core_utils, VectorMath, Classes, SysUtils, Graphics,
    math, OpenGLContext, dialogs, nifti {$IFDEF VIEW2D}, drawvolume, colorEditor, slices2D, glfont{$ENDIF};
const
 kDefaultDistance = 2.25;
 kMaxDistance = 40;
type
  TGPUVolume = class
      private
        {$IFDEF VIEW2D}
        programLine2D, programTex2D, vaoTex2D, vboTex2D, vaoLine2D, vboLine2D, vboBox3D: GLuint;
        prefLoc: array [1..kMaxUniform] of GLint;
        slices2D: TSlices2D;
        uniform_drawTex, uniform_drawLUT, uniform_drawAlpha,
        uniform_viewportSizeLine, uniform_viewportSizeTex, uniform_backAlpha, uniform_tex, uniform_overlay: GLint;
        colorEditor: TColorEditor;
        isSmooth2D, colorEditorVisible: boolean;
        txt: TGPUFont;
        {$ENDIF}
        shaderPrefs: TShaderPrefs;
        RayCastQuality1to10, maxDim,fAzimuth,fElevation, overlayNum, overlayGradTexWidth: integer;
        fDistance: single;
        fLightPos: TVec4;
        fClipPlane: TVec4;
        clrbar: TGPUClrbar;
        glControl: TOpenGLControl;
        overlayGradientVolLoc, overlayIntensityVolLoc,
        rayDirLoc,intensityVolLoc, gradientVolLoc,mvpLoc,normLoc, //imvLoc,
        lightPositionLoc,clipPlaneLoc,
        backAlphaLoc, sliceSizeLoc, stepSizeLoc, loopsLoc, overlaysLoc : GLint;
        overlayGradientTexture3D, overlayIntensityTexture3D,
        drawTexture1D, drawTexture3D,
        gradientTexture3D, intensityTexture3D, vao, programRaycast: GLuint;
        {$IFDEF MATCAP}
        matcap2D: GLuint;
        {$ENDIF}
        {$IFDEF CUBE} gCube :TGPUCube; {$ENDIF}
        {$IFDEF GPUGRADIENTS}programSobel, programBlur: GLuint;
        procedure CreateGradientVolumeGPU(Xsz,Ysz,Zsz: integer; var inTex, grTex: GLuint);
        {$ENDIF}
        procedure LoadCube();
        function LoadTexture(var vol: TNIfTI): boolean;
        procedure CreateDrawColorTable;//1D texture for drawing
        procedure CreateDrawTex(Dim: TVec3i; Vals: TUInt8s);
        procedure UpdateDraw(Drawing: TDraw);
        procedure CreateOverlayTextures(Dim: TVec3i; volRGBA: TRGBAs);
      public
        matcapLoc: GLint;
        {$IFDEF VIEW2D}
        SelectionRect: TVec4;
        property ShowColorEditor: boolean read colorEditorVisible write colorEditorVisible;
        property ShowSmooth2D: boolean read isSmooth2D write isSmooth2D;
        property CE: TColorEditor read colorEditor;
        property Slices: Tslices2D read slices2D;
        function Slice2Dmm(var vol: TNIfTI; out vox: TVec3i): TVec3;
        procedure SetSlice2DFrac(frac : TVec3);
        function GetSlice2DFrac(mouseX, mouseY: integer; out Orient: integer): TVec3;
        function GetSlice2DMaxXY(mouseX, mouseY: integer; var Lo: TPoint): TPoint;
        procedure Paint2D(var vol: TNIfTI; Drawing: TDraw; DisplayOrient: integer);
        procedure PaintMosaicRender(var vol: TNIfTI; lRender: TMosaicRender);
        procedure PaintMosaic2D(var vol: TNIfTI; Drawing: TDraw; MosaicString: string);
        {$ENDIF}
        {$IFDEF MATCAP}procedure SetMatCap(lFilename: string);{$ENDIF}
        //procedure CreateOverlayTextures();
        procedure UpdateOverlays(vols: TNIfTIs);
        property Quality1to10: integer read RayCastQuality1to10 write RayCastQuality1to10;
        property ShaderSliders: TShaderPrefs read shaderPrefs write shaderPrefs;
        procedure SetShaderSlider(idx: integer; newVal: single);
        property Azimuth: integer read fAzimuth write fAzimuth;
        property Elevation: integer read fElevation write fElevation;
        property Distance: single read fDistance write fDistance;
        property LightPosition: TVec4 read fLightPos write fLightPos;
        property ClipPlane: TVec4 read fClipPlane write fClipPlane;
        procedure Prepare(shaderName: string);
        constructor Create(fromView: TOpenGLControl);
        procedure Paint(var vol: TNIfTI);
        procedure SetShader(shaderName: string);
        procedure SetColorBar(fromColorbar: TGPUClrbar);
        procedure  SetTextContrast(clearclr: TRGBA);
        destructor Destroy; override;
  end;

implementation

//uses mainunit;

procedure printf (lS: AnsiString);
begin
{$IFNDEF WINDOWS} writeln(lS); {$ENDIF}
end;

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


procedure  TGPUVolume.SetTextContrast(clearclr: TRGBA);
begin
  if (clearclr.R + clearclr.G + clearclr.B) > 300 then
     txt.FontColor := Vec4(0,0,0,1)
  else
     txt.FontColor := Vec4(1,1,1,1);
end;

{$IFDEF VIEW2D}
const
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
//Simple Fragment Shader
kFragTex2D = '#version 330'
+#10'in vec4 texCoord;'
+#10'out vec4 color;'
+#10'uniform float backAlpha = 1.0;'
+#10'uniform sampler3D tex, drawTex, overlay;'
+#10'uniform sampler1D drawLUT;'
+#10'uniform float drawAlpha = 0.0;'
+#10'void main() {'
+#10'    color = texture(tex,texCoord.xyz);'
+#10'    color.a = smoothstep(0.0, 0.1, color.a);'
+#10'    //color.a = smoothstep(0.0, 0.00001, color.a);'
+#10'    color.a *= backAlpha;'
+#10'    //if (color.a > 0.0) color.a = backAlpha;'
+#10'    vec4 ocolor = texture(overlay, texCoord.xyz);'
+#10'    //vec4 ocolor = texture(drawLUT, texture(overlay, texCoord.xyz).r).rgba;'
+#10'    //vec4 ocolor = texture(drawLUT, texture(drawTex, texCoord.xyz).r).rgba;'
+#10'    color.rgb = mix(color.rgb, ocolor.rgb, ocolor.a);'
+#10'    //color.a = min(color.a, 0.5);'
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

{$ENDIF}
//uses vrForm;
procedure TGPUVolume.SetShaderSlider(idx: integer; newVal: single);
begin
      if (idx < 1) or (idx > kMaxUniform) then exit;
     shaderPrefs.Uniform[idx].DefaultV:=newVal;
end;

{$IFDEF GPUGRADIENTS}
function bindBlankGL(Xsz,Ysz,Zsz: integer): GLuint;
begin //creates an empty texture in VRAM without requiring memory copy from RAM
    //later run glDeleteTextures(1,&oldHandle);
    glGenTextures(1, @result);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glBindTexture(GL_TEXTURE_3D, result);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE); //, GL_CLAMP_TO_BORDER) will wrap
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
    glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA16, XSz, YSz, ZSz, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
end;

procedure glUniform1ix(prog: GLuint; name: AnsiString; value: integer);
begin
    glUniform1i(glGetUniformLocation(prog, pAnsiChar(Name)), value) ;
end;

procedure glUniform1fx(prog: GLuint; name: AnsiString; value: single );
begin
    glUniform1f(glGetUniformLocation(prog, pAnsiChar(Name)), value) ;
end;

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
   vx,i: integer;
   v: TUInt8s;
begin
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
     glTexImage3D(GL_TEXTURE_3D, 0, GL_RED, Dim.X, Dim.Y, Dim.Z, 0, GL_RED, GL_UNSIGNED_BYTE,@v[0]);
     v := nil;
  end else
      glTexImage3D(GL_TEXTURE_3D, 0, GL_RED, Dim.X, Dim.Y, Dim.Z, 0, GL_RED, GL_UNSIGNED_BYTE,@Vals[0]);
end;

procedure TGPUVolume.CreateGradientVolumeGPU(Xsz,Ysz,Zsz: integer; var inTex, grTex: GLuint);
//given 3D input texture inTex (with dimensions Xsz, Ysz, Zsz) generate 3D gradient texture gradTex
//http://www.opengl-tutorial.org/intermediate-tutorials/tutorial-14-render-to-texture/
//http://www.opengl.org/wiki/Framebuffer_Object_Examples
var
   i: integer;
   coordZ: single;
   fb, tempTex3D: GLuint;{$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
begin
  glFinish();//force update
  {$IFDEF TIMER}startTime := now;
  {$ELSE}
  {$IFDEF UNIX}printf('Creating GPU gradients');{$ENDIF}
  {$ENDIF}
  glBindVertexArray(vao);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboBox3D);
  glGenFramebuffers(1, @fb);
  glBindFramebuffer(GL_FRAMEBUFFER, fb);
  glDisable(GL_CULL_FACE);
  //{$IFNDEF COREGL}glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);{$ENDIF}// <- REQUIRED
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
  glViewport(0, 0, XSz, YSz);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);
  //STEP 1: run smooth program gradientTexture -> tempTex3D
  tempTex3D := bindBlankGL(Xsz,Ysz,Zsz);
  glUseProgram(programBlur);
  glActiveTexture( GL_TEXTURE1);
  //glBindTexture(GL_TEXTURE_3D, gRayCast.gradientTexture3D);//input texture
  glBindTexture(GL_TEXTURE_3D, inTex);//input texture is overlay
  glUniform1ix(programBlur, 'intensityVol', 1);
  glUniform1fx(programBlur, 'dX', 0.7/XSz); //0.5 for smooth - center contributes
  glUniform1fx(programBlur, 'dY', 0.7/YSz);
  glUniform1fx(programBlur, 'dZ', 0.7/ZSz);
  glBindVertexArray(vao);
  for i := 0 to (ZSz-1) do begin
      coordZ := 1/ZSz * (i + 0.5);
      glUniform1fx(programBlur, 'coordZ', coordZ);
      //glFramebufferTexture3D(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0, GL_TEXTURE_3D, tempTex3D, 0, i);//output texture
      //Ext required: Delphi compile on Winodws 32-bit XP with NVidia 8400M
      glFramebufferTexture3D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_3D, tempTex3D, 0, i);//output texture
      glClear(GL_DEPTH_BUFFER_BIT);  // clear depth bit (before render every layer)
      {$IFDEF STRIP}
      glDrawElements(GL_TRIANGLE_STRIP, 4, GL_UNSIGNED_INT, nil);
      {$ELSE}
      glDrawElements(GL_TRIANGLES, 2*3, GL_UNSIGNED_INT, nil);
      {$ENDIF}
  end;
  glUseProgram(0);
  //STEP 2: run sobel program gradientTexture -> tempTex3D
  //glUseProgramObjectARB(gRayCast.glslprogramSobel);
  glUseProgram(programSobel);
  glActiveTexture(GL_TEXTURE1);
  //x glBindTexture(GL_TEXTURE_3D, gRayCast.intensityTexture3D);//input texture
  glBindTexture(GL_TEXTURE_3D, tempTex3D);//input texture
    glUniform1ix(programSobel, 'intensityVol', 1);
    glUniform1fx(programSobel, 'dX', 1.2/XSz ); //1.0 for SOBEL - center excluded
    glUniform1fx(programSobel, 'dY', 1.2/YSz);
    glUniform1fx(programSobel, 'dZ', 1.2/ZSz);
    glBindVertexArray(vao);
    for i := 0 to (ZSz-1) do begin
        coordZ := 1/ZSz * (i + 0.5);
        glUniform1fx(programSobel, 'coordZ', coordZ);
        glFramebufferTexture3D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_3D, grTex, 0, i);//output is background
        glClear(GL_DEPTH_BUFFER_BIT);
        {$IFDEF STRIP}
        glDrawElements(GL_TRIANGLE_STRIP, 4, GL_UNSIGNED_INT, nil);
        {$ELSE}
        glDrawElements(GL_TRIANGLES, 2*3, GL_UNSIGNED_INT, nil);
        {$ENDIF}
    end;
    glUseProgram(0);
    glFinish();//force update
     //clean up:
     glDeleteTextures(1,@tempTex3D);
     glBindFramebuffer(GL_FRAMEBUFFER, 0);
     glDeleteFramebuffers(1, @fb);
     glActiveTexture( GL_TEXTURE0 );  //required if we will draw 2d slices next
    {$IFDEF TIMER}printf(format('GPU Gradient time %d',[MilliSecondsBetween(Now,startTime)]));{$ENDIF}
end;

{$ENDIF}

const
 //This fall-back render shader is used if the shader folder is not found or is empty
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

kFrag = #10'#version 330 core'
+#10'in vec3 TexCoord1;'
+#10'out vec4 FragColor;'
+#10'in vec4 vPosition;'
+#10'uniform int loops;'
+#10'uniform float stepSize, sliceSize;'
+#10'uniform sampler3D intensityVol, gradientVol;'
+#10'uniform sampler3D intensityOverlay, gradientOverlay;'
+#10'uniform vec3 lightPosition, rayDir;'
+#10'uniform vec4 clipPlane;'
+#10'uniform float ambient = 1.0;'
+#10'uniform float diffuse = 0.3;'
+#10'uniform float specular = 0.25;'
+#10'uniform float shininess = 10.0;'
+#10'uniform float overlayDepth = 0.3;'
+#10'uniform int overlays = 0;'
+#10'uniform float backAlpha = 0.5;'
+#10'vec3 GetBackPosition (vec3 startPosition) { //when does ray exit unit cube http://prideout.net/blog/?p=64'
+#10'	vec3 invR = 1.0 / rayDir;'
+#10'    vec3 tbot = invR * (vec3(0.0)-startPosition);'
+#10'    vec3 ttop = invR * (vec3(1.0)-startPosition);'
+#10'    vec3 tmax = max(ttop, tbot);'
+#10'    vec2 t = min(tmax.xx, tmax.yz);'
+#10'	return startPosition + (rayDir * min(t.x, t.y));'
+#10'}'
+#10'void main() {'
+#10'    vec3 start = TexCoord1.xyz;'
+#10'	vec3 backPosition = GetBackPosition(start);'
+#10'	vec3 dir = backPosition - start;'
+#10'	float len = length(dir);'
+#10'	dir = normalize(dir);'
+#10'	vec3 deltaDir = dir * stepSize;'
+#10'	vec4 gradSample, colorSample;'
+#10'	float stepSizeX2 = stepSize * 2.0; //avoid specular effects in clip plane'
+#10'	float bgNearest = len; //assume no hit'
+#10'	float overNearest = bgNearest;'
+#10''
+#10'	vec4 overAcc = vec4(0.0,0.0,0.0,0.0);'
+#10'	vec4 colAcc = vec4(0.0,0.0,0.0,0.0);'
+#10'	vec4 prevGrad = vec4(0.0,0.0,0.0,0.0);'
+#10'	float lengthAcc = 0.0;'
+#10'	vec3 samplePos;'
+#10'	//overlay pass'
+#10'	if ( overlays > 0 ) {'
+#10'samplePos = start.xyz +deltaDir* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453));'
+#10''
+#10'while (lengthAcc <= len) {'
+#10'	colorSample = texture(intensityOverlay,samplePos);'
+#10'	colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);'
+#10'	vec3 a = colorSample.rgb * ambient;'
+#10'	float s =  0;'
+#10'	vec3 d = vec3(0.0, 0.0, 0.0);'
+#10'	if ((colorSample.a > 0.01) && (lengthAcc > stepSizeX2)) {'
+#10'		bgNearest = min(lengthAcc,bgNearest);'
+#10'		//gradient based lighting http://www.mccauslandcenter.sc.edu/mricrogl/gradients'
+#10'		gradSample = texture(gradientOverlay,samplePos); //interpolate gradient direction and magnitude'
+#10'		gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);'
+#10'		//reusing Normals http://www.marcusbannerman.co.uk/articles/VolumeRendering.html'
+#10'		if (gradSample.a < prevGrad.a)'
+#10'			gradSample.rgb = prevGrad.rgb;'
+#10'		prevGrad = gradSample;'
+#10'		float lightNormDot = dot(gradSample.rgb, lightPosition);'
+#10'		d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;'
+#10'		s =   specular * pow(max(dot(reflect(lightPosition, gradSample.rgb), dir), 0.0), shininess);'
+#10''
+#10'	}'
+#10'	colorSample.rgb = a + d + s;'
+#10'	colorSample.rgb *= colorSample.a;'
+#10'	colAcc= (1.0 - colAcc.a) * colorSample + colAcc;'
+#10'	samplePos += deltaDir;'
+#10'	lengthAcc += stepSize;'
+#10'	if ( lengthAcc >= len || colAcc.a > 0.95 )'
+#10'		break;'
+#10'} //while lengthAcc < len'
+#10'colAcc.a = colAcc.a/0.95;'
+#10'overAcc = colAcc; //color accumulated by overlays'
+#10'overNearest = bgNearest;'
+#10'//clear values for background'
+#10'colAcc = vec4(0.0,0.0,0.0,0.0);'
+#10'prevGrad = vec4(0.0,0.0,0.0,0.0);'
+#10'lengthAcc = 0.0;'
+#10'	} //if overlayNum > 0'
+#10'	bgNearest = len; //assume no hit'
+#10'	//end ovelay pass clip plane applied to background ONLY...'
+#10'	samplePos = start.xyz +deltaDir* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453));'
+#10'	if (clipPlane.a > -0.5) {'
+#10'bool frontface = (dot(dir , clipPlane.xyz) > 0.0);'
+#10'float dis = dot(dir,clipPlane.xyz);'
+#10'if (dis != 0.0  )  dis = (-clipPlane.a - dot(clipPlane.xyz, start.xyz-0.5)) / dis;'
+#10'//test: "return" fails on 2006MacBookPro10.4ATI1900, "discard" fails on MacPro10.5NV8800'
+#10'if (((frontface) && (dis >= len)) || ((!frontface) && (dis <= 0.0)))'
+#10'	lengthAcc = len + 1.0; //no background'
+#10'else if ((dis > 0.0) && (dis < len)) {'
+#10'	if (frontface) {'
+#10'		lengthAcc = dis;'
+#10'		//stepSizeX2 = dis;'
+#10'		samplePos += dir * dis;'
+#10'		//len -= dir * dis;'
+#10'	} else {'
+#10'		backPosition =  start + dir * (dis);'
+#10'		len = length(backPosition - start);'
+#10'	}'
+#10'}'
+#10'	}'
+#10'	stepSizeX2 = lengthAcc + (stepSize * 2.0);'
+#10'	while (lengthAcc <= len) {'
+#10'colorSample = texture(intensityVol,samplePos);'
+#10'colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);'
+#10'vec3 a = colorSample.rgb * ambient;'
+#10'float s =  0;'
+#10'vec3 d = vec3(0.0, 0.0, 0.0);'
+#10'if ((colorSample.a > 0.01) && (lengthAcc > stepSizeX2)) {'
+#10'	bgNearest = min(lengthAcc,bgNearest);'
+#10'	//gradient based lighting http://www.mccauslandcenter.sc.edu/mricrogl/gradients'
+#10'	gradSample= texture(gradientVol,samplePos);'
+#10'	gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);'
+#10'	//reusing Normals http://www.marcusbannerman.co.uk/articles/VolumeRendering.html'
+#10'	if (gradSample.a < prevGrad.a)'
+#10'		gradSample.rgb = prevGrad.rgb;'
+#10'	prevGrad = gradSample;'
+#10'	float lightNormDot = dot(gradSample.rgb, lightPosition);'
+#10'	d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;'
+#10'	s =   specular * pow(max(dot(reflect(lightPosition, gradSample.rgb), dir), 0.0), shininess);'
+#10'}'
+#10'colorSample.rgb = a + d + s;'
+#10'colorSample.rgb *= colorSample.a;'
+#10'colAcc= (1.0 - colAcc.a) * colorSample + colAcc;'
+#10'samplePos += deltaDir;'
+#10'lengthAcc += stepSize;'
+#10'if ( lengthAcc >= len || colAcc.a > 0.95 )'
+#10'	break;'
+#10'	} //while lengthAcc < len'
+#10'	colAcc.a = colAcc.a/0.95;'
+#10'	colAcc.a *= backAlpha;'
+#10'	//if (overAcc.a > 0.0) { //<- conditional not required: overMix always 0 for overAcc.a = 0.0'
+#10'float overMix = overAcc.a;'
+#10'if ((overNearest > bgNearest) && (colAcc.a > 0.0)) { //background (partially) occludes overlay'
+#10'	//max distance between two vertices of unit cube is 1.73'
+#10'	float dx = (overNearest - bgNearest)/1.73;'
+#10'	dx = colAcc.a * pow(dx, overlayDepth);'
+#10'	overMix *= 1.0 - dx;'
+#10'}'
+#10'colAcc.rgb = mix(colAcc.rgb, overAcc.rgb, overMix);'
+#10'colAcc.a = max(colAcc.a, overAcc.a);'
+#10'	//}'
+#10'    FragColor = colAcc;'
+#10'}';

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

procedure TGPUVolume.SetMatCap(lFilename: string);
begin
  glControl.MakeCurrent();
  LoadMatCap(lFilename, matcap2D);
  glControl.ReleaseContext;
end;
{$ENDIF}

procedure TGPUVolume.SetShader(shaderName: string);
var
  VertexProgram, FragmentProgram: string;
  i : integer;
begin
  glControl.MakeCurrent();
  glUseProgram(0);
  if (programRaycast <> 0) then glDeleteProgram(programRaycast);
  loadVertFrag(shaderName, VertexProgram, FragmentProgram);
  if VertexProgram = '' then VertexProgram := kVert;
  if FragmentProgram = '' then FragmentProgram := kFrag;
  programRaycast :=  initVertFrag(VertexProgram, FragmentProgram);
  //imvLoc := glGetUniformLocation(programRaycast, pAnsiChar('ModelViewMatrixInverse'));
  mvpLoc := glGetUniformLocation(programRaycast, pAnsiChar('ModelViewProjectionMatrix'));
  rayDirLoc := glGetUniformLocation(programRaycast, pAnsiChar('rayDir'));
  sliceSizeLoc := glGetUniformLocation(programRaycast, pAnsiChar('sliceSize'));
  stepSizeLoc := glGetUniformLocation(programRaycast, pAnsiChar('stepSize'));
  backAlphaLoc := glGetUniformLocation(programRaycast, pAnsiChar('backAlpha'));
  matcapLoc := glGetUniformLocation(programRaycast, pAnsiChar('matcap2D'));
  {$IFDEF MATCAP}
  normLoc := glGetUniformLocation(programRaycast, pAnsiChar('NormalMatrix'));
  //printf(format('%d %s--->matcap @ %d = %d', [normLoc, shaderName, matcapLoc, matcap2D]));
  {$ENDIF}
  loopsLoc := glGetUniformLocation(programRaycast, pAnsiChar('loops'));
  overlaysLoc := glGetUniformLocation(programRaycast, pAnsiChar('overlays'));
  lightPositionLoc := glGetUniformLocation(programRaycast, pAnsiChar('lightPosition'));
  clipPlaneLoc :=  glGetUniformLocation(programRaycast, pAnsiChar('clipPlane'));
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

procedure TGPUVolume.Prepare(shaderName: string);
var
 success: boolean;
begin
  glControl.MakeCurrent();
  if (shaderName = '') or (not fileexists(shaderName)) then
     shaderName := ShaderDir+pathdelim+'Default.glsl';
  if (not fileexists(shaderName)) then
     shaderName := '';
  SetShader(shaderName);
  {$IFDEF GPUGRADIENTS}
  programBlur := initVertFrag(kBlurSobelVert,kBlurFrag);
  programSobel := initVertFrag(kBlurSobelVert,kSobelFrag);
  {$ENDIF}
  {$IFDEF VIEW2D}
  colorEditor := TColorEditor.Create();
  //ShowColorEditor := true;
  //line program
  programLine2D := initVertFrag(kVertLine2D,kFragLine2D);
  glUseProgram(programLine2D);
  uniform_viewportSizeLine := glGetUniformLocation(programLine2D, pAnsiChar('ViewportSize'));
  //setup VAO for Tex
  vboTex2D := 0;
  vaoTex2D := 0;
  vboBox3D := 0;
  glGenVertexArrays(1, @vaoTex2D);
  vboTex2D := 0;
  glGenBuffers(1, @vboTex2D);
  //glBindBuffer(GL_ARRAY_BUFFER, vboTex2D);
  //glBufferData(GL_ARRAY_BUFFER,slices2D.NumberOfLineVertices*SizeOf(TVertex2D), @slices2D.LineVertices[0], GL_STATIC_DRAW);
  //glBindBuffer(GL_ARRAY_BUFFER, 0);
  // Prepare vertrex array object (VAO)
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
  //2D program
  programTex2D := initVertFrag(kVertTex2D,kFragTex2D);
  glUseProgram(programTex2D);
  uniform_tex := glGetUniformLocation(programTex2D, pAnsiChar('tex'));
  uniform_overlay := glGetUniformLocation(programTex2D, pAnsiChar('overlay'));
  uniform_drawTex := glGetUniformLocation(programTex2D, pAnsiChar('drawTex'));
  uniform_drawLUT := glGetUniformLocation(programTex2D, pAnsiChar('drawLUT'));
  uniform_drawAlpha := glGetUniformLocation(programTex2D, pAnsiChar('drawAlpha'));
  uniform_backAlpha := glGetUniformLocation(programTex2D, pAnsiChar('backAlpha'));
  uniform_viewportSizeTex := glGetUniformLocation(programTex2D, pAnsiChar('ViewportSize'));
  CreateDrawTex(pti(4,4,4), nil);
  CreateDrawColorTable;
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
  Txt := TGPUFont.Create(ResourceDir+pathdelim+'Roboto.png',  success, glControl); //<-multi-channel channel fonts glmtext
  slices2D := TSlices2D.Create(Txt);
  //slices2D := TSlices2D.Create();
  {$ENDIF}
  LoadCube();
  {$IFDEF CUBE} gCube := TGPUCube.Create(glControl); {$ENDIF}
  glControl.ReleaseContext;
  if GLErrorStr <> '' then
     showmessage(GLErrorStr);
end;

procedure TGPUVolume.SetColorBar(fromColorbar: TGPUClrbar);
begin
     clrbar := fromColorbar;
end;

constructor TGPUVolume.Create(fromView: TOpenGLControl);
begin
  glControl := fromView;
  clrbar := nil;
  fDistance := kDefaultDistance;
  fAzimuth := 110;
  fElevation := 30;
  overlayNum := 0;
  overlayGradTexWidth := 2; //refresh
  RaycastQuality1to10 := 5;
  SelectionRect := Vec4(-1,0,0,0);
  fLightPos := Vec4(0, 0.707, 0.707, 0);
  fClipPlane := Vec4(0, 0, 0, -1);
  vao:= 0;
  matcap2D := 0;
  overlayGradientTexture3D := 0;
  overlayIntensityTexture3D := 0;
  drawTexture1D := 0;
  drawTexture3D := 0;
  gradientTexture3D := 0;
  intensityTexture3D := 0;
  colorEditorVisible := false;
  isSmooth2D := true;
  shaderPrefs.nUniform:= 0;
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
    vbo_point: gluint;
begin  //vboCube, vaoCube,
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
end;

procedure TGPUVolume.CreateOverlayTextures(Dim: TVec3i; volRGBA: TRGBAs);
var
   gradData: TRGBAs;
begin
  //GLForm1.LayerBox.Caption := ':>>'+inttostr(random(888));
  if (overlayIntensityTexture3D <> 0) then glDeleteTextures(1,@overlayIntensityTexture3D);
  if (overlayGradientTexture3D <> 0) then glDeleteTextures(1,@overlayGradientTexture3D);
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
    SetLength (gradData, Dim.X*Dim.Y*Dim.Z);
    glTexImage3D(GL_TEXTURE_3D, 0,GL_RGBA, Dim.X, Dim.Y, Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE,@gradData[0]);
    gradData := nil;
    CreateGradientVolumeGPU (Dim.X, Dim.Y, Dim.Z, overlayIntensityTexture3D, overlayGradientTexture3D);
    {$ELSE}
    //Vol.CreateGradientVolume (volRGBA, Dim.X, Dim.Y, Dim.Z, gradData);
    CreateGradientVolumeX (TUInt8s(volRGBA), Dim.X, Dim.Y, Dim.Z, 1, gradData);
    glTexImage3D(GL_TEXTURE_3D, 0,GL_RGBA, Dim.X, Dim.Y, Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE,@gradData[0]);
    gradData := nil;
    //Form1.Caption := 'CPU gradients '+inttostr(MilliSecondsBetween(Now,startTime))+' ms ';
    {$ENDIF}
  end else
      glTexImage3D(GL_TEXTURE_3D, 0,GL_RGBA, Dim.X, Dim.Y, Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE,@volRGBA[0]);
 overlayGradTexWidth := Dim.X;
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
      //GLForm1.LayerBox.Caption := '>>>'+inttostr(random(888));
    if (overlayGradientTexture3D = 0) or (overlayGradTexWidth > 1) then //e.g. update texture after user closes overlays
       CreateOverlayTextures(Vol.Dim, nil); // <- empty overlay texture
    exit; //no overlays
  end;
  if not vols.OverlaysNeedsUpdate then exit;
  intensityData := vols.CreateOverlayVolume;
  CreateOverlayTextures(Vol.Dim, intensityData);
end;

function TGPUVolume.LoadTexture(var vol: TNIfTI): boolean;
var
 i: GLint;
 gradData: TRGBAs;
 //startTime : TDateTime;
begin
 result := true;
 glControl.MakeCurrent();
 if (Vol.VolRGBA = nil) then exit;
 if (intensityTexture3D <> 0) then glDeleteTextures(1,@intensityTexture3D);
 if (gradientTexture3D <> 0) then glDeleteTextures(1,@gradientTexture3D);
 //next: see if our video card can show this texture
 glTexImage3D(GL_PROXY_TEXTURE_3D, 0, GL_RGBA, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE, NIL);
 glGetTexLevelParameteriv(GL_PROXY_TEXTURE_3D, 0, GL_TEXTURE_WIDTH, @i);
 //next copy the image to the GPU
 glPixelStorei(GL_UNPACK_ALIGNMENT,1);
 glGenTextures(1, @intensityTexture3D);
 glBindTexture(GL_TEXTURE_3D, intensityTexture3D);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_BORDER);
 (*  //CLAMP_TO_BORDER avoids pinochio nose when panning
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);   //GL_CLAMP_TO_EDGE
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE); *)
 glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE, @Vol.VolRGBA[0]);
 glPixelStorei(GL_UNPACK_ALIGNMENT,1);
 glGenTextures(1, @gradientTexture3D);
 glBindTexture(GL_TEXTURE_3D, gradientTexture3D);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_BORDER);
 //startTime := Now;
 {$IFDEF GPUGRADIENTS}
 SetLength (gradData, Vol.Dim.X*Vol.Dim.Y*Vol.Dim.Z);
 glTexImage3D(GL_TEXTURE_3D, 0,GL_RGBA, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE,@gradData[0]);
 gradData := nil;
 CreateGradientVolumeGPU (Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, intensityTexture3D, gradientTexture3D);
 {$ELSE}
 gradData := Vol.GenerateGradientVolume;
 //CreateGradientVolume (Vol.VolRGBA, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, gradData);
 glTexImage3D(GL_TEXTURE_3D, 0,GL_RGBA, Vol.Dim.X, Vol.Dim.Y,Vol.Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE,@gradData[0]);
 gradData := nil;
 //Form1.Caption := 'CPU gradients '+inttostr(MilliSecondsBetween(Now,startTime))+' ms ';
 {$ENDIF}
 maxDim := max(Vol.Dim.X,max(Vol.Dim.Y,Vol.Dim.Z));
 Vol.GPULoadDone;
 if (overlayIntensityTexture3D = 0) then CreateOverlayTextures(Vol.Dim, nil); //load blank overlay
end;

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
function TGPUVolume.Slice2Dmm(var vol: TNIfTI; out vox: TVec3i): TVec3;
begin
    result := slices2D.FracMM(vol.Mat, vol.Dim, vox);
end;

procedure TGPUVolume.SetSlice2DFrac(frac : TVec3);
begin
     if (frac.x < 0.0) or (frac.y < 0.0) or (frac.z < 0.0) then exit;
     slices2D.sliceFrac := frac;
end;

function TGPUVolume.GetSlice2DMaxXY(mouseX, mouseY: integer; var Lo: TPoint): TPoint;
begin
  result := slices2D.GetSlice2DMaxXY(mouseX, mouseY, lo);
end;

function TGPUVolume.GetSlice2DFrac(mouseX, mouseY: integer; out Orient: integer): TVec3;
begin
    result := slices2D.GetSlice2DFrac(mouseX, mouseY, Orient);
end;

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
  glUseProgram(programRaycast);
  glBindFramebuffer(GL_FRAMEBUFFER, 0); //draw to screen
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
  glUniform1f(stepSizeLoc, ComputeStepSize(RayCastQuality1to10, maxDim)) ;
  if vol.IsLabels then
     glUniform1f(backAlphaLoc, 1)
   else
     glUniform1f(backAlphaLoc, vol.OpacityPercent/100);
  glUniform1f(sliceSizeLoc, 1/maxDim);
  glUniform1i(loopsLoc,round(maxDim*2.2));
  {$IFDEF MATCAP}
  //printf(format('>>matcapLoc %d matcap %d',[matcapLoc, matcap2D]));
  if (matcapLoc >= 0) and (matcap2D > 0) then begin
    modelMatrix := TMat4.Identity;
    modelMatrix *= TMat4.Translate(0, 0, -fDistance);
    modelMatrix *= TMat4.RotateX(-DegToRad(90-lElevation));
    modelMatrix *= TMat4.RotateZ(DegToRad(lAzimuth));
    modelMatrix *= TMat4.Translate(-vol.Scale.X/2, -vol.Scale.Y/2, -vol.Scale.Z/2);
    modelLightPos := (modelMatrix.Transpose * fLightPos);
    modelMatrix *= TMat4.Scale(vol.Scale.X, vol.Scale.Y, vol.Scale.Z); //for volumes that are rectangular not square

    glActiveTexture(GL_TEXTURE6);
    glBindTexture(GL_TEXTURE_2D, matcap2D);
    glUniform1i(matcapLoc, 6);
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
  end;
  {$ENDIF}

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

  projectionMatrix := TMat4.OrthoGL (0, glControl.clientwidth, 0, glControl.clientheight, 0.01, 2);
  glUniform3f(lightPositionLoc,modelLightPos.x, modelLightPos.y, modelLightPos.z);
  glUniform4f(clipPlaneLoc, fClipPlane.x, fClipPlane.y, fClipPlane.z, fClipPlane.w);
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
  glBindVertexArray(vao);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboBox3D);
  {$IFDEF STRIP}
  glCullFace(GL_BACK);
  glDrawElements(GL_TRIANGLE_STRIP, 14, GL_UNSIGNED_INT, nil);
  {$ELSE}
  glCullFace(GL_FRONT);
  glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, nil);
  {$ENDIF}
  glDisable(GL_CULL_FACE);
end;

procedure TGPUVolume.PaintMosaic2D(var vol: TNIfTI; Drawing: TDraw; MosaicString: string);
var
  i: integer;
  w,h, f: single;
begin
  if vao = 0 then // only once
    LoadCube();
  if (vol.VolRGBA <> nil) then
     LoadTexture(vol);
  if (intensityTexture3D = 0) then
    exit;
  UpdateDraw(Drawing);
  glBindFramebuffer(GL_FRAMEBUFFER, 0); //draw to screen
  glBindFramebuffer(GL_RENDERBUFFER, 0); //draw to screen
  w := glControl.clientwidth;
  h := glControl.clientheight;
  //load
  if clrbar <> nil then begin
       f := clrbar.PanelFraction;
       if (f > 0.0) and (f < 0.5) then begin
          if (clrbar.isVertical) then
             w := w - (w * f)
          else
             h := h - (h * f);
       end;
  end;
  slices2D.UpdateMosaic(MosaicString, vol.Mat, vol.InvMat, vol.Dim, vol.Scale, w,h);
  w := glControl.clientwidth;
  h := glControl.clientheight;

  if (slices2D.NumberOfVertices < 3) and (slices2D.NumberOfMosaicRenders < 1) then exit; //nothing to do
  //draw
  //glViewport(0, 0, glControl.ClientWidth, glControl.ClientHeight); //required for form resize
  glControl.SetViewport();
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glBindFramebuffer(GL_FRAMEBUFFER, 0); //draw to screen
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
    if vol.IsLabels then
       glUniform1f(uniform_backAlpha, 1)
    else
        glUniform1f(uniform_backAlpha, vol.OpacityPercent/100);
    glUniform2f(uniform_viewportSizeTex, w, h);
    glBindBuffer(GL_ARRAY_BUFFER, vboTex2D);
    glBufferData(GL_ARRAY_BUFFER,slices2D.NumberOfVertices*SizeOf(TVertex2D), @slices2D.SliceVertices[0], GL_STATIC_DRAW);
    glBindVertexArray(vaoTex2d);
    glDrawArrays(GL_TRIANGLES, 0, slices2D.NumberOfVertices);
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
    glBindBuffer(GL_ARRAY_BUFFER, vboLine2D);
    glBufferData(GL_ARRAY_BUFFER,slices2D.NumberOfLineVertices*SizeOf(TVertex2D), @slices2D.LineVertices[0], GL_STATIC_DRAW);
    glBindVertexArray(vaoLine2D);
    glDrawArrays(GL_TRIANGLES, 0, slices2D.NumberOfLineVertices);
  end;
  if clrbar <> nil then
   clrbar.Draw();
   txt.DrawText();
  glControl.SwapBuffers;
end;

procedure TGPUVolume.Paint2D(var vol: TNIfTI; Drawing: TDraw; DisplayOrient: integer);
var
 w,h: single;
begin
  if vao = 0 then // only once
    LoadCube();
  if (vol.VolRGBA <> nil) then
     LoadTexture(vol);
  if (intensityTexture3D = 0) then
    exit;
  UpdateDraw(Drawing);
  //if vao = 0 then // only once
  //  LoadCube(fromView);
  if (vol.VolRGBA <> nil) then
     LoadTexture(vol);
  if (intensityTexture3D = 0) then
    exit;
  w := glControl.clientwidth;
  h := glControl.clientheight;
  //load
  if (clrbar <> nil) and (clrbar.PanelFraction < 1.0) and (clrbar.PanelFraction > 0.0) then begin
     if (clrbar.isVertical) then
        w := round(w * (1.0-clrbar.PanelFraction))
     else
         h := round(h * (1.0-clrbar.PanelFraction));
   slices2D.Update(vol.Scale, w, h, DisplayOrient, glControl.clientheight);
   w := glControl.clientwidth;
   h := glControl.clientheight;
  end else
      slices2D.Update(vol.Scale, w, h, DisplayOrient);
  if SelectionRect.x > 0 then
     slices2D.DrawOutLine(SelectionRect.X,h-SelectionRect.Y, SelectionRect.Z,h-SelectionRect.W);
  if slices2D.NumberOfVertices < 3 then exit; //nothing to do
  //select nearest neighbor vs linear interpolation
  (*if not isSmooth2D then begin
    glBindTexture(GL_TEXTURE_3D, intensityTexture3D);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  end;*)
  //draw
  //glViewport(0, 0, glControl.ClientWidth, glControl.ClientHeight); //required for form resize
  glControl.SetViewport();
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glBindFramebuffer(GL_FRAMEBUFFER, 0); //draw to screen
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
  if vol.IsLabels then
     glUniform1f(uniform_backAlpha, 1)
  else
      glUniform1f(uniform_backAlpha, vol.OpacityPercent/100);
  glUniform2f(uniform_viewportSizeTex, w, h);
  glBindBuffer(GL_ARRAY_BUFFER, vboTex2D);
  glBufferData(GL_ARRAY_BUFFER,slices2D.NumberOfVertices*SizeOf(TVertex2D), @slices2D.SliceVertices[0], GL_STATIC_DRAW);
  glBindVertexArray(vaoTex2d);
  glDrawArrays(GL_TRIANGLES, 0, slices2D.NumberOfVertices);
  if (slices2D.NumberOfLineVertices > 0) then begin //draw 2D lines
    glUseProgram(programLine2D);
    glUniform2f(uniform_viewportSizeLine, w, h);
    glBindBuffer(GL_ARRAY_BUFFER, vboLine2D);
    glBufferData(GL_ARRAY_BUFFER,slices2D.NumberOfLineVertices*SizeOf(TVertex2D), @slices2D.LineVertices[0], GL_STATIC_DRAW);
    glBindVertexArray(vaoLine2D);
    glDrawArrays(GL_TRIANGLES, 0, slices2D.NumberOfLineVertices);
  end;
  //draw color editor
  if colorEditorVisible then begin
    colorEditor.Update(w, h, vol);
    if colorEditor.NumberOfLineVertices > 2 then begin
        glUseProgram(programLine2D);
        glUniform2f(uniform_viewportSizeLine, w, h);
        glBindBuffer(GL_ARRAY_BUFFER, vboLine2D);
        glBufferData(GL_ARRAY_BUFFER,colorEditor.NumberOfLineVertices*SizeOf(TVertex2D), @colorEditor.LineVertices[0], GL_STATIC_DRAW);
        glBindVertexArray(vaoLine2D);
        glDrawArrays(GL_TRIANGLES, 0, colorEditor.NumberOfLineVertices);
    end;
  end;
  txt.DrawText(); //D888
  if clrbar <> nil then
   clrbar.Draw();
  glControl.SwapBuffers;
  //reset linear interpolation - much better for rendering and mosaics
  if not isSmooth2D then begin
      glBindTexture(GL_TEXTURE_3D, intensityTexture3D);
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glBindTexture(GL_TEXTURE_3D, overlayIntensityTexture3D);
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  end;
end;
{$ENDIF}

procedure TGPUVolume.Paint(var vol: TNIfTI);
var
  //modelViewProjectionMatrixInverse,
 normalMatrix,
  modelViewProjectionMatrix, projectionMatrix, modelMatrix: TMat4;
  nMtx: array [0..8] of single;
  modelLightPos, v, rayDir: TVec4;
  w,h, whratio, scale: single;
  i: integer;
begin
  if vao = 0 then // only once
    LoadCube();
  if (vol.VolRGBA <> nil) then
     LoadTexture(vol);
  if (intensityTexture3D = 0) then
    exit;
  glUseProgram(programRaycast);
  glBindFramebuffer(GL_FRAMEBUFFER, 0); //draw to screen
  //bind background intensity (color)
  glActiveTexture(GL_TEXTURE2);
  glBindTexture(GL_TEXTURE_3D, intensityTexture3D);
  glUniform1i(intensityVolLoc, 2);
  //bind background gradient (edges)
  glActiveTexture(GL_TEXTURE3);
  glBindTexture(GL_TEXTURE_3D, gradientTexture3D);
  glUniform1i(gradientVolLoc, 3);
  //bind overlay intensity (color)
  glUniform1i(overlaysLoc, overlayIntensityTexture3D); //0 if no overlays
  glActiveTexture(GL_TEXTURE4);
  glBindTexture(GL_TEXTURE_3D, overlayIntensityTexture3D);
  glUniform1i(overlayIntensityVolLoc, 4);
  //bind background gradient  (edges)
  glActiveTexture(GL_TEXTURE5);
  glBindTexture(GL_TEXTURE_3D, overlayGradientTexture3D);
  glUniform1i(overlayGradientVolLoc, 5);
  //bind other uniforms
  glUniform1f(stepSizeLoc, ComputeStepSize(RayCastQuality1to10, maxDim)) ;
  if vol.IsLabels then
     glUniform1f(backAlphaLoc, 1)
  else
   glUniform1f(backAlphaLoc, vol.OpacityPercent/100);
  glUniform1f(sliceSizeLoc, 1/maxDim);
  glUniform1i(loopsLoc,round(maxDim*2.2));
  //glUniform3f(clearColorLoc, fClearColor.r/255, fClearColor.g/255, fClearColor.b/255);
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
  whratio := glControl.clientwidth/glControl.clientheight;
  if (whratio > 1) or (whratio = 0) then //Wide window
     projectionMatrix := TMat4.OrthoGL (-scale * whratio, scale * whratio, -scale, scale, fDistance-1, fDistance+1)
  else
      projectionMatrix := TMat4.OrthoGL (-scale, scale, -scale/whratio, scale/whratio, fDistance-1, fDistance+1);
  glUniform3f(lightPositionLoc,modelLightPos.x, modelLightPos.y, modelLightPos.z);
  glUniform4f(clipPlaneLoc, fClipPlane.x, fClipPlane.y, fClipPlane.z, fClipPlane.w);
  if (shaderPrefs.nUniform > 0) then
     for i := 1 to shaderPrefs.nUniform do
         glUniform1f(prefLoc[i], shaderPrefs.Uniform[i].DefaultV);

  //bind matcap
  {$IFDEF MATCAP}
  //printf(format('>>matcapLoc %d matcap %d',[matcapLoc, matcap2D]));
  if (matcapLoc >= 0) and (matcap2D > 0) then begin
     glActiveTexture(GL_TEXTURE6);
     glBindTexture(GL_TEXTURE_2D, matcap2D);
     glUniform1i(matcapLoc, 6);
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
  end;
  {$ENDIF}
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
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_CULL_FACE);
  glBindVertexArray(vao);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboBox3D);
  {$IFDEF STRIP}
  glCullFace(GL_BACK);
  glDrawElements(GL_TRIANGLE_STRIP, 14, GL_UNSIGNED_INT, nil);
  {$ELSE}
  glCullFace(GL_FRONT);
  glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, nil);
  {$ENDIF}
  glDisable(GL_CULL_FACE);
  //draw color editor

  if clrbar <> nil then
   clrbar.Draw();
  if colorEditorVisible then begin
     w := glControl.clientwidth;
     h := glControl.clientheight;
    colorEditor.Update(w, h, vol);
    if colorEditor.NumberOfLineVertices > 2 then begin
        glUseProgram(programLine2D);
        glUniform2f(uniform_viewportSizeLine, w, h);
        glBindBuffer(GL_ARRAY_BUFFER, vboLine2D);
        glBufferData(GL_ARRAY_BUFFER,colorEditor.NumberOfLineVertices*SizeOf(TVertex2D), @colorEditor.LineVertices[0], GL_STATIC_DRAW);
        glBindVertexArray(vaoLine2D);
        glDrawArrays(GL_TRIANGLES, 0, colorEditor.NumberOfLineVertices);
    end;
  end;
  {$IFDEF CUBE}
  if Slices.LabelOrient then begin
     //gCube.Size := 0.02;
     gCube.Azimuth:=fAzimuth;
     gCube.Elevation:=-fElevation;
     gCube.Draw(glControl.ClientWidth, glControl.ClientHeight);
  end;
  {$ENDIF}
  glControl.SwapBuffers;
end;

end.

