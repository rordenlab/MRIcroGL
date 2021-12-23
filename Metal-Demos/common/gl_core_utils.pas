unit gl_core_utils;
interface

{$IFDEF Darwin}
  //{$mode objfpc}
 // {$modeswitch objectivec1}
{$ENDIF}
{$include glopts.inc}
uses
  //{$IFDEF Darwin} CocoaAll, MacOSAll, {$ENDIF}
  {$IFDEF COREGL}
  glcorearb,

  {$ELSE}
  gl, glext,
  {$ENDIF}
  {$IFDEF LCLCocoa}retinahelper,{$ENDIF}
  Dialogs, clipbrd,
  SysUtils, OpenGLContext, Graphics, lcltype, LCLIntf, GraphType;
  procedure  loadVertFrag(shaderName: string; out VertexProgram, FragmentProgram: string; fragHeader: string = '');
  function  initVertFrag(vert, frag: string): GLuint;
  procedure GetError(p: integer; str: string = '');  //report OpenGL Error
  function ScreenShot(GLBox : TOpenGLControl; ScreenCaptureTransparentBackground: boolean = false): TBitmap;
  procedure SaveBmp(pngName: string; GLBox : TOpenGLControl; ScreenCaptureTransparentBackground: boolean = false);
  procedure ScreenToClipBoard(GLBox : TOpenGLControl; ScreenCaptureTransparentBackground: boolean = false);
var
   GLErrorStr: string = '';

implementation

function ScreenShot(GLBox : TOpenGLControl; ScreenCaptureTransparentBackground: boolean = false): TBitmap;
var
  RawImage: TRawImage;
  p: array of byte;
  w, h, x, y, BytePerPixel: integer;
  z: int64;
  DestPtr: PInteger;
  maxXY : array[0..1] of GLuint;
begin
 GLBox.MakeCurrent;
 glGetIntegerv(GL_MAX_VIEWPORT_DIMS, @maxXY);  //GL_MAX_TEXTURE_SIZE
  w := GLBox.ClientWidth;
  h := GLBox.ClientHeight;
 Result:=TBitmap.Create;
 Result.Width:=w;
 Result.Height:=h;
 if ScreenCaptureTransparentBackground then
   Result.PixelFormat := pf32bit
 else
     Result.PixelFormat := pf24bit; //if pf32bit the background color is wrong, e.g. when alpha = 0
 RawImage := Result.RawImage;
 BytePerPixel := RawImage.Description.BitsPerPixel div 8;
 setlength(p, 4*w* h);
 glFlush;
 glFinish;//<-this would pause until all jobs finished: generally a bad idea! required here

 {$IFDEF Darwin} //http://lists.apple.com/archives/mac-opengl/2006/Nov/msg00196.html
 glReadPixels(0, 0, w, h, $80E1, $8035, @p[0]); //OSX-Darwin   GL_BGRA = $80E1;  GL_UNSIGNED_INT_8_8_8_8_EXT = $8035;
 {$ELSE}
  {$IFDEF Linux}
   glReadPixels(0, 0, w, h, $80E1, GL_UNSIGNED_BYTE, @p[0]); //https://github.com/rordenlab/MRIcroGL12/issues/9
  //  glReadPixels(0, 0, w, h, GL_RGBA, GL_UNSIGNED_BYTE, @p[0]); //Linux-Windows   GL_RGBA = $1908; GL_UNSIGNED_BYTE
  {$ELSE}
   glReadPixels(0, 0, w, h, $80E1, GL_UNSIGNED_BYTE, @p[0]); //Linux-Windows   GL_RGBA = $1908; GL_UNSIGNED_BYTE
  {$ENDIF}
 {$ENDIF}
 GLbox.ReleaseContext;
 z := 0;
 if BytePerPixel <> 4 then begin
   for y:= h-1 downto 0 do begin
        DestPtr := PInteger(RawImage.Data);
        Inc(PByte(DestPtr), y * RawImage.Description.BytesPerLine );
        for x := 1 to w do begin
            DestPtr^ := p[z] + (p[z+1] shl 8) + (p[z+2] shl 16);
            Inc(PByte(DestPtr), BytePerPixel);
            z := z + 4;
        end;
    end; //for y : each line in image
 end else begin
     for y:= h-1 downto 0 do begin
         DestPtr := PInteger(RawImage.Data);
         Inc(PByte(DestPtr), y * RawImage.Description.BytesPerLine );
         System.Move(p[z], DestPtr^, w * BytePerPixel );
         z := z + ( w * 4 );
   end; //for y : each line in image
 end;
 setlength(p, 0);
 //{$DEFINE ISOPAQUE} //see ScreenCaptureTransparentBackground
 {$IFDEF ISOPAQUE}
 DestPtr := PInteger(RawImage.Data);
 for z := 0 to ((w * h)-1) do begin
     DestPtr[z] := DestPtr[z] and $FFFFFF00;
 end;
 {$ENDIF}
 //GLbox.ReleaseContext;
end;

procedure ScreenToClipBoard(GLBox : TOpenGLControl; ScreenCaptureTransparentBackground: boolean = false);
var
  bmp: TBitmap;
begin
  bmp := ScreenShot(GLBox, ScreenCaptureTransparentBackground);
  if (bmp = nil) then exit;
   Clipboard.Assign(bmp);
  bmp.Free;
end;

procedure SaveBmp(pngName: string; GLBox : TOpenGLControl; ScreenCaptureTransparentBackground: boolean = false);
var
  bmp: TBitmap;
  PNG: TPortableNetworkGraphic;
begin
  bmp := ScreenShot(GLBox, ScreenCaptureTransparentBackground);
  if (bmp = nil) then exit;
  PNG := TPortableNetworkGraphic.Create;
  try
    PNG.Assign(bmp);    //Convert data into png
    PNG.SaveToFile(ChangeFileExt(pngName,'.png'));
  finally
         PNG.Free;
  end;
  bmp.Free;
end;

procedure  loadVertFrag(shaderName: string; out VertexProgram, FragmentProgram: string; fragHeader: string = '');
const
  //kCR = chr (13)+chr(10); //UNIX end of line
  //kCR = chr(10); //UNIX end of line
  knone=0;
  kpref=1;
  kvert = 2;
  kfrag = 3;
var
  mode: integer;
  F : TextFile;
  S: string;
begin
 FragmentProgram := '';
 VertexProgram := '';
 if not fileexists(shaderName) then begin
   {$IFNDEF UNIX}
   if (shaderName <> '') then showmessage('Unable to find '+  shaderName);
   {$ELSE}
   if (shaderName <> '') then writeln('Unable to find '+  shaderName);
   {$ENDIF}
   exit;
 end;
 mode := knone;
 FileMode := fmOpenRead;
 AssignFile(F,shaderName);
 Reset(F);
 while not Eof(F) do begin
   ReadLn(F, S);
   if S = '//pref' then
     mode := kpref
   else if S = '//frag' then begin
     mode := kfrag;
     FragmentProgram := fragHeader;
   end else if S = '//vert' then
     mode := kvert
   else if mode = kpref then begin
     //mode := kpref
   end else if mode = kfrag then begin
     //if PosEx('#version', S) > 0 then continue;
     FragmentProgram := FragmentProgram + S+#13#10 //kCR
   end else if mode = kvert then
     VertexProgram := VertexProgram + S+#13#10;
 end;//EOF
 CloseFile(F);
end;

procedure ReportErrorsGL(glObjectID: GLuint);
var
  s : string;
  maxLength : GLint;
begin
  glGetShaderiv(glObjectID, GL_INFO_LOG_LENGTH, @maxLength);
  if (maxLength < 2) then exit;
  setlength(s, maxLength);
  glGetShaderInfoLog(glObjectID, maxLength, @maxLength, @s[1]);
  s:=trim(s);
  if GLErrorStr = '' then begin
     GLErrorStr := 'GLSL error '+s;
     {$IFDEF UNIX}writeln(GLErrorStr); {$ENDIF}
  end;
end;

procedure GetError(p: integer; str: string = '');  //report OpenGL Error
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
 if GLErrorStr = '' then begin
    GLErrorStr := 'GLSL error '+str+s;
    {$IFDEF UNIX}writeln(GLErrorStr);{$ENDIF}
 end;
end;

function compileShaderOfType (shaderType: GLEnum;  shaderText: string): GLuint;
var
   status: GLint;
begin
     result := glCreateShader(shaderType);
     glShaderSource(result, 1, PChar(@shaderText), nil);
     glCompileShader(result);
     ReportErrorsGL(result);
     status := 0;
     glGetShaderiv(result, GL_COMPILE_STATUS, @status);
     if (status =  0) and (GLErrorStr = '') then begin //report compiling errors.
        GLErrorStr := 'GLSL shader compile failure';
     end;
end;

procedure ReportCompileProgramError(glObjectID: GLuint);
var
  s : string;
  maxLength : GLint;
begin
  glGetProgramiv(glObjectID, GL_LINK_STATUS, @maxLength);
  //if (maxLength = GL_TRUE) then exit;
  if (maxLength = 1) then exit; //DGL  GL_TRUE
  maxLength := 4096;
  setlength(s, maxLength);
  {$IFDEF OLDDGL} //older DGL
  glGetProgramInfoLog(glObjectID, maxLength, maxLength, @s[1]);
  {$ELSE}
  glGetProgramInfoLog(glObjectID, maxLength, @maxLength, @s[1]);
  {$ENDIF}
  if maxLength < 1 then begin
     if GLErrorStr = '' then
        GLErrorStr := ('Program compile error (unspecified)');
     exit
  end;
  s:=trim(s);
  if (length(s) < 2) then exit;
  if GLErrorStr = '' then
          GLErrorStr := ('Program compile error '+s);
end;

function  initVertFrag(vert, frag: string): GLuint;
var
   fs, vs: GLuint;
begin
  result := 0;
  vs := 0;
  glGetError(); //<- ignore proior errors
  //GetError(121); // <- report prior errors
  result := glCreateProgram();
  if (length(vert) > 0) then begin
     vs := compileShaderOfType(GL_VERTEX_SHADER, vert);
     if (vs = 0) then exit;
     glAttachShader(result, vs);
  end;
  fs := compileShaderOfType(GL_FRAGMENT_SHADER, frag);
  if (fs = 0) then exit;
  glAttachShader(result, fs);
  glLinkProgram(result);
  ReportCompileProgramError(result);
  if (length(vert) > 0) then begin
     glDetachShader(result, vs);
     glDeleteShader(vs);
  end;
  glDetachShader(result, fs);
  glDeleteShader(fs);
  //glUseProgram(result);
  GetError(123,'newShader');
  glGetError();
end;

function  initVertFragX(vert, frag: string): GLuint;
var
	fr, vt: GLuint;
begin
    result := 0;
    glGetError(); //<- ignore proior errors
    vt := compileShaderOfType(GL_VERTEX_SHADER, vert);
    fr := compileShaderOfType(GL_FRAGMENT_SHADER, frag);
    if (fr = 0) or (vt = 0) then exit;
    result := glCreateProgram();
    glAttachShader(result, vt);
    glAttachShader(result, fr);
    glBindFragDataLocation(result, 0, 'FragColor');
    glLinkProgram(result);
    glDeleteShader(vt);
    glDeleteShader(fr);
    GetError(1,'initX');
    glGetError(); //<- ignore proior errors

end;

end.

