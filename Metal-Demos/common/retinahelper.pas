unit retinahelper;
{$IFDEF LCLCocoa}
{$ModeSwitch objectivec1}
{$ENDIF}
interface

 {$include glopts.inc}



uses {$IFDEF COREGL}glcorearb, {$ELSE}gl, {$ENDIF} OpenGLContext;

type
  TCustomOpenGLControl = class helper for TOpenGLControl
    function clientWidth: Integer;
   function clientHeight: Integer;
   procedure setRetina(wantRetina: boolean);
   procedure enableTiledScreenShot(tileLeft, tileBottom,totalWidth, totalHeight: integer );
   procedure disableTiledScreenShot();
   procedure SetViewPort(); //same as glViewport(0,0,w,h) but handles tiled screenshots
   function retinaScale (isXnotY: boolean = true): Single;
   function tileLeft: Integer;
   function tileBottom: Integer;

end;

implementation
{$IFDEF LCLCocoa}
uses  glcocoanscontext, MacOSAll, CocoaAll;
{$ENDIF}

var
    fTileLeft, fTileBottom,fTotalWidth, fTotalHeight: integer;

function  TCustomOpenGLControl.tileLeft: integer;
begin
	exit(fTileLeft);
end;

function  TCustomOpenGLControl.tileBottom: integer;
begin
	exit(fTileBottom);
end;

procedure TCustomOpenGLControl.setRetina(wantRetina: boolean);
begin
  {$IFDEF LCLCocoa}
  LSetWantsBestResolutionOpenGLSurface(wantRetina, self.Handle);
  {$ENDIF}
end;

function TCustomOpenGLControl.ClientWidth: integer;
begin
  if (fTotalWidth > 0) then begin
    result := fTotalWidth;
    exit;
  end;
  {$IFDEF LCLCocoa}
  result := Round(width * retinaScale(true));
  {$ELSE}
  result := width;
  {$ENDIF}
end;


function TCustomOpenGLControl.ClientHeight: integer;
begin
  if (fTotalHeight > 0) then begin
    result := fTotalHeight;
    exit;
  end;
  {$IFDEF LCLCocoa}
  result := Round(height * retinaScale(false));
  {$ELSE}
   result := height;
  {$ENDIF}
end;

procedure TCustomOpenGLControl.SetViewPort();
var
    w,h: integer;
    {$IFDEF LCLCocoa}pt: NSPoint;{$ENDIF}
begin
  {$IFDEF LCLCocoa}
  //f := LBackingScaleFactor(self.Handle);
  //https://developer.apple.com/library/archive/documentation/GraphicsImaging/Conceptual/OpenGL-MacProgGuide/EnablingOpenGLforHighResolution/EnablingOpenGLforHighResolution.html
  pt.x := width;
  pt.y := height;
  pt := TCocoaOpenGLView(Handle).convertPointToBacking(pt);

  w := round(pt.x);//round(f*width);
  h := round(pt.y);//round(f*height);
  {$ELSE}
  w := width;
  h := height;
  {$ENDIF}
  if (fTotalHeight < 1) or (fTotalWidth < 1) then begin
       glViewport(0, 0, w, h);
      exit;
  end;
  //glViewport(fTileLeft, fTileBottom, w, h);
  glViewport(fTileLeft,fTileBottom, fTotalWidth, fTotalHeight);

end;

procedure TCustomOpenGLControl.enableTiledScreenShot(tileLeft, tileBottom, totalWidth, totalHeight: integer );
begin
  fTileLeft := tileLeft;
  fTileBottom := tileBottom;
  fTotalWidth := totalWidth;
  fTotalHeight := totalHeight;
end;



procedure TCustomOpenGLControl.disableTiledScreenShot();
begin
     fTileLeft := 0;
     fTileBottom := 0;
     fTotalWidth := 0;
     fTotalHeight := 0;
end;

function TCustomOpenGLControl.retinaScale(isXnotY: boolean = true): Single;
{$IFDEF LCLCocoa}
const
  kScale = 10000;
var
    pt: NSPoint;
{$ENDIF}
begin
  {$IFDEF LCLCocoa}
  pt.x := kScale;
  pt.y := kScale;
  pt := TCocoaOpenGLView(Handle).convertPointToBacking(pt);
  if (isXnotY) then // doubt we will ever have rectangular scaling, but to be safe...
  	result := pt.x / kScale
  else
  	result := pt.y / kScale;
  {$ELSE}
   result := 1;
  {$ENDIF}
end;

(*function TCustomOpenGLControl.retinaScale: Single;
//This only returns 1 or 2, fails on non-integer scaling or with multiple monitors
// https://developer.apple.com/documentation/appkit/nswindow/1419459-backingscalefactor
//   For almost all common cases, developers should avoid using the value of backingScaleFactor
begin
  {$IFDEF LCLCocoa}
  //result := LBackingScaleFactor(self.Handle);
  result := TCocoaOpenGLView(Handle).backingScaleFactor;
  {$ELSE}
   result := 1;
  {$ENDIF}
end; *)


initialization
 fTotalWidth := 0;
 fTotalHeight := 0;

end.
