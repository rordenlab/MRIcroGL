unit nsappkitext;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  CocoaAll, LCLType,Classes, SysUtils, Controls, LCLClasses;

  procedure setThemeMode(Owner: TComponent; isDarkMode: boolean);
  function isDarkModeSupported: boolean;

implementation

function ComponentToNSWindow(Owner: TComponent): NSWindow;
var
  obj : NSObject;
begin
  Result := nil;
  if not Assigned(Owner) or not (Owner is TWinControl) then Exit;

  obj := NSObject(TWinControl(Owner).Handle);
  if not Assigned(obj) then Exit;

  if obj.respondsToSelector(ObjCSelector('window')) then
    Result := objc_msgSend(obj, ObjCSelector('window'));
end;

const
  macOSNSAppearanceNameAqua = 'NSAppearanceNameAqua';
  DefaultAppearance = macOSNSAppearanceNameAqua;
  macOSNSAppearanceNameVibrantDark = 'NSAppearanceNameVibrantDark';
  macOSNSAppearanceNameVibrantLight = 'NSAppearanceNameVibrantLight';
  macOSNSAppearanceNameDarkAqua = 'NSAppearanceNameDarkAqua';


function UpdateAppearance(Owner: TComponent; const AAppearance: String): Boolean;
var
  cls : id;
  ap  : string;
  apr : id;
  win : NSWindow;
begin
  Result := false;

  win := ComponentToNSWindow(Owner);
  if not Assigned(win) then Exit;

  if AAppearance = ''
    then ap := DefaultAppearance
    else ap := AAppearance;

  cls := NSClassFromString( NSSTR('NSAppearance'));
  if not Assigned(cls) then Exit; // not supported in OSX version

  apr := objc_msgSend(cls, ObjCSelector('appearanceNamed:'), NSSTR(@ap[1]));
  if not Assigned(apr) then Exit;

  if win.respondsToSelector(ObjCSelector('setAppearance:')) then
  begin
    objc_msgSend(win, ObjCSelector('setAppearance:'), apr);
    Result := true;
  end;
end;

procedure setThemeMode(Owner: TComponent; isDarkMode: boolean);
begin
  if (isDarkMode) then begin
     if not UpdateAppearance(Owner, macOSNSAppearanceNameDarkAqua) then
       UpdateAppearance(Owner, macOSNSAppearanceNameVibrantDark);
  end else
      UpdateAppearance(Owner, DefaultAppearance);
end;

function isDarkModeSupported: boolean;
begin
    result :=  Assigned(NSClassFromString( NSSTR('NSAppearance')));
end;

end.

