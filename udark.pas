unit udark;
//supports dark mode on Windows
// based on https://github.com/tomboy-notes/tomboy-ng
{$mode objfpc}{$H+}

interface

uses
{$IFDEF LCLGtk2} Gtk2Def, gtk2, Gtk2Proc, {$ENDIF}
Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
StdCtrls, Buttons, Menus, CheckLst, registry;

procedure SetDarkTheme;

implementation

const
     kBackGndColour = clBlack;
     kHiColor = clDkGray;
     kTextColour = clLtGray;

function isWinDarkTheme : boolean;
var
  RegValue : string='';
  Registry : TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize') then
        exit(Registry.ReadInteger('AppsUseLightTheme') = 0)
    else exit(false);
  finally
    Registry.Free;
  end;
end;

procedure SetDark(Control: TControl);
var
  i: integer;
  WinControl: TWinControl;
  {$IFDEF LCLGtk2}
  Widget: PGtkWidget;
  {$ENDIF}
begin
  //if not WinDarkTheme() then exit;
  exit;
  if (Control is TTrackBar) then begin
     (Control as TTrackBar).Color := kBackGndColour;
  end;
  if (Control is TMemo) then begin
     (Control as TMemo).Color := kBackGndColour;
     (Control as TMemo).Font.Color := kTextColour;
  end;
  if (Control is TComboBox) then begin
     (Control as TComboBox).Color := kBackGndColour;
     (Control as TComboBox).Font.Color := kTextColour;
  end;
  if (Control is TButton) then begin
     (Control as TButton).Color := kBackGndColour;
  end;
  if (Control is TCheckListBox) then begin
     (Control as TCheckListBox).Color := kBackGndColour;
     (Control as TCheckListBox).Font.Color := kTextColour;
  end;
  if (Control is TEdit) then begin
     (Control as TEdit).Color := kHiColor;
  end;
  if not (Control is TWinControl) then exit;
  WinControl := TWinControl(Control);
  if WinControl.ControlCount = 0 then
     exit;
  for i := 0 to WinControl.ControlCount - 1 do
      SetDark(WinControl.Controls[i]);
end;

procedure SetDarkTheme;
var
  i: integer;
begin
  if not isWinDarkTheme then exit;
  for i := 0 to Screen.FormCount - 1 do begin
      Screen.Forms[i].color := kHiColor;
      Screen.Forms[i].font.color := kTextColour;
      SetDark(Screen.Forms[i]);
  end;
   (*        color := Sett.hiColor;
         font.color := Sett.TextColour;
         ButtonNoteBookOptions.Color := Sett.HiColor;
         ButtonClearFilters.Color := Sett.HiColor;
         SpeedButton1.color := Sett.HiColor;
         StringGrid1.Color := Sett.BackGndColour;
         StringGrid1.Font.color := Sett.TextColour;
         stringGrid1.GridLineColor:= clnavy; //Sett.HiColor;
         stringgridnotebooks.GridLineColor:= clnavy;
         StringGrid1.FixedColor := Sett.HiColor;
         StringGridNotebooks.FixedColor := Sett.HiColor;
         ButtonRefresh.Color := Sett.HiColor;
         splitter1.Color:= clnavy;*)
end;

end.

