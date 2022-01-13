unit uscale;
 //http://wiki.lazarus.freepascal.org/High_DPI
{$IFDEF FPC}{$mode delphi}  {$H+}{$ENDIF}
interface

uses
    {$IFDEF LCLGtk2} strutils, FileUtil, Process, Gtk2Def, gtk2, Gtk2Proc, {$ENDIF}
   Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
    StdCtrls, Buttons, Menus;

procedure ConstrainTrackBars;

implementation

procedure ConstrainTrackBar(Control: TControl; Scale: single);
var
  i: integer;
  WinControl: TWinControl;
  {$IFDEF LCLGtk2}
  Widget: PGtkWidget;
  {$ENDIF}
begin
  if (Control is TTrackBar) then begin
     {$IFDEF LCLgtk2}
     if ((Control as TTrackBar).TickStyle = tsNone) then begin
        Widget:=GetStyleWidget(lgsHScale);
        gtk_scale_set_draw_value(GTK_SCALE(Widget), false);
        gtk_widget_size_request(Widget,@Widget^.requisition);
        (Control as TTrackBar).Constraints.MaxHeight := Widget^.requisition.height;
        (Control as TTrackBar).Height := (Control as TTrackBar).Constraints.MaxHeight;
     end;
     (Control as TTrackBar).Width := round((Control as TTrackBar).Width * scale);
     {$ENDIF}
     {$IFDEF Darwin}
     (Control as TTrackBar).Constraints.MaxHeight := 24;
     (Control as TTrackBar).Height := (Control as TTrackBar).Constraints.MaxHeight;
     {$ENDIF}
     {$IFDEF LCLQT5}
      (Control as TTrackBar).Constraints.MaxHeight := 32;
      (Control as TTrackBar).Height := (Control as TTrackBar).Constraints.MaxHeight;
     {$ENDIF}
     {$IFDEF LCLGtk3}
      (Control as TTrackBar).Constraints.MaxHeight := 32;
      (Control as TTrackBar).Height := (Control as TTrackBar).Constraints.MaxHeight;
     {$ENDIF}
  end;
  if not (Control is TWinControl) then exit;
  WinControl := TWinControl(Control);
  if WinControl.ControlCount = 0 then
     exit;
  for i := 0 to WinControl.ControlCount - 1 do
      ConstrainTrackBar(WinControl.Controls[i], scale);
end;

{$IFDEF LCLGtk2}
function str2XPix(str: string): integer;
// '1920x1080+0+0' -> 1920   1280x778+0+0
var
  s: string;
begin
     result := 0;
     if length(str) < 1 then exit;
     if not (str[1] in ['0'..'9']) then exit;
     if not AnsiContainsText(str, 'x') then exit;
     if not AnsiContainsText(str, '+') then exit;
     s := copy(str, 1, PosEx('x',str)-1);
     result := strtointdef(s,0);
end;

function getDPIXRANDR(): single;
var
  AProcess: TProcess;
  Exe, mmStr: String;
  dpi, mm: single;
  i, k, xPix: integer;
  AStringList, BStringList: TStringList;
begin
  result := 144.0;
  Exe := FindDefaultExecutablePath('xrandr');
  if length(Exe) < 1 then begin
     Exe := '/opt/X11/bin/xrandr';
     //Exe := '/Users/rorden/vx.sh';
     if not fileexists(Exe) then
        Exe := '';
  end;
  writeln('xrandr : '+Exe);
  if length(Exe) < 1 then exit;
  if not FileExists(Exe) then exit;
  //result := 1;
  AProcess := TProcess.Create(nil);
  AProcess.Executable:=Exe;
  AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
  AProcess.Execute;
  if (AProcess.ExitCode = 0) then begin
     AStringList := TStringList.Create;
     BStringList := TStringList.Create;
     AStringList.LoadFromStream(AProcess.Output);
     if AStringList.Count > 0 then begin  //"uint32 2"
        for i := 0 to (AStringList.Count-1) do begin
            if not AnsiContainsText(AStringList.Strings[i], 'connected') then continue;
            writeln(AStringList.Strings[i]);
            BStringList.DelimitedText   := AStringList.Strings[i];
            if (BStringList.Count < 5) then continue;
            k := 0;
            xPix := -1;
            while (k < (BStringList.Count-1)) and (xPix < 1) do begin
                xPix := str2XPix(BStringList.Strings[k]);
                k := k + 1;
            end;
            if xPix < 1 then continue;
            mmStr := BStringList.Strings[BStringList.Count-3];
            if length(mmStr) < 3 then continue;  //"9mm"
            if mmStr[length(mmStr)] <> 'm' then continue;
            if mmStr[length(mmStr)-1] = 'c' then
               mm := 10.0 //cm
            else if mmStr[length(mmStr)-1] = 'm' then
                 mm := 1.0 //mm
            else
                continue;
            delete(mmStr,length(mmStr)-1,2);
            mm := strtointdef(mmStr,0)*mm;
            if mm <= 0 then continue;
            dpi := xPix/( mm/25.4);
            writeln(format(' Xpix %d Xmm %g dpi %g',[xPix, mm, dpi]));
            //Form1.Memo1.lines.Add( inttostr(xPix)+':'+floattostr(mm)+' dpi '+floattostr(dpi));
            if dpi > 0 then
               result := dpi;
               //result := 96/dpi;
            //if (result < 1) then result := 1;
            break;
        end; //for i: each line of output
     end; //if output
     AStringList.Free;
     BStringList.Free;
  end;
  AProcess.Free;
end;
{$ENDIF}

procedure ConstrainTrackBars;
var
  i: integer;
  scale: single;
begin
  scale := 1.0;
  {$IFDEF LCLGtk2}
  scale := getDPIXRANDR() / 144;
  if (scale < 1) then scale := 1;
  writeln('Scale factor: ', scale);
  {$ENDIF}

  for i := 0 to Screen.FormCount - 1 do
    ConstrainTrackBar(Screen.Forms[i], scale);
end;

end.

