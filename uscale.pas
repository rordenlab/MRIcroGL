unit uscale;
 //http://wiki.lazarus.freepascal.org/High_DPI
{$IFDEF FPC}{$mode delphi}  {$H+}{$ENDIF}
interface

uses
    {$IFDEF LCLGtk2} Gtk2Def, gtk2, Gtk2Proc, {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
    StdCtrls, Buttons, Menus;

procedure ConstrainTrackBars;

implementation

procedure ConstrainTrackBar(Control: TControl);
var
  i: integer;
  WinControl: TWinControl;
  {$IFDEF LCLGtk2}
  Widget: PGtkWidget;
  {$ENDIF}
begin
  if (Control is TTrackBar) then begin
     {$IFDEF Darwin}
     (Control as TTrackBar).Constraints.MaxHeight := 22;
     (Control as TTrackBar).Height := (Control as TTrackBar).Constraints.MaxHeight;
     {$ENDIF}
     {$IFDEF LCLQT5}
      (Control as TTrackBar).Constraints.MaxHeight := 32;
      (Control as TTrackBar).Height := (Control as TTrackBar).Constraints.MaxHeight;
     {$ENDIF}
     {$IFDEF LCLGtk2}
     if ((Control as TTrackBar).TickStyle = tsNone) then begin
        Widget:=GetStyleWidget(lgsHScale);
        gtk_scale_set_draw_value(GTK_SCALE(Widget), false);
        gtk_widget_size_request(Widget,@Widget^.requisition);
        (Control as TTrackBar).Constraints.MaxHeight := Widget^.requisition.height;
        (Control as TTrackBar).Height := (Control as TTrackBar).Constraints.MaxHeight;
     end;
     {$ENDIF}
  end;
  if not (Control is TWinControl) then exit;
  WinControl := TWinControl(Control);
  if WinControl.ControlCount = 0 then
     exit;
  for i := 0 to WinControl.ControlCount - 1 do
      ConstrainTrackBar(WinControl.Controls[i]);
end;

procedure ConstrainTrackBars;
var
  i: integer;
begin
  for i := 0 to Screen.FormCount - 1 do
    ConstrainTrackBar(Screen.Forms[i]);
end;

end.

