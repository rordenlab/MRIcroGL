unit intensityFilterForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls;

type

  { TIntensityFilterForm }

  TIntensityFilterForm = class(TForm)
    ActionDrop: TComboBox;
    CancelBtn: TButton;
    Panel1: TPanel;
    RampAbove: TTrackBar;
    RampBelow: TTrackBar;
    OKBtn: TButton;
    BackShape: TShape;
    FrontShape: TShape;
    procedure RampAboveChange(Sender: TObject);
  private
         procedure CreateGraph();
  public

  end;

var
  IntensityFilterForm: TIntensityFilterForm;

implementation

{$R *.lfm}

{ TIntensityFilterForm }
procedure TIntensityFilterForm.CreateGraph();
var
  Lo,Hi, R, Wid: single;
begin
  Lo := min(RampAbove.Position, RampBelow.Position);
  Hi := max(RampAbove.Position, RampBelow.Position);
  Wid := ShapeBack.Width;
  if (RampAbove.Position > RampBelow.Position) then begin
     FrontShape.Color:=clRed;
     BackShape.Color:=clBlack;
  end else begin
      FrontShape.Color:=clBlack;
      BackShape.Color:=clRed;
  end;
  FrontShape.Left := round((Lo/RampAbove.Max) * Wid)+1;
  FrontShape.Width := round(((Hi-Lo)/RampAbove.Max) * Wid)+1;
end;

procedure TIntensityFilterForm.RampAboveChange(Sender: TObject);
begin
  CreateGraph();
end;

end.

