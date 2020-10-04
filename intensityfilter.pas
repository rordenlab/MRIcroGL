unit intensityfilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, math;

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
    TimerROI: TTimer;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure RampChange(Sender: TObject);
    procedure TimerROITimer(Sender: TObject);
  private
         procedure CreateGraph();
         procedure IntensityFilter(isUndo: boolean = true);
  public

  end;

var

IntensityFilterForm: TIntensityFilterForm;

implementation

{$R *.lfm}
uses mainunit;

procedure TIntensityFilterForm.IntensityFilter(isUndo: boolean = true);
begin
 if (isUndo) then
    GLForm1.voiUndo();
 GLForm1.DrawIntensityFilter(RampAbove.position, RampBelow.position, ActionDrop.itemIndex);
end;

{ TIntensityFilterForm }
procedure TIntensityFilterForm.CreateGraph();
var
  Lo,Hi, Wid: single;
begin
  Lo := min(RampAbove.Position, RampBelow.Position);
  Hi := max(RampAbove.Position, RampBelow.Position);
  if (RampAbove.Position < RampBelow.Position) then begin
     FrontShape.Brush.Color:=clRed;
     BackShape.Brush.Color:=clBlack;
  end else begin
      FrontShape.Brush.Color:=clBlack;
      BackShape.Brush.Color:=clRed;
  end;
  Wid := BackShape.Width;
  FrontShape.Left := round((Lo/RampAbove.Max) * Wid)+1;
  FrontShape.Width := round(((Hi-Lo)/RampAbove.Max) * Wid)+1;
end;

procedure TIntensityFilterForm.RampChange(Sender: TObject);
begin
  if not IntensityFilterForm.visible then exit;
  CreateGraph();
  TimerROI.Enabled := true;

end;

procedure TIntensityFilterForm.TimerROITimer(Sender: TObject);
begin
  if not IntensityFilterForm.visible then exit;
  TimerROI.Enabled := false;
  IntensityFilter();
end;

procedure TIntensityFilterForm.FormShow(Sender: TObject);
begin
     CreateGraph();
     IntensityFilter(false);
end;

procedure TIntensityFilterForm.OKBtnClick(Sender: TObject);
var
  Lo, Hi: single;
begin
  Lo := min(RampAbove.Position, RampBelow.Position)/RampAbove.Max;
  Hi := max(RampAbove.Position, RampBelow.Position)/RampAbove.Max;
  Self.ModalResult := mrOK;
  Self.close;
  if ActionDrop.ItemIndex = 2 then begin
  	 GLForm1.voiUndo(true);
     GLForm1.ClipIntensity(Lo, Hi);
  end;
end;

procedure TIntensityFilterForm.CancelBtnClick(Sender: TObject);
begin
  Self.ModalResult := mrCancel;
  GLForm1.voiUndo(true);
  Self.Hide;
end;

end.

