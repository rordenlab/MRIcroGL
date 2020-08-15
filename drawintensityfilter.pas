unit drawIntensityFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls;

type

  { TDrawIntensityFilterForm }

  TDrawIntensityFilterForm = class(TForm)
    ActionDrop: TComboBox;
    CancelBtn: TButton;
    OKBtn: TButton;
    IntensityTrack: TTrackBar;
    TimerROI: TTimer;
    procedure CancelBtnClick(Sender: TObject);
    procedure FilterChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure TimerROITimer(Sender: TObject);
  private
    procedure IntensityFilter(isUndo: boolean = true);

  public

  end;

var
  DrawIntensityFilterForm: TDrawIntensityFilterForm;

implementation

{$R *.lfm}
uses mainunit;

procedure TDrawIntensityFilterForm.IntensityFilter(isUndo: boolean = true);
begin
 if (isUndo) then
    GLForm1.voiUndo();
 GLForm1.DrawIntensityFilter(IntensityTrack.position, ActionDrop.itemIndex+3);
end;

{ TDrawIntensityFilterForm }

procedure TDrawIntensityFilterForm.OKBtnClick(Sender: TObject);
begin
  Self.ModalResult := mrOK;
  Self.close;
  //Self.Hide;
end;

procedure TDrawIntensityFilterForm.TimerROITimer(Sender: TObject);
begin
 if not DrawIntensityFilterForm.visible then exit;
 TimerROI.Enabled := false;
 IntensityFilter();
end;

procedure TDrawIntensityFilterForm.CancelBtnClick(Sender: TObject);
begin
  Self.ModalResult := mrCancel;
  GLForm1.voiUndo(true);
  Self.Hide;
end;

procedure TDrawIntensityFilterForm.FilterChange(Sender: TObject);
begin
 if not DrawIntensityFilterForm.visible then exit;
 TimerROI.Enabled := true;
end;

procedure TDrawIntensityFilterForm.FormShow(Sender: TObject);
begin
    IntensityFilter(false);
end;


end.

