unit autoroi;

interface
//{$include opts.inc}
uses
 {$IFNDEF FPC}

  Spin,
 {$ELSE}
 Spin,lResources,
 {$ENDIF}
 {$IFNDEF FPC} Windows,{$ENDIF}
 SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls,  ExtCtrls, VectorMath;    //define_types,

type
  { TAutoROIForm }
  TAutoROIForm = class(TForm)
    AutoROIBtn: TButton;
    CancelBtn: TButton;
    OriginBtn: TButton;
    ROIconstraint: TComboBox;
    //RadiusEdit: TSpinEdit;
    //VarianceEdit: TSpinEdit;
    DiffLabel: TLabel;
    Label2: TLabel;
    TimerROI: TTimer;
    VarianceEdit: TSpinEdit;
    RadiusEdit: TSpinEdit;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
 procedure OriginBtnClick(Sender: TObject);
	procedure MorphologyFill(isUndo: boolean = true);
	procedure FormShow(Sender: TObject);
	procedure FormCreate(Sender: TObject);
	procedure FormHide(Sender: TObject);
	procedure AutoROIBtnClick(Sender: TObject);
	procedure CancelBtnClick(Sender: TObject);
	procedure AutoROIchange(Sender: TObject);
	procedure TimerROITimer(Sender: TObject);
	procedure FormDestroy(Sender: TObject);
  private
	{ Private declarations }
  public
	{ Public declarations }
  end;

var
  AutoROIForm: TAutoROIForm;
  gOrigin: TVec3;

implementation

{$R *.lfm}

uses mainunit;



procedure TAutoROIForm.OriginBtnClick(Sender: TObject);
begin
  gOrigin := GLForm1.SliceFrac;
 MorphologyFill();
end;

procedure TAutoROIForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
   //
end;

procedure TAutoROIForm.MorphologyFill(isUndo: boolean = true);
begin
 if (isUndo) then
    GLForm1.voiUndo();
 GLForm1.MorphologyFill(gOrigin, VarianceEdit.value, RadiusEdit.value, ROIconstraint.itemIndex);
end;

procedure TAutoROIForm.FormShow(Sender: TObject);
begin
  gOrigin := GLForm1.SliceFrac;
  MorphologyFill(false);
end;

procedure TAutoROIForm.FormCreate(Sender: TObject);
begin
  //
end;

procedure TAutoROIForm.FormHide(Sender: TObject);
begin
     //GLForm1.NoDraw1.Click;
end;

procedure TAutoROIForm.AutoROIBtnClick(Sender: TObject);
begin
	AutoROIForm.ModalResult := mrOK;
	AutoROIForm.close;
end;

procedure TAutoROIForm.CancelBtnClick(Sender: TObject);
begin
  AutoROIForm.ModalResult := mrCancel;
  GLForm1.voiUndo(true);
  //GLForm1.ViewGPU1.Invalidate;
  AutoROIForm.close;
end;

procedure TAutoROIForm.AutoROIchange(Sender: TObject);
begin
     if not AutoROIForm.visible then exit;
     TimerROI.Enabled := true;
end;

procedure TAutoROIForm.TimerROITimer(Sender: TObject);
begin
  TimerROI.Enabled := false;
  MorphologyFill();
end;

procedure TAutoROIForm.FormDestroy(Sender: TObject);
begin
  //if gImageBackupSz <> 0 then Freemem(gImageBackupBuffer);
  //gImageBackupSz := 0;
end;

end.
