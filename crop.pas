unit crop;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, Math, SimdUtils;//, VectorMath, Prefs;

type

  { TCropForm }

  TCropForm = class(TForm)
    NoteLabel: TLabel;
    OKBtn: TButton;
    FilenameLabel: TLabel;
    CroppedShapeLabel: TLabel;
    CancelBtn: TButton;
    xMaxLabel: TLabel;
    zMaxVLabel: TLabel;
    tMaxVLabel: TLabel;
    zMinVLabel: TLabel;
    tMinVLabel: TLabel;
    yMaxVLabel: TLabel;
    xMinVLabel: TLabel;
    xMaxVLabel: TLabel;
    yMinVLabel: TLabel;
    yMaxLabel: TLabel;
    zMaxLabel: TLabel;
    yMaxTrack: TTrackBar;
    tMaxLabel: TLabel;
    zMaxTrack: TTrackBar;
    yMinLabel: TLabel;
    xMinTrack: TTrackBar;
    xMinLabel: TLabel;
    xMaxTrack: TTrackBar;
    tMaxTrack: TTrackBar;
    zMinLabel: TLabel;
    yMinTrack: TTrackBar;
    tMinLabel: TLabel;
    zMinTrack: TTrackBar;
    tMinTrack: TTrackBar;
    procedure BtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure MinTrackChange(Sender: TObject);
    procedure MaxTrackChange(Sender: TObject);
  private

  public
     //function GetCrop(const Dim: TVec4i; filename: string; out cropVols: TPoint): TVec6i;
     procedure GetCrop(const Dim: TVec4i; filename: string);
  end;

var
  CropForm: TCropForm;

implementation

{$R *.lfm}
uses mainunit;

procedure TCropForm.MinTrackChange(Sender: TObject);
var
  v: TVec3i;
  msk: TVec6;
  vols: integer;
  inVx, outVx: int64;
begin
     if (xMinTrack.position >= xMaxTrack.position) then
        xMaxTrack.position := xMinTrack.position + 1;
     if (yMinTrack.position >= yMaxTrack.position) then
        yMaxTrack.position := yMinTrack.position + 1;
     if (zMinTrack.position >= zMaxTrack.position) then
        zMaxTrack.position := zMinTrack.position + 1;
     if (tMinTrack.position >= tMaxTrack.position) then
        tMaxTrack.position := tMinTrack.position + 1;
     v.x := xMaxTrack.position - xMinTrack.position;
     v.y := yMaxTrack.position - yMinTrack.position;
     v.z := zMaxTrack.position - zMinTrack.position;
     vols := tMaxTrack.position - tMinTrack.position; ;
     xMinVLabel.caption := inttostr(xMinTrack.position);
     xMaxVLabel.caption := inttostr(xMaxTrack.position);
     yMinVLabel.caption := inttostr(yMinTrack.position);
     yMaxVLabel.caption := inttostr(yMaxTrack.position);
     zMinVLabel.caption := inttostr(zMinTrack.position);
     zMaxVLabel.caption := inttostr(zMaxTrack.position);
     tMinVLabel.caption := inttostr(tMinTrack.position);
     tMaxVLabel.caption := inttostr(tMaxTrack.position);
     if min(vols, min(v.x, min(v.y, v.z))) < 1 then begin
        CroppedShapeLabel.Caption := 'Invalid crop';
        exit;
     end;
     inVx := xMinTrack.max * yMinTrack.max * zMinTrack.max * tMinTrack.max;
     if inVx < 1 then inVx := 1;
     outVx := v.x * v.y * v.z * vols;
     CroppedShapeLabel.Caption := format('Cropped shape: %dx%dx%d %d volumes (x%.3g)', [v.x, v.y, v.z, vols,outVx/inVx]);
     msk.xLo := xMinTrack.position/xMinTrack.max;
     msk.xHi := xMaxTrack.position/xMaxTrack.max;
     msk.yLo := yMinTrack.position/yMinTrack.max;
     msk.yHi := yMaxTrack.position/yMaxTrack.max;
     msk.zLo := zMinTrack.position/zMinTrack.max;
     msk.zHi := zMaxTrack.position/zMaxTrack.max;
     GLForm1.UpdateCropMask(msk);
end;

procedure TCropForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
   msk: TVec6;
   crop: TVec6i;
   cropVols: TPoint;
begin
     msk := Vec6(-1, 0, 0, 0, 0, 0); //hide
     GLForm1.UpdateCropMask(msk);
     if Self.ModalResult <> mrOK then exit;
     crop.xLo := xMinTrack.position;
     crop.xHi := xMaxTrack.position;
     crop.yLo := yMinTrack.position;
     crop.yHi := yMaxTrack.position;
     crop.zLo := zMinTrack.position;
     crop.zHi := zMaxTrack.position;
     if (crop.xHi <= crop.xLo) or (crop.yHi <= crop.yLo) or (crop.zHi <= crop.zLo) then
     crop.xLo := -1;
     cropVols.x := tMinTrack.position;
     cropVols.y := tMaxTrack.position;
     if cropVols.y <= cropVols.x then
        crop.xLo := -1; //invalid
     //if Self.ModalResult <> mrOK then
     //   crop.xLo := -1; //invalid
     GLForm1.ApplyCrop(crop, cropVols);
end;

procedure TCropForm.BtnClick(Sender: TObject);
begin
  self.Close;
end;

procedure TCropForm.MaxTrackChange(Sender: TObject);
begin
     if (xMinTrack.position >= xMaxTrack.position) then
        xMinTrack.position := xMaxTrack.position - 1;
     if (yMinTrack.position >= yMaxTrack.position) then
        yMinTrack.position := yMaxTrack.position - 1;
     if (zMinTrack.position >= zMaxTrack.position) then
        zMinTrack.position := zMaxTrack.position - 1;
     if (tMinTrack.position >= tMaxTrack.position) then
        tMinTrack.position := tMaxTrack.position - 1;
     MinTrackChange(sender);
end;

//function TCropForm.GetCrop(const Dim: TVec4i; filename: string; out cropVols: TPoint): TVec6i;
procedure TCropForm.GetCrop(const Dim: TVec4i; filename: string);
begin
     Caption := 'Crop '+filename;
     FilenameLabel.Caption := format('Original shape: %dx%dx%d %d volumes', [Dim.x, Dim.y, Dim.z, Dim.t]);
     xMinTrack.max := Dim.x;
     xMinTrack.position := 0;
     xMaxTrack.max := Dim.x;
     xMaxTrack.position := Dim.x;
     yMinTrack.max := Dim.y;
     yMinTrack.position := 0;
     yMaxTrack.max := Dim.y;
     yMaxTrack.position := Dim.y;
     zMinTrack.max := Dim.z;
     zMinTrack.position := 0;
     zMaxTrack.max := Dim.z;
     zMaxTrack.position := Dim.z;
     tMinTrack.max := Dim.t;
     tMinTrack.position := 0;
     tMinTrack.enabled := (Dim.t  > 1);
     tMaxTrack.max := Dim.t;
     tMaxTrack.position := Dim.t;
     tMaxTrack.enabled := (Dim.t  > 1);

     Self.MinTrackChange(nil);
     {$IFDEF LCLCocoa}
     //GLForm1.SetFormDarkMode(Self);
     {$ENDIF}
     Self.show;
end;

end.

