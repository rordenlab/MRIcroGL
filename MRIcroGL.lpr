program MRIcroGL;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uscale, mainunit, dcm2nii, drawVolume, autoroi, nifti_hdr_view;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TGLForm1, GLForm1);
  Application.CreateForm(Tdcm2niiForm, dcm2niiForm);
  Application.CreateForm(TAutoROIForm, AutoROIForm);
  Application.CreateForm(THdrForm, HdrForm);
  ConstrainTrackBars();
  Application.Run;
end.

