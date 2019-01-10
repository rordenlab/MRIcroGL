program MRIcroGL;

{$mode objfpc}{$H+}
{$DEFINE UseCThreads}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  // cthreads,  //<- if parallel NIfTI
  //cmem, // <- http://wiki.freepascal.org/Parallel_procedures
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
  //Windows: if you get an error "Can't find object file" you can copy the 'static' folder from
  //  https://github.com/synopse/mORMot
  //Alternatively, disable "FastGZ" in nifti.pas and umat.pas
end.

