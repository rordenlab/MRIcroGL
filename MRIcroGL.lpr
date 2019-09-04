program MRIcroGL;

{$mode objfpc}{$H+}
{$DEFINE UseCThreads}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,  //<- if parallel NIfTI
  cmem, // <- http://wiki.freepascal.org/Parallel_procedures
  {$ENDIF}{$ENDIF}
  //{$IFDEF LCLGtk2}uscale,{$ENDIF}
  //{$ifdef windows},udark {$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, mainunit, TimedDialog, dcm2nii, drawVolume, autoroi,
  nifti_hdr_view, nifti_resize, resize, crop;

{$R *.res}
begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TGLForm1, GLForm1);
  Application.CreateForm(Tdcm2niiForm, dcm2niiForm);
  Application.CreateForm(TTimedDialogForm, TimedDialogForm);
  Application.CreateForm(TAutoROIForm, AutoROIForm);
  Application.CreateForm(THdrForm, HdrForm);
  Application.CreateForm(TResizeForm, ResizeForm);
  Application.CreateForm(TCropForm, CropForm);
  //{$ifdef windows} SetDarkTheme; {$endif}
  //{$IFDEF LCLGtk2}ConstrainTrackBars();{$ENDIF}  //if unpatched, see https://bugs.freepascal.org/view.php?id=35861
  Application.Run;
  //Windows: if you get an error "Can't find object file" you can copy the 'static' folder from
  //  https://github.com/synopse/mORMot
  //Alternatively, disable "FastGZ" in nifti.pas and umat.pas
end.

