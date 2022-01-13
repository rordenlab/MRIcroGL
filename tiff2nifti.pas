unit tiff2nifti;

{$mode objfpc}{$H+}
//{$DEFINE TIMER}
interface

uses
  {$IFDEF TIMER}dateutils, {$ENDIF}nifti_save, Classes, SysUtils, StdCtrls, Controls, Dialogs, Forms, nifti_types, SimdUtils,
  nifti_foreign, nifti_tiff, Math; //, zstream;

  function convertTiffDir(initdir: string): boolean;
  function convertTiff2NIfTI(fnm: string): boolean;

implementation

uses mainunit;

function GetPixDim(prompt: string; var Xmm, Ymm, Zmm: single): boolean;
var
    PrefForm: TForm;
    CancelBtn,OkBtn: TButton;
    promptLabel: TLabel;
    xEdit, yEdit, zEdit: TEdit;
begin
  result := false;
  PrefForm:=TForm.Create(nil);
  //PrefForm.SetBounds(100, 100, 512, 212);
  PrefForm.AutoSize := True;
  PrefForm.BorderWidth := 8;
  PrefForm.Caption:='Value required';
  PrefForm.Position := poScreenCenter;
  PrefForm.BorderStyle := bsDialog;
  //label
  promptLabel:=TLabel.create(PrefForm);
  promptLabel.Caption:= prompt;
  promptLabel.AutoSize := true;
  promptLabel.AnchorSide[akTop].Side := asrTop;
  promptLabel.AnchorSide[akTop].Control := PrefForm;
  promptLabel.BorderSpacing.Top := 6;
  promptLabel.AnchorSide[akLeft].Side := asrLeft;
  promptLabel.AnchorSide[akLeft].Control := PrefForm;
  promptLabel.BorderSpacing.Left := 6;
  promptLabel.Parent:=PrefForm;
  //edit
  xEdit:=TEdit.create(PrefForm);
  xEdit.Caption := FloatToStrF(Xmm, ffGeneral, 8, 4);
  xEdit.Constraints.MinWidth:= 300;
  xEdit.AutoSize := true;
  xEdit.AnchorSide[akTop].Side := asrBottom;
  xEdit.AnchorSide[akTop].Control := promptLabel;
  xEdit.BorderSpacing.Top := 6;
  xEdit.AnchorSide[akLeft].Side := asrLeft;
  xEdit.AnchorSide[akLeft].Control := PrefForm;
  xEdit.BorderSpacing.Left := 6;
  xEdit.Parent:=PrefForm;

  yEdit:=TEdit.create(PrefForm);
  yEdit.Caption := FloatToStrF(Ymm, ffGeneral, 8, 4);
  yEdit.Constraints.MinWidth:= 300;
  yEdit.AutoSize := true;
  yEdit.AnchorSide[akTop].Side := asrBottom;
  yEdit.AnchorSide[akTop].Control := xEdit;
  yEdit.BorderSpacing.Top := 6;
  yEdit.AnchorSide[akLeft].Side := asrLeft;
  yEdit.AnchorSide[akLeft].Control := PrefForm;
  yEdit.BorderSpacing.Left := 6;
  yEdit.Parent:=PrefForm;


  zEdit:=TEdit.create(PrefForm);
  zEdit.Caption := FloatToStrF(Zmm, ffGeneral, 8, 4);
  zEdit.Constraints.MinWidth:= 300;
  zEdit.AutoSize := true;
  zEdit.AnchorSide[akTop].Side := asrBottom;
  zEdit.AnchorSide[akTop].Control := yEdit;
  zEdit.BorderSpacing.Top := 6;
  zEdit.AnchorSide[akLeft].Side := asrLeft;
  zEdit.AnchorSide[akLeft].Control := PrefForm;
  zEdit.BorderSpacing.Left := 6;
  zEdit.Parent:=PrefForm;
  //Cancel Btn
  CancelBtn:=TButton.create(PrefForm);
  CancelBtn.Caption:='Cancel';
  CancelBtn.AutoSize := true;
  CancelBtn.AnchorSide[akTop].Side := asrBottom;
  CancelBtn.AnchorSide[akTop].Control := zEdit;
  CancelBtn.BorderSpacing.Top := 6;
  CancelBtn.AnchorSide[akLeft].Side := asrLeft;
  CancelBtn.AnchorSide[akLeft].Control := PrefForm;
  CancelBtn.BorderSpacing.Left := 200;
  CancelBtn.Parent:=PrefForm;
  CancelBtn.ModalResult:= mrCancel;
  //OK button
  OkBtn:=TButton.create(PrefForm);
  OkBtn.Caption:='OK';
  OkBtn.AutoSize := true;
  OkBtn.AnchorSide[akTop].Side := asrBottom;
  OkBtn.AnchorSide[akTop].Control := zEdit;
  OkBtn.BorderSpacing.Top := 6;
  OkBtn.AnchorSide[akLeft].Side := asrRight;
  OkBtn.AnchorSide[akLeft].Control := CancelBtn;
  OkBtn.BorderSpacing.Left := 6;
  OkBtn.Parent:=PrefForm;
  OkBtn.ModalResult:= mrOK;
  //OK button
  PrefForm.ShowModal;
  if (PrefForm.ModalResult = mrOK) then begin
    result := true;
    Xmm := StrToFloatDef(xEdit.Caption, Xmm);
    Ymm := StrToFloatDef(yEdit.Caption, Ymm);
    Zmm := StrToFloatDef(zEdit.Caption, Zmm);
  end;
  FreeAndNil(PrefForm);
end; //()


(*function SaveDialogNIfTI(initdir: string): string;
var
   dlg: TSaveDialog;
begin
  result := '';
  dlg := TSaveDialog.Create(nil);
  dlg.Title := 'Save NIfTI volume';
  dlg.InitialDir := initdir;
  dlg.Filter := 'NIfTI|*.nii|Compressed NIfTI|*.nii.gz';
  dlg.DefaultExt := '*.nii';
  dlg.FilterIndex := 0;
  if dlg.Execute then
     result := dlg.FileName;
  dlg.Free;
end; *)

(*function Save2NiiGz(niftiFileName: string; fHdr: TNIFTIhdr; img: TUInt8s): string;
const
  bytesPerMb = 1048576;
var
   Stream : TGZFileStream;
   pad: uint32;
   hdr : TNIFTIhdr;
begin
 result := '';
 if fileexists(niftiFileName) then begin
    Showmessage('Unable to overwrite existing file "'+niftiFileName+'".');
    exit;
 end;
 if (length(img) > (2 * bytesPerMb)) then
    Showmessage('WarningL GZ can have issues with huge files (consider saving uncompressed and compressing after resizing/cropping).');
 hdr := fHdr;
 hdr.HdrSz:= SizeOf (TNIFTIHdr);
 hdr.vox_offset:= 352;
 Stream:= TGZFileStream.Create(niftiFileName, gzopenwrite);
 Try
    Stream.WriteBuffer(hdr, SizeOf(TNIFTIHdr));
    pad := 0;
    Stream.WriteBuffer(pad, SizeOf(pad));
    Stream.WriteBuffer(img[0], length(img));
 Finally
  Stream.Free;
 End;
 result := niftiFileName;
end;

function ChangeFileExtX(fnm, ext: string): string; //treat "brik.gz" and ".nii.gz" as single extension
var
   lExt: string;
begin
     lExt := uppercase(extractfileext(fnm));
     result := changefileext(fnm,'');
     if (lExt = '.GZ') or (lExt = '.BZ2') then
        result := changefileext(result,'');
     result := result + ext;
end;

function Save2Nii(fnm: string; nhdr: TNIFTIhdr; img: TUInt8s): string;
const
  kMaxBlockWrite = 1073741824;
var
  f: file;
  outFnm, lExt: string;
  nImgBytes, nWritten, nBlock, nOK: int64;
  u0 : uint32;
begin
  result := '';
  if fnm = '' then exit;
  lExt := uppercase(extractfileext(fnm));
  if (lExt = '.GZ')  then begin
     fnm := changefileextX(fnm,'.nii.gz');
     result := Save2NiiGz(fnm, nhdr, img);
     exit;
  end;
  outFnm := changefileext(fnm,'.nii');
  if fileexists(outFnm) then begin
     showmessage('File already exists '+outFnm);
     exit;
  end;
  nImgBytes := (nhdr.bitpix div 8) * nhdr.dim[1] * nhdr.dim[2] * nhdr.dim[3];
  if (nhdr.datatype = kDT_RGB) then nImgBytes := nImgBytes * 3;
  if (nhdr.dim[4] > 1) then nImgBytes := nImgBytes * nhdr.dim[4];
  if (nhdr.dim[5] > 1) then nImgBytes := nImgBytes * nhdr.dim[5];
  if length(img) <> nImgBytes then begin
     showmessage(format('Unexpected data size: have %d expected %d bytes', [length(img), nImgBytes]));
     exit;
  end;
  nhdr.vox_offset := 352;
  u0 := 0; //348 byte header padded to 352
  AssignFile(f, outFnm);
  FileMode := 2;
  Rewrite(f, 1);
  Blockwrite(f,nhdr,sizeof(nhdr));
  Blockwrite(f,u0,sizeof(u0));
  nWritten := 0;
  while (nWritten < nImgBytes) do begin
     nBlock := min(kMaxBlockWrite, nImgBytes - nWritten);
     //mStream.Write(img[nWritten], nBlock);
     Blockwrite(f,img[nWritten],nBlock, nOK);
     if nOK <> nBlock then
        break;
     nWritten += nBlock;
  end;
  CloseFile(f);
  result := outFnm;
  if (nImgBytes <> nWritten) then begin
    showmessage(format('Failed to save all data: wrote %d of %d bytes', [nWritten, nImgBytes]));
    result := '';
  end;
end; *)

function convertTiffDir(initdir: string): boolean;
var
  lSearchRec: TSearchRec;
  lS : TStringList;
  fnm, tiffdir, outnm: string;
  nOK, i, j: int64;
  sliceBytes : int64;
  hdr0, hdr: TNIFTIHdr;
  img0, img: TUInt8s;
  errStr: string = '';
  {$IFDEF TIMER}startTime : TDateTime;{$ENDIF}
label
     666;
begin
     result := false;
     tiffdir := '';
     if not SelectDirectory('Select a folder containing 2D TIFF images', initdir, tiffdir) then exit;
     if not DirectoryExists(tiffdir) then exit;
     if (tiffdir[length(tiffdir)] <> '\') and (tiffdir[length(tiffdir)] <> '/' ) then
        tiffdir := tiffdir + pathdelim;
     //tiffdir := '/Users/chris/Meriones_unguiculatus/8bitTIFF/';
     lS := TStringList.Create;
     if FindFirst(tiffdir+'*', faAnyFile, lSearchRec) = 0 then
        repeat
              fnm := lSearchRec.Name;
              if (length(fnm) < 1) or (fnm[1] = '.') then
                 continue;
              fnm := tiffdir + fnm;
              if not isTIFF(fnm) then
                 continue;
              lS.Add(fnm) ;
        until (FindNext(lSearchRec) <> 0);
     FindClose(lSearchRec);
     if (lS.Count < 1) then begin
        Showmessage('No TIFF files found in '+tiffdir);
        Freeandnil(lS);
        exit;
     end;
     lS.sort;
     img0 := nil;
     img := nil;
     {$IFDEF TIMER}startTime := Now;{$ENDIF}
     errStr := 'Unable to load '+lS[0];
     if not LoadTIFFAsNifti(lS[0], img0, hdr0) then
        goto 666;
     hdr0.dim[3] := max(hdr0.dim[3], 1);
     sliceBytes := hdr0.dim[1] * hdr0.dim[2] * (hdr0.bitpix div 8);
     nOK := 1;
     if (lS.Count > 1) and (hdr0.dim[4] < 2) then begin
        if hdr0.dim[3] > 1 then begin
           {$IFDEF UNIX}writeln('All TIFFs should be 2D for stacking');{$ENDIF}
           errStr := 'All TIFFs should be 2D for stacking. '+inttostr(hdr0.dim[3])+' slices in '+lS[0];
           goto 666;
        end;
        setlength(img0, sliceBytes * lS.Count);
        for i := 1 to (lS.Count - 1) do begin
            if not LoadTIFFAsNifti(lS[i], img, hdr) then continue;
            if hdr.dim[1] <> hdr0.dim[1] then continue;
            if hdr.dim[2] <> hdr0.dim[2] then continue;
            if hdr.dim[3] > 1 then begin
               {$IFDEF UNIX}writeln('All TIFFs should be 2D for stacking');{$ENDIF}
               errStr := 'All TIFFs should be 2D for stacking. '+inttostr(hdr0.dim[3])+' slices in '+lS[i];
               goto 666;
            end;
            hdr.dim[3] := max(hdr.dim[3], 1);
            if hdr.datatype <> hdr0.datatype then continue;
            if hdr.dim[4] > 1 then continue;
            j := sliceBytes * hdr0.dim[3];
            System.Move(img[0], img0[j],  sliceBytes);
            //for k := 0 to ((sliceBytes * hdr.dim[3]) -1) do
            //    img0[j+k] := img[k];
            hdr0.dim[3] := hdr0.dim[3] + hdr.dim[3];
            nOK := nOK + 1;
        end;
        setlength(img0, sliceBytes * hdr0.dim[3]);
        img := nil;
        {$IFDEF UNIX}if (nOK <> lS.Count) then writeln('TIFF load '+  inttostr(nOK)+' of '+inttostr(lS.Count)+' files.');{$ENDIF}
     end;
     {$IFDEF TIMER}{$IFDEF UNIX} writeln('TIFF load '+  inttostr(MilliSecondsBetween(Now,StartTime)));{$ENDIF} {$ENDIF}
     errStr := 'Spatial resolution not provided.';
     if not GetPixDim(format('Confirm spatial resolution (in mm) for width, height and slices (voxels %dx%dx%d).',[hdr0.dim[1],hdr0.dim[2],hdr0.dim[3]]), hdr0.pixdim[1], hdr0.pixdim[2], hdr0.pixdim[3]) then
            goto 666;
     outnm := GLForm1.NiftiSaveDialogFilename();
     errStr := 'Output name not provided.';
     //outnm := SaveDialogNIfTI(tiffdir);
     if outnm = '' then
        goto 666;
     result := SaveVolumeFormatBasedOnExt(outnm, hdr0, img0);
     //if Save2Nii(outnm, hdr0, img0) <> '' then
     //   result := true;
     666: //report error and clean up
     Freeandnil(lS);
     img0 := nil;
     img := nil;
     if not result then
        showmessage('Fatal error: unable to convert TIFF files in '+tiffdir+'  '+errStr);
end;

function convertTiff2NIfTI(fnm: string): boolean;
var
  hdr: TNIFTIHdr;
  img: TUInt8s;
  outnm: string;
label
     666;
begin
     result := false;
     if not isTIFF(fnm) then
        exit;
     img := nil;
     if not LoadTIFFAsNifti(fnm, img, hdr) then
        exit;
     if not GetPixDim(format('Confirm spatial resolution (in mm) for width, height and slices (voxels %dx%dx%d).',[hdr.dim[1],hdr.dim[2],hdr.dim[3]]), hdr.pixdim[1], hdr.pixdim[2], hdr.pixdim[3]) then
        goto 666;
     //outnm := SaveDialogNIfTI(changefileext(fnm,''));
     outnm := GLForm1.NiftiSaveDialogFilename();
     if outnm = '' then
        goto 666;
     result := SaveVolumeFormatBasedOnExt(outnm, hdr, img);
     //if Save2Nii(outnm, hdr, img) <> '' then
     //   result := true;
     666: //report error and clean up
     img := nil;
     if not result then
        showmessage('Fatal error: unable to convert TIFF files in '+fnm);
end;

end.

