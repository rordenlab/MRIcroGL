unit umat;

//{$mode objfpc}{$H+}
{$mode delphi}{$H+}
interface

uses
  ExtCtrls, StdCtrls, Forms, Controls, Classes, SysUtils, nifti_types;
Type
    TUInt8s = array of uint8;
const
  //force of 0 or above explicitly selects index, negative values select name, e.g. 'lesion' of 'DWI'
  kMatNoForce = -1;
  kMatForceT1 = -2;
  kMatForcefMRI = -3;
  kMatForceDWI = -4;
  kMatForceLesion = -5;


procedure MatLoadForce(Modality: integer);
function MatTestHasAnatomical(fnm: string; ForceAnatomical: boolean): boolean;
//function MatLoadForceAnatomical(fnm: string; out hasLesion: boolean): boolean;
function MatLoadNii(fnm: string; out hdr: TNIFTIHdr; var img: TUInt8s): boolean;

implementation
//{$DEFINE TIMER}
{$include opts.inc} //for  DEFINE FASTGZ
uses
  {$IFDEF FASTGZ}
    SynZip,
  {$ELSE}
  gziputils,
  {$ENDIF}
  zstream,
  {$IFDEF TIMER} DateUtils,{$ENDIF}dialogs;

var
  gMatModality : integer = kMatForceT1;//kMatNoForce;



procedure MatLoadForce(Modality: integer);
begin
     gMatModality := modality;
end;

procedure readTag(stream : TMemoryStream; var offset, itemType, itemBytes, itemOffset: uint32);
begin
    Stream.Position := offset;
    Stream.Read(itemType, SizeOf(itemType));
    Stream.Read(itemBytes, SizeOf(itemBytes));
    if (itemType > 65535) then begin //small data element format
        itemBytes := itemType shr 16;
        itemType := itemType and 65535;
        itemOffset := offset + 4;
        offset := offset + 8;
    end else begin
        itemOffset := offset + 8;
        offset := offset + 8 +  (trunc( (itemBytes + 7) /8) * 8 );
    end;
    //printf("itemType %d itemBytes %d nextTag @ %d\n", *itemType, *itemBytes, *offset);
end;//readTag()

function readTagNoSkip(stream : TMemoryStream; var offset,itemType,itemBytes,itemOffset: uint32): uint32;
var
   offsetIn: uint32;
begin
    offsetIn := offset;
    readTag(stream, offset, itemType, itemBytes, itemOffset);
    result := offset;
    offset := offsetIn + 8;
    //printf("itemType %d itemBytes %d nextTag @ %d\n", *itemType, *itemBytes, *offset);
end; //readTagNoSkip()

function stream2String(var stream: TMemoryStream; offset, bytes: uint32): string;
var
  buffer: PAnsiChar;
begin
  stream.Position:=offset;
  GetMem ( buffer, bytes+1 );
  setlength(result, bytes);
  stream.read(PAnsiChar(result)^, bytes);
  result := trim(result);
  FreeMem ( buffer );
end;

function readUI32(stream : TMemoryStream; offset, index: uint32): uint32;
var
  ret: uint32;
begin
  stream.Position := offset + (4 * index);
  stream.Read(ret, SizeOf(ret));
  result := ret; // byte shift 2 as we are these are 32-bit items
end; //readUI32()

const
  miINT8 = 1;
  miUINT8 = 2;
  miINT16 = 3;
  miUINT16 = 4;
  miINT32 = 5;
  miUINT32 = 6;
  miDOUBLE = 9;

function readItem(stream : TMemoryStream; offset, index, dtype: uint32): double;
var
  i8: int8;
  u8: uint8;
  i16: int16;
  u16: uint16;
  i32: int32;
  u32: uint32;
  f64: double;
begin
    if (dtype = miINT8) then begin
        stream.Position:=offset+ index;
        stream.ReadBuffer(i8, SizeOf(i8));
        exit(i8);
    end else if (dtype = miUINT8)  then begin
       stream.Position:=offset+ index;
       stream.ReadBuffer(u8, SizeOf(u8));
       exit(u8);
    end else if (dtype = miINT16)  then begin
       stream.Position:=offset+ 2* index;
       stream.ReadBuffer(i16, SizeOf(i16));
       exit(i16);
    end else if (dtype = miUINT16)  then begin
       stream.Position:=offset+ 2* index;
       stream.ReadBuffer(u16, SizeOf(u16));
       exit(u16);
    end else if (dtype = miINT32) then begin
       stream.Position:=offset+ 4* index;
       stream.ReadBuffer(i32, SizeOf(i32));
       exit(i32);
    end else if (dtype = miUINT32)  then begin
       stream.Position:= offset+ 4* index;
       stream.ReadBuffer(u32, SizeOf(u32));
       exit(u32);
    end else if (dtype = miDOUBLE) then begin
       stream.Position:= offset+ 8* index;
       stream.ReadBuffer(f64, SizeOf(f64));
       exit(f64);
    end;
    result := 0;
end; //readItem()

function MinU32(a,b: uint32): uint32;
begin
     if a < b then exit(a);
     exit(b);

end;

procedure sform2pixdim(var hdr: TNIFTIHdr);
begin //SPM does not save pixdim, so we infer it from the SForm
     hdr.pixdim[1] := sqrt(sqr(hdr.srow_x[0])+sqr(hdr.srow_y[0])+sqr(hdr.srow_z[0]));
     hdr.pixdim[2] := sqrt(sqr(hdr.srow_x[1])+sqr(hdr.srow_y[1])+sqr(hdr.srow_z[1]));
     hdr.pixdim[3] := sqrt(sqr(hdr.srow_x[2])+sqr(hdr.srow_y[2])+sqr(hdr.srow_z[2]));
     //writeln(format('%g %g %g',  [hdr.pixdim[1], hdr.pixdim[2], hdr.pixdim[3] ]));
end;

function MatLoad(fnm, tagname: string; var strs: TStringList; out hdr: TNIFTIHdr; var img: TUInt8s): boolean;
const
  miCOMPRESSED = 15;
  miMATRIX = 14;
  kCache = 8000;
var
  bytes : array of byte;
  zStream: Tdecompressionstream;
  fieldNameLength, itemType, itemBytes, tagStartOffsetBytes,
    itemOffset, tagType, offsetBytes, nSubfields: uint32;
  tag: array [0..1] of uint32;
  tagBytes, tagEnd: uint32;
  sig: array [0..63] of word;
  mat: array [0..15] of double;
  x, i, numfields, datIndex, hdrIndex, indx, nameBytes: integer;
  Stream: TFileStream;
  inStream, outStream : TMemoryStream;
  namStr, datStr, subfieldname: string;
label
  123, 128;
begin
  result := false;
  NII_Clear (hdr);
  if not fileexists(fnm) then exit;
  Stream := TFileStream.Create(fnm, fmOpenRead or fmShareDenyWrite);
  inStream := TMemoryStream.Create();
  outStream := TMemoryStream.Create();
  if (Stream.Size) < 200 then
     goto 123;
  Stream.Read(sig[0], SizeOf(sig));
  if ((sig[0] <> 16717) or (sig[1] <> 19540)) then begin
      {$IFDEF UNIX}writeln('Not a Matlab 5.0 file: ', fnm);{$ENDIF}
      goto 123;
  end;
  if ((sig[62] <> 256) or (sig[63] <> 19785)) then begin
      {$IFDEF UNIX}writeln('Not a little-endian Matlab 5.0 file\n');{$ENDIF}
      goto 123;
  end;
  while Stream.Position < Stream.Size do begin
    Stream.Read(tag[0], SizeOf(tag));
    tagBytes := tag[1];
    tagEnd := Stream.Position + tagBytes;

    if (tag[0] > 65535) then tagBytes := tag[0] shr 16; //small element format
    if (tag[0] = miCOMPRESSED) then begin
        inStream.SetSize(0);
        inStream.CopyFrom(Stream, tagBytes);
        {$IFDEF FASTGZ}
           inStream.Position := 0; // goto start of input stream
           zStream := Tdecompressionstream.create(inStream,false);
           outStream.SetSize(0);
           outStream.Position := 0; // goto start of output stream
           setlength(bytes, kCache);
           //only read first chunk of data to read tag name
	   //repeat
		i := zStream.read(bytes[0],kCache);
		outStream.Write(bytes[0],i) ;
	   //until i < kCache;
           zStream.Free;
        {$ELSE}
        unzipStream(inStream, outStream);
        {$ENDIF}
        //UnCompressStream
        if outStream.Size < 56 then continue;

        //if outStream.Size >= 56 then begin
           offsetBytes := 0;
            readTagNoSkip(outStream, offsetBytes, tagType, itemBytes,  itemOffset); //flag
            readTag(outStream, offsetBytes, itemType, itemBytes, itemOffset); //flag
            readTag(outStream, offsetBytes, itemType, itemBytes, itemOffset); //dimension
            //uint32_t off = offsetBytes;
            readTag(outStream, offsetBytes, itemType, itemBytes, itemOffset); //name
            namStr := stream2String(outStream, itemOffset, itemBytes);
            //showmessage(namStr+' '+inttostr(Stream.position)+' '+inttostr(outStream.SIZE));
            //showmessage(format('%d->%d %d %d', [inStream.Size, outStream.Size, length(namStr), itemBytes]));
            //if CompareText(namStr,'lesion') = 0 then showmessage(namStr);
            readTag(outStream, offsetBytes, itemType, itemBytes, itemOffset); //field name
            //NSLog(@"%dx%d\n", itemType, itemBytes);
            fieldNameLength := readUI32(outStream, itemOffset, 0);
            //NSLog(@"> %dx%d\n", fieldNameLength, itemBytes);
            readTag(outStream, offsetBytes, itemType, itemBytes,  itemOffset); //field names
            if (fieldNameLength < 1) then goto 128;
            numfields := itemBytes div fieldNameLength;
            if (numfields < 1) then goto 128;
            //showmessage(inttostr(numfields));
            hdrIndex := -1;
            datIndex := -1;
            //if CompareText(namStr,'lesion') = 0 then showmessage(inttostr(numfields));
            for i := 0 to numfields-1 do begin
              datStr := stream2String(outStream, itemOffset+(i * fieldNameLength), fieldNameLength);
              //if CompareText(namStr,'lesion') = 0 then showmessage(inttostr(length(datStr))+'*'+datStr+'*');
              if (CompareText(datStr,'hdr') = 0)  then
                 hdrIndex := i;
              if (CompareText(datStr,'dat') = 0)  then
                 datIndex := i;
            end;
            if (hdrIndex >= 0) and (datIndex >= 0) then
               strs.Add(namStr);
            if tagname = '' then continue;
            if CompareText(namStr,tagname) <> 0 then continue;
            {$IFDEF FASTGZ}
              inStream.Position := 0; // goto start of input stream
              {$IFDEF XLIBDEFLATE}
              outStream.SetSize(tagBytes);
              outStream.Position := 0; // goto start of output stream
              UncompressMemX(inStream.Memory, outStream.Memory, tagBytes, dstLen, cmpBytes);
              {$ELSE}
              outStream.SetSize(0);
              outStream.Position := 0; // goto start of output stream
              UnCompressStream(inStream.Memory, tagBytes, outStream, nil, true);
              {$ENDIF}
            {$ENDIF}
            //at this stage we have found our desired header and image
            //read header
            tagStartOffsetBytes := offsetBytes;
            indx := 0;
            while (indx < hdrIndex) do begin
                readTag(outStream, offsetBytes, itemType, itemBytes,  itemOffset); //skipped field's overall tag
                indx := indx + 1;
            end;
            readTagNoSkip(outStream, offsetBytes, itemType, itemBytes, itemOffset); //desired field's overall tag
            readTag(outStream, offsetBytes, itemType, itemBytes, itemOffset); //array flags
            readTag(outStream, offsetBytes, itemType, itemBytes, itemOffset); //dimensions
            readTag(outStream, offsetBytes, itemType, itemBytes, itemOffset); //array name
            readTag(outStream, offsetBytes, itemType, itemBytes, itemOffset); //number of structure subfields
            nSubfields := readUI32(outStream, itemOffset, 0);
            readTag(outStream, offsetBytes, itemType, itemBytes,  itemOffset); //
            nameBytes := itemBytes div nSubfields;
            indx := -1;
            for i := 0 to (nSubfields- 1) do begin
              subfieldname := stream2String(outStream, itemOffset+(i * nameBytes), nameBytes);
              if ( CompareText(subfieldname,'mat') = 0) then
                 indx := i;
           end;
           if indx < 0 then goto 123; //fail

           x := 0;
           while (x < indx) do begin
                 readTag(outStream, offsetBytes, itemType, itemBytes, itemOffset); //skipped field's overall tag
                 x := x + 1;
           end;
           readTagNoSkip(outStream, offsetBytes, itemType, itemBytes, itemOffset);
           if (itemType = miMATRIX) then begin
              readTag(outStream, offsetBytes, itemType, itemBytes,  itemOffset); //desired field's array flags
              readTag(outStream, offsetBytes, itemType, itemBytes,  itemOffset); //desired field's dimension
              readTag(outStream, offsetBytes, itemType, itemBytes,  itemOffset); //desired field's name [EMPTY :UNUSED]
              readTag(outStream, offsetBytes, itemType, itemBytes,  itemOffset); //desired field's name Matrix
              for i := 0 to 15 do begin
                  mat[i] := readItem(outStream, itemOffset, i, itemType);
                  //showmessage(format('%d -> %g',[i, mat[i]]));
              end;
           end;
           hdr.srow_x[0] := mat[0];
           hdr.srow_y[0] := mat[1];
           hdr.srow_z[0] := mat[2];
           //3
           hdr.srow_x[1] := mat[4];
           hdr.srow_y[1] := mat[5];
           hdr.srow_z[1] := mat[6];
           //7
           hdr.srow_x[2] := mat[8];
           hdr.srow_y[2] := mat[9];
           hdr.srow_z[2] := mat[10];
           //11
           hdr.srow_x[3] := mat[12];
           hdr.srow_y[3] := mat[13];
           hdr.srow_z[3] := mat[14];
           hdr.sform_code := kNIFTI_XFORM_SCANNER_ANAT;
           sform2pixdim(hdr);
           (*showmessage(format('%g %g %g %g; %g %g %g %g; %g %g %g %g; 0 0 0 1',
             [lHdr.srow_x[0],lHdr.srow_x[1],lHdr.srow_x[2],lHdr.srow_x[3],
              lHdr.srow_y[0],lHdr.srow_y[1],lHdr.srow_y[2],lHdr.srow_y[3],
              lHdr.srow_z[0],lHdr.srow_z[1],lHdr.srow_z[2],lHdr.srow_z[3] ]));*)
           //done reading header
           //read image
           offsetBytes := tagStartOffsetBytes;
           indx := 0;
           while (indx < datIndex) do begin
               readTag(outStream, offsetBytes, itemType, itemBytes,  itemOffset); //skipped field's overall tag
               indx := indx + 1;
           end;
           readTagNoSkip(outStream, offsetBytes, itemType, itemBytes,  itemOffset); //desired field's overall tag
           if (itemType <> miMATRIX) then goto 123; //failure
           readTag(outStream, offsetBytes, itemType, itemBytes,  itemOffset); //desired field's array flags
           readTag(outStream, offsetBytes, itemType, itemBytes,  itemOffset); //desired field's dimension
           hdr.Dim[1] := readUI32(outStream, itemOffset, 0);
           hdr.Dim[2] := readUI32(outStream, itemOffset, 1);
           hdr.Dim[3] := 1;
           if (itemBytes > 8) then hdr.Dim[3] := readUI32(outStream, itemOffset, 2);
           if (itemBytes > 12) then goto 123; //failure  //can only read up to 3 dimensions
           readTag(outStream, offsetBytes, itemType, itemBytes, itemOffset); //desired field's name [EMPTY :UNUSED]
           readTag(outStream, offsetBytes, itemType, itemBytes, itemOffset); //desired field's matrix data
           //showmessage(inttostr(itemBytes));
           if (itemBytes < 1) then goto 123; //failure  must have data
           //printf("%d : %d\n", itemType, itemBytes );
           if (itemType = miINT8) then
               hdr.dataType := kDT_INT8
           else if (itemType = miUINT8) then
               hdr.dataType := kDT_UINT8
           else if (itemType = miINT16) then
               hdr.dataType := kDT_INT16
           else if (itemType = miUINT16) then
               hdr.dataType := kDT_UINT16
           else if (itemType = miINT32) then
               hdr.dataType := kDT_INT32
           else if (itemType = miUINT32) then
               hdr.dataType := kDT_UINT32
           else if (itemType = miINT32) then
               hdr.dataType := kDT_INT32
           else if (itemType = miDOUBLE) then
               hdr.dataType := kDT_DOUBLE
           else
               goto 123; //failure: unknown type
           if (hdr.datatype = kDT_INT8) or (hdr.datatype = kDT_UINT8) then
               hdr.bitpix := 8
           else if (hdr.datatype = kDT_INT16) or (hdr.datatype = kDT_UINT16) then
               hdr.bitpix := 16
           else if (hdr.datatype = kDT_DOUBLE) then
               hdr.bitpix := 64
           else
               hdr.bitpix := 32;
            setlength(img, itemBytes);
            outStream.Position := itemOffset;
            if (outStream.Size - outStream.Position) < itemBytes then
                goto 123; //not enough space
            outStream.Read(img[0], length(img));
            //showmessage(format('%d@%d %d', [outStream.Position, outStream.Size, itemBytes]));
            goto 128; //success!
    end;
    inStream.SetSize(0);
    Stream.seek(tagEnd, soFromBeginning);
  end;
128:
  result := true;
123:
  inStream.Free;
  outStream.Free;
  Stream.Free;
end;



function matSeriesSelectForm(matStrings: TStringlist): string;
//choose from one of multiple strings
const
  kMaxItems = 16;
var
  PrefForm: TForm;
  rg: TRadioGroup;
  OKBtn, CancelBtn: TButton;
  forceStr: string;
  w,h, idx: integer;
begin
  result := '';
  if matStrings.Count < 1 then exit; //no files
  if matStrings.Count = 1 then begin
    result := matStrings[0];//seriesNum(dcmStrings[0]);
    exit;
  end;
  //showmessage(format('%d %d', [gMatModality, matStrings.Count]));
  if (gMatModality > kMatNoForce) and (gMatModality < matStrings.Count) then begin
     //showmessage(format('--%d %d', [gMatModality, matStrings.Count]));
     result := matStrings[gMatModality];
     exit;
  end;
  forceStr := '';
  if gMatModality = kMatForceT1 then forceStr := 'T1';
  if gMatModality = kMatForcefMRI then forceStr := 'fMRI';
  if gMatModality = kMatForceDWI then forceStr := 'DWI';
  if gMatModality = kMatForceLesion then forceStr := 'lesion';
  if forceStr <> '' then begin
     MatLoadForce(kMatNoForce);
     idx := matStrings.IndexOf(forceStr);
     if idx >= 0 then
        exit(forceStr);
     exit('');
  end;
  PrefForm:=TForm.Create(nil);
  //PrefForm.SetBounds(100, 100, 520, 212);
  //PrefForm.Caption:='DICOM Loading '+dcm2niixExe;
  PrefForm.Caption:='Choose image from MAT file ';
  PrefForm.Position := poScreenCenter;
  PrefForm.BorderStyle := bsDialog;
  PrefForm.BorderWidth := 8;
  PrefForm.AutoSize:=true;
  {$IFNDEF FPC}PrefForm.AutoSize := true;{$ENDIF}
  //radio group
  rg := TRadioGroup.create(PrefForm);
  rg.align := alTop;
  rg.AutoSize:=false;
  rg.parent := PrefForm;
  rg.caption := 'Select DICOM Series';
  if matStrings.Count > (kMaxItems) then begin
     rg.caption := rg.caption + ' (Partial Listing)';
     while (matStrings.Count > kMaxItems) do
           matStrings.Delete(matStrings.Count-1);
  end;
  rg.items := matStrings;
  rg.BorderSpacing.Around := 8;
  rg.AutoSize := true;
  rg.Constraints.MinWidth:= 320;
  rg.HandleNeeded;
  rg.GetPreferredSize(w, h);
  rg.Align := alTop;
  rg.Height := h;
  rg.ItemIndex:=0;
  //Cancel button
  CancelBtn:=TButton.create(PrefForm);
  CancelBtn.Caption:='Cancel';
  //CancelBtn.Left := 28;
  //CancelBtn.Width:= 100;
  //CancelBtn.Top := rg.Height+rg.Top+4;
  CancelBtn.AutoSize := true;
  CancelBtn.Constraints.MinWidth:= 80;
  CancelBtn.AnchorSide[akLeft].Side := asrLeft;
  CancelBtn.AnchorSide[akLeft].Control := PrefForm;
  CancelBtn.BorderSpacing.Left := 6;
  CancelBtn.AnchorSide[akTop].Side := asrBottom;
  CancelBtn.AnchorSide[akTop].Control := rg;
  CancelBtn.BorderSpacing.Top := 6;
  CancelBtn.Parent:=PrefForm;
  CancelBtn.ModalResult:= mrCancel;
  //OK button
  OkBtn:=TButton.create(PrefForm);
  OkBtn.AutoSize := true;
  OkBtn.Caption:='OK';
  OkBtn.Constraints.MinWidth:= 80;
  OkBtn.AnchorSide[akTop].Side := asrTop;
  OkBtn.AnchorSide[akTop].Control := CancelBtn;
  OkBtn.AnchorSide[akLeft].Side := asrRight;
  OkBtn.AnchorSide[akLeft].Control := CancelBtn;
  OkBtn.BorderSpacing.Left := 6;
  //OkBtn.AnchorSide[akRight].Side := asrBottom;
  //OkBtn.AnchorSide[akRight].Control := rg;
  OkBtn.Parent:=PrefForm;
  OkBtn.ModalResult:= mrOK;
  PrefForm.SetFocusedControl(OkBtn); //<- pressing return key opens first image
  PrefForm.Height:= OkBtn.Top + OkBtn.Height+4;
  //{$IFDEF LCLCocoa}GLForm1.SetFormDarkMode(PrefForm); {$ENDIF}
  //86
  //{$IFDEF Windows} ScaleDPI(PrefForm, 96);  {$ENDIF}
  //{$IFDEF Linux} ScaleDPIX(PrefForm, 96); {$ENDIF}
  PrefForm.ShowModal;
  result := rg.Items[rg.ItemIndex];//seriesNum(rg.Items[rg.ItemIndex]);
  if PrefForm.ModalResult = mrCancel then
    result :=  '';
  FreeAndNil(PrefForm);
end; // PrefMenuClick()

function MatLoadNii(fnm: string; out hdr: TNIFTIHdr; var img: TUInt8s): boolean;
var
  strs: TStringList;
  tagname: string;
  {$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
begin
  result := false;
  if not fileexists(fnm) then exit;
  strs := TStringList.Create();
  tagname := '';
  img := nil; //clear
   {$IFDEF TIMER}startTime := now;{$ENDIF}
  if not MatLoad(fnm, '', strs, hdr, img) then begin
     strs.Free;
     exit;
  end;
  {$IFDEF TIMER}Showmessage(inttostr(MilliSecondsBetween(Now,startTime)));{$ENDIF}
  tagname := matSeriesSelectForm(strs);
  strs.Clear;
  img := nil;
  if (tagname = '') or (not MatLoad(fnm, tagname, strs, hdr, img)) then begin
       strs.Free;
       exit;
  end;
  result := true;
  strs.Free;
end;

function MatTestHasAnatomical(fnm: string; ForceAnatomical: boolean): boolean;
label
  123;
var
  strs: TStringList;
  hdr: TNIFTIHdr;
  img: TUInt8s;
  idx: integer;
begin
  result := false;
  if not fileexists(fnm) then exit;
  strs := TStringList.Create();
  img := nil; //clear
  if not MatLoad(fnm, '', strs, hdr, img) then begin
     strs.Free;
     exit;
  end;
  img := nil;
  idx := strs.IndexOf('lesion');
  if idx >= 0 then
     result := true;
  gMatModality := kMatNoForce;
  if (strs.count = 1) then goto 123; //only one modality in this file
  if (strs.count = 2) and (result) then begin //only lesion and one other type: load lesion of remaining image!
     gMatModality := (1 - idx); //two volumes [0,1] lesion is either 0,1: use remaining one as our modality
     goto 123;
  end;
  if not ForceAnatomical then
     goto 123;
  idx := strs.IndexOf('T1');
  if idx >= 0 then begin
     gMatModality := kMatForceT1;
     goto 123;
  end;
  idx := strs.IndexOf('DWI');
  if idx >= 0 then begin
     gMatModality := kMatForceDWI;
     goto 123;
  end;
123:
  strs.Free;
end;

(*function MatLoadForceAnatomical(fnm: string; out hasLesion: boolean): boolean;
label
  123;
var
  strs: TStringList;
  hdr: TNIFTIHdr;
  img: TUInt8s;
  idx: integer;
begin
  result := false;
  hasLesion := false;
  if not fileexists(fnm) then exit;
  strs := TStringList.Create();
  //tagname := '';
  img := nil; //clear
  if not MatLoad(fnm, '', strs, hdr, img) then begin
     strs.Free;
     exit;
  end;
  img := nil;
  idx := strs.IndexOf('lesion');
  if idx >= 0 then
     hasLesion := true;
  gMatModality := kMatNoForce;
  idx := strs.IndexOf('T1');
  if idx >= 0 then begin
     gMatModality := kMatForceT1;
     goto 123;
  end;
  idx := strs.IndexOf('DWI');
  if idx >= 0 then begin
     gMatModality := kMatForceDWI;
     goto 123;
  end;
123:
  strs.Free;
  result := gMatModality <> kMatNoForce;

end;*)



end.


