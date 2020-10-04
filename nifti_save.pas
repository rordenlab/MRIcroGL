unit nifti_save;

{$mode objfpc}{$H+}
{$DEFINE GZIP}
interface

uses
  Dialogs, SimdUtils, Classes, SysUtils, nifti_types, Math, zstream;

function SaveVolumeFormatBasedOnExt(fnm: string; var hdr: TNIFTIhdr; var img8: TUInt8s): boolean;


implementation

procedure printf (str: AnsiString);
begin
{$IFNDEF WINDOWS}
writeln(str);
{$ELSE}
if IsConsole then writeln(str);
{$ENDIF}
end;

Procedure BlockWrite64(var f:File; img8: TUInt8s;Count:Int64);
const
  kMaxBlockWrite = 1073741824;
var
  nWritten, nBlock, nOK: int64;
begin
 nWritten := 0;
 while (nWritten < Count) do begin
    nBlock := min(kMaxBlockWrite, Count - nWritten);
    Blockwrite(f,img8[nWritten], nBlock, nOK);
    if nOK <> nBlock then
       break;
    nWritten += nBlock;
 end;
end;

function SaveNii(fnm: string; hdr: TNIFTIhdr; img8: TUInt8s): boolean;
//const
//  kMaxBlockWrite = 1073741824;
var
  f: file;
  //nImgBytes: int64;//, nWritten, nBlock, nOK: int64;
  u0 : uint32;
begin
  result := false;
  if fnm = '' then exit;
  (*nImgBytes := (nhdr.bitpix div 8) * hdr.dim[1] * hdr.dim[2] * hdr.dim[3];
  if (hdr.datatype = kDT_RGB) then nImgBytes := nImgBytes * 3;
  if (hdr.dim[4] > 1) then nImgBytes := nImgBytes * nhdr.dim[4];
  if (hdr.dim[5] > 1) then nImgBytes := nImgBytes * nhdr.dim[5];
  if length(img) <> nImgBytes then begin
     showmessage(format('Unexpected data size: have %d expected %d bytes', [length(img), nImgBytes]));
     exit;
  end; *)
  hdr.vox_offset := 352;
  u0 := 0; //348 byte header padded to 352
  AssignFile(f, fnm);
  FileMode := 2;
  Rewrite(f, 1);
  Blockwrite(f,hdr,sizeof(hdr));
  Blockwrite(f,u0,sizeof(u0));
  (*nWritten := 0;
  while (nWritten < nImgBytes) do begin
     nBlock := min(kMaxBlockWrite, nImgBytes - nWritten);
     //mStream.Write(img[nWritten], nBlock);
     Blockwrite(f,img8[nWritten],nBlock, nOK);
     if nOK <> nBlock then
        break;
     nWritten += nBlock;
  end;*)
  BlockWrite64(f, img8, length(img8));
  CloseFile(f);
  //if (nImgBytes <> nWritten) then
  //  showmessage(format('Failed to save all data: wrote %d of %d bytes', [nWritten, nImgBytes]))
  //else
  	    result := true;
end;


function SaveNiiGz(niftiFileName: string; hdr: TNIFTIhdr; img8: TUInt8s): boolean;
const
  bytesPerMb = 1048576;
var
   Stream : TGZFileStream;
   pad: uint32;
begin
 result := false;
 if (length(img8) > (2000 * bytesPerMb)) then
    Showmessage('Warning GZ can have issues with huge files (consider saving uncompressed and compressing after resizing/cropping).');
 hdr.HdrSz:= SizeOf (TNIFTIHdr);
 hdr.vox_offset:= 352;
 Stream:= TGZFileStream.Create(niftiFileName, gzopenwrite);
 Try
    Stream.WriteBuffer(hdr, SizeOf(TNIFTIHdr));
    pad := 0;
    Stream.WriteBuffer(pad, SizeOf(pad));
    Stream.WriteBuffer(img8[0], length(img8));
 Finally
  Stream.Free;
 End;
 result := true;
end;


function saveNrrd(fnm: string; var hdr: TNIFTIhdr; var img8: TUInt8s): boolean; overload;
//http://teem.sourceforge.net/nrrd/format.html
var
	txt : TextFile;
	f: file;
	i, dim: integer;
	ext: string;
	{$IFDEF GZIP}
	zStream : TGZFileStream;
	{$ENDIF}
begin
	result := false;
	ext := upcase(ExtractFileExt(fnm));
	if ext <> '.NRRD' then
		fnm := changefileext(fnm,'.nhdr');
	if fileexists(fnm) then begin
		printf('Already a file named '+fnm);
		exit;
	end;

	dim := 0;
	for i := 1 to 7 do
		if hdr.dim[i] > 1 then
			dim := dim + 1;
    if dim < 1 then exit; //not an image
	FileMode := fmOpenWrite;   //FileMode := fmOpenRead;
	AssignFile (txt, fnm);
	Rewrite(txt);
	Writeln(txt, 'NRRD0005');
	Writeln(txt, '# Complete NRRD file format specification at:');
	Writeln(txt, '# http://teem.sourceforge.net/nrrd/format.html');
    if (hdr.datatype = kDT_RGBA32) or (hdr.datatype = kDT_RGB) then begin
		dim := dim + 1;
		Writeln(txt, 'type: unsigned char');
	end else if (hdr.datatype = kDT_UINT8) then
		Writeln(txt, 'type: unsigned char')
	else if (hdr.datatype = kDT_INT8) then
		Writeln(txt, 'type: signed char')
	else if (hdr.datatype = kDT_UINT16) then
		Writeln(txt, 'type: unsigned short')
	else if (hdr.datatype = kDT_INT16) then
		Writeln(txt, 'type: signed short')
	else if (hdr.datatype = kDT_UINT32) then
		Writeln(txt, 'type: unsigned int')
	else if (hdr.datatype = kDT_INT32) then
		Writeln(txt, 'type: signed int')
	else if (hdr.datatype = kDT_FLOAT32) then
		Writeln(txt, 'type: float')
	else begin
		printf('Unsupported format');
		exit(false);
	end;
	Writeln(txt, 'dimension: '+inttostr(dim));
	if (hdr.datatype = kDT_RGBA32) then
		Write(txt, 'sizes: 4 ')
	else if (hdr.datatype = kDT_RGB) then
		Write(txt, 'sizes: 3 ')
	else
		Write(txt, 'sizes: ');
	for i := 1 to 7 do
		if hdr.dim[i] > 1 then
			Write(txt, inttostr(hdr.dim[i])+' ');
	Writeln(txt,'');
	{$IFDEF ENDIAN_LITTLE}
	Writeln(txt,'endian: little');
	{$ELSE}
	Writeln(txt,'endian: big');
	{$ENDIF}
	if (hdr.dim[1] > 0) and (hdr.dim[2] > 0) and (hdr.dim[3] > 0) then begin
		Writeln(txt,'space: right-anterior-superior');
		Writeln(txt,'space units: "mm" "mm" "mm"');
		if (hdr.datatype = kDT_RGBA32) or (hdr.datatype = kDT_RGB) then
			Write(txt, 'centerings: ??? cell cell cell')
		else
			Write(txt,'centerings: cell cell cell');
		for i := 4 to 7 do
			if hdr.dim[i] > 1 then
				Write(txt, ' ???');
		Writeln(txt,'');
		//save space directions
		if (hdr.datatype = kDT_RGBA32) or (hdr.datatype = kDT_RGB) then
			Write(txt, 'space directions: none ')
		else
			Write(txt, 'space directions: ');
		Write(txt, format('(%g,%g,%g) (%g,%g,%g) (%g,%g,%g)',[
			hdr.srow_x[0], hdr.srow_y[0], hdr.srow_z[0],
			hdr.srow_x[1], hdr.srow_y[1], hdr.srow_z[1],
			hdr.srow_x[2], hdr.srow_y[2], hdr.srow_z[2]
			]));
		for i := 4 to 7 do
			if hdr.dim[i] > 1 then
				Write(txt, ' none');
		Writeln(txt,'');
		//save kinds
		if (hdr.datatype = kDT_RGBA32) or (hdr.datatype = kDT_RGB) then
			Write(txt, 'kinds: vector')
		else
			Write(txt, 'kinds:');
		Write(txt, ' space space space');
		for i := 4 to 7 do
			if hdr.dim[i] > 1 then
				Write(txt, ' list');
		Writeln(txt,'');
		Writeln(txt,format('space origin: (%g,%g,%g)', [hdr.srow_x[3], hdr.srow_y[3],hdr.srow_z[3]]));
	end;
	if ext = '.NRRD' then //attached header
		Writeln(txt, 'encoding: raw')
	else begin	//detached header
		{$IFDEF GZIP}
		Writeln(txt, 'encoding: gzip');
		fnm := changefileext(fnm,'.raw.gz');
		{$ELSE}
		Writeln(txt, 'encoding: raw');
		fnm := changefileext(fnm,'.raw');
		{$ENDIF}
		Writeln(txt, format('data file: %s',[extractfilename(fnm)]));
	end;
	Writeln(txt);
	CloseFile(txt);
	if ext = '.NRRD' then begin //attached header
		AssignFile(f,fnm);
		Reset(f,1);
		Seek(f,filesize(f)); { move poinetr to the end of the file }
		//BlockWrite(f, img8[0], length(img8));
		BlockWrite64(f, img8, length(img8));
		Close(f);
		exit(true);
	end;
	{$IFDEF GZIP}
	zStream := TGZFileStream.Create(fnm, gzopenwrite);
	zStream.Write(img8[0], length(img8));
	zStream.Free;
	{$ELSE}
	AssignFile(f, fnm);
	ReWrite(f, 1);
	//BlockWrite(f, img8[0], length(img8));
    BlockWrite64(f, img8, length(img8));
    FileMode := fmOpenRead;
	{$ENDIF}
	exit(true);
end; //SaveNrrd

function saveTiff(fnm: string; var hdr: TNIFTIhdr; var img8: TUInt8s): boolean; overload;
//http://paulbourke.net/dataformats/tiff/
const
{$IFDEF ENDIAN_LITTLE}
kmagic : uint32 = 2771273;
{$ELSE}
kmagic : uint32 =  1296891946;
{$ENDIF}
dtByte = 1;
dtShort = 3;
dtLong = 4;
dtRational = 5;
var
	f: file;
	samplesPerPixel, nEntries: uint16;
	dim3to7, i, sliceBytes, offset, zero, offsetSamplesPerPixel: uint32;
procedure WriteTag(TagId, DataType: uint16; DataCount, DataOffset: uint32);
begin
	BlockWrite(f, TagId, sizeof(TagId));
	BlockWrite(f, DataType, sizeof(DataType));
	BlockWrite(f, DataCount, sizeof(DataCount));
	BlockWrite(f, DataOffset, sizeof(DataOffset));
end;
begin
	if (hdr.datatype = kDT_RGBA32)  then begin
		printf('Unable to export color images to TIFF (use Slicer)');
		exit(false);
	end;
	dim3to7 := 1;
	for i := 3 to 7 do
	if hdr.dim[i] > 1 then
		dim3to7 := dim3to7 * hdr.dim[i];
	result := true;
	//fnm :=  fnm + '.tiff';
	if fileexists(fnm) then
		printf('Overwriting "'+fnm+'"')
	else
		printf('Converted "'+fnm+'"');
	FileMode := fmOpenWrite;   //FileMode := fmOpenRead;
	AssignFile(f, fnm);
	ReWrite(f, 1);
	BlockWrite(f, kmagic, 4);
	offset := 8 + length(img8);
	offsetSamplesPerPixel := offset;
	if (hdr.datatype = kDT_RGBA32)  or  (hdr.datatype = kDT_RGB) then
    	offset += 3 * sizeof(samplesPerPixel);
    BlockWrite(f, offset, sizeof(offset));
    BlockWrite64(f, img8, length(img8));
	//BlockWrite(f, img8[0], length(img8));
	//BlockWrite(lF,lOutputRA, 1  {, NumWritten});
	sliceBytes := hdr.dim[1] * hdr.dim[2] * (hdr.bitpix div 8);
	zero := 0;
	nEntries := 11;
    if (hdr.datatype = kDT_RGBA32)  or  (hdr.datatype = kDT_RGB) then begin
	   nEntries += 1; //reequires 0x011C PlanarConfiguration
	   samplesPerPixel := 8;
	   BlockWrite(f, samplesPerPixel, sizeof(samplesPerPixel));
	   BlockWrite(f, samplesPerPixel, sizeof(samplesPerPixel));
	   BlockWrite(f, samplesPerPixel, sizeof(samplesPerPixel));
    end;
    for i := 0 to (dim3to7-1) do begin
		BlockWrite(f, nEntries, sizeof(nEntries));
		WriteTag(256, dtLong, 1, hdr.dim[1]);//0x0100 ImageWidth
		WriteTag(257, dtLong, 1, hdr.dim[2]);//0x0101 ImageHeight
		if (hdr.datatype = kDT_RGBA32)  or  (hdr.datatype = kDT_RGB) then begin
			WriteTag(258, dtShort, 3, offsetSamplesPerPixel);//0x0102 BitsPerSample
			WriteTag(262, dtShort, 1, 2);//0x0106 PhotometricInterpretation, GrayScale 0=whiteIsZero, 1=blackIsZero, 2=RGB
			WriteTag(277, dtShort, 1, 3);//0x0115 SamplesPerPixel
			WriteTag(284, dtShort, 1, 1);//0x011C PlanarConfiguration 1=singleplane, chunky RGBRGBRGB...
		end else begin
			WriteTag(258, dtShort, 1, hdr.bitpix);//BitsPerSample
			WriteTag(262, dtShort, 1, 1);//0x0106 PhotometricInterpretation, GrayScale 0=whiteIsZero, 1=blackIsZero, 2=RGB
			WriteTag(277, dtShort, 1, 1);//0x0115 SamplesPerPixel
		end;
		WriteTag(259, dtShort, 1, 1);//0x0103 Compression
		WriteTag(273, dtLong, 1, 8+(i*sliceBytes));//0x0111 StripOffsets
		WriteTag(274, dtLong, 1, 3);//0x0112 Orientation
		WriteTag(278, dtLong, 1, hdr.dim[2]);//0x0116 RowsPerStrip
		WriteTag(279, dtLong, 1, sliceBytes);//0x0117 StripByteCounts
		if (hdr.datatype = kDT_FLOAT32) then
			WriteTag(339, dtShort, 1, 3)//0x0153 SampleFormat UINT = 1; INT = 2; FLOAT = 3;
		else if (hdr.datatype = kDT_INT32) or (hdr.datatype = kDT_INT16) or (hdr.datatype = kDT_INT8)  then
			WriteTag(339, dtShort, 1, 2)//0x0153 SampleFormat UINT = 1; INT = 2; FLOAT = 3;
		else
			WriteTag(339, dtShort, 1, 1);//0x0153 SampleFormat UINT = 1; INT = 2; FLOAT = 3;
		offset += 2 + (nEntries * 12) + 4;
		if i = (dim3to7-1) then
			Blockwrite(f, zero, sizeof(zero))
		else
			BlockWrite(f, offset, sizeof(offset));
	end;
	CloseFile(f);
	FileMode := fmOpenRead;
	result := true;
end; //SaveTiff

function SaveOsp(OspFileName: string; var hdr: TNIFTIhdr; var img8: TUInt8s): boolean;
var
  nByte: int64;
  Stream : TFileStream;
  fnm : string;
  txt : TextFile;
begin
 fnm := changefileext(OspFileName,'.osp');
 if fileexists(fnm) then begin
    showmessage('Already a file named '+fnm);
    exit;
 end;
 AssignFile (txt,fnm);
 fnm := changefileext(OspFileName,'.raw');
 Rewrite(txt);
 Writeln(txt, '<?xml version="1.0"?>');
 Writeln(txt, '<!-- OSPRay format file -->');
 Writeln(txt, '<volume name="volume">');
 Writeln(txt, format('  <dimensions> %d %d %d </dimensions>',[hdr.dim[1], hdr.dim[2], hdr.dim[3]]));
 nByte := hdr.dim[1] * hdr.dim[2] * hdr.dim[3];
 if hdr.datatype = kDT_UINT8 then begin
    Writeln(txt, '  <voxelType> uchar </voxelType>');
 end else if hdr.datatype = kDT_INT16 then begin
   Writeln(txt, '  <voxelType> short </voxelType>');
   nByte := nByte * 2;
 end else if hdr.datatype = kDT_FLOAT then begin
     Writeln(txt, '  <voxelType> float </voxelType>');
     nByte := nByte * 4;
 end;
 Writeln(txt, '  <samplingRate> 1.0 </samplingRate> ');
 Writeln(txt, format('  <filename> %s </filename> ',[extractfilename(fnm)]));
 Writeln(txt, '</volume> ');
 CloseFile(txt);
 Stream:= TFileStream.Create(fnm, fmCreate);
 Try
    Stream.WriteBuffer(img8[0], nByte);
 Finally
  Stream.Free;
 End;
 result := true;
end; //SaveOsp

function SaveBVox(bVoxFileName: string; var hdr: TNIFTIhdr; var img8: TUInt8s): boolean;
var
  nX,nY,nZ,nVol: uint32;
  i, nVox: int64;
  vol16 : TInt16s;
  vol32, v: TFloat32s;
  Stream : TFileStream;
  rawMin, rawMax, rawRange: single;
begin
 nX := hdr.dim[1];
 nY := hdr.dim[2];
 nZ := hdr.dim[3];
 nVol := 1;
 nVox := nX * nY * nZ * nVol;
 setlength(v, nVox);
 if hdr.datatype = kDT_UINT8 then begin
   rawMin := img8[0];
   rawMax := rawMin;
   for i := 0 to (nVox-1) do begin
       rawMin := min(rawMin, img8[i]);
       rawMax := min(rawMin, img8[i]);
   end;
   rawRange := abs(rawMax-rawMin);
   if rawRange = 0 then
      rawRange := 1;
    for i := 0 to (nVox-1) do
        v[i] := (img8[i]-rawMin) / rawRange;
 end else if hdr.datatype = kDT_INT16 then begin
     vol16 := TInt16s(img8);
     rawMin := vol16[0];
     rawMax := rawMin;
     for i := 0 to (nVox-1) do begin
         rawMin := min(rawMin, vol16[i]);
         rawMax := min(rawMin, vol16[i]);
     end;
     rawRange := abs(rawMax-rawMin);
     if rawRange = 0 then
        rawRange := 1;
     for i := 0 to (nVox-1) do
         v[i] := (vol16[i]-rawMin) / rawRange;
 end else if hdr.datatype = kDT_FLOAT then begin
   vol32 := TFloat32s(img8);
   rawMin := vol32[0];
   rawMax := rawMin;
   for i := 0 to (nVox-1) do begin
       rawMin := min(rawMin, vol32[i]);
       rawMax := min(rawMin, vol32[i]);
   end;
   rawRange := abs(rawMax-rawMin);
   if rawRange = 0 then
      rawRange := 1;
   for i := 0 to (nVox-1) do
       v[i] := (vol32[i]-rawMin) / rawRange;
 end;
 Stream:= TFileStream.Create(bVoxFileName, fmCreate);
 Try
    Stream.WriteBuffer(nX, SizeOf(nX));
    Stream.WriteBuffer(nY, SizeOf(nY));
    Stream.WriteBuffer(nZ, SizeOf(nZ));
    Stream.WriteBuffer(nVol, SizeOf(nVol));
    Stream.WriteBuffer(v[0], nVox * sizeof(single));
 Finally
  Stream.Free;
 End;
 v := nil;
 result := true;
end; //SaveBVox

(*function saveNii(fnm: string; var hdr: TNIFTIhdr; var img8: TUInt8s): boolean; overload;
var
  Stream : TFileStream;
  pad: uint32;
begin
 result := false;
 hdr.HdrSz:= SizeOf (TNIFTIHdr);
 hdr.vox_offset:= 352;
 Stream:= TFileStream.Create(fnm, fmCreate);
 Try
    Stream.WriteBuffer(hdr, SizeOf(TNIFTIHdr));
    pad := 0;
    Stream.WriteBuffer(pad, SizeOf(pad));
    Stream.WriteBuffer(img8[0], length(img8));
 Finally
  Stream.Free;
 End;
 result := true;
end; *)

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

function SaveVolumeFormatBasedOnExt(fnm: string; var hdr: TNIFTIhdr; var img8: TUInt8s): boolean;
var
  ext: string;
begin
 if fnm = '' then exit(false);
 ext := uppercase(extractfileext(fnm));
 if (ext = '.BVOX') then begin
    result := SaveBVox(fnm, hdr, img8);
    exit;
 end;
 if (ext = '.OSP') then begin
    result := SaveOsp(fnm, hdr, img8);
    exit;
 end;
 if (ext = '.TIF') or (ext = '.TIFF') then begin
    result := SaveTiff(fnm, hdr, img8);
    exit;
 end;
 if (ext = '.NRRD') or (ext = '.NHDR') then begin
    result := SaveNrrd(fnm, hdr, img8);
    exit;
 end;
 if (ext = '.GZ') or (ext = '.VOI') then begin  //
    if (ext = '.GZ') then
       fnm := ChangeFileExtX(fnm,'.nii.gz'); //img.gz -> img.nii.gz
    result := SaveNiiGz(fnm, hdr, img8);
 	exit;
 end;
 result := SaveNii(fnm, hdr, img8);
end;

end.

