unit nifti_tiff;
{$ifdef fpc}{$mode delphi}{$endif}

interface

uses
   nifti_types, dialogs, nifti_foreign, sysutils, classes, clipbrd;
   function SaveTIFFAsNifti(fnm: string): string;

implementation

Type
  ByteRA0 = array [0..0] of byte;
  Bytep0 = ^ByteRA0;
  WordRA0 = array [0..0] of Word;
  Wordp0 = ^WordRA0;

{$ifndef fpc}
Type
  PtrInt = integer;
{$endif}


{$DEFINE GRAPHICEX_LZW}
{$IFDEF GRAPHICEX_LZW}
{$include lzw.inc}
procedure DecodeLZW(var Buffer: Pointer; var Count, CountDecompressed: PtrInt);
var
  NewBuffer: Pointer;
begin
  GetMem(NewBuffer, CountDecompressed);
  LZWDecoder(Buffer, NewBuffer, Count, CountDecompressed);
  FreeMem(Buffer);
  Buffer:=NewBuffer;
  Count := CountDecompressed;
end;
{$ELSE}
//the code calls DecompressLZW from the FPReadTiff
// it would require accepting the GPL and is slower than the GraphicEx code
procedure DecodeLZW(var Buffer: Pointer; var Count, Expected: PtrInt);
var
  NewBuffer: Pointer;
  NewCount: PtrInt;
begin
  DecompressLZW(Buffer,Count,NewBuffer,NewCount);
  FreeMem(Buffer);
  Buffer:=NewBuffer;
  Count:=NewCount;
end;
{$ENDIF}

procedure swapVoxels(var img8: ByteP0; var nhdr: TNIfTIhdr);
var
   i, nVox: integer;
   img16 : WordP0;
begin
    if (nhdr.datatype <> kDT_UINT16) and (nhdr.datatype <> kDT_INT16) then exit;
    nVox := 1;
    for i := 1 to 7 do
        if nhdr.dim[i] > 1 then
          nVox := nVox * nhdr.dim[i];
    img16 := WordP0(img8 );
    i := 0;
    for i := 0 to (nVox - 1) do
        img16[i] := swap(img16[i]);
end;

function decodeHorizontalDifferencingPredictor16(var img8: ByteP0; SamplesPerPixel, nX, nY, nZ: integer ): boolean;
//http://thorntonzone.com/manuals/Compression/Fax,%20IBM%20MMR/G3,%20G4%20compression/TIFF%20stuff/TIFPRE.TXT
var
  i, S,X,Y,Z: integer;
  img16 : WordP0;
begin
  result := false;
  if nX < 2 then exit;
  img16 := WordP0(img8 );
  i := 0;
  for Z := 1 to nZ do begin
      for Y := 1 to nY do begin
          i := i + SamplesPerPixel; //do not predict first column, X=1
          for X := 2 to nX do begin
              for S := 1 to SamplesPerPixel do begin
                  img16^[i] := img16^[i]+img16^[i-SamplesPerPixel];
                  i := i + 1;
              end;//S

          end; //X
      end; //Y
  end; //Z
  result := true;
end;

function decodeHorizontalDifferencingPredictor8(var img: ByteP0; SamplesPerPixel, nX, nY, nZ: integer ): boolean;
//http://thorntonzone.com/manuals/Compression/Fax,%20IBM%20MMR/G3,%20G4%20compression/TIFF%20stuff/TIFPRE.TXT
var
  i, S,X,Y,Z: integer;
begin
  result := false;
  if nX < 2 then exit;
  i := 0;
  for Z := 1 to nZ do begin
      for Y := 1 to nY do begin
          i := i + SamplesPerPixel; //do not predict first column, X=1
          for X := 2 to nX do begin
              for S := 1 to SamplesPerPixel do begin
                  img^[i] := img^[i]+img^[i-SamplesPerPixel];
                  i := i + 1;
              end;//S

          end; //X
      end; //Y
  end; //Z
  result := true;
end;

procedure msgTIFF(s: string);
begin
  {$IFNDEF Windows}
  writeln(s);
  {$ENDIF}
  //you could do something with these messages
end;

function swizzleDims(var nhdr: TNIFTIhdr; img: ByteP0; oZ, oT, oC: integer): boolean;
//NIfTI requires that 3rd dim is space (oZ), 4th dim is time (oT), so 5th must be Channels (oC)
var
  ztc, xyBytes, xyzBytes, xyztBytes, Z,T,C, o, iZ, iT, iC,i, p: integer;
  imgIn: byteP0;
begin
  msgTIFF(format('swizzle XYZorder %d %d %d XYZdim %d %d %d',[oZ, oT, oC, nhdr.dim[3], nhdr.dim[4], nhdr.dim[5]]));
  result := true;
  if (oZ = 3) and (oT = 4) and (oC = 5) then exit;
  if (nhdr.dim[3] < 1) then nhdr.dim[3] := 1;
  if (nhdr.dim[4] < 1) then nhdr.dim[4] := 1;
  if (nhdr.dim[5] < 1) then nhdr.dim[5] := 1;
  if ((nhdr.dim[3] < 2) and (nhdr.dim[4] < 2)) or ((nhdr.dim[3] < 2) and (nhdr.dim[5] < 2))
     or ((nhdr.dim[4] < 2) and (nhdr.dim[5] < 2)) then
        exit; //do not swizzle if volumes only in one dimension
  if oZ = 3 then begin  //must be ZCT, as ZTC exited since it is our desired solution
     iZ := 1;
     iC := (nhdr.dim[3]); //Z
     iT := (nhdr.dim[3] * nhdr.dim[5]); //Z*C
  end else if oC = 3 then begin
     iC := 1;
     if oZ = 4 then begin //CZT
        iZ := nhdr.dim[5]; //C
        iT := nhdr.dim[3] * nhdr.dim[5]; //Z *C
     end else begin //CTZ
         iT := nhdr.dim[5]; //C
         iZ := nhdr.dim[4] * nhdr.dim[5]; //C*T
     end;
  end else   if oT = 3 then begin
     iT := 1;
     if oZ = 4 then begin //TZC
        iZ := nhdr.dim[4]; //T
        iC := nhdr.dim[3] * nhdr.dim[4]; //Z *T
     end else begin //TCZ
         iC := nhdr.dim[4]; //T
         iZ := nhdr.dim[4] * nhdr.dim[5]; //C*T
     end;
  end else begin
      msgTIFF('OME order misspecified');
      exit;
  end;
  if nhdr.datatype = kDT_UINT8 then
     xyBytes := 1
  else if  nhdr.datatype = kDT_UINT16 then
       xyBytes := 2
  else if nhdr.datatype = kDT_RGB then
       xyBytes := 3;
  xyBytes := nhdr.dim[1] * nhdr.dim[2] * xyBytes; //slices per 2D slice
  xyzBytes :=  xyBytes * nhdr.dim[3];
  xyztBytes :=  xyzBytes * nhdr.dim[4];
  ztc := nhdr.dim[3] * nhdr.dim[4] * nhdr.dim[5];
  getmem(imgIn, xyBytes * ztc);
  move(img^[0],imgIn^[0], xyBytes * ztc);
  C := 0;
  T := 0;
  Z := 0;
  p := 0;
  for i := 1 to ztc do begin
    o := Z * xyBytes + T * xyzBytes + C * xyztBytes;
    move(imgIn^[p], img^[o], xyBytes);
    p := p + xyBytes;
    if ((i mod iZ) = 0) then Z := Z + 1;
    if ((i mod iT) = 0) then T := T + 1;
    if ((i mod iC) = 0) then C := C + 1;
    if (Z >= nhdr.dim[3]) then Z := 0;
    if (T >= nhdr.dim[4]) then T := 0;
    if (C >= nhdr.dim[5]) then C := 0;
  end;
  freemem(imgIn);
end;

function RGB2planar8(var img: ByteP0; SamplesPerPixel, nX, nY, nZ: integer ): boolean;
var
  o, s, XY,Z, nXY, nXYS: integer;
  imgXY: byteP0;
begin
  result := true;
  if SamplesPerPixel <= 1 then exit;
  result := false;
  nXY := nX * nY;
  nXYS := nXY * SamplesPerPixel;
  if nXY < 2 then exit;
  o := 0;
  getmem(imgXY, nXYS);
  for Z := 1 to nZ do begin
      move(img^[o],imgXY^[0], nXYS);
      for XY := 0 to (nXY-1) do begin
          for s := 0 to (SamplesPerPixel - 1) do begin
              img^[XY+ (s * nXY)] := imgXY^[o];
              o := o + 1;
          end;
      end; //XY
  end; //Z
  freemem(imgXY);
  result := true;
end;

function planar2RGB8(var img: ByteP0; SamplesPerPixel, nX, nY, nZ: integer ): boolean;
var
  o, s, XY,Z, nXY, nXYS: integer;
  imgXY: byteP0;
begin
  result := true;
  if SamplesPerPixel <= 1 then exit;
  result := false;
  nXY := nX * nY;
  nXYS := nXY * SamplesPerPixel;
  if nXY < 2 then exit;
  o := 0;
  getmem(imgXY, nXYS);
  for Z := 1 to nZ do begin
      move(img^[o],imgXY^[0], nXYS);
      for XY := 0 to (nXY-1) do begin
          for s := 0 to (SamplesPerPixel - 1) do begin
              img^[o] := imgXY^[XY+ (s * nXY)];
              o := o + 1;
          end;
      end; //XY
  end; //Z
  freemem(imgXY);
  result := true;
end;

Type TLSMINF2 = packed record //224 bytes
    //https://www.mathworks.com/matlabcentral/fileexchange/8412-lsm-file-toolbox
    //http://imagej.net/LSM_Toolbox
    MagicNumber, StructureSize,DimensionX,DimensionY,DimensionZ,DimensionChannels,
    DimensionTime,IntensityDataType,ThumbnailX,ThumbnailY: uint32;
    VoxelSizeX,VoxelSizeY,VoxelSizeZ,OriginX,OriginY,OriginZ: double;
    ScanType, SpectralScan: uint16;
    DataType,OffsetVectorOverlay,OffsetInputLut,OffsetOutputLut,OffsetChannelColors: uint32;
    TimeInterval: double;
    OffsetChannelDataTypes,OffsetScanInformation,OffsetKsData,OffsetTimeStamps,
    OffsetEventList,OffsetRoi,OffsetBleachRoi,OffsetNextRecording: uint32;
    DisplayAspectX,DisplayAspectY,DisplayAspectZ,DisplayAspectTime: double;
    OffsetMeanOfRoisOverlay, OffsetTopoIsolineOverlay, OffsetTopoProfileOverlay,
    OffsetLinescanOverlay,ToolbarFlags,OffsetChannelWavelenth,OffsetChannelFactors: uint32;
    ObjectiveSphereCorrection: double;
    OffsetUnmixParameters: uint32;
end;

procedure pswap4ui(var s : uint32);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:uint32);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  s:=outguy.Long;
end; //proc pswap4ui

function readTiff(fnm: string; out nhdr: TNIFTIhdr; out img: byteP0): string;
//http://paulbourke.net/dataformats/tiff/
label
  666;
type
  TU32s = array of uint32;
  TTIFFTag = packed record
        tagID,tagType: Word;
        count, offset: uint32;
  end; // TIFD;
  TTIFFhdr = packed record
        //ImageDescription,
        StripOffsets, StripByteCounts, ColorMap,PageNum: TTIFFTag;
        XResolution, YResolution: single;
        isLZW: boolean;
        ImageHeight, ImageWidth,
        bpp, PhotometricInterpretation, Orientation,SamplesPerPixel,
        RowsPerStrip, MinSampleValue, MaxSampleValue,
        PlanarConfig, SampleFormat,
        ResolutionUnit, Software, Predictor,
        NewSubfileType: uint32;
        OK: boolean;
  end;
  TOME = packed record
        oX, oY, oZ, oT, oC, //order, e.g. DimensionOrder="XYZTC" means oX=1 and oT=4
        nX, nY, nZ, nT, nC: uint32;
        spacing: single;
        isImageJ: boolean;

  end;
const
  kMaxIFD = 2048;
  kPREDICTOR_NONE = 1;
  kPREDICTOR_HORIZONTAL = 2;
var
  tTag: TTIFFTag;
  lsm: TLSMINF2;
  ome: TOME;
  lOffsets, lCounts: TU32s;
  lzw: pointer;
  lzwCount: PtrInt;
  fsz, i, j, nOK, nImgBytes, imgBytes, ok1, sliceBytesLeft: integer;
  outCount: PtrInt;
  IFDstart: uint32;
  w, nTag, nIFD: uint16;
  f: file;
  eStr: string;
  swapEndian, isLSM, isOME: boolean;
  hdr: array[1..kMaxIFD] of TTIFFhdr;
function readU16: word;
begin
  result := 0;
  BlockRead(f, result, sizeof(result));
  if swapEndian then result := swap(result);
end;
function readU32: uint32;
begin
  result := 0;
  BlockRead(f, result, sizeof(result));
  if swapEndian then pswap4ui(result);
end;
function readIFD: TTIFFTag;
begin
    BlockRead(f, result, sizeof(result));
    if swapEndian then begin
       result.tagID := swap(result.tagID);
       result.tagType := swap(result.tagType);
       pswap4ui(result.count);
       pswap4ui(result.offset);
       {$IFDEF ENDIAN_BIG}check next lines!{$ENDIF}
       if (result.Count = 1) and (result.tagType = 1) then  //32->8
          result.offset := result.offset shr 24;
       if (result.Count = 1) and (result.tagType = 3) then  //32->16
           result.offset := result.offset shr 16;
    end;
end;

procedure readTagArray(lTag: TTIFFTag; out vals: TU32s);
var
  b: byte;
  p,i: integer;
begin
    if lTag.Count < 1 then exit;
    setlength(vals, lTag.Count);
    if lTag.Count = 1 then begin
       vals[0] := lTag.offset;
       exit;
    end;
    p := filepos(f);
    seek(f, lTag.offset);
    for i := 0 to (lTag.count-1) do begin
      if lTag.tagType = 1 then begin
         BlockRead(f, b, sizeof(b));
         vals[i] :=b;
      end else if lTag.tagType = 3 then
          vals[i] := readU16
      else if lTag.tagType = 4 then
          vals[i] := readU32;
      //memo1.lines.add(inttostr(vals[i]));
    end;
    seek(f, p); //return
end;

procedure readLSM;
var
   p: integer;
begin
     if tTag.Count < sizeof(lsm) then exit;
     isLSM := true;
     p := filepos(f);
     seek(f, tTag.offset);
     BlockRead(f, lsm, sizeof(lsm));
     seek(f, p); //return
end;

function getVal(substr, str: string): string;
//getval('DimensionOrder','DimensionOrder="XYZCT"') returns 'XYZCT'
var
   i: integer;
begin
  result := '';
  i := AnsiPos(substr,str);
  if  i < 1 then exit;
  while (i < length(str)) and (str[i] <> '=') do
        i := i + 1;
  i := i + 2;
  while (i < length(str)) and (str[i] <> '"') do begin
        result := result + str[i];
        i := i + 1;
  end;
end;

function readOME: boolean;
var
   p: integer;
   oStr, str: string;
begin
     result := false;
     if tTag.count < 12 then exit;
     p := filepos(f);
     seek(f, tTag.offset);
     SetLength(str, tTag.count);
     BlockRead(f, str[1], length(str));
     seek(f, p); //return
     if AnsiPos('</OME>',str) < 1 then exit;
     oStr := getVal('DimensionOrder',str);
     ome.Spacing := 1.0;
     ome.oX := AnsiPos('X',oStr);
     ome.oY := AnsiPos('Y',oStr);
     ome.oZ := AnsiPos('Z',oStr);
     ome.oT := AnsiPos('T',oStr);
     ome.oC := AnsiPos('C',oStr);
     //form1.memo1.lines.add(format('%d %d %d %d %d',[ome.oX, ome.oY, ome.oZ, ome.oT, ome.oC]));
     //oX, oY, oZ, oT, oC, //order, e.g. DimensionOrder="XYZTC" means oX=1 and oT=4
     ome.nX := strtointdef(getVal('SizeX',str), 1);
     ome.nY := strtointdef(getVal('SizeY',str), 1);
     ome.nZ := strtointdef(getVal('SizeZ',str), 1);
     ome.nT := strtointdef(getVal('SizeT',str), 1);
     ome.nC := strtointdef(getVal('SizeC',str), 1);
     ome.isImageJ := false;
     //DimensionOrder="XYZCT" ID="Pixels:0" SizeC="1" SizeT="1" SizeX="439" SizeY="167" SizeZ="1"
     //if not ContainsText(str,'</OME>') then exit;
     isOME := true;
     result := true;
end;

function getValJ(substr, str: string): string;
//getval('slices=','slices=5') returns '5'
var
   i: integer;
begin
  result := '';
  i := AnsiPos(substr,str);
  if i < 1 then exit;
  i := i + length(substr);
  while (i < length(str)) and (str[i] in ['0'..'9','.']) do begin
        result := result + str[i];
        i := i + 1;
  end;
end;

function readImageJ: boolean;
var
   p: integer;
   str: string;
begin
     result := false;
     if tTag.count < 12 then exit;
     p := filepos(f);
     seek(f, tTag.offset);
     SetLength(str, tTag.count);
     BlockRead(f, str[1], length(str));
     seek(f, p); //return
     if AnsiPos('ImageJ',str) < 1 then exit;
     ome.oZ := 4;
     ome.oT := 5;
     ome.oC := 3;
     ome.nC := strtointdef(getValJ('channels=',str), 1);
     ome.nT := strtointdef(getValJ('frames=',str), 1);
     ome.nZ := strtointdef(getValJ('slices=',str), 1);
     ome.spacing:= strtofloatdef(getValJ('spacing=',str), 1); ;
     //clipboard.asText := str+ format('*ZTC %d %d %d',[ome.nZ,ome.nT, ome.nC]);
     ome.isImageJ := true;
     isOME := true;
     result := true;
end;

//http://www.fileformat.info/format/tiff/egff.htm
//tagType 1=uint8, 2=ASCISS, 3=uint16, 4=uint32, 5=(rational)uint32/uint32
function readBpp: integer;//sum of all Bytes-per-pixel
var
  lVals: TU32s;
  i: integer;
begin
    //Form1.memo1.lines.Add(format('@ %d bpp %x %d %d %d', [filepos(f), tTag.tagID,tTag.tagType, tTag.count, tTag.offset]));
    result := tTag.offset;
    if tTag.Count < 2 then exit;
    result := 0;
    readTagArray(tTag, lVals);
    for i := 0 to (tTag.count-1) do begin
        result := result + lVals[i];
        //showmessage('+'+inttostr(lVals[i]));
    end;
    //showmessage('='+inttostr(result));
    lVals := nil;
end; //readBpp()
function readRationalTag: single;
var
  p, numer, denom: integer;
begin
  result := 0;
  if tTag.tagType <> 5 then exit;
  p := filepos(f);
  seek(f, tTag.offset);
  numer := readU32;
  denom := readU32;
  if denom <> 0 then
    result := numer/denom;
  seek(f, p); //return
end; //readRationalTag()
begin
  NII_Clear(nhdr);
  ome.oZ := 3; ome.oT := 4; ome.oC := 5; //dimension order: space, time, channels
  swapEndian := false;
  isLSM := false;
  isOME := false;
  img:= nil;
  result := 'unable to find file '+fnm;
  if not fileexists(fnm) then exit;
  result := 'folder not file '+fnm;
  if DirectoryExists(fnm) then exit;
  eStr := 'file too small';
  fsz := FSize(fnm);
  if fsz < 32 then exit;
  eStr := 'unable to open file '+fnm;

  {$I-}
  AssignFile(f, fnm);
  FileMode := 0;  //Set file access to read only
  Reset(f, 1);
  {$I+}
  if ioresult <> 0 then begin
        goto 666;
        exit;
  end;
  //read header
  w := 0;
  BlockRead(f, w, sizeof(w)); //Byte-order Identifier
  eStr := 'signature';
  {$IFDEF ENDIAN_BIG}
  if w = $4D4D then //big endian
     swapEndian := false
  else if w = $4949 then //little-endian
     swapEndian := true
  {$ELSE}
  if w = $4D4D then //big-endian
     swapEndian := true
  else if w = $4949 then //little-endian
     swapEndian := false
  {$ENDIF}
  else
      goto 666;
  w := readU16; //TIFF version number (always 2Ah)
  if w <> $2A then goto 666;
  IFDstart := readU32; //offset to IFD
  if IFDstart < 8 then goto 666;
  //read all the IFDs
  nIFD := 0;
  while IFDstart > 0 do begin
    eStr := 'offset exceeds filesize';
    if IFDstart > fsz then goto 666;
    nIFD := nIFD + 1;
    eStr := 'two many IFDs (exceeds '+inttostr(kMaxIFD)+')';
    if nIFD > kMaxIFD then goto 666;
    //read IFD
    seek(f, IFDstart);
    nTag := readU16;
    eStr := 'nTag'+inttostr(nTag);
    if nTag < 1 then goto 666;
    //memo1.lines.Add(format('niFD = %d', [nIFD]));
    hdr[nIFD].ImageWidth := 0;
    hdr[nIFD].ImageHeight := 0;
    hdr[nIFD].PlanarConfig := 0;
    hdr[nIFD].XResolution := 1.0;
    hdr[nIFD].YResolution := 1.0;
    hdr[nIFD].Predictor := kPREDICTOR_NONE;
    hdr[nIFD].OK := true;
    for i := 1 to nTag do begin
        tTag := readIFD;
        //memo1.lines.Add(format('%x %d %d %d', [IFD.tagID,IFD.tagType, IFD.count, IFD.offset]));
        case tTag.tagID of
          //http://www.fileformat.info/format/tiff/corion.htm
          //http://www.awaresystems.be/imaging/tiff/tifftags.html
          ////tagType 1=uint8, 2=ASCISS, 3=uint16, 4=uint32, 5=(rational)uint32/uint32
          $00FE : hdr[nIFD].NewSubfileType := tTag.offset;
          $0100 : hdr[nIFD].ImageWidth := tTag.offset;
          $0101 : hdr[nIFD].ImageHeight := tTag.offset;
          $0102 : hdr[nIFD].bpp := readBpp;
          $0103 : begin
               //memo1.lines.Add(format('format %x %d %d %d', [tTag.tagID,tTag.tagType, tTag.count, tTag.offset]));
               if (tTag.offset <> 1) and (tTag.offset <> 5) then begin
                  eStr := 'Unsupported compression format '+inttostr(tTag.offset)+' try Fiji/ImageJ';
                  goto 666;
               end;
               hdr[nIFD].isLZW := (tTag.offset = 5);
            end;
          $0106 : hdr[nIFD].PhotometricInterpretation := tTag.offset;
          $010E :  begin
                if not readOME then
                   readImageJ;
                end;
          $0111 : hdr[nIFD].StripOffsets := tTag;
          $0112 : hdr[nIFD].Orientation := tTag.offset;
          $0115 : hdr[nIFD].SamplesPerPixel := tTag.offset;
          $0116 : hdr[nIFD].RowsPerStrip := tTag.offset;
          $0117 : hdr[nIFD].StripByteCounts := tTag;
          $0118 : hdr[nIFD].MinSampleValue := tTag.offset;
          $0119 : hdr[nIFD].MaxSampleValue := tTag.offset;
          $011A : hdr[nIFD].XResolution := readRationalTag;
          $011B : hdr[nIFD].YResolution := readRationalTag;
          $011C : hdr[nIFD].PlanarConfig := tTag.offset;
          $011D : hdr[nIFD].PageNum := tTag;
          $0128 : hdr[nIFD].ResolutionUnit := tTag.offset;
          $0131 : hdr[nIFD].Software := tTag.offset;
          $013D : begin //http://www.awaresystems.be/imaging/tiff/tifftags/predictor.html
                    hdr[nIFD].Predictor := tTag.offset;
                    if (hdr[nIFD].Predictor <> kPREDICTOR_NONE) and (hdr[nIFD].Predictor <> kPREDICTOR_HORIZONTAL) then begin
                       eStr := 'Unsupported predictor '+inttostr(hdr[nIFD].Predictor);
                       goto 666;
                    end;
                  end;
          $0140 : hdr[nIFD].ColorMap := tTag;
          $142 : begin
               eStr := 'TIFF tiles not supported';
               goto 666;
            end;
          $0153 : hdr[nIFD].SampleFormat := tTag.offset;
          34412: readLSM;
          else msgTIFF(format('tagID %4x %d %d %d', [tTag.tagID,tTag.tagType, tTag.count, tTag.offset]));
          //8639, 863A, 8652
        end; //case
        //form1.memo1.lines.Add(format('tagID %4x %d %d %d', [tTag.tagID,tTag.tagType, tTag.count, tTag.offset]));

    end;
    IFDstart := readU32; //offset to IFD
    //form1.Memo1.lines.add(format('%d %d', [hdr[nIFD].ImageWidth,hdr[nIFD].ImageHeight]));
    //IFDstart := 0;//xxx
  end; //for each IFD
  //memo1.lines.add(inttostr(iIFD));
  //remove thumbnails - find biggest image
  ok1 := 1;
  for i := 1 to nIFD do
      if (hdr[i].ImageHeight*hdr[i].ImageWidth) > (hdr[ok1].ImageHeight*hdr[ok1].ImageWidth) then
         ok1 := i;
  for i := 1 to nIFD do //discard small images
      if (hdr[i].ImageHeight*hdr[i].ImageWidth) < (hdr[ok1].ImageHeight*hdr[ok1].ImageWidth) then
         hdr[i].OK := false;
  //check all IFDs have 2D images of the same size
  eStr := 'All images must have the same dimensions and precision';
  nOK := 0;
  for i := 1 to nIFD do begin
      if not(hdr[i].OK) then continue;
      nOK := nOK + 1;
      if (hdr[ok1].bpp <> hdr[i].bpp)
       or (hdr[ok1].SamplesPerPixel <> hdr[i].SamplesPerPixel)
       or (hdr[ok1].ImageHeight <> hdr[i].ImageHeight)
       or (hdr[ok1].ImageWidth <> hdr[i].ImageWidth) then
         goto 666;
  end;
  //check sensible bytes-per-pixel
  eStr := 'Bad BPP '+inttostr(hdr[ok1].bpp);
  if (hdr[ok1].bpp < 8) or ((hdr[ok1].bpp mod 8) <>  0) then
     goto 666;
  //decode image
   nImgBytes := (hdr[ok1].bpp div 8) * hdr[ok1].ImageHeight * hdr[ok1].ImageWidth * nOK;
   imgBytes := 0;


   GetMem(img, nImgBytes);
   for i := 1 to nIFD do begin
       if not (hdr[i].OK) then continue;
       readTagArray(hdr[i].StripOffsets, lOffsets);
       readTagArray(hdr[i].StripByteCounts, lCounts);
       if (length(lOffsets) <> length(lCounts)) or (length(lOffsets) < 1) then exit;
       outCount := (hdr[ok1].bpp div 8) * hdr[ok1].ImageWidth * hdr[ok1].RowsPerStrip;
       sliceBytesLeft :=  (hdr[ok1].bpp div 8) * hdr[ok1].ImageHeight * hdr[ok1].ImageWidth;
       for j := 0 to (length(lOffsets)-1) do begin
           seek(f,lOffsets[j]);
           if hdr[i].isLZW then begin
              getmem(lzw, lCounts[j]);
              blockread(f,lzw^, lCounts[j]);
              lzwCount := lCounts[j];
              if outCount > sliceBytesLeft then
                 outCount := sliceBytesLeft;
              DecodeLZW(lzw, lzwCount, outCount);
              sliceBytesLeft := sliceBytesLeft - outCount;
              //DecompressLZW(@stripLzw, lCounts[j], unZipBuffer, unZipBytes);//out NewBuffer: PByte; out NewCount: PtrInt);
              //memo1.Lines.Add(format('%d -> %d %d',[lCounts[j],lzwCount, lOffsets[j]]) );
              move(lzw^,img[imgBytes], lzwCount);
              imgBytes := imgBytes + lzwCount;
              freemem(lzw);

           end else begin
               blockread(f,img[imgBytes], lCounts[j]);
               imgBytes := imgBytes + lCounts[j];
           end;
       end;

   end;
   //convert planar RRR..RGGG...GBBB..B to triplesRGBRGBRGB
   if (not hdr[i].isLZW) and (hdr[ok1].PlanarConfig = 2) and (hdr[ok1].SamplesPerPixel > 1) and ((hdr[ok1].bpp div hdr[ok1].SamplesPerPixel) = 8)  then begin
     //see sample "Oxford" - added "not hdr[i].isLZW"
     msgTIFF(format('deplane %d',[hdr[ok1].PlanarConfig]));
      eStr := 'planar to RGB';
      if not planar2RGB8(img, hdr[ok1].SamplesPerPixel, hdr[ok1].ImageWidth, hdr[ok1].ImageHeight, nOK ) then
         goto 666;
   end;
   //apply predictor
   if hdr[ok1].Predictor = kPREDICTOR_HORIZONTAL then begin
      eStr := 'decode horizontal prediction';
      if (hdr[ok1].bpp div hdr[ok1].SamplesPerPixel) = 8 then begin//e.g. 32/4 24/3 8/1
         if not decodeHorizontalDifferencingPredictor8(img, hdr[ok1].SamplesPerPixel, hdr[ok1].ImageWidth, hdr[ok1].ImageHeight, nOK ) then
                goto 666;
      end else if (hdr[ok1].bpp div hdr[ok1].SamplesPerPixel) = 16 then begin//e.g. 16/1
         if not decodeHorizontalDifferencingPredictor16(img, hdr[ok1].SamplesPerPixel, hdr[ok1].ImageWidth, hdr[ok1].ImageHeight, nOK ) then
                goto 666;
      end else
          goto 666;
   end;

  nhdr.dim[0]:=3;//3D
  nhdr.dim[1]:=hdr[ok1].ImageWidth;
  nhdr.dim[2]:=hdr[ok1].ImageHeight;
  msgTIFF(format('-> %d',[hdr[ok1].SamplesPerPixel]));
  if (hdr[ok1].SamplesPerPixel > 3) then begin
     hdr[ok1].bpp := hdr[ok1].bpp div hdr[ok1].SamplesPerPixel;
     if (hdr[ok1].SamplesPerPixel = 4)  and (hdr[ok1].PlanarConfig = 1) and (hdr[ok1].bpp = 8) then begin
      msgTIFF(format('replane %d',[hdr[ok1].PlanarConfig]));
      eStr := 'planar to RGB';
      if not RGB2planar8(img, hdr[ok1].SamplesPerPixel, hdr[ok1].ImageWidth, hdr[ok1].ImageHeight, nOK ) then
         goto 666;
     end;
     nhdr.dim[3]:=hdr[ok1].SamplesPerPixel;
     nhdr.dim[4]:=nOK;
  end else if isOME then begin
     msgTIFF(format('OME slices=%d Z=%d  T=%d C=%d',[nOK, ome.nZ, ome.nT, ome.nC]));
     if ome.isImageJ then begin
        ome.nX := nhdr.dim[1];
        ome.nY := nhdr.dim[2];
     end;
     if (nOK <> (ome.nZ * ome.nT * ome.nC)) or (ome.nX <> nhdr.dim[1]) or (ome.nY <> nhdr.dim[2]) then begin
        isOME := false; //do not swizzle
        nhdr.dim[3]:=nOK;
        nhdr.dim[4]:=1;
     end else begin
          nhdr.dim[3]:=ome.nZ;//NIFTI requires 3rd dim is spatial
          nhdr.dim[4]:=ome.nT;//NIfTI requires 4th dim is time
          nhdr.dim[5]:=ome.nC;//therefore, Channels must be 5th dim!
     end;
  end else begin
      nhdr.dim[3]:=nOK;
      nhdr.dim[4]:=1;
  end;
  if isOme and ome.isImageJ then
     nhdr.pixdim[1]:=ome.Spacing
  else
      nhdr.pixdim[1]:=1.0;
  nhdr.pixdim[2]:=nhdr.pixdim[1];
  nhdr.pixdim[3]:=nhdr.pixdim[1];
  if isLsm then begin
     msgTIFF(format('ZT %d %d',[nhdr.dim[3], nhdr.dim[4]]));
     msgTIFF(format('ZCT %d %d %d',[lsm.DimensionZ, lsm.DimensionChannels, lsm.DimensionTime]));
     if ((nhdr.dim[3] * nhdr.dim[4]) = (lsm.DimensionZ * lsm.DimensionChannels * lsm.DimensionTime)) then begin //todo swizzle Channels & Time
       nhdr.dim[3]:= lsm.DimensionZ;
       nhdr.dim[4]:= lsm.DimensionChannels;
       nhdr.dim[5]:= lsm.DimensionTime;

     end;
     msgTIFF(format('ZCT %d %d %d',[nhdr.dim[3],nhdr.dim[4],nhdr.dim[5]]));
     nhdr.pixdim[1]:= lsm.VoxelSizeX * 1000000;
     nhdr.pixdim[2]:= lsm.VoxelSizeY * 1000000;
     nhdr.pixdim[3]:= lsm.VoxelSizeZ * 1000000;
     nhdr.xyzt_units:= kNIFTI_UNITS_MICRON +kNIFTI_UNITS_SEC;
     msgTIFF(format('pixdim %g %g %g',[nhdr.pixdim[1],nhdr.pixdim[2],nhdr.pixdim[3]]));
  end;
  eStr:= 'unsupported dataType '+inttostr(hdr[ok1].bpp);
  case hdr[ok1].bpp of
       8: nhdr.datatype := kDT_UINT8;
       16: nhdr.datatype := kDT_UINT16;
       24: nhdr.datatype := kDT_RGB;
       else goto 666;
  end;
  if (swapEndian) and (nhdr.datatype = kDT_UINT16) then
     swapVoxels(img, nhdr);
  if isOME then
     swizzleDims(nhdr, img, ome.oZ, ome.oT, ome.oC);
  if isLSM then
     swizzleDims(nhdr, img, 4, 3, 5);

  nhdr.vox_offset := 352;
  nhdr.sform_code := 1;
  nhdr.srow_x[0]:=nhdr.pixdim[1];nhdr.srow_x[1]:=0.0;nhdr.srow_x[2]:=0.0;nhdr.srow_x[3]:=0.0;
  nhdr.srow_y[0]:=0.0;nhdr.srow_y[1]:=-nhdr.pixdim[2];nhdr.srow_y[2]:=0.0;nhdr.srow_y[3]:=0.0;
  nhdr.srow_z[0]:=0.0;nhdr.srow_z[1]:=0.0;nhdr.srow_z[2]:=nhdr.pixdim[3];nhdr.srow_z[3]:=0.0;
  convertForeignToNifti(nhdr);
  eStr := ''; //no error
  msgTIFF(format('bytes %d',[nImgBytes]));
  666:
  FileMode := 2;
  CloseFile(f);
  result := eStr;
end;

function Save2Nii(fnm: string; nhdr: TNIFTIhdr; img: byteP0): string;
var
  f: file;
  outFnm: string;
  nImgBytes: integer;
  u0 : uint32;
begin
  result := '';
  outFnm := changefileext(fnm,'.nii');
  if fileexists(outFnm) then begin
     showmessage('File already exists '+outFnm);
     exit;
  end;
  nhdr.vox_offset := 352;
  AssignFile(f, outFnm);
  FileMode := 2;
  Rewrite(f, 1);
  Blockwrite(f,nhdr,sizeof(nhdr));
  u0 := 0; //348 byte header padded to 352
  Blockwrite(f,u0,sizeof(u0));
  nImgBytes := (nhdr.bitpix div 8) * nhdr.dim[1] * nhdr.dim[2] * nhdr.dim[3];
  if (nhdr.datatype = kDT_RGB) then nImgBytes := nImgBytes * 3;
  if (nhdr.dim[4] > 1) then nImgBytes := nImgBytes * nhdr.dim[4];
  if (nhdr.dim[5] > 1) then nImgBytes := nImgBytes * nhdr.dim[5];
  Blockwrite(f,img[0],nImgBytes);
  CloseFile(f);
  Freemem(img);
  result := outFnm;
end;

function SaveTIFFAsNifti(fnm: string): string;
var
  s: string;
  nhdr: TNIFTIhdr;
  img: byteP0;
begin
  result := '';
  if not isTIFF(fnm) then exit;
  s := readTiff(fnm, nhdr, img);
  if s <> '' then begin
     showmessage('TIFF error '+s);
     exit;
  end;
  result := save2nii(fnm, nhdr, img);
end;

end.

