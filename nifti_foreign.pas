unit nifti_foreign;
//{$DEFINE MRIcron}
interface
//{$mode objfpc} //test
{$mode Delphi}
{$H+}
{$DEFINE GZIP}
{$Include isgui.inc}//<- {$DEFINE GUI}
//{$DEFINE GL10} //define for MRIcroGL1.0, comment for MRIcroGL1.2 and later
{$H+}
{$IFDEF GL10}
{$Include isgui.inc}
{$ENDIF}
uses
{$IFDEF MRIcron}define_types,{$ENDIF}
{$IFDEF GL10} define_types, {$IFNDEF FPC}gziod,{$ELSE}gzio2,{$ENDIF}{$ENDIF}
{$IFDEF GZIP}zstream, {$ENDIF}
{$IFDEF GUI}
 dialogs,lazfileutils,
{$ELSE}
 dialogsx,
{$ENDIF}
//ClipBrd,
 nifti_types,  sysutils, classes, StrUtils, math;//2015! dialogsx

const
    kIVers = ' i2nii v1.0.0707';

{$IFDEF GL10}
procedure NII_Clear (out lHdr: TNIFTIHdr);
procedure NII_SetIdentityMatrix (var lHdr: TNIFTIHdr); //create neutral rotation matrix
{$ELSE}

const
      //seee README.attributes.html and parser_int.c
      //FUNC_FIM_TYPE = 0; // 1 value
      //FUNC_THR_TYPE = 1; //obsolete
      kFUNC_NOT_STAT =0;
      kFUNC_COR_TYPE =2;// Correlation Coeff # Samples, # Fit Param, # Orts
      kFUNC_TT_TYPE = 3;//  Student t         Degrees-of-Freedom (DOF)
      kFUNC_FT_TYPE = 4;//  F ratio           Numerator DOF, Denominator DOF
      kFUNC_ZT_TYPE = 5;//  Standard Normal   -- none --
      kFUNC_CT_TYPE = 6;//  Chi-Squared       DOF
      kFUNC_BT_TYPE = 7;//  Incomplete Beta   Parameters "a" and "b"
      kFUNC_BN_TYPE = 8;//  Binomial          # Trials, Probability per trial
      kFUNC_GT_TYPE = 9;//  Gamma             Shape, Scale
      kFUNC_PT_TYPE = 10;//  Poisson           Mean
      kFUNC_NO_STAT_AUX = 13;
type
  TFloatVec = record
    ar: array of single;
    dx: single;
    x0: single;
    function nar: integer; inline;
  end;

type
   TAFNI = record
		jv: integer; //statistical code
		nv: integer; //number of parameters that follow (may be 0)
		param: array [0..2] of single;
                scl_slopex: single;
                minVal, maxVal, maxAbsVal: single;
		nam: string[64];
        FDRcurv: TFloatVec;
   end;
   TAFNIs = array of TAFNI;

type

  mat44 = array [0..3, 0..3] of Single;
  {$IFNDEF MRIcron}
    	ByteRA = array [1..1] of byte;

	Bytep = ^ByteRA;
        procedure UnGZip(const FileName: string; buffer: bytep; offset, sz: integer);
  {$ENDIF}
{$ENDIF}
function readAFNIHeader(var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian, isAllVolumesSame: boolean;  var AFNIs: TAFNIs; var fLabels: TStringList): boolean; overload;
function readAFNIHeader(var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean): boolean; overload;
function isConcordeHeader(var fname: string): boolean;
function isINTERFILE(var fname: string): boolean;
function readForeignHeader(var lFilename: string; var lHdr: TNIFTIhdr; var gzBytes: int64; var swapEndian, isDimPermute2341: boolean; out xDim64: int64): boolean; overload;
function readForeignHeader(var lFilename: string; var lHdr: TNIFTIhdr; var gzBytes: int64; var swapEndian, isDimPermute2341: boolean): boolean; overload;
function IsReadable(const AFilename: string): boolean;
procedure convertForeignToNifti(var nhdr: TNIFTIhdr);
function FSize (lFName: String): Int64;
function isTIFF(fnm: string): boolean;
procedure nifti_mat44_to_quatern( lR :mat44; out qb, qc, qd, qx, qy, qz, dx, dy, dz, qfac : single);

implementation

const
    kNaNSingle : single = 1/0;
Type
  vect4 = array [0..3] of Single;
  mat33 = array [0..2, 0..2] of Single;
  vect3 = array [0..2] of Single;
  ivect3 = array [0..2] of integer;

procedure printf(str: string);
begin
  {$IFDEF Windows}
  if IsConsole then //'System' in uses
     writeln(str);
  {$ENDIF}
  {$IFDEF UNIX} writeln(str);{$ENDIF}
end;


(*
//https://github.com/neurolabusc/surf-ice/issues/35
function IsReadable(fnm: string): boolean;
//https://wiki.freepascal.org/macOS_Programming_Tips#Determining_if_a_file_is_readable
{$IFDEF Darwin}
var
  f: file;
  b: byte;
{$ENDIF}
begin
  result := false;
  if not fileexists(fnm) then exit;
  if FSize(fnm) < 2 then exit;
  {$IFDEF Darwin}
  AssignFile(f, fnm);
  {$I+}
  try
    FileMode := fmOpenRead;  //Set file access to read only
    Reset(f, 1);
    if ioresult <> 0 then
       exit;
    b := 0;
    BlockRead(f, b, sizeof(b)); //Byte-order Identifier
    CloseFile(f);
    result := true;
  except
    result := false;
  end;
  {$ELSE}
  result := true;
  {$ENDIF}
end; *)

(*function IsReadable (fnm: string): boolean;
//https://wiki.freepascal.org/macOS_Programming_Tips#Determining_if_a_file_is_readable
var
  fs : TFileStream;
  b: byte;
begin
  result := false;
  if not fileexists(fnm) then begin
    printf('File does not exist: '+fnm);
    exit;
  end;
  if FSize(fnm) < 2 then begin
    printf('Empty file: '+fnm);
    exit;
  end;
  {$IFDEF Darwin}
  {$I-}
  try
  fs := TFileStream.Create (fnm, fmOpenRead or fmShareDenyWrite);
  except
    printf('Unable to open (permissions): '+fnm);
    fs.Free;
    exit(false);
  end;
  try
    fs.Seek(0, soFromBeginning);
    fs.Read(b, 1);
    //writeln(inttostr(b));
  except
    printf('Unable to read: '+fnm);
    fs.Free;
    exit(false);
  end;
  {$ENDIF}
  fs.Free;
  result := true;
end;*)
function IsReadable (const AFilename: string): boolean;
// macOS will block applications from reading files outside their sandbox
// therefore, simply knowing the user has read acces is not sufficient
// requires "uses LazFileUtils"
var
  f: file;
  b: byte;
begin
  Result := lazfileutils.FileIsReadable(AFilename);
  {$IFDEF GUI}
  {$IFDEF Darwin}
  if not Result then exit; //globally not readable
  if FileSizeUtf8(AFilename) < 1 then exit(false);
  AssignFile(f, AFilename);
  {$I+}
  try
    FileMode := fmOpenRead;  //Set file access to read only
    Reset(f, 1);
    if ioresult <> 0 then
       exit;
    b := 0;
    BlockRead(f, b, sizeof(b));
    CloseFile(f);
    result := true;
  except
    result := false;
  end;
  {$ENDIF}
  {$ENDIF}
end;


{$IFDEF GL10}
procedure NII_SetIdentityMatrix (var lHdr: TNIFTIHdr); //create neutral rotation matrix
var lInc: integer;
begin
	with lHdr do begin
		 for lInc := 0 to 3 do
			 srow_x[lInc] := 0;
		 for lInc := 0 to 3 do
             srow_y[lInc] := 0;
         for lInc := 0 to 3 do
             srow_z[lInc] := 0;
         for lInc := 1 to 16 do
             intent_name[lInc] := chr(0);
         //next: create identity matrix: if code is switched on there will not be a problem
		 srow_x[0] := 1;
         srow_y[1] := 1;
         srow_z[2] := 1;
    end;
end; //proc NIFTIhdr_IdentityMatrix

procedure NII_Clear (out lHdr: TNIFTIHdr);
var
 lInc: integer;
begin
  with lHdr do begin
    HdrSz := sizeof(TNIFTIhdr);
    for lInc := 1 to 10 do
       Data_Type[lInc] := chr(0);
    for lInc := 1 to 18 do
       db_name[lInc] := chr(0);
    extents:=0;
    session_error:= 0;
    regular:='r'{chr(0)};
    dim_info:=(0);
    dim[0] := 4;
    for lInc := 1 to 7 do
       dim[lInc] := 0;
    intent_p1 := 0;
    intent_p2 := 0;
    intent_p3 := 0;
    intent_code:=0;
    datatype:=0 ;
    bitpix:=0;
    slice_start:=0;
    for lInc := 1 to 7 do
       pixdim[linc]:= 1.0;
    vox_offset:= 0.0;
    scl_slope := 1.0;
    scl_inter:= 0.0;
    slice_end:= 0;
    slice_code := 0;
    xyzt_units := 10;
    cal_max:= 0.0;
    cal_min:= 0.0;
    slice_duration:=0;
    toffset:= 0;
    glmax:= 0;
    glmin:= 0;
    for lInc := 1 to 80 do
      descrip[lInc] := chr(0);{80 spaces}
    for lInc := 1 to 24 do
      aux_file[lInc] := chr(0);{80 spaces}
    {below are standard settings which are not 0}
    bitpix := 16;//vc16; {8bits per pixel, e.g. unsigned char 136}
    DataType := 4;//vc4;{2=unsigned char, 4=16bit int 136}
    Dim[0] := 3;
    Dim[1] := 256;
    Dim[2] := 256;
    Dim[3] := 1;
    Dim[4] := 1; {n vols}
    Dim[5] := 1;
    Dim[6] := 1;
    Dim[7] := 1;
    glMin := 0;
    glMax := 255;
    qform_code := kNIFTI_XFORM_UNKNOWN;
    sform_code:= kNIFTI_XFORM_UNKNOWN;
    quatern_b := 0;
    quatern_c := 0;
    quatern_d := 0;
    qoffset_x := 0;
    qoffset_y := 0;
    qoffset_z := 0;
    NII_SetIdentityMatrix(lHdr);
    magic := kNIFTI_MAGIC_SEPARATE_HDR;
  end; //with the NIfTI header...
end;
{$ENDIF}

function UpCaseExt(lFileName: string): string;
var lI: integer;
l2ndExt,lExt : string;
begin
         lExt := ExtractFileExt(lFileName);
         if length(lExt) > 0 then
        	for lI := 1 to length(lExt) do
        		lExt[lI] := upcase(lExt[lI]);
         result := lExt;
         if lExt <> '.GZ' then exit;
         lI := length(lFileName) - 6;
         if li < 1 then exit;
         l2ndExt := upcase(lFileName[lI])+upcase(lFileName[lI+1])+upcase(lFileName[li+2])+upcase(lFileName[li+3]);
         if (l2ndExt = '.NII')then
        	result :=  l2ndExt+lExt
         else if  (l2ndExt = 'BRIK') and (lI > 1) and (lFileName[lI-1] = '.') then
              result := '.BRIK'+lExt;
end;

{$IFNDEF GL10}
procedure UnGZip(const FileName: string; buffer: bytep; offset, sz: integer);
{$IFDEF GZIP}
var
   decomp: TGZFileStream;
   skip: array of byte;
begin
     decomp := TGZFileStream.create(FileName, gzopenread);
     if offset > 0 then begin
        setlength(skip, offset);
        decomp.Read(skip[0], offset);
     end;
     //buffer^[0] :=  0; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
     //decomp.Read(buffer[0], sz);
     decomp.Read(buffer[1], sz);
     decomp.free;
end;
{$ELSE}
begin
  {$IFDEF UNIX} writeln('Recompile with GZ support!'); {$ENDIF}
end;
{$ENDIF}
{$ENDIF}
(*  function isECAT(fnm: string): boolean;
  type
  THdrMain = packed record //Next: ECAT signature
    magic: array[1..14] of char;
  end;
  var
    f: file;
    mhdr: THdrMain;
  begin
    result := false;
    if not fileexists(fnm) then exit;
    if DirectoryExists(fnm) then exit;
    if FSize(fnm) < 32 then exit;
    {$I-}
    AssignFile(f, fnm);
    FileMode := fmOpenRead;  //Set file access to read only
    Reset(f, 1);
    {$I+}
    if ioresult <> 0 then
       exit;
    BlockRead(f, mhdr, sizeof(mhdr));
    closefile(f);
    if ((mhdr.magic[1] <> 'M') or (mhdr.magic[2] <> 'A') or (mhdr.magic[3] <> 'T') or (mhdr.magic[4] <> 'R') or (mhdr.magic[5] <> 'I') or (mhdr.magic[6] <> 'X')) then
       exit;
    result := true;
  end; *)
  function FSize (lFName: String): Int64;
var SearchRec: TSearchRec;
begin
  result := 0;
  if not fileexists(lFName) then exit;
  FindFirst(lFName, faAnyFile, SearchRec);
  result := SearchRec.size;
  FindClose(SearchRec);
end;

  function Swap2(s : SmallInt): smallint;
  type
    swaptype = packed record
      case byte of
        0:(Word1 : word); //word is 16 bit
        1:(Small1: SmallInt);
    end;
    swaptypep = ^swaptype;
  var
    inguy:swaptypep;
    outguy:swaptype;
  begin
    inguy := @s; //assign address of s to inguy
    outguy.Word1 := swap(inguy^.Word1);
    result :=outguy.Small1;
  end;

procedure swap4(var s : LongInt);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:LongInt);
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
end;


procedure pswap4r ( var s:single);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  inguy^.Word1 := outguy.Word1;
  inguy^.Word2 := outguy.Word2;
end; //proc pswap4r

procedure pswap4i(var s : LongInt);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:LongInt);
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
end; //proc swap4

function swap64r(s : double):double;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2,Word3,Word4 : word); //word is 16 bit
      1:(float:double);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word4);
  outguy.Word2 := swap(inguy^.Word3);
  outguy.Word3 := swap(inguy^.Word2);
  outguy.Word4 := swap(inguy^.Word1);
  try
    swap64r:=outguy.float;
  except
        swap64r := 0;
        exit;
  end;{}
end;

FUNCTION specialsingle (var s:single): boolean;
//returns true if s is Infinity, NAN or Indeterminate
//4byte IEEE: msb[31] = signbit, bits[23-30] exponent, bits[0..22] mantissa
//exponent of all 1s =   Infinity, NAN or Indeterminate
CONST kSpecialExponent = 255 shl 23;
VAR Overlay: LongInt ABSOLUTE s;
BEGIN
  IF ((Overlay AND kSpecialExponent) = kSpecialExponent) THEN
     RESULT := true
  ELSE
      RESULT := false;
END;

function isBioFormats(fnm: string): string;
//detect LIF and LIFF format or other Imagej/Fiji Bioformat
const
     LIF_MAGIC_BYTE = $70;
     LIF_MEMORY_BYTE = $2a;
var
  f: file;
  bs: array[0..255] of byte;
begin
  result := '';
  if not fileexists(fnm) then exit;
  if DirectoryExists(fnm) then exit;
  if FSize(fnm) < 256 then exit;
  {$I-}
  AssignFile(f, fnm);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(f, 1);
  {$I+}
  if ioresult <> 0 then
     exit;
  bs[0] :=  0; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
  BlockRead(f, bs, sizeof(bs)); //Byte-order Identifier
  if (bs[8] = LIF_MEMORY_BYTE) and ((bs[0] = LIF_MAGIC_BYTE) or (bs[3] = LIF_MAGIC_BYTE)) then
     result := 'LIF'; //file can be read using LIFReader.java
  if (bs[4] = ord('i')) and (bs[5] = ord('m')) and (bs[6] = ord('p')) and (bs[7] = ord('r')) then
     result := 'LIFF'; //Openlab LIFF format OpenLabReader.java
  if (bs[0] = $D0) and (bs[1] = $CF) and (bs[2] = $11) and (bs[3] = $E0) then //IPW_MAGIC_BYTES = 0xd0cf11e0
     result := 'IPW'; //IPWReader.java
  if (bs[0] = ord('i')) and (bs[1] = ord('i')) and (bs[2] = ord('i')) and (bs[3] = ord('i')) then
     result := 'IPL';//IPLabReader.java
  if (bs[0] = $89) and (bs[1] = $48) and (bs[2] = $44) and (bs[3] = $46) then //IPW_MAGIC_BYTES = 0xd0cf11e0
     result := 'HDF';//Various readers: ImarisHDFReader, CellH5Reader, etc
  if (bs[0] = $DA) and (bs[1] = $CE) and (bs[2] = $BE) and (bs[3] = $0A) then//DA CE BE 0A
     result := 'ND2';//MAGIC_BYTES_1 ND2Reader
  if (bs[0] = $6a) and (bs[1] = $50) and (bs[2] = $20) and (bs[3] = $20) then
     result := 'ND2';//MAGIC_BYTES_2 ND2Reader
  if (bs[208] = $4D) and (bs[209] = $41) and (bs[210] = $50) then
     result := 'MAP';//MRCReader http://www.ccpem.ac.uk/mrc_format/mrc2014.php
  //GatanReader.java
  closefile(f);
end;

  function isTIFF(fnm: string): boolean;
  var
    f: file;
    w: word;
  begin
    result := false;
    if not fileexists(fnm) then exit;
    if DirectoryExists(fnm) then exit;
    if FSize(fnm) < 32 then exit;
    {$I-}
    AssignFile(f, fnm);
    FileMode := fmOpenRead;  //Set file access to read only
    Reset(f, 1);
    {$I+}
    if ioresult <> 0 then
       exit;
    w := 0;
    BlockRead(f, w, sizeof(w)); //Byte-order Identifier
    if (w = $4D4D) or (w = $4949) then
       result := true;
    closefile(f);
  end;

{$IFDEF GUI}
procedure ShowMsg(s: string);
begin
     Showmessage(s);
end;
{$ENDIF}

procedure fromMatrix (m: mat44; out r11,r12,r13,r21,r22,r23,r31,r32,r33: double);
begin
  r11 := m[0,0];
  r12 := m[0,1];
  r13 := m[0,2];
  r21 := m[1,0];
  r22 := m[1,1];
  r23 := m[1,2];
  r31 := m[2,0];
  r32 := m[2,1];
  r33 := m[2,2];
end;

function Matrix2D (r11,r12,r13,r21,r22,r23,r31,r32,r33: double): mat33;
begin
  result[0,0] := r11;
  result[0,1] := r12;
  result[0,2] := r13;
  result[1,0] := r21;
  result[1,1] := r22;
  result[1,2] := r23;
  result[2,0] := r31;
  result[2,1] := r32;
  result[2,2] := r33;
end;

function nifti_mat33_determ( R: mat33 ):double;   //* determinant of 3x3 matrix */
begin
  result := r[0,0]*r[1,1]*r[2,2]
           -r[0,0]*r[2,1]*r[1,2]
           -r[1,0]*r[0,1]*r[2,2]
           +r[1,0]*r[2,1]*r[0,2]
           +r[2,0]*r[0,1]*r[1,2]
           -r[2,0]*r[1,1]*r[0,2] ;
end;

function nifti_mat33_rownorm( A: mat33 ): single;  // max row norm of 3x3 matrix
var
   r1,r2,r3: single ;
begin
   r1 := abs(A[0,0])+abs(A[0,1])+abs(A[0,2]);
   r2 := abs(A[1,0])+abs(A[1,1])+abs(A[1,2]);
   r3 := abs(A[2,0])+abs(A[2,1])+abs(A[2,2]);
   if( r1 < r2 ) then r1 := r2 ;
   if( r1 < r3 ) then r1 := r3 ;
   result := r1 ;
end;


procedure fromMatrix33 (m: mat33; out r11,r12,r13,r21,r22,r23,r31,r32,r33: double);
begin
  r11 := m[0,0];
  r12 := m[0,1];
  r13 := m[0,2];
  r21 := m[1,0];
  r22 := m[1,1];
  r23 := m[1,2];
  r31 := m[2,0];
  r32 := m[2,1];
  r33 := m[2,2];
end;


function nifti_mat33_inverse( R: mat33 ): mat33;   //* inverse of 3x3 matrix */
var
   r11,r12,r13,r21,r22,r23,r31,r32,r33 , deti: double ;
begin
   FromMatrix33(R,r11,r12,r13,r21,r22,r23,r31,r32,r33);
   deti := r11*r22*r33-r11*r32*r23-r21*r12*r33
         +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;
   if( deti <> 0.0 ) then deti := 1.0 / deti ;
   result[0,0] := deti*( r22*r33-r32*r23) ;
   result[0,1] := deti*(-r12*r33+r32*r13) ;
   result[0,2] := deti*( r12*r23-r22*r13) ;
   result[1,0] := deti*(-r21*r33+r31*r23) ;
   result[1,1] := deti*( r11*r33-r31*r13) ;
   result[1,2] := deti*(-r11*r23+r21*r13) ;
   result[2,0] := deti*( r21*r32-r31*r22) ;
   result[2,1] := deti*(-r11*r32+r31*r12) ;
   result[2,2] := deti*( r11*r22-r21*r12) ;
end;

function nifti_mat33_colnorm( A: mat33 ): single;  //* max column norm of 3x3 matrix */
var
   r1,r2,r3: single ;
begin
   r1 := abs(A[0,0])+abs(A[1,0])+abs(A[2,0]) ;
   r2 := abs(A[0,1])+abs(A[1,1])+abs(A[2,1]) ;
   r3 := abs(A[0,2])+abs(A[1,2])+abs(A[2,2]) ;
   if( r1 < r2 ) then r1 := r2 ;
   if( r1 < r3 ) then r1 := r3 ;
   result := r1 ;
end;

function nifti_mat33_polar( A: mat33 ): mat33;
var
   k:integer;
   X , Y , Z: mat33 ;
   dif,alp,bet,gam,gmi : single;
begin
  dif := 1;
  k := 0;
   X := A ;
   gam := nifti_mat33_determ(X) ;
   while( gam = 0.0 )do begin        //perturb matrix
     gam := 0.00001 * ( 0.001 + nifti_mat33_rownorm(X) ) ;
     X[0,0] := X[0,0]+gam ;
     X[1,1] := X[1,1]+gam ;
     X[2,2] := X[2,2] +gam ;
     gam := nifti_mat33_determ(X) ;
   end;
   while true do begin
     Y := nifti_mat33_inverse(X) ;
     if( dif > 0.3 )then begin     // far from convergence
       alp := sqrt( nifti_mat33_rownorm(X) * nifti_mat33_colnorm(X) ) ;
       bet := sqrt( nifti_mat33_rownorm(Y) * nifti_mat33_colnorm(Y) ) ;
       gam := sqrt( bet / alp ) ;
       gmi := 1.0 / gam ;
     end else begin
       gam := 1.0;
       gmi := 1.0 ;  //close to convergence
     end;
     Z[0,0] := 0.5 * ( gam*X[0,0] + gmi*Y[0,0] ) ;
     Z[0,1] := 0.5 * ( gam*X[0,1] + gmi*Y[1,0] ) ;
     Z[0,2] := 0.5 * ( gam*X[0,2] + gmi*Y[2,0] ) ;
     Z[1,0] := 0.5 * ( gam*X[1,0] + gmi*Y[0,1] ) ;
     Z[1,1] := 0.5 * ( gam*X[1,1] + gmi*Y[1,1] ) ;
     Z[1,2] := 0.5 * ( gam*X[1,2] + gmi*Y[2,1] ) ;
     Z[2,0] := 0.5 * ( gam*X[2,0] + gmi*Y[0,2] ) ;
     Z[2,1] := 0.5 * ( gam*X[2,1] + gmi*Y[1,2] ) ;
     Z[2,2] := 0.5 * ( gam*X[2,2] + gmi*Y[2,2] ) ;
     dif := abs(Z[0,0]-X[0,0])+abs(Z[0,1]-X[0,1])+abs(Z[0,2]-X[0,2])
           +abs(Z[1,0]-X[1,0])+abs(Z[1,1]-X[1,1])+abs(Z[1,2]-X[1,2])
           +abs(Z[2,0]-X[2,0])+abs(Z[2,1]-X[2,1])+abs(Z[2,2]-X[2,2]);
     k := k+1 ;
     if( k > 100) or (dif < 3.e-6 ) then begin
         result := Z;
         break ; //convergence or exhaustion
     end;
     X := Z ;
   end;
   result := Z ;
end;

procedure nifti_mat44_to_quatern( lR :mat44; out qb, qc, qd, qx, qy, qz, dx, dy, dz, qfac : single);
var
   r11,r12,r13 , r21,r22,r23 , r31,r32,r33, xd,yd,zd , a,b,c,d : double;
   P,Q: mat33;  //3x3
begin
   // offset outputs are read write out of input matrix
   qx := lR[0,3];
   qy := lR[1,3];
   qz := lR[2,3];
   //load 3x3 matrix into local variables
   fromMatrix(lR,r11,r12,r13,r21,r22,r23,r31,r32,r33);
   //compute lengths of each column; these determine grid spacings
   xd := sqrt( r11*r11 + r21*r21 + r31*r31 ) ;
   yd := sqrt( r12*r12 + r22*r22 + r32*r32 ) ;
   zd := sqrt( r13*r13 + r23*r23 + r33*r33 ) ;
   //if a column length is zero, patch the trouble
   if( xd = 0.0 )then begin r11 := 1.0 ; r21 := 0; r31 := 0.0 ; xd := 1.0 ; end;
   if( yd = 0.0 )then begin r22 := 1.0 ; r12 := 0; r32 := 0.0 ; yd := 1.0 ; end;
   if( zd = 0.0 )then begin r33 := 1.0 ; r13 := 0; r23 := 0.0 ; zd := 1.0 ; end;
   //assign the output lengths
   dx := xd;
   dy := yd;
   dz := zd;
   //normalize the columns
   r11 := r11/xd ; r21 := r21/xd ; r31 := r31/xd ;
   r12 := r12/yd ; r22 := r22/yd ; r32 := r32/yd ;
   r13 := r13/zd ; r23 := r23/zd ; r33 := r33/zd ;
   { At this point, the matrix has normal columns, but we have to allow
      for the fact that the hideous user may not have given us a matrix
      with orthogonal columns. So, now find the orthogonal matrix closest
      to the current matrix.
      One reason for using the polar decomposition to get this
      orthogonal matrix, rather than just directly orthogonalizing
      the columns, is so that inputting the inverse matrix to R
      will result in the inverse orthogonal matrix at this point.
      If we just orthogonalized the columns, this wouldn't necessarily hold.}
   Q :=  Matrix2D (r11,r12,r13,          // 2D "graphics" matrix
                           r21,r22,r23,
                           r31,r32,r33);
   P := nifti_mat33_polar(Q) ; //P is orthog matrix closest to Q
   FromMatrix33(P,r11,r12,r13,r21,r22,r23,r31,r32,r33);
{                           [ r11 r12 r13 ]
 at this point, the matrix  [ r21 r22 r23 ] is orthogonal
                            [ r31 r32 r33 ]
 compute the determinant to determine if it is proper}

   zd := r11*r22*r33-r11*r32*r23-r21*r12*r33
       +r21*r32*r13+r31*r12*r23-r31*r22*r13 ; //should be -1 or 1

   if( zd > 0 )then begin // proper
     qfac  := 1.0 ;
   end else begin //improper ==> flip 3rd column
     qfac := -1.0 ;
     r13 := -r13 ; r23 := -r23 ; r33 := -r33 ;
   end;
   // now, compute quaternion parameters
   a := r11 + r22 + r33 + 1.0;
   if( a > 0.5 ) then begin  //simplest case
     a := 0.5 * sqrt(a) ;
     b := 0.25 * (r32-r23) / a ;
     c := 0.25 * (r13-r31) / a ;
     d := 0.25 * (r21-r12) / a ;
   end else begin  //trickier case
     xd := 1.0 + r11 - (r22+r33) ;// 4*b*b
     yd := 1.0 + r22 - (r11+r33) ;// 4*c*c
     zd := 1.0 + r33 - (r11+r22) ;// 4*d*d
     if( xd > 1.0 ) then begin
       b := 0.5 * sqrt(xd) ;
       c := 0.25* (r12+r21) / b ;
       d := 0.25* (r13+r31) / b ;
       a := 0.25* (r32-r23) / b ;
     end else if( yd > 1.0 ) then begin
       c := 0.5 * sqrt(yd) ;
       b := 0.25* (r12+r21) / c ;
       d := 0.25* (r23+r32) / c ;
       a := 0.25* (r13-r31) / c ;
     end else begin
       d := 0.5 * sqrt(zd) ;
       b := 0.25* (r13+r31) / d ;
       c := 0.25* (r23+r32) / d ;
       a := 0.25* (r21-r12) / d ;
     end;
     if( a < 0.0 )then begin b:=-b ; c:=-c ; d:=-d; {a:=-a; this is not used} end;
   end;
   qb := b ;
   qc := c ;
   qd := d ;
end;

procedure ZERO_MAT33(out m: mat33); //note sets m[3,3] to one
var
  i,j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      m[i,j] := 0.0;
  m[2,2] := 1;
end;

procedure ZERO_MAT44(out m: mat44); //note sets m[3,3] to one
var
  i,j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      m[i,j] := 0.0;
  m[3,3] := 1;
end;

procedure LOAD_MAT33(out m: mat33; m00,m01,m02, m10,m11,m12, m20,m21,m22: single);
begin
  m[0,0] := m00;
  m[0,1] := m01;
  m[0,2] := m02;
  m[1,0] := m10;
  m[1,1] := m11;
  m[1,2] := m12;
  m[2,0] := m20;
  m[2,1] := m21;
  m[2,2] := m22;
end;

function nifti_mat33vec_mul(m: mat33; v: vect3): vect3;
var
  i: integer;
begin
     for i := 0 to 2 do
         result[i] := (v[0]*m[i,0])+(v[1]*m[i,1])+(v[2]*m[i,2]);
end;

function nifti_mat33_mul( A,B: mat33): mat33;
var
  i,j: integer;
begin
     for i:=0 to 2 do
    	for j:=0 to 2 do
            result[i,j] :=  A[i,0] * B[0,j]
            + A[i,1] * B[1,j]
            + A[i,2] * B[2,j] ;
end;

procedure LOAD_MAT44(out m: mat44; m00,m01,m02,m03, m10,m11,m12,m13, m20,m21,m22,m23: single);
begin
  m[0,0] := m00;
  m[0,1] := m01;
  m[0,2] := m02;
  m[0,3] := m03;
  m[1,0] := m10;
  m[1,1] := m11;
  m[1,2] := m12;
  m[1,3] := m13;
  m[2,0] := m20;
  m[2,1] := m21;
  m[2,2] := m22;
  m[2,3] := m23;
  m[3,0] := 0.0;
  m[3,1] := 0.0;
  m[3,2] := 0.0;
  m[3,3] := 1.0;
end;

function validMatrix(var m: mat44): boolean;
var
  i: integer;
begin
     result := false;
     for i := 0 to 2 do begin
         if (m[0,i] = 0.0) and (m[1,i] = 0.0) and (m[2,i] = 0.0) then exit;
         if (m[i,0] = 0.0) and (m[i,1] = 0.0) and (m[i,2] = 0.0) then exit;
     end;
     result := true;
end;

procedure  vCoord(var lV: vect3; lMat: mat33);
//transform X Y Z by matrix
var
  lXi,lYi,lZi: single;
begin
  lXi := lV[0]; lYi := lV[1]; lZi := lV[2];
  lV[0] := (lXi*lMat[0,0]+lYi*lMat[0,1]+lZi*lMat[0,2]);
  lV[1] := (lXi*lMat[1,0]+lYi*lMat[1,1]+lZi*lMat[1,2]);
  lV[2] := (lXi*lMat[2,0]+lYi*lMat[2,1]+lZi*lMat[2,2]);
end;

procedure SetCenter(var nhdr: TNIFTIhdr);
var
   v: vect3;
   m: mat33;
   i: integer;
begin
  for i := 0 to 2 do begin
      m[0,i] := nhdr.srow_x[i];
      m[1,i] := nhdr.srow_y[i];
      m[2,i] := nhdr.srow_z[i];
      v[i] := nhdr.dim[i+1]*0.5;
  end;
  vCoord(v,m);
  //Voxel2mm
  //nhdr.srow_x[3]:=-0.5 * (d[0]*m[0,0]+ d[1]*m[0,1]+d[2]*m[0,2]);
  nhdr.srow_x[3] := -v[0];
  nhdr.srow_y[3] := -v[1];
  nhdr.srow_z[3] := -v[2];
end;

procedure SetSForm(var nhdr: TNIFTIhdr);
begin
  nhdr.srow_x[0]:=nhdr.pixdim[1]; nhdr.srow_x[1]:=0; nhdr.srow_x[2]:=0; nhdr.srow_x[3]:=-(nhdr.dim[1]-2.0)/2.0*nhdr.pixdim[1];
  nhdr.srow_y[0]:=0; nhdr.srow_y[1]:=nhdr.pixdim[2]; nhdr.srow_y[2]:=0; nhdr.srow_y[3]:=-(nhdr.dim[2]-2.0)/2.0*nhdr.pixdim[2];
  nhdr.srow_z[0]:=0; nhdr.srow_z[1]:=0; nhdr.srow_z[2]:=nhdr.pixdim[3]; nhdr.srow_z[3]:=-(nhdr.dim[3]-2.0)/2.0*nhdr.pixdim[3];
end;

procedure convertForeignToNifti(var nhdr: TNIFTIhdr);
var
  i,nonSpatialMult: integer;
  qto_xyz: mat44;
  dumdx, dumdy, dumdz: single;
begin
  nhdr.HdrSz := 348; //used to signify header does not need to be byte-swapped
	nhdr.magic:=kNIFTI_MAGIC_EMBEDDED_HDR;
	if (nhdr.dim[3] = 0) then nhdr.dim[3] := 1; //for 2D images the 3rd dim is not specified and set to zero
	nhdr.dim[0] := 3; //for 2D images the 3rd dim is not specified and set to zero
  nonSpatialMult := 1;
  for i := 4 to 7 do
    if nhdr.dim[i] > 0 then
      nonSpatialMult := nonSpatialMult * nhdr.dim[i];
	if (nonSpatialMult > 1) then begin
    nhdr.dim[0] := 4;
    nhdr.dim[4] := nonSpatialMult;
    for i := 5 to 7 do
      nhdr.dim[i] := 0;
  end;
  nhdr.bitpix := 8;
  if (nhdr.scl_slope = 0.0) then
    nhdr.scl_slope := 1.0;
  if (nhdr.datatype = 4) or (nhdr.datatype = 512) then nhdr.bitpix := 16;
  if (nhdr.datatype = kDT_RGB) or (nhdr.datatype = kDT_RGBplanar3D) then nhdr.bitpix := 24;
  if (nhdr.datatype = 8) or (nhdr.datatype = 16) or (nhdr.datatype = 768) then nhdr.bitpix := 32;
  if (nhdr.datatype = 32) or (nhdr.datatype = 64) or (nhdr.datatype = 1024) or (nhdr.datatype = 1280) then nhdr.bitpix := 64;
  LOAD_MAT44(qto_xyz, nhdr.srow_x[0], nhdr.srow_x[1], nhdr.srow_x[2], nhdr.srow_x[3],
              nhdr.srow_y[0], nhdr.srow_y[1], nhdr.srow_y[2], nhdr.srow_y[3],
              nhdr.srow_z[0], nhdr.srow_z[1], nhdr.srow_z[2], nhdr.srow_z[3]);
  if not validMatrix(qto_xyz) then begin
     nhdr.sform_code := 0;
     nhdr.qform_code :=  0;
     for i := 0 to 3 do begin
         nhdr.srow_x[i] := 0;
         nhdr.srow_y[i] := 0;
         nhdr.srow_z[i] := 0;
     end;
     nhdr.srow_x[0] := 1;
     nhdr.srow_y[1] := 1;
     nhdr.srow_z[2] := 1;
     exit;
  end;
  nhdr.sform_code := 1;
  nifti_mat44_to_quatern( qto_xyz , nhdr.quatern_b, nhdr.quatern_c, nhdr.quatern_d,nhdr.qoffset_x,nhdr.qoffset_y,nhdr.qoffset_z, dumdx, dumdy, dumdz,nhdr.pixdim[0]) ;
  nhdr.qform_code := 0;//kNIFTI_XFORM_SCANNER_ANAT;
end;

procedure NSLog( str: string);
begin
  {$IFDEF GUI}
  showmsg(str);
  {$ENDIF}
  {$IFDEF UNIX}writeln(str);{$ENDIF}
end;

function parsePicString(s: string): single;
//given "AXIS_4 001 0.000000e+00 4.000000e-01 microns"  return 0.4
var
  sList : TStringList;
begin
  result := 0.0;
  {$IFDEF FPC}
  DefaultFormatSettings.DecimalSeparator := '.' ;
  {$ELSE}
  DecimalSeparator := '.';
  {$ENDIF}
  sList := TStringList.Create;
  sList.Delimiter := ' ';        // Each list item will be blank separated
  sList.DelimitedText := s;
  if sList.Count > 4 then begin
     //ShowMessage(sList[3]);
     try
        result := StrToFloat(sList[3]);    // Middle blanks are not supported
     except
           //ShowMessage(Exception.Message);
     end;
  end;
  sList.Free;
end;

function readVmr (var fname: string; isV16: boolean; var nhdr: TNIFTIhdr; var swapEndian: boolean): boolean;
//http://support.brainvoyager.com/automation-aamp-development/23-file-formats/385-developer-guide-26-the-format-of-vmr-files.html
Type
  Tvmr_header = packed record //Next: VMR Format Header structure
        ver, nx, ny, nz: word; //  0,4,8,12
  end; // Tbv_header;
  (*Tvmr_tail = packed record //
  	Xoff,Yoff,Zoff,FramingCube: int16; //v3
	PosFlag,CoordSystem: int32;
	X1, Y1, Z1,Xn,Yn,Zn, RXv,RYv,RZv, CXv,CYv,CZv: single;
	nRmat, nCmat: int32;
	Rfov, Cfov, Zthick, Zgap: single;
	nTrans: int32;
	LRconv: uint8;
	vXres, vYres, vZres: single;
	isResVerified, isTal: uint8;
	min, mean, max: int32;
  end; *)
var
   vhdr : Tvmr_header;
   //vtail : Tvmr_tail;
   lHdrFile: file;
   xSz, nvox, FSz, Hsz : integer;
begin
  result := false;
  {$I-}
  AssignFile(lHdrFile, fname);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(lHdrFile, 1);
  {$I+}
  if ioresult <> 0 then begin
        NSLog('Error in reading vmr header.'+inttostr(IOResult));
        FileMode := 2;
        exit;
  end;
  FSz := Filesize(lHdrFile);
  vhdr.nx :=  0; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
  BlockRead(lHdrFile, vhdr, sizeof(Tvmr_header));
  nVox := vhdr.nx * vhdr.ny * vhdr.nz;
  if isV16 then
    xSz := (2 * nVox) + sizeof(Tvmr_header)
  else
    xSz := nVox + sizeof(Tvmr_header);//+ sizeof(Tvmr_tail);
  Hsz := sizeof(Tvmr_header);
  if (xSz > FSz) then begin //version 1? (6 byte header)
     nVox := vhdr.ver * vhdr.nx * vhdr.ny;
     if isV16 then
        xSz := (2 * nVox) + 6
     else
         xSz := nVox + 6;
     if (xSz = FSz) then begin //version 1
        vhdr.nz := vhdr.ny;
        vhdr.ny := vhdr.nx;
        vhdr.nx := vhdr.ver;
        vhdr.ver := 1;
        Hsz := 6;
     end;
  end;
  if (xSz > FSz) then begin //docs do not specify endian - wrong endian?
     NSLog(format('Odd v16 or vmr format image %dx%dx%d ver %d sz %d', [vhdr.nx, vhdr.ny, vhdr.nz, vhdr.ver, FSz] ));
     CloseFile(lHdrFile);
     exit;
  end;
  //seek(lHdrFile, nVox + sizeof(Tvmr_header));
  //BlockRead(lHdrFile, vtail, sizeof(Tvmr_tail));
  CloseFile(lHdrFile);
  swapEndian := false;
  nhdr.dim[0]:=3;//3D
  nhdr.dim[1]:=vhdr.nx;
  nhdr.dim[2]:=vhdr.ny;
  nhdr.dim[3]:=vhdr.nz;
  nhdr.dim[4]:=1;
  nhdr.pixdim[1]:=1.0;
  nhdr.pixdim[2]:=1.0;
  nhdr.pixdim[3]:=1.0;
  //Need examples
  //if vtail.isResVerified > 0 then begin
  //  showmessage(format('%g %g %g',[vtail.X1, vtail.Y1, vtail.Z1]));
  //end;
  nhdr.bitpix:= 8;
  nhdr.datatype := kDT_UNSIGNED_CHAR;
  if isV16 then begin
     nhdr.bitpix:= 16;
     nhdr.datatype := kDT_INT16;
  end;
  nhdr.vox_offset := HSz;
  nhdr.sform_code := 1;
  (*nhdr.srow_x[0]:=nhdr.pixdim[1];nhdr.srow_x[1]:=0.0;nhdr.srow_x[2]:=0.0;nhdr.srow_x[3]:=0.0;
  nhdr.srow_y[0]:=0.0;nhdr.srow_y[1]:=nhdr.pixdim[2];nhdr.srow_y[2]:=0.0;nhdr.srow_y[3]:=0.0;
  nhdr.srow_z[0]:=0.0;nhdr.srow_z[1]:=0.0;nhdr.srow_z[2]:=-nhdr.pixdim[3];nhdr.srow_z[3]:=0.0;*)
  SetSForm(nhdr);
  convertForeignToNifti(nhdr);
  nhdr.descrip := 'VMR'+kIVers;
  result := true;
end; //readVmr()

function readV3draw(var fname: string; var nhdr: TNIFTIhdr; var swapEndian: boolean): boolean;
//https://github.com/Vaa3D
// https://github.com/fiji/Vaa3d_Reader/blob/master/src/main/java/org/janelia/vaa3d/reader/Vaa3d_Reader.java
Type
  Tdf_header = packed record
        variant: array [1..24] of char;
        endian: char;
        dataTypeSize: int16;
        nx, ny, nz, nc: uint32;
  end; // Tbv_header;
var
   dhdr : Tdf_header;
   lHdrFile: file;
   FSz : integer;
   nBytes : Int64;
begin
  result := false;
  {$I-}
  AssignFile(lHdrFile, fname);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(lHdrFile, 1);
  {$I+}
  if ioresult <> 0 then begin
        NSLog('Error in reading v3draw header.'+inttostr(IOResult));
        FileMode := 2;
        exit;
  end;
  FSz := Filesize(lHdrFile);
  dhdr.nx :=  0; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
  BlockRead(lHdrFile, dhdr, sizeof(Tdf_header));
  CloseFile(lHdrFile);
  if (dhdr.endian <> 'B') and (dhdr.endian <> 'L') then begin
     NSLog('Unknown v3draw endian');
     exit;
  end;
  {$IFDEF ENDIAN_LITTLE}
  if (dhdr.endian = 'B') then swapEndian := true;
  {$ELSE}
  if (dhdr.endian = 'L') then swapEndian := true;
  {$ENDIF}
  if swapEndian then begin
     pswap4i(int32(dhdr.nx));
     pswap4i(int32(dhdr.ny));
     pswap4i(int32(dhdr.nz));
     pswap4i(int32(dhdr.nc));
     dhdr.dataTypeSize := swap(dhdr.dataTypeSize);
  end;
  if (dhdr.dataTypeSize <> 1) and (dhdr.dataTypeSize <> 2) and (dhdr.dataTypeSize <> 4) then begin
     NSLog('Unknown v3draw datatype');
     exit;
  end;
  if (dhdr.variant[1] <> 'r') or (dhdr.variant[2] <> 'a') or (dhdr.variant[3] <> 'w') then begin
     NSLog('Unsupported v3draw compression');
     exit;
  end;
  nBytes := dhdr.dataTypeSize * dhdr.nx * dhdr.ny * dhdr.nz * dhdr.nc; //assume 8-bit
  if (nBytes + sizeof(Tdf_header) ) <> FSz then begin
     NSLog(format('Not a valid v3draw file: %d bytes not expected filesize of %d bytes (%dx%dx%d)',[FSz, nBytes+sizeof(Tdf_header), dhdr.nx, dhdr.ny, dhdr.nz]));
     exit;
  end;
  nhdr.dim[0]:=3;//3D
  nhdr.dim[1]:= dhdr.nx;
  nhdr.dim[2]:= dhdr.ny;
  nhdr.dim[3]:= dhdr.nz;
  nhdr.pixdim[1]:=1.0;
  nhdr.pixdim[2]:=1.0;
  nhdr.pixdim[3]:=1.0;
  nhdr.datatype := kDT_UINT8;
  nhdr.bitpix := dhdr.dataTypeSize * 8;
  if (dhdr.dataTypeSize = 1) and (dhdr.nc = 3) then begin
     nhdr.datatype := kDT_RGBplanar3D;
     nhdr.bitpix := 24;
     dhdr.nc := 1;
  end;

  if (dhdr.nc > 1) then begin
     nhdr.dim[0]:=4;//4D
     nhdr.dim[4]:= dhdr.nc;
  end;
  if dhdr.dataTypeSize = 2 then
     nhdr.datatype := kDT_UINT16;
  if dhdr.dataTypeSize = 4 then
     nhdr.datatype := kDT_FLOAT;
  nhdr.vox_offset := sizeof(Tdf_header);
  //showmessage(format('%d %d %d  -> %d',[nhdr.dim[1],nhdr.dim[2],nhdr.dim[3],  nhdr.bitpix]));
  nhdr.sform_code := 1;
  SetSForm(nhdr);
  convertForeignToNifti(nhdr);
  nhdr.descrip := 'V3draw'+kIVers;
  result := true;
end; //readV3draw

function readDf3 (var fname: string; var nhdr: TNIFTIhdr; var swapEndian: boolean): boolean;
//https://www.povray.org/documentation/view/3.6.1/374/
//The df3 format consists of a 6 byte header of three 16-bit integers (big endian)
//The header is followed by x*y*z unsigned integer bytes of data with a resolution of 8, 16 or 32 bit.
// The data are written with high order byte first (big-endian)
Type
  Tdf_header = packed record //Next: PIC Format Header structure
        nx, ny, nz: word;
  end; // Tbv_header;
var
   dhdr : Tdf_header;
   lHdrFile: file;
   nvox, FSz : integer;
begin
  result := false;
  {$I-}
  AssignFile(lHdrFile, fname);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(lHdrFile, 1);
  {$I+}
  if ioresult <> 0 then begin
        NSLog('Error in reading DF3 header.'+inttostr(IOResult));
        FileMode := 2;
        exit;
  end;
  FSz := Filesize(lHdrFile);
  dhdr.nx :=  0; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
  BlockRead(lHdrFile, dhdr, sizeof(Tdf_header));
  CloseFile(lHdrFile);
  swapEndian := false;
  {$IFDEF ENDIAN_LITTLE}
  swapEndian := true;
  dhdr.nx := swap(dhdr.nx);
  dhdr.ny := swap(dhdr.ny);
  dhdr.nz := swap(dhdr.nz);
  {$ENDIF}
  nVox := dhdr.nx * dhdr.ny * dhdr.nz; //assume 8-bit
  if (nVox + sizeof(Tdf_header) ) > FSz then begin
     NSLog(format('Not a valid DF3 file: %d bytes not expected filesize of %d bytes (%dx%dx%d)',[FSz, nVox+sizeof(Tdf_header), dhdr.nx, dhdr.ny, dhdr.nz]));
     exit;
  end;
  nhdr.dim[0]:=3;//3D
  nhdr.dim[1]:= dhdr.nx;
  nhdr.dim[2]:= dhdr.ny;
  nhdr.dim[3]:= dhdr.nz;
  nhdr.pixdim[1]:=1.0;
  nhdr.pixdim[2]:=1.0;
  nhdr.pixdim[3]:=1.0;
  nhdr.datatype := kDT_UINT8;
  if ((nVox*2) + sizeof(Tdf_header) ) <= FSz then
     nhdr.datatype := kDT_UINT16;
  if ((nVox*4) + sizeof(Tdf_header) ) <= FSz then
     nhdr.datatype := kDT_UINT32;
  nhdr.vox_offset := sizeof(Tdf_header);
  nhdr.sform_code := 1;
  (*nhdr.srow_x[0]:=nhdr.pixdim[1];nhdr.srow_x[1]:=0.0;nhdr.srow_x[2]:=0.0;nhdr.srow_x[3]:=0.0;
  nhdr.srow_y[0]:=0.0;nhdr.srow_y[1]:=nhdr.pixdim[2];nhdr.srow_y[2]:=0.0;nhdr.srow_y[3]:=0.0;
  nhdr.srow_z[0]:=0.0;nhdr.srow_z[1]:=0.0;nhdr.srow_z[2]:=-nhdr.pixdim[3];nhdr.srow_z[3]:=0.0; *)
  SetSForm(nhdr);
  convertForeignToNifti(nhdr);
  nhdr.descrip := 'DF3'+kIVers;
  result := true;
end; //readDf3

function readXVF (var fname: string; var nhdr: TNIFTIhdr): boolean;
//http://ivl.calit2.net/wiki/index.php/VOX_and_Virvo
//http://web.eng.ucsd.edu/~jschulze/projects/vox/release/deskvox2_00b.txt
//https://github.com/deskvox/deskvox/blob/f43bc19ecd82c2c2bcc5d7dfb7e65aa9ae99f57d/virvo/virvo/vvvoldesc.h
Type
  Tbv_header = packed record
        id: array [1..9] of ansichar;
        offset : uint16; //  0,4,8,12
        dim: array [1..4] of uint32;
        datatype: uint8;
        pixdim: array [1..4] of single;
        mn,mx: single;
        origin: array [1..3] of single;
        storagetype, compressiontype: uint8;
        transferfunctions, transfertype, iconsize : uint16;
  end; // Tbv_header;
var
   bhdr : Tbv_header;
   lHdrFile: file;
   i, nvox, FSz : integer;
begin
  result := false;
  {$I-}
  AssignFile(lHdrFile, fname);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(lHdrFile, 1);
  {$I+}
  if ioresult <> 0 then begin
        NSLog('Error in reading RVF header.'+inttostr(IOResult));
        FileMode := 2;
        exit;
  end;
  FSz := Filesize(lHdrFile);
  bhdr.datatype :=  0; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
  BlockRead(lHdrFile, bhdr, sizeof(Tbv_header));
  CloseFile(lHdrFile);
  //VIRVO-XVF
  if (bhdr.id[1]<>'V') or (bhdr.id[2]<>'I') or (bhdr.id[3]<>'R') or (bhdr.id[4]<>'V') or (bhdr.id[5]<>'O')
      or (bhdr.id[6]<>'-') or (bhdr.id[7]<>'X')  or (bhdr.id[8]<>'V')  or (bhdr.id[9]<>'F') then
     exit;
  (*if bHdr.compressiontype <> 0 then begin
    //supposed to be 0 or 1, but default-animation is 17?
    NSLog('XVF RLE compression not supported.'+inttostr(bHdr.compressiontype));
    exit;
  end;*)
  {$IFDEF ENDIAN_LITTLE}
  bhdr.offset := swap(bhdr.offset);
  for i := 1 to 4 do
    pswap4i(int32(bhdr.dim[i]));
  for i := 1 to 4 do
      pswap4i(int32(bhdr.pixdim[i]));
  pswap4i(int32(bhdr.mn));
  pswap4i(int32(bhdr.mx));
  for i := 1 to 3 do
      pswap4i(int32(bhdr.origin[i]));
  bhdr.transferfunctions := swap(bhdr.transferfunctions);
  bhdr.transfertype := swap(bhdr.transfertype);
  bhdr.iconsize := swap(bhdr.iconsize);
  {$ENDIF}
  //NSLog(format('%d %d', [bhdr.datatype, bhdr.storagetype])); //animation:8:16 rgbCube 24:0 torso 8:0
  for i := 1 to 4 do
      nhdr.dim[i]:=bhdr.dim[i];
  for i := 1 to 4 do
      nhdr.pixdim[i]:=bhdr.pixdim[i];
  nhdr.dim[0]:=3;//3D
  if nhdr.dim[4] > 1 then
     nhdr.dim[0]:=4;//4D
  if bhdr.datatype = 24 then
     nhdr.datatype := kDT_RGB
  else if bhdr.datatype = 8 then
       nhdr.datatype := kDT_UINT8
  else begin
       NSLog(format('Unknown XVF datatype : %d', [bhdr.datatype]));
       exit;
  end;
  nhdr.vox_offset := bhdr.offset;

  nVox := nhdr.dim[1] * nhdr.dim[2] * nhdr.dim[3] * nhdr.dim[4];
  if (nVox + bhdr.offset ) > FSz then begin
       NSLog(format('Not a valid XVF file: expected filesize of %d bytes (%dx%dx%dx%d+%d)',[nVox + bhdr.offset, bhdr.dim[1], bhdr.dim[2], bhdr.dim[3], bhdr.dim[4], bhdr.offset]));
       exit;
  end;
  convertForeignToNifti(nhdr);
  SetSForm(nhdr);
  nhdr.descrip := 'XVF'+kIVers;
  result := true;
end; //readXVF

function readRVF (var fname: string; var nhdr: TNIFTIhdr): boolean;
//http://ivl.calit2.net/wiki/index.php/VOX_and_Virvo
//http://web.eng.ucsd.edu/~jschulze/projects/vox/release/deskvox2_00b.txt
Type
  Tbv_header = packed record
        nx, ny, nz : word; //  0,4,8,12
  end; // Tbv_header;
var
   bhdr : Tbv_header;
   lHdrFile: file;
   nvox, FSz : integer;
begin
  result := false;
  {$I-}
  AssignFile(lHdrFile, fname);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(lHdrFile, 1);
  {$I+}
  if ioresult <> 0 then begin
        NSLog('Error in reading RVF header.'+inttostr(IOResult));
        FileMode := 2;
        exit;
  end;
  FSz := Filesize(lHdrFile);
  bhdr.nx :=  0; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
  BlockRead(lHdrFile, bhdr, sizeof(Tbv_header));
  CloseFile(lHdrFile);
  {$IFDEF ENDIAN_LITTLE}
  bhdr.nx := swap(bhdr.nx);
  bhdr.ny := swap(bhdr.ny);
  bhdr.nz := swap(bhdr.nz);
  {$ENDIF}
  nVox := bhdr.nx * bhdr.ny * bhdr.nz; //*4 as 32-bpp
  if (nVox + sizeof(Tbv_header) ) <> FSz then begin
       NSLog(format('Not a valid RVF file: expected filesize of %d bytes (%dx%dx%d+%d)',[nVox + sizeof(Tbv_header), bhdr.nx, bhdr.ny, bhdr.nz, sizeof(Tbv_header)]));
       exit;
  end;
  nhdr.dim[0]:=3;//3D
  nhdr.dim[1]:=bhdr.nx;
  nhdr.dim[2]:=bhdr.ny;
  nhdr.dim[3]:=bhdr.nz;
  nhdr.pixdim[1]:=1.0;
  nhdr.pixdim[2]:=1.0;
  nhdr.pixdim[3]:=1.0;
  nhdr.datatype := kDT_UINT8;
  nhdr.vox_offset := sizeof(Tbv_header);
  nhdr.sform_code := 1;
  SetSForm(nhdr);
  convertForeignToNifti(nhdr);
  nhdr.descrip := 'RVF'+kIVers;
  result := true;
end; //readRVF


function readXRaw (var fname: string; var nhdr: TNIFTIhdr; var swapEndian: boolean): boolean;
//https://twitter.com/ephtracy/status/653721698328551424
Type
  Tbv_header = packed record //Next: XRaw Format Header structure
        magic: UINT32; //'XRAW'
        datatype: UINT8; // 0=uint, 1=int, 2=float
        channels: UINT8; //4=RGBA, 3=RGB, 2=RB, 1=R
        bits: UINT8; //8, 16, 32
        bitsPerIndex: UINT8; //0=scalar, 8=256, 16=32768
        nx, ny, nz: UINT32; //
        nPal: UINT32; //0, 256, or 32768
  end; // Tbv_header;
var
   bhdr : Tbv_header;
   lHdrFile: file;
   nvox, FSz : integer;
begin
  result := false;
  {$I-}
  AssignFile(lHdrFile, fname);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(lHdrFile, 1);
  {$I+}
  if ioresult <> 0 then begin
        NSLog('Error in reading XRaw header.'+inttostr(IOResult));
        FileMode := 2;
        exit;
  end;
  FSz := Filesize(lHdrFile);
  bhdr.nx :=  0; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
  BlockRead(lHdrFile, bhdr, sizeof(Tbv_header));
  CloseFile(lHdrFile);
  swapEndian := false;
  {$IFDEF ENDIAN_BIG}
  swapEndian := true;
  pswap4i(longint(bhdr.magic));
  pswap4i(longint(bhdr.nx));
  pswap4i(longint(bhdr.ny));
  pswap4i(longint(bhdr.nz));
  pswap4i(longint(bhdr.nPal));
  {$ELSE}
  swapEndian := false;
  {$ENDIF}
  if bhdr.magic <> 1463898712 then begin
    NSLog('Not a valid XRaw file: should begin "XRAW"');
    exit;
  end;
  nVox := bhdr.nx * bhdr.ny * bhdr.nz * 1;
  if (nVox + sizeof(Tbv_header) ) > FSz then begin
    NSLog(format('Not a valid XRaw file: filesize too small (%dx%dx%d voxels)',[bhdr.nx, bhdr.ny, bhdr.nz]));
    exit;
  end;
  nhdr.dim[0]:=3;//3D
  nhdr.dim[1]:=bhdr.nx;
  nhdr.dim[2]:=bhdr.ny;
  nhdr.dim[3]:=bhdr.nz;
  //datatype: UINT8; // 0=uint, 1=int, 2=float
  //channels: UINT8; //4=RGBA, 3=RGB, 2=RB, 1=R
  //bits: UINT8; //8, 16, 32
  //bitsPerIndex: UINT8; //0=scalar, 8=256, 16=32768
  nhdr.datatype := 0; //unsupported
  if bhdr.datatype = 0 then begin //0=uint
     if bhdr.bits = 8 then
       nhdr.datatype := kDT_UINT8;
     if bhdr.bits = 16 then
       nhdr.datatype := kDT_UINT16;
     if bhdr.bits = 32 then
       nhdr.datatype := kDT_UINT32;
  end;
  if bhdr.datatype = 1 then begin //1=int
     if bhdr.bits = 8 then
       nhdr.datatype := kDT_INT8;
     if bhdr.bits = 16 then
       nhdr.datatype := kDT_INT16;
     if bhdr.bits = 32 then
       nhdr.datatype := kDT_INT32;
  end;
  if (bhdr.datatype = 2) and (bhdr.bits = 32) then //1=int
     nhdr.datatype := kDT_FLOAT32;
  if (bhdr.bitsPerIndex = 0) and (bhdr.channels > 1) then begin //scalar not palette
     if (bhdr.bits = 8) and (bhdr.channels = 3) then
       nhdr.datatype := kDT_RGB
     else if (bhdr.bits = 8) and (bhdr.channels = 4) then
         nhdr.datatype := kDT_RGBA32
     else
         nhdr.datatype := 0; //unsupported
  end;
  if (nhdr.datatype = 0) then begin
    NSLog(format('Unsupported XRaw file type. datatype: %d channels %d bits %d bitsPerIndex %d',[bhdr.datatype, bhdr.channels, bhdr.bits, bhdr.bitsPerIndex]));
    exit;
  end;
  nhdr.vox_offset := sizeof(Tbv_header);
  SetSForm(nhdr);
  convertForeignToNifti(nhdr);
  nhdr.descrip := 'XRaw'+kIVers;
  result := true;
end; //readXRaw

function readVox (var fname: string; var nhdr: TNIFTIhdr): boolean;
//slab6 vox format
Type
  Tbv_header = packed record //Next: Vox Format Header structure
        nz, ny, nx : LongInt;
  end; // Tbv_header;
var
   bhdr : Tbv_header;
   lHdrFile: file;
   Sz, FSz : integer;
begin
  result := false;
  {$I-}
  AssignFile(lHdrFile, fname);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(lHdrFile, 1);
  {$I+}
  if ioresult <> 0 then begin
        NSLog('Error in reading BVox header.'+inttostr(IOResult));
        FileMode := 2;
        exit;
  end;
  FSz := Filesize(lHdrFile);
  bhdr.nx :=  0; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
  BlockRead(lHdrFile, bhdr, sizeof(Tbv_header));
  CloseFile(lHdrFile);
  {$IFDEF ENDIAN_BIG}
  pswap4i(longint(bhdr.nx));
  pswap4i(longint(bhdr.ny));
  pswap4i(longint(bhdr.nz));
  {$ENDIF}
  Sz := bhdr.nx * bhdr.ny * bhdr.nz + sizeof(Tbv_header);
  if (Sz <> FSz) and ((768 + Sz ) <> FSz) then begin //768 byte RGB palette as footer
     //there are so many other formats that use the extension .vox, and Slab6 has no magic signature, so do not report a misleading error
     //NSLog(format('Not a valid Slab6 Vox file (%dx%dx%d) expected %d or %d bytes.',[bhdr.nx, bhdr.ny, bhdr.nz, Sz, Sz+768]));
     exit;
  end;
  nhdr.dim[0]:=3;//3D
  nhdr.dim[1]:=bhdr.nx;
  nhdr.dim[2]:=bhdr.ny;
  nhdr.dim[3]:=bhdr.nz;
  nhdr.datatype := kDT_UINT8;
  nhdr.vox_offset := sizeof(Tbv_header);
  SetSForm(nhdr);
  convertForeignToNifti(nhdr);
  nhdr.descrip := 'Slab6VOX'+kIVers;
  result := true;
end; //readVox

(* //SPARSE STORAGE
function readVox (var fname: string; var nhdr: TNIFTIhdr): boolean;
//https://github.com/DGtal-team/DGtalTools/blob/master/converters/vol2vox.cpp
Type
  Thdr = packed record //signature always at start of file
        magic, version: UINT32;
  end;
  TChunk = packed record
        id, szBytes, nChild: UINT32;
  end;

var
   h : Thdr;
   chunk: Tchunk;
   f: file;
   FSz, ChunkEnd : integer;
begin
  result := false;
  {$I-}
  AssignFile(f, fname);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(f, 1);
  {$I+}
  if ioresult <> 0 then begin
        NSLog('Error in reading BVox header.'+inttostr(IOResult));
        FileMode := 2;
        exit;
  end;
  FSz := Filesize(f);
  h.magic :=  0; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
  BlockRead(f, h, sizeof(Thdr));
  {$IFDEF ENDIAN_BIG}
  pswap4i(longint(h.magic));
  pswap4i(longint(h.version));
  {$ENDIF}
  if (h.magic <> 542658390) then begin
     CloseFile(f);
     result := readVoxSlab6(fname, nhdr);
     exit;
  end;
  if (h.version > 150) then begin
    CloseFile(f);
    NSLog(format('Unsupported .VOX version %d',[h.version]));
    exit;
  end;
  nhdr.vox_offset := 0;
  nhdr.dim[1] := 0;
  while (filepos(f)+sizeof(TChunk)) < FSz do begin
       BlockRead(f, chunk, sizeof(Tchunk));
       {$IFDEF ENDIAN_BIG}
       pswap4i(longint(chunk.id));
       pswap4i(longint(chunk.szBytes));
       pswap4i(longint(chunk.nChild));
       {$ENDIF}
       chunkEnd := filepos(f) + chunk.szBytes;
       if chunk.id = 1313423693 then begin //MAIN
          //NSLog('MAIN');
       end;
      if chunk.id = 1163544915 then begin //SIZE
         NSLog('SiZE');
      end;
      if chunk.id = 1230657880 then begin //XYZI
         NSLog('XYZI');
      end;
      seek(f, chunkEnd);
  end;
  CloseFile(f);
   convertForeignToNifti(nhdr);
  nhdr.descrip := 'VOX'+kIVers;
  result := true;
end; //readVox *)


function readBVox (var fname: string; var nhdr: TNIFTIhdr; var swapEndian: boolean): boolean;
//http://pythology.blogspot.com/2014/08/you-can-do-cool-stuff-with-manual.html
Type
  Tbv_header = packed record //Next: BVox Format Header structure
        nx, ny, nz, nvol : LongInt;
  end; // Tbv_header;
var
   bhdr : Tbv_header;
   lHdrFile: file;
   nvox, nvoxswap, FSz : integer;
begin
  result := false;
  {$I-}
  AssignFile(lHdrFile, fname);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(lHdrFile, 1);
  {$I+}
  if ioresult <> 0 then begin
        NSLog('Error in reading BVox header.'+inttostr(IOResult));
        FileMode := 2;
        exit;
  end;
  FSz := Filesize(lHdrFile);
  bhdr.nx :=  0; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
  BlockRead(lHdrFile, bhdr, sizeof(Tbv_header));
  CloseFile(lHdrFile);
  swapEndian := false;
  nVox := bhdr.nx * bhdr.ny * bhdr.nz * bhdr.nvol * 4; //*4 as 32-bpp
  if (nVox + sizeof(Tbv_header) ) <> FSz then begin
    swapEndian := true;
    pswap4i(bhdr.nx);
    pswap4i(bhdr.ny);
    pswap4i(bhdr.nz);
    pswap4i(bhdr.nvol);
    nVoxSwap := bhdr.nx * bhdr.ny * bhdr.nz * bhdr.nvol * 4; //*4 as 32-bpp
    if (nVoxSwap + sizeof(Tbv_header) ) <> FSz then begin
       NSLog(format('Not a valid BVox file: expected filesize of %d or %d bytes (%dx%dx%dx%d)',[nVoxSwap,nVox, bhdr.nx, bhdr.ny, bhdr.nz, bhdr.nvol]));
       exit;
    end;

  end;
  if (bhdr.nvol > 1) then
     nhdr.dim[0]:=4//4D
  else
      nhdr.dim[0]:=3;//3D
  nhdr.dim[1]:=bhdr.nx;
  nhdr.dim[2]:=bhdr.ny;
  nhdr.dim[3]:=bhdr.nz;
  nhdr.dim[4]:=bhdr.nvol;
  nhdr.pixdim[1]:=1.0;
  nhdr.pixdim[2]:=1.0;
  nhdr.pixdim[3]:=1.0;
  nhdr.datatype := kDT_FLOAT32;
  nhdr.vox_offset := sizeof(Tbv_header);
  nhdr.sform_code := 1;
  (*nhdr.srow_x[0]:=nhdr.pixdim[1];nhdr.srow_x[1]:=0.0;nhdr.srow_x[2]:=0.0;nhdr.srow_x[3]:=0.0;
  nhdr.srow_y[0]:=0.0;nhdr.srow_y[1]:=nhdr.pixdim[2];nhdr.srow_y[2]:=0.0;nhdr.srow_y[3]:=0.0;
  nhdr.srow_z[0]:=0.0;nhdr.srow_z[1]:=0.0;nhdr.srow_z[2]:=-nhdr.pixdim[3];nhdr.srow_z[3]:=0.0;*)
  SetSForm(nhdr);
  convertForeignToNifti(nhdr);
  nhdr.descrip := 'BVOX'+kIVers;
  result := true;
end; //readBVox

function readDeltaVision (var fname: string; var nhdr: TNIFTIhdr; var swapEndian: boolean): boolean;
const
     kDV_HEADER_SIZE = 1024;
     kSIG_NATIVE = 49312;
     kSIG_SWAPPED = 41152;
Type
  Tdv_header = packed record //Next: PIC Format Header structure
        nx, ny, nz, datatype : LongInt; //  0,4,8,12
        pad0: array [1..24] of char; //padding 16..39
        xDim,yDim,zDim : single; //40,44,48
        pad1: array [1..40] of char; //padding 52..91
        ExtendedHeaderSize: LongInt; //92
        sig: word; //96
        pad2: array [1..82] of char; //padding 98..179
        numTimes : int32; //180
        pad3: array [1..12] of char;//padding 184..195
        numChannels : word; //196
        pad4: array [1..10] of char;//padding 198..207
        xOri, yOri, zOri: single; //208,212,216
        pad5: array [1..804] of char;//padding 220..1024
        //padding
  end; // Tdv_header;
var
   bhdr : Tdv_header;
   lHdrFile: file;
   sizeZ, sizeT: integer;
begin
  result := false;
  {$I-}
  AssignFile(lHdrFile, fname);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(lHdrFile, 1);
  {$I+}
  if ioresult <> 0 then begin
        NSLog('Error in reading DeltaVision header.'+inttostr(IOResult));
        FileMode := 2;
        exit;
  end;
  bhdr.nx :=  0; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
  BlockRead(lHdrFile, bhdr, sizeof(Tdv_header));
  CloseFile(lHdrFile);
  if (bhdr.sig <> kSIG_NATIVE) and (bhdr.sig <> kSIG_SWAPPED) then begin //signature not found!
    NSLog('Error in reading DeltaVision file (signature not correct).');
    exit;
  end;
  swapEndian := false;
  if (bhdr.sig = kSIG_SWAPPED) then begin
    swapEndian := true;
    pswap4i(bhdr.nx);
    pswap4i(bhdr.ny);
    pswap4i(bhdr.nz);
    pswap4r(bhdr.xDim);
    pswap4r(bhdr.yDim);
    pswap4r(bhdr.zDim);
    pswap4i(bhdr.ExtendedHeaderSize);
    bhdr.sig := swap(bhdr.sig);
    pswap4i(bhdr.numTimes);
    bhdr.numChannels := swap(bhdr.numChannels);
    pswap4r(bhdr.xOri);
    pswap4r(bhdr.yOri);
    pswap4r(bhdr.zOri);
  end;
  sizeZ := bhdr.nz;
  sizeT := 1;
  if ( bhdr.nz mod (bhdr.numTimes * bhdr.numChannels) = 0 ) then begin
        sizeZ := bhdr.nz div (bhdr.numTimes * bhdr.numChannels);
        sizeT := bhdr.nz div sizeZ;
  end;
  if (sizeT > 1) then
     nhdr.dim[0]:=4//4D
  else
      nhdr.dim[0]:=3;//3D
  nhdr.dim[1]:=bhdr.nx;
  nhdr.dim[2]:=bhdr.ny;
  nhdr.dim[3]:=sizeZ;
  nhdr.dim[4]:=sizeT;
  nhdr.pixdim[1]:=1.0;
  nhdr.pixdim[2]:=1.0;
  nhdr.pixdim[3]:=1.0;
  nhdr.datatype := kDT_UINT16;
  nhdr.vox_offset := kDV_HEADER_SIZE + bhdr.ExtendedHeaderSize;
  nhdr.sform_code := 1;
  (*nhdr.srow_x[0]:=nhdr.pixdim[1];nhdr.srow_x[1]:=0.0;nhdr.srow_x[2]:=0.0;nhdr.srow_x[3]:=0.0;
  nhdr.srow_y[0]:=0.0;nhdr.srow_y[1]:=nhdr.pixdim[2];nhdr.srow_y[2]:=0.0;nhdr.srow_y[3]:=0.0;
  nhdr.srow_z[0]:=0.0;nhdr.srow_z[1]:=0.0;nhdr.srow_z[2]:=-nhdr.pixdim[3];nhdr.srow_z[3]:=0.0;*)
  SetSForm(nhdr);
  convertForeignToNifti(nhdr);
  nhdr.descrip := 'DV'+kIVers;
  result := true;
end; //readDeltaVision

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
end; //proc swap4

function readGipl (var fname: string; var nhdr: TNIFTIhdr; var swapEndian: boolean): boolean;
const
     kmagic_number =4026526128;
Type
  Tdv_header = packed record
        dim: array [1..4] of Word;
        data_type: word;
        pixdim: array [1..4] of Single;
        patient: array [1..80] of char;
        matrix: array [1..20] of Single;
        orientation, par2: byte;
        voxmin, voxmax: Double;
        origin: array [1..4] of Double;
        pixval_offset, pixval_cal, interslicegap, user_def2 : single;
        magic_number : uint32;
  end; // Tdv_header;
var
   bhdr : Tdv_header;
   lHdrFile: file;
   i, FSz,FSzX: integer;
begin
  result := false;
  {$I-}
  AssignFile(lHdrFile, fname);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(lHdrFile, 1);
  {$I+}
  if ioresult <> 0 then begin
        NSLog('Error in reading GIPL header.'+inttostr(IOResult));
        FileMode := 2;
        exit;
  end;
  FSz := Filesize(lHdrFile);
  bhdr.data_type :=  0; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
  BlockRead(lHdrFile, bhdr, sizeof(Tdv_header));
  CloseFile(lHdrFile);
  swapEndian := false;
  {$IFNDEF ENDIAN_BIG} //GIPL is big endian, so byte swap on little endian
  swapEndian := true;
  for i := 1 to 4 do begin
      bhdr.dim[i] := swap(bhdr.dim[i]);
      pswap4r(bhdr.pixdim[i]);
      bhdr.origin[i] := swap64r(bhdr.origin[i]);
  end;
  for i := 1 to 20 do
      pswap4r(bhdr.matrix[i]);
  bhdr.data_type := swap(bhdr.data_type);
  bhdr.voxmin := swap64r(bhdr.voxmin);
  bhdr.voxmax := swap64r(bhdr.voxmax);
  pswap4r(bhdr.pixval_offset);
  pswap4r(bhdr.pixval_cal);
  pswap4r(bhdr.interslicegap);
  pswap4r(bhdr.user_def2);
  pswap4ui(bhdr.magic_number);
  {$ENDIF}
  //NSLog(format('%g %g %g %g ', [bhdr.matrix[1],bhdr.matrix[2],bhdr.matrix[3],bhdr.matrix[4]] ));
  if bhdr.magic_number <> kmagic_number then begin
     NSLog('Error in reading GIPL header signature '+inttostr(bhdr.magic_number)+' != '+inttostr(sizeof(Tdv_header)));
     exit;
  end;
  if (bhdr.data_type = 1) then
     nhdr.datatype := kDT_BINARY
  else if (bhdr.data_type = 7) then
       nhdr.datatype := kDT_INT8
  else if (bhdr.data_type = 8) then
       nhdr.datatype := kDT_UNSIGNED_CHAR
  else if (bhdr.data_type = 15) then
       nhdr.datatype := kDT_INT16
  else if (bhdr.data_type = 16) then
       nhdr.datatype := kDT_UINT16
  else if (bhdr.data_type = 31) then
       nhdr.datatype := kDT_UINT32
  else if (bhdr.data_type = 32) then
       nhdr.datatype := kDT_INT32
  else if (bhdr.data_type = 64) then
       nhdr.datatype := kDT_FLOAT32
  else if (bhdr.data_type = 64) then
       nhdr.datatype := kDT_DOUBLE
  else begin
    NSLog('Unsupported GIPL data type '+inttostr(nhdr.datatype));
    exit;
  end;
  for i := 1 to 4 do begin
      if bhdr.dim[i] < 1 then
         bhdr.dim[i] := 1;
       nhdr.dim[i]:=bhdr.dim[i];
       nhdr.pixdim[i]:=bhdr.pixdim[i]
  end;
  if (bhdr.dim[4] > 1) then
     nhdr.dim[0]:=4//4D
  else
     nhdr.dim[0]:=3;//3D
  if bhdr.interslicegap > 0 then
     nhdr.pixdim[3] := bhdr.pixdim[3] + bhdr.interslicegap;
  nhdr.vox_offset := sizeof(Tdv_header);
  nhdr.sform_code := 1;
  (*nhdr.srow_x[0]:=nhdr.pixdim[1];nhdr.srow_x[1]:=0.0;nhdr.srow_x[2]:=0.0;nhdr.srow_x[3]:=0.0;
  nhdr.srow_y[0]:=0.0;nhdr.srow_y[1]:=nhdr.pixdim[2];nhdr.srow_y[2]:=0.0;nhdr.srow_y[3]:=0.0;
  nhdr.srow_z[0]:=0.0;nhdr.srow_z[1]:=0.0;nhdr.srow_z[2]:=nhdr.pixdim[3];nhdr.srow_z[3]:=0.0; *)
  SetSForm(nhdr);
  convertForeignToNifti(nhdr);
  FSzX := sizeof(Tdv_header) + ( bhdr.dim[1]*bhdr.dim[2]*bhdr.dim[3]*bhdr.dim[4]*(nhdr.bitpix div 8));
  if (nhdr.bitpix <> 1) and (FSz <> FSzX) then begin
     NSLog('Error unexpected file size '+inttostr(FSz)+' != '+inttostr(FSzX));
     exit;
  end;
  result := true;
    nhdr.descrip := 'GIPL'+kIVers;
end; //readGipl

function readpic (var fname: string; var nhdr: TNIFTIhdr): boolean;
//function readpic (var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean): boolean;
//https://github.com/jefferis/pic2nifti/blob/master/libpic2nifti.c
const
     kBIORAD_HEADER_SIZE  = 76;
     kBIORAD_NOTE_HEADER_SIZE = 16;
     kBIORAD_NOTE_SIZE = 80;
Type
  Tbiorad_header = packed record //Next: PIC Format Header structure
        nx, ny : word;    //  0   2*2     image width and height in pixels
        npic: SmallInt;               //  4   2       number of images in file
        ramp1_min: SmallInt;          //  6   2*2     LUT1 ramp min. and max.
        ramp1_max: SmallInt;
        notes: LongInt;                // 10   4       no notes=0; has notes=non zero
        byte_format: SmallInt;        // 14   2       bytes=TRUE(1); words=FALSE(0)
        n : word;         // 16   2       image number within file
        name: array [1..32] of char;            // 18   32      file name
        merged: SmallInt;             // 50   2       merged format
        color1 : word;    // 52   2       LUT1 color status
        file_id : word;   // 54   2       valid .PIC file=12345
        ramp2_min: SmallInt;          // 56   2*2     LUT2 ramp min. and max.
        ramp2_max: SmallInt;
        color2: word;    // 60   2       LUT2 color status
        edited: SmallInt;             // 62   2       image has been edited=TRUE(1)
        lens: SmallInt;               // 64   2       Integer part of lens magnification
        mag_factor: single;         // 66   4       4 byte real mag. factor (old ver.)
        dummy1, dummy2, dummy3: word;  // 70   6       NOT USED (old ver.=real lens mag.)
     end; // biorad_header;
    Tbiorad_note_header = packed record
      blank: SmallInt;		// 0	2
      note_flag: LongInt;		// 2	4
      blank2: LongInt;			// 6	4
      note_type: SmallInt;	// 10	2
      blank3: LongInt;			// 12	4
      note: array[1..kBIORAD_NOTE_SIZE] of char;
    end;//biorad_note_header;
var
   bhdr : Tbiorad_header;
   nh: Tbiorad_note_header;
   lHdrFile: file;
   //s: string;
   i, bytesHdrImg, nNotes: integer;
begin
  result := false;
  {$I-}
  AssignFile(lHdrFile, fname);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(lHdrFile, 1);
  {$I+}
  if ioresult <> 0 then begin
        NSLog('Error in reading BioRad PIC header.'+inttostr(IOResult));
        FileMode := 2;
        exit;
  end;
  bhdr.nx :=  0; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
  BlockRead(lHdrFile, bhdr, sizeof(Tbiorad_header));
  if (bhdr.file_id <> 12345) then begin //signature not found!
    CloseFile(lHdrFile);
    NSLog('Error in reading BioRad PIC header file ID not 12345.');
    exit;
  end;
  {$IFDEF ENDIAN_BIG}
  swapEndian := true;
  bhdr.nx := swap(bhdr.nx);
  bhdr.ny := swap(bhdr.ny);
  bhdr.npic := swap(bhdr.npic);
  bhdr.byte_format := swap(bhdr.byte_format);
  {$ENDIF}
  nhdr.dim[0]:=3;//3D
  nhdr.dim[1]:=bhdr.nx;
  nhdr.dim[2]:=bhdr.ny;
  nhdr.dim[3]:=bhdr.npic;
  nhdr.dim[4]:=1;
  nhdr.pixdim[1]:=1.0;
  nhdr.pixdim[2]:=1.0;
  nhdr.pixdim[3]:=1.0;
  if (bhdr.byte_format = 1) then
      nhdr.datatype := kDT_UINT8 // 2
  else
      nhdr.datatype := kDT_UINT16;
  nhdr.vox_offset := kBIORAD_HEADER_SIZE;
  bytesHdrImg := sizeof(Tbiorad_header)+bhdr.nx*bhdr.ny*bhdr.npic*bhdr.byte_format;
  nNotes := (Filesize(lHdrFile) - bytesHdrImg) div (kBIORAD_NOTE_HEADER_SIZE+kBIORAD_NOTE_SIZE);
  if (nNotes > 0) then begin
     seek(lHdrFile, bytesHdrImg);
     for i := 1 to nNotes do begin
         nh.note_type :=  1; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
         BlockRead(lHdrFile, nh, sizeof(Tbiorad_note_header));
         {$IFDEF ENDIAN_BIG}
         nh.note_type := swap(nh.note_type);
         {$ENDIF}
         if(nh.note_type=1) then continue; // These are not interesting notes
         if AnsiStartsStr('AXIS_2 ', nh.note) then
             nhdr.pixdim[1]  := parsePicString(nh.note);
         if AnsiStartsStr('AXIS_3 ', nh.note) then
             nhdr.pixdim[2]  := parsePicString(nh.note);
         if AnsiStartsStr('AXIS_4 ', nh.note) then
             nhdr.pixdim[3]  := parsePicString(nh.note);
     end;
  end;
  CloseFile(lHdrFile);
  nhdr.sform_code := 1;
  (*nhdr.srow_x[0]:=nhdr.pixdim[1];nhdr.srow_x[1]:=0.0;nhdr.srow_x[2]:=0.0;nhdr.srow_x[3]:=0.0;
  nhdr.srow_y[0]:=0.0;nhdr.srow_y[1]:=nhdr.pixdim[2];nhdr.srow_y[2]:=0.0;nhdr.srow_y[3]:=0.0;
  nhdr.srow_z[0]:=0.0;nhdr.srow_z[1]:=0.0;nhdr.srow_z[2]:=-nhdr.pixdim[3];nhdr.srow_z[3]:=0.0;*)
  SetSForm(nhdr);
  convertForeignToNifti(nhdr);
  nhdr.descrip := 'PIC'+kIVers;
  result := true;
end;

function readEcat(var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean): boolean;
Const
  ECAT7_BYTE =1;
  (*ECAT7_VAXI2 =2;
  ECAT7_VAXI4 =3;
  ECAT7_VAXR4 =4;
  ECAT7_IEEER4 =5;*)
  ECAT7_SUNI2 =6;
  ECAT7_SUNI4 =7;
  //image types
  ECAT7_2DSCAN =1;
  (*ECAT7_IMAGE16 =2;
  ECAT7_ATTEN =3;
  ECAT7_2DNORM =4;
  ECAT7_POLARMAP =5;
  ECAT7_VOLUME8 =6;
  ECAT7_VOLUME16 =7;
  ECAT7_PROJ =8;
  ECAT7_PROJ16 =9;
  ECAT7_IMAGE8 =10;
  ECAT7_3DSCAN =11;
  ECAT7_3DSCAN8 =12;
  ECAT7_3DNORM =13;*)
  ECAT7_3DSCANFIT =14;
Label
  666;
Type
  THdrMain = packed record //Next: ECAT Format Header structure
    magic: array[1..14] of char;
    original_filename: array[1..32] of char;
    sw_version, system_type, file_type: uint16;
    serial_number: array[1..10] of char;
    scan_start_time: uint32;
    isotope_name: array[1..8] of char;
    isotope_halflife: single;
    radiopharmaceutical: array[1..32] of char;
    gantry_tilt, gantry_rotation, bed_elevation, intrinsic_tilt: single;
    wobble_speed, transm_source_type: int16;
    distance_scanned, transaxial_fov: single;
    angular_compression, coin_samp_mode, axial_samp_mode: uint16;
    ecat_calibration_factor: single;
    calibration_unitS, calibration_units_type, compression_code: uint16;
    study_type: array[1..12] of char;
    patient_id: array[1..16] of char;
    patient_name: array[1..32] of char;
    patient_sex, patient_dexterity: char;
    patient_age, patient_height, patient_weight: single;
    patient_birth_date: uint32;
    physician_name, operator_name, study_description: array[1..32] of char;
    acquisition_type, patient_orientation: uint16;
    facility_name: array[1..20] of char;
    num_planes, num_frames, num_gates, num_bed_pos: uint16;
    init_bed_position: single;
    bed_position: array[1..15] of single;
    plane_separation: single;
    lwr_sctr_thres, lwr_true_thres, upr_true_thres: uint16;
    user_process_code: array[1..10] of char;
    acquisition_mode: uint16;
    bin_size, branching_fraction: single;
    dose_start_time: single;
    dosage, well_counter_corr_factor: single;
    data_units: array[1..32] of char;
    septa_state: uint16;
    fill: array[1..12] of char;
  end;
  THdrList = packed record
        hdr,
        r01,r02,r03,r04,r05,r06,r07,r08,r09,r10,
        r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,
        r21,r22,r23,r24,r25,r26,r27,r28,r29,r30,
        r31 : array[1..4] of int32;
    end;
  THdrImg = packed record
    data_type, num_dimensions, x_dimension, y_dimension, z_dimension: smallint;
    x_offset, y_offset, z_offset, recon_zoom, scale_factor: single;
    image_min, image_max: smallint;
    x_pixel_size, y_pixel_size, z_pixel_size: single;
    frame_duration, frame_start_time,filter_code: smallint;
    x_resolution, y_resolution, z_resolution, num_r_elements, num_angles, z_rotation_angle, decay_corr_fctr: single;
    processing_code, gate_duration, r_wave_offset, num_accepted_beats: int32;
    filter_cutoff_frequenc, filter_resolution, filter_ramp_slope: single;
    filter_order: smallint;
    filter_scatter_fraction, filter_scatter_slope: single;
    annotation: string[40];
    mtx: array [1..9] of single;
    rfilter_cutoff, rfilter_resolution: single;
    rfilter_code, rfilter_order: int16;
    zfilter_cutoff, zfilter_resolution: single;
    zfilter_code, zfilter_order: smallint;
    mtx_1_4, mtx_2_4, mtx_3_4: single;
    scatter_type, recon_type, recon_views: smallint;
    fill_cti: array [1..87] of int16;
    fill_user: array [1..49] of int16;
  end;
var
  mhdr: THdrMain;
  ihdr: THdrImg;
  lhdr: THdrList;
  lHdrFile: file;
  img1_StartBytes: integer;
begin
  result := false;
  gzBytes := 0;
  {$I-}
  AssignFile(lHdrFile, fname);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(lHdrFile, 1);
  {$I+}
  if ioresult <> 0 then begin
        NSLog('Error in reading ECAT header.'+inttostr(IOResult));
        FileMode := 2;
        exit;
  end;
  mhdr.file_type :=  0; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
  BlockRead(lHdrFile, mhdr, sizeof(mhdr));
  {$IFDEF FPC} mhdr.magic:=upcase(mhdr.magic); {$ENDIF} //Delphi 7 can not upcase arrays
  if ((mhdr.magic[1] <> 'M') or (mhdr.magic[2] <> 'A') or (mhdr.magic[3] <> 'T') or (mhdr.magic[4] <> 'R') or (mhdr.magic[5] <> 'I') or (mhdr.magic[6] <> 'X')) then
       goto 666;
  {$IFDEF ENDIAN_BIG} //data always stored big endian
    swapEndian := false;
  {$ELSE}
    swapEndian := true;
    mhdr.sw_version := swap2(mhdr.sw_version);
    mhdr.file_type := swap2(mhdr.file_type);
    mhdr.num_frames := swap2(mhdr.num_frames);
    pswap4r(mhdr.ecat_calibration_factor);
  {$ENDIF}
  if ((mhdr.file_type < ECAT7_2DSCAN) or (mhdr.file_type > ECAT7_3DSCANFIT)) then begin
      ShowMsg('Unknown ECAT file type '+ inttostr( mhdr.file_type));
      goto 666;
  end;
  //read list header
  lhdr.r01[2] :=  0; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
  BlockRead(lHdrFile, lhdr, sizeof(lhdr));
  {$IFNDEF ENDIAN_BIG} //data always stored big endian
  pswap4i(lhdr.r01[2]);
  {$ENDIF}
  img1_StartBytes := lhdr.r01[2] * 512;
  //read image header
  seek(lHdrFile, img1_StartBytes - 512);
  ihdr.data_type :=  0; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
  BlockRead(lHdrFile, ihdr, sizeof(ihdr));
  {$IFNDEF ENDIAN_BIG} //data always stored big endian
  ihdr.data_type := swap(ihdr.data_type);
  pswap4r(ihdr.x_pixel_size);
  pswap4r(ihdr.y_pixel_size);
  pswap4r(ihdr.z_pixel_size);
  pswap4r(ihdr.scale_factor);
  ihdr.x_dimension := swap(ihdr.x_dimension);
  ihdr.y_dimension := swap(ihdr.y_dimension);
  ihdr.z_dimension := swap(ihdr.z_dimension);
  {$ENDIF}
  ihdr.x_pixel_size := ihdr.x_pixel_size * 10.0;
  ihdr.y_pixel_size := ihdr.y_pixel_size * 10.0;
  ihdr.z_pixel_size := ihdr.z_pixel_size * 10.0;
  if ((ihdr.data_type <> ECAT7_BYTE) and (ihdr.data_type <> ECAT7_SUNI2) and (ihdr.data_type <> ECAT7_SUNI4)) then begin
      ShowMsg('Unknown ECAT data type '+ inttostr(ihdr.data_type));
      goto 666;
  end;
  nhdr.scl_slope := ihdr.scale_factor * mhdr.ecat_calibration_factor;
  nhdr.datatype := kDT_INT16;
  if (ihdr.data_type = ECAT7_BYTE) then
        nhdr.datatype := kDT_UINT8
  else if (ihdr.data_type = ECAT7_SUNI4)  then
       nhdr.datatype := kDT_INT32;
  nhdr.dim[1]:=ihdr.x_dimension;
  nhdr.dim[2]:=ihdr.y_dimension;
  nhdr.dim[3]:=ihdr.z_dimension;
  nhdr.dim[4]:=1;
  nhdr.pixdim[1]:=ihdr.x_pixel_size;
  nhdr.pixdim[2]:=ihdr.y_pixel_size;
  nhdr.pixdim[3]:=ihdr.z_pixel_size;
  nhdr.vox_offset := img1_StartBytes;
  nhdr.sform_code := 0;
  nhdr.srow_x[0]:=-nhdr.pixdim[1]; nhdr.srow_x[1]:=0; nhdr.srow_x[2]:=0; nhdr.srow_x[3]:=(ihdr.x_dimension-2.0)/2.0*ihdr.x_pixel_size;
  nhdr.srow_y[0]:=0; nhdr.srow_y[1]:=-nhdr.pixdim[2]; nhdr.srow_y[2]:=0; nhdr.srow_y[3]:=(ihdr.y_dimension-2.0)/2.0*ihdr.y_pixel_size;
  nhdr.srow_z[0]:=0; nhdr.srow_z[1]:=0; nhdr.srow_z[2]:=-nhdr.pixdim[3]; nhdr.srow_z[3]:=(ihdr.z_dimension-2.0)/2.0*ihdr.z_pixel_size;
  //SetSForm(nhdr);
  convertForeignToNifti(nhdr);
  nhdr.descrip := 'ECAT'+kIVers;
  result := true;
666:
CloseFile(lHdrFile);
end; //ECAT

function readVOL(var fname: string; var nhdr: TNIFTIhdr; var swapEndian: boolean): boolean;
// original format by Barthold Lichtenbelt, 1998
// for Mark Dow's vol format see http://paulbourke.net/dataformats/vol/
//   Ogles2 volumeReaders.cpp for more details
Type
  Tvol = packed record // Header structure
   version, hdrSz, width, height, depth, bpp : longint;
  end;

var
  f: file;
  hdr: Tvol;
  FSz, vox: int64;
begin
	result := false;
	FileMode := fmOpenRead;
	{$I-}
	AssignFile(f, fname);
	FileMode := 0;  //Set file access to read only
	Reset(f, 1);
	FSz := Filesize(f);
	{$I+}
	if ioresult <> 0 then begin
		NSLog('Error in reading VOL header.'+inttostr(IOResult));
		FileMode := 2;
		exit;
	end;
	BlockRead(f, hdr, sizeof(Tvol));
	CloseFile(f);
	FileMode := 2;
	if (hdr.version <> 192837465) then begin
	  swapEndian := true;
	  swap4(hdr.version);
	  swap4(hdr.hdrSz);
	  swap4(hdr.width);
	  swap4(hdr.height);
	  swap4(hdr.depth);
	  swap4(hdr.bpp);
	end;
	if (hdr.version <> 192837465) or (FSz <= hdr.hdrSz) or (hdr.width < 1) or (hdr.height < 1) or (hdr.depth < 1) then begin
		NSLog('Unsupported .vol format.');
		exit;
	end;
	FSz := FSz - hdr.hdrSz;
	vox := hdr.width * hdr.height * hdr.depth;
	if (vox = FSz) then
		nhdr.datatype := kDT_UINT8
	else if ((2 * vox) = FSz)  then
		nhdr.datatype := kDT_INT16
	else if((2 * vox) = FSz)  then
		nhdr.datatype := kDT_INT32
	else begin
		NSLog(format('Unknown .vol format. offset %d size %dx%dx%d', [hdr.hdrSz, hdr.width, hdr.height, hdr.depth]));
		exit;
	end;
	nhdr.dim[1]:=hdr.width;
	nhdr.dim[2]:=hdr.height;
	nhdr.dim[3]:=hdr.depth;
	nhdr.vox_offset := hdr.hdrSz;
        SetSForm(nhdr);
	convertForeignToNifti(nhdr);
	nhdr.descrip := 'VOL'+kIVers;
	result := true;
end; //readVOL

function readAim(var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean): boolean;
procedure readKeyAim(field: string; var str: string; out x,y,z: single);
//Orig-ISQ-Dim-um                                 20480      20480      12300
var
	st, en: integer;
	s: string;
	strlst: TStringList;
begin
	x := kNANsingle;
	y := kNANsingle;
	z := kNANsingle;
	st := PosEx(field,  str);
	if st < 1 then exit();
	st := st + length(field);
	en := PosEx(chr($0A),  str, st);
	if en < 2 then exit();
	s := copy(str, st, en-st);
	s := trim(s);
	strlst:=TStringList.Create;
	strlst.DelimitedText := s;
	if strlst.Count > 0 then
		x := strtoFloatDef(strlst[0], kNANsingle);
	if strlst.Count > 1 then
		y := strtoFloatDef(strlst[1], kNANsingle);
	if strlst.Count > 2 then
		z := strtoFloatDef(strlst[2], kNANsingle);
	strlst.Free;
end;
//https://www.researchgate.net/publication/320077296_AIMreader_python_implementation_and_examples
const
	kheaderints = 32;
var
	AIM_ints: array [0..(kheaderints-1)] of longint;
	f: file;
	binSz, strSz: integer;
	x, y, z, xp, yp, zp: single;
	str: string;
	fSz: int64;
begin
	result := false;
	result := false;
	FileMode := fmOpenRead;
	gzBytes := 0;
	{$I-}
	AssignFile(f, fname);
	FileMode := 0;  //Set file access to read only
	Reset(f, 1);
	fSz := FileSize(f);
	{$I+}
	if ioresult <> 0 then begin
		NSLog('Error in reading AIM header.'+inttostr(IOResult));
		FileMode := 2;
		exit;
	end;
	BlockRead(f, AIM_ints, sizeof(AIM_ints));
	CloseFile(f);
	FileMode := 2;
	{$IFDEF ENDIAN_BIG} //data always stored big endian
	for i := 0 to (kheaderints -1) do
		swap4(AIM_ints[i]);
	swapEndian := true;
	{$ELSE}
	swapEndian := false;
	{$ENDIF}
        (* //not all AIM files start with signature, see SEGMENTED_V20TRUE_BIN.AIM;1
        if (AIM_ints[0] <> 1145915713) then begin
		NSLog('Error AIM images should begin "AIMD".');
		exit;
	end;*)
        str := '';
	nhdr.datatype := 0;
	if (AIM_ints[5] = 16) then begin
		//-> version 020
		if (AIM_ints[10] = 131074) then
			nhdr.datatype := kDT_INT16
		else if (AIM_ints[10] = 65537)  then
			nhdr.datatype := kDT_UINT8
		else if (AIM_ints[10] = 1376257) or (AIM_ints[10] = 524290)  then
			str := '(compressed)'
		else
			nhdr.datatype :=  1;//str := 'Datatype='+inttostr(AIM_ints[10]);
		strSz := AIM_ints[2];
		binSz := 160;
		nhdr.vox_offset := strSz + binSz;
		nhdr.dim[1] := AIM_ints[14];
		nhdr.dim[2] := AIM_ints[15];
		nhdr.dim[3] := AIM_ints[16];
	end else begin
		//-> version 030
		if (AIM_ints[17] = 131074) then
			nhdr.datatype := kDT_INT16
		else if (AIM_ints[17] = 65537)  then
			nhdr.datatype := kDT_UINT8
		else if (AIM_ints[17] = 1376257) or (AIM_ints[17] = 524290)  then
			str := '(compressed)'
		else
			nhdr.datatype :=  1;//str := 'Datatype='+inttostr(AIM_ints[17]);
		strSz := AIM_ints[8];
		binSz := 280;
		nhdr.vox_offset := strSz + binSz;
		nhdr.dim[1] := AIM_ints[24];
		nhdr.dim[2] := AIM_ints[26];
		nhdr.dim[3] := AIM_ints[28];
    end;
    if nhdr.datatype =  1 then exit; //do not report: since not all AIM files have a signature, perhaps not an AIM format file at all
    if (nhdr.datatype = 0) or (strSz < 1) or (nhdr.vox_offset > fSz) then begin
    	NSLog('Unable to read AIM file. Offset='+inttostr(round(nhdr.vox_offset))+' '+str);
    	exit;
    end;
    //read string header
	AssignFile(f, fname);
	FileMode := 0;  //Set file access to read only
	Reset(f,1);
	Seek(f, sizeof(AIM_ints));
	SetLength(str, strSz);
	BlockRead(f, Str[1],strSz);
	CloseFile(f);
	FileMode := 2;
	readKeyAim('Orig-GOBJ-Dim-p',str, xp,yp,zp); //Resolution
	if (xp = kNANsingle) then
		readKeyAim('Orig-ISQ-Dim-p',str, xp,yp,zp); //Resolution
	readKeyAim('Orig-GOBJ-Dim-um', str, x,y,z); //Field of View
	if (x = kNANsingle) then
		readKeyAim('Orig-ISQ-Dim-um', str, x,y,z); //Field of View
	if (zp <> kNANsingle) and (z <> kNANsingle) then begin
		if (x > 0) and (xp > 0) then
			nhdr.pixdim[1] := x / xp;
		if (y > 0) and (yp > 0) then
			nhdr.pixdim[2] := y / yp;
		if (z > 0) and (zp > 0) then
			nhdr.pixdim[3] := z / zp;
	end;
	nhdr.xyzt_units := 3; //microns
	readKeyAim('Density: intercept', str, x,y,z);
	if x <> kNANsingle then
		nhdr.scl_inter := x;
	readKeyAim('Density: slope', str, x,y,z);
	if x <> kNANsingle then
		nhdr.scl_slope := x;
	readKeyAim('Scaled by factor', str, x,y,z);
	readKeyAim('scale (el_size factor)', str, xp,yp,zp);
	//writeln(format('%g %dx%dx%d', [nhdr.vox_offset, nhdr.dim[1], nhdr.dim[2], nhdr.dim[3]]));
        SetSForm(nhdr);
	convertForeignToNifti(nhdr);
	nhdr.descrip := 'AIM'+kIVers;
	if x <> kNANsingle then
		nhdr.aux_file := format('Scaling %g', [x]);
	if xp <> kNANsingle then
		nhdr.db_name := format('Factor %g', [xp]);
	result := true;
end; //readAIM()

function readMRCHeader (var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean): boolean;
//https://mrcfile.readthedocs.io/en/latest/usage_guide.html
//https://bio3d.colorado.edu/imod/doc/mrc_format.txt
//https://www.ccpem.ac.uk/mrc_format/mrc2014.php
Type
  Tmrc = packed record //Next: MGH Format Header structure
   nx,ny,nz,mode, nxstart, nystart, nzstart, mx,my,mz : int32;
   xlen,ylen,zlen, alpha, beta, gamma: single;
   mapc, mapr, maps: int32;
   amin, amax,amean: single;
   ispg, next: int32;
   creatid, extra0, extra1, extra2: int16;
   extType: array [1..4] of char;
   nversion: int32;
   extraData: array [1..16] of char;
   nint, nreal: int16;
   extraData2: array [1..20] of char;
   imodStamp, imodFlags: int32;
   idtype, lens, nd1, nd2, vd1, vd2: int16;
   tiltangles: array [1..6] of single;
   xorg, yorg, zorg: single;
   cmap: array [1..4] of char;
   stamp: array [1..4] of uint8;
   rms: single;
   nlabs: int32;
   labels: array [1..800] of char;
  end;
var
  h: Tmrc;
  lHdrFile: file;
  lExt: string;
  //isModern: boolean = false;
  lBuff: Bytep;
begin
	result := false;
	FileMode := fmOpenRead;
	lExt := UpCaseExt(fname);
	if (lExt = '.GZ') then begin
		lBuff := @h;
		UnGZip(fname,lBuff,0,sizeof(Tmrc)); //1388
		gzBytes := K_gzBytes_headerAndImageCompressed;
	end else begin //if MGZ, else assume uncompressed MGH
		gzBytes := 0;
		{$I-}
		AssignFile(lHdrFile, fname);
		FileMode := 0;  //Set file access to read only
		Reset(lHdrFile, 1);
		{$I+}
		if ioresult <> 0 then begin
			NSLog('Error in reading MRC header.'+inttostr(IOResult));
			FileMode := 2;
			exit;
		end;
		BlockRead(lHdrFile, h, sizeof(Tmrc));
		CloseFile(lHdrFile);
	end;
	//isModern := (h.cmap[1] = 'M') and (h.cmap[2] = 'A') and (h.cmap[3] = 'P');
	swapEndian := false;
	if ((h.stamp[1] = 68) or (h.stamp[1] = 65)) and ((h.stamp[2] = 68) or (h.stamp[2] = 65)) then begin
		{$IFDEF ENDIAN_BIG}
		swapEndian := true; //data is little-endian
		{$ENDIF}
	end else if (h.stamp[1] = 17) and (h.stamp[2] = 17) then begin
		{$IFDEF ENDIAN_LITTLE}
		swapEndian := true; //data is big-endian
		{$ENDIF}
	end else begin //endian not specified, e.g. demo iMOD file
		printf('MRC header machine stamp not specified.');
		if (h.mode > 101) or (h.nx > 16777215) or (h.ny > 16777215) or (h.nz > 16777215) then
			swapEndian := true;
	end;
	if swapEndian then begin
		swap4(h.nx);
		swap4(h.ny);
		swap4(h.nz);
		swap4(h.mode);
		pswap4r(h.xlen);
		pswap4r(h.ylen);
		pswap4r(h.zlen);
	end;
	if (h.mode = 0) then
		nhdr.datatype := kDT_INT8
	else if (h.mode = 1)  then
		nhdr.datatype := kDT_INT16
	else if (h.mode = 2)  then
		nhdr.datatype := kDT_FLOAT32
	else if (h.mode = 6)  then
		nhdr.datatype := kDT_INT16
	else begin
		  NSLog('Unsupported MRC mode: '+inttostr(h.mode));
		  exit;
	end;
	//NSLog(format('%d %d %d %d %d',[h.nx, h.ny, h.nz, h.mode, h.imodStamp]));
	if (h.nx < 1) or (h.ny < 1) or (h.nz < 1) then begin
		NSLog(format('MRC file does not make sense %dx%dx%d mode: %d', [h.nx, h.ny,h.nz, h.mode]));
		exit;
	end;
	nhdr.dim[1]:=h.nx;
	nhdr.dim[2]:=h.ny;
	nhdr.dim[3]:=h.nz;
	nhdr.pixdim[1] := h.xlen/h.nx;
	nhdr.pixdim[2] := h.ylen/h.ny;
	nhdr.pixdim[3] := h.zlen/h.nz;
	if h.zlen = 0 then nhdr.pixdim[3] := max(nhdr.pixdim[1], nhdr.pixdim[2]);
	nhdr.vox_offset := 1024;
        SetSForm(nhdr);
	convertForeignToNifti(nhdr);
	nhdr.descrip := 'MGH'+kIVers;
	result := true;
end; //readMRCHeader()

function readMGH(var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean; out xDim64: int64): boolean;
Type
  Tmgh = packed record //Next: MGH Format Header structure
   version, width,height,depth,nframes,mtype,dof : longint;
   goodRASFlag: smallint;
   spacingX,spacingY,spacingZ,xr,xa,xs,yr,ya,ys,zr,za,zs,cr,ca,cs: single;
  end;
var
  mgh: Tmgh;
  lBuff: Bytep;
  lExt: string;
  lHdrFile: file;
  PxyzOffset, Pcrs: vect4;
  i,j: integer;
  base: single;
  m: mat44;
begin
  result := false;
  FileMode := fmOpenRead;
  lExt := UpCaseExt(fname);
  if (lExt = '.MGZ') then begin
          lBuff := @mgh;
	  UnGZip(fname,lBuff,0,sizeof(Tmgh)); //1388
    gzBytes := K_gzBytes_headerAndImageCompressed;
  end else begin //if MGZ, else assume uncompressed MGH
     gzBytes := 0;
	   {$I-}
	   AssignFile(lHdrFile, fname);
	   FileMode := 0;  //Set file access to read only
	   Reset(lHdrFile, 1);
	   {$I+}
	   if ioresult <> 0 then begin
		  NSLog('Error in reading MGH header.'+inttostr(IOResult));
		  FileMode := 2;
		  exit;
	   end;
           mgh.version :=  128; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
	   BlockRead(lHdrFile, mgh, sizeof(Tmgh));
	   CloseFile(lHdrFile);
  end;
  {$IFDEF ENDIAN_BIG} //data always stored big endian
    swapEndian := false;
  {$ELSE}
  swapEndian := true;
  swap4(mgh.version);
  swap4(mgh.width);
  swap4(mgh.height);
  swap4(mgh.depth);
  swap4(mgh.nframes);
  swap4(mgh.mtype);
  swap4(mgh.dof);
  mgh.goodRASFlag := swap(mgh.goodRASFlag);
  pswap4r(mgh.spacingX);
  pswap4r(mgh.spacingY);
  pswap4r(mgh.spacingZ);
  pswap4r(mgh.xr);
  pswap4r(mgh.xa);
  pswap4r(mgh.xs);
  pswap4r(mgh.yr);
  pswap4r(mgh.ya);
  pswap4r(mgh.ys);
  pswap4r(mgh.zr);
  pswap4r(mgh.za);
  pswap4r(mgh.zs);
  pswap4r(mgh.cr);
  pswap4r(mgh.ca);
  pswap4r(mgh.cs);
  {$ENDIF}
  if ((mgh.version <> 1) or (mgh.mtype < 0) or (mgh.mtype > 4)) then begin
        NSLog(format('Error: first value in a MGH header should be 1 (got %d) and data type should be in the range 1..4. (got %d)', [mgh.version, mgh.mtype] ));
        exit;
  end;
  if (mgh.mtype = 0) then
        nhdr.datatype := kDT_UINT8
  else if (mgh.mtype = 4)  then
        nhdr.datatype := kDT_INT16
  else if (mgh.mtype = 1)  then
        nhdr.datatype := kDT_INT32
  else if (mgh.mtype = 3)  then
        nhdr.datatype := kDT_FLOAT32;
  xDim64 := mgh.Width;
  nhdr.dim[1]:=mgh.width;
  nhdr.dim[2]:=mgh.height;
  nhdr.dim[3]:=mgh.depth;
	nhdr.dim[4]:=mgh.nframes;
	nhdr.pixdim[1]:=mgh.spacingX;
	nhdr.pixdim[2]:=mgh.spacingY;
	nhdr.pixdim[3]:=mgh.spacingZ;
	nhdr.vox_offset := 284;
	nhdr.sform_code := 1;
	//convert MGH to NIfTI transform see Bruce Fischl mri.c MRIxfmCRS2XYZ https://github.com/neurodebian/freesurfer/blob/master/utils/mri.c
	LOAD_MAT44(m,mgh.xr*nhdr.pixdim[1],mgh.yr*nhdr.pixdim[2],mgh.zr*nhdr.pixdim[3],0,
               mgh.xa*nhdr.pixdim[1],mgh.ya*nhdr.pixdim[2],mgh.za*nhdr.pixdim[3],0,
			         mgh.xs*nhdr.pixdim[1],mgh.ys*nhdr.pixdim[2],mgh.zs*nhdr.pixdim[3],0);
  base := 0.0; //0 or 1: are voxels indexed from 0 or 1?
	Pcrs[0] := (nhdr.dim[1]/2.0)+base;
	Pcrs[1] := (nhdr.dim[2]/2.0)+base;
	Pcrs[2] := (nhdr.dim[3]/2.0)+base;
	Pcrs[3] := 1;
	for i:=0 to 3 do begin //multiply Pcrs * m
		PxyzOffset[i] := 0;
		for j := 0 to 3 do
			PxyzOffset[i] := PxyzOffset[i]+ (m[i,j]*Pcrs[j]);
	end;
  nhdr.srow_x[0]:=m[0,0]; nhdr.srow_x[1]:=m[0,1]; nhdr.srow_x[2]:=m[0,2]; nhdr.srow_x[3]:=mgh.cr - PxyzOffset[0];
	nhdr.srow_y[0]:=m[1,0]; nhdr.srow_y[1]:=m[1,1]; nhdr.srow_y[2]:=m[1,2]; nhdr.srow_y[3]:=mgh.ca - PxyzOffset[1];
	nhdr.srow_z[0]:=m[2,0]; nhdr.srow_z[1]:=m[2,1]; nhdr.srow_z[2]:=m[2,2]; nhdr.srow_z[3]:=mgh.cs - PxyzOffset[2];
  convertForeignToNifti(nhdr);
  nhdr.descrip := 'MGH'+kIVers;
  result := true;
end;

procedure splitStr(delimiter: char; str: string; mArray: TStrings);
begin
  mArray.Clear;
  mArray.Delimiter := delimiter;
  mArray.DelimitedText := str;
end;

procedure splitStrStrict(delimiter: char; S: string; sl: TStrings);
begin
  sl.Clear;
  sl.Delimiter := delimiter;
  sl.DelimitedText := '"' + StringReplace(S, sl.Delimiter, '"' + sl.Delimiter + '"', [rfReplaceAll]) + '"';
end;

function cleanStr (S:string): string; // "(12.31)" ->"12.31"
begin
  result := StringReplace(S, '(', '', [rfReplaceAll]);
  result := StringReplace(result, ')', '', [rfReplaceAll]);
end;

type TFByte =  File of Byte;

  function ReadLnBin(var f: TFByte; out s: string; skipEmptyLines: boolean = false): boolean;
  const
    kEOLN = $0A;
    kCR = $0D;
  var
     bt : Byte;
  begin
       s := '';
       if EOF(f) then exit(false);
       while (not  EOF(f)) do begin
             Read(f,bt);
             if (bt = kCR) then continue;
             if (bt = kEOLN) and ((not skipEmptyLines) or (s <> '' )) then exit(true);
             if (bt = kEOLN) then continue;
             s := s + Chr(bt);
       end;
       exit(true);
  end;

function readVTK(var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean): boolean;
//VTK Simple Legacy Formats : STRUCTURED_POINTS : BINARY
// http://daac.hpc.mil/gettingStarted/VTK_DataFormats.html
// https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bvtk/readVtk.m
// http://www.ifb.ethz.ch/education/statisticalphysics/file-formats.pdf
// ftp://ftp.tuwien.ac.at/visual/vtk/www/FileFormats.pdf
//  "The VTK data files described here are written in big endian form"
//Some VTK datasets seem to insert empty lines (e.g. 0x0A0A instead of 0x0A: "ironProt.vtk"
//   https://www.aliza-dicom-viewer.com/download/datasets
//   https://vtk.org/vtk-textbook-examples-and-data/
label
   666;
var
   f: TFByte;
   strlst: TStringList;
   str: string;
   i, num_vox: integer;
   ok: boolean;
begin
  gzBytes := 0;
  {$IFDEF ENDIAN_BIG}
  swapEndian := false;
  {$ELSE}
  swapEndian := true;
  {$ENDIF}
  result := false;
  strlst:=TStringList.Create;
  AssignFile(f, fname);
  FileMode := fmOpenRead;
  {$IFDEF FPC} Reset(f,1); {$ELSE} Reset(f); {$ENDIF}
  ReadLnBin(f, str); //signature: '# vtk DataFile'
  if (pos('<?XML VERSION=', UpperCase(str)) > 0) or (pos('<VTKFile"', str) = 1) then begin
     NSLog('Only able to read legacy VTK files, not XML files "'+fname+'"');
     goto 666;
  end;
  if pos('VTK', UpperCase(str)) <> 3 then begin
    NSLog('Not a VTK file');
    goto 666;
  end;
  ReadLnBin(f, str); //comment: 'Comment: created with MRIcroS'
  ReadLnBin(f, str, true); //kind: 'BINARY' or 'ASCII'
  if (pos('ASCII', UpperCase(str)) > 0) then begin  // '# vtk DataFile'
	  nhdr.regular := 'A';//ASCII
  end else  if (pos('BINARY', UpperCase(str)) < 1)  then begin  // '# vtk DataFile'
     NSLog('Only able to read ASCII or BINARY VTK files, not "'+str+'"');
     goto 666;
  end;
  ReadLnBin(f, str, true); // kind, e.g. "DATASET POLYDATA" or "DATASET STRUCTURED_ POINTS"
  if pos('STRUCTURED_POINTS', UpperCase(str)) = 0 then begin
    NSLog('Only able to read VTK images saved as STRUCTURED_POINTS (hint: try Slicer or Surfice), not '+ str);
    goto 666;
  end;
  //while (str <> '') and (pos('POINT_DATA', UpperCase(str)) = 0) do begin
  ok := true;
  while (ok) and (pos('POINT_DATA', UpperCase(str)) = 0) do begin
    ok := ReadLnBin(f, str, true);
    strlst.DelimitedText := str;
    if pos('DIMENSIONS', UpperCase(str)) <> 0 then begin //e.g. "DIMENSIONS 128 128 128"
       nhdr.dim[1] := StrToIntDef(strlst[1],1);
       nhdr.dim[2] := StrToIntDef(strlst[2],1);
       nhdr.dim[3] := StrToIntDef(strlst[3],1);
    end; //dimensions
    if (pos('ASPECT_RATIO', UpperCase(str)) <> 0) or (pos('SPACING', UpperCase(str)) <> 0) then begin //e.g. "ASPECT_RATIO 1.886 1.886 1.913"
      nhdr.pixdim[1] := StrToFloatDef(strlst[1],1);
      nhdr.pixdim[2] := StrToFloatDef(strlst[2],1);
      nhdr.pixdim[3] := StrToFloatDef(strlst[3],1);
      //showmessage(format('%g %g %g',[nhdr.pixdim[1], nhdr.pixdim[2], nhdr.pixdim[3] ]));
    end; //aspect ratio
    if (pos('ORIGIN', UpperCase(str)) <> 0) then begin //e.g. "ASPECT_RATIO 1.886 1.886 1.913"
      nhdr.srow_x[3] := -StrToFloatDef(strlst[1],1);
      nhdr.srow_y[3] := -StrToFloatDef(strlst[2],1);
      nhdr.srow_z[3] := -StrToFloatDef(strlst[3],1);
      //showmessage(format('%g %g %g',[nhdr.pixdim[1], nhdr.pixdim[2], nhdr.pixdim[3] ]));
    end; //aspect ratio
  end; //not POINT_DATA
  if pos('POINT_DATA', UpperCase(str)) = 0 then goto 666;
  num_vox :=  StrToIntDef(strlst[1],0);
  if num_vox <> (nhdr.dim[1] * nhdr.dim[2] * nhdr.dim[3]) then begin
     NSLog(format('Expected POINT_DATA to equal %dx%dx%d',[nhdr.dim[1], nhdr.dim[2], nhdr.dim[3] ]));
     goto 666;
  end;
  ReadLnBin(f, str, true);
  if pos('SCALARS', UpperCase(str)) = 0 then goto 666; //"SCALARS scalars unsigned_char"
  strlst.DelimitedText := str;
  str := UpperCase(strlst[2]);
  //dataType is one of the types bit, unsigned_char, char, unsigned_short, short, unsigned_int, int, unsigned_long, long, float, or double
  if pos('UNSIGNED_CHAR', str) <> 0 then
      nhdr.datatype := kDT_UINT8 //
  else if pos('SHORT', str) <> 0 then
       nhdr.datatype := kDT_INT16 //
  else if pos('UNSIGNED_SHORT', str) <> 0 then
       nhdr.datatype := kDT_UINT16 //
  else if pos('INT', str) <> 0 then
       nhdr.datatype := kDT_INT32 //
  else  if pos('FLOAT', str) <> 0 then
      nhdr.datatype := kDT_FLOAT
  else  if pos('DOUBLE', str) <> 0 then
      nhdr.datatype := kDT_DOUBLE
  else begin
        NSLog('Unknown VTK scalars type '+str);
        goto 666;
  end;
  convertForeignToNifti(nhdr);
  //showmessage(inttostr(nhdr.datatype));
  ReadLnBin(f, str);
  if pos('LOOKUP_TABLE', UpperCase(str)) = 0 then goto 666; //"LOOKUP_TABLE default"
  nhdr.vox_offset := filepos(f);
  //fill matrix
  for i := 0 to 2 do begin
    nhdr.srow_x[i] := 0;
    nhdr.srow_y[i] := 0;
    nhdr.srow_z[i] := 0;
  end;
  nhdr.srow_x[0] := nhdr.pixdim[1];
  nhdr.srow_y[1] := nhdr.pixdim[2];
  nhdr.srow_z[2] := nhdr.pixdim[3];
  //showmessage('xx' +inttostr( filepos(f) ));
  result := true;
  666:
  closefile(f);
  strlst.Free;
  nhdr.descrip := 'VTK'+kIVers;
end;

function readMHA(var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean): boolean;
//Read VTK "MetaIO" format image
//http://www.itk.org/Wiki/ITK/MetaIO/Documentation#Reading_a_Brick-of-Bytes_.28an_N-Dimensional_volume_in_a_single_file.29
//https://www.assembla.com/spaces/plus/wiki/Sequence_metafile_format
//http://itk-insight-users.2283740.n2.nabble.com/MHA-MHD-File-Format-td7585031.html
var
  FP: TextFile;
  str, tagName, elementNames: string;
  ch: char;
  isLocal,compressedData: boolean;
  matOrient, mat, d, t: mat33;
  //compressedDataSize,
  nPosition, nOffset, matElements, matElementsOrient,  headerSize, nItems, nBytes, i, channels, fileposBytes: longint;
  //elementSize,
  offset,position: array [0..3] of single;
  transformMatrix: array [0..11] of single;
  mArray: TStringList;
begin
  result := false;
  if not FileExists(fname) then exit;
    {$IFDEF FPC}
  DefaultFormatSettings.DecimalSeparator := '.' ;
   // DecimalSeparator := '.';
  {$ELSE}
  DecimalSeparator := '.';
  {$ENDIF}
  for i := 0 to 3 do begin
      position[i] := 0;
      offset[i] := 0;
      //elementSize[i] := 1;
  end;
  ZERO_MAT33(matOrient);
  ZERO_MAT33(mat);
  for i := 0 to 11 do
      transformMatrix[i] := 0;
  nPosition := 0;
  nOffset := 0;
  gzBytes := 0;
  fileposBytes := 0;
  //compressedDataSize := 0;
  swapEndian := false;
  isLocal := true; //image and header embedded in same file, if false detached image
  headerSize := 0;
  matElements := 0;
  matElementsOrient := 0;
  compressedData := false;
  mArray := TStringList.Create;
  Filemode := fmOpenRead;
  AssignFile(fp,fname);
  reset(fp);
  while not EOF(fp) do begin
    str := '';
    while not EOF(fp) do begin
      read(fp,ch);
      inc(fileposBytes);
      if (ch = chr($0D)) or (ch = chr($0A)) then break;
      str := str+ch;
    end;
    if (length(str) < 1) or (str[1]='#') then continue;
    splitstrStrict('=',str,mArray);
    if (mArray.count < 2) then continue;
    tagName := cleanStr(mArray[0]);
    elementNames := mArray[1];
    splitstr(',',elementNames,mArray);
    nItems :=mArray.count;
    if (nItems < 1) then continue;
    for i := 0 to (nItems-1) do
      mArray[i] := cleanStr(mArray[i]); //remove '(' and ')',
    if AnsiContainsText(tagName, 'ObjectType') and (not AnsiContainsText(mArray.Strings[0], 'Image')) then begin
        NSLog('Expecting file with tag "ObjectType = Image" instead of "ObjectType = '+mArray.Strings[0]+'"');

    end {else if AnsiContainsText(tagName, 'NDims') then begin
            nDims := strtoint(mArray[0]);
            if (nDims > 4) then begin
                NSLog('Warning: only reading first 4 dimensions');
                nDims := 4;
            end;
    end} else if AnsiContainsText(tagName, 'BinaryDataByteOrderMSB') then begin
            {$IFDEF ENDIAN_BIG} //data always stored big endian
            if not AnsiContainsText(mArray[0], 'True') then swapEndian := true;
            {$ELSE}
            if AnsiContainsText(mArray[0], 'True') then swapEndian := true;
            {$ENDIF}
    end {else if AnsiContainsText(tagName, 'BinaryData') then begin
            if AnsiContainsText(mArray[0], 'True') then binaryData := true;
    end else if AnsiContainsText(tagName, 'CompressedDataSize') then begin
            compressedDataSize := strtoint(mArray[0]);
        end} else if AnsiContainsText(tagName, 'CompressedData') then begin
            if AnsiContainsText(mArray[0], 'True') then
                compressedData := true;
        end  else if AnsiContainsText(tagName, 'Orientation') and (not AnsiContainsText(tagName, 'Anatomical') ) then begin
            if (nItems > 12) then nItems := 12;
            matElementsOrient := nItems;
            for i := 0 to (nItems-1) do
              transformMatrix[i] :=  strtofloat(mArray[i]);
            if (matElementsOrient >= 12) then
                LOAD_MAT33(matOrient, transformMatrix[0],transformMatrix[1],transformMatrix[2],
                           transformMatrix[4],transformMatrix[5],transformMatrix[6],
                           transformMatrix[8],transformMatrix[9],transformMatrix[10])
            else if (matElementsOrient >= 9) then
                LOAD_MAT33(matOrient, transformMatrix[0],transformMatrix[1],transformMatrix[2],
                           transformMatrix[3],transformMatrix[4],transformMatrix[5],
                           transformMatrix[6],transformMatrix[7],transformMatrix[8]);

        end else if AnsiContainsText(tagName, 'TransformMatrix') then begin
            if (nItems > 12) then nItems := 12;
            matElements := nItems;
            for i := 0 to (nItems-1) do
              transformMatrix[i] :=  strtofloat(mArray[i]);
            if (matElements >= 12) then
                LOAD_MAT33(mat, transformMatrix[0],transformMatrix[1],transformMatrix[2],
                           transformMatrix[4],transformMatrix[5],transformMatrix[6],
                           transformMatrix[8],transformMatrix[9],transformMatrix[10])
            else if (matElements >= 9) then
                LOAD_MAT33(mat, transformMatrix[0],transformMatrix[1],transformMatrix[2],
                           transformMatrix[3],transformMatrix[4],transformMatrix[5],
                           transformMatrix[6],transformMatrix[7],transformMatrix[8]);
        end else if AnsiContainsText(tagName, 'Position') then begin
            if (nItems > 3) then nItems := 3;
            nPosition := nItems;
            for i := 0 to (nItems-1) do
              position[i] :=  strtofloat(mArray[i]);
        end else if AnsiContainsText(tagName, 'Offset') then begin
            if (nItems > 3) then nItems := 3;
            nOffset := nItems;
            for i := 0 to (nItems-1) do
              offset[i] :=  strtofloat(mArray[i]);
        end else if AnsiContainsText(tagName, 'AnatomicalOrientation') then begin
            //e.g. RAI
        end else if AnsiContainsText(tagName, 'ElementSpacing') then begin
            if (nItems > 4) then nItems := 4;
            for i := 0 to (nItems-1) do
                nhdr.pixdim[i+1] := strtofloat(mArray[i]);
        end else if AnsiContainsText(tagName, 'DimSize') then begin
            if (nItems > 4) then nItems := 4;
            for i := 0 to (nItems-1) do
                nhdr.dim[i+1] :=  strtoint(mArray[i]);
        end else if AnsiContainsText(tagName, 'HeaderSize') then begin
            headerSize := strtoint(mArray[0]);
        end else if AnsiContainsText(tagName, 'ElementSize') then begin
            //if (nItems > 4) then nItems := 4;
            //for i := 0 to (nItems-1) do
            //    elementSize[i] := strtofloat(mArray[i]);
        end else if AnsiContainsText(tagName, 'ElementNumberOfChannels') then begin
            channels := strtoint(mArray[0]);
            if (channels > 1) then NSLog('Unable to read MHA/MHD files with multiple channels ');
        end else if AnsiContainsText(tagName, 'ElementByteOrderMSB') then begin
            {$IFDEF ENDIAN_BIG} //data always stored big endian
            if not AnsiContainsText(mArray[0], 'True') then swapEndian := true;
            {$ELSE}
            if AnsiContainsText(mArray[0], 'True') then swapEndian := true;
            {$ENDIF}
        end else if AnsiContainsText(tagName, 'ElementType') then begin
            //convert metaImage format to NIfTI http://portal.nersc.gov/svn/visit/tags/2.2.1/vendor_branches/vtk/src/IO/vtkMetaImageWriter.cxx
            //set NIfTI datatype http://nifti.nimh.nih.gov/pub/dist/src/niftilib/nifti1.h
            if AnsiContainsText(mArray[0], 'MET_UCHAR') then
                nhdr.datatype := kDT_UINT8 //
            else if AnsiContainsText(mArray[0], 'MET_CHAR') then
                nhdr.dataType := kDT_INT8 //
            else if AnsiContainsText(mArray[0], 'MET_SHORT') then
                nhdr.dataType := kDT_INT16 //
            else if AnsiContainsText(mArray[0], 'MET_USHORT') then
                nhdr.dataType := kDT_UINT16 //
            else if AnsiContainsText(mArray[0], 'MET_INT') then
                nhdr.dataType := kDT_INT32 //DT_INT32
            else if AnsiContainsText(mArray[0], 'MET_UINT') then
                nhdr.dataType := kDT_UINT32 //DT_UINT32
            else if AnsiContainsText(mArray[0], 'MET_ULONG') then
                nhdr.dataType := kDT_UINT64 //DT_UINT64
            else if AnsiContainsText(mArray[0], 'MET_LONG') then
                nhdr.dataType := kDT_INT64 //DT_INT64
            else if AnsiContainsText(mArray[0], 'MET_FLOAT') then
                nhdr.dataType := kDT_FLOAT32 //DT_FLOAT32
            else if AnsiContainsText(mArray[0], 'MET_DOUBLE') then
                nhdr.dataType := kDT_DOUBLE; //DT_FLOAT64
        end else if AnsiContainsText(tagName, 'ElementDataFile') then begin
            if not AnsiContainsText(mArray[0], 'local') then begin
                str := mArray.Strings[0];
                if fileexists(str) then
                  fname := str
                else begin
                  fname := ExtractFilePath(fname)+str;
                end;
                isLocal := false;
            end;
            break;
        end;
  end; //while reading
  if (headerSize = 0) and (isLocal) then headerSize :=fileposBytes; //!CRAP 2015
  nhdr.vox_offset := headerSize;
  CloseFile(FP);
  Filemode := 2;
  mArray.free;
  //convert transform
  if (matElements >= 9) or (matElementsOrient >= 9) then begin
    //report_Mat(matOrient);
    LOAD_MAT33(d,  nhdr.pixdim[1],0,0,
                 0, nhdr.pixdim[2],0,
                 0,0, nhdr.pixdim[3]);
      if (matElements >= 9) then
         t := nifti_mat33_mul( d, mat)
      else
          t := nifti_mat33_mul( d, matOrient) ;
      if nPosition > nOffset then begin
          offset[0] := position[0];
          offset[1] := position[1];
          offset[2] := position[2];

      end;
      nhdr.srow_x[0] := -t[0,0];
      nhdr.srow_x[1] := -t[1,0];
      nhdr.srow_x[2] := -t[2,0];
      nhdr.srow_x[3] := -offset[0];
      nhdr.srow_y[0] := -t[0,1];
      nhdr.srow_y[1] := -t[1,1];
      nhdr.srow_y[2] := -t[2,1];
      nhdr.srow_y[3] := -offset[1];
      nhdr.srow_z[0] := t[0,2];
      nhdr.srow_z[1] := t[1,2];
      nhdr.srow_z[2] := t[2,2];
      nhdr.srow_z[3] := offset[2];
  end else begin
      //NSLog('Warning: unable to determine image orientation (unable to decode metaIO "TransformMatrix" tag)')};
      (*nhdr.sform_code:=0;
      nhdr.srow_x[0] := 0;
      nhdr.srow_x[1] := 0;
      nhdr.srow_x[2] := 0;  *)
      SetSForm(nhdr);
  end;
  //end transform
  convertForeignToNifti(nhdr);
  {$IFDEF UNIX} writeln(format('MHA format offset=%d bpp=%d %dx%dx%d', [round(nhdr.vox_offset), nhdr.bitpix, nhdr.dim[1], nhdr.dim[2], nhdr.dim[3]]));{$ENDIF}
  if (compressedData) then
      gzBytes := K_gzBytes_onlyImageCompressed;
  if (nhdr.vox_offset < 0) then begin
      nBytes := (nhdr.bitpix div 8);
      for i := 1 to 7 do begin
          if nhdr.dim[i] > 0 then
              nBytes := nBytes * nhdr.dim[i];
      end;
      nhdr.vox_offset := FSize(fname) - nBytes;
      if (nhdr.vox_offset < 0) then nhdr.vox_offset := -1;
  end;
  result := true;
    nhdr.descrip := 'MHA'+kIVers;
end;//MHA
//{$DEFINE DECOMPRESSGZ}
{$IFDEF DECOMPRESSGZ}
function readMIF(var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian, isDimPermute2341: boolean): boolean;
//https://github.com/MRtrix3/mrtrix3/blob/master/matlab/read_mrtrix.m
//https://mrtrix.readthedocs.io/en/latest/getting_started/image_data.html
//https://mrtrix.readthedocs.io/en/latest/getting_started/image_data.html#the-image-transfom
//https://github.com/MRtrix3/mrtrix3/blob/52a2540d7d3158ec74d762ad5dd387777569f325/core/file/nifti1_utils.cpp
label
  666;
{$IFDEF GZIP}
const
  kGzSz=65536;
{$ENDIF}
var
  FP: TextFile;
  str, key, vals, fstr: string;
  mArray: TStringList;
  nTransforms, nItems, i, j, k, nDim : integer;
  repetitionTime: single;
  layout: array [1..7] of double;
  pixdim: array [1..7] of single;
  dim: array [1..7] of integer;
  m: Mat44;
  m33: mat33;
  originVox, originMM: vect3;
  {$IFDEF GZIP}
  //the GZ MIF header is trouble: unlike NIfTI it is variable size, unlike NRRD it is part of the compressed stream
  // here the kludge is to extract the ENTIRE image to disk in order to read the header.
  // optimal would be to read a memory stream and detect '\nEND\n' when decompressing...
  // however, this format is discouraged so for the moment this seems sufficient
  fnameGZ: string = '';
  zStream: TGZFileStream;
  dStream: TFileStream;
  bytes : array of byte;
  bytescopied: integer;
  {$ENDIF}
begin
  str := UpCaseExt(fname);
  if str = '.GZ' then begin
     fstr := fname;
     {$IFDEF GZIP}
     fname := changefileext(fstr,'');
     if not fileexists(fname) then begin
        fnameGZ := fstr;
        zStream := TGZFileStream.Create(fstr,gzOpenRead);
        dStream := TFileStream.Create(fname,fmOpenWrite or fmCreate );
        setlength(bytes, kGzSz);
        repeat
              bytescopied := zStream.read(bytes[0],kGzSz);
              dStream.Write(bytes[0],bytescopied) ;
        until bytescopied < kGzSz;
        dStream.Free;
        zStream.Free;
		bytes := nil;
     end;
     {$ELSE}
     showmessage('Unable to decompress .MIF.GZ');
     exit;
     {$ENDIF}
  end;
  swapEndian :=false;
  result := false;
  for i := 1 to 7 do begin
      layout[i] := i;
      dim[i] := 1;
      pixdim[i] := 1.0;
  end;
  repetitionTime := 0.0;
  LOAD_MAT44(m,1,0,0,0, 0,1,0,0, 0,0,1,0);
  nTransforms := 0;
  FileMode := fmOpenRead;
  AssignFile(fp,fname);
  reset(fp);
  mArray := TStringList.Create;
  if EOF(fp) then goto 666;
  readln(fp,str);
  if str <> 'mrtrix image' then goto 666;
  while (not EOF(fp))  do begin
    readln(fp,str);
    if str = 'END' then break;
    splitstrStrict(':',str,mArray);
    if mArray.count < 2 then continue;
    key := mArray[0]; //e.g. "dim: 1,2,3" -> "dim"
    vals := mArray[1]; //e.g. "dim: 1,2,3" -> "1,2,3"
    splitstrStrict(',',vals,mArray);
    nItems := mArray.count;
    mArray[0] := Trim(mArray[0]);  //" Float32LE" -> "Float32LE"
    //str := mArray[0];
    //mArray.Delete(i);
    if (ansipos('RepetitionTime', key) = 1) and (nItems > 0)  then begin
      repetitionTime := strtofloatdef(mArray[0], 0);
      continue;
    end;
    if (ansipos('layout', key) = 1) and (nItems > 1) and (nItems < 7) then begin
      for i := 1 to nItems do begin
          layout[i] := strtofloatdef(mArray[i-1],i);
          if (mArray[i-1][1] = '-') and (layout[i] >= 0) then
             layout[i] := -0.00001;
      end;
      continue;
    end;
    if (ansipos('transform', key) = 1) and (nItems > 1) and (nItems < 5) and (nTransforms < 3) then begin
      for i := 0 to (nItems-1) do
          m[nTransforms,i] := strtofloatdef(mArray[i],i);
      nTransforms := nTransforms + 1;
      continue;
    end;
    if (ansipos('dim', key) = 1) and (nItems > 1) and (nItems < 7) then begin
      nDim := nItems;
      for i := 1 to nItems do
           dim[i] := strtointdef(mArray[i-1],0);
       continue;
    end;
    if (ansipos('scaling', key) = 1) and (nItems > 1) and (nItems < 7) then begin
        nhdr.scl_inter := strtofloatdef(mArray[0],0);
        nhdr.scl_slope := strtofloatdef(mArray[1],1);
    end;
    if (ansipos('vox', key) = 1) and (nItems > 1) and (nItems < 7) then begin
       //NSLog('BINGO'+mArray[0]);
       for i := 1 to nItems do
           pixdim[i] := strtofloatdef(mArray[i-1],0);
           //nhdr.pixdim[i] := strtofloatdef(mArray[i-1],0);
       continue;
    end;
    if (ansipos('datatype', key) = 1) and (nItems > 0) then begin
      if (ansipos('Int8', mArray[0]) = 1) then
         nhdr.datatype := kDT_INT8
      else if (ansipos('UInt8', mArray[0]) = 1) then
         nhdr.datatype := kDT_UINT8
      else if (ansipos('UInt16', mArray[0]) = 1) then
            nhdr.datatype := kDT_UINT16
      else if (ansipos('Int16', mArray[0]) = 1) then
        nhdr.datatype := kDT_INT16
      else if (ansipos('Float32', mArray[0]) = 1) then
         nhdr.datatype := kDT_FLOAT32
      else
         NSLog('unknown datatype '+mArray[0]+' '+inttostr(ansipos('Float32LX', mArray[0])));
      {$IFDEF ENDIAN_BIG}
      if (ansipos('LE', mArray[0]) > 0) then
         swapEndian :=true;
      {$ELSE}
      if (ansipos('BE', mArray[0]) > 0) then
         swapEndian :=true;
      {$ENDIF}
      continue;

    end;
    if (ansipos('file', key) = 1) and (nItems > 0) then begin
       fstr := trim(copy(str,pos(':',str)+1, maxint)); //get full string, e.g. "file: with spaces.dat"
       splitstrStrict(' ',mArray[0],mArray);
       nItems :=mArray.count;
       if (nItems > 1) and (mArray[0] = '.') then
          nhdr.vox_offset := strtointdef(mArray[1],0) //"file: . 328" -> 328 *)
       else begin
           if not fileexists(fstr) then //e.g. "out.dat" -> "\mydir\out.dat"
              fname := ExtractFilePath(fname) + fstr
           else
               fname := fstr;
       end;
       continue;
    end;
    //NSLog(format('%d "%s" %d',[ansipos('file', key) , key, nItems]));
  end;
  //https://github.com/MRtrix3/mrtrix3/blob/52a2540d7d3158ec74d762ad5dd387777569f325/core/file/nifti_utils.cpp
  // transform_type adjust_transform (const Header& H, vector<size_t>& axes)
  for i := 0 to 2 do
      originVox[0] := 0;
  if nDim < 2 then goto 666;
  nhdr.dim[0] := nDim;
  LOAD_MAT33(m33,1,0,0, 0,1,0, 0,0,1);
  for i := 1 to nDim do begin
      j := abs(round(layout[i]))+1;
      nhdr.dim[j] := dim[i];
      if specialsingle(pixdim[i]) then
         pixdim[i] := 0.0;
      nhdr.pixdim[j] := pixdim[i];
      if j = 4 then
         nhdr.pixdim[j] := repetitionTime;
      if i > 3 then continue;
      //for k := 0 to 2 do
      //    m33[k, j-1] :=  m[i-1,k];
      //for k := 0 to 2 do
      //    m33[i-1, k] :=  m[i-1,k];
      for k := 0 to 2 do
          m33[k,j-1] :=  m[k,i-1];

      //rot33[j-1,i-1] := nhdr.pixdim[j];
      if layout[i] < 0 then begin
         nhdr.pixdim[j] := -pixdim[i];
         originVox[j-1] := dim[i]-1;
      end;
  end;
  //scale matrix
  for i := 0 to 2 do
      for j := 0 to 2 do
          m33[j,i] := m33[j,i] * nhdr.pixdim[i+1];
  originMM := nifti_mat33vec_mul(m33, originVox);
  for i := 1 to 3 do
      nhdr.pixdim[i] := abs(nhdr.pixdim[i]);
  for i := 0 to 2 do
    m[i,3] := m[i,3] - originMM[i];
  (*
  str := format('%g %g %g', [pixdim[1], pixdim[2], pixdim[3]]);
  str := format('m = [%g %g %g; %g %g %g; %g %g %g]',[
        m33[0,0], m33[0,1], m33[0,2],
        m33[1,0], m33[1,1], m33[1,2],
        m33[2,0], m33[2,1], m33[2,2]]);
  str := format('%g %g %g', [originVox[0], originVox[1], originVox[2]]);
  str := format('v = [%g %g %g]', [originMM[0], originMM[1], originMM[2]]);
  Clipboard.AsText := str; *)

    nhdr.srow_x[0] := m33[0,0];
    nhdr.srow_x[1] := m33[0,1];
    nhdr.srow_x[2] := m33[0,2];
    nhdr.srow_x[3] :=   m[0,3];
    nhdr.srow_y[0] := m33[1,0];
    nhdr.srow_y[1] := m33[1,1];
    nhdr.srow_y[2] := m33[1,2];
    nhdr.srow_y[3] :=   m[1,3];
    nhdr.srow_z[0] := m33[2,0];
    nhdr.srow_z[1] := m33[2,1];
    nhdr.srow_z[2] := m33[2,2];
    nhdr.srow_z[3] :=   m[2,3];
  (*str := (format('m = [%g %g %g %g; %g %g %g %g; %g %g %g %g; 0 0 0 1]',[
    nhdr.srow_x[0], nhdr.srow_x[1], nhdr.srow_x[2], nhdr.srow_x[3],
    nhdr.srow_y[0], nhdr.srow_y[1], nhdr.srow_y[2], nhdr.srow_y[3],
    nhdr.srow_z[0], nhdr.srow_z[1], nhdr.srow_z[2], nhdr.srow_z[3]]));
  Clipboard.AsText := str; *)
  convertForeignToNifti(nhdr);
  result := true;
666:
    CloseFile(FP);
    Filemode := 2;
    {$IFDEF GZIP}
    if (fnameGZ <> '') and (fileexists(fnameGZ)) then begin
       deletefile(fname);
       fname := fnameGZ;
       gzBytes := K_gzBytes_headerAndImageCompressed;
    end;

    {$ENDIF}
    mArray.Free;
end; //readMIF()
{$ELSE}
function StreamNullStrRaw(Stream: TFileStream): string;
var
  b: byte;
begin
  result := '';
  while (Stream.Position < Stream.Size) do begin
        b := Stream.ReadByte;
        if b = $0A then exit;
        if b = $0D then continue;
        result := result + chr(b);
  end;
end;

(*function StreamNullStrGz(Stream: TGZFileStream): string;
var
  b: byte;
begin
  result := '';
  while (true) do begin
        b := Stream.ReadByte;
        if b = $0A then exit;
        if b = $00 then exit;
        if b = $0D then continue;
        result := result + chr(b);
  end;
end;*)

function StreamNullStrGz(Stream: TGZFileStream): string;
var
  n: integer;
  b: array [0..0] of byte;
begin
  result := '';
  b[0] := $0A;
  while (true) do begin
        n := Stream.read(b,1);
        if n < 1 then break;
        if b[0] = $0A then exit;
        if b[0] = $00 then exit;
        if b[0] = $0D then continue;
        result := result + chr(b[0]);
  end;
  if n < 1 then result := 'END';
end;


function readMIF(var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean): boolean;
//https://github.com/MRtrix3/mrtrix3/blob/master/matlab/read_mrtrix.m
//https://mrtrix.readthedocs.io/en/latest/getting_started/image_data.html
//https://mrtrix.readthedocs.io/en/latest/getting_started/image_data.html#the-image-transfom
//https://github.com/MRtrix3/mrtrix3/blob/52a2540d7d3158ec74d762ad5dd387777569f325/core/file/nifti1_utils.cpp
label
  666;
var
  str, key, vals, fstr: string;
  mArray: TStringList;
  nTransforms, nItems, i, j, k, nDim : integer;
  repetitionTime: single;
  layout: array [1..7] of double;
  pixdim: array [1..7] of single;
  dim: array [1..7] of integer;
  m: Mat44;
  m33: mat33;
  originVox, originMM: vect3;
  fs: TFileStream;
  {$IFDEF GZIP}
  zfs: TGZFileStream;
  isGz: boolean = false;
  {$ENDIF}
begin
  str := UpCaseExt(fname);
  if str = '.GZ' then begin
     {$IFDEF GZIP}
     isGz := true;
     zfs := TGZFileStream.Create(fname,gzOpenRead);
     {$ELSE}
     showmessage('Unable to decompress .MIF.GZ');
     exit;
     {$ENDIF}
  end;
  nDim := 0;
  fs := nil;
  zfs := nil;
  ZERO_MAT44(m);
  swapEndian :=false;
  result := false;
  for i := 1 to 7 do begin
      layout[i] := i;
      dim[i] := 1;
      pixdim[i] := 1.0;
  end;
  repetitionTime := 0.0;
  LOAD_MAT44(m,1,0,0,0, 0,1,0,0, 0,0,1,0);
  nTransforms := 0;
  mArray := TStringList.Create;
  if isGz then
     str := StreamNullStrGz(zfs)
  else begin
       fs := TFileStream.Create(fname, fmOpenRead);
       str := StreamNullStrRaw(fs);
  end;
  if str <> 'mrtrix image' then goto 666;
  while (isGz ) or ((not isGz) and (fs.position < fs.Size))  do begin
    if isGz then
       str := StreamNullStrGz(zfs)
    else
        str := StreamNullStrRaw(fs);
    if str = 'END' then break;
    splitstrStrict(':',str,mArray);
    if mArray.count < 2 then continue;
    key := mArray[0]; //e.g. "dim: 1,2,3" -> "dim"
    vals := mArray[1]; //e.g. "dim: 1,2,3" -> "1,2,3"
    splitstrStrict(',',vals,mArray);
    nItems := mArray.count;
    mArray[0] := Trim(mArray[0]);  //" Float32LE" -> "Float32LE"
    if (ansipos('RepetitionTime', key) = 1) and (nItems > 0)  then begin
      repetitionTime := strtofloatdef(mArray[0], 0);
      continue;
    end;
    if (ansipos('layout', key) = 1) and (nItems > 1) and (nItems < 7) then begin
      for i := 1 to nItems do begin
          layout[i] := strtofloatdef(mArray[i-1],i);
          if (mArray[i-1][1] = '-') and (layout[i] >= 0) then
             layout[i] := -0.00001;
          if (i <= 3) and (abs(layout[i]) >= 3) then begin
              NSLog('The first three strides are expected to be spatial (check for update).');
              goto 666;
          end;
      end;
      continue;
    end;
    if (ansipos('transform', key) = 1) and (nItems > 1) and (nItems < 5) and (nTransforms < 3) then begin
      for i := 0 to (nItems-1) do
          m[nTransforms,i] := strtofloatdef(mArray[i],i);
      nTransforms := nTransforms + 1;
      continue;
    end;
    if (ansipos('dim', key) = 1) and (nItems > 1) and (nItems < 7) then begin
      nDim := nItems;
      for i := 1 to nItems do
           dim[i] := strtointdef(mArray[i-1],0);
       continue;
    end;
    if (ansipos('scaling', key) = 1) and (nItems > 1) and (nItems < 7) then begin
        nhdr.scl_inter := strtofloatdef(mArray[0],0);
        nhdr.scl_slope := strtofloatdef(mArray[1],1);
    end;
    if (ansipos('vox', key) = 1) and (nItems > 1) and (nItems < 7) then begin
       //NSLog('BINGO'+mArray[0]);
       for i := 1 to nItems do
           pixdim[i] := strtofloatdef(mArray[i-1],0);
           //nhdr.pixdim[i] := strtofloatdef(mArray[i-1],0);
       continue;
    end;
    if (ansipos('datatype', key) = 1) and (nItems > 0) then begin
      if (ansipos('Int8', mArray[0]) = 1) then
         nhdr.datatype := kDT_INT8
      else if (ansipos('UInt8', mArray[0]) = 1) then
         nhdr.datatype := kDT_UINT8
      else if (ansipos('UInt16', mArray[0]) = 1) then
            nhdr.datatype := kDT_UINT16
      else if (ansipos('Int16', mArray[0]) = 1) then
        nhdr.datatype := kDT_INT16
      else if (ansipos('Float32', mArray[0]) = 1) then
         nhdr.datatype := kDT_FLOAT32
      else
         NSLog('unknown datatype '+mArray[0]);
      {$IFDEF ENDIAN_BIG}
      if (ansipos('LE', mArray[0]) > 0) then
         swapEndian :=true;
      {$ELSE}
      if (ansipos('BE', mArray[0]) > 0) then
         swapEndian :=true;
      {$ENDIF}
      continue;
    end;
    if (ansipos('file', key) = 1) and (nItems > 0) then begin
       fstr := trim(copy(str,pos(':',str)+1, maxint)); //get full string, e.g. "file: with spaces.dat"
       splitstrStrict(' ',mArray[0],mArray);
       nItems :=mArray.count;
       if (nItems > 1) and (mArray[0] = '.') then
          nhdr.vox_offset := strtointdef(mArray[1],0) //"file: . 328" -> 328
       else begin
           if not fileexists(fstr) then //e.g. "out.dat" -> "\mydir\out.dat"
              fname := ExtractFilePath(fname) + fstr
           else
               fname := fstr;
       end;
       continue;
    end;
  end;
  //https://github.com/MRtrix3/mrtrix3/blob/52a2540d7d3158ec74d762ad5dd387777569f325/core/file/nifti_utils.cpp
  // transform_type adjust_transform (const Header& H, vector<size_t>& axes)
  for i := 0 to 2 do
      originVox[0] := 0;
  if nDim < 2 then goto 666;
  nhdr.dim[0] := nDim;
  LOAD_MAT33(m33,1,0,0, 0,1,0, 0,0,1);
  for i := 1 to nDim do begin
      j := abs(round(layout[i]))+1;
      nhdr.dim[j] := dim[i];
      if specialsingle(pixdim[i]) then
         pixdim[i] := 0.0;
      nhdr.pixdim[j] := pixdim[i];
      if j = 4 then
         nhdr.pixdim[j] := repetitionTime;
      if i > 3 then continue;
      for k := 0 to 2 do
          m33[k,j-1] :=  m[k,i-1];
      if layout[i] < 0 then begin
         nhdr.pixdim[j] := -pixdim[i];
         originVox[j-1] := dim[i]-1;
      end;
  end;
  //scale matrix
  for i := 0 to 2 do
      for j := 0 to 2 do
          m33[j,i] := m33[j,i] * nhdr.pixdim[i+1];
  originMM := nifti_mat33vec_mul(m33, originVox);
  for i := 1 to 3 do
      nhdr.pixdim[i] := abs(nhdr.pixdim[i]);
  for i := 0 to 2 do
    m[i,3] := m[i,3] - originMM[i];
    nhdr.srow_x[0] := m33[0,0];
    nhdr.srow_x[1] := m33[0,1];
    nhdr.srow_x[2] := m33[0,2];
    nhdr.srow_x[3] :=   m[0,3];
    nhdr.srow_y[0] := m33[1,0];
    nhdr.srow_y[1] := m33[1,1];
    nhdr.srow_y[2] := m33[1,2];
    nhdr.srow_y[3] :=   m[1,3];
    nhdr.srow_z[0] := m33[2,0];
    nhdr.srow_z[1] := m33[2,1];
    nhdr.srow_z[2] := m33[2,2];
    nhdr.srow_z[3] :=   m[2,3];
  convertForeignToNifti(nhdr);
  result := true;
666:
    if isGz then begin
       gzBytes := K_gzBytes_headerAndImageCompressed;
       zfs.Free;
    end else
        fs.Free;
    mArray.Free;
    nhdr.descrip := 'MIF'+kIVers;
end; //readMIF()
{$ENDIF}

function readICS(var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean): boolean;
label
	666;
var
  isInt: boolean = true;
  isSigned: boolean = true;
  f: TFByte;
  str: string;
  i,nItems, lsb, bpp: integer;
  mArray: TStringList;
   //https://onlinelibrary.wiley.com/doi/epdf/10.1002/cyto.990110502
begin
  lsb := 0;
  bpp := 0;
  gzBytes := 0;
  result := false;
  mArray := TStringList.Create;
  AssignFile(f, fname);
  FileMode := fmOpenRead;
  Reset(f,1);
  ReadLnBin(f, str); //first line 011 012
  if (length(str) < 1) or (str[1] <> chr($09)) then
  	goto 666; //not a valid ICS file
  ReadLnBin(f, str); //version
  if not AnsiStartsText('ics_version', str) then
  	goto 666;
  ReadLnBin(f, str); //filename
  if not AnsiStartsText('filename', str) then begin
 	{$IFDEF UNIX} writeln('Error: expected ICS tag "filename": ICS 2.0?');{$ENDIF}
  	goto 666;
  end;
  splitstr(' ',str,mArray);
  nItems :=mArray.count;
  if (nItems < 2) then goto 666;
  str := fname;
  fname := extractfilename(mArray[1]);
  if upcase(extractfileext(fname)) <> '.IDS' then
  	fname := fname+'.ids';
  (*if (not fileexists(fname)) and fileexists(fname+ '.Z') then begin
     gzBytes := K_gzBytes_onlyImageCompressed;//K_gzBytes_headerAndImageCompressed;
     fname := fname+'.Z';
     //example testim_c.ids.Z has no Zlib header or footer
  end; *)
  if not fileexists(fname) then begin
  	fname := ExtractFilePath(str)+fname;
      (*if (not fileexists(fname)) and fileexists(fname+ '.Z') then begin
         gzBytes := K_gzBytes_onlyImageCompressed;//K_gzBytes_headerAndImageCompressed;
         fname := fname+'.Z';
      end;*)
        if not fileexists(fname) then begin
  	   NSLog('Unable to find IDS image '+fname);
  	   goto 666;
  	end;
  end;
  while ReadLnBin(f, str) do begin
  	splitstr(' ',str,mArray);
  	nItems :=mArray.count;
        //showmessage(str);
  	if (nItems > 4) and AnsiStartsText('layout', mArray[0]) and AnsiStartsText('sizes', mArray[1]) then begin
  		//writeln('!bpp', mArray[2]);
                bpp := StrToIntDef(mArray[2],0);
                //showmessage(str);
                for i := 3 to (nItems-1) do
                    nhdr.dim[i-2] := StrToIntDef(mArray[i],0);
  		//layout	sizes	8	256	256
  	end;
  	if (nItems > 3) and AnsiStartsText('parameter', mArray[0]) and AnsiStartsText('scale', mArray[1]) then begin
           for i := 2 to (nItems-1) do
               nhdr.pixdim[i-1] := StrToIntDef(mArray[i],0);
  	end;
  	if (nItems > 2) and AnsiStartsText('representation', mArray[0]) and AnsiStartsText('compression', mArray[1]) then begin
  		if not AnsiStartsText('uncompressed', mArray[2]) then begin
  			{$IFDEF UNIX} writeln('Unknown compression '+str);{$ENDIF}
  			goto 666;
  		end;
  		writeln('!no compression', mArray[2]);
  		//layout	sizes	8	256	256
  	end;
  	if (nItems > 2) and AnsiStartsText('representation', mArray[0]) and AnsiStartsText('format', mArray[1]) then begin
  		if not AnsiStartsText('integer', mArray[2]) then
                   isInt := false;
  	end;
  	if (nItems > 2) and AnsiStartsText('representation', mArray[0]) and AnsiStartsText('sign', mArray[1]) then begin
  		if AnsiStartsText('unsigned', mArray[2]) then
                   isSigned := false;
  	end;
  	if (nItems > 2) and AnsiStartsText('representation', mArray[0]) and AnsiStartsText('byte_order', mArray[1]) then begin
  	   lsb := StrToIntDef(mArray[2],0);
  	end;
        //representation	byte_order	1
  	//writeln('-->'+str+'<<');
  end;
  if (bpp = 32) and (not isInt) then
     nhdr.datatype := kDT_FLOAT32
  else if (bpp = 32) and (isSigned) and (isInt) then
       nhdr.datatype := kDT_INT32
  else if (bpp = 32) and (not isSigned) and (isInt) then
       nhdr.datatype := kDT_UINT32
  else if (bpp = 16) and (isSigned) and (isInt) then
       nhdr.datatype := kDT_INT16
  else if (bpp = 16) and (not isSigned) and (isInt) then
       nhdr.datatype := kDT_UINT16
  else if (bpp = 8) and (isSigned) and (isInt) then
       nhdr.datatype := kDT_INT8
  else if (bpp = 8) and (not isSigned) and (isInt) then
       nhdr.datatype := kDT_UINT8
  else begin
       NSLog(format('Unsupported data type: bits %d signed %s int %s', [bpp, BoolToStr(isSigned,'T','F'), BoolToStr(isInt,'T','F')]));
       goto 666;
  end;
  nhdr.srow_x[0] := -nhdr.pixdim[1];
  nhdr.srow_x[1] := 0;
  nhdr.srow_x[2] := 0;
  nhdr.srow_x[3] := 0;

  nhdr.srow_y[0] := 0;
  nhdr.srow_y[1] := -nhdr.pixdim[2];
  nhdr.srow_y[2] := 0;
  nhdr.srow_y[3] := 0;

  nhdr.srow_z[0] := 0;
  nhdr.srow_z[1] := 0;
  nhdr.srow_z[2] := nhdr.pixdim[3];
  nhdr.srow_z[3] := 0;

  nhdr.vox_offset := 0;
  {$IFDEF ENDIAN_BIG}
  if (bpp > 8) and (lsb < 2) then
  {$ELSE}
  if (bpp > 8) and (lsb > 1) then
  {$ENDIF}
     swapEndian := true;
  convertForeignToNifti(nhdr);
  result := true;
  666:
  mArray.free;
  closefile(f);
  nhdr.descrip := 'ICS'+kIVers;
end; //readICSHeader

function readpgm (var fname: string; var nhdr: TNIFTIhdr; var swapEndian: boolean): boolean;
//http://netpbm.sourceforge.net/doc/pgm.html
//http://paulbourke.net/dataformats/ppm/
label
  666;
var
  FP: TextFile;
  ch: char;
  str: string;
  maxval: integer;
  fileposBytes: integer = 0;
  itemIdx: integer = 0;
begin
	result := false;
      	{$IFDEF ENDIAN_LITTLE}
      	swapEndian := true; //most significant byte is first
        {$ELSE}
        swapEndian := false;
        {$ENDIF}
	nhdr.datatype := kDT_UINT8;
	FileMode := fmOpenRead;
	AssignFile(fp,fname);
	reset(fp);
	while (not EOF(fp))  do begin
		str := '';
		while not EOF(fp) do begin
			read(fp,ch);
			fileposBytes := fileposBytes + 1;
			if (ch = '#') then begin //comment: ignore rest of line
				while not EOF(fp) do begin
					read(fp,ch);
					fileposBytes := fileposBytes + 1;
					if (ch = chr($0A)) or (ch = chr($0D)) then break;
				end;
			end;
			if (ch = chr($0D)) or (ch = chr($0A)) or (ch = ' ') then break;
			str := str+ch;
		end;
		str := Trim(str);
		if str = '' then continue;
		//NSLog(inttostr(itemIdx)+'"'+str+'"');
		if itemIdx = 0 then begin
			if length(str) < 2 then goto 666;
			if str[1] <> 'P' then goto 666;
			if str[2] = '2' then
				nhdr.regular := 'A'
			else if str[2] = '3' then begin
				nhdr.regular := 'A';
				nhdr.datatype := kDT_RGB;
			end else if str[2] = '5' then
				//i := 0//
			else if str[2] = '6' then
				nhdr.datatype := kDT_RGB
			else begin
				NSLog('Unknown pgm type:'+str);
				goto 666;
			end;
		end else if itemIdx = 1 then begin
			nhdr.dim[1] := strtointdef(str, 0);
		end else if itemIdx = 2 then begin
			nhdr.dim[2] := strtointdef(str, 0);
		end else begin
			maxval := strtointdef(str, 0);
			if nhdr.datatype = kDT_RGB then
				//
			else if maxval > 255 then //Must be less than 65536, and more than zero.
				nhdr.datatype := kDT_UINT16
			else
				nhdr.datatype := kDT_UINT8;
			break;
		end;
		itemIdx := itemIdx + 1;
	end;
	nhdr.vox_offset := fileposBytes;
	result := true;
        SetSForm(nhdr);
	convertForeignToNifti(nhdr);
	nhdr.descrip := 'VTI'+kIVers;
666:
	CloseFile(FP);
	Filemode := 2;
end; //readpgm()

procedure swap4u(var s : UInt32);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:LongInt);
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
end;

function ReadUInt32(fname: string; offset: integer; swapEndian: boolean): UInt32;
var
	f: file;
	i: UInt32;
begin
  AssignFile(f, fname);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(f, 1);
  seek(f, offset);
  BlockRead(f, i, sizeof(i));
  CloseFile(f);
  if swapEndian then
  	swap4u(i);
  result := i;
end;

procedure swap8u(var s : UInt64);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2,Word3,Word4 : word); //word is 16 bit
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word4);
  outguy.Word2 := swap(inguy^.Word3);
  outguy.Word3 := swap(inguy^.Word2);
  outguy.Word4 := swap(inguy^.Word1);
  inguy^.Word1 := outguy.Word1;
  inguy^.Word2 := outguy.Word2;
  inguy^.Word3 := outguy.Word3;
  inguy^.Word4 := outguy.Word4;
end;

function ReadUInt64(fname: string; offset: integer; swapEndian: boolean): UInt64;
var
	f: file;
	i: UInt64;
begin
  AssignFile(f, fname);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(f, 1);
  seek(f, offset);
  BlockRead(f, i, sizeof(i));
  CloseFile(f);
  if swapEndian then
  	swap8u(i);
  result := i;
end;

function readVTI(var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean): boolean;
//https://vtk.org/Wiki/VTK_XML_Formats
//similar to *.vtp/*.vtu  described here http://www.earthmodels.org/software/vtk-and-paraview/vtk-file-formats
function parseField(field, str: string): string;
//parseField('WholeExtent="', str) would return '0 63 0 63 0 34' for str =
//   <ImageData WholeExtent="0 63 0 63 0 34" Origin="104 -58.6843 -84.798"
var
	i: integer;
begin
	i := PosEx(field,  str);
	if i < 1 then exit('');
	result := copy(str, i+length(field), maxint);
	i := PosEx('"',  result);
	if i < 2 then exit('');
	result := copy(result, 1, i-1);
end;
//https://vtk.org/Wiki/VTK_XML_Formats
label
  666;
var
  FP: TextFile;
  ch: char;
  mArray: TStringList;
  //pth,
  str, str2: string;
  offset, i, headerSize,fileposBytes: integer;
  isDetachedFile: boolean = false;
  isFirstLine: boolean;
  header_type : integer = kDT_UINT32;
  a64, e64: UInt64;
begin
  //pth := ExtractFilePath(fname);
  {$IFDEF FPC}
  DefaultFormatSettings.DecimalSeparator := '.' ;
  //DecimalSeparator := '.';
  {$ELSE}
  DecimalSeparator := '.';
  {$ENDIF}
  result := false;
  gzBytes :=0;
  fileposBytes := 0;
  swapEndian :=false;
  //nDims := 0;
  nhdr.dim[1] := 1;
  nhdr.dim[2] := 1;
  nhdr.dim[3] := 1;
  headerSize :=0;
  //isDetachedFile :=false;
  mArray := TStringList.Create;
  isFirstLine := true;
  offset := 0;
  FileMode := fmOpenRead;
  AssignFile(fp,fname);
  reset(fp);
  while (not EOF(fp))  do begin
    str := '';
    while not EOF(fp) do begin
      read(fp,ch);
      fileposBytes := fileposBytes + 1;
      //if (ch = chr($0D)) or (ch = chr($0A)) then break;
      if (ch = chr($0D)) then continue;
      if (ch = chr($0A)) then break;
      str := str+ch;
    end;
    if str = '' then break; //if str = '' then continue;
    str := Trim(str);
    if (isFirstLine) then begin
      if AnsiStartsText('<?xml',str) then
      	continue; //read next line
      if not AnsiStartsText('<VTKFile',str) then begin
      	NSLog('File does not start with "<VTIFile" '+ fname);
      	goto 666;
      end;
      if not AnsiContainsText(str, 'type="ImageData"') then begin
      	NSLog('VTK import only supports type="ImageData" '+fname);
      	goto 666;
      end;
      if AnsiContainsText(str, 'byte_order="LittleEndian"') then begin
      	{$IFDEF ENDIAN_BIG}
      	swapEndian :=false;
      	{$ENDIF}
      end else if AnsiContainsText(str, 'byte_order="BigEndian"') then begin
      	{$IFDEF ENDIAN_LITTLE}
      	swapEndian :=false;
      	{$ENDIF}
      end else
      	NSLog('byte_order not specified, assuming native endian.');
      if AnsiContainsText(str, 'header_type="UInt64"') then
      	header_type := kDT_UINT64
      else if  AnsiContainsText(str, 'header_type="UInt32"') then
      	header_type := kDT_UINT32
      else
      	NSLog('header_type not specified, assuming UInt32');
      isFirstLine := false;
      continue;
    end;
    if AnsiStartsText('<ImageData',str) then begin
   		//WholeExtent="0 63 0 63 0 34"
   		str2 := parseField('WholeExtent="', str);
   		if str2 = '' then begin
   			NSLog('error parsing WholeExtent');
   			goto 666;
   		end;
   		mArray.DelimitedText := str2;
   		if odd(mArray.count) or (mArray.count < 2) then begin
   			NSLog('WholeExtent must have an even number of arguments');
   			goto 666;
   		end;
   		nhdr.dim[0] := (mArray.count div 2);
   		for i := 0 to ((mArray.count div 2) -1) do
   			nhdr.dim[i+1] := 1 + strtointDef(mArray.Strings[(i*2)+1],0) - strtointDef(mArray.Strings[i*2],0);
   		//Origin="104 -58.6843 -84.798"
   		str2 := parseField('Origin="', str);
   		if str2 <> '' then begin
   			mArray.DelimitedText := str2;
   			if mArray.count > 0 then
   				nhdr.srow_x[3] := StrToFloatDef(mArray.Strings[0],0);
   			if mArray.count > 1 then
   				nhdr.srow_y[3] := StrToFloatDef(mArray.Strings[1],0);
   			if mArray.count > 2 then
   				nhdr.srow_z[3] := StrToFloatDef(mArray.Strings[2],0);
   		end;
   		//Spacing="3.25 3.2499993809390175 3.599996584498935" Direction="1 0 0 0 1 0 0 0 1">
		str2 := parseField('Spacing="', str);
   		if str2 <> '' then begin
   			mArray.DelimitedText := str2;
   			if mArray.count > 0 then
   				for i := 0 to min(6, mArray.count - 1) do
   					nhdr.pixdim[i+1] := StrToFloatDef(mArray.Strings[i],0);
   		end;
   		nhdr.srow_x[0] := nhdr.pixdim[1];
   		nhdr.srow_y[1] := nhdr.pixdim[2];
   		nhdr.srow_z[2] := nhdr.pixdim[3];

   		//TODO Direction="1 0 0 0 1 0 0 0 1"
   		continue;
    end; // <ImageData
    // <DataArray type="Int16" Name="ImageFile" format="appended" RangeMin="0"
    // <DataArray type="Int16" Name="ImageFile" format="ascii" RangeMin="0" RangeMax="2734">
    if AnsiStartsText('<DataArray',str) then begin
   	 	//type="Int16"
   	 	str2 := parseField('type="', str);
   	 	if AnsiStartsText( 'UInt8', str2) then
   	 		nhdr.datatype := kDT_UINT8
   	 	else if AnsiStartsText( 'Int8', str2) then
   	 		nhdr.datatype := kDT_INT8
   	 	else if AnsiStartsText( 'UInt16', str2) then
   	 		nhdr.datatype := kDT_UINT16
   	 	else if AnsiStartsText( 'Int16', str2) then
   	 		nhdr.datatype := kDT_INT16
   	 	else if AnsiStartsText( 'Float32', str2) then
   	 		nhdr.datatype := kDT_Float32
   	 	else if AnsiStartsText( 'Float64', str2) then
   	 		nhdr.datatype := kDT_Float64
   	 	else begin
   	 		NSLog('Unknown VTI value for type='+str2);
   	 		goto 666;
   	 	end;
   	 	//format="ascii"
   	 	str2 := parseField('format="', str);
   	 	if AnsiStartsText( 'ascii', str2) then begin
   	 		//for ASCII data, the raw data follows the DataArray tag
   	 		nhdr.regular := 'A';
   	 		break;
   	 	end else if AnsiStartsText('appended', str2) then
   	 		//
   	 	else if AnsiStartsText('binary', str2) then begin
   	 		NSLog('Unable to read VTI binary files save as base64. Convert with ParaView.');
   	 		goto 666;
   	 	end else begin
   	 		NSLog('Unsupported data "format" '+str2);
   	 		goto 666;
   	 	end;
   	 	//offset="0"
   	 	str2 := parseField('offset="', str);
   	 	if length(str2) > 0 then
   	 		offset := strtointdef(str2, 0);

   		//Name="ImageFile" format="ascii" RangeMin="0" RangeMax="2734">
    end; // <DataArray
    //<AppendedData encoding="raw">
    if AnsiStartsText('<AppendedData', str) then begin
    	ch := 'x';
    	while (not EOF(fp)) and (ch <> '_') do begin
      		read(fp,ch);
      		fileposBytes := fileposBytes + 1;
      	end;
    	break;
    end;
    if AnsiStartsText('<', str) then
    	continue; //ignored tag, like "<Piece Extent="0 63 0 63 0 34">"
  end;
  headerSize := fileposBytes;
  nhdr.vox_offset := headerSize;
  result := true;
666:
  CloseFile(FP);
  Filemode := 2;
  mArray.free;
  convertForeignToNifti(nhdr);
  if (result) and (not isDetachedFile) and (nhdr.regular <> 'A') then begin
  		headerSize := headerSize + offset;
  		//read actual size of
  		if header_type = kDT_UINT64 then begin
  			a64 := ReadUInt64(fname, headerSize, swapendian);
  			headerSize := headerSize + 8;
  		end else begin
  			a64 := ReadUInt32(fname, headerSize, swapendian);
  			headerSize := headerSize + 4;
  		end;
  		//expected size
  		e64 := 1;
  		for i := 1 to 7 do
  			if (nhdr.dim[i] > 1) then
  				e64 *= nhdr.dim[i];
  		e64 := e64 * (nhdr.bitpix div 8);
  		//
  		if e64 <> a64 then begin
  			result := false;
  			NSLog(format('Only able to read unpartitioned VTI images saved as single contiguous block. Expected %d bytes, not %d', [e64, a64]));
  		end;
  		nhdr.vox_offset := headerSize;
  end;
  nhdr.descrip := 'VTI'+kIVers;
end; //readVTI()

procedure reportMat(s: string; m: mat33);
begin
  {$ifdef unix}
  writeln(format('%s = [%g %g %g; %g %g %g; %g %g %g]',[s, m[0,0],m[0,1],m[0,2],     m[1,0],m[1,1],m[1,2],     m[2,0],m[2,1],m[2,2]]));
  {$endif}
end;

function parseStr2Float(var a: AnsiString): float;
//"file_type 5" -> 5.0
var
  aa:array of string;  
begin
  aa := a.split([' ']);
  result := StrToFloatDef(aa[1], 0.0);
end;

function parseStr2Int(var a: AnsiString): integer;
begin
  result := round(parseStr2Float(a));
end;

function isConcordeHeader(var fname: string): boolean;
var
  f: file;
  str: string;
  fp: TextFile;
  FSz: int64;
begin
  {$I-}
  AssignFile(f, fname);
  FileMode := 0;  //Set file access to read only
  Reset(f, 1);
  FSz := Filesize(f);
  CloseFile(f);
  {$I+}
  FileMode := 2;
  if (FSz < 600) then
    exit(false);
  AssignFile(fp,fname);
  reset(fp);
  while (not EOF(fp))  do begin
    readln(fp,str);
    if (AnsiStartsText( 'file_type', str)) then begin
      CloseFile(FP);
      Filemode := 2;
      exit(true);
    end;
  end;
  CloseFile(FP);
  Filemode := 2;
  exit(false);
end;

function readConcorde(var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean): boolean;
// siemens/concorde
//see Andy Loening's m-conc.h for AMIDE and (X)MedCon
var
  str: string;
  fp: TextFile;
  filetype, i: integer;
  pixel_size_x: single = 0.0;
  pixel_size_y: single = 0.0;
  pixel_size_z: single = 0.0;
  
begin
  result := false;
  swapEndian :=false;
  nhdr.dim[1] := 1;
  nhdr.dim[2] := 1;
  nhdr.dim[3] := 1;
  AssignFile(fp,fname);
  reset(fp);
  while (not EOF(fp))  do begin
    readln(fp,str);
    if (AnsiStartsText( 'file_type', str)) then begin
      filetype := parseStr2Int(str);
    end else if (AnsiStartsText( 'x_dimension', str)) then begin
      nhdr.dim[1] := parseStr2Int(str);
    end else if (AnsiStartsText( 'y_dimension', str)) then begin
      nhdr.dim[2] := parseStr2Int(str);
    end else if (AnsiStartsText( 'z_dimension', str)) then begin
      nhdr.dim[3] := parseStr2Int(str);
    end else if (AnsiStartsText( 'w_dimension', str)) then begin
      nhdr.dim[4] := parseStr2Int(str);
    end else if (AnsiStartsText( 'data_type', str)) then begin
      i := parseStr2Int(str);
      if (i = 1) then
        nhdr.datatype := kDT_INT8;
      if (i = 2)  or (i = 6) then
        nhdr.datatype := kDT_INT16;
      if (i = 3) or (i = 7) then
        nhdr.datatype := kDT_INT32;
      if (i = 4) or (i = 5) then
        nhdr.datatype := kDT_FLOAT32;
      {$IFDEF ENDIAN_LITTLE}
      if (i > 4) then
        swapEndian := true;
      {$ELSE}
      if (i < 5) then
        swapEndian := true;
      {$ENDIF}
    end  else if (AnsiStartsText( 'pixel_size_x', str)) then begin
      pixel_size_x := parseStr2Float(str);
    end  else if (AnsiStartsText( 'pixel_size_y', str)) then begin
      pixel_size_y := parseStr2Float(str);
    end  else if (AnsiStartsText( 'pixel_size_z', str)) then begin
      pixel_size_z := parseStr2Float(str);
    end  else if (AnsiStartsText( 'pixel_size', str)) then begin
      nhdr.pixdim[1] := parseStr2Float(str);
      nhdr.pixdim[2] := nhdr.pixdim[1];
    end else if (AnsiStartsText( 'axial_crystal_pitch', str)) then begin
      nhdr.pixdim[3] := parseStr2Float(str) / 2.0;
    end;
  end;
  //two methods to report pixel size: use preferred if available
  if (pixel_size_x > 0) then
    nhdr.pixdim[1] := pixel_size_x;
  if (pixel_size_y > 0) then
    nhdr.pixdim[2] := pixel_size_y;
  if (pixel_size_z > 0) then
    nhdr.pixdim[3] := pixel_size_z;
  CloseFile(FP);
  Filemode := 2;
  result := true;
  SetSForm(nhdr);
  if (filetype = 2) or (filetype = 4) or (filetype  = 3) then //sinogram, attenuation, normalization
    nhdr.dim[3] := 0;
  convertForeignToNifti(nhdr);
  nhdr.descrip := 'CONC'+kIVers;
end;

function readNRRD(var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian, isDimPermute2341: boolean): boolean;
//http://www.sci.utah.edu/~gk/DTI-data/
//http://teem.sourceforge.net/nrrd/format.html
//https://github.com/addisonElliott/matnrrd
FUNCTION specialdouble (d:double): boolean;
//returns true if s is Infinity, NAN or Indeterminate
//8byte IEEE: msb[63] = signbit, bits[52-62] exponent, bits[0..51] mantissa
//exponent of all 1s =   Infinity, NAN or Indeterminate
CONST kSpecialExponent = 2047 shl 20;
VAR Overlay: ARRAY[1..2] OF LongInt ABSOLUTE d;
BEGIN
  IF ((Overlay[2] AND kSpecialExponent) = kSpecialExponent) THEN
     RESULT := true
  ELSE
      RESULT := false;
END;
label
  666;
const
  NaN : double = 1/0;
var
  FP: TextFile;
  ch: char;
  mArray: TStringList;
  pth, str,tagName,elementNames, str2: string;
  lineskip,byteskip, i, j, s,nItems,headerSize,matElements,fileposBytes: integer;
  mat: mat33;
  rot33: mat33;
  isMicron, isOK, isDetachedFile,isFirstLine, isOffset: boolean;
  offset: array[0..3] of single;
  vSqr, flt: single;
  transformMatrix: array [0..11] of single;
  dtMin, dtMax, dtRange, dtScale, oldRange, oldMin, oldMax: double;
begin
  //gX := gX + 1; GLForm1.caption := inttostr(gX);
  //LOAD_MAT33(rot33, 1,0,0, 0,1,0, 0,0,1);
  LOAD_MAT33(rot33, -1,0,0, 0,-1,0, 0,0,1);
  for i := 0 to 11 do
      transformMatrix[i] := 0;
  for i := 0 to 2 do
    offset[i] := 0.0;
  for i := 0 to 2 do
      for j := 0 to 2 do
          mat[i,j] := 0;
  oldMin := NaN;
  oldMax := NaN;
  isOffset := false;
  isMicron := false;
  isDimPermute2341 := false;
  pth := ExtractFilePath(fname);
  isOK := true;
  {$IFDEF FPC}
  DefaultFormatSettings.DecimalSeparator := '.' ;
  //DecimalSeparator := '.';
  {$ELSE}
  DecimalSeparator := '.';
  {$ENDIF}
  result := false;
  gzBytes :=0;
  fileposBytes := 0;
  swapEndian :=false;
  //nDims := 0;
  nhdr.dim[1] := 1;
  nhdr.dim[2] := 1;
  nhdr.dim[3] := 1;
  headerSize :=0;
  lineskip := 0;
  byteskip := 0;
  isDetachedFile :=false;
  matElements :=0;
  mArray := TStringList.Create;
  isFirstLine := true;
  FileMode := fmOpenRead;
  AssignFile(fp,fname);
  reset(fp);
  while (not EOF(fp))  do begin
    str := '';
    while not EOF(fp) do begin
      read(fp,ch);
      if (ch = chr($00)) then break; //NRRD format specifies blank line before raw data, but some writers ignore this requirement, e.g. https://www.mathworks.com/matlabcentral/fileexchange/51174-dicom-medical-image-to-nrrd-medical-image
      fileposBytes := fileposBytes + 1;
      //if (ch = chr($0D)) or (ch = chr($0A)) then break;
      if (ch = chr($0D)) then continue;
      if (ch = chr($0A)) then break;
      str := str+ch;
    end;
    if str = '' then break; //if str = '' then continue;
    if (isFirstLine) then begin
      if (length(str) <4) or (str[1]<>'N') or (str[2]<>'R') or (str[3]<>'R') or (str[4]<>'D') then
        goto 666;
      isFirstLine := false;
    end;
    if (length(str) < 1) or (str[1]='#') then continue;
    splitstrStrict(':',str,mArray);
    if (mArray.count < 2) then continue;
    tagName := mArray[0];
    if AnsiStartsText('space units',tagName) then begin
        if PosEx('microns',  str) > 0 then
          isMicron := true;
        continue;
    end;
    //showmessage(inttostr(length(tagName))+':'+tagName);
    elementNames := mArray[1];
    splitstr(',',elementNames,mArray);
    nItems :=mArray.count;
    if (nItems < 1) then continue;
    for i := 0 to (nItems-1) do
      mArray.Strings[i] := cleanStr(mArray.Strings[i]); //remove '(' and ')'
    (*if AnsiContainsText(tagName, 'dimension') then
      nDims := strtoint(mArray.Strings[0])
    else*) if AnsiStartsText( 'spacings', tagName) then begin
      if (nItems > 6) then nItems :=6;
      for i:=0 to (nItems-1) do
        nhdr.pixdim[i+1] :=strtofloat(mArray.Strings[i]);
    end else if (AnsiStartsText( 'oldmin', tagName)) or (AnsiStartsText( 'old min', tagName)) then begin
          oldMin :=strtofloat(mArray.Strings[i]);
    end else if (AnsiStartsText( 'oldmax', tagName)) or (AnsiStartsText( 'old max', tagName)) then begin
          oldMax :=strtofloat(mArray.Strings[i]);
    end else if AnsiStartsText('sizes', tagName) then begin
      if (nItems > 6) then nItems :=6;
      //for i:=1 to 6 do
      //    nhdr.dim[i] := 1;
      for i:=0 to (nItems-1) do
          nhdr.dim[i+1] := strtoint(mArray.Strings[i]);
    end else if AnsiStartsText('space directions',tagName) then begin
      if (nItems > 12) then nItems :=12;
      matElements := 0;
      for i:=0 to (nItems-1) do begin
        if (matElements = 0) and AnsiContainsText(mArray.Strings[i], 'none') then begin
           isDimPermute2341 := true;
        end;
          flt := strToFloatDef(mArray.Strings[i], kNANsingle);
          if not specialsingle(flt) then begin
             transformMatrix[matElements] :=strtofloat(mArray.Strings[i]);
             matElements := matElements + 1;
          end;
        end;
      if (matElements >= 12) then
          LOAD_MAT33(mat, transformMatrix[0],transformMatrix[1],transformMatrix[2],
                     transformMatrix[4],transformMatrix[5],transformMatrix[6],
                     transformMatrix[8],transformMatrix[9],transformMatrix[10])
      else if (matElements >= 9) then
          LOAD_MAT33(mat, transformMatrix[0],transformMatrix[1],transformMatrix[2],
                     transformMatrix[3],transformMatrix[4],transformMatrix[5],
                     transformMatrix[6],transformMatrix[7],transformMatrix[8]);
    end else if AnsiStartsText('type', tagName) then begin //AnsiContainsText(tagName, 'type') then begin
      if AnsiContainsText(mArray.Strings[0], 'uchar') or
          AnsiContainsText(mArray.Strings[0], 'uint8') or
          AnsiContainsText(mArray.Strings[0], 'uint8_t')  then
          nhdr.datatype := KDT_UINT8 //DT_UINT8 DT_UNSIGNED_CHAR
       else if AnsiContainsText(mArray.Strings[0], 'ushort') or
               (AnsiContainsText(mArray.Strings[0], 'unsigned') and AnsiContainsText(mArray.Strings[1], 'short')) or
               AnsiContainsText(mArray.Strings[0], 'uint16') then //specific so
         	nhdr.datatype := KDT_UINT16
       else if AnsiContainsText(mArray.Strings[0], 'short') or //specific so
               AnsiContainsText(mArray.Strings[0], 'int16') or
               AnsiContainsText(mArray.Strings[0], 'int16_t') then
          nhdr.datatype :=kDT_INT16 //DT_INT16
      else if AnsiContainsText(mArray.Strings[0], 'float') then
          nhdr.datatype := kDT_FLOAT32 //DT_FLOAT32
      else if AnsiContainsText(mArray.Strings[0], 'unsigned')
               and (nItems > 1) and AnsiContainsText(mArray.Strings[1], 'char') then
          nhdr.datatype := kDT_UINT8 //DT_UINT8
      else if AnsiContainsText(mArray.Strings[0], 'unsigned') and
               (nItems > 1) and AnsiContainsText(mArray.Strings[1], 'int') then
          nhdr.datatype := kDT_UINT32 //
      else if AnsiContainsText(mArray.Strings[0], 'signed') and
               (nItems > 1) and AnsiContainsText(mArray.Strings[1], 'char') then
          nhdr.datatype := kDT_INT8 //do UNSIGNED first, as "isigned" includes string "unsigned"
      else if AnsiContainsText(mArray.Strings[0], 'signed') and
               (nItems > 1) and AnsiContainsText(mArray.Strings[1], 'short') then
          nhdr.datatype := kDT_INT16 //do UNSIGNED first, as "isigned" includes string "unsigned"
      else if AnsiContainsText(mArray.Strings[0], 'double') then
          nhdr.datatype := kDT_DOUBLE //DT_DOUBLE
      else if AnsiContainsText(mArray.Strings[0], 'uint') then
          nhdr.datatype := kDT_UINT32
      else if AnsiContainsText(mArray.Strings[0], 'int') then //do this last and "uint" includes "int"
          nhdr.datatype := kDT_INT32
      else begin
          NSLog('Unsupported NRRD datatype'+mArray.Strings[0]);
          isOK := false;
          break;
      end
    end else if AnsiStartsText('endian', tagName) then begin
      {$IFDEF ENDIAN_BIG} //data always stored big endian
      if AnsiContainsText(mArray.Strings[0], 'little') then swapEndian :=true;
      {$ELSE}
      if AnsiContainsText(mArray.Strings[0], 'big') then swapEndian :=true;
      {$ENDIF}
    end else if AnsiStartsText('encoding',tagName) then begin
      if AnsiContainsText(mArray.Strings[0], 'raw') then
          gzBytes := 0
      else if AnsiContainsText(mArray.Strings[0], 'bz2') or AnsiContainsText(mArray.Strings[0], 'bzip2') then
          gzBytes := K_bz2Bytes_headerAndImageCompressed
      else if AnsiContainsText(mArray.Strings[0], 'gz') or AnsiContainsText(mArray.Strings[0], 'gzip') then
          gzBytes := K_gzBytes_headerAndImageCompressed//K_gzBytes_headeruncompressed
      else if AnsiContainsText(mArray.Strings[0], 'ASCII') or AnsiContainsText(mArray.Strings[0], 'TEXT') or AnsiContainsText(mArray.Strings[0], 'TXT') then
          nhdr.regular := 'A'
      else begin
          NSLog('Unknown encoding format '+mArray.Strings[0]);
          isOK := false;
          break;
      end;
    end else if (AnsiStartsText('lineskip',tagName) or AnsiContainsText(tagName, 'line skip')) then begin //http://teem.sourceforge.net/nrrd/format.html#lineskip
      lineskip := strtointdef(mArray.Strings[0],0);
    end else if (AnsiStartsText('byteskip', tagName) or AnsiContainsText(tagName, 'byte skip')) then begin //http://teem.sourceforge.net/nrrd/format.html#byteskip
      byteskip := strtointdef(mArray.Strings[0],0);
    end else if AnsiStartsText('space origin', tagName) then begin
      if (nItems > 3) then nItems :=3;
      isoffset := true;
      for i:=0 to (nItems-1) do
          offset[i] := strtofloat(mArray.Strings[i]);
    end else if (nItems > 0) and AnsiStartsText('space', tagName) then begin //must do this after "space origin" check
      if AnsiStartsText('right-anterior-superior', mArray.Strings[0]) or AnsiStartsText('RAS', mArray.Strings[0]) then
        LOAD_MAT33(rot33, 1,0,0, 0,1,0, 0,0,1); //native NIfTI, default identity transform
      if AnsiStartsText('left-anterior-superior', mArray.Strings[0]) or AnsiStartsText('LAS', mArray.Strings[0]) then
        LOAD_MAT33(rot33, -1,0,0, 0,1,0, 0,0,1); //left-right swap relative to NIfTI
      if AnsiStartsText('left-posterior-superior', mArray.Strings[0]) or AnsiStartsText('LPS', mArray.Strings[0]) then begin//native NIfTI, default identity transform
         LOAD_MAT33(rot33, -1,0,0, 0,-1,0, 0,0,1); //left-right and anterior-posterior swap relative to NIfTI
      end;
    end else if AnsiStartsText('data file',tagName) or AnsiContainsText(tagName, 'datafile') then begin
      str2 := str;
      str := mArray.Strings[0];
      if (pos('LIST', UpperCase(str)) = 1) and (length(str) = 4) then begin  //e.g. "data file: LIST"
         readln(fp,str);
      end;
      if (pos('%', UpperCase(str)) > 0) and (nItems  > 1) then begin  //e.g. "data file: ./r_sphere_%02d.raw.gz 1 4 1"
         str := format(str,[strtoint(mArray.Strings[1])]);
      end;
      if fileexists(str) then
        fname := str
      else begin
         if (length(str) > 0) and (str[1] = '.') then  // "./r_sphere_01.raw.gz"
           str := copy(str, 2, length(str)-1 );
         if (length(str) > 0) and (str[1] = pathdelim) then  // "./r_sphere_01.raw.gz"
           str := copy(str, 2, length(str)-1 );  // "/r_sphere_01.raw.gz"
        fname := ExtractFilePath(fname)+str;
      end;
      if not fileexists(fname) then begin
          str2 := trim(copy(str2,pos(':',str2)+1, maxint));
          fname := str2;
          if not fileexists(fname) then
            fname := pth + str2;
          //showmessage(inttostr(nhdr.datatype));
      end;
      isDetachedFile :=true;
      //break;
    end; //for ...else tag names
  end;
  if ((headerSize = 0) and ( not isDetachedFile)) then begin
      if gzBytes = K_bz2Bytes_headerAndImageCompressed then
        gzBytes := K_bz2Bytes_onlyImageCompressed; //raw text file followed by GZ image
    if gzBytes = K_gzBytes_headerAndImageCompressed then
      gzBytes := K_gzBytes_onlyImageCompressed; //raw text file followed by GZ image
    if lineskip > 0 then begin
      for i := 1 to lineskip do begin
        while not EOF(fp) do begin
              read(fp,ch);
              fileposBytes := fileposBytes + 1;
              if (ch = chr($0D)) or (ch = chr($0A)) then break;
        end; //for each character in line
      end; //for each line
    end; //if lineskip
    headerSize :=fileposBytes;
      end;
  result := true;
  if (lineskip > 0) and (isDetachedFile) then begin
     NSLog('Unsupported NRRD feature: lineskip in detached file');
     result := false;
  end;
  if (byteskip > 0) then begin
    headerSize := headerSize + byteskip;
    //NSLog('Unsupported NRRD feature: byteskip');
    //result := false;
  end;
  if (nhdr.datatype <> kDT_FLOAT32) and (nhdr.datatype <> kDT_DOUBLE) and (not specialdouble(oldMin)) and (not specialdouble(oldMax)) then begin
     oldRange := oldMax - oldMin;
     dtMin := 0; //DT_UINT8, DT_RGB24, DT_UINT16
     if (nhdr.datatype = kDT_INT16) then dtMin := -32768.0;
     if (nhdr.datatype = kDT_INT32) then dtMin := -2147483648;
     dtMax := 255.00; //DT_UINT8, DT_RGB24
     if (nhdr.datatype = kDT_INT16) then dtMax := 32767;
     if (nhdr.datatype = kDT_UINT16) then dtMax := 65535.0;
     if (nhdr.datatype = kDT_INT32) then dtMax := 2147483647.0;
     dtRange := dtMax - dtMin;
     dtScale := oldRange/dtRange;
     nhdr.scl_slope := dtScale;
     nhdr.scl_inter := oldMin - (dtMin*dtScale);
     //showmessage(format('%g..%g', [oldMin,oldMax]));
  end;
  if (isDetachedFile) then
     headerSize := byteskip;
  if not isOK then result := false;
  //GLForm1.ShaderMemo.Lines.Add(format(' %d', [gzBytes]));
666:
  CloseFile(FP);
  Filemode := 2;
  mArray.free;
  if not result then exit;
  nhdr.vox_offset := headerSize;

  if (matElements >= 9) then begin
      //mat := nifti_mat33_mul( mat , rot33);
      if isMicron then begin
         for i := 0 to 2 do
           for j := 0 to 2 do
               mat[i,j] := mat[i,j] * 0.001;
         for i := 0 to 2 do
             offset[i] := offset[i] * 0.001;
      end;
      if rot33[0,0] < 0 then offset[0] := -offset[0]; //origin L<->R
      if rot33[1,1] < 0 then offset[1] := -offset[1]; //origin A<->P
      if rot33[2,2] < 0 then offset[2] := -offset[2]; //origin S<->I
      //reportMat('rot33', rot33);
      //reportMat('mat', mat);
       mat := nifti_mat33_mul( mat , rot33);
      //reportMat('mat', mat);

        nhdr.srow_x[0] := mat[0,0];
        nhdr.srow_x[1] := mat[1,0];
        nhdr.srow_x[2] := mat[2,0];
        nhdr.srow_x[3] := offset[0];
        nhdr.srow_y[0] := mat[0,1];
        nhdr.srow_y[1] := mat[1,1];
        nhdr.srow_y[2] := mat[2,1];
        nhdr.srow_y[3] := offset[1];
        nhdr.srow_z[0] := mat[0,2];
        nhdr.srow_z[1] := mat[1,2];
        nhdr.srow_z[2] := mat[2,2];
        nhdr.srow_z[3] := offset[2];
      //end;
        //next: ITK does not generate a "spacings" tag - get this from the matrix...
        for s :=0 to 2 do begin
            vSqr :=0.0;
            for i :=0 to 2 do
                vSqr := vSqr+ ( mat[s,i]*mat[s,i]);
            nhdr.pixdim[s+1] :=sqrt(vSqr);
        end; //for each dimension
        if not (isoffset) then
           SetCenter(nhdr);

  end;
  (*showmessage(format('m = [%g %g %g %g; %g %g %g %g; %g %g %g %g; 0 0 0 1]',[
    nhdr.srow_x[0], nhdr.srow_x[1], nhdr.srow_x[2], nhdr.srow_x[3],
    nhdr.srow_y[0], nhdr.srow_y[1], nhdr.srow_y[2], nhdr.srow_y[3],
    nhdr.srow_z[0], nhdr.srow_z[1], nhdr.srow_z[2], nhdr.srow_z[3]]));*)
  convertForeignToNifti(nhdr);
  nhdr.descrip := 'NRRD'+kIVers;
  //showmessage(floattostr(nhdr.vox_offset));
  //nhdr.vox_offset := 209;
end; //readNRRD()

procedure THD_daxes_to_NIFTI (var nhdr: TNIFTIhdr; xyzDelta, xyzOrigin: vect3; orientSpecific: ivect3);
//see http://afni.nimh.nih.gov/pub/dist/src/thd_matdaxes.c
const
  ORIENT_xyz1 = 'xxyyzzg'; //note Pascal strings indexed from 1, not 0!
  //ORIENT_sign1 = '+--++-';  //note Pascal strings indexed from 1, not 0!
var
  //axnum: array[0..2] of integer;
  axcode: array[0..2] of char;
  //axsign: array[0..2] of char;
  axstart,axstep: array[0..2] of single;
  ii, nif_x_axnum, nif_y_axnum, nif_z_axnum: integer;
  qto_xyz: mat44;

begin
    nif_x_axnum := -1;
    nif_y_axnum := -1;
    nif_z_axnum := -1;
    //axnum[0] := nhdr.dim[1];
    //axnum[1] := nhdr.dim[2];
    //axnum[2] := nhdr.dim[3];
    axcode[0] := ORIENT_xyz1[1+ orientSpecific[0] ] ;
    axcode[1] := ORIENT_xyz1[1+ orientSpecific[1] ] ;
    axcode[2] := ORIENT_xyz1[1+ orientSpecific[2] ] ;
    //axsign[0] := ORIENT_sign1[1+ orientSpecific[0] ] ;
    //axsign[1] := ORIENT_sign1[1+ orientSpecific[1] ] ;
    //axsign[2] := ORIENT_sign1[1+ orientSpecific[2] ] ;
    axstep[0] := xyzDelta[0] ;
    axstep[1] := xyzDelta[1]  ;
    axstep[2] := xyzDelta[2]  ;
    axstart[0] := xyzOrigin[0] ;
    axstart[1] := xyzOrigin[1] ;
    axstart[2] := xyzOrigin[2] ;
    for ii := 0 to 2 do begin
        if (axcode[ii] = 'x') then
            nif_x_axnum := ii
        else if (axcode[ii] = 'y') then
            nif_y_axnum := ii
        else
          nif_z_axnum := ii ;
    end;
    if (nif_x_axnum < 0) or (nif_y_axnum < 0) or (nif_z_axnum < 0) then exit; //not assigned
    if (nif_x_axnum  = nif_y_axnum) or (nif_x_axnum  = nif_z_axnum) or (nif_y_axnum  = nif_z_axnum) then exit; //not assigned
    ZERO_MAT44(qto_xyz);
    //-- set voxel and time deltas and units --
    nhdr.pixdim[1] := abs ( axstep[0] ) ;
    nhdr.pixdim[2] := abs ( axstep[1] ) ;
    nhdr.pixdim[3] := abs ( axstep[2] ) ;
    qto_xyz[0,nif_x_axnum] := - axstep[nif_x_axnum];
    qto_xyz[1,nif_y_axnum] := - axstep[nif_y_axnum];
    qto_xyz[2,nif_z_axnum] :=   axstep[nif_z_axnum];
    nhdr.qoffset_x :=  -axstart[nif_x_axnum] ;
    nhdr.qoffset_y :=  -axstart[nif_y_axnum];
    nhdr.qoffset_z :=  axstart[nif_z_axnum];
    qto_xyz[0,3] := nhdr.qoffset_x ;
    qto_xyz[1,3] := nhdr.qoffset_y ;
    qto_xyz[2,3] := nhdr.qoffset_z ;
    //nifti_mat44_to_quatern( qto_xyz , nhdr.quatern_b, nhdr.quatern_c, nhdr.quatern_d,dumqx, dumqy, dumqz, dumdx, dumdy, dumdz,nhdr.pixdim[0]) ;
    //nhdr.qform_code := kNIFTI_XFORM_SCANNER_ANAT;
    nhdr.srow_x[0] :=qto_xyz[0,0]; nhdr.srow_x[1] :=qto_xyz[0,1]; nhdr.srow_x[2] :=qto_xyz[0,2]; nhdr.srow_x[3] :=qto_xyz[0,3];
    nhdr.srow_y[0] :=qto_xyz[1,0]; nhdr.srow_y[1] :=qto_xyz[1,1]; nhdr.srow_y[2] :=qto_xyz[1,2]; nhdr.srow_y[3] :=qto_xyz[1,3];
    nhdr.srow_z[0] :=qto_xyz[2,0]; nhdr.srow_z[1] :=qto_xyz[2,1]; nhdr.srow_z[2] :=qto_xyz[2,2]; nhdr.srow_z[3] :=qto_xyz[2,3];
    nhdr.sform_code := kNIFTI_XFORM_SCANNER_ANAT;
end;

(*procedure Report_Mat(var nhdr: TNIFTIhdr);
begin
     printf(format('m=[%g %g %g %g; %g %g %g %g; %g %g %g %g; 0 0 0 1]',[nhdr.srow_x[0], nhdr.srow_x[1], nhdr.srow_x[2], nhdr.srow_x[3],
       nhdr.srow_y[0], nhdr.srow_y[1], nhdr.srow_y[2], nhdr.srow_y[3],
        nhdr.srow_z[0], nhdr.srow_z[1], nhdr.srow_z[2], nhdr.srow_z[3] ]));

end;

procedure m33 (m: mat33);
begin
     printf(format('m=[%g %g %g; %g %g %g; %g %g %g]',
       [m[0,0], m[0,1], m[0,2],
        m[1,0], m[1,1], m[1,2],
        m[2,0], m[2,1], m[2,2]  ]));

end;
*)

procedure set_Mat(var nhdr : TNIFTIhdr; x0,x1,x2,x3, y0,y1,y2,y3, z0,z1,z2,z3: single);
begin
     nhdr.srow_x[0] := x0;
     nhdr.srow_x[1] := x1;
     nhdr.srow_x[2] := x2;
     nhdr.srow_x[3] := x3;
     nhdr.srow_y[0] := y0;
     nhdr.srow_y[1] := y1;
     nhdr.srow_y[2] := y2;
     nhdr.srow_y[3] := y3;
     nhdr.srow_z[0] := z0;
     nhdr.srow_z[1] := z1;
     nhdr.srow_z[2] := z2;
     nhdr.srow_z[3] := z3;
end;

procedure idf2SForm(var nhdr: TNIFTIhdr; toplc: vect3; dcos: mat33);
var
   mm, diag, lps2ras, m: mat33;
begin
  diag := Matrix2D(nhdr.pixdim[1],0,0,  0,nhdr.pixdim[2],0, 0,0,nhdr.pixdim[3]);
  mm := nifti_mat33_mul(dcos, diag);
  lps2ras := Matrix2D(-1,0,0,  0,-1,0, 0,0,1);
  m :=  nifti_mat33_mul(lps2ras,mm);
  set_Mat(nhdr,
               m[0,0],m[0,1],m[0,2],-toplc[0],
               m[1,0],m[1,1],m[1,2],-toplc[1],
               m[2,0],m[2,1],m[2,2], toplc[2]
               );
  //Report_Mat(nhdr);
end;

function readIdf (var fname: string; var nhdr: TNIFTIhdr; var swapEndian: boolean): boolean;
//UCSF IDF format used by SIVIC
label
  666;
var
  FP: TextFile;
  sList: TStringList;
  i, j : integer;
  isOK: boolean;
  str, errStr, hdrName, lExt: string;
  dcos: mat33;
  toplc: vect3;
begin
  result := false;
  {$IFDEF FPC}
   DefaultFormatSettings.DecimalSeparator := '.' ;
   {$ELSE}
   DecimalSeparator := '.';
   {$ENDIF}
   hdrName := fname;
   lExt := UpCaseExt(fname);
   if (lExt <> '.IDF') then
      hdrName := ChangeFileExt(hdrName, '.idf');
   if not fileexists(hdrName) then begin
      NSLog('Unable to find IDF header '+hdrName);
      exit;
   end;
   fname := ChangeFileExt(hdrName, '.byt');
   nhdr.datatype := kDT_UINT8;
   if not fileexists(fname) then begin
      fname := ChangeFileExt(hdrName, '.int2');
      nhdr.datatype := kDT_UINT16;  //int2 ALWAYS unsigned: "If input pixels are signed ints, convert them to reals"
   end;
   if not fileexists(fname) then begin
      fname := ChangeFileExt(hdrName, '.real');
      nhdr.datatype := kDT_FLOAT;
   end;
   if not fileexists(fname) then begin
      NSLog('Unable to find IDF image *.byt, *int2, *.real associated with '+hdrName);
      exit;
   end;
   for i := 0 to 2 do begin
       toplc[i] := 0;
       for j := 0 to 2 do
           dcos[i,j] := 0.0;
   end;
   for i := 1 to 3 do begin
       nhdr.dim[i] := 0;
   end;
   //read
  isOK := false;
   {$IFDEF ENDIAN_BIG}
   swapEndian := false;
   {$ELSE} // IDF files are always big-endian
  swapEndian := true;
   {$ENDIF}
  result := false;
  AssignFile(fp,hdrName);
  reset(fp);
  sList := TStringList.Create;
  sList.Delimiter := ' ';        // Each list item will be blank separated
  errStr := 'EOF';
  if (EOF(fp)) then goto 666;
  readln(fp,str);
  errStr := 'File signature';
  if PosEx('IMAGE DESCRIPTOR FILE', str) < 1 then goto 666;
  errStr := 'main body';
  while (not EOF(fp))  do begin
      readln(fp,str);
      if length(str) < 1 then continue;
      sList.DelimitedText := str;
      if sList.Count < 2 then continue;
      if posex('filetype:', sList[0]) = 1 then begin
         i := strtointdef(sList[1], 0);
         //2 :UNSIGNED_INT_1;
         //3 :UNSIGNED_INT_2
         //7 :SIGNED_FLOAT_4;
         if (nhdr.datatype = kDT_UINT8) and (i = 2) then
            //OK
         else if (nhdr.datatype = kDT_UINT16) and (i = 3) then
            //OK
         else if (nhdr.datatype = kDT_FLOAT) and (i = 7) then
            //OK
         else
             NSLog('Warning: IDF file extension and type do not match');
      end; //filetype
      if posex('dimension:', sList[0]) = 1 then begin
         i := strtointdef(sList[1], 0);
         if (i < 1) or (i > 7) then goto 666;
         if EOF(fp) then goto 666;
         readln(fp,str);
         if length(str) < 1 then goto 666;
         sList.DelimitedText := str;
         if sList.Count < 8 then continue;
         //[0]npix:    [1]64   [2]fov(mm):  [3]300.00  [4]center(mm):    [5]0.00  [6]pixelsize(mm):    [7]4.68750
         nhdr.dim[i] := strtointdef(sList[1], 0);
         nhdr.pixdim[i] := strtofloatdef(sList[7], 0);
      end; //dimension
      //if posex('orientation:', sList[0]) = 1 then begin
      //   orient := strtointdef(sList[1], 0);
      //end; //orientation
      if (posex('toplc:', sList[0]) = 1) and (sList.Count > 3) then begin
         toplc[0] := strtofloatdef(sList[1], 0);
         toplc[1] := strtofloatdef(sList[2], 0);
         toplc[2] := strtofloatdef(sList[3], 0);
      end;
      if (posex('dcos1:', sList[0]) = 1) and (sList.Count > 3) then begin
         dcos[0,0] := strtofloatdef(sList[1], 0);
         dcos[1,0] := strtofloatdef(sList[2], 0);
         dcos[2,0] := strtofloatdef(sList[3], 0);
      end;
      if (posex('dcos2:', sList[0]) = 1) and (sList.Count > 3) then begin
         dcos[0,1] := strtofloatdef(sList[1], 0);
         dcos[1,1] := strtofloatdef(sList[2], 0);
         dcos[2,1] := strtofloatdef(sList[3], 0);
      end;
      if (posex('dcos3:', sList[0]) = 1) and (sList.Count > 3) then begin
         dcos[0,2] := strtofloatdef(sList[1], 0);
         dcos[1,2] := strtofloatdef(sList[2], 0);
         dcos[2,2] := strtofloatdef(sList[3], 0);
         //showmessage(sList[1]+':'+sList[2]+':'+sList[3]);
      end;
  end; //while not end
  if (nhdr.dim[1] > 0) and (nhdr.dim[2] > 0) then isOK := true;
  if isOK then
     result := true;
  idf2SForm(nhdr, toplc, dcos);
  //set_Mat(nhdr, -3.4375,0,0,108.281, 0,-3.4375,0.0017875,129.565, 0,-0.001976,-3.8,87.6606); //17 OK
  //set_Mat(nhdr, -3.4375,0,0,108.281, 0,-3.4375,-0.001976,129.565, 0,0.0017875,-3.8,87.6606); //17
  //set_Mat(nhdr, 0,0,-3.8,70.8193, -3.4375,0,0,138.281, 0,-3.4375,0,114.135); //19 OK
  //Report_Mat(nhdr);
  //set_Mat(nhdr, -3.4375,0,0,108.281, 0,0,3.8,-58.8783, 0,-3.4375,0,122.915); //23
  //set_Mat(nhdr, -3.4375,0,0,108.281, 0,0.0248531,3.79989,-59.6592, 0,-3.4374,0.027474,122.279); //25
666:
  CloseFile(FP);
  Filemode := 2;
  sList.Free;
  if not result then begin
    NSLog('IDF error "'+errStr+'" '+hdrName);
    exit; //error - code jumped to 666 without setting result to true
  end;
  convertForeignToNifti(nhdr);
  nhdr.descrip := 'IDF'+kIVers;
end; //readIdf

function isINTERFILE(var fname: string): boolean;
var
  fp: TextFile;
  str: string;
begin
  result := false;
  if not fileexists(fname) then exit;
  FileMode := fmOpenRead;
  AssignFile(fp,fname);
  reset(fp);
  if not (EOF(fp)) then begin
     readln(fp,str);
     if PosEx('!INTERFILE', str) = 1 then
        result := true;
  end;
  CloseFile(FP);
  Filemode := 2;
end;

function getDim(tag: string): integer; //'!matrix size [1]' -> 1
var
   lo, hi: integer;
   s: string;
begin
     result := 0;
     lo := posex('[',tag);
     hi := posex(']',tag);
     if (lo < 1) or (hi < (lo + 1)) then exit;
     s := upcase(copy(tag,lo+1,hi-lo-1));
     result := strtointdef(s, 0);
     if result > 0 then exit;
     if s = 'X' then result := 1;
     if s = 'Y' then result := 2;
     if s = 'Z' then result := 3;
end;


procedure lpi2ras(var nhdr: TNIFTIhdr; xyzmm1, xyzmmMax:  vect3);
begin
    nhdr.srow_x[0] := -nhdr.srow_x[0];
    nhdr.srow_y[1] := -nhdr.srow_y[1];
    nhdr.srow_z[2] := -nhdr.srow_z[2];
    nhdr.srow_x[3] := -nhdr.srow_x[3];
    nhdr.srow_y[3] := -nhdr.srow_y[3];
    nhdr.srow_z[3] := -nhdr.srow_z[3];
    if xyzmm1[0] = kNaNSingle then begin
        NSLog('Warning: origin not set, "image info" tags not found');
        exit;
    end;
    if (abs(xyzmm1[1]-xyzmmMax[1]) > 0.01) or (abs(xyzmm1[2]-xyzmmMax[2]) > 0.01)  then begin
        NSLog('Warning: origin not set, "image info" suggest slices not orthogonal with scanner bore.');
        exit;
    end;
    nhdr.srow_x[3] := -xyzmm1[1];
    nhdr.srow_y[3] := -xyzmm1[2];
    nhdr.srow_z[3] := xyzmm1[0];
    //NSLog(format('--> %g %g %g',[nhdr.srow_x[3], nhdr.srow_y[3], nhdr.srow_z[3]]));
    //Report_Mat(nhdr);
end;

function SiemensImageInfo(tag, val: string; out xyzmm: vect3): integer;
//e.g. for
// image info[99]:={98,-253.016,-408.116,-655.193...
// tag = 'image info[99]', val ] '{98,-25...'
// slice is 98, xyzmm[0,1,2]= -253,-408,-655
var
    sList : TStringList;
begin
    result := 0;
    if (val[1] <> '{') or (val[length(val)] <> '}') then exit;
    result := getDim(tag);
    //writeln(inttostr(result)+'->'+val);
    sList := TStringList.Create;
    sList.Delimiter := ',';
    sList.DelimitedText := copy(val,2,length(val)-2);
    if sList.Count > 3 then begin
        xyzmm[0] := strtofloatdef(sList[1],0.0);
        xyzmm[1] := strtofloatdef(sList[2],0.0);
        xyzmm[2] := strtofloatdef(sList[3],0.0);
        //NSLog(format('%s-> %g %g %g',[val, xyzmm[0],xyzmm[1], xyzmm[2]]));
    end;
    sList.Free;
end;

function readInterfile (var fname: string; var nhdr: TNIFTIhdr; var swapEndian: boolean): boolean;
//interfile
//https://www.researchgate.net/publication/21707960_A_file_format_for_the_exchange_of_nuclear_medicine_image_data_A_specification_of_interfile_version_33
label
  666;
var
  fp: TextFile;
  i , bpp, dim: integer;
  isLPI, isOK, isUint, isOrigin, isPixDim: boolean;
  tmp, tag, str, errStr, val, valu: string;
  slice, sliceMax: integer;
  xyzmm, xyzmm1, xyzmmMax:  vect3;
begin
  result := false;
   if not fileexists(fname) then exit;
   FileMode := fmOpenRead;
   {$IFDEF FPC}
   DefaultFormatSettings.DecimalSeparator := '.' ;
   {$ELSE}
   DecimalSeparator := '.';
   {$ENDIF}
   //read
   isOK := false;
   bpp := 0;
   sliceMax := -1;
   xyzmm1[0] := kNaNSingle;
   xyzmmMax := xyzmm1;
   isOrigin := false;
   isPixDim := false;
   isUint := true;
   isLPI := false; //?
   for i := 0 to 3 do begin
       nhdr.srow_x[i] := 0;
       nhdr.srow_y[i] := 0;
       nhdr.srow_z[i] := 0;
       nhdr.dim[i] := 0;
   end;
  result := false;
  AssignFile(fp,fname);
  reset(fp);
  errStr := 'EOF';
  if (EOF(fp)) then goto 666;
  readln(fp,str);
  errStr := 'File signature';
  if PosEx('!INTERFILE', str) < 1 then goto 666;
  errStr := 'main body';
  while (not EOF(fp))  do begin
      readln(fp,str);
      i := posex(':=', str);
      if (i < 2) then continue;
      tag := copy(str, 1, i-1);
      tag := lowercase(tag);
      val := copy(str, i+2, maxint);
      val := trim(val);
      valu := val; //retain case - important for Linux fname
      val := lowercase(val);
      if length(val) < 1 then continue;
      if PosEx('name of data file', tag) > 0 then begin
         tmp := valu;
         if not fileexists(tmp) then
             tmp := ExtractFilePath(fname)+valu;
         if not fileexists(tmp) then begin
            NSLog('Unabled to find Interfile image "'+valu+'"');
            goto 666;
         end;
         fname := tmp;
      end;
      if PosEx('imagedata byte order', tag) > 0 then begin
         if PosEx('littleendian', val) > 0 then
            {$IFDEF ENDIAN_BIG}
            swapEndian := true;
            {$ELSE} // IDF files are always big-endian
            swapEndian := false;
            {$ENDIF}
         if PosEx('bigendian', val) > 0 then
               {$IFDEF ENDIAN_BIG}
               swapEndian := false;
               {$ELSE} // IDF files are always big-endian
               swapEndian := true;
               {$ENDIF}
      end;
      if PosEx('number format', tag) > 0 then begin //some examples start with !
         if PosEx('float', val) > 0 then
            nhdr.datatype := kDT_FLOAT
         else if PosEx('unsignedinteger', val) > 0 then
            isUint := true
         else if  PosEx('integer', val) > 0 then
            isUint := false
         else begin
              NSLog('Unknown interfile datatype "'+tag+'"'); //some examples start with !
              goto 666;
         end;
      end; //number of bytes per pixel
      if (PosEx('image orientation', tag) > 0) and (PosEx('{1,0,0,0,1,0}', val) > 0) then
         isLPI := true;
      if PosEx('number of bytes per pixel', tag) > 0 then
        bpp := strtointdef(val,0);
      if PosEx('number of time frames', tag) > 0 then
         nhdr.dim[4] := strtointdef(val, 0);
      if PosEx('matrix size', tag) > 0 then begin //some examples start with !
         dim := getDim(tag);
         if dim = 0 then continue;
         nhdr.dim[dim] := strtointdef(val, 0);
      end;
      //https://github.com/UCL/STIR/issues/333 : "mmpixel", "mm/pixel", "scaling factor (mm/pixel)"
      //Siemens SMS JSRecon appears to have a typo relative to the standard "scale factor (mm/pixel)" vs"'scaling factor'"
      if ((PosEx('scaling factor', tag) > 0) or (PosEx('scale factor', tag) > 0)) and (PosEx('mm', tag) > 0) and (PosEx('pixel', tag) > 0) then begin
         dim := getDim(tag);
         if dim = 0 then continue;
         nhdr.pixdim[dim] := strtofloatdef(val, 0);
         if dim = 1 then
            nhdr.srow_x[0] := nhdr.pixdim[dim];
         if dim = 2 then
            nhdr.srow_y[1] := nhdr.pixdim[dim];
         if dim = 3 then
            nhdr.srow_z[2] := nhdr.pixdim[dim];
         isPixDim := true;
      end;
      if PosEx('first pixel offset (mm)', tag) > 0 then begin
         isOrigin := true;
         dim := getDim(tag);
         if dim = 0 then continue;
         if dim = 1 then
            nhdr.srow_x[3] := strtofloatdef(val, 0);
         if dim = 2 then
            nhdr.srow_y[3] := strtofloatdef(val, 0);
         if dim = 3 then
            nhdr.srow_z[3] := strtofloatdef(val, 0);
         //nhdr.pixdim[dim] := strtofloatdef(val, 0);
      end; //'first pixel
      if  PosEx('image info[', tag) > 0 then begin
            slice := SiemensImageInfo(tag,val,xyzmm);
            if slice = 1 then
                xyzmm1 := xyzmm;
            if slice > sliceMax then begin
                sliceMax := slice;
                xyzmmMax := xyzmm;
            end;
      end //'image info'
  end; //while not end
  if (nhdr.dim[1] > 0) and (nhdr.dim[2] > 0) and (bpp > 0) then isOK := true;
  if (nhdr.datatype = kDT_FLOAT) then begin
     if (bpp = 8) then nhdr.datatype := kDT_DOUBLE;
  end else begin
    if (isUint) and (bpp = 1) then nhdr.datatype := kDT_UINT8;
    if (isUint) and (bpp = 2) then nhdr.datatype := kDT_UINT16;
    if (isUint) and (bpp = 4) then nhdr.datatype := kDT_UINT32;
    if (not isUint) and (bpp = 1) then nhdr.datatype := kDT_INT8;
    if (not isUint) and (bpp = 2) then nhdr.datatype := kDT_INT16;
    if (not isUint) and (bpp = 4) then nhdr.datatype := kDT_INT32;
  end;
  if isOK then
     result := true;
  if not isPixDim then
    printf('Interfile missing "scaling factor" (pixdim)');
  printf(format('Interfile dim %dx%dx%d %dbpp pixdim %.3fx%.3fx%.3f', [nhdr.dim[1], nhdr.dim[2], nhdr.dim[3], bpp, nhdr.pixdim[1], nhdr.pixdim[2], nhdr.pixdim[3]]));
  //two methods to determine origin: "first pixel offset" or Siemens usage of "image info"
  if isLPI then
    lpi2ras(nhdr, xyzmm1, xyzmmMax)
  else if not isOrigin then
    printf('Unable to determine origin');
666:
    CloseFile(FP);
    Filemode := 2;
  if not result then begin
     NSLog('Interfile error "'+errStr+'" ');
     //printf(format('  %dx%dx%d %dbpp', [nhdr.dim[1], nhdr.dim[2], nhdr.dim[3], bpp]));
     exit; //error - code jumped to 666 without setting result to true
  end;
  nhdr.descrip := 'Interfile'+kIVers;
  convertForeignToNifti(nhdr);
end; //read INTERFILE

function readVFF(var fname: string; var nhdr: TNIFTIhdr; var swapEndian: boolean): boolean;
//http://cbi.nyu.edu/svn/mrTools/trunk/mrLoadRet/File/loadVFF.m
label
  666;
const
	kFF = chr($0C);
var
  FP: TFByte;
  str: string;
  sList: TStringList;
  i : integer;
  isOK: boolean;
begin
  {$IFDEF FPC}
   DefaultFormatSettings.DecimalSeparator := '.' ;
   {$ELSE}
   DecimalSeparator := '.';
   {$ENDIF}
  isOK := false;
  {$IFDEF ENDIAN_LITTLE}
  swapEndian := true;
  {$ELSE}
  swapEndian := false;
  {$ENDIF}
  result := false;
  AssignFile(fp,fname);
  reset(fp);
  sList := TStringList.Create;
  sList.Delimiter := ' ';        // Each list item will be blank separated
  fname := ChangeFileExt(fname, '.vff');
  if not fileexists(fname) then begin
     NSLog('Unable to find '+fname);
     goto 666;
  end;
  ReadLnBin(fp,str);
  if posex('ncaa',str) <> 1 then begin
		NSLog('vff files should begin with "ncaa": '+fname);
		goto 666;
  end;
  nhdr.datatype := kDT_UINT8;
  nhdr.dim[0] := 0;
  while (not EOF(fp))  do begin
      ReadLnBin(fp,str);
      if length(str) < 1 then continue;
      if str = kFF then begin
      	isOK := true;
      	nhdr.vox_offset := filepos(fp);
      	break;
      end;
      if AnsiEndsStr(';', str) then
      	SetLength(str, length(str) - 1);
      sList.Delimiter:='=';
      sList.DelimitedText := str;
      if sList.Count < 2 then continue;
      if length(sList[1]) < 1 then continue;
      if posex('rank', sList[0]) = 1 then begin
         nhdr.dim[0] := strtointdef(sList[1],-1);
      end;
      if posex('bits', sList[0]) = 1 then begin
         nhdr.bitpix := strtointdef(sList[1],-1);
         if nhdr.bitpix = 8 then
            nhdr.datatype := kDT_UINT8  //Mango error
         else if nhdr.bitpix = 16 then
            nhdr.datatype := kDT_UINT16
         else if nhdr.bitpix = 32 then
            nhdr.datatype := kDT_UINT32
         else begin
              NSLog('Unsupported datatype "'+sList[1]+'"');
              goto 666;
         end;
      end;
      if (posex('type', sList[0]) = 1) and (posex('raster', sList[1]) <> 1) then begin
         NSLog('only able to read vff files of type "raster": '+fname);
		goto 666;
      end;
      if posex('size', sList[0]) = 1 then begin
      	for i := 1 to sList.Count -1 do
        	nhdr.dim[i] := strtointdef(sList[i],0);
      end;
      if (posex('spacing', sList[0]) = 1) or (posex('aspect', sList[0]) = 1) then begin
      	for i := 1 to sList.Count -1 do
        	nhdr.pixdim[i] := strtofloatdef(sList[i],0);
        nhdr.srow_x[0] := nhdr.pixdim[1];
        nhdr.srow_y[1] := nhdr.pixdim[2];
        nhdr.srow_z[2] := nhdr.pixdim[3];
      end;
      if posex('origin', sList[0]) = 1 then begin
      	nhdr.srow_x[3] := strtofloatdef(sList[1],0);
      	if sList.Count > 2 then
      		nhdr.srow_y[3] := strtofloatdef(sList[2],0);
      	if sList.Count > 3 then
      		nhdr.srow_z[3] := strtofloatdef(sList[3],0);
      end;
      if posex('data_scale', sList[0]) = 1 then begin
      		nhdr.scl_slope := strtofloatdef(sList[1],0);
      end;
      if posex('data_offset', sList[0]) = 1 then begin
      		nhdr.scl_inter := strtofloatdef(sList[1],0);
      end;
      if (posex('value', sList[0]) = 1) and (sList.Count > 2) then begin
      	nhdr.scl_inter := strtofloatdef(sList[1],0);
      	nhdr.scl_slope := strtofloatdef(sList[1],1);
      end;
      if (posex('byte_order', sList[0]) = 1) and (posex('little_endian', sList[1]) = 1) then begin
		  {$IFDEF ENDIAN_LITTLE}
		  swapEndian := false;
		  {$ELSE}
		  swapEndian := true;
		  {$ENDIF}
      end;
      if (posex('byte_order', sList[0]) = 1) and (posex('big_endian', sList[1]) = 1) then begin
		  {$IFDEF ENDIAN_LITTLE}
		  swapEndian := true;
		  {$ELSE}
		  swapEndian := false;
		  {$ENDIF}
      end;
      if posex('guidid', sList[0]) = 1 then begin
      		nhdr.descrip := sList[1];
      end;
      //NSLog(sList[0]+'>>"'+sList[1]+'"');
  end; //while not end
  if isOK then
     result := true;
666:
  CloseFile(FP);
  sList.Free;
  Filemode := 2;
  if not result then exit; //error - code jumped to 666 without setting result to true
  convertForeignToNifti(nhdr);
  nhdr.aux_file := 'VFF'+kIVers;
end;

function readSPR(var fname: string; var nhdr: TNIFTIhdr; var swapEndian: boolean): boolean;
//SLicer3D export format https://www.cmrr.umn.edu/stimulate/stimUsersGuide/node57.html
label
  666;
var
  FP: TextFile;
  str: string;
  sList: TStringList;
  i : integer;
  isOK: boolean;
begin
  {$IFDEF FPC}
   DefaultFormatSettings.DecimalSeparator := '.' ;
   {$ELSE}
   DecimalSeparator := '.';
   {$ENDIF}
  isOK := false;
  {$IFDEF ENDIAN_LITTLE}
  swapEndian := true;
  {$ELSE}
  swapEndian := false;
  {$ENDIF}
  result := false;
  AssignFile(fp,fname);
  reset(fp);
  sList := TStringList.Create;
  sList.Delimiter := ' ';        // Each list item will be blank separated
  fname := ChangeFileExt(fname, '.sdt');
  if not fileexists(fname) then begin
     NSLog('Unable to find '+fname);
     goto 666;
  end;
  nhdr.datatype := kDT_UINT8;
  while (not EOF(fp))  do begin
      readln(fp,str);
      if length(str) < 1 then continue;
      sList.Delimiter:=':'; //mango sometimes omits white space between name:value "endian:ieee-le", Slicer always includes white space "numDim: 4"
      sList.DelimitedText := str;
      if sList.Count < 2 then continue;
      //if posex('numDim', sList[0]) = 1 then begin
      //   showmessage('x');
      //end;
      if posex('endian', sList[0]) = 1 then begin  //endian:ieee-le
         if posex('-be', sList[1]) > 0 then
            {$IFDEF ENDIAN_LITTLE}
            swapEndian := true;
            {$ELSE}
            swapEndian := false;
            {$ENDIF}
         if posex('-le', sList[1]) > 0 then
            {$IFDEF ENDIAN_LITTLE}
            swapEndian := false;
            {$ELSE}
            swapEndian := true;
            {$ENDIF}
      end;
      if posex('dim', sList[0]) = 1 then begin
         nhdr.dim[0] := sList.Count - 1;
         isOK := true;
         for i := 1 to (sList.Count - 1) do
             nhdr.dim[i] := strtointdef(sList[i],-1);
      end;
      if posex('dataType', sList[0]) = 1 then begin
         if posex('BTYE', sList[1]) = 1 then
            nhdr.datatype := kDT_UINT8  //Mango error
         else if posex('BYTE', sList[1]) = 1 then
            nhdr.datatype := kDT_UINT8
         else if posex('WORD', sList[1]) = 1 then
            nhdr.datatype := kDT_UINT16
         else if posex('LWORD', sList[1]) = 1 then
            nhdr.datatype := kDT_UINT32
         else if posex('REAL', sList[1]) = 1 then
            nhdr.datatype := kDT_FLOAT
         else begin
              NSLog('Unsupported datatype "'+sList[1]+'"');
              goto 666;
         end;
      end; //dataType
      if (posex('interval', sList[0]) = 1) and (sList.Count > 3) then begin
         nhdr.pixdim[1] := strtofloatdef(sList[1],1.0);
         nhdr.pixdim[2] := strtofloatdef(sList[2],1.0);
         nhdr.pixdim[3] := strtofloatdef(sList[3],1.0);
         for i := 0 to 3 do begin
             nhdr.srow_x[i] := 0.0;
             nhdr.srow_y[i] := 0.0;
             nhdr.srow_z[i] := 0.0;
         end;
         nhdr.srow_x[0] := nhdr.pixdim[1];
         nhdr.srow_y[1] := nhdr.pixdim[2];
         nhdr.srow_z[2] := nhdr.pixdim[3];

      end;
      if (posex('origin', sList[0]) = 1) and (sList.Count > 3) then begin
         nhdr.srow_x[3] := -1 * strtofloatdef(sList[1],1.0);
         nhdr.srow_y[3] := -1 * strtofloatdef(sList[2],1.0);
         nhdr.srow_z[3] := strtofloatdef(sList[3],1.0);
      end;
  end; //while not end
  if isOK then
     result := true;
666:
  CloseFile(FP);
  sList.Free;
  Filemode := 2;
  if not result then exit; //error - code jumped to 666 without setting result to true
  convertForeignToNifti(nhdr);
  nhdr.descrip := 'SPR'+kIVers;
end;

function TFloatVec.nar: integer;
begin
  result := length(ar);
end;

procedure initAFNIs(var AFNIs: TAFNIs; n: integer);
var
	prev, i: integer;
begin
	if n < 0 then exit;
	if n < 1 then begin //release
		setlength(AFNIs, 0);
		exit;
	end;
	prev := length(AFNIs);
	if prev = n then exit; //no change
	if prev > n then exit; //keep previous items
	setLength(AFNIs, n);
	for i := prev to (n-1) do begin
		AFNIs[i].nam := '';
		AFNIs[i].jv := kFUNC_NOT_STAT;
		AFNIs[i].nv := 0;
		AFNIs[i].param[0] := 0;
		AFNIs[i].param[1] := 0;
		AFNIs[i].param[2] := 0;
		AFNIs[i].minVal := 0;
		AFNIs[i].maxVal := 0;
		AFNIs[i].maxAbsVal := 0;
		AFNIs[i].FDRcurv.dx:= 0;
		AFNIs[i].FDRcurv.x0:=0;
		AFNIs[i].FDRcurv.ar := nil;
	end;
end; // initAFNIs()

function parseStr(s, key: string): string;
// 'VAL="106" OKEY="106"' with key='VAL="' will return '106'
//beware, not all images follow specification https://sscc.nimh.nih.gov/sscc/dglen/Makinganatlas
// e.g. HaskinsPeds_aff_atlas1.0+tlrc.HEAD reports 'STRUCT=Left-Caudate' but should report 'STRUCT="Left-Caudate"'
var
	b,e, e2: integer;
begin
	result := '';
	b := Pos(key, s);
	if b < 1 then exit;
	b := b + length(key);
	if (b < length(s)) and (s[b] = '"') then
		b := b + 1;
	e := PosEx('"', s, b);
	e2 := PosEx(chr(9), s, b);//
	if (e2 > 0) and (e2 < e) then e := e2;
	if e < b then exit;
	result := copy(s, b, e-b);
end;

function readAFNIHeader(var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian, isAllVolumesSame: boolean;  var AFNIs: TAFNIs; var fLabels: TStringList): boolean; overload;
label
  666;
var
  sl, mArray: TStringList;
  typeStr,nameStr, valStr, tmpStr: string;
  xForm, lineNum, itemCount,i, vInt, nVols: integer;
  isProbMap, isStringAttribute, hasStatAux, hasIJK_TO_DICOM_REAL: boolean;
  valArray  : Array of double;
  orientSpecific: ivect3;
  xyzOrigin, xyzDelta: vect3;
  sList : TStringList;
  ivMax, iv, jv, nv: integer;
  p0,p1, p2: single;
begin
  {$IFDEF FPC}
  DefaultFormatSettings.DecimalSeparator := '.' ;
 //DecimalSeparator := '.';
  {$ELSE}
  DecimalSeparator := '.';
  {$ENDIF}
  for i := 0 to 2 do begin
      xyzDelta[i] := 0;
      xyzOrigin[i] := 0;
      orientSpecific[i] := 0;
  end;
  NII_Clear (nhdr);
  xForm := kNIFTI_XFORM_SCANNER_ANAT;
  fLabels.Clear;
  hasStatAux := false;
  hasIJK_TO_DICOM_REAL := false;
  isAllVolumesSame := true;
  setLength(AFNIs, 0);
  nVols := 1;
  result := false;
  isProbMap := false;
  gzBytes := 0;
  swapEndian := false;
  sl := TStringList.Create;
  mArray := TStringList.Create;
  sl.LoadFromFile(fname);
  if(sl.count) < 4 then goto 666;
  lineNum := -1;
  repeat
    //read type string
    lineNum := lineNum + 1;
    if length(sl[lineNum]) < 1 then continue;
    splitstr('=',sl[lineNum],mArray);
    if mArray.Count < 2 then continue;
    if not AnsiContainsText(cleanStr(mArray[0]), 'type') then continue;
    typeStr := cleanStr(mArray[1]);
    isStringAttribute :=  AnsiContainsText(typeStr, 'string-attribute');
    //next: read name string
    lineNum := lineNum + 1;
    if (lineNum >= (sl.count-1)) then continue;
    splitstr('=',sl[lineNum],mArray);
    if mArray.Count < 2 then continue;
    if not AnsiContainsText(cleanStr(mArray[0]), 'name') then continue;
    nameStr := cleanStr(mArray[1]);
    //if AnsiContainsText(nameStr,'BYTEORDER_STRING') and isStringAttribute then showmessage('txt');
    //next: read count string
    lineNum := lineNum + 1;
    if (lineNum >= (sl.count-1)) then continue;
    splitstr('=',sl[lineNum],mArray);
    if mArray.Count < 2 then continue;
    if not AnsiContainsText(cleanStr(mArray[0]), 'count') then continue;
    itemCount := strtoint(cleanStr(mArray[1]));
    if itemCount < 1 then exit;

    if isStringAttribute then begin

    	valStr := '';
    	repeat
    		lineNum := lineNum + 1;
    		if (lineNum > (sl.count-1)) then continue;
    		valStr := valStr+chr(9)+sl[lineNum];
    	until pos('~',sl[lineNum]) > 0;
    	if AnsiContainsText(nameStr,'VALUE_LABEL_DTABLE') then begin
           //seeTT_N27_EZ_ML+tlrc
           // https://afni.nimh.nih.gov/pub/dist/doc/program_help/README.environment.html
           if fLabels.Count > 1 then continue;
           nhdr.intent_code := kNIFTI_INTENT_LABEL;
           nv := Pos('>',valStr);
           if nv < 1 then continue;
           nv := nv + 1;
           jv := PosEx('<',valStr, nv);
           if jv < nv then continue;
           valStr := copy(valStr, nv, jv-nv-1);
           sList := TStringList.Create;
           sList.Delimiter := '"';
           sList.DelimitedText := valStr;
           for i := sList.Count-1 downto 0 do
           		if sList[i] = '' then sList.Delete(i);
           //first pass: max label
           nv := 0;
           i := 0;
           while (i+1) < sList.Count do begin
           		jv := StrToIntDef(sList[i], 99999);
           		if jv > nv then nv := jv;
           		i := i + 2;
           end;
           if (nv = 99999) or (nv < 1) then begin
           	sList.Free;
           	printf('Unable to parse VALUE_LABEL_DTABLE');
           	continue;
           end;
           //second pass
           for i := 0 to nv do
          	    fLabels.Add(inttostr(i)); //TT_N27_CA_EZ_MPM+tlrc.HEAD only labels some regionsf
  		   i := 0;
           while (i+1) < sList.Count do begin
           		jv := StrToIntDef(sList[i], 0);
           		fLabels[jv] := sList[i+1];
           		i := i + 2;
           end;
  		   sList.Free;
    	end; //VALUE_LABEL_DTABLE
        if AnsiContainsText(nameStr,'ATLAS_LABEL_TABLE') then begin
           nhdr.intent_code := kNIFTI_INTENT_LABEL;
           //writeln(valStr);
        	sList := TStringList.Create;
        	sList.StrictDelimiter := true;
  			sList.Delimiter := '<';        // Each list item will be blank separated
  			sList.DelimitedText := copy(valStr, 2, length(valStr)-2);
  			if sList.count < 1 then begin
  				sList.Free;
  				break;
  			end;
  			//if sList.count > (length(AFNIs)) then
  			//	initAFNIs(AFNIs, sList.count);
  			//first pass: max label
  			nv := 0;
  			for i := 0 to (sList.count-1) do begin
  				if (Pos('ATLAS_POINT',sList[i]) <> 1) then continue;
  				tmpStr := parseStr(sList[i], 'VAL="');
      			if tmpStr = '' then continue;
      			jv := StrToIntDef(tmpStr,-1);
      			if jv > nv then
      				nv := jv;
      		end;
  		//second pass
  		for i := 0 to nv do
          	    fLabels.Add('');
  		for i := 0 to (sList.count-1) do begin
                  if (Pos('ATLAS_POINT',sList[i]) <> 1) then continue;
                  tmpStr := parseStr(sList[i], 'VAL="');
                  if tmpStr = '' then continue;
                  nv := StrToIntDef(tmpStr,-1);
                  if nv < 0 then continue;
                  tmpStr := parseStr(sList[i], 'STRUCT=');
                  fLabels[nv] := tmpStr;
  		end;
  		sList.Free;
  		//for i := 0 to fLabels.Count - 1 do
  		//    writeln(fLabels[i]);
           //<ATLAS_POINT
        end; //ATLAS_LABEL_TABLE
        if AnsiContainsText(nameStr,'BYTEORDER_STRING') then begin
              {$IFDEF ENDIAN_BIG}
              if AnsiContainsText(valStr,'LSB_FIRST') then swapEndian := true;
              {$ELSE}
              if AnsiContainsText(valStr,'MSB_FIRST') then swapEndian := true;
              {$ENDIF}
        end; //BYTEORDER_STRING
        if AnsiContainsText(nameStr,'TEMPLATE_SPACE') then begin
          i := pos('''', valStr) ;
          if i > 0 then
             nhdr.aux_file := copy(valStr, i+1, length(valStr)-i-1);
          if AnsiContainsText(valStr,'TLRC') then xForm := kNIFTI_XFORM_TALAIRACH;
          if AnsiContainsText(valStr,'TT_N27') then xForm := kNIFTI_XFORM_TALAIRACH;//
          if AnsiContainsText(valStr,'MNI') then xForm := kNIFTI_XFORM_MNI_152;
        end; //TEMPLATE_SPACE
        if AnsiContainsText(nameStr,'BRICK_LABS') then begin
        	sList := TStringList.Create;
  			sList.Delimiter := '~';        // Each list item will be blank separated
                        valStr := trim(valStr);
                        valStr := copy(valStr, 2, length(valStr)-2);

  			sList.DelimitedText := valStr;
  			if sList.count < 1 then begin
  				sList.Free;
  				break;
  			end;
  			if sList.count > (length(AFNIs)) then
  				initAFNIs(AFNIs, sList.count);
  			for i := 0 to (sList.count-1) do
  				AFNIs[i].nam := sList[i] ;
  			sList.Free;
        end; //BRICK_LABS
    	continue;
    end;// if isStringAttribute else numeric inputs...

    //next read values
    lineNum := lineNum + 1;
    if (lineNum > (sl.count-1)) then continue;
    valStr := sl[lineNum];
    while ((lineNum+1) <= (sl.count-1)) and (length(sl[lineNum+1]) > 0) do begin
      lineNum := lineNum + 1;  //AFNI wraps some arrays across multiple lines
      valStr := valStr + ' '+ sl[lineNum];
    end;
    splitstr(' ',valStr,mArray);
    if (mArray.Count < itemCount) then itemCount := mArray.Count; // <- only if corrupt
    if itemCount < 1 then continue; // <- only if corrupt data
      setlength(valArray,itemCount);
      for i := 0 to (itemCount-1) do
        valArray[i] := strtofloat(cleanStr(mArray[i]) );
      //next - harvest data from important names
      if AnsiContainsText(nameStr,'BRICK_STATAUX') then begin
      	//first pass: find max index
        hasStatAux := true;
      	i := 0;
      	ivMax := 0;
      	while (i < itemCount) do begin
      		iv := round(valArray[i]); //sub-brick index
      		i := i + 1;
      		jv := round(valArray[i]); //statistical code
      		i := i + 1;
      		nv := round(valArray[i]); //number of parameters
      		i := i + 1;
      		i := i + nv;
      		if (iv < 0) then begin
      			NSLog('Error: BRICK_STATAUX indices must be positive');
      			goto 666;
      		end;
      		if (iv > ivMax) then ivMax := iv;
      	end;
      	//second pass: fill values
      	if (ivMax+1) > (length(AFNIs)) then
  			initAFNIs(AFNIs, ivMax+1);
  		i := 0;
      	while (i < itemCount) do begin
      		iv := round(valArray[i]); //sub-brick index
      		i := i + 1;
      		jv := round(valArray[i]); //statistical code
      		i := i + 1;
      		nv := round(valArray[i]); //number of parameters
      		i := i + 1;
      		p0 := 0;
      		if nv > 0 then
      			p0 := valArray[i];
      		p1 := 0;
      		if nv > 1 then
      			p1 := valArray[i+1];
      		p2 := 0;
      		if nv > 2 then
      			p2 := valArray[i+2];
      		i := i + nv;
      		AFNIs[iv].jv := jv;
      		AFNIs[iv].nv := nv;
      		AFNIs[iv].param[0] := p0;
      		AFNIs[iv].param[1] := p1;
      		AFNIs[iv].param[2] := p2;
      	end;
      end; //BRICK_STATAUX
      if AnsiContainsText(nameStr,'BRICK_TYPES') then begin
              vInt := round(valArray[0]);
              if (vInt = 0) then begin
                  nhdr.datatype := kDT_UINT8;
              end else if (vInt = 1) then begin
                  nhdr.datatype := kDT_INT16; //16 bit signed int
              end else if (vInt = 3) then begin
                  nhdr.datatype := kDT_FLOAT32;//32-bit float
              end else begin
                  NSLog('Unsupported BRICK_TYPES '+inttostr(vInt));
                  goto 666;
              end;
              if (itemCount > 1) then begin //check that all volumes are of the same datatype
                  nVols := itemCount;
                  for i := 1 to (itemCount-1) do
                      if (valArray[0] <> valArray[i]) then isAllVolumesSame := false;
                  if (not isAllVolumesSame) then begin
                      NSLog('Unsupported BRICK_TYPES feature: datatype varies between sub-bricks');
                      goto 666;
                  end;
              end; //if itemCount > 1
              //NSLog('HEAD datatype is '+inttostr(nhdr.datatype) );
         end else if AnsiContainsText(nameStr,'BRICK_STATS') then begin
              if (itemCount < 2) then continue;
              initAFNIs(AFNIs, itemCount div 2);
              for i := 0 to ((itemCount div 2) -1) do begin
                  AFNIs[i].minVal := valArray[i*2];
                  AFNIs[i].maxVal := valArray[(i*2)+1];
                  AFNIs[i].maxAbsVal := max(abs(AFNIs[i].minVal), abs(AFNIs[i].maxVal));
                 //printf(format('min..max %g..%g', [AFNIs[i].minVal, AFNIs[i].maxVal]));
              end;
         end else if AnsiContainsText(nameStr,'IJK_TO_DICOM_REAL') then begin
         
         	hasIJK_TO_DICOM_REAL := true;
         	nhdr.srow_x[0] := -valArray[0];
         	nhdr.srow_x[1] := -valArray[1];
         	nhdr.srow_x[2] := -valArray[2];
         	nhdr.srow_x[3] := -valArray[3];
         	nhdr.srow_y[0] := -valArray[4];
         	nhdr.srow_y[1] := -valArray[5];
         	nhdr.srow_y[2] := -valArray[6];
         	nhdr.srow_y[3] := -valArray[7];         	
         	nhdr.srow_z[0] := valArray[8];
         	nhdr.srow_z[1] := valArray[9];
         	nhdr.srow_z[2] := valArray[10];
         	nhdr.srow_z[3] := valArray[11];         	
         end else if AnsiContainsText(nameStr,'FDRCURVE_') then begin
             if (itemCount < 20) then continue;
             tmpStr := copy(nameStr, 10, maxint);
             iv := strtointdef(tmpStr, -1);
             if iv < 0 then continue;
             initAFNIs(AFNIs, iv+1);
             AFNIs[iv].FDRcurv.x0 := valArray[0];
             AFNIs[iv].FDRcurv.dx := valArray[1];
             setlength(AFNIs[iv].FDRcurv.ar, itemCount-2);
             for i := 0 to (itemCount-3) do
                 AFNIs[iv].FDRcurv.ar[i] := valArray[i+2];
             //printf(format('%d %d', [iv,itemCount]));
         end else if AnsiContainsText(nameStr,'BRICK_FLOAT_FACS') then begin
              nhdr.scl_slope := valArray[0];
              if nhdr.scl_slope = 0 then
                 nhdr.scl_slope := 1;
              initAFNIs(AFNIs, itemCount);
              AFNIs[0].scl_slopex := valArray[0];
              if (itemCount > 1) then begin //check that all volumes are of the same datatype
                  for i := 1 to (itemCount-1) do begin
                      if (valArray[0] <> valArray[i]) then isAllVolumesSame := false;
                      AFNIs[i].scl_slopex := valArray[i];
                  end;
              end; //if itemCount > 0
          end else if AnsiContainsText(nameStr,'DATASET_DIMENSIONS') then begin
              if itemCount > 3 then itemCount := 3;
              for i := 0 to (itemCount-1) do
                  nhdr.dim[i+1] := round(valArray[i]);
          end else if AnsiContainsText(nameStr,'ORIENT_SPECIFIC') then begin
              if itemCount > 3 then itemCount := 3;
              for i := 0 to (itemCount-1) do
                  orientSpecific[i] := round(valArray[i]);;
              //NSLog(@"HEAD orient specific %d %d %d",orientSpecific.v[0],orientSpecific.v[1],orientSpecific.v[2]);
          end else if AnsiContainsText(nameStr,'ORIGIN') then begin
              if itemCount > 3 then itemCount := 3;
              for i := 0 to (itemCount-1) do
                  xyzOrigin[i] := valArray[i];
              //NSLog(@"HEAD origin %g %g %g",xyzOrigin.v[0],xyzOrigin.v[1],xyzOrigin.v[2]);
          end else if AnsiContainsText(nameStr,'ATLAS_PROB_MAP') then begin
              if (round(valArray[0]) = 1) then isProbMap := true;
          end else if AnsiContainsText(nameStr,'ATLAS_LABEL_TABLE') then begin
              nhdr.intent_code := kNIFTI_INTENT_LABEL;
          end else if AnsiContainsText(nameStr,'DELTA') then begin
              if itemCount > 3 then itemCount := 3;
              for i := 0 to (itemCount-1) do
                  xyzDelta[i] := valArray[i];
              //NSLog(@"HEAD delta %g %g %g",xyzDelta.v[0],xyzDelta.v[1],xyzDelta.v[2]);
          end else if AnsiContainsText(nameStr,'TAXIS_FLOATS') then begin
              if (itemCount > 1) then nhdr.pixdim[4] := valArray[1]; //second item is TR
          end;
  until (lineNum >= (sl.count-1));
  result := true;
666: 
  valArray := nil; //release dynamic array
  Filemode := 2;
  sl.free;
  mArray.free;
  if not result then exit; //error - code jumped to 666 without setting result to true
  if (nVols > 1) then nhdr.dim[4] := nVols;
  if (isProbMap) and (nhdr.intent_code = kNIFTI_INTENT_LABEL)  then nhdr.intent_code := kNIFTI_INTENT_NONE;
  if (not hasIJK_TO_DICOM_REAL) then
  	THD_daxes_to_NIFTI(nhdr, xyzDelta, xyzOrigin, orientSpecific );
  nhdr.vox_offset := 0;
  convertForeignToNifti(nhdr);
  nhdr.sform_code:= xForm;
  if fLabels.Count > 2 then
     nhdr.intent_name := 'Labels'+inttostr(fLabels.Count-1); //0..N
  fname := ChangeFileExt(fname, '.BRIK');
  if (not FileExists(fname)) and (FileExists(fname+'.bz2')) then begin
    fname := fname+'.bz2';
    gzBytes := K_bz2Bytes_headerAndImageCompressed;
  end;
  if (not FileExists(fname)) then begin
    fname := fname+'.gz';
    gzBytes := K_gzBytes_headerAndImageCompressed;
  end;
  nhdr.descrip := 'AFNI'+kIVers;
  if (not hasStatAux) and  (length(AFNIs) > 1) then
     AFNIs[0].jv := kFUNC_NO_STAT_AUX ; //e.g. raw fMRI with BRICK_FLOAT_FACS but no BRICK_STATAUX
  if (length(AFNIs) = 1) and ((AFNIs[0].jv = kFUNC_NO_STAT_AUX) or (AFNIs[0].jv = kFUNC_NOT_STAT)) then
     setlength(AFNIs,0); //e.g. for T1 scan, the AFNI fields hold no useful information
end;

function readAFNIHeader(var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean): boolean; overload;
var
    AFNIs: TAFNIs;
    isAllVolumesSame: boolean;
    fLabels: TStringList;
begin
  fLabels := TStringList.Create;
  AFNIs := nil;
  result := readAFNIHeader(fname, nhdr, gzBytes, swapEndian, isAllVolumesSame, AFNIs, fLabels);
  fLabels.free;
  setlength(AFNIs,0);
  if isAllVolumesSame then exit;
  NSLog('Unsupported BRICK_FLOAT_FACS feature: intensity scale between sub-bricks');
  result := false;
end;

function readRAW(var fname: string; var nhdr: TNIFTIhdr; var swapEndian: boolean): boolean;
//Drishti raw format http://paulbourke.net/dataformats/pvl/
Type
  Tvol = packed record // Header structure
   datatype: byte;
   depth, height, width : longint;
  end;

var
  f: file;
  hdr: Tvol;
  FSz, vox: int64;
begin
  	result := false;
	FileMode := fmOpenRead;
	{$I-}
	AssignFile(f, fname);
	FileMode := 0;  //Set file access to read only
	Reset(f, 1);
	FSz := Filesize(f);
	{$I+}
	if ioresult <> 0 then begin
		NSLog('Error in reading RAW header.'+inttostr(IOResult));
		FileMode := 2;
		exit;
	end;
	BlockRead(f, hdr, sizeof(Tvol));
	CloseFile(f);
	FileMode := 2;
        //NSLog(format('offset %d dt %d size %dx%dx%d bytes %d', [sizeof(Tvol), hdr.datatype, hdr.width, hdr.height, hdr.depth, FSz]));
        if hdr.datatype = 0 then
           nhdr.datatype := kDT_UINT8
        else if hdr.datatype = 2 then
           nhdr.datatype := kDT_UINT16
        else if hdr.datatype = 4 then
           nhdr.datatype := kDT_UINT32
        else if hdr.datatype = 8 then
           nhdr.datatype := kDT_FLOAT32
        else
             exit;
        {$IFDEF ENDIAN_BIG}
        swapEndian := true;
	swap4(hdr.hdrSz);
	swap4(hdr.width);
	swap4(hdr.height);
        {$ELSE}
        swapEndian := false;
        {$ENDIF}
	FSz := FSz - sizeof(Tvol);
        if nhdr.datatype = kDT_UINT8 then
           vox := 1
        else if nhdr.datatype = kDT_UINT16 then
           vox := 2
        else
             vox := 4;
        vox := vox * hdr.width * hdr.height * hdr.depth;
	if (vox <> FSz) then begin
	    NSLog(format('Not Drishti .raw format. offset %d size %dx%dx%d', [sizeof(Tvol), hdr.width, hdr.height, hdr.depth]));
	    exit;
	end;
	nhdr.dim[1]:=hdr.width;
	nhdr.dim[2]:=hdr.height;
	nhdr.dim[3]:=hdr.depth;
	nhdr.vox_offset := sizeof(Tvol);
	SetSForm(nhdr);
        convertForeignToNifti(nhdr);
	nhdr.descrip := 'DrishtiRAW'+kIVers;
	result := true;
end;

function readPVLNC(var fname: string; var nhdr: TNIFTIhdr; var swapEndian: boolean): boolean;
//https://github.com/nci/drishti/wiki/file-format---.pvl.nc
//http://paulbourke.net/dataformats/pvl/
label
  666;
var
  FP: TextFile;
  F : File Of byte;
  str, rawfile, tag: string;
  mArray: TStringList;
  slabsize : integer = 0;
  Sz, FSz: int64;
procedure ParseStr();
var
	e: integer;
begin
	tag := '';
	mArray.clear;
	str := trim(str);
	if (length(str) < 2) or (str[1] <> '<') or (str[2] = '/') then exit;
	e := Pos('>', str);
	if e < 3 then exit;
	tag := copy(str, 2, e-2);
	str := copy(str, e+1, maxint);
	e := Pos('<', str);
	str := copy(str, 1, e-1);
	mArray.DelimitedText := str;
	//writeln(tag+':'+str+':'+inttostr(mArray.count));
end; //ParseStr()
begin
	result := false;
	FileMode := fmOpenRead;
	AssignFile(fp,fname);
	reset(fp);
	mArray := TStringList.Create;
        tag := '';
	rawfile := fname + '.001';
	nhdr.datatype := kDT_UINT8; //default if pvlvoxeltype not specified
	if EOF(fp) then goto 666;
	readln(fp,str);
	if PosEx('<!DOCTYPE Drishti_Header>',  str) < 1 then begin
		//NSLog('Drishti .pvl.nc files should begin with" <!DOCTYPE Drishti_Header>"');
                {$ifdef lcl}
                {$IFDEF UNIX}
                Writeln('Drishti .pvl.nc files should begin with" <!DOCTYPE Drishti_Header>". Perhaps this is a Drishti geometry file.');
                {$ENDIF}
                {$else}
                Writeln('Drishti .pvl.nc files should begin with" <!DOCTYPE Drishti_Header>". Perhaps this is a Drishti geometry file.');
                {$endif}
                goto 666;
	end;
	while (not EOF(fp))  do begin
		readln(fp,str);
		ParseStr();
		if tag = '' then continue;
		if (PosEx('gridsize', tag) > 0) and (mArray.count > 2) then begin
			nhdr.dim[3] := strtointdef(mArray[0],0);
			nhdr.dim[2] := strtointdef(mArray[1],0);
			nhdr.dim[1] := strtointdef(mArray[2],0);
			continue;
		end;
		if (PosEx('slabsize', tag) > 0) and (mArray.count > 0) then begin
			slabsize := strtointdef(mArray[0],0);
			continue;
		end;
		if (PosEx('voxelsize', tag) > 0) and (mArray.count > 2) then begin
			nhdr.pixdim[3] := strtofloatdef(mArray[0],0);
			nhdr.pixdim[2] := strtofloatdef(mArray[1],0);
			nhdr.pixdim[1] := strtofloatdef(mArray[2],0);
			continue;
		end;
		if (PosEx('voxelunit', tag) > 0) and (mArray.count > 0) then begin
			if (PosEx('meter', mArray[0]) > 0) then
				nhdr.xyzt_units := kNIFTI_UNITS_METER
			else if (PosEx('millimeter', mArray[0]) > 0) then
				nhdr.xyzt_units := kNIFTI_UNITS_MM
			else if (PosEx('micron', mArray[0]) > 0) then
				nhdr.xyzt_units := kNIFTI_UNITS_MICRON
			else begin
			     //NSLog('Warning: NIfTI does not have voxelunit equivalent for Drishti ' + mArray[0]);
                        end;
		end;
		if (PosEx('pvlvoxeltype', tag) > 0) and (mArray.count > 0) then begin
			if (PosEx('unsigned', mArray[0]) > 0) and (mArray.count > 1) and (PosEx('char', mArray[1]) > 0) then
				nhdr.datatype := kDT_UINT8
			else if (PosEx('char', mArray[0]) > 0) then
				nhdr.datatype := kDT_INT8
			else if (PosEx('unsigned', mArray[0]) > 0)  and (mArray.count > 1) and (PosEx('short', mArray[1]) > 0) then
				nhdr.datatype := kDT_UINT16
			else if (PosEx('short', mArray[0]) > 0) then
				nhdr.datatype := kDT_INT16
			else if (PosEx('int', mArray[0]) > 0) then
				nhdr.datatype := kDT_INT32
			else if (PosEx('float', mArray[0]) > 0) then
				nhdr.datatype := kDT_FLOAT32
			else begin
				NSLog('Unknown Drishti voxeltype: '+mArray[0]);
				goto 666;
			end;
			continue;
		end; //voxeltype
		if (PosEx('rawfile', tag) > 0) and (mArray.count > 0) then begin //aka pvlvoxeltype
			rawfile := mArray[0];
			if not fileexists(rawfile) then
				rawfile := ExtractFilePath(fname) + ExtractFileName(mArray[0]);
			if not fileexists(rawfile) then
				rawfile := fname + '.001';
			if not fileexists(rawfile) then
				NSLog('Unable to find image data '+rawfile);
			if mArray.count > 1 then begin
				NSLog('Drishti "rawfile" has multiple elements: spaces in filenames or multiple files?');
			end;
			continue;
		end;
	end;
	if not fileexists(rawfile) then begin
		NSLog('Unable to find Drishti raw file named '+rawfile);
		goto 666;
	end;
	//check single volume
	nhdr.vox_offset := 13;//[single byte][NZ][NY][NX][volume data]
	//head-foot?
    nhdr.srow_x[0] := nhdr.pixdim[1];
    nhdr.srow_x[1] := 0;
    nhdr.srow_x[2] := 0;
    nhdr.srow_x[3] := 0;
    nhdr.srow_y[0] := 0;
    nhdr.srow_y[1] := nhdr.pixdim[2];
    nhdr.srow_y[2] := 0;
    nhdr.srow_y[3] := 0;
    nhdr.srow_z[0] := 0;
    nhdr.srow_z[1] := 0;
    nhdr.srow_z[2] := -nhdr.pixdim[3];
    nhdr.srow_z[3] := 0;
	//
	convertForeignToNifti(nhdr);
	Sz := round(nhdr.vox_offset) + (nhdr.dim[1]*nhdr.dim[2]*nhdr.dim[3]*(nhdr.bitpix div 8));
	Assign (F,rawfile);
  	Reset (F);
  	FSz := FileSize(F);
  	Close (F);
	if (slabsize < nhdr.dim[3]) or (FSz < Sz) then begin
		if (slabsize < nhdr.dim[3]) then
			NSLog('Drishti slabsize suggests image split between multiple volume (not supported)')
		else
			NSLog(format('Drishti image data split among multiple files? Expected %d (%dx%dx%dx%d+13) not %d', [Sz, nhdr.dim[1], nhdr.dim[2], nhdr.dim[3], nhdr.bitpix div 8, FSz]));
		goto 666;
	end;
	//all good
        {$IFDEF ENDIAN_BIG}
        swapEndian := true;
        {$ELSE}
        swapEndian := false;
        {$ENDIF}
	fname := rawfile;
	nhdr.descrip := 'Drishti'+kIVers;
	result := true;
	666:
	CloseFile(FP);
	Filemode := 2;
	mArray.Free;
end; //readPVLNC()


function readOSP(var fname: string; var nhdr: TNIFTIhdr; var swapEndian: boolean): boolean;
//https://www.ospray.org/documentation.html
label
  666;
var
  FP: TextFile;
  F : File Of byte;
  str, rawfile, tag: string;
  mArray: TStringList;
  //Sz, FSz: int64;
procedure ParseStr();
var
	e: integer;
begin
	tag := '';
	mArray.clear;
	str := trim(str);
	if (length(str) < 2) or (str[1] <> '<') or (str[2] = '/') then exit;
	e := Pos('>', str);
	if e < 3 then exit;
	tag := copy(str, 2, e-2);
	str := copy(str, e+1, maxint);
	e := Pos('<', str);
	str := copy(str, 1, e-1);
	mArray.DelimitedText := str;
	//writeln(tag+':'+str+':'+inttostr(mArray.count));
end; //ParseStr()
begin
	result := false;
	FileMode := fmOpenRead;
	AssignFile(fp,fname);
	reset(fp);
	mArray := TStringList.Create;
        tag := '';
	rawfile := changefileext(fname,'.raw');
	nhdr.datatype := kDT_UINT8; //default if pvlvoxeltype not specified
	if EOF(fp) then goto 666;
	readln(fp,str);
	if PosEx('<?xml version="',  str) < 1 then begin
		{$IFDEF UNIX}
                Writeln('ospray files should begin with" <?xml ". Perhaps this is a Drishti geometry file.');
                {$ENDIF}
                goto 666;
	end;
	while (not EOF(fp))  do begin
		readln(fp,str);
		ParseStr();
		if tag = '' then continue;
		if (PosEx('dimensions', tag) > 0) and (mArray.count > 2) then begin
			nhdr.dim[1] := strtointdef(mArray[0],0);
			nhdr.dim[2] := strtointdef(mArray[1],0);
			nhdr.dim[3] := strtointdef(mArray[2],0);
			continue;
		end;
		if (PosEx('voxelType', tag) > 0) and (mArray.count > 0) then begin
			if (PosEx('uchar', mArray[0]) > 0) then
				nhdr.datatype := kDT_UINT8
			else if (PosEx('short', mArray[0]) > 0) then
				nhdr.datatype := kDT_INT16
			else if (PosEx('float', mArray[0]) > 0) then
				nhdr.datatype := kDT_FLOAT32
			else begin
				NSLog('Unknown ospray voxeltype: "'+mArray[0]+'"');
				goto 666;
			end;
			continue;
		end; //voxeltype
		if (PosEx('filename', tag) > 0) and (mArray.count > 0) then begin //aka pvlvoxeltype
			rawfile := mArray[0];
			if not fileexists(rawfile) then
				rawfile := ExtractFilePath(fname) + ExtractFileName(mArray[0]);
			if not fileexists(rawfile) then
				NSLog('Unable to find image data '+rawfile);
			if mArray.count > 1 then begin
				NSLog('ospray "filename" has multiple elements: spaces in filenames or multiple files?');
			end;
			continue;
		end;
	end;
	if not fileexists(rawfile) then begin
		NSLog('Unable to find Drishti raw file named '+rawfile);
		goto 666;
	end;
	convertForeignToNifti(nhdr);
	//Sz := round(nhdr.vox_offset) + (nhdr.dim[1]*nhdr.dim[2]*nhdr.dim[3]*(nhdr.bitpix div 8));
	Assign (F,rawfile);
  	Reset (F);
  	//FSz := FileSize(F);
  	Close (F);
	//all good
        {$IFDEF ENDIAN_BIG}
        swapEndian := true;
        {$ELSE}
        swapEndian := false;
        {$ENDIF}
	fname := rawfile;
	nhdr.descrip := 'ospray'+kIVers;
	result := true;
	666:
	CloseFile(FP);
	Filemode := 2;
	mArray.Free;
end; //readOSP()

function readDat (var fname: string; var nhdr: TNIFTIhdr; var swapEndian: boolean): boolean;
//https://www.wolfgang-knecht.com/computer-graphics/advanced-volume-rendering/
//https://www.cg.tuwien.ac.at/courses/Visualisierung/Angaben/Bsp1.html#Volums%20Datensätze
Type
  Tbv_header = packed record //Next: BVox Format Header structure
        nx, ny, nz: UINT16;
  end; // Tbv_header;
var
   bhdr : Tbv_header;
   lHdrFile: file;
   nvox, nvoxswap, FSz : integer;
begin
  result := false;
  {$I-}
  AssignFile(lHdrFile, fname);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(lHdrFile, 1);
  {$I+}
  if ioresult <> 0 then begin
        NSLog('Error in reading BVox header.'+inttostr(IOResult));
        FileMode := 2;
        exit;
  end;
  FSz := Filesize(lHdrFile);
  bhdr.nx :=  0; //BlockRead should be out not var: https://fpc-pascal.freepascal.narkive.com/M2rzyAkf/blockread-and-buffers
  BlockRead(lHdrFile, bhdr, sizeof(Tbv_header));
  CloseFile(lHdrFile);
  swapEndian := false;
  nVox := bhdr.nx * bhdr.ny * bhdr.nz * 2; //*2 as 16-bpp
  if (nVox + sizeof(Tbv_header) ) <> FSz then begin
    swapEndian := true;
    bhdr.nx := Swap(bhdr.nx);
    bhdr.ny := Swap(bhdr.ny);
    bhdr.nz := Swap(bhdr.nz);
    nVoxSwap := bhdr.nx * bhdr.ny * bhdr.nz * 2; //*2 as 16-bpp
    if (2 * nVoxSwap + sizeof(Tbv_header) ) <> FSz then begin
       NSLog(format('Not a valid DAT file: expected filesize of %d or %d bytes (%dx%dx%d)',[nVoxSwap,nVox, bhdr.nx, bhdr.ny, bhdr.nz]));
       exit;
    end;

  end;
  nhdr.dim[0]:=3;//3D
  nhdr.dim[1]:=bhdr.nx;
  nhdr.dim[2]:=bhdr.ny;
  nhdr.dim[3]:=bhdr.nz;
  nhdr.dim[4]:=1;
  nhdr.pixdim[1]:=1.0;
  nhdr.pixdim[2]:=1.0;
  nhdr.pixdim[3]:=1.0;
  nhdr.datatype := kDT_INT16;
  nhdr.vox_offset := sizeof(Tbv_header);
  nhdr.sform_code := 1;
  //nhdr.srow_x[0]:=nhdr.pixdim[1];nhdr.srow_x[1]:=0.0;nhdr.srow_x[2]:=0.0;nhdr.srow_x[3]:=0.0;
  //nhdr.srow_y[0]:=0.0;nhdr.srow_y[1]:=nhdr.pixdim[2];nhdr.srow_y[2]:=0.0;nhdr.srow_y[3]:=0.0;
  //nhdr.srow_z[0]:=0.0;nhdr.srow_z[1]:=0.0;nhdr.srow_z[2]:=-nhdr.pixdim[3];nhdr.srow_z[3]:=0.0;
  SetSForm(nhdr);
  convertForeignToNifti(nhdr);
  nhdr.descrip := 'DAT'+kIVers;
  result := true;
end; //readDAT

function readForeignHeader(var lFilename: string; var lHdr: TNIFTIhdr; var gzBytes: int64; var swapEndian, isDimPermute2341: boolean; out xDim64: int64): boolean; overload;
var
  lExt, lExt2GZ: string;
begin
  NII_Clear (lHdr);
  xDim64 := -1;
  gzBytes := 0;
  swapEndian := false;
  //gzBytes := false;
  isDimPermute2341 := false;
  result := false;
  if FSize(lFilename) < 32 then //NRRD headers can be very short
      exit;
  {$IFDEF Darwin}
  if not IsReadable(lFilename) then exit;
  {$ENDIF}
  lExt := UpCaseExt(lFilename);
  lExt2GZ := '';
  if (lExt = '.GZ') or (lExt = '.BZ2') then begin
     lExt2GZ := changefileext(lFilename,'');
     lExt2GZ := UpCaseExt(lExt2GZ);
  end;
  if (PosEx('.AIM',lExt) = 1) then
       result := readAim(lFilename, lHdr, gzBytes, swapEndian)
  else if (lExt = '.DAT') then
     result := readDat(lFilename, lHdr, swapEndian)
  else if (lExt = '.DV') then
     result := readDeltaVision(lFilename, lHdr, swapEndian)
  else if (lExt = '.V') then
       result := readEcat(lFilename, lHdr, gzBytes, swapEndian)
  else if (lExt = '.VMR') then
       result := readVmr(lFilename, false, lHdr, swapEndian)
  else if (lExt = '.V16') then
       result := readVmr(lFilename, true, lHdr, swapEndian)
  else if (lExt = '.DF3') then
       result := readDf3(lFilename, lHdr, swapEndian)
  else if (lExt = '.BRIK.GZ') or (lExt2GZ = '.BRIK')  then begin
       lFilename := ChangeFileExt(lFilename, '');
       lFilename := ChangeFileExt(lFilename, '.HEAD');
       result := readAFNIHeader(lFilename, lHdr, gzBytes, swapEndian)
  end else if (lExt = '.HDR') and isConcordeHeader(lFilename) then begin
       result := readConcorde(lFilename, lHdr, gzBytes, swapEndian);
       lFilename := ChangeFileExt(lFilename, '');
  end else if (lExt = '.BRIK')  then begin
       lFilename := ChangeFileExt(lFilename, '.HEAD');
       result := readAFNIHeader(lFilename, lHdr, gzBytes, swapEndian)
  end else if (lExt = '.BVOX') then
       result := readBVox(lFilename, lHdr, swapEndian)
  else if (lExt = '.GIPL') then
       result := readGipl(lFilename, lHdr, swapEndian)
  else if (lExt = '.IDF') or (lExt = '.BYT') or (lExt = '.INT2') or (lExt = '.REAL') then
       result := readIdf(lFilename, lHdr, swapEndian)
  else if (lExt = '.RAW') then
    result := readRAW(lFilename, lHdr, swapEndian)
  else if (lExt = '.OSP') then
    result := readOSP(lFilename, lHdr, swapEndian)
  else if (lExt = '.NC') then
    result := readPVLNC(lFilename, lHdr, swapEndian)
  else if (lExt = '.PIC') then
    result := readpic(lFilename, lHdr)
  else if (lExt = '.PGM') or (lExt = '.PPM') or (lExt = '.PNM') then
    result := readpgm(lFilename, lHdr, swapEndian)
  else if (lExt = '.VTK') then
    result := readVTK(lFilename, lHdr, gzBytes, swapEndian)
  else if (lExt = '.MGH') or (lExt = '.MGZ') then
    result := readMGH(lFilename, lHdr, gzBytes, swapEndian, xDim64)
  else if (lExt = '.MHD') or (lExt = '.MHA') then
    result := readMHA(lFilename, lHdr, gzBytes, swapEndian)
  else if (lExt = '.ICS') then
    result := readICS(lFilename, lHdr, gzBytes, swapEndian)
  else if (lExt = '.MRC') or (lExt = '.MAP') or (lExt2GZ = '.MRC') or (lExt2GZ = '.MAP')  then
    result := readMRCHeader(lFilename, lHdr, gzBytes, swapEndian)
  else if ((lExt2GZ = '.MIF') or (lExt = '.MIF') or (lExt = '.MIH')) then
       result := readMIF(lFilename, lHdr, gzBytes, swapEndian)
  else if (lExt = '.NRRD') or (lExt = '.NHDR') then
       result := readNRRD(lFilename, lHdr, gzBytes, swapEndian, isDimPermute2341)
  else if (lExt = '.HEAD')  then
    result := readAFNIHeader(lFilename, lHdr, gzBytes, swapEndian)
  else if (lExt = '.RVF') then
     result := readRVF(lFilename, lHdr)
  else if (lExt = '.XVF') then
     result := readXVF(lFilename, lHdr)
  else if (lExt = '.SPR') then
    result := readSPR(lFilename, lHdr, swapEndian)
  else if (lExt = '.V3DRAW') then
    result := readV3draw(lFilename, lHdr, swapEndian)
  else if (lExt = '.VFF') then
    result := readVFF(lFilename, lHdr, swapEndian)
  else if (lExt = '.VOL') then
    result := readVOL(lFilename, lHdr, swapEndian)
  else if (lExt = '.VOX') then
    result := readVOX(lFilename, lHdr)
  else if (lExt = '.VTI') then
       result := readVTI(lFilename, lHdr, gzBytes, swapEndian)
  else if (lExt = '.XRAW') then
     result := readXRaw(lFilename, lHdr, swapEndian);
  if (not result) and (isINTERFILE(lFilename)) then
     result := readInterfile(lFilename, lHdr, swapEndian);
  if (not result) and (isTIFF(lFilename)) then
    NSLog('Use ImageJ/Fiji to convert TIFF and LSM files to NIfTI (or NRRD) for viewing')
  else if (not result) then begin
       lExt2GZ := isBioFormats(lFilename);
       if lExt2GZ <> '' then
          NSLog('Use ImageJ/Fiji to convert this '+lExt2GZ+' BioFormat image to NIfTI (or NRRD) for viewing');
  end;
  if (xDim64 < 0) and (result) then //only NGH supports 32-bit XDims, rather than 16-bit
    xDim64 := lHdr.dim[1];
end;

function readForeignHeader(var lFilename: string; var lHdr: TNIFTIhdr; var gzBytes: int64; var swapEndian, isDimPermute2341: boolean): boolean; overload;
var
    xDim64: int64;
begin
    result := readForeignHeader(lFilename, lHdr, gzBytes, swapEndian, isDimPermute2341, xDim64);
end;

end.
