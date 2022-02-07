unit nifti_types;
{$IFDEF FPC}
{$mode objfpc}{$ENDIF}{$H+}
interface

uses
  Classes, SysUtils;

type
 TNIFTI2hdr = packed record //Next: analyze Format Header structure
  HdrSz : longint; //MUST BE 540
  magic: array [1..8] of ansichar; //valid sig
  datatype, bitpix: word;
  dim: array [0..7] of int64; //unused
  intent_p1,intent_p2,intent_p3: double;
  pixdim: array [0..7] of double; //unused
  vox_offset: int64;
  scl_slope,scl_inter,cal_max,cal_min,slice_duration,toffset: double;
  slice_start, slice_end: int64;
  descrip: array[1..80] of ansichar;
  aux_file: array[1..24] of ansichar;
  qform_code, sform_code: longint;
  quatern_b,quatern_c,quatern_d,
  qoffset_x,qoffset_y,qoffset_z: double;
  srow_x: array[0..3]of double;
  srow_y: array[0..3]of double;
  srow_z: array[0..3]of double;
  slice_code, xyzt_units, intent_code: longint;
  intent_name: array[1..16] of ansichar;
  dim_info: byte;
  unused_str: array[1..15] of ansichar;
end; //TNIFTI2hdr Header Structure

 TNIFTIhdr = packed record //Next: analyze Format Header structure
   HdrSz : longint; //MUST BE 348
   Data_Type: array [1..10] of ansichar; //unused
   db_name: array [1..18] of ansichar; //unused
   extents: longint; //unused
   session_error: smallint; //unused
   regular: ansichar; ////unused: in Analyze 7.5 this must be 114
   dim_info: byte; //MRI slice order
   dim: array[0..7] of smallint; //Data array dimensions
   intent_p1, intent_p2, intent_p3: single;
   intent_code: smallint;
   datatype: smallint;
   bitpix: smallint;
   slice_start: smallint;
   pixdim: array[0..7]of single;
   vox_offset: single;
   scl_slope: single;//scaling slope
   scl_inter: single;//scaling intercept
   slice_end: smallint;
   slice_code: byte; //e.g. ascending
   xyzt_units: byte; //e.g. mm and sec
   cal_max,cal_min: single; //unused
   slice_duration: single; //time for one slice
   toffset: single; //time axis to shift
   glmax, glmin: longint; //UNUSED
   descrip: array[1..80] of ansichar;
   aux_file: array[1..24] of ansichar;
   qform_code, sform_code: smallint;
   quatern_b,quatern_c,quatern_d,
   qoffset_x,qoffset_y,qoffset_z: single;
   srow_x: array[0..3]of single;
   srow_y: array[0..3]of single;
   srow_z: array[0..3]of single;
   intent_name: array[1..16] of ansichar;
   magic: longint;
 end; //TNIFTIhdr Header Structure
 TAnalyzeHdrSection = packed record //Next: analyze Format Header structure
	Pad: array [1..253] of byte;
   originator: array [1..5] of smallint;                    (* 105 + 10  *)
end;//TAnalyzeHdrSection Structure


 const

 K_bz2Bytes_headerAndImageCompressed = -4;
 K_bz2Bytes_onlyImageCompressed = -3;
   K_gzBytes_headerAndImageCompressed = -2;
 K_gzBytes_onlyImageCompressed= -1;
 K_gzBytes_headerAndImageUncompressed= 0;
//
//DataTypes
kDT_BINARY                 =1;     // binary (1 bit/voxel)
kDT_UNSIGNED_CHAR          =2;     // unsigned char (8 bits/voxel)
kDT_UINT8 = kDT_UNSIGNED_CHAR;
kDT_SIGNED_SHORT           =4;     // signed short (16 bits/voxel)
kDT_INT16 = kDT_SIGNED_SHORT;
kDT_SIGNED_INT             =8;     // signed int (32 bits/voxel)
kDT_INT32 = kDT_SIGNED_INT;
kDT_FLOAT                 =16;     // float (32 bits/voxel)
kDT_FLOAT32 = kDT_FLOAT;
kDT_COMPLEX               =32;     // complex (64 bits/voxel)
kDT_COMPLEX64  = kDT_COMPLEX;     // single pair (64 bits)

kDT_DOUBLE                =64;     // double (64 bits/voxel)
kDT_FLOAT64 = kDT_DOUBLE;
kDT_RGB                   =128;     // RGB triple (24 bits/voxel)
kDT_RGBplanar3D             =129; //RRR..RGGG..GBBBB...B  (24 bits/voxel) entire 3D volume contiguous
kDT_RGBplanar2D             =129; //RRR..RGGG..GBBBB...B  (24 bits/voxel) each 2D slice saved sequentially
kDT_INT8                  =256;     // signed char (8 bits)
kDT_UINT16                =512;     // unsigned short (16 bits)
kDT_UINT32                =768;     // unsigned int (32 bits)
kDT_INT64                =1024;     // long long (64 bits)
kDT_UINT64               =1280;     // unsigned long long (64 bits)
kDT_FLOAT128             =1536;     // long double (128 bits)
kDT_COMPLEX128           =1792;     // double pair (128 bits)
kDT_COMPLEX256           =2048;     // long double pair (256 bits)
kDT_RGBA32               =2304;     // 4 byte RGBA (32 bits/voxel)
//   slice_code values
 kNIFTI_SLICE_SEQ_UNKNOWN = 0;
 kNIFTI_SLICE_SEQ_INC = 1;
 kNIFTI_SLICE_SEQ_DEC = 2;
 kNIFTI_SLICE_ALT_INC = 3;
 kNIFTI_SLICE_ALT_DEC = 4;
//xyzt_units values: note 3bit space and 3bit time packed into single byte
 kNIFTI_UNITS_UNKNOWN = 0;
 kNIFTI_UNITS_METER =  1;
 kNIFTI_UNITS_MM = 2;
 kNIFTI_UNITS_MICRON  = 3;
 kNIFTI_UNITS_SEC = 8;
 kNIFTI_UNITS_MSEC = 16;
 kNIFTI_UNITS_USEC = 24;
 kNIFTI_UNITS_HZ = 32;
 kNIFTI_UNITS_PPM = 40;
 //qform_code, sform_code values
 kNIFTI_XFORM_UNKNOWN = 0;
 kNIFTI_XFORM_SCANNER_ANAT = 1;//Scanner-based anatomical coordinates
 kNIFTI_XFORM_ALIGNED_ANAT = 2; //Coordinates aligned to another file e.g. EPI coregistered to T1
 kNIFTI_XFORM_TALAIRACH = 3; //Talairach-Tournoux Atlas; (0,0,0)=AC, etc.
 kNIFTI_XFORM_MNI_152 = 4; //MNI 152 normalized coordinates
 kNIFTI_XFORM_OTHER_TEMPLATE = 5;
{$IFDEF ENDIAN_BIG}
 //Magic values
 kswapNIFTI_MAGIC_SEPARATE_HDR = $0031696E;
 kswapNIFTI_MAGIC_EMBEDDED_HDR = $00312B6E;
  kNIFTI_MAGIC_DCM = $0044434D;//DCM
 //byte-swapped magic values
 kNIFTI_MAGIC_SEPARATE_HDR = $6E693100;
 kNIFTI_MAGIC_EMBEDDED_HDR = $6E2B3100;
 kNIFTI_MAGIC_SEPARATE_HDR = $6E693200;
 kNIFTI_MAGIC_EMBEDDED_HDR = $6E2B3200;
{$ELSE}
 //Magic values
 kNIFTI_MAGIC_SEPARATE_HDR = $0031696E;
 kNIFTI_MAGIC_EMBEDDED_HDR = $00312B6E;
 kNIFTI2_MAGIC_SEPARATE_HDR = $0032696E;
 kNIFTI2_MAGIC_EMBEDDED_HDR = $00322B6E;
 kNIFTI_MAGIC_DCM = $0044434D;//DCM
 //byte-swapped magic values
 kswapNIFTI_MAGIC_SEPARATE_HDR = $6E693100;
 kswapNIFTI_MAGIC_EMBEDDED_HDR = $6E2B3100;
{$ENDIF}
 //Statistics Intention
 kNIFTI_INTENT_NONE        =0;
kNIFTI_INTENT_CORREL      =2;
kNIFTI_INTENT_TTEST       =3;
kNIFTI_INTENT_FTEST       =4;
kNIFTI_INTENT_ZSCORE      =5;
kNIFTI_INTENT_CHISQ       =6;
kNIFTI_INTENT_BETA        =7;
kNIFTI_INTENT_BINOM       =8;
kNIFTI_INTENT_GAMMA       =9;
kNIFTI_INTENT_POISSON    =10;
kNIFTI_INTENT_NORMAL     =11;
kNIFTI_INTENT_FTEST_NONC =12;
kNIFTI_INTENT_CHISQ_NONC =13;
kNIFTI_INTENT_LOGISTIC   =14;
kNIFTI_INTENT_LAPLACE    =15;
kNIFTI_INTENT_UNIFORM    =16;
kNIFTI_INTENT_TTEST_NONC =17;
kNIFTI_INTENT_WEIBULL    =18;
kNIFTI_INTENT_CHI        =19;
kNIFTI_INTENT_INVGAUSS   =20;
kNIFTI_INTENT_EXTVAL     =21;
kNIFTI_INTENT_PVAL       =22;
NIFTI_INTENT_LOGPVAL     =23;
NIFTI_INTENT_LOG10PVAL	 =24;
kNIFTI_LAST_STATCODE = 24;//kNIFTI_INTENT_PVAL;
kNIFTI_INTENT_ESTIMATE  =1001;
kNIFTI_FIRST_NONSTATCODE = kNIFTI_INTENT_ESTIMATE;
kNIFTI_INTENT_LABEL     =1002;
kNIFTI_INTENT_NEURONAME =1003;
kNIFTI_INTENT_GENMATRIX =1004;
kNIFTI_INTENT_SYMMATRIX =1005;
kNIFTI_INTENT_DISPVECT  =1006;
kNIFTI_INTENT_VECTOR    =1007;
kNIFTI_INTENT_POINTSET  =1008;
kNIFTI_INTENT_TRIANGLE  =1009;
kNIFTI_INTENT_QUATERNION =1010;
kNIFTI_INTENT_RGB_VECTOR = 2003;


 procedure NIFTIhdr_SwapBytes (var lAHdr: TNIFTIhdr); //Swap Byte order for the Analyze type
 function Swap2(s : SmallInt): smallint;
 procedure swap4(var s : LongInt);
 procedure Xswap4r( var s:single);
 procedure swap8(var s : int64);
 procedure Xswap8r(var s : double);
 procedure NII_Clear (out lHdr: TNIFTIHdr);
 procedure NII_SetIdentityMatrix (var lHdr: TNIFTIHdr); //create neutral rotation matrix


implementation


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

procedure swap8(var s : int64);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2,Word3,Word4 : word); //word is 16 bit
      //1:(float:double);
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

procedure Xswap8r(var s : double);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2,Word3,Word4 : word); //word is 16 bit
      //1:(float:double);
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

procedure Xswap4r( var s:single);
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
end;


procedure NIFTIhdr_SwapBytes (var lAHdr: TNIFTIhdr); //Swap Byte order for the Analyze type
var
   lInc: integer;
begin
    with lAHdr do begin
         swap4(hdrsz);
         swap4(extents);
         session_error := swap2(session_error);
         for lInc := 0 to 7 do
             dim[lInc] := swap2(dim[lInc]);//666
         Xswap4r(intent_p1);
         Xswap4r(intent_p2);
         Xswap4r(intent_p3);
         intent_code:= swap2(intent_code);
         datatype:= swap2(datatype);
         bitpix := swap2(bitpix);
         slice_start:= swap2(slice_start);
         for lInc := 0 to 7 do
             Xswap4r(pixdim[linc]);
         Xswap4r(vox_offset);
{roi scale = 1}
         Xswap4r(scl_slope);
         Xswap4r(scl_inter);
         slice_end := swap2(slice_end);
         Xswap4r(cal_max);
         Xswap4r(cal_min);
         Xswap4r(slice_duration);
         Xswap4r(toffset);
         swap4(glmax);
         swap4(glmin);
         qform_code := swap2(qform_code);
         sform_code:= swap2(sform_code);
         Xswap4r(quatern_b);
         Xswap4r(quatern_c);
         Xswap4r(quatern_d);
         Xswap4r(qoffset_x);
         Xswap4r(qoffset_y);
         Xswap4r(qoffset_z);
		 for lInc := 0 to 3 do //alpha
			 Xswap4r(srow_x[lInc]);
		 for lInc := 0 to 3 do //alpha
			 Xswap4r(srow_y[lInc]);
		 for lInc := 0 to 3 do //alpha
             Xswap4r(srow_z[lInc]);
    end; //with NIFTIhdr
end; //proc NIFTIhdr_SwapBytes

end.

