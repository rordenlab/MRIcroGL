unit nifti_hdr_view;
interface
{$H+}
{$MODE DELPHI}
//{$DEFINE MRIcron}

uses

LResources, Spin,
{$IFNDEF MRIcron} SimdUtils, {$ENDIF}
{$IFNDEF Unix} ShellAPI, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, math,

  StdCtrls, Menus, ComCtrls, Buttons, nifti_types;
type
  { THdrForm }
  THdrForm = class(TForm)
    UnitLabel: TLabel;
    SpacingLabel: TLabel;
    LengthLabel: TLabel;
    Ymm: TFloatSpinEdit;
    HdrMenu: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Save1: TMenuItem;
    SaveHdrDlg: TSaveDialog;
    PageControl1: TPageControl;
    DimensionSheet: TTabSheet;
    OptionalSheet: TTabSheet;
    intent_nameEdit: TEdit;
    data_typeEdit: TEdit;
    CommentEdit: TEdit;
    db_nameEdit: TEdit;
    aux: TEdit;
    gmax: TSpinEdit;
    gmin: TSpinEdit;
    ses: TSpinEdit;
    ext: TSpinEdit;
    reg: TSpinEdit;
    DataLabel: TLabel;
    IntentStrLabel: TLabel;
    ExtentLabel: TLabel;
    SessErrLabel: TLabel;
    RegLabel: TLabel;
    GMinLabel: TLabel;
    GMaxLabel: TLabel;
    AuxLabel: TLabel;
    DBLabel: TLabel;
    NotesLabel: TLabel;
    HeaderMagicDrop: TComboBox;
    Label21: TLabel;
    DimLabel: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    Label7: TLabel;
    Endian: TComboBox;
    fTypeDrop: TComboBox;
    Label44: TLabel;
    xyzt_sizeDrop: TComboBox;
    xyzt_timeDrop: TComboBox;
    Xdim: TSpinEdit;
    Ydim: TSpinEdit;
    Zdim: TSpinEdit;
    Zmm: TFloatSpinEdit;
    OffsetEdit: TSpinEdit;
    TDim: TSpinEdit;
    Xmm: TFloatSpinEdit;
    TSec: TFloatSpinEdit;
    StatusBar1: TStatusBar;
    Label29: TLabel;
    Dim5Edit: TSpinEdit;
    StatSheet: TTabSheet;
    IntentLabel: TLabel;
    IntentCodeDrop: TComboBox;
    intent_p1Edit: TFloatSpinEdit;
    intent_p2Edit: TFloatSpinEdit;
    intent_p3Edit: TFloatSpinEdit;
    Intent1Label: TLabel;
    Intent2Label: TLabel;
    Intent3Label: TLabel;
    fmriSheet: TTabSheet;
    OrderLabel: TLabel;
    TimeLabel: TLabel;
    DurationLabel: TLabel;
    StartLabel: TLabel;
    slice_startEdit: TSpinEdit;
    Slice_durationEdit: TFloatSpinEdit;
    toffsetEdit: TFloatSpinEdit;
    IntensitySheet: TTabSheet;
    cmax: TFloatSpinEdit;
    cmin: TFloatSpinEdit;
    MaxLabel: TLabel;
    MinLabel: TLabel;
    Scale: TFloatSpinEdit;
    SlopeLabel: TLabel;
    Intercept: TFloatSpinEdit;
    InterceptLabel: TLabel;
    CalLabel: TLabel;
    DisplayLabel: TLabel;
    Page1: TMenuItem;
    Dimensions1: TMenuItem;
    ImageIntensity1: TMenuItem;
    Statistics1: TMenuItem;
    FunctionalMRI1: TMenuItem;
    Optional1: TMenuItem;
    ReorientSheet: TTabSheet;
    Rotations1: TMenuItem;
    srow_x0Edit: TFloatSpinEdit;
    srow_x1Edit: TFloatSpinEdit;
    srow_x2Edit: TFloatSpinEdit;
    Label24: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    srow_y0Edit: TFloatSpinEdit;
    srow_y1Edit: TFloatSpinEdit;
    srow_y2Edit: TFloatSpinEdit;
    srow_z0Edit: TFloatSpinEdit;
    srow_z1Edit: TFloatSpinEdit;
    srow_z2Edit: TFloatSpinEdit;
    srow_x3Edit: TFloatSpinEdit;
    srow_y3Edit: TFloatSpinEdit;
    srow_z3Edit: TFloatSpinEdit;
    quatern_bEdit: TFloatSpinEdit;
    quatern_cEdit: TFloatSpinEdit;
    quatern_dEdit: TFloatSpinEdit;
    qoffset_x_Edit: TFloatSpinEdit;
    qoffset_y_Edit: TFloatSpinEdit;
    qoffset_z_Edit: TFloatSpinEdit;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Dim6Edit: TSpinEdit;
    Label42: TLabel;
    Dim7Edit: TSpinEdit;
    PixDim5: TFloatSpinEdit;
    PixDim6: TFloatSpinEdit;
    PixDim7: TFloatSpinEdit;
    SliceCodeDrop: TComboBox;
    EndLabel: TLabel;
    slice_endEdit: TSpinEdit;
    FreqDimDrop: TComboBox;
    PhaseDimDrop: TComboBox;
    SliceDimDrop: TComboBox;
    FreqLabel: TLabel;
    PhaseLabel: TLabel;
    SliceLabel: TLabel;
    QFacEdit: TFloatSpinEdit;
    Label46: TLabel;
    QFormDrop: TComboBox;
    SFormDrop: TComboBox;
    Label38: TLabel;
    Label47: TLabel;
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure SaveHdrDlgClose(Sender: TObject);
    procedure WriteHdrForm (lHdr: TNIFTIhdr; IsNativeEndian: boolean; filename: string); overload;
    procedure WriteHdrForm (lHdr: TNIFTIhdr; IsNativeEndian: boolean; filename: string; DisplayDims: TVec3i; volsLoaded: integer = -1); overload;
    procedure ReadHdrDimensionsOnly (var lHdr: TNIFTIhdr); //reads only size dimensions: useful for computing estimated filesize
    procedure ReadHdrForm (var lHdr: TNIFTIhdr); //reads entire header
    procedure Save1Click(Sender: TObject);
    procedure TabMenuClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HeaderMagicDropSelect(Sender: TObject);
    //function OpenAndDisplayHdr (var lFilename: string; var lHdr: TNIFTIhdr): boolean;
  private
	{ Private declarations }

  public
	{ Public declarations }
  end;

var
  HdrForm: THdrForm;

implementation

{$R *.lfm}
function DropIndex2IntentCode(index: integer; isIndex2Intent: boolean): integer;
const
  kMaxIndex = 46;
  kDrop2IntentCode : array[0..kMaxIndex] of integer = (
  0, //Not statistics
  2, //Correlation coefficient
  3, //T-testation coefficient
  4, //F-test
  5, //Z-score
  6, //Chi-squared
  7, //Beta distribution
  8, //Binomial distribution
  9, //Gamma distribution
  10, //Poisson distribution
  11, //Normal distribution
  12, //Noncentral F statistic
  13, //Noncentral chi-squared
  14, //Logistic distributiond statistic
  15, //Laplace distribution
  16, //Uniform distribution
  17, //Noncentral t statistic
  18, //Weibull distribution
  19, //Chi distribution
  20, //Inverse Gaussian
  21, //Extreme value type I
  22, //p-value value type I
  23, //ln(p-value)
  24, //log10(p-value)
  1001, //Estimate
  1002, //Labels
  1003, //NeuroN
  1004, //Generic M
  1005, //Symmetric Matrix
  1006, //Displacement Field/Vector
  1007, //Vectorcement Field/Vector
  1008, //Points
  1009, //Triangle (mesh)
  1010, //Quaternion
  1011, //Dimensionless
  2001, //Time series
  2002, //Node index
  2003, //RGB Vector
  2004, //RGBA Vector
  2005, //Shape
  2006, //Displacement Field
  2007, //Cubic Spline Coefficients
  2008, //DCT Coefficients
  2009, //Quadratic Spline Coefficients
  2016, //TOPUP Cubic Spline Coefficients
  2017, //TOPUP Quadratic Spline Coefficients
  2018); //TOPUP Field
var
  i: integer;
begin
     result := 0;
     if (isIndex2Intent) then begin
     	if (index >= 0) and (index <=  kMaxIndex) then
           result := kDrop2IntentCode[index];
        exit;
     end;
     //given intent code, report drop down index
     for i := 0 to kMaxIndex do
     	 if  kDrop2IntentCode[i] = index  then
         	 exit(i);
end;

function DropItem2DataType(lItemIndex: integer): integer; //returns NIfTI datatype number
begin
  case  lItemIndex of
     0: result :=1; //binary
     1 : result := 256; //8-bit S
     2 : result := 2; //8-bit int U*
     3 : result := 4; //16-bit int S*
     4 : result := 512; //16-bit int U
     5 : result := 8; //32-bit int S*
     6 : result := 768; //32-bit int U
     7: result := 1024; //64-bit int S
     8: result := 1280; //64-bit int U
     9: result := 16; //32-bit real*
     10: result := 64; //64-bit real*
     11: result := 1536; //128-bit real
     12: result := 128; //24-bit rgb
     13: result := kDT_RGBA32;
     14: result := 32; //64-bit complex
     15: result := 1792; //128-bit complex
     16: result := 2048; //256-bit complex
     else
       result := 0;
  end; //case
end; //func DropItem2DataType

function DataType2DropItem (lDataType: smallint): integer;
begin
  case  lDataType of
     1: result := 0; //binary
     256: result := 1; //8-bit S
     2: result := 2; //8-bit int U*
     4: result := 3; //16-bit int S*
     512: result := 4; //16-bit int U
     8: result := 5; //32-bit int S*
     768: result := 6; //32-bit int U
     1024: result := 7; //64-bit int S
     1280: result := 8; //64-bit int U
     16: result := 9; //32-bit real*
     64: result := 10; //64-bit real*
     1536: result := 11; //128-bit real
     128: result := 12; //24-bit rgb
     kDT_RGBA32: result := 13;
     32: result := 14; //64-bit complex
     1792: result := 15; //128-bit complex
     2048: result := 16; //256-bit complex
     else
       result := 0;
  end; //case
end; //func DataType2DropItem

function DataType2BitsPerVoxel (lDataType: smallint): integer;
begin
  case  lDataType of
     1: result := 1; //binary
     256: result := 8; //8-bit S
     2: result := 8; //8-bit int U*
     4: result := 16; //16-bit int S*
     512: result := 16; //16-bit int U
     8: result := 32; //32-bit int S*
     768: result := 32; //32-bit int U
     1024: result := 64; //64-bit int S
     1280: result := 64; //64-bit int U
     16: result := 32; //32-bit real*
     64: result := 64; //64-bit real*
     1536: result := 128; //128-bit real
     128: result := 24; //24-bit rgb
     32: result := 64; //64-bit complex
     1792: result := 128; //128-bit complex
     2048: result := 256; //256-bit complex
     else
       result := 0;
  end; //case
end; //func DataType2BitsPerVoxel

function time_units2DropItem (lxyzt_units: byte): integer;
var lxyzt_unitsClipped: byte;
begin
     lxyzt_unitsClipped := lxyzt_units and 56;
     case lxyzt_unitsClipped of
          kNIFTI_UNITS_SEC  : result := 1;//= 8;
          kNIFTI_UNITS_MSEC : result := 2;//= 16;
          kNIFTI_UNITS_USEC : result := 3;//= 24;
          kNIFTI_UNITS_HZ   : result := 4;//= 32;
          kNIFTI_UNITS_PPM  : result := 5;//= 40;
          else result := 0; //unknown
     end; //case
end; //func time_units2DropItem

function DropItem2time_units (lDropItemIndex: byte): integer; //convert ComboBox index to NIFTI time units
begin
     case lDropItemIndex of
          1: result := kNIFTI_UNITS_SEC;
          2: result := kNIFTI_UNITS_MSEC;
          3: result := kNIFTI_UNITS_USEC;
          4: result := kNIFTI_UNITS_HZ;
          5: result := kNIFTI_UNITS_PPM;
          else result := 0; //unknown
     end; //case
end; //func DropItem2time_units

procedure THdrForm.WriteHdrForm (lHdr: TNIFTIhdr; IsNativeEndian: boolean; filename: string; DisplayDims: TVec3i; volsLoaded: integer = -1); overload; //writes a header to the various controls
begin
     WriteHdrForm (lHdr, IsNativeEndian, filename);
     if (DisplayDims.X <> lHdr.dim[1]) or (DisplayDims.Y <> lHdr.dim[2]) or (DisplayDims.Z <> lHdr.dim[3]) then
        StatusBar1.Panels[0].text := format('Resliced: %dx%dx%d', [DisplayDims.X,DisplayDims.Y,DisplayDims.Z]) ;
     if (volsLoaded > 0) then
     StatusBar1.Panels[1].Text := 'Volumes loaded: '+inttostr(volsLoaded);

end;

procedure THdrForm.WriteHdrForm (lHdr: TNIFTIhdr; IsNativeEndian: boolean; filename: string); overload;//writes a header to the various controls
var //lCStr: string[80];
    lInc: Integer;
begin
     StatusBar1.Panels[0].text := '';
     //caption := 'xx'+inttostr(lHdr.intent_code);
     StatusBar1.Panels[1].text := filename;
     with lHdr do begin
          XDim.Value := dim[1];
          YDim.Value := dim[2];
          ZDim.Value := dim[3];
          TDim.Value := dim[4];
          Dim5Edit.value := dim[5];
          Dim6Edit.value := dim[6];
          Dim7Edit.value := dim[7];
          Xmm.Value := pixdim[1];
          Ymm.Value := pixdim[2];
          Zmm.Value := pixdim[3];
          TSec.Value := pixdim[4];
          PixDim5.value := pixdim[5];
          PixDim6.value := pixdim[6];
          PixDim7.value := pixdim[7];
          OffsetEdit.value := round(vox_offset);
          Scale.value := scl_slope;
          Intercept.value := scl_inter;
          fTypeDrop.ItemIndex := (  DataType2DropItem( datatype));
          if IsNativeEndian then
             Endian.ItemIndex := 0
          else
              Endian.ItemIndex:= 1;
          if (Magic = kNIFTI_MAGIC_SEPARATE_HDR) or (Magic = kswapNIFTI_MAGIC_SEPARATE_HDR) then
	     HeaderMagicDrop.ItemIndex := 1
	  else if (Magic = kNIFTI_MAGIC_EMBEDDED_HDR) or (Magic = kswapNIFTI_MAGIC_EMBEDDED_HDR) then
	     HeaderMagicDrop.ItemIndex := 2
	  else if (Magic = kNIFTI2_MAGIC_SEPARATE_HDR) then
             HeaderMagicDrop.ItemIndex := 3
          else  if (Magic = kNIFTI2_MAGIC_EMBEDDED_HDR) then
             HeaderMagicDrop.ItemIndex := 4
          else
              HeaderMagicDrop.ItemIndex:=(0);
          xyzt_sizeDrop.ItemIndex:=(xyzt_units and 3);
          xyzt_timeDrop.ItemIndex:=(time_units2DropItem(xyzt_units));
          CommentEdit.text := descrip;
          data_typeEdit.text := data_type;
          db_nameEdit.text := db_name;
          aux.text := aux_file;
          intent_nameEdit.text := intent_name;
	  ext.value := extents;
    IntentCodeDrop.ItemIndex:=DropIndex2IntentCode(intent_code, false);

      {$IFNDEF FPC}
		  SliceCodeDrop.SetItemIndex(slice_code);
          FreqDimDrop.SetItemIndex(dim_info and 3);
          PhaseDimDrop.SetItemIndex((dim_info shr 2) and 3);
          SliceDimDrop.SetItemIndex((dim_info shr 4) and 3);
  {$ELSE}
          SliceCodeDrop.ItemIndex:=(slice_code);
          FreqDimDrop.ItemIndex:=(dim_info and 3);
          PhaseDimDrop.ItemIndex:=((dim_info shr 2) and 3);
          SliceDimDrop.ItemIndex:=((dim_info shr 4) and 3);
  {$ENDIF}
          intent_p1Edit.value := intent_p1;
          intent_p2Edit.value := intent_p2;
          intent_p3Edit.value := intent_p3;
          ses.value := session_error;
          reg.value := ord(regular);
          slice_startEdit.value := slice_start;
          slice_endEdit.value := slice_end;
          cmax.value := cal_max;
          cmin.value := cal_min;
          slice_durationEdit.value := slice_duration;
          toffsetEdit.value := toffset;
          gmax.value := glmax;
          gmin.value := glmin;
          //Next: 3D orientation rotations
          QFacEdit.value := pixdim[0];
            {$IFNDEF FPC}
          QFormDrop.SetItemIndex(qform_code);
          SFormDrop.SetItemIndex(sform_code);
  {$ELSE}
          QFormDrop.ItemIndex:= (qform_code);
          SFormDrop.ItemIndex :=(sform_code);
  {$ENDIF}
          //caption := format('%d %d', [qform_code, sform_code]);
          //showmessage(format('%g %g %g', [lHdr.qoffset_x, lHdr.qoffset_y, lHdr.qoffset_z]));
          //showmessage(format('%g %g %g', [qoffset_x, qoffset_y, qoffset_z]));

          quatern_bEdit.value := quatern_b;
          quatern_cEdit.value := quatern_c;
          quatern_dEdit.value := quatern_d;
          qoffset_x_Edit.value := qoffset_x;
          qoffset_y_Edit.value := qoffset_y;
          qoffset_z_Edit.value := qoffset_z;
          //caption := format('%g %g %g', [srow_x[0], srow_x[1], srow_x[2]]);
		  srow_x0Edit.value := srow_x[0];//12 affine matrix values
          srow_x1Edit.value := srow_x[1];
          srow_x2Edit.value := srow_x[2];
          srow_x3Edit.value := srow_x[3];
          srow_y0Edit.value := srow_y[0];
          srow_y1Edit.value := srow_y[1];
          srow_y2Edit.value := srow_y[2];
          srow_y3Edit.value := srow_y[3];
          srow_z0Edit.value := srow_z[0];
          srow_z1Edit.value := srow_z[1];
          srow_z2Edit.value := srow_z[2];
          srow_z3Edit.value := srow_z[3];
          //Finally... check values
          HeaderMagicDropSelect(nil); //disable or enable offset based on image format
          //showmessage(lHdr.ECodeText);
          (*if length(lHdr.ECodeText) > 0 then begin
             s := lHdr.ECodeText;
             s  := StringReplace(s, chr (0), '',[rfReplaceAll, rfIgnoreCase]);
             s := AdjustLineBreaks(s);//, tlbsLF);
             EcodeMemo.Lines.Text:= s;
             EcodeMemo.Visible := true;
          end else
              EcodeMemo.Visible := false; *)

    end;  //with lHdr
end;

procedure THdrForm.SaveHdrDlgClose(Sender: TObject);
begin
  //ApplySaveDlgFilter(SaveHdrDlg);
end;

procedure THdrForm.FormShow(Sender: TObject);
begin
     {$IFDEF Darwin}
     DimensionSheet.Constraints.MinHeight := fTypeDrop.Top+fTypeDrop.Height;
     {$ELSE}
     HdrForm.Constraints.MinHeight := (2*HdrMenu.Height)+ fTypeDrop.Top+fTypeDrop.Height;
     {$ENDIF}
end;

procedure THdrForm.PageControl1Change(Sender: TObject);
begin

end;

procedure THdrForm.FormHide(Sender: TObject);
begin
  {$IFDEF Darwin}Application.MainForm.SetFocus;{$ENDIF}
end;

procedure THdrForm.ReadHdrDimensionsOnly (var lHdr: TNIFTIhdr); //reads only size dimensions: useful for computing estimated filesize
var
    lInc: Integer;
begin
     with lHdr do begin
          dim[1] := round(XDim.Value);
          dim[2] := round(YDim.Value);
          dim[3] := round(ZDim.Value);
          dim[4] := round(TDim.Value);
          dim[5] := round(Dim5Edit.value);
          dim[6] := round(Dim6Edit.value);
          dim[7] := round(Dim7Edit.value);
          //Next: compute Dim[0]: compute number of dimensions by finding largest dimension with at least two samples
          lInc := 7;
          while dim[lInc] < 2 do
            dec(lInc);
          Dim[0] := lInc; //comp
          //showmessage(inttostr(Dim[0]));
          vox_offset := OffsetEdit.value;
          DataType := DropItem2DataType(FTypeDrop.ItemIndex);
          bitpix := DataType2BitsPerVoxel(DataType);
     end; //with NIfTIhdr
end; //proc ReadHdrDimensionsOnly

type
  kStr255 = string[255];

function getStr(inStr: string; len: integer): kStr255;
var
    i, n: integer;
begin
     result := '';
     for i := 1 to len do
         result[i] := chr(0);
     //showmessage(format('%d %d', [len, length(inStr)]));
     n := min(len, length(inStr));
     if n < 1 then exit;
     for i := 1 to n do
         result[i]  := inStr[i];
end;

procedure THdrForm.ReadHdrForm (var lHdr: TNIFTIhdr); //read the values the user has entered
var
    i: Integer;
    str: kStr255;
begin
     NII_Clear(lHdr); //important: reset values like first 4 bytes = 348
     ReadHdrDimensionsOnly(lHdr);

     //StatusBar1.Panels[0].text := 'ImageData (bytes)= '+inttostr(ComputeImageDataBytes(lHdr));
     with lHdr do begin
          pixdim[1] := Xmm.Value;
          pixdim[2] := Ymm.Value;
          pixdim[3] := Zmm.Value;
          pixdim[4] := TSec.Value;
          pixdim[5] := PixDim5.Value;
          pixdim[6] := PixDim6.Value;
          pixdim[7] := PixDim7.Value;
          scl_slope := Scale.value;
          scl_inter := Intercept.value;
          if HeaderMagicDrop.ItemIndex = 2 then
             Magic := kNIFTI_MAGIC_EMBEDDED_HDR
          else if HeaderMagicDrop.ItemIndex = 1 then
             Magic := kNIFTI_MAGIC_SEPARATE_HDR
          else
             Magic := 0; //not saed as NIFTI
          str := getStr(CommentEdit.text, 80);
          for i := 1 to 80 do
              descrip[i] := str[i];
          str := getStr(data_typeEdit.text, 10);
          for i := 1 to 10 do
              data_type[i] := str[i];
          str := getStr(db_nameEdit.text, 18);
          for i := 1 to 18 do
              db_name[i] := str[i];
          str := getStr(aux.text, 24);
          for i := 1 to 24 do
              aux_file[i] := str[i];
          str := getStr(intent_nameEdit.text, 16);
          for i := 1 to 16 do
              intent_name[i] := str[i];

          xyzt_units := xyzt_sizeDrop.ItemIndex;
          xyzt_units := xyzt_units+ (DropItem2time_units(xyzt_timeDrop.ItemIndex));
      intent_code := DropIndex2IntentCode(IntentCodeDrop.ItemIndex, true);

	  intent_p1 := intent_p1Edit.value;
          intent_p2 := intent_p2Edit.value;
          intent_p3 := intent_p3Edit.value;
          extents:= round(ext.value);
          session_error := round(ses.value);
          regular := chr(round(reg.value));
          dim_Info := FreqDimDrop.ItemIndex+(PhaseDimDrop.ItemIndex shl 2)+(SliceDimDrop.ItemIndex shl 4);
          slice_start := round(slice_startEdit.value);
          slice_end := round(slice_endEdit.value);
          slice_code := SliceCodeDrop.ItemIndex;
          Slice_duration := (Slice_DurationEdit.value);
          toffset := (toffsetEdit.value);
          cal_max := cmax.value;
          cal_min := cmin.value;
          glmax := round(gmax.value);
          glmin := round(gmin.value);
          //Next: 3D orientation rotations
          pixdim[0] := QFacEdit.value;
          qform_code := QFormDrop.ItemIndex;
          quatern_b := quatern_bEdit.value;
          quatern_c := quatern_cEdit.value;
          quatern_d := quatern_dEdit.value;
          qoffset_x := qoffset_x_Edit.value;
          qoffset_y := qoffset_y_Edit.value;
          qoffset_z := qoffset_z_Edit.value;
          sform_code  :=  SFormDrop.ItemIndex;
          srow_x[0] := srow_x0Edit.value;//12 affine matrix values
          srow_x[1] := srow_x1Edit.value;
          srow_x[2] := srow_x2Edit.value;
          srow_x[3] := srow_x3Edit.value;
          srow_y[0] := srow_y0Edit.value;
          srow_y[1] := srow_y1Edit.value;
          srow_y[2] := srow_y2Edit.value;
          srow_y[3] := srow_y3Edit.value;
          srow_z[0] := srow_z0Edit.value;
          srow_z[1] := srow_z1Edit.value;
          srow_z[2] := srow_z2Edit.value;
          srow_z[3] := srow_z3Edit.value;
     end; //with lHdr
     //zero_intercept := intercept.value;
end;

function NIFTIhdr_SaveHdr(var lFilename: string; var lHdr: TNIFTIhdr; lAllowOverwrite, lIsNativeEndian: boolean): boolean;
var
    lExt: string;
    lOutHdr: TNIFTIhdr;
    lF: File;
    lOverwrite: boolean;
begin
     lOverwrite := false; //will we overwrite existing file?
     lExt := upcase(ExtractFileExt(lFilename));
     if (lExt = '.NII') then lHdr.magic := kNIFTI_MAGIC_EMBEDDED_HDR;
     if (lExt = '.HDR') then lHdr.magic := kNIFTI_MAGIC_SEPARATE_HDR;
     result := false; //assume failure
	 if lHdr.magic = kNIFTI_MAGIC_EMBEDDED_HDR then begin
		 if (lExt = '.GZ') or (lExt = '.NII.GZ') then begin
			showmessage('Unable to save .nii.gz headers (first ungzip your image if you wish to edit the header)');
			exit;
		 end;
		 lFilename := changefileext(lFilename,'.nii')
	 end else
         lFilename := changefileext(lFilename,'.hdr');
     (*if ((sizeof(TNIFTIhdr))> DiskFree(lFileName)) then begin
        ShowMessage('There is not enough free space on the destination disk to save the header. '+kCR+
        lFileName+ kCR+' Bytes Required: '+inttostr(sizeof(TNIFTIhdr)) );
        exit;
     end;*)
     (*if Fileexists(lFileName) then begin
         if lAllowOverwrite then begin
            case MessageDlg('Do you wish to modify the existing file '+lFilename+'?', mtConfirmation,[mbYes, mbNo], 0) of	{ produce the message dialog box }
             6: lOverwrite := true; //6= mrYes, 7=mrNo... not sure what this is for Linux. Hardcoded as we do not include Form values
        end;//case
         end else
             showmessage('Error: the file '+lFileName+' already exists.');
         if not lOverwrite then Exit;
     end;*)
     if Fileexists(lFileName) and (not lAllowOverwrite) then begin
    	showmessage('Error: the file '+lFileName+' already exists.');
        exit;
     end;
     if Fileexists(lFileName) then
        lOverwrite := true;
     if lHdr.magic = kNIFTI_MAGIC_EMBEDDED_HDR then
        if lHdr.vox_offset < sizeof(TNIFTIHdr) then
           lHdr.vox_offset := sizeof(TNIFTIHdr)+4; //352 embedded images MUST start after header
     //if (lHdr.magic = kNIFTI_MAGIC_SEPARATE_HDR) then
     //      lHdr.vox_offset := 0; //embedded images MUST start after header
     result := true;
     move(lHdr, lOutHdr, sizeof(lOutHdr));
     if lIsNativeEndian = false then
        NIFTIhdr_SwapBytes (lOutHdr);{swap to big-endianformat}
     Filemode := 1;
     AssignFile(lF, lFileName); {WIN}
     if lOverwrite then //this allows us to modify just the 348byte header of an existing NII header without touching image data
         Reset(lF,sizeof(TNIFTIhdr))
     else
         Rewrite(lF,sizeof(TNIFTIhdr));
     BlockWrite(lF,lOutHdr, 1  {, NumWritten});
     CloseFile(lF);
     Filemode := 2;
end; //func NIFTIhdr_SaveHdr

procedure THdrForm.Save1Click(Sender: TObject);
var lHdr: TNIFTIhdr;
    lFilename, lExt: string;
begin
  if (HeaderMagicDrop.ItemIndex >= 3) then begin
     showmessage('Unable to save NIfTI2 headers');
     exit;
  end;
  if not SaveHdrDlg.Execute then exit;
  lExt := upcase(ExtractFileExt(SaveHdrDlg.Filename));
  if (lExt = '.GZ') or (lExt = '.NII.GZ') then begin
     showmessage('Unable to save .nii.gz headers (first ungzip your image if you wish to edit the header)');
     exit;
  end;
  NII_Clear(lHdr);
  ReadHdrForm (lHdr);
  if (lExt <> '.HDR') and (lExt <> '.NII') then begin
     if lHdr.magic = kNIFTI_MAGIC_SEPARATE_HDR then
        SaveHdrDlg.Filename := SaveHdrDlg.Filename +'.hdr'
     else begin
         SaveHdrDlg.Filename := SaveHdrDlg.Filename +'.nii';
     end;
  end;
  lFilename := SaveHdrDlg.Filename;
  //999 ImgForm.SaveDialog1.InitialDir := extractfiledir(lFilename);
  if not NIFTIhdr_SaveHdr (lFilename, lHdr, true, HdrForm.Endian.ItemIndex = 0) then exit;
  StatusBar1.Panels[1].text := 'wrote: '+lFilename;
end;

procedure THdrForm.TabMenuClick(Sender: TObject);
begin
     PageControl1.ActivePage := PageControl1.Pages[(Sender as TMenuItem).Tag];
end;

procedure THdrForm.Exit1Click(Sender: TObject); //Quit the program or form
begin
     Close;
end;


procedure THdrForm.FormCreate(Sender: TObject);
var lHdr: TNIFTIhdr;
    v: TVec3i;
begin
  NII_Clear(lHdr);
  v.x := lHdr.dim[1]; v.y := lHdr.dim[2]; v.z := lHdr.dim[3];
  HdrForm.WriteHdrForm (lHdr, true,'', v); //show default header
  {$IFDEF Darwin}
  Save1.ShortCut := ShortCut(Word('S'), [ssMeta]);
  Exit1.ShortCut := ShortCut(Word('W'), [ssMeta]);
  Dimensions1.ShortCut := ShortCut(Word('A'), [ssMeta]);
  Rotations1.ShortCut := ShortCut(Word('B'), [ssMeta]);
  ImageIntensity1.ShortCut := ShortCut(Word('I'), [ssMeta]);
  Statistics1.ShortCut := ShortCut(Word('D'), [ssMeta]);
  FunctionalMRI1.ShortCut := ShortCut(Word('E'), [ssMeta]);
  Optional1.ShortCut := ShortCut(Word('F'), [ssMeta]);
  {$ENDIF}
end;

procedure THdrForm.HeaderMagicDropSelect(Sender: TObject);
var lHdrIndex: integer;
begin
     lHdrIndex := HeaderMagicDrop.ItemIndex; //0=unknown, 1=nifti hdr+img, 2=nifti .nii embedded
     (*if lHdrIndex = 1 then begin//nifti hdr+img, offset must be = 0
        OffsetEdit.MinValue := 0;
        //OffsetEdit.Enabled := false;
        OffsetEdit.value := 0;
     end else*) if lHdrIndex = 2 then begin//embedded header, offset must be at least 352
        //OffsetEdit.Enabled := true;
        if OffsetEdit.value < sizeof(TNIFTIHdr) then
           OffsetEdit.value := sizeof(TNIFTIHdr);
        OffsetEdit.MinValue := sizeof(TNIFTIHdr);
     end else begin //no embedded header... therefore offset can be zero
        OffsetEdit.MinValue := 0;
        //OffsetEdit.Enabled := true;

        if OffsetEdit.value = sizeof(TNIFTIHdr) then
		   OffsetEdit.value := 0;
     end;
end;

end.
