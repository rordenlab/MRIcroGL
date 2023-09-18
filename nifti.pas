unit nifti;
{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}
//{$IFDEF FPC}{$mode delphi} {$H+}{$ENDIF}

{$DEFINE CUSTOMCOLORS}
interface
{$include opts.inc} //for  DEFINE FASTGZ
{$DEFINE TIMER} //reports load times to stdout (Unix only)
{$IFDEF FPC}
 {$DEFINE GZIP}
{$ENDIF}
//{$DEFINE PARALLEL}  //for parallel Unix also edit CThreads in LPR file
{$DEFINE OPENFOREIGN} //used for q and s form as well as opening
{$DEFINE AFNI} //read Head/Brik statistics
{$IFNDEF OPENFOREIGN}{$IFDEF AFNI} error: AFNI requires OPENFOREIGN {$ENDIF} {$ENDIF}
{$DEFINE CPUGRADIENTS} //Computing volume gradients on the GPU is much faster than using the CPU
{$DEFINE CACHEUINT8} //save 8-bit data: faster requires RAM
{$DEFINE SSE}

{$DEFINE TIF} //load bitmaps, e.g. PNG, BMP, TIFFs not handled by NIfTI_TIFF
{$DEFINE BMP} //load bitmaps, e.g. PNG, BMP, TIFFs not handled by NIfTI_TIFF
{$DEFINE BZIP2}
//{$IFDEF MYTHREADS}
{$ModeSwitch nestedprocvars}
//{$ENDIF}
uses

  //{$IFDEF UNIX}cthreads, cmem,{$ENDIF}
  {$IFDEF AFNI}afni_fdr,{$ENDIF}
  {$IFDEF BZIP2}bzip2stream,{$ENDIF}
  {$IFDEF BMP} Graphics, GraphType,{$ENDIF}
  {$IFDEF PARALLEL}mtprocs,mtpcpu,{$ENDIF}
  {$IFDEF TIMER} DateUtils,{$ENDIF}
  {$IFDEF FASTGZ}
    {$IFDEF LIBDEFLATE}
    libdeflate,
    {$ENDIF}
    SynZip,

  {$ENDIF}
  {$IFDEF OPENFOREIGN} nifti_foreign, {$ENDIF}
  {$IFDEF CUSTOMCOLORS} colorTable,  {$ENDIF}
  {$IFDEF GZIP}zstream, umat, GZIPUtils, {$ENDIF} //Freepascal includes the handy zstream function for decompressing GZip files
  {$IFDEF TIF} nifti_tiff, {$ENDIF}
  {$IFDEF SSE}
    {$IFDEF CPUAARCH64}
       neon,
    {$ELSE}
  	sse,
    {$ENDIF}
  {$ENDIF}
  nifti_save,
  sortu, strutils, dialogs, clipbrd, SimdUtils, sysutils,Classes, nifti_types, Math, VectorMath, otsuml;
//Written by Chris Rorden, released under BSD license
//This is the header NIfTI format http://nifti.nimh.nih.gov/nifti-1/
//NIfTI is popular in neuroimaging - should be compatible for Analyze format
//   http://eeg.sourceforge.net/ANALYZE75.pdf
//NIfTI format images have two components:
// 1.) Header data provides image dimensions and details
// 2.) Image data
//These two components can be separate files: MRI.hdr, MRI.img
//  or a single file with the header at the start MRI.nii
//Note raw image daya begins vox_offset bytes into the image data file
//  For example, in a typical NII file, the header is the first 348 bytes,
//  but the image data begins at byte 352 (as this is evenly divisible by 8)
(*const
	 kDTIno = -1; //this is NOT a DTI V1 image
     kDTIscalar = 0; //this might be a DTI V1 image, but display as typical image
     kDTIrgb = 1; //display as RGB V1 image
     kDTIlines = 2; //display as lines  *)
Type

  TCluster = record
		CogXYZ, PeakXYZ: TVec3; //Center of Gravity, Peak
                Peak, SzMM3: single;
		PeakStructure, Structure: string[248];
  end;
  TClusters = array of TCluster;
  {$IFNDEF DYNRGBA}
  TRGBA0 = array [0..MAXINT] of TRGBA;
  TRGBAp = ^TRGBA0; //pointer to RGBA array
  {$ENDIF}

  TNIfTI = Class(TObject)  // This is an actual class definition :
      // Internal class field definitions - only accessible in this unit
      private
        fMat, fInvMat, fMatInOrient: TMat4;
        fMin, fMax, fAutoBalMin, fAutoBalMax, fWindowMin, fWindowMax: single;
        fOpacityPct: integer;
        fScale, fCutoutLow, fCutoutHigh : TVec3;
        fDim, fPermInOrient: TVec3i;
        fHdr, fHdrNoRotation : TNIFTIhdr;
        fKnownOrientation, fIsNativeEndian: boolean;
        fFileName,fShortName, fBidsName: string;
        {$IFDEF DYNRGBA}
        fVolRGBA: TRGBAs;
        {$ELSE}
        fVolRGBA: TRGBAp;
        {$ENDIF}
        //fLabels: TStringList;
        fhistogram: TLUT;
        fisLinearReslice, fIsOverlay, fIsDrawing: boolean; //booleans do not generate 32-bit RGBA images
        {$IFDEF CACHEUINT8}
        fVolumesTotal, fVolumeDisplayed, fVolumesLoaded, fOpacityPctCache8: integer;
        fWindowMinCache8, fWindowMaxCache8: single;
        fCache8: TUInt8s;
        {$ENDIF}
        {$IFDEF CUSTOMCOLORS}
        clut: TCLUT;
        {$ELSE}
        fLUT: TLUT;
        {$ENDIF}
        //init functions compute volume min/max and set default min/max brightness/contrast
        procedure InitUInt8();
        procedure InitInt16();
        procedure InitFloat32();
        procedure DetectV1();
        //display functions apply window min/max to generate image with desired brightness/contrast
        procedure SetDisplayMinMaxRGB24();
        //procedure SetDisplayMinMaxRGBV1();
        procedure LoadRGBVector();
        procedure DisplayLabel2Uint8();
        //procedure SetDisplayMinMaxFloat32();
        //procedure SetDisplayMinMaxInt16();
        //procedure SetDisplayMinMaxUint8();
        {$IFDEF PARALLEL}
        //procedure SetDisplayMinMaxParallel(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
        {$ENDIF}
        procedure ConvertUint16Int16();
        procedure Convert2Float();
        procedure Convert2RGB(); //convert RGBA -> RGB
        procedure Convert2UInt8(); //convert int8 -> int16
        procedure VolumeReslice(tarMat: TMat4; tarDim: TVec3i; isLinearReslice: boolean);
        procedure VolumeReorient();
        //procedure ApplyVolumeReorient(perm: TVec3i; outR: TMat4);
        function OpenNIfTI(): boolean;
        procedure MakeBorg(voxelsPerDimension: integer);
        procedure ApplyCutout();
        function loadForeign(FileName : AnsiString; var  rawData: TUInt8s; var lHdr: TNIFTIHdr): boolean;// Load 3D data
        function LoadRaw(FileName : AnsiString; out isNativeEndian: boolean): boolean;
        //function SaveBVox(bVoxFileName: string; var hdr: TNIFTIhdr; var img8: TUInt8s): boolean;
        //function SaveOsp(OspFileName: string; var hdr: TNIFTIhdr; var img8: TUInt8s): boolean;
        //function SaveNii(niftiFileName: string): boolean; overload;
        //function SaveFormatBasedOnExt(fnm: string; var hdr: TNIFTIhdr; var img8: TUInt8s): boolean; overload;
        //function SaveNii(fnm: string; var hdr: TNIFTIhdr; var img8: TUInt8s): boolean; overload;
        {$IFDEF GZIP}
        //function SaveGz(niftiFileName: string; var hdr: TNIFTIhdr; var img8: TUInt8s): boolean;
        {$IFDEF FASTGZ}function LoadFastGz(FileName : AnsiString; out isNativeEndian: boolean): boolean;{$ENDIF}
        function LoadGz(FileName : AnsiString; out isNativeEndian: boolean): boolean;
        {$ENDIF}
        function skipVox(): int64; //if fVolumeDisplayed > 0, skip this many VOXELS for first byte of data
        //function skipBytes(): int64; //if fVolumeDisplayed > 0, skip this many BYTES for first byte of data
        //function skipRawVolBytes(): TUInt8s;
        function VoiDescriptivesLabels(VoiRawBytes: TUInt8s): TStringList;
        //function CenterOfMass(idx: integer; out sizeMM3: single): TVec3;
        //procedure GenerateAtlasClusters8bit;
        procedure GenerateAtlasClusters;
        function rawAtlasMax: integer;
        function mm3toVox(mm3: single) : integer; //e.g. if 3x3x3mm voxels (27mm) and input is 28, return 2
        procedure initHistogram(Histo: TUInt32s =  nil);
        //procedure robustMinMax(var rMin, rMax: single);
      public
        RefreshCount: integer;
        fLabels: TStringList;
        fLabelMask: TUInt8s;
        fRawVolBytes: TUInt8s;
        LoadFewVolumes: boolean;
        SortClustersBySize : boolean;
        HiddenByCutout: boolean;
        //fDTImode: integer; //kDTIno, kDTIscalar,kDTIrgb,kDTIlines
        IsShrunken : boolean;
        IsFightInterpolationBleeding: boolean;
        IsInterpolated : boolean;
        ZeroIntensityInvisible: boolean;
        MaxTexMb: integer; //maximum number of voxels in any dimension
        isAntiAliasHugeTexMb: boolean;
        MinPixDim, MaxMM: single; //maximum mm in any dimension
        mmAsFrac: TVec3;
        {$IFDEF AFNI}
        afnis: TAFNIs;
        {$ENDIF}
        clusters: TClusters;
        clusterNotes: string[128]; //interpolated, label map, etc
        procedure MakeHistogram(Histo: TUInt32s; mn, mx: single; isClampExtremeValues : boolean = true; isIgnoreZeros: boolean = false);
        property InputReorientPermute: TVec3i read fPermInOrient;
        property IsNativeEndian: boolean read fIsNativeEndian;
        property VolumeDisplayed: integer read fVolumeDisplayed; //indexed from 0 (0..VolumesLoaded-1)
        property VolumesLoaded: integer read fVolumesLoaded; //1 for 3D data, for 4D 1..hdr.dim[4] depending on RAM
        property VolumesTotal: integer read fVolumesTotal; //1 for 3D data, for 4D 1..hdr.dim[4] depending on RAM
        property KnownOrientation: boolean read fKnownOrientation;
        {$IFDEF CUSTOMCOLORS}
        function GetColorTable: TLUT;
        //property ColorTable: TLUT read clut.fLUT write clut.fLUT;
        function FullColorTable: TCLUTrec;
        property CX: TCLUT read clut write clut;
        {$ELSE}
        property ColorTable: TLUT read fLUT write fLUT;
        {$ENDIF}
        //procedure SmoothMaskedImages();
        property Drawing: boolean read fIsDrawing;
        function AsFloats(): TFloat32s;
        procedure SortClusters;
        function NotZero(): TInt16s; //volume where voxels non-zero voxels are set to 1, voxels with intensity zero set to 0
        function NeedsUpdate(): boolean;
        function DisplayMinMax2Uint8(isForceRefresh: boolean = false): TUInt8s;
        procedure SaveRotated(fnm: string; perm: TVec3i);
        function RemoveSmallClusters(thresh, mm: double; NeighborMethod: integer = 1): single; //returns surviving mm3
        function SaveCropped(fnm: string; crop: TVec6i; cropVols: TPoint): boolean;
        function SaveAs8Bit(fnm: string; Lo,Hi: single): boolean;
        function SaveRescaled(fnm: string; xFrac, yFrac, zFrac: single; OutDataType, Filter: integer; isAllVolumes: boolean): boolean;
        procedure SaveAsSourceOrient(NiftiOutName: string; rawVolBytesIn: TUInt8s);  overload;
        procedure SaveAsSourceOrient(NiftiOutName, HdrDescrip, IntentName: string; rawVolBytesIn: TUInt8s; dataType: integer; intentCode: integer = 0; mn: single = 0; mx: single = 0);
        function FracToSlice(Frac: TVec3): TVec3i; //given volume fraction return zero-indexed slice
        function FracShiftSlice(Frac: TVec3; sliceMove: TVec3i): TVec3; //move a desired number of slices
        function FracMM(Frac: TVec3): TVec3; //return mm coordinates given volume fraction
        function MMFrac(MM: TVec3): TVec3; //return frac coordinates given volume mm
        function VoiDescriptives(VoiRawBytes: TUInt8s): TStringList;
        function VoxMM3: single; //voxel volume of resliced data
        function VoxIntensityString(vox: int64): string; overload;
        function VoxIntensityString(vox: TVec3i): string; overload;
        function VoxIntensity(vox: int64): single; overload; //return intensity of voxel at coordinate
        function VoxIntensity(vox: TVec3i): single; overload; //return intensity of voxel at coordinate
        function VoxIntensity(frac: TVec3): single; overload;//return intensity of voxel fraction
        function VoxIntensityArray(vox: TVec3i): TFloat32s; overload;
        function VoxIntensityArray(roi: TUInt8s): TFloat32s; overload;
        function nifti_smooth_gauss(var img: TFloat32s; SigmammX, SigmammY, SigmammZ: single; nVol: int64): integer;
        function EdgeMap(isSmooth: boolean): TUInt8s;
        function Scaled2Raw(lScaled: single): single;
        function SeedCorrelationMap(vox: TVec3i; isZ: boolean): TFloat32s; overload;
        function SeedCorrelationMap(roi: TUInt8s; isZ: boolean): TFloat32s; overload;
        function SeedCorrelationMap(vSeed: TFloat32s; isZ: boolean): TFloat32s; overload;
        property OpacityPercent: integer read fOpacityPct write fOpacityPct; //0=transparent, 50=translucent, 100=opaque
        property Histogram: TLUT read fhistogram;
        property VolumeMin: single read fMin; //darkest voxel in volume
        property VolumeMax: single read fMax; //brightest voxel in volume
        property SuggestedDisplayMin: single read fAutoBalMin;
        property SuggestedDisplayMax: single read fAutoBalMax;
        property DisplayMin: single read fWindowMin; //write warning: changes only isplayed if you call SetDisplayMinMax()
        property DisplayMax: single read fWindowMax; //write warning: changes only isplayed if you call SetDisplayMinMax()
        property Header: TNIFTIhdr read fhdr;
        property HeaderNoRotation: TNIfTIhdr read fHdrNoRotation;
        property Scale: TVec3 read fScale;
        property ShortName: String read fShortName;
        property Mat: TMat4 read fMat;
        property InvMat: TMat4 read fInvMat;
        property Dim: TVec3i read fDim;
        {$IFDEF DYNRGBA}
        property VolRGBA: TRGBAs read fVolRGBA;
        {$IFDEF CPUGRADIENTS}
        procedure CreateGradientVolume (rData: TRGBAs; Xsz,Ysz,Zsz: integer; out Vol : TRGBAs);
        function GenerateGradientVolume: TRGBAs; overload;
        {$ENDIF}
        {$ELSE}
        property VolRGBA: TRGBAp read fVolRGBA;
        {$IFDEF CPUGRADIENTS}
        procedure CreateGradientVolume (rData: TRGBAp; Xsz,Ysz,Zsz: integer; out Vol : TRGBAp);
        function GenerateGradientVolume: TRGBAp; overload;
        {$ENDIF}
        {$ENDIF}
        property VolRawBytes: TUInt8s read fRawVolBytes;
        property Filename: String read fFileName;
        property BidsName: String read fBidsName;
        procedure Sharpen();
        procedure Smooth(Mask: TUInt8s = nil);
        procedure Mask(MaskingVolume: TUInt8s; isPreserveMask: boolean);
        procedure RemoveHaze(isSmoothEdges: boolean = true; isSingleObject: boolean = true; OtsuLevels : integer = 5);
        procedure GPULoadDone();
        function SaveFormatBasedOnExt(fnm: string): boolean; overload; //filename determines format.nii, .tif, .bvox, .nrrd, .nhdr etc...
        function Load(niftiFileName: string): boolean; overload;
        function Load(niftiFileName: string; tarMat: TMat4; tarDim: TVec3i; isInterpolate: boolean; hdr: TNIFTIhdr; img: TFloat32s; isUINT8: boolean = false): boolean; overload;
        function Load(niftiFileName: string; tarMat: TMat4; tarDim: TVec3i; isInterpolate: boolean; volumeNumber: integer = 0; isKeepContrast: boolean = false): boolean; overload;
        procedure SetDisplayMinMax(newMin, newMax: single); overload;
        procedure SetDisplayMinMax(isForceRefresh: boolean = false); overload;
        procedure SetDisplayMinMaxNoUpdate(newMin, newMax: single); overload;
        procedure SetCutoutNoUpdate(CutoutLow, CutoutHigh: TVec3);
        procedure GenerateClusters(thresh, smallestClusterMM3: single; NeighborMethod: integer = 1; isDarkAndBright: boolean = false); overload;
        //thresh, mm: double; NeighborMethod: integer
        procedure GenerateClusters(LabelMap: TNIfTI; thresh, smallestClusterMM3: single; NeighborMethod: integer = 1; isDarkAndBright: boolean = false); overload;
        function DisplayRGBGreen(): TUInt8s;
        procedure DisplayRGB();
        procedure SetDisplayColorScheme(clutFileName: string; cTag: integer);
        procedure SetDisplayVolume(newDisplayVolume: integer);
        //procedure SetLabelMask(Idx: integer; isMasked: boolean);
        procedure ForceUpdate;
        procedure SetClusters(c: TClusters; notes: string);
        constructor Create(); overload; //empty volume
        constructor Create(niftiFileName: string; backColor: TRGBA;  tarMat: TMat4; tarDim: TVec3i; isInterpolate: boolean; hdr: TNIFTIhdr; img: TFloat32s; isUINT8: boolean = false); overload;
        constructor Create(niftiFileName: string; backColor: TRGBA; lLoadFewVolumes: boolean; lMaxTexMb: integer; out isOK: boolean); overload; //background
        constructor Create(niftiFileName: string; backColor: TRGBA; tarMat: TMat4; tarDim: TVec3i; isInterpolate: boolean; out isOK: boolean; lLoadFewVolumes: boolean = true; lMaxTexMb: integer = 640); overload; //overlay
        constructor Create(tarMat: TMat4; tarDim: TVec3i); overload; //blank drawing
        procedure SetIsLabels(b: boolean);
        function IsLabels: boolean; //e.g. template map: should be drawn nearest neighbor (border of area 19 and 17 is NOT 18)
        destructor Destroy; override;
  end;
  {$IFDEF CPUGRADIENTS}
  {$IFDEF DYNRGBA}
  procedure CreateGradientVolumeX (rData: TUInt8s; Xsz,Ysz,Zsz, isRGBA: integer; out VolRGBA : TRGBAs);
  {$ELSE}
  procedure CreateGradientVolumeX (rData: TUInt8s; Xsz,Ysz,Zsz, isRGBA: integer; out VolRGBA : TRGBAp);
  procedure SetLengthP(var S: TRGBAp; Len: SizeInt);
  {$ENDIF}
  {$ENDIF}
  function extractfileextX(fnm: string): string;

implementation

uses nifti_resize;
//uses reorient;

(*procedure TNIfTI.SetLabelMask(Idx: integer; isMasked: boolean);
var
   i: integer;
begin
     if fLabels.Count < 1 then exit;
     if fLabelMask = nil then begin
        SetLength(fLabelMask, fLabels.Count );
        for i := 0 to fLabels.Count - 1 do
            fLabelMask[i] := 0;
     end;

end;*)

{$IFDEF CUSTOMCOLORS}
function TNIfTI.GetColorTable: TLUT;
begin
     result := clut.LUT;
end;
{$ENDIF}

FUNCTION specialsingle (var s:single): boolean; inline;
//returns true if s is Infinity, NAN or Indeterminate
CONST kSpecialExponent = 255 shl 23;
VAR Overlay: LongInt ABSOLUTE s;
BEGIN
 IF ((Overlay AND kSpecialExponent) = kSpecialExponent) THEN
   RESULT := true
 ELSE
   RESULT := false;
END; //specialsingle()

procedure TNIfTI.SetClusters(c: TClusters; notes: string);
begin
     clusterNotes := notes+ 'From AFNI: volume in voxels, peak intensity not reported';
     //zz setlength(clusters, length(c));
     if length(c) < 1 then begin
       setlength(clusters, 0);
       exit;
     end;
     clusters := copy(c, 0, maxint);
end;

procedure  Coord(var lV: TVec4; lMat: TMat4);
//transform X Y Z by matrix
var
  lXi,lYi,lZi: single;
begin
  lXi := lV[0]; lYi := lV[1]; lZi := lV[2];
  lV[0] := (lXi*lMat[0,0]+lYi*lMat[0,1]+lZi*lMat[0,2]+lMat[0,3]);
  lV[1] := (lXi*lMat[1,0]+lYi*lMat[1,1]+lZi*lMat[1,2]+lMat[1,3]);
  lV[2] := (lXi*lMat[2,0]+lYi*lMat[2,1]+lZi*lMat[2,2]+lMat[2,3]);
end;

function voxelCenter(frac: single; dim: integer): single;
//if 3 voxels span 0..1 then centers are at 0.25/0.5/0.75 - voxels frac is 1/(n+1)
//if 4 voxels, centers 0.125, 0.375. 0.625. 0.875
// so val 0..0.33 will return 0.25
var
  sliceFrac: double;
  slice: integer;
begin
  if (dim < 2) then exit(0.5);
  sliceFrac := 1/dim;
  slice := trunc(frac/sliceFrac);
  if (slice >= dim) then slice := dim - 1; //e.g. frac 1.0 = top slice
  if (slice < 0) then slice := 0; //only with negative fraction!
  result := (sliceFrac * (slice+0.5));
end;

function frac2slice(frac: single; dim: integer): integer;
var
  sliceFrac: double;
begin
     if (dim < 2) then exit(0);
     sliceFrac := 1/dim;
     result := trunc(frac/sliceFrac);
     if (result >= dim) then result := dim - 1; //e.g. frac 1.0 = top slice
     if (result < 0) then result := 0; //only with negative fraction!
end;

function SliceMM(vox0: TVec3; Mat: TMat4): TVec3;
var
  v4: TVec4;
begin
  v4 := Vec4(vox0.x, vox0.y, vox0.z, 1.0); //Vec4(X-1,Y-1,Z-1, 1); //indexed from 0, use -1 if indexed from 1
  Coord(v4, Mat);
  result := Vec3(v4.x, v4.y, v4.z);
end;


function TNIfTI.skipVox(): int64;
begin
     if (fVolumeDisplayed <= 0) or (fIsOverlay) then exit(0);
     result := prod(fDim) * fVolumeDisplayed;
end;

procedure  TNIfTI.ForceUpdate;
begin
  fWindowMinCache8 := infinity;
end;

procedure TNIfTI.SetIsLabels(b: boolean);
var
  i: smallint;
begin
     //see todo in main unit
     i := fHdr.intent_code;
     if b then
        fHdr.intent_code := kNIFTI_INTENT_LABEL
     else if fHdrNoRotation.intent_code = kNIFTI_INTENT_LABEL then
          fHdr.intent_code := kNIFTI_INTENT_NONE
     else
           fHdr.intent_code := fHdrNoRotation.intent_code;
     if (i <> fHdr.intent_code ) then
     fWindowMinCache8 := infinity; //force update
end;

function TNIfTI.IsLabels: boolean;
//return TRUE for neurolabels
begin
 result :=  (fHdr.intent_code= kNIFTI_INTENT_LABEL);
end;

function TNIfTI.VoiDescriptivesLabels(VoiRawBytes: TUInt8s): TStringList;
const
  kTab = chr(9);
var
   i, j, nVox, nVoi, sumTotal : int64;
   sumVois, sumVoisNot0: TUInt32s;
   frac: single;
begin
     result := TStringList.Create();
     if fLabels.Count < 1 then exit;
     nVox := length(VoiRawBytes);
     nVoi := fLabels.Count;
     if (nVox <> (dim.x * dim.y * dim.z)) then exit;
     setlength(sumVois, nVoi);
     setlength(sumVoisNot0, nVoi);
     for i := 0 to (nVoi -1) do begin
         sumVois[i] := 0;
         sumVoisNot0[i] := 0;
     end;
     sumTotal := 0;
     for i := 0 to (nVox -1) do begin
         j :=  round(VoxIntensity(i));
         if j = 0 then continue;
         sumVois[j] := sumVois[j] + 1;
         if (VoiRawBytes[i] = 0) then continue;
         sumTotal := sumTotal + 1;
         if (j >= nVox) then continue;
         sumVoisNot0[j] := sumVoisNot0[j] + 1;
     end;
     result.Add('Background image: '+ShortName);
     result.Add(format('Regions: ', [nVoi]));
     result.Add(format('TotalVoxelsNotZero%s%d', [kTab, sumTotal]));
     result.Add('Background image: '+ShortName);
     result.Add('Index'+kTab+'Name'+kTab+'numVox'+kTab+'numVoxNotZero'+kTab+'fracVoxNotZero');
     for i := 0 to (fLabels.Count -1) do begin
         if sumVoisNot0[i] < 1 then continue;
         if sumVois[i] < 1 then continue;
         frac := sumVoisNot0[i] / sumVois[i];
         if fLabels[i] = '' then
            result.Add(format('%d%s<%d>%s%d%s%d%s%g', [i,kTab, i, kTab, sumVois[i], kTab, sumVoisNot0[i],kTab,frac]))
         else
             result.Add(format('%d%s"%s"%s%d%s%d%s%g', [i,kTab, fLabels[i], kTab, sumVois[i], kTab, sumVoisNot0[i],kTab,frac]));
     end;
     sumVois := nil;
     sumVoisNot0 := nil;
end;

function RealToStr(lR: double {was extended}; lDec: integer): string;
begin
     result := FloatToStrF(lR, ffFixed,7,lDec);
end;

function TNIfTI.VoiDescriptives(VoiRawBytes: TUInt8s): TStringList;
const
  kTab = ' ';//chr(9);
var
   x, y, z, lInc, i, nVox, nVoi : int64;
   lROIVol: array [1..3] of int64;
   lROISum,lROISumSqr,lROImin,lROImax:array [1..3] of double;
   lCC,lVal,lSD,lROImean: double;
   lLabelStr, lStr: string;
   mm: TVec3;
procedure  AddVal( lRA: integer);
begin
  inc(lROIVol[lRA]);
  lROISum[lRA] := lROISum[lRA]+lVal;
  lROISumSqr[lRA] := lROISumSqr[lRA] + sqr(lVal);
  if lVal > lROImax[lRA] then
    lROImax[lRA] := lVal;
  if lVal < lROImin[lRA] then
    lROImin[lRA] := lVal;
end;//nested AddVal()
begin
     if fLabels.Count > 0 then begin
       result := VoiDescriptivesLabels(VoiRawBytes);
       exit;
     end;
     lLabelStr := 'Drawing ';
     result := TStringList.Create();
     result.Add('Background image: '+ShortName);
     nVox := length(VoiRawBytes);
     if (nVox <> (dim.x * dim.y * dim.z)) then exit;
     //center of mass
     for i := 1 to 3 do
         lROISum[i] := 0;
     i := 0;
     nVoi := 0;
     for z := 0 to (dim.z-1) do
         for y := 0 to (dim.y-1) do
             for x := 0 to (dim.x-1) do begin
                 if (VoiRawBytes[i] > 0) then begin
                    nVoi := nVoi + 1;
                    lROISum[1] := lROISum[1] + x;
                    lROISum[2] := lROISum[2] + y;
                    lROISum[3] := lROISum[3] + z;
                 end;
                 i := i + 1;
             end;
     if (nVoi = 0) then begin
        result.Add('Drawing empty');
        exit;
     end;
     lROISum[1] := lROISum[1] / nVoi;
     lROISum[2] := lROISum[2] / nVoi;
     lROISum[3] := lROISum[3] / nVoi;
     //mm := FracMM(Vec3(lROISum[1],lROISum[2],lROISum[3]));
     result.Add('Drawing Center of mass XYZvox '+RealToStr(lROISum[1],2)+'x'+RealToStr(lROISum[2],2)+'x'+RealToStr(lROISum[3],2));
     mm := SliceMM(Vec3(lROISum[1],lROISum[2],lROISum[3]), Mat);
     result.Add('Drawing Center of mass XYZmm '+RealToStr(mm.x,2)+'x'+RealToStr(mm.y,2)+'x'+RealToStr(mm.z,2));
     //statistics
     for i := 1 to 3 do begin
         lROIVol[i] := 0;
         lROISum[i] := 0;
         lROISumSqr[i] := 0;
         lROImax[i] := NegInfinity;
         lROImin[i] := Infinity;
     end;
     for i := 0 to (nVox -1) do begin
         if (VoiRawBytes[i] = 0) then continue;
         lVal := VoxIntensity(i);
         AddVal(1);
	 if lVal <> 0 then
	    AddVal(2);
	 if lVal > 0 then
	    AddVal(3);
     end;
     result.Add('Background image: '+ShortName);
     for lInc := 1 to 3 do begin
     	if lROIVol[lInc] > 1 then begin
     	   lSD := (lROISumSqr[lInc] - ((Sqr(lROISum[lInc]))/lROIVol[lInc]));
     	   if  (lSD > 0) then
     	       lSD :=  Sqrt ( lSD/(lROIVol[lInc]-1))
     	   else
     	       lSD := 0;
     	end else
     	    lSD := 0;
     	//next compute mean
     	if lROIVol[lInc] > 0 then begin
     	   lROImean := lROISum[lInc]/lROIVol[lInc];
     	end else begin //2/2008
                  lROImin[lInc] := 0;
                  lROImax[lInc] := 0;
                  lROImean := 0;
        end;
     	lcc := ((lROIVol[lInc]/1000)*fHdr.pixdim[1]*fHdr.pixdim[2]*fHdr.pixdim[3]);
     	case lInc of
     		3: lStr := 'VOI  >0 ';
     		2: lStr := 'VOI <>0 ';
     		else lStr := 'VOI     ';
     	end;
     	lStr := lStr+' nvox(cc)=min/mean/max=SD: '+inttostr(round(lROIVol[lInc]))+kTab+RealToStr(lCC,2)+kTab+'='+kTab+RealToStr(lROIMin[lInc],4)+kTab+realToStr(lROIMean,4)+kTab+realToStr(lROIMax[lInc],4)+kTab+'='+kTab+realtostr(lSD,4);
     	result.Add(lLabelStr+ lStr);
     end;
end;

function isStat(statCode: integer): boolean;
begin
     result := (statCode = kFUNC_ZT_TYPE) or (statCode = kFUNC_TT_TYPE) or (statCode = kFUNC_FT_TYPE) or (statCode = kFUNC_CT_TYPE) ;
end;

function TNIfTI.VoxIntensityString(vox: int64): string; overload;//return intensity or label of voxel at coordinate
var
   f, q : single;
   i: integer;
begin
     if fHdr.datatype = kDT_RGB then begin
        i := vox * 3;
        result := format('%d,%d,%d',[fRawVolBytes[i],fRawVolBytes[i+1],fRawVolBytes[i+2]]);
        exit;
     end;
 	 f :=  VoxIntensity(vox);
     {$IFDEF AFNI}
     if (f <> 0) and (length(AFNIs) > 0) and (fVolumeDisplayed  < length(AFNIs)) and (isStat(AFNIs[fVolumeDisplayed].jv) ) then begin
        q := VoxelIntensity2q(AFNIs[fVolumeDisplayed].FDRcurv, f);
        q := max(q, 0.0001);
        result := format(' %3.6g (q<%.4f)', [f,q]);
        exit;
     end;
     {$ENDIF}
     if IsLabels then begin
         i := round(f);
         if (i >= 0) and (i < fLabels.Count) then
            result := fLabels[i]
         else
             result := inttostr(i);//''; //todo
     end else
         result := format(' %3.6g', [f]);
end;

function TNIfTI.VoxIntensityString(vox: TVec3i): string; overload;//return intensity of voxel at coordinate
var
  i: int64;
begin
     i := vox.x + (vox.y * dim.x) + (vox.z * (dim.x * dim.y));
     result := '';
     if (i < 0) or (i >= (dim.x * dim.y * dim.z)) then exit;
     result := VoxIntensityString(i);
end;

function TNIfTI.VoxIntensity(frac: TVec3): single; overload;//return intensity of voxel fraction
begin
     result := VoxIntensity(FracToSlice(Frac));
end;

procedure printf (str: AnsiString);
begin
{$IFNDEF WINDOWS}
writeln(str);
{$ELSE}
if IsConsole then writeln(str);
{$ENDIF}
end;

//{$DEFINE FASTCORREL}
{$IFDEF FASTCORREL} //~10% faster, but may be less precise for large values.
function correl(var v: TFloat32s): double;
var
  i, n: int64;
  sum, mn: double;

begin
     n := length(v);
     if n < 2 then exit(0); //avoid div by zero
     //compute mean for seed
     sum := 0.0;
     for i := 0 to (n - 1) do
         sum := sum + v[i];
     mn := sum / n;
     result := 0.0;
     for i := 0 to (n - 1) do begin
         v[i] := (v[i]-mn);
         result := result + sqr(v[i]);
     end;
end;

function TNIfTI.SeedCorrelationMap(vox: TVec3i): TFloat32s;
//Based on Numerical Recipes. One suspects this might have poor precision for large values
// https://www.johndcook.com/blog/2008/11/05/how-to-calculate-pearson-correlation-accurately/
var
  vx, nVx, vol, nVol, volBytes: int64;
  vol8: TUInt8s;
  vol16: TInt16s;
  x, y, vol32: TFloat32s;
  sxx, syy, sxy: double;
  r: double;
  {$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
begin
 setlength(result, 0);
 if IsLabels then exit;
 if volumesLoaded < 2 then exit;
 if fHdr.bitpix = 24 then exit;
 {$IFDEF TIMER}startTime := now;{$ENDIF}
 nVx := fHdr.Dim[1]*fHdr.Dim[2]*fHdr.Dim[3];
 volBytes := nVx * (fHdr.bitpix shr 3);
 nVol := length(fRawVolBytes) div volBytes;
 if nVol <> fVolumesLoaded then exit;
 vx := vox.x + (vox.y * dim.x) + (vox.z * (dim.x * dim.y));
 if (vx < 0) or (vx >= nVx) then exit;
 setlength(result, nVx);
 setlength(x, nVol);
 setlength(y, nVol);
 vol8 := fRawVolBytes;
 vol16 := TInt16s(vol8);
 vol32 := TFloat32s(vol8);
 //load values for seed
 if fHdr.datatype = kDT_UINT8 then begin
    for vol := 0 to (nVol - 1) do
        x[vol] := vol8[vx + (vol * nVx)];
 end else if fHdr.datatype = kDT_INT16 then begin
   for vol := 0 to (nVol - 1) do
       x[vol] := vol16[vx + (vol * nVx)];
 end else if fHdr.datatype = kDT_FLOAT then begin
   for vol := 0 to (nVol - 1) do
       x[vol] := vol32[vx + (vol * nVx)];
 end;
 sxx := correl(x);
 //compute correlation for each voxel
 for vx := 0 to (nVx - 1) do begin
    if fHdr.datatype = kDT_UINT8 then begin
       for vol := 0 to (nVol - 1) do
           y[vol] := vol8[vx + (vol * nVx)];
    end else if fHdr.datatype = kDT_INT16 then begin
      for vol := 0 to (nVol - 1) do
          y[vol] := vol16[vx + (vol * nVx)];
    end else if fHdr.datatype = kDT_FLOAT then begin
      for vol := 0 to (nVol - 1) do
          y[vol] := vol32[vx + (vol * nVx)];
    end;
    syy := correl(y);
    sxy := 0.0;
    for vol := 0 to (nVol-1) do
        sxy := sxy + (x[vol] * y[vol]);
    r := sxy / sqrt(sxx*syy);
    result[vx] := r;
 end;
 {$IFDEF TIMER}printf(format('Correl time %d',[MilliSecondsBetween(Now,startTime)]));{$ENDIF}
end;

{$ELSE}

//{$DEFINE ONEPASSCORREL}
{$IFDEF ONEPASSCORREL}
function correl(var v: TFloat32s): double;
//stable one-pass method: extra divisions mean this is slower than 2 pass
//http://jonisalonen.com/2013/deriving-welfords-method-for-computing-variance/
//aka Welford’s method for computing variance
//https://www.strchr.com/standard_deviation_in_one_pass
var
  i, n: int64;
  sd, mn, delta: double;
begin
     n := length(v);
     if n < 2 then exit(0.0); //avoid div by zero
     //compute mean for seed
     sd := 0.0;
     mn := v[0];
     for i := 1 to (n - 1) do begin
         delta := v[i] - mn;
         mn := mn + delta / (i+1);
         sd := sd + delta*(v[i]- mn);
     end;
     sd := sqrt(sd / (n - 1));
     if (sd = 0.0) then exit; //no variance
     sd := 1/sd;
     for i := 0 to (n - 1) do
         v[i] := (v[i]-mn)*sd;
     result := sd;
end;
{$ELSE}
function correl(var v: TFloat32s): double;
var
  i, n: int64;
  sd, sum, mn: double;
begin
     n := length(v);
     if n < 2 then exit(0.0); //avoid div by zero
     //compute mean for seed
     sd := 0.0;
     sum := 0.0;
     for i := 0 to (n - 1) do
         sum := sum + v[i];
     mn := sum / n;
     for i := 0 to (n - 1) do
         sd := sd + sqr(v[i] - mn);
     sd := sqrt(sd / (n - 1));
     if (sd = 0.0) then exit(0.0); //no variance
     sd := 1/sd;
     for i := 0 to (n - 1) do
         v[i] := (v[i]-mn)*sd;
     result := sd;
end;
{$ENDIF}//ONEPASSCORREL
{$DEFINE FASTCORREL2}

{$IFDEF FASTCORREL2}
function correlR(var x, y: TFloat32s): single;
//assumes X already processed with correl()
// about 10% faster in practice than running correl() twice
var
  i, n: int64;
  r, sum, mn, sd: double;
begin
  n := length(x);
  if (n < 2) or (n <> length(y)) then exit(0.0); //avoid div by zero
  sum := 0.0;
  for i := 0 to (n - 1) do
      sum := sum + y[i];
  mn := sum / n;
  sd := 0.0;
  for i := 0 to (n - 1) do
      sd := sd + sqr(y[i] - mn);
  sd := sqrt(sd / (n - 1));
  if (sd = 0) then exit(0.0);
  sd := 1/sd;
  r := 0.0;
  for i := 0 to (n - 1) do begin
      r := r + (x[i] * ((y[i]-mn)*sd));
  end;
  r := r / (n - 1);
  exit(r);
end;
{$ENDIF} //FASTCORREL2

procedure transposeXY( img3Din: TFloat32s; var img3Dout: TFloat32s; var nxp, nyp, nz: int64);
//transpose X and Y dimensions: rows <-> columns
var
  nx, ny, vi, x, xo, y, z, zo: int64;
begin
 nx := nxp;
 ny := nyp;
 vi := 0; //volume offset
 for z := 0 to (nz -1) do begin
		zo := z * nx * ny;
		for y := 0 to (ny -1) do begin
			xo := 0;
			for x := 0 to (nx -1) do begin
             img3Dout[zo + xo + y] := img3Din[vi];
             xo += ny;
             vi += 1;
            end;
        end;
	end;
	nxp := ny;
	nyp := nx;
end;

procedure transposeXZ( img3Din: TFloat32s; var img3Dout: TFloat32s; var nxp, ny, nzp: int64);
//transpose X and Z dimensions: slices <-> columns
var
  nx, nyz, nz, vi, x, y, yo, z, zo: int64;
begin
 nx := nxp;
 nz := nzp;
 nyz := ny * nzp;
 vi := 0; //volume offset
 for z := 0 to (nzp -1) do begin
	for y := 0 to (ny -1) do begin
     yo := y * nz;
	 zo := 0;
     for x := 0 to (nx -1) do begin
      img3Dout[z + yo + zo] := img3Din[vi];
      zo += nyz;
      vi += 1;
      end;
     end;
    end;
	nxp := nz;
	nzp := nx;
end;

//Gaussian blur, both serial and parallel variants, https://github.com/neurolabusc/niiSmooth
procedure blurS(var img: TFloat32s; nx, ny: integer; xmm, Sigmamm: single);
var
	sigma, expd, wt, sum: single;
	cutoffvox, i, j, y: integer;
	k, kWeight, tmp: TFloat32s;
	kStart, kEnd: TInt32s;
	x, imgp: int64;
begin
	//make kernels
	if ((xmm = 0) or (nx < 2) or (ny < 1) or (Sigmamm <= 0.0)) then
		exit();
	sigma := (Sigmamm / xmm); //mm to vox
	//cutoffvox := ceil(4 * sigma); //filter width to 4 sigma (FSL): faster but lower precision AFNI_BLUR_FIRFAC = 2.5
	cutoffvox := ceil(2.5 * sigma); //filter width to 6 sigma: faster but lower precision AFNI_BLUR_FIRFAC = 2.5
	//printf(".Blur Cutoff (%g) %d\n", 4*sigma, cutoffvox);
	//validated on SPM12's 1.5mm isotropic mask_ICV.nii (discrete jump in number of non-zero voxels)
	//fslmaths mask -s 2.26 f6.nii //Blur Cutoff (6.02667) 7
	//fslmaths mask -s 2.24 f4.nii //Blur Cutoff (5.97333) 6
	cutoffvox := MAX(cutoffvox, 1);
	setlength(k, cutoffvox + 1); //FIR Gaussian
	expd := 2 * sigma * sigma;
	for i := 0 to cutoffvox do
		k[i] := exp(-1.0 * (i * i) / expd);
	//calculate start, end for each voxel in
	setlength(kStart, nx); //-cutoff except left left columns, e.g. 0, -1, -2... cutoffvox
	setlength(kEnd, nx); //+cutoff except right columns
	setlength(kWeight, nx); //ensure sum of kernel = 1.0
	for i := 0 to (nx - 1) do begin
		kStart[i] := MAX(-cutoffvox, -i); //do not read below 0
		kEnd[i] := MIN(cutoffvox, nx - i - 1); //do not read beyond final columnn
		if ((i > 0) and (kStart[i] = (kStart[i - 1])) and (kEnd[i] = (kEnd[i - 1]))) then begin //reuse weight
			kWeight[i] := kWeight[i - 1];
			continue;
		end;
		wt := 0.0;
		for j := kStart[i] to kEnd[i] do
			wt += k[abs(j)];
		kWeight[i] := 1 / wt;
		//printf("%d %d->%d %g\n", i, kStart[i], kEnd[i], kWeight[i]);
	end;
	//apply kernel to each row
	setlength(tmp, nx); //input values prior to blur
	imgp := 0; //pointer
	for y := 0 to (ny - 1) do begin
		//tmp := copy(img, imgp, nx);
        Move(img[imgp], tmp[0],nx*4);//src,dst
		for x := 0 to (nx - 1) do begin
			sum := 0;
			for i := kStart[x] to kEnd[x] do
				sum += tmp[x + i] * k[abs(i)];
			img[imgp + x] := sum * kWeight[x];
		end;
		imgp += nx;
	end; //blurX
	//free kernel
	tmp := nil;
	k := nil;
	kStart := nil;
	kEnd := nil;
	kWeight := nil;
end;

function TNIfTI.nifti_smooth_gauss(var img: TFloat32s; SigmammX, SigmammY, SigmammZ: single; nVol: int64): integer;
label
     DO_Y_BLUR, DO_Z_BLUR;
var
	nRow, nvox3D, nx, ny, nz, v, vo: int64;
    dx, dy, dz: single;
    img3D: TFloat32s;
begin
    //https://github.com/afni/afni/blob/699775eba3c58c816d13947b81cf3a800cec606f/src/edt_blur.c
    if (nVol <> 1) then
        exit(123);//only supports 3D
    nx := fHdr.Dim[1];
    ny := fHdr.Dim[2];
    nz := fHdr.Dim[3];
    dx := abs(fHdr.PixDim[1]);
    dy := abs(fHdr.PixDim[2]);
    dz := abs(fHdr.PixDim[3]);
    if ((nx < 2) or (ny < 2) or (nz < 1) or (dx = 0) or (dy = 0) or (dz = 0)) then
		exit( 1);
	//if (nim->datatype != DT_CALC) then
	//	exit( 1);
	if ((SigmammX = 0) and (SigmammY = 0) and (SigmammZ = 0)) then
		exit(0); //all done: no smoothing, e.g. small kernel for difference of Gaussian
	if (SigmammX < 0) then //negative values for voxels, not mm
		SigmammX := -SigmammX  * dx;
	if (SigmammY < 0) then //negative values for voxels, not mm
		SigmammY := -SigmammY  * dy;
	if (SigmammZ < 0) then //negative values for voxels, not mm
		SigmammZ := -SigmammZ  * dz;
	nvox3D := nx * ny * MAX(nz, 1);
	if ((nvox3D * nVol) < 1) then
		exit( 1);
	if (SigmammX <= 0.0) then
		goto DO_Y_BLUR;
	//BLUR X
    nRow := ny * nz * nVol;
    blurS(img, nx, nRow, dx, SigmammX);
    //BLUR Y
    DO_Y_BLUR:
    if (SigmammY <= 0.0) then
		goto DO_Z_BLUR;
    nRow := nx * nz; //transpose XYZ to YXZ and blur Y columns with XZ Rows
    setlength(img3D, nvox3D);
	for v := 0 to (nVol - 1) do begin //transpose each volume separately
		vo := v * nvox3D; //volume offset
		transposeXY(@img[vo], img3D, nx, ny, nz);
		blurS(img3D, fHdr.Dim[2], nRow, dy, SigmammY);
		transposeXY(img3D, img, nx, ny, nz);
	end;
    img3D := nil;
    //BLUR Z
    DO_Z_BLUR:
	if ((SigmammZ <= 0.0) or (nz < 2)) then
		exit(0); //all done!
	nRow := nx * ny; //transpose XYZ to ZXY and blur Z columns with XY Rows

    setlength(img3D, nvox3D);
	for v := 0 to (nVol - 1) do begin //transpose each volume separately
        vo := v * nvox3D; //volume offset
		transposeXZ(@img[vo], img3D, nx, ny, nz);
		blurS(img3D, fHdr.Dim[3], nRow, dz, SigmammZ);
		transposeXZ(img3D, img, nx, ny, nz);
	end;
    img3D := nil;
    exit(0);
end;

function TNIfTI.EdgeMap(isSmooth: boolean): TUInt8s;
var
  i, vx, nx, ny, nz, nxy, nxyz, x, y, z: int64;
  vol8: TUInt8s;
  vol25, vol40: TFloat32s;
  val: single;
begin
 setlength(result, 0);
 if IsLabels then exit;
 if fHdr.bitpix = 24 then exit;
 nX := fHdr.Dim[1];
 nY := fHdr.Dim[2];
 nZ := fHdr.Dim[3];
 if (nx < 3) or (ny < 3) or (nz < 3) then exit;
 nxy := nx * ny;
 nxyz := nxy * nz;
 setlength(vol25, nxyz);
 vol8 := fCache8;
 for vx := 0 to (nxyz -1) do
  	vol25[vx] := vol8[vx];
 nifti_smooth_gauss(vol25, 2.5, 2.5, 2.5, 1);
 vol40 := Copy(vol25, Low(vol25), Length(vol25));
 //use 2mm blur as input for 4mm blur, allowing 3.122498999
 nifti_smooth_gauss(vol40, 3.122498999 , 3.122498999 , 3.122498999 , 1);
 //scale 0..1
 for vx := 0 to (nxyz - 1) do begin
     vol25[vx] := vol25[vx] - vol40[vx];
     vol40[vx] := 0;
 end;
 for z := 1 to (nz - 2) do begin
 	for y := 1 to (ny - 2) do begin
 		for x := 1 to (nx - 2) do begin
 			i := x + (y * nx) + (z * nxy);//for 4D + (v * nxyz);
 			val := vol25[i];
 			if (val <= 0.0) then continue; //one sided edge
 			//logic: pos*neg = neg; pos*pos=pos; neg*neg=neg
 			//check six neighbors that share a face
 			if (val * vol25[i-1] < 0) then vol40[i] := 1.0;
 			if (val * vol25[i+1] < 0) then vol40[i] := 1.0;
 			if (val * vol25[i-nx] < 0) then vol40[i] := 1.0;
 			if (val * vol25[i+nx] < 0) then vol40[i] := 1.0;
 			if (val * vol25[i-nxy] < 0) then vol40[i] := 1.0;
 			if (val * vol25[i+nxy] < 0) then vol40[i] := 1.0;
 		end; //x
 	end; //y
 end; //z
 vol25 := nil;
 setlength(result, nXYZ);
 for vx := 0 to (nXYZ - 1) do
     result[vx] := round(vol40[vx] * 255);
 vol40 := nil;
end;

{$IFDEF SOBELUNUSED} //DoG is better
function TNIfTI.EdgeMap(isSmooth: boolean): TUInt8s;
//blur
var
  vx, nXYZ: int64;
  mn, mx, scale255: single;
  vol8: TUInt8s;
  vol25: TFloat32s;
begin
 setlength(result, 0);
 if IsLabels then exit;
 if fHdr.bitpix = 24 then exit;
 nXYZ := fHdr.Dim[1] * fHdr.Dim[2] * fHdr.Dim[3];
 setlength(vol25, nXYZ);
 vol8 := fCache8;
 for vx := 0 to (nXYZ -1) do
  	vol25[vx] := vol8[vx];
 nifti_smooth_gauss(vol25, 2.5, 2.5, 2.5, 1);
 //scale 0..1
 mn := infinity;
 mx := -infinity;
 for vx := 0 to (nXYZ - 1) do begin
     if specialsingle(vol25[vx]) then continue;
     mn := min(mn, vol25[vx]);
     mx := max(mx, vol25[vx]);
 end;
 if (mn >= mx) then begin
   setlength(vol25, 0);
   exit;
 end;
 scale255 := 255.0 / (mx - mn);
 setlength(result, nXYZ);
 for vx := 0 to (nXYZ - 1) do
     result[vx] := round(scale255 * (vol25[vx] - mn));
 setlength(vol25, 0);
end;
{$ENDIF}
(*
function TNIfTI.EdgeMap(isSmooth: boolean): TUInt8s;
//https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/programs/3dedge3_sphx.html
// 2dedge3 uses R. Deriche formula https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.476.5736&rep=rep1&type=pdf
//https://en.wikipedia.org/wiki/Sobel_operator#Extension_to_other_dimensions
//optional compile:
// Asjad and M. Deriche: https://www.researchgate.net/publication/283280465_A_new_approach_for_salt_dome_detection_using_a_3D_multidirectional_edge_detector
//{$DEFINE ASJAD}
var
  vx, nXYZ: int64;
  mn, mx, scale255: single;
  vol8: TUInt8s;
  vol32: TFloat32s;
begin
 setlength(result, 0);
 if IsLabels then exit;
 if fHdr.bitpix = 24 then exit;
 nXYZ := fHdr.Dim[1] * fHdr.Dim[2] * fHdr.Dim[3];
 setlength(vol32, nXYZ);
 vol8 := fCache8;
 for vx := 0 to (nXYZ -1) do
  	vol32[vx] := vol8[vx];
 nifti_smooth_gauss(vol32, 4, 4, 4, 1);
 //scale 0..1
 mn := infinity;
 mx := -infinity;
 for vx := 0 to (nXYZ - 1) do begin
     if specialsingle(vol32[vx]) then continue;
     mn := min(mn, vol32[vx]);
     mx := max(mx, vol32[vx]);
 end;
 if (mn >= mx) then begin
   setlength(vol32, 0);
   exit;
 end;
 scale255 := 255.0 / (mx - mn);
 setlength(result, nXYZ);
 for vx := 0 to (nXYZ - 1) do
     result[vx] := round(scale255 * (vol32[vx] - mn));
 setlength(vol32, 0);
end; *)

(*function TNIfTI.EdgeMap(isSmooth: boolean): TUInt8s;
//https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/programs/3dedge3_sphx.html
// 2dedge3 uses R. Deriche formula https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.476.5736&rep=rep1&type=pdf
//https://en.wikipedia.org/wiki/Sobel_operator#Extension_to_other_dimensions
//optional compile:
// Asjad and M. Deriche: https://www.researchgate.net/publication/283280465_A_new_approach_for_salt_dome_detection_using_a_3D_multidirectional_edge_detector
//{$DEFINE ASJAD}
var
  x, y, z, vx, nXYZ, nXY, nX, nY, nZ: int64;
  llh, mlh, hlh, lmh, mmh, hmh,  lhh, mhh, hhh, //voxel neighbors: slice above (high)
  llm, mlm, hlm, lmm,      hmm,  lhm, mhm, hhm, //voxel neighbors: same slice (middle)
  lll, mll, hll, lml, mml, hml,  lhl, mhl, hhl, //voxel neighbors: slice below (low)
  gx, gy, gz, mn, mx, scale255: single;
  {$IFDEF ASJAD}g45, g135: single;  {$ENDIF}
  vol8: TUInt8s;
  vol32: TFloat32s;
begin
 setlength(result, 0);
 if IsLabels then exit;
 if fHdr.bitpix = 24 then exit;
 nX := fHdr.Dim[1];
 nY := fHdr.Dim[2];
 nZ := fHdr.Dim[3];
 if (nX < 3) or (nY < 3) or (nZ < 3) then exit;
 nXY := nX * nY;
 nXYZ := nXY * nZ;
 setlength(vol32, nXYZ);
 vol8 := fCache8;
 vx := -1;
 for z := 1 to nZ do
     for y := 1 to nY do
         for x := 1 to nX do begin
            vx += 1;
            if (z = 1) or (z = nZ) or (y = 1) or (y = nY) or (x = 1) or (x = nX) then begin
              vol32[vx] := 0.0;
              continue;
            end;
            //slice above (high)
            llh := vol8[vx - 1 - nX + nXY];
            mlh := vol8[vx - 0 - nX + nXY] * 2;
            hlh := vol8[vx + 1 - nX + nXY];
            lmh := vol8[vx - 1 - 0 + nXY] * 2;
            mmh := vol8[vx - 0 - 0 + nXY] * 4;
            hmh := vol8[vx + 1 - 0 + nXY] * 2;
            lhh := vol8[vx - 1 + nX + nXY];
            mhh := vol8[vx - 0 + nX + nXY] * 2;
            hhh := vol8[vx + 1 + nX + nXY];
            //same slice (middle)
            llm := vol8[vx - 1 - nX + 0] * 2;
            mlm := vol8[vx - 0 - nX + 0] * 4;
            hlm := vol8[vx + 1 - nX + 0] * 2;
            lmm := vol8[vx - 1 - 0 + 0] * 4;
            //mmm := vol8[vx - 0 - 0 + 0];
            hmm := vol8[vx + 1 - 0 + 0] * 4;
            lhm := vol8[vx - 1 + nX + 0] * 2;
            mhm := vol8[vx - 0 + nX + 0] * 4;
            hhm := vol8[vx + 1 + nX + 0] * 2;
            //slice below (low)
            lll := vol8[vx - 1 - nX - nXY];
            mll := vol8[vx - 0 - nX - nXY] * 2;
            hll := vol8[vx + 1 - nX - nXY];
            lml := vol8[vx - 1 - 0 - nXY] * 2;
            mml := vol8[vx - 0 - 0 - nXY] * 4;
            hml := vol8[vx + 1 - 0 - nXY] * 2;
            lhl := vol8[vx - 1 + nX - nXY];
            mhl := vol8[vx - 0 + nX - nXY] * 2;
            hhl := vol8[vx + 1 + nX - nXY];
            gx := (hlh + hmh + hhh + hlm + hmm + hhm + hll + hml + hhl) - (llh + lmh + lhh + llm + lmm + lhm + lll + lml + lhl);
            gy := (lhh + mhh + hhh + lhm + mhm + hhm + lhl + mhl + hhl) - (llh + mlh + hlh + llm + mlm + hlm + lll + mll + hll);
            gz := (llh + mlh + hlh + lmh + mmh + hmh +  lhh + mhh + hhh) - (lll + mll + hll + lml + mml + hml +  lhl + mhl + hhl);
            {$IFDEF ASJAD}
            //slice above (high)
            llh := vol8[vx - 1 - nX + nXY] * 2;
            mlh := vol8[vx - 0 - nX + nXY];
            hlh := vol8[vx + 1 - nX + nXY] * 2;
            lmh := vol8[vx - 1 - 0 + nXY];
            //mmh := vol8[vx - 0 - 0 + nXY] * 0;
            hmh := vol8[vx + 1 - 0 + nXY];
            lhh := vol8[vx - 1 + nX + nXY] * 2;
            mhh := vol8[vx - 0 + nX + nXY];
            hhh := vol8[vx + 1 + nX + nXY] * 2;
            //same slice (middle)
            llm := vol8[vx - 1 - nX + 0] * 4;
            mlm := vol8[vx - 0 - nX + 0] * 2;
            hlm := vol8[vx + 1 - nX + 0] * 4;
            lmm := vol8[vx - 1 - 0 + 0] * 2;
            //mmm := vol8[vx - 0 - 0 + 0];
            hmm := vol8[vx + 1 - 0 + 0] * 2;
            lhm := vol8[vx - 1 + nX + 0] * 4;
            mhm := vol8[vx - 0 + nX + 0] * 2;
            hhm := vol8[vx + 1 + nX + 0] * 4;
            //slice below (low)
            lll := vol8[vx - 1 - nX - nXY] * 2;
            mll := vol8[vx - 0 - nX - nXY];
            hll := vol8[vx + 1 - nX - nXY] * 2;
            lml := vol8[vx - 1 - 0 - nXY];
            //mml := vol8[vx - 0 - 0 - nXY] * 0;
            hml := vol8[vx + 1 - 0 - nXY];
            lhl := vol8[vx - 1 + nX - nXY] * 2;
            mhl := vol8[vx - 0 + nX - nXY];
            hhl := vol8[vx + 1 + nX - nXY] * 2;

            g45 := (hmh + mlh + hlh + hmm + mlm + hlm + hml + mll + hll) - (lhh + mhh + lmh + lhm + mhm + lmm + lhl + mhl + lml);
            g135 := (lmh + llh + mlh + lmm + llm + mlm + lml + lll +mll) - (mhh + hhh + hmh + mhm + hhm + hmm + mhl + hhl + hml);


            vol32[vx] += sqrt( sqr(gx) + sqr(gy) + sqr(gz)  + sqr(g45) + sqr(g135));
            {$ELSE}
            vol32[vx] += sqrt( sqr(gx) + sqr(gy) + sqr(gz));
            {$ENDIF}

         end;
 //scale 0..1
 mn := infinity;
 mx := -infinity;
 for vx := 0 to (nXYZ - 1) do begin
     if specialsingle(vol32[vx]) then continue;
     mn := min(mn, vol32[vx]);
     mx := max(mx, vol32[vx]);
 end;
 if (mn >= mx) then begin
   setlength(vol32, 0);
   exit;
 end;
 scale255 := 255.0 / (mx - mn);
 setlength(result, nXYZ);
 for vx := 0 to (nXYZ - 1) do
     result[vx] := round(scale255 * (vol32[vx] - mn));
 setlength(vol32, 0);
end;   *)

function TNIfTI.SeedCorrelationMap(vSeed: TFloat32s; isZ: boolean): TFloat32s; overload;
//https://www.johndcook.com/blog/2008/11/05/how-to-calculate-pearson-correlation-accurately/
var
  vx, nVx, vol, nVol, volBytes: int64;
  vol8: TUInt8s;
  vol16: TInt16s;
  v, vol32: TFloat32s;
  r: double;
  {$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
begin
 setlength(result, 0);
 if IsLabels then exit;
 if volumesLoaded < 2 then exit;
 if fHdr.bitpix = 24 then exit;
 {$IFDEF TIMER}startTime := now;{$ENDIF}
 nVx := fHdr.Dim[1]*fHdr.Dim[2]*fHdr.Dim[3];
 volBytes := nVx * (fHdr.bitpix shr 3);
 nVol := length(fRawVolBytes) div volBytes;
 if nVol <> fVolumesLoaded then exit;
 if length(vSeed) <> nVol then exit;
 if (correl(vSeed) = 0.0) then exit; //no variance in seed
 vol8 := fRawVolBytes;
 vol16 := TInt16s(vol8);
 vol32 := TFloat32s(vol8);
 setlength(v, nVol);
 setlength(result, nVx);
 //compute correlation for each voxel
 for vx := 0 to (nVx - 1) do begin
    if fHdr.datatype = kDT_UINT8 then begin
       for vol := 0 to (nVol - 1) do
           v[vol] := vol8[vx + (vol * nVx)];
    end else if fHdr.datatype = kDT_INT16 then begin
      for vol := 0 to (nVol - 1) do
          v[vol] := vol16[vx + (vol * nVx)];
    end else if fHdr.datatype = kDT_FLOAT then begin
      for vol := 0 to (nVol - 1) do
          v[vol] := vol32[vx + (vol * nVx)];
    end;
    {$IFDEF FASTCORREL2} //FastCorrel2 about 10% faster
    result[vx] := CorrelR(vSeed, v);
    {$ELSE}
    if (correl(v) = 0.0) then begin //no variance at this voxel
       result[vx] := 0;
       continue;
    end;
    r := 0.0;
    for vol := 0 to (nVol - 1) do
        r := r + (v[vol] * vSeed[vol]);
    r := r / (nVol - 1);
    result[vx] := r;
    {$ENDIF}
 end;
 if isZ then begin
    //https://www.statisticshowto.datasciencecentral.com/fisher-z/
    //z’ = .5[ln(1+r) – ln(1-r)]
    for vx := 0 to (nVx - 1) do begin
        r := result[vx];
        if (r >= 1.0) or (r <= -1.0) then
           result[vx] := 10.0
        else
            result[vx] := 0.5 * ln((1+r)/(1-r));
            //result[vx] := arctanh(r);
            //result[vx] := 0.5 * (ln(1+r) - ln(1-r));
            //Matlab check: these generate same values
            // atanh(r)
            // 0.5 * (log(1+r)-log(1-r))
            // log((1+r)/(1-r))*0.5
        //result[vx] := 0.5*(ln(1+r) – ln(1-r));
    end;
 end;
 setlength(v, 0);
 {$IFDEF TIMER}printf(format('Correl time %d',[MilliSecondsBetween(Now,startTime)]));{$ENDIF}
end;

//VolRawBytes: TUInt8s
function TNIfTI.SeedCorrelationMap(roi: TUInt8s; isZ: boolean): TFloat32s; overload;
//https://www.johndcook.com/blog/2008/11/05/how-to-calculate-pearson-correlation-accurately/
var
  nROI, vx, nVx, vol, nVol, volBytes: int64;
  vol8: TUInt8s;
  vol16: TInt16s;
  vSeed, vol32: TFloat32s;
begin
 result := nil;
 if IsLabels then exit;
 if volumesLoaded < 2 then exit;
 if fHdr.bitpix = 24 then exit;
 nVx := fHdr.Dim[1]*fHdr.Dim[2]*fHdr.Dim[3];
 volBytes := nVx * (fHdr.bitpix shr 3);
 nVol := length(fRawVolBytes) div volBytes;
 if nVol <> fVolumesLoaded then exit;
 if nVx <> length(roi) then exit;
 nROI := 0;
 for vx := 0 to (nVx-1) do
     if roi[vx] <> 0 then nROI := nROI + 1;
 if (nROI < 1) then exit; //no images in mask
 setlength(vSeed, nVol);
 vol8 := fRawVolBytes;
 vol16 := TInt16s(vol8);
 vol32 := TFloat32s(vol8);
 //load values for seed
 for vol := 0 to (nVol - 1) do
     vSeed[vol] := 0;
 if fHdr.datatype = kDT_UINT8 then begin
    for vol := 0 to (nVol - 1) do begin
        for vx := 0 to (nVx-1) do
            if roi[vx] <> 0 then
               vSeed[vol] := vSeed[vol]  + vol8[vx + (vol * nVx)];
    end;
 end else if fHdr.datatype = kDT_INT16 then begin
   for vol := 0 to (nVol - 1) do begin
       for vx := 0 to (nVx-1) do
            if roi[vx] <> 0 then
               vSeed[vol] := vSeed[vol]  + vol16[vx + (vol * nVx)];
   end;
 end else if fHdr.datatype = kDT_FLOAT then begin
   for vol := 0 to (nVol - 1) do begin
       for vx := 0 to (nVx-1) do
            if roi[vx] <> 0 then
               vSeed[vol] := vSeed[vol]  + vol32[vx + (vol * nVx)];
   end;
 end;
 for vol := 0 to (nVol - 1) do
     vSeed[vol] := vSeed[vol] / nROI;

 result := SeedCorrelationMap(vSeed, isZ);
end;


function TNIfTI.SeedCorrelationMap(vox: TVec3i; isZ: boolean): TFloat32s; overload;
//https://www.johndcook.com/blog/2008/11/05/how-to-calculate-pearson-correlation-accurately/
var
  //vx1,
  vx, nVx, vol, nVol, volBytes: int64;
  vol8: TUInt8s;
  vol16: TInt16s;
  vSeed, vol32: TFloat32s;
begin
 result := nil;
 if IsLabels then exit;
 if volumesLoaded < 2 then exit;
 if fHdr.bitpix = 24 then exit;
 nVx := fHdr.Dim[1]*fHdr.Dim[2]*fHdr.Dim[3];
 volBytes := nVx * (fHdr.bitpix shr 3);
 nVol := length(fRawVolBytes) div volBytes;
 if nVol <> fVolumesLoaded then exit;
 vx := vox.x + (vox.y * dim.x) + (vox.z * (dim.x * dim.y));
 if (vx < 0) or (vx >= nVx) then exit;
 setlength(vSeed, nVol);
 vol8 := fRawVolBytes;
 vol16 := TInt16s(vol8);
 vol32 := TFloat32s(vol8);
 //load values for seed
 if fHdr.datatype = kDT_UINT8 then begin
    for vol := 0 to (nVol - 1) do
        vSeed[vol] := vol8[vx + (vol * nVx)];
 end else if fHdr.datatype = kDT_INT16 then begin
   for vol := 0 to (nVol - 1) do
       vSeed[vol] := vol16[vx + (vol * nVx)];
 end else if fHdr.datatype = kDT_FLOAT then begin
   for vol := 0 to (nVol - 1) do
       vSeed[vol] := vol32[vx + (vol * nVx)];
 end;
 result := SeedCorrelationMap(vSeed, isZ);
end;


(*function TNIfTI.SeedCorrelationMap(vox: TVec3i; isZ: boolean): TFloat32s; overload;
//https://www.johndcook.com/blog/2008/11/05/how-to-calculate-pearson-correlation-accurately/
var
  //vx1,
  vx, nVx, vol, nVol, volBytes: int64;
  vol8: TUInt8s;
  vol16: TInt16s;
  vSeed, v, vol32: TFloat32s;
  r: double;
  {$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
begin
 setlength(result, 0);
 if IsLabels then exit;
 if volumesLoaded < 2 then exit;
 if fHdr.bitpix = 24 then exit;
 {$IFDEF TIMER}startTime := now;{$ENDIF}
 nVx := fHdr.Dim[1]*fHdr.Dim[2]*fHdr.Dim[3];
 volBytes := nVx * (fHdr.bitpix shr 3);
 nVol := length(fRawVolBytes) div volBytes;
 if nVol <> fVolumesLoaded then exit;
 vx := vox.x + (vox.y * dim.x) + (vox.z * (dim.x * dim.y));
 if (vx < 0) or (vx >= nVx) then exit;
 setlength(result, nVx);
 setlength(v, nVol);
 setlength(vSeed, nVol);
 vol8 := fRawVolBytes;
 vol16 := TInt16s(vol8);
 vol32 := TFloat32s(vol8);
 //load values for seed
 if fHdr.datatype = kDT_UINT8 then begin
    for vol := 0 to (nVol - 1) do
        v[vol] := vol8[vx + (vol * nVx)];
 end else if fHdr.datatype = kDT_INT16 then begin
   for vol := 0 to (nVol - 1) do
       v[vol] := vol16[vx + (vol * nVx)];
 end else if fHdr.datatype = kDT_FLOAT then begin
   for vol := 0 to (nVol - 1) do
       v[vol] := vol32[vx + (vol * nVx)];
 end;
 if (correl(v) = 0.0) then begin //no variance in seed
   setlength(v, 0);
   setlength(vSeed, 0);
   setlength(result, 0);
   exit;
 end;
 for vol := 0 to (nVol - 1) do
       vSeed[vol] := v[vol];
 //vSeed := copy(v, low(v), high(v));
 //vx1 := vx;
 //compute correlation for each voxel
 for vx := 0 to (nVx - 1) do begin
    if fHdr.datatype = kDT_UINT8 then begin
       for vol := 0 to (nVol - 1) do
           v[vol] := vol8[vx + (vol * nVx)];
    end else if fHdr.datatype = kDT_INT16 then begin
      for vol := 0 to (nVol - 1) do
          v[vol] := vol16[vx + (vol * nVx)];
    end else if fHdr.datatype = kDT_FLOAT then begin
      for vol := 0 to (nVol - 1) do
          v[vol] := vol32[vx + (vol * nVx)];
    end;
    {$IFDEF FASTCORREL2} //FastCorrel2 about 10% faster
    result[vx] := CorrelR(vSeed, v);
    {$ELSE}
    if (correl(v) = 0.0) then begin //no variance at this voxel
       result[vx] := 0;
       continue;
    end;
    r := 0.0;
    for vol := 0 to (nVol - 1) do
        r := r + (v[vol] * vSeed[vol]);
    r := r / (nVol - 1);
    result[vx] := r;
    {$ENDIF}
 end;
 if isZ then begin
    //https://www.statisticshowto.datasciencecentral.com/fisher-z/
    //z’ = .5[ln(1+r) – ln(1-r)]
    for vx := 0 to (nVx - 1) do begin
        r := result[vx];
        if (r >= 1.0) or (r <= -1.0) then
           result[vx] := 10.0
        else
            result[vx] := 0.5 * ln((1+r)/(1-r));
            //result[vx] := arctanh(r);
            //result[vx] := 0.5 * (ln(1+r) - ln(1-r));
            //Matlab check: these generate same values
            // atanh(r)
            // 0.5 * (log(1+r)-log(1-r))
            // log((1+r)/(1-r))*0.5
        //result[vx] := 0.5*(ln(1+r) – ln(1-r));
    end;
 end;
 //result[vx1] := 1;
 setlength(v, 0);
 setlength(vSeed, 0);
 {$IFDEF TIMER}printf(format('Correl time %d',[MilliSecondsBetween(Now,startTime)]));{$ENDIF}
end;  *)
{$ENDIF}

function TNIfTI.VoxIntensityArray(roi: TUInt8s): TFloat32s; overload;
var
  vx, nVx, nVxROI, vol, nVol, volBytes: int64;
  d: double;
  vol8: TUInt8s;
  vol16: TInt16s;
  vol32: TFloat32s;
begin
  setlength(result, 0);
  if IsLabels then exit;
  if volumesLoaded < 2 then exit;
  if fHdr.bitpix = 24 then exit;  //>>
  nVx := fHdr.Dim[1]*fHdr.Dim[2]*fHdr.Dim[3];
  if nVx < 1 then exit;
  if length(roi) <> nVx then exit;
  nVxROI := 0;
  for vx := 0 to (nVx-1) do
      if roi[vx] <> 0 then
         nVxROI := nVxROI + 1;
  if nVxROI < 1 then exit;
  volBytes := nVx * (fHdr.bitpix shr 3);
  nVol := length(fRawVolBytes) div volBytes;
  if nVol <> fVolumesLoaded then exit;
  //vx := vox.x + (vox.y * dim.x) + (vox.z * (dim.x * dim.y));
  //if (vx < 0) or (vx >= nVx) then exit;
  setlength(result, fVolumesLoaded);
  vol8 := fRawVolBytes;
  vol16 := TInt16s(vol8);
  vol32 := TFloat32s(vol8);
  for vol := 0 to (nVol - 1) do begin
    d := 0;
    if fHdr.datatype = kDT_UINT8 then begin
      for vx := 0 to (nVx-1) do
          if roi[vx] <> 0 then
             d := d  + vol8[vx + (vol * nVx)];
    end else if fHdr.datatype = kDT_INT16 then begin
      for vx := 0 to (nVx-1) do
          if roi[vx] <> 0 then
             d := d  + vol16[vx + (vol * nVx)];
    end else if fHdr.datatype = kDT_FLOAT then begin
      for vx := 0 to (nVx-1) do
          if roi[vx] <> 0 then
             d := d  + vol32[vx + (vol * nVx)];
    end;
    result[vol] := ( d * (1.0/nVxROI) * fHdr.scl_slope) + fHdr.scl_inter;
    if specialsingle(result[vol]) then
       result[vol] := 0;
  end;
end; // VoxIntensityArray()

function TNIfTI.VoxIntensityArray(vox: TVec3i): TFloat32s; overload;
var
  vx, nVx, vol, nVol, volBytes: int64;
  vol8: TUInt8s;
  vol16: TInt16s;
  vol32: TFloat32s;
begin
     setlength(result, 0);
     if IsLabels then exit;
     if volumesLoaded < 2 then exit;
     if fHdr.bitpix = 24 then exit;  //>>
     nVx := fHdr.Dim[1]*fHdr.Dim[2]*fHdr.Dim[3];
     volBytes := nVx * (fHdr.bitpix shr 3);
     nVol := length(fRawVolBytes) div volBytes;
     if nVol <> fVolumesLoaded then exit;
     vx := vox.x + (vox.y * dim.x) + (vox.z * (dim.x * dim.y));
     if (vx < 0) or (vx >= nVx) then exit;
     setlength(result, fVolumesLoaded);
     vol8 := fRawVolBytes;
     vol16 := TInt16s(vol8);
     vol32 := TFloat32s(vol8);
     if fHdr.datatype = kDT_UINT8 then begin
        for vol := 0 to (nVol - 1) do
            result[vol] := vol8[vx + (vol * nVx)];
     end else if fHdr.datatype = kDT_RGB then begin
       nVx := nVx * 3;
       vx := (vx * 3) + 1; //read green
       for vol := 0 to (nVol - 1) do
           result[vol] := vol8[vx + (vol * nVx)];  //20201102
     end else if fHdr.datatype = kDT_INT16 then begin
       for vol := 0 to (nVol - 1) do
           result[vol] := vol16[vx + (vol * nVx)];
     end else if fHdr.datatype = kDT_FLOAT then begin
       for vol := 0 to (nVol - 1) do
           result[vol] := vol32[vx + (vol * nVx)];
     end;
     for vol := 0 to (nVol - 1) do begin
         result[vol] := (result[vol] * fHdr.scl_slope) + fHdr.scl_inter;
         if specialsingle(result[vol]) then
            result[vol] := 0;  //e.g. Inf+
     end;
end; // VoxIntensityArray()

function TNIfTI.VoxIntensity(vox: int64): single; overload; //return intensity of voxel at coordinate
var
  vol32 : TFloat32s;
  vol16: TInt16s;
  vol8: TUInt8s;
begin
     result := 0;
     if (vox < 0) or (vox >= (dim.x * dim.y * dim.z)) then exit;
     if fHdr.datatype = kDT_RGB then vox := (vox * 3) + 1; //read green
     vox := vox + skipVox;
     vol8 := fRawVolBytes;
     vol16 := TInt16s(vol8);
     vol32 := TFloat32s(vol8);
     if (fHdr.datatype = kDT_UINT8) or (fHdr.datatype = kDT_RGB) then
       result := vol8[vox]
     else if fHdr.datatype = kDT_INT16 then
          result := vol16[vox]
     else if fHdr.datatype = kDT_FLOAT then
          result := vol32[vox];
     result := (result * fHdr.scl_slope) + fHdr.scl_inter;
end;

function TNIfTI.VoxIntensity(vox: TVec3i): single; overload;//return intensity of voxel at coordinate
var
  i: int64;
begin
     result := 0;
     i := vox.x + (vox.y * dim.x) + (vox.z * (dim.x * dim.y));
     if (i < 0) or (i >= (dim.x * dim.y * dim.z)) then exit;
     result := VoxIntensity(i);
end;


function TNIfTI.AsFloats(): TFloat32s; //volume where voxels non-zero voxels are set to 1, voxels with intensity zero set to 0
var
  vol32, out32 : TFloat32s;
  vol16: TInt16s;
  vol8: TUInt8s;
  i, vox: int64;
begin
     vox := dim.x * dim.y * dim.z;
     setlength(out32, vox);
     if (vox < 1) then exit(out32);
     vol8 := fRawVolBytes;
     vol16 := TInt16s(vol8);
     vol32 := TFloat32s(vol8);
     if fHdr.datatype = kDT_UINT8 then begin
        for i := 0 to (vox - 1) do
            out32[i] := vol8[i];
     end else if fHdr.datatype = kDT_INT16 then begin
       for i := 0 to (vox - 1) do
           out32[i] := vol16[i];
     end else if fHdr.datatype = kDT_FLOAT then begin
       for i := 0 to (vox - 1) do
           out32[i] := vol32[i];
     end;
     result := out32;
end;

function TNIfTI.NotZero(): TInt16s; //volume where voxels non-zero voxels are set to 1, voxels with intensity zero set to 0
var
  vol32 : TFloat32s;
  vol16, out16: TInt16s;
  vol8: TUInt8s;
  i, vox: int64;
begin
     vox := dim.x * dim.y * dim.z;
     setlength(out16, vox);
     if (vox < 1) then exit(out16);
     vol8 := fRawVolBytes;
     vol16 := TInt16s(vol8);
     vol32 := TFloat32s(vol8);
     for i := 0 to (vox - 1) do
         out16[i] := 0;
     if fHdr.datatype = kDT_UINT8 then begin
        for i := 0 to (vox - 1) do
            if (vol8[i] <> 0) then
               out16[i] := 1;
     end else if fHdr.datatype = kDT_INT16 then begin
       for i := 0 to (vox - 1) do
           if (vol16[i] <> 0) then
              out16[i] := 1;
     end else if fHdr.datatype = kDT_FLOAT then begin
       for i := 0 to (vox - 1) do
           if (vol32[i] <> 0) then
              out16[i] := 1;
     end;
     result := out16;
end;

function TNIfTI.FracToSlice(Frac: TVec3): TVec3i;
begin
     result.X := frac2slice(Frac.X, fDim.X);
     result.Y := frac2slice(Frac.Y, fDim.Y);
     result.Z := frac2slice(Frac.Z, fDim.Z);
end;

function TNIfTI.FracShiftSlice(Frac: TVec3; sliceMove: TVec3i): TVec3; //move a desired number of slices
//align to slices
begin
     //if 3 voxels span 0..1 then centers are at 0.25/0.5/0.75 - voxels frac is 1/(n+1)
  result.X := Frac.X + (sliceMove.X * (1.0/(fDim.X+1)));
  result.Y := Frac.Y + (sliceMove.Y * (1.0/(fDim.Y+1)));
  result.Z := Frac.Z + (sliceMove.Z * (1.0/(fDim.Z+1)));
  result.X := voxelCenter(result.X, fDim.X);
  result.Y := voxelCenter(result.Y, fDim.Y);
  result.Z := voxelCenter(result.Z, fDim.Z);
end;

(*procedure ReportMat(s: string; m:TMat4);
begin
  printf(format('%s = [%g %g %g %g; %g %g %g %g; %g %g %g %g; %g %g %g %g]', [s,
    m[0,0], m[0,1], m[0,2], m[0,3],
    m[1,0], m[1,1], m[1,2], m[1,3],
    m[2,0], m[2,1], m[2,2], m[2,3],
    m[3,0], m[3,1], m[3,2], m[3,3]
    ]));
end; *)

function TNIfTI.MMFrac(MM: TVec3): TVec3;
var
   lV: TVec4;
begin
     lV := Vec4(MM.X,MM.Y,MM.Z,1);
     if (fDim.X < 1) or (fDim.Y < 1) or (fDim.Z < 1) then
          exit(vec3(0.5, 0.5, 0.5));
     Coord (lV,InvMat);
     result.X := (lV[0]+0.5)/fDim.X;
     result.Y := (lV[1]+0.5)/fDim.Y;
     result.Z := (lV[2]+0.5)/fDim.Z;
     //printf(format('dim = [%d %d %d]', [fDim.X,fDim.Y,fDim.Z]));
     //printf(format('mm = [%g %g %g 1]', [MM.X,MM.Y,MM.Z]));
     //reportMat('inv', InvMat);
     //printf(format('frac = [%g %g %g]', [result.X,result.Y,result.Z]));
     //result.X := (lV[0]+1)/fDim.X;
     //result.Y := (lV[1]+1)/fDim.Y;
     //result.Z := (lV[2]+1)/fDim.Z;
end;

function TNIfTI.FracMM(Frac: TVec3): TVec3;
var
  vox0: TVec3;
begin
     //result := sliceFrac2D; exit;
     vox0.x := trunc(Frac.x * Dim.x);
     vox0.y := trunc(Frac.y * Dim.y);
     vox0.z := trunc(Frac.z * Dim.z);
     vox0.x := min(vox0.x, Dim.x -1); //0..Dim-1, perfect unless sliceFrac2D.x=1
     vox0.y := min(vox0.y, Dim.y -1); //0..Dim-1, perfect unless sliceFrac2D.y=1
     vox0.z := min(vox0.z, Dim.z -1); //0..Dim-1, perfect unless sliceFrac2D.z=1
     //result := vox0; exit;
     result := SliceMM(vox0, Mat);
end;

{$IFDEF CUSTOMCOLORS}
function TNIfTI.FullColorTable: TCLUTrec;
begin
 result := clut.FullColorTable;
end;
{$ENDIF}

{$IFDEF CPUGRADIENTS}
const
 kRGBAclear : TRGBA = (r: 0; g: 0; b: 0; a:0);

Function XYZI (X1,X2,Y1,Y2,Z1,Z2: single; Center: byte): TRGBA;
//gradients in range -1..1
//input voxel intensity to the left,right,anterior,posterior,inferior,superior and center
// Output RGBA image where values correspond to X,Y,Z gradients and ImageIntensity
//AlphaT will make a voxel invisible if center intensity is less than specified value
// Voxels where there is no gradient (no edge boundary) are made transparent
var
  X,Y,Z,Dx: single;
begin
  Result := kRGBAclear;
  if Center < 1 then
    exit; //intensity less than threshold: make invisible
  X := X1-X2;
  Y := Y1-Y2;
  Z := Z1-Z2;
  Dx := sqrt(X*X+Y*Y+Z*Z);
  if Dx = 0 then
    exit;  //no gradient - set intensity to zero.
  result.r :=round((X/(Dx*2)+0.5)*255); //X
  result.g :=round((Y/(Dx*2)+0.5)*255); //Y
  result.b := round((Z/(Dx*2)+0.5)*255); //Z
  result.a := Center;
end;

function Sobel (rawData: TUInt8s; Xsz,Ysz, I : integer; var GradMag: single): TRGBA;
//this computes intensity gradients using 3D Sobel filter.
//Much slower than central difference but more accurate
//http://www.aravind.ca/cs788h_Final_Project/gradient_estimators.htm
var
  Y,Z,J: integer;
  Xp,Xm,Yp,Ym,Zp,Zm: single;
begin
  GradMag := 0;//gradient magnitude
  Result := kRGBAclear;
  if rawData[i] < 1 then
    exit; //intensity less than threshold: make invisible
  Y := XSz; //each row is X voxels
  Z := YSz*XSz; //each plane is X*Y voxels
  //X:: cols: +Z +0 -Z, rows -Y +0 +Y
  J := I+1;
  Xp := rawData[J-Y+Z]+3*rawData[J-Y]+rawData[J-Y-Z]
        +3*rawData[J+Z]+6*rawData[J]+3*rawData[J-Z]
        +rawData[J+Y+Z]+3*rawData[J+Y]+rawData[J+Y-Z];
  J := I-1;
  Xm := rawData[J-Y+Z]+3*rawData[J-Y]+rawData[J-Y-Z]
        +3*rawData[J+Z]+6*rawData[J]+3*rawData[J-Z]
        +rawData[J+Y+Z]+3*rawData[J+Y]+rawData[J+Y-Z];
  //Y:: cols: +Z +0 -Z, rows -X +0 +X
  J := I+Y;
  Yp := rawData[J-1+Z]+3*rawData[J-1]+rawData[J-1-Z]
        +3*rawData[J+Z]+6*rawData[J]+3*rawData[J-Z]
        +rawData[J+1+Z]+3*rawData[J+1]+rawData[J+1-Z];
  J := I-Y;
  Ym := rawData[J-1+Z]+3*rawData[J-1]+rawData[J-1-Z]
        +3*rawData[J+Z]+6*rawData[J]+3*rawData[J-Z]
        +rawData[J+1+Z]+3*rawData[J+1]+rawData[J+1-Z];
  //Z:: cols: +Z +0 -Z, rows -X +0 +X
  J := I+Z;
  Zp := rawData[J-Y+1]+3*rawData[J-Y]+rawData[J-Y-1]
        +3*rawData[J+1]+6*rawData[J]+3*rawData[J-1]
        +rawData[J+Y+1]+3*rawData[J+Y]+rawData[J+Y-1];
  J := I-Z;
  Zm := rawData[J-Y+1]+3*rawData[J-Y]+rawData[J-Y-1]
        +3*rawData[J+1]+6*rawData[J]+3*rawData[J-1]
        +rawData[J+Y+1]+3*rawData[J+Y]+rawData[J+Y-1];
  result := XYZI (Xm,Xp,Ym,Yp,Zm,Zp,rawData[I]);
  GradMag :=  sqrt( sqr(Xm-Xp)+sqr(Ym-Yp)+sqr(Zm-Zp));//gradient magnitude
end;

procedure NormVol (var Vol: TFloat32s);
var
  n,i: int64;
  mx,mn: single;
begin
  n := length(Vol);
  if n < 1 then
    exit;
  mx := Vol[0];
  mn := Vol[0];
  for i := 0 to (n-1) do begin
    if Vol[i] > mx then
      mx := Vol[i];
    if Vol[i] < mn then
      mn := Vol[i];
  end;
  if mx = mn then
    exit;
  mx := mx-mn;//range
  for i := 0 to (n-1) do
    Vol[i] := (Vol[i]-mn)/mx;
end;

function SmoothVol (var rawData: TUInt8s; lXdim,lYdim,lZdim: integer): integer;
//simple 3D smoothing for noisy data - makes images blurry
//this is useful prior to generating gradients, as it reduces stepping
const
  kCen : single = 2/3; //center of kernel is 2/3
  kEdge : single = 1/6; //voxel on each edge is 1/6
var
   lSmoothImg,lSmoothImg2: TFloat32s;
   lSliceSz,lZPos,lYPos,lX,lY,lZ,lnVox,lVox: int64;
begin
   result := -1;
   lSliceSz := lXdim*lYdim;
   lnVox := lSliceSz*lZDim;
    if (lnVox < 0) or (lXDim < 3) or (lYDim < 3) or (lZDim < 3) then begin
	    printf(format ('Smooth3DNII error: Image dimensions are not large enough to filter %dx%dx%d.',[lXDim,lYDim,lZDim]));
	    exit;
   end;
  setlength(lSmoothImg,lnVox);
  setlength(lSmoothImg2,lnVox);
  for lX := 0 to (lnVox-1) do
    lSmoothImg[lX] := rawData[lX];
  for lX := 0 to (lnVox-1) do
    lSmoothImg2[lX] := rawData[lX];
  //X-direction - copy from SmoothImg -> SmoothImg2
  for lZ := 2 to lZdim-1 do begin
    lZPos := (lZ-1)*lSliceSz;
    for lY := 2 to lYdim-1 do begin
      lYPos := (lY-1)*lXdim;
      for lX := 2 to lXdim-1 do begin
            lVox := lZPos+lYPos+lX-1;//-1 as indexed from 0
            lSmoothImg2[lVox] := (lSmoothImg[lVox-1]*kEdge)+(lSmoothImg[lVox]*kCen)+(lSmoothImg[lVox+1]*kEdge);
      end; {lX}
    end; {lY}
  end; {lZ loop for X-plane}
  //Y-direction - copy from SmoothImg2 -> SmoothImg
  for lZ := 2 to lZdim-1 do begin
    lZPos := (lZ-1)*lSliceSz;
    for lY := 2 to lYdim-1 do begin
      lYPos := (lY-1)*lXdim;
      for lX := 2 to lXdim-1 do begin
            lVox := lZPos+lYPos+lX-1;//-1 as indexed from 0
            lSmoothImg[lVox] := (lSmoothImg2[lVox-lXdim]*kEdge)+(lSmoothImg2[lVox]*kCen)+(lSmoothImg2[lVox+lXdim]*kEdge);
      end; {lX}
    end; {lY}
  end; {lZ loop for Y-plane}
  //Z-direction - copy from SmoothImg -> SmoothImg2
  for lZ := 2 to lZdim-1 do begin
    lZPos := (lZ-1)*lSliceSz;
    for lY := 2 to lYdim-1 do begin
      lYPos := (lY-1)*lXdim;
      for lX := 2 to lXdim-1 do begin
            lVox := lZPos+lYPos+lX-1;//-1 as indexed from 0
            lSmoothImg2[lVox] := (lSmoothImg[lVox-lSliceSz]*kEdge)+(lSmoothImg[lVox]*kCen)+(lSmoothImg[lVox+lSliceSz]*kEdge);
      end; //x
    end; //y
  end; //z
   //next make this in the range 0..255
  for lX := 0 to (lnVox-1) do
    rawData[lX] := round(lSmoothImg2[lX]);
  lSmoothImg2 := nil;
  lSmoothImg := nil;
end;

{$IFNDEF DYNRGBA}
procedure SetLengthP(var S: TRGBAp; Len: SizeInt); inline;
begin
	ReAllocMem(S, Len *sizeof(TRGBA));
end;
{$ENDIF}

{$IFDEF DYNRGBA}
procedure CreateGradientVolumeX (rData: TUInt8s; Xsz,Ysz,Zsz, isRGBA: integer; out VolRGBA : TRGBAs);
{$ELSE}
procedure CreateGradientVolumeX (rData: TUInt8s; Xsz,Ysz,Zsz, isRGBA: integer; out VolRGBA : TRGBAp);
{$ENDIF}
//compute gradients for each voxel... Output texture in form RGBA
//  RGB will represent as normalized X,Y,Z gradient vector:  Alpha will store gradient magnitude
const
  kEdgeSharpness = 255;//value 1..255: 1=all edges transparent, 255=edges very opaque
var
  X, Y,Z,Index,XYsz : int64;
  VolData: TUInt8s;
  {$IFDEF DYNRGBA}
  tRGBA: TRGBAs;
  {$ELSE}
  tRGBA: TRGBAp = nil;
  {$ENDIF}
  GradMagS: TFloat32s;
Begin
  tRGBA := nil;
  if (XSz < 1) or (YSz < 1) or (ZSz < 1) then
    exit;
  XYsz :=  Xsz*Ysz;
  Setlength (VolData,XYsz*Zsz);
  if isRGBA = 1 then begin
     {$IFDEF DYNRGBA}
     tRGBA := TRGBAs(rData );
     {$ELSE}
     tRGBA := TRGBAp(rData );
     {$ENDIF}
     for Index := 0 to ((XYsz*Zsz)-1) do
     {$IFDEF DYNRGBA}
     VolData[Index] := tRGBA[Index].a;
     {$ELSE}
     VolData[Index] := tRGBA^[Index].a;
     {$ENDIF}
  end else
    for Index := 0 to ((XYsz*Zsz)-1) do
      VolData[Index] := rData[Index];
  //next line: blur the data
  if (ZSz > 2) then
  	SmoothVol (VolData, Xsz,Ysz,Zsz);
  {$IFDEF DYNRGBA}
  SetLength (VolRGBA, XYsz*Zsz);
  {$ELSE}
  SetLengthP (VolRGBA, XYsz*Zsz);
  {$ENDIF}
  if (ZSz < 3) then exit;
  SetLength (GradMagS,XYsz*Zsz);
  for Index := 0 to ((XYsz*Zsz)-1) do //we can not compute gradients for image edges, so initialize volume so all voxels are transparent
  	  {$IFDEF DYNRGBA}
      VolRGBA[Index] := kRGBAclear;
	  {$ELSE}
      VolRGBA^[Index] := kRGBAclear;
	  {$ENDIF}
  for Z := 1 To Zsz - 2 do  //for X,Y,Z dimensions indexed from zero, so := 1 gives 1 voxel border
    for Y := 1 To Ysz - 2 do
      for X := 1 To Xsz - 2 do begin
        Index := (Z * XYsz) + (Y * Xsz) + X;
        //Next line computes gradients using Sobel filter
        {$IFDEF DYNRGBA}
        VolRGBA[Index] := Sobel (VolData, Xsz,Ysz, Index,GradMagS[Index]);
        {$ELSE}
        VolRGBA^[Index] := Sobel (VolData, Xsz,Ysz, Index,GradMagS[Index]);
        {$ENDIF}
      end;//X
  VolData := nil;
  //next: generate normalized gradient magnitude values
  NormVol (GradMagS);
  for Index := 0 to ((XYsz*Zsz)-1) do
   {$IFDEF DYNRGBA}
   VolRGBA[Index].A := round(GradMagS[Index]*kEdgeSharpness);
    {$ELSE}
    VolRGBA^[Index].A := round(GradMagS[Index]*kEdgeSharpness);
    {$ENDIF}
  GradMagS := nil;
end;

{$IFDEF DYNRGBA}
procedure TNIfTI.CreateGradientVolume (rData: TRGBAs; Xsz,Ysz,Zsz: integer; out Vol : TRGBAs);
{$ELSE}
procedure TNIfTI.CreateGradientVolume (rData: TRGBAp; Xsz,Ysz,Zsz: integer; out Vol : TRGBAp);
{$ENDIF}
begin
     CreateGradientVolumeX (TUInt8s(rData), Xsz,Ysz,Zsz, 1, Vol);
end;

{$IFDEF DYNRGBA}
function TNIfTI.GenerateGradientVolume: TRGBAs;
var
  GradRGBA : TRGBAs;
{$ELSE}
function TNIfTI.GenerateGradientVolume: TRGBAp;
var
  GradRGBA : TRGBAp = nil;
{$ENDIF}
begin
 CreateGradientVolumeX (TUInt8s(fVolRGBA), fDim.X, fDim.Y, fDim.Z, 1, GradRGBA);
 result := GradRGBA;
end;
{$ENDIF} //CPU gradients

procedure printMat(s: string; m: TMat4);
begin
  printf(format('%s = [%g %g %g %g,  %g %g %g %g,  %g %g %g %g,  %g %g %g %g]', [s,
   m.m[0,0], m.m[0,1], m.m[0,2], m.m[0,3],
   m.m[1,0], m.m[1,1], m.m[1,2], m.m[1,3],
   m.m[2,0], m.m[2,1], m.m[2,2], m.m[2,3],
   m.m[3,0], m.m[3,1], m.m[3,2], m.m[3,3]
  ]));
end;

function EstimateReorient(dim : TVec3i; R: TMat4; out residualR: TMat4; out perm : TVec3i): boolean;
//compute dimension permutations and flips to reorient volume to standard space
//From Xiangrui Li's BSD 2-Clause Licensed code
// https://github.com/xiangruili/dicm2nii/blob/master/nii_viewer.m
var
  a, rotM: TMat4;
  i,j: integer;
  flp,ixyz : TVec3i;
begin
  result := false;
  a := TMat4.Identity;
  //memo1.lines.add(writeMat('R',R));
  for i := 0 to 3 do
      for j := 0 to 3 do
          a[i,j] := abs(R[i,j]);
  //memo1.lines.add(writeMat('a',a));
  //first column = x
  ixyz.x := 1;
  if (a[1,0] > a[0,0]) then ixyz.x := 2;
  if (a[2,0] > a[0,0]) and (a[2,0]> a[1,0]) then ixyz.x := 3;
  //second column = y: constrained as must not be same row as x
  if (ixyz.x = 1) then begin
     if (a[1,1] > a[2,1]) then
        ixyz.y := 2
     else
         ixyz.y := 3;
  end else if (ixyz.x = 2) then begin
     if (a[0,1] > a[2,1]) then
        ixyz.y := 1
     else
         ixyz.y := 3;
  end else begin //ixyz.x = 3
     if (a[0,1] > a[1,1]) then
        ixyz.y := 1
     else
         ixyz.y := 2;
  end;
  //third column = z:constrained as x+y+z = 1+2+3 = 6
  ixyz.z := 6 - ixyz.y - ixyz.x;
  //printf(format('perm ixyz %d %d %d', [ixyz.x, ixyz.y, ixyz.z]));
  perm.v[ixyz.x-1] := 1;
  perm.v[ixyz.y-1] := 2;
  perm.v[ixyz.z-1] := 3;
  //printf(format('perm v %d %d %d', [perm.v[0], perm.v[1], perm.v[2]]));
  //sort columns  R(:,1:3) = R(:,perm);
  rotM := R;
  //printMat('->inR', rotM);
  for i := 0 to 3 do
      for j := 0 to 2 do
          R[i,j] := rotM[i,perm.v[j]-1];
  //printMat('->R', R);
  //compute if dimension is flipped
  if R[0,0] < 0 then flp.x := 1 else flp.x := 0;
  if R[1,1] < 0 then flp.y := 1 else flp.y := 0;
  if R[2,2] < 0 then flp.z := 1 else flp.z := 0;
  if (perm.x = 1) and (perm.y = 2) and (perm.z = 3) and (flp.x = 0) and (flp.y = 0) and (flp.z = 0) then exit;//already oriented correctly
  result := true; //reorient required!
  rotM := TMat4.Identity;
  rotM[0,0] := 1-flp.x*2;
  rotM[1,1] := 1-flp.y*2;
  rotM[2,2] := 1-flp.z*2;
  rotM[0,3] := ((dim.v[perm.x-1])-1) * flp.x;
  rotM[1,3] := ((dim.v[perm.y-1])-1) * flp.y;
  rotM[2,3] := ((dim.v[perm.z-1])-1) * flp.z;
  residualR := rotM.Inverse;
  //printMat('R', residualR);
  //printMat('inverse', residualR);
  residualR *= R;
  //printMat('residual', residualR);
  for i := 0 to 2 do
      if (flp.v[i] <> 0) then perm.v[i] := -perm.v[i];
  //printf(format('%d %d %d', [perm.x, perm.y, perm.z]));
end;

function SForm2Mat(hdr: TNIFTIhdr): TMat4;
begin
  result := TMat4.Identity;
  result[0,0] := hdr.srow_x[0]; result[0,1] := hdr.srow_x[1]; result[0,2] := hdr.srow_x[2]; result[0,3] := hdr.srow_x[3];
  result[1,0] := hdr.srow_y[0]; result[1,1] := hdr.srow_y[1]; result[1,2] := hdr.srow_y[2]; result[1,3] := hdr.srow_y[3];
  result[2,0] := hdr.srow_z[0]; result[2,1] := hdr.srow_z[1]; result[2,2] := hdr.srow_z[2]; result[2,3] := hdr.srow_z[3];
end;

procedure Mat2QForm(m: TMat4; var hdr: TNIFTIhdr);
var
  i,j: integer;
  m44 :mat44;
  dumdx, dumdy, dumdz: single;
begin
   for i := 0 to 3 do
       for j := 0 to 3 do
           m44[i,j] := m[i,j];
   nifti_mat44_to_quatern( m44 , hdr.quatern_b, hdr.quatern_c, hdr.quatern_d,hdr.qoffset_x,hdr.qoffset_y,hdr.qoffset_z, dumdx, dumdy, dumdz,hdr.pixdim[0]) ;
   hdr.qform_code := hdr.sform_code;
end;

procedure Mat2SForm(m: TMat4; var hdr: TNIFTIhdr);
begin
 hdr.srow_x[0] := m[0,0]; hdr.srow_x[1] := m[0,1]; hdr.srow_x[2] := m[0,2]; hdr.srow_x[3] := m[0,3];
 hdr.srow_y[0] := m[1,0]; hdr.srow_y[1] := m[1,1]; hdr.srow_y[2] := m[1,2]; hdr.srow_y[3] := m[1,3];
 hdr.srow_z[0] := m[2,0]; hdr.srow_z[1] := m[2,1]; hdr.srow_z[2] := m[2,2]; hdr.srow_z[3] := m[2,3];
end;

procedure ApplyVolumeReorient(perm: TVec3i; outR: TMat4; var fDim: TVec3i; var fScale : TVec3; var fHdr  : TNIFTIhdr; var rawVolBytes: TUInt8s);
var
   flp: TVec3i;
    inScale: TVec3;
    inDim, inStride, outStride : TVec3i;
    i8: UInt8;
    i16: Int16;
    i32: Int32;
    half, mx: int64;
    in8: TUInt8s;
    in16, out16: TInt16s;
    in32, out32: TInt32s;
    in24, out24: TRGBs;
    inperm: TVec3i;
    xOffset, yOffset, zOffset: array of int64;
    voxOffset, byteOffset, volBytes,vol, volumesLoaded,  x,y,z, i: int64;
(*procedure flipLR(); inline;
var
   halfx, x, y, nx, xend, nyzt: int64;
   tmp8: uint8;
begin
 nyzt := max(1, fDim.y) * max(1, fDim.z) * max(1, volumesLoaded);
 nx := fDim.x;
 if nx < 2 then exit;
 halfx := nx div 2;
 i := 0;
 for y :=  0 to (nyzt-1) do begin //for each row
  xend := i + nx - 1;
  for x := 0 to (halfx) do begin
  	  tmp8 := rawVolBytes[i+x];
      rawVolBytes[i+x] := rawVolBytes[xend-x];
      rawVolBytes[xend-x] := tmp8;
  end;
  i := i + nx;
 end;
end; //flipLR  *)
begin
  if (perm.x = 1) and (perm.y = 2) and (perm.z = 3) then begin
     Mat2SForm(outR, fHdr); //could skip: no change!
    exit;
  end;
  inperm := perm;
  printf(format('Reorient Dimensions %d %d %d', [perm.x, perm.y, perm.z]));
 for i := 0 to 2 do begin
       flp.v[i] := 0;
       if (perm.v[i] < 0) then flp.v[i] := 1;
       perm.v[i] := abs(perm.v[i]);
   end;
  if (perm.x = perm.y) or (perm.x = perm.z) or (perm.y = perm.z) or ((perm.x+perm.y+perm.z) <> 6 ) then begin
     Mat2SForm(outR, fHdr); //could skip: bogus
    exit;
  end;

  inDim := fDim;
 inStride.x := 1;
 inStride.y := inDim.x;
 inStride.z := inDim.x * inDim.y;
 outStride.x := inStride.v[perm.x-1];
 outStride.y := inStride.v[perm.y-1];
 outStride.z := inStride.v[perm.z-1];
 //set outputs
 fDim.x := inDim.v[perm.x-1];
 fDim.y := inDim.v[perm.y-1];
 fDim.z := inDim.v[perm.z-1];
 inscale := fScale;
 fScale.x := inScale.v[perm.x-1];
 fScale.y := inScale.v[perm.y-1];
 fScale.z := inScale.v[perm.z-1];
 volBytes := fHdr.Dim[1]*fHdr.Dim[2]*fHdr.Dim[3]* (fHdr.bitpix shr 3);
 volumesLoaded := length(rawVolBytes) div volBytes;
 Mat2SForm(outR, fHdr);
 Mat2QForm(outR, fHdr);
 if (fHdr.bitpix <> 24) and (inperm.x = -1) and (inperm.y = 2) and (inperm.z = 3) and (fDim.x > 2) then begin
    //optimize most common case of left-right mirror: no need to copy memory, 240ms -> 170ms
     half := (fDim.x-1) div 2; // [0 1 2]
     mx := fDim.x - 1;
     i := 0;
     {$DEFINE OLD8} //no benefit of line copies
     {$IFDEF OLD8}
     setlength(in8, fDim.x);
     if (fHdr.bitpix = 8) then begin
       for vol := 1 to volumesLoaded do
           for z := 0 to (fDim.z - 1) do
               for y := 0 to (fDim.y - 1) do begin
                   for x := 0 to half do begin
                       i8 := rawVolBytes[i+(mx-x)];
                       rawVolBytes[i+(mx-x)] := rawVolBytes[i+x];
                       rawVolBytes[i+x] := i8;
                   end;
                   i := i + fDim.x;
               end;
     end;
     {$ELSE}
     if (fHdr.bitpix = 8) then begin
       setlength(in8, fDim.x);
       for vol := 1 to volumesLoaded do
          for z := 0 to (fDim.z - 1) do
              for y := 0 to (fDim.y - 1) do begin
                  in8 := Copy(rawVolBytes, i, mx+1);
                  for x := 0 to mx do
                      rawVolBytes[i+x] := rawVolBytes[i+(mx-x)];
                  i := i + fDim.x;
              end;
       in8 := nil;
     end;
     {$ENDIF}
     if (fHdr.bitpix = 16) then begin
       out16 := TInt16s(rawVolBytes);
       for vol := 1 to volumesLoaded do
           for z := 0 to (fDim.z - 1) do
               for y := 0 to (fDim.y - 1) do begin
                   for x := 0 to half do begin
                      i16 := out16[i+(mx-x)];
                      out16[i+(mx-x)] := out16[i+x];
                      out16[i+x] := i16;
                   end;
                   i := i + fDim.x;
               end;
     end;
     if (fHdr.bitpix = 32) then begin
       out32 := TInt32s(rawVolBytes);
       for vol := 1 to volumesLoaded do
           for z := 0 to (fDim.z - 1) do
               for y := 0 to (fDim.y - 1) do begin
                   for x := 0 to half do begin
                       i32 := out32[i+(mx-x)];
                       out32[i+(mx-x)] := out32[i+x];
                       out32[i+x] := i32;
                   end;
                   i := i + fDim.x;
               end;
     end;
     exit;
 end;
 //setup lookup tables
 setlength(xOffset, fDim.x);
 if flp.x = 1 then begin
    for x := 0 to (fDim.x - 1) do
        xOffset[fDim.x-1-x] := x*outStride.x;
 end else
     for x := 0 to (fDim.x - 1) do
         xOffset[x] := x*outStride.x;
 setlength(yOffset, fDim.y);
 if flp.y = 1 then begin
    for y := 0 to (fDim.y - 1) do
        yOffset[fDim.y-1-y] := y*outStride.y;
 end else
     for y := 0 to (fDim.y - 1) do
         yOffset[y] := y*outStride.y;
 setlength(zOffset, fDim.z);
 if flp.z = 1 then begin
    for z := 0 to (fDim.z - 1) do
        zOffset[fDim.z-1-z] := z*outStride.z;
 end else
     for z := 0 to (fDim.z - 1) do
         zOffset[z] := z*outStride.z;
 //copy data
 SetLength(in8, volBytes);
 if volumesLoaded < 1 then exit;
 for vol := 1 to volumesLoaded do begin
   byteOffset := (vol-1) * volBytes;
   voxOffset := fHdr.Dim[1]*fHdr.Dim[2]*fHdr.Dim[3]* (vol-1);
   in8 := Copy(rawVolBytes, byteOffset, volBytes);
   if fHdr.bitpix = 8 then begin
      i := voxOffset;
      for z := 0 to (fDim.z - 1) do
          for y := 0 to (fDim.y - 1) do
              for x := 0 to (fDim.x - 1) do begin
                rawVolBytes[i] := in8[xOffset[x]+yOffset[y]+zOffset[z]];
                i := i + 1;
              end;
   end;
   if fHdr.bitpix = 16 then begin
      in16 := TInt16s(in8);
      out16 := TInt16s(rawVolBytes);
      i := voxOffset;
      for z := 0 to (fDim.z - 1) do
          for y := 0 to (fDim.y - 1) do
              for x := 0 to (fDim.x - 1) do begin
                out16[i] := in16[xOffset[x]+yOffset[y]+zOffset[z]];
                i := i + 1;
              end;
   end;
   if fHdr.bitpix = 24 then begin
      in24 := TRGBs(in8);
      out24 := TRGBs(rawVolBytes);
      i := voxOffset;
      for z := 0 to (fDim.z - 1) do
          for y := 0 to (fDim.y - 1) do
              for x := 0 to (fDim.x - 1) do begin
                out24[i] := in24[xOffset[x]+yOffset[y]+zOffset[z]];
                i := i + 1;
              end;
   end;
   if fHdr.bitpix = 32 then begin
      in32 := TInt32s(in8);
      out32 := TInt32s(rawVolBytes);
      i := voxOffset;
      for z := 0 to (fDim.z - 1) do
          for y := 0 to (fDim.y - 1) do
              for x := 0 to (fDim.x - 1) do begin
                out32[i] := in32[xOffset[x]+yOffset[y]+zOffset[z]];
                i := i + 1;
              end;
   end;
 end; //for vol 1..volumesLoaded
 xOffset := nil;
 yOffset := nil;
 zOffset := nil;
 in8 := nil;
 fHdr.dim[1] := fDim.X;
 fHdr.dim[2] := fDim.Y;
 fHdr.dim[3] := fDim.Z;
 //shuffle pixdim
 inScale[0] := fHdr.pixdim[1];
 inScale[1] := fHdr.pixdim[2];
 inScale[2] := fHdr.pixdim[3];
 fHdr.pixdim[1] := inScale.v[perm.x-1];
 fHdr.pixdim[2] := inScale.v[perm.y-1];
 fHdr.pixdim[3] := inScale.v[perm.z-1];
 //showmessage(format('%g %g %g', [fHdr.qoffset_x, fHdr.qoffset_y, fHdr.qoffset_z]));
end;


function extractfileextX(fnm: string): string; //treat ".nii.gz" as single extension, return .NII.GZ
var
  s: string;
begin
 result := upcase(extractfileext(fnm));
 if  (result <> '.GZ') and (result <> '.BZ2')  then exit;
 s := changefileext(fnm,'');
 result := upcase(extractfileext(s))+result;
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

(*function ChangeFileExtX(fnm, ext: string): string; //treat ".nii.gz" as single extension
begin
 result := changefileext(fnm,'');
 if upcase(extractfileext(result)) = '.NII' then
      result := changefileext(result,'');
 result := result + ext;
end;*)

(*function EstimateResidual(dim : TVec3i; inR: TMat4; perm : TVec3i): TMat4;
//compute dimension permutations and flips to reorient volume to standard space
//From Xiangrui Li's BSD 2-Clause Licensed code
// https://github.com/xiangruili/dicm2nii/blob/master/nii_viewer.m
var
  rotM: TMat4;
  flp : TVec3i;
begin
  //sort columns  R(:,1:3) = R(:,perm);
  if perm.x < 0 then flp.x := 1 else flp.x := 0;
  if perm.y < 0 then flp.y := 1 else flp.y := 0;
  if perm.z < 0 then flp.z := 1 else flp.z := 0;
  if (perm.x = 1) and (perm.y = 2) and (perm.z = 3) and (flp.x = 0) and (flp.y = 0) and (flp.z = 0) then exit(inR);//already oriented correctly
  rotM := TMat4.Identity;
  rotM[0,0] := 1-flp.x*2;
  rotM[1,1] := 1-flp.y*2;
  rotM[2,2] := 1-flp.z*2;
  rotM[0,3] := ((dim.v[abs(perm.x)-1])-1) * flp.x;
  rotM[1,3] := ((dim.v[abs(perm.y)-1])-1) * flp.y;
  rotM[2,3] := ((dim.v[abs(perm.z)-1])-1) * flp.z;
  result := rotM.Inverse;
  result *= inR;
end; *)


function mStr(prefix: string; m: TMat4): string;
begin
    result := format('%s = [%g %g %g %g; %g %g %g %g; %g %g %g %g; %g %g %g %g]; ', [prefix,
      m[0,0], m[0,1],m[0,2],m[0,3],
      m[1,0], m[1,1],m[1,2],m[1,3],
      m[2,0], m[2,1],m[2,2],m[2,3],
      m[3,0], m[3,1],m[3,2],m[3,3]]);
end; //

//rM = M*spm_get_space(fnm);
//function EstimateResidual(dim : TVec3i; inR: TMat4; perm : TVec3i): TMat4;
function EstimateResidual(inR: TMat4; perm : TVec3i): TMat4;
//compute dimension permutations and flips to reorient volume to standard space
//From Xiangrui Li's BSD 2-Clause Licensed code
// https://github.com/xiangruili/dicm2nii/blob/master/nii_viewer.m
var
  M: TMat4;
begin
  if (perm.x = 1) and (perm.y = 2) and (perm.z = 3) then exit(inR);//already oriented correctly
  M := TMat4.Diag(0.0, 0.0, 0.0);// zero3D;
  if perm.x < 0 then
      M[0,abs(perm.x)-1] := -1
  else
       M[0,abs(perm.x)-1] := 1;
   if perm.y < 0 then
      M[1,abs(perm.y)-1] := -1
   else
       M[1,abs(perm.y)-1] := 1;
   if perm.z < 0 then
      M[2,abs(perm.z)-1] := -1
   else
       M[2,abs(perm.z)-1] := 1;
  result := inR * M;
  //Clipboard.AsText := mStr('in', inR)+chr(10)+mStr('M', M)+chr(10)+mStr('result', result);
end;


(*procedure ApplyVolumeReorientx(perm: TVec3i; outR: TMat4; var fDim: TVec3i; var fScale : TVec3; var fHdr  : TNIFTIhdr; var rawVolBytes: TUInt8s);
var
   flp: TVec3i;
    inScale: TVec3;
    inDim, inStride, outStride : TVec3i;
    in8: TUInt8s;
    in16, out16: TInt16s;
    in32, out32: TInt32s;
    in24, out24: TRGBs;
    xOffset, yOffset, zOffset: array of integer;
    voxOffset, byteOffset, volBytes,vol, volumesLoaded,  x,y,z, i: int64;
begin
  if (perm.x = 1) and (perm.y = 2) and (perm.z = 3) then begin
     Mat2SForm(outR, fHdr); //could skip: no change!
    exit;
  end;
  for i := 0 to 2 do begin
       flp.v[i] := 0;
       if (perm.v[i] < 0) then flp.v[i] := 1;
       perm.v[i] := abs(perm.v[i]);
   end;
  showmessage(format('%d %d %d', [flp.x, flp.y, flp.z]));
  if (perm.x = perm.y) or (perm.x = perm.z) or (perm.y = perm.z) or ((perm.x+perm.y+perm.z) <> 6 ) then begin
     Mat2SForm(outR, fHdr); //could skip: bogus
    exit;
  end;
  inDim := fDim;
 inStride.x := 1;
 inStride.y := inDim.x;
 inStride.z := inDim.x * inDim.y;
 outStride.x := inStride.v[perm.x-1];
 outStride.y := inStride.v[perm.y-1];
 outStride.z := inStride.v[perm.z-1];
 //set outputs
 fDim.x := inDim.v[perm.x-1];
 fDim.y := inDim.v[perm.y-1];
 fDim.z := inDim.v[perm.z-1];
 inscale := fScale;
 fScale.x := inScale.v[perm.x-1];
 fScale.y := inScale.v[perm.y-1];
 fScale.z := inScale.v[perm.z-1];
 Mat2SForm(outR, fHdr);
 Mat2QForm(outR, fHdr);
 //setup lookup tables
 setlength(xOffset, fDim.x);
 if flp.x = 1 then begin
    for x := 0 to (fDim.x - 1) do
        xOffset[fDim.x-1-x] := x*outStride.x;
 end else
     for x := 0 to (fDim.x - 1) do
         xOffset[x] := x*outStride.x;
 setlength(yOffset, fDim.y);
 if flp.y = 1 then begin
    for y := 0 to (fDim.y - 1) do
        yOffset[fDim.y-1-y] := y*outStride.y;
 end else
     for y := 0 to (fDim.y - 1) do
         yOffset[y] := y*outStride.y;
 setlength(zOffset, fDim.z);
 if flp.z = 1 then begin
    for z := 0 to (fDim.z - 1) do
        zOffset[fDim.z-1-z] := z*outStride.z;
 end else
     for z := 0 to (fDim.z - 1) do
         zOffset[z] := z*outStride.z;
 //copy data
 volBytes := fHdr.Dim[1]*fHdr.Dim[2]*fHdr.Dim[3]* (fHdr.bitpix shr 3);
 volumesLoaded := length(rawVolBytes) div volBytes;
 SetLength(in8, volBytes);
 if volumesLoaded < 1 then exit;
 for vol := 1 to volumesLoaded do begin
   byteOffset := (vol-1) * volBytes;
   voxOffset := fHdr.Dim[1]*fHdr.Dim[2]*fHdr.Dim[3]* (vol-1);
   in8 := Copy(rawVolBytes, byteOffset, volBytes);
   if fHdr.bitpix = 8 then begin
      i := voxOffset;
      for z := 0 to (fDim.z - 1) do
          for y := 0 to (fDim.y - 1) do
              for x := 0 to (fDim.x - 1) do begin
                rawVolBytes[i] := in8[xOffset[x]+yOffset[y]+zOffset[z]];
                i := i + 1;
              end;
   end;
   if fHdr.bitpix = 16 then begin
      in16 := TInt16s(in8);
      out16 := TInt16s(rawVolBytes);
      i := voxOffset;
      for z := 0 to (fDim.z - 1) do
          for y := 0 to (fDim.y - 1) do
              for x := 0 to (fDim.x - 1) do begin
                out16[i] := in16[xOffset[x]+yOffset[y]+zOffset[z]];
                i := i + 1;
              end;
   end;
   if fHdr.bitpix = 24 then begin
      in24 := TRGBs(in8);
      out24 := TRGBs(rawVolBytes);
      i := voxOffset;
      for z := 0 to (fDim.z - 1) do
          for y := 0 to (fDim.y - 1) do
              for x := 0 to (fDim.x - 1) do begin
                out24[i] := in24[xOffset[x]+yOffset[y]+zOffset[z]];
                i := i + 1;
              end;
   end;
   if fHdr.bitpix = 32 then begin
      in32 := TInt32s(in8);
      out32 := TInt32s(rawVolBytes);
      i := voxOffset;
      for z := 0 to (fDim.z - 1) do
          for y := 0 to (fDim.y - 1) do
              for x := 0 to (fDim.x - 1) do begin
                out32[i] := in32[xOffset[x]+yOffset[y]+zOffset[z]];
                i := i + 1;
              end;
   end;
 end; //for vol 1..volumesLoaded
 xOffset := nil;
 yOffset := nil;
 zOffset := nil;
 in8 := nil;
 fHdr.dim[1] := fDim.X;
 fHdr.dim[2] := fDim.Y;
 fHdr.dim[3] := fDim.Z;
 //shuffle pixdim
 inScale[0] := fHdr.pixdim[1];
 inScale[1] := fHdr.pixdim[2];
 inScale[2] := fHdr.pixdim[3];
 fHdr.pixdim[1] := inScale.v[perm.x-1];
 fHdr.pixdim[2] := inScale.v[perm.y-1];
 fHdr.pixdim[3] := inScale.v[perm.z-1];
 //showmessage(format('%g %g %g', [fHdr.qoffset_x, fHdr.qoffset_y, fHdr.qoffset_z]));
end;   *)

(*function EstimateReorientx(dim : TVec3i; R: TMat4; out residualR: TMat4; in inperm : TVec3i): boolean;
//compute dimension permutations and flips to reorient volume to standard space
//From Xiangrui Li's BSD 2-Clause Licensed code
// https://github.com/xiangruili/dicm2nii/blob/master/nii_viewer.m
var
  a, rotM: TMat4;
  i,j: integer;
  flp, perm : TVec3i;
begin
  if (perm.x = 1) and (perm.y = 2) and (perm.z = 3) then exit (false);//already oriented correctly
  result := true; //reorient required!
  for i := 0 to 2 do begin
      perm[i] := abs(inperm[i]);
      flip[i] := inperm[i] < 0;
  end;
  rotM := R;
  for i := 0 to 3 do
      for j := 0 to 2 do
          R[i,j] := rotM[i,perm.v[j]-1];
  //compute if dimension is flipped
  if R[0,0] < 0 then flp.x := 1 else flp.x := 0;
  if R[1,1] < 0 then flp.y := 1 else flp.y := 0;
  if R[2,2] < 0 then flp.z := 1 else flp.z := 0;
  rotM := TMat4.Identity;
  rotM[0,0] := 1-flp.x*2;
  rotM[1,1] := 1-flp.y*2;
  rotM[2,2] := 1-flp.z*2;
  rotM[0,3] := ((dim.v[perm.x-1])-1) * flp.x;
  rotM[1,3] := ((dim.v[perm.y-1])-1) * flp.y;
  rotM[2,3] := ((dim.v[perm.z-1])-1) * flp.z;
  residualR := rotM.Inverse;
  residualR *= R;
end;*)

function EstimateReorienty(dim : TVec3i; R: TMat4; out residualR: TMat4; out perm : TVec3i): boolean;
//compute dimension permutations and flips to reorient volume to standard space
//From Xiangrui Li's BSD 2-Clause Licensed code
// https://github.com/xiangruili/dicm2nii/blob/master/nii_viewer.m
var
  a, rotM: TMat4;
  i,j: integer;
  flp,ixyz : TVec3i;
begin
  result := false;
  a := TMat4.Identity;
  //memo1.lines.add(writeMat('R',R));
  for i := 0 to 3 do
      for j := 0 to 3 do
          a[i,j] := abs(R[i,j]);
  //memo1.lines.add(writeMat('a',a));
  //first column = x
  ixyz.x := 1;
  if (a[1,0] > a[0,0]) then ixyz.x := 2;
  if (a[2,0] > a[0,0]) and (a[2,0]> a[1,0]) then ixyz.x := 3;
  //second column = y: constrained as must not be same row as x
  if (ixyz.x = 1) then begin
     if (a[1,1] > a[2,1]) then
        ixyz.y := 2
     else
         ixyz.y := 3;
  end else if (ixyz.x = 2) then begin
     if (a[0,1] > a[2,1]) then
        ixyz.y := 1
     else
         ixyz.y := 3;
  end else begin //ixyz.x = 3
     if (a[0,1] > a[1,1]) then
        ixyz.y := 1
     else
         ixyz.y := 2;
  end;
  //third column = z:constrained as x+y+z = 1+2+3 = 6
  ixyz.z := 6 - ixyz.y - ixyz.x;
  perm.v[ixyz.x-1] := 1;
  perm.v[ixyz.y-1] := 2;
  perm.v[ixyz.z-1] := 3;
  //sort columns  R(:,1:3) = R(:,perm);
  rotM := R;
  for i := 0 to 3 do
      for j := 0 to 2 do
          R[i,j] := rotM[i,perm.v[j]-1];
  //compute if dimension is flipped
  if R[0,0] < 0 then flp.x := 1 else flp.x := 0;
  if R[1,1] < 0 then flp.y := 1 else flp.y := 0;
  if R[2,2] < 0 then flp.z := 1 else flp.z := 0;
  if (perm.x = 1) and (perm.y = 2) and (perm.z = 3) and (flp.x = 0) and (flp.y = 0) and (flp.z = 0) then exit;//already oriented correctly
  result := true; //reorient required!
  rotM := TMat4.Identity;
  rotM[0,0] := 1-flp.x*2;
  rotM[1,1] := 1-flp.y*2;
  rotM[2,2] := 1-flp.z*2;
  rotM[0,3] := ((dim.v[perm.x-1])-1) * flp.x;
  rotM[1,3] := ((dim.v[perm.y-1])-1) * flp.y;
  rotM[2,3] := ((dim.v[perm.z-1])-1) * flp.z;
  residualR := rotM.Inverse;
  residualR *= R;
  for i := 0 to 2 do
      if (flp.v[i] <> 0) then perm.v[i] := -perm.v[i];
end;

(*procedure ApplyVolumeReorienty(perm: TVec3i; outR: TMat4; var fDim: TVec3i; var fScale : TVec3; var fHdr  : TNIFTIhdr; var rawVolBytes: TUInt8s);
var
   flp: TVec3i;
    inScale: TVec3;
    inDim, inStride, outStride : TVec3i;
    in8: TUInt8s;
    in16, out16: TInt16s;
    in32, out32: TInt32s;
    in24, out24: TRGBs;
    xOffset, yOffset, zOffset: array of int64;
    voxOffset, byteOffset, volBytes,vol, volumesLoaded,  x,y,z, i: int64;
begin
  if (perm.x = 1) and (perm.y = 2) and (perm.z = 3) then begin
     Mat2SForm(outR, fHdr); //could skip: no change!
    exit;
  end;
  for i := 0 to 2 do begin
       flp.v[i] := 0;
       if (perm.v[i] < 0) then flp.v[i] := 1;
       perm.v[i] := abs(perm.v[i]);
   end;
  if (perm.x = perm.y) or (perm.x = perm.z) or (perm.y = perm.z) or ((perm.x+perm.y+perm.z) <> 6 ) then begin
     Mat2SForm(outR, fHdr); //could skip: bogus
    exit;
  end;
  inDim := fDim;
 inStride.x := 1;
 inStride.y := inDim.x;
 inStride.z := inDim.x * inDim.y;
 outStride.x := inStride.v[perm.x-1];
 outStride.y := inStride.v[perm.y-1];
 outStride.z := inStride.v[perm.z-1];
 //set outputs
 fDim.x := inDim.v[perm.x-1];
 fDim.y := inDim.v[perm.y-1];
 fDim.z := inDim.v[perm.z-1];
 inscale := fScale;
 fScale.x := inScale.v[perm.x-1];
 fScale.y := inScale.v[perm.y-1];
 fScale.z := inScale.v[perm.z-1];
 Mat2SForm(outR, fHdr);
 Mat2QForm(outR, fHdr);
 //setup lookup tables
 setlength(xOffset, fDim.x);
 if flp.x = 1 then begin
    for x := 0 to (fDim.x - 1) do
        xOffset[fDim.x-1-x] := x*outStride.x;
 end else
     for x := 0 to (fDim.x - 1) do
         xOffset[x] := x*outStride.x;
 setlength(yOffset, fDim.y);
 if flp.y = 1 then begin
    for y := 0 to (fDim.y - 1) do
        yOffset[fDim.y-1-y] := y*outStride.y;
 end else
     for y := 0 to (fDim.y - 1) do
         yOffset[y] := y*outStride.y;
 setlength(zOffset, fDim.z);
 if flp.z = 1 then begin
    for z := 0 to (fDim.z - 1) do
        zOffset[fDim.z-1-z] := z*outStride.z;
 end else
     for z := 0 to (fDim.z - 1) do
         zOffset[z] := z*outStride.z;
 //copy data
 volBytes := fHdr.Dim[1]*fHdr.Dim[2]*fHdr.Dim[3]* (fHdr.bitpix shr 3);
 volumesLoaded := length(rawVolBytes) div volBytes;
 SetLength(in8, volBytes);
 showmessage(format('-> %d  %d', [length(rawVolBytes), volBytes]));

 if volumesLoaded < 1 then exit;
 showmessage(format('-> %d %d %d : %d, %d', [flp.x, flp.y, flp.z, xOffset[0], xOffset[1]]));

 for vol := 1 to volumesLoaded do begin
   byteOffset := (vol-1) * volBytes;
   voxOffset := fHdr.Dim[1]*fHdr.Dim[2]*fHdr.Dim[3]* (vol-1);
   in8 := Copy(rawVolBytes, byteOffset, volBytes);
   if fHdr.bitpix = 8 then begin
      i := voxOffset;
      for z := 0 to (fDim.z - 1) do
          for y := 0 to (fDim.y - 1) do
              for x := 0 to (fDim.x - 1) do begin
                rawVolBytes[i] := in8[xOffset[x]+yOffset[y]+zOffset[z]];
                i := i + 1;
              end;
   end;
   if fHdr.bitpix = 16 then begin
      in16 := TInt16s(in8);
      out16 := TInt16s(rawVolBytes);
      i := voxOffset;
      for z := 0 to (fDim.z - 1) do
          for y := 0 to (fDim.y - 1) do
              for x := 0 to (fDim.x - 1) do begin
                out16[i] := in16[xOffset[x]+yOffset[y]+zOffset[z]];
                i := i + 1;
              end;
   end;
   if fHdr.bitpix = 24 then begin
      in24 := TRGBs(in8);
      out24 := TRGBs(rawVolBytes);
      i := voxOffset;
      for z := 0 to (fDim.z - 1) do
          for y := 0 to (fDim.y - 1) do
              for x := 0 to (fDim.x - 1) do begin
                out24[i] := in24[xOffset[x]+yOffset[y]+zOffset[z]];
                i := i + 1;
              end;
   end;
   if fHdr.bitpix = 32 then begin
      in32 := TInt32s(in8);
      out32 := TInt32s(rawVolBytes);
      i := voxOffset;
      for z := 0 to (fDim.z - 1) do
          for y := 0 to (fDim.y - 1) do
              for x := 0 to (fDim.x - 1) do begin
                out32[i] := in32[xOffset[x]+yOffset[y]+zOffset[z]];
                i := i + 1;
              end;
   end;
 end; //for vol 1..volumesLoaded
 xOffset := nil;
 yOffset := nil;
 zOffset := nil;
 in8 := nil;
 fHdr.dim[1] := fDim.X;
 fHdr.dim[2] := fDim.Y;
 fHdr.dim[3] := fDim.Z;
 //shuffle pixdim
 inScale[0] := fHdr.pixdim[1];
 inScale[1] := fHdr.pixdim[2];
 inScale[2] := fHdr.pixdim[3];
 fHdr.pixdim[1] := inScale.v[perm.x-1];
 fHdr.pixdim[2] := inScale.v[perm.y-1];
 fHdr.pixdim[3] := inScale.v[perm.z-1];
 //showmessage(format('%g %g %g', [fHdr.qoffset_x, fHdr.qoffset_y, fHdr.qoffset_z]));
end;
*)

function NIFTIhdr_SlicesToCoord (var lHdr: TNIFTIhdr; lXslice,lYslice,lZslice: integer): TVec3;
//ignores origin offset
begin
    result.x := (lHdr.srow_x[0]*lXslice)+ (lHdr.srow_x[1]*lYslice)+(lHdr.srow_x[2]*lzslice);
    result.y := (lHdr.srow_y[0]*lXslice)+ (lHdr.srow_y[1]*lYslice)+(lHdr.srow_y[2]*lzslice);
    result.z := (lHdr.srow_z[0]*lXslice)+ (lHdr.srow_z[1]*lYslice)+(lHdr.srow_z[2]*lzslice);
end;

procedure TNIfTI.VolumeReorient();
//rotate volume so X ~ left->right, Y ~ posterior->anterior, Z ~ inferior->superior
//rotations are lossless so only to nearest orthogonal
var
   outR: TMat4;
begin
     if fHdr.sform_code = kNIFTI_XFORM_UNKNOWN then exit;
     //R := SForm2Mat(fHdr);
     if not EstimateReorient(fDim, fMatInOrient, outR, fPermInOrient)  then exit;
     ApplyVolumeReorient(fPermInOrient, outR, fDim, fScale, fHdr, fRawVolBytes);
     fMat := outR;
end;

procedure TNIfTI.MakeBorg(voxelsPerDimension: integer);
const
 Border : integer= 4;//margin so we can calculate gradients at edge
var
  d, I, X, Y, Z: integer;
 F: array of single;
 mn, mx, slope: single;
begin
 d := voxelsPerDimension;
 fBidsName := '';
 NII_Clear(fhdr);
 fhdr.dim[0] := 3;
 fhdr.dim[1] := d;
 fhdr.dim[2] := d;
 fhdr.dim[3] := d;
 fhdr.bitpix := 8;
 fhdr.datatype := kDT_UNSIGNED_CHAR;
 fhdr.intent_code:= kNIFTI_INTENT_NONE;
 fKnownOrientation := true;
 fHdr.cal_min:= 0;
 fHdr.cal_max:= 255;
 for i := 0 to 3 do begin
     fHdr.srow_x[i] := 0.0;
     fHdr.srow_y[i] := 0.0;
     fHdr.srow_z[i] := 0.0;
 end;
 fHdr.srow_x[0] := 1.0;
 fHdr.srow_y[1] := 1.0;
 fHdr.srow_z[2] := 1.0;
 fHdr.srow_x[3] := -d/2.0;
 fHdr.srow_y[3] := -d/2.0;
 fHdr.srow_z[3] := -d/2.0;
 fHdr.sform_code:= kNIFTI_XFORM_SCANNER_ANAT;
 if (voxelsPerDimension <= Border) then begin
    SetLength(fRawVolBytes, d*d*d);
    for I := 0 to d*d*d-1 do
        fRawVolBytes[I] := 0;
    exit;
 end;
 SetLength(F, d * d * d);
 slope := 0.005;
 I := 0;
 for X := 0 to d-1 do
  for Y := 0 to d-1 do
   for Z := 0 to d-1 do
   begin
    if (X < Border) or (Y < Border) or (Z < Border) or ((d-X) < Border) or ((d-Y) < Border) or ((d-Z) < Border) then
     F[I] := 0
    else
     F[I] := sin(slope *x *y) + sin(slope *y * z) + sin(slope *z * x);
    Inc(I);
   end;
 //next find range...
 mn := F[0];
 for I := 0 to d*d*d-1 do
  if F[I] < mn then
   mn := F[I];
 mx := F[0];
 for I := 0 to d*d*d-1 do
  if F[I] > mx then
   mx := F[I];
 slope := 255/(mx-mn);
 SetLength(fRawVolBytes, d*d*d);
 for I := 0 to d*d*d-1 do begin
  if F[I] <= 0 then
   fRawVolBytes[I] := 0
  else
   fRawVolBytes[I] := Round((F[I]-mn)*slope);
 end;
 F := nil;
end;

procedure SwapImg(var  rawData: TUInt8s; bitpix: integer);
var
   i16s: TInt16s;
   i32s: TInt32s;
   f64s: TFloat64s;
   i, n: int64;
begin
     if bitpix < 15 then exit;
     if bitpix = 16 then begin
        n := length(rawData) div 2;
        i16s := TInt16s(rawData);
        for i := 0 to (n-1) do
            i16s[i] := swap2(i16s[i]);
     end;
     if bitpix = 32 then begin
        n := length(rawData) div 4;
        i32s := TInt32s(rawData);
        for i := 0 to (n-1) do
            swap4(i32s[i]);
     end;
     if bitpix = 64 then begin
        n := length(rawData) div 8;
        f64s := TFloat64s(rawData);
        for i := 0 to (n-1) do
            Xswap8r(f64s[i]);
     end;
     if bitpix = 128 then begin
        printf('TODO: need to handle 128-bit foreign endian');

     end;
end;

procedure nifti_quatern_to_mat44( out lR :TMat4;
                             var qb, qc, qd,
                             qx, qy, qz,
                             dx, dy, dz, qfac : single);
var
   a,b,c,d,xd,yd,zd: double;
begin
   //a := qb;
   b := qb;
   c := qc;
   d := qd;
   //* last row is always [ 0 0 0 1 ] */
   lR := TMat4.Identity;
   //* compute a parameter from b,c,d */
   a := 1.0 - (b*b + c*c + d*d) ;
   if( a < 1.e-7 ) then begin//* special case */
     a := 1.0 / sqrt(b*b+c*c+d*d) ;
     b := b*a ; c := c*a ; d := d*a ;//* normalize (b,c,d) vector */
     a := 0.0 ;//* a = 0 ==> 180 degree rotation */
   end else begin
     a := sqrt(a) ; //* angle = 2*arccos(a) */
   end;
   //* load rotation matrix, including scaling factors for voxel sizes */
   if dx > 0 then
      xd := dx
   else
       xd := 1;
   if dy > 0 then
      yd := dy
   else
       yd := 1;
   if dz > 0 then
      zd := dz
   else
       zd := 1;
   if( qfac < 0.0 ) then zd := -zd ;//* left handedness? */
   lR[0,0] := (a*a+b*b-c*c-d*d) * xd ;
   lR[0,1] := 2.0 * (b*c-a*d        ) * yd ;
   lR[0,2]:= 2.0 * (b*d+a*c        ) * zd ;
   lR[1,0] := 2.0 * (b*c+a*d        ) * xd ;
   lR[1,1] := (a*a+c*c-b*b-d*d) * yd ;
   lR[1,2] :=  2.0 * (c*d-a*b        ) * zd ;
   lR[2,0] := 2.0 * (b*d-a*c        ) * xd ;
   lR[2,1] :=  2.0 * (c*d+a*b        ) * yd ;
   lR[2,2] :=         (a*a+d*d-c*c-b*b) * zd ;
   //* load offsets */
   lR[0,3]:= qx ;
   lR[1,3]:= qy ;
   lR[2,3]:= qz ;
end;

procedure fixBogusHeaders(var h: TNIFTIhdr);
var
   isBogus : boolean = false;
   i: integer;
procedure checkSingle(var f: single);
begin
 if (specialSingle(f)) or (f = 0.0) then begin
    isBogus := true;
    f := 1.0;
 end;
end;
begin
  checkSingle(h.scl_slope); //https://github.com/nipreps/fmriprep/issues/2507
  if specialSingle(h.scl_inter) then begin
    h.scl_inter := 0.0;
    isBogus := true;
  end;
  if (isBogus) then
    printf('Bogus scl_slope/scl_inter data scaling repaired.');
  isBogus := false;
  checkSingle(h.PixDim[1]);
  checkSingle(h.PixDim[2]);
  checkSingle(h.PixDim[3]);
  if (isBogus) then
    printf('Bogus PixDim repaired. Check dimensions and orientation.');
  isBogus := false;
  for i := 0 to 3 do begin
      if specialSingle(h.srow_x[i]) then isBogus := true;
      if specialSingle(h.srow_y[i]) then isBogus := true;
      if specialSingle(h.srow_z[i]) then isBogus := true;
  end;
  if (h.srow_x[0] = 0) and (h.srow_x[1] = 0) and (h.srow_x[2] = 0) then isBogus := true;
  if (h.srow_y[0] = 0) and (h.srow_y[1] = 0) and (h.srow_y[2] = 0) then isBogus := true;
  if (h.srow_z[0] = 0) and (h.srow_z[1] = 0) and (h.srow_z[2] = 0) then isBogus := true;
  if (h.srow_x[0] = 0) and (h.srow_y[0] = 0) and (h.srow_z[0] = 0) then isBogus := true;
  if (h.srow_x[1] = 0) and (h.srow_y[1] = 0) and (h.srow_z[1] = 0) then isBogus := true;
  if (h.srow_x[2] = 0) and (h.srow_y[2] = 0) and (h.srow_z[2] = 0) then isBogus := true;
  if (not isBogus) then exit;
  NII_SetIdentityMatrix(h);
  printf('Bogus header repaired. Check orientation.');
end;

procedure fixHdr( var lHdr: TNIfTIHdr );
//ensure bitpix and datatype match
// fix headers where possible, e.g. ImageJ saves RGB images as bitpix=24, datatype=0 dt should be 128)
var
   bitpix: integer;
begin
     if (lHdr.bitpix = 1) and (lHdr.datatype = kDT_BINARY) then
        exit;
     if (lHdr.bitpix = 8) and ((lHdr.datatype = kDT_UINT8) or (lHdr.datatype = kDT_INT8)) then
        exit;
     if (lHdr.bitpix = 16) and ((lHdr.datatype = kDT_UINT16) or (lHdr.datatype = kDT_INT16)) then
        exit;
     if (lHdr.bitpix = 24) and (lHdr.datatype = kDT_RGB) then
        exit;
     if (lHdr.bitpix = 32) and ((lHdr.datatype = kDT_UINT32) or (lHdr.datatype = kDT_INT32) or (lHdr.datatype = kDT_FLOAT32) or (lHdr.datatype = kDT_RGBA32) ) then
        exit;
     if (lHdr.bitpix = 64) and ((lHdr.datatype = kDT_UINT64) or (lHdr.datatype = kDT_INT64) or (lHdr.datatype = kDT_FLOAT64) or (lHdr.datatype = kDT_COMPLEX64)) then
        exit;
     if (lHdr.bitpix = 128) and ((lHdr.datatype = kDT_FLOAT128) or (lHdr.datatype = kDT_COMPLEX128)) then
        exit;
     if (lHdr.bitpix = 256) and (lHdr.datatype = kDT_COMPLEX256) then
        exit;
     if (lHdr.bitpix = 24) then
       lHdr.datatype := kDT_RGB;
     bitpix := lHdr.bitpix;
     lHdr.bitpix := -1;
     if (lHdr.datatype = kDT_BINARY) then
        lHdr.bitpix := 1;
     if (lHdr.datatype = kDT_UINT8) or (lHdr.datatype = kDT_INT8) then
        lHdr.bitpix := 8;
     if (lHdr.datatype = kDT_UINT16) or (lHdr.datatype = kDT_INT16) then
        lHdr.bitpix := 16;
     if (lHdr.datatype = kDT_RGB) then
        lHdr.bitpix := 24;
     if (lHdr.datatype = kDT_UINT32) or (lHdr.datatype = kDT_INT32) or (lHdr.datatype = kDT_FLOAT32) or (lHdr.datatype = kDT_RGBA32) then
        lHdr.bitpix := 32;
     if (lHdr.datatype = kDT_UINT64) or (lHdr.datatype = kDT_INT64) or (lHdr.datatype = kDT_FLOAT64) or (lHdr.datatype = kDT_COMPLEX64) then
        lHdr.bitpix := 64;
     if (lHdr.datatype = kDT_FLOAT128) or (lHdr.datatype = kDT_COMPLEX128) then
        lHdr.bitpix := 128;
     if (lHdr.datatype = kDT_COMPLEX256) then
        lHdr.bitpix := 256;
     if lHdr.bitpix > 0 then exit; //correct match inferred from either datatype or bitpix
     //unable to fix this - report this as an error
     lHdr.bitpix := bitpix;
     printf(format('Invalid NIfTI: Header bitpix (%d) does not match datatype (%d)', [lHdr.bitpix, lHdr.datatype])); // < this will not end well
end;

procedure NoMat( var lHdr: TNIfTIHdr );
//situation where no matrix is provided
//var
//   lR :TMat4;
begin
     if (lHdr.sform_code > kNIFTI_XFORM_UNKNOWN) or (lHdr.qform_code > kNIFTI_XFORM_UNKNOWN) then exit;
     if (lHdr.srow_x[0] <> 0) or (lHdr.srow_x[1] <> 0) or (lHdr.srow_x[2] <> 0) then exit;
     if lHdr.pixdim[1] = 0 then lHdr.pixdim[1] := 1;
     if lHdr.pixdim[2] = 0 then lHdr.pixdim[2] := 1;
     if lHdr.pixdim[3] = 0 then lHdr.pixdim[3] := 1;
     lHdr.srow_x[0] := lHdr.pixdim[1];
     lHdr.srow_x[1] := 0;
     lHdr.srow_x[2] := 0;
     lHdr.srow_x[3] := ((lHdr.dim[1]-1)*lHdr.pixdim[1]) * -0.5;
     lHdr.srow_y[0] := 0;
     lHdr.srow_y[1] := lHdr.pixdim[2];
     lHdr.srow_y[2] := 0;
     lHdr.srow_y[3] := ((lHdr.dim[2]-1)*lHdr.pixdim[2]) * -0.5;
     lHdr.srow_z[0] := 0;
     lHdr.srow_z[1] := 0;
     lHdr.srow_z[2] := lHdr.pixdim[3];
     lHdr.srow_z[3] := ((lHdr.dim[3]-1)*lHdr.pixdim[3]) * -0.5;
end;

procedure fixAnalyze(var h: TNIfTIHdr; isNativeEndian: boolean);
var
   //isBogus: boolean = false;
   h2: TNIfTIHdr;
   a: TAnalyzeHdrSection;
   //i : integer;
begin
	 if (h.magic = kNIFTI_MAGIC_SEPARATE_HDR) or (h.magic = kNIFTI_MAGIC_EMBEDDED_HDR) then exit;
     if (h.magic = kNIFTI2_MAGIC_SEPARATE_HDR) or (h.magic = kNIFTI2_MAGIC_EMBEDDED_HDR) then exit;
     if  (h.HdrSz <> 348) then exit;;
     printf('Warning: this does not appear to be a valid  NIfTI image. Perhaps legacy Analyze.');
     (*if (h.pixdim[1] = 0) or (h.pixdim[2] = 0) or (h.pixdim[3] = 0) then exit;
     for i := 0 to 3 do begin
         if specialSingle(h.srow_x[i]) then isBogus := true;
         if specialSingle(h.srow_y[i]) then isBogus := true;
         if specialSingle(h.srow_z[i]) then isBogus := true;
     end;
     if (not isBogus) then begin
     	isBogus := true;
        for i := 0 to 2 do begin
            if h.srow_x[i] <> 0 then isBogus := false;
            if h.srow_y[i] <> 0 then isBogus := false;
            if h.srow_z[i] <> 0 then isBogus := false;
        end;
     end;
     if not isBogus then exit;*)
     h2 := h;
     if not isNativeEndian then NIFTIhdr_SwapBytes(h2);
     move(h2,a,sizeof(a));
     if not isNativeEndian then begin
     	swap(a.originator[1]);
        swap(a.originator[2]);
        swap(a.originator[3]);

     end;
     if  (a.originator[1] < 0) or (a.originator[2] < 0) or (a.originator[3] < 0)  then  exit;
     if  (a.originator[1] > h.dim[1]) or (a.originator[2] > h.dim[2]) or (a.originator[3] > h.dim[3])  then  exit;
     //printf(format(' %d %d %d', [a.originator[1], a.originator[2], a.originator[3]]));
     h.srow_x[0] := h.pixdim[1];
     h.srow_x[1] := 0;
     h.srow_x[2] := 0;
     h.srow_x[3] := -(a.originator[1]-1) *h.pixdim[1];
     h.srow_y[0] := 0;
     h.srow_y[1] := h.pixdim[2];
     h.srow_y[2] := 0;
     h.srow_y[3] := -(a.originator[2]-1) *h.pixdim[2];
     h.srow_z[0] := 0;
     h.srow_z[1] := 0;
     h.srow_z[2] := h.pixdim[3];
     h.srow_z[3] := -(a.originator[3]-1) *h.pixdim[3];
     printf('Warning: unable to determine left from right side SPM-style for Analyze images');
end;

procedure CheckXForm(var lHdr: TNIfTIHdr);
begin
 if (lHdr.qform_code > kNIFTI_XFORM_OTHER_TEMPLATE) or (lHdr.qform_code < kNIFTI_XFORM_UNKNOWN) then begin
    {$IFDEF UNIX}
    printf('Unkown qform_code (update)');
    {$ENDIF}
    lHdr.qform_code := kNIFTI_XFORM_UNKNOWN;
 end;
 if (lHdr.sform_code > kNIFTI_XFORM_OTHER_TEMPLATE) or (lHdr.sform_code < kNIFTI_XFORM_UNKNOWN) then begin
    {$IFDEF UNIX}
    printf('Unkown sform_code (update)');
    {$ENDIF}
    lHdr.sform_code := kNIFTI_XFORM_UNKNOWN;
 end;
end;

function Quat2Mat( var lHdr: TNIfTIHdr ): boolean;
var lR :TMat4;
begin
  result := false;
  CheckXForm(lHdr);
  if (lHdr.sform_code <> kNIFTI_XFORM_UNKNOWN) or (lHdr.qform_code <= kNIFTI_XFORM_UNKNOWN) or (lHdr.qform_code > kNIFTI_XFORM_MNI_152) then
     exit;
  result := true;
  nifti_quatern_to_mat44(lR,lHdr.quatern_b,lHdr.quatern_c,lHdr.quatern_d,
  lHdr.qoffset_x,lHdr.qoffset_y,lHdr.qoffset_z,
  lHdr.pixdim[1],lHdr.pixdim[2],lHdr.pixdim[3],
  lHdr.pixdim[0]);
  lHdr.srow_x[0] := lR[0,0];
  lHdr.srow_x[1] := lR[0,1];
  lHdr.srow_x[2] := lR[0,2];
  lHdr.srow_x[3] := lR[0,3];
  lHdr.srow_y[0] := lR[1,0];
  lHdr.srow_y[1] := lR[1,1];
  lHdr.srow_y[2] := lR[1,2];
  lHdr.srow_y[3] := lR[1,3];
  lHdr.srow_z[0] := lR[2,0];
  lHdr.srow_z[1] := lR[2,1];
  lHdr.srow_z[2] := lR[2,2];
  lHdr.srow_z[3] := lR[2,3];
  lHdr.sform_code := kNIFTI_XFORM_SCANNER_ANAT;
end;

function Nifti2to1(h2 : TNIFTI2hdr): TNIFTIhdr; overload;
type
  tmagic = packed record
    case byte of
      0:(b1,b2,b3,b4 : ansichar); //word is 16 bit
      1:(l: longint);
  end;
var
  h1 : TNIFTIhdr;
  i: integer;
  magic: tmagic;
begin
  NII_Clear(h1);
  magic.b1 := h2.magic[1];
  magic.b2 := h2.magic[2];
  magic.b3 := h2.magic[3];
  magic.b4 := h2.magic[4];
  h1.magic := magic.l;
  h1.dim_info := h2.dim_info; //MRI slice order
  for i := 0 to 7 do
   h1.dim[i] := h2.dim[i];
  h1.intent_p1 := h2.intent_p1;
  h1.intent_p2 := h2.intent_p2;
  h1.intent_p3 := h2.intent_p3;
  h1.intent_code := h2.intent_code;
  if (h2.intent_code >= 3000) and (h2.intent_code <= 3012) then begin //https://www.nitrc.org/forum/attachment.php?attachid=342&group_id=454&forum_id=1955
     showmessage('NIfTI2 image has CIfTI intent code ('+inttostr(h2.intent_code)+'): open as an overlay with Surfice');
  end;
  h1.datatype := h2.datatype;
  h1.bitpix := h2.bitpix;
  h1.slice_start := h2.slice_start;
  for i := 0 to 7 do
   h1.pixdim[i] := h2.pixdim[i];
  h1.vox_offset := h2.vox_offset;
  h1.scl_slope := h2.scl_slope;
  h1.slice_end := h2.slice_end;
  h1.slice_code := h2.slice_code; //e.g. ascending
  h1.cal_min:= h2.cal_min;
  h1.cal_max:= h2.cal_max;
  h1.xyzt_units := h2.xyzt_units; //e.g. mm and sec
  h1.slice_duration := h2.slice_duration; //time for one slice
  h1.toffset := h2.toffset; //time axis to shift
  for i := 1 to 80 do
   h1.descrip[i] := h2.descrip[i];
  for i := 1 to 24 do
   h1.aux_file[i] := h2.aux_file[i];
  h1.qform_code := h2.qform_code;
  h1.sform_code := h2.sform_code;
  h1.quatern_b := h2.quatern_b;
  h1.quatern_c := h2.quatern_c;
  h1.quatern_d := h2.quatern_d;
  h1.qoffset_x := h2.qoffset_x;
  h1.qoffset_y := h2.qoffset_y;
  h1.qoffset_z := h2.qoffset_z;
  for i := 0 to 3 do begin
     h1.srow_x[i] := h2.srow_x[i];
     h1.srow_y[i] := h2.srow_y[i];
     h1.srow_z[i] := h2.srow_z[i];
  end;
  for i := 1 to 16 do
     h1.intent_name[i] := h2.intent_name[i];
  h1.HdrSz := 348;
  Quat2Mat(h1);
  result := h1;
end;

procedure NIFTI2hdr_SwapBytes (var lAHdr: TNIFTI2hdr); //Swap Byte order for the Analyze type
var
   i: integer;
begin
    with lAHdr do begin
         swap4(HdrSz);
         datatype := swap(datatype);
         bitpix := swap(bitpix);
         for i := 0 to 7 do begin
             swap8(dim[i]);
             xswap8r(pixdim[i]);
         end;
         Xswap8r(intent_p1);
         Xswap8r(intent_p2);
         Xswap8r(intent_p3);
         swap8(vox_offset);
         Xswap8r(scl_slope);
         Xswap8r(scl_inter);
         Xswap8r(cal_max);
         Xswap8r(cal_min);
         Xswap8r(slice_duration);
         Xswap8r(toffset);
         swap8(slice_start);
         swap8(slice_end);
         swap4(qform_code);
         swap4(sform_code);
         Xswap8r(quatern_b);
         Xswap8r(quatern_c);
         Xswap8r(quatern_d);
         Xswap8r(qoffset_x);
         Xswap8r(qoffset_y);
         Xswap8r(qoffset_z);
         for i := 0 to 3 do begin
             Xswap8r(srow_x[i]);
             Xswap8r(srow_y[i]);
             Xswap8r(srow_z[i]);

         end;
         swap4(slice_code);
         swap4(xyzt_units);
         swap4(intent_code);
         //showmessage(format('offset %d datatype %d bitpix %d %dx%dx%d', [vox_offset, datatype, bitpix, dim[1], dim[2], dim[3]]));
    end;

end;

function Nifti2to1(Stream:TFileStream; var isNativeEndian: boolean): TNIFTIhdr; overload;
var
  h2 : TNIFTI2hdr;
  h1 : TNIFTIhdr;
  lSwappedReportedSz: LongInt;
begin
  h1.HdrSz:= 0; //error
  isNativeEndian := true;
  Stream.Seek(0,soFromBeginning);
  Stream.ReadBuffer (h2, SizeOf (TNIFTI2hdr));
  lSwappedReportedSz := h2.HdrSz;
  swap4(lSwappedReportedSz);
  if (lSwappedReportedSz = SizeOf (TNIFTI2hdr)) then begin
    NIFTI2hdr_SwapBytes(h2);
    isNativeEndian := false;
    //exit(h1);
  end;
  if (h2.HdrSz <> SizeOf (TNIFTI2hdr)) then exit(h1);
  result := Nifti2to1(h2);
end;

{$IFDEF GZIP}
function Nifti2to1(Stream:TGZFileStream; var isNativeEndian: boolean): TNIFTIhdr; overload;
var
  h2 : TNIFTI2hdr;
  h1 : TNIFTIhdr;
  lSwappedReportedSz: LongInt;
begin
  h1.HdrSz:= 0; //error
  isNativeEndian := true;
  Stream.Seek(0,soFromBeginning);
  //{$PUSH}
  //{$WARN 5057 OFF}
  Stream.ReadBuffer (h2, SizeOf (TNIFTI2hdr));
  //{$WARN 5057 ON}
  lSwappedReportedSz := h2.HdrSz;
  swap4(lSwappedReportedSz);
  if (lSwappedReportedSz = SizeOf (TNIFTI2hdr)) then begin
    //printf('Not yet able to handle byte-swapped NIfTI2');
    NIFTI2hdr_SwapBytes(h2);
    isNativeEndian := false;
    //exit(h1);
  end;
  if (h2.HdrSz <> SizeOf (TNIFTI2hdr)) then exit(h1);
  result := Nifti2to1(h2);
end;

function GetCompressedFileInfo(const comprFile: TFileName; var size: int64; var crc32: dword; skip: int64 = 0): int64;
//read GZ footer https://www.forensicswiki.org/wiki/Gzip
type
TGzHdr = packed record
   Signature1,Signature2,
   Method, Flags: byte;
   ModTime: DWord;
   Extra, OS: byte;
 end;
var
  F : File Of byte;
  b: byte;
  i, xtra: word;
  cSz : int64; //uncompressed, compressed size
  uSz: dword;
  Hdr: TGzHdr;
begin
  result := -1;
  size := 0;
  crc32 := 0;
  if not fileexists(comprFile) then exit;
  result := skip;
  FileMode := fmOpenRead;
  Assign (F, comprFile);
  Reset (F);
  cSz := FileSize(F);
  if cSz < (18+skip) then begin
    Close (F);
    exit;
  end;
  seek(F,skip);
  blockread(F, Hdr, SizeOf(Hdr) );
  //https://en.wikipedia.org/wiki/List_of_file_signatures
  if Hdr.Signature1 = $78 then begin
    if (Hdr.Signature2 =  $01) or (Hdr.Signature2 =  $5E) or (Hdr.Signature2 =  $9C) or (Hdr.Signature2 =  $DA) or (Hdr.Signature2 =  $20) or (Hdr.Signature2 =  $7D) or (Hdr.Signature2 =  $BB) or(Hdr.Signature2 =  $F9) then
    	exit(2);

  end;
  if (Hdr.Signature1 <> $1F) or (Hdr.Signature2 <> $8B) then begin //hex: 1F 8B
    Close (F);
    exit;
  end;
  //http://www.zlib.org/rfc-gzip.html
  if (Hdr.method <> 8) then
     printf('Expected GZ method 8 (deflate) not '+inttostr(Hdr.method));
  if ((Hdr.Flags and $04) = $04) then begin //FEXTRA
  	blockread(F, xtra, SizeOf(xtra));
  	{$IFDEF ENDIAN_BIG}
  	xtra := Swap(xtra);
  	{$ENDIF}
  	if xtra > 1 then
  		for i := 1 to xtra do
  			blockread(F, b, SizeOf(b));
  end;
  if ((Hdr.Flags and $08) = $08) then begin //FNAME
  	b := 1;
  	while (b <> 0) and (not EOF(F)) do
  		blockread(F, b, SizeOf(b));
  end;
  if ((Hdr.Flags and $10) = $10) then begin //FCOMMENT
  	b := 1;
  	while (b <> 0) and (not EOF(F)) do
  		blockread(F, b, SizeOf(b));
  end;
  if ((Hdr.Flags and $02) = $02) then begin //FHCRC
  	blockread(F, xtra, SizeOf(xtra));
  end;
  result := Filepos(F);
  Seek(F,cSz-8);
  blockread(f, crc32, SizeOf(crc32) );
  blockread(f, uSz, SizeOf(uSz) );
  {$IFDEF ENDIAN_BIG}
  crc32 = Swap(crc32);
  uSz = Swap(uSz);
  {$ENDIF}
  size := uSz; //note this is the MODULUS of the file size, beware for files > 2Gb
  Close (F);
end;

const
     kLoadFewVolumesBytes =  16777216 * 4;
     kLoadFewVolumesN = 3;
     //kLoadFewVolumesBytes =  168 * 144 * 111 * 3 *   4; //HCP DTI sequence V1 image

function HdrVolumes(hdr: TNIfTIhdr): integer;
var
  i: integer;
begin
     result := 1;
     for i := 4 to 7 do
         if hdr.dim[i] > 1 then
            result := result * hdr.dim[i];
end;

procedure TNIfTI.DetectV1();
begin
  if (fVolumesLoaded <> 3) or (fHdr.intent_code = kNIFTI_INTENT_RGB_VECTOR) or (fHdr.datatype <> kDT_FLOAT32) then
     exit;
  if AnsiContainsText(fShortName,'_V1') then
  	 fHdr.intent_code := kNIFTI_INTENT_RGB_VECTOR;
end;

{$IFDEF FASTGZ}
function Nifti2to1(Stream:TMemoryStream; var isNativeEndian: boolean): TNIFTIhdr; overload;
var
  h2 : TNIFTI2hdr;
  h1 : TNIFTIhdr;
  lSwappedReportedSz: LongInt;
begin
  h1.HdrSz:= 0; //error
  isNativeEndian := true;
  Stream.Seek(0,soFromBeginning);
  Stream.ReadBuffer (h2, SizeOf (TNIFTI2hdr));
  lSwappedReportedSz := h2.HdrSz;
  swap4(lSwappedReportedSz);
  if (lSwappedReportedSz = SizeOf (TNIFTI2hdr)) then begin
    //printf('Not yet able to handle byte-swapped NIfTI2');
    NIFTI2hdr_SwapBytes(h2);
    isNativeEndian := false;
    //exit(h1);
  end;
  if (h2.HdrSz <> SizeOf (TNIFTI2hdr)) then exit(h1);
  result := Nifti2to1(h2);
end;

function ExtractGzNoCrcX(fnm: string; var mStream : TMemoryStream; skip: int64 = 0; expected: int64 = 0): boolean;
//a bit faster: do not compute CRC check
{$DEFINE ROBUST} //handle ImageJ/Fiji MHA compression without GZ header : see cpts.mha
var
   {$IFDEF ROBUST}
   fStream : TFileStream;
   inStream : TMemoryStream;
   {$ENDIF}
   ret, size, usize: int64;
   crc32: dword;
   src : array of byte;
   f: file of byte;
begin
  result := false;
  ret := GetCompressedFileInfo(fnm, usize, crc32, skip);
  if ret < 0 then exit;
  {$IFDEF ROBUST}
  if (ret = 2)  then begin //zlib : no file size...
      src := nil;
      fStream := TFileStream.Create(fnm, fmOpenRead);
      fStream.seek(skip, soFromBeginning);
      inStream := TMemoryStream.Create();
      inStream.CopyFrom(fStream, fStream.Size - skip);
      result := unzipStream(inStream, mStream);
      fStream.Free;
      inStream.Free;
      if (not result) and (expected >= mStream.size) then begin
         printf('unzipStream error but sufficient bytes extracted (perhaps GZ without length in footer)');
         result := true;
      end;
      exit;
  end;
  {$ENDIF}//ROBUST
  AssignFile(f, fnm);
  FileMode := fmOpenRead;
  Reset(f,1);
  size := FileSize(f);
  if size < 1 then begin
     CloseFile(f);
     exit;
  end;
  if (size+ret+10) > usize then begin
  	printf('Concatenated zstream? https://bugs.freepascal.org/view.php?id=36822');
        CloseFile(f);
        exit;
  end;
  setlength(src, size);
  blockread(f, src[0], size );
  CloseFile(f);
  result := false;
  if (ret = 2)  then begin //zlib : no file size...
      UnCompressStream(@src[0], size, mStream, nil, true); //SynZip
      result := true;
  end else if (ret > skip) then begin
     if (usize < expected) and (expected > 0) then begin
        printf(format('GZ footer reports %d bytes but expected %d (corrupt GZ)\n', [usize, expected]));
        usize := expected;
     end;
     mStream.setSize(usize);
     mStream.position := 0;
     ret := UnCompressMem(@src[ret], mStream.memory, size-ret, usize); //SynZip
     result := (ret = usize);
  end else if (skip < size) then begin //assume uncompressed
      mStream.Write(src[skip],size-skip);
      result := true;
  end;
  src := nil;
end;

{$IFNDEF LIBDEFLATE}
function UnCompressMemX(src, dst: pointer; srcLen, dstLen: integer;
  out cmpBytes: integer; ZlibFormat: Boolean = false) : integer;
var strm: TZStream;
    Bits: integer;
begin
  StreamInit(strm);
  strm.next_in := src;
  strm.avail_in := srcLen;
  strm.next_out := dst;
  strm.avail_out := dstLen;
  if ZlibFormat then
    Bits := MAX_WBITS else
    Bits := -MAX_WBITS; // -MAX_WBITS -> no zLib header => .zip compatible !
  if inflateInit2_(strm, Bits, ZLIB_VERSION, sizeof(strm))>=0 then
  try
    Check(inflate(strm, Z_FINISH),[Z_OK,Z_STREAM_END],'UnCompressMem');
  finally
    inflateEnd(strm);
  end;
  cmpBytes := strm.next_in - src;
  result := strm.total_out;
end;
{$ENDIF}

function ExtractGzNoCrc(fnm: string; var mStream : TMemoryStream; skip: int64 = 0; expected: int64 = 0): boolean;
//a bit faster: do not compute CRC check
{$DEFINE ROBUST} //handle ImageJ/Fiji MHA compression without GZ header : see cpts.mha
var
   {$IFDEF ROBUST}
   fStream : TFileStream;
   inStream : TMemoryStream;
   {$ENDIF}
   cmpBytes: integer;
   outBytes, newBytes, hdrBytes, size, usize, usizePart: int64;
   crc32: dword;
   src : array of byte;
   f: file of byte;
begin
  result := false;
  hdrBytes := GetCompressedFileInfo(fnm, usize, crc32, skip);
  if hdrBytes < 0 then exit;
  {$IFDEF ROBUST}
  if (hdrBytes = 2)  then begin //zlib : no file size...
      src := nil;
      fStream := TFileStream.Create(fnm, fmOpenRead);
      fStream.seek(skip, soFromBeginning);
      inStream := TMemoryStream.Create();
      inStream.CopyFrom(fStream, fStream.Size - skip);
      result := unzipStream(inStream, mStream);
      fStream.Free;
      inStream.Free;
      if (not result) and (expected >= mStream.size) then begin
         printf('unzipStream error but sufficient bytes extracted (perhaps GZ without length in footer)');
         result := true;
      end;
      exit;
  end;
  {$ENDIF}//ROBUST
  AssignFile(f, fnm);
  FileMode := fmOpenRead;
  Reset(f,1);
  size := FileSize(f);
  if size < 1 then begin
     CloseFile(f);
     exit;
  end;
  //printf(format('A %d %d',[hdrBytes, skip]));
  if (expected = 0) and ((size+hdrBytes+10) > usize) then begin
  	printf('Concatenated zstream?');
        CloseFile(f);
        exit;
  end;
  setlength(src, size);
  blockread(f, src[0], size );
  CloseFile(f);
  result := false;
  if (hdrBytes = 2)  then begin //zlib : no file size...
      UnCompressStream(@src[0], size, mStream, nil, true); //SynZip
      result := true;
  end else if (hdrBytes > skip) then begin
    if (usize < expected) and (expected > 0) then begin
    	printf(format('GZ footer reports %d bytes but expected %d (corrupt or concatenated GZ)', [usize, expected]));
    	usize := expected;
    end;
    mStream.setSize(usize);
    outBytes := 0;
    while outBytes < usize do begin //handle concatenated GZip files
      mStream.position := outBytes;
      newBytes := UnCompressMemX(@src[hdrBytes], mStream.memory, size-hdrBytes, usize, cmpBytes); //SynZip
      outBytes += newBytes;
      //showDebug(format('%s: read %d total %d hdrOffset %d', [fnm, newBytes, outBytes, hdrBytes]));
      if (outBytes >= usize) then break;
      hdrBytes := GetCompressedFileInfo(fnm, usizePart, crc32, hdrBytes + cmpBytes + 8);
    end;
    result := (outBytes = usize);
  end else if (skip < size) then begin //assume uncompressed
      mStream.Write(src[skip],size-skip);
      result := true;
  end;
  src := nil;
end;

{$IFDEF LIBDEFLATE}
function ExtractGz(fnm: string; var mStream : TMemoryStream): boolean;
begin
result := ExtractGzNoCrc(fnm, mStream);
end;

{$ELSE}
function ExtractGz(fnm: string; var mStream : TMemoryStream): boolean;
var
  gz: TGZRead;  //SynZip
  F : File Of byte;
  src : array of byte;
  cSz : int64; //uncompressed, compressed size
begin
  if not fileexists(fnm) then exit(false);
  FileMode := fmOpenRead;
  Assign (F, fnm);
  Reset (F);
  cSz := FileSize(F);
  setlength(src, cSz);
  blockread(f, src[0], cSz );
  CloseFile(f);
  result := gz.Init(@src[0], cSz);
  if not result then begin
    src := nil;
    exit;
  end;
  gz.ToStream(mStream, cSz);
  src := nil;
  gz.ZStreamDone;
end;
{$ENDIF}

function TNIfTI.LoadFastGZ(FileName : AnsiString; out isNativeEndian: boolean): boolean;
//FSL compressed nii.gz file
var
  volBytes: int64;
  lSwappedReportedSz : LongInt;
  Stream : TMemoryStream;
begin
 isNativeEndian := true;
 result := false;
 Stream := TMemoryStream.Create;
 {$DEFINE FASTERGZ} //IGNORE CRC
 {$IFDEF FASTERGZ}
 if not ExtractGzNoCrc(FileName,  Stream) then begin
  {$ELSE}
  if not ExtractGz(FileName,  Stream) then begin
  {$ENDIF}

    printf('Unable to extract image '+Filename);
    Stream.Free;
    exit;
   end;
 Stream.Position:=0;
 Try
  {$warn 5058 off}Stream.ReadBuffer (fHdr, SizeOf (TNIFTIHdr));{$warn 5058 on}
  //Stream.ReadBuffer(fHdr, SizeOf (TNIFTIHdr));
  //Move(Stream.memory, fHdr, Sizeof(fHdr));
  lSwappedReportedSz := fHdr.HdrSz;
  swap4(lSwappedReportedSz);
  //showmessage(format('%d @ %d = %d %d %d',[Stream.Size, fHdr.HdrSz,   fHdr.dim[1], fHdr.dim[2], fHdr.dim[3] ]));
  if (lSwappedReportedSz = SizeOf (TNIFTIHdr)) then begin
    NIFTIhdr_SwapBytes(fHdr);
     isNativeEndian := false;
  end;
  if fHdr.HdrSz <> SizeOf (TNIFTIHdr) then begin
     fHdr := Nifti2to1(Stream, isNativeEndian);
     if fHdr.HdrSz <> SizeOf (TNIFTIHdr) then begin
       printf('Unable to read image '+Filename);
       exit;
     end;
  end;
  fixHdr(fHdr);
  if (fHdr.bitpix <> 8) and (fHdr.bitpix <> 16) and (fHdr.bitpix <> 24) and (fHdr.bitpix <> 32) and (fHdr.bitpix <> 64) and (fHdr.bitpix <> 128) then begin
   printf('Unable to load '+Filename+' - this software can only read 8,16,24,32,64,128-bit NIfTI files. (bitpix: '+inttostr(fHdr.bitpix)+' datatype: '+inttostr(fHdr.datatype)+ ')');
   exit;
  end;
  Quat2Mat(fHdr);
  //read the image data
  volBytes := fHdr.Dim[1]*fHdr.Dim[2]*fHdr.Dim[3]* (fHdr.bitpix div 8);
  if  (fVolumeDisplayed > 0) and (fVolumeDisplayed < HdrVolumes(fHdr)) and (fIsOverlay) then begin
      Stream.Seek(round(fHdr.vox_offset)+(fVolumeDisplayed*volBytes),soFromBeginning);
  end else begin
      fVolumeDisplayed := 0;
      Stream.Seek(round(fHdr.vox_offset),soFromBeginning);
  end;
  fVolumesLoaded := max(HdrVolumes(fHdr),1);
  fVolumesTotal :=  fVolumesLoaded;
  DetectV1();
  if (fVolumesLoaded = 3) and ((fHdr.intent_code = kNIFTI_INTENT_RGB_VECTOR) or (fHdr.intent_code = kNIFTI_INTENT_VECTOR)) and (fHdr.datatype = kDT_FLOAT32) then
     volBytes := volBytes * fVolumesLoaded
  else if (HdrVolumes(fHdr) > 1) and (not fIsOverlay) then begin
     if LoadFewVolumes then
       //fVolumesLoaded :=  trunc((16777216.0 * 4) /volBytes)
       fVolumesLoaded :=  max(trunc((kLoadFewVolumesBytes) /volBytes), kLoadFewVolumesN)
     else
         fVolumesLoaded :=  trunc(2147483647.0 /volBytes);
     if fVolumesLoaded < 1 then fVolumesLoaded := 1;
     if fVolumesLoaded > HdrVolumes(fHdr) then fVolumesLoaded := HdrVolumes(fHdr);
     volBytes := volBytes * fVolumesLoaded;
  end;
  if (Stream.Position+ volBytes) > Stream.Size then begin
       showmessage(format('Fatal error decoding GZ expected %d bytes found %d', [Stream.Position+ volBytes, Stream.Size]));
     exit
  end;
  SetLength (fRawVolBytes, volBytes);
  Stream.ReadBuffer (fRawVolBytes[0], volBytes);
  if not isNativeEndian then
   SwapImg(fRawVolBytes, fHdr.bitpix);
 Finally
  Stream.Free;
 End; { Try }
 result := true;
end;
{$ENDIF} //FastGZ()

(*function SaveGz(niftiFileName: string; var hdr: TNIFTIhdr; var img8: TUInt8s): boolean;
var
   Stream : TGZFileStream;
   pad: uint32;
   //hdr : TNIFTIhdr;
begin
 result := false;
 if fileexists(niftiFileName) then begin
    Showmessage('Unable to overwrite existing file "'+niftiFileName+'".');
    exit;
 end;
 //hdr := fHdr;
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
end; *)
{$ENDIF} //GZIP

function TNIfTI.SaveFormatBasedOnExt(fnm: string): boolean; overload; //filename determines format.nii, .tif, .bvox, .nrrd, .nhdr etc...
begin
     result := SaveVolumeFormatBasedOnExt(fnm, fHdr, fRawVolBytes);
end;

(*function TNIfTI.SaveNii(niftiFileName: string): boolean;
begin
     result := SaveNii(niftiFileName, fHdr, fRawVolBytes);
end;*)

(*var
   Stream : TFileStream;
   pad: uint32;
   hdr : TNIFTIhdr;
   lExt: string;
begin
 result := false;
 if fileexists(niftiFileName) then begin
    Showmessage('Unable to overwrite existing file "'+niftiFileName+'".');
    exit;
 end;
 lExt := uppercase(extractfileext(niftiFileName));
 if (lExt = '.GZ') or (lExt = '.VOI') then begin
    if lExt <> '.VOI' then
       niftiFileName := changefileextX(niftiFileName,'.nii.gz');
    {$IFDEF GZIP}
    result := SaveGz(niftiFileName);
    {$ELSE}
    showmessage('Please recompile for GZ support.');
    {$ENDIF}
    exit;
 end;
 if (lExt <> '.NII') then niftiFileName := niftiFileName + '.nii';
 hdr := fHdr;
 //showmessage(format('%g %g %g', [fHdr.qoffset_x, fHdr.qoffset_y, fHdr.qoffset_z]));
 //hdr.qform_code:= kNIFTI_XFORM_SCANNER_ANAT;
 //exit;
 hdr.HdrSz:= SizeOf (TNIFTIHdr);
 hdr.vox_offset:= 352;
 Stream:= TFileStream.Create(niftiFileName, fmCreate);
 Try
    Stream.WriteBuffer(hdr, SizeOf(TNIFTIHdr));
    pad := 0;
    Stream.WriteBuffer(pad, SizeOf(pad));
    Stream.WriteBuffer(fRawVolBytes[0], length(fRawVolBytes));

 Finally
  Stream.Free;
 End;
 result := true;
end; *)

function isGz(hdrSz: longint): boolean;
var
   b0, b1, b2: byte;
begin
 {$IFDEF ENDIAN_BIG}
 b0 := (hdrSz shr 24) and 255;
 b1 := (hdrSz shr 16) and 255;
 b2 := (hdrSz shr 8) and 255;
 {$ELSE}
 b0 := (hdrSz shr 0) and 255;
 b1 := (hdrSz shr 8) and 255;
 b2 := (hdrSz shr 16) and 255;

 {$ENDIF}
 //https://www.filesignatures.net/index.php?page=search&search=GZ&mode=EXT
 //showmessage(format('%2x %2x %2x', [b0,b1,b2]));
 result := (b0 = $1F) and (b1 = $8B) and (b2 = $08);
end;

function TNIfTI.LoadRaw(FileName : AnsiString; out isNativeEndian: boolean): boolean;// Load 3D data                                 }
//Uncompressed .nii or .hdr/.img pair
const
     kChunkBytes = 1073741824; //2^30;
var
   lSwappedReportedSz: LongInt;
   imgName: string;
   FSz, volBytes: int64;
   Stream : TFileStream;
   copied: int64;
begin
 isNativeEndian := true;
 FSz := FSize(FileName);
 result := false;
 if (FSz < SizeOf (TNIFTIHdr)) then exit;
 Stream := TFileStream.Create (FileName, fmOpenRead or fmShareDenyWrite);
 Try
  try
     Stream.ReadBuffer (fHdr, SizeOf (TNIFTIHdr));
  except
    Stream.Free;
    showmessage('Fatal error (check file size and permissions): '+ FileName);
    exit;
  end;
  lSwappedReportedSz := fHdr.HdrSz;
  swap4(lSwappedReportedSz);
  if (lSwappedReportedSz = SizeOf (TNIFTIHdr)) then begin
     NIFTIhdr_SwapBytes(fHdr);
     isNativeEndian := false;
  end;
  {$IFDEF GZIP}
  if (fHdr.HdrSz <> SizeOf (TNIFTIHdr))  and (isGz(fHdr.HdrSz)) then begin
     printf('Wrong file extension: Should be .nii.gz: '+Filename);
     result := LoadGz(FileName, isNativeEndian);
     exit;
  end;
  {$ENDIF}
  fixAnalyze(fHdr, isNativeEndian);
  if fHdr.HdrSz <> SizeOf (TNIFTIHdr) then begin
    fHdr := Nifti2to1(Stream, isNativeEndian);
    if fHdr.HdrSz <> SizeOf (TNIFTIHdr) then begin
       printf('Unable to read image (NIfTI Header Size incorrect) '+Filename);
       //Stream.Free; //Finally ALWAYS executed!
       exit;
    end;
  end;
  fixHdr(fHdr);
  if (fHdr.bitpix <> 8) and (fHdr.bitpix <> 16) and (fHdr.bitpix <> 24) and (fHdr.bitpix <> 32) and (fHdr.bitpix <> 64)  and (fHdr.bitpix <> 128) then begin
   printf('Unable to load '+Filename+' - this software can only read 8,16,24,32,64,128-bit NIfTI files. (bitpix: '+inttostr(fHdr.bitpix)+' datatype: '+inttostr(fHdr.datatype)+ ')');
   exit;
  end;
  Quat2Mat(fHdr);
  //read the image data
  if extractfileext(Filename) = '.hdr' then begin
    imgName := changefileext(FileName,'.img');
    if not FileExists(imgName) then begin
      showmessage('Unable to NIfTI image named "'+imgName+'"');
      exit;
    end;
    FSz := FSize(imgName);
    Stream.Free;
    Stream := TFileStream.Create (imgName, fmOpenRead or fmShareDenyWrite);
  end;
  if (round(fHdr.vox_offset)) > FSz then begin
       Showmessage('File smaller than expected (perhaps header with no image).');
       exit;
  end;
  volBytes := fHdr.Dim[1]*fHdr.Dim[2]*fHdr.Dim[3]* (fHdr.bitpix div 8);
  if  (fVolumeDisplayed > 0) and (fVolumeDisplayed < HdrVolumes(fHdr)) and (fIsOverlay) then begin
      Stream.Seek(round(fHdr.vox_offset)+(fVolumeDisplayed*volBytes),soFromBeginning);
  end else begin
      fVolumeDisplayed := 0;
      Stream.Seek(round(fHdr.vox_offset),soFromBeginning);
  end;
  fVolumesLoaded := max(HdrVolumes(fHdr),1);
  fVolumesTotal :=  fVolumesLoaded;
  if (fVolumesLoaded = 3) and ((fHdr.intent_code = kNIFTI_INTENT_RGB_VECTOR) or (fHdr.intent_code = kNIFTI_INTENT_VECTOR)) and (fHdr.datatype = kDT_FLOAT32) then
     volBytes := volBytes * fVolumesLoaded
  else if (HdrVolumes(fHdr) > 1) and (not fIsOverlay) then begin
     if LoadFewVolumes then
       fVolumesLoaded :=  max(trunc((kLoadFewVolumesBytes) /volBytes), kLoadFewVolumesN)
     else
         fVolumesLoaded :=  trunc(2147483647.0 /volBytes);
     if fVolumesLoaded < 1 then fVolumesLoaded := 1;
     if fVolumesLoaded > HdrVolumes(fHdr) then fVolumesLoaded := HdrVolumes(fHdr);
     volBytes := volBytes * fVolumesLoaded;
     //vol := SimpleGetInt(extractfilename(Filename)+' select volume',1,1,fHdr.dim[4]);
     //Stream.Seek((vol-1)*volBytes,soFromCurrent);
  end;
  //showmessage(format('%d %d', [HdrVolumes(fHdr), fVolumesLoaded]));

  if (volBytes + Stream.Position) > FSz then begin
       Showmessage(format('File smaller than described in header (expected %d, found %d)', [volBytes + Stream.Position, FSz]));
       exit;
  end;
  SetLength (fRawVolBytes, volBytes);
  if volBytes < 2147483647 then
     Stream.ReadBuffer (fRawVolBytes[0], volBytes)
  else begin
     copied := 0;
     while copied < volBytes do begin
           if (copied+kChunkBytes) <  volBytes then
              Stream.ReadBuffer (fRawVolBytes[copied], kChunkBytes)
           else
               Stream.ReadBuffer (fRawVolBytes[copied], volBytes-copied);
           copied := copied + kChunkBytes;
           //printf(format('> copied %d', [copied]));
     end;
  end;
  if not isNativeEndian then
   SwapImg(fRawVolBytes, fHdr.bitpix);
 Finally
  Stream.Free;
 End;
 result := true;
end;


procedure TNIfTI.GPULoadDone();
begin
 {$IFNDEF DYNRGBA}
 setlengthP(fVolRGBA,0);
 {$ENDIF}
 fVolRGBA := nil; //close CPU buffer afterit has been copied to GPU
end;

(*function blurS(img: TFloat32s; nx, ny: integer; xmm, Sigmamm: single): boolean;
var
	sum, wt, expd, sigma: single;
	i, j, x, y, cutoffvox, rowStart: integer;
	tmp, k, kWeight: array of single;
	kStart, kEnd: array of integer;
begin
	//make kernels
	if ((xmm = 0) or (nx < 2) or (ny < 1) or (Sigmamm <= 0.0)) then
		exit(false);
	sigma := (Sigmamm / xmm); //mm to vox
	cutoffvox := ceil(4 * sigma); //filter width to 6 sigma: faster but lower precision AFNI_BLUR_FIRFAC = 2.5
	cutoffvox := max(cutoffvox, 1);
	setlength(k, cutoffvox + 1);
	expd := 2 * sigma * sigma;
	for i := 0 to cutoffvox do
		k[i] := exp(-1.0 * (i * i) / expd);
	//calculate start, end for each voxel in
	setlength(kStart, nx); //-cutoff except left left columns, e.g. 0, -1, -2... cutoffvox
	setlength(kEnd, nx); //+cutoff except right columns
	setlength(kWeight, nx); //ensure sum of kernel = 1.0
	for i := 0 to (nx - 1) do begin
		kStart[i] := max(-cutoffvox, -i); //do not read below 0
		kEnd[i] := min(cutoffvox, nx - i - 1); //do not read beyond final columnn
		if ((i > 0) and (kStart[i] = (kStart[i - 1])) and (kEnd[i] = (kEnd[i - 1]))) then begin //reuse weight
			kWeight[i] := kWeight[i - 1];
			continue;
		end;
		wt := 0.0;
		for j := kStart[i] to kEnd[i] do
			wt += k[abs(j)];
		kWeight[i] := 1 / wt;
	end;
	//apply kernel to each row
	setlength(tmp, nx); //input values prior to blur
	rowStart := 0;
	for y := 0 to (ny - 1) do begin
		//printf("-+ %d:%d\n", y, ny);
		for x := 0 to (nx - 1) do
			tmp[x] := img[x+rowStart];
		for x := 0 to (nx - 1) do begin
			sum := 0;
			for i := kStart[x] to kEnd[x] do
				sum += tmp[x + i] * k[abs(i)];
			img[x + rowStart] := sum * kWeight[x];
		end;
		rowStart += nx;
	end; //blurX
	tmp := nil;
	k := nil;
	kStart := nil;
	kEnd := nil;
	kWeight := nil;
    exit(true);
end; //blurS()*)

procedure SmoothFloat(smoothImg: TFloat32s; X,Y,Z: int64);
var
   i, xydim, vx: int64;
   svol32b: TFloat32s;
begin
 if (x < 3) or (y < 3) or (z < 3) then exit;
 xydim := x * y;
 vx := x * y * z;
 //zz setlength(svol32b, vx);
 svol32b := copy(smoothImg,0, vx);
 for i := 1 to (vx-2) do
     smoothImg[i] := svol32b[i-1] + (svol32b[i] * 2) + svol32b[i+1];
 for i := x to (vx-x-1) do  //output *4 input (10bit->12bit)
     svol32b[i] := smoothImg[i-x] + (smoothImg[i] * 2) + smoothImg[i+x];
 for i := xydim to (vx-xydim-1) do  // *4 input (12bit->14bit) : 6 for 8 bit output
     smoothImg[i] := (svol32b[i-xydim] + (svol32b[i] * 2) + svol32b[i+xydim]) * 1/64;
 svol32b := nil;
end; //SmoothFloat()

procedure TNIfTI.Smooth(Mask: TUInt8s = nil);
//blur image
var
   skip, i, vx: int64;
   vol32, svol32 : TFloat32s;
   vol16: TInt16s;
   vol8: TUInt8s;
   x,y,z: int64;
   xMask, yMask, zMask: boolean;
begin
 if fHdr.datatype = kDT_RGB then exit;
 if (fHdr.dim[1] < 3) or (fHdr.dim[2] < 3) or (fHdr.dim[3] < 3) then exit;
 if ((fMax-fMin) <= 0) then exit;

 skip := skipVox();
 vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
 //vol8 := @fRawVolBytes[lBPP*skipVox()];
 vol8 := fRawVolBytes;
 vol16 := TInt16s(vol8);
 vol32 := TFloat32s(vol8);
 //smooth
 setlength(svol32,vx);
 if fHdr.datatype = kDT_UINT8 then
   for i := 0 to (vx-1) do
       svol32[i] := vol8[i+skip]
 else if fHdr.datatype = kDT_INT16 then
     for i := 0 to (vx-1) do
         svol32[i] := vol16[i+skip]
 else if fHdr.datatype = kDT_FLOAT then
    svol32 := Copy(vol32, skip, vx+skip);
 smoothFloat(svol32, fHdr.dim[1], fHdr.dim[2], fHdr.dim[3]);
   i := 0;
   for z := 1 to fHdr.dim[3] do begin
       zMask := (z = 1) or (z = fHdr.dim[3]);
       for y := 1 to fHdr.dim[2] do begin
           yMask := (y = 1) or (y = fHdr.dim[2]);
           for x := 1 to fHdr.dim[1] do begin
               xMask := (x = 1) or (x = fHdr.dim[1]);
               if (Mask <> nil) and (Mask[i] <> 0) then
                 xMask := true;
               if (not zMask) and (not yMask) and (not xMask) then begin
                  if fHdr.datatype = kDT_UINT8 then
                     vol8[i+skip] := round(svol32[i])
                  else if fHdr.datatype = kDT_INT16 then
                       vol16[i+skip] := round(svol32[i])
                  else if fHdr.datatype = kDT_FLOAT then
                    vol32[i+skip] := svol32[i];
               end;
               i := i +1;
           end; //x

       end; //y
   end; //z
   SetDisplayMinMax(true);
 svol32 := nil;
end; //smooth()

procedure TNIfTI.Sharpen();
//unsharp mask: emphasize edges by comparing original image to a blurred image
var
   inSkip, i, vx, xydim: int64;
   in32, vol32, svol32 : TFloat32s;
   in16: TInt16s;
   in8: TUInt8s;
   v, mn, mx: single;
begin
 if fHdr.datatype = kDT_RGB then exit;
 if (fHdr.dim[1] < 3) or (fHdr.dim[2] < 3) or (fHdr.dim[3] < 3) then exit;
 if ((fMax-fMin) <= 0) then exit;
 vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
 xydim := fHdr.dim[1]*fHdr.dim[2];
 inSkip  := skipVox();
 in8 := fRawVolBytes;
 in16 := TInt16s(fRawVolBytes);
 in32 := TFloat32s(fRawVolBytes);
 //smooth
 setlength(svol32,vx);
 if fHdr.datatype = kDT_UINT8 then begin
   for i := 0 to (vx-1) do
       svol32[i] := in8[inSkip+i];
 end else if fHdr.datatype = kDT_INT16 then begin
     for i := 0 to (vx-1) do
         svol32[i] := in16[inSkip+i];
 end else if fHdr.datatype = kDT_FLOAT then begin
    svol32 := Copy(in32, 0, vx);
 end;
 //zz setlength(vol32,vx);
 vol32 := Copy(svol32, 0, vx);
 smoothFloat(svol32, fHdr.dim[1], fHdr.dim[2], fHdr.dim[3]);
 //find range
 mx := vol32[0];
 mn := mx;
 for i := 0 to (vx-1) do begin
     if vol32[i] > mx then mx := vol32[i];
     if vol32[i] < mn then mn := vol32[i];
 end;
 //unsharp
 for i := 0 to (vx-1) do begin
     v := 2 * vol32[i] - svol32[i];
     if (v >= mn) and (v <= mx) then
        vol32[inSkip+i] := v;
 end;
 if fHdr.datatype = kDT_UINT8 then begin
   for i := inSkip+xydim to (inSkip+vx-1-xydim) do
          in8[inSkip+i] := round(vol32[i]);
 end else if fHdr.datatype = kDT_INT16 then begin
   for i := inSkip+xydim to (inSkip+vx-1-xydim) do
       in16[i] := round(vol32[i]);
 end else if fHdr.datatype = kDT_FLOAT then begin
   for i := inSkip+xydim to (inSkip+vx-1-xydim) do
       in32[i] := vol32[i];
 end;
 svol32 := nil;
 vol32 := nil;
 SetDisplayMinMax(true);
end; //sharpen()

procedure TNIfTI.MakeHistogram(Histo: TUInt32s; mn, mx: single; isClampExtremeValues : boolean = true; isIgnoreZeros: boolean = false);
var
   i, j, vx, maxBin: integer;
   vol32 : TFloat32s;
   vol16: TInt16s;
   vol8: TUInt8s;
   v,scaleBin: single;
begin
    vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
    maxBin := length(Histo) - 1;
    if (maxBin < 1) or (vx < 1) or (abs(mx-mn) <= 0) then exit;
	for i := 0 to maxBin do
    	Histo[i] := 0;
    vol8 := fRawVolBytes;
    vol16 := TInt16s(vol8);
    vol32 := TFloat32s(vol8);
    scaleBin := (maxBin) / abs(mx-mn);
    j := 1; //for RGBA read Green
    v := 0;
    for i := 0 to (vx - 1) do begin
        if fHdr.datatype = kDT_UINT8 then
           v := vol8[i]
        else if fHdr.datatype = kDT_INT16 then
             v := vol16[i]
        else if fHdr.datatype = kDT_FLOAT then
             v := vol32[i]
        else if fHdr.datatype = kDT_RGB then begin
             v := max(max(vol8[j], vol8[j+1]),vol8[j+2]);
             j := j + 3;
        end;
        v := (v * fHdr.scl_slope) + fHdr.scl_inter;
        if (v < mn) then begin
          if (not isClampExtremeValues) then continue;
          v := 0;
        end;
        if (v > mx) then begin
          if (not isClampExtremeValues) then continue;
          v := maxBin;
        end;
        if (v = 0) and (isIgnoreZeros) then
        	continue;
        v := (v - mn) * scaleBin;
        inc(Histo[round(v)]);
    end;
end;

procedure TNIfTI.initHistogram(Histo: TUInt32s = nil);
{$DEFINE HISTO_SQRT}
{$DEFINE DESPIKE}
const
	kSpikeFrac = 1.25; //only allow 25% increase relative to neighboring bins
var
   i,j,vx, mx, prevBin, currBin, nextBin: int64;
   v,scale255: single;
   cnt: TUInt32s; //cnt: array [0..255] of integer;
begin
    for i := 0 to 255 do
        fHistogram[i].A := 0;
    vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
    if (vx < 1) or ((fMax-fMin) <= 0) then exit;
    setlength(cnt,256);
    if (Histo <> nil) and (length(Histo) = 256) then begin
      for i := 0 to 255 do
        cnt[i] := Histo[i];
    end else if (Histo = nil) or (length(Histo) < 255) then begin
    	MakeHistogram(cnt, fMin, fMax);
    end else begin //if Histo = nil, else use provided histogram
        mx := length(Histo)-1;
        scale255 := 255/mx;
        for i := 0 to mx do begin
            j := round(i * scale255);
            cnt[j] := cnt[j] + Histo[i];
        end;
    end;
    {$IFDEF DESPIKE} //limit bin to not be extreme relative to previous or next bin
    prevBin := cnt[1]; // bin[0] has only one neighbor: bin[1]
    currBin := cnt[0];
    for i := 0 to 255 do begin
        if i < 255 then
    		nextBin := cnt[i+1]
        else
            nextBin := cnt[254]; //bin[255] has only one neighbor: bin[254]
        mx := round(max(nextBin, prevBin) * kSpikeFrac);
        if currBin > mx then
        	cnt[i] := mx;
        prevBin := currBin;
        currBin := nextBin;
    end;
    {$ENDIF}
    mx := 0;
    for i := 0 to 255 do
        if (cnt[i] > mx) then mx := cnt[i];
    if mx < 2 then begin
      cnt := nil;
      exit;
    end;
    {$IFDEF HISTO_SQRT}
    scale255 := 255/sqrt(mx);
    {$ELSE}
    scale255 := 255/log2(mx);
    {$ENDIF}
    for i := 0 to 255 do begin
      if (cnt[i] < 1) then // log2(0) = -inf!
           v := 0
      else
      {$IFDEF HISTO_SQRT}
      	v := scale255 * sqrt(cnt[i]);
      {$ELSE}
        v := scale255 * log2(cnt[i]);
      {$ENDIF}
      fHistogram[i].A := round(v);
    end;
    cnt := nil;
end;

procedure TNIfTI.initUInt8();
//use header's cal_max and cal_min to rescale 8-bit data
var
  histo: TUInt32s;
  //slope255, f: single;
  i, vx,mn,mx, thresh, sum: int64;
  vol8: TUInt8s;
begin
  setlength(histo,256);
  for i := 0 to 255 do
      histo[i] := 0;
  vol8 := fRawVolBytes;
  mn := vol8[0];
  mx := mn;
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
  for i := 0 to (vx-1) do begin
      if vol8[i] < mn then
         mn := vol8[i];
      if vol8[i] > mx then
         mx := vol8[i];
      inc(histo[vol8[i]]);
  end;
  fMin := (mn * fHdr.scl_slope) + fHdr.scl_inter;
  fMax := (mx * fHdr.scl_slope) + fHdr.scl_inter;
  thresh := round(0.01 * vx);
  sum := 0;
  for i := 0 to 255 do begin
      sum := sum + histo[i];
      if (sum > thresh) then break;
  end;
  fAutoBalMin := (i * fHdr.scl_slope) + fHdr.scl_inter;
  //find high thresh
  if (histo[mn] > thresh) then
  	thresh := round(0.01 * (vx-histo[mn])); //<- preserve dynamic range for thresholded images
  sum := 0;
  for i := 255 downto 0 do begin
      sum := sum + histo[i];
      if (sum > thresh) then break;
  end;
  fAutoBalMax := (i * fHdr.scl_slope) + fHdr.scl_inter;
  initHistogram(histo);
  if histo[0] > round(0.75 * vx) then begin //thresholded image
     fAutoBalMin := 0;
     fAutoBalMax := 0;
  end;
  histo := nil;
  {$IFDEF TIMER}
  printf(format('uint8 range %g...%g', [ fMin, fMax]));
  printf(format('uint8 window %g...%g', [ fAutoBalMin, fAutoBalMax]));
  {$ENDIF}
end;

procedure TNIfTI.Convert2RGB(); //convert RGBA -> RGB
var
  rgba8in, rgba8temp: TUInt8s;
  i, j, k, vx: int64;
begin
  if (fHdr.datatype <> kDT_RGBA32) then exit;
  //showmessage(format('%d %d %d %d', [fHdr.dim[1], fHdr.dim[2], fHdr.dim[3], fVolumesLoaded]));
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]*fVolumesLoaded);
  rgba8in := TUInt8s(fRawVolBytes);
  setlength(rgba8temp, vx * 4);
  for i := 0 to ((vx*4)-1) do
      rgba8temp[i] := rgba8in[i];
  fRawVolBytes := nil; //release
  setlength(fRawVolBytes, 3 * vx); //RGB
  j := 0;
  k := 0;
  for i := 0 to (vx-1) do begin
      fRawVolBytes[j] := rgba8temp[k];
      j := j + 1; k := k + 1;
      fRawVolBytes[j] := rgba8temp[k];
      j := j + 1; k := k + 1;
      fRawVolBytes[j] := rgba8temp[k];
      j := j + 1; k := k + 1;
      k := k + 1;
  end;
  rgba8temp := nil;
  //change header
  fHdr.datatype := kDT_RGB;
  fHdr.bitpix:= 24;
  fHdrNoRotation.datatype := kDT_RGB;
  fHdrNoRotation.bitpix:= 24;
  printf('Converted RGBA -> RGB, Data type 2304 -> 128');
end;

procedure TNIfTI.Convert2UInt8(); //convert int8 -> int16
var
  i8in: TInt8s;
  ui8out: TUInt8s;
  i,vx: int64;
begin
  if (fHdr.datatype <> kDT_INT8) then exit;
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]*fVolumesLoaded);
  i8in := TInt8s(fRawVolBytes); //-128..127
  ui8out := TUInt8s(fRawVolBytes); //0..255
  for i := 0 to (vx-1) do
      ui8out[i] := i8in[i] + 128;
  fHdr.scl_inter:= fHdr.scl_inter - (128 * fHdr.scl_slope);
  fHdr.datatype := kDT_UINT8;
  fHdr.bitpix:= 8;
  fHdrNoRotation.scl_inter := fHdr.scl_inter;
  fHdrNoRotation.datatype := kDT_UINT8;
  fHdrNoRotation.bitpix:= 8;
  printf('Converted Int8 -> UInt8, Data type 256 -> 2');
end;

procedure TNIfTI.ConvertUint16Int16();
//change datatype kDT_UINT16 ->  kDT_SIGNED_SHORT
// uint16->int16
// optional: if skipped Convert2Float() will convert uint16->float32 (requiring twice the RAM!)

var
   u16: TUInt16s;
   i16: TInt16s;
   i,vx: int64;
   mn, mx: uint16;
begin
 if fHdr.datatype <> kDT_UINT16 then exit;
 //vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]*fVolumesLoaded);
 vx := length(fRawVolBytes) shr 1;
 if vx < 1 then exit;
 u16 := TUInt16s(fRawVolBytes);
 i16 := TInt16s(fRawVolBytes);
 mn :=u16[0];
 mx := mn;
 for i := 0 to (vx-1) do begin
     if (u16[i] > mx) then mx := u16[i];
     if (u16[i] < mn) then mn := u16[i];
 end;
 printf(format('Resize requires converting UInt16 -> Int16. Intensity range %d..%d', [mn, mx]));
 if (mx < 32768) then begin
    //int16 can store -32768..32767
    for i := 0 to (vx-1) do
        i16[i] := smallint(u16[i]);
    fHdr.datatype := kDT_SIGNED_SHORT;
    exit;
 end;
 printf(' Adjusting NIfTI header "scl_inter" to losslessy store UInt16 data');
 printf(format('UINT16 -> INT16 %d×%d×%d×%d ',[fHdr.dim[1],fHdr.dim[2],fHdr.dim[3],fVolumesLoaded]));
 for i := 0 to (vx - 1) do
 	i16[i] := smallint(u16[i]- 32768);
 fHdr.datatype := kDT_SIGNED_SHORT;
 fHdr.glmin := -32768; //glmin and glmax are technically unused, we use to flag that areas outside bounding box should be -32768
 fHdr.glmax := 32767;
 fHdr.scl_inter:= (32768 * fHdr.scl_slope) + fHdr.scl_inter;
end;

procedure TNIfTI.Convert2Float();
const
    flintmax32 = 16777216;
var
  ui16in,ui16temp: TUInt16s;
  ui32in,ui32temp: TUInt32s;
  i32in,i32temp: TInt32s;
  i64in,i64temp: TInt64s;
  ui64in,ui64temp: TUInt64s;
  f64in,f64temp: TFloat64s;
  f32in,f32temp,f32out: TFloat32s;
  mx, i,vx, j: int64;
begin
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]*fVolumesLoaded);
  if (fHdr.datatype = kDT_UINT16) then begin
     ui16in := TUInt16s(fRawVolBytes);
     setlength(ui16temp, vx);
     for i := 0 to (vx-1) do
         ui16temp[i] := ui16in[i];
     fRawVolBytes := nil; //release
     setlength(fRawVolBytes, 4 * vx);
     f32out := TFloat32s(fRawVolBytes);
     for i := 0 to (vx-1) do
         f32out[i] := ui16temp[i];
     ui16temp := nil;
  end else if (fHdr.datatype = kDT_INT32) then begin
     i32in := TInt32s(fRawVolBytes);
     setlength(i32temp, vx);
     mx := abs(i32in[0]);
     for i := 0 to (vx-1) do begin
         i32temp[i] := i32in[i];
         mx := max(mx, abs(i32in[i]));
     end;
     if (mx > flintmax32) then
     	printf('Loss of precision representing integers as floating point.');
     fRawVolBytes := nil; //release
     setlength(fRawVolBytes, 4 * vx);
     f32out := TFloat32s(fRawVolBytes);
     for i := 0 to (vx-1) do
         f32out[i] := i32temp[i];
      i32temp := nil;
  end else if (fHdr.datatype = kDT_UINT32) then begin
     ui32in := TUInt32s(fRawVolBytes);
     setlength(ui32temp, vx);
     mx := ui32in[0];
     for i := 0 to (vx-1) do begin
         ui32temp[i] := ui32in[i];
         mx := max(mx, ui32in[i]);
     end;
     if (mx > flintmax32) then
     	printf('Loss of precision representing integers as floating point.');
     fRawVolBytes := nil; //release
     setlength(fRawVolBytes, 4 * vx);
     f32out := TFloat32s(fRawVolBytes);
     for i := 0 to (vx-1) do
         f32out[i] := ui32temp[i];
      ui32temp := nil;
  end else if (fHdr.datatype = kDT_INT64) then begin
     i64in := TInt64s(fRawVolBytes);
     printf('Warning: many tools do not support INT64 images created with nibabel');
     setlength(i64temp, vx);
     mx := abs(i64in[0]);
     for i := 0 to (vx-1) do begin
         i64temp[i] := i64in[i];
         mx := max(mx, abs(i64in[i]));
     end;
     if (mx > flintmax32)  then
     	printf('Loss of precision representing integers as floating point.');
     fRawVolBytes := nil; //release
     setlength(fRawVolBytes, 4 * vx);
     f32out := TFloat32s(fRawVolBytes);
     for i := 0 to (vx-1) do
         f32out[i] := i64temp[i];
      i64temp := nil;
  end else if (fHdr.datatype = kDT_UINT64) then begin
     ui64in := TUInt64s(fRawVolBytes);
     printf('Warning: many tools do not support UINT64 images created with nibabel');
     setlength(ui64temp, vx);
     mx := ui64in[0];
     for i := 0 to (vx-1) do begin
         ui64temp[i] := ui64in[i];
         if ui64in[i] > mx then
         	mx := ui64in[i];
     end;
     if (mx > flintmax32)  then
     	printf('Loss of precision representing integers as floating point.');
     fRawVolBytes := nil; //release
     setlength(fRawVolBytes, 4 * vx);
     f32out := TFloat32s(fRawVolBytes);
     for i := 0 to (vx-1) do
         f32out[i] := ui64temp[i];
      ui64temp := nil;
  end else if (fHdr.datatype = kDT_DOUBLE) then begin
   f64in := TFloat64s(fRawVolBytes);
   setlength(f64temp, vx);
   for i := 0 to (vx-1) do
       f64temp[i] := f64in[i];
   fRawVolBytes := nil; //release
   setlength(fRawVolBytes, 4 * vx);
   f32out := TFloat32s(fRawVolBytes);
   for i := 0 to (vx-1) do
       f32out[i] := f64temp[i];
   f64temp := nil;

 end else if (fHdr.datatype = kDT_COMPLEX64) then begin
 //issue34 return absolute/modulus https://en.wikipedia.org/wiki/Absolute_value#Complex_numbers
 //printf('Converted complex data to absolute values (aka modulus)');
 printf('Displaying real component for DT_COMPLEX64 data');

 f32in := TFloat32s(fRawVolBytes);
 setlength(f32temp, vx);
 j := 0;
 for i := 0 to (vx-1) do begin
     f32temp[i] := f32in[j]; //<- real
     //f64temp[i] := sqrt(sqr(f64in[j]) + sqr(f64in[j+1])); //<- absolute, aka modulus
     j := j + 2;
 end;
 fRawVolBytes := nil; //release
 setlength(fRawVolBytes, 4 * vx);
 f32out := TFloat32s(fRawVolBytes);
 for i := 0 to (vx-1) do
     f32out[i] := f32temp[i];
 f32temp := nil;

 end else if (fHdr.datatype = kDT_COMPLEX128) then begin
 //issue34 return absolute/modulus https://en.wikipedia.org/wiki/Absolute_value#Complex_numbers
 //printf('Converted complex data to absolute values (aka modulus)');
 printf('Displaying real component for DT_COMPLEX128 data');

 f64in := TFloat64s(fRawVolBytes);
 setlength(f64temp, vx);
 j := 0;
 for i := 0 to (vx-1) do begin
     f64temp[i] := f64in[j]; //<- real
     //f64temp[i] := sqrt(sqr(f64in[j]) + sqr(f64in[j+1])); //<- absolute, aka modulus
     j := j + 2;
 end;
 fRawVolBytes := nil; //release
 setlength(fRawVolBytes, 4 * vx);
 f32out := TFloat32s(fRawVolBytes);
 for i := 0 to (vx-1) do
     f32out[i] := f64temp[i];
 f64temp := nil;
end;
  printf('Converted data type '+ inttostr(fHdr.datatype) +' -> '+inttostr(kDT_FLOAT32));
  fHdr.datatype := kDT_FLOAT32;
  fHdr.bitpix:= 32;
  fHdrNoRotation.datatype := kDT_FLOAT32;
  fHdrNoRotation.bitpix:= 32;
end;

function Scaled2RawIntensity (lHdr: TNIFTIhdr; lScaled: single): single;
begin
  if lHdr.scl_slope = 0 then
	result := (lScaled)-lHdr.scl_inter
  else
	result := (lScaled-lHdr.scl_inter) / lHdr.scl_slope;
end;

function TNIfTI.Scaled2Raw(lScaled: single): single;
begin
	result := Scaled2RawIntensity(fHdr, lScaled);
end;

procedure TNIfTI.InitFloat32(); //kDT_FLOAT
const
 kMaxBin = 4095;
function boundFloat(v: single): integer; inline;
begin
     if v <= 0 then exit(0);
     if v >= kMaxBin then exit(kMaxBin);
     result := round(v);
end;
var
  vol32: TFloat32s;
  //n0: int64;//masked images have huge numbers of zeros!
  thresh, sum, i,vx, nZero, nMax, mni: int64;
  mn : Single = 1.0 / 0.0;
  mx : Single = (- 1.0) / (0.0);
  v, slope: single;
  histo: TUInt32s;
  //{$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
begin
  vol32 := TFloat32s(fRawVolBytes);
  //vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]*fVolumesLoaded);
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
  //{$DEFINE RESCALE32}
  //n0 := 0;
  nZero := 0;
  nMax := 0;
  {$IFDEF RESCALE32}
  for i := 0 to (vx-1) do begin
     if (specialsingle(vol32[i])) then vol32[i] := 0.0;
     vol32[i] := (vol32[i] * fHdr.scl_slope) + fHdr.scl_inter;
     if vol32[i] < mn then
        mn := vol32[i];
     if vol32[i] > mx then
        mx := vol32[i];
     if vol32[i] = 0 then
        nZero := nZero + 1;
  end;
  //the for loop above has applied the scale and intercept
  // next lines ensure we do not apply them again!
  fHdr.scl_inter := 0;
  fHdr.scl_slope := 1;
  {$ELSE}
  // {$IFDEF TIMER}startTime := now;{$ENDIF}

  for i := 0 to (vx-1) do begin
     if (specialsingle(vol32[i])) then vol32[i] := 0.0;
     v := (vol32[i] * fHdr.scl_slope) + fHdr.scl_inter;
     if v < mn then
        mn := v;
     if v > mx then begin
        mx := v;
        nMax := 0;
     end;
     if (v = mx) then
     	nMax := nMax + 1;
     if v = 0 then
        nZero := nZero + 1;
  end;
  {$ENDIF}
  //robustMinMax(mn,mx);
  fMin := mn;
  fMax := mx;
  if ((mx > 0.0) and (nZero > 0) and ((nZero + nMax) = vx)) then begin
     	printf(format('binary float range %g..%g',[fMin,fMax]));
        fAutoBalMin := (0.0 * fHdr.scl_slope) + fHdr.scl_inter;
        fAutoBalMax := (mx * fHdr.scl_slope) + fHdr.scl_inter;
        exit;
  end;
  {$IFNDEF RESCALE32}
  mn := Scaled2RawIntensity(fHdr, mn);
  mx := Scaled2RawIntensity(fHdr, mx);
  {$ENDIF}
  if mx = mn then begin
     printf('No variability in Float32 data ');
     slope := 1;
  end else
      slope := kMaxBin / (mx - mn);
  //GLForm1.LayerBox.caption := format('%g',[slope]);
  setlength(histo, kMaxBin+1);//0..kMaxBin
  for i := 0 to kMaxBin do
      histo[i] := 0;
  for i := 0 to (vx-1) do
      inc(histo[ boundFloat((vol32[i]-mn) * slope)]);

  thresh := round(0.01 * vx);
  sum := 0;
  for i := 0 to kMaxBin do begin
      sum := sum + histo[i];
      if (sum > thresh) then break;
  end;
  {$IFDEF RESCALE32}
  fAutoBalMin := (i * 1/slope) + mn;
  {$ELSE}
  fAutoBalMin := (i/slope) + mn;
  fAutoBalMin := (fAutoBalMin * fHdr.scl_slope) + fHdr.scl_inter;
  {$ENDIF}
  //find high thresh
  for i := 0 to kMaxBin do begin
      mni := i;
      if (histo[i] > 0) then break;
  end;
  if (histo[mni] > thresh) then
  	thresh := round(0.01 * (vx-histo[mni])); //<- preserve dynamic range for thresholded images
  sum := 0;
  //printf(format('bins %d min %g max %g', [kMaxBin, mn, mx]));
  for i := kMaxBin downto 0 do begin
      sum := sum + histo[i];
      if (sum > thresh) then break;
      //printf(format('%d%s%d', [i,#9 ,histo[i]]));
  end;
  {$IFDEF RESCALE32}
  fAutoBalMax := (i * 1/slope) + mn;
  {$ELSE}
  fAutoBalMax := (i/slope) + mn;
  fAutoBalMax := (fAutoBalMax * fHdr.scl_slope) + fHdr.scl_inter;
  {$ENDIF}
  //if (fMax > 0.0) and (n0 > (10 * thresh)) then
  //   fAutoBalMax := fMax; //masked image - huge number of zeros
   if (fAutoBalMax = fAutoBalMin) then
     fAutoBalMax := ((i+1) * 1/slope) + mn;
  initHistogram(histo);
  histo := nil;
  if nZero > round(0.75 * vx) then begin //thresholded image
     fAutoBalMin := 0;
     fAutoBalMax := 0;
  end;
  {$IFDEF TIMER}
  //printf(format('float voxels %d slope %g inter %g',[vx, fHdr.scl_slope, fHdr.scl_inter]));
  //printf(format('n0 = %d (%g)', [n0, n0/vx]));
  printf(format('float range %g..%g',[fMin,fMax]));
  printf(format('float window %g...%g', [ fAutoBalMin, fAutoBalMax]));
  {$ENDIF}
end;

{$DEFINE FAST16}
{$IFDEF FAST16}
procedure TNIfTI.initInt16(); //kDT_SIGNED_SHORT
const
 kMaxWord = 65535;
 kMin16 = 32768;
var
  histo, histo8: TUInt32s;
  vol16: TInt16s;
  slope: single;
  i, j, vx, mn,mx, thresh, sum: int64;
begin
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
  if vx < 1 then exit;
  setlength(histo, kMaxWord+1); //0..kMaxBin
  FillChar(histo[0], Length(histo) * SizeOf(uint32), 0);
  vol16 := TInt16s(fRawVolBytes);
  for i := 0 to (vx-1) do
      inc(histo[vol16[i]+kMin16]);
  //find min
  for i := 0 to kMaxWord do
    if histo[i] > 0 then
      break;
  mn := i;
  fMin := ((mn-kMin16) * fHdr.scl_slope) + fHdr.scl_inter;
  //find max
  for i := kMaxWord downto 0 do
    if histo[i] > 0 then
      break;
  mx := i;
  fMax := ((mx-kMin16) * fHdr.scl_slope) + fHdr.scl_inter;
  thresh := round(0.01 * vx);
  //find low thresh
  sum := 0;
  for i := 0 to kMaxWord do begin
      sum := sum + histo[i];
      if (sum > thresh) then break;
  end;
  fAutoBalMin := i - kMin16;
  fAutoBalMin := (fAutoBalMin * fHdr.scl_slope) + fHdr.scl_inter;
  //find high thresh
  if (histo[mn] > thresh) then
  	thresh := round(0.01 * (vx-histo[mn])); //<- preserve dynamic range for thresholded images
  sum := 0;
  for i := kMaxWord downto 0 do begin
    sum := sum + histo[i];
    if (sum > thresh) then break;
  end;
  fAutoBalMax := i - kMin16;
  fAutoBalMax := (fAutoBalMax * fHdr.scl_slope) + fHdr.scl_inter;
  //histogram is in range 0...65535, we want a histogram from min..max
  setlength(histo8, 256); //0..255
  for i := 0 to 255 do
      histo8[i] := 0;
  slope := 1;
  if (mx > mn) then
     slope := 255 / (mx - mn) ;
  for i := mn to mx do begin
      j := round((i - mn) * slope);
      histo8[j] := histo8[j] + histo[i];
  end;
  if histo[kMin16] > round(0.75 * vx) then begin //thresholded image
     fAutoBalMin := 0;
     fAutoBalMax := 0;
  end;
  histo := nil;
  initHistogram(histo8);
  histo8 := nil;
  {$IFDEF TIMER}
  printf(format('int16 range %g...%g', [ fMin, fMax]));
  printf(format('int16 window %g...%g', [fAutoBalMin, fAutoBalMax]));
  {$ENDIF}
end;
{$ELSE}
procedure TNIfTI.initInt16(); //kDT_SIGNED_SHORT
const
 kMaxBin = 4095;
function boundFloat(v: single): integer; inline;
begin
    if v <= 0 then exit(0);
    if v >= 4095 then exit(4095);
    result := round(v);
end;
var
  vol16: TInt16s;
  histo: TUInt32s;
  i, vx,mn,mx, thresh, sum: int64;
  slope: single;
begin
  setlength(histo, kMaxBin+1); //0..kMaxBin
  for i := 0 to kMaxBin do
      histo[i] := 0;
  vol16 := TInt16s(fRawVolBytes);
  mn := vol16[0];
  mx := mn;
  //vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]*fVolumesLoaded);
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
  for i := 0 to (vx-1) do begin
      if vol16[i] < mn then
         mn := vol16[i];
      if vol16[i] > mx then
         mx := vol16[i];
  end;
  fMin := mn;
  fMax := mx;
  //robustMinMax(fMin,fMax);
  mn := round(fMin);
  mx := round(fMax);
  fMin := (mn * fHdr.scl_slope) + fHdr.scl_inter;
  fMax := (mx * fHdr.scl_slope) + fHdr.scl_inter;
  slope := kMaxBin / (mx - mn) ;
  for i := 0 to kMaxBin do
      histo[i] := 0;
  for i := 0 to (vx-1) do
      inc(histo[ boundFloat((vol16[i]-mn) * slope)]);
  thresh := round(0.01 * vx);
  sum := 0;
  for i := 0 to kMaxBin do begin
      sum := sum + histo[i];
      if (sum > thresh) then break;
  end;
  fAutoBalMin := (i * 1/slope) + mn;
  sum := 0;
  for i := kMaxBin downto 0 do begin
      sum := sum + histo[i];
      if (sum > thresh) then break;
  end;
  fAutoBalMax := (i * 1/slope) + mn;
  if (fAutoBalMax = fAutoBalMin) then
     fAutoBalMax := ((i+1) * 1/slope) + mn;
  fAutoBalMin := (fAutoBalMin * fHdr.scl_slope) + fHdr.scl_inter;
  fAutoBalMax := (fAutoBalMax * fHdr.scl_slope) + fHdr.scl_inter;
  initHistogram(histo);
  histo := nil;
  {$IFDEF TIMER}
  printf(format('int16 range %g...%g', [ fMin, fMax]));
  printf(format('int16 window %g...%g', [ fAutoBalMin, fAutoBalMax]));
  {$ENDIF}
end;
{$ENDIF}

procedure TNIfTI.Mask(MaskingVolume: TUInt8s; isPreserveMask: boolean);
var
  mni, i, j, vx, skipVx: int64;
  mn: single;
  vol8: TUInt8s;
  vol16: TInt16s;
  vol32: TFloat32s;
begin
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
  if vx < 1 then exit;
  if vx <> length(MaskingVolume) then exit;
  skipVx := skipVox();
  vol8 := fRawVolBytes;
  vol16 := TInt16s(vol8);
  vol32 := TFloat32s(vol8);
  mn := Scaled2RawIntensity(fHdr, fMin);
  mni := round(mn);
  if (isPreserveMask) then begin
     if fHdr.datatype = kDT_RGB then begin
       j := 0;
       for i := 0 to (vx-1) do begin
            if MaskingVolume[i] = 0 then begin
              vol8[skipVx+j+0] := 0;
              vol8[skipVx+j+1] := 0;
              vol8[skipVx+j+2] := 0;
            end;
            j := j + 3;
       end;
     end else if fHdr.datatype = kDT_UINT8 then begin
        for i := 0 to (vx-1) do
            if MaskingVolume[i] = 0 then vol8[skipVx+i] := 0;
     end else if fHdr.datatype = kDT_INT16 then begin
       for i := 0 to (vx-1) do
           if MaskingVolume[i] = 0 then vol16[skipVx+i] := mni;
     end else if fHdr.datatype = kDT_FLOAT then begin
       for i := 0 to (vx-1) do
           if MaskingVolume[i] = 0 then vol32[skipVx+i] := mn;
     end;
  end else begin
     if fHdr.datatype = kDT_RGB then begin
       j := 0;
       for i := 0 to (vx-1) do begin
            if MaskingVolume[i] <> 0 then begin
              vol8[skipVx+j+0] := 0;
              vol8[skipVx+j+1] := 0;
              vol8[skipVx+j+2] := 0;
            end;
            j := j + 3;
       end;
     end else if fHdr.datatype = kDT_UINT8 then begin
       for i := 0 to (vx-1) do
           if MaskingVolume[i] <> 0 then vol8[skipVx+i] := 0;
    end else if fHdr.datatype = kDT_INT16 then begin
      for i := 0 to (vx-1) do
          if MaskingVolume[i] <> 0 then vol16[skipVx+i] := mni;
    end else if fHdr.datatype = kDT_FLOAT then begin
      for i := 0 to (vx-1) do
          if MaskingVolume[i] <> 0 then vol32[skipVx+i] := mn;
    end;
  end;
  SetDisplayMinMax(true);
end;

function TNIfTI.VoxMM3: single;
var
  x, y, z: single;
  v, v0: TVec4;
begin
 v0 := Vec4(0, 0, 0, 1);
 Coord(v0, fMat);
 v := Vec4(1, 0, 0, 1); //X
 Coord(v, fMat);
 v := v - v0;
 x := v.Length;
 v := Vec4(0, 1, 0, 1); //Y
 Coord(v, fMat);
 v := v - v0;
 y := v.Length;
 v := Vec4(0, 0, 1, 1); //Z
 Coord(v, fMat);
 v := v - v0;
 z := v.Length;
 result := x * y * z;
 //printf(format('%.4fx%.4fx%.4fmm = %.4fmm3', [x,y,z, result]));
end;

function TNIfTI.RemoveSmallClusters(thresh, mm: double; NeighborMethod: integer): single; //returns surviving mm3
{$IFDEF UNIX}{$DEFINE rsc}{$ENDIF} //Windows can not get console messages
var
  mnVx, n, i, vx, skipVx : int64;
  vol8, out8: TUInt8s;
  mni: int16;
  vol16: TInt16s;
  vol32: TFloat32s;
  rthresh, mn: single;
  mmPerVox: double;
begin
 result := 0;
 vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
 if fHdr.datatype = kDT_RGB then exit;
 //todo: DT_RGB
 if vx < 1 then exit;
 skipVx := skipVox();
 out8 := fRawVolBytes;
 vol16 := TInt16s(out8);
 vol32 := TFloat32s(out8);
 rthresh := Scaled2RawIntensity(fHdr, thresh);
 setlength(vol8,vx);
 FillChar(vol8[0], vx, 0);
 if thresh < 0.0 then begin
    if fHdr.datatype = kDT_UINT8 then begin
       for i := 0 to (vx-1) do
           if out8[skipVx+i] <= rthresh then vol8[i] := 255;
    end else if fHdr.datatype = kDT_INT16 then begin
      for i := 0 to (vx-1) do
          if vol16[skipVx+i] <= rthresh then vol8[i] := 255;
    end else if fHdr.datatype = kDT_FLOAT then begin
      for i := 0 to (vx-1) do
          if vol32[skipVx+i] <= rthresh then vol8[i] := 255;
    end;
 end else begin
   if fHdr.datatype = kDT_UINT8 then begin
      for i := 0 to (vx-1) do
          if out8[skipVx+i] >= rthresh then vol8[i] := 255;
   end else if fHdr.datatype = kDT_INT16 then begin
     for i := 0 to (vx-1) do
         if vol16[skipVx+i] >= rthresh then vol8[i] := 255;
   end else if fHdr.datatype = kDT_FLOAT then begin
     for i := 0 to (vx-1) do
         if vol32[skipVx+i] >= rthresh then vol8[i] := 255;
   end;
 end;
 //clusterize
 //mmPerVox := fHdr.pixdim[1] * fHdr.pixdim[2] * fHdr.pixdim[3];
 mmPerVox := VoxMM3();
 if (mmPerVox = 0) then mmPerVox := 1;
 mnVx := round(mm/mmPerVox);
 {$IFDEF rsc}
 printf(format('RemoveSmallClusters(thresh=%0.4f, mm=%g)', [thresh, mm]));
 printf(format(' resliced voxels %dx%dx%d', [fHdr.dim[1], fHdr.dim[2], fHdr.dim[3]]));
 printf(format(' mm2vox=%d', [mnVx]));
 n := 0;
 for i := 0 to (vx-1) do
     if  vol8[i] <> 0 then inc(n);
 printf(format(' voxelsExceedingThresh=%d (%.4fmm3)', [n, n * mmPerVox]));
 {$ENDIF}
 if mnVx > 0 then
    RemoveAllSmallClusters(vol8, fHdr.dim[1], fHdr.dim[2], fHdr.dim[3],255,0 , mnVx, NeighborMethod);
 if (fMin < 0.0) and (fMax > 0.0) then
    mn := Scaled2RawIntensity(fHdr, 0)
 else
     mn := Scaled2RawIntensity(fHdr, fMin);
 mni := round(mn);
 if fHdr.datatype = kDT_UINT8 then begin
    for i := 0 to (vx-1) do
        if vol8[i] = 0 then out8[skipVx+i] := mni;
 end else if fHdr.datatype = kDT_INT16 then begin
   for i := 0 to (vx-1) do
       if vol8[i] = 0 then vol16[skipVx+i] := mni;
 end else if fHdr.datatype = kDT_FLOAT then begin
   for i := 0 to (vx-1) do
       if vol8[i] = 0 then vol32[skipVx+i] := mn;
 end;
 //compute number of survivors
 n := 0;
 for i := 0 to (vx-1) do
     if  vol8[i] <> 0 then inc(n);
 printf(format(' voxelsInLargeClusters=%d (%.4fmm3)', [n, n * mmPerVox]));
 result := n * mmPerVox;
 vol8 := nil;
end;

procedure TNIfTI.RemoveHaze(isSmoothEdges: boolean = true; isSingleObject: boolean = true; OtsuLevels : integer = 5);
//procedure TNIfTI.RemoveHaze(isSmoothEdges: boolean; OtsuLevels : integer = 5);
var
   mni, i,vx, skipVx: int64;
   mask8, vol8, out8: TUInt8s;
   vol16: TInt16s;
   vol32: TFloat32s;
   mn: single;
begin
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
  //todo: DT_RGB
  if vx < 1 then exit;
  vol8 := DisplayMinMax2Uint8;
  if vol8 = nil then exit;
  skipVx := skipVox();
  //1=1/4, 2=1/3, 3=1/2, 4=2/3, 5=3/4
  if (OtsuLevels < 1) or (OtsuLevels > 5) then begin
     OtsuLevels := max(OtsuLevels, 1);
     OtsuLevels := min(OtsuLevels, 5);
     printf('Otsu levels must be between 1..5: using '+inttostr(OtsuLevels));
  end;
  ApplyOtsuBinary (vol8, vx, OtsuLevels);
  if (isSingleObject) then
     PreserveLargestCluster(vol8, fHdr.dim[1], fHdr.dim[2], fHdr.dim[3],255,0 );
  if (isSmoothEdges) then begin //.Smooth soften edges but preserve interior
    //zz setlength(mask8,vx);
    mask8 := Copy(vol8, Low(vol8), Length(vol8));
    SimpleMaskErode(mask8, fHdr.dim[1], fHdr.dim[2], fHdr.dim[3]);
    SimpleMaskErode(mask8, fHdr.dim[1], fHdr.dim[2], fHdr.dim[3]);
    SimpleMaskErode(mask8, fHdr.dim[1], fHdr.dim[2], fHdr.dim[3]);
    Smooth(mask8);
    mask8 := nil;
  end;
  //
  SimpleMaskDilate(vol8, fHdr.dim[1], fHdr.dim[2], fHdr.dim[3]);
  SimpleMaskDilate(vol8, fHdr.dim[1], fHdr.dim[2], fHdr.dim[3]);
  out8 := fRawVolBytes;
  vol16 := TInt16s(out8);
  vol32 := TFloat32s(out8);
  mn := Scaled2RawIntensity(fHdr, fMin);
  mni := round(mn);
  if fHdr.datatype = kDT_UINT8 then begin
     for i := 0 to (vx-1) do
         if vol8[i] = 0 then out8[skipVx+i] := 0;
  end else if fHdr.datatype = kDT_INT16 then begin
    for i := 0 to (vx-1) do
        if vol8[i] = 0 then vol16[skipVx+i] := mni;
  end else if fHdr.datatype = kDT_FLOAT then begin
    for i := 0 to (vx-1) do
        if vol8[i] = 0 then vol32[skipVx+i] := mn;
  end;
  vol8 := nil;//free
  SetDisplayMinMax(true);
end;

procedure TNIfTI.LoadRGBVector();
var
   i, vx, byts, k: int64;
   in8: TUInt8s;
   in32: TFloat32s;
   r,g,b, mx, slope: single;
begin
 //printf(format('>>>%d', [fVolumesLoaded]));
  	 if ((fHdr.intent_code <> kNIFTI_INTENT_RGB_VECTOR) and (fHdr.intent_code <> kNIFTI_INTENT_VECTOR)) or (fHdr.datatype <> kDT_FLOAT32) or (fVolumesLoaded <> 3) then exit;
 //printf(format('>>>??%d', [fVolumesLoaded]));
     vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
     byts := vx * 3 * 4;
     if length(fRawVolBytes) < byts then exit; //3 volumes, each 32-bit
     //zz setlength(in8, byts);
     in8 := Copy(fRawVolBytes, 0, byts);
     in32 := TFloat32s(in8);
     setlength(fRawVolBytes, vx * 3);
     fVolumesLoaded := 1;
     fVolumesTotal := 1;
     //fHdr.datatype := kDT_RGBA32;
     //fHdr.bitpix := 32;

     fHdr.datatype := kDT_RGB;
     fHdr.cal_max := 255;
     fHdr.cal_min := 0;
     fHdr.bitpix := 24;
     fHdr.dim[0] := 3;
     mx := abs(in32[0]);
     for i := 0 to ((vx * 3) - 1) do
         mx := max(mx, abs(in32[i]));
     slope := 1.0;
     if (mx > 0.0) then
     	slope := 1.0 / mx;
     for i := 4 to 7 do
     	 fHdr.dim[i] := 1;
     k := 0;
     for i := 0 to (vx - 1) do begin
         //alpha := (vol24[skipVx+i].r+vol24[skipVx+i].g+vol24[skipVx+i].g+vol24[skipVx+i].b) div 4;  //favor green
         (*r := min(abs(in32[i]),1);
         g := min(abs(in32[i+vx]),1);
         b := min(abs(in32[i+vx+vx]),1); *)
         r := abs(in32[i]) * slope;
         g := abs(in32[i+vx]) * slope;
         b := abs(in32[i+vx+vx]) * slope;

         //a := sqrt(r*r + g*g + b * b);
         //{$IFDEF DYNRGBA}
         //outRGBA[i] := SetRGBA(round(r*255), round(g*255), round(b*255), round(a*255));
         fRawVolBytes[k] := round(r*255); k += 1;
         fRawVolBytes[k] := round(g*255); k+= 1;
         fRawVolBytes[k] := round(b*255); k+= 1;
         //{$ELSE}
         //fRawVolBytes[k] := round(r*255); k += 1;
         //fRawVolBytes[k] := round(g*255); k+= 1;
         //fRawVolBytes[k] := round(b*255); k+= 1;
         //outRGBA^[i] := SetRGBA(luts[vol24[skipVx+i].r], luts[vol24[skipVx+i].g], luts[vol24[skipVx+i].b], alpha);
         //{$ENDIF}
     end;
     fAutoBalMin := 0;
     fAutoBalMax := 255;
     fMin := 0;
     fMax := 255;
     fWindowMin := 0;
     fWindowMax := 255;
     in8 := nil;
end;

(*procedure TNIfTI.SetDisplayMinMaxRGBV1();
var
   i, vx, lC: int64;
   vol32: TFloat32s;
   r,g,b, a: single;
begin
  if (fHdr.datatype <> kDT_FLOAT32) or (fVolumesLoaded <> 3) then exit;
  vol32 := TFloat32s(fRawVolBytes);
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
  for i := 0 to (vx - 1) do begin
      //alpha := (vol24[skipVx+i].r+vol24[skipVx+i].g+vol24[skipVx+i].g+vol24[skipVx+i].b) div 4;  //favor green
      r := min(abs(vol32[i]),1);
      g := min(abs(vol32[i+vx]),1);
      b := min(abs(vol32[i+vx+vx]),1);
      a := sqrt(r*r + g*g + b * b);
      {$IFDEF DYNRGBA}
      fVolRGBA[i] := SetRGBA(round(r*255), round(g*255), round(b*255), round(a*255));
      {$ELSE}
      fVolRGBA^[i] := SetRGBA(luts[vol24[skipVx+i].r], luts[vol24[skipVx+i].g], luts[vol24[skipVx+i].b], alpha);
      {$ENDIF}
  end;
end;  *)

procedure TNIfTI.SetDisplayMinMaxRGB24();
var
   skipVx, i, vx, lC: int64;
   vol24: TRGBs;
   luts: array[0..255] of byte;
   alpha: integer;
   lV, bias, lSwap, lMin, lMax, lRng: single;
begin
  if fHdr.datatype <> kDT_RGB then exit;
  lMin := Scaled2RawIntensity(fHdr, fWindowMin);
  lMax := Scaled2RawIntensity(fHdr, fWindowMax);
  if lMin > lMax then begin
        lSwap := lMin;
        lMin := lMax;
        lMax := lSwap;
  end;
  lRng := abs(lMax - lMin);
  if lRng <= 0.0 then lRng := 255;
  bias := 1.0 - ( 0.5 * (lRng/ 255));
  if (bias <= 0.0) then bias := 0.001;
  if (bias >= 1.0) then bias := 0.999;
  for lC := 0 to 255 do begin
          lV := lC/255.0;
          lV := (lV/ ((((1/bias) - 2)*(1 - lV))+1));
          if (lV > 1.0) then lV := 1.0;
          if (lV < 0.0) then lV := 0.0;
          luts[lC] := round(255.0 * lV);
   end; //for all indices
  vol24 := TRGBs(fRawVolBytes);
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
  skipVx := skipVox();
  for i := 0 to (vx - 1) do begin
      //alpha := (vol24[skipVx+i].r+vol24[skipVx+i].g+vol24[skipVx+i].g+vol24[skipVx+i].b) div 4;  //favor green
      alpha := max(max(vol24[skipVx+i].r,vol24[skipVx+i].g), vol24[skipVx+i].b);
      {$IFDEF DYNRGBA}
      fVolRGBA[i] := SetRGBA(luts[vol24[skipVx+i].r], luts[vol24[skipVx+i].g], luts[vol24[skipVx+i].b], alpha);
      {$ELSE}
      fVolRGBA^[i] := SetRGBA(luts[vol24[skipVx+i].r], luts[vol24[skipVx+i].g], luts[vol24[skipVx+i].b], alpha);
      {$ENDIF}
  end;
end;

function setUnitRange(s: single): single;
begin
     if s < 0 then s := 0;
     if s > 1 then s := 1;
     result := s;
end;

procedure sortVec3(var lo, hi: TVec3);
var
   temp: TVec3;
   i: integer;
begin
     temp := hi;
     for i := 0 to 2 do begin
         if (hi[i] < lo[i]) then begin
            hi[i]:= lo[i];
            lo[i] := temp[i];
         end;
         lo[i] := setUnitRange(lo[i]);
         hi[i] := setUnitRange(hi[i]);
     end;
end;

procedure TNIfTI.ApplyCutout();
var
  x, y, z, xMn, yMn, zMn, xMx, yMx, zMx, dX, dXY: int64;
  clr: TRGBA;
begin
 if (fCutoutLow.X = fCutoutHigh.X) or (fCutoutLow.Y = fCutoutHigh.Y) or (fCutoutLow.Z = fCutoutHigh.Z) then exit;
 if not HiddenByCutout then exit;
 sortVec3(fCutoutLow, fCutoutHigh);
 xMn := round((fHdr.dim[1]-1)*fCutoutLow.x);
 yMn := round((fHdr.dim[2]-1)*fCutoutLow.y);
 zMn := round((fHdr.dim[3]-1)*fCutoutLow.z);
 xMx := round((fHdr.dim[1]-1)*fCutoutHigh.x);
 yMx := round((fHdr.dim[2]-1)*fCutoutHigh.y);
 zMx := round((fHdr.dim[3]-1)*fCutoutHigh.z);
 if (xMx <= xMn) or (yMx <= yMn) or (zMx <= zMn) then exit;
 dX := fHdr.dim[1];
 dXY := fHdr.dim[1] * fHdr.dim[2];
 //if (length(fVolRGBA) < 1) and (DisplayMin < 0) and (DisplayMax < 0) then begin
 if (fVolRGBA = nil) and (DisplayMin < 0) and (DisplayMax < 0) then begin
    if (length(fCache8) < 1) then exit;
    for z := zMn to zMx do
        for y := yMn to yMx do
            for x := xMn to xMx do
                fCache8[x + (y*dX) + (z * dXY)] := 255;
    exit;
 end;
 //if (length(fVolRGBA) < 1) then begin
 if (fVolRGBA = nil) then begin
    if (length(fCache8) < 1) then exit;
    for z := zMn to zMx do
        for y := yMn to yMx do
            for x := xMn to xMx do
                fCache8[x + (y*dX) + (z * dXY)] := 0;
    exit;
 end;
 clr := setRGBA(0,0,0,0);
 for z := zMn to zMx do
     for y := yMn to yMx do
         for x := xMn to xMx do
         {$IFDEF DYNRGBA}
         	 fVolRGBA[x + (y*dX) + (z * dXY)] := clr;
		 {$ELSE}
     	 fVolRGBA^[x + (y*dX) + (z * dXY)] := clr;
         {$ENDIF}
end;

function isContrastSame(fWindowMin, fWindowMinCache8, fWindowMax, fWindowMaxCache8: single):boolean;
const //the min/max brightness labels have a rounding error
 kTiny = 1e-4;
var
  mn, mx: single;
begin
  mn := abs(fWindowMin - fWindowMinCache8);
  mx := abs(fWindowMax - fWindowMaxCache8);
  if (mn < kTiny) and (mx < kTiny) then exit(true);
  //writeln(format('++Set Min/Max Window %g..%g %g', [mn,mx, kTiny]));
  exit(false);
end;

function TNIfTI.NeedsUpdate(): boolean;
begin
     result := true;
     //if (not clut.NeedsUpdate) and (fHdr.datatype <> kDT_RGB) and (fCache8 <> nil) and (not specialsingle(fWindowMinCache8)) and (fOpacityPct = fOpacityPctCache8) and (fWindowMin = fWindowMinCache8) and (fWindowMax = fWindowMaxCache8) then
     if (not clut.NeedsUpdate) and (fHdr.datatype <> kDT_RGB) and (fCache8 <> nil) and (not specialsingle(fWindowMinCache8)) and (fOpacityPct = fOpacityPctCache8) and isContrastSame(fWindowMin, fWindowMinCache8, fWindowMax, fWindowMaxCache8) then
        result := false;
end;

procedure TNIfTI.DisplayLabel2Uint8();
var
   skipVx, v, vx: int64;
   vol8: TUInt8s;
   vol16: TInt16s;
   vol32f: TFloat32s;
   //j: integer;
begin
 vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
 setlength(fCache8, vx);
 vol8 := fRawVolBytes;
 skipVx := skipVox();
 if fHdr.datatype = kDT_FLOAT then begin //32 bit data
   vol32f := TFloat32s(vol8);
   for v := 0 to (vx-1) do
       fCache8[v] := ((round(vol32f[v+skipVx])-1) mod 100)+1;
 end else if fHdr.datatype = kDT_INT16 then begin //16 bit data
   vol16 := TInt16s(vol8);
   if fLabelMask <> nil then begin
        for v := 0 to (vx-1) do
            if fLabelMask[vol16[v+skipVx]] <> 1 then
               fCache8[v] := ((vol16[v+skipVx]-1) mod 100)+1
            else
                fCache8[v] := 0;
   end else
       for v := 0 to (vx-1) do
           fCache8[v] := ((vol16[v+skipVx]-1) mod 100)+1;
 end else begin
     if (fHdr.datatype <> kDT_UNSIGNED_CHAR) then
        printf('Unsupported label datatype '+inttostr(fHdr.datatype));
     for v := 0 to (vx-1) do
            fCache8[v] := vol8[v+skipVx];
     if fLabelMask <> nil then begin
        for v := 0 to (vx-1) do
            if fLabelMask[fCache8[v]] = 1 then
                 fCache8[v] := 0;
        (*j := 0;
        for v := 0 to length(fLabelMask)-1 do
            if fLabelMask[v] = 0 then
                 j := j + 1;
        writeln(inttostr(j)+'<<<<visible regions'+inttostr(random(123))); *)

     end;
 end; //if ... else 8bit
end;

{$IFDEF PARALLEL16}
const kMaxParallel = 4;

procedure TNIfTI.SetDisplayMinMaxParallel(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
//Compute transfer function across cores
//http://wiki.lazarus.freepascal.org/Parallel_procedures
//Remarkably little benefit, perhaps because
// "Do not work on vast amounts of memory. On some systems one thread alone is fast enough to fill the memory bus speed."
var
  slope, lMin, lMax, lSwap, lRng, lMod: single;
  skipVx, i, vx, vxLo, vxHi: int64;
  vol8:TUInt8s;
  vol16: TInt16s;
  vol32: TFloat32s;
  luts: array[0..255] of byte;
begin
 vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
 vxLo := round((index/kMaxParallel) * vx);
 vxHi := round(((index+1)/kMaxParallel) * vx)-1;
 vol8 := fRawVolBytes;
 skipVx := SkipVox();
 lMin := Scaled2RawIntensity(fHdr, fWindowMin);
 lMax := Scaled2RawIntensity(fHdr, fWindowMax);
 slope := 255/(lMax - lMin);
 vol16 := TInt16s(vol8);
 vol32 := TFloat32s(vol8);
 if fHdr.datatype = kDT_UINT8 then begin
   lRng := (lMax - lMin);
   if lRng <> 0 then
    lMod := abs((((254)/lRng)))
   else
     lMod := 0;
   if lMin > lMax then begin
        lSwap := lMin;
        lMin := lMax;
        lMax := lSwap;
   end;
   for i := 0 to 255 do begin
     if i <= lMin then
       luts[i] := 0
     else if i >= lMax then
         luts[i] := 255
     else
        luts[i] := trunc(((i-lMin)*lMod)+1);
   end;
   for i := vxLo to (vxHi) do
       fCache8[i] := luts[vol8[skipVx+i]];
 end else if fHdr.datatype = kDT_INT16 then begin
     for i := vxLo to (vxHi) do begin
       if (vol16[skipVx+i] >= lMax) then
          fCache8[i] := 255
       else if  (vol16[skipVx+i] <= lMin) then
           fCache8[i] := 0
       else
          fCache8[i] := round((vol16[skipVx+i] - lMin) * slope);
     end;
 end else if fHdr.datatype = kDT_FLOAT then begin
   for i := vxLo to vxHi do begin
     if (vol32[skipVx+i] >= lMax) then
        fCache8[i] := 255
     else if  (vol32[skipVx+i] <= lMin) then
         fCache8[i] := 0
     else
        fCache8[i] := round((vol32[skipVx+i] - lMin) * slope);
   end;
 end;
end;
{$ENDIF}
procedure TNIfTI.DisplayRGB();
var
   vx: integer;
begin
 vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
 if (vx < 1) then exit;
 {$IFDEF DYNRGBA}
 setlength(fVolRGBA, vx);
 {$ELSE}
 setlengthp(fVolRGBA, vx);
 {$ENDIF}
 //for i := 0 to (vx -1) do
 //	 fVolRGBA[i] := setrgba(255,0,128,128);

 //exit;
 if fHdr.datatype = kDT_RGB then
 	SetDisplayMinMaxRGB24;
 //if (fDTImode = kDTIrgb) and (fHdr.datatype = kDT_FLOAT32) and (fVolumesLoaded = 3) then
 //	SetDisplayMinMaxRGBV1();
end;

function TNIfTI.DisplayRGBGreen(): TUInt8s;
//return green component as volume: must free memory!
var
  vol8:TUInt8s;
  skipVx, i, j, vx: int64;
begin
     if (fHdr.datatype <> kDT_RGB) then exit(nil);
     vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
     if vx < 1 then exit(nil);
     skipVx := SkipVox();
     setlength(vol8, vx);
     (*j := 1;//0=R,1=G,2=B
     for i := 0 to (vx-1) do begin
         vol8[i] := fRawVolBytes[skipVx+j];
         j := j + 3;
     end;*)
     j := 0;//0=R,1=G,2=B
     for i := 0 to (vx-1) do begin
         vol8[i] := (fRawVolBytes[skipVx+j]+fRawVolBytes[skipVx+j+1]+fRawVolBytes[skipVx+j+1]+fRawVolBytes[skipVx+j+2]) div 4;
         j := j + 3;
     end;
     result := vol8;
end;

function TNIfTI.DisplayMinMax2Uint8(isForceRefresh: boolean = false): TUInt8s;
//{$DEFINE LUT16}
{$IFDEF LUT16}
const
 kMaxWord = 65535;
 kMin16 = 32768;
{$ENDIF}
var
  {$IFDEF LUT16}luts16: TUInt16s;{$ENDIF}
  {$IFNDEF SSE} slope: single;
  j : int64;
  {$ENDIF}
  lMin, lMax, lSwap, lRng, lMod: single;
  skipVx, i,  vx: int64;
  zero8: UInt8;
  zero16: UInt16;
  zero32: single;
  vol8:TUInt8s;
  vol16: TInt16s;
  vol32: TFloat32s;
  luts: array[0..255] of byte;
begin

  if (fHdr.datatype = kDT_RGB) then exit(nil); //DT_RGB: use DisplayRGBGreen
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
  if vx < 1 then exit(nil);
  if (not isForceRefresh) and (not NeedsUpdate) then// (not clut.NeedsUpdate) and (fHdr.datatype <> kDT_RGB) and (fCache8 <> nil) and (fWindowMinCache8 <> fWindowMaxCache8) and (fWindowMin = fWindowMinCache8) and (fWindowMax = fWindowMaxCache8) then begin
     exit(fCache8);
  vol8 := fRawVolBytes;
  skipVx := SkipVox();
  //if (IsLabels()) and  (not specialsingle(fWindowMinCache8)) then begin //handle 2 volume TTatlas+tlrc.HEAD
  if (IsLabels())  then begin
    clut.GenerateLUT(abs(fWindowMax-fWindowMin)/100, fOpacityPct);
    result := fCache8;
    if specialsingle(fWindowMinCache8) then begin
       DisplayLabel2Uint8();
    end;
    fWindowMinCache8 := fWindowMin;
    fWindowMaxCache8 := fWindowMax;
    fOpacityPctCache8 := fOpacityPct;
    clut.NeedsUpdate := true; //<- 20200606 CRUCIAL CHANGE
    inc(RefreshCount);
    exit;
  end;
  setlength(fCache8, vx);
  {$IFDEF PARALLEL16}
  if (vx > (kMaxParallel*10)) and (kMaxParallel > 1) then begin
     ProcThreadPool.DoParallel(@SetDisplayMinMaxParallel,0,kMaxParallel-1,nil);
  end else {$ENDIF} begin
    lMin := Scaled2RawIntensity(fHdr, fWindowMin);
    lMax := Scaled2RawIntensity(fHdr, fWindowMax);
    vol16 := TInt16s(vol8);
    vol32 := TFloat32s(vol8);
    if fHdr.datatype = kDT_UINT8 then begin
      lRng := (lMax - lMin);
      if lRng <> 0 then
       lMod := abs((((254)/lRng)))
      else
        lMod := 0;
      if lMin > lMax then begin
           lSwap := lMin;
           lMin := lMax;
           lMax := lSwap;
      end;
      for i := 0 to 255 do begin
        if i <= lMin then
          luts[i] := 0
        else if i >= lMax then
            luts[i] := 255
        else
           luts[i] := trunc(((i-lMin)*lMod)+1);
      end;
      for i := 0 to (vx - 1) do
          fCache8[i] := luts[vol8[skipVx+i]];
    {$IFDEF SSE}
    end else if fHdr.datatype = kDT_INT16 then begin
    	//int2byte(vol16, fCache8, lMin, lMax, skipVx);
        int2byte(lMin, lMax, vol16, fCache8, skipVx);
    {$ELSE}
    {$IFDEF LUT16}
    end else if (fHdr.datatype = kDT_INT16) and (true) and (vx > (10 * kMaxWord)) then begin
        slope := 255.0/(lMax - lMin);
    	setlength(luts16, kMaxWord + 1); // 0..65535
        for i := 0 to kMaxWord do begin
          j := i - kMin16;
          if (j >= lMax) then
             luts16[i] := 255
          else if  (j <= lMin) then
             luts16[i] := 0
          else
             luts16[i] := round((j - lMin) * slope);
        end;
        for i := 0 to (vx - 1) do
            fCache8[i] := luts16[vol16[skipVx+i] + kMin16];
        luts16 := nil;
    {$ENDIF} //LUT16
    end else if fHdr.datatype = kDT_INT16 then begin
        slope := 255.0/(lMax - lMin);
    	for i := 0 to (vx - 1) do begin
          if (vol16[skipVx+i] >= lMax) then
             fCache8[i] := 255
          else if  (vol16[skipVx+i] <= lMin) then
             fCache8[i] := 0
          else
             fCache8[i] := round((vol16[skipVx+i] - lMin) * slope);
        end;
    {$ENDIF} //SSE
    end else if fHdr.datatype = kDT_FLOAT then begin
      {$IFDEF SSE} //54ms->29ms
      //float2byte(vol32, fCache8, lMin, lMax, skipVx);
      flt2byte(vol32, fCache8, lMin, lMax, skipVx);
      {$ELSE}
        for i := 0 to (vx - 1) do begin
          if (vol32[skipVx+i] >= lMax) then
             fCache8[i] := 255
          else if  (vol32[skipVx+i] <= lMin) then
              fCache8[i] := 0
          else
             fCache8[i] := round((vol32[skipVx+i] - lMin) * slope);
        end;
      {$ENDIF}
    end;
    if ZeroIntensityInvisible then begin
       for i := 0 to (vx - 1) do
           if (fCache8[i] = 0) then
              fCache8[i] := 1;
       zero32 := fHdr.scl_slope;
       if (zero32 = 0) then zero32 := 1;
       zero32 := -fHdr.scl_inter/zero32;
       if fHdr.datatype = kDT_UINT8 then begin
         zero8 := round(zero32);
         for i := 0 to (vx - 1) do
             if (vol8[skipVx+i] = zero8) then
                fCache8[i] := 0;
       end else if fHdr.datatype = kDT_INT16 then begin
         zero16 := round(zero32);
         for i := 0 to (vx - 1) do
             if (vol16[skipVx+i] = zero16) then
                fCache8[i] := 0;
       end else if fHdr.datatype = kDT_FLOAT then begin

           for i := 0 to (vx - 1) do
               if (vol32[skipVx+i] = zero32) then
                  fCache8[i] := 0;
       end;
    end;
  end; //parallel
  result := fCache8;
  fWindowMinCache8 := fWindowMin;
  fWindowMaxCache8 := fWindowMax;
  fOpacityPctCache8 := fOpacityPct;
  clut.NeedsUpdate := false;
  inc(RefreshCount);
  ApplyCutout(); //run twice! also in SetDisplayMinMax
end;

{$IFDEF PARALLEL}
(*const kMaxParallel = 6;

procedure TNIfTI.SetDisplayMinMaxParallel(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
//Compute transfer function across cores
//http://wiki.lazarus.freepascal.org/Parallel_procedures
//Remarkably little benefit, perhaps because
// "Do not work on vast amounts of memory. On some systems one thread alone is fast enough to fill the memory bus speed."
var
  i, vx, vxLo, vxHi: int64;
begin
 vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
 vxLo := round((index/kMaxParallel) * vx);
 vxHi := round(((index+1)/kMaxParallel) * vx)-1;
 for i := vxLo to vxHi do
  {$IFDEF CUSTOMCOLORS}
  fVolRGBA[i] := clut.LUT[fCache8[i]];
  {$ELSE}
  fVolRGBA[i] := fLUT[fCache8[i]];
  {$ENDIF}
  printf(format('>%d..%d %d',[vxLo, vxHi, index]));
end;
*)
{$ENDIF}
procedure TNIfTI.SetDisplayMinMax(isForceRefresh: boolean = false); overload;
{$DEFINE UNROLL}
{$IFDEF PARALLEL}
		{$DEFINE PARA}
        {$IFDEF DYNRGBA}parallel does not help when dynrgba {$ENDIF}
{$ENDIF}
type
  TLUTi = array [0..255] of Int32; //Color Lookup Table
  {$IFNDEF DYNRGBA}
  TInt320 = array [0..MAXINT] of int32;
  TInt32p = ^TInt320; //pointer to RGBA array
  {$ENDIF}
var
  {$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
  i, vx: int64;
  {$IFDEF UNROLL}
  vx8: int64;
  lut: TLUTi;
  {$IFDEF DYNRGBA}
  rgba: TInt32s;
  {$ELSE}
  rgba: TInt32p;
  {$ENDIF}


  {$ENDIF}
//{$DEFINE PARA}   No benefit from parallel threads
 {$IFDEF PARA}
 tPerThread: int64;
 MaxThreads: integer = 0;
 {$ENDIF}
{$IFDEF PARA}
procedure SubProc(ThreadIndex: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
var
	j, tStart, tEnd: int64;
begin
	tStart := (ThreadIndex * tPerThread);
	if (tStart >= vx) then exit; //more threads than slices in Z direction
	tEnd := tStart + tPerThread - 1; //e.g. if zStart=4 and zPerThread=1 then zEnd=4
	tEnd := min(tEnd, vx-1); //final thread when slices in Z not evenly divisible by number of threads
        for j := tStart to tEnd do
        {$IFDEF DYNRGBA}
            fVolRGBA[j] := clut.LUT[fCache8[j]];
        {$ELSE}
        fVolRGBA^[j] := clut.LUT[fCache8[j]];
        {$ENDIF}
end; //SubProc()
{$ENDIF}


begin
 {$IFDEF TIMER}startTime := now;{$ENDIF}
  if (fIsOverlay) then exit; //overlays visualized using DisplayMinMax2Uint8
  if (not isForceRefresh) and (not NeedsUpdate) then //(not clut.NeedsUpdate) and (fHdr.datatype <> kDT_RGB) and (fCache8 <> nil) and (fWindowMinCache8 <> fWindowMaxCache8) and (fWindowMin = fWindowMinCache8) and (fWindowMax = fWindowMaxCache8) then
     exit; //no change
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
  if (vx < 1) then exit;
  if fHdr.datatype = kDT_RGB then begin
    {$IFDEF DYNRGBA}
    setlength(fVolRGBA, vx);
    {$ELSE}
    setlengthp(fVolRGBA, vx);
    {$ENDIF}
    SetDisplayMinMaxRGB24();
    ApplyCutout();
    {$IFDEF TIMER}printf(format('Set Min/Max Window RGB time %d', [ MilliSecondsBetween(Now,startTime)]));{$ENDIF}
    exit;
  end;
  (*if (fDTImode = kDTIrgb) and (fHdr.datatype = kDT_FLOAT32) and (fVolumesLoaded = 3) then begin
    {$IFDEF DYNRGBA}
    setlength(fVolRGBA, vx);
    {$ELSE}
    setlengthp(fVolRGBA, vx);
    {$ENDIF}
    SetDisplayMinMaxRGBV1();
    ApplyCutout();
    {$IFDEF TIMER}printf(format('Set Min/Max Window V1 time %d', [ MilliSecondsBetween(Now,startTime)]));{$ENDIF}
    exit;
  end;  *)
  DisplayMinMax2Uint8(isForceRefresh);
  {$IFDEF TIMER}
  printf(format('Set Min/Max Window %g..%g time %d', [fWindowMin, fWindowMax, MilliSecondsBetween(Now,startTime)]));
  startTime := now;
  {$ENDIF}
  //https://alt.comp.lang.borland-delphi.narkive.com/O0jRrNgS/speed-problem-with-setlength
  {$IFDEF DYNRGBA}
  setlength(fVolRGBA, vx); //TODO: this takes the most time
  {$ELSE}
  setlengthP(fVolRGBA, vx); //TODO: this takes the most time
  {$ENDIF}
  {$IFDEF PARALLEL} //test to see if CMem is faster for setlength()
  //{$IFDEF TIMER}printf(format('PARALLEL Update RGBA time %d', [MilliSecondsBetween(Now,startTime)]));{$ENDIF}
  {$ELSE}
  //{$IFDEF TIMER}printf(format('SERIAL Update RGBA time %d', [MilliSecondsBetween(Now,startTime)]));{$ENDIF}
  {$ENDIF}
  {$IFDEF PARA}
 if (MaxThreads < 1) then MaxThreads :=  GetSystemThreadCount();
 printf('Threads'+inttostr(MaxThreads));
 tPerThread := ceil(vx/MaxThreads);
 ProcThreadPool.DoParallelNested(@SubProc,0,MaxThreads-1, nil, MaxThreads);

  //for i := 0 to (vx - 1) do
  //   fVolRGBA[i] := clut.LUT[fCache8[i]];
 {$ELSE} //PARA
  {$IFDEF UNROLL}
  //~20% faster
  lut := TLUTi(clut.LUT);
 {$IFDEF DYNRGBA}
  rgba := TInt32s(fVolRGBA);
 vx8 := ((vx div 8) * 8);
 i := 0;
 while i < vx8 do begin
   rgba[i+0] := lut[fCache8[i+0]];
   rgba[i+1] := lut[fCache8[i+1]];
   rgba[i+2] := lut[fCache8[i+2]];
   rgba[i+3] := lut[fCache8[i+3]];
   rgba[i+4] := lut[fCache8[i+4]];
   rgba[i+5] := lut[fCache8[i+5]];
   rgba[i+6] := lut[fCache8[i+6]];
   rgba[i+7] := lut[fCache8[i+7]];
   i := i + 8;
 end;
 //tail - for volumes not evenly divisible by 8
 while i < (vx) do begin
       rgba[i] := lut[fCache8[i]];
       i := i + 1;
 end;
 {$ELSE}
 rgba := TInt32p(fVolRGBA);
 vx8 := ((vx div 8) * 8);
 i := 0;
 while i < vx8 do begin
   rgba^[i+0] := lut[fCache8[i+0]];
   rgba^[i+1] := lut[fCache8[i+1]];
   rgba^[i+2] := lut[fCache8[i+2]];
   rgba^[i+3] := lut[fCache8[i+3]];
   rgba^[i+4] := lut[fCache8[i+4]];
   rgba^[i+5] := lut[fCache8[i+5]];
   rgba^[i+6] := lut[fCache8[i+6]];
   rgba^[i+7] := lut[fCache8[i+7]];
   i := i + 8;
 end;
 //tail - for volumes not evenly vivisible by 8
 while i < (vx) do begin
       rgba^[i] := lut[fCache8[i]];
       i := i + 1;
 end;
 {$ENDIF}

  {$ELSE}
  //next stage remarkably slow, even though just lookup table.
  for i := 0 to (vx - 1) do
     {$IFDEF CUSTOMCOLORS}
     fVolRGBA[i] := clut.LUT[fCache8[i]];
     {$ELSE}
     fVolRGBA[i] := fLUT[fCache8[i]];
     {$ENDIF}
  {$ENDIF}
 {$ENDIF} //parap
  ApplyCutout();
  {$IFDEF TIMER}printf(format('Update RGBA time %d', [MilliSecondsBetween(Now,startTime)]));{$ENDIF}
end;

(*procedure TNIfTI.SetDisplayMinMax(isForceRefresh: boolean = false); overload;
var
  {$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
  i, vx: int64;
begin
 {$IFDEF TIMER}startTime := now;{$ENDIF}
  if (fIsOverlay) then exit; //overlays visualized using DisplayMinMax2Uint8
  if (not isForceRefresh) and (not NeedsUpdate) then //(not clut.NeedsUpdate) and (fHdr.datatype <> kDT_RGB) and (fCache8 <> nil) and (fWindowMinCache8 <> fWindowMaxCache8) and (fWindowMin = fWindowMinCache8) and (fWindowMax = fWindowMaxCache8) then
     exit; //no change
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
  if (vx < 1) then exit;
  if fHdr.datatype = kDT_RGB then begin
    setlength(fVolRGBA, vx);
    SetDisplayMinMaxRGB24();
    ApplyCutout();
    {$IFDEF TIMER}printf(format('Set Min/Max Window RGB time %d', [ MilliSecondsBetween(Now,startTime)]));{$ENDIF}
    exit;
  end;
  DisplayMinMax2Uint8(isForceRefresh);
  {$IFDEF TIMER}
  printf(format('Set Min/Max Window %g..%g time %d', [fWindowMin, fWindowMax, MilliSecondsBetween(Now,startTime)]));
  startTime := now;
  {$ENDIF}
  setlength(fVolRGBA, vx);
  {$IFDEF PARALLEL}
  ProcThreadPool.DoParallel(@SetDisplayMinMaxParallel,0,kMaxParallel-1,nil);
  {$ELSE}
  //next stage remarkably slow, even though just lookup table.
  for i := 0 to (vx - 1) do
     {$IFDEF CUSTOMCOLORS}
     fVolRGBA[i] := clut.LUT[fCache8[i]];
     {$ELSE}
     fVolRGBA[i] := fLUT[fCache8[i]];
     {$ENDIF}
  {$ENDIF}
  ApplyCutout();
  {$IFDEF TIMER}printf(format('Update RGBA time %d', [MilliSecondsBetween(Now,startTime)]));{$ENDIF}
end;   *)

procedure TNIfTI.SetCutoutNoUpdate(CutoutLow, CutoutHigh: TVec3);
begin
     fCutoutLow := CutoutLow;
     fCutoutHigh := CutoutHigh;
     {$IFDEF CACHEUINT8}
      fWindowMinCache8 := infinity;
     {$ENDIF}
end;

procedure TNIfTI.SetDisplayVolume(newDisplayVolume: integer);
var
  n: integer;
begin
   if (fIsOverlay) then
      n := HdrVolumes(fHdr)
   else
      n := fVolumesLoaded;
  if (newDisplayVolume < 0) then
     newDisplayVolume := n - 1;
  if (newDisplayVolume >= n)  then
     newDisplayVolume := 0;
  if newDisplayVolume = fVolumeDisplayed  then exit;
  fVolumeDisplayed := newDisplayVolume;
  if not fIsOverlay then begin
     {$IFDEF CACHEUINT8} //force refresh
     fWindowMinCache8 := infinity;
     {$ENDIF}
     SetDisplayMinMax(); //refresh RGBA
     exit;
  end;
  //following for overlays, which we load one at a time, and often need to reslice...
  //close old image
  fLabels.clear;
  fLabelMask := nil;
  fCache8 := nil;
  {$IFDEF CUSTOMCOLORS}
  //clut := nil;
  {$ENDIF}
  //Load(fFileName, fMat, fDim, fisLinearReslice, fVolumeDisplayed, true); //isKeepContrast
  Load(fFileName, fMat, fDim, fisLinearReslice, fVolumeDisplayed, false); //isKeepContrast
  //Load(niftiFileName, tarMat, tarDim, isInterpolate, hdr, img)
end;

procedure TNIfTI.SetDisplayColorScheme(clutFileName: string; cTag: integer);
begin
 {$IFDEF CUSTOMCOLORS}
 CLUT.OpenCLUT(clutFileName, cTag);
 if (CLUT.SuggestedMinIntensity < CLUT.SuggestedMaxIntensity) and (fMax > CLUT.SuggestedMinIntensity) and (fMin < CLUT.SuggestedMaxIntensity) then begin
    fWindowMin := CLUT.SuggestedMinIntensity;
    fWindowMax := CLUT.SuggestedMaxIntensity;
 end;
 {$IFDEF CACHEUINT8} //force refresh
  fWindowMinCache8 := infinity;
 {$ENDIF}
 SetDisplayMinMax(); //refresh RGBA
 {$ENDIF}
end;

procedure TNIfTI.SetDisplayMinMaxNoUpdate(newMin, newMax: single); overload;
begin
 if newMin > newMax then begin
    fWindowMin := newMax;
    fWindowMax := newMin;
 end else begin
     fWindowMin := newMin;
     fWindowMax := newMax;
 end;
end;

procedure TNIfTI.SetDisplayMinMax(newMin, newMax: single); overload;
begin
 SetDisplayMinMaxNoUpdate(newMin, newMax);
 SetDisplayMinMax;
end;

{$IFDEF GZIP}
function LoadImgGZ(FileName : AnsiString; swapEndian: boolean; var  rawData: TUInt8s; var lHdr: TNIFTIHdr): boolean;
//foreign: both image and header compressed
var
  Stream: TGZFileStream;
  StreamSize: int64;
  crc32: dword;
  volBytes, offset: int64;
begin
 result := false;
 StreamSize := 0; //unknown
 if lHdr.vox_offset < 0 then begin
   //byteskip = -1
   // this is expressly forbidden in the NRRD specification
   // " skip can be -1. This is valid only with raw encoding"
   // we handle it here because these images are seen in practice
   GetCompressedFileInfo(FileName, StreamSize, crc32);
 end;
 Stream := TGZFileStream.Create (FileName, gzopenread);
 Try
  if (lHdr.bitpix <> 8) and (lHdr.bitpix <> 16) and (lHdr.bitpix <> 24) and (lHdr.bitpix <> 32) and (lHdr.bitpix <> 64)  and (lHdr.bitpix <> 128) then begin
   printf('Unable to load '+Filename+' - this software can only read 8,16,24,32,64,128-bit NIfTI files. (bitpix: '+inttostr(lHdr.bitpix)+' datatype: '+inttostr(lHdr.datatype)+ ')');
   exit;
  end;
  //read the image data
  volBytes := lHdr.Dim[1]*lHdr.Dim[2]*lHdr.Dim[3] * (lHdr.bitpix div 8);
  if HdrVolumes(lHdr) > 1 then
    volBytes := volBytes * HdrVolumes(lHdr);
  offset := round(lHdr.vox_offset);
  if lHdr.vox_offset < 0 then begin
     offset := StreamSize-volBytes;
     if offset < 0 then
       offset := 0; //report sensible error
  end;
  if ((offset+volBytes) < StreamSize) then begin
    printf(format('Uncompressed file too small: expected %d got %d: %s', [offset+volBytes , StreamSize , Filename]));
    exit;
  end;
  Stream.Seek(offset,soFromBeginning);
  SetLength (rawData, volBytes);
  Stream.ReadBuffer (rawData[0], volBytes);
  if swapEndian then
   SwapImg(rawData, lHdr.bitpix);
 Finally
  Stream.Free;
 End;
 result := true;
end;


function LoadHdrRawImgGZ(FileName : AnsiString; swapEndian: boolean; var  rawData: TUInt8s; var lHdr: TNIFTIHdr): boolean;
var
//20220827: FastGZ fix for `CTA-cardio.nrrd`
   {$IFNDEF FASTGZ}
   fStream: TFileStream;
   inStream: TMemoryStream;
   {$ENDIF}
   volBytes: int64;
   outStream : TMemoryStream;
label
     123;
begin
 result := false;
 if not fileexists(Filename) then exit;
 if (lHdr.bitpix <> 8) and (lHdr.bitpix <> 16) and (lHdr.bitpix <> 24) and (lHdr.bitpix <> 32) and (lHdr.bitpix <> 64)  and (lHdr.bitpix <> 128) then begin
   printf('Unable to load '+Filename+' - this software can only read 8,16,24,32,64,128-bit NIfTI files. (bitpix: '+inttostr(lHdr.bitpix)+' datatype: '+inttostr(lHdr.datatype)+ ')');
   exit;
 end;
 volBytes := lHdr.Dim[1]*lHdr.Dim[2]*lHdr.Dim[3] * (lHdr.bitpix div 8);
 volBytes := volBytes * HdrVolumes(lHdr);
 outStream := TMemoryStream.Create();
 {$IFDEF FASTGZ}
 result := ExtractGzNoCrc(Filename, outStream, round(lHdr.vox_offset), volBytes);
 {$ELSE}
 fStream := TFileStream.Create(Filename, fmOpenRead);
 fStream.seek(round(lHdr.vox_offset), soFromBeginning);
 inStream := TMemoryStream.Create();
 inStream.CopyFrom(fStream, fStream.Size - round(lHdr.vox_offset));
 result := unzipStream(inStream, outStream);
 fStream.Free;
 inStream.Free;
 if (not result) and (volBytes >=outStream.size) then begin
   printf('unzipStream error but sufficient bytes extracted (perhaps GZ without length in footer)');
   result := true;
 end;
 {$ENDIF}
 if not result then goto 123;
 //showmessage(format('%g  %dx%dx%dx%d ~= %d',[lHdr.vox_offset, lHdr.dim[1],lHdr.dim[2],lHdr.dim[3],lHdr.dim[4], HdrVolumes(lHdr) ]));
 if outStream.Size < volBytes then begin
    result := false;
    showmessage(format('GZ error expected %d found %d bytes (offset %d): %s',[volBytes,outStream.Size, round(lHdr.vox_offset), Filename]));
    goto 123;
 end;
 SetLength (rawData, volBytes);
 outStream.Position := 0;
 outStream.ReadBuffer (rawData[0], volBytes);
 if swapEndian then
   SwapImg(rawData, lHdr.bitpix);
 123:
   outStream.Free;
end;

function TNIfTI.LoadGZ(FileName : AnsiString; out isNativeEndian: boolean): boolean;
//FSL compressed nii.gz file
var
  Stream: TGZFileStream;
  volBytes: int64;
  lSwappedReportedSz : LongInt;
  isFastFailed: boolean = false;
begin
 isNativeEndian := true;
 result := false;
 if not fileexists(FileName) then exit;
 {$IFDEF FASTGZ}
 volBytes := FSize(fFilename);
 if (volBytes < 104857600) then begin // < 100mb, unlikely to trigger LoadFewVolumes
    //FastGz uncompresses entire volume in one step: much faster
    // disadvantage: for very large files we might not want to decompress entire volume
    result := LoadFastGz(FileName,isNativeEndian);
    if result then exit;
    isFastFailed := true;
 end;
 {$ENDIF}
 Stream := TGZFileStream.Create (FileName, gzopenread);
 //Try
  {$warn 5058 off}Stream.ReadBuffer (fHdr, SizeOf (TNIFTIHdr));{$warn 5058 on}
  lSwappedReportedSz := fHdr.HdrSz;
  swap4(lSwappedReportedSz);
  if (lSwappedReportedSz = SizeOf (TNIFTIHdr)) then begin
    NIFTIhdr_SwapBytes(fHdr);
     isNativeEndian := false;
  end;
  if fHdr.HdrSz <> SizeOf (TNIFTIHdr) then begin
    fHdr := Nifti2to1(Stream, isNativeEndian);
    if fHdr.HdrSz <> SizeOf (TNIFTIHdr) then begin
       printf('Unable to read image '+Filename);
       //Stream.Free; //Finally ALWAYS executed!
       exit;
    end;
  end;
  fixHdr(fHdr);
  if (fHdr.bitpix <> 8) and (fHdr.bitpix <> 16) and (fHdr.bitpix <> 24) and (fHdr.bitpix <> 32) and (fHdr.bitpix <> 64)  and (fHdr.bitpix <> 128) then begin
   printf('Unable to load '+Filename+' - this software can only read 8,16,24,32,64,128-bit NIfTI files. (bitpix: '+inttostr(fHdr.bitpix)+' datatype: '+inttostr(fHdr.datatype)+ ')');
   exit;
  end;
  Quat2Mat(fHdr);
  //read the image data
  volBytes := fHdr.Dim[1]*fHdr.Dim[2]*fHdr.Dim[3]* (fHdr.bitpix div 8);
  fVolumesLoaded := max(HdrVolumes(fHdr),1);
  fVolumesTotal :=  fVolumesLoaded;
  DetectV1();
  if (fVolumesLoaded = 3) and ((fHdr.intent_code = kNIFTI_INTENT_RGB_VECTOR) or (fHdr.intent_code = kNIFTI_INTENT_VECTOR)) and (fHdr.datatype = kDT_FLOAT32) then
     volBytes := volBytes * fVolumesLoaded
  else if (HdrVolumes(fHdr) > 1) and (not fIsOverlay) then begin
     if LoadFewVolumes then
       //fVolumesLoaded :=  trunc((16777216.0 * 4) /volBytes)
        fVolumesLoaded :=  max(trunc((kLoadFewVolumesBytes) /volBytes), kLoadFewVolumesN)
     else
         fVolumesLoaded :=  trunc(2147483647.0 /volBytes);
     if fVolumesLoaded < 1 then fVolumesLoaded := 1;
     if fVolumesLoaded > HdrVolumes(fHdr) then fVolumesLoaded := HdrVolumes(fHdr);
     volBytes := volBytes * fVolumesLoaded;
  end;
  {$IFDEF FASTGZ}
  if (fVolumesLoaded = fVolumesTotal) and (not isFastFailed) then begin // did not trigger LoadFewVolumes
     Stream.Free;
     result := LoadFastGz(FileName,isNativeEndian);
     if result then exit;
     Stream := TGZFileStream.Create (FileName, gzopenread);
  end;
  {$ENDIF}
  if  (fVolumeDisplayed > 0) and (fVolumeDisplayed < HdrVolumes(fHdr)) and (fIsOverlay) then begin
      Stream.Seek(round(fHdr.vox_offset)+(fVolumeDisplayed*volBytes),soFromBeginning);
  end else begin
      fVolumeDisplayed := 0;
      Stream.Seek(round(fHdr.vox_offset),soFromBeginning);
  end;
  SetLength (fRawVolBytes, volBytes);
  try
  Stream.ReadBuffer (fRawVolBytes[0], volBytes);
  except
    printf('GZ error: image corrupted?');
    Stream.Free;
    fRawVolBytes := nil;
    showmessage('GZ error: image corrupted?');
    exit;
  end;

  if not isNativeEndian then
   SwapImg(fRawVolBytes, fHdr.bitpix);
 //Finally
  Stream.Free;
 //End; { Try }
 result := true;
end;

{$ENDIF}// GZIP

procedure DimPermute2341(var  rawData: TUInt8s; var lHdr: TNIFTIHdr);
//NIfTI demands first three dimensions are spatial, NRRD often makes first dimension non-spatial (e.g. DWI direction)
// This function converts NRRD TXYZ to NIfTI compatible XYZT
var
   i, x,y,z,t,xInc,yInc,zInc,tInc, nbytes: int64;
   i8, o8: TUint8s;
   i16, o16: TUInt16s;
   i32, o32: TUInt32s;
begin
     if HdrVolumes(lHdr) < 2 then exit;
     if (lHdr.bitpix mod 8) <> 0 then exit;
     if (lHdr.bitpix <> 8) and (lHdr.bitpix <> 16) and (lHdr.bitpix <> 32) then exit;
     nbytes := lHdr.Dim[1] * lHdr.Dim[2] * lHdr.Dim[3] * HdrVolumes(lHdr) * (lHdr.bitpix div 8);
     if nbytes < 4 then exit;
     //zz setlength(i8, nbytes);
     i8 := copy(rawData, low(rawData), high(rawData));
     i16 := TUInt16s(i8);
     i32 := TUInt32s(i8);
     o8 := TUInt8s(rawData);
     o16 := TUInt16s(rawData);
     o32 := TUInt32s(rawData);
     t :=  lHdr.Dim[1];
     x :=  lHdr.Dim[2];
     y :=  lHdr.Dim[3];
     z :=  HdrVolumes(lHdr);
     lHdr.Dim[1] := x;
     lHdr.Dim[2] := y;
     lHdr.Dim[3] := z;
     lHdr.Dim[4] := t;
     lHdr.Dim[5] := 1;
     lHdr.Dim[6] := 1;
     lHdr.Dim[7] := 1;
     tInc := 1;
     xInc := t;
     yInc := t * x;
     zInc := t * x * y;
     i := 0;
     if (lHdr.bitpix = 8) then
        for t := 0 to (lHdr.Dim[4]-1) do
            for z := 0 to (lHdr.Dim[3]-1) do
                for y := 0 to (lHdr.Dim[2]-1) do
                    for x := 0 to (lHdr.Dim[1]-1) do begin
                        o8[i] := i8[(x*xInc)+(y*yInc)+(z*zInc)+(t*tInc)];
                        i := i + 1;
                    end;
     if (lHdr.bitpix = 16) then
        for t := 0 to (lHdr.Dim[4]-1) do
            for z := 0 to (lHdr.Dim[3]-1) do
                for y := 0 to (lHdr.Dim[2]-1) do
                    for x := 0 to (lHdr.Dim[1]-1) do begin
                        o16[i] := i16[(x*xInc)+(y*yInc)+(z*zInc)+(t*tInc)];
                        i := i + 1;
                    end;
     if (lHdr.bitpix = 32) then
        for t := 0 to (lHdr.Dim[4]-1) do
            for z := 0 to (lHdr.Dim[3]-1) do
                for y := 0 to (lHdr.Dim[2]-1) do
                    for x := 0 to (lHdr.Dim[1]-1) do begin
                        o32[i] := i32[(x*xInc)+(y*yInc)+(z*zInc)+(t*tInc)];
                        i := i + 1;
                    end;
     i8 := nil;
end;

{$IFDEF BMP}
//{$IFDEF UNIX}
(*function xRGB(i: TRGB): TRGB;
begin
     result.r := i.B;
     result.g := i.G;
     result.b := i.R;
end;*)

function xRGBA(i: TRGBA): TRGBA;
begin
     result.r := i.B;
     result.g := i.G;
     result.b := i.R;
     result.a := i.a;
end;

(*function xYCbCR(i: TRGBA): TRGBA;
var
   Y,Cr,Cb: byte;
begin
 //https://en.wikipedia.org/wiki/YCbCr
 Y := i.B;
 Cr := i.G;
 Cb := i.R;
 result.r := round(Y + 1.402 * (Cr-128));
 result.g := round(Y - 0.344136 * (Cb-128) - 0.714136 * (Cr-128) );
 result.b := round(Y + 1.772 * (Cb-128));
end;*)
//{$ENDIF}

function LoadBmpAsNifti(fnm: string; var  rawData: TUInt8s; var nhdr: TNIFTIHdr): boolean;
var
   pic : TPicture;
   RawImage: TRawImage;
   RowPtr, SrcPtr, DestPtr: PInteger;
   n, x, y,i,nBytes: integer;
   //LTempBitmap : TBitmap;
   Row32: TUInt8s;
   //{$IFDEF UNIX}
   //v24s: TRGBs;
   v32s: TRGBAs;
   //{$ENDIF}
begin
 result := false;
 pic := TPicture.create;
 //pic.Bitmap.PixelFormat := pf24bit;
 try
    pic.Loadfromfile(fnm);
 except
   pic.free;
   exit;
 end;
 NII_Clear(nhdr);
  (*if pic.Bitmap.PixelFormat = pf32bit then begin
    //https://forum.lazarus.freepascal.org/index.php?topic=29701.0
    LTempBitmap := TBitmap.Create;
    try
      LTempBitmap.PixelFormat := pf24bit;
      LTempBitmap.SetSize(pic.Bitmap.Width, pic.Bitmap.Height);
      LTempBitmap.Canvas.Draw(0, 0, pic.Bitmap);
      pic.Bitmap.PixelFormat := pf24bit;
      pic.Bitmap.Canvas.Draw(0, 0, LTempBitmap);
    finally
      FreeAndNil(LTempBitmap);
    end;
  end; *)
     (*if pic.Bitmap.PixelFormat = pf24bit then begin
         //https://forum.lazarus.freepascal.org/index.php?topic=29701.0
         LTempBitmap := TBitmap.Create;
         try
           LTempBitmap.PixelFormat := pf24bit;
           LTempBitmap.SetSize(pic.Bitmap.Width, pic.Bitmap.Height);
           LTempBitmap.Canvas.Draw(0, 0, pic.Bitmap);
           pic.Bitmap.PixelFormat := pf32bit;
           pic.Bitmap.Canvas.Draw(0, 0, LTempBitmap);
         finally
           FreeAndNil(LTempBitmap);
         end;
       end;*)
  //Assert( pic.Bitmap.PixelFormat = pf24bit); //<- does nothing!
  //Assert( pic.Bitmap.PixelFormat = pf32bit); //<- does nothing!
 case pic.Bitmap.PixelFormat of
     pf8bit: nhdr.bitpix:=8;
     pf16bit: nhdr.bitpix:=16;
     pf24bit: nhdr.bitpix:=24;
     pf32bit: nhdr.bitpix:=32;
     else begin
      pic.free;
      exit;
     end;
 end;
 case pic.Bitmap.PixelFormat of
   pf8bit: nhdr.datatype:=kDT_UINT8;
   pf16bit: nhdr.datatype:=kDT_UINT16;
   pf24bit: nhdr.datatype:=kDT_RGB;
   pf32bit: nhdr.datatype:=kDT_RGBA32;
 end;
 nhdr.dim[0] := 2;
 nhdr.dim[1] := pic.Width;
 nhdr.dim[2] := pic.Height;
 nhdr.dim[3] := 1;
 nBytes :=  nhdr.dim[1] * nhdr.dim[2] * (nhdr.bitpix div 8);
 setlength(rawData, nBytes);
 //https://wiki.freepascal.org/Fast_direct_pixel_access
 RawImage := pic.Bitmap.RawImage;
 SrcPtr := PInteger(RawImage.Data);
 {$DEFINE FLIPBMP}
 {$IFDEF FLIPBMP}
 Inc(PByte(SrcPtr), (nhdr.dim[2] - 1) * RawImage.Description.BytesPerLine);
 {$ENDIF}
 DestPtr := PInteger(@rawData[0]);
 nBytes :=  nhdr.dim[1] * (nhdr.bitpix div 8);
 if nhdr.bitpix = 24 then begin
    nBytes :=  nhdr.dim[1] * 4;
    SetLength (Row32, nBytes);
    RowPtr := PInteger(@Row32[0]);
    n := 0;
    for y := 0 to nhdr.dim[2] - 1 do begin
        System.Move(SrcPtr^, RowPtr^, nBytes);
        {$IFDEF FLIPBMP}
        Dec(PByte(SrcPtr), RawImage.Description.BytesPerLine);
        {$ELSE}
        Inc(PByte(SrcPtr), RawImage.Description.BytesPerLine);
        {$ENDIF}
        i := 0;
        for x := 1 to nhdr.dim[1] do begin
            rawData[n+2] := Row32[i];
            //n += 1;
            i += 1;
            rawData[n+1] := Row32[i];
            //n += 1;
            i += 1;
            rawData[n] := Row32[i];
            //n += 1;
            n += 3;
            i += 1;
            i += 1;
        end;
    end;
    Row32 := nil;
    nBytes :=  nhdr.dim[1] * (nhdr.bitpix div 8);
 end else begin
   for y := 0 to nhdr.dim[2] - 1 do begin
       System.Move(SrcPtr^, DestPtr^, nBytes);
       {$IFDEF FLIPBMP}
       Dec(PByte(SrcPtr), RawImage.Description.BytesPerLine);
       {$ELSE}
       Inc(PByte(SrcPtr), RawImage.Description.BytesPerLine);
       {$ENDIF}
       Inc(PByte(DestPtr), nBytes);
   end;
 end;
 //{$IFDEF UNIX} //MacOS RGBA order
 (*printf('Import '+inttostr(nhdr.bitpix)+'-bit bitmap');
 if nhdr.bitpix = 24 then begin
    nBytes := nhdr.dim[1] * nhdr.dim[2];
    v24s := TRGBs(rawData);
    for i := 0 to (nBytes-1) do
        v24s[i] := xRGB(v24s[i]);
 end;*)
 if nhdr.bitpix = 32 then begin
    nBytes := nhdr.dim[1] * nhdr.dim[2];
    v32s := TRGBAs(rawData);
    for i := 0 to (nBytes-1) do
        v32s[i] := xRGBA(v32s[i]);
        //v32s[i] := xYCbCR(v32s[i]);
 end;
 //{$ENDIF}
 result := true;
 pic.free;
end;

{$ENDIF}

(*procedure planar2RGB8(var  rawData: TUInt8s; var lHdr: TNIFTIHdr);
var
   img: TUInt8s;
   xy, xys, z, i, j, k,  s, nBytes, SamplesPerPixel: integer;
begin
  if lHdr.datatype <> kDT_RGBplanar then exit;
  lHdr.datatype := kDT_RGB;
  SamplesPerPixel := 3;
  xy := lHdr.dim[1] * lHdr.dim[2];
  xys := xy *  SamplesPerPixel;
  nBytes := xys * lHdr.dim[3] ;
  setlength(img, nBytes);
  img := Copy(rawData, Low(rawData), Length(rawData));
  j := 0;
  for z := 0 to (lHdr.dim[3]-1) do begin
      k := z * xys;
      for i := 0 to (xy-1) do begin
          for s := 0 to (SamplesPerPixel-1) do begin
              rawData[j] := img[k+i+(s * xy)] ;
              j := j + 1;
          end;
          //
      end;
  end;
  img := nil;
end; *)

procedure planar3D2RGB8(var  rawData: TUInt8s; var lHdr: TNIFTIHdr);
var
   img: TUInt8s;
   xy, xys, i, j, s, xyz, nBytesS, SamplesPerPixel: int64;
begin
  if lHdr.datatype <> kDT_RGBplanar3D then exit;
  lHdr.datatype := kDT_RGB;
  SamplesPerPixel := 3;
  xy := lHdr.dim[1] * lHdr.dim[2];
  xyz := xy * lHdr.dim[3];
  xys := xy *  SamplesPerPixel;
  nBytesS := xys * lHdr.dim[3] ;
  //zz setlength(img, nBytesS);
  img := Copy(rawData, Low(rawData), Length(rawData));
  j := 0;
  for i := 0 to (xyz-1) do begin
      for s := 0 to (SamplesPerPixel-1) do begin
          rawData[j] := img[i+(s * xyz)] ;
          j := j + 1;
      end;
  end;
  img := nil;
end;

procedure correctScaleSlope(var  rawData: TUInt8s; var lHdr: TNIFTIHdr; volumeDisplayed: integer; var afnis: TAFNIs);
var
   vOff, v, i, vx, volBytes, floatBytes: int64;
   in8: TUInt8s;
   in16: TInt16s;
   out32f: TFloat32s;
begin
 if length(afnis) < lHdr.Dim[4] then exit;
 volBytes := lHdr.Dim[1]*lHdr.Dim[2]*lHdr.Dim[3] * (lHdr.bitpix div 8);
 if length(rawData) = volBytes then begin//e.g. only load single volume
    lHdr.scl_slope := lHdr.scl_slope * afnis[volumeDisplayed].scl_slopex;
    exit;
 end;
 if lHdr.Dim[4] <= 1 then exit;
 vx := lHdr.Dim[1]*lHdr.Dim[2]*lHdr.Dim[3];
 floatBytes := vx * lHdr.Dim[4]* 4;
 if (lHdr.bitpix = 8) then begin
   //zz setlength(in8, Length(rawData));
   in8 := Copy(rawData, Low(rawData), Length(rawData));
   setlength(rawData, floatBytes);
   out32f := TFloat32s(rawData);
   for v := 0 to (lHdr.Dim[4] -1) do begin
       vOff := v * vx;
       for i := 0 to (vx-1) do
           out32f[i+vOff] := in8[i+vOff] * afnis[v].scl_slopex;
   end;
   lHdr.datatype := kDT_FLOAT32;
   lHdr.bitpix := 32;
 end else if (lHdr.bitpix = 16) then begin
   //zz setlength(in8, Length(rawData));
   in8 := Copy(rawData, Low(rawData), Length(rawData));
   in16 := TInt16s(in8);
   setlength(rawData, floatBytes);
   out32f := TFloat32s(rawData);
   for v := 0 to (lHdr.Dim[4] -1) do begin
       vOff := v * vx;
       for i := 0 to (vx-1) do
           out32f[i+vOff] := in16[i+vOff] * afnis[v].scl_slopex;
   end;
   lHdr.datatype := kDT_FLOAT32;
   lHdr.bitpix := 32;
 end else begin
    printf('Unsupported datatype for variable AFNI BRICK_FLOAT_FACS');
 end;

end;

{$IFDEF BZIP2}
function LoadImgBZ(FileName : AnsiString; swapEndian: boolean; var  rawData: TUInt8s; var lHdr: TNIFTIHdr; gzBytes: integer): boolean;
//foreign: using .bz2 compression, e.g. AFNI
label
	123;
var
  Decompressed: TDecompressBzip2Stream;
  InFile: TFileStream;
  i, volBytes, offset: int64;
begin
  result := false;
  volBytes := lHdr.Dim[1]*lHdr.Dim[2]*lHdr.Dim[3] * (lHdr.bitpix div 8);
  if HdrVolumes(lHdr) > 1 then
  volBytes := volBytes * HdrVolumes(lHdr);
  InFile:=TFileStream.Create(FileName, fmOpenRead);
  offset := round(lHdr.vox_offset);
  if (offset > 0) and (gzBytes = K_bz2Bytes_onlyImageCompressed) then
    Infile.Seek(offset, soFromBeginning);
  Decompressed:=TDecompressBzip2Stream.Create(InFile);
  if (offset > 0) and (gzBytes = K_bz2Bytes_headerAndImageCompressed) then begin
      SetLength (rawData, offset);
      i:=Decompressed.Read(Pointer(@rawData[0])^,volBytes);
      if i <> offset then begin
	      printf(format('BZip2 error: unable to skip header for %s', [FileName]));
	      goto 123;
      end;
  end;
  SetLength (rawData, volBytes);
  i:=Decompressed.Read(Pointer(@rawData[0])^,volBytes);
  result := (i = volBytes);
  if not result then
  printf(format('BZip2 error: read %d but expected %d bytes for %s', [i, volBytes, FileName]));
  123:
  Decompressed.Free;
  InFile.Free;
  if result and swapEndian then
    SwapImg(rawData, lHdr.bitpix);
end;
{$ENDIF}

function LoadImgASCII(FileName : AnsiString; var  rawData: TUInt8s; var lHdr: TNIFTIHdr): boolean;
//TODO: support 4D ASCII: note VTK-ASCII only 3D. ASCII not good for random access 4D files
var
   f: file;
   str: string;
   strlst: TStringList;
   u8: TUint8s;
   i16: TInt16s;
   u16: TUInt16s;
   i32: TInt32s;
   u32: TUInt32s;
   f32: TFloat32s;
   f64: TFloat64s;
   i, nvox, volBytes, FSz: int64;
begin
     result := false;
     lHdr.regular := 'r'; //we use a regular of "A" to denote ASCII encoding
     lHdr.Dim[4] := 1;
     volBytes := lHdr.Dim[1]*lHdr.Dim[2]*lHdr.Dim[3] * (lHdr.bitpix div 8);
     FSz := FSize(FileName);
     FSz := FSz - round(lHdr.vox_offset);
     if FSz < 1 then begin
        printf('ASCII file corrupted: ' + FileName);
        exit;
     end;
     setlength(str, FSz);
     FileMode := fmOpenRead;
     AssignFile(f, FileName);
     FileMode := fmOpenRead;  //Set file access to read only
     Reset(f, 1);
     Seek(f, round(lHdr.vox_offset)); //NIfTI2 has 4 byte padding to be divisible by 8
     BlockRead(f, str[1], FSz);
     FileMode := 2;
     CloseFile(f);
     nvox := lHdr.Dim[1]*lHdr.Dim[2]*lHdr.Dim[3];
     strlst:=TStringList.Create;
     strlst.DelimitedText := str;
     str := '';
     if (strlst.count < nvox) then begin
        printf(format('Found %d voxels, expected %d: %s', [strlst.count, nvox, FileName]));
        exit;
     end;
     printf(format('Found %d voxels, expected %d', [strlst.count, nvox]));
     SetLength (rawData, volBytes);
     if lHdr.datatype = kDT_UINT8 then begin
     	u8 := TUInt8s(rawData);
     	for i := 0 to (nvox - 1) do
     		u8[i] := StrToIntDef(strlst[i],0);
     end else if lHdr.datatype = kDT_INT16 then begin
     	i16 := TInt16s(rawData);
     	for i := 0 to (nvox - 1) do
     		i16[i] := StrToIntDef(strlst[i],0);
     end else if lHdr.datatype = kDT_UINT16 then begin
     	u16 := TUInt16s(rawData);
     	for i := 0 to (nvox - 1) do
     		u16[i] := StrToIntDef(strlst[i],0);
     end else if lHdr.datatype = kDT_INT32 then begin
     	i32 := TInt32s(rawData);
     	for i := 0 to (nvox - 1) do
     		i32[i] := StrToIntDef(strlst[i],0);
     end else if lHdr.datatype = kDT_UINT32 then begin
     	u32 := TUInt32s(rawData);
     	for i := 0 to (nvox - 1) do
     		u32[i] := StrToIntDef(strlst[i],0);
     end else  if lHdr.datatype = kDT_FLOAT then begin
     	f32 := TFloat32s(rawData);
     	for i := 0 to (nvox - 1) do
     		f32[i] := StrToFloatDef(strlst[i],0);
     end else  if lHdr.datatype = kDT_DOUBLE then begin
     	f64 := TFloat64s(rawData);
     	for i := 0 to (nvox - 1) do
     		f64[i] := StrToFloatDef(strlst[i],0);
     end else begin
     	printf('Unsupported ASCII data type '+inttostr(lHdr.datatype));
     	exit;
     end;
     strlst:=TStringList.Create;
     strlst.free;
     result := true;
end;

function TNIfTI.loadForeign(FileName : AnsiString; var  rawData: TUInt8s; var lHdr: TNIFTIHdr): boolean;// Load 3D data
//Uncompressed .nii or .hdr/.img pair
const
 kChunkBytes = 1073741824; //2^30;
var
   Stream : TFileStream;
   copied, gzBytes, volBytes, FSz: int64;
   swapEndian: boolean;
   tmpData: TUInt8s;
   isDimPermute2341: boolean = false;
   isAllVolumesSameScale: boolean = true;
   overlayVol: integer;
   {$IFDEF BMP}lExt: string; {$ENDIF}
begin
 if not fileexists(FileName) then exit(false);
 {$IFDEF TIF}
 if (isTIFF(FileName)) then begin
    result := LoadTIFFAsNifti(FileName, rawData, lHdr);
    if result then exit;
 end;
 {$ENDIF}
 {$IFDEF BMP}
 lExt := UpCase(extractfileextX(Filename));
 if (lExt = '.PNG') or (lExt = '.BMP') or (lExt = '.TIF') or (lExt = '.TIFF') then begin
    result := LoadBmpAsNifti(FileName, rawData, lHdr);
    if result then exit;
 end;
 {$ENDIF}
 {$IFDEF AFNI}
 if (lExt = '.HEAD') or  (lExt = '.BRIK') or (lExt = '.BRIK.GZ') then begin
    if lExt <> '.HEAD' then
       FileName := changefileextX(FileName, '.HEAD');
    result := readAFNIHeader (FileName, lHdr,  gzBytes, swapEndian, isAllVolumesSameScale,  afnis, fLabels);
 end else
 {$ENDIF}
 result := readForeignHeader (FileName, lHdr,  gzBytes, swapEndian, isDimPermute2341);
 if not result then exit(false);
 if (lHdr.bitpix <> 8) and (lHdr.bitpix <> 16) and (lHdr.bitpix <> 24) and (lHdr.bitpix <> 32) and (lHdr.bitpix <> 64)  and (lHdr.bitpix <> 128) then begin
   printf('Unable to load '+Filename+' - this software can only read 8,16,24,32,64,128-bit NIfTI files. (bitpix: '+inttostr(lHdr.bitpix)+' datatype: '+inttostr(lHdr.datatype)+ ')');
  exit(false);
 end;
 if not fileexists(FileName) then begin
    showmessage('Unable to find '+Filename);
    exit(false);
 end;
 overlayVol := fVolumeDisplayed;
 (*if ((isDimPermute2341) and (overlayVol > 0)) then begin
    printf('First 3 dimensions are not spatial: convert to NIfTI (i2nii)');
    overlayVol := 0;
 end;*)
 {$IFDEF BZIP2}
 if ((gzBytes = K_bz2Bytes_headerAndImageCompressed) or (gzBytes = K_bz2Bytes_onlyImageCompressed)) then
 	result := LoadImgBZ(FileName, swapEndian,  rawData, lHdr, gzBytes)
 {$ELSE}
 if (gzBytes = K_bz2Bytes_headerAndImageCompressed) then begin
    printf('Not compiled to read BZip2 files: '+FileName);
    exit;
 end
 {$ENDIF}
 else if gzBytes = K_gzBytes_onlyImageCompressed then
   result := LoadHdrRawImgGZ(FileName, swapEndian,  rawData, lHdr)
 else if gzBytes < 0 then
    result := LoadImgGZ(FileName, swapEndian,  rawData, lHdr)
 else if lHdr.regular = 'A' then
    result := LoadImgASCII(FileName, rawData, lHdr)
 else begin //raw
   volBytes := lHdr.Dim[1]*lHdr.Dim[2]*lHdr.Dim[3] * (lHdr.bitpix div 8);
   FSz := FSize(FileName);
   if (FSz < (round(lHdr.vox_offset)+volBytes)) then begin
    showmessage(format('Unable to load '+Filename+' - file size (%d) smaller than expected (%d) ', [FSz, round(lHdr.vox_offset)+volBytes]));
    exit(false);
   end;
   Stream := TFileStream.Create (FileName, fmOpenRead or fmShareDenyWrite);
   Try
    Stream.Seek(round(lHdr.vox_offset),soFromBeginning);
    if (overlayVol > 0) and (not isDimPermute2341) then begin
         Stream.Seek(overlayVol * volBytes,soFromCurrent);
         overlayVol := 0;
    end else begin
        if (lHdr.dim[4] > 1) then
           volBytes := volBytes * lHdr.dim[4];
    end;
    SetLength (rawData, volBytes);
    if volBytes < 2147483647 then
      Stream.ReadBuffer (rawData[0], volBytes)
    else begin
       copied := 0;
       while copied < volBytes do begin
             if (copied+kChunkBytes) <  volBytes then
                Stream.ReadBuffer (rawData[copied], kChunkBytes)
             else
                 Stream.ReadBuffer (rawData[copied], volBytes-copied);
             copied := copied + kChunkBytes;
             //printf(format('> copied %d', [copied]));
       end;
    end;
   Finally
    Stream.Free;
   End;
   //showmessage(format('%d  %d',[length(rawData), lHdr.bitpix div 8])); //x24bit
   if swapEndian then
      SwapImg(rawData, lHdr.bitpix);
 end;
 if not result then exit(false);
 planar3D2RGB8(rawData, lHdr);
 if isDimPermute2341 then
    DimPermute2341(rawData, lHdr);
 if (overlayVol > 0) then begin
    volBytes := lHdr.Dim[1]*lHdr.Dim[2]*lHdr.Dim[3] * (lHdr.bitpix div 8);
    //zz SetLength (tmpData, volBytes);
    tmpData := copy(rawData, overlayVol* volBytes, volBytes);
    //zz SetLength (rawData, volBytes);
    rawData := copy(tmpData, 0, volBytes);
    tmpData := nil;
    //GLForm1.LayerBox.Caption := inttostr(overlayVol)+':'+inttostr(random(888));
 end;
 {$IFDEF AFNI}
 if not isAllVolumesSameScale then begin
    correctScaleSlope(rawData, lHdr, fVolumeDisplayed, afnis);
 end;
 {$ENDIF}
 result := true;
end;

function TNIfTI.OpenNIfTI(): boolean;
var
   {$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
   F_Filename,lExt: string;
begin
 result := false;
 if (length(fFilename) < 1) then exit;
 if (not FileExists(fFilename)) or (FSize(fFilename) < 1) then begin
   printf('Missing or empty file '+fFilename);
   exit;
 end;
 {$IFDEF Darwin}
 if not IsReadable(fFilename) then
    exit;
 {$ENDIF}
 fVolumesLoaded := 1;
 fVolumesTotal :=  fVolumesLoaded;
 lExt := extractfileextX (fFilename);
 //lExt := uppercase(extractfileext(fFilename));
 if lExt = '.IMG' then begin
   //NIfTI images can be a single .NII file [contains both header and image]
   //or a pair of files named .HDR and .IMG. If the latter, we want to read the header first
   F_Filename := changefileext(fFilename,'.hdr');
   {$IFDEF LINUX} //LINUX is case sensitive, OSX is not
   printf('Unable to find header (case sensitive!) '+F_Filename);
   {$ELSE}
   printf('Unable to find header '+F_Filename);
   {$ENDIF}
   lExt := uppercase(extractfileext(F_Filename));
 end else
   F_Filename := fFilename;
 fKnownOrientation := false;
 {$IFDEF GZIP}
 if (lExt = '.MAT') then begin
    result := MatLoadNii(F_FileName, fHdr, fRawVolBytes);
    exit;
 end;
 {$ENDIF}
 if ((lExt = '.HDR') and (isINTERFILE(F_Filename) or (isConcordeHeader(F_Filename)))) or ((lExt <> '.IMG') and (lExt <> '.NII') and (lExt <> '.NII.GZ') and (lExt <> '.VOI') and  (lExt <> '.HDR') and (lExt <> '.GZ')) then begin
   result := loadForeign(F_FileName, fRawVolBytes, fHdr);
    if result then begin
       fVolumesLoaded := length(fRawVolBytes) div (fHdr.Dim[1]*fHdr.Dim[2]*fHdr.Dim[3] * (fHdr.bitpix div 8));
    end;
    if (fIsOverlay) then
       fVolumesTotal := fHdr.dim[4]
    else
        fVolumesTotal :=  fVolumesLoaded;
    exit;
 end;
 {$IFDEF TIMER}startTime := now;{$ENDIF}
 if (lExt = '.NII.GZ') or (lExt = '.GZ') or (lExt = '.VOI') then begin
   {$IFDEF GZIP}
   if not LoadGZ(F_FileName, fIsNativeEndian) then
      exit;
   {$ELSE}
   Showdebug('Please manually decompress images '+F_Filename);
   exit;
   {$ENDIF}
 end else if not (result) then begin
   //fVolumeDisplayed := 0;
   if not LoadRaw(F_FileName, fIsNativeEndian) then
      exit(false);
 end;
 {$IFDEF TIMER}printf(format('Load time %d',[MilliSecondsBetween(Now,startTime)]));{$ENDIF}
 if (fHdr.sform_code > kNIFTI_XFORM_UNKNOWN) or (fHdr.qform_code > kNIFTI_XFORM_UNKNOWN) then
    fKnownOrientation := true
 else
    NoMat(fHdr);
 fBidsName := changefileextX(F_FileName,'.json'); //file.nii.gz -> file.txt
 if not Fileexists(fBidsName) then
    fBidsName := '';
 result := true;
end;

{$DEFINE OVERLAY}
{$IFDEF OVERLAY}
procedure  CoordX(var lV: TVec3; lMat: TMat4);
//transform X Y Z by matrix
var
  lXi,lYi,lZi: single;
begin
  lXi := lV.x; lYi := lV.y; lZi := lV.z;
  lV.x := (lXi*lMat[0,0]+lYi*lMat[0,1]+lZi*lMat[0,2]+lMat[0,3]);
  lV.y := (lXi*lMat[1,0]+lYi*lMat[1,1]+lZi*lMat[1,2]+lMat[1,3]);
  lV.z := (lXi*lMat[2,0]+lYi*lMat[2,1]+lZi*lMat[2,2]+lMat[2,3]);
end;

(*function matrix3D(a,b,c,d, e,f,g,h, i,j,k,l: single): TMat4;
begin
     result := TMat4.Identity;
     result[0,0] := a;
     result[0,1] := b;
     result[0,2] := c;
     result[0,3] := d;
     result[1,0] := e;
     result[1,1] := f;
     result[1,2] := g;
     result[1,3] := h;
     writeln(k);
     result[2,0] := i;
     result[2,1] := j;
     result[2,2] := k;
     result[2,3] := l;
end;  //matrix3D()

procedure rep9(a,b,c,d, e,f,g,h, i,j,k,l: single);
begin
 writeln(format('%g %g %g %g', [a,b,c,d]));
 writeln(format('%g %g %g %g', [e,f,g,h]));
 writeln(format('%g %g %g %g', [i,j,k,l]));

end;

procedure ReportMat(m:TMat4);
begin
  writeln(format(':>>>>%g %g %g', [m[0,0], m[1,0], m[2,0]]));
  writeln(format(':>>>>%g %g %g', [m[0,1], m[1,1], m[2,1]]));
  writeln(format(':>>>>%g %g %g', [m[0,2], m[1,2], m[2,2]]));
  writeln('');
end;      *)
function Voxel2Voxel (lDestMat, lSrcMat: TMat4): TMat4;
//returns matrix for transforming voxels from one image to the other image
//results are in VOXELS not mm
var
   lV0,lVx,lVy,lVz: TVec3;
   lSrcMatInv: TMat4;
begin
     //Step 1 - compute source coordinates in mm for 4 voxels
     //the first vector is at 0,0,0, with the
     //subsequent voxels being left, up or anterior
     //lDestMat := Hdr2Mat(lDestHdr);
     lV0 := Vec3( 0,0,0);
     lVx := Vec3( 1,0,0);
     lVy := Vec3( 0,1,0);
     lVz := Vec3( 0,0,1);
     CoordX(lV0,lDestMat);
     CoordX(lVx,lDestMat);
     CoordX(lVy,lDestMat);
     CoordX(lVz,lDestMat);
     lSrcMatInv := lSrcMat.inverse;
     //ReportMat(lSrcMat);
     //ReportMat(lSrcMatInv);

     //clipboard.AsText := mStr('i2',lSrcMatInv);
     //the vectors should be rows not columns....
     //therefore we transpose the matrix
     //lSrcMatInv := lSrcMatInv.transpose;
     //the 'transform' multiplies the vector by the matrix
     lV0 := (lV0 * lSrcMatInv);
     lVx := (lVx * lSrcMatInv);
     lVy := (lVy * lSrcMatInv);
     lVz := (lVz * lSrcMatInv);
     //subtract each vector from the origin
     // this reveals the voxel-space influence for each dimension
     lVx := lVx - lV0;
     lVy := lVy - lV0;
     lVz := lVz - lV0;
     result := TMat4.Identity;
     result.m[0,0] := lVx.x;
     result.m[0,1] := lVy.x;
     result.m[0,2] := lVz.x;
     result.m[0,3] := lV0.x;
     result.m[1,0] := lVx.y;
     result.m[1,1] := lVy.y;
     result.m[1,2] := lVz.y;
     result.m[1,3] := lV0.y;
     result.m[2,0] := lVx.z;
     result.m[2,1] := lVy.z;
     result.m[2,2] := lVz.z;
     result.m[2,3] := lV0.z;

     //rep9(1,2,3,4,5,6,7,8,9,10,11,12);
     //writeln(format('<< %g %g %g', [lVx.z,lVy.z,lVz.z]));
     //writeln(lVz.z);
     (*result := Matrix3D(lVx.x,lVy.x,lVz.x,lV0.x,
      lVx.y,lVy.y,lVz.y,lV0.y,
      lVx.z,lVy.z,lVz.z,lV0.z);*)
     //ReportMat(result);
end;

procedure TNIfTI.VolumeReslice(tarMat: TMat4; tarDim: TVec3i; isLinearReslice: boolean);
var
   m: TMat4;
   f32zero: single;
   i16zero: int16;
   i32zero: int32;
   u8zero: byte;
   in8, out8: TUInt8s;
   in16, out16: TInt16s;
   in32, out32: TInt32s;
   in32f, out32f: TFloat32s;
   lMaxY, lMaxZ, lMinY, lMinZ: int64;
   lXrM1, lYrM1, lZrM1, lZx, lZy, lZz, lYx, lYy, lYz, lXreal, lYreal, lZreal: single;
   lXx, lXy, lXz: array of single;
   lXYs, lXs, lYs, lZs, lXo, lYo, lZo, lPos,i,lXi, lYi, lZi, datatype, lBPP: int64;
   lOverlap : boolean = false;
   inVox, outvox: int64;
   minNegNotZero, minPosNotZero, minNegThresh, minPosThresh: single;
   minNegNotZeroI, minPosNotZeroI, minNegThreshI, minPosThreshI: integer;
begin
     if prod(tarDim) < 1 then exit;
     if (tarMat = fMat) and (tarDim.X = fDim.X) and (tarDim.Y = fDim.Y) and (tarDim.Z = fDim.Z) then exit;
     if prod(tarDim) = prod(fDim) then begin
        VolumeReorient();
        if (tarMat = fMat) and (tarDim.X = fDim.X) and (tarDim.Y = fDim.Y) and (tarDim.Z = fDim.Z) then exit;
     end;
     IsInterpolated := true;
     (*if fVolumesLoaded > 1 then begin
        showmessage('Fatal error: overlays can not have multiple volumes [yet]');
        exit;
     end; *)
     //if isLinearReslice then
     //   SmoothMaskedImages();
     m := Voxel2Voxel (tarMat,fMat); //source to target voxels
     //clipboard.AsText := mStr('t',tarMat)+mStr('s',fMat)+mStr('vx',m);
     lXs := fDim.X;
     lYs := fDim.Y;
     lZs := fDim.Z;
     lXYs:=lXs*lYs; //slicesz
     dataType := fHdr.datatype;
     case dataType of
       kDT_UNSIGNED_CHAR : lBPP := 1;
       kDT_SIGNED_SHORT: lBPP := 2;
       kDT_SIGNED_INT:lBPP := 4;
       kDT_FLOAT: lBPP := 4;
       else begin
          printf('NII reslice error: datatype not supported.');
          exit;
       end;
     end; //case
     i := prod(fDim) * lBPP; //input bytes
     //zz setlength(in8, i);
     in8 := Copy(fRawVolBytes, Low(fRawVolBytes), Length(fRawVolBytes));
     outVox := prod(tarDim);
     i := outVox * lBPP; //target bytes
     //clipboard.AsText := format('%d %d', [prod(tarDim), lBPP]);
     setlength(fRawVolBytes, i);
     out8 := TUInt8s(fRawVolBytes);
     for lXi := 0 to (i-1) do
         out8[lXi] := 0;
     if ((dataType = kDT_SIGNED_SHORT) and (round(fHdr.glmin) = -32768) and (round(fHdr.glmax) = 32767)) then begin
     	out16 := TInt16s(fRawVolBytes);
        lYi :=  prod(tarDim);
        for lXi := 0 to (lYi-1) do
        	out16[lXi] := -32768;
     end;
     //clipboard.AsText := format('%d %d', [prod(tarDim), lBPP]);
     in16 := TInt16s(in8);
     out16 := TInt16s(fRawVolBytes);
     in32 := TInt32s(in8);
     out32 := TInt32s(fRawVolBytes);
     in32f := TFloat32s(in8);
     out32f := TFloat32s(fRawVolBytes);
     if (fHdr.scl_inter <> 0.0) and (fHdr.scl_slope <> 0.0) then begin
        f32zero := fHdr.scl_inter / fHdr.scl_slope;
        i16zero := round(f32zero);
        i32zero := round(f32zero);
        u8zero :=  round(f32zero);
        if lBPP = 1 then
        	for i := 0 to (outVox-1) do
        		out8[i] := u8zero;
        if lBPP = 2 then
        	for i := 0 to (outVox-1) do
        		out16[i] := i16zero;
        if (lBPP = 4) and (dataType <> kDT_FLOAT) then
        	for i := 0 to (outVox-1) do
        		out32[i] := i32zero;
        if (lBPP = 4) and (dataType = kDT_FLOAT) then
        	for i := 0 to (outVox-1) do
        		out32f[i] := f32zero;
     end;
     setlength(lXx, tarDim.x);
     setlength(lXy, tarDim.x);
     setlength(lXz, tarDim.x);
     for lXi := 0 to (tarDim.x-1) do begin
      lXx[lXi] := lXi*m[0,0];
      lXy[lXi] := lXi*m[1,0];
      lXz[lXi] := lXi*m[2,0];
     end;
     if (dataType = kDT_FLOAT) then begin
        inVox := prod(fDim);
        for i := 0 to (inVox-1) do
            if specialsingle(in32f[i]) then
                in32f[i] := 0;
     end;
     (*TODO writeln(format('>>>>%g %g %g', [lXx[tarDim.x-1], lXy[tarDim.x-1], lXz[tarDim.x-1]]));
     writeln(format('>>>>%d %d %d', [tarDim.X, tarDim.Y, tarDim.Z]));
     writeln(format('>>>>%d %d %d', [lXs, lYs, lZs]));
     writeln('');
     writeln(format('>>>>%g %g %g', [m[0,0], m[1,0], m[2,0]]));
     writeln(format('>>>>%g %g %g', [m[0,1], m[1,1], m[2,1]]));
     writeln(format('>>>>%g %g %g', [m[0,2], m[1,2], m[2,2]])); *)

     lPos := -1;
     if isLinearReslice then begin //trilinear
        for lZi := 0 to (tarDim.Z-1) do begin
            //these values are the same for all voxels in the slice
            // compute once per slice
            lZx := lZi*m[0,2];
            lZy := lZi*m[1,2];
            lZz := lZi*m[2,2];
            for lYi := 0 to (tarDim.Y-1) do begin
                //these values change once per row
                // compute once per row
                lYx :=  lYi*m[0,1];
                lYy :=  lYi*m[1,1];
                lYz :=  lYi*m[2,1];
                for lXi := 0 to (tarDim.x-1) do begin
                    inc(lPos);
                    //compute each column
                    lXreal := (lXx[lXi]+lYx+lZx+m[0,3]);
                    lYreal := (lXy[lXi]+lYy+lZy+m[1,3]);
                    lZreal := (lXz[lXi]+lYz+lZz+m[2,3]);
                    //need to test Xreal as -0.01 truncates to zero
                    if (lXreal >= 0) and (lYreal >= 0{1}) and (lZreal >= 0{1}) and
                        (lXreal < (lXs -1)) and (lYreal < (lYs -1) ) and (lZreal < (lZs -1))
                     then begin
                        //compute the contribution for each of the 8 source voxels
                        //nearest to the target
                        lOverlap := true;
                        lXo := trunc(lXreal);
                        lYo := trunc(lYreal);
                        lZo := trunc(lZreal);
                        lXreal := lXreal-lXo;
                        lYreal := lYreal-lYo;
                        lZreal := lZreal-lZo;
                        lXrM1 := 1-lXreal;
                        lYrM1 := 1-lYreal;
                        lZrM1 := 1-lZreal;
                        lMinY := lYo*lXs;
                        lMinZ := lZo*lXYs;
                        lMaxY := lMinY+lXs;
                        lMaxZ := lMinZ+lXYs;
                        //inc(lXo);//if images incremented from 1 not 0
                        case dataType of
                          kDT_UNSIGNED_CHAR : begin// l8is := l8is;
                               out8[lPos] := round (
                             {all min} ( (lXrM1*lYrM1*lZrM1)*in8[lXo+lMinY+lMinZ])
                             {x+1}+((lXreal*lYrM1*lZrM1)*in8[lXo+1+lMinY+lMinZ])
                             {y+1}+((lXrM1*lYreal*lZrM1)*in8[lXo+lMaxY+lMinZ])
                             {z+1}+((lXrM1*lYrM1*lZreal)*in8[lXo+lMinY+lMaxZ])
                             {x+1,y+1}+((lXreal*lYreal*lZrM1)*in8[lXo+1+lMaxY+lMinZ])
                             {x+1,z+1}+((lXreal*lYrM1*lZreal)*in8[lXo+1+lMinY+lMaxZ])
                             {y+1,z+1}+((lXrM1*lYreal*lZreal)*in8[lXo+lMaxY+lMaxZ])
                             {x+1,y+1,z+1}+((lXreal*lYreal*lZreal)*in8[lXo+1+lMaxY+lMaxZ]) );
                          end;
                          kDT_SIGNED_SHORT: begin
                               out16[lPos] := round (
                             {all min} ( (lXrM1*lYrM1*lZrM1)*in16[lXo+lMinY+lMinZ])
                             {x+1}+((lXreal*lYrM1*lZrM1)*in16[lXo+1+lMinY+lMinZ])
                             {y+1}+((lXrM1*lYreal*lZrM1)*in16[lXo+lMaxY+lMinZ])
                             {z+1}+((lXrM1*lYrM1*lZreal)*in16[lXo+lMinY+lMaxZ])
                             {x+1,y+1}+((lXreal*lYreal*lZrM1)*in16[lXo+1+lMaxY+lMinZ])
                             {x+1,z+1}+((lXreal*lYrM1*lZreal)*in16[lXo+1+lMinY+lMaxZ])
                             {y+1,z+1}+((lXrM1*lYreal*lZreal)*in16[lXo+lMaxY+lMaxZ])
                             {x+1,y+1,z+1}+((lXreal*lYreal*lZreal)*in16[lXo+1+lMaxY+lMaxZ]) );
                          end;
                          kDT_SIGNED_INT:begin
                               out32[lPos] :=
                                round (
                             {all min} ( (lXrM1*lYrM1*lZrM1)*in32[lXo+lMinY+lMinZ])
                             {x+1}+((lXreal*lYrM1*lZrM1)*in32[lXo+1+lMinY+lMinZ])
                             {y+1}+((lXrM1*lYreal*lZrM1)*in32[lXo+lMaxY+lMinZ])
                             {z+1}+((lXrM1*lYrM1*lZreal)*in32[lXo+lMinY+lMaxZ])
                             {x+1,y+1}+((lXreal*lYreal*lZrM1)*in32[lXo+1+lMaxY+lMinZ])
                             {x+1,z+1}+((lXreal*lYrM1*lZreal)*in32[lXo+1+lMinY+lMaxZ])
                             {y+1,z+1}+((lXrM1*lYreal*lZreal)*in32[lXo+lMaxY+lMaxZ])
                             {x+1,y+1,z+1}+((lXreal*lYreal*lZreal)*in32[lXo+1+lMaxY+lMaxZ]) );
                          end;
                          kDT_FLOAT: begin
                               out32f[lPos] := (
                             {all min} ( (lXrM1*lYrM1*lZrM1)*in32f[lXo+lMinY+lMinZ])
                             {x+1}+((lXreal*lYrM1*lZrM1)*in32f[lXo+1+lMinY+lMinZ])
                             {y+1}+((lXrM1*lYreal*lZrM1)*in32f[lXo+lMaxY+lMinZ])
                             {z+1}+((lXrM1*lYrM1*lZreal)*in32f[lXo+lMinY+lMaxZ])
                             {x+1,y+1}+((lXreal*lYreal*lZrM1)*in32f[lXo+1+lMaxY+lMinZ])
                             {x+1,z+1}+((lXreal*lYrM1*lZreal)*in32f[lXo+1+lMinY+lMaxZ])
                             {y+1,z+1}+((lXrM1*lYreal*lZreal)*in32f[lXo+lMaxY+lMaxZ])
                             {x+1,y+1,z+1}+((lXreal*lYreal*lZreal)*in32f[lXo+1+lMaxY+lMaxZ]) );
                          end;
                      end; //case of datatype
                    end; //if voxel is in source image's bounding box
                end;//z
            end;//y
        end;//z
        if (fHdr.scl_inter = 0.0) then begin //Fight Interpolation Bleeding
           //consider data thresholded at z > 3.0: do not allow interpolated values below this!
           // values 0..1.5 will be set to zero, values 1.5..3 will be set to 3
          inVox := prod(fDim);
          outVox := prod(tarDim);
          minPosNotZero := infinity;
          minNegNotZero := -infinity;
           if (dataType = kDT_UNSIGNED_CHAR) then begin
             for i := 0 to (inVox-1) do
                 if (in8[i] > 0) and (in8[i] <  minPosNotZero) then
                    minPosNotZero := in8[i]; //closest positive to zero
             minPosNotZeroI := round(minPosNotZero);
             minPosThreshI := round(minPosNotZero * 0.5);
             for i := 0 to (outVox-1) do
                 if (out8[i] > 0) and (out8[i] <  minPosNotZeroI) then begin
                    if out8[i] < minPosThreshI then
                       out8[i] := 0
                    else
                        out8[i] := minPosNotZeroI; //closest negative to zero
                 end;
           end; //dataType = kDT_UNSIGNED_CHAR
          if (dataType = kDT_SIGNED_SHORT) then begin
            minPosNotZero := 32767;
            minNegNotZero := -32768;
            for i := 0 to (inVox-1) do
                if (in16[i] > 0) and (in16[i] <  minPosNotZero) then
                   minPosNotZero := in16[i]; //closest positive to zero
            for i := 0 to (inVox-1) do
                if (in16[i] < 0) and (in16[i] >  minNegNotZero) then
                   minNegNotZero := in16[i]; //closest negative to zero
            minNegNotZeroI := round(minNegNotZero);
            minPosNotZeroI := round(minPosNotZero);
            minNegThreshI := round(minNegNotZero * 0.5);
            minPosThreshI := round(minPosNotZero * 0.5);
            for i := 0 to (outVox-1) do
                if (out16[i] < 0) and (out16[i] >  minNegNotZeroI) then begin
                   if out16[i] > minNegThreshI then
                      out16[i] := 0
                   else
                       out16[i] := minNegNotZeroI; //closest negative to zero
                end;
            for i := 0 to (outVox-1) do
                if (out16[i] > 0) and (out16[i] <  minPosNotZeroI) then begin
                   if out16[i] < minPosThreshI then
                      out16[i] := 0
                   else
                       out16[i] := minPosNotZeroI; //closest negative to zero
                end;
          end; //dataType = kDT_SIGNED_SHORT
           if (dataType = kDT_SIGNED_INT) then begin
             minPosNotZero := 2147483647;
             minNegNotZero := -2147483648;
             for i := 0 to (inVox-1) do
                 if (in32[i] > 0) and (in32[i] <  minPosNotZero) then
                    minPosNotZero := in32[i]; //closest positive to zero
             for i := 0 to (inVox-1) do
                 if (in32[i] < 0) and (in32[i] >  minNegNotZero) then
                    minNegNotZero := in32[i]; //closest negative to zero
             minNegNotZeroI := round(minNegNotZero);
             minPosNotZeroI := round(minPosNotZero);
             minNegThreshI := round(minNegNotZero * 0.5);
             minPosThreshI := round(minPosNotZero * 0.5);
             for i := 0 to (outVox-1) do
                 if (out32[i] < 0) and (out32[i] >  minNegNotZeroI) then begin
                    if out32[i] > minNegThreshI then
                       out32[i] := 0
                    else
                        out32[i] := minNegNotZeroI; //closest negative to zero
                 end;
             for i := 0 to (outVox-1) do
                 if (out32[i] > 0) and (out32[i] <  minPosNotZeroI) then begin
                    if out32[i] < minPosThreshI then
                       out32[i] := 0
                    else
                        out32[i] := minPosNotZeroI; //closest negative to zero
                 end;
           end; //dataType = kDT_SIGNED_INT
          if (dataType = kDT_FLOAT) then begin
            for i := 0 to (inVox-1) do
                if (in32f[i] > 0) and (in32f[i] <  minPosNotZero) then
                   minPosNotZero := in32f[i]; //closest positive to zero
            for i := 0 to (inVox-1) do
                if (in32f[i] < 0) and (in32f[i] >  minNegNotZero) then
                   minNegNotZero := in32f[i]; //closest negative to zero
            minNegThresh := minNegNotZero * 0.5;
            minPosThresh := minPosNotZero * 0.5;
            for i := 0 to (outVox-1) do
                if (out32f[i] < 0) and (out32f[i] >  minNegNotZero) then begin
                   if out32f[i] > minNegThresh then
                      out32f[i] := 0
                   else
                       out32f[i] := minNegNotZero; //closest negative to zero
                end;
            for i := 0 to (outVox-1) do
                if (out32f[i] > 0) and (out32f[i] <  minPosNotZero) then begin
                   if out32f[i] < minPosThresh then
                      out32f[i] := 0
                   else
                       out32f[i] := minPosNotZero; //closest negative to zero
                end;
          end; //dataType = kDT_FLOAT
        end; //IsControlInterpolationBleeding
     end else begin //if trilinear, else nearest neighbor
        for lZi := 0 to (tarDim.Z-1) do begin
          //these values are the same for all voxels in the slice
          // compute once per slice
            lZx := lZi*m[0,2];
            lZy := lZi*m[1,2];
            lZz := lZi*m[2,2];
            for lYi := 0 to (tarDim.Y-1) do begin
               //these values change once per row
               // compute once per row
               lYx :=  lYi*m[0,1];
               lYy :=  lYi*m[1,1];
               lYz :=  lYi*m[2,1];
               for lXi := 0 to (tarDim.X-1) do begin
                   //compute each column
                   inc(lPos);
                   lXo := round(lXx[lXi]+lYx+lZx+m[0,3]);
                   lYo := round(lXy[lXi]+lYy+lZy+m[1,3]);
                   lZo := round(lXz[lXi]+lYz+lZz+m[2,3]);
                   if (lXo >= 0) and (lYo >= 0{1}) and (lZo >= 0{1}) and
                       (lXo < (lXs -1)) and (lYo < (lYs -1) ) and (lZo < (lZs {-1}))
                    then begin
                      lOverlap := true;
                      //inc(lXo);//if images incremented from 1 not 0
                      lYo := lYo*lXs;
                      lZo := lZo*lXYs;
                      case dataType of
                        kDT_UNSIGNED_CHAR : // l8is := l8is;
                        out8[lPos] :=in8[lXo+lYo+lZo];
                        kDT_SIGNED_SHORT: out16[lPos] := in16[lXo+lYo+lZo];
                        kDT_FLOAT,kDT_SIGNED_INT: out32[lPos] := in32[lXo+lYo+lZo];
                      // kDT_FLOAT: out32f[lPos] := in32f[lXo+lYo+lZo];
                      end; //case
                   end; //if voxel is in source image's bounding box
               end;//z
            end;//y
        end;//z
     end; //if trilinear else nearest
     fMat := tarMat;
     fDim := tarDim;
     fhdr.dim[1] := tarDim.x;
     fhdr.dim[2] := tarDim.y;
     fhdr.dim[3] := tarDim.z;
     Mat2SForm(tarMat, fHdr);
     if not lOverlap then
        printf('Warning no overlap between overlay and background volume');
end;

{$ELSE}
procedure TNIfTI.VolumeReslice(tarMat: TMat4; tarDim: TVec3i);
begin
     printf('Recompile with reslice support!');
end;

{$ENDIF}

procedure LoadLabelsCore(lInStr: string; var lLabels: TStringList);
const
  kTab = chr(9);
  //kEsc = chr(27);
  kCR = chr (13);
  //kBS = #8 ; // Backspace
  //kDel = #127 ; // Delete
  kUNIXeoln = chr(10);
var
   lIndex,lPass,lMaxIndex,lPos,lLength: integer;
   lStr1: string;
   lCh: char;
begin
     lLabels.Clear;
     if AnsiContainsText(lInstr, 'xml version=') then
        exit;
     lLength := length(lInStr);
     lMaxIndex := -1;
     for lPass := 1 to 2 do begin
      lPos := 1;
      if lPass = 2 then begin
        if lMaxIndex < 1 then
          exit;
        for lIndex := 0 to lMaxIndex do
          lLabels.Add('');
      end;
      while lPos <= lLength do begin
        lStr1 := '';
        repeat
	      lCh := lInStr[lPos]; inc(lPos);
	      if (lCh >= '0') and (lCh <= '9') then
	         lStr1 := lStr1 + lCh
              else
                  lCh := kTab;
        until (lPos > lLength) or ((lCh = kTab) and (length(lStr1)>0));

              // until (lPos > lLength) or (lCh = kCR) or (lCh = kUNIXeoln) or ((lCh = kTab) and (length(lStr1)>0));
       if (length(lStr1) > 0) and (lPos <= lLength) then begin
		      lIndex := strtoint(lStr1);
          if lPass = 1 then begin
                      if lIndex > lMaxIndex then
                         lMaxIndex := lIndex
          end else if lIndex >= 0 then begin //pass 2
            lStr1 := '';
		        repeat
              lCh := lInStr[lPos]; inc(lPos);
              if (lPos > lLength) or (lCh=kCR) or (lCh=kUNIXeoln) {or (lCh=kTab) or (lCh=' ')} then
                  //
              else
                lStr1 := lStr1 + lCh;
            until (lPos > lLength) or (lCh=kCR) or (lCh=kUNIXeoln) {or (lCh=kTab)or (lCh=' ')};
			      lLabels[lIndex] := lStr1;
                              //printf(format('%d:%s',[lIndex,lStr1]));
		      end; //if pass 2
		    end; //if lStr1>0
	    end; //while not EOF
     end; //for each pass
end;

procedure LoadLabels(lFileName: string; var lLabels: TStringList; lOffset,lLength: integer);
var
  f : file; // untyped file
  s, lExt : string; // string for reading a file
  sz: int64;
  ptr: bytep;
begin
 lExt := uppercase(extractfileext(lFileName));
  if (lExt = '.GZ') or (lExt = '.VOI') then begin  //open gz compressed
    if (lLength < 1) then exit;
    SetLength(s, lLength);
    ptr :=  @s[1];
    UnGZip (lFileName,ptr, lOffset,lLength);
  end else begin
    AssignFile(f, lFileName);
    FileMode := fmOpenRead;
    reset(f, 1);
    if lOffset > 0 then
      seek(f, lOffset);
    if (lLength < 1) then
      sz := FileSize(f)-lOffset
    else
      sz := lLength;
    if (lOffset+sz) > FileSize(f) then
      exit;
    SetLength(s, sz);
    BlockRead(f, s[1], length(s));
    CloseFile(f);
    FileMode := fmOpenReadWrite;
   end;
   LoadLabelsCore(s, lLabels);
   //showmessage(lLabels[1]);
end;

procedure LoadLabelsTxt(lFileName: string; var lLabels: TStringList);
//filename = 'all.nii' will read 'aal.txt'
var
   lLUTname: string;
begin
     lLabels.Clear; //empty current labels
     lLUTname := changefileextX(lFileName,'.txt'); //file.nii.gz -> file.txt
     if not Fileexists(lLUTname) then
        lLUTname := changefileext(lFileName,'.txt'); //file.nii.gz -> file.nii.txt ;
     if not Fileexists(lLUTname) then
        exit;
     LoadLabels(lLUTname, lLabels,0,-1);
end;

function TNIfTI.Load(niftiFileName: string; tarMat: TMat4; tarDim: TVec3i; isInterpolate: boolean; hdr: TNIFTIhdr; img: TFloat32s; isUINT8: boolean = false): boolean; overload;
var
   scaleMx: single;
   vol8: TUInt8s;
   vol32: TFloat32s;
   i: int64;
   {$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
begin
  {$IFDEF CACHEUINT8} //release cache, reload on next refresh
  fWindowMinCache8 := infinity;
  fCache8 := nil;
  {$ENDIF}
  fisLinearReslice := isInterpolate;
  fLabels.Clear;
  fLabelMask := nil;
  IsInterpolated := false;
  fBidsName := '';
  fFilename := niftiFileName;
  fVolumeDisplayed := 0;
  if fVolumeDisplayed < 0 then fVolumeDisplayed := 0;
  fIsOverlay := (tarDim.X) > 0;
  HiddenByCutout := not fIsOverlay;
  fShortName := changefileextX(extractfilename(fFilename),'');
  result := true;
  fHdr := Hdr;
  fHdr.cal_max := 0;
  fHdr.cal_min := 0;
  fHdr.dim[0] := 3;
  fHdr.dim[4] := 1;
  fHdr.dim[5] := 1;
  fHdr.dim[6] := 1;
  fHdr.dim[7] := 1;
  if fHdr.intent_code = kNIFTI_INTENT_CORREL then begin
    fHdr.cal_min := 0.05;
    fHdr.cal_max := 1;
  end;
  if (isUINT8) then begin
	fHdr.datatype:= kDT_UINT8;
	fHdr.bitpix := 8;
    fHdr.scl_inter:= 0;
    fHdr.scl_slope:= 1.0 / 255.0;
    vol8 := TUInt8s(img);
    setlength(fRawVolBytes, length(vol8));
    for i := 0 to (length(vol8)-1) do
          fRawVolBytes[i] := vol8[i];
  end else begin
  	fHdr.datatype:= kDT_FLOAT32;
  	fHdr.bitpix := 32;
    fHdr.scl_inter:= 0;
    fHdr.scl_slope:= 1;
    setlength(fRawVolBytes, length(img)*4);
    vol32 := TFloat32s(fRawVolBytes);
    for i := 0 to (length(img)-1) do
          vol32[i] := img[i];

  end;
  fixBogusHeaders(fHdr);
  fHdrNoRotation := fHdr; //raw header without reslicing or orthogonal rotation
  fVolumesLoaded := max(HdrVolumes(fHdr),1);
  fVolumesTotal :=  fVolumesLoaded;
  //showmessage(format('%d', [prod(tarDim)]));
  //IsLabels := true;
  if (prod(tarDim) = 0) and (fHdr.dim[3] > 1) and (MaxTexMb > 0) then //reduce size of huge background images
    if ShrinkLargeMb(fHdr,fRawVolBytes, MaxTexMb, isAntiAliasHugeTexMb, true) then begin
        fVolumesLoaded := 1;
        IsShrunken := true;
     end;
  scaleMx := max(max(abs(fHdr.Dim[1]*fHdr.PixDim[1]),abs(fHdr.Dim[2]*fHdr.pixDim[2])),abs(fHdr.Dim[3]*fHdr.pixDim[3]));
  if (scaleMx <> 0) then begin
    fScale.X := abs((fHdr.Dim[1]*fHdr.PixDim[1]) / scaleMx);
    fScale.Y := abs((fHdr.Dim[2]*fHdr.PixDim[2]) / scaleMx);
    fScale.Z := abs((fHdr.Dim[3]*fHdr.PixDim[3]) / scaleMx);
  end;
  fDim.x := fHdr.Dim[1];
  fDim.y := fHdr.Dim[2];
  fDim.z := fHdr.Dim[3];
  fPermInOrient := pti(1,2,3);
  fMat := SForm2Mat(fHdr);
  fMatInOrient := fMat;
  {$IFDEF TIMER}startTime := now;{$ENDIF}
  if fHdr.scl_slope = 0 then fHdr.scl_slope := 1;
  if prod(tarDim) > 0 then begin
       VolumeReslice(tarMat, tarDim, isInterpolate)
  end else
      VolumeReorient();
  {$IFDEF TIMER}
  i := MilliSecondsBetween(Now,startTime);
  if (i > 5) then
     printf(format('Reorient time %d',[i]));
  {$ENDIF}
  fMat := SForm2Mat(fHdr);
  fInvMat := fMat.inverse;
  {$IFDEF TIMER}startTime := now;{$ENDIF}
  if (isUINT8) then
    initUInt8()
  else
  	initFloat32();
  if fAutoBalMin = fAutoBalMax then begin //e.g. thresholded data
     fAutoBalMin := fMin;
     fAutoBalMax := fMax;
     fWindowMin := fMin;
     fWindowMax := fMax;
  end else if (fIsOverlay) and (fAutoBalMin < -1.0) and (fAutoBalMax > 1.0) then begin
    fAutoBalMin := fAutoBalMax;
    fAutoBalMax := fAutoBalMax;
  end;
  if (fHdr.cal_max > fHdr.cal_min) then begin
     if (fAutoBalMax > fAutoBalMin) and (((fHdr.cal_max - fHdr.cal_min)/(fAutoBalMax - fAutoBalMin)) > 5) then begin
          {$IFDEF Unix}printf('Ignoring implausible cal_min..cal_max: FSL eddy?');{$ENDIF}
     end else begin
          fAutoBalMin := fHdr.cal_min;
          fAutoBalMax := fHdr.cal_max;
     end;
  end;
  {$IFDEF TIMER}printf(format('Load Init time %d',[MilliSecondsBetween(Now,startTime)]));{$ENDIF}
  {$IFDEF TIMER}startTime := now;{$ENDIF}
  fWindowMin := fAutoBalMin;
  fWindowMax := fAutoBalMax;
  if (CLUT.SuggestedMinIntensity < CLUT.SuggestedMaxIntensity) and (fMax > CLUT.SuggestedMinIntensity) and (fMin < CLUT.SuggestedMaxIntensity) then begin
     fWindowMin := CLUT.SuggestedMinIntensity;
     fWindowMax := CLUT.SuggestedMaxIntensity;
  end;
  if (fIsOverlay) and (fMin > -25) and (fMin < -5) and (fMax > 5) and (fMax < 25) and (fAutoBalMin < -0.5) and (fAutoBalMax > 0.5) then begin
     printf('Adjusting initial image intensity: Assuming statistical overlay.');
     fWindowMin := 1;
     fWindowMax := 1;
  end;
  //{$IFDEF TIMER}startTime := now;{$ENDIF}
  (*if (fHdr.datatype = kDT_FLOAT32) and (fVolumesLoaded = 3) then
    fDTImode := kDTIscalar;*)
  SetDisplayMinMax();
  //{$IFDEF TIMER}printf(format('Set Min/Max time %d',[MilliSecondsBetween(Now,startTime)]));{$ENDIF}
end;

function Clusterize(var lImg: TUInt8s; Xi,Yi,Zi: integer; out clusterNumber: integer; out img32: TInt32s; NeighborMethod: integer; smallestClusterVox: integer = 0): boolean;
label
     123;
var
  i, j, XY, XYZ, qlo, qhi: integer;
  qimg: TInt32s;
procedure checkPixel(vxl: integer); inline;
begin
     if img32[vxl] <> -1 then exit; //already found or not a target
     qhi := qhi + 1;
     img32[vxl] := clusterNumber; //found
     qimg[qhi] := vxl; //location
end;//nested checkPixel()
procedure retirePixel6(); inline;
var
  vxl: integer;
begin
     vxl := qimg[qlo];
     checkPixel(vxl-1);
     checkPixel(vxl+1);
     checkPixel(vxl-Xi);
     checkPixel(vxl+Xi);
     checkPixel(vxl-XY);
     checkPixel(vxl+XY);
     qlo := qlo + 1;
end;//nested retirePixel()
procedure retirePixel18(); inline;
var
  vxl: integer;
begin
     vxl := qimg[qlo];
     //edges in plane
     checkPixel(vxl-Xi-1);
     checkPixel(vxl-Xi+1);
     checkPixel(vxl+Xi-1);
     checkPixel(vxl+Xi+1);
     //edges below
     checkPixel(vxl-1-XY);
     checkPixel(vxl+1-XY);
     checkPixel(vxl-Xi-XY);
     checkPixel(vxl+Xi-XY);
     //edges above
     checkPixel(vxl-1+XY);
     checkPixel(vxl+1+XY);
     checkPixel(vxl-Xi+XY);
     checkPixel(vxl+Xi+XY);
     retirePixel6();
end;//nested retirePixel()
procedure retirePixel26(); inline;
var
  vxl: integer;
begin
     vxl := qimg[qlo];
     //corners below
     checkPixel(vxl-Xi-XY-1);
     checkPixel(vxl-Xi-XY+1);
     checkPixel(vxl+Xi-XY-1);
     checkPixel(vxl+Xi-XY+1);
     //corners above
     checkPixel(vxl-Xi+XY-1);
     checkPixel(vxl-Xi+XY+1);
     checkPixel(vxl+Xi+XY-1);
     checkPixel(vxl+Xi+XY+1);
     retirePixel18();
end;
begin //main RemoveSmallClusters()
  clusterNumber := 0;
  result := false;
  if (Zi < 1) then exit;
  XY := Xi * Yi;
  XYZ := XY * Zi;
  setlength(img32, XYZ);
  setlength(qimg, XYZ);
  //set target voxels
  for i := 0 to (XYZ-1) do begin
      img32[i] := 0;
      if lImg[i] > 0 then
         img32[i] := -1;
  end;
  //clear bottom and top slices
  for i := 0 to (XY-1) do
    img32[i] := 0;
  for i := (XYZ-1-XY) to (XYZ-1) do
    img32[i] := 0;
  //now seed each voxel
  for i := (XY) to (XYZ-1-XY) do begin
      if (img32[i] < 0) then begin //voxels not yet part of any region
         clusterNumber := clusterNumber + 1;
         if (clusterNumber < 1) then goto 123; //more than 2^32 clusters!
         qlo := 0;
         qhi := -1;
         checkPixel(i);
         while qlo <= qhi do begin
             case NeighborMethod of
               3:
                 retirePixel26;
               2:
                 retirePixel18;
               1:
                 retirePixel6;
             end;
           end;
         if (qhi +1) < smallestClusterVox then begin
           for j := 0 to qhi do
              img32[qimg[j]] := 0;//qhi + 1;
           clusterNumber := clusterNumber - 1;
         end;
      end;
  end;
  result := true;
123:
  qimg := nil;
  if not result then clusterNumber := 0;
  //img32 := nil;
end;

function InitCluster():TCluster;
begin
     result.CogXYZ := Vec3(0,0,0);
     result.PeakStructure := '';
     result.Structure := '';
     result.SzMM3:= 0;
     result.Peak := 0;
     result.PeakXYZ := Vec3(0,0,0);
end;


function GetClusterNotes(var n: TNIfTI): string;
begin
     if n = nil then exit('');
     result := n.shortName;
     if (n.IsShrunken) or (n.IsInterpolated) then
         result += '(interpolated)';
end;

function TNIfTI.mm3toVox(mm3: single) : integer; //e.g. if 3x3x3mm voxels (27mm) and input is 28, return 2
var
   mmPerVox: single;

begin
     mmPerVox := VoxMM3();
     if (mmPerVox = 0) then mmPerVox := 1;
     result := ceil(mm3/mmPerVox);
     result := max(0,result);
end;

{$DEFINE XF2}
{$IFDEF XF2}
procedure TNIfTI.GenerateClusters(LabelMap: TNIfTI; thresh, smallestClusterMM3: single; NeighborMethod: integer; isDarkAndBright: boolean = false); overload;
label
    100,123;
var
  zeroPad, i,o,n, v, nVox, idx, x,y,z, vx, skipVx, PeakVx : int64;
  isDarkClusters: boolean = false;
  isPass2: boolean = false;
  clust: TCluster;
  label8, in8, mask8: TUInt8s;
  label16, in16: TInt16s;
  in32, v32: TFloat32s;
  clusterImg: TInt32s;
  clusterNumber, smallestClusterVox: integer;
  mm3PerVox, total: double;
  inten: single;
  {$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
function Peak2Label(): string;
var
   j: integer;
begin
     result := '';
     if (LabelMap.fHdr.bitpix = 16) then
        j := label16[PeakVx]
     else
         j := label8[PeakVx];
     if (j < 0) or (j >= LabelMap.fLabels.Count) then exit;
     result := LabelMap.fLabels[j];
end;

function Map2Label(): string; //idx:
const
     kMinFrac = 0.0051; //label regions that make up at least 1/2% of cluster
var
   j , nL: int64;
   region: string;
   sum: TSortType;
   labelCount: TSortArray;
begin
     result := '';
     nL := LabelMap.fLabels.Count;
     setlength(labelCount, nL);
     for j := 0 to (nL - 1) do begin
         labelCount[j].index := j;
         labelCount[j].value := 0;
     end;
     if (LabelMap.fHdr.bitpix = 16) then begin
       for j := 0 to (vx-1) do
           if (clusterImg[j] = idx) then
              labelCount[label16[j]].value := labelCount[label16[j]].value + 1;
     end else begin
       for j := 0 to (vx-1) do
           if (clusterImg[j] = idx) then
              labelCount[label8[j]].value := labelCount[label8[j]].value + 1;
     end;
     sum := 0;
     for j := 0 to (nL - 1) do
         sum := sum + labelCount[j].value;
     //result := floattostr(sum)+'==>'+inttostr(nx);
     if sum = 0 then begin
        labelCount := nil;
        exit;
     end;
     //fractional proportion damaged
     for j := 0 to (nL - 1) do
         labelCount[j].value := labelCount[j].value / sum;
     SortArray(labelCount);
     j := nL-1;
     while (j >= 0) and (labelCount[labelCount[j].index].value >= kMinFrac) do begin
           //if labelCount[j].index = 0 then continue; //do not report air
           region := LabelMap.fLabels[labelCount[j].index];
           if region = '' then
              region := '-';
           result := result + format('%s(%.0f) ',[ region, 100.0*labelCount[labelCount[j].index].value ]);
           //result := result + format('%d(%.0f) ',[ labelCount[j].index, 100.0*labelCount[labelCount[j].index].value ]);
           j := j - 1;
     end;
     //result := floattostr(labelCount[nL-1].value)+'..'+floattostr(labelCount[0].value);
end;
function voxInten(vx: int64): single; inline;
begin
     result := v32[vx]
end;//nested voxInten()

begin
     if IsLabels then begin
       GenerateAtlasClusters;
       exit;
    end;
    {$IFDEF TIMER}startTime := now;{$ENDIF}
    smallestClusterVox := mm3toVox(smallestClusterMM3);
    if fHdr.datatype = kDT_RGB then exit;
    clusterNotes := GetClusterNotes(self) ;
    if (LabelMap <> nil) then
       clusterNotes+= ' on '+GetClusterNotes(LabelMap);
    vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
    if vx < 1 then exit;
    skipVx := skipVox();
    in8 := fRawVolBytes;
    in16 := TInt16s(in8);
    in32 := TFloat32s(in8);
    setlength(v32,vx);
    if fHdr.datatype = kDT_UINT8 then begin
       for i := 0 to (vx-1) do
        v32[i] := in8[skipVx+i];
    end else if fHdr.datatype = kDT_INT16 then begin
      for i := 0 to (vx-1) do
       v32[i] := in16[skipVx+i];
    end else if fHdr.datatype = kDT_FLOAT then begin
      for i := 0 to (vx-1) do
       v32[i] := in32[skipVx+i];
    end;
    for i := 0 to (vx-1) do
        v32[i] := (v32[i]* fHdr.scl_slope) + fHdr.scl_inter;
    if specialsingle( smallestClusterMM3) then begin
       if (LabelMap = nil) or (not LabelMap.IsLabels) then begin
          printf('Expected an Atlas map');
          exit;
       end;
       //GenerateAtlasClusters(v32);  //xxxxxxxx
       if LabelMap.clusters = nil then
          LabelMap.GenerateAtlasClusters;
       if length(LabelMap.clusters) < 1 then begin
          v32 := nil;
          exit;
       end;
       label8 := LabelMap.fRawVolBytes;
       label16 := TInt16s(label8);
       n := length(LabelMap.clusters);
       setlength(clusters, n);
       mm3PerVox := VoxMM3();
       //clusters := copy(LabelMap.clusters, 0, n);
       for i := 0 to (n-1) do begin
           clusters[i] := LabelMap.clusters[i];
           o := round(clusters[i].Peak);
           //clusters[i].Structure := LabelMap.clusters[i].Structure +' ('+inttostr(o)+')';
           //clusters[i].Structure :=  format('%d (%g %g)',[vx, fHdr.scl_slope, fHdr.scl_inter]);
           total := 0.0;
           nVox := 0;
           if (LabelMap.fHdr.bitpix = 16) then begin
             for v := 0 to (vx-1) do begin
                 if label16[v] = o then begin
                    total += v32[v];
                    nVox := nVox + 1;
                 end;
             end;
           end else begin
             for v := 0 to (vx-1) do begin
                 if label8[v] = o then begin
                    total += v32[v];
                    nVox := nVox + 1;
                 end;
             end;
           end;
           if nVox > 0 then
           	  clusters[i].Peak := total/nVox
           else
               clusters[i].Peak := 0;
           clusters[i].PeakStructure := '~';
           clust.SzMM3:= nVox * mm3PerVox;
           //xxx

       end;
       v32 := nil;
       exit;
    end;
    setlength(mask8,vx);
    if specialSingle(thresh) then begin
       if (DisplayMax < 0) and (DisplayMin < 0) then
            thresh := max(DisplayMin, DisplayMax)
         else
             thresh := min(DisplayMin, DisplayMax);
    end;
    if isDarkAndBright then
       thresh := abs(thresh); //positive as first pass
    o := 0;
 100:
    if (thresh < 0) then isDarkClusters := true;
    FillChar(mask8[0], vx, 0);
    if (isDarkClusters) then begin
      for i := 0 to (vx-1) do
          if v32[i] <= thresh then mask8[i] := 255;
      for i := 0 to (vx-1) do
          v32[i] := -v32[i]; //invert so peaks are bright
    end else
        for i := 0 to (vx-1) do
            if v32[i] >= thresh then mask8[i] := 255;
    Clusterize(mask8, fHdr.dim[1], fHdr.dim[2], fHdr.dim[3], clusterNumber, clusterImg, NeighborMethod, smallestClusterVox);
    if clusterNumber < 1 then goto 123;
    zeroPad := trunc(log10(clusterNumber))+1;
    //ccPerVox := VoxMM3()/1000.0;
    mm3PerVox := VoxMM3();
    setlength(clusters,clusterNumber+o);
    for i := 0 to (clusterNumber-1) do begin
        nVox := 0;
        clust := InitCluster();
        idx := i + 1;
        v := 0;
        PeakVx := 0;
        for z := 0 to (dim.z-1) do
            for y := 0 to (dim.y-1) do
              for x := 0 to (dim.x-1) do begin
                if (clusterImg[v] = idx) then begin
                  nVox := nVox + 1;
                  clust.CogXYZ := clust.CogXYZ + Vec3(x,y,z);
                  inten := voxInten(v);
                  if inten > clust.Peak then begin
                     clust.Peak := inten;
                     clust.PeakXYZ := Vec3(x,y,z);
                     PeakVx := v;
                  end;
                end; //voxel in cluster
                v := v + 1;
              end;
        //result in voxels 0..Dim-1
        if nVox < 1 then continue;
        if isDarkClusters then clust.Peak := -clust.Peak;
        clust.CogXYZ := clust.CogXYZ / nVox;
        //result in frac
        clust.CogXYZ.x := clust.CogXYZ.x / (dim.x-1);
        clust.CogXYZ.y := clust.CogXYZ.y / (dim.y-1);
        clust.CogXYZ.z := clust.CogXYZ.z / (dim.z-1);
        clust.CogXYZ := FracMM(clust.CogXYZ);
        //result in frac
        clust.PeakXYZ.x := clust.PeakXYZ.x / (dim.x-1);
        clust.PeakXYZ.y := clust.PeakXYZ.y / (dim.y-1);
        clust.PeakXYZ.z := clust.PeakXYZ.z / (dim.z-1);
        clust.PeakXYZ := FracMM(clust.PeakXYZ);
        clust.SzMM3:= nVox * mm3PerVox;
        clusters[o] := clust;
        //clusters[o].Structure := format('%.1f×%.1f×%.1f %.1fcc max%.1f', [clust.CogXYZ.x, clust.CogXYZ.y, clust.CogXYZ.z, clust.SzCC, clust.Peak]);
        clusters[o].Structure := strutils.Dec2Numb(o+1,zeroPad,10);
        //clusters[o].Structure := inttostr(o+1);
        clusters[o].PeakStructure := '-';
        if (LabelMap <> nil) and (LabelMap.IsLabels) then begin
           label8 := LabelMap.fRawVolBytes;
           label16 := TInt16s(label8);
           clusters[o].Structure := Map2Label();
           clusters[o].PeakStructure := Peak2Label();
        end;
        o := o + 1;
    end;
    //procedure VoxelsNearestCogs(var c: TClusters; maxIdx: integer; var clusterImg: TInt32s);
    setlength(clusters,o);
    if (isDarkAndBright) then begin
      if fHdr.datatype = kDT_UINT8 then begin
         for i := 0 to (vx-1) do
             if (clusterImg[i] <= 0) and (v32[i] > 0) then in8[skipVx+i] := 0;
      end else if fHdr.datatype = kDT_INT16 then begin
        for i := 0 to (vx-1) do
            if (clusterImg[i] <= 0) and (v32[i] > 0) then in16[skipVx+i] := 0;
      end else if fHdr.datatype = kDT_FLOAT then begin
        for i := 0 to (vx-1) do
            if (clusterImg[i] <= 0) and (v32[i] > 0) then in32[skipVx+i] := 0;
      end;
    end;
    if (not isDarkAndBright)  then begin
      if fHdr.datatype = kDT_UINT8 then begin
         for i := 0 to (vx-1) do
             if clusterImg[i] <= 0 then in8[skipVx+i] := 0;
      end else if fHdr.datatype = kDT_INT16 then begin
        for i := 0 to (vx-1) do
            if clusterImg[i] <= 0 then in16[skipVx+i] := 0;
      end else if fHdr.datatype = kDT_FLOAT then begin
        for i := 0 to (vx-1) do
            if clusterImg[i] <= 0 then in32[skipVx+i] := 0;
      end;
    end;
    if (not isPass2) and (isDarkAndBright) then begin
       isPass2 := true;
       thresh := -thresh;
       clusterImg := nil;
       goto 100;
    end;
    SortClusters();
    {$IFDEF TIMER}
    printf(format('Cluster time %d', [MilliSecondsBetween(Now,startTime)]));
    if isDarkAndBright then
       printf('  Bimodal (dark and bright peaks');
    printf(format('  threshold=%g smallest-cluster(mm3)=%g neighbors=%d numClusters=%d', [thresh, smallestClusterMM3, NeighborMethod, o]));
    {$ENDIF}
    initHistogram();
    fWindowMinCache8 := infinity;
123:
    clusterImg := nil;
    mask8 := nil;
    v32 := nil;
end;



{$ELSE}
procedure TNIfTI.GenerateClusters(LabelMap: TNIfTI; thresh, smallestClusterMM3: single; NeighborMethod: integer; isDarkAndBright: boolean = false); overload;
label
    100,123;
function vx2xyz(vx:integer): TVec3; inline;
var
  z,y: integer;
begin
     z := trunc(vx/ (dim.x * dim.y));
     vx := vx - (z *(dim.x * dim.y));
     y := trunc(vx/ (dim.x));
     result.x := vx - (y *(dim.x));
     result.y := y;
     result.z := z;
end;
procedure VoxelsNearestCogs32(var c: TClusters; clusterOffset, maxIdx: integer; var clusterImg: TInt32s);
//optional step to ensure that "Center of Gravity" is a voxel of the correct label
// e.g. for a torus, the CoG should not be the hole.
var
   x,y,z, vx,i: integer;
   dx: single;
   minDx : TFloat32s;
   minVx: TInt32s;
begin
     setlength(minDx, maxIdx+1);
     for i := 0 to maxIdx do
         minDx[i] := infinity;
     setlength(minVx, maxIdx+1);
     vx := -1;
         for z := 0 to (dim.z-1) do
           for y := 0 to (dim.y-1) do
             for x := 0 to (dim.x-1) do begin
                 vx := vx + 1;
                 i := clusterImg[vx];
                 if (i = 0) then continue; //air
                 dx := (sqr(c[i+clusterOffset].CogXYZ.x - x)+sqr(c[i+clusterOffset].CogXYZ.y - y)+sqr(c[i+clusterOffset].CogXYZ.z - z));
                 if dx >= minDx[i] then continue;
                 minDx[i] := dx;
                 minVx[i] := vx;
             end;
     for i := 1 to maxIdx do
         if not specialsingle(minDx[i]) then //e.g. not infinity, e.g. detected
            c[i+clusterOffset].CogXYZ := vx2xyz(minVx[i]);
     minVx := nil;
     minDx := nil;
end;
var
  zeroPad, i,o,v, nVox, idx, x,y,z, vx, skipVx, PeakVx : int64;
  isDarkClusters: boolean = false;
  isPass2: boolean = false;
  clust: TCluster;
  label8, in8, mask8: TUInt8s;
  label16, in16: TInt16s;
  in32, v32: TFloat32s;
  clusterImg: TInt32s;
  clusterNumber, smallestClusterVox: integer;
  mm3PerVox: double;
  inten: single;
  {$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
function Peak2Label(): string;
var
   j: integer;
begin
     result := '';
     if (LabelMap.fHdr.bitpix = 16) then
        j := label16[PeakVx]
     else
         j := label8[PeakVx];
     if (j < 0) or (j >= LabelMap.fLabels.Count) then exit;
     result := LabelMap.fLabels[j];
end;

function Map2Label(): string; //idx:
const
     kMinFrac = 0.0051; //label regions that make up at least 1/2% of cluster
var
   j , nL: int64;
   region: string;
   sum: TSortType;
   labelCount: TSortArray;
begin
     result := '';
     nL := LabelMap.fLabels.Count;
     setlength(labelCount, nL);
     for j := 0 to (nL - 1) do begin
         labelCount[j].index := j;
         labelCount[j].value := 0;
     end;
     if (LabelMap.fHdr.bitpix = 16) then begin
       for j := 0 to (vx-1) do
           if (clusterImg[j] = idx) then
              labelCount[label16[j]].value := labelCount[label16[j]].value + 1;
     end else begin
       for j := 0 to (vx-1) do
           if (clusterImg[j] = idx) then
              labelCount[label8[j]].value := labelCount[label8[j]].value + 1;
     end;
     sum := 0;
     for j := 0 to (nL - 1) do
         sum := sum + labelCount[j].value;
     //result := floattostr(sum)+'==>'+inttostr(nx);
     if sum = 0 then begin
        labelCount := nil;
        exit;
     end;
     //fractional proportion damaged
     for j := 0 to (nL - 1) do
         labelCount[j].value := labelCount[j].value / sum;
     SortArray(labelCount);
     j := nL-1;
     while (j >= 0) and (labelCount[labelCount[j].index].value >= kMinFrac) do begin
           //if labelCount[j].index = 0 then continue; //do not report air
           region := LabelMap.fLabels[labelCount[j].index];
           if region = '' then
              region := '-';
           result := result + format('%s(%.0f) ',[ region, 100.0*labelCount[labelCount[j].index].value ]);
           //result := result + format('%d(%.0f) ',[ labelCount[j].index, 100.0*labelCount[labelCount[j].index].value ]);
           j := j - 1;
     end;
     //result := floattostr(labelCount[nL-1].value)+'..'+floattostr(labelCount[0].value);
end;
function voxInten(vx: int64): single; inline;
begin
     result := v32[vx]
end;//nested voxInten()

begin
    if IsLabels then begin
       GenerateAtlasClusters(nil);
       exit;
    end;
    {$IFDEF TIMER}startTime := now;{$ENDIF}
    smallestClusterVox := mm3toVox(smallestClusterMM3);
    if fHdr.datatype = kDT_RGB then exit;
    clusterNotes := GetClusterNotes(self) ;
    if (LabelMap <> nil) then
       clusterNotes+= ' on '+GetClusterNotes(LabelMap);
    vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
    if vx < 1 then exit;
    skipVx := skipVox();
    in8 := fRawVolBytes;
    in16 := TInt16s(in8);
    in32 := TFloat32s(in8);
    setlength(v32,vx);
    if fHdr.datatype = kDT_UINT8 then begin
       for i := 0 to (vx-1) do
        v32[i] := in8[skipVx+i];
    end else if fHdr.datatype = kDT_INT16 then begin
      for i := 0 to (vx-1) do
       v32[i] := in16[skipVx+i];
    end else if fHdr.datatype = kDT_FLOAT then begin
      for i := 0 to (vx-1) do
       v32[i] := in32[skipVx+i];
    end;
    for i := 0 to (vx-1) do
        v32[i] := (v32[i]* fHdr.scl_slope) + fHdr.scl_inter;
    setlength(mask8,vx);
    if specialSingle(thresh) then begin
       if (DisplayMax < 0) and (DisplayMin < 0) then
            thresh := max(DisplayMin-TSingleHelper.Epsilon, DisplayMax-TSingleHelper.Epsilon)
         else
             thresh := min(DisplayMin+TSingleHelper.Epsilon, DisplayMax+TSingleHelper.Epsilon);

    end;
    if isDarkAndBright then
       thresh := abs(thresh); //positive as first pass
    o := 0;
 100:
    if (thresh < 0) then isDarkClusters := true;
    FillChar(mask8[0], vx, 0);
    if (isDarkClusters) then begin
      for i := 0 to (vx-1) do
          if v32[i] <= thresh then mask8[i] := 255;
      for i := 0 to (vx-1) do
          v32[i] := -v32[i]; //invert so peaks are bright
    end else
        for i := 0 to (vx-1) do
            if v32[i] >= thresh then mask8[i] := 255;
    (*j := 0;
    for i := 0 to (vx-1) do
        if mask8[i] = 255 then
           j := j + 1;
    if (j <= 0) then begin
       printf(format('Cluster is empty, no voxels survive threshold=%g', [thresh]));
       goto 123;
    end; *)
    Clusterize(mask8, fHdr.dim[1], fHdr.dim[2], fHdr.dim[3], clusterNumber, clusterImg, NeighborMethod, smallestClusterVox);
    if clusterNumber < 1 then goto 123;
    zeroPad := trunc(log10(clusterNumber))+1;
    //ccPerVox := VoxMM3()/1000.0;
    mm3PerVox := VoxMM3();
    setlength(clusters,clusterNumber+o);
    for i := 0 to (clusterNumber-1) do begin
        nVox := 0;
        clust := InitCluster();
        idx := i + 1;
        v := 0;
        PeakVx := 0;
        for z := 0 to (dim.z-1) do
            for y := 0 to (dim.y-1) do
              for x := 0 to (dim.x-1) do begin
                if (clusterImg[v] = idx) then begin
                  nVox := nVox + 1;
                  clust.CogXYZ := clust.CogXYZ + Vec3(x,y,z);
                  inten := voxInten(v);
                  if inten > clust.Peak then begin
                     clust.Peak := inten;
                     clust.PeakXYZ := Vec3(x,y,z);
                     PeakVx := v;
                  end;
                end; //voxel in cluster
                v := v + 1;
              end;
        //result in voxels 0..Dim-1
        if nVox < 1 then continue;
        if isDarkClusters then clust.Peak := -clust.Peak;
        clust.CogXYZ := clust.CogXYZ / nVox;
        clust.SzMM3:= nVox * mm3PerVox;
        clusters[i] := clust;
    end;
    VoxelsNearestCogs32(clusters, o-1, clusterNumber, clusterImg);
    for i := 0 to (clusterNumber-1) do begin
        if (clusters[i].SzMM3 <= 0) then continue;
        clust := clusters[i];
        //result in frac
        clust.CogXYZ.x := clust.CogXYZ.x / (dim.x-1);
        clust.CogXYZ.y := clust.CogXYZ.y / (dim.y-1);
        clust.CogXYZ.z := clust.CogXYZ.z / (dim.z-1);
        //result in frac
        clust.PeakXYZ.x := clust.PeakXYZ.x / (dim.x-1);
        clust.PeakXYZ.y := clust.PeakXYZ.y / (dim.y-1);
        clust.PeakXYZ.z := clust.PeakXYZ.z / (dim.z-1);

        clust.CogXYZ := FracMM(clust.CogXYZ);
        clust.PeakXYZ := FracMM(clust.PeakXYZ);

        clusters[o] := clust;
        //clusters[o].Structure := format('%.1f×%.1f×%.1f %.1fcc max%.1f', [clust.CogXYZ.x, clust.CogXYZ.y, clust.CogXYZ.z, clust.SzCC, clust.Peak]);
        clusters[o].Structure := strutils.Dec2Numb(o+1,zeroPad,10);
        //clusters[o].Structure := inttostr(o+1);
        clusters[o].PeakStructure := '-';
        if (LabelMap <> nil) and (LabelMap.IsLabels) then begin
           label8 := LabelMap.fRawVolBytes;
           label16 := TInt16s(label8);
           clusters[o].Structure := Map2Label();
           clusters[o].PeakStructure := Peak2Label();
        end;
        o := o + 1;
    end;
    setlength(clusters,o);
    if (isDarkAndBright) then begin
      if fHdr.datatype = kDT_UINT8 then begin
         for i := 0 to (vx-1) do
             if (clusterImg[i] <= 0) and (v32[i] > 0) then in8[skipVx+i] := 0;
      end else if fHdr.datatype = kDT_INT16 then begin
        for i := 0 to (vx-1) do
            if (clusterImg[i] <= 0) and (v32[i] > 0) then in16[skipVx+i] := 0;
      end else if fHdr.datatype = kDT_FLOAT then begin
        for i := 0 to (vx-1) do
            if (clusterImg[i] <= 0) and (v32[i] > 0) then in32[skipVx+i] := 0;
      end;
    end;
    if (not isDarkAndBright)  then begin
      if fHdr.datatype = kDT_UINT8 then begin
         for i := 0 to (vx-1) do
             if clusterImg[i] <= 0 then in8[skipVx+i] := 0;
      end else if fHdr.datatype = kDT_INT16 then begin
        for i := 0 to (vx-1) do
            if clusterImg[i] <= 0 then in16[skipVx+i] := 0;
      end else if fHdr.datatype = kDT_FLOAT then begin
        for i := 0 to (vx-1) do
            if clusterImg[i] <= 0 then in32[skipVx+i] := 0;
      end;
    end;
    if (not isPass2) and (isDarkAndBright) then begin
       isPass2 := true;
       thresh := -thresh;
       clusterImg := nil;
       goto 100;
    end;
    SortClusters();
    {$IFDEF TIMER}
    printf(format('Cluster time %d', [MilliSecondsBetween(Now,startTime)]));
    if isDarkAndBright then
       printf('  Bimodal (dark and bright peaks');
    printf(format('  threshold=%g smallest-cluster(mm3)=%g neighbors=%d numClusters=%d', [thresh, smallestClusterMM3, NeighborMethod, o]));
    {$ENDIF}
    initHistogram();
    fWindowMinCache8 := infinity;
123:
    clusterImg := nil;
    mask8 := nil;
    v32 := nil;
end;
{$ENDIF}


procedure TNIfTI.GenerateClusters(thresh, smallestClusterMM3: single; NeighborMethod: integer = 1; isDarkAndBright: boolean = false); overload;
begin
     GenerateClusters(nil, thresh, smallestClusterMM3, NeighborMethod, isDarkAndBright);
end;

procedure TNIfTI.SortClusters();
var
   s: TSortArray;
   i, n: integer;
   inClust: TClusters;
begin
     n := length(clusters);
     if n < 1 then exit;
     //zz setlength(inClust, n);
     inClust := copy(clusters, 0, n);
     setlength(s, n);
     for i := 0 to (n-1) do begin
         s[i].index:=0;
         if SortClustersBySize then
            s[i].value := inClust[i].SzMM3
         else
             s[i].value := inClust[i].Peak;
     end;
     SortArray(s);
     for i := 0 to (n-1) do
         clusters[n-i-1] := inClust[s[i].index];
     s := nil;
     inClust := nil;
end;

{$DEFINE FASTCLUSTERS}
{$IFDEF FASTCLUSTERS}
{$DEFINE CLUSTERX}
{$IFDEF CLUSTERX}


procedure TNIfTI.GenerateAtlasClusters();
function vx2xyz(vx:integer): TVec3; inline;
var
  vol,z,y: integer;
begin
     vol := trunc(vx / (dim.x * dim.y * dim.z));
     vx := vx - (vol *(dim.x * dim.y * dim.z));
     z := trunc(vx/ (dim.x * dim.y));
     vx := vx - (z *(dim.x * dim.y));
     y := trunc(vx/ (dim.x));
     result.x := vx - (y *(dim.x));
     result.y := y;
     result.z := z;
end;
procedure VoxelsNearestCogs(var c: TClusters; maxIdx: integer);
//optional step to ensure that "Center of Gravity" is a voxel of the correct label
// e.g. for a torus, the CoG should not be the hole.
var
   v,x,y,z, vx,i: integer;
   dx: single;
   minDx : TFloat32s;
   minVx: TInt32s;
   vol16 : TInt16s;
   err: boolean = false;
begin
     if (fHdr.datatype <> kDT_UINT8) and (fHdr.datatype <> kDT_INT16) then exit;
     setlength(minDx, maxIdx+1);
     for i := 0 to maxIdx do
         minDx[i] := infinity;
     setlength(minVx, maxIdx+1);
     vol16 := TInt16s(fRawVolBytes);
     for v := 0 to (fVolumesLoaded-1) do begin //n.b. TTatlas+tlrc.HEAD has two volumes, scan both
         vx := (v * (dim.x * dim.y * dim.z)) -1;
         for z := 0 to (dim.z-1) do
           for y := 0 to (dim.y-1) do
             for x := 0 to (dim.x-1) do begin
                 vx := vx + 1;
                 if (fHdr.datatype = kDT_INT16) then //opimization: loop hoisting
                    i := vol16[vx]
                 else
                     i := fRawVolBytes[vx];
                 if (i = 0) then continue; //air
                 if (i > maxIdx) then err := true;
                 if (i > maxIdx) then continue;
                 dx := (sqr(c[i].CogXYZ.x - x)+sqr(c[i].CogXYZ.y - y)+sqr(c[i].CogXYZ.z - z));
                 if dx >= minDx[i] then continue;
                 minDx[i] := dx;
                 minVx[i] := vx;
             end;
     end;
     if (err) then
        printf('VoxelsNearestCogs: Catastrophic error, wrong number of labels');
      for i := 1 to maxIdx do
         if not specialsingle(minDx[i]) then begin//e.g. not infinity, e.g. detected
            c[i].CogXYZ := vx2xyz(minVx[i]);
            //c[i].CogXYZ.x := 0;
         end;
     minVx := nil;
     minDx := nil;
end;
var
   c: TClusters;
   vol16 : TInt16s;
   vx: int64;
   v, z,y,x,i, o, maxIdx: integer;
   mm3: single;
   {$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
begin
    if (not IsLabels) then exit;
    if (fHdr.datatype <> kDT_UINT8) and (fHdr.datatype <> kDT_INT16) then exit;
    {$IFDEF TIMER}startTime := now;{$ENDIF}
    clusterNotes := GetClusterNotes(self);
    maxIdx := fLabels.Count - 1; //0=air
    if maxIdx < 1 then exit;
    setlength(c,maxIdx+1);
    c[0] := InitCluster;
    for i := 1 to maxIdx do
        c[i] := c[0];
    if (fHdr.datatype = kDT_INT16) then begin
       vol16 := TInt16s(fRawVolBytes);
       printf(format('<><><> %d %d %d', [dim.x, dim.y, dim.z]));
       for v := 0 to (fVolumesLoaded-1) do begin //n.b. TTatlas+tlrc.HEAD has two volumes, scan both
          vx := (v * (dim.x * dim.y * dim.z)) -1;
          for z := 0 to (dim.z-1) do
            for y := 0 to (dim.y-1) do
              for x := 0 to (dim.x-1) do begin
                  vx := vx + 1;
                  o := vol16[vx];
                  if o <= 0 then continue;
                  if o > maxIdx then continue;
                  c[o].SzMM3 := c[o].SzMM3 + 1;
                  c[o].CogXYZ.x := c[o].CogXYZ.x + x;
                  c[o].CogXYZ.y := c[o].CogXYZ.y + y;
                  c[o].CogXYZ.z := c[o].CogXYZ.z + z;
              end;
      end;
    end else begin
      for v := 0 to (fVolumesLoaded-1) do begin //n.b. TTatlas+tlrc.HEAD has two volumes, scan both
          vx := (v * (dim.x * dim.y * dim.z)) -1;
          for z := 0 to (dim.z-1) do
            for y := 0 to (dim.y-1) do
              for x := 0 to (dim.x-1) do begin
                  vx := vx + 1;
                  o := fRawVolBytes[vx];
                  if o = 0 then continue;
                  if o > maxIdx then continue;
                  c[o].SzMM3 := c[o].SzMM3 + 1;
                  c[o].CogXYZ.x := c[o].CogXYZ.x + x;
                  c[o].CogXYZ.y := c[o].CogXYZ.y + y;
                  c[o].CogXYZ.z := c[o].CogXYZ.z + z;
              end;
      end;
    end;
    o := 0;
    for i := 1 to maxIdx do
        if c[i].SzMM3 > 0 then
           o := o + 1;
    setlength(clusters,o);
    o := 0;
    mm3 := VoxMM3();
    for i := 1 to maxIdx do
        if c[i].SzMM3 > 0 then begin
          c[i].Structure := fLabels[i];
          //c[i].Structure := '>>>>>>'+inttostr(length(fLabels[i]));
          //printf(format('%d %s %g', [i, c[i].Structure, c[i].SzMM3]));
          c[i].PeakStructure := '-';
          //result in voxels 0..Dim-1
          c[i].CogXYZ.x := c[i].CogXYZ.x / c[i].SzMM3;
          c[i].CogXYZ.y := c[i].CogXYZ.y / c[i].SzMM3;
          c[i].CogXYZ.z := c[i].CogXYZ.z / c[i].SzMM3;
          //set peak to actual CoG, without requiring true voxel
          c[i].PeakXYZ := c[i].CogXYZ;
          c[i].PeakXYZ.x := c[i].PeakXYZ.x / (dim.x-1);
          c[i].PeakXYZ.y := c[i].PeakXYZ.y / (dim.y-1);
          c[i].PeakXYZ.z := c[i].PeakXYZ.z / (dim.z-1);
          c[i].PeakXYZ := FracMM(c[i].PeakXYZ);
          c[i].Peak := i;
        end;
    VoxelsNearestCogs(c, maxIdx);
    for i := 1 to maxIdx do
        if c[i].SzMM3 > 0 then begin
          //result in frac
          c[i].CogXYZ.x := c[i].CogXYZ.x / (dim.x-1);
          c[i].CogXYZ.y := c[i].CogXYZ.y / (dim.y-1);
          c[i].CogXYZ.z := c[i].CogXYZ.z / (dim.z-1);
          c[i].CogXYZ := FracMM(c[i].CogXYZ);
          c[i].SzMM3 := c[i].SzMM3 * mm3;
          //c[i].PeakXYZ := c[i].CogXYZ;
          //c[i].Peak := o; //"Peak" is output index, only for sortclusters...
          clusters[o] := c[i];
           o := o + 1;
        end;
    SortClusters(); //optional: sort by size or cluster index
    {$IFDEF TIMER}printf(format('Clusterize Atlas time %d',[MilliSecondsBetween(Now,startTime)]));{$ENDIF}
end;

{$ELSE}
 procedure TNIfTI.GenerateAtlasClusters();
function VoxelNearestCog(CogXYZ:TVec3; i: integer):TVec3;
var
   v,x,y,z, o, vx: integer;
   minDx, dx: single;
   vol16 : TInt16s;
begin
     result := CogXYZ;
     minDx := infinity;
     if (fHdr.datatype = kDT_INT16) then begin
        vol16 := TInt16s(fRawVolBytes);
        for v := 0 to (fVolumesLoaded-1) do begin //n.b. TTatlas+tlrc.HEAD has two volumes, scan both
          vx := (v * (dim.x * dim.y * dim.z)) -1;
          x := trunc(CogXYZ.x);
          y := trunc(CogXYZ.x);
          z := trunc(CogXYZ.x);
          vx := vx + x + (y* dim.x) + (z * dim.x * dim.y);
          o := vol16[vx];
          if (o = i) then
             exit; //already in location
        end;
        for v := 0 to (fVolumesLoaded-1) do begin //n.b. TTatlas+tlrc.HEAD has two volumes, scan both
           vx := (v * (dim.x * dim.y * dim.z)) -1;
           for z := 0 to (dim.z-1) do
             for y := 0 to (dim.y-1) do
               for x := 0 to (dim.x-1) do begin
                   vx := vx + 1;
                   o := vol16[vx];
                   if o <> i then continue;
                   dx := sqrt(sqr(CogXYZ.x - x)+sqr(CogXYZ.y - y)+sqr(CogXYZ.z - z));
                   if dx >= minDx then continue;
                   minDx := dx;
                   result := Vec3(x,y,z);
               end;
       end;
     end; //int16
     if (fHdr.datatype = kDT_UINT8) then begin
        for v := 0 to (fVolumesLoaded-1) do begin //n.b. TTatlas+tlrc.HEAD has two volumes, scan both
          vx := (v * (dim.x * dim.y * dim.z)) -1;
          x := trunc(CogXYZ.x);
          y := trunc(CogXYZ.x);
          z := trunc(CogXYZ.x);
          vx := vx + x + (y* dim.x) + (z * dim.x * dim.y);
          o := fRawVolBytes[vx];
          if (o = i) then
             exit; //already in location
        end;
        for v := 0 to (fVolumesLoaded-1) do begin //n.b. TTatlas+tlrc.HEAD has two volumes, scan both
           vx := (v * (dim.x * dim.y * dim.z)) -1;
           for z := 0 to (dim.z-1) do
             for y := 0 to (dim.y-1) do
               for x := 0 to (dim.x-1) do begin
                   vx := vx + 1;
                   o := fRawVolBytes[vx];
                   if o <> i then continue;
                   dx := sqrt(sqr(CogXYZ.x - x)+sqr(CogXYZ.y - y)+sqr(CogXYZ.z - z));
                   if dx >= minDx then continue;
                   minDx := dx;
                   result := Vec3(x,y,z);
               end;
       end;
     end; //int8
end;

var
   c: TClusters;
   vol16 : TInt16s;
   vx: int64;
   v, z,y,x,i, o, maxIdx: integer;
   mm3: single;
   {$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
begin
    if (not IsLabels) then exit;
    if (fHdr.datatype <> kDT_UINT8) and (fHdr.datatype <> kDT_INT16) then exit;
    {$IFDEF TIMER}startTime := now;{$ENDIF}
    clusterNotes := GetClusterNotes(self);
    maxIdx := fLabels.Count - 1; //0=air
    if maxIdx < 1 then exit;
    setlength(c,maxIdx+1);
    c[0] := InitCluster;
    for i := 1 to maxIdx do
        c[i] := c[0];
    if (fHdr.datatype = kDT_INT16) then begin
       vol16 := TInt16s(fRawVolBytes);
       for v := 0 to (fVolumesLoaded-1) do begin //n.b. TTatlas+tlrc.HEAD has two volumes, scan both
          vx := (v * (dim.x * dim.y * dim.z)) -1;
          for z := 0 to (dim.z-1) do
            for y := 0 to (dim.y-1) do
              for x := 0 to (dim.x-1) do begin
                  vx := vx + 1;
                  o := vol16[vx];
                  if o <= 0 then continue;
                  if o > maxIdx then continue;
                  c[o].SzMM3 := c[o].SzMM3 + 1;
                  c[o].CogXYZ.x := c[o].CogXYZ.x + x;
                  c[o].CogXYZ.y := c[o].CogXYZ.y + y;
                  c[o].CogXYZ.z := c[o].CogXYZ.z + z;
              end;
      end;
    end else begin
      for v := 0 to (fVolumesLoaded-1) do begin //n.b. TTatlas+tlrc.HEAD has two volumes, scan both
          vx := (v * (dim.x * dim.y * dim.z)) -1;
          for z := 0 to (dim.z-1) do
            for y := 0 to (dim.y-1) do
              for x := 0 to (dim.x-1) do begin
                  vx := vx + 1;
                  o := fRawVolBytes[vx];
                  if o = 0 then continue;
                  if o > maxIdx then continue;
                  c[o].SzMM3 := c[o].SzMM3 + 1;
                  c[o].CogXYZ.x := c[o].CogXYZ.x + x;
                  c[o].CogXYZ.y := c[o].CogXYZ.y + y;
                  c[o].CogXYZ.z := c[o].CogXYZ.z + z;
              end;
      end;
    end;
    o := 0;
    for i := 1 to maxIdx do
        if c[i].SzMM3 > 0 then
           o := o + 1;
    setlength(clusters,o);
    o := 0;
    mm3 := VoxMM3();
    for i := 1 to maxIdx do
        if c[i].SzMM3 > 0 then begin
          c[i].Structure := fLabels[i];
          c[i].PeakStructure := '-';
          //result in voxels 0..Dim-1
          c[i].CogXYZ.x := c[i].CogXYZ.x / c[i].SzMM3;
          c[i].CogXYZ.y := c[i].CogXYZ.y / c[i].SzMM3;
          c[i].CogXYZ.z := c[i].CogXYZ.z / c[i].SzMM3;
          c[i].CogXYZ := VoxelNearestCog(c[i].CogXYZ, i);
          //result in frac
          c[i].CogXYZ.x := c[i].CogXYZ.x / (dim.x-1);
          c[i].CogXYZ.y := c[i].CogXYZ.y / (dim.y-1);
          c[i].CogXYZ.z := c[i].CogXYZ.z / (dim.z-1);
          c[i].CogXYZ := FracMM(c[i].CogXYZ);
          c[i].SzMM3 := c[i].SzMM3 * mm3;
          c[i].PeakXYZ := c[i].CogXYZ;
          c[i].Peak := o; //"Peak" is output index, only for sortclusters...
          clusters[o] := c[i];
           o := o + 1;
        end;
    SortClusters(); //optional: sort by size or cluster index
    {$IFDEF TIMER}printf(format('Clusterize Atlas time %d',[MilliSecondsBetween(Now,startTime)]));{$ENDIF}
end;
{$ENDIF}

{$ELSE}
procedure TNIfTI.GenerateAtlasClusters();
var
   c: TClusters;
   vol16 : TInt16s;
   vx: int64;
   v, z,y,x,i, o, maxIdx: integer;
   mm3: single;
   {$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
begin
    if (not IsLabels) then exit;
    if (fHdr.datatype <> kDT_UINT8) and (fHdr.datatype <> kDT_INT16) then exit;
    {$IFDEF TIMER}startTime := now;{$ENDIF}
    clusterNotes := GetClusterNotes(self);
    maxIdx := fLabels.Count - 1; //0=air
    if maxIdx < 1 then exit;
    setlength(c,maxIdx+1);
    c[0] := InitCluster;
    for i := 1 to maxIdx do
        c[i] := c[0];
    if (fHdr.datatype = kDT_INT16) then begin
       vol16 := TInt16s(fRawVolBytes);
       for v := 0 to (fVolumesLoaded-1) do begin //n.b. TTatlas+tlrc.HEAD has two volumes, scan both
          vx := (v * (dim.x * dim.y * dim.z)) -1;
          for z := 0 to (dim.z-1) do
            for y := 0 to (dim.y-1) do
              for x := 0 to (dim.x-1) do begin
                  vx := vx + 1;
                  o := vol16[vx];
                  if o <= 0 then continue;
                  if o > maxIdx then continue;
                  c[o].SzMM3 := c[o].SzMM3 + 1;
                  c[o].CogXYZ.x := c[o].CogXYZ.x + x;
                  c[o].CogXYZ.y := c[o].CogXYZ.y + y;
                  c[o].CogXYZ.z := c[o].CogXYZ.z + z;
              end;
      end;
    end else begin
      for v := 0 to (fVolumesLoaded-1) do begin //n.b. TTatlas+tlrc.HEAD has two volumes, scan both
          vx := (v * (dim.x * dim.y * dim.z)) -1;
          for z := 0 to (dim.z-1) do
            for y := 0 to (dim.y-1) do
              for x := 0 to (dim.x-1) do begin
                  vx := vx + 1;
                  o := fRawVolBytes[vx];
                  if o = 0 then continue;
                  if o > maxIdx then continue;
                  c[o].SzMM3 := c[o].SzMM3 + 1;
                  c[o].CogXYZ.x := c[o].CogXYZ.x + x;
                  c[o].CogXYZ.y := c[o].CogXYZ.y + y;
                  c[o].CogXYZ.z := c[o].CogXYZ.z + z;
              end;
      end;
    end;
    o := 0;
    for i := 1 to maxIdx do
        if c[i].SzMM3 > 0 then
           o := o + 1;
    setlength(clusters,o);
    o := 0;
    mm3 := VoxMM3();
    for i := 1 to maxIdx do
        if c[i].SzMM3 > 0 then begin
          c[i].Structure := fLabels[i];
          c[i].PeakStructure := '-';
          //result in voxels 0..Dim-1
          c[i].CogXYZ.x := c[i].CogXYZ.x / c[i].SzMM3;
          c[i].CogXYZ.y := c[i].CogXYZ.y / c[i].SzMM3;
          c[i].CogXYZ.z := c[i].CogXYZ.z / c[i].SzMM3;
          //result in frac
          c[i].CogXYZ.x := c[i].CogXYZ.x / (dim.x-1);
          c[i].CogXYZ.y := c[i].CogXYZ.y / (dim.y-1);
          c[i].CogXYZ.z := c[i].CogXYZ.z / (dim.z-1);
          c[i].CogXYZ := FracMM(c[i].CogXYZ);
          c[i].SzMM3 := c[i].SzMM3 * mm3;
          c[i].PeakXYZ := c[i].CogXYZ;
          c[i].Peak := o; //"Peak" is output index, only for sortclusters...
          clusters[o] := c[i];
           o := o + 1;
        end;
    SortClusters(); //optional: sort by size or cluster index
    {$IFDEF TIMER}printf(format('Clusterize Atlas time %d',[MilliSecondsBetween(Now,startTime)]));{$ENDIF}
end;
{$ENDIF}

function TNIfTI.rawAtlasMax: integer;
var
   i,vx: integer;
   vol16: TInt16s;
begin
     //report maximum intensity in indexed atlas, an integer image
     result := 0;
     if (fHdr.datatype <> kDT_UINT8) and (fHdr.datatype <> kDT_INT16) then begin
        printf(format('Unsupported atlas datatype: %d (expected UINT8 or INT16)', [fHdr.datatype]));
        exit;
     end;
     vx := fHdr.Dim[1]*fHdr.Dim[2]*fHdr.Dim[3]*max(fHdr.Dim[4],1);
     if (vx < 1) then exit;
     if (fHdr.datatype <> kDT_UINT8) then begin
        result := fRawVolBytes[0];
        for i := 0 to (vx-1) do
            if fRawVolBytes[i] > result then
               result := fRawVolBytes[i];
     end;
     if (fHdr.datatype <> kDT_INT16) then begin
        vol16 := TInt16s(fRawVolBytes);
        result := vol16[0];
        for i := 0 to (vx-1) do
            if fRawVolBytes[i] > result then
               result := vol16[i];
     end;
end;

function TNIfTI.Load(niftiFileName: string; tarMat: TMat4; tarDim: TVec3i; isInterpolate: boolean; volumeNumber: integer = 0; isKeepContrast: boolean = false): boolean; overload;
var
   scaleMx: single;
   lLUTname: string;
   i: integer;
   {$IFDEF TIMER}StartTime: TDateTime;{$ENDIF}
begin
  {$IFDEF CACHEUINT8} //release cache, reload on next refresh
  fWindowMinCache8 := infinity;
  fCache8 := nil;
  {$ENDIF}
  fLabels.Clear;
  fLabelMask := nil;
  fBidsName := '';
  IsInterpolated := false;
  fFilename := niftiFileName;
  fVolumeDisplayed := volumeNumber;
  if fVolumeDisplayed < 0 then fVolumeDisplayed := 0;
  fIsOverlay := (tarDim.X) > 0;
  HiddenByCutout := not fIsOverlay;
  fShortName := changefileextX(extractfilename(fFilename),'');
  result := true;
  if niftiFileName = '+' then begin
    MakeBorg(64);
    fVolumeDisplayed := 0;
    fShortName := 'Borg_Temp';
    result := false;
  end else if not OpenNIfTI() then begin
     MakeBorg(64);
     fVolumeDisplayed := 0;
     fShortName := 'Borg_Error';
     result := false;
  end;
  if (fHdr.datatype = kDT_RGB) and (fIsOverlay) then begin
     showmessage('Unable to load RGB image as overlay (overlays must be scalar).');
     exit(false);
  end;
  if (PosEx(pathdelim+'atlases'+pathdelim, niftiFileName) > 0) and (HdrVolumes(fHdr) < 2) then
     fHdr.intent_code := kNIFTI_INTENT_LABEL;
  if (IsLabels) then begin
     if (fHdr.bitpix <= 32) and(fLabels.Count < 1)   then begin
        LoadLabelsTxt(fFilename, fLabels);
        if (fLabels.Count < 1) and (fHdr.HdrSz = 348) and (( fHdr.vox_offset- fHdr.HdrSz) > 128) then
           LoadLabels(fFilename, fLabels, fHdr.HdrSz+4, round( fHdr.vox_offset)-fHdr.HdrSz-4)
        else if (fLabels.Count < 1) and (( fHdr.vox_offset- fHdr.HdrSz) > 128) then
           LoadLabels(fFilename, fLabels, fHdr.HdrSz, round( fHdr.vox_offset)-fHdr.HdrSz);
        if (fLabels.Count < 1) then begin
           for i := 0 to max(rawAtlasMax, 1) do
               fLabels.Add(format('roi%d',[i]));
        end;
     end;
      if (fLabels.Count < 1) then begin
        //fHdr.intent_code :=  kNIFTI_INTENT_NON
        //tobo
      end else begin
        lLUTname := changefileextX(fFilename,'.lut'); //file.nii.gz -> file.lut
        if not Fileexists(lLUTname) then
           lLUTname := changefileext(fFilename,'.lut'); //file.nii.gz -> file.nii.lut ;
        if Fileexists(lLUTname) then
           clut.LoadCustomLabelLut(lLUTname);
      end;
  end;
  fixBogusHeaders(fHdr);
  ConvertUint16Int16();
  if (MaxTexMb <= 0) then begin
    printf('Loading image for conversion not visualization');
    fScale := Vec3(1,1,1);
    fDim.x := fHdr.Dim[1];
    fDim.y := fHdr.Dim[2];
    fDim.z := fHdr.Dim[3];
    fPermInOrient := pti(1,2,3);
    fMat := SForm2Mat(fHdr);
    fMatInOrient := fMat;
    exit;
  end;
  fHdrNoRotation := fHdr; //raw header without reslicing or orthogonal rotation
  if (prod(tarDim) = 0) and (fHdr.dim[3] > 1) and (MaxTexMb > 0) then //reduce size of huge background images
	if ShrinkLargeMb(fHdr,fRawVolBytes, MaxTexMb, isAntiAliasHugeTexMb, IsLabels) then begin
        fVolumesLoaded := 1;
        IsShrunken := true;
  end;
  scaleMx := max(max(abs(fHdr.Dim[1]*fHdr.PixDim[1]),abs(fHdr.Dim[2]*fHdr.pixDim[2])),abs(fHdr.Dim[3]*fHdr.pixDim[3]));
  if (scaleMx <> 0) then begin
    fScale.X := abs((fHdr.Dim[1]*fHdr.PixDim[1]) / scaleMx);
    fScale.Y := abs((fHdr.Dim[2]*fHdr.PixDim[2]) / scaleMx);
    fScale.Z := abs((fHdr.Dim[3]*fHdr.PixDim[3]) / scaleMx);
  end;
  MinPixDim := min(min(fHdr.PixDim[1], fHdr.PixDim[2]), fHdr.PixDim[3] );
  MaxMM := max(max(fHdr.Dim[1]*fHdr.PixDim[1], fHdr.Dim[2]*fHdr.PixDim[2]), fHdr.Dim[3]*fHdr.PixDim[3] );
  mmAsFrac.x := 1.0 / (fHdr.Dim[1]*fHdr.PixDim[1]);
  mmAsFrac.y := 1.0 / (fHdr.Dim[2]*fHdr.PixDim[2]);
  mmAsFrac.z := 1.0 / (fHdr.Dim[3]*fHdr.PixDim[3]);
  fDim.x := fHdr.Dim[1];
  fDim.y := fHdr.Dim[2];
  fDim.z := fHdr.Dim[3];
  fPermInOrient := pti(1,2,3);
  fMat := SForm2Mat(fHdr);
  fMatInOrient := fMat;
  {$IFDEF TIMER}startTime := now;{$ENDIF}
  if fHdr.scl_slope = 0 then fHdr.scl_slope := 1;
  LoadRGBVector();
  Convert2RGB();
  Convert2UInt8();
  if (fHdr.datatype = kDT_UINT16) or (fHdr.datatype = kDT_INT32) or (fHdr.datatype = kDT_UINT32) or (fHdr.datatype = kDT_DOUBLE) or (fHdr.datatype = kDT_INT64) or (fHdr.datatype = kDT_COMPLEX64) or (fHdr.datatype = kDT_COMPLEX128)  then
     Convert2Float();
  //printf(format('->> %d', [length(fRawVolBytes)]));  //saveRotat
  if prod(tarDim) > 0 then begin
    if IsLabels then
       VolumeReslice(tarMat, tarDim, false)
    else
       VolumeReslice(tarMat, tarDim, isInterpolate)
  end else
      VolumeReorient();
  if (isKeepContrast) then begin //do not change color settings when switching volumes in multi-volume overlay
    SetDisplayMinMax();
    exit;
  end;
  {$IFDEF TIMER}
  i := MilliSecondsBetween(Now,startTime);
  if (i > 5) then
     printf(format('Reorient time %d',[i]));
  {$ENDIF}
  //printf(format('-->> %d', [length(fRawVolBytes)]));  //saveRotat
  fMat := SForm2Mat(fHdr);
  fInvMat := fMat.inverse;
  {$IFDEF TIMER}startTime := now;{$ENDIF}
  if IsLabels then begin
    fAutoBalMin := 0;
    fAutoBalMax := 100;
    fMin := 0;
    fMax := 100;
    fWindowMin := 0;
    fWindowMax := 100;
    clut.SetLabels();
    DisplayLabel2Uint8; //TTatlas+tlrc.HEAD
  end else if fHdr.datatype = kDT_UINT8 then
     initUInt8()
  else if fHdr.datatype = kDT_INT16 then
       initInt16()
  else if fHdr.datatype = kDT_FLOAT then
  	initFloat32()
  else if fHdr.datatype = kDT_RGB then begin
       fAutoBalMin := 0;
       fAutoBalMax := 255;
       fMin := 0;
       fMax := 255;
       fWindowMin := 0;
       fWindowMax := 255;
       initHistogram();
  end else
      printf('Unsupported data format '+inttostr(fHdr.datatype));
  if fMin = fMax then begin
     printf(format('No variability in volume, all voxels have intensity %g',[fMin]));
      fMax := fMin + 1;
  end;
  //if IsLabels then
  //   GenerateClusters();
  if (IsLabels) then begin
     //
  end else if fAutoBalMin = fAutoBalMax then begin //e.g. thresholded data
     fAutoBalMin := fMin;
     fAutoBalMax := fMax;
     fWindowMin := fMin;
     fWindowMax := fMax;
  end else if (fIsOverlay) and (fAutoBalMin < -1.0) and (fAutoBalMax > 1.0) then begin
    fAutoBalMin := fAutoBalMax;
    fAutoBalMax := fAutoBalMax;
  end;
  {$IFDEF TIMER}printf(format('Init time %d',[MilliSecondsBetween(Now,startTime)]));{$ENDIF}
  {$IFDEF TIMER}startTime := now;{$ENDIF}
  if (fHdr.cal_min = 0) and (fHdr.cal_max = 255) then
  	fHdr.cal_max := 0; //BrainVoyager kludge
  if (IsLabels) then
     //
  else if (fHdr.cal_max > fHdr.cal_min)then begin
     if (fHdr.dataType > kDT_UNSIGNED_CHAR) and (fAutoBalMax > fAutoBalMin) and (((fHdr.cal_max - fHdr.cal_min)/(fAutoBalMax - fAutoBalMin)) > 5) then begin
          printf('Ignoring implausible cal_min..cal_max: FSL eddy?');
     end else begin
        printf(format('cal_min %g cal_max %g', [fHdr.cal_min, fHdr.cal_max]));
        fAutoBalMin := fHdr.cal_min;
        fAutoBalMax := fHdr.cal_max;
     end;
  end;
  fWindowMin := fAutoBalMin;
  fWindowMax := fAutoBalMax;
  if (CLUT.SuggestedMinIntensity < CLUT.SuggestedMaxIntensity) and (fMax > CLUT.SuggestedMinIntensity) and (fMin < CLUT.SuggestedMaxIntensity) then begin
     fWindowMin := CLUT.SuggestedMinIntensity;
     fWindowMax := CLUT.SuggestedMaxIntensity;
  end;
  if (fIsOverlay) and (fMin > -25) and (fMin < -5) and (fMax > 5) and (fMax < 25) and (fAutoBalMin < -0.5) and (fAutoBalMax > 0.5) then begin
     printf('Adjusting initial image intensity: Assuming statistical overlay.');
     fWindowMin := 1;
     fWindowMax := 1;
  end;
  //{$IFDEF TIMER}startTime := now;{$ENDIF}
  (*if (fHdr.datatype = kDT_FLOAT32) and (fVolumesLoaded = 3) then
    fDTImode := kDTIscalar;*)
  SetDisplayMinMax();
  //{$IFDEF TIMER}printf(format('Set Min/Max time %d',[MilliSecondsBetween(Now,startTime)]));{$ENDIF}
end;

function TNIfTI.Load(niftiFileName: string): boolean; overload;
var
   tarMat: TMat4;
   tarDim: TVec3i;
begin
     tarMat := TMat4.Identity;
     tarDim := pti(0,0,0);
     result := Load(niftiFileName,tarMat, tarDim, false);
end;

procedure TNIfTI.SaveAsSourceOrient(NiftiOutName, HdrDescrip, IntentName: string; rawVolBytesIn: TUInt8s; dataType: integer; intentCode: integer = 0; mn: single = 0; mx: single = 0);
//for drawing rotate back to orientation of input image!
var
   nVox: int64;
   oHdr  : TNIFTIhdr;
   oDim, oPerm: TVec3i;
   oScale: TVec3;
   i: integer;
   rawVolBytes: TUInt8s;
   {$IFDEF OLDSAVE}
   oPad32: Uint32; //nifti header is 348 bytes padded with 4
   mStream : TMemoryStream;
   zStream: TGZFileStream;
   lExt: string;
   {$ENDIF}
begin
 rawVolBytes := Copy(rawVolBytesIn, Low(rawVolBytesIn), Length(rawVolBytesIn));
 oHdr := fHdr;
 oHdr.descrip := HdrDescrip;
 oHdr.intent_name := IntentName;
 oHdr.intent_code:= intentCode;
 oHdr.datatype := dataType;
 if (dataType = kDT_UNSIGNED_CHAR) then
    oHdr.bitpix := 8
 else if (dataType = kDT_INT16) then
   oHdr.bitpix := 16
 else if (dataType = kDT_FLOAT) then
   oHdr.bitpix := 32
 else begin
      showmessage('Unsupported export datatype '+inttostr(datatype));
     exit;
 end;
 oHdr.scl_slope := 1;
 oHdr.scl_inter := 0;
 oHdr.cal_min:= mn;
 oHdr.cal_max:= mx;
 oHdr.glmin:= 0;
 oHdr.glmax:= 0;
 oHdr.dim[0] := 3;
 for i := 4 to 7 do
     oHdr.dim[i] := 1;
 nVox := prod(fDim);
 oDim := fDim;
 oScale := fScale;
 //rawVolBytes:= TUInt8s(rawVol);
 //output perm reverses input permute
 //fPermInOrient
 oPerm := pti(1,2,3);
 for i := 0 to 2 do
     if abs(fPermInOrient.v[i]) = 1 then
       oPerm.X := (i+1) * sign(fPermInOrient.v[i]);
 for i := 0 to 2 do
     if abs(fPermInOrient.v[i]) = 2 then
       oPerm.Y := (i+1) * sign(fPermInOrient.v[i]);
 for i := 0 to 2 do
     if abs(fPermInOrient.v[i]) = 3 then
       oPerm.Z := (i+1) * sign(fPermInOrient.v[i]);
 //showmessage(format('%d %d %d -> %d %d %d', [fPermInOrient.X, fPermInOrient.Y, fPermInOrient.Z, oPerm.X, oPerm.y, oPerm.Z]));
 ApplyVolumeReorient(oPerm, fMatInOrient, oDim, oScale, oHdr, rawVolBytes);
 oHdr.vox_offset :=  sizeof(oHdr) + 4;
 {$IFDEF OLDSAVE}
 mStream := TMemoryStream.Create;
 mStream.Write(oHdr,sizeof(oHdr));
 oPad32 := 4;
 mStream.Write(oPad32, 4);
 mStream.Write(rawVolBytes[0],nvox * (oHdr.bitpix div 8));
 mStream.Position := 0;
 FileMode := fmOpenWrite;
 lExt := uppercase(extractfileext(NiftiOutName));
 if (lExt = '.GZ') or (lExt = '.VOI') then begin  //save gz compressed
    if (lExt = '.GZ') then
       NiftiOutName := ChangeFileExtX(NiftiOutName,'.nii.gz'); //img.gz -> img.nii.gz
    zStream := TGZFileStream.Create(NiftiOutName, gzopenwrite);
    zStream.CopyFrom(mStream, mStream.Size);
    zStream.Free;
 end else begin
     if (lExt <> '.NII') then
        NiftiOutName := NiftiOutName + '.nii';
     mStream.SaveToFile(NiftiOutName); //save uncompressed
 end;
 mStream.Free;
 FileMode := fmOpenRead;
 {$ELSE}
 setlength(rawVolBytes,nvox * (oHdr.bitpix div 8) );
 SaveVolumeFormatBasedOnExt(NiftiOutName, oHdr, rawVolBytes);
 {$ENDIF}
end;

procedure TNIfTI.SaveAsSourceOrient(NiftiOutName: string; rawVolBytesIn: TUInt8s);
//for drawing rotate back to orientation of input image!
var
   oHdr  : TNIFTIhdr;
   oDim, oPerm: TVec3i;
   oScale: TVec3;
   nBytes,nVox: int64;
   i: integer;
   rawVolBytes: TUInt8s;
   {$IFDEF OLDSAVE}
   oPad32: Uint32; //nifti header is 348 bytes padded with 4
   mStream : TMemoryStream;
   zStream: TGZFileStream;
   lExt: string;
   {$ENDIF}
begin
 nBytes := length(rawVolBytesIn);
 rawVolBytes := Copy(rawVolBytesIn, Low(rawVolBytesIn), Length(rawVolBytesIn));
 nVox := prod(fDim);
 oHdr := fHdr;
 if (nBytes = nVox) then
   oHdr.bitpix := 8
 else if (nBytes = (2*nVox)) then
   oHdr.bitpix := 16
 else if (nBytes = (4*nVox)) then
   oHdr.bitpix := 32
 else
     exit;
 if oHdr.bitpix = 8 then
   oHdr.datatype := kDT_UNSIGNED_CHAR
 else if oHdr.bitpix = 16 then
   oHdr.datatype := kDT_INT16
 else
     oHdr.datatype := kDT_FLOAT;
 oDim := fDim;
 oScale := fScale;
 oHdr.scl_slope := 1;
 oHdr.scl_inter := 0;
 oHdr.intent_code := kNIFTI_INTENT_NONE; //https://www.nitrc.org/forum/forum.php?thread_id=13440&forum_id=4442
 oHdr.cal_min:= 0.0;
 oHdr.cal_max:= 0.0;
 oHdr.glmin:= 0;
 oHdr.glmax:= 0;
 oHdr.dim[0] := 3;
 for i := 4 to 7 do
     oHdr.dim[i] := 1;
 //output perm reverses input permute
 //fPermInOrient
 oPerm := pti(1,2,3);
 for i := 0 to 2 do
     if abs(fPermInOrient.v[i]) = 1 then
       oPerm.X := (i+1) * sign(fPermInOrient.v[i]);
 for i := 0 to 2 do
     if abs(fPermInOrient.v[i]) = 2 then
       oPerm.Y := (i+1) * sign(fPermInOrient.v[i]);
 for i := 0 to 2 do
     if abs(fPermInOrient.v[i]) = 3 then
       oPerm.Z := (i+1) * sign(fPermInOrient.v[i]);
 //showmessage(format('%d %d %d -> %d %d %d', [fPermInOrient.X, fPermInOrient.Y, fPermInOrient.Z, oPerm.X, oPerm.y, oPerm.Z]));
 ApplyVolumeReorient(oPerm, fMatInOrient, oDim, oScale, oHdr, rawVolBytes);
 oHdr.vox_offset :=  sizeof(oHdr) + 4;
 {$IFDEF OLDSAVE}
 mStream := TMemoryStream.Create;
 mStream.Write(oHdr,sizeof(oHdr));
 oPad32 := 4;
 mStream.Write(oPad32, 4);
 mStream.Write(rawVolBytes[0],length(rawVolBytes));
  mStream.Position := 0;
 FileMode := fmOpenWrite;
 lExt := uppercase(extractfileext(NiftiOutName));
 if (lExt = '.GZ') or (lExt = '.VOI') then begin  //save gz compressed
    if (lExt = '.GZ') then
       NiftiOutName := ChangeFileExtX(NiftiOutName,'.nii.gz'); //img.gz -> img.nii.gz
    zStream := TGZFileStream.Create(NiftiOutName, gzopenwrite);
    zStream.CopyFrom(mStream, mStream.Size);
    zStream.Free;
 end else begin
     if (lExt <> '.NII') then
        NiftiOutName := NiftiOutName + '.nii';
     mStream.SaveToFile(NiftiOutName); //save uncompressed
 end;
 mStream.Free;
 FileMode := fmOpenRead;
 {$ELSE}
 SaveVolumeFormatBasedOnExt(NiftiOutName, oHdr, rawVolBytes);
 {$ENDIF}
end;

function TNIfTI.SaveCropped(fnm: string; crop: TVec6i; cropVols: TPoint): boolean;
var
  oHdr  : TNIFTIhdr;
  {$IFDEF OLDSAVE}
  mStream : TMemoryStream;
  zStream: TGZFileStream;
  NiftiOutName, lExt: string;
  oPad32: Uint32; //nifti header is 348 bytes padded with 4
  {$ENDIF}
  isV, isZ, isY: boolean;
  offsetMM: TVec3;
  lBPP, ovox, ivox, v,x,y,z: int64;
  orawVolBytes: TUInt8s;
  o16s, i16s: TInt16s;
  o24s, i24s: TRGBs;
  o32s, i32s: TInt32s;
  oMat: TMat4;
begin
 //xLo goes from 0 (no slices cropped) to (dim[1]-1) all but one slice cropped
 //xHi goes from dim[1] (no slices cropped) to 1 (all but first slice)
 if (crop.xLo < 0) or (crop.yLo < 0) or (crop.zLo < 0) then
    exit(false);
 if (crop.xHi > fHdr.dim[1]) or (crop.yHi > fHdr.dim[2]) or (crop.zHi > fHdr.dim[3]) then
    exit(false);
 if (crop.xLo >= crop.xHi) or (crop.yLo >= crop.yHi) or (crop.zLo >= crop.zHi) then
    exit(false);
 if (cropVols.x >= cropVols.y) or (cropVols.y >  fVolumesLoaded) then
    exit(false);
 oHdr := fHdr;
 oHdr.dim[0] := 3;
 oHdr.dim[1] := crop.xHi- crop.xLo;
 oHdr.dim[2] := crop.yHi- crop.yLo;
 oHdr.dim[3] := crop.zHi- crop.zLo;
 oHdr.dim[4] := cropVols.y - cropVols.x;
 oHdr.dim[5] := 1;
 oHdr.dim[6] := 1;
 oHdr.dim[7] := 1;
 if (oHdr.dim[4] > 1) then oHdr.dim[0] := 4;
 offsetMM := NIFTIhdr_SlicesToCoord (oHdr,crop.xLo,crop.yLo,crop.zLo);
 oHdr.srow_x[3] := oHdr.srow_x[3] + offsetMM.x;
 oHdr.srow_y[3] := oHdr.srow_y[3] + offsetMM.y;
 oHdr.srow_z[3] := oHdr.srow_z[3] + offsetMM.z;
 oMat := SForm2Mat(oHdr);
 Mat2QForm(oMat, oHdr);
 lBPP := 4;
 case fHdr.datatype of
   kDT_UNSIGNED_CHAR : lBPP := 1;
   kDT_SIGNED_SHORT: lBPP := 2;
   kDT_RGB: lBPP := 3;
 end;
 ovox := (oHdr.dim[1]*oHdr.dim[2]*oHdr.dim[3]*oHdr.dim[4]);
 setlength(orawVolBytes, ovox * lBPP);
 //FillChar(orawVolBytes[0], ovox * lBPP, 0);
 ivox := 0;
 ovox := 0;
 i16s := TInt16s(fRawVolBytes);
 i24s := TRGBs(fRawVolBytes);
 i32s := TInt32s(fRawVolBytes);
 o16s := TInt16s(oRawVolBytes);
 o24s := TRGBs(oRawVolBytes);
 o32s := TInt32s(orawVolBytes);
 for v := 1 to fVolumesLoaded do begin
     isV := (v > cropVols.x) and (v <= cropVols.y);
     for z := 1 to fHdr.dim[3] do begin
         isZ := (z > crop.zLo) and (z <= crop.zHi);
         for y := 1 to fHdr.dim[2] do begin
             isY := (y > crop.yLo) and (y <= crop.yHi);
             for x := 1 to fHdr.dim[1] do begin
                 if (isV) and (isZ) and (isY) and (x > crop.xLo) and (x <= crop.xHi) then begin
                    if (lBPP = 4) then
                       o32s[ovox] := i32s[ivox]
                    else if (lBPP = 3) then
                       o24s[ovox] := i24s[ivox]
                    else if (lBPP = 2) then
                       o16s[ovox] := i16s[ivox]
                    else
                        orawVolBytes[ovox] := fRawVolBytes[ivox];
                    ovox := ovox + 1;
                 end;
                 ivox := ivox + 1;
             end; //x
         end; //y
     end; //z
 end;  //vol
 //showmessage(format('%d %d  -> %d', [ivox, ovox, oHdr.dim[1]*oHdr.dim[2]*oHdr.dim[3]]));
 oHdr.vox_offset :=  sizeof(oHdr) + 4;
 {$IFDEF OLDSAVE}
 mStream := TMemoryStream.Create;
 mStream.Write(oHdr,sizeof(oHdr));
 oPad32 := 4;
 mStream.Write(oPad32, 4);
 mStream.Write(oRawVolBytes[0], length(orawVolBytes));
 oRawVolBytes := nil;
 mStream.Position := 0;
 FileMode := fmOpenWrite;
 NiftiOutName := fnm;
 lExt := uppercase(extractfileext(NiftiOutName));
 if (lExt = '.GZ') or (lExt = '.VOI') then begin  //
    if (lExt = '.GZ') then
       NiftiOutName := ChangeFileExtX(NiftiOutName,'.nii.gz'); //img.gz -> img.nii.gz
    zStream := TGZFileStream.Create(NiftiOutName, gzopenwrite);
    zStream.CopyFrom(mStream, mStream.Size);
    zStream.Free;
 end else begin
     if (lExt <> '.NII') then
        NiftiOutName := NiftiOutName + '.nii';
     mStream.SaveToFile(NiftiOutName); //save uncompressed
 end;
 mStream.Free;
 FileMode := fmOpenRead;
 exit(true);
 {$ELSE}
 result := SaveVolumeFormatBasedOnExt(fnm, oHdr, oRawVolBytes);
 oRawVolBytes := nil;
 {$ENDIF}
end;

procedure TNIfTI.SaveRotated(fnm: string; perm: TVec3i);
//perm[1,2,3) means unchanged, perm[-1,2,3] flips X
{$DEFINE RESLICE}
var
  oHdr  : TNIFTIhdr;
  oMat: TMat4;
  {$IFDEF OLDSAVE}
  mStream : TMemoryStream;
  zStream: TGZFileStream;
  NiftiOutName, lExt: string;
  oPad32: Uint32; //nifti header is 348 bytes padded with 4
  {$ENDIF}
  {$IFDEF RESLICE}
  orawVolBytes: TUInt8s;
  iMat: TMat4;
  //oPerm: TVec3i;
  oDim: TVec3i;
  oScale : TVec3;
  {$ENDIF}
begin
 //showmessage('Memory exhausted ' + inttostr(length(fRawVolBytes))); exit; xxx
 if length(fRawVolBytes) < 1 then begin
    showmessage(format('Load image to rotate %d %d %d',[fDim.x, fDim.y, fDim.z ] ));
    exit;
 end;
 oHdr := fHdr;
 oMat := EstimateResidual(fMat, perm);
 Mat2SForm(oMat, oHdr);
 Mat2QForm(oMat, oHdr);
 {$IFDEF RESLICE}
 (*try
 //zz setlength(orawVolBytes, length(fRawVolBytes));
 except
   showmessage(format('Memory exhausted (%d bytes) ',[length(fRawVolBytes)]));
   exit;
 end;*)
 try
 orawVolBytes := copy(fRawVolBytes, low(fRawVolBytes), length(fRawVolBytes));
 except
   showmessage(format('Memory exhausted (%d bytes) ',[length(fRawVolBytes)]));
   exit;
 end;
 oDim := fDim;
 oScale := fScale;
 iMat := oMat;
 //showmessage( mStr('i!', fMat)+chr(10)+mStr('o', oMat));
 //showmessage(format('%d %d %d', [Perm.x, perm.y, perm.z]));
 if not EstimateReorient(oDim, iMat, oMat, Perm)  then exit;
 //showmessage(format('%d %d %d', [Perm.x, perm.y, perm.z]));
 //showmessage( mStr('i?', fMat)+chr(10)+mStr('o', oMat));
 //showmessage( mStr('i', fMat)+chr(10)+mStr('o', oMat));
 ApplyVolumeReorient(Perm, oMat, oDim, oScale, oHdr, orawVolBytes);
 //Clipboard.AsText := mStr('i', iMat)+chr(10)+mStr('o', oMat);
 {$ENDIF}
 oHdr.vox_offset :=  sizeof(oHdr) + 4;
 oHdr.dim[0] := 3;
 if fVolumesLoaded > 1 then oHdr.dim[0] := 4;
 oHdr.dim[4] := fVolumesLoaded;
 oHdr.dim[5] := 1;
 oHdr.dim[6] := 1;
 oHdr.dim[7] := 1;
 {$IFDEF OLDSAVE}
 mStream := TMemoryStream.Create;
 mStream.Write(oHdr,sizeof(oHdr));
 oPad32 := 4;
 mStream.Write(oPad32, 4);
 {$IFDEF RESLICE}
 mStream.Write(oRawVolBytes[0], length(fRawVolBytes));
 oRawVolBytes := nil;
 {$ELSE}
 mStream.Write(fRawVolBytes[0], length(fRawVolBytes));
 {$ENDIF}
 mStream.Position := 0;
 FileMode := fmOpenWrite;
 NiftiOutName := fnm;
 lExt := uppercase(extractfileext(NiftiOutName));
 if (lExt = '.GZ') or (lExt = '.VOI') then begin  //save gz compressed
    if (lExt = '.GZ') then
       NiftiOutName := ChangeFileExtX(NiftiOutName,'.nii.gz'); //img.gz -> img.nii.gz
    zStream := TGZFileStream.Create(NiftiOutName, gzopenwrite);
    zStream.CopyFrom(mStream, mStream.Size);
    zStream.Free;
 end else begin
     if (lExt <> '.NII') then
        NiftiOutName := NiftiOutName + '.nii';
     mStream.SaveToFile(NiftiOutName); //save uncompressed
 end;
 mStream.Free;
 FileMode := fmOpenRead;
 {$ELSE}
 {$IFDEF RESLICE}
 SaveVolumeFormatBasedOnExt(fnm, oHdr, oRawVolBytes);
 oRawVolBytes := nil;
 {$ELSE}
 SaveFormatBasedOnExtCore(fnm, oHdr, fRawVolBytes);
 {$ENDIF}
 {$ENDIF}
end;

function TNIfTI.SaveAs8Bit(fnm: string; Lo,Hi: single): boolean;
var
  out8, in8: TUInt8s;
  in16: TInt16s;
  in32: TFloat32s;
  oHdr  : TNIFTIhdr;
  nVox,i: int64;
  scl, mns, mxs, mn, mx, rng: single;
begin
     result := false;
 	 if (fHdr.DataType <> kDT_UINT8) and (fHdr.DataType <> kDT_INT16) and (fHdr.DataType <> kDT_FLOAT) then begin
     	printf('Unsupported format for 8-bit downsampling');
        exit;
     end;
     nVox := length(fRawVolBytes) div (fHdr.bitpix div 8);
     if (Lo > Hi) then begin
     	rng := Lo;
        Lo := Hi;
        Hi := rng;
     end;
     rng := abs(fWindowMax-fWindowMin);
     mns := fWindowMin + (Lo * rng);
     mxs := fWindowMin + (Hi * rng);
     mn := Scaled2RawIntensity(fHdr, mns);
  	 mx := Scaled2RawIntensity(fHdr, mxs);
     rng := abs(mx-mn);
     if rng = 0 then
     	rng := 0.00001;
     printf(format('Clip intensity fraction %g..%g = raw %g..%g\n', [Lo,Hi, mn, mx]));
     scl := 255/rng;
     in8 := fRawVolBytes;
     in16 := TInt16s(in8);
     in32 := TFloat32s(in8);
     setlength(out8, nVox);
     if (fHdr.DataType = kDT_FLOAT) then begin
	 	for i := 0 to (nVox -1) do
        	out8[i] := round(max(min((in32[i]-mn)*scl,255),0)) ;
     end else if (fHdr.DataType = kDT_INT16) then begin
	 	for i := 0 to (nVox -1) do
        	out8[i] := round(max(min((in16[i]-mn)*scl,255),0)) ;
     end else begin
   	 	 for i := 0 to (nVox -1) do
      	 	 out8[i] := round(max(min((in8[i]-mn)*scl,255),0)) ;
     end;
 	 oHdr := fHdr;
     oHdr.datatype := kDT_Uint8;
     oHdr.bitpix := 8;
	 oHdr.scl_inter := mns;
     oHdr.scl_slope := abs(mxs-mns)/255;
     oHdr.cal_min := 0;
     oHdr.cal_max := 0;
     oHdr.glmin := 0;
     oHdr.glmax := 0;
     result := SaveVolumeFormatBasedOnExt(fnm, oHdr, out8);
end;

function TNIfTI.SaveRescaled(fnm: string; xFrac, yFrac, zFrac: single; OutDataType, Filter: integer; isAllVolumes: boolean): boolean;
{$DEFINE 4DRESCALE}
//{$DEFINE OLDSAVE}
var
  oHdr  : TNIFTIhdr;
  {$IFDEF OLDSAVE}
  mStream : TMemoryStream;
  zStream: TGZFileStream;
  NiftiOutName, lExt: string;
  {$ENDIF}
  m: TMat4;
  {$IFDEF OLDSAVE} oPad32: Uint32;{$ENDIF} //nifti header is 348 bytes padded with 4
  orawVolBytes: TUInt8s;
  inVolBytes: int64;
  {$IFDEF 4DRESCALE}
  vrawVolBytes: TUInt8s;
  outVolBytes, v, j, vOffset: int64;
  vHdr, vvHdr  : TNIFTIhdr;
  {$ENDIF}
begin
 oHdr := fHdr;
 //showmessage(format('%d %d', [oHdr.dim[4], fVolumesLoaded])); exit;
 {$IFDEF 4DRESCALE}
 vHdr := fHdr;
 {$ENDIF}
 //zz setlength(orawVolBytes, length(fRawVolBytes));
 inVolBytes := oHdr.dim[1] * oHdr.dim[2] * oHdr.dim[3] * (oHdr.bitpix div 8);
 orawVolBytes := copy(fRawVolBytes, 0, inVolBytes);
 if not ShrinkOrEnlarge(oHdr, orawVolBytes, Filter, xFrac, yFrac, zFrac, OutDataType) then begin
    showmessage('Catastrophic error: perhaps format not supported '+inttostr(OutDataType));
    orawVolBytes := nil;
    exit(false);
 end;
 m := SForm2Mat(oHdr);
 Mat2QForm(m, oHdr);

 oHdr.vox_offset :=  sizeof(oHdr) + 4;
 oHdr.dim[0] := 3;
 oHdr.dim[4] := 1;
 oHdr.dim[5] := 1;
 oHdr.dim[6] := 1;
 oHdr.dim[7] := 1;
 {$IFDEF 4DRESCALE}
 //showmessage(format('%d %d', [length(oRawVolBytes), fVolumesLoaded]));
 if (fVolumesLoaded > 1) and (isAllVolumes) then begin
    outVolBytes := length(oRawVolBytes);
    //showmessage(format('%d %d', [length(oRawVolBytes), fVolumesLoaded]));
    setlength(orawVolBytes,  outVolBytes * fVolumesLoaded);
    oHdr.dim[4] := fVolumesLoaded;
    oHdr.dim[0] := 4;
    for v := 2 to fVolumesLoaded do begin
       vrawVolBytes := copy(fRawVolBytes, (v-1) * inVolBytes, inVolBytes);
       vvHdr := vHdr;
       ShrinkOrEnlarge(vvHdr, vrawVolBytes, Filter, xFrac, yFrac, zFrac, OutDataType);
       vOffset := (v-1) * outVolBytes;
       for j := 0 to (outVolBytes-1) do
           orawVolBytes[j+vOffset] := vrawVolBytes[j];
    end;  //vol
 end; //4D data
 {$ENDIF}
 {$IFDEF OLDSAVE}
 mStream := TMemoryStream.Create;
 mStream.Write(oHdr,sizeof(oHdr));
 oPad32 := 4;
 mStream.Write(oPad32, 4);
 mStream.Write(oRawVolBytes[0], length(oRawVolBytes));
 oRawVolBytes := nil;
 mStream.Position := 0;
 FileMode := fmOpenWrite;
 NiftiOutName := fnm;
 lExt := uppercase(extractfileext(NiftiOutName));
 if (lExt = '.GZ') or (lExt = '.VOI') then begin  //save gz compressed
    if (lExt = '.GZ') then
       NiftiOutName := ChangeFileExtX(NiftiOutName,'.nii.gz'); //img.gz -> img.nii.gz
    zStream := TGZFileStream.Create(NiftiOutName, gzopenwrite);
    zStream.CopyFrom(mStream, mStream.Size);
    zStream.Free;
 end else begin
     if (lExt <> '.NII') then
        NiftiOutName := NiftiOutName + '.nii';
     mStream.SaveToFile(NiftiOutName); //save uncompressed
 end;
 mStream.Free;
 FileMode := fmOpenRead;
 exit(true);
 {$ELSE}
 result := SaveVolumeFormatBasedOnExt(fnm, oHdr, oRawVolBytes);
 {$ENDIF}
end;

constructor TNIfTI.Create(niftiFileName: string; backColor: TRGBA;  tarMat: TMat4; tarDim: TVec3i; isInterpolate: boolean; hdr: TNIFTIhdr; img: TFloat32s; isUINT8: boolean = false); overload;
{$IFNDEF CUSTOMCOLORS}
var
   i: integer;
{$ENDIF}
begin
 LoadFewVolumes := false;
 SortClustersBySize := true;
 ZeroIntensityInvisible := false;
 IsFightInterpolationBleeding := false;
 IsShrunken := false;
 //fDTImode := kDTIno;
 IsInterpolated := false;
 MaxTexMb := 1024;
 isAntiAliasHugeTexMb := false;
 RefreshCount := Random(maxint);
 {$IFDEF CACHEUINT8}  //release cache, force creation on next refresh
 fWindowMinCache8 := infinity;
 fCache8 := nil;
 {$ENDIF}
 Nii_Clear(fHdrNoRotation);
 fVolumeDisplayed := 0;
 fVolumesLoaded := 1;
 fVolumesTotal :=  fVolumesLoaded;
 fIsNativeEndian := true;
 fKnownOrientation := false;
 fIsOverlay := false;
 fIsDrawing := false;
 fCutoutLow := Vec3(0,0,0);
 fCutoutHigh := Vec3(0,0,0);
 fOpacityPct := 100;
 fRawVolBytes := nil;
 fisLinearReslice := false;
 clusterNotes := '';
 {$IFDEF AFNI}
 afnis := nil;
 {$ENDIF}
 clusters := nil;
 fVolRGBA := nil;
 fLabels := TStringList.Create;
 {$IFDEF CUSTOMCOLORS}
 clut := TCLUT.Create();
 clut.BackColor :=  backColor;
 {$ELSE}
 for i := 0 to 255 do
     fLUT[i] := setRGBA(i,i,i,i); //grayscale default
 {$ENDIF}
 // isOK :=
  Load(niftiFileName, tarMat, tarDim, isInterpolate, hdr, img, isUINT8);
end;

constructor TNIfTI.Create(niftiFileName: string;  backColor: TRGBA; tarMat: TMat4; tarDim: TVec3i; isInterpolate: boolean; out isOK: boolean; lLoadFewVolumes: boolean = true; lMaxTexMb: integer = 640); overload;
{$IFNDEF CUSTOMCOLORS}
var
   i: integer;
{$ENDIF}
begin
 IsShrunken := false;
 //fDTImode := kDTIno;
 IsFightInterpolationBleeding := false;
 SortClustersBySize := false;
 LoadFewVolumes := lLoadFewVolumes;
 ZeroIntensityInvisible := false;
 MaxTexMb := lMaxTexMb;
 RefreshCount := Random(MaxInt);
 {$IFDEF CACHEUINT8}  //release cache, force creation on next refresh
 fWindowMinCache8 := infinity;
 fCache8 := nil;
 {$ENDIF}
 Nii_Clear(fHdrNoRotation);
 fVolumeDisplayed := 0;
 fVolumesLoaded := 1;
 fVolumesTotal :=  fVolumesLoaded;
 fIsNativeEndian := true;
 fKnownOrientation := false;
 fIsOverlay := false;
 fIsDrawing := false;
 fCutoutLow := Vec3(0,0,0);
 fCutoutHigh := Vec3(0,0,0);
 fOpacityPct := 100;
 fRawVolBytes := nil;
 fisLinearReslice := false;
 clusterNotes := '';
 {$IFDEF AFNI}
 afnis := nil;
 {$ENDIF}
 clusters := nil;
 fVolRGBA := nil;
 fLabels := TStringList.Create;
 {$IFDEF CUSTOMCOLORS}
 clut := TCLUT.Create();
 clut.BackColor :=  backColor;
 {$ELSE}
 for i := 0 to 255 do
     fLUT[i] := setRGBA(i,i,i,i); //grayscale default
 {$ENDIF}
  isOK := Load(niftiFileName, tarMat, tarDim, isInterpolate);
end;

constructor TNIfTI.Create(niftiFileName: string;  backColor: TRGBA; lLoadFewVolumes: boolean; lMaxTexMb: integer; out isOK: boolean); overload;
var
   tarMat: TMat4;
   tarDim: TVec3i;
begin
  tarMat := TMat4.Identity;
  tarDim := pti(0,0,0); //not an overlay: use source dimensions
  Create(niftiFileName, backColor, tarMat, tarDim, false, isOK, lLoadFewVolumes, lMaxTexMb);
end;

constructor TNIfTI.Create(tarMat: TMat4; tarDim: TVec3i); overload; //blank drawing
var
   isOK: boolean;
   backColor: TRGBA;
begin
 backColor := setRGBA(0,0,0,0);
 Create('-', backColor, tarMat, tarDim, false, isOK);
end;

constructor TNIfTI.Create(); overload;
var
   isOK: boolean;
   backColor: TRGBA;
begin
 backColor := setRGBA(0,0,0,0);
 Create('', backColor, true, 256, isOK);
end;

destructor TNIfTI.Destroy;
begin
  fLabels.Free;
  fRawVolBytes := nil;
  {$IFNDEF DYNRGBA}
  setlengthP(fVolRGBA,0);
  {$ENDIF}
  fVolRGBA := nil;
  fCache8 := nil;
  clusterNotes := '';
  {$IFDEF AFNI}
  afnis := nil;
  {$ENDIF}
  clusters := nil;
  {$IFDEF CUSTOMCOLORS}
  clut.Free;
  {$ENDIF}
end;

end.
