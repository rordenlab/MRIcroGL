unit mainunit;

{$IFDEF LCLCocoa}
 //MetalAPI supported on modern MacOS: disable for Linux, Windows and old MacOS
   //{$DEFINE METALAPI}
   {$modeswitch objectivec1}
   {$DEFINE NewCocoa}
{$ENDIF}
{$IFDEF LCLCarbon}
  error: you must compile for the Cocoa widgetset (ProjectOptions/Additions&Overrides)
{$ENDIF}
{$H+}
{$DEFINE MYPY} //use Python scripts
{$DEFINE COMPILEYOKE} //use yoking
{$DEFINE CLRBAR} //provide color bar
{$WARN 5024 OFF} //disable warnings about unused parameters
{$WARN 5043 off : Symbol "$1" is deprecated}
//{$DEFINE MATT1}
interface

uses
  {$IFDEF MATT1}umat, {$ENDIF}
  {$IFDEF COMPILEYOKE} yokesharemem, {$ENDIF}
  {$IFDEF MYPY}PythonEngine,  {$ENDIF}
  {$IFDEF LCLCocoa} {$IFDEF NewCocoa}nsappkitext, UserNotification,{$ENDIF} {$ENDIF}
  {$IFDEF UNIX}Process,{$ELSE} Windows,{$ENDIF}
  lcltype, GraphType, Graphics, dcm_load,
  LCLIntf, slices2D, StdCtrls, SimdUtils, Classes, SysUtils, Forms, Controls,clipbrd,
  Dialogs, Menus, ExtCtrls, CheckLst, ComCtrls, Spin, Types, fileutil, ulandmarks,
  nifti_hdr_view, fsl_calls, math, nifti, niftis, prefs, dcm2nii, strutils, drawVolume, autoroi, VectorMath;

const
  kVers = '1.2.20181114+'; //+fixes Metal memory leak
type

  { TGLForm1 }

  TGLForm1 = class(TForm)
    AnatDrop: TComboBox;
    LayerOptionsBtn: TButton;
    ImportMenu: TMenuItem;
    ImportDicomMenu: TMenuItem;
    HelpAboutMenu: TMenuItem;
    ClrbarMenu: TMenuItem;
    ClrbarSep: TMenuItem;
    BlackClrbarMenu: TMenuItem;
    ExtractBrainMenu: TMenuItem;
    LayerUpMenu: TMenuItem;
    LayerDownMenu: TMenuItem;
    LayerSmoothMenu: TMenuItem;
    LayerAdditiveMenu: TMenuItem;
    LayerResetBrightnessMenu: TMenuItem;
    DrawMenu: TMenuItem;
    DrawOpenMenu: TMenuItem;
    DrawSaveMenu: TMenuItem;
    DrawTransparencyMenu: TMenuItem;
    DrawHideMenu: TMenuItem;
    DrawTrans25Menu: TMenuItem;
    DrawTrans50Menu: TMenuItem;
    DrawTrans90Menu: TMenuItem;
    DrawColorMenu: TMenuItem;
    DrawNoneMenu: TMenuItem;
    DrawEraseMenu: TMenuItem;
    DrawRedMenu: TMenuItem;
    DrawAdvancedMenu: TMenuItem;
    DrawGreenMenu: TMenuItem;
    DrawBlueMenu: TMenuItem;
    DrawOverwriteMenu: TMenuItem;
    DrawUndoMenu: TMenuItem;
    DrawCloseMenu: TMenuItem;
    DrawBinarizeMenu: TMenuItem;
    DrawSmoothMenu: TMenuItem;
    DrawCloneMenu: TMenuItem;
    DrawAutomaticMenu: TMenuItem;
    DrawInterpolateMenu: TMenuItem;
    DrawInterpolate2Menu: TMenuItem;
    DrawInterpolateAxMenu: TMenuItem;
    DrawInterpolateCorMenu: TMenuItem;
    DrawInterpolateSagMenu: TMenuItem;
    DrawDescriptivesMenu: TMenuItem;
    LayerFromZeroMenu: TMenuItem;
    ApplePrefMenu: TMenuItem;
    EditMenu: TMenuItem;
    EditCopyMenu: TMenuItem;
    FileSepMenu: TMenuItem;
    FileExitMenu: TMenuItem;
    HelpPrefMenu: TMenuItem;
    LayerCloseMenu: TMenuItem;
    DisplaySep3: TMenuItem;
    DisplayNextMenu: TMenuItem;
    DisplayPrevMenu: TMenuItem;
    LayerShowHeaderMenu: TMenuItem;
    LandmarkMenu: TMenuItem;
    LandmarkOpenMenu: TMenuItem;
    LandmarkSaveMenu: TMenuItem;
    LandmarkAddMenu: TMenuItem;
    LandmarkUpdateMenu: TMenuItem;
    LandmarkDeleteMenu: TMenuItem;
    LandmarkSelectNext: TMenuItem;
    LayerNextVolumeMenu: TMenuItem;
    LayerPrevVolumeMenu: TMenuItem;
    LayerCutoutMenu: TMenuItem;
    MenuItem1: TMenuItem;
    DisplayAnimateMenu: TMenuItem;
    LayerShowBidsMenu: TMenuItem;
    ExitFullScreenMenu: TMenuItem;
    LayerMaskWithBackgroundMenu: TMenuItem;
    StoreFMRIMenu: TMenuItem;
    SmoothMenu: TMenuItem;
    TextAndCubeMenu: TMenuItem;
    OpenFSLMenu: TMenuItem;
    OpenStandardMenu: TMenuItem;
    NewWindowMenu: TMenuItem;
    AnimateTimer: TTimer;
    YokeTimer: TTimer;
    YokeMenu: TMenuItem;
    DisplaySep2: TMenuItem;
    ScriptingPyVersion: TMenuItem;
    TransBlackClrbarMenu: TMenuItem;
    TransWhiteClrbarMenu: TMenuItem;
    WhiteClrbarMenu: TMenuItem;
    VisibleClrbarMenu: TMenuItem;
    ScriptingNewMenu: TMenuItem;
    RemoveHazeMenu: TMenuItem;
    OpenRecentMenu: TMenuItem;
    MosLabelCheck: TCheckBox;
    MosaicText: TMemo;
    MosCrossCheck: TCheckBox;
    MosOrientDrop: TComboBox;
    CutNearBtn: TButton;
    CutNoneBtn: TButton;
    CutoutBox: TGroupBox;
    MosOrientLabel: TLabel;
    MosColLabel: TLabel;
    MosRowOverlapTrack: TTrackBar;
    MosRowLabel: TLabel;
    MosaicBox: TGroupBox;
    MosColEdit: TSpinEdit;
    MosRowEdit: TSpinEdit;
    MosColOverlapTrack: TTrackBar;
    Y2TrackBar: TTrackBar;
    Z2TrackBar: TTrackBar;
    YLabel: TLabel;
    ZLabel: TLabel;
    XTrackBar: TTrackBar;
    XLabel: TLabel;
    XCoordEdit: TEdit;
    CoordLabel: TLabel;
    SliceABtn: TButton;
    SliceLBtn: TButton;
    SliceRBtn: TButton;
    SliceSBtn: TButton;
    SlicePBtn: TButton;
    SliceBox: TGroupBox;
    LineColorBtn: TButton;
    ClipAziTrack: TTrackBar;
    ClipElevTrack: TTrackBar;
    AddOverlayMenu: TMenuItem;
    DisplaySep: TMenuItem;
    DisplayLeftMenu: TMenuItem;
    DisplayRightMenu: TMenuItem;
    DisplayPosteriorMenu: TMenuItem;
    DisplayAnteriorMenu: TMenuItem;
    DisplayInferiorMenu: TMenuItem;
    DisplaySuperiorMenu: TMenuItem;
    LineWidthLabel: TLabel;
    LineBox: TGroupBox;
    HelpMenu: TMenuItem;
    OnlineHelpMenu: TMenuItem;
    ScriptSaveDialog: TSaveDialog;
    ScriptOpenDialog: TOpenDialog;
    ScriptingMenu: TMenuItem;
    ScriptingSepMenu: TMenuItem;
    ScriptingSaveMenu: TMenuItem;
    ScriptingRunMenu: TMenuItem;
    ScriptingTemplatesMenu: TMenuItem;
    ScriptingOpenMenu: TMenuItem;
    SharpenMenu: TMenuItem;
    S2Label: TLabel;
    S3Label: TLabel;
    S4Label: TLabel;
    S5Label: TLabel;
    S1Track: TTrackBar;
    LightAziTrack: TTrackBar;
    QLabel: TLabel;
    LightLabel: TLabel;
    LightElevTrack: TTrackBar;
    S1Label: TLabel;
    S2Track: TTrackBar;
    S3Track: TTrackBar;
    S4Track: TTrackBar;
    S6Label: TLabel;
    S7Label: TLabel;
    S8Label: TLabel;
    S9Label: TLabel;
    S10Label: TLabel;
    S5Track: TTrackBar;
    S6Track: TTrackBar;
    S7Track: TTrackBar;
    S8Track: TTrackBar;
    S9Track: TTrackBar;
    S10Track: TTrackBar;
    ShaderDrop: TComboBox;
    LayerAlphaLabel: TLabel;
    ClipDepthLabel: TLabel;
    ClipDepthTrack: TTrackBar;
    ClipAziLabel: TLabel;
    ClipElevLabel: TLabel;
    LayerColorDrop: TComboBox;
    LayerDarkEdit: TEdit;
    LayerBrightEdit: TEdit;
    LayerDarkLabel: TLabel;
    LayerBrightLabel: TLabel;
    LayerList: TCheckListBox;
    ColorDialog1: TColorDialog;
    ClipBox: TGroupBox;
    CenterPanel: TPanel;
    LayerPopup: TPopupMenu;
    ScriptMemo: TMemo;
    ScriptOutputMemo: TMemo;
    ScriptBox: TGroupBox;
    ScriptPanel: TPanel;
    ShaderBox: TGroupBox;
    LayerBox: TGroupBox;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ResetDefaultsMenu: TMenuItem;
    DisplayMenu: TMenuItem;
    CoronalMenu: TMenuItem;
    ColorMenu: TMenuItem;
    BackColorMenu: TMenuItem;
    ColorEditorMenu: TMenuItem;
    LineWidthEdit: TSpinEdit;
    SliceIBtn: TButton;
    YCoordEdit: TEdit;
    ZCoordEdit: TEdit;
    ScriptSplitter: TSplitter;
    ToolPanel: TScrollBox;
    SagittalLMenu: TMenuItem;
    MosaicMenu: TMenuItem;
    SagittalMenu: TMenuItem;
    MPRMenu: TMenuItem;
    RenderMenu: TMenuItem;
    AxialMenu: TMenuItem;
    SaveDialog1: TSaveDialog;
    SaveMenu: TMenuItem;
    AppleMenu: TMenuItem;
    AboutMenu: TMenuItem;
    OpenDialog1: TOpenDialog;
    LeftSplitter: TSplitter;
    RightSplitter: TSplitter;
    LayerAlphaTrack: TTrackBar;
    QualityTrack: TTrackBar;
    UpdateTimer: TTimer;
    ViewMenu: TMenuItem;
    OpenMenu: TMenuItem;
    X2TrackBar: TTrackBar;
    YTrackBar: TTrackBar;
    ZTrackBar: TTrackBar;
    procedure AnatAddBtnClick(Sender: TObject);
    procedure AnatDeleteBtnClick(Sender: TObject);
    procedure AnatDropChange(Sender: TObject);
    procedure AnatSaveBtnClick(Sender: TObject);
    procedure AnatUpdate();
    procedure AnatOpenBtnClick(Sender: TObject);
    procedure AnatUpdateBtnClick(Sender: TObject);
    procedure AnimateTimerTimer(Sender: TObject);
    procedure CreateStandardMenus;
    procedure DisplayAnimateMenuClick(Sender: TObject);
    function DisplayNextMenuClick(Sender: TObject): boolean;
    procedure EditCopyMenuClick(Sender: TObject);
    procedure ExitFullScreenMenuClick(Sender: TObject);
    procedure FileExitMenuClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure LandmarkSelectNextClick(Sender: TObject);
    procedure LayerCloseMenuClick(Sender: TObject);
    procedure LayerCutoutMenuClick(Sender: TObject);
    procedure LayerListClickCheck(Sender: TObject);
    procedure LayerListShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure LayerMaskWithBackgroundMenuClick(Sender: TObject);
    procedure LayerOptionsBtnClick(Sender: TObject);
    procedure LayerShowBidsMenuClick(Sender: TObject);
    procedure LayerShowHeaderMenuClick(Sender: TObject);
    procedure LayerVolumeChange(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure OpenFSLMenuClick(Sender: TObject);
    procedure Quit2TextEditor;
    procedure DrawAutomaticMenuClick(Sender: TObject);
    procedure DrawBinarizeMenuClick(Sender: TObject);
    procedure DrawCloneMenuClick(Sender: TObject);
    procedure DrawCloseMenuClick(Sender: TObject);
    procedure DrawDescriptivesMenuClick(Sender: TObject);
    procedure DrawSmoothMenuClick(Sender: TObject);
    procedure DrawUndoMenuClick(Sender: TObject);
    procedure EnsureOpenVoi();
    procedure ClrbarClr(i: integer);
    procedure InterpolateDrawMenuClick(Sender: TObject);
    procedure LayerFromZeroMenuClick(Sender: TObject);
    procedure HelpPrefMenuClick(Sender: TObject);
    procedure SaveMosaicBmp(bmpName: string);
    procedure DrawSaveMenuClick(Sender: TObject);
    procedure Save2Bmp(fnm: string);
    procedure DrawOpenMenuClick(Sender: TObject);
    procedure DrawTool1Click(Sender: TObject);
    procedure DrawTransClick(Sender: TObject);
    procedure ExtractBrainMenuClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LayerAdditiveMenuClick(Sender: TObject);
    procedure LayerPopupPopup(Sender: TObject);
    procedure LayerMoveUpOrDown(layer: integer; isMoveDown:boolean);
    procedure LayerResetBrightnessMenuClick(Sender: TObject);
    procedure LayerUpDownClick(Sender: TObject);
    procedure NewWindowMenuClick(Sender: TObject);
    procedure ScriptingPyVersionClick(Sender: TObject);
    procedure ScriptMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure ScriptPanelDblClick(Sender: TObject);
    procedure SetColorBarPosition;
    procedure SmoothMenuClick(Sender: TObject);
    procedure StoreFMRIMenuClick(Sender: TObject);
    procedure TextAndCubeMenuClick(Sender: TObject);
    procedure ToolPanelDblClick(Sender: TObject);
    procedure UpdateColorBar;
    procedure SaveColorTable;
    procedure ClrbarVisibleClick(Sender: TObject);
    procedure CoordEditChange(Sender: TObject);
    procedure CutNearBtnClick(Sender: TObject);
    procedure CutNoneBtnClick(Sender: TObject);
    procedure SetFormDarkMode(var f: TForm);
    procedure DisplayViewMenu(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure HelpMenuClick(Sender: TObject);
    procedure ImportDicomMenuClick(Sender: TObject);
    procedure LayerAlphaTrackMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LayerChange(layer, colorTag, opacityPercent: integer; displayMin, displayMax: single);
    procedure LayerListSelectionChange(Sender: TObject; User: boolean);
    procedure LayerWidgetChange(Sender: TObject);
    procedure LineColorBtnClick(Sender: TObject);
    //procedure LineColorOpacityTrackChange(Sender: TObject);
    procedure LineWidthEditChange(Sender: TObject);
    procedure MosaicTextChange(Sender: TObject);
    procedure OrientBtnClick(Sender: TObject);
    procedure RemoveHazeMenuClick(Sender: TObject);
    procedure ScriptingNewMenuClick(Sender: TObject);
    procedure ClrbarMenuClick(Sender: TObject);
    procedure UpdateMosaic(Sender: TObject);
    procedure OnlineHelpMenuClick(Sender: TObject);
    procedure OpenScript(scriptname: string; isShowScriptPanel: boolean = true);
    procedure AddOverlayMenuClick(Sender: TObject);
    procedure ScriptingOpenMenuClick(Sender: TObject);
    procedure ScriptingRunMenuClick(Sender: TObject);
    procedure ScriptingSaveMenuClick(Sender: TObject);
    procedure ScriptingTemplatesMenuClick(Sender: TObject);
    procedure ScriptFormVisible(vis: boolean);
    procedure UpdateLayerBox(NewLayers: boolean);
    function AddLayer(Filename: string): boolean;
    procedure UpdateContrast (Xa,Ya, Xb, Yb: integer);
    function AddBackground(Filename: string; isAddToRecent: boolean = true): boolean;
    procedure AboutMenuClick(Sender: TObject);
    procedure BackColorMenuClick(Sender: TObject);
    procedure ClipLabelClick(Sender: TObject);
    procedure ColorEditorMenuClick(Sender: TObject);
    procedure LayerContrastKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UpdateVisibleBoxes(setMenuChecked: boolean = false);
    procedure SetDisplayCheck();
    procedure UpdateOpenRecent();
    procedure OptimalMosaicPixels(out w,h: integer);
    procedure AddOpenRecent(Filename: string);
    procedure ResetDefaultsClick(Sender: TObject);
    procedure DisplayMenuClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    //procedure FormResize(Sender: TObject);
    procedure PrefTrackChange(Sender: TObject);
    procedure ShaderDropChange(Sender: TObject);
    procedure SharpenMenuClick(Sender: TObject);
    //procedure SplitterMoved(Sender: TObject);
    procedure SaveMenuClick(Sender: TObject);
    procedure SetXHairPosition (lX,lY,lZ: single);
    procedure UpdateShaderSettings(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure ViewGPUMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ViewGPUMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewGPUMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OpenMenuClick(Sender: TObject);
    //procedure ShaderMenuClick(Sender: TObject);
    //procedure LayerColorDropChange(Sender: TObject);
    procedure ViewGPUMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ParamStr2Script();
    procedure ViewGPUPrepare(Sender: TObject);
    procedure ViewGPUPaint(Sender: TObject);
    procedure ViewGPUDblClick(Sender: TObject);
    procedure setShaderSliders;
    procedure SetDarkMode();
    procedure CutoutChange(Sender: TObject);
    procedure OpenRecentMenuClick(Sender: TObject);
    procedure OpenStandardMenuClick(Sender: TObject);
    procedure CutoutMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LayerColorFromZero(Layer: integer; IsFromZero: boolean);
    function SliceFrac : TVec3;
    procedure SetRetina;
    {$IFDEF MYPY}
    function PyCreate: boolean;
    function PyExec(): boolean;
    procedure PyEngineAfterInit(Sender: TObject);
    procedure PyIOSendData(Sender: TObject; const Data: AnsiString);
    procedure PyIOSendUniData(Sender: TObject; const Data: UnicodeString);
    procedure PyModInitialization(Sender: TObject);
    {$ENDIF}
    procedure YokeMenuClick(Sender: TObject);
    procedure YokeTimerTimer(Sender: TObject);
    procedure voiUndo(isRefresh: boolean = false);
    procedure MorphologyFill(Origin: TVec3; dxOrigin, radiusMM: int64; drawMode: int64);
    procedure ForceOverlayUpdate();
  private
    //
  end;

var
  GLForm1: TGLForm1;

implementation

{$R *.lfm}
{$IFDEF METALAPI}
uses
  {$IFDEF CLRBAR}mtlclrbar, {$ENDIF} MetalPipeline,  Metal,MetalControl, mtlvolume2;
const  kExt = '.metal';
{$ELSE}
uses
  {$IFDEF LCLCocoa}glcocoanscontext,{$ENDIF}{$IFDEF CLRBAR}glclrbar, {$ENDIF} retinahelper,  OpenGLContext,  glvolume2, glcorearb, gl_core_utils {$IFNDEF UNIX}, proc_py {$ENDIF};
const kExt = '.glsl';
{$ENDIF}
const
     kNaNsingle: single = 1/0;
     kNaN : double = 1/0;
var
 gPrefs: TPrefs;
 gMouse : TPoint = (x: -1; y: -1);
 gMouseDrag: boolean = false;
 gSliceMM : TVec3 = (x: 0; y: 0; z: 0);
 Vol1: TGPUVolume;
 isBusy: boolean = false;
 vols: TNIfTIs;
 gLandmark : TLandmark;
 {$IFDEF METALAPI}
 isPrepared: boolean = false;
 ViewGPU1: TMetalControl;
 {$ELSE}
 ViewGPU1: TOpenGLControl;
 {$ENDIF}
 {$IFDEF CLRBAR}gClrbar: TGPUClrbar;{$ENDIF}

procedure TGLForm1.ForceOverlayUpdate();
var
    i: integer;
    niftiVol: TNIfTI;
begin
  i := LayerList.Count-1;
  caption := inttostr(i);
  if (i < 0) then exit;
  if not vols.Layer(i,niftiVol) then exit;
  niftiVol.CX.NeedsUpdate := true;
end;

procedure TGLForm1.voiUndo(isRefresh: boolean = false);
begin
 if not vols.Drawing.IsOpen then exit;
 Vols.Drawing.voiUndo();
 if isRefresh then
    ViewGPU1.Invalidate;
end;

procedure dwriteln(str: string);
begin
     {$IFDEF UNIX} writeln(str); {$ENDIF}
end;

procedure TGLForm1.MorphologyFill(Origin: TVec3; dxOrigin, radiusMM: int64; drawMode: int64);
var
 niftiVol: TNIfTI;
 mm: TVec3;
 clr: integer;
begin
  //if not Vols.Drawing.IsOpen then exit;
  EnsureOpenVoi();
  clr := Vols.Drawing.ActivePenColor;
  if  (clr < 0) then clr := 1;
  if (gPrefs.DisplayOrient = kRenderOrient) or (gPrefs.DisplayOrient = kMosaicOrient) then exit;
  if not vols.Layer(0,niftiVol) then exit;
  EnsureOpenVoi();
  mm := Vec3(niftiVol.Header.pixdim[1], niftiVol.Header.pixdim[2], niftiVol.Header.pixdim[3]);


  Vols.Drawing.voiMorphologyFill(niftiVol.DisplayMinMax2Uint8, clr, mm.X, mm.Y, mm.Z, Origin.X, Origin.Y, Origin.Z, dxOrigin, radiusMM, drawMode);
  ViewGPU1.Invalidate;
end;

procedure TGLForm1.SetRetina;
begin
 {$IFDEF LCLCocoa} {$IFNDEF METALAPI}
  LSetWantsBestResolutionOpenGLSurface(gPrefs.RetinaDisplay, ViewGPU1.Handle);
  {$ENDIF} {$ENDIF}
end;

function TGLForm1.SliceFrac : TVec3;
begin
     result.x := vol1.Slices.SliceFrac.X;
     result.y := vol1.Slices.SliceFrac.Y;
     result.z := vol1.Slices.SliceFrac.Z
end;

procedure TGLForm1.UpdateColorbar();
var
 i, n: integer;
 niftiVol: TNIfTI;
begin
  {$IFDEF CLRBAR}
  n := vols.NumLayers;
  if n < 1 then exit;
  if n < 2 then begin //with single background image show color range for background
    if not vols.Layer(0,niftiVol) then exit;
    gClrbar.SetLUT(1, niftiVol.ColorTable, niftiVol.DisplayMin,niftiVol.DisplayMax, niftiVol.CX.FromZero);
    gClrbar.Number := 1;
  end else begin //when overlays are loaded, show color range of overlays
      for i := 1 to (n-1) do begin
          if not vols.Layer(i,niftiVol) then exit;
          gClrbar.SetLUT(i, niftiVol.ColorTable, niftiVol.DisplayMin,niftiVol.DisplayMax, niftiVol.CX.FromZero);
      end;
      gClrbar.Number := n - 1;
  end;
  {$ENDIF}
end;

function AddNiiExt(Filename: string): string;
//does adding ".nii" or ".nii.gz" identify an eexisting file?
begin
     result := Filename;
     if Fileexists(result) then exit;
     result := Filename+'.nii';
     if Fileexists(result) then exit;
     result := Filename+'.nii.gz';
     if Fileexists(result) then exit;
     result := Filename;
end;

function CheckParentFolders(Path, Filename: string): string;
//does adding ".nii" or ".nii.gz" identify an eexisting file?
var
   opth, pth: string;
begin
     pth := Path;
     while (length(pth) > 0) and (DirectoryExists(pth)) do begin
           result := AddNiiExt(pth+Filename);
           if Fileexists(result) then exit;
           //showmessage(pth);
           opth := pth;
           pth := ExtractFilePath(ExtractFileDir(pth));
           if opth = pth then break; //FilePath of "/" is "/"
     end;
     result := Filename; //fail
end;

function StandardDir (): string;
begin
	result := ResourceDir + pathdelim + 'standard';
end;

function GetFullPath(Filename: string): string;
// "motor" -> /home/smith/myDir/motor.nii.gz
var
   i: integer;
begin
     result := Filename;
     if (length(Filename) < 1) or (Fileexists(result)) then exit;
     result := AddNiiExt(Filename);
     if Fileexists(result) then exit;
     {$IFDEF UNIX}
     if Filename[1] = '~' then begin
        result := AddNiiExt(ExpandFileName(Filename));
        if Fileexists(result) then exit;
     end;
     {$ENDIF}
     //result := Filename;
     //Filename := ExtractFileName(result);
     result := AddNiiExt(GetCurrentDir + pathdelim+ Filename);
     if Fileexists(result) then exit;
     if upcase(ExtractFileExt(Filename))= '.NII' then
        Filename := ChangeFileExt(Filename,''); //img.nii -> img (allows us to find .nii.gz
     if (length(ExtractFilePath(Filename)) > 1) then begin //path provided
        result := AddNiiExt(Filename);
        exit;
     end;
     result := CheckParentFolders(StandardDir+pathdelim, Filename); //ResourceDir is parent of standardDir, so we check both
     //result := CheckParentFolders(ResourceDir+pathdelim, Filename);
     if (Fileexists(result)) then exit;
     for i := 1 to knMRU do begin
         result := CheckParentFolders(ExtractFilePath(gPrefs.PrevFilename[i]), Filename);
         if (Fileexists(result)) then exit;
     end;
     if (Filename[1] = '-') then begin //fsleyes defaults
        if (Filename = '-std1mm') or (Filename = '--standard1mm') then
           result := GetFSLdir+pathdelim+ 'data'+pathdelim+'standard'+pathdelim+'MNI152_T1_1mm';
        if (Filename = '-std') or (Filename = '--standard') then
           result := GetFSLdir+pathdelim+ 'data'+pathdelim+'standard'+pathdelim+'MNI152_T1_2mm';
        result := AddNiiExt(result);
        if (Fileexists(result)) then exit;
     end;
     result := Filename; //failed to find a match!
     {$IFDEF UNIX}
     writeln('Unable to find image: "', result,'"');
     {$ENDIF}
end;

 {$IFDEF MYPY}
 var
  PythonIO : TPythonInputOutput;
  PyMod: TPythonModule;
  PyEngine: TPythonEngine = nil;
  gPyRunning: boolean = false;
  {$IFDEF Darwin}
  const
       kBasePath = '/Library/Frameworks/Python.framework/Versions/';

  {$ENDIF}

{$IFDEF UNIX}
function InitPyLibraryPath: string;
  function GetMacPath(NMinorVersion: integer): string;
  begin
    Result:= Format('/Library/Frameworks/Python.framework/Versions/3.%d/lib/libpython3.%d.dylib',
      [NMinorVersion, NMinorVersion]);
  end;
var
  N: integer;
begin
  Result:= '';
  {$ifdef windows}
  exit('python35.dll');
  {$endif}

  {$ifdef linux}
  exit('libpython3.6m.so.1.0');
  {$endif}

  {$ifdef freebsd}
  exit('libpython3.6m.so');
  {$endif}

  {$ifdef darwin}
  for N:= 4 to 9 do
  begin
    Result:= GetMacPath(N);
    if FileExists(Result) then exit;
  end;
  {$endif}
end;

function searchPy(pth: string): string;
var
   searchResult : TSearchRec;
begin
    result := '';
    {$IFDEF Darwin}
    if FindFirst(IncludeTrailingPathDelimiter(pth)+'libpython*.dylib', faDirectory, searchResult) = 0 then
    {$ELSE}
    if FindFirst(IncludeTrailingPathDelimiter(pth)+'libpython*.so', faDirectory, searchResult) = 0 then
    {$ENDIF}
    result := IncludeTrailingPathDelimiter(pth)+(searchResult.Name);
    FindClose(searchResult);
end;
{$ENDIF}
function findPythonLib(def: string): string;
{$IFDEF WINDOWS}
var
  fnm: string;
begin
     result := def;
     if fileexists(def) then exit;
     result :=''; //assume failure
     fnm := ScriptDir + pathdelim + 'python35.dll';
     if not FileExists(fnm) then exit;
     if not FileExists(changefileext(fnm,'.zip')) then exit;
     result := fnm;
end;
{$ELSE}
{$IFDEF Linux}
  const
       knPaths = 8;
       // /usr/lib/i386-linux-gnu/
       {$IFDEF CPU64}
       kBasePaths : array [1..knPaths] of string = ('/lib/','/lib64/','/usr/lib64/','/usr/lib/x86_64-linux-gnu/','/usr/lib/','/usr/local/lib/','/usr/lib/python2.7/config-x86_64-linux-gnu/','/opt/gitlab/embedded/lib/');
       {$ELSE}
       kBasePaths : array [1..knPaths] of string = ('/lib/','/lib32/','/usr/lib32/','/usr/lib/i386-linux-gnu/','/usr/lib/','/usr/local/lib/','/usr/lib/python2.7/config-i386-linux-gnu/','/opt/gitlab/embedded/lib/');
       {$ENDIF}
       kBaseName = 'libpython';
{$ENDIF}
{$IFDEF Darwin}
    const
       knPaths = 3;
       kBasePaths : array [1..knPaths] of string = (kBasePath, '/System'+kBasePath, '/System/Library/Frameworks/Python.framework/Versions/Current/lib/');

{$ENDIF}
    var
         searchResult : TSearchRec;
         pth, fnm: string;
         vers : TStringList;
         n: integer;
      begin
        result := def;
           if DirectoryExists(def) then begin //in case the user supplies libdir not the library name
             result := searchPy(def);
             if length(result) > 0 then exit;
           end;
           {$IFDEF LCLCocoa}
           result := searchPy('/System/Library/Frameworks/Python.framework/Versions/Current/lib');
           if fileexists(result) then exit;
           {$ENDIF}
           //if fileexists(def) then exit;
           result := InitPyLibraryPath;
           if fileexists(result) then exit;
           {$IFDEF Darwin}  //check in application resource folder
           result := resourceDir + pathdelim+'libpython3.6.dylib';
           if fileexists(result) then exit;
           {$ENDIF}
           vers := TStringList.Create;
           n := 1;
           while (n <= knPaths) and (vers.Count < 1) do begin
             pth := kBasePaths[n];
             n := n + 1;
             if not DirectoryExists(pth) then continue;
             {$IFDEF Linux}
             if FindFirst(pth+'*.so', faDirectory, searchResult) = 0 then begin
             {$ELSE}
             if FindFirst(pth+'*', faDirectory, searchResult) = 0 then begin
             {$ENDIF}
               repeat
                      //showmessage('?'+searchResult.Name);
                      if (length(searchResult.Name) < 1) or (searchResult.Name[1] = '.') then continue;
                      {$IFDEF LINUX}
                      if (pos(kBaseName,searchResult.Name) < 1) then continue;
                      {$ELSE}
                      if (not (searchResult.Name[1] in ['0'..'9'])) then continue;
                      {$ENDIF}
                  if (pos('libpython2.6',searchResult.Name) < 1) then
                     vers.Add(searchResult.Name);
                until findnext(searchResult) <> 0;
             end;
            FindClose(searchResult);
          end;
          if vers.Count < 1 then begin
             vers.Free;
             result :=''; //assume failure
             for n := 1 to knPaths do begin
               pth := kBasePaths[n];
               result := searchPy(pth);
               if fileexists(result) then exit;
             end;
             result := '';
             exit;
          end;
          vers.Sort;
          fnm := vers.Strings[vers.Count-1]; //newest version? what if 3.10 vs 3.9?
          vers.Free;
          {$IFDEF Darwin}
          fnm := kBasePath+fnm+'/lib/libpython'+fnm+'.dylib';
          {$ENDIF}
          {$IFDEF LINUX}
          fnm := pth+ fnm;
          {$ENDIF}
          if fileexists(fnm) then
             result := fnm;
      end;
{$ENDIF}
function TGLForm1.PyCreate: boolean;
var
  S: string;
begin
  result := false;
  //showmessage(gPrefs.PyLib);
  //gPrefs.PyLib := '';
  if not fileexists(gPrefs.PyLib) then begin
     S:= findPythonLib('');
     if (S = '') then begin
        Showmessage('Unable to find PyLib (install Python or edit preferences).');
        exit;
     end;
    {$IFDEF UNIX} writeln('PyLib: '+S); {$ENDIF}
  end else begin
    S := gPrefs.PyLib;
    {$IFDEF UNIX}writeln('Using preference file PyLib "'+S+'"'); {$ENDIF}
  end;
  if not fileexists(S) then begin
     {$IFDEF UNIX}writeln('Unable to find %s');{$ENDIF}
     exit;
  end;
  if (pos('libpython2.6',S) > 0) then begin
     showmessage('Old, unsupported version of Python '+S);
     exit;
  end;
  gPrefs.PyLib := S;
  result := true;
  PythonIO := TPythonInputOutput.Create(GLForm1);
  PyMod := TPythonModule.Create(GLForm1);
  PyEngine := TPythonEngine.Create(GLForm1);
  PyEngine.IO := PythonIO;
  PyEngine.PyFlags:=[pfIgnoreEnvironmentFlag];
  PyEngine.UseLastKnownVersion:=false;
  PyMod.Engine := PyEngine;
  PyMod.ModuleName := 'gl';
  PyMod.OnInitialization:= @PyModInitialization;
  PythonIO.OnSendData := @PyIOSendData;
  PythonIO.OnSendUniData:= @PyIOSendUniData;
  PyEngine.DllPath:= ExtractFileDir(S);
  PyEngine.DllName:= ExtractFileName(S);
  PyEngine.LoadDll
end;
procedure TGLForm1.PyIOSendData(Sender: TObject;
  const Data: AnsiString);
begin
  ScriptOutputMemo.Lines.Add(Data);
end;

procedure TGLForm1.PyIOSendUniData(Sender: TObject;
  const Data: UnicodeString);
begin
  ScriptOutputMemo.Lines.Add(ANSIString(Data));
end;

{$IFDEF PYOBSOLETE}
function PyAZIMUTH(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'i:azimuth', @A)) then
      Vol1.Azimuth := Vol1.Azimuth+A;
  ViewGPU1.Invalidate;
end;


function PyCOLORNAME(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  i: integer;
  ret: boolean;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 's:colorname', @PtrName)) then
    begin
      StrName:= string(PtrName);
      i := GLForm1.LayerColorDrop.Items.IndexOf(StrName); //search is case-insensitive!

      ret := (i >= 0);
      if ret then begin
         //GLForm1.LayerColorDrop.ItemIndex := i;
         GLForm1.LayerChange(0, i, -1, kNaNsingle, kNaNsingle); //kNaNsingle
         //todo background layer
         //GLForm1.LayerColorDropChange(nil);
         //GLForm1.LayerWidgetChange(nil);
      end;
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PyELEVATION(Self, Args : PPyObject): PPyObject; cdecl;
var
  E: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'i:elevation', @E)) then
      Vol1.Elevation := Vol1.Elevation - E;
  ViewGPU1.Invalidate;
end;

function PyCLIP(Self, Args : PPyObject): PPyObject; cdecl;
var
  D: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'f:clip', @D)) then
      SetTrackFrac(GLForm1.ClipDepthTrack, D);
end;

function PyEXISTS(Self, Args : PPyObject): PPyObject; cdecl;
//exists('motor') returns true if it finds "~/mydir/motor.nii.gz"
var
  PtrName: PChar;
  StrName: string;
  Ret: boolean;
begin
 Result:= GetPythonEngine.PyBool_FromLong(Ord(false));
 with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 's:exists', @PtrName)) then
    begin
      StrName:= string(PtrName);
      StrName := GetFullPath(StrName);
      ret := fileexists(StrName);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(ret));
    end;
end;

function PyMODELESSMESSAGE(Self, Args : PPyObject): PPyObject; cdecl;
//added for compatibility: one could just call print()
var
  PtrName: PChar;
  StrName: string;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 's:modalmessage', @PtrName)) then
    begin
      StrName:= string(PtrName);
      GLForm1.ScriptOutputMemo.lines.add(StrName);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PyORTHOVIEW(Self, Args : PPyObject): PPyObject; cdecl;
var
  X,Y,Z: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'fff:orthoview', @X,@Y,@Z)) then begin
      gPrefs.DisplayOrient := kAxCorSagOrient;
      GLForm1.UpdateVisibleBoxes();
      Vol1.SetSlice2DFrac(Vec3(X,Y,Z));
      GLForm1.MPRMenu.checked := true;
      ViewGPU1.Invalidate;
      //ORTHOVIEW(X,Y,Z);
    end;
end;

function PyOVERLAYCOLORNUMBER(Self, Args : PPyObject): PPyObject; cdecl;
var
  A,B: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'ii:overlaycolornumber', @A, @B)) then
       GLForm1.LayerChange(A, B, -1, kNaNsingle, kNaNsingle); //kNaNsingle
end;

function PySETCOLORTABLE(Self, Args : PPyObject): PPyObject; cdecl;
var
  i: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'i:setcolortable', @i)) then
       GLForm1.LayerChange(0, i, -1, kNaNsingle, kNaNsingle); //kNaNsingle
end;
{$ENDIF}

function PyCOLORBARSIZE(Self, Args : PPyObject): PPyObject; cdecl;
var
  D: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'f:colorbarsize', @D)) then
       gClrbar.SizeFraction := D;
  ViewGPU1.Invalidate;
end;

function PyVERSION(Self, Args : PPyObject): PPyObject; cdecl;
var
  s: string;
begin
  s := kVers+' PyLib: '+gPrefs.PyLib;
  with GetPythonEngine do
    Result:= PyString_FromString(PChar(s));
end;

function PyLOADIMAGE(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  ret: boolean;
begin
  //Get_ob_refcnt(Args);
  //showmessage(inttostr(Args.Get_ob_refcnt));
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 's:loadimage', @PtrName)) then
    begin
      StrName:= string(PtrName);
      ret := GLForm1.AddBackground(StrName);
      if not ret then
         GLForm1.ScriptOutputMemo.Lines.Add('unable to load "'+StrName+'"');
      Result:= GetPythonEngine.PyBool_FromLong(Ord(ret));
    end;
end;

function PyOVERLAYCLOSEALL(Self, Args : PPyObject): PPyObject; cdecl;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(TRUE));
  vols.CloseAllOverlays;
  GLForm1.UpdateLayerBox(true);
end;

function PyMOSAIC(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 's:mosaic', @PtrName)) then
    begin
      StrName:= string(PtrName);
      gPrefs.DisplayOrient := kMosaicOrient;
      GLForm1.UpdateVisibleBoxes();
      gPrefs.MosaicStr := (StrName);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
      ViewGPU1.Invalidate;
    end;
end;

function PyAZIMUTHELEVATION(Self, Args : PPyObject): PPyObject; cdecl;
var
  A,E: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'ii:azimuthelevation', @A, @E)) then begin
      Vol1.Azimuth := A;
      Vol1.Elevation := E;
      {$IFDEF COMPILEYOKE}
      SetShareFloats3D(Vol1.Azimuth, Vol1.Elevation);
      {$ENDIF}
    end;
    ViewGPU1.Invalidate;
end;

function PyOVERLAYLOAD(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  Ret: boolean;
begin
  Result:= GetPythonEngine.PyInt_FromLong(-1);
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 's:overlayload', @PtrName)) then
    begin
      StrName:= string(PtrName);
      ret := GLForm1.AddLayer(StrName);
      if not ret then
         GLForm1.ScriptOutputMemo.Lines.Add('unable to load "'+StrName+'"');
      Result:= GetPythonEngine.PyBool_FromLong(Ord(ret));
    end;
end;

function PyBACKCOLOR(Self, Args : PPyObject): PPyObject; cdecl;
var
  R,G,B, A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  A := gPrefs.ClearColor.A;
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'iii:backcolor', @R,@G,@B)) then
       gPrefs.ClearColor:= setRGBA(R,G,B,A);
  ViewGPU1.Invalidate;
end;

function PyRESETDEFAULTS(Self, Args : PPyObject): PPyObject; cdecl;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  GLForm1.ResetDefaultsClick(nil);
end;

procedure SetTrackFrac(Track: TTrackBar; frac: single);
begin
     Track.position := Track.Min + round(frac * (Track.Max - Track.Min));
end;

procedure SetTrackPos(Track: TTrackBar; pos: single);
begin
     Track.position := round(pos);
end;

function PyCLIPAZIMUTHELEVATION(Self, Args : PPyObject): PPyObject; cdecl;
var
  D,A,E: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'fff:clipazimuthelevation', @D,@A,@E)) then begin
      SetTrackFrac(GLForm1.ClipDepthTrack, D);
      SetTrackPos(GLForm1.ClipAziTrack, A);
      SetTrackPos(GLForm1.ClipElevTrack, E);
    end;
  ViewGPU1.Invalidate;
end;

function PyCUTOUT(Self, Args : PPyObject): PPyObject; cdecl;
var
  L,A,S,R,P,I: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'ffffff:cutout', @L,@A,@S,@R,@P,@I)) then begin
      SetTrackFrac(GLForm1.XTrackBar, L);
      SetTrackFrac(GLForm1.YTrackBar, A);
      SetTrackFrac(GLForm1.ZTrackBar, S);
      SetTrackFrac(GLForm1.X2TrackBar, R);
      SetTrackFrac(GLForm1.Y2TrackBar, P);
      SetTrackFrac(GLForm1.Z2TrackBar, I);
      GLForm1.CutoutChange(nil);
    end;
end;

function PySHADERLIGHTAZIMUTHELEVATION(Self, Args : PPyObject): PPyObject; cdecl;
var
  A,E: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'ii:shaderlightazimuthelevation', @A, @E)) then begin
       SetTrackPos(GLForm1.LightAziTrack, A);
       SetTrackPos(GLForm1.LightElevTrack, E);
    end;
end;

function PySHARPEN(Self, Args : PPyObject): PPyObject; cdecl;
begin
  SetTrackPos(GLForm1.LightAziTrack, random(180));
   GLForm1.SharpenMenuClick(nil);
   Result:= GetPythonEngine.PyBool_FromLong(Ord(TRUE));

end;

function PyCAMERADISTANCE(Self, Args : PPyObject): PPyObject; cdecl;
var
  Z: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'f:cameradistance', @Z)) then
      Vol1.Distance := (Z);
  ViewGPU1.Invalidate;
end;

function PySHADERADJUST(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  f: single;
  i,j, n: integer;
  function Val2Percent (min,val,max: single): integer;
  var
    S: single;
  begin
    if max = min then
      S := 0
    else if max < min then
      S := 100* ((val-max)/(min-max))
    else
      S := 100* ((val-min)/(max-min));
    if S < 0 then
      S := 0;
    if S > 100 then
      S := 100;
    result := round(S);
  end;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  n := Vol1.ShaderSliders.nUniform;
  if n < 1 then exit;
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'sf:shaderadjust', @PtrName, @f)) then
    begin
      StrName:= string(PtrName);
      j := -1;
      for i := 1 to n do
          if PosEx(Vol1.ShaderSliders.Uniform[i].name, StrName) > 0 then
             j := i;
      for i := 0 to GLForm1.ShaderBox.ControlCount - 1 do begin
            if (GLForm1.ShaderBox.Controls[i].tag <> j) then continue;
            if not (GLForm1.ShaderBox.Controls[i] is TTrackBar) then continue;
            (GLForm1.ShaderBox.Controls[i] as TTrackBar).position := Val2Percent(Vol1.ShaderSliders.Uniform[j].min, f, Vol1.ShaderSliders.Uniform[j].max);
            Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
      end;
    end;
end;

function PySHADERNAME(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  i: integer;
  ret: boolean;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 's:shadername', @PtrName)) then
    begin
      StrName:= string(PtrName);
      i := GLForm1.ShaderDrop.Items.IndexOf(StrName); //search is case-insensitive!
      ret := (i >= 0);
      if ret then begin
         GLForm1.ShaderDrop.ItemIndex := i;
         GLForm1.ShaderDropChange(nil);
      end;
      Result:= GetPythonEngine.PyBool_FromLong(Ord(ret));
    end;
end;

function PyCOLORFROMZERO(Self, Args : PPyObject): PPyObject; cdecl;
var
  Layer, IsFromZero: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'ii:colorfromzero', @Layer, @IsFromZero)) then
    begin
     GLForm1.LayerColorFromZero(Layer, IsFromZero = 1);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PyHIDDENBYCUTOUT(Self, Args : PPyObject): PPyObject; cdecl;
var
  Layer, isHidden: integer;
  niftiVol: TNIfTI;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'ii:hiddenbycutout', @Layer, @isHidden)) then
    begin
     if not vols.Layer(Layer,niftiVol) then exit;
     niftiVol.HiddenByCutout:= (isHidden = 1);
     GLForm1.CutoutChange(nil);
     niftiVol.CX.NeedsUpdate := true;
     Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PyOVERLAYCOLORNAME(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  V, i: integer;
  ret: boolean;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'is:overlaycolorname', @V, @PtrName)) then
    begin
      StrName:= string(PtrName);
      i := GLForm1.LayerColorDrop.Items.IndexOf(StrName); //search is case-insensitive!
      ret := (i >= 0);
      if ret then begin
         GLForm1.LayerChange(V, i, -1, kNaNsingle, kNaNsingle); //kNaNsingle
      end;
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PyOPACITY(Self, Args : PPyObject): PPyObject; cdecl;
var
  PCT: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:opacity', @PCT)) then
       GLForm1.LayerChange(0, -1, PCT, kNaNsingle, kNaNsingle); //kNaNsingle
end;

function PyCOLORBARPOSITION(Self, Args : PPyObject): PPyObject; cdecl;
var
  p: integer;
  v: boolean;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:colorbarposition', @p)) then begin
       //GLForm1.LayerChange(0, -1, PCT, kNaNsingle, kNaNsingle); //kNaNsingle
       v :=  (p <> 0);
       gClrbar.isVisible := v;
       GLForm1.VisibleClrbarMenu.checked := v;
       gPrefs.ColorbarVisible := v;
       if v then
          gPrefs.ColorBarPosition := p;
       GLForm1.SetColorBarPosition;
       ViewGPU1.Invalidate;
    end;
end;

function PyOVERLAYOPACITY(Self, Args : PPyObject): PPyObject; cdecl;
var
  A,B: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'ii:overlayopacity', @A, @B)) then
       GLForm1.LayerChange(A, -1, B, kNaNsingle, kNaNsingle); //kNaNsingle
end;

function PyORTHOVIEWMM(Self, Args : PPyObject): PPyObject; cdecl;
var
  X,Y,Z: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'fff:orthoviewmm', @X,@Y,@Z)) then begin
      gPrefs.DisplayOrient := kAxCorSagOrient;
      GLForm1.UpdateVisibleBoxes();
      GLForm1.SetXHairPosition(X,Y,Z);
      GLForm1.MPRMenu.checked := true;
      ViewGPU1.Invalidate;
    end;
end;

function PyQUIT(Self, Args : PPyObject): PPyObject; cdecl;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(TRUE));
  GLForm1.Close;
end;

function PySAVEBMP(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 's:savebmp', @PtrName)) then
    begin
      StrName:= string(PtrName);
      GLForm1.Save2Bmp(StrName);
    end;
end;

function PyCONTRASTMINMAX(Self, Args : PPyObject): PPyObject; cdecl;
var
  MN,MX: single;
var
   niftiVol: TNIfTI;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(False));
  if not vols.Layer(0,niftiVol) then exit;
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'ff:contrastminmax', @MN,@MX)) then begin
      GLForm1.LayerChange(0, -1, -1, MN, MX); //kNaNsingle
    end;
end;

function PyOVERLAYMINMAX(Self, Args : PPyObject): PPyObject; cdecl;
var
  layer: integer;
  MN,MX: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'iff:overlayminmax', @layer, @MN, @MX)) then begin
       //OVERLAYMINMAX(A,B,C);
       GLForm1.LayerChange(layer, -1, -1, MN, MX); //kNaNsingle
    end;
end;

function PyVOLUME(Self, Args : PPyObject): PPyObject; cdecl;
var
  layer, vol: integer;
  v: TNIfTI;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'ii:overlayminmax', @layer, @vol)) then begin
       if not vols.Layer(layer,v) then exit;
          v.SetDisplayVolume(vol);
        //GLForm1.caption := format('->%d/%d ',[v.VolumeDisplayed+1, v.Header.dim[4]]); //+1 as indexed from 1
        GLForm1.UpdateLayerBox(true);// e.g. "fMRI (1/60)" -> "fMRI (2/60"
        GLForm1.UpdateTimer.Enabled := true;
    end;
end;

function PyBMPZOOM(Self, Args : PPyObject): PPyObject; cdecl;
var
  Z: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'i:bmpzoom', @Z)) then begin
      gPrefs.DisplayOrient := kRenderOrient;
      gPrefs.BitmapZoom:= Z;
      {$IFDEF METALAPI}
      //GLForm1.ScriptOutputMemo.lines.Add('warning: Metal does not yet support bmpzoom()');
      {$ENDIF}
    end;
end;

function PyVIEWAXIAL(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'i:viewaxial', @A)) then begin
      gPrefs.DisplayOrient := kRenderOrient;
      GLForm1.UpdateVisibleBoxes();
      if A = 1 then
         GLForm1.DisplaySuperiorMenu.click
      else
          GLForm1.DisplayInferiorMenu.click;
      ViewGPU1.Invalidate;
    end;
end;

function PyVIEWCORONAL(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'i:viewcoronal', @A)) then begin
      gPrefs.DisplayOrient := kRenderOrient;
      GLForm1.UpdateVisibleBoxes();
      if A = 1 then
         GLForm1.DisplayPosteriorMenu.click
      else
          GLForm1.DisplayAnteriorMenu.click;
      ViewGPU1.Invalidate;
    end;
end;

function PyVIEWSAGITTAL(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'i:viewsagittal', @A)) then begin
      gPrefs.DisplayOrient := kRenderOrient;
      GLForm1.UpdateVisibleBoxes();
      if A = 1 then
         GLForm1.DisplayLeftMenu.click
      else
          GLForm1.DisplayRightMenu.click;
      ViewGPU1.Invalidate;
    end;
end;

function PyWAIT(Self, Args : PPyObject): PPyObject; cdecl;
var
  waitTime: integer;
  endTime : QWord;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'i:wait', @waitTime)) then begin
       if waitTime < 0 then exit;
       endTime := GetTickCount64+DWord(waitTime);
       repeat
              Application.ProcessMessages;//HandleMessage
       until (GetTickCount64 >= endTime);
       while (isBusy) or (GLForm1.Updatetimer.enabled) do
             Application.ProcessMessages;
    end;
end;

function PyLINECOLOR(Self, Args : PPyObject): PPyObject; cdecl;
var
  R,G,B: integer;
  clr: TVec4;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'iii:linecolor', @R,@G,@B)) then begin
      clr := Vol1.Slices.LineColor;
      clr := Vec4(R/255.0, G/255.0, B/255.0, clr.a);
      Vol1.Slices.LineColor := clr;
    end;
end;

function PyLINEWIDTH(Self, Args : PPyObject): PPyObject; cdecl;
var
  W: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'i:linewidth', @W)) then begin
       GLForm1.LineWidthEdit.value := W;
       Vol1.Slices.LineWidth := W;
    end;
    ViewGPU1.Invalidate;
end;

function PyMODALMESSAGE(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 's:modalmessage', @PtrName)) then
    begin
      StrName:= string(PtrName);
      Showmessage(StrName);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PySCRIPTFORMVISIBLE(Self, Args : PPyObject): PPyObject; cdecl;
var
  Vis: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'i:scriptformvisible', @Vis)) then
       GLForm1.ScriptFormVisible(Vis = 1);
end;

function PyTOOLFORMVISIBLE(Self, Args : PPyObject): PPyObject; cdecl;
var
  Vis: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'i:toolformvisible', @Vis)) then begin
      if (Boolean(Vis)) then
         GLForm1.ToolPanel.width := GLForm1.ToolPanel.Constraints.MaxWidth
      else
          GLForm1.ToolPanel.width := 0;
    end;
end;

function PySHADERQUALITY1TO10(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'i:shaderquality1to10', @A)) then
       GLForm1.QualityTrack.Position := A;
end;

function PySHADERUPDATEGRADIENTS(Self, Args : PPyObject): PPyObject; cdecl;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(TRUE));
  GLForm1.UpdateTimer.Enabled := true;
end;

function PyEXTRACT(Self, Args : PPyObject): PPyObject; cdecl;
var
  Otsu,Dil,One: integer;
  niftiVol: TNIfTI;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  if not vols.Layer(0,niftiVol) then exit;
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Boolean(PyArg_ParseTuple(Args, 'iii:extract', @Otsu,@Dil,@One)) then begin
      //EXTRACT(Otsu,Dil,Boolean(One));
      niftiVol.RemoveHaze; //todo: dilation and number of Otsu layers
      ViewGPU1.Invalidate;
    end;
end;

function PyOVERLAYLOADSMOOTH(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:overlayloadsmooth', @A)) then begin
       Vols.InterpolateOverlays := A = 1;
       GLForm1.LayerSmoothMenu.Checked := A = 1;
       //no need to refresh: inlfuences future loads
    end;
end;

function PyOVERLAYADDITIVEBLENDING(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:overlayadditiveblending', @A)) then begin
       Vols.AdditiveOverlayBlending := A = 1;
       GLForm1.LayerAdditiveMenu.Checked := A = 1;
       GLForm1.ForceOverlayUpdate();
       GLForm1.UpdateTimer.Enabled := true;
    end;
end;

function PyOVERLAYMASKWITHBACKGROUND(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:overlayadditiveblending', @A)) then begin
       Vols.MaskWithBackground := A = 1;
       GLForm1.LayerMaskWithBackgroundMenu.Checked := A = 1;
       GLForm1.ForceOverlayUpdate();
       GLForm1.UpdateTimer.Enabled := true;
    end;
end;

procedure TGLForm1.PyModInitialization(Sender: TObject);
begin
  with Sender as TPythonModule do begin
    //AddMethod('smoothmask', @PySMOOTHMASK, ' smoothmask(i) -> Blur edges of a masked image.');
    AddMethod('azimuthelevation', @PyAZIMUTHELEVATION, ' azimuthelevation(azi, elev) -> Sets the camera location.');
    AddMethod('backcolor', @PyBACKCOLOR, ' backcolor(r, g, b) -> changes the background color, for example backcolor(255, 0, 0) will set a bright red background');
    AddMethod('bmpzoom', @PyBMPZOOM, ' bmpzoom(z) -> changes resolution of savebmp(), for example bmpzoom(2) will save bitmaps at twice screen resolution');
    AddMethod('cameradistance', @PyCAMERADISTANCE, ' cameradistance(z) -> Sets the viewing distance from the object.');
    AddMethod('colorbarposition', @PyCOLORBARPOSITION, ' colorbarposition(p) -> Set colorbar position (0=off, 1=top, 2=right).');
    AddMethod('colorbarsize', @PyCOLORBARSIZE, ' colorbarsize(p) -> Change width of color bar f is a value 0.01..0.5 that specifies the fraction of the screen used by the colorbar.');
    AddMethod('clipazimuthelevation', @PyCLIPAZIMUTHELEVATION, ' clipazimuthelevation(depth, azi, elev) -> Set a view-point independent clip plane.');
    AddMethod('cutout', @PyCUTOUT, ' cutout(L,A,S,R,P,I) -> Remove sector from volume.');
    AddMethod('extract', @PyEXTRACT, ' extract(Otsu,Dil,One) -> Remove haze.');
    AddMethod('hiddenbycutout', @PyHIDDENBYCUTOUT, ' hiddenbycutout(layer, isHidden) -> Will cutout hide (1) or show (0) this layer?');
    AddMethod('linecolor', @PyLINECOLOR, ' linecolor(r,g,b) -> Set color of crosshairs, so "linecolor(255,0,0)" will use bright red lines.');
    AddMethod('linewidth', @PyLINEWIDTH, ' linewidth(wid) -> Set thickness of crosshairs used on 2D slices.');
    AddMethod('loadimage', @PyLOADIMAGE, ' loadimage(imageName) -> Close all open images and load new background image.');
    AddMethod('modalmessage', @PyMODALMESSAGE, ' modalmessage(msg) -> Show a message in a dialog box, pause script until user presses "OK" button.');
    AddMethod('mosaic', @PyMOSAIC, ' mosaic(mosString) -> Create a series of 2D slices.');
    AddMethod('orthoviewmm', @PyORTHOVIEWMM, ' orthoviewmm(x,y,z) -> Show 3 orthogonal slices of the brain, specified in millimeters.');
    AddMethod('opacity', @PyOPACITY, ' opacity(opacityPct) -> Is the background image transparent (0), translucent (~50) or opaque (100)?');
    AddMethod('overlayadditiveblending', @PyOVERLAYADDITIVEBLENDING, ' overlayadditiveblending(v) -> Merge overlays using additive (1) or multiplicative (0) blending.');
    AddMethod('overlaymaskwithbackground', @PyOVERLAYMASKWITHBACKGROUND, 'overlaymaskwithbackground(v) -> hide (1) or show (0) overlay voxels that are transparent in background image.');
    AddMethod('overlaycloseall', @PyOVERLAYCLOSEALL, ' overlaycloseall() -> Close all open overlays.');
    AddMethod('colorname', @PyOVERLAYCOLORNAME, ' colorname(layer, colorName) -> Set the colorscheme for the target overlay (0=background layer) to a specified name.');
    AddMethod('colorfromzero', @PyCOLORFROMZERO, ' colorfromzero(layer, isFromZero) -> Color scheme display range from zero (1) or from treshold value (0)?');
    AddMethod('overlayload', @PyOVERLAYLOAD, ' overlayload(filename) -> Load an image on top of prior images.');
    AddMethod('overlayloadsmooth', @PyOVERLAYLOADSMOOTH, ' overlayloadsmooth(0) -> Will future overlayload() calls use smooth (1) or jagged (0) interpolation?');
    AddMethod('minmax', @PyOVERLAYMINMAX, ' minmax(layer, min, max) -> Sets the color range for the overlay (layer 0 = background).');
    AddMethod('opacity', @PyOVERLAYOPACITY, ' opacity(layer, opacityPct) -> Make the layer (0 for background, 1 for 1st overlay) transparent(0), translucent (~50) or opaque (100).');
    AddMethod('quit', @PyQUIT, ' quit() -> Terminate the application.');
    AddMethod('resetdefaults', @PyRESETDEFAULTS, ' resetdefaults() -> Revert settings to sensible values.');
    AddMethod('savebmp', @PySAVEBMP, ' savebmp(pngName) -> Save screen display as bitmap. For example "savebmp(''test.png'')"');
    AddMethod('scriptformvisible', @PySCRIPTFORMVISIBLE, ' scriptformvisible (visible) -> Show (1) or hide (0) the scripting window.');
    AddMethod('toolformvisible', @PyTOOLFORMVISIBLE, ' toolformvisible(visible) -> Show (1) or hide (0) the tool panel.');
    AddMethod('shaderadjust', @PySHADERADJUST, ' shaderadjust(sliderName, sliderValue) -> Set level of shader property. Example "gl.shaderadjust(''edgethresh'', 0.6)"');
    AddMethod('shaderlightazimuthelevation', @PySHADERLIGHTAZIMUTHELEVATION, ' shaderlightazimuthelevation(a,e) -> Position the light that illuminates the rendering. For example, "shaderlightazimuthelevation(0,45)" places a light 45-degrees above the object');
    AddMethod('shadername', @PySHADERNAME, ' shadername(name) -> Choose rendering shader function. For example, "shadername(''mip'')" renders a maximum intensity projection.');
    AddMethod('shaderquality1to10', @PySHADERQUALITY1TO10, ' shaderquality1to10(i) -> Renderings can be fast (1) or high quality (10), medium values (6) balance speed and quality.');
    AddMethod('shaderupdategradients', @PySHADERUPDATEGRADIENTS, ' shaderupdategradients() -> Recalculate volume properties.');
    AddMethod('sharpen', @PySHARPEN, ' sharpen() -> apply unsharp mask to background volume to enhance edges');
    AddMethod('version', @PyVERSION, ' version() -> Return the version of MRIcroGL.');
    AddMethod('viewaxial', @PyVIEWAXIAL, ' viewaxial(SI) -> Show rendering with camera superior (1) or inferior (0) of volume.');
    AddMethod('viewcoronal', @PyVIEWCORONAL, ' viewcoronal(AP) -> Show rendering with camera posterior (1) or anterior (0) of volume.');
    AddMethod('viewsagittal', @PyVIEWSAGITTAL, ' viewsagittal(LR) -> Show rendering with camera left (1) or right (0) of volume.');
    AddMethod('volume', @PyVOLUME, ' volume(layer, vol) -> For 4D images, set displayed volume (layer 0 = background; volume 0 = first volume in layer).');
    AddMethod('wait', @PyWAIT, ' wait(ms) -> Pause script for (at least) the desired milliseconds.');
    {$IFDEF PYOBSOLETE}
    AddMethod('azimuth', @PyAZIMUTH, ' azimuth(degrees) -> Rotates the rendering.');
    AddMethod('clip', @PyCLIP, ' clip(depth) -> Creates a clip plane that hides information close to the viewer.');
    AddMethod('colorname', @PyCOLORNAME, ' colorname(colorName) -> Loads  the requested colorscheme for the background image.');
    AddMethod('contrastminmax', @PyCONTRASTMINMAX, ' contrastminmax(min,max) -> Sets the minumum nd maximum value for the color lookup table.');
    AddMethod('elevation', @PyELEVATION, ' elevation(degrees) -> Rotates volume rendering relative to camera.');
    AddMethod('exists', @PyEXISTS, ' exists(imageName) -> Is image in path? Example: "exists(''motor'') returns true if image ''motor.nii.gz'' found');
    AddMethod('modelessmessage', @PyMODELESSMESSAGE, ' modelessmessage(msg) -> An alias for "print(''msg'')".'); //use print('Hello')
    AddMethod('orthoview', @PyORTHOVIEW, ' orthoview(x,y,z) -> Show 3 orthogonal slices of the brain, specified as a fraction of image size.');
    AddMethod('overlaycolornumber', @PyOVERLAYCOLORNUMBER, 'overlaycolornumber(overlayNum, colorNum) -> Set the colorscheme for the target overlay specified by number.');
    AddMethod('setcolortable', @PySETCOLORTABLE, ' setcolortable(tablenum) -> Changes the color scheme used to display an image.');
    {$ENDIF}
    with DocString do begin //print(gl.__doc__)
        Add( 'The gl module displays NIfTI format images' );
        Add( ' To see all functions: "print(dir(gl))"' );
        Add( ' To see details for a specific function: "print(gl.wait.__doc__)"' );
      end;
  end;
end;

function TGLForm1.PyExec(): boolean;
begin
  result := false; //assume code is not Python
  ScriptOutputMemo.lines.Clear;
  if PyEngine = nil then begin
    if not PyCreate then begin //do this the first time
       {$IFDEF Windows}
       ScriptOutputMemo.lines.Add('Unable to find Python library [place Python .dll and .zip in Script folder]');
       {$ENDIF}
       {$IFDEF Unix}
       ScriptOutputMemo.lines.Add('Unable to find Python library');
       {$IFDEF Darwin}
       ScriptOutputMemo.lines.Add('   For MacOS this is typically in: '+kBasePath+'');
       {$ELSE}
       ScriptOutputMemo.lines.Add('   run ''find -name "*libpython*"'' to find the library');
       ScriptOutputMemo.lines.Add('   if it does not exist, install it (e.g. ''apt-get install libpython2.7'')');
       {$ENDIF}
       ScriptOutputMemo.lines.Add('   if it does exist, set use the Preferences/Advanced to set ''PyLib''');
       {$IFDEF Darwin}
       //otool -L $(which python)
       ScriptOutputMemo.lines.Add('   PyLib should be the complete path and filename of libpython*.dylib');
       {$ELSE}
       ScriptOutputMemo.lines.Add('   PyLib should be the complete path and filename of libpython*.so');
       {$ENDIF}
       ScriptOutputMemo.lines.Add('   This file should be in your LIBDIR, which you can detect by running Python from the terminal:');
       ScriptOutputMemo.lines.Add('     ''import sysconfig; print(sysconfig.get_config_var("LIBDIR"))''');
       {$ENDIF}
       result := true;
       exit;
    end;
  end;
  result := true;
  ScriptOutputMemo.lines.Add('Running Python script');
  gPyRunning := true;
  try
     PyEngine.ExecStrings(GLForm1.ScriptMemo.Lines);
  except
    ScriptOutputMemo.lines.Add('Python Engine Failed');
    gPyRunning := false;
  end;
  ScriptOutputMemo.lines.Add('Python Succesfully Executed');
  gPyRunning := false;
  result := true;
end;

procedure TGLForm1.PyEngineAfterInit(Sender: TObject);
{$IFDEF WINDOWS}
var
  dir: string;
begin
  dir:= ExtractFilePath(Application.ExeName);
  Py_SetSysPath([ScriptDir, changefileext(gPrefs.PyLib,'.zip')], false);
  //Py_SetSysPath([ScriptDir], true);
end;
{$ELSE}
begin
    //
end;

{$ENDIF}

 {$ENDIF}//python

procedure TGLForm1.YokeMenuClick(Sender: TObject);
begin
  {$IFDEF COMPILEYOKE}
  YokeTimer.Enabled := YokeMenu.Checked;
  {$ELSE}
  Showmessage('Recompile for yoking support');
  {$ENDIF}
end;

procedure TGLForm1.YokeTimerTimer(Sender: TObject);
{$IFDEF COMPILEYOKE}
var
   lAzimuth, lElevation,lXmm,lYmm,lZmm: single;
   sliceMM: TVec3;
begin
  YokeTimer.Enabled := YokeMenu.Checked;
  if not YokeMenu.Checked then exit;
  if (gPrefs.DisplayOrient = kMosaicOrient) then exit;//not for mosaics
  if not GetShareFloats(lXmm,lYmm,lZmm, lAzimuth, lElevation) then
     exit;
  if  (gPrefs.DisplayOrient = kRenderOrient) then  begin //not 2D slice view: assume rendering
      if (Vol1.Azimuth = round(lAzimuth)) and (Vol1.Elevation = round(lElevation) ) then exit;
      Vol1.Azimuth := round(lAzimuth);
      Vol1.Elevation := round(lElevation);
      ViewGPU1.Invalidate;
      exit;
  end;
  sliceMM := Vec3(lXmm, lYmm, lZmm);
  if (sliceMM = gSliceMM) then exit;
  SetXHairPosition(lXmm,lYmm,lZmm );
  gSliceMM := sliceMM;
  //GLBox.Invalidate;
end;
{$ELSE}
begin
 //
end;
{$ENDIF}

procedure TGLForm1.SetDisplayCheck();
begin
  case gPrefs.DisplayOrient of
      kRenderOrient: RenderMenu.Checked := true;
      kAxialOrient: AxialMenu.Checked := true;
      kCoronalOrient: CoronalMenu.Checked := true;
      kSagRightOrient: SagittalMenu.Checked := true;
      kSagLeftOrient: SagittalLMenu.Checked := true;
      kAxCorSagOrient: MPRMenu.Checked := true;
      kMosaicOrient: MosaicMenu.Checked := true;
  end;
end;

procedure TGLForm1.UpdateVisibleBoxes(setMenuChecked: boolean = false);
begin
     if (setMenuChecked) then
        SetDisplayCheck();
     //SliceBox.Visible := gPrefs.DisplayOrient = kRenderOrient;
     ClipBox.Visible := gPrefs.DisplayOrient = kRenderOrient;
     CutoutBox.Visible := gPrefs.DisplayOrient = kRenderOrient;
     ShaderBox.Visible := gPrefs.DisplayOrient = kRenderOrient;
     SliceBox.Visible := gPrefs.DisplayOrient <= kAxCorSagOrient;
     AnatDrop.Visible := length(gLandmark.Landmarks) > 0;
     LandmarkMenu.Visible := gPrefs.LandmarkPanel;
     LineBox.Visible := gPrefs.DisplayOrient <= kMosaicOrient;
     MosaicBox.Visible := gPrefs.DisplayOrient = kMosaicOrient;

end;

procedure TGLForm1.UpdateOpenRecent();
var
 i: integer;
 imgName: string;
   newMenu: TMenuItem;
begin
 while OpenRecentMenu.Count > 0 do
       OpenRecentMenu.Delete(OpenRecentMenu.Count - 1);
 for i := 1 to knMRU do begin
     imgName := gPrefs.PrevFilename[i];
     if (imgName = '') then continue;
     if (not fileexists(imgName)) then continue;
     imgName := ChangeFileExt(ExtractFileName(imgName),'');
     if upcase(ExtractFileExt(imgName))= '.NII' then
        imgName := ChangeFileExt(imgName,''); //img.nii.gz -> img
     newMenu := TMenuItem.Create(MainMenu);
     newMenu.Caption := imgName;
     newMenu.OnClick:= @OpenRecentMenuClick;
     newMenu.Tag:=i;
     OpenRecentMenu.Add(newMenu);
 end;
end;

procedure TGLForm1.AddOpenRecent(Filename: string);
begin
 if extractfiledir(Filename) = StandardDir then exit;
 Add2MRU (gPrefs.PrevFilename, Filename);
 UpdateOpenRecent();
end;

procedure TGLForm1.CreateStandardMenus;
var
  standardNamesGZ, standardNames : TStringList;
  standardName, ext: string;
  i: integer;
  newMenu: TMenuItem;
begin
   OpenStandardMenu.Enabled:= false;
   if not DirectoryExists(StandardDir) then exit;//showmessage('Unable to find folder "'+StandardDir+'"');
   standardNames := FindAllFiles(StandardDir, '*.nii', false);
   try
    standardNamesGZ := FindAllFiles(StandardDir, '*.nii.gz', false);
    try
      standardNames.AddStrings(standardNamesGZ);
    finally
      standardNamesGZ.Free;
    end;
    if standardNames.Count > 0 then begin
      OpenStandardMenu.Enabled:= true;
      standardNames.Sort;
      for i := 0 to (standardNames.Count-1) do begin
          standardName := ChangeFileExt(ExtractFileName(standardNames[i]),'');
          ext := uppercase(extractfileext(standardName));
          if (ext = '.NII') then
             standardName := ChangeFileExt(ExtractFileName(standardName),'');
          if (length(standardName) < 1) or (standardName[1] = '_') or (standardName[1] = '.') then
             continue;
          newMenu := TMenuItem.Create(MainMenu);
          newMenu.Caption := standardName;
          newMenu.OnClick:= @OpenStandardMenuClick;
          OpenStandardMenu.Add(newMenu);
      end;
    end;
   finally
     standardNames.Free;
   end;
end;

procedure TGLForm1.AnatUpdate();
var
  i: integer;
begin
  AnatDrop.Items.Clear;
  if length(gLandmark.Landmarks) < 1 then begin
     AnatDrop.Visible := false;
    exit;
  end;
  for i := 0 to length(gLandmark.Landmarks)-1 do
    AnatDrop.Items.Add(gLandmark.Landmarks[i].Name);
  AnatDrop.ItemIndex := length(gLandmark.Landmarks)-1;
  AnatDrop.Visible := true;
end;

procedure TGLForm1.AnatSaveBtnClick(Sender: TObject);
const
     kAnatFilter = 'AnatomyFile|*.anat';
     kSep = chr(9);
var
   saveDlg : TSaveDialog;
   lF: TextFile;
   i: integer;
begin
  if length(gLandmark.Landmarks) < 1 then begin
     showmessage('No landmarks open - either open a file or create new landmarks');
     exit;
  end;
  saveDlg := TSaveDialog.Create(self);
  saveDlg.Filter := kAnatFilter;
  saveDlg.DefaultExt := '.anat';
  saveDlg.Filename := gLandmark.Filename; //10102006
  if not saveDlg.Execute then exit;
  Filemode := 0;
  AssignFile(lF, saveDlg.Filename);
  rewrite(lF);
  for i := 0 to length(gLandmark.Landmarks)-1 do
      Writeln(lF, gLandmark.Landmarks[i].Name+kSep+floattostr(gLandmark.Landmarks[i].X)+kSep+floattostr(gLandmark.Landmarks[i].Y)+kSep+floattostr(gLandmark.Landmarks[i].Z)  );
  CloseFile(lF);
end;

procedure TGLForm1.AnatUpdateBtnClick(Sender: TObject);
begin
 if not LandmarkMenu.visible then exit;
 gLandmark.UpdateCoord(AnatDrop.ItemIndex, gSliceMM.X, gSliceMM.Y, gSliceMM.Z);
end;

procedure TGLForm1.AnatAddBtnClick(Sender: TObject);
var
  s: string;
  i: integer;
  lOK: boolean;
begin
  if not LandmarkMenu.visible then exit;
  i := length(gLandmark.Landmarks)+1;
  s := 'A'+inttostr(i);
  lOK := InputQuery('Enter a name', 'region name', s);
  if not lOK then
    exit;
  setlength(gLandmark.Landmarks,i);
  gLandmark.Landmarks[i-1].Name := s;
  gLandmark.UpdateCoord(i-1, gSliceMM.X, gSliceMM.Y, gSliceMM.Z);
  AnatUpdate();
end;

procedure TGLForm1.LandmarkSelectNextClick(Sender: TObject);
begin
    if AnatDrop.Items.Count < 2 then exit;
    if AnatDrop.ItemIndex >= (AnatDrop.Items.Count -1) then
       AnatDrop.ItemIndex := 0
    else
        AnatDrop.ItemIndex := AnatDrop.ItemIndex + 1;
    AnatDropChange(Sender);
end;

procedure TGLForm1.AnatDeleteBtnClick(Sender: TObject);
var
  p,i,l: integer;
begin
  if not LandmarkMenu.visible then exit;
  l := Length(gLandmark.Landmarks);
  i := AnatDrop.ItemIndex;
  if (l < 1) or (i >= l) or (i < 0) then
    exit;
  if i < (l-1) then
    for p := i+1 to l-1 do
      gLandmark.Landmarks[p-1] := gLandmark.Landmarks[p];
  SetLength(gLandmark.Landmarks,l-1);
  AnatUpdate();
end;

procedure TGLForm1.AnatDropChange(Sender: TObject);
var
   i : integer;
begin
     i := AnatDrop.ItemIndex;
     if (i < 0) or (i >= Length(gLandmark.Landmarks)) then exit;
     XCoordEdit.Text := format('%.6g', [gLandmark.Landmarks[i].X]);//floattostr(gLandmark.Landmarks[i].X);
     YCoordEdit.Text := format('%.6g', [gLandmark.Landmarks[i].Y]);
     ZCoordEdit.Text := format('%.6g', [gLandmark.Landmarks[i].Z]);
     CoordEditChange(Sender);
end;

procedure TGLForm1.AnatOpenBtnClick(Sender: TObject);
const
     kAnatFilter = 'AnatomyFile|*.anat';
var
   openDlg : TOpenDialog;
begin
  openDlg := TOpenDialog.Create(self);
  openDlg.InitialDir := GetCurrentDir;
  openDlg.Options := [ofFileMustExist];
  openDlg.Filter := kAnatFilter;
  if openDlg.Execute then begin
    gLandmark.Open(openDlg.Filename);
    AnatUpdate();
    AnatDropChange(Sender);
  end;
  openDlg.Free;
end;

procedure TGLForm1.OpenStandardMenuClick(Sender: TObject);
var
  ss: TShiftState;
begin
  ss := getKeyshiftstate;
  if (ssMeta in ss) or (ssCtrl in ss) then
     AddLayer(StandardDir +pathdelim +(sender as TMenuItem).caption)
  else
      AddBackground(StandardDir +pathdelim +(sender as TMenuItem).caption, false);
end;

procedure TGLForm1.OpenRecentMenuClick(Sender: TObject);
var
  ss: TShiftState;
begin
  ss := getKeyshiftstate;
  if (ssMeta in ss) or (ssCtrl in ss) then
     AddLayer(gPrefs.PrevFilename[(sender as TMenuItem).tag])
  else
     AddBackground(gPrefs.PrevFilename[(sender as TMenuItem).tag]);
end;

procedure TGLForm1.SetFormDarkMode(var f: TForm);
begin
 {$IFDEF LCLCocoa}{$IFDEF NewCocoa}
  if not gPrefs.DarkMode then exit;
  f.PopupMode:= pmAuto;
  f.HandleNeeded;
  setThemeMode(f, true);
 {$ENDIF} {$ENDIF}
end;

procedure TGLForm1.SetDarkMode();
begin
  {$IFDEF LCLCocoa}{$IFDEF NewCocoa}
  setThemeMode(Self, gPrefs.DarkMode);
  if gPrefs.DarkMode then begin
     ScriptMemo.Color := clGray;
     ScriptOutputMemo.Color := clGray;
     MosaicText.Color := clGray;
  end else begin
      ScriptMemo.Color := clDefault;
      ScriptOutputMemo.Color := clDefault;
      MosaicText.Color := clDefault;
  end;
  {$ENDIF}{$ENDIF}
end;

procedure TGLForm1.CutoutChange(Sender: TObject);
var
   niftiVol: TNIfTI;
   CutoutLow, CutoutHigh: TVec3;
   i: integer;
begin
     CutoutLow.x := XTrackBar.Position / XTrackBar.Max;
     CutoutLow.y := YTrackBar.Position / YTrackBar.Max;
     CutoutLow.z := ZTrackBar.Position / ZTrackBar.Max;
     CutoutHigh.x := X2TrackBar.Position / X2TrackBar.Max;
     CutoutHigh.y := Y2TrackBar.Position / Y2TrackBar.Max;
     CutoutHigh.z := Z2TrackBar.Position / Z2TrackBar.Max;
     if not vols.Layer(0,niftiVol) then exit;
     if niftiVol.HiddenByCutout then
        niftiVol.SetCutoutNoUpdate(CutoutLow, CutoutHigh);
     if (vols.NumLayers > 1) then begin
        //CutoutBox.Caption := inttostr(vols.NumLayers)+':'+ inttostr(888);
        for i := 2 to vols.NumLayers do begin
            if not vols.Layer(i-1,niftiVol) then exit;
            if not niftiVol.HiddenByCutout then continue;
            niftiVol.SetCutoutNoUpdate(CutoutLow, CutoutHigh);
            niftiVol.DisplayMinMax2Uint8(true);
            niftiVol.CX.NeedsUpdate := true;
        end;
     end;
     UpdateTimer.Enabled:=true;
end;

procedure TGLForm1.CutoutMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
     CutoutChange(Sender);
end;

procedure TGLForm1.OpenScript(scriptname: string; isShowScriptPanel: boolean = true);
begin
     if not fileexists(scriptname) then exit;
     if (ScriptPanel.Width < 24) and (isShowScriptPanel) then
        ScriptPanel.Width := 240;
     ScriptMemo.Lines.LoadFromFile(scriptname);
     ScriptingRunMenuClick(nil);
end;

procedure TGLForm1.ScriptingTemplatesMenuClick(Sender: TObject);
var
 shaderName: string;
begin
 shaderName := ResourceDir+pathdelim+'script' + pathdelim + (Sender as TMenuItem).caption+'.py';
 if not fileexists(shaderName) then
    showmessage('Unable to find '+shaderName);
 OpenScript(shaderName);
end;

procedure TGLForm1.DisplayViewMenu(Sender: TObject);
var
 i: integer;
begin
     i := (Sender as TMenuItem).Tag;
     if (gPrefs.DisplayOrient <> kRenderOrient) then begin
        case i of
             0: SliceLBtn.Click;
             1: SliceRBtn.Click;
             2: SlicePBtn.Click;
             3: SliceABtn.Click;
             4: SliceIBtn.Click;
             else SliceSBtn.Click;
        end;
        exit;
     end;
     case i of
          0: Vol1.Azimuth:=90;
          1: Vol1.Azimuth:=270;
          3: Vol1.Azimuth:=180;
          4: Vol1.Azimuth:=180;
          else Vol1.Azimuth:=0;
     end;
     case i of
          4: Vol1.Elevation := -90;
          5: Vol1.Elevation := 90;
          else Vol1.Elevation := 0;
     end;
     if (gPrefs.DisplayOrient <> kRenderOrient) then begin
        RenderMenu.Checked := true;
        gPrefs.DisplayOrient := kRenderOrient;
        UpdateVisibleBoxes();
     end;
     {$IFDEF COMPILEYOKE}
     SetShareFloats3D(Vol1.Azimuth, Vol1.Elevation);
     {$ENDIF}
     ViewGPU1.Invalidate;
end;

procedure TGLForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
     if (not vols.Drawing.IsOpen) then exit;
     if (vols.Drawing.voiIsEmpty) then begin
        vols.Drawing.voiClose;
        exit;
     end;
     DrawCloseMenuClick(nil);
     //Cocoa svn 59338 generates an error if we close immediately after a Dialog
     //we have three options...
     // showmessage('Goodbye');
     // CanClose := false;
     Application.ProcessMessages;
end;

procedure TGLForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 gPrefs.CustomDcm2niix := dcm2niiForm.getCustomDcm2niix();
 IniFile(false, gPrefs);
 Vol1.Free;
end;

procedure TGLForm1.HelpMenuClick(Sender: TObject);
begin

end;

procedure TGLForm1.ImportDicomMenuClick(Sender: TObject);
begin
 {$IFDEF LCLCocoa}
 dcm2niiForm.show;
 {$ELSE}
 dcm2niiForm.showmodal;
 {$ENDIF}
end;

procedure TGLForm1.LayerWidgetChange(Sender: TObject);
var
  i, pct: integer;
  niftiVol: TNIfTI;
  mn,mx: single;
  lutName: string;
  isChange: boolean = false;
begin
 i := LayerList.ItemIndex;
  if not vols.Layer(i,niftiVol) then exit;
  mn := strtofloatdef(LayerDarkEdit.Caption, niftiVol.DisplayMin);
  mx := strtofloatdef(LayerBrightEdit.Caption, niftiVol.DisplayMax);
  //layerBox.Caption := format('%d %g %g',[random(888), mn, mx]);
  pct := LayerAlphaTrack.position;
  if (niftiVol.FullColorTable.Tag <> LayerColorDrop.ItemIndex) then begin
   lutName := ResourceDir + pathdelim + 'lut' + pathdelim + LayerColorDrop.Items[LayerColorDrop.ItemIndex]+'.clut';
   niftiVol.SetDisplayColorScheme(lutName, LayerColorDrop.ItemIndex);
   UpdateLayerBox(false);
   updateTimer.enabled := true;
   exit;
  end;
  if pct <> niftiVol.OpacityPercent then isChange := true;
  if mn <> niftiVol.DisplayMin then isChange := true;
  if mx <> niftiVol.DisplayMax then isChange := true;
  if (not isChange) then exit;
  if Vols.MaskWithBackground then ForceOverlayUpdate();
  niftiVol.SetDisplayMinMaxNoUpdate(mn, mx); //defer time consuming work
  niftiVol.OpacityPercent := pct;
  updateTimer.enabled := true;
end;

procedure TGLForm1.LayerAlphaTrackMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LayerWidgetChange(sender);
end;

procedure TGLForm1.UpdateLayerBox(NewLayers: boolean);
var
   i: integer;
   v: TNIfTI;
   s: string;
   isMultiVol: boolean;
begin
     if (NewLayers) then begin
        LayerList.Items.Clear;
        isMultiVol := false;
        if vols.NumLayers < 1 then exit;
        for i := 1 to vols.NumLayers do begin
            vols.Layer(i-1,v);
            if v.Header.dim[4] > 1 then
               isMultiVol := true;
            s := v.ShortName;
            if v.Header.dim[4] > 1 then
               s := format('%d/%d: ', [v.VolumeDisplayed+1, v.Header.dim[4]] )+s;
            LayerList.Items.add(s);
            LayerList.Checked[i-1] := true;
        end;
        LayerList.ItemIndex := vols.NumLayers - 1;
        DisplayPrevMenu.Enabled := isMultiVol;
        DisplayNextMenu.Enabled := isMultiVol;
        DisplayAnimateMenu.Enabled := isMultiVol;
     end;
     if (LayerList.ItemIndex < 0)  then
        LayerList.ItemIndex := LayerList.Items.Count -1;
     if (vols.NumLayers < 0) or (vols.NumLayers < LayerList.Items.Count) then exit;
     if (LayerList.ItemIndex < 0) then exit;
     vols.Layer(LayerList.ItemIndex,v);
     LayerDarkEdit.Enabled := not v.Drawing;
     LayerBrightEdit.Enabled := not v.Drawing;
     LayerColorDrop.Enabled := not v.Drawing;
     if v.IsLabels then
        LayerColorDrop.Enabled := false;
     LayerDarkEdit.Text := format('%.6g', [v.DisplayMin]);
     LayerBrightEdit.Text := format('%.6g', [v.DisplayMax]);
     LayerColorDrop.ItemIndex := v.FullColorTable.Tag;
     LayerAlphaTrack.Position := v.OpacityPercent;
end;

procedure TGLForm1.LayerChange(layer, colorTag, opacityPercent: integer; displayMin, displayMax: single); //kNaNsingle
//to leave colorTag or opacity unchanged use -1 as input,
// to leave displayMin or displayMax unchanged use kNaNsingle as input
var
   v: TNIfTI;
   mn, mx: single;
   isChanged: boolean;
   lutName: string;
begin
     isChanged := false;
     if not vols.Layer(layer,v) then exit; //layer not loaded
     mn := v.DisplayMin;
     if (displayMin <> kNaNsingle) and (mn <> displayMin) then begin
         isChanged := true;
         mn := displayMin;
     end;
     mx := v.DisplayMax;
     if (displayMax <> kNaNsingle) and (mx <> displayMax) then begin
        isChanged := true;
        mx := displayMax;
     end;
     if (isChanged) then
        v.SetDisplayMinMaxNoUpdate(mn, mx);
     if (colorTag >= 0) and (v.FullColorTable.Tag <> colorTag) then begin
        isChanged := true;
        lutName := ResourceDir + pathdelim + 'lut' + pathdelim + LayerColorDrop.Items[colorTag]+'.clut';
        v.SetDisplayColorScheme(lutName, colorTag);
     end;
     if (opacityPercent >= 0) and (v.OpacityPercent <> opacityPercent) then begin
         isChanged := true;
         v.OpacityPercent := opacityPercent;
     end;
     if not isChanged then exit;
     if layer = LayerList.ItemIndex then begin
        UpdateLayerBox(false);
     end;
     UpdateTimer.Enabled := true;
end;

procedure TGLForm1.LayerListSelectionChange(Sender: TObject; User: boolean);
begin
     UpdateLayerBox(false);
end;

procedure TGLForm1.LayerContrastKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
     LayerWidgetChange(Sender);
end;

procedure TGLForm1.LineColorBtnClick(Sender: TObject);
var
 clr: TVec4;
begin
 clr := Vol1.Slices.LineColor;
 clr *= 255;
 ColorDialog1.Color:= RGBToColor(round(clr.r), round(clr.g), round(clr.b));
 if not ColorDialog1.Execute then exit;
 clr := Vec4(Red(ColorDialog1.Color)/255.0, Green(ColorDialog1.Color)/255.0, Blue(ColorDialog1.Color)/255.0, clr.a);
 Vol1.Slices.LineColor := clr;
 ViewGPU1.Invalidate;
end;

procedure TGLForm1.LineWidthEditChange(Sender: TObject);
begin
  if (gPrefs.LineWidth = LineWidthEdit.value) then exit;
   gPrefs.LineWidth := LineWidthEdit.value;
  Vol1.Slices.LineWidth := gPrefs.LineWidth;
  ViewGPU1.Invalidate;
end;

procedure TGLForm1.MosaicTextChange(Sender: TObject);
begin
     gPrefs.MosaicStr := MosaicText.Text;
     ViewGPU1.Invalidate;
end;

function lerpFraction (frac: single; min,max: single): integer;
var
  f: single;
begin
     if frac < 0.0 then
        f := min
     else if frac > 1.0 then
        f := max
     else
         f := min + (frac * (max-min));
     result := round (f);
     if odd(result) then result := result + 1;
     if result > max then result := result -2;
end;

procedure TGLForm1.UpdateMosaic(Sender: TObject);
var
  lXYZmmMin, lXYZmmMax : TVec3;
  isMNISpaceV: boolean;
  lRi,lCi,lR,lC,lRxC,lI, lmm, lO: integer;
  lInterval, lFrac: single;
  lOrthoCh: Char;
  lStr: string;
  niftiVol: TNIfTI;
begin
 if gPrefs.DisplayOrient <> kMosaicOrient then exit;
     //determine if image is in MNI spase - if so use mm slices
 if vols = nil then exit; //mungo : mosaic selected before image is loaded
 if not vols.Layer(0,niftiVol) then exit;
 lXYZmmMin := niftiVol.FracMM(Vec3(0,0,0));
 lXYZmmMax := niftiVol.FracMM(Vec3(1,1,1));
 isMNISpaceV := (lXYZmmMin.X < -70) and (lXYZmmMax.X > 70) and
       (lXYZmmMin.Y < -100) and (lXYZmmMax.Y > 70) and
       (lXYZmmMin.Z < -38) and (lXYZmmMax.Z > 76);
lR := MosRowEdit.value;
lC := MosColEdit.value;
lRxC := lR * lC;
if lRxC < 1 then
 exit;
if (lRxC > 1) and (MosCrossCheck.Checked) then
 lInterval := 1 / (lRxC) //with cross-check, final image will be 0.5
else
 lInterval := 1 / (lRxC+1);
lCi := MosOrientDrop.ItemIndex;
case lCi of
 1 : lStr := 'C';//coronal
 2 : lStr := 'S'; //Sag
 3 : lStr := 'Z'; //rev Sag
 else lStr := 'A'; //axial
end; //Case
lO := lCi;
case lCi of
 1 : lOrthoCh := 'S';//coronal
 2 : lOrthoCh := 'C'; //Sag
 3 : lOrthoCh := 'C'; //rev Sag
 else lOrthoCh := 'S'; //axial
end; //Case
lStr := lStr + ' ';
//next Labels...
if MosLabelCheck.checked then
 lStr := lStr + 'L+ ';
//next horizonatal overlap
if MosColOverlapTrack.Position <> 0 then
 lStr := lStr +'H '+ FloatToStrF(MosColOverlapTrack.Position/10, ffGeneral, 4, 3)+ ' ';
//next vertical overlap
if MosRowOverlapTrack.Position <> 0 then
 lStr := lStr +'V '+ FloatToStrF(MosRowOverlapTrack.Position/10, ffGeneral, 4, 3) + ' ';
//next draw rows....
lI := 0;
for lRi := 1 to lR do begin
 for lCi := 1 to lC do begin
   inc(lI);
   if (lI = lRxC) and (MosCrossCheck.Checked) then begin
     if isMNISpaceV then
         lStr := lStr +lOrthoCh + ' X R 0' //maybe "X" used to disable text on cross slice? perhaps "L-"
     else
         lStr := lStr +lOrthoCh + ' X R 0.5' //maybe "X" used to disable text on cross slice? perhaps "L-"
     //lStr := lStr + 'X '+lOrthoCh + ' 0.5'
   end else begin
     lFrac := lI * lInterval;
     if isMNISpaceV then begin
        case lO of
           1 : lmm := lerpFraction(lFrac, lXYZmmMin.Y,lXYZmmMax.Y);//coronal
           2,3 : lmm := lerpFraction(lFrac, lXYZmmMin.X,lXYZmmMax.X); //Sag
           else lmm := lerpFraction(lFrac, lXYZmmMin.Z,lXYZmmMax.Z); //axial
         end; //Case
          lStr := lStr + InttoStr(lmm);
          //lStr := lStr + FloatToStrF(lFrac, ffFixed, 8, 4);
     end else
         lStr := lStr + FloatToStrF(lFrac, ffGeneral, 8, 4);
   end;
   if lCi < lC then
     lStr := lStr + ' ';
 end; //for each column
 if lRi < lR then
   lStr := lStr +'; ';
end;//for each row
MosaicText.Text := lStr;
gPrefs.MosaicStr := lStr;
ViewGPU1.Invalidate;
end;

procedure TGLForm1.OnlineHelpMenuClick(Sender: TObject);
begin
    OpenURL('https://www.nitrc.org/plugins/mwiki/index.php/mricrogl:MainPage');
end;

procedure TGLForm1.OrientBtnClick(Sender: TObject);
var
 sliceMove: TVec3i;
 vFrac: TVec3;
 niftiVol: TNIfTI;
begin
 if not vols.Layer(0,niftiVol) then exit;
 sliceMove:= pti(0,0,0);
 Case (sender as TButton).tag of
     0: sliceMove.X := -1; //LEFT
     1: sliceMove.X := +1; //RIGHT
     2: sliceMove.Y := -1; //POSTERIOR
     3: sliceMove.Y := +1; //ANTERIOR
     4: sliceMove.Z := -1; //INFERIOR
     5: sliceMove.Z := +1; //SUPERIOR
 end;
 vFrac := niftiVol.FracShiftSlice(vol1.Slices.SliceFrac, sliceMove); //move a desired number of slices
 vol1.SetSlice2DFrac(vFrac);
 ViewGPU1.Invalidate;
end;

procedure TGLForm1.RemoveHazeMenuClick(Sender: TObject);
var
 niftiVol: TNIfTI;
begin
  if not vols.Layer(0,niftiVol) then exit;
  niftiVol.RemoveHaze;
  ViewGPU1.Invalidate;
end;

procedure TGLForm1.ScriptFormVisible(vis: boolean);
begin
  if (vis) and (GLForm1.ScriptPanel.Width < GLForm1.ToolPanel.Constraints.MaxWidth) then begin
     //GLForm1.ScriptPanel.Width := GLForm1.ToolPanel.Constraints.MaxWidth
     if Screen.PixelsPerInch > 70 then
       ScriptPanel.Width := ceil(2.8 * Screen.PixelsPerInch)
     else
         ScriptPanel.Width := 268;
  end else if (not vis) then
      GLForm1.ScriptPanel.width := 0;
end;

procedure TGLForm1.ScriptingNewMenuClick(Sender: TObject);
begin
  ScriptFormVisible(true);
  ScriptMemo.Lines.Clear;
  ScriptMemo.Lines.Add('import gl');
  ScriptMemo.Lines.Add('import sys');
  ScriptMemo.Lines.Add('print(sys.version)');
  ScriptMemo.Lines.Add('print(gl.version())');
  ScriptMemo.Lines.Add('gl.resetdefaults()');
end;

procedure TGLForm1.ClrbarClr(i: integer);
begin
 {$IFDEF CLRBAR}
 if (i < 1) or (i > 4) then i := 4;
 //gPrefs.ColorbarColor:= i;
 Case i of
      1: begin
        gClrbar.BackColor := (setRGBA(255,255,255,255));
        gClrbar.FontColor := (setRGBA(0,0,0,255));
        WhiteClrbarMenu.checked := true;
      end;
      2: begin
        gClrbar.BackColor := (setRGBA(255,255,255,168));
        gClrbar.FontColor := (setRGBA(0,0,0,255));
        TransWhiteClrbarMenu.checked := true;
      end;
      3: begin
        gClrbar.BackColor := (setRGBA(0,0,0,255));
        gClrbar.FontColor := (setRGBA(255,255,255,255));
        BlackClrbarMenu.checked := true;
      end;
      else begin
        gClrbar.BackColor := (setRGBA(0,0,0,168));
        gClrbar.FontColor := (setRGBA(255,255,255,255));
        TransBlackClrbarMenu.checked := true;
      end;
 end;
 {$ENDIF}
end;

procedure TGLForm1.InterpolateDrawMenuClick(Sender: TObject);
begin
  if (not vols.Drawing.IsOpen) or (vols.Drawing.voiIsEmpty) then exit; //nothing to do
  vols.Drawing.voiInterpolate ((sender as tMenuItem).tag);
  ViewGPU1.Invalidate;
end;

procedure TGLForm1.EnsureOpenVoi();
var
 niftiVol: TNIfTI;
begin
  if vols.Drawing.IsOpen then exit; //Drawing already opened
  if not vols.Layer(0,niftiVol) then exit;
  vols.Drawing.voiCreate(niftiVol.Dim.x, niftiVol.Dim.y, niftiVol.Dim.z, nil );
end;

procedure TGLForm1.DrawUndoMenuClick(Sender: TObject);
begin
 if not vols.Drawing.IsOpen then exit; //Drawing not open
 Vols.Drawing.voiUndo;
 ViewGPU1.Invalidate;
end;

procedure TGLForm1.DrawCloseMenuClick(Sender: TObject);
begin
  if (not vols.Drawing.IsOpen) then exit;
  if (vols.Drawing.voiIsEmpty) then begin
    vols.Drawing.voiClose;
    exit;
  end;
  if (vols.Drawing.NeedsSave) then
     DrawSaveMenuClick(nil);
  vols.Drawing.voiClose;
   ViewGPU1.Invalidate;
end;

procedure TGLForm1.DrawDescriptivesMenuClick(Sender: TObject);
var
   strs: TStringList;
   PrefForm: TForm;
   Memo: TMemo;
   niftiVol: TNIfTI;
begin
  if not Vols.Drawing.IsOpen then begin
     showmessage('Unable to generate descriptives: no open drawing');
     exit;
  end;
 if not vols.Layer(0,niftiVol) then exit;
 //if not niftiVol.IsLabels then exit;
  strs := niftiVol.VoiDescriptives(Vols.Drawing.VolRawBytes);
  Clipboard.AsText:= strs.Text;
  PrefForm := TForm.Create(nil);
  PrefForm.Caption := 'Descriptives for "'+niftiVol.ShortName+'" (Copied to clip board)' ;
  PrefForm.Width := Max(80, Screen.Width div 2);
  PrefForm.Height := Max(60, Screen.Height div 2);
  PrefForm.BorderStyle:= bsSizeable;
  PrefForm.FormStyle:= fsNormal;
  //PrefForm.AutoSize := True;
  PrefForm.BorderWidth := 4;
  PrefForm.Position := poScreenCenter;
  Memo:=TMemo.create(PrefForm);
  Memo.Align := alClient;
  Memo.ScrollBars:= ssAutoBoth;
  Memo.Lines.AddStrings(strs);
  strs.Free;
  Memo.Parent:=PrefForm;
  PrefForm.ShowModal;
  //Memo.Free;
  FreeAndNil(PrefForm);
end; //ShowDescriptives()

procedure TGLForm1.DrawSmoothMenuClick(Sender: TObject);
begin
     if not Vols.Drawing.IsOpen then exit;
     Vols.Drawing.voiSmoothIntensity;
     ViewGPU1.Invalidate;
end;

procedure TGLForm1.DrawBinarizeMenuClick(Sender: TObject);
begin
  if not Vols.Drawing.IsOpen then exit;
  Vols.Drawing.voiBinarize(1);
  ViewGPU1.Invalidate;
end;

procedure TGLForm1.DrawAutomaticMenuClick(Sender: TObject);
begin
 AutoROIForm.Show;
end;

procedure TGLForm1.DrawCloneMenuClick(Sender: TObject);
var
   niftiVol: TNIfTI;
begin
  if not vols.Layer(0,niftiVol) then exit;
  if (gPrefs.DisplayOrient = kRenderOrient) or (gPrefs.DisplayOrient = kMosaicOrient) then exit;
  if (not vols.Drawing.IsOpen)  then exit;
  Vols.Drawing.voiPasteSlice(vol1.Slices.SliceFrac.X, vol1.Slices.SliceFrac.Y, vol1.Slices.SliceFrac.Z);
  ViewGPU1.Invalidate;
end;

procedure TGLForm1.DrawSaveMenuClick(Sender: TObject);
var
   niftiVol: TNIfTI;
   fnm: string;
   dlg : TSaveDialog;
begin
 if not vols.Layer(0,niftiVol) then exit;
 if (not vols.Drawing.IsOpen)  or (vols.Drawing.voiIsEmpty) then begin
     showmessage('No drawing is open. Nothing to save.');
     exit;
  end;
  dlg := TSaveDialog.Create(self);
  dlg.Title := 'Save drawing';
  dlg.InitialDir := ExtractFileDir(niftiVol.Filename);//GetUserDir;
  dlg.Filter := 'Volume of interest|*.voi|NIfTI|*.nii|Compressed NIfTI|*.nii.gz';
  dlg.DefaultExt := '.voi';
  dlg.FilterIndex := 0;
  if not dlg.Execute then begin
     dlg.Free;
     exit;
  end;
  fnm := dlg.FileName;
  dlg.Free;
  niftiVol.SaveAsSourceOrient(fnm, vols.Drawing.VolRawBytes);
  vols.Drawing.NeedsSave := false;
end;

procedure TGLForm1.DrawOpenMenuClick(Sender: TObject);
begin
  if (vols.Drawing.IsOpen) and (not vols.Drawing.voiIsEmpty) and (vols.Drawing.NeedsSave) then
     DrawSaveMenuClick(nil);
  if not OpenDialog1.execute then
      OpenDialog1.filename := '';
   Vols.OpenDrawing(OpenDialog1.filename);
   ViewGPU1.Invalidate;
end;

procedure TGLForm1.DrawTool1Click(Sender: TObject);
var
 sliceMove: TVec3i;
 vFrac: TVec3;
 niftiVol: TNIfTI;
begin
 Vols.Drawing.ActivePenColor:= (sender as TMenuItem).tag;
 Vols.Drawing.PenColorOnRelease := (sender as TMenuItem).tag;
 if (sender as TMenuItem).tag < 0 then exit; //for pen make sure we are at slice center
 if not vols.Layer(0,niftiVol) then exit;
 sliceMove:= pti(0,0,0);
 vFrac := niftiVol.FracShiftSlice(vol1.Slices.SliceFrac, sliceMove); //move a desired number of slices
 vol1.SetSlice2DFrac(vFrac);
 ViewGPU1.Invalidate;
end;

procedure TGLForm1.DrawTransClick(Sender: TObject);
var
 i: integer;
begin
     i := (Sender as TMenuItem).Tag;
     if i <> 0 then
        Vols.Drawing.OpacityFraction := i/100
     else begin
        if (Sender as TMenuItem).Checked then
           Vols.Drawing.OpacityFraction := -abs(Vols.Drawing.OpacityFraction)
        else
            Vols.Drawing.OpacityFraction := abs(Vols.Drawing.OpacityFraction);
     end;
     UpdateTimer.Enabled := true;
end;

function GetFloat(prompt: string; min,def,max: double): double;
var
    PrefForm: TForm;
    CancelBtn,OkBtn: TButton;
    promptLabel: TLabel;
    valEdit: TEdit;
begin
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
  if (min < max) then
     promptLabel.Caption:= format('%s (range %0.3g..%0.3g)', [prompt, min, max]);
  //promptLabel.Left := 8;
  //promptLabel.Top := 12;
  promptLabel.AutoSize := true;
  promptLabel.AnchorSide[akTop].Side := asrTop;
  promptLabel.AnchorSide[akTop].Control := PrefForm;
  promptLabel.BorderSpacing.Top := 6;
  promptLabel.AnchorSide[akLeft].Side := asrLeft;
  promptLabel.AnchorSide[akLeft].Control := PrefForm;
  promptLabel.BorderSpacing.Left := 6;
  promptLabel.Parent:=PrefForm;
  //edit
  valEdit:=TEdit.create(PrefForm);
  valEdit.Caption := FloatToStrF(def, ffGeneral, 8, 4);
  //valEdit.Top := 42;
  //valEdit.Width := PrefForm.Width - 16;
  valEdit.Constraints.MinWidth:= 300;
  valEdit.AutoSize := true;
  valEdit.AnchorSide[akTop].Side := asrBottom;
  valEdit.AnchorSide[akTop].Control := promptLabel;
  valEdit.BorderSpacing.Top := 6;
  valEdit.AnchorSide[akLeft].Side := asrLeft;
  valEdit.AnchorSide[akLeft].Control := PrefForm;
  valEdit.BorderSpacing.Left := 6;
  valEdit.Parent:=PrefForm;
  //Cancel Btn
  CancelBtn:=TButton.create(PrefForm);
  CancelBtn.Caption:='Cancel';
  CancelBtn.AutoSize := true;
  CancelBtn.AnchorSide[akTop].Side := asrBottom;
  CancelBtn.AnchorSide[akTop].Control := valEdit;
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
  OkBtn.AnchorSide[akTop].Control := valEdit;
  OkBtn.BorderSpacing.Top := 6;
  OkBtn.AnchorSide[akLeft].Side := asrRight;
  OkBtn.AnchorSide[akLeft].Control := CancelBtn;
  OkBtn.BorderSpacing.Left := 6;
  OkBtn.Parent:=PrefForm;
  OkBtn.ModalResult:= mrOK;
  //OK button
  {$IFDEF LCLCocoa}
  if gPrefs.DarkMode then GLForm1.SetFormDarkMode(PrefForm);
  {$ENDIF}
  PrefForm.ShowModal;
  result := kNaN;
  if (PrefForm.ModalResult = mrOK) then begin
    result := StrToFloatDef(valEdit.Caption, def);
    if (min < max) and (result < min) then
      result := min;
    if (min < max) and (result > max) then
      result := max;
  end;
  FreeAndNil(PrefForm);
end; //GetFloat()

procedure TGLForm1.ExtractBrainMenuClick(Sender: TObject);
var
lFrac: double;
lB: string;
begin
lFrac := GetFloat('Brain extraction fraction',0.1,0.45,0.9);
if (lFrac= kNaN) then exit;
if not OpenDialog1.Execute then
  exit;
lB := FSLbet(OpenDialog1.FileName,lFrac);
AddBackground(lB);
end;

procedure TGLForm1.LayerAdditiveMenuClick(Sender: TObject);
begin
  Vols.AdditiveOverlayBlending := LayerAdditiveMenu.Checked;
  GLForm1.ForceOverlayUpdate();
  UpdateTimer.Enabled := true;
end;

procedure TGLForm1.LayerMaskWithBackgroundMenuClick(Sender: TObject);
begin
     Vols.MaskWithBackground:= LayerMaskWithBackgroundMenu.Checked;
     GLForm1.ForceOverlayUpdate();
     UpdateTimer.Enabled := true;
end;

procedure TGLForm1.LayerPopupPopup(Sender: TObject);
var
  i: integer;
  niftiVol: TNIfTI;
begin
  i := LayerList.ItemIndex;
  if (i < 0) or (i >= LayerList.Count) then exit;
  LayerFromZeroMenu.Enabled := (i > 0);
  LayerCloseMenu.Enabled:= (i > 0);
  LayerUpMenu.Enabled := (i > 1);
  LayerDownMenu.Enabled := (i > 0) and (i < (LayerList.Count-1));
  if not vols.Layer(LayerList.ItemIndex,niftiVol) then exit;
  LayerCutoutMenu.Checked := niftiVol.HiddenByCutout;
  LayerPrevVolumeMenu.Enabled := (niftiVol.Header.dim[4] > 1);
  LayerNextVolumeMenu.Enabled := LayerPrevVolumeMenu.Enabled;
  LayerShowBidsMenu.Enabled := (niftiVol.BidsName <> '');
  if (i = 0) then
     exit;
  LayerFromZeroMenu.Checked := niftiVol.CX.FromZero;
end;

procedure TGLForm1.LayerFromZeroMenuClick(Sender: TObject);
var
    i: integer;
    niftiVol: TNIfTI;
begin
  i := LayerList.ItemIndex;
  if (i < 0) or (i >= LayerList.Count) then exit;
  if not vols.Layer(LayerList.ItemIndex,niftiVol) then exit;
  LayerColorFromZero(i, not niftiVol.CX.FromZero);
end;

procedure TGLForm1.Quit2TextEditor;
{$IFDEF UNIX}
var
  AProcess: TProcess;
  {$IFDEF LINUX} I: integer; EditorFName : string; {$ENDIF}
begin
    {$IFDEF LINUX}
    EditorFName := FindDefaultExecutablePath('gedit');
   if EditorFName = '' then
     EditorFName := FindDefaultExecutablePath('tea');
    if EditorFName = '' then
      EditorFName := FindDefaultExecutablePath('nano');
    if EditorFName = '' then
      EditorFName := FindDefaultExecutablePath('pico');
    if EditorFName = '' then begin
       Showmessage(ExtractFilename(paramstr(0))+' will now quit. You can then use a text editor to modify the file '+IniName);
       Clipboard.AsText := EditorFName;
    end else begin
      EditorFName := '"'+EditorFName +'" "'+IniName+'"';
      Showmessage(ExtractFilename(paramstr(0))+' will now quit. Modify the settings with the command "'+EditorFName+'"');
         AProcess := TProcess.Create(nil);
         AProcess.InheritHandles := False;
         AProcess.Options := [poNewProcessGroup, poNewConsole];
         AProcess.ShowWindow := swoShow;
        for I := 1 to GetEnvironmentVariableCount do
            AProcess.Environment.Add(GetEnvironmentString(I));
         AProcess.Executable := EditorFName;
         AProcess.Execute;
         AProcess.Free;
    end;
    Clipboard.AsText := EditorFName;
    GLForm1.close;
    exit;
    {$ENDIF}
    Showmessage('Preferences will be opened in a text editor. The program '+ExtractFilename(paramstr(0))+' will now quit, so that the file will not be overwritten.');
    AProcess := TProcess.Create(nil);
    {$IFDEF UNIX}
      //AProcess.CommandLine := 'open -a TextEdit '+IniName;
      AProcess.Executable := 'open';
      AProcess.Parameters.Add('-e');
      AProcess.Parameters.Add(IniName);
    {$ELSE}
      AProcess.CommandLine := 'notepad '+IniName;
    {$ENDIF}
   Clipboard.AsText := AProcess.CommandLine;
  //AProcess.Options := AProcess.Options + [poWaitOnExit];
  AProcess.Execute;
  AProcess.Free;
  GLForm1.close;
end;
{$ELSE} //ShellExecute(Handle,'open', 'c:\windows\notepad.exe','c:\SomeText.txt', nil, SW_SHOWNORMAL) ;
begin
  gPrefs.SkipPrefWriting := true;
    Showmessage('Preferences will be opened in a text editor. The program '+ExtractFilename(paramstr(0))+' will now quit, so that the file will not be overwritten.');
   //GLForm1.SavePrefs;
    ShellExecute(Handle,'open', 'notepad.exe',PAnsiChar(AnsiString(IniName)), nil, SW_SHOWNORMAL) ;
  //WritePrefsOnQuit.checked := false;
  GLForm1.close;
end;
{$ENDIF}

procedure TGLForm1.HelpPrefMenuClick(Sender: TObject);
var
  PrefForm: TForm;
  bmpEdit: TEdit;
  LoadFewVolumesCheck, LandMarkCheck,
  {$IFDEF LCLCocoa} DarkModeCheck, RetinaCheck,{$ENDIF} RadiologicalCheck: TCheckBox;
  OkBtn, dcm2niixBtn, AdvancedBtn: TButton;
  bmpLabel: TLabel;
  WindowCombo : TComboBox;
  isDcm2niix, isAdvancedPrefs  {$IFDEF LCLCocoa}, isDarkModeChanged, isRetinaChanged {$ENDIF}: boolean;
begin
  PrefForm:=TForm.Create(GLForm1);
  PrefForm.AutoSize := true;
  PrefForm.BorderWidth := 8;
  PrefForm.Caption:='Preferences';
  PrefForm.Position := poScreenCenter;
  PrefForm.BorderStyle := bsDialog;
  //Startup window size
  WindowCombo:=TComboBox.create(PrefForm);
  WindowCombo.Parent:=PrefForm;
  WindowCombo.AutoSize:= true;
  WindowCombo.Width := 320;
  WindowCombo.Items.Add('Startup: As Window');
  WindowCombo.Items.Add('Startup: Maximized');
  WindowCombo.Items.Add('Startup: Full Screen');
  WindowCombo.ItemIndex:=  gPrefs.StartupWindowMode;
  WindowCombo.Style := csDropDownList;
  WindowCombo.AnchorSide[akTop].Side := asrTop;
  WindowCombo.AnchorSide[akTop].Control := PrefForm;
  WindowCombo.BorderSpacing.Top := 6;
  WindowCombo.AnchorSide[akLeft].Side := asrLeft;
  WindowCombo.AnchorSide[akLeft].Control := PrefForm;
  WindowCombo.BorderSpacing.Left := 6;
  WindowCombo.Anchors := [akTop, akLeft];
  WindowCombo.Parent:=PrefForm;
  //Bitmap Scale
  bmpLabel:=TLabel.create(PrefForm);
  bmpLabel.Width := PrefForm.Width - 86;
  bmpLabel.Caption := 'Bitmap zoom (large values create huge images)';
  bmpLabel.AnchorSide[akTop].Side := asrBottom;
  bmpLabel.AnchorSide[akTop].Control := WindowCombo;
  bmpLabel.BorderSpacing.Top := 6;
  bmpLabel.AnchorSide[akLeft].Side := asrLeft;
  bmpLabel.AnchorSide[akLeft].Control := PrefForm;
  bmpLabel.BorderSpacing.Left := 6;
  bmpLabel.Anchors := [akTop, akLeft];
  bmpLabel.Parent:=PrefForm;
  //bmp edit
  bmpEdit := TEdit.Create(PrefForm);
  bmpEdit.Width := 60;
  bmpEdit.AutoSize := true;
  bmpEdit.Text := inttostr(gPrefs.BitmapZoom);
  bmpEdit.AnchorSide[akTop].Side := asrCenter;
  bmpEdit.AnchorSide[akTop].Control := bmpLabel;
  bmpEdit.BorderSpacing.Top := 6;
  bmpEdit.AnchorSide[akLeft].Side := asrRight;
  bmpEdit.AnchorSide[akLeft].Control := bmpLabel;
  bmpEdit.BorderSpacing.Left := 8;
  bmpEdit.Anchors := [akTop, akLeft];
  bmpEdit.Parent:=PrefForm;
  //  RadiologicalCheck
  RadiologicalCheck:=TCheckBox.create(PrefForm);
  RadiologicalCheck.Checked := gPrefs.FlipLR_Radiological;
  RadiologicalCheck.Caption:='Radiological convention (left on right)';
  RadiologicalCheck.AnchorSide[akTop].Side := asrBottom;
  RadiologicalCheck.AnchorSide[akTop].Control := bmpLabel;
  RadiologicalCheck.BorderSpacing.Top := 6;
  RadiologicalCheck.AnchorSide[akLeft].Side := asrLeft;
  RadiologicalCheck.AnchorSide[akLeft].Control := PrefForm;
  RadiologicalCheck.BorderSpacing.Left := 6;
  RadiologicalCheck.Anchors := [akTop, akLeft];
  RadiologicalCheck.Parent:=PrefForm;
  //LoadFewVolumes
  LoadFewVolumesCheck:=TCheckBox.create(PrefForm);
  LoadFewVolumesCheck.Checked := gPrefs.LoadFewVolumes;
  LoadFewVolumesCheck.Caption:='Only initial volumes (load 4D series faster)';
  //LoadFewVolumesCheck.Top := 78;
  LoadFewVolumesCheck.AnchorSide[akTop].Side := asrBottom;
  LoadFewVolumesCheck.AnchorSide[akTop].Control := RadiologicalCheck;
  LoadFewVolumesCheck.BorderSpacing.Top := 6;
  LoadFewVolumesCheck.AnchorSide[akLeft].Side := asrLeft;
  LoadFewVolumesCheck.AnchorSide[akLeft].Control := PrefForm;
  LoadFewVolumesCheck.BorderSpacing.Left := 6;
  LoadFewVolumesCheck.Anchors := [akTop, akLeft];
  LoadFewVolumesCheck.Parent:=PrefForm;
  //LandmarkCheck
  LandmarkCheck:=TCheckBox.create(PrefForm);
  LandmarkCheck.Checked := gPrefs.LandmarkPanel;
  LandmarkCheck.Caption:='Show landmark tools';
  LandmarkCheck.AnchorSide[akTop].Side := asrBottom;
  LandmarkCheck.AnchorSide[akTop].Control := LoadFewVolumesCheck;
  LandmarkCheck.BorderSpacing.Top := 6;
  LandmarkCheck.AnchorSide[akLeft].Side := asrLeft;
  LandmarkCheck.AnchorSide[akLeft].Control := PrefForm;
  LandmarkCheck.BorderSpacing.Left := 6;
  LandmarkCheck.Anchors := [akTop, akLeft];
  LandmarkCheck.Parent:=PrefForm;
  {$IFDEF LCLCocoa}
  //Retina Check
  RetinaCheck:=TCheckBox.create(PrefForm);
  RetinaCheck.Checked := gPrefs.RetinaDisplay;
  RetinaCheck.Caption:='Retina display (better but slower)';
  RetinaCheck.AnchorSide[akTop].Side := asrBottom;
  RetinaCheck.AnchorSide[akTop].Control := LandmarkCheck;
  RetinaCheck.BorderSpacing.Top := 6;
  RetinaCheck.AnchorSide[akLeft].Side := asrLeft;
  RetinaCheck.AnchorSide[akLeft].Control := PrefForm;
  RetinaCheck.BorderSpacing.Left := 6;
  RetinaCheck.Anchors := [akTop, akLeft];
  RetinaCheck.Parent:=PrefForm;
  {$IFDEF METALAPI}RetinaCheck.visible := false;{$ENDIF}
  //DarkMode
  DarkModeCheck:=TCheckBox.create(PrefForm);
  DarkModeCheck.visible := isDarkModeSupported;
  DarkModeCheck.Checked := gPrefs.DarkMode;
  DarkModeCheck.Caption:='Dark Mode';
  DarkModeCheck.AnchorSide[akTop].Side := asrBottom;
  DarkModeCheck.AnchorSide[akTop].Control := RetinaCheck;
  DarkModeCheck.BorderSpacing.Top := 6;
  DarkModeCheck.AnchorSide[akLeft].Side := asrLeft;
  DarkModeCheck.AnchorSide[akLeft].Control := PrefForm;
  DarkModeCheck.BorderSpacing.Left := 6;
  DarkModeCheck.Anchors := [akTop, akLeft];
  DarkModeCheck.Parent:=PrefForm;
  GLForm1.SetFormDarkMode(PrefForm);
  {$ENDIF}
  //OK button
  OkBtn:=TButton.create(PrefForm);
  OkBtn.Caption:='OK';
  OkBtn.Width:= 100;
  OkBtn.AutoSize := true;
  OkBtn.AnchorSide[akTop].Side := asrBottom;
  {$IFDEF LCLCocoa}
  OkBtn.AnchorSide[akTop].Control := DarkModeCheck;
  {$ELSE}
  OkBtn.AnchorSide[akTop].Control := LandmarkCheck;
  {$ENDIF}
  OkBtn.BorderSpacing.Top := 6;
  OkBtn.AnchorSide[akLeft].Side := asrRight;
  OkBtn.AnchorSide[akLeft].Control := bmpLabel;
  OkBtn.BorderSpacing.Left := 6;
  OkBtn.Anchors := [akTop, akLeft];
  OkBtn.Parent:=PrefForm;
  OkBtn.ModalResult:= mrOK;
  //dcm2niix button
  dcm2niixBtn :=TButton.create(PrefForm);
  dcm2niixBtn.Caption:='dcm2niix path';
  dcm2niixBtn.Hint := 'Specify the location of the DICOM importer (allows you to select custom version)';
  dcm2niixBtn.ShowHint := true;
  dcm2niixBtn.Width:= 100;
  dcm2niixBtn.AutoSize := true;
  dcm2niixBtn.AnchorSide[akTop].Side := asrCenter;
  dcm2niixBtn.AnchorSide[akTop].Control := OkBtn;
  dcm2niixBtn.BorderSpacing.Top := 6;
  dcm2niixBtn.AnchorSide[akLeft].Side := asrCenter;
  dcm2niixBtn.AnchorSide[akLeft].Control := PrefForm;
  dcm2niixBtn.BorderSpacing.Left := 0;
  dcm2niixBtn.Anchors := [akTop, akLeft];
  dcm2niixBtn.Parent:=PrefForm;
  dcm2niixBtn.ModalResult:= mrCancel;
  //Advanced button
  AdvancedBtn:=TButton.create(PrefForm);
  AdvancedBtn.Caption:='Advanced';
  AdvancedBtn.Width:= 100;
  AdvancedBtn.AutoSize := true;
  //AdvancedBtn.Left := PrefForm.Width - 256;
  //AdvancedBtn.Top := 198;
  AdvancedBtn.AnchorSide[akTop].Side := asrCenter;
  AdvancedBtn.AnchorSide[akTop].Control := OkBtn;
  AdvancedBtn.BorderSpacing.Top := 6;
  AdvancedBtn.AnchorSide[akLeft].Side := asrLeft;
  AdvancedBtn.AnchorSide[akLeft].Control := PrefForm;
  AdvancedBtn.BorderSpacing.Left := 6;
  AdvancedBtn.Anchors := [akTop, akLeft];
  AdvancedBtn.Parent:=PrefForm;
  AdvancedBtn.ModalResult:= mrYesToAll;
  PrefForm.ShowModal;
  if (PrefForm.ModalResult <> mrCancel) and (PrefForm.ModalResult <> mrOK) and (PrefForm.ModalResult <> mrYesToAll) then begin
  	FreeAndNil(PrefForm);
  	exit; //if user closes window with out pressing "OK"
  end;
  gPrefs.FlipLR_Radiological := RadiologicalCheck.Checked;
  Vol1.Slices.RadiologicalConvention := gPrefs.FlipLR_Radiological;
  gPrefs.BitmapZoom:= strtointdef(bmpEdit.Text,1);
  gPrefs.LoadFewVolumes := LoadFewVolumesCheck.Checked;
  gPrefs.StartupWindowMode := WindowCombo.ItemIndex;
  vols.LoadFewVolumes  := gPrefs.LoadFewVolumes;
  if gPrefs.BitmapZoom < 1 then gPrefs.BitmapZoom := 1;
  if gPrefs.BitmapZoom > 10 then gPrefs.BitmapZoom := 10;
  isAdvancedPrefs := (PrefForm.ModalResult = mrYesToAll);
  isDcm2niix := (PrefForm.ModalResult = mrCancel);
  if (gPrefs.LandmarkPanel <> LandmarkCheck.Checked) then begin
     gPrefs.LandmarkPanel := LandmarkCheck.Checked;
     UpdateVisibleBoxes();
  end;
  {$IFDEF LCLCocoa}
  isRetinaChanged := gPrefs.RetinaDisplay <> RetinaCheck.Checked;
  gPrefs.RetinaDisplay := RetinaCheck.Checked;
  isDarkModeChanged := gPrefs.DarkMode <> DarkModeCheck.Checked;
  gPrefs.DarkMode := DarkModeCheck.Checked;
  {$ENDIF}
  FreeAndNil(PrefForm);
  if  isAdvancedPrefs then begin
    GLForm1.Refresh;
    GLForm1.Quit2TextEditor;
    exit;
  end;
  {$IFDEF LCLCocoa}
  if isDarkModeChanged then
       GLForm1.SetDarkMode();
  if isRetinaChanged then begin
     GLForm1.SetRetina;
  end;
  {$ENDIF}
  ViewGPU1.Invalidate;
  if (isDcm2niix) then
     dcm2niiForm.findCustomDcm2niix;
end; // PrefMenuClick()

procedure TGLForm1.LayerColorFromZero(Layer: integer; IsFromZero: boolean);
var
    niftiVol: TNIfTI;
begin
   if not vols.Layer(LayerList.ItemIndex,niftiVol) then exit;
   niftiVol.CX.FromZero := IsFromZero;
   niftiVol.ForceUpdate;
   UpdateTimer.Enabled := true;
end;

procedure TGLForm1.LayerMoveUpOrDown(layer: integer; isMoveDown:boolean);
var
   swapLayer: integer;
begin
     if (isMoveDown) then
        swapLayer := layer + 1
     else
         swapLayer := layer -1;
     if not vols.SwapLayers(layer, swapLayer) then exit;
     UpdateLayerBox(true);
     UpdateTimer.Enabled := true;
end;

procedure TGLForm1.LayerResetBrightnessMenuClick(Sender: TObject);
var
   niftiVol: TNIfTI;
begin
  if not vols.Layer(LayerList.ItemIndex,niftiVol) then exit;
  GLForm1.LayerChange(LayerList.ItemIndex, -1, -1, niftiVol.SuggestedDisplayMin, niftiVol.SuggestedDisplayMax); //kNaNsingle

end;

procedure TGLForm1.LayerUpDownClick(Sender: TObject);
begin
  LayerMoveUpOrDown(LayerList.ItemIndex, (sender as TMenuItem).tag = 1);
end;

procedure TGLForm1.NewWindowMenuClick(Sender: TObject);
{$IFNDEF UNIX}
begin
   ShellExecute(handle,'open',PChar(paramstr(0)), '','',SW_SHOWNORMAL); //uses ShellApi;
end;
{$ELSE}
var
    AProcess: TProcess;
    i : integer;
    //http://wiki.freepascal.org/Executing_External_Programs
begin
  gPrefs.CustomDcm2niix := dcm2niiForm.getCustomDcm2niix();
  IniFile(false, gPrefs);   //load new window with latest settings
  AProcess := TProcess.Create(nil);
  AProcess.InheritHandles := False;
  //AProcess.Options := [poNoConsole];  //poNoConsole is Windows only! http://lazarus-ccr.sourceforge.net/docs/fcl/process/tprocess.options.html
  //AProcess.ShowWindow := swoShow; //Windows only http://www.freepascal.org/docs-html/fcl/process/tprocess.showwindow.html
  for I := 1 to GetEnvironmentVariableCount do
      AProcess.Environment.Add(GetEnvironmentString(I));
  {$IFDEF Darwin}
  AProcess.Executable := 'open';
  AProcess.Parameters.Add('-n');
  AProcess.Parameters.Add('-a');
  AProcess.Parameters.Add(paramstr(0));
  {$ELSE}
  AProcess.Executable := paramstr(0);
  {$ENDIF}
  //AProcess.Parameters.Add('/Users/rorden/Documents/osx/MRIcroGL.app/Contents/MacOS/MRIcroGL');
  AProcess.Execute;
  AProcess.Free;
end;
{$ENDIF}

procedure TGLForm1.ScriptingPyVersionClick(Sender: TObject);
(*const
  {$IFDEF Darwin} kPyLib = 'libpython2.7.dylib'; {$ENDIF}
  {$IFDEF Linux} kPyLib = 'libpython3.6.so'; {$ENDIF}
  {$IFDEF Windows} kPyLib = 'python35.dll'; {$ENDIF}
var
  openDialog : TOpenDialog;
begin
  {$IFDEF MYPY}
  if not fileexists(gPrefs.PyLib) then
     gPrefs.PyLib := findPythonLib(gPrefs.PyLib);
  if fileexists(gPrefs.PyLib) then begin
     if messagedlg('Select a new library (e.g. '+kPyLib+') to replace '+gPrefs.PyLib+'?',mtConfirmation, mbOKCancel, 0) = mrCancel then exit;
  end else
      showmessage('Select a Python library (e.g. '+kPyLib+')');
  openDialog := TOpenDialog.Create(self);
  openDialog.InitialDir := GetCurrentDir;
  openDialog.Options := [ofFileMustExist];
  //{$IFDEF Darwin} openDialog.Filter := 'Python library|*.dylib|All files|*.*'; {$ENDIF}
  //{$IFDEF Linux} openDialog.Filter := 'Python library|*.so|All files|*.*'; {$ENDIF}
  //{$IFDEF Windows} openDialog.Filter := 'Python library|*.dll|All files|*.*'; {$ENDIF}
  if not OpenDialog.execute then exit;
  if not fileexists(openDialog.FileName) then exit;
  gPrefs.Pylib := openDialog.FileName;
*)
begin
 {$IFDEF MYPY}
 showmessage('not enabled');
  {$ELSE}
  showmessage('Recompile with scripting enabled');
  {$ENDIF}
end;

procedure TGLForm1.ScriptMemoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  ScriptBox.Caption := format('Scripting [Line %d Col %d]', [ScriptMemo.CaretPos.Y+1, ScriptMemo.CaretPos.X+1]);
end;

procedure TGLForm1.ScriptPanelDblClick(Sender: TObject);
begin
  ScriptPanel.Width := 4;
end;

procedure TGLForm1.ClrbarMenuClick(Sender: TObject);
begin
 ClrbarClr((sender as TMenuItem).Tag);
 ViewGPU1.Invalidate;
end;

procedure TGLForm1.SetColorBarPosition;
begin
  {$IFDEF CLRBAR}
  if (gPrefs.ColorBarPosition < 1) or (gPrefs.ColorBarPosition > 2) then gPrefs.ColorBarPosition := 1;
  case gPrefs.ColorBarPosition of  //1=top, 2=right
      1: begin gClrbar.isTopOrRight := true; gClrbar.isVertical:=false; end; //top row
      2: begin gClrbar.isTopOrRight := true; gClrbar.isVertical:=true; end; //right column
      //1: begin gClrbar.isTopOrRight := false; gClrbar.isVertical:=false; end;//bottom row
      //2: begin gClrbar.isTopOrRight := false; gClrbar.isVertical:=true; end;//left column
  end;
  {$ENDIF}
   //gClrbar.isTopOrRight := true; gClrbar.isVertical:=false;
end;

procedure TGLForm1.SmoothMenuClick(Sender: TObject);
var
    niftiVol: TNIfTI;
begin
   if not vols.Layer(0,niftiVol) then exit;
   niftiVol.Smooth;
   ViewGPU1.Invalidate;
end;

procedure TGLForm1.StoreFMRIMenuClick(Sender: TObject);
{$IFDEF MATT1}
var
  fnm: string;
  txt : TextFile;
begin
  fnm := gPrefs.PrevBackgroundImage;
  fnm := changefileext(fnm,'.txt');
  if fileexists(fnm) then begin
     showmessage('Already a file named '+fnm);
     exit;
  end;
  AssignFile (txt,fnm);
  Rewrite(txt); {open the file 'fname' for writing}
  Writeln(txt, GLForm1.caption);
  CloseFile(txt);
end;
{$ELSE}
begin
     //
end;
{$ENDIF}

procedure TGLForm1.TextAndCubeMenuClick(Sender: TObject);
begin
   gPrefs.LabelOrient := TextAndCubeMenu.Checked;
   Vol1.Slices.LabelOrient := gPrefs.LabelOrient;
   ViewGPU1.Invalidate;
end;

procedure TGLForm1.ToolPanelDblClick(Sender: TObject);
begin
  ToolPanel.Width := 4;
end;

procedure TGLForm1.SetXHairPosition (lX,lY,lZ: single);
var
   vFrac: TVec3;
   niftiVol: TNIfTI;
begin
     if not vols.Layer(0,niftiVol) then exit;
     vFrac := niftiVol.MMFrac(Vec3(lX,lY,lZ));
     vol1.SetSlice2DFrac(vFrac);
     ViewGPU1.Invalidate;
end;

procedure TGLForm1.CoordEditChange(Sender: TObject);
begin
  SetXHairPosition(StrToFloatDef(XCoordEdit.Text,0),StrToFloatDef(YCoordEdit.Text,0),StrToFloatDef(ZCoordEdit.Text,0) );
end;

procedure TGLForm1.ClrbarVisibleClick(Sender: TObject);
begin
  {$IFDEF CLRBAR}
  gClrbar.isVisible := VisibleClrbarMenu.checked;
  gPrefs.ColorbarVisible := gClrbar.isVisible;
  ViewGPU1.Invalidate;
  {$ENDIF}
end;

procedure TGLForm1.CutNearBtnClick(Sender: TObject);
var
   Azimuth, Elevation: integer;
begin
  Azimuth := Vol1.Azimuth;
  Azimuth := Azimuth mod 360;
  Elevation := Vol1.Elevation;
  XTrackBar.Position := 500;
  YTrackBar.Position := 500;
  ZTrackBar.Position := 500;
  if Elevation < 0 then
     Z2TrackBar.Position := 0
  else
      Z2TrackBar.Position := 1000;
  if (Azimuth < 90) or (Azimuth > 270) then
     Y2TrackBar.Position := 0
  else
      Y2TrackBar.Position := 1000;
  if (Azimuth < 180) then
     X2TrackBar.Position := 0
  else
      X2TrackBar.Position := 1000;
 CutoutChange(nil);
end;

procedure TGLForm1.CutNoneBtnClick(Sender: TObject);
begin
   XTrackBar.Position := 0;
   X2TrackBar.Position := 0;
   if sender <> nil then CutoutChange(Sender);
end;

procedure TGLForm1.ScriptingSaveMenuClick(Sender: TObject);
begin
   if not ScriptSaveDialog.Execute then exit;
     ScriptSaveDialog.Filename := ChangeFileExt(ScriptSaveDialog.Filename,'py');
     ScriptMemo.Lines.SaveToFile(ScriptSaveDialog.Filename);
end;

procedure TGLForm1.ScriptingRunMenuClick(Sender: TObject);
begin
 {$IFDEF MYPY}
 if gPyRunning then begin
   //PyEngine.PyErr_SetString(PyExc_KeyboardInterrupt^, 'Terminated');
   //PyEngine.PyExc_KeyboardInterrupt();
   ScriptOutputMemo.Lines.Add('Unable to run script: a script is already running.');
   //PyEngine.PyErr_SetString(@PyEngine.PyExc_KeyboardInterrupt^, 'Terminated');
   exit;
 end;
 if PyExec() then exit;
 if (not (AnsiContainsText(ScriptMemo.Text, 'import gl'))) then begin
     ScriptOutputMemo.Lines.Clear;
     ScriptOutputMemo.Lines.Add('Error: script must contain "import gl" (for Python) or "begin" (for Pascal).');
     exit;
 end;
 {$ELSE}
 Showmessage('Scripting not yet implemented');
 {$ENDIF}
end;



function TGLForm1.AddLayer(Filename: string): boolean;
//add a new layer on top of existing layers
var
 fnm: string;
begin
 AnimateTimer.Enabled := false;
 DisplayAnimateMenu.Checked := false;
  result := true;
  fnm := Filename;
  fnm := GetFullPath(fnm);
  if not Fileexists(fnm) then exit(false);
  if (vols.NumLayers >= kMaxOverlay) then begin
    showmessage('Unable to add another layer: please close a few layers');
    exit(false);
  end;
  result := vols.AddLayer(fnm, gPrefs.ClearColor);
  if (vols.NumLayers > 1) then
    GLForm1.LayerChange(vols.NumLayers-1, vols.NumLayers-1, -1, kNaNsingle, kNaNsingle); //kNaNsingle
  UpdateLayerBox(true);
  UpdateColorbar();
  //Vol1.UpdateOverlays(vols);
  UpdateTimer.Enabled:= true;
  //ViewGPU1.Invalidate;
end;

function TGLForm1.AddBackground(Filename: string; isAddToRecent: boolean = true): boolean;
//close all open layers and add a new background layer
var
   {$IFDEF MATT1}ext: string; {$ENDIF}
 fnm: string;
begin
  AnimateTimer.Enabled := false;
  DisplayAnimateMenu.Checked := false;
  fnm := Filename;
  fnm := GetFullPath(fnm);
  if not Fileexists(fnm) then exit(false);
  vols.CloseAllLayers;
  CutNoneBtnClick(nil); //turn off cut-out: not persistent like clip plane
  {$IFDEF MATT1}
  ext := uppercase(extractfileext(fnm));
  if (ext = '.MAT') then
     MatLoadForce(kMatForceT1);
  {$ENDIF}
  if not AddLayer(fnm) then
    exit(false);
  gPrefs.PrevBackgroundImage := fnm; //launch with last image, even if it is template/fsl image
  if (isAddToRecent) then AddOpenRecent(fnm);
  {$IFDEF MATT1}
  if (ext = '.MAT') then begin
     MatLoadForce(kMatForcefMRI);
     AddLayer(fnm);
  end;
  {$ENDIF}
  exit(true);
end;

function isNifti(fnm: string): boolean;
var
 lExt: string;
begin
     result := true;
     lExt := uppercase(extractfileext(fnm));
     if (lExt = '.NII') or (lExt = '.HDR') or (lExt = '.VOI') then exit;
     if (lExt = '.GZ') then begin
        lExt := uppercase(extractfileext(changefileext(fnm,'')));
        if (lExt = '.NII') then exit;
     end;
     result := false;
end;

function isDICOM(fnm: string): boolean;
var
   f: file;
   sz: integer;
   magic: array [0..3] of char; //signature of DICOM = 'DICM'
begin
     if (isNifti(fnm)) then
        exit(false);
     result := true;
     if DirectoryExists(fnm) then exit;
     AssignFile(f, fnm);
     FileMode := fmOpenRead;
     Reset(f,1);
     sz := FileSize(f);
     if sz < 256 then begin
        CloseFile(f);
        exit(false);
     end;
     Seek(f, 128);
     magic[0] := 'x'; //just to hide compiler warning
     blockread(f, magic[0],  sizeof(magic));
     //showmessage(magic); //will report DICM for DICOM images, but not DICOM meta objects
     if (magic[0] <> 'D') or (magic[1] <> 'I') or (magic[2] <> 'C') or (magic[3] <> 'M') then
        result := false;
end;

procedure TGLForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
 ss: TShiftState;
 fnm: string;
begin
  if (gPrefs.InitScript <> '') then exit; //MacOS gets paramstr as FormDropFiles, but deletes those that begin with '-', e.g. '-std'
  if length(FileNames) < 1 then exit;
  if (dcm2niiForm.visible) and ((dcm2niiForm.Active) or (dcm2niiForm.Focused)) then begin
     dcm2niiForm.FormDropFiles(Sender, FileNames);
     exit;
  end;
  fnm := GetFullPath(Filenames[0]);
  if (not fileexists(fnm)) and (not DirectoryExists(fnm)) then exit;
  if (isDICOM(fnm)) then begin
  //if (not isNifti(Filenames[0])) then begin
     fnm := dcm2Nifti(dcm2niiForm.getExeName, fnm);
     if fnm = '' then exit;
     AddBackground(fnm, false);
     if fnm <> Filenames[0] then
        deletefile(fnm);
     exit;
  end;
  if (DirectoryExists(fnm)) then exit;
  ss := getKeyshiftstate;
  //if (ssModifier in ss) then begin
  if (ssMeta in ss) or (ssCtrl in ss) then
     AddLayer(fnm)
  else
      AddBackground(fnm);
end;

procedure TGLForm1.OpenMenuClick(Sender: TObject);
begin
   if not OpenDialog1.execute then
      OpenDialog1.filename := '';
   AddBackground(OpenDialog1.filename);
end;

procedure TGLForm1.AddOverlayMenuClick(Sender: TObject);
begin
   if not OpenDialog1.execute then
      exit;
   AddLayer(OpenDialog1.filename);
end;

procedure TGLForm1.ScriptingOpenMenuClick(Sender: TObject);
begin
  if not ScriptOpenDialog.execute then exit;
  OpenScript(ScriptOpenDialog.Filename);
end;

procedure TGLForm1.setShaderSliders;
function Val2Percent (min,val,max: single): integer;
var
  S: single;
begin
  if max = min then
    S := 0
  else if max < min then
    S := 100* ((val-max)/(min-max))
  else
    S := 100* ((val-min)/(max-min));
  if S < 0 then
    S := 0;
  if S > 100 then
    S := 100;
  result := round(S);
end;
 var
  i, t: integer;
 begin
  //ShaderSliders
    for i := 0 to GLForm1.ShaderBox.ControlCount - 1 do begin
       t := GLForm1.ShaderBox.Controls[i].tag;
       if (t < 1) or (t > kMaxUniform) then continue;
       if (GLForm1.ShaderBox.Controls[i] is TLabel) then begin
          (GLForm1.ShaderBox.Controls[i] as TLabel).visible := t <= Vol1.ShaderSliders.nUniform;
          (GLForm1.ShaderBox.Controls[i] as TLabel).caption := Vol1.ShaderSliders.Uniform[t].Name;
          (GLForm1.ShaderBox.Controls[i] as TLabel).Hint := Vol1.ShaderSliders.Uniform[t].Hint;
       end;
       if (GLForm1.ShaderBox.Controls[i] is TTrackBar) then begin
          (GLForm1.ShaderBox.Controls[i] as TTrackBar).visible := t <= Vol1.ShaderSliders.nUniform;
          (GLForm1.ShaderBox.Controls[i] as TTrackBar).position := Val2Percent(Vol1.ShaderSliders.Uniform[t].min, Vol1.ShaderSliders.Uniform[t].DefaultV, Vol1.ShaderSliders.Uniform[t].max);
       end;
   end;
end;

procedure TGLForm1.ViewGPUDblClick(Sender: TObject);
var
niftiVol: TNIfTI;
begin
	if not vols.Layer(0,niftiVol) then exit;
	if Vol1.CE.ColorEditorDblClick(niftiVol) then exit;
	if true then begin//(not (gPrefs.ColorEditor)) or (not InColorBox(abs(MousePt.X),abs(MousePt.Y))) then begin
		if not gPrefs.ColorbarVisible then
		exit;
		gPrefs.ColorBarPosition := gPrefs.ColorBarPosition + 1;
		SetColorbarPosition;
		ViewGPU1.invalidate;
		exit;
	end;
end;

procedure TGLForm1.ResetDefaultsClick(Sender: TObject);
var
 ss: TShiftState;
begin
  //to do
  GLForm1.RenderMenu.Checked := true;
  //gPrefs.LabelOrient := TextAndCubeMenu.Checked;
  //Vol1.Slices.LabelOrient := gPrefs.LabelOrient;
  gPrefs.DisplayOrient := kRenderOrient;
  Vols.AdditiveOverlayBlending := false;
  LayerAdditiveMenu.Checked := Vols.AdditiveOverlayBlending;
  LayerMaskWithBackgroundMenu.Checked := Vols.MaskWithBackground;
  //GLForm1.MosaicMenu.Checked := true;
  //gPrefs.DisplayOrient := kMosaicOrient;
  gClrbar.SizeFraction := gPrefs.ColorbarSize/1000;
  SetDefaultPrefs(gPrefs, false);
  //gPrefs.colorbar := gClrbar.isVisible;
  VisibleClrbarMenu.Checked := gPrefs.ColorbarVisible;
  gClrbar.isVisible := VisibleClrbarMenu.checked;
  TransBlackClrbarMenu.Checked := true;
  ClrbarClr(4);
  //gPrefs.ColorBarPosition := 3;
  SetColorbarPosition;
  Vol1.Azimuth := 110;
  Vol1.Elevation := 30;
  ClipDepthTrack.Position := 0;
  ClipAziTrack.Position := 180;
  ClipElevTrack.Position := 0;
  LightElevTrack.Position := 45;
  LightAziTrack.Position := 0;
  ShaderDrop.ItemIndex := 0;
  ShaderDropChange(Sender);
  ss := getKeyshiftstate;
  if (Sender <> nil) and (ssShift in ss) then begin
    LineWidthEdit.value := 1;
    gPrefs.LineWidth := 1;
    Vol1.Slices.LineWidth := 1;
    Vol1.Slices.LineColor := Vec4(0.5, 0.5, 0.7, 1.0);
    SetDefaultPrefs(gPrefs,true);
    UpdateOpenRecent();
  end;
  UpdateVisibleBoxes();
end;

procedure TGLForm1.DisplayMenuClick(Sender: TObject);
begin
     gPrefs.DisplayOrient := (sender as TMenuItem).tag;
     UpdateVisibleBoxes();
     if gPrefs.DisplayOrient = kMosaicOrient then
       UpdateMosaic(Sender)
     else
         ViewGPU1.Invalidate;
end;

procedure TGLForm1.PrefTrackChange(Sender: TObject);
function Track2S(Pct,Min,Max: single): single;
begin
  if Max > Min then
    result := Min + (Pct/100)*(Max-Min)
  else
    result := Min;
end;
var
 i: integer;
 trk : TTrackBar;
begin
  trk := (Sender as TTrackBar);
  i := trk.Tag;
  if (i < 1) or (i > Vol1.ShaderSliders.nUniform) then exit;
  Vol1.SetShaderSlider(i, Track2S(trk.Position, Vol1.ShaderSliders.Uniform[i].Min, Vol1.ShaderSliders.Uniform[i].Max)) ;
  ViewGPU1.Invalidate;
end;

procedure TGLForm1.ShaderDropChange(Sender: TObject);
var
 shaderName: string;
begin
 if ShaderDrop.Items.Count < 1 then begin
   showmessage('Missing plug ins: Unable to find '+ShaderDir);
   exit;
 end;
 shaderName := ShaderDir +pathdelim+ ShaderDrop.Items[ShaderDrop.ItemIndex]+kExt;
 Vol1.SetShader(shaderName);
 setShaderSliders;
 ViewGPU1.Invalidate;
end;

procedure TGLForm1.SharpenMenuClick(Sender: TObject);
var
   niftiVol: TNIfTI;
begin
  if not vols.Layer(0,niftiVol) then exit;
  niftiVol.Sharpen;
  ViewGPU1.Invalidate;
end;

procedure TGLForm1.BackColorMenuClick(Sender: TObject);
var
 ss: TShiftState;
 vol: TNIfTI;
begin
  ss := getKeyshiftstate;
  if (ssShift in ss) then begin
     if (gPrefs.ClearColor.g > 127) then
        gPrefs.ClearColor := SetRGBA(0,0,0, 255)
     else
         gPrefs.ClearColor := SetRGBA(255,255,255, 255);
  end else begin
      ColorDialog1.Color:= RGBToColor(gPrefs.ClearColor.r, gPrefs.ClearColor.g, gPrefs.ClearColor.b);
      if not ColorDialog1.Execute then exit;
      gPrefs.ClearColor := SetRGBA(Red(ColorDialog1.Color), Green(ColorDialog1.Color), Blue(ColorDialog1.Color), 255);
  end;
  //if (gPrefs.ClearColor.R+gPrefs.ClearColor.G+gPrefs.ClearColor.B) > 128 then
  Vol1.SetTextContrast(gPrefs.ClearColor);
  //else
  //    ViewGPU1.SetTextColor(Vec4(0,0,0, 255));

  if not vols.Layer(0,vol) then exit;
  vol.CX.BackColor := gPrefs.ClearColor;
  //LineBox.caption := '>>'+inttostr(gPrefs.ClearColor.r);
  vol.ForceUpdate;
  if vols.NumLayers > 1 then begin
     if not vols.Layer(1,vol) then exit;
     vol.ForceUpdate;
  end;
  updateTimer.Enabled := true;
  //ViewGPU1.Invalidate;
end;

procedure IncTrackBar (T: TTrackBar);
var
   i: integer;
begin
     i := (T.Max div 4);
     i := ((i+T.Position) div i) * i;
     if i >= T.Max then i := T.Min;
     T.position := i;
end;

procedure TGLForm1.ClipLabelClick(Sender: TObject);
begin
     if (Sender as TLabel).tag = 2 then
        IncTrackBar(ClipElevTrack)
     else if (Sender as TLabel).tag = 1 then
          IncTrackBar(ClipAziTrack)
     else
         IncTrackBar(ClipDepthTrack);
end;

procedure TGLForm1.ColorEditorMenuClick(Sender: TObject);
begin
  Vol1.ShowColorEditor:= ColorEditorMenu.checked;
  ViewGPU1.Invalidate;
end;

procedure TGLForm1.OptimalMosaicPixels(out w,h: integer);
var
   vol: TNIfTI;
   f: single;
begin
  w := ViewGPU1.ClientWidth;
  h := ViewGPU1.ClientHeight;
  if (gPrefs.DisplayOrient <> kMosaicOrient) or (gPrefs.MosaicStr = '') then exit;
  if not vols.Layer(0,vol) then exit;
  Vol1.Slices.MosaicScale(gPrefs.MosaicStr, vol.Mat, vol.InvMat, vol.Dim, vol.Scale, w,h);
  if not gClrbar.isVisible then exit;
  f := gClrBar.PanelFraction;
  if (f > 0.0) then begin
    f := 1.0 +(f/(1-f)); //e.g. if frac is 20%, must increase by 25% (100*1.25 = 125)
    if (gClrBar.isVertical) then
       w := round(w * f)
    else
        h := round(h * f);
  end;
end;

{$IFDEF METALAPI}
procedure TGLForm1.SaveMosaicBmp(bmpName: string);
var
   w,h: integer;
begin
  OptimalMosaicPixels(w,h);
  w := w * gPrefs.BitmapZoom;
  h := h * gPrefs.BitmapZoom;
  if (w <= 0) or (h <= 0) then exit;
  ViewGPU1.align := alNone;
  ViewGPU1.Width := w;
  ViewGPU1.Height := h;
  {$IFDEF METALAPI}
  Vol1.SaveBmp(bmpName);
  {$ELSE}
  ViewGPU1.MakeCurrent();
  ViewGPU1.Width := w;
  ViewGPU1.Height := h;
  ViewGPUPaint(self);
  ViewGPUPaint(self);
  SaveBmp(bmpName, ViewGPU1);
  {$ENDIF}
  ViewGPU1.align := alClient;
  ViewGPU1.Invalidate;
end;

{$ELSE}
procedure TGLForm1.SaveMosaicBmp(bmpName: string);
begin
     //metal only
end;

function ScreenShotGL(GLBox : TOpenGLControl): TBitmap;
var
  RawImage: TRawImage;
  p: array of byte;
  tileW, tileH, nTileW, nTileH,
  wOut, hOut,
  q, w, w4, yOut, h, y, hR, wR, BytePerPixel: integer;
  z: int64;
  DestPtr: PInteger;
begin
 if (gPrefs.BitmapZoom = 1) and (gPrefs.DisplayOrient <> kMosaicOrient) then begin
    result := ScreenShot(GLBox);
    exit;
 end;
 w := GLBox.ClientWidth;
 h := GLBox.ClientHeight;
 GLForm1.OptimalMosaicPixels(wOut,hOut);
 wOut := wOut * gPrefs.BitmapZoom;
 hOut := hOut * gPrefs.BitmapZoom;
 if (wOut <= 0) or (hOut <= 0) or (w <= 0) or (h <= 0) then exit(nil);
 nTileW := ceil(wOut/w);
 nTileH := ceil(hOut/h);
 //GLForm1.Caption := format('%d %d   %d %d',[h,w, hOut, wOut]);
 Result:=TBitmap.Create;
 Result.Width:= wOut;
 Result.Height:= hOut;
 Result.PixelFormat := pf24bit; //if pf32bit the background color is wrong, e.g. when alpha = 0
 //Result.PixelFormat := pf32bit;
 RawImage := Result.RawImage;
 BytePerPixel := RawImage.Description.BitsPerPixel div 8;
 GLBox.MakeCurrent;
 q := Vol1.Quality1to10;
 Vol1.Quality1to10 := 10;
 w4 := 4 * w;
 yOut := 0;
 hR := 0;
 setlength(p, w4 * h);
 for TileH := 0 to  (nTileH-1) do begin
     for TileW := 0 to (nTileW-1) do begin
        GLBox.enableTiledScreenShot(-TileW*w, -h*TileH,wOut, hOut); //tileLeft, tileBottom,totalWidth, totalHeight
        GLForm1.ViewGPUPaint(nil);
        glFlush;
        glFinish;//<-this would pause until all jobs finished: generally a bad idea! required here
        GLBox.SwapBuffers; //<- required by Windows
        {$IFDEF Darwin} //http://lists.apple.com/archives/mac-opengl/2006/Nov/msg00196.html
        glReadPixels(0, 0, w, h, $80E1, $8035, @p[0]); //OSX-Darwin   GL_BGRA = $80E1;  GL_UNSIGNED_INT_8_8_8_8_EXT = $8035;
        {$ELSE} {$IFDEF Linux}
         glReadPixels(0, 0, w, h, GL_RGBA, GL_UNSIGNED_BYTE, @p[0]); //Linux-Windows   GL_RGBA = $1908; GL_UNSIGNED_BYTE
        {$ELSE}
         glReadPixels(0, 0, w, h, $80E1, GL_UNSIGNED_BYTE, @p[0]); //Linux-Windows   GL_RGBA = $1908; GL_UNSIGNED_BYTE
        {$ENDIF} {$ENDIF}
        DestPtr := PInteger(RawImage.Data);
        hR := min(h, hOut-yOut);
        wR := min(w, wOut-(TileW*w) ) ;
        Inc(PByte(DestPtr), (hOut - yOut - hR) * RawImage.Description.BytesPerLine );
        Inc(PByte(DestPtr), TileW * w * 4 );
        z := hR * w4;
        for y:= hR-1 downto 0 do begin
            Dec(z,w4);
            System.Move(p[z], DestPtr^, wR * BytePerPixel );
            Inc(PByte(DestPtr), RawImage.Description.BytesPerLine);
        end; //for y : each line in image
   end;
   yOut := yOut + hR;
 end; //TileH
 //{$DEFINE ISOPAQUE} //see pf24bit
 {$IFDEF ISOPAQUE}
 DestPtr := PInteger(RawImage.Data);
 for z := 0 to ((w * h)-1) do begin
     DestPtr[z] := DestPtr[z] and $FFFFFF00;
 end;
 {$ENDIF}
 GLBox.disableTiledScreenShot();
 GLbox.ReleaseContext;
 Vol1.Quality1to10 := q;
 setlength(p, 0);
 ViewGPU1.Invalidate;
end;
{$ENDIF}

procedure TGLForm1.ExitFullScreenMenuClick(Sender: TObject);
begin
  GLForm1.WindowState:= wsNormal;
  ExitFullScreenMenu.Visible := false;
end;

procedure TGLForm1.EditCopyMenuClick(Sender: TObject);
{$IFNDEF METALAPI}
var
  bmp: TBitmap;
begin
 bmp := ScreenShotGL(ViewGPU1);
 if (bmp = nil) then exit;
 Clipboard.Assign(bmp);
 bmp.Free;
end;
{$ELSE}
begin
//
end;
{$ENDIF}

procedure TGLForm1.DisplayAnimateMenuClick(Sender: TObject);
begin
    AnimateTimer.enabled := DisplayAnimateMenu.Checked;
end;

procedure TGLForm1.AnimateTimerTimer(Sender: TObject);
var
    v: TNIfTI;
    i: integer;
begin
  for i := 1 to vols.NumLayers do begin
    vols.Layer(i-1,v);
    if v.Header.dim[4] < 2 then
       continue;
    v.SetDisplayVolume(v.VolumeDisplayed + 1);
    //LayerBox.caption := format('%d %d/%d ',[i,v.VolumeDisplayed+1, v.Header.dim[4]]); //+1 as indexed from 1
    UpdateLayerBox(true);// e.g. "fMRI (1/60)" -> "fMRI (2/60"
    //UpdateTimer.Enabled := true;
    UpdateTimerTimer(Sender);
    exit;
  end;
  //exit if success
  AnimateTimer.Enabled := false;
  DisplayAnimateMenu.Checked := false;
end;

function TGLForm1.DisplayNextMenuClick(Sender: TObject): boolean;
var
   v: TNIfTI;
   i: integer;
begin
 result := false;
 for i := 1 to vols.NumLayers do begin
   vols.Layer(i-1,v);
   if v.Header.dim[4] < 2 then
      continue;
   if (Sender as TMenuItem).Tag = 1 then
      v.SetDisplayVolume(v.VolumeDisplayed + 1)
   else
       v.SetDisplayVolume(v.VolumeDisplayed - 1);
   //LayerBox.caption := format('%d %d/%d ',[i,v.VolumeDisplayed+1, v.Header.dim[4]]); //+1 as indexed from 1
   UpdateLayerBox(true);// e.g. "fMRI (1/60)" -> "fMRI (2/60"
   UpdateTimer.Enabled := true;
   exit(true);
 end;

end;

procedure TGLForm1.FileExitMenuClick(Sender: TObject);
begin
  Close;
end;

procedure TGLForm1.LayerVolumeChange(Sender: TObject);
var
  i: integer;
  v: TNIfTI;
begin
  i := LayerList.ItemIndex;
  if (i < 0) or (i >= LayerList.Count) then exit;
  if not vols.Layer(LayerList.ItemIndex,v) then exit;
  if v.Header.dim[4] < 2 then exit;
  if (Sender as TMenuItem).Tag = 1 then
     v.SetDisplayVolume(v.VolumeDisplayed + 1)
  else
      v.SetDisplayVolume(v.VolumeDisplayed - 1);
  //caption := format('%d/%d ',[v.VolumeDisplayed+1, v.Header.dim[4]]); //+1 as indexed from 1
  UpdateLayerBox(true);// e.g. "fMRI (1/60)" -> "fMRI (2/60"
  UpdateTimer.Enabled := true;
end;

procedure TGLForm1.MenuItem1Click(Sender: TObject);
const
  {$IFDEF UNIX} //end of line
  kEOLN = #10; //Windows CRLF   ;
  {$ELSE}
   kEOLN = #13#10; //Windows CRLF
  {$ENDIF}
var
  s: string;
begin
 s := 'Display 3D Render'
    +kEOLN+'  Drag: Rotate view'
    +kEOLN+'Display 2D Slices'
    +kEOLN+'Drag: Move crosshair'
    +kEOLN+'  Shift-Drag: Adjust contrast'
    +kEOLN+'Colorbar'
    +kEOLN+'  Double-Click: Change position'
    +kEOLN+'Color Editor'
    +kEOLN+'  Drag Node: Change node intensity/opacity'
    +kEOLN+'  Shift-Click Node: Delete node'
    +kEOLN+'  Double-Click Node: Edit node color'
    +kEOLN+'  Control-Click: Add node'
    +kEOLN+'  Alt-Click: Save colors to disk';
  {$IFDEF NewCocoa}
  ShowAlertSheet(GLForm1.Handle,'Mouse Gestures', s);
  {$ELSE}
  Showmessage(s);
  {$ENDIF}
end;

procedure TGLForm1.LayerCloseMenuClick(Sender: TObject);
begin
 vols.CloseLayer(LayerList.ItemIndex);
 UpdateLayerBox(true);
 UpdateTimer.Enabled := true;
end;

procedure TGLForm1.LayerCutoutMenuClick(Sender: TObject);
var
    i: integer;
    niftiVol: TNIfTI;
begin
  i := LayerList.ItemIndex;
  if (i < 0) or (i >= LayerList.Count) then exit;
  if not vols.Layer(LayerList.ItemIndex,niftiVol) then exit;
  niftiVol.HiddenByCutout:= LayerCutoutMenu.Checked;
  niftiVol.CX.NeedsUpdate := true;
  CutoutChange(Sender)
end;

procedure TGLForm1.LayerListClickCheck(Sender: TObject);
var
  i: integer;
  niftiVol: TNIfTI;
begin
  UpdateLayerBox(false);
  i := LayerList.ItemIndex;
  if not vols.Layer(LayerList.ItemIndex,niftiVol) then exit;
  if LayerList.Checked[i] then
     LayerAlphaTrack.Position := 100
  else
      LayerAlphaTrack.Position := 0;
  niftiVol.CX.NeedsUpdate:=true;
  LayerWidgetChange(Sender);
end;

procedure TGLForm1.LayerListShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  i: integer;
  niftiVol: TNIfTI;
begin
  LayerList.Hint := 'Right-click for options';
  i := LayerList.ItemIndex;
  if (i < 0) or (i >= LayerList.Count) then exit;
  if not vols.Layer(LayerList.ItemIndex,niftiVol) then exit;
  LayerList.Hint := 'Right-click for options: '+ niftiVol.Filename;
end;

procedure TGLForm1.LayerOptionsBtnClick(Sender: TObject);
begin
  LayerPopup.PopUp;
end;

procedure TGLForm1.LayerShowBidsMenuClick(Sender: TObject);
var
  i: integer;
  strs: TStringList;
  PrefForm: TForm;
  Memo: TMemo;
  niftiVol: TNIfTI;
begin
  i := LayerList.ItemIndex;
  if (i < 0) or (i >= LayerList.Count) then exit;
  if not vols.Layer(LayerList.ItemIndex,niftiVol) then exit;
  if (niftiVol.BidsName = '') or (not Fileexists(niftiVol.BidsName)) then exit;
  strs := TStringList.Create;
  strs.LoadFromFile(niftiVol.BidsName);
  //Clipboard.AsText:= strs.Text;
  PrefForm := TForm.Create(nil);
  PrefForm.Caption := 'BIDS file for "'+niftiVol.ShortName+'"' ;
  PrefForm.Width := Max(80, Screen.Width div 2);
  PrefForm.Height := Max(60, Screen.Height div 2);
  PrefForm.BorderStyle:= bsSizeable;
  PrefForm.FormStyle:= fsNormal;
  //PrefForm.AutoSize := True;
  PrefForm.BorderWidth := 4;
  PrefForm.Position := poScreenCenter;
  Memo:=TMemo.create(PrefForm);
  Memo.Align := alClient;
  Memo.ScrollBars:= ssAutoBoth;
  Memo.ReadOnly:=true;
  Memo.Lines.AddStrings(strs);
  strs.Free;
  Memo.Parent:=PrefForm;
  PrefForm.ShowModal;
  //Memo.Free;
  FreeAndNil(PrefForm);
end;

procedure TGLForm1.LayerShowHeaderMenuClick(Sender: TObject);
var
  i: integer;
  niftiVol: TNIfTI;
begin
  i := LayerList.ItemIndex;
  if (i < 0) or (i >= LayerList.Count) then exit;
  if not vols.Layer(LayerList.ItemIndex,niftiVol) then exit;
  HdrForm.WriteHdrForm(niftiVol.HeaderNoRotation, niftiVol.IsNativeEndian, niftiVol.Filename, niftiVol.Dim);
  HdrForm.SaveHdrDlg.Filename := niftiVol.Filename;
  HdrForm.show;
end;

procedure TGLForm1.OpenFSLMenuClick(Sender: TObject);
var
  p,f: string;
  ss: TShiftState;
begin
  ss := getKeyshiftstate;
  f := OpenDialog1.filename;
  p := OpenDialog1.InitialDir;
  OpenDialog1.InitialDir :=  GetFSLdir+pathdelim+ 'data'+pathdelim+'standard';
  if not OpenDialog1.execute then begin
    OpenDialog1.filename := f;
    OpenDialog1.InitialDir := p;
    exit;
  end;
  if (ssMeta in ss) or (ssCtrl in ss) then
     AddLayer(OpenDialog1.filename)
  else
      AddBackground(OpenDialog1.filename);
  OpenDialog1.filename := f;
  OpenDialog1.InitialDir := p;
end;

function ensurePath(fnm: string): string;
var
  pth: string;
begin
     result := fnm;
     if length(ExtractFilePath(fnm)) > 0 then exit;
     pth := GetUserDir+pathdelim+'Desktop';
     if not fileexists(pth) then
        pth := GetUserDir;
     result := IncludeTrailingPathDelimiter(pth)+fnm;
end;

procedure TGLForm1.Save2Bmp(fnm: string);
var
{$IFDEF METALAPI}
 q: integer;
{$ELSE}
bmp: TBitmap;
png: TPortableNetworkGraphic;
{$ENDIF}
begin
 while (isBusy) or (GLForm1.Updatetimer.enabled) do
       Application.ProcessMessages; //apply any time consuming updates
 fnm := ChangeFileExt(fnm,'.png');
 fnm := ensurePath(fnm);
 {$IFDEF METALAPI} //to do: OpenGL must using tiling due to "pixel ownership problem"
 q := Vol1.Quality1to10;
 Vol1.Quality1to10 := 10;
 if (gPrefs.DisplayOrient = kMosaicOrient) and (gPrefs.MosaicStr <> '') then begin
    SaveMosaicBmp(fnm);
    Vol1.Quality1to10 := q;
    exit;
 end;
 Vol1.SaveBmp(fnm);
 Vol1.Quality1to10 := q;
 {$ELSE}
  bmp := ScreenShotGL(ViewGPU1);
  if (bmp = nil) then exit;
  png := TPortableNetworkGraphic.Create;
  try
   png.Assign(bmp);    //Convert data into png
   png.SaveToFile(fnm);
  finally
    png.Free;
  end;
  bmp.Free;
 {$ENDIF}
end;

procedure TGLForm1.SaveMenuClick(Sender: TObject);
begin
     if not SaveDialog1.execute then exit;
     Save2Bmp(SaveDialog1.Filename);
end;

function sph2cartDeg(Azimuth,Elevation: single): TVec4;
//convert spherical AZIMUTH,ELEVATION,RANGE to Cartesion
//see Matlab's [x,y,z] = sph2cart(THETA,PHI,R)
// reverse with cart2sph
var
  E,Phi,Theta: single;
begin
  E := Azimuth;
  while E < 0 do
    E := E + 360;
  while E > 360 do
    E := E - 360;
  Theta := DegToRad(E);
  E := Elevation;
  while E > 90 do
    E := E - 90;
  while E < -90 do
    E := E + 90;
  Phi := DegToRad(E);
  result := Vec4(cos(Phi)*cos(Theta),cos(Phi)*sin(Theta),sin(Phi), 0.0);
  result := result.normalize;
end;

function sph2cartDeg90clip(Azimuth,Elevation, Depth: single): TVec4;
begin
  result := sph2cartDeg(Azimuth-90,Elevation);
  result := Vec4(-result.X,-result.Y,-result.Z, 0.5-Depth);
end;

function sph2cartDeg90Light(Azimuth,Elevation: single): TVec4;
var
  T: TVec4;
begin
  T := sph2cartDeg(Azimuth,Elevation);
  result := Vec4(T.Y,T.Z,T.X, 0.0); //OpenGL X/Y/Z = LeftRight/TopBottom/NearFar
end;

procedure TGLForm1.UpdateShaderSettings(Sender: TObject);
begin
 Vol1.Quality1to10:=QualityTrack.Position;
 Vol1.LightPosition := sph2cartDeg90Light(LightAziTrack.Position, LightElevTrack.Position);
 Vol1.clipPlane := sph2cartDeg90clip(ClipAziTrack.Position, ClipElevTrack.Position, ClipDepthTrack.Position/ClipDepthTrack.Max );
 ViewGPU1.Invalidate;
end;

procedure TGLForm1.ParamStr2Script();
var
   i, layers: integer;
   lo, hi: single;
   s: string;
   script : TStringList;
procedure AddImg();
begin
 if layers = 0 then
    script.Add(format('gl.loadimage(''%s'')',[s]) )
 else
     script.Add(format('gl.overlayload(''%s'')',[s]) );
 layers := layers + 1;
end;
//e.g. ./MRIcroGL -std  motor -cm actc -dr 2 4
begin
     script := TStringList.Create;
     script.Add('import gl');
     i := 1;
     layers := 0;
     while i <= ParamCount do begin
        s := ParamStr(i);
        inc(i);
        //https://users.fmrib.ox.ac.uk/~paulmc/fsleyes/userdoc/latest/command_line.html
        if s[1] = '-' then begin //special commands, '-std' '-cm actc'
           if (upcase(s[2]) = 'S') and (upcase(s[3]) = 'T') then//e.g. '-std', '-standard1mm'
              AddImg()
           else if (upcase(s[2]) = 'C') and (upcase(s[3]) = 'M') and (i <= ParamCount) then begin//e.g. '-cm actc'
              s := ParamStr(i);
              inc(i);
              script.Add(format('gl.colorname (%d,"%s")',[max(layers-1,0), s]) );
           end else if (upcase(s[2]) = 'D') and (upcase(s[3]) = 'R') and ((i+1) <= ParamCount) then begin//e.g. '-dr 2 5'
              lo := strtofloatdef(ParamStr(i), 0.0);
              inc(i);
              hi := strtofloatdef(ParamStr(i), 01.0);
              inc(i);
              script.Add(format('gl.minmax (%d, %g, %g)',[max(layers-1,0), lo, hi]) );
           end else             //gl.minmax(2, -4, -4)
               dwriteln('Unknown argument "'+s+'"');
           //dwriteln('>>Unknown argument "'+upcase(s[2])+upcase(s[3])+'"');
           continue;
        end;
        AddImg();
      end; //for each parameter
      ScriptMemo.Lines.Clear;
      ScriptMemo.Lines.AddStrings(script);
      Script.Free;
      ScriptingRunMenuClick(nil);
end;

procedure TGLForm1.UpdateTimerTimer(Sender: TObject);
//compute slow updates - defer if still computing prior update
var
   niftiVol: TNIfTI;
   s: string;
begin
 if isBusy then exit;
 UpdateTimer.Enabled := false;
 if not vols.Layer(0,niftiVol) then exit;
 if (gPrefs.InitScript <> '') then begin
    UpdateTimer.Tag := 0;
    s := gPrefs.InitScript;
    gPrefs.InitScript := '';
    //if ParamCount < 1 then exit;
    if (ParamCount > 0) and (s = ('-')) then begin
       s := ParamStr(ParamCount);
       if (upcase(ExtractFileExt(s)) <> '.PY') and (upcase(ExtractFileExt(s)) <> '.TXT') then begin
          {$IFDEF UNIX}writeln('Assuming arguments are images not script (not .py or .txt) "'+s+'"');{$ENDIF}
          ParamStr2Script();// AddBackground(s);
          exit;
       end;

       //gPrefs.InitScript := s;
    end;
    if (not FileExists(s)) then begin
       {$IFDEF UNIX}writeln('Unable to find file provided from command line: '+s);{$ENDIF}
       exit;
    end;
    {$IFDEF UNIX}writeln('Reading script '+s);{$ENDIF}
    if (upcase(ExtractFileExt(s)) = '.PY') then
       OpenScript(s, false);
    exit;
 end;
 if (Updatetimer.Tag = 1) then begin
    UpdateTimer.Tag := 0;
    ViewGPU1.Invalidate;
    exit;
 end;
 isBusy := true;
 niftiVol.SetDisplayMinMax(niftiVol.DisplayMin, niftiVol.DisplayMax);
 Vol1.UpdateOverlays(vols);
 UpdateColorBar();
 isBusy := false;
 ViewGPU1.Invalidate;
end;

procedure TGLForm1.AboutMenuClick(Sender: TObject);
var
   niftiVol: TNIfTI;
   w, s: string;
begin
  //showmessage(IntToStr(Lo(DosVersion) - 4)+'.' +IntToStr(Hi(DosVersion))); //add 'DOS to uses
  if not vols.Layer(0,niftiVol) then exit;
  w := '';
  {$IFDEF METALAPI}
  //s := chr(13)+chr(10)+'Apple Metal';
  s := chr(13)+chr(10)+'Apple Metal multisample=' + inttostr(ViewGPU1.renderView.sampleCount);
  {$ELSE}
  ViewGPU1.MakeCurrent(false);
  {$IFDEF LCLCocoa}
  w := 'Cocoa';
  {$ENDIF}
  {$IFDEF LCLQT5}
  w := 'QT5';
  {$ENDIF}
  {$IFDEF LCLQT}
  w := 'QT';
  {$ENDIF}
  {$IFDEF LCLGTK2}
  w := 'QT';
  {$ENDIF}
  {$IFDEF LCLGTK2}
  w := 'QT';
  {$ENDIF}
  {$IFDEF LCLWin64}
  w := 'QT';
  {$ENDIF}
  w := w +chr(13)+chr(10);
  s := chr(13)+chr(10)+ glGetString(GL_VENDOR)+'; OpenGL= '+glGetString(GL_VERSION)+'; Shader='+glGetString(GL_SHADING_LANGUAGE_VERSION);
  ViewGPU1.ReleaseContext;
  {$ENDIF}
  s := format('%s %sVolume rendering for NIfTI images %d %d %d %s', [kVers, w, niftiVol.Dim.X, niftiVol.Dim.Y, niftiVol.Dim.Z, s]);
  {$IFDEF NewCocoa}
  ShowAlertSheet(GLForm1.Handle,'MRIcroGL', s);
  {$ELSE}
  s := 'MRIcroGL '+ s;
  Showmessage(s);
  {$ENDIF}
end;

procedure TGLForm1.SaveColorTable;
var
 niftiVol: TNIfTI;
 dlg : TSaveDialog;
begin
  if not vols.Layer(0,niftiVol) then exit;
  if messagedlg('Alt-click with color editor: do you want to save this color table?',mtConfirmation, mbOKCancel, 0) = mrCancel then exit;
  dlg := TSaveDialog.Create(self);
  dlg.Title := 'Save color table';
  dlg.InitialDir := ResourceDir+pathdelim+'lut'; ;
  dlg.Filter := 'Color Table|*.clut';
  dlg.DefaultExt := 'clut';
  dlg.FilterIndex := 0;
  if dlg.Execute then begin
     niftiVol.CX.SaveCLUT(dlg.Filename);
     showmessage('Restart to load the new color table');
  end;
  dlg.Free;
end;

procedure TGLForm1.ViewGPUMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   niftiVol: TNIfTI;
   xIn, yIn, i: integer;
   {$IFDEF LCLCocoa}{$IFNDEF METALAPI}
   f: single;
   {$ENDIF}{$ENDIF}
   fracXYZ: TVec3;
begin
 xIn := X;
 yIn := Y;
 {$IFDEF LCLCocoa}{$IFNDEF METALAPI}
 f := ViewGPU1.retinaScale;
 X := round(X * f);
 Y := round(Y * f);
 {$ENDIF}{$ENDIF}
 gMouseDrag := false;
  if (ssAlt in Shift) and (ColorEditorMenu.Checked) then begin
     SaveColorTable;
     exit;
  end;
  if not vols.Layer(0,niftiVol) then exit;
 gMouse.Y := Y;
 gMouse.X := X;
  if (Vol1.CE.ColorEditorMouseDown(X,Y, (ssShift in Shift), (ssCtrl in Shift), niftiVol)) then begin
     ViewGPU1.Invalidate;
     if (ssShift in Shift) or (ssCtrl in Shift) then
        UpdateTimer.Enabled := true;
     exit;
  end;
 if gPrefs.DisplayOrient > kMax2DOrient then exit;
 if  (Vols.Drawing.ActivePenColor >= 0) and (not AutoROIForm.Visible) then begin
    EnsureOpenVoi();
    fracXYZ := Vol1.GetSlice2DFrac(X,Y,i);
    if (ssShift in Shift) then begin
       if Vols.Drawing.ActivePenColor <> 0 then
           Vols.Drawing.ActivePenColor := 0
        else
          Vols.Drawing.ActivePenColor := 1;
     end;
    if (i > 0) then
       Vols.Drawing.voiMouseDown(i, fracXYZ);
    exit;
 end;
 ViewGPUMouseMove(Sender, Shift, xIn,Yin);
 if (ssShift in Shift) or  (SSRight in Shift) then
    gMouseDrag := true;
end;

procedure TGLForm1.ViewGPUMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
{$IFDEF LCLCocoa}{$IFNDEF METALAPI}
var
 f: single;
 {$ENDIF}{$ENDIF}
begin
 {$IFDEF LCLCocoa}{$IFNDEF METALAPI}
 f := ViewGPU1.retinaScale;
 X := round(X * f);
 Y := round(Y * f);
 {$ENDIF}{$ENDIF}
 vol1.SelectionRect.x := -1;
 if Vol1.CE.ColorEditorMouseUp() then
    UpdateTimer.Enabled := true;
 if (Vols.Drawing.IsOpen) and (Vols.Drawing.ActivePenColor >= 0) and (Vols.Drawing.MouseDown) then begin
    Vols.Drawing.voiMouseUp(true, DrawOverwriteMenu.Checked);
    Vols.Drawing.ActivePenColor := Vols.Drawing.PenColorOnRelease;
    UpdateTimer.enabled := true;
    gMouse.Y := -1; //released
    exit;
 end;
 if gMouseDrag then begin
    UpdateContrast(gMouse.X,gMouse.Y,X,Y);
    gMouseDrag := false;
 end;
 gMouse.Y := -1; //released
end;

procedure TGLForm1.ViewGPUMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
 sliceMM, fracXYZ: TVec3;
 i: integer;
 vox: TVec3i;
 str : string;
 niftiVol: TNIfTI;
 {$IFDEF LCLCocoa}{$IFNDEF METALAPI}
 f:single;
 {$ENDIF}{$ENDIF}
begin
 {$IFDEF LCLCocoa}{$IFNDEF METALAPI}
 f := ViewGPU1.retinaScale;
 X := round(X * f);
 Y := round(Y * f);
 {$ENDIF}{$ENDIF}
 if not vols.Layer(0,niftiVol) then exit;
 if gMouse.Y < 0 then exit; //mouse is not down
 if (Vol1.CE.ColorEditorMouseMove(X,Y, niftiVol)) then begin
    ViewGPU1.Invalidate;
    //UpdateTimer.Enabled := true;;
    exit;
 end;
 if gMouseDrag then begin
    vol1.SelectionRect.x := gMouse.X;
    vol1.SelectionRect.y := gMouse.Y;
    vol1.SelectionRect.z := X;
    vol1.SelectionRect.w := Y;
    ViewGPU1.Invalidate;
    exit;
 end;
 if gPrefs.DisplayOrient <= kMax2DOrient then begin
    if  (Vols.Drawing.IsOpen) and (Vols.Drawing.ActivePenColor >= 0) and (Vols.Drawing.MouseDown) then begin
       fracXYZ := Vol1.GetSlice2DFrac(X,Y,i);
       if (i = Vols.Drawing.voiActiveOrient) then begin
          Vols.Drawing.voiMouseMove(fracXYZ.X, fracXYZ.Y, fracXYZ.Z);
          ViewGPU1.Invalidate;
       end;
       exit;
    end;
    Vol1.SetSlice2DFrac(Vol1.GetSlice2DFrac(X,Y,i));
    if (Vols.Drawing.IsOpen) then //set crosshair to voxel center
       Vol1.SetSlice2DFrac(niftiVol.FracShiftSlice(vol1.Slices.SliceFrac, pti(0,0,0)));
     sliceMM := Vol1.Slice2Dmm(niftiVol, vox);
     {$IFDEF COMPILEYOKE}
     //if (not isYoke) then
     gSliceMM := Vec3(sliceMM.X, sliceMM.Y, sliceMM.Z);
        SetShareFloats2D(sliceMM.X,sliceMM.Y,sliceMM.Z);
     {$ENDIF}
     str := '';
     str := str + format('%3.6g %3.6g %3.6g = ', [sliceMM.x, sliceMM.y, sliceMM.z]);
     str := str + niftiVol.VoxIntensityString(vox);//format('%3.6g', [niftiVol.VoxIntensity(vox)]);
     if vols.NumLayers > 1 then
        for i := 1 to (vols.NumLayers-1) do begin
            if not vols.Layer(i,niftiVol) then exit;
            str := str + '; ' + niftiVol.VoxIntensityString(vox);
        end;
     caption := str;
     ViewGPU1.Invalidate;
     exit;
  end;
  if gPrefs.DisplayOrient <> kRenderOrient then exit; //e.g. mosaics
  Vol1.Azimuth := Vol1.Azimuth + (X - gMouse.X);
  Vol1.Elevation := Vol1.Elevation + (Y - gMouse.Y);
  while Vol1.Azimuth > 360 do Vol1.Azimuth := Vol1.Azimuth - 360;
  while Vol1.Azimuth < 0 do Vol1.Azimuth := Vol1.Azimuth + 360;
  if Vol1.Elevation > 90 then Vol1.Elevation := 90;
  if Vol1.Azimuth < -90 then Vol1.Elevation := -90;
  {$IFDEF COMPILEYOKE}
  SetShareFloats3D(Vol1.Azimuth, Vol1.Elevation);
  {$ENDIF}
  gMouse.X := X;
  gMouse.Y := Y;
  ViewGPU1.Invalidate;
end;

procedure TGLForm1.UpdateContrast (Xa,Ya, Xb, Yb: integer);
var
   ptA, ptB: TVec3; //fractional coordinates
   ptAi, ptBi: TVec3i; //voxel coordinates
   yi, zi, x,y,z: integer;
   v, Mn, Mx: single;
var
   niftiVol: TNIfTI;
begin
  if not vols.Layer(0,niftiVol) then begin
     ViewGPU1.Invalidate;
     exit;
  end;
  if niftiVol.IsLabels then exit;
  ptA := Vol1.GetSlice2DFrac(Xa,Ya,x);
  ptB := Vol1.GetSlice2DFrac(Xb,Yb,y);
  if x <> y then exit; //e.g. dragged across coronal and axial slice
  if (x < kAxialOrient) or (x > kSagLeftOrient) then exit;
  //LayerBox.Caption := format('%.2g %.2g %.2g -> %.2g %.2g %.2g', [ptA.X,ptA.Y,ptA.Z, ptB.X,ptB.Y,ptB.Z]);
  ptAi := niftiVol.FracToSlice(ptA);
  ptBi := niftiVol.FracToSlice(ptB);
  SortVec3i(ptAi, ptBi);
  Mx := (- 1.0) / (0.0); //SingleHelper.NegativeInfinity;
  Mn := 1.0 / 0.0; //SingleHelper.PositiveInfinity
  for z := ptAi.z to ptBi.z do begin
    zi := z * niftiVol.Dim.x * niftiVol.Dim.y;
    for y := ptAi.y to ptBi.y do begin
      yi := y * niftiVol.Dim.x;
      for x := ptAi.x to ptBi.x do begin
          v := niftiVol.VoxIntensity(zi+yi+x);
          if (v < Mn) then Mn := v;
          if (v > Mx) then Mx := v;
      end; //x
    end; //y
  end; //z
  if Mn = Mx then exit;
  LayerChange(0, -1, -1, Mn, Mx); //kNaNsingle
end;

procedure TGLForm1.ViewGPUMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
   niftiVol: TNIfTI;
begin
  if gPrefs.DisplayOrient = kMosaicOrient then exit;
  if gPrefs.DisplayOrient <> kRenderOrient then begin
     if not vols.Layer(0,niftiVol) then exit;
     if niftiVol.VolumesLoaded > 1 then
        niftiVol.SetDisplayVolume(niftiVol.VolumeDisplayed + 1);
     UpdateTimer.Enabled := true;
     exit;
  end;
  if WheelDelta = 0 then exit;
  if Wheeldelta < 0 then
     Vol1.Distance := Vol1.Distance - 0.1
  else
      Vol1.Distance := Vol1.Distance + 0.1;
  if Vol1.Distance > kMaxDistance then
     Vol1.Distance := kMaxDistance;
  if Vol1.Distance < 1 then
     Vol1.Distance := 1.0;
  ViewGPU1.Invalidate;
end;

procedure TGLForm1.FormShow(Sender: TObject);
var
 i: integer;
 c: char;
 isForceReset, isOK: boolean;
 s, shaderPath, shaderName: string;
 shaderNames : TStringList;
 newMenu: TMenuItem;
begin
 {$IFDEF FPC} Application.ShowButtonGlyphs:= sbgNever; {$ENDIF}
 isForceReset := false;
 gPrefs.InitScript := '';
 //gPrefs.InitScript := '/Users/rorden/MRIcroGL12/MRIcroGL.app/Contents/Resources/script/basic.py';
 i := 1;
 while i <= ParamCount do begin
    s := ParamStr(i);
    if (length(s)> 1) and (s[1]='-') then begin
        c := upcase(s[2]);
        if c='R' then
           isForceReset := true
        else if (i < paramcount) and (c='S') then begin
          inc(i);
          //if fileexists(ParamStr(i)) then
          if (upcase(ExtractFileExt(ParamStr(i))) = '.PY') or (upcase(ExtractFileExt(ParamStr(i))) = '.TXT') then
             gPrefs.InitScript := ParamStr(i);
        end;
    end;
    inc(i);
  end; //for each parameter
  //check - on darwin form drop file
  if (gPrefs.InitScript = '') and (ParamCount >= 1) and (not isForceReset) then //and (fileexists(ParamStr(ParamCount))) then
     gPrefs.InitScript := '-';
  if (length(gPrefs.InitScript) > 0) and (gPrefs.InitScript <> '-') and (not fileexists(gPrefs.InitScript)) then begin
     {$IFDEF UNIX}writeln('Unable to find script '+gPrefs.InitScript);{$ENDIF}
     gPrefs.InitScript := '';
  end;
  //set defaults
  if isForceReset then
     SetDefaultPrefs(gPrefs, true)
  else begin
      if (gPrefs.InitScript = '') and (fileexists(ScriptDir+pathdelim+'startup.py')) then
         gPrefs.InitScript := ScriptDir+pathdelim+'startup.py';
      IniFile(true,  gPrefs);
      dcm2niiForm.setCustomDcm2niix(gPrefs.CustomDcm2niix);
      if (gPrefs.DisplayOrient = kMosaicOrient) then
         gPrefs.DisplayOrient := kRenderOrient;
  end;
  AnimateTimer.Interval:= gPrefs.AnimationIntervalMsec;
  if gPrefs.StartupWindowMode = 1 then begin
     GLForm1.BoundsRect := Screen.MonitorFromWindow(Handle).BoundsRect;
     GLForm1.WindowState:= wsMaximized;
  end;
  if gPrefs.StartupWindowMode = 2 then begin
     GLForm1.WindowState:= wsFullScreen;
     {$IFNDEF LCLCocoa}ExitFullScreenMenu.Visible:=true;{$ENDIF} //Linux has issues getting out of full screen
  end;
  //auto generate shaders
  shaderPath := ShaderDir;
  if not DirectoryExists(shaderPath) then showmessage('Unable to find shaders "'+shaderPath+'"');
  shaderNames := FindAllFiles(shaderPath, '*'+kExt, false);
  try
    if shaderNames.Count > 0 then begin
       shaderNames.Sort;
       for i := 0 to (shaderNames.Count-1) do begin
           shaderName := ChangeFileExt(ExtractFileName(shaderNames[i]),'');
           if (length(shaderName) < 1) or (shaderName[1] = '_') or (shaderName[1] = '.') then
              continue;
           ShaderDrop.Items.add(shaderName);
       end;
       shaderName := shaderPath + pathdelim + ShaderDrop.Items[0]+kExt;
    end else begin
        shaderName := '';
        ShaderDrop.Items.add('Shaders missing');
        {$IFDEF METALAPI}
        showmessage('Fatal error: Unable to find metal shaders in '+shaderPath);
        {$ENDIF}
    end;
    ShaderDrop.ItemIndex := 0;
  finally
    shaderNames.Free;
  end;
  //prepare defaults
  gMouse.y := -1;
  gLandmark := TLandmark.Create();
  CreateStandardMenus;
  if DirectoryExists(GetFSLdir+pathdelim+ 'data'+pathdelim+'standard') then
     OpenFSLMenu.Visible := true;
  s := gPrefs.PrevBackgroundImage;
  if (not fileexists(s)) then
     s := gPrefs.PrevFilename[1];
  if (not fileexists(s)) then
     s := StandardDir+pathdelim+'mni152.nii.gz';
  if (not fileexists(s)) and (OpenStandardMenu.count > 0) then begin
     s := StandardDir+pathdelim+OpenStandardMenu.Items[0].Caption+'.nii';
     if not fileexists(s) then
        s := s + '.gz';
  end;
  if (length(gPrefs.InitScript) > 0) then
     s := '+'; //load borg for quick load
  vols := TNIfTIs.Create(s,  gPrefs.ClearColor, gPrefs.LoadFewVolumes, gPrefs.MaxVox, isOK); //to do: warning regarding multi-volume files?
  //niftiVol := TNIfTI.Create('/Users/rorden/metal_demos/tar.nii');
  //niftiVol := TNIfTI.Create('/Users/rorden/metal_demos/rmotor.nii.gz', niftiVol.Mat, niftiVol.Dim);
  {$IFDEF METALAPI}
  ViewGPU1 :=  TMetalControl.Create(CenterPanel);
  //ViewGPU1.OnPrepare := @ViewGPUPrepare;
  EditMenu.Visible := false; //to do: copy bitmap
  {$ELSE}
  ViewGPU1 :=  TOpenGLControl.Create(GLForm1);
  ViewGPU1.OpenGLMajorVersion := 3;
  ViewGPU1.OpenGLMinorVersion := 3;
  ViewGPU1.MultiSampling := 4;
  {$ENDIF}
  ViewGPU1.Parent := GLForm1;
  {$IFDEF METALAPI}ViewGPU1.renderView.setSampleCount(4);{$ENDIF}
  ViewGPU1.Align:= alClient;
  ViewGPU1.OnDblClick :=  @ViewGPUDblClick;
  ViewGPU1.OnMouseDown := @ViewGPUMouseDown;
  ViewGPU1.OnMouseMove := @ViewGPUMouseMove;
  ViewGPU1.OnMouseUp := @ViewGPUMouseUp;
  ViewGPU1.OnMouseWheel := @ViewGPUMouseWheel;
  ViewGPU1.OnPaint := @ViewGPUPaint;
  Vol1 := TGPUVolume.Create(ViewGPU1);
  //Vol1.Slices.RadiologicalConvention := gPrefs.FlipLR_Radiological;
  {$IFNDEF METALAPI}
  {$IFDEF LCLCocoa}
  HelpPrefMenu.Visible:= false;
  ViewGPU1.setRetina(gPrefs.RetinaDisplay);
  {$ENDIF}
  ViewGPU1.MakeCurrent(false);
  if (not  Load_GL_version_3_3_CORE) then begin
     showmessage('Unable to load OpenGL 3.3 Core');
     halt;
  end;
  //GLForm1.caption := glGetString(GL_VENDOR)+'; OpenGL= '+glGetString(GL_VERSION)+'; Shader='+glGetString(GL_SHADING_LANGUAGE_VERSION);
  ViewGPU1.ReleaseContext;
  Vol1.Prepare(shaderName);
  setShaderSliders;
  //ViewGPUPrepare(Sender);
  {$ENDIF}
  //auto generate color tables
  shaderPath := ResourceDir+pathdelim+'lut';
  if not DirectoryExists(shaderPath) then showmessage('Unable to find color tables "'+shaderPath+'"');
  shaderNames := FindAllFiles(shaderPath, '*.clut', false);
  try
    if shaderNames.Count > 0 then begin
       shaderNames.Sort;
       for i := 0 to (shaderNames.Count-1) do begin
           shaderName := ChangeFileExt(ExtractFileName(shaderNames[i]),'');
           if (length(shaderName) < 1) or (shaderName[1] = '_') or (shaderName[1] = '.') then
              continue;
           LayerColorDrop.Items.add(shaderName);
       end;
    end;
  finally
    shaderNames.Free;
  end;
  //auto generate template script
  shaderPath := ScriptDir;
  if not DirectoryExists(shaderPath) then showmessage('Unable to find scripts "'+shaderPath+'"');
  shaderNames := FindAllFiles(shaderPath, '*py', false);
  try
    if shaderNames.Count > 0 then begin
       shaderNames.Sort;
       for i := 0 to (shaderNames.Count-1) do begin
           shaderName := ChangeFileExt(ExtractFileName(shaderNames[i]),'');
           if (length(shaderName) < 1) or (shaderName[1] = '_') or (shaderName[1] = '.') then
              continue;
           newMenu := TMenuItem.Create(MainMenu);
           newMenu.Caption := shaderName;
           newMenu.OnClick := @ScriptingTemplatesMenuClick;
           ScriptingTemplatesMenu.Add(newMenu);
       end;
    end;
  finally
    shaderNames.Free;
  end;
  {$IFDEF MATT1}
  StoreFMRIMenu.Visible := true;
  {$ENDIF}
  {$IFDEF Darwin}
  SetDarkMode();
  HelpPrefMenu.Visible := false; //use apple menu
  HelpAboutMenu.Visible := false; //use apple menu
  FileSepMenu.Visible := false;
  FileExitMenu.Visible := false;
  ScriptingRunMenu.ShortCut := ShortCut(Word('R'), [ssModifier]); //ssCtrl -> ssMeta
  OpenMenu.ShortCut := ShortCut(Word('O'), [ssModifier]); //ssCtrl -> ssMeta
  AddOverlayMenu.ShortCut := ShortCut(Word('A'), [ssModifier]); //ssCtrl -> ssMeta
  DrawHideMenu.ShortCut := ShortCut(Word('H'), [ssModifier]); //ssCtrl -> ssMeta
  ScriptingNewMenu.ShortCut  := ShortCut(Word('N'), [ssModifier]); //ssCtrl -> ssMeta
  DrawUndoMenu.ShortCut  := ShortCut(Word('U'), [ssModifier]);
  DrawCloneMenu.ShortCut  := ShortCut(Word('Z'), [ssModifier]);
  EditCopyMenu.ShortCut  := ShortCut(Word('C'), [ssModifier]);
  DrawNoneMenu.ShortCut  := ShortCut(Word('D'), [ssModifier]);
  DrawEraseMenu.ShortCut  := ShortCut(Word('E'), [ssModifier]);
  DrawRedMenu.ShortCut  := ShortCut(Word('1'), [ssModifier]);
  DrawGreenMenu.ShortCut  := ShortCut(Word('2'), [ssModifier]);
  DrawBlueMenu.ShortCut  := ShortCut(Word('3'), [ssModifier]);
  YokeMenu.ShortCut:= ShortCut(Word('Y'), [ssModifier]); ;
  {$ELSE}
  AppleMenu.Visible := false;
  {$ENDIF}
   {$IFDEF COMPILEYOKE}
   CreateSharedMem(self);
   SetShareFloats2D(0,0,0);
   SetShareFloats2D(0,0,0); //twice so previous is set
   SetShareFloats3D(Vol1.Azimuth, Vol1.Elevation);
   SetShareFloats3D(Vol1.Azimuth, Vol1.Elevation); //twice so previous is set
   {$ENDIF}
  //show details for layer
  SetDisplayCheck();
  UpdateLayerBox(true);
  UpdateVisibleBoxes();
  UpdateOpenRecent();
  QualityTrack.Position:= gPrefs.Quality1to10;
  {$IFNDEF METALAPI}
  if gPrefs.InitScript <> '' then
     UpdateTimer.Enabled:=true;
  ViewGPUPrepare(Sender);
  {$ENDIF}
end;

procedure TGLForm1.ViewGPUPrepare(Sender: TObject);
begin
  {$IFDEF METALAPI}
  isPrepared := true;
  ViewGPU1.SetPreferredFrameRate(0);
  Vol1.Prepare(ShaderDir+ pathdelim + ShaderDrop.Items[0]+kExt);
  setShaderSliders;
  //ViewGPU1.Invalidate;
  //GLForm1.OnResize := @FormResize;
  ViewGPU1.InvalidateOnResize := true;
  UpdateTimer.Tag := 1;
  UpdateTimer.Enabled:=true;
  {$ENDIF}
  gClrbar:= TGPUClrbar.Create(ViewGPU1);
  Vol1.SetColorBar(gClrBar);
  gClrbar.isVisible := gPrefs.ColorbarVisible;
  VisibleClrbarMenu.checked := gPrefs.ColorbarVisible;
  TextAndCubeMenu.Checked := gPrefs.LabelOrient;
  SetColorBarPosition;
  gClrbar.SizeFraction := gPrefs.ColorbarSize/1000;
  UpdateColorbar();
  Vol1.SetTextContrast(gPrefs.ClearColor);
  Vol1.Slices.RadiologicalConvention := gPrefs.FlipLR_Radiological;
  Vol1.Slices.LabelOrient := gPrefs.LabelOrient;
  Vol1.Quality1to10:= gPrefs.Quality1to10;
  Vol1.Slices.LineWidth := gPrefs.LineWidth;
  LineWidthEdit.Value := gPrefs.LineWidth;
end;

procedure TGLForm1.ViewGPUPaint(Sender: TObject);
var
   niftiVol: TNIfTI;
begin
  if isBusy then exit;
  if not vols.Layer(0,niftiVol) then exit;
 {$IFDEF METALAPI}
 if not isPrepared then ViewGPUPrepare(Sender);
 MTLSetClearColor(MTLClearColorMake(gPrefs.ClearColor.r/255, gPrefs.ClearColor.g/255, gPrefs.ClearColor.b/255, 1));
 {$ELSE}
 glClearColor(gPrefs.ClearColor.R/255, gPrefs.ClearColor.G/255, gPrefs.ClearColor.B/255, 1.0);
 {$ENDIF}
  if gPrefs.DisplayOrient = kMosaicOrient then
       Vol1.PaintMosaic2D(niftiVol, vols.Drawing, gPrefs.MosaicStr)
 else if gPrefs.DisplayOrient = kRenderOrient then //render view
    Vol1.Paint(niftiVol)
 else
     Vol1.Paint2D(niftiVol, vols.Drawing, gPrefs.DisplayOrient);
end;

procedure TGLForm1.FormDestroy(Sender: TObject);
begin
 {$IFDEF COMPILEYOKE}
   YokeTimer.Enabled := false;
   CloseSharedMem;
  {$ENDIF}
  AnimateTimer.Enabled := false;
  UpdateTimer.Enabled := false;
  vols.CloseAllLayers;
  vols.Free;
  gLandmark.Free;
end;

end.

