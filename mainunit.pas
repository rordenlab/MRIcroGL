unit mainunit;
//{$mode delphi}
{$mode delphi}{$H+}
{$IFDEF LCLCocoa}
 //MetalAPI supported on modern MacOS: disable for Linux, Windows and old MacOS
   //{$DEFINE METALAPI} //set in ProjectOptions/CompilerOptions/CustomOptions: -dMETALAPI
   //{$modeswitch objectivec1}
{$modeswitch objectivec1}
   {$DEFINE NewCocoa}
   //{$DEFINE DARKMODE}
{$ENDIF}
{$IFNDEF METALAPI}
{$WARN 5023 off : Unit "$1" not used in $2}
 {$include ../Metal-Demos/common/glopts.inc}
{$ENDIF}
{$IFDEF LCLCarbon}
  error: you must compile for the Cocoa widgetset (ProjectOptions/Additions&Overrides)
{$ENDIF}

{$DEFINE MATCAP}
{$DEFINE COMPILEYOKE} //use yoking
{$DEFINE CLRBAR} //provide color bar
{$DEFINE GRAPH} //timeseries viewer
{$DEFINE AFNI} //afni statistical maps                                             \

{$WARN 5024 OFF} //disable warnings about unused parameters
{$WARN 5043 off : Symbol "$1" is deprecated}
{$DEFINE MATT1}

{$IFDEF MYPY}{$IFNDEF PY4LAZ}
  {$ifdef darwin}
    {$ifdef CPUAARCH64}
      {$linklib ./PythonBridge/aarch64-darwin/libpython3.7m.a}
    {$else}
      {$linklib ./PythonBridge/x86_64-darwin/libpython3.7m.a}
    {$endif}
  {$endif}
  {$ifdef linux}
    {$linklib c}
    {$linklib m}
    {$linklib pthread}
    {$linklib util}
    {$linklib dl}
    {$linklib ./PythonBridge/x86_64-linux/libpython3.7m.a}
  {$endif}
{$ENDIF}{$ENDIF}

interface
{$include opts.inc} //for  DEFINE MOSAICS
uses
  {$IFDEF MATT1}umat, {$ENDIF}
  {$IFDEF METALAPI}
  	//{$IFDEF DEPTHPICKER}MetalUtils,{$ENDIF}
  	MetalPipeline,  Metal,MetalControl, mtlvolume2,
  	{$IFDEF GRAPH}mtlgraph,{$ENDIF}
  	{$IFDEF CLRBAR}mtlclrbar, {$ENDIF}
  {$ENDIF}
  {$IFDEF COMPILEYOKE} yokesharemem, {$ENDIF}
  {$IFDEF Linux} LazFileUtils, {$ENDIF}
  {$IFDEF AFNI} nifti_foreign, afni_fdr, {$ENDIF}
  {$IFDEF MYPY}
    {$IFDEF PY4LAZ}
      PythonEngine, //use PythonForLazarus
    {$ELSE}
      Python3Core, PythonBridge, //use PythonPascalBridge
    {$ENDIF}
  {$ENDIF} //if MYPY
  {$IFDEF Darwin} MacOSAll, CocoaAll,{$ENDIF}
  {$IFDEF LCLCocoa}SysCtl, {$IFDEF DARKMODE}nsappkitext, {$ENDIF}{$IFDEF NewCocoa} UserNotification,{$ENDIF} {$ENDIF}
  {$IFDEF UNIX}Process,{$ELSE} Windows,{$ENDIF}
  {$WARN 5024 OFF}LazVersion,{$WARN 5024 ON}
  ctypes, resize, ustat,  tiff2nifti, //LCLMessageGlue,
  lcltype, GraphType, Graphics, dcm_load, crop, intensityfilter,
  LCLIntf, slices2D, StdCtrls, SimdUtils, Classes, SysUtils, Forms, Controls,clipbrd,
  Dialogs, Menus, ExtCtrls, CheckLst, ComCtrls, Spin, Types, fileutil, ulandmarks, nifti_types,
  nifti_hdr_view, fsl_calls, math, nifti, niftis, prefs, dcm2nii, strutils, drawVolume, autoroi, VectorMath, LMessages;

const
  {$IFDEF FASTGZ}
  kVers = '1.2.20210816';
  {$ELSE}
  kVers = '1.2.20210816+';
  {$ENDIF}
type

  { TGLForm1 }

  TGLForm1 = class(TForm)
    CenterPanel: TPanel;
    ClipThickLabel: TLabel;
    ClipThickTrack: TTrackBar;
    ClusterView: TListView;
    GraphMenu: TMenuItem;
    GraphScalingMenu: TMenuItem;
    GraphSaveMenu: TMenuItem;
    GraphOpenMenu: TMenuItem;
    GraphMarkerMenu: TMenuItem;
    GraphRawMenu: TMenuItem;
    GraphDemeanMenu: TMenuItem;
    GraphNormalizeMenu: TMenuItem;
    GraphNormalize01Menu: TMenuItem;
    GraphPercentMenu: TMenuItem;
    GraphAddMenu: TMenuItem;
    GraphShowHideMenu: TMenuItem;
    GraphClearMenu: TMenuItem;
    GraphSaveBitmapMenu: TMenuItem;
    DisplayCorrelationR: TMenuItem;
    DisplayCorrelationZ: TMenuItem;
    AfniPrevMenu: TMenuItem;
    AfniPopup: TPopupMenu;
    LayerAfniBtn: TButton;
    AfniNextMenu: TMenuItem;
    LayerAfniDrop: TComboBox;
    MatCapDrop: TComboBox;
    CropMenu1: TMenuItem;
    EditPasteMenu: TMenuItem;
    AddOverlayClusterMenu: TMenuItem;
    GraphDrawingMenu: TMenuItem;
    DrawFilledMenu: TMenuItem;
    LayerIsLabelMenu: TMenuItem;
    AfniDetailsMenu: TMenuItem;
    AfniDirMenu: TMenuItem;
    LayerClusterMenu: TMenuItem;
    ClusterCopyMenu: TMenuItem;
    ClusterSaveMenu: TMenuItem;
    GraphSaveTxtMenu: TMenuItem;
    GraphSaveBmpMenu: TMenuItem;
    LayerFindPeakMenu: TMenuItem;
    AfniPMenu: TMenuItem;
    AfniQMenu: TMenuItem;
    GraphOpenAddMenu: TMenuItem;
    LayerClusterOptsMenu: TMenuItem;
    ImportTIFFFolderMenu: TMenuItem;
    LayerClusterMeansMenu: TMenuItem;
    DrawIntensityFilterMenu: TMenuItem;
    DrawDilateMenu: TMenuItem;
    LayerExport8BitMenu: TMenuItem;
    DicomDirMenu: TMenuItem;
    EdgeMenu2: TMenuItem;
    MPR4Menu: TMenuItem;
    ScriptHaltMenu: TMenuItem;
    NimlMenu: TMenuItem;
    OpenAFNIMenu: TMenuItem;
    ClusterPopUp: TPopupMenu;
    GraphPanel: TPanel;
    ClusterPanel: TPanel;
    GraphPopUp: TPopupMenu;
    RemoveSmallClusterMenu: TMenuItem;
    ResizeMenu1: TMenuItem;
    ReorientMenu1: TMenuItem;
    InvalidateTImer: TTimer;
    RulerCheck: TCheckBox;
    BetterRenderTimer: TTimer;
    ClusterGraphSplitter: TSplitter;
    ToolsMenu: TMenuItem;
    OpenAltasMenu: TMenuItem;
    TBSplitter: TSplitter;
    LayerWidgetChangeTimer: TTimer;
    TopPanel: TPanel;
    BottomPanel: TPanel;
    AnatDrop: TComboBox;
    CreateOverlapImageMenu: TMenuItem;
    CreateSubtractionPlotMenu: TMenuItem;
    LayerZeroIntensityInvisibleMenu: TMenuItem;
    LayerInvertColorMapMenu: TMenuItem;
    Smooth2DCheck: TCheckBox;
    ZoomBtn: TButton;
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
    MaskMenu: TMenuItem;
    MaskDeleteMenu: TMenuItem;
    MaskPreserveMenu: TMenuItem;
    DrawHintsMenu: TMenuItem;
    ImportTIFFMenu: TMenuItem;
    RemoveHazeOptionsMenu: TMenuItem;
    SaveNIfTIMenu: TMenuItem;
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
    SliceZoom: TTrackBar;
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
    procedure AfniPMenuClick(Sender: TObject);
    procedure AfniQMenuClick(Sender: TObject);
    procedure CenterPanelClick(Sender: TObject);
    procedure DicomDirMenuClick(Sender: TObject);
    procedure DrawIntensityFilterMenuClick(Sender: TObject);
    procedure EdgeMenuClick(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
    function HasLabelLayer: boolean;
    procedure ClusterSaveClick(Sender: TObject);
    procedure ClusterViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure ClusterViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure AddOverlayClusterMenuClick(Sender: TObject);
    procedure AfniDetailsMenuClick(Sender: TObject);
    procedure AfniDirMenuClick(Sender: TObject);
    procedure BetterRenderTimerTimer(Sender: TObject);
    procedure ClusterByMenuClick(Sender: TObject);
    procedure ClusterViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ClusterViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure EditPasteMenuClick(Sender: TObject);
    function DefaultImage():string;
    procedure FormCreate(Sender: TObject);
    procedure GraphPanelResize(Sender: TObject);
    procedure ImportTIFFFolderMenuClick(Sender: TObject);
    procedure InvalidateTImerTimer(Sender: TObject);
    procedure LayerAfniBtnClick(Sender: TObject);
    procedure LayerAfniDropClick(Sender: TObject);
    procedure LayerClusterMenuClick(Sender: TObject);
    procedure LayerContrastChange(Sender: TObject);
    procedure GraphDrawingMenuClick(Sender: TObject);
    procedure LayerExport8BitMenuClick(Sender: TObject);
    procedure LayerFindPeakMenuClick(Sender: TObject);
    procedure LayerIsLabelMenuClick(Sender: TObject);
    procedure LayerSmoothMenuClick(Sender: TObject);
    procedure DrawDilateMenuClick(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure NimlMenuClick(Sender: TObject);
    procedure RemoveSmallClusterMenuClick(Sender: TObject);
    procedure RulerVisible();
    procedure RulerCheckChange(Sender: TObject);
    procedure ScriptHaltMenuClick(Sender: TObject);
    procedure ScriptingPyVersionClick(Sender: TObject);
    procedure ScriptingSepMenuClick(Sender: TObject);
    procedure UpdateCropMask(msk: TVec6);
    procedure CreateOverlapImageMenuClick(Sender: TObject);
    procedure CreateSubtractionPlotMenuClick(Sender: TObject);
    procedure ApplyCrop(crop: TVec6i; cropVols: TPoint);
    procedure CropMenuClick(Sender: TObject);
    procedure DisplayCorrelationRClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GraphMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GraphMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    procedure GraphAddMenuClick(Sender: TObject);
    procedure GraphClearMenuClick(Sender: TObject);
    procedure GraphMarkerMenuClick(Sender: TObject);
    procedure GraphOpenMenuClick(Sender: TObject);
    function GraphOpen(fnm: string; isKeepOldGraph: boolean = false; isShowMessageOnError: boolean = false): boolean;
    procedure GraphSaveBitmapMenuClick(Sender: TObject);
    procedure GraphSaveMenuClick(Sender: TObject);
    procedure GraphScaleClick(Sender: TObject);
    procedure GraphShowHideMenuClick(Sender: TObject);
    procedure LayerInvertColorMapMenuClick(Sender: TObject);
    procedure LayerWidgetChangeTimerTimer(Sender: TObject);
    procedure LayerZeroIntensityInvisibleMenuClick(Sender: TObject);
    procedure MatCapDropChange(Sender: TObject);
    procedure ReorientMenuClick(Sender: TObject);
    procedure ReportPositionXYZ(isUpdateYoke: boolean = false);
    procedure AnatAddBtnClick(Sender: TObject);
    procedure AnatDeleteBtnClick(Sender: TObject);
    procedure AnatDropChange(Sender: TObject);
    procedure AnatSaveBtnClick(Sender: TObject);
    procedure AnatUpdate();
    function OpenDialogExecute (lFilter,lCaption: string): TStringList;
    function OpenDialogExecute1 (lFilter,lCaption: string): String;
    procedure AnatOpenBtnClick(Sender: TObject);
    procedure AnatUpdateBtnClick(Sender: TObject);
    procedure AnimateTimerTimer(Sender: TObject);
    procedure CreateStandardMenus (ParentMenu: TMenuItem);
    procedure DisplayAnimateMenuClick(Sender: TObject);
    function Active4DLayer: integer;
    function DisplayNextMenuClick(Sender: TObject): boolean;
    procedure DrawHintsMenuClick(Sender: TObject);
    procedure EditCopyMenuClick(Sender: TObject);
    procedure UpdateColorBar;
    procedure ExitFullScreenMenuClick(Sender: TObject);
    procedure FileExitMenuClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure ImportTIFFMenuClick(Sender: TObject);
    procedure LandmarkSelectNextClick(Sender: TObject);
    procedure LayerCloseMenuClick(Sender: TObject);
    procedure AfniNextClick(Sender: TObject);
    procedure LayerListClickCheck(Sender: TObject);
    procedure LayerListShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure LayerMaskWithBackgroundMenuClick(Sender: TObject);
    procedure LayerOptionsBtnClick(Sender: TObject);
    procedure LayerShowBidsMenuClick(Sender: TObject);
    procedure LayerShowHeaderMenuClick(Sender: TObject);
    procedure LayerVolumeChange(Sender: TObject);
    procedure MaskMenuClick(Sender: TObject);
    procedure MouseGesturesMenu(Sender: TObject);
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
    procedure ResizeMenuClick(Sender: TObject);
    procedure SaveMosaicBmp(bmpName: string);
    procedure DrawSaveMenuClick(Sender: TObject);
    function Save2Bmp(fnm: string): boolean;
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
    function NiftiSaveDialogFilename(isVOI: boolean = false; initialFilename: string = ''): string;
    procedure SaveNIfTIMenuClick(Sender: TObject);
    procedure ClipIntensity(Lo, Hi: single);
    procedure ScriptMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ScriptPanelDblClick(Sender: TObject);
    procedure SetColorBarPosition;
    procedure SliceZoomChange(Sender: TObject);
    procedure SmoothCheckChange(Sender: TObject);
    procedure SmoothMenuClick(Sender: TObject);
    procedure StoreFMRIMenuClick(Sender: TObject);
    procedure TextAndCubeMenuClick(Sender: TObject);
    procedure ViewGPUgPrepare(Sender: TObject);
    procedure ToolPanelDblClick(Sender: TObject);
    procedure SaveColorTable;
    procedure ClrbarVisibleClick(Sender: TObject);
    procedure CoordEditChange(Sender: TObject);
    procedure CutNearBtnClick(Sender: TObject);
    procedure CutNoneBtnClick(Sender: TObject);
    {$IFDEF DARKMODE}
    procedure SetFormDarkMode(f: TForm);
    procedure SetDarkMode();
    {$ENDIF}
    procedure SetToolPanelMaxWidth();
    function ClusterOpen(fnm: string): boolean;
    procedure DisplayViewMenu(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
    procedure MakeList(var c: TClusters);
    procedure UpdateLayerBox(NewLayers: boolean; NewVolumes: boolean = false);
    function AddLayer(Filename: string): boolean;
    procedure UpdateContrast (Xa,Ya, Xb, Yb: integer);
    function AddBackground(Filename: string; isAddToRecent: boolean = true; isFromFormDrop: boolean = false): boolean;
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
    procedure GraphPaint(Sender: TObject);
    procedure GraphShow();
    procedure ShaderDropChange(Sender: TObject);
    procedure SharpenMenuClick(Sender: TObject);
    //procedure SplitterMoved(Sender: TObject);
    procedure SaveMenuClick(Sender: TObject);
    procedure SetXHairPosition (lXmm,lYmm,lZmm: single; isUpdateYoke: boolean = false);
    procedure UpdateShaderSettings(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure ViewGPUMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ViewGPUMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewGPUMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OpenMenuClick(Sender: TObject);
    //procedure ShaderMenuClick(Sender: TObject);
    //procedure LayerColorDropChange(Sender: TObject);
    procedure PickRenderDepth( X, Y: Integer);
    procedure ViewGPUMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ParamStr2Script();
    procedure ViewGPUPrepare(Sender: TObject);
    procedure ViewGPUPaint(Sender: TObject);
    procedure ViewGPUDblClick(Sender: TObject);
    procedure ViewGPUKeyPress(Sender: TObject; var Key: char);
    procedure ViewGPUKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ViewGPUKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ViewGPUgKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ViewGPUgResize(Sender: TObject);
    procedure setShaderSliders;
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

    {$IFDEF PY4LAZ}
    procedure PyEngineAfterInit(Sender: TObject);
    procedure PyIOSendData(Sender: TObject; const Data: AnsiString);
    procedure PyIOSendUniData(Sender: TObject; const Data: UnicodeString);
    procedure PyModInitialization(Sender: TObject);
    {$ELSE}
    procedure GotPythonData(str: UnicodeString);
    {$ENDIF}
    {$ENDIF}
    procedure YokeMenuClick(Sender: TObject);
    procedure YokeTimerTimer(Sender: TObject);
    procedure voiUndo(isRefresh: boolean = false);
    procedure MorphologyFill(Origin: TVec3; dxOrigin, radiusMM: int64; drawMode: int64);
    procedure DrawIntensityFilter(rampAbove, rampBelow, drawMode: integer);
    procedure ForceOverlayUpdate();
    procedure ZoomBtnClick(Sender: TObject);
    {$IFDEF DEPTHPICKER}{$IFDEF METALAPI}
    //function MTLCopyLastFrameTextureX(texture: MTLTextureProtocol; hasAlpha: boolean): CGImageRef;
    {$ENDIF}{$ENDIF}
  private
    //
  end;

  var
  GLForm1: TGLForm1;

implementation

{$R *.lfm}

{$IFDEF METALAPI}
const  kExt = '.metal';
{$ELSE}
uses
 {$IFDEF GRAPH}glgraph,{$ENDIF}
 {$IFDEF COREGL}glcorearb,{$ELSE}gl,glext,{$ENDIF}
 {$IFDEF LCLCocoa}glcocoanscontext,{$ENDIF}{$IFDEF CLRBAR}glclrbar, {$ENDIF}
 {$IFDEF RETINA}retinahelper,{$ENDIF}
 OpenGLContext,  glvolume2, gl_core_utils {$IFDEF MYPY}{$IFDEF PY4LAZ}{$IFNDEF UNIX}, proc_py {$ENDIF}{$ENDIF}{$ENDIF};
const kExt = '.glsl';
{$ENDIF}
const
     kNaNsingle: single = 1/0;
     kNaN : double = 1/0;
  {$IFDEF UNIX} //end of line
  kEOLN = #10;
  {$ELSE}
   kEOLN = #13#10; //Windows CRLF
  {$ENDIF}
  kTab = #9;
var
 gPrefs: TPrefs;
 gPyRunning: boolean = false;
 gMouseLimitLo, gMouseLimitHi : TPoint;
 gMouse : TPoint = (x: -1; y: -1);
 gMouseDrag: boolean = false;
 gSliceMM : TVec3 = (x: 0; y: 0; z: 0);
 Vol1: TGPUVolume;
 isBusy: boolean = false;
 isBusyCore: boolean = false;
 vols: TNIfTIs;
 gLandmark : TLandmark;
 {$IFDEF METALAPI}
 //isFormShown: boolean = false;
 isPrepared: boolean = false;
 ViewGPU1: TMetalControl;
 {$IFDEF GRAPH}ViewGPUg: TMetalControl;{$ENDIF}
 {$ELSE}
 ViewGPU1: TOpenGLControl;
  {$IFDEF GRAPH}ViewGPUg: TOpenGLControl;{$ENDIF}
 {$ENDIF}
  {$IFDEF GRAPH}gGraph: TGPUGraph;{$ENDIF}
 {$IFDEF CLRBAR}gClrbar: TGPUClrbar;{$ENDIF}

 {$IFDEF LCLCocoa}{$IFNDEF METALAPI}
{$DEFINE isCocoaOpenGL}
{$ENDIF}{$ENDIF}

{$IFDEF DBUG}
procedure DebugLn(str: string);  //2022
begin
{$IFDEF UNIX}
        writeln(str);
{$ENDIF}
end;
{$ENDIF}

procedure GenerateClustersCore(var v: TNIfTI; thresh, mm: single; method: integer; isDarkAndBright: boolean);
var
  i: integer = 0;
  v2: TNIfTI;
begin
  GLForm1.ClusterView.tag := -1;
  if (v.IsLabels) or (GLForm1.LayerList.Count < 2) then begin
    //either atlas or only image loaded
    v.GenerateClusters(thresh, mm, gPrefs.ClusterNeighborMethod, isDarkAndBright);
    //GLForm1.ClusterView.visible := true;
    exit;
  end;
  //not an atlas... see if one is loaded
  v2 := nil;
  while (i < GLForm1.LayerList.Count) and (v2 = nil) do begin
    if not vols.Layer(i,v2) then exit;
    if not v2.IsLabels then
       v2 := nil;
    i := i + 1;
  end;
  v.GenerateClusters(v2, thresh, mm, method, isDarkAndBright); //v2 is either nil or a label map
  //GLForm1.ClusterView.visible := (v2 <> nil);
end;


var isDescending: boolean = false;
procedure TGLForm1.ClusterViewColumnClick(Sender: TObject; Column: TListColumn);
//https://www.delphitips.net/2008/04/10/sort-listview-by-clicking-on-columns/
var
 i: integer;
begin
  ClusterView.SortColumn := Column.Index;
  //ClusterView.AutoSortIndicator:= true;
  //Descending := not Descending;
  isDescending := not isDescending;
  //{$ifndef windows} //next lines require trunk or Lazarus >= 2.0.8
  //{$IFDEF (LCL_FULLVERSION > 2080600)}
  {$IF laz_fullversion >= 2000800}
  for i := 0 to (ClusterView.ColumnCount -1) do begin
      if (i = Column.Index) then continue;
      ClusterView.Column[i].SortIndicator:= siNone;
  end;
  if isDescending then
     ClusterView.Column[Column.Index].SortIndicator:= siDescending
  else
      ClusterView.Column[Column.Index].SortIndicator:= siAscending;
  {$ENDIF}
  //ClusterView.SortType := stData;   //so
  if ClusterView.SortDirection = sdDescending then
     ClusterView.SortDirection := sdAscending
  else
      ClusterView.SortDirection := sdDescending;
end;

procedure TGLForm1.ClusterSaveClick(Sender: TObject);
var
   contents,row: ansistring;
   column: TListColumn;
   item: TListItem;
   i, j: integer;
   dlg : TSaveDialog;
   Txt: TextFile;
   niftiVol: TNIfTI;
begin
  contents := '';
  row := '';
  contents := 'MRIcroGL '+kVers+kEOLN;
  //if vols.Layer(GLForm1.LayerList.ItemIndex,niftiVol) then
  if vols.Layer(ClusterView.tag,niftiVol) then
     contents += 'Notes: '+niftiVol.clusterNotes+kEOLN;
  // header
  for i := 0 to ClusterView.Columns.Count - 1 do
      begin
          column := TListColumn(ClusterView.Columns[i]);
          if not column.Visible then
              continue;
          row += column.Caption;
          if i < ClusterView.Columns.Count - 1 then
              row += kTab;
      end;
  contents += row+kEOLN;
  // items
  for i := 0 to ClusterView.Items.Count - 1 do
      begin
          item := TListItem(ClusterView.Items[i]);
          row := '';
          for j := 0 to item.SubItems.Count do
              begin
                  column := TListColumn(ClusterView.Columns[j]);
                  if not column.Visible then
                      continue;
                  if j = 0 then
                      row += item.Caption
                  else
                      row += item.SubItems[j - 1];
                  if j < item.SubItems.Count then
                      row += kTab;
              end;
          contents += row;
          if i < ClusterView.Items.Count - 1 then
              contents += kEOLN;
      end;
  if (Sender as TMenuItem).tag = 0 then begin
     Clipboard.AsText:= contents;
     exit;
  end;
  dlg := TSaveDialog.Create(self);
  dlg.Title := 'Save table';
  dlg.InitialDir := GetUserDir;//GetUserDir;
  {$IFDEF Darwin}
  if PosEx('.app', dlg.InitialDir) > 0  then
     dlg.InitialDir := HomeDir(false);
  {$ENDIF}
  dlg.Filter := 'Text|*.txt';
  dlg.DefaultExt := '.txt';
  dlg.FilterIndex := 0;
  if not dlg.Execute then begin
   dlg.Free;
   exit;
  end;
  AssignFile(Txt, dlg.FileName);
  Rewrite(Txt);
  WriteLn(Txt, contents);
  CloseFile(Txt);
  dlg.Free;
end;

{$IFDEF AFNI}
function AFNIjvLabel(statCode: integer): string;
begin
     case statCode of
        kFUNC_COR_TYPE: result := 'Correl';//// Correlation Coeff # Samples, # Fit Param, # Orts
        kFUNC_TT_TYPE: result := 'T';//  Student t         Degrees-of-Freedom (DOF)
        kFUNC_FT_TYPE: result := 'F';//  F ratio           Numerator DOF, Denominator DOF
        kFUNC_ZT_TYPE: result := 'Normal';//  Standard Normal   -- none --
        kFUNC_CT_TYPE: result := 'Chi';//  Chi-Squared       DOF
        kFUNC_BT_TYPE: result := 'Beta';//  Incomplete Beta   Parameters "a" and "b"
        kFUNC_BN_TYPE: result := 'Binomial';//            # Trials, Probability per trial
        kFUNC_GT_TYPE: result := 'Gamma';//               Shape, Scale
        kFUNC_PT_TYPE: result := 'Poisson';
        else result := 'Other'
    end;
end;
{$ENDIF}

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
  {$IFDEF DARKMODE}
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

procedure TGLForm1.AfniPMenuClick(Sender: TObject);
{$IFDEF AFNI}
var
   v: TNIfTI;
  jv: integer;
  s: string;
  df0, df1, p, thresh: double;
begin

  vols.Layer(LayerList.ItemIndex,v);
  if v = nil then exit;
  if length(v.afnis) < v.VolumeDisplayed then exit;
  jv := v.afnis[v.VolumeDisplayed].jv;
  s := AFNIjvLabel(jv);
  if (jv <> kFUNC_TT_TYPE) and (jv <> kFUNC_FT_TYPE) then begin
     showmessage('Current version only supports p-values for T and F tests, not "'+s+'"');
     exit;
  end;
  df0 := v.afnis[v.VolumeDisplayed].param[0];
  df1 := v.afnis[v.VolumeDisplayed].param[1];
  if jv = kFUNC_TT_TYPE then
     s := format('%s (%d)', [s, round(df0)])
  else
      s := format('%s (%d,%d)', [s, round(df0), round(df1)]);

  p := GetFloat('Threshold at uncorrected p-value for '+s, 0,0.05,0);
  if jv = kFUNC_TT_TYPE then begin
     p := p / 2; //2 tails
     thresh := inverset(p, df0); //pTdistrInv(round(df0), p);
  end else
      thresh := inversef(p, round(df0), round(df1));//pFdistrInv(round(df0), round(df1), p);
  LayerChange(LayerList.ItemIndex, -1, -1, thresh, thresh);
end;
{$ELSE}
begin
     showmessage('Not compiled with AFNI support');
end;
{$ENDIF}

procedure TGLForm1.AfniDetailsMenuClick(Sender: TObject);
var
   s: string;
   v: TNIfTI;
   i: integer;

begin
  {$IFDEF AFNI}
  vols.Layer(LayerList.ItemIndex,v);
  if v = nil then exit;
  if length(v.afnis) < v.VolumeDisplayed then exit;
  s := 'Statistical code: '+AFNIjvLabel(v.afnis[v.VolumeDisplayed].jv);
  if (v.afnis[v.VolumeDisplayed].nv > 0) then begin
     s := s + kEOLN +'  Parameters:';
     for i := 0 to v.afnis[v.VolumeDisplayed].nv - 1 do
         s := s + ' '+floattostr(v.afnis[v.VolumeDisplayed].param[i]);
  end;
  s := s + kEOLN + format('Volume intensity %.5f..%.5f', [v.afnis[v.VolumeDisplayed].minVal, v.afnis[v.VolumeDisplayed].maxVal ]);
  showmessage(s);
  {$ENDIF}
end;

procedure TGLForm1.AfniQMenuClick(Sender: TObject);
{$IFDEF AFNI}
var
  v: TNIfTI;
  i: integer;
  q, thresh: double;
begin
  vols.Layer(LayerList.ItemIndex,v);
  if v = nil then exit;
  i := v.VolumeDisplayed;
  if length(v.afnis) < i then exit;
  if (length(v.afnis[i].FDRcurv.ar) < 20) then begin
     showmessage('This volume does not include an FDR curve');
  end;
  q := GetFloat('Threshold at FDR-corrected q-value (e.g. q=0.05 for 1 in 20 false alarms)', 0,0.05,0);
  thresh := q2VoxelIntensity(v.afnis[i].FDRcurv, q, v.afnis[i].maxAbsVal, true);
  LayerChange(LayerList.ItemIndex, -1, -1, thresh, thresh);
end;
{$ELSE}
begin
     showmessage('Not compiled with AFNI support');
end;
{$ENDIF}

procedure TGLForm1.CenterPanelClick(Sender: TObject);
begin

end;

procedure TGLForm1.DicomDirMenuClick(Sender: TObject);
var
 Info : TSearchRec;
 Dir: string;
 mostRecent: TDateTime;
 FileNames: array of String ;
begin
  mostRecent := -1;
  Dir := '';
    if not DirectoryExists(gPrefs.DicomDir) then exit;
    If FindFirst (IncludeTrailingPathDelimiter(gPrefs.DicomDir)+'*',faDirectory,Info)=0 then begin
      Repeat
        if ((Info.Attr and faDirectory) = faDirectory)  and (Info.Name[1] <> '.') then begin
          if (Info.TimeStamp < mostRecent) then continue;
          mostRecent := Info.TimeStamp;
          Dir := Info.Name;
      	end;
      Until FindNext(info)<>0;
      FindClose(Info);
   end;
   if length(Dir) < 1 then exit;
   Dir :=  IncludeTrailingPathDelimiter(gPrefs.DicomDir) + Dir;
   setlength(Filenames, 1);
   Filenames[0] := Dir;
   FormDropFiles(nil, FileNames);
   Filenames := nil;
end;

procedure TGLForm1.DrawIntensityFilterMenuClick(Sender: TObject);
begin
  IntensityFilterForm.Show;
end;

procedure TGLForm1.EdgeMenuClick(Sender: TObject);
var
 niftiVol: TNIfTI;
 i: integer;
begin
  if not vols.Layer(0,niftiVol) then exit;

  if not vols.AddEdgeLayer(gPrefs.ClearColor) then exit;
  i := GLForm1.LayerColorDrop.Items.IndexOf('8RedYell'); //search is case-insensitive!
 if i > 0 then
    LayerChange(vols.NumLayers-1, i, -1, 0.1, 1.0)
 else
     LayerChange(vols.NumLayers-1, vols.NumLayers-1, -1, 0.1, 1.0);
 UpdateLayerBox(true);
 UpdateColorbar();
 //Vol1.UpdateOverlays(vols);
 UpdateTimer.Enabled:= true;
end;

procedure TGLForm1.FormChangeBounds(Sender: TObject);
begin
  {$IFDEF LCLCocoa} {$IFNDEF METALAPI}
  ViewGPU1.Invalidate;
  {$ENDIF}{$ENDIF}
end;

procedure TGLForm1.FormResize(Sender: TObject);
begin
  //LayerBox.Caption := inttostr(random(888));
end;

procedure TGLForm1.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
begin

end;



function CompareFloatText(const S1, S2: string): integer;
begin
     //result := round(StrToFloatDef(S1,0) - StrToFloatDef(S2,0));
     result := sign(StrToFloatDef(S1,0) - StrToFloatDef(S2,0));
     //writeln(format('%g %g %d',[ StrToFloatDef(S1,0), StrToFloatDef(S2,0), result]));
end;

procedure TGLForm1.ClusterViewCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
begin
  //if (ClusterView.SortType = stText) then begin
  if ClusterView.Items.Count < 2 then exit;
  if not odd(ClusterView.Columns[ClusterView.SortColumn].tag) then begin
    if ClusterView.SortColumn = 0 then
       Compare := CompareText(Item1.Caption, Item2.Caption)
    else
        Compare := CompareText(Item1.SubItems[ ClusterView.SortColumn-1], Item2.SubItems[ ClusterView.SortColumn-1]);
  end else begin
    //LayerBox.caption := inttostr(random(888));
    if ClusterView.SortColumn = 0 then
       Compare := CompareFloatText(Item1.Caption, Item2.Caption)
    else
        Compare := CompareFloatText(Item1.SubItems[ ClusterView.SortColumn-1], Item2.SubItems[ ClusterView.SortColumn-1]);
  end;
  if isDescending then
     Compare := -Compare;
end;

procedure TGLForm1.ForceOverlayUpdate();
var
    i: integer;
    niftiVol: TNIfTI;
begin
  i := LayerList.Count-1;
  //caption := inttostr(i);
  if (i < 0) then exit;
  if not vols.Layer(i,niftiVol) then exit;
  niftiVol.CX.NeedsUpdate := true;
end;

procedure TGLForm1.UpdateCropMask(msk: TVec6);
begin
  Vol1.Slices.CropMask := msk;
  ViewGPU1.Invalidate;
end;


procedure TGLForm1.ZoomBtnClick(Sender: TObject);
begin
  Vol1.Slices.ZoomCenter := Vec3(0.5, 0.5, 0.5);
  SliceZoom.Position :=  SliceZoom.min;
  //Vol1.Slices.ZoomScale := 1;
  ViewGPU1.Invalidate;
end;

procedure TGLForm1.voiUndo(isRefresh: boolean = false);
begin
 if not vols.Drawing.IsOpen then exit;
 Vols.Drawing.voiUndo();
 if isRefresh then
    ViewGPU1.Invalidate;
end;

procedure printf(str: string);
begin
     {$IFDEF UNIX} writeln(str);
     {$ELSE}
     if IsConsole then writeln(str);
     {$ENDIF}
end;

procedure pyprintf(str: string);
begin
     GLForm1.ScriptOutputMemo.Lines.Add(str);
     {$IFDEF UNIX} writeln(str);
     {$ELSE}
     if IsConsole then writeln(str);
     {$ENDIF}
end;
procedure TGLForm1.DrawIntensityFilter(rampAbove, rampBelow, drawMode: integer);
var
 niftiVol: TNIfTI;
 swp, clr: integer;
 vol8:TUInt8s;
begin
  EnsureOpenVoi();
  clr := Vols.Drawing.ActivePenColor;
  if  (clr <= 0) then clr := 1;
  if (gPrefs.DisplayOrient = kRenderOrient) or (gPrefs.DisplayOrient = kMosaicOrient) then exit;
  if not vols.Layer(0,niftiVol) then exit;
  EnsureOpenVoi();
  if drawMode = kDrawModeConstrain then begin
  	 if rampAbove > rampBelow then begin
     	swp := rampAbove;
        rampAbove := rampBelow;
        rampBelow := swp;
     end;
     drawMode := kDrawModeAppend; //Clip Image Intensity Range
  end;
  if niftiVol.Header.datatype = kDT_RGB then begin
      vol8 := niftiVol.DisplayRGBGreen;
      Vols.Drawing.voiIntensityFilter(vol8, clr, rampAbove, rampBelow, drawMode);
      vol8 := nil; //free
  end else
      Vols.Drawing.voiIntensityFilter(niftiVol.DisplayMinMax2Uint8, clr, rampAbove, rampBelow, drawMode);
  //LayerBox.caption := format('%d %d', [threshold, drawMode]);
  ViewGPU1.Invalidate;
end;

procedure TGLForm1.MorphologyFill(Origin: TVec3; dxOrigin, radiusMM: int64; drawMode: int64);
var
 niftiVol: TNIfTI;
 mm: TVec3;
 clr: integer;
 vol8:TUInt8s;
begin
  //if not Vols.Drawing.IsOpen then exit;
  EnsureOpenVoi();
  clr := Vols.Drawing.ActivePenColor;
  if  (clr < 0) then clr := 1;
  if (gPrefs.DisplayOrient = kRenderOrient) or (gPrefs.DisplayOrient = kMosaicOrient) then exit;
  if not vols.Layer(0,niftiVol) then exit;
  EnsureOpenVoi();
  mm := Vec3(niftiVol.Header.pixdim[1], niftiVol.Header.pixdim[2], niftiVol.Header.pixdim[3]);
  if niftiVol.Header.datatype = kDT_RGB then begin
      vol8 := niftiVol.DisplayRGBGreen;
      Vols.Drawing.voiMorphologyFill(vol8, clr, mm.X, mm.Y, mm.Z, Origin.X, Origin.Y, Origin.Z, dxOrigin, radiusMM, drawMode);
      vol8 := nil; //free
  end else
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

function AddNiiExt(Filename: string; changeExt: boolean): string;
//does adding ".nii" or ".nii.gz" identify an eexisting file?
begin
     result := Filename;
     if Fileexists(result) then exit;
     if not (changeExt) then exit;
     result := Filename+'.nii';
     if Fileexists(result) then exit;
     result := Filename+'.nii.gz';
     if Fileexists(result) then exit;
     result := Filename+'.HEAD';
     if Fileexists(result) then exit;
     result := Filename;
end;

function CheckParentFolders(Path, Filename: string; changeExt: boolean): string;
//does adding ".nii" or ".nii.gz" identify an eexisting file?
var
   opth, pth: string;
begin
     pth := Path;
     while (length(pth) > 0) and (DirectoryExists(pth)) do begin
           result := AddNiiExt(pth+Filename, changeExt);
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

function AtlasDir (): string;
begin
	result := ResourceDir + pathdelim + 'atlas';
end;

function GetFullPathX( Filename: string; changeExt: boolean = true): string;
// "motor" -> /home/smith/myDir/motor.nii.gz
var
   i: integer;
begin
     result := Filename;
     if (length(Filename) < 1) or (Fileexists(result)) then exit;
     result := AddNiiExt(Filename, changeExt);
     if Fileexists(result) then exit;
     {$IFDEF UNIX}
     if Filename[1] = '~' then begin
        result := AddNiiExt(ExpandFileName(Filename), changeExt);
        if Fileexists(result) then exit;
     end;
     {$ENDIF}
     //result := Filename;
     //Filename := ExtractFileName(result);
     result := AddNiiExt(GetCurrentDir + pathdelim+ Filename, changeExt);
     if Fileexists(result) then exit;
     if upcase(ExtractFileExt(Filename))= '.NII' then
        Filename := ChangeFileExt(Filename,''); //img.nii -> img (allows us to find .nii.gz
     if (length(ExtractFilePath(Filename)) > 1) then begin //path provided
        result := AddNiiExt(Filename, changeExt);
        exit;
     end;
     result := CheckParentFolders(StandardDir+pathdelim, Filename, changeExt); //ResourceDir is parent of standardDir, so we check both
     //result := CheckParentFolders(ResourceDir+pathdelim, Filename);
     if (Fileexists(result)) then exit;
     if DirectoryExists(AtlasDir) then
        result := CheckParentFolders(AtlasDir+pathdelim, Filename, changeExt);
     if (Fileexists(result)) then exit;
     if DirectoryExists(GetFSLdir) then
        result := CheckParentFolders(GetFSLdir+pathdelim, Filename, changeExt);
     if (Fileexists(result)) then exit;
     if DirectoryExists(gPrefs.AfniDir) then
        result := CheckParentFolders(gPrefs.AfniDir+pathdelim, Filename, changeExt);
     if (Fileexists(result)) then exit;

     for i := 1 to knMRU do begin
         result := CheckParentFolders(ExtractFilePath(gPrefs.PrevFilename[i]), Filename, changeExt);
         if (Fileexists(result)) then exit;
     end;
     if (Filename[1] = '-') then begin //fsleyes defaults
        if (Filename = '-std1mm') or (Filename = '--standard1mm') then
           result := GetFSLdir+pathdelim+ 'data'+pathdelim+'standard'+pathdelim+'MNI152_T1_1mm';
        if (Filename = '-std') or (Filename = '--standard') then
           result := GetFSLdir+pathdelim+ 'data'+pathdelim+'standard'+pathdelim+'MNI152_T1_2mm';
        result := AddNiiExt(result, changeExt);
        if (Fileexists(result)) then exit;
     end;
     result := Filename; //failed to find a match!
     writeln('Unable to find image: "', result,'"');
end;

function GetFullPath( Filename: string; changeExt: boolean = true): string;
begin
    result := GetFullPathX(Filename, changeExt);
    {$IFDEF Darwin}
    if Fileexists(result) and (not IsReadable(result)) then
       result := ''; //outside sandbox
    {$ENDIF}
end;

 {$IFDEF MYPY}
{$IFDEF PY4LAZ}
 var
  PythonIO : TPythonInputOutput;
  PyMod: TPythonModule;
  PyEngine: TPythonEngine = nil;

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

//{$DEFINE NEWPY} //Warning: ApplieSilicon will allow Python2.7 but not Python3.x
function searchPy(pth: string): string;
var
   searchResult : TSearchRec;
begin
    result := '';
    {$IFDEF NEWPY}
      {$IFDEF Darwin}
      if FindFirst(IncludeTrailingPathDelimiter(pth)+'libpython3*.dylib', faDirectory, searchResult) = 0 then
      {$ELSE}
      if FindFirst(IncludeTrailingPathDelimiter(pth)+'libpython3*.so', faDirectory, searchResult) = 0 then
      {$ENDIF}
    {$ELSE}
      {$IFDEF Darwin}
      if FindFirst(IncludeTrailingPathDelimiter(pth)+'libpython*.dylib', faDirectory, searchResult) = 0 then
      {$ELSE}
      if FindFirst(IncludeTrailingPathDelimiter(pth)+'libpython*.so', faDirectory, searchResult) = 0 then
      {$ENDIF}
    {$ENDIF}
    result := IncludeTrailingPathDelimiter(pth)+(searchResult.Name);
    FindClose(searchResult);
end;
{$ENDIF}

{$IFDEF Darwin}
{$IFDEF NEWPY}
{$ifdef darwin}
function findMacOSLibPython3x: string;
var
 N: integer;
 S: String;
begin
for N:= 25 to 5 do
begin
  S:= Format('/Library/Frameworks/Python.framework/Versions/3.%d/lib/libpython3.%d.dylib',
    [N, N]);
  if FileExists(S) then exit(S);
end;
exit('');
end;
{$endif}
{$ENDIF} //NewPy

function findMacOSLibPython27: string;
const
     kPth = '/System/Library/Frameworks/Python.framework/Versions/Current/lib/libpython2.7.dylib';
begin
 result := '';
 if not FileExists(kPth) then exit;
 result := kPth;
end;


{$ENDIF}

{$IFDEF LINUX}
function findFirstRecursive(const SearchPath: String; SearchMask: String): string;
//example findFirstRecursive('/usr/lib/', 'libpython3*so')
//uses LazFileUtils
var
  ret: string;
procedure find1(Dir: string);
var
   SR: TSearchRec;
begin
     if (ret <> '') or (FileIsSymlink(Dir)) then exit;
     if FindFirst(IncludeTrailingBackslash(Dir) + SearchMask, faAnyFile or faDirectory, SR) = 0 then begin
         ret := IncludeTrailingBackslash(Dir) +SR.Name;
         FindClose(SR);
         exit;
      end;
      if FindFirst(IncludeTrailingBackslash(Dir) + '*.*', faAnyFile or faDirectory, SR) = 0 then
         try
           repeat
             if ((SR.Attr and faDirectory) <> 0) and (SR.Name <> '.') and (SR.Name <> '..') then
               find1(IncludeTrailingBackslash(Dir) + SR.Name);  // recursive call!
           until FindNext(Sr) <> 0;
         finally
           FindClose(SR);
         end;
end;
begin
 ret := '';
 find1(SearchPath);
 result := ret;
end;

function findLinuxLibPython3(pthroot: string = '/usr/lib/'): string;
begin
     result := findFirstRecursive(pthroot, 'libpython3*so');
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
        {$IFDEF Darwin}
        {$IFDEF NEWPY}
        result := findMacOSLibPython3x();
        if length(result) > 0 then exit;
        {$ENDIF}
        result := findMacOSLibPython27();
        if length(result) > 0 then exit;
        {$ENDIF}
        {$IFDEF LINUX}
        result := findLinuxLibPython3();
        if length(result) > 0 then exit;
        writeln('If scripts generate "PyUnicode_FromWideChar" errors, install Python3 and reset ("-R") this software.');
        {$ENDIF}
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
  {$IFDEF NEWPY}
  if PosEx('2.7',gPrefs.PyLib) > 0 then begin
    gPrefs.PyLib := '';
    printf('Python4Lazarus version requires Python3');
  end;
  {$ENDIF}
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
     {$IFDEF UNIX}
     if (S <> '') then
        writeln('Unable to find '+S);
     writeln('Unable to find Python. Install Python3 and reset this software (-R) or set the "PyLib" in the preferences.');
     {$ENDIF}
     exit;
  end;
  if (pos('libpython2.6',S) > 0) then begin
     showmessage('Old, unsupported version of Python '+S);
     exit;
  end;
  {$IFDEF NEWPY}
  if PosEx('2.7',S) > 0 then begin
    gPrefs.PyLib := '';
    printf('Python4Lazarus version requires Python3');
  end;
  {$ENDIF}
  //S := '/Users/chris/src/MRIcroGL12/MRIcroGL.app/Contents/Frameworks/python37/libpython3.7.dylib';
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
  PyMod.OnInitialization:= PyModInitialization;
  PythonIO.OnSendData := PyIOSendData;
  PythonIO.OnSendUniData:= PyIOSendUniData;
  PyEngine.DllPath:= ExtractFileDir(S);
  PyEngine.DllName:= ExtractFileName(S);
  try
     PyEngine.LoadDll;
  except
    printf('Unable to use Python library '+gPrefs.PyLib);
    //gPrefs.PyLib := '';
    result := false;
  end;
end;

function PyString_FromString(s: PAnsiChar):PPyObject;
begin
  {$IFDEF NEWPY}
  result := GetPythonEngine.PyString_FromString(s); //PyUnicodeFromString(s);
  {$ELSE}
  result := GetPythonEngine.PyString_FromString(s);
  {$ENDIF}
end;

function PyInt_FromLong(l: integer):PPyObject;
begin
  {$IFDEF NEWPY}
  result:= GetPythonEngine.PyLong_FromLong(l);
  {$ELSE}
  result:= GetPythonEngine.PyInt_FromLong(l);
  {$ENDIF}
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

procedure TGLForm1.PyEngineAfterInit(Sender: TObject);
{$IFDEF WINDOWS}
var
  dir: string;
begin
  dir:= ExtractFilePath(Application.ExeName);
  Py_SetSysPath([ScriptDir, changefileext(gPrefs.PyLib,'.zip')], false);
end;
{$ELSE}
begin
    //
end;
{$ENDIF}
{$ELSE}
function PyInt_FromLong(l: integer):PPyObject;
begin
  result:= PyLong_FromLong(l);
end;

{$ENDIF}  //IF PY4LAZ




function PyString_AsAnsiString(obj : PPyObject): string;
begin
  {$IFDEF PY4LAZ}
  {$IFDEF NEWPY}
  result := GetPythonEngine.PyUnicode_AsWideString(obj);
  {$ELSE}
  {$Message HINT The error 'no member "PyString_AsAnsiString"' suggests you are using a version of Python4Lazarus that requires Python3}
  {$Message HINT  Solution 1: Use an older version of Python4Lazarus}
  {$Message HINT  Solution 2: Uncomment 'DEFINE NEWPY' in mainunit.pas}
  {$Message HINT  https://github.com/Alexey-T/Python-for-Lazarus/issues/25}

  result := GetPythonEngine.PyString_AsAnsiString(obj);
  {$ENDIF}
  {$ELSE} //if PY4LAZ}
  result := PyString_AsAnsiString(obj);
  {$ENDIF}
end;



function PyGUI_INPUT(Self, Args : PPyObject): PPyObject; cdecl;
var
 caption, prompt, default: string;
 n: integer;
begin
 caption := 'Input Required';
 prompt := 'Input Required';
 default := '';
 {$IFDEF PY4LAZ}with GetPythonEngine do begin {$ENDIF}
 n := PyTuple_Size(args);
 if n > 0 then
    caption := PyString_AsAnsiString(PyTuple_GetItem(Args,0));
 if n > 1 then
    prompt := PyString_AsAnsiString(PyTuple_GetItem(Args,1));
 if n > 2 then
    default := PyString_AsAnsiString(PyTuple_GetItem(Args,2));
 default := InputBox(caption,prompt,default);
 Result := PyString_FromString(PAnsiChar(default+#0));
 {$IFDEF PY4LAZ}end; {$ENDIF}
end;

function PyATLASSHOWHIDE(Self, Args : PPyObject; isHide: boolean): PPyObject; cdecl;
//https://stackoverflow.com/questions/8001923/python-extension-module-with-variable-number-of-arguments
var
  layer, i, isHide01, n, idx, maxIdx: integer;
  //f: single;
  v: TNIfTI;
  ob, obi: PPyObject;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin {$ENDIF}
  Result:= PyBool_FromLong(Ord(False));
  n := PyTuple_Size(args);
  if ((n < 1) or (n > 2))then begin
       GLForm1.ScriptOutputMemo.lines.add('atlashide: requires at least two arguments (layer, (regions)) ');
       exit;
  end;
  ob :=   PyTuple_GetItem(Args,0);
  if(ob = nil) then exit;
  if (PyNumber_Check(ob) <> 1) then begin
     GLForm1.ScriptOutputMemo.lines.add('atlashide: layer argument should be an integer');
     exit;
  end;
  layer := PyLong_AsLong(ob);
  if (layer < 0) or (layer >= vols.NumLayers) then begin
     GLForm1.ScriptOutputMemo.lines.add('atlashide: layer should be in range 0..'+inttostr(vols.NumLayers-1));
     exit;
  end;
  if not vols.Layer(layer,v) then begin
     GLForm1.ScriptOutputMemo.lines.add('atlashide: fatal error');
     exit;
  end;
  if (not v.IsLabels) or (v.fLabels.count < 1) then begin
     GLForm1.ScriptOutputMemo.lines.add('atlashide: selected layer is not an indexed label map (NIfTI header intention not Labels)');
     GLForm1.updateTimer.Enabled := true;
     exit;
  end;
  //GLForm1.ScriptOutputMemo.lines.add('atlashide:::: '+inttostr(v.fLabels.count));
  maxIdx := v.fLabels.count - 1;
  //for i := 0 to  v.fLabels.count -1 do
  //    GLForm1.ScriptOutputMemo.lines.add(inttostr(i)+':'+v.fLabels[i]);
  if (v.fLabelMask = nil) or (length(v.fLabelMask) < v.fLabels.count) then begin
     setlength(v.fLabelMask, v.fLabels.count);
     for i := 0 to  maxIdx do
            v.fLabelMask[i] := 0; //assume no masks
  end;
  Result:= PyBool_FromLong(Ord(True));
  isHide01 := 0;
  if isHide then
     isHide01 := 1;
  for i := 0 to  maxIdx do
      v.fLabelMask[i] := 1 - isHide01;
  if n < 2 then begin
     for i := 0 to  maxIdx do
         v.fLabelMask[i] := isHide01;
     v.ForceUpdate;
     exit;
  end;
  ob :=   PyTuple_GetItem(Args,1);
  if(ob = nil) then exit;
  if not (PyTuple_Check(ob)) then begin  //2nd argument of atlashide(1,(17,22)) is tuple, atlashide(1,(17)) is integer
  	 idx := PyLong_AsLong(ob);
     idx := min(idx, maxIdx);
     idx := max(0, idx);
     v.fLabelMask[idx] := isHide01;
  end else begin
    n := PyTuple_Size(ob);
    if n < 1 then exit;
    for i := 0 to (n-1) do begin
  	    obi :=   PyTuple_GetItem(ob,i);
        idx := PyLong_AsLong(obi);
        idx := min(idx, maxIdx);
        idx := max(0, idx);
        v.fLabelMask[idx] := isHide01;
    end;
  end;
  v.ForceUpdate;
  {$IFDEF PY4LAZ}end;{$ENDIF}
end; //PyATLASSHOWHIDE()

function PyATLASHIDE(Self, Args : PPyObject): PPyObject; cdecl;
begin
     result := PyATLASSHOWHIDE(Self, Args, true);
end; //PyAtlasHide

function PyATLASSHOW(Self, Args : PPyObject): PPyObject; cdecl;
begin
     result := PyATLASSHOWHIDE(Self, Args, false);
end; //PyAtlasHide

function PyATLASMAXINDEX(Self, Args : PPyObject): PPyObject; cdecl;
var
 v: TNIfTI;
 layer: integer;
begin
  Result:= PyInt_FromLong(-1);
  {$IFDEF PY4LAZ}with GetPythonEngine do begin {$ENDIF}
    if Bool(PyArg_ParseTuple(Args, 'i:atlasmaxindex', @layer)) then begin
       if (layer < 0) or (layer >= vols.NumLayers) then begin
          GLForm1.ScriptOutputMemo.lines.add('atlasmaxindex: layer should be in range 0..'+inttostr(vols.NumLayers-1));
          exit;
       end;
       if not vols.Layer(layer,v) then begin
          GLForm1.ScriptOutputMemo.lines.add('atlasmaxindex: fatal error');
          exit;
       end;
       if (not v.IsLabels) or (v.fLabels.count < 1) then begin
          GLForm1.ScriptOutputMemo.lines.add('atlasmaxindex: not a label map (NIfTI header intention or labels missing)');
          exit;
       end;

       Result:= PyInt_FromLong(v.fLabels.count-1);
    end;
    {$IFDEF PY4LAZ}end; {$ENDIF}
end;

function PyATLASLABELS(Self, Args : PPyObject): PPyObject; cdecl;
var
 v: TNIfTI;
 layer, i: integer;
 str: string;
begin
  str := '';
  Result:= PyString_FromString(PAnsiChar(str+#0));
  {$IFDEF PY4LAZ}with GetPythonEngine do {$ENDIF}
    if Bool(PyArg_ParseTuple(Args, 'i:atlaslabels', @layer)) then begin
       if (layer < 0) or (layer >= vols.NumLayers) then begin
          GLForm1.ScriptOutputMemo.lines.add('atlaslabels: layer should be in range 0..'+inttostr(vols.NumLayers-1));
          exit;
       end;
       if not vols.Layer(layer,v) then begin
          GLForm1.ScriptOutputMemo.lines.add('atlaslabels: fatal error');
          exit;
       end;
       if (not v.IsLabels) or (v.fLabels.count < 2) then begin
          GLForm1.ScriptOutputMemo.lines.add('atlaslabels: not a label map (NIfTI header intention or labels missing)');
          exit;
       end;
       for i := 1 to v.fLabels.count -1 do
       str := str + inttostr(i)+#9+v.fLabels[i]+ #10;
       //str := str + format('%d\t%s\n', [i, v.fLabels[i]]);//+ #10;
       Result:= PyString_FromString(PAnsiChar(str+#0));
    end;
end;

function PyCOLORBARSIZE(Self, Args : PPyObject): PPyObject; cdecl;
var
  D: single;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
  {$IFDEF CLRBAR}
    if Boolean(PyArg_ParseTuple(Args, 'f:colorbarsize', @D)) then
       gClrbar.SizeFraction := D;
  ViewGPU1.Invalidate;
  {$ENDIF}
 {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PyCOLOREDITOR(Self, Args : PPyObject): PPyObject; cdecl;
var
  D: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
  D := 1;

    if Boolean(PyArg_ParseTuple(Args, '|i:coloreditor', @D)) then begin
       Vol1.ShowColorEditor := (D = 1);
       GLForm1.ColorEditorMenu.checked := Vol1.ShowColorEditor;
    end;
  ViewGPU1.Invalidate;
  {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PyCOLORNODE(Self, Args : PPyObject): PPyObject; cdecl;
var
 layer, index, intensity, R,G,B, A: integer;
 vol: TNIfTI;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'iiiiiiI:colornode', @layer, @index, @intensity, @R,@G,@B, @A)) then begin
       if not vols.Layer(layer,vol) then exit;
       if (vol.CX.FullColorTable.numnodes < 2) then exit;
       if (index >= vol.CX.FullColorTable.numnodes) then begin
          pyprintf(format('index should be in the range 0..%d',[vol.CX.FullColorTable.numnodes - 1]));
          exit;
       end;
       vol.CX.ChangeNode(index, intensity,R,G,B,A);
    end;
  {$IFDEF PY4LAZ}end;  {$ENDIF}
  ViewGPU1.Invalidate;
end;

function PyCOLORNAME(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  V, i: integer;
  ret: boolean;
  niftiVol: TNIfTI;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(FALSE));
    if Boolean(PyArg_ParseTuple(Args, 'is:overlaycolorname', @V, @PtrName)) then
    begin
      StrName:= string(PtrName);
      i := GLForm1.LayerColorDrop.Items.IndexOf(StrName); //search is case-insensitive!
      ret := (i >= 0);
      Result:= PyBool_FromLong(Ord(True));
      if ret then begin
         GLForm1.LayerChange(V, i, -1, kNaNsingle, kNaNsingle); //kNaNsingle
      end else if fileexists(StrName)  and vols.Layer(V,niftiVol)then begin
         while (isBusy) or (GLForm1.Updatetimer.enabled) do
               Application.ProcessMessages;
          niftiVol.SetDisplayColorScheme(StrName, 0);
          niftiVol.CX.NeedsUpdate := true;
          niftiVol.CX.GenerateLUT();
          niftiVol.ForceUpdate();
          GLForm1.UpdateTimer.Enabled := true;
      end else
      	  Result:= PyBool_FromLong(Ord(false));
    end;
  {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PyVERSION(Self, Args : PPyObject): PPyObject; cdecl;
var
  s: string;
begin
s := kVers+{$IFDEF PY4LAZ}'Python-for-Lazarus'{$ELSE}'PythonBridge' {$ENDIF}  +' PyLib: '+gPrefs.PyLib;
{$IFDEF PY4LAZ}with GetPythonEngine do {$ENDIF}
  Result:= PyString_FromString(PChar(s));
end;

function PyLOADGRAPH(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  IsAdd: integer;
  ret: boolean;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(FALSE));
  IsAdd := 0;
    if Boolean(PyArg_ParseTuple(Args, 's|i:loadgraph', @PtrName, @IsAdd)) then
    begin
      StrName:= string(PtrName);
      if fileexists(StrName) and (not IsReadable(StrName)) then begin
         pyprintf('not permitted to read "'+StrName+'"');
         exit;
      end;
      ret := GLForm1.GraphOpen(StrName, IsAdd = 1, false);
      //ret := GLForm1.AddBackground(StrName);xxxx
      if not ret then
         GLForm1.ScriptOutputMemo.Lines.Add('unable to load graph "'+StrName+'"');
      Result:= PyBool_FromLong(Ord(ret));
    end;
  {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PyLOADIMAGE(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  ret: boolean;
begin
  {$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result := PyBool_FromLong(Ord(FALSE));
    if Boolean(PyArg_ParseTuple(Args, 's:loadimage', @PtrName)) then
    begin
      StrName:= string(PtrName);
      if fileexists(StrName) and (not IsReadable(StrName)) then begin
         pyprintf('not permitted to read "'+StrName+'"');
         exit;
      end;
      ret := GLForm1.AddBackground(StrName);
      if not ret then begin
         GLForm1.ScriptOutputMemo.Lines.Add('unable to load "'+StrName+'"');
         printf('unable to load "'+StrName+'"');
      end;
      Result:= PyBool_FromLong(Ord(ret));
    end;
  {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PyOVERLAYCLOSEALL(Self, Args : PPyObject): PPyObject; cdecl;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do   {$ENDIF}
  Result:= PyBool_FromLong(Ord(TRUE));
  vols.CloseAllOverlays;
  GLForm1.UpdateLayerBox(true);
end;

function PyOVERLAYCOUNT(Self, Args : PPyObject): PPyObject; cdecl;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do   {$ENDIF}
    Result:= PyInt_FromLong(vols.NumLayers - 1) ; //indexed from 0, e.g. background is layer 0, first overlay is 1...
end;

function PyMOSAIC(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(FALSE));
    if Boolean(PyArg_ParseTuple(Args, 's:mosaic', @PtrName)) then
    begin
      StrName:= string(PtrName);
      gPrefs.DisplayOrient := kMosaicOrient;
      GLForm1.UpdateVisibleBoxes();
      gPrefs.MosaicStr := (StrName);
      Result:= PyBool_FromLong(Ord(True));
      ViewGPU1.Invalidate;
    end;
  {$IFDEF PY4LAZ}end;   {$ENDIF}
end;

function PyAZIMUTHELEVATION(Self, Args : PPyObject): PPyObject; cdecl;
var
  A,E: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'ii:azimuthelevation', @A, @E)) then begin
      Vol1.Azimuth := A;
      Vol1.Elevation := E;
      {$IFDEF COMPILEYOKE}
      SetShareFloats3D(Vol1.Azimuth, Vol1.Elevation);
      {$ENDIF}
    end;
    ViewGPU1.Invalidate;
    {$IFDEF PY4LAZ}end;   {$ENDIF}
end;

function PyPITCH(Self, Args : PPyObject): PPyObject; cdecl;
var
  P : integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'i:pitch', @P)) then begin
      Vol1.Pitch := P;
      {$IFDEF COMPILEYOKE}
      SetShareFloats3D(Vol1.Azimuth, Vol1.Elevation);
      {$ENDIF}
    end;
    ViewGPU1.Invalidate;
    {$IFDEF PY4LAZ}end;   {$ENDIF}
end;

function PyDrawLOAD(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  Ret: boolean;
begin
  Result:= PyInt_FromLong(-1);
  {$IFDEF PY4LAZ}with GetPythonEngine do   {$ENDIF}
    if Boolean(PyArg_ParseTuple(Args, 's:drawload', @PtrName)) then
    begin
      StrName:= string(PtrName);
      if fileexists(StrName) and (not IsReadable(StrName)) then begin
         pyprintf('not permitted to read "'+StrName+'"');
         exit;
      end;
      if (vols.Drawing.IsOpen) then
         vols.Drawing.voiClose;
      ret := Vols.OpenDrawing(StrName);
      ViewGPU1.Invalidate;
      if not ret then begin
         GLForm1.ScriptOutputMemo.Lines.Add('unable to load "'+StrName+'"');
         printf('unable to load "'+StrName+'"');
      end;
      Result:= PyBool_FromLong(Ord(ret));
    end;
end;


function PyOVERLAYLOAD(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  Ret: boolean;
begin
  Result:= PyInt_FromLong(-1);
  {$IFDEF PY4LAZ}with GetPythonEngine do   {$ENDIF}
    if Boolean(PyArg_ParseTuple(Args, 's:overlayload', @PtrName)) then
    begin
      StrName:= string(PtrName);
      if fileexists(StrName) and (not IsReadable(StrName)) then begin
         pyprintf('not permitted to read "'+StrName+'"');
         exit;
      end;
      ret := GLForm1.AddLayer(StrName);
      if not ret then begin
         GLForm1.ScriptOutputMemo.Lines.Add('unable to load "'+StrName+'"');
         printf('unable to load "'+StrName+'"');
      end;
      Result:= PyBool_FromLong(Ord(ret));
    end;
end;

function PyBACKCOLOR(Self, Args : PPyObject): PPyObject; cdecl;
var
  R,G,B, A: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
  A := gPrefs.ClearColor.A;
    if Boolean(PyArg_ParseTuple(Args, 'iii:backcolor', @R,@G,@B)) then begin
       gPrefs.ClearColor:= setRGBA(R,G,B,A);
       Vol1.SetTextContrast(gPrefs.ClearColor);
    end;
  ViewGPU1.Invalidate;
  {$IFDEF PY4LAZ}end;   {$ENDIF}
end;

function PyRESETDEFAULTS(Self, Args : PPyObject): PPyObject; cdecl;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do   {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
  gPrefs.DisplayOrient := kRenderOrient;
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
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'fff:clipazimuthelevation', @D,@A,@E)) then begin
      SetTrackFrac(GLForm1.ClipDepthTrack, D);
      SetTrackPos(GLForm1.ClipAziTrack, A);
      SetTrackPos(GLForm1.ClipElevTrack, E);
    end;
  ViewGPU1.Invalidate;
  {$IFDEF PY4LAZ}end; {$ENDIF}
end;

function PyCLIPTHICK(Self, Args : PPyObject): PPyObject; cdecl;
var
  Z: single;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'f:clipthick', @Z)) then
       SetTrackFrac(GLForm1.ClipThickTrack, Z);
  ViewGPU1.Invalidate;
  {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PyCUTOUT(Self, Args : PPyObject): PPyObject; cdecl;
var
  L,A,S,R,P,I: single;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'ffffff:cutout', @L,@A,@S,@R,@P,@I)) then begin
      SetTrackFrac(GLForm1.XTrackBar, L);
      SetTrackFrac(GLForm1.YTrackBar, A);
      SetTrackFrac(GLForm1.ZTrackBar, S);
      SetTrackFrac(GLForm1.X2TrackBar, R);
      SetTrackFrac(GLForm1.Y2TrackBar, P);
      SetTrackFrac(GLForm1.Z2TrackBar, I);
      GLForm1.CutoutChange(nil);
    end;
   {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PySHADERLIGHTAZIMUTHELEVATION(Self, Args : PPyObject): PPyObject; cdecl;
var
  A,E: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'ii:shaderlightazimuthelevation', @A, @E)) then begin
       SetTrackPos(GLForm1.LightAziTrack, A);
       SetTrackPos(GLForm1.LightElevTrack, E);
    end;
    {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PySHARPEN(Self, Args : PPyObject): PPyObject; cdecl;
begin
  SetTrackPos(GLForm1.LightAziTrack, random(180));
   GLForm1.SharpenMenuClick(nil);
  {$IFDEF PY4LAZ}with GetPythonEngine do {$ENDIF}
   Result:= PyBool_FromLong(Ord(TRUE));

end;

function PyCAMERADISTANCE(Self, Args : PPyObject): PPyObject; cdecl;
var
  Z: single;
begin
  {$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'f:cameradistance', @Z)) then
      Vol1.Distance := (Z);
  ViewGPU1.Invalidate;
     {$IFDEF PY4LAZ}end;  {$ENDIF}
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
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(FALSE));
  n := Vol1.ShaderSliders.nUniform;
  if n < 1 then exit;
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
            Result:= PyBool_FromLong(Ord(True));
      end;
    end;
    {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PySHADERNAME(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  i: integer;
  ret: boolean;
begin
  {$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(FALSE));
  if Boolean(PyArg_ParseTuple(Args, 's:shadername', @PtrName)) then
    begin
      StrName:= string(PtrName);
      i := GLForm1.ShaderDrop.Items.IndexOf(StrName); //search is case-insensitive!
      ret := (i >= 0);
      if ret then begin
         GLForm1.ShaderDrop.ItemIndex := i;
         GLForm1.ShaderDropChange(nil);
      end;
      Result:= PyBool_FromLong(Ord(ret));
    end;
       {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PySHADERMATCAP(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  i: integer;
begin
  {$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(FALSE));
    if Bool(PyArg_ParseTuple(Args, 's:shadermatcap', @PtrName)) then
    begin
      StrName:= string(PtrName);
      i := GLForm1.MatCapDrop.Items.IndexOf(StrName);
      if i < 0 then begin
         if GLForm1.MatCapDrop.Items.Count < 1 then
            GLForm1.ScriptOutputMemo.Lines.Add('No matcap images available: reinstall Surfice.')
         else
             GLForm1.ScriptOutputMemo.Lines.Add('Unable to find matcap named '+StrName+'. Solution: choose an available matcap, e.g. "'+GLForm1.MatCapDrop.Items[0]+'"');
         Result:= PyBool_FromLong(Ord(False));
         exit;
      end;
      GLForm1.MatCapDrop.ItemIndex := i;
      GLForm1.MatCapDropChange(nil);
      if not GLForm1.MatCapDrop.visible then
         GLForm1.ScriptOutputMemo.Lines.Add('Hint: shadermatcap() requires using a shader that supports matcaps (use shadername() to select a new shader).');
      Result:= PyBool_FromLong(Ord(True));
    end;
       {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PyCOLORFROMZERO(Self, Args : PPyObject): PPyObject; cdecl;
var
  Layer, IsFromZero: integer;
begin
  {$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(FALSE));
    if Boolean(PyArg_ParseTuple(Args, 'ii:colorfromzero', @Layer, @IsFromZero)) then
    begin
     GLForm1.LayerColorFromZero(Layer, IsFromZero = 1);
      Result:= PyBool_FromLong(Ord(True));
    end;
       {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PyINVERTCOLOR(Self, Args : PPyObject): PPyObject; cdecl;
var
  Layer, isInvert: integer;
  niftiVol: TNIfTI;
begin
  {$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(FALSE));
    if Boolean(PyArg_ParseTuple(Args, 'ii:invertcolor', @Layer, @isInvert)) then
    begin
     if not vols.Layer(Layer,niftiVol) then exit;
     niftiVol.CX.InvertColorMap := isInvert = 1;
     niftiVol.CX.NeedsUpdate := true;
     niftiVol.CX.GenerateLUT();
     niftiVol.ForceUpdate();
     GLForm1.updateTimer.Enabled := true;
     Result:= PyBool_FromLong(Ord(True));
    end;
       {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PyGRAPHSCALING(Self, Args : PPyObject): PPyObject; cdecl;
var
  Style: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
Result:= PyBool_FromLong(Ord(FALSE));
    if Boolean(PyArg_ParseTuple(Args, 'i:graphscaling', @Style)) then
    begin
      {$IFDEF GRAPH}
      gGraph.Style := Style;
      gGraph.isRedraw := true;
      ViewGPUg.Invalidate;
      {$ENDIF}
      Result:= PyBool_FromLong(Ord(True));
    end;
       {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PyHIDDENBYCUTOUT(Self, Args : PPyObject): PPyObject; cdecl;
var
  Layer, isHidden: integer;
  niftiVol: TNIfTI;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(FALSE));
    if Boolean(PyArg_ParseTuple(Args, 'ii:hiddenbycutout', @Layer, @isHidden)) then
    begin
     if not vols.Layer(Layer,niftiVol) then exit;
     niftiVol.HiddenByCutout := (isHidden = 1);
     GLForm1.CutoutChange(nil);
     niftiVol.CX.NeedsUpdate := true;
     Result:= PyBool_FromLong(Ord(True));
    end;
       {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PyOPACITY(Self, Args : PPyObject): PPyObject; cdecl;
var
  PCT: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Bool(PyArg_ParseTuple(Args, 'i:opacity', @PCT)) then
       GLForm1.LayerChange(0, -1, PCT, kNaNsingle, kNaNsingle); //kNaNsingle
       {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PyCOLORBARPOSITION(Self, Args : PPyObject): PPyObject; cdecl;
var
  p: integer;
  v: boolean;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
  {$IFDEF CLRBAR}
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
  {$ENDIF}
       {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PyOVERLAYOPACITY(Self, Args : PPyObject): PPyObject; cdecl;
var
  A,B: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Bool(PyArg_ParseTuple(Args, 'ii:overlayopacity', @A, @B)) then
       GLForm1.LayerChange(A, -1, B, kNaNsingle, kNaNsingle); //kNaNsingle
       {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PyORTHOVIEWMM(Self, Args : PPyObject): PPyObject; cdecl;
var
  X,Y,Z: single;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'fff:orthoviewmm', @X,@Y,@Z)) then begin
      gPrefs.DisplayOrient := kAxCorSagOrient3;
      GLForm1.UpdateVisibleBoxes();
      GLForm1.SetXHairPosition(X,Y,Z);
      GLForm1.MPRMenu.checked := true;
      ViewGPU1.Invalidate;
    end;
       {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PyFULLSCREEN(Self, Args : PPyObject): PPyObject; cdecl;
var
  Vis: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'i:fullscreen', @Vis)) then begin
       if (Vis = 1) then
          GLForm1.WindowState := wsFullScreen// wsMaximized
       else
           GLForm1.WindowState := wsMaximized;
    end;
       {$IFDEF PY4LAZ}end;  {$ENDIF}
end;

function PyQUIT(Self, Args : PPyObject): PPyObject; cdecl;
begin
  //GLForm1.ScriptOutputMemo.Lines.Add('Terminating application');
{$IFDEF PY4LAZ}with GetPythonEngine do  {$ENDIF}
  Result:= PyBool_FromLong(Ord(TRUE));
  //GLForm1.ScriptOutputMemo.Lines.Add('Terminating application 2');
  printf('Script executed quit(): Terminating MRIcroGL');
  GlForm1.ScriptOutputMemo.Tag := 123;
  //GLForm1.Close;
end;

function PySAVEBMP(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 's:savebmp', @PtrName)) then
    begin
      StrName:= string(PtrName);
      if not GLForm1.Save2Bmp(StrName) then begin
        pyprintf('Unable to write a bitmap named '+StrName);
        {$IFDEF Darwin}
        pyprintf(' Warning: MacOS security entitlements can block file writing.');
        {$ENDIF}
        Result:= PyBool_FromLong(Ord(False));
      end;
      //todo
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PySAVEIMG(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  fnm: string;
  niftiVol: TNIfTI;
  ok: boolean;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(False));
  if not vols.Layer(0,niftiVol) then exit;
  Result:=  PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 's:saveimg', @PtrName)) then begin
      fnm := string(PtrName);
      ok := niftiVol.SaveFormatBasedOnExt(fnm);
      (*ext := upcase(ExtractFileExt(fnm));
      if (ext = '.OSP') then
         ok := niftiVol.SaveOsp(fnm)
      else if (ext = '.BVOX') then
         ok := niftiVol.SaveBVox(fnm)
      else
          ok := niftiVol.Save(fnm); *)
      Result:= PyBool_FromLong(Ord(ok));
    end; //OK
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function pyBMPTRANSPARENT(Self, Args : PPyObject): PPyObject; cdecl;
var
  transparent: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'i:transparent', @transparent)) then begin
       gPrefs.ScreenCaptureTransparentBackground := (transparent = 1);
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyCONTRASTMINMAX(Self, Args : PPyObject): PPyObject; cdecl;
var
  MN,MX: single;
var
   niftiVol: TNIfTI;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(False));
  if not vols.Layer(0,niftiVol) then exit;
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'ff:contrastminmax', @MN,@MX)) then begin
      GLForm1.LayerChange(0, -1, -1, MN, MX); //kNaNsingle
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;
//removesmallclusters(layer, thresh, mm, neighbors)
//pyREMOVESMALLCLUSTERS removesmallclusters(layer, thresh, mm) -> Set the colorscheme for the target overlay (0=background layer) to a specified name.
function pyREMOVESMALLCLUSTERS(Self, Args : PPyObject): PPyObject; cdecl;
var
  i, neighbors: integer;
  thresh,mm: single;
  niftiVol: TNIfTI;
begin
  {$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
  mm := 1;
  neighbors := gPrefs.ClusterNeighborMethod;
    if Bool(PyArg_ParseTuple(Args, 'if|fi:removesmallclusters', @i, @thresh, @mm, @neighbors)) then begin
         GLForm1.ScriptOutputMemo.lines.add(format('removesmallclusters(%d, %d, %g, %g)', [i, neighbors, thresh, mm]));
         GLForm1.ScriptOutputMemo.lines.add(format(' neighbors=%d (1=faces(6),2=faces+edges(18),3=faces+edges+corners(26)', [neighbors]));
         if (i < 0) or (i >= GLForm1.LayerList.Count) then begin
            GLForm1.ScriptOutputMemo.lines.add('error: is this layer loaded?');
            exit;
         end;
         if not vols.Layer(GLForm1.LayerList.ItemIndex,niftiVol) then begin
            GLForm1.ScriptOutputMemo.lines.add('error: is this layer loaded?');
            exit;
         end;
         mm := niftiVol.RemoveSmallClusters(thresh,mm, neighbors);
         GLForm1.ScriptOutputMemo.lines.add(format('%.3fmm3 survive threshold', [mm]));
         niftiVol.ForceUpdate(); //defer time consuming work
         GLForm1.updateTimer.enabled := true;
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyOVERLAYMINMAX(Self, Args : PPyObject): PPyObject; cdecl;
var
  layer: integer;
  MN,MX: single;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Bool(PyArg_ParseTuple(Args, 'iff:overlayminmax', @layer, @MN, @MX)) then begin
       //OVERLAYMINMAX(A,B,C);
       GLForm1.LayerChange(layer, -1, -1, MN, MX); //kNaNsingle
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyVOLUME(Self, Args : PPyObject): PPyObject; cdecl;
var
  layer, vol: integer;
  v: TNIfTI;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
Result:= PyBool_FromLong(Ord(True));
    if Bool(PyArg_ParseTuple(Args, 'ii:volume', @layer, @vol)) then begin
       if not vols.Layer(layer,v) then exit;
          v.SetDisplayVolume(vol);
        //GLForm1.caption := format('->%d/%d ',[v.VolumeDisplayed+1, v.Header.dim[4]]); //+1 as indexed from 1
        GLForm1.UpdateLayerBox(true);// e.g. "fMRI (1/60)" -> "fMRI (2/60"
        GLForm1.UpdateTimer.Enabled := true;
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

//AddMethod('zerointensityinvisible', @PyZEROINTENSITYVISIBLE, ' zerointensityinvisible(layer, bool) ->  For specified layer (0 = background) should voxels with intensity 0 be opaque (bool= 0) or transparent (bool = 1).');
function PyZEROINTENSITYINVISIBLE(Self, Args : PPyObject): PPyObject; cdecl;
var
  layer, vol: integer;
  v: TNIfTI;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Bool(PyArg_ParseTuple(Args, 'ii:zerointensityinvisible', @layer, @vol)) then begin
       if not vols.Layer(layer,v) then exit;
       v.ZeroIntensityInvisible:= (vol = 1);
       v.ForceUpdate(); //defer time consuming work
       GLForm1.UpdateTimer.Enabled := true;
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyGENERATECLUSTERS(Self, Args : PPyObject): PPyObject; cdecl;
var
  layer, method, bimodal: integer;
  thresh, mm: single;
  v: TNIfTI;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
Result:= PyBool_FromLong(Ord(True));
 thresh := kNaN;
 mm := 32;
 method := gPrefs.ClusterNeighborMethod;
 bimodal := 0;
   if Bool(PyArg_ParseTuple(Args, 'i|ffii:generateclusters', @layer, @thresh, @mm, @method, @bimodal)) then begin
      if not vols.Layer(layer,v) then begin
         GLForm1.ScriptOutputMemo.lines.Add('generateclusters unable to load layer '+inttostr(layer));
         exit;
      end;
      while (isBusy) or (GLForm1.Updatetimer.enabled) do
            Application.ProcessMessages;
       GenerateClustersCore(v, thresh, mm,method, bimodal = 1);
       //GenerateClustersCore(v, thresh, mm,method, true);
       GLForm1.UpdateLayerBox(true);// show cluster panel
       //GLForm1.UpdateTimer.Enabled := true;
    end;
   {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyBMPZOOM(Self, Args : PPyObject): PPyObject; cdecl;
var
  Z: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'i:bmpzoom', @Z)) then begin
      gPrefs.DisplayOrient := kRenderOrient;
      gPrefs.BitmapZoom:= Z;
      {$IFDEF METALAPI}
      //GLForm1.ScriptOutputMemo.lines.Add('warning: Metal does not yet support bmpzoom()');
      {$ENDIF}
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyVIEW(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'i:view', @A)) then begin
      gPrefs.DisplayOrient := A;
      GLForm1.UpdateVisibleBoxes();
      ViewGPU1.Invalidate;
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyVIEWAXIAL(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'i:viewaxial', @A)) then begin
      gPrefs.DisplayOrient := kRenderOrient;
      GLForm1.UpdateVisibleBoxes();
      if A = 1 then
         GLForm1.DisplaySuperiorMenu.click
      else
          GLForm1.DisplayInferiorMenu.click;
      ViewGPU1.Invalidate;
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyVIEWCORONAL(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'i:viewcoronal', @A)) then begin
      gPrefs.DisplayOrient := kRenderOrient;
      GLForm1.UpdateVisibleBoxes();
      if A = 1 then
         GLForm1.DisplayPosteriorMenu.click
      else
          GLForm1.DisplayAnteriorMenu.click;
      ViewGPU1.Invalidate;
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyVIEWSAGITTAL(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'i:viewsagittal', @A)) then begin
      gPrefs.DisplayOrient := kRenderOrient;
      GLForm1.UpdateVisibleBoxes();
      if A = 1 then
         GLForm1.DisplayLeftMenu.click
      else
          GLForm1.DisplayRightMenu.click;
      ViewGPU1.Invalidate;
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyYOKE(Self, Args : PPyObject): PPyObject; cdecl;
var
  isYoke: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'i:yoke', @isYoke)) then begin
       GLForm1.YokeMenu.Checked := (isYoke = 1);
       GLForm1.YokeTimer.Enabled := (isYoke = 1);
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyWAIT(Self, Args : PPyObject): PPyObject; cdecl;
var
  waitTime: integer;
  endTime : QWord;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'i:wait', @waitTime)) then begin
       if waitTime < 0 then exit;
       endTime := GetTickCount64+DWord(waitTime);
       repeat
              Application.ProcessMessages;//HandleMessage
       until (GetTickCount64 >= endTime);
       while (isBusy) or (GLForm1.Updatetimer.enabled) do
             Application.ProcessMessages;
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

//(name: 'windowposition'; callback: @PyWINDOWPOSITION; help: ' windowposition(l,t,w,h) -> Set the left, top, width and height for the executable window.'),
function PyWINDOWPOSITION(Self, Args : PPyObject): PPyObject; cdecl;
var
  l,t,w,h: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'iiii:windowposition', @l, @t, @w, @h)) then begin
       GLForm1.Left := l;
       GLForm1.Top := t;
       GLForm1.Width := w;
       GLForm1.Height := h;
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;


function PyZOOMSCALE2D(Self, Args : PPyObject): PPyObject; cdecl;
var
  zoom: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'i:zoomscale', @zoom)) then begin
       if zoom < 1 then zoom := 1;
       if zoom > 6 then zoom := 6;
       GLForm1.sliceZoom.position := round(zoom * 100);
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyZOOMCENTER(Self, Args : PPyObject): PPyObject; cdecl;
var
  X,Y,Z: single;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'fff:zoomcenter', @X,@Y,@Z)) then begin
      if (X < 0) then X := 0;
      if (X > 1) then X := 1;
      if (Y < 0) then Y := 0;
      if (Y > 0) then Y := 1;
      if (Z < 0) then Z := 0;
      if (Z > 0) then Z := 1;
      Vol1.Slices.ZoomCenter := Vec3(X,Y,Z);
      ViewGPU1.Invalidate;
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PySMOOTH2D(Self, Args : PPyObject): PPyObject; cdecl;
var
  smooth: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'i:smooth', @smooth)) then begin
       GLForm1.smooth2DCheck.checked := (smooth = 1);
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyLINECOLOR(Self, Args : PPyObject): PPyObject; cdecl;
var
  R,G,B: integer;
  clr: TVec4;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
  {$IFDEF CLRBAR}
    if Boolean(PyArg_ParseTuple(Args, 'iii:linecolor', @R,@G,@B)) then begin
      clr := Vol1.Slices.LineColor;
      clr := Vec4(R/255.0, G/255.0, B/255.0, clr.a);
      //gPrefs.LineColor := clr;
      Vol1.Slices.LineColor := clr;
      gClrbar.RulerColor := SetRGBA(Vol1.Slices.LineColor);
      GLFOrm1.RulerVisible();
    end;
  {$ENDIF}
  {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyLINEWIDTH(Self, Args : PPyObject): PPyObject; cdecl;
var
  W: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'i:linewidth', @W)) then begin
       GLForm1.LineWidthEdit.value := W;
       Vol1.Slices.LineWidth := W;
    end;
    ViewGPU1.Invalidate;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyMODALMESSAGE(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(FALSE));
    if Boolean(PyArg_ParseTuple(Args, 's:modalmessage', @PtrName)) then
    begin
      StrName:= string(PtrName);
      Showmessage(StrName);
      Result:= PyBool_FromLong(Ord(True));
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PySCRIPTFORMVISIBLE(Self, Args : PPyObject): PPyObject; cdecl;
var
  Vis: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'i:scriptformvisible', @Vis)) then
       GLForm1.ScriptFormVisible(Vis = 1);
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyTOOLFORMVISIBLE(Self, Args : PPyObject): PPyObject; cdecl;
var
  Vis: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));

    if Boolean(PyArg_ParseTuple(Args, 'i:toolformvisible', @Vis)) then begin
      if (Boolean(Vis)) then
         GLForm1.ToolPanel.width := GLForm1.ToolPanel.Constraints.MaxWidth
      else
          GLForm1.ToolPanel.width := 0;
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PySHADERQUALITY1TO10(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, 'i:shaderquality1to10', @A)) then
       GLForm1.QualityTrack.Position := A;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PySHADERUPDATEGRADIENTS(Self, Args : PPyObject): PPyObject; cdecl;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result := PyBool_FromLong(Ord(TRUE));
  GLForm1.UpdateTimer.Enabled := true;
  {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyEXTRACT(Self, Args : PPyObject): PPyObject; cdecl;
var
  isSmoothEdges, isSingleObject, OtsuLevels: integer;
  niftiVol: TNIfTI;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
  //isSmoothEdges: boolean = true; isSingleObject: boolean = true; OtsuLevels : integer = 5
  isSmoothEdges := 1;
  isSingleObject := 1;
  OtsuLevels := 5;
  if not vols.Layer(0,niftiVol) then exit;
  Result:= PyBool_FromLong(Ord(True));
    if Boolean(PyArg_ParseTuple(Args, '|iii:extract', @isSmoothEdges,@isSingleObject,@OtsuLevels)) then begin
      //EXTRACT(Otsu,Dil,Boolean(One));
      niftiVol.RemoveHaze(isSmoothEdges=1, isSingleObject=1, OtsuLevels); //todo: dilation and number of Otsu layers
      ViewGPU1.Invalidate;
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyOVERLAYLOADSMOOTH(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Bool(PyArg_ParseTuple(Args, 'i:overlayloadsmooth', @A)) then begin
       Vols.InterpolateOverlays := A = 1;
       GLForm1.LayerSmoothMenu.Checked := A = 1;
       //no need to refresh: inlfuences future loads
    end;
    {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyOVERLAYADDITIVEBLENDING(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Bool(PyArg_ParseTuple(Args, 'i:overlayadditiveblending', @A)) then begin
       Vols.AdditiveOverlayBlending := A = 1;
       GLForm1.LayerAdditiveMenu.Checked := A = 1;
       GLForm1.ForceOverlayUpdate();
       GLForm1.UpdateTimer.Enabled := true;
    end;
      {$IFDEF PY4LAZ}end;{$ENDIF}
end;

function PyOVERLAYMASKWITHBACKGROUND(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
{$IFDEF PY4LAZ}with GetPythonEngine do begin  {$ENDIF}
  Result:= PyBool_FromLong(Ord(True));
    if Bool(PyArg_ParseTuple(Args, 'i:overlayadditiveblending', @A)) then begin
       Vols.MaskWithBackground := A = 1;
       GLForm1.LayerMaskWithBackgroundMenu.Checked := A = 1;
       GLForm1.ForceOverlayUpdate();
       GLForm1.UpdateTimer.Enabled := true;
    end;
     {$IFDEF PY4LAZ}end;{$ENDIF}

end;

{$IFDEF PY4LAZ}
type
  TPythonBridgeMethod = record
    name: ansistring;
    callback: PyCFunction;
    help: ansistring;
  end;
{$ENDIF}
var
  methods: array[0..69] of TPythonBridgeMethod = (
(name: 'atlashide'; callback: @PyATLASHIDE; help: ' atlashide(layer, indices...) -> Hide all (e.g. "atlashide(1)") or some (e.g. "atlashide(1, (17, 22))") regions of an atlas.'),
(name: 'atlaslabels'; callback: @PyATLASLABELS; help: ' atlasmaxindex(layer) -> Returns string listing all regions in an atlas'),
(name: 'atlasmaxindex'; callback: @PyATLASMAXINDEX; help: ' atlasmaxindex(layer) -> Returns maximum region humber in specified atlas. For example, if you load the CIT168 atlas (which has 15 regions) as your background image, then atlasmaxindex(0) will return 15.'),
(name: 'atlasshow'; callback: @PyATLASSHOW; help: ' atlasshow(layer, indices...) -> Show all (e.g. "atlasshow(1)") or some (e.g. "atlasshow(1, (17, 22))") regions of an atlas.'),
(name: 'azimuthelevation'; callback: @PyAZIMUTHELEVATION; help: ' azimuthelevation(azi, elev) -> Sets the render camera location.'),
(name: 'backcolor'; callback: @PyBACKCOLOR; help: ' backcolor(r, g, b) -> changes the background color, for example backcolor(255, 0, 0) will set a bright red background'),
(name: 'bmptransparent'; callback: @pyBMPTRANSPARENT; help: ' bmptransparent(v) -> set if bitmaps use transparent (1) or opaque (0) background'),
(name: 'bmpzoom'; callback: @PyBMPZOOM; help: ' bmpzoom(z) -> changes resolution of savebmp(), for example bmpzoom(2) will save bitmaps at twice screen resolution'),
(name: 'cameradistance'; callback: @PyCAMERADISTANCE; help: ' cameradistance(z) -> Sets the viewing distance from the object.'),
(name: 'clipazimuthelevation'; callback: @PyCLIPAZIMUTHELEVATION; help: ' clipazimuthelevation(depth, azi, elev) -> Set a view-point independent clip plane.'),
(name: 'clipthick'; callback: @PyCLIPTHICK; help: ' clipthick(thick) -> Set size of clip plane slab (0..1).'),
(name: 'colorbarposition'; callback: @PyCOLORBARPOSITION; help: ' colorbarposition(p) -> Set colorbar position (0=off, 1=top, 2=right).'),
(name: 'colorbarsize'; callback: @PyCOLORBARSIZE; help: ' colorbarsize(p) -> Change width of color bar f is a value 0.01..0.5 that specifies the fraction of the screen used by the colorbar.'),
(name: 'coloreditor'; callback: @PyCOLOREDITOR; help: ' coloreditor(s) -> Show (1) or hide (0) color editor and histogram.'),
(name: 'colorfromzero'; callback: @PyCOLORFROMZERO; help: ' colorfromzero(layer, isFromZero) -> Color scheme display range from zero (1) or from treshold value (0)?'),
(name: 'colorname'; callback: @PyCOLORNAME; help: ' colorname(colorName) -> Loads  the requested colorscheme for the background image.'),
(name: 'colornode'; callback: @PyCOLORNODE; help: ' colornode(layer, index, intensity, r, g, b, a) -> Adjust color scheme for image.'),
(name: 'cutout'; callback: @PyCUTOUT; help: ' cutout(L,A,S,R,P,I) -> Remove sector from volume.'),
(name: 'drawload'; callback: @PyDRAWLOAD; help: ' drawload(filename) -> Load an image as a drawing (region of interest).'),
(name: 'extract'; callback: @PyEXTRACT; help: ' extract(b,s,t) -> Remove haze from background image. Blur edges (b: 0=no, 1=yes, default), single object (s: 0=no, 1=yes, default), threshold (t: 1..5=high threshold, 5 is default, higher values yield larger objects)'),
(name: 'fullscreen'; callback: @PyFULLSCREEN; help: ' fullscreen(max) -> Form expands to size of screen (1) or size is maximized (0).'),
(name: 'generateclusters'; callback: @PyGENERATECLUSTERS; help: ' generateclusters(layer, thresh, minClusterMM3, method, bimodal) -> create list of distinct regions. Optionally provide cluster intensity, minimum cluster size, neighbor method(1=faces,2=faces+edges,3=faces+edges+corners). If bimodal = 1, both dark and bright clusters are detected.'),
(name: 'graphscaling'; callback: @PyGRAPHSCALING; help: ' graphscaling(type) -> Vertical axis of graph is raw (0), demeaned (1) normalized -1..1 (2) normalized 0..1 (3) or percent (4).'),
(name: 'gui_input'; callback: @PyGUI_INPUT; help: 'gui_input(caption, prompt, default) -> allow user to type value into a dialog box.'),
(name: 'hiddenbycutout'; callback: @PyHIDDENBYCUTOUT; help: ' hiddenbycutout(layer, isHidden) -> Will cutout hide (1) or show (0) this layer?'),
(name: 'invertcolor'; callback: @PyINVERTCOLOR; help: ' invertcolor(layer, isInverted) -> Is color intensity inverted (1) or not (0) this layer?'),
(name: 'linecolor'; callback: @PyLINECOLOR; help: ' linecolor(r,g,b) -> Set color of crosshairs, so "linecolor(255,0,0)" will use bright red lines.'),
(name: 'linewidth'; callback: @PyLINEWIDTH; help: ' linewidth(wid) -> Set thickness of crosshairs used on 2D slices.'),
(name: 'loadgraph'; callback: @PyLOADGRAPH; help: ' loadgraph(graphName, add = 0) -> Load text file graph (e.g. AFNI .1D, FSL .par, SPM rp_.txt). If "add" equals 1 new graph added to existing graph'),
(name: 'loadimage'; callback: @PyLOADIMAGE; help: ' loadimage(imageName) -> Close all open images and load new background image.'),
(name: 'minmax'; callback: @PyOVERLAYMINMAX; help: ' minmax(layer, min, max) -> Sets the color range for the overlay (layer 0 = background).'),
(name: 'modalmessage'; callback: @PyMODALMESSAGE; help: ' modalmessage(msg) -> Show a message in a dialog box, pause script until user presses "OK" button.'),
(name: 'mosaic'; callback: @PyMOSAIC; help: ' mosaic(mosString) -> Create a series of 2D slices.'),
(name: 'opacity'; callback: @PyOPACITY; help: ' opacity(opacityPct) -> Is the background image transparent (0), translucent (~50) or opaque (100)?'),
(name: 'opacity'; callback: @PyOVERLAYOPACITY; help: ' opacity(layer, opacityPct) -> Make the layer (0 for background, 1 for 1st overlay) transparent(0), translucent (~50) or opaque (100).'),
(name: 'orthoviewmm'; callback: @PyORTHOVIEWMM; help: ' orthoviewmm(x,y,z) -> Show 3 orthogonal slices of the brain, specified in millimeters.'),
(name: 'overlayadditiveblending'; callback: @PyOVERLAYADDITIVEBLENDING; help: ' overlayadditiveblending(v) -> Merge overlays using additive (1) or multiplicative (0) blending.'),
(name: 'overlaycloseall'; callback: @PyOVERLAYCLOSEALL; help: ' overlaycloseall() -> Close all open overlays.'),
(name: 'overlaycount'; callback: @PyOVERLAYCOUNT; help: ' overlaycount() -> Return number of overlays currently open.'),
(name: 'overlayload'; callback: @PyOVERLAYLOAD; help: ' overlayload(filename) -> Load an image on top of prior images.'),
(name: 'overlayloadsmooth'; callback: @PyOVERLAYLOADSMOOTH; help: ' overlayloadsmooth(0) -> Will future overlayload() calls use smooth (1) or jagged (0) interpolation?'),
(name: 'overlaymaskwithbackground'; callback: @PyOVERLAYMASKWITHBACKGROUND; help: 'overlaymaskwithbackground(v) -> hide (1) or show (0) overlay voxels that are transparent in background image.'),
(name: 'pitch'; callback: @PyPITCH; help: ' pitch(degrees) -> Sets the pitch of object to be rendered.'),
(name: 'quit'; callback: @PyQUIT; help: ' quit() -> Terminate the application.'),
(name: 'removesmallclusters'; callback: @pyREMOVESMALLCLUSTERS; help: ' removesmallclusters(layer, thresh, mm, neighbors) -> only keep clusters where intensity exceeds thresh and size exceed mm. Clusters based on neighbors that share faces (1), faces+edges (2) or faces+edges+corners (3)'),
(name: 'resetdefaults'; callback: @PyRESETDEFAULTS; help: ' resetdefaults() -> Revert settings to sensible values.'),
(name: 'savebmp'; callback: @PySAVEBMP; help: ' savebmp(pngName) -> Save screen display as bitmap. For example "savebmp(''test.png'')"'),
(name: 'saveimg'; callback: @PySAVEIMG; help: ' saveimg(filename) -> Save background image (layer 0) to disk. For example "saveimg(''test.nii'')", extension defines type (.nii=NIfTI, .osp=ospray, .bvox=blender)'),
(name: 'scriptformvisible'; callback: @PySCRIPTFORMVISIBLE; help: ' scriptformvisible (visible) -> Show (1) or hide (0) the scripting window.'),
(name: 'shaderadjust'; callback: @PySHADERADJUST; help: ' shaderadjust(sliderName, sliderValue) -> Set level of shader property. Example "gl.shaderadjust(''edgethresh'', 0.6)"'),
(name: 'shaderlightazimuthelevation'; callback: @PySHADERLIGHTAZIMUTHELEVATION; help: ' shaderlightazimuthelevation(a,e) -> Position the light that illuminates the rendering. For example, "shaderlightazimuthelevation(0,45)" places a light 45-degrees above the object'),
(name: 'shadermatcap'; callback: @PySHADERMATCAP; help: ' shadermatcap(name) -> Set material capture file (assumes "matcap" shader. For example, "shadermatcap(''mc01'')" selects mc01 matcap.'),
(name: 'shadername'; callback: @PySHADERNAME; help: ' shadername(name) -> Choose rendering shader function. For example, "shadername(''mip'')" renders a maximum intensity projection.'),
(name: 'shaderquality1to10'; callback: @PySHADERQUALITY1TO10; help: ' shaderquality1to10(i) -> Renderings can be fast (1) or high quality (10), medium values (6) balance speed and quality.'),
(name: 'shaderupdategradients'; callback: @PySHADERUPDATEGRADIENTS; help: ' shaderupdategradients() -> Recalculate volume properties.'),
(name: 'sharpen'; callback: @PySHARPEN; help: ' sharpen() -> apply unsharp mask to background volume to enhance edges'),
(name: 'smooth'; callback: @PySMOOTH2D; help: ' smooth2D(s) -> make 2D images blurry (linear interpolation, 1) or jagged (nearest neightbor, 0).'),
(name: 'toolformvisible'; callback: @PyTOOLFORMVISIBLE; help: ' toolformvisible(visible) -> Show (1) or hide (0) the tool panel.'),
(name: 'version'; callback: @PyVERSION; help: ' version() -> Return the version of MRIcroGL.'),
(name: 'view'; callback: @PyVIEW; help: ' view(v) -> Display Axial (1), Coronal (2), Sagittal (4), Flipped Sagittal (8), MPR (16), Mosaic (32) or Rendering (64)'),
(name: 'viewaxial'; callback: @PyVIEWAXIAL; help: ' viewaxial(SI) -> Show rendering with camera superior (1) or inferior (0) of volume.'),
(name: 'viewcoronal'; callback: @PyVIEWCORONAL; help: ' viewcoronal(AP) -> Show rendering with camera posterior (1) or anterior (0) of volume.'),
(name: 'viewsagittal'; callback: @PyVIEWSAGITTAL; help: ' viewsagittal(LR) -> Show rendering with camera left (1) or right (0) of volume.'),
(name: 'volume'; callback: @PyVOLUME; help: ' volume(layer, vol) -> For 4D images, set displayed volume (layer 0 = background; volume 0 = first volume in layer).'),
(name: 'wait'; callback: @PyWAIT; help: ' wait(ms) -> Pause script for (at least) the desired milliseconds.'),
(name: 'windowposition'; callback: @PyWINDOWPOSITION; help: ' windowposition(l,t,w,h) -> Set the left, top, width and height for the executable window.'),
(name: 'yoke'; callback: @PyYOKE; help: ' yoke(v) -> Yoke (1) instances so different instances show same view.'),
(name: 'zerointensityinvisible'; callback: @PyZEROINTENSITYINVISIBLE; help: ' zerointensityinvisible(layer, bool) ->  For specified layer (0 = background) should voxels with intensity 0 be opaque (bool= 0) or transparent (bool = 1).'),
(name: 'zoomcenter'; callback: @PyZOOMCENTER; help: ' zoomcenter(x,y,z) -> Set center of expansion for zoom scale (values in range 0..1 with 0.5 in volume center).'),
(name: 'zoomscale'; callback: @PyZOOMSCALE2D; help: ' zoomscale2D(z) -> Enlarge 2D image (range 1..6).')
);

{$IFDEF PY4LAZ}
procedure TGLForm1.PyModInitialization(Sender: TObject);
var
 i: integer;
begin
  for i := 0 to (length(methods) -1)  do
    (Sender as TPythonModule).AddMethod (PAnsiChar(methods[i].name), methods[i].callback, PAnsiChar(methods[i].help));
end;
{$ELSE}

var
 PyEngine: TPythonModule = nil;

var
  gPythonData: UnicodeString = '';

procedure TGLForm1.GotPythonData(str: UnicodeString);
var
  i: integer;
begin
  if length(str) < 1 then exit;
  for i := 1 to length(str) do begin
    if str[i] = LF then begin
      ScriptOutputMemo.lines.add(gPythonData);
      {$ifdef unix}
      //writeln('<<<'+gPythonData);
      {$endif}
      gPythonData := '';
    end else
      gPythonData += str[i];
  end;
end;

function TGLForm1.PyCreate: boolean;
begin

 if not PythonLoadAndInitialize(ResourceDir, GLForm1.GotPythonData) then begin
   writeln('Unable to load Python');
   exit(false);
 end;
 printf('Loading modules....');
 PyEngine := PythonAddModule('gl', @methods, length(methods));
 printf('Done Loading modules....');
 result := true;
end;
{$ENDIF} //IF PY4LAZ ELSE PYTHONBRIDGE

function TGLForm1.PyExec(): boolean;
begin
  result := false; //assume code is not Python
  ScriptOutputMemo.lines.Clear;
  if PyEngine = nil then begin
    if not PyCreate then begin //do this the first time
      {$IFDEF PY4LAZ}
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
       {$ELSE}
        GLForm1.ScriptOutputMemo.lines.Add('Unable to start PythonBridge');
       {$ENDIF}
       result := true;
       exit;
    end;
  end;
  result := true;
  ScriptOutputMemo.lines.Add('Running Python script');
   gPyRunning := true;
  GLForm1.ScriptingRunMenu.caption := 'Halt';
  try
   {$IFDEF PY4LAZ}
 PyEngine.ExecStrings(GLForm1.ScriptMemo.Lines);
   {$ELSE}
 PyRun_SimpleString(PAnsiChar(GLForm1.ScriptMemo.Lines.Text));
   {$ENDIF}
  except
    ScriptOutputMemo.lines.Add('Python Engine Failed');
    gPyRunning := false;
  end;
  if gPyRunning then
     ScriptOutputMemo.lines.Add('Python Succesfully Executed');
  gPyRunning := false;
  GLForm1.ScriptingRunMenu.caption := 'Run';
  if  GlForm1.ScriptOutputMemo.Tag = 123 then
      GLForm1.Close
  else if (gPrefs.DisplayOrient = kRenderOrient)  and (Vol1.Quality1to5 = 0) and (not gPyRunning) then
       BetterRenderTimer.enabled := true;
  result := true;
end;
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

procedure TGLForm1.SetDisplayCheck();
begin
  case gPrefs.DisplayOrient of
      kRenderOrient: RenderMenu.Checked := true;
      kAxialOrient: AxialMenu.Checked := true;
      kCoronalOrient: CoronalMenu.Checked := true;
      kSagRightOrient: SagittalMenu.Checked := true;
      kSagLeftOrient: SagittalLMenu.Checked := true;
      kAxCorSagOrient3: MPRMenu.Checked := true;
      kAxCorSagOrient4: MPR4Menu.Checked := true;
      kMosaicOrient: MosaicMenu.Checked := true;
  end;
end;

procedure TGLForm1.UpdateVisibleBoxes(setMenuChecked: boolean = false);
var Ht: integer;
begin
     if (setMenuChecked) then
        SetDisplayCheck();
     //SliceBox.Visible := gPrefs.DisplayOrient = kRenderOrient;
     ClipBox.Visible := gPrefs.DisplayOrient = kRenderOrient;
     CutoutBox.Visible := gPrefs.DisplayOrient = kRenderOrient;
     ShaderBox.Visible := gPrefs.DisplayOrient = kRenderOrient;
     SliceBox.Visible := gPrefs.DisplayOrient <= kAxCorSagOrient3;
     if (gPrefs.DisplayOrient = kAxCorSagOrient4) then begin
        Ht := LayerBox.Height + LineBox.Height+ SliceBox.Height+ClipBox.Height;
        //LayerBox.Caption := inttostr(Ht) +':'+inttostr(ToolPanel.ClientHeight);
     	ClipBox.Visible := (ToolPanel.ClientHeight) > Ht;
        Ht += CutoutBox.Height;
        CutoutBox.Visible := (ToolPanel.ClientHeight) > Ht;
        Ht += 100;//ShaderBox.Height;
        ShaderBox.Visible := (ToolPanel.ClientHeight) > Ht;
     end;
     //ClusterView.Visible := gPrefs.DisplayOrient <= kAxCorSagOrient;
     //ViewGPUg.Visible := not ClusterView.Visible;
     AnatDrop.Visible := length(gLandmark.Landmarks) > 0;
     LandmarkMenu.Visible := gPrefs.LandmarkPanel;
     LineBox.Visible := gPrefs.DisplayOrient <= kMosaicOrient;
     MosaicBox.Visible := gPrefs.DisplayOrient = kMosaicOrient;
     if (Vol1.Slices <> nil) then Vol1.Slices.distanceLineOrient := 0;
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
     {$IFDEF Darwin}
     if not isReadable(imgName) then continue;
     {$ENDIF}
     imgName := ChangeFileExt(ExtractFileName(imgName),'');
     if upcase(ExtractFileExt(imgName))= '.NII' then
        imgName := ChangeFileExt(imgName,''); //img.nii.gz -> img
     newMenu := TMenuItem.Create(MainMenu);
     newMenu.Caption := imgName;
     newMenu.OnClick:= OpenRecentMenuClick;
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

procedure TGLForm1.CreateStandardMenus (ParentMenu: TMenuItem);
var
  standardNamesGZ, standardNames : TStringList;
  dir, standardName, ext: string;
  i: integer;
  newMenu: TMenuItem;
begin
   //OpenStandardMenu.Enabled:= false;
   if ParentMenu.tag = 1 then
      dir := AtlasDir
   else if ParentMenu.tag = 2 then begin
        dir := gPrefs.AfniDir;
        if not DirectoryExists(dir) then exit;
   end else
       dir := StandardDir;
   if not DirectoryExists(dir) then begin
     ParentMenu.Enabled := false;
     printf('Unable to find folder "'+dir+'"');
     exit;//showmessage('Unable to find folder "'+dir+'"');
   end;
   standardNames := FindAllFiles(dir, '*.nii', false);
   try
    standardNamesGZ := FindAllFiles(dir, '*.nii.gz', false);
    try
      standardNames.AddStrings(standardNamesGZ);
    finally
      standardNamesGZ.Free;
    end;
    standardNamesGZ := FindAllFiles(dir, '*.HEAD', false);
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
          newMenu.OnClick:= OpenStandardMenuClick;
          newMenu.tag := ParentMenu.tag;
          ParentMenu.Add(newMenu);
      end;
    end;
   finally
     standardNames.Free;
   end;
   if ParentMenu.tag = 2 then begin
     if (OpenAfniMenu.Count > 1) then
     AfniDirMenu.Visible := false;
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


function TGLForm1.OpenDialogExecute (lFilter,lCaption: string): TStringList;
var
   openDlg : TOpenDialog;
begin
  openDlg := TOpenDialog.Create(self);
  openDlg.InitialDir := GetCurrentDir;
  OpenDlg.Filter := lFilter;
  OpenDlg.FilterIndex := 1;
  OpenDlg.Title := lCaption;
  OpenDlg.Options := [ofAllowMultiSelect,ofFileMustExist];
  result := TStringList.Create();
  if OpenDlg.Execute then
     result.AddStrings(OpenDlg.Files);
  openDlg.Free;
end;

function TGLForm1.OpenDialogExecute1 (lFilter,lCaption: string): String;
var
   openDlg : TOpenDialog;
begin
  openDlg := TOpenDialog.Create(self);
  openDlg.InitialDir := GetCurrentDir;
  openDlg.Options := [ofFileMustExist];
  OpenDlg.Filter := lFilter;
  OpenDlg.FilterIndex := 1;
  OpenDlg.Title := lCaption;
  result := '';
  if OpenDlg.Execute then
     result := OpenDlg.FileName;
  openDlg.Free;
end;


procedure TGLForm1.CreateOverlapImageMenuClick(Sender: TObject);
var
   fnms: TStrings;
   nii1, niiI: TNIfTI;
   sum16, newI: TInt16s;
   sum8: TUInt8s;
   isOK: boolean;
   mx,i,j, vox: integer;
   dlg : TSaveDialog;
label
  123, 124, 125;
begin
    fnms := OpenDialogExecute(OpenDialog1.Filter, 'Select files to combine');
    if fnms.count < 1 then begin
       ShowMessage('Please select at least 2 images');
       goto 123;
     end;
    sum16 := nil;
    newI := nil;
    niiI := TNIfTI.Create();
    nii1 := TNIfTI.Create(fnms[0], gPrefs.ClearColor, true, gPrefs.MaxTexMb, isOK);
    if not isOK then goto 124;
    vox := nii1.Header.dim[1] * nii1.Header.dim[2] * nii1.Header.dim[3];
    if (vox < 1) then goto 124;
    sum16 := nii1.NotZero();
    for i := 2 to fnms.count do begin
        niiI := TNIfTI.Create(fnms[i-1], gPrefs.ClearColor, true, gPrefs.MaxTexMb, isOK);
        if not isOK then goto 124;
        newI := niiI.NotZero();
        for j := 0 to (vox-1) do
            sum16[j] := sum16[j] + newI[j];
    end;
    mx := 0;
    for j := 0 to (vox-1) do
        if (sum16[j] > mx) then mx := sum16[j];
    if mx = 0 then begin
       showmessage('No non-zero voxels found');
       goto 125;
    end;
    if (mx > 32767) then begin
       showmessage('Error: maximum density exceeds 32767');
       goto 125;
    end;
    dlg := TSaveDialog.Create(self);
    dlg.Title := 'Save sum map as NIfTI volume';
    dlg.InitialDir := extractfiledir(gPrefs.PrevBackgroundImage);
    {$IFDEF Darwin}
    if PosEx('.app', dlg.InitialDir) > 0 then
       dlg.InitialDir := HomeDir(false);
    {$ENDIF}
    dlg.Filter := 'NIfTI|*.nii|Compressed NIfTI|*.nii.gz';
    dlg.DefaultExt := '*.nii';
    dlg.FilterIndex := 0;
    if not dlg.Execute then
       goto 125;
    if (mx < 256) then begin
       setlength(sum8, vox);
       for j := 0 to (vox-1) do
           sum8[j] := sum16[j];
       nii1.SaveAsSourceOrient(dlg.FileName, '', 'N'+inttostr(fnms.count), sum8, kDT_UINT8, kNIFTI_INTENT_ESTIMATE);
       sum8 := nil;
    end else
        nii1.SaveAsSourceOrient(dlg.FileName, '', 'N'+inttostr(fnms.count), TUInt8s(sum16), kDT_INT16, kNIFTI_INTENT_ESTIMATE);
    goto 125; //success - go to cleanup
    124: //report error, then cleanup
    showmessage('Error: make sure all images have the same dimensions');
    125:
    nii1.Free;
    niiI.Free;
    sum16 := nil;
    newI := nil;
    123:
    fnms.Free;
end;

function parseIntentCode(nii: TNIfTI): integer;
var
   s: string;
begin
     result := -1; //error;
     if (nii.Header.intent_code <> kNIFTI_INTENT_ESTIMATE) or (length(nii.Header.intent_name) < 2) or (nii.Header.intent_name[1] <> 'N')  then exit;
     s := copy(nii.Header.intent_name,2, length(nii.Header.intent_name)-1);
     result := StrToIntDef(s, -1);
end;

procedure TGLForm1.CreateSubtractionPlotMenuClick(Sender: TObject);
var
   fnm: string;
   niiPos, niiNeg: TNIfTI;
   nPos, nNeg, vox, i: integer;
   isOK: boolean;
   pct32, neg32 : TFloat32s;
      dlg : TSaveDialog;
   //dlg : TSaveDialog;
//label
//  123, 124, 125;
begin
    {$IFDEF Darwin}
    //showmessage('Select positive image (made with "Create Overlap Image")');
    {$ENDIF}
    fnm := OpenDialogExecute1(OpenDialog1.Filter, 'Select positive image (made with "Create Overlap Image")');
    niiPos := TNIfTI.Create(fnm, gPrefs.ClearColor, true, gPrefs.MaxTexMb, isOK);
    if not isOK then
       nPos := -1
    else
        nPos := parseIntentCode(niiPos);
    if (nPos < 1)  then begin
       niiPos.Free;
       showmessage('Expected NIfTI image created with "Create Overlap Image")');
       exit;

    end;

    {$IFDEF Darwin}
    //showmessage('Select negative image (made with "Create Overlap Image")');
    {$ENDIF}
    fnm := OpenDialogExecute1(OpenDialog1.Filter, 'Select negative image (made with "Create Overlap Image")');
    niiNeg := TNIfTI.Create(fnm, gPrefs.ClearColor, true, gPrefs.MaxTexMb, isOK);
    if not isOK then
       nNeg := -1
    else
        nNeg := parseIntentCode(niiNeg);
    if (nNeg < 1)  then begin
       niiNeg.Free;
       niiPos.Free;
       showmessage('Expected NIfTI image created with "Create Overlap Image")');
       exit;
    end;
    vox := niiPos.Header.dim[1] * niiPos.Header.dim[2] * niiPos.Header.dim[3];
    i   := niiNeg.Header.dim[1] * niiNeg.Header.dim[2] * niiNeg.Header.dim[3];
    if (vox < 1) or (vox <> i) then begin
       niiNeg.Free;
       niiPos.Free;
       showmessage('Images should have same number of voxels');
       exit;
    end;
    dlg := TSaveDialog.Create(self);
    dlg.Title := 'Save sum map as NIfTI volume';
    dlg.InitialDir := extractfiledir(gPrefs.PrevBackgroundImage);
    {$IFDEF Darwin}
    if PosEx('.app', dlg.InitialDir) > 0 then
       dlg.InitialDir := HomeDir(false);
    {$ENDIF}
    dlg.Filter := 'NIfTI|*.nii|Compressed NIfTI|*.nii.gz';
    dlg.DefaultExt := '*.nii';
    dlg.FilterIndex := 0;
    pct32 := niiPos.AsFloats();
    neg32 := niiNeg.AsFloats();
    for i := 0 to (vox -1) do
        pct32[i] := ((pct32[i] / nPos) - (neg32[i] / nNeg)) * 100;
    if dlg.Execute then
       niiPos.SaveAsSourceOrient(dlg.FileName, '', '%'+inttostr(nPos)+':'+inttostr(nNeg), TUInt8s(pct32), kDT_FLOAT32, kNIFTI_INTENT_ESTIMATE, -100, 100);
    pct32 := nil;
    neg32 := nil;
    niiNeg.Free;
    niiPos.Free;
end;

procedure TGLForm1.DisplayCorrelationRClick(Sender: TObject);
var
 niftiVol: TNIfTI;
 vox: TVec3i;
 i: integer;
begin
  if not vols.Layer(0,niftiVol) then exit;

  Vol1.Slice2Dmm(niftiVol, vox);
  if not vols.AddCorrelLayer(vox, gPrefs.ClearColor, (sender as TMenuItem).tag = 1) then exit;
 //if (vols.NumLayers > 1) then
 //  GLForm1.LayerChange(vols.NumLayers-1, vols.NumLayers-1, -1, kNaNsingle, kNaNsingle); //kNaNsingle
 //set color scheme
 i := GLForm1.LayerColorDrop.Items.IndexOf('8RedYell'); //search is case-insensitive!
 if i > 0 then
    LayerChange(vols.NumLayers-1, i, -1, kNaNsingle, kNaNsingle)
 else
     LayerChange(vols.NumLayers-1, vols.NumLayers-1, -1, kNaNsingle, kNaNsingle);
 UpdateLayerBox(true);
 UpdateColorbar();
 //Vol1.UpdateOverlays(vols);
 UpdateTimer.Enabled:= true;
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
  dir, x: string;
  isOpenAsOverlay: boolean;
begin
  ss := getKeyshiftstate;
  x := '';
  isOpenAsOverlay := (ssMeta in ss) or (ssCtrl in ss);
  if (Sender as TMenuItem).tag = 2 then begin
     dir := gPrefs.AfniDir;
     x := '.HEAD';
     if not FileExists(dir +pathdelim +(sender as TMenuItem).caption+x) then
        x := '';
  end else if (Sender as TMenuItem).tag = 1 then begin
     dir := AtlasDir;
     isOpenAsOverlay := not isOpenAsOverlay; //default behavior is to open template as overlay
  end else
      dir := StandardDir;
  if isOpenAsOverlay then
     AddLayer(dir +pathdelim +(sender as TMenuItem).caption+x)
  else
      AddBackground(dir +pathdelim +(sender as TMenuItem).caption+x, false);
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

procedure TGLForm1.SetToolPanelMaxWidth();
var
 mx: integer;
begin
     mx := LayerAlphaTrack.Left + LayerAlphaTrack.Width;
     mx := max(mx, MosColOverlapTrack.Left + MosColOverlapTrack.Width);
     mx := max(mx, CutNearBtn.Left + CutNearBtn.Width);
     mx := max(mx, ZCoordEdit.Left + ZCoordEdit.Width);
     mx := max(mx, QualityTrack.Left + QualityTrack.Width);
     ToolPanel.Constraints.MaxWidth:= mx+16;
end;

{$IFDEF DARKMODE}
procedure TGLForm1.SetFormDarkMode(f: TForm);
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
     {$IFDEF GRAPH}
     gGraph.DarkColorScheme();
     {$ENDIF}
  end else begin
      ScriptMemo.Color := clDefault;
      ScriptOutputMemo.Color := clDefault;
      MosaicText.Color := clDefault;
      {$IFDEF GRAPH}
      gGraph.GrayColorScheme();
      {$ENDIF}
  end;
  {$ENDIF}{$ENDIF}
end;
{$ENDIF}

procedure TGLForm1.CutoutChange(Sender: TObject);
function  tf(t: TTrackBar): single; //trackbar fraction
begin
     result := t.Position/t.Max;
end;
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
     CutoutBox.Hint := format('cutout(%.2f, %2f, %.2f, %.2f, %2f, %.2f)',[tf(XTrackBar),tf(YTrackBar),tf(ZTrackBar), tf(X2TrackBar),tf(Y2TrackBar),tf(Z2TrackBar)]);
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
        ScriptFormVisible(true);
        //ScriptPanel.Width := 240;
     gPrefs.PrevScript := scriptname;
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
     (*if i = 0 then begin
        gPrefs.FlipLR_Radiological := not gPrefs.FlipLR_Radiological;
        Vol1.Slices.RadiologicalConvention := gPrefs.FlipLR_Radiological;
        ViewGPU1.Invalidate;
        exit;
     end;*)
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


procedure TGLForm1.ImportTIFFMenuClick(Sender: TObject);
var
  openDlg: TOpenDialog;
begin
  openDlg := TOpenDialog.Create(application);
  openDlg.Title := 'Select TIFF image to convert';
  openDlg.InitialDir := extractfiledir(gPrefs.PrevBackgroundImage);
  if not Fileexists(openDlg.InitialDir) then
     openDlg.InitialDir := GetCurrentDir;
  openDlg.Filter := 'TIFF file|*.*|TIFF extensions|*.lsm;*.tif;*.tiff';
  openDlg.DefaultExt := '';
  openDlg.FilterIndex := 1;
  if not openDlg.Execute then begin
     openDlg.Free;
     exit;
  end;
  convertTiff2NIfTI(openDlg.Filename);
  openDlg.Free;
end;

procedure TGLForm1.ImportTIFFFolderMenuClick(Sender: TObject);
var
   indir: string;
begin
 indir := extractfiledir(gPrefs.PrevBackgroundImage);
 {$ifdef darwin}
 //Darwin does not show dialog title
 showmessage('Select a folder with all the 2D TIFFs (e.g. from DigiMorph or MorphoSource). They must be the same dimension and slice order matches alphabetical order of names.');
 if PosEx('.app/Contents/Resources', indir) > 0 then
    indir := '';
 {$endif}
 if indir = '' then
    indir := GetUserDir;
  convertTiffDir(indir);
end;

procedure TGLForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 gPrefs.CustomDcm2niix := dcm2niiForm.getCustomDcm2niix();
 IniFile(false, gPrefs);
 FreeandNil(Vol1);
end;

procedure TGLForm1.ImportDicomMenuClick(Sender: TObject);
begin
 {$IFDEF LCLCocoa}
 dcm2niiForm.show;
 {$ELSE}
 dcm2niiForm.showmodal;
 {$ENDIF}
end;


function  DefuzzX(const x:  single):  single;
//instead of "5.9e-6" write "0.0"
const
 fuzz = 1.0E-5;
begin
  if  ABS(x) < fuzz then exit(0.0);
  exit(x);
end {Defuzz};

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
  if Vols.OverlaysNeedsUpdate then isChange := true; //magma 20200606
  if (not isChange) then exit;
  if Vols.MaskWithBackground then ForceOverlayUpdate();
  niftiVol.SetDisplayMinMaxNoUpdate(mn, mx); //defer time consuming work
  niftiVol.OpacityPercent := pct;
  updateTimer.enabled := true;
end;

procedure TGLForm1.LayerAlphaTrackMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LayerWidgetChangeTimer.enabled := true;
  //LayerWidgetChange(sender);
end;

function absFrac(f: single): single;
begin
     result := frac(abs(f - round(f)));
end;

function XYZstr(xyz: TVec3): string;
var
   f: single;
begin
     f := max(absFrac(xyz.x), absFrac(xyz.y));
     f := max(absFrac(xyz.z), f);
     if (f < 0.001) then
        result := format(' %.0f×%.0f×%.0f', [xyz.x, xyz.y, xyz.z])
     else
         result := format(' %.1f×%.1f×%.1f', [xyz.x, xyz.y, xyz.z])
end;

procedure TGLForm1.MakeList(var c: TClusters);
var
   i, n: integer;
   itm: TListItem;
   isPeak, isLabels, isVolume: boolean;
begin
     ClusterView.Clear;
     n := length(c);
     if n < 1 then exit;
     isLabels := c[0].PeakStructure = '-';
     isVolume := c[0].PeakStructure = '~';
     isPeak := (not isLabels) and (not isVolume);
	 //if isVolume  then
     //  ClusterView.Column[1].Caption:= 'Mean'
     //else
     ClusterView.Column[1].Caption:= 'Volume';
     if isLabels then
        ClusterView.Column[2].Caption:= 'Index'
     else if isVolume then
        ClusterView.Column[2].Caption:= 'Mean'
     else
         ClusterView.Column[2].Caption:= 'Peak';
     //Column 0: index
     //Column 1: Volume
     //Column 2: Peak
     //Column 3: PeakXYZ
     //Column 4: PeakStructure
     //Column 5: XYZ
     //Column 6: Structure
     ClusterView.Column[2].Visible:= true;//not isLabels;
     ClusterView.Column[3].Visible:= isPeak;
     ClusterView.Column[4].Visible:= isPeak;
     for i := 0 to (n-1) do begin
         itm := ClusterView.Items.Add;
         itm.Caption := format('%d', [i]);
         //volume
         //if isVolume then
         //   itm.SubItems.Add(format('%.3f', [c[i].SzMM3]))
         //else
             itm.SubItems.Add(format('%d', [round(c[i].SzMM3)]));
         //PeakMax
         if isLabels then
             itm.SubItems.Add(format('%d', [round(c[i].Peak)]))
         else if isVolume then
         	  itm.SubItems.Add(format('%.3f', [c[i].Peak]))
         else
             itm.SubItems.Add(format('%.1f', [c[i].Peak]));
         //PeakXYZ
         itm.SubItems.Add(XYZstr(c[i].PeakXYZ));
         //itm.SubItems.Add(format(' %.1f!%.1f×%.1f', [c[i].PeakXYZ.x, c[i].PeakXYZ.y, c[i].PeakXYZ.z]));
         //PeakStructure
         itm.SubItems.Add(c[i].PeakStructure);
         //CogXYZ
         itm.SubItems.Add(XYZstr(c[i].CogXYZ));
         //itm.SubItems.Add(format(' %.0f×%.0f×%.0f', [c[i].CogXYZ.x, c[i].CogXYZ.y, c[i].CogXYZ.z]));
         //itm.SubItems.Add(format(' %.1f×%.1f×%.1f', [c[i].CogXYZ.x, c[i].CogXYZ.y, c[i].CogXYZ.z]));
         //Structure
         itm.SubItems.Add(c[i].Structure);
         //itm.SubItems.Add(format('%d:%s', [i, c[i].Structure])); //add index
     end;
end;

procedure TGLForm1.UpdateLayerBox(NewLayers: boolean; NewVolumes: boolean = false);
var
   i: integer;
   v: TNIfTI;
   s: string;
   isMultiVolLabel, isMultiVol, isAFNI, hasClusters, isMultiVol1: boolean;
begin
     isMultiVol1 := false;
     isMultiVol := false;
     hasClusters := false;
     isMultiVolLabel := false;
     if (not NewLayers) and (NewVolumes) and (vols.NumLayers > 0) then begin
        for i := 1 to vols.NumLayers do begin
            vols.Layer(i-1,v);
            if (v.VolumesTotal > 1) and (v.IsLabels) then isMultiVolLabel := true;
            if (v.VolumesTotal > 1) and (not v.IsLabels) then begin //IsLabels detect 2-volume TTatlas+tlrc.HEAD
               isMultiVol := true;
               if (i = 1) then //TGV
                  isMultiVol1 := true;
            end;
            s := v.ShortName;
            if (v.VolumesTotal > 1) then
               s := format('%d/%d: ', [v.VolumeDisplayed+1, v.VolumesTotal] )+s;
            LayerList.Items[i-1] := s;
            if length(v.clusters) > 0 then begin
               hasClusters := true;
               //Layerbox.caption := format('layer %d clusters %d', [i, length(v.clusters)]);
            end;
        end;
     end;
     if (NewLayers) then begin
        LayerList.Items.Clear;
        if vols.NumLayers < 1 then exit;
        v := nil;
        for i := 1 to vols.NumLayers do begin
            vols.Layer(i-1,v);
            if (v.VolumesTotal > 1) and (v.IsLabels) then isMultiVolLabel := true;
            if (v.VolumesTotal > 1) and (not v.IsLabels) then begin //IsLabels detect 2-volume TTatlas+tlrc.HEAD
               isMultiVol := true;
               if (i = 1) then //TGV
                  isMultiVol1 := true;
            end;
            s := v.ShortName;
            if v.VolumesTotal > 1 then
               s := format('%d/%d: ', [v.VolumeDisplayed+1, v.VolumesTotal] )+s;
            LayerList.Items.add(s);
            LayerList.Checked[i-1] := true;
            if length(v.clusters) > 0 then hasClusters := true;
        end;
        LayerList.ItemIndex := vols.NumLayers - 1;
        DisplayPrevMenu.Enabled := isMultiVol or isMultiVolLabel;
        DisplayNextMenu.Enabled := isMultiVol or isMultiVolLabel;
        DisplayAnimateMenu.Enabled := isMultiVol;
        DisplayCorrelationR.Enabled := isMultiVol;
        DisplayCorrelationZ.Enabled := isMultiVol;
        GraphAddMenu.Enabled := isMultiVol;
        if (v <> nil) and (gPrefs.AutoClusterizeAtlases) then begin
           if (v.IsLabels) and (length(v.clusters) < 1) then begin
              generateClustersCore(v, 1, 1, gPrefs.ClusterNeighborMethod, false);
              hasClusters := true;
           end;
        end;
     end;
     if (NewLayers) or (NewVolumes) then begin
        {$IFDEF GRAPH}
        if gGraph.PointsPerLine > 3 then
           isMultiVol1 := true;
        {$ENDIF}
        if hasClusters then begin
           if isMultiVol1 then
              GraphPanel.width := BottomPanel.Width div 2
           else
               GraphPanel.width := 8;
        end else if isMultiVol1 then
            GraphPanel.width := BottomPanel.ClientWidth - ClusterGraphSplitter.width - 8;
        if (not hasClusters) and (not isMultiVol1) then
           BottomPanel.Height := 8
        else if (BottomPanel.Height < 20) then
             BottomPanel.Height := round(GLForm1.Height * 0.25);
     end;
     if (LayerList.ItemIndex < 0)  then
        LayerList.ItemIndex := LayerList.Items.Count -1;
     if (vols.NumLayers < 0) or (vols.NumLayers < LayerList.Items.Count) then exit;
     if (LayerList.ItemIndex < 0) then exit;
     vols.Layer(LayerList.ItemIndex,v);
     LayerDarkEdit.Enabled := not v.Drawing;
     LayerBrightEdit.Enabled := not v.Drawing;
     LayerColorDrop.Enabled := not v.Drawing;
     if (v.IsLabels) or (v.Header.datatype = kDT_RGB) or (v.Header.datatype = kDT_RGBA32) then
        LayerColorDrop.Enabled := false;
     LayerDarkEdit.Text := format('%.6g', [v.DisplayMin]);
     LayerBrightEdit.Text := format('%.6g', [v.DisplayMax]);
     //writeln(format('>Set Min/Max Window %g..%g', [v.DisplayMin,v.DisplayMax]));
     LayerColorDrop.ItemIndex := v.FullColorTable.Tag;
     LayerAlphaTrack.Position := v.OpacityPercent;
     LayerBox.Hint := format('image intensity range %.4g..%.4g',[DefuzzX(v.VolumeMin), DefuzzX(v.VolumeMax)]);
     //ClusterView.Visible := length(v.clusters) > 0;
     if (NewLayers) or ((LayerList.ItemIndex <> ClusterView.tag) and (length(v.clusters) > 0 )) then begin
        MakeList(v.clusters); //tue
        //Layerbox.caption := format('layer %d clusters %d', [i, length(v.clusters)]);
        ClusterView.tag := LayerList.ItemIndex;

     end;
     (*LayerDtiDrop.Visible := v.fDTImode > kDTIno;
     if (v.fDTImode > kDTIno) then begin
	 	LayerDtiDrop.ItemIndex := v.fDTImode;
        if (v.fDTImode > kDTIscalar) then
           LayerColorDrop.enabled := false;
     end; *)
     {$IFDEF AFNI}
     isAFNI := length(v.afnis) > 0;
     if (isAFNI) and (v.VolumeDisplayed >=  length(v.afnis)) then
        isAFNI := false;
     if (isAFNI) and (v.afnis[0].jv = kFUNC_NO_STAT_AUX) then
        isAFNI := false; //e.g. raw fMRI with BRICK_FLOAT_FACS but no BRICK_STATAUX
     if  (isAFNI)  then begin
         LayerAfniDrop.Items.Clear;
         for i := 0 to (length(v.afnis)-1) do
             LayerAfniDrop.Items.Add(v.afnis[i].nam+' '+ AFNIjvLabel(v.afnis[i].jv) );
         LayerAfniDrop.ItemIndex := v.VolumeDisplayed ;
     end;
     LayerAfniDrop.Visible := isAFNI;
     LayerAfniBtn.Visible := isAFNI;
     {$ENDIF}
     //LayerClusterMenu.click;
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
     //if v.NeedsUpdate() then isChanged := true; //magma
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
     //writeln(format('?Set Min/Max Window %g..%g', [displayMin,displayMax]));
     UpdateTimer.Enabled := true;
end;

procedure TGLForm1.LayerListSelectionChange(Sender: TObject; User: boolean);
begin
     UpdateLayerBox(false);
end;

procedure TGLForm1.LayerContrastChange(Sender: TObject);
begin
 LayerWidgetChangeTimer.enabled := false;
 LayerWidgetChangeTimer.enabled := true;
end;

procedure TGLForm1.RulerVisible();
var
    clr: TRGBA;
begin
  if vols = nil then exit; //mungo : mosaic selected before image is loaded
  {$IFDEF CLRBAR}
  clr := gClrBar.RulerColor;
  if (RulerCheck.checked) then
      clr.A := 255
   else
       clr.A := 0;
   gClrbar.RulerColor := clr;
   {$ENDIF}
end;

procedure TGLForm1.RulerCheckChange(Sender: TObject);
begin
  if vols = nil then exit; //mungo : mosaic selected before image is loaded
 RulerVisible();
 ViewGPU1.Invalidate;
end;

procedure TGLForm1.ScriptHaltMenuClick(Sender: TObject);
begin
  (*if gPyRunning then begin
     //PyEngine.PyErr_SetString(PyExc_KeyboardInterrupt^, 'Terminated');
     //PyEngine.PyExc_KeyboardInterrupt();
     //ScriptOutputMemo.Lines.Add('Unable to run script: a script is already running.');
     PyEngine.PyErr_SetString(@PyEngine.PyExc_KeyboardInterrupt^, 'Terminated');
    exit;
  end;*)
end;

procedure TGLForm1.ScriptingPyVersionClick(Sender: TObject);
begin
 //Let user select python library
end;

procedure TGLForm1.ScriptingSepMenuClick(Sender: TObject);
begin

end;

procedure TGLForm1.LayerContrastKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
//     LayerWidgetChangeTimer.enabled := true;
end;

procedure TGLForm1.LineColorBtnClick(Sender: TObject);
var
 clr: TVec4;
begin
 clr := Vol1.Slices.LineColor;
 clr *= 255;
 ColorDialog1.Color:= RGBToColor(round(clr.r), round(clr.g), round(clr.b));
 if not ColorDialog1.Execute then exit;
 gPrefs.LineColor.R := Red(ColorDialog1.Color);
 gPrefs.LineColor.G := Green(ColorDialog1.Color);
 gPrefs.LineColor.B := Blue(ColorDialog1.Color);

 clr := Vec4(Red(ColorDialog1.Color)/255.0, Green(ColorDialog1.Color)/255.0, Blue(ColorDialog1.Color)/255.0, clr.a/255.0);
 Vol1.Slices.LineColor := clr;
 {$IFDEF CLRBAR}
 gClrbar.RulerColor := SetRGBA(clr);
 RulerVisible();
 {$ENDIF}
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
 ReportPositionXYZ(true);
end;

procedure GetRemoveHazePrefs(var isSmoothEdges, isSingleObject: boolean; var OtsuLevels: integer);
var
    PrefForm: TForm;
    CancelBtn,OkBtn: TButton;
    threshLabel: TLabel;
    threshEdit: TEdit;
    smoothCheck, singleCheck: TCheckBox;
begin
  PrefForm:=TForm.Create(nil);
  //PrefForm.SetBounds(100, 100, 512, 212);
  PrefForm.AutoSize := True;
  PrefForm.BorderWidth := 8;
  PrefForm.Caption:='Remove Haze Settings';
  PrefForm.Position := poScreenCenter;
  PrefForm.BorderStyle := bsDialog;
  //label
  threshLabel:=TLabel.create(PrefForm);
  threshLabel.Caption:= 'Threshold 1..5 (only brightest survive..dim voxels survive)';
  threshLabel.AutoSize := true;
  threshLabel.AnchorSide[akTop].Side := asrTop;
  threshLabel.AnchorSide[akTop].Control := PrefForm;
  threshLabel.BorderSpacing.Top := 6;
  threshLabel.AnchorSide[akLeft].Side := asrLeft;
  threshLabel.AnchorSide[akLeft].Control := PrefForm;
  threshLabel.BorderSpacing.Left := 6;
  threshLabel.Parent:=PrefForm;
  //edit
  threshEdit:=TEdit.create(PrefForm);
  threshEdit.Caption := IntToStr(OtsuLevels);
  threshEdit.Constraints.MinWidth:= 300;
  threshEdit.AutoSize := true;
  threshEdit.AnchorSide[akTop].Side := asrBottom;
  threshEdit.AnchorSide[akTop].Control := threshLabel;
  threshEdit.BorderSpacing.Top := 6;
  threshEdit.AnchorSide[akLeft].Side := asrLeft;
  threshEdit.AnchorSide[akLeft].Control := PrefForm;
  threshEdit.BorderSpacing.Left := 6;
  threshEdit.Parent:=PrefForm;
  //smoothCheck
  smoothCheck:=TCheckBox.create(PrefForm);
  smoothCheck.Checked := isSmoothEdges;
  smoothCheck.Caption:='Smooth Edges';
  smoothCheck.AnchorSide[akTop].Side := asrBottom;
  smoothCheck.AnchorSide[akTop].Control := threshEdit;
  smoothCheck.BorderSpacing.Top := 6;
  smoothCheck.AnchorSide[akLeft].Side := asrLeft;
  smoothCheck.AnchorSide[akLeft].Control := PrefForm;
  smoothCheck.BorderSpacing.Left := 6;
  smoothCheck.Anchors := [akTop, akLeft];
  smoothCheck.Parent:=PrefForm;
  //singleCheck
  singleCheck:=TCheckBox.create(PrefForm);
  singleCheck.Checked := isSingleObject;
  singleCheck.Caption:='Only extract the largest object';
  singleCheck.AnchorSide[akTop].Side := asrBottom;
  singleCheck.AnchorSide[akTop].Control := smoothCheck;
  singleCheck.BorderSpacing.Top := 6;
  singleCheck.AnchorSide[akLeft].Side := asrLeft;
  singleCheck.AnchorSide[akLeft].Control := PrefForm;
  singleCheck.BorderSpacing.Left := 6;
  singleCheck.Anchors := [akTop, akLeft];
  singleCheck.Parent:=PrefForm;

  //
  //Cancel Btn
  CancelBtn:=TButton.create(PrefForm);
  CancelBtn.Caption:='Cancel';
  CancelBtn.AutoSize := true;
  CancelBtn.AnchorSide[akTop].Side := asrBottom;
  CancelBtn.AnchorSide[akTop].Control := singleCheck;
  CancelBtn.BorderSpacing.Top := 8;
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
  OkBtn.AnchorSide[akTop].Control := singleCheck;
  OkBtn.BorderSpacing.Top := 8;
  OkBtn.AnchorSide[akLeft].Side := asrRight;
  OkBtn.AnchorSide[akLeft].Control := CancelBtn;
  OkBtn.BorderSpacing.Left := 6;
  OkBtn.Parent:=PrefForm;
  OkBtn.ModalResult:= mrOK;
  //OK button
  {$IFDEF DARKMODE}
  if gPrefs.DarkMode then GLForm1.SetFormDarkMode(PrefForm);
  {$ENDIF}
  PrefForm.ShowModal;
  OtsuLevels := 0;
  if (PrefForm.ModalResult = mrOK) then begin
    OtsuLevels := StrToInt(threshEdit.Caption);
    isSingleObject := singleCheck.Checked;
    isSmoothEdges := smoothCheck.Checked;
  end;
  FreeAndNil(PrefForm);
end; //GetRemoveHazePrefs()

procedure TGLForm1.RemoveHazeMenuClick(Sender: TObject);
var
 niftiVol: TNIfTI;
 isSmoothEdges: boolean = true;
 isSingleObject: boolean = true;
 OtsuLevels : integer = 5;
begin
  if not vols.Layer(0,niftiVol) then exit;
  if (Sender as TMenuItem).tag = 1 then
     GetRemoveHazePrefs(isSmoothEdges, isSingleObject, OtsuLevels);
  if OtsuLevels < 1 then exit; //user pressed cancel
  niftiVol.RemoveHaze(isSmoothEdges, isSingleObject, OtsuLevels);
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
  {$IFDEF DARKMODE}
  if gPrefs.DarkMode then GLForm1.SetFormDarkMode(PrefForm);
  {$ENDIF}
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
   //dlg : TSaveDialog;
begin
 if not vols.Layer(0,niftiVol) then exit;
 if (not vols.Drawing.IsOpen)  or (vols.Drawing.voiIsEmpty) then begin
     showmessage('No drawing is open. Nothing to save.');
     exit;
  end;
  (*dlg := TSaveDialog.Create(self);
  dlg.Title := 'Save drawing';
  dlg.InitialDir := ExtractFileDir(niftiVol.Filename);//GetUserDir;
  {$IFDEF Darwin}
 if PosEx('.app', dlg.InitialDir) > 0  then
       dlg.InitialDir := HomeDir(false);
 {$ENDIF}
  dlg.Filter := 'Volume of interest|*.voi|NIfTI|*.nii|Compressed NIfTI|*.nii.gz';
  dlg.DefaultExt := '.voi';
  dlg.FilterIndex := 0;
  if not dlg.Execute then begin
     dlg.Free;
     exit;
  end;
  fnm := dlg.FileName;
  dlg.Free;*)
  fnm := NiftiSaveDialogFilename(true, vols.Drawing.Filename);
  if fnm = '' then exit;
  vols.Drawing.Filename := fnm;
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

procedure TGLForm1.ExtractBrainMenuClick(Sender: TObject);
var
lFrac: double;
fnm: string;
saveDlg : TSaveDialog;
begin
 fnm := gPrefs.PrevBackgroundImage;
lFrac := GetFloat('Brain extraction fraction',0.1,0.45,0.9);
if (lFrac= kNaN) then exit;
OpenDialog1.FileName := '';
if fileexists(fnm) and isNifti(fnm) then begin
   if MessageDlg('Choose image', 'Do you wish extract '+ fnm+'?', mtConfirmation, [mbYes, mbNo],0) = mrYes then
      OpenDialog1.FileName := fnm;
end;
if (OpenDialog1.FileName = '') then
    if not OpenDialog1.Execute then
       exit;
if not isNifti(OpenDialog1.FileName) then
   showmessage('BET brain extraction requires NIfTI format images.');
//we could just rename c:\img.nii to c:\bimg.nii, however MacOS sandboxing will not allow us to save to novel location
saveDlg := TSaveDialog.Create(application);
saveDlg.Title := 'Brain Extracted Image';
saveDlg.InitialDir := extractfiledir(fnm);
saveDlg.Filename := 'b'+extractfilename(ChangeFileExtX(fnm,''));
if not Fileexists(saveDlg.InitialDir) then
   saveDlg.InitialDir := GetCurrentDir;
{$IFDEF Darwin}
if PosEx('.app/Contents/Resources', saveDlg.InitialDir) > 0 then begin
      saveDlg.InitialDir := GetCurrentDir;
end;
{$ENDIF}
//saveDlg.Filter := 'Text file|*.txt';
//saveDlg.DefaultExt := 'txt';
//saveDlg.FilterIndex := 1;
if not saveDlg.Execute then begin
   saveDlg.Free;
   exit;
end;
fnm :=  saveDlg.filename;
saveDlg.Free;
fnm := FSLbet(OpenDialog1.FileName,lFrac,fnm);
AddBackground(fnm);

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
  LayerZeroIntensityInvisibleMenu.Checked := niftiVol.ZeroIntensityInvisible;
  LayerIsLabelMenu.Checked := niftiVol.IsLabels;
  LayerSmoothMenu.Checked := Vols.InterpolateOverlays;
  LayerInvertColorMapMenu.Checked := niftiVol.CX.InvertColorMap;
  LayerExport8BitMenu.Enabled := (niftiVol.Header.datatype = kDT_INT16) or (niftiVol.Header.datatype = kDT_FLOAT);
  LayerPrevVolumeMenu.Enabled := (niftiVol.VolumesTotal > 1);
  LayerNextVolumeMenu.Enabled := LayerPrevVolumeMenu.Enabled;
  LayerClusterMenu.Enabled := not niftiVol.IsLabels;
  LayerClusterOptsMenu.Enabled := not niftiVol.IsLabels;
  LayerClusterMeansMenu.Enabled := (not niftiVol.IsLabels) and (HasLabelLayer());
  if LayerList.Count < 2 then
     LayerClusterMeansMenu.Enabled := false; // requires selected continuous layer as well as an ATLAS
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
  bmpEdit, maxVoxEdit: TEdit;
  LoadFewVolumesCheck, LandMarkCheck,
   {$IFDEF DARKMODE}DarkModeCheck, {$ENDIF}
  {$IFDEF LCLCocoa}  RetinaCheck,{$ENDIF}
  ClusterizeAtlasCheck, BitmapAlphaCheck, RadiologicalCheck: TCheckBox;
  OkBtn, AdvancedBtn: TButton;
  bmpLabel, maxVoxLabel: TLabel;
  RenderQCombo, WindowCombo : TComboBox;
  {$IFDEF DARKMODE} isDarkModeChanged, {$ENDIF}
  {$IFDEF LCLCocoa} isRetinaChanged, {$ENDIF}
  isAdvancedPrefs: boolean;
begin
  PrefForm:=TForm.Create(GLForm1);
  PrefForm.AutoSize := true;
  PrefForm.BorderWidth := 8;
  PrefForm.Caption:='Preferences';
  PrefForm.Position := poScreenCenter;
  PrefForm.BorderStyle := bsDialog;
  //RenderQuality
  RenderQCombo:=TComboBox.create(PrefForm);
  RenderQCombo.Parent:=PrefForm;
  RenderQCombo.AutoSize:= true;
  RenderQCombo.Width := 420;
  RenderQCombo.Items.Add('Render Quality: Adaptive (flicker)');
  RenderQCombo.Items.Add('Render Quality: Poorest (fastest)');
  RenderQCombo.Items.Add('Render Quality: Poor');
  RenderQCombo.Items.Add('Render Quality: Medium');
  RenderQCombo.Items.Add('Render Quality: Better');
  RenderQCombo.Items.Add('Render Quality: Best (slowest)');
  RenderQCombo.ItemIndex:=  gPrefs.Quality1to5;
  RenderQCombo.Style := csDropDownList;
  RenderQCombo.AnchorSide[akTop].Side := asrTop;
  RenderQCombo.AnchorSide[akTop].Control := PrefForm;
  RenderQCombo.BorderSpacing.Top := 6;
  RenderQCombo.AnchorSide[akLeft].Side := asrLeft;
  RenderQCombo.AnchorSide[akLeft].Control := PrefForm;
  RenderQCombo.BorderSpacing.Left := 6;
  RenderQCombo.Anchors := [akTop, akLeft];
  RenderQCombo.Parent:=PrefForm;
  //Startup window size
  WindowCombo:=TComboBox.create(PrefForm);
  WindowCombo.Parent:=PrefForm;
  WindowCombo.AutoSize:= true;
  WindowCombo.Width := 420;
  WindowCombo.Items.Add('Startup: As Window');
  WindowCombo.Items.Add('Startup: Maximized');
  WindowCombo.Items.Add('Startup: Full Screen');
  WindowCombo.ItemIndex:=  gPrefs.StartupWindowMode;
  WindowCombo.Style := csDropDownList;
  WindowCombo.AnchorSide[akTop].Side := asrBottom;
  WindowCombo.AnchorSide[akTop].Control := RenderQCombo;
  WindowCombo.BorderSpacing.Top := 6;
  WindowCombo.AnchorSide[akLeft].Side := asrLeft;
  WindowCombo.AnchorSide[akLeft].Control := PrefForm;
  WindowCombo.BorderSpacing.Left := 6;
  WindowCombo.Anchors := [akTop, akLeft];
  WindowCombo.Parent:=PrefForm;
  //Bitmap Scale
  maxVoxLabel:=TLabel.create(PrefForm);
  maxVoxLabel.Width := PrefForm.Width - 86;
  maxVoxLabel.Caption := 'Maximum Texture Size (mb, 64 for software rendering, default '+inttostr(kMaxTexMb)+', 2048 for 8Gb graphics cards)';
  maxVoxLabel.Hint := 'If using software rather than hardware graphics, consider 256';
  maxVoxLabel.AnchorSide[akTop].Side := asrBottom;
  maxVoxLabel.AnchorSide[akTop].Control := WindowCombo;
  maxVoxLabel.BorderSpacing.Top := 6;
  maxVoxLabel.AnchorSide[akLeft].Side := asrLeft;
  maxVoxLabel.AnchorSide[akLeft].Control := PrefForm;
  maxVoxLabel.BorderSpacing.Left := 6;
  maxVoxLabel.Anchors := [akTop, akLeft];
  maxVoxLabel.Parent:=PrefForm;
  //maxVox edit
  maxVoxEdit := TEdit.Create(PrefForm);
  maxVoxEdit.Width := 60;
  maxVoxEdit.AutoSize := true;
  maxVoxEdit.Text := inttostr(gPrefs.MaxTexMb);
  maxVoxEdit.AnchorSide[akTop].Side := asrCenter;
  maxVoxEdit.AnchorSide[akTop].Control := maxVoxLabel;
  maxVoxEdit.BorderSpacing.Top := 6;
  maxVoxEdit.AnchorSide[akLeft].Side := asrRight;
  maxVoxEdit.AnchorSide[akLeft].Control := maxVoxLabel;
  maxVoxEdit.BorderSpacing.Left := 8;
  maxVoxEdit.Anchors := [akTop, akLeft];
  maxVoxEdit.Parent:=PrefForm;
  //Bitmap Scale
  bmpLabel:=TLabel.create(PrefForm);
  bmpLabel.Width := PrefForm.Width - 86;
  bmpLabel.Caption := 'Bitmap zoom (large values create huge images)';
  bmpLabel.AnchorSide[akTop].Side := asrBottom;
  bmpLabel.AnchorSide[akTop].Control := maxVoxEdit;
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
  //BitmapAlphaCheck
  BitmapAlphaCheck:=TCheckBox.create(PrefForm);
  BitmapAlphaCheck.Checked := gPrefs.ScreenCaptureTransparentBackground;
  BitmapAlphaCheck.Caption:='Background transparent in bitmaps';
  //BitmapAlphaCheck.Left := 8;
  //BitmapAlphaCheck.Top := 8;
  BitmapAlphaCheck.AutoSize := true;
  BitmapAlphaCheck.AnchorSide[akTop].Side := asrBottom;
  BitmapAlphaCheck.AnchorSide[akTop].Control := bmpLabel;
  BitmapAlphaCheck.BorderSpacing.Top := 6;
  BitmapAlphaCheck.AnchorSide[akLeft].Side := asrLeft;
  BitmapAlphaCheck.AnchorSide[akLeft].Control := PrefForm;
  BitmapAlphaCheck.BorderSpacing.Left := 6;
  BitmapAlphaCheck.Parent:=PrefForm;
  ClusterizeAtlasCheck:=TCheckBox.create(PrefForm);
  ClusterizeAtlasCheck.Checked := gPrefs.AutoClusterizeAtlases;
  ClusterizeAtlasCheck.Caption:='Show cluster table for atlases';
  ClusterizeAtlasCheck.AnchorSide[akTop].Side := asrBottom;
  ClusterizeAtlasCheck.AnchorSide[akTop].Control := BitmapAlphaCheck;
  ClusterizeAtlasCheck.BorderSpacing.Top := 6;
  ClusterizeAtlasCheck.AnchorSide[akLeft].Side := asrLeft;
  ClusterizeAtlasCheck.AnchorSide[akLeft].Control := PrefForm;
  ClusterizeAtlasCheck.BorderSpacing.Left := 6;
  ClusterizeAtlasCheck.Anchors := [akTop, akLeft];
  ClusterizeAtlasCheck.Parent:=PrefForm;
  //  RadiologicalCheck
  RadiologicalCheck:=TCheckBox.create(PrefForm);
  RadiologicalCheck.Checked := gPrefs.FlipLR_Radiological;
  RadiologicalCheck.Caption:='Radiological convention (left on right)';
  RadiologicalCheck.AnchorSide[akTop].Side := asrBottom;
  RadiologicalCheck.AnchorSide[akTop].Control := ClusterizeAtlasCheck;
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
  RetinaCheck :=TCheckBox.create(PrefForm);
  {$IFNDEF RETINA} RetinaCheck.Enabled := false;{$ENDIF}
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
  {$IFDEF DARKMODE}
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

  {$ENDIF}
  //OK button
  OkBtn:=TButton.create(PrefForm);
  OkBtn.Caption:='OK';
  OkBtn.Width:= 100;
  OkBtn.AutoSize := true;
  OkBtn.AnchorSide[akTop].Side := asrBottom;
  {$IFDEF DARKMODE}
  OkBtn.AnchorSide[akTop].Control := DarkModeCheck;
  {$ELSE}
  {$IFDEF LCLCocoa}
  if RetinaCheck.visible then
     OkBtn.AnchorSide[akTop].Control := RetinaCheck
  else
  {$ENDIF}
  OkBtn.AnchorSide[akTop].Control := LandmarkCheck;
  {$ENDIF}
  OkBtn.BorderSpacing.Top := 6;
  OkBtn.AnchorSide[akLeft].Side := asrRight;
  OkBtn.AnchorSide[akLeft].Control := bmpLabel;
  OkBtn.BorderSpacing.Left := 6;
  OkBtn.Anchors := [akTop, akLeft];
  OkBtn.Parent:=PrefForm;
  OkBtn.ModalResult:= mrOK;
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
  gPrefs.AutoClusterizeAtlases := ClusterizeAtlasCheck.Checked;
  Vol1.Slices.RadiologicalConvention := gPrefs.FlipLR_Radiological;
  gPrefs.BitmapZoom:= strtointdef(bmpEdit.Text,1);
  gPrefs.MaxTexMb :=  strtointdef(maxVoxEdit.Text, kMaxTexMb);
  gPrefs.MaxTexMb := max(1, gPrefs.MaxTexMb);
  vols.MaxVox := gPrefs.MaxTexMb;
  gPrefs.LoadFewVolumes := LoadFewVolumesCheck.Checked;
  if (gPrefs.ScreenCaptureTransparentBackground <>  BitmapAlphaCheck.Checked) then begin
     gPrefs.ScreenCaptureTransparentBackground :=  BitmapAlphaCheck.Checked;
     if gPrefs.ScreenCaptureTransparentBackground then
        gPrefs.ClearColor.A := 0
     else
         gPrefs.ClearColor.A := 255;
  end;
  gPrefs.Quality1to5 := RenderQCombo.ItemIndex;
  if gPrefs.Quality1to5 <> Vol1.Quality1to5 then begin
     Vol1.Quality1to5 := gPrefs.Quality1to5;
     QualityTrack.Position := gPrefs.Quality1to5;
  end;
  gPrefs.StartupWindowMode := WindowCombo.ItemIndex;
  vols.LoadFewVolumes  := gPrefs.LoadFewVolumes;
  if gPrefs.BitmapZoom < 1 then gPrefs.BitmapZoom := 1;
  if gPrefs.BitmapZoom > 10 then gPrefs.BitmapZoom := 10;
  isAdvancedPrefs := (PrefForm.ModalResult = mrYesToAll);
  if (gPrefs.LandmarkPanel <> LandmarkCheck.Checked) then begin
     gPrefs.LandmarkPanel := LandmarkCheck.Checked;
     UpdateVisibleBoxes();
  end;
  {$IFDEF LCLCocoa}
  isRetinaChanged := gPrefs.RetinaDisplay <> RetinaCheck.Checked;
  gPrefs.RetinaDisplay := RetinaCheck.Checked;
  {$IFDEF DARKMODE}
  isDarkModeChanged := gPrefs.DarkMode <> DarkModeCheck.Checked;
  gPrefs.DarkMode := DarkModeCheck.Checked;
  {$ENDIF}
  {$ENDIF}
  FreeAndNil(PrefForm);
  if  isAdvancedPrefs then begin
    GLForm1.Refresh;
    GLForm1.Quit2TextEditor;
    exit;
  end;
  {$IFDEF LCLCocoa}
  {$IFDEF DARKMODE}
  if isDarkModeChanged then
       GLForm1.SetDarkMode();
  {$ENDIF}
  if isRetinaChanged then begin
     //ViewGPU1.MakeCurrent(false);

     GLForm1.SetRetina;
     ViewGPU1.Align:= alNone;
     ViewGPU1.width := 30;
     ViewGPU1.Align:= alClient;


     //ViewGPU1.ReleaseContext;

     //
  end;
  {$ENDIF}
  ViewGPU1.Invalidate;
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

function TGLForm1.NiftiSaveDialogFilename(isVOI: boolean = false; initialFilename: string = ''): string;
const
     kOutFilter = 'NIfTI (.nii)|*.nii|Compressed NIfTI (.nii.gz)|*.nii.gz|Volume of interest (.voi)|*.voi|Blender Volume (.bvox)|*.bvox|OSPRay Volume (.osp)|*.osp|TIF (.tif)|*.tif|NRRD (.nrrd)|*.nrrd|Compressed NRRD (.nhdr)|*.nhdr';
     kMaxExt = 8;
     kExt : array [1..kMaxExt] of string = ('*.nii', '*.nii.gz', '*.voi', '*.bvox', '*.osp', '*.tif', '*.nrrd', '*.nhdr');
var
   dlg : TSaveDialog;
   ext: string = '';
   i, len : integer;
   idx: integer = 0;
begin
  result := '';
  dlg := TSaveDialog.Create(self);
  dlg.Options := [ofEnableSizing, ofViewDetail]; //disable ofOverwritePrompt
  dlg.Title := 'Save NIfTI volume';
  dlg.InitialDir := extractfiledir(gPrefs.PrevBackgroundImage);
  if ( initialFilename <> '') then begin
     dlg.InitialDir := extractfilepath(initialFilename);
     //dlg.FileName := ChangeFileExt(extractfilename(initialFilename), '');
     ext := extractfileextX(initialFilename);
     dlg.FileName := extractfilename(initialFilename);
     dlg.FileName := copy(dlg.FileName, 1, length(dlg.FileName)-length(ext));
     for i := 1 to kMaxExt do
     	 if (ext  = extractfileextX(kExt[i])) then
         	idx := i;
  end;
  {$IFDEF Darwin}
  if PosEx('.app', dlg.InitialDir) > 0  then
       dlg.InitialDir := HomeDir(false);
  {$ENDIF}
  dlg.Filter := kOutFilter;
  if idx > 0 then begin
     dlg.DefaultExt := kExt[idx];
     dlg.FilterIndex:= idx;
  end else if isVoi then begin
     if (gPrefs.VoiSaveFormat < 1) or (gPrefs.VoiSaveFormat > kMaxExt) then
        gPrefs.VoiSaveFormat := 1;
     dlg.DefaultExt := kExt[gPrefs.VoiSaveFormat];
     dlg.FilterIndex:= gPrefs.VoiSaveFormat;
     //dlg.FileName:= inttostr(gPrefs.VoiSaveFormat);
  end else begin
    if (gPrefs.VolumeSaveFormat < 1) or (gPrefs.VolumeSaveFormat > kMaxExt) then
       gPrefs.VolumeSaveFormat := 1;
    dlg.DefaultExt := kExt[gPrefs.VolumeSaveFormat];
    dlg.FilterIndex:= gPrefs.VolumeSaveFormat;
  end;
  //niftiVol.SaveBVox('/Users/rorden/tmp/v.bvox'); exit;
  if not dlg.Execute then begin
  	 dlg.Free;
     exit;
  end;
  result := dlg.FileName;
  //
  ext := extractfileextX(result);
  len := length(ext);
  idx := 0;
  for i := 1 to kMaxExt do
  	  if (ext  = extractfileextX(kExt[i])) then
      	 idx := i;
  //i := idx; //0 if idx not found
  if idx = 0 then
  	 idx := dlg.FilterIndex;
  result := copy(result, 1, length(result)-len)+LowerCase(extractfileextX(kExt[idx]));
  if fileexists(result) then begin
  	 if MessageDlg('“'+extractfilename(result)+'” already exists. Do you want to replace it?', mtConfirmation,[mbYes, mbNo], 0) = mrNo then begin
  	 	result := '';
     	dlg.Free;
        exit;
     end;
  end;
  if isVoi then
  	 gPrefs.VoiSaveFormat :=  dlg.FilterIndex
  else
  	  gPrefs.VolumeSaveFormat :=  dlg.FilterIndex;
  dlg.Free;
end;

procedure TGLForm1.ClipIntensity(Lo, Hi: single);
var
   niftiVol: TNIfTI;
   fnm : string;
begin
 if not vols.Layer(0,niftiVol) then exit;
 fnm := NiftiSaveDialogFilename();
 if fnm = '' then exit;
 niftiVol.SaveAs8Bit(fnm, Lo, Hi); exit;
end;

procedure TGLForm1.SaveNIfTIMenuClick(Sender: TObject);
var
   niftiVol: TNIfTI;
   fnm : string;
begin
 if not vols.Layer(0,niftiVol) then exit;
 fnm := NiftiSaveDialogFilename(false, niftiVol.Filename);
 if fnm = '' then exit;
 niftiVol.SaveFormatBasedOnExt(fnm);
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

procedure TGLForm1.SliceZoomChange(Sender: TObject);
begin
  if Vol1 = nil then exit;
  {$IFDEF LCLgtk3} if not GLForm1.Visible then exit; {$ENDIF}
  Vol1.Slices.ZoomScale := SliceZoom.position/100;
  ViewGPU1.Invalidate;
end;

procedure TGLForm1.SmoothCheckChange(Sender: TObject);
begin
 if vols = nil then exit; //mungo : mosaic selected before image is loaded
 Vol1.ShowSmooth2D:= Smooth2DCheck.checked;
 ViewGPU1.Invalidate;
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

procedure TGLForm1.UpdateColorbar;
var
 i, n: integer;
 v: TNIfTI;
begin
  {$IFDEF CLRBAR}
  n := vols.NumLayers;
  if n < 1 then exit;
  if n < 2 then begin //with single background image show color range for background
    if not vols.Layer(0, v) then exit;
    if (v.IsLabels) or  (v.Header.datatype = kDT_RGBA32) or (v.Header.datatype = kDT_RGB) then exit;
    gClrbar.SetLUT(1, v.GetColorTable, v.DisplayMin, v.DisplayMax, v.CX.FromZero);
    gClrbar.Number := 1;
  end else begin //when overlays are loaded, show color range of overlays
      gClrbar.Number := 0;
      for i := 1 to (n-1) do begin
          if not vols.Layer(i, v) then exit;
          if (v.IsLabels) or  (v.Header.datatype = kDT_RGBA32) or (v.Header.datatype = kDT_RGB) then continue;
          gClrbar.SetLUT(i, v.GetColorTable, v.DisplayMin, v.DisplayMax, v.CX.FromZero);
          gClrbar.Number := gClrbar.Number + 1;
      end;
  end;
  {$ENDIF}
end;

procedure TGLForm1.ReportPositionXYZ(isUpdateYoke: boolean = false);
var
   str: string;
   niftiVol: TNIfTI;
   sliceMM, endMM, diffMM: TVec3;
   len: single;
   vox: TVec3i;
   i: integer;
   graphLine: TFloat32s;
begin
     if not vols.Layer(0,niftiVol) then exit;
     sliceMM := Vol1.Slice2Dmm(niftiVol, vox);
     {$IFDEF GRAPH}
     if (niftiVol.volumesLoaded > 1) and (not niftiVol.IsLabels) then begin
       graphLine :=  niftiVol.VoxIntensityArray(vox);
       if (length(graphLine) <> niftiVol.volumesLoaded) then begin
          setlength(graphLine, 0);
          exit;
       end;
       gGraph.HorizontalSelection:= niftiVol.VolumeDisplayed;
       if (niftiVol.VolumesLoaded < niftiVol.VolumesTotal) then
          gGraph.AddLine(graphLine,format('%s (%d of %d) [%d %d %d]',[niftiVol.ShortName, niftiVol.VolumesLoaded, niftiVol.VolumesTotal, vox.x, vox.y, vox.z]), true)
       else
           gGraph.AddLine(graphLine,format('%s [%d %d %d]',[niftiVol.ShortName, vox.x, vox.y, vox.z]), true);
       ViewGPUg.Invalidate;
       //gGraph.isRedraw:= true;  //2021
       setlength(graphLine, 0);
     end;
     {$ENDIF}
     str := '';
     {$IFDEF COMPILEYOKE}
     if (isUpdateYoke) then begin
        gSliceMM := Vec3(sliceMM.X, sliceMM.Y, sliceMM.Z);
        SetShareFloats2D(sliceMM.X,sliceMM.Y,sliceMM.Z);
     end;
     {$ENDIF}
     if (Vol1.Slices <> nil) and (Vol1.Slices.distanceLineOrient <> 0) then begin
       sliceMM := Vol1.Slices.FracMM(Vol1.Slices.distanceLineStart, niftivol.Mat, niftivol.Dim);
       endMM := Vol1.Slices.FracMM(Vol1.Slices.distanceLineEnd, niftivol.Mat, niftivol.Dim);
       diffMM := sliceMM - endMM;
       len := diffMM.length;
       str := str + format('%0.4g×%0.4g×%0.4g -> %0.4g×%0.4g×%0.4g  = %0.4g', [sliceMM.x, sliceMM.y, sliceMM.z, endMM.x, endMM.y, endMM.z, len]);
       caption := str;
       exit;
     end;
     if (niftiVol.Header.xyzt_units and kNIFTI_UNITS_MM) = kNIFTI_UNITS_MM then
        str := str + format('%0.6g×%0.6g×%0.6gmm (%d×%d×%d) = ', [sliceMM.x, sliceMM.y, sliceMM.z, vox.x, vox.y, vox.z])
     else
         str := str + format('%0.6g×%0.6g×%0.6g = ', [sliceMM.x, sliceMM.y, sliceMM.z]);
     str := str + niftiVol.VoxIntensityString(vox);//format('%3.6g', [niftiVol.VoxIntensity(vox)]);
     if vols.NumLayers > 1 then
        for i := 1 to (vols.NumLayers-1) do begin
            if not vols.Layer(i,niftiVol) then exit;
            str := str + '; ' + niftiVol.VoxIntensityString(vox);
        end;
     caption := str;
end;

procedure TGLForm1.SetXHairPosition (lXmm,lYmm,lZmm: single; isUpdateYoke: boolean = false);
var
   vFrac: TVec3;
   niftiVol: TNIfTI;
   sliceMove: TVec3i;
begin
     if not vols.Layer(0,niftiVol) then exit;
     vFrac := niftiVol.MMFrac(Vec3(lXmm,lYmm,lZmm));
     //next 2 lines constrain 2D images
     sliceMove:= pti(0,0,0);
     vFrac := niftiVol.FracShiftSlice(vFrac, sliceMove); //move a desired number of slices
     vol1.SetSlice2DFrac(vFrac);
     ReportPositionXYZ(isUpdateYoke);
     ViewGPU1.Invalidate;
     //{$DEFINE XDEBUG}
     {$IFDEF XDEBUG}
     ScriptOutputMemo.Lines.Clear;
     ScriptOutputMemo.Lines.Add(format('XYZmm=[%g %g %g]',[lXmm,lYmm,lZmm]));
     ScriptOutputMemo.Lines.Add(format('Frac=[%g %g %g]',[vFrac.x,vFrac.y,vFrac.z]));
     ScriptFormVisible(true);
     {$ENDIF}
end;

procedure TGLForm1.CoordEditChange(Sender: TObject);
begin
 SetXHairPosition(StrToFloatDef(XCoordEdit.Text,0),StrToFloatDef(YCoordEdit.Text,0),StrToFloatDef(ZCoordEdit.Text,0), true );
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

function erfc(x: extended):extended;
const
 sqrtPi = 1.772453851;
 Terms = 12;
var
  x2,u,v,sum: extended;
  i: integer;
begin
     x2 := x*x;
     v := 1.0/ (2.0*x2);
     u := 1.0 + v*(Terms+1.0);
     for i := Terms downto 1 do begin
         sum := 1.0 + i*v/u;
         u := sum;
     end;
     result := exp(-x2)/(x*sum*sqrtPi);
end;

procedure TGLForm1.ScriptingSaveMenuClick(Sender: TObject);
var
  fnm, dir: string;
  dlg : TSaveDialog;
begin
 fnm := (gPrefs.PrevScript);
 if (FileGetAttr(fnm) and faReadOnly) <>0 then
    fnm := extractfilename(fnm);
 {$IFDEF Darwin}
 if AnsiContainsText(fnm, '.app/') then
    fnm := extractfilename(fnm);
 {$ENDIF}
 if fnm = '' then
    fnm := 'myScript.py';
  dir := ExtractFileDir(fnm);
  if (dir = '') or (not DirectoryExists(dir)) then
     dir := HomeDir(false);
  dlg := TSaveDialog.Create(self);
  dlg.InitialDir:= dir;
  dlg.Filename := fnm;
  dlg.DefaultExt := '.py';
  dlg.Filter := 'Python Script|*.py';

  dlg.Options := [ofEnableSizing, ofViewDetail, ofNoReadOnlyReturn, ofOverwritePrompt];
  if not dlg.Execute then begin
     dlg.Free;
     exit;
  end;
  fnm := ChangeFileExt(dlg.Filename,'.py');
  dlg.Free;
  ScriptMemo.Lines.SaveToFile(fnm);
  gPrefs.PrevScript := fnm;

end;

procedure TGLForm1.ScriptingRunMenuClick(Sender: TObject);
begin
  if (vols.Drawing.NeedsSave) then begin
     showmessage('Please close or save your drawing before running a script.');
     exit;
  end;
 //DrawSaveMenuClick
  {$IFDEF MYPY}
 if gPyRunning then begin
    {$IFDEF PY4LAZ}
    PyEngine.PyErr_SetString(PyEngine.PyExc_KeyboardInterrupt^, 'Operation cancelled') ;
    {$ELSE}
    PyInterrupt();  //TODO!!!!
    {$ENDIF}
   ScriptOutputMemo.Lines.Add('Halting script ("Run" called while script still running).');
   exit;
 end;
 if PyExec() then exit;
 if (not (AnsiContainsText(ScriptMemo.Text, 'import gl'))) then begin
     ScriptOutputMemo.Lines.Clear;
     ScriptOutputMemo.Lines.Add('Error: script must contain "import gl" (for Python) or "begin" (for Pascal).');
     exit;
 end;
 {$ELSE}
 //Showmessage('Scripting not yet implemented');
 ScriptOutputMemo.Lines.Add('Not compiled for scripting');
 printf('Not compiled for scripting');
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
  if (vols.NumLayers > 0) then
     MatLoadForce(kMatNoForce);
  result := vols.AddLayer(fnm, gPrefs.ClearColor);
  //handle AutoClusterizeAtlases in UpdateLayerBox() so it impacts startup images
  if (vols.NumLayers > 1) then
    GLForm1.LayerChange(vols.NumLayers-1, vols.NumLayers-1, -1, kNaNsingle, kNaNsingle); //kNaNsingle
  UpdateLayerBox(true);
  UpdateColorbar();
  //LayerClusterMenuClick(nil);
  //Vol1.UpdateOverlays(vols);
  UpdateTimer.Enabled:= true;
  //ViewGPU1.Invalidate;
end;

function TGLForm1.AddBackground(Filename: string; isAddToRecent: boolean = true; isFromFormDrop: boolean = false): boolean;
//close all open layers and add a new background layer
var
   {$IFDEF MATT1}
   ext: string;
   ss: TShiftState;
   {$ENDIF}
 fnm: string;
 FileNames: array of String ;
 MatHasLesion: boolean = false;
begin
  AnimateTimer.Enabled := false;
  DisplayAnimateMenu.Checked := false;
  if CropForm.visible then CropForm.close;
  fnm := Filename;
  fnm := GetFullPath(fnm);
  if not Fileexists(fnm) then exit(false);
  vols.CloseAllLayers;
  //TGV
  {$IFDEF GRAPH}
  GraphClearMenuClick(nil);
  {$ENDIF}
  CutNoneBtnClick(nil); //turn off cut-out: not persistent like clip plane
  {$IFDEF MATT1}
  ext := uppercase(extractfileext(fnm));
  if (ext = '.MAT') then begin
     ss := getKeyshiftstate;
     MatHasLesion := MatTestHasAnatomical(fnm, (ssShift in ss));
  end;
  {$ENDIF}
  if (not AddLayer(fnm)) then begin
    if (isFromFormDrop) then exit(false);
    setlength(Filenames, 1);
    Filenames[0] := fnm;
    FormDropFiles(nil, FileNames);
    Filenames := nil;
    exit(false);
  end;
  gPrefs.PrevBackgroundImage := fnm; //launch with last image, even if it is template/fsl image
  if (isAddToRecent) then AddOpenRecent(fnm);
  {$IFDEF MATT1}
  if (MatHasLesion) then begin
     MatLoadForce(kMatForceLesion);
     if (vols.Drawing.IsOpen) and (not vols.Drawing.voiIsEmpty) and (vols.Drawing.NeedsSave) then
        DrawSaveMenuClick(nil);
     Vols.OpenDrawing(fnm);
  end;
  //if (ext = '.MAT') then begin
     //MatLoadForce(kMatForceLesion);
     //AddLayer(fnm);
     //i := GLForm1.LayerColorDrop.Items.IndexOf('actc'); //search is case-insensitive!
     //if i > 0 then
     //   GLForm1.LayerChange(1, i, -1, kNaNsingle, kNaNsingle); //kNaNsingle
  //end;
  (*if (ext = '.MAT') then begin
     MatLoadForce(kMatForcefMRI);
     AddLayer(fnm);
     i := GLForm1.LayerColorDrop.Items.IndexOf('actc'); //search is case-insensitive!
     if i > 0 then
        GLForm1.LayerChange(1, i, -1, kNaNsingle, kNaNsingle); //kNaNsingle
  end;  *)
  {$ENDIF}
  //if not YokeMenu.Checked then begin
  if true then begin
    XCoordEdit.Text := '0';
    YCoordEdit.Text := '0';
    ZCoordEdit.Text := '0';
    SetXHairPosition(0,0,0 );
  end;
  {$IFDEF GRAPH}
  ViewGPUg.Invalidate; //20200606 for Metal
  {$ENDIF}
  exit(true);
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

function TGLForm1.ClusterOpen(fnm: string): boolean;
label
     123;
var
   s, s2: TStringList;
   st: string;
   i, o: integer;
   c: TClusters;
   niftiVol: TNIfTI;
begin
     result := false;
     if not fileexists(fnm) then exit;
     i := LayerList.ItemIndex;
     if (i < 0) or (i >= LayerList.Count) then exit;
     if not vols.Layer(LayerList.ItemIndex,niftiVol) then exit;
     s := TStringList.Create();
     s2 := TStringList.Create();
     s.LoadFromFile(fnm);
     if s.Count < 1 then goto 123;
     if not AnsiContainsText(s[0],'AFNI interactive cluster table') then
        goto 123;
     i := 0;
     o := 0;
     setlength(c,100);
     while i < s.Count do begin
           st := s[i];
           i := i + 1;
           if (length(st) < 1) or (st[1] = '#') then continue;
           s2.DelimitedText := st;
           if s2.Count <> 7 then exit;
           if (o >= length(c)) then
              setlength(c, 100+length(c));
           c[o].SzMM3:= strtointdef(s2[0],0);
           c[o].CogXYZ.x := strtofloatdef(s2[1],0);
           c[o].CogXYZ.y := strtofloatdef(s2[2],0);
           c[o].CogXYZ.z := strtofloatdef(s2[3],0);
           c[o].PeakXYZ.x := strtofloatdef(s2[4],0);
           c[o].PeakXYZ.y := strtofloatdef(s2[5],0);
           c[o].PeakXYZ.z := strtofloatdef(s2[6],0);
           c[o].Structure:= inttostr(o+1);
           c[o].PeakStructure:= inttostr(o+1);
           c[o].Peak := 1;
           o := o + 1;
     end;
     setlength(c,o);
     niftiVol.SetClusters(c, fnm);
     UpdateLayerBox(true);
    123:
     s.Free;
     s2.Free;
     c := nil;
end;

procedure TGLForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
 ss: TShiftState;
 fnm, ext, fnmREC, fnmRec2: string;
 i : integer;
begin
  if (gPrefs.InitScript <> '') then exit; //MacOS gets paramstr as FormDropFiles, but deletes those that begin with '-', e.g. '-std'
  if length(FileNames) < 1 then exit;
  if (dcm2niiForm.visible) and ((dcm2niiForm.Active) or (dcm2niiForm.Focused)) then begin
     dcm2niiForm.FormDropFiles(Sender, FileNames);
     exit;
  end;
  ss := getKeyshiftstate;
  fnm := GetFullPath(Filenames[0]);
  if (not fileexists(fnm)) and (not DirectoryExists(fnm)) then exit;
  ext := upcase(ExtractFileExt(fnm));
  if (ext = '.PAR') then begin
     //need to distinguish Philips PAR/REC from FSL mcflirt PAR file
     fnmREC := changefileext(fnm,'.REC');
     fnmRec2 := changefileext(fnm,'.rec');
     if (not fileexists(fnmREC)) and (not fileexists(fnmREC2)) then begin
        GraphOpen(fnm, (ssCtrl in ss));
        exit;
     end;
  end;
  if (ext = '.JSON') then begin
     fnmREC := changefileext(fnm,'.nii');
     if (not fileexists(fnmREC)) then
        fnmREC := fnmREC + '.gz';
     if fileexists(fnmREC) then
        fnm := fnmREC;
  end;
  if (ext = '.PY') then begin
     OpenScript(fnm);
     exit;
  end;
  if (ext = '.1D') and (ClusterOpen(fnm)) then
     exit;
  if (ext = '.1D') or (ext = '.TXT') or (ext = '.CSV') then begin
     GraphOpen(fnm, ssCtrl in ss);
     exit;
  end;
  (*if (isTIFF(fnm)) then begin
     fnm := SaveTIFFAsNifti(fnm);
     if fnm <> '' then
        AddBackground(fnm)
     else
         showmessage('Unable to convert image(s): make sure they are TIFF format');
     exit;
  end;*)

  if (isDICOM(fnm)) or (ext = '.DCM') then begin //part-10 compliant DICOM images should have "DICM" signature, but this is missing for some DICOM meta data
     //if (not isNifti(Filenames[0])) then begin
     //printf('>drop:'+fnm);
     fnm := dcm2Nifti(dcm2niiForm.getCurrentDcm2niix, fnm);
     //printf('>got:'+fnm);
     if fnm = '' then exit;
     AddBackground(fnm, false, true);
     if fnm <> Filenames[0] then
        deletefile(fnm);
     exit;
  end;
  if (DirectoryExists(fnm)) then exit;
  //if (ssModifier in ss) then begin
  if (ssMeta in ss) or (ssCtrl in ss) then
     AddLayer(fnm)
  else
      AddBackground(fnm,true, true);
  if length(FileNames) > 1 then begin
     for i := 1 to length(FileNames)-1 do begin
       fnm := GetFullPath(Filenames[i]);
       if (not fileexists(fnm)) then continue;
       ext := upcase(ExtractFileExt(fnm));
       if (ext <> '.NII') and (ext <> '.HEAD') and (ext <> '.GZ') and (ext <> '.HDR') then
          continue;
       AddLayer(fnm)

     end;
  end;
end;

procedure TGLForm1.OpenMenuClick(Sender: TObject);
begin
   //OpenDialog1.Filter := 'Text file|*.txt;*.1D;*.PAR|Comma separated values|*.csv|Any file|*.*';
   if not OpenDialog1.execute then
      OpenDialog1.filename := '';
   AddBackground(OpenDialog1.filename);
end;

procedure TGLForm1.AddOverlayClusterMenuClick(Sender: TObject);
(*var
   thresh, mm: double;
begin
   GetThresh(thresh, mm);
   if thresh = kNaN then exit;
   if not OpenDialog1.execute then
      exit;
   AddLayer(OpenDialog1.filename);*)
begin
     //Use option pull down instead!
end;

procedure TGLForm1.AfniDirMenuClick(Sender: TObject);
begin
  {$IFDEF UNIX}
  if not DirectoryExists(gPrefs.AfniDir) then
     gPrefs.AfniDir := expandfilename('~/')+'abin';
  {$ENDIF}
  if not DirectoryExists(gPrefs.AfniDir) then
     gPrefs.AfniDir := '';
   if not Dialogs.SelectDirectory('Select AFNI Folder (often named "abin")', gPrefs.AfniDir, gPrefs.AfniDir) then exit;
  CreateStandardMenus(OpenAFNIMenu);
end;



procedure TGLForm1.ClusterByMenuClick(Sender: TObject);
var
   v: TNIfTI;
begin
   if not vols.Layer(LayerList.ItemIndex,v) then exit;
   v.SortClustersBySize := (Sender as TMenuItem).tag = 0;
   v.SortClusters;
   UpdateLayerBox(false);
end;

procedure TGLForm1.ClusterViewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  kSelectedColumnPrefix = '• ';
  kPeakXYZCol = 3;
  kCogXYZCol = 5;
var
 w, row, col, selCol, unSelCol: integer;
 v: TNIfTI;
begin
 //if not vols.Layer(LayerList.ItemIndex,v) then exit;
 if not vols.Layer(ClusterView.tag,v) then exit;
 if ClusterView.Selected = nil then
    exit;
 row := ClusterView.Selected.Index;
 if (row >=  ClusterView.Items.Count) then exit;
 row := strtointdef(ClusterView.Items[row].caption, 0); //rows can be sorted by volume, peak ht, etc. find original index
 //determine which column user clicked, required to select whether XYZ refers to peak or CoG
 w := 0;
 col := -1;
 while (w < X) and ((col+1) < ClusterView.Columns.Count) do begin
     col  := col + 1;
     if not ClusterView.Column[col].visible then continue;
     w := w + ClusterView.Column[col].Width;
 end;
 if (col < 0) or (col >= ClusterView.ColumnCount) then exit;
 if (row >= length(v.clusters)) or (row< 0) then exit;
 //LayerBox.caption := format('%d %d %d',[col, row, ClusterView.Column[col].Tag]);
 if ClusterView.Column[col].Tag > 1 then begin
    SetXHairPosition(v.clusters[row].PeakXYZ.x, v.clusters[row].PeakXYZ.Y, v.clusters[row].PeakXYZ.Z, true );
    selCol := kPeakXYZCol;
    unSelCol := kCogXYZCol;
 end else begin
     SetXHairPosition(v.clusters[row].CogXYZ.x, v.clusters[row].CogXYZ.Y, v.clusters[row].CogXYZ.Z, true );
     selCol := kCogXYZCol;
     unSelCol := kPeakXYZCol;
 end;
 //LayerBox.Caption := inttostr(selCol)+':'+inttostr(unSelCol);
 if AnsiStartsText(kSelectedColumnPrefix, ClusterView.Column[unSelCol].Caption) then
     ClusterView.Column[unSelCol].Caption := Copy(ClusterView.Column[unSelCol].Caption, Length(kSelectedColumnPrefix) + 1, MaxInt);
 if not AnsiStartsText(kSelectedColumnPrefix, ClusterView.Column[selCol].Caption) then
     ClusterView.Column[selCol].Caption := kSelectedColumnPrefix+ClusterView.Column[selCol].Caption;
end;


procedure TGLForm1.ClusterViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
 i: integer;
 v: TNIfTI;
begin
 //if not vols.Layer(LayerList.ItemIndex,v) then exit;
 if not vols.Layer(ClusterView.tag,v) then exit;
 i := StrToIntDef(Item.Caption,0);
 if (i >= length(v.clusters)) or (i < 0) then exit;
 SetXHairPosition(v.clusters[i].CogXYZ.x, v.clusters[i].CogXYZ.Y, v.clusters[i].CogXYZ.Z, true );
end;

procedure TGLForm1.AddOverlayMenuClick(Sender: TObject);
var
 opts: TOpenOptions;
 i: integer;
begin
  opts := OpenDialog1.Options ;
  OpenDialog1.Options := [ofAllowMultiSelect, ofEnableSizing, ofViewDetail];
  if (not OpenDialog1.execute) or (OpenDialog1.Files.count < 1) then begin
     OpenDialog1.Options := opts;
     exit;
  end;
  for i := 0 to (OpenDialog1.Files.count -1) do
   AddLayer(OpenDialog1.Files[i]);
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
 {$IFDEF MATCAP}
 MatCapDrop.Visible := (Vol1.matcapLoc >= 0);
 LightElevTrack.Visible := (Vol1.matcapLoc < 0);
 LightAziTrack.Visible := (Vol1.matcapLoc < 0);
 {$ENDIF MATCAP}
 //if nUniform
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
          if (GLForm1.ShaderBox.Controls[i] as TTrackBar).visible then
             (GLForm1.ShaderBox.Controls[i] as TTrackBar).position := Val2Percent(Vol1.ShaderSliders.Uniform[t].min, Vol1.ShaderSliders.Uniform[t].DefaultV, Vol1.ShaderSliders.Uniform[t].max);
       end;
   end;
end;

procedure TGLForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  //LayerBox.caption := 'x' +inttostr(random(888));

end;

procedure TGLForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //if (VK_UP = Key) then LayerBox.caption := 'F'+inttostr(random(888));
  //if (VK_UP <> Key) then LayerBox.caption := 'F-'+inttostr(random(888));     //LayerBox.caption := inttostr(random(888));
end;

procedure TGLForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
//
end;


procedure TGLForm1.GraphOpenMenuClick(Sender: TObject);
{$IFNDEF GRAPH}
begin
     //
end;
{$ELSE}
 var
   openDlg: TOpenDialog;
 begin
   openDlg := TOpenDialog.Create(application);
   openDlg.Title := 'Select text file with graph data';
   openDlg.InitialDir := extractfiledir(gPrefs.PrevBackgroundImage);
   if not Fileexists(openDlg.InitialDir) then
      openDlg.InitialDir := GetCurrentDir;
   openDlg.Filter := 'Text file|*.txt;*.1D;*.PAR|Comma separated values|*.csv|Any file|*.*';
   openDlg.DefaultExt := 'txt';
   openDlg.FilterIndex := 1;
   if not openDlg.Execute then begin
      openDlg.Free;
      exit;
   end;
   GraphOpen(openDlg.Filename, (Sender as TMenuItem).tag = 1);
   openDlg.Free;
 end;
{$ENDIF}

procedure TGLForm1.GraphDrawingMenuClick(Sender: TObject);
 var
    niftiVol: TNIfTI;
    graphLine: TFloat32s;
begin
  if not vols.Layer(0,niftiVol) then exit;
  if (not vols.Drawing.IsOpen)  or (vols.Drawing.voiIsEmpty) then begin
    showmessage('No drawing is open. Nothing to graph.');
    exit;
  end;
  {$IFDEF GRAPH}
  if niftiVol.volumesLoaded < 2 then exit;
  graphLine := niftiVol.VoxIntensityArray(vols.Drawing.VolRawBytes);
  if (length(graphLine) <> niftiVol.volumesLoaded) then begin
     setlength(graphLine, 0);
     exit;
  end;
  //gGraph.ClearLines;
  gGraph.HorizontalSelection := niftiVol.VolumeDisplayed;
  gGraph.AddLine(graphLine,'Drawing', true, true);
  //gGraph.CloneLine();
  ViewGPUg.Invalidate;
  setlength(graphLine, 0);
  if BottomPanel.Height < 50 then
     GraphShowHideMenu.Click();
  {$ENDIF}
end;

procedure TGLForm1.LayerExport8BitMenuClick(Sender: TObject);
begin
  GLForm1.ClipIntensity(0, 1.0);//0.0..1.0: full display range
end;

procedure TGLForm1.LayerFindPeakMenuClick(Sender: TObject);
var
  i, nearestIdx: integer;
  v: TNIfTI;
  sliceMM, vec: TVec3;
  vox: TVec3i;
  nearestMM, mm: single;
  isActiveClusterized : boolean = true;
  itm: TListItem;
begin
 i := LayerList.ItemIndex;
 if (i < 0) or (i >= LayerList.Count) then exit;
 if not vols.Layer(i,v) then exit;
 if length(v.clusters) < 1 then begin
    isActiveClusterized := false;
    i := 0;
    while (i <  LayerList.Count) and (length(v.clusters) < 1) do begin
       vols.Layer(i,v);
       i := i + 1;
    end;
    if length(v.clusters) < 1 then begin
       showmessage('No clusters open: use "Generate Clusters" or open a template image.');
       exit;
    end;
 end;
 sliceMM := Vol1.Slice2Dmm(v, vox);
 nearestIdx := 0;
 vec :=  sliceMM-v.clusters[0].CogXYZ;
 nearestMM := vec.Length;
 for i := 0 to (length(v.clusters) -1) do begin
     vec := (sliceMM-v.clusters[i].CogXYZ);
     mm := vec.Length;
     if (mm < nearestMM) then begin
        nearestIdx := i;
        nearestMM := mm;
     end;

 end;
 if isActiveClusterized then begin
    //ClusterView.FindCaption(0,inttostr(nearestIdx), false,true,true);
    itm := ClusterView.FindCaption(0, inttostr(nearestIdx), false, true, false);
    if itm <> nil then begin
       itm.Focused := true;
       itm.Selected:= true;
    end;
       //LayerBox.caption := 'x';
 end;
 SetXHairPosition(v.clusters[nearestIdx].CogXYZ.x, v.clusters[nearestIdx].CogXYZ.Y, v.clusters[nearestIdx].CogXYZ.Z, true );
 //LayerBox.Caption := format('%g %g %g', [sliceMM.x, sliceMM.y, sliceMM.z]);

end;

procedure TGLForm1.LayerIsLabelMenuClick(Sender: TObject);
var
   i: integer;
   niftiVol: TNIfTI;
begin
//to do:
// 1: set color lookup table
// 2: intensity min..max range
    i := LayerList.ItemIndex;
    if (i < 0) or (i >= LayerList.Count) then exit;
    if not vols.Layer(LayerList.ItemIndex,niftiVol) then exit;
    niftiVol.SetIsLabels(LayerZeroIntensityInvisibleMenu.Checked);
    niftiVol.ForceUpdate(); //defer time consuming work
    updateTimer.enabled := true;
end;

procedure TGLForm1.LayerSmoothMenuClick(Sender: TObject);
begin
  Vols.InterpolateOverlays := GLForm1.LayerSmoothMenu.Checked;
end;

procedure TGLForm1.DrawDilateMenuClick(Sender: TObject);
var
 niftiVol: TNIfTI;
 clr: integer;
 p: single;
begin
  if not vols.Drawing.IsOpen then begin
     showmessage('First open the drawing you wish to dilate or erode');
     exit;
  end;
  p := GetFloat('Set voxels to dilate (positive) or erode (negative) ', -100,5,100);
  if p = 0 then exit;
  clr := Vols.Drawing.ActivePenColor;
  if  (clr <= 0) then clr := 1;
  if (gPrefs.DisplayOrient = kRenderOrient) or (gPrefs.DisplayOrient = kMosaicOrient) then exit;
  if not vols.Layer(0,niftiVol) then exit;
  Vols.Drawing.voiDilate(p);
  //LayerBox.caption := format('%d %d', [threshold, drawMode]);
  ViewGPU1.Invalidate;
end;

procedure TGLForm1.MenuItem3Click(Sender: TObject);
begin

end;

procedure TGLForm1.NimlMenuClick(Sender: TObject);
begin
  showmessage('Hello world');
end;

procedure TGLForm1.GraphAddMenuClick(Sender: TObject);
begin
 {$IFDEF GRAPH}
 gGraph.CloneLine();
 ViewGPUg.Invalidate;
 if BottomPanel.Height < 50 then
    GraphShowHideMenu.Click();
 {$ENDIF}
end;

procedure TGLForm1.GraphClearMenuClick(Sender: TObject);
const
  k = 3;
var
   newVals: TFloat32s;
   i: integer;
begin
 {$IFDEF GRAPH}
  gGraph.ClearLines();
  setlength(newVals, k);
  for i := 0 to k-1 do
      newVals[i] := 0;//(3 * sin(i/(k/12))) + 6;
  gGraph.AddLine(newVals, 'No Graph Loaded: Use Graph Open Menu or open 4D Image as background');
  newVals := nil;
  ViewGPUg.Invalidate;
 {$ENDIF}
end;

procedure TGLForm1.GraphMarkerMenuClick(Sender: TObject);
begin
 {$IFDEF GRAPH}
 gGraph.isMarker := GraphMarkerMenu.Checked;
 gGraph.isRedraw:= true;
 ViewGPUg.Invalidate;
 {$ENDIF}
end;

function TGLForm1.GraphOpen(fnm: string; isKeepOldGraph: boolean = false; isShowMessageOnError: boolean = false): boolean;
var
   fnm2: string;
begin
 {$IFDEF GRAPH}
 fnm2 := fnm;
 result := fileexists(fnm2);
 if not result then begin
    fnm2 := GetFullPath(fnm2, false);
    result := fileexists(fnm2);
 end;
 if (not result) and (isShowMessageOnError) then
     showmessage('Unable to find file "'+fnm2+'"');
 if not result then exit;
 result := gGraph.LoadText(fnm2, isKeepOldGraph);
 if (not result) and (isShowMessageOnError) then
     showmessage('Unable to load graph from text file "'+fnm2+'"');
  ViewGPUg.Invalidate;
  GLForm1.UpdateLayerBox(false, true);
  //if BottomPanel.Height < 20 then
  //  BottomPanel.Height := round(GLForm1.Height * 0.25);
 {$ENDIF}
end;

procedure TGLForm1.GraphSaveMenuClick(Sender: TObject);
{$IFNDEF GRAPH}
begin
     //
end;
{$ELSE}
 var
   saveDlg: TSaveDialog;
   strs: TStringList;
   resp : integer;
 begin
   resp := MessageDlg('Export X axis data as the first column?', mtConfirmation,[mbCancel, mbYes, mbNo], 0);
   if resp = mrCancel then exit;
   strs := gGraph.AsText(resp = mrYes);
   if (strs.Count < 1) then begin
      showmessage('Unable to convert graph to text');
      strs.Free;
      exit;
   end;
   saveDlg := TSaveDialog.Create(application);
   saveDlg.Title := 'Export graph data as text';
   saveDlg.InitialDir := extractfiledir(gPrefs.PrevBackgroundImage);
   if not Fileexists(saveDlg.InitialDir) then
      saveDlg.InitialDir := GetCurrentDir;
   saveDlg.Filter := 'Text file|*.txt';
   saveDlg.DefaultExt := 'txt';
   saveDlg.FilterIndex := 1;
   if not saveDlg.Execute then begin
      saveDlg.Free;
      strs.Free;
      exit;
   end;
   strs.SaveToFile(saveDlg.Filename);
   strs.Free;
   saveDlg.Free;
 end;
{$ENDIF}

procedure TGLForm1.GraphScaleClick(Sender: TObject);
begin
  {$IFDEF GRAPH}
  gGraph.Style := (sender as TMenuItem).tag;
  gGraph.isRedraw := true;
  ViewGPUg.Invalidate;
  {$ENDIF}
end;

procedure TGLForm1.GraphShowHideMenuClick(Sender: TObject);
begin
 {$IFDEF GRAPH}
 if BottomPanel.Height > 50 then
    BottomPanel.Height := TBSplitter.MinSize
 else
   BottomPanel.Height := GLForm1.Height div 2;
 {$ENDIF}
end;


procedure TGLForm1.LayerInvertColorMapMenuClick(Sender: TObject);
 var
     i: integer;
     niftiVol: TNIfTI;
 begin
   i := LayerList.ItemIndex;
   if (i < 0) or (i >= LayerList.Count) then exit;
   if not vols.Layer(LayerList.ItemIndex,niftiVol) then exit;
   niftiVol.CX.InvertColorMap:= LayerInvertColorMapMenu.Checked;
   niftiVol.CX.NeedsUpdate := true;
   niftiVol.CX.GenerateLUT();
   niftiVol.ForceUpdate();
   updateTimer.Enabled := true;
end;

procedure TGLForm1.LayerWidgetChangeTimerTimer(Sender: TObject);
begin
  LayerWidgetChangeTimer.enabled := false;
  LayerWidgetChange(Sender);
end;

procedure TGLForm1.LayerZeroIntensityInvisibleMenuClick(Sender: TObject);
var
     i: integer;
     niftiVol: TNIfTI;
 begin
   i := LayerList.ItemIndex;
   if (i < 0) or (i >= LayerList.Count) then exit;
   if not vols.Layer(LayerList.ItemIndex,niftiVol) then exit;
   niftiVol.ZeroIntensityInvisible:= LayerZeroIntensityInvisibleMenu.Checked;
   niftiVol.ForceUpdate(); //defer time consuming work
   updateTimer.enabled := true;
end;

procedure GetThresh(var thresh: double; var mm: double; out NeighborMethod: integer; out isDarkAndBright: boolean; selectDarkAndBright: boolean = true);  //TResliceForm
var
    PrefForm: TForm;
    CancelBtn,OkBtn: TButton;
    threshLabel, ClusterLabel: TLabel;
    threshEdit, ClusterEdit: TEdit;
    NeighborCombo: TComboBox;
    BimodalCheck: TCheckBox;
begin
  PrefForm:=TForm.Create(nil);
  //PrefForm.SetBounds(100, 100, 512, 212);
  PrefForm.AutoSize := True;
  PrefForm.BorderWidth := 8;
  PrefForm.Caption:='Overlay Settings';
  PrefForm.Position := poScreenCenter;
  PrefForm.BorderStyle := bsDialog;
  //label
  threshLabel:=TLabel.create(PrefForm);
  threshLabel.Caption:= 'Threshold intensity';
  threshLabel.AutoSize := true;
  threshLabel.AnchorSide[akTop].Side := asrTop;
  threshLabel.AnchorSide[akTop].Control := PrefForm;
  threshLabel.BorderSpacing.Top := 6;
  threshLabel.AnchorSide[akLeft].Side := asrLeft;
  threshLabel.AnchorSide[akLeft].Control := PrefForm;
  threshLabel.BorderSpacing.Left := 6;
  threshLabel.Parent:=PrefForm;
  //edit
  threshEdit:=TEdit.create(PrefForm);
  threshEdit.Caption := FloatToStrF(thresh, ffGeneral, 8, 4);
  threshEdit.Constraints.MinWidth:= 300;
  threshEdit.AutoSize := true;
  threshEdit.AnchorSide[akTop].Side := asrBottom;
  threshEdit.AnchorSide[akTop].Control := threshLabel;
  threshEdit.BorderSpacing.Top := 6;
  threshEdit.AnchorSide[akLeft].Side := asrLeft;
  threshEdit.AnchorSide[akLeft].Control := PrefForm;
  threshEdit.BorderSpacing.Left := 6;
  threshEdit.Parent:=PrefForm;

  //label
  clusterLabel:=TLabel.create(PrefForm);
  clusterLabel.Caption:= 'Minimum cluster size (mm^3)';
  clusterLabel.AutoSize := true;
  clusterLabel.AnchorSide[akTop].Side := asrBottom;
  clusterLabel.AnchorSide[akTop].Control := threshEdit;
  clusterLabel.BorderSpacing.Top := 6;
  clusterLabel.AnchorSide[akLeft].Side := asrLeft;
  clusterLabel.AnchorSide[akLeft].Control := PrefForm;
  clusterLabel.BorderSpacing.Left := 6;
  clusterLabel.Parent:=PrefForm;
  //edit
  clusterEdit:=TEdit.create(PrefForm);
  clusterEdit.Caption := FloatToStrF(mm, ffGeneral, 8, 4);
  clusterEdit.Constraints.MinWidth:= 300;
  clusterEdit.AutoSize := true;
  clusterEdit.AnchorSide[akTop].Side := asrBottom;
  clusterEdit.AnchorSide[akTop].Control := clusterLabel;
  clusterEdit.BorderSpacing.Top := 6;
  clusterEdit.AnchorSide[akLeft].Side := asrLeft;
  clusterEdit.AnchorSide[akLeft].Control := PrefForm;
  clusterEdit.BorderSpacing.Left := 6;
  clusterEdit.Parent:=PrefForm;

  NeighborCombo:=TComboBox.create(PrefForm);
  NeighborCombo.Parent:=PrefForm;
  NeighborCombo.AutoSize:= true;
  NeighborCombo.Width := 320;
  NeighborCombo.Items.Add('Neighbors: Faces (6)');
  NeighborCombo.Items.Add('Neighbors: Faces, Edges (18)');
  NeighborCombo.Items.Add('Neighbors: Faces, Edges, Corners (26)');
  NeighborCombo.ItemIndex:=  0;
  NeighborCombo.Style := csDropDownList;
  NeighborCombo.AnchorSide[akTop].Side := asrBottom;
  NeighborCombo.AnchorSide[akTop].Control := clusterEdit;
  NeighborCombo.BorderSpacing.Top := 6;
  NeighborCombo.AnchorSide[akLeft].Side := asrLeft;
  NeighborCombo.AnchorSide[akLeft].Control := PrefForm;
  NeighborCombo.BorderSpacing.Left := 6;
  NeighborCombo.Anchors := [akTop, akLeft];
  NeighborCombo.Parent:=PrefForm;


  BimodalCheck:=TCheckBox.create(PrefForm);
  BimodalCheck.Checked := false;
  BimodalCheck.Caption:='Bimodal (identify both dark and bright clusters)';
  BimodalCheck.AnchorSide[akTop].Side := asrBottom;
  BimodalCheck.AnchorSide[akTop].Control := NeighborCombo;
  BimodalCheck.BorderSpacing.Top := 6;
  BimodalCheck.AnchorSide[akLeft].Side := asrLeft;
  BimodalCheck.AnchorSide[akLeft].Control := PrefForm;
  BimodalCheck.BorderSpacing.Left := 6;
  BimodalCheck.Anchors := [akTop, akLeft];
  BimodalCheck.visible := selectDarkAndBright;
  BimodalCheck.Parent:=PrefForm;
  //Cancel Btn
  CancelBtn:=TButton.create(PrefForm);
  CancelBtn.Caption:='Cancel';
  CancelBtn.AutoSize := true;
  CancelBtn.AnchorSide[akTop].Side := asrBottom;
  CancelBtn.AnchorSide[akTop].Control := BimodalCheck;
  CancelBtn.BorderSpacing.Top := 8;
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
  OkBtn.AnchorSide[akTop].Control := BimodalCheck;
  OkBtn.BorderSpacing.Top := 8;
  OkBtn.AnchorSide[akLeft].Side := asrRight;
  OkBtn.AnchorSide[akLeft].Control := CancelBtn;
  OkBtn.BorderSpacing.Left := 6;
  OkBtn.Parent:=PrefForm;
  OkBtn.ModalResult:= mrOK;
  //OK button
  {$IFDEF DARKMODE}
  if gPrefs.DarkMode then GLForm1.SetFormDarkMode(PrefForm);
  {$ENDIF}
  PrefForm.ShowModal;
  thresh := kNaN;
  if (PrefForm.ModalResult = mrOK) then begin
    thresh := StrToFloatDef(threshEdit.Caption, 4);
    mm :=  StrToFloatDef(clusterEdit.Caption, 0);
    isDarkAndBright := BimodalCheck.Checked;
  end;
  NeighborMethod := NeighborCombo.ItemIndex+1;
  FreeAndNil(PrefForm);
end; //GetThresh()

procedure TGLForm1.RemoveSmallClusterMenuClick(Sender: TObject);
var
     i, NeighborMethod: integer;
     niftiVol: TNIfTI;
     thresh, mm: double;
     dummy: boolean;
begin
 i := LayerList.ItemIndex;
 if (i < 0) or (i >= LayerList.Count) then exit;
 if not vols.Layer(LayerList.ItemIndex,niftiVol) then exit;
 thresh := (0.5*(niftiVol.DisplayMax-niftiVol.DisplayMin))+niftiVol.DisplayMin;
 mm := 32;
 GetThresh(thresh, mm, NeighborMethod, dummy, false);
 if thresh = kNaN then exit;
 niftiVol.RemoveSmallClusters(thresh,mm, NeighborMethod);
 niftiVol.ForceUpdate(); //defer time consuming work
 updateTimer.enabled := true;
end;

procedure TGLForm1.MatCapDropChange(Sender: TObject);
begin
 {$IFDEF MATCAP}
 //GLBox.MakeCurrent;
 //SetMatCap(MatCapDir+pathdelim+MatCapDrop.Items[MatCapDrop.ItemIndex]+'.jpg');
 //GLBox.ReleaseContext;
 //GLBoxRequestUpdate(Sender);
 if MatCapDrop.Items.Count < 1 then exit;
 if MatCapDrop.ItemIndex < 0 then
    MatCapDrop.ItemIndex := 0;
 Vol1.SetMatCap(MatCapDrop.Items[MatCapDrop.ItemIndex]);
  ViewGPU1.Invalidate;
 {$ENDIF}
end;

//{$DEFINE RESIZE_FROM_DISK}
{$IFDEF RESIZE_FROM_DISK}
(*procedure TGLForm1.ResizeMenuClick(Sender: TObject);
const
  kInFilter = 'NIfTI|*.nii;*.nii.gz|Supported image format|*.*';
  kFilter = 'NIfTI|*.nii|Compressed NIfTI|*.nii.gz';
var
   saveDlg : TSaveDialog;
   openDlg : TOpenDialog;
   nii: TNIfTI;
   fnm: string;
   isOK, isAllVolumes: boolean;
   backColor: TRGBA;
   filter, datatype: integer;
   scale: TVec3;
begin
 openDlg := TOpenDialog.Create(self);
 openDlg.InitialDir := extractfiledir(gPrefs.PrevBackgroundImage);
 openDlg.FileName:= gPrefs.PrevBackgroundImage;
 {$IFDEF Darwin}
 showmessage('Select the image you wish to resize');
  if PosEx('.app/Contents/Resources', gPrefs.PrevBackgroundImage) > 0 then begin
     openDlg.InitialDir := GetCurrentDir;
     openDlg.FileName := '';
  end;
  {$ENDIF}
  OpenDlg.Filter := kInFilter;
  OpenDlg.FilterIndex := 1;
  OpenDlg.Title := 'Select image to resize';
  OpenDlg.Options := [ofFileMustExist];
  if not OpenDlg.Execute then begin
    openDlg.Free;
    exit;
  end;
  fnm := openDlg.FileName;
  openDlg.Free;
  //fnm := '/Users/chris/spmMotor.nii.gz';
     backColor := setRGBA(0,0,0,0);
     //nii := TNIfTI.Create(fnm, backColor, false, 32768, isOK);
     nii := TNIfTI.Create(fnm, backColor, false, -1, isOK);
     //nii.Create(fnm, rgba, false, 4096, ok);
     if not isOK then
        exit;
     //isLabel := (hdr.intent_code = kNIFTI_INTENT_LABEL) or (hdr.regular = char(98));
     scale := ResizeForm.GetScale(nii.Header, nii.IsLabels, fnm, datatype, Filter, isAllVolumes);
     if scale.x = 0 then begin
        nii.Free;
        exit;
     end;
     if (scale.x = 1) and (scale.y = 1) and (scale.z = 1) and (datatype = nii.Header.datatype) then begin
      nii.Free;
      showmessage('Nothing to do: no change to volume.' );
      exit;
     end;
     saveDlg := TSaveDialog.Create(self);
     saveDlg.Filter := kFilter;
     saveDlg.DefaultExt := '.nii';
     saveDlg.InitialDir:= extractfiledir(fnm);
     saveDlg.Filename := 'r'+ChangeFileExtX(extractfilename(fnm),'');
     if not saveDlg.Execute then begin
        nii.Free;
        saveDlg.Free;
        exit;
     end;
     //outnm := '/Users/chris/tiff/xbango.nii';
     nii.SaveRescaled(saveDlg.filename, scale.x, scale.y, scale.z, datatype, filter, isAllVolumes);
     saveDlg.Free;
     nii.Free;
end; *)
procedure TGLForm1.ResizeMenuClick(Sender: TObject);
const
  kInFilter = 'NIfTI|*.nii;*.nii.gz|Supported image format|*.*';
  //kFilter = 'NIfTI|*.nii|Compressed NIfTI|*.nii.gz';
var
   openDlg : TOpenDialog;
   nii: TNIfTI;
   fnm: string;
   isOK, isAllVolumes: boolean;
   backColor: TRGBA;
   filter, datatype: integer;
   scale: TVec3;
begin
 openDlg := TOpenDialog.Create(self);
 openDlg.InitialDir := extractfiledir(gPrefs.PrevBackgroundImage);
 openDlg.FileName:= gPrefs.PrevBackgroundImage;
 {$IFDEF Darwin}
 showmessage('Select the image you wish to resize');
  if PosEx('.app/Contents/Resources', gPrefs.PrevBackgroundImage) > 0 then begin
     openDlg.InitialDir := GetCurrentDir;
     openDlg.FileName := '';
  end;
  {$ENDIF}
  OpenDlg.Filter := kInFilter;
  OpenDlg.FilterIndex := 1;
  OpenDlg.Title := 'Select image to resize';
  OpenDlg.Options := [ofFileMustExist];
  if not OpenDlg.Execute then begin
    openDlg.Free;
    exit;
  end;
  fnm := openDlg.FileName;
  openDlg.Free;
  //fnm := '/Users/chris/spmMotor.nii.gz';
     backColor := setRGBA(0,0,0,0);
     //nii := TNIfTI.Create(fnm, backColor, false, 32768, isOK);
     nii := TNIfTI.Create(fnm, backColor, false, -1, isOK);
     //nii.Create(fnm, rgba, false, 4096, ok);
     if not isOK then
        exit;
     //isLabel := (hdr.intent_code = kNIFTI_INTENT_LABEL) or (hdr.regular = char(98));
     scale := ResizeForm.GetScale(nii.Header, nii.IsLabels, fnm, datatype, Filter, isAllVolumes);
     if scale.x = 0 then begin
        nii.Free;
        exit;
     end;
     if (scale.x = 1) and (scale.y = 1) and (scale.z = 1) and (datatype = nii.Header.datatype) then begin
      nii.Free;
      showmessage('Nothing to do: no change to volume.' );
      exit;
     end;
     fnm := NiftiSaveDialogFilename();
     if fnm = '' then exit;
     //SaveFormatBasedOnExt
     //outnm := '/Users/chris/tiff/xbango.nii';
     nii.SaveRescaled(fnm, scale.x, scale.y, scale.z, datatype, filter, isAllVolumes);
     //saveDlg.Free;
     nii.Free;
end;
{$ELSE} //not RESIZE_FROM_DISK
function HeaderRotate(hdr: TNIFTIhdr; perm: TVec3i): TNIFTIhdr;
begin
	 result := hdr;
     result.dim[1] := hdr.dim[abs(perm.x)];
     result.dim[2] := hdr.dim[abs(perm.y)];
     result.dim[3] := hdr.dim[abs(perm.z)];
     result.pixdim[1] := hdr.pixdim[abs(perm.x)];
     result.pixdim[2] := hdr.pixdim[abs(perm.y)];
     result.pixdim[3] := hdr.pixdim[abs(perm.z)];
end;

procedure TGLForm1.ResizeMenuClick(Sender: TObject);
var
   //dlg : TSaveDialog;
   isOK: boolean;
   nii, niiBig: TNIfTI;
   hdr: TNIFTIhdr;
   filter, datatype: integer;
   scale: TVec3;
   fnm: string;
   isAllVolumes: boolean;
   backColor: TRGBA;
begin

  if not vols.Layer(0, nii) then exit;

  //TODO:2020 if big is reoriented
  //TODO:2020 clip when big
  //TODO:2020 reorient when big

 (*if (nii.Header.datatype= kDT_RGB) then begin
    showmessage('This function does not yet support RGB images');
    exit;
 end;*)
 //showmessage(format('%d %d', [nii.VolumesTotal], nii.VolumesLoaded])); exit;
 //nii.SaveRescaled('fx.nii', 0.5, 0.5, 0.5, kDT_FLOAT, 6); exit;
 //nii.SaveRescaled('fx.nii', 0.75, 0.75, 0.75, kDT_FLOAT, 6); exit;
 //nii.SaveRescaled('fx.nii', 0.5, 0.5, 0.5, kDT_SIGNED_SHORT, 6); exit;
 //nii.SaveRescaled('ax.nii', 2.0, 2.0, 2.0, kDT_SIGNED_SHORT, 6); exit;
 //{$DEFINE zDEBUG}
 {$IFDEF zDEBUG}
 nii.SaveRescaled('ax.nii', 0.5, 1, 0.5, kDT_SIGNED_SHORT, -1, true);
 exit;
 {$ENDIF}
 //showmessage(format('%d %d', [nii.HeaderNoRotation.Dim[1], nii.Header.Dim[1] ]));
 //exit;

 //hdr := nii.Header;
 //hdr := nii.HeaderNoRotation;
 if (not nii.IsShrunken) then
 	hdr := HeaderRotate(nii.HeaderNoRotation, nii.InputReorientPermute)
 else
 	 hdr := nii.HeaderNoRotation;
 //showmessage(format('%d %d %d', [nii.InputReorientPermute.x, nii.InputReorientPermute.y, nii.InputReorientPermute.z ]));
 //exit;
 scale := ResizeForm.GetScale(hdr, nii.isLabels, nii.ShortName, datatype, filter, isAllVolumes);
 if scale.x <= 0 then exit;
 if (scale.x = 1) and (scale.y = 1) and (scale.z = 1) and (datatype = hdr.datatype) then begin
    showmessage('Nothing to do: no change to volume.' );
    exit;
 end;
 (*dlg := TSaveDialog.Create(self);
 dlg.Title := 'Save NIfTI volume';
 dlg.InitialDir := extractfiledir(gPrefs.PrevBackgroundImage);
 {$IFDEF Darwin}
 if PosEx('.app', dlg.InitialDir) > 0  then
       dlg.InitialDir := HomeDir(false);
 {$ENDIF}
 dlg.Filename := 'r'+ChangeFileExtX(extractfilename(gPrefs.PrevBackgroundImage),'');
 dlg.Filter := 'NIfTI|*.nii|Compressed NIfTI|*.nii.gz';
 dlg.DefaultExt := '*.nii';
 dlg.FilterIndex := 0;
 if not dlg.Execute then begin
    dlg.Free;
    exit;
 end;
 fnm :=  dlg.filename;
 dlg.Free; *)
 fnm := NiftiSaveDialogFilename();
 if fnm = '' then exit;
 if (not nii.IsShrunken) then begin
 	nii.SaveRescaled(fnm, scale.x, scale.y, scale.z, datatype, filter, isAllVolumes);
    exit;
 end;
 //we shrank the image - load full size
 backColor := setRGBA(0,0,0,0);
 niiBig := TNIfTI.Create(nii.Filename, backColor, false, -1, isOK);
 //nii.Create(fnm, rgba, false, 4096, ok);
 if not isOK then
 	exit;
 niiBig.SaveRescaled(fnm, scale.x, scale.y, scale.z, datatype, filter, isAllVolumes);
 //saveDlg.Free;
 niiBig.Free;

end;
{$ENDIF} //if RESIZE_FROM_DISK else end

procedure TGLForm1.ApplyCrop(crop: TVec6i; cropVols: TPoint);
var
   nii, niiBig: TNIfTI;
   fnm: string;
   isOK: boolean;
   backColor: TRGBA;
   cropBig: TVec6i;
   scale: single;
begin
 if crop.xLo < 0 then exit;
 if not vols.Layer(0, nii) then exit;
 fnm := NiftiSaveDialogFilename();
 if fnm = '' then exit;
 if (not nii.IsShrunken) or (not fileexists(nii.Filename)) then begin
     nii.SaveCropped(fnm, crop, cropVols);
     exit;
 end;
 backColor := setRGBA(0,0,0,0);
 niiBig := TNIfTI.Create(nii.Filename, backColor, false, 4096, isOK);
 if not isOK then begin
    showmessage('Unable to open large file '+  nii.Filename);
    exit;
 end;
 scale := niiBig.Header.dim[1]/nii.Header.dim[1];
 cropBig.xHi:= round(crop.xHi * scale);
 cropBig.xLo:= round(crop.xLo * scale);
 scale := niiBig.Header.dim[2]/nii.Header.dim[2];
 cropBig.yHi:= round(crop.yHi * scale);
 cropBig.yLo:= round(crop.yLo * scale);
 scale := niiBig.Header.dim[3]/nii.Header.dim[3];
 cropBig.zHi:= round(crop.zHi * scale);
 cropBig.zLo:= round(crop.zLo * scale);
 niiBig.SaveCropped(fnm, cropBig, cropVols);
 niiBig.Free;
end;

procedure TGLForm1.CropMenuClick(Sender: TObject);
var
   nii: TNIfTI;
   //crop: TVec6i;
   dim4: TVec4i;
   //cropVols: TPoint;
   //dlg : TSaveDialog;
begin
 if not vols.Layer(0, nii) then exit;
 if gPrefs.DisplayOrient <> kAxCorSagOrient3 then
    MPRMenu.click;
 dim4.x := nii.Dim.x;
 dim4.y := nii.Dim.y;
 dim4.z := nii.Dim.z;
 dim4.t := nii.volumesLoaded;//nii.header.dim[4];
 CropForm.GetCrop(dim4, nii.ShortName);
 (*if crop.xLo < 0 then exit;
 dlg := TSaveDialog.Create(self);
 dlg.Title := 'Save NIfTI volume';
 dlg.InitialDir := extractfiledir(gPrefs.PrevBackgroundImage);
 {$IFDEF Darwin}
 if PosEx('.app', dlg.InitialDir) > 0  then
       dlg.InitialDir := HomeDir(false);
 {$ENDIF}
 dlg.FileName:= 'r'+extractfilename(gPrefs.PrevBackgroundImage);
 dlg.Filter := 'NIfTI|*.nii|Compressed NIfTI|*.nii.gz';
 dlg.DefaultExt := '*.nii';
 dlg.FilterIndex := 0;
 if not dlg.Execute then exit;
 nii.SaveCropped(dlg.filename, crop, cropVols);
 dlg.Free; *)

end;

procedure TGLForm1.ReorientMenuClick(Sender: TObject);
//{$DEFINE REORIENTDEBUG}
label
  245;
var
  //dlg : TSaveDialog;
  fnm : string;
  nii, niiBig: TNIfTI;
  backColor: TRGBA;
  perm: TVec3i;
  isOK: boolean;
  btn : array  [1..6] of string = ('red','green','blue','purple','orange','yellow');
  btnR,btnA,btnS: integer;
begin
 //perm.x := 2;perm.y := 1; perm.z := 3;
 //{$DEFINE zDEBUG}
 {$IFDEF zDEBUG}
 perm.x := -1;perm.y := 2; perm.z := -3;
 if not vols.Layer(0, niftiVol) then exit;

 niftiVol.SaveRotated('~\x.nii', perm);
 exit;
 {$ENDIF}

 (*if not vols.Layer(0, niftiVol) then exit;

 niftiVol.SaveRescaled('r.nii', 0.5, 0.5, 0.5);
 exit;*)

 Vol1.Slices.isOrientationTriangles := true;
 ViewGPU1.Invalidate;
 if gPrefs.DisplayOrient <> kAxCorSagOrient3 then
    MPRMenu.click;
 btnR := QuestionDlg ('Reorient image','Which arrow is pointing toward participant’s RIGHT?',
      mtInformation,[ 11,btn[1], 12,btn[2], 13,btn[3], 14,btn[4], 15,btn[5], 16,btn[6] ],'');
 if btnR = mrCancel then goto 245;
 btnR := btnR - 10;
 if (btnR <= 2) then
    btnA := QuestionDlg ('Reorient image','Which arrow is pointing toward participant’s ANTERIOR?',
         mtCustom,[ 13,btn[3], 14,btn[4], 15,btn[5], 16,btn[6] ],'')
 else if (btnR <= 4) then
    btnA := QuestionDlg ('Reorient image','Which arrow is pointing toward participant’s ANTERIOR?',
         mtCustom,[ 11,btn[1], 12,btn[2], 15,btn[5], 16,btn[6] ],'')
 else
     btnA := QuestionDlg ('Reorient image','Which arrow is pointing toward participant’s ANTERIOR?',
         mtCustom,[ 11,btn[1], 12,btn[2], 13,btn[3], 14,btn[4] ],'');
 if btnA = mrCancel then goto 245;
 btnA := btnA - 10;
 if (max(btnR,btnA) <= 4) then
    btnS := QuestionDlg ('Reorient image','Which arrow is pointing toward participant’s SUPERIOR?',
         mtCustom,[15,btn[5], 16,btn[6] ],'')
 else if (min(btnR,btnA) >= 3) then
    btnS := QuestionDlg ('Reorient image','Which arrow is pointing toward participant’s SUPERIOR?',
         mtCustom,[11,btn[1], 12,btn[2] ],'')
 else
     btnS := QuestionDlg ('Reorient image','Which arrow is pointing toward participant’s SUPERIOR?',
          mtCustom,[13,btn[3], 14,btn[4] ],'');
 if btnS = mrCancel then goto 245;
 btnS := btnS - 10;
 Vol1.Slices.isOrientationTriangles := false;
 if (btnR=2) and (btnA=4) and (btnS=6) then begin
    showmessage('Image already oriented');
    goto 245;
 end;
 perm := pti( (btnR + 1) div 2, (btnA + 1) div 2,  (btnS + 1) div 2);
 if odd(btnR) then perm.x := -perm.x;
 if odd(btnA) then perm.y := -perm.y;
 if odd(btnS) then perm.z := -perm.z;
 //showmessage(format('%d %d %d', [perm.x, perm.y, perm.z]));
 if not vols.Layer(0, nii) then goto 245;
 fnm := NiftiSaveDialogFilename();
 if fnm = '' then goto 245;
 if (not nii.IsShrunken) then begin
 	nii.SaveRotated(fnm, perm);
    goto 245;
 end;
 //we shrank the image - load full size
 backColor := setRGBA(0,0,0,0);
 niiBig := TNIfTI.Create(nii.Filename, backColor, false, -1, isOK);
 //nii.Create(fnm, rgba, false, 4096, ok);
 if not isOK then
 	exit;
 niiBig.SaveRotated(fnm, perm);
 //saveDlg.Free;
 niiBig.Free;
 245:
 Vol1.Slices.isOrientationTriangles := false;
end;

procedure UpdateMatCapDrop (var LUTdrop: TComboBox);
{$IFDEF MATCAP}
var
  lSearchRec: TSearchRec;
  lF: ansistring;
  lS: TStringList;
  MatCapDir: string;
begin
  LUTdrop.Items.Clear;
  lS := TStringList.Create;
  MatCapDir := ExtractFilePath(ShaderDir)+ 'matcap';
  if FindFirst(MatCapDir+pathdelim+'*.jpg', faAnyFile, lSearchRec) = 0 then
    repeat
      lF :=ExtractFileName(lSearchRec.Name);
      if (length(lF) > 1) and (lF[1] <> '.') then  //OSX can create hidden files
        lS.Add(ChangeFileExt(ExtractFileName(lSearchRec.Name),'')) ;
    until (FindNext(lSearchRec) <> 0);
  FindClose(lSearchRec);
  if lS.Count < 1 then begin;
     showmessage('Error: unable to find any MatCaps in '+MatCapDir+pathdelim+'*.jpg' );
     //LUTdrop.Items.Add('No MatCaps found');
     Freeandnil(lS);
     exit;
  end;
  lS.sort;
  LUTdrop.Items.AddStrings(lS);
  Freeandnil(lS);
end;//UpdateMatCapDrop()
{$ELSE}
begin
  //
end;
{$ENDIF}

var
  gHideDrawing: boolean = false;

procedure TGLForm1.ViewGPUKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (gHideDrawing) then begin //we can not read shift state, as released
  	 Vols.Drawing.OpacityFraction := abs(Vols.Drawing.OpacityFraction);
  	 ViewGPU1.Invalidate;
     gHideDrawing := false;
  end;
end;

procedure TGLForm1.ViewGPUKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
 var
  sliceMove: TVec3i;
  vFrac: TVec3;
  niftiVol: TNIfTI;
 begin
  if not vols.Layer(0,niftiVol) then exit;
  if (vols.Drawing.IsOpen) and (ssAlt in Shift) then begin
  	 Vols.Drawing.OpacityFraction := -abs(Vols.Drawing.OpacityFraction);
     gHideDrawing := true;
  	 ViewGPU1.Invalidate;
  end;

  (*if not vols.Layer(0,niftiVol) then begin
    if DisplayNextMenu.Enabled then begin
       if (Key = VK_LEFT) or (Key = VK_Down) then DisplayPrevMenu.Click;
       if (Key = VK_RIGHT) or (Key = VK_Up) then DisplayNextMenu.Click;

       exit;
    end;
      exit;
  end; *)


  sliceMove:= pti(0,0,0);
   Case Key of
      VK_LEFT: sliceMove.X := -1; //LEFT
      VK_RIGHT: sliceMove.X := +1; //RIGHT
      VK_DOWN: sliceMove.Y := -1; //POSTERIOR
      VK_UP: sliceMove.Y := +1; //ANTERIOR
      VK_NEXT: sliceMove.Z := -1; //INFERIOR
      VK_PRIOR: sliceMove.Z := +1; //SUPERIOR
  end;
  vFrac := niftiVol.FracShiftSlice(vol1.Slices.SliceFrac, sliceMove); //move a desired number of slices
  vol1.SetSlice2DFrac(vFrac);
  ViewGPU1.Invalidate;
  ReportPositionXYZ(true);
 end;
(*begin
 case Key of
              VK_LEFT: SliceLBtn.Click;
              VK_RIGHT: SliceRBtn.Click;
              VK_UP: SlicePBtn.Click;
              VK_DOWN: SliceABtn.Click; xxx
              //VK_DOWN: SliceIBtn.Click;
              //VK_UP: SliceSBtn.Click;
         end;

end;*)

procedure TGLForm1.ViewGPUKeyPress(Sender: TObject; var Key: char);
begin
 //LayerBox.caption := 'z'+inttostr(random(888));
  // if (VK_UP = Key) then caption := inttostr(random(888));
  //if (VK_UP <> Key) then caption := '-'+inttostr(random(888));
end;

procedure TGLForm1.ViewGPUDblClick(Sender: TObject);
var
niftiVol: TNIfTI;
 ss: TShiftState;
 //fracXYZ: TVec3;
 i: integer;
begin
	if not vols.Layer(0,niftiVol) then exit;
	if Vol1.CE.ColorEditorDblClick(niftiVol) then exit;
        ss := getKeyshiftstate;
        if (Sender <> nil) and (ssCtrl in ss) and (gPrefs.DisplayOrient <= kMax2DOrient) then begin
           //fracXYZ := Vol1.GetSlice2DFrac(X,Y,i);
           //LayerBox.Caption := format('%f %f %f', [Vol1.Slices.ZoomCenter.x, Vol1.Slices.ZoomCenter.y, Vol1.Slices.ZoomCenter.z]);
           if (ssShift in ss) then begin
              Vol1.SetSlice2DFrac(Vec3(0.5,0.5,0.5));
              //Vol1.Slices.ZoomScale := 1.0;
              SliceZoom.Position := SliceZoom.min;
           end else
               Vol1.SetSlice2DFrac(Vol1.GetSlice2DFrac(gMouse.X,gMouse.Y,i)); //
           Vol1.Slices.ZoomCenter := 1.0 - Vol1.Slices.SliceFrac;
           //Vol1.Slices.ZoomCenter := 1.0 - fracXYZ;
           //SliceBox.Caption := format('%f %f %f', [Vol1.Slices.ZoomCenter.x, Vol1.Slices.ZoomCenter.y, Vol1.Slices.ZoomCenter.z]);
           //Vol1.Slices.ZoomCenter := Vol1.Slices.ZoomCenter - 0.5;
           ViewGPU1.invalidate;
           exit;
        end;

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
 ClearColor: TRGBA;
begin
  //to do
  GLForm1.RenderMenu.Checked := true;
  //gPrefs.LabelOrient := TextAndCubeMenu.Checked;
  //Vol1.Slices.LabelOrient := gPrefs.LabelOrient;
  Vols.AdditiveOverlayBlending := false;
  LayerAdditiveMenu.Checked := Vols.AdditiveOverlayBlending;
  LayerMaskWithBackgroundMenu.Checked := Vols.MaskWithBackground;
  //GLForm1.MosaicMenu.Checked := true;
  //gPrefs.DisplayOrient := kMosaicOrient;
  {$IFDEF CLRBAR}
  gClrbar.SizeFraction := gPrefs.ColorbarSize/1000;
  {$ENDIF}
  ClearColor := gPrefs.ClearColor;
  SetDefaultPrefs(gPrefs, false);
  if (ClearColor.R <> gPrefs.ClearColor.R) or (ClearColor.G <> gPrefs.ClearColor.G) or (ClearColor.B <> gPrefs.ClearColor.B) then
     Vol1.SetTextContrast(gPrefs.ClearColor);
  //gPrefs.colorbar := gClrbar.isVisible;
  VisibleClrbarMenu.Checked := gPrefs.ColorbarVisible;
  Smooth2DCheck.Checked := gPrefs.Smooth2D;
  Vols.InterpolateOverlays := gPrefs.LoadSmooth;
  RulerCheck.checked := gPrefs.RulerVisible;
  {$IFDEF CLRBAR}
  gClrbar.isVisible := VisibleClrbarMenu.checked;
  {$ENDIF}
  TransBlackClrbarMenu.Checked := true;
  ClrbarClr(4);
  //gPrefs.ColorBarPosition := 3;
  SetColorbarPosition;
  Vol1.Azimuth := 110;
  Vol1.Elevation := 30;
  Vol1.Pitch := 0;
  if gPrefs.FlipYZ then
     Vol1.Pitch := -90;
  ClipDepthTrack.Position := 0;
  ClipAziTrack.Position := 180;
  ClipElevTrack.Position := 0;
  ClipThickTrack.Position:= ClipThickTrack.max;
  LightElevTrack.Position := 45;
  LightAziTrack.Position := 0;
  ShaderDrop.ItemIndex := 0;
  ShaderDropChange(Sender);
  Vol1.Slices.ZoomCenter := Vec3(0.5,0.5,0.5);
  Vol1.Slices.distanceLineOrient := 0;
  sliceZoom.position := 100;
  ss := getKeyshiftstate;
  {$IFDEF MATCAP}
  if (MatCapDrop.Items.Count > 0) and (MatCapDrop.ItemIndex <> 0) then begin
     MatCapDrop.ItemIndex := 0;
     Vol1.SetMatCap(MatCapDrop.Items[MatCapDrop.ItemIndex]);
  end;
  {$ENDIF}
  if (Sender <> nil) and (ssShift in ss) then begin
    LineWidthEdit.value := 1;
    //gPrefs.DisplayOrient := kRenderOrient;
    //gPrefs.LineWidth := 1;
    {$IFDEF CLRBAR}
    gClrbar.RulerColor := SetRGBA(Vol1.Slices.LineColor);
    {$ENDIF}
    RulerVisible();
    SetDefaultPrefs(gPrefs,true);
    Vol1.Slices.LineColor := Vec4(gPrefs.LineColor.R/255.0, gPrefs.LineColor.G/255.0, gPrefs.LineColor.B/255.0, gPrefs.LineColor.A/255.0);
    Vol1.Slices.LineWidth := gPrefs.LineWidth;
    UpdateOpenRecent();
  end;
  UpdateVisibleBoxes();
end;

procedure TGLForm1.DisplayMenuClick(Sender: TObject);
begin
     gPrefs.DisplayOrient := (sender as TMenuItem).tag;
     //gPrefs.StartupDisplayOrient:= gPrefs.DisplayOrient; //set so program remembers preferred view
     UpdateVisibleBoxes();
     if gPrefs.DisplayOrient = kMosaicOrient then begin
       {$IFDEF MOSAIC}
       UpdateMosaic(Sender);
       {$ELSE}
       gPrefs.DisplayOrient := kRenderOrient;
       {$ENDIF}
     end else
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

(*procedure TGLForm1.ReloadShader(isUpdatePrefs: boolean);
var
 shaderName: string;
begin
 shaderName := ShaderDir +pathdelim+ ShaderDrop.Items[ShaderDrop.ItemIndex]+kExt;
 Vol1.SetShader(shaderName, isUpdatePrefs);
 setShaderSliders;
end;  *)

procedure TGLForm1.ShaderDropChange(Sender: TObject);
var
 shaderName: string;
begin
 if ShaderDrop.Items.Count < 1 then begin
   showmessage('Missing plug ins: Unable to find '+ShaderDir);
   exit;
 end;
 //ReloadShader(true);
 shaderName := ShaderDir +pathdelim+ ShaderDrop.Items[ShaderDrop.ItemIndex]+kExt;
 Vol1.SetShader(shaderName);//, Vol1.Quality1to10 = 10);
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
  if gPrefs.ScreenCaptureTransparentBackground then
     gPrefs.ClearColor.A := 0;
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
     if (Sender as TLabel).tag = 3 then
        IncTrackBar(ClipThickTrack)
     else if (Sender as TLabel).tag = 2 then
        IncTrackBar(ClipElevTrack)
     else if (Sender as TLabel).tag = 1 then
          IncTrackBar(ClipAziTrack)
     else if (Sender as TLabel).tag = 0 then
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
  {$IFDEF MOSAIC}
  Vol1.Slices.MosaicScale(gPrefs.MosaicStr, vol.Mat, vol.InvMat, vol.Dim, vol.Scale, w,h);
  {$ENDIF}
  {$IFDEF CLRBAR}
  if not gClrbar.isVisible then exit;
  f := gClrBar.PanelFraction;
  if (f > 0.0) then begin
    f := 1.0 +(f/(1-f)); //e.g. if frac is 20%, must increase by 25% (100*1.25 = 125)
    if (gClrBar.isVertical) then
       w := round(w * f)
    else
        h := round(h * f);
  end;
  {$ENDIF}
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
  ViewGPU1.Invalidate;
  Vol1.SaveBmp(bmpName, gPrefs.ScreenCaptureTransparentBackground);
  ViewGPU1.align := alClient;
  ViewGPU1.Invalidate;
end;

{$ELSE}
procedure TGLForm1.SaveMosaicBmp(bmpName: string);
begin
     //metal only
end;

(*function ScreenShotGL(GLBox : TOpenGLControl): TBitmap;
var
  RawImage: TRawImage;
  p: array of byte;
  tileW, tileH, nTileW, nTileH,
  wOut, hOut,
  q, w, w4, yOut, h, x, y, hR, wR, BytePerPixel: integer;
  z: int64;
  DestPtr: PInteger;
begin
 if (gPrefs.BitmapZoom = 1) and (gPrefs.DisplayOrient <> kMosaicOrient) then begin
    result := ScreenShot(GLBox, gPrefs.ScreenCaptureTransparentBackground); //if you get an error here, update your metal-demos
    exit;
 end;
 w := GLBox.ClientWidth;
 h := GLBox.ClientHeight;
 GLForm1.OptimalMosaicPixels(wOut,hOut);
 //showmessage(format('%d %d', [wOut, hOut])); exit;
 wOut := wOut * gPrefs.BitmapZoom;
 hOut := hOut * gPrefs.BitmapZoom;
 if (wOut <= 0) or (hOut <= 0) or (w <= 0) or (h <= 0) then exit(nil);
 nTileW := ceil(wOut/w);
 nTileH := ceil(hOut/h);
 //GLForm1.Caption := format('%d %d   %d %d',[h,w, hOut, wOut]);
 Result:=TBitmap.Create;
 Result.Width:= wOut;
 Result.Height:= hOut;
 if gPrefs.ScreenCaptureTransparentBackground then
   Result.PixelFormat := pf32bit
 else
     Result.PixelFormat := pf24bit; //if pf32bit the background color is wrong, e.g. when alpha = 0
 RawImage := Result.RawImage;
 BytePerPixel := RawImage.Description.BitsPerPixel div 8;
 //note lines can be padded, on MacOS the results are evenly divisible by 16
 // e.g. bmp=4 wOut=994 BytesPerLine=3984 (expected 3976)
 if (RawImage.Description.BytesPerLine < (wOut *BytePerPixel)) then begin
    Showmessage(format('Catastrophic bmp error bpp=%d wid=%d bytes=%d',[BytePerPixel, wOut, RawImage.Description.BytesPerLine])); //not sure if this is fatal - e.g. line lengths rounded up
    exit;
 end;
 //GLForm1.caption := format('%d %d %d 3=%d 4=%d', [BytePerPixel,RawImage.Description.BytesPerLine, wOut, wOut * 3, wOut * 4]);
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
        Inc(PByte(DestPtr), TileW * w * BytePerPixel );
        z := hR * w4;
        if BytePerPixel = 3 then begin
          for y:= hR-1 downto 0 do begin
               DestPtr := PInteger(RawImage.Data);
               Inc(PByte(DestPtr), y * RawImage.Description.BytesPerLine );
               for x := 1 to wR do begin
                   DestPtr^ := p[z] + (p[z+1] shl 8) + (p[z+2] shl 16);
                   Inc(PByte(DestPtr), BytePerPixel);
                   z := z + 4;
               end;
           end; //for y : each line in image
        end else begin
          for y:= hR-1 downto 0 do begin
              Dec(z,w4);
              System.Move(p[z], DestPtr^, wR * BytePerPixel );
              //if (y > 1) then
              Inc(PByte(DestPtr), RawImage.Description.BytesPerLine);
          end; //for y : each line in image
        end;
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
end;*)

function ScreenShotGL(GLBox : TOpenGLControl): TBitmap;
var
  RawImage: TRawImage;
  p: array of byte;
  tileW, tileH, nTileW, nTileH,
  wOut, hOut,
  q, w, w4, yOut, h, x, y, hR, wR, BytePerPixel: integer;
  z: int64;
  DestPtr: PInteger;
begin
 if (gPrefs.BitmapZoom = 1) and (gPrefs.DisplayOrient <> kMosaicOrient) then begin
    q := Vol1.Quality1to5;
    Vol1.Quality1to5 := 5;
    //if (gPrefs.DisplayOrient <> kRenderOrient) then exit;
    if q <> 6 then begin
       GLForm1.BetterRenderTimer.Enabled := false;
       GLForm1.BetterRenderTimer.Tag := 1;
       GLForm1.ViewGPUPaint(nil);
    end;
    result := ScreenShot(GLBox, gPrefs.ScreenCaptureTransparentBackground); //update Metal-Demos if you get an error here
    Vol1.Quality1to5 := q;
    exit;
 end;
 w := GLBox.ClientWidth;
 h := GLBox.ClientHeight;
 GLForm1.OptimalMosaicPixels(wOut,hOut);
 //showmessage(format('%d %d', [wOut, hOut])); exit;
 wOut := wOut * gPrefs.BitmapZoom;
 hOut := hOut * gPrefs.BitmapZoom;
 if (wOut <= 0) or (hOut <= 0) or (w <= 0) or (h <= 0) then exit(nil);
 nTileW := ceil(wOut/w);
 nTileH := ceil(hOut/h);
 //GLForm1.Caption := format('x%d %dx%d   %d %d',[gPrefs.BitmapZoom, w,h, wOut, hOut]);
 Result:=TBitmap.Create;
 Result.Width:= wOut;
 Result.Height:= hOut;
 //Result.PixelFormat := pf24bit; //if pf32bit the background color is wrong, e.g. when alpha = 0
 {$IFDEF Windows} //at least for Lazarus 2.0.2
 Result.PixelFormat := pf32bit;
 {$ELSE}
 if gPrefs.ScreenCaptureTransparentBackground then
   Result.PixelFormat := pf32bit
 else
     Result.PixelFormat := pf24bit; //if pf32bit the background color is wrong, e.g. when alpha = 0
 {$ENDIF}
 RawImage := Result.RawImage;
 BytePerPixel := RawImage.Description.BitsPerPixel div 8;
 if (RawImage.Description.BytesPerLine <  (wOut *BytePerPixel)) then begin
    showmessage(format('Catastrophic error bytes=%d bpp=%d w=%d', [RawImage.Description.BytesPerLine, BytePerPixel, wOut]));

 end;
 GLBox.MakeCurrent;
 q := Vol1.Quality1to5;
 Vol1.Quality1to5 := 5;
 //if (q > 5) then GLForm1.ReloadShader(false); //best quality
 w4 := 4 * w;
 yOut := 0;
 hR := 0;
 setlength(p, w4 * h);
 for TileH := 0 to  (nTileH-1) do begin
     for TileW := 0 to (nTileW-1) do begin
        {$IFDEF RETINA}
        GLBox.enableTiledScreenShot(-TileW*w, -h*TileH,wOut, hOut); //tileLeft, tileBottom,totalWidth, totalHeight
        {$ENDIF}
        GLForm1.ViewGPUPaint(nil);
        glFlush;
        glFinish;//<-this would pause until all jobs finished: generally a bad idea! required here
        {$IFDEF Linux} //QT5 glFinish returns before finishing!
        sleep(10);
        glFlush;
        glFinish;//<-this would pause until all jobs finished: generally a bad idea! required here
        sleep(10);
        {$ENDIF}
        {$IFDEF Windows}
        GLBox.SwapBuffers; //<- required by Windows
        {$ENDIF}
        {$IFDEF Darwin} //http://lists.apple.com/archives/mac-opengl/2006/Nov/msg00196.html
        glReadPixels(0, 0, w, h, $80E1, $8035, @p[0]); //OSX-Darwin   GL_BGRA = $80E1;  GL_UNSIGNED_INT_8_8_8_8_EXT = $8035;
        {$ELSE} {$IFDEF Linux}
        // glReadPixels(0, 0, w, h, GL_RGBA, GL_UNSIGNED_BYTE, @p[0]); //Linux-Windows   GL_RGBA = $1908; GL_UNSIGNED_BYTE
         glReadPixels(0, 0, w, h, $80E1, GL_UNSIGNED_BYTE, @p[0]); //https://github.com/rordenlab/MRIcroGL12/issues/9
        {$ELSE}
         glReadPixels(0, 0, w, h, $80E1, GL_UNSIGNED_BYTE, @p[0]); //Linux-Windows   GL_RGBA = $1908; GL_UNSIGNED_BYTE
        {$ENDIF} {$ENDIF}
        DestPtr := PInteger(RawImage.Data);
        hR := min(h, hOut-yOut);
        wR := min(w, wOut-(TileW*w) ) ;
        Inc(PByte(DestPtr), (hOut - yOut - hR) * RawImage.Description.BytesPerLine );
        Inc(PByte(DestPtr), TileW * w * BytePerPixel );
        z := hR * w4;
        if BytePerPixel = 3 then begin
          for y:= hR-1 downto 0 do begin
               DestPtr := PInteger(RawImage.Data);
               Inc(PByte(DestPtr), y * RawImage.Description.BytesPerLine );
               for x := 1 to wR do begin
                   DestPtr^ := p[z] + (p[z+1] shl 8) + (p[z+2] shl 16);
                   Inc(PByte(DestPtr), BytePerPixel);
                   z := z + 4;
               end;
           end; //for y : each line in image
        end else begin
          for y:= hR-1 downto 0 do begin
              Dec(z,w4);
              System.Move(p[z], DestPtr^, wR * BytePerPixel );
              //if (y > 1) then
              Inc(PByte(DestPtr), RawImage.Description.BytesPerLine);
          end; //for y : each line in image
        end;
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
 {$IFDEF RETINA}
 GLBox.disableTiledScreenShot();
 {$ENDIF}
 GLbox.ReleaseContext;
 Vol1.Quality1to5 := q;
 setlength(p, 0);
 //if (q <= 5) then GLForm1.ReloadShader(false); //best quality
 ViewGPU1.Invalidate;
end;
{$ENDIF}

procedure TGLForm1.GraphSaveBitmapMenuClick(Sender: TObject);
{$IFDEF GRAPH}
{$IFDEF METALAPI}
begin
 //ViewGPU1.MakeCurrent(false);
 //ViewGPUg.Invalidate;
 //gGraph.SaveBmp(''); //2021
 //exit;
 if not SaveDialog1.execute then exit;
 //bmp := ScreenShotGL(ViewGPUg);
 gGraph.SaveBmp(SaveDialog1.Filename, gPrefs.ScreenCaptureTransparentBackground);
end;
{$ELSE}
var
   bmp: TBitmap;
   png: TPortableNetworkGraphic;
begin
  if not SaveDialog1.execute then exit;
  //bmp := ScreenShotGL(ViewGPUg);
  bmp := ScreenShot(ViewGPUg);
  if (bmp = nil) then exit;
  png := TPortableNetworkGraphic.Create;
  try
   png.Assign(bmp);    //Convert data into png
   png.SaveToFile(SaveDialog1.Filename);
  finally
    png.Free;
  end;
  bmp.Free;
end;
{$ENDIF}
{$ELSE}
begin
  //Showmessage('compile for graph');
end;
{$ENDIF}

procedure TGLForm1.ExitFullScreenMenuClick(Sender: TObject);
begin
  GLForm1.WindowState:= wsNormal;
  ExitFullScreenMenu.Visible := false;
end;


procedure TGLForm1.EditPasteMenuClick(Sender: TObject);
var
  TActive: TWinControl;
begin
 (*if (HdrForm.Visible) and (Screen.ActiveForm = HdrForm) then begin
    inherited;
    exit;
 end; *)
 (*caption := inttostr(random(888));
 if Clipboard.HasFormat(PredefinedClipboardFormat(pcfText)) and (ScriptMemo.Width > 20) and (ScriptMemo.Focused) then begin
    ScriptMemo.PasteFromClipboard;
    exit;
 end; *)
 TActive := Screen.ActiveControl;
 //LCLSendPasteFromClipboardMsg(TActive);
 if (TActive is TEdit) then
      (TActive as TEdit).PasteFromClipboard
   else if (TActive is TSpinEdit) then
      (TActive as TSpinEdit).PasteFromClipboard
   else if (TActive is TFloatSpinEdit) then
      (TActive as TFloatSpinEdit).PasteFromClipboard
   else if (TActive is TMemo) then
      (TActive as TMemo).PasteFromClipboard;
 //inherited;//showmessage('Use paste to insert text from clipboard into scripts');
end;

procedure TGLForm1.GraphPanelResize(Sender: TObject);
begin
 {$IFDEF GRAPH}
 //ViewGPUg.Invalidate;
 {$ENDIF}

end;

procedure TGLForm1.EditCopyMenuClick(Sender: TObject);
{$IFNDEF METALAPI}
var
  bmp: TBitmap;
{$ENDIF}
var
  TActive: TWinControl;
begin
  if (ClusterView.Focused) then begin
     ClusterCopyMenu.Click;
     exit;
  end;
  if (ScriptOutputMemo.Focused) then begin
    if (ScriptOutputMemo.SelLength < 1) then
       ScriptOutputMemo.SelectAll();
    ScriptOutputMemo.CopyToClipboard;
    exit;
 end;
 if (ScriptMemo.Focused) then begin
    if (ScriptMemo.SelLength < 1) then
       ScriptMemo.SelectAll();
    ScriptMemo.CopyToClipboard;
    exit;
 end;
 TActive := Screen.ActiveControl;
 if (TActive is TEdit) then begin
    (TActive as TEdit).CopyToClipboard;
    exit;
 end else if (TActive is TFloatSpinEdit) then begin
    (TActive as TFloatSpinEdit).CopyToClipboard;
    exit;
 end else if (TActive is TSpinEdit) then begin
    (TActive as TSpinEdit).CopyToClipboard;
    exit;
 end else if (TActive is TMemo) then begin
    (TActive as TMemo).CopyToClipboard;
    exit;
 end;


 {$IFNDEF METALAPI}
 bmp := ScreenShotGL(ViewGPU1);
 if (bmp = nil) then exit;
 Clipboard.Assign(bmp);
 bmp.Free;
{$ELSE}
  ViewGPU1.Invalidate; //in case Graph has context
  Save2Bmp('');
{$ENDIF}
end;

procedure TGLForm1.DisplayAnimateMenuClick(Sender: TObject);
begin
    AnimateTimer.enabled := DisplayAnimateMenu.Checked;
end;

function TGLForm1.Active4DLayer: integer;
var
  i: integer;
  v: TNIfTI;
begin
     i := LayerList.ItemIndex;
     if not vols.Layer(i,v) then exit(-1);
     if v.VolumesTotal > 1 then exit(i);
     for i := 0 to (vols.NumLayers-1) do begin
       vols.Layer(i,v);
       if v.VolumesTotal > 1 then exit(i);
     end;
     exit(-1);
end;

procedure TGLForm1.AnimateTimerTimer(Sender: TObject);
var
    v: TNIfTI;
    i: integer;
begin
  //BUBBA
  i := Active4DLayer;
  if i >= 0 then begin
     vols.Layer(i,v);
     v.SetDisplayVolume(v.VolumeDisplayed + 1);
     UpdateLayerBox(false, true);// e.g. "fMRI (1/60)" -> "fMRI (2/60"
     //UpdateTimer.Enabled := true;
     UpdateTimerTimer(Sender);
     {$IFDEF GRAPH}
     if i > 0 then exit; //TGV
     gGraph.HorizontalSelection:= v.VolumeDisplayed;
     gGraph.isRedraw := true;
     ViewGPUg.Invalidate;
     {$ENDIF}
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
     i := Active4DLayer;
     if i < 0 then exit(false);
     vols.Layer(i,v);
     if (Sender as TMenuItem).Tag = 1 then
        v.SetDisplayVolume(v.VolumeDisplayed + 1)
     else
         v.SetDisplayVolume(v.VolumeDisplayed - 1);
     UpdateLayerBox(false, true);
     //UpdateLayerBox(true);// e.g. "fMRI (1/60)" -> "fMRI (2/60"
     UpdateTimer.Enabled := true;
     {$IFDEF GRAPH}
     if (i > 0) then exit(true);
     gGraph.HorizontalSelection:= v.VolumeDisplayed;
     gGraph.isRedraw := true;
     ViewGPUg.Invalidate;
     {$ENDIF}
     exit(true);
end;

(*function TGLForm1.DisplayNextMenuClick(Sender: TObject): boolean;
var
   v: TNIfTI;
   i: integer;
begin
 result := false;

 for i := 1 to vols.NumLayers do begin
   vols.Layer(i-1,v);
   if v.VolumesTotal < 2 then
      continue;
   if (Sender as TMenuItem).Tag = 1 then
      v.SetDisplayVolume(v.VolumeDisplayed + 1)
   else
       v.SetDisplayVolume(v.VolumeDisplayed - 1);
   UpdateLayerBox(false, true);
   //UpdateLayerBox(true);// e.g. "fMRI (1/60)" -> "fMRI (2/60"
   UpdateTimer.Enabled := true;
   {$IFDEF GRAPH}
   gGraph.HorizontalSelection:= v.VolumeDisplayed;
   gGraph.isRedraw := true;
   ViewGPUg.Invalidate;
   {$ENDIF}
   exit(true);
 end;
end;  *)

procedure TGLForm1.GraphMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
 {$IFDEF GRAPH}
 //TGV
 if not DisplayNextMenu.Enabled then exit;
 if LayerList.ItemIndex <>0 then
    LayerList.ItemIndex:= 0;
 if (WheelDelta < 0) then
    DisplayNextMenu.Click
 else
     DisplayPrevMenu.Click;
 {$ENDIF}
end;

procedure TGLForm1.ViewGPUgKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
     if not DisplayNextMenu.Enabled then exit;
     if (Key = VK_LEFT) or (Key = VK_Down) then DisplayPrevMenu.Click;
     if (Key = VK_RIGHT) or (Key = VK_Up) then DisplayNextMenu.Click;
     //
end;

procedure TGLForm1.ViewGPUgResize(Sender: TObject);
begin
  {$IFDEF GRAPH}
  inherited;
  ViewGPUg.Invalidate;

  (*if not DisplayNextMenu.Enabled then exit;
  ViewGPU1.Invalidate;
  gGraph.isRedraw := true;
  ViewGPUg.Invalidate;
  UpdateLayerBox(false, true);// e.g. "fMRI (1/60)" -> "fMRI (2/60"
  UpdateTimer.Enabled := true; *)
  //Caption := format('%d + %d = %d', [GraphPanel.ClientWidth, ClusterPanel.ClientWidth, GLForm1.width]);
  {$ENDIF}
end;

procedure TGLForm1.GraphMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
var
   f: double;
   v: TNIfTI;
   vol: integer;
begin
     {$IFDEF GRAPH}
    {$IFDEF isCocoaOpenGL}
    f := ViewGPUg.retinaScale;
    X := round(X * f);
    Y := round(Y * f);
    {$ENDIF}
     f := gGraph.HorizontalClickFrac(X);
     if (f < 0) or (f > 1) then exit;
     if f = 1 then f := f - 0.000001;
     if LayerList.ItemIndex <>0 then
        LayerList.ItemIndex:= 0;
     if not vols.Layer(0,v) then exit;
     if (v.VolumesLoaded < 2) then exit;
     //TGV
     vol := trunc(v.VolumesLoaded * f) ; // 0...(v.VolumesTotal-1)
     v.SetDisplayVolume(vol);
     gGraph.HorizontalSelection:= v.VolumeDisplayed;
     gGraph.isRedraw := true;
     ViewGPUg.Invalidate;
     UpdateLayerBox(false, true);// e.g. "fMRI (1/60)" -> "fMRI (2/60"
   UpdateTimer.Enabled := true;
     {$ENDIF}
end;

procedure TGLForm1.DrawHintsMenuClick(Sender: TObject);
begin
  showmessage('Drag mouse to draw'+kEOLN
+'Shift+Drag to erase'+kEOLN
+'Control+Click to change view'+kEOLN
+'Option+Click for flood fill'+kEOLN
+'Mouse-Wheel to change slice'+kEOLN )
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
  if v.VolumesTotal < 2 then exit;
  if (Sender as TMenuItem).Tag = 1 then
     v.SetDisplayVolume(v.VolumeDisplayed + 1)
  else
      v.SetDisplayVolume(v.VolumeDisplayed - 1);
  //caption := format('%d/%d ',[v.VolumeDisplayed+1, v.VolumesTotal]); //+1 as indexed from 1
  UpdateLayerBox(true);// e.g. "fMRI (1/60)" -> "fMRI (2/60"
  UpdateTimer.Enabled := true;
end;

procedure TGLForm1.MaskMenuClick(Sender: TObject);
var
   niftiVol: TNIfTI;
begin
  if not vols.Layer(0,niftiVol) then exit;
  if not vols.Drawing.IsOpen then begin
     showmessage('Open or create a drawing to use as a mask');
     exit;
  end;
  niftiVol.Mask(vols.Drawing.VolRawBytes, ((sender as TMenuItem).Tag <> 0));
  UpdateTimer.Enabled := true;
end;

procedure TGLForm1.MouseGesturesMenu(Sender: TObject);
const
  {$IFDEF Darwin}
  kAlt = 'Command';
  kAlt2 = 'Option';
  {$ELSE}
  kAlt = 'Alt';
  kAlt2 = 'Alt';
  {$ENDIF}
var
  s: string;
begin
 s := 'Display 3D Render'
    +kEOLN+'  Drag: Rotate view'
    +kEOLN+'Display 2D Slices'
    +kEOLN+'  Click: Move crosshair'
    +kEOLN+'  Shift-Drag: Adjust contrast'
    +kEOLN+'  Mouse-Wheel: Change slice'
    +kEOLN+'  Control-Mouse-Wheel: Change zoom'
    +kEOLN+'  Control-Shift-Drag: Pan image'
    +kEOLN+'  Control-Double-Click: Pan to center clicked location'
    +kEOLN+'  Control-Shift-Double-Click: Reset pan and zoom'
    +kEOLN+'  '+kAlt2+'-Drag: Measuring tool'
    +kEOLN+'Colorbar'
    +kEOLN+'  Double-Click: Change position'
    +kEOLN+'Color Editor'
    +kEOLN+'  Drag Node: Change node intensity/opacity'
    +kEOLN+'  Shift-Click Node: Delete node'
    +kEOLN+'  Double-Click Node: Edit node color'
    +kEOLN+'  Control-Click: Add node'
    +kEOLN+'  Alt-Click: Save colors to disk'
    +kEOLN+'Drawing (Draw/DrawColor selected)'
    +kEOLN+'  Drag: draw filled region'
    +kEOLN+'  Shift-Drag: erase filled region'
    +kEOLN+'  '+kAlt2+'-Down: Briefly hide drawing'
    +kEOLN+'  '+kAlt+'-Click: Move crosshair';
  //{$IFDEF NewCocoa}
  //ShowAlertSheet(GLForm1.Handle,'Mouse Gestures', s);
  //{$ELSE}
  Showmessage(s);
  //{$ENDIF}
end;

procedure TGLForm1.LayerCloseMenuClick(Sender: TObject);
begin
 vols.CloseLayer(LayerList.ItemIndex);
 UpdateLayerBox(true);
 UpdateTimer.Enabled := true;
end;

procedure TGLForm1.AfniNextClick(Sender: TObject);
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
  {$IFDEF DARKMODE}
  if gPrefs.DarkMode then GLForm1.SetFormDarkMode(PrefForm);
  {$ENDIF}
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
  if HdrForm.Visible then begin
     HdrForm.BringToFront;
     exit;
  end;
  i := LayerList.ItemIndex;
  if (i < 0) or (i >= LayerList.Count) then exit;
  if not vols.Layer(LayerList.ItemIndex,niftiVol) then exit;
  {$IFDEF DARKMODE}
  setThemeMode(HdrForm, gPrefs.DarkMode);
  {$ENDIF}
  HdrForm.WriteHdrForm(niftiVol.HeaderNoRotation, niftiVol.IsNativeEndian, niftiVol.Filename, niftiVol.Dim, niftiVol.VolumesLoaded);
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

function TGLForm1.Save2Bmp(fnm: string): boolean;
var
{$IFDEF METALAPI}
 q: integer;
{$ELSE}
bmp: TBitmap;
png: TPortableNetworkGraphic;
{$ENDIF}
begin
 result := false;
 while (isBusy) or (GLForm1.Updatetimer.enabled) do
       Application.ProcessMessages; //apply any time consuming updates
 if fnm <> '' then begin
    fnm := ChangeFileExt(fnm,'.png');
    fnm := ensurePath(fnm);
 end;
 {$IFDEF METALAPI} //to do: OpenGL must using tiling due to "pixel ownership problem"
 q := Vol1.Quality1to5;
 Vol1.Quality1to5 := 5;
 //gPyRunning := true;
 BetterRenderTimer.tag := 0;
 //BetterRenderTimer.enabled := false; //20200616
 if (gPrefs.DisplayOrient = kMosaicOrient) and (gPrefs.MosaicStr <> '') then begin
     SaveMosaicBmp(fnm);
    Vol1.Quality1to5 := q;
    exit(false);
 end;
 ViewGPU1.Invalidate; //20200616
 Vol1.SaveBmp(fnm, gPrefs.ScreenCaptureTransparentBackground);
 Vol1.Quality1to5 := q;
 {$ELSE}
  bmp := ScreenShotGL(ViewGPU1);
  if (bmp = nil) then exit(false);
  result := true;
  png := TPortableNetworkGraphic.Create;
  try
   try
      png.Assign(bmp);    //Convert data into png
      png.SaveToFile(fnm);
   except
     result := false;
   end;
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

(*function sph2cartDeg(Azimuth,Elevation: single): TVec4;
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
end;  *)

function sph2cartDeg(Azimuth,Elevation: single): TVec4;
//convert spherical AZIMUTH,ELEVATION,RANGE to Cartesion
//see Matlab's [x,y,z] = sph2cart(THETA,PHI,R)
// reverse with cart2sph
var
  A,E, Phi,Theta: single;
begin
 E := Elevation;
 //while E > 90 do
 //    E := E - 90;
 //while E < -90 do
 //    E := E + 90;
 Phi := DegToRad(E);
 A := Azimuth;
  while A < 0 do
    A := A + 360;
  while A > 360 do
    A := A - 360;
  Theta := DegToRad(A);
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
var
   v: boolean;
begin
 Vol1.Quality1to5 := QualityTrack.Position;
 Vol1.LightPosition := sph2cartDeg90Light(LightAziTrack.Position, LightElevTrack.Position);
 //Caption := format('%g %g %g', [Vol1.LightPosition.X, Vol1.LightPosition.Y, Vol1.LightPosition.Z]);
 if ClipDepthTrack.Position = 0 then
     Vol1.clipPlane := sph2cartDeg90clip(ClipAziTrack.Position, ClipElevTrack.Position, -1.0 )
 else
     Vol1.clipPlane := sph2cartDeg90clip(ClipAziTrack.Position, ClipElevTrack.Position, ClipDepthTrack.Position/(ClipDepthTrack.Max+1) );
 v :=  ClipDepthTrack.Position <> 0;
 if (v <> ClipAziTrack.enabled) then begin
    ClipAziTrack.enabled := v;
    ClipElevTrack.enabled := v;
    ClipThickTrack.enabled := v;
 end;
 Vol1.ClipThick := 1.73 * ClipThickTrack.Position/ClipThickTrack.Max; //1.73 as opposite corners of 1x1x1mm cube are 1.73mm apart
 ClipBox.Hint := (format('clipazimuthelevation(%.2f %d %d)',[ClipDepthTrack.Position/ClipDepthTrack.Max , ClipAziTrack.Position, ClipElevTrack.Position]));
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
 {$IFDEF WINDOWS}
 s := ReplaceStr(s, '\', '/');
 {$ENDIF}
 if layers = 0 then
    script.Add(format('gl.loadimage(''%s'')',[s]) )
 else
     script.Add(format('gl.overlayload(''%s'')',[s]) );
 layers := layers + 1;
end;
//e.g. ./MRIcroGL -std  motor -cm actc -dr 2 4
begin
     {$IFNDEF MYPY}
     exit;
     {$ENDIF}
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
           else if (upcase(s[2]) = 'P') then begin//e.g. '-psn_0_989382' https://stackoverflow.com/questions/10242115/os-x-strange-psn-command-line-parameter-when-launched-from-finder
                {$IFDEF Darwin}
                if s.StartsWith('-psn', true) then
                   script.Add(format('#MacOS process serial number ''%s'')',[s]) );
                {$ENDIF}
                s := DefaultImage();
                AddImg();
           end else if (upcase(s[2]) = 'C') and (upcase(s[3]) = 'M') and (i <= ParamCount) then begin//e.g. '-cm actc'
              s := ParamStr(i);
              inc(i);
              script.Add(format('gl.colorname (%d,"%s")',[max(layers-1,0), s]) );
           end else if (upcase(s[2]) = 'D') and (upcase(s[3]) = 'R') and ((i+1) <= ParamCount) then begin//e.g. '-dr 2 5'
              lo := strtofloatdef(ParamStr(i), 0.0);
              inc(i);
              hi := strtofloatdef(ParamStr(i), 01.0);
              inc(i);
              script.Add(format('gl.minmax (%d, %g, %g)',[max(layers-1,0), lo, hi]) );
           end else if (upcase(s[2]) = 'H')  then begin//e.g. '-h'
              {$IFDEF UNIX}
              printf('');
              printf('MRIcroGL '+kVers);
              printf(' Usage: MRIcroGL [options] file [displayOpts] file [displayOpts]');
              printf('   -h  : Display help');
              printf('   -cm : Color map: "-cm actc"');
              printf('   -dr : Display range: "-dr 2 5"');
              printf('   -std : Load standard image: "-std" ');
              printf('   -m : Set MaxVox (downsample large images). n.b. retained in preferences: "-m 256" ');
              printf('   -r : Reset all preferences to defaults (recovery)');
              printf(' Examples:');
              printf('   MRIcroGL spm152 spmMotor  -cm actc -dr 2 5');
              printf('   MRIcroGL spm152 -cm copper -dr 25 70 spmMotor  -cm actc -dr 2 5');
              printf('   MRIcroGL ~/myScript.py');
              printf(' ');
              printf(' ');
              {$ENDIF}
           end else             //gl.minmax(2, -4, -4)
               printf('Unknown argument "'+s+'"');
           //printf('>>Unknown argument "'+upcase(s[2])+upcase(s[3])+'"');
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
    {$IFDEF METALAPI}{$IFDEF GRAPH}
    ViewGPUg.InvalidateOnResize := true; //20200606
    {$ENDIF} {$ENDIF}
    s := gPrefs.InitScript;
    gPrefs.InitScript := '';
    //if ParamCount < 1 then exit;
    if (ParamCount > 0) and (s = ('-')) then begin
       s := ParamStr(ParamCount);
       if (upcase(ExtractFileExt(s)) <> '.PY') and (upcase(ExtractFileExt(s)) <> '.TXT') then begin
          printf('Assuming arguments are images not script (not .py or .txt) "'+s+'"');

          ParamStr2Script();// AddBackground(s);
          exit;
       end;
       //gPrefs.InitScript := s;
    end;
    if FileExists(ScriptDir+pathdelim+s) then
       s := ScriptDir+pathdelim+s;
    if FileExists(GetCurrentDir+pathdelim+s) then
       s := GetCurrentDir+pathdelim+s;
    if (not FileExists(s)) then begin
       //{$IFDEF UNIX}writeln('Unable to find file provided from command line: '+s);{$ENDIF}
       printf('Reading input as literal Python script (use "^" as end-of-line delimiter)');
       ScriptMemo.Lines.Clear;
       //ScriptMemo.Lines.Add(s);
       ScriptMemo.Lines.Delimiter:= '^';
       ScriptMemo.Lines.StrictDelimiter:=true;
       ScriptMemo.Lines.DelimitedText := s;
       //ScriptMemo.Lines.AddText(s);
       ScriptingRunMenuClick(nil);

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

{$IFDEF LCLCocoa}
function HWmodel : AnsiString;
{$macro on}
type
  PSysCtl = {$IF FPC_FULLVERSION>30100}pcint{$ELSE}pchar{$ENDIF};
var
  mib : array[0..1] of Integer;
  status : Integer;
  {$IFDEF CPU64}
  len : Int64;
  {$ELSE}
  len : Int32;
  {$ENDIF}
  p   : PChar;
begin
 mib[0] := CTL_HW;
 mib[1] := HW_MODEL;
 status := fpSysCtl(PSysCtl(@mib), Length(mib), Nil, @len, Nil, 0);
 //status := fpSysCtl(PCInt(@mib), Length(mib), Nil, @len, Nil, 0);
 if status <> 0 then RaiseLastOSError;
 GetMem(p, len);
 try
   status := fpSysCtl(PSysCtl(@mib), Length(mib), p, @len, Nil, 0);
   if status <> 0 then RaiseLastOSError;
   Result := p;
 finally
   FreeMem(p);
 end;
 {$IFNDEF CPULLVM}
 Result := Result + ' FPC:'+inttostr(FPC_FULLVERSION);
 {$ENDIF}
end;
{$ENDIF}

function versionStr: string;
var
  w: string;
begin
 w := '';
 {$IFDEF LCLCocoa}
 w := 'Cocoa';
 {$ENDIF}
 {$IFDEF LCLQT5}
 w := 'QT5';
 {$ENDIF}
 {$IFDEF LCLQT}
 w := 'QT4';
 {$ENDIF}
 {$IFDEF LCLGTK2}
 w := 'GTK2';
 {$ENDIF}
 {$IFDEF LCLGTK3}
 w := 'GTK3';
 {$ENDIF}
 {$IFDEF LCLWin64}
 w := 'Windows';
 {$ENDIF}
  {$IFDEF CPUX86_64}
  w := w + ' x86-64';
  {$ENDIF}
  {$IFDEF CPUAARCH64}
  w := w + ' ARM64';
  {$ENDIF}
  {$IFDEF CPULLVM}
  w := w + ' LLVM';
  {$ENDIF}
 w := w + chr(13)+chr(10);
 w := w + 'Author: Chris Rorden' +kEOLN;
 w := w + 'License: BSD 2-Clause' +kEOLN;
 {$IFDEF WINDOWS}
 w := w + 'Windows: ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion)+kEOLN;
 {$ENDIF}
 {$IFDEF LCLCocoa}
 //w := w + 'MacOS: 10.' + IntToStr(Lo(DosVersion) - 4) + '.' + IntToStr(Hi(DosVersion))+' '+HWmodel+kEOLN;
  w := w + NSProcessInfo.ProcessInfo.operatingSystemVersionString.UTF8String+' '+HWmodel+kEOLN;


 {$IFNDEF METALAPI}{$IFDEF RETINA}
  w := w + format('Retina Scale: %g', [ViewGPU1.retinaScale])+kEOLN;
  {$ENDIF}{$ENDIF}
 {$ENDIF}
 {$IFDEF Linux}
 w := w + 'Linux'+kEOLN;
 {$ENDIF}
 result := format('%s %s', [kVers, w]);
end;

(*
{$ASMMODE INTEL}
function FMA(const A, B, C: TVec4): TVec4;
begin
	asm
          movups    xmm0, [A]
	  movups    xmm1, [B]
	  movups    xmm2, [C]
          vfmadd231ps xmm0, xmm1, xmm2
          movhlps xmm1, xmm0
          //ret
	end;
end; *)

procedure TGLForm1.AboutMenuClick(Sender: TObject);
var
   niftiVol: TNIfTI;
   v: string;
   {$IFNDEF METALAPI}maxVox: integer;{$ENDIF}
   //A, B, C, D: TVec4;
begin
 (*A := V4(0,0,0,0);//+
 B := V4(10,10,10,10); //+
 C := V4(1,2,3,4); //*
 D := V4(0,0,0,0);

 D := FMA(A,B,C);
 showmessage(format('%g %g %g %g', [D.x, D.y, D.z, D.w])); exit;*)
 v := versionStr;
 if not vols.Layer(0,niftiVol) then exit;
 {$IFDEF METALAPI}
 v := v+'Apple Metal multisample=' + inttostr(ViewGPU1.renderView.sampleCount);
 {$ELSE}
 ViewGPU1.MakeCurrent(false);
 //{$IFDEF MYPY}{$IFDEF PY4LAZ}v := v + 'PythonForLazarus' + kEOLN;{$ELSE}v := v+ 'PythonBridge ' + kEOLN;{$ENDIF}{$ENDIF}
 v := v + glGetString(GL_VENDOR)+'; OpenGL= '+glGetString(GL_VERSION)+'; Shader='+glGetString(GL_SHADING_LANGUAGE_VERSION);
 glGetIntegerv(GL_MAX_3D_TEXTURE_SIZE, @maxVox);  //For 3D textures, no dimension can be greater than GL_MAX_3D_TEXTURE_SIZ
 {$IFDEF COREGL}
 v := v+ kEOLN+ '3.3 Core ';
 {$ELSE}
 v := v+ kEOLN+ '2.1 ' ;
 {$ENDIF}
 v := v+ format('Max 3D texture: %d', [maxVox]);
 ViewGPU1.ReleaseContext;
 {$ENDIF}
 v := v+ kEOLN+ format('Current texture: %d×%d×%d %dmb', [niftiVol.Dim.X, niftiVol.Dim.Y, niftiVol.Dim.Z, round((niftiVol.Dim.X * niftiVol.Dim.Y * niftiVol.Dim.Z * 4.0)/1048576.0)]);
 {$IFDEF NewCocoa}
 ShowAlertSheet(GLForm1.Handle,'MRIcroGL', v);
 //MessageBox(Handle, pChar(v), 'MRIcroGL', MB_OK)
 {$ELSE}
 Showmessage('MRIcroGL '+v);
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



{$IFDEF isCocoaOpenGL}
var
gIsMouseDown: boolean = false;      // https://bugs.freepascal.org/view.php?id=35480
{$ENDIF}

{$IFDEF DEPTHPICKER}{$IFDEF METALAPI}

(*function TGLForm1.MTLCopyLastFrameTextureX(texture: MTLTextureProtocol; hasAlpha: boolean): CGImageRef;
var
  wid, ht, bytesPerRow, bytesCount: integer;
  bytes: pointer;
  colorSpace: CGColorSpaceRef;
  bitmapInfo: CGBitmapInfo;
  provider: CGDataProviderRef;
  imageRef: CGImageRef;
  blitEncoder: MTLBlitCommandEncoderProtocol;
  bitmapContext: CGContextRef;
  decompressedImageRef: CGImageRef;
begin
 ViewGPU1.SharedContext;
	Fatal(CurrentThreadContext = nil, kError_InvalidContext);
	Fatal(texture.pixelFormat <> MTLPixelFormatBGRA8Unorm, 'texture must be MTLPixelFormatBGRA8Unorm pixel format.');

	// read bytes
	wid := texture.width;
	ht := texture.height;
  bytesPerRow := texture.width * 4;
	bytesCount := wid * ht * 4;
	bytes := GetMem(bytesCount);

	// blit from last command buffer
	with CurrentThreadContext do
	begin
		commandBuffer := commandQueue.commandBuffer;
		blitEncoder := commandBuffer.blitCommandEncoder;
		view.draw;
		blitEncoder.synchronizeResource(texture);
		blitEncoder.endEncoding;
		commandBuffer.waitUntilCompleted;
	end;

	// get bytes from texture
  texture.getBytes_bytesPerRow_fromRegion_mipmapLevel(bytes, bytesPerRow, MTLRegionMake2D(0, 0, wid, ht), 0);

  // create CGImage from texture bytes
  colorSpace := CGColorSpaceCreateDeviceRGB;
	bitmapInfo := kCGImageAlphaFirst or kCGBitmapByteOrder32Little;
	provider := CGDataProviderCreateWithData(nil, bytes, bytesCount, nil);
	imageRef := CGImageCreate(wid, ht, 8, 32, bytesPerRow, colorSpace, bitmapInfo, provider, nil, 1, kCGRenderingIntentDefault);

	if not hasAlpha then
		begin
		  bitmapInfo := kCGImageAlphaNoneSkipLast or kCGBitmapByteOrder32Little;
		  bitmapContext := CGBitmapContextCreate(nil, CGImageGetWidth(imageRef), CGImageGetHeight(imageRef), CGImageGetBitsPerComponent(imageRef), CGImageGetBytesPerRow(imageRef),  colorSpace, bitmapInfo);
		  CGContextDrawImage(bitmapContext, CGRectMake(0, 0, CGImageGetWidth(imageRef), CGImageGetHeight(imageRef)), imageRef);
		  decompressedImageRef := CGBitmapContextCreateImage(bitmapContext);
		  CFRelease(imageRef);
		  CFRelease(bitmapContext);
		  result := decompressedImageRef;
	  end
	else
		result := imageRef;

	CFRelease(provider);
	CFRelease(colorSpace);
end;      *)
{$ENDIF}{$ENDIF}
{$IFDEF DEPTHPICKER2}
procedure TGLForm1.PickRenderDepth( X, Y: Integer);
var
 c3: TVec3;
 niftiVol: TNIfTI;
 depth: single;
begin
  if not vols.Layer(0,niftiVol) then exit;
  glDrawBuffer(GL_BACK);
  Vol1.PaintDepth(niftiVol, gPrefs.DisplayOrient = kAxCorSagOrient4);
  glFinish;
  glReadBuffer(GL_BACK);
  glReadPixels( X, Y, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @depth);
  c3 := Vol1.Unproject(X,Y, depth);
  //Vol1.PaintDepth(niftiVol, gPrefs.DisplayOrient = kAxCorSagOrient4);
  //Vol1.PaintDepth(niftiVol, false);
  //LineBox.caption := format('xyz %0.3g %0.3g %0.3g len %g', [c3.X, c3.Y, c3.Z, c3.Length]);
  if (min(c3.X, min(c3.Y, c3.z)) < 0.0) or  (max(c3.X, max(c3.Y, c3.z)) > 1.0) then begin
  	//Caption := '';
    exit;
  end;
  gSliceMM := niftiVol.FracMM(Vec3(c3.X, c3.Y, c3.Z));
  Caption := format('%0.4g×%0.4g×%0.4g', [gSliceMM.x, gSliceMM.y, gSliceMM.z]);
  vol1.SetSlice2DFrac(Vec3(c3.X, c3.Y, c3.Z));
  //gSliceMM := Vec3(sliceMM.X, sliceMM.Y, sliceMM.Z);
  {$IFDEF COMPILEYOKE}
  SetShareFloats2D(gSliceMM.X,gSliceMM.Y,gSliceMM.Z);
  {$ENDIF}
  //if (gPrefs.DisplayOrient = kAxCorSagOrient4) then
  	ViewGPU1.Invalidate
  //else
  //	Vol1.Paint(niftiVol);
  {$IFDEF METALAPI}
  UpdateTimer.Enabled := true;//OKRA
  {$ENDIF}
  //ViewGPU1.Invalidate;
end;

{$ELSE}
procedure TGLForm1.PickRenderDepth( X, Y: Integer);
{$IFDEF DEPTHPICKER}
var
 c: TVec4;
 sum: single;
 sliceMM : TVec3;
 niftiVol: TNIfTI;
 //u: uint32;
 depth: single;
begin
  if not vols.Layer(0,niftiVol) then exit;
  {$IFNDEF METALAPI}
  glDrawBuffer(GL_BACK);
  {$ENDIF}
  glReadPixels( X, Y, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @depth);

  Vol1.Unproject(X,Y, depth);
  //https://www.khronos.org/opengl/wiki/Compute_eye_space_from_window_space
  //https://community.khronos.org/t/depth-buffer-how-do-i-get-the-pixels-z-coord/35318/3
  //https://stackoverflow.com/questions/30340558/how-to-get-object-coordinates-from-screen-coordinates
  //glm::vec3 result  = glm::unProject(a, viewMatrix, proj, viewport);
  //https://community.khronos.org/t/converting-gl-fragcoord-to-model-space/57397
 (*vec4 v = vec4(2.0*(gl_FragCoord.x-view.x)/view.z-1.0,
              2.0*(gl_FragCoord.y-view.y)/view.w-1.0,
              2.0*texture2DRect(DepthTex,gl_FragCoord.xy).z-1.0,
              1.0 );
v = gl_ModelViewProjectionMatrixInverse * v;
v /= v.w;*)
  Vol1.PaintDepth(niftiVol, gPrefs.DisplayOrient = kAxCorSagOrient4);
  {$IFDEF METALAPI}
  //ViewGPU1.Invalidate;
  //xxx
  //todo
  c := Vec4(0.5, 0.5, 0.5, 0.5);
  u := Vol1.ReadPixel(X,Y);
  c := Vec4(((u shr 16) and $FF) / 255, ((u shr 8) and $FF) / 255, ((u shr 0) and $FF) / 255, ((u shr 24) and $FF) / 255);
  //https://stackoverflow.com/questions/28424883/ios-metal-how-to-read-from-the-depth-buffer-at-a-point
  {$ELSE}
  //https://learnopengl.com/Advanced-OpenGL/Framebuffers
  glReadBuffer(GL_BACK);
  glReadPixels(X, Y, 1, 1, GL_RGBA, GL_FLOAT, @c); //OSX-Darwin   GL_BGRA = $80E1;  GL_UNSIGNED_INT_8_8_8_8_EXT = $8035;
  {$IFDEF COREGL}
  {$IFDEF LCLgtk3}
  glBindFramebuffer(GL_FRAMEBUFFER, 1);
  {$ELSE}
   glBindFramebuffer(GL_FRAMEBUFFER, 0);
  {$ENDIF}
  {$ELSE}
  glBindFramebufferExt(GL_FRAMEBUFFER, 0);
  {$ENDIF}
  {$ENDIF}
  LayerBox.caption := format('%0.3g %0.3g %0.3g', [c.X, c.Y, c.Z]);
  sliceMM := niftiVol.FracMM(Vec3(c.X, c.Y, c.Z));
  sum := c.r + c.g + c.b; //a click outside the rendering will return background color. No good solution...
  if (sum < 0.001) or (sum > 2.99) then begin
     Caption := '';
     exit;
  end;
  Caption := format('%0.4g×%0.4g×%0.4g', [sliceMM.x, sliceMM.y, sliceMM.z]);
  vol1.SetSlice2DFrac(Vec3(c.X, c.Y, c.Z));
  gSliceMM := Vec3(sliceMM.X, sliceMM.Y, sliceMM.Z);
  {$IFDEF COMPILEYOKE}
  SetShareFloats2D(sliceMM.X,sliceMM.Y,sliceMM.Z);
  {$ENDIF}
  if (gPrefs.DisplayOrient = kAxCorSagOrient4) then
  	ViewGPU1.Invalidate
  else
  	Vol1.Paint(niftiVol);
  {$IFDEF METALAPI}
  UpdateTimer.Enabled := true;//OKRA
  {$ENDIF}
  //ViewGPU1.Invalidate;
{$ELSE}
begin
   //
{$ENDIF}
end;
{$ENDIF}
var
	gAxCorSagOrient4MouseDownOrient: integer = 222;

procedure TGLForm1.ViewGPUMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   niftiVol: TNIfTI;
   xIn, yIn, i: integer;
   //{$IFDEF isCocoaOpenGL}
   f: single = 1.0;
   //{$ENDIF}
   Yrev :integer;
   fracXYZ: TVec3;
begin
 xIn := X;
 yIn := Y;
 //PickRenderDepth( X * 2, Y * 2); exit;
 ViewGPU1.SetFocus;
 Vol1.Slices.distanceLineOrient := 0;
 Yrev := ViewGPU1.ClientHeight - Y;
 {$IFDEF isCocoaOpenGL}
 {$IFDEF RETINA}
 f := ViewGPU1.retinaScale;
 {$ENDIF}
 X := round(X * f);
 Y := round(Y * f);
 Yrev := ViewGPU1.ClientHeight - Y;

 //ss := getKeyshiftstate;
 //if  (ssShift in ss) then
 {$ENDIF}
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
 //if gPrefs.DisplayOrient = kRenderOrient then PickRenderDepth(X,Yrev);
 if gPrefs.DisplayOrient > kMax2DOrient then exit;
 if  (Vols.Drawing.ActivePenColor >= 0) and (not AutoROIForm.Visible) and (not (ssCtrl in Shift)) and (not (ssAlt in Shift)) and (not (ssMeta in Shift))  then begin
    EnsureOpenVoi();
    fracXYZ := Vol1.GetSlice2DFrac(X,Y,i);
    gMouseLimitHi := Vol1.GetSlice2DMaxXY(X,Y, gMouseLimitLo);
    if (i > 0) then begin
       if (ssAlt in Shift) then begin
          {$IFDEF isCocoaOpenGL}
          if gIsMouseDown then exit;
          gIsMouseDown := true;
          {$ENDIF}
          Vols.Drawing.voiFloodFill(i, fracXYZ, (ssShift in Shift));
          ViewGPU1.Invalidate;
       end else
           Vols.Drawing.voiMouseDown(i, fracXYZ, (ssShift in Shift));
    end;
    exit;
 end;
 if ssAlt in Shift then Vol1.Slices.distanceLineOrient := -1; // xxx
 if (gPrefs.DisplayOrient = kAxCorSagOrient4) then begin
 	Vol1.SetSlice2DFrac(Vol1.GetSlice2DFrac(X,Y,i)); //
    //LineBox.caption := inttostr(i);
    gAxCorSagOrient4MouseDownOrient := i;
 	if (i < 0) and (gPrefs.RenderDepthPicker) then begin
       if (ssShift in Shift) then exit;
       {$IFDEF METALAPI}
        {TODO
        f := Vol1.DrawableWidth();
        if (f > 0) and (ViewGPU1.width > 0) then
        	f := f / ViewGPU1.width
        else
          f := 1;
        PickRenderDepth( round(X * f), round(Y * f) );}
        {$ELSE}
       	PickRenderDepth(X,Yrev);
        {$ENDIF}
          exit;
        end;
 end;
 ViewGPUMouseMove(Sender, Shift, xIn,Yin);
 if (ssShift in Shift) or  (ssRight in Shift) then
    gMouseDrag := true;
end;

procedure TGLForm1.ViewGPUMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
{$IFDEF isCocoaOpenGL}
var
 f: single = 1.0;
 {$ENDIF}
begin
 {$IFDEF isCocoaOpenGL}
 gIsMouseDown := false;
 {$IFDEF RETINA}
 f := ViewGPU1.retinaScale;
 {$ENDIF}
 X := round(X * f);
 Y := round(Y * f);
 {$ENDIF}
 vol1.SelectionRect.x := -1;
 if Vol1.CE.ColorEditorMouseUp() then
    UpdateTimer.Enabled := true;
 if (Vols.Drawing.IsOpen) and (Vols.Drawing.ActivePenColor >= 0) and (Vols.Drawing.MouseDown) then begin
    Vols.Drawing.voiMouseUp(DrawFilledMenu.Checked, DrawOverwriteMenu.Checked);
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

function limit(val,min,max: TScalar): TScalar;
begin
     if (val < min) then exit(min);
     if (val > max) then exit(max);
     exit(val);
end;

procedure TGLForm1.ViewGPUMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
label
  222;
var
 fracXYZdown, fracXYZ, diff, frac: TVec3;
 i, j: integer;
 niftiVol: TNIfTI;
 {$IFDEF LCLCocoa}{$IFNDEF METALAPI}
 f:single = 1.0;
 {$ENDIF}{$ENDIF}
begin
 {$IFDEF LCLCocoa}{$IFNDEF METALAPI}
 {$IFDEF RETINA} f := ViewGPU1.retinaScale;{$ENDIF}
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
       if (X < gMouseLimitLo.X) then X := gMouseLimitLo.X;
       if (Y < gMouseLimitLo.Y) then Y := gMouseLimitLo.Y;
       if (X > gMouseLimitHi.X) then X := gMouseLimitHi.X;
       if (Y > gMouseLimitHi.Y) then Y := gMouseLimitHi.Y;
       fracXYZ := Vol1.GetSlice2DFrac(X,Y,i);
       //SliceBox.Caption := format('%.2f %.2f %.2f',[fracXYZ.x, fracXYZ.y, fracXYZ.z]);
       if (fracXYZ.x > 1) or (fracXYZ.x < 0) or (fracXYZ.y > 1) or (fracXYZ.y < 0) or (fracXYZ.z > 1) or (fracXYZ.z < 0) then
          exit;
       //fracXYZ := Vol1.GetSlice2DFrac(X,Y,i);
       if (i = Vols.Drawing.voiActiveOrient) then begin
          Vols.Drawing.voiMouseMove(fracXYZ.X, fracXYZ.Y, fracXYZ.Z);
          ViewGPU1.Invalidate;
       end;
       exit;
    end;
    if (ssCtrl in Shift)  then begin //pan image
       fracXYZdown := Vol1.GetSlice2DFrac(gMouse.X,gMouse.Y,j);
       fracXYZ := Vol1.GetSlice2DFrac(X,Y,i);
       if (j <> i) then exit;
       diff :=  (fracXYZdown-fracXYZ);
       //if Vol1.Slices.RadiologicalConvention then
       //   diff.x := -diff.x;
       diff := Vol1.Slices.ZoomCenter - diff;
       diff.x := limit(diff.x, -0.8, 1.8);
       diff.y := limit(diff.y, -0.8, 1.8);
       diff.z := limit(diff.z, -0.8, 1.8);
       Vol1.Slices.ZoomCenter := diff;
       ViewGPU1.Invalidate;
       gMouse.X := X;
       gMouse.Y := Y;
       exit;
    end;
    //Vol1.SetSlice2DFrac(Vol1.GetSlice2DFrac(X,Y,i)); //
    frac := Vol1.GetSlice2DFrac(X,Y,i); //
    if (i < 0) and (i = gAxCorSagOrient4MouseDownOrient) and (gPrefs.DisplayOrient = kAxCorSagOrient4) then goto 222; //okra
    if (i <> gAxCorSagOrient4MouseDownOrient) and (gPrefs.DisplayOrient = kAxCorSagOrient4) then exit;
    //SliceBox.Caption := format('%d:%d %d,%d',[i, gAxCorSagOrient4MouseDownOrient,x ,y]);
    Vol1.SetSlice2DFrac(frac); //

    //if (i < 0) and (gPrefs.DisplayOrient = kAxCorSagOrient4) then goto 222; //okra
    if (Vol1.Slices.distanceLineOrient < 0) then begin
       Vol1.Slices.distanceLineOrient:= i;
       Vol1.Slices.distanceLineStart := Vol1.Slices.SliceFrac;
       Vol1.Slices.distanceLineEnd := Vol1.Slices.SliceFrac;
       //Vol1.Slices.distanceLineStart.position := Vec2(X,Y);
    end else if (Vol1.Slices.distanceLineOrient > 0) then begin
       if Vol1.Slices.distanceLineOrient <> i then
       	  Vol1.Slices.distanceLineOrient := 0;
       Vol1.Slices.distanceLineEnd := Vol1.Slices.SliceFrac;
    end;
    if (Vols.Drawing.IsOpen) then //set crosshair to voxel center
       Vol1.SetSlice2DFrac(niftiVol.FracShiftSlice(vol1.Slices.SliceFrac, pti(0,0,0)));
    //sliceMM := Vol1.Slice2Dmm(niftiVol, vox);
    ReportPositionXYZ(true);
    ViewGPU1.Invalidate;
    exit;
  end;
  if gPrefs.DisplayOrient <> kRenderOrient then exit; //e.g. mosaics
222:
  if (ssCtrl in Shift)  then begin  //adjust pitch
     //
     Vol1.Pitch := Vol1.Pitch - (Y - gMouse.Y);
     Vol1.Pitch := min(Vol1.Pitch, 180);
     Vol1.Pitch := max(Vol1.Pitch, -180);
     gMouse.X := X;
     gMouse.Y := Y;
     ViewGPU1.Invalidate;
     exit;
  end;
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
   i, yi, zi, x,y,z: integer;
   v, Mn, Mx: single;
var
   niftiVol: TNIfTI;
begin

  i := LayerList.ItemIndex;
  //i := 0; //always adjust background layer
  if not vols.Layer(i,niftiVol) then begin
     ViewGPU1.Invalidate;
     exit;
  end;
  if (niftiVol.IsLabels) and (i <> 0) then begin
     i := 0;
     if not vols.Layer(0,niftiVol) then begin
        ViewGPU1.Invalidate;
        exit;
     end;
  end;
  if (niftiVol.IsLabels) then begin
     ViewGPU1.Invalidate;
     exit;
  end;
  ptA := Vol1.GetSlice2DFrac(Xa,Ya,x);
  ptB := Vol1.GetSlice2DFrac(Xb,Yb,y);
  if x <> y then exit; //e.g. dragged across coronal and axial slice
  if (x < kAxialOrient) or (x > kSagLeftOrient) then exit;
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
  LayerChange(i, -1, -1, Mn, Mx); //kNaNsingle
end;

procedure TGLForm1.ViewGPUMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
label
  222;
var
   niftiVol: TNIfTI;
   isUp: boolean;
   ss: TShiftState;
   x,y,orient: integer;
   f: single = 1.0;
 begin
  {$IFDEF RETINA}
  f := ViewGPU1.retinaScale;
  {$ENDIF}
  if gPrefs.DisplayOrient = kMosaicOrient then exit;
  if gPrefs.DisplayOrient <> kRenderOrient then begin
     if not vols.Layer(0,niftiVol) then exit;
     //if niftiVol.VolumesLoaded > 1 then
     //   niftiVol.SetDisplayVolume(niftiVol.VolumeDisplayed + 1)
     //else
     begin
        if WheelDelta = 0 then exit;
        isUp := (WheelDelta > 0);
        ss := getKeyshiftstate;
        if  (ssCtrl in ss) then begin
        //if not (ssCtrl in Shift) then begin
           if isUp then
              SliceZoom.Position:= SliceZoom.Position - 20
           else
               SliceZoom.Position:= SliceZoom.Position + 20;
           exit;
        end;
        orient := gPrefs.DisplayOrient;
        if (orient = kAxCorSagOrient3) or (orient = kAxCorSagOrient4) then begin
           f := 1;
           {$IFDEF LCLCocoa}{$IFNDEF METALAPI} {$IFDEF RETINA}
           f := ViewGPU1.retinaScale;
           {$ENDIF}{$ENDIF}{$ENDIF}
           X := round(MousePos.X * f);
           Y := round(MousePos.Y * f);
           Vol1.GetSlice2DFrac(X,Y,orient);
           //LayerBox.Caption := format('%d %d %d %d %g', [MousePos.X,MousePos.Y, orient, WheelDelta, Vol1.Distance]);
           if (orient < 0) and (gPrefs.DisplayOrient = kAxCorSagOrient4) then goto 222;
        end;
        if orient = kCoronalOrient then begin
             if (isUp) then
                SliceABtn.Click
             else
                 SlicePBtn.Click;
        end else if (orient = kSagRightOrient) or (orient =  kSagLeftOrient) then begin
             if (isUp) then
                SliceRBtn.Click
             else
                 SliceLBtn.Click;
        end else if (orient = kAxialOrient) then begin
            if (isUp) then
               SliceSBtn.Click
            else
                SliceIBtn.Click;
        //end else if (DisplayNextMenu.Enabled) and (gPrefs.DisplayOrient = kAxCorSagOrient ) then begin //scroll wheel in empty space, can we change volume?
        end else if (DisplayNextMenu.Enabled) then begin //scroll wheel in empty space, can we change volume?
           if (isUp) then
              DisplayNextMenu.Click
           else
               DisplayPrevMenu.Click;
        end;
        exit;
     end;
     UpdateTimer.Enabled := true;
     exit;
  end;
 222:
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

{$IFDEF GRAPH}{$IFDEF METALAPI}
var
 gGraphPrepared : boolean = false;
{$ENDIF}{$ENDIF}

procedure TGLForm1.GraphPaint(Sender: TObject);
begin
 {$IFDEF GRAPH}
 {$IFDEF METALAPI}
 if not gGraphPrepared then begin
    ViewGPUgPrepare(Sender);
    gGraphPrepared := true;
 end;

 {$ENDIF}
 gGraph.Paint(ViewGPUg);  //2021
 {$ENDIF}
end;

{$IFDEF GRAPH}
procedure TGLForm1.GraphShow();
begin
 {$IFDEF METALAPI}
 ViewGPUg :=  TMetalControl.Create(GLForm1);
 //ViewGPUg.OnPrepare := @ViewGPUgPrepare; //20200606 not now!
 {$ELSE}
 ViewGPUg :=  TOpenGLControl.Create(GLForm1);
 //ViewGPUg.DoubleBuffered:= false;
 {$IFDEF COREGL}
 ViewGPUg.OpenGLMajorVersion:= 3;
 ViewGPUg.OpenGLMinorVersion:= 3;
 {$ELSE}
 ViewGPUg.OpenGLMajorVersion:= 2;
 ViewGPUg.OpenGLMinorVersion:= 1;
 {$ENDIF}
 ViewGPUg.MultiSampling:=4;
 {$ENDIF}
 //2022
 ViewGPUg.Parent := GraphPanel;
 ViewGPUg.Align:= alClient;
 ViewGPUg.Constraints.MinHeight:=4;
 ViewGPUg.Constraints.MinWidth:=4;
 //2025
 //ViewGPUg.OnPaint := @GraphPaint;
 ViewGPUg.OnMouseDown := GraphMouseDown;
 ViewGPUg.OnMouseWheel:= GraphMouseWheel;
 ViewGPUg.OnKeyDown := ViewGPUgKeyDown;
 //ViewGPUg.OnResize:=@ViewGPUgResize;
 {$IFDEF METALAPI}
 ViewGPUg.renderView.setSampleCount(4);
 {$ELSE}
 //ViewGPUg.OnPaint := @GraphPaint;
 //ViewGPUg.OnMouseDown := @GraphMouseDown;
 //ViewGPUg.OnMouseWheel:= @GraphMouseWheel;
 //ViewGPUg.OnKeyDown := @ViewGPUgKeyDown;
 ViewGPUg.OnResize:= ViewGPUgResize;
 ViewGPUg.MakeCurrent(false);
 {$IFDEF LCLCocoa}
 ViewGPUg.setRetina(true);
 {$ENDIF}
 {$IFDEF COREGL}
 if (not  Load_GL_version_3_3_CORE) then begin
    printf('Unable to load OpenGL 3.3 Core');
    showmessage('Unable to load OpenGL 3.3 Core');
    halt;
 end;
 {$ELSE}
 if (not  Load_GL_version_2_1) then begin
    printf('Unable to load OpenGL 2.1');
    showmessage('Unable to load OpenGL 2.1');
    halt;
 end;
 (*  //extensions used by rendering, not graphing
 glext_LoadExtension('GL_version_1_2'); //https://bugs.freepascal.org/view.php?id=37368
 if not glext_LoadExtension('GL_EXT_framebuffer_object') then begin
    printf('This software does not support GL_EXT_framebuffer_object used for GPU-based Gradient calculation');
 end; *)
 {$ENDIF}
 ViewGPUg.ReleaseContext;
 if GLErrorStr <> '' then begin
    printf('Error initializing OpenGL');
    {$IFDEF UNIX}
    halt;
    {$ELSE}
    showmessage(GLErrorStr);
    {$ENDIF}
    GLErrorStr := '';
 end;
 {$ENDIF}
 gGraph := TGPUGraph.Create(ViewGPUg);
  ViewGPUg.OnPaint := GraphPaint;
 GraphClearMenuClick(nil);

end;
{$ELSE}
procedure TGLForm1.GraphShow();
begin
  GraphMenu.visible := false;
  BottomPanel.visible := false;
  TBSplitter.Visible := false;
end;
{$ENDIF}

procedure TGLForm1.FormCreate(Sender: TObject);
begin
 {$IFDEF METALAPI}
 ViewGPU1 :=  TMetalControl.Create(CenterPanel);
 Vol1 := TGPUVolume.Create(ViewGPU1);
 {$ELSE}
 ViewGPU1 :=  TOpenGLControl.Create(GLForm1);
 {$IFDEF COREGL}
 ViewGPU1.OpenGLMajorVersion:= 3;
 ViewGPU1.OpenGLMinorVersion:= 3;
 {$ELSE}
 ViewGPU1.OpenGLMajorVersion:= 2;
 ViewGPU1.OpenGLMinorVersion:= 1;
 {$ENDIF}
 {$IFDEF LINE3D}
 ViewGPU1.DepthBits:= 24;
 {$ELSE}
 ViewGPU1.DepthBits:= 0;
 {$ENDIF}
 Vol1 := TGPUVolume.Create(ViewGPU1);
 {$ENDIF}
end;

function TGLForm1.DefaultImage():string;
var
 s: string;
begin
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
 result := s;
end;

procedure TGLForm1.FormShow(Sender: TObject);
//const
//  kImgFilter = 'Volumes|*.nii;*.nii.gz;*.HEAD;*.hdr;*.nrrd;*.nhdr;*.mgh;*.mgz;*.mhd;*.mha|NIfTI|*.nii|Compressed NIfTI|*.nii.gz|All|*.*';
var
 {$IFNDEF METALAPI}
 glMaxTex: glint;
 glMaxTexMb: integer;
 glMaxTexf: double;
 {$ENDIF}
 i, MaxTexMb: integer;
 c: char;
 isForceReset, isOK: boolean;
 psnSkip: boolean = false;
 s, shaderPath, shaderName: string;
 shaderNames : TStringList;
 newMenu: TMenuItem;
begin
 {$IFDEF LCLGTK2}{$IFDEF LINUX}
 writeln('If there is a long delay at launch, ensure full GTK2 install: "sudo apt-get install appmenu-gtk2-module"');
 {$ENDIF}{$ENDIF}
 {$IFDEF FPC} Application.ShowButtonGlyphs:= sbgNever; {$ENDIF}
 //OpenDialog1.Filter := kImgFilter;
 isForceReset := false;
 gPrefs.InitScript := '';
 MaxTexMb := -1;
 //gPrefs.InitScript := '/Users/rorden/MRIcroGL12/MRIcroGL.app/Contents/Resources/script/basic.py';
 i := 1;
 //{$IFDEF UNIX}writeln('>>>Setting MaxVox to '+inttostr(MaxVox));{$ENDIF}
 while i <= ParamCount do begin
    s := ParamStr(i);
    if (length(s)> 1) and (s[1]='-') then begin
        c := upcase(s[2]);
        if c='V' then
           printf(versionStr)
        else if c='R' then
           isForceReset := true
        {$IFDEF Darwin}
        else if c='P' then
             psnSkip := true //-psn: MacOS process serial number
        {$ENDIF}
        else if (i < paramcount) and (c='M') then begin
          inc(i);
          MaxTexMb := strtointdef(ParamStr(i), -1);
           {$IFDEF UNIX}writeln('Setting MaxTexMb to '+inttostr(MaxTexMb));{$ENDIF}
        end else if (i < paramcount) and (c='S') then begin
          inc(i);
          //if (upcase(ExtractFileExt(ParamStr(i))) = '.PY') or (upcase(ExtractFileExt(ParamStr(i))) = '.TXT') then
             gPrefs.InitScript := ParamStr(i);
        end;
    end;
    inc(i);
  end; //for each parameter
  //check - on darwin form drop file
  if (not psnSkip) and (gPrefs.InitScript = '') and (MaxTexMb < 1) and (ParamCount >= 1) and (not isForceReset) then //and (fileexists(ParamStr(ParamCount))) then
     gPrefs.InitScript := '-';
  {$IFNDEF MYPY}
  gPrefs.InitScript := '';
  {$ENDIF}
  if (length(gPrefs.InitScript) > 0) and (gPrefs.InitScript <> '-') and (not fileexists(gPrefs.InitScript)) then begin
     if (upcase(ExtractFileExt(ParamStr(i))) = '.PY') or (upcase(ExtractFileExt(ParamStr(i))) = '.TXT') then begin
        {$IFDEF UNIX}writeln('Unable to find script '+gPrefs.InitScript);{$ENDIF}
        gPrefs.InitScript := '';
     end;
  end;
  //set defaults
  if isForceReset then
     SetDefaultPrefs(gPrefs, true)
  else begin
      if (gPrefs.InitScript = '') and (fileexists(ScriptDir+pathdelim+'startup.py')) then
         gPrefs.InitScript := ScriptDir+pathdelim+'startup.py';
      IniFile(true,  gPrefs);
      gPrefs.DisplayOrient := gPrefs.StartupDisplayOrient;
      dcm2niiForm.setCustomDcm2niix(gPrefs.CustomDcm2niix);
      if (gPrefs.DisplayOrient = kMosaicOrient) then
         gPrefs.DisplayOrient := kAxCorSagOrient3;//kRenderOrient;
  end;
  {$ifdef windows}
  if gPrefs.DebugMode then begin
   AllocConsole;      // in Windows unit
   IsConsole := True; // in System unit
   SysInitStdIO;      // in System unit
  end;
  {$endif}
  if MaxTexMb > 0 then
     gPrefs.MaxTexMb := MaxTexMb;
  AnimateTimer.Interval:= gPrefs.AnimationIntervalMsec;
  if gPrefs.StartupWindowMode = 1 then begin
     GLForm1.BoundsRect := Screen.MonitorFromWindow(Handle).BoundsRect;
     GLForm1.WindowState:= wsMaximized;
  end;
  if gPrefs.StartupWindowMode = 2 then begin
     GLForm1.WindowState:= wsFullScreen;
     {$IFNDEF LCLCocoa}ExitFullScreenMenu.Visible:=true;{$ENDIF} //Linux has issues getting out of full screen
  end;

  if DirectoryExists(gPrefs.DicomDir) then
  	DicomDirMenu.Visible:= true;
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
  CreateStandardMenus(OpenStandardMenu);
  CreateStandardMenus(OpenAltasMenu);
  if not DirectoryExists(GetFSLdir+pathdelim+ 'data'+pathdelim+'standard') then
     OpenFSLMenu.Visible := false;
  //if DirectoryExists(GetFSLdir+pathdelim+ 'data'+pathdelim+'standard') then
  //   OpenFSLMenu.Visible := true;
  CreateStandardMenus(OpenAFNIMenu);
  s := DefaultImage();
  if (length(gPrefs.InitScript) > 0) then
     s := '+'; //load borg for quick load
  {$IFNDEF MYPY}
  if ParamCount > 0 then begin
     s := ParamStr(1);
     gPrefs.InitScript := '';
  end;
  {$ENDIF}
  //TODO: house keeping with new volumes: autoload cluster AutoClusterizeAtlases
  {$IFDEF METALAPI}
  //must be done at FormCreate, not FormShow!: ViewGPU1 :=  TMetalControl.Create(CenterPanel);
  //ViewGPU1.OnPrepare := @ViewGPUPrepare;
  //EditMenu.Visible := true; //to do: copy bitmap
  //Smooth2DCheck.Visible := false; //to do: Metal nearest neighbor must change shader
  //To do: variant of _Texture2D.metal e.g. _Texture2DFlat.metal with nearest neighbor :
  // constexpr sampler texSampL (mag_filter::linear,min_filter::linear);
  // constexpr sampler texSampN (mag_filter::nearest,min_filter::nearest);

  {$ELSE}
  //ViewGPU1 :=  TOpenGLControl.Create(CenterPanel);
  //x ViewGPU1 :=  TOpenGLControl.Create(GLForm1);
  //x ViewGPU1.OpenGLMajorVersion := 3;
  //x ViewGPU1.OpenGLMinorVersion := 3;
  //x ViewGPU1.DepthBits:= 0;
  //Multisampling influences borders of vertex shader - orientation cube looks MUCH better with multisampling
  // Has no influence on fragment shader away from edges, e.g. no benefit for volume rendering but no cost?
  ViewGPU1.MultiSampling := 4;
  if gPrefs.MultiSample124 = 2 then
     ViewGPU1.MultiSampling := 2
  else if gPrefs.MultiSample124 = 1 then begin
    printf('Multisampling disabled: orientation cube might look jagged. Performance benefit unlikely.');
    ViewGPU1.MultiSampling := 1;
  end;
  //x Vol1 := TGPUVolume.Create(ViewGPU1);
  {$ENDIF}
  //ViewGPU1.Parent := GLForm1;
  ViewGPU1.Parent := CenterPanel;
  {$IFDEF METALAPI}ViewGPU1.renderView.setSampleCount(4);{$ENDIF}
  ViewGPU1.Align:= alClient;
  //for Metal, Must be done at FormCreate, not FormShow! Vol1 := TGPUVolume.Create(ViewGPU1);
  //Vol1.Slices.RadiologicalConvention := gPrefs.FlipLR_Radiological;
  {$IFNDEF METALAPI}
  {$IFDEF LCLCocoa}{$IFDEF RETINA}
  ViewGPU1.setRetina(gPrefs.RetinaDisplay);
  {$ENDIF}{$ENDIF}
  ViewGPU1.MakeCurrent(false);
  {$IFDEF COREGL}
  if (not  Load_GL_version_3_3_CORE) then begin
     showmessage('Unable to load OpenGL 3.3 Core');
     halt;
  end;
  {$ELSE}
  if (not  Load_GL_version_2_1) then begin
     printf('Unable to load OpenGL 2.1');
     showmessage('Unable to load OpenGL 2.1');
     halt;
  end;
  glext_LoadExtension('GL_version_1_2'); //https://bugs.freepascal.org/view.php?id=37368
  if not glext_LoadExtension('GL_EXT_framebuffer_object') then begin
     printf('OpenGL Driver does not suppout GL_EXT_framebuffer_object. Gradient Calculations will be slow');
     gPrefs.GradientMode := kGradientModeCPUSlowest;
  end;
  {$ENDIF}//if coregl
  glGetIntegerv(GL_MAX_3D_TEXTURE_SIZE, @glMaxTex);  //For 3D textures, no dimension can be greater than GL_MAX_3D_TEXTURE_SIZ
  glMaxTex := max(glMaxTex,64);
  glMaxTexf := glMaxTex;
  glMaxTexMb := ceil((glMaxTexf * glMaxTexf * glMaxTexf * 4.0)/1048576.0);
  if glMaxTexMb < gPrefs.MaxTexMb then begin
     gPrefs.MaxTexMb := floor((glMaxTexf * glMaxTexf * glMaxTexf * 4)/1048576);
     printf('Warning GL_MAX_3D_TEXTURE_SIZE is '+inttostr(glMaxTex)+' setting MaxTexMb to '+inttostr(gPrefs.MaxTexMb));
  end;
  //GLForm1.caption := glGetString(GL_VENDOR)+'; OpenGL= '+glGetString(GL_VERSION)+'; Shader='+glGetString(GL_SHADING_LANGUAGE_VERSION);
  ViewGPU1.ReleaseContext;
  Vol1.SetGradientMode(gPrefs.GradientMode);
  Vol1.Prepare(shaderName);
  if gPrefs.FlipYZ then
     Vol1.Pitch := -90;
  setShaderSliders;
  {$ENDIF} //if OpenGL
  vols := TNIfTIs.Create(s,  gPrefs.ClearColor, gPrefs.LoadFewVolumes, gPrefs.MaxTexMb, isOK); //to do: warning regarding multi-volume files?
  GraphShow();
  ViewGPU1.OnKeyPress:=ViewGPUKeyPress;
  ViewGPU1.OnKeyDown := ViewGPUKeyDown;
  ViewGPU1.OnKeyUp := ViewGPUKeyUp;
  ViewGPU1.OnDblClick :=  ViewGPUDblClick;
  ViewGPU1.OnMouseDown := ViewGPUMouseDown;
  ViewGPU1.OnMouseMove := ViewGPUMouseMove;
  ViewGPU1.OnMouseUp := ViewGPUMouseUp;
  ViewGPU1.OnMouseWheel := ViewGPUMouseWheel;
  ViewGPU1.OnPaint := ViewGPUPaint;
  {$IFNDEF MOSAIC}
  MosaicMenu.Visible := false;
  gPrefs.DisplayOrient := kRenderOrient;
  {$ENDIF}
  {$IFDEF MATCAP}
  UpdateMatCapDrop(MatCapDrop);
  if (MatCapDrop.Items.Count > 0) then begin
     MatCapDrop.ItemIndex := 0;
     Vol1.SetMatCap(MatCapDrop.Items[MatCapDrop.ItemIndex]);
  end;
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
           newMenu.OnClick := ScriptingTemplatesMenuClick;
           ScriptingTemplatesMenu.Add(newMenu);
       end;
    end;
  finally
    shaderNames.Free;
  end;
  {$IFDEF MATT1}
  //StoreFMRIMenu.Visible := true;
  {$ENDIF}
  {$IFNDEF MYPY}
  ScriptingMenu.Visible:= false;
  ScriptOutputMemo.Lines.Add('Not compiled for scripting');
  {$ENDIF}
  {$IFDEF Darwin}
  //LayerList.Style := lbOwnerDrawFixed;//Dark mode bug https://bugs.freepascal.org/view.php?id=34600
  {$IFDEF DARKMODE}
  SetDarkMode();
  {$ENDIF}
  HelpPrefMenu.Visible:= false;
  HelpAboutMenu.Visible := false; //use apple menu
  FileSepMenu.Visible := false;
  FileExitMenu.Visible := false;
  NewWindowMenu.ShortCut := ShortCut(Word('N'), [ssShift, ssModifier]); //ssCtrl -> ssMeta
  ScriptingRunMenu.ShortCut := ShortCut(Word('R'), [ssModifier]); //ssCtrl -> ssMeta
  OpenMenu.ShortCut := ShortCut(Word('O'), [ssModifier]); //ssCtrl -> ssMeta
  AddOverlayMenu.ShortCut := ShortCut(Word('A'), [ssModifier]); //ssCtrl -> ssMeta
  //SaveNIfTIMenu.ShortCut := ShortCut(Word('S'), [ssModifier]);
  DrawSaveMenu.ShortCut := ShortCut(Word('S'), [ssModifier]); //ssCtrl -> ssMeta used to be meta-H but used by Apple to Hide program
  DrawHideMenu.ShortCut := ShortCut(Word('T'), [ssModifier]); //ssCtrl -> ssMeta used to be meta-H but used by Apple to Hide program
  ScriptingNewMenu.ShortCut  := ShortCut(Word('N'), [ssModifier]); //ssCtrl -> ssMeta
  DrawUndoMenu.ShortCut  := ShortCut(Word('U'), [ssModifier]);
  DrawCloneMenu.ShortCut  := ShortCut(Word('Z'), [ssModifier]);
  EditCopyMenu.ShortCut  := ShortCut(Word('C'), [ssModifier]);
  EditPasteMenu.ShortCut  := ShortCut(Word('V'), [ssModifier]);
  DrawNoneMenu.ShortCut  := ShortCut(Word('D'), [ssModifier]);
  DrawEraseMenu.ShortCut  := ShortCut(Word('E'), [ssModifier]);
  DrawRedMenu.ShortCut  := ShortCut(Word('1'), [ssModifier]);
  DrawGreenMenu.ShortCut  := ShortCut(Word('2'), [ssModifier]);
  DrawBlueMenu.ShortCut  := ShortCut(Word('3'), [ssModifier]);
  YokeMenu.ShortCut:= ShortCut(Word('Y'), [ssModifier]);
  //MakePullDownButton(LayerOptionsBtn, LayerPopup, false, 0, 2, 0, 0);
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
  {$IFDEF METALAPI}
  if gPrefs.Quality1to5 = 5 then
     QualityTrack.Position:= 4; //???METAL???
  {$ENDIF}
          QualityTrack.Position:= gPrefs.Quality1to5;
  {$IFNDEF METALAPI}
  if gPrefs.InitScript <> '' then
     UpdateTimer.Enabled:=true;
  ViewGPUPrepare(Sender);
  {$ENDIF}
  if gPrefs.InitScript = '' then begin //update graph and caption
    XCoordEdit.Text := '0';
    YCoordEdit.Text := '0';
    ZCoordEdit.Text := '0';
    SetXHairPosition(0,0,0 );
  end;
  SetToolPanelMaxWidth();
  {$IFDEF METALAPI}
  //isFormShown := true;
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
  //QualityTrack.Position:= gPrefs.Quality1to5;
  {$IFDEF CLRBAR}
  gClrbar:= TGPUClrbar.Create(ViewGPU1);
  Vol1.SetColorBar(gClrBar);
  gClrbar.RulerColor := SetRGBA(Vol1.Slices.LineColor);
  RulerVisible();
  gClrbar.isVisible := gPrefs.ColorbarVisible;
  VisibleClrbarMenu.checked := gPrefs.ColorbarVisible;
  {$ENDIF}
  Smooth2DCheck.Checked := gPrefs.Smooth2D;
  RulerCheck.checked := gPrefs.RulerVisible;
  TextAndCubeMenu.Checked := gPrefs.LabelOrient;
  SetColorBarPosition;
  {$IFDEF CLRBAR}
  gClrbar.SizeFraction := gPrefs.ColorbarSize/1000;
  {$ENDIF}
  UpdateColorbar();
  Vol1.SetTextContrast(gPrefs.ClearColor);
  Vol1.Slices.RadiologicalConvention := gPrefs.FlipLR_Radiological;
  Vol1.Slices.LabelOrient := gPrefs.LabelOrient;
  Vol1.Quality1to5 := gPrefs.Quality1to5;
  Vol1.Slices.LineWidth := gPrefs.LineWidth;
  Vol1.Slices.LineColor := Vec4(gPrefs.LineColor.R/255.0, gPrefs.LineColor.G/255.0, gPrefs.LineColor.B/255.0, gPrefs.LineColor.A/255.0);
  LineWidthEdit.Value := gPrefs.LineWidth;
end;

procedure TGLForm1.InvalidateTImerTimer(Sender: TObject);
begin
    if isBusyCore then exit;
    InvalidateTimer.enabled := false;
    ViewGPUPaint(sender);
end;

procedure TGLForm1.LayerAfniBtnClick(Sender: TObject);
begin
     AfniPopup.PopUp;
end;

procedure TGLForm1.LayerAfniDropClick(Sender: TObject);
var
   v: TNIfTI;
   i: integer;
begin
 i := LayerAfniDrop.ItemIndex;
 vols.Layer(LayerList.ItemIndex,v);
 if v.VolumeDisplayed = i then exit;
 v.SetDisplayVolume(i);
 UpdateLayerBox(false, true);// e.g. "fMRI (1/60)" -> "fMRI (2/60"
 UpdateTimer.Enabled := true;
end;

function TGLForm1.HasLabelLayer: boolean;
var
   v: TNIfTI;
   i: integer;
begin
  v := nil;
  i := 0;
  while (i < GLForm1.LayerList.Count) do begin
        if not vols.Layer(i,v) then exit(false);
        if v.IsLabels then
           break;
        i := i + 1;
  end;
  result := (v <> nil) and (v.IsLabels);
end;


procedure TGLForm1.LayerClusterMenuClick(Sender: TObject);
var
   v: TNIfTI;
   smallestClusterMM3,thresh: double;
   NeighborMethod: integer;
   isDarkAndBright: boolean;
begin
  if not vols.Layer(LayerList.ItemIndex,v) then exit;
  if ((v.VolumesLoaded > 2) and (length(v.afnis) < 1)) then begin
     showmessage('Clusters are for 3D data.');
     exit;
  end;
  NeighborMethod := gPrefs.ClusterNeighborMethod;
  smallestClusterMM3 := 32;
  isDarkAndBright := false;
  if (v.DisplayMax < 0) and (v.DisplayMin < 0) then begin
     thresh := max(v.DisplayMin, v.DisplayMax);
     if thresh = (v.VolumeMax) then
        thresh := (0.5*(v.DisplayMax-v.DisplayMin))+v.DisplayMin;
  end else begin
      thresh := min(v.DisplayMin, v.DisplayMax);
      if thresh = (v.VolumeMin) then
         thresh := (0.5*(v.DisplayMax-v.DisplayMin))+v.DisplayMin;
  end;
  //var thresh: double; out mm: double; out NeighborMethod: integer
  if ((Sender as TMenuItem).tag = 1) and (v.IsLabels) then
     showmessage('No cluster options for label maps (atlases)')
  else if (Sender as TMenuItem).tag = 1 then begin
     GetThresh(thresh, smallestClusterMM3, NeighborMethod, isDarkAndBright);
  end;
  if thresh = kNaN then begin
     //showmessage('Skipping clusters');
     exit;
  end;
  if ((Sender as TMenuItem).tag = 2)  then begin
     if (v.IsLabels) then begin
        showmessage('Make sure a non-atlas image is selected in the "Layer" panel.');
        exit;
     end;
      if not HasLabelLayer() then begin
         showmessage('Load an atlas before running this function.');
         exit;
      end;
     generateClustersCore(v, kNaNsingle, kNaNsingle, NeighborMethod, isDarkAndBright)
  end else
      generateClustersCore(v, thresh, smallestClusterMM3, NeighborMethod, isDarkAndBright);
  UpdateLayerBox(false);
  if gPrefs.DisplayOrient > kAxCorSagOrient3 then
     MPRMenu.Click;
  GLForm1.UpdateLayerBox(false, true);
  if (length(v.clusters) < 1) then
     showmessage('No clusters survive');
end;

procedure TGLForm1.BetterRenderTimerTimer(Sender: TObject);
begin
     if isBusyCore then exit;
     BetterRenderTimer.Enabled := false;
     if (gPrefs.DisplayOrient <> kRenderOrient) then exit;
     BetterRenderTimer.Tag := 1;
     ViewGPU1.Invalidate;
     //ViewGPUPaint(sender); //<- this causes the graph to get repainted with the rendering!
end;


procedure TGLForm1.ViewGPUgPrepare(Sender: TObject);
begin
  {$IFDEF GRAPH}
  {$IFDEF METALAPI}
  //ViewGPUg.Invalidate;  //2021
  {$IFDEF DBUG} DebugLn('>>Prepare');{$ENDIF}
  ViewGPUg.SetPreferredFrameRate(0);
  //2022
  ViewGPUg.InvalidateOnResize := true; //20200606 : no longer works

 (*ViewGPUg.OnPaint := @GraphPaint;

 ViewGPUg.OnMouseDown := @GraphMouseDown;
 ViewGPUg.OnMouseWheel:= @GraphMouseWheel;
 ViewGPUg.OnKeyDown := @ViewGPUgKeyDown;
 ViewGPUg.OnResize:=@ViewGPUgResize;
 ViewGPUg.Invalidate;*)
  {$ENDIF}
  {$ENDIF}
end;

procedure TGLForm1.ViewGPUPaint(Sender: TObject);
var
   niftiVol: TNIfTI;
   q: integer;
begin

 {$IFDEF METALAPI}
 //if not isFormShown then exit;
 {$ENDIF}
  if isBusy then exit; //invalidateTimer
  if isBusyCore then begin
     InvalidateTimer.Enabled := true;
    exit;
  end;
  if not vols.Layer(0,niftiVol) then exit;
  isBusyCore := true;
  {$IFDEF METALAPI}
 if not isPrepared then begin
    ViewGPUPrepare(Sender);
    {$IFDEF GRAPH}
    //ViewGPUgPrepare(Sender);
    //ViewGPUg.invalidate;
    //gGraph.Paint(ViewGPUg);    //2021
    //ViewGPUg.InvalidateOnResize := true; //20200606 : no longer works
    {$ENDIF}
 end;
 MTLSetClearColor(MTLClearColorMake(gPrefs.ClearColor.r/255, gPrefs.ClearColor.g/255, gPrefs.ClearColor.b/255, gPrefs.ClearColor.A/255));
 //gPrefs.DisplayOrient := kRenderOrient;
 //Vol1.Paint2D(niftiVol, vols.Drawing, gPrefs.DisplayOrient);
 {$ELSE}
 ViewGPU1.MakeCurrent(false);
 glClearColor(gPrefs.ClearColor.R/255, gPrefs.ClearColor.G/255, gPrefs.ClearColor.B/255, gPrefs.ClearColor.A/255);
 //glClearColor(random(256)/255, gPrefs.ClearColor.G/255, gPrefs.ClearColor.B/255, gPrefs.ClearColor.A/255);
 {$ENDIF}
 {$IFDEF MOSAIC}
  if gPrefs.DisplayOrient = kMosaicOrient then
       Vol1.PaintMosaic2D(niftiVol, vols.Drawing, gPrefs.MosaicStr)
  else {$ENDIF} if (gPrefs.DisplayOrient = kRenderOrient) and (BetterRenderTimer.Tag <> 0) then begin //render view
      q := Vol1.Quality1to5;
      Vol1.Quality1to5 := 5;
      Vol1.Paint(niftiVol);
      //GraphPaint(nil);
      BetterRenderTimer.Tag := 0;
      Vol1.Quality1to5 := q;
 end else if gPrefs.DisplayOrient = kRenderOrient then begin //render view
    Vol1.Paint(niftiVol);  //OKRA
    if (Vol1.Quality1to5 = 0) and (not gPyRunning) then
       BetterRenderTimer.enabled := true;
 end else
     Vol1.Paint2D(niftiVol, vols.Drawing, gPrefs.DisplayOrient);
 isBusyCore := false;
 {$IFDEF METALAPI}  {$IFDEF GRAPH}
 //if (gGraph.isRedraw) and (gGraph.numLines > 0) then
 (*if forceGraph then begin
    gGraph.isRedraw := true;
    forceGraph := false;
    ViewGPUg.invalidate;  //2021
 end;*)
 {$ENDIF} {$ENDIF}

end;

procedure TGLForm1.FormDestroy(Sender: TObject);
begin
 {$IFDEF COMPILEYOKE}
   YokeTimer.Enabled := false;
   CloseSharedMem;
  {$ENDIF}
  AnimateTimer.Enabled := false;
  UpdateTimer.Enabled := false;
  {$IFDEF GRAPH}
  gGraph.Destroy;
  {$ENDIF}
  vols.CloseAllLayers;
  vols.Free;
  gLandmark.Free;
end;

end.

