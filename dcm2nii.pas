unit dcm2nii;

{$mode objfpc}{$H+}
{$IFDEF Darwin}
  {$modeswitch objectivec1}
{$ENDIF}
{$DEFINE isGL}

interface

uses
  {$IFDEF Darwin} CocoaAll, {$ENDIF}
  lclintf, strutils, Process, Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, IniFiles, ComCtrls; //'Types' not used by Darwin

type

  { Tdcm2niiForm }

  Tdcm2niiForm = class(TForm)
    BidsDrop: TComboBox;
    MergeDrop: TComboBox;
    BidsLabel: TLabel;
    VerboseCheck: TCheckBox;
    UpdateBtn: TButton;
    CropCheck: TCheckBox;
    FormatDrop: TComboBox;
    FormatLabel: TLabel;
    GeneralGroup: TGroupBox;
    AdvancedGroup: TGroupBox;
    IgnoreCheck: TCheckBox;
    LosslessScaleCheck: TCheckBox;
    OutDirDrop: TComboBox;
    OutDirLabel: TLabel;
    OutNameEdit: TEdit;
    OutNameExampleLabel: TLabel;
    OutNameLabel: TLabel;
    OutputDirDialog: TSelectDirectoryDialog;
    InputDirDialog: TSelectDirectoryDialog;
    PhilipsPreciseCheck: TCheckBox;
    SelectFilesBtn: TButton;
    ResetBtn: TButton;
    OutputMemo: TMemo;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ConvertDicomDir(DirName: string);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure OutDirDropChange(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
    procedure SelectFilesBtnClick(Sender: TObject);
    procedure ShowPrefs;
    procedure ReadPrefs;
    function RunCmd (lCmd: string; isDemo: boolean): string;
    procedure UpdateBtnClick(Sender: TObject);
    procedure UpdateCommand(Sender: TObject);
    function TerminalCommand: string;
    function getCurrentDcm2niix(): string;
    function getCustomDcm2niix(): string;
    procedure setCustomDcm2niix(fnm: string);
    //procedure findCustomDcm2niix();
    procedure UpdateDialog(versionMsg: string);

  private

  public

  end;

var
  dcm2niiForm: Tdcm2niiForm;

implementation

{$ifdef LCLCocoa}{$IFDEF isGL}
uses mainunit; //darkmode
{$ENDIF}{$ENDIF}

{$R *.lfm}
const kExeName = 'dcm2niix';
  {$IFDEF Unix}
  kDemoInputDir = ' "/home/user/DicomDir"';
  {$ELSE}
  kDemoInputDir = ' "c:\DicomDir"';
  {$ENDIF}
Type
TPrefs = record
  UseOutDir, Ignore, LosslessScale,PhilipsPrecise, Crop, Verbose: boolean;
  SeriesMerge,Bids,Format: integer;
  OutDir,OutName: String;
end;
var
  gPrefs: TPrefs;
  gCustomDcm2niix : string = '';

(*procedure Tdcm2niiForm.findCustomDcm2niix();
const
     kStr = 'Find the dcm2niix executable (latest at https://github.com/rordenlab/dcm2niix/releases).';
var
  openDialog : TOpenDialog;
begin
     if fileexists(gCustomDcm2niix) then
        showmessage(kStr + ' Currently using "'+gCustomDcm2niix+'".')
     else
         showmessage(kStr);
     openDialog := TOpenDialog.Create(self);
     openDialog.InitialDir := GetCurrentDir;
     openDialog.Options := [ofFileMustExist];
     if openDialog.Execute then
        gCustomDcm2niix := openDialog.FileName;
     openDialog.Free;
end;*)

{$IFDEF Darwin}
function ResourceDir (): string;
begin
	result := NSBundle.mainBundle.resourcePath.UTF8String;
end;
{$ELSE}
function ResourceDir (): string;
begin
     result := extractfilepath(paramstr(0))+'Resources';
end;
{$ENDIF}

function getDefaultDcm2niix2(): string;
begin
  {$IFDEF UNIX}
  result := ResourceDir + pathdelim + kExeName;
  if fileexists(result) then exit;
  result := '/usr/bin/' + kExeName;
  if fileexists(result) then exit;
  result := '/usr/local/bin/' + kExeName;
  {$ELSE}
  result := ResourceDir + pathdelim + kExeName+'.exe';
  {$ENDIF}
end;


function Tdcm2niiForm.getCurrentDcm2niix(): string;
begin
  result := gCustomDcm2niix;
  if (result  = '') or (not fileexists(result)) then
     result := getDefaultDcm2niix2();
end;

function Tdcm2niiForm.getCustomDcm2niix(): string;
begin
  //if (gCustomDcm2niix  = '') or (not fileexists(gCustomDcm2niix)) then
  //   gCustomDcm2niix := getDefaultDcm2niix();
  result := gCustomDcm2niix;
end;

procedure Tdcm2niiForm.setCustomDcm2niix(fnm: string);
begin
   gCustomDcm2niix := fnm;
end;

function SetDefaultPrefs(): TPrefs;
begin
     with result do begin
          Ignore := false;
          LosslessScale := false;
          SeriesMerge := 0;
          PhilipsPrecise := true;
          Crop := false;
          Verbose := false;
          UseOutDir := false;
          Bids := 1;
          Format := 1;
          OutDir := GetUserDir;
          OutName := '%f_%p_%t_%s';
     end;
end;

procedure Tdcm2niiForm.ShowPrefs;
begin
     with gPrefs do begin
          IgnoreCheck.Checked := Ignore;
          LosslessScaleCheck.Checked := LosslessScale;
          MergeDrop.ItemIndex := SeriesMerge;
          PhilipsPreciseCheck.Checked := PhilipsPrecise;
          CropCheck.Checked := Crop;
          VerboseCheck.Checked := Verbose;
          if (UseOutDir) then
             OutDirDrop.ItemIndex := 2
          else
              OutDirDrop.ItemIndex:= 0;
          OutDirDrop.Items[2] := OutDir;
          FormatDrop.ItemIndex := Format;
          OutNameEdit.Text:= OutName;
          BidsDrop.ItemIndex := Bids;
     end;
end;

procedure Tdcm2niiForm.ReadPrefs;
begin
     with gPrefs do begin
          Ignore := IgnoreCheck.Checked;
          LosslessScale := LosslessScaleCheck.Checked;
          SeriesMerge := MergeDrop.ItemIndex;
          PhilipsPrecise := PhilipsPreciseCheck.Checked;
          Crop := CropCheck.Checked;
          Verbose := VerboseCheck.Checked;
          UseOutDir := (OutDirDrop.ItemIndex = 2);
          OutDir := OutDirDrop.Items[2];
          Format := FormatDrop.ItemIndex;
          OutName := OutNameEdit.Text;
          Bids := BidsDrop.ItemIndex;
     end;
end;

function IniName: string;
begin
  result := GetUserDir;
  if result = '' then exit;
  result := result + '.'+ChangeFileExt(ExtractFileName(ParamStr(0)),'')+'_dcm.ini';
end;

procedure IniInt(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: integer);
//read or write an integer value to the initialization file
var
	lStr: string;
begin
        if not lRead then begin
           lIniFile.WriteString('INT',lIdent,IntToStr(lValue));
           exit;
        end;
	lStr := lIniFile.ReadString('INT',lIdent, '');
	if length(lStr) > 0 then
		lValue := StrToInt(lStr);
end; //IniInt

function Bool2Char (lBool: boolean): char;
begin
	if lBool then
		result := '1'
	else
		result := '0';
end;

function Char2Bool (lChar: char): boolean;
begin
	if lChar = '1' then
		result := true
	else
		result := false;
end;

procedure IniBool(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: boolean);
//read or write a boolean value to the initialization file
var
	lStr: string;
begin
        if not lRead then begin
           lIniFile.WriteString('BOOL',lIdent,Bool2Char(lValue));
           exit;
        end;
	lStr := lIniFile.ReadString('BOOL',lIdent, '');
	if length(lStr) > 0 then
		lValue := Char2Bool(lStr[1]);
end; //IniBool

procedure IniStr(lRead: boolean; lIniFile: TIniFile; lIdent: string; var lValue: string);
//read or write a string value to the initialization file
begin
  if not lRead then begin
    lIniFile.WriteString('STR',lIdent,lValue);
    exit;
  end;
  lValue := lIniFile.ReadString('STR',lIdent, '');
end; //IniStr

function IniFile(lRead: boolean;  var lPrefs: TPrefs): boolean;
//Read or write initialization variables to disk
var
   lFilename: string;
   lIniFile: TIniFile;
begin
  if (lRead) then
     lPrefs := SetDefaultPrefs
  else
      dcm2niiForm.ReadPrefs;
  lFilename := IniName;
  if lFilename = '' then exit(true);
  if (lRead) and (not Fileexists(lFilename)) then
        exit(false);
  {$IFDEF UNIX}if (lRead) then writeln('Loading preferences '+lFilename);{$ENDIF}
  lIniFile := TIniFile.Create(lFilename);
  IniBool(lRead,lIniFile, 'UseOutDir',lPrefs.UseOutDir);
  IniBool(lRead,lIniFile, 'Ignore',lPrefs.Ignore);
  IniBool(lRead,lIniFile, 'LosslessScale',lPrefs.LosslessScale);
  IniBool(lRead,lIniFile, 'PhilipsPrecise',lPrefs.PhilipsPrecise);
  IniBool(lRead,lIniFile, 'Crop',lPrefs.Crop);
  IniBool(lRead,lIniFile, 'Verbose',lPrefs.Verbose);
  IniInt(lRead,lIniFile, 'SeriesMerge',lPrefs.SeriesMerge);
  IniInt(lRead,lIniFile, 'Bids', lPrefs.Bids);
  IniInt(lRead,lIniFile, 'Format', lPrefs.Format);
  IniStr(lRead, lIniFile, 'OutDir', lPrefs.OutDir);
  IniStr(lRead, lIniFile, 'OutName', lPrefs.OutName  );
  lIniFile.Free;
  exit(true);
end;

function Tdcm2niiForm.TerminalCommand: string;
begin
    result := '';
    if OutNameEdit.Text <> '' then
       result := result + ' -f "'+OutNameEdit.Text+'"';
    if IgnoreCheck.Checked then result := result + ' -i y';
    if LosslessScaleCheck.Checked then result := result + ' -l y';
    if MergeDrop.ItemIndex = 1 then result := result + ' -m y';
    if MergeDrop.ItemIndex = 2 then result := result + ' -m n';
    if PhilipsPreciseCheck.Checked then
       result := result + ' -p y'
    else
       result := result + ' -p n';
    if CropCheck.Checked then result := result + ' -x y';
    if VerboseCheck.Checked then result := result + ' -v y';
    if odd(FormatDrop.ItemIndex) then
           result := result + ' -z y'
    else
      result := result + ' -z n';
    if (FormatDrop.ItemIndex > 1) then
       result := result + ' -e y';
    if BidsDrop.ItemIndex = 0 then
       result := result + ' -b n';
    if BidsDrop.ItemIndex = 2 then
       result := result + ' -ba n';
    if OutDirDrop.ItemIndex > 1 then
       result := result + ' -o "'+OutDirDrop.Items[OutDirDrop.ItemIndex]+'"';
end;

procedure Tdcm2niiForm.UpdateCommand(Sender: TObject);
var
   cmd: string;
begin
  cmd := TerminalCommand();
  StatusBar1.SimpleText:= kExeName+cmd+kDemoInputDir;
  OutNameExampleLabel.Caption:= RunCmd(TerminalCommand,true);
end;

procedure Tdcm2niiForm.ResetBtnClick(Sender: TObject);
begin
    gPrefs := SetDefaultPrefs();
    gCustomDcm2niix := '';
    ShowPrefs;
    UpdateCommand(Sender);
end;

procedure Tdcm2niiForm.SelectFilesBtnClick(Sender: TObject);
begin
  if not InputDirDialog.Execute then exit;
  ConvertDicomDir(InputDirDialog.Filename);
end;

procedure Tdcm2niiForm.FormShow(Sender: TObject);
begin
  {$IFNDEF UNIX}
  UpdateBtn.Caption := 'Set Executable Path';
  {$ENDIF}
  IniFile(true, gPrefs);
  ShowPrefs;
  UpdateCommand(Sender);
  InputDirDialog.InitialDir := GetUserDir;
  {$IFDEF LCLCocoa} {$IFDEF DARKMODE} GLForm1.SetFormDarkMode(dcm2niiForm); {$ENDIF}{$ENDIF}
end;

procedure Tdcm2niiForm.OutDirDropChange(Sender: TObject);
begin
  if OutDirDrop.ItemIndex <> 1 then begin
    UpdateCommand(Sender);
    exit;
  end;
  if not DirectoryExists(gPrefs.OutDir) then
     gPrefs.OutDir := GetUserDir;
  OutputDirDialog.InitialDir := gPrefs.OutDir;
  if OutputDirDialog.Execute then
     gPrefs.OutDir := OutputDirDialog.FileName;
  OutDirDrop.Items[2] := gPrefs.OutDir;
  OutDirDrop.ItemIndex := 2;
  UpdateCommand(Sender);
end;

procedure Tdcm2niiForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IniFile(false, gPrefs);
end;

procedure Tdcm2niiForm.ConvertDicomDir(DirName: string);
var
   cmd: string;
begin
     cmd := TerminalCommand()+' "'+DirName+'"';
     RunCmd(cmd,false);
end;

procedure Tdcm2niiForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  ConvertDicomDir( FileNames[0]);
end;

function Tdcm2niiForm.RunCmd (lCmd: string; isDemo: boolean): string;
//http://wiki.freepascal.org/Executing_External_Programs
const
  kKeyStr = 'Example output filename: ';
var
  OutputLines: TStringList;
  MemStream: TMemoryStream;
  OurProcess: TProcess;
  NumBytes: LongInt;
  i: integer;
  exe: string;
const
  READ_BYTES_BIG = 32768;

begin
     result := ''; //EXIT_FAILURE
   //if (not isAppDoneInitializing) then exit;
   exe := getCurrentDcm2niix();
   if not fileexists(exe) then begin
      OutputMemo.Lines.Clear;
      OutputMemo.Lines.Add('Error: unable to find '+exe);
      exit;
   end;
   OutputMemo.Lines.Clear;
   if isDemo then
      OutputMemo.Lines.Add(exe+lCmd+kDemoInputDir)
   else
       OutputMemo.Lines.Add(exe+lCmd);
   MemStream := TMemoryStream.Create;
   //BytesRead := 0;
   OurProcess := TProcess.Create(nil);
   {$IFDEF UNIX}
   OurProcess.Environment.Add(GetEnvironmentVariable('PATH'));
   {$ENDIF}
   OurProcess.CommandLine := exe+lCmd;
  OurProcess.Options := [poUsePipes, poNoConsole];
  OurProcess.Execute;
  OutputLines := TStringList.Create;
  MemStream.SetSize(READ_BYTES_BIG);
  dcm2niiForm.Refresh;
  while True do begin
    NumBytes := OurProcess.Output.Read((MemStream.Memory)^, READ_BYTES_BIG);
    if NumBytes > 0 then begin
       MemStream.SetSize(NumBytes);
       OutputLines.LoadFromStream(MemStream);
       if (isDemo) and (result = '') and (OutputLines.Count > 0) then begin
         for i := 0 to OutputLines.Count - 1 do begin
             if PosEx(kKeyStr,OutputLines[i]) > 0 then begin
                result := OutputLines[i];
                Delete(result, 1, length(kKeyStr));
             end;
         end;
       end;
       OutputMemo.Lines.AddStrings(OutputLines);
       MemStream.SetSize(READ_BYTES_BIG);
       MemStream.Position:=0;
       dcm2niiForm.Refresh;
       Application.ProcessMessages;
       Sleep(15);
    end else
      BREAK; // Program has finished execution.
  end;
  if isDemo then begin
     OutputMemo.Lines.Add('');
     OutputMemo.Lines.Add('Drop files/folders to convert here');

  end;
  dcm2niiForm.Refresh;
  OutputLines.Free;
  OurProcess.Free;
  MemStream.Free;
end;

procedure Tdcm2niiForm.UpdateDialog(versionMsg: string);
const
  kURL = 'https://github.com/rordenlab/dcm2niix/releases';
var
    PrefForm: TForm;
    openDialog : TOpenDialog;
    UrlBtn,OkBtn,UseDefaultBtn, CustomBtn: TButton;
    promptLabel: TLabel;
    defaultDcm2niix, currentDcm2niix: string;
    isSetCustomPath,isGotoURL, isCurrentAlsoDefault: boolean;
begin
  PrefForm:=TForm.Create(nil);
  //PrefForm.SetBounds(100, 100, 512, 212);
  PrefForm.AutoSize := True;
  PrefForm.BorderWidth := 8;
  PrefForm.Caption:='dcm2niix Settings';
  PrefForm.Position := poScreenCenter;
  PrefForm.BorderStyle := bsDialog;
  //Possible situations:
  // if getDefaultDcm2niix is not set but exists
  //label
  promptLabel:=TLabel.create(PrefForm);
  currentDcm2niix := getCustomDcm2niix();
  defaultDcm2niix := getCurrentDcm2niix();
  if (currentDcm2niix  = '') or (not fileexists(currentDcm2niix)) then
     currentDcm2niix := defaultDcm2niix;
  isCurrentAlsoDefault := CompareStr(currentDcm2niix, defaultDcm2niix) = 0;
  if (not fileexists(defaultDcm2niix)) and (not isCurrentAlsoDefault) then
     isCurrentAlsoDefault := true; //default does not exist: do not show "Select Default")
  if versionMsg = '' then begin
     if fileexists(currentDcm2niix) then
        promptLabel.Caption:= format('dcm2niix path: "%s"', [currentDcm2niix])
     else
         promptLabel.Caption:= 'Unable to find dcm2niix';
  end else
      promptLabel.Caption:= versionMsg;
  promptLabel.AutoSize := true;
  promptLabel.AnchorSide[akTop].Side := asrTop;
  promptLabel.AnchorSide[akTop].Control := PrefForm;
  promptLabel.BorderSpacing.Top := 6;
  promptLabel.AnchorSide[akLeft].Side := asrLeft;
  promptLabel.AnchorSide[akLeft].Control := PrefForm;
  promptLabel.BorderSpacing.Left := 6;
  promptLabel.Parent:=PrefForm;
  //UrlBtn Btn
  UrlBtn:=TButton.create(PrefForm);
  UrlBtn.Caption:='Visit '+kURL;
  UrlBtn.AutoSize := true;
  UrlBtn.AnchorSide[akTop].Side := asrBottom;
  UrlBtn.AnchorSide[akTop].Control := promptLabel;
  UrlBtn.BorderSpacing.Top := 6;
  UrlBtn.AnchorSide[akLeft].Side := asrLeft;
  UrlBtn.AnchorSide[akLeft].Control := PrefForm;
  UrlBtn.BorderSpacing.Left := 6;
  UrlBtn.Parent:=PrefForm;
  UrlBtn.ModalResult:= mrCancel;
  //CustomBtn
  CustomBtn:=TButton.create(PrefForm);
  CustomBtn.Caption:='Select custom dcm2niix path';
  CustomBtn.AutoSize := true;
  CustomBtn.AnchorSide[akTop].Side := asrBottom;
  CustomBtn.BorderSpacing.Top := 6;
  CustomBtn.AnchorSide[akLeft].Side := asrLeft;
  CustomBtn.AnchorSide[akLeft].Control := PrefForm;
  CustomBtn.BorderSpacing.Left := 6;
  CustomBtn.Parent:=PrefForm;
  CustomBtn.ModalResult:= mrIgnore;
  //UseDefaultBtn
  if not isCurrentAlsoDefault then begin
    UseDefaultBtn:=TButton.create(PrefForm);
    //UseDefaultBtn.Caption:= format('Reset default "%s"', [defaultDcm2niix]);
    UseDefaultBtn.Caption:= 'Reset default dcm2niix path';
    UseDefaultBtn.AutoSize := true;
    UseDefaultBtn.AnchorSide[akTop].Side := asrBottom;
    UseDefaultBtn.AnchorSide[akTop].Control := UrlBtn;
    UseDefaultBtn.BorderSpacing.Top := 6;
    UseDefaultBtn.AnchorSide[akLeft].Side := asrLeft;
    UseDefaultBtn.AnchorSide[akLeft].Control := PrefForm;
    UseDefaultBtn.BorderSpacing.Left := 6;
    UseDefaultBtn.Parent:=PrefForm;
    UseDefaultBtn.ModalResult:= mrAbort;
    CustomBtn.AnchorSide[akTop].Control := UseDefaultBtn;
  end else
     CustomBtn.AnchorSide[akTop].Control := UrlBtn;
  //OK button
  OkBtn:=TButton.create(PrefForm);
  OkBtn.Caption:='OK';
  OkBtn.AutoSize := true;
  OkBtn.AnchorSide[akTop].Side := asrBottom;
  OkBtn.AnchorSide[akTop].Control := CustomBtn;
  OkBtn.BorderSpacing.Top := 6;
  OkBtn.AnchorSide[akLeft].Side := asrRight;
  OkBtn.AnchorSide[akLeft].Control := UrlBtn;
  OkBtn.BorderSpacing.Left := 6;
  OkBtn.Parent:=PrefForm;
  OkBtn.ModalResult:= mrOK;
  //Display form
  PrefForm.ActiveControl := OkBtn;
  PrefForm.ShowModal;
  isGotoURL := (PrefForm.ModalResult = mrCancel);
  if (PrefForm.ModalResult = mrAbort) then //isResetDefault
     gCustomDcm2niix := defaultDcm2niix;
  isSetCustomPath := (PrefForm.ModalResult = mrIgnore);
  FreeAndNil(PrefForm);
  if (isGotoURL) then
     OpenURL(kURL); //uses lclintf
  if isSetCustomPath then begin
    openDialog := TOpenDialog.Create(self);
    openDialog.Title := 'Find dcm2niix executable';
    openDialog.InitialDir := GetCurrentDir;
    openDialog.Options := [ofFileMustExist];
    if openDialog.Execute then
       gCustomDcm2niix := openDialog.FileName;
    openDialog.Free;
  end;
end; //GetFloat()

procedure Tdcm2niiForm.UpdateBtnClick(Sender: TObject);
begin
  {$IFDEF UNIX}
  RunCmd(' -u', false);
  if OutputMemo.Lines.Count > 2 then
     UpdateDialog(OutputMemo.Lines[2])
  else
  {$ENDIF}
  UpdateDialog('');
  UpdateCommand(Sender);
end;

end.

