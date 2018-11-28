unit dcm2nii;

{$mode delphi}{$H+}
interface
uses
    FileUtil, Process,LResources,
    {$IFDEF UNIX} LCLIntf, {$ENDIF}
  {$IFNDEF UNIX} Registry, {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, SimdUtils;

type
  { Tdcm2niiForm }
  Tdcm2niiForm = class(TForm)
    CompressCheck: TCheckBox;
    verboseCheck: TCheckBox;
    bidsCheck: TCheckBox;
    outnameEdit: TEdit;
    outnameLabel: TLabel;
    outputFolderLabel: TLabel;
    outputFolderName: TButton;
    Panel2: TPanel;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    EditMenu: TMenuItem;
    CopyMenu: TMenuItem;
    DicomMenu: TMenuItem;
    ResetMenu: TMenuItem;
    ParRecMenu: TMenuItem;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    procedure compressCheckClick(Sender: TObject);
    procedure DicomMenuClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function getOutputFolder: string;
    procedure outnameEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ParRecMenuClick(Sender: TObject);
    procedure ProcessFile(infilename: string);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure CopyMenuClick(Sender: TObject);
    procedure outputFolderNameClick(Sender: TObject);
    procedure ResetMenuClick(Sender: TObject);
    function RunCmd (lCmd: string; isDemo: boolean; out line1: string): integer;
    function getExeName : string; //return path for command line tool
    procedure readIni (ForceReset: boolean); //load preferences
    procedure writeIni; //save preferences
    function getCustomDcm2niix(): string;
    procedure setCustomDcm2niix(fnm: string);
    procedure findCustomDcm2niix();

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  dcm2niiForm: Tdcm2niiForm;


implementation
  {$R *.lfm}

  {$IFDEF CPU64}
 const kExeName = 'dcm2niix';
 {$ELSE}
        {$IFDEF Linux}
        const kExeName = 'dcm2niix32';
        {$ELSE}
        const kExeName = 'dcm2niix';
        {$ENDIF}
 {$ENDIF}

  var
    isAppDoneInitializing : boolean = false;
    gCustomDcm2niix : string = '';

function Tdcm2niiForm.getCustomDcm2niix(): string;
begin
      result := gCustomDcm2niix;
end;

procedure Tdcm2niiForm.setCustomDcm2niix(fnm: string);
begin
     gCustomDcm2niix := fnm;
end;

{$IFDEF UNIX}
function FindDefaultExecutablePathX(const Executable: string): string;
begin
     {$IFDEF Darwin}
     result := ResourceDir +pathdelim+ kExeName;
     if fileexists(result) then exit;
     {$ENDIF}
     result := FindDefaultExecutablePath(kExeName);
     if result = '' then
        result := FindDefaultExecutablePath(ExtractFilePath  (paramstr(0)) +kExeName);
     {$IFDEF Darwin}
     if fileexists(result) then exit;
     result := '/usr/local/bin/'+kExeName;
     if fileexists(result) then exit;
     result := ResourceDir +pathdelim+ kExeName;
     {$ENDIF}
end;
{$ELSE}
function FindDefaultExecutablePathX(const Executable: string): string;
begin
     result := extractfilepath(paramstr(0))+kExeName+'.exe';
     if fileexists(result) then exit;
     result := ResourceDir +pathdelim+ kExeName+'.exe';
end;
{$ENDIF}

function Tdcm2niiForm.getExeName : string;
var
  lF: string;
begin
     if fileexists(gCustomDcm2niix) then
        exit(gCustomDcm2niix);
     result := FindDefaultExecutablePathX(kExeName);
     if not fileexists(result) then begin
        lF :=  ExtractFilePath (paramstr(0))+'Resources'+pathdelim;
        if not DirectoryExists(lF) then
           lF :=  ExtractFilePath (paramstr(0));
        result := lF+kExeName;
      {$IFNDEF UNIX}result := result + '.exe'; {$ENDIF}
      if not fileexists(result) then begin
           Memo1.Lines.Clear;
           memo1.Lines.Add('Error: unable to find executable '+kExeName+' in path');
           memo1.Lines.Add(' Solution: copy '+kExeName+' to '+lF);
           result := '';
        end;  //not in same folder as GUI
     end; //not in path
     {$IFNDEF UNIX} //strip .exe for Windows
     result := ChangeFileExt(result, '');
     {$ENDIF}
end; //exeName()

{$IFDEF UNIX}
function iniName : string;
begin
     result := GetEnvironmentVariable ('HOME')+PathDelim+'.dcm2nii.ini';
end;

procedure Tdcm2niiForm.writeIni;
var
   iniFile : TextFile;
 begin
   FileMode := fmOpenWrite;
   AssignFile(iniFile, iniName);
   ReWrite(iniFile);
   if (CompressCheck.checked) then
      WriteLn(iniFile, 'isGZ=1')
   else
       WriteLn(iniFile, 'isGZ=0');
   if (bidsCheck.checked) then
      WriteLn(iniFile, 'isBIDS=1')
   else
       WriteLn(iniFile, 'isBIDS=0');
   WriteLn(iniFile, 'filename='+outnameEdit.caption);
   CloseFile(iniFile);
   FileMode := fmOpenRead;
end; //writeIni

procedure Tdcm2niiForm.readIni (ForceReset: boolean);
var
  fileData, rowData : TStringList;
  row, i: integer;
  opts_isGz, opts_isBids: boolean;
  opts_filename: string;
begin
     opts_isGz := true;
     opts_isBids := true;
     //opts_outdir := '';
     opts_filename := '%t_%p_%s';
     if FileExists( iniName) and (not (ForceReset )) then begin
        fileData := TStringList.Create;
        fileData.LoadFromFile(iniName);  // Load from Testing.txt file
        if (fileData.Count > 0) then begin
           rowData := TStringList.Create;
           rowData.Delimiter := '=';
           for row := 0 to (fileData.Count-1) do begin //for each row of file
               rowData.DelimitedText:=fileData[row];
               if ((rowData.Count > 1) and (CompareText(rowData[0] ,'isGZ')= 0)) then
                  opts_isGz := (CompareText(rowData[1],'1') = 0);
               if ((rowData.Count > 1) and (CompareText(rowData[0] ,'isBIDS')= 0)) then
                  opts_isBids := (CompareText(rowData[1],'1') = 0);
               if ((rowData.Count > 1) and (CompareText(rowData[0] ,'filename')= 0)) then begin
                  opts_filename := '';
                  if (rowData.Count > 2) then
                     for i := 1 to (rowData.Count-2) do
                         opts_filename := opts_filename+ rowData[i]+' ';
                  opts_filename := opts_filename+ rowData[rowData.Count-1];
               end;
           end;
          rowData.Free;
        end;
        fileData.Free;
     end else
         memo1.Lines.Add('Using default settings');
     CompressCheck.Checked := opts_isGz;
     bidsCheck.Checked := opts_isBids;
     outnameEdit.Caption := opts_filename;
     //getExeName;
end; //readIni()
{$ELSE}
//For Windows we save preferences in the registry to ensure user has write access
procedure Tdcm2niiForm.writeIni;
var
  ARegistry: TRegistry;
begin
     ARegistry := TRegistry.Create;
     ARegistry.RootKey := HKEY_CURRENT_USER;//HKEY_LOCAL_MACHINE;
     if ARegistry.OpenKey ('\Software\dcm2nii',true) then begin
       	  ARegistry.WriteBool('isGZ', CompressCheck.Checked );
          ARegistry.WriteBool('isBIDS', bidsCheck.Checked );
       	  ARegistry.WriteString('filename', outnameEdit.text );
     end;
     ARegistry.Free;
end; //writeIni()

procedure Tdcm2niiForm.readIni (ForceReset: boolean);
var
  ARegistry: TRegistry;
  opts_isGz, opts_isBids: boolean;
  opts_filename: string;
begin
     opts_isBids := true;
     opts_isGz := true;
     opts_filename := '%t_%p_%s';
     if not ForceReset then begin
       ARegistry := TRegistry.Create;
       ARegistry.RootKey := HKEY_CURRENT_USER;//HKEY_LOCAL_MACHINE;
       if ARegistry.OpenKey ('\Software\dcm2nii',true) then begin
       	    if ARegistry.ValueExists( 'isGZ' ) then
          	   opts_isGz := ARegistry.ReadBool( 'isGZ' );
       	    if ARegistry.ValueExists( 'isBIDS' ) then
          	   opts_isBids := ARegistry.ReadBool( 'isBIDS' );
            if ARegistry.ValueExists( 'isGZ' ) then
          	   opts_filename := ARegistry.ReadString( 'filename' );
       end;
       ARegistry.Free;
     end;
     bidsCheck.Checked := opts_isBids;
     CompressCheck.Checked := opts_isGz;
     outnameEdit.text := opts_filename;
end; //readIni()
{$ENDIF}

function Tdcm2niiForm.RunCmd (lCmd: string; isDemo: boolean; out line1: string): integer;
//http://wiki.freepascal.org/Executing_External_Programs
var
  OutputLines: TStringList;
  MemStream: TMemoryStream;
  OurProcess: TProcess;
  NumBytes: LongInt;
  BytesRead: LongInt;
const
  READ_BYTES = 2048;
begin
     result := 1; //EXIT_FAILURE
   if (not isAppDoneInitializing) then exit;
   if (getExeName = '') then exit;
   Memo1.Lines.Clear;
   dcm2niiForm.refresh; Memo1.refresh; Memo1.invalidate;
   MemStream := TMemoryStream.Create;
   BytesRead := 0;
   OurProcess := TProcess.Create(nil);
   {$IFDEF UNIX}
   OurProcess.Environment.Add(GetEnvironmentVariable('PATH'));
   {$ENDIF}
   OurProcess.CommandLine := lCmd;
  // We cannot use poWaitOnExit here since we don't
  // know the size of the output. On Linux the size of the
  // output pipe is 2 kB; if the output data is more, we
  // need to read the data. This isn't possible since we are
  // waiting. So we get a deadlock here if we use poWaitOnExit.
  OurProcess.Options := [poUsePipes, poNoConsole];
  OurProcess.Execute;
  while True do begin
    // make sure we have room
    MemStream.SetSize(BytesRead + READ_BYTES);
    // try reading it
    NumBytes := OurProcess.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES);
    if NumBytes > 0 // All read() calls will block, except the final one.
    then begin
      Inc(BytesRead, NumBytes);
    end else
      BREAK // Program has finished execution.
  end;
  MemStream.SetSize(BytesRead);
  OutputLines := TStringList.Create;
  OutputLines.LoadFromStream(MemStream);
  Memo1.Lines.AddStrings(OutputLines);
  if OutputLines.Count > 0 then begin
      Line1 := OutputLines[0];
      //skip if line is "Compression will be faster with 'pigz'"
      if (pos('Compression', Line1) = 1) and (OutputLines.Count > 0) then
         Line1 := OutputLines[1];
  end;
  if isDemo then
     Memo1.Lines.Add(lCmd+' "MyDicomFolder"')
  else
      Memo1.Lines.Add(lCmd);
  OutputLines.Free;
  result := OurProcess.ExitCode;
  OurProcess.Free;
  MemStream.Free;
end;

function Tdcm2niiForm.getOutputFolder: string;
begin
     if (outputFolderName.Tag > 0) then
        result := outputFolderName.Caption
     else
         result := '';
end; //getOutputFolder

procedure Tdcm2niiForm.ProcessFile(infilename: string);
var
  cmd, outputFolder, inFolder, line1: string;
begin
  inFolder := infilename;
  (*if isTGZ(inFolder) then begin
  	 infolder := deTGZ(infolder);
     if infolder = '' then exit; //error
 end;*)
 cmd := '"'+getExeName +'" ';
 if bidsCheck.checked then
    cmd := cmd + '-b y '
 else
     cmd := cmd + '-b n ';
 if CompressCheck.checked then
    cmd := cmd + '-z y '
 else
     cmd := cmd + '-z n ';
 if verboseCheck.checked then
 cmd := cmd + '-v y ';
 outputFolder := getOutputFolder;
 if length(outputFolder) > 0 then
     cmd := cmd + '-o '+outputFolder+' ';
 cmd := cmd + '-f "'+outnameEdit.Text+'" ';
 if length(inFolder) > 0 then
     cmd := cmd +'"'+inFolder+'"';
     //Caption := inttostr(length(inFolder));
 RunCmd(cmd, length(inFolder) = 0, line1);
end; //ProcessFile()

procedure Tdcm2niiForm.outnameEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
      ProcessFile('');
end; //outnameEditKeyUp()

procedure Tdcm2niiForm.ParRecMenuClick(Sender: TObject);
var
  lI: integer;
begin
  if not OpenDialog1.execute then exit;
  //ProcessFile(OpenDialog1.filename);
  if OpenDialog1.Files.count < 1 then exit;
     for lI := 0 to (OpenDialog1.Files.count-1) do
         ProcessFile(OpenDialog1.Files[lI]);
end; //ParRecMenuClick()

function getDirPrompt (lDefault: string): string;
begin
  result := lDefault;  // Set the starting directory
  chdir(result); //start search from default dir...
  if SelectDirectory(result, [sdAllowCreate,sdPerformCreate,sdPrompt], 0) then
     chdir(result)
  else
      result := '';
end;  //getDirPrompt()

procedure Tdcm2niiForm.DicomMenuClick(Sender: TObject);
var
  dir: string;
begin
     dir := getDirPrompt('');
     ProcessFile( dir);
end; //DicomMenuClick()

procedure Tdcm2niiForm.compressCheckClick(Sender: TObject);
begin
  ProcessFile('');
end;

procedure Tdcm2niiForm.FormResize(Sender: TObject);
begin
  outputFolderName.width := dcm2niiForm.Width-outputFolderName.left-2;
end; //FormResize()

procedure Tdcm2niiForm.findCustomDcm2niix();
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
end;

procedure Tdcm2niiForm.FormShow(Sender: TObject);
begin
     if not fileexists(getExeName) then
        findCustomDcm2niix();

     ProcessFile('');
     inherited;


end;

procedure Tdcm2niiForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
    ProcessFile( FileNames[0]);
end; //FormDropFiles()

procedure Tdcm2niiForm.CopyMenuClick(Sender: TObject);
begin
     Memo1.SelectAll;
     Memo1.CopyToClipboard;
end; //CopyMenuClick()

procedure Tdcm2niiForm.outputFolderNameClick(Sender: TObject);
var
  lDir : string;
begin
     if (outputFolderName.Tag > 0) then //start search from prior location
        lDir := outputFolderName.Caption
     else
         lDir := '';
     lDir := getDirPrompt(lDir);
     outputFolderName.Tag := length(lDir);
     if length(lDir) > 0 then
        outputFolderName.Caption := lDir
     else
         outputFolderName.Caption := 'input folder';
end; //outputFolderNameClick()

procedure Tdcm2niiForm.ResetMenuClick(Sender: TObject);
begin
  isAppDoneInitializing := false;
     readIni(true);
     isAppDoneInitializing := true;
     ProcessFile('');
end;

procedure Tdcm2niiForm.FormCreate(Sender: TObject);
begin
     readIni(false);
     application.ShowButtonGlyphs:= sbgNever;
     isAppDoneInitializing := true;
end; //FormCreate()

procedure Tdcm2niiForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
     writeIni;
end; //FormClose()

end.

