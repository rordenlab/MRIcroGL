unit prefs;
{$D-,O+,Q-,R-,S-}   //Delphi L-,Y-
{$H+}

interface
uses IniFiles,SysUtils,Dialogs,Classes, SimdUtils, slices2D;
const
  knMRU = 10;
type
  TMRU =  array [1..knMRU] of string;
  TPrefs = record
         AnimationIntervalMsec, LineWidth, StartupWindowMode,DisplayOrient, StartupDisplayOrient, ColorbarSize,ColorbarPosition, Quality1to10, BitmapZoom, MaxVox: integer;
         LandmarkPanel, LoadFewVolumes, LabelOrient, ColorbarVisible, Smooth2D, DarkMode, RetinaDisplay, FlipYZ, FlipLR_Radiological, SkipPrefWriting: boolean;
         CustomDcm2niix, PyLib, MosaicStr, InitScript, PrevBackgroundImage: string;
         ClearColor: TRGBA;
         PrevFilename: TMRU;

  end;
  function IniName: string;
function IniFile(lRead: boolean; var lPrefs: TPrefs): boolean;
procedure SetDefaultPrefs (var lPrefs: TPrefs; lEverything: boolean);
procedure Add2MRU (var MRU: TMRU; Filename: string);

implementation

procedure Add2MRU (var MRU: TMRU; Filename: string);
var
 strs: array [1..knMRU+1] of string;
 i, nOK, k: integer;
 isNovel: boolean;
begin
     if not Fileexists(Filename) then exit;
     if Filename = MRU[1] then exit; //no change
     for i := 1 to knMRU+1 do
         strs[i] := '';
     nOK := 1;
     strs[nOK] :=  Filename;

     for i := 1 to knMRU do begin
         if not fileexists(MRU[i]) then continue;
         isNovel := true;
         for k := 1 to nOK do
             if MRU[i] = strs[k] then
                isNovel := false;
         if not isNovel then continue;
         nOK := nOK + 1;
         strs[nOK] := MRU[i];
     end;
     for i := 1 to knMRU do
         MRU[i] := strs[i];
end;

procedure SetDefaultPrefs (var lPrefs: TPrefs; lEverything: boolean);
var
  i: integer;
begin
  if lEverything then begin  //These values are typically not changed...
     with lPrefs do begin
            for i := 1 to knMRU do
              PrevFilename[i] := '';
            PyLib := '';
            PrevBackgroundImage := '';
            CustomDcm2niix := '';
            RetinaDisplay := false;
            DisplayOrient:= kAxCorSagOrient; //kRenderOrient;
            StartupDisplayOrient := DisplayOrient;
            DarkMode := false;
            StartupWindowMode := 0;
            LineWidth := 1;
            AnimationIntervalMsec:=100;
            LabelOrient := true;
            LoadFewVolumes := true;
            LandmarkPanel := false;
            BitmapZoom := 2;
            FlipLR_Radiological := true;
            ColorbarSize := 50;
            MaxVox := 640;
            Smooth2D := true;
       end;
  end;
  with lPrefs do begin
    //clear
      ColorbarVisible := true;

    ClearColor := setRGBA( 0, 0, 0, 255);
    //ClearColor := setRGBA( 255, 255, 255, 255);
    ColorBarPosition := 3;
    SkipPrefWriting := false;
    FlipYZ := false;
    Quality1to10 := 5;
    MosaicStr := 'H 0.2 S 0.5 0.3 A 0.5; S 0.2 C 0.3 A 0.4';
  end;//with lPrefs
end; //Proc SetDefaultPrefs

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

procedure IniMRU(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lMRU: TMRU);
var
   lI: integer;
begin
     for lI := 1 to knMRU do
         IniStr(lRead,lIniFile,lIdent+inttostr(lI),lMRU[lI]); //write values
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

function IniName: string;
begin
  result := GetUserDir;
  if result = '' then exit;
  result := result + '.'+ChangeFileExt(ExtractFileName(ParamStr(0)),'')+'12.ini';
end;

function IniFile(lRead: boolean;  var lPrefs: TPrefs): boolean;
//Read or write initialization variables to disk
var
   lFilename: string;
   lIniFile: TIniFile;
begin
  if (lRead) then
     SetDefaultPrefs(lPrefs, true);
  if (not lRead) and (lPrefs.SkipPrefWriting) then exit;
  lFilename := IniName;
  if lFilename = '' then exit;
  //showmessage(lFilename);
  result := false;
  if (lRead) and (not Fileexists(lFilename)) then
        exit;
  {$IFDEF UNIX}if (lRead) then writeln('Loading preferences '+lFilename);{$ENDIF}
  lIniFile := TIniFile.Create(lFilename);
  IniMRU(lRead,lIniFile,'PrevFilename', lPrefs.PrevFilename);
  IniStr(lRead, lIniFile, 'PyLib', lPrefs.PyLib);
  IniStr(lRead, lIniFile, 'PrevBackgroundImage', lPrefs.PrevBackgroundImage  );
  IniStr(lRead, lIniFile, 'CustomDcm2niix', lPrefs.CustomDcm2niix);
  IniInt(lRead,lIniFile, 'Quality1to10', lPrefs.Quality1to10);
  IniInt(lRead,lIniFile, 'ColorbarPosition',lPrefs.ColorBarPosition);
  IniInt(lRead,lIniFile, 'ColorbarSize', lPrefs.ColorbarSize);
  IniInt(lRead,lIniFile, 'BitmapZoom', lPrefs.BitmapZoom);
  //IniInt(lRead,lIniFile, 'DisplayOrient', lPrefs.DisplayOrient);
  IniInt(lRead,lIniFile, 'StartupDisplayOrient', lPrefs.StartupDisplayOrient);

  IniInt(lRead,lIniFile, 'MaxVox', lPrefs.MaxVox);
  IniInt(lRead,lIniFile, 'LineWidth', lPrefs.LineWidth);
  IniInt(lRead,lIniFile, 'StartupWindowMode', lPrefs.StartupWindowMode);
  IniInt(lRead,lIniFile, 'AnimationIntervalMsec', lPrefs.AnimationIntervalMsec);
  {$IFDEF LCLCocoa}
        IniBool(lRead,lIniFile, 'RetinaDisplay',lPrefs.RetinaDisplay);
        IniBool(lRead,lIniFile, 'DarkMode',lPrefs.DarkMode);
    {$ENDIF}
  IniBool(lRead,lIniFile, 'ColorbarVisible',lPrefs.ColorbarVisible);
  IniBool(lRead,lIniFile, 'Smooth2D',lPrefs.Smooth2D);
  IniBool(lRead,lIniFile, 'LabelOrient',lPrefs.LabelOrient);
  IniBool(lRead,lIniFile, 'LoadFewVolumes',lPrefs.LoadFewVolumes);
  IniBool(lRead,lIniFile, 'LandmarkPanel',lPrefs.LandmarkPanel);
  IniBool(lRead,lIniFile, 'FlipYZ',lPrefs.FlipYZ);
  IniBool(lRead,lIniFile, 'FlipLR_Radiological',lPrefs.FlipLR_Radiological);
  lIniFile.Free;
end;

end.
