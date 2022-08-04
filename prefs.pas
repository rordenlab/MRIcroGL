unit prefs;
{$D-,O+,Q-,R-,S-}   //Delphi L-,Y-
{$H+}
{$IFDEF Darwin} {$modeswitch objectivec2} {$ENDIF}

interface
uses Process, {$IFDEF UNIX}BaseUnix,{$ENDIF} {$IFDEF Darwin}CocoaAll, {$ENDIF} IniFiles,SysUtils,Dialogs,Classes, SimdUtils, Math, slices2D;
const
  knMRU = 10;
  //kMaxVoxDefault = 560;
  kMaxTexMb = 512; //64^3=1mb, 256^3=64mb, 512^3=512mb, 812^3=2042mb, 1024^2=4096mb
type
  TMRU =  array [1..knMRU] of string;
  TPrefs = record
         GradientMode, AnimationIntervalMsec, LineWidth, StartupWindowMode,DisplayOrient,
         StartupDisplayOrient, ColorbarSize,ColorbarPosition, Quality1to5, BitmapZoom,
         MaxTexMb, MultiSample124, ClusterNeighborMethod, VolumeSaveFormat, VoiSaveFormat: integer;
         ScreenCaptureTransparentBackground, LandmarkPanel, LoadFewVolumes,
         DebugMode, LoadSmooth, LabelOrient, RulerVisible, ColorbarVisible, Smooth2D, DarkMode, RetinaDisplay,
         FlipYZ, FlipLR_Radiological, SkipPrefWriting, AutoClusterizeAtlases, RenderDepthPicker: boolean;
         FSLDir, AfniDir, CustomDcm2niix, PyLib, MosaicStr, InitScript, PrevScript, PrevBackgroundImage, DicomDir: string;
         LineColor, ClearColor: TRGBA;
         PrevFilename: TMRU;

  end;
  function IniName: string;
function IniFile(lRead: boolean; var lPrefs: TPrefs): boolean;
procedure SetDefaultPrefs (var lPrefs: TPrefs; lEverything: boolean);
procedure Add2MRU (var MRU: TMRU; Filename: string);

implementation

{$IFDEF Darwin} uses nifti_foreign; {$ENDIF}

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
         {$IFDEF Darwin}
         if not IsReadable(MRU[i]) then continue;
         {$ENDIF}
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

(*function RunCommandX(exe,args: string; out outputstring: string): boolean;
//e.g.  RunCommandX('/bin/zsh',' -l -c "which afni"', s)
begin
     outputstring := '';
     if not FileExists(exe) then exit(false);
     {$IFDEF UNIX}
     if fpAccess(exe,X_OK) <> 0 then exit(false); //If access is denied, or an error occurred, a nonzero value is returned
     {$ENDIF}
     result := RunCommand(exe+' '+args, outputstring);
     {$IFDEF UNIX}
     writeln(format('Result %s: %s', [BoolToStr(result, true), outputstring]));
     {$ENDIF}
end; *)

procedure SetDefaultPrefs (var lPrefs: TPrefs; lEverything: boolean);
//label
//  123;
var
  i: integer;
  tmp: string;
begin
  if lEverything then begin  //These values are typically not changed...
     with lPrefs do begin
            for i := 1 to knMRU do
              PrevFilename[i] := '';
            PyLib := '';
            //InitScript := '';
            PrevScript := '';
            {$IFDEF UNIX}
            if not DirectoryExists(AfniDir) then
            	AfniDir := '';
            tmp := GetEnvironmentVariable('AFNI_ATLAS_PATH');
            if DirectoryExists(tmp) then
            	AfniDir := tmp;
             if not DirectoryExists(FSLDir)  then
                FSLDir := '';
            FSLDir := GetEnvironmentVariable('FSLDIR');
            if DirectoryExists(tmp) then
              FSLDir := tmp;
            (*if DirectoryExists(AfniDir) then goto 123;
            AfniDir := expandfilename('~/')+'abin';
            if DirectoryExists(AfniDir) then goto 123;
            RunCommandX('/bin/zsh','-l -c "which afni"', AfniDir);
            if DirectoryExists(AfniDir) then goto 123;
            //if (not FileExists('/bin/zsh')) and (not DirectoryExists(AfniDir)) then begin
            RunCommandX('/bin/bash', '-l -c "which afni"', AfniDir);
            123:*)
            if not DirectoryExists(AfniDir) then begin
               writeln('Unable to find AFNI_ATLAS_PATH ', AfniDir);
               AfniDir := '';
            end;
            {$ELSE}
            AfniDir := '';
            {$ENDIF}
            //if not DirectoryExists(AfniDir) then AfniDir := '';
            ClusterNeighborMethod := 1; //1=faces only,2=faces+edges,3=faces+edges+corners
            PrevBackgroundImage := '';
            DicomDir := '';
            CustomDcm2niix := '';
            RetinaDisplay := true;
            //MultiSample := true;
            MultiSample124 := 4;
            VolumeSaveFormat := 1;
            VoiSaveFormat := 3;
            LineColor.R := 128;
            LineColor.G := 128;
            LineColor.B := 179;
            LineColor.A := 255;
            DisplayOrient:= kAxCorSagOrient3; //kRenderOrient;
            //DisplayOrient:= kRenderOrient;
            StartupDisplayOrient := DisplayOrient;
            DarkMode := false;
            RenderDepthPicker := true;
            GradientMode := 2; //kGradientModeGPUSlow
            StartupWindowMode := 0;
            LineWidth := 1;
            AnimationIntervalMsec:=100;
            LabelOrient := true;
            //LabelOrientCube := true;
            LoadFewVolumes := true;
            LandmarkPanel := false;
            FlipYZ := false;
            ScreenCaptureTransparentBackground := false;
            BitmapZoom := 2;
            FlipLR_Radiological := true;
            AutoClusterizeAtlases := true;
            ColorbarSize := 50;
            MaxTexMb := kMaxTexMb; //560;
            Smooth2D := true;
            LoadSmooth := true;
            DebugMode := false;
       end;
  end;
  with lPrefs do begin
    ColorbarVisible := true;
    RulerVisible := false;
    ClearColor := setRGBA( 0, 0, 0, 255);
    //ClearColor := setRGBA( 255, 255, 255, 255);
    ColorBarPosition := 3;
    SkipPrefWriting := false;
    Quality1to5 := 0;
    MosaicStr := 'H 0.2 S 0.5 0.3 A 0.5; S 0.2 C 0.3 A 0.4';
    if ScreenCaptureTransparentBackground then
       ClearColor.A := 0;
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

const
  kStrSep = '|';

function RGBAToStr (lU: tRGBA) : string;
begin
  result := Inttostr(lU.r)+ kStrSep+Inttostr(lU.g)+ kStrSep+Inttostr(lU.b)+ kStrSep+Inttostr(lU.a);
end;

function StrToRGBA(lS: string; var lU: TRGBA): boolean;
var
 strlst:TStringList;
begin
  result := false;
  strlst:=TStringList.Create;
  strlst.Delimiter:=kStrSep;
  strlst.DelimitedText := lS;
  if strlst.Count > 3 then begin
     lU := SetRGBA( strtoint(strlst[0]), strtoint(strlst[1]), strtoint(strlst[2]), strtoint(strlst[3])) ;
     result := true;
  end;
  strlst.free;
end;

procedure IniRGBA(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: TRGBA);
//read or write an integer value to the initialization file
var
	lStr: string;
begin
  if not lRead then begin
    //lI64 := lValue.rgbred + lValue.rgbGreen shl 8 + lValue.rgbBlue shl 16 + lValue.rgbReserved shl 24;
    //lIniFile.WriteString('RGBA',lIdent,InttoStr(lI64));
    lIniFile.WriteString('RGBA255',lIdent,RGBAToStr(lValue));
    exit;
  end;
	lStr := lIniFile.ReadString('RGBA255',lIdent, '');
  StrToRGBA(lStr,lValue);
end; //IniRGBA

procedure IniStr(lRead: boolean; lIniFile: TIniFile; lIdent: string; var lValue: string);
//read or write a string value to the initialization file
begin
  if not lRead then begin
    lIniFile.WriteString('STR',lIdent,lValue);
    exit;
  end;
  //lValue := lIniFile.ReadString('STR',lIdent, '');
  lValue := lIniFile.ReadString('STR',lIdent, lValue);
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

{$IFDEF DARWIN}
function SharedSupportFolder: ansistring;
var
   path: NSString;
begin
   path := NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, true).lastObject;
   path := path.stringByAppendingPathComponent(NSBundle.mainBundle.bundlePath.lastPathComponent.stringByDeletingPathExtension);
   NSFileManager.defaultManager.createDirectoryAtPath_withIntermediateDirectories_attributes_error(path, false, nil, nil);
   result := path.UTF8String+PathDelim;
end;

function IniName: string;
begin
  result := SharedSupportFolder;
  if result = '' then exit;
  result := result +ChangeFileExt(ExtractFileName(ParamStr(0)),'')+'.ini';
end;

{$ELSE}
function IniName: string;
begin
  result := GetUserDir;
  if result = '' then exit;
  result := result + '.'+ChangeFileExt(ExtractFileName(ParamStr(0)),'')+'12.ini';
end;
{$ENDIF}

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
  IniStr(lRead, lIniFile, 'AfniDir', lPrefs.AfniDir);
  IniStr(lRead, lIniFile, 'FSLDIR', lPrefs.FSLDir);
  IniStr(lRead, lIniFile, 'PrevBackgroundImage', lPrefs.PrevBackgroundImage  );
  IniStr(lRead, lIniFile, 'DicomDir', lPrefs.DicomDir  );
  if (lRead) and (not DirectoryExists(lPrefs.DicomDir)) then
  	lPrefs.DicomDir := '';
  IniStr(lRead, lIniFile, 'CustomDcm2niixExe', lPrefs.CustomDcm2niix);
  //Ints
  IniInt(lRead,lIniFile, 'ClusterNeighborMethod', lPrefs.ClusterNeighborMethod);
  IniInt(lRead,lIniFile, 'Quality1to5', lPrefs.Quality1to5);
  lPrefs.Quality1to5 := max(0, lPrefs.Quality1to5);
  lPrefs.Quality1to5 := min(5, lPrefs.Quality1to5);
  IniInt(lRead,lIniFile, 'ColorbarPosition',lPrefs.ColorBarPosition);
  IniInt(lRead,lIniFile, 'ColorbarSize', lPrefs.ColorbarSize);
  IniInt(lRead,lIniFile, 'BitmapZoom', lPrefs.BitmapZoom);
  //IniInt(lRead,lIniFile, 'DisplayOrient', lPrefs.DisplayOrient);
  IniInt(lRead,lIniFile, 'StartupDisplayOrient', lPrefs.StartupDisplayOrient);
  IniInt(lRead,lIniFile, 'MaxTexMb', lPrefs.MaxTexMb);
  IniInt(lRead,lIniFile, 'LineWidth', lPrefs.LineWidth);
  IniInt(lRead,lIniFile, 'StartupWindowMode', lPrefs.StartupWindowMode);
  IniInt(lRead,lIniFile, 'AnimationIntervalMsec', lPrefs.AnimationIntervalMsec);
  IniInt(lRead,lIniFile, 'GradientMode_Fast0_Slow3',lPrefs.GradientMode);
  IniInt(lRead,lIniFile, 'MultiSample124',lPrefs.MultiSample124);
  IniInt(lRead,lIniFile, 'VolumeSaveFormat',lPrefs.VolumeSaveFormat);
  IniInt(lRead,lIniFile, 'VoiSaveFormat',lPrefs.VoiSaveFormat);
  {$IFDEF LCLCocoa}
        IniBool(lRead,lIniFile, 'RetinaDisplay',lPrefs.RetinaDisplay);
        IniBool(lRead,lIniFile, 'DarkMode',lPrefs.DarkMode);
    {$ENDIF}
  IniBool(lRead,lIniFile, 'RenderDepthPicker',lPrefs.RenderDepthPicker);
  //IniBool(lRead,lIniFile, 'MultiSample',lPrefs.MultiSample);
  IniBool(lRead,lIniFile, 'ColorbarVisible',lPrefs.ColorbarVisible);
  IniBool(lRead,lIniFile, 'RulerVisible',lPrefs.RulerVisible);
  IniBool(lRead,lIniFile, 'Smooth2D',lPrefs.Smooth2D);
  IniBool(lRead,lIniFile, 'LoadSmoothOverlays',lPrefs.LoadSmooth);
  {$ifdef windows}
  IniBool(lRead,lIniFile, 'DebugMode',lPrefs.DebugMode);
  {$endif}
  IniBool(lRead,lIniFile, 'LabelOrient',lPrefs.LabelOrient);
  //LabelOrientCube
  IniBool(lRead,lIniFile, 'ScreenCaptureTransparentBackground',lPrefs.ScreenCaptureTransparentBackground);
  IniBool(lRead,lIniFile, 'LoadFewVolumes',lPrefs.LoadFewVolumes);
  IniBool(lRead,lIniFile, 'LandmarkPanel',lPrefs.LandmarkPanel);
  IniBool(lRead,lIniFile, 'FlipYZ',lPrefs.FlipYZ);
  IniBool(lRead,lIniFile, 'FlipLR_Radiological',lPrefs.FlipLR_Radiological);
  IniRGBA(lRead,lIniFile, 'LineColor',lPrefs.LineColor);

  IniBool(lRead,lIniFile, 'AutoClusterizeAtlases', lPrefs.AutoClusterizeAtlases);
  lIniFile.Free;
end;

end.
