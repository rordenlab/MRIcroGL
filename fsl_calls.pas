unit fsl_calls;

{$IFDEF FPC}{$mode objfpc}{$H+} {$ENDIF}

interface
uses
  {$IFNDEF UNIX} SimdUtils,  FileUtil, windows,{$ENDIF}
  LResources, Dialogs, Classes, SysUtils,Process;

//procedure FSLcmd (lCmd: string);
function FSLbet (lFilename: string; lFrac: single; lOutname: string = ''): string;
function FSLmcflirt (lFilename4D: string): string;
function FSLmean(lFilename4D: string): string;
function FSLflirt (lFilenames: tstrings): string;
function GetFSLdir: string;

var gFSLbase: string;
implementation

uses dcm_load, strutils;

function ChangeFilePrefix(lInName,lPrefix: string): string;
begin
     result := extractfilepath(lInName)+lPrefix+extractfilename(lInName);
end;

function ChangeFileExtX( lFilename: string; lExt: string): string;
begin
     result := ChangeFileExt(ExtractFileName(lFilename),'');
     if upcase(ExtractFileExt(result))= '.NII' then
        result := ChangeFileExt(result,''); //img.nii.gz -> img
     result := ExtractFilePath(lFilename) + result + lExt;
end;

procedure msg(S: string);
begin
  showmessage(S);
end;

{$IFNDEF UNIX}
procedure riteln(S: string);
begin
  //showmessage(S);
end;

function GetFSLdir: string;
begin
    result := '';
end;


function FindDefaultExecutablePathX(const Executable: string): string;
begin
	result := FindDefaultExecutablePath(Executable);
	if fileexists(result) then exit;
	result := FindDefaultExecutablePath(ExtractFilePath  (paramstr(0)) +Executable);
	if fileexists(result) then exit;
	result := ResourceDir + pathdelim + Executable;
end;

procedure FSLcmd (lCmd: string);
const
  FSLOUTPUTTYPE = 'FSLOUTPUTTYPE=NIFTI_GZ';
 var
 AProcess: TProcess;
 i: integer;
   AStringList: TStringList;
   //PATH,FSLDIR,lS,FULL,FSLDIRBIN: string;
 begin
   AProcess := TProcess.Create(nil);
   AStringList := TStringList.Create;
   AProcess.Environment.Add(FSLOUTPUTTYPE);
   AProcess.CommandLine := lCmd;
   AProcess.Options := AProcess.Options + [poNoConsole,poWaitOnExit, poStderrToOutPut, poUsePipes];
   riteln(AProcess.CommandLine);
   {$IFDEF GUI}application.processmessages;{$ENDIF}
   AProcess.Execute;
   AStringList.LoadFromStream(AProcess.Output);
   if AStringList.Count > 0 then
      for i := 1 to AStringList.Count do
       riteln(AStringList.Strings[i-1]);
   AStringList.Free;
   AProcess.Free;
end;

function FSLbet (lFilename: string; lFrac: single; lOutname: string = ''): string;
const
  kExe = 'bet2.exe';
var
   lCmd, lExe,OutName: string;
begin
  lExe := FindDefaultExecutablePathX(kExe);
  if lExe = '' then begin
    msg('Unable to find executable '+kExe);
    exit;
  end;
  result := lOutname;
  if lOutname = '' then
     result := ChangeFilePrefix(lFilename,'b');
  result := ChangeFileExtX( result, '.nii.gz');  //e.g. .nii -> .nii.gz
  lCmd := lExe+' "'+lFilename+'" "'+result +'" -f '+floattostr(lFrac);
  FSLCmd (lCmd);
  //msg('FSL is only available for Unix');
end;

function FSLmcflirt (lFilename4D: string): string;
begin
  msg('FSL is only available for Unix');
end;

function FSLmean(lFilename4D: string): string;
begin
  msg('FSL is only available for Unix');
  result := '';
end;

function FSLflirt (lFilenames: tstrings): string;
begin
  msg('FSL is only available for Unix');
  result := '';
end;
{$ELSE}

procedure riteln(S: string);
begin
  writeln(S);
end;

(*function RunCommandX(exe,args: string; out outputstring: string): boolean;
//e.g.  RunCommandX('/bin/zsh',' -l -c "which afni"', s)
begin
     outputstring := '';
     if not FileExists(exe) then exit(false);
     result := RunCommand(exe+' '+args, outputstring);
end;*)

function GetFSLdir: string;
//const FSLBase = '/usr/local/fsl';
label 123;
begin
	result := gFSLBASE;
   	if (length(result) > 0) and (DirectoryExists(result)) then exit;
    {$IFDEF UNIX}
    result := GetEnvironmentVariable('FSLDIR');
    if DirectoryExists(result) then goto 123;
    result := GetEnvironmentVariable('FSL_DIR');
    //if DirectoryExists(result) then goto 123;
    //RunCommandX('/bin/zsh','-l -c "which fsl"', result);
    //if DirectoryExists(result) then goto 123;
    //RunCommandX('/bin/bash', '-l -c "which fsl"', result);
    {$ENDIF}
123:
    if not DirectoryExists(result) then begin
       result := '';
       {$IFDEF UNIX}
       writeln('Unable to find FSLDIR');
       {$ENDIF}
    end else begin
        {$IFDEF UNIX}
        writeln('Found FSLDIR: '+result);
        {$ENDIF}
    end;
    gFSLBASE := result;
end;

procedure FSLcmd (lCmd: string);
const
  FSLOUTPUTTYPE = 'FSLOUTPUTTYPE=NIFTI_GZ';
 var
 AProcess: TProcess;
 i: integer;
   AStringList: TStringList;
   PATH,FSLDIR,lS,FULL,FSLDIRBIN: string;
 begin
   AProcess := TProcess.Create(nil);
   AStringList := TStringList.Create;
   PATH:=GetEnvironmentVariable('PATH');
   FSLDIR := GetFSLdir;
   FSLDIRBIN :=  FSLDIR+'/bin' ;
   //if not (fileexists(FSLDIR)) then begin
   if not (directoryexists(FSLDIR)) then begin

      msg('Please install FSL, unable to find '+FSLDIR);
      exit;
   end;
    FULL :=   PATH+':'+FSLDIR+':'+FSLDIRBIN;
    lS := 'FSLDIR='+FSLDIR;
    AProcess.Environment.Add(lS);
    lS := 'LD_LIBRARY_PATH='+FSLDIRBIN;
    AProcess.Environment.Add(lS);
    lS := 'PATH='+FULL;
    AProcess.Environment.Add(lS);
    lS := 'FSLCLUSTER_MAILOPTS="n"';
    AProcess.Environment.Add(lS);
   AProcess.Environment.Add(FSLOUTPUTTYPE);
   AProcess.CommandLine := FSLDIRBIN+pathdelim+lCmd;
   AProcess.Options := AProcess.Options + [poWaitOnExit, poStderrToOutPut, poUsePipes];
   riteln(AProcess.CommandLine);
   {$IFDEF GUI}application.processmessages;{$ENDIF}
   AProcess.Execute;
   AStringList.LoadFromStream(AProcess.Output);
   if AStringList.Count > 0 then
      for i := 1 to AStringList.Count do
       riteln(AStringList.Strings[i-1]);
   AStringList.Free;
   AProcess.Free;
end;



function FSLbet (lFilename: string; lFrac: single; lOutname: string = ''): string;
var
   lCmd: string;
begin
    result := lOutname;
    if lOutname = '' then
       result := ChangeFilePrefix(lFilename,'b');
    {$IFDEF Darwin}
    if PosEx('.app', result) > 0 then begin
       result := extractfilename(result);
       result := HomeDir(false) + result;
    end;
    {$ENDIF}
    lCmd := 'bet "'+lFilename+'" "'+result +'" -S -B -R -f '+floattostr(lFrac);
    FSLCmd (lCmd);
end;

function FSLmean (lFilename4D: string): string;
var
   lCmd: string;
begin
    result := ChangeFilePrefix(lFilename4D,'w');
    lCmd := 'fslmaths  "'+lFilename4D+'" -Tmean "'+result +'"';
    FSLCmd (lCmd);
end;

function FSLmcflirt (lFilename4D: string): string;
var
   lCmd: string;
begin
    result := ChangeFilePrefix(lFilename4D,'r');
    lCmd := 'mcflirt -in  "'+lFilename4D+'" -o "'+result +'"  -cost mutualinfo';
    FSLCmd (lCmd);
end;

function FSLflirt (lFilenames: tstrings): string;
var
   lCmd,Ref,lFilename,Mat,Warped: string;
   i: integer;
begin
     lFilename := lFilenames[0];
     Ref := GetFSLdir+'/data/standard/MNI152_T1_2mm_brain';
     Mat := ChangeFileExtX(lFilename,'.mat');
     result := ChangeFilePrefix(lFilename,'w');

     lCmd := 'flirt -in "'+lFilename+'" -ref '+ref+' -out '+result+'  -omat "'+Mat+ '" -bins 256 -cost mutualinfo -searchrx -90 90 -searchry -90 90 -searchrz -90 90 -dof 12  -interp trilinear';

     FSLCmd (lCmd);
     if lFilenames.count <= 1 then
       exit;
     for i := 2 to (lFilenames.count) do begin
         lFilename := lFilenames[i-1];
         Warped := ChangeFilePrefix(lFilename,'w');
         lCmd := 'flirt -in "'+lFilename+'" -ref "'+Ref+'" -applyxfm -init "'+Mat+'" -out "'+Warped+'"';
         FSLCmd (lCmd);
     end;
end;

{$ENDIF}
initialization
  gFSLbase:= '/usr/local/fsl';
end.

