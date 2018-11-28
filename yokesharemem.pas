unit yokesharemem;
{$IFDEF FPC}{$mode delphi}{$H+}{$ENDIF}
interface
//http://community.freepascal.org:10000/docs-html/rtl/ipc/shmctl
// call  CreateSharedMem when an application is created and  CloseSharedMem when a program is closed
// along with NInstances, these functions return the number of concurrent instances.
// if a program crashes, the values may not be reset until the next reboot

uses
  {$IFDEF UNIX}
  BaseUnix, SysUtils, ipc, dialogs,
 {$ELSE}
     winmemmap,
 {$ENDIF}
 forms, classes;
 procedure CreateSharedMem (lApp: TComponent);
 procedure CloseSharedMem;
 function NInstances: integer; //returns number of instances
 procedure SetShareFloats2D(lXmm,lYmm,lZmm: single);
 procedure SetShareFloats3D(lAzimuth, lElevation: single);
 function GetShareFloats(out lXmm,lYmm,lZmm, lAzimuth, lElevation: single): boolean;

implementation

type
  TShareMem =  record
   Instances: integer;
   Xmm,Ymm,Zmm, Azimuth, Elevation: single;
  end;
  PIntBuffer = ^TShareMem;
var
   gShareIntBuf: PIntBuffer;
   gPrevShare : TShareMem;
   gYokeEnabled : boolean = false;

function NInstances: integer;
begin
      result := gShareIntBuf^.Instances;
end;

procedure SetShareFloats2D(lXmm,lYmm,lZmm: single);
begin
     if not gYokeEnabled then exit;
        gShareIntBuf^.Xmm := lXmm;
        gShareIntBuf^.Ymm := lYmm;
        gShareIntBuf^.Zmm := lZmm;
        gShareIntBuf^.Azimuth := gPrevShare.Azimuth;
        gShareIntBuf^.Elevation := gPrevShare.Elevation;
        gPrevShare :=  gShareIntBuf^;
end;

procedure SetShareFloats3D(lAzimuth, lElevation: single);
begin
     if not gYokeEnabled then exit;
  gShareIntBuf^.Xmm := gPrevShare.Xmm;
  gShareIntBuf^.Ymm := gPrevShare.Ymm;
  gShareIntBuf^.Zmm := gPrevShare.Zmm;
  gShareIntBuf^.Azimuth := lAzimuth;
  gShareIntBuf^.Elevation := lElevation;
  gPrevShare :=  gShareIntBuf^;
end;

function GetShareFloats(out lXmm,lYmm,lZmm, lAzimuth, lElevation: single): boolean;
begin
  result := false;
  if not gYokeEnabled then exit;
        lXmm := gShareIntBuf^.Xmm;
        lYmm := gShareIntBuf^.Ymm;
        lZmm := gShareIntBuf^.Zmm;
        lAzimuth := gShareIntBuf^.Azimuth;
        lElevation := gShareIntBuf^.Elevation;
        if (lXmm <> gPrevShare.Xmm) or (lYmm <> gPrevShare.Ymm) or (lZmm <> gPrevShare.Zmm) or (lAzimuth <> gPrevShare.Azimuth) or (lElevation <> gPrevShare.Elevation) then
            result := true;
        gPrevShare :=  gShareIntBuf^;
end;
{$IFNDEF UNIX} //Windows implementation
var
EMemMap : TEMemMap;

procedure CreateSharedMem (lApp: TComponent);
begin
  EMemMap:=TEMemMap.Create(lApp{Self});
  {$IFDEF WIN64}
  EMemMap.CreateMutex('MRICROMUTEXGL64');
  {$ELSE}
  EMemMap.CreateMutex('MRICROMUTEXGL32');
  {$ENDIF}
  //If true then begin// NOT EMemMap.MapExisting('MRICROMAP64',SizeOf(TShareMem)) then begin
  gPrevShare.Xmm:=0;
  gPrevShare.Ymm:=0;
  gPrevShare.Zmm:=0;
  gPrevShare.Azimuth:=0;
  gPrevShare.Elevation:=0;
  gPrevShare.Instances:=0;  //not tracked in Windows
  {$IFDEF WIN64}
  If NOT EMemMap.CreateMemMap('MRICROMAPGL64',SizeOf(TShareMem),gPrevShare) then
  {$ELSE}
  If NOT EMemMap.CreateMemMap('MRICROMAPGL32',SizeOf(TShareMem),gPrevShare) then ;
  {$ENDIF}
     EMemMap.RaiseMappingException;
  //Not sure if windows 32 and 64 can share memory... pointer sizes differ?
  //EMemMap.CreateMemMap('MRICROMAPGL64',SizeOf(TShareMem),gPrevShare);
  gShareIntBuf := PINtBuffer(EMemMap.MemMap);
  gYokeEnabled := true;
  //end else
  //    gShareIntBuf^.Instances := gShareIntBuf^.Instances + 1;
end;

procedure CloseSharedMem;
begin
  if not gYokeEnabled then exit;
    EMemMap.Free;
end;

{$ELSE}
var
 fshmid: longint;
 segptr  : Pointer;

procedure CreateSharedMem (lApp: TComponent);
 var
    pth : string;
    p : Pchar;
    key : Tkey;
    new: boolean;
    const ftokpath = './mricrogl'#0;  //'.'#0 './udp2ser4'  http://free-pascal-general.1045716.n5.nabble.com/Sharing-memory-between-applications-td2819146.html
 begin
   //gYokeEnabled := false;
   pth := GetUserDir +'.'+ChangeFileExt(ExtractFileName(ParamStr(0)),'')+'12.ini';
   if FileExists(pth) then begin //preferred: ini file is different for each users, good for multiuser system
     p:=StrAlloc (length(pth)+1);
     StrPCopy (p,pth);
     key := ftok (p,ord('S'));
     StrDispose(P);
   end else
       key := ftok (pchar(@ftokpath[1]),ord('S'));
  fshmid := shmget(key,SizeOf(TShareMem),IPC_CREAT or IPC_EXCL or 438);
  new := true;
   If fshmid=-1 then begin
    //showmessage('Loading existing memory.');
    new := false;
    fshmid := shmget(key,SizeOf(TShareMem){segsize},0);
    If fshmid = -1 then begin
      showmessage ('Shared memory error (yoking disabled, may require a restart): '+inttostr(fpgeterrno));
      segptr:= nil;
      exit;//halt(1);
    end
  end;
  segptr:=shmat(fshmid,nil,0);
  gShareIntBuf := segptr;
  if new then
     gShareIntBuf^.Instances := 1
  else
      gShareIntBuf^.Instances :=gShareIntBuf^.Instances + 1;
 gYokeEnabled := true;
 //result :=  gShareIntBuf^.Instances;  //returns number of instances after including this one...
end;

procedure CloseSharedMem;
var
 nInstances : integer; // number of instances after this application quits
begin
 if not gYokeEnabled then exit;
  gShareIntBuf^.Instances := gShareIntBuf^.Instances -1;
  nInstances := gShareIntBuf^.Instances;
  if Assigned (segptr) then
    shmdt (segptr);
  if nInstances < 1 then begin //last running instance - close shared memory
    if shmctl (FShmId, IPC_RMID, nil) = -1 then
      Showmessage('unable to release shared memory');
  end;
end;
{$ENDIF}
end.


