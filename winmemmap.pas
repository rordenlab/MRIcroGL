unit winmemmap;
{$IFDEF FPC}{$mode delphi}{$H+}{$ENDIF}
interface
{ This Unit implements an interface to Win32 memory mapped files. It
  can be used to map data simply residing in memory or data residing
  in a file. The data can be fully mapped into the processes address
  space or chunks can be mapped. It also provides capabilities for
  processes to synchronize via mutexes. When mapping sections of the
  memory, you must be aware that the Win32 memory mapped file interface
  requires that when you are requesting an offset into the memory
  region, this offset must be a multiple of the system's memory
  allocation granularity (I've called it PageSize). At this point
  it is 64K. This is not a concern when you are mapping anything less
  than 64K. However, to map anything > 64K the total memory size
  mapped must be a multiple of 64K or you will not have access to
  the memorysize MOD 64K bytes left over. Basically there are five
  rules to be successful when using these routines:
           1. Mapname must be unique for each different case you use
              these objects (MyMap1 for case 1, MyMap2 for case 2
              etc.).However, each process using the same memory map
              MUST use the same MapName.
           2. Call MapExisting before CreateMemMap or FCreateMemMap.
              If another process has already started the mapping,
              all you want to do is map to the existing map. ie.
              If NOT MapExisting then CreateMemMap.
           3. If your processes are going to write to the mapped
              memory, it is suggested you use the mutex stuff.
           4. Pay heed to the warning above concerning seeking
              offsets into the mapped memory. Whenever you call
              the seek function, always check for an error. Errors
              in mapping to the file will result in the Memmap
              pointer being Nil.
           5. You MUST call LeaveCriticalSection after calling
              EnterCriticalSection or you will lock other processes wishing
              to use the map into an infinite wait state. Always use
              a Try..Finally block.
}
Uses
   Classes,Windows;
Const
 {$IFDEF WIN64} // magic constants... http://www.viva64.com/en/b/0022/
     //hMemMap = $FFFFFFFFFFFFFFFF;
     hMemMap : UINT64 = high(UINT64);
 {$ELSE}
    hMemMap = $FFFFFFFF;
 {$ENDIF}
Type
   //Map to memory
   TEMemMap = Class(TComponent)
   Private
      FhFile          : THandle;   //File handle, hMemMap when simple memory
      FhMap           : THandle;   //Mapping handle
      FMap            : Pointer;   //Memory Pointer
      FMapSize        : Cardinal;  //Mapping Page Size
      FMemSize        : Cardinal;  //Maximum size allocated, >=FileSize when a file
      FPageSize       : Cardinal;  //Minimum System allocation size
      FMaxSeeks       : Cardinal;  //Maximum seeks available,(FMemSize DIV PageSize)-1
      FMapError       : Integer;   //Error returned
      FhMutex         : THandle;   //Mutex handle for sharing
      FInMutex        : Boolean;   //Internal flag
      Function SetMapError : Boolean;
      Procedure SetMemSize(Size : Cardinal);
   Public
      Constructor Create(Aowner : TComponent); Override;
      Destructor Destroy; Override;
      //Create a mutex for sychronizing access
      Function CreateMutex(Const MutexName : String) : Boolean;
      //Use the mutex
      Procedure EnterCriticalSection;
      //Release the mutex
      Procedure LeaveCriticalSection;
      //Map to existing memory map
      Function MapExisting(Const MapName : String;
                           Const MapSize : Cardinal) : Boolean;Virtual;
      //Create a new memory map
      Function CreateMemMap(Const MapName : String;
                            Const MapSize : Cardinal;
                            Const MapData ) : Boolean;Virtual;
      //seek to an offset in the memory map
      Function Seek(Const OffSet : Cardinal) : Boolean;
      //duh?
      Procedure RaiseMappingException;Virtual;

      Property MemMap     : Pointer  Read FMap; //The mapped memory
      Property MapError   : Integer  Read FMapError Write FMapError;
      Property MemSize    : Cardinal Read FMemSize  Write SetMemSize; //Memory size to allocate
      Property PageSize : Cardinal Read FPageSize; //system returned page size
      Property MaxSeeks : Cardinal Read FMaxSeeks; //maximum seeks allowed
   end;
   //map to a file
   TEFileMap = Class(TEMemMap)
   Public
      Function FCreateMemMap(Const Filename : String;
                             Const MapName  : String;
                             Const MapSize  : Cardinal) : Boolean;

      Function FlushFileView : Boolean;
   end;
implementation
Uses
    SysUtils;
Type
   EMappingException = class(Exception);
Constructor TEMemMap.Create(AOwner : TComponent);
Var
    SysInfo : TSystemInfo;
begin
  Inherited Create(AOwner);
  FhFile:=hMemMap;
  GetSystemInfo(SysInfo);
  FPageSize:=SysInfo.dwAllocationGranularity;
end;
Destructor TEMemmap.Destroy;
begin
  LeaveCriticalSection;
  If FhMutex<>0 then
    CloseHandle(FhMutex);
  If FMap<>Nil then
    UnMapViewOfFile(FMap);
  If FHMap<>0 then
    CloseHandle(FHMap);
  Inherited Destroy;
end;
Function TEMemMap.CreateMutex(Const MutexName : String) : Boolean;
begin
  If FhMutex=0 then
    FhMutex:=Windows.CreateMutex(Nil,False,PChar(MutexName));
  If FhMutex=0 then
    Result:=SetMapError
  else
    Result:=True;
end;
Procedure TEMemMap.EnterCriticalSection;
begin
  If (NOT FInMutex) AND (FhMutex>0) then
  begin
    WaitForSingleObject(FhMutex,INFINITE);
    FInMutex:=True;
  end;
end;
Procedure TEMemMap.LeaveCriticalSection;
begin
  If FInMutex AND (FhMutex>0) then
  begin
    ReleaseMutex(FhMutex);
    FInMutex:=False;
  end;
end;
Function TEMemMap.SetMapError : Boolean;
begin
  FMapError:=GetLastError;
  Result:=False;
end;
Procedure TEMemMap.RaiseMappingException;
Var
    TError : Integer;
begin
  If FMapError<>0 then
  begin
    LeaveCriticalSection;
    TError:=FMapError;
    FMapError:=0;
    Raise EMappingException.Create('Memory Mapping Error #'+IntToStr(TError));
  end;
end;
Procedure TEMemMap.SetMemSize(Size : Cardinal);
begin
  FMemSize:=Size;
  If FMemSize>PageSize then
    FMaxSeeks:=(FMemSize DIV PageSize)-1
  else
    FMaxSeeks:=0;
end;
//map to an existing memory map described by MapName
Function TEMemMap.MapExisting(Const MapName : String;
                              Const MapSize : Cardinal) : Boolean;
begin
  FMapSize:=MapSize;
  FMap:=Nil;
  FhMap:=OpenFileMapping(FILE_MAP_WRITE,BOOL(True),PChar(MapName));
  If FhMap<>0 then
  begin
    FMap:=MapViewOfFile(FhMap,FILE_MAP_WRITE,0,0,MapSize);
    If FMap=Nil then
    begin
      CloseHandle(FHMap);
      FHMap:=0;
      SetMapError;
    end;
  end;
  Result:=FMap<>Nil;
end;
//Create a new memory mapping
Function TEMemMap.CreateMemMap(Const MapName : String;
                               Const MapSize : Cardinal;
                               Const MapData ) : Boolean;
begin
  If FMemSize=0 then
    FMemSize:=MapSize;
  FhMap:=CreateFileMapping(FhFile,nil,PAGE_READWRITE,0,FMemSize,PChar(MapName));
  If FhMap<>0 then
  begin
    FMap:=MapViewOfFile(FhMap,FILE_MAP_WRITE,0,0,MapSize);
    If FMap<>Nil then
    begin
      If fHFile=hMemMap then
      begin
        EnterCriticalSection;
        Try
          Move(MapData,FMap^,MapSize);
        Finally
          LeaveCriticalSection;
        end;
      end;
      Result:=True;
    end
    else
      Result:=SetMapError;
  end
  else
    Result:=SetMapError;
end;
//seek to a different position in map (0..MaxSeeks)
Function TEMemMap.Seek(Const OffSet : Cardinal) : Boolean;
begin
  Result:=True;
  If NOT UnMapViewOfFile(FMap) then
    Result:=SetMapError
  else
  begin
    FMap:=MapViewOfFile(FhMap,FILE_MAP_WRITE,0,OffSet*PageSize,FMapSize);
    If FMap=Nil then
      Result:=SetMapError;
  end;
end;
//Create a file mapping
Function TEFileMap.FCreateMemMap(Const Filename : String;
                                 Const MapName  : String;
                                 Const MapSize  : Cardinal) : Boolean;
Var
    TInt  : Cardinal;
begin
  FHFile:=CreateFile(PChar(FileName),GENERIC_READ OR GENERIC_WRITE,
                     FILE_SHARE_READ OR FILE_SHARE_WRITE,NIl,OPEN_EXISTING,
                     FILE_FLAG_RANDOM_ACCESS,0);
  If FhFile<>0 then
  begin
    Try
      Result:=CreateMemMap(MapName,MapSize,TInt);
    Finally
      CloseHandle(FhFile);
    end;
  end
  else
    Result:=SetMapError;
end;

Function TEFileMap.FlushFileView : Boolean;
begin
  EnterCriticalSection;
  Try
    Result:=FlushViewOfFile(FMap,FMapSize) OR SetMapError;
  Finally
    LeaveCriticalSection;
  end;
end;
   {$IFDEF WIN64}
     //ERROR THIS LIBRARY DOES NOT SUPPORT WINDOWS 64
   {$ENDIF}

end.

