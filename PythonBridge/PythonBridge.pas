{$mode objfpc}
{$assertions on}
{$ifdef windows}
  {$define PYTHON_DYNAMIC}
{$endif}



unit PythonBridge;
interface
uses
  {$ifdef windows}windows,{$endif}{$ifdef lcl} dialogs, {$endif} Python3Core, SysUtils;

type
  TPythonBridgeMethod = record
    name: ansistring;
    callback: PyCFunction;
    help: ansistring;
  end;
  TPythonBridgeMethodArray = array[0..0] of TPythonBridgeMethod;
  PPythonBridgeMethodArray = ^TPythonBridgeMethodArray;

type
  TPythonModule = class
    private
      FModuleName : ansiString;
      FModule : PPyObject;
      FMethodCount : integer;
      FAllocatedMethodCount : integer;
      FMethods : PPyMethodDef;
      FModuleDef : PyModuleDef;

      procedure AllocMethods;
      procedure ReallocMethods;
    public
      constructor Create(name: ansistring);
      destructor Destroy; override;
      procedure Finalize;
      function AddMethod(AMethodName: PAnsiChar;
                         AMethod: PyCFunction;
                         ADocString: PAnsiChar): PPyMethodDef;
      property MethodCount: integer read FMethodCount;
      property MethodsData: PPyMethodDef read FMethods;
      property ModuleDef: PyModuleDef read FModuleDef;
  end;

type
  PythonDataMethodCallback = procedure (data: UnicodeString) of object; 

function PythonLoadAndInitialize(resourceDir: ansistring; callback: PythonDataMethodCallback): boolean;
function PythonInitialize(pythonHome: ansistring; callback: PythonDataMethodCallback): boolean;
function PythonAddModule(name: ansistring; methods: PPythonBridgeMethodArray; count: integer): TPythonModule;
function PyString_FromString( str: ansistring): PPyObject;
function PyObject_TypeCheck(obj : PPyObject; t : PPyTypeObject) : Boolean;
function PyTuple_Check( obj : PPyObject ) : Boolean;
function PyUnicode_Check( obj : PPyObject ) : Boolean;
procedure PyInterrupt(text: PAnsiChar = 'Operation cancelled');
function PyUnicode_AsUTF8(ob: PPyObject): AnsiString;

implementation

type
  TMethodArray = array[ 0 .. 16000 ] of PyMethodDef;
  PMethodArray = ^TMethodArray;
  EPythonError = class(Exception)
    public
      EName : String;
      EValue : String;
  end;
  EPyExecError   = class(EPythonError);

const
  PYTHON_API = 1013;

var
  DataMethodCallback: PythonDataMethodCallback = nil;

{$ifdef windows}
function findDll(resourceDir: string): string; //Detect .DLL version
var
  key: string;
  info : TSearchRec;
Begin
  result := '';
  key := resourceDir + pathdelim + 'python3*.dll';
  if FindFirst (key,faAnyFile,Info)=0 then
    result := resourceDir+pathdelim+ Info.name
  else
      Assert(FileExists(key), 'Unable to find required DLL: ' + key );
  FindClose(Info);
end;
{$endif}

{$ifdef windows}
Function  SetDllDirectory(lpPathName: LPCTSTR):LongBool; stdcall; external 'kernel32.dll' name 'SetDllDirectoryA'; //uses add windows,sysutils,
{$endif}

function PythonLoadAndInitialize(resourceDir: ansistring; callback: PythonDataMethodCallback): boolean;
var
  home: ansistring;
begin
  {$ifdef windows}
    SetDllDirectory(PChar(resourceDir));
    //home := resourceDir + pathdelim + 'vcruntime140.dll';
    //LoadLibrary(home, false);
    home := findDll(resourceDir);
    LoadLibrary(home);
    home := changefileext(home, '.zip');
    Assert(FileExists(home), 'Python home can''t be found at '+home);
    PythonInitialize(home, callback);
  {$else}
    home := resourceDir + pathdelim + 'python37';
    if not (DirectoryExists(home)) then begin
      {$ifdef lcl}
      ShowMessage('Unable to find Python home: '+home);
      {$else}
      writeln('Unable to find Python home: '+home);
      {$endif}
       exit(false);
    end;
    writeln('Python Home: ' + home);
    PythonInitialize(home, callback);
    Assert(DirectoryExists(home), 'Python home can''t be found at '+home);
    PythonInitialize(home, callback);
  {$endif}
  result := true;
end;

function PyUnicode_FromWideString(const AString : UnicodeString) : PPyObject;
{$IFDEF unix}
var
  _ucs4Str : UCS4String;
{$ENDIF}
begin
{$IFDEF unix}
  // Note that Linux uses UCS4 strings, whereas it declares using UCS2 strings!!!
  _ucs4Str := WideStringToUCS4String(AString);
  Result := PyUnicode_FromWideChar( {PWideChar}(@_ucs4Str[0]), Length(_ucs4Str)-1 {trim trailing zero});
{$ELSE}
  Result := PyUnicode_FromWideChar( PWideChar(AString), Length(AString) );
{$ENDIF}
end;

function PyUnicode_AsWideString( obj : PPyObject ) : UnicodeString;
var
  _size : Integer;
{$IFDEF unix}
  _ucs4Str : UCS4String;
{$ENDIF}
begin
    //_size := PySequence_Length(obj); //not fully correct for some Unicode
    _size := PyUnicode_GetSize(obj);
    if _size > 0 then
    begin
{$IFDEF unix}
      // Note that Linux uses UCS4 strings, whereas it declares using UCS2 strings!!!
      SetLength(_ucs4Str, _size+1);
      if PyUnicode_AsWideChar(obj, @_ucs4Str[0], _size) <> _size then
        raise EPythonError.Create('Could not copy the whole Unicode string into its buffer');
      Result := UCS4StringToWideString(_ucs4Str);
{$ELSE}
      SetLength(Result, _size);
      if PyUnicode_AsWideChar(obj, @Result[1], _size) <> _size then
        raise EPythonError.Create('Could not copy the whole Unicode string into its buffer');
{$ENDIF}
      // Clean line endings
      while (Length(Result) > 0) and (Result[Length(Result)] in [#0, CR, LF]) do
        Delete(Result, Length(Result), 1);
    end
    else
      Result := '';
end;

function PyString_FromString( str: ansistring): PPyObject;
var
  _text : UnicodeString;
begin
  _text := UnicodeString(str);
  result := PyUnicode_FromWideString(_text);
end;

procedure Py_INCREF(op: PPyObject);
begin
  Inc(op^.ob_refcnt);
end;

procedure Py_DECREF(op: PPyObject);
begin
  with op^ do begin
    Dec(ob_refcnt);
    if ob_refcnt = 0 then begin
      ob_type^.tp_dealloc(op);
    end;
  end;
end;

// https://docs.python.org/3.7/c-api/none.html#c.Py_RETURN_NONE
function ReturnNone: PPyObject;
begin
  result := Py_None;
  Py_INCREF(result);
end;

function pyio_write(self, args : PPyObject) : PPyObject; cdecl;
var
  a1: PPyObject;
  s: UnicodeString;
begin

  if Assigned(args) and (PyTuple_Size(args) > 0) then
    begin
      a1 := PyTuple_GetItem(args, 0);
      if Assigned(a1) then
        begin
          s := PyUnicode_AsWideString(a1);
          if Length(s) > 0 then
            DataMethodCallback(s)
          else
            DataMethodCallback(LF);
        end;
      Result := ReturnNone;
    end
  else
    begin
      PyErr_BadArgument;
      Result := nil;
    end;
end;

procedure PyInterrupt(text: PAnsiChar = 'Operation cancelled');
begin
//   procedure PyErr_SetString ( ErrorObject: PPyObject; text: PAnsiChar); cdecl; external;
//Py_None := PPyObject(@_Py_NoneStruct);
  PyErr_SetString( PPyObject(@PyExc_KeyboardInterrupt), text);
  //PyErr_SetString( PyExc_KeyboardInterrupt^, text) ;
end;

function PyObject_TypeCheck(obj : PPyObject; t : PPyTypeObject) : Boolean;
begin
  Result := Assigned(obj) and (obj^.ob_type = t);
  if not Result and Assigned(obj) and Assigned(t) then
    Result := PyType_IsSubtype(obj^.ob_type, t) = 1;
end;

function PyTuple_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, @PyTuple_Type);
end;

function PyUnicode_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, @PyUnicode_Type);
end;

function PyUnicode_AsUTF8(ob: PPyObject): AnsiString;
begin
  if PyUnicode_Check(ob) then
    Result := AnsiString(PyUnicode_AsWideString(ob))
  else
    Result := AnsiString(PyBytes_AsString(ob)); 
end;

procedure CheckError(ACatchStopEx : Boolean = False);
begin
  if PyErr_Occurred <> nil then
    begin
      if ACatchStopEx and (PyErr_GivenExceptionMatches(PyErr_Occurred(), PyExc_StopIteration^) <> 0) then
        begin
          PyErr_Clear;
          raise EPythonError.Create('Stop iteration');
        end
      else
        begin
          PyErr_Print;
          raise EPythonError.Create('Error');
        end;
    end;
end;

function TPythonModule.AddMethod( AMethodName  : PAnsiChar;
                                  AMethod  : PyCFunction;
                                  ADocString : PAnsiChar ) : PPyMethodDef;
begin
  Assert(FMethods <> nil);
  if FMethodCount = FAllocatedMethodCount then
    ReallocMethods;
  Result := @(PMethodArray(FMethods)^[MethodCount]);
  Result^.ml_name  := AMethodName;
  Result^.ml_meth  := AMethod;
  Result^.ml_flags := METH_VARARGS;
  Result^.ml_doc   := ADocString;
  Inc(FMethodCount);
end;

procedure TPythonModule.ReallocMethods;
var
  MethodPtr : PPyMethodDef;
begin
  Inc(FAllocatedMethodCount, PYT_METHOD_BUFFER_INCREASE);
  ReAllocMem(FMethods, SizeOf(PyMethodDef)*(FAllocatedMethodCount+1));
  MethodPtr :=@(PMethodArray(FMethods)^[MethodCount+1]);
  FillChar(MethodPtr^, SizeOf(PyMethodDef)*PYT_METHOD_BUFFER_INCREASE,0);
end;

procedure TPythonModule.AllocMethods;
begin
  Assert(FMethods = nil);
  FAllocatedMethodCount := PYT_METHOD_BUFFER_INCREASE;
  FMethodCount := 0;
  FMethods := PPyMethodDef(AllocMem(SizeOf(PyMethodDef)*(FAllocatedMethodCount+1)));
end;

procedure TPythonModule.Finalize; 
var
  modules: PPyObject;
begin
  FModuleDef.m_base.ob_refcnt := 1;
  FModuleDef.m_name := PAnsiChar(FModuleName);
  FModuleDef.m_methods := MethodsData;
  FModuleDef.m_size := -1;

  // https://docs.python.org/3.7/c-api/module.html
  FModule:= PyModule_Create2(@ModuleDef, PYTHON_API);
  if not Assigned(FModule) then
    CheckError;

  modules := PyImport_GetModuleDict();
  if PyDict_SetItemString(modules, ModuleDef.m_name, FModule) <> 0 then
    CheckError;
end;

constructor TPythonModule.Create(name: ansistring);
begin
  FModuleName := name;
  FillChar(FModuleDef, SizeOf(FModuleDef), 0);
  AllocMethods;
end;

destructor TPythonModule.Destroy;
begin
  FreeMem(FMethods);
  FMethods := nil;
  FMethodCount := -1;
end;

var
  pyio_module: TPythonModule = nil;

function PythonAddModule(name: ansistring; methods: PPythonBridgeMethodArray; count: integer): TPythonModule;
var
  i: integer;
begin
  result := TPythonModule.Create(name);
  for i := 0 to count - 1 do
    result.AddMethod(PAnsiChar(methods^[i].name), methods^[i].callback, PAnsiChar(methods^[i].help));
  result.Finalize;
end;

function RedirectIO: boolean;
var
  code: ansistring = 'import sys'+LF+
                     'class DebugOutput:'+LF+
                     '  pyio = __import__("pyio")'+LF+
                     '  softspace=0'+LF+
                     '  encoding=None'+LF+
                     '  def write(self,message):'+LF+
                     '     self.pyio.write(message)'+LF+
                     '  def flush(self):' + LF +
                     '     pass' + LF +
                     'sys.old_stdout=sys.stdout'+LF+
                     'sys.old_stderr=sys.stderr'+LF+
                     'sys.stdout=DebugOutput()'+LF+
                     'sys.stderr=DebugOutput()'+LF+
                     #0;
begin
  pyio_module := TPythonModule.Create('pyio');
  pyio_module.AddMethod('write', @pyio_write, 'write(String) -> None');
  pyio_module.Finalize;
  result := PyRun_SimpleString(PAnsiChar(code)) = 0;
end;

function PythonInitialize(pythonHome: ansistring; callback: PythonDataMethodCallback): boolean;
begin
  DataMethodCallback := callback;
  Py_SetPythonHome(Py_DecodeLocale(PAnsiChar(pythonHome), nil));
  //Py_SetPythonHome(PWideChar(pythonHome));
  Py_Initialize;
  result := RedirectIO;
end;

begin
  {$ifndef PYTHON_DYNAMIC}
  // note: these are macros which are pointers to structs when static linking               
  // https://stackoverflow.com/questions/15287590/why-should-py-increfpy-none-be-required-before-returning-py-none-in-c#15288194
  Py_None := PPyObject(@_Py_NoneStruct);
  Py_False := @_Py_FalseStruct;
  Py_True := @_Py_TrueStruct;
  {$endif}
end.
