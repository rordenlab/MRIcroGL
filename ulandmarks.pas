unit ulandmarks;

interface

uses
  sysutils,Classes, dialogs;//, SimdTypes;

type
  TLndmrk = record
    Name: string;
    X,Y,Z: single;
  end;
  TLandmarkRA = array of TLndmrk;
TLandmark = Class(TObject)
    private

    protected
    public
    	Landmarks: TLandmarkRA;
        Filename: string;
    	constructor Create();
    	destructor Destroy; override;
    	procedure Open(lFilename: string);
        procedure UpdateCoord(idx: integer; Xmm, Ymm, Zmm: single);
    	procedure Close;

    published
  end;

implementation

procedure TLandmark.UpdateCoord(idx: integer; Xmm, Ymm, Zmm: single);
//var
//  X,Y,Z: integer;
begin
  if (idx < 0) or (idx >= Length(Landmarks)) then
    exit;
  Landmarks[idx].X := Xmm;
  Landmarks[idx].Y := Ymm;
  Landmarks[idx].Z := Zmm;
end;

procedure TLandmark.Close;
begin
  Landmarks := nil;
end;

constructor TLandmark.Create();
begin
  Landmarks := nil;
  Filename :='';
end;

destructor TLandmark.Destroy;
begin
  Landmarks := nil;
  inherited;
end;

function NextTab(lStr: string; var lP: integer): string;
//reports text prior to comma...
var
 len: integer;
begin
  result := '';
  len := length(lStr);
  if len < lP then exit;
  repeat
    if (lStr[lP] = chr(9){','})   then begin
      lP := lP + 1;
      exit;
    end;
    //if lStr[lP] <> ' ' then
      result := result + lStr[lP];
    lP := lP + 1;
  until (lP > len);
end;

procedure TLandmark.Open(lFilename: string);
var
  st: string;
   sl: TStringList;
  n, line, col : integer;
begin
  if not Fileexists(lFilename) then begin
    Close;
    exit;
  end;
  Filename := lFilename;
   //will load the TAB delimited TXT here
   sl := TStringList.Create;
   try
     //load the tab delimited txt file
     sl.LoadFromFile(lFilename) ;
     //for each tab delimited line
     n := 0;
     setlength(Landmarks,sl.Count);
     for line := 0 to sl.Count-1 do begin
       st := sl[line];
       col := 1;
       if (NextTab(st,col) <> '') and  (NextTab(st,col) <> '') and(NextTab(st,col) <> '') and(NextTab(st,col) <> '')  then begin
          inc(n);
          col := 1;
          Landmarks[line].Name := NextTab(st,col);
          Landmarks[line].X := strtofloat(NextTab(st,col));
          Landmarks[line].Y := strtofloat(NextTab(st,col));
          Landmarks[line].Z := strtofloat(NextTab(st,col));
       end;
     end;
     setlength(Landmarks,n);
   finally
     sl.Free;
   end;
end;

end.
