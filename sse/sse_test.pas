program sse_test;
uses sse_testu;

procedure floatSSE;
const
  nvox = 8;	
var
  f: TFloat32s;
  b: TUInt8s;
  i: integer;
  mn,mx: single;
begin
	setlength(f, nvox);
	setlength(b, nvox);
	mn := 4.0;
	mx := 4.0;
	for i := 0 to (nvox-1) do 
		f[i] := i;
	flt2byte(f, b, mn, mx, 0);
	for i := 0 to (nvox-1) do 
		writeln(i, 'f', b[i]);
end;

procedure intSSE;
const
  nvox = 8;	
var
  f: TInt16s;
  b: TUInt8s;
  i: integer;
  mn,mx: single;
begin
	setlength(f, nvox);
	setlength(b, nvox);
	mn := 4.0;
	mx := 4.0;
	for i := 0 to (nvox-1) do 
		f[i] := i;
	int2byte(mn, mx, f, b, 0);
	for i := 0 to (nvox-1) do 
		writeln(i, 'i', b[i]);
end;

begin
  floatSSE;
  intSSE;
end.