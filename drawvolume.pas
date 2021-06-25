unit drawvolume;

{$mode Delphi}
{$H+} {$M+}
{$DEFINE DILATE}
interface

uses
  VectorMath, Classes, SysUtils, SimdUtils, dialogs;

const
 kDrawModeAppend = 0;
 kDrawModeDelete = 1;
 kDrawModeConstrain = 2;
 kDrawModeDeleteDark = 3;
 kDrawModeAddDark = 4;
 kDrawModeDeleteBright = 5;
 kDrawModeAddBright = 6;

Type
 TSlice2D = array of single;

Type
TDraw = Class //(TNIfTI)  // This is an actual class definition :
  // Internal class field definitions - only accessible in this unit
  private
     dim3d: array [0..3] of int64; //unused,X,Y,Z voxels in volume space
    dim2d: array [0..3] of int64; //orient,X,Y,Slice in slice space
    currSlice, prevSlice, prevOrient: int64;
    clickStartX, clickStartY,clickPrevX,clickPrevY: int64; //location of previous mouse buttons
    colorLut: TLUT;
    penColor : integer;
    fOpacityFrac: single;
    view3d, view2d, undo2d, modified2d: TUInt8s;
    //start2dOXYZ, prev2dOXYZ : array [0..3] of integer; //orient,X,Y,Slice for pixel coordinates
    doRedraw,doAlpha, isMouseDown, isModified, isModifiedSinceSave: boolean;
    procedure borderPixel (x,y: int64);
    procedure fillPixel (x,y: int64);
    procedure fillPixelBound (x,y: int64);
    function isFillNewColor (x,y: int64): boolean;
    procedure setColor (x,y: int64; clr: byte);
    procedure fillBubbles;
    procedure fillRegion;
    procedure doFloodFill(x,y: int64; newColor, oldColor: byte);
    procedure voiMouseFloodFill(Color, Orient: int64; Xfrac, Yfrac, Zfrac:  single);
    procedure drawPixel (x,y: int64);
    procedure DrawLine (x,y, x2, y2:  int64);
    procedure SmoothVol(lColor: int64; intenVol: TFloat32s);

  protected

  public
    //constructor Create();
    Filename: string;
    PenColorOnRelease: integer;
    property OpacityFraction: single read fOpacityFrac write fOpacityFrac;
    property ColorTable: TLUT read colorLUt;
    property VolRawBytes: TUInt8s read view3d;
    function voiActiveOrient: integer;
    property MouseDown: boolean read isMouseDown;
    property ActivePenColor: integer read penColor write penColor;
    property NeedsUpdate : boolean read isModified write isModified;
    property NeedsSave: boolean read isModifiedSinceSave write isModifiedSinceSave;
    function IsOpen: boolean;
    function voiDescriptives: int64;
    procedure PasteSlice2D(Orient: int64; var in2D, out3D: TUInt8s);
    procedure voiCloseSlice;
    procedure UpdateView3d;
    procedure voiColor (idx, r, g, b: byte);
    function voiIsEmpty: boolean;
    function voiIsOpen: boolean;
    procedure click2pix (var Xpix, Ypix: int64; Xfrac, Yfrac, Zfrac:  single);
    function setDim2D(Orient: int64): int64;
    procedure CopySlice2D(Orient: int64; var out2D: TUInt8s);
    procedure voiMouseDown(Color, Orient: int64; Xfrac, Yfrac, Zfrac:  single); overload;
    procedure voiMouseDown(Orient: int64; XYZfrac:  TVec3); overload;
    procedure voiMouseDown(Orient: int64; XYZfrac:  TVec3; isInvertPenColor: boolean); overload;
    procedure voiFloodFill(Orient: int64; XYZfrac:  TVec3); overload;
    procedure voiFloodFill(Orient: int64; XYZfrac:  TVec3; isInvertPenColor: boolean); overload;
    procedure voiPasteSlice(Xfrac, Yfrac, Zfrac:  single);
    procedure voiUndo;
    procedure voiMouseUp ( autoClose, overwriteColors: boolean);
    procedure preserveColors;
    function voiMouseMove (Xfrac, Yfrac, Zfrac:  single): boolean;
    function Dim: TVec3i;
    procedure voiCreate(X,Y,Z: int64; rawBytes: TUInt8s; binarize: boolean = false);
    procedure voiBinarize (Color: int64); //set all non-zero voxels to Color
    procedure voiInterpolate (Orient: int64);
    procedure voiInterpolateAllGaps(Orient: int64);
    procedure voiInterpolateCore(orient, zLoIn, zHiIn: int64);
    function CopySlice2DSingle(Orient: int64; out out2D: TSlice2D; lSmooth: boolean = true): boolean;
    procedure SmoothSlice(var slice: TSlice2D; Xdim, Ydim: int64);
    procedure voiClose;
    procedure voiSmoothIntensity();
    procedure voiDefaultLUT;
    procedure voiMorphologyFill(intenVol: TUInt8s; Color: int64; Xmm, Ymm, Zmm, Xfrac, Yfrac, Zfrac:  single; dxOrigin, radiusMM: int64; drawMode: int64);
    procedure voiIntensityFilter(intenVol: TUInt8s; Color: int64; rampAbove, rampBelow, drawMode: integer);
    procedure voiDilate(dilationInVoxels: single);
    function voiGetVolume: TUInt8s;
    procedure voiChangeAlpha (a: byte);
    procedure morphFill(volImg, volVoi: TUInt8s; Color: int64; Xmm,Ymm,Zmm: single; xOri, yOri, zOri, dxOrigin,  radiusMM : int64; drawMode: int64);
    constructor Create();//niftiFileName: string; tarMat: TMat4; tarDim: TVec3i; isInterpolate: boolean; out isOK: boolean); overload; //overlay
    destructor Destroy; override;
  published
end;

implementation

//uses mainunit;
{$IFDEF DILATE}
uses math;
{$ENDIF}

const
  kOrientSagRL = 8;
  kOrientSagLR = 4;
  kOrientCoro = 2;
  kOrientAx = 1;
  kOrient3D = 0;
  kFillNewColor = 255;//255;
  kIgnoreColor = 253;//253;
  kFillOldColor = 0;

{$IFDEF DILATE}
type
    FloatRA = array [0..32767] of single;
    FloatRAp = ^FloatRA;
procedure edt(var f: FloatRAp; var d,z: TFloat32s; var v: TInt32s; n: integer); inline;
function vx(p, q: integer): single; inline;
begin
	try
		result := ((f[q] + q*q) - (f[p] + p*p)) / (2.0*q - 2.0*p);
	except
		result := infinity;
	end;
        if isnan(result) then result := infinity;
end;
var
	p, k, q: integer;
	s, dx: single;
begin
    (*# Find the lower envelope of a sequence of parabolas.
    #   f...source data (returns the Y of the parabola vertex at X)
    #   d...destination data (final distance values are written here)
    #   z...temporary used to store X coords of parabola intersections
    #   v...temporary used to store X coords of parabola vertices
    #   i...resulting X coords of parabola vertices
    #   n...number of pixels in "f" to process
    # Always add the first pixel to the enveloping set since it is
    # obviously lower than all parabolas processed so far.*)
    k := 0;
    v[0] := 0;
    z[0] := -infinity;
    z[1] := infinity;
    for q := 1 to n-1 do begin
        (*# If the new parabola is lower than the right-most parabola in
        # the envelope, remove it from the envelope. To make this
        # determination, find the X coordinate of the intersection (s)
        # between the parabolas with vertices at (q,f[q]) and (p,f[p]).
        *)
        p := v[k];
        s := vx(p,q);
        while (s <= z[k]) and (k > 0) do begin
            k := k - 1;
            p := v[k];
            s := vx(p,q);
        end;
        //# Add the new parabola to the envelope.
        k := k + 1;
        v[k] := q;
        z[k] := s;
        z[k + 1] := infinity;
    end;
    (*# Go back through the parabolas in the envelope and evaluate them
    # in order to populate the distance values at each X coordinate.*)
    k := 0;
    for q := 0 to n-1 do begin
        while z[k + 1] < q do
            k := k + 1;
        dx := (q - v[k]);
        d[q] := dx * dx + f[v[k]];
        //i[q] = v[k];
    end;
    for q := 0 to n-1 do
    	f[q] := d[q];
end;
procedure edt1(var df: FloatRAp; n: integer); inline;

var
	q, prevX : integer;
	prevY, v : single;
begin
	prevX := 0;
	prevY := infinity;
	//forward
	for q := 0 to (n-1) do begin
		if (df[q] = 0) then begin
			prevX := q;
			prevY := 0;
		end else
			df[q] := sqr(q-prevX)+prevY;
	end;
	//reverse
	prevX := n;
	prevY := infinity;
	for q := (n-1) downto 0 do begin
		v := sqr(q-prevX)+prevY;
		if (df[q] < v) then begin
        	prevX := q;
        	prevY := df[q];
    	end else
        	df[q] := v;
    end
end;

procedure distanceFieldLR(var dim: TVec3i; var img: TFloat32s);
//filter data in the X dimension (Left/Right)
var
	s, r, cols: int64;
	f: FloatRAp;
begin
	cols := dim.x;
	s := dim.y * max(dim.z, 1);
	for r := 0 to (s-1) do begin
		f := @img[r*cols];
                edt1(f,cols);
	end;
end;

(*function hasZeros(var df: FloatRAp; n: integer): boolean;
var
   q: integer;
begin
     result := false;
     for q := 0 to (n-1) do
         if df[q] = 0 then
            exit(true);
end;

procedure report(var df: FloatRAp; n: integer);
var
   q: integer;
begin
     writeln(format('n=%d', [n]));
     writeln('m=[');
     for q := 0 to (n-1) do
         write(format('%g, ',[df[q]]));
     writeln('];');
end; *)


procedure distanceFieldAP(var dim: TVec3i; var img: TFloat32s);
//filter data in the Y dimension (Anterior/Posterior)
(*const
  	kn = 255;
	c: array [0..kn] of single =(Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, 144, 0, 0, 0, 0, 144, 144, 1, 1, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity, Infinity);
*)
var
  //j : integer;
  slices: int64;
  x,y, k, s, r, rows, cols: int64;
  f: FloatRAp;
  d,z, img2D : TFloat32s;
  v: TInt32s;
  //zeros: boolean;
begin
	cols := dim.y;
	rows := dim.x;
	setlength(z, cols+1);
	setlength(d, cols+1);
	setlength(v, cols+1);
	setlength(img2D, rows*cols);
	slices := 1;
	slices := slices * max(dim.z, 1);
	for s := 0 to (slices-1) do begin
		//transpose
		k := s * (rows * cols); //slice offset
		for x := 0 to (cols-1) do begin
			for y := 0 to (rows-1) do begin
				img2D[x+(y*cols)] := img[k];
				k := k + 1;
			end;
		end;
		for r := 0 to (rows-1) do begin
                    //for j := 0 to kn do
                    //    img2D[j] := c[j];
                    f := @img2D[r*cols];
                    (*zeros := hasZeros(f, cols);
                    if zeros then
                       report(f,cols); *)
		    edt(f, d, z, v, cols);
                    (*if zeros then begin
                       report(f,cols);
                       exit;
                    end;*)
                end;
		//transpose
		k := s * (rows * cols); //slice offset
		for x := 0 to (cols-1) do begin
			for y := 0 to (rows-1) do begin
				img[k] := img2D[x+(y*cols)];
				k := k + 1;
			end;
		end;
	end;
end;

procedure distanceFieldHF(var dim: TVec3i; var img: TFloat32s);
//filter data in the Z dimension (Head/Foot)
//by far the most computationally expensive pass
// unlike LR and AP, we must process 3rd (Z) and 4th (volume number) dimension separately
var
	sx, sxy, x,y,k, s, r, rows, cols: int64;
	f: FloatRAp;
	d,z, img2D : TFloat32s;
	v: TInt32s;
begin
	if (dim.z < 2) then exit; //2D images have height and width but not depth
	//we could transpose [3,2,1] or [3,1,2] - latter improves cache?
	cols := dim.z;
	rows := dim.x;
	sxy := dim.x * dim.y;
	setlength(z, cols+1);
	setlength(d, cols+1);
        setlength(v, cols+1);
	setlength(img2D, rows*cols);
	for s := 0 to (dim.y-1) do begin
		//transpose
		sx := (s * rows);
		k := 0; //slice offset along Y axis
		for x := 0 to (rows-1) do begin
			for y := 0 to (cols-1) do begin
				img2D[k] := img[x + sx + (y*sxy)];
				k := k + 1;
			end;
		end;
		for r := 0 to (rows-1) do begin
			f := @img2D[r*cols];
			edt(f, d, z, v, cols);
		end;
		//transpose
		k := 0; //slice offset along Y axis
		for x := 0 to (rows-1) do begin
			for y := 0 to (cols-1) do begin
				img[x + sx + (y*sxy)] := img2D[k];
				k := k + 1;
			end;
		end;
	end; //slice
end; //distanceFieldHF()

function distanceFieldVolume3D(var dim: TVec3i; var img32: TFloat32s): boolean;
var
   i, vx: int64;
begin
	result := false;
        if (dim.x < 1) or (dim.y < 1) or (dim.z < 1) then exit;
	vx := dim.x * dim.y * dim.z;
	distanceFieldLR(dim, img32);
	distanceFieldAP(dim, img32);
	distanceFieldHF(dim, img32);
	for i := 0 to (vx-1) do
	    img32[i] := sqrt(img32[i]) ;
	result := true;
end;

procedure TDraw.voiDilate(dilationInVoxels: single);
var
  nPix,  i : int64;
  dim: TVec3i;
  color :  byte;
  img32: TFloat32s;
  //f    : File;
begin
        color := min(max(1, penColor), 255);
        if (view3d = nil) and (Color = 0)  then exit; //nothing to erase
        if (dim3d[1] < 1) or (dim3d[2] < 1) or (dim3d[3] < 1) then exit;
        dim := pti(dim3d[1], dim3d[2], dim3d[3]);
        nPix := dim3d[1] * dim3d[2] * dim3d[3];
        //GLForm1.SliceBox.Caption := inttostr(Color)+'b'+ inttostr(nPix);
        voiCloseSlice;
        dim2d[0] := kOrient3D; //dim[0] = slice orientation 0 = 3d volume
        setlength(view2d, nPix);
        setlength(undo2d, nPix);
        Move(view3d[0], undo2d[0],nPix);//source/dest
        Move(undo2d[0],view2d[0],nPix);//source/dest
        setlength(img32, nPix);
        if (dilationInVoxels < 0) then begin
          dilationInVoxels := abs(dilationInVoxels);
          for i := 0 to (nPix -1) do begin
              if view2d[i] > 0 then
       		  img32[i] := infinity
       	      else
       		  img32[i] := 0;
          end;
          distanceFieldVolume3D(dim, img32);
          for i := 0 to (nPix -1) do begin
              if img32[i] > dilationInVoxels then
       		  view2d[i] := color
       	      else
       		  view2d[i] := 0;
          end;
        end else begin
            for i := 0 to (nPix -1) do begin
                if view2d[i] > 0 then
         		  img32[i] := 0
         	      else
         		  img32[i] := infinity;
            end;
            distanceFieldVolume3D(dim, img32);
            (*AssignFile(f, '/Users/chris/dx.img');
            ReWrite(f, 1);
            BlockWrite(f, img32[0], nPix * 4);   // Write 2 'records' of 4 bytes
            CloseFile(f); *)
            for i := 0 to (nPix -1) do begin
                //if img32[i] < dilationInVoxels then
         	if img32[i] < dilationInVoxels then
         		  view2d[i] := 1
         	      else
         		  view2d[i] := 0;
            end;
        end;
        img32 := nil;
        //       if (intenVol[i] > threshold) then view2d[i] := Color;
        doRedraw := true;
        UpdateView3d;
        isModified := true;
        isModifiedSinceSave := true;

end;

{$ELSE}
procedure TDraw.voiDilate(dilationInVoxels: single);
begin
     printf('voiDilate not supported');
end;
{$ENDIF}

function TDraw.Dim: TVec3i;
begin
     result := pti(dim3d[1], dim3d[2], dim3d[3]);
end;

destructor  TDraw.Destroy;
begin
     view3d := nil;
     view2d := nil;
     undo2d  := nil;
     modified2d := nil;
end;

procedure TDraw.voiClose;
begin
  view3d := nil;
  view2d := nil;
  undo2d  := nil;
  modified2d := nil;
  //isMouseDown:= false;
  isModified := true; // <- update on GPU
  voiCloseSlice;
end;

function TDraw.voiActiveOrient: integer;
begin
     if (view3d <> nil) and  (isMouseDown) then
          result := dim2d[0] //return 3D(0), Axial(1), Coronal(2) or Sagittal(3)
       else
         result := -1;
  end;

procedure TDraw.voiMouseDown(Orient: int64; XYZfrac:  TVec3); overload;
begin
     voiMouseDown(penColor, Orient, XYZfrac.X, XYZfrac.Y, XYZfrac.Z);
end;

procedure TDraw.voiMouseDown(Orient: int64; XYZfrac:  TVec3; isInvertPenColor: boolean); overload;
//https://bugs.freepascal.org/view.php?id=35480
begin
     if (isInvertPenColor) then begin
        if PenColorOnRelease <> 0 then
           voiMouseDown(0, Orient, XYZfrac.X, XYZfrac.Y, XYZfrac.Z)
        else
          voiMouseDown(1, Orient, XYZfrac.X, XYZfrac.Y, XYZfrac.Z);
     end else
         voiMouseDown(penColor, Orient, XYZfrac.X, XYZfrac.Y, XYZfrac.Z);
end;

function frac2pix (frac: single; dimPix: int64): int64;
begin
  result := round((frac * dimPix)-0.5);
  if (result < 0) then result := 0;
  if (result >= dimPix) then result := dimPix - 1;
end;


procedure SortInt (var lMin,lMax: int64); overload;
var
   lSwap: int64;
begin
     if lMin <= lMax then
        exit;
     lSwap := lMax;
     lMax := lMin;
     lMin := lSwap;
end;

procedure TDraw.voiChangeAlpha (a: byte);
var
  i: int64;
begin
      //alpha := a;
      doAlpha := true;
      colorLut[0].A := 0;
      for i := 1 to 255 do
        colorLut[i].A := a;
end;

function TDraw.voiGetVolume: TUInt8s;
begin
     result := view3d;
end;

function makeRGB(r,g,b: byte): TRGBA;
begin
    result.r := r;
    result.g := g;
    result.b := b;
end;

procedure TDraw.voiDefaultLUT;
var
  i:int64;
begin
    colorLut[0] := makeRGB(0,0,0);
    colorLut[1] := makeRGB(255,0,0);//red
    colorLut[2] := makeRGB(0,128,0);//green
    colorLut[3] := makeRGB(0,0,255);//blue
    colorLut[4] := makeRGB(255,128,0);//orange
    colorLut[5] := makeRGB(128,0,255);//purple
    colorLut[6] := makeRGB(0,200,200);//cyan
    colorLut[7] := makeRGB(160,48,48);//brick
    colorLut[8] := makeRGB(32,255,32);//lime
    colorLut[9] := makeRGB(128,160,230);//lightblue
    for i := 10 to 255 do
        colorLut[i] := colorLut[((i-1) mod 9)+1];
end;

(*procedure TDraw.SmoothSlice(var slice: TSlice2D; Xdim, Ydim: int64);
var
  x,y, pos: int64;
  sliceTemp : TSlice2D;
begin
     if (Xdim < 3) or (Ydim < 3) then exit;
     setlength(sliceTemp, Xdim * Ydim);
     //emulate 2D Gaussian blur: since function is separable, compute as two 1D filter
     //smooth in X dimension
     pos := 0;
     sliceTemp := Copy(slice, Low(slice), Length(slice));  //really clean, but unnecessary
     for x := 0 to (Xdim -1) do begin
         for y := 0 to (Ydim -1) do begin
             pos := pos + 1;
             if (x = 0) or (x = (Xdim-1)) then continue;
             sliceTemp[pos] := (slice[pos-1] + slice[pos] + slice[pos] + slice[pos+1]) * 0.25;
         end;
     end;
     //smooth in Y dimension
     pos := 0;
     for x := 0 to (Xdim -1) do begin
         for y := 0 to (Ydim -1) do begin
             pos := pos + 1;
             if (y = 0) or (y = (Ydim-1)) then continue;
             slice[pos] := (sliceTemp[pos-Ydim] + sliceTemp[pos] + sliceTemp[pos] + sliceTemp[pos+Ydim]) * 0.25;
         end;
     end;
     sliceTemp := nil; //free
end;*)

procedure TDraw.SmoothSlice(var slice: TSlice2D; Xdim, Ydim: int64);
var
  x,y, pos: int64;
  sliceTemp : TSlice2D;
begin
     if (Xdim < 3) or (Ydim < 3) then exit;
     setlength(sliceTemp, Xdim * Ydim);
     //emulate 2D Gaussian blur: since function is separable, compute as two 1D filter
     //smooth in X dimension
     pos := 0;
     sliceTemp := Copy(slice, Low(slice), Length(slice));  //really clean, but unnecessary
     for x := 0 to (Xdim -1) do begin
         for y := 0 to (Ydim -1) do begin
             pos := pos + 1;
             if (x = 0) or (x = (Xdim-1)) then continue;
             sliceTemp[pos] := (slice[pos-1] + slice[pos] + slice[pos] + slice[pos+1]) * 0.25;
         end;
     end;
     //smooth in Y dimension
     pos := 0;
     for x := 0 to (Xdim -1) do begin
         for y := 0 to (Ydim -1) do begin
             pos := pos + 1;
             if (y = 0) or (y = (Ydim-1)) then continue;
             //slice[pos] := (sliceTemp[pos-Ydim] + sliceTemp[pos] + sliceTemp[pos] + sliceTemp[pos+Ydim]) * 0.25;
             slice[pos] := (sliceTemp[pos-Xdim] + sliceTemp[pos] + sliceTemp[pos] + sliceTemp[pos+Xdim]) * 0.25;

         end;
     end;
     sliceTemp := nil; //free
end;

procedure SmoothImg(vol: TFloat32s; X,Y,Z: int64);
var
   vol2: TFloat32s;
   i, nPix, dimOff: int64;
begin
     if (X < 5) or (Y < 5) or (Z < 5) then exit;
     nPix := X * Y * Z;
     setlength(vol2, nPix * sizeof(single));
     Move(vol[0], vol2[0],nPix * sizeof(single));//source/dest -> only for edges
     for i := 2 to (nPix-1-2) do //smooth in X direction
         vol[i] := vol2[i-2] + 2*vol2[i-1] + 3* vol2[i] + 2* vol2[i+1] + vol2[i+2]; //x9 of original value
     dimOff := X;
     for i := 2*dimOff to (nPix-1-(2*dimOff)) do //smooth in Y direction
         vol2[i] := vol[i-2*dimOff] + 2*vol[i-dimOff] + 3* vol[i] + 2* vol[i+dimOff] + vol[i+2*dimOff]; //x9 of original value
     dimOff := X * Y;
     for i := 2*dimOff to (nPix-1-(2*dimOff)) do //smooth in Z direction
         vol[i] := vol2[i-2*dimOff] + 2*vol2[i-dimOff] + 3* vol2[i] + 2* vol2[i+dimOff] + vol2[i+2*dimOff]; //x9 of original value
     vol2 := nil;
end;

function maxVol (v: TFloat32s; lStart,lEnd: int64): single; //brightest and darkest
var
  i: int64;
begin
     result := v[0];
     for i := lStart to lEnd do
         if v[i] >= result then result := v[i];
end;

procedure meanStdInMask(mask, v: TFloat32s; lStart,lEnd: int64) ;
//modulate mask based on variance - in v
const
  kFrac = 0.7; //0..1 what proportion of signal modulatated by intensity
var
  mx: single;
  mean, stdev, delta,m2: double;
  i,n: int64;
begin
     //1.) determine max intensity in mask, e.g. if a binary 0/1 mask this will be 1
     mx := maxVol(mask,lStart,lEnd);
     if mx = 0 then exit;
     mx := 0.75 * mx;
     //calculate stdev and mean with Welford on pass methodhttp://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
     n := 0;
     mean := 0.0;
     m2 := 0.0;
     for i := lStart to lEnd do begin
         if (mask[i] >= mx) then begin
            n := n + 1;
            delta := v[i] - mean;
            mean := mean + (delta/n);
            m2 := m2 + (delta * (v[i] -mean)  );
         end;
     end;
     if (n < 2) then exit;
     stdev := sqrt(m2/(n-1)); //convert to standard deviation
     //GLForm1.Caption := inttostr(n)+' '+ floattostr(mean)+'  '+floattostr(stdev);
     for i := lStart to lEnd do begin
         delta := (v[i] - mean)/stdev; //z-score
         delta := abs(delta)/3; //e.g. Z= -1.5 -> 0.5
         if delta > kFrac then delta := kFrac;  //delta is clipped to 0..kFrac
         delta := 1-delta;
         //if (mask[i] > 0) then   mask[i] := 243 * delta; //purely drive by intensity
         mask[i] := mask[i] * (delta  + (1-kFrac) );
     end;
end;

function numThresh (v: TFloat32s; lStart,lEnd: int64; thresh: single): int64; //how many members of the array are >= thresh?
var
  i: int64;
begin
     result := 0;
     for i := lStart to lEnd do
         if v[i] >= thresh then inc(result);
end;

procedure TDraw.SmoothVol(lColor: int64; intenVol: TFloat32s);
label
  666;
var
  vol: TFloat32s;
  nPix,i,  origVol,
  tLo,tMid,tHi, nMid,t1stVox, tLastVox: int64;
begin
     //volume dimensions must be at least 5x5x5 voxels for smooth
     nPix := dim3d[1] * dim3d[2]*dim3d[3];
     setlength(vol, nPix * sizeof(single));
     origVol := 0;
     for i := 0 to (nPix-1) do
         if (view2d[i] = lColor) then begin
            vol[i] := 1;
            inc(origVol);
         end else
             vol[i] := 0;
     if (origVol < 1) then goto 666;
     SmoothImg(vol, dim3d[1], dim3d[2], dim3d[3]);
     t1stVox := -1;
     tLastVox := -1;
     for i := 0 to (nPix-1) do begin
         vol[i] := vol[i] * (1/3); //normalize from 0..729 to 0..243
         if vol[i] = 0.0 then begin
            tLastVox := i;
            if (t1stVox < 0) then t1stVox := i;
         end;
     end;
     if (intenVol <> nil) then
        meanStdInMask(vol, intenVol,  t1stVox,tLastVox) ;
     //find threshold that maintains overall volume
     tLo := 1;
     tMid := 121;
     tHi := 243;
     nMid := numThresh (vol, t1stVox,tLastVox , tMid);
     while (tHi-tLo) > 3 do begin
           if (nMid > origVol) then
              tLo := tMid
           else
               tHi := tMid;
           tMid := ((tHi-tLo) div 2) + tLo;
           nMid := numThresh (vol, t1stVox,tLastVox , tMid);
     end;
     for i := 0 to (nPix-1) do
         if (vol[i] >= tMid) then
                view2d[i] := lColor
         else
             view2d[i] := 0;
666:
     vol := nil;
end;

procedure TDraw.voiSmoothIntensity();
var
  nPix, dark, bright, i: int64;
begin
     if (view3d = nil) then exit;
     if (dim3d[1] < 5) or (dim3d[2] < 5) or (dim3d[3] < 5) then exit;
     nPix := dim3d[1] * dim3d[2]*dim3d[3];
     dark := 256;
     bright := 0;
     for i := 0 to (nPix -1) do begin
         if (view3d[i] < dark) then dark := view3d[i];
         if (view3d[i] > bright) then bright := view3d[i];
     end;
     if (bright = dark) then exit; //no variability
     voiCloseSlice;  //2015
     dim2d[0] := kOrient3D; //dim[0] = slice orientation 0 = 3d volume
     setlength(view2d, nPix);
     setlength(undo2d, nPix);
     Move(view3d[0], undo2d[0],nPix);//source/dest
     Move(undo2d[0],view2d[0],nPix);//source/dest
     if (dark = 0) then dark := 1;
     for i := dark to bright do
       SmoothVol(i, nil);

  doRedraw := true;
  UpdateView3d;
  isModified := true;
  isModifiedSinceSave := true;
end;

function TDraw.CopySlice2DSingle(Orient: int64; out out2D: TSlice2D; lSmooth: boolean = true): boolean;
var
   u8_2D: TUInt8s;
   nPix, i: int64;
begin
     nPix := dim2d[1]*dim2d[2];
     setlength(u8_2D, nPix);
     setlength(out2D, nPix);
     CopySlice2D(Orient,u8_2D);
     result := false;
     for i := 0 to (nPix -1) do
       if u8_2D[i] <> u8_2D[0] then
          result := true; //variability
     for i := 0 to (nPix -1) do
       out2D[i] := u8_2D[i];
     if lSmooth then
        SmoothSlice(out2D, dim2d[1], dim2d[2]);
     u8_2D := nil;
end;

procedure TDraw.voiInterpolateCore(orient, zLoIn, zHiIn: int64);
//interpolate voi between two 2D slices, slices indexed from 0
label
  666;
var
  xy, z, zLo, zHi, nSlices, nPix: int64;
  FracLo, FracHi: double;
  sliceLo, sliceHi: TSlice2D;
  outSlice: TUInt8s;
begin
     zLo := zLoIn;
     zHi := zHiIn;
     SortInt(zLo, zHi);
     nSlices := setDim2D(Orient);
     nPix := dim2d[1]*dim2d[2];
     if (zLo >= zHi) or (zLo < 0) or (zHi >= nSlices) or (nPix < 9) then exit;
     setlength(outSlice, nPix);
     setlength(sliceLo, 0);
     setLength(sliceHi, 0);
     //get bottom slice
     dim2d[3] :=zLo;
     if not CopySlice2DSingle(orient, sliceLo) then begin
        showmessage('Error: no variability in 1st slice '+inttostr(zLo));
        goto 666;
     end;
     //set top slice
     dim2d[3] :=zHi;
     if not CopySlice2DSingle(orient, sliceHi) then begin
        showmessage('Error: no variability in 2nd slice '+inttostr(zHi));
        goto 666;
     end;
     //estimate intensity at each voxel in between
     for z := (zLo+1) to (zHi -1) do begin
         fracHi := abs(z-zLo) / abs(zHi - zLo);//weighting for of lower slice
         fracLo := 1 - fracHi; //weighting for upper slice
         for xy := 0 to (nPix-1) do begin
             if ((sliceLo[xy]*fracLo) + (sliceHi[xy]*fracHi)) >= 0.375 then
                outSlice[xy] := 1
             else
                 outSlice[xy] := 0;
             //outSlice[xy] := 1;
         end; //each pixel in 2D slice
         dim2d[3] :=z;
         PasteSlice2D(Orient, outSlice, view2d);
     end;
666:
     outSlice := nil;
     sliceLo := nil;
     sliceHi := nil;
end;

procedure TDraw.voiInterpolateAllGaps(Orient: int64);
label
  666;
var
   zHi, zLo, i, z, nPix2D, nZ, nGaps: int64;
   isEmpty: array of boolean;
   slice2D: TUInt8s;
   s :string;
begin
     nZ := setDim2D(Orient);
     nPix2D := dim2d[1] * dim2d[2];
     setlength(slice2D,nPix2D);
     setlength(isEmpty, dim2d[3]);
     zHi := 0;
     zLo := nZ;
     for z := 0 to (nZ -1) do begin
         isEmpty[z] := true;
         dim2d[3] :=z;
         CopySlice2D(Orient,slice2D);
         for i := 0 to (nPix2D -1) do begin
             if slice2D[i] <> 0 then begin
                isEmpty[z] := false; //variability
                if (z < zLo) then zLo := z;
                zHi := z;
                break;
             end; //if slice has ROI in it
         end; //for each voxel in slice
         //if not isEmpty[z] then showmessage(inttostr(z));
     end; //for each slice
     if (zHi < 1) or (zLo >= zHi) then begin
        showmessage(format('No gaps in drawing slices. %d %d',[zLo, zHi]));
        goto 666; //nothing to do: all slices empty or only one slice filled
     end;
     //GLForm1.Caption := format('interpolate %d %d',[zLo, zHi]);
     nGaps := 0;
     while zLo < nZ do begin //for each slice, -2 since we need a gap
         if (isEmpty[zLo]) or (not isEmpty[zLo+1]) then begin //don't interpolate if current slice is empty or next slice not empty
            zLo := zLo + 1;
            continue;
         end;
         zHi := zLo + 1;
         while (isEmpty[zHi]) and (zHi < nZ) do
               zHi := zHi + 1;
         if zHi >=  nZ then break;
         inc(nGaps);
         voiInterpolateCore(Orient, zLo, zHi);
         //showmessage(format('interpolating %d %d',[zLo, zHi]));
         //look for next gap
         zLo := zHi;
     end;
     if nGaps = 0 then begin
        if Orient = kOrientCoro then
           s := 'Coronal'
        else if (Orient = kOrientSagRL) or (Orient = kOrientSagLR) then
           s := 'Sagittal'
        else
            s := 'Axial';
        showmessage('No gaps found in the '+s+' direction');
     end;
666:
    slice2D := nil;
  doRedraw := true;
  UpdateView3d;
  isModified := true;
  isModifiedSinceSave := true;
end;

procedure TDraw.voiInterpolate (Orient: int64);
//mode is one of these options:
//  kOrientSag = 3;
//  kOrientCoro = 2;
//  kOrientAx = 1;
//  kLast = 0; //last two slices
var
  nPix3D: int64;
begin
     if (view3d = nil)  then begin
        showmessage('No drawing to interpolate');
        exit; //nothing to erase
     end;
     if (Orient < kOrientAx) or (Orient > kOrientSagRL) then begin
        if (prevSlice < 0) or (prevSlice = currSlice) then begin
           showmessage('You need to draw on two different slices of the same orientation (Axial, Coronal, Sagittal)');
           exit;
        end;
     end;
     if (dim3d[1] < 5) or (dim3d[2] < 5) or (dim3d[3] < 1) then exit;
     nPix3D := dim3d[3] * dim3d[1] * dim3d[2];
     UpdateView3d;
     voiCloseSlice;
     dim2d[0] := kOrient3D; //dim[0] = slice orientation 0 = 3d volume
     setlength(view2d, nPix3D);
     setlength(undo2d, nPix3D);
     Move(view3d[0], undo2d[0],nPix3D);//source/dest
     Move(undo2d[0],view2d[0],nPix3D);//source/dest
     if (Orient < kOrientAx) or (Orient > kOrientSagRL) then
        voiInterpolateCore(prevOrient, prevSlice, currSlice)
     else
        voiInterpolateAllGaps(Orient);
     dim2d[0] := kOrient3D; //dim[0] = slice orientation 0 = 3d volume
     doRedraw := true;
     UpdateView3d;
     isModified := true;
     isModifiedSinceSave := true;
end;

procedure TDraw.voiBinarize (Color: int64); //set all non-zero voxels to Color
var
  nPix,  i: int64;
begin
     if (Color < 0) then exit;
     if (view3d = nil) and (Color = 0)  then exit; //nothing to erase
     if (dim3d[1] < 5) or (dim3d[2] < 5) or (dim3d[3] < 1) then exit;
     nPix := dim3d[1] * dim3d[2]*dim3d[3];
     UpdateView3d;
     voiCloseSlice;
     dim2d[0] := kOrient3D; //dim[0] = slice orientation 0 = 3d volume
     setlength(view2d, nPix);
     setlength(undo2d, nPix);
     Move(view3d[0], undo2d[0],nPix);//source/dest
     Move(undo2d[0],view2d[0],nPix);//source/dest
     for i := 0 to (nPix-1) do
            if view3d[i] <> 0 then
               view2d[i] := 1;
     doRedraw := true;
     UpdateView3d;
     isModified := true;
     isModifiedSinceSave := true;
end;

procedure TDraw.voiCreate(X,Y,Z: int64; rawBytes: TUInt8s; binarize: boolean = false);
var
  i, vx: int64;
begin
  isModified := true;
  isModifiedSinceSave := false;
  dim3d[0] := 3;
  dim3d[1] := X;
  dim3d[2] := Y;
  dim3d[3] := Z;
  prevSlice := -1;
  voiCloseSlice;
  view3d := nil;
  vx :=  dim3d[1] * dim3d[2] * dim3d[3];
  if (vx <1) or (dim3d[1] < 1) or (dim3d[2] < 1) or (dim3d[3] < 1) then  //requires 3D image
     exit;
  //isForceCreate := true;
  setlength(view3d, vx);
  if (rawBytes <> nil) then begin
     Move(rawBytes[0],view3d[0],vx);//source/dest
     if (binarize) then
        for i := 0 to (vx-1) do
          if view3d[i] <> 0 then
             view3d[i] := 1;
  end else
     FillChar(view3d[0],vx,0); //set all to zero
  doRedraw := true;
  UpdateView3d;
end; //voiCreate()

function TDraw.voiMouseMove (Xfrac, Yfrac, Zfrac:  single): boolean;
var
    clickX, clickY: int64;
begin
    result := false;
    if not isMouseDown then exit;
    click2pix (clickX, clickY, Xfrac, Yfrac, Zfrac);
    if (clickX = clickPrevX) and (clickY = clickPrevY) then exit;
    DrawLine (clickPrevX, clickPrevY, clickX, clickY);
    clickPrevX := clickX;
    clickPrevY := clickY;
    doRedraw := true;
    result := true;
        UpdateView3d;
        isModified := true;
        isModifiedSinceSave := true;
end;

procedure TDraw.preserveColors;
var
i: int64;
begin
	 for i := 0 to ((dim2d[1]*dim2d[2])-1) do
	   if (undo2d[i] <> 0) then
		  view2d[i] := undo2d[i];
end;

procedure TDraw.voiMouseUp ( autoClose, overwriteColors: boolean);
begin
	if (autoClose) then begin
	   DrawLine (clickPrevX, clickPrevY, clickStartX, clickStartY);
	   fillRegion;
	end;
	if (not overwriteColors) and (penColor <> 0) then
	   preserveColors;
	doRedraw := true;
	UpdateView3d;
	isMouseDown := false;
	isModified := true;
	isModifiedSinceSave := true;
end;

procedure TDraw.voiUndo;
var
temp: TUInt8s;
nPix: int64;
begin
   if ( view2d = nil) or ( undo2d = nil) then exit;
   if (dim2d[0] = kOrient3D) then  //3D volume
	  nPix := dim3d[1]* dim3d[2] * dim3d[3]
   else
		nPix := dim2d[1] * dim2d[2]; //dim[3] = number of pixels
   if (nPix < 1) then exit;
   //swap active and undo images, so multiple calls to 'Undo' will undo/redo last drawing...
   setlength(temp, nPix);
   Move(undo2d[0],temp[0],nPix);//source/dest
   Move(view2d[0],undo2d[0],nPix);//source/dest
   Move(temp[0],view2d[0],nPix);//source/dest
   temp := nil;
   doRedraw := true;
   isModified := true;
   isModifiedSinceSave := true;
   UpdateView3d;
end;

procedure TDraw.voiPasteSlice(Xfrac, Yfrac, Zfrac:  single);
begin
   if ( view2d = nil) or (dim2d[0] =  kOrient3D) then exit;
   if (dim2d[0] = kOrientSagRL) or (dim2d[0] = kOrientSagLR) then begin
	  dim2d[3] := frac2pix(Xfrac, dim3d[1]); //sag slices select Left-Right slices
   end else if (dim2d[0] =  kOrientCoro) then begin
	  dim2d[3] := frac2pix(Yfrac, dim3d[2]); //coro slices select Anterio-Posterior slices
   end else begin  //Axial
	   dim2d[3] := frac2pix(Zfrac, dim3d[3]); //axial influences head-foot
   end;
   doRedraw := true;
   UpdateView3d;
   isModified := true;
   isModifiedSinceSave := true;
end;

procedure TDraw.fillRegion;
var
  i: int64;
begin
     if (view2d = nil) or (modified2d = nil) then exit;
     if (dim2d[1] < 3) or (dim2d[2] < 3) then exit;
     fillBubbles;
     for i := 0 to ((dim2d[1]*dim2d[2])-1) do
         if (modified2d[i] = kFillOldColor) then
            view2d[i] := penColor;
end;

procedure TDraw.doFloodFill(x,y: int64; newColor, oldColor: byte);
//set all voxels connected to location x,y of oldColor to have newColor
var
   nPix, i: int64;
begin
     nPix := dim2d[1]*dim2d[2];
     for i := 0 to (nPix-1) do begin
         if view2d[i] = oldColor then
            modified2d[i] := kFillOldColor
         else
            modified2d[i] := kIgnoreColor;
     end;
     fillPixelBound (x,y);
     if newColor = oldColor then begin
        for i := 0 to (nPix-1) do begin
         if modified2d[i] = kFillNewColor then
            modified2d[i] := kIgnoreColor
         else
           modified2d[i] := kFillOldColor;
        end;
        fillBubbles; //any oldcolor connected to the border set to newcolor
        for i := 0 to (nPix-1) do begin
         if modified2d[i] = kFillOldColor then
           modified2d[i] := kFillNewColor  //fill pockets
        else
          modified2d[i] := kIgnoreColor;
         end;
     end;
     for i := 0 to (nPix-1) do
         if modified2d[i] = kFillNewColor then
            view2d[i] := newColor;
end;

procedure TDraw.voiMouseFloodFill(Color, Orient: int64; Xfrac, Yfrac, Zfrac:  single);
var
   x,y: int64;
   oldColor: byte;
begin
     voiMouseDown(Color, Orient, Xfrac, Yfrac, Zfrac);
     isMouseDown := false;
     if (view2d = nil) or (modified2d = nil) then exit;
     if (dim2d[1] < 3) or (dim2d[2] < 3) then exit;
     click2pix (x,  y, Xfrac, Yfrac, Zfrac);
     oldColor := view2d[x + y* dim2d[1]];
     //GLForm1.caption :=inttostr(oldColor)  +'->'+ inttostr(Color)+'xxxx'+inttostr(random(888));
     doFloodFill(x,y, Color, oldColor);
     doRedraw := true;
     UpdateView3d;
end;


procedure TDraw.voiFloodFill(Orient: int64; XYZfrac:  TVec3);
begin
    voiMouseFloodFill(penColor, Orient,  XYZfrac.X, XYZfrac.Y, XYZfrac.Z);
end;

procedure TDraw.voiFloodFill(Orient: int64; XYZfrac:  TVec3; isInvertPenColor: boolean); overload;
// https://bugs.freepascal.org/view.php?id=35480
begin
     if (isInvertPenColor) then begin
        if PenColorOnRelease <> 0 then
           voiMouseFloodFill(0, Orient,  XYZfrac.X, XYZfrac.Y, XYZfrac.Z)
        else
          voiMouseFloodFill(1, Orient,  XYZfrac.X, XYZfrac.Y, XYZfrac.Z);
     end else
         voiMouseFloodFill(penColor, Orient,  XYZfrac.X, XYZfrac.Y, XYZfrac.Z);



end;

procedure TDraw.drawPixel (x,y: int64);
var
  px : int64;
begin
     px := x + y* dim2d[1];
     modified2d[px] := kIgnoreColor;//1;
     view2d[px] := penColor;
end;

procedure TDraw.DrawLine (x,y, x2, y2:  int64);
//http://www.edepot.com/lineb.html
var
  yLonger: boolean;
  i,incrementVal, shortLen, longLen, swap: int64;
  multDiff: double;
begin
	drawPixel(x2,y2);
	yLonger:=false;
	shortLen:=y2-y;
	longLen:=x2-x;
	if (abs(shortLen)>abs(longLen)) then begin
		swap:=shortLen;
		shortLen:=longLen;
		longLen:=swap;
		yLonger:=true;
	end;
	if (longLen<0) then
		incrementVal:=-1
	else
		incrementVal:=1;
	if (longLen=0.0) then
		multDiff:=shortLen
	else
		multDiff:=shortLen/longLen;
	i := 0;
	if (yLonger) then begin
		while (i <> longLen) do begin
			drawPixel(round(x+(i*multDiff)),y+i);
			i := i +incrementVal;
		end;
	end else begin
		while (i <> longLen) do begin
	 		drawPixel(x+i,round(y+(i*multDiff)));
			i := i +incrementVal;
		end;
	end;
end; //DrawX

procedure TDraw.borderPixel (x,y: int64);
var
  px : int64;
begin
     px := x + y* dim2d[1];
     if modified2d[px] = kFillOldColor then
        modified2d[px] := kFillNewColor;
end;

{$IFDEF SIMPLEFILL}
//simple fill uses recursion can overwhelm the stack
procedure TDraw.fillPixel (x,y: int64);
var
   px : int64;
begin
     px := x + y* dim2d[1];
     if modified2d[px] <> kFillOldColor then exit;
     modified2d[px] := kFillNewColor;
     exit;
     fillPixel (x-1,y);
     fillPixel (x+1,y);
     fillPixel (x,y-1);
     fillPixel (x,y+1);
end;
{$ELSE}
//do not use simple fill: prevent overwhelming the stack
procedure TDraw.fillPixel (x,y: int64);
var
   px : integer;
   queue: TInt32s;
   qLo, qHi: integer;
procedure fillPix(p: integer);
begin
     if modified2d[p] <> kFillOldColor then exit;
     modified2d[p] := kFillNewColor;
     qHi := qHi + 1;
     queue[qHi] := p;
end;

procedure retirePixel();
var
   p: integer;
begin
     p := queue[qLo];
     fillPix (p-1);
     fillPix (p+1);
     fillPix (p-dim2d[1]);
     fillPix (p+dim2d[1]);
     qLo := qLo + 1;
end;

begin
     px := x + y* dim2d[1];
     if modified2d[px] <> kFillOldColor then exit;
     //modified2d[px] := kFillNewColor;
     setlength(queue, dim2d[1]*dim2d[2]);
     qLo := 0;
     qHi := -1;
     fillPix(px);
     while qLo <= qHi do
           retirePixel();

     //fillPixel (x-1,y);
     //fillPixel (x+1,y);
     //fillPixel (x,y-1);
     //fillPixel (x,y+1);

     queue := nil;
end;
{$ENDIF}

procedure TDraw.fillPixelBound (x,y: int64);
//fill pixel with range checking
var
   px : int64;
begin
       px := x + y* dim2d[1];
       if modified2d[px] <> kFillOldColor then exit;
       modified2d[px] := kFillNewColor;
       if (x > 0) then fillPixelBound (x-1,y);
       if (x < (dim2d[1]-1)) then fillPixelBound (x+1,y);
       if (y > 0) then fillPixelBound (x,y-1);
       if (y < (dim2d[2]-1)) then fillPixelBound (x,y+1);
end;

function TDraw.isFillNewColor (x,y: int64): boolean;
begin
       result := (modified2d[x + y* dim2d[1] ] = kFillNewColor);
end;

procedure TDraw.setColor (x,y: int64; clr: byte);
begin
       modified2d[x + y* dim2d[1]] := clr;
end;

procedure TDraw.fillBubbles;
//from borders identifies all connected voxels of kFillOldColor and makes them kFillNewColor
var
  i: int64;
begin
     if (view2d = nil) or (modified2d = nil) then exit;
     if (dim2d[1] < 3) or (dim2d[2] < 3) then exit;
     //blank left and right sides to prevent overflow errors
     for i := 0 to (dim2d[2]-1) do begin
         borderPixel(0,i);
         borderPixel(dim2d[1]-1,i);
     end;
     //blank top and bottom sides to prevent overflow errors
     for i := 0 to (dim2d[1]-1) do begin
         borderPixel(i,0);
         borderPixel(i,dim2d[2]-1);
     end;
     //seed left and right edges
    for i := 1 to (dim2d[2]-2) do begin
         if isFillNewColor(0,i) then fillPixel(1,i);
         if isFillNewColor(dim2d[1]-1,i) then fillPixel(dim2d[1]-2,i);
     end;
     //seed top and bottom edges
     for i := 1 to (dim2d[1]-2) do begin
         if isFillNewColor(i, 0) then fillPixel(i,1);
         if isFillNewColor(i, dim2d[2]-1) then fillPixel(i,dim2d[2]-2);
     end;
end;

procedure TDraw.voiMouseDown(Color, Orient: int64; Xfrac, Yfrac, Zfrac:  single);
//Orient: Ax(1), Cor(2), Sag(3)
var
  nPix, nSlices: int64;
begin
     if (view3d = nil) then exit;
     if ((dim3d[1] * dim3d[2]*dim3d[3]) < 1) then exit;
     if (Color < 0) or (Color > 255) or (Xfrac < 0.0) or (Yfrac < 0.0) or (Zfrac < 0.0) or (Xfrac > 1.0) or (Yfrac > 1.0) or (Zfrac > 1.0) then exit;
     if (Orient < 1) or (Orient > 8) then exit; //accept Axial, Sag, Coro
     voiCloseSlice;
     penColor := Color;
     nSlices := setDim2D(Orient);
     if (Orient = kOrientSagLR) or (Orient = kOrientSagRL) then
        dim2d[3] := frac2pix(Xfrac, dim3d[1]) //sag slices select Left-Right slices
     else if (Orient = kOrientCoro) then
        dim2d[3] := frac2pix(Yfrac, dim3d[2]) //coro slices select Anterio-Posterior slices
     else  //Axial
         dim2d[3] := frac2pix(Zfrac, dim3d[3]); //axial influences head-foot
     //store previous slice and orientation for interpolation between slices
     prevSlice := currSlice;
     currSlice := dim2d[3];
     if prevOrient <> Orient then //reset: e.g. we can not interpolate slices between coronal and axial orientations
        prevSlice := currSlice;
     prevOrient := Orient;
     //2D info
     nPix := dim2d[1] * dim2d[2]; //dim[3] = number of pixels
     if (nPix < 1) then exit;
     //dim2d[3] is the active slice we will be manipulating
     if (dim2d[3] < 0) or (dim2d[3] >= nSlices) then exit;
     //create slices holding initial pixel values
     setlength(view2d, nPix);
     setlength(undo2d, nPix);
     setlength(modified2d, nPix);
     FillChar(modified2d[0],nPix,0); //set all to zero: nothing drawn yet
     CopySlice2D(Orient, undo2d);
     Move(undo2d[0],view2d[0],nPix);//source/dest
     //for i := 0 to (nPix div 4) do
     //    view2d[i] := i mod 2;
     //record mouse
     click2pix (clickStartX, clickStartY, Xfrac, Yfrac, Zfrac);
     clickPrevX := clickStartX;
     clickPrevY := clickStartY;
     isMouseDown := true;
     isModified := true;
     isModifiedSinceSave := isModifiedSinceSave;
end;

procedure TDraw.CopySlice2D(Orient: int64; var out2D: TUInt8s);
var
  volOffset, i, j, k, nPix: int64;
begin
  nPix := dim2d[1] * dim2d[2]; //dim[3] = number of pixels
  if (Orient = kOrientSagRL) or (Orient = kOrientSagLR) then begin//Sag
  volOffset := dim2d[3]; //sag slices are in X direction
  i := 0; //sag is Y*Z
  for j := 0 to (dim3d[3]-1) do begin //read each slice (Z)
    for  k := 0 to (dim3d[2]-1) do begin //read Y direction
	     out2D[i] := view3d[(k*dim3d[1]) +volOffset];
	     i := i + 1;
    end;
    volOffset := volOffset + (dim3d[1]*dim3d[2]); //next Z slice
  end;
  end else if (Orient = kOrientCoro) then begin//Coro
   volOffset := dim2d[3]*dim3d[1]; //coro slices are in Y direction
   i := 0;  //coro is X*Z
   for j := 0 to (dim3d[3]-1) do begin //read each slice (Z)
     for  k := 0 to (dim3d[1]-1) do begin //read X direction
        out2D[i] := view3d[k+volOffset];
        i := i + 1;
     end;
     volOffset := volOffset + (dim3d[1]*dim3d[2]); //next Z slice
   end;
  end else begin //Axial
     volOffset := dim2d[3]*dim3d[1]*dim3d[2]; //axial slices are in Z direction, each X*Y pixels
     Move(view3d[volOffset], out2D[0],nPix);//source/dest
     //GLForm1.caption := format('%d %d',  [random(888), dim2d[3]]);
  end;
end;

function TDraw.voiIsEmpty: boolean;
var
  i,vx: int64;
begin
     result := true;
     vx := dim3d[1] * dim3d[2]* dim3d[3];
     if (vx < 0) or (view3d = nil)  then exit;
     i := 0;
     while (i < vx) and (view3d[i] = 0) do
       inc(i);
     if (i < vx) then
        result := false;
end;

function TDraw.setDim2D(Orient: int64): int64;
begin
     dim2d[0] := Orient; //dim[0] = slice orient
     if (Orient = kOrientSagRL) or (Orient = kOrientSagLR) then begin
        dim2d[1] := dim3d[2]; //Sag is Y*Z
        dim2d[2] := dim3d[3]; //Sag is Y*Z
        dim2d[3] := dim3d[1]; //Sag slices in X direction
        //dim2d[3] := frac2pix(Xfrac, dim3d[1]); //sag slices select Left-Right slices
     end else if (Orient = kOrientCoro) then begin
        dim2d[1] := dim3d[1]; //Coro X*Z
        dim2d[2] := dim3d[3]; //Coro is X*Z
        dim2d[3] := dim3d[2]; //Axial slices in Y direction
        //dim2d[3] := frac2pix(Yfrac, dim3d[2]); //coro slices select Anterio-Posterior slices
     end else begin  //Axial
         dim2d[1] := dim3d[1]; //Axial X*Y
         dim2d[2] := dim3d[2]; //Axial X*Y
         dim2d[3] := dim3d[3]; //Axial slices in Z direction
         //dim2d[3] := frac2pix(Zfrac, dim3d[3]); //axial influences head-foot
     end;
     result := dim2d[3];
end;

procedure TDraw.click2pix (var Xpix, Ypix: int64; Xfrac, Yfrac, Zfrac:  single);
begin
  if (dim2d[0] = kOrientSagRL) or (dim2d[0] = kOrientSagLR) then begin //Sag
     Xpix := frac2pix(Yfrac, dim2d[1]); //Sag is Y*Z
     Ypix := frac2pix(Zfrac, dim2d[2]); //Sag is Y*Z
  end else if (dim2d[0] = kOrientCoro) then begin //Coro
     Xpix := frac2pix(Xfrac, dim2d[1]); //Coro X*Z
     Ypix := frac2pix(Zfrac, dim2d[2]); //Coro is X*Z
  end else begin  //Axial
      Xpix := frac2pix(Xfrac, dim2d[1]); //Axial X*Y
      //GLForm1.Caption := floattostr(Xfrac)+' fcx  '+inttostr(dim2d[1])+'  '+inttostr(Xpix);
      Ypix := frac2pix(Yfrac, dim2d[2]); //Axial X*Y
  end;
end;

function TDraw.voiIsOpen: boolean;
begin
     if (view3d = nil) then
        result := false
     else
       result := true;
end;

procedure TDraw.voiColor (idx, r, g, b: byte);
begin
     doAlpha := true;
     colorLut[idx].r := r;
     colorLut[idx].g := g;
     colorLut[idx].b := b;
end;

function TDraw.IsOpen: boolean;
begin
  result := (view3d <> nil);
end;

function TDraw.voiDescriptives: int64;
var
  i, nPix, nPixDraw: int64;
begin
     if not IsOpen then begin
        exit (-1);
     end;
     nPix := dim3d[1] * dim3d[2]*dim3d[3];
     if nPix < 1 then exit(0);
     nPixDraw := 0;
     for i := 0 to (nPix -1) do begin
         if view3d[i] <> 0 then
            inc(nPixDraw);
     end;
     exit (nPixDraw);//('Volume of drawing is '+inttostr(nPixDraw)+' pixels.');
end;

procedure TDraw.PasteSlice2D(Orient: int64; var in2D, out3D: TUInt8s);
var
  volOffset, i, j, k, nPix: int64;
begin
  nPix := dim2d[1] * dim2d[2]; //number of pixels in 2D slice
  if (nPix < 1) or (Orient = kOrient3D) then exit;
  if (Orient = kOrientSagRL) or (Orient = kOrientSagLR) then begin//Sag
     volOffset := dim2d[3]; //sag slices are in X direction
     i := 0; //sag is Y*Z
     for j := 0 to (dim3d[3]-1) do begin //read each slice (Z)
         for  k := 0 to (dim3d[2]-1) do begin //read Y direction
              out3D[(k*dim3d[1]) +volOffset] := in2D[i];
              i := i + 1;
         end;
         volOffset := volOffset + (dim3d[1]*dim3d[2]); //next Z slice
     end;
  end else if (Orient = kOrientCoro) then begin//Coro
      volOffset := dim2d[3]*dim3d[1]; //coro slices are in Y direction
      i := 0;  //coro is X*Z
      for j := 0 to (dim3d[3]-1) do begin //read each slice (Z)
          for  k := 0 to (dim3d[1]-1) do begin //read X direction
               out3D[k+volOffset] := in2D[i];
               i := i + 1;
          end;
          volOffset := volOffset + (dim3d[1]*dim3d[2]); //next Z slice
      end;
  end else begin //Axial
        volOffset := dim2d[3]*dim3d[1]*dim3d[2]; //axial slices are in Z direction, each X*Y pixels
        Move(in2D[0], out3D[volOffset],nPix);//source/dest
  end;
end;

procedure TDraw.UpdateView3d;
var
  nPix: int64;
begin
     if (undo2d = nil) or (view3d = nil) then exit;
     if (dim2d[0] = kOrient3D) then begin //3D volume
        nPix := dim3d[1]* dim3d[2] * dim3d[3];
        if nPix < 1 then exit;
        Move(view2d[0], view3d[0],nPix);//source/dest
        exit;
     end else
         PasteSlice2D(dim2d[0], view2d, view3d);
end;

procedure TDraw.voiCloseSlice;
begin
  view2d := nil;
  undo2d := nil;
  modified2d := nil;
end;

type
LongIntRA0 = array [0.. 2147483647] of LongInt; //large number to avoid range check error:
LongIntp0 = ^LongIntRA0;

procedure TDraw.morphFill(volImg, volVoi: TUInt8s; Color: int64; Xmm,Ymm,Zmm: single; xOri, yOri, zOri, dxOrigin,  radiusMM : int64; drawMode: int64);
var
  //vol,
   queue: longintp0;
   qLo, qHi,xyPix, xPix, xyzPix: int64;
function clrImg(X,Y,Z: int64) : byte;
begin
     result := volImg[X+ xPix * Y + Z * xyPix];
end; //clrImg()
procedure filterImg;  //set volImg so 1=possible target, 0=not target
var
   clrOri, i, mn, mx, x, y, z: int64;
   dxZ,dxY, dxThresh: single;
begin
     //intensity filter
     clrOri := clrImg(Xori, Yori, Zori);
     mn := (clrOri-dxOrigin);
     mx := (clrOri+dxOrigin);
     for i := 0 to (xyzPix -1) do begin
         if (volImg[i] >= mn ) and (volImg[i] <= mx ) then
            volImg[i] := 1
         else
             volImg[i] := 0;
     end;
     //(optional) constrain with current voi
     if (drawMode =kDrawModeConstrain)  then
        for i := 0 to (xyzPix - 1) do
            if(volVoi[i] <> 0) then volImg[i] := 0;
     //constrain with radius
     dxThresh := sqr(radiusMM); //avoid sqrt, distance = sqrt(x^2+y^2+z^2)
     i := 0;
     for z := 0 to (dim3d[3] - 1) do begin
         dxZ := sqr( (z-zOri)*Zmm);
            for y := 0 to (dim3d[2] - 1) do begin
                dxY := sqr( (y-yOri)*Ymm);
                   for x := 0 to (dim3d[1] - 1) do begin
                       if ( sqr( (x-xOri)*Xmm) + dxY + dxZ) > dxThresh then
                          volImg[i] := 0; //outside sphere
                       i := i + 1;
                   end; //for y
            end; //for y
     end; //for z
end; // filterImg()
procedure startQ;
begin
     qLo := 0;
     qHi := 0;
     queue^[0] := xOri + (yOri * xPix) + (zOri * xyPix);
     volImg[xOri + (yOri * xPix) + (zOri * xyPix)] := 2; //select this voxel
end; //

procedure addQueue (pix: int64);
begin
     if (pix < 0) or (pix > (xyzPix -1)) then exit;
     if (volImg[pix] <> 1) then exit;
     volImg[pix] := 2;  //select this voxel
      qHi := qHi+1;
      queue^[qHi] := pix; //add to stack
end; //addQueue

procedure retireQLo;
//we always increment QLo - we can increase QHi from 0..6
begin
     addQueue(queue^[qLo]-1); //left
     addQueue(queue^[qLo]+1); //right
     addQueue(queue^[qLo]-xPix); //anterior
     addQueue(queue^[qLo]+xPix); //posterior
     addQueue(queue^[qLo]-xyPix); //below
     addQueue(queue^[qLo]+xyPix); //above
     qLo := qLo + 1;
end;  //retireQLo()

begin
     xPix := dim3d[1];
     xyPix :=  dim3d[1] * dim3d[2];
     xyzPix :=  dim3d[1] * dim3d[2] * dim3d[3];
     filterImg;
     getmem(queue, xyzPix * sizeof(longint));
     startQ;
     while (qLo <= qHi) do
           retireQLo;
     freemem(queue);
     if drawMode = kDrawModeDelete then color := 0;
     for xPix := 0 to (xyzPix-1) do
         if (volImg[xPix] = 2) then
            view2d[xPix] := color;
     //freemem(vol);
     //GLForm1.Caption := floattostr(Xmm)+'x'+floattostr(Ymm)+'x'+floattostr(Zmm);
end;

procedure TDraw.voiIntensityFilter(intenVol: TUInt8s; Color: int64; rampAbove, rampBelow, drawMode: integer);
var
  nPix,  i : int64;
begin
     if (Color < 0) then exit;
     if (intenVol = nil) then exit;
     if (view3d = nil) and (Color = 0)  then exit; //nothing to erase
     if (dim3d[1] < 1) or (dim3d[2] < 1) or (dim3d[3] < 1) then exit;
     nPix := dim3d[1] * dim3d[2] * dim3d[3];
     //GLForm1.SliceBox.Caption := inttostr(Color)+'b'+ inttostr(nPix);
     voiCloseSlice;
     dim2d[0] := kOrient3D; //dim[0] = slice orientation 0 = 3d volume
     setlength(view2d, nPix);
     setlength(undo2d, nPix);
     Move(view3d[0], undo2d[0],nPix);//source/dest
     Move(undo2d[0],view2d[0],nPix);//source/dest
     if (drawMode = kDrawModeAppend) then begin
       if rampAbove < rampBelow then begin
          for i := 0 to (nPix - 1) do
              if (intenVol[i] >= rampAbove) and (intenVol[i] <= rampBelow) then view2d[i] := Color;


       end else begin
           for i := 0 to (nPix - 1) do
               if (intenVol[i] > rampAbove) or (intenVol[i] < rampBelow) then view2d[i] := Color;
       end;
     end else begin
         if rampAbove < rampBelow then begin
            for i := 0 to (nPix - 1) do
                if (intenVol[i] >= rampAbove) and (intenVol[i] <= rampBelow) then
                   //view2d[i] := Color;
                else
                    view2d[i] := 0;


         end else begin
             for i := 0 to (nPix - 1) do
                 if (intenVol[i] > rampAbove) or (intenVol[i] < rampBelow) then
                    //view2d[i] := Color;
                 else
                     view2d[i] := 0;
         end;
     end;
     (*if (drawMode = kDrawModeDeleteDark)  then begin
        for i := 0 to (nPix - 1) do
            if (intenVol[i] < threshold) then view2d[i] := 0;
     end else if (drawMode = kDrawModeAddDark)  then begin
        for i := 0 to (nPix - 1) do
            if (intenVol[i] < threshold) then view2d[i] := Color;
     end else if (drawMode = kDrawModeDeleteBright)  then begin
        for i := 0 to (nPix - 1) do
            if (intenVol[i] > threshold) then view2d[i] := 0;
     end else if (drawMode = kDrawModeAddBright)  then begin
        for i := 0 to (nPix - 1) do
            if (intenVol[i] > threshold) then view2d[i] := Color;
     end;  *)
     doRedraw := true;
     UpdateView3d;
     isModified := true;
     isModifiedSinceSave := true;
end;

procedure TDraw.voiMorphologyFill(intenVol: TUInt8s; Color: int64; Xmm, Ymm, Zmm, Xfrac, Yfrac, Zfrac:  single; dxOrigin, radiusMM: int64; drawMode: int64);
var
  nPix,  x, y, z: int64;
  inVol: TUInt8s;
begin
     if (Color < 0) then exit;
     if (intenVol = nil) then exit;
     if (view3d = nil) and (Color = 0)  then exit; //nothing to erase
     if (view3d = nil) then voiMouseDown(Color, 1, Xfrac, Yfrac, Zfrac);  //1 : slice orient arbitrary
     if (dim3d[1] < 5) or (dim3d[2] < 5) or (dim3d[3] < 1) then exit;
     nPix := dim3d[1] * dim3d[2] * dim3d[3];
     voiCloseSlice;
     dim2d[0] := kOrient3D; //dim[0] = slice orientation 0 = 3d volume
     setlength(view2d, nPix);
     setlength(undo2d, nPix);
     setlength(inVol, nPix);
     Move(intenVol[0], inVol[0],nPix);//source/dest
     Move(view3d[0], undo2d[0],nPix);//source/dest
     Move(undo2d[0],view2d[0],nPix);//source/dest
     //get intensity
     x := frac2pix(Xfrac, dim3d[1]);
     y := frac2pix(Yfrac, dim3d[2]);
     z := frac2pix(Zfrac, dim3d[3]);
     //morphFill(volImg, volVoi: bytep0; Color: int64; Xmm,Ymm,Zmm: single; xOri, yOri, zOri, dxOrigin, dxEdge, radiusMM, erodeCycles, growStyle: int64; constrain0: boolean);
     morphFill(inVol, view3d,  Color, Xmm, Ymm, Zmm, x,y,z, dxOrigin,  radiusMM, drawMode);
     inVol := nil;
     doRedraw := true;
     UpdateView3d;
     isModified := true;
     isModifiedSinceSave := true;
end;

constructor TDraw.Create();//niftiFileName: string; tarMat: TMat4; tarDim: TVec3i; isInterpolate: boolean; out isOK: boolean); overload; //overlay
//constructor TDraw.Create();
begin
  doRedraw := false;
  penColor := -1;
  PenColorOnRelease := -1;
  doAlpha:= false;
  isModified := false;
  isModifiedSinceSave := false;
  view2d := nil;
  undo2d := nil;
  modified2d := nil;
  filename := '';
  //view3d := nil;
  //isForceCreate := false;
  isMouseDown:= false;
  fOpacityFrac := 0.5;
  voiDefaultLUT();
  //inherited Create(niftiFileName, tarMat, tarDim, isInterpolate, isOK);
  //voiCreate(tarDim.x, tarDim.y, tarDim.z);
end;

end.

