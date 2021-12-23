unit otsuml;
//Multilevel Otsu's Method
//Otsu N (1979) A threshold selection method from gray-level histograms. IEEE Trans. Sys., Man., Cyber. 9: 62-66.
//Lookup Tables as suggested by Liao, Chen and Chung (2001) A fast algorithm for multilevel thresholding
//note that my "otsu.pas" is slightly faster and much simpler if you only want bi-level output

interface
uses SimdUtils, sysutils, math, dialogs;

function FindOtsu2(var Img: TUInt8s; nVox: integer): byte;
procedure ApplyOtsu(var Img: TUInt8s; nVox, levels: integer);//levels: 2=black/white, 3=3tone, 4=4tone
procedure ApplyOtsuBinary(var Img: TUInt8s; nVox,levels: integer);
procedure PreserveLargestCluster(var lImg: TUInt8s; Xi,Yi,Zi: integer; lClusterValue,ValueForSmallClusters: byte  );
procedure SimpleMaskDilate(var lImg: TUInt8s; lXi,lYi,lZi: integer);
procedure SimpleMaskErode(var lImg: TUInt8s; lXi,lYi,lZi: integer);
procedure RemoveAllSmallClusters(var lImg: TUInt8s; Xi,Yi,Zi: integer; lClusterValue,ValueForSmallClusters: byte; ThresholdVox, NeighborMethod:integer);

implementation

Type
HistoRA = array [0..255] of integer; 
HistoRAd = array [0..255] of double;
Histo2Dp = array of array of double;

type
    ByteRA = array [1..1] of byte;
    Bytep = ^ByteRA;
    LongIntRA = array [1..1] of LongInt;
    LongIntp = ^LongIntRA;

procedure RemoveAllSmallClusters(var lImg: TUInt8s; Xi,Yi,Zi: integer; lClusterValue,ValueForSmallClusters: byte; ThresholdVox, NeighborMethod:integer);
(*NeighborMethod https://afni.nimh.nih.gov/pub/dist/doc/program_help/3dClusterize.html
-NN {1|2|3}    :Necessary option to specify how many neighbors a voxel
                 has; one MUST put one of 1, 2 or 3 after it:
                   1 -> 6 facewise neighbors
                   2 -> 18 face+edgewise neighbors
                   3 -> 26 face+edge+cornerwise neighbors

*)
var
  i, j, XY, XYZ, qlo, qhi: integer;
  qimg, img32: TInt32s;
procedure checkPixel(vxl: integer);
begin
     if img32[vxl] <> -1 then exit; //already found or not a target
     qhi := qhi + 1;
     img32[vxl] := 1; //found
     qimg[qhi] := vxl; //location
end;//nested checkPixel()
procedure retirePixel6();
var
  vxl: integer;
begin
     vxl := qimg[qlo];
     checkPixel(vxl-1);
     checkPixel(vxl+1);
     checkPixel(vxl-Xi);
     checkPixel(vxl+Xi);
     checkPixel(vxl-XY);
     checkPixel(vxl+XY);
     qlo := qlo + 1;
end;//nested retirePixel()
procedure retirePixel18();
var
  vxl: integer;
begin
     vxl := qimg[qlo];
     //edges in plane
     checkPixel(vxl-Xi-1);
     checkPixel(vxl-Xi+1);
     checkPixel(vxl+Xi-1);
     checkPixel(vxl+Xi+1);
     //edges below
     checkPixel(vxl-1-XY);
     checkPixel(vxl+1-XY);
     checkPixel(vxl-Xi-XY);
     checkPixel(vxl+Xi-XY);
     //edges above
     checkPixel(vxl-1+XY);
     checkPixel(vxl+1+XY);
     checkPixel(vxl-Xi+XY);
     checkPixel(vxl+Xi+XY);
     retirePixel6();
end;//nested retirePixel()

procedure retirePixel26();
var
  vxl: integer;
begin
     vxl := qimg[qlo];
     //corners below
     checkPixel(vxl-Xi-XY-1);
     checkPixel(vxl-Xi-XY+1);
     checkPixel(vxl+Xi-XY-1);
     checkPixel(vxl+Xi-XY+1);
     //corners above
     checkPixel(vxl-Xi+XY-1);
     checkPixel(vxl-Xi+XY+1);
     checkPixel(vxl+Xi+XY-1);
     checkPixel(vxl+Xi+XY+1);
     retirePixel18();
end;

begin //main RemoveSmallClusters()
  if (Zi < 1) then exit;
  XY := Xi * Yi;
  XYZ := XY * Zi;
  setlength(img32, XYZ);
  setlength(qimg, XYZ);
  //set target voxels
  for i := 0 to (XYZ-1) do begin
      img32[i] := 0;
      if lImg[i] = lClusterValue then
         img32[i] := -1;
  end;
  //clear bottom and top slices
  for i := 0 to (XY-1) do
    img32[i] := 0;
  for i := (XYZ-1-XY) to (XYZ-1) do
    img32[i] := 0;
  //now seed each voxel
  if NeighborMethod = 2 then begin
     for i := (XY) to (XYZ-1-XY) do begin
         if (img32[i] < 0) then begin //voxels not yet part of any region
            qlo := 0;
            qhi := -1;
            checkPixel(i);
            while qlo <= qhi do
              retirePixel18();
            for j := 0 to qhi do
                img32[qimg[j]] := qhi + 1;
         end;
     end;
  end else begin
    for i := (XY) to (XYZ-1-XY) do begin
        if (img32[i] < 0) then begin //voxels not yet part of any region
           qlo := 0;
           qhi := -1;
           checkPixel(i);
           while qlo <= qhi do
             retirePixel6();
           for j := 0 to qhi do
               img32[qimg[j]] := qhi + 1;
        end;
    end;
  end;
  //delete voxels not part of largest cluster
  for i := 0 to (XYZ-1) do
      if img32[i] < ThresholdVox then
         img32[i] := 0;
  //recover bottom and top slices
  for i := 0 to (XY-1) do
      if (img32[i+XY] >= ThresholdVox) then
         img32[i] := 1;
  for i := (XYZ-1-XY) to (XYZ-1) do
      if (img32[i-XY] >=  ThresholdVox) then
         img32[i] := 1;
  //apply filter to input image
  for i := 0 to (XYZ-1) do
      if img32[i] = 0 then
         lImg[i] := 0;
  qimg := nil;
  img32 := nil;
end;// RemoveSmallClusters()



procedure PreserveLargestCluster(var lImg: TUInt8s; Xi,Yi,Zi: integer; lClusterValue,ValueForSmallClusters: byte);
var
  mx, i, j, XY, XYZ, qlo, qhi: integer;
  qimg, img32: TInt32s;
procedure checkPixel(vxl: integer);
begin
     if img32[vxl] <> -1 then exit; //already found or not a target
     qhi := qhi + 1;
     img32[vxl] := 1; //found
     qimg[qhi] := vxl; //location
end;//nested checkPixel()
procedure retirePixel();
var
  vxl: integer;
begin
     vxl := qimg[qlo];
     checkPixel(vxl-1);
     checkPixel(vxl+1);
     checkPixel(vxl-Xi);
     checkPixel(vxl+Xi);
     checkPixel(vxl-XY);
     checkPixel(vxl+XY);
     qlo := qlo + 1;
end;//nested retirePixel()
begin //main PreserveLargestCluster()
  if (Zi < 1) then exit;
  XY := Xi * Yi;
  XYZ := XY * Zi;
  setlength(img32, XYZ);
  setlength(qimg, XYZ);
  //set target voxels
  for i := 0 to (XYZ-1) do begin
      img32[i] := 0;
      if lImg[i] = lClusterValue then
         img32[i] := -1;
  end;
  //clear bottom and top slices
  for i := 0 to (XY-1) do
    img32[i] := 0;
  for i := (XYZ-1-XY) to (XYZ-1) do
    img32[i] := 0;
  //now seed each voxel
  mx := 0;
  for i := (XY) to (XYZ-1-XY) do begin
      if (img32[i] < 0) then begin //voxels not yet part of any region
         qlo := 0;
         qhi := -1;
         checkPixel(i);
         while qlo <= qhi do
           retirePixel();
         for j := 0 to qhi do
             img32[qimg[j]] := qhi + 1;
         if (qhi+1) > mx then mx := qhi + 1;
      end;
  end;
  if mx < 2 then begin
     qimg := nil;
     img32 := nil;
     exit;
  end;
  //delete voxels not part of largest cluster
  for i := 0 to (XYZ-1) do
      if img32[i] <> mx then
         img32[i] := 0;
  //recover bottom and top slices
  for i := 0 to (XY-1) do
      if (img32[i+XY] = mx) then
         img32[i] := mx;
  for i := (XYZ-1-XY) to (XYZ-1) do
      if (img32[i-XY] = mx) then
         img32[i] := mx;
  //apply filter to input image
  for i := 0 to (XYZ-1) do
      if img32[i] = 0 then
         lImg[i] := 0;
  qimg := nil;
  img32 := nil;
end;// PreserveLargestCluster()

procedure ZeroFaces(var lImg: TUInt8s; lXi,lYi,lZi: integer);
var
  x,y,z, i, lo, hi, j, lXYZ, lXY: integer;
begin
  if (lXi < 5) or (lYi < 5) or (lZi < 1) then exit;
  lXYZ := lXi*lYi*lZi;
  lXY := lXi*lYi;
  //zero faces
  for i := 0 to (lXY-1) do //bottom
      lImg[i] := 0;
  for i := (lXYZ-lXY-1) to (lXYZ-1) do //top
      lImg[i] := 0;
  for z := 0 to (lZi -1) do begin //left/right
      lo := lXY * z;
      hi := lo + (lXi-1);
      for y := 0 to (lYi-1) do begin
          j := y * (lXi);
          lImg[lo+j] := 0;
          lImg[hi+j] := 0;
      end;
  end;
  for z := 0 to (lZi -1) do begin //anterior/posterior
      lo := lXY * z;
      hi := lo + ((lYi-1) * (lXi));
      for x := 0 to (lXi-1) do begin
          lImg[lo+x] := 0;
          lImg[hi+x] := 0;
      end;
  end;
end;// ZeroFaces()

procedure SimpleMaskErode(var lImg: TUInt8s; lXi,lYi,lZi: integer);
var
  i,lXYZ, lXY: integer;
  lTemp: TUInt8s;
begin
     if (lXi < 5) or (lYi < 5) or (lZi < 1) then exit;
     lXYZ := lXi*lYi*lZi;
     lXY := lXi*lYi;
     setlength(lTemp, lXYZ);
     lTemp := Copy(lImg, Low(lImg), Length(lImg));
     ZeroFaces (lTemp, lXi,lYi,lZi);
     for i := 1 to (lXYZ-1) do  //left
         lImg[i] := min(lImg[i], lTemp[i-1]);
     for i := 0 to (lXYZ-2) do  //right
         lImg[i] := min(lImg[i], lTemp[i+1]);
     for i := lXi to (lXYZ-1) do  //posterior
         lImg[i] := min(lImg[i], lTemp[i-lXi]);
     for i := 0 to (lXYZ-1-lXi) do  //anterior
         lImg[i] := min(lImg[i], lTemp[i+lXi]);
     for i := lXY to (lXYZ-1) do  //inferior
         lImg[i] := min(lImg[i], lTemp[i-lXY]);
     for i := 0 to (lXYZ-1-lXY) do  //superior
         lImg[i] := min(lImg[i], lTemp[i+lXY]);
     lTemp := nil;
end;// SimpleMaskErode()

procedure SimpleMaskDilate(var lImg: TUInt8s; lXi,lYi,lZi: integer);
var
  i,lXYZ, lXY: integer;
  lTemp: TUInt8s;
begin
     if (lXi < 5) or (lYi < 5) or (lZi < 1) then exit;
     lXYZ := lXi*lYi*lZi;
     lXY := lXi*lYi;
     setlength(lTemp, lXYZ);
     lTemp := Copy(lImg, Low(lImg), Length(lImg));
     ZeroFaces (lTemp, lXi,lYi,lZi);
     for i := 1 to (lXYZ-1) do  //left
         lImg[i] := max(lImg[i], lTemp[i-1]);
     for i := 0 to (lXYZ-2) do  //right
         lImg[i] := max(lImg[i], lTemp[i+1]);
     for i := lXi to (lXYZ-1) do  //posterior
         lImg[i] := max(lImg[i], lTemp[i-lXi]);
     for i := 0 to (lXYZ-1-lXi) do  //anterior
         lImg[i] := max(lImg[i], lTemp[i+lXi]);
     for i := lXY to (lXYZ-1) do  //inferior
         lImg[i] := max(lImg[i], lTemp[i-lXY]);
     for i := 0 to (lXYZ-1-lXY) do  //superior
         lImg[i] := max(lImg[i], lTemp[i+lXY]);
     lTemp := nil;
end;// SimpleMaskDilate()

procedure OtsuLUT(H: HistoRA; var resultp: Histo2Dp);
var
  Sum,Prob: double;
  v,u: integer;//column/rom index
  P,S: array of array of double;
  //P,S: Histo2D; //<- this works in Lazarus, but crashes Delphi: static arrays are too large for heap
begin
     resultp[0][0] := 0;
     Sum := 0;
     for v := 0 to 255 do
       Sum := Sum + H[v];
     if Sum <= 0 then
        exit;
     SetLength(P,256,256);
     SetLength(S,256,256);
     P[0][0] := H[0];
     S[0][0] := H[0];
     for v := 1 to 255 do begin
         prob := H[v]/Sum;
         P[0][v] := P[0][v-1]+prob;
         S[0][V] := S[0][v-1]+(v+1)*prob;
     end;
     for u := 1 to 255 do begin
         for v := u to 255 do begin
             P[u][v] := P[0][v]-P[0][u-1];
             S[u][v] := S[0][v]-S[0][u-1];
         end
     end;
    //result is eq 29 from Liao
     for u := 0 to 255 do begin
         for v := u to 255 do begin
             if (S[u][v] = 0) or (P[u][v] = 0) then  //avoid divide by zero errors...
                resultp[u][v] := 0
             else
                 resultp[u][v] := sqr(S[u][v]) /P[u][v];
         end
     end;
     P := nil;
     S := nil;
end;// OtsuLUT()

function OtsuCostFunc(H: HistoRA): integer;
//Otsu N (1979) A threshold selection method from gray-level histograms". IEEE Trans. Sys., Man., Cyber. 9: 62-66.
//http://en.wikipedia.org/wiki/Otsu's_method
//http://www.labbookpages.co.uk/software/imgProc/otsuThreshold.html
//returns threshold for binarizing an image
// all voxel <=Threshold are background
// all voxel >Threshold are object
const
  kMaxBin = 255;
var
   t,total: integer;
   wB,wF,Sum,SumB,mF,mB,varBetween,varMax: double;
begin
     result := 0;
     wB := 0;
     //wF := 0;
     SumB := 0;
  	 Sum := 0;
     Total := 0;
     varMax := 0;
     for t := 0 to kMaxBin do
     	 Total := Total + H[t];
     if Total = 0 then exit;
     for t := 0 to kMaxBin do
     	 Sum := Sum + (t*H[t]);
	 for t :=0 to kMaxBin do begin
   	 	 wB :=  wB + H[t];               // Weight Background
   		 if (wB = 0) then continue;
		 wF := Total - wB;                 // Weight Foreground
   		 if (wF = 0) then break;
   		 sumB := sumB+(t * H[t]);
         mB := sumB / wB;            // Mean Background
   		 mF := (sum - sumB) / wF;    // Mean Foreground
         // Calculate Between Class Variance
   		 varBetween := (wB/Total) * (wF/Total) * sqr(mB - mF);
         // Check if new maximum found
   		 if (t=0) or (varBetween > varMax) then begin
      	 	varMax := varBetween;
      		result := t;
   		 end;
     end;
end;// OtsuCostFunc()

function OtsuCostFunc2(lHisto: HistoRA): integer;
//only 2 levels: black and white
var
  v,max: double;
  h2d: Histo2Dp;
  n: integer;
begin
  SetLength(h2d, 256, 256);
  OtsuLUT(lHisto,h2d);
  //default solution
  n := 128;
  max := h2d[0,n]+h2d[n+1,255];
  result := n;
  //exhaustively search
  for n := 0 to (255-1) do begin
      v := h2d[0,n]+h2d[n+1,255];
      if v > max then begin
         result := n;
         max := v;
      end; //new max
  end; //for n
  h2d := nil;
end;// OtsuCostFunc2()

procedure OtsuCostFunc3(lHisto: HistoRA; var Lo,Hi: integer);
//three levels: black, white gray
var
  v,max: double;
  l,h: integer;
  h2d: Histo2Dp;
begin
  SetLength(h2d, 256, 256);
  OtsuLUT(lHisto,h2d);
  //default solution
  lo := 85;
  hi := 170;
  max := h2d[0,lo]+h2d[lo+1,Hi]+h2d[Hi+1,255];
  //exhaustively search
  for l := 0 to (255-2) do begin
      for h := l+1 to (255-1) do begin
          v := h2d[0,l]+h2d[l+1,h]+h2d[h+1,255];
          if v > max then begin
             lo := l;
             hi := h;
             max := v;
          end; //new max
      end;//for h -> hi
  end; //for l -> low
  h2d := nil;
end;// OtsuCostFunc3()

procedure OtsuCostFunc4(lHisto: HistoRA; var Lo,Mid,Hi: integer);
// 4 levels
var
  v,max: double;
  l,m,h: integer;
  h2d: Histo2Dp;
begin
  SetLength(h2d, 256, 256);
  OtsuLUT(lHisto,h2d);
  //default solution
  lo := 64;
  mid := 128;
  hi := 192;
  max := h2d[0,lo]+h2d[lo+1,mid]+h2d[mid+1,hi]+h2d[Hi+1,255];
  //exhaustively search
  for l := 0 to (255-3) do begin
      for m := l+1 to (255-2) do begin
        for h := m+1 to (255-1) do begin
          v := h2d[0,l]+h2d[l+1,m]+h2d[m+1,h]+h2d[h+1,255];
          if v > max then begin
             lo := l;
             mid := m;
             hi := h;
             max := v;
          end; //new max
        end;//for h -> hi
      end; //for mid
  end; //for l -> low
  h2d := nil;
end;// OtsuCostFunc4()

function FindOtsu2 (var Img: TUInt8s; nVox: integer): byte;
var
  n: integer;
  lHisto: HistoRA;
begin
  result := 128;
  if nVox < 1 then exit;
  //create histogram
  for n := 0 to 255 do
    lHisto[n] := 0;
  for n := 0 to (nVox-1) do
    inc(lHisto[Img[n]]);
  //now find minimum intraclass variance....
  //result := OtsuCostFunc(lHisto);
  result := OtsuCostFunc2(lHisto); //same answer, just slower and more memory
end;// FindOtsu2()

procedure FindOtsu3 (var Img: TUInt8s; nVox: integer; var lo, hi: integer);
var
  n: integer;
  lHisto: HistoRA;
begin
  lo := 85;
  hi := 170;
  if nVox < 1 then exit;
  //create histogram
  for n := 0 to 255 do
    lHisto[n] := 0;
  for n := 0 to (nVox-1) do
    inc(lHisto[Img[n]]);
  //now find minimum intraclass variance....
  OtsuCostFunc3(lHisto,lo,hi);
end;// FindOtsu3()

procedure FindOtsu4(var Img: TUInt8s; nVox: integer; var lo, med, hi: integer);
var
  n: integer;
  lHisto: HistoRA;
begin
  lo := 64;
  med := 128;
  hi := 192;
  if nVox < 1 then exit;
  //create histogram
  for n := 0 to 255 do
    lHisto[n] := 0;
  for n := 0 to (nVox-1) do
    inc(lHisto[Img[n]]);
  //now find minimum intraclass variance....
  OtsuCostFunc4(lHisto,lo,med,hi);
end;// FindOtsu4()

function ApplyOtsu2(var Img: TUInt8s; nVox: integer): byte;
var
  n: integer;
begin
  result := 128;
  if nVox < 1 then exit;
  result := FindOtsu2(Img,nVox);
  for n := 0 to (nVox-1) do
    if Img[n] > result then
      Img[n] := 255
    else
      Img[n] := 0;
end; // ApplyOtsu2()

procedure ApplyOtsu3(var Img: TUInt8s; nVox: integer);
var
  n,lo,hi: integer;
  h: histora;
begin
  if nVox < 1 then exit;
  FindOtsu3(Img,nVox,lo,hi);
  for n := 0 to 255 do
    if n <= Lo then
       H[n] := 0
    else if n <= hi then
       h[n] := 128
    else
        h[n] := 255;
  for n := 0 to (nVox-1) do
      Img[n] := H[Img[n]];
end; // ApplyOtsu3()

procedure ApplyOtsu4(var Img: TUInt8s; nVox: integer);
var
  n,lo,med,hi: integer;
  h: histora;
begin
  if nVox < 1 then exit;
  FindOtsu4(Img,nVox,lo,med,hi);
  for n := 0 to 255 do
    if n <= Lo then
       H[n] := 0
    else if n <= med then
       h[n] := 85
    else if n <= hi then
       h[n] := 170
    else
        h[n] := 255;
  for n := 0 to (nVox-1) do
      Img[n] := H[Img[n]];
end;// ApplyOtsu4()

procedure ApplyOtsu(var Img: TUInt8s; nVox,levels: integer);
begin
     if levels <= 2 then
        ApplyOtsu2(Img,nVox)
     else if levels = 3 then
          ApplyOtsu3(Img,nVox)
     else
         ApplyOtsu4(Img,nVox);
end;// ApplyOtsu()

procedure ApplyOtsuBinary(var Img: TUInt8s; nVox,levels: integer);
//1=1/4, 2=1/3, 3=1/2, 4=2/3, 5=3/4
var
  n: integer;
  h: histora;
begin
     if nVox < 1 then exit;
     if (levels <= 1) or (levels >= 5) then
        ApplyOtsu4(Img,nVox)
     else if (levels = 2) or (levels = 4) then
          ApplyOtsu3(Img,nVox)
     else //level = 3
         ApplyOtsu2(Img,nVox);
     if levels <= 3 then begin //make dark: all except 255 equal 0
        for n := 0 to 254 do
            H[n] := 0;
        H[255] := 255;
     end else begin //make bright: all except 0 equal 255
       H[0] := 0;
       for n := 1 to 255 do
           H[n] := 255;
     end;
     for n := 0 to (nVox-1) do
            Img[n] := H[Img[n]];
end;// ApplyOtsuBinary()


end.

