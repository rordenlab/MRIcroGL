unit colorTable;
{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}

interface
uses SimdUtils, Dialogs, IniFiles, SysUtils, Math;

type
  TCLUTnode =record
    intensity: byte;
    rgba: TRGBA;
  end;
  TCLUTnodeRA = array [0..255] of TCLUTnode;
  TCLUTrec =record
    numnodes, tag: integer;
    min,max: single;
    nodes: TCLUTnodeRA;
  end;
  TCLUT = Class(TObject)  //color lookup table manager
      private
        CLUT: TCLUTrec; //nodes, min/max
        fLUT, fLUTlabel: TLUT; //256 discrete colors
        fNeedsUpdate, fIsDrawing, fIsLabels, fIsFromZero: boolean;
        fBackColor: TRGBA;
    public
      InvertColorMap: boolean;
      property FromZero: boolean read fIsFromZero write fIsFromZero;
      property BackColor: TRGBA read fBackColor write fBackColor;
      procedure GenerateLUT(saturation: float = 1.0; LabelAlpha: integer = -1);
      property NeedsUpdate: boolean read fNeedsUpdate write fNeedsUpdate;
      property Tag: integer read CLUT.Tag; //arbitrary number associated with table, e.g. position in drop down list
      property SuggestedMinIntensity: single read CLUT.min;
      property SuggestedMaxIntensity: single read CLUT.max;
      function OpenCLUT(clutFileName: string; cTag: integer): boolean;
      function SaveCLUT(clutFileName: string): boolean;
      property FullColorTable: TCLUTrec read CLUT;
      procedure ChangeNode(index, intensity, R,G,B,A: byte);
      procedure AddNode(intensity, A: byte);
      procedure SetLabels();
      function DeleteNode (index: integer): boolean;
      //procedure SetTag(cTag: integer);
      property LUT: TLUT read fLUT;
      procedure LoadCustomLabelLut(lFileName: string);
      constructor Create(isDrawing: boolean = false); overload;
      constructor Create(clutFileName: string; cTag: integer); overload;

  end;

implementation

//uses mainunit;

function makeRGB(R,G,B: byte): TRGBA;
begin
  result := SetRGBA(R,G,B,64);
end;

procedure TCLUT.LoadCustomLabelLut(lFileName: string);
var
 i: integer;
   lBuff: array of byte;
   lFData: file;
begin
  if not fileexists(lFileName) then exit;
  assignfile(lFdata, lFileName);
  Filemode := 0;
  reset(lFdata,1);
  if FileSize(lFData) <> 768 then begin
     closefile(lFdata);
     exit;
  end;
  SetLength( lBuff, 768);
  BlockRead(lFdata, lBuff[0], 768);
  closefile(lFdata);
  for i := 0 to 255 do
      fLUTlabel[i] := makeRGB(lBuff[i],lBuff[i+256],lBuff[i+512]);
  lBuff := nil;
end;

{procedure saveLUT(l:TLUT);
var
   i: integer;
begin
  for i := 0 to 255 do
      writeln(format('nodergba%d=%d|%d|%d|%d|',[i, l[i].r, l[i].g, l[i].b, l[i].a]));
end;}

function defaultLabelLut: TLUT;
var
   lut: TLUT;
   i: integer;
begin
    //lut[0] := makeRGB(fBackColor.r,fBackColor.g, fBackColor.b);
    lut[0].A := 0;
    lut[1] := makeRGB(71,46,154);
    lut[2] := makeRGB(33,78,43);
    lut[3] := makeRGB(192,199,10);
    lut[4] := makeRGB(32,79,207);
    lut[5] := makeRGB(195,89,204);
    lut[6] := makeRGB(208,41,164);
    lut[7] := makeRGB(173,208,231);
    lut[8] := makeRGB(233,135,136);
    lut[9] := makeRGB(202,20,58);
    lut[10] := makeRGB(25,154,239);
    lut[11] := makeRGB(210,35,30);
    lut[12] := makeRGB(145,21,147);
    lut[13] := makeRGB(89,43,230);
    lut[14] := makeRGB(87,230,101);
    lut[15] := makeRGB(245,113,111);
    lut[16] := makeRGB(246,191,150);
    lut[17] := makeRGB(38,147,35);
    lut[18] := makeRGB(3,208,128);
    lut[19] := makeRGB(25,37,57);
    lut[20] := makeRGB(57,28,252);
    lut[21] := makeRGB(167,27,79);
    lut[22] := makeRGB(245,86,173);
    lut[23] := makeRGB(86,203,120);
    lut[24] := makeRGB(227,25,25);
    lut[25] := makeRGB(208,209,126);
    lut[26] := makeRGB(81,148,81);
    lut[27] := makeRGB(64,187,85);
    lut[28] := makeRGB(90,139,8);
    lut[29] := makeRGB(199,111,7);
    lut[30] := makeRGB(140,48,122);
    lut[31] := makeRGB(48,102,237);
    lut[32] := makeRGB(212,76,190);
    lut[33] := makeRGB(180,110,152);
    lut[34] := makeRGB(70,106,246);
    lut[35] := makeRGB(120,130,182);
    lut[36] := makeRGB(9,37,130);
    lut[37] := makeRGB(192,160,219);
    lut[38] := makeRGB(245,34,67);
    lut[39] := makeRGB(177,222,76);
    lut[40] := makeRGB(65,90,167);
    lut[41] := makeRGB(157,165,178);
    lut[42] := makeRGB(9,245,235);
    lut[43] := makeRGB(193,222,250);
    lut[44] := makeRGB(100,102,28);
    lut[45] := makeRGB(181,47,61);
    lut[46] := makeRGB(125,19,186);
    lut[47] := makeRGB(145,130,250);
    lut[48] := makeRGB(62,4,199);
    lut[49] := makeRGB(8,232,67);
    lut[50] := makeRGB(108,137,58);
    lut[51] := makeRGB(36,211,50);
    lut[52] := makeRGB(140,240,86);
    lut[53] := makeRGB(237,11,182);
    lut[54] := makeRGB(242,140,108);
    lut[55] := makeRGB(248,21,77);
    lut[56] := makeRGB(161,42,89);
    lut[57] := makeRGB(189,22,112);
    lut[58] := makeRGB(41,241,59);
    lut[59] := makeRGB(114,61,125);
    lut[60] := makeRGB(65,99,226);
    lut[61] := makeRGB(121,115,50);
    lut[62] := makeRGB(97,199,205);
    lut[63] := makeRGB(50,166,227);
    lut[64] := makeRGB(238,114,125);
    lut[65] := makeRGB(149,190,128);
    lut[66] := makeRGB(44,204,104);
    lut[67] := makeRGB(214,60,27);
    lut[68] := makeRGB(124,233,59);
    lut[69] := makeRGB(167,66,66);
    lut[70] := makeRGB(40,115,53);
    lut[71] := makeRGB(167,230,133);
    lut[72] := makeRGB(127,125,159);
    lut[73] := makeRGB(178,103,203);
    lut[74] := makeRGB(231,203,97);
    lut[75] := makeRGB(30,125,125);
    lut[76] := makeRGB(173,13,139);
    lut[77] := makeRGB(244,176,159);
    lut[78] := makeRGB(193,94,158);
    lut[79] := makeRGB(203,131,7);
    lut[80] := makeRGB(204,39,215);
    lut[81] := makeRGB(238,198,47);
    lut[82] := makeRGB(139,167,140);
    lut[83] := makeRGB(135,124,226);
    lut[84] := makeRGB(71,67,223);
    lut[85] := makeRGB(234,175,231);
    lut[86] := makeRGB(234,254,44);
    lut[87] := makeRGB(217,1,110);
    lut[88] := makeRGB(66,15,184);
    lut[89] := makeRGB(14,198,61);
    lut[90] := makeRGB(129,62,233);
    lut[91] := makeRGB(19,237,47);
    lut[92] := makeRGB(97,159,67);
    lut[93] := makeRGB(165,31,148);
    lut[94] := makeRGB(112,218,22);
    lut[95] := makeRGB(244,58,120);
    lut[96] := makeRGB(35,244,173);
    lut[97] := makeRGB(73,47,156);
    lut[98] := makeRGB(192,61,117);
    lut[99] := makeRGB(12,67,181);
    lut[100] := makeRGB(149,94,94);
    for i := 1 to 100 do
        lut[i+100] := lut[i]; //fill 101..200
    for i := 1 to 55 do
        lut[i+200] := lut[i]; //fill 201..255
    result := lut;
    //saveLUT(lut);
end;

procedure TCLUT.SetLabels();
begin
     fIsLabels := true;
     fLUT := fLUTlabel;
     fNeedsUpdate := true;
end;

(*procedure TCLUT.SetTagX(cTag: integer);
begin
  CLUT.tag := cTag;
end; *)

procedure desaturateRGBA( var lRGBA: TRGBA; frac: single);
var
  r,g,b: byte;
  y: single;
begin
  r := lRGBA.r;
  g := lRGBA.g;
  b := lRGBA.b;
  //convert RGB->YUV http://en.wikipedia.org/wiki/YUV
  y := 0.299 * r + 0.587 * g + 0.114 * b;
  r := round(y * (1-frac) + r * frac);
  g := round(y * (1-frac) + g * frac);
  b := round(y * (1-frac) + b * frac);
  lRGBA.r := r;
  lRGBA.g := g;
  lRGBA.b := b;
end;

procedure  TCLUT.GenerateLUT(saturation: float = 1.0; LabelAlpha: integer = -1);
var
  lSlope: single;
  lSpace,lI,lIprev,lS: integer;
  lMin,lMax: TCLUTnode;
  lPreInvert: TLUT;
begin
  if (fIsLabels) then begin
     //GLForm1.Caption := floattostr(saturation) +'+-->'+inttostr(random(888));
     fLUT := fLUTlabel;
     fLUT[0] := SetRGBA(fBackColor.r,fBackColor.g, fBackColor.b,0);
     if (LabelAlpha >= 0) and (LabelAlpha <= 255) then
        for lI := 1 to 255 do
            if (fLUT[lI].a > 0) then
               fLUT[lI] := SetRGBA(fLUT[lI].r,fLUT[lI].g, fLUT[lI].b, LabelAlpha);
     if (saturation < 0) or (saturation >= 1.0) then
        exit;
     for lI := 1 to 255 do
        desaturateRGBA(fLUT[lI], saturation);
     exit;
  end;
  if CLUT.numNodes < 2 then exit;
  lMin := CLUT.nodes[0];
  lMax := CLUT.nodes[CLUT.NumNodes-1];
  //check that nodes are in order...
  lIprev := lMin.intensity;
  for lI := 1 to (CLUT.numnodes-1) do begin
    if CLUT.nodes[lI].intensity <= lIprev then begin
      showmessage('Error, nodes not sorted or overlapping.');
      exit;
    end;
    lIprev := CLUT.nodes[lI].intensity;
  end;
  //clip values <= lMin to value of lMin
  for lI := 0 to lMin.Intensity do begin
    fLUT[lI] := lMin.rgba;
    //render with white background
    //if (fLUT[lI].A= 0) then  fLUT[lI] := SetRGBA(fBackColor.r,fBackColor.g, fBackColor.b,0); //some clear nodes have RGB values to help interpolation
  end;
  //clip values >= lMax to value of lMin
  for lI := lMax.Intensity to 255 do begin
    fLUT[lI] := lMax.rgba;
  end;
  for lI := 0 to (CLUT.NumNodes-2) do begin
    lSpace := CLUT.nodes[lI+1].Intensity-CLUT.nodes[lI].Intensity;
    //interpolate red
    lSlope := (CLUT.nodes[lI+1].rgba.R-CLUT.nodes[lI].rgba.R)/lSpace;
    for lS := 1 to lSpace do
      fLUT[CLUT.nodes[lI].Intensity+lS].R  := CLUT.nodes[lI].rgba.R + round(lS * lSlope);
    //interpolate green
    lSlope := (CLUT.nodes[lI+1].rgba.G-CLUT.nodes[lI].rgba.G)/lSpace;
    for lS := 1 to lSpace do
      fLUT[CLUT.nodes[lI].Intensity+lS].G  := CLUT.nodes[lI].rgba.G + round(lS * lSlope);
    //interpolate blue
    lSlope := (CLUT.nodes[lI+1].rgba.B-CLUT.nodes[lI].rgba.B)/lSpace;
    for lS := 1 to lSpace do
      fLUT[CLUT.nodes[lI].Intensity+lS].B  :=CLUT.nodes[lI].rgba.B + round(lS * lSlope);
    //interpolate alpha
    lSlope := (CLUT.nodes[lI+1].rgba.A-CLUT.nodes[lI].rgba.A)/lSpace;
    for lS := 1 to lSpace do
      fLUT[CLUT.nodes[lI].Intensity+lS].A  :=CLUT.nodes[lI].rgba.A + round(lS * lSlope);
  end;
  if InvertColorMap then begin
    for lS := 0 to 255 do
      lPreInvert[lS] := fLUT[lS];
    for lS := 0 to 255 do begin
         fLUT[lS].R := lPreInvert[255-lS].R;
         fLUT[lS].G := lPreInvert[255-lS].G;
         fLUT[lS].B := lPreInvert[255-lS].B;
    end;
         fLUT[255].R := lPreInvert[1].R;
         fLUT[255].G := lPreInvert[1].G;
         fLUT[255].B := lPreInvert[1].B;
  end;
end;

(*procedure  TCLUT.GenerateLUT(lNodeRA: TCLUTrec; var lCLUT: TLUT);
var
  lSlope: single;
  lSpace,lI,lIprev,lS: integer;
  lMin,lMax: TCLUTnode;
begin
  if (fIsLabels) then begin
     GLForm1.Caption := '-->'+inttostr(random(888));
     lSlope := abs(CLUT.Max-CLUT.Min)/100;
     lCLUT := defaultLabelLut;
     if (lSlope < 0) or (lSlope >= 1.0) then
        exit;
     for lI := 1 to 255 do
        desaturateRGBA(lCLUT[lI], lSlope);
     exit;
  end;
  if lNodeRA.numNodes < 2 then exit;
  lMin := lNodeRA.nodes[0];
  lMax := lNodeRA.nodes[lNodeRA.NumNodes-1];
  //check that nodes are in order...
  lIprev := lMin.intensity;
  for lI := 1 to (lNodeRA.numnodes-1) do begin
    if lNodeRA.nodes[lI].intensity <= lIprev then begin
      showmessage('Error, nodes not sorted or overlapping.');
      exit;
    end;
    lIprev := lNodeRA.nodes[lI].intensity;
  end;
  //clip values <= lMin to value of lMin
  for lI := 0 to lMin.Intensity do begin
    lCLUT[lI] := lMin.rgba;
    if (lCLUT[lI].A= 0) then  lCLUT[lI] := SetRGBA(0,0,0,0); //some clear nodes have RGB values to help interpolation
  end;
  //clip values >= lMax to value of lMin
  for lI := lMax.Intensity to 255 do begin
    lCLUT[lI] := lMax.rgba;
  end;
  for lI := 0 to (lNodeRA.NumNodes-2) do begin
    lSpace := lNodeRA.nodes[lI+1].Intensity-lNodeRA.nodes[lI].Intensity;
    //interpolate red
    lSlope := (lNodeRA.nodes[lI+1].rgba.R-lNodeRA.nodes[lI].rgba.R)/lSpace;
    for lS := 1 to lSpace do
      lCLUT[lNodeRA.nodes[lI].Intensity+lS].R  :=lNodeRA.nodes[lI].rgba.R + round(lS * lSlope);
    //interpolate green
    lSlope := (lNodeRA.nodes[lI+1].rgba.G-lNodeRA.nodes[lI].rgba.G)/lSpace;
    for lS := 1 to lSpace do
      lCLUT[lNodeRA.nodes[lI].Intensity+lS].G  :=lNodeRA.nodes[lI].rgba.G + round(lS * lSlope);
    //interpolate blue
    lSlope := (lNodeRA.nodes[lI+1].rgba.B-lNodeRA.nodes[lI].rgba.B)/lSpace;
    for lS := 1 to lSpace do
      lCLUT[lNodeRA.nodes[lI].Intensity+lS].B  :=lNodeRA.nodes[lI].rgba.B + round(lS * lSlope);
    //interpolate alpha
    lSlope := (lNodeRA.nodes[lI+1].rgba.A-lNodeRA.nodes[lI].rgba.A)/lSpace;
    for lS := 1 to lSpace do
      lCLUT[lNodeRA.nodes[lI].Intensity+lS].A  :=lNodeRA.nodes[lI].rgba.A + round(lS * lSlope);
  end;
end;*)

function Node(lIntensity,lR,lG,lB,lA: byte): TCLUTnode;
begin
  result.intensity := lIntensity;
  result.rgba := SetRGBA(lR,lG,lB,lA);
end;

procedure TCLUT.AddNode(intensity, A: byte);
var
  i,n: integer;
begin
     if CLUT.numnodes < 2 then exit;
     fNeedsUpdate := true;
     n := 0;
     while (n < CLUT.numnodes) and (CLUT.nodes[n].intensity < intensity) do
           n := n + 1;
     if (CLUT.nodes[n].intensity = intensity) or (n <= 0) then  //change node alpha, no new node
       CLUT.nodes[n].rgba.a := A
     else begin
          inc(CLUT.numNodes);
          for i := (CLUT.numnodes-1) downto (n+1) do
              CLUT.nodes[i] := CLUT.nodes[i-1];
          CLUT.nodes[n].intensity := intensity;
          CLUT.nodes[n].rgba.A := A;
          if (n > 0) and (n < (CLUT.numnodes-1)) then begin
             CLUT.nodes[n].rgba.r := (CLUT.nodes[n-1].rgba.r + CLUT.nodes[n+1].rgba.r) div 2;
             CLUT.nodes[n].rgba.g := (CLUT.nodes[n-1].rgba.g + CLUT.nodes[n+1].rgba.g) div 2;
             CLUT.nodes[n].rgba.b := (CLUT.nodes[n-1].rgba.b + CLUT.nodes[n+1].rgba.b) div 2;
          end;
     end;
    GenerateLUT();//CLUT,fLUT);
end;

function TCLUT.DeleteNode (index: integer): boolean;
//deletes a node from color table: will not delete 1st or final node
var
  lN: integer;
begin
  if (index <= 0) or (index >= (CLUT.numnodes-1)) or (CLUT.numnodes < 3) then
    exit(false); //out of range or edge node...
  fNeedsUpdate := true;
  dec(CLUT.numnodes); //if 3 now only 2...
  if index < (CLUT.numnodes) then begin
    for lN := index to (CLUT.numnodes-1) do
      CLUT.nodes[lN] := CLUT.nodes[lN+1];
  end;
  GenerateLUT();//CLUT,fLUT);
  exit(true);
end;

procedure TCLUT.ChangeNode(index, intensity, R,G,B,A: byte);
//index is node number, 0..(numnodes-1)
var
  inten: integer;
begin
  if (index >= CLUT.numnodes) then
    exit;
  fNeedsUpdate := true;
  inten := intensity;
  //window spans from zero to 255, so bounding nodes must be in this range....
  if index = 0 then
    inten := 0;
  if index = (CLUT.numnodes-1) then
    inten := 255;
  //we need to keep the nodes in order...
  if (index > 0) and (inten <= CLUT.nodes[index-1].intensity) then
    inten := CLUT.nodes[index-1].intensity + 1;
  if (index < (CLUT.numnodes-1)) and (inten >= CLUT.nodes[index+1].intensity) then
    inten := CLUT.nodes[index+1].intensity - 1;
  CLUT.nodes[index].intensity := inten;
  CLUT.nodes[index].rgba := SetRGBA(R,G,B,A);
  GenerateLUT();//CLUT,fLUT);
end;

procedure AutoContrast (var lCLUTrec: TCLUTrec);
begin
  lCLUTrec.nodes[0] := node(0,0,0,0,0);
  lCLUTrec.nodes[1] := node(128,128,128,128,84);
  lCLUTrec.nodes[2] := node(255,255,255,255,168);
  lCLUTrec.numnodes := 3;
  lCLUTrec.min := 0;
  lCLUTrec.max := 0;
  //RangeRec(gTexture3D.MinThreshScaled,gTexture3D.MaxThreshScaled);
end;

procedure DrawingContrast (var lCLUTrec: TCLUTrec);
begin
  lCLUTrec.nodes[0] := node(0, 0,0,0,0);
(*  lCLUTrec.nodes[1] := node(1, 255,0,0,255);//red
  lCLUTrec.nodes[2] := node(56, 0,128,0,255);//green
  lCLUTrec.nodes[3] := node(85, 0,0,255,255);//blue
  lCLUTrec.nodes[4] := node(113,255,128,0,255);//orange
  lCLUTrec.nodes[5] := node(141,128,0,255,255);//purple
  lCLUTrec.nodes[6] := node(170,0,200,200,255);//cyan
  lCLUTrec.nodes[7] := node(198,160,48,48,255);//brick
  lCLUTrec.nodes[8] := node(226,32,255,32,255);//lime
  lCLUTrec.nodes[9] := node(255, 128,160,230,255);//lightblue   *)
  lCLUTrec.nodes[1] := node(1, 255,0,0,255);//red
  lCLUTrec.nodes[2] := node(2, 0,128,0,255);//green
  lCLUTrec.nodes[3] := node(3, 0,0,255,255);//blue
  lCLUTrec.nodes[4] := node(4,255,128,0,255);//orange
  lCLUTrec.nodes[5] := node(5,128,0,255,255);//purple
  lCLUTrec.nodes[6] := node(6,0,200,200,255);//cyan
  lCLUTrec.nodes[7] := node(7,160,48,48,255);//brick
  lCLUTrec.nodes[8] := node(8,32,255,32,255);//lime
  lCLUTrec.nodes[9] := node(9, 128,160,230,255);//lightblue
  lCLUTrec.nodes[10] := node(255, 128,160,230,255);//lightblue
  lCLUTrec.numnodes := 11;
  lCLUTrec.min := 0;
  lCLUTrec.max := 9;
end;
procedure IniFloat(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: single);
//read or write an integer value to the initialization file
var
	lStr: string;
begin
        if not lRead then begin
           lIniFile.WriteString('FLT',lIdent,FloattoStr(lValue));
           exit;
        end;
	lStr := lIniFile.ReadString('FLT',lIdent, '');
	if length(lStr) > 0 then
		lValue := StrToFloat(lStr);
end; //IniFloat

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

procedure IniByte(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: byte);
//read or write an integer value to the initialization file
var
	lStr: string;
begin
        if not lRead then begin
           lIniFile.WriteString('BYT',lIdent,InttoStr(lValue));
           exit;
        end;
	lStr := lIniFile.ReadString('BYT',lIdent, '');
	if length(lStr) > 0 then
		lValue := StrToInt(lStr);
end; //IniByte

function RGBAToStr (lU: TRGBA) : string;
const
  kStrSep = '|';
  //floatrect values 0..1 convert to byte 0..1
begin
  result := Inttostr(lU.R)+ kStrSep+Inttostr(lU.G)+ kStrSep+Inttostr(lU.B)+ kStrSep+Inttostr(lU.A);
end;

function StrToRGBA(lS: string; out lU: TRGBA): boolean;
var
  lV: string;
  lI: byte;
  lLen,lP,lN: integer;
begin
  result := false;
  lLen := length(lS);
  if lLen < 7 then  //shortest possible: 1|1|1|1 or 0|0|0|0
    exit;
  //read values
  lV := '';
  lP := 1;
  lN := 0;
  while (lP <= lLen) do begin
    if lS[lP] in ['0'..'9'] then
      lV := lV + lS[lP];
    if (lV <> '') and ((lP = lLen) or (not (lS[lP] in ['0'..'9']))) then begin
        inc(lN);
        lI := Min(Max(strtoint(lV),0), 255);
        case lN of
          1: lU.R := lI;
          2: lU.G := lI;
          3: lU.B := lI;
          4: lU.A := lI;
        end;
        lV := '';
    end;
    inc(lP);
  end;
  if lN >= 4 then
    result := true;
end;

procedure IniRGBA(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: TRGBA);
//read or write an integer value to the initialization file
var
	lStr: string;
begin
  if not lRead then begin
    lIniFile.WriteString('RGBA255',lIdent,RGBAToStr(lValue));
    exit;
  end;
	lStr := lIniFile.ReadString('RGBA255',lIdent, '');
  StrToRGBA(lStr,lValue);
end; //IniRGBA

function CLUT2disk(lRead: boolean; lFilename: string; var lCLUTrec: TCLUTrec): boolean;
//Read or write initialization variables to disk
var
  lIniFile: TIniFile;
  lI: integer;
begin
  result := false;
  if lRead then
    AutoContrast(lCLUTrec);
  if lFilename = '-' then begin
     DrawingContrast(lCLUTrec);
     exit;
  end;
  if (lRead) and (not Fileexists(lFilename)) then
        exit;
  lIniFile := TIniFile.Create(lFilename);
  IniFloat(lRead,lIniFile, 'min',lCLUTrec.min);
  IniFloat(lRead,lIniFile, 'max',lCLUTrec.max);
  IniInt(lRead,lIniFile, 'numnodes',lCLUTrec.numnodes);
  if (lCLUTrec.numnodes > 1) and (lCLUTrec.numnodes <= 256) then begin
    for lI := 0 to (lCLUTrec.numnodes-1) do begin
      IniByte(lRead,lIniFile, 'nodeintensity'+inttostr(lI),lCLUTrec.nodes[lI].intensity);
      IniRGBA(lRead,lIniFile, 'nodergba'+inttostr(lI),lCLUTrec.nodes[lI].rgba);
    end;
    if lCLUTrec.nodes[lCLUTrec.numnodes-1].intensity < 255 then
       lCLUTrec.nodes[lCLUTrec.numnodes-1].intensity := 255;
  end else
    AutoContrast (lCLUTrec);
  lIniFile.Free;
  result := true;
end;

function CLUT2json(lFilename: string; var lCLUTrec: TCLUTrec): boolean;
//Save color tables as NiiVue format JSON
var
  i: integer;
  f: TextFile;
begin
  result := false;
  AssignFile(f, lFilename);
  rewrite(f);
  writeln(f,'{');
  writeln(f, format('"min": %g,', [lCLUTrec.min]));
  writeln(f, format('"max": %g,', [lCLUTrec.max]));
  //red
  write(f, '"R": [');
  for i := 0 to (lCLUTrec.numnodes-1) do begin
  	if (i > 0) then write(f, ', ');
    write(f, inttostr(lCLUTrec.nodes[i].rgba.r));
  end;
  writeln(f, '],');
  //green
  write(f, '"G": [');
  for i := 0 to (lCLUTrec.numnodes-1) do begin
  	if (i > 0) then write(f, ', ');
    write(f, inttostr(lCLUTrec.nodes[i].rgba.g));
  end;
  writeln(f, '],');
  //blue
  write(f, '"B": [');
  for i := 0 to (lCLUTrec.numnodes-1) do begin
  	if (i > 0) then write(f, ', ');
    write(f, inttostr(lCLUTrec.nodes[i].rgba.b));
  end;
  writeln(f, '],');
  //alpha
  write(f, '"A": [');
  for i := 0 to (lCLUTrec.numnodes-1) do begin
  	if (i > 0) then write(f, ', ');
    write(f, inttostr(lCLUTrec.nodes[i].rgba.a));
  end;
  writeln(f, '],');
  //intensity
  write(f, '"I": [');
  for i := 0 to (lCLUTrec.numnodes-1) do begin
  	if (i > 0) then write(f, ', ');
    write(f, inttostr(lCLUTrec.nodes[i].intensity));
  end;
  writeln(f, ']');
  writeln(f, '}');
  closefile(f);
  result := true;
end;

function TCLUT.SaveCLUT(clutFileName: string): boolean;
begin
  if upcase(ExtractFileExt(clutFileName))= '.JSON' then begin
  	exit(CLUT2json(clutFileName, CLUT));
  end;
  //lCLUTrec := CLUT;
  //lCLUTrec.min := 0;
  //lCLUTrec.Max := 0;
  result := CLUT2disk(false, clutFileName, CLUT);
end;

(*procedure export2JSON(fnm: string; var lCLUTrec: TCLUTrec);
begin
    if (fnm = '') then exit;
    fnm := changefileext(fnm,'.json');
    {$IFDEF UNIX}writeln('Saving '+fnm);{$ENDIF}
    CLUT2json(fnm, lCLUTrec);
end;*)

function TCLUT.OpenCLUT(clutFileName: string; cTag: integer): boolean;
begin
  if fIsDrawing then exit(false);
  CLUT.Tag := cTag;
 fNeedsUpdate := true;
 result := CLUT2disk(true, clutFileName, CLUT);
 //export2Json(clutFileName, CLUT);
 GenerateLUT();//CLUT,fLUT);
end;

constructor TCLUT.Create(clutFileName: string; cTag: integer); overload;
begin
  fBackColor := SetRGBA(0, 0, 0, 0);
  fIsLabels := false;
  InvertColorMap := false;
  fLUTlabel := defaultLabelLut;
  fLUTlabel[0] := fBackColor;
  fIsDrawing := false;
  OpenCLUT(clutFileName, cTag);
end;

constructor TCLUT.Create(isDrawing: boolean = false); overload;
begin
     if isDrawing then begin
        Create('-', 0);
        fIsDrawing := true;
     end else
         Create('', 0);
end;

end.
