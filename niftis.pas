unit niftis; //manage a collection of nifti images
{$IFDEF FPC}{$mode delphi}{$H+}{$ENDIF}
interface
//manages multiple overlays
uses
  {$IFDEF CUSTOMCOLORS} colorTable,  {$ENDIF}
  {$IFDEF LCLCocoa}UserNotification, {$ELSE} TimedDialog,{$ENDIF}

  drawvolume, Clipbrd, SimdUtils, sysutils,Classes, nifti, Math,  dialogs;//, SimdTypes;
const
  kBackgroundLayer = 0;
  kMinOverlay = 1;
  kMaxOverlay = 32;
Type
  TNIfTIs = Class(TObject)  // This is an actual class definition :
      // Internal class field definitions - only accessible in this unit
      private
        fNumLayers: integer;
        fAdditiveOverlayBlending, fMaskWithBackground, fInterpolateOverlays : boolean;
        niis: array[0..kMaxOverlay] of TNIfTI;
        fDrawing: TDraw;
        procedure SkippedVolumesWarning();
      public
        LoadFewVolumes: boolean;
        MaxVox: integer; //maximum number of voxels in any dimension
        property Drawing: TDraw read fDrawing write fDrawing;
        property NumLayers: integer read fNumLayers;
        property InterpolateOverlays: boolean read fInterpolateOverlays write fInterpolateOverlays;
        property MaskWithBackground: boolean read fMaskWithBackground write fMaskWithBackground;
        property AdditiveOverlayBlending: boolean read fAdditiveOverlayBlending write fAdditiveOverlayBlending;
        function AddLayer(niftiFileName: string; backColor: TRGBA): boolean;
        function AddCorrelLayer(vox: TVec3i; backColor: TRGBA; isZ: boolean): boolean;
        function AddEdgeLayer(Idx: integer; backColor: TRGBA): boolean;
        function OpenDrawing(niftiFileName: string): boolean;
        function OverlaysNeedsUpdate: boolean;
        function Layer(idx: integer; out vol: TNIfTI): boolean;
        function SwapLayers(fromIdx, toIdx: integer): boolean;
        function CloseLayer(Idx: integer): boolean;
        function CreateOverlayVolume: TRGBAs;
        procedure CloseAllOverlays(); //keep background open
        procedure CloseAllLayers(); //close overlays and background
        procedure CloseDraw(); //close overlays and background
        constructor Create(); overload;
        constructor Create(niftiFileName: string; backColor: TRGBA; lLoadFewVolumes: boolean; lMaxVox: integer; out isOK: boolean); overload;
        destructor Destroy; override;
  end;

implementation

uses nifti_types , mainunit;

procedure printerror(msg: string);
begin
     {$IFDEF UNIX}
     writeln(msg);
     {$ELSE}
     showmessage(msg);
     {$ENDIF}
end;

procedure printf(msg: string);
begin
     {$IFDEF UNIX}
     writeln(msg);
     {$ENDIF}
end;

function mixRGB(rgbaOld, rgba: TRGBA): TRGBA;
var
  frac, frac1: single;
begin
  if rgba.a = 0 then exit(rgbaOld); //transparent
  if rgba.a = 255 then exit(rgba); //opaque
  if rgbaOld.a = 0 then exit(rgba);
  //else some of each: blena
  frac := rgba.a/255;
  frac1 := 1 - frac;
  result.r := round((frac * rgba.r) + (frac1 * rgbaOld.r));
  result.g := round((frac * rgba.g) + (frac1 * rgbaOld.g));
  result.b := round((frac * rgba.b) + (frac1 * rgbaOld.b));
  result.a := max(rgbaOld.a, rgba.a);
end;

function addRGB(rgbaOld, rgba: TRGBA): TRGBA;
begin
  result.r := max(rgbaOld.r, rgba.r);
  result.g := max(rgbaOld.g, rgba.g);
  result.b := max(rgbaOld.b, rgba.b);
  result.a := max(rgbaOld.a, rgba.a);
end;

function TNIfTIs.SwapLayers(fromIdx, toIdx: integer): boolean;
var
  tmp: TNIfTI;
begin
     result := false;
  if (fromIdx < 1) or (toIdx < 1) then exit;
  if (fromIdx >= fNumLayers) or (toIdx >= fNumLayers) then exit;
  tmp := niis[fromIdx];
  niis[fromIdx] := niis[toIdx];
  niis[toIdx] := tmp;
  result := true;
end;

function TNIfTIs.CloseLayer(Idx: integer): boolean;
var
  i: integer;
begin
  result := false;
  if (Idx < 1) then exit;
  if (Idx >= fNumLayers) then exit;
  niis[Idx].Destroy;
  result := true;
  fNumLayers := fNumLayers - 1;
  if (Idx >= fNumLayers) then exit;
  for i := Idx to (fNumLayers-1) do
      niis[i] := niis[i + 1];
  niis[Idx].ForceUpdate();
end;

function TNIfTIs.OverlaysNeedsUpdate: boolean;
var
  i: integer;
begin
  result := false;
  //if (fDrawing.voiActiveX) and (fDrawing.NeedsUpdate) then result := true;
  if fNumLayers < 2 then exit; //no overlays loaded
  for i := 1 to (fNumLayers-1) do //-1: ignore background layer 0
      if (niis[i].NeedsUpdate) then
         result := true;
end;

function TNIfTIs.CreateOverlayVolume: TRGBAs;
var
  i, j, k, vx, nOK, nUsed: integer;
  vol: TNIfTI;
  frac, mn, mx: single;
  lut, lutin: TLUT;
  vol8: TUInt8s;
  vol8bg: TUInt8s;
  //vol24: TRGBAs;
  rgba : TRGBA;
  pct255: byte;
begin
     result := nil;
     if (fNumLayers < 2) then exit; //no overlays loaded
     vol := niis[0]; //background
     vx := (vol.dim.x*vol.dim.y*vol.dim.z);
     setlength(result, vx);
     //lut[0] := vol.CX.BackColor; //<- better for 2D slices
     lut[0] := setRGBA(0,0,0,0); //<- better for render
     lut[0].a := 0; //BGxxxxx
     for j := 0 to vx -1 do
         result[j] := lut[0];
     nOK := 0;
     nUsed := 0;
     vol := niis[0]; //background
     vol8bg := vol.DisplayMinMax2Uint8;
     //vol24 := vol.VolRGBA; //background rgba
     if (fNumLayers > 1) then begin
       for i := 1 to (fNumLayers-1) do begin//-1: ignore background layer 0
           vol := niis[i]; //ith overlay
           if vx <> (vol.dim.x*vol.dim.y*vol.dim.z) then continue;
           nOK := nOK + 1;
           if (vol.OpacityPercent = 0) then continue;
           nUsed := nUsed + 1;
           if (vol.Header.datatype =  kDT_RGB) or (vol.Header.datatype =  kDT_RGBA32) then begin
           	   pct255 := round(255 * (vol.OpacityPercent/100));
               vol.DisplayRGB();
               if vol.VolRGBA = nil then continue;
               (*for j := 0 to (vx  -1) do begin
               	   result[j] := vol.VolRGBA[j];
                   if result[j].a > 0 then
                   	  result[j].a := pct255;
               end;*)
               if (fMaskWithBackground) and (vol8bg <> nil) then begin //fMaskWithBackground then begin
                             if fAdditiveOverlayBlending then begin
                                for j := 0 to (vx  -1) do begin
                                    if vol8bg[j] = 0 then continue;
                                    rgba := vol.VolRGBA[j];
                                    if rgba.a > 0 then
                                    	rgba.a := pct255;
                                    result[j] := addRGB(result[j], rgba);
                                end;
                             end else begin
                               for j := 0 to (vx  -1) do begin
                                   //if (vol24[j].A = 0) then continue;
                                   if vol8bg[j] = 0 then continue;
                                    rgba := vol.VolRGBA[j];
                                    if rgba.a > 0 then
                                    	rgba.a := pct255;
                                   result[j] := mixRGB(result[j], rgba);
                               end;
                             end; //if additive else end
                          end else begin
                            if fAdditiveOverlayBlending then begin
                               for j := 0 to (vx  -1) do begin
                                    rgba := vol.VolRGBA[j];
                                    if rgba.a > 0 then
                                    	rgba.a := pct255;
                                   result[j] := addRGB(result[j], rgba);
                               end;
                            end else begin
                              for j := 0 to (vx  -1) do begin
                                    rgba := vol.VolRGBA[j];
                                    if rgba.a > 0 then
                                    	rgba.a := pct255;
                                  result[j] := mixRGB(result[j], rgba);
                              end;
                            end; //if additive else end
                          end;
               vol.GPULoadDone();
               continue;
               //
           end;
           vol8 := vol.DisplayMinMax2Uint8;
           lut := vol.GetColorTable;
           lut[0] := setRGBA(lut[1].r, lut[1].g, lut[1].b, 0); //<- better for render
           mn := min(vol.DisplayMin, vol.DisplayMax);
           mx := max(vol.DisplayMin, vol.DisplayMax);
           if (vol.CX.FromZero) and (mn <> mx) and ((mn > 0) or (mx < 0)) then begin //range does not cross zero
              if (mn > 0) then
                 frac := mn/mx
              else
                  frac := abs(mx)/abs(mn);
              lutin := lut;
              for j := 1 to 255 do begin
                  k := round (255 * (frac + ((1-frac) * j/255)));
                  lut[j] := lutin[k];
              end;
           end;
           pct255 := round(255 * (vol.OpacityPercent/100));
           for j := 1 to 255 do
                  lut[j].a := pct255;
           if (vol.DisplayMin < 0) and (vol.DisplayMax < 0) then begin
              lutin := lut;
              for j := 0 to 255 do
                  lut[j] := lutin[255-j];
           end;
           if (fMaskWithBackground) and (vol8bg <> nil) then begin //fMaskWithBackground then begin
              if fAdditiveOverlayBlending then begin
                 for j := 0 to (vx  -1) do begin
                     //if (vol24[j].A = 0) then continue;
                     if vol8bg[j] = 0 then continue;
                     rgba := lut[vol8[j]];
                     result[j] := addRGB(result[j], rgba);
                 end;
              end else begin
                for j := 0 to (vx  -1) do begin
                    //if (vol24[j].A = 0) then continue;
                    if vol8bg[j] = 0 then continue;
                    rgba := lut[vol8[j]];
                    result[j] := mixRGB(result[j], rgba);
                end;
              end; //if additive else end
           end else begin
             if fAdditiveOverlayBlending then begin
                for j := 0 to (vx  -1) do begin
                    rgba := lut[vol8[j]];
                    result[j] := addRGB(result[j], rgba);
                end;
             end else begin
               for j := 0 to (vx  -1) do begin
                   rgba := lut[vol8[j]];
                   result[j] := mixRGB(result[j], rgba);
                   //result[j] := addRGB(result[j], rgba);
               end;
             end; //if additive else end
           end;

       end;
     end;
     if (nUsed = 0) then begin
        {$IFDEF UNIX}printf('no overlays applied (all transparent?)'); {$ENDIF}
        result := nil;
        exit;
     end;
end;

function TNIfTIs.Layer(idx: integer; out vol: TNIfTI): boolean;
begin
     vol := nil;
     if (idx < 0) or (idx >= fNumLayers) then exit(false);
     vol := niis[idx];
     if vol = nil then exit(false);
     result := true;
end;

procedure TNIfTIs.CloseDraw();
begin
     Drawing.voiClose;
end;

procedure TNIfTIs.CloseAllOverlays();
//only keep background image open
var
  i : integer;
begin
     if fNumLayers < 2 then exit;
     for i := 2 to fNumLayers do
         niis[i-1].Destroy;
     CloseDraw;
     fNumLayers := 1;
end;
procedure TNIfTIs.CloseAllLayers();
//close overlays and background
var
  i : integer;
begin
     if fNumLayers < 1 then exit;
     for i := 1 to fNumLayers do
         niis[i-1].Destroy;
     CloseDraw;
     fNumLayers := 0;
end;

function TNIfTIs.OpenDrawing(niftiFileName: string): boolean;
var
  nii: TNIfTI;
  backColor : TRGBA;
begin
     //n.b. assumes caller closed previously edited drawing!
     result :=  (fileexists(niftiFileName));
     if not result then exit;
     backColor := setRGBA(0,0,0,0);
     nii := TNIfTI.Create(niftiFileName, backColor, niis[0].Mat, niis[0].Dim, false, result); //false: use nearest neighbor for VOI
     if not result then exit;
     //nii.Header.scl_inter := 0.0;
     //nii.Header.scl_slope := 1.0;
     if (nii.Header.datatype = kDT_UINT8) then
        Drawing.voiCreate(nii.Dim.x, nii.Dim.y, nii.Dim.z, nii.fRawVolBytes)
     else
         Drawing.voiCreate(nii.Dim.x, nii.Dim.y, nii.Dim.z, nii.DisplayMinMax2Uint8, true);
     Drawing.filename:= niftiFileName;
     nii.Destroy;
end;

procedure TNIfTIs.SkippedVolumesWarning();
var
  isSkipped : boolean = false;
  str, str2: string;
begin
  if (fNumLayers = 0) and (niis[fNumLayers].VolumesLoaded > 0) and (niis[fNumLayers].VolumesLoaded < niis[fNumLayers].Header.dim[4]) then
  isSkipped := true;
  if (not isSkipped) and (not niis[fNumLayers].IsShrunken) then exit;
  str := '';
  str2 := '';
  if (niis[fNumLayers].IsShrunken) then begin
     str := 'Large image downsampled. ';
     str2 := ' to load large textures.';
  end;
  if isSkipped then begin
     str := str + format('Loaded %d of %d volumes.', [niis[fNumLayers].VolumesLoaded, niis[fNumLayers].Header.dim[4]]);
     str2 := ' to load all volumes.';
  end;
  {$IFDEF LCLCocoa}

  DeliverUserNotification(str, 'Adjust preferences'+str2,'','');
  {$ELSE}
  TimedDialogForm.ShowTimedDialog('MRIcroGL Warning', str, 3000, GLForm1.Left+20, GLForm1.Top+20);
  {$ENDIF}
     //TimedDialogForm.ShowTimedDialog('MRIcroGL Warning', format('Only loaded %d of %d volumes. Adjust preferences to see all volumes (full graphs and calibration maps).', [niis[fNumLayers].VolumesLoaded, niis[fNumLayers].Header.dim[4]]), 3000);
end;

function TNIfTIs.AddLayer(niftiFileName: string; backColor: TRGBA): boolean;
begin
     result :=  (niftiFileName <> '') and (fileexists(niftiFileName));
     if (fNumLayers > kMaxOverlay) or (fNumLayers < 0) then exit;
     if fNumLayers = 0 then begin
        niis[fNumLayers] := TNIfTI.Create(niftiFileName, backColor, LoadFewVolumes, MaxVox, result);
     end else begin
        niis[fNumLayers] := TNIfTI.Create(niftiFileName, backColor, niis[0].Mat, niis[0].Dim, fInterpolateOverlays, result);
        if (result) and (niis[fNumLayers].IsLabels) then
          niis[fNumLayers].OpacityPercent := 50;
     end;
     SkippedVolumesWarning();
     if (not result) and (fNumLayers > 0) then
        niis[fNumLayers].Destroy //e.g. attempted to open RGB as overlay
     else
         fNumLayers := fNumLayers + 1;
end;


function TNIfTIs.AddEdgeLayer(Idx: integer; backColor: TRGBA): boolean;
var
  img8: TUInt8s;
  hdr: TNIFTIhdr;
  prefix: string;
begin
     result := false;
     if (fNumLayers > kMaxOverlay) or (fNumLayers < 1) then exit;
     img8 := niis[Idx].EdgeMap(false);
     if length(img8) < 1 then exit;
     hdr := niis[Idx].Header;
     hdr.intent_code := kNIFTI_INTENT_NONE;
     hdr.descrip := 'Sobel MRIcroGL'+kVers;
     prefix := 'edge_';
     niis[fNumLayers] := TNIfTI.Create(prefix+niis[Idx].shortname, backColor, niis[Idx].Mat, niis[Idx].Dim, fInterpolateOverlays, hdr, TFloat32s(img8), true);
     fNumLayers := fNumLayers + 1;
     setlength(img8,0);
     result := true;
end;

function TNIfTIs.AddCorrelLayer(vox: TVec3i; backColor: TRGBA; isZ: boolean): boolean;
var
  rs: TFloat32s;
  hdr: TNIFTIhdr;
  posStr, prefix,s: string;
begin
     result := false;
     if (fNumLayers > kMaxOverlay) or (fNumLayers < 1) then exit;
     if Drawing.IsOpen then begin
        posStr := '_roi';
        rs := niis[0].SeedCorrelationMap(Drawing.VolRawBytes, isZ);
     end else begin
         posStr := inttostr(vox.x)+'x'+inttostr(vox.y)+'x'+inttostr(vox.z);
         rs := niis[0].SeedCorrelationMap(vox, isZ);
     end;
     if length(rs) < 1 then exit;
     //n := length(rs);
     //for i := 0 to (n-1) do
     //    rs[i] := random();
     result := true;
     hdr := niis[0].Header;
     hdr.intent_code := kNIFTI_INTENT_CORREL;
     prefix := 'r';
     if isZ then begin
        prefix := 'z';
        hdr.intent_code := kNIFTI_INTENT_ZSCORE;
     end;
     s := '';
     if (niis[0].VolumesLoaded < niis[0].Header.dim[4]) then
        s := format('%dof%d_',[niis[0].VolumesLoaded, niis[0].header.dim[4]]);
     niis[fNumLayers] := TNIfTI.Create(prefix+PosStr+'_'+s+niis[0].shortname, backColor, niis[0].Mat, niis[0].Dim, fInterpolateOverlays, hdr, rs);
     //!TOBO niis[fNumLayers].SetDisplayColorScheme('8RedYell', 8);
     fNumLayers := fNumLayers + 1;
     setlength(rs,0);
end;

constructor TNIfTIs.Create(niftiFileName: string; backColor: TRGBA; lLoadFewVolumes: boolean; lMaxVox: integer; out isOK: boolean); overload;
begin
     LoadFewVolumes := lLoadFewVolumes;
     MaxVox := lMaxVox;
     fNumLayers := 0;
     niis[0] := TNIfTI.Create(niftiFileName, backColor, LoadFewVolumes, MaxVox, isOK);
     fDrawing := TDraw.Create();
     SkippedVolumesWarning();
     fNumLayers := 1;
     fAdditiveOverlayBlending := false;
     fMaskWithBackground := false;
     fInterpolateOverlays := true;
end;

constructor TNIfTIs.Create(); overload;
var
  isOK: boolean;
  backColor: TRGBA;
begin
    backColor := setRGBA(0,0,0,0);
    Create('', backColor, true, 512, isOK);
end;

destructor  TNIfTIs.Destroy;
begin
     CloseAllLayers();
     fDrawing.Destroy;
end;

end.
