unit graphTicks;

{$mode objfpc}{$H+}

interface

procedure SelectTicks(mn, mx: double; out ticMin, ticStep: double; out ticDecimals: integer);

implementation

uses
  //Dialogs, SysUtils,
  Math;

function tickSpacing(tickCount: integer; mn, mx: double; out ticMin: double): double;
//https://stackoverflow.com/questions/326679/choosing-an-attractive-linear-scale-for-a-graphs-y-axis
var
   pow10x, x, range, unroundedTickSize: double;
begin
     ticMin := mn;
     range := abs(mx-mn);
     if range = 0.0 then exit(0);
     unroundedTickSize  := range/(tickCount-1);
     x := ceil(log10(unroundedTickSize)-1);
     pow10x := power(10, x);
     result := ceil(unroundedTickSize / pow10x) * pow10x;
     //if mn < 0 then
     //   ticMin := ceil((mn+result) / result) * result
     //else
     if frac(mn / result) = 0.0 then exit;
     ticMin := floor((mn+result) / result) * result;
end;

procedure SelectTicks(mn, mx: double; out ticMin, ticStep: double; out ticDecimals: integer);
var
   imx, imn, range: double;
begin
     imn := min(mx,mn);
     imx := max(mx,mn);
     range := mx - mn;
     ticStep := tickSpacing(5, imn, imx, ticMin);
     if frac(range / ticStep) <> 0.0 then begin
        ticStep := tickSpacing(6, imn, imx, ticMin);
        if frac(range / ticStep) <> 0.0 then begin
           ticStep := tickSpacing(7, imn, imx, ticMin);
           if frac(range / ticStep) <> 0.0 then begin
              ticStep := tickSpacing(6, imn, imx, ticMin);
              //if (range / ticStep) <> 0.0 then begin
              //   ticStep := tickSpacing(5, imn, imx, ticMin);
              //end;
           end;
        end;
     end;
     ticDecimals := 0;
     if ticStep > 1 then exit;
     ticDecimals := abs(floor(log10(ticStep)));
end;

(*type
  TTicks = record
    stepSize, remainder: single;
    decimals: integer;
  end;

    function decimals(v: double): integer;
  var
    f: double;
  begin
    result := 0;
    f := frac(v);
    while (f > 0.001) and (f < 0.999) do begin
          v := v * 10;
          result := result + 1;
          f := frac(v);
    end;
  end;

  function fRemainder(const a,b:double):double;
  begin
    result := a-b * Int(a/b);
    if (result > (0.5 * b)) then result := b - result;
  end;

function setStepSize(lRange: double; lDesiredSteps: integer): TTicks;
var
   lPower: integer;
begin
  result.stepSize := lRange / lDesiredSteps;
  //{$DEFINE OLD}
  {$IFDEF OLD}
  lPower := 0;
  while result.stepSize >= 10 do begin
        result.stepSize := result.stepSize/10;
        inc(lPower);
  end;
  while result.stepSize < 1 do begin
       result.stepSize := result.stepSize * 10;
       dec(lPower);
  end;
  {$ELSE}
  lPower := floor(log10(result.stepSize));
  result.stepSize := result.stepSize/power(10, lPower);
  {$ENDIF}
  if lPower < 0 then
        result.decimals := abs(lPower)
  else
        result.decimals := 0;
  result.stepSize := round(result.stepSize) * Power(10,lPower);
  result.remainder := fRemainder(lRange, result.stepSize);
  if result.remainder < (0.001* result.stepSize) then
     result.remainder := 0;
end;

function setStepSizeForce(lRange: double; lDesiredSteps: integer): TTicks;
begin
  result.stepSize := lRange / lDesiredSteps;
  result.decimals := decimals(result.stepSize);
  result.remainder := fRemainder(lRange, result.stepSize);
  if result.remainder < (0.001* result.stepSize) then
     result.remainder := 0;
end;

procedure SelectTicks(mn, mx: double; out ticMin, ticStep: double; out ticDecimals: integer);
var
   lStep,lRange: double;
   tic, ticAlt: TTicks;
begin
  ticMin := 0;
  ticStep := 1;
  ticDecimals := 0;
  lRange := abs(mx - mn);
  if (mn < 0) and (mx > 0) then
     lRange := max(abs(mn),mx);// + (0.5 * min(abs(mn),mx));
  if lRange < 0.000001 then exit;
  if ((mn < 0) and (mx > 0)) and ((min(abs(mn),mx)/lRange) > 0.65)  then  begin
    tic := setStepSize(lRange, 2);
    //now try forcing other values
    ticAlt := setStepSizeForce(lRange, 3);
    if (ticAlt.remainder < tic.remainder) and (ticAlt.decimals <= tic.decimals) then
       tic := ticAlt;
    ticAlt := setStepSizeForce(lRange, 4);
    if (ticAlt.remainder < tic.remainder) and (ticAlt.decimals <= tic.decimals) then
       tic := ticAlt;
  end else begin
       tic := setStepSize(lRange, 3);
       //now try forcing other values
       ticAlt := setStepSizeForce(lRange, 2);
       if (ticAlt.remainder < tic.remainder) and (ticAlt.decimals <= tic.decimals) then
          tic := ticAlt;
       ticAlt := setStepSizeForce(lRange, 4);
       if (ticAlt.remainder < tic.remainder) and (ticAlt.decimals <= tic.decimals) then
          tic := ticAlt;
       ticAlt := setStepSizeForce(lRange, 1);
       if (ticAlt.remainder < tic.remainder) and (ticAlt.decimals < tic.decimals) then
          tic := ticAlt;
  end;
  if (mn > 0) and (decimals(mn) <= tic.decimals) then
     lStep := mn
  else
      lStep := trunc((mn)  / tic.stepSize)*tic.stepSize;
  if (lStep < (mn)) and ((mn -lStep) > (lStep * 0.001) ) then lStep := lStep+tic.stepSize;

  lStep := 400;
  //showmessage(format('%g %g', [lStep, mn]));
  //lRange := abs(mx - mn); //full range, in case mn < 0 and mx > 0
  ticMin := lStep;
  ticStep := tic.stepSize;
  ticDecimals := tic.decimals;
  //result := format('stepSize %g 1stTick %g decimals %d', [tic.stepSize, lStep, tic.decimals]);

end; *)


end.

