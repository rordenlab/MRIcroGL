unit afni_fdr;

{$mode objfpc}{$H+}
//https://afni.nimh.nih.gov/pub/dist/doc/program_help/README.copyright.html
//The MCW-copyrighted part of this software is released to the public under the GNU General Public License, Version 2 (or any later edition).
//The MCW-copyrighted part of the documentation is released to the public under the Open Content License (OCL).

interface

uses
  Classes, SysUtils, nifti_foreign, ustat;
//see thd_fdrcurve.c
function q2VoxelIntensity(fv: TFloatVec; q: single; maxAbsVal: single; isVerbose : boolean = false): single;
function VoxelIntensity2q(fv: TFloatVec; val: single; isVerbose : boolean = false): single;
function THD_stat_to_pval( thr, df0, df1: single; statcode: integer; isVerbose : boolean = false ): single;

implementation

function THD_stat_to_pval( thr, df0, df1: single; statcode: integer; isVerbose : boolean = false ): single;
begin
	if( thr = 0.0 ) then
		exit(1.0);
	case ( statcode ) of  //statcode is illegal, will return -1
			kFUNC_ZT_TYPE: result := z2p(thr);
			kFUNC_TT_TYPE: result := t2p(thr, df0);
			kFUNC_FT_TYPE: result := f2p(thr, df0, df1);
			kFUNC_CT_TYPE: result := chi2p(thr, round(df0));
		else
			exit(1.0);
	end;
	if not isVerbose then exit;
	{$IFDEF Windows}
	if not isConsole then
		exit;
	{$ENDIF}
	case ( statcode ) of  
		kFUNC_ZT_TYPE: writeln(format('z=%g, p<%g', [thr, result] ));
		kFUNC_TT_TYPE: writeln(format('t(%d)=%g, p<%g', [round(df0), thr, result]));
		kFUNC_FT_TYPE: writeln(format('f(%d,%d)=%g, p<%g', [round(df0), round(df0), thr, result]));
		kFUNC_CT_TYPE: writeln(format('chi(%d)=%g, p<%g', [round(df0), thr, result]));
	end;
end;


function P_M1(x: single): single; inline;
begin
  result := ((x)*(1.0-(x))*((x)-2.0)*0.1666667);
end;

function P_00(x: single): single; inline;
begin
  result := (((x)+1.0)*((x)-1.0)*((x)-2.0)*0.5);
end;

function P_P1(x: single): single; inline;
begin
  result := ((x)*((x)+1.0)*(2.0-(x))*0.5);
end;

function P_P2(x: single): single; inline;
begin
  result := ((x)*((x)+1.0)*((x)-1.0)*0.1666667);
end;

//Cubic interpolation in a floatvec
function interp_floatvec(constref fv: TFloatVec; x: single): single; //mri_floatvec.c
var
  ix , im1,ip1,ip2 , itop: integer;
  fx , val , abot,atop: single;
begin
  if fv.ar = nil then
    exit(0.0);

  itop := fv.nar - 1 ;
  if (itop <= 1) or (fv.dx = 0.0) then
    exit(fv.ar[0]);
  //if input x is out of range, return the edge value }
  fx := (x - fv.x0) / fv.dx;
  if fx <= 0.0 then
    exit(fv.ar[0])
  else if fx >= itop then
    exit(fv.ar[itop]);
  //input x is between point #ix and #ix+1 fractional offset between them is fx
  ix := trunc(fx);
  fx := fx - ix;
  //get indexes below (im1) and above (ip1 and ip2)
  im1 := ix-1;
  if im1 < 0 then
    im1 := 0;
  ip1 := ix+1 ;
  if ip1 > itop then
    begin
      ip1 := itop;
      ip2 := itop;
    end
  else
    begin
      ip2 := ip1 + 1;
      if ip2 > itop then
        ip2 := itop;
    end;
  //cubic interpolation between these 4 points
  val := P_M1(fx)*fv.ar[im1] + P_00(fx)*fv.ar[ix]
       + P_P1(fx)*fv.ar[ip1] + P_P2(fx)*fv.ar[ip2];
  //make sure result lies in the local range of values
  abot := fv.ar[ix];
  atop := fv.ar[ip1];
  if abot > atop then
    begin
      fx := abot;
      abot := atop;
      atop := fx;
    end;
  if val < abot then
    begin
      val := abot;
    end
  else if (val > atop) then
    val := atop;
  exit(val);
end;

function regula_falsi_step(constref fv: TFloatVec; y, x0, x1: single): single; //mri_floatvec.c
var
  y0 , y1 , dy: single;
begin
  y0 := interp_floatvec(fv,x0);
  y1 := interp_floatvec(fv,x1);
  dy := y1-y0;
  if (dy = 0.0) or (abs(dy) < 0.00666 * (abs(y-y0) + abs(y-y1))) then
    exit(x0);
  dy := x0 + (x1-x0)/dy * (y-y0);
  exit(dy);
end;

function interp_inverse_floatvec(constref fv: TFloatVec; y: single): single; //mri_floatvec.c
var
  ip,itop: integer;
  ym,yp,dx: single;
  x0,x1,x2: single;
  xm,xp,y0: single;
begin
  { check for stoopid inputs }
  itop := fv.nar - 1;
  if ((fv.ar = nil) or (itop <= 1) or (fv.dx = 0.0)) then
    exit(fv.x0);
  { off the left edge? }
  if (((fv.ar[0] < fv.ar[itop]) and (y <= fv.ar[0])) or
     ((fv.ar[0] > fv.ar[itop]) and (y >= fv.ar[0]))) then
    exit(fv.x0);
  { off the right edge? }
  if (((fv.ar[0] < fv.ar[itop]) and (y >= fv.ar[itop])) or
      ((fv.ar[0] > fv.ar[itop]) and (y <= fv.ar[itop]))) then
    exit(fv.x0+fv.dx*itop);
  { find the intermediate point that brackets the desired result
   [27 Feb 2014] -- replace simple linear interpolation with
     linear interpolation plus a regula falsi step for improvement
     (since the forward interpolation method is cubic, not linear). }
  for ip := 1 to itop - 1 do
    begin
      ym := fv.ar[ip-1];
      yp := fv.ar[ip];
      { the desired y is now bracketed }
      if (y-ym) * (y-yp) <= 0.0 then
        begin
          dx := (y-ym) / (yp-ym);

          x0 := fv.x0 + fv.dx *(ip-1.0+dx);
          y0 := interp_floatvec(fv,x0);
          x1 := x0 + 0.05 * fv.dx;   // try nearby points above and below
          x2 := x0 - 0.05 * fv.dx;   // and then regula falsi from them
          xp := regula_falsi_step(fv,y,x0,x1);
          yp := interp_floatvec(fv,xp);
          xm := regula_falsi_step(fv,y,x0,x2);
          ym := interp_floatvec(fv,xm);

          {$ifdef DO_FIVE}
            { extra regula falsi steps (not needed IMHO) }
          {$else}
            yp := abs(yp-y);
            ym := abs(ym-y);
            y0 := abs(y0-y);
            if (yp < ym) and (yp < y0) then
              exit(xp) //yp is smallest residual
            else if (ym < yp) and (ym < y0) then
              exit(xm) //ym is smallest residual
            else
              exit(x0);//y0 is smallest residual
          {$endif}
        end;
    end;
  { should never happen }
  exit(fv.x0 + fv.dx * 0.5*itop );  // the midpoint
end;

function q2VoxelIntensity(fv: TFloatVec; q: single; maxAbsVal: single; isVerbose : boolean = false): single;//thd_fdrcurve.c
var
  zval: single;
  itop: integer;
  xtop: single;
begin
     //zval = qginv(0.5*val) ;
     //qval = THD_fdrcurve_zqtot( dset , ival , zval ) ;
     zval := inversez(0.5* q);//qginv
     itop := fv.nar - 1;
     if (zval <= fv.ar[0]) then
        result := 0.0
     else if (zval >= fv.ar[itop]) then begin
          result := fv.x0 + fv.dx*fv.nar ;
          if( maxAbsVal >= result ) then
            result := maxAbsVal*1.000002 ;
     end else
       result := interp_inverse_floatvec(fv,zval);
     if not isVerbose then exit;
     {$IFDEF Windows}
     if not isConsole then
     	exit;
     {$ENDIF}    
     xtop := fv.x0+(fv.dx*itop);
     writeln(format('zval=%.4f x0=%.8f z0=%.7f xtop=%.4f ztop=%.4f thresh=%.4f' ,[zval, fv.x0, fv.ar[0], xtop, fv.ar[itop], result ]));
end;

{$DEFINE AFNI_APPROX} //use same z2p as AFNI for identical results
{$IFDEF AFNI_APPROX}
(*Unit to implement some probability functions, v. 1.0.
Copyright (C) 2005 Franco Milani
email: nadir@solaris.polarhome.com or astlab@inwind.it
web sites:
http://www.polarhome.com:793/~franco/
http://www.polarhome.com/~franco
http://www.polarhome.com:723/~nadir/software
http://spazioinwind.libero.it/frm/software
-----------------------------------------------------------------------------
This software is released in the hope that it will be useful but on an as-is
basis, so without any warranty, either expressed or implied. It can be used
and modified for personal purposes, or linked to distributed programs,
in which case an acknowledgment will be appreciated. This software can also
be redistributed, but only in the original form. Possible changes in the
distributed versions will be made only by the author, also according to
hints, requests or bug reports.
-----------------------------------------------------------------------------
This unit contains some functions useful in probability and statistics.
To achieve the highest possible precision with extended real variables,
the routines have been translated from the Cephes Mathematical Library
written in C by Stephen L. Moshier [1].
The accuracy in the usual ranges, not too near to overflow or underflow,
is of 14 or more significant decimal digits.
The following is a listing of the available functions and some of their
characteristics ("inf" means infinity).*)

const
UERF: extended =-5.9302291691084904282;        {erf and erfc limit}
URFC: extended = 1.0672964589947618630E+2;     {erfc underflow point}

FUNCTION erfc(a: extended): extended;
var
pb,qb,xb,yb,zb: extended;
Qh: array [0..9] of extended =
(1.130609910594093747762E9,3.56592869656703138891E9,5.188672873106859049556E9,
 4.58801818891860972689E9,2.729005809811924550999E9,1.138778654945478547049E9,
 3.358653716579278063988E8,6.822450775590265689648E7,8.79923997735126107761E6,
 5.669830829076399819566E5);
Rh: array [0..4] of extended =
(3.621349282255624026891,7.173690522797138522298,3.445028155383625172464,
 5.537445669807799246891E-1,2.697535671015506686136E-2);
Sh: array [0..4] of extended =
(1.072884067182663823072E1,1.533713447609627196926E1,6.572990478128949439509,
 1.005392977603322982436,4.781257488046430019872E-2);
Tk: array [0..6] of extended =
(1.097496774521124996496E-1,5.402980370004774841217,2.871822526820825849235E2,
 2.677472796799053019985E3,4.825977363071025440855E4,1.549905740900882313773E5,
 1.104385395713178565288E6);
Uk: array [0..5] of extended =
(4.525777638142203713736E1,9.715333124857259246107E2,1.245905812306219011252E4,
 9.942956272177178491525E4,4.636021778692893773576E5,9.787360737578177599571E5);
Ph: array [0..9] of extended =
(1.130609921802431462353E9,2.290171954844785638925E9,2.295563412811856278515E9,
 1.448651275892911637208E9,6.234814405521647580919E8,1.87009507112043671593E8,
 3.833161455208142870198E7,4.964439504376477951135E6,3.198859502299390825278E5,
-9.085943037416544232472E-6);
begin
	if a<UERF then
		yb:=2.0
	else if a>URFC then
		yb:=0.0
	else begin
		xb:=abs(a);
		if xb<1.0 then begin
			xb:=a*a;
			zb:=a*((((((Tk[0]*xb+Tk[1])*xb+Tk[2])*xb+Tk[3])*xb+Tk[4])*xb+Tk[5])*xb+Tk[6])/
				((((((xb+Uk[0])*xb+Uk[1])*xb+Uk[2])*xb+Uk[3])*xb+Uk[4])*xb+Uk[5]);
			yb:=1.0-zb
		end else begin
			yb:=1.0/xb;
			if xb<8.0 then begin
				pb:=((((((((Ph[0]*yb+Ph[1])*yb+Ph[2])*yb+Ph[3])*yb+Ph[4])*yb+Ph[5])*yb
					+Ph[6])*yb+Ph[7])*yb+Ph[8])*yb+Ph[9];
				qb:=(((((((((yb+Qh[0])*yb+Qh[1])*yb+Qh[2])*yb+Qh[3])*yb+Qh[4])*yb+Qh[5])*yb
					+Qh[6])*yb+Qh[7])*yb+Qh[8])*yb+Qh[9]
			end else begin
				zb:=yb*yb;
				pb:=yb*((((Rh[0]*zb+Rh[1])*zb+Rh[2])*zb+Rh[3])*zb+Rh[4]);
				qb:=((((zb+Sh[0])*zb+Sh[1])*zb+Sh[2])*zb+Sh[3])*zb+Sh[4]
			end;
			yb:=exp(-a*a)*pb/qb;
			if a<0 then
				yb:=2.0-yb
		end
	end;
	erfc:=yb
end;

function qg(x: double): double;
begin
	result := 0.5*erfc(x/1.414213562373095);
end;
{$ENDIF}

function VoxelIntensity2q(fv: TFloatVec; val: single; isVerbose : boolean = false): single;
var
  zval : single;
begin
  zval := interp_floatvec(fv,val);
  if( zval > 0.0 ) then
    {$IFDEF AFNI_APPROX}
    result := 2.0*qg(zval)
    {$ELSE}
    result := 2.0*z2p(-zval)
    {$ENDIF}
  else
    result := 1.0;
  if not isVerbose then exit;
  {$IFDEF Windows}
  if not isConsole then exit;
  {$ENDIF}
  writeln(format('zval=%.8f',[zval ]));
end;

end.

