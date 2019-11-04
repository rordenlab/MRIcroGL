unit ustat;
//written by William ('Bill') G. Miller 2004, released under BSD 2-Clause License
interface
{$mode Delphi}
uses Math; {for Power/Ln}

function inversez(prob : double) : double;
function inverset(prob, DF : double) : double;
function inversef(prob : real; k1,k2 : integer) : real;

implementation

function zprob(p : double; VAR errorstate : boolean) : double;
VAR
   z, xp, lim, p0, p1, p2, p3, p4, q0, q1, q2, q3, q4, Y : double;
begin
     // value of probability between approx. 0 and .5 entered in p and the
     // z value is returned  z
     errorstate := true;
     lim := 1E-19;
     p0 := -0.322232431088;
     p1 := -1.0;
     p2 := -0.342242088547;
     p3 := -0.0204231210245;
     p4 := -4.53642210148E-05;
     q0 := 0.099348462606;
     q1 := 0.588581570495;
     q2 := 0.531103462366;
     q3 := 0.10353775285;
     q4 := 0.0038560700634;
     xp := 0.0;
     if (p > 0.5) then
     	p := 1 - p;
     if (p < lim) then
     	z := xp
     else
     begin
          errorstate := false;
          if (p = 0.5) then z := xp
          else
          begin
               Y := sqrt(ln(1.0 / (p * p)));
               xp := Y + ((((Y * p4 + p3) * Y + p2) * Y + p1) * Y + p0) /
                    ((((Y * q4 + q3) * Y + q2) * Y + q1) * Y + q0);
               if (p < 0.5) then xp := -xp;
               z := xp;
          end;
     end;
     zprob := z;
end;  // End function zprob

function inversez(prob : double) : double;
var
   z, p : double;
   flag : boolean;
begin
	// obtains the inverse of z, that is, the z for a probability associated
	// with a normally distributed z score.
	p := prob;
	if (prob > 0.5) then
		p := 1.0 - prob;
	z := zprob(p,flag);
	if (prob < 0.5) then 
		z := abs(z);
	inversez := z;
end;    //End of inversez Function

function inverset(prob, DF : double) : double;
var
   z, W, tValue: double;
begin
    // Returns the t value corresponding to a two-tailed t test probability.
    z := inversez(prob);
    W := z * ((8.0 * DF + 3.0) / (1.0 + 8.0 * DF));
    tValue := sqrt(DF * (exp(W * W / DF) - 1.0));
    inverset := tValue;
end;

function lngamma(w : real) : real;
(* Calculates the logarithm of the gamma function.  w must be such that *)
(*   2*w is an integer > 0.                                             *)
const a = 0.57236494; (* ln(sqrt(pi)) *)
var sum:real; (* a temporary store for summation of values *)
begin
     sum := 0;
     w := w-1;
     while w > 0.0 do
     begin
          sum := sum + ln(w);
          w := w - 1
     end; (* of summation loop *)
     if w < 0.0
        then lngamma := sum + a  (* note!!! is something is missing here? *)
        else lngamma := sum
end; (* of lngamma *)

function betaratio(x,a,b,lnbeta : real) : real;
(* calculates the incomplete beta function ratio with parameters a    *)
(* and b.  LnBeta is the logarithm of the complete beta function with *)
(* parameters a and b.                                                *)
const error = 1.0E-7;
var c : real; (* c = a + b *)
    factor1,factor2,factor3 : real; (* factors multiplying terms in series *)
    i,j : integer; (* counters *)
    sum : real; (* current sum of series *)
    temp : real; (* temporary store for exchanges *)
    term : real; (* term of series *)
    xlow : boolean; (* status of x which determines the end from which the *)
                    (* series is evaluated *)
    // ylow : real; (* adjusted argument *)
    y : real;
begin
     if (x=0) or (x=1)
     then
         sum := x
     else begin
          c := a + b;
          if a < c*x
          then begin
               xlow := true;
               y := x;
               x := 1 - x;
               temp := a;
               a := b;
               b := temp
          end else begin
               xlow := false;
               y := 1 - x;
          end;
          term := 1;
          j := 0;
          sum := 1;
          i := trunc(b + c * y) + 1;
          factor1 := x/y;
          repeat
                j := j + 1;
                i := i - 1;
                if i >= 0
                then begin
                     factor2 := b - j;
                     if i = 0 then factor2 := x;
                end;
                if abs(a+j) < 1.0e-6 then
                begin
                     betaratio := sum;
                     exit;
                end;
                term := term*factor2*factor1/(a+j);
                sum := sum + term;
          until (abs(term) <= sum) and (abs(term) <= error*sum);
          factor3 := exp(a*ln(x) + (b-1)*ln(y) - lnbeta);
          sum := sum*factor3/a;
          if xlow
             then sum := 1 - sum;
     end;
     betaratio := sum;
end; (* of betaratio *)

function inversebetaratio(ratio,a,b,lnbeta : real) : real;
(* Calculates the inverse of the incomplete beta function ratio with *)
(* parameters a and b.  LnBeta is the logarithm of the complete beta *)
(* function with parameters a and b.  Uses function betaratio.       *)
const error = 1.0E-7;
var
//   c: real; (* c = a + b *)
    largeratio : boolean;
    temp1,temp2,temp3,temp4 : real; (* temporary variables *)
    x,x1 : real; (* successive estimates of inverse ratio *)
    y : real; (* adjustment during newton iteration *)
begin
     if (ratio = 0) or (ratio = 1)
     then
         x := ratio
     else begin
          largeratio := false;
          if ratio > 0.5
          then begin
               largeratio := true;
               ratio := 1 - ratio;
               temp1 := a;
               b := a;
               a := temp1
          end;
          (* calcuates initial estimate for x *)
          temp1 := sqrt(-ln(ratio*ratio));
          temp2 := 1.0 + temp1*(0.99229 + 0.04481*temp1);
          temp2 := temp1 - (2.30753 + 0.27061*temp1)/temp2;
          if (a > 1) and (b > 1)
          then begin
               temp1 := (temp2*temp2 - 3.0)/6.0;
               temp3 := 1.0/(a + a -1.0);
               temp4 := 1.0/ (b + b - 1.0);
               x1 := 2.0 /(temp3 + temp4);
               x := temp1 + 5.0/6.0 - 2.0/(3.0*x1);
               x := temp2*sqrt(x1 + temp1)/x1 - x*(temp4 - temp3);
               x := a/(a + b*exp(x + x))
          end
          else begin
               temp1 := b + b;
               temp3 := 1.0/(9.0*b);
               temp3 := 1.0 - temp3 + temp2*sqrt(temp3);
               temp3 := temp1*temp3*temp3*temp3;
               if temp3 > 0
               then begin
                    temp3 := (4.0*a + temp1 - 2.0)/temp3;
                    if temp3 > 1 then x := 1.0-2.0/(1 + temp3)
                    else x := exp((ln(ratio*a) + lnbeta)/a)
               end
               else x := 1.0 - exp((ln((1-ratio)*b) + lnbeta)/b);
          end;
          (* Newton iteration *)
          repeat
                y := betaratio(x,a,b,lnbeta);
                y := (y-ratio)*exp((1-a)*ln(x)+(1-b)*ln(1-x)+lnbeta);
                temp4 := y;
                x1 := x - y;
                while (x1 <= 0) or (x1 >= 1) do
                begin
                     temp4 := temp4/2;
                     x1 := x - temp4
                end;
                x := x1;
          until abs(y) < error;
          if largeratio then x := 1 - x;
     end;
     inversebetaratio := x
end; (* of inversebetaratio *)

function inversef(prob : real; k1,k2 : integer) : real;
(* Calculates the inverse F distribution function based on k1 and k2 *)
(* degrees of freedom.  Uses function lngamma, betaratio and the     *)
(* inversebetaratio routines.                                        *)
var h1,h2 : real; (* half degrees of freedom k1, k2 *)
    lnbeta : real; (* log of complete beta function with params h1 and h2 *)
    ratio : real; (* beta ratio *)
    x : real; (* inverse beta ratio *)

begin
     h1 := 0.5 * k2;
     h2 := 0.5 * k1;
     if prob > 0.5 then
     	ratio := 1 - prob
     else
     	ratio := prob;
     lnbeta := lngamma(h1) + lngamma(h2) - lngamma(h1 + h2);
     x := inversebetaratio(ratio,h1,h2,lnbeta);
     result := k2 * (1 - x) / (k1 * x)
end; (* of fpercentpoint *)

end.
