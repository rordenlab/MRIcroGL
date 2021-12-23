{$mode objfpc}
{$modeswitch advancedrecords}

unit VectorMath;
interface
uses
	Math, SysUtils;

// NOTE: constref is bugged on 3.0.4 and below
{$macro on}
{$if FPC_FULLVERSION < 30005}
  {$define constref:=}
{$endif}

type
	TScalar = single;

type
	TVec2 = record
		public
			function Length: TScalar; inline;
			function SquaredLength: TScalar; inline;
			function Normalize: TVec2; inline;
			function Dot (constref vec: TVec2): TScalar; inline;
			function Cross (constref vec:TVec2): TVec2; inline;
			function Negate: TVec2; inline;
			procedure Show;
			function Str: string;
		
		private
			function GetComponent(const pIndex:integer):TScalar; inline;
      procedure SetComponent(const pIndex:integer;const pValue:TScalar); inline;
		public
			property Components[const pIndex:integer]:TScalar read GetComponent write SetComponent; default;	
		public
			class operator := (a:TScalar): TVec2;
			class operator + (constref p1, p2: TVec2): TVec2; overload;
			class operator - (constref p1, p2: TVec2): TVec2; overload; 
			class operator * (constref p1, p2: TVec2): TVec2; overload; 
			class operator / (constref p1, p2: TVec2): TVec2; overload;
			class operator = (constref p1, p2: TVec2): boolean; 
			class operator + (constref p1: TVec2; p2: TScalar): TVec2; overload; 
			class operator - (constref p1: TVec2; p2: TScalar): TVec2; overload; 
			class operator * (constref p1: TVec2; p2: TScalar): TVec2; overload; 
			class operator / (constref p1: TVec2; p2: TScalar): TVec2; overload;
		public
			case integer of
				0: (v: array[0..1] of TScalar);
				1: (x, y: TScalar);
	end;

type
	TVec2s = array of TVec2;

type
	TVec3 = record
		public
			class function Up: TVec3; static; inline;
			
			function Length: TScalar; inline;
			function SquaredLength: TScalar; inline;
			function Normalize: TVec3; inline;
			function Dot (constref vec: TVec3): TScalar; inline;
			function Cross (constref vec:TVec3): TVec3; inline;
			function Negate: TVec3; inline;

			function XY: TVec2; inline;

			procedure Show;
			function Str: string;

		private
			function GetComponent(const pIndex:integer):TScalar; inline;
      procedure SetComponent(const pIndex:integer;const pValue:TScalar); inline;
		public
			property Components[const pIndex:integer]:TScalar read GetComponent write SetComponent; default;
		public
			class operator := (a:TScalar): TVec3;
			class operator + (constref p1, p2: TVec3): TVec3; overload;
			class operator - (constref p1, p2: TVec3): TVec3; overload; 
			class operator * (constref p1, p2: TVec3): TVec3; overload; 
			class operator / (constref p1, p2: TVec3): TVec3; overload;
			class operator = (constref p1, p2: TVec3): boolean; 
			class operator + (constref p1: TVec3; p2: TScalar): TVec3; overload; 
			class operator - (constref p1: TVec3; p2: TScalar): TVec3; overload; 
			class operator * (constref p1: TVec3; p2: TScalar): TVec3; overload; 
			class operator / (constref p1: TVec3; p2: TScalar): TVec3; overload;
		public
			case integer of
				0: (v: array[0..2] of TScalar);
				1: (x, y, z: TScalar);
				2: (r, g, b: TScalar);
				3: (Pitch,Yaw,Roll:TScalar);
	end;

type
	TVec4 = record
		public
			function Length: TScalar; inline;
			function SquaredLength: TScalar; inline;
			function Normalize: TVec4; inline;
			function Dot (constref vec: TVec4): TScalar; inline;
			function Cross(constref vec: TVec4): TVec4; inline;
			function Negate: TVec4; inline;

			function XY: TVec2; inline;
			function XYZ: TVec3; inline;

			procedure Show;
			function Str: string;
		private
			function GetComponent(const pIndex:integer):TScalar; inline;
      procedure SetComponent(const pIndex:integer;const pValue:TScalar); inline;
		public
			property Components[const pIndex:integer]:TScalar read GetComponent write SetComponent; default;
		public
			class operator := (a:TScalar): TVec4;
			class operator + (constref p1, p2: TVec4): TVec4; overload;
			class operator - (constref p1, p2: TVec4): TVec4; overload; 
			class operator * (constref p1, p2: TVec4): TVec4; overload; 
			class operator / (constref p1, p2: TVec4): TVec4; overload;
			class operator = (constref p1, p2: TVec4): boolean; 
			class operator + (constref p1: TVec4; p2: TScalar): TVec4; overload; 
			class operator - (constref p1: TVec4; p2: TScalar): TVec4; overload; 
			class operator * (constref p1: TVec4; p2: TScalar): TVec4; overload; 
			class operator / (constref p1: TVec4; p2: TScalar): TVec4; overload;
		public
			case integer of
				0: (v: array[0..3] of TScalar);
				1: (x, y, z, w: TScalar);
				2: (r, g, b, a: TScalar);
	end;

type
	TMat4 = record
		public
			function Ptr: pointer;
			class function Identity: TMat4; static; inline;
						
			constructor Translate(const tx,ty,tz:TScalar); overload;
      constructor Translate(const pTranslate:TVec3); overload;
      constructor Translate(const tx,ty,tz,tw:TScalar); overload;
      constructor Diag(const tx,ty,tz, ti:TScalar); overload; //Diag(1 2 3 4) is the same as matlab diag([1 2 3 4])
      constructor Diag(const tx,ty,tz:TScalar);  overload;//4th component is 1: Diag(1 2 3) is the same as matlab diag([1 2 3 1])
      
			constructor Scale (x, y, z: TScalar);
			constructor RotateX(const Angle:TScalar);
      constructor RotateY(const Angle:TScalar);
      constructor RotateZ(const Angle:TScalar);
      constructor Rotate(const Angle:TScalar;const Axis:TVec3); overload;
      constructor Rotate(constref pMatrix:TMat4); overload;
			constructor Ortho(const Left,Right,Bottom,Top,zNear,zFar:TScalar);
			constructor OrthoGL(const Left,Right,Bottom,Top,zNear,zFar:TScalar);
			constructor Perspective (const fovy,Aspect,zNear,zFar:TScalar);
			constructor PerspectiveGL(const fovy,Aspect,zNear,zFar:TScalar);
			constructor LookAt (constref Eye,Center,Up:TVec3);
			
			function Inverse:TMat4; inline;
      function Transpose:TMat4; inline;
     
 			procedure Show;
		private
			function GetComponent(const column, row:integer):TScalar; inline;
      procedure SetComponent(const column, row:integer;const pValue:TScalar); inline;
		public
			property Components[const column, row:integer]:TScalar read GetComponent write SetComponent; default;
		public
			class operator := (const a:TScalar):TMat4;
      class operator = (constref a,b:TMat4):boolean;
      class operator <> (constref a,b:TMat4):boolean;
      class operator + (constref a,b:TMat4):TMat4;
      class operator + (constref a:TMat4;const b:TScalar):TMat4;
      class operator + (const a:TScalar;constref b:TMat4):TMat4;
      class operator - (constref a,b:TMat4):TMat4;
      class operator - (constref a:TMat4;const b:TScalar):TMat4;
      class operator - (const a:TScalar;constref b:TMat4): TMat4;
      class operator * (constref b,a:TMat4):TMat4;
      class operator * (constref a:TMat4;const b:TScalar):TMat4;
      class operator * (const a:TScalar;constref b:TMat4):TMat4;
      class operator * (constref a:TMat4;constref b:TVec3):TVec3;
      class operator * (constref a:TVec3;constref b:TMat4):TVec3;
      class operator * (constref a:TMat4;constref b:TVec4):TVec4;
      class operator * (constref a:TVec4;constref b:TMat4):TVec4;
      class operator / (constref a,b:TMat4):TMat4;
      class operator / (constref a:TMat4;const b:TScalar):TMat4;
      class operator / (const a:TScalar;constref b:TMat4):TMat4;
		public
			case integer of
				0:(m:array[0..3,0..3] of TScalar);
				1:(column: array[0..3] of TVec4);
				2:(Right,Up,Forwards,Offset:TVec4);
				3:(Tangent,Bitangent,Normal,Translation:TVec4);
	end;

function M4: TMat4;

function Vec2 (x, y: TScalar): TVec2;
function Vec3 (x, y, z: TScalar): TVec3; overload;
function Vec3 (constref vec: TVec2; z: TScalar): TVec3; overload;
function Vec4 (x, y, z, w: TScalar): TVec4; overload;
function Vec4 (constref vec: TVec3; w: TScalar): TVec4; overload;
function Vec4 (constref vec: TVec2; z, w: TScalar): TVec4; overload;

function V2 (x, y: TScalar): TVec2;
function V3 (x, y, z: TScalar): TVec3; overload;
function V3 (constref vec: TVec2; z: TScalar): TVec3; overload;
function V4 (x, y, z, w: TScalar): TVec4;
function V4 (constref vec: TVec3; w: TScalar): TVec4; overload;
function V4 (constref vec: TVec2; z, w: TScalar): TVec4; overload;

implementation

var
	Matrix4x4Identity:TMat4=(m:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0)));
	
const
	DEG2RAD=pi/180.0;
  //RAD2DEG=180.0/pi;
  //HalfPI=pi*0.5;	

{=============================================}
{@! ___PROCEDURAL___ } 
{=============================================}
function Vec2 (x, y: TScalar): TVec2; inline;
begin
	result.x := x;
	result.y := y;
end;

function V2 (x, y: TScalar): TVec2; inline;
begin
	result := Vec2(x, y);
end;

function Vec3 (x, y, z: TScalar): TVec3; inline;
begin
	result.x := x;
	result.y := y;
	result.z := z;
end;

function Vec3 (constref vec: TVec2; z: TScalar): TVec3; inline;
begin
	result.x := vec.x;
	result.y := vec.y;
	result.z := z;
end;

function V3 (x, y, z: TScalar): TVec3; inline;
begin
	result := Vec3(x, y, z);
end;

function V3 (constref vec: TVec2; z: TScalar): TVec3; inline;
begin
	result := Vec3(vec, z);
end;

function Vec4 (x, y, z, w: TScalar): TVec4; inline;
begin
	result.x := x;
	result.y := y;
	result.z := z;
	result.w := w;
end;

function Vec4 (constref vec: TVec3; w: TScalar): TVec4; inline;
begin
	result.x := vec.x;
	result.y := vec.y;
	result.z := vec.z;
	result.w := w;
end;

function Vec4 (constref vec: TVec2; z, w: TScalar): TVec4; inline;
begin
	result.x := vec.x;
	result.y := vec.y;
	result.z := z;
	result.w := w;
end;

function V4 (constref vec: TVec3; w: TScalar): TVec4; inline;
begin
	result := Vec4(vec, w);
end;

function V4 (constref vec: TVec2; z, w: TScalar): TVec4; inline;
begin
	result := Vec4(vec, z, w);
end;

function V4 (x, y, z, w: TScalar): TVec4; inline;
begin
	result := Vec4(x, y, z, w);
end;

{=============================================}
{@! ___VEC2___ } 
{=============================================}
function TVec2.GetComponent(const pIndex:integer):TScalar;
begin
 result:=v[pIndex];
end;

procedure TVec2.SetComponent(const pIndex:integer;const pValue:TScalar);
begin
 v[pIndex]:=pValue;
end;

function TVec2.Length: TScalar;
begin
	result := Sqrt(SquaredLength);
end;

function TVec2.SquaredLength: TScalar;
begin
	result := Power(x, 2) + Power(y, 2);
end;

function TVec2.Normalize: TVec2;
var
	fac: TScalar;
begin
	//result := self / Magnitude;
	fac:=Sqrt(Sqr(x)+Sqr(y));
	if fac<>0.0 then begin
		fac:=1.0/fac;
		result.x:=x*fac;
		result.y:=y*fac;
	end else begin
		result.x:=0.0;
		result.y:=0.0;
	end;
end;

function TVec2.Negate: TVec2;
begin
	result := Vec2(-x, -y);
end;

function TVec2.Dot (constref vec: TVec2): TScalar;
begin
	result := (x * vec.x) + (y * vec.y);
end;

function TVec2.Cross(constref vec:TVec2):TVec2;
begin
 result.x:=(y*vec.x)-(x*vec.y);
 result.y:=(x*vec.y)-(y*vec.x);
end;

procedure TVec2.Show;
begin
	writeln(Str);
end;

function TVec2.Str: string;
begin
	result := FloatToStr(x)+','+FloatToStr(y);
end;

class operator TVec2.:= (a:TScalar): TVec2;
begin
	result.x := a;
	result.y := a;
end;

class operator TVec2.+ (constref p1, p2: TVec2): TVec2;
begin
	result := Vec2(p1.x+p2.x, p1.y+p2.y);
end;

class operator TVec2.- (constref p1, p2: TVec2): TVec2;
begin
	result := Vec2(p1.x-p2.x, p1.y-p2.y);
end;

class operator TVec2.* (constref p1, p2: TVec2): TVec2; 
begin
	result := Vec2(p1.x*p2.x, p1.y*p2.y);
end;

class operator TVec2./ (constref p1, p2: TVec2): TVec2; 
begin
	result := Vec2(p1.x/p2.x, p1.y/p2.y);
end;

class operator TVec2.= (constref p1, p2: TVec2): boolean; 
begin
	result := (p1.x = p2.x) and (p1.y = p2.y);
end;

class operator TVec2.+ (constref p1: TVec2; p2: TScalar): TVec2;
begin
	result := Vec2(p1.x+p2, p1.y+p2);
end;

class operator TVec2.- (constref p1: TVec2; p2: TScalar): TVec2;
begin
	result := Vec2(p1.x-p2, p1.y-p2);
end;

class operator TVec2.* (constref p1: TVec2; p2: TScalar): TVec2;
begin
	result := Vec2(p1.x*p2, p1.y*p2);
end;

class operator TVec2./ (constref p1: TVec2; p2: TScalar): TVec2;
begin
	result := Vec2(p1.x/p2, p1.y/p2);
end;

{=============================================}
{@! ___VEC3___ } 
{=============================================}
function TVec3.GetComponent(const pIndex:integer):TScalar;
begin
 result:=v[pIndex];
end;

procedure TVec3.SetComponent(const pIndex:integer;const pValue:TScalar);
begin
 v[pIndex]:=pValue;
end;

class function TVec3.Up: TVec3;
begin
	result := Vec3(0,1,0);
end;

procedure TVec3.Show;
begin
	writeln(Str);
end;

function TVec3.XY: TVec2;
begin
	result := V2(x, y);
end;

function TVec3.Str: string;
begin
	result := FloatToStr(x)+','+FloatToStr(y)+','+FloatToStr(z);
end;

function TVec3.Length: TScalar;
begin
	result := Sqrt(SquaredLength);
end;

function TVec3.SquaredLength: TScalar;
begin
	result := Power(x, 2) + Power(y, 2) + Power(z, 2);
end;

function TVec3.Normalize: TVec3;
var
	fac: TScalar;
begin
	//result := self / Magnitude;
	fac:=Sqrt(Sqr(x)+Sqr(y)+Sqr(z));
	if fac<>0.0 then begin
		fac:=1.0/fac;
		result.x:=x*fac;
		result.y:=y*fac;
		result.z:=z*fac;
	end else begin
		result.x:=0.0;
		result.y:=0.0;
		result.z:=0.0;
	end;
end;

function TVec3.Negate: TVec3;
begin
	result := Vec3(-x, -y, -z);
end;

function TVec3.Dot (constref vec: TVec3): TScalar;
begin
	result := (x * vec.x) + (y * vec.y) + (z * vec.z);
end;

function TVec3.Cross(constref vec: TVec3): TVec3;
begin
	result := Vec3(y * vec.z - z * vec.y, z * vec.x - x * vec.z, x * vec.y - y * vec.x);
end;

class operator TVec3.:= (a:TScalar): TVec3;
begin
	result.x := a;
	result.y := a;
	result.z := a;
end;

class operator TVec3.+ (constref p1, p2: TVec3): TVec3;
begin
	result := Vec3(p1.x+p2.x, p1.y+p2.y, p1.z+p2.z);
end;

class operator TVec3.- (constref p1, p2: TVec3): TVec3;
begin
	result := Vec3(p1.x-p2.x, p1.y-p2.y, p1.z-p2.z);
end;

class operator TVec3.* (constref p1, p2: TVec3): TVec3; 
begin
	result := Vec3(p1.x*p2.x, p1.y*p2.y, p1.z*p2.z);
end;

class operator TVec3./ (constref p1, p2: TVec3): TVec3; 
begin
	result := Vec3(p1.x/p2.x, p1.y/p2.y, p1.z/p2.z);
end;

class operator TVec3.= (constref p1, p2: TVec3): boolean; 
begin
	result := (p1.x = p2.x) and (p1.y = p2.y) and (p1.z = p2.z);
end;

class operator TVec3.+ (constref p1: TVec3; p2: TScalar): TVec3;
begin
	result := Vec3(p1.x+p2, p1.y+p2, p1.z+p2);
end;

class operator TVec3.- (constref p1: TVec3; p2: TScalar): TVec3;
begin
	result := Vec3(p1.x-p2, p1.y-p2, p1.z-p2);
end;

class operator TVec3.* (constref p1: TVec3; p2: TScalar): TVec3;
begin
	result := Vec3(p1.x*p2, p1.y*p2, p1.z*p2);
end;

class operator TVec3./ (constref p1: TVec3; p2: TScalar): TVec3;
begin
	result := Vec3(p1.x/p2, p1.y/p2, p1.z/p2);
end;

{=============================================}
{@! ___VEC4___ } 
{=============================================}

function TVec4.Length: TScalar;
begin
	result := Sqrt(SquaredLength);
end;

function TVec4.SquaredLength: TScalar;
begin
	result := Power(x, 2) + Power(y, 2) + Power(z, 2) + Power(w, 2);
end;

function TVec4.Normalize: TVec4;
var
	fac: TScalar;
begin
	//result := self / Magnitude;
	fac:=Sqrt(Sqr(x)+Sqr(y)+Sqr(z)+Sqr(w));
	if fac<>0.0 then begin
		fac:=1.0/fac;
		result.x:=x*fac;
		result.y:=y*fac;
		result.z:=z*fac;
		result.w:=w*fac;
	end else begin
		result.x:=0.0;
		result.y:=0.0;
		result.z:=0.0;
		result.w:=0.0;
	end;
end;

function TVec4.Negate: TVec4;
begin
	result := Vec4(-x, -y, -z, -w);
end;

function TVec4.Dot (constref vec: TVec4): TScalar;
begin
	result := (x * vec.x) + (y * vec.y) + (z * vec.z) + (w * vec.w);
end;

function TVec4.Cross(constref vec: TVec4): TVec4;
begin
 result.x:=(y*vec.z)-(z*vec.y);
 result.y:=(z*vec.x)-(x*vec.z);
 result.z:=(x*vec.y)-(y*vec.x);
 result.w:=1.0;
end;

function TVec4.GetComponent(const pIndex:integer):TScalar;
begin
 result:=v[pIndex];
end;

procedure TVec4.SetComponent(const pIndex:integer;const pValue:TScalar);
begin
 v[pIndex]:=pValue;
end;

class operator TVec4.:= (a:TScalar): TVec4;
begin
	result.x := a;
	result.y := a;
	result.z := a;
	result.w := a;
end;

class operator TVec4.+ (constref p1, p2: TVec4): TVec4;
begin
	result := Vec4(p1.x+p2.x, p1.y+p2.y, p1.z+p2.z, p1.w+p2.w);
end;

class operator TVec4.- (constref p1, p2: TVec4): TVec4;
begin
	result := Vec4(p1.x-p2.x, p1.y-p2.y, p1.z-p2.z, p1.w-p2.w);
end;

class operator TVec4.* (constref p1, p2: TVec4): TVec4; 
begin
	result := Vec4(p1.x*p2.x, p1.y*p2.y, p1.z*p2.z, p1.w*p2.w);
end;

class operator TVec4./ (constref p1, p2: TVec4): TVec4; 
begin
	result := Vec4(p1.x/p2.x, p1.y/p2.y, p1.z/p2.z, p1.w/p2.w);
end;

class operator TVec4.= (constref p1, p2: TVec4): boolean; 
begin
	result := (p1.x = p2.x) and (p1.y = p2.y) and (p1.z = p2.z) and (p1.w = p2.w);
end;

class operator TVec4.+ (constref p1: TVec4; p2: TScalar): TVec4;
begin
	result := Vec4(p1.x+p2, p1.y+p2, p1.z+p2, p1.w+p2);
end;

class operator TVec4.- (constref p1: TVec4; p2: TScalar): TVec4;
begin
	result := Vec4(p1.x-p2, p1.y-p2, p1.z-p2, p1.w-p2);
end;

class operator TVec4.* (constref p1: TVec4; p2: TScalar): TVec4;
begin
	result := Vec4(p1.x*p2, p1.y*p2, p1.z*p2, p1.w*p2);
end;

class operator TVec4./ (constref p1: TVec4; p2: TScalar): TVec4;
begin
	result := Vec4(p1.x/p2, p1.y/p2, p1.z/p2, p1.w/p2);
end; 

procedure TVec4.Show;
begin
	writeln(Str);
end;

function TVec4.XY: TVec2;
begin
	result := V2(x, y);
end;

function TVec4.XYZ: TVec3;
begin
	result := V3(x, y, z);
end;

function TVec4.Str: string;
begin
	result := FloatToStr(x)+','+FloatToStr(y)+','+FloatToStr(z)+','+FloatToStr(w);
end;

{=============================================}
{@! ___MAT4___ } 
{=============================================}
function M4: TMat4;
begin
	result := TMat4.Identity;
end;

function TMat4.GetComponent(const column, row:integer):TScalar;
begin
 result:=m[column,row];
end;

procedure TMat4.SetComponent(const column, row:integer;const pValue:TScalar);
begin
 m[column,row]:=pValue;
end;

function SameValue (a, b: TScalar): boolean; inline;
begin
	result := a = b;
end;

function TMat4.Ptr: pointer;
begin
	result := @column[0];
end;

class function TMat4.Identity: TMat4;
begin
	result := Matrix4x4Identity;
end;

constructor TMat4.Diag(const tx,ty,tz, ti:TScalar); overload;
begin
 m[0,0]:=tx;
 m[0,1]:=0.0;
 m[0,2]:=0.0;
 m[0,3]:=0.0;
 m[1,0]:=0.0;
 m[1,1]:=ty;
 m[1,2]:=0.0;
 m[1,3]:=0.0;
 m[2,0]:=0.0;
 m[2,1]:=0.0;
 m[2,2]:=tz;
 m[2,3]:=0.0;
 m[3,0]:=0.0;
 m[3,1]:=0.0;
 m[3,2]:=0.0;
 m[3,3]:=ti;
end;

constructor TMat4.Diag(const tx,ty,tz:TScalar); overload;
begin
     Diag(tx, ty, tz, 1);

end;

constructor TMat4.Translate(const tx,ty,tz:TScalar);
begin
 m[0,0]:=1.0;
 m[0,1]:=0.0;
 m[0,2]:=0.0;
 m[0,3]:=0.0;
 m[1,0]:=0.0;
 m[1,1]:=1.0;
 m[1,2]:=0.0;
 m[1,3]:=0.0;
 m[2,0]:=0.0;
 m[2,1]:=0.0;
 m[2,2]:=1.0;
 m[2,3]:=0.0;
 m[3,0]:=tx;
 m[3,1]:=ty;
 m[3,2]:=tz;
 m[3,3]:=1.0;
end;

constructor TMat4.Translate(const pTranslate:TVec3);
begin
 m[0,0]:=1.0;
 m[0,1]:=0.0;
 m[0,2]:=0.0;
 m[0,3]:=0.0;
 m[1,0]:=0.0;
 m[1,1]:=1.0;
 m[1,2]:=0.0;
 m[1,3]:=0.0;
 m[2,0]:=0.0;
 m[2,1]:=0.0;
 m[2,2]:=1.0;
 m[2,3]:=0.0;
 m[3,0]:=pTranslate.x;
 m[3,1]:=pTranslate.y;
 m[3,2]:=pTranslate.z;
 m[3,3]:=1.0;
end;

constructor TMat4.Translate(const tx,ty,tz,tw:TScalar);
begin
 m[0,0]:=1.0;
 m[0,1]:=0.0;
 m[0,2]:=0.0;
 m[0,3]:=0.0;
 m[1,0]:=0.0;
 m[1,1]:=1.0;
 m[1,2]:=0.0;
 m[1,3]:=0.0;
 m[2,0]:=0.0;
 m[2,1]:=0.0;
 m[2,2]:=1.0;
 m[2,3]:=0.0;
 m[3,0]:=tx;
 m[3,1]:=ty;
 m[3,2]:=tz;
 m[3,3]:=tw;
end;

constructor TMat4.Scale (x, y, z: TScalar);
begin	
	column[0].x := x;
	column[0].y := 0;
	column[0].z := 0;
	column[0].w := 0;
          
	column[1].x := 0;
	column[1].y := y;
	column[1].z := 0;
	column[1].w := 0;
 
	column[2].x := 0;
	column[2].y := 0;
	column[2].z := z;
	column[2].w := 0;
	        
	column[3].x := 0;
	column[3].y := 0;
	column[3].z := 0;
	column[3].w := 1;
end;

constructor TMat4.RotateX(const Angle:TScalar);
begin
 m[0,0]:=1.0;
 m[0,1]:=0.0;
 m[0,2]:=0.0;
 m[0,3]:=0.0;
 m[1,0]:=0.0;
 SinCos(Angle,m[1,2],m[1,1]);
 m[1,3]:=0.0;
 m[2,0]:=0.0;
 m[2,1]:=-m[1,2];
 m[2,2]:=m[1,1];
 m[2,3]:=0.0;
 m[3,0]:=0.0;
 m[3,1]:=0.0;
 m[3,2]:=0.0;
 m[3,3]:=1.0;
end;

constructor TMat4.RotateY(const Angle:TScalar);
begin
 SinCos(Angle,m[2,0],m[0,0]);
 m[0,1]:=0.0;
 m[0,2]:=-m[2,0];
 m[0,3]:=0.0;
 m[1,0]:=0.0;
 m[1,1]:=1.0;
 m[1,2]:=0.0;
 m[1,3]:=0.0;
 m[2,1]:=0.0;
 m[2,2]:=m[0,0];
 m[2,3]:=0.0;
 m[3,0]:=0.0;
 m[3,1]:=0.0;
 m[3,2]:=0.0;
 m[3,3]:=1.0;
end;

constructor TMat4.RotateZ(const Angle:TScalar);
begin
 SinCos(Angle,m[0,1],m[0,0]);
 m[0,2]:=0.0;
 m[0,3]:=0.0;
 m[1,0]:=-m[0,1];
 m[1,1]:=m[0,0];
 m[1,2]:=0.0;
 m[1,3]:=0.0;
 m[2,0]:=0.0;
 m[2,1]:=0.0;
 m[2,2]:=1.0;
 m[2,3]:=0.0;
 m[3,0]:=0.0;
 m[3,1]:=0.0;
 m[3,2]:=0.0;
 m[3,3]:=1.0;
end;

constructor TMat4.Rotate(const Angle:TScalar;const Axis:TVec3);
var SinusAngle,CosinusAngle:TScalar;
begin
 SinCos(Angle,SinusAngle,CosinusAngle);
 m[0,0]:=CosinusAngle+((1.0-CosinusAngle)*sqr(Axis.x));
 m[1,0]:=((1.0-CosinusAngle)*Axis.x*Axis.y)-(Axis.z*SinusAngle);
 m[2,0]:=((1.0-CosinusAngle)*Axis.x*Axis.z)+(Axis.y*SinusAngle);
 m[0,3]:=0.0;
 m[0,1]:=((1.0-CosinusAngle)*Axis.x*Axis.z)+(Axis.z*SinusAngle);
 m[1,1]:=CosinusAngle+((1.0-CosinusAngle)*sqr(Axis.y));
 m[2,1]:=((1.0-CosinusAngle)*Axis.y*Axis.z)-(Axis.x*SinusAngle);
 m[1,3]:=0.0;
 m[0,2]:=((1.0-CosinusAngle)*Axis.x*Axis.z)-(Axis.y*SinusAngle);
 m[1,2]:=((1.0-CosinusAngle)*Axis.y*Axis.z)+(Axis.x*SinusAngle);
 m[2,2]:=CosinusAngle+((1.0-CosinusAngle)*sqr(Axis.z));
 m[2,3]:=0.0;
 m[3,0]:=0.0;
 m[3,1]:=0.0;
 m[3,2]:=0.0;
 m[3,3]:=1.0;
end;

constructor TMat4.Rotate(constref pMatrix:TMat4);
begin
 m[0,0]:=pMatrix.m[0,0];
 m[0,1]:=pMatrix.m[0,1];
 m[0,2]:=pMatrix.m[0,2];
 m[0,3]:=0.0;
 m[1,0]:=pMatrix.m[1,0];
 m[1,1]:=pMatrix.m[1,1];
 m[1,2]:=pMatrix.m[1,2];
 m[1,3]:=0.0;
 m[2,0]:=pMatrix.m[2,0];
 m[2,1]:=pMatrix.m[2,1];
 m[2,2]:=pMatrix.m[2,2];
 m[2,3]:=0.0;
 m[3,0]:=0.0;
 m[3,1]:=0.0;
 m[3,2]:=0.0;
 m[3,3]:=1.0;
end;

constructor TMat4.OrthoGL(const Left,Right,Bottom,Top,zNear,zFar:TScalar);
var rml,tmb,fmn:TScalar;
begin
 rml:=Right-Left;
 tmb:=Top-Bottom;
 fmn:=zFar-zNear;
 m[0,0]:=2.0/rml;
 m[0,1]:=0.0;
 m[0,2]:=0.0;
 m[0,3]:=0.0;
 m[1,0]:=0.0;
 m[1,1]:=2.0/tmb;
 m[1,2]:=0.0;
 m[1,3]:=0.0;
 m[2,0]:=0.0;
 m[2,1]:=0.0;
 m[2,2]:=(-2.0)/fmn;
 m[2,3]:=0.0;
 m[3,0]:=(-(Right+Left))/rml;
 m[3,1]:=(-(Top+Bottom))/tmb;
 m[3,2]:=(-(zFar+zNear))/fmn;
 m[3,3]:=1.0;
end;

constructor TMat4.Ortho(const Left,Right,Bottom,Top,zNear,zFar:TScalar);
var rml,tmb,fmn: TScalar;
begin
	rml:=Right-Left;
	tmb:=Top-Bottom;
	fmn:=zFar-zNear;
	m[0,0]:=2.0/rml;
	m[0,1]:=0.0;
	m[0,2]:=0.0;
	m[0,3]:=0.0;
	m[1,0]:=0.0;
	m[1,1]:=2.0/tmb;
	m[1,2]:=0.0;
	m[1,3]:=0.0;
	m[2,0]:=0.0;
	m[2,1]:=0.0;
	m[2,2]:=(-1.0)/fmn;
	m[2,3]:=0.0;
	m[3,0]:=(-(Right+Left))/rml;
	m[3,1]:=(-(Top+Bottom))/tmb;
	m[3,2]:=(-(zNear))/fmn;
	m[3,3]:=1.0;  
end;

constructor TMat4.Perspective(const fovy,Aspect,zNear,zFar:TScalar);
//var Sine,Cotangent,ZDelta,Radians:TScalar;
//begin
// Radians:=(fovy*0.5)*DEG2RAD;
// ZDelta:=zFar-zNear;
// Sine:=sin(Radians);
// if not ((ZDelta=0) or (Sine=0) or (aspect=0)) then begin
//  Cotangent:=cos(Radians)/Sine;
//  m:=Matrix4x4Identity.m;
//  m[0,0]:=Cotangent/aspect;
//  m[1,1]:=Cotangent;
//  m[2,2]:=(-(zFar+zNear))/ZDelta;
//  m[2,3]:=-1-0;
//  m[3,2]:=(-(2.0*zNear*zFar))/ZDelta;
//  m[3,3]:=0.0;
// end;
//end;
var Sine,Cotangent,ZDelta,Radians:TScalar;
begin
 Radians:=(fovy*0.5)*DEG2RAD;
 ZDelta:=zFar-zNear;
 Sine:=sin(Radians);
 if not ((ZDelta=0) or (Sine=0) or (aspect=0)) then begin
  Cotangent:=cos(Radians)/Sine;
  m:= Matrix4x4Identity.m;
  m[0,0]:=Cotangent/aspect;
  m[1,1]:=Cotangent;
  m[2,2]:=(-zFar)/ZDelta;
  m[2,3]:=-1;
  m[3,2]:=(-(zNear*zFar))/ZDelta;
  m[3,3]:=0.0;
 end;
end; 

constructor TMat4.PerspectiveGL(const fovy,Aspect,zNear,zFar:TScalar);
var Sine,Cotangent,ZDelta,Radians:TScalar;
begin
 Radians:=(fovy*0.5)*DEG2RAD;
 ZDelta:=zFar-zNear;
 Sine:=sin(Radians);
 if not ((ZDelta=0) or (Sine=0) or (aspect=0)) then begin
  Cotangent:=cos(Radians)/Sine;
  m:=Matrix4x4Identity.m;
  m[0,0]:=Cotangent/aspect;
  m[1,1]:=Cotangent;
  m[2,2]:=(-(zFar+zNear))/ZDelta;
  m[2,3]:=-1-0;
  m[3,2]:=(-(2.0*zNear*zFar))/ZDelta;
  m[3,3]:=0.0;
 end;
end;

constructor TMat4.LookAt(constref Eye,Center,Up:TVec3);
var RightVector,UpVector,ForwardVector:TVec3;
begin
 ForwardVector:=(Eye-Center).Normalize;
 RightVector:=(Up.Cross(ForwardVector)).Normalize;
 UpVector:=(ForwardVector.Cross(RightVector)).Normalize;
 m[0,0]:=RightVector.x;
 m[1,0]:=RightVector.y;
 m[2,0]:=RightVector.z;
 m[3,0]:=-((RightVector.x*Eye.x)+(RightVector.y*Eye.y)+(RightVector.z*Eye.z));
 m[0,1]:=UpVector.x;
 m[1,1]:=UpVector.y;
 m[2,1]:=UpVector.z;
 m[3,1]:=-((UpVector.x*Eye.x)+(UpVector.y*Eye.y)+(UpVector.z*Eye.z));
 m[0,2]:=ForwardVector.x;
 m[1,2]:=ForwardVector.y;
 m[2,2]:=ForwardVector.z;
 m[3,2]:=-((ForwardVector.x*Eye.x)+(ForwardVector.y*Eye.y)+(ForwardVector.z*Eye.z));
 m[0,3]:=0.0;
 m[1,3]:=0.0;
 m[2,3]:=0.0;
 m[3,3]:=1.0;
end;

function TMat4.Inverse:TMat4;
var
	t0,t4,t8,t12,d:TScalar;
begin
 t0:=(((m[1,1]*m[2,2]*m[3,3])-(m[1,1]*m[2,3]*m[3,2]))-(m[2,1]*m[1,2]*m[3,3])+(m[2,1]*m[1,3]*m[3,2])+(m[3,1]*m[1,2]*m[2,3]))-(m[3,1]*m[1,3]*m[2,2]);
 t4:=((((-(m[1,0]*m[2,2]*m[3,3]))+(m[1,0]*m[2,3]*m[3,2])+(m[2,0]*m[1,2]*m[3,3]))-(m[2,0]*m[1,3]*m[3,2]))-(m[3,0]*m[1,2]*m[2,3]))+(m[3,0]*m[1,3]*m[2,2]);
 t8:=((((m[1,0]*m[2,1]*m[3,3])-(m[1,0]*m[2,3]*m[3,1]))-(m[2,0]*m[1,1]*m[3,3]))+(m[2,0]*m[1,3]*m[3,1])+(m[3,0]*m[1,1]*m[2,3]))-(m[3,0]*m[1,3]*m[2,1]);
 t12:=((((-(m[1,0]*m[2,1]*m[3,2]))+(m[1,0]*m[2,2]*m[3,1])+(m[2,0]*m[1,1]*m[3,2]))-(m[2,0]*m[1,2]*m[3,1]))-(m[3,0]*m[1,1]*m[2,2]))+(m[3,0]*m[1,2]*m[2,1]);
 d:=(m[0,0]*t0)+(m[0,1]*t4)+(m[0,2]*t8)+(m[0,3]*t12);
 if d<>0.0 then begin
  d:=1.0/d;
  result.m[0,0]:=t0*d;
  result.m[0,1]:=(((((-(m[0,1]*m[2,2]*m[3,3]))+(m[0,1]*m[2,3]*m[3,2])+(m[2,1]*m[0,2]*m[3,3]))-(m[2,1]*m[0,3]*m[3,2]))-(m[3,1]*m[0,2]*m[2,3]))+(m[3,1]*m[0,3]*m[2,2]))*d;
  result.m[0,2]:=(((((m[0,1]*m[1,2]*m[3,3])-(m[0,1]*m[1,3]*m[3,2]))-(m[1,1]*m[0,2]*m[3,3]))+(m[1,1]*m[0,3]*m[3,2])+(m[3,1]*m[0,2]*m[1,3]))-(m[3,1]*m[0,3]*m[1,2]))*d;
  result.m[0,3]:=(((((-(m[0,1]*m[1,2]*m[2,3]))+(m[0,1]*m[1,3]*m[2,2])+(m[1,1]*m[0,2]*m[2,3]))-(m[1,1]*m[0,3]*m[2,2]))-(m[2,1]*m[0,2]*m[1,3]))+(m[2,1]*m[0,3]*m[1,2]))*d;
  result.m[1,0]:=t4*d;
  result.m[1,1]:=((((m[0,0]*m[2,2]*m[3,3])-(m[0,0]*m[2,3]*m[3,2]))-(m[2,0]*m[0,2]*m[3,3])+(m[2,0]*m[0,3]*m[3,2])+(m[3,0]*m[0,2]*m[2,3]))-(m[3,0]*m[0,3]*m[2,2]))*d;
  result.m[1,2]:=(((((-(m[0,0]*m[1,2]*m[3,3]))+(m[0,0]*m[1,3]*m[3,2])+(m[1,0]*m[0,2]*m[3,3]))-(m[1,0]*m[0,3]*m[3,2]))-(m[3,0]*m[0,2]*m[1,3]))+(m[3,0]*m[0,3]*m[1,2]))*d;
  result.m[1,3]:=(((((m[0,0]*m[1,2]*m[2,3])-(m[0,0]*m[1,3]*m[2,2]))-(m[1,0]*m[0,2]*m[2,3]))+(m[1,0]*m[0,3]*m[2,2])+(m[2,0]*m[0,2]*m[1,3]))-(m[2,0]*m[0,3]*m[1,2]))*d;
  result.m[2,0]:=t8*d;
  result.m[2,1]:=(((((-(m[0,0]*m[2,1]*m[3,3]))+(m[0,0]*m[2,3]*m[3,1])+(m[2,0]*m[0,1]*m[3,3]))-(m[2,0]*m[0,3]*m[3,1]))-(m[3,0]*m[0,1]*m[2,3]))+(m[3,0]*m[0,3]*m[2,1]))*d;
  result.m[2,2]:=(((((m[0,0]*m[1,1]*m[3,3])-(m[0,0]*m[1,3]*m[3,1]))-(m[1,0]*m[0,1]*m[3,3]))+(m[1,0]*m[0,3]*m[3,1])+(m[3,0]*m[0,1]*m[1,3]))-(m[3,0]*m[0,3]*m[1,1]))*d;
  result.m[2,3]:=(((((-(m[0,0]*m[1,1]*m[2,3]))+(m[0,0]*m[1,3]*m[2,1])+(m[1,0]*m[0,1]*m[2,3]))-(m[1,0]*m[0,3]*m[2,1]))-(m[2,0]*m[0,1]*m[1,3]))+(m[2,0]*m[0,3]*m[1,1]))*d;
  result.m[3,0]:=t12*d;
  result.m[3,1]:=(((((m[0,0]*m[2,1]*m[3,2])-(m[0,0]*m[2,2]*m[3,1]))-(m[2,0]*m[0,1]*m[3,2]))+(m[2,0]*m[0,2]*m[3,1])+(m[3,0]*m[0,1]*m[2,2]))-(m[3,0]*m[0,2]*m[2,1]))*d;
  result.m[3,2]:=(((((-(m[0,0]*m[1,1]*m[3,2]))+(m[0,0]*m[1,2]*m[3,1])+(m[1,0]*m[0,1]*m[3,2]))-(m[1,0]*m[0,2]*m[3,1]))-(m[3,0]*m[0,1]*m[1,2]))+(m[3,0]*m[0,2]*m[1,1]))*d;
  result.m[3,3]:=(((((m[0,0]*m[1,1]*m[2,2])-(m[0,0]*m[1,2]*m[2,1]))-(m[1,0]*m[0,1]*m[2,2]))+(m[1,0]*m[0,2]*m[2,1])+(m[2,0]*m[0,1]*m[1,2]))-(m[2,0]*m[0,2]*m[1,1]))*d;
 end;
end;

function TMat4.Transpose:TMat4;
begin
 result.m[0,0]:=m[0,0];
 result.m[0,1]:=m[1,0];
 result.m[0,2]:=m[2,0];
 result.m[0,3]:=m[3,0];
 result.m[1,0]:=m[0,1];
 result.m[1,1]:=m[1,1];
 result.m[1,2]:=m[2,1];
 result.m[1,3]:=m[3,1];
 result.m[2,0]:=m[0,2];
 result.m[2,1]:=m[1,2];
 result.m[2,2]:=m[2,2];
 result.m[2,3]:=m[3,2];
 result.m[3,0]:=m[0,3];
 result.m[3,1]:=m[1,3];
 result.m[3,2]:=m[2,3];
 result.m[3,3]:=m[3,3];
end;

procedure TMat4.Show; 
var
	x, y: integer;
begin
	for y := 0 to 3 do
		begin
			write('[');
			for x := 0 to 3 do
				begin
					if x < 3 then
						write(FloatToStr(m[x, y]),',')
					else
						write(FloatToStr(m[x, y]));
				end;
			writeln(']');
		end;
end;

class operator TMat4.:= (const a:TScalar):TMat4;
begin
 result.m[0,0]:=a;
 result.m[0,1]:=a;
 result.m[0,2]:=a;
 result.m[0,3]:=a;
 result.m[1,0]:=a;
 result.m[1,1]:=a;
 result.m[1,2]:=a;
 result.m[1,3]:=a;
 result.m[2,0]:=a;
 result.m[2,1]:=a;
 result.m[2,2]:=a;
 result.m[2,3]:=a;
 result.m[3,0]:=a;
 result.m[3,1]:=a;
 result.m[3,2]:=a;
 result.m[3,3]:=a;
end;

class operator TMat4.=(constref a,b:TMat4):boolean;
begin
 result:=SameValue(a.m[0,0],b.m[0,0]) and
         SameValue(a.m[0,1],b.m[0,1]) and
         SameValue(a.m[0,2],b.m[0,2]) and
         SameValue(a.m[0,3],b.m[0,3]) and
         SameValue(a.m[1,0],b.m[1,0]) and
         SameValue(a.m[1,1],b.m[1,1]) and
         SameValue(a.m[1,2],b.m[1,2]) and
         SameValue(a.m[1,3],b.m[1,3]) and
         SameValue(a.m[2,0],b.m[2,0]) and
         SameValue(a.m[2,1],b.m[2,1]) and
         SameValue(a.m[2,2],b.m[2,2]) and
         SameValue(a.m[2,3],b.m[2,3]) and
         SameValue(a.m[3,0],b.m[3,0]) and
         SameValue(a.m[3,1],b.m[3,1]) and
         SameValue(a.m[3,2],b.m[3,2]) and
         SameValue(a.m[3,3],b.m[3,3]);
end;

class operator TMat4.<>(constref a,b:TMat4):boolean;
begin
 result:=(not SameValue(a.m[0,0],b.m[0,0])) or
         (not SameValue(a.m[0,1],b.m[0,1])) or
         (not SameValue(a.m[0,2],b.m[0,2])) or
         (not SameValue(a.m[0,3],b.m[0,3])) or
         (not SameValue(a.m[1,0],b.m[1,0])) or
         (not SameValue(a.m[1,1],b.m[1,1])) or
         (not SameValue(a.m[1,2],b.m[1,2])) or
         (not SameValue(a.m[1,3],b.m[1,3])) or
         (not SameValue(a.m[2,0],b.m[2,0])) or
         (not SameValue(a.m[2,1],b.m[2,1])) or
         (not SameValue(a.m[2,2],b.m[2,2])) or
         (not SameValue(a.m[2,3],b.m[2,3])) or
         (not SameValue(a.m[3,0],b.m[3,0])) or
         (not SameValue(a.m[3,1],b.m[3,1])) or
         (not SameValue(a.m[3,2],b.m[3,2])) or
         (not SameValue(a.m[3,3],b.m[3,3]));
end;

class operator TMat4.+(constref a,b:TMat4):TMat4;
begin
 result.m[0,0]:=a.m[0,0]+b.m[0,0];
 result.m[0,1]:=a.m[0,1]+b.m[0,1];
 result.m[0,2]:=a.m[0,2]+b.m[0,2];
 result.m[0,3]:=a.m[0,3]+b.m[0,3];
 result.m[1,0]:=a.m[1,0]+b.m[1,0];
 result.m[1,1]:=a.m[1,1]+b.m[1,1];
 result.m[1,2]:=a.m[1,2]+b.m[1,2];
 result.m[1,3]:=a.m[1,3]+b.m[1,3];
 result.m[2,0]:=a.m[2,0]+b.m[2,0];
 result.m[2,1]:=a.m[2,1]+b.m[2,1];
 result.m[2,2]:=a.m[2,2]+b.m[2,2];
 result.m[2,3]:=a.m[2,3]+b.m[2,3];
 result.m[3,0]:=a.m[3,0]+b.m[3,0];
 result.m[3,1]:=a.m[3,1]+b.m[3,1];
 result.m[3,2]:=a.m[3,2]+b.m[3,2];
 result.m[3,3]:=a.m[3,3]+b.m[3,3];
end;

class operator TMat4.+(constref a:TMat4;const b:TScalar):TMat4;
begin
 result.m[0,0]:=a.m[0,0]+b;
 result.m[0,1]:=a.m[0,1]+b;
 result.m[0,2]:=a.m[0,2]+b;
 result.m[0,3]:=a.m[0,3]+b;
 result.m[1,0]:=a.m[1,0]+b;
 result.m[1,1]:=a.m[1,1]+b;
 result.m[1,2]:=a.m[1,2]+b;
 result.m[1,3]:=a.m[1,3]+b;
 result.m[2,0]:=a.m[2,0]+b;
 result.m[2,1]:=a.m[2,1]+b;
 result.m[2,2]:=a.m[2,2]+b;
 result.m[2,3]:=a.m[2,3]+b;
 result.m[3,0]:=a.m[3,0]+b;
 result.m[3,1]:=a.m[3,1]+b;
 result.m[3,2]:=a.m[3,2]+b;
 result.m[3,3]:=a.m[3,3]+b;
end;

class operator TMat4.+(const a:TScalar;constref b:TMat4):TMat4;
begin
 result.m[0,0]:=a+b.m[0,0];
 result.m[0,1]:=a+b.m[0,1];
 result.m[0,2]:=a+b.m[0,2];
 result.m[0,3]:=a+b.m[0,3];
 result.m[1,0]:=a+b.m[1,0];
 result.m[1,1]:=a+b.m[1,1];
 result.m[1,2]:=a+b.m[1,2];
 result.m[1,3]:=a+b.m[1,3];
 result.m[2,0]:=a+b.m[2,0];
 result.m[2,1]:=a+b.m[2,1];
 result.m[2,2]:=a+b.m[2,2];
 result.m[2,3]:=a+b.m[2,3];
 result.m[3,0]:=a+b.m[3,0];
 result.m[3,1]:=a+b.m[3,1];
 result.m[3,2]:=a+b.m[3,2];
 result.m[3,3]:=a+b.m[3,3];
end;

class operator TMat4.-(constref a,b:TMat4):TMat4;
begin
 result.m[0,0]:=a.m[0,0]-b.m[0,0];
 result.m[0,1]:=a.m[0,1]-b.m[0,1];
 result.m[0,2]:=a.m[0,2]-b.m[0,2];
 result.m[0,3]:=a.m[0,3]-b.m[0,3];
 result.m[1,0]:=a.m[1,0]-b.m[1,0];
 result.m[1,1]:=a.m[1,1]-b.m[1,1];
 result.m[1,2]:=a.m[1,2]-b.m[1,2];
 result.m[1,3]:=a.m[1,3]-b.m[1,3];
 result.m[2,0]:=a.m[2,0]-b.m[2,0];
 result.m[2,1]:=a.m[2,1]-b.m[2,1];
 result.m[2,2]:=a.m[2,2]-b.m[2,2];
 result.m[2,3]:=a.m[2,3]-b.m[2,3];
 result.m[3,0]:=a.m[3,0]-b.m[3,0];
 result.m[3,1]:=a.m[3,1]-b.m[3,1];
 result.m[3,2]:=a.m[3,2]-b.m[3,2];
 result.m[3,3]:=a.m[3,3]-b.m[3,3];
end;

class operator TMat4.-(constref a:TMat4;const b:TScalar):TMat4;
begin
 result.m[0,0]:=a.m[0,0]-b;
 result.m[0,1]:=a.m[0,1]-b;
 result.m[0,2]:=a.m[0,2]-b;
 result.m[0,3]:=a.m[0,3]-b;
 result.m[1,0]:=a.m[1,0]-b;
 result.m[1,1]:=a.m[1,1]-b;
 result.m[1,2]:=a.m[1,2]-b;
 result.m[1,3]:=a.m[1,3]-b;
 result.m[2,0]:=a.m[2,0]-b;
 result.m[2,1]:=a.m[2,1]-b;
 result.m[2,2]:=a.m[2,2]-b;
 result.m[2,3]:=a.m[2,3]-b;
 result.m[3,0]:=a.m[3,0]-b;
 result.m[3,1]:=a.m[3,1]-b;
 result.m[3,2]:=a.m[3,2]-b;
 result.m[3,3]:=a.m[3,3]-b;
end;

class operator TMat4.-(const a:TScalar;constref b:TMat4): TMat4;
begin
 result.m[0,0]:=a-b.m[0,0];
 result.m[0,1]:=a-b.m[0,1];
 result.m[0,2]:=a-b.m[0,2];
 result.m[0,3]:=a-b.m[0,3];
 result.m[1,0]:=a-b.m[1,0];
 result.m[1,1]:=a-b.m[1,1];
 result.m[1,2]:=a-b.m[1,2];
 result.m[1,3]:=a-b.m[1,3];
 result.m[2,0]:=a-b.m[2,0];
 result.m[2,1]:=a-b.m[2,1];
 result.m[2,2]:=a-b.m[2,2];
 result.m[2,3]:=a-b.m[2,3];
 result.m[3,0]:=a-b.m[3,0];
 result.m[3,1]:=a-b.m[3,1];
 result.m[3,2]:=a-b.m[3,2];
 result.m[3,3]:=a-b.m[3,3];
end;

class operator TMat4.*(constref b,a:TMat4):TMat4;
begin
 result.m[0,0]:=(a.m[0,0]*b.m[0,0])+(a.m[0,1]*b.m[1,0])+(a.m[0,2]*b.m[2,0])+(a.m[0,3]*b.m[3,0]);
 result.m[0,1]:=(a.m[0,0]*b.m[0,1])+(a.m[0,1]*b.m[1,1])+(a.m[0,2]*b.m[2,1])+(a.m[0,3]*b.m[3,1]);
 result.m[0,2]:=(a.m[0,0]*b.m[0,2])+(a.m[0,1]*b.m[1,2])+(a.m[0,2]*b.m[2,2])+(a.m[0,3]*b.m[3,2]);
 result.m[0,3]:=(a.m[0,0]*b.m[0,3])+(a.m[0,1]*b.m[1,3])+(a.m[0,2]*b.m[2,3])+(a.m[0,3]*b.m[3,3]);
 result.m[1,0]:=(a.m[1,0]*b.m[0,0])+(a.m[1,1]*b.m[1,0])+(a.m[1,2]*b.m[2,0])+(a.m[1,3]*b.m[3,0]);
 result.m[1,1]:=(a.m[1,0]*b.m[0,1])+(a.m[1,1]*b.m[1,1])+(a.m[1,2]*b.m[2,1])+(a.m[1,3]*b.m[3,1]);
 result.m[1,2]:=(a.m[1,0]*b.m[0,2])+(a.m[1,1]*b.m[1,2])+(a.m[1,2]*b.m[2,2])+(a.m[1,3]*b.m[3,2]);
 result.m[1,3]:=(a.m[1,0]*b.m[0,3])+(a.m[1,1]*b.m[1,3])+(a.m[1,2]*b.m[2,3])+(a.m[1,3]*b.m[3,3]);
 result.m[2,0]:=(a.m[2,0]*b.m[0,0])+(a.m[2,1]*b.m[1,0])+(a.m[2,2]*b.m[2,0])+(a.m[2,3]*b.m[3,0]);
 result.m[2,1]:=(a.m[2,0]*b.m[0,1])+(a.m[2,1]*b.m[1,1])+(a.m[2,2]*b.m[2,1])+(a.m[2,3]*b.m[3,1]);
 result.m[2,2]:=(a.m[2,0]*b.m[0,2])+(a.m[2,1]*b.m[1,2])+(a.m[2,2]*b.m[2,2])+(a.m[2,3]*b.m[3,2]);
 result.m[2,3]:=(a.m[2,0]*b.m[0,3])+(a.m[2,1]*b.m[1,3])+(a.m[2,2]*b.m[2,3])+(a.m[2,3]*b.m[3,3]);
 result.m[3,0]:=(a.m[3,0]*b.m[0,0])+(a.m[3,1]*b.m[1,0])+(a.m[3,2]*b.m[2,0])+(a.m[3,3]*b.m[3,0]);
 result.m[3,1]:=(a.m[3,0]*b.m[0,1])+(a.m[3,1]*b.m[1,1])+(a.m[3,2]*b.m[2,1])+(a.m[3,3]*b.m[3,1]);
 result.m[3,2]:=(a.m[3,0]*b.m[0,2])+(a.m[3,1]*b.m[1,2])+(a.m[3,2]*b.m[2,2])+(a.m[3,3]*b.m[3,2]);
 result.m[3,3]:=(a.m[3,0]*b.m[0,3])+(a.m[3,1]*b.m[1,3])+(a.m[3,2]*b.m[2,3])+(a.m[3,3]*b.m[3,3]);
end;

class operator TMat4.*(constref a:TMat4;const b:TScalar):TMat4;
begin
 result.m[0,0]:=a.m[0,0]*b;
 result.m[0,1]:=a.m[0,1]*b;
 result.m[0,2]:=a.m[0,2]*b;
 result.m[0,3]:=a.m[0,3]*b;
 result.m[1,0]:=a.m[1,0]*b;
 result.m[1,1]:=a.m[1,1]*b;
 result.m[1,2]:=a.m[1,2]*b;
 result.m[1,3]:=a.m[1,3]*b;
 result.m[2,0]:=a.m[2,0]*b;
 result.m[2,1]:=a.m[2,1]*b;
 result.m[2,2]:=a.m[2,2]*b;
 result.m[2,3]:=a.m[2,3]*b;
 result.m[3,0]:=a.m[3,0]*b;
 result.m[3,1]:=a.m[3,1]*b;
 result.m[3,2]:=a.m[3,2]*b;
 result.m[3,3]:=a.m[3,3]*b;
end;

class operator TMat4.*(const a:TScalar;constref b:TMat4):TMat4;
begin
 result.m[0,0]:=a*b.m[0,0];
 result.m[0,1]:=a*b.m[0,1];
 result.m[0,2]:=a*b.m[0,2];
 result.m[0,3]:=a*b.m[0,3];
 result.m[1,0]:=a*b.m[1,0];
 result.m[1,1]:=a*b.m[1,1];
 result.m[1,2]:=a*b.m[1,2];
 result.m[1,3]:=a*b.m[1,3];
 result.m[2,0]:=a*b.m[2,0];
 result.m[2,1]:=a*b.m[2,1];
 result.m[2,2]:=a*b.m[2,2];
 result.m[2,3]:=a*b.m[2,3];
 result.m[3,0]:=a*b.m[3,0];
 result.m[3,1]:=a*b.m[3,1];
 result.m[3,2]:=a*b.m[3,2];
 result.m[3,3]:=a*b.m[3,3];
end;

class operator TMat4.*(constref a:TMat4;constref b:TVec3):TVec3;
begin
 result.x:=(a.m[0,0]*b.x)+(a.m[1,0]*b.y)+(a.m[2,0]*b.z)+a.m[3,0];
 result.y:=(a.m[0,1]*b.x)+(a.m[1,1]*b.y)+(a.m[2,1]*b.z)+a.m[3,1];
 result.z:=(a.m[0,2]*b.x)+(a.m[1,2]*b.y)+(a.m[2,2]*b.z)+a.m[3,2];
end;

class operator TMat4.*(constref a:TVec3;constref b:TMat4):TVec3;
begin
 result.x:=(a.x*b.m[0,0])+(a.y*b.m[0,1])+(a.z*b.m[0,2])+b.m[0,3];
 result.y:=(a.x*b.m[1,0])+(a.y*b.m[1,1])+(a.z*b.m[1,2])+b.m[1,3];
 result.z:=(a.x*b.m[2,0])+(a.y*b.m[2,1])+(a.z*b.m[2,2])+b.m[2,3];
end;

class operator TMat4.*(constref a:TMat4;constref b:TVec4):TVec4;
begin
 result.x:=(a.m[0,0]*b.x)+(a.m[1,0]*b.y)+(a.m[2,0]*b.z)+(a.m[3,0]*b.w);
 result.y:=(a.m[0,1]*b.x)+(a.m[1,1]*b.y)+(a.m[2,1]*b.z)+(a.m[3,1]*b.w);
 result.z:=(a.m[0,2]*b.x)+(a.m[1,2]*b.y)+(a.m[2,2]*b.z)+(a.m[3,2]*b.w);
 result.w:=(a.m[0,3]*b.x)+(a.m[1,3]*b.y)+(a.m[2,3]*b.z)+(a.m[3,3]*b.w);
end;

class operator TMat4.*(constref a:TVec4;constref b:TMat4):TVec4;
begin
 result.x:=(a.x*b.m[0,0])+(a.y*b.m[0,1])+(a.z*b.m[0,2])+(a.w*b.m[0,3]);
 result.y:=(a.x*b.m[1,0])+(a.y*b.m[1,1])+(a.z*b.m[1,2])+(a.w*b.m[1,3]);
 result.z:=(a.x*b.m[2,0])+(a.y*b.m[2,1])+(a.z*b.m[2,2])+(a.w*b.m[2,3]);
 result.w:=(a.x*b.m[3,0])+(a.y*b.m[3,1])+(a.z*b.m[3,2])+(a.w*b.m[3,3]);
end;

class operator TMat4./(constref a,b:TMat4):TMat4;
begin
 result:=a*b.Inverse;
end;

class operator TMat4./(constref a:TMat4;const b:TScalar):TMat4;
begin
 result.m[0,0]:=a.m[0,0]/b;
 result.m[0,1]:=a.m[0,1]/b;
 result.m[0,2]:=a.m[0,2]/b;
 result.m[0,3]:=a.m[0,3]/b;
 result.m[1,0]:=a.m[1,0]/b;
 result.m[1,1]:=a.m[1,1]/b;
 result.m[1,2]:=a.m[1,2]/b;
 result.m[1,3]:=a.m[1,3]/b;
 result.m[2,0]:=a.m[2,0]/b;
 result.m[2,1]:=a.m[2,1]/b;
 result.m[2,2]:=a.m[2,2]/b;
 result.m[2,3]:=a.m[2,3]/b;
 result.m[3,0]:=a.m[3,0]/b;
 result.m[3,1]:=a.m[3,1]/b;
 result.m[3,2]:=a.m[3,2]/b;
 result.m[3,3]:=a.m[3,3]/b;
end;

class operator TMat4./(const a:TScalar;constref b:TMat4):TMat4;
begin
 result.m[0,0]:=a/b.m[0,0];
 result.m[0,1]:=a/b.m[0,1];
 result.m[0,2]:=a/b.m[0,2];
 result.m[0,3]:=a/b.m[0,3];
 result.m[1,0]:=a/b.m[1,0];
 result.m[1,1]:=a/b.m[1,1];
 result.m[1,2]:=a/b.m[1,2];
 result.m[1,3]:=a/b.m[1,3];
 result.m[2,0]:=a/b.m[2,0];
 result.m[2,1]:=a/b.m[2,1];
 result.m[2,2]:=a/b.m[2,2];
 result.m[2,3]:=a/b.m[2,3];
 result.m[3,0]:=a/b.m[3,0];
 result.m[3,1]:=a/b.m[3,1];
 result.m[3,2]:=a/b.m[3,2];
 result.m[3,3]:=a/b.m[3,3];
end;

end.
