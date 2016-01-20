unit vectors;

interface uses
  dglOpenGl;

type
  TAffineVector = TGLVectorf3;
function VectorMul(const v: TGLVectorf3; f: Single): TGLVectorf3;
function VectorNew(f: Single): TGLVectorf3;
procedure VectorSlide(var v: TGLVectorf3; f: Single); overload;
procedure VectorSlide(var v: TGLVectorf3; const f: TGLVectorf3); overload;
function VectorCrossProduct(V1, V2: TAffineVector): TAffineVector;
function VectorAffineSubtract(V1, V2: TAffineVector): TAffineVector;
function Quaternion2Matrix(Q: TGLVectorf4): TGLMatrixf4;

implementation

function VectorNew(f: Single): TGLVectorf3;
begin
  Result[0] := f;
  Result[1] := f;
  Result[2] := f;
end;

function VectorMul(const v: TGLVectorf3; f: Single): TGLVectorf3;
begin
  Result[0] := v[0]*f;
  Result[1] := v[1]*f;
  Result[2] := v[2]*f;
end;

const
  SlideFade = 0.003;

procedure VectorSlide(var v: TGLVectorf3; f: Single);
begin
  v[0] := v[0]*(1-SlideFade) + f*SlideFade;
  v[1] := v[0];
  v[2] := v[0];
end;

procedure VectorSlide(var v: TGLVectorf3; const f: TGLVectorf3);
begin
  v[0] := v[0]*(1-SlideFade) + f[0]*SlideFade;
  v[1] := v[1]*(1-SlideFade) + f[1]*SlideFade;
  v[2] := v[2]*(1-SlideFade) + f[2]*SlideFade;
end;

const X=0; Y=1; Z=2;
function VectorAffineSubtract(V1, V2: TAffineVector): TAffineVector;
// returns v1 minus v2
// EAX contains address of V1
// EDX contains address of V2
// ECX contains address of the result
begin
  Result[X] := V1[X]-V2[X];
  Result[Y] := V1[Y]-V2[Y];
  Result[Z] := V1[Z]-V2[Z];
end;

function VectorCrossProduct(V1, V2: TAffineVector): TAffineVector;
// calculates the cross product between vector 1 and 2, Temp is necessary because
// either V1 or V2 could also be the result vector
//
// EAX contains address of V1
// EDX contains address of V2
// ECX contains address of result
var Temp: TAffineVector;
begin
  Temp[X] := V1[Y] * V2[Z]-V1[Z] * V2[Y];
  Temp[Y] := V1[Z] * V2[X]-V1[X] * V2[Z];
  Temp[Z] := V1[X] * V2[Y]-V1[Y] * V2[X];
  Result := Temp;
end;

function Quaternion2Matrix(Q: TGLVectorf4): TGLMatrixf4;
var
  x2, y2, z2, xy, xz, yz, wx, wy, wz: Single;
begin
	x2 := q[0] * q[0];
	y2 := q[1] * q[1];
	z2 := q[2] * q[2];
	xy := q[0] * q[1];
	xz := q[0] * q[2];
	yz := q[1] * q[2];
	wx := q[3] * q[0];
	wy := q[3] * q[1];
	wz := q[3] * q[2];
  Result[0, 0] := 1.0 - 2.0 * (y2 + z2);
  Result[0, 1] := 2.0 * (xy - wz);
  Result[0, 2] := 2.0 * (xz + wy);
  Result[0, 3] := 0;
  Result[1, 0] := 2.0 * (xy + wz);
  Result[1, 1] := 1.0 - 2.0 * (x2 + z2);
  Result[1, 2] := 2.0 * (yz - wx);
  Result[1, 3] := 0;
  Result[2, 0] := 2.0 * (xz - wy);
  Result[2, 1] := 2.0 * (yz + wx);
  Result[2, 2] := 1.0 - 2.0 * (x2 + y2);
  Result[2, 3] := 0;
  Result[3, 0] := 0;
  Result[3, 1] := 0;
  Result[3, 2] := 0;
  Result[3, 3] := 1;
end;

end.
