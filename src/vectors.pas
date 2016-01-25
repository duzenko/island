unit vectors;

interface uses
  Windows, dglOpenGl;

type
//  PVector = ^TVector;
  TVector = record
    class operator Implicit(const a: TGLVectorf3): TVector;
//    class operator Implicit(const a: TVector3): TAffineVector;
    class operator Implicit(a: TVector): Single;
    class operator Add(const a, b: TVector): TVector;
    class operator Subtract(const a, b: TVector): TVector;
    class operator Multiply(const a: TVector; b: Single): TVector;
    class operator Multiply(const a, b: TVector): Single;
    class operator Modulus(const a, b: TVector): TVector;
    constructor Create(a, b, c: Single; d: Single = 1);
    procedure Normalise;
    case byte of
    0:
      (x, y, z, w: Single);
    1:
      (v: TGLVectorf3);
  end;

  TMatrix = record
    procedure CalcTransformationMatrix(const a, b: TVector);
    function Invert3: TMatrix;
    function Transpose: TMatrix;
    procedure Mul3(const a, b: TMatrix);
    case Byte of
    0:
      (v0, v1, v2, v3: TVector);
    1:
      (v: array[0..3] of TVector);
  end;

function VectorMul(const v: TGLVectorf3; f: Single): TGLVectorf3;
function VectorNew(f: Single): TGLVectorf3;
procedure VectorSlide(var v: TGLVectorf3; f: Single); overload;
procedure VectorSlide(var v: TGLVectorf3; const f: TGLVectorf3); overload;

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

{ TVector3f }

//class operator TVector3.Implicit(const a: TGLVectorf3): TVector3;
//begin
//  Result := PVector3(@a)^;
//end;

class operator TVector.Add(const a, b: TVector): TVector;
begin
  Result.x := a.x + b.x;
  Result.y := a.y + b.y;
  Result.z := a.z + b.z;
end;

constructor TVector.Create(a, b, c: Single; d: Single = 1);
begin
  x := a;
  y := b;
  z := c;
  w := d;
end;

class operator TVector.Implicit(a: TVector): Single;
begin
  Result := Sqr(a.x) + Sqr(a.y) + Sqr(a.z);
  if Result > 0 then
    Result := Sqrt(Result);
end;

class operator TVector.Implicit(const a: TGLVectorf3): TVector;
begin
  Result.v := a;
end;

class operator TVector.Modulus(const a, b: TVector): TVector;
begin
  Result.x := a.Y * b.Z-a.Z * b.Y;
  Result.Y := a.Z * b.X-a.X * b.z;
  Result.z := a.X * b.y-a.y * b.X;
end;

class operator TVector.Multiply(const a, b: TVector): Single;
begin
  Result := a.X * b.X + a.Y * b.Y + a.Z * b.Z;
end;

//class operator TVector.Implicit(const a: TVector): TAffineVector;
//begin
//  Result := TAffineVector(pointer(@a)^);
//end;

class operator TVector.Multiply(const a: TVector; b: Single): TVector;
begin
  Result.x := a.x * b;
  Result.y := a.y * b;
  Result.z := a.z * b;
end;

procedure TVector.Normalise;
var
  f: Single;
begin
  f := Self;
  if f > 0 then begin
    f := 1/f;
    x := x*f;
    y := y*f;
    z := z*f;
  end;
end;

class operator TVector.Subtract(const a, b: TVector): TVector;
begin
  Result.x := a.x - b.x;
  Result.y := a.y - b.y;
  Result.z := a.z - b.z;
end;

{ TMatrix }

procedure TMatrix.CalcTransformationMatrix(const a, b: TVector);
var
  axis, v4: TVector;
  M1, M1R, M2, tmp: TMatrix;
  c, s: Single;
begin
  axis := a mod b;
  axis.Normalise;
  v4 := v3 mod a;
  M1.v0 := a;
  M2.v0 := b;
  M1.v1 := axis;
  M2.v1 := axis;
  M1.v2 := axis mod a;
  M2.v2 := axis mod b;
  M1R := M1.Invert3;
  Mul3(M2, M1R);
{  M1R := M1.Invert3;
  c := b * a;
  s := b * v4;
  M2.v0 := TVector.Create(c, s, 0);
  M2.v1 := TVector.Create(-s, c, 0);
  M2.v2 := TVector.Create(0, 0, 1);
  M2.v3 := TVector.Create(0, 0, 0);
  tmp.Mul3(M1R, M2);
  Mul3(tmp, M1);}
  V[0].w := 0;
  V[1].w := 0;
  V[2].w := 0;
  Self.V[3] := TVector.Create(0, 0, 0, 1);
end;

function det2(a1, a2, b1, b2: Single): Single;
begin
  Result := a1 * b2 - a2*b1;
end;

function det3(a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;
// internal version for the determinant of a 3x3 matrix
begin
  Result := a1 * (b2 * c3 - b3 * c2) -
            b1 * (a2 * c3 - a3 * c2) +
            c1 * (a2 * b3 - a3 * b2);
end;

function TMatrix.Invert3;
var
  det: Single;
begin
  det := det3(v0.x,v0.y,v0.z,v1.x,v1.y,v1.z,v2.x,v2.y,v2.z);
  Result.v0.x := det2(v1.y, v1.z, v2.y, v2.z)/det;
  Result.v0.y := det2(v1.x, v1.z, v2.x, v2.z)/det;
  Result.v0.z := det2(v1.x, v1.y, v2.x, v2.y)/det;
  Result.v1.x := det2(v0.y, v0.z, v2.y, v2.z)/det;
  Result.v1.y := det2(v0.x, v0.z, v2.x, v2.z)/det;
  Result.v1.z := det2(v0.x, v0.y, v2.x, v2.y)/det;
  Result.v2.x := det2(v0.y, v0.z, v1.y, v1.z)/det;
  Result.v2.y := det2(v0.x, v0.z, v1.x, v1.z)/det;
  Result.v2.z := det2(v0.x, v0.y, v1.x, v1.y)/det;
//  Scale(
end;

{procedure TMatrix.Mul3(const a, b: TMatrix);
var
  i, j, m: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do begin
      v[i].v[j] := 0;
      for m := 0 to 2 do
        v[i].v[j] := v[i].v[j] + a.v[i].v[m]*b.v[m].v[j];
    end;
end;   }

procedure TMatrix.Mul3(const a, b: TMatrix);
var
  i, j, m: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do begin
      v[i].v[j] := 0;
      for m := 0 to 2 do
        v[i].v[j] := v[i].v[j] + a.v[i].v[m]*b.v[m].v[j];
    end;
end;

function TMatrix.Transpose: TMatrix;
begin
  Result.v0 := TVector.Create(v0.x, v1.x, v2.x);
  Result.v1 := TVector.Create(v0.y, v1.y, v2.y);
  Result.v2 := TVector.Create(v0.z, v1.z, v2.z);
end;

end.
