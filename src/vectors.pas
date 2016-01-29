unit vectors;

interface uses
  Windows, dglOpenGl;

type
//  PVector = ^TVector;
  TVector = record
    class operator Implicit(const a: TGLVectorf3): TVector;
    class operator Implicit(const a: TVector): TGLVectorf3;
    class operator Implicit(a: TVector): Single;
    class operator Negative(const a: TVector): TVector;
    class operator Add(const a, b: TVector): TVector;
    class operator Subtract(const a, b: TVector): TVector;
    class operator Multiply(const a: TVector; b: Single): TVector;
    class operator Multiply(const a, b: TVector): Single;
    class operator Modulus(const a, b: TVector): TVector;
    constructor Create(a, b, c: Single; d: Single = 1);
    function Normalise: Boolean;
    function Inside(const a, b: TVector): ShortInt;
    case byte of
    0:
      (x, y, z, w: Single);
    1:
      (v: TGLVectorf4);
  end;

  TMatrix = record
  private
    procedure MatrixAdjoint;
    function  Determinant(): Single;
    procedure Scale(Factor: Single); register;
  const X = 0; Y = 1; Z = 2; W = 3;
  public
    class operator Multiply(const M1, M2: TMatrix): TMatrix;
    class operator Multiply(const M: TMatrix; const V: TVector): TVector;
    procedure CalcTransformationMatrix(const a, b: TVector);
    procedure CalcTransformationMatrix2(const a, b: TVector);
    function Invert3: TMatrix;
    procedure Invert;
    function Transpose3: TMatrix;
    function Transpose: TMatrix;
    procedure Mul3(const a, b: TMatrix);
    procedure Translate(x, y, z: Single);
    procedure Rotate(angle: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat);
    case Byte of
    0:
      (v0, v1, v2, v3: TVector);
    1:
      (v: array[0..3] of TVector);
    2:
      (M: array[0..3, 0..3] of Single);
  end;
  PMatrix = ^TMatrix;

function VectorMul(const v: TGLVectorf3; f: Single): TGLVectorf3;
function VectorNew(f: Single): TGLVectorf3;
procedure VectorSlide(var v: TGLVectorf3; f: Single); overload;
procedure VectorSlide(var v: TGLVectorf3; const f: TGLVectorf3); overload;

const
  NullVector: TVector = (w: 1);
  IdentityMatrix: TMatrix = (v:(
    (v: (1, 0, 0, 0)),
    (v: (0, 1, 0, 0)),
    (v: (0, 0, 1, 0)),
    (v: (0, 0, 0, 1))
  ));

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

class operator TVector.Implicit(const a: TVector): TGLVectorf3;
begin
  Move(a, Result, 12);
end;

function TVector.Inside(const a, b: TVector): ShortInt;
begin
  Result := 0;
  if y < a.y then
    Result := -1
  else
    if y > b.y then
      Result := +1;
  if x < a.x then
    Dec(Result, 3)
  else
    if x > b.x then
      Inc(Result, 3);
end;

class operator TVector.Implicit(const a: TGLVectorf3): TVector;
begin
  Move(a, Result.v, 12);
  Result.w := 1;
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

class operator TVector.Negative(const a: TVector): TVector;
begin
  Result.x := -a.x;
  Result.y := -a.y;
  Result.z := -a.z;
end;

function TVector.Normalise;
var
  f: Single;
begin
  f := Self;
  Result := f > 0;
  if Result then begin
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
  axis: TVector;
  M1, M1R, M2: TMatrix;
begin
  axis := a mod b;
  if not axis.Normalise then begin
    Self := IdentityMatrix;
    Exit;
  end;
//  v4 := axis mod a;
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
  V[2].z := 1;
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

procedure TMatrix.CalcTransformationMatrix2(const a, b: TVector);
begin
  Self := IdentityMatrix;
  v[0].x := a.x*b.x + a.y*b.y;
  v[0].y := -b.x*a.y + a.x*b.y;
  v[1].x := -v[0].y;
  v[1].y := v[0].x;
end;

function MatrixDetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;

// internal version for the determinant of a 3x3 matrix

begin
  Result := a1 * (b2 * c3 - b3 * c2) -
            b1 * (a2 * c3 - a3 * c2) +
            c1 * (a2 * b3 - a3 * b2);
end;

function TMatrix.Determinant: Single;
var a1, a2, a3, a4,
    b1, b2, b3, b4,
    c1, c2, c3, c4,
    d1, d2, d3, d4  : Single;
begin
  a1 := M[X, X];  b1 := M[X, Y];  c1 := M[X, Z];  d1 := M[X, W];
  a2 := M[Y, X];  b2 := M[Y, Y];  c2 := M[Y, Z];  d2 := M[Y, W];
  a3 := M[Z, X];  b3 := M[Z, Y];  c3 := M[Z, Z];  d3 := M[Z, W];
  a4 := M[W, X];  b4 := M[W, Y];  c4 := M[W, Z];  d4 := M[W, W];

  Result := a1 * MatrixDetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4) -
            b1 * MatrixDetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4) +
            c1 * MatrixDetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4) -
            d1 * MatrixDetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);
end;

procedure TMatrix.MatrixAdjoint();

// Adjoint of a 4x4 matrix - used in the computation of the inverse
// of a 4x4 matrix

var a1, a2, a3, a4,
    b1, b2, b3, b4,
    c1, c2, c3, c4,
    d1, d2, d3, d4: Single;


begin
    a1 :=  M[X, X]; b1 :=  M[X, Y];
    c1 :=  M[X, Z]; d1 :=  M[X, W];
    a2 :=  M[Y, X]; b2 :=  M[Y, Y];
    c2 :=  M[Y, Z]; d2 :=  M[Y, W];
    a3 :=  M[Z, X]; b3 :=  M[Z, Y];
    c3 :=  M[Z, Z]; d3 :=  M[Z, W];
    a4 :=  M[W, X]; b4 :=  M[W, Y];
    c4 :=  M[W, Z]; d4 :=  M[W, W];

    // row column labeling reversed since we transpose rows & columns
    M[X, X] :=  MatrixDetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4);
    M[Y, X] := -MatrixDetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4);
    M[Z, X] :=  MatrixDetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4);
    M[W, X] := -MatrixDetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);

    M[X, Y] := -MatrixDetInternal(b1, b3, b4, c1, c3, c4, d1, d3, d4);
    M[Y, Y] :=  MatrixDetInternal(a1, a3, a4, c1, c3, c4, d1, d3, d4);
    M[Z, Y] := -MatrixDetInternal(a1, a3, a4, b1, b3, b4, d1, d3, d4);
    M[W, Y] :=  MatrixDetInternal(a1, a3, a4, b1, b3, b4, c1, c3, c4);

    M[X, Z] :=  MatrixDetInternal(b1, b2, b4, c1, c2, c4, d1, d2, d4);
    M[Y, Z] := -MatrixDetInternal(a1, a2, a4, c1, c2, c4, d1, d2, d4);
    M[Z, Z] :=  MatrixDetInternal(a1, a2, a4, b1, b2, b4, d1, d2, d4);
    M[W, Z] := -MatrixDetInternal(a1, a2, a4, b1, b2, b4, c1, c2, c4);

    M[X, W] := -MatrixDetInternal(b1, b2, b3, c1, c2, c3, d1, d2, d3);
    M[Y, W] :=  MatrixDetInternal(a1, a2, a3, c1, c2, c3, d1, d2, d3);
    M[Z, W] := -MatrixDetInternal(a1, a2, a3, b1, b2, b3, d1, d2, d3);
    M[W, W] :=  MatrixDetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3);
end;

procedure TMatrix.Scale(Factor: Single); register;

// multiplies all elements of a 4x4 matrix with a factor

var I, J: Integer;

begin
  for I := 0 to 3 do
    for J := 0 to 3 do M[I, J] := M[I, J] * Factor;
end;

const
  EPSILON  = 1e-100;
  EPSILON2 = 1e-50;
procedure TMatrix.Translate(x, y, z: Single);
var
  tm: TMatrix;
begin
  tm := IdentityMatrix;
  tm.M[3, 0] := x;
  tm.M[3, 1] := y;
  tm.M[3, 2] := z;
  self := Self * tm;
end;

procedure TMatrix.Invert;
var Det: Single;

begin
  Det := Determinant();
  if Abs(Det) < EPSILON then Self := IdentityMatrix
                        else
  begin
    MatrixAdjoint();
    Scale(1 / Det);
  end;
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

class operator TMatrix.Multiply(const M: TMatrix; const V: TVector): TVector;
begin
  Result.v[X] := V.v[X] * M.M[X, X] + V.v[Y] * M.M[Y, X] + V.v[Z] * M.M[Z, X] + M.M[W, X];
  Result.v[Y] := V.v[X] * M.M[X, Y] + V.v[Y] * M.M[Y, Y] + V.v[Z] * M.M[Z, Y] + M.M[W, Y];
  Result.v[Z] := V.v[X] * M.M[X, Z] + V.v[Y] * M.M[Y, Z] + V.v[Z] * M.M[Z, Z] + M.M[W, Z];
  Result.v[W] := V.v[X] * M.M[X, W] + V.v[Y] * M.M[Y, W] + V.v[Z] * M.M[Z, W] + M.M[W, W];
end;

procedure TMatrix.Rotate(angle, x, y, z: GLfloat);
var
  rm: TMatrix;
  s, c: Double;
begin
  SineCosine(angle/180*Pi, s, c);
  rm := IdentityMatrix;
  rm.v[0] := TVector.Create(c + x*x*(1-c), x*y*(1-c)+z*s, z*x*(1-c)-y*s, 0);
  rm.v[1] := TVector.Create(x*y*(1-c)-z*s, c + y*y*(1-c), z*y*(1-c)+x*s, 0);
  rm.v[2] := TVector.Create(x*z*(1-c)+y*s, z*y*(1-c)-x*s, c + z*z*(1-c), 0);
  self := Self * rm;
end;

class operator TMatrix.Multiply(const M1, M2: TMatrix): TMatrix;
var I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      Result.M[I, J] :=
                  M1.M[X, I] * M2.M[J, X] +
                  M1.M[Y, I] * M2.M[J, Y] +
                  M1.M[Z, I] * M2.M[J, Z] +
                  M1.M[W, I] * M2.M[J, W];
{                  M1.M[I, X] * M2.M[X, J] +
                  M1.M[I, Y] * M2.M[Y, J] +
                  M1.M[I, Z] * M2.M[Z, J] +
                  M1.M[I, W] * M2.M[W, J];{}
end;

function TMatrix.Transpose: TMatrix;
begin
  Result.v0 := TVector.Create(v0.x, v1.x, v2.x, v3.x);
  Result.v1 := TVector.Create(v0.y, v1.y, v2.y, v3.y);
  Result.v2 := TVector.Create(v0.z, v1.z, v2.z, v3.z);
  Result.v3 := TVector.Create(v0.w, v1.w, v2.w, v3.w);
end;

function TMatrix.Transpose3: TMatrix;
begin
  Result.v0 := TVector.Create(v0.x, v1.x, v2.x);
  Result.v1 := TVector.Create(v0.y, v1.y, v2.y);
  Result.v2 := TVector.Create(v0.z, v1.z, v2.z);
end;

end.
