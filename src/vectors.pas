unit vectors;

interface uses
  dglOpenGl;

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

end.
