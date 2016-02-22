unit Trees;

interface uses
  SysUtils, Classes, dglOpengl, Model3D;

type
  TTrees = class
    class constructor Create;
    class procedure Draw;
  class var
    Trees, Bushes: TModel3D;
  end;

implementation uses
  Shaders, Terrain;

{ TTrees }

class constructor TTrees.Create;
begin
  TThread.CreateAnonymousThread(procedure begin
    Trees := TModel3D.Create('..\models\trees9\untitled.obj');
    Bushes := TModel3D.Create('..\models\bushes\untitled.obj');
  end).Start;
end;

class procedure TTrees.Draw;

  procedure DrawRandom(m: TModel3D);
  var
    a, r: Single;
    i: Integer;
    p: TFastArray<TGLVectorf3>;
    v: TGLVectorf3;
  begin
    p.Count := 0;
    for i := 1 to 22 do begin
      r := random*99 + 22;
      a := (Random*2+0.4)*Pi;
      v[0] := r*sin(a);
      v[1] := r*cos(a);
      v[2] := GetHeight(v[0], v[1]);
      p.Add(v);
    end;
    m.Draw(p);
  end;

var
  i, tk: Integer;
begin
  if Trees = nil then
    Exit;
  RandSeed := 0;
  glPushMatrix;
  glScalef(4, 4, 4);
  for i := 0 to 9 do begin
    tk := i;//Random(10);
      if tk = 0 then
        tk := 1 shl 6 + 1
      else
        tk := 1 shl tk;
      Trees.TurnMeshes(tk);
      DrawRandom(Trees);
  end;
  glPopMatrix;
  if Bushes = nil then
    Exit;
  glPushMatrix;
  glScalef(4, 4, 4);
  for i := 0 to 8 do begin
    tk := 1 shl i;//Random(10);
      Bushes.TurnMeshes(tk);
      DrawRandom(Bushes);
  end;
  glPopMatrix;
end;

initialization

finalization
  FreeAndNil(TTrees.Trees);
  FreeAndNil(TTrees.Bushes);

end.
