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
    Trees := TModel3D.Create('..\models\treelp\xp.obj');
//    Trees := TModel3D.Create('..\models\trees9\untitled.obj');
//    Bushes := TModel3D.Create('..\models\bushes\untitled.obj');
  end).Start;
end;

class procedure TTrees.Draw;
const
  p: TFastArray<TGLVectorf3> = ();

  procedure DrawRandom(m: TModel3D);
  begin
    m.Draw(p);
  end;

var
  i, tk: Integer;
  var
    a, r: Single;
    v: TGLVectorf3;
begin
  if Trees = nil then
    Exit;
  if (p.Count = 0) and (TerrainSize.cy > 0) then
    for i := 1 to 2222 do begin
      r := random*999 + 44;
      a := (Random*2+0.4)*Pi;
      v[0] := r*sin(a);
      v[1] := r*cos(a);
      v[2] := GetHeight(v[0], v[1]);
      p.Add(v);
    end;

  RandSeed := 0;
  glPushMatrix;
//  glTranslatef(0, 0, 1);
  glScalef(9);
  for i := 0 to 9 do begin
//    tk := i;//Random(10);
//      if tk = 0 then
//        tk := 1 shl 6 + 1
//      else
//        tk := 1 shl tk;
//      Trees.TurnMeshes(tk);
      glDisable(GL_CULL_FACE);
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      DrawRandom(Trees);
      glEnable(GL_CULL_FACE);
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
