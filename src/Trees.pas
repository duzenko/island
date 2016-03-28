unit Trees;

interface uses
  SysUtils, Classes, dglOpengl, Model3D;

type
  TTrees = class
//    constructor Create;
    class procedure Draw;
  class var
    Trees, Bushes: TModel3D;
  end;

implementation uses
  Shaders, Terrain;

{ TTrees }

class procedure TTrees.Draw;
const  {$J+}
  p: TFloat3Array = nil;

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

  if (p = nil) and (TerrainSize.cy > 0) then begin
    p := TFloat3Array.Create;
    for i := 1 to 2222 do begin
      r := random*999 + 44;
      a := (Random*2+0.4)*Pi;
      v[0] := r*sin(a);
      v[1] := r*cos(a);
      v[2] := GetHeight(v[0], v[1]);
      p.Add(v);
    end;
  end;

  RandSeed := 0;
  glPushMatrix;
  glScalef(9);
  for i := 0 to 0 do begin
      glDisable(GL_CULL_FACE);
      glEnable(GL_BLEND);
//      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glBlendFunc(GL_ONE_MINUS_SRC_COLOR, GL_SRC_COLOR);
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

procedure LoadAll;
begin
    TTrees.Trees := TModel3D.Create('..\models\treelp\xp.obj');
//    TTrees.Trees := TModel3D.Create('..\models\trees9\untitled.obj');
//    TTrees.Bushes := TModel3D.Create('..\models\bushes\untitled.obj');
end;

var
  tid: Cardinal;
initialization
  BeginThread(nil, 0, @LoadAll, nil, 0, tid);
//  TThread.CreateAnonymousThread(procedure begin
//    Trees := TModel3D.Create('..\models\treelp\xp.obj');
//    Trees := TModel3D.Create('..\models\trees9\untitled.obj');
//    Bushes := TModel3D.Create('..\models\bushes\untitled.obj');
//  end).Start;

finalization
  FreeAndNil(TTrees.Trees);
  FreeAndNil(TTrees.Bushes);

end.
