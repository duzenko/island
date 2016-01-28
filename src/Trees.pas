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

implementation

{ TTrees }

class constructor TTrees.Create;
begin
  TThread.CreateAnonymousThread(procedure begin
    Trees := TModel3D.Create('..\models\trees9\untitled.obj');
    Bushes := TModel3D.Create('..\models\bushes\untitled.obj');
  end).Start;
end;

class procedure TTrees.Draw;
var
  i, tk: Integer;
  a, r: Single;
begin
  if Trees = nil then
    Exit;
  RandSeed := 0;
  for i := 0 to 9 do begin
    tk := i;//Random(10);
    a := {i/10*2*Pi;// }(0.9+i*0.05)*Pi;
    r := random*3 + 22;
      glPushMatrix;
      glTranslatef(r*sin(a), r*cos(a), 0);
      glScalef(3, 3, 3);
      if tk = 0 then
        tk := 1 shl 6 + 1
      else
        tk := 1 shl tk;
      Trees.TurnMeshes(tk);
      Trees.Draw;
      glPopMatrix;
  end;
  if Bushes = nil then
    Exit;
  for i := 0 to 8 do begin
    tk := i;//Random(10);
    a := {i/10*2*Pi;// }(0.82+i*0.05)*Pi;
    r := random*3 + 18;
      glPushMatrix;
      glTranslatef(r*sin(a), r*cos(a), 0);
      glScalef(4, 4, 4);
      Bushes.TurnMeshes(tk);
      Bushes.Draw;
      glPopMatrix;
  end;
end;

initialization

finalization
  FreeAndNil(TTrees.Trees);
  FreeAndNil(TTrees.Bushes);

end.
