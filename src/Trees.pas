unit Trees;

interface uses
  SysUtils, Classes, dglOpengl, Model3D;

type
  TTrees = class
    class constructor Create;
    class procedure Draw;
  class var
    Model3d: TModel3D;
  end;

implementation

{ TTrees }

class constructor TTrees.Create;
begin
  TThread.CreateAnonymousThread(procedure begin
    Model3D := TModel3D.Create('..\models\trees9\untitled.obj');
  end).Start;
end;

class procedure TTrees.Draw;
var
  i, tk: Integer;
  x, y, a, r: Single;
begin
  if Model3d = nil then
    Exit;
  RandSeed := 0;
  for i := 0 to 9 do begin
    tk := i;//Random(10);
    a := i/10*2*Pi;// random*Pi+Pi*1/4;
    r := random*1 + 4;
      glPushMatrix;
      glTranslatef(r*sin(a), r*cos(a), 0);
      glScalef(3, 3, 3);
      if tk = 0 then
        tk := 1 shl 6 + 1
      else
        tk := 1 shl tk;
      Model3d.TurnMeshes(tk);
      Model3d.Draw;
      glPopMatrix;
  end;
end;

initialization

finalization
  FreeAndNil(TTrees.Model3d);

end.
