unit Obstacles;

interface uses
  Classes, Vectors;

type
  TObstacles = class
    class procedure FixDir(var Dir: TVector; const Loc: TVector);
  end;

implementation

{ TObstacles }

class procedure TObstacles.FixDir(var Dir: TVector; const Loc: TVector);
const
  HouseX1 = 6;
  HouseX2 = 10;
  HouseY1 = -22;
  HouseY2 = -2;
  NormX1: TVector = (v: (-1, 0, 0, 0));
  NormX2: TVector = (v: (+1, 0, 0, 0));
  NormY1: TVector = (v: (0, -1, 0, 0));
  NormY2: TVector = (v: (0, +1, 0, 0));
var
  ObstNorm, RightVec, TestLoc: TVector;
begin
  ObstNorm := TVector.Create(0, 0, 0);
  TestLoc := Loc + Dir;
  if (Loc.x < HouseX1) and (TestLoc.x > HouseX1) then
    ObstNorm := ObstNorm + NormX1;
  if (Loc.x > HouseX2) and (TestLoc.x < HouseX2) then
    ObstNorm := ObstNorm + NormX2;
  if (Loc.y < HouseY1) and (TestLoc.y > HouseY1) then
    ObstNorm := ObstNorm + NormY1;
  if (Loc.y < HouseY2) and (TestLoc.y > HouseY2) then
    ObstNorm := ObstNorm + NormY2;
  if not ObstNorm.Normalise then
    Exit;
  RightVec := ObstNorm mod Dir;
  Dir := -(ObstNorm mod Dir);
end;

end.
