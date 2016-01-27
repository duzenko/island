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
  HouseC1: TVector = (x: 6; y: -22);
  HouseC2: TVector = (x: 12; y: -2);
  TentC1: TVector = (x: 6; y: -2);
  TentC2: TVector = (x: 12; y: 0);
  ObstNorms: array[-4..4] of TVector = (
    (v: (-1, -1, 0, 0)),
    (v: (-1, 0, 0, 0)),
    (v: (-1, +1, 0, 0)),
    (v: (0, -1, 0, 0)),
    (),
    (v: (0, +1, 0, 0)),
    (v: (+1, -1, 0, 0)),
    (v: (+1, 0, 0, 0)),
    (v: (+1, +1, 0, 0))
  );

var
  TestInside, LocInside: ShortInt;
  ObstNorm, RightVec, TestLoc: TVector;

  procedure TestHouse;
  begin
    TestInside := TestLoc.Inside(HouseC1, HouseC2);
    if TestInside <> 0 then
      Exit;
    LocInside := Loc.Inside(HouseC1, HouseC2);
    ObstNorm := ObstNorms[LocInside];
    if not ObstNorm.Normalise then
      Exit;
    RightVec := ObstNorm mod Dir;
    if RightVec.Normalise then
      Dir := -(ObstNorm mod RightVec);
  end;

  procedure TestTent;
  begin
    TestInside := TestLoc.Inside(TentC1, TentC2);
    if TestInside <> 0 then
      Dir.z := -Loc.z
    else
      Dir.z := 0.5 - Loc.z;
  end;

begin
  TestLoc := Loc + Dir;
  TestHouse;
  TestLoc := Loc + Dir;
  TestTent;
end;

end.
