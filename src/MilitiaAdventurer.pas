unit MilitiaAdventurer;

interface uses
  Windows, Model3D;

type
  TMilitiaAdventurer = class
  class var
    Model3d: T3DModel;
    class constructor Create;
    procedure Draw;
  end;

implementation

type
  TMesh = (Body, Sheat, Purse, Shield, Sword1, LegBag,
    LegKnife, Bandolera, ThrowingKnife1, ThrowingKnife2,
    Scroll1, ThrowingKnife3, Scroll2, Potion, PitchFork,
    Shovel, Scythe, Sword2);
  TAnimations = (Idle, Attack1, Attack2, ShieldBlock, Cheering,
    Death1, Death2, UsePitchfork, UseShovel, HoldScythe, Walk);
  TAnimationFrames = record 
    StartNo, EndNo: Integer 
  end;
  
const
  AnimationFrames: array[TAnimations] of TAnimationFrames = (
    (StartNo: 1; EndNo: 210),
    (StartNo: 211; EndNo: 250),
    (StartNo: 251; EndNo: 270),
    (StartNo: 271; EndNo: 285),
    (StartNo: 286; EndNo: 310),
    (StartNo: 311; EndNo: 333),
    (StartNo: 334; EndNo: 345),
    (StartNo: 346; EndNo: 375),
    (StartNo: 376; EndNo: 390),
    (StartNo: 391; EndNo: 394),
    (StartNo: 395; EndNo: 415)
  );
  cDefaultMeshes = [Body, Sheat, Purse, Shield, LegBag,
    LegKnife, Bandolera, ThrowingKnife1, ThrowingKnife2,
    Scroll1, ThrowingKnife3, Scroll2, Potion];
  cAnimationMeshes: array[TAnimations] of set of TMesh = (
    cDefaultMeshes + [Sword1],
    cDefaultMeshes + [Sword1],
    cDefaultMeshes + [Sword1],
    cDefaultMeshes + [Sword1],
    cDefaultMeshes + [Sword1],
    cDefaultMeshes + [Sword1],
    cDefaultMeshes + [Sword1],
    cDefaultMeshes + [PitchFork],
    cDefaultMeshes + [Shovel],
    cDefaultMeshes + [Scythe],
    cDefaultMeshes + [Sword1]
  );

{ TMilitiaAdventurer }

class constructor TMilitiaAdventurer.Create;
begin
  Model3D := T3DModel.Create('D:\temp\peasant\test.obj');
end;

procedure TMilitiaAdventurer.Draw;
var
  frame: Integer;
  ani: TAnimations;
begin
  frame := gettickcount div 40 mod 415 + 1;
  if T3DModel.DebugDraw then
    frame := T3DModel.DebugIndex+1;
  for ani := Low(TAnimations) to High(TAnimations) do     
    if (frame >= AnimationFrames[ani].StartNo) and (frame <= AnimationFrames[ani].EndNo) then
      Model3d.TurnMeshes(Cardinal(cAnimationMeshes[ani]));
  Model3d.Draw(frame - 1);
end;

end.
