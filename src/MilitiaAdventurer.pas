unit MilitiaAdventurer;

interface uses
  Windows, Model3D;

type
  TMilitiaAdventurer = class
  type
    TMeshes = (Body, Sheat, Purse, Shield, Sword1, LegBag,
      LegKnife, Bandolera, ThrowingKnife1, ThrowingKnife2,
      Scroll1, ThrowingKnife3, Scroll2, Potion, PitchFork,
      Shovel, Scythe, Sword2);
    TAnimations = (Idle, Attack1, Attack2, ShieldBlock, Cheering,
      Death1, Death2, UsePitchfork, UseShovel, HoldScythe, Walk);
  class var
    Model3d: T3DModel;
    class constructor Create;
    procedure Draw;
  end;

implementation

  const
    TAnimationFrames: array[TMilitiaAdventurer.TAnimations] of record StartNo, EndNo: Integer end = (
      (StartNo: 1; EndNo: 210),
      (StartNo: 211; EndNo: 250),
      (StartNo: 251; EndNo: 270),
      (StartNo: 271; EndNo: 285),
      (StartNo: 286; EndNo: 310),
      (StartNo: 311; EndNo: 333),
      (StartNo: 334; EndNo: 345),
      (StartNo: 346; EndNo: 375),
      (StartNo: 376; EndNo: 390),
      (StartNo: 391; EndNo: 391),
      (StartNo: 395; EndNo: 415)
    );

{ TMilitiaAdventurer }

class constructor TMilitiaAdventurer.Create;
begin
  Model3D := T3DModel.Create('D:\temp\peasant\test.obj');
end;

procedure TMilitiaAdventurer.Draw;
begin
  Model3d.Draw(gettickcount div 40 mod 415);
end;

end.
