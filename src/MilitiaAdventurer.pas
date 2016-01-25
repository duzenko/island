unit MilitiaAdventurer;

interface uses
  Windows, Classes, Model3D, dglOpengl, Vectors;

type
  TRoutine = (Hail, Drill, Field, Reflex, Sleep);
  TAnimation = (Idle, Attack1, Attack2, ShieldBlock, Cheering,
    Death1, Death2, UsePitchfork, UseShovel, HoldScythe, Walk);

  TMilitiaAdventurer = class
    AIThread: TThread;
    Visible: Boolean;
    Location, Look: TVector;
    Routine: TRoutine;
    Animation: TAnimation;
    class constructor Create;
    constructor Create;
    destructor Destroy; override;
    procedure Draw;
    procedure AI;
  class var
    Model3d: T3DModel;
  end;

implementation uses
  DateUtils, Khrono;

type
  TMesh = (Body, Sheat, Purse, Shield, Sword1, LegBag,
    LegKnife, Bandolera, ThrowingKnife1, ThrowingKnife2,
    Scroll1, ThrowingKnife3, Scroll2, Potion, PitchFork,
    Shovel, Scythe, Sword2);
  TAnimationFrames = record
    StartNo, EndNo: Cardinal
  end;

const
  AnimationFrames: array[TAnimation] of TAnimationFrames = (
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
  cDefaultMeshes = [Body, Sheat, Purse, LegBag,
    LegKnife, Bandolera, ThrowingKnife1, ThrowingKnife2,
    Scroll1, ThrowingKnife3, Scroll2, Potion];
  cAnimationMeshes: array[TAnimation] of set of TMesh = (
    cDefaultMeshes + [Sword1, Shield],
    cDefaultMeshes + [Sword1, Shield],
    cDefaultMeshes + [Sword1, Shield],
    cDefaultMeshes + [Sword1, Shield],
    cDefaultMeshes + [],
    cDefaultMeshes + [Sword1, Shield],
    cDefaultMeshes + [Sword1, Shield],
    cDefaultMeshes + [PitchFork],
    cDefaultMeshes + [Shovel],
    cDefaultMeshes + [Scythe],
    cDefaultMeshes + []
  );
  RoutineTime: array[TRoutine] of TDateTime = (0.2, 0.35, 0.5, 0.65, 0.8);
  RoutineLoc: array[TRoutine] of record Loc, Look: TGLVectorf3; FastTravel: Boolean end = (
    (Loc: (5, -3, 0); Look: (-1, 0, 0)),
    (Loc: (3, -3, 0); Look: (-1, 0, 0)),
    (Loc: (2, -6, 0); Look: (0, -1, 0)),
    (Loc: (12, -3, 0); Look: (1, 0, 0)),
    (Loc: (8, -3, 0.5); Look: (0, -1, 0); FastTravel: True)
  );
  RoutineAnimations: array[TRoutine] of TAnimation = (
    Cheering, Attack1, UseShovel, HoldScythe, Idle
  );

{ TMilitiaAdventurer }

procedure TMilitiaAdventurer.AI;
var
  Time: TDateTime;
  r: TRoutine;
  Dir: TVector;
  DestDist: Single;
const
  cSpeed = 1e-3;
begin
  while not TThread.CheckTerminated do begin
    Time := TimeOf(Khrono.Time);
    Visible := Abs(Time - 0.5) < 0.3;
    Routine := High(TRoutine);
    for r := Low(TRoutine) to High(TRoutine) do
      if Time > RoutineTime[r] then
        Routine := r;
    if RoutineLoc[Routine].FastTravel then begin
      Location := RoutineLoc[Routine].Loc;
      Look := RoutineLoc[Routine].Look;
    end else begin
      Dir := RoutineLoc[Routine].Loc - Location;
      DestDist := Dir;
      Dir.Normalise;
      Location := Location + Dir*cSpeed;
//      Look := Look + RoutineLoc[Routine].Look;
//      Look.Normalise;
      Look := RoutineLoc[Routine].Look;
      if DestDist > 0.5 then
        Animation := Walk
      else
        Animation := RoutineAnimations[Routine];
    end;
    Windows.Sleep(1);
  end;
end;

class constructor TMilitiaAdventurer.Create;
begin
  Model3D := T3DModel.Create('..\models\peasant\model.obj');
end;

constructor TMilitiaAdventurer.Create;
begin
  AIThread := TThread.CreateAnonymousThread(AI);
  AIThread.Start;
end;

destructor TMilitiaAdventurer.Destroy;
begin
  AIThread.Free;
  inherited;
end;

procedure TMilitiaAdventurer.Draw;
var
  frame: Integer;
  M: TMatrix;
begin
  if not Visible then
    Exit;
  with AnimationFrames[Animation] do
    frame := gettickcount div 40 mod (EndNo - StartNo) + StartNo;
//  if T3DModel.DebugDraw then
//    frame := T3DModel.DebugIndex+1;
  Model3d.TurnMeshes(Cardinal(cAnimationMeshes[Animation]));
  glPushMatrix;
  glTranslatef(Location.x, Location.y, Location.z);
  M.CalcTransformationMatrix(tvector.Create(0, -1, 0), Look);
  glMultMatrixf(@M);
//  glRotatef(-180, 0, 0, 1);
  Model3d.Draw(frame - 1);
  glPopMatrix;
end;

end.
