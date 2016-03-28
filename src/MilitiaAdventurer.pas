unit MilitiaAdventurer;

interface uses
  Windows, SysUtils, Classes, Model3D, dglOpengl, Vectors, Forms;

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
//    class procedure constructorCreate;
    constructor Create;
    destructor Destroy; override;
    procedure Draw;
    procedure AI;
  class var
    Model3d: TModel3D;
  end;

var
  Peasant: TMilitiaAdventurer;

implementation uses
  DateUtils, Khrono, Obstacles, shaders, Terrain;

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
  RoutineTime: array[TRoutine] of TDateTime = (0.2, 0.32, 0.44, 0.56, 0.74);
  RoutineLoc: array[TRoutine] of record Loc, Look: TGLVectorf3 end = (
    (Loc: (5, -2, 0); Look: (-1, 0, 0)),
    (Loc: (4, -1, 0)),
    (Loc: (2, -5, 0)),
    (Loc: (13, -1.8, 0); Look: (1, 0, 0)),
    (Loc: (8, -2, 0.5); Look: (0, -1, 0))
  );
  RoutineAnimations: array[TRoutine] of TAnimation = (
    Cheering, Attack1, UseShovel, HoldScythe, Idle
  );

type
  TAIThread = class(TThread)
    FOwner: TMilitiaAdventurer;
  protected
    procedure Execute; override;
  end;

{ TMilitiaAdventurer }

procedure TMilitiaAdventurer.AI;
var
  Time: TDateTime;

  procedure GetRoutine;
  var
    r: TRoutine;
  begin
    Time := TimeOf(Khrono.Time);
    Routine := High(TRoutine);
    for r := Low(TRoutine) to High(TRoutine) do
      if Time > RoutineTime[r] then
        Routine := r;
    Visible := Abs(Time - 0.5) < 0.3;
  end;

var
  Dir: TVector;
  DestDist: Single;
const
  cSpeed = 2e-3;
begin
  GetRoutine;
  Location := RoutineLoc[Routine].Loc;
  Look := RoutineLoc[Routine].Look;
  while (Application <> nil) and not Application.Terminated do begin
    GetRoutine;
    if not Paused then begin
      Dir := RoutineLoc[Routine].Loc - Location;
      DestDist := Dir;
      if Dir.Normalise then
        TObstacles.FixDir(Dir, Location);
      Location := Location + Dir*cSpeed;
//      if Single(TVector(RoutineLoc[Routine].Look)) = 0 then
        Look := Look + Dir*3e-3;
//      else
//        Look := Look + TVector(RoutineLoc[Routine].Look)*3e-3;
      Look.Normalise;
//      Look := RoutineLoc[Routine].Look;
      if DestDist > 0.5 then
        Animation := Walk
      else
        Animation := RoutineAnimations[Routine];
    end;
    Windows.Sleep(1);
  end;
end;

constructor TMilitiaAdventurer.Create;
begin
  Location := RoutineLoc[Sleep].Loc;
  AIThread := TAIThread.Create(true);
  TAIThread(AIThread).FOwner := Self;
  AIThread.FreeOnTerminate := true;
  AIThread.Resume
end;

destructor TMilitiaAdventurer.Destroy;
begin
  AIThread.Terminate;
  inherited;
end;

procedure TMilitiaAdventurer.Draw;
var
  frame: Integer;
  M: TMatrix;
begin
  if Self = nil then
    Exit;
  if not Visible then
    Exit;
  with AnimationFrames[Animation] do
    frame := gettickcount div 40 mod (EndNo - StartNo) + StartNo;
  Model3d.TurnMeshes(Cardinal(cAnimationMeshes[Animation]));
  glPushMatrix;
  glTranslatef(Location.x, Location.y, Location.z+GetHeight(Location.x, Location.y));
  if Look.Normalise2 then begin
    M.CalcTransformationMatrix2(tvector.Create(0, -1, 0), Look);
    glMultMatrixf(M);
  end;
  Model3d.Draw(frame - 1);
  glPopMatrix;
end;

{ TAIThread }

procedure TAIThread.Execute;
begin
  inherited;
  FOwner.AI;
end;

initialization
  TMilitiaAdventurer.Model3D := TModel3D.Create('..\models\peasant\model.obj');

finalization
  FreeAndNil(Peasant);

end.
