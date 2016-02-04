unit Khrono;

interface uses
  SysUtils, Classes, dglOpengl, vectors;

var
  Time: TDateTime = 0.26;
  SunPos, MoonPos: record x, y, z, w: Single end;
  SkyColor: Single;
  ShadowAngles: array[0..1] of Single;
  SunAngle: Single;
  Paused: Boolean = false;

procedure Init;
procedure UISync;

implementation uses
  Math, gfxrender, unit1, shaders;

procedure UISync;
begin
  SetShaderVec3('sunPos', @SunPos);
end;

procedure TimeProc;
var
  s, c: Single;
begin
  while true do begin
    if not Paused then
      Time := Time + 2e-5; //Now*3000;//6000;
    SunAngle := Frac(Time);
    SinCos(SunAngle*2*Pi, s, c);
    MoonPos.x := s;
    MoonPos.y := c*0.6;
    MoonPos.z := c*0.8;
    SunPos.x := -MoonPos.x;
    SunPos.y := -MoonPos.y;
    SunPos.z := -MoonPos.z;

    ShadowAngles[0] := ArcCos(SunPos.z)/Pi*180;
    if SunPos.z < 0 then
      ShadowAngles[0] := ShadowAngles[0] - 180;
    ShadowAngles[1] := 180-SunAngle*360;

    SkyColor := SunPos.z+0.4;
    Sleep(1);
  end;
end;

procedure init;
begin
  TThread.CreateAnonymousThread(TimeProc).Suspended := false;
end;

end.
