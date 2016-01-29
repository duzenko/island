unit Khrono;

interface uses
  SysUtils, Classes, dglOpengl;

const
  DayAmbient = 0.4;
  NightAmbient = 0.20;
  SunLight: TGLVectorf3 = (1-DayAmbient, 1-DayAmbient, 0.9-DayAmbient);
  MoonLight: TGLVectorf3 = (0.25-NightAmbient, 0.25-NightAmbient, 0.35-NightAmbient);
var
  Time: TDateTime = 0.46;
  SunPos, MoonPos: record x, y, z, w: Single end;
  SunColor: TGLVectorf4 = (1, 1, 0.9, 1);
  MoonColor: TGLVectorf4 = (0.3, 0.3, 0.4, 1);
  AdjustedSunLight, AdjustedMoonLight, AmbientLight: TGLVectorf3;
  SkyColor: Single;
  ShadowMatrix: array[0..3, 0..3] of Single = (
    (1, 0, 0, 0),
    (0, 0, 0, 0),
    (0, 0, 1, 0),
    (1, 0, 0, 1)
  );
  SunMatrix: array[0..3, 0..3] of Single = (
    (1, 0, 0, 0),
    (0, 0.6, 0, 0),
    (0, 0, 1, 0),
    (1, 0, 0, 1)
  );
  SunAngle: Single;
  Paused: Boolean = true;

procedure Init;
procedure UISync;

implementation uses
  Math, vectors, gfxrender;

const
  SunBreak = 0.2;

procedure UISync;
begin
//  if SunPos.z > -SunBreak then
//    glEnable(GL_LIGHT0)
//  else
//    glDisable(GL_LIGHT0);
//  if MoonPos.z > -SunBreak then
//    glEnable(GL_LIGHT1)
//  else
//    glDisable(GL_LIGHT1);
  if SunPos.z > 0 then begin
//    glLightfv(GL_LIGHT0, GL_POSITION, @SunPos);
//    glLightfv(GL_LIGHT0, GL_DIFFUSE, @AdjustedSunLight);
  end else begin
//    glLightfv(GL_LIGHT1, GL_POSITION, @MoonPos);
//    glLightfv(GL_LIGHT1, GL_DIFFUSE, @AdjustedMoonLight);
  end;
//  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @AmbientLight);
end;

procedure TimeProc;
var
  s, c: Single;
  SunEclipse: Single;
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
    SunMatrix[0][0] := -c*0.8;
    SunMatrix[2][0] := -s;
    SunMatrix[0][2] := s;
    SunMatrix[2][2] := -c*0.8;
    SkyColor := SunPos.z+0.4;
    SunEclipse := (SunPos.z+SunBreak)/2/SunBreak;
    if SunPos.z > -SunBreak then begin
      if SunPos.z > SunBreak then
        AdjustedSunLight := SunLight
      else
        AdjustedSunLight := VectorMul(SunLight, Power(SunEclipse, 3));
    end;
    if MoonPos.z > -SunBreak then begin
      if MoonPos.z > SunBreak then
        AdjustedMoonLight := MoonLight
      else
        AdjustedMoonLight := VectorMul(MoonLight, Power(1-SunEclipse, 3));
    end;
    if SunPos.z > SunBreak then
      AmbientLight := VectorNew(DayAmbient)
    else
      if MoonPos.z > SunBreak then
        AmbientLight := VectorNew(NightAmbient)
      else
        AmbientLight := VectorNew(SunEclipse*DayAmbient + (1-SunEclipse)*NightAmbient);
    if SunPos.z > 0 then begin
      ShadowMatrix[1][0] := -SunPos.x / SunPos.y;
      ShadowMatrix[1][2] := SunPos.z / SunPos.y;
    end else begin
      ShadowMatrix[1][0] := -MoonPos.x / MoonPos.y;
      ShadowMatrix[1][2] := MoonPos.z / MoonPos.y;
    end;
    Sleep(1);
  end;
end;

procedure init;
begin
  TThread.CreateAnonymousThread(TimeProc).Suspended := false;
end;

end.
