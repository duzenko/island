unit gfxrender;

interface uses
  Windows, SysUtils, Classes, Graphics, Model3D, dglOpenGL,
  MilitiaAdventurer, Dialogs, Forms, vectors;

var
  glRC: THandle;
  AspectRatio: Single;
  CameraLook: record
    case byte of
    0:
      (x, y, z, ax, ay: Single);
//    1:
//      (v: TVector);
  end = (z: 1.8e0; ax: -100; ay: -90);
  Farmhouse, Oldhouse, Wagen: TModel3D;

procedure Render;
//procedure CameraMoved;
procedure RenderScene;

implementation uses
  Math, Khrono, TextureManager, Shadows, shaders, unit1, Trees, Terrain;

type
  TTextureEnum = (texSun, texMoon, texWheat);

var
  Wheat: array of array[0..15] of Single;

procedure BindTexture(texEnum: TTextureEnum);
const
  TexFiles: array[TTextureEnum] of string = (
    '..\textures\sun.jpg',
    '..\textures\a2fdc0061b03.png',
    '..\textures\wheat.bmp'
  );
begin
  TTextureManager.SwitchTo(TexFiles[texEnum]);
end;

procedure LoadModels;
const {$J+}
  No: Integer = 0;
begin
  Farmhouse := TModel3D.Create('..\models\farmhouse\untitled.obj');
  Wagen := TModel3D.Create('..\models\wagen\untitled.obj');
//  Oldhouse := TModel3D.Create('..\models\oldhouse\untitled.obj');
  Peasant := TMilitiaAdventurer.Create();
end;

function Init: Boolean;
var
  i: Integer;
begin
  TThread.CreateAnonymousThread(LoadModels).Start;
  glPointSize(7);
  prgTerrain := LoadGpuProgram('terrain');
  prgObjects := LoadGpuProgram('objects');
  result := prgObjects*prgTerrain <> 0;

  glEnable(GL_DEPTH_TEST);
end;

procedure RenderSky;
  procedure RenderSunMoon;
  const
    vdata: array[0..15] of Single = (
      -1, -1, -0, -0,
      -1, +1, -0, +1,
      +1, -1, +1, -0,
      +1, +1, +1, +1
    );
  begin
    SetShaderPointer('vpos', 2, 16, @vdata[0]);
    SetShaderPointer('vtex', 2, 16, @vdata[2]);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
  end;
begin
  SetShaderFloat('lightOverride', 1);
  glDepthMask(false);
  glBlendFunc (GL_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR);
  glEnable(GL_BLEND);
  glPushMatrix;
//  glTranslatef(0, 0, 1.7);
  glRotatef(Time*360, 0, 1, 0);
  glRotatef(-Cos(SunAngle*2*pi)*30, 1, 0, 0);
  glTranslatef(0, 0, 10);
  BindTexture(texMoon);
  RenderSunMoon;
  glTranslatef(0, 0, -15);
  BindTexture(texSun);
  glCullFace(GL_FRONT);
  RenderSunMoon;
  glCullFace(GL_BACK);
  glPopMatrix;
  glDisable(GL_BLEND);
  glDepthMask(true);
  glBlendFunc (GL_DST_ALPHA, GL_ONE_MINUS_DST_ALPHA);
  SetShaderFloat('lightOverride', 0);
end;

procedure CameraMoved;
begin
  MatrixMode(false);
  LoadIdentity;
  Frustum(1, AspectRatio, 0.1);
  glRotatef(CameraLook.ay, 1, 0, 0);
  glRotatef(-CameraLook.ax, 0, 0, 1);
  MatrixMode(true);
  LoadIdentity;
  RenderSky;
  MatrixMode(false);
  glTranslatef(-CameraLook.x, -CameraLook.y, -CameraLook.z - GetHeight(CameraLook.x, CameraLook.y));
  MatrixMode(true);
end;

procedure RenderScene;
var
  i: Integer;
begin
  glPushMatrix;
  TranslateOnTerrain(9, -5);
  glScalef(0.3);
  Farmhouse.Draw;
  glPopMatrix;

  glPushMatrix;
  TranslateOnTerrain(5, 0);
  Wagen.Draw;
  glPopMatrix;

  glPushMatrix;
  TranslateOnTerrain(8, 8);
  glScalef(3, 3, 3);
  Oldhouse.Draw;
  glPopMatrix;

  Peasant.Draw;
  TTrees.Draw;

  if (Wheat = nil) and (TerrainSize.cy > 0) then begin  
    SetLength(Wheat, 8000);
  for i := 0 to High(Wheat) do begin
      Wheat[i, 0] := -6+random*9;
      Wheat[i, 1] := -12+random*9;
      Wheat[i, 2] := GetHeight(Wheat[i, 0], Wheat[i, 1]);
      Wheat[i, 7] := 1;
      Wheat[i, 8] := Wheat[i, 0] + random-0.5;
      Wheat[i, 9] := Wheat[i, 1] + random-0.5;
      Wheat[i, 10] := Wheat[i, 2] + 0.7;
      Wheat[i, 15] := 1;
  end;
  end;
  if Wheat <> nil then begin  
  BindTexture(texWheat);
//  glPushMatrix;
//  glTranslatef(-6, -12, 0);
  SetShaderPointer('vpos', 3, 32, @Wheat[0]);
  SetShaderPointer('vtex', 2, 32, @Wheat[0][3]);
  SetShaderPointer('vnorm', 3, 32, @Wheat[0][5]);
  glDrawArrays(GL_LINES, 0, Length(Wheat)*2);
//  glPopMatrix;
  end;
end;

procedure Render;
const {$J+}
  N = 100;
  InitDone: Boolean = false;
begin
  if not InitDone then begin
    InitDone := true;
    if not Init then
      Exit;
  end;
  if Application.Terminated then
    Exit;
  glClearColor(skyColor*0.3, skyColor*0.3, skyColor, 1);
  glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);

  TTextureManager.Disabled := true;
  DrawShadow;
  TTextureManager.Disabled := false;

  CameraMoved;
  RenderTerrain;

  RenderScene;
end;

end.
