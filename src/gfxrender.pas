unit gfxrender;

interface uses
  Windows, SysUtils, Classes, Graphics, Model3D, dglOpenGL,
  MilitiaAdventurer, Dialogs, Forms;

var
  glRC: THandle;
  AspectRatio: Single;
  CameraLook: record x, y: Single end = (x: -130; y: -90);
  Farmhouse, Oldhouse, Wagen: TModel3D;

procedure Render;
procedure CameraMoved;
procedure RenderScene;

implementation uses
  Math, Khrono, TextureManager, Shadows, shaders, vectors, unit1, Trees;

type
  TTextureEnum = (texGrass, texSun, texMoon, texWheat);

var
  Wheat: array[-3999..3999, 0..15] of Single;

procedure BindTexture(texEnum: TTextureEnum);
const
  TexFiles: array[TTextureEnum] of string = (
    '..\textures\green-grass-texture.jpg',
    '..\textures\sun.jpg',
    '..\textures\a2fdc0061b03.png',
    '..\textures\wheat.bmp'
  );
begin
  TTextureManager.SwitchTo(TexFiles[texEnum]);
end;

procedure CameraMoved;
begin
  MatrixMode(false);
  LoadIdentity;
  Frustum(1, AspectRatio, 0.1);
  glRotatef(CameraLook.y, 1, 0, 0);
  glRotatef(-CameraLook.x, 0, 0, 1);
  glTranslatef(1, 0, -1.8);
  MatrixMode(true);
  LoadIdentity;
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
  result := GenerateRenderPrograms <> 0;
  glEnableVertexAttribArray(1);
//  glEnableVertexAttribArray(2);
  glEnable(GL_DEPTH_TEST);
  for i := -3999 to 3999 do begin
      Wheat[i, 0] := random*9;
      Wheat[i, 1] := random*9;
      Wheat[i, 7] := 1;
      Wheat[i, 8] := Wheat[i, 0] + random-0.5;
      Wheat[i, 9] := Wheat[i, 1] + random-0.5;
      Wheat[i, 10] := 0.7;
      Wheat[i, 15] := 1;
  end;
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
  glBlendFunc (GL_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR);
  glDepthMask(false);
  glEnable(GL_BLEND);
  glPushMatrix;
  glTranslatef(0, 0, 1.7);
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

procedure RenderScene;
const
  vdata: array[0..31] of Single = (
    -999, -999, 0, -333, -333, 0, 0, 1,
    +999, -999, 0, +333, -333, 0, 0, 1,
    -999, +999, 0, -333, +333, 0, 0, 1,
    +999, +999, 0, +333, +333, 0, 0, 1
  );
begin
  BindTexture(texGrass);
  SetShaderPointer('vpos', 3, 32, @vdata[0]);
  SetShaderPointer('vnorm', 3, 32, @vdata[5]);
  SetShaderPointer('vtex', 2, 32, @vdata[3]);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);

//  Exit;
  glPushMatrix;
  glTranslatef(9, -5, 0);
  glScalef(0.3);
  Farmhouse.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(4, 0, 0);
  Wagen.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(8, 8, 0);
  glScalef(3, 3, 3);
  Oldhouse.Draw;
  glPopMatrix;

  Peasant.Draw;
  TTrees.Draw;

  BindTexture(texWheat);
  glPushMatrix;
  glTranslatef(-6, -12, 0);
  SetShaderPointer('vpos', 3, 32, @Wheat);
  SetShaderPointer('vtex', 2, 32, @Wheat[3]);
  SetShaderPointer('vnorm', 3, 32, @Wheat[5]);
  glDrawArrays(GL_LINES, 0, SizeOf(Wheat) div 64);
  glPopMatrix;
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

  DrawShadow;

  CameraMoved;
  RenderSky;
  RenderScene;
end;

end.
