unit gfxrender;

interface uses
  Windows, SysUtils, Classes, Graphics, Model3D, dglOpenGL,
  MilitiaAdventurer, Dialogs;

var
  glRC: THandle;
  AspectRatio: Single;
  CameraLook: record x, y: Single end = (x: -140; y: -90);
  Farmhouse, Oldhouse, Wagen: TModel3D;

procedure Render;
procedure CameraMoved;
procedure RenderScene;

implementation uses
  Math, Khrono, TextureManager, Shadows, shaders, vectors, unit1, Trees;

type
  TTextureEnum = (texGrass, texSun, texMoon, texWheat);

var
  Wheat: array[-3999..3999, 0..5] of Single;

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
  LoadIdentity;
  Frustum(1, AspectRatio, 0.1);
  glRotatef(CameraLook.y, 1, 0, 0);
  glRotatef(-CameraLook.x, 0, 0, 1);
  glTranslatef(0, 0, -1.8);
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

procedure Init;
var
  i: Integer;
begin
  TThread.CreateAnonymousThread(LoadModels).Start;
    glPointSize(7);
    GenerateRenderPrograms;
  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  glEnable(GL_DEPTH_TEST);
  for i := -3999 to 3999 do begin
      Wheat[i, 0] := random*9;
      Wheat[i, 1] := random*9;
      Wheat[i, 2] := 0;
      Wheat[i, 3] := Wheat[i, 0] + random-0.5;
      Wheat[i, 4] := Wheat[i, 1] + random-0.5;
      Wheat[i, 5] := 0.7;
  end;

//  glEnable(GL_NORMALIZE);
//  glEnable(GL_COLOR_MATERIAL);
//  glEnable(GL_POINT_SMOOTH);
//  glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, 0);
//  glEnableClientState(GL_VERTEX_ARRAY);
//  glEnableClientState(GL_NORMAL_ARRAY);
//  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
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
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 16, @vdata);//Vertices.Data[0]);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 16, @vdata[2]);//Vertices.Data[0]);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
  end;
begin
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
  RenderSunMoon;
  glPopMatrix;
  glDisable(GL_BLEND);
  glDepthMask(true);
  glBlendFunc (GL_DST_ALPHA, GL_ONE_MINUS_DST_ALPHA);
end;

procedure RenderScene;
const
  vdata: array[0..15] of Single = (
    -999, -999, -333, -333,
    -999, +999, -333, +333,
    +999, -999, +333, -333,
    +999, +999, +333, +333
  );
begin
  glPushMatrix;
  glTranslatef(9, -5, 0);
  glScalef(0.3);
  Farmhouse.Draw;
  glPopMatrix;
//
//  glEnable(GL_LIGHTING);
//  glEnable( GL_TEXTURE_2D );
  BindTexture(texGrass);
//  glEnable (GL_BLEND);
//  glColor3f(1, 1, 1);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 16, @vdata);//Vertices.Data[0]);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 16, @vdata[2]);//Vertices.Data[0]);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);

  glPushMatrix;
  glTranslatef(4, -1, 0);
  Wagen.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(8, 8, 0);
  glScalef(3, 3, 3);
  Oldhouse.Draw;
  glPopMatrix;

  Peasant.Draw;
  TTrees.Draw;

//  glDisable( GL_TEXTURE_2D);

//  glNormal3f(0, 0, 1);
//  glColor3f(1, 1, 0.5);
  BindTexture(texWheat);
  glPushMatrix;
  glTranslatef(-6, -12, 0);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, @Wheat);//Vertices.Data[0]);
  glDrawArrays(GL_LINES, 0, SizeOf(Wheat) div 12);
//  glBegin(GL_LINES);
//  RandSeed := 0;
//  glEnd;
  glPopMatrix;
//  glColor3f(1, 1, 1);
end;

procedure Render;
const {$J+}
  N = 100;
  InitDone: Boolean = false;
begin
  if not InitDone then begin
    InitDone := true;
    Init;
  end;
  glClearColor(skyColor*0.3, skyColor*0.3, skyColor, 1);
  glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);

  CameraMoved;
//  glEnable( GL_TEXTURE_2D );
  RenderSky;

//  glEnable(GL_LIGHTING);
  RenderScene;
//  glDisable(GL_LIGHTING);
end;

end.
