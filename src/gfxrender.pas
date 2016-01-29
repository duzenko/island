unit gfxrender;

interface uses
  Windows, SysUtils, Classes, Graphics, Model3D, dglOpenGL,
  MilitiaAdventurer, Dialogs;

var
  glRC: THandle;
  AspectRatio: Single;
  CameraLook: record x, y: Single end = (x: -70; y: -0);
  Farmhouse, Oldhouse, Wagen: TModel3D;

procedure Render;
procedure CameraMoved;
procedure RenderScene;

implementation uses
  Math, Khrono, TextureManager, Shadows, shaders, vectors, unit1;

type
  TTextureEnum = (texGrass, texSun, texMoon);

procedure BindTexture(texEnum: TTextureEnum);
const
  TexFiles: array[TTextureEnum] of string = (
    '..\textures\green-grass-texture.jpg',
    '..\textures\sun.jpg',
    '..\textures\a2fdc0061b03.png'
  );
begin
  TTextureManager.SwitchTo(TexFiles[texEnum]);
end;

procedure CameraMoved;
begin
//  glMatrixMode(GL_PROJECTION);
  LoadIdentity;
  Frustum(-AspectRatio, AspectRatio, -1, 1, 1, 1000);
//  glMatrixMode(GL_MODELVIEW);
//  LoadIdentity;
//  glRotatef(CameraLook.y, 1, 0, 0);
//  glRotatef(-CameraLook.x, 0, 0, 1);
  glTranslatef(0, 0, 2);
//  glTranslatef(0, 0, 1.7e-0);
end;

procedure LoadModels;
const {$J+}
  No: Integer = 0;
begin
  Farmhouse := TModel3D.Create('..\models\farmhouse\Farmhouse OBJ.obj');
  Wagen := TModel3D.Create('..\models\wagen\untitled.obj');
//  Oldhouse := TModel3D.Create('..\models\oldhouse\untitled.obj');
  Peasant := TMilitiaAdventurer.Create();
end;

procedure Init;
begin
  TThread.CreateAnonymousThread(LoadModels).Start;
//  glEnable(GL_NORMALIZE);
  glEnable(GL_DEPTH_TEST);
//  glEnable(GL_COLOR_MATERIAL);
//  glEnable(GL_POINT_SMOOTH);
//  glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, 0);
//  glEnableClientState(GL_VERTEX_ARRAY);
//  glEnableClientState(GL_NORMAL_ARRAY);
//  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
end;

procedure RenderSky;
  procedure RenderSunMoon;
  begin
{  glBegin(GL_TRIANGLE_FAN);
    glTexCoord2i(0, 0);
    glVertex2i(-1, -1);
    glTexCoord2i(0, +1);
    glVertex2i(-1, +1);
    glTexCoord2i(+1, +1);
    glVertex2i(+1, +1);
    glTexCoord2i(+1, 0);
    glVertex2i(+1, -1);
  glEnd;}
  end;
begin
//  glColor3f(1, 1, 1);
  glBlendFunc (GL_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR);
  glDepthMask(false);
  glEnable(GL_BLEND);
  glPushMatrix;
  glTranslatef(0, 0, 1.7);
//  glMultMatrixf(@SunMatrix);
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
var
  i: Integer;
  v: TGLVector3f;
const
  grass: array[0..7] of Single = (
    -999, -999,
    -999, +999,
    +999, -999,
    +999, +999
  );
begin
  glPushMatrix;
  glTranslatef(9, -5, 0);
  glScalef(0.3, 0.3, 0.3);
  glRotatef(90, 1, 0, 0);
  Farmhouse.Draw;

//  glDisable( GL_TEXTURE_2D);
//  glDisable(GL_LIGHTING);
//  glColor4f(0, 0, 0, (1-Abs(SunPos.z))/2+0.5);
//
//  glDepthMask(false);
//  glMultMatrixf(@ShadowMatrix);
//  Farmhouse.Draw;
//  glDepthMask(true);
//
  glPopMatrix;
//
//  glEnable(GL_LIGHTING);
//  glEnable( GL_TEXTURE_2D );
  BindTexture(texGrass);
//  glEnable (GL_BLEND);
//  glColor3f(1, 1, 1);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, @grass);//Vertices.Data[0]);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
//  glPointSize(1);
//  glNormal3f(0, 0, 1);
//  glBegin(GL_TRIANGLE_STRIP);
//      glTexCoord2i(0, 0);
//      glVertex2i(-999, -999);
//      glTexCoord2i(0, 333);
//      glVertex2i(-999, 999);
//      glTexCoord2i(333, 0);
//      glVertex2i(999, -999);
//      glTexCoord2i(333, 333);
//      glVertex2i(999, 999);
//  glEnd;
//  glDisable (GL_BLEND);

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
//  TTrees.Draw;

  glDisable( GL_TEXTURE_2D);

//  glNormal3f(0, 0, 1);
//  glColor3f(1, 1, 0.5);
//  glPushMatrix;
//  glTranslatef(-6, -12, 0);
//  glBegin(GL_LINES);
//  RandSeed := 0;
//  for i := -3999 to 3999 do begin
//      v[0] := random*9;
//      v[1] := random*9;
//      v[2] := 0;
//      glVertex3fv(@v);
//      v[0] := v[0] + random-0.5;
//      v[1] := v[1] + random-0.5;
//      v[2] := 0.7;
//      glVertex3fv(@v);
//  end;
//  glEnd;
//  glPopMatrix;
//  glColor3f(1, 1, 1);
end;

procedure Render;
const {$J+}
  N = 100;
  InitDone: Boolean = false;
const
  test: array[0..8] of Single = (
    -1.0, -1.0, 0.0,
    1.0, -1.0, 0.0,
    0.0,  1.0, 0.0
  );
var
  test2: array[0..N, 0..2] of Single;
  i: Integer;
begin
  if not InitDone then begin
    InitDone := true;
//    Init;
    glPointSize(7);
    GenerateRenderPrograms;
    glEnableVertexAttribArray(0);
  end;
  glClearColor(skyColor*0.3, skyColor*0.3, skyColor, 1);
  glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);

  LoadIdentity;
  Frustum(-1, 1, -1, 1, 0, 9);
//  glRotatef((GetTickCount mod 2000)*0.18, 0, 0, 1);
  dbg2 := 2*Sin(Now*3e4);
  glTranslatef(0, 0, dbg2);
//  glTranslatef(0.5, 0, 0);
//  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, @test);//Vertices.Data[0]);
//  glDrawArrays(GL_TRIANGLES, 0, 3);

//  CameraMoved;
//  DrawShadow;
//
//  CameraMoved;
  for i := 0 to N do begin
    test2[i][0] := i*0.1;
    test2[i][1] := 0;
    test2[i][2] := 0;
  end;
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, @test2);//Vertices.Data[0]);
  glDrawArrays(GL_POINTS, 0, N+1);
  for i := 0 to N do begin
    test2[i][1] := i*0.1;
    test2[i][0] := 0;
    test2[i][2] := 0;
  end;
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, @test2);//Vertices.Data[0]);
  glDrawArrays(GL_POINTS, 0, N+1);
//  glEnable( GL_TEXTURE_2D );
//  RenderSky;

//  glEnable(GL_LIGHTING);
//  RenderScene;
//  glDisable(GL_LIGHTING);
end;

end.
