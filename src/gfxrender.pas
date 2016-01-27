unit gfxrender;

interface uses
  Windows, SysUtils, Classes, Graphics, Model3D, Model3DBlend, dglOpenGL,
  MilitiaAdventurer, Trees;

var
  glRC: THandle;
  AspectRatio: Single;
  CameraLook: record x, y: Single end = (x: -140; y: -90);
  House: TModel3D;

procedure Render;
procedure CameraMoved;

implementation uses
  Math, Khrono, TextureManager;

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
  glLoadIdentity;
  glRotatef(CameraLook.y, 1, 0, 0);
  glRotatef(-CameraLook.x, 0, 0, 1);
  glTranslatef(0, 0, -1.7);
end;

procedure LoadModels;
const {$J+}
  No: Integer = 0;
begin
  House := TModel3D.Create('..\models\farmhouse\Farmhouse OBJ.obj');
  Peasant := TMilitiaAdventurer.Create();
end;

procedure Init;
begin
  TThread.CreateAnonymousThread(LoadModels).Start;
  glEnable(GL_NORMALIZE);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_COLOR_MATERIAL);
  glEnable(GL_POINT_SMOOTH);
  glMatrixMode(GL_PROJECTION);
  glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, 0);
  glLoadIdentity;
  glFrustum(-AspectRatio, AspectRatio, -1, 1, 1, 1000);
  glMatrixMode(GL_MODELVIEW);
end;

procedure RenderSky;
  procedure RenderSunMoon;
  begin
  glBegin(GL_TRIANGLE_FAN);
    glTexCoord2i(0, 0);
    glVertex2i(-1, -1);
    glTexCoord2i(0, +1);
    glVertex2i(-1, +1);
    glTexCoord2i(+1, +1);
    glVertex2i(+1, +1);
    glTexCoord2i(+1, 0);
    glVertex2i(+1, -1);
  glEnd;
  end;
begin
  glColor3f(1, 1, 1);
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

procedure Render;
var
  i: Integer;
  v: TGLVector3f;
const {$J+}
  N = 1;
  InitDone: Boolean = false;
begin
  if not InitDone then begin
    Init;
    CameraMoved;
    InitDone := true;
  end;
  glClearColor(skyColor*0.3, skyColor*0.3, skyColor, 1);
  glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);

  glEnable( GL_TEXTURE_2D );
  RenderSky;

  glEnable(GL_LIGHTING);
//  BindTexture(texHouse);

  glPushMatrix;
  glTranslatef(9, -5, 0);
  glScalef(0.3, 0.3, 0.3);
  glRotatef(90, 1, 0, 0);
  House.Draw;

  glDisable( GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  glColor4f(0, 0, 0, (1-Abs(SunPos.z))/2+0.5);

  glDepthMask(false);
  glMultMatrixf(@ShadowMatrix);
  House.Draw;
  glDepthMask(true);

  glPopMatrix;

  glEnable(GL_LIGHTING);
  glEnable( GL_TEXTURE_2D );
  BindTexture(texGrass);
  glEnable (GL_BLEND);
  glColor3f(1, 1, 1);
  glPointSize(1);
  glNormal3f(0, 0, 1);
  glBegin(GL_TRIANGLE_STRIP);
      glTexCoord2i(0, 0);
      glVertex2i(-999, -999);
      glTexCoord2i(0, 333);
      glVertex2i(-999, 999);
      glTexCoord2i(333, 0);
      glVertex2i(999, -999);
      glTexCoord2i(333, 333);
      glVertex2i(999, 999);
  glEnd;
  glDisable (GL_BLEND);

  Peasant.Draw;
  TTrees.Draw;

  glDisable( GL_TEXTURE_2D);

  glNormal3f(0, 0, 1);
  glColor3f(1, 1, 0.5);
  glPushMatrix;
  glTranslatef(-6, -12, 0);
  glBegin(GL_LINES);
  RandSeed := 0;
  for i := -3999 to 3999 do begin
      v[0] := random*9;
      v[1] := random*9;
      v[2] := 0;
      glVertex3fv(@v);
      v[0] := v[0] + random-0.5;
      v[1] := v[1] + random-0.5;
      v[2] := 0.7;
      glVertex3fv(@v);
  end;
  glEnd;
  glPopMatrix;

  glDisable(GL_LIGHTING);
  glColor3f(1, 1, 1);
end;

end.
