unit gfxrender;

interface uses
  Windows, SysUtils, Classes, Graphics, Model3D, Model3DBlend, dglOpenGL;

var
  glRC: THandle;
  AspectRatio: Single;
  CameraLook: record x, y: Single end = (x: -140; y: -90);
  House: T3DModel;
  Peasants: array[1..415] of T3DModel;

procedure Render;
procedure CameraMoved;

implementation uses
  Math, Khrono, MilitiaAdventurer;

var
  Peasant: TMilitiaAdventurer;

type
  TTextureEnum = (texGrass, texHouse, texSun, texMoon, texPeasant);
var
  Textures: array[TTextureEnum] of GLint;

procedure BindTexture(texEnum: TTextureEnum);
begin
  glBindTexture(GL_TEXTURE_2D, Ord(texEnum));
end;

procedure LoadTexture(texEnum: TTextureEnum; const fn: string);
begin
  BindTexture(texEnum);
  with TBitmap.Create do try
    LoadFromFile(fn);
    gluBuild2DMipmaps( GL_TEXTURE_2D, 3, width, height,
                         GL_BGR, GL_UNSIGNED_BYTE, ScanLine[Height-1]);
  finally
    Free;
  end;
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST);
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
var
  i: Integer;
begin
//  TThread.CurrentThread.Priority := tpLower;
  House := T3DModel.Create('..\models\farmhouse\OBJ\Farmhouse OBJ.obj');
//  Peasant := T3DModel.Create('D:\temp\peasant\test.obj');
  Peasant := TMilitiaAdventurer.Create();
//  Peasant := T3DModel.Create('D:\temp\Militia-Adventurer-RIGGED.obj');
//  Peasant := TModel3DBlend.Create('D:\temp\Militia-Adventurer-RIGGED.json');
//  Peasant2 := T3DModel.Create('D:\dev\rnd\island\models\peasant\Militia-Adventurer-RIGGED.obj');
  repeat
    i := InterlockedIncrement(No);
    if i > 415 then
      Break;
//    Peasants[i] := T3DModel.Create(Format('D:\temp\objanim\Militia-Adventurer-RIGGED_%.*d.obj', [6, i]));//'..\models\peasant\Militia-Adventurer-RIGGED.obj');
//    Peasants[i] := T3DModel.Create(Format('D:\temp\peasant\test_%.*d.objx', [6, i]));//'..\models\peasant\Militia-Adventurer-RIGGED.obj');
    Sleep(1);
  until false;
end;

procedure Init;
const
  TexFiles: array[TTextureEnum] of string = (
    '..\textures\grass2.bmp',
    '..\models\farmhouse\Textures\Farmhouse Texture.bmp',
    '..\textures\sun.bmp',
    '..\textures\moon.bmp',
    '..\models\peasant\Militia-Texture.bmp'
  );
var
  tex: TTextureEnum;
//  i: Integer;
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
  glGenTextures(Length(Textures), @Textures);
  for tex := Low(TTextureEnum) to High(TTextureEnum) do
    LoadTexture(tex, TexFiles[tex]);
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
//  RenderSky;

  glEnable(GL_LIGHTING);
  BindTexture(texHouse);
  glColor3f(1, 1, 1);

  glPushMatrix;
  glTranslatef(9, -6, 0);
  glScalef(0.3, 0.3, 0.3);
  glRotatef(90, 1, 0, 0);
//  House.Draw;

  glDisable( GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  glColor4f(0, 0, 0, (1-Abs(SunPos.z))/2+0.5);

  glDepthMask(false);
  glMultMatrixf(@ShadowMatrix);
//  House.Draw;
  glDepthMask(true);

  glPopMatrix;

  glEnable(GL_LIGHTING);
  glEnable( GL_TEXTURE_2D );
  BindTexture(texGrass);
  glEnable (GL_BLEND);
  glColor3f(1, 1, 1);
  glPointSize(1);
  glNormal3f(0, 0, 1);
//  glBegin(GL_TRIANGLE_STRIP);
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

  BindTexture(texPeasant);
  glPushMatrix;
  glTranslatef(3, -2, 0.1);
//  glRotatef(90, 1, 0, 0);
  glRotatef(-99, 0, 0, 1);
//  glScalef(0.04, 0.04, 0.04);
//  glScalef(44, 44, 44);
  Peasant.Draw;
//  Peasants[gettickcount div 40 mod 415 + 1].Draw;
//  TMilitiaAdventurer.Model3d.Draw;
  glPopMatrix;

  glDisable( GL_TEXTURE_2D);

  glNormal3f(0, 0, 1);
  glColor3f(1, 1, 0.5);
  glPushMatrix;
  glTranslatef(-6, -12, 0);
//  glBegin(GL_LINES);
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
end;

end.
