unit Terrain;

interface uses
  Windows, SysUtils, Classes, Jpeg, dglOpengl, Graphics, Forms, GifImg;

var
  TerrainSize: TSize;

procedure RenderTerrain;

implementation uses
  Math, shaders, texturemanager, gfxrender, shadows, vectors, Khrono;

var
  HeightTexture: TGLuint;
  CenterHeight: Single;

procedure LoadMap;
const
//  cFileName = '..\maps\qr7YY.jpg';
  cFileName = '..\maps\1881.DinoIsland06.gif';
//  cFileName = '..\maps\1.bmp';
var
  pic: TPicture;
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  pic := TPicture.Create;
  try
    pic.LoadFromFile(cFileName);
    TerrainSize.cx := pic.Width;
    TerrainSize.cy := pic.Height;
    if Application = nil then
      Exit;
    bmp.Assign(pic.Graphic);
    CenterHeight := (bmp.Canvas.Pixels[bmp.Width div 2, bmp.Height div 2] and $ff);
    TThread.Synchronize(nil, procedure
    begin
      glActiveTexture(GL_TEXTURE2);
      glBindTexture(GL_TEXTURE_2D, HeightTexture);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, TerrainSize.cx, TerrainSize.cy, 0,
        IfThen(bmp.PixelFormat = pf8bit, GL_RED, GL_RGB),
        GL_UNSIGNED_BYTE, bmp.ScanLine[bmp.Height-1]);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glActiveTexture(GL_TEXTURE0);
    end);
  finally
    pic.Free;
    FreeAndNil(bmp);
  end;
end;

procedure RenderTerrain;
var
  mxVP, mxM: tmatrix;
const
  TerrainVisibility = 999;
  WorldSize = 19999;
const
  vdata: array[0..31] of Single = (
    -9999, -9999, 0, -333, -333, 0, 0, 1,
    +9999, -9999, 0, +333, -333, 0, 0, 1,
    -9999, +9999, 0, -333, +333, 0, 0, 1,
    +9999, +9999, 0, +333, +333, 0, 0, 1
  );
begin
  if HeightTexture = 0 then begin
    glGenTextures(1, @HeightTexture);
    TThread.CreateAnonymousThread(LoadMap).Start;
  end;
  if TerrainSize.cy = 0 then
    Exit;
  SwitchProgram(prgTerrain);
  Khrono.UISync;
  glPushMatrix;
  glTranslatef(0, 0, -CenterHeight);
  SetShaderMatrix('shadowMatrix', ShadowMatrix);
  SetShaderFloat('worldSize', WorldSize);
  SetShaderFloat('visibility', TerrainVisibility);
  TTextureManager.SwitchTo('..\textures\green-grass-texture.jpg');
  glDrawArraysInstanced(GL_TRIANGLE_STRIP, 0, 2*(TerrainVisibility), TerrainVisibility);
  glPopMatrix;

  SwitchProgram(prgObjects);
  glTranslatef(0, 0, -CenterHeight+1);
  TTextureManager.SwitchTo('..\textures\Tileable classic water texture.jpg');
  SetShaderPointer('vpos', 3, 32, @vdata[0]);
  SetShaderPointer('vnorm', 3, 32, @vdata[5]);
  SetShaderPointer('vtex', 2, 32, @vdata[3]);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
  SetShaderFloat('worldSize', WorldSize);
  SetShaderFloat('visibility', TerrainVisibility);
  glCheckError;
end;

initialization

finalization

end.
