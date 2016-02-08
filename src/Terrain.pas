unit Terrain;

interface uses
  Windows, SysUtils, Classes, Jpeg, dglOpengl, Graphics, Forms;

var
  TerrainSize: TSize;

procedure RenderTerrain;

implementation uses
  shaders, texturemanager, gfxrender, shadows, vectors, Khrono;

var
  HeightTexture, vao: TGLuint;

procedure LoadMapJpg;
const
  cFileName = '..\maps\qr7YY.jpg';
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
    TThread.Synchronize(nil, procedure
    begin
      glActiveTexture(GL_TEXTURE2);
      glBindTexture(GL_TEXTURE_2D, HeightTexture);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, TerrainSize.cx, TerrainSize.cy, 0, GL_RGB, GL_UNSIGNED_BYTE, bmp.ScanLine[bmp.Height-1]);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  //    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  //    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  //    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);
  //    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC, GL_LEQUAL);
      glActiveTexture(GL_TEXTURE0);
    end);
  finally
    pic.Free;
    FreeAndNil(bmp);
  end;
end;

procedure RenderTerrain;
const
  vdata: array[0..31] of Single = (
    -999, -999, 0, -333, -333, 0, 0, 1,
    +999, -999, 0, +333, -333, 0, 0, 1,
    -999, +999, 0, -333, +333, 0, 0, 1,
    +999, +999, 0, +333, +333, 0, 0, 1
  );
var
  mxVP, mxM: tmatrix;
begin
  if HeightTexture = 0 then begin
    glGenTextures(1, @HeightTexture);
    glGenVertexArrays(1, @vao);
    TThread.CreateAnonymousThread(LoadMapJpg).Start;
  end;
  if TerrainSize.cy = 0 then
    Exit;
  SwitchProgram(prgTerrain);
  Khrono.UISync;
  SetShaderMatrix('shadowMatrix', ShadowMatrix);
//  GetMem(tmp, 4*TerrainSize.cx*TerrainSize.cy);
  SetShaderFloat('terrainSize', TerrainSize.cx);
  TTextureManager.SwitchTo('..\textures\green-grass-texture.jpg');
  glDrawArraysInstanced(GL_TRIANGLE_STRIP, 0, 2*(TerrainSize.cx-1), TerrainSize.cy-1);
  SwitchProgram(prgObjects);
  glCheckError;
//  Dispose(tmp);
end;

initialization

finalization

end.
