unit Terrain;

interface uses
  Windows, SysUtils, Classes, Jpeg, dglOpengl, Graphics, Forms, GifImg;

var
  TerrainSize: TSize;

const
  TerrainDetail = 399;
  WorldSize = 9990;

procedure RenderTerrain;
procedure TranslateOnTerrain(x, y: Single);
function GetHeight(x, y: Single): Single;

implementation uses
  Math, shaders, texturemanager, gfxrender, shadows, vectors, Khrono;

var
  HeightTexture: TGLuint;
  htbmp: TBitmap;

function GetHeight(x, y: Single): Single;

  function HeightAt(x, y: Integer): Single;
  begin
    Result := (htbmp.Canvas.Pixels[x, y] and $ff)*0.5
  end;

var
  ix, iy: Integer;
  h: array[0..1, 0..1] of Single;
begin
  if TerrainSize.cy = 0 then
    Result := 0
  else begin
    x := (x/WorldSize+0.5)*(htbmp.Width-1);
    y := (-y/WorldSize+0.5)*(htbmp.Height-1);
    ix := Floor(x);
    iy := Floor(y);
    h[0, 0] := HeightAt(ix, iy);
    h[0, 1] := HeightAt(ix, iy+1);
    h[1, 0] := HeightAt(ix+1, iy);
    h[1, 1] := HeightAt(ix+1, iy+1);
    Result := h[0, 0]*(ix+1-x)*(iy+1-y)+h[1, 0]*(x-ix)*(iy+1-y)+h[0, 1]*(ix+1-x)*(y-iy)+h[1, 1]*(x-ix)*(y-iy);
  end;
end;

procedure TranslateOnTerrain(x, y: Single);
begin
  glTranslatef(x, y, GetHeight(x, y));
end;

procedure LoadMap;
const
//  cFileName = '..\maps\qr7YY.jpg';
  cFileName = '..\maps\1881.DinoIsland06.gif';
  cNormalsFileName = '..\maps\normalmap.bmp';
//  cFileName = '..\maps\1.bmp';
var
  pic: TPicture;
  nbmp: TBitmap;
  y: Integer;
  x: Integer;
  pq: PRGBQuadArray;
  pb: PByteArray;
begin
  htbmp := TBitmap.Create;
  nbmp := TBitmap.Create;
  pic := TPicture.Create;
  try
    pic.LoadFromFile(cNormalsFileName);
    if Application = nil then
      Exit;
    nbmp.Assign(pic.Graphic);
    nbmp.PixelFormat := pf32bit;
    pic.LoadFromFile(cFileName);
    if Application = nil then
      Exit;
    htbmp.Assign(pic.Graphic);
    for y := 0 to nbmp.Height-1 do begin
      pq := nbmp.ScanLine[y];
      pb := htbmp.ScanLine[y];
      for x := 0 to nbmp.Width - 1 do
        pq^[x].rgbReserved := pb^[x];
    end;
    TerrainSize.cx := htbmp.Width;
    TerrainSize.cy := htbmp.Height;
    TThread.Synchronize(nil, procedure
    begin
      glActiveTexture(GL_TEXTURE2);
      glBindTexture(GL_TEXTURE_2D, HeightTexture);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, TerrainSize.cx, TerrainSize.cy, 0,
        GL_BGRA, GL_UNSIGNED_BYTE, nbmp.ScanLine[nbmp.Height-1]);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
      glActiveTexture(GL_TEXTURE0);
    end);
  finally
    nbmp.Free;
    pic.Free;
  end;
end;

procedure RenderTerrain;
const
  vdata: array[0..31] of Single = (
    -1e5, -1e5, 0.5, -1e4, -1e4, 0, 0, 1,
    +1e5, -1e5, 0.5, +1e4, -1e4, 0, 0, 1,
    -1e5, +1e5, 0.5, -1e4, +1e4, 0, 0, 1,
    +1e5, +1e5, 0.5, +1e4, +1e4, 0, 0, 1
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
  SetShaderMatrix('shadowMatrix', ShadowMatrix);
  SetShaderVec3('cameraPos', @CameraLook.x);
  SetShaderFloat('worldSize', WorldSize);
  SetShaderFloat('terrainDetail', TerrainDetail);
  TTextureManager.SwitchTo('..\textures\green-grass-texture.jpg');
  TTextureManager.SwitchTo('..\textures\seamless_stone_cliff_face_mountain_texture_by_hhh316-d68i26q.jpg', GL_TEXTURE3);
  glDrawArraysInstanced(GL_TRIANGLE_STRIP, 0, 2*(TerrainDetail), TerrainDetail{});
  glPopMatrix;

  SwitchProgram(prgObjects);
  TTextureManager.SwitchTo('..\textures\Tileable classic water texture.jpg');
  SetShaderPointer('vpos', 3, 32, @vdata[0]);
  SetShaderPointer('vnorm', 3, 32, @vdata[5]);
  SetShaderPointer('vtex', 2, 32, @vdata[3]);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
  SetShaderFloat('worldSize', WorldSize);
  glCheckError;
end;

initialization

finalization
  FreeAndNil(htbmp);

end.
