unit TextureManager;

interface uses
  Windows, SysUtils, Classes, dglOpengl, Graphics, Jpeg, PngImage, Generics.Collections;

type
  TTextureManager = class
    class constructor Create;
    class procedure SwitchTo(const fn: string);
  class var
    Last: string;
    List: TStringList;
  end;

implementation

procedure LoadTexture(Texture: Integer; const fn: string);
var
  pic: TPicture;
  bmp: TBitmap;
  aniso: Single;
begin
  pic := TPicture.Create;
  try
    pic.LoadFromFile(fn);
    bmp := TBitmap.Create;
    bmp.PixelFormat := pf24bit;
    bmp.Width := pic.Width;
    bmp.Height := pic.Height;
    bmp.Assign(pic.Graphic);
    GdiFlush;
//      Assign(pic.Graphic);
    with bmp do
      TThread.Synchronize(nil, procedure
      begin
        glBindTexture(GL_TEXTURE_2D, Texture);
        glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, 1);
   //      gluBuild2DMipmaps( GL_TEXTURE_2D, 3, width, height,
  //                           GL_BGR, GL_UNSIGNED_BYTE, ScanLine[Height-1]);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, Width, Height, 0, GL_BGR, GL_UNSIGNED_BYTE, ScanLine[Height-1]);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
        glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, @aniso);
        glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, aniso);
      end);
  finally
    bmp.Free;
    pic.Free;
  end;
end;

{ TTextureManager }

class constructor TTextureManager.Create;
begin
  list := TStringList.Create;
end;

class procedure TTextureManager.SwitchTo(const fn: string);
var
  i, Texture: Integer;
begin
  if Last = fn then
    Exit;
  i := List.IndexOf(fn);
  if i < 0 then begin
    i := List.Add(fn);
    glGenTextures(1, @Texture);
    List.Objects[i] := pointer(Texture);
    TThread.CreateAnonymousThread(procedure begin
      LoadTexture(Texture, fn);
    end).Start;
  end else begin
    Texture := integer(List.Objects[i]);
    glBindTexture(GL_TEXTURE_2D, Texture);
  end;
end;

initialization

finalization
  FreeAndNil(TTextureManager.List);

end.
