unit TextureManager;

interface uses
  Windows, SysUtils, Classes, dglOpengl, Graphics, Jpeg, PngImage,
  Generics.Collections, Forms;

type
  TTextureRecord = class
    FileName: string;
    FileSize, TextureId: Integer;
    Loaded: Boolean;
  end;
  TTextureList = class(TObjectList<TTextureRecord>)
    function IndexOf(const fn: string): Integer;
    function Add(const fn: string): Integer;
  end;

  TTextureManager = class
    class constructor Create;
    class procedure SwitchTo(const fn: string);
  class var
    Last: string;
    List: TTextureList;
    Disabled: Boolean;
  end;

implementation uses
  unit1;

procedure UploaderProc(bmp: TBitmap);
var
  aniso: Single;
begin
  with bmp do
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, Width, Height, 0, GL_BGR, GL_UNSIGNED_BYTE, ScanLine[Height-1]);
  glGenerateMipmap(GL_TEXTURE_2D);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
  glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, @aniso);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, aniso);
end;

procedure LoadTexture(tr: TTextureRecord);
var
  pic: TPicture;
  bmp: TBitmap;
begin
  tr.Loaded := true;
  bmp := TBitmap.Create;
  pic := TPicture.Create;
  try
    pic.LoadFromFile(tr.FileName);
    bmp.Assign(pic.Graphic);
  finally
    pic.Free;
  end;
  TThread.CreateAnonymousThread(procedure begin
    TThread.Synchronize(nil, procedure
    begin
      with TTextureManager.List do begin
        glBindTexture(GL_TEXTURE_2D, tr.TextureId);
      end;
      Form1.CheckListBox1.Items.Add('upload ' + IntToStr(tr.FileSize div 1024) + ' ' + tr.FileName);
      UploaderProc(bmp);
    end);
    bmp.Free;
  end).Start;
end;

procedure LoaderThread;
var
  tr: TTextureRecord;
  i: Integer;
begin
  repeat
    tr := nil;
    with TTextureManager.List do
      for i := 0 to Count-1 do
        if not Items[i].Loaded then begin
          tr := Items[i];
          Break;
        end;
    if tr <> nil then
      LoadTexture(tr)
    else
      Sleep(1);
  until TTextureManager.List=nil;
end;

{ TTextureManager }

class constructor TTextureManager.Create;
begin
  list := TTextureList.Create;
  list.OwnsObjects := true;
  TThread.CreateAnonymousThread(LoaderThread).Start;
end;

class procedure TTextureManager.SwitchTo(const fn: string);
var
  i, Texture: Integer;
begin
  if Last = fn then
    Exit;
  if Application.Terminated or Disabled then
    Exit;
  i := List.IndexOf(fn);
  if i < 0 then begin
    List.Add(fn);
  end else begin
    Texture := List[i].TextureId;
    glBindTexture(GL_TEXTURE_2D, Texture);
  end;
end;

{ TTextureList }

function TTextureList.Add(const fn: string): Integer;
var
  r: TTextureRecord;
begin
  r := TTextureRecord.Create;
  r.FileName := fn;
  with TFileStream.Create(fn, fmOpenRead) do try
    r.FileSize := Size;
  finally
    Free;
  end;
//  Form1.CheckListBox1.Items.Add('add ' + IntToStr(r.FileSize div 1024));
  glGenTextures(1, @r.TextureId);
  for Result := 0 to Count-1 do
    if Items[Result].FileSize > r.FileSize then begin
      Insert(Result, r);
      Exit;
    end;
  Result := inherited Add(r);
end;

function TTextureList.IndexOf(const fn: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if Items[i].FileName = fn then begin
      Result := i;
      Exit;
    end;
end;

initialization

finalization
  FreeAndNil(TTextureManager.List);

end.
