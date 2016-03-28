unit TextureManager;

interface uses
  Windows, SysUtils, Classes, dglOpengl, Graphics, Jpeg, PngImage,
  Forms, Contnrs;

type
  TTextureRecord = class
    FileName: string;
    FileSize, TextureId, TexUnit: Integer;
    Loaded: Boolean;
  end;
  TTextureList = class(TObjectList)
  private
    function GetItems(index: Integer): TTextureRecord; 
    procedure SetItems(index: Integer; const Value: TTextureRecord);
    procedure UploaderProc;
  public
    function IndexOf(const fn: string): Integer;
    function Add(const fn: string; TexUnit: Integer): Integer;
    property Items[index: Integer]: TTextureRecord read GetItems write SetItems; default;
  end;

  TTextureManager = class
    class procedure SwitchTo(const fn: string; TexUnit: Integer = GL_TEXTURE0);
  class var
    Disabled: Boolean;
  end;

implementation uses
  unit1, Math;

  var
    Last: string;
    List: TTextureList;

var
  UploaderProcbmp: TBitmap;
  UploaderProctr: TTextureRecord;
procedure TTextureList.UploaderProc();
var
  aniso: Single;
begin
  with UploaderProctr do begin
      glActiveTexture(TexUnit);
      glBindTexture(GL_TEXTURE_2D, TextureId);
      Form1.CheckListBox1.Items.Add('upload ' + IntToStr(FileSize div 1024) + ' ' + FileName);
  end;
  with UploaderProcbmp do
    glTexImage2D(GL_TEXTURE_2D, 0,
      IfThen(PixelFormat = pf24bit, GL_RGB, GL_RGBA),
      Width, Height, 0,
      IfThen(PixelFormat = pf24bit, GL_BGR, GL_BGRA),
      GL_UNSIGNED_BYTE, ScanLine[Height-1]);
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
    if Application = nil then
      Exit;
    if pic.Graphic is TPNGObject then begin
      bmp.Width := pic.Width;
      bmp.Height := pic.Height;
      bmp.Canvas.Brush.Color := clBlack;
      bmp.Canvas.FillRect(bmp.Canvas.ClipRect);
      bmp.Canvas.Draw(0, 0, pic.Graphic);
    end else
      bmp.Assign(pic.Graphic);
  finally
    pic.Free;
  end;
  UploaderProctr := tr;
  UploaderProcbmp := bmp;
  TThread.Synchronize(nil, List.UploaderProc);
  bmp.Free;
end;

procedure LoaderThread;
var
  tr: TTextureRecord;
  i: Integer;
begin
  repeat
    tr := nil;
    with List do
      for i := 0 to Count-1 do
        if not Items[i].Loaded then begin
          tr := Items[i];
          Break;
        end;
    if tr <> nil then
      LoadTexture(tr)
    else
      Sleep(1);
  until List=nil;
end;

{ TTextureManager }

class procedure TTextureManager.SwitchTo(const fn: string; TexUnit: Integer = GL_TEXTURE0);
var
  i, Texture: Integer;
begin
  if Last = fn then
    Exit;
  if Application.Terminated or Disabled then
    Exit;
  i := List.IndexOf(fn);
  if i < 0 then begin
    List.Add(fn, TexUnit);
  end else begin
    Texture := List[i].TextureId;
    glActiveTexture(TexUnit);
    glBindTexture(GL_TEXTURE_2D, Texture);
  end;
end;

{ TTextureList }

function TTextureList.Add(const fn: string; TexUnit: Integer): Integer;
var
  r: TTextureRecord;
begin
  r := TTextureRecord.Create;
  r.TexUnit := TexUnit;
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

function TTextureList.GetItems(index: Integer): TTextureRecord;
begin
  result := TTextureRecord(inherited Items[index]);
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

procedure TTextureList.SetItems(index: Integer; const Value: TTextureRecord);
begin
  inherited;
end;

var
  tid: Cardinal;
initialization
  list := TTextureList.Create;
  list.OwnsObjects := true;
//  TThread.CreateAnonymousThread(LoaderThread).Start;
  BeginThread(nil, 0, @LoaderThread, nil, 0, tid);

finalization
  FreeAndNil(List);

end.
