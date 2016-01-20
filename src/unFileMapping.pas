unit unFileMapping;
{
Copyright (c) 2005-2006 by Davy Landman

See the file COPYING.FPC, included in this distribution,
for details about the copyright. Alternately, you may use this source under the provisions of MPL v1.x or later

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

interface
uses
  Windows, SysUtils;
type
  TMappedFile = class
  private
    FMapping: THandle;
    FContent: Pointer;
    FSize: Integer;
    procedure MapFile(const AFileName: WideString);
  public
    constructor Create(const AFileName: WideString);
    destructor Destroy; override;
    property Content: Pointer read FContent;
    property Size: Integer read FSize;
  end;

implementation

function FileExistsLongFileNames(const FileName: WideString): Boolean;
begin
  if Length(FileName) < 2 then
  begin
    Result := False;
    Exit;
  end;
  if CompareMem(@FileName[1], @WideString('\\')[1], 2) then
    Result := (GetFileAttributesW(PWideChar(FileName)) and FILE_ATTRIBUTE_DIRECTORY = 0)
  else
    Result := (GetFileAttributesW(PWideChar(WideString('\\?\' + FileName))) and FILE_ATTRIBUTE_DIRECTORY = 0)
end;

{ TMappedFile }



constructor TMappedFile.Create(const AFileName: WideString);
begin
  inherited Create;
  if FileExists(AFileName) then
    MapFile(AFileName)
  else
    raise Exception.Create('File "' + AFileName + '" does not exists.');
end;

destructor TMappedFile.Destroy;
begin
  if Assigned(FContent) then
  begin
    UnmapViewOfFile(FContent);
    CloseHandle(FMapping);
  end;
  inherited;
end;

procedure TMappedFile.MapFile(const AFileName: WideString);
var
  FileHandle: THandle;
begin
  if CompareMem(@(AFileName[1]), @('\\'[1]), 2) then
    { Allready an UNC path }
    FileHandle := CreateFileW(PWideChar(AFileName), GENERIC_READ, FILE_SHARE_READ or
      FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0)
  else
    FileHandle := CreateFileW(PWideChar(WideString('\\?\' + AFileName)), GENERIC_READ, FILE_SHARE_READ or
      FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if FileHandle <> 0 then
  try
    FSize := GetFileSize(FileHandle, nil);
    if FSize <> 0 then
    begin
      FMapping := CreateFileMappingW(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
      //Win32Check(FMapping <> 0);
    end;
  finally
    CloseHandle(FileHandle);
  end;
  if FSize = 0 then
    FContent := nil
  else
    FContent := MapViewOfFile(FMapping, FILE_MAP_READ, 0, 0, 0);
  //Win32Check(FContent <> nil);
end;

end.
