unit jsonhelper;

interface uses
  SysUtils, Classes, dbxjson, dglOpengl;

type
  TJsonArrayHelper = class helper for TJSONArray
  private
    function GetObject(index: Integer): TJSONObject;
  public
    property Objects[index: Integer]: TJSONObject read GetObject; default;
  end;

  TJsonObjectHelper = class helper for TJSONObject
  private
    function GetArray(const AName: string): TJSONArray;
    function GetInteger(const AName: string): Integer;
    function GetString(const AName: string): String;
    function GetObject(const AName: string): TJSONObject;
  public
    class function LoadFromFile(const fn: string): TJSONObject;

    property Arrays[const AName: string]: TJSONArray read GetArray;
    property Integers[const AName: string]: Integer read GetInteger;
    property Strings[const AName: string]: String read GetString;
    property Objects[const AName: string]: TJSONObject read GetObject; default;

    function IntArray(const AName: string; p: PInteger): Integer;
    function FloatArray(const AName: string; p: PSingle): Integer;
  end;

implementation

{ TJsonHelper }

function TJsonObjectHelper.GetInteger(const AName: string): Integer;
begin
  Result := StrToInt(Get(AName).JsonValue.Value);
end;

function TJsonObjectHelper.GetObject(const AName: string): TJSONObject;
begin
  Result := TJSONObject(Get(AName).JsonValue);
end;

function TJsonObjectHelper.GetString(const AName: string): String;
begin
  Result := Get(AName).JsonValue.Value;
end;

function TJsonObjectHelper.IntArray(const AName: string; p: PInteger): Integer;
begin
  with Arrays[AName] do
    for Result := 0 to Size-1 do begin
      p^ := StrToInt(Get(Result).Value);
      Inc(p);
    end;
  Result := Size;
end;

class function TJsonObjectHelper.LoadFromFile(const fn: string): TJSONObject;
begin
  with TStringStream.Create do try
    LoadFromFile(fn);
    result := TJSONObject(TJSONObject.ParseJSONValue(DataString));
  finally
    Free;
  end;
end;

function TJsonObjectHelper.FloatArray(const AName: string; p: PSingle): Integer;
begin
  with Arrays[AName] do
    for Result := 0 to Size-1 do begin
      p^ := StrToFloat(Get(Result).Value);
      Inc(p);
    end;
  Result := Size;
end;

function TJsonObjectHelper.GetArray(const AName: string): TJSONArray;
begin
  Result := TJSONArray(Get(AName).JsonValue);
end;

{ TJsonArrayHelper }

function TJsonArrayHelper.GetObject(index: Integer): TJSONObject;
begin
  Result := TJSONObject(Get(index));
end;

end.
