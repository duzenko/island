unit ObjLoader;

interface uses
  SysUtils, Classes, Model3D, dglOpengl, Forms, Vectors;

type
  TObjLoader = class helper for TModel3D
  private
    procedure LoadMtl(const fn: string);
  public
    procedure LoadFromObj(const fn: string);
  end;

implementation uses
  unit1;

type
//  TIndexVector = TGLVectori3;
  TStringListX = class(TFastArray)
  private
//    FStrings: TStringArray;
    function ElementSize: Integer; override;
    function GetString(i: Integer): PAnsiChar;
    function GetWideString(i: Integer): String;
  public
    Delimiter: AnsiChar;
    procedure SetDelimitedText(Value: PAnsiChar);
    function CombineFrom(index: Integer): AnsiString;
    constructor Create;
    destructor Destroy; override;
    property Strings[i: Integer]: PAnsiChar read GetString; default;
    property WideStrings[i: Integer]: String read GetWideString;
  end;

function TextToFloat(S: PAnsiChar; var Value: Extended): Boolean;
begin
  Result := SysUtils.TextToFloat(S, Value, fvExtended);
end;

function ValLong(S: PAnsiChar): Longint;
var
  Dig: Integer;
begin
  Result := 0;
  while S^<>#0 do begin
    Dig := Ord(S^)-Ord('0');
    Result := Result * 10 + Dig;
    Inc(S);
  end;
end;

{ TStringListX }

function TStringListX.CombineFrom(index: Integer): AnsiString;
var
  i: Integer;
begin
  Result := Strings[index];
  for i := index + 1 to Count-1 do
    Result := Result + Delimiter + Strings[i];
end;

constructor TStringListX.Create;
begin
  Delimiter := ' ';
end;

destructor TStringListX.Destroy;
begin
  inherited;
end;

function TStringListX.ElementSize: Integer;
begin
  Result := 4;
end;

function TStringListX.GetString(i: Integer): PAnsiChar;
begin
  Result := PAnsiChar(inherited Items[i]^);
end;

function TStringListX.GetWideString(i: Integer): String;
begin
  Result := String(strings[i]);
end;

procedure TStringListX.SetDelimitedText(Value: PAnsiChar);
var
  P, P1: PAnsiChar;
const
  StrictDelimiter = true;
  QuoteChar = '"';
  FStrictDelimiter = StrictDelimiter;
begin
    Count := 0;
    P := PAnsiChar(Value);
    P1 := P;
    while P^ <> #0 do begin
      if P^ = Delimiter then begin
        if p<>p1 then
          Add(p1);
        p^ := #0;
        Inc(P);// := NextChar(P);
        P1 := P;
      end;
      if p^ = #0 then
        Break;
      if P^ <> Delimiter then
        Inc(P);// := NextChar(P);
    end;
    if P1<>P then
      Add(p1);
end;

procedure TObjLoader.LoadMtl(const fn: string);
var
  Line: TStringListX;
  i: integer;
  mtlName: string;
begin
  Line := TStringListX.Create;
  with TStringList.Create do try
    LoadFromFile(fn);
    for i := 0 to Count-1 do begin
      Line.SetDelimitedText(PAnsiChar(UTF8Encode(Strings[i])));
      if Line[0] = 'newmtl' then
        mtlName := String(Line[1]);
      if Line[0] = 'map_Kd' then begin
        MtlStyles.Values[mtlName] := ExtractFilePath(fn) + UTF8Decode(Line.CombineFrom(1));
//        TThread.Synchronize(nil, procedure begin
//          if Form1 <> nil then
//            Form1.CheckListBox1.Items.Add('add ' + fn);
//        end);
      end;
    end;
  finally
    Free;
    Line.Free;
  end;
end;

procedure TObjLoader.LoadFromObj(const fn: string);
var
  Mesh: TModelMesh;
  Bone: TModelBone;
  Line, Pack: TStringListX;
  d: Extended;
  ObjVertices, ObjNormals: TFloat3Array;
  ObjTexcoords: TFloat2Array;
  ReindexMap: TIntegerArray;

procedure AddVertex(s1, s2, s3: PAnsiChar);
var
  f: Extended;
  v: TGLVectorf3;
begin
  if TextToFloat(s1, f) then
    v[0] := f;
  if TextToFloat(s2, f) then
    v[1] := f;
  if TextToFloat(s3, f) then
    v[2] := f;
  ObjVertices.Add(v);
end;

procedure AddNormal(s1, s2, s3: PAnsiChar);
var
  f: Extended;
  v: TGLVectorf3;
begin
  if TextToFloat(s1, f) then
    v[0] := f;
  if TextToFloat(s2, f) then
    v[1] := f;
  if TextToFloat(s3, f) then
    v[2] := f;
  ObjNormals.Add(v);
end;

procedure AddTex(s1, s2: PAnsiChar);
var
  f: Extended;
  v: TGLVectorf2;
begin
  if TextToFloat(s1, f) then
    v[0] := f;
  if TextToFloat(s2, f) then
    v[1] := f;
  ObjTexCoords.Add(v);
end;

  procedure AddFaces();
  var
    vi, ni, ti, j: Integer;
    poly: TIntegerArray;
  begin
    if Line.Count = 4 then
      poly := Mesh.Triangles
    else
      Mesh.AddPoly(poly);
    with Pack do begin
      for j := 1 to Line.Count-1 do begin
        SetDelimitedText(Line[j]);
        vi := ValLong(Strings[0])-1;
        ti := ValLong(Strings[1])-1;
        ni := ValLong(Strings[2])-1;
//        ReindexMap.SetMinSize(vi+1);
        ReindexMap.Add(vi);
        poly.Add(Vertices.Count);
        Vertices.Add(ObjVertices[vi]^);
        TexCoords.Add(ObjTexCoords[ti]^);
        Normals.Add(ObjNormals[ni]^);
      end;
    end;
  end;

  procedure AddWeights();
  var
    i, j, ObjIndex: Integer;
    w: TWeight;
  begin
    with Pack do begin
      for j := 1 to Line.Count-1 do begin
        SetDelimitedText(Line[j]);
        for I := 0 to ReindexMap.Count-1 do begin
          ObjIndex := ValLong(Strings[0]);
          if ReindexMap[i] = ObjIndex then begin
            with w do begin
              VertexIndex := i;
              if TextToFloat(Strings[1], d) then
                Weight := d
              else
                Weight := 0;
            end;
            Bone.Weights.Add(w);
          end;
        end;
      end;
    end;
  end;

  procedure NewMesh;
  begin
    Mesh := AddMesh;
  end;

var
  FStorage: PAnsiChar;
  FLines: TStringListX;
  v16: TMatrix;

  procedure LoadObj;
  var
    i, j: Integer;
  begin
    ObjVertices.Count := 0;
    ObjTexcoords.Count := 0;
    ObjNormals.Count := 0;
    ReindexMap.Count := 0;
      for i := 0 to FLines.Count-1 do begin
        if Application = nil then
          Exit;
        if FLines[i]^ = 'f' then
          Now;
        Line.SetDelimitedText(FLines[i]);
        if Line.Count = 0 then
          Continue;
        if Line[0] = 'v' then begin
          if Mesh = nil then
            NewMesh;
          AddVertex(Line[1], Line[2], Line[3]);
        end;
        if Line[0] = 'b' then
        begin
          Bone := BoneByName(Line[1]);
          for j := 0 to 15 do
            if TextToFloat(Line[2+j], d) then
              Bone.ObjectMatrix.v16[j] := d;
          Mesh.HasBones := true;
        end;
        if Line[0] = 'vw' then
          AddWeights();
        if Line[0] = 'vn' then
          AddNormal(Line.Strings[1], Line[2], Line[3]);
        if Line[0] = 'vt' then
          AddTex(Line.Strings[1], Line.Strings[2]);
        if Line[0] = 'f' then
          AddFaces();
        if (Line[0] = 'o') or (Line[0] = 'g') then begin
          NewMesh;
          Mesh.Name := Line.WideStrings[1];
        end;
        if Line[0] = 'mtllib' then
          LoadMtl(ExtractFilePath(fn) + string(Line.CombineFrom(1)));
        if Line[0] = 'usemtl' then
          Mesh.Material := Line.WideStrings[1];
      end;
  end;

  procedure LoadObjA;
  var
    i, j: Integer;
  begin
      if not FileExists(fn + 'a') then
        Exit;
      with TFileStream.Create(fn+'a', fmOpenRead + fmShareDenyNone) do try
        StrDispose(FStorage);
        FStorage := StrAlloc(Size);
        Read(FStorage^, Size);
        FLines.SetDelimitedText(FStorage);
        for i := 0 to FLines.Count-1 do begin
          Line.SetDelimitedText(FLines[i]);
          Mesh := MeshByName(Line.WideStrings[1]);
          if Mesh = nil then
            Continue;
          for j := 0 to 15 do
            if TextToFloat(Line[2+j], d) then
              v16.v16[j] := d;
          Mesh.Animations.Add(v16);
        end;
      finally
        Free;
      end;
  end;

  procedure LoadObjB;
  var
    i, j: Integer;
    Bone: TModelBone;
  begin
      if not FileExists(fn + 'b') then
        Exit;
      with TFileStream.Create(fn+'b', fmOpenRead + fmShareDenyNone) do try
        StrDispose(FStorage);
        FStorage := StrAlloc(Size);
        Read(FStorage^, Size);
        FLines.SetDelimitedText(FStorage);
        for i := 0 to FLines.Count-1 do begin
          Line.SetDelimitedText(FLines[i]);
          Bone := BoneByName(Line[1]);
          if Line.Count = 19 then
            Bone.Parent := BoneByName(Line[2]);
          for j := 0 to 15 do
            if TextToFloat(Line[Line.Count-16+j], d) then
              v16.v16[j] := d;
          Bone.Frames.Add(v16);
        end;
      finally
        Free;
      end;
  end;

begin
  ObjVertices := TFloat3Array.Create;
  ObjNormals := TFloat3Array.Create;
  ObjTexcoords := TFloat2Array.Create;
  ReindexMap := TIntegerArray.Create;

  Mesh := nil;
  with TFileStream.Create(fn, fmOpenRead + fmShareDenyNone) do try
    FStorage := StrAlloc(Size);
    Read(FStorage^, Size);
  finally
    Free;
  end;
  FLines := TStringListX.Create;
  try
    FLines.Delimiter := #10;
    FLines.SetDelimitedText(FStorage);
    Line := TStringListX.Create;
    Pack := TStringListX.Create;
    try
      Pack.Delimiter := '/';
      LoadObj;
      LoadObjA;
      LoadObjB;
    finally
      Line.Free;
      Pack.Free;
    end;
  finally
    FLines.Free;
    StrDispose(FStorage);
  end;
  ObjVertices.Free;
  ObjNormals.Free;
  ObjTexcoords.Free;
  ReindexMap.Free;
end;

end.
