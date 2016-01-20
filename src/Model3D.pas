unit Model3D;

interface uses
  Windows, SysUtils, Classes, dglOpengl, MSXML2_TLB;

type
  TFastArray<T> = record
  type P = ^T;
  public
    Count: Integer;
    Data: array of T;
    function Add: P; overload;
    procedure Add(const v: T); overload;
    procedure SetSize(Size: Integer);
  end;
  TIndexVector = TGLVectori3;
  TIndexArray = TFastArray<TIndexVector>;
  PIndexArray = ^TIndexArray;
  TVectorf16 = array[0..15] of Single;
  TVectorf16Array = TFastArray<TVectorf16>;

  TModelMesh = record
    Name: AnsiString;
    Faces: TFastArray<TIndexArray>;
    Animations: TVectorf16Array;
    procedure AddPoly(out poly: PIndexArray);
    procedure AddFace(poly: PIndexArray; s1, s2, s3: PAnsiChar);
  end;
  PModelMesh = ^TModelMesh;

  T3DModel = class
  private
    function AddMesh: PModelMesh;
    procedure AddNormal(s1, s2, s3: PAnsiChar);
    procedure AddTex(s1, s2: PAnsiChar);
    procedure AddVertex(s1, s2, s3: PAnsiChar);
    function MeshByName(s: PAnsiChar): PModelMesh;
  public
    TexCoords: TFastArray<TGLVectorf2>;
    Vertices, Normals: TFastArray<TGLVectorf3>;
    Meshes: array of TModelMesh;
    DebugDraw: Boolean;

    constructor Create(const fn: string);
    destructor Destroy; override;

    procedure Draw; overload;
    procedure Draw(frame: Integer); overload;
  const {$J+}
    DebugIndex: Integer = 0;
  end;

implementation uses
  Math, Character, AnsiStrings, ActiveX, File3DS, Types3DS, vectors;

type
  TStringListX = class
  private
    FStrings: TFastArray<PAnsiChar>;
    function GetString(i: Integer): PAnsiChar;
    function GetCount: Integer;
  public
    Delimiter: AnsiChar;
    procedure SetDelimitedText(Value: PAnsiChar);
    constructor Create;
    destructor Destroy; override;
    property Strings[i: Integer]: PAnsiChar read GetString; default;
    property Count: Integer read GetCount;
  end;

  TFileLoader = class helper for T3DModel
    procedure LoadFromFile(const fn: string);
//    procedure LoadFromDae(const fn: string);
    procedure LoadFromObj(const fn: string);
//    procedure LoadFromOgre(const fn: string);
//    procedure LoadFrom3ds(const fn: string);
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

{ TVectorf3Array }

function TFastArray<T>.Add: P;
begin
  Inc(Count);
  if Count > Length(Data) then
    SetLength(Data, Length(Data)+1);
//    if Count < 10 then
//      SetLength(Data, 32)
//    else
//      SetLength(Data, Length(Data)*2);
  Result := @Data[Count-1];
end;

procedure TFastArray<T>.Add(const v: T);
begin
  Add^ := v;
end;

procedure TFastArray<T>.SetSize(Size: Integer);
begin
  Count := Size;
  SetLength(Data, Size);
end;

{ TStringListX }

constructor TStringListX.Create;
begin
  Delimiter := ' ';
end;

destructor TStringListX.Destroy;
begin
  inherited;
end;

function TStringListX.GetCount: Integer;
begin
  Result := FStrings.Count;
end;

function TStringListX.GetString(i: Integer): PAnsiChar;
begin
  Result := FStrings.Data[i];
end;

procedure TStringListX.SetDelimitedText(Value: PAnsiChar);
var
  P, P1: PAnsiChar;
const
  StrictDelimiter = true;
  QuoteChar = '"';
  FStrictDelimiter = StrictDelimiter;
//  Delimiter = ' ';
begin
    FStrings.Count := 0;
    P := PAnsiChar(Value);
    P1 := P;
    while P^ <> #0 do begin
      if P^ = Delimiter then begin
        FStrings.Add(p1);
        p^ := #0;
        Inc(P);// := NextChar(P);
        P1 := P;
      end;
      Inc(P);// := NextChar(P);
    end;
    if P1<>P then
      FStrings.Add(p1);
end;

{ T3DModel }

constructor T3DModel.Create(const fn: string);
begin
  LoadFromFile(fn);
end;

destructor T3DModel.Destroy;
begin
  inherited;
end;

procedure T3DModel.Draw(frame: Integer);
var
  i, j, k: Integer;
  vi3: TIndexVector;
  Mesh: PModelMesh;
  Faces: ^TIndexArray;
begin
  if Self = nil then
    Exit;
  if not (DebugIndex in [Low(Meshes)..High(Meshes)]) then
    Exit;
//  for k := DebugIndex to DebugIndex do begin
  for k := 0{DebugIndex} to {DebugIndex{ }High(Meshes){} do begin
    Mesh := @Meshes[k];
    for i := 0 to Mesh.Faces.Count-1 do begin
      if frame >= Mesh.Animations.Count then
        Continue;
      Faces := @Mesh.Faces.Data[i];
      glPushMatrix;
      glMultMatrixf(@Mesh.Animations.Data[frame]);
      glBegin(GL_TRIANGLE_FAN);
      for j := 0 to Faces.Count-1 do begin
        vi3 := Faces.Data[j];
        glNormal3fv(@Normals.Data[vi3[2]]);
        glTexCoord2fv(@texCoords.Data[vi3[1]]);
        glVertex3fv(@Vertices.Data[vi3[0]]);
      end;
      glEnd;
      glPopMatrix;
    end;
  end;
end;

procedure T3DModel.Draw;
var
  i, j, k: Integer;
  vi3: TIndexVector;
  Mesh: PModelMesh;
  Faces: ^TIndexArray;
begin
  if Self = nil then
    Exit;
  if not (DebugIndex in [Low(Meshes)..High(Meshes)]) then
    Exit;
//  for k := DebugIndex to DebugIndex do begin
  for k := 0{DebugIndex} to {DebugIndex{ }High(Meshes){} do begin
    Mesh := @Meshes[k];
    for i := 0 to Mesh.Faces.Count-1 do begin
      Faces := @Mesh.Faces.Data[i];
      glBegin(GL_TRIANGLE_FAN);
      for j := 0 to Faces.Count-1 do begin
        vi3 := Faces.Data[j];
        glNormal3fv(@Normals.Data[vi3[2]]);
        glTexCoord2fv(@texCoords.Data[vi3[1]]);
        glVertex3fv(@Vertices.Data[vi3[0]]);
      end;
      glEnd;
    end;
  end;
end;

function T3DModel.MeshByName(s: PAnsiChar): PModelMesh;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to High(Meshes) do
    if Meshes[i].Name = s then
      Result := @Meshes[i];
end;

procedure OutputDebug(const s: string);
begin
  OutputDebugString(PWideChar(StringReplace(s, #13#10, '', [rfReplaceAll])));
end;

  function T3DModel.AddMesh;
  begin
    SetLength(Meshes, Length(Meshes)+1);
    Result := @Meshes[High(Meshes)];
  end;

function TextToFloat(S: PAnsiChar; var Value: Double): Boolean;
begin
  Result := SysUtils.TextToFloat(string(S), Value, FormatSettings);
end;

procedure T3DModel.AddVertex(s1, s2, s3: PAnsiChar);
var
  f: Double;
  v: TGLVectorf3;
begin
  if TextToFloat(s1, f) then
    v[0] := f;
  if TextToFloat(s2, f) then
    v[1] := f;
  if TextToFloat(s3, f) then
    v[2] := f;
  Vertices.Add(v);
end;

procedure T3DModel.AddNormal(s1, s2, s3: PAnsiChar);
var
  f: Double;
  v: TGLVectorf3;
begin
  if TextToFloat(s1, f) then
    v[0] := f;
  if TextToFloat(s2, f) then
    v[1] := f;
  if TextToFloat(s3, f) then
    v[2] := f;
  Normals.Add(v);
end;

procedure TModelMesh.AddPoly(out poly: PIndexArray);
begin
  Poly := pointer(Faces.Add);
end;

procedure T3DModel.AddTex(s1, s2: PAnsiChar);
var
  f: Double;
  v: TGLVectorf2;
begin
  if TextToFloat(s1, f) then
    v[0] := f;
  if TextToFloat(s2, f) then
    v[1] := f;
  TexCoords.Add(v);
end;

procedure TModelMesh.AddFace(poly: PIndexArray; s1, s2, s3: PAnsiChar);
var
  v: TIndexVector;
begin
  v[0] := ValLong(s1);
  v[1] := ValLong(s2);
  v[2] := ValLong(s3);
  poly.Add(v);
end;

{ TFileLoader }

{procedure TFileLoader.LoadFrom3ds(const fn: string);
var
  i, j: Integer;
  File3ds: TFile3DS;
  Mesh: PModelMesh;
  Faces: PIndexArray;
  f: TFace3DS;
  v: TIndexVector;
  Vector1, Vector2: TVector3f;
begin
  File3ds := TFile3DS.Create;
  try
    File3ds.LoadFromFile(fn);
    for i := 0 to File3ds.Objects.MeshCount-1 do begin
      Mesh := AddMesh;
      for j := 0 to File3ds.Objects.Mesh[i].NVertices-1 do
        Mesh.Vertices.Add(TGLVectorf3(File3ds.Objects.Mesh[i].VertexArray[j]));
      for j := 0 to File3ds.Objects.Mesh[i].NTextVerts-1 do
        Mesh.TexCoords.Add(TGLVectorf2(File3ds.Objects.Mesh[i].TextArray[j]));
      for j := 0 to File3ds.Objects.Mesh[i].NFaces-1 do begin
        Faces := Pointer(Mesh.Faces.Add);
        f := File3ds.Objects.Mesh[i].FaceArray[j];
        Vector1 := VectorAffineSubtract(Mesh.Vertices.Data[f.V1], Mesh.Vertices.Data[f.V2]);
        Vector2 := VectorAffineSubtract(Mesh.Vertices.Data[f.V3], Mesh.Vertices.Data[f.V2]);
        Mesh.Normals.Add(VectorCrossProduct(Vector2, Vector1));
        v[0] := f.V1;  v[1] := f.V1;  v[2] := j;
        Faces.Add(v);
        v[0] := f.V2;  v[1] := f.V2;  v[2] := j;
        Faces.Add(v);
        v[0] := f.V3;  v[1] := f.V3;  v[2] := j;
        Faces.Add(v);
      end;
    end;
  finally
    File3ds.Free;
  end;
end;

procedure TFileLoader.LoadFromDae(const fn: string);
var
  Mesh: PModelMesh;
  poly: PIndexArray;

  procedure AddMeshData(node: IXMLDOMNode);
  var
    i: Integer;
    id: string;
  begin
    id := node.attributes[0].nodeValue;
    with TStringListX.Create do try
      SetDelimitedText(pansichar(UTF8Encode(node.text)));
      if Pos('position', id) > 0 then
        for i := 0 to Count div 3 - 1 do
          Mesh.AddVertex(Strings[i*3], Strings[i*3+1], Strings[i*3+2]);
      if Pos('normal', id) > 0 then
        for i := 0 to Count div 3 - 1 do
          Mesh.AddNormal(Strings[i*3], Strings[i*3+1], Strings[i*3+2]);
      if Pos('map', id) > 0 then
        for i := 0 to Count div 2 - 1 do
          Mesh.AddTex(Strings[i*2], Strings[i*2+1]);
    finally
      Free;
    end;
  end;

  procedure AddPolys(node: IXMLDOMNode);
  var
    i: Integer;
  begin
    with TStringListX.Create do try
      SetDelimitedText(pansichar(UTF8Encode(node.text)));
      for i := 0 to Count div 3 - 1 do begin
        if i mod 3 = 0 then
          Mesh.AddPoly(poly);
        Mesh.AddFace(poly, Strings[i*3], Strings[i*3+2], Strings[i*3+1]);
      end;
    finally
      Free;
    end;
  end;

  procedure AddMesh(node: IXMLDOMNode);
  var
    i: Integer;
    nodes: IXMLDOMNodeList;
  begin
    Mesh := Self.AddMesh;
    nodes := node.selectNodes('d:source/d:float_array');
    for i := 0 to nodes.length-1 do
      AddMeshData(nodes[i]);
    AddPolys(node.selectSingleNode('d:polylist/d:p'));
//    OutputDebug(node.text);
  end;

var
  xmlSource: DOMDocument60;
  nodes: IXMLDOMNodeList;
  i: Integer;
begin
  CoInitialize(nil);
  xmlSource := CoDOMDocument60.Create;
  assert(xmlSource.load(fn));
  xmlSource.setProperty('SelectionNamespaces', 'xmlns:d="http://www.collada.org/2005/11/COLLADASchema"');
  nodes := xmlSource.documentElement.selectNodes('/d:COLLADA/d:library_geometries/d:geometry/d:mesh');
  for i := 0 to nodes.length-1 do
    AddMesh(nodes.item[i]);
//  nodes := xmlSource.documentElement.selectNodes('/d:COLLADA/d:library_geometries/d:geometry/d:mesh');
//  for i := 0 to nodes.length-1 do
//    AddMesh(nodes.item[i]);
end;           }

procedure TFileLoader.LoadFromFile(const fn: string);
begin
  if ExtractFileExt(fn) = '.obj' then
    LoadFromObj(fn);
//  if ExtractFileExt(fn) = '.dae' then
//    LoadFromDae(fn);
//  if ExtractFileExt(fn) = '.scene' then
//    LoadFromOgre(fn);
//  if ExtractFileExt(fn) = '.3ds' then
//    LoadFrom3ds(fn);
end;

procedure TFileLoader.LoadFromObj(const fn: string);
var
  Mesh: PModelMesh;
  poly: PIndexArray;

  procedure AddFaces(Line, Face: TStringListX);
  var
    j: Integer;
  begin
    Mesh.AddPoly(poly);
    with Face do begin
      for j := 1 to Line.Count-1 do begin
        SetDelimitedText(Line[j]);
        Mesh.AddFace(poly, Strings[0], Strings[1], Strings[2]);
      end;
    end;
  end;

  procedure NewMesh;
  begin
    Mesh := AddMesh;
  end;

var
  i, j: Integer;
  Line, Face: TStringListX;
  FStorage: PAnsiChar;
  FLines: TStringListX;
  v16: TVectorf16Array.P;
  d: Double;
begin
  Mesh := nil;
  with TFileStream.Create(fn, fmOpenRead + fmShareDenyNone) do try
    FStorage := AnsiStrings.AnsiStrAlloc(Size);
    Read(FStorage^, Size);
  finally
    Free;
  end;
  FLines := TStringListX.Create;
  try
    FLines.Delimiter := #10;
    FLines.SetDelimitedText(FStorage);
    Line := TStringListX.Create;
    Face := TStringListX.Create;
    try
      Face.Delimiter := '/';
      Vertices.Add;
      Normals.Add;
      TexCoords.Add;
      for i := 0 to FLines.Count-1 do begin
        Line.SetDelimitedText(FLines[i]);
        if Line.Count = 0 then
          Continue;
        if Line[0] = 'v' then begin
          if Mesh = nil then
            NewMesh;
          AddVertex(Line[1], Line[2], Line[3]);
        end;
        if Line[0] = 'vn' then
          AddNormal(Line.Strings[1], Line[2], Line[3]);
        if Line[0] = 'vt' then
          AddTex(Line.Strings[1], Line.Strings[2]);
        if Line[0] = 'f' then begin
          AddFaces(Line, Face);
        end;
        if Line[0] = 'o' then begin
          NewMesh;
          Mesh.Name := Line[1];
        end
      end;
      if not FileExists(fn + 'a') then
        Exit;
      with TFileStream.Create(fn+'a', fmOpenRead + fmShareDenyNone) do try
        AnsiStrings.StrDispose(FStorage);
        FStorage := AnsiStrings.AnsiStrAlloc(Size);
        Read(FStorage^, Size);
        FLines.SetDelimitedText(FStorage);
        for i := 0 to FLines.Count-1 do begin
          Line.SetDelimitedText(FLines[i]);
          Mesh := MeshByName(Line[1]);
          if Mesh = nil then
            Continue;
          v16 := Mesh.Animations.Add;
          for j := 0 to 15 do
            if TextToFloat(Line[2+j], d) then
              v16[j] := d;
        end;
      finally
        Free;
      end;
    finally
      Line.Free;
      Face.Free;
    end;
  finally
    FLines.Free;
    AnsiStrings.StrDispose(FStorage);
  end;
end;

{procedure TFileLoader.LoadFromOgre(const fn: string);

  procedure AddNode(node: IXMLDOMNode);
  var
    ModelPart: PModelPart;
    n: IXMLDOMNode;
  begin
    New(ModelPart);
    n := node.selectSingleNode('position');
    ModelPart.Translate[0] := StrToFloat(n.attributes.getNamedItem('x').nodeValue);
  end;

var
  xmlSource: DOMDocument60;
  nodes: IXMLDOMNodeList;
  i: Integer;
begin
  CoInitialize(nil);
  xmlSource := CoDOMDocument60.Create;
  assert(xmlSource.load(fn));
  nodes := xmlSource.documentElement.selectNodes('/scene/nodes/node');
  for i := 0 to nodes.length-1 do
    AddNode(nodes.item[i]);
end;          }

end.
