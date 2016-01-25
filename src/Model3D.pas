unit Model3D;

interface uses
  Windows, SysUtils, Classes, Generics.Collections, dglOpengl, MSXML2_TLB, 
  vectors, Geometry;

type
  TFastArray<T> = record
  type P = ^T;
  private
    FData: array of T; 
    function GetData(i: Integer): P;
  public
    Count: Integer;
    function Add: P; overload;
    procedure Add(const v: T); overload;
    procedure SetSize(Size: Integer);
    property Data[i:Integer]: P read GetData; default;
  end;
  TIndexVector = TGLVectori3;
  TIndexArray = TFastArray<TIndexVector>;
  PIndexArray = ^TIndexArray;
  TVectorf16 = array[0..15] of Single;
  TVectorf16Array = TFastArray<TVectorf16>;
  TFaces = TFastArray<TIndexArray>;

  TModelMesh = record
    Name: AnsiString;
    TurnedOff, HasBones: Boolean;
    Faces: TFaces;
    Animations: TVectorf16Array;
    procedure AddPoly(out poly: PIndexArray);
    procedure AddFace(poly: PIndexArray; s1, s2, s3: PAnsiChar);
  end;
  PModelMesh = ^TModelMesh;

  TWeightArray = TFastArray<record VertexIndex: Integer; Weight: Single; end>;
  PModelBone = ^TModelBone;
  TModelBone = record
    Name: AnsiString;
    Point: TGLVectorf3;
    Frames: TVectorf16Array;
    Weights: TWeightArray;
    Parent: PModelBone;
    DebugDraw: Boolean;
    ObjectMatrix: TMatrix;
    function WorldMatrix(Frame: Integer): TMatrix;
  end;
//  PModelBone = ^TModelBone;
  TBoneArray = TFastArray<TModelBone>;
  
  T3DModel = class
  private
    function AddMesh: PModelMesh;
    procedure AddNormal(s1, s2, s3: PAnsiChar);
    procedure AddTex(s1, s2: PAnsiChar);
    procedure AddVertex(s1, s2, s3: PAnsiChar);
    function MeshByName(s: PAnsiChar): PModelMesh;
    function BoneByName(s: PAnsiChar): TBoneArray.P;
    procedure CalcSkin(frame: Integer);
  public
    TexCoords: TFastArray<TGLVectorf2>;
    Vertices, Normals: TFastArray<TGLVectorf3>;
    VerticesSkinned: array of record v: TGLVectorf3; used: Boolean end;
    Meshes: array of TModelMesh;
    Bones: TBoneArray;

    constructor Create(const fn: string);
    destructor Destroy; override;

    procedure Draw; overload;
    procedure Draw(frame: Integer); overload;
    procedure TurnMeshes(Flags: Cardinal);
  const {$J+}
    DebugDraw: Boolean = true;
    DebugIndex: Integer = 211;
  end;

implementation uses
  Math, Character, AnsiStrings, ActiveX;

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
  if Count > Length(FData) then
    SetLength(FData, Length(FData)+1);
//    if Count < 10 then
//      SetLength(Data, 32)
//    else
//      SetLength(Data, Length(Data)*2);
  Result := @FData[Count-1];
end;

procedure TFastArray<T>.Add(const v: T);
begin
  Add^ := v;
end;

function TFastArray<T>.GetData(i: Integer): P;
begin
  Result := @FData[i];
end;

procedure TFastArray<T>.SetSize(Size: Integer);
begin
  Count := Size;
  SetLength(FData, Size);
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
  Result := FStrings[i]^;
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
      if P^ <> Delimiter then 
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

procedure T3DModel.CalcSkin(frame: Integer);
var
  i, j, vi: Integer;
  mpw, mpwi, mt: TMatrix;
  v1: TGLVectorf3;
begin
  SetLength(VerticesSkinned, 0);
  SetLength(VerticesSkinned, Vertices.Count);
  for i := 0 to Bones.Count-1 do
    for j := 0 to Bones[i].Weights.Count-1 do begin
      vi := Bones[i].Weights[j].VertexIndex;
      mpwi := Bones[i].WorldMatrix(0);
      MatrixInvert(mpwi);
      mpw := Bones[i].WorldMatrix(frame);
      mt := MatrixMultiply(mpwi, mpw);
      v1 := VectorTransform(Vertices[vi]^, mt);
      VectorScale(v1, Bones[i].Weights[j].Weight);
      VerticesSkinned[vi].v[0] := VerticesSkinned[vi].v[0] + v1[0];
      VerticesSkinned[vi].v[1] := VerticesSkinned[vi].v[1] + v1[1];
      VerticesSkinned[vi].v[2] := VerticesSkinned[vi].v[2] + v1[2];
      VerticesSkinned[vi].used := true;
    end;
end;

procedure T3DModel.Draw(frame: Integer);
var
  i, j, k: Integer;
  vi3: TIndexVector;
  Mesh: PModelMesh;
  Faces: TFaces.P;
begin
  if Self = nil then
    Exit;
//  if not (DebugIndex in [0..414]) then
//    Exit;
//  for k := DebugIndex to DebugIndex do begin
  CalcSkin(frame);
  for k := 0{DebugIndex} to {DebugIndex{ }High(Meshes){} do begin
    Mesh := @Meshes[k];
    if Mesh.TurnedOff then
      Continue;
    for i := 0 to Mesh.Faces.Count-1 do begin
      if frame >= Mesh.Animations.Count then
        Continue;                                          
      Faces := Mesh.Faces[i];
      glPushMatrix;
      glMultMatrixf(pointer(Mesh.Animations[frame]));
      glBegin(GL_TRIANGLE_FAN);
      for j := 0 to Faces.Count-1 do begin
        vi3 := Faces.FData[j];                 
        glNormal3fv(pointer(Normals[vi3[2]]));
        glTexCoord2fv(pointer(texCoords[vi3[1]]));
        if VerticesSkinned[vi3[0]].used then
          glVertex3fv(@VerticesSkinned[vi3[0]].v)
        else
          glVertex3fv(pointer(Vertices[vi3[0]]));
      end;
      glEnd;
{      if Mesh.HasBones then begin
        glDisable(GL_DEPTH_TEST);
        glPointSize(8);
        glLineWidth(3);
        glDisable(GL_TEXTURE_2D);
        glBegin(GL_POINTS);
        for j := 0 to Bones.Count-1 do begin
          if Bones[j].DebugDraw then
            glColor3f(0, 1, 1)
          else
            glColor3f(1, 0, 1);
          glVertex3fv(@Bones[j].Point);
        end;
        glEnd;
        glBegin(GL_LINES);
        for j := 0 to Bones.Count-1 do begin
          if Bones[j].DebugDraw then
            glColor3f(0, 1, 1)
          else
            glColor3f(1, 0, 1);
          glVertex3fv(@Bones[j].Point);
          if Bones[j].Parent = nil then
            glVertex3fv(@NullVector)
          else
            glVertex3fv(@Bones[j].Parent.Point);
        end;
        glEnd;
        glLineWidth(1);
        glColor3f(1, 1, 1);
        glEnable(GL_DEPTH_TEST);
        glEnable(GL_TEXTURE_2D);
      end;     }
      glPopMatrix;
    end;
  end;
end;

procedure T3DModel.Draw;
var
  i, j, k: Integer;
  vi3: TIndexArray.P;
  Mesh: PModelMesh;
  Faces: TFaces.P;
begin
  if Self = nil then
    Exit;
//  if not (DebugIndex in [Low(Meshes)..High(Meshes)]) then
//    Exit;
//  for k := DebugIndex to DebugIndex do begin
  for k := 0{DebugIndex} to {DebugIndex{ }High(Meshes){} do begin
    Mesh := @Meshes[k];
    for i := 0 to Mesh.Faces.Count-1 do begin
      Faces := Mesh.Faces[i];
      glBegin(GL_TRIANGLE_FAN);
      for j := 0 to Faces.Count-1 do begin
        vi3 := Faces.Data[j];
        glNormal3fv(pointer(Normals[vi3[2]]));
        glTexCoord2fv(pointer(texCoords[vi3[1]]));
        glVertex3fv(pointer(Vertices[vi3[0]]));
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

procedure T3DModel.TurnMeshes(Flags: Cardinal);
var
  i: Integer;
begin
  for i := 0 to High(Meshes) do
    Meshes[i].TurnedOff := Flags and (1 shl i) = 0;      
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

function T3DModel.BoneByName(s: PAnsiChar): TBoneArray.P;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Bones.Count-1 do
    if Bones.Data[i].Name = s then
      Result := Bones[i];
  if (Result = nil) and (s <> '') then begin
    Result := Bones.Add;
    Result.Name := s;
  end;
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
  Bone: TBoneArray.P;
  Line, Pack: TStringListX;
  d: Double;

  procedure AddFaces();
  var
    j: Integer;
  begin
    Mesh.AddPoly(poly);
    with Pack do begin
      for j := 1 to Line.Count-1 do begin
        SetDelimitedText(Line[j]);
        Mesh.AddFace(poly, Strings[0], Strings[1], Strings[2]);
      end;
    end;
  end;

  procedure AddWeights();
  var
    j: Integer;
  begin
    with Pack do begin
      for j := 1 to Line.Count-1 do begin
        SetDelimitedText(Line[j]);
        with Bone.Weights.Add^ do begin
          VertexIndex := ValLong(Strings[0]) + 1;
          if TextToFloat(Strings[1], d) then
            Weight := d;
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
  v16: TVectorf16Array.P;

  procedure LoadObj;
  var
    i, j: Integer;
  begin
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
        if Line[0] = 'b' then
        begin
          Bone := BoneByName(Line[1]);
          for j := 0 to 15 do
            if TextToFloat(Line[2+j], d) then
              TVectorf16(Bone.ObjectMatrix)[j] := d;
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
        if Line[0] = 'o' then begin
          NewMesh;
          Mesh.Name := Line[1];
        end
      end;  
  end;

  procedure LoadObjA;
  var
    i, j: Integer;
  begin
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
  end;

  procedure LoadObjB;
  var
    i, j: Integer;
    Bone: TBoneArray.P;
  begin
      if not FileExists(fn + 'b') then
        Exit;
      with TFileStream.Create(fn+'b', fmOpenRead + fmShareDenyNone) do try
        AnsiStrings.StrDispose(FStorage);
        FStorage := AnsiStrings.AnsiStrAlloc(Size);
        Read(FStorage^, Size);
        FLines.SetDelimitedText(FStorage);
        for i := 0 to FLines.Count-1 do begin
          Line.SetDelimitedText(FLines[i]);
          Bone := BoneByName(Line[1]);
          Bone.Parent := pmodelbone(BoneByName(Line[2]));
          v16 := Bone.Frames.Add;
          for j := 0 to 15 do
            if TextToFloat(Line[3+j], d) then
              v16[j] := d;
        end;
      finally
        Free;
      end;
  end;

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
    AnsiStrings.StrDispose(FStorage);
  end;
end;

{ TModelBone }

function TModelBone.WorldMatrix(Frame: Integer): TMatrix;
begin
  Result := MatrixMultiply(TMatrix(Frames[frame]^), ObjectMatrix);
  Point := VectorTransform(NullVector, Result);
end;

end.
