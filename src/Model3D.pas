unit Model3D;

interface uses
  Windows, SysUtils, Classes, dglOpengl,
  vectors;

type
  TFastArray = class
  private
    FData: array of Byte;
    function GetItems(index: Integer): Pointer;
  protected
    function ElementSize: Integer; virtual;
  public
    Count: Integer;
    function Add: Pointer; overload;
    procedure Add(const v); overload;
    function GetData(i: Integer): Pointer;
    procedure SetMinSize(Size: Integer);
    procedure SetSize(Size: Integer);
    property Items[index: Integer]: Pointer read GetItems; default;
  end;

  TMatrixArray = class(TFastArray)
  protected
    function ElementSize: Integer; override;
  private
    function GetItems(index: Integer): PMatrix;
  public
    property Items[index: Integer]: PMatrix read GetItems; default;
  end;

  TIntegerArray = class(TFastArray)
  private
    function GetItems(index: Integer): Integer;
  public
    property Items[index: Integer]: Integer read GetItems; default;
  end;

  TFaces = class(TFastArray)
  private
    function GetItems(index: Integer): TIntegerArray;
  public
    property Items[index: Integer]: TIntegerArray read GetItems; default;
  end;

  TModelMesh = class
    Name, Material: String;
    TurnedOff, HasBones: Boolean;
    Polys: TFaces;
    Triangles: TIntegerArray;
    Animations: TMatrixArray;
    procedure AddPoly(out poly: TIntegerArray);
    constructor Create;
    destructor Destroy; override;
  end;

  TMeshArray = class(TFastArray)
  private
    function GetItems(index: Integer): TModelMesh;
  public
    property Items[index: Integer]: TModelMesh read GetItems; default;
  end;


  TWeight = record VertexIndex: Integer; Weight: Single; end;
  TWeightArray = class(TFastArray)
  private
    function GetItems(index: Integer): TWeight;
  protected
    function ElementSize: Integer; override;
  public
    property Items[index: Integer]: TWeight read GetItems; default;
  end;

  TModelBone = class
    Name: AnsiString;
    Point: TGLVectorf3;
    Frames: TMatrixArray;
    Weights: TWeightArray;
    Parent: TModelBone;
    DebugDraw: Boolean;
    ObjectMatrix: TMatrix;
    constructor Create;
    destructor Destroy; override;
    function WorldMatrix(Frame: Integer): TMatrix;
  end;

  TBoneArray = class(TFastArray)
  private
    function GetItems(index: Integer): TModelBone;
  protected
    function ElementSize: Integer; override;
  public
    property Items[index: Integer]: TModelBone read GetItems; default;
  end;

  PVector2f = ^TGLVectorf2;
  TFloat2Array = class(TFastArray)
  private
    function GetItems(index: Integer): PVector2f;
  protected
    function ElementSize: Integer; override;
  public
    property Items[index: Integer]: PVector2f read GetItems; default;
  end;

  PVector3f = ^TVector3f;
  TFloat3Array = class(TFastArray)
  private
    function GetItems(index: Integer): PVector3f;
  protected
    function ElementSize: Integer; override;
  public
    property Items[index: Integer]: PVector3f read GetItems; default;
  end;

  TModel3D = class
  protected
    vbos: array[0..3] of GLuint;
    function AddMesh: TModelMesh;
    function MeshByName(const s: String): TModelMesh;
    function BoneByName(s: PAnsiChar): TModelBone;
    procedure CalcSkin(frame: Integer);
    procedure LoadFromFile(const fn: string);
    procedure DrawIndexArray(Mode: GLint; ia: TIntegerArray);
  public
    TexCoords: TFloat2Array;
    Vertices, Normals: TFloat3Array;
    VerticesSkinned: array of TGLVectorf3;
    Meshes: TMeshArray;
    Bones: TBoneArray;
    MtlStyles: TStringList;

    constructor Create(const fn: string);
    destructor Destroy; override;

    procedure Draw; overload;
    procedure Draw(frame: Integer); overload;
    procedure Draw(const pos: TFloat3Array); overload;
    procedure TurnMeshes(Flags: Cardinal);
    function AbsSize: TGLVectorf2;
  const {$J+}
    DebugDraw: Boolean = true;
    DebugIndex: Integer = 211;
  end;

implementation uses
  Math, ActiveX, ObjLoader, TextureManager, shaders;

{ TVectorf3Array }

function TFastArray.Add: Pointer;
begin
  Inc(Count);
  if Count*ElementSize > Length(FData) then
    SetLength(FData, Length(FData)+ElementSize);
//    if Count < 10 then
//      SetLength(Data, 32)
//    else
//      SetLength(Data, Length(Data)*2);
  Result := Items[Count-1];
end;

procedure TFastArray.Add(const v);
var
  p: pointer;
begin
  p := Add;
  Move(v, p^, ElementSize);
end;

function TFastArray.ElementSize: Integer;
begin
  Result := 4;
end;

function TFastArray.GetData(i: Integer): Pointer;
begin
  Result := @FData[i];
end;

function TFastArray.GetItems(index: Integer): Pointer;
begin
  Result := @FData[index*ElementSize];
end;

procedure TFastArray.SetMinSize(Size: Integer);
begin
  if Size > Count then
    SetSize(Size);
end;

procedure TFastArray.SetSize(Size: Integer);
begin
  Count := Size;
  SetLength(FData, Size);
end;

{ T3DModel }

constructor TModel3D.Create(const fn: string);
begin
  MtlStyles := TStringList.Create;
  Bones := TBoneArray.Create;
  Meshes := TMeshArray.Create;
  Vertices := TFloat3Array.Create;
  Normals := TFloat3Array.Create;
  TexCoords := TFloat2Array.Create;
  LoadFromFile(fn);
end;

destructor TModel3D.Destroy;
begin
  inherited;
  FreeAndNil(Meshes);
  FreeAndNil(Bones);
  FreeAndNil(MtlStyles);
  FreeAndNil(Vertices);
  FreeAndNil(Normals);
  FreeAndNil(TexCoords);
end;

procedure TModel3D.CalcSkin(frame: Integer);
var
  i, j, vi: Integer;
  mpw, mpwi, mt: TMatrix;
  v1: TVector;
  used: array of Boolean;
begin
  SetLength(VerticesSkinned, Vertices.Count);
  ZeroMemory(VerticesSkinned, Vertices.Count*12);
  SetLength(used, Vertices.Count);
  for i := 0 to Bones.Count-1 do begin
      mpwi := Bones[i].WorldMatrix(0);
      mpwi.Invert;
      mpw := Bones[i].WorldMatrix(frame);
      mt := mpw*mpwi;
    for j := 0 to Bones[i].Weights.Count-1 do begin
      vi := Bones[i].Weights[j].VertexIndex;
      v1 := mt*Vertices[vi]^;
      v1 := v1 * Bones[i].Weights[j].Weight;
      VerticesSkinned[vi] := TVector(VerticesSkinned[vi]) + v1;
      used[vi] := true;
    end;
  end;
  for i := 0 to Vertices.Count-1 do
    if not used[i] then
      VerticesSkinned[i] := Vertices[i]^;
end;

procedure TModel3D.Draw(frame: Integer);
var
  i, k: Integer;
  Mesh: TModelMesh;
  Faces: TIntegerArray;
begin
  if Self = nil then
    Exit;
  CalcSkin(frame);
  SetShaderPointer('vpos', 3, 0, VerticesSkinned);
  SetShaderPointer('vtex', 2, 0, TexCoords[0]);
  SetShaderPointer('vnorm', 3, 0, Normals[0]);
  for k := 0{DebugIndex} to {DebugIndex{ }Meshes.Count-1{} do begin
    Mesh := Meshes[k];
    if Mesh.TurnedOff then
      Continue;
    glPushMatrix;
    TTextureManager.SwitchTo(MtlStyles.Values[mesh.Material]);
    if frame < Mesh.Animations.Count then begin
      glMultMatrixf(Mesh.Animations[frame]^);
      for i := 0 to Mesh.Polys.Count-1 do begin
        Faces := Mesh.Polys[i];
        DrawIndexArray(GL_TRIANGLE_FAN, Faces);
      end;
    end;
    glPopMatrix;
  end;
end;
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

procedure TModel3D.Draw(const pos: TFloat3Array);
var
  k: Integer;
  Mesh: TModelMesh;
  attrLoc: Integer;
begin
  if Self = nil then
    Exit;
  if (pos = nil) or (pos.Count = 0) then
    Exit;
  if vbos[0] = 0 then begin
    glGenBuffers (4, @vbos[0]);
    glBindBuffer (GL_ARRAY_BUFFER, vbos[0]);
    glBufferData (GL_ARRAY_BUFFER, 3*Vertices.Count*sizeof(Single), Vertices[0], GL_STATIC_DRAW);
    glBindBuffer (GL_ARRAY_BUFFER, vbos[1]);
    glBufferData (GL_ARRAY_BUFFER, 2*TexCoords.Count*sizeof(Single), TexCoords[0], GL_STATIC_DRAW);
    glBindBuffer (GL_ARRAY_BUFFER, vbos[2]);
    glBufferData (GL_ARRAY_BUFFER, 3*Normals.Count*sizeof(Single), Normals[0], GL_STATIC_DRAW);
  end;
  glBindBuffer (GL_ARRAY_BUFFER, vbos[0]);
  SetShaderPointer('vpos', 3, 0, nil);
  glBindBuffer (GL_ARRAY_BUFFER, vbos[1]);
  SetShaderPointer('vtex', 2, 0, nil);
  glBindBuffer (GL_ARRAY_BUFFER, vbos[2]);
  SetShaderPointer('vnorm', 3, 0, nil);
  glBindBuffer (GL_ARRAY_BUFFER, vbos[3]);
  glBufferData (GL_ARRAY_BUFFER, 3*sizeof(Single)*pos.Count, pos[0], GL_STATIC_DRAW);
  attrLoc := SetShaderPointer('instPos', 3, 0, nil);
  if attrLoc >= 0 then
    glVertexAttribDivisor(attrLoc, 1);
  SetShaderFloat('instanced', 1);
  for k := 0{DebugIndex} to {DebugIndex{ }Meshes.Count-1{} do begin
    Mesh := Meshes[k];
    if Mesh.TurnedOff then
      Continue;
    TTextureManager.SwitchTo(MtlStyles.Values[mesh.Material]);
    glDrawArraysInstanced(GL_TRIANGLES, Mesh.Triangles[0], Mesh.Triangles.Count, pos.Count);
  end;
  SetShaderFloat('instanced', 0);
//  SetShaderPointer('instPos', 3, 0, @NullVector);
//  glVertexAttribDivisor(attrLoc, 1);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
end;

procedure TModel3D.DrawIndexArray(Mode: GLint; ia: TIntegerArray);
begin
  if ia.Count = 0 then
    Exit;
  glDrawArrays(Mode, ia[0], ia.Count);
end;

procedure TModel3D.Draw;
var
  i, k: Integer;
  Mesh: TModelMesh;
begin
  if Self = nil then
    Exit;
  SetShaderPointer('vpos', 3, 0, Vertices[0]);
  SetShaderPointer('vtex', 2, 0, TexCoords[0]);
  SetShaderPointer('vnorm', 3, 0, Normals[0]);
  for k := 0{DebugIndex} to {DebugIndex{ }Meshes.Count-1{} do begin
    Mesh := Meshes[k];
    if Mesh.TurnedOff then
      Continue;
    TTextureManager.SwitchTo(MtlStyles.Values[mesh.Material]);
    for i := 0 to Mesh.Polys.Count-1 do
      DrawIndexArray(GL_TRIANGLE_FAN, Mesh.Polys[i]);
    DrawIndexArray(GL_TRIANGLES, Mesh.Triangles);
  end;
end;

function TModel3D.MeshByName(const s: String): TModelMesh;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Meshes.Count-1 do
    if Meshes[i].Name = s then
      Result := Meshes[i];
end;

procedure TModel3D.TurnMeshes(Flags: Cardinal);
var
  i: Integer;
begin
  for i := 0 to Meshes.Count-1 do
    Meshes[i].TurnedOff := Flags and (1 shl i) = 0;      
end;

procedure OutputDebug(const s: string);
begin
//  OutputDebugString(PWideChar(StringReplace(s, #13#10, '', [rfReplaceAll])));
end;

function TModel3D.AbsSize: TGLVectorf2;
var
  k: Integer;
begin
  Result[0] := 0;
  for k := 0 to Vertices.Count-1 do begin
    if Abs(Vertices[k][0]) > result[0] then
      Result[0] := Abs(Vertices[k][0]);
  end;
end;

function TModel3D.AddMesh;
begin
  Result := TModelMesh.Create;
  Meshes.Add(Result);
end;

function TModel3D.BoneByName(s: PAnsiChar): TModelBone;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Bones.Count-1 do
    if Bones[i].Name = s then
      Result := Bones[i];
  if (Result = nil) and (s <> '') then begin
    Result := TModelBone.Create;
    Bones.Add(Result);
    Result.Name := s;
  end;
end;

procedure TModelMesh.AddPoly(out poly: TIntegerArray);
begin
  poly := TIntegerArray.Create;
  Polys.Add(poly);
end;

procedure TModel3D.LoadFromFile(const fn: string);
begin
  if ExtractFileExt(fn) = '.obj' then
    LoadFromObj(fn);
end;

{ TModelBone }

constructor TModelBone.Create;
begin
  Frames := TMatrixArray.Create;
  Weights := TWeightArray.Create;
end;

destructor TModelBone.Destroy;
begin
  FreeAndNil(Weights);
  FreeAndNil(Frames);
  inherited;
end;

function TModelBone.WorldMatrix(Frame: Integer): TMatrix;
begin
  Result := ObjectMatrix*Frames[frame]^;
  Point := Result * NullVector;
end;

{ TIntegerArray }

function TIntegerArray.GetItems(index: Integer): Integer;
begin
  Result := integer(inherited Items[index]^);
end;

{ TWeightArray }

function TWeightArray.ElementSize: Integer;
begin
  Result := SizeOf(TWeight);
end;

function TWeightArray.GetItems(index: Integer): TWeight;
begin
  Result := TWeight(inherited Items[index]^);
end;

{ TBoneArray }

function TBoneArray.ElementSize: Integer;
begin
  Result := SizeOf(TModelBone);
end;

function TBoneArray.GetItems(index: Integer): TModelBone;
begin
  Result := TModelBone(inherited items[index]^);
end;

{ TFloat2Array }

function TFloat2Array.ElementSize: Integer;
begin
  Result := SizeOf(Tglvectorf2);
end;

function TFloat2Array.GetItems(index: Integer): PVector2f;
begin
  Result := PVector2f(inherited Items[index]);
end;

{ TFloat3Array }

function TFloat3Array.ElementSize: Integer;
begin
  Result := SizeOf(TVector3f);
end;

function TFloat3Array.GetItems(index: Integer): PVector3f;
begin
  Result := PVector3f(inherited Items[index]);
end;

{ TVectorf16Array }

function TMatrixArray.ElementSize: Integer;
begin
  Result := SizeOf(TMatrix);
end;

function TMatrixArray.GetItems(index: Integer): PMatrix;
begin
  Result := PMatrix(inherited Items[index]);
end;

{ TMeshArray }

function TMeshArray.GetItems(index: Integer): TModelMesh;
begin
  Result := TModelMesh(inherited Items[index]^);
end;

constructor TModelMesh.Create;
begin
  Triangles := TIntegerArray.Create;
  Animations := TMatrixArray.Create;
  Polys := TFaces.Create;
end;

destructor TModelMesh.Destroy;
begin
  FreeAndNil(Triangles);
  FreeAndNil(Animations);
  FreeAndNil(Polys);
  inherited;
end;

{ TFaces }

function TFaces.GetItems(index: Integer): TIntegerArray;
begin
  Result := TIntegerArray(inherited GetItems(index)^);
end;

end.
