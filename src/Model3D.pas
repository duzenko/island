unit Model3D;

interface uses
  Windows, SysUtils, Classes, Generics.Collections, dglOpengl, MSXML2_TLB, 
  vectors, Geometry;

type
  TFastArray<T> = record
  type P = ^T;
  private
    function GetData(i: Integer): P;
  public
    Count: Integer;
    FData: array of T;
    function Add: P; overload;
    procedure Add(const v: T); overload;
    procedure SetSize(Size: Integer);
    procedure SetMinSize(Size: Integer);
    property Data[i:Integer]: P read GetData; default;
  end;
  TIndexArray = TFastArray<Integer>;
  PIndexArray = ^TIndexArray;
  TVectorf16 = array[0..15] of Single;
  TVectorf16Array = TFastArray<TVectorf16>;
  TFaces = TFastArray<TIndexArray>;

  TModelMesh = record
    Name, Material: String;
    TurnedOff, HasBones: Boolean;
    Polys: TFaces;
    Triangles: TIndexArray;
    Animations: TVectorf16Array;
    procedure AddPoly(out poly: PIndexArray);
//    procedure AddFace(poly: PIndexArray; s1, s2, s3: PAnsiChar);
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

  TModel3D = class
  protected
    function AddMesh: PModelMesh;
    function MeshByName(const s: String): PModelMesh;
    function BoneByName(s: PAnsiChar): TBoneArray.P;
    procedure CalcSkin(frame: Integer);
    procedure LoadFromFile(const fn: string);
    procedure DrawIndexArray(Mode: GLint; const ia: TIndexArray);
  public
    TexCoords: TFastArray<TGLVectorf2>;
    Vertices, Normals: TFastArray<TGLVectorf3>;
    VerticesSkinned: array of record v: TGLVectorf3; used: Boolean end;
    Meshes: array of TModelMesh;
    Bones: TBoneArray;
    MtlStyles: TStringList;

    constructor Create(const fn: string);
    destructor Destroy; override;

    procedure Draw; overload;
    procedure Draw(frame: Integer); overload;
    procedure TurnMeshes(Flags: Cardinal);
    function AbsSize: TGLVectorf2;
  const {$J+}
    DebugDraw: Boolean = true;
    DebugIndex: Integer = 211;
  end;

implementation uses
  Math, Character, AnsiStrings, ActiveX, ObjLoader, TextureManager;

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

procedure TFastArray<T>.SetMinSize(Size: Integer);
begin
  if Size > Count then
    SetSize(Size);
end;

procedure TFastArray<T>.SetSize(Size: Integer);
begin
  Count := Size;
  SetLength(FData, Size);
end;

{ T3DModel }

constructor TModel3D.Create(const fn: string);
begin
  MtlStyles := TStringList.Create;
  LoadFromFile(fn);
end;

destructor TModel3D.Destroy;
begin
  inherited;
  FreeAndNil(MtlStyles);
end;

procedure TModel3D.CalcSkin(frame: Integer);
var
  i, j, vi: Integer;
  mpw, mpwi, mt: TMatrix;
  v1: TGLVectorf3;
begin
  SetLength(VerticesSkinned, 0);
  SetLength(VerticesSkinned, Vertices.Count);
  for i := 0 to Bones.Count-1 do begin
      mpwi := Bones[i].WorldMatrix(0);
      MatrixInvert(mpwi);
      mpw := Bones[i].WorldMatrix(frame);
      mt := MatrixMultiply(mpwi, mpw);
    for j := 0 to Bones[i].Weights.Count-1 do begin
      vi := Bones[i].Weights[j].VertexIndex;
      v1 := TGLVectorf3(VectorTransform(TAffineVector(pointer(Vertices[vi])^), mt));
      VectorScale(v1, Bones[i].Weights[j].Weight);
      VerticesSkinned[vi].v[0] := VerticesSkinned[vi].v[0] + v1[0];
      VerticesSkinned[vi].v[1] := VerticesSkinned[vi].v[1] + v1[1];
      VerticesSkinned[vi].v[2] := VerticesSkinned[vi].v[2] + v1[2];
      VerticesSkinned[vi].used := true;
    end;
  end;
end;

procedure TModel3D.Draw(frame: Integer);
var
  i, j, k: Integer;
  vi3: Integer;
//  vi3: TIndexVector;
  Mesh: PModelMesh;
  Faces: TFaces.P;
begin
  if Self = nil then
    Exit;
  CalcSkin(frame);
  for k := 0{DebugIndex} to {DebugIndex{ }High(Meshes){} do begin
    Mesh := @Meshes[k];
    if Mesh.TurnedOff then
      Continue;
    TTextureManager.SwitchTo(MtlStyles.Values[mesh.Material]);
    for i := 0 to Mesh.Polys.Count-1 do begin
      if frame >= Mesh.Animations.Count then
        Continue;
      Faces := Mesh.Polys[i];
      glPushMatrix;
      glMultMatrixf(pointer(Mesh.Animations[frame]));
      glBegin(GL_TRIANGLE_FAN);
      for j := 0 to Faces.Count-1 do begin
        vi3 := Faces.FData[j];
        glNormal3fv(pointer(Normals[vi3]));
        glTexCoord2fv(pointer(texCoords[vi3]));
        if VerticesSkinned[vi3].used then
          glVertex3fv(@VerticesSkinned[vi3].v)
        else
          glVertex3fv(pointer(Vertices[vi3]));
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

procedure TModel3D.DrawIndexArray(Mode: GLint; const ia: TIndexArray);
begin
  glDrawElements(Mode, ia.Count, GL_UNSIGNED_INT, ia.Data[0]);
//      glBegin(GL_TRIANGLE_FAN);
//      for j := 0 to Faces.Count-1 do begin
//        vi3 := Faces.Data[j]^;
//        glNormal3fv(pointer(Normals[vi3]));
//        glTexCoord2fv(pointer(texCoords[vi3]));
//        glVertex3fv(pointer(Vertices[vi3]));
//      end;
//      glEnd;
end;

procedure TModel3D.Draw;
var
  i, k: Integer;
  Mesh: PModelMesh;
begin
  if Self = nil then
    Exit;
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glVertexPointer(3, GL_FLOAT, 0, Vertices.Data[0]);
  glNormalPointer(GL_FLOAT, 0, Normals.Data[0]);
    glTexCoordPointer(2, GL_FLOAT, 0, TexCoords.Data[0]);
  for k := 0{DebugIndex} to {DebugIndex{ }High(Meshes){} do begin
    Mesh := @Meshes[k];
    if Mesh.TurnedOff then
      Continue;
    TTextureManager.SwitchTo(MtlStyles.Values[mesh.Material]);
    for i := 0 to Mesh.Polys.Count-1 do
      DrawIndexArray(GL_TRIANGLE_FAN, Mesh.Polys[i]^);
    DrawIndexArray(GL_TRIANGLES, Mesh.Triangles);
  end;
end;

function TModel3D.MeshByName(const s: String): PModelMesh;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to High(Meshes) do
    if Meshes[i].Name = s then
      Result := @Meshes[i];
end;

procedure TModel3D.TurnMeshes(Flags: Cardinal);
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
    SetLength(Meshes, Length(Meshes)+1);
    Result := @Meshes[High(Meshes)];
  end;

function TModel3D.BoneByName(s: PAnsiChar): TBoneArray.P;
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

procedure TModelMesh.AddPoly(out poly: PIndexArray);
begin
  Poly := pointer(Polys.Add);
end;

//procedure TModelMesh.AddFace(poly: PIndexArray; s1, s2, s3: PAnsiChar);
//var
//  v: TIndexVector;
//begin
//  v[0] := ValLong(s1);
//  v[1] := ValLong(s2);
//  v[2] := ValLong(s3);
//  poly.Add(v);
//end;

procedure TModel3D.LoadFromFile(const fn: string);
begin
  if ExtractFileExt(fn) = '.obj' then
    LoadFromObj(fn);
end;

{ TModelBone }

function TModelBone.WorldMatrix(Frame: Integer): TMatrix;
begin
  Result := MatrixMultiply(TMatrix(Frames[frame]^), ObjectMatrix);
  Point := TGLVectorf3(VectorTransform(NullVector, Result));
end;

end.
