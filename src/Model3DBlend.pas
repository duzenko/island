unit Model3DBlend;

interface uses
  SysUtils, Classes, System.Generics.Collections, dglOpengl, DBXJSON, JsonHelper;

type
  TModel3DObjectType = (mtArmature, mtMesh);
  TModel3DObject = class
    ObjectType: TModel3DObjectType;
    Parent: TModel3DObject;
    uid: string;
    List: TList<TModel3DObject>;
    constructor Create(const uuid: string; AList: TList<TModel3DObject>);
  end;

  TModel3DObjectArmature = class(TModel3DObject)
  end;

  TModel3DObjectMesh = class(TModel3DObject)
    Indices: array of Integer;
    Vertices, Normals: array of TGLVectorf3;
    TexCoords: array of TGLVectorf2;
    Location, Scale: TGLVectorf3;
    RotationQuaternion: TGLVectorf4;
    procedure Draw;
  end;

  TModel3dObjectList = class(TList<TModel3DObject>)
    function ByName(const AName: string): TModel3DObject;
  end;

  TModel3DBlend = class
    Objects: TModel3dObjectList;
    constructor Create(const fn: string);
    destructor Destroy; override;
    procedure LoadFromFile(const fn: string);
    procedure Draw;
  end;

implementation uses
  vectors;

{ TModel3DBlend }

constructor TModel3DBlend.Create;
begin
  Objects := TModel3dObjectList.Create;
  LoadFromFile(fn);
end;

destructor TModel3DBlend.Destroy;
begin

  inherited;
end;

procedure TModel3DBlend.Draw;
var
  k: Integer;
begin
  if Self = nil then
    Exit;
  for k := 0{DebugIndex} to {DebugIndex{ }Objects.Count-1{} do begin
    if not (Objects[k] is TModel3DObjectMesh) then
      Continue;
    TModel3DObjectMesh(Objects[k]).Draw;
  end;
end;

procedure TModel3DBlend.LoadFromFile(const fn: string);
var
  Bin: TMemoryStream;
  Offsets: record
    float, short, ushort: Integer
  end;
  Addr: record
    Offset, Count: Integer;
  end;

{  procedure AddSubMesh(submesh: TJSONObject);
//  var
//    I: Integer;
//    mesh: PModelMesh;
//    poly: PIndexArray;
  begin
    if submesh.Integers['base_length'] = 0 then
      Exit;
    mesh := AddMesh;

  end;

  procedure AddSubMeshes(submeshes: TJSONArray);
  var
    i: Integer;
  begin
    for i := 0 to submeshes.Size-1 do
      AddSubMesh(submeshes[i])
  end;

  procedure AddMesh(meshes: TJSONObject);
  begin
    AddSubmeshes(meshes.Arrays['submeshes']);
  end;    }

  procedure AddMeshes(meshes: TJSONArray);
  var
    i: Integer;
    mesh: TModel3DObjectMesh;
    submesh: TJSONObject;
  begin
    for i := 0 to meshes.Size-1 do begin
      mesh := TModel3DObjectMesh.Create(meshes[i].Strings['uuid'], Objects);
      Objects.Add(mesh);
      submesh := meshes[i].Arrays['submeshes'][0];
      submesh.IntArray('indices', @Addr);

      Bin.Position := 12 + Addr.Offset*4;
      SetLength(mesh.Indices, Addr.Count);
      Bin.Read(mesh.Indices[0], Addr.Count*SizeOf(mesh.Indices[0]));

      submesh.IntArray('texcoord', @Addr);
      SetLength(mesh.TexCoords, Addr.Count);
      Bin.Position := 12 + Offsets.float + Addr.Offset*4;
      Bin.Read(mesh.TexCoords[0], Addr.Count*SizeOf(mesh.TexCoords[0]));

      submesh.IntArray('position', @Addr);
      SetLength(mesh.Vertices, Addr.Count);
      Bin.Position := 12 + Offsets.float + Addr.Offset*4;
      Bin.Read(mesh.Vertices[0], Addr.Count*SizeOf(mesh.Vertices[0]));

      submesh.IntArray('normal', @Addr);
      SetLength(mesh.Normals, Addr.Count);
      Bin.Position := 12 + Offsets.float + Addr.Offset*4;
      Bin.Read(mesh.Normals[0], Addr.Count*SizeOf(mesh.Normals[0]));
    end;
  end;

  procedure AddObjects(objects: TJSONArray);
  var
    i: Integer;
    objectType, objid: string;
//    meshJson: TJSONObject;
    mesh: TModel3DObjectMesh;
    arm: TModel3DObjectArmature;
  begin
    for i := 0 to objects.Size-1 do begin
      objectType := objects[i].Strings['type'];
      if objectType = 'ARMATURE' then begin
        arm := TModel3DObjectArmature.Create(objects[i].Strings['uuid'], Self.Objects);
        Self.Objects.Add(arm);
      end;
      if objectType = 'MESH' then begin
        objid := objects[i]['data'].Strings['uuid'];
        mesh := TModel3DObjectMesh(Self.Objects.ByName(objid));
        if not (objects[i].Get('parent').JsonValue is TJSONNull) then begin
          objid := objects[i]['parent'].Strings['uuid'];
          mesh.Parent := Self.Objects.ByName(objid);
        end;
        objects[i].FloatArray('scale', @mesh.Scale);
        objects[i].FloatArray('location', @mesh.Location);
        objects[i].FloatArray('rotation_quaternion', @mesh.RotationQuaternion);
//        mesh.
      end;
    end;
  end;

  procedure AddBinary(binary: TJSONObject);
  begin
    bin.LoadFromFile(ExtractFilePath(fn) + binary.Strings['binfile']);
    offsets.float := binary.Integers['float'];
    offsets.short := binary.Integers['short'];
    offsets.ushort := binary.Integers['ushort'];
  end;

begin
  bin := TMemoryStream.Create;
  with TJSONObject.LoadFromFile(fn) do try
    AddBinary(Arrays['binaries'][0]);
    AddMeshes(Arrays['meshes']);
    AddObjects(Arrays['objects']);
  finally
    Free;
    bin.Free;
  end;
end;

{ TModel3DObjectMesh }

procedure TModel3DObjectMesh.Draw;
var
  i: Integer;
begin
  glPushMatrix;
  glTranslatef(location[0], location[1], location[2]);
//  glMultMatrixf(@m4f);
  glScalef(Scale[0], Scale[1], Scale[2]);
  glBegin(GL_TRIANGLES);
  for i := 0 to High(Indices) do begin
    glNormal3fv(@Normals[Indices[i]]);
    if TexCoords <> nil then
      glTexCoord2fv(@texCoords[Indices[i]]);
    glVertex3fv(@Vertices[Indices[i]]);
  end;
  glEnd;
  for i := 0 to List.Count-1 do
    if (List[i] is TModel3DObjectMesh) and (List[i].Parent = Self) then
      TModel3DObjectMesh(List[i]).draw;
  glPopMatrix;
end;

{ TModel3DObject }

constructor TModel3DObject.Create(const uuid: string; AList: TList<TModel3DObject>);
begin
  Self.uid := uuid;
  List := AList;
end;

{ TModel3dObjectList }

function TModel3dObjectList.ByName(const AName: string): TModel3DObject;
var
  obj: TModel3DObject;
begin
  obj := nil;
  for obj in Self do
  begin
    if obj.uid = AName then
      Result := obj;
  end;
end;

end.
