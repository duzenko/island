unit shaders;

interface uses
  SysUtils, Classes, dglOpengl, Dialogs, Vectors, Forms, Contnrs;

function LoadGpuProgram(const Name: string): TGLUInt;

procedure glTranslatef(x, y, z: Single);
procedure glScalef(f: Single); overload;
procedure glScalef(x, y, z: Single); overload;
procedure LoadIdentity;
procedure glPushMatrix;
procedure glPopMatrix;
procedure MatrixMode(model: Boolean);
procedure glMultMatrixf(const M: TMatrix);
procedure Frustum(fov, aspect, near: Single);
procedure GetCurrentMatrix(out m: TMatrix);
procedure SetShaderMatrix(const varName: AnsiString; const m: TMatrix);
function  SetShaderPointer(const name: AnsiString; c, s: Integer; p: Pointer): Integer;
procedure SetShaderFloat(const name: AnsiString; f: Single);
procedure SetShaderVec3(const name: AnsiString; v: PGLfloat);
procedure glRotatef(angle: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat);
procedure glCheckError;
procedure SwitchProgram(prg: TGLuint);

var
  prgTerrain, prgObjects: TGLUInt;

implementation

type
  TMatrixStack = class(TStack)
  private
  public
    destructor Destroy; override;
    function Peek: PMatrix;
  end;

var
  CurMatrixMode: Boolean;
  modelStack, viewProjectionStack: TMatrixStack;
  ActiveProgram: TGLuint;

function MatrixStack: TMatrixStack;
begin
  if CurMatrixMode then
    Result := modelStack
  else
    Result := viewProjectionStack;
end;

procedure UpdateShaderMatrix;
begin
  if ActiveProgram = 0 then
    Exit;
  if CurMatrixMode then
    SetShaderMatrix('modelMatrix', MatrixStack.Peek^)
  else
    SetShaderMatrix('viewProjectionMatrix', MatrixStack.Peek^);
end;

procedure SwitchProgram(prg: TGLuint);
begin
  ActiveProgram := prg;
  glUseProgram(prg);
  CurMatrixMode := false;
  UpdateShaderMatrix;
  CurMatrixMode := True;
  UpdateShaderMatrix;
end;

procedure glCheckError;
var
  errCode: Cardinal;
begin
  errCode := glGetError;
  if errCode <> 0 then begin
    MessageDlg(string(gluErrorString(errCode)), mtError, [mbok], 0);
    Application.Terminate;
  end;
end;

procedure MatrixMode(model: Boolean);
begin
  CurMatrixMode := model;
end;

procedure SetShaderFloat(const name: AnsiString; f: Single);
var
  uniLoc: Integer;
begin
  if ActiveProgram = 0 then
    Exit;
  uniLoc := glGetUniformLocation(ActiveProgram, PAnsiChar(name));
  glUniform1f(uniLoc, f);
  glCheckError;
end;

procedure SetShaderVec3(const name: AnsiString; v: PGLfloat);
var
  uniLoc: Integer;
begin
  if ActiveProgram = 0 then
    Exit;
  uniLoc := glGetUniformLocation(ActiveProgram, PAnsiChar(name));
  glUniform3fv(uniLoc, 1, v);
  glCheckError;
end;

function SetShaderPointer(const name: AnsiString; c, s: Integer; p: Pointer): Integer;
var
  attrLoc: Integer absolute Result;
begin
  attrLoc := glGetAttribLocation(ActiveProgram, PAnsiChar(name));
  if attrLoc < 0 then
    Exit;
//  if p = nil then
//    glDisableVertexAttribArray(attrLoc)
//  else begin
    glEnableVertexAttribArray(attrLoc);
    glVertexAttribPointer(attrLoc, c, GL_FLOAT, GL_FALSE, s, p);
//  end;
end;

procedure SetShaderMatrix(const varName: AnsiString; const m: TMatrix);
var
  texLoc: Integer;
begin
  texLoc := glGetUniformLocation(ActiveProgram, PAnsiChar(varName));
  glUniformMatrix4fv(texLoc, 1, GL_FALSE, @M);
  glCheckError;
end;

procedure GetCurrentMatrix(out m: TMatrix);
begin
  m := MatrixStack.Peek^;
end;

procedure glRotatef(angle: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat);
begin
  MatrixStack.Peek.Rotate(angle, x, y, z);
  UpdateShaderMatrix;
end;

procedure Frustum(fov, aspect, near: Single);
var
  m: TMatrix;
begin
  m := IdentityMatrix;
  m.M[0, 0] := 1/(fov*aspect);
  m.M[1, 1] := 1/fov;
  m.M[2, 2] := 0;
  m.M[3, 2] := -near;
  m.M[2, 3] := -1;
  m.M[3, 3] := -near;
  MatrixStack.Peek^ := MatrixStack.Peek^ * m;
  UpdateShaderMatrix;
end;

procedure LoadIdentity;
begin
  MatrixStack.Peek^ := IdentityMatrix;
  UpdateShaderMatrix;
end;

procedure glTranslatef(x, y, z: Single);
begin
  MatrixStack.Peek.Translate(x, y, z);
  UpdateShaderMatrix;
end;

procedure glScalef(f: Single);
begin
  glScalef(f, f, f);
end;

procedure glScalef(x, y, z: Single);
begin
  MatrixStack.Peek.Scale(x, y, z);
  UpdateShaderMatrix;
end;

procedure glPushMatrix;
var
  M: PMatrix;
begin
  New(M);
  if MatrixStack.Count > 0 then
    M^ := MatrixStack.Peek^;
  MatrixStack.Push(M);
//  UpdateShaderMatrix;
end;

procedure glPopMatrix;
begin
  Dispose(MatrixStack.Pop);
  if MatrixStack.Count > 0 then
    UpdateShaderMatrix;
end;

procedure glMultMatrixf(const M: TMatrix);
begin
  MatrixStack.Peek^ := MatrixStack.Peek^ * M;
  UpdateShaderMatrix;
end;

function GetInfoLog(glObject : glHandle) : AnsiString;
var
 blen : GLInt;
 slen : GLSizei;
 InfoLog   : PGLChar;
begin
  glGetShaderiv(glObject, GL_INFO_LOG_LENGTH , @blen);
  if blen > 1 then
    begin
      GetMem(InfoLog, blen*SizeOf(PGLCharARB));
      glGetShaderInfoLog(glObject, blen, @slen, InfoLog);
      Result := PAnsiChar(InfoLog);
      Dispose(InfoLog);
    end;
end;

function GetProgramInfoLog(glObject : glHandle) : AnsiString;
var
 blen : GLInt;
 slen : GLsizei;
 InfoLog   : PGLCharARB;
begin
  glGetProgramiv(glObject, GL_INFO_LOG_LENGTH , @blen);
  if blen > 1 then
    begin
      GetMem(InfoLog, blen*SizeOf(PGLCharARB));
      glGetProgramInfoLog(glObject, blen, @slen, InfoLog);
      Result := PAnsiChar(InfoLog);
      Dispose(InfoLog);
    end;
end;

function LoadGpuProgram(const Name: string): TGLUInt;
var
	VP, FP: TGLUInt;
  texLoc, SLen: Integer;
  RValue : TGLUInt;
  ShaderLog, VertexProgram, FragmentProgram: AnsiString;
begin
	Result := glCreateProgram;

	VP := glCreateShader(GL_VERTEX_SHADER);
//	GP := glCreateShader(GL_GEOMETRY_SHADER);
  FP := glCreateShader(GL_FRAGMENT_SHADER);

  with TStringList.Create() do try
    LoadFromFile('..\shaders\' + Name + '.vs');
    VertexProgram := ansistring(text);
//    LoadFromFile('..\shaders\' + Name + '.gs');
//    GeometryProgram := ansistring(text);
    LoadFromFile('..\shaders\' + Name + '.fs');
    FragmentProgram := ansistring(text);
  finally
    Free;
  end;
  SLen := Length(VertexProgram);
  glShaderSource(VP, 1, @VertexProgram, @SLen);

//  SLen := Length(GeometryProgram);
//  glShaderSource(GP, 1, @GeometryProgram, @SLen);

  SLen := Length(FragmentProgram);
  glShaderSource(FP, 1, @FragmentProgram, @SLen);

  glCompileShader(VP);
  glGetShaderiv(VP, GL_COMPILE_STATUS, @RValue);
  if RValue = 0 then
  	begin
    	ShaderLog := GetInfoLog(VP);
			MessageDlg(string(ShaderLog), mtError, [mbOK], 0);
      exit;
    end;
  glAttachShader(Result, VP);

{  glCompileShader(GP);
  glGetShaderiv(GP, GL_COMPILE_STATUS, @Rvalue);
  if RValue = 0 then
  	begin
    	ShaderLog := GetInfoLog(GP);
			MessageDlg(string(ShaderLog), mtError, [mbOK], 0);
      Application.Terminate;
      Result := 0;
      exit;
    end;
  glAttachShader(Result, GP);}

  glCompileShader(FP);
  glGetShaderiv(FP, GL_COMPILE_STATUS, @Rvalue);
  if RValue = 0 then
  	begin
    	ShaderLog := GetInfoLog(FP);
			MessageDlg(string(ShaderLog), mtError, [mbOK], 0);
      Application.Terminate;
      Result := 0;
      exit;
    end;
  glAttachShader(Result, FP);

	glBindFragDataLocation(Result, 0, 'color');
  glLinkProgram(Result);

  glGetProgramiv(Result, GL_LINK_STATUS, @Rvalue);
  if RValue = 0 then
  	begin
    	ShaderLog := GetProgramInfoLog(Result);
			MessageDlg(string(ShaderLog), mtError, [mbOK], 0);
      exit;
    end;
	SwitchProgram(Result);
  texLoc := glGetUniformLocation(Result, 'material');
  glUniform1i(texLoc, 0);
  texLoc := glGetUniformLocation(Result, 'shadow');
  glUniform1i(texLoc, 1);
  texLoc := glGetUniformLocation(Result, 'heights');
  glUniform1i(texLoc, 2);
  texLoc := glGetUniformLocation(Result, 'material2');
  glUniform1i(texLoc, 3);
end;

{ TMatrixStack }

destructor TMatrixStack.Destroy;
begin
  while Count > 0 do
    Dispose(Pop);
  inherited;
end;

function TMatrixStack.Peek: PMatrix;
begin
  Result := inherited Peek;
end;

initialization
  viewProjectionStack := TMatrixStack.Create();
  glPushMatrix;
  LoadIdentity;
  modelStack := TMatrixStack.Create();
  MatrixMode(true);
  glPushMatrix;
  LoadIdentity;

finalization
  FreeAndNil(modelStack);
  FreeAndNil(viewProjectionStack);

end.
