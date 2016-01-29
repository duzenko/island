unit shaders;

interface uses
  SysUtils, Classes, dglOpengl, Dialogs, Vectors, Generics.Collections;

function GenerateRenderPrograms: TGLUInt;
procedure glTranslatef(x, y, z: Single);
procedure glScalef(f: Single); overload;
procedure glScalef(x, y, z: Single); overload;
procedure LoadIdentity;
procedure glPushMatrix;
procedure glPopMatrix;
procedure glMultMatrixf(const M: TMatrix);
procedure Frustum(left: GLdouble; right: GLdouble; bottom: GLdouble; top: GLdouble; zNear: GLdouble; zFar: GLdouble);
procedure glRotatef(angle: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat);

implementation

type
  TMatrixStack = class(TStack<PMatrix>)
  end;

var
  program_id: TGLUInt;
  MatrixStack: TMatrixStack;

procedure UpdateShaderMatrix;
var
  MatrixID: GLuint;
  M: TMatrix;
const {$J+}
  t: TVector = ();
begin
  if program_id = 0 then
    Exit;
  MatrixID := glGetUniformLocation(program_id, 'mvp');
  M := MatrixStack.Peek^;
  t := MatrixStack.Peek^ * NullVector;
//  t := MatrixStack.Peek^.Transpose * NullVector;
  glUniformMatrix4fv(MatrixID, 1, GL_FALSE, @M);
end;

procedure glRotatef(angle: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat);
begin
  MatrixStack.Peek.Rotate(angle, x, y, z);
  UpdateShaderMatrix;
end;

procedure Frustum(left: GLdouble; right: GLdouble; bottom: GLdouble; top: GLdouble; zNear: GLdouble; zFar: GLdouble);
var
  m: TMatrix;
begin
  m := IdentityMatrix;
  m.M[2, 2] := 0;
  m.M[2, 3] := 1;
  m.M[3, 2] := -1;
{  m.M[2, 2] := -10.1/9.9;
  m.M[3, 2] := -1;
  m.M[2, 3] := -2/9.9;}

  m.M[3, 3] := 0;
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
end;

procedure glScalef(x, y, z: Single);
begin

end;

procedure glPushMatrix;
var
  M: PMatrix;
begin
  New(M);
  MatrixStack.Push(M);
end;

procedure glPopMatrix;
begin
  Dispose(MatrixStack.Pop);
end;

procedure glMultMatrixf(const M: TMatrix);
begin
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

function GenerateRenderPrograms: TGLUInt;
var
	VP, FP : TGLUInt;
  SLen: Integer;
  RValue : TGLUInt;
  ShaderLog, VertexProgram, FragmentProgram: AnsiString;
begin
	Result := glCreateProgram;

	VP := glCreateShader(GL_VERTEX_SHADER);
  FP := glCreateShader(GL_FRAGMENT_SHADER);

  with TStringList.Create() do try
    LoadFromFile('..\shaders\vertex.txt');
    VertexProgram := ansistring(text);
    LoadFromFile('..\shaders\fragment.txt');
    FragmentProgram := ansistring(text);
  finally
    Free;
  end;
  SLen := Length(VertexProgram);
  glShaderSource(VP, 1, @VertexProgram, @SLen);

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

  glCompileShader(FP);
  glGetShaderiv(FP, GL_COMPILE_STATUS, @Rvalue);
  if RValue = 0 then
  	begin
    	ShaderLog := GetInfoLog(FP);
			MessageDlg(string(ShaderLog), mtError, [mbOK], 0);
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

	glUseProgram(Result);
  program_id := Result;
end;

initialization
  MatrixStack := TMatrixStack.Create();
  glPushMatrix;
  LoadIdentity;

finalization
  glPopMatrix;
  FreeAndNil(MatrixStack);

end.
