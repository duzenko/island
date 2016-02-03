unit Shadows;

interface uses
  Windows, dglOpengl, Dialogs, Vectors, Forms;

procedure DrawShadow;

implementation uses
  gfxrender, graphics, shaders, unit1;

var
  FramebufferName: TGLuint = 0;
  depthTexture: TGLuint = 0;
  ShadowMatrix: TMatrix;
const
  ShadowMapSize = 2048;

procedure CheckError;
var
  errCode: Cardinal;
begin
  errCode := glGetError;
  if errCode <> 0 then begin
    MessageDlg(string(gluErrorString(errCode)), mtError, [mbok], 0);
    Application.Terminate;
  end;
end;

procedure Init;
begin
  glGenFramebuffers(1, @FramebufferName);
  glGenTextures(1, @depthTexture);
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, depthTexture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT16, ShadowMapSize, ShadowMapSize, 0, GL_DEPTH_COMPONENT, GL_FLOAT, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glActiveTexture(GL_TEXTURE0);
  CheckError;
//  if(glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE) then
//    ;
end;

procedure ShadowProj(s: Single);
begin
  ShadowMatrix := IdentityMatrix;
  ShadowMatrix[0, 0] := -1/s;
  ShadowMatrix[1, 1] := -1/s;
  ShadowMatrix[2, 1] := 1/s;
  ShadowMatrix[1, 2] := -1/s;
  ShadowMatrix[2, 2] := -1/s;
  ShadowMatrix[3, 3] := 1;
//  MatrixStack.Peek^ := MatrixStack.Peek^ * m;
//  UpdateShaderMatrix;
end;

procedure DrawShadow;
var
//  m_viewport: TRect;
  bmp: TBitmap;
  m_viewport: TRect;
const
  s = 22;
begin
  if FramebufferName = 0 then
    Init;
//  glDrawBuffer(GL_NONE); // No color buffer is drawn to.
  glBindFramebuffer(GL_FRAMEBUFFER, FramebufferName);
  glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, depthTexture, 0);
  glGetIntegerv(GL_VIEWPORT, @m_viewport);
  glViewport(0, 0, ShadowMapSize, ShadowMapSize);
  glClear(GL_DEPTH_BUFFER_BIT);
//  glDepthMask(false);
  MatrixMode(false);
  LoadIdentity;
  ShadowProj(s);
  glMultMatrixf(ShadowMatrix);
//  glRotatef(-45, 1, 0, 0);
//  glRotatef(180, 0, 0, 1);
//  glTranslatef(0, -s, -s);
  SetShaderMatrix('shadowMatrix', ShadowMatrix);
  MatrixMode(true);
  LoadIdentity;

  RenderScene;

  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glViewport(0, 0, m_viewport.Width, m_viewport.Height);
  bmp := TBitmap.Create;
  with bmp do try
    PixelFormat := pf16bit;
    Width := ShadowMapSize;
    Height := ShadowMapSize;
  glActiveTexture(GL_TEXTURE1);
    glGetTexImage(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, GL_UNSIGNED_SHORT, ScanLine[Height-1]);
  glActiveTexture(GL_TEXTURE0);
//    SaveToFile('1.bmp');
    Form1.Image1.Picture.Graphic := bmp;
  finally
    Free;
  end;
//  glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
//  glDepthMask(true);
  CheckError;
//  glDrawBuffer(GL_BACK); // No color buffer is drawn to.
end;

end.
