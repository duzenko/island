unit Shadows;

interface uses
  Windows, dglOpengl, Dialogs, Vectors, Forms, Khrono;

procedure DrawShadow;

var
  ShadowMatrix: TMatrix;

implementation uses
  gfxrender, graphics, shaders, unit1, Terrain;

var
  FramebufferName: TGLuint = 0;
  depthTexture: TGLuint = 0;
const
  ShadowMapSize = 4096;

procedure Init;
begin
  glGenFramebuffers(1, @FramebufferName);
  glGenTextures(1, @depthTexture);
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, depthTexture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, ShadowMapSize, ShadowMapSize,
    0, GL_DEPTH_COMPONENT, GL_SHORT, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC, GL_LEQUAL);
  glActiveTexture(GL_TEXTURE0);
  glCheckError;
end;

procedure Ortho(s: Single);
var
  M: TMatrix;
begin
  M := IdentityMatrix;
  M[0, 0] := 1/s;
  M[1, 1] := 1/s;
  M[2, 2] := -1/s;
  M[3, 3] := 1;
  glMultMatrixf(M);
end;

procedure DrawShadow;
var
  bmp: TBitmap;
  m_viewport: TRect;
const
  s = 144;
begin
  if FramebufferName = 0 then
    Init;
  glBindFramebuffer(GL_FRAMEBUFFER, FramebufferName);
  glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, depthTexture, 0);
  glGetIntegerv(GL_VIEWPORT, @m_viewport);
  glViewport(0, 0, ShadowMapSize, ShadowMapSize);
  glClear(GL_DEPTH_BUFFER_BIT);
  MatrixMode(false);
  LoadIdentity;
  Ortho(s);
  glRotatef(ShadowAngles[0], 1, 0, 0);
  glRotatef(-ShadowAngles[1], 0, 0, 1);
  glTranslatef(0, 0, -GetHeight(0, 0));
  GetCurrentMatrix(ShadowMatrix);
  SetShaderMatrix('shadowMatrix', ShadowMatrix);
  MatrixMode(true);
  LoadIdentity;

  glEnable(GL_CULL_FACE);
  glCullFace(GL_FRONT);
  RenderScene;
  glCullFace(GL_BACK);

  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glViewport(0, 0, m_viewport.Width, m_viewport.Height);
  if Form1.Image1.Visible then begin
    bmp := TBitmap.Create;
    with bmp do try
      PixelFormat := pf16bit;
      Width := ShadowMapSize;
      Height := ShadowMapSize;
      glBindTexture(GL_TEXTURE_2D, depthTexture);
      glGetTexImage(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, GL_SHORT, bmp.ScanLine[bmp.Height-1]);
      Form1.Image1.Picture.Graphic := bmp;
    finally
      Free;
    end;
  end;
  glCheckError;
end;

end.
