unit Shadows;

interface uses
  Windows, dglOpengl, Dialogs, Vectors;

procedure DrawShadow;

var
  ShadowMatrix: TMatrix;

implementation uses
  gfxrender, graphics, shaders, unit1;

var
  FramebufferName: TGLuint = 0;
  depthTexture: TGLuint = 0;

procedure CheckError;
var
  errCode: Cardinal;
begin
  errCode := glGetError;
  if errCode <> 0 then
    MessageDlg(string(gluErrorString(errCode)), mtError, [mbok], 0);
end;

procedure Init;
begin
  glGenFramebuffers(1, @FramebufferName);
  glGenTextures(1, @depthTexture);
  glBindTexture(GL_TEXTURE_2D, depthTexture);
//  glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT16, 1024, 1024, 0, GL_DEPTH_COMPONENT, GL_FLOAT, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
//  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
//  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
//  glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, depthTexture, 0);
//  CheckError;
//  if(glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE) then
//    ;
end;

procedure DrawShadow;
var
  m_viewport: TRect;
  bmp: TBitmap;
const
  s = 14;
begin
  if FramebufferName = 0 then
    Init;
//  glDrawBuffer(GL_NONE); // No color buffer is drawn to.
//  glBindFramebuffer(GL_FRAMEBUFFER, FramebufferName);
//  glDepthMask(false);
  LoadIdentity;
  Ortho(s, s, 99);
  glRotatef(180, 0, 0, 1);
  glRotatef(45, 1, 0, 0);
  glTranslatef(0, -s, -s);
  RenderScene;
  bmp := TBitmap.Create;
  with bmp do try
    PixelFormat := pf16bit;
    glGetIntegerv( GL_VIEWPORT, @m_viewport);
    Width := m_viewport.Width;
    Height := m_viewport.Height;
//    glReadPixels(0, 0, Width, Height, GL_RGB, GL_UNSIGNED_SHORT_5_6_5, ScanLine[Height-1]);
    glReadPixels(0, 0, Width, Height, GL_DEPTH_COMPONENT, GL_UNSIGNED_SHORT, ScanLine[Height-1]);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, depthTexture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, Width, Height, 0, GL_DEPTH_COMPONENT, GL_UNSIGNED_SHORT, ScanLine[Height-1]);
//    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, Width, Height, 0, GL_RGB, GL_UNSIGNED_SHORT_5_6_5, ScanLine[Height-1]);
    glActiveTexture(GL_TEXTURE0);
//    glReadPixels(0, 0, Width, Height, GL_RGB, GL_UNSIGNED_SHORT_5_6_5, ScanLine[Height-1]);
//    glGetTexImage(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT16, GL_UNSIGNED_SHORT, ScanLine[Height-1]);
    CheckError;
//    SaveToFile('1.bmp');
    Form1.Image1.Picture.Graphic := bmp;
  finally
    Free;
  end;
  GetCurrentMatrix(ShadowMatrix);
  SetShaderMatrix('shadowMatrix', ShadowMatrix);
  glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
//  glDepthMask(true);
//  glBindFramebuffer(GL_FRAMEBUFFER, 0);
//  glDrawBuffer(GL_BACK); // No color buffer is drawn to.
end;

end.
