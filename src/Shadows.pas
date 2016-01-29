unit Shadows;

interface uses
  Windows, dglOpengl, Dialogs;

procedure DrawShadow;

implementation uses
  gfxrender, graphics, shaders;

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
  glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT16, 1024, 1024, 0, GL_DEPTH_COMPONENT, GL_FLOAT, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
//  glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, depthTexture, 0);
//  CheckError;
//  if(glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE) then
//    ;
end;

procedure DrawShadow;
var
  m_viewport: TRect;
begin
  if FramebufferName = 0 then
    Init;
  glDrawBuffer(GL_NONE); // No color buffer is drawn to.
//  glBindFramebuffer(GL_FRAMEBUFFER, FramebufferName);
//  glDepthMask(false);
//  glMatrixMode(GL_PROJECTION);
//  glLoadIdentity;
//  glOrtho(-1, 1, -0.1, 2, -2, 2);
//  glMatrixMode(GL_MODELVIEW);
  LoadIdentity;
  gluLookAt(0, 10, 10, 0, 0, 0, 0, 0, 1);
  RenderScene;
  with TBitmap.Create do try
    PixelFormat := pf16bit;
    glGetIntegerv( GL_VIEWPORT, @m_viewport);
    Width := m_viewport.Width;
    Height := m_viewport.Height;
    glReadPixels(0, 0, Width, Height, GL_DEPTH_COMPONENT, GL_UNSIGNED_SHORT, ScanLine[Height-1]);
//    glGetTexImage(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT16, GL_UNSIGNED_SHORT, ScanLine[Height-1]);
    CheckError;
    SaveToFile('1.bmp');
  finally
    Free;
  end;
  glClear(GL_DEPTH_BUFFER_BIT);
//  glDepthMask(true);
//  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glDrawBuffer(GL_BACK); // No color buffer is drawn to.
end;

end.
