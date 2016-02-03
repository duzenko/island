unit Unit7;

interface uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  gfxrender, dglOpenGL, Vcl.ExtCtrls;

type
  TForm7 = class(TForm)
    Timer1: TTimer;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FMousePoint: TPoint;
    pcf: Int64;
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation uses
  Math, Unit1, Khrono, Model3D;

{$R *.dfm}

procedure TForm7.FormCreate(Sender: TObject);
begin
  Cursor := crNone;
  QueryPerformanceFrequency(pcf);
end;

procedure TForm7.FormDestroy(Sender: TObject);
begin
  DeactivateRenderingContext;
  DestroyRenderingContext(glRC);
end;

procedure TForm7.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
  VK_ESCAPE:  Close;
  VK_F2:      glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );
  VK_F3:      glPolygonMode( GL_FRONT_AND_BACK, GL_FILL );
  VK_F4:      TModel3D.DebugDraw := not TModel3D.DebugDraw;
  VK_F5:
  begin
    Mouse.CursorPos := ClientToScreen(FMousePoint);
    Cursor := crNone;
  end;
  VK_F6:      Cursor := crDefault;
  VK_LEFT:    Dec(TModel3D.DebugIndex);
  VK_RIGHT:   Inc(TModel3D.DebugIndex);
  VK_SPACE:   Khrono.Paused := not Khrono.Paused;
  end;
end;

procedure TForm7.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
const
  cSens = 0.3;
begin
  if Cursor <> crNone then
    Exit;
  if (FMousePoint.X = X) and (FMousePoint.Y = Y) then
    Exit;
  CameraLook.x := CameraLook.x + cSens*(FMousePoint.X - X);
  CameraLook.y := Min(0, Max(-180, CameraLook.y + cSens*(FMousePoint.Y - Y)));
  gfxrender.CameraMoved;
  Mouse.CursorPos := ClientToScreen(FMousePoint);
//  FormPaint(nil);
end;

procedure TForm7.FormResize(Sender: TObject);
begin
  if glRC = 0 then begin
    Khrono.Init;
    glRC := CreateRenderingContext(Canvas.Handle, [opDoubleBuffered], 32, 32, 0, 0, 0, 0);
    ActivateRenderingContext(Canvas.Handle, glRC);
    ReadOpenGLCore;
  end;
  gfxrender.AspectRatio := ClientWidth/ClientHeight;
  FMousePoint := Point(ClientWidth div 2, ClientHeight div 2);
  if Cursor = crNone then
    Mouse.CursorPos := ClientToScreen(FMousePoint);
  glViewport(0, 0, ClientWidth, ClientHeight);
end;

procedure TForm7.Timer1Timer(Sender: TObject);
var
  pc1, pc2: Int64;
begin
  Timer1.Enabled := false;
  QueryPerformanceCounter(pc1);
  Khrono.UISync;
  gfxrender.Render;
  SwapBuffers(Canvas.Handle);
  QueryPerformanceCounter(pc2);
  Form1.FrameTime := (pc2-pc1)*1000 div pcf;
//  FormPaint(nil);
  Timer1.Enabled := true;
end;

end.
