unit Unit7;

interface uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs,
  gfxrender, dglOpenGL, ExtCtrls;

type
  TForm7 = class(TForm)
    Timer1: TTimer;
    procedure FormResize(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FMousePoint: TPoint;
    pcf: Int64;
    procedure CheckKeys;
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation uses
  Math, Unit1, Khrono, Model3D, TextureManager, Vectors;

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

procedure TForm7.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
const
  cSens = 0.3;
begin
  if Cursor <> crNone then
    Exit;
  if (FMousePoint.X = X) and (FMousePoint.Y = Y) then
    Exit;
  CameraLook.ax := CameraLook.ax + cSens*(FMousePoint.X - X);
  CameraLook.ay := Min(0, Max(-180, CameraLook.ay + cSens*(FMousePoint.Y - Y)));
//  gfxrender.CameraMoved;
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

procedure TForm7.CheckKeys;
var
  spd: Single;
  KeyState: array[0..255] of ShortInt;

  procedure MoveCamera(t: Single);
  var
    mv: tvector;
    s, c: Extended;
  begin
    SinCos(DegToRad(-CameraLook.ax)+t, s, c);
    mv.x := s; mv.y := c;
    CameraLook.x := CameraLook.x + spd*mv.x;
    CameraLook.y := CameraLook.y + spd*mv.y;
  end;

begin
  GetKeyboardState(TKeyboardState(KeyState));
  spd := 0.3;
  if KeyState[VK_CONTROL] < 0 then
    spd := spd * 3;
  if KeyState[VK_SHIFT] < 0 then
    spd := spd * 3;
  if KeyState[VK_MENU] < 0 then
    spd := spd * 3;
  if KeyState[VK_ESCAPE] < 0 then
    Close;
  if KeyState[VK_F2] < 0 then
    glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );
  if KeyState[VK_F3] < 0 then
    glPolygonMode( GL_FRONT_AND_BACK, GL_FILL );
  if KeyState[VK_F4] < 0 then
    TModel3D.DebugDraw := not TModel3D.DebugDraw;
  if KeyState[VK_F5] < 0 then
  begin
    Mouse.CursorPos := ClientToScreen(FMousePoint);
    Cursor := crNone;
  end;
  if KeyState[VK_F6] < 0 then
    Cursor := crDefault;

  if KeyState[Ord('W')] < 0 then
    MoveCamera(0);
  if KeyState[Ord('S')] < 0 then
    MoveCamera(Pi);
  if KeyState[Ord('A')] < 0 then
    MoveCamera(-Pi/2);
  if KeyState[Ord('D')] < 0 then
    MoveCamera(+Pi/2);
  if KeyState[VK_SPACE] < 0 then
    Khrono.Paused := not Khrono.Paused;
  if KeyState[VK_PRIOR] < 0 then
    CameraLook.z := CameraLook.z + spd;
  if KeyState[VK_NEXT] < 0 then
    CameraLook.z := CameraLook.z - spd;
end;

var
  TotalTime: Int64;
procedure TForm7.Timer1Timer(Sender: TObject);
var
  pc2: Int64;
const {$J+}
  pc1: Int64 = 0;
begin
  Timer1.Enabled := false;
  if pc1 = 0 then begin
    QueryPerformanceCounter(pc1);
  end;
  CheckKeys;
  Khrono.UISync;
  gfxrender.Render;
//  Application.ProcessMessages;
  SwapBuffers(Canvas.Handle);
  QueryPerformanceCounter(pc2);
  Form1.FrameTime := (pc2-pc1)*1000/pcf;
  pc1 := pc2;
  Inc(TotalTime, Round(Form1.FrameTime));
  with Form1.CheckListBox1{, TTextureManager.ToLoad }do begin
    if TotalTime < 5000 then
      Items.Add(Format('%5d %5.0f', [TotalTime, Form1.FrameTime]));
//    UnlockList;
  end;
  Form1.Timer1Timer(nil);
//  FormPaint(nil);
  Timer1.Enabled := true;
end;

end.
