unit Unit1;

interface uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids,
  Vcl.ValEdit, Vcl.ComCtrls, Vcl.CheckLst, Vcl.Menus, dglOpengl, ShellApi;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    ValueListEditor1: TValueListEditor;
    TreeView1: TTreeView;
    CheckListBox1: TCheckListBox;
    MainMenu1: TMainMenu;
    Utils1: TMenuItem;
    Rendertrees1: TMenuItem;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure Rendertrees1Click(Sender: TObject);
  private
    { Private declarations }
  public
    FrameTime: Integer;
  end;

var
  Form1: TForm1;
  Dbg1: Integer;
  Dbg2: Single;

implementation uses
  Unit7, Khrono, gfxrender, Model3D, dbxjson, StrUtils, MilitiaAdventurer, shaders;

{$R *.dfm}

procedure TForm1.CheckListBox1ClickCheck(Sender: TObject);
begin
  Peasant.Model3d.Bones[CheckListBox1.ItemIndex].DebugDraw := CheckListBox1.Checked[CheckListBox1.ItemIndex];
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Left := Screen.Width - Width;
  Top := 0;
  Height := Screen.Height;
  Form7.SetBounds(0, 0, Left, Screen.Height);
//  for I := 0 to High(House.faces) do
//    TreeView1.Items.AddChild(nil, IntToStr(i));
  with TStringStream.Create do try
//    LoadFromFile('D:\temp\Militia-Adventurer-RIGGED.json');
//    LoadFromFile('D:\temp\untitled.json');
//    AddItem(TJSONObject(TJSONObject.ParseJSONValue(DataString)), nil, 'root');
//    TreeView1.Items[0].Expand(false);
  finally
    Free;
  end;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
  VK_ESCAPE:  Application.Terminate;
  end;
end;

procedure TForm1.Rendertrees1Click(Sender: TObject);
var
  BMP: TBitmap;
begin
  BMP := TBitmap.Create;
  BMP.PixelFormat := pf24bit;
  BMP.Width := Form7.ClientHeight;
  BMP.Height := Form7.ClientHeight;

  glViewport(0, 0, Form7.ClientHeight, Form7.ClientHeight);
  glClearColor(0.27, 0.4, 0.7, 0.0);//Light blue
  glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
//  glMatrixMode(GL_PROJECTION);
  LoadIdentity;
//  glOrtho(-1, 1, -0.1, 2, -2, 2);
//  glMatrixMode(GL_MODELVIEW);
  LoadIdentity;
  glEnable( GL_TEXTURE_2D);
//  TTrees.Trees.TurnMeshes(2);
  glTranslatef(0, 0, 2);
  glRotatef(-90, 1, 0, 0);
//  glScalef(2, 2, 2);
//  TTrees.Trees.Draw;
  glDisable( GL_TEXTURE_2D);
  glFinish;
  glFlush;
  glReadPixels(0, 0, Form7.ClientHeight, Form7.ClientHeight, GL_BGR, GL_UNSIGNED_BYTE, BMP.ScanLine[Form7.ClientHeight-1]);
  CameraMoved;
  bmp.SaveToFile('1.bmp');
  bmp.Free;
  glViewport(0, 0, Form7.ClientWidth, Form7.ClientHeight);
  ShellExecute(0, nil, '1.bmp', nil, nil, 0);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  i: Integer;
  s: Ansistring;
begin
  with ValueListEditor1.Strings do begin
    ValueFromIndex[0] := TimeToStr(Khrono.Time);
    ValueFromIndex[1] := Format('%d ms', [frametime]);
    ValueFromIndex[2] := Format('%d %f', [Dbg1, Dbg2]);
    ValueFromIndex[3] := Format('%f', [sunpos.x]);
    ValueFromIndex[4] := Format('%f', [sunpos.z]);
  end;
end;

procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  if TreeView1.Selected = Node then
    if GetKeyState(VK_CONTROL) < 0 then
      MessageDlg(TreeView1.Selected.Text, mtInformation, [mbok], 0);
end;

end.
