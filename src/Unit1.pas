unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids,
  Vcl.ValEdit, Vcl.ComCtrls, Vcl.CheckLst;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    ValueListEditor1: TValueListEditor;
    TreeView1: TTreeView;
    CheckListBox1: TCheckListBox;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure CheckListBox1ClickCheck(Sender: TObject);
  private
    { Private declarations }
  public
    FrameTime: Integer;
  end;

var
  Form1: TForm1;

implementation uses
  Unit7, Khrono, gfxrender, Model3D, dbxjson, StrUtils, MilitiaAdventurer;

{$R *.dfm}

procedure TForm1.CheckListBox1ClickCheck(Sender: TObject);
begin
  Peasant.Model3d.Bones[CheckListBox1.ItemIndex].DebugDraw := CheckListBox1.Checked[CheckListBox1.ItemIndex];
end;

procedure TForm1.FormCreate(Sender: TObject);

  procedure AddItem(json: TJSONValue; parent: TTreeNode; const AName: string);
  var
    i: Integer;
    node: TTreeNode;

{    procedure DoAdd(get: TJSONValue; const AName: string);
    begin
      if Get is TJSONObject then
        AddItem(TJSONObject(Get), node, AName)
      else
        if Get is TJSONArray then
          AddItem(TJSONObject(Get), node, AName)
        else
//        if TJSONObject(json).Value <> '' then
    end;}

  var
    f: Boolean;
  begin
    node := TreeView1.Items.AddChild(parent, AName);
    if json is TJSONArray then
      with TJSONArray(json) do begin
        f := false;
        for i := 0 to Size-1 do
          if Get(i) is TJSONObject then
            f := true;
        if f then
          for i := 0 to Size-1 do
            AddItem(Get(i), node, AName + '[' + IntToStr(i) + ']')
        else
          for i := 0 to Size-1 do
            node.Text := node.Text + IfThen(i=0, ': ', ', ') + Get(i).Value;
//          DoAdd(get(i), )
      end
    else
    if json is TJSONObject then
      with TJSONObject(json) do begin
        for i := 0 to Size-1 do
          AddItem(Get(i).JsonValue, node, get(i).JsonString.Value)
//          DoAdd(get(i).JsonValue, get(i).JsonString.Value);
      end
    else
      node.Text := node.Text + ': ' + json.Value;
//      raise Exception.Create('');
  end;


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

procedure TForm1.Timer1Timer(Sender: TObject);
var
  i: Integer;
  s: Ansistring;
begin
  with ValueListEditor1.Strings do begin
    ValueFromIndex[0] := TimeToStr(Khrono.Time);
    ValueFromIndex[1] := Format('%d ms', [frametime]);
    ValueFromIndex[2] := Format('%d/%d', [t3dmodel.DebugIndex, 1{High(Peasant)}]);
    ValueFromIndex[3] := Format('%f', [sunpos.x]);
    ValueFromIndex[4] := Format('%f', [sunpos.z]);
  end;
  if (Peasant.Model3d <> nil) and (CheckListBox1.Tag = 0) then begin
    CheckListBox1.Tag := 1;
    with Peasant.Model3d.Bones do
      for i := 0 to Count-1 do begin
        s := Data[i].Name;
        if Data[i].Parent <> nil then
          s := s + ' <- ' + Data[i].Parent.Name;
        CheckListBox1.Items.Add(String(s));
      end;
  end;
end;

procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  if TreeView1.Selected = Node then
    if GetKeyState(VK_CONTROL) < 0 then
      MessageDlg(TreeView1.Selected.Text, mtInformation, [mbok], 0);
end;

end.
