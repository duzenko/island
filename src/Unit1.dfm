object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 775
  ClientWidth = 337
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 120
  TextHeight = 19
  object Image1: TImage
    Left = 0
    Top = 447
    Width = 337
    Height = 328
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    Center = True
    Proportional = True
    Visible = False
  end
  object ValueListEditor1: TValueListEditor
    Left = 0
    Top = 0
    Width = 337
    Height = 106
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goAlwaysShowEditor, goThumbTracking]
    Strings.Strings = (
      'World Time='
      'Frame Time='
      'Debug Index='
      '='
      '='
      '='
      '='
      '='
      '='
      '=')
    TabOrder = 0
    ColWidths = (
      87
      174)
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 106
    Width = 337
    Height = 341
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    Indent = 19
    TabOrder = 1
    Visible = False
    OnChange = TreeView1Change
  end
  object CheckListBox1: TCheckListBox
    Left = 0
    Top = 106
    Width = 337
    Height = 341
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    OnClickCheck = CheckListBox1ClickCheck
    Align = alClient
    TabOrder = 2
  end
  object Timer1: TTimer
    Interval = 11
    OnTimer = Timer1Timer
    Left = 88
    Top = 472
  end
  object MainMenu1: TMainMenu
    Left = 136
    Top = 328
    object Utils1: TMenuItem
      Caption = 'Utils'
      object Rendertrees1: TMenuItem
        Caption = 'Render trees'
        OnClick = Rendertrees1Click
      end
    end
  end
end
