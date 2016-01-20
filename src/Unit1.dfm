object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 653
  ClientWidth = 284
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 16
  object ValueListEditor1: TValueListEditor
    Left = 0
    Top = 0
    Width = 284
    Height = 89
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
    ExplicitWidth = 184
    ColWidths = (
      87
      174)
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 89
    Width = 284
    Height = 564
    Align = alClient
    Indent = 19
    TabOrder = 1
    OnChange = TreeView1Change
  end
  object Timer1: TTimer
    Interval = 11
    OnTimer = Timer1Timer
    Left = 88
    Top = 472
  end
end
