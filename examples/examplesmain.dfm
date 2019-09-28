object Form1: TForm1
  Left = 310
  Height = 379
  Top = 132
  Width = 532
  BorderStyle = bsSingle
  Caption = 'Demo'
  ClientHeight = 379
  ClientWidth = 532
  Color = clBtnFace
  DesignTimePPI = 120
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Position = poDefault
  LCLVersion = '1.8.4.0'
  WindowState = wsMaximized
  object BitBtn1: TBitBtn
    Left = 10
    Height = 38
    Top = 8
    Width = 240
    Caption = 'Find Minimum Bounding Box'
    OnClick = BitBtn1Click
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 10
    Height = 289
    Top = 80
    Width = 512
    Anchors = [akTop, akLeft, akRight, akBottom]
    ScrollBars = ssAutoBoth
    TabOrder = 1
  end
  object Label1: TLabel
    Left = 10
    Height = 13
    Top = 56
    Width = 38
    Caption = 'Output:'
    ParentColor = False
  end
  object CheckBox1: TCheckBox
    Left = 264
    Height = 23
    Top = 23
    Width = 104
    Caption = 'Show extra data'
    TabOrder = 2
  end
  object DownhillSimplexAlgorithm1: TDownhillSimplexAlgorithm
    left = 248
    top = 296
  end
end
