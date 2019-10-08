object BoundingBoxServerForm: TBoundingBoxServerForm
  Left = 141
  Top = 131
  BorderStyle = bsSingle
  Caption = 'TBoundingBoxServerForm Demo'
  ClientHeight = 379
  ClientWidth = 532
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poDefault
  WindowState = wsMaximized
  DesignSize = (
    532
    379)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 56
    Width = 38
    Height = 13
    Caption = 'Output:'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object BitBtn1: TBitBtn
    Left = 10
    Top = 8
    Width = 240
    Height = 38
    Caption = 'Find Minimum Bounding Box'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 72
    Width = 512
    Height = 289
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
  object CheckBox1: TCheckBox
    Left = 264
    Top = 23
    Width = 104
    Height = 23
    Caption = 'Show extra data'
    TabOrder = 2
  end
  object DownhillSimplexAlgorithm1: TDownhillSimplexAlgorithm
    Left = 248
    Top = 296
  end
end
