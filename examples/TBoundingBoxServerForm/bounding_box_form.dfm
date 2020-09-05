object BoundingBoxForm: TBoundingBoxForm
  Left = 3
  Top = 2
  BorderStyle = bsSingle
  Caption = 'Minimum Bounding Box Demo'
  ClientHeight = 400
  ClientWidth = 776
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -9
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poDefault
  OnDestroy = FormDestroy
  DesignSize = (
    776
    400)
  PixelsPerInch = 96
  TextHeight = 11
  object Label1: TLabel
    Left = 8
    Top = 48
    Width = 32
    Height = 11
    Caption = 'Output:'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Label2: TLabel
    Left = 7
    Top = 273
    Width = 44
    Height = 11
    Caption = 'Test Runs:'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Label3: TLabel
    Left = 7
    Top = 6
    Width = 28
    Height = 11
    Caption = 'Model:'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Label4: TLabel
    Left = 201
    Top = 6
    Width = 71
    Height = 11
    Caption = 'Initial angle step:'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Label5: TLabel
    Left = 201
    Top = 24
    Width = 63
    Height = 11
    Caption = 'Final tolerance:'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Label6: TLabel
    Left = 201
    Top = 42
    Width = 63
    Height = 11
    Caption = 'Exit derivative:'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object BitBtnFindMinimumBoundingBox: TBitBtn
    Left = 403
    Top = 8
    Width = 192
    Height = 44
    Anchors = [akTop, akRight]
    Caption = 'Find Minimum Bounding Box'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = BitBtnFindMinimumBoundingBoxClick
  end
  object Memo1: TMemo
    Left = 6
    Top = 64
    Width = 760
    Height = 125
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object CheckBoxRandomData: TCheckBox
    Left = 74
    Top = -1
    Width = 91
    Height = 19
    Caption = 'Use random data'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object ButtonBruteForce: TButton
    Left = 601
    Top = 8
    Width = 104
    Height = 20
    Anchors = [akTop, akRight]
    Caption = 'Brute Force'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = ButtonBruteForceClick
  end
  object Memo2: TMemo
    Left = 6
    Top = 216
    Width = 760
    Height = 178
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object ButtonStop: TButton
    Left = 711
    Top = 8
    Width = 55
    Height = 44
    Anchors = [akTop, akRight]
    Caption = 'Stop'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = ButtonStopClick
  end
  object ButtonRandomTest: TButton
    Left = 601
    Top = 32
    Width = 104
    Height = 20
    Anchors = [akTop, akRight]
    Caption = 'Random Test'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = ButtonRandomTestClick
  end
  object ComboBoxFiles: TComboBox
    Left = 7
    Top = 21
    Width = 154
    Height = 19
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnChange = ComboBoxFilesChange
  end
  object EditInitialAngleStep: TEdit
    Left = 293
    Top = 3
    Width = 97
    Height = 19
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    Text = '37'
  end
  object EditFinalTolerance: TEdit
    Left = 293
    Top = 21
    Width = 97
    Height = 19
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    Text = '0.00001'
  end
  object EditExitDerivate: TEdit
    Left = 293
    Top = 38
    Width = 97
    Height = 19
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 10
    Text = '0.5'
  end
end
