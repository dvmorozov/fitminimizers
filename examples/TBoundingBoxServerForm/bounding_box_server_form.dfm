object BoundingBoxServerForm: TBoundingBoxServerForm
  Left = 554
  Height = 472
  Top = 131
  Width = 878
  BorderStyle = bsSingle
  Caption = 'Minimum Bounding Box Demo'
  ClientHeight = 472
  ClientWidth = 878
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -9
  Font.Name = 'Tahoma'
  OnDestroy = FormDestroy
  Position = poDefault
  LCLVersion = '2.0.6.0'
  object Label1: TLabel
    Left = 8
    Height = 11
    Top = 48
    Width = 32
    Caption = 'Output:'
    Color = clBtnFace
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Label2: TLabel
    Left = 7
    Height = 11
    Top = 273
    Width = 44
    Caption = 'Test Runs:'
    Color = clBtnFace
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Label3: TLabel
    Left = 7
    Height = 11
    Top = 6
    Width = 28
    Caption = 'Model:'
    Color = clBtnFace
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Label4: TLabel
    Left = 201
    Height = 11
    Top = 6
    Width = 71
    Caption = 'Initial angle step:'
    Color = clBtnFace
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Label5: TLabel
    Left = 201
    Height = 11
    Top = 24
    Width = 63
    Caption = 'Final tolerance:'
    Color = clBtnFace
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Label6: TLabel
    Left = 201
    Height = 11
    Top = 42
    Width = 63
    Caption = 'Exit derivative:'
    Color = clBtnFace
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object BitBtnFindMinimumBoundingBox: TBitBtn
    Left = 505
    Height = 44
    Top = 8
    Width = 192
    Anchors = [akTop, akRight]
    Caption = 'Find Minimum Bounding Box'
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    OnClick = BitBtnFindMinimumBoundingBoxClick
    ParentFont = False
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 6
    Height = 197
    Top = 64
    Width = 862
    Anchors = [akTop, akLeft, akRight, akBottom]
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object CheckBoxRandomData: TCheckBox
    Left = 74
    Height = 19
    Top = -1
    Width = 91
    Caption = 'Use random data'
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ParentFont = False
    TabOrder = 2
  end
  object ButtonBruteForce: TButton
    Left = 703
    Height = 20
    Top = 8
    Width = 104
    Anchors = [akTop, akRight]
    Caption = 'Brute Force'
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    OnClick = ButtonBruteForceClick
    ParentFont = False
    TabOrder = 3
  end
  object Memo2: TMemo
    Left = 6
    Height = 178
    Top = 288
    Width = 862
    Anchors = [akLeft, akRight, akBottom]
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object ButtonStop: TButton
    Left = 813
    Height = 44
    Top = 8
    Width = 55
    Anchors = [akTop, akRight]
    Caption = 'Stop'
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    OnClick = ButtonStopClick
    ParentFont = False
    TabOrder = 5
  end
  object ButtonRandomTest: TButton
    Left = 703
    Height = 20
    Top = 32
    Width = 104
    Anchors = [akTop, akRight]
    Caption = 'Random Test'
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    OnClick = ButtonRandomTestClick
    ParentFont = False
    TabOrder = 6
  end
  object ComboBoxFiles: TComboBox
    Left = 7
    Height = 19
    Top = 21
    Width = 154
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ItemHeight = 11
    OnChange = ComboBoxFilesChange
    ParentFont = False
    TabOrder = 7
  end
  object EditInitialAngleStep: TEdit
    Left = 293
    Height = 19
    Top = 3
    Width = 97
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ParentFont = False
    TabOrder = 8
    Text = '37'
  end
  object EditFinalTolerance: TEdit
    Left = 293
    Height = 19
    Top = 21
    Width = 97
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ParentFont = False
    TabOrder = 9
    Text = '0.00001'
  end
  object EditExitDerivate: TEdit
    Left = 293
    Height = 19
    Top = 38
    Width = 97
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ParentFont = False
    TabOrder = 10
    Text = '0.5'
  end
end
