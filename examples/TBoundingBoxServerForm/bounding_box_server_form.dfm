object BoundingBoxServerForm: TBoundingBoxServerForm
  Left = 355
  Height = 590
  Top = 171
  Width = 1098
  BorderStyle = bsSingle
  Caption = 'TBoundingBoxServerForm Demo'
  ClientHeight = 590
  ClientWidth = 1098
  Color = clBtnFace
  DesignTimePPI = 120
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  Position = poDefault
  LCLVersion = '1.8.4.0'
  object Label1: TLabel
    Left = 10
    Height = 13
    Top = 56
    Width = 38
    Caption = 'Output:'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label2: TLabel
    Left = 10
    Height = 13
    Top = 341
    Width = 52
    Caption = 'Test Runs:'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label3: TLabel
    Left = 9
    Height = 13
    Top = 8
    Width = 32
    Caption = 'Model:'
    ParentColor = False
  end
  object Label4: TLabel
    Left = 251
    Height = 13
    Top = 8
    Width = 83
    Caption = 'Initial angle step:'
    ParentColor = False
  end
  object Label5: TLabel
    Left = 251
    Height = 13
    Top = 30
    Width = 74
    Caption = 'Final tolerance:'
    ParentColor = False
  end
  object Label6: TLabel
    Left = 251
    Height = 13
    Top = 52
    Width = 73
    Caption = 'Exit derivative:'
    ParentColor = False
  end
  object BitBtnFindMinimumBoundingBox: TBitBtn
    Left = 616
    Height = 57
    Top = 9
    Width = 240
    Anchors = [akTop, akRight]
    Caption = 'Find Minimum Bounding Box'
    OnClick = BitBtnFindMinimumBoundingBoxClick
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 8
    Height = 257
    Top = 72
    Width = 1078
    Anchors = [akTop, akLeft, akRight, akBottom]
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object CheckBoxRandomData: TCheckBox
    Left = 93
    Height = 23
    Top = 3
    Width = 106
    Caption = 'Use random data'
    TabOrder = 2
  end
  object ButtonBruteForce: TButton
    Left = 872
    Height = 25
    Top = 8
    Width = 130
    Anchors = [akTop, akRight]
    Caption = 'Brute Force'
    OnClick = ButtonBruteForceClick
    TabOrder = 3
  end
  object Memo2: TMemo
    Left = 8
    Height = 222
    Top = 360
    Width = 1078
    Anchors = [akLeft, akRight, akBottom]
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object ButtonStop: TButton
    Left = 1017
    Height = 58
    Top = 8
    Width = 69
    Anchors = [akTop, akRight]
    Caption = 'Stop'
    OnClick = ButtonStopClick
    TabOrder = 5
  end
  object ButtonRandomTest: TButton
    Left = 872
    Height = 25
    Top = 41
    Width = 130
    Anchors = [akTop, akRight]
    Caption = 'Random Test'
    OnClick = ButtonRandomTestClick
    TabOrder = 6
  end
  object ComboBoxFiles: TComboBox
    Left = 8
    Height = 21
    Top = 30
    Width = 193
    ItemHeight = 13
    TabOrder = 7
  end
  object Ed_IniParamLenght: TEdit
    Left = 366
    Height = 21
    Top = 4
    Width = 121
    TabOrder = 8
    Text = '37'
  end
  object Ed_FinalTolerance: TEdit
    Left = 366
    Height = 21
    Top = 26
    Width = 121
    TabOrder = 9
    Text = '0.00001'
  end
  object Ed_ExitDerivate: TEdit
    Left = 366
    Height = 21
    Top = 48
    Width = 121
    TabOrder = 10
    Text = '0.5'
  end
  object RunnerMinimumBoundingBox: TRunner
    OnCompute = RunnerMinimumBoundingBoxCompute
    OnOutput = RunnerMinimumBoundingBoxOutput
    OnCreate = RunnerMinimumBoundingBoxCreate
    left = 624
    top = 88
  end
end
