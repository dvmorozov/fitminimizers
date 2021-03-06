object BoundingBoxComponentForm: TBoundingBoxComponentForm
  Left = 355
  Height = 472
  Top = 171
  Width = 878
  BorderStyle = bsSingle
  Caption = 'Component Demo'
  ClientHeight = 472
  ClientWidth = 878
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -9
  Font.Name = 'Tahoma'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  Position = poDefault
  LCLVersion = '2.0.6.0'
  object Label1: TLabel
    Left = 8
    Height = 10
    Top = 45
    Width = 36
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
    Left = 8
    Height = 10
    Top = 273
    Width = 48
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
    Height = 10
    Top = 6
    Width = 32
    Caption = 'Model:'
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ParentColor = False
    ParentFont = False
  end
  object Label4: TLabel
    Left = 201
    Height = 10
    Top = 6
    Width = 85
    Caption = 'Initial angle step:'
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ParentColor = False
    ParentFont = False
  end
  object Label5: TLabel
    Left = 201
    Height = 10
    Top = 24
    Width = 74
    Caption = 'Final tolerance:'
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ParentColor = False
    ParentFont = False
  end
  object Label6: TLabel
    Left = 201
    Height = 10
    Top = 42
    Width = 72
    Caption = 'Exit derivative:'
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ParentColor = False
    ParentFont = False
  end
  object BitBtnFindMinimumBoundingBox: TBitBtn
    Left = 492
    Height = 46
    Top = 7
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
    Height = 205
    Top = 58
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
    Height = 24
    Top = 2
    Width = 109
    Caption = 'Use random data'
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ParentFont = False
    TabOrder = 2
  end
  object ButtonBruteForce: TButton
    Left = 697
    Height = 20
    Top = 6
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
    Height = 46
    Top = 6
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
    Left = 697
    Height = 20
    Top = 33
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
    Left = 6
    Height = 27
    Top = 24
    Width = 154
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ItemHeight = 0
    ParentFont = False
    TabOrder = 7
  end
  object Ed_IniParamLenght: TEdit
    Left = 293
    Height = 20
    Top = 3
    Width = 97
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ParentFont = False
    TabOrder = 8
    Text = '37'
  end
  object Ed_FinalTolerance: TEdit
    Left = 293
    Height = 20
    Top = 21
    Width = 97
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ParentFont = False
    TabOrder = 9
    Text = '0.00001'
  end
  object Ed_ExitDerivate: TEdit
    Left = 293
    Height = 20
    Top = 38
    Width = 97
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    ParentFont = False
    TabOrder = 10
    Text = '0.5'
  end
  object RunnerMinimumBoundingBox: TRunner
    OnCompute = RunnerMinimumBoundingBoxCompute
    OnOutput = RunnerMinimumBoundingBoxOutput
    OnCreate = RunnerMinimumBoundingBoxCreate
    left = 499
    top = 70
  end
end
