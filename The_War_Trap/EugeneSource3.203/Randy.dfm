object RandysWindow: TRandysWindow
  Left = 185
  Top = 248
  Width = 572
  Height = 314
  Caption = 'Randy'#39's S Calculations - arbitrary countries and years'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 56
    Width = 82
    Height = 13
    Caption = 'Output File Name'
  end
  object Label2: TLabel
    Left = 16
    Top = 16
    Width = 74
    Height = 13
    Caption = 'Input File Name'
  end
  object Edit1: TEdit
    Left = 112
    Top = 56
    Width = 257
    Height = 21
    ReadOnly = True
    TabOrder = 0
    Text = 'Output File'
  end
  object Edit2: TEdit
    Left = 112
    Top = 16
    Width = 257
    Height = 21
    ReadOnly = True
    TabOrder = 1
    Text = 'Input File'
  end
  object Button1: TButton
    Left = 24
    Top = 200
    Width = 217
    Height = 33
    Caption = 'Run Randy'#39's S Score Computations'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 264
    Top = 200
    Width = 113
    Height = 33
    Caption = 'Exit'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 392
    Top = 56
    Width = 145
    Height = 25
    Caption = 'Specify Output File'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 392
    Top = 16
    Width = 145
    Height = 25
    Caption = 'Specify Input File'
    TabOrder = 5
    OnClick = Button4Click
  end
  object VarSeparatorBox: TGroupBox
    Left = 19
    Top = 86
    Width = 313
    Height = 79
    Caption = 'Output Variable Separator'
    TabOrder = 6
    object SeparateWithTabs: TRadioButton
      Left = 13
      Top = 15
      Width = 241
      Height = 21
      Caption = 'Separate Variables With Tabs'
      TabOrder = 0
      OnClick = SeparateWithTabsClick
    end
    object SeparateWithSpaces: TRadioButton
      Left = 13
      Top = 36
      Width = 241
      Height = 20
      Caption = 'Separate Variables With Spaces'
      TabOrder = 1
      OnClick = SeparateWithSpacesClick
    end
    object SeparateWithCommas: TRadioButton
      Left = 13
      Top = 55
      Width = 241
      Height = 21
      Caption = 'Separate Variables With Commas'
      TabOrder = 2
      OnClick = SeparateWithCommasClick
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 456
    Top = 128
  end
  object OpenDialog1: TOpenDialog
    Left = 408
    Top = 128
  end
end
