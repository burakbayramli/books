object SystemCharacteristicsForm: TSystemCharacteristicsForm
  Left = 143
  Top = 226
  Width = 420
  Height = 288
  Caption = 'System Variable Selections'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SystemVariables: TGroupBox
    Left = 16
    Top = 16
    Width = 313
    Height = 193
    Caption = 'System Variables'
    TabOrder = 0
    object StatesInSystemBox: TCheckBox
      Left = 16
      Top = 24
      Width = 273
      Height = 17
      Caption = 'Number of States in the International System'
      TabOrder = 0
      OnClick = SystemVarsOKButtonClick
    end
    object GPsInSystemBox: TCheckBox
      Left = 16
      Top = 48
      Width = 273
      Height = 17
      Caption = 'Number of Major Powers in the International System'
      TabOrder = 1
    end
    object SysConBox: TCheckBox
      Left = 16
      Top = 72
      Width = 281
      Height = 17
      Caption = 'System Capability Concentration'
      TabOrder = 2
    end
    object SysMoveBox: TCheckBox
      Left = 16
      Top = 96
      Width = 289
      Height = 17
      Caption = 'System Movement (1 yr)'
      TabOrder = 3
    end
    object SysMove5Box: TCheckBox
      Left = 16
      Top = 120
      Width = 233
      Height = 17
      Caption = 'System Movement (5 yr)'
      TabOrder = 4
    end
    object SysMoveGPBox: TCheckBox
      Left = 16
      Top = 144
      Width = 249
      Height = 17
      Caption = 'System Movement (major powers only) (1 yr)'
      TabOrder = 5
    end
    object SysMoveGP5Box: TCheckBox
      Left = 16
      Top = 168
      Width = 257
      Height = 17
      Caption = 'System Movement (major powers only) (5 yr)'
      TabOrder = 6
    end
  end
  object SystemVarsOKButton: TBitBtn
    Left = 16
    Top = 224
    Width = 78
    Height = 26
    TabOrder = 1
    OnClick = SystemVarsOKButtonClick
    Kind = bkOK
  end
end
