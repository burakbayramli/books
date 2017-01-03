object CombineRiskFilesYearsForm: TCombineRiskFilesYearsForm
  Left = 155
  Top = 166
  Width = 314
  Height = 212
  Caption = 'CombineRiskFilesYearsForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  object OKBtn: TBitBtn
    Left = 120
    Top = 142
    Width = 70
    Height = 24
    TabOrder = 0
    OnClick = OKBtnClick
    Kind = bkOK
  end
  object CombineRiskFilesYearsPanel: TPanel
    Left = 24
    Top = 24
    Width = 260
    Height = 106
    BevelInner = bvLowered
    Color = clSilver
    TabOrder = 1
    object CombineRiskFilesYearsLabel: TLabel
      Left = 12
      Top = 12
      Width = 240
      Height = 17
      AutoSize = False
      Caption = 'Use only subset of years for this file?'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object From_Year: TLabel
      Left = 78
      Top = 53
      Width = 51
      Height = 13
      Caption = 'From Year:'
      Color = clSilver
      ParentColor = False
    end
    object To_Year: TLabel
      Left = 160
      Top = 53
      Width = 41
      Height = 13
      Caption = 'To Year:'
    end
    object NoSubsetButton: TRadioButton
      Left = 12
      Top = 34
      Width = 100
      Height = 16
      Caption = 'No'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = NoSubsetButtonClick
    end
    object YesSubsetButton: TRadioButton
      Left = 12
      Top = 52
      Width = 60
      Height = 16
      Caption = 'Yes'
      TabOrder = 1
      OnClick = YesSubsetButtonClick
    end
    object fromyearedit: TEdit
      Left = 78
      Top = 70
      Width = 60
      Height = 20
      AutoSize = False
      TabOrder = 2
    end
    object toyearedit: TEdit
      Left = 160
      Top = 70
      Width = 60
      Height = 20
      AutoSize = False
      TabOrder = 3
    end
  end
end
