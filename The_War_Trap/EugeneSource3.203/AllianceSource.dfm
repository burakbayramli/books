object AllianceDataSourceForm: TAllianceDataSourceForm
  Left = 233
  Top = 118
  Width = 313
  Height = 193
  Caption = 'AllianceDataSourceForm'
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
  object AllianceSourceOKButton: TBitBtn
    Left = 78
    Top = 120
    Width = 70
    Height = 24
    TabOrder = 0
    OnClick = AllianceSourceOKButtonClick
    Kind = bkOK
  end
  object HelpBtn: TBitBtn
    Left = 160
    Top = 120
    Width = 70
    Height = 24
    TabOrder = 1
    OnClick = HelpBtnClick
    Kind = bkHelp
  end
  object AllianceDataSourcePanel: TPanel
    Left = 24
    Top = 24
    Width = 260
    Height = 84
    BevelInner = bvLowered
    Color = clSilver
    TabOrder = 2
    object AllianceDataSourceLabel: TLabel
      Left = 12
      Top = 12
      Width = 144
      Height = 17
      AutoSize = False
      Caption = 'Alliance Data Source'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object SequenceNumAllianceDataButton: TRadioButton
      Left = 12
      Top = 52
      Width = 173
      Height = 16
      Caption = 'COW Sequenced Alliance Data'
      TabOrder = 0
    end
    object DyadicInputDataButton: TRadioButton
      Left = 12
      Top = 34
      Width = 181
      Height = 16
      Caption = 'Dyadic Alliance File'
      TabOrder = 1
    end
  end
end
