object RegionSelectForm: TRegionSelectForm
  Left = 660
  Top = 319
  Width = 340
  Height = 264
  Caption = 'Region Selection'
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
  object OKBtn: TBitBtn
    Left = 99
    Top = 192
    Width = 70
    Height = 24
    TabOrder = 0
    OnClick = OKBtnClick
    Kind = bkOK
  end
  object RegionHelpBtn: TBitBtn
    Left = 181
    Top = 192
    Width = 70
    Height = 24
    TabOrder = 1
    OnClick = RegionHelpBtnClick
    Kind = bkHelp
  end
  object SelectRegionPanel: TPanel
    Left = 24
    Top = 24
    Width = 282
    Height = 156
    BevelInner = bvLowered
    Color = clSilver
    TabOrder = 2
    object SelectRegionLabel: TLabel
      Left = 12
      Top = 12
      Width = 260
      Height = 17
      AutoSize = False
      Caption = 'Available Regions (select all that apply):'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object EuropeBox: TCheckBox
      Left = 12
      Top = 34
      Width = 160
      Height = 16
      Caption = 'Europe'
      TabOrder = 0
      OnClick = RegionChoiceClick
    end
    object MideastBox: TCheckBox
      Left = 12
      Top = 52
      Width = 160
      Height = 16
      Caption = 'Middle East'
      TabOrder = 1
      OnClick = RegionChoiceClick
    end
    object AfricaBox: TCheckBox
      Left = 12
      Top = 70
      Width = 160
      Height = 16
      Caption = 'Africa'
      TabOrder = 2
      OnClick = RegionChoiceClick
    end
    object AsiaBox: TCheckBox
      Left = 12
      Top = 88
      Width = 160
      Height = 16
      Caption = 'Asia'
      TabOrder = 3
      OnClick = RegionChoiceClick
    end
    object AmericasBox: TCheckBox
      Left = 12
      Top = 106
      Width = 160
      Height = 16
      Caption = 'North and South America'
      TabOrder = 4
      OnClick = RegionChoiceClick
    end
    object GlobeBox: TCheckBox
      Left = 12
      Top = 124
      Width = 160
      Height = 16
      Caption = 'All (default)'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = GlobalChoiceClick
    end
  end
end
