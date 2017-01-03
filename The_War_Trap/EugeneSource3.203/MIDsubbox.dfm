object MidSubInfoWindow: TMidSubInfoWindow
  Left = 364
  Top = 208
  Width = 579
  Height = 520
  Caption = 'MID Variable Selection'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MIDGrpBox: TGroupBox
    Left = 7
    Top = 6
    Width = 449
    Height = 359
    Caption = 'Available MID Variables'
    TabOrder = 0
    object CountryDisputeGroupBox: TGroupBox
      Left = 10
      Top = 115
      Width = 196
      Height = 204
      Caption = 'Country-dispute characteristics'
      TabOrder = 0
      object startdatecheck: TCheckBox
        Left = 20
        Top = 20
        Width = 163
        Height = 13
        Caption = 'Start Date'
        TabOrder = 0
      end
      object enddatecheckbox: TCheckBox
        Left = 20
        Top = 39
        Width = 163
        Height = 14
        Caption = 'End Date'
        TabOrder = 1
      end
      object SideACheckBox: TCheckBox
        Left = 20
        Top = 59
        Width = 163
        Height = 13
        Caption = 'Side A (boolean)'
        TabOrder = 2
      end
      object OriginatorCheckBox: TCheckBox
        Left = 20
        Top = 176
        Width = 163
        Height = 13
        Caption = 'Originator'
        TabOrder = 3
      end
      object HostLevStateCheckBox: TCheckBox
        Left = 20
        Top = 156
        Width = 163
        Height = 14
        Caption = 'Hostility Level (state)'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = HostLevStateCheckBoxClick
      end
      object HiActCheckBox: TCheckBox
        Left = 20
        Top = 137
        Width = 163
        Height = 13
        Caption = 'Highest Action (state)'
        TabOrder = 5
      end
      object FatalityLevelStateCheckBox: TCheckBox
        Left = 20
        Top = 117
        Width = 163
        Height = 14
        Caption = 'Fatality Level (state)'
        TabOrder = 6
      end
      object RevTypeCheckBox: TCheckBox
        Left = 20
        Top = 98
        Width = 163
        Height = 13
        Caption = 'Revision Type'
        TabOrder = 7
      end
      object RevStateCheckBox: TCheckBox
        Left = 20
        Top = 78
        Width = 163
        Height = 14
        Caption = 'Revisionist State (boolean)'
        TabOrder = 8
      end
    end
    object OverallDisputeGroupBox: TGroupBox
      Left = 215
      Top = 26
      Width = 228
      Height = 235
      Caption = 'Overall Dispute characteristics'
      TabOrder = 1
      object HostLevDisputeCheckBox: TCheckBox
        Left = 20
        Top = 95
        Width = 137
        Height = 14
        Caption = 'Hostility Level (dispute)'
        TabOrder = 0
      end
      object HiActDisputeCheckBox: TCheckBox
        Left = 20
        Top = 76
        Width = 137
        Height = 13
        Caption = 'Highest Action (dispute)'
        TabOrder = 1
      end
      object FatalityLevelDisputeCheckBox: TCheckBox
        Left = 20
        Top = 56
        Width = 137
        Height = 14
        Caption = 'Fatality Level (dispute)'
        TabOrder = 2
      end
      object OutcomeCheckBox: TCheckBox
        Left = 20
        Top = 17
        Width = 137
        Height = 14
        Caption = 'Outcome'
        TabOrder = 3
      end
      object SettlementCheckBox: TCheckBox
        Left = 20
        Top = 37
        Width = 137
        Height = 13
        Caption = 'Settlement'
        TabOrder = 4
      end
      object RecipCheckBox: TCheckBox
        Left = 20
        Top = 115
        Width = 137
        Height = 14
        Caption = 'Reciprocated Dispute'
        TabOrder = 5
      end
      object NumOfStatesBox: TCheckBox
        Left = 20
        Top = 135
        Width = 189
        Height = 14
        Caption = 'Number of States on each side'
        TabOrder = 6
      end
      object maozcowwarbox: TCheckBox
        Left = 20
        Top = 154
        Width = 189
        Height = 14
        Caption = 'Maoz COWWar marker'
        TabOrder = 7
      end
      object maozdurindxbox: TCheckBox
        Left = 20
        Top = 174
        Width = 202
        Height = 14
        Caption = 'Maoz DurIndx (dispute year counter)'
        TabOrder = 8
      end
      object maozdurdaysbox: TCheckBox
        Left = 20
        Top = 193
        Width = 189
        Height = 14
        Caption = 'Maoz DurDays (days in dispute, this year)'
        TabOrder = 9
      end
      object maozreciprocateddyadicbox: TCheckBox
        Left = 20
        Top = 213
        Width = 189
        Height = 14
        Caption = 'Maoz dyadic reciprocation marker'
        TabOrder = 10
        Visible = False
      end
    end
    object DyadicDisputeGroupBox1: TGroupBox
      Left = 10
      Top = 26
      Width = 196
      Height = 85
      Caption = 'Dyadic Dispute Characteristics'
      TabOrder = 2
      object InitiationCheckBox: TCheckBox
        Left = 20
        Top = 20
        Width = 163
        Height = 13
        Caption = 'Initiation'
        Checked = True
        ParentShowHint = False
        ShowHint = False
        State = cbChecked
        TabOrder = 0
        OnClick = InitiationCheckBoxClick
      end
      object OngoingCheckBox: TCheckBox
        Left = 20
        Top = 39
        Width = 163
        Height = 14
        Caption = 'Ongoing'
        Checked = True
        ParentShowHint = False
        ShowHint = False
        State = cbChecked
        TabOrder = 1
        OnClick = OngoingCheckBoxClick
      end
      object MIDNumCheckBox: TCheckBox
        Left = 20
        Top = 59
        Width = 163
        Height = 13
        Caption = 'MID Number'
        TabOrder = 2
      end
    end
  end
  object OKBtn: TBitBtn
    Left = 121
    Top = 372
    Width = 72
    Height = 27
    TabOrder = 1
    OnClick = OKBtnClick
    Kind = bkOK
  end
  object MIDHelpBtn: TBitBtn
    Left = 234
    Top = 372
    Width = 72
    Height = 27
    TabOrder = 2
    OnClick = MIDHelpBtnClick
    Kind = bkHelp
  end
  object MarkJoinersCheckBox: TCheckBox
    Left = 20
    Top = 338
    Width = 118
    Height = 14
    Caption = 'Identify MID Joiners '
    TabOrder = 3
  end
end
