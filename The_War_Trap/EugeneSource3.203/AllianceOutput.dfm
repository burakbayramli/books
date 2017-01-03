object AllianceOutputForm: TAllianceOutputForm
  Left = 501
  Top = 267
  Width = 643
  Height = 397
  Caption = 'Hypothetical Alliance Pattern Output'
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
  object GroupBox1: TGroupBox
    Left = 304
    Top = 16
    Width = 321
    Height = 305
    Caption = 'File Options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object Panel2: TPanel
      Left = 8
      Top = 112
      Width = 305
      Height = 89
      Color = clSilver
      TabOrder = 0
      object Label1: TLabel
        Left = 16
        Top = 8
        Width = 71
        Height = 13
        Caption = 'Format Type'
      end
      object DyadYearButton: TRadioButton
        Left = 24
        Top = 32
        Width = 113
        Height = 17
        Caption = 'Directed Dyad-Year'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = DyadYearButtonClick
      end
      object CountryListButton: TRadioButton
        Left = 24
        Top = 56
        Width = 113
        Height = 17
        Caption = 'Country List'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = CountryListButtonClick
      end
    end
  end
  object FileControlsPanel: TPanel
    Left = 312
    Top = 32
    Width = 305
    Height = 89
    Color = clSilver
    TabOrder = 1
    object OutputLabel: TLabel
      Left = 16
      Top = 8
      Width = 54
      Height = 13
      Caption = 'Output to'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ToFileButton: TRadioButton
      Left = 24
      Top = 24
      Width = 113
      Height = 17
      Caption = 'File'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = ToFileButtonClick
    end
    object SetOutputFileButton: TButton
      Left = 232
      Top = 40
      Width = 65
      Height = 17
      Caption = 'Set File'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = SetOutputFileButtonClick
    end
    object FilePathField: TEdit
      Left = 32
      Top = 40
      Width = 193
      Height = 21
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 2
      Text = 'File'
    end
    object ToScreenSelectionButton: TRadioButton
      Left = 24
      Top = 64
      Width = 113
      Height = 17
      Caption = 'Screen'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = ToScreenSelectionButtonClick
    end
  end
  object VariableSeparatorPanel: TPanel
    Left = 312
    Top = 224
    Width = 305
    Height = 89
    Color = clSilver
    TabOrder = 2
    object VariableSeparatorLabel: TLabel
      Left = 16
      Top = 8
      Width = 106
      Height = 13
      Caption = 'Variable Separator'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object TabSeparatorButton: TRadioButton
      Left = 24
      Top = 24
      Width = 113
      Height = 17
      Caption = 'Tab Separated'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = TabSeparatorButtonClick
    end
    object SpaceSeparatorButton: TRadioButton
      Left = 24
      Top = 40
      Width = 113
      Height = 17
      Caption = 'Space Separated'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = SpaceSeparatorButtonClick
    end
    object CommaSeparatorButton: TRadioButton
      Left = 24
      Top = 56
      Width = 113
      Height = 17
      Caption = 'Comma Separator'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = CommaSeparatorButtonClick
    end
  end
  object DataOptionsBox: TGroupBox
    Left = 8
    Top = 16
    Width = 289
    Height = 305
    Caption = 'Data Options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    object ScoresPanel: TPanel
      Left = 8
      Top = 16
      Width = 121
      Height = 89
      Color = clSilver
      TabOrder = 0
      object ScoresLabel: TLabel
        Left = 8
        Top = 8
        Width = 40
        Height = 13
        Caption = 'Scores'
      end
      object TauSelectionButton: TRadioButton
        Left = 16
        Top = 24
        Width = 57
        Height = 17
        Caption = 'Tau'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = TauSelectionButtonClick
      end
      object sSelectionButton: TRadioButton
        Left = 16
        Top = 48
        Width = 97
        Height = 17
        Caption = 'S (unweighted)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = sSelectionButtonClick
      end
    end
    object YearRangePanel: TPanel
      Left = 8
      Top = 112
      Width = 273
      Height = 121
      Color = clSilver
      TabOrder = 1
      object yearlabel1: TLabel
        Left = 128
        Top = 80
        Width = 9
        Height = 13
        Caption = 'to'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object YearRangeLabel: TLabel
        Left = 16
        Top = 8
        Width = 68
        Height = 13
        Caption = 'Year Range'
      end
      object AllYearsButton: TRadioButton
        Left = 24
        Top = 32
        Width = 81
        Height = 17
        Caption = 'All Years'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = AllYearsButtonClick
      end
      object YearRangeButton: TRadioButton
        Left = 24
        Top = 56
        Width = 113
        Height = 17
        Caption = 'Specified Range '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = YearRangeButtonClick
      end
      object StartYearField: TEdit
        Left = 64
        Top = 80
        Width = 49
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        Text = '1816'
        OnChange = StartYearFieldChange
      end
      object EndYearField: TEdit
        Left = 152
        Top = 80
        Width = 49
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        Text = '1999'
        OnChange = EndYearFieldChange
      end
    end
    object Panel1: TPanel
      Left = 8
      Top = 240
      Width = 273
      Height = 57
      Color = clSilver
      TabOrder = 2
      object HeaderLabel: TLabel
        Left = 8
        Top = 8
        Width = 109
        Height = 13
        Caption = 'Header Information'
      end
      object HeaderInfoBox: TCheckBox
        Left = 24
        Top = 24
        Width = 201
        Height = 17
        Caption = 'Output Header for Each Year'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
  end
  object BestWorstAlliancePanel: TPanel
    Left = 144
    Top = 32
    Width = 145
    Height = 89
    Color = clSilver
    TabOrder = 4
    object BestWorstLabel: TLabel
      Left = 16
      Top = 8
      Width = 114
      Height = 13
      Caption = 'Best/Worst Alliance'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object BestSelectionButton: TRadioButton
      Left = 16
      Top = 24
      Width = 65
      Height = 17
      Caption = 'Best'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = BestSelectionButtonClick
    end
    object WorstSelectionButton: TRadioButton
      Left = 16
      Top = 40
      Width = 57
      Height = 17
      Caption = 'Worst'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = WorstSelectionButtonClick
    end
    object BestAndWorstButton: TRadioButton
      Left = 16
      Top = 56
      Width = 113
      Height = 17
      Caption = 'Best and Worst'
      TabOrder = 2
      OnClick = BestAndWorstButtonClick
    end
  end
  object CancelButton: TButton
    Left = 320
    Top = 328
    Width = 89
    Height = 33
    Caption = 'Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = CancelButtonClick
  end
  object ConfirmButton: TButton
    Left = 184
    Top = 328
    Width = 105
    Height = 33
    Caption = 'Generate Output'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    OnClick = ConfirmButtonClick
  end
  object AllianceSaveDialog: TSaveDialog
    Left = 584
    Top = 328
  end
end
