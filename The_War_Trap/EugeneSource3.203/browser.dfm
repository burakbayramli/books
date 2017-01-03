object DatasetBrowserForm: TDatasetBrowserForm
  Left = 266
  Top = 199
  Width = 978
  Height = 691
  Caption = 'Dataset Browser'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  DesignSize = (
    970
    657)
  PixelsPerInch = 96
  TextHeight = 13
  object PercentageLabel: TLabel
    Left = 352
    Top = 635
    Width = 12
    Height = 13
    Caption = '00'
    Constraints.MaxWidth = 12
  end
  object PercentLabel: TLabel
    Left = 364
    Top = 635
    Width = 8
    Height = 13
    Caption = '%'
    Visible = False
  end
  object DatasetGrid: TStringGrid
    Left = 16
    Top = 48
    Width = 945
    Height = 569
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 15
    FixedCols = 0
    RowCount = 23
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing, goRowSelect]
    TabOrder = 0
    ColWidths = (
      64
      64
      64
      63
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64)
  end
  object FileSelectionBox: TComboBox
    Left = 24
    Top = 18
    Width = 345
    Height = 21
    ItemHeight = 13
    TabOrder = 1
  end
  object LoadButton: TButton
    Left = 376
    Top = 16
    Width = 89
    Height = 25
    Caption = 'Load File'
    TabOrder = 2
    OnClick = LoadButtonClick
  end
  object SearchField: TEdit
    Left = 704
    Top = 18
    Width = 145
    Height = 21
    Hint = 'Search for this value starting from current row'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    Text = 'search for...'
  end
  object SearchButton: TButton
    Left = 856
    Top = 16
    Width = 97
    Height = 25
    Hint = 'Search for this value starting from current row'
    Caption = 'Search'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = SearchButtonClick
  end
  object ExitButton: TButton
    Left = 808
    Top = 632
    Width = 129
    Height = 25
    Caption = 'Exit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = ExitButtonClick
  end
  object FixColumnButton: TButton
    Left = 24
    Top = 632
    Width = 105
    Height = 25
    Caption = 'Fix First Column'
    TabOrder = 6
    OnClick = FixColumnButtonClick
  end
  object ProgressBar1: TProgressBar
    Left = 200
    Top = 640
    Width = 137
    Height = 9
    TabOrder = 7
  end
end
