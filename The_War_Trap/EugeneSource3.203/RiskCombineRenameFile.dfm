object RiskCombineFileRename: TRiskCombineFileRename
  Left = 195
  Top = 247
  Width = 857
  Height = 195
  Caption = 'Error in File Name vs. Year'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ErrorMessageLabel: TLabel
    Left = 40
    Top = 24
    Width = 768
    Height = 20
    Caption = 'ErrorMessageLabel'
    Constraints.MaxHeight = 70
    Constraints.MaxWidth = 769
    Constraints.MinHeight = 20
    Constraints.MinWidth = 768
    WordWrap = True
  end
  object RenameFilesButton: TButton
    Left = 216
    Top = 104
    Width = 129
    Height = 33
    Caption = 'Confirm Rename'
    TabOrder = 0
    OnClick = RenameFilesButtonClick
  end
  object CancelButton: TButton
    Left = 440
    Top = 104
    Width = 129
    Height = 33
    Caption = 'Cancel Rename'
    TabOrder = 1
    OnClick = CancelButtonClick
  end
end
