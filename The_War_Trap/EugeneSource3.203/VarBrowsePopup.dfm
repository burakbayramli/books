object PopupBrowseForm: TPopupBrowseForm
  Left = 674
  Top = 290
  Width = 123
  Height = 132
  Caption = 'PopupBrowseForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PopupMenu1: TPopupMenu
    Left = 32
    Top = 16
    object BrowseData1: TMenuItem
      Caption = 'Browse Data'
      OnClick = BrowseData1Click
    end
    object SelectVariable1: TMenuItem
      Caption = 'Select Variable'
    end
  end
end
