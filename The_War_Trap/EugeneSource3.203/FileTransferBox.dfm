object FileTransferForm: TFileTransferForm
  Left = 318
  Top = 265
  Width = 355
  Height = 175
  Caption = 'EUGene dataset transfer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object AbortFileTransferButton: TBitBtn
    Left = 105
    Top = 104
    Width = 140
    Height = 24
    Caption = 'Abort File Transfer'
    TabOrder = 0
    OnClick = AbortFileTransferButtonClick
    Kind = bkCancel
  end
  object FileTransferPanel: TPanel
    Left = 24
    Top = 24
    Width = 302
    Height = 68
    BevelInner = bvLowered
    Color = clSilver
    TabOrder = 1
    object FileTransferLabel: TLabel
      Left = 12
      Top = 12
      Width = 278
      Height = 17
      AutoSize = False
      Caption = 'File Transfer Status'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object StatusBar1: TStatusBar
      Left = 2
      Top = 33
      Width = 298
      Height = 33
      Color = clSilver
      Panels = <>
      SimplePanel = False
    end
  end
end
