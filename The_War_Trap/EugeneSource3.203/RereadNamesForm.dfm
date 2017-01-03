object ReadVarNameOkForm: TReadVarNameOkForm
  Left = 243
  Top = 194
  Width = 474
  Height = 231
  Caption = 'Confirm Variable Name Read...'
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
  object ConfirmNamesPanel: TPanel
    Left = 24
    Top = 24
    Width = 420
    Height = 120
    BevelInner = bvLowered
    Color = clSilver
    TabOrder = 0
    object AskLabel: TLabel
      Left = 12
      Top = 34
      Width = 44
      Height = 13
      Caption = 'AskLabel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object ConfirmNamesHeader: TLabel
      Left = 12
      Top = 12
      Width = 291
      Height = 17
      AutoSize = False
      Caption = 'Confirm Change in Variable Names'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object okbtn: TBitBtn
    Left = 46
    Top = 156
    Width = 180
    Height = 24
    Caption = 'OK (Read New Names)'
    TabOrder = 1
    Kind = bkOK
  end
  object cancelbtn: TBitBtn
    Left = 240
    Top = 156
    Width = 180
    Height = 24
    Caption = 'Cancel (Keep Current Names)'
    TabOrder = 2
    Kind = bkCancel
  end
end
