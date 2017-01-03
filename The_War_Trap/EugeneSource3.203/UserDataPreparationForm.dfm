object UserDataPrepForm: TUserDataPrepForm
  Left = 386
  Top = 238
  Caption = 'Preparing User Dataset for Submission'
  ClientHeight = 361
  ClientWidth = 338
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 12
    Top = 12
    Width = 312
    Height = 247
    BevelInner = bvLowered
    Color = clSilver
    TabOrder = 0
    object Button1: TButton
      Left = 20
      Top = 13
      Width = 269
      Height = 27
      Caption = 'Step 1:  Define File Names'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 20
      Top = 62
      Width = 269
      Height = 27
      Caption = 'Step 2:  Define Variables'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 20
      Top = 111
      Width = 269
      Height = 26
      Caption = 'Step 3:  Define Time Span and Number of Cases'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 20
      Top = 159
      Width = 269
      Height = 27
      Caption = 'Step 4:  Save Configuration File'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 20
      Top = 208
      Width = 269
      Height = 27
      Caption = 'Step 5:  Upload Dataset to EUGene server'
      TabOrder = 4
      OnClick = Button5Click
    end
  end
  object DoneBtn: TBitBtn
    Left = 23
    Top = 267
    Width = 85
    Height = 26
    Caption = 'Done'
    TabOrder = 1
    OnClick = DoneBtnClick
    Kind = bkOK
  end
  object CancelBtn: TBitBtn
    Left = 126
    Top = 267
    Width = 85
    Height = 26
    TabOrder = 2
    OnClick = CancelBtnClick
    Kind = bkCancel
  end
  object HelpBtn: TBitBtn
    Left = 225
    Top = 267
    Width = 85
    Height = 26
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = HelpBtnClick
    Kind = bkHelp
  end
  object SaveDialog1: TSaveDialog
    Left = 288
    Top = 296
  end
  object OpenDialog1: TOpenDialog
    Left = 256
    Top = 296
  end
end
