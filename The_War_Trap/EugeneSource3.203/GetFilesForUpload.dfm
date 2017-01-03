object FilesForUploadForm: TFilesForUploadForm
  Left = 296
  Top = 171
  Width = 449
  Height = 349
  Caption = 'Confirm Files For Upload'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object UploadBtn: TBitBtn
    Left = 116
    Top = 276
    Width = 100
    Height = 24
    Hint = 
      'When you have specified all files, click here to upload dataset ' +
      'to EUGene server'
    Caption = 'Upload Files'
    Default = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnClick = UploadBtnClick
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object CancelBtn: TBitBtn
    Left = 228
    Top = 276
    Width = 100
    Height = 24
    Hint = 'Cancel the file upload'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = CancelBtnClick
    Kind = bkCancel
  end
  object DataFilePanel: TPanel
    Left = 24
    Top = 24
    Width = 396
    Height = 72
    BevelInner = bvLowered
    Color = clSilver
    TabOrder = 2
    object DataFileLabel: TLabel
      Left = 12
      Top = 12
      Width = 372
      Height = 17
      AutoSize = False
      Caption = 'Data File (.csv)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object DataFileEdit: TEdit
      Left = 12
      Top = 34
      Width = 268
      Height = 21
      Hint = 
        'Name/disk location of file containing the data (comma-separated ' +
        'variable format)'
      Color = clSilver
      ReadOnly = True
      TabOrder = 0
    end
    object DataFileButton: TPanel
      Left = 304
      Top = 34
      Width = 80
      Height = 18
      BevelInner = bvRaised
      BevelOuter = bvNone
      BevelWidth = 2
      Caption = 'Select File...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = SelectDataButtonClick
    end
  end
  object DocFilePanel: TPanel
    Left = 24
    Top = 108
    Width = 396
    Height = 72
    BevelInner = bvLowered
    Color = clSilver
    TabOrder = 3
    object DocFileLabel: TLabel
      Left = 12
      Top = 12
      Width = 372
      Height = 17
      AutoSize = False
      Caption = 'Documentation File (.rtf)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object DocFileEdit: TEdit
      Left = 12
      Top = 34
      Width = 268
      Height = 21
      Hint = 
        'Name/disk location of file containing the documentation (rich-te' +
        'xt format)'
      Color = clSilver
      ReadOnly = True
      TabOrder = 0
    end
    object DocFileButton: TPanel
      Left = 304
      Top = 34
      Width = 80
      Height = 18
      BevelInner = bvRaised
      BevelOuter = bvNone
      BevelWidth = 2
      Caption = 'Select File...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = SelectDocFileButtonClick
    end
  end
  object DataConfigFilePanel: TPanel
    Left = 24
    Top = 192
    Width = 396
    Height = 72
    BevelInner = bvLowered
    Color = clSilver
    TabOrder = 4
    object DataConfigFileLabel: TLabel
      Left = 12
      Top = 12
      Width = 372
      Height = 17
      AutoSize = False
      Caption = 'Data Configuration File (.edf)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ConfigFileEdit: TEdit
      Left = 12
      Top = 34
      Width = 268
      Height = 21
      Hint = 
        'Name/disk location of file containing configuration information ' +
        'about your data (flat text, eugene data format)'
      Color = clSilver
      ReadOnly = True
      TabOrder = 0
    end
    object DataConfigFileButton: TPanel
      Left = 304
      Top = 34
      Width = 80
      Height = 18
      BevelInner = bvRaised
      BevelOuter = bvNone
      BevelWidth = 2
      Caption = 'Select File...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = SelectedfButtonClick
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 384
    Top = 277
  end
end
