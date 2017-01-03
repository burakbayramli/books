object FTP_Form: TFTP_Form
  Left = 164
  Top = 118
  ActiveControl = CloseButton
  Caption = 'EUGene FTP connection'
  ClientHeight = 541
  ClientWidth = 542
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    542
    541)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 320
    Width = 523
    Height = 113
    Hint = 'File transfer message window'
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ReadOnly = True
    ScrollBars = ssVertical
    ShowHint = False
    TabOrder = 0
  end
  object UploadButton: TButton
    Left = 283
    Top = 290
    Width = 241
    Height = 23
    Hint = 
      'Transfer a dataset from your PC to EUGene file area for distribu' +
      'tion'
    Caption = 'Upload dataset to EUGene uploads area...'
    TabOrder = 1
    OnClick = UploadButtonClick
  end
  object DownloadButton: TButton
    Left = 8
    Top = 290
    Width = 249
    Height = 23
    Hint = 'Transfer a dataset from EUGene data file area to your PC'
    Caption = 'Download dataset from EUGene user files area...'
    TabOrder = 2
    OnClick = DownloadButtonClick
  end
  object CloseButton: TBitBtn
    Left = 52
    Top = 440
    Width = 102
    Height = 25
    Caption = 'Close'
    TabOrder = 3
    OnClick = CloseButtonClick
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
  object ServerFileBox: TGroupBox
    Left = 0
    Top = 96
    Width = 521
    Height = 185
    Anchors = [akLeft, akTop, akRight]
    Caption = ' User Datasets on EUGene Server: '
    TabOrder = 4
    DesignSize = (
      521
      185)
    object StringGrid1: TStringGrid
      Left = 8
      Top = 16
      Width = 505
      Height = 161
      Anchors = [akLeft, akTop, akRight, akBottom]
      ColCount = 1
      DefaultRowHeight = 20
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      GridLineWidth = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect]
      TabOrder = 0
      OnSelectCell = StringGrid1SelectCell
      ColWidths = (
        501)
    end
  end
  object HelpButton: TBitBtn
    Left = 368
    Top = 440
    Width = 89
    Height = 25
    TabOrder = 5
    OnClick = HelpButtonClick
    Kind = bkHelp
  end
  object CancelAbortButton: TBitBtn
    Left = 192
    Top = 440
    Width = 137
    Height = 25
    Cancel = True
    Caption = 'Abort Transfer'
    TabOrder = 6
    OnClick = CancelAbortButtonClick
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      333333333333333333333333000033338833333333333333333F333333333333
      0000333911833333983333333388F333333F3333000033391118333911833333
      38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
      911118111118333338F3338F833338F3000033333911111111833333338F3338
      3333F8330000333333911111183333333338F333333F83330000333333311111
      8333333333338F3333383333000033333339111183333333333338F333833333
      00003333339111118333333333333833338F3333000033333911181118333333
      33338333338F333300003333911183911183333333383338F338F33300003333
      9118333911183333338F33838F338F33000033333913333391113333338FF833
      38F338F300003333333333333919333333388333338FFF830000333333333333
      3333333333333333333888330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 522
    Width = 542
    Height = 19
    Panels = <
      item
        Width = 200
      end
      item
        Width = 50
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 520
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
    object emailLabel: TLabel
      Left = 8
      Top = 44
      Width = 139
      Height = 13
      Caption = 'Your email address (required):'
    end
    object FTPSiteLabel: TLabel
      Left = 8
      Top = 16
      Width = 88
      Height = 13
      Caption = 'EUGene FTP Site:'
    end
    object EmailEdit: TEdit
      Left = 168
      Top = 40
      Width = 169
      Height = 21
      Hint = 'Enter your email address here'
      TabOrder = 0
      OnChange = EmailEditChange
    end
    object ConnectButton: TButton
      Left = 368
      Top = 11
      Width = 113
      Height = 23
      Hint = 'Press to connect to server'
      Caption = 'Connect'
      Default = True
      TabOrder = 1
      OnClick = ConnectButtonClick
    end
    object ftpedit: TEdit
      Left = 168
      Top = 11
      Width = 169
      Height = 21
      Hint = 'EUGene download ftp site'
      ReadOnly = True
      TabOrder = 2
      OnChange = EmailEditChange
    end
    object UsePassive: TCheckBox
      Left = 368
      Top = 43
      Width = 121
      Height = 17
      Caption = 'Use passive transfer'
      Enabled = False
      TabOrder = 3
      Visible = False
      OnClick = UsePassiveClick
    end
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 468
    Width = 199
    Height = 17
    Smooth = True
    TabOrder = 9
  end
  object O: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 344
    Top = 344
  end
  object SaveDialog1: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 376
    Top = 344
  end
  object IdFTP1: TIdFTP
    OnStatus = IdFTP1Status
    OnDisconnected = IdFTP1OnDisconnected
    OnWork = IdFTP1OnWork
    OnWorkBegin = IdFTP1OnWorkBegin
    OnWorkEnd = IdFTP1OnWorkEnd
    AutoLogin = True
    TransferType = ftASCII
    ProxySettings.ProxyType = fpcmNone
    ProxySettings.Port = 0
    ReadTimeout = 0
    Left = 304
    Top = 344
  end
end
