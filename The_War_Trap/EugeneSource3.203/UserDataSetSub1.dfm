object UserDataSetSubForm1: TUserDataSetSubForm1
  Left = 311
  Top = 100
  Width = 538
  Height = 633
  Caption = 'Set File Names for User Dataset Configuration'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 7
    Top = 7
    Width = 416
    Height = 498
    BevelInner = bvLowered
    Color = clSilver
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 7
      Top = 13
      Width = 403
      Height = 220
      Caption = ' Descriptive names to be seen by users '
      TabOrder = 0
      object GroupBox6: TGroupBox
        Left = 13
        Top = 20
        Width = 378
        Height = 52
        Hint = 
          'This description will appear in the window listing your variable' +
          's within EUGene'
        Caption = ' Long descriptive name for your dataset (up to 45 characters):  '
        TabOrder = 0
        object LongNameEdit: TEdit
          Left = 20
          Top = 21
          Width = 298
          Height = 21
          Hint = 
            'This description will appear in the window listing your variable' +
            's within EUGene'
          TabOrder = 0
          Text = 'LongName'
          OnChange = LongNameEditChange
        end
      end
      object GroupBox7: TGroupBox
        Left = 13
        Top = 91
        Width = 378
        Height = 50
        Hint = 
          'This name will appear as the tab label for your dataset in EUGen' +
          'e'#39's user variable selection box'
        Caption = 
          ' Short Descriptive Name for your dataset (up to 15 characters): ' +
          ' '
        TabOrder = 1
        object ShortNameEdit: TEdit
          Left = 20
          Top = 19
          Width = 113
          Height = 21
          Hint = 
            'This name will appear as the tab label for your dataset in EUGen' +
            'e'#39's user variable selection box'
          TabOrder = 0
          Text = 'ShortName'
          OnChange = ShortNameEditChange
        end
      end
      object GroupBox8: TGroupBox
        Left = 13
        Top = 156
        Width = 378
        Height = 50
        Caption = 'Citation for your dataset (up to 200 characters)'
        TabOrder = 2
        object CitationEdit: TEdit
          Left = 20
          Top = 18
          Width = 337
          Height = 21
          TabOrder = 0
          Text = 'Citation'
          OnChange = CitationEditChange
        end
      end
    end
    object GroupBox2: TGroupBox
      Left = 7
      Top = 238
      Width = 403
      Height = 248
      Hint = 'Select data file from disk.'
      Caption = ' File Names '
      TabOrder = 1
      object GroupBox3: TGroupBox
        Left = 14
        Top = 23
        Width = 377
        Height = 52
        Hint = 'File containing your data on disk'
        Caption = 'Data File (.csv)'
        TabOrder = 0
        object DataFileEdit: TEdit
          Left = 16
          Top = 20
          Width = 228
          Height = 21
          Hint = 
            'Name/disk location of file containing your data (comma-separated' +
            ' variable format)'
          TabStop = False
          ReadOnly = True
          TabOrder = 0
        end
        object SelectDataButton: TButton
          Left = 261
          Top = 23
          Width = 105
          Height = 19
          Hint = 'Click to select data file on disk'
          Caption = 'Select File...'
          TabOrder = 1
          OnClick = SelectDataButtonClick
        end
      end
      object GroupBox4: TGroupBox
        Left = 14
        Top = 161
        Width = 377
        Height = 74
        Hint = 'Name of dataset configuration file that EUGene will create'
        Caption = 'Data Configuration File (.edf, to be created by EUGene)'
        TabOrder = 1
        object ConfigFileEdit: TEdit
          Left = 16
          Top = 20
          Width = 228
          Height = 21
          Hint = 'Name of dataset configuration file that EUGene will create'
          TabStop = False
          ReadOnly = True
          TabOrder = 0
        end
        object ConfigFileNewNameBtn: TButton
          Left = 261
          Top = 23
          Width = 105
          Height = 19
          Hint = 'Specify a new file name to save the dataset specifications'
          Caption = 'Specify New File...'
          TabOrder = 1
          OnClick = ConfigFileNewNameBtnClick
        end
        object Button1: TButton
          Left = 261
          Top = 48
          Width = 105
          Height = 20
          Hint = 
            'Click to load an existing set of dataset specifications from dis' +
            'k'
          Caption = 'Load Existing File...'
          TabOrder = 2
          OnClick = ConfigFileLoadBtnClick
        end
      end
      object GroupBox5: TGroupBox
        Left = 14
        Top = 90
        Width = 377
        Height = 52
        Hint = 'File containing the documentation for your dataset'
        Caption = 'Documentation File (.rtf)'
        TabOrder = 2
        object DocFileEdit: TEdit
          Left = 16
          Top = 20
          Width = 228
          Height = 21
          Hint = 'File containing the documentation for your dataset'
          ReadOnly = True
          TabOrder = 0
        end
        object SelectDocFileButton: TButton
          Left = 261
          Top = 22
          Width = 105
          Height = 20
          Hint = 'Click to select documentation file on disk'
          Caption = 'Select File...'
          TabOrder = 1
          OnClick = SelectDocFileButtonClick
        end
      end
    end
  end
  object DoneBtn: TBitBtn
    Left = 32
    Top = 512
    Width = 92
    Height = 26
    Caption = 'Done'
    Default = True
    TabOrder = 1
    OnClick = DoneBtnClick
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
    Left = 162
    Top = 512
    Width = 84
    Height = 26
    TabOrder = 2
    Kind = bkCancel
  end
  object HelpBtn: TBitBtn
    Left = 284
    Top = 512
    Width = 98
    Height = 26
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = HelpBtnClick
    Kind = bkHelp
  end
  object OpenDialog1: TOpenDialog
    Left = 392
    Top = 512
  end
end
