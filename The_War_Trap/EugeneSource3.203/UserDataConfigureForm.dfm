object UserDataForm: TUserDataForm
  Left = 65
  Top = 123
  Width = 868
  Height = 605
  Caption = 'Define User Data Specification File'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object QuitNoSaveButton: TBitBtn
    Left = 344
    Top = 532
    Width = 137
    Height = 25
    Hint = 
      'Click to exit data set configurator without saving configuration' +
      ' file'
    Caption = 'Quit (without saving)'
    TabOrder = 2
    OnClick = QuitNoSaveButtonClick
    Kind = bkCancel
  end
  object HelpBtn: TBitBtn
    Left = 528
    Top = 532
    Width = 113
    Height = 25
    TabOrder = 3
    OnClick = HelpBtnClick
    Kind = bkHelp
  end
  object AutoDetectButton: TPanel
    Left = 24
    Top = 24
    Width = 689
    Height = 497
    BevelInner = bvLowered
    Color = clSilver
    TabOrder = 0
    object NumCasesLabel: TLabel
      Left = 24
      Top = 470
      Width = 90
      Height = 13
      Caption = 'Number of Cases:  '
    end
    object FirstYearLabel: TLabel
      Left = 24
      Top = 424
      Width = 104
      Height = 13
      Caption = 'First Year in Dataset:  '
    end
    object LongNameLabel: TLabel
      Left = 16
      Top = 61
      Width = 221
      Height = 13
      Caption = 'Long Descriptive Name (up to 45 characters):  '
    end
    object DataFileLabel: TLabel
      Left = 16
      Top = 37
      Width = 82
      Height = 13
      Caption = 'Data File Name:  '
    end
    object ShortNameLabel: TLabel
      Left = 16
      Top = 85
      Width = 222
      Height = 13
      Caption = 'Short Descriptive Name (up to 15 characters):  '
    end
    object LastYearLabel: TLabel
      Left = 24
      Top = 448
      Width = 105
      Height = 13
      Caption = 'Last Year in Dataset:  '
    end
    object UnitLabel: TLabel
      Left = 16
      Top = 132
      Width = 81
      Height = 13
      Caption = 'Unit of Analysis:  '
    end
    object ConfigFileNameLabel: TLabel
      Left = 16
      Top = 13
      Width = 120
      Height = 13
      Caption = 'Specification File Name:  '
    end
    object CitationLabel: TLabel
      Left = 16
      Top = 107
      Width = 162
      Height = 13
      Caption = 'Official Citation (no character limit):'
    end
    object NumCasesMaskEdit: TMaskEdit
      Left = 135
      Top = 466
      Width = 65
      Height = 21
      Hint = 'Number of cases (data lines) in dataset'
      EditMask = '99999999999;0;_'
      MaxLength = 11
      TabOrder = 8
    end
    object LastYearMaskEdit: TMaskEdit
      Left = 136
      Top = 444
      Width = 41
      Height = 21
      Hint = 'Year of latest observation in dataset'
      EditMask = '0000;1;_'
      MaxLength = 4
      TabOrder = 7
      Text = '    '
    end
    object UnitComboBox: TComboBox
      Left = 104
      Top = 128
      Width = 161
      Height = 21
      Hint = 
        'Unit of analysis of the dataset (annual data, country-year data,' +
        ' etc.)'
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 5
      OnChange = UnitComboBoxChange
      Items.Strings = (
        'None'
        'Year (Annual Data)'
        'Country-Year (Monadic Data)'
        'Nondirected Dyad-Year '
        'Directed Dyad-Year ')
    end
    object LongNameEdit: TEdit
      Left = 240
      Top = 58
      Width = 297
      Height = 21
      Hint = 
        'This description will appear in the tab listing data set variabl' +
        'e in EUGene'#39's user variable selection box'
      TabOrder = 3
      Text = 'LongName'
      OnChange = LongNameEditChange
    end
    object ShortNameEdit: TEdit
      Left = 240
      Top = 81
      Width = 113
      Height = 21
      Hint = 
        'This name will appear as the tab name in EUGene'#39's user variable ' +
        'selection box'
      TabOrder = 4
      Text = 'ShortName'
      OnChange = ShortNameEditChange
    end
    object DataFileEdit: TEdit
      Left = 104
      Top = 33
      Width = 305
      Height = 21
      Hint = 'File name of dataset on disk'
      TabStop = False
      ReadOnly = True
      TabOrder = 9
      Text = 'Data File Name (.csv)'
    end
    object DataFileSelectButton: TButton
      Left = 423
      Top = 35
      Width = 113
      Height = 18
      Hint = 'Locate a dataset on disk'
      Caption = 'Find Data File...'
      TabOrder = 2
      OnClick = DataFileSelectButtonClick
    end
    object FirstYearMaskEdit: TMaskEdit
      Left = 136
      Top = 420
      Width = 41
      Height = 21
      Hint = 'Year of earliest observation in dataset'
      EditMask = '0000'
      MaxLength = 4
      TabOrder = 6
      Text = '    '
    end
    object ConfigFileEdit: TEdit
      Left = 136
      Top = 9
      Width = 233
      Height = 21
      Hint = 'Name of dataset configuration file to be saved'
      TabStop = False
      ReadOnly = True
      TabOrder = 10
      Text = 'Specification File Name (.edf)'
    end
    object ConfigFileNewNameBtn: TButton
      Left = 385
      Top = 11
      Width = 137
      Height = 18
      Hint = 'Specify a new file name to save the dataset specifications'
      Caption = 'Specify New File Name...'
      TabOrder = 1
      OnClick = ConfigFileNewNameBtnClick
    end
    object ConfigFileLoadBtn: TButton
      Left = 536
      Top = 11
      Width = 113
      Height = 18
      Hint = 'Load dataset specifications from an existing configuration file'
      Caption = 'Load Existing File...'
      TabOrder = 0
      OnClick = ConfigFileLoadBtnClick
    end
    object AutoDetectYearCaseButton: TButton
      Left = 240
      Top = 440
      Width = 345
      Height = 23
      Caption = 'Attempt to Detect Year Range and # Cases from Dataset...'
      Enabled = False
      TabOrder = 11
      OnClick = AutoDetectYearCaseButtonClick
    end
    object VarGroupBox: TGroupBox
      Left = 8
      Top = 168
      Width = 673
      Height = 233
      Caption = 'Variable Information:  0 variables currently defined'
      TabOrder = 12
      object NumVarsLabel: TLabel
        Left = 16
        Top = 48
        Width = 104
        Height = 13
        Caption = 'Number of Variables:  '
      end
      object VarInfoStringGrid: TStringGrid
        Left = 144
        Top = 56
        Width = 513
        Height = 169
        ColCount = 6
        DefaultRowHeight = 22
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSizing, goColSizing, goRowSelect]
        TabOrder = 0
        OnSelectCell = VarInfoStringGridSelectCell
        ColWidths = (
          64
          87
          64
          64
          64
          64)
      end
      object MoveDownVariableButton: TButton
        Left = 16
        Top = 204
        Width = 109
        Height = 20
        Hint = 
          'Move the selected variable down in the order it appears in the d' +
          'ataset'
        Caption = 'Move Variable Down'
        TabOrder = 1
        OnClick = MoveDownVariableButtonClick
      end
      object MoveUpVariableButton: TButton
        Left = 16
        Top = 178
        Width = 109
        Height = 20
        Hint = 
          'Move the selected variable up in the order it appears in the dat' +
          'aset'
        Caption = 'Move Variable Up'
        TabOrder = 2
        OnClick = MoveUpVariableButtonClick
      end
      object DeleteVarButton: TButton
        Left = 16
        Top = 152
        Width = 109
        Height = 20
        Hint = 'Delete the selected variable from the dataset specification'
        Caption = 'Delete Variable Info'
        TabOrder = 3
        OnClick = DeleteVarButtonClick
      end
      object EditVarButton: TButton
        Left = 16
        Top = 126
        Width = 109
        Height = 20
        Hint = 'Edit the selected variable'#39's specification for name, type, etc.'
        Caption = 'Edit Variable Info'
        TabOrder = 4
        OnClick = EditVarButtonClick
      end
      object AddVariableButton: TButton
        Left = 16
        Top = 100
        Width = 109
        Height = 20
        Hint = 'Define an additional variable in data set'
        Caption = 'Add Variable Info'
        TabOrder = 5
        OnClick = AddVariableButtonClick
      end
      object NumVarsMaskEdit: TMaskEdit
        Left = 16
        Top = 65
        Width = 73
        Height = 21
        Hint = 'Number of variables in dataset'
        EditMask = '999;0;'
        MaxLength = 3
        TabOrder = 6
      end
      object LabelLineCheckBox: TCheckBox
        Left = 16
        Top = 24
        Width = 209
        Height = 17
        Hint = 'Check if the first line of the dataset contains variable names'
        Caption = 'Variable Names in Line 1 of Data File'
        TabOrder = 7
        OnClick = LabelLineCheckBoxClick
      end
      object AutoDetectNameButton: TButton
        Left = 240
        Top = 21
        Width = 345
        Height = 22
        Caption = 'Attempt to Read Variable Names from Line 1 of Dataset...'
        Enabled = False
        TabOrder = 8
        OnClick = AutoDetectNameButtonClick
      end
    end
    object CitationEdit: TEdit
      Left = 240
      Top = 104
      Width = 409
      Height = 21
      TabOrder = 13
      Text = 'Citation'
      OnChange = CitationEditChange
    end
  end
  object SaveNoExitButton: TBitBtn
    Left = 128
    Top = 532
    Width = 169
    Height = 25
    Hint = 
      'Click to save dataset specifications within the given configurat' +
      'ion file'
    Caption = 'Save Specification File'
    Default = True
    TabOrder = 1
    OnClick = SaveNoExitButtonClick
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
  object OpenDialog1: TOpenDialog
    Left = 29
    Top = 534
  end
  object SaveDialog1: TSaveDialog
    Left = 61
    Top = 534
  end
end
