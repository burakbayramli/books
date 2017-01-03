object CombineRiskFileForm: TCombineRiskFileForm
  Left = 299
  Top = 249
  Width = 809
  Height = 536
  Caption = 'Combine Risk Files'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = CombineFileInitialize
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 777
    Height = 313
    Caption = 'Step 1:  Specify Input Files'
    TabOrder = 0
    object Label1: TLabel
      Left = 648
      Top = 10
      Width = 85
      Height = 13
      Caption = 'Use Subset Only?'
    end
    object Label2: TLabel
      Left = 190
      Top = 10
      Width = 55
      Height = 13
      Caption = 'File Names:'
    end
    object Label3: TLabel
      Left = 538
      Top = 10
      Width = 49
      Height = 13
      Caption = 'File Years:'
    end
    object Label4: TLabel
      Left = 168
      Top = 10
      Width = 7
      Height = 13
      Caption = '#'
    end
    object InputFileListBox: TListBox
      Left = 189
      Top = 24
      Width = 340
      Height = 257
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 0
    end
    object DateRangeListBox: TListBox
      Left = 537
      Top = 24
      Width = 104
      Height = 257
      ItemHeight = 13
      TabOrder = 1
    end
    object FileAddButton: TButton
      Left = 15
      Top = 93
      Width = 139
      Height = 24
      Hint = 
        'Click to add a risk or security file to the list.  These files w' +
        'ill be combined into one master file.'
      Caption = 'Add Input File...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = FileAddButtonClick
    end
    object clearlistbutton: TButton
      Left = 15
      Top = 165
      Width = 139
      Height = 24
      Hint = 'Clear the list of selected risk/security files'
      Caption = 'Clear Input File List'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = clearlistbuttonClick
    end
    object RiskSecurityGroupBox: TGroupBox
      Left = 10
      Top = 24
      Width = 147
      Height = 57
      Caption = 'Select Risk or Security Files'
      TabOrder = 4
      object RiskFilesRadioButton: TRadioButton
        Left = 8
        Top = 16
        Width = 113
        Height = 17
        Caption = 'Risk Files'
        TabOrder = 0
        OnClick = RiskFilesRadioButtonClick
      end
      object SecurityFilesButton: TRadioButton
        Left = 8
        Top = 35
        Width = 129
        Height = 17
        Caption = 'Security/Alliance Files'
        TabOrder = 1
        OnClick = SecurityFilesButtonClick
      end
    end
    object SubsetList: TListBox
      Left = 647
      Top = 24
      Width = 20
      Height = 257
      ItemHeight = 13
      TabOrder = 5
    end
    object NumList: TListBox
      Left = 163
      Top = 24
      Width = 20
      Height = 257
      ItemHeight = 13
      TabOrder = 6
    end
    object YearSubsetButton: TButton
      Left = 15
      Top = 202
      Width = 139
      Height = 24
      Hint = 
        'In case of file overlap in dates, specify a sub-range of years t' +
        'o be read from the selected file.'
      Caption = 'Specify year subset for file'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = YearSubsetButtonClick
    end
    object FromYearList: TListBox
      Left = 673
      Top = 24
      Width = 39
      Height = 257
      ItemHeight = 13
      TabOrder = 8
    end
    object ToYearList: TListBox
      Left = 719
      Top = 24
      Width = 39
      Height = 257
      ItemHeight = 13
      TabOrder = 9
    end
    object FileRemoveButton: TButton
      Left = 15
      Top = 129
      Width = 139
      Height = 24
      Hint = 'Click to remove the selected file from the list.  '
      Caption = 'Remove Input File'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      OnClick = RemoveFileButtonClick
    end
    object RetrieveOldFileListButton: TButton
      Left = 15
      Top = 258
      Width = 139
      Height = 24
      Hint = 
        'Reads in list of files from last time procedure successfully exe' +
        'cuted'
      Caption = 'Retrieve saved file names'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 11
      OnClick = RetrieveOldFileListButtonClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 338
    Width = 569
    Height = 55
    Caption = 'Step 2:  Specify Output file'
    TabOrder = 1
    object FileDisplay: TEdit
      Left = 172
      Top = 21
      Width = 251
      Height = 21
      TabStop = False
      ReadOnly = True
      TabOrder = 0
      Text = 'FileDisplay'
    end
    object ChangeOutputButton: TButton
      Left = 14
      Top = 19
      Width = 139
      Height = 24
      Hint = 
        'Specify the name of the output file where the combined risk/secu' +
        'rity scores will be saved'
      Caption = 'Change Output File...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = ChangeOutputButtonClick
    end
  end
  object CombineFilesRunBox: TGroupBox
    Left = 8
    Top = 402
    Width = 569
    Height = 47
    Caption = 'Step 3:  Combine Files'
    TabOrder = 2
    object CombineButton: TButton
      Left = 16
      Top = 15
      Width = 139
      Height = 24
      Hint = 
        'Read the input files, combine them, and save the resulting combi' +
        'ned output file'
      Caption = 'COMBINE FILES'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = CombineButtonClick
    end
  end
  object CancelButton: TBitBtn
    Left = 336
    Top = 456
    Width = 201
    Height = 27
    Caption = 'Cancel - Close without Saving'
    TabOrder = 3
    OnClick = CancelButtonClick
    Kind = bkCancel
  end
  object DoneButton: TBitBtn
    Left = 120
    Top = 456
    Width = 161
    Height = 25
    Caption = 'Finished - Save Settings'
    ModalResult = 1
    TabOrder = 4
    OnClick = DoneButtonClick
  end
  object SaveDialog1: TSaveDialog
    Left = 712
    Top = 352
  end
  object OpenDialog1: TOpenDialog
    Left = 680
    Top = 352
  end
end
