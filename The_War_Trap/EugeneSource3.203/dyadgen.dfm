object DyadicGen: TDyadicGen
  Left = 661
  Top = 599
  Caption = 'Generate Dyadic MID data'
  ClientHeight = 343
  ClientWidth = 589
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = SetLabels
  PixelsPerInch = 96
  TextHeight = 13
  object InputTitleLabel: TLabel
    Left = 16
    Top = 8
    Width = 63
    Height = 16
    Caption = 'Input Files:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object File1Label: TLabel
    Left = 38
    Top = 56
    Width = 48
    Height = 13
    Caption = 'File1Label'
  end
  object File2Label: TLabel
    Left = 38
    Top = 96
    Width = 48
    Height = 13
    Caption = 'File2Label'
  end
  object File3Label: TLabel
    Left = 38
    Top = 136
    Width = 48
    Height = 13
    Caption = 'File3Label'
  end
  object File4Label: TLabel
    Left = 38
    Top = 176
    Width = 48
    Height = 13
    Caption = 'File4Label'
  end
  object OutputTitleLabel: TLabel
    Left = 16
    Top = 216
    Width = 111
    Height = 16
    Caption = 'Output Destination:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object MID_File_A_Header: TLabel
    Left = 32
    Top = 40
    Width = 79
    Height = 13
    Caption = 'MID Case File A:'
  end
  object MID_File_B_Header: TLabel
    Left = 32
    Top = 80
    Width = 80
    Height = 13
    Caption = 'MID Actor File B:'
  end
  object MID_File_C_Header: TLabel
    Left = 32
    Top = 120
    Width = 83
    Height = 13
    Caption = 'MID Name File C:'
  end
  object Incident_header: TLabel
    Left = 32
    Top = 160
    Width = 134
    Height = 13
    Caption = 'Incident Participant data file:'
  end
  object FileDestination: TEdit
    Left = 32
    Top = 240
    Width = 377
    Height = 21
    ReadOnly = True
    TabOrder = 2
    Text = 'c:\eugene\MIDDyadic_v3.02.csv'
  end
  object ChangeFileButtonClick: TButton
    Left = 448
    Top = 240
    Width = 105
    Height = 17
    Caption = 'Change Output File'
    TabOrder = 0
    OnClick = ChangeFileButtonClickClick
  end
  object GenerateButton: TButton
    Left = 160
    Top = 288
    Width = 73
    Height = 33
    Caption = 'Generate'
    TabOrder = 1
    OnClick = GenerateButtonClick
  end
  object CancelButton: TButton
    Left = 368
    Top = 288
    Width = 65
    Height = 33
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = CancelButtonClick
  end
  object ChangeMidAButton: TButton
    Left = 448
    Top = 56
    Width = 105
    Height = 17
    Caption = 'Change MID A File'
    TabOrder = 4
    OnClick = ChangeMidAButtonClick
  end
  object ChangeMidBButton: TButton
    Left = 448
    Top = 96
    Width = 105
    Height = 17
    Caption = 'Change MID B File'
    TabOrder = 5
    OnClick = ChangeMidBButtonClick
  end
  object ChangeMidCButton: TButton
    Left = 448
    Top = 136
    Width = 105
    Height = 17
    Caption = 'Change MID C File'
    TabOrder = 6
    OnClick = ChangeMidBButtonClick
  end
  object ChangeIncidentFileButton: TButton
    Left = 448
    Top = 176
    Width = 105
    Height = 17
    Caption = 'Change Inc-Part File'
    TabOrder = 7
    OnClick = ChangeIncidentFileButtonClick
  end
  object DyadicSaveDialog: TSaveDialog
    Left = 552
    Top = 296
  end
  object DyadicOpenDialog1: TOpenDialog
    Left = 512
    Top = 304
  end
end
