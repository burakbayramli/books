object UserDataSetSubForm3: TUserDataSetSubForm3
  Left = 100
  Top = 99
  Width = 413
  Height = 268
  Caption = 'Define Dataset Time Span and Number of Cases'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object DoneBtn: TBitBtn
    Left = 33
    Top = 177
    Width = 91
    Height = 26
    Caption = 'Done'
    TabOrder = 0
    OnClick = DoneBtnClick
    Kind = bkOK
  end
  object CancelBtn: TBitBtn
    Left = 150
    Top = 177
    Width = 85
    Height = 26
    TabOrder = 1
    Kind = bkCancel
  end
  object HelpBtn: TBitBtn
    Left = 259
    Top = 177
    Width = 99
    Height = 26
    TabOrder = 2
    OnClick = HelpBtnClick
    Kind = bkHelp
  end
  object Panel1: TPanel
    Left = 7
    Top = 7
    Width = 377
    Height = 163
    BevelInner = bvLowered
    Color = clSilver
    TabOrder = 3
    object FirstYearLabel: TLabel
      Left = 24
      Top = 66
      Width = 104
      Height = 13
      Hint = 'Year of earliest observation in dataset'
      Caption = 'First Year in Dataset:  '
    end
    object LastYearLabel: TLabel
      Left = 24
      Top = 98
      Width = 105
      Height = 13
      Hint = 'Year of latest observation in dataset'
      Caption = 'Last Year in Dataset:  '
    end
    object NumCasesLabel: TLabel
      Left = 24
      Top = 131
      Width = 141
      Height = 13
      Hint = 'Number of cases (data lines) in dataset'
      Caption = 'Number of Cases in Dataset:  '
    end
    object FirstYearMaskEdit: TMaskEdit
      Left = 135
      Top = 63
      Width = 41
      Height = 21
      Hint = 'Year of earliest observation in dataset'
      EditMask = '0000'
      MaxLength = 4
      TabOrder = 0
      Text = '    '
      OnChange = FirstYearMaskEditChange
    end
    object LastYearMaskEdit: TMaskEdit
      Left = 135
      Top = 95
      Width = 41
      Height = 21
      Hint = 'Year of latest observation in dataset'
      EditMask = '0000;1;_'
      MaxLength = 4
      TabOrder = 1
      Text = '    '
      OnChange = LastYearMaskEditChange
    end
    object NumCasesMaskEdit: TMaskEdit
      Left = 176
      Top = 128
      Width = 65
      Height = 21
      Hint = 'Number of cases (data lines) in dataset'
      EditMask = '99999999999;0;_'
      MaxLength = 11
      TabOrder = 2
      OnChange = NumCasesMaskEditChange
    end
    object AutoDetectYearCaseButton: TButton
      Left = 14
      Top = 17
      Width = 344
      Height = 23
      Hint = 
        'Click to scan dataset for first year, last year, and to count ca' +
        'ses (lines)'
      Caption = 'Attempt to Detect Year Range and # Cases from Dataset...'
      Enabled = False
      TabOrder = 3
      OnClick = AutoDetectYearCaseButtonClick
    end
  end
end
