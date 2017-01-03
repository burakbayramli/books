object VariableInformationForm: TVariableInformationForm
  Left = 65
  Top = 89
  Width = 689
  Height = 150
  Caption = 'Variable Information'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object VarPanel: TPanel
    Left = 12
    Top = 12
    Width = 625
    Height = 57
    BevelInner = bvLowered
    Color = clSilver
    TabOrder = 0
    object VariableNameLabel: TLabel
      Left = 8
      Top = 8
      Width = 69
      Height = 13
      Hint = 
        'Variable name, up to 8 characters in length.  Must start with al' +
        'phabetic character (a..z)'
      Caption = 'Variable Name'
    end
    object VarDataTypeLabel: TLabel
      Left = 104
      Top = 8
      Width = 62
      Height = 13
      Hint = 
        'Specify whether variable contains integer, real (i.e. decimal), ' +
        'or string (i.e. alphabetic) data'
      Caption = 'Type of Data'
    end
    object VariableUnitLabel: TLabel
      Left = 216
      Top = 8
      Width = 60
      Height = 13
      Hint = 
        'Specify whether the variable is annual, country-year, ordered dy' +
        'adic (e.g. A'#39's dependence on B), or unordered dyadic (e.g. A and' +
        ' B are allied) '
      Caption = 'Variable Unit'
    end
    object ComplementaryVarLabel: TLabel
      Left = 352
      Top = 8
      Width = 144
      Height = 13
      Hint = 
        'For directed-dyadic and monadic variables, enter the name of var' +
        'iable for the other direction (e.g. DemB might be the complement' +
        ' of DemA)'
      Caption = 'Complementary Variable Name'
    end
    object MissingValueLable: TLabel
      Left = 520
      Top = 8
      Width = 65
      Height = 13
      Hint = 'Enter an integer missing value for the variable (e.g. -9)'
      Caption = 'Missing Value'
    end
    object NameMask: TMaskEdit
      Left = 8
      Top = 28
      Width = 73
      Height = 21
      Hint = 
        'Variable name, up to 8 characters in length.  Must start with al' +
        'phabetic character (a..z)'
      EditMask = 'Laaaaaaa;0;_'
      MaxLength = 8
      TabOrder = 0
    end
    object DataTypeCombo: TComboBox
      Left = 104
      Top = 28
      Width = 89
      Height = 21
      Hint = 
        'Specify whether variable contains integer, real (i.e. decimal), ' +
        'or string (i.e. alphabetic) data'
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      Items.Strings = (
        'Integer'
        'Real'
        'String')
    end
    object VarUnitCombo: TComboBox
      Left = 216
      Top = 28
      Width = 113
      Height = 21
      Hint = 
        'Specify whether the variable is annual, country-year, ordered dy' +
        'adic (e.g. A'#39's dependence on B), or unordered dyadic (e.g. A and' +
        ' B are allied) '
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      Items.Strings = (
        'Specify Unit...'
        'Year Identifier'
        'Country Code 1 Identifier'
        'Country Code 2 Identifer'
        'Annual Data'
        'Monadic (Country Data)'
        'Dyadic, and ordered'
        'Dyadic, but unordered')
    end
    object CompNameEdit: TMaskEdit
      Left = 352
      Top = 28
      Width = 73
      Height = 21
      Hint = 
        'For directed-dyadic and monadic variables, enter the name of var' +
        'iable for the other direction (e.g. DemB might be the complement' +
        ' of DemA)'
      EditMask = 'laaaaaaaa;0;_'
      MaxLength = 9
      TabOrder = 3
    end
    object MissingEdit: TEdit
      Left = 520
      Top = 28
      Width = 57
      Height = 21
      Hint = 'Enter an integer missing value for the variable (e.g. -9)'
      TabOrder = 4
    end
  end
  object VarOKBtn: TBitBtn
    Left = 128
    Top = 79
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = VarOKBtnClick
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
  object VarCancelBtn: TBitBtn
    Left = 268
    Top = 79
    Width = 85
    Height = 25
    TabOrder = 2
    OnClick = VarCancelBtnClick
    Kind = bkCancel
  end
  object HelpBtn: TBitBtn
    Left = 408
    Top = 79
    Width = 81
    Height = 25
    TabOrder = 3
    OnClick = HelpBtnClick
    Kind = bkHelp
  end
end
