object UserDataSetSubForm2: TUserDataSetSubForm2
  Left = 89
  Top = 114
  Width = 645
  Height = 545
  Caption = 'Define Variables For User Dataset'
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
    Left = 136
    Top = 478
    Width = 91
    Height = 27
    Caption = 'Done'
    TabOrder = 0
    OnClick = DoneBtnClick
    Kind = bkOK
  end
  object CancelBtn: TBitBtn
    Left = 268
    Top = 478
    Width = 85
    Height = 27
    TabOrder = 1
    Kind = bkCancel
  end
  object HelpBtn: TBitBtn
    Left = 388
    Top = 478
    Width = 98
    Height = 27
    TabOrder = 2
    OnClick = HelpBtnClick
    Kind = bkHelp
  end
  object UserSub2Panel: TPanel
    Left = 24
    Top = 24
    Width = 584
    Height = 438
    BevelInner = bvLowered
    Color = clSilver
    TabOrder = 3
    object GroupBox1: TGroupBox
      Left = 12
      Top = 12
      Width = 553
      Height = 104
      Caption = ' Basic Settings '
      TabOrder = 0
      object UnitLabel: TLabel
        Left = 16
        Top = 23
        Width = 121
        Height = 13
        Hint = 'Select unit of analysis of dataset from pulldown list'
        Caption = 'Dataset Unit of Analysis:  '
      end
      object NumVarsLabel: TLabel
        Left = 16
        Top = 50
        Width = 104
        Height = 13
        Hint = 'Provide number of variables.'
        Caption = 'Number of Variables:  '
      end
      object LabelLineCheckBox: TCheckBox
        Left = 16
        Top = 76
        Width = 209
        Height = 17
        Hint = 'Check if the first line of the dataset contains variable names'
        Caption = 'Variable Names in Line 1 of Data File'
        TabOrder = 0
        OnClick = LabelLineCheckBoxClick
      end
      object UnitComboBox: TComboBox
        Left = 141
        Top = 20
        Width = 160
        Height = 21
        Hint = 
          'Unit of analysis of the dataset (annual data, country-year data,' +
          ' etc.)'
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = UnitComboBoxChange
        Items.Strings = (
          'None'
          'Year (Annual Data)'
          'Country-Year (Monadic Data)'
          'Nondirected Dyad-Year '
          'Directed Dyad-Year ')
      end
      object NumVarsMaskEdit: TMaskEdit
        Left = 128
        Top = 46
        Width = 73
        Height = 21
        Hint = 'Number of variables in dataset'
        EditMask = '999;0;'
        MaxLength = 3
        TabOrder = 2
      end
      object AutoDetectNameButton: TButton
        Left = 228
        Top = 74
        Width = 306
        Height = 22
        Hint = 'Click to try reading user defined variable names from dataset'
        Caption = 'Attempt to Read Variable Names from Line 1 of Dataset...'
        Enabled = False
        TabOrder = 3
        OnClick = AutoDetectNameButtonClick
      end
    end
    object VarGroupBox: TGroupBox
      Left = 12
      Top = 134
      Width = 555
      Height = 269
      Caption = 'Variable Information:  0 variables currently defined'
      TabOrder = 1
      object VarInfoStringGrid: TStringGrid
        Left = 13
        Top = 25
        Width = 527
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
        Left = 309
        Top = 236
        Width = 121
        Height = 21
        Hint = 
          'Move the selected variable later in the order it appears in the ' +
          'dataset'
        Caption = 'Move Variable Down'
        TabOrder = 1
        OnClick = MoveDownVariableButtonClick
      end
      object MoveUpVariableButton: TButton
        Left = 127
        Top = 236
        Width = 121
        Height = 21
        Hint = 
          'Move the selected variable earlier in the order it appears in th' +
          'e dataset'
        Caption = 'Move Variable Up'
        TabOrder = 2
        OnClick = MoveUpVariableButtonClick
      end
      object DeleteVarButton: TButton
        Left = 384
        Top = 206
        Width = 156
        Height = 20
        Hint = 'Delete the selected variable from the dataset specification'
        Caption = 'Delete Selected Variable Info'
        TabOrder = 3
        OnClick = DeleteVarButtonClick
      end
      object EditVarButton: TButton
        Left = 200
        Top = 206
        Width = 156
        Height = 20
        Hint = 'Edit the selected variable'#39's specification for name, type, etc.'
        Caption = 'Edit Selected Variable Info'
        TabOrder = 4
        OnClick = EditVarButtonClick
      end
      object AddVariableButton: TButton
        Left = 14
        Top = 206
        Width = 156
        Height = 20
        Hint = 'Define an additional variable in data set'
        Caption = 'Add New Variable Info'
        TabOrder = 5
        OnClick = AddVariableButtonClick
      end
    end
  end
end
