object Output_Options: TOutput_Options
  Left = 550
  Top = 49
  Caption = 'Output Options Available'
  ClientHeight = 586
  ClientWidth = 637
  Color = clActiveBorder
  ParentFont = True
  OldCreateOrder = True
  OnCreate = GenerateUserVarPanels
  OnShow = OutputFormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 26
    Top = 43
    Width = 173
    Height = 13
    Caption = 'Proportion Sampled, Dispute Dyads:'
  end
  object PagedOutput: TPageControl
    Left = 12
    Top = 12
    Width = 608
    Height = 500
    ActivePage = CaseSelectionPage
    MultiLine = True
    TabOrder = 0
    object OutputDestinationSheet: TTabSheet
      Caption = 'Files / Format'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object OutputDestinationPanel: TPanel
        Left = 12
        Top = 12
        Width = 282
        Height = 130
        BevelInner = bvLowered
        Color = clSilver
        TabOrder = 0
        object OutputDestinationLabel: TLabel
          Left = 12
          Top = 12
          Width = 260
          Height = 17
          AutoSize = False
          Caption = 'Output Destination'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object ToFileRadioDial: TRadioButton
          Left = 12
          Top = 34
          Width = 100
          Height = 16
          Caption = 'To File:'
          TabOrder = 0
          OnClick = disk1Click
        end
        object FileDisplay: TEdit
          Left = 30
          Top = 55
          Width = 240
          Height = 21
          TabStop = False
          Color = clSilver
          ReadOnly = True
          TabOrder = 1
          Text = 'FileDisplay'
        end
        object ToScreenRadioDial: TRadioButton
          Left = 12
          Top = 80
          Width = 100
          Height = 17
          Caption = 'To Screen'
          TabOrder = 2
          OnClick = ToScreenRadioDialClick
        end
        object ToPrinterRadioDial: TRadioButton
          Left = 12
          Top = 98
          Width = 100
          Height = 16
          Caption = 'To Printer'
          TabOrder = 3
          OnClick = Printer1Click
        end
        object ChangeOutputFileButton: TPanel
          Left = 150
          Top = 34
          Width = 120
          Height = 18
          BevelInner = bvRaised
          BevelOuter = bvNone
          BevelWidth = 2
          Caption = 'Change Output File...'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          OnClick = FileChangeButtonClick
          OnMouseDown = ChangeOutputFileMouseDown
          OnMouseUp = ChangeOutputFileMouseUp
        end
        object PrinterSetupButton: TPanel
          Left = 150
          Top = 98
          Width = 120
          Height = 18
          BevelInner = bvRaised
          BevelOuter = bvNone
          BevelWidth = 2
          Caption = 'Printer Setup...'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 5
          OnClick = PrinterSetupButtonClick
          OnMouseDown = PrinterSetupMouseDown
          OnMouseUp = PrinterSetupMouseUp
        end
      end
      object VariableSeparatorPanel: TPanel
        Left = 306
        Top = 90
        Width = 282
        Height = 102
        BevelInner = bvLowered
        Color = clSilver
        TabOrder = 1
        object VariableSeparatorLabel: TLabel
          Left = 12
          Top = 12
          Width = 260
          Height = 17
          AutoSize = False
          Caption = 'Variable Separator'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object SeparateWithTabs: TRadioButton
          Left = 12
          Top = 34
          Width = 260
          Height = 16
          Caption = 'Separate Variables With Tabs'
          TabOrder = 0
        end
        object SeparateWithSpaces: TRadioButton
          Left = 12
          Top = 52
          Width = 260
          Height = 16
          Caption = 'Separate Variables With Spaces'
          TabOrder = 1
        end
        object SeparateWithCommas: TRadioButton
          Left = 12
          Top = 70
          Width = 260
          Height = 16
          Caption = 'Separate Variables With Commas'
          TabOrder = 2
        end
      end
      object CommandFilesPanel: TPanel
        Left = 12
        Top = 154
        Width = 282
        Height = 120
        BevelInner = bvLowered
        Color = clSilver
        TabOrder = 2
        object CommandFileSelectLabel: TLabel
          Left = 12
          Top = 12
          Width = 260
          Height = 17
          AutoSize = False
          Caption = 'Create Command File(s)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object SPSSCommandFileCheckBox: TCheckBox
          Left = 12
          Top = 34
          Width = 260
          Height = 16
          Caption = 'SPSS Command File'
          Enabled = False
          TabOrder = 0
        end
        object StataCommandFileCheckBox: TCheckBox
          Left = 12
          Top = 52
          Width = 260
          Height = 16
          Caption = 'STATA Command File'
          Enabled = False
          TabOrder = 1
        end
        object LIMDEPCommandFileCheckBox: TCheckBox
          Left = 12
          Top = 70
          Width = 260
          Height = 16
          Caption = 'Limdep Command File'
          Enabled = False
          TabOrder = 2
        end
        object commandfileOnlyCheckBox: TCheckBox
          Left = 12
          Top = 88
          Width = 260
          Height = 16
          Caption = 'Create Command File Only (no data output)'
          Enabled = False
          TabOrder = 3
        end
      end
      object HeaderInfoPanel: TPanel
        Left = 306
        Top = 12
        Width = 282
        Height = 66
        BevelInner = bvLowered
        Color = clSilver
        TabOrder = 3
        object HeaderInfoLabel: TLabel
          Left = 12
          Top = 12
          Width = 260
          Height = 17
          AutoSize = False
          Caption = 'Header Information'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Header: TCheckBox
          Left = 12
          Top = 34
          Width = 260
          Height = 16
          Caption = 'Include Header Line Containing Variable Names'
          TabOrder = 0
        end
      end
    end
    object CaseSelectionPage: TTabSheet
      Caption = 'Population of Cases'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object YearsOutput: TPanel
        Left = 12
        Top = 12
        Width = 282
        Height = 258
        BevelInner = bvLowered
        Color = clSilver
        TabOrder = 0
        object YearsOutputLabel: TLabel
          Left = 12
          Top = 12
          Width = 258
          Height = 17
          AutoSize = False
          Caption = 'Years to Include in Output'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object nationdatlabel: TLabel
          Left = 12
          Top = 102
          Width = 126
          Height = 13
          Caption = 'Statehood Data Available:'
        end
        object limitinglabel: TLabel
          Left = 30
          Top = 118
          Width = 72
          Height = 13
          Caption = '(limiting factor)'
        end
        object nationyear1label: TLabel
          Left = 156
          Top = 102
          Width = 6
          Height = 13
          Caption = 'a'
        end
        object nationyear2label: TLabel
          Left = 230
          Top = 102
          Width = 6
          Height = 13
          Caption = 'a'
        end
        object AllianceYrs: TLabel
          Left = 12
          Top = 136
          Width = 116
          Height = 13
          Caption = 'Alliance Years Available:'
        end
        object CapYrs: TLabel
          Left = 12
          Top = 154
          Width = 127
          Height = 13
          Caption = 'Capability Years Available:'
        end
        object AllYrFrom: TLabel
          Left = 156
          Top = 136
          Width = 6
          Height = 13
          Caption = 'a'
        end
        object CapYrFrom: TLabel
          Left = 156
          Top = 154
          Width = 23
          Height = 13
          Caption = 'capa'
        end
        object AllYrTo: TLabel
          Left = 230
          Top = 136
          Width = 6
          Height = 13
          Caption = 'a'
        end
        object CApYrTo: TLabel
          Left = 230
          Top = 154
          Width = 6
          Height = 13
          Caption = 'a'
        end
        object RiskYr: TLabel
          Left = 12
          Top = 172
          Width = 99
          Height = 13
          Caption = 'Risk Years Available:'
        end
        object EUYr: TLabel
          Left = 12
          Top = 190
          Width = 93
          Height = 13
          Caption = 'EU Years Available:'
        end
        object MIDYearLabel: TLabel
          Left = 12
          Top = 208
          Width = 99
          Height = 13
          Caption = 'MID Years Available:'
        end
        object polityyearlabel: TLabel
          Left = 12
          Top = 226
          Width = 115
          Height = 13
          Caption = 'Polity 3 Years Available:'
        end
        object polityyear1label: TLabel
          Left = 156
          Top = 226
          Width = 20
          Height = 13
          Caption = 'pola'
        end
        object MIDLabelFrom: TLabel
          Left = 156
          Top = 208
          Width = 22
          Height = 13
          Caption = 'mida'
        end
        object EUYrFrom: TLabel
          Left = 156
          Top = 190
          Width = 6
          Height = 13
          Caption = 'a'
        end
        object RiskYrFrom: TLabel
          Left = 156
          Top = 172
          Width = 22
          Height = 13
          Caption = 'riska'
        end
        object RiskYrTo: TLabel
          Left = 230
          Top = 172
          Width = 22
          Height = 13
          Caption = 'riskb'
        end
        object EUYrTo: TLabel
          Left = 230
          Top = 190
          Width = 6
          Height = 13
          Caption = 'a'
        end
        object MIDLabelTo: TLabel
          Left = 230
          Top = 208
          Width = 22
          Height = 13
          Caption = 'midb'
        end
        object polityyear2label: TLabel
          Left = 230
          Top = 226
          Width = 20
          Height = 13
          Caption = 'polb'
        end
        object All_Yrs: TRadioButton
          Left = 12
          Top = 34
          Width = 258
          Height = 16
          Caption = 'All Years'
          TabOrder = 0
          OnClick = All_YrsClick
        end
        object Specified_Yrs: TRadioButton
          Left = 12
          Top = 70
          Width = 258
          Height = 16
          Caption = 'Specified Range:  From                      to'
          TabOrder = 1
          OnClick = Specified_YrsClick
        end
        object Edit1: TEdit
          Left = 154
          Top = 69
          Width = 40
          Height = 18
          AutoSize = False
          TabOrder = 2
          OnExit = YearEditExit
        end
        object Edit2: TEdit
          Left = 228
          Top = 69
          Width = 40
          Height = 18
          AutoSize = False
          TabOrder = 3
          OnExit = YearEditExit
        end
        object Panel2: TPanel
          Left = 24
          Top = 58
          Width = 234
          Height = 3
          BevelInner = bvLowered
          BevelOuter = bvNone
          Color = clSilver
          TabOrder = 4
        end
      end
      object SelfReferencePanel: TPanel
        Left = 14
        Top = 282
        Width = 282
        Height = 66
        BevelInner = bvLowered
        Color = clSilver
        TabOrder = 1
        object SelfReferenceLabel: TLabel
          Left = 12
          Top = 12
          Width = 258
          Height = 17
          AutoSize = False
          Caption = 'Self-Referencing Dyads'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object PrintAACheckBox: TCheckBox
          Left = 12
          Top = 34
          Width = 258
          Height = 16
          Caption = 'Include Dyads i vs. i'
          TabOrder = 0
        end
      end
      object CaseSelectionPanel: TPanel
        Left = 306
        Top = 12
        Width = 282
        Height = 309
        BevelInner = bvLowered
        Color = clSilver
        TabOrder = 2
        object CaseSelectionLabel: TLabel
          Left = 12
          Top = 12
          Width = 260
          Height = 17
          AutoSize = False
          Caption = 'Dyad-Years Included'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object alldyadsfromallstates: TRadioButton
          Left = 12
          Top = 34
          Width = 205
          Height = 16
          Caption = 'All States'
          TabOrder = 0
          OnClick = alldyadsfromallstatesClick
        end
        object Allgpvsgpdyads: TRadioButton
          Left = 12
          Top = 61
          Width = 189
          Height = 16
          Caption = 'All Major Powers '
          TabOrder = 1
          OnClick = AllgpvsgpdyadsClick
        end
        object allgpvsany: TRadioButton
          Left = 12
          Top = 88
          Width = 258
          Height = 16
          Caption = 'All Major Power vs. Any State Dyads'
          TabOrder = 2
          OnClick = allgpvsanyClick
        end
        object allcontg: TRadioButton
          Left = 12
          Top = 115
          Width = 200
          Height = 16
          Caption = 'All Contiguous Dyads'
          TabOrder = 3
          Visible = False
          OnClick = allcontgClick
        end
        object MaximumDistanceButton: TRadioButton
          Left = 12
          Top = 196
          Width = 200
          Height = 16
          Caption = 'Within Maximum Distance (mi.)'
          TabOrder = 4
          OnClick = maxdistclick
        end
        object SpecifiedRegionButton: TRadioButton
          Left = 12
          Top = 169
          Width = 200
          Height = 16
          Caption = 'Within Selected Regions'
          TabOrder = 5
          OnClick = spec_regionClick
        end
        object PolRelevant: TRadioButton
          Left = 12
          Top = 142
          Width = 200
          Height = 16
          Caption = 'Politically Relevant Dyads'
          TabOrder = 6
          OnClick = PolRelevantClick
        end
        object specific_dyads: TRadioButton
          Left = 12
          Top = 223
          Width = 200
          Height = 16
          Caption = 'Specific Set of Countries'
          TabOrder = 7
          OnClick = specific_dyadsClick
        end
        object DyadsFromFileButton: TRadioButton
          Left = 12
          Top = 250
          Width = 200
          Height = 16
          Caption = 'Dyads Read from User File'
          TabOrder = 8
          OnClick = DyadsFromFileButtonClick
        end
        object DistanceEditBox: TMaskEdit
          Left = 209
          Top = 195
          Width = 60
          Height = 18
          AutoSize = False
          TabOrder = 9
          Text = '12500'
          OnExit = SetMaxDistance
        end
        object ChangeContiguityButton1: TPanel
          Left = 180
          Top = 114
          Width = 90
          Height = 18
          BevelInner = bvRaised
          BevelOuter = bvNone
          BevelWidth = 2
          Caption = 'Set Contiguity'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 10
          OnClick = ChangeContiguityButton1Click
          OnMouseDown = Contiguity1MouseDown
          OnMouseUp = Contiguity1MouseUp
        end
        object SelectRegionButton: TPanel
          Left = 180
          Top = 168
          Width = 90
          Height = 18
          BevelInner = bvRaised
          BevelOuter = bvNone
          BevelWidth = 2
          Caption = 'Select Regions'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 11
          OnClick = Select_RegionButtonClick
          OnMouseDown = SelectRegionMouseDown
          OnMouseUp = SelectRegionMouseUp
        end
        object ChangeContiguityButton2: TPanel
          Left = 180
          Top = 141
          Width = 90
          Height = 18
          BevelInner = bvRaised
          BevelOuter = bvNone
          BevelWidth = 2
          Caption = 'Set Contiguity'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 12
          OnClick = ChangeContiguityButton2Click
          OnMouseDown = Contiguity2MouseDown
          OnMouseUp = Contiguity2MouseUp
        end
        object SetCountriesButton: TPanel
          Left = 180
          Top = 222
          Width = 90
          Height = 18
          BevelInner = bvRaised
          BevelOuter = bvNone
          BevelWidth = 2
          Caption = 'Select Countries'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 13
          OnClick = ChangeCountriesButtonClick
          OnMouseDown = SetCountriesMouseDown
          OnMouseUp = SetCountriesMouseUp
        end
        object ChangeInputFileButton: TPanel
          Left = 180
          Top = 249
          Width = 90
          Height = 18
          BevelInner = bvRaised
          BevelOuter = bvNone
          BevelWidth = 2
          Caption = 'Set File...'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 14
          OnClick = changeuserdyadfilebuttonClick
          OnMouseDown = ChangeInputFileMouseDown
          OnMouseUp = ChangeInputFileMouseUp
        end
        object userdyadfiledisplay: TEdit
          Left = 30
          Top = 271
          Width = 240
          Height = 18
          TabStop = False
          AutoSize = False
          Color = clSilver
          ReadOnly = True
          TabOrder = 15
          Text = 'userdyadfiledisplay'
        end
      end
    end
    object SamplingSheet: TTabSheet
      Caption = 'Sampling'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object SamplingPanel: TPanel
        Left = 117
        Top = 12
        Width = 366
        Height = 165
        BevelInner = bvLowered
        Color = clSilver
        TabOrder = 0
        object NonDisputeSampleLabel: TLabel
          Left = 31
          Top = 85
          Width = 221
          Height = 13
          Caption = 'Proportion Sampled, Non-Dispute Dyad Years:'
        end
        object disputedyadsamplelabel: TLabel
          Left = 31
          Top = 109
          Width = 198
          Height = 13
          Caption = 'Proportion Sampled, Dispute Dyad Years:'
        end
        object SamplingLabel: TLabel
          Left = 12
          Top = 12
          Width = 278
          Height = 17
          AutoSize = False
          Caption = 'Sampling Options'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object NoSampleBtn: TRadioButton
          Left = 12
          Top = 34
          Width = 278
          Height = 16
          Caption = 'No Sampling, Include All Appropriate Cases'
          TabOrder = 0
          TabStop = True
          OnClick = NoSampleBtnClick
        end
        object SamplingYesBtn: TRadioButton
          Left = 12
          Top = 61
          Width = 278
          Height = 16
          Caption = 'Stratified Random Sampling:'
          TabOrder = 1
          OnClick = SamplingYesBtnClick
        end
        object RandSeedCheckBox: TCheckBox
          Left = 24
          Top = 133
          Width = 240
          Height = 16
          Caption = 'Use Specific Random Number Seed:'
          TabOrder = 2
        end
        object NonDispSampleEdit: TMaskEdit
          Left = 316
          Top = 84
          Width = 38
          Height = 21
          EditMask = '9.999;1;_'
          MaxLength = 5
          TabOrder = 3
          Text = ' .   '
        end
        object DispSampleEdit: TMaskEdit
          Left = 316
          Top = 108
          Width = 38
          Height = 21
          EditMask = '9.999;1;'
          MaxLength = 5
          TabOrder = 4
          Text = ' .   '
        end
        object RandomSeedEdit: TMaskEdit
          Left = 284
          Top = 132
          Width = 70
          Height = 21
          EditMask = '#99999999;0;_'
          MaxLength = 9
          TabOrder = 5
        end
      end
    end
    object VariableSheet: TTabSheet
      Caption = 'Variables'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object VariableTabsControl: TPageControl
        Left = 12
        Top = 8
        Width = 582
        Height = 460
        ActivePage = ConflictTabSheet
        TabOrder = 0
        object GeneralSheet: TTabSheet
          Caption = 'General'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object GeographyPanel: TPanel
            Left = 12
            Top = 198
            Width = 269
            Height = 156
            BevelInner = bvLowered
            Color = clSilver
            TabOrder = 2
            object GeographyLabel: TLabel
              Left = 12
              Top = 12
              Width = 62
              Height = 13
              Caption = 'Geography'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object RelRegionCheckBox: TCheckBox
              Left = 12
              Top = 34
              Width = 240
              Height = 16
              Caption = 'Relevant Region'
              Color = clSilver
              ParentColor = False
              TabOrder = 0
            end
            object HomeRegionCheckBox: TCheckBox
              Left = 12
              Top = 52
              Width = 240
              Height = 16
              Caption = 'Home Region'
              Color = clSilver
              ParentColor = False
              TabOrder = 1
            end
            object MarkPolRelevantBox: TCheckBox
              Left = 12
              Top = 70
              Width = 240
              Height = 16
              Caption = 'Politically Relevant'
              Color = clSilver
              ParentColor = False
              TabOrder = 2
            end
            object ContiguityCheckBox: TCheckBox
              Left = 12
              Top = 88
              Width = 240
              Height = 16
              Caption = 'Direct Contiguity Level (1-6)'
              Color = clSilver
              ParentColor = False
              TabOrder = 3
            end
            object ColonialContiguityCheckBox: TCheckBox
              Left = 12
              Top = 106
              Width = 240
              Height = 16
              Caption = 'Colonial Contiguity Level (1-6)'
              Color = clSilver
              ParentColor = False
              TabOrder = 4
            end
            object DistanceVarBox: TCheckBox
              Left = 12
              Top = 124
              Width = 240
              Height = 16
              Caption = 'Distance between States'
              Color = clSilver
              ParentColor = False
              TabOrder = 5
            end
            object ChangePolRelButton: TPanel
              Left = 197
              Top = 69
              Width = 60
              Height = 18
              BevelInner = bvRaised
              BevelOuter = bvNone
              BevelWidth = 2
              Caption = 'Options'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 6
              OnClick = ChangeContiguityButton3Click
              OnMouseDown = PolRelMouseDown
              OnMouseUp = PolRelMouseUp
            end
            object DistanceOptionsButton: TPanel
              Left = 197
              Top = 123
              Width = 60
              Height = 18
              BevelWidth = 2
              Caption = 'Options'
              TabOrder = 7
              OnClick = DistanceVarOptionsButtonClick
              OnMouseDown = DistOptionsMouseDown
              OnMouseUp = DistOptionsMouseUp
            end
          end
          object CapabilitiesPanel: TPanel
            Left = 12
            Top = 12
            Width = 269
            Height = 174
            BevelInner = bvLowered
            Color = clSilver
            TabOrder = 1
            object CapabilitiesLabel: TLabel
              Left = 12
              Top = 12
              Width = 204
              Height = 13
              Caption = 'Identification and Basic Capabilities'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object CcodeVarBox: TCheckBox
              Left = 12
              Top = 34
              Width = 240
              Height = 16
              Caption = 'CCode'
              Color = clSilver
              ParentColor = False
              TabOrder = 0
              OnClick = CcodeVarBoxClick
            end
            object Abbrevcheckbox: TCheckBox
              Left = 12
              Top = 52
              Width = 240
              Height = 16
              Caption = 'Abbreviation'
              Color = clSilver
              ParentColor = False
              TabOrder = 1
            end
            object YearVarBox: TCheckBox
              Left = 12
              Top = 70
              Width = 240
              Height = 16
              Caption = 'Year'
              Color = clSilver
              ParentColor = False
              TabOrder = 2
              OnClick = YearVarBoxClick
            end
            object natcapbox: TCheckBox
              Left = 12
              Top = 88
              Width = 240
              Height = 16
              Caption = 'Capabilities (CINC score)'
              Color = clSilver
              ParentColor = False
              TabOrder = 3
            end
            object rawcapbox: TCheckBox
              Left = 12
              Top = 106
              Width = 240
              Height = 16
              Caption = 'Capabilities (components)'
              Color = clSilver
              ParentColor = False
              TabOrder = 4
            end
            object MajorPowerCheckBox: TCheckBox
              Left = 12
              Top = 124
              Width = 240
              Height = 16
              Caption = 'Major Power Status'
              Color = clSilver
              ParentColor = False
              TabOrder = 5
            end
            object ExistTimeCheckBox: TCheckBox
              Left = 12
              Top = 142
              Width = 240
              Height = 16
              Caption = 'Dyad Duration'
              Color = clSilver
              ParentColor = False
              TabOrder = 6
            end
          end
          object SystemCharsPanel: TPanel
            Left = 293
            Top = 12
            Width = 269
            Height = 174
            BevelInner = bvLowered
            Color = clSilver
            TabOrder = 0
            object SystemCharsLabel: TLabel
              Left = 12
              Top = 12
              Width = 129
              Height = 13
              Caption = 'System Characteristics'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object StatesInSystemBox: TCheckBox
              Left = 12
              Top = 34
              Width = 240
              Height = 16
              Caption = 'Number of States in the International System'
              Color = clSilver
              ParentColor = False
              TabOrder = 0
            end
            object GPsInSystemBox: TCheckBox
              Left = 12
              Top = 52
              Width = 240
              Height = 16
              Caption = 'Number of Major Powers in the System'
              Color = clSilver
              ParentColor = False
              TabOrder = 1
            end
            object SysConBox: TCheckBox
              Left = 12
              Top = 70
              Width = 240
              Height = 16
              Caption = 'System Capability Concentration'
              Color = clSilver
              ParentColor = False
              TabOrder = 2
            end
            object SysMoveBox: TCheckBox
              Left = 12
              Top = 88
              Width = 240
              Height = 16
              Caption = 'System Movement (1 year)'
              Color = clSilver
              ParentColor = False
              TabOrder = 3
            end
            object SysMove5Box: TCheckBox
              Left = 12
              Top = 106
              Width = 240
              Height = 16
              Caption = 'System Movement (5 year)'
              Color = clSilver
              ParentColor = False
              TabOrder = 4
            end
            object SysMoveGPBox: TCheckBox
              Left = 12
              Top = 124
              Width = 240
              Height = 16
              Caption = 'System Movement (major powers only) (1 yr)'
              Color = clSilver
              ParentColor = False
              TabOrder = 5
            end
            object SysMoveGP5Box: TCheckBox
              Left = 12
              Top = 142
              Width = 240
              Height = 16
              Caption = 'System Movement (major powers only) (5 yr)'
              Color = clSilver
              ParentColor = False
              TabOrder = 6
            end
          end
          object Panel4: TPanel
            Left = 292
            Top = 198
            Width = 269
            Height = 156
            BevelInner = bvLowered
            Color = clSilver
            TabOrder = 3
            object Label3: TLabel
              Left = 12
              Top = 12
              Width = 53
              Height = 13
              Caption = 'ISO Data'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object ISO_code_CheckBox: TCheckBox
              Left = 12
              Top = 34
              Width = 240
              Height = 16
              Caption = 'ISO Code'
              Color = clSilver
              ParentColor = False
              TabOrder = 0
            end
            object ISO_abb2_CheckBox: TCheckBox
              Left = 12
              Top = 52
              Width = 240
              Height = 16
              Caption = 'ISO 2 Letter Abbrev.'
              Color = clSilver
              ParentColor = False
              TabOrder = 1
            end
            object ISO_abb3_CheckBox: TCheckBox
              Left = 12
              Top = 70
              Width = 240
              Height = 16
              Caption = 'ISO 3 Letter Abbrev.'
              Color = clSilver
              ParentColor = False
              TabOrder = 2
            end
            object ISO_short_CheckBox: TCheckBox
              Left = 12
              Top = 88
              Width = 240
              Height = 16
              Caption = 'ISO Short Name'
              Color = clSilver
              ParentColor = False
              TabOrder = 3
            end
            object ISO_full_CheckBox: TCheckBox
              Left = 12
              Top = 106
              Width = 240
              Height = 16
              Caption = 'ISO Full Name'
              Color = clSilver
              ParentColor = False
              TabOrder = 4
            end
          end
        end
        object Polity3Sheet: TTabSheet
          Caption = 'Polity III'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object PolityVarsPanel: TPanel
            Left = 12
            Top = 12
            Width = 269
            Height = 228
            BevelInner = bvLowered
            Color = clSilver
            TabOrder = 0
            object PolityVarsHeader: TLabel
              Left = 12
              Top = 12
              Width = 159
              Height = 13
              Caption = 'Standard Polity III Variables'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object democcheckbox: TCheckBox
              Left = 12
              Top = 34
              Width = 240
              Height = 16
              Caption = 'Democracy Score (democ)'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 0
            end
            object autoccheckbox: TCheckBox
              Left = 12
              Top = 52
              Width = 240
              Height = 16
              Caption = 'Autocracy Score (autoc)'
              TabOrder = 1
            end
            object xrregCheckBox: TCheckBox
              Left = 12
              Top = 70
              Width = 240
              Height = 16
              Caption = 'Executive Recruitment Regulation (xrreg)'
              TabOrder = 2
            end
            object xrcompCheckBox: TCheckBox
              Left = 12
              Top = 88
              Width = 240
              Height = 16
              Caption = 'Executive Recruitment Competition (xrcomp)'
              TabOrder = 3
            end
            object xropenCheckBox: TCheckBox
              Left = 12
              Top = 106
              Width = 240
              Height = 16
              Caption = 'Executive Recruitment Openness (xropen)'
              TabOrder = 4
            end
            object monoCheckBox: TCheckBox
              Left = 12
              Top = 124
              Width = 240
              Height = 16
              Caption = 'Monocratism (mono)'
              TabOrder = 5
            end
            object xconstCheckBox: TCheckBox
              Left = 12
              Top = 142
              Width = 240
              Height = 16
              Caption = 'Executive Constraints (xconst)'
              TabOrder = 6
            end
            object parregCheckBox: TCheckBox
              Left = 12
              Top = 160
              Width = 240
              Height = 16
              Caption = 'Regulation of Participation (parreg)'
              TabOrder = 7
            end
            object parcmpCheckBox: TCheckBox
              Left = 12
              Top = 178
              Width = 240
              Height = 16
              Caption = 'Competitiveness of Participation (parcomp)'
              TabOrder = 8
            end
            object centCheckBox: TCheckBox
              Left = 12
              Top = 196
              Width = 240
              Height = 16
              Caption = 'Centralization (cent)'
              TabOrder = 9
            end
          end
          object PolityAuxPanel: TPanel
            Left = 12
            Top = 252
            Width = 269
            Height = 102
            BevelInner = bvLowered
            Color = clSilver
            TabOrder = 1
            object PolityAuxLabel: TLabel
              Left = 12
              Top = 12
              Width = 167
              Height = 13
              Caption = 'Generated Polity III Variables'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object RussettDem: TCheckBox
              Left = 12
              Top = 34
              Width = 240
              Height = 16
              Caption = 'Generate "Dem" (Democ-Autoc) measure '
              TabOrder = 0
            end
            object laggeddemaut: TCheckBox
              Left = 12
              Top = 52
              Width = 240
              Height = 16
              Caption = 'Lagged measures (Democ, Autoc, Dem)'
              TabOrder = 1
            end
            object demchangebox: TCheckBox
              Left = 12
              Top = 70
              Width = 240
              Height = 16
              Caption = 'Democratization (Dem - lag(Dem))'
              TabOrder = 2
            end
          end
        end
        object AlliancesSheet: TTabSheet
          Caption = 'Alliances'
          ImageIndex = 4
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object AlliancesPanel: TPanel
            Left = 12
            Top = 12
            Width = 269
            Height = 192
            BevelInner = bvLowered
            Color = clSilver
            TabOrder = 0
            object AlliancesLabel: TLabel
              Left = 12
              Top = 12
              Width = 177
              Height = 13
              Caption = 'Alliances and Alliance Patterns'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object AllianceCheckBox: TCheckBox
              Left = 12
              Top = 34
              Width = 220
              Height = 16
              Caption = 'Alliance Type'
              TabOrder = 0
            end
            object AlliancePortfolioTableUnweightedCheckBox: TCheckBox
              Left = 12
              Top = 52
              Width = 220
              Height = 16
              Caption = 'Alliance Portfolio - unweighted'
              TabOrder = 1
            end
            object AlliancePortfolioTableWeightedCheckBox: TCheckBox
              Left = 12
              Top = 70
              Width = 220
              Height = 16
              Caption = 'Alliance Portfolio - weighted'
              TabOrder = 2
            end
            object Taubox: TCheckBox
              Left = 12
              Top = 88
              Width = 220
              Height = 16
              Caption = 'Tau-b Score'
              TabOrder = 3
            end
            object Sboxunweighted: TCheckBox
              Left = 12
              Top = 106
              Width = 220
              Height = 16
              Caption = 'S Score - unweighted'
              TabOrder = 4
            end
            object sboxweighted: TCheckBox
              Left = 12
              Top = 124
              Width = 220
              Height = 16
              Caption = 'S Score - weighted'
              TabOrder = 5
            end
            object TauWithLeaderBox: TCheckBox
              Left = 12
              Top = 142
              Width = 220
              Height = 16
              Caption = 'Tau-b with System Leader'
              TabOrder = 6
            end
            object swithleaderbox: TCheckBox
              Left = 12
              Top = 160
              Width = 220
              Height = 16
              Caption = 'S with System Leader'
              TabOrder = 7
            end
            object TauWithLeaderButton: TPanel
              Left = 197
              Top = 141
              Width = 60
              Height = 18
              BevelInner = bvRaised
              BevelOuter = bvNone
              BevelWidth = 2
              Caption = 'Options'
              TabOrder = 8
              OnClick = system_variable_optionboxClick
              OnMouseDown = TauWithLeaderMouseDown
              OnMouseUp = TauWithLeaderMouseUp
            end
            object SWithLeaderButton: TPanel
              Left = 197
              Top = 160
              Width = 60
              Height = 18
              BevelInner = bvRaised
              BevelOuter = bvNone
              BevelWidth = 2
              Caption = 'Options'
              TabOrder = 9
              OnClick = system_variable_optionboxClick
              OnMouseDown = SWithLeaderMouseDown
              OnMouseUp = SWithLeaderMouseUp
            end
          end
        end
        object BdMSheet: TTabSheet
          Caption = 'Expected Utility'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object WarTrapPanel: TPanel
            Left = 12
            Top = 12
            Width = 269
            Height = 99
            Hint = 
              'Values for these variables are recomputed using the method from ' +
              'The War Trap, but with risk scores included.'
            BevelInner = bvLowered
            Color = clSilver
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            object WarTrapLabel: TLabel
              Left = 12
              Top = 12
              Width = 250
              Height = 17
              AutoSize = False
              Caption = 'The War Trap'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object WarTrapLabel2: TLabel
              Left = 12
              Top = 28
              Width = 250
              Height = 17
              AutoSize = False
              Caption = 'Bueno de Mesquita (1981)'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object EUWartrapTaubox: TCheckBox
              Left = 12
              Top = 49
              Width = 240
              Height = 16
              Caption = 'Utility Scores - Tau'
              TabOrder = 0
            end
            object EUWarTrapSBox: TCheckBox
              Left = 12
              Top = 67
              Width = 240
              Height = 16
              Caption = 'Utility Scores - S (unweighted)'
              TabOrder = 1
            end
          end
          object WarAndReasonPanel: TPanel
            Left = 293
            Top = 12
            Width = 269
            Height = 306
            Hint = 
              'Values for War and Reason variables are recomputed from scratch ' +
              'using the latest data.'
            BevelInner = bvLowered
            Color = clSilver
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            object WarAndReasonLabel: TLabel
              Left = 12
              Top = 12
              Width = 250
              Height = 17
              AutoSize = False
              Caption = 'War and Reason'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label2: TLabel
              Left = 12
              Top = 28
              Width = 250
              Height = 17
              AutoSize = False
              Caption = 'Bueno de Mesquita and Lalman (1992)'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object riskEUGENETaubox: TCheckBox
              Left = 12
              Top = 49
              Width = 240
              Height = 16
              Caption = 'Risk Attitude - Tau'
              TabOrder = 0
              OnClick = riskEUGENETauboxClick
            end
            object RiskEUGENESBox: TCheckBox
              Left = 12
              Top = 67
              Width = 240
              Height = 16
              Caption = 'Risk Attitude - S'
              TabOrder = 1
              OnClick = RiskEUGENESBoxClick
            end
            object RiskDetailTauCheckBox: TCheckBox
              Left = 12
              Top = 85
              Width = 240
              Height = 16
              Caption = 'Risk Details - Tau'
              TabOrder = 2
              OnClick = RiskDetailTauCheckBoxClick
            end
            object RiskDetailSCheckBox: TCheckBox
              Left = 12
              Top = 103
              Width = 240
              Height = 16
              Caption = 'Risk Details - S'
              TabOrder = 3
              OnClick = RiskDetailSCheckBoxClick
            end
            object UncertTauCheckBox: TCheckBox
              Left = 12
              Top = 121
              Width = 240
              Height = 16
              Caption = 'Uncertainty - Tau'
              TabOrder = 4
              OnClick = UncertTauCheckBoxClick
            end
            object UncertSCheckBox: TCheckBox
              Left = 12
              Top = 139
              Width = 240
              Height = 16
              Caption = 'Uncertainty - S'
              TabOrder = 5
              OnClick = UncertSCheckBoxClick
            end
            object EUReasonTaubox: TCheckBox
              Left = 12
              Top = 175
              Width = 240
              Height = 16
              Caption = 'Utility Scores - Tau'
              TabOrder = 6
            end
            object EUReasonSUnBox: TCheckBox
              Left = 12
              Top = 193
              Width = 240
              Height = 16
              Caption = 'Utility Scores - S (unweighted)'
              TabOrder = 7
            end
            object EUReasonSWtBox: TCheckBox
              Left = 12
              Top = 211
              Width = 240
              Height = 16
              Caption = 'Utility Scores - S (weighted)'
              TabOrder = 8
            end
            object EqTauBox: TCheckBox
              Left = 12
              Top = 238
              Width = 240
              Height = 16
              Caption = 'Equilibria - Tau'
              TabOrder = 9
            end
            object EQSUnBox: TCheckBox
              Left = 12
              Top = 256
              Width = 240
              Height = 16
              Caption = 'Equilibria - S (unweighted)'
              TabOrder = 10
            end
            object EQSWtBox: TCheckBox
              Left = 12
              Top = 274
              Width = 240
              Height = 16
              Caption = 'Equilibria - S (weighted)'
              TabOrder = 11
            end
            object EquilibriumButton: TPanel
              Left = 197
              Top = 238
              Width = 60
              Height = 54
              BevelInner = bvRaised
              BevelOuter = bvNone
              BevelWidth = 2
              Caption = 'Options'
              TabOrder = 12
              OnClick = eq_options_buttonClick
              OnMouseDown = EquilibriumMouseDown
              OnMouseUp = EquilibriumMouseUp
            end
            object Panel1: TPanel
              Left = 24
              Top = 164
              Width = 221
              Height = 3
              BevelInner = bvLowered
              BevelOuter = bvNone
              Color = clSilver
              TabOrder = 13
            end
          end
          object WTRPanel: TPanel
            Left = 12
            Top = 123
            Width = 269
            Height = 81
            Hint = 
              'These risk attitude values are taken directly from The War Trap ' +
              'Revisited, and are only available for a limited set of years.'
            BevelInner = bvLowered
            Color = clSilver
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            object WTRLabel: TLabel
              Left = 12
              Top = 12
              Width = 250
              Height = 17
              AutoSize = False
              Caption = 'The War Trap Revisited'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object WTRLabel2: TLabel
              Left = 12
              Top = 28
              Width = 250
              Height = 17
              AutoSize = False
              Caption = 'Bueno de Mesquita (1985)'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object RiskWTRCheckBox: TCheckBox
              Left = 12
              Top = 49
              Width = 240
              Height = 16
              Caption = 'Risk Attitude'
              TabOrder = 0
              OnClick = RiskWTRCheckBoxClick
            end
          end
        end
        object ConflictTabSheet: TTabSheet
          Caption = 'Conflict Data'
          ImageIndex = 3
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object MIDSubPage: TPageControl
            Left = 12
            Top = 10
            Width = 556
            Height = 420
            ActivePage = DisputeInitiatorSheet
            TabOrder = 0
            object COWMIDTabSheet: TTabSheet
              Caption = 'COW Dyadic MIDs'
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object COWMIDSettingsPanel: TPanel
                Left = 12
                Top = 9
                Width = 256
                Height = 66
                BevelInner = bvLowered
                Color = clSilver
                TabOrder = 0
                object COWMIDBoxLabel: TLabel
                  Left = 12
                  Top = 12
                  Width = 240
                  Height = 17
                  AutoSize = False
                  Caption = 'Militarized Interstate Disputes'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object COWMIDDisputeDataCheckBox: TCheckBox
                  Left = 12
                  Top = 34
                  Width = 240
                  Height = 16
                  Caption = 'COW MID Data (1816+)'
                  TabOrder = 0
                  OnClick = COWMIDDisputeDataCheckBoxClick
                end
              end
              object COWMIDDyadicPanel: TPanel
                Left = 12
                Top = 87
                Width = 256
                Height = 298
                BevelInner = bvLowered
                Color = clSilver
                TabOrder = 1
                object COWMIDDyadicLabel: TLabel
                  Left = 12
                  Top = 4
                  Width = 240
                  Height = 17
                  AutoSize = False
                  Caption = 'Dispute-Level Variables'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object COWMIDInitiationCheckBox: TCheckBox
                  Left = 12
                  Top = 26
                  Width = 236
                  Height = 16
                  Caption = 'Initiation'
                  ParentShowHint = False
                  ShowHint = False
                  TabOrder = 0
                  OnClick = COWMIDInitiationCheckBoxClick
                end
                object COWMIDOngoingCheckBox: TCheckBox
                  Left = 12
                  Top = 44
                  Width = 240
                  Height = 16
                  Caption = 'Ongoing'
                  ParentShowHint = False
                  ShowHint = False
                  TabOrder = 1
                  OnClick = COWMIDOngoingCheckBoxClick
                end
                object COWMIDNumCheckBox: TCheckBox
                  Left = 12
                  Top = 62
                  Width = 197
                  Height = 16
                  Caption = 'MID Number'
                  TabOrder = 2
                  OnClick = COWMIDNumCheckBoxClick
                end
                object COWOutcomeCheckBox: TCheckBox
                  Left = 12
                  Top = 190
                  Width = 240
                  Height = 16
                  Caption = 'Outcome (dispute)'
                  TabOrder = 3
                end
                object COWSettlementCheckBox: TCheckBox
                  Left = 12
                  Top = 208
                  Width = 240
                  Height = 16
                  Caption = 'Settlement (dispute)'
                  TabOrder = 4
                end
                object COWFatalityLevelDisputeCheckBox: TCheckBox
                  Left = 12
                  Top = 135
                  Width = 240
                  Height = 16
                  Caption = 'Fatality Level (dispute)'
                  TabOrder = 5
                end
                object COWHiActDisputeCheckBox: TCheckBox
                  Left = 12
                  Top = 153
                  Width = 240
                  Height = 16
                  Caption = 'Highest Action (dispute)'
                  TabOrder = 6
                end
                object COWHostLevDisputeCheckBox: TCheckBox
                  Left = 12
                  Top = 171
                  Width = 240
                  Height = 16
                  Caption = 'Hostility Level (dispute)'
                  TabOrder = 7
                end
                object COWRecipCheckBox: TCheckBox
                  Left = 12
                  Top = 98
                  Width = 240
                  Height = 16
                  Caption = 'Reciprocated Dispute'
                  TabOrder = 8
                end
                object COWNumOfStatesBox: TCheckBox
                  Left = 12
                  Top = 117
                  Width = 240
                  Height = 16
                  Hint = 
                    'Note: number of states taken directly from MIDA, not adjusted fo' +
                    'r dyadic interaction'
                  Caption = 'Number of States on each side (dispute)'
                  ParentShowHint = False
                  ShowHint = False
                  TabOrder = 9
                end
                object COWMidNameCheckBox: TCheckBox
                  Left = 12
                  Top = 80
                  Width = 157
                  Height = 16
                  Caption = 'MID Name'
                  TabOrder = 10
                end
                object COWNumMIDCheckBox: TCheckBox
                  Left = 12
                  Top = 226
                  Width = 240
                  Height = 16
                  Caption = 'Number of Dyadic MIDs in Year'
                  TabOrder = 11
                end
                object COWPeaceYrsBox: TCheckBox
                  Left = 12
                  Top = 244
                  Width = 233
                  Height = 16
                  Caption = 'Peace Years'
                  TabOrder = 12
                end
                object PeaceYearsOptionsButton1: TPanel
                  Left = 184
                  Top = 243
                  Width = 60
                  Height = 30
                  BevelInner = bvRaised
                  BevelOuter = bvNone
                  BevelWidth = 2
                  Caption = 'Options'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clBlack
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 13
                  OnClick = PeaceYearsOptionsButton1Click
                  OnMouseDown = PeaceYearsOptionsButtonMouseDown
                  OnMouseUp = PeaceYearsOptionsButtonMouseUp
                end
                object COWLinkStatusCheckBox: TCheckBox
                  Left = 12
                  Top = 279
                  Width = 233
                  Height = 16
                  Caption = 'Links and 2002 Status'
                  TabOrder = 14
                end
                object COWPeaceDaysCheckBox: TCheckBox
                  Left = 12
                  Top = 260
                  Width = 157
                  Height = 17
                  Caption = 'Peace Days'
                  TabOrder = 15
                end
              end
              object COWMIDMonadicPanel: TPanel
                Left = 280
                Top = 9
                Width = 256
                Height = 245
                BevelInner = bvLowered
                Color = clSilver
                TabOrder = 2
                object COWMIDMonadicLabel: TLabel
                  Left = 12
                  Top = 12
                  Width = 149
                  Height = 17
                  AutoSize = False
                  Caption = 'Country-Level Variables'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object COWstartdatecheck: TCheckBox
                  Left = 12
                  Top = 34
                  Width = 165
                  Height = 16
                  Caption = 'Start Date'
                  TabOrder = 0
                end
                object COWenddatecheckbox: TCheckBox
                  Left = 12
                  Top = 52
                  Width = 165
                  Height = 16
                  Caption = 'End Date'
                  TabOrder = 1
                end
                object COWSideACheckBox: TCheckBox
                  Left = 12
                  Top = 88
                  Width = 165
                  Height = 16
                  Caption = 'Side A (boolean)'
                  TabOrder = 2
                end
                object COWRevStateCheckBox: TCheckBox
                  Left = 12
                  Top = 106
                  Width = 165
                  Height = 16
                  Caption = 'Revisionist State (boolean)'
                  TabOrder = 3
                end
                object COWRevTypeCheckBox: TCheckBox
                  Left = 12
                  Top = 124
                  Width = 165
                  Height = 16
                  Caption = 'Revision Type'
                  TabOrder = 4
                end
                object COWFatalityLevelStateCheckBox: TCheckBox
                  Left = 12
                  Top = 142
                  Width = 165
                  Height = 16
                  Caption = 'Fatality Level (state)'
                  TabOrder = 5
                end
                object COWHiActCheckBox: TCheckBox
                  Left = 12
                  Top = 160
                  Width = 165
                  Height = 16
                  Caption = 'Highest Action (state)'
                  TabOrder = 6
                end
                object COWMIDHostLevStateCheckBox: TCheckBox
                  Left = 12
                  Top = 178
                  Width = 165
                  Height = 16
                  Caption = 'Hostility Level (state)'
                  TabOrder = 7
                  OnClick = COWMIDHostLevStateCheckBoxClick
                end
                object COWOriginatorCheckBox: TCheckBox
                  Left = 12
                  Top = 70
                  Width = 165
                  Height = 16
                  Caption = 'Originator'
                  TabOrder = 8
                end
                object COWMarkJoinersCheckBox: TCheckBox
                  Left = 12
                  Top = 196
                  Width = 165
                  Height = 16
                  Caption = 'Mark Joiners '
                  TabOrder = 9
                end
                object COWRoleCheckBox: TCheckBox
                  Left = 12
                  Top = 214
                  Width = 165
                  Height = 16
                  Caption = 'Role'
                  TabOrder = 10
                end
              end
            end
            object MaozMidTabSheet: TTabSheet
              Caption = 'Maoz Dyadic MIDs'
              ImageIndex = 1
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object maozmidsettingspanel: TPanel
                Left = 12
                Top = 9
                Width = 256
                Height = 66
                BevelInner = bvLowered
                Color = clSilver
                TabOrder = 0
                object MaozDispBoxLabel: TLabel
                  Left = 12
                  Top = 12
                  Width = 240
                  Height = 17
                  AutoSize = False
                  Caption = 'Maoz Militarized Interstate Disputes'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object MaozDisputeDataCheckBox: TCheckBox
                  Left = 12
                  Top = 34
                  Width = 240
                  Height = 16
                  Caption = 'Maoz Dyadic MID Data v1.1 (1816-1992)'
                  TabOrder = 0
                  OnClick = MaozDisputeDataCheckBoxClick
                end
              end
              object MaozMIDDyadicPanel: TPanel
                Left = 12
                Top = 87
                Width = 256
                Height = 298
                BevelInner = bvLowered
                Color = clSilver
                TabOrder = 1
                object MaozMIDDyadicLabel: TLabel
                  Left = 12
                  Top = 4
                  Width = 240
                  Height = 17
                  AutoSize = False
                  Caption = 'Maoz Dispute-Level Variables'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object MaozMIDInitiationCheckBox: TCheckBox
                  Left = 12
                  Top = 16
                  Width = 236
                  Height = 16
                  Caption = 'Initiation'
                  ParentShowHint = False
                  ShowHint = False
                  TabOrder = 0
                  OnClick = MaozMIDInitiationCheckBoxClick
                end
                object MaozMIDOngoingCheckBox: TCheckBox
                  Left = 12
                  Top = 34
                  Width = 240
                  Height = 16
                  Caption = 'Ongoing'
                  ParentShowHint = False
                  ShowHint = False
                  TabOrder = 1
                  OnClick = MaozMIDOngoingCheckBoxClick
                end
                object MaozMIDNumCheckBox: TCheckBox
                  Left = 12
                  Top = 52
                  Width = 240
                  Height = 16
                  Caption = 'MID Number'
                  TabOrder = 2
                  OnClick = MaozMIDNumCheckBoxClick
                end
                object MaozOutcomeCheckBox: TCheckBox
                  Left = 12
                  Top = 181
                  Width = 240
                  Height = 16
                  Caption = 'Outcome'
                  TabOrder = 3
                end
                object MaozSettlementCheckBox: TCheckBox
                  Left = 12
                  Top = 199
                  Width = 240
                  Height = 16
                  Caption = 'Settlement'
                  TabOrder = 4
                end
                object MaozFatalityLevelDisputeCheckBox: TCheckBox
                  Left = 12
                  Top = 126
                  Width = 240
                  Height = 16
                  Caption = 'Fatality Level (dispute)'
                  TabOrder = 5
                end
                object MaozHiActDisputeCheckBox: TCheckBox
                  Left = 12
                  Top = 144
                  Width = 240
                  Height = 16
                  Caption = 'Highest Action (dispute)'
                  TabOrder = 6
                end
                object MaozHostLevDisputeCheckBox: TCheckBox
                  Left = 12
                  Top = 163
                  Width = 240
                  Height = 16
                  Caption = 'Hostility Level (dispute)'
                  TabOrder = 7
                end
                object MaozRecipCheckBox: TCheckBox
                  Left = 12
                  Top = 89
                  Width = 240
                  Height = 16
                  Caption = 'Reciprocated Dispute'
                  TabOrder = 8
                end
                object MaozNumOfStatesBox: TCheckBox
                  Left = 12
                  Top = 107
                  Width = 240
                  Height = 16
                  Caption = 'Number of States on each side'
                  TabOrder = 9
                end
                object MaozNumMIDCheckBox: TCheckBox
                  Left = 12
                  Top = 218
                  Width = 240
                  Height = 16
                  Caption = 'Number of Dyadic MIDs in Year'
                  TabOrder = 10
                end
                object MaozPeaceYrsBox: TCheckBox
                  Left = 12
                  Top = 236
                  Width = 240
                  Height = 16
                  Caption = 'Peace Years'
                  TabOrder = 11
                end
                object PeaceYearsOptionsButton2: TPanel
                  Left = 184
                  Top = 251
                  Width = 60
                  Height = 18
                  BevelInner = bvRaised
                  BevelOuter = bvNone
                  BevelWidth = 2
                  Caption = 'Options'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clBlack
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 12
                  OnClick = PeaceYearsOptionsButton2Click
                  OnMouseDown = PeaceYearsOptionsButtonMouseDown
                  OnMouseUp = PeaceYearsOptionsButtonMouseUp
                end
                object MaozMidNameCheckBox: TCheckBox
                  Left = 12
                  Top = 71
                  Width = 157
                  Height = 16
                  Caption = 'MID Name'
                  TabOrder = 13
                end
                object MaozLinkStatusCheckBox: TCheckBox
                  Left = 12
                  Top = 271
                  Width = 233
                  Height = 16
                  Caption = 'Links and 2002 Status'
                  TabOrder = 14
                end
                object MaozPeaceDysBox: TCheckBox
                  Left = 12
                  Top = 252
                  Width = 125
                  Height = 16
                  Caption = 'Peace Days'
                  TabOrder = 15
                end
              end
              object MaozMIDMonadicPanel: TPanel
                Left = 280
                Top = 9
                Width = 256
                Height = 237
                BevelInner = bvLowered
                Color = clSilver
                TabOrder = 2
                object MaozMIDMonadicLabel: TLabel
                  Left = 12
                  Top = 12
                  Width = 189
                  Height = 17
                  AutoSize = False
                  Caption = 'Maoz Country-Level Variables'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object Maozstartdatecheck: TCheckBox
                  Left = 12
                  Top = 34
                  Width = 165
                  Height = 16
                  Caption = 'Start Date'
                  TabOrder = 0
                end
                object Maozenddatecheckbox: TCheckBox
                  Left = 12
                  Top = 52
                  Width = 165
                  Height = 16
                  Caption = 'End Date'
                  TabOrder = 1
                end
                object MaozSideACheckBox: TCheckBox
                  Left = 12
                  Top = 88
                  Width = 165
                  Height = 16
                  Caption = 'Side A (boolean)'
                  TabOrder = 2
                end
                object MaozRevStateCheckBox: TCheckBox
                  Left = 12
                  Top = 106
                  Width = 165
                  Height = 16
                  Caption = 'Revisionist State (boolean)'
                  TabOrder = 3
                end
                object MaozRevTypeCheckBox: TCheckBox
                  Left = 12
                  Top = 124
                  Width = 165
                  Height = 16
                  Caption = 'Revision Type'
                  TabOrder = 4
                end
                object MaozFatalityLevelStateCheckBox: TCheckBox
                  Left = 12
                  Top = 142
                  Width = 165
                  Height = 16
                  Caption = 'Fatality Level (state)'
                  TabOrder = 5
                end
                object MaozHiActCheckBox: TCheckBox
                  Left = 12
                  Top = 160
                  Width = 165
                  Height = 16
                  Caption = 'Highest Action (state)'
                  TabOrder = 6
                end
                object MaozMIDHostLevStateCheckBox: TCheckBox
                  Left = 12
                  Top = 178
                  Width = 165
                  Height = 16
                  Caption = 'Hostility Level (state)'
                  TabOrder = 7
                  OnClick = MaozMIDHostLevStateCheckBoxClick
                end
                object MaozOriginatorCheckBox: TCheckBox
                  Left = 12
                  Top = 70
                  Width = 165
                  Height = 16
                  Caption = 'Originator'
                  TabOrder = 8
                end
                object MaozMarkJoinersCheckBox: TCheckBox
                  Left = 12
                  Top = 196
                  Width = 165
                  Height = 16
                  Caption = 'Mark Joiners '
                  TabOrder = 9
                end
                object MaozRoleCheckBox: TCheckBox
                  Left = 12
                  Top = 214
                  Width = 165
                  Height = 16
                  Caption = 'Role'
                  TabOrder = 10
                end
              end
              object MaozMIDOnlyPanel: TPanel
                Left = 280
                Top = 257
                Width = 256
                Height = 112
                BevelInner = bvLowered
                Color = clSilver
                TabOrder = 3
                object MIDMaozLabel: TLabel
                  Left = 12
                  Top = 12
                  Width = 240
                  Height = 17
                  AutoSize = False
                  Caption = 'Maoz Dyadic Variables'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object maozcowwarbox: TCheckBox
                  Left = 12
                  Top = 34
                  Width = 240
                  Height = 16
                  Caption = 'Maoz COWWar marker'
                  TabOrder = 0
                end
                object maozdurindxbox: TCheckBox
                  Left = 12
                  Top = 52
                  Width = 240
                  Height = 16
                  Caption = 'Maoz DurIndx (dispute year counter)'
                  TabOrder = 1
                end
                object maozdurdaysbox: TCheckBox
                  Left = 12
                  Top = 70
                  Width = 240
                  Height = 16
                  Caption = 'Maoz DurDays (days in dispute, this year)'
                  TabOrder = 2
                end
                object maozreciprocateddyadicbox: TCheckBox
                  Left = 12
                  Top = 88
                  Width = 240
                  Height = 16
                  Caption = 'Maoz dyadic reciprocation marker'
                  TabOrder = 3
                  Visible = False
                end
              end
            end
            object ICBTabSheet: TTabSheet
              Caption = 'ICB Crises'
              ImageIndex = 2
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object ICBSettingsPanel: TPanel
                Left = 12
                Top = 12
                Width = 253
                Height = 81
                BevelInner = bvLowered
                Color = clSilver
                TabOrder = 0
                object ICBBoxLabel: TLabel
                  Left = 12
                  Top = 12
                  Width = 213
                  Height = 17
                  AutoSize = False
                  Caption = 'ICB Crises'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object ICBChoiceLabel: TLabel
                  Left = 12
                  Top = 28
                  Width = 213
                  Height = 17
                  AutoSize = False
                  Caption = 'Choose one of the following options:'
                end
                object ICBDyadicDataCheckBox: TCheckBox
                  Left = 12
                  Top = 49
                  Width = 213
                  Height = 16
                  Caption = 'ICB Dyadic Crisis Data'
                  TabOrder = 0
                  OnClick = ICBDyadicDataCheckBoxClick
                end
              end
              object ICBDyadicPanel: TPanel
                Left = 12
                Top = 97
                Width = 253
                Height = 280
                BevelInner = bvLowered
                Color = clSilver
                TabOrder = 1
                object Label4: TLabel
                  Left = 12
                  Top = 12
                  Width = 201
                  Height = 17
                  AutoSize = False
                  Caption = 'Crisis-Level Variables'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object ICBCrisisCheckBox: TCheckBox
                  Left = 12
                  Top = 34
                  Width = 201
                  Height = 16
                  Caption = 'New Crisis'
                  ParentShowHint = False
                  ShowHint = False
                  TabOrder = 0
                  OnClick = ICBCrisisCheckBoxClick
                end
                object ICBOngoingCheckBox: TCheckBox
                  Left = 12
                  Top = 52
                  Width = 201
                  Height = 16
                  Caption = 'Ongoing'
                  ParentShowHint = False
                  ShowHint = False
                  TabOrder = 1
                  OnClick = ICBOngoingCheckBoxClick
                end
                object ICBCrisisNameCheckBox: TCheckBox
                  Left = 12
                  Top = 88
                  Width = 201
                  Height = 16
                  Caption = 'Crisis Name'
                  TabOrder = 2
                end
                object ICBDurDaysCheckBox: TCheckBox
                  Left = 12
                  Top = 179
                  Width = 201
                  Height = 16
                  Caption = 'Duration (Days)'
                  TabOrder = 3
                end
                object ICBDurYearsCheckBox: TCheckBox
                  Left = 12
                  Top = 198
                  Width = 201
                  Height = 16
                  Caption = 'Duration (Years) [Years Spanned]'
                  TabOrder = 4
                end
                object ICBDyadicStartCheckBox: TCheckBox
                  Left = 12
                  Top = 143
                  Width = 201
                  Height = 16
                  Caption = 'Dyadic Crisis Trigger Date'
                  TabOrder = 5
                end
                object ICBDyadicEndCheckBox: TCheckBox
                  Left = 12
                  Top = 161
                  Width = 201
                  Height = 16
                  Caption = 'Dyadic Crisis Termination Date'
                  TabOrder = 6
                end
                object ICBOneSidedCheckBox: TCheckBox
                  Left = 12
                  Top = 125
                  Width = 201
                  Height = 16
                  Caption = 'One-Sided Crisis'
                  TabOrder = 7
                end
                object ICBCrisisDyadNumberCheckBox: TCheckBox
                  Left = 12
                  Top = 106
                  Width = 201
                  Height = 16
                  Hint = 'Dyadic ICB variable "CRDYNUM"'
                  Caption = 'ICB Crisis Dyad Number'
                  TabOrder = 8
                end
                object ICBCrisisNumberCheckBox: TCheckBox
                  Left = 12
                  Top = 70
                  Width = 201
                  Height = 16
                  Caption = 'ICB Crisis Number'
                  TabOrder = 9
                  OnClick = ICBCrisisNumberCheckBoxClick
                end
                object ICBNumCrisisCheckBox: TCheckBox
                  Left = 12
                  Top = 216
                  Width = 201
                  Height = 16
                  Caption = 'Number of Crises in Year'
                  TabOrder = 10
                end
                object ICBPeaceYrsBox: TCheckBox
                  Left = 12
                  Top = 234
                  Width = 194
                  Height = 16
                  Caption = 'Peace Years'
                  TabOrder = 11
                end
                object PeaceYearsOptionsButton3: TPanel
                  Left = 181
                  Top = 234
                  Width = 60
                  Height = 31
                  BevelInner = bvRaised
                  BevelOuter = bvNone
                  BevelWidth = 2
                  Caption = 'Options'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clBlack
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 12
                  OnClick = PeaceYearsOptionsButton3Click
                  OnMouseDown = PeaceYearsOptionsButtonMouseDown
                  OnMouseUp = PeaceYearsOptionsButtonMouseUp
                end
                object ICBpeacedysCheckBox: TCheckBox
                  Left = 12
                  Top = 252
                  Width = 97
                  Height = 17
                  Caption = 'Peace Days'
                  TabOrder = 13
                end
              end
              object ICBMonadicPanel: TPanel
                Left = 279
                Top = 12
                Width = 258
                Height = 174
                BevelInner = bvLowered
                Color = clSilver
                TabOrder = 2
                object Label5: TLabel
                  Left = 12
                  Top = 12
                  Width = 240
                  Height = 17
                  AutoSize = False
                  Caption = 'Country-Level Variables'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object ICBStateStartCheckBox: TCheckBox
                  Left = 12
                  Top = 34
                  Width = 229
                  Height = 16
                  Caption = 'Crisis Trigger Date (State)'
                  TabOrder = 0
                end
                object ICBStateEndCheckBox: TCheckBox
                  Left = 12
                  Top = 52
                  Width = 229
                  Height = 16
                  Caption = 'Crisis Termination Date (State)'
                  TabOrder = 1
                end
                object ICBJoinerCheckBox: TCheckBox
                  Left = 12
                  Top = 142
                  Width = 229
                  Height = 16
                  Caption = 'Mark Joiners'
                  TabOrder = 2
                end
                object ICBActorSequenceCheckBox: TCheckBox
                  Left = 12
                  Top = 70
                  Width = 229
                  Height = 16
                  Caption = 'Actor Sequence Number'
                  TabOrder = 3
                end
                object ICBCOWMemberCheckBox: TCheckBox
                  Left = 12
                  Top = 88
                  Width = 229
                  Height = 16
                  Caption = 'COW Membership Status'
                  TabOrder = 4
                end
                object ICBGWMemberCheckBox: TCheckBox
                  Left = 12
                  Top = 106
                  Width = 229
                  Height = 16
                  Caption = 'Gleditsch/Ward Membership Status'
                  TabOrder = 5
                end
                object ICBIntraWarCheckBox: TCheckBox
                  Left = 12
                  Top = 124
                  Width = 229
                  Height = 16
                  Caption = 'Intra-War Crisis'
                  TabOrder = 6
                end
              end
            end
            object WarTabSheet: TTabSheet
              Caption = 'Wars'
              ImageIndex = 3
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object Label6: TLabel
                Left = 280
                Top = 56
                Width = 227
                Height = 96
                Caption = 
                  'Note:  This tab is currently made tab not visible as first line ' +
                  'of pagedoutput.formshow.'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -20
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentFont = False
                WordWrap = True
              end
              object WarSettingsPanel: TPanel
                Left = 12
                Top = 12
                Width = 269
                Height = 81
                BevelInner = bvLowered
                Color = clSilver
                TabOrder = 0
                object COWBoxLabel: TLabel
                  Left = 12
                  Top = 12
                  Width = 240
                  Height = 17
                  AutoSize = False
                  Caption = 'COW Wars'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object Label7: TLabel
                  Left = 12
                  Top = 28
                  Width = 240
                  Height = 17
                  AutoSize = False
                  Caption = 'Choose one of the following options:'
                end
                object COWDyadicWarsDataCheckBox: TCheckBox
                  Left = 12
                  Top = 49
                  Width = 240
                  Height = 16
                  Caption = 'COW Dyadic Wars Set'
                  TabOrder = 0
                  OnClick = COWDyadicWarsDataCheckBoxClick
                end
              end
              object WarVariablePanel: TPanel
                Left = 12
                Top = 105
                Width = 269
                Height = 228
                BevelInner = bvLowered
                Color = clSilver
                TabOrder = 1
                object WarVarLabel: TLabel
                  Left = 12
                  Top = 12
                  Width = 240
                  Height = 17
                  AutoSize = False
                  Caption = 'War Variables'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object NewWarCheckBox: TCheckBox
                  Left = 12
                  Top = 34
                  Width = 240
                  Height = 16
                  Caption = 'New War'
                  Checked = True
                  ParentShowHint = False
                  ShowHint = False
                  State = cbChecked
                  TabOrder = 0
                  OnClick = NewWarCheckBoxClick
                end
                object WarOngoingCheckBox: TCheckBox
                  Left = 12
                  Top = 52
                  Width = 240
                  Height = 16
                  Caption = 'Ongoing'
                  Checked = True
                  ParentShowHint = False
                  ShowHint = False
                  State = cbChecked
                  TabOrder = 1
                  OnClick = WarOngoingCheckBoxClick
                end
                object COWWarNumberCheckBox: TCheckBox
                  Left = 12
                  Top = 70
                  Width = 240
                  Height = 16
                  Caption = 'COW War Number'
                  TabOrder = 2
                end
              end
            end
            object DisputeInitiatorSheet: TTabSheet
              Caption = 'Initiator/Multiple MID Settings'
              ImageIndex = 4
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object InitiatorTimingPanel: TPanel
                Left = 14
                Top = 12
                Width = 443
                Height = 150
                BevelInner = bvLowered
                Color = clSilver
                TabOrder = 0
                object InitiatorTimingLabel: TLabel
                  Left = 12
                  Top = 12
                  Width = 278
                  Height = 17
                  AutoSize = False
                  Caption = 'Initiator Coding: Timing'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object originatorInitFinishLabel: TLabel
                  Left = 30
                  Top = 50
                  Width = 221
                  Height = 13
                  Caption = '(also, only originators are included as targets)'
                end
                object joinerinitfinishlabel: TLabel
                  Left = 30
                  Top = 84
                  Width = 80
                  Height = 13
                  Caption = 'Label set in code'
                end
                object JoinerAnyFinishlabel: TLabel
                  Left = 30
                  Top = 118
                  Width = 80
                  Height = 13
                  Caption = 'Label set in code'
                end
                object TrueInitiatorsCheckBox: TRadioButton
                  Left = 12
                  Top = 34
                  Width = 349
                  Height = 16
                  Hint = 
                    'Marks initiation only in dyads in which both states are dispute ' +
                    'originators'
                  Caption = 'Code Only Originators (Involved Day 1) as Initiators'
                  TabOrder = 0
                end
                object JoinerInitCheckBox: TRadioButton
                  Left = 12
                  Top = 68
                  Width = 413
                  Height = 16
                  Hint = 
                    'Codes dispute initiations in directed dyads if both are originat' +
                    'ors, if the target state was a joiner on the target side vs. the' +
                    ' initiator in the directed dyad, or if the initiating state in t' +
                    'he directed dyad was a joiner on the initiating side'
                  Caption = 
                    'Include Joiners on Initiating Side as Initiators, and Joiners on' +
                    ' Target side as Targets'
                  TabOrder = 1
                end
                object JoinerAnyCheckBox: TRadioButton
                  Left = 12
                  Top = 102
                  Width = 349
                  Height = 16
                  Hint = 
                    'Be very careful with selection - marking all joiners as initiato' +
                    'rs vastly distorts the meaning of "initiation"!'
                  Caption = 'Code Originators and Joiners (regardless of side) as Initiators '
                  TabOrder = 2
                end
              end
              object InitiatorIdentityPanel: TPanel
                Left = 14
                Top = 174
                Width = 443
                Height = 87
                BevelInner = bvLowered
                Color = clSilver
                TabOrder = 1
                object InitiatorIdentityLabel: TLabel
                  Left = 12
                  Top = 12
                  Width = 278
                  Height = 17
                  AutoSize = False
                  Caption = 'Initiator Coding: Identity (MIDs only)'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object SideAAsInitiatorCheckBox: TRadioButton
                  Left = 12
                  Top = 34
                  Width = 278
                  Height = 16
                  Caption = 'Code "Side A" as Initiator'
                  TabOrder = 0
                end
                object InitiationAsRevisionistRadioButton: TRadioButton
                  Left = 12
                  Top = 52
                  Width = 278
                  Height = 16
                  Caption = 'Code "Revisionist State(s)" as Initiator(s)'
                  TabOrder = 1
                end
              end
              object MultiMidYearPanel: TPanel
                Left = 14
                Top = 273
                Width = 443
                Height = 99
                BevelInner = bvLowered
                Color = clSilver
                TabOrder = 2
                object MultiMidTitle: TLabel
                  Left = 12
                  Top = 12
                  Width = 240
                  Height = 17
                  AutoSize = False
                  Caption = 'Multiple MIDs in Year (MIDs only)'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsBold]
                  ParentFont = False
                end
                object MIDmultilabel2: TLabel
                  Left = 12
                  Top = 28
                  Width = 240
                  Height = 17
                  AutoSize = False
                  Caption = 'If more than one MID occurs in dyad-year:'
                end
                object UseFirstMIDCheckBox: TCheckBox
                  Left = 12
                  Top = 49
                  Width = 240
                  Height = 16
                  Hint = 
                    'Gives priority to timing of dispute regardless of hostility leve' +
                    'l;  First identifies and uses new dispute initiations/onsets, th' +
                    'en identifies the earliest dispute joining, then identifies ongo' +
                    'ing disputes.'
                  Caption = 'Output Information about First MID in Year'
                  TabOrder = 0
                  OnClick = UseFirstMIDCheckBoxClick
                end
                object UseMostSeriousMIDCheckBox: TCheckBox
                  Left = 12
                  Top = 67
                  Width = 349
                  Height = 16
                  Hint = 
                    'EUGene gives priority to dispute hostility levels;  Identifies a' +
                    'nd uses the most serious MID, even if it is an ongoing MID or oc' +
                    'curs as a 2nd MID in a given year'
                  Caption = 
                    'Output Information about Most Serious (highest hostility level) ' +
                    'MID'
                  TabOrder = 1
                  OnClick = UseMostSeriousMIDCheckBoxClick
                end
              end
            end
          end
        end
        object UserDataTabSheet: TTabSheet
          Caption = 'User Data'
          ImageIndex = 7
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object UserDataTabControl: TPageControl
            Left = 12
            Top = 12
            Width = 548
            Height = 389
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
          end
        end
      end
    end
    object ExclusionSheet: TTabSheet
      Caption = 'Case/Conflict Exclusions'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object OngoingMIDPanel: TPanel
        Left = 110
        Top = 92
        Width = 380
        Height = 138
        BevelInner = bvLowered
        Color = clSilver
        TabOrder = 0
        object OngoingMIDLabel: TLabel
          Left = 12
          Top = 12
          Width = 278
          Height = 17
          AutoSize = False
          Caption = 'Dyad Years with Ongoing Conflicts'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object DividerPanel1: TPanel
          Left = 24
          Top = 95
          Width = 318
          Height = 3
          BevelInner = bvLowered
          BevelOuter = bvNone
          Color = clSilver
          TabOrder = 0
        end
        object DropAllOngoingButton: TRadioButton
          Left = 12
          Top = 34
          Width = 325
          Height = 16
          Caption = 'Drop All Dyad-Years with An Ongoing Conflict (MID or Crisis)'
          TabOrder = 1
        end
        object OngoingIfNewDispButton: TRadioButton
          Left = 12
          Top = 52
          Width = 278
          Height = 16
          Caption = 'Include Ongoing Dispute Dyad-Years Iff New Conflict'
          TabOrder = 2
        end
        object AllOngoingDisputeYearButton: TRadioButton
          Left = 12
          Top = 70
          Width = 278
          Height = 16
          Caption = 'Include All Dyad-Years With An Ongoing Conflict'
          TabOrder = 3
        end
        object MarkSubsequentAsInitiationCheckBox: TCheckBox
          Left = 12
          Top = 106
          Width = 278
          Height = 16
          Caption = 'Treat Ongoing Dispute Years as New Initiations'
          TabOrder = 4
        end
      end
      object TargetInitiatorPanel: TPanel
        Left = 110
        Top = 238
        Width = 380
        Height = 102
        BevelInner = bvLowered
        Color = clSilver
        TabOrder = 1
        object TargetInitiatorLabel: TLabel
          Left = 12
          Top = 12
          Width = 278
          Height = 17
          AutoSize = False
          Caption = 'Target vs. Initiator Dyads'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object AlwaysIncludeTgtVsInitDyadsRadioButton: TRadioButton
          Left = 12
          Top = 34
          Width = 300
          Height = 16
          Caption = 'Keep Target vs. Initiator Directed Dyads if No New Conflict'
          TabOrder = 0
        end
        object IncludeTgtVsInitDyadsiffNewRadioButton: TRadioButton
          Left = 12
          Top = 52
          Width = 300
          Height = 16
          Caption = 'Drop Target vs. Initiator Directed Dyads if No New Conflict'
          TabOrder = 1
        end
        object TargetDispYearNoteLabel: TStaticText
          Left = 30
          Top = 70
          Width = 289
          Height = 17
          Caption = '(Target vs. Initiator Dyads Always Included if New Conflict)'
          TabOrder = 2
        end
      end
      object JoinerInclusionPanel: TPanel
        Left = 110
        Top = 348
        Width = 380
        Height = 84
        BevelInner = bvLowered
        Color = clSilver
        TabOrder = 2
        object JoinerInclusionLabel: TLabel
          Left = 12
          Top = 12
          Width = 300
          Height = 17
          AutoSize = False
          Caption = 'Dyads Involving Joiners'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object IncludeJoinerRadioButton: TRadioButton
          Left = 12
          Top = 34
          Width = 300
          Height = 16
          Caption = 'Include All Joiner Dyads'
          TabOrder = 0
        end
        object DropJoinerRadioButton: TRadioButton
          Left = 12
          Top = 52
          Width = 300
          Height = 16
          Caption = 'Drop All Joiner Dyads'
          TabOrder = 1
        end
      end
      object Panel3: TPanel
        Left = 110
        Top = 7
        Width = 380
        Height = 77
        BevelInner = bvLowered
        Color = clSilver
        TabOrder = 3
        object ExcludeDyadYearLabel: TLabel
          Left = 12
          Top = 12
          Width = 357
          Height = 17
          AutoSize = False
          Caption = 'Exclude Conflict Dyad-Years Based on Which Data Set?'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object ExcludeConflictCOWMIDButton: TRadioButton
          Left = 12
          Top = 34
          Width = 109
          Height = 16
          Caption = 'COW Dyadic MIDs'
          TabOrder = 0
          OnClick = ExcludeConflictCOWMIDButtonClick
        end
        object ExcludeConflictMaozMIDButton: TRadioButton
          Left = 12
          Top = 52
          Width = 125
          Height = 16
          Caption = 'Maoz Dyadic MIDs'
          TabOrder = 1
          OnClick = ExcludeConflictMaozMIDButtonClick
        end
        object ExcludeConflictICBButton: TRadioButton
          Left = 132
          Top = 34
          Width = 109
          Height = 16
          Caption = 'Dyadic ICB Crises'
          TabOrder = 2
          OnClick = ExcludeConflictICBButtonClick
        end
        object ExcludeConflictWarsButton: TRadioButton
          Left = 132
          Top = 52
          Width = 109
          Height = 16
          Caption = 'Dyadic COW Wars'
          Enabled = False
          TabOrder = 3
          OnClick = ExcludeConflictWarsButtonClick
        end
        object ExcludeConflictNoneButton: TRadioButton
          Left = 252
          Top = 34
          Width = 109
          Height = 16
          Caption = 'No Exclusions'
          TabOrder = 4
          OnClick = ExcludeConflictNoneButtonClick
        end
      end
    end
  end
  object ButtonGroupBox: TGroupBox
    Left = 200
    Top = 524
    Width = 280
    Height = 48
    TabOrder = 1
    object Okbtn: TBitBtn
      Left = 12
      Top = 14
      Width = 70
      Height = 24
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
      OnClick = OkbtnClick
      Glyph.Data = {
        CE070000424DCE07000000000000360000002800000024000000120000000100
        1800000000009807000000000000000000000000000000000000008284008284
        0082840082840082840082840082840082840082840082840082840082840082
        8400828400828400828400828400828400828400828400828400828400828400
        8284008284008284008284008284008284008284008284008284008284008284
        0082840082840082840082840082840082840082840082840082840082840082
        8400828400828400828400828400828400828400828400828400828400828400
        8284008284008284008284008284FFFFFF008284008284008284008284008284
        0082840082840082840082840082840082840082840082840082840082840082
        8484000084000000828400828400828400828400828400828400828400828400
        8284008284008284008284008284008284008284008284848284848284FFFFFF
        0082840082840082840082840082840082840082840082840082840082840082
        8400828400828400828484000000820000820084000000828400828400828400
        8284008284008284008284008284008284008284008284008284008284008284
        848284008284008284848284FFFFFF0082840082840082840082840082840082
        8400828400828400828400828400828400828484000000820000820000820000
        8200840000008284008284008284008284008284008284008284008284008284
        008284008284008284848284008284008284008284008284848284FFFFFF0082
        8400828400828400828400828400828400828400828400828400828484000000
        8200008200008200008200008200008200840000008284008284008284008284
        0082840082840082840082840082840082848482840082840082840082840082
        84008284008284848284FFFFFF00828400828400828400828400828400828400
        828400828484000000820000820000820000FF00008200008200008200008200
        840000008284008284008284008284008284008284008284008284848284FFFF
        FF008284008284848284FFFFFF008284008284008284848284FFFFFF00828400
        828400828400828400828400828400828400820000820000820000FF00008284
        00FF000082000082000082008400000082840082840082840082840082840082
        84008284008284848284FFFFFF008284848284008284848284FFFFFF00828400
        8284848284FFFFFF00828400828400828400828400828400828400828400FF00
        00820000FF0000828400828400828400FF000082000082000082008400000082
        84008284008284008284008284008284008284848284FFFFFF84828400828400
        8284008284848284FFFFFF008284008284848284FFFFFF008284008284008284
        00828400828400828400828400FF0000828400828400828400828400828400FF
        0000820000820000820084000000828400828400828400828400828400828400
        8284848284008284008284008284008284008284848284FFFFFF008284008284
        848284FFFFFF0082840082840082840082840082840082840082840082840082
        8400828400828400828400828400FF0000820000820000820084000000828400
        8284008284008284008284008284008284008284008284008284008284008284
        008284848284FFFFFF008284008284848284FFFFFF0082840082840082840082
        8400828400828400828400828400828400828400828400828400828400FF0000
        8200008200008200840000008284008284008284008284008284008284008284
        008284008284008284008284008284008284848284FFFFFF0082840082848482
        84FFFFFF00828400828400828400828400828400828400828400828400828400
        828400828400828400828400FF00008200008200008200840000008284008284
        0082840082840082840082840082840082840082840082840082840082840082
        84848284FFFFFF008284008284848284FFFFFF00828400828400828400828400
        828400828400828400828400828400828400828400828400828400FF00008200
        0082000082008400000082840082840082840082840082840082840082840082
        84008284008284008284008284008284848284FFFFFF008284008284848284FF
        FFFF008284008284008284008284008284008284008284008284008284008284
        00828400828400828400FF000082000082008400000082840082840082840082
        8400828400828400828400828400828400828400828400828400828400828484
        8284FFFFFF008284848284FFFFFF008284008284008284008284008284008284
        00828400828400828400828400828400828400828400828400FF000082000082
        0000828400828400828400828400828400828400828400828400828400828400
        8284008284008284008284008284848284FFFFFF848284008284008284008284
        0082840082840082840082840082840082840082840082840082840082840082
        8400828400828400FF0000828400828400828400828400828400828400828400
        8284008284008284008284008284008284008284008284008284008284848284
        0082840082840082840082840082840082840082840082840082840082840082
        8400828400828400828400828400828400828400828400828400828400828400
        8284008284008284008284008284008284008284008284008284008284008284
        008284008284008284008284008284008284}
      NumGlyphs = 2
    end
    object CancelBtn: TBitBtn
      Left = 105
      Top = 14
      Width = 70
      Height = 24
      Cancel = True
      Caption = 'Cancel'
      Default = True
      ModalResult = 2
      TabOrder = 1
      OnClick = CancelBtnClick
      Glyph.Data = {
        CE070000424DCE07000000000000360000002800000024000000120000000100
        1800000000009807000000000000000000000000000000000000008284008284
        0082840082840082840082840082840082840082840082840082840082840082
        8400828400828400828400828400828400828400828400828400828400828400
        8284008284008284008284008284008284008284008284008284008284008284
        0082840082840082840082840082840082848482848482840082840082840082
        8400828400828400828400828400828400828400828400828400828400828400
        8284008284008284008284FFFFFF008284008284008284008284008284008284
        0082840082840082840082840082840082840082840082840082840000FF0000
        840000848482840082840082840082840082840082840000FF84828400828400
        8284008284008284008284008284008284008284848284848284FFFFFF008284
        008284008284008284008284008284FFFFFF0082840082840082840082840082
        840082840082840000FF00008400008400008484828400828400828400828400
        00FF000084000084848284008284008284008284008284008284008284848284
        FFFFFF008284848284FFFFFF008284008284008284FFFFFF848284848284FFFF
        FF0082840082840082840082840082840082840000FF00008400008400008400
        00848482840082840000FF000084000084000084000084848284008284008284
        008284008284008284848284FFFFFF008284008284848284FFFFFF008284FFFF
        FF848284008284008284848284FFFFFF00828400828400828400828400828400
        82840000FF000084000084000084000084848284000084000084000084000084
        000084848284008284008284008284008284008284848284FFFFFF0082840082
        84008284848284FFFFFF848284008284008284008284008284848284FFFFFF00
        82840082840082840082840082840082840000FF000084000084000084000084
        0000840000840000840000848482840082840082840082840082840082840082
        84008284848284FFFFFF00828400828400828484828400828400828400828400
        8284FFFFFF848284008284008284008284008284008284008284008284008284
        0000FF0000840000840000840000840000840000848482840082840082840082
        84008284008284008284008284008284008284848284FFFFFF00828400828400
        8284008284008284008284FFFFFF848284008284008284008284008284008284
        0082840082840082840082840082840000840000840000840000840000848482
        8400828400828400828400828400828400828400828400828400828400828400
        8284848284FFFFFF008284008284008284008284008284848284008284008284
        0082840082840082840082840082840082840082840082840082840000FF0000
        8400008400008400008484828400828400828400828400828400828400828400
        8284008284008284008284008284008284848284FFFFFF008284008284008284
        8482840082840082840082840082840082840082840082840082840082840082
        840082840000FF00008400008400008400008400008484828400828400828400
        8284008284008284008284008284008284008284008284008284008284848284
        008284008284008284008284848284FFFFFF0082840082840082840082840082
        840082840082840082840082840000FF00008400008400008484828400008400
        0084000084848284008284008284008284008284008284008284008284008284
        008284008284848284008284008284008284008284008284848284FFFFFF0082
        840082840082840082840082840082840082840082840000FF00008400008400
        00848482840082840000FF000084000084000084848284008284008284008284
        008284008284008284008284008284848284008284008284008284848284FFFF
        FF008284008284848284FFFFFF00828400828400828400828400828400828400
        82840000FF0000840000848482840082840082840082840000FF000084000084
        000084848284008284008284008284008284008284008284848284FFFFFF0082
        84008284848284008284848284FFFFFF008284008284848284FFFFFF00828400
        82840082840082840082840082840082840000FF000084008284008284008284
        0082840082840000FF0000840000840000840082840082840082840082840082
        84008284848284FFFFFFFFFFFF848284008284008284008284848284FFFFFF00
        8284008284848284FFFFFF008284008284008284008284008284008284008284
        0082840082840082840082840082840082840082840000FF0000840000FF0082
        8400828400828400828400828400828400828484828484828400828400828400
        8284008284008284848284FFFFFFFFFFFFFFFFFF848284008284008284008284
        0082840082840082840082840082840082840082840082840082840082840082
        8400828400828400828400828400828400828400828400828400828400828400
        8284008284008284008284008284008284008284008284848284848284848284
        0082840082840082840082840082840082840082840082840082840082840082
        8400828400828400828400828400828400828400828400828400828400828400
        8284008284008284008284008284008284008284008284008284008284008284
        008284008284008284008284008284008284}
      NumGlyphs = 2
    end
    object Helpbtn: TBitBtn
      Left = 198
      Top = 14
      Width = 70
      Height = 24
      Caption = 'Help'
      TabOrder = 2
      OnClick = HelpbtnClick
      Kind = bkHelp
    end
  end
  object PrintDialog1: TPrintDialog
    Left = 59
    Top = 531
  end
  object PrinterSetupDialog1: TPrinterSetupDialog
    Left = 19
    Top = 531
  end
  object PagedOutputSaveDialog: TSaveDialog
    Left = 88
    Top = 528
  end
  object PagedOutputOpenDialog: TOpenDialog
    Left = 128
    Top = 536
  end
end
