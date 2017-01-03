unit PagedOutput;

{EUGene  Copyright 1997-2003+  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

{This unit has main window for making user selections, and main run button.}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Math,
  StdCtrls, ComCtrls, Buttons, cmnprocd, eutypes1, eutypes2, eutypes3, {euinoutd,}
  Setting, Cntry, Contigu, Mask, ExtCtrls, allianceSource, DistOptions,
  SelectRegion, PeaceYearsOptions;
{uses
  ExtCtrls, }

{The three key procedures in here are
     TOutput_Options.OutputFormShow, which sets all the settings in the display window based
        on user_selections and configuration info; and
     Check_completeness, which evaluates the settings in user_selections and displays
        pop-ups for the user if key elements are missing.
     TOutput_Options.OkbtnClick, which takes all the settings from the display window and
        puts them into the user_selections variable.  }

type
  TOutput_Options = class(TForm)
    PagedOutput: TPageControl;
    OutputDestinationSheet: TTabSheet;
    VariableSheet: TTabSheet;
    CaseSelectionPage: TTabSheet;
    PrintDialog1: TPrintDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    ButtonGroupBox: TGroupBox;
    Okbtn: TBitBtn;
    CancelBtn: TBitBtn;
    Helpbtn: TBitBtn;
    SamplingSheet: TTabSheet;
    Label1: TLabel;
    ExclusionSheet: TTabSheet;
    VariableTabsControl: TPageControl;
    Polity3Sheet: TTabSheet;
    BdMSheet: TTabSheet;
    ConflictTabSheet: TTabSheet;
    GeneralSheet: TTabSheet;
    SystemCharsPanel: TPanel;
    CapabilitiesPanel: TPanel;
    GeographyPanel: TPanel;
    AlliancesSheet: TTabSheet;
    PolityVarsPanel: TPanel;
    democcheckbox: TCheckBox;
    autoccheckbox: TCheckBox;
    xrregCheckBox: TCheckBox;
    xrcompCheckBox: TCheckBox;
    xropenCheckBox: TCheckBox;
    monoCheckBox: TCheckBox;
    xconstCheckBox: TCheckBox;
    parregCheckBox: TCheckBox;
    parcmpCheckBox: TCheckBox;
    centCheckBox: TCheckBox;
    PolityVarsHeader: TLabel;
    PolityAuxPanel: TPanel;
    RussettDem: TCheckBox;
    laggeddemaut: TCheckBox;
    demchangebox: TCheckBox;
    PolityAuxLabel: TLabel;
    StatesInSystemBox: TCheckBox;
    GPsInSystemBox: TCheckBox;
    SysConBox: TCheckBox;
    SysMoveBox: TCheckBox;
    SysMove5Box: TCheckBox;
    SysMoveGPBox: TCheckBox;
    SysMoveGP5Box: TCheckBox;
    SystemCharsLabel: TLabel;
    CcodeVarBox: TCheckBox;
    Abbrevcheckbox: TCheckBox;
    YearVarBox: TCheckBox;
    natcapbox: TCheckBox;
    rawcapbox: TCheckBox;
    MajorPowerCheckBox: TCheckBox;
    ExistTimeCheckBox: TCheckBox;
    CapabilitiesLabel: TLabel;
    RelRegionCheckBox: TCheckBox;
    HomeRegionCheckBox: TCheckBox;
    MarkPolRelevantBox: TCheckBox;
    ContiguityCheckBox: TCheckBox;
    ColonialContiguityCheckBox: TCheckBox;
    DistanceVarBox: TCheckBox;
    ChangePolRelButton: TPanel;
    DistanceOptionsButton: TPanel;
    GeographyLabel: TLabel;
    AlliancesPanel: TPanel;
    AlliancesLabel: TLabel;
    AllianceCheckBox: TCheckBox;
    AlliancePortfolioTableUnweightedCheckBox: TCheckBox;
    AlliancePortfolioTableWeightedCheckBox: TCheckBox;
    Taubox: TCheckBox;
    Sboxunweighted: TCheckBox;
    sboxweighted: TCheckBox;
    TauWithLeaderBox: TCheckBox;
    swithleaderbox: TCheckBox;
    TauWithLeaderButton: TPanel;
    SWithLeaderButton: TPanel;
    WarTrapPanel: TPanel;
    EUWartrapTaubox: TCheckBox;
    EUWarTrapSBox: TCheckBox;
    WarTrapLabel: TLabel;
    WarAndReasonPanel: TPanel;
    WarAndReasonLabel: TLabel;
    riskEUGENETaubox: TCheckBox;
    RiskEUGENESBox: TCheckBox;
    RiskDetailTauCheckBox: TCheckBox;
    RiskDetailSCheckBox: TCheckBox;
    UncertTauCheckBox: TCheckBox;
    UncertSCheckBox: TCheckBox;
    EUReasonTaubox: TCheckBox;
    EUReasonSUnBox: TCheckBox;
    EUReasonSWtBox: TCheckBox;
    EqTauBox: TCheckBox;
    EQSUnBox: TCheckBox;
    EQSWtBox: TCheckBox;
    WTRPanel: TPanel;
    WTRLabel: TLabel;
    RiskWTRCheckBox: TCheckBox;
    EquilibriumButton: TPanel;
    OutputDestinationPanel: TPanel;
    OutputDestinationLabel: TLabel;
    VariableSeparatorPanel: TPanel;
    VariableSeparatorLabel: TLabel;
    CommandFilesPanel: TPanel;
    CommandFileSelectLabel: TLabel;
    HeaderInfoPanel: TPanel;
    HeaderInfoLabel: TLabel;
    ToFileRadioDial: TRadioButton;
    FileDisplay: TEdit;
    ToScreenRadioDial: TRadioButton;
    ToPrinterRadioDial: TRadioButton;
    ChangeOutputFileButton: TPanel;
    PrinterSetupButton: TPanel;
    SeparateWithTabs: TRadioButton;
    SeparateWithSpaces: TRadioButton;
    SeparateWithCommas: TRadioButton;
    Header: TCheckBox;
    SPSSCommandFileCheckBox: TCheckBox;
    StataCommandFileCheckBox: TCheckBox;
    LIMDEPCommandFileCheckBox: TCheckBox;
    commandfileOnlyCheckBox: TCheckBox;
    SamplingPanel: TPanel;
    NoSampleBtn: TRadioButton;
    SamplingYesBtn: TRadioButton;
    NonDisputeSampleLabel: TLabel;
    disputedyadsamplelabel: TLabel;
    RandSeedCheckBox: TCheckBox;
    NonDispSampleEdit: TMaskEdit;
    DispSampleEdit: TMaskEdit;
    RandomSeedEdit: TMaskEdit;
    SamplingLabel: TLabel;
    YearsOutput: TPanel;
    YearsOutputLabel: TLabel;
    All_Yrs: TRadioButton;
    Specified_Yrs: TRadioButton;
    SelfReferencePanel: TPanel;
    SelfReferenceLabel: TLabel;
    PrintAACheckBox: TCheckBox;
    CaseSelectionPanel: TPanel;
    CaseSelectionLabel: TLabel;
    alldyadsfromallstates: TRadioButton;
    Allgpvsgpdyads: TRadioButton;
    allgpvsany: TRadioButton;
    allcontg: TRadioButton;
    MaximumDistanceButton: TRadioButton;
    SpecifiedRegionButton: TRadioButton;
    PolRelevant: TRadioButton;
    specific_dyads: TRadioButton;
    DyadsFromFileButton: TRadioButton;
    DistanceEditBox: TMaskEdit;
    ChangeContiguityButton1: TPanel;
    SelectRegionButton: TPanel;
    ChangeContiguityButton2: TPanel;
    SetCountriesButton: TPanel;
    ChangeInputFileButton: TPanel;
    userdyadfiledisplay: TEdit;
    OngoingMIDPanel: TPanel;
    DividerPanel1: TPanel;
    OngoingMIDLabel: TLabel;
    DropAllOngoingButton: TRadioButton;
    OngoingIfNewDispButton: TRadioButton;
    AllOngoingDisputeYearButton: TRadioButton;
    MarkSubsequentAsInitiationCheckBox: TCheckBox;
    TargetInitiatorPanel: TPanel;
    TargetInitiatorLabel: TLabel;
    AlwaysIncludeTgtVsInitDyadsRadioButton: TRadioButton;
    IncludeTgtVsInitDyadsiffNewRadioButton: TRadioButton;
    TargetDispYearNoteLabel: TStaticText;
    JoinerInclusionPanel: TPanel;
    JoinerInclusionLabel: TLabel;
    IncludeJoinerRadioButton: TRadioButton;
    DropJoinerRadioButton: TRadioButton;
    Panel1: TPanel;
    Edit1: TEdit;
    Edit2: TEdit;
    nationdatlabel: TLabel;
    limitinglabel: TLabel;
    nationyear1label: TLabel;
    nationyear2label: TLabel;
    AllianceYrs: TLabel;
    CapYrs: TLabel;
    AllYrFrom: TLabel;
    CapYrFrom: TLabel;
    AllYrTo: TLabel;
    CApYrTo: TLabel;
    RiskYr: TLabel;
    EUYr: TLabel;
    MIDYearLabel: TLabel;
    polityyearlabel: TLabel;
    polityyear1label: TLabel;
    MIDLabelFrom: TLabel;
    EUYrFrom: TLabel;
    RiskYrFrom: TLabel;
    RiskYrTo: TLabel;
    EUYrTo: TLabel;
    MIDLabelTo: TLabel;
    polityyear2label: TLabel;
    Panel2: TPanel;
    WTRLabel2: TLabel;
    WarTrapLabel2: TLabel;
    Label2: TLabel;
    UserDataTabSheet: TTabSheet;
    Panel3: TPanel;
    ExcludeDyadYearLabel: TLabel;
    ExcludeConflictCOWMIDButton: TRadioButton;
    ExcludeConflictMaozMIDButton: TRadioButton;
    ExcludeConflictICBButton: TRadioButton;
    ExcludeConflictWarsButton: TRadioButton;
    ExcludeConflictNoneButton: TRadioButton;

    MIDSubPage: TPageControl;
    COWMIDTabSheet: TTabSheet;

   {CB changes 2 lines}
    UserDataTabControl: TPageControl;
    MaozMidTabSheet: TTabSheet;
    COWMIDSettingsPanel: TPanel;
    COWMIDBoxLabel: TLabel;
    COWMIDDisputeDataCheckBox: TCheckBox;
    COWMIDDyadicPanel: TPanel;
    COWMIDDyadicLabel: TLabel;
    COWMIDInitiationCheckBox: TCheckBox;
    COWMIDOngoingCheckBox: TCheckBox;
    COWMIDNumCheckBox: TCheckBox;
    COWOutcomeCheckBox: TCheckBox;
    COWSettlementCheckBox: TCheckBox;
    COWFatalityLevelDisputeCheckBox: TCheckBox;
    COWHiActDisputeCheckBox: TCheckBox;
    COWHostLevDisputeCheckBox: TCheckBox;
    COWRecipCheckBox: TCheckBox;
    COWNumOfStatesBox: TCheckBox;
    COWMidNameCheckBox: TCheckBox;
    COWNumMIDCheckBox: TCheckBox;
    COWPeaceYrsBox: TCheckBox;
    PeaceYearsOptionsButton1: TPanel;
    COWMIDMonadicPanel: TPanel;
    COWMIDMonadicLabel: TLabel;
    COWstartdatecheck: TCheckBox;
    COWenddatecheckbox: TCheckBox;
    COWSideACheckBox: TCheckBox;
    COWRevStateCheckBox: TCheckBox;
    COWRevTypeCheckBox: TCheckBox;
    COWFatalityLevelStateCheckBox: TCheckBox;
    COWHiActCheckBox: TCheckBox;
    COWMIDHostLevStateCheckBox: TCheckBox;
    COWOriginatorCheckBox: TCheckBox;
    COWMarkJoinersCheckBox: TCheckBox;
    maozmidsettingspanel: TPanel;
    MaozDispBoxLabel: TLabel;
    MaozDisputeDataCheckBox: TCheckBox;
    MaozMIDDyadicPanel: TPanel;
    MaozMIDDyadicLabel: TLabel;
    MaozMIDInitiationCheckBox: TCheckBox;
    MaozMIDOngoingCheckBox: TCheckBox;
    MaozMIDNumCheckBox: TCheckBox;
    MaozOutcomeCheckBox: TCheckBox;
    MaozSettlementCheckBox: TCheckBox;
    MaozFatalityLevelDisputeCheckBox: TCheckBox;
    MaozHiActDisputeCheckBox: TCheckBox;
    MaozHostLevDisputeCheckBox: TCheckBox;
    MaozRecipCheckBox: TCheckBox;
    MaozNumOfStatesBox: TCheckBox;
    MaozNumMIDCheckBox: TCheckBox;
    MaozPeaceYrsBox: TCheckBox;
    PeaceYearsOptionsButton2: TPanel;
    MaozMIDMonadicPanel: TPanel;
    MaozMIDMonadicLabel: TLabel;
    Maozstartdatecheck: TCheckBox;
    Maozenddatecheckbox: TCheckBox;
    MaozSideACheckBox: TCheckBox;
    MaozRevStateCheckBox: TCheckBox;
    MaozRevTypeCheckBox: TCheckBox;
    MaozFatalityLevelStateCheckBox: TCheckBox;
    MaozHiActCheckBox: TCheckBox;
    MaozMIDHostLevStateCheckBox: TCheckBox;
    MaozOriginatorCheckBox: TCheckBox;
    MaozMarkJoinersCheckBox: TCheckBox;
    MaozMIDOnlyPanel: TPanel;
    MIDMaozLabel: TLabel;
    maozcowwarbox: TCheckBox;
    maozdurindxbox: TCheckBox;
    maozdurdaysbox: TCheckBox;
    maozreciprocateddyadicbox: TCheckBox;
    COWRoleCheckBox: TCheckBox;
    MaozRoleCheckBox: TCheckBox;
    ICBTabSheet: TTabSheet;
    ICBSettingsPanel: TPanel;
    ICBBoxLabel: TLabel;
    ICBChoiceLabel: TLabel;
    ICBDyadicDataCheckBox: TCheckBox;
    ICBDyadicPanel: TPanel;
    Label4: TLabel;
    ICBCrisisCheckBox: TCheckBox;
    ICBOngoingCheckBox: TCheckBox;
    ICBCrisisNameCheckBox: TCheckBox;
    ICBDurDaysCheckBox: TCheckBox;
    ICBDurYearsCheckBox: TCheckBox;
    ICBDyadicStartCheckBox: TCheckBox;
    ICBDyadicEndCheckBox: TCheckBox;
    ICBOneSidedCheckBox: TCheckBox;
    ICBCrisisDyadNumberCheckBox: TCheckBox;
    ICBCrisisNumberCheckBox: TCheckBox;
    ICBNumCrisisCheckBox: TCheckBox;
    ICBPeaceYrsBox: TCheckBox;
    PeaceYearsOptionsButton3: TPanel;
    ICBMonadicPanel: TPanel;
    Label5: TLabel;
    ICBStateStartCheckBox: TCheckBox;
    ICBStateEndCheckBox: TCheckBox;
    ICBJoinerCheckBox: TCheckBox;
    ICBActorSequenceCheckBox: TCheckBox;
    ICBCOWMemberCheckBox: TCheckBox;
    ICBGWMemberCheckBox: TCheckBox;
    ICBIntraWarCheckBox: TCheckBox;
    WarTabSheet: TTabSheet;
    WarSettingsPanel: TPanel;
    COWBoxLabel: TLabel;
    Label7: TLabel;
    COWDyadicWarsDataCheckBox: TCheckBox;
    WarVariablePanel: TPanel;
    WarVarLabel: TLabel;
    NewWarCheckBox: TCheckBox;
    WarOngoingCheckBox: TCheckBox;
    COWWarNumberCheckBox: TCheckBox;
    Label6: TLabel;
    DisputeInitiatorSheet: TTabSheet;
    InitiatorTimingPanel: TPanel;
    InitiatorTimingLabel: TLabel;
    originatorInitFinishLabel: TLabel;
    joinerinitfinishlabel: TLabel;
    JoinerAnyFinishlabel: TLabel;
    TrueInitiatorsCheckBox: TRadioButton;
    JoinerInitCheckBox: TRadioButton;
    JoinerAnyCheckBox: TRadioButton;
    InitiatorIdentityPanel: TPanel;
    InitiatorIdentityLabel: TLabel;
    SideAAsInitiatorCheckBox: TRadioButton;
    InitiationAsRevisionistRadioButton: TRadioButton;
    MultiMidYearPanel: TPanel;
    MultiMidTitle: TLabel;
    MIDmultilabel2: TLabel;
    UseFirstMIDCheckBox: TCheckBox;
    UseMostSeriousMIDCheckBox: TCheckBox;
    MaozMidNameCheckBox: TCheckBox;
    MaozLinkStatusCheckBox: TCheckBox;
    COWLinkStatusCheckBox: TCheckBox;
    COWPeaceDaysCheckBox: TCheckBox;
    MaozPeaceDysBox: TCheckBox;
    ICBpeacedysCheckBox: TCheckBox;
    Panel4: TPanel;
    Label3: TLabel;
    ISO_code_CheckBox: TCheckBox;
    ISO_abb2_CheckBox: TCheckBox;
    ISO_abb3_CheckBox: TCheckBox;
    ISO_short_CheckBox: TCheckBox;
    ISO_full_CheckBox: TCheckBox;
    PagedOutputSaveDialog: TSaveDialog;
    PagedOutputOpenDialog: TOpenDialog;
    procedure OkbtnClick(Sender: TObject);
    procedure Specified_YrsClick(Sender: TObject);
    procedure All_YrsClick(Sender: TObject);
    procedure disk1Click(Sender: TObject);
    procedure ToScreenRadioDialClick(Sender: TObject);
    procedure Printer1Click(Sender: TObject);
    procedure alldyadsfromallstatesClick(Sender: TObject);
    procedure AllgpvsgpdyadsClick(Sender: TObject);
    procedure specific_dyadsClick(Sender: TObject);
    procedure allcontgClick(Sender: TObject);
    procedure allgpvsanyClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure HelpbtnClick(Sender: TObject);
    procedure PrinterSetupButtonClick(Sender: TObject);
    procedure OutputFormShow(Sender: TObject);
    procedure PolRelevantClick(Sender: TObject);
    procedure RiskWTRCheckBoxClick(Sender: TObject);
    procedure riskEUGENETauboxClick(Sender: TObject);
    procedure riskEUGENESboxClick(Sender: TObject);
    procedure ChangeCountriesButtonClick(Sender: TObject);
    procedure ChangeContiguityButton1Click(Sender: TObject);
    procedure ChangeContiguityButton2Click(Sender: TObject);
    procedure FileChangeButtonClick(Sender: TObject);
    procedure COWMIDDisputeDataCheckBoxClick(Sender: TObject);
    procedure eq_options_buttonClick(Sender: TObject);
    procedure system_variable_optionboxClick(Sender: TObject);
    procedure SamplingYesBtnClick(Sender: TObject);
    procedure NoSampleBtnClick(Sender: TObject);
    procedure DyadsFromFileButtonClick(Sender: TObject);
    procedure changeuserdyadfilebuttonClick(Sender: TObject);
    procedure ChangeContiguityButton3Click(Sender: TObject);
    procedure RiskDetailTauCheckBoxClick(Sender: TObject);
    procedure RiskDetailSCheckBoxClick(Sender: TObject);
    procedure UncertTauCheckBoxClick(Sender: TObject);
    procedure UncertSCheckBoxClick(Sender: TObject);
    {procedure AllianceButtonClick(Sender: TObject);  }
    {procedure MIDVariableSelectionButtonClick(Sender: TObject);}
    procedure MaozDisputeDataCheckBoxClick(Sender: TObject);
    procedure DistanceVarOptionsButtonClick(Sender: TObject);
    procedure maxdistclick(Sender: TObject);
    procedure spec_regionClick(Sender: TObject);
    procedure SetMaxDistance(Sender: TObject);
    procedure Select_RegionButtonClick(Sender: TObject);
    procedure COWMIDInitiationCheckBoxClick(Sender: TObject);
    procedure COWMIDOngoingCheckBoxClick(Sender: TObject);
    procedure COWMIDHostLevStateCheckBoxClick(Sender: TObject);
    procedure CcodeVarBoxClick(Sender: TObject);
    procedure YearVarBoxClick(Sender: TObject);
    procedure ICBDyadicDataCheckBoxClick(Sender: TObject);
    procedure COWDyadicWarsDataCheckBoxClick(Sender: TObject);
    procedure ICBCrisisCheckBoxClick(Sender: TObject);
    procedure ICBOngoingCheckBoxClick(Sender: TObject);
    procedure NewWarCheckBoxClick(Sender: TObject);
    procedure WarOngoingCheckBoxClick(Sender: TObject);
    procedure ExcludeConflictWarsButtonClick(Sender: TObject);
    procedure ExcludeConflictCOWMIDButtonClick(Sender: TObject);
    procedure ExcludeConflictNoneButtonClick(Sender: TObject);
    procedure ExcludeConflictICBButtonClick(Sender: TObject);
    procedure ExcludeConflictMaozMIDButtonClick(Sender: TObject);
    procedure MaozMIDHostLevStateCheckBoxClick(Sender: TObject);
    procedure MaozMIDInitiationCheckBoxClick(Sender: TObject);
    procedure MaozMIDOngoingCheckBoxClick(Sender: TObject);
    procedure UseFirstMIDCheckBoxClick(Sender: TObject);
    procedure UseMostSeriousMIDCheckBoxClick(Sender: TObject);
    procedure GenerateUserVarPanels(Sender: TObject);
    procedure COWMIDNumCheckBoxClick(Sender: TObject);
    procedure MaozMIDNumCheckBoxClick(Sender: TObject);
    procedure ICBCrisisNumberCheckBoxClick(Sender: TObject);
    procedure PeaceYearsOptionsButton1Click(Sender: TObject);
    procedure PeaceYearsOptionsButton2Click(Sender: TObject);
    procedure PeaceYearsOptionsButton3Click(Sender: TObject);
    procedure PeaceYearsOptionsButtonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PeaceYearsOptionsButtonMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PolRelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PolRelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DistOptionsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DistOptionsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TauWithLeaderMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TauWithLeaderMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SWithLeaderMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SWithLeaderMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EquilibriumMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EquilibriumMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ChangeOutputFileMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ChangeOutputFileMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PrinterSetupMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PrinterSetupMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Contiguity1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Contiguity1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Contiguity2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Contiguity2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SelectRegionMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SelectRegionMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetCountriesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetCountriesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ChangeInputFileMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ChangeInputFileMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    procedure SetUserVarTabs(var user_selections: user_selection_type; var configuration: configuration_type);
    procedure Enable_Disable_exclusions;
    procedure Enable_Disable_initiation_settings;
    procedure check_completeness (var user_selections: user_selection_type);
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

uses
   mdiframe, GenrHelp, EUOptions, TauOptions, errbx, TraceUnit ;

{----------------------------------------------------------------------}

procedure TOutput_Options.Enable_Disable_initiation_settings;
   {this proc just sets initiation setting tabs to be enabled or not, depending on whether
    any conflict variables are selected for output.  }
   var x : integer;
   begin
      if (COWMIDDisputeDataCheckBox.Checked = true) or
         (MaozDisputeDataCheckBox.Checked = true) or
         (ICBDyadicDataCheckBox.Checked = true) or
         (COWDyadicWarsDataCheckBox.Checked = true) then
         begin
            InitiatorTimingPanel.enabled := true;
            for x := 0 to InitiatorTimingPanel.controlcount-1 do
               InitiatorTimingPanel.controls[x].enabled := true;
            {The side A / rev only applies to MIDs, not ICB or wars.}
            if (COWMIDDisputeDataCheckBox.Checked = true) or
               (MaozDisputeDataCheckBox.Checked = true) then
               begin
                  InitiatorIdentityPanel.enabled := true;
                  for x := 0 to InitiatorIdentityPanel.controlcount-1 do
                     InitiatorIdentityPanel.controls[x].enabled := true;
                  MultiMidYearPanel.enabled := true;
                  for x := 0 to MultiMidYearPanel.controlcount-1 do
                     MultiMidYearPanel.controls[x].enabled := true;
               end
               else
                  begin
                     InitiatorIdentityPanel.enabled := false;
                     for x := 0 to InitiatorIdentityPanel.controlcount-1 do
                        InitiatorIdentityPanel.controls[x].enabled := false;
                     MultiMidYearPanel.enabled := false;
                     for x := 0 to MultiMidYearPanel.controlcount-1 do
                        MultiMidYearPanel.controls[x].enabled := false;
                  end;
         end
      else   {none is checked, so make them inactive}
         begin
            InitiatorTimingPanel.enabled := false;
            InitiatorIdentityPanel.enabled := false;
            for x := 0 to InitiatorTimingPanel.controlcount-1 do
               InitiatorTimingPanel.controls[x].enabled := false;
            for x := 0 to InitiatorIdentityPanel.controlcount-1 do
               InitiatorIdentityPanel.controls[x].enabled := false;
            MultiMidYearPanel.enabled := false;
            for x := 0 to MultiMidYearPanel.controlcount-1 do
               MultiMidYearPanel.controls[x].enabled := false;
         end;
   end;

procedure TOutput_Options.Enable_Disable_exclusions;
   {this proc just sets exclusion tabs to be enabled or not, depending on whether an exclusion
    set is selected.}
   var x : integer;
   begin
      if (ExcludeConflictNoneButton.Checked = true)  then {if no set checked, enable exclusions}
         begin
            OngoingMIDPanel.enabled := false;
            TargetInitiatorPanel.enabled := false;
            JoinerInclusionPanel.enabled := false;
            for x := 0 to OngoingMIDPanel.controlcount-1 do
               OngoingMIDPanel.controls[x].enabled := false;
            for x := 0 to TargetInitiatorPanel.controlcount-1 do
               TargetInitiatorPanel.controls[x].enabled := false;
            for x := 0 to JoinerInclusionPanel.controlcount-1 do
               JoinerInclusionPanel.controls[x].enabled := false;
         end
      else   {one is checked, so make them active}
         begin
            OngoingMIDPanel.enabled := true;
            TargetInitiatorPanel.enabled := true;
            JoinerInclusionPanel.enabled := true;
            for x := 0 to OngoingMIDPanel.controlcount-1 do
               OngoingMIDPanel.controls[x].enabled := true;
            for x := 0 to TargetInitiatorPanel.controlcount-1 do
               TargetInitiatorPanel.controls[x].enabled := true;
            for x := 0 to JoinerInclusionPanel.controlcount-1 do
               JoinerInclusionPanel.controls[x].enabled := true;
         end;
   end;

procedure TOutput_Options.Specified_YrsClick(Sender: TObject);
   begin
      user_selections.years := subrange;
      Specified_Yrs.checked := true;
      All_Yrs.checked := false;
   end;


{----------------------------------------------------------------------}

procedure TOutput_Options.All_YrsClick(Sender: TObject);
   begin
       SettingsForm.label1.caption:='Start Year Selected = ' + inttostr(configuration.first_any_year);
       SettingsForm.label2.caption:='End Year Selected = ' + inttostr(configuration.last_any_year);
       user_selections.years := all;
       user_selections.first_year := configuration.first_nation_year;
       user_selections.last_year := configuration.last_nation_year;
       All_Yrs.checked := true;
       Specified_Yrs.checked := false;
   end;

{----------------------------------------------------------------------}



procedure TOutput_Options.disk1Click(Sender: TObject);
   begin
      if not (user_selections.output_format.location = tofile) then FileChangeButtonClick (self);
   end;


procedure TOutput_Options.FileChangeButtonClick(Sender: TObject);
var fileok : boolean;
begin
      PagedOutputSaveDialog.Filter := 'Output files (*.OUT)|*.OUT|Text files (*.TXT)|*.TXT|All files (*.*)|*.*';
      PagedOutputSaveDialog.FileName := user_selections.output_format.output_file_name;
      PagedOutputSaveDialog.Options := [ofOverwritePrompt, ofHideReadOnly];
      fileok := false;
      user_selections.output_format.overwrite := false;
      if PagedOutputSaveDialog.execute then
         begin
            if (extractFileExt(PagedOutputSaveDialog.FileName) = '.') or (extractFileExt(PagedOutputSaveDialog.FileName) = '') then
               begin    {add .edf}
                  PagedOutputSaveDialog.FileName := changeFileExt(PagedOutputSaveDialog.FileName, '.raw');
                  showmessage ('Your output data file must have a 3 character extension (like ".out" ".dat" or ".raw") to ensure proper reading by statistical software.  EUGene has renamed your output file to '+PagedOutputSaveDialog.FileName);
               end;
            user_selections.output_format.overwrite := true;
            fileok:= true;
            user_selections.output_format.output_file_name := PagedOutputSaveDialog.FileName;
            FileDisplay.text:=PagedOutputSaveDialog.FileName;
            user_selections.output_format.location := tofile;
            SettingsForm.outputlb.caption:='To File';
            user_selections.output_format.output_set := true;
            trace.message ('Output selected to '+user_selections.output_format.output_file_name);
            ToFileRadioDial.checked := true;
            CommandFilesPanel.enabled := true;
            SPSSCommandFileCheckBox.enabled := true;
            StataCommandFileCheckBox.enabled := true;
            LIMDEPCommandFileCheckBox.enabled := true;
            CommandFileOnlyCheckBox.enabled := true;
         end
      else {exited with cancel}
         begin
            user_selections.output_format.overwrite := false;
            user_selections.output_format.output_set := false;
            user_selections.output_format.location := tonone;
            ToFileRadioDial.checked := false;
            fileOK := false;
         end;
end;

{------------------------------------------------------------------------}

procedure TOutput_Options.ToScreenRadioDialClick(Sender: TObject);
begin
   SettingsForm.outputlb.caption:='To Screen';
   {Want to open a new window, probably associate it with outfile}
   user_selections.output_format.location := toscreen;
       {outputwindow.screen_output.lines.add (toscreen);}
   user_selections.output_format.output_set := true;
   CommandFilesPanel.enabled := false;
   {must uncheck the command files only box, b/c cmd file can't be generated without file output}
   SPSSCommandFileCheckBox.enabled := false;
   StataCommandFileCheckBox.enabled := false;
   LIMDEPCommandFileCheckBox.enabled := false;
   CommandFileOnlyCheckBox.enabled := false;
   CommandFileOnlyCheckBox.checked := false;

end;

{------------------------------------------------------------------------}

procedure TOutput_Options.Printer1Click(Sender: TObject);
begin
    SettingsForm.outputlb.caption:='To Printer';
    user_selections.output_format.location := toprinter;
    user_selections.output_format.output_set := true;
    CommandFilesPanel.enabled := false;
   {must uncheck the command files only box, b/c cmd file can't be generated without file output}
   SPSSCommandFileCheckBox.enabled := false;
   StataCommandFileCheckBox.enabled := false;
   LIMDEPCommandFileCheckBox.enabled := false;
   CommandFileOnlyCheckBox.enabled := false;
   CommandFileOnlyCheckBox.checked := false;
end;

{----------------------------------------------------------------------}



procedure TOutput_Options.PrinterSetupButtonClick(Sender: TObject);
begin
   PrinterSetupDialog1.execute;
end;

{-----------------------------------------------------------------------------}

procedure TOutput_Options.alldyadsfromallstatesClick(Sender: TObject);
begin
    if ((user_selections.output_this = output_directed_dyads) or
        (user_selections.output_this = output_nondirected_dyads)) then
       begin
          user_selections.dyads_selected := all_states;
          SettingsForm.cntryLabel.caption:='Countries Selected = All Dyads from All States';
       end
    else if user_selections.output_this = output_monads then
       begin
          user_selections.monads_selected := all_states_mono;
          SettingsForm.cntryLabel.caption:='Countries Selected = All States';
       end
    else showmessage ('Error - program values undefined in all dyads click');
    SettingsForm.ListBox1.visible:=false;
end;

{------------------------------------------------------------------------}

procedure TOutput_Options.AllgpvsgpdyadsClick(Sender: TObject);
begin
    if ((user_selections.output_this = output_directed_dyads) or
        (user_selections.output_this = output_nondirected_dyads)) then
       begin
          user_selections.dyads_selected := all_gp_gp;
          SettingsForm.cntryLabel.caption:='Countries Selected = All Major Power Vs. Major Power Dyads';
       end
    else if user_selections.output_this = output_monads then
       begin
          user_selections.monads_selected := all_gp_mono;
          SettingsForm.cntryLabel.caption:='Countries Selected = All Major Powers';
       end
    else showmessage ('Error - program values undefined in all Major Power dyads click');
    SettingsForm.ListBox1.visible:=false;
end;

{-------------------------------------------------------------------------}

procedure TOutput_Options.specific_dyadsClick(Sender: TObject);
begin
   {if there are countries previously (and still) in the list, just check the box,
    otherwise call the subwindow to pick countries. }
   if ((user_selections.output_this = output_monads) and
       (user_selections.selected_country_list.num_countries < 1)) or
      (
       ((user_selections.output_this = output_directed_dyads) or
        (user_selections.output_this = output_nondirected_dyads)) and
        (user_selections.selected_country_list.num_countries < 2)) then
      ChangeCountriesButtonClick (self)
   else
      begin
        if ((user_selections.output_this = output_directed_dyads) or
            (user_selections.output_this = output_nondirected_dyads)) then
             begin
                user_selections.dyads_selected := selected_set;
                SettingsForm.cntryLabel.caption:='Countries Selected = Specified Subset';
             end
          else if user_selections.output_this = output_monads then
             begin
                user_selections.monads_selected := selected_set_mono;
                SettingsForm.cntryLabel.caption:='Countries Selected = Specified Subset';
             end
      end;
   SettingsForm.ListBox1.visible:=true;
end;
{------------------------------------------------------------------------}

procedure TOutput_Options.maxdistclick(Sender: TObject);
begin
   SettingsForm.cntryLabel.caption:='Countries Selected = States Within Maximum Distance';
   user_selections.dyads_selected := within_distance;
   SettingsForm.ListBox1.visible:=false;
end;

procedure TOutput_Options.SetMaxDistance(Sender: TObject);
begin
   user_selections.maximum_distance := strtointdef (DistanceEditBox.text, 12500);
end;

{------------------------------------------------------------------------}

procedure TOutput_Options.spec_regionClick(Sender: TObject);
begin
   if ((user_selections.output_this = output_directed_dyads) or
       (user_selections.output_this = output_nondirected_dyads)) then
      begin
         user_selections.dyads_selected := within_region;
         SettingsForm.cntryLabel.caption:='Countries Selected = States Within Specified Region(s)';
      end
   else if user_selections.output_this = output_monads then
      begin
         user_selections.monads_selected := within_region_mono;
         SettingsForm.cntryLabel.caption:='Countries Selected = States Within Specified Region(s)';
      end
   else showmessage ('Error - program values undefined in within region click');
   SettingsForm.ListBox1.visible:=false;
end;

procedure TOutput_Options.Select_RegionButtonClick(Sender: TObject);
begin
   RegionSelectForm.showmodal;
   user_selections.dyads_selected := within_region;
   SpecifiedRegionButton.checked := true;
   SettingsForm.cntryLabel.caption:='Countries Selected = Region';
end;

{------------------------------------------------------------------------}

procedure TOutput_Options.allcontgClick(Sender: TObject);
begin
   SettingsForm.cntryLabel.caption:='Countries Selected = Contiguous States';
   user_selections.dyads_selected := all_contiguous;
   SettingsForm.ListBox1.visible:=false;
end;

{-------------------------------------------------------------------------}

procedure TOutput_Options.allgpvsanyClick(Sender: TObject);
begin
   SettingsForm.cntryLabel.caption:='Countries Selected = All Major Powers vs. Any Other State';
   user_selections.dyads_selected := all_gp_any;
   SettingsForm.ListBox1.visible:=false;
end;

{------------------------------------------------------------------------}

procedure TOutput_Options.PolRelevantClick(Sender: TObject);
begin
   SettingsForm.cntryLabel.caption:='Countries Selected = Politically Relevant Dyads';
   user_selections.dyads_selected := politically_relevant;
   SettingsForm.ListBox1.visible:=false;
end;

{------------------------------------------------------------------------}

procedure TOutput_Options.DyadsFromFileButtonClick(Sender: TObject);
begin
   if not (user_selections.dyads_selected = user_file_read) then
      begin
         changeuserdyadfilebuttonClick(self);
      end;
end;

procedure TOutput_Options.changeuserdyadfilebuttonClick(Sender: TObject);
begin
      PagedOutputOpenDialog.Filter := 'Text files (*.TXT)|*.TXT|All files (*.*)|*.*';
      PagedOutputOpenDialog.FileName := user_selections.user_specified_dyad_list.file_name;
      if PagedOutputOpenDialog.execute then
         begin
            user_selections.dyads_selected := user_file_read;
            DyadsFromFileButton.checked := true;
            user_selections.user_specified_dyad_list.file_name := PagedOutputOpenDialog.FileName;
            UserDyadFileDisplay.text :=PagedOutputOpenDialog.FileName;
            SettingsForm.cntryLabel.caption:='Cases Selected = Dyads from file';
            SettingsForm.ListBox1.visible:=false;
         end
      else {exited with cancel}
         begin
            user_selections.dyads_selected := not_selected;
            DyadsFromFileButton.checked := false;
         end;
end;


{------------------------------------------------------------------------}

procedure TOutput_Options.ChangeCountriesButtonClick(Sender: TObject);
var
  CountryListDlg: TCountryListDlg;

begin
   try
      CountryListDlg := TCountryListDlg.Create(self);
      if CountryListDlg.showmodal = mrOK then specific_dyads.checked := true;
   finally
      CountryListDlg.Free;
   end;

   {The dialog box sets the button to show if they select countries properly}
end;

{------------------------------------------------------------------------}

procedure TOutput_Options.ChangeContiguityButton1Click(Sender: TObject);
begin
   if ContiguityBox.showmodal = mrOK then
      allcontg.checked := true;
end;

procedure TOutput_Options.ChangeContiguityButton2Click(Sender: TObject);
begin

   if ContiguityBox.showmodal = mrOK then
      PolRelevant.checked := true;
end;

procedure TOutput_Options.ChangeContiguityButton3Click(Sender: TObject);
begin
      if (ContiguityBox.showmodal=mrOK) then MarkPolRelevantBox.checked := true;
end;

{----------------------------------------------------------------------}

procedure TOutput_Options.SetUserVarTabs(var user_selections: user_selection_type; var configuration: configuration_type);

   const numcolumns = 4; {This sets the number of variable box columns in the user data tabs}

   var
      num_user_data_sets, Vars_added, column, row, tabsheet, varloop,
         varcounter, totaluservars, numsubtabs, subtab, varloopbound,
         smallpagelimit, largepagelimit : integer;
      atabsheet, asubtabsheet: TTabSheet;
      apanel: TPanel;
      asubpagecontrol: TPageControl;
      AboveTabControl: TTabControl;
      avar : integer;
      alabel_full_name : TLabel;
      alabel_year_range : TLabel;
      alabel_unit_of_analysis : TLabel;
      alabel_citation : TLabel;

   begin

         num_user_data_sets := configuration.user_data_set_info.get_num_data_sets;

         {Need to get a count of how many total user variables need check boxes.  Then set up
          a structure in memory to hold those checkboxes.  This will allow us to just loop through
          all the check boxes to look for checks, without worrying about what data set they are in.}
         totaluservars := 0;
         for tabsheet := 0 to (num_user_data_sets - 1) do
         begin
            atabsheet := TTabsheet.create(userdatatabsheet);
            atabsheet.PageControl := UserDataTabControl;  {new tabsheet}
            UserDataTabControl.Pages[tabsheet].name := 'UserVarTab'+inttostr(tabsheet);  {This is an internal Delphi name}
            {make the displayed caption/name below the name of the user data set: }
            UserDataTabControl.Pages[tabsheet].caption := configuration.user_data_set_info.get_data_set_short_name (tabsheet);
            UserDataTabControl.Pages[tabsheet].hint := configuration.user_data_set_info.get_data_set_full_name (tabsheet);
            UserDataTabControl.MultiLine := true;
            {add to the running total of user variables so we can set the length of the array}
            for avar := 0 to configuration.User_data_set_info.get_data_set_num_vars(tabsheet) - 1 do
               if ( (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, avar) <> identifierccode1) and
                    (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, avar) <> identifierccode2) and
                    (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, avar) <> identifieryear) ) then
               inc(totaluservars);

         end;

         setlength (VarFormCheckBoxes, totaluservars);

         vars_added := 0;  {vars_added will be our running counter of var boxes put onto tabs}
                           {it should sum at the end to equal totaluservars}
         for tabsheet := 0 to (num_user_data_sets - 1) do
            begin
               largepagelimit := numcolumns*Trunc((UserDataTabControl.Pages[tabsheet].height-48)/18);
               {The number 90 in the following equation is a guess.  It works fine the way things are
                set up right now, but smallpagelimit may be sensitive to other changes in the layout.}
               smallpagelimit := numcolumns*Trunc((UserDataTabControl.Pages[tabsheet].height-90)/18);

               varcounter := configuration.User_data_set_info.get_data_set_num_vars(tabsheet);
               if varcounter > largepagelimit    {if we have more variables than will fit on a single, larger panel}
                  then numsubtabs := Ceil(varcounter/smallpagelimit)  {then find out how many subtabs we need
                                                                        when the vars are split amoung smaller panels}
                  else numsubtabs := 1;

               if (numsubtabs = 1) then
                  begin
                     apanel := TPanel.create(UserDataTabControl.Pages[tabsheet]);
                     apanel.name := 'VarPanel'+inttostr(tabsheet)+'0';
                     apanel.caption := '';
                     apanel.parent := UserDataTabControl.Pages[tabsheet];
                     apanel.left := 12;
                     apanel.width := apanel.parent.ClientWidth - 24;
                     apanel.top := 40;
                     apanel.height := apanel.parent.ClientHeight - 48;
                     apanel.BevelInner := bvLowered;
                     apanel.BevelOuter := bvRaised;
                     apanel.BevelWidth := 1;
                     apanel.Color := clSilver;

                     {Need new label:}
                     alabel_full_name := tlabel.create(UserDataTabControl.Pages[tabsheet]);
                     alabel_full_name.name := 'TabLabel'+inttostr(tabsheet)+'0';
                     alabel_full_name.caption := configuration.user_data_set_info.get_data_set_full_name
                      (tabsheet);
                     alabel_full_name.parent := UserDataTabControl.Pages[tabsheet];
                     alabel_full_name.left := 12;

                     {alabel_citation := tlabel.Create(UserDataTabControl.Pages[tabsheet]);
                     alabel_citation.name := 'TabCitationLabel'+inttostr(tabsheet)+'0';
                     alabel_citation.caption := configuration.user_data_set_info.get_data_set_citation(tabsheet);
                     alabel_citation.parent := UserDataTabControl.Pages[tabsheet];
                     alabel_citation.Left := 12;
                     alabel_citation.Top := 16;   }

                     alabel_year_range := tlabel.create(UserDataTabControl.Pages[tabsheet]);
                     alabel_year_range.name := 'TabYearLabel'+inttostr(tabsheet)+'0';
                     alabel_year_range.Caption := 'Data Coverage: '+inttostr(configuration.User_data_set_info.get_data_set_first_year_possible(tabsheet))+' to '+inttostr(configuration.User_data_set_info.get_data_set_last_year_possible(tabsheet));
                     alabel_year_range.Parent := UserDataTabControl.Pages[tabsheet];
                     alabel_year_range.Left := 12;
                     alabel_year_range.Top := 16;

                     alabel_unit_of_analysis := tlabel.create(UserDataTabControl.Pages[tabsheet]);
                     alabel_unit_of_analysis.name := 'TabUnitLabel'+inttostr(tabsheet)+'0';
                     {take data type and show what type it is
                      data_set_unit : dataset_unit_of_analysis_type;}
                     {dataset_unit_of_analysis_type = (no_unit_dataset,
                      country_year, directed_dyad_year, nondirected_dyad_year, annual_data);}
                     if(configuration.User_data_set_info.get_data_set_unit(tabsheet) = no_unit_dataset) then
                      alabel_unit_of_analysis.Caption := 'Unit of Analysis: None';
                     if(configuration.User_data_set_info.get_data_set_unit(tabsheet) = country_year) then
                      alabel_unit_of_analysis.Caption := 'Unit of Analysis: Country-Year';
                     if(configuration.User_data_set_info.get_data_set_unit(tabsheet) = directed_dyad_year) then
                      alabel_unit_of_analysis.Caption := 'Unit of Analysis: Directed Dyad Year';
                     if(configuration.User_data_set_info.get_data_set_unit(tabsheet) = nondirected_dyad_year) then
                      alabel_unit_of_analysis.Caption := 'Unit of Analysis: Nondirected Dyad Year';
                     if(configuration.User_data_Set_info.get_data_set_unit(tabsheet) = annual_data) then
                      alabel_unit_of_analysis.Caption := 'Unit of Analysis: Annual Data';
                     alabel_unit_of_analysis.Parent := UserDataTabControl.Pages[tabsheet];
                     alabel_unit_of_analysis.Left := 200;
                     alabel_unit_of_analysis.Top := 16;

                     column := 0;
                     row := 0;
                     for varloop := 0 to (varcounter - 1) do  {add var boxes to the existing panel}
                        begin
                           if ( (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) <> identifierccode1) and
                                (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) <> identifierccode2) and
                                (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) <> identifieryear) ) then
                           begin
                              VarFormCheckBoxes[vars_added].original_user_var_num := varloop;
                              VarFormCheckBoxes[vars_added].original_user_dataset_num := tabsheet;
                              VarFormCheckBoxes[vars_added].ACheckBox := TCheckBox.Create(UserDataTabControl.Pages[tabsheet]);
                              VarFormCheckBoxes[vars_added].ACheckBox.name := 'VarBox'+inttostr(vars_added);
                              VarFormCheckBoxes[vars_added].ACheckBox.parent := apanel;
                              VarFormCheckBoxes[vars_added].ACheckBox.Caption := configuration.user_data_set_info.get_data_set_var_name (tabsheet, varloop);
                              VarFormCheckBoxes[vars_added].ACheckBox.height := 18;
                              VarFormCheckBoxes[vars_added].ACheckBox.width := 80;
                              VarFormCheckBoxes[vars_added].ACheckBox.left := 12 + (column*(Trunc((VarFormCheckBoxes[vars_added].ACheckBox.Parent.ClientWidth-36)/numcolumns)+12));
                              VarFormCheckBoxes[vars_added].ACheckBox.top := 12 + row*18;
                              VarFormCheckBoxes[vars_added].ACheckBox.BringToFront;
                              VarFormCheckBoxes[vars_added].ACheckBox.checked := false;
                              inc(row);
                              if (VarFormCheckBoxes[vars_added].ACheckBox.Top + VarFormCheckBoxes[vars_added].ACheckBox.Height) >
                                 (VarFormCheckBoxes[vars_added].ACheckBox.Parent.ClientHeight - 30) then
                                 begin  {Need a new column for next variable.}
                                    inc(column);
                                    row := 0;
                                 end;
                              inc(vars_added);
                           end;
                        end;        {for varloop}

                  end  {if only one subtab}
               else    {If necessary, create new subtab sheets.}
                     begin
                        asubpagecontrol := TPageControl.create (UserDataTabControl.Pages[tabsheet]);
                        asubpagecontrol.name := 'SubPageControl'+inttostr(tabsheet);
                        asubpagecontrol.parent := UserDataTabControl.Pages[tabsheet];
                        asubpagecontrol.left := 12;
                        asubpagecontrol.width := asubpagecontrol.parent.ClientWidth - 24;
                        asubpagecontrol.top := 12;
                        asubpagecontrol.height := asubpagecontrol.parent.ClientHeight - 24;
                        for subtab := 0 to (numsubtabs - 1) do
                           begin
                              asubtabsheet := TTabsheet.create(asubpagecontrol);
                              asubtabsheet.name := 'SubTabSheet'+inttostr(tabsheet)+inttostr(subtab);
                              asubtabsheet.pagecontrol := asubpagecontrol;
                              asubtabsheet.caption := inttostr(subtab);

                              apanel := TPanel.create(asubtabsheet);
                              apanel.name := 'VarPanel'+inttostr(tabsheet)+inttostr(subtab);
                              apanel.caption := '';
                              apanel.parent := asubtabsheet;
                              apanel.left := 12;
                              apanel.width := apanel.parent.ClientWidth - 24;
                              apanel.top := 12;
                              apanel.height := apanel.parent.ClientHeight - 24;
                              apanel.BevelInner := bvLowered;
                              apanel.BevelOuter := bvRaised;
                              apanel.BevelWidth := 1;
                              apanel.Color := clSilver;
                              column := 0;
                              row := 0;

                              {Because we have multiple subtabs, we need to loop through the variables, keeping track
                               of the number of variables already assigned to the panel from this dataset (varloop),
                               as well as the number of variables added overall (vars_added).  We are already in a "subtab"
                               for loop, so we need to assign the bounds of the "varloop" for loop such that we only assign
                               variables destined for the current subtab to the panel.  We begin each "varloop" for loop at
                               the lowest possible varloop number for this subtab, and end it after either the small page
                               variable limit or the total number of vars in the user data set, whichever is smaller.}
                              varloopbound := min((smallpagelimit*(subtab+1))-1,(varcounter-1));
                              for varloop := (smallpagelimit * subtab) to varloopbound do
                                 begin
                                    if ( (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) <> identifierccode1) and
                                         (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) <> identifierccode2) and
                                         (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) <> identifieryear) ) then
                                    begin
                                       VarFormCheckBoxes[vars_added].original_user_var_num := varloop;
                                       VarFormCheckBoxes[vars_added].original_user_dataset_num := tabsheet;
                                       VarFormCheckBoxes[vars_added].ACheckBox := TCheckBox.Create(UserDataTabControl.Pages[tabsheet]);
                                       VarFormCheckBoxes[vars_added].ACheckBox.name := 'VarBox'+inttostr(vars_added);
                                       VarFormCheckBoxes[vars_added].ACheckBox.parent := apanel;
                                       VarFormCheckBoxes[vars_added].ACheckBox.Caption := configuration.user_data_set_info.get_data_set_var_name (tabsheet, varloop);
                                       VarFormCheckBoxes[vars_added].ACheckBox.height := 18;
                                       VarFormCheckBoxes[vars_added].ACheckBox.width := 80;
                                       VarFormCheckBoxes[vars_added].ACheckBox.left := 12 + (column*(Trunc((VarFormCheckBoxes[vars_added].ACheckBox.Parent.ClientWidth-36)/numcolumns)+12));
                                       VarFormCheckBoxes[vars_added].ACheckBox.top := 12 + row*18;
                                       VarFormCheckBoxes[vars_added].ACheckBox.BringToFront;
                                       VarFormCheckBoxes[vars_added].ACheckBox.checked := false;
                                       inc(row);
                                       {We don't need to check to see if there are too many vars for the subtab, because we already
                                        limited ourselves with the variable varloopbound.}
                                       if (VarFormCheckBoxes[vars_added].ACheckBox.Top + VarFormCheckBoxes[vars_added].ACheckBox.Height) >
                                          (VarFormCheckBoxes[vars_added].ACheckBox.Parent.ClientHeight - 30) then
                                          begin  {Need a new column for next variable.}
                                             inc(column);
                                             row := 0;
                                          end;
                                       inc(vars_added);
                                    end;
                              end;        {for varloop}

                        end; {for subtab}
                  end; {else if more than one subtab}
            end;  {for tabsheet}
         if vars_added <> totaluservars then EUGeneError ('Programming error - number of check boxes added to form unequal to number available in directory.  Notify programmer of error.',1,stop,error_log);
         if vars_added <> length (VarFormCheckBoxes) then EUGeneError ('Programming error - number of check boxes added to form unequal to number loaded from documentation.  Notify programmer of error.',1,stop,error_log);

   end;   {proc  SetUserVarTabs}

{------------------------------------------------------------------------}

procedure TOutput_Options.GenerateUserVarPanels(Sender: TObject);

var varbox: integer;
begin
   if configuration.User_data_set_info.get_num_data_sets > 0 then
      begin
         UserDataTabSheet.enabled := true;
         SetUserVarTabs(user_selections, configuration);    {this creates user var check boxes}
         for varbox := 0 to high(VarFormCheckBoxes) do
            begin
               VarFormCheckBoxes[varbox].ACheckBox.enabled := true;
               case configuration.user_data_set_info.get_data_set_var_unit(VarFormCheckBoxes[varbox].original_user_dataset_num, VarFormCheckBoxes[varbox].original_user_var_num) of
                  dyadic_ordered   : if    (user_selections.output_this = output_monads)
                                        or (user_selections.output_this = output_directed_dispute_initiation_dyads)  or (user_selections.output_this = output_nondirected_dispute_dyads)
                                        then VarFormCheckBoxes[varbox].ACheckBox.enabled := false;
                  dyadic_unordered : if    (user_selections.output_this = output_monads)
                                        or (user_selections.output_this = output_directed_dispute_initiation_dyads) or (user_selections.output_this = output_nondirected_dispute_dyads)
                                        then VarFormCheckBoxes[varbox].ACheckBox.enabled := false;
               end;

            end;
      end
      else UserDataTabSheet.enabled := false;
end;

{------------------------------------------------------------------------}

procedure TOutput_Options.OutputFormShow(Sender: TObject);
var varbox, x: integer;
    tabsheet, varcounter, varloop : integer;
    make_visible : boolean;
begin

   try
      {Output Sheets information}
      pagedOutput.ActivePage := OutputDestinationSheet;

      {Must set to one, then switch to other, to get boxes to show up right.}
      MIDSubPage.ActivePage := MaozMIDTabSheet;
      MIDSubPage.ActivePage := COWMIDTabSheet;

      {Note: once war data is added, remove the following hide of the war tab.}
      {Note:  It is also set below within the procedure.}
      {Note: this .tabvisible must come after the active page for MID sub page is set above.
       Not sure why, but it seems to make a difference.}
      WarTabSheet.TabVisible := false;

      {Also, I double set the visible tab to general in order to make sure it doesn't
       display the internals of the war (or some other) tab, which it sometimes wants to do!}
      VariableTabsControl.activePage := AlliancesSheet;
      VariableTabsControl.activePage := GeneralSheet;

      Scaled:=true;

      {User supplied data sets -- probably need to move this, do more with dyads/monads etc.}
      {if configuration.User_data_set_info.get_num_data_sets > 0 then
         begin
            UserDataVarSelectionButton.visible := true;
            UserDataCheckBox.enabled := true;
         end
      else
         begin
            UserDataVarSelectionButton.visible := false;
            UserDataCheckBox.enabled := false;
         end;   }

       {Specific numbers for year ranges}

       NationYear1label.caption := inttostr(configuration.first_nation_year);
       NationYear2label.caption := inttostr(configuration.last_nation_year);
       AllYrFrom.caption := inttostr(configuration.first_alliance_year);
       AllYrTo.caption := inttostr(configuration.last_alliance_year);
       CapYrFrom.caption := inttostr(configuration.first_cap_year);
       CapYrTo.caption := inttostr(configuration.last_cap_year);
       RiskYrFrom.caption := inttostr(configuration.first_risk_year);
       RiskYrTo.caption := inttostr(configuration.last_risk_year);
       EUYrFrom.caption := inttostr(configuration.first_eu_year_possible);
       EUYrTo.caption := inttostr(configuration.last_eu_year_possible);
       MIDLabelFrom.caption := inttostr(configuration.first_mid_year);
       MIDLabelTo.caption := inttostr(configuration.last_mid_year);
       polityyear1label.caption := inttostr(configuration.first_polity3_year);
       polityyear2label.caption := inttostr(configuration.last_polity3_year);
       edit1.text := inttostr (user_selections.first_year);
       edit2.text := inttostr (user_selections.last_year);


        {first, hide or show the choice options for monadic/dyadic}
     if (user_selections.output_this = output_monads) then
        begin
           {set monadic_data display.  Change captions, and visibility}
           {selection area options}
           self.caption := 'Country Year Output Options Available';
           SettingsForm.countrydyadlb.caption:='Country Dyads';
           CaseSelectionLabel.Caption := 'Country-Years Included';
           alldyadsfromallstates.caption := 'All Countries';
           specific_dyads.caption := 'Specific Set of Countries';
           allgpvsgpdyads.caption := 'All Major Powers';
           CaseSelectionPanel.visible := true;
           alldyadsfromallstates.visible := true;
           allgpvsgpdyads.visible := true;
           specific_dyads.visible := true;
           DyadsFromFileButton.visible := false;
           ChangeInputFileButton.visible := false;
           userdyadfiledisplay.visible := false;
           allcontg.visible := false;
           allgpvsany.visible := false;
           PolRelevant.visible := false;
           MaximumDistanceButton.visible := false;
           DistanceEditBox.visible := false;
           SpecifiedRegionButton.Visible := true;
           SelectRegionButton.visible := true;
           SetCountriesButton.visible := true;
           ChangeContiguityButton1.visible := false;
           ChangeContiguityButton2.visible := false;

           {Print of dyads i vs. i}
           SelfReferencePanel.visible := false;

           {Dispute tab sheet is not visible, set below}

           {variable area options.  Natcap, ccode, year, risk, polity always visible.}
           DistanceVarBox.visible := false;
           DistanceOptionsButton.visible := false;
           taubox.visible := false;
           WarTrapPanel.visible := false;
           EUReasonTaubox.visible := false;
           EUReasonSUnbox.visible := false;
           EUReasonSWtbox.visible := false;
           EQTaubox.visible := false;
           EQSUnBox.visible := false;
           EQSWtBox.visible := false;
           EquilibriumButton.visible := false;
           MarkPolRelevantBox.visible := false;
           ContiguityCheckBox.visible := false;
           ColonialContiguityCheckBox.visible := false;
           ExistTimeCheckBox.visible := false;
           AllianceCheckBox.visible := false;
   {        AllianceOptionsButton.visible := false;   }
           AlliancePortfolioTableUnWeightedCheckBox.visible := false;
           AlliancePortfolioTableWeightedCheckBox.visible := false;
           ConflictTabSheet.tabvisible := false;
           COWMIDTabSheet.tabvisible := false;
           MaozMIDTabSheet.tabvisible := false;
           ICBTabSheet.TabVisible := false;
           WarTabSheet.tabvisible := false;
           CcodeVarBox.caption := 'CCode';
           AbbrevCheckBox.caption := 'Abbreviation';
           tauwithleaderbox.visible := true;
           swithleaderbox.visible := true;
           TauWithLeaderButton.visible := true;
           SWithLeaderButton.visible := true;
           sboxunweighted.visible := false;
           sboxweighted.visible := false;
           RelRegionCheckBox.visible := false;
           HomeRegionCheckBox.visible := true;
           COWPeaceYrsBox.visible := false;
           MaozPeaceYrsBox.visible := false;
           ICBPeaceYrsBox.visible := false;
           ChangePolRelButton.visible := false;
           ISO_code_CheckBox.visible := true;
           ISO_abb2_CheckBox.visible := true;
           ISO_abb3_CheckBox.Visible := true;
           ISO_short_CheckBox.visible := true;
           ISO_full_CheckBox.visible := true;


           {User Variables - make user data set tab visible if at least one var of proper type.}
           {variable_unit_of_analysis_type = (no_unit_variable, identifierccode1, identifierccode2,
           identifieryear, identifierversion, monadic, dyadic_ordered, dyadic_unordered, annual);  }
           for tabsheet := (configuration.User_data_set_info.get_num_data_sets - 1) downto 0 do
              begin
                 make_visible := false;
                 varcounter := configuration.User_data_set_info.get_data_set_num_vars(tabsheet);
                 for varloop := 0 to (varcounter - 1) do  {check to see if we have a unit appropriate for country-year display.}
                    if ((configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) = monadic) or
                        (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) = annual))
                    then make_visible := true;
                 UserDataTabControl.Pages[tabsheet].TabVisible := make_visible;
              end;

           {Exclusion sheet information}
           ExclusionSheet.enabled := false;
           ExclusionSheet.tabvisible := false;

           {Output Sheets information}
           DisputeInitiatorSheet.enabled := false;
           DisputeInitiatorSheet.tabvisible := false;
           SamplingSheet.enabled := false;
           SamplingSheet.tabvisible := false;

        end
      else if ((user_selections.output_this = output_directed_dispute_initiation_dyads) or (user_selections.output_this = output_nondirected_dispute_dyads)) then
        begin
            {since it is implied by this choice, mark MIDs as included as output vars}
           if not ((COW_disputes in user_selections.output_format.variables) or
                   (Maoz_dyadic_disputes in user_selections.output_format.variables) ) then
              include (user_selections.output_format.variables, COW_disputes);
           SettingsForm.ListBox1.visible:=false;

           if (user_selections.output_this = output_directed_dispute_initiation_dyads) then
              begin
                 SettingsForm.cntrylabel.caption:='Directed Dispute Initiations';
                 if user_selections.disputes_selected = all_disputes then
                    begin
                       self.caption := 'Directed Dispute Dyad Initiation Output Options Available';
                       SettingsForm.countrydyadlb.caption:='1 case per dispute initiation';
                    end
                 else if user_selections.disputes_selected = all_dispute_years then
                    begin
                       self.caption := 'Directed Dispute Dyad Initiation Year Output Options Available';
                       SettingsForm.countrydyadlb.caption:='1 case per dispute initiation year';
                    end
                 else EUGeneError ('Error in programming - labels cannot be set in nondirected dispute output in PagedOutput routine - notify programmer.', 0, continue, error_log);
              end
           else {(user_selections.output_this = output_nondirected_dispute_dyads) }
              begin
                 SettingsForm.cntrylabel.caption:='NonDirected Disputes';
                 if user_selections.disputes_selected = all_disputes then
                    begin
                       self.caption := 'Nondirected Dispute Dyad Output Options Available';
                       SettingsForm.countrydyadlb.caption:='1 case per dispute';
                    end
                 else if user_selections.disputes_selected = all_dispute_years then
                    begin
                       self.caption := 'Nondirected Dispute Dyad Year Output Options Available';
                       SettingsForm.countrydyadlb.caption:='1 case per dispute year';
                    end
                 else EUGeneError ('Error in programming - labels cannot be set in nondirected dispute output in PagedOutput routine - notify programmer.', 0, continue, error_log);
              end;

           CaseSelectionLabel.Caption := 'Dispute-Years Included';
           CaseSelectionPanel.visible := false;
           alldyadsfromallstates.visible := false;
           allgpvsgpdyads.visible := false;
           specific_dyads.visible := false;
           DyadsFromFileButton.visible := false;
           ChangeInputFileButton.visible := false;
           userdyadfiledisplay.visible := false;
           allcontg.visible := false;
           allgpvsany.visible := false;
           PolRelevant.visible := false;
           MaximumDistanceButton.visible := false;
           DistanceEditBox.visible := false;
           SpecifiedRegionButton.visible := false;
           SelectRegionButton.visible := false;
           SetCountriesButton.visible := false;
           ChangeContiguityButton1.visible := false;
           ChangeContiguityButton2.visible := false;

           {Print of dyads i vs. i;  }
           SelfReferencePanel.visible := false;

           {Dispute tab is visible, set below}
           {for outputting dispute dyads, make it clear to the user that only joiners on the
            initiating side will be coded to create directed dispute dyads.}
           {The labelling is a bit different for directed and non-directed dispute dyads,
            but settings are the same}
           if (user_selections.output_this = output_directed_dispute_initiation_dyads) then
              begin
                 DisputeInitiatorSheet.caption := 'Initiator/Multiple MID Settings';
                 InitiatorTimingLabel.caption := 'Initiator Coding : Timing';
                 InitiatorTimingPanel.visible := true;
                 TrueInitiatorsCheckBox.Caption := 'Code Only Originators (Involved Day 1) as Initiators';
                 originatorinitfinishlabel.caption := '(originators only are included as targets)';
                 TrueInitiatorsCheckBox.visible := true;
                 originatorinitfinishlabel.visible := true;
                 JoinerInitCheckBox.caption := 'Code Initiating-Side Joiners as Initiators, and Target-Side Joiners as Targets';
                 joinerinitfinishlabel.caption := '(note: mark "Include All Joiner Dyads" to include these cases in output)';
                 JoinerInitCheckBox.visible := true;
                 JoinerInitFinishLabel.visible := true;
                 {Removed the following option from possible consideration in v3.12, Jan 2007.
                  The option allows something that just doesn't make sense for this unit of analysis.}
                 JoinerAnyCheckBox.caption := 'Code Originators and Joiners (regardless of side) as Initiators';
                 JoinerAnyFinishLabel.caption := '(note: mark "Include All Joiner Dyads" to include these cases in output)';
                 JoinerAnyCheckBox.visible := false;
                 JoinerAnyFinishLabel.visible := false;
                 InitiatorIdentityPanel.visible := true;
                 SideAasInitiatorCheckBox.visible := true;
                 InitiationAsRevisionistRadioButton.visible := true;
                 MultiMidYearPanel.visible := false;
              end
           else {(user_selections.output_this = output_nondirected_dispute_dyads) }
              begin
                 DisputeInitiatorSheet.Caption := 'Originator/Joiner Settings';
                 InitiatorTimingLabel.caption := 'Create Dyads for Joiners?';
                 InitiatorTimingPanel.visible := true;
                 TrueInitiatorsCheckBox.caption := 'Create Dyads only for Originators';
                 originatorinitfinishlabel.caption := 'No label here';
                 TrueInitiatorsCheckBox.visible := true;
                 OriginatorInitFinishLabel.Visible := false;
                 JoinerInitCheckBox.caption := 'No label here';
                 joinerinitfinishlabel.caption := 'No label here';
                 JoinerInitCheckBox.visible := false;
                 JoinerInitFinishLabel.visible := false;
                 JoinerAnyCheckBox.caption := 'Create Dyads for Originators and Joiners';
                 JoinerAnyFinishLabel.caption := '(note: mark "Include All Joiner Dyads" to include these cases in output)';
                 JoinerAnyCheckBox.visible := true;
                 JoinerAnyFinishLabel.visible := true;
                 InitiatorIdentityPanel.visible := false;
                 MultiMidYearPanel.visible := false;
              end;

           If (user_selections.output_this = output_nondirected_dispute_dyads  ) then
           begin
            end;

           {variable area}
           DistanceVarBox.visible := true;
           DistanceOptionsButton.visible := true;
           taubox.visible := true;
           WarTrapPanel.visible := true;
           EUReasonTaubox.visible := true;
           EUReasonSUnbox.visible := true;
           EUReasonSWtbox.visible := true;
           EQTaubox.visible := true;
           EQSUnBox.visible := true;
           EQSWtBox.visible := true;
           EquilibriumButton.visible := true;
           {In this version, this is always invisible b/c unnecessary}
           {eu_options_button.visible := true;}
           MarkPolRelevantBox.visible := true;
           ContiguityCheckBox.visible := true;
           ColonialContiguityCheckBox.visible := true;
           ExistTimeCheckBox.visible := true;
           AllianceCheckBox.visible := true;
   {        AllianceOptionsButton.visible := true;   }
           AlliancePortfolioTableUnWeightedCheckBox.visible := true;
           AlliancePortfolioTableWeightedCheckBox.visible := true;
           ConflictTabSheet.tabvisible := true;
           COWMIDTabSheet.tabvisible := true;
           MaozMIDTabSheet.tabvisible := true;
           ICBTabSheet.TabVisible := false;
           WarTabSheet.tabvisible := false;
           CcodeVarBox.caption := 'CCodes';
           AbbrevCheckBox.caption := 'Abbreviations';
           tauwithleaderbox.visible := true;
           swithleaderbox.visible := true;
           TauWithLeaderButton.visible := true;
           SWithLeaderButton.visible := true;
           sboxunweighted.visible := true;
           sboxweighted.visible := true;
           sboxunweighted.enabled := true;
           sboxweighted.enabled := true;
           RelRegionCheckBox.visible := true;
           HomeRegionCheckBox.visible := true;
           COWPeaceYrsBox.visible := true;
           MaozPeaceYrsBox.visible := true;
           ICBPeaceYrsBox.visible := true;
           ChangePolRelButton.visible := true;
           COWMIDInitiationCheckBox.caption := 'Dispute Initiation';
           MaozMIDInitiationCheckBox.caption := 'Dispute Initiation';
           ISO_code_CheckBox.visible := true;
           ISO_abb2_CheckBox.visible := true;
           ISO_abb3_CheckBox.Visible := true;
           ISO_short_CheckBox.visible := true;
           ISO_full_CheckBox.visible := true;

           {User Variables - make user data set tab visible if at least one var of proper type.}
           for tabsheet := (configuration.User_data_set_info.get_num_data_sets - 1) downto 0 do
              begin
                 make_visible := false;
                 varcounter := configuration.User_data_set_info.get_data_set_num_vars(tabsheet);
                 for varloop := 0 to (varcounter - 1) do  {check to see if we have a unit appropriate for country-year display.}
                    if ((configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) = monadic) or
                        (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) = annual) or
                        (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) = dyadic_ordered) or
                        (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) = dyadic_unordered)
                        )
                    then make_visible := true;
                UserDataTabControl.Pages[tabsheet].TabVisible := make_visible;
              end;

           {Exclusion sheet information}
           ExclusionSheet.enabled := true;
           ExclusionSheet.tabvisible := true;
           {Initially mark the conflict exclusion box as COW if not set.}
           if not ( (user_selections.conflict_exclusion_selection = cds_COWMID) or (user_selections.conflict_exclusion_selection = cds_MaozMID) ) then
              user_selections.conflict_exclusion_selection := cds_COWMID;
           {Set exclusion labels, which say something different if dispute init dyads}
           ExclusionSheet.Caption := 'Base Data Set/Conflict Exclusions';
           ExcludeDyadYearLabel.caption := 'Base Dispute Initiation and Exclusion on Which Data Set?';
           ExcludeConflictICBButton.Enabled := false;
           ExcludeConflictNoneButton.Enabled := false;
           ExcludeConflictWarsButton.Enabled := false;

           MarkSubsequentAsInitiationCheckBox.caption := 'Treat Ongoing Dispute Years as New Disputes';
           user_selections.dispute_info.AlwaysIncludeTgtVsInitiator := false;
           user_selections.dispute_info.IncludeTgtVsInitiatoriffNew := false;
           user_selections.output_format.printAllOngoing := true;
           user_selections.output_format.printOngoingifNewDisp := false;
           OngoingMIDPanel.visible := false;
           TargetInitiatorPanel.Visible := false;
           {AlwaysIncludeTgtVsInitDyadsRadioButton.visible := true;
           IncludeTgtVsInitDyadsiffNewRadioButton.visible := true;}
           JoinerInclusionPanel.visible := true;
           TargetDispYearNoteLabel.visible := true;

           {output sheets information}
           DisputeInitiatorSheet.enabled := true;
           DisputeInitiatorSheet.tabvisible := true;
           SamplingSheet.enabled := false;
           SamplingSheet.tabvisible := false;

        end
      else if (user_selections.output_this = output_directed_dyads) or
              (user_selections.output_this = output_none) then
        begin
           self.caption := 'Directed Dyad Year Output Options Available';
           SettingsForm.countrydyadlb.caption:='Directed Dyad Years';
           CaseSelectionLabel.Caption := 'Dyad-Years Included';
           alldyadsfromallstates.caption := 'All Dyads from All Countries';
           Allgpvsgpdyads.caption := 'All Major Power vs. Major Power Dyads';
           specific_dyads.caption := 'Specific Set of Dyads';
           allcontg.caption := 'All Contiguous Dyads';
           allgpvsany.caption := 'All Major Power vs Any State Dyads';
           PolRelevant.caption := 'Politically Relevant Dyads';
           CaseSelectionPanel.visible := true;
           alldyadsfromallstates.visible := true;
           allgpvsgpdyads.visible := true;
           specific_dyads.visible := true;
           DyadsFromFileButton.visible := true;
           ChangeInputFileButton.visible := true;
           userdyadfiledisplay.visible := true;
           allcontg.visible := true;
           MaximumDistanceButton.Visible := true;
           DistanceEditBox.visible := true;
           SpecifiedRegionButton.visible := true;
           SelectRegionButton.visible := true;
           PolRelevant.visible := true;
           allgpvsany.visible := true;
           SetCountriesButton.visible := true;
           ChangeContiguityButton1.visible := true;
           ChangeContiguityButton2.visible := true;

           {Print of dyads i vs. i }
           SelfReferencePanel.visible := true;

           {Dispute tab is visible, set below}
           {for outputting dispute dyads, joiners include both joiners on the initiating and
            the target side for purposes of creating initiation.}
           TrueInitiatorsCheckBox.Caption := 'Code Only "Originators" (Involved Day 1) as Initiators';
           originatorinitfinishlabel.caption := '(only dyads with originators as targets included in output)';
           TrueInitiatorsCheckBox.visible := true;
           originatorinitfinishlabel.visible := true;
           JoinerInitCheckBox.caption := 'Include Joiners on Initiating Side as Initiators, and Joiners on Target side as Targets';
           joinerinitfinishlabel.caption := '(note: mark "Include All Joiner Dyads" to include these cases in output)';
           JoinerInitCheckBox.visible := true;
           JoinerInitFinishLabel.visible := true;
           JoinerAnyCheckBox.caption := 'Code Originators and Joiners (regardless of side) as Initiators';
           JoinerAnyFinishLabel.caption := '(note: mark "Include All Joiner Dyads" to include these cases in output)';
           JoinerAnyCheckBox.visible := true;
           JoinerAnyFinishLabel.visible := true;
           InitiatorTimingPanel.visible := true;
           InitiatorIdentityPanel.visible := true;
           SideAasInitiatorCheckBox.visible := true;
           InitiationAsRevisionistRadioButton.visible := true;
           InitiatorTimingLabel.caption := 'Initiator Coding : Timing';
           DisputeInitiatorSheet.caption := 'Initiator/Multiple Mid Settings';
           MultiMidYearPanel.visible := true;
           {variable area}
           DistanceVarBox.visible := true;
           DistanceOptionsButton.visible := true;
           taubox.visible := true;
           WarTrapPanel.visible := true;
           EUReasonTaubox.visible := true;
           EUReasonSUnbox.visible := true;
           EUReasonSWtbox.visible := true;
           EQTaubox.visible := true;
           EQSUnBox.visible := true;
           EQSWtBox.visible := true;
           EquilibriumButton.visible := true;
           {In this version, this is always invisible b/c unnecessary}
           {eu_options_button.visible := true;}
           MarkPolRelevantBox.visible := true;
           ContiguityCheckBox.visible := true;
           ColonialContiguityCheckBox.visible := true;
           ExistTimeCheckBox.visible := true;
           AllianceCheckBox.visible := true;
   {        AllianceOptionsButton.visible := true;   }
           AlliancePortfolioTableUnWeightedCheckBox.visible := true;
           AlliancePortfolioTableWeightedCheckBox.visible := true;
           ConflictTabSheet.tabvisible := true;
           COWMIDTabSheet.tabvisible := true;
           MaozMIDTabSheet.tabvisible := true;
           ICBTabSheet.TabVisible := true;
           WarTabSheet.tabvisible := false;
           CcodeVarBox.caption := 'CCodes';
           AbbrevCheckBox.caption := 'Abbreviations';
           tauwithleaderbox.visible := true;
           swithleaderbox.visible := true;
           TauWithLeaderButton.visible := true;
           SWithLeaderButton.visible := true;
           sboxunweighted.visible := true;
           sboxweighted.visible := true;
           sboxunweighted.enabled := true;
           sboxweighted.enabled := true;
           RelRegionCheckBox.visible := true;
           HomeRegionCheckBox.visible := true;
           COWPeaceYrsBox.visible := true;
           MaozPeaceYrsBox.visible := true;
           ICBPeaceYrsBox.visible := true;
           ChangePolRelButton.visible := true;
           COWMIDInitiationCheckBox.caption := 'Dispute Initiation';
           MaozMIDInitiationCheckBox.caption := 'Dispute Initiation';
           ISO_code_CheckBox.visible := true;
           ISO_abb2_CheckBox.visible := true;
           ISO_abb3_CheckBox.Visible := true;
           ISO_short_CheckBox.visible := true;
           ISO_full_CheckBox.visible := true;

           {User Variables - make user data set tab visible if at least one var of proper type.}
           for tabsheet := (configuration.User_data_set_info.get_num_data_sets - 1) downto 0 do
              begin
                 make_visible := false;
                 varcounter := configuration.User_data_set_info.get_data_set_num_vars(tabsheet);
                 for varloop := 0 to (varcounter - 1) do  {check to see if we have a unit appropriate for country-year display.}
                    if ((configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) = monadic) or
                        (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) = annual) or
                        (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) = dyadic_ordered) or
                        (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) = dyadic_unordered)
                        )
                    then make_visible := true;
                UserDataTabControl.Pages[tabsheet].TabVisible := make_visible;
              end;

           {Exclusion sheet information}
           {Set exclusion labels, which say something different if dispute init dyads}
           ExclusionSheet.Caption := 'Case/Conflict Exclusions';
           ExcludeDyadYearLabel.caption := 'Exclude Conflict Dyad-Years Based on Which Data Set?';
           ExcludeConflictICBButton.Enabled := true;
           ExcludeConflictNoneButton.Enabled := true;
           {ExcludeConflictWarsButton.Enabled := true;}
           ExclusionSheet.enabled := true;
           ExclusionSheet.tabvisible := true;
           MarkSubsequentAsInitiationCheckBox.caption := 'Treat Ongoing Dispute Years as New Initiations';
           OngoingMIDPanel.visible := true;
           TargetInitiatorPanel.Visible := true;
           {AlwaysIncludeTgtVsInitDyadsRadioButton.visible := true;
           IncludeTgtVsInitDyadsiffNewRadioButton.visible := true;}
           JoinerInclusionPanel.visible := true;
           TargetDispYearNoteLabel.visible := true;

           {output sheet information}
           DisputeInitiatorSheet.enabled := true;
           DisputeInitiatorSheet.tabvisible := true;
           SamplingSheet.Enabled := true;
           SamplingSheet.tabvisible := true;

        end
      else if (user_selections.output_this = output_nondirected_dyads) then
        begin
           self.caption := 'Non-Directed Dyad Year Output Options Available';
           SettingsForm.countrydyadlb.caption:='Non-Directed Dyad Years';
           CaseSelectionLabel.Caption := 'Dyad-Years Included';
           alldyadsfromallstates.caption := 'All Dyads from All Countries';
           Allgpvsgpdyads.caption := 'All Major Power vs. Major Power Dyads';
           specific_dyads.caption := 'Specific Set of Dyads';
           allcontg.caption := 'All Contiguous Dyads';
           allgpvsany.caption := 'All Major Power vs Any State Dyads';
           PolRelevant.caption := 'Politically Relevant Dyads';
           CaseSelectionPanel.visible := true;
           alldyadsfromallstates.visible := true;
           allgpvsgpdyads.visible := true;
           specific_dyads.visible := true;
           DyadsFromFileButton.visible := true;
           ChangeInputFileButton.visible := true;
           userdyadfiledisplay.visible := true;
           allcontg.visible := true;
           MaximumDistanceButton.visible := true;
           DistanceEditBox.Visible := true;
           SpecifiedRegionButton.visible := true;
           SelectRegionButton.visible := true;
           PolRelevant.visible := true;
           allgpvsany.visible := true;
           SetCountriesButton.visible := true;
           ChangeContiguityButton1.visible := true;
           ChangeContiguityButton2.visible := true;

           {Print of dyads i vs. i }
           SelfReferencePanel.visible := true;

           {Dispute tab is visible, set below}
           {for outputting dispute dyads, joiners include both joiners on the initiating and
            the target side for purposes of creating initiation.}
           TrueInitiatorsCheckBox.Caption := 'Mark dispute/crisis onset ONLY for originators (involved day 1)';
           TrueInitiatorsCheckBox.visible := true;
           originatorinitfinishlabel.visible := false;
           JoinerInitCheckBox.visible := false;
           JoinerInitFinishLabel.visible := false;
           JoinerAnyCheckBox.caption := 'Mark dispute/crisis onset for originators and joiners';
           JoinerAnyCheckBox.visible := true;
           JoinerAnyFinishLabel.visible := false;
           InitiatorTimingLabel.caption := 'Initiator Coding : Timing';
           InitiatorTimingPanel.visible := true;
           MultiMidYearPanel.visible := true;
           InitiatorIdentityPanel.visible := false;
           SideAasInitiatorCheckBox.visible := false;
           InitiationAsRevisionistRadioButton.visible := false;
           DisputeInitiatorSheet.caption := 'Initiator/Multiple Mid Settings';
           {variable area}
           DistanceVarBox.visible := true;
           DistanceOptionsButton.visible := true;
           taubox.visible := true;
           WarTrapPanel.visible := true;
           EUReasonTaubox.visible := true;
           EUReasonSUnbox.visible := true;
           EUReasonSWtbox.visible := true;
           EQTaubox.visible := true;
           EQSUnBox.visible := true;
           EQSWtBox.visible := true;
           EquilibriumButton.visible := true;
           {In this version, this is always invisible b/c unnecessary}
           {eu_options_button.visible := true;}
           MarkPolRelevantBox.visible := true;
           ContiguityCheckBox.visible := true;
           ColonialContiguityCheckBox.visible := true;
           ExistTimeCheckBox.visible := true;
           AllianceCheckBox.visible := true;
   {        AllianceOptionsButton.visible := true;  }
           AlliancePortfolioTableUnWeightedCheckBox.visible := true;
           AlliancePortfolioTableWeightedCheckBox.visible := true;
           ConflictTabSheet.tabvisible := true;
           COWMIDTabSheet.tabvisible := true;
           MaozMIDTabSheet.tabvisible := true;
           ICBTabSheet.TabVisible := true;
           WarTabSheet.tabvisible := false;
           CcodeVarBox.caption := 'CCodes';
           AbbrevCheckBox.caption := 'Abbreviation';
           tauwithleaderbox.visible := true;
           swithleaderbox.visible := true;
           TauWithLeaderButton.visible := true;
           SWithLeaderButton.visible := true;
           sboxunweighted.visible := true;
           sboxweighted.visible := true;
           sboxunweighted.enabled := true;
           sboxweighted.enabled := true;
           RelRegionCheckBox.visible := true;
           HomeRegionCheckBox.visible := true;
           COWPeaceYrsBox.visible := true;
           MaozPeaceYrsBox.visible := true;
           ICBPeaceYrsBox.visible := true;
           ChangePolRelButton.visible := true;
           COWMIDInitiationCheckBox.caption := 'Dispute Onset';
           MaozMIDInitiationCheckBox.caption := 'Dispute Onset';
           ISO_code_CheckBox.visible := true;
           ISO_abb2_CheckBox.visible := true;
           ISO_abb3_CheckBox.Visible := true;
           ISO_short_CheckBox.visible := true;
           ISO_full_CheckBox.visible := true;

           {User Variables - make user data set tab visible if at least one var of proper type.}
           for tabsheet := (configuration.User_data_set_info.get_num_data_sets - 1) downto 0 do
              begin
                 make_visible := false;
                 varcounter := configuration.User_data_set_info.get_data_set_num_vars(tabsheet);
                 for varloop := 0 to (varcounter - 1) do  {check to see if we have a unit appropriate for country-year display.}
                    if ((configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) = monadic) or
                        (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) = annual) or
                        (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) = dyadic_ordered) or
                        (configuration.User_data_set_info.get_data_set_var_unit(tabsheet, varloop) = dyadic_unordered)
                        )
                    then make_visible := true;
                UserDataTabControl.Pages[tabsheet].TabVisible := make_visible;
              end;

           {Exclusion sheet information}
           {Set exclusion labels, which say something different if dispute init dyads}
           ExclusionSheet.Caption := 'Case/Conflict Exclusions';
           ExcludeDyadYearLabel.caption := 'Exclude Conflict Dyad-Years Based on Which Data Set?';
           ExcludeConflictICBButton.Enabled := true;
           ExcludeConflictNoneButton.Enabled := true;
           {ExcludeConflictWarsButton.Enabled := true;}
           ExclusionSheet.enabled := true;
           ExclusionSheet.tabvisible := true;
           MarkSubsequentAsInitiationCheckBox.caption := 'Treat Ongoing Dispute Years as New Disputes';
           OngoingMIDPanel.visible := true;
           TargetInitiatorPanel.Visible := false;
           {AlwaysIncludeTgtVsInitDyadsRadioButton.visible := false;
           IncludeTgtVsInitDyadsiffNewRadioButton.visible := false;}
           JoinerInclusionPanel.visible := true;
           TargetDispYearNoteLabel.visible := false;

           {output sheet information}
           DisputeInitiatorSheet.enabled := true;
           DisputeInitiatorSheet.tabvisible := true;
           SamplingSheet.Enabled := true;
           SamplingSheet.tabvisible := true;

           {Also need to check one setting that cannot occur with non-directed dyads}
           if user_selections.dispute_info.JoinersOnInitiatingSideAsInitiators = true then
              {this cannot be, so change the check box, which will then change the setting
               when the OK button is hit}
              JoinerInitCheckBox.checked := true;

        end          {if output nondirected}
      else
        begin
           EUGeneError ('Program reached output box OnShow without valid output selection',
                  5, stop, error_log);
        end;

      {Now set the displayed marks in the check boxes and radio dials}
      {output options}
      ToScreenRadioDial.checked := (user_selections.output_format.location = toscreen);
      ToPrinterRadioDial.checked := (user_selections.output_format.location = toprinter);
      ToFileRadioDial.checked := (user_selections.output_format.location = tofile);
      FileDisplay.text := user_selections.output_format.output_file_name;

      {separator}
      SeparateWithTabs.checked := (user_selections.output_format.separator = tab);
      SeparateWithSpaces.checked := (user_selections.output_format.separator = space);
      SeparateWithCommas.checked := (user_selections.output_format.separator = comma);

      SPSSCommandFileCheckBox.checked := SPSS in user_selections.output_format.CommandFiles;
      limdepCommandFileCheckBox.checked := limdep in user_selections.output_format.CommandFiles;
      STataCommandFileCheckBox.checked := stata in user_selections.output_format.CommandFiles;
      commandfileOnlyCheckBox.checked := commandfilesonly in user_selections.output_format.CommandFiles;

      {dyad/monad selection.  And initialize radio buttons.}
      alldyadsfromallstates.checked :=
          (((user_selections.output_this = output_directed_dyads) or (user_selections.output_this = output_nondirected_dyads))
             and (user_selections.dyads_selected = all_states)) or
          ((user_selections.output_this = output_monads) and (user_selections.monads_selected = all_states_mono));
      Allgpvsgpdyads.checked :=
          (((user_selections.output_this = output_directed_dyads) or (user_selections.output_this = output_nondirected_dyads))
             and (user_selections.dyads_selected = all_gp_gp)) or
          ((user_selections.output_this = output_monads) and (user_selections.monads_selected = all_gp_mono));
      specific_dyads.checked :=
         (((user_selections.output_this = output_directed_dyads) or (user_selections.output_this = output_nondirected_dyads))
             and (user_selections.dyads_selected = selected_set)) or
         ((user_selections.output_this = output_monads) and (user_selections.monads_selected = selected_set_mono));
      DyadsFromFileButton.checked :=
         (((user_selections.output_this = output_directed_dyads) or (user_selections.output_this = output_nondirected_dyads))
             and (user_selections.dyads_selected = user_file_read));
      SpecifiedRegionButton.checked :=
         (((user_selections.output_this = output_directed_dyads) or (user_selections.output_this = output_nondirected_dyads))
             and (user_selections.dyads_selected = within_region)) or
         ((user_selections.output_this = output_monads) and (user_selections.monads_selected = within_region_mono));

      userdyadfiledisplay.text := user_selections.user_specified_dyad_list.file_name;

      MaximumDistanceButton.checked := (((user_selections.output_this = output_directed_dyads) or (user_selections.output_this = output_nondirected_dyads)) and (user_selections.dyads_selected = within_distance));

      DistanceEditBox.text := inttostr(user_selections.maximum_distance);
      DistanceEditBox.height := 17;

      allcontg.checked := (((user_selections.output_this = output_directed_dyads) or (user_selections.output_this = output_nondirected_dyads)) and (user_selections.dyads_selected = all_contiguous));
      allgpvsany.checked := (((user_selections.output_this = output_directed_dyads) or (user_selections.output_this = output_nondirected_dyads)) and (user_selections.dyads_selected = all_gp_any));
      PolRelevant.checked := (((user_selections.output_this = output_directed_dyads) or (user_selections.output_this = output_nondirected_dyads)) and (user_selections.dyads_selected = politically_relevant));

      {years}
      All_yrs.checked := (user_selections.years=all);
      Specified_yrs.checked := (user_selections.years=subrange);

      {Printing of dyads i vs. i}
      PrintAACheckBox.checked := user_selections.output_format.printii;

      {Printing of ongoing, other dispute info.}
      AllOngoingDisputeYearButton.checked := user_selections.output_format.printAllOngoing;
      OngoingIfNewDispButton.checked := user_selections.output_format.printOngoingifNewDisp;
      DropAllOngoingButton.checked := not (user_selections.output_format.printAllOngoing) and
                                      not (user_selections.output_format.printOngoingifNewDisp);

      AlwaysIncludeTgtVsInitDyadsRadioButton.checked := (user_selections.dispute_info.AlwaysIncludeTgtVsInitiator);
      IncludeTgtVsInitDyadsiffNewRadioButton.checked := (user_selections.dispute_info.IncludeTgtVsInitiatoriffNew);

      DropJoinerRadioButton.checked := user_selections.dispute_info.DropJoinerDirectedDyads;
      IncludeJoinerRadioButton.checked := not(user_selections.dispute_info.DropJoinerDirectedDyads);

      {sampling}
      NoSampleBtn.Checked := (user_selections.sample_info.sampling=false);
      SamplingYesBtn.Checked := (user_selections.sample_info.sampling=true);
      DispSampleEdit.text := realtostring (user_selections.sample_info.proportion_dispute_dyads, 5, 3);
      NonDispSampleEdit.text := realtostring (user_selections.sample_info.proportion_non_dispute_dyads, 5, 3);
      RandSeedCheckBox.checked := user_selections.sample_info.use_randseed;
      RandomSeedEdit.text := inttostr (user_selections.sample_info.randseed);


      {header line}
      header.checked := user_selections.output_format.header;

      {Variables}
      ccodeVarBox.checked := (ccodes in user_selections.output_format.variables);
      YearVarBox.checked := (year in user_selections.output_format.variables);
      AbbrevCheckBox.checked := (abbrevs in user_selections.output_format.variables);
      ISO_code_CheckBox.checked := (ISO_code in user_selections.output_format.variables);
      ISO_abb2_CheckBox.checked := (ISO_abb2 in user_selections.output_format.variables);
      ISO_abb3_CheckBox.checked := (ISO_abb3 in user_selections.output_format.variables);
      ISO_full_CheckBox.checked := (ISO_full in user_selections.output_format.variables);
      ISO_short_CheckBox.checked := (ISO_short in user_selections.output_format.variables);
      natcapbox.checked := (SystemCapabilities in user_selections.output_format.variables);
      rawcapbox.checked := (RawCapabilities in user_selections.output_format.variables);
      MajorPowerCheckBox.checked := (PowerStatus in user_selections.output_format.variables);
      RelRegionCheckBox.checked := (RelRegion in user_selections.output_format.variables);
      HomeRegionCheckBox.checked := (HomeRegion in user_selections.output_format.variables);
      MarkPolRelevantBox.checked := (MarkPolRelevant in user_selections.output_format.variables);
      taubox.checked := (tau in user_selections.output_format.variables);
      sboxunweighted.checked := (sunweighted in user_selections.output_format.variables);
      sboxweighted.checked := (sweighted in user_selections.output_format.variables);
      tauwithleaderbox.checked := (tauwithleader in user_selections.output_format.variables);
      swithleaderbox.checked := (swithleader in user_selections.output_format.variables);

      AllianceCheckBox.checked := (Alliance in user_selections.output_format.variables);
      AlliancePortfolioTableUnWeightedCheckBox.checked := (AlliancePortfolioUnweighted in user_selections.output_format.variables);
      AlliancePortfolioTableWeightedCheckBox.checked := (AlliancePortfolioWeighted in user_selections.output_format.variables);

      democcheckbox.checked := democ in user_selections.output_format.variables;
      autoccheckbox.checked := autoc in user_selections.output_format.variables;
      xrregCheckBox.checked := xrreg in user_selections.output_format.variables;
      xrcompCheckBox.checked := xrcomp in user_selections.output_format.variables;
      xropenCheckBox.checked := xropen in user_selections.output_format.variables;
      monoCheckBox.checked := mono in user_selections.output_format.variables;
      xconstCheckBox.checked := xconst in user_selections.output_format.variables;
      parregCheckBox.checked := parreg in user_selections.output_format.variables;
      parcmpCheckBox.checked := parcomp in user_selections.output_format.variables;
      centCheckBox.checked := cent in user_selections.output_format.variables;

      RussettDem.checked := dem in user_selections.output_format.variables;
      laggeddemaut.checked := laggeddem in user_selections.output_format.variables;
      demchangebox.checked := democratization in user_selections.output_format.variables;
      StatesInSystemBox.checked := StatesInSystem in user_selections.output_format.variables;
      GPsInSystemBox.checked := GPsInSystem in user_selections.output_format.variables;
      SysConBox.checked := SysConcentration in user_selections.output_format.variables;
      SysMoveBox.checked := SysMovement in user_selections.output_format.variables;
      SysMove5Box.checked := SysMovement5Yr in user_selections.output_format.variables;
      SysMoveGPBox.checked := SysMoveGP in user_selections.output_format.variables;
      SysMoveGP5Box.checked := SysMoveGP5Yr in user_selections.output_format.variables;
      ContiguityCheckBox.checked := (contig in user_selections.output_format.variables);
      ColonialContiguityCheckBox.checked := (ColonialContig in user_selections.output_format.variables);
      DistanceVarBox.checked := (distance in user_selections.output_format.variables);
      ExistTimeCheckBox.checked := (DyadicTime in user_selections.output_format.variables);
      ICBPeaceYrsBox.checked := (ICBpeaceyrs in user_selections.output_format.variables);

      ISO_code_CheckBox.checked := (ISO_code in user_selections.output_format.variables);
      ISO_abb2_CheckBox.checked := (ISO_abb2 in user_selections.output_format.variables);
      ISO_abb3_CheckBox.checked := (ISO_abb3 in user_selections.output_format.variables);
      ISO_full_CheckBox.checked := (ISO_full in user_selections.output_format.variables);
      ISO_short_CheckBox.checked := (ISO_short in user_selections.output_format.variables);

      {In order to properly enable/disable the sub boxes, initially turn all of them on, then
       set them to whatever the actual setting is.}
      COWMIDDisputeDataCheckBox.checked := true;
      MaozDisputeDataCheckBox.checked := true;
      ICBDyadicDataCheckBox.checked := true;
      COWMIDDisputeDataCheckBox.checked := (COW_disputes in user_selections.output_format.variables);
      MaozDisputeDataCheckBox.checked := (Maoz_dyadic_disputes in user_selections.output_format.variables);
      ICBDyadicDataCheckBox.checked := (ICB_crises in user_selections.output_format.variables);

      UseMostSeriousMIDCheckBox.Checked := (user_selections.dispute_info.UseMostSeriousDispute);
      UseFirstMIDCheckBox.Checked := (user_selections.dispute_info.UseFirstDispute);

      COWMidNumCheckBox.checked := (COWMIDNumber in user_selections.output_format.variables);
      COWMidNameCheckBox.checked := (COWMIDName in user_selections.output_format.variables);
      COWstartdatecheck.checked := (COWMIDStart in user_selections.output_format.variables);
      COWenddatecheckbox.checked := (COWMIDEnd in user_selections.output_format.variables);
      COWSideACheckBox.checked := (COWMIDSideA in user_selections.output_format.variables);
      COWRevStateCheckBox.checked := (COWMIDRevisionist in user_selections.output_format.variables);
      COWRevTypeCheckBox.checked := (COWMIDRevisiontype in user_selections.output_format.variables);
      COWFatalityLevelStateCheckBox.checked := (COWMIDFatalityState in user_selections.output_format.variables);
      COWHiActCheckBox.checked := (COWMIDHiActState in user_selections.output_format.variables);
      COWOriginatorCheckBox.checked := (COWMIDOriginator in user_selections.output_format.variables);
      COWOutcomeCheckBox.checked := (COWMIDOutcome in user_selections.output_format.variables);
      COWSettlementCheckBox.checked := (COWMIDSettlement in user_selections.output_format.variables);
      COWFatalityLevelDisputeCheckBox.checked := (COWMIDFatalityDispute in user_selections.output_format.variables);
      COWHiActDisputeCheckBox.checked := (COWMIDHiActDispute in user_selections.output_format.variables);
      COWHostLevDisputeCheckBox.checked := (COWMIDHostLevDispute in user_selections.output_format.variables);
      COWRecipCheckBox.checked := (COWMIDReciprocated in user_selections.output_format.variables);
      COWNumOfStatesBox.checked := (COWMIDNumStates in user_selections.output_format.variables);
      COWMarkJoinersCheckBox.checked := (COWMarkMIDJoiners in user_selections.output_format.variables);
      COWNumMIDCheckBox.checked := (COWNumMIDs in user_selections.output_format.variables);
      COWRoleCheckBox.checked := (COWRole in user_selections.output_format.variables);
      COWPeaceYrsBox.checked := (COWpeaceyrs in user_selections.output_format.variables);
      COWPeaceDaysCheckBox.checked := (COWPeaceDays in user_selections.output_format.variables);
      COWLinkStatusCheckBox.checked := (COWLinkStatus in user_selections.output_format.variables);

      MaozMidNumCheckBox.checked := (MaozMIDNumber in user_selections.output_format.variables);
      MaozMidNameCheckBox.checked := (MaozMIDName in user_selections.output_format.variables);
      Maozstartdatecheck.checked := (MaozMIDStart in user_selections.output_format.variables);
      Maozenddatecheckbox.checked := (MaozMIDEnd in user_selections.output_format.variables);
      MaozSideACheckBox.checked := (MaozMIDSideA in user_selections.output_format.variables);
      MaozRevStateCheckBox.checked := (MaozMIDRevisionist in user_selections.output_format.variables);
      MaozRevTypeCheckBox.checked := (MaozMIDRevisiontype in user_selections.output_format.variables);
      MaozFatalityLevelStateCheckBox.checked := (MaozMIDFatalityState in user_selections.output_format.variables);
      MaozHiActCheckBox.checked := (MaozMIDHiActState in user_selections.output_format.variables);
      MaozOriginatorCheckBox.checked := (MaozMIDOriginator in user_selections.output_format.variables);
      MaozOutcomeCheckBox.checked := (MaozMIDOutcome in user_selections.output_format.variables);
      MaozSettlementCheckBox.checked := (MaozMIDSettlement in user_selections.output_format.variables);
      MaozFatalityLevelDisputeCheckBox.checked := (MaozMIDFatalityDispute in user_selections.output_format.variables);
      MaozHiActDisputeCheckBox.checked := (MaozMIDHiActDispute in user_selections.output_format.variables);
      MaozHostLevDisputeCheckBox.checked := (MaozMIDHostLevDispute in user_selections.output_format.variables);
      MaozRecipCheckBox.checked := (MaozMIDReciprocated in user_selections.output_format.variables);
      MaozNumOfStatesBox.checked := (MaozMIDNumStates in user_selections.output_format.variables);
      MaozMarkJoinersCheckBox.checked := (MaozMarkMIDJoiners in user_selections.output_format.variables);
      MaozCowWarBox.checked := (MaozCOWWar in user_selections.output_format.variables);
      MaozDurIndxBox.checked := (MaozDurindx in user_selections.output_format.variables);
      MaozDurDaysBox.checked := (MaozDurDays in user_selections.output_format.variables);
      MaozReciprocatedDyadicBox.checked := (MaozReciprocatedDyadic in user_selections.output_format.variables);
      MaozNumMIDCheckBox.checked := (MaozNumMIDs in user_selections.output_format.variables);
      MaozRoleCheckBox.checked := (MaozRole in user_selections.output_format.variables);
      MaozPeaceYrsBox.checked := (Maozpeaceyrs in user_selections.output_format.variables);
      MaozPeaceDysBox.checked := (Maozpeacedys in user_selections.output_format.variables);

      MaozLinkStatusCheckBox.checked := (MaozLinkStatus in user_selections.output_format.variables);

      ICBCrisisCheckBox.checked := (ICB_crises in user_selections.output_format.variables);
      ICBOngoingCheckBox.checked := (ICB_crises in user_selections.output_format.variables);
      ICBCrisisNumberCheckBox.checked := (ICBCrisisNumber in user_selections.output_format.variables);
      ICBCrisisNameCheckBox.checked := (ICBCrisisName in user_selections.output_format.variables);
      ICBCrisisDyadNumberCheckBox.checked := (ICBCrisisDyadNumber in user_selections.output_format.variables);
      ICBDurDaysCheckBox.checked := (ICBDurDays in user_selections.output_format.variables);
      ICBDurYearsCheckBox.checked := (ICBDurYear in user_selections.output_format.variables);
      ICBDyadicStartCheckBox.checked := (ICBDyadicStart in user_selections.output_format.variables);
      ICBDyadicEndCheckBox.checked := (ICBDyadicEnd in user_selections.output_format.variables);
      ICBOneSidedCheckBox.checked := (ICBOneSided in user_selections.output_format.variables);
      ICBStateStartCheckBox.checked := (ICBStateStart in user_selections.output_format.variables);
      ICBStateEndCheckBox.checked := (ICBStateEnd in user_selections.output_format.variables);
      ICBJoinerCheckBox.checked := (ICBJoiner in user_selections.output_format.variables);
      ICBActorSequenceCheckBox.checked := (ICBActorSequence in user_selections.output_format.variables);
      ICBCOWMemberCheckBox.checked := (ICBCOWMember in user_selections.output_format.variables);
      ICBGWMemberCheckBox.checked := (ICBGWMember in user_selections.output_format.variables);
      ICBIntraWarCheckBox.checked := (ICBIntraWar in user_selections.output_format.variables);
      ICBNumCrisisCheckBox.checked := (ICBNumCrises in user_selections.output_format.variables);
      ICBpeacedysCheckBox.checked := (ICBpeacedys in user_selections.output_format.variables);

      NewWarCheckBox.checked := (COW_wars in user_selections.output_format.variables);
      WarOngoingCheckBox.checked := (COW_wars in user_selections.output_format.variables);
      COWWarNumberCheckBox.checked := (COW_warnum in user_selections.output_format.variables);

{old code      UserDataCheckBox.checked := (UserVariables in user_selections.output_format.variables);
}
      riskEUGENETaubox.checked := ((riskTau in user_selections.output_format.variables) AND
                                (user_selections.risk_data_source = risk_EUGENE));
      riskEUGENESbox.checked := ((riskS in user_selections.output_format.variables) AND
                                (user_selections.risk_data_source = risk_EUGENE));
      riskWTRCheckbox.checked := ((riskTau in user_selections.output_format.variables) AND
                                (user_selections.risk_data_source = risk_WTR));
      RiskDetailTauCheckBox.checked := (riskdetailsTau in user_selections.output_format.variables);
      RiskDetailSCheckBox.checked := (riskdetailsS in user_selections.output_format.variables);
      EUWartrapTaubox.checked := (EUWarTrapTau in user_selections.output_format.variables);
      EUWartrapSbox.checked := (EUWarTrapS in user_selections.output_format.variables);
      EUReasonTaubox.checked := (EUWarReasonTau in user_selections.output_format.variables);
      EUReasonSUnbox.checked := (EUWarReasonSUnweighted in user_selections.output_format.variables);
      EUReasonSWtbox.checked := (EUWarReasonSweighted in user_selections.output_format.variables);
      EQTaubox.checked := (EQWarReasonTau in user_selections.output_format.variables);
      EQSUnBox.checked := (EQWarReasonSUnweighted in user_selections.output_format.variables);
      EQSWtBox.checked := (EQWarReasonSweighted in user_selections.output_format.variables);
      UncertTauCheckBox.checked := (UncertaintyTau in user_selections.output_format.variables);
      UncertSCheckBox.checked := (UncertaintyS in user_selections.output_format.variables);


           {For monadic selection, also need to specifically exclude dyadic vars from the list
            that might have been created earlier.  Do this by unchecking boxes (even if they are
            invisible.}

      if (user_selections.output_this = output_monads) then
         begin
           RelRegionCheckBox.checked := false;
           taubox.checked:= false;
           ExistTimeCheckBox.checked:= false;
           AllianceCheckBox.checked:= false;
           AlliancePortfolioTableUnWeightedCheckBox.checked:= false;
           AlliancePortfolioTableWeightedCheckBox.checked:= false;
           sboxunweighted.checked:= false;
           sboxweighted.checked:= false;
           EUWartrapTaubox.checked:= false;
           EUWartrapSbox.checked:= false;
           EUReasonTaubox.checked:= false;
           EUReasonSUnbox.checked:= false;
           EUReasonSWtbox.checked:= false;
           EQTaubox.checked:= false;
           EQSUnBox.checked:= false;
           EQSWtBox.checked:= false;
           DistanceVarBox.checked:= false;
           MarkPolRelevantBox.checked:= false;
           ContiguityCheckBox.checked:= false;
           ColonialContiguityCheckBox.checked:= false;
           COWMIDDisputeDataCheckBox.checked:= false;
           MaozDisputeDataCheckBox.checked:= false;
           COWPeaceYrsBox.checked:= false;
           MaozPeaceYrsBox.checked := false;
           ICBPeaceYrsBox.checked := false;
         end;

      {Conflict exclusions buttons, and enabling/disabling of exclusion/initiation windows}
      ExcludeConflictCOWMIDButton.checked := (user_selections.conflict_exclusion_selection = cds_COWMID);
      ExcludeConflictMaozMIDButton.checked := (user_selections.conflict_exclusion_selection = cds_MaozMID);
      ExcludeConflictICBButton.checked := (user_selections.conflict_exclusion_selection = cds_ICB);
      ExcludeConflictWarsButton.checked := (user_selections.conflict_exclusion_selection = cds_COWWar);
      ExcludeConflictNoneButton.checked := (user_selections.conflict_exclusion_selection = cds_none);

      {ongoing listing of disputes in subsequent years}
      MarkSubsequentAsInitiationCheckBox.checked := user_selections.dispute_info.MarkSubsequentAsInitiation = true;
      TrueInitiatorsCheckBox.checked := (((user_selections.output_this = output_directed_dyads) or
                                          (user_selections.output_this = output_directed_dispute_initiation_dyads) or
                                          (user_selections.output_this = output_nondirected_dispute_dyads) or
                                          (user_selections.output_this = output_nondirected_dyads) ) and
                                          (user_selections.dispute_info.OnlyTrueInitiators=true));
      JoinerInitCheckBox.checked := (((user_selections.output_this = output_directed_dyads) or
                                      (user_selections.output_this = output_directed_dispute_initiation_dyads) or
                                      (user_selections.output_this = output_nondirected_dispute_dyads) ) and
                                      (user_selections.dispute_info.JoinersOnInitiatingSideAsInitiators=true));
      JoinerAnyCheckBox.checked := (((user_selections.output_this = output_directed_dyads) or
                                     (user_selections.output_this = output_directed_dispute_initiation_dyads) or
                                     (user_selections.output_this = output_nondirected_dispute_dyads) or
                                     (user_selections.output_this = output_nondirected_dyads) ) and
                                     (user_selections.dispute_info.AllJoinersAsInitiators=true));
      SideAAsInitiatorCheckBox.checked := (((user_selections.output_this = output_directed_dyads) or
                                            (user_selections.output_this = output_directed_dispute_initiation_dyads) or
                                            (user_selections.output_this = output_nondirected_dispute_dyads) ) and
                                            (user_selections.dispute_info.SideAIsInitiator=true));
      InitiationAsRevisionistRadioButton.checked := (((user_selections.output_this = output_directed_dyads) or
                                                      (user_selections.output_this = output_nondirected_dispute_dyads) or
                                                      (user_selections.output_this = output_directed_dispute_initiation_dyads) ) and
                                                      (user_selections.dispute_info.SideAIsInitiator=false));

      {After checking disputes, turn optional tabs and panels on/off as necessary.}
      Enable_Disable_exclusions;
      Enable_Disable_initiation_settings;

      {Set some hints for display}
      ICBCrisisCheckBox.hint := 'Based on presence of new dyadic ICB crisis';
      ICBOngoingCheckBox.hint := 'Ongoing ICB crisis at beginning of year';
      ICBCrisisNumberCheckBox.hint := 'ICB Crisis number, dyadic ICB variable "CRISNO"';
      ICBCrisisDyadNumberCheckBox.hint := 'Crisis dyad number, Dyadic ICB variable "CRDYNUM"';
      ICBDurDaysCheckBox.hint := 'Dyadic ICB variable "DURDAYS"';
      ICBDurYearsCheckBox.hint := 'Dyadic ICB variable "DURYEAR"';
      ICBOneSidedCheckBox.hint := 'One sided crisis, yes/no, Dyadic ICB variable "ONESIDE"';
      ICBJoinerCheckBox.hint := 'Marks each state as originator or joiner in crisis';
      ICBActorSequenceCheckBox.hint := 'Dyadic ICB variable "ACTNUMA", "ACTNUMB"';
      ICBCOWMemberCheckBox.hint := 'Dyadic ICB variable "COWMEMA", "COWMEMB"';
      ICBGWMemberCheckBox.hint := 'Dyadic ICB variable "GWMEMA", "GWMEMB"';
      ICBIntraWarCheckBox.hint := 'Dyadic ICB variable "IWCA", "IWCB"';

      {User data var boxes}
      for varbox := 0 to high(VarFormCheckBoxes) do
         begin
            VarFormCheckBoxes[varbox].ACheckBox.checked := false;
            if (length(user_selections.user_data_sets_selections[VarFormCheckBoxes[varbox].original_user_dataset_num].data_set_variables) > 0) then
            begin
               for x := 0 to high(user_selections.user_data_sets_selections[VarFormCheckBoxes[varbox].original_user_dataset_num].data_set_variables) do
               begin
                  if (user_selections.user_data_sets_selections[VarFormCheckBoxes[varbox].original_user_dataset_num].data_set_variables[x] = VarFormCheckBoxes[varbox].original_user_var_num)
                     then VarFormCheckBoxes[varbox].ACheckBox.checked := true;
               end;
            end;
         end;

   except
      EUGeneError ('problem setting displayed options in output_options window.  Displayed options are not correct.',0,continue,error_log);
   end;    {except}

end;               {procedure outputformshow}

{---------------------------------------------------------------------}

procedure TOutput_Options.check_completeness (var user_selections: user_selection_type);
      {checks to see if all of the appropriate settings are made to allow processing.}
   begin
      user_selections.complete := true;
      if ((((user_selections.output_this = output_directed_dyads) or
           (user_selections.output_this = output_nondirected_dyads)) and
          (user_selections.dyads_selected = not_selected)) or
         ((user_selections.output_this = output_monads) and
          (user_selections.monads_selected = not_selected_mono)) or
          ((user_selections.output_this = output_nondirected_dispute_dyads) or
         (user_selections.output_this = output_directed_dispute_initiation_dyads)) and
          (user_selections.disputes_selected = not_selected_disputes)) then
         begin
            ShowMessage ('You have not selected a set of cases to output');
            user_selections.complete := false;
         end;

      {for the directed dispute initiation set, they must have a set of disputes selected.}
      if ( (user_selections.output_this = output_directed_dispute_initiation_dyads) or (user_selections.output_this = output_nondirected_dispute_dyads)) and
         not ((COW_disputes in user_selections.output_format.variables) or
              (Maoz_dyadic_disputes in user_selections.output_format.variables) or
              (ICB_crises in user_selections.output_format.variables)) then
         begin
            ShowMessage ('You have specified dispute dyads, but have not specified a set of disputes to output under the "Variables" tab');
            user_selections.complete := false;
         end;

      {Also need to check for the dispute initiation set that the exclusion setting matches
       the variable selected.}
      if ((user_selections.output_this = output_directed_dispute_initiation_dyads) or (user_selections.output_this = output_nondirected_dispute_dyads)) then
         begin
            {do they match?}
            if not ( ((COW_disputes in user_selections.output_format.variables) and
                      (user_selections.conflict_exclusion_selection = cds_COWMID)) or
                     ((Maoz_dyadic_disputes in user_selections.output_format.variables) and
                      (user_selections.conflict_exclusion_selection = cds_MaozMID) ) ) then
            if (COW_disputes in user_selections.output_format.variables) and
               (not ((user_selections.conflict_exclusion_selection = cds_COWMID) )) then
               begin
                  user_selections.conflict_exclusion_selection := cds_COWMID;
                  ExcludeConflictCOWMIDButton.checked := true;
                  ShowMessage ('You are outputting disputes only, and have selected COW disputes, but your exclusion criteria was based on a different set.  The exclusion criteria have been set to match the COW dispute set.');
               end
            else
            if (Maoz_dyadic_disputes in user_selections.output_format.variables) and
               (not ((user_selections.conflict_exclusion_selection = cds_MaozMID) )) then
               begin
                  user_selections.conflict_exclusion_selection := cds_MaozMID;
                  ExcludeConflictMaozMIDButton.checked := true;
                  ShowMessage ('You are outputting disputes only, and have selected Maoz dyadic disputes, but your exclusion criteria was based on a different set.  The exclusion criteria have been set to match the Maoz dispute set.');
               end;
         end;

      {Must be sure if they chose MIDs that OK choices are made on inclusions/exclusions.}
      if (COW_disputes in user_selections.output_format.variables) or
         (Maoz_dyadic_disputes in user_selections.output_format.variables) then
         begin
          if not (user_selections.dispute_info.OnlyTrueInitiators or
                  user_selections.dispute_info.JoinersOnInitiatingSideAsInitiators or
                  user_selections.dispute_info.AllJoinersAsInitiators) then
            begin
               ShowMessage ('You have not selected what states will be coded as initiators.  Check the "Dispute Initiators" tab.');
               user_selections.complete := false;
            end;
         end;

      if (user_selections.output_format.variables = []) then
         begin
            ShowMessage ('You have not selected a set of variables to output');
            user_selections.complete := false;
         end;
      if (user_selections.years = noyears) then
         begin
            ShowMessage ('You have not selected a set of years for output');
            user_selections.complete := false;
         end;
      if (user_selections.output_format.output_set = false) then
         begin
            ShowMessage ('You have not selected a proper destination (file, printer, or screen) for output');
            user_selections.complete := false;
         end;
      if (commandfilesonly in user_selections.output_format.CommandFiles) and (not
         ((SPSS in user_selections.output_format.CommandFiles)  or
          (limdep in user_selections.output_format.CommandFiles)  or
          (stata in user_selections.output_format.CommandFiles))) then
         begin
            ShowMessage ('You have not selected a type of command file to write, but specified that only a command file should be written');
            user_selections.complete := false;
         end;

      {if they chose sampling, then be sure they also selected some kind of MIDs}
      if user_selections.sample_info.sampling then
        if not ((COW_disputes in user_selections.output_format.variables) or
         (Maoz_dyadic_disputes in user_selections.output_format.variables)) then
            begin
               ShowMessage ('You have specified sampling on MIDs, but have not selected MIDs in the variables.  Select a type of MIDs under the "Variables | MIDs" tab.');
               user_selections.complete := false;
            end;

   end;

{ ------------------------------------------------------------ }

procedure TOutput_Options.OkbtnClick(Sender: TObject);
var varbox, x, spot_to_add: integer;
    tempmessage : string;

   procedure include_exclude (avar : output_variable_type; var varlist : output_variable_set;
             thecheckbox : TCheckBox; var thelabel:Tlabel);
      begin
         if thecheckbox.checked then
            begin
               include (varlist, avar);
               thelabel.visible := true;
            end
         else
            begin
               exclude (varlist, avar);
               thelabel.visible := false;
            end;
      end;

procedure Set_Year_Range();
   var start_year_ok, end_year_ok : boolean;
       temp_year1, temp_year2, third : integer;
begin
   start_year_ok := false;
   end_year_ok := false;

   temp_year1 := strtointdef (edit1.text, 0);
   temp_year2 := strtointdef (edit2.text, 0);

   if temp_year1 = 0 then
     begin   {problem with entry}
        ShowMessage ('First year value was not a valid integer.  Please reenter.');
        Edit1.clear;
     end
   else if not ( (temp_year1 >= configuration.first_nation_year) and
                 (temp_year1 <= configuration.last_nation_year)) then
      begin
         ShowMessage ('First year value out of range.  Values must be within range of system membership data.  Reenter a value between ' +
                 inttostr(configuration.first_nation_year) + ' and ' + inttostr(configuration.last_nation_year));
         Edit1.clear;
      end
   else start_year_ok := true;

   if temp_year2 = 0 then
     begin   {problem with entry}
        ShowMessage ('Second year value was not a valid integer.  Please reenter.');
        Edit2.clear;
     end
   else if not ( (temp_year2 >= configuration.first_nation_year) and
                 (temp_year2 <= configuration.last_nation_year)) then
      begin
         ShowMessage ('Second year value out of range.  Values must be within range of system membership data.  Reenter a value between ' +
                 inttostr(configuration.first_nation_year) + ' and ' + inttostr(configuration.last_nation_year));
         Edit2.clear;
      end
   else end_year_ok := true;

   if (start_year_ok and end_year_ok) then
      begin
         {make sure they are ordered lower to higher}
         if temp_year1 > temp_year2 then
           begin
              ShowMessage ('You entered the end year as prior to the start year.  Order of the two years has been reversed.');
              third := temp_year1;
              temp_year1 := temp_year2;
              temp_year2 := third;
              edit1.text := inttostr (temp_year1);
              edit2.text := inttostr (temp_year2);
           end;
         SettingsForm.label1.caption:='Start Year Selected = ' + Edit1.text;
         SettingsForm.label2.caption:='End Year Selected = ' + Edit2.text;
         user_selections.years := subrange;
         user_selections.first_year := temp_year1;
         user_selections.last_year := temp_year2;
         close;
      end
      else user_selections.years := noyears;
end;



begin      {OK btn click}
   {need to update user_selections for most elements, because most are not
    updated as they click.  Those elements are the check boxes, header and vars.}

   {First, basic destination/format tab}
   {Only one of the separation checks will be true}
   if SeparateWithTabs.checked then user_selections.output_format.separator := tab;
   if SeparateWithSpaces.checked then user_selections.output_format.separator := space;
   if SeparateWithCommas.checked then user_selections.output_format.separator := comma;

   {command file options}
   if SPSSCommandFileCheckBox.checked then include (user_selections.output_format.CommandFiles, SPSS) else exclude (user_selections.output_format.CommandFiles, SPSS);
   if StataCommandFileCheckBox.checked then include (user_selections.output_format.CommandFiles, Stata) else exclude (user_selections.output_format.CommandFiles, Stata);
   if LIMDEPCommandFileCheckBox.checked then include (user_selections.output_format.CommandFiles, limdep) else exclude (user_selections.output_format.CommandFiles, limdep);
   if commandfileOnlyCheckBox.checked then include (user_selections.output_format.CommandFiles, commandfilesonly) else exclude (user_selections.output_format.CommandFiles, commandfilesonly);



   {Second, cases tab}

   if PrintAACheckBox.checked then
         SettingsForm.dyadivsilb.visible:=true;
   {most radio-dial settings were set when cases were chosen, via onclick}



  {4th tab is variables Tab.  This section includes checks for what to do if dyadic output.}
   {some variables are included only for monadic or dyadic}
  {In order to preserve settings on vars, do  not blank out the set of variables,
   but just add/subtract.  problematic dyadic/monadic variables were cleared in entering
   the output window.}

   {clear all the variable labels, and reset them below}
   With SettingsForm do
      begin
         ccodelb.visible:=false;
         yearlb.visible:=false;
         caplb.visible:=false;
         majpowlb.visible:=false;
         relreglb.visible:=false;
         reguncerlb.visible:=false;
         riskeulb.visible:=false;
         riskwtrlb.visible:=false;
         risklb.visible:=false;
         taulb.visible:=false;
         distancelb.visible:=false;
         euwartrplb.visible:=false;
         euwrlb.visible:=false;
         polrellb.visible:=false;
         contlb.visible:=false;
         dyadlb.visible:=false;
         countrydyadlb.visible:=false;
         HomeReglb.visible:=false;
         slb.visible:=false;
         isolb.Visible:=false;
         tauwithleaderlb.visible:=false;
         DispLb.visible:=false;
         polity3lb.visible:=false;
         PeaceYrsLb.visible:=false;
         UserVariablesLb.visible:=false;
         Alliancelb.visible:=false;
         AlliancePortLb.visible:=false;
         EqLabel.visible:=false;
         OngoingLb.visible:=false;
         reverselb.visible := false;
         MarkSubsLabel.visible:=false;
         DropJoinerLb.visible:=false;
         DispTrueLabel.visible:=false;
         SideADispLb.visible:=false;
      end;

   {Deal with year range settings, if they want a subset}
   if user_selections.years = subrange then Set_Year_Range();

   {Previously, I checked to make sure that the general COW MID or Maoz MID vars are included if a
    constituent var has been checked.  Now, don't bother with this.  This will make the addition of
    additional conflict tabs easier, plus, they're smart enough to set the main check box
    themselves.  If they don't it will quickly and easily make it so they don't get the sub
    variables if they just uncheck the top boxes.}
   {if not (COWMIDDisputeDataCheckBox.checked or MaozDisputeDataCheckBox.checked) then
      if (MidNumCheckBox.checked or startdatecheck.checked or enddatecheckbox.checked
          or SideACheckBox.checked or RevStateCheckBox.checked or RevTypeCheckBox.checked
          or FatalityLevelStateCheckBox.checked or HiActCheckBox.checked
          or OriginatorCheckBox.checked or OutcomeCheckBox.checked or SettlementCheckBox.checked
          or FatalityLevelDisputeCheckBox.checked or HiActDisputeCheckBox.checked
          or HostLevDisputeCheckBox.checked or RecipCheckBox.checked or NumOfStatesBox.checked)
      then COWMIDDisputeDataCheckBox.checked := true;
   if not (COWMIDDisputeDataCheckBox.checked or MaozDisputeDataCheckBox.checked) then
      if (MaozCowWarBox.checked or MaozDurIndxBox.checked or MaozDurDaysBox.checked or MaozReciprocatedDyadicBox.checked)
      then MaozDisputeDataCheckBox.checked := true;   }

   include_exclude (ccodes, user_selections.output_format.variables, CcodeVarBox, SettingsForm.ccodelb);
   include_exclude (year, user_selections.output_format.variables, YearVarBox, SettingsForm.yearlb);
   include_exclude (abbrevs, user_selections.output_format.variables, AbbrevCheckBox, SettingsForm.abbrlb);
   include_exclude (SystemCapabilities, user_selections.output_format.variables, natcapbox, SettingsForm.caplb);
   include_exclude (RawCapabilities, user_selections.output_format.variables, rawcapbox, SettingsForm.caplb);
   include_exclude (PowerStatus, user_selections.output_format.variables, MajorPowerCheckBox, SettingsForm.majpowlb);
   include_exclude (riskdetailsTau, user_selections.output_format.variables, RiskDetailTauCheckBox, SettingsForm.risklb);
   include_exclude (riskdetailsS, user_selections.output_format.variables, RiskDetailSCheckBox, SettingsForm.risklb);
   include_exclude (HomeRegion, user_selections.output_format.variables, HomeRegionCheckBox, SettingsForm.homereglb);
   include_exclude (UncertaintyTau, user_selections.output_format.variables, UncertTauCheckBox, SettingsForm.reguncerlb);
   include_exclude (UncertaintyS, user_selections.output_format.variables, UncertSCheckBox, SettingsForm.reguncerlb);
   include_exclude (tauwithleader, user_selections.output_format.variables, tauwithleaderbox, SettingsForm.tauwithleaderlb);
   include_exclude (swithleader, user_selections.output_format.variables, swithleaderbox, SettingsForm.tauwithleaderlb);
   include_exclude (democ, user_selections.output_format.variables, democcheckbox, SettingsForm.polity3lb);
   include_exclude (autoc, user_selections.output_format.variables, autoccheckbox, SettingsForm.polity3lb);
   include_exclude (xrreg, user_selections.output_format.variables, xrregCheckBox, SettingsForm.polity3lb);
   include_exclude (xrcomp, user_selections.output_format.variables, xrcompCheckBox, SettingsForm.polity3lb);
   include_exclude (xropen, user_selections.output_format.variables, xropenCheckBox, SettingsForm.polity3lb);
   include_exclude (mono, user_selections.output_format.variables, monoCheckBox, SettingsForm.polity3lb);
   include_exclude (xconst, user_selections.output_format.variables, xconstCheckBox, SettingsForm.polity3lb);
   include_exclude (parreg, user_selections.output_format.variables, parregCheckBox, SettingsForm.polity3lb);
   include_exclude (parcomp, user_selections.output_format.variables, parcmpCheckBox, SettingsForm.polity3lb);
   include_exclude (cent, user_selections.output_format.variables, centCheckBox, SettingsForm.polity3lb);
   include_exclude (dem, user_selections.output_format.variables, RussettDem, SettingsForm.polity3lb);
   include_exclude (laggeddem, user_selections.output_format.variables, laggeddemaut, SettingsForm.polity3lb);
   include_exclude (democratization, user_selections.output_format.variables, demchangebox, SettingsForm.polity3lb);
   include_exclude (StatesInSystem, user_selections.output_format.variables, StatesInSystemBox, SettingsForm.SysCharLabel);
   include_exclude (GPsInSystem, user_selections.output_format.variables, GPsInSystemBox, SettingsForm.SysCharLabel);
   include_exclude (SysConcentration, user_selections.output_format.variables, SysConBox, SettingsForm.SysCharLabel);
   include_exclude (SysMovement, user_selections.output_format.variables, SysMoveBox, SettingsForm.SysCharLabel);
   include_exclude (SysMovement5Yr, user_selections.output_format.variables, SysMove5Box, SettingsForm.SysCharLabel);
   include_exclude (SysMoveGP, user_selections.output_format.variables, SysMoveGPBox, SettingsForm.SysCharLabel);
   include_exclude (SysMoveGP5Yr, user_selections.output_format.variables, SysMoveGP5Box, SettingsForm.SysCharLabel);
   include_exclude (RelRegion, user_selections.output_format.variables, RelRegionCheckBox, SettingsForm.relreglb);
   include_exclude (tau, user_selections.output_format.variables, taubox, SettingsForm.taulb);
   include_exclude (DyadicTime, user_selections.output_format.variables, ExistTimeCheckBox, SettingsForm.dyadlb);
   include_exclude (alliance, user_selections.output_format.variables, AllianceCheckBox, SettingsForm.alliancelb);
   include_exclude (AlliancePortfolioUnweighted, user_selections.output_format.variables, AlliancePortfolioTableUnWeightedCheckBox, SettingsForm.AlliancePortLb);
   include_exclude (AlliancePortfolioWeighted, user_selections.output_format.variables, AlliancePortfolioTableWeightedCheckBox, SettingsForm.AlliancePortLb);
   include_exclude (sunweighted, user_selections.output_format.variables, sboxunweighted, SettingsForm.slb);
   include_exclude (sweighted, user_selections.output_format.variables, sboxweighted, SettingsForm.slb);
   include_exclude (EUWarTrapTau, user_selections.output_format.variables, EUWartrapTaubox, SettingsForm.euwartrplb);
   include_exclude (EUWarTrapS, user_selections.output_format.variables, EUWartrapSbox, SettingsForm.euwartrplb);
   include_exclude (EUWarReasonTau, user_selections.output_format.variables, EUReasonTaubox, SettingsForm.euwrlb);
   include_exclude (EUWarReasonSUnweighted, user_selections.output_format.variables, EUReasonSUnbox, SettingsForm.euwrlb);
   include_exclude (EUWarReasonSweighted, user_selections.output_format.variables, EUReasonSWtbox, SettingsForm.euwrlb);
   include_exclude (EQWarReasonTau, user_selections.output_format.variables, EQTaubox, SettingsForm.eqlabel);
   include_exclude (EQWarReasonSUnweighted, user_selections.output_format.variables, EQSUnBox, SettingsForm.eqlabel);
   include_exclude (EQWarReasonSweighted, user_selections.output_format.variables, EQSWtBox, SettingsForm.eqlabel);
   include_exclude (distance, user_selections.output_format.variables, DistanceVarBox, SettingsForm.distancelb);
   include_exclude (MarkPolRelevant, user_selections.output_format.variables, MarkPolRelevantBox, SettingsForm.polrellb);
   include_exclude (contig, user_selections.output_format.variables, ContiguityCheckBox, SettingsForm.contlb);
   include_exclude (ColonialContig, user_selections.output_format.variables, ColonialContiguityCheckBox, SettingsForm.contlb);
   include_exclude (COWpeaceyrs, user_selections.output_format.variables, COWPeaceYrsBox, SettingsForm.peaceyrslb);
   include_exclude (Maozpeaceyrs, user_selections.output_format.variables, MaozPeaceYrsBox, SettingsForm.peaceyrslb);
   include_exclude (ICBpeaceyrs, user_selections.output_format.variables, ICBPeaceYrsBox, SettingsForm.peaceyrslb);
   include_exclude (ICBpeacedys, user_selections.output_format.variables, ICBpeacedysCheckBox, SettingsForm.peaceyrslb);

   include_exclude (ISO_code, user_selections.output_format.variables, ISO_code_CheckBox, SettingsForm.isolb);
   include_exclude (ISO_abb2, user_selections.output_format.variables, ISO_abb2_CheckBox, SettingsForm.isolb);
   include_exclude (ISO_abb3, user_selections.output_format.variables, ISO_abb3_CheckBox, SettingsForm.isolb);
   include_exclude (ISO_full, user_selections.output_format.variables, ISO_full_CheckBox, SettingsForm.isolb);
   include_exclude (ISO_short, user_selections.output_format.variables, ISO_short_CheckBox, SettingsForm.isolb);

   include_exclude (COW_disputes, user_selections.output_format.variables, COWMIDDisputeDataCheckBox, SettingsForm.displb);
   include_exclude (COWMIDNumber, user_selections.output_format.variables, COWMidNumCheckBox, SettingsForm.displb);
   include_exclude (COWMIDName, user_selections.output_format.variables, COWMidNameCheckBox, SettingsForm.displb);
   include_exclude (COWMIDStart, user_selections.output_format.variables, COWstartdatecheck, SettingsForm.displb);
   include_exclude (COWMIDEnd, user_selections.output_format.variables, COWenddatecheckbox, SettingsForm.displb);
   include_exclude (COWMIDSideA, user_selections.output_format.variables, COWSideACheckBox, SettingsForm.displb);
   include_exclude (COWMIDRevisionist, user_selections.output_format.variables, COWRevStateCheckBox, SettingsForm.displb);
   include_exclude (COWMIDRevisiontype, user_selections.output_format.variables, COWRevTypeCheckBox, SettingsForm.displb);
   include_exclude (COWMIDFatalityState, user_selections.output_format.variables, COWFatalityLevelStateCheckBox, SettingsForm.displb);
   include_exclude (COWMIDHiActState, user_selections.output_format.variables, COWHiActCheckBox, SettingsForm.displb);
   include_exclude (COWMIDOriginator, user_selections.output_format.variables, COWOriginatorCheckBox, SettingsForm.displb);
   include_exclude (COWMIDOutcome, user_selections.output_format.variables, COWOutcomeCheckBox, SettingsForm.displb);
   include_exclude (COWMIDSettlement, user_selections.output_format.variables, COWSettlementCheckBox, SettingsForm.displb);
   include_exclude (COWMIDFatalityDispute, user_selections.output_format.variables, COWFatalityLevelDisputeCheckBox, SettingsForm.displb);
   include_exclude (COWMIDHiActDispute, user_selections.output_format.variables, COWHiActDisputeCheckBox, SettingsForm.displb);
   include_exclude (COWMIDHostLevDispute, user_selections.output_format.variables, COWHostLevDisputeCheckBox, SettingsForm.displb);
   include_exclude (COWMIDReciprocated, user_selections.output_format.variables, COWRecipCheckBox, SettingsForm.displb);
   include_exclude (COWMIDNumStates, user_selections.output_format.variables, COWNumOfStatesBox, SettingsForm.displb);
   include_exclude (COWMarkMIDJoiners, user_selections.output_format.variables, COWMarkJoinersCheckBox, SettingsForm.displb);
   include_exclude (COWNumMIDs, user_selections.output_format.variables, COWNumMIDCheckBox, SettingsForm.displb);
   include_exclude (COWRole, user_selections.output_format.variables, COWRoleCheckBox, SettingsForm.displb);
   include_exclude (COWLinkStatus, user_selections.output_format.variables, COWLinkStatusCheckBox, SettingsForm.displb);
   include_exclude (COWPeaceDays, user_selections.output_format.variables, COWPeaceDaysCheckBox, SettingsForm.displb);

   include_exclude (Maoz_dyadic_disputes, user_selections.output_format.variables, MaozDisputeDataCheckBox, SettingsForm.displb);
   include_exclude (MaozMIDNumber, user_selections.output_format.variables, MaozMidNumCheckBox, SettingsForm.displb);
   include_exclude (MaozMIDName, user_selections.output_format.variables, MaozMidNameCheckBox, SettingsForm.displb);
   include_exclude (MaozMIDStart, user_selections.output_format.variables, Maozstartdatecheck, SettingsForm.displb);
   include_exclude (MaozMIDEnd, user_selections.output_format.variables, Maozenddatecheckbox, SettingsForm.displb);
   include_exclude (MaozMIDSideA, user_selections.output_format.variables, MaozSideACheckBox, SettingsForm.displb);
   include_exclude (MaozMIDRevisionist, user_selections.output_format.variables, MaozRevStateCheckBox, SettingsForm.displb);
   include_exclude (MaozMIDRevisiontype, user_selections.output_format.variables, MaozRevTypeCheckBox, SettingsForm.displb);
   include_exclude (MaozMIDFatalityState, user_selections.output_format.variables, MaozFatalityLevelStateCheckBox, SettingsForm.displb);
   include_exclude (MaozMIDHiActState, user_selections.output_format.variables, MaozHiActCheckBox, SettingsForm.displb);
   include_exclude (MaozMIDOriginator, user_selections.output_format.variables, MaozOriginatorCheckBox, SettingsForm.displb);
   include_exclude (MaozMIDOutcome, user_selections.output_format.variables, MaozOutcomeCheckBox, SettingsForm.displb);
   include_exclude (MaozMIDSettlement, user_selections.output_format.variables, MaozSettlementCheckBox, SettingsForm.displb);
   include_exclude (MaozMIDFatalityDispute, user_selections.output_format.variables, MaozFatalityLevelDisputeCheckBox, SettingsForm.displb);
   include_exclude (MaozMIDHiActDispute, user_selections.output_format.variables, MaozHiActDisputeCheckBox, SettingsForm.displb);
   include_exclude (MaozMIDHostLevDispute, user_selections.output_format.variables, MaozHostLevDisputeCheckBox, SettingsForm.displb);
   include_exclude (MaozMIDReciprocated, user_selections.output_format.variables, MaozRecipCheckBox, SettingsForm.displb);
   include_exclude (MaozMIDNumStates, user_selections.output_format.variables, MaozNumOfStatesBox, SettingsForm.displb);
   include_exclude (MaozMarkMIDJoiners, user_selections.output_format.variables, MaozMarkJoinersCheckBox, SettingsForm.displb);
   include_exclude (MaozCOWWar, user_selections.output_format.variables, MaozCowWarBox, SettingsForm.displb);
   include_exclude (MaozDurindx, user_selections.output_format.variables, MaozDurIndxBox, SettingsForm.displb);
   include_exclude (MaozDurDays, user_selections.output_format.variables, MaozDurDaysBox, SettingsForm.displb);
   include_exclude (MaozReciprocatedDyadic, user_selections.output_format.variables, MaozReciprocatedDyadicBox, SettingsForm.displb);
   include_exclude (MaozNumMIDs, user_selections.output_format.variables, MaozNumMIDCheckBox, SettingsForm.displb);
   include_exclude (MaozRole, user_selections.output_format.variables, MaozRoleCheckBox, SettingsForm.displb);
   include_exclude (MaozLinkStatus, user_selections.output_format.variables, MaozLinkStatusCheckBox, SettingsForm.displb);
   include_exclude (Maozpeacedys, user_selections.output_format.variables, MaozPeaceDysBox, SettingsForm.displb);

   {Also set first or most serious dispute setting.}
   user_selections.dispute_info.UseMostSeriousDispute := UseMostSeriousMIDCheckBox.Checked;
   user_selections.dispute_info.UseFirstDispute := UseFirstMIDCheckBox.Checked;

   include_exclude (ICB_crises, user_selections.output_format.variables, ICBDyadicDataCheckBox, SettingsForm.displb);
   include_exclude (ICBCrisisNumber, user_selections.output_format.variables, ICBCrisisNumberCheckBox, SettingsForm.displb);
   include_exclude (ICBCrisisName, user_selections.output_format.variables, ICBCrisisNameCheckBox, SettingsForm.displb);
   include_exclude (ICBCrisisDyadNumber, user_selections.output_format.variables, ICBCrisisDyadNumberCheckBox, SettingsForm.displb);
   include_exclude (ICBDurDays, user_selections.output_format.variables, ICBDurDaysCheckBox, SettingsForm.displb);
   include_exclude (ICBDurYear, user_selections.output_format.variables, ICBDurYearsCheckBox, SettingsForm.displb);
   include_exclude (ICBDyadicStart, user_selections.output_format.variables, ICBDyadicStartCheckBox, SettingsForm.displb);
   include_exclude (ICBDyadicEnd, user_selections.output_format.variables, ICBDyadicEndCheckBox, SettingsForm.displb);
   include_exclude (ICBOneSided, user_selections.output_format.variables, ICBOneSidedCheckBox, SettingsForm.displb);
   include_exclude (ICBStateStart, user_selections.output_format.variables, ICBStateStartCheckBox, SettingsForm.displb);
   include_exclude (ICBStateEnd, user_selections.output_format.variables, ICBStateEndCheckBox, SettingsForm.displb);
   include_exclude (ICBJoiner, user_selections.output_format.variables, ICBJoinerCheckBox, SettingsForm.displb);
   include_exclude (ICBActorSequence, user_selections.output_format.variables, ICBActorSequenceCheckBox, SettingsForm.displb);
   include_exclude (ICBCOWMember, user_selections.output_format.variables, ICBCOWMemberCheckBox, SettingsForm.displb);
   include_exclude (ICBGWMember, user_selections.output_format.variables, ICBGWMemberCheckBox, SettingsForm.displb);
   include_exclude (ICBIntraWar, user_selections.output_format.variables, ICBIntraWarCheckBox, SettingsForm.displb);
   include_exclude (ICBNumCrises, user_selections.output_format.variables, ICBNumCrisisCheckBox, SettingsForm.displb);


   {These are a few variables and settings that can't be processed by the default method}
   if (democcheckbox.checked or autoccheckbox.checked or xrregCheckBox.checked or
       xrcompCheckBox.checked or xropenCheckBox.checked or monoCheckBox.checked or
       xconstCheckBox.checked or parregCheckBox.checked or parcmpCheckBox.checked or
       centCheckBox.checked or RussettDem.checked or laggeddemaut.checked or
       demchangebox.checked)
      then
         include (user_selections.output_format.variables, polity3)
      else    {there are no polity variables}
         exclude (user_selections.output_format.variables, polity3);
   if (StatesInSystemBox.checked or GPsInSystemBox.checked or SysConBox.checked or SysMoveBox.checked
       or SysMove5Box.checked or SysMoveGPBox.checked or SysMoveGP5Box.checked)
      then
         include (user_selections.output_format.variables, systemchars)
      else
         exclude (user_selections.output_format.variables, systemchars);

   exclude (user_selections.output_format.variables, riskTau);
   exclude (user_selections.output_format.variables, riskS);
   if riskEUGENETaubox.checked then
      begin
         include (user_selections.output_format.variables, riskTau);
         user_selections.risk_data_source := risk_EUGENE;
         SettingsForm.riskeulb.visible:=true;
      end;
   if riskEUGENESbox.checked then
      begin
         include (user_selections.output_format.variables, riskS);
         user_selections.risk_data_source := risk_EUGENE;
         SettingsForm.riskeulb.visible:=true;
      end;
   if RiskWTRCheckBox.checked then
      begin
         include (user_selections.output_format.variables, riskTau);
         user_selections.risk_data_source := risk_WTR;
         SettingsForm.riskwtrlb.visible:=true;
      end;
  

   {User data set variables}
   {first, blank out list of any previously selected user variables.}
   for x := 0 to (configuration.user_data_set_info.get_num_data_sets - 1) do
      setlength(user_selections.user_data_sets_selections[x].data_set_variables, 0);
   exclude (user_selections.output_format.variables, UserVariables);

   for varbox := 0 to high (VarFormCheckBoxes) do
      begin
         if ((VarFormCheckBoxes[varbox].ACheckBox.enabled) and (VarFormCheckBoxes[varbox].ACheckBox.checked)) then
         {add this var}
         begin
            setlength(user_selections.user_data_sets_selections[VarFormCheckBoxes[varbox].original_user_dataset_num].data_set_variables,
                      length(user_selections.user_data_sets_selections[VarFormCheckBoxes[varbox].original_user_dataset_num].data_set_variables)+1);
            spot_to_add :=high(user_selections.user_data_sets_selections[VarFormCheckBoxes[varbox].original_user_dataset_num].data_set_variables);
            user_selections.user_data_sets_selections[VarFormCheckBoxes[varbox].original_user_dataset_num].data_set_variables[spot_to_add] :=
                VarFormCheckBoxes[varbox].original_user_var_num;
            include (user_selections.output_format.variables, UserVariables);
         end;
      end; {for varbox}



   if (user_selections.output_this = output_directed_dyads) or
      (user_selections.output_this = output_nondirected_dyads) or
      (user_selections.output_this = output_directed_dispute_initiation_dyads) or
      (user_selections.output_this = output_nondirected_dispute_dyads) then
     begin

       {Several options will only be enabled, and relevant, if MIDs are selected
        for output.  The actual "non-implementation" of these settings is done
        in the euinoutd file's wanted_in_year procedure.  I don't need to change
        the actual variable settings here.  But, do change the display.  }


       {3rd tab is Sampling tab.
        But we process it first because the sampling settings affect the exclusion
        settings (if they select sampling, they must have an exclusion set).}
       {Note that the effect of picking sampling on exclusions is processed below.}
       if NoSampleBtn.Checked then user_selections.sample_info.sampling := false
       else if SamplingYesBtn.checked then
          begin
             user_selections.sample_info.sampling := true;

             try     {convert the sampling %s from the edit boxes}
                user_selections.sample_info.proportion_dispute_dyads := strtofloat(DispSampleEdit.text);
                if user_selections.sample_info.proportion_dispute_dyads > 1 then
                   begin
                      EUGeneError ('Sampling proportion for dispute dyads was > 1.  Value must be less than or equal to 1.  Setting proportion of dispute dyads to 1.0.', 2, continue, error_log);
                      user_selections.sample_info.proportion_dispute_dyads := 1;
                   end;
             except
                EUGeneError ('Sampling proportion for dispute dyads was not entered properly in edit window.  Setting proportion of dispute dyads to 1.0.', 2, continue, error_log);
                user_selections.sample_info.proportion_dispute_dyads := 1;
             end;

             try     {convert the sampling %s from the edit boxes}
                user_selections.sample_info.proportion_non_dispute_dyads := strtofloat(NonDispSampleEdit.text);
                if user_selections.sample_info.proportion_non_dispute_dyads > 1 then
                   begin
                      EUGeneError ('Sampling proportion for non-dispute dyads was > 1.  Value must be less than or equal to 1.  Setting proportion of non-dispute dyads to 1.0.', 2, continue, error_log);
                      user_selections.sample_info.proportion_non_dispute_dyads := 1;
                   end;
             except
                EUGeneError ('Sampling proportion for non-dispute dyads was not entered properly in edit window.  Setting proportion of non-dispute dyads to 1.0.', 2, continue, error_log);
                user_selections.sample_info.proportion_non_dispute_dyads := 1;
             end;

             if RandSeedCheckBox.checked then
                begin
                   user_selections.sample_info.use_randseed := true;
                   try
                      randseed := strtoint (RandomSeedEdit.text);
                      user_selections.sample_info.randseed := strtoint (RandomSeedEdit.text);
                   except
                      EUGeneError ('Random number seed not entered properly in edit window for sampling.  Setting random seed to 0.', 2, continue, error_log);
                      randseed := 0;
                      user_selections.sample_info.randseed := 0;
                   end;
                end
             else
                begin
                   user_selections.sample_info.use_randseed := false;
                   randomize;
                end;
          end
       else
          begin
             EUGeneError ('Programming error - sampling buttons incorrectly set, neither sampling button was set;  setting to no sampling;  Please notify programmer of error',
                    2, continue, error_log);
             user_selections.sample_info.sampling := false;
          end;


       {Now set the conflict exclusion data set}
       if ExcludeConflictCOWMIDButton.checked then
          user_selections.conflict_exclusion_selection := cds_COWMID;
       if ExcludeConflictMaozMIDButton.checked then
          user_selections.conflict_exclusion_selection := cds_MaozMID;
       if ExcludeConflictICBButton.checked then
          user_selections.conflict_exclusion_selection := cds_ICB;
       if ExcludeConflictWarsButton.checked then
          user_selections.conflict_exclusion_selection := cds_COWWar;
       if ExcludeConflictNoneButton.checked then
          user_selections.conflict_exclusion_selection := cds_none;

          {Note that if sampling is on, they must also have an exclusion set selected, because
           sampling works for dispute dyad years.  The check for this is processed here.}
          {Do they have an exclusion set picked?}
       if (user_selections.sample_info.sampling) and (user_selections.conflict_exclusion_selection = cds_none) then
          begin
             {We need to get an exclusion set, but it's not marked.  Go in order sequentially and mark
              the first that we come across.}
             if COWMIDDisputeDataCheckBox.checked then
                begin
                   ExcludeConflictCOWMIDButton.checked := true;
                   user_selections.conflict_exclusion_selection := cds_COWMID;
                end
             else
             if MaozDisputeDataCheckBox.checked then
                begin
                   ExcludeConflictMaozMIDButton.checked := true;
                   user_selections.conflict_exclusion_selection := cds_MaozMID;
                end
             else
             if ICBDyadicDataCheckBox.checked then
                begin
                   ExcludeConflictICBButton.checked := true;
                   user_selections.conflict_exclusion_selection := cds_ICB;
                end
             else   {if they haven't selected a dispute variable, then pick COW MIDs}
                begin
                   ExcludeConflictCOWMIDButton.checked := true;
                   user_selections.conflict_exclusion_selection := cds_COWMID;
                end;
             tempmessage := 'Using sampling requires you to pick a dispute data set which can be used as the basis for dispute inclusion and exclusion.  You may change this setting under the "Case/Conflict Exclusion" tab.  EUGene has currently set this data set to the ';
             case
                user_selections.conflict_exclusion_selection of
                   cds_COWMID : tempmessage := tempmessage + 'COW dispute data set.';
                   cds_MaozMID : tempmessage := tempmessage + 'Maoz dyadic dispute data set.';
                   cds_ICB : tempmessage := tempmessage + 'ICB crisis data set.';
                   else begin
                         EUGeneError ('programming error - in setting exclusion set based on sampling choice, an appropriate dispute setting was not seen.', 1, continue, error_log);
                         ExcludeConflictCOWMIDButton.checked := true;
                         user_selections.conflict_exclusion_selection := cds_COWMID;
                      end;
                end;    {case}
             ShowMessage (tempmessage);
          end;         {if sampling and no exclusion set}


       {Process the set of exclusions, and separately process the treatment of initiators.}

       {First, initiators, if they selected a conflict set.}
       if (COWMIDDisputeDataCheckBox.checked) or (MaozDisputeDataCheckBox.checked) or
          (ICBDyadicDataCheckBox.checked) or (COWDyadicWarsDataCheckBox.checked) then
          begin
             {5th, the MID/ongoing tab}
             {Only one of the next 3 will be checked, b/c they are radio btns}
             user_selections.dispute_info.OnlyTrueInitiators := false;
             user_selections.dispute_info.JoinersOnInitiatingSideAsInitiators := false;
             user_selections.dispute_info.AllJoinersAsInitiators := false;
             if TrueInitiatorsCheckBox.checked then
                begin
                   user_selections.dispute_info.OnlyTrueInitiators := true;
                   user_selections.dispute_info.JoinersOnInitiatingSideAsInitiators := false;
                   user_selections.dispute_info.AllJoinersAsInitiators := false;
                   SettingsForm.DispTrueLabel.caption:='True Initiators';
                   SettingsForm.DispTrueLabel.visible:=true;
                end;
             if JoinerInitCheckBox.checked then
                begin
                   user_selections.dispute_info.OnlyTrueInitiators := false;
                   user_selections.dispute_info.JoinersOnInitiatingSideAsInitiators := true;
                   user_selections.dispute_info.AllJoinersAsInitiators := false;
                   SettingsForm.DispTrueLabel.caption:='Initiators + Init side Joiners';
                   SettingsForm.DispTrueLabel.visible:=true;
                end;
             if JoinerAnyCheckBox.checked then
                begin
                   user_selections.dispute_info.OnlyTrueInitiators := false;
                   user_selections.dispute_info.JoinersOnInitiatingSideAsInitiators := false;
                   user_selections.dispute_info.AllJoinersAsInitiators := true;
                   SettingsForm.DispTrueLabel.caption:='Initiators + All Joiners';
                   SettingsForm.DispTrueLabel.visible:=true;
                end;

                {Only one of next 2 will be true b/c of radio buttons}
             if (user_selections.output_this = output_directed_dyads) or
                (user_selections.output_this = output_nondirected_dispute_dyads) or
                (user_selections.output_this = output_directed_dispute_initiation_dyads) then
                begin
                   if SideAAsInitiatorCheckBox.checked then
                     begin
                         user_selections.dispute_info.SideAIsInitiator := true;
                         SettingsForm.SideADispLb.caption:='Initiator = Side A';
                         SettingsForm.SideADispLb.visible:=true;
                     end;
                   if InitiationAsRevisionistRadioButton.checked then
                     begin
                         user_selections.dispute_info.SideAIsInitiator := false;
                         SettingsForm.SideADispLb.caption:='Initiator = Revisionist';
                         SettingsForm.SideADispLb.visible:=true;
                     end;
                end
                else
                   SettingsForm.SideADispLb.visible:=false;

                {Only one of next 2 will be true b/c of radio buttons}
          end;


       {Now, exclusions}

       if user_selections.conflict_exclusion_selection <> cds_none then
          {process the rest of the exclusion tabs.  }
          begin

             if MarkSubsequentAsInitiationCheckBox.checked then
                begin
                   user_selections.dispute_info.MarkSubsequentAsInitiation := true;
                   SettingsForm.MarkSubsLabel.visible:=true;
                end
                else
                begin
                   user_selections.dispute_info.MarkSubsequentAsInitiation := false;
                   SettingsForm.MarkSubsLabel.visible:=false;
                end;

             {Only one of the next 3 will be checked, b/c they are radio buttons.}
             if AllOngoingDisputeYearButton.checked then
                begin
                  SettingsForm.OngoingLb.caption := 'Ongoing MID Years Always Included';
                  SettingsForm.OngoingLb.visible:=true;
                  user_selections.output_format.printAllOngoing := true;
                  user_selections.output_format.printOngoingifNewDisp := false;
                end;
             if OngoingIfNewDispButton.checked then
                begin
                  SettingsForm.OngoingLb.caption := 'Ongoing MID Years Included iff New MID';
                  SettingsForm.OngoingLb.visible:=true;
                  user_selections.output_format.printOngoingifNewDisp := true;
                  user_selections.output_format.printAllOngoing := false;
                end;
             if DropAllOngoingButton.checked then
                begin
                  SettingsForm.OngoingLb.caption := 'Including No Ongoing MID Years';
                  SettingsForm.OngoingLb.visible:=true;
                  user_selections.output_format.printOngoingifNewDisp := false;
                  user_selections.output_format.printAllOngoing := false;
                end;

                {Only one of next 2 will be true b/c of radio buttons.}
             if (user_selections.output_this = output_directed_dyads) or
                (user_selections.output_this = output_directed_dispute_initiation_dyads) or
                (user_selections.output_this = output_nondirected_dispute_dyads) then
                begin
                   if AlwaysIncludeTgtVsInitDyadsRadioButton.checked then
                      begin
                         user_selections.dispute_info.AlwaysIncludeTgtVsInitiator := true;
                         user_selections.dispute_info.IncludeTgtVsInitiatoriffNew := false;
                         SettingsForm.reverselb.caption := 'Include All Tgt vs. Init. Dyads';
                         SettingsForm.reverselb.visible := true;
                      end;
                   if IncludeTgtVsInitDyadsiffNewRadioButton.checked then
                      begin
                         user_selections.dispute_info.AlwaysIncludeTgtVsInitiator := false;
                         user_selections.dispute_info.IncludeTgtVsInitiatoriffNew := true;
                         SettingsForm.reverselb.caption := 'Include Tgt vs. Init. Dyads iff new';
                         SettingsForm.reverselb.visible := true;
                      end;
                end
                else
                   SettingsForm.reverselb.visible := false;

             if DropJoinerRadioButton.checked then
                begin
                   user_selections.dispute_info.DropJoinerDirectedDyads := true;
                   SettingsForm.DropJoinerLb.visible := true;
                   SettingsForm.DropJoinerLb.caption := 'Drop Joiner Dyads'
                end;
             if IncludeJoinerRadioButton.checked then
                begin
                   user_selections.dispute_info.DropJoinerDirectedDyads := false;
                   SettingsForm.DropJoinerLb.visible:=false;
                   SettingsForm.DropJoinerLb.caption := 'Keep Joiner Dyads'
                end;
          end;               {if disputes checked}


     end;                      {if output dyads}

   if Header.checked then
      SettingsForm.headerlb.visible:=true
      else SettingsForm.headerlb.visible:=false;

   {header}
   user_selections.output_format.header := header.checked;


   {Printing of dyads i vs. i}
   user_selections.output_format.printii := PrintAACheckBox.checked;

    {if I need it, read user dyads.  This procedure will also delete an existing
     list if one exists.}
   if user_selections.dyads_selected = user_file_read then
      specified_dyad_list_set (user_selections);

   {output of dispute years with ongoing dispute at the beginning.}
   user_selections.output_format.printAllOngoing := AllOngoingDisputeYearButton.checked;

   user_selections.output_format.printOngoingIfNewDisp := OngoingIfNewDispButton.checked;


   {Check procedure to run - monadic/dyadic.}
   check_completeness (user_selections);
   if (user_selections.complete=true) then
      begin
         {All options are selected, so return control to main window,
          which will call the output procedure.}
         modalResult := MROK;
         {close;   Window will be closed since the MROk is set.}
      end              {if complete}
   else modalResult := MRNone;

end;        {OK button click}

{-----------------------------------------------------------------------}

procedure TOutput_Options.CancelBtnClick(Sender: TObject);
begin
   {Close;}
end;

{------------------------------------------------------------------------}

procedure TOutput_Options.HelpbtnClick(Sender: TObject);
var helpfilename : string;
begin
   if PagedOutput.ActivePage = OutputDestinationSheet then
      helpfilename := configuration.HelpFiles_DestinationPage_name
   else if PagedOutput.ActivePage = CaseSelectionPage then
      helpfilename := configuration.HelpFiles_CasePage_name
   else if PagedOutput.ActivePage = SamplingSheet then
      helpfilename := configuration.HelpFiles_SamplingPage_name
   else if PagedOutput.ActivePage = VariableSheet then
      helpfilename := configuration.HelpFiles_VariablePage_name
   else if PagedOutput.ActivePage = DisputeInitiatorSheet then
      helpfilename := configuration.HelpFiles_DisputePage_name
   else if PagedOutput.ActivePage = ExclusionSheet then
      helpfilename := configuration.HelpFiles_ExclusionPage_name
   else EUGeneError ('Programming error - did not see an appropriate paged output page name when "Help" selected.  Please notify programmer.', 3, continue, error_log);

   GenericHelpForm.present (helpfilename);
   GenericHelpForm.Showmodal;
end;

{-------------------------------------------------------------------------}

procedure TOutput_Options.RiskWTRCheckBoxClick(Sender: TObject);
begin
   {When this is checked, must uncheck the other risk selection.  They can only output 1.}
   {At this point, box is either checked if user clicked it, or unchecked if called
    from the other check box.}
   if RiskWTRCheckBox.checked = true then    {check the other checkbox}
      if (riskEUGENETaubox.checked = true) or (riskEUGENESbox.checked = true) or
         (uncertScheckbox.checked) or (uncertTaucheckbox.checked) or
         (riskdetailTauCheckbox.checked=true) or (riskdetailSCheckbox.checked=true) then
         begin
            riskEUGENETaubox.checked := false;
            riskEUGENESbox.checked := false;
            uncertTaucheckbox.checked := false;
            uncertScheckbox.checked := false;
            riskdetailTauCheckbox.checked := false;
            riskdetailSCheckbox.checked := false;
         end;
end;

{-------------------------------------------------------------------------}

procedure TOutput_Options.riskEUGENETauboxClick(Sender: TObject);
begin
   if riskEUGENETaubox.checked = true then    {check the other checkbox}
      if RiskWTRCheckBox.checked = true then
         RiskWTRCheckBox.checked := false;
end;

procedure TOutput_Options.riskEUGENESboxClick(Sender: TObject);
begin
   if riskEUGENESbox.checked = true then    {check the other checkbox}
      if RiskWTRCheckBox.checked = true then
         RiskWTRCheckBox.checked := false;
end;

{-----------------------------------------------------------------------------}

procedure TOutput_Options.COWMIDDisputeDataCheckBoxClick(Sender: TObject);
   var x : integer;
   begin
      if COWMIDDisputeDataCheckBox.checked = true then
         begin
            {also enable selection boxes}
            COWMIDDyadicpanel.enabled := true;
            COWMIDMonadicpanel.Enabled := true;
            for x := 0 to COWMIDDyadicpanel.controlcount-1 do
               COWMIDDyadicpanel.controls[x].enabled := true;
            for x := 0 to COWMIDMonadicpanel.controlcount-1 do
               COWMIDMonadicpanel.controls[x].enabled := true;
         end
         else
         begin   {if unselect COW MIDs, disable selection boxes}
            COWMIDDyadicpanel.enabled := false;
            COWMIDMonadicpanel.Enabled := false;
            for x := 0 to COWMIDDyadicpanel.controlcount-1 do
               COWMIDDyadicpanel.controls[x].enabled := false;
            for x := 0 to COWMIDMonadicpanel.controlcount-1 do
               COWMIDMonadicpanel.controls[x].enabled := false;
         end;
      COWMIDInitiationCheckBox.checked := true;
      COWMIDOngoingCheckBox.checked := true;
      COWMIDNumCheckBox.checked := true;
      COWMIDHostLevStateCheckBox.checked := true;
      {Also, when unchecked, disable the various exclusions and related options }
      Enable_Disable_initiation_settings;
   end;

procedure TOutput_Options.MaozDisputeDataCheckBoxClick(Sender: TObject);
   var x : integer;
   begin
      if MaozDisputeDataCheckBox.checked = true then
         begin
            MaozMIDDyadicpanel.enabled := true;
            MaozMIDMonadicpanel.Enabled := true;
            MaozMIDOnlyPanel.enabled := true;
            for x := 0 to MaozMIDDyadicpanel.controlcount-1 do
               MaozMIDDyadicpanel.controls[x].enabled := true;
            for x := 0 to MaozMIDMonadicpanel.controlcount-1 do
               MaozMIDMonadicpanel.controls[x].enabled := true;
            for x := 0 to MaozMIDOnlyPanel.controlcount-1 do
               MaozMIDOnlyPanel.controls[x].enabled := true;
         end
         else
         begin   {unselect maoz; disable all boxes}
            MaozMIDDyadicpanel.enabled := false;
            MaozMIDMonadicpanel.Enabled := false;
            MaozMIDOnlyPanel.enabled := false;
            for x := 0 to MaozMIDDyadicpanel.controlcount-1 do
               MaozMIDDyadicpanel.controls[x].enabled := false;
            for x := 0 to MaozMIDMonadicpanel.controlcount-1 do
               MaozMIDMonadicpanel.controls[x].enabled := false;
            for x := 0 to MaozMIDOnlyPanel.controlcount-1 do
               MaozMIDOnlyPanel.controls[x].enabled := false;
         end;
      MaozMIDInitiationCheckBox.checked := true;
      MaozMIDOngoingCheckBox.checked := true;
      MaozMIDNumCheckBox.checked := true;
      MaozMIDHostLevStateCheckBox.checked := true;
      {Also, when unchecked, disable the various exclusions and related options }
      Enable_Disable_initiation_settings;
   end;

procedure TOutput_Options.ICBDyadicDataCheckBoxClick(Sender: TObject);
   var x : integer;
   begin
      if ICBDyadicDataCheckBox.checked = true then
         begin
            ICBDyadicpanel.enabled := true;
            for x := 0 to ICBDyadicpanel.controlcount-1 do
               ICBDyadicpanel.controls[x].enabled := true;
            ICBMonadicpanel.Enabled := true;
            for x := 0 to ICBMonadicpanel.controlcount-1 do
               ICBMonadicpanel.controls[x].enabled := true;
         end
         else
         begin   {unselect ; disable all boxes}
            ICBDyadicpanel.enabled := false;
            for x := 0 to ICBDyadicpanel.controlcount-1 do
               ICBDyadicpanel.controls[x].enabled := false;
            ICBMonadicpanel.Enabled := false;
            for x := 0 to ICBMonadicpanel.controlcount-1 do
               ICBMonadicpanel.controls[x].enabled := false;
         end;
      ICBCrisisCheckBox.checked := true;
      ICBOngoingCheckBox.checked := true;
      ICBCrisisNumberCheckBox.checked := true;
      {Also, when unchecked, disable the various exclusions and related options }
      Enable_Disable_initiation_settings;

   end;

procedure TOutput_Options.COWDyadicWarsDataCheckBoxClick(Sender: TObject);
   var x : integer;
   begin
      if COWDyadicWarsDataCheckBox.checked = true then
         begin
            WarVariablePanel.enabled := true;
            for x := 0 to WarVariablepanel.controlcount-1 do
               WarVariablepanel.controls[x].enabled := true;
         end
         else
         begin   {unselect ; disable all boxes}
            WarVariablepanel.enabled := false;
            for x := 0 to WarVariablepanel.controlcount-1 do
               WarVariablepanel.controls[x].enabled := false;
         end;
      {Also, when unchecked, disable the various exclusions and related options }
      Enable_Disable_initiation_settings;

   end;

{-----------------------------------------------------------------------------}

procedure TOutput_Options.eq_options_buttonClick(Sender: TObject);
begin
   EU_Options_Window.EquilibriumSolutionPanel.visible := true;
   EU_Options_Window.showmodal;
end;

procedure TOutput_Options.system_variable_optionboxClick(Sender: TObject);
begin
   if sender = TauWithLeaderButton then
      begin
         SystemLeaderVariableOptions := TSystemLeaderVariableOptions.createwithitem (use_tau, self);
      end
   else
   if sender = SWithLeaderButton then
      begin
         SystemLeaderVariableOptions := TSystemLeaderVariableOptions.createwithitem (use_s, self);

      end
   else EUGeneError ('Program tried to call system variable option box, but sender was not s or tau box.  Notify programmer of error.', 1, stop, error_log);
   SystemLeaderVariableOptions.Top := 25;
   SystemLeaderVariableOptions.Left := 25;
   SystemLeaderVariableOptions.showmodal;
   SystemLeaderVariableOptions.free;
end;


procedure TOutput_Options.SamplingYesBtnClick(Sender: TObject);
begin
   SamplingPanel.enabled := true;
   NonDispSampleEdit.enabled := true;
   NonDisputeSampleLabel.enabled := true;
   disputedyadsamplelabel.enabled := true;
   DispSampleEdit.enabled := true;
   RandSeedCheckBox.enabled := true;
   RandomSeedEdit.enabled := true;
end;

procedure TOutput_Options.NoSampleBtnClick(Sender: TObject);
begin
   NonDispSampleEdit.enabled := false;
   NonDisputeSampleLabel.enabled := false;
   disputedyadsamplelabel.enabled := false;
   DispSampleEdit.enabled := false;
   RandSeedCheckBox.enabled := false;
   RandomSeedEdit.enabled := false;
end;


{procedure TOutput_Options.UserDataVarSelectionButtonClick(Sender: TObject);
var UserDataSelectionForm: TUserDataSelectionForm;

begin
   {need to pop up a window with variables included in user data files here}
{   try
      UserDataSelectionForm:= TUserDataSelectionForm.create(self);
      UserDataSelectionForm.showmodal;
      if UserDataSelectionForm.modalresult = mrOK then UserDataCheckBox.checked := true;
   finally
      UserDataSelectionForm.free;
   end;
end;
}

procedure TOutput_Options.RiskDetailTauCheckBoxClick(Sender: TObject);
begin
   if RiskDetailTauCheckBox.checked = true then    {check the other checkbox}
      if RiskWTRCheckBox.checked = true then
         RiskWTRCheckBox.checked := false;
end;

procedure TOutput_Options.RiskDetailSCheckBoxClick(Sender: TObject);
begin
   if RiskDetailSCheckBox.checked = true then    {check the other checkbox}
      if RiskWTRCheckBox.checked = true then
         RiskWTRCheckBox.checked := false;
end;

procedure TOutput_Options.UncertTauCheckBoxClick(Sender: TObject);
begin
   if UncertTauCheckBox.checked = true then    {check the other checkbox}
      if RiskWTRCheckBox.checked = true then
         RiskWTRCheckBox.checked := false;
end;

procedure TOutput_Options.UncertSCheckBoxClick(Sender: TObject);
begin
   if UncertSCheckBox.checked = true then    {check the other checkbox}
      if RiskWTRCheckBox.checked = true then
         RiskWTRCheckBox.checked := false;
end;

{
procedure TOutput_Options.AllianceButtonClick(Sender: TObject);
begin
   AllianceDataSourceForm.showmodal;
end;    }



procedure TOutput_Options.DistanceVarOptionsButtonClick(Sender: TObject);
var   DistanceOutputOptions: TDistanceOutputOptions;

begin
   try
      DistanceOutputOptions:= TDistanceOutputOptions.create(self);
      DistanceOutputOptions.showmodal;

      if ((user_selections.distance_method = capitols) or
          (user_selections.distance_method = capitols_contiguity) or
          (user_selections.distance_method = capitols_contiguity_war_trap) or
          (user_selections.distance_method = minimum))
      then DistanceVarBox.checked := true
      else DistanceVarBox.checked := false;
   finally
      DistanceOutputOptions.free;
   end;
end;

     { -----------------------------------------------  }

{There are a set of variables that they must have selected, either in MIDs (which are
 then output if they check the overall MID variable box),  or for year and ccode, which
 they just can't turn off.}

procedure TOutput_Options.CcodeVarBoxClick(Sender: TObject);
begin
   CcodeVarBox.checked := true;
end;

procedure TOutput_Options.YearVarBoxClick(Sender: TObject);
begin
   YearVarBox.checked := true;
end;

procedure TOutput_Options.COWMIDInitiationCheckBoxClick(Sender: TObject);
begin
   if COWMIDDisputeDataCheckBox.checked then
   COWMIDInitiationCheckBox.checked := true;
end;

procedure TOutput_Options.COWMIDOngoingCheckBoxClick(Sender: TObject);
begin
   if COWMIDDisputeDataCheckBox.checked then
   COWMIDOngoingCheckBox.checked := true;
end;

procedure TOutput_Options.COWMIDNumCheckBoxClick(Sender: TObject);
begin
   if COWMIDDisputeDataCheckBox.checked then
   COWMIDNumCheckBox.checked := true;
end;

procedure TOutput_Options.COWMIDHostLevStateCheckBoxClick(Sender: TObject);
begin
   if COWMIDDisputeDataCheckBox.checked then
   COWMIDHostLevStateCheckBox.checked := true;
end;

procedure TOutput_Options.MaozMIDInitiationCheckBoxClick(Sender: TObject);
begin
   if MaozDisputeDataCheckBox.checked then
   MaozMIDInitiationCheckBox.checked := true;
end;

procedure TOutput_Options.MaozMIDOngoingCheckBoxClick(Sender: TObject);
begin
   if MaozDisputeDataCheckBox.checked then
   MaozMIDOngoingCheckBox.checked := true;
end;

procedure TOutput_Options.MaozMIDNumCheckBoxClick(Sender: TObject);
begin
   if MaozDisputeDataCheckBox.checked then
   MaozMIDNumCheckBox.checked := true;
end;

procedure TOutput_Options.MaozMIDHostLevStateCheckBoxClick(Sender: TObject);
begin
   if MaozDisputeDataCheckBox.checked then
   MaozMIDHostLevStateCheckBox.checked := true;
end;

procedure TOutput_Options.ICBCrisisCheckBoxClick(Sender: TObject);
begin
   if ICBDyadicDataCheckBox.checked then
      ICBCrisisCheckBox.checked := true;
end;

procedure TOutput_Options.ICBOngoingCheckBoxClick(Sender: TObject);
begin
   if ICBDyadicDataCheckBox.checked then
      ICBOngoingCheckBox.checked := true;
end;

procedure TOutput_Options.ICBCrisisNumberCheckBoxClick(Sender: TObject);
begin
   if ICBDyadicDataCheckBox.checked then
   ICBCrisisNumberCheckBox.checked := true;
end;

procedure TOutput_Options.NewWarCheckBoxClick(Sender: TObject);
begin
   if COWDyadicWarsDataCheckBox.checked then
      NewWarCheckBox.Checked := true;
end;

procedure TOutput_Options.WarOngoingCheckBoxClick(Sender: TObject);
begin
   if COWDyadicWarsDataCheckBox.checked then
      WarOngoingCheckBox.checked := true;
end;

procedure TOutput_Options.ExcludeConflictWarsButtonClick(Sender: TObject);
begin
   Showmessage ('This option not yet enabled');
   {Enable_Disable_exclusions;}
end;

procedure TOutput_Options.ExcludeConflictCOWMIDButtonClick(Sender: TObject);
begin
   Enable_Disable_exclusions;
end;

procedure TOutput_Options.ExcludeConflictNoneButtonClick(Sender: TObject);
begin
   Enable_Disable_exclusions;
end;

procedure TOutput_Options.ExcludeConflictICBButtonClick(Sender: TObject);
begin
   Enable_Disable_exclusions;
end;

procedure TOutput_Options.ExcludeConflictMaozMIDButtonClick(Sender: TObject);
begin
   Enable_Disable_exclusions;
end;

procedure TOutput_Options.UseFirstMIDCheckBoxClick(Sender: TObject);
begin
   if UseFirstMIDCheckBox.checked then
      UseMostSeriousMIDCheckBox.checked := false
      else   {use first is unchecked, so set most serious to checked.}
      UseMostSeriousMIDCheckBox.checked := true;
end;

procedure TOutput_Options.UseMostSeriousMIDCheckBoxClick(Sender: TObject);
begin
   if UseMostSeriousMIDCheckBox.checked then
      UseFirstMIDCheckBox.checked := false
      else   {use serious is unchecked, so set first to checked.}
      UseFirstMIDCheckBox.checked := true;
end;

procedure TOutput_Options.PeaceYearsOptionsButton1Click(Sender: TObject);
begin
      if (PeaceYearsOptionForm.showmodal = mrOK) then COWPeaceYrsBox.checked := true;
end;
procedure TOutput_Options.PeaceYearsOptionsButton2Click(Sender: TObject);
begin
      if (PeaceYearsOptionForm.showmodal = mrOK) then MaozPeaceYrsBox.checked := true;
end;

procedure TOutput_Options.PeaceYearsOptionsButton3Click(Sender: TObject);
begin
      if (PeaceYearsOptionForm.showmodal = mrOK) then ICBPeaceYrsBox.checked := true;
end;



procedure TOutput_Options.PeaceYearsOptionsButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   PeaceYearsOptionsButton1.bevelinner := bvlowered;
   PeaceYearsOptionsButton2.bevelinner := bvlowered;
   PeaceYearsOptionsButton3.bevelinner := bvlowered;
end;

procedure TOutput_Options.PeaceYearsOptionsButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   PeaceYearsOptionsButton1.bevelinner := bvraised;
   PeaceYearsOptionsButton2.bevelinner := bvraised;
   PeaceYearsOptionsButton3.bevelinner := bvraised;
end;

procedure TOutput_Options.PolRelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   ChangePolRelButton.bevelinner := bvlowered;
end;

procedure TOutput_Options.PolRelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   ChangePolRelButton.bevelinner := bvraised;
end;

procedure TOutput_Options.DistOptionsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   DistanceOptionsButton.BevelInner := bvlowered;
end;

procedure TOutput_Options.DistOptionsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   DistanceOptionsButton.BevelInner := bvraised;
end;

procedure TOutput_Options.TauWithLeaderMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   TauWithLeaderButton.bevelinner := bvlowered;
end;

procedure TOutput_Options.TauWithLeaderMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   TauWithLeaderButton.BevelInner := bvraised;
end;

procedure TOutput_Options.SWithLeaderMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   SWithLeaderButton.BevelInner := bvlowered;
end;

procedure TOutput_Options.SWithLeaderMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   SWithLeaderButton.bevelinner := bvraised;
end;

procedure TOutput_Options.EquilibriumMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    EquilibriumButton.BevelInner := bvlowered;
end;

procedure TOutput_Options.EquilibriumMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    EquilibriumButton.BevelInner := bvraised;
end;

procedure TOutput_Options.ChangeOutputFileMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    ChangeOutputFileButton.bevelinner := bvlowered;
end;

procedure TOutput_Options.ChangeOutputFileMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    ChangeOutputFileButton.bevelinner := bvraised;
end;

procedure TOutput_Options.PrinterSetupMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   PrinterSetupButton.BevelInner := bvlowered;
end;

procedure TOutput_Options.PrinterSetupMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   PrinterSetupButton.BevelInner := bvraised;
end;

procedure TOutput_Options.Contiguity1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    ChangeContiguityButton1.BevelInner := bvlowered;
end;

procedure TOutput_Options.Contiguity1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    ChangeContiguityButton1.BevelInner := bvraised;
end;

procedure TOutput_Options.Contiguity2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    ChangeContiguityButton2.BevelInner := bvlowered;
end;

procedure TOutput_Options.Contiguity2MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    ChangeContiguityButton2.bevelinner := bvraised;
end;

procedure TOutput_Options.SelectRegionMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    SelectRegionButton.BevelInner := bvlowered;
end;

procedure TOutput_Options.SelectRegionMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    SelectRegionButton.bevelinner := bvraised;
end;

procedure TOutput_Options.SetCountriesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    SetCountriesButton.BevelInner := bvlowered;
end;

procedure TOutput_Options.SetCountriesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    SetCountriesButton.BevelInner := bvraised;
end;

procedure TOutput_Options.ChangeInputFileMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    ChangeInputFileButton.bevelinner := bvlowered;
end;

procedure TOutput_Options.ChangeInputFileMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    ChangeInputFileButton.BevelInner := bvraised;
end;

end.  {unit}






