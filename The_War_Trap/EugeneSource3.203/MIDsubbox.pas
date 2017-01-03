unit MIDsubbox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, EUTypes1;

type
  TMidSubInfoWindow = class(TForm)
    MIDGrpBox: TGroupBox;
    OKBtn: TBitBtn;
    MIDHelpBtn: TBitBtn;
    CountryDisputeGroupBox: TGroupBox;
    OverallDisputeGroupBox: TGroupBox;
    HostLevDisputeCheckBox: TCheckBox;
    HiActDisputeCheckBox: TCheckBox;
    FatalityLevelDisputeCheckBox: TCheckBox;
    startdatecheck: TCheckBox;
    enddatecheckbox: TCheckBox;
    SideACheckBox: TCheckBox;
    OriginatorCheckBox: TCheckBox;
    HostLevStateCheckBox: TCheckBox;
    HiActCheckBox: TCheckBox;
    FatalityLevelStateCheckBox: TCheckBox;
    RevTypeCheckBox: TCheckBox;
    RevStateCheckBox: TCheckBox;
    OutcomeCheckBox: TCheckBox;
    SettlementCheckBox: TCheckBox;
    MarkJoinersCheckBox: TCheckBox;
    RecipCheckBox: TCheckBox;
    NumOfStatesBox: TCheckBox;
    DyadicDisputeGroupBox1: TGroupBox;
    InitiationCheckBox: TCheckBox;
    OngoingCheckBox: TCheckBox;
    MIDNumCheckBox: TCheckBox;
    maozcowwarbox: TCheckBox;
    maozdurindxbox: TCheckBox;
    maozdurdaysbox: TCheckBox;
    maozreciprocateddyadicbox: TCheckBox;
    procedure MIDHelpBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure InitiationCheckBoxClick(Sender: TObject);
    procedure OngoingCheckBoxClick(Sender: TObject);
    procedure HostLevStateCheckBoxClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MidSubInfoWindow: TMidSubInfoWindow;

implementation

uses PagedOutput;

{$R *.DFM}

procedure TMidSubInfoWindow.MIDHelpBtnClick(Sender: TObject);
begin
   ShowMessage ('Select which MID variables you want to appear in your output data file.  ');
end;

procedure TMidSubInfoWindow.OKBtnClick(Sender: TObject);
  {A few things are always checked, initiation, ongoing, and hostility level, and are
   always sent to output.  So, don't have to check those.}
begin
   with user_selections.output_format do
   begin
      if MidNumCheckBox.checked then
         include (variables, MIDNumber)
         else exclude (variables, MIDNumber);

      if startdatecheck.checked then
         include (variables, MIDStart)
         else exclude (variables, MIDStart);

      if enddatecheckbox.checked then
         include (variables, MIDEnd)
         else exclude (variables, MIDEnd);

      if SideACheckBox.checked then
         include (variables, MIDSideA)
         else exclude (variables, MIDSideA);

      if RevStateCheckBox.checked then
         include (variables, MIDRevisionist)
         else exclude (variables, MIDRevisionist);

      if RevTypeCheckBox.checked then
         include (variables, MIDRevisiontype)
         else exclude (variables, MIDRevisiontype);

      if FatalityLevelStateCheckBox.checked then
         include (variables, MIDFatalityState)
         else exclude (variables, MIDFatalityState);

      if HiActCheckBox.checked then
         include (variables, MIDHiActState)
         else exclude (variables, MIDHiActState);

      if OriginatorCheckBox.checked then
         include (variables, MIDOriginator)
         else exclude (variables, MIDOriginator);

      if OutcomeCheckBox.checked then
         include (variables, MIDOutcome)
         else exclude (variables, MIDOutcome);

      if SettlementCheckBox.checked then
         include (variables, MIDSettlement)
         else exclude (variables, MIDSettlement);

      if FatalityLevelDisputeCheckBox.checked then
         include (variables, MIDFatalityDispute)
         else exclude (variables, MIDFatalityDispute);

      if HiActDisputeCheckBox.checked then
         include (variables, MIDHiActDispute)
         else exclude (variables, MIDHiActDispute);

      if HostLevDisputeCheckBox.checked then
         include (variables, MIDHostLevDispute)
         else exclude (variables, MIDHostLevDispute);

      if RecipCheckBox.checked then
         include (variables, MIDReciprocated)
         else exclude (variables, MIDReciprocated);

      if NumOfStatesBox.checked then
         include (variables, MIDNumStates)
         else exclude (variables, MIDNumStates);

      if MarkJoinersCheckBox.checked then
         include (variables, MarkMIDJoiners)
         else exclude (variables, MarkMIDJoiners);

      if MaozCowWarBox.checked then
         include (variables, MaozCOWWar)
         else exclude (variables, MaozCOWWar);

      if MaozDurIndxBox.checked then
         include (variables, MaozDurindx)
         else exclude (variables, MaozDurindx);

      if MaozDurDaysBox.checked then
         include (variables, MaozDurDays)
         else exclude (variables, MaozDurDays);

      if MaozReciprocatedDyadicBox.checked then
         include (variables, MaozReciprocatedDyadic)
         else exclude (variables, MaozReciprocatedDyadic);

      {now, if MIDs weren't already marked, mark it on the main variable sheet as COW MIDs}
      if not (Output_Options.COWMIDDisputeDataCheckBox.checked or Output_Options.MaozDisputeDataCheckBox.checked) then
              Output_Options.COWMIDDisputeDataCheckBox.checked := true;
   end;   {with}
   close;
end;

procedure TMidSubInfoWindow.InitiationCheckBoxClick(Sender: TObject);
begin
  InitiationCheckBox.checked := true;
end;

procedure TMidSubInfoWindow.OngoingCheckBoxClick(Sender: TObject);
begin
   OngoingCheckBox.checked := true;
end;

procedure TMidSubInfoWindow.HostLevStateCheckBoxClick(Sender: TObject);
begin
   HostLevStateCheckBox.checked := true;
end;

procedure TMidSubInfoWindow.FormShow(Sender: TObject);
begin
   if Output_Options.MaozDisputeDataCheckBox.checked then
      begin
         MaozCowWarBox.enabled := true;
         MaozDurIndxBox.enabled := true;
         MaozDurDaysBox.enabled := true;
         MaozReciprocatedDyadicBox.enabled := true;
      end
   else    {cow, or no disputes selected, don't show maoz options}
      begin
         MaozCowWarBox.enabled := false;
         MaozDurIndxBox.enabled := false;
         MaozDurDaysBox.enabled := false;
         MaozReciprocatedDyadicBox.enabled := false;
      end;

   {Set check boxes for vars already selected}
   MidNumCheckBox.checked := (MIDNumber in user_selections.output_format.variables);
   startdatecheck.checked := (MIDStart in user_selections.output_format.variables);
   enddatecheckbox.checked := (MIDEnd in user_selections.output_format.variables);
   SideACheckBox.checked := (MIDSideA in user_selections.output_format.variables);
   RevStateCheckBox.checked := (MIDRevisionist in user_selections.output_format.variables);
   RevTypeCheckBox.checked := (MIDRevisiontype in user_selections.output_format.variables);
   FatalityLevelStateCheckBox.checked := (MIDFatalityState in user_selections.output_format.variables);
   HiActCheckBox.checked := (MIDHiActState in user_selections.output_format.variables);
   OriginatorCheckBox.checked := (MIDOriginator in user_selections.output_format.variables);
   OutcomeCheckBox.checked := (MIDOutcome in user_selections.output_format.variables);
   SettlementCheckBox.checked := (MIDSettlement in user_selections.output_format.variables);
   FatalityLevelDisputeCheckBox.checked := (MIDFatalityDispute in user_selections.output_format.variables);
   HiActDisputeCheckBox.checked := (MIDHiActDispute in user_selections.output_format.variables);
   HostLevDisputeCheckBox.checked := (MIDHostLevDispute in user_selections.output_format.variables);
   RecipCheckBox.checked := (MIDReciprocated in user_selections.output_format.variables);
   NumOfStatesBox.checked := (MIDNumStates in user_selections.output_format.variables);
   MarkJoinersCheckBox.checked := (MarkMIDJoiners in user_selections.output_format.variables);
   MaozCowWarBox.checked := (MaozCOWWar in user_selections.output_format.variables);
   MaozDurIndxBox.checked := (MaozDurindx in user_selections.output_format.variables);
   MaozDurDaysBox.checked := (MaozDurDays in user_selections.output_format.variables);
   MaozReciprocatedDyadicBox.checked := (MaozReciprocatedDyadic in user_selections.output_format.variables);

end;

end.
