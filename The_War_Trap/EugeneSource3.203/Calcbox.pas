unit Calcbox;

{EUGene  Copyright 1997, 1998 D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons,
  cmnprocd, euinoutd, eutypes1, eutypes2, euprocs1, euprocs2, Mask,
  GABox, SteepBox, RandBox, ExtCtrls;

type
  TCalculation_box = class(TForm)
    Distancebox: TGroupBox;
    Cap_contbox: TRadioButton;
    Cap_cont_wtbox: TRadioButton;
    Cap_to_capbox: TRadioButton;
    riskbox: TGroupBox;
    controlbox: TGroupBox;
    OKBtn: TBitBtn;
    CancBtn: TBitBtn;
    HlpBtn: TBitBtn;
    NoDiscountButton: TRadioButton;
    AllianceDataSourceGroupBox: TGroupBox;
    DyadicInputDataButton: TRadioButton;
    SequenceNumAllianceDataButton: TRadioButton;
    TitlePanel: TPanel;
    CalculationLabel: TLabel;
    TimeLabel: TLabel;
    SteepestDescentButton: TRadioButton;
    GeneticAlgButton: TRadioButton;
    RandomWalkButton: TRadioButton;
    RiskDataSourceGroupBox: TGroupBox;
    RiskEugeneButton: TRadioButton;
    RiskWTRButton: TRadioButton;
    CapabilityChoiceBox: TGroupBox;
    CapabilitiesAsCowButton: TRadioButton;
    DropEnergyButton: TRadioButton;
    ModifiedCapabilityButton: TRadioButton;
    SimilaritySourceBox: TGroupBox;
    TauButton: TRadioButton;
    SScoreButton: TRadioButton;
    SteepestSettingsButton: TButton;
    GASettingsButton: TButton;
    RandomWalkSettingsButton: TButton;
    procedure CaptoCapClick(Sender: TObject);
    procedure Cap_contboxClick(Sender: TObject);
    procedure Cap_cont_wtboxClick(Sender: TObject);
    procedure OnShowCalcBox(Sender: TObject);
    procedure NoDiscountButtonClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure HlpBtnClick(Sender: TObject);
    procedure CancBtnClick(Sender: TObject);
    procedure DyadicInputDataButtonClick(Sender: TObject);
    procedure SequenceNumAllianceDataButtonClick(Sender: TObject);
    procedure GeneticAlgButtonClick(Sender: TObject);
    procedure SteepestDescentButtonClick(Sender: TObject);
    procedure RandomWalkButtonClick(Sender: TObject);
    procedure RiskEugeneButtonClick(Sender: TObject);
    procedure RiskWTRButtonClick(Sender: TObject);
    procedure CapabilitiesAsCowButtonClick(Sender: TObject);
    procedure DropEnergyButtonClick(Sender: TObject);
    procedure ModifiedCapabilityButtonClick(Sender: TObject);
    procedure TauButtonClick(Sender: TObject);
    procedure SScoreButtonClick(Sender: TObject);
    procedure SteepestSettingsButtonClick(Sender: TObject);
    procedure GASettingsButtonClick(Sender: TObject);
    procedure RandomWalkSettingsButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Calculation_box: TCalculation_box;

implementation

{$R *.DFM}
uses
    mdiframe, GenrHelp, SWeightBox, errbx;

{-----------------------------------------------------------------------}

procedure TCalculation_box.CaptoCapClick(Sender: TObject);
begin
   user_selections.distance_method := capitols;
end;

{-----------------------------------------------------------------------}

procedure TCalculation_box.Cap_contboxClick(Sender: TObject);
begin
   user_selections.distance_method := capitols_contiguity;
end;

{---------------------------------------------------------------------}

procedure TCalculation_box.Cap_cont_wtboxClick(Sender: TObject);
begin
  user_selections.distance_method := capitols_contiguity_war_trap;
end;

{------------------------------------------------------------------------}

procedure TCalculation_box.NoDiscountButtonClick(Sender: TObject);
begin
  user_selections.distance_method := nodiscount;
end;

{------------------------------------------------------------------------}

procedure TCalculation_box.OnShowCalcBox(Sender: TObject);
begin
     Scaled:=true;

     CalculationLabel.Font.style := [fsbold];
    { CalculationLabel.Font.size := Calculation_Box.font.size + 1;}
     TimeLabel.Font.style := {TimeLabel.font.style + }[fsbold];

     {Set buttons on form to existing values}

     Cap_to_capbox.checked := (user_selections.distance_method = capitols);
     Cap_contbox.checked := (user_selections.distance_method = capitols_contiguity);
     Cap_cont_wtbox.checked := (user_selections.distance_method = capitols_contiguity_war_trap);
     NoDiscountButton.checked := (user_selections.distance_method = nodiscount);

     CapabilitiesAsCowButton.checked := (user_selections.capability_modifications = COW_only);
     DropEnergyButton.checked := (user_selections.capability_modifications = no_energy);
     ModifiedCapabilityButton.checked := (user_selections.capability_modifications = modified_capability);

     DyadicInputDataButton.checked := (user_selections.alliance_data_source = flat_dyadic);
     {SequenceNumAllianceDataButton.checked := (user_selections.alliance_data_source = flat_cow_sequence);}

     RiskEugeneButton.checked := (user_selections.risk_data_source = risk_EUGENE);
     RiskWTRButton.checked := (user_selections.risk_data_source = risk_WTR);

     SteepestDescentButton.checked := (user_selections.risk_calculation_info.method = use_steepest);
     GeneticAlgButton.checked := (user_selections.risk_calculation_info.method = use_ga);
     RandomWalkButton.checked := (user_selections.risk_calculation_info.method = use_random_walk);


     if (user_selections.compute_this = compute_syscap)then
        begin
          CalculationLabel.Caption := 'Calculation Chosen: Percent System Capabilities';
          DistanceBox.enabled := false;
          DistanceBox.visible := false;
          CapabilityChoiceBox.enabled := true;
          CapabilityChoiceBox.visible := true;
          Riskbox.enabled := false;
          Riskbox.visible := false;
          AllianceDataSourceGroupBox.enabled := false;
          AllianceDataSourceGroupBox.visible := false;
          RiskDataSourceGroupBox.enabled := false;
          RiskDataSourceGroupBox.visible := false;
          SimilaritySourceBox.enabled := false;
          SimilaritySourceBox.visible := false;
        end

     else if (user_selections.compute_this = compute_tau) then
        begin
          CalculationLabel.Caption := 'Calculation Chosen: Tau-b Scores';
          CapabilityChoiceBox.enabled := false;
          CapabilityChoiceBox.visible := false;
          DistanceBox.enabled := false;
          DistanceBox.visible := false;
          Riskbox.enabled := false;
          Riskbox.visible := false;
          AllianceDataSourceGroupBox.enabled := false;
          AllianceDataSourceGroupBox.visible := false;
          RiskDataSourceGroupBox.enabled := false;
          RiskDataSourceGroupBox.visible := false;
          SimilaritySourceBox.enabled := false;
          SimilaritySourceBox.visible := false;
        end

     else if (user_selections.compute_this = compute_s) then
        begin
          CalculationLabel.Caption := 'Calculation Chosen: S similarity index';
          CapabilityChoiceBox.enabled := false;
          CapabilityChoiceBox.visible := false;
          DistanceBox.enabled := false;
          DistanceBox.visible := false;
          Riskbox.enabled := false;
          Riskbox.visible := false;
          AllianceDataSourceGroupBox.enabled := false;
          AllianceDataSourceGroupBox.visible := false;
          RiskDataSourceGroupBox.enabled := false;
          RiskDataSourceGroupBox.visible := false;
          SimilaritySourceBox.enabled := false;
          SimilaritySourceBox.visible := false;
        end

     else if (user_selections.compute_this = compute_EUWarTrap) then
        begin
           CalculationLabel.Caption := 'Calculation Chosen: EU (War Trap)';
           CapabilityChoiceBox.enabled := false;
           CapabilityChoiceBox.visible := false;
           DistanceBox.enabled := true;
           DistanceBox.visible := true;
           Riskbox.enabled := false;
           Riskbox.visible := false;
           AllianceDataSourceGroupBox.enabled := false;
           AllianceDataSourceGroupBox.visible := false;
           RiskDataSourceGroupBox.enabled := false;
           RiskDataSourceGroupBox.visible := false;
           SimilaritySourceBox.enabled := true;
           SimilaritySourceBox.visible := true;
        end

     else if ((user_selections.compute_this = compute_risk) or
              (user_selections.compute_this = compute_single_risk)) then
        begin
           CalculationLabel.Caption := 'Calculation Chosen: Risk Attitude';
           CapabilityChoiceBox.enabled := false;
           CapabilityChoiceBox.visible := false;
           DistanceBox.enabled := true;
           DistanceBox.visible := true;
           Riskbox.enabled := true;
           Riskbox.visible := true;
           AllianceDataSourceGroupBox.enabled := false;
           AllianceDataSourceGroupBox.visible := false;
           RiskDataSourceGroupBox.enabled := false;
           RiskDataSourceGroupBox.visible := false;
           SimilaritySourceBox.enabled := true;
           SimilaritySourceBox.visible := true;
        end

     else if (user_selections.compute_this = compute_EUWarReasonProb) then
        begin
           CalculationLabel.Caption := 'Calculation Chosen: EU (War and Reason)';
           CapabilityChoiceBox.enabled := false;
           CapabilityChoiceBox.visible := false;
           DistanceBox.enabled := true;
           DistanceBox.visible := true;
           Riskbox.enabled := false;
           Riskbox.visible := false;
           AllianceDataSourceGroupBox.enabled := false;
           AllianceDataSourceGroupBox.visible := false;
           RiskDataSourceGroupBox.enabled := true;
           RiskDataSourceGroupBox.visible := true;
           SimilaritySourceBox.enabled := true;
           SimilaritySourceBox.visible := true;
       end

     else
         begin
           CalculationLabel.Caption := 'Calculation Chosen: None';
           CapabilityChoiceBox.enabled := false;
           CapabilityChoiceBox.visible := false;
           DistanceBox.enabled := false;
           DistanceBox.visible := false;
           Riskbox.enabled := false;
           Riskbox.visible := false;
           AllianceDataSourceGroupBox.enabled := false;
           AllianceDataSourceGroupBox.visible := false;
           RiskDataSourceGroupBox.enabled := false;
           RiskDataSourceGroupBox.visible := false;
           SimilaritySourceBox.enabled := false;
           SimilaritySourceBox.visible := false;
         end;

      {Now set the displayed marks in the check boxes and radio dials}
   {calculations box}
       Cap_to_capbox.checked := (user_selections.distance_method = capitols);
       Cap_contbox.checked := (user_selections.distance_method = capitols_contiguity);
       Cap_cont_wtbox.checked := (user_selections.distance_method = capitols_contiguity_war_trap);
       NoDiscountButton.checked := (user_selections.distance_method = nodiscount);
       DyadicInputDataButton.checked := (user_selections.alliance_data_source = flat_dyadic);
       {SequenceNumAllianceDataButton.checked := (user_selections.alliance_data_source = flat_cow_sequence);}
       CapabilitiesAsCowButton.checked := (user_selections.capability_modifications = COW_only);
       DropEnergyButton.checked := (user_selections.capability_modifications = no_energy);
       ModifiedCapabilityButton.checked := (user_selections.capability_modifications = modified_capability);
       TauButton.checked := user_selections.similarity_method = use_tau;
       SScoreButton.checked := user_selections.similarity_method = use_s;
end;


procedure TCalculation_box.OKBtnClick(Sender: TObject);
   var OKToExecute : boolean;
begin

   {Were necessary selections made? }
   OKToExecute := true;
   if user_selections.compute_this = compute_risk then
     begin
        if not (SteepestDescentButton.checked or GeneticAlgButton.checked or
                RandomWalkButton.checked) then
           begin
              OKToExecute := false;
              ShowMessage ('You must select an algorithm for Risk Recomputation.  '+
                           'Please select one and press "OK", or choose "Cancel" to '+
                           'return to the main EUGene screen.');
           end;
     end;

   if ((user_selections.compute_this = compute_EUWarTrap) or
       (user_selections.compute_this = compute_EUWarReasonProb) or
       (user_selections.compute_this = compute_risk) or
       (user_selections.compute_this = compute_single_risk)) then
     begin
        if not (Cap_to_capbox.checked or Cap_contbox.checked or
                Cap_cont_wtbox.checked or NoDiscountButton.checked) then
                {distance not selected; maybe they selected minimum elsewhere}
           begin
              ShowMessage ('You must select an acceptable distance method for Recalculation (one of the 4 shown on this screen).  '+
                           'Currently the "minimum distance" option is not supported for utility calculation.  '+
                           'Please select one and press "OK", or choose "Cancel" to '+
                           'return to the main EUGene screen.');
              OKToExecute := false;
           end;

     end;

   {All options other than risk algorithm and parts that use distance have a default,
    so they don't have to be checked.}

   if OKToExecute then
      begin
         modalResult := MROk;
      end
   else modalResult := MRNone;

end;

{--------------------------------------------------------}

procedure TCalculation_box.HlpBtnClick(Sender: TObject);
begin
     GenericHelpForm.present (configuration.HelpFiles_RecalcBox_name);
     GenericHelpForm.Showmodal;
end;

{---------------------------------------------------------}

procedure TCalculation_box.CancBtnClick(Sender: TObject);
begin
   Close;
end;

{------------------------------------------------------------}

procedure TCalculation_box.DyadicInputDataButtonClick(Sender: TObject);
begin
   user_selections.alliance_data_source := flat_dyadic;
end;

{----------------------------------------------------------}

procedure TCalculation_box.SequenceNumAllianceDataButtonClick(
  Sender: TObject);
begin
   {user_selections.alliance_data_source := flat_cow_sequence;}
   EUGeneError ('Seqnum alliance button pushed, but seqnum alliances have been eliminated.  Programming error.  Fatal. ',1,stop,error_log);
end;

{------------------------------------------------------------}

procedure TCalculation_box.GeneticAlgButtonClick(Sender: TObject);
begin
   user_selections.risk_calculation_info.method := use_ga;
end;

{------------------------------------------------------------}

procedure TCalculation_box.SteepestDescentButtonClick(Sender: TObject);
begin
   user_selections.risk_calculation_info.method := use_steepest;
end;

{---------------------------------------------------------}

procedure TCalculation_box.RandomWalkButtonClick(Sender: TObject);
begin
   user_selections.risk_calculation_info.method := use_random_walk;
end;

{------------------------------------------------------------}

procedure TCalculation_box.RiskEugeneButtonClick(Sender: TObject);
begin
   user_selections.risk_data_source := risk_EUGENE;
   TauButton.checked := true;
   TauButton.enabled := true;
   SScoreButton.enabled := true;
end;

{----------------------------------------------------------}

procedure TCalculation_box.RiskWTRButtonClick(Sender: TObject);
begin
   user_selections.risk_data_source := risk_WTR;
   TauButton.checked := true;
   TauButton.enabled := false;
   SScoreButton.enabled := false;
end;

procedure TCalculation_box.CapabilitiesAsCowButtonClick(Sender: TObject);
begin
    user_selections.capability_modifications := COW_only;
end;

procedure TCalculation_box.DropEnergyButtonClick(Sender: TObject);
begin
    user_selections.capability_modifications := no_energy;
end;

procedure TCalculation_box.ModifiedCapabilityButtonClick(Sender: TObject);
begin
    user_selections.capability_modifications := modified_capability;
end;

procedure TCalculation_box.TauButtonClick(Sender: TObject);
begin
   user_selections.similarity_method := use_tau;
end;

procedure TCalculation_box.SScoreButtonClick(Sender: TObject);
begin
   user_selections.similarity_method := use_s;
   SimilarityWeightingForm.showmodal;
end;

procedure TCalculation_box.SteepestSettingsButtonClick(Sender: TObject);
begin
   SteepestSubForm.showModal;
   if SteepestSubForm.modalResult = mrOK then SteepestDescentButton.checked := true;
end;

procedure TCalculation_box.GASettingsButtonClick(Sender: TObject);
begin
   if GAOptionBox.ShowModal = mrOK then
      begin
         GeneticAlgButton.checked := true;
         user_selections.risk_calculation_info.method := use_ga;
      end;
end;

procedure TCalculation_box.RandomWalkSettingsButtonClick(Sender: TObject);
begin
   RandomIterationForm.ShowModal;
   if RandomIterationForm.modalResult = mrOK then RandomWalkButton.checked := true;
end;

procedure TCalculation_box.FormCreate(Sender: TObject);
begin
   self.height := TitlePanel.height + CapabilityChoiceBox.height + RiskDataSourceGroupBox.height + riskbox.height + SimilaritySourceBox.height + 10+11*5;
   self.width := CapabilityChoiceBox.Width + 24;
end;


end.
