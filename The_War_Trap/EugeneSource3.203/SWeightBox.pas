unit SWeightBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, GenrHelp, eutypes1, euinoutd, ExtCtrls;

type
  TSimilarityWeightingForm = class(TForm)
    WeightOKBtn: TBitBtn;
    WeightCancelBtn: TBitBtn;
    WeightHelpBtn: TBitBtn;
    SimilarityWeightPanel: TPanel;
    WeightCapabilitiesButton: TRadioButton;
    NoWeightingButton: TRadioButton;
    procedure WeightHelpBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure WeightOKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SimilarityWeightingForm: TSimilarityWeightingForm;

implementation

{$R *.DFM}

procedure TSimilarityWeightingForm.WeightHelpBtnClick(Sender: TObject);
begin
     GenericHelpForm.present (configuration.HelpFiles_RecalcBox_name);
     GenericHelpForm.Showmodal;
end;

procedure TSimilarityWeightingForm.FormShow(Sender: TObject);
begin
   WeightCapabilitiesButton.checked := user_selections.s_weighting = weighted;
   NoWeightingButton.checked := user_selections.s_weighting = unweighted;
end;

procedure TSimilarityWeightingForm.WeightOKBtnClick(Sender: TObject);
begin
   if WeightCapabilitiesButton.checked then user_selections.s_weighting := weighted else
               user_selections.s_weighting := unweighted;
   {case user_selections.compute_this  of
      compute_EUWarReasonProb : begin
            if WeightCapabilitiesButton.checked then user_selections.EU_calculation_info.s_weighting := use_capabilities else
               user_selections.EU_calculation_info.s_weighting := no_s_weight;
         end;
      compute_risk : begin
            if WeightCapabilitiesButton.checked then user_selections.risk_calculation_info.s_weighting := use_capabilities else
               user_selections.risk_calculation_info.s_weighting := no_s_weight;
         end;
      else EUGeneError ('Error in showing weighting options on similarity score method.  Computation selection not risk or EU.', 5, continue, error_log);
      end;   {case}

end;

end.
