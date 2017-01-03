unit GABox;

{EUGene  Copyright 1997, 1998  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

uses
  Wintypes, winprocs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, Buttons, CmnProcD, EUTypes1, EUinoutD, ExtCtrls;

type
  TGAOptionBox = class(TForm)
    OKBtn: TBitBtn;
    CancBtn: TBitBtn;
    HelpBtn: TBitBtn;
    GeneticAlgorithmOptionsPanel: TPanel;
    GeneticAlgorithmOptionsLabel: TLabel;
    PopSizeLabel: TLabel;
    ProbMutateLabel: TLabel;
    ConvergenceLabel: TLabel;
    NumberToKeepLabel: TLabel;
    TolLabel: TLabel;
    ToleranceEditBox: TMaskEdit;
    PatternMaintainMaskBox: TMaskEdit;
    IterationsEdit: TMaskEdit;
    MutProbEdit: TMaskEdit;
    PopSizeEdit: TMaskEdit;
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CancBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GAOptionBox: TGAOptionBox;

implementation

   uses errbx;
   
{$R *.DFM}

procedure TGAOptionBox.OKBtnClick(Sender: TObject);
var errcode : integer;
begin
    {check values entered, and set user options accordingly.}

   val (ToleranceEditBox.edittext, user_selections.risk_calculation_info.risk_search_tolerance, errcode);
   if errcode <> 0 then {input was invalid}
      begin
         ShowMessage('Invalid risk tolerance input - setting tolerance to 0.01');
         user_selections.risk_calculation_info.risk_search_tolerance := 0.01;
      end;

    val (MutProbEdit.edittext, user_selections.risk_calculation_info.mutate_probability, errcode);
    if errcode <> 0 then {input was invalid}
      begin
         EUGeneError ('Invalid entry in GA Mutation Probability input - setting Probability to 0.05', 3, continue, error_log);
         user_selections.risk_calculation_info.mutate_probability := 0.05;
      end;

    val (PopSizeEdit.edittext, user_selections.risk_calculation_info.num_in_pop, errcode);
    if errcode <> 0 then {input was invalid}
      begin
         EUGeneError ('Invalid number in GA Population Size input - setting Population to 25', 3, continue, error_log);
         user_selections.risk_calculation_info.num_in_pop := 25;
      end;

    val (IterationsEdit.edittext, user_selections.risk_calculation_info.generations_to_be_stable, errcode);
    if errcode <> 0 then {input was invalid}
      begin
         EUGeneError ('Invalid entry in GA Stable-Generations input - setting generations to 8', 3, continue, error_log);
         user_selections.risk_calculation_info.generations_to_be_stable := 8;
      end;

    val (PatternMaintainMaskBox.edittext, user_selections.risk_calculation_info.num_from_previous_to_keep, errcode);
    if errcode <> 0 then {input was invalid}
      begin
         EUGeneError ('Invalid number in GA patterns to maintain input - setting maintained to 2', 3, continue, error_log);
         user_selections.risk_calculation_info.num_from_previous_to_keep := 2;
      end;
    if user_selections.risk_calculation_info.num_from_previous_to_keep > user_selections.risk_calculation_info.num_in_pop then
      begin
         EUGeneError ('Invalid number of patterns to maintain - greater than population size.  Set to to 2', 3, continue, error_log);
         user_selections.risk_calculation_info.num_from_previous_to_keep := 2;
      end;

    modalresult := mrOK;
end;

procedure TGAOptionBox.HelpBtnClick(Sender: TObject);
begin
   ShowMessage ('Enter values of parameters for Genetic Algorithm Optimization, then Press OK. '+
               ' Or, press "Cancel" to return to the previous window.');
end;

procedure TGAOptionBox.FormShow(Sender: TObject);
var TempString : string;
begin
    {Note:  When setting values in these mask boxes, need to be sure to set the length, because
     otherwise the maxlength will be set to the length of whatever values are initially here,
     when we want to leave it open for the user to have more digits if they want to.}
    Left := 1;
    Top := 1;
    TempString := '     ';
    str(user_selections.risk_calculation_info.mutate_probability:5:3, TempString);
    MutProbEdit.edittext := TempString;
    str (user_selections.risk_calculation_info.num_in_pop:PopSizeEdit.MaxLength, TempString);
    PopSizeEdit.edittext := TempString;
    str (user_selections.risk_calculation_info.generations_to_be_stable:IterationsEdit.MaxLength, TempString);
    IterationsEdit.edittext := TempString;
    str (user_selections.risk_calculation_info.num_from_previous_to_keep:PatternMaintainMaskBox.MaxLength, TempString);
    PatternMaintainMaskBox.edittext := TempString;
    TempString := '     ';
    str(user_selections.risk_calculation_info.risk_search_tolerance:5:3, TempString);
    ToleranceEditBox.edittext := TempString;
end;



procedure TGAOptionBox.CancBtnClick(Sender: TObject);
begin
   modalresult := mrCancel;
end;

end.
