unit EUOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TEU_Options_Window = class(TForm)
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    GroupBox1: TGroupBox;
    OriginalIIGVariantButton: TRadioButton;
    NoWarVariantButton: TRadioButton;
    NoForceVariantButton: TRadioButton;
    Node3ForwardVariantButton: TRadioButton;
    PhiGroupBox: TGroupBox;
    PhiAdjustedButton: TRadioButton;
    PhiNotAdjustedButton: TRadioButton;
    EquilibriumSolutionPanel: TPanel;
    EquilibriumSolutionLabel: TLabel;
    InductionButton: TRadioButton;
    LogicalSolutionButton: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure InductionButtonClick(Sender: TObject);
    procedure LogicalSolutionButtonClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure OriginalIIGVariantButtonClick(Sender: TObject);
    procedure NoWarVariantButtonClick(Sender: TObject);
    procedure NoForceVariantButtonClick(Sender: TObject);
    procedure Node3ForwardVariantButtonClick(Sender: TObject);
    procedure PhiAdjustedButtonClick(Sender: TObject);
    procedure PhiNotAdjustedButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EU_Options_Window: TEU_Options_Window;

implementation

uses eutypes1;

{$R *.DFM}

procedure TEU_Options_Window.FormShow(Sender: TObject);
begin
      {this version has no phi adjustment}
    PhiAdjustedButton.checked := (user_selections.eu_calculation_info.adjusted_phi=true);
    PhiNotAdjustedButton.checked  := (user_selections.eu_calculation_info.adjusted_phi=false);
    {To get back, load project file EUOptionsWithAdjustPhi.pas;  But also check file
     PagedOutput which hid the buttons for Phi Adjustment.}
    LogicalSolutionButton.checked := (user_selections.eu_calculation_info.equilibrium_solution=logical);
    InductionButton.checked := (user_selections.eu_calculation_info.equilibrium_solution=induction);
    OriginalIIGVariantButton.checked := (user_selections.eu_calculation_info.game_variant=original);
    NoWarVariantButton.checked := (user_selections.eu_calculation_info.game_variant=NoWar);
    NoForceVariantButton.checked := (user_selections.eu_calculation_info.game_variant=NoForce);
    Node3ForwardVariantButton.checked := (user_selections.eu_calculation_info.game_variant=Node3Forward);
end;

procedure TEU_Options_Window.HelpBtnClick(Sender: TObject);
begin
   ShowMessage ('War and Reason provides logical complete information conditions for game equilibrium solutions.  EUGene can calculate equilibria using those logical conditions, or by '+
                'backwards induction using computed utility values for each dyad-year.  ' );
{   ShowMessage ('1) In War and Reason, Phi ranges from -1 to +1.  This causes problems in satisfying basic game conditions.  '+
                'To adjust Phi to be between 0 and +1, choose "Adjusted Phi".  2) War and Reason provides logical complete information '+
                'conditions for game equilibrium solutions.  EUGene can calculate equilibria using those logical conditions, or by '+
                'backwards induction using computed utility values for each dyad-year.  If phi is adjusted, these produce the same results.' +
                '3) You may select a game variant following Bennett (2001, APSA paper).  For the original IIG, select "Original" and be sure to use the unadjusted phi.');
}
end;

procedure TEU_Options_Window.OKBtnClick(Sender: TObject);
begin
   Close;
end;


procedure TEU_Options_Window.InductionButtonClick(Sender: TObject);
begin
   user_selections.eu_calculation_info.equilibrium_solution := induction;
end;

procedure TEU_Options_Window.LogicalSolutionButtonClick(Sender: TObject);
begin
   user_selections.eu_calculation_info.equilibrium_solution := logical;
end;


procedure TEU_Options_Window.OriginalIIGVariantButtonClick(Sender: TObject);
begin
   InductionButton.enabled := true;
   LogicalSolutionButton.enabled := true;
   user_selections.eu_calculation_info.game_variant := original;
end;

procedure TEU_Options_Window.NoWarVariantButtonClick(Sender: TObject);
begin
   InductionButton.enabled := true;
   InductionButtonClick(self);
   InductionButton.Checked := true;
   LogicalSolutionButton.enabled := false;
   user_selections.eu_calculation_info.game_variant := NoWar;
end;

procedure TEU_Options_Window.NoForceVariantButtonClick(Sender: TObject);
begin
   InductionButton.enabled := true;
   InductionButtonClick(self);
   InductionButton.Checked := true;
   LogicalSolutionButton.enabled := false;
   user_selections.eu_calculation_info.game_variant := NoForce;
end;

procedure TEU_Options_Window.Node3ForwardVariantButtonClick(Sender: TObject);
begin
   InductionButton.enabled := true;
   InductionButtonClick(self);
   InductionButton.Checked := true;
   LogicalSolutionButton.enabled := false;
   user_selections.eu_calculation_info.game_variant := Node3Forward;
end;


procedure TEU_Options_Window.PhiAdjustedButtonClick(Sender: TObject);
begin
    user_selections.eu_calculation_info.adjusted_phi:=true;
end;

procedure TEU_Options_Window.PhiNotAdjustedButtonClick(Sender: TObject);
begin
    user_selections.eu_calculation_info.adjusted_phi:=false;
end;

end.
