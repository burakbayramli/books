unit TauOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, eutypes1, ExtCtrls;

type
  TSystemLeaderVariableOptions = class(TForm)
    OKBtn: TBitBtn;
    VarHelpBtn: TBitBtn;
    TauWLeaderPanel: TPanel;
    ScopeLabel: TLabel;
    regionalbutton: TRadioButton;
    globalbutton: TRadioButton;
    SWeightingPanel: TPanel;
    SWeightLabel: TLabel;
    SysLeaderSUnbtn: TRadioButton;
    SysLeaderSWtbtn: TRadioButton;
    procedure VarHelpBtnClick(Sender: TObject);
    procedure regionalbuttonClick(Sender: TObject);
    procedure globalbuttonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure SysLeaderSUnbtnClick(Sender: TObject);
    procedure SysLeaderSWtbtnClick(Sender: TObject);
  private
    sortau : stau_type;
    { Private declarations }
  public
    constructor createwithitem (SOrTauIn : stau_type; owner: TComponent);
    { Public declarations }
  end;

var
  SystemLeaderVariableOptions: TSystemLeaderVariableOptions;

implementation

{$R *.DFM}

constructor TSystemLeaderVariableOptions.createwithitem (SOrTauIn : stau_type; owner: TComponent);
begin
   inherited create(owner);
   self.Height := TauWLeaderPanel.Height + 32 + SWeightingPanel.height+ 12 +OKBtn.height + 12 + 50;
   self.Width := TauWLeaderPanel.Width + 60 ;
   sortau := sortauin;
   case sortau of
      use_s : begin
           ScopeLabel.caption := 'Scope for s calculation';
           regionalbutton.caption := 'Use regional subsystem s';
           globalbutton.caption := 'Use global system s';
           SysLeaderSWtbtn.visible := true;
           SysLeaderSWtbtn.enabled := true;
           SysLeaderSUnbtn.visible := true;
           SysLeaderSUnbtn.enabled := true;
           SWeightingPanel.visible := true;
         end;
      use_tau : begin
           ScopeLabel.caption := 'Scope for tau calculation';
           regionalbutton.caption := 'Use regional subsystem tau';
           globalbutton.caption := 'Use global system tau';
           SysLeaderSWtbtn.visible := false;
           SysLeaderSWtbtn.enabled := false;
           SysLeaderSUnbtn.visible := false;
           SysLeaderSUnbtn.enabled := false;
           SWeightingPanel.visible := false;
         end;
      end;
end;

procedure TSystemLeaderVariableOptions.VarHelpBtnClick(Sender: TObject);
begin
   ShowMessage ('Scores between the state(s) in question and the system leader '+
                'may be calculated either using the alliances of either 1) all states '+
                'in the international system, or 2) only the states in some region.  '+
                'If output is directed dyads, regional output uses the "relevant region" '+
                'between the states in question and the system leader.  If output is '+
                'non-directed dyads, regional output uses the home region of the state in question.');

end;

procedure TSystemLeaderVariableOptions.regionalbuttonClick(Sender: TObject);
begin
   case sortau of
      use_tau : user_selections.tau_leader_calculation_info := regional;
      use_s : user_selections.s_leader_calculation_info := regional;
   end;
end;

procedure TSystemLeaderVariableOptions.globalbuttonClick(Sender: TObject);
begin
   case sortau of
      use_tau : user_selections.tau_leader_calculation_info := global;
      use_s :  user_selections.s_leader_calculation_info := global;
   end;
end;

procedure TSystemLeaderVariableOptions.FormShow(Sender: TObject);
begin
   case sortau of
      use_tau : begin
            globalbutton.checked := (user_selections.tau_leader_calculation_info=global);
            regionalbutton.checked  := (user_selections.tau_leader_calculation_info=regional);
         end;
      use_s : begin
            globalbutton.checked := (user_selections.s_leader_calculation_info=global);
            regionalbutton.checked  := (user_selections.s_leader_calculation_info=regional);
            SysLeaderSUnbtn.checked := (user_selections.s_weighting = unweighted);
            SysLeaderSWtbtn.checked := (user_selections.s_weighting = weighted);
         end;
   end;   {case}
   if not globalbutton.checked or regionalbutton.checked then
      regionalbutton.checked := true;
end;


procedure TSystemLeaderVariableOptions.OKBtnClick(Sender: TObject);
begin
   close;
end;

procedure TSystemLeaderVariableOptions.SysLeaderSUnbtnClick(
  Sender: TObject);
begin
   user_selections.s_weighting := unweighted;
end;

procedure TSystemLeaderVariableOptions.SysLeaderSWtbtnClick(
  Sender: TObject);
begin
   user_selections.s_weighting := weighted;
end;

end.
