unit RiskGenBox;

interface

uses
  eutypes1, Setting, OutWindow, Printers, 
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TRiskGenForm = class(TForm)
    OK: TBitBtn;
    Cancel: TBitBtn;
    Reset: TBitBtn;
    Helpbtn: TBitBtn;
    RiskGenRangePanel: TPanel;
    RiskGenRangeLabel: TLabel;
    From_Year: TLabel;
    To_Year: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    AllianceYrs: TLabel;
    CapYrs: TLabel;
    RiskYr: TLabel;
    EUYr: TLabel;
    EUYrFrom: TLabel;
    RiskYrFrom: TLabel;
    CapYrFrom: TLabel;
    AllYrFrom: TLabel;
    AllYrTo: TLabel;
    CApYrTo: TLabel;
    RiskYrTo: TLabel;
    EUYrTo: TLabel;
    OutputFileChoicePanel: TPanel;
    OutputFileChoiceLabel: TLabel;
    OverwriteInButton: TRadioButton;
    CreateNewFileButton: TRadioButton;
    Label1: TLabel;
    procedure OKClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure HelpbtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OverwriteInButtonClick(Sender: TObject);
    procedure CreateNewFileButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Year1, year2 : year_range;
    newRiskFile, OverwriteRiskRecords : boolean;
  end;

var
  RiskGenForm: TRiskGenForm;

implementation

{$R *.DFM}
procedure TRiskGenForm.OKClick(Sender: TObject);
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
   else if not ( (temp_year1 >= configuration.first_risk_year) and
                 (temp_year1 <= configuration.last_risk_year)) then
      begin
         ShowMessage ('First year value out of range.  Reenter a value between ' +
                 inttostr(configuration.first_risk_year) + ' and ' + inttostr(configuration.last_risk_year));
         Edit1.clear;
      end
   else start_year_ok := true;

   if temp_year2 = 0 then
     begin   {problem with entry}
        ShowMessage ('Second year value was not a valid integer.  Please reenter.');
        Edit2.clear;
     end
   else if not ( (temp_year2 >= configuration.first_risk_year) and
                 (temp_year2 <= configuration.last_risk_year)) then
      begin
         ShowMessage ('Second year value out of range.  Reenter a value between ' +
             inttostr(configuration.first_risk_year) + ' and ' + inttostr(configuration.last_risk_year));
         Edit2.clear;
      end
   else end_year_ok := true;

   if (start_year_ok and end_year_ok) then
      begin
         {make sure they are ordered lower to higher}
         if temp_year1 > temp_year2 then
           begin
              third := temp_year1;
              temp_year1 := temp_year2;
              temp_year2 := third;
              edit1.text := inttostr (temp_year1);
              edit2.text := inttostr (temp_year2);
           end;
         year1 := temp_year1;
         year2 := temp_year2;
         modalResult := mrOK;
      end;
end;

{------------------------------------------------------------------------}

procedure TRiskGenForm.ResetClick(Sender: TObject);
begin
     edit2.Clear;
     edit1.Clear;
     edit1.text := inttostr (configuration.first_any_year);
     edit2.text := inttostr (configuration.last_any_year);
end;

{-------------------------------------------------------------------------}

procedure TRiskGenForm.CancelClick(Sender: TObject);
begin
     {if they cancel, none of the initial values will be changed, because we
      have not changed user_selections.first_year }
     ModalResult := mrCancel;
end;

{------------------------------------------------------------------------}

procedure TRiskGenForm.FormActivate(Sender: TObject);
begin
    AllYrFrom.caption := inttostr(configuration.first_alliance_year);
    AllYrTo.caption := inttostr(configuration.last_alliance_year);
    CapYrFrom.caption := inttostr(configuration.first_cap_year);
    CapYrTo.caption := inttostr(configuration.last_cap_year);
    RiskYrFrom.caption := inttostr(configuration.first_risk_year);
    RiskYrTo.caption := inttostr(configuration.last_risk_year);
    EUYrFrom.caption := inttostr(configuration.first_eu_year_possible);
    EUYrTo.caption := inttostr(configuration.last_eu_year_possible);
    edit1.text := inttostr (user_selections.first_year);
    edit2.text := inttostr (user_selections.last_year);

end;

procedure TRiskGenForm.HelpbtnClick(Sender: TObject);
begin
     ShowMessage('This command allows you to re-estimate risk values for a subset of years.  Select the years that you want to be used as start and end dates for generating risk in this run, and press OK.'+
          ' If you want to select a different set of years, press Reset to clear the existing year selections.');
end;


procedure TRiskGenForm.FormCreate(Sender: TObject);
begin
   CreateNewFileButton.checked := true;
end;

procedure TRiskGenForm.OverwriteInButtonClick(Sender: TObject);
begin
   OverwriteRiskRecords := true;
   newRiskFile := false;
end;

procedure TRiskGenForm.CreateNewFileButtonClick(Sender: TObject);
begin
   OverwriteRiskRecords := false;
   newRiskFile := true;
end;

end.
