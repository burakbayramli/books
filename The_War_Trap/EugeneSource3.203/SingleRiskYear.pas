unit SingleRiskYear;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, eutypes1, ExtCtrls;

type
  TSingleRiskYearForm = class(TForm)
    OK: TBitBtn;
    Cancel: TBitBtn;
    Reset: TBitBtn;
    Helpbtn: TBitBtn;
    Panel1: TPanel;
    FileChangeButton: TButton;
    FileDisplay: TEdit;
    StaticText2: TStaticText;
    SaveDialog1: TSaveDialog;
    SpecificRiskGenPanel: TPanel;
    SpecificRiskGenLabel: TLabel;
    From_Year: TLabel;
    To_Year: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    AllYrTo: TLabel;
    AllYrFrom: TLabel;
    CapYrFrom: TLabel;
    CApYrTo: TLabel;
    RiskYrTo: TLabel;
    RiskYrFrom: TLabel;
    EUYrFrom: TLabel;
    EUYrTo: TLabel;
    EUYr: TLabel;
    RiskYr: TLabel;
    CapYrs: TLabel;
    AllianceYrs: TLabel;
    procedure OKClick(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure HelpbtnClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FileChangeButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    year : year_range;
    ccode : ccode_range;
    outfile_name : TFileName;
  end;


implementation

{$R *.DFM}

procedure TSingleRiskYearForm.OKClick(Sender: TObject);
   var year_ok, ccode_ok : boolean;
       tempyear, tempccode : integer;
begin
   year_ok := false;
   ccode_ok := false;

   tempyear := strtointdef (edit1.text, 0);
   tempccode := strtointdef (edit2.text, 0);

   if tempyear = 0 then
     begin   {problem with entry}
        ShowMessage ('Year value was not a valid integer.  Please reenter.');
        Edit1.clear;
     end
   else if not ( (tempyear >= configuration.first_risk_year) and
                 (tempyear <= configuration.last_risk_year)) then
      begin
         ShowMessage ('Year value out of range.  Reenter a value between ' +
                 inttostr(configuration.first_risk_year) + ' and ' + inttostr(configuration.last_risk_year));
         Edit1.clear;
      end
   else year_ok := true;

   if year_ok then
      begin
         if tempccode = 0 then
           begin   {problem with entry}
              ShowMessage ('CCode value was not a valid integer.  Please reenter.');
              Edit2.clear;
           end
         else if not (nation_list.is_a_state(tempccode, tempyear)) then
            begin
               ShowMessage ('CCode value out of range.  CCode '+inttostr(tempccode) + ' is not a state in ' + inttostr(tempyear));
               Edit2.clear;
            end
         else ccode_ok := true;
      end;
      
   if (year_ok and ccode_ok) then
      begin
         year := tempyear;
         ccode := tempccode;
         close;
         modalResult := mrOK;
         {filename will be OK}
      end;

end;

procedure TSingleRiskYearForm.ResetClick(Sender: TObject);
begin
     edit2.Clear;
     edit1.Clear;
     edit1.text := inttostr (configuration.first_risk_year);
     edit2.text := inttostr (ccode_index.ccode(1));
end;

procedure TSingleRiskYearForm.HelpbtnClick(Sender: TObject);
begin
     ShowMessage('This command allows you to re-estimate risk values and display risk details for a single year and ccode.  Select the year, ccode, and output file for risk details that you want to use, and press OK.'+
          ' If you want to select different values, press Reset to clear the existing year selections.  NOTE:  this procedure will not overwrite any records in the file of risk data, it is for display purposes only.');
end;

procedure TSingleRiskYearForm.CancelClick(Sender: TObject);
begin
     Close;
     ModalResult := mrCancel;
end;

procedure TSingleRiskYearForm.FormCreate(Sender: TObject);
begin
    AllYrFrom.caption := inttostr(configuration.first_alliance_year);
    AllYrTo.caption := inttostr(configuration.last_alliance_year);
    CapYrFrom.caption := inttostr(configuration.first_cap_year);
    CapYrTo.caption := inttostr(configuration.last_cap_year);
    RiskYrFrom.caption := inttostr(configuration.first_risk_year);
    RiskYrTo.caption := inttostr(configuration.last_risk_year);
    EUYrFrom.caption := inttostr(configuration.first_eu_year_possible);
    EUYrTo.caption := inttostr(configuration.last_eu_year_possible);
    year := user_selections.first_year;
    ccode := ccode_index.ccode(1);
    outfile_name := 'OptimalRiskPatterns.out';
    edit1.text := inttostr (year);
    edit2.text := inttostr (ccode);
    FileDisplay.text:= outfile_name;

end;

procedure TSingleRiskYearForm.FileChangeButtonClick(Sender: TObject);
begin
      SaveDialog1.title := 'Choose File for Risk Detail output';
      SaveDialog1.Filter := 'Output files (*.OUT)|*.OUT|Text files (*.TXT)|*.TXT|All files (*.*)|*.*';
      SaveDialog1.FileName := outfile_name;
      SaveDialog1.Options := [ofOverwritePrompt, ofHideReadOnly];
      if SaveDialog1.execute then
         begin
            outfile_name := SaveDialog1.FileName;
            FileDisplay.text:=outfile_name;
         end
      else {exited with cancel}
         begin
         end;
end;

end.
