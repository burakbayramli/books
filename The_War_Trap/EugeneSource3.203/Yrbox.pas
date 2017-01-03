unit YrBox;

{EUGene  Copyright 1997, 1998  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

uses
  eutypes1, Setting, OutWindow, Printers, 
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons;

type
  TYear_Choice_Box = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    OK: TBitBtn;
    Cancel: TBitBtn;
    Reset: TBitBtn;
    From_Year: TLabel;
    To_Year: TLabel;
    AllianceYrs: TLabel;
    CapYrs: TLabel;
    RiskYr: TLabel;
    EUYr: TLabel;
    AllYrFrom: TLabel;
    AllYrTo: TLabel;
    CapYrFrom: TLabel;
    CApYrTo: TLabel;
    RiskYrFrom: TLabel;
    RiskYrTo: TLabel;
    EUYrFrom: TLabel;
    EUYrTo: TLabel;
    Helpbtn: TBitBtn;
    MIDYearLabel: TLabel;
    MIDLabelFrom: TLabel;
    MIDLabelTo: TLabel;
    polityyearlabel: TLabel;
    polityyear1label: TLabel;
    polityyear2label: TLabel;
    nationdatlabel: TLabel;
    nationyear1label: TLabel;
    nationyear2label: TLabel;
    limitinglabel: TLabel;
    procedure OKClick(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure HelpbtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Year_Choice_Box: TYear_Choice_Box;

implementation

{$R *.DFM}

{--------------------------------------------------------------}

procedure TYear_Choice_Box.OKClick(Sender: TObject);
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
      end;
end;

{------------------------------------------------------------------------}

procedure TYear_Choice_Box.ResetClick(Sender: TObject);
begin
     edit2.Clear;
     edit1.Clear;
     edit1.text := inttostr (configuration.first_any_year);
     edit2.text := inttostr (configuration.last_any_year);
end;

{-------------------------------------------------------------------------}

procedure TYear_Choice_Box.CancelClick(Sender: TObject);
begin
     {if they cancel, none of the initial values will be changed, because we
      have not changed user_selections.first_year }
     Close;
end;

{------------------------------------------------------------------------}

procedure TYear_Choice_Box.FormActivate(Sender: TObject);
begin
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
end;

procedure TYear_Choice_Box.HelpbtnClick(Sender: TObject);
begin
     ShowMessage('Select the years to be included in the output and press OK.'+
          ' If you want to select a different set of years, press Reset to' +
          ' clear the existing year selections.');
end;









procedure TYear_Choice_Box.FormCreate(Sender: TObject);
begin
   self.height := 10+from_year.height+10+edit1.height + 10 + (nationdatlabel.height+10)*7 + 10 + limitinglabel.height + 10 + OK.height+ 10;
   self.width := nationdatlabel.width + edit1.width + edit2.width+ 50;
end;

end.
