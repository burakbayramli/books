unit CombineYearsSubbox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, eutypes1, ExtCtrls;

type
  TCombineRiskFilesYearsForm = class(TForm)
    OKBtn: TBitBtn;
    CombineRiskFilesYearsPanel: TPanel;
    CombineRiskFilesYearsLabel: TLabel;
    NoSubsetButton: TRadioButton;
    YesSubsetButton: TRadioButton;
    From_Year: TLabel;
    fromyearedit: TEdit;
    To_Year: TLabel;
    toyearedit: TEdit;
    procedure NoSubsetButtonClick(Sender: TObject);
    procedure YesSubsetButtonClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
    filenum : integer;
    year1, year2 : year_range;
    use_subset : boolean;
  public
    constructor createwithitem (itemnum : integer; owner: TComponent);
    procedure showmodal_returns (var int1, int2 : year_range; var use : boolean);
    { Public declarations }
  end;

var
  CombineRiskFilesYearsForm: TCombineRiskFilesYearsForm;

implementation

uses RiskCombineIntegrated;

{$R *.DFM}

constructor TCombineRiskFilesYearsForm.createwithitem (itemnum : integer; owner: TComponent);
   begin
      inherited create(owner);
      filenum := itemnum;
      NoSubsetButton.checked := true;
      YesSubsetButton.checked := false; 
   end;

procedure TCombineRiskFilesYearsForm.NoSubsetButtonClick(Sender: TObject);
begin
   Fromyearedit.enabled := false;
   Toyearedit.enabled := false;
end;

procedure TCombineRiskFilesYearsForm.YesSubsetButtonClick(Sender: TObject);
begin
   Fromyearedit.enabled := true;
   Toyearedit.enabled := true;
end;

procedure TCombineRiskFilesYearsForm.OKBtnClick(Sender: TObject);
   var start_year_ok, end_year_ok : boolean;
       temp_year1, temp_year2, third : integer;
begin
   if  yesSubsetButton.checked then
      begin
         start_year_ok := false;
         end_year_ok := false;

         temp_year1 := strtointdef (fromyearedit.text, 0);
         temp_year2 := strtointdef (toyearedit.text, 0);

         if temp_year1 = 0 then
           begin   {problem with entry}
              ShowMessage ('First year value was not a valid integer.  Please reenter.');
              fromyearedit.clear;
           end
         else if not ( (temp_year1 >= min_year) and
                       (temp_year1 <= max_year)) then
            begin
               ShowMessage ('First year value out of range.  Values must be within range of system membership data.  Reenter a value between ' +
                       inttostr(min_year) + ' and ' + inttostr(max_year));
               fromyearedit.clear;
            end
         else start_year_ok := true;

         if temp_year2 = 0 then
           begin   {problem with entry}
              ShowMessage ('Second year value was not a valid integer.  Please reenter.');
              toyearedit.clear;
           end
         else if not ( (temp_year2 >= configuration.first_nation_year) and
                       (temp_year2 <= configuration.last_nation_year)) then
            begin
               ShowMessage ('Second year value out of range.  Values must be within range of system membership data.  Reenter a value between ' +
                       inttostr(configuration.first_nation_year) + ' and ' + inttostr(configuration.last_nation_year));
               toyearedit.clear;
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
                    fromyearedit.text := inttostr (temp_year1);
                    toyearedit.text := inttostr (temp_year2);
                 end;
               {Now store}
               year1 := temp_year1;
               year2 := temp_year2;
               use_subset := true;

{               self.fromyearlist.items[filenum] := fromyearedit.text;
               self.toyearlist.items[filenum] := toyearedit.text;
               self.subsetlist.items[filenum] := 'y';
}
               close;
            end;
      end   {if yes subset checked}
   else
      {self.subsetlist.items[filenum] := 'n'  ;}
      use_subset := false;
end;

procedure TCombineRiskFilesYearsForm.showmodal_returns (var int1, int2 : year_range; var use : boolean);
   begin
      self.showmodal;
      int1 := year1;
      int2 := year2;
      use := use_subset;
   end;
end.
