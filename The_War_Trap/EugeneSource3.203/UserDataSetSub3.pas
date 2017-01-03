unit UserDataSetSub3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, Buttons, EUTypes1, ExtCtrls;

type
  TUserDataSetSubForm3 = class(TForm)
    DoneBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Panel1: TPanel;
    FirstYearLabel: TLabel;
    FirstYearMaskEdit: TMaskEdit;
    LastYearMaskEdit: TMaskEdit;
    LastYearLabel: TLabel;
    NumCasesLabel: TLabel;
    NumCasesMaskEdit: TMaskEdit;
    AutoDetectYearCaseButton: TButton;
    procedure AutoDetectYearCaseButtonClick(Sender: TObject);
    procedure FirstYearMaskEditChange(Sender: TObject);
    procedure LastYearMaskEditChange(Sender: TObject);
    procedure NumCasesMaskEditChange(Sender: TObject);
    procedure DoneBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UserDataSetSubForm3: TUserDataSetSubForm3;

implementation

   uses UserDataPreparationForm;
   
{$R *.DFM}

procedure TUserDataSetSubForm3.AutoDetectYearCaseButtonClick(Sender: TObject);
begin
   UserDataPreparationForm.AutoDetectYearCaseButtonClick(sender);
end;

procedure TUserDataSetSubForm3.FirstYearMaskEditChange(Sender: TObject);
begin
   UserDataPreparationForm.FirstYearMaskEditChange(sender);
end;

procedure TUserDataSetSubForm3.LastYearMaskEditChange(Sender: TObject);
begin
   UserDataPreparationForm.LastYearMaskEditChange(sender);
end;

procedure TUserDataSetSubForm3.NumCasesMaskEditChange(Sender: TObject);
begin
   UserDataPreparationForm.NumCasesMaskEditChange(sender);
end;

procedure TUserDataSetSubForm3.DoneBtnClick(Sender: TObject);
   var all_values_ok : boolean;
begin
   all_values_ok := true;

   With User_data_set_info do
   begin
      if num_cases <= 0 then
         begin
            Showmessage ('Number of cases must be > 0.  Configuration file will be saved, but it is incorrect and the dataset will not be properly accessible in EUGene.');
            all_values_ok := false;
         end;

      if not ((data_set_first_year_possible >= min_year) and (data_set_first_year_possible <= max_year)) then
         begin
            Showmessage ('First year of data set out of bounds.  It must fall between '+inttostr(min_year)+' and '+inttostr(max_year)+'.  Configuration file will be saved, but it is incorrect and the dataset will not be properly accessible in EUGene.');
            all_values_ok := false;
         end;
      if not ((data_set_last_year_possible >= min_year) and (data_set_last_year_possible <= max_year)) then
         begin
            Showmessage ('Last year of data set out of bounds.  It must fall between '+inttostr(min_year)+' and '+inttostr(max_year)+'.  Configuration file will be saved, but it is incorrect and the dataset will not be properly accessible in EUGene.');
            all_values_ok := false;
         end;
   end;
end;

procedure TUserDataSetSubForm3.HelpBtnClick(Sender: TObject);
begin
     ShowMessage ('EUGene can automatically determine the year range and number of cases from user data.  Confirm the results before proceeding.')
end;

end.
