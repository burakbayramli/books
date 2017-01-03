unit PeaceYearsOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, eutypes1;

type
  TPeaceYearsOptionForm = class(TForm)
    ContiguityLevelPanel: TPanel;
    label1: TLabel;
    Adjustbutton: TRadioButton;
    noadjustbutton: TRadioButton;
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Helpbtn: TBitBtn;
    WernerExtraNoteLabel: TLabel;
    procedure OKBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure HelpbtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PeaceYearsOptionForm: TPeaceYearsOptionForm;

implementation

{$R *.dfm}

procedure TPeaceYearsOptionForm.OKBtnClick(Sender: TObject);
begin
   user_selections.werner_peace_year_adjustment := (adjustbutton.checked = true);
   modalresult := mrOK;
end;

procedure TPeaceYearsOptionForm.FormActivate(Sender: TObject);
begin
   adjustbutton.checked := (user_selections.werner_peace_year_adjustment = true);
   noadjustbutton.checked := (user_selections.werner_peace_year_adjustment = false);
end;

procedure TPeaceYearsOptionForm.CancelBtnClick(Sender: TObject);
begin
   ModalResult := mrCancel;
end;

procedure TPeaceYearsOptionForm.HelpbtnClick(Sender: TObject);
begin
   ShowMessage ('Werner (2000) examined historical records to identify the date of last dispute between all states present in the COW system in 1816, if that dispute occured before 1816.'+
                'Selecting this adjustment adds these pre-1816 years to the initial value of the peace years variable for those dyads present in 1816.');
end;

end.
