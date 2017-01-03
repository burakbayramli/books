unit AllianceSource;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, eutypes1, Buttons, ExtCtrls;

type
  TAllianceDataSourceForm = class(TForm)
    AllianceSourceOKButton: TBitBtn;
    HelpBtn: TBitBtn;
    AllianceDataSourcePanel: TPanel;
    SequenceNumAllianceDataButton: TRadioButton;
    DyadicInputDataButton: TRadioButton;
    AllianceDataSourceLabel: TLabel;
    procedure AllianceSourceOKButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AllianceDataSourceForm: TAllianceDataSourceForm;

implementation
   uses euinoutd, Errbx;  

{$R *.DFM}


procedure TAllianceDataSourceForm.AllianceSourceOKButtonClick(Sender: TObject);
begin
   {Only option now is for dyadic data;  old sequence # version removed}
   user_selections.alliance_data_source := flat_dyadic;


   {if DyadicInputDataButton.checked = true then
      user_selections.alliance_data_source := flat_dyadic
   else
   if SequenceNumAllianceDataButton.checked = true then
      user_selections.alliance_data_source := flat_cow_sequence
   else
      EUGeneError ('Error in alliance data source button - source not selected on form exit.  Notify programmer.', 2, stop, error_log);
   }
end;

procedure TAllianceDataSourceForm.FormShow(Sender: TObject);
begin
   case user_selections.alliance_data_source of
         flat_dyadic : DyadicInputDataButton.checked := true;
         {flat_cow_sequence : SequenceNumAllianceDataButton.checked := true;}
   else
      EUGeneError ('Error in alliance data source button - Entered proc without alliance data source set.  Notify programmer.', 2, stop, error_log);
   end;  {case}
end;

procedure TAllianceDataSourceForm.HelpBtnClick(Sender: TObject);
begin
   ShowMessage ('Choosing "Dyadic Alliance File" uses the updated, dyadic version of the alliance data described in the program documentation.  '+
                'Choosing "COW sequenced alliance data" uses only the original COW data running to 1984 from the original sequence-number data set.  ');
end;

end.
