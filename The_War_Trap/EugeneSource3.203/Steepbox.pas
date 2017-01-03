unit SteepBox;

{EUGene  Copyright 1997, 1998  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

   uses   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     StdCtrls, Buttons, Mask, ExtCtrls, cmnprocd, euinoutd, eutypes1, eutypes2,
     euprocs1, euprocs2 ;

type
  TSteepestSubForm = class(TForm)
    Panel1: TPanel;
    risklabel: TLabel;
    ToleranceEditBox: TMaskEdit;
    OKBtn: TBitBtn;
    CancBtn: TBitBtn;
    HelpBtn: TBitBtn;
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
  SteepestSubForm: TSteepestSubForm;

implementation

{$R *.DFM}

procedure TSteepestSubForm.OKBtnClick(Sender: TObject);
var errcode : integer;
begin
   val (ToleranceEditBox.edittext, user_selections.risk_calculation_info.risk_search_tolerance, errcode);
   if errcode <> 0 then {input was invalied}
      begin
         ShowMessage('Invalid risk tolerance input - setting tolerance to 0.01');
         user_selections.risk_calculation_info.risk_search_tolerance := 0.01;
      end;

    user_selections.risk_calculation_info.method := use_steepest;
    modalresult := mrOK;
end;

procedure TSteepestSubForm.HelpBtnClick(Sender: TObject);
begin
   ShowMessage ('Enter Search Tolerance, and press OK');
end;

procedure TSteepestSubForm.FormShow(Sender: TObject);
var TempString : string;
begin
    TempString := '     ';
    str(user_selections.risk_calculation_info.risk_search_tolerance:5:3, TempString);
    ToleranceEditBox.edittext := TempString;

end;

procedure TSteepestSubForm.CancBtnClick(Sender: TObject);
begin
   modalresult := mrCancel;
end;

end.
