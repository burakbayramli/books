unit RandBox;

{EUGene  Copyright 1997, 1998  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     StdCtrls, Buttons, Mask, ExtCtrls, EUtypes1;

type
  TRandomIterationForm = class(TForm)
    RandIterationsPanel: TPanel;
    Iterations_Label: TLabel;
    IterationsEditBox: TMaskEdit;
    OKBtn: TBitBtn;
    CancBtn: TBitBtn;
    HelpBtn: TBitBtn;
    RandIterationsLabel: TLabel;
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure CancBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RandomIterationForm: TRandomIterationForm;

implementation

{$R *.DFM}

procedure TRandomIterationForm.OKBtnClick(Sender: TObject);
var errcode : integer;
begin
   val (IterationsEditBox.edittext, user_selections.risk_calculation_info.random_risk_iterations, errcode);
   if errcode <> 0 then {input was invalied}
      begin
         ShowMessage('Invalid number of iterations input - setting iterations to 1');
         user_selections.risk_calculation_info.random_risk_iterations := 1;
      end;

    user_selections.risk_calculation_info.method := use_random_walk;
    modalresult := mrOK;
end;

procedure TRandomIterationForm.HelpBtnClick(Sender: TObject);
begin
   ShowMessage ('Enter the number of random walks you want the algorithm to make.');
end;

procedure TRandomIterationForm.CancBtnClick(Sender: TObject);
begin
   modalresult := mrCancel;
end;

end.
