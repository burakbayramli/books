unit abortcheck;

{EUGene  Copyright 1997, 1998  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type
  TVerifyStopForm = class(TForm)
    Panel1: TPanel;
    Interrupt: TBitBtn;
    cancbut: TBitBtn;
    WarningMessage: TMemo;
    procedure InterruptClick(Sender: TObject);
    procedure cancbutClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  VerifyStopForm: TVerifyStopForm;

implementation

uses EUTypes1;

{$R *.DFM}

procedure TVerifyStopForm.InterruptClick(Sender: TObject);
begin
   {this will be processed in the next .tick call}
   userinterrupt := true;
   close;
end;

procedure TVerifyStopForm.cancbutClick(Sender: TObject);
begin
   userinterrupt := false;
   close;
end;

end.
