unit SplashMove;

{EUGene  Copyright 1997-2002  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

uses
  ProgressInterruptWind, SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls;

type
  TSplashy = class(TForm)
    EugeneLbl: TLabel;
    CreatedLbl: TLabel;
    CopyLbl: TLabel;
    WaitLbl: TLabel;
    Label1: TLabel;
    MainContinueButton: TBitBtn;
    GlobeAnimation: TAnimate;
    Label2: TLabel;
    Label3: TLabel;
    SplashScreenVersionLabel: TLabel;
    procedure notready;
    procedure ready;
    procedure MainContinueButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

  end;

var
  Splashy: TSplashy;

implementation

{$R *.DFM}

procedure TSplashy.notready;
   begin
      WaitLbl.caption := 'Initializing.  Please Wait...';
      MainContinueButton.enabled := false;
      Self.enabled := false;
   end;

procedure TSplashy.ready;
   begin
      self.enabled := true;
      waitlbl.caption := 'Initialization Complete!  ';
      MainContinueButton.enabled := true;
   end;

procedure TSplashy.MainContinueButtonClick(Sender: TObject);
begin
   close;
end;


end.
