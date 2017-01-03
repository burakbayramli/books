unit Setting;

{EUGene  Copyright 1997, 1998  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  TSettingsForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    CntryLabel: TLabel;
    ListBox1: TListBox;
    varbox: TGroupBox;
    countrydyadbox: TGroupBox;
    ccodelb: TLabel;
    yearlb: TLabel;
    caplb: TLabel;
    majpowlb: TLabel;
    relreglb: TLabel;
    reguncerlb: TLabel;
    riskeulb: TLabel;
    riskwtrlb: TLabel;
    risklb: TLabel;
    taulb: TLabel;
    distancelb: TLabel;
    euwartrplb: TLabel;
    euwrlb: TLabel;
    polrellb: TLabel;
    contlb: TLabel;
    dyadlb: TLabel;
    countrydyadlb: TLabel;
    HomeReglb: TLabel;
    slb: TLabel;
    tauwithleaderlb: TLabel;
    DispLb: TLabel;
    polity3lb: TLabel;
    PeaceYrsLb: TLabel;
    UserVariablesLb: TLabel;
    Alliancelb: TLabel;
    AlliancePortLb: TLabel;
    EqLabel: TLabel;
    outputbox: TGroupBox;
    outputlb: TLabel;
    Extrasbox: TGroupBox;
    dyadivsilb: TLabel;
    headerlb: TLabel;
    OngoingLb: TLabel;
    joinlb: TLabel;
    DisputeSettingLbBox: TGroupBox;
    SideADispLb: TLabel;
    DispTrueLabel: TLabel;
    MarkSubsLabel: TLabel;
    reverselb: TLabel;
    DropJoinerLb: TLabel;
    SysCharLabel: TLabel;
    AbbrLb: TLabel;
    isolb: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SettingsForm: TSettingsForm;

implementation

{$R *.DFM}

uses
    mdiframe, Clipbrd;

{--------------------------------------------------------------------}

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
     Scaled:=true;

     {self.height := Frame.clientheight;
     self.width := Frame.clientwidth;}

     outputlb.caption:='None';
     countrydyadlb.caption:='None';

end;







end.
