unit Contigu;

{EUGene  Copyright 1997, 1998  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, eutypes1;

type
  TContigForm = class(TForm)
    CancelBtn: TBitBtn;
    Helpbtn: TBitBtn;
    OKBtn: TBitBtn;
    ContiguityLevelPanel: TPanel;
    ContiguityLevelLabel: TLabel;
    ContigRButtonLevel1: TRadioButton;
    ContigRButtonLevel2: TRadioButton;
    ContigRButtonLevel3: TRadioButton;
    ContigRButtonLevel4: TRadioButton;
    ContigRButtonLevel5: TRadioButton;
    procedure CancelBtnClick(Sender: TObject);
    procedure HelpbtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ContiguityBox : TContigForm;

implementation

{$R *.DFM}

procedure TContigForm.CancelBtnClick(Sender: TObject);
begin
   modalResult := mrCancel;
end;

procedure TContigForm.HelpbtnClick(Sender: TObject);
begin
   ShowMessage ('Select the level of contiguity between states that is required '+
                'if a dyad is to be included in your output file or printout.');
end;

procedure TcontigForm.OKBtnClick(Sender: TObject);
begin
   if ContigRButtonLevel1.checked then user_selections.contiguity_level_required := 1;
   if ContigRButtonLevel2.checked then user_selections.contiguity_level_required := 2;
   if ContigRButtonLevel3.checked then user_selections.contiguity_level_required := 3;
   if ContigRButtonLevel4.checked then user_selections.contiguity_level_required := 4;
   if ContigRButtonLevel5.checked then user_selections.contiguity_level_required := 5;
   ModalResult := MrOK;
end;

procedure TcontigForm.FormActivate(Sender: TObject);
begin
   ContigRButtonLevel1.checked := (user_selections.contiguity_level_required = 1);
   ContigRButtonLevel2.checked := (user_selections.contiguity_level_required = 2);
   ContigRButtonLevel3.checked := (user_selections.contiguity_level_required = 3);
   ContigRButtonLevel4.checked := (user_selections.contiguity_level_required = 4);
   ContigRButtonLevel5.checked := (user_selections.contiguity_level_required = 5);
end;

end.
