unit Pause;

{EUGene  Copyright 1997  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, Buttons;

type
  TPauseBox = class(TForm)
    BitBtn1: TBitBtn;
    PausePanel: TPanel;
    PauseLabel: TLabel;
    procedure BitBtn1Click(Sender: TObject);
  end;

var
  PauseBox: TPauseBox;

implementation

{$R *.DFM}

procedure TPauseBox.BitBtn1Click(Sender: TObject);
begin
   self.Hide;
end;

end.
