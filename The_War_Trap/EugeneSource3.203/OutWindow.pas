unit OutWindow;

{EUGene  Copyright 1997, 1998  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  Toutputwindow = class(TForm)
    Screen_Output: TMemo;
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
 outputwindow: Toutputwindow;

implementation

{$R *.DFM}

procedure Toutputwindow.FormResize(Sender: TObject);
begin
   Screen_output.width := self.clientwidth;
   Screen_output.height := self.clientheight;
end;

end.
