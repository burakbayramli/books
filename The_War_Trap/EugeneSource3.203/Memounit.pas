unit MemoUnit;

{EUGene  Copyright 1997, 1998  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  TTraceWindow = class(TForm)
    OutputMem: TMemo;
    procedure OutputSetSize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TraceWindow: TTraceWindow;

implementation

procedure TTraceWindow.OutputSetSize(Sender: TObject);
begin
     self.width := round(4 * screen.pixelsPerInch);
     self.height := round (5 * screen.pixelsPerInch);
     self.left := screen.width - self.width;
     self.top := screen.height - self.height;
     self.outputmem.height := self.clientheight;
     self.outputmem.width := self.clientwidth;
     outputmem.left := 0;
     outputmem.top := 0;
end;

{$R *.DFM}


end.
