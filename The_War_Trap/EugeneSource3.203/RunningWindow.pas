unit RunningWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TRunningForm = class(TForm)
    Runningword: TStaticText;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RunningForm: TRunningForm;

implementation

{$R *.DFM}

end.
