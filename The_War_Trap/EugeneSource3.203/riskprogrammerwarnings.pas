unit RiskProgrammerWarnings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TSecurityFileOverwriteWarning = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    overwriteWarningMemo: TMemo;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TSecurityFileOverwriteWarning.BitBtn1Click(Sender: TObject);
begin
   modalresult := mrOK;
end;

procedure TSecurityFileOverwriteWarning.BitBtn2Click(Sender: TObject);
begin
   modalresult := mrcancel;
end;

end.
