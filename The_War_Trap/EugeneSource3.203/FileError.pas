unit FileError;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, cmnprocd, ExtCtrls;

type
  TFileErrorBox = class(TForm)
    OkButton: TBitBtn;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure maindo (string1, string2, string3 : string);
  private
  public
  end;

var
  FileErrorBox: TFileErrorBox;

implementation

{$R *.DFM}


procedure TFileErrorBox.maindo (string1, string2, string3 : string);
begin
   self.label1.caption := string1;
   self.label2.caption := string2;
   self.label3.caption := string3;
end;



end.
