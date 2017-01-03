unit RereadNamesForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TReadVarNameOkForm = class(TForm)
    ConfirmNamesPanel: TPanel;
    AskLabel: TLabel;
    okbtn: TBitBtn;
    cancelbtn: TBitBtn;
    ConfirmNamesHeader: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ReadVarNameOkForm: TReadVarNameOkForm;

implementation

{$R *.DFM}

procedure TReadVarNameOkForm.FormCreate(Sender: TObject);
begin
   AskLabel.width := ConfirmNamesPanel.width - 12;
   AskLabel.height := ConfirmNamesPanel.height - 24;
   AskLabel.caption := 'You already have variables defined.  Do you want to attempt to read new variable names from data file?  This will reset current variable information.  Press OK to read from file, or Cancel to keep current names.';
end;

end.
