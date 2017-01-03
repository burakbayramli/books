unit GenrHelp;

interface

uses
  EUTypes1, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls;

type
  TGenericHelpForm = class(TForm)
    CloseHelpButton: TBitBtn;
    GenericHelpMemo: TRichEdit;
    procedure HelpCloseButttonClick(Sender: TObject);
    procedure present (helpfile_name : TFileName);
    procedure CloseHelpButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GenericHelpForm: TGenericHelpForm;

implementation

{$R *.DFM}

procedure TGenericHelpForm.present (helpfile_name : TFileName);
   begin
     with GenericHelpMemo.Lines do
     begin
        clear;
        LoadFromFile(helpfile_name);
     end;
   end;

procedure TGenericHelpForm.HelpCloseButttonClick(Sender: TObject);
begin
   Close;
end;

procedure TGenericHelpForm.CloseHelpButtonClick(Sender: TObject);
begin
   close;
end;

end.
