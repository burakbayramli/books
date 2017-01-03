unit FileTransferBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ComCtrls, ExtCtrls;

type
  TFileTransferForm = class(TForm)
    AbortFileTransferButton: TBitBtn;
    FileTransferPanel: TPanel;
    FileTransferLabel: TLabel;
    StatusBar1: TStatusBar;
    procedure AbortFileTransferButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FileTransferForm: TFileTransferForm;

implementation

uses FTPConnect;

{$R *.DFM}

procedure TFileTransferForm.AbortFileTransferButtonClick(Sender: TObject);
begin
   FTP_Form.AbortTransfer (self);
   close;
end;

end.
