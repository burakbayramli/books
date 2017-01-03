unit FileOutError;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, cmnprocd;

type
  TFileOutErrorBox = class(TForm)
    RetryBtn: TBitBtn;
    CancelButton: TBitBtn;
    Memo1: TMemo;
    procedure fileset (filename : file_name_type);
    procedure CancelButtonClick(Sender: TObject);
    procedure RetryBtnClick(Sender: TObject);
  private
    outfile : text;
    filename : file_name_type;
  public
    fileOK : boolean;
  end;

var
  FileOutErrorBox: TFileOutErrorBox;

implementation

{$R *.DFM}

procedure TFileOutErrorBox.fileset (filename : file_name_type);
begin
   self.filename := filename;
   assignFile(self.outfile, filename);
end;

procedure TFileOutErrorBox.CancelButtonClick(Sender: TObject);
begin
      self.fileOK := false;
end;

procedure TFileOutErrorBox.RetryBtnClick(Sender: TObject);
begin
   try
      rewrite (self.outfile);
      self.fileOK := true;
   except
      self.fileOK := false;
      {raise;}
   end;

end;

end.
