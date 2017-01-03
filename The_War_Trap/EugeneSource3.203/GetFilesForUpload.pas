unit GetFilesForUpload;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TFilesForUploadForm = class(TForm)
    OpenDialog1: TOpenDialog;
    UploadBtn: TBitBtn;
    CancelBtn: TBitBtn;
    DataFilePanel: TPanel;
    DataFileLabel: TLabel;
    DocFilePanel: TPanel;
    DocFileLabel: TLabel;
    DataConfigFilePanel: TPanel;
    DataConfigFileLabel: TLabel;
    DataFileEdit: TEdit;
    DocFileEdit: TEdit;
    ConfigFileEdit: TEdit;
    DataFileButton: TPanel;
    DocFileButton: TPanel;
    DataConfigFileButton: TPanel;
    procedure SelectDataButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure UploadBtnClick(Sender: TObject);
    procedure SelectedfButtonClick(Sender: TObject);
    procedure SelectDocFileButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FilesForUploadForm: TFilesForUploadForm;

implementation

uses ftpconnect;

{$R *.DFM}

procedure TFilesForUploadForm.SelectDataButtonClick(Sender: TObject);
   begin
       try
          OpenDialog1 := TOpenDialog.Create(Self);
          OpenDialog1.Title := 'Select data file associated with this dataset';
          OpenDialog1.Filter := 'Data files (*.csv)|*.CSV';
          if OpenDialog1.Execute then
             begin
                DataFileEdit.text := OpenDialog1.filename;
             end;
       finally
          OpenDialog1.Free;
       end;    {finally}
   end;

procedure TFilesForUploadForm.SelectedfButtonClick(Sender: TObject);
begin
       try
          OpenDialog1 := TOpenDialog.Create(Self);
          OpenDialog1.Title := 'Select EUGene data set configuration file associated with this dataset';
          OpenDialog1.Filter := 'Configuration files (*.edf)|*.EDF';
          if OpenDialog1.Execute then
             begin
                ConfigFileEdit.text := OpenDialog1.filename;
             end;
       finally
          OpenDialog1.Free;
       end;    {finally}
end;

procedure TFilesForUploadForm.SelectDocFileButtonClick(Sender: TObject);
begin
       try
          OpenDialog1 := TOpenDialog.Create(Self);
          OpenDialog1.Title := 'Select documentation file associated with this dataset';
          OpenDialog1.Filter := 'Documentation files (*.rtf)|*.RTF';
          if OpenDialog1.Execute then
             begin
                DocFileEdit.text := OpenDialog1.filename;
             end;
       finally
          OpenDialog1.Free;
       end;    {finally}
end;

procedure TFilesForUploadForm.FormShow(Sender: TObject);
begin
   if ftpconnect.UploadFileNames.count > 0 then
      begin
         DataFileEdit.text := ftpconnect.UploadFilenames.strings[0];
         ConfigFileEdit.text := ftpconnect.UploadFilenames.strings[1];
         DocFileEdit.text := ftpconnect.UploadFilenames.strings[2];
      end;
end;

procedure TFilesForUploadForm.CancelBtnClick(Sender: TObject);
begin
   modalresult := mrcancel;
end;

procedure TFilesForUploadForm.UploadBtnClick(Sender: TObject);
begin
   {if datasetnameedit.text = '' then ShowMessage ('You must choose a descriptive name for this data set.  This name will appear for others to download.')
   else}
   if datafileedit.text = '' then ShowMessage ('You must select a data file before uploading')
   else
   if ConfigFileEdit.text = '' then ShowMessage ('You must select a EUGene configuration file before uploading')
   else
   if DocFileEdit.text = '' then ShowMessage ('You must select a documentation file in rich-text-format before uploading')
   else
      begin
         ftpconnect.UploadFilenames.add (DataFileEdit.text);
         ftpconnect.UploadFilenames.add (ConfigFileEdit.text);
         ftpconnect.UploadFilenames.add (DocFileEdit.text);
         modalresult := mrok;
      end;
end;

end.
