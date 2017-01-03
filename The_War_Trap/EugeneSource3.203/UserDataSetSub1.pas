unit UserDataSetSub1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TUserDataSetSubForm1 = class(TForm)
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    DocFileEdit: TEdit;
    OpenDialog1: TOpenDialog;
    DataFileEdit: TEdit;
    ConfigFileEdit: TEdit;
    ConfigFileNewNameBtn: TButton;
    Button1: TButton;
    SelectDataButton: TButton;
    SelectDocFileButton: TButton;
    GroupBox6: TGroupBox;
    LongNameEdit: TEdit;
    GroupBox7: TGroupBox;
    ShortNameEdit: TEdit;
    DoneBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    GroupBox8: TGroupBox;
    CitationEdit: TEdit;
    procedure DoneBtnClick(Sender: TObject);
    procedure ConfigFileNewNameBtnClick(Sender: TObject);
    procedure ConfigFileLoadBtnClick(Sender: TObject);
    procedure DataFileSelectButtonClick(Sender: TObject);
    procedure LongNameEditChange(Sender: TObject);
    procedure ShortNameEditChange(Sender: TObject);
    procedure SelectDataButtonClick(Sender: TObject);
    procedure SelectDocFileButtonClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure CitationEditChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UserDataSetSubForm1: TUserDataSetSubForm1;

implementation

uses UserDataPreparationForm;

{$R *.DFM}

procedure TUserDataSetSubForm1.DoneBtnClick(Sender: TObject);
var all_values_ok : boolean;
begin
   all_values_ok := true;
   if config_file_name_set = false then
      begin
         ShowMessage ('You must specify a name for the output configuration file.  Configuration file will be saved, but it is incorrect and the dataset will not be properly accessible in EUGene.');
         all_values_ok := false;
      end;

   With User_data_set_info do
   begin
      if not(fileexists (User_data_set_info.file_name)) then
         begin
            Showmessage ('Warning:  Specified data file '+User_data_set_info.file_name+' does not exist.  You must ensure that the file exists before you may proceed.  ');
            all_values_ok := false;
         end;
      if length(User_data_set_info.data_set_full_name) > 45 then
         begin
            Showmessage ('The descriptive name of the user data set is too long.  Only 45 characters will be kept.');
            data_set_full_name := copy (data_set_full_name, 0, 45);
            updatevalues (User_data_set_info);
         end;
      if length(User_data_set_info.data_set_short_name) > 18 then
         begin
            Showmessage ('The descriptive name of the user data set is too long.  Only 18 characters will be kept.');
            data_set_short_name := copy (data_set_short_name, 0, 18);
            updatevalues (User_data_set_info);
         end;
   end;

   if all_values_ok then modalresult := mrOK else modalresult := mrnone;
end;

procedure TUserDataSetSubForm1.ConfigFileNewNameBtnClick(Sender: TObject);
begin
   UserDataPreparationForm.ConfigFileNewNameBtnClick(Sender);
end;

procedure TUserDataSetSubForm1.ConfigFileLoadBtnClick(Sender: TObject);
begin
   UserDataPreparationForm.ConfigFileLoadBtnClick(sender);
end;

procedure TUserDataSetSubForm1.DataFileSelectButtonClick(Sender: TObject);
begin
   UserDataPreparationForm.DataFileSelectButtonClick(sender);
end;

procedure TUserDataSetSubForm1.LongNameEditChange(Sender: TObject);
begin
   UserDataPreparationForm.LongNameEditChange(sender);
end;

procedure TUserDataSetSubForm1.ShortNameEditChange(Sender: TObject);
begin
   UserDataPreparationForm.ShortNameEditChange(sender);
end;

procedure TUserDataSetSubForm1.CitationEditChange(Sender: TObject);
begin
   UserDataPreparationForm.CitationEditChange(sender);
end;

procedure TUserDataSetSubForm1.SelectDataButtonClick(Sender: TObject);
begin
   UserDataPreparationForm.DataFileSelectButtonClick(sender);

end;

procedure TUserDataSetSubForm1.SelectDocFileButtonClick(Sender: TObject);
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

procedure TUserDataSetSubForm1.HelpBtnClick(Sender: TObject);
begin
     ShowMessage ('This form permits the user to provide long and short descriptive names for the dataset, and also specify the data, configuration, and documentation file names.  More details may be found in the Eugene documentation.') 
end;



end.
