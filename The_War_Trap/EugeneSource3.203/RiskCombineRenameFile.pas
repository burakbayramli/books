unit RiskCombineRenameFile;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StrUtils, RiskCombineIntegrated, eutypes1;

type
  TRiskCombineFileRename = class(TForm)
    RenameFilesButton: TButton;
    CancelButton: TButton;
    ErrorMessageLabel: TLabel;
    procedure RenameFilesButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    { Private declarations }

    StartYear : year_range;
    EndYear : year_range;
    RenameThisFile : string;
    {UpdatedFileName : string;  }
    PFile : ^string;

  public
    { Public declarations }
    constructor Init(filename : string; year1 : year_range; year2 : year_range ; PUpdate : Pointer; Owner : TComponent);

  end;

var
  RiskCombineFileRename: TRiskCombineFileRename;

implementation

{$R *.dfm}
{On Creation}
constructor TRiskCombineFileRename.Init(filename : string; year1 : year_range; year2 : year_range ; PUpdate : Pointer; Owner : TComponent);
begin
  inherited Create(Owner);

  RenameThisFile := filename;
  StartYear := year1;
  EndYear := year2;

  {Because the form is freed upon completion, this pointer links with a string back in the RiskCombineIntegrated form so that the renamed file can be used}
  PFile := PUpdate;

  ErrorMessageLabel.caption := 'There is an error with file ' + RenameThisFile + ' the actual years in the file ('+InttoStr(StartYear)+' to '+InttoStr(EndYear)+
    ') do not match the file name.  Choosing confirm will automatically back up the original and create a new file with the appropriate name.  Cancel will change nothing.';

end;



procedure TRiskCombineFileRename.RenameFilesButtonClick(Sender: TObject);
var
  newfilename, backupfilename, directory, original_name, file_extension, temp_string : String;
  temp_ctr : integer;
  temp_char : Char;
  temp_filecopy : string;
  copyfail : bool;


begin

  temp_char := 't';   {just a junk character that isn't 1 or 2}
  temp_ctr := 1;     {starts at one for the string}
  temp_filecopy := ' ';

  {save the directory of the file}
  directory := ExtractFileDir(RenameThisFile);
  original_name := ExtractFileName(RenameThisFile);
  file_extension := ExtractFileExt(RenameThisFile);

  original_name := AnsiReplaceStr(original_name, file_extension, '');


  backupfilename := directory + '\' + original_name + '_original' + file_extension;

  {Note: the following copies the first characters of the file name, up until the start
   of the year given in the file.  It doesn't work perfectly if there is no year in the
   file name, it loses the last character.  Not worth the time to fix right now.}
  repeat
    SetLength(temp_filecopy, temp_ctr);
    temp_filecopy[temp_ctr] := original_name[temp_ctr];
    temp_ctr := temp_ctr + 1;
    temp_char := original_name[temp_ctr];
  Until (temp_char = '1') or (temp_char = '2') or (temp_ctr >= length(original_name));

  newfilename := directory + '\' + temp_filecopy + InttoStr(StartYear) + InttoStr(EndYear) + file_extension;

  copyfail := CopyFile(PChar(RenameThisFile), PChar(backupfilename), false);

  if(copyfail = false) then
  begin
    String(PFile^) := RenameThisFile;
    showmessage('Error in creating backup file, canceling the renaming.  Possibly out of disk space, or no rights to create a file in that directory.');
  end
  else
    String(PFile^) := newfilename;


  RenameFile(RenameThisFile, newfilename);

  Close;



end;

procedure TRiskCombineFileRename.CancelButtonClick(Sender: TObject);
begin
  String(PFile^) := RenameThisFile;
  Close;
end;

end.
