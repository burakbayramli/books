unit AllianceOutput;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons,
  EUtypes1, EUtypes2, originalriskunit;

type
  TAllianceOutputForm = class(TForm)
    GroupBox1: TGroupBox;
    FileControlsPanel: TPanel;
    OutputLabel: TLabel;
    ToFileButton: TRadioButton;
    SetOutputFileButton: TButton;
    FilePathField: TEdit;
    ToScreenSelectionButton: TRadioButton;
    VariableSeparatorPanel: TPanel;
    VariableSeparatorLabel: TLabel;
    TabSeparatorButton: TRadioButton;
    SpaceSeparatorButton: TRadioButton;
    CommaSeparatorButton: TRadioButton;
    DataOptionsBox: TGroupBox;
    ScoresPanel: TPanel;
    TauSelectionButton: TRadioButton;
    sSelectionButton: TRadioButton;
    ScoresLabel: TLabel;
    BestWorstAlliancePanel: TPanel;
    BestSelectionButton: TRadioButton;
    WorstSelectionButton: TRadioButton;
    BestWorstLabel: TLabel;
    YearRangePanel: TPanel;
    AllYearsButton: TRadioButton;
    YearRangeButton: TRadioButton;
    StartYearField: TEdit;
    EndYearField: TEdit;
    yearlabel1: TLabel;
    YearRangeLabel: TLabel;
    BestAndWorstButton: TRadioButton;
    CancelButton: TButton;
    ConfirmButton: TButton;
    Panel2: TPanel;
    DyadYearButton: TRadioButton;
    CountryListButton: TRadioButton;
    Panel1: TPanel;
    HeaderLabel: TLabel;
    HeaderInfoBox: TCheckBox;
    Label1: TLabel;
    AllianceSaveDialog: TSaveDialog;
    procedure CancelButtonClick(Sender: TObject);
    procedure TauSelectionButtonClick(Sender: TObject);
    procedure sSelectionButtonClick(Sender: TObject);
    procedure BestSelectionButtonClick(Sender: TObject);
    procedure WorstSelectionButtonClick(Sender: TObject);
    procedure BestAndWorstButtonClick(Sender: TObject);
    procedure AllYearsButtonClick(Sender: TObject);
    procedure YearRangeButtonClick(Sender: TObject);
    procedure ToScreenSelectionButtonClick(Sender: TObject);
    procedure SetOutputFileButtonClick(Sender: TObject);
    procedure ToFileButtonClick(Sender: TObject);
    procedure TabSeparatorButtonClick(Sender: TObject);
    procedure SpaceSeparatorButtonClick(Sender: TObject);
    procedure CommaSeparatorButtonClick(Sender: TObject);
    procedure ConfirmButtonClick(Sender: TObject);
    procedure StartYearFieldChange(Sender: TObject);
    procedure EndYearFieldChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DyadYearButtonClick(Sender: TObject);
    procedure CountryListButtonClick(Sender: TObject);

  private
    { Private declarations }
    score : integer;
    best : bool;
    worst : bool;
    first_year : year_range;
    last_year : year_range;
    output_to_file : bool;
    OutputFile : string;
    variable_separator : char;
    config : configuration_type;
    years_set : bool;
    output_to_screen : bool;
    user_years : bool;
    run_once : bool;
    file_selected : bool;

    procedure AllianceOutputToFile;
    procedure AllianceOutputToScreen;

  public
    { Public declarations }
    procedure Init(configuration : configuration_type; Sender : TObject);
  end;

var
  AllianceOutputForm : TAllianceOutputForm;

implementation

uses OutWindow;

{$R *.dfm}

procedure TAllianceOutputForm.Init(configuration : configuration_type; Sender : TObject);
begin
{this initializes certain fields and selections in the window upon the first run
  all of these buttons/fields will remain upon further runs (until program exit)}
  if (run_once <> true) then
    begin
      config := configuration;
      StartYearField.Text := Inttostr(config.first_alliance_year);
      EndYearField.Text := Inttostr(config.last_alliance_year);
      FilePathField.Text := config.eugene_directory;
      YearRangeButton.checked := False;
      file_selected := false;
    end;
    {we ran, don't initialize these again}
    run_once := true;

end;

procedure TAllianceOutputForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

{The next several procedures just record the result of various option selections}
procedure TAllianceOutputForm.TauSelectionButtonClick(Sender: TObject);
begin
  score := 1;
end;

procedure TAllianceOutputForm.sSelectionButtonClick(Sender: TObject);
begin
  score := 2;
end;

procedure TAllianceOutputForm.BestSelectionButtonClick(Sender: TObject);
begin
  best := true;
  worst := false;
end;

procedure TAllianceOutputForm.WorstSelectionButtonClick(Sender: TObject);
begin
  worst := true;
  best := false;
end;

procedure TAllianceOutputForm.BestAndWorstButtonClick(Sender: TObject);
begin
  best := true;
  worst := true;
end;

procedure TAllianceOutputForm.AllYearsButtonClick(Sender: TObject);
begin
  first_year := config.first_alliance_year;
  last_year := config.last_alliance_year;
  years_set := true;
end;

procedure TAllianceOutputForm.YearRangeButtonClick(Sender: TObject);
begin
  years_set := true;
  user_years := true;
end;

procedure TAllianceOutputForm.ToScreenSelectionButtonClick(Sender: TObject);
begin
  {a series of things have to be adjusted here based on checks later, also disables separator selections}
  Output_to_Screen := true;
  Output_to_File := false;
  File_selected := true;
  TabSeparatorButton.Enabled := false;
  CommaSeparatorButton.Enabled := false;
  SpaceSeparatorButton.Enabled := false;

end;

procedure TAllianceOutputForm.SetOutputFileButtonClick(Sender: TObject);
var fileok : boolean;
begin
  {stolen code from elsewhere in EUGene, selects the file to be saved to.
    This will prompt that a file will be overwritten}
      AllianceSaveDialog.Filter := 'Data Files (*.CSV)|*.CSV|All files (*.*)|*.*';
      AllianceSaveDialog.FileName := '';
      AllianceSaveDialog.Options := [ofOverwritePrompt, ofHideReadOnly];
      fileok := false;
      if AllianceSaveDialog.execute then
         begin
            if (extractFileExt(AllianceSaveDialog.FileName) = '.') or (extractFileExt(AllianceSaveDialog.FileName) = '') then
               begin
                  AllianceSaveDialog.FileName := changeFileExt(AllianceSaveDialog.FileName, '.csv');
                  {showmessage ('Your output data file must have a 3 character extension (like ".csv"). EUGene has renamed your output file to '+Frame.SaveDialog1.FileName);    }
               end;
            fileok:= true;
            outputfile := AllianceSaveDialog.FileName;
            FilePathField.text:=AllianceSaveDialog.FileName;
            Output_to_File := true;
            Output_to_Screen := false;
         end
      else {exited with cancel}
         begin
            fileOK := false;
         end;

     { if (FileExists(FilePathField.text)) then
        ShowMessage('File already exists - generating data will overwrite the file.');     }

      ToFileButton.checked := true;
      Output_to_file := true;
      TabSeparatorButton.Enabled := true;
      CommaSeparatorButton.Enabled := true;
      SpaceSeparatorButton.Enabled := true;
      file_selected := true;


end;

procedure TAllianceOutputForm.ToFileButtonClick(Sender: TObject);
begin
  Output_to_file := true;
  TabSeparatorButton.Enabled := true;
  CommaSeparatorButton.Enabled := true;
  SpaceSeparatorButton.Enabled := true;
end;

procedure TAllianceOutputForm.TabSeparatorButtonClick(Sender: TObject);
begin
  variable_separator := chr(9);
end;

procedure TAllianceOutputForm.SpaceSeparatorButtonClick(Sender: TObject);
begin
  variable_separator := ' ';
end;

procedure TAllianceOutputForm.CommaSeparatorButtonClick(Sender: TObject);
begin
  variable_separator := ',';
end;

procedure TAllianceOutputForm.ConfirmButtonClick(Sender: TObject);
var
  error : bool;
begin

  error := false;

  if (user_years = true) then
  begin
    first_year := StrtoInt(StartYearField.Text);
    last_year := StrtoInt(EndYearField.Text);

   if(first_year >= config.first_alliance_year) and (first_year <= config.last_alliance_year) and (first_year <= last_year) and (last_year <= config.last_alliance_year) then
    begin
       {moved code out of here, the check works the same, no real need to change the if statement}
    end
   else
    begin
      showmessage('Error with year range.');
      error := true;
    end;
   end;

  if (file_selected = false) then
    begin
      showmessage('Invalid File selection - need to select a specific file');
      error := true;
    end;

  if (best = false) and (worst = false) then
    begin
      showmessage('Need to chose best, worst, or both alliance sets.');
      error := true;
    end;

  if (years_set = false) then
    begin
      showmessage('Select years for output.');
      error := true;
    end;


   if (output_to_screen = false) and (output_to_file = false) then
    begin
      showmessage('Select an output destination.');
      error := true;
    end;

   if (variable_separator = 'x') and (output_to_screen = false) then
    begin
      showmessage('Select a variable separator.');
      error := true;
    end;

   if (score = 0) then
    begin
      showmessage('Select an Alliance Score.');
      error := true;
    end;

    if (DyadYearButton.checked = false) and (CountryListButton.Checked = false) then
      begin
        showmessage('Select Format type.');
        error := true;
      end;

    if (error = false) and (output_to_file = true) then
      begin
        AllianceOutputToFile;
        Close;
      end;

    if (error = false) and (output_to_screen = true) then
      begin
        AllianceOutputToScreen;
        Close;
      end;


end;

procedure TAllianceOutputForm.AllianceOutputToFile;
var
  sec_data : Trisk_stored_security_alliance_obj;
  year_loop : year_range;
  ccode, ccode2 : ccode_range;
  loop_region : region_type;
  val1, val2, outstring : string;
  Output : TextFile;
begin

  if(score = 1) then
    sec_data := Trisk_Stored_security_alliance_obj.init(config.security_alliance_Tau_file_name, first_year, last_year);
  if(score = 2) then
    sec_data := Trisk_Stored_security_alliance_obj.init(config.security_alliance_S_unweighted_file_name, first_year, last_year);

  if ( (score = 1) and (FileExists(config.security_alliance_Tau_file_name)) ) or ( (score = 2) and (FileExists(config.security_alliance_S_unweighted_file_name)) ) then
    begin

    OutputFile := FilePathField.Text;
  AssignFile(Output, OutputFile);
  Rewrite(Output);

  if (CountryListButton.Checked = true) then
  begin
  For year_loop := first_year to last_year do
    begin
      {header info for the year}
      if(HeaderInfoBox.Checked = true) then
        begin
          outstring := 'Year'+variable_separator+'CCode'+variable_separator+'BestWors'+variable_separator+'Region'+variable_separator;
          for ccode := min_ccode to max_ccode do
            if(nation_list.is_a_state(ccode, year_loop)) then
              outstring := outstring+inttostr(ccode)+variable_separator;
          Writeln(Output, outstring);
        end;

      {now the actual data}
      for ccode := min_ccode to max_ccode do
        if(nation_list.is_a_state(ccode, year_loop)) then
          begin
            for loop_region := europe to americas do
              begin
                if(best = true) then
                begin
                  outstring := inttostr(year_loop) + variable_separator + inttostr(ccode) + variable_separator + '1' + variable_separator+ inttostr(ord(loop_region))+variable_separator;
                  for ccode2 := min_ccode to max_ccode do
                    if(nation_list.is_a_state(ccode2, year_loop)) then
                      begin
                        str(sec_data.get_best_alliance(ccode, ccode2, year_loop, loop_region):1, val1);
                       outstring := outstring + val1 + variable_separator;
                     end;
                    Writeln(Output, outstring);
                  end;
                  if(worst = true) then
                  begin
                    outstring := inttostr(year_loop) + variable_separator + inttostr(ccode) + variable_separator + '2' + variable_separator+ inttostr(ord(loop_region))+variable_separator;
                    for ccode2 := min_ccode to max_ccode do
                      if(nation_list.is_a_state(ccode2, year_loop)) then
                      begin
                        str(sec_data.get_worst_alliance(ccode, ccode2, year_loop, loop_region):1, val1);
                        outstring := outstring + val1 + variable_separator;
                      end;
                     Writeln(Output, outstring);
                  end;
              end;
            end;
      end;
    end;

    if(dyadyearbutton.Checked = true) then
    begin
      outstring := 'CCode'+variable_separator+'CCode2'+variable_separator+'Year'+variable_separator+'Region';  {'BestType'+variable_separator+'WorsType';   }
      if (best = true) then
        outstring := outstring + variable_separator + 'BestType';
      if (worst = true) then
        outstring := outstring + variable_separator + 'WorsType';
      Writeln(Output, outstring);

      For year_loop := first_year to last_year do
        begin
          for ccode := min_ccode to max_ccode do
            if(nation_list.is_a_state(ccode, year_loop)) then
              for ccode2 := min_ccode to max_ccode do
                if(nation_list.is_a_state(ccode2, year_loop)) then
                  for loop_region := europe to americas do
                  begin
                    outstring := inttostr(ccode) + variable_separator + inttostr(ccode2) + variable_separator + inttostr(year_loop) + variable_separator + inttostr(ord(loop_region))+variable_separator;
                    if(best = true) then
                      begin
                        str(sec_data.get_best_alliance(ccode, ccode2, year_loop, loop_region):1, val1);
                        outstring := outstring + val1 + variable_separator;
                      end;
                    if(worst = true) then
                      begin
                        str(sec_data.get_worst_alliance(ccode, ccode2, year_loop, loop_region):1, val2);
                        outstring := outstring + val2 + variable_separator;
                      end;
                    Writeln(Output, outstring);
                  end;
          end;

    CloseFile(Output);

    {resets the file to prevent accidental overwrite, the user must specify a file again
      (and thus, if they want to overwrite the file, they have to specifically chose to
      note: it defaults back to the eugene directory, may eventually want to have it use the last path specified?}
    FilePathField.Text := config.eugene_directory;

    end;

    end

    else
      begin
      {error}
      if (score = 1) then
        showmessage('File '+config.security_alliance_Tau_file_name+' does not exist.');
      if (score = 2) then
        showmessage('File '+config.security_alliance_S_unweighted_file_name+' does not exist.');
      end;

end;

procedure TAllianceOutputForm.AllianceOutputToScreen;
var
  sec_data : Trisk_stored_security_alliance_obj;
  year_loop : year_range;
  ccode, ccode2 : ccode_range;
  loop_region : region_type;
  val1, val2, outstring : string;
begin
  {determine which file to use based on the type of data}
  if(score = 1) then
    sec_data := Trisk_Stored_security_alliance_obj.init(config.security_alliance_Tau_file_name, first_year, last_year);
  if(score = 2) then
    sec_data := Trisk_Stored_security_alliance_obj.init(config.security_alliance_S_unweighted_file_name, first_year, last_year);
    {overly complicated boolean expression to check if files exist for given type}
if ( (score = 1) and (FileExists(config.security_alliance_Tau_file_name)) ) or ( (score = 2) and (FileExists(config.security_alliance_S_unweighted_file_name)) ) then
    begin

  OutputWindow.Screen_Output.Clear;
  OutputWindow.Show;


  if (CountryListButton.Checked = true) then
  begin
  For year_loop := first_year to last_year do
    begin
      {header info for the year}
      if(HeaderInfoBox.Checked = true) then
        begin
          outstring := 'Year'+chr(9)+'CCode'+chr(9)+'BestWors'+chr(9)+'Region'+chr(9);
          for ccode := min_ccode to max_ccode do
            if(nation_list.is_a_state(ccode, year_loop)) then
              outstring := outstring+inttostr(ccode)+chr(9);
          OutputWindow.Screen_Output.lines.Add(outstring);
        end;

      {now the actual data}
      for ccode := min_ccode to max_ccode do
        if(nation_list.is_a_state(ccode, year_loop)) then
          begin
            for loop_region := europe to americas do
              begin
                if(best = true) then
                begin
                  outstring := inttostr(year_loop) + chr(9) + inttostr(ccode) + chr(9) + '1' + chr(9)+ inttostr(ord(loop_region))+chr(9);
                  for ccode2 := min_ccode to max_ccode do
                    if(nation_list.is_a_state(ccode2, year_loop)) then
                      begin
                        str(sec_data.get_best_alliance(ccode, ccode2, year_loop, loop_region):1, val1);
                       outstring := outstring + val1 + chr(9);
                     end;
                    OutputWindow.Screen_Output.lines.Add(outstring);
                  end;
                  if(worst = true) then
                  begin
                    outstring := inttostr(year_loop) + chr(9) + inttostr(ccode) + chr(9) + '2' + chr(9)+ inttostr(ord(loop_region))+chr(9);
                    for ccode2 := min_ccode to max_ccode do
                      if(nation_list.is_a_state(ccode2, year_loop)) then
                      begin
                        str(sec_data.get_worst_alliance(ccode, ccode2, year_loop, loop_region):1, val1);
                        outstring := outstring + val1 + chr(9);
                      end;
                     OutputWindow.Screen_Output.lines.Add(outstring);
                  end;
              end;
            end;
      end;
    end;

    if(dyadyearbutton.Checked = true) then
    begin
      variable_separator := chr(9);
      outstring := 'CCode'+variable_separator+'CCode2'+variable_separator+'Year'+variable_separator+'Region';
      if (best = true) then
        outstring := outstring + variable_separator + 'BestType';
      if (worst = true) then
        outstring := outstring + variable_separator + 'WorsType';
      OutputWindow.Screen_Output.Lines.Add(outstring);

      For year_loop := first_year to last_year do
        begin
          for ccode := min_ccode to max_ccode do
            if(nation_list.is_a_state(ccode, year_loop)) then
              for ccode2 := min_ccode to max_ccode do
                if(nation_list.is_a_state(ccode2, year_loop)) then
                  for loop_region := europe to americas do
                  begin
                    outstring := inttostr(ccode) + variable_separator + inttostr(ccode2) + variable_separator + inttostr(year_loop) + variable_separator + inttostr(ord(loop_region))+variable_separator;
                    if(best = true) then
                      begin
                        str(sec_data.get_best_alliance(ccode, ccode2, year_loop, loop_region):1, val1);
                        outstring := outstring + val1 + variable_separator;
                      end;
                    if(worst = true) then
                      begin
                        str(sec_data.get_worst_alliance(ccode, ccode2, year_loop, loop_region):1, val2);
                        outstring := outstring + val2 + variable_separator;
                      end;
                    OutputWindow.Screen_Output.Lines.Add(outstring);
                  end;
          end;


     end ;

    end

    else
      begin
      {error}
      if (score = 1) then
        showmessage('File '+config.security_alliance_Tau_file_name+' does not exist.');
      if (score = 2) then
        showmessage('File '+config.security_alliance_S_unweighted_file_name+' does not exist.');
      end;
end;


procedure TAllianceOutputForm.StartYearFieldChange(Sender: TObject);
begin
  years_set := true;
  user_years := true;
  YearRangeButton.Checked := true;
end;

procedure TAllianceOutputForm.EndYearFieldChange(Sender: TObject);
begin
  years_set := true;
  user_years := true;
  YearRangeButton.Checked := true;
end;

procedure TAllianceOutputForm.FormCreate(Sender: TObject);
begin
  {first initialization}
    {config := configuration;
    StartYearField.Text := '1816';
    EndYearField.Text := '1999';
    FilePathField.Text := config.eugene_directory + 'alliance_output.csv';    }
    best := false;
    worst := false;
    years_set := false;
    output_to_file := false;
    user_years := false;
    output_to_screen := false;
    score := 0;
    variable_separator := 'x';  {junk value to show that it's not selected yet}


    HeaderInfoBox.Checked := true;
    BestAndWorstButton.Checked := false;
    TabSeparatorButton.Checked := false;
    SpaceSeparatorButton.Checked := false;
    CommaSeparatorButton.Checked := false;
    ToFileButton.Checked := false;
    ToScreenSelectionButton.Checked := false;
    BestSelectionButton.checked := false;
    WorstSelectionButton.checked := false;
    AllYearsButton.checked := false;
    YearRangeButton.checked := false;
    TauSelectionButton.checked := false;
    sSelectionButton.checked := false;
    DyadYearButton.checked := false;
    CountryListButton.Checked := false;

end;

procedure TAllianceOutputForm.DyadYearButtonClick(Sender: TObject);
begin
  if(dyadyearbutton.Checked = true) then
    HeaderInfoBox.Enabled := false;
end;

procedure TAllianceOutputForm.CountryListButtonClick(Sender: TObject);
begin
  if(Countrylistbutton.Checked = true) then
    HeaderInfoBox.Enabled := true;
end;

end.
