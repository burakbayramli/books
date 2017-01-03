unit UserDataConfigureForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, Buttons, ExtCtrls, Grids, math, EUTypes1, UserDataVarForm, cmnprocd;

const ConfiguratorVersion = 0.92;

type
  TUserDataForm = class(TForm)
    OpenDialog1: TOpenDialog;
    QuitNoSaveButton: TBitBtn;
    HelpBtn: TBitBtn;
    AutoDetectButton: TPanel;
    NumCasesLabel: TLabel;
    FirstYearLabel: TLabel;
    LongNameLabel: TLabel;
    DataFileLabel: TLabel;
    ShortNameLabel: TLabel;
    LastYearLabel: TLabel;
    UnitLabel: TLabel;
    NumCasesMaskEdit: TMaskEdit;
    LastYearMaskEdit: TMaskEdit;
    UnitComboBox: TComboBox;
    LongNameEdit: TEdit;
    ShortNameEdit: TEdit;
    DataFileEdit: TEdit;
    DataFileSelectButton: TButton;
    FirstYearMaskEdit: TMaskEdit;
    ConfigFileNameLabel: TLabel;
    ConfigFileEdit: TEdit;
    ConfigFileNewNameBtn: TButton;
    ConfigFileLoadBtn: TButton;
    SaveDialog1: TSaveDialog;
    SaveNoExitButton: TBitBtn;
    AutoDetectYearCaseButton: TButton;
    VarGroupBox: TGroupBox;
    VarInfoStringGrid: TStringGrid;
    MoveDownVariableButton: TButton;
    MoveUpVariableButton: TButton;
    DeleteVarButton: TButton;
    EditVarButton: TButton;
    AddVariableButton: TButton;
    NumVarsLabel: TLabel;
    NumVarsMaskEdit: TMaskEdit;
    LabelLineCheckBox: TCheckBox;
    AutoDetectNameButton: TButton;
    procedure QuitNoSaveButtonClick(Sender: TObject);
    procedure ConfigFileNewNameBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ConfigFileLoadBtnClick(Sender: TObject);
    procedure VarInfoStringGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure SaveNoExitButtonClick(Sender: TObject);
    procedure DataFileSelectButtonClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure EditVarButtonClick(Sender: TObject);
    procedure AddVariableButtonClick(Sender: TObject);
    procedure UnitComboBoxChange(Sender: TObject);
    procedure DeleteVarButtonClick(Sender: TObject);
    procedure MoveUpVariableButtonClick(Sender: TObject);
    procedure MoveDownVariableButtonClick(Sender: TObject);
    procedure AutoDetectNameButtonClick(Sender: TObject);
    procedure AutoDetectYearCaseButtonClick(Sender: TObject);
    procedure LongNameEditChange(Sender: TObject);
    procedure ShortNameEditChange(Sender: TObject);
    procedure LabelLineCheckBoxClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UserDataForm: TUserDataForm;
  User_data_set_info : data_set_info_record;
  Config_file_name : TFileName;
  blank_input_data : one_var_data_type;
  Config_file_name_set : boolean;

implementation

uses RereadNamesForm;

{$R *.DFM}


procedure set_variable_buttons;
  {enables/disables variable buttons when user selects a row/variable.}
   begin
      with UserDataForm do
         begin
            AddVariableButton.enabled := (User_data_set_info.num_vars < max_user_variables_per_data_set);
            {The other buttons all enabled iff user has selected a row of data.}
            if User_data_set_info.num_vars <= 0 then
               begin
                  EditVarButton.enabled := false;
                  DeleteVarButton.enabled := false;
                  MoveUpVariableButton.enabled := false;
                  MoveDownVariableButton.enabled := false;
               end
               else
                  {there is at least one variable.  Is one selected?  If so, enable.}
               if VarInfoStringGrid.row > 0 then  {they have selected a row, other than the label line}
               begin
                  EditVarButton.enabled := true;
                  DeleteVarButton.enabled := true;
                  MoveUpVariableButton.enabled := true;
                  MoveDownVariableButton.enabled := true;
               end;
         end;
   end;   {proc set variables buttons}

procedure UpdateValues (User_data_set_info : data_set_info_record);
   {procedure takes all values from user data set record and displays in form.}
   var avar : integer;
   begin
      with UserDataForm do
         begin
            ConfigFileEdit.text := Config_file_name;
            DataFileEdit.Text := User_data_set_info.file_name;
            LongNameEdit.text := User_data_set_info.data_set_full_name;
            ShortNameEdit.text := User_data_set_info.data_set_short_name;
            case User_data_set_info.data_set_unit of
               no_unit_dataset : UnitComboBox.itemindex := 0;
               annual_data : UnitComboBox.itemindex := 1;
               country_year : UnitComboBox.itemindex := 2;
               directed_dyad_year : UnitComboBox.itemindex := 3;
               nondirected_dyad_year : UnitComboBox.itemindex := 4;
            end;

            LabelLineCheckBox.checked := (User_data_set_info.label_line=true);
            FirstYearMaskEdit.text := inttostr(User_data_set_info.data_set_first_year_possible);
            LastYearMaskEdit.text := inttostr(User_data_set_info.data_set_last_year_possible);
            NumCasesMaskEdit.text := inttostr(User_data_set_info.num_cases);
            NumVarsMaskEdit.text := inttostr(User_data_set_info.num_vars);

            VarGroupBox.caption := ' Variable Information:  '+inttostr (length(User_data_set_info.var_info))+' variables currently defined ';
            with VarInfoStringGrid do
               begin
                  {accessed by column, row}
                  {Want to set # of scrollable rows to # user data vars.  But, if there
                   are not any vars specified, set to 2 so can display headers.}
                  rowcount := length(User_data_set_info.var_info)+1;
                  if rowcount < 2 then rowcount := 2;
                  {Need to reset fixed rows after row # change}
                  fixedrows := 1;
                  width := colwidths[0]+colwidths[1]+colwidths[2]+colwidths[3]+colwidths[4]+colwidths[5];
                  if length(User_data_set_info.var_info) >0 then
                     begin
                        for avar := 0 to User_data_set_info.num_vars - 1 do
                           begin
                              {display info on a variable}
                              cells[0,avar+1] := inttostr(avar+1);
                              cells[1,avar+1] := User_data_set_info.var_info[avar].var_name;
                              case User_data_set_info.var_info[avar].var_type of
                                    varinteger : cells[2,avar+1] := 'integer';
                                    varsingle : cells[2,avar+1] := 'real';
                                    varolestr : cells[2,avar+1] := 'string';
                                 end;   {case}
                              case User_data_set_info.var_info[avar].var_unit of
                                    no_unit_variable : cells[3,avar+1] := 'Not specified';
                                    identifierccode1 : case User_data_set_info.data_set_unit of
                                          country_year : cells[3,avar+1] := 'Country Code identifier';
                                          directed_dyad_year, nondirected_dyad_year : cells[3,avar+1] := 'Country Code 1 identifier';
                                          else begin
                                             showmessage ('Invalid variable type seen for type of data set - setting variable type to none');
                                             User_data_set_info.var_info[avar].var_unit := no_unit_variable;
                                             end;
                                       end;   {case data set unit of}
                                    identifierccode2 : cells[3,avar+1] := 'Country Code 2 identifier';
                                    identifieryear : cells[3,avar+1] := 'Year identifier';
                                    identifierversion : cells[3,avar+1] := 'Dataset version number';
                                    annual : cells[3,avar+1] := 'Annual data';
                                    monadic : cells[3,avar+1] := 'Monadic (country data)';
                                    dyadic_ordered : cells[3,avar+1] := 'Dyadic, and ordered';
                                    dyadic_unordered : cells[3,avar+1] := 'Dyadic, but unordered';
                                 end;   {case}
                              cells[4,avar+1] := User_data_set_info.var_info[avar].var_reversed_var;
                              if User_data_set_info.var_info[avar].var_missing_value = low(longint) then
                                 cells[5,avar+1] := '.'
                              else
                                 cells[5,avar+1] := inttostr(User_data_set_info.var_info[avar].var_missing_value);
                           end;               {for avar}
                     end
                  else
                     begin    {length of var list <= 0, so there is one visibiel row,
                              but need to blank out the visible cell row now.}
                        VarInfoStringGrid.cells[0,1] := '';
                        VarInfoStringGrid.cells[1,1] := '';
                        VarInfoStringGrid.cells[2,1] := '';
                        VarInfoStringGrid.cells[3,1] := '';
                        VarInfoStringGrid.cells[4,1] := '';
                        VarInfoStringGrid.cells[5,1] := '';
                     end;
               end;  {with stringgrid}
         end;   {with userdataform}
      Set_Variable_Buttons;

   end;   {proc showvalues}


procedure TUserDataForm.ConfigFileNewNameBtnClick(Sender: TObject);
begin
   UserDataForm.SaveDialog1.Filter :='Configuration files (*.edf)|*.edf|Text files (*.txt)|*.txt|All files (*.*)|*.*';
   UserDataForm.SaveDialog1.FileName := Config_file_name;
   UserDataForm.SaveDialog1.Title := 'Specify User Configuration File for Output';
   UserDataForm.SaveDialog1.Options := [ofOverwritePrompt, ofHideReadOnly];

   if UserDataForm.SaveDialog1.execute then
         begin
            {check and see if .edf extension.  If not, make it that.}
            if (extractFileExt(UserDataForm.SaveDialog1.FileName) <> '.'+eugene_data_file_extension) then
               begin    {add .edf}
                  UserDataForm.SaveDialog1.FileName := UserDataForm.SaveDialog1.FileName + '.' + eugene_data_file_extension;
               end;
            ConfigFileEdit.text := UserDataForm.SaveDialog1.FileName;
            Config_file_name := UserDataForm.SaveDialog1.FileName;
            config_file_name_set := true;
         end
      else {exited with cancel}
         begin
         end;

end;

procedure TUserDataForm.ConfigFileLoadBtnClick(Sender: TObject);
   var baddataset : boolean;
begin
   UserDataForm.OpenDialog1.Filter :='Configuration files (*.edf)|*.edf|Text files (*.txt)|*.txt|All files (*.*)|*.*';
   UserDataForm.OpenDialog1.FileName := Config_file_name;
   UserDataForm.OpenDialog1.Title := 'Load Information from Configuration File';
   {no .options specified.}
   if UserDataForm.OpenDialog1.execute then
         begin
            ConfigFileEdit.text:=UserDataForm.OpenDialog1.FileName;
            Config_file_name := UserDataForm.OpenDialog1.FileName;
            Read_one_user_data_file_info(Config_file_name,
               User_data_set_info, baddataset);
            if baddataset then
               showmessage ('Errors in input configuration file '+Config_file_name+'.  Please check all values before saving new configuration file')
            else
               begin
                  AutoDetectNameButton.enabled := true;
                  AutoDetectYearCaseButton.enabled := true;
               end;
            config_file_name_set := true;
            UpdateValues (User_data_set_info);
         end
      else {exited with cancel}
         begin
         end;
end;

procedure TUserDataForm.DataFileSelectButtonClick(Sender: TObject);
begin
   UserDataForm.OpenDialog1.Filter :='Comma-separated files (*.csv)|*.csv|Data files (*.dat)|*.dat|Text files (*.txt)|*.txt|All files (*.*)|*.*';
   UserDataForm.OpenDialog1.FileName := User_data_set_info.file_name;
   UserDataForm.OpenDialog1.Title := 'Locate User Data File';
   if UserDataForm.OpenDialog1.execute then
         begin
            DataFileEdit.text:=UserDataForm.OpenDialog1.FileName;
            User_data_set_info.file_name := UserDataForm.OpenDialog1.FileName;
            AutoDetectNameButton.enabled := true;
            AutoDetectYearCaseButton.enabled := true;
         end
      else {exited with cancel}
         begin
         end;
end;


procedure TUserDataForm.FormCreate(Sender: TObject);
   {var baseSelection : TGridRect;}
begin
   {Initialize all fields to acceptable and meaningless values}
   UserDataForm.caption := 'Define User Data File  [EUGene data configuration utility v'+realtostring(ConfiguratorVersion,1,2)+']';

   {ShortNameLabel.hint := ShortNameEdit.hint;
   LongNameLabel.hint := LongNameEdit.hint;  }
   
   Config_file_name := 'No configuration file identified';
   config_file_name_set := false;
   ForceCurrentDirectory := true;
   {UserDataForm.OpenDialog1.initialdir := ExtractFilePath(Application.ExeName);
   UserDataForm.SaveDialog1.initialdir := UserDataForm.OpenDialog1.initialdir;
}
   AutoDetectNameButton.enabled := false;
   AutoDetectYearCaseButton.enabled := false;
   with User_data_set_info do
      begin
         file_name := 'No file name identified';
         data_set_full_name := 'No full name defined';
         data_set_short_name := 'No short name';
         data_set_unit := no_unit_dataset;
         data_set_first_year_possible := min_year;
         data_set_last_year_possible := min_year;
         label_line := false;
         num_vars := 0;
         num_cases := 0;
         setlength(var_info,0);
      end;
      UnitComboBox.items[0] := 'Select Data Unit...';
      UnitComboBox.items[1] := 'Year (Annual Data)';
      UnitComboBox.items[2] := 'Country-Year (Monadic Data)';
      UnitComboBox.items[3] := 'Directed Dyad-Year';
      UnitComboBox.items[4] := 'Nondirected Dyad-Year';
      LabelLineCheckBox.checked := true;

      {Set initial grid/table size and labels.}
      with VarInfoStringGrid do
         begin
            {accessed by column, row}
            colcount := 5+1;
            {Want to set # of scrollable rows to # user data vars.  But, if there
             are not any vars specified, set to 2 so can display headers.}
            rowcount := User_data_set_info.num_vars+1;
            if rowcount < 2 then rowcount := 2;
            fixedrows := 1;
            fixedcols := 1;
            cells[0,0] := 'Number';
            cells[1,0] := 'Variable Name';
            cells[2,0] := 'Data Type';
            cells[3,0] := 'Variable Unit';
            cells[4,0] := 'Complementary Var. Name';
            cells[5,0] := 'Missing Value';
            colwidths[0] := 45;
            colwidths[1] := 85;
            colwidths[2] := 65;
            colwidths[3] := 100;
            colwidths[4] := 130;
            colwidths[5] := 90;
            width := colwidths[0]+colwidths[1]+colwidths[2]+colwidths[3]+colwidths[4]+colwidths[5];
            {Now, set none to be initially selected.}
            {BaseSelection.left := 0;
             BaseSelection.right := 0;
             BaseSelection.top := 0;
             BaseSelection.bottom := 0;
             selection := BaseSelection; }
            VarInfoStringGrid.row := 0;
         end;

   UpdateValues (User_data_set_info);

   {Create a blank variable to use for initializations later}
   blank_input_data.var_name := '';
   blank_input_data.var_type := varinteger;
   blank_input_data.var_unit := no_unit_variable;
   blank_input_data.var_missing_value := -9;
   blank_input_data.var_reversed_var := 'none';

end;


procedure TUserDataForm.VarInfoStringGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  {First set can select to true for all except top row}
  canselect := false;
  if Arow > 0 then
     begin
        canselect := true;
        {For some reason, clicking on fixed column 0 never registers!
         Even with this code!}
        if acol = 0 then VarInfoStringGrid.row := arow;
     end;
   {When user clicks a row, set the buttons.}
  Set_Variable_Buttons;
end;

procedure TUserDataForm.EditVarButtonClick(Sender: TObject);
  var resultvardata : one_var_data_type_ptr;
begin
   if User_data_set_info.data_set_unit = no_unit_dataset then
      ShowMessage ('Data set unit of analysis must be defined before adding or editing variables')
   else
      begin
         new (resultvardata);
         VariableInformationForm := TVariableInformationForm.createWithInfo(User_data_set_info.var_info[VarInfoStringGrid.row-1],
                                    User_data_set_info.data_set_unit, resultvardata, self);
         if VariableInformationForm.showmodal = mrOK then
            begin
               {change the data.  Access as current selected row -1, because
                var_info is indexed from 0, unlike rows.  But only -1 because
                the presence of the first row is already in the .row variable.}
               User_data_set_info.var_info[VarInfoStringGrid.row-1] := resultvardata^;
               UpdateValues(User_data_set_info);
            end;
         dispose (resultvardata);
         VariableInformationForm.free;
      end;
end;

procedure TUserDataForm.AddVariableButtonClick(Sender: TObject);
  var resultvardata : one_var_data_type_ptr;
begin
   if User_data_set_info.data_set_unit = no_unit_dataset then
      ShowMessage ('Data set unit of analysis must be defined before adding or editing variables')
   else
      begin
         {Do edit, but pass in a blank record.}
         new (resultvardata);
         VariableInformationForm := TVariableInformationForm.createWithInfo(blank_input_data,
                                    User_data_set_info.data_set_unit, resultvardata, self);
         if VariableInformationForm.showmodal = mrOK then
            begin
               {Just lengthen list, then add at position "length-1"}
               setlength(User_data_set_info.var_info, length(User_data_set_info.var_info)+1);
               User_data_set_info.var_info[length(User_data_set_info.var_info)-1] := resultvardata^;
               {If the num vars box matches, then increment it}
               if User_data_set_info.num_vars < length(User_data_set_info.var_info) then
                  User_data_set_info.num_vars := length(User_data_set_info.var_info);

               {normally add one row.  But, if it's really the first variable, then don't.}

               UpdateValues(User_data_set_info);
            end;
         dispose (resultvardata);
         VariableInformationForm.free;
      end;
end;

procedure TUserDataForm.DeleteVarButtonClick(Sender: TObject);
  var avar : integer;
      VarNumToDelete, TopVarNum : integer;
   begin
      if not(length(User_data_set_info.var_info) = 0) then
         begin
         if MessageDlg ('Do you really want to delete this variable?  Press "Yes" to delete.', mtwarning, mbyesnocancel, 0) = mryes then
            begin
               {First delete that variable from var info. }
               {Move all var info down 1}
               VarNumToDelete := VarInfoStringGrid.row-1;
               TopVarNum := length(User_data_set_info.var_info)-1;
               if TopVarNum > VarNumToDelete then
                  {move list down}
                  begin
                     for avar := VarNumToDelete to TopVarNum-1 do
                        User_data_set_info.var_info[avar] := User_data_set_info.var_info[avar+1];
                     User_data_set_info.var_info[TopVarNum] := blank_input_data;
                     setlength(User_data_set_info.var_info, length(User_data_set_info.var_info)-1);
                  end
               else if TopVarNum = VarNumToDelete then
                  begin
                     {deleting last var, blank and truncate the element}
                     User_data_set_info.var_info[VarNumToDelete] := blank_input_data;
                     setlength(User_data_set_info.var_info, length(User_data_set_info.var_info)-1);
                  end
               else ShowMessage ('Programming error deleting variable - # to delete > # existing vars!!');
               {If the num vars box matches, then decrement it}
               if User_data_set_info.num_vars = length(User_data_set_info.var_info)+1 then
                  User_data_set_info.num_vars := length(User_data_set_info.var_info);
            end;
         end
      else
         {length of vars is 0, shouldn't ever get here}
         showmessage ('Logical  programming error - delete procedure called with 0 vars in data set.');

      UpdateValues(User_data_set_info);

   end;   {procedure}

procedure TUserDataForm.UnitComboBoxChange(Sender: TObject);
begin
   case UnitComboBox.itemindex of
      0 : User_data_set_info.data_set_unit := no_unit_dataset;
      1 : User_data_set_info.data_set_unit := annual_data;
      2 : User_data_set_info.data_set_unit := country_year;
      3 : User_data_set_info.data_set_unit := directed_dyad_year;
      4 : User_data_set_info.data_set_unit := nondirected_dyad_year;
   end;

end;

procedure TUserDataForm.MoveUpVariableButtonClick(Sender: TObject);
   var VarNumToMove : integer;
       tempvar : one_var_data_type;
begin
      if not(length(User_data_set_info.var_info) = 0) then
         begin
            VarNumToMove := VarInfoStringGrid.row-1;
            {if at top, can't move up}
            if not(VarNumToMove <= 0) then
               begin
                  tempvar := User_data_set_info.var_info[VarNumToMove];
                  User_data_set_info.var_info[VarNumToMove] := User_data_set_info.var_info[VarNumToMove-1];
                  User_data_set_info.var_info[VarNumToMove-1] := tempvar;
                  VarInfoStringGrid.row := VarInfoStringGrid.row-1;
               end;
         end
      else
         {length of vars is 0, shouldn't ever get here}
         showmessage ('Logical  programming error - move up procedure called with 0 vars in data set.');
      UpdateValues(User_data_set_info);
end;

procedure TUserDataForm.MoveDownVariableButtonClick(Sender: TObject);
   var VarNumToMove : integer;
       tempvar : one_var_data_type;
begin
      if not(length(User_data_set_info.var_info) = 0) then
         begin
            VarNumToMove := VarInfoStringGrid.row-1;
            {if at bottom, can't move up}
            if not(VarNumToMove >= length(User_data_set_info.var_info)-1) then
               begin
                  tempvar := User_data_set_info.var_info[VarNumToMove];
                  User_data_set_info.var_info[VarNumToMove] := User_data_set_info.var_info[VarNumToMove+1];
                  User_data_set_info.var_info[VarNumToMove+1] := tempvar;
                  VarInfoStringGrid.row := VarInfoStringGrid.row+1;
               end;
         end
      else
         {length of vars is 0, shouldn't ever get here}
         showmessage ('Logical  programming error - move up procedure called with 0 vars in data set.');
      UpdateValues(User_data_set_info);
end;


procedure TUserDataForm.SaveNoExitButtonClick(Sender: TObject);
   var found, all_values_ok : boolean;
       x, y, numannual, nummonadic, numnondir, numdir,
       numbadunit, numccode1id, numccode2id, numyearid, numversionid : integer;

begin
   all_values_ok := true;

   if config_file_name_set = false then
      begin
         ShowMessage ('You must specify a name for the output configuration file.  Configuration file will be saved, but it is incorrect and the dataset will not be properly accessible in EUGene.');
         all_values_ok := false;
      end;

   With User_data_set_info do
   begin
      if num_vars <> length(User_data_set_info.var_info) then
         begin
            Showmessage ('Number of variables specified not equal to number of defined variables (rows).  Please check variable information or listed number of variables. Configuration file will be saved, but is incorrect.');
            all_values_ok := false;
         end;
      if num_vars <= 0 then
         begin
            Showmessage ('Number of variables must be > 0.  Configuration file will be saved, but it is incorrect and the dataset will not be properly accessible in EUGene.');
            all_values_ok := false;
         end;
      if num_cases <= 0 then
         begin
            Showmessage ('Number of cases must be > 0.  Configuration file will be saved, but it is incorrect and the dataset will not be properly accessible in EUGene.');
            all_values_ok := false;
         end;
      if not(fileexists (User_data_set_info.file_name)) then
         begin
            Showmessage ('Warning:  Specified data file '+User_data_set_info.file_name+' does not exist.  You must ensure that the file exists before this configuration file will work.  Configuration file will be saved, but it is incorrect and the dataset will not be properly accessible in EUGene.');
            {let program proceed here}
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
      if not ((data_set_first_year_possible >= min_year) and (data_set_first_year_possible <= max_year)) then
         begin
            Showmessage ('First year of data set out of bounds.  It must fall between '+inttostr(min_year)+' and '+inttostr(max_year)+'.  Configuration file will be saved, but it is incorrect and the dataset will not be properly accessible in EUGene.');
            all_values_ok := false;
         end;
      if not ((data_set_last_year_possible >= min_year) and (data_set_last_year_possible <= max_year)) then
         begin
            Showmessage ('Last year of data set out of bounds.  It must fall between '+inttostr(min_year)+' and '+inttostr(max_year)+'.  Configuration file will be saved, but it is incorrect and the dataset will not be properly accessible in EUGene.');
            all_values_ok := false;
         end;

      {Check of # of year, ccode identifiers, and unit definition consistency.}
      numccode1id := 0;
      numccode2id := 0;
      numyearid := 0;
      for x := 0 to num_vars - 1 do
         begin
            if var_info[x].var_unit=identifierccode1 then inc(numccode1id);
            if var_info[x].var_unit=identifierccode2 then inc(numccode2id);
            if var_info[x].var_unit=identifieryear then inc(numyearid);
            if var_info[x].var_unit=identifierversion then inc(numversionid);
         end;
      if (numyearid <> 1) then
         begin
            Showmessage ('There must be one and only 1 variable identifying the year of the observation.  Configuration file will be saved, but it is incorrect and the dataset will not be properly accessible in EUGene.');
            all_values_ok := false;
         end;
      if (numversionid <> 1) then
         begin
            Showmessage ('There must be one and only 1 variable identifying the dataset version.  Please ensure that your dataset contains a version variable and that it is entered in the variable list.  Configuration file will be saved, but it is incorrect.');
            all_values_ok := false;
         end;
      if data_set_unit = no_unit_dataset then
         begin
            Showmessage ('Data set unit must be defined.  Configuration file will be saved, but it is incorrect and the dataset will not be properly accessible in EUGene.');
            all_values_ok := false;
         end
         else
            if (data_set_unit=annual_data) and ((numccode1id <> 0) or (numccode2id <> 0)) then
               begin
                  Showmessage ('In an annual data set there cannot be any variables identifying country codes.  This is inconsistent with the data set structure.  Configuration file will be saved, but it is incorrect and the dataset will not be properly accessible in EUGene.');
                  all_values_ok := false;
               end
         else
            if (data_set_unit=country_year) and (numccode2id <> 0) then
               begin
                  Showmessage ('In a monadic data set there cannot be a variable identifying the country code of state 2, as there should be no state 2.  This is inconsistent with the data set structure.  Configuration file will be saved, but it is incorrect.');
                  all_values_ok := false;
               end
         else
            if (data_set_unit=country_year) and (numccode1id <> 1) then
               begin
                  Showmessage ('Monadic data sets must have one variable identifying the country code of state 1.  This is a requirement of the data set structure.  Configuration file will be saved, but it is incorrect and the dataset will not be properly accessible in EUGene.');
                  all_values_ok := false;
               end
         else
            if ((data_set_unit=directed_dyad_year) or (data_set_unit=nondirected_dyad_year)) and
               (numccode1id <> 1) and (numccode2id <> 1) then
               begin
                  Showmessage ('Dyadic data sets must have one country code identifier variable for state 1 and for state 2.  This is a requirement of the data set structure.  Configuration file will be saved, but it is incorrect.');
                  all_values_ok := false;
               end;
      if (numccode1id > 1) or (numccode2id > 1) then
         begin
            Showmessage ('There must be one and only 1 variable identifying the ccodes of the states.  Multiple identifying variables were observed.  Configuration file will be saved, but it is incorrect and the dataset will not be properly accessible in EUGene.');
            all_values_ok := false;
         end;



      {Now check the variable units for consistency with data set.}
      {Annual vars can be in any type of data set.
       monadic vars can be in country_year, directed_dyad_year, nondirected_dyad_year datasets only.
       dyadic_ordered vars can be in directed or nondir dyad_year datasets only.
       dyadic_unordered can be in directed or nondirected dyadic only.}

      numannual := 0;
      nummonadic := 0;
      numnondir := 0;
      numdir := 0;
      numbadunit := 0;
      for x := 0 to num_vars - 1 do
         begin
            case var_info[x].var_unit of
               annual : inc(numannual);
               monadic : inc(nummonadic);
               dyadic_unordered : inc(numnondir);
               dyadic_ordered : inc(numdir);
               identifierccode1, identifierccode2, identifieryear, identifierversion : begin end; {nothing}
               no_unit_variable : inc(numbadunit);
            else
               showMessage ('unknown var type seen in checking variable unit consistency.  Programming error - notify programmer.');
            end   {case}
         end;
      if numbadunit > 0 then
         begin
            Showmessage ('Variable specification includes '+inttostr(numbadunit)+' variable(s) without the variable type (annual, monadic, ordered-dyadic, or unordered-dyadic) set.  Configuration file will be saved, but it is incorrect and the dataset will not be properly accessible in EUGene.');
            all_values_ok := false;
         end;
      {only two problematic cases here, for annual and monadic dat sets}
      case data_set_unit of
            country_year : if (numdir>0) or (numnondir>0) then
               begin
                  Showmessage ('Variable specification includes dyadic variables, but data set type is monadic (country-year).  This is inconsistent and not allowed.  Configuration file will be saved, but it is incorrect and the dataset will not be properly accessible in EUGene.');
                  all_values_ok := false;
               end;
            annual_data : if (numdir>0) or (numnondir>0) or (nummonadic>0) then
               begin
                  Showmessage ('Variable specification includes monadic or dyadic variables, but data set type is annual.  This is inconsistent and not allowed.  Configuration file will be saved, but it is incorrect and the dataset will not be properly accessible in EUGene.');
                  all_values_ok := false;
               end;
         end;  {case}

      {Check var reversed}
      {Go through var list.  For each var that has a reverse listed, make sure it's on the list.
       And check units that are required.  Reverse only needed in nondir dyad year data.}
      {if var reversed not valid, set to "none"}
      if data_set_unit = nondirected_dyad_year then
      for x := 0 to num_vars - 1 do
         begin
            {First, set any blank names to 'none'}
            if var_info[x].var_reversed_var = '' then var_info[x].var_reversed_var := 'none';
            {Now check if required reverse is listed, and if name is OK.}
            if (var_info[x].var_unit=dyadic_ordered) or
               (var_info[x].var_unit=monadic) then
            {The reverse variable must exist}
            begin
               y := 0;
               found := false;
               while not(found) and (y <= num_vars) do
                  begin
                     if var_info[y].var_name = var_info[x].var_reversed_var then
                        found := true;
                          {OK!  reverse var found}
                     inc(y);
                  end;
               if not found then
                  begin
                     ShowMessage ('Variable '+var_info[y].var_name+' requires a complementary variable in the data set because this is a nondirected data set, and because the variable has direction (is ordered dyadic or monadic).  Configuration file will be saved, but it is incorrect.');
                     all_values_ok := false;
                  end;
            end;
         end;

   end;           {with User_data_set_info}

  {Save all data here}
  write_one_user_data_file_info(Config_file_name, User_data_set_info);

  {used to save only if configuration file was correct.}
  {if all_values_ok then
      begin
         write_one_user_data_file_info(Config_file_name, User_data_set_info);
      end;   }

end;    {proc save button click}

procedure TUserDataForm.QuitNoSaveButtonClick(Sender: TObject);
begin
   close;
end;

procedure TUserDataForm.HelpBtnClick(Sender: TObject);
begin
   ShowMessage ('To access the variables in your dataset, EUGene requires a "specification file" containing key information about your data.  This window allows you to enter and save this required information.  Further details are provided in the printed documentation.');
end;


procedure TUserDataForm.AutoDetectNameButtonClick(Sender: TObject);
   type idlisttype = (id1, id2, yearid, versionid);
   var all_good_names : boolean;
       haveyearidentifier, haveccode1identifier, haveccode2identifier, haveversionidentifier : boolean;
       x, avar, line, num_vars_counted : integer;
       invar : string;
       indatafile : textfile;
       rereadvarsOK : boolean;
       could_be_real, could_be_numeric, is_string : boolean;
       first_char : integer;
       prev_type : array of integer;

   function onlist (avarname : string; listtype: idlisttype) : boolean;
      var id1list, id2list, idyearlist, versionlist : array of string;
          x : integer;
          found : boolean;
      begin
         found := false;
         setlength (id1list, 10);
         id1list[0] := 'ccodea';
         id1list[1] := 'ccode1';
         id1list[2] := 'state1';
         id1list[3] := 'statea';
         id1list[4] := 'ccode';
         id1list[5] := 'state';
         id1list[6] := 'stnum';
         id1list[7] := 'stnuma';
         id1list[8] := 'stanum';
         id1list[9] := 'sta';
         setlength (id2list, 7);
         id2list[0] := 'ccodeb';
         id2list[1] := 'ccode2';
         id2list[2] := 'state2';
         id2list[3] := 'stateb';
         id2list[4] := 'stnumb';
         id2list[5] := 'stbnum';
         id2list[6] := 'stb';
         setlength (idyearlist, 2);
         idyearlist[0] := 'year';
         idyearlist[1] := 'yr';
         setlength (versionlist, 3);
         versionlist[0] := 'version';
         versionlist[1] := 'v';
         versionlist[2] := 'ver';
         case listtype of
               id1: for x := 0 to length(id1list)-1 do if lowercase(avarname)=id1list[x] then found := true;
               id2: for x := 0 to length(id2list)-1 do if lowercase(avarname)=id2list[x] then found := true;
               yearid: for x := 0 to length(idyearlist)-1 do if lowercase(avarname)=idyearlist[x] then found := true;
               versionid: for x := 0 to length(versionlist)-1 do if lowercase(avarname)=versionlist[x] then found := true;
            end;  {case}
         if found = true then result := true else result := false;
      end;  {function}

begin
   {Can look for:
    # variables (based on commas, since csv file
    var names, if alphanumeric in first line [look at setting for label line?]
    If have var names, look for identifier vars.
    If have identifier vars, scan for first year, last year, # cases.}
   try
      assignfile(indatafile, User_data_set_info.file_name);

      rereadvarsOK := false;
      if User_data_set_info.num_vars = 0 then
         rereadvarsOK := true
      else if MessageDlg ('You already have variables defined.  Do you want to attempt to read new variable names from data file?  This will reset current variable information.  Press "Yes" to read from file, or "No" to keep current names.', mtwarning, mbyesnocancel, 0) = mryes then
         rereadvarsOK := true;

      if rereadvarsOK then
         begin
            num_vars_counted := 0;
            reset (indatafile);
            {Since csv, scan 1st line for # of csv vars}
            while not(eoln(indatafile)) and not(eof(indatafile)) do
               begin
                  invar := read_csv_string(indatafile);
                  inc(num_vars_counted);
               end;
            closefile (indatafile);
            if num_vars_counted = 0 then showMessage ('No variables seen in dataset!');
            setlength (User_data_set_info.var_info, num_vars_counted);

            {Need to set up all blank variables for these}
            for avar := 0 to num_vars_counted-1 do User_data_set_info.var_info[avar] := blank_input_data;

            User_data_set_info.num_vars := num_vars_counted;

            if User_data_set_info.num_vars > 0 then
               begin
                  {now try to read names for each var}
                  reset (indatafile);
                  all_good_names := true;
                  for avar := 0 to User_data_set_info.num_vars-1 do
                     begin
                        invar := read_csv_string(indatafile);
                        if (invar[1] in ['a'..'z']) or (invar[1] in ['A'..'Z']) then
                           {this is probably a var name, treat it as such}
                           User_data_set_info.var_info[avar].var_name := invar
                           else
                              begin
                                 all_good_names := false;
                                 User_data_set_info.var_info[avar].var_name := 'var'+inttostr(avar);
                              end;
                     end;
                  {Skip to next line}
                  readln (indatafile);
                  if all_good_names = true then User_data_set_info.label_line := true
                  else
                     begin
                        ShowMessage ('Unable to read user-defined variable names from this data file - at least one variable does not have a valid name (starting with a..z or A..Z) on line 1 of the data file.');
                        User_data_set_info.label_line := false
                     end;

                  {Now, set var types}
                  for avar := 0 to User_data_set_info.num_vars - 1 do
                     begin
                        {if blank record look at name}
                        if User_data_set_info.var_info[avar].var_unit = no_unit_variable then
                           {need to look at name}
                           begin
                              {Try to guess at data set unit by what names we see}
                              if onlist(User_data_set_info.var_info[avar].var_name, id1) then
                                 begin
                                    User_data_set_info.var_info[avar].var_unit := identifierccode1;
                                    haveccode1identifier := true;
                                 end
                              else
                              if onlist(User_data_set_info.var_info[avar].var_name, id2) then
                                 begin
                                    User_data_set_info.var_info[avar].var_unit := identifierccode2;
                                    haveccode2identifier := true;
                                 end
                              else
                              if onlist(User_data_set_info.var_info[avar].var_name, versionid) then
                                 begin
                                    User_data_set_info.var_info[avar].var_unit := identifierversion;
                                    haveversionidentifier := true;
                                 end
                              else
                              if onlist(User_data_set_info.var_info[avar].var_name, yearid) then
                                 begin
                                    User_data_set_info.var_info[avar].var_unit := identifieryear;
                                    haveyearidentifier := true;
                                 end;
                           end
                     end;

                  {Now try to guess at variable types - real, integer, string}
                  {Read and check 3 lines.}
                  setlength(prev_type,User_data_set_info.num_vars);
                  for avar := 0 to User_data_set_info.num_vars-1 do
                    prev_type[avar] := varinteger;
                  for line := 1 to 3 do
                  begin
                     for avar := 0 to User_data_set_info.num_vars-1 do
                        begin
                           invar := read_csv_string(indatafile);
                           is_string := false;
                           could_be_numeric := false;
                           could_be_real := false;

                           if invar='' then     {if missing in first case, set to integer}
                              User_data_set_info.var_info[avar].var_type := varinteger
                           else
                              begin
                                 first_char := 0;
                                 repeat
                                    inc(first_char);
                                 until (invar[first_char] in ['a'..'z','A'..'Z','0'..'9','-','.']) or (first_char=length(invar));

                                 if (invar[first_char] in ['0'..'9']) or (invar[1]='-') or (invar[1]='.') then
                                    could_be_numeric := true;
                                 for x := first_char to length(invar) do
                                    begin
                                       {if there is ever an alphabetic char, it's string}
                                       if (invar[x] in ['a'..'z']) or (invar[x] in ['A'..'Z']) then
                                          begin
                                             is_string := true;
                                             could_be_real := false;
                                             could_be_numeric := false;
                                          end;
                                       if (invar[x] = '.') then
                                          could_be_real := true;
                                    end;

                                 {If it ever has an alphabetic, is string, whatever we saw before. }
                                 if is_string then
                                    User_data_set_info.var_info[avar].var_type := varolestr
                                 else  {no alphabetic, is it real or integer?}
                                    if could_be_numeric then   {1st char is numeric}
                                       begin
                                          {If it ever has a period but no alphabetic, then it's real}
                                          {If it starts with a #, has no alphabetic, and has no ., then it's integer;}
                                          if could_be_real   {had a period}
                                             then
                                                begin
                                                   {it's real as long as previous case didn't have alpha char}
                                                   if prev_type[avar] <> varoleStr then User_data_set_info.var_info[avar].var_type := varsingle;
                                                end
                                          else
                                             {it's an integer, as long as a prior type didn't say something different.  This if stmt should be redundant, but it's another check on code.}
                                             if prev_type[avar] = varinteger then User_data_set_info.var_info[avar].var_type := varinteger;
                                       end
                                    else    {couldn't be numeric, this shouldn't happen}
                                       begin
                                          ShowMessage ('Notify programmer of error in identifying variable type - impossible combo seen.  Invar = '+invar);
                                       end;
                                 prev_type[avar] := User_data_set_info.var_info[avar].var_type;
                              end;   {not missing in first case}
                        end;       {for avar}
                        if not eof(indatafile) then readln (indatafile);
                     end;   {for line}

                  {now guess at data set type}
                  if User_data_set_info.data_set_unit = no_unit_dataset {not defined} then
                     begin
                        if haveccode2identifier then User_data_set_info.data_set_unit := directed_dyad_year
                        else if haveccode1identifier then User_data_set_info.data_set_unit := country_year
                        else if haveyearidentifier then User_data_set_info.data_set_unit := annual_data;
                     end;

                  {Now, leave other values (reverse, missing, var_unit) at blank values}

                  {update a couple of values to show they are not known}
                  User_data_set_info.data_set_first_year_possible := min_year;
                  User_data_set_info.data_set_last_year_possible := min_year;
                  User_data_set_info.num_cases := 0;

                  UpdateValues(User_data_set_info);

                  closefile(indatafile);
               end;
         end;        {Read vars0}

   except
      {some kind of I/O error}
      ShowMessage ('Error reading data file.  Please check file and run and try again.');
   end;   {except}
end;              {procedure autocheck click}

procedure TUserDataForm.AutoDetectYearCaseButtonClick(Sender: TObject);
   var haveyearidentifier, haveccode1identifier, haveccode2identifier : boolean;
       avar, cases_read, yearval : integer;
       indatafile : textfile;
       current_low_year, current_high_year : year_range;
       invar : string;
begin
   try
      assignfile(indatafile, User_data_set_info.file_name);
      {Now, have variables.  Look for identifier vars.  These should be predefined at this point.}
      if User_data_set_info.num_vars = 0 then
         ShowMessage ('No variables defined yet - read or define variables before scanning for year values and # cases')
      else
         begin
            haveyearidentifier := false;
            haveccode1identifier := false;
            haveccode2identifier := false;

            for avar := 0 to User_data_set_info.num_vars - 1 do
               begin
                  if User_data_set_info.var_info[avar].var_unit = identifierccode1 then
                     haveccode1identifier := true else
                  if User_data_set_info.var_info[avar].var_unit = identifierccode2 then
                     haveccode2identifier := true else
                  if User_data_set_info.var_info[avar].var_unit = identifieryear then
                     haveyearidentifier := true;
              end;   {for avar}

            if haveyearidentifier then {look for #cases, first year, last year}
               begin
                  {This may take a while, so disable main form while searching...}
                  UserDataForm.enabled := false;
                  reset (indatafile);
                  cases_read := 0;

                  {Read year value from first line}
                  if User_data_set_info.label_line then readln(indatafile);
                  avar := 0;
                  while User_data_set_info.var_info[avar].var_unit <> identifieryear do
                     begin
                        invar := read_csv_string(indatafile);
                        inc(avar);
                     end;
                  {now at position because of "while", read the value}
                  invar := read_csv_string(indatafile);
                  if not (User_data_set_info.var_info[avar].var_unit = identifieryear) then
                     ShowMessage ('Programming error in auto read values - read to idyear but its not idyear')
                  else
                     {have the 1st year identifier var}
                    begin
                       if not((strtointdef(invar,0) >= min_year) and (strtointdef(invar,0) <= max_year)) then
                          begin
                             ShowMessage ('Error in Data file - values under year are out of range.  This data file either has errors or cannot be processed by EUGene.  Drop years outside the acceptable range '+inttostr(min_year)+' to '+inttostr(max_year)+' and try again.  Program is aborting now.');
                             halt;
                          end
                       else   {ok years}
                          begin
                             yearval := strtoint(invar);
                             current_low_year := yearval;
                             current_high_year := yearval;
                          end;
                    end;
                  readln(indatafile);
                  inc(cases_read);

                  {now look for values in rest of file}
                  while not eof(indatafile) do
                     begin
                        avar := 0;
                        while User_data_set_info.var_info[avar].var_unit <> identifieryear do
                           begin
                              invar := read_csv_string(indatafile);
                              inc(avar);
                           end;
                        {now at position because of "while", read the value}
                        invar := read_csv_string(indatafile);
                        {Now invar is my var_unit}
                        if not (User_data_set_info.var_info[avar].var_unit = identifieryear) then
                           ShowMessage ('Programming error in auto read values - read to idyear but its not idyear')
                        else
                           {hvae theh year identifier var}
                          begin
                             yearval := strtoint(invar);
                             if yearval < current_low_year then current_low_year := yearval else
                             if yearval > current_high_year then current_high_year := yearval;
                          end;
                        readln(indatafile);
                        inc(cases_read);
                     end;       {while not eof}
                  closefile(indatafile);
                  User_data_set_info.data_set_first_year_possible := current_low_year;
                  User_data_set_info.data_set_last_year_possible := current_high_year;
                  User_data_set_info.num_cases := cases_read;
               end;    {have year identifier}

            UpdateValues(User_data_set_info);
            UserDataForm.enabled := true;
         end;       {numvars was > 0;  we have variables}
   except
      {some kind of I/O error}
      ShowMessage ('Error reading data file.  Please check file and run and try again.');
      UserDataForm.enabled := true;
   end;   {except}

   end;

procedure TUserDataForm.LongNameEditChange(Sender: TObject);
begin
   User_data_set_info.data_set_full_name := LongNameEdit.text;
end;

procedure TUserDataForm.ShortNameEditChange(Sender: TObject);
begin
   User_data_set_info.data_set_short_name := ShortNameEdit.text;
end;


procedure TUserDataForm.LabelLineCheckBoxClick(Sender: TObject);
begin
   if LabelLineCheckBox.checked then AutoDetectNameButton.enabled := true
   else AutoDetectNameButton.enabled := false;
end;

end.


