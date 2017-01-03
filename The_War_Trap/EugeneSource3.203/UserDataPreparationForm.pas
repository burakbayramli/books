unit UserDataPreparationForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Menus, math, EUTypes1, UserDataVarForm, cmnprocd,
  ExtCtrls;

type
  TUserDataPrepForm = class(TForm)
    SaveDialog1: TSaveDialog;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    OpenDialog1: TOpenDialog;
    DoneBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure DoneBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure LoadExistingConfigurationFile1Click(Sender: TObject);
    procedure SaveCurrentConfiguration1Click(Sender: TObject);
    procedure ExitUserDataSetPreparation1Click(Sender: TObject);
  private
    { Private declarations }
  public
  end;

   procedure UpdateValues (User_data_set_info : data_set_info_record);
   procedure UnitComboBoxChange(Sender: TObject);
   procedure ConfigFileNewNameBtnClick(Sender: TObject);
   procedure ConfigFileLoadBtnClick(Sender: TObject);
   procedure DataFileSelectButtonClick(Sender: TObject);
   procedure LongNameEditChange(Sender: TObject);
   procedure ShortNameEditChange(Sender: TObject);
   procedure CitationEditChange(Sender: TObject);
   procedure set_variable_buttons;
   procedure VarInfoStringGridSelectCell(Sender: TObject; ACol,
             ARow: Integer; var CanSelect: Boolean);
   procedure EditVarButtonClick(Sender: TObject);
   procedure AddVariableButtonClick(Sender: TObject);
   procedure DeleteVarButtonClick(Sender: TObject);
   procedure MoveUpVariableButtonClick(Sender: TObject);
   procedure MoveDownVariableButtonClick(Sender: TObject);
   procedure LabelLineCheckBoxClick(Sender: TObject);
   procedure AutoDetectNameButtonClick(Sender: TObject);
   procedure FirstYearMaskEditChange(Sender: TObject);
   procedure LastYearMaskEditChange(Sender: TObject);
   procedure NumCasesMaskEditChange(Sender: TObject);
   procedure AutoDetectYearCaseButtonClick(Sender: TObject);

var
  UserDataPrepForm: TUserDataPrepForm;
  User_data_set_info : data_set_info_record;
  Config_file_name : TFileName;
  blank_input_data : one_var_data_type;
  Config_file_name_set : boolean;

implementation

uses UserDataSetSub1, UserDataSetSub2, UserDataSetSub3, mdiframe,
  FTPConnect, GetFilesForUpload;

{$R *.DFM}

{ ---------------------------------------------------------------  }

procedure UpdateValues (User_data_set_info : data_set_info_record);
   {procedure takes all values from user data set record and displays in form.}
   var avar : integer;
   begin
      with UserDataPrepForm do
         begin
         with UserDataSetSubForm1 do
            begin
               ConfigFileEdit.text := Config_file_name;
               DataFileEdit.Text := User_data_set_info.file_name;
               LongNameEdit.text := User_data_set_info.data_set_full_name;
               ShortNameEdit.text := User_data_set_info.data_set_short_name;
            end;

         with UserDataSetSubForm2 do
            begin
               LabelLineCheckBox.checked := (User_data_set_info.label_line=true);
               NumVarsMaskEdit.text := inttostr(User_data_set_info.num_vars);
               case User_data_set_info.data_set_unit of
                  no_unit_dataset : UnitComboBox.itemindex := 0;
                  annual_data : UnitComboBox.itemindex := 1;
                  country_year : UnitComboBox.itemindex := 2;
                  directed_dyad_year : UnitComboBox.itemindex := 3;
                  nondirected_dyad_year : UnitComboBox.itemindex := 4;
               end;
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
               Set_Variable_Buttons;
            end;

         with UserDataSetSubForm3 do
            begin
               FirstYearMaskEdit.text := inttostr(User_data_set_info.data_set_first_year_possible);
               LastYearMaskEdit.text := inttostr(User_data_set_info.data_set_last_year_possible);
               NumCasesMaskEdit.text := inttostr(User_data_set_info.num_cases);
            end;

         end;   {with UserDataPrepForm}
   end;   {proc showvalues}


procedure TUserDataPrepForm.FormCreate(Sender: TObject);
   {var baseSelection : TGridRect;}
begin
   {Initialize all fields to acceptable and meaningless values}

   Config_file_name := 'No configuration file identified';
   config_file_name_set := false;
   ForceCurrentDirectory := true;
   {UserDataPrepForm.OpenDialog1.initialdir := ExtractFilePath(Application.ExeName);
    UserDataPrepForm.SaveDialog1.initialdir := UserDataPrepForm.OpenDialog1.initialdir;}

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

   with UserDataSetSubForm1 do
      begin
      end;

   with UserDataSetSubForm2 do
      begin
         LabelLineCheckBox.checked := false;
         AutoDetectNameButton.enabled := false;
         UnitComboBox.items[0] := 'Select Data Unit...';
         UnitComboBox.items[1] := 'Year (Annual Data)';
         UnitComboBox.items[2] := 'Country-Year (Monadic Data)';
         UnitComboBox.items[3] := 'Directed Dyad-Year';
         UnitComboBox.items[4] := 'Nondirected Dyad-Year';
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
               colwidths[0] := 58;
               colwidths[1] := 100;
               colwidths[2] := 70;
               colwidths[3] := 115;
               colwidths[4] := 165;
               colwidths[5] := 95;
               {width := colwidths[0]+colwidths[1]+colwidths[2]+colwidths[3]+colwidths[4]+colwidths[5];}
               {Now, set none to be initially selected.}
               {BaseSelection.left := 0;
                BaseSelection.right := 0;
                BaseSelection.top := 0;
                BaseSelection.bottom := 0;
                selection := BaseSelection; }
               VarInfoStringGrid.row := 0;
            end;
      end;

   with UserDataSetSubForm3 do
      begin
         AutoDetectYearCaseButton.enabled := false;
      end;

   UpdateValues (User_data_set_info);

   {Create a blank variable to use for initializations later}
   blank_input_data.var_name := '';
   blank_input_data.var_type := varinteger;
   blank_input_data.var_unit := no_unit_variable;
   blank_input_data.var_missing_value := -9;
   blank_input_data.var_reversed_var := 'none';

   {And only the first button is enabled to start}
   Button1.enabled := true;
   Button2.enabled := false;
   Button3.enabled := false;
   Button4.enabled := false;
   Button5.enabled := false;
end;


procedure config_file_save;
begin
  {Save all data here}
  write_one_user_data_file_info(Config_file_name, User_data_set_info);
  {Procedure is actually in EUTypes1 unit.}

  {used to save only if configuration file was correct.}
  {if all_values_ok then
      begin
         write_one_user_data_file_info(Config_file_name, User_data_set_info);
      end;   }
end;


{ ---------------------------------------------------------------  }

procedure TUserDataPrepForm.HelpBtnClick(Sender: TObject);
begin
   ShowMessage ('To access the variables in your dataset, EUGene requires a "configuration file" containing key information about your data.  Follow the steps in this window to enter this required information.  Further details are provided in the printed documentation.');
end;

procedure TUserDataPrepForm.CancelBtnClick(Sender: TObject);
begin
   close;
end;

procedure TUserDataPrepForm.DoneBtnClick(Sender: TObject);
begin
   close;
end;

procedure TUserDataPrepForm.LoadExistingConfigurationFile1Click(Sender: TObject);
begin
   ConfigFileLoadBtnClick(sender);
end;

procedure TUserDataPrepForm.SaveCurrentConfiguration1Click(Sender: TObject);
begin
  Config_file_save;
end;

procedure TUserDataPrepForm.ExitUserDataSetPreparation1Click(Sender: TObject);
begin
  close;
end;

{ ---------------------------------------------------------------  }

procedure TUserDataPrepForm.Button1Click(Sender: TObject);
begin
   if UserDataSetSubForm1.showmodal = mrOK then
   Button2.enabled := true;
end;

procedure TUserDataPrepForm.Button2Click(Sender: TObject);
begin
   if UserDataSetSubForm2.showmodal = mrOK then
   Button3.enabled := true;
end;

procedure TUserDataPrepForm.Button3Click(Sender: TObject);
begin
   if UserDataSetSubForm3.showmodal = mrOK then
   Button4.enabled := true;
end;

procedure TUserDataPrepForm.Button4Click(Sender: TObject);
begin
  Config_file_save;
  Button5.enabled := true;
end;

procedure TUserDataPrepForm.Button5Click(Sender: TObject);
var x : integer;
    badtransfer : boolean;
    {UploadFileNames : Tstrings;}
begin
   try
      try
         {put file names on form, then act like clicked upload button in ftpConnect}
         FilesForUploadForm.DataFileEdit.text := UserDataSetSubForm1.DataFileEdit.text;
         FilesForUploadForm.ConfigFileEdit.text := UserDataSetSubForm1.ConfigFileEdit.text;
         FilesForUploadForm.DocFileEdit.text := UserDataSetSubForm1.DocFileEdit.text;
         ftp_form.show;
         ftp_form.ConnectButton.Click;
         ftp_form.UploadButton.Click;
         ftp_form.CloseButton.Click;
      except
         badtransfer := true;
         MessageDlg ('Unable to transfer files or change to uploads directory:  Check for possible connection error.', mterror, [mbOK], 0);
      end;
   finally
      if not badtransfer then ShowMessage ('Dataset Upload Complete!  Please note that your file will not appear immediately for user downloading.  Notify sbennett@psu.edu and allan.stam@dartmouth.edu that your dataset has been uploaded.  The files will be made available to users after review.');
   end;

end;

{       Application.CreateForm(TFTP_Form, FTP_Form);

       {upload here}
{      ftpconnect.UploadFileNames.clear;

      {put file names on form}

{      FilesForUploadForm.DataFileEdit.text := UserDataSetSubForm1.DataFileEdit.text;
      FilesForUploadForm.ConfigFileEdit.text := UserDataSetSubForm1.ConfigFileEdit.text;
      FilesForUploadForm.DocFileEdit.text := UserDataSetSubForm1.DocFileEdit.text;
      FilesForUploadForm.selectDataButton.caption := 'Change file...';
      FilesForUploadForm.selectedfButton.caption := 'Change file...';
      FilesForUploadForm.selectDocFileButton.caption := 'Change file...';

      {putting file names in upload list will be done by the 3 box window.}
      {ftpconnect.UploadFilenames.add (UserDataSetSubForm1.DataFileEdit.text);
      ftpconnect.UploadFilenames.add (UserDataSetSubForm1.ConfigFileEdit.text);
      ftpconnect.UploadFilenames.add (UserDataSetSubForm1.DocFileEdit.text);}

{      if FilesForUploadForm.showmodal = mrOK then
         begin
            try
              badtransfer := true;
              FTP_Form.show;
              disable_most_buttons;   {keeps cancel, close, help}
{              try
                 if not ftp_form.IdFTP1.Connected then
                     ftp_form.ConnectButtonClick (self);
                 if ftp_form.IdFTP1.Connected then
                    try
                       try
                           ftp_form.IdFTP1.ChangeDir('\uploads');             {change on server to this}
{                           ftp_form.IdFTP1.Mode(MODE_ASCII);
                           for x := 0 to ftpconnect.UploadFileNames.count-1 do
                                transfer_up (ftpconnect.UploadFileNames.strings[x]);
                           ftp_form.NMFTP1.ChangeDir('\');             {change on server to this}
{                           ftp_form.ConnectButtonClick(self);
                       finally
                           badtransfer := false;
                       end;
                    except
                         ftp_form.enable_all_buttons;
                         MessageDlg ('Unable to transfer file or change to uploads directory:  Check for possible connection error.', mterror, [mbOK], 0);
                         badtransfer := true;
                    end;  {outer except loop}
{              except
                 badtransfer := true;
              end;
            finally
               ftp_form.enable_all_buttons;
               if not badtransfer then ShowMessage ('Dataset Upload Complete!  Please note that your file will not appear immediately for user downloading.  Notify sbennett@psu.edu and allan.stam@dartmouth.edu that your dataset has been uploaded.  The files will be made available to users after review.');
               FTP_Form.close;
            end;
         end
      else
         begin
         {do nothing, no transfer}
{         end;
   finally
      FTP_Form.free;
   end;
}

{ ---------------------------------------------------------------  }


procedure UnitComboBoxChange(Sender: TObject);
begin
   case UserDataSetSubForm2.UnitComboBox.itemindex of
      0 : User_data_set_info.data_set_unit := no_unit_dataset;
      1 : User_data_set_info.data_set_unit := annual_data;
      2 : User_data_set_info.data_set_unit := country_year;
      3 : User_data_set_info.data_set_unit := directed_dyad_year;
      4 : User_data_set_info.data_set_unit := nondirected_dyad_year;
   end;
end;

procedure ConfigFileNewNameBtnClick(Sender: TObject);
begin
   with UserDataPrepForm do
   begin
      SaveDialog1.Filter :='Configuration files (*.edf)|*.edf|Text files (*.txt)|*.txt|All files (*.*)|*.*';
      SaveDialog1.FileName := Config_file_name;
      SaveDialog1.Title := 'Specify User Configuration File for Output';
      SaveDialog1.Options := [ofOverwritePrompt, ofHideReadOnly];

      if SaveDialog1.execute then
            begin
               {check and see if .edf extension.  If not, make it that.}
               if (extractFileExt(SaveDialog1.FileName) <> '.'+eugene_data_file_extension) then
                  begin    {add .edf}
                     SaveDialog1.FileName := SaveDialog1.FileName + '.' + eugene_data_file_extension;
                  end;
               UserDataSetSubForm1.ConfigFileEdit.text := SaveDialog1.FileName;
               Config_file_name := SaveDialog1.FileName;
               config_file_name_set := true;
            end
         else {exited with cancel}
            begin
            end;
   end;
end;

procedure ConfigFileLoadBtnClick(Sender: TObject);
   var baddataset : boolean;
begin
   with UserDataPrepForm do
   begin
      OpenDialog1.Filter :='Configuration files (*.edf)|*.edf|Text files (*.txt)|*.txt|All files (*.*)|*.*';
      OpenDialog1.FileName := Config_file_name;
      OpenDialog1.Title := 'Load Information from Configuration File';
      {no .options specified.}
      if OpenDialog1.execute then
            begin
               UserDataSetSubForm1.ConfigFileEdit.text:=OpenDialog1.FileName;
               Config_file_name := OpenDialog1.FileName;
               Read_one_user_data_file_info(Config_file_name, User_data_set_info, baddataset);
               if baddataset then
                  showmessage ('Errors in input configuration file '+Config_file_name+'.  Please check all values before saving new configuration file')
               else
                  begin
                     UserDataSetSubForm2.AutoDetectNameButton.enabled := true;
                     UserDataSetSubForm3.AutoDetectYearCaseButton.enabled := true;
                  end;
               config_file_name_set := true;
               UpdateValues (User_data_set_info);
            end
         else {exited with cancel}
            begin
            end;
   end;
end;

procedure DataFileSelectButtonClick(Sender: TObject);
begin
   with UserDataPrepForm do
   begin
      OpenDialog1.Filter :='Comma-separated files (*.csv)|*.csv|Data files (*.dat)|*.dat|Text files (*.txt)|*.txt|All files (*.*)|*.*';
      OpenDialog1.FileName := User_data_set_info.file_name;
      OpenDialog1.Title := 'Locate User Data File';
      if OpenDialog1.execute then
            begin
               UserDataSetSubForm1.DataFileEdit.text:=OpenDialog1.FileName;
               User_data_set_info.file_name := OpenDialog1.FileName;
               UserDataSetSubForm3.AutoDetectYearCaseButton.enabled := true;
               {add a likely data configuration file name if user has not entered one.}
               if (UserDataSetSubForm1.configfileedit.text = '') or
                  (UserDataSetSubForm1.configfileedit.text = 'No configuration file identified') then
                  begin
                     UserDataSetSubForm1.configfileedit.text := copy(OpenDialog1.FileName, 0, length(OpenDialog1.FileName)-3) + 'edf';
                     Config_file_name := UserDataSetSubForm1.configfileedit.text;
                     config_file_name_set := true;
                  end;
            end
         else {exited with cancel}
            begin
            end;
   end;
end;

procedure LongNameEditChange(Sender: TObject);
begin
   User_data_set_info.data_set_full_name := UserDataSetSubForm1.LongNameEdit.text;
end;

procedure ShortNameEditChange(Sender: TObject);
begin
   User_data_set_info.data_set_short_name := UserDataSetSubForm1.ShortNameEdit.text;
end;

procedure CitationEditChange(Sender: TObject);
begin
   User_data_set_info.data_set_citation := UserDataSetSubForm1.CitationEdit.Text;
end;

{ ---------------------------------------------------------------  }

procedure set_variable_buttons;
  {enables/disables variable buttons when user selects a row/variable.}
   begin
      with UserDataSetSubForm2 do
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

procedure VarInfoStringGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
   with UserDataSetSubForm2 do
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
end;

procedure EditVarButtonClick(Sender: TObject);
  var resultvardata : one_var_data_type_ptr;
begin
   if User_data_set_info.data_set_unit = no_unit_dataset then
      ShowMessage ('Data set unit of analysis must be defined before adding or editing variables')
   else
      begin
         with UserDataSetSubForm2 do
         begin
            new (resultvardata);
            VariableInformationForm := TVariableInformationForm.createWithInfo(User_data_set_info.var_info[VarInfoStringGrid.row-1],
                                       User_data_set_info.data_set_unit, resultvardata, UserDataPrepForm);
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
end;

procedure AddVariableButtonClick(Sender: TObject);
  var resultvardata : one_var_data_type_ptr;
begin
   if User_data_set_info.data_set_unit = no_unit_dataset then
      ShowMessage ('Data set unit of analysis must be defined before adding or editing variables')
   else
      begin
         {Do edit, but pass in a blank record.}
         new (resultvardata);
         VariableInformationForm := TVariableInformationForm.createWithInfo(blank_input_data,
                                    User_data_set_info.data_set_unit, resultvardata, UserDataPrepForm);
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

procedure DeleteVarButtonClick(Sender: TObject);
  var avar : integer;
      VarNumToDelete, TopVarNum : integer;
begin
   with UserDataSetSubForm2 do
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
   end;
end;   {procedure}


procedure MoveUpVariableButtonClick(Sender: TObject);
   var VarNumToMove : integer;
       tempvar : one_var_data_type;
begin
   with UserDataSetSubForm2 do
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
end;

procedure MoveDownVariableButtonClick(Sender: TObject);
   var VarNumToMove : integer;
       tempvar : one_var_data_type;
begin
   with UserDataSetSubForm2 do
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
end;


procedure LabelLineCheckBoxClick(Sender: TObject);
begin
   if UserDataSetSubForm2.LabelLineCheckBox.checked then UserDataSetSubForm2.AutoDetectNameButton.enabled := true
   else UserDataSetSubForm2.AutoDetectNameButton.enabled := false;
end;


procedure AutoDetectNameButtonClick(Sender: TObject);
   type idlisttype = (id1, id2, yearid, versionid);
   var all_good_names : boolean;
       haveyearidentifier, haveccode1identifier, haveccode2identifier, haveversionidentifier : boolean;
       x, avar, line, num_vars_counted : integer;
       invar : string;
       indatafile : textfile;
       rereadvarsOK : boolean;
       could_be_real, could_be_numeric, is_string : boolean;
       first_char : integer;
       prev_type : TIntegerArray;

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
      ShowMessage ('Error reading data file in Autodetect names.  Please check file and run and try again.');
      closefile(indatafile);
   end;   {except}
end;              {procedure autocheck click}


{ ---------------------------------------------------------------  }


procedure FirstYearMaskEditChange(Sender: TObject);
begin
   try
      User_data_set_info.data_set_first_year_possible := strtoint(UserDataSetSubForm3.FirstYearMaskEdit.text);
   except
      ShowMessage ('First year value not acceptable (out of range or non-numeric).  Resetting to 1800;  please reenter.');
      User_data_set_info.data_set_first_year_possible := 1800;
      UserDataSetSubForm3.FirstYearMaskEdit.text := inttostr(User_data_set_info.data_set_first_year_possible);
   end;
end;

procedure LastYearMaskEditChange(Sender: TObject);
begin
   try
      User_data_set_info.data_set_last_year_possible := strtoint(UserDataSetSubForm3.LastYearMaskEdit.text);
   except
      ShowMessage ('Last year value not acceptable (out of range or non-numeric).  Resetting to 1800;  please reenter.');
      User_data_set_info.data_set_last_year_possible := 1800;
      UserDataSetSubForm3.LastYearMaskEdit.text := inttostr(User_data_set_info.data_set_last_year_possible);
   end;
end;

procedure NumCasesMaskEditChange(Sender: TObject);
begin
   try
      User_data_set_info.num_cases := strtoint(UserDataSetSubForm3.NumCasesMaskEdit.text);
   except
      ShowMessage ('Number of cases value not acceptable (out of range or non-numeric).  Resetting to 1;  please reenter.');
      User_data_set_info.num_cases := 1;
      UserDataSetSubForm3.NumCasesMaskEdit.text := inttostr(User_data_set_info.num_cases);
   end;
end;

procedure AutoDetectYearCaseButtonClick(Sender: TObject);
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
                  UserDataPrepForm.enabled := false;
                  UserDataSetSubForm3.enabled := false;
                  reset (indatafile);
                  cases_read := 0;

                  {Skip over initial label line if one exists.}
                  if User_data_set_info.label_line then readln(indatafile);
                  {Note: do not count the label line in counting cases.}
                  {Read year value from first line}
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
                             if not((strtointdef(invar,0) >= min_year) and (strtointdef(invar,0) <= max_year)) then
                                begin
                                   ShowMessage ('Error in Data file - read year value out of range, data set line '+inttostr(cases_read)+
                                                '.  Value read is "'+invar+'".  This data file either has errors or cannot be processed by EUGene.  Drop years outside the acceptable range '+
                                                inttostr(min_year)+' to '+inttostr(max_year)+' and try again.  Program is aborting now.');
{                                   halt;  }
                                end
                             else   {ok years}
                                begin
                                   yearval := strtoint(invar);
                                   if yearval < current_low_year then current_low_year := yearval else
                                   if yearval > current_high_year then current_high_year := yearval;
                                end;
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
            UserDataPrepForm.enabled := true;
            UserDataSetSubForm3.enabled := true;
         end;       {numvars was > 0;  we have variables}
   except
      {some kind of I/O error}
      ShowMessage ('Error reading data file in autodetect year and case #s.  Please check file and run and try again.');
      closefile(indatafile);
      UserDataPrepForm.enabled := true;
      UserDataSetSubForm3.enabled := true;
   end;   {except}

   end;


{ ---------------------------------------------------------------  }

end.
