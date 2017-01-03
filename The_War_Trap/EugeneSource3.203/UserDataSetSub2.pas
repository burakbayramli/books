unit UserDataSetSub2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Mask, Grids, EUTypes1, ExtCtrls;

type
  TUserDataSetSubForm2 = class(TForm)
    DoneBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    UserSub2Panel: TPanel;
    GroupBox1: TGroupBox;
    UnitLabel: TLabel;
    NumVarsLabel: TLabel;
    LabelLineCheckBox: TCheckBox;
    UnitComboBox: TComboBox;
    NumVarsMaskEdit: TMaskEdit;
    AutoDetectNameButton: TButton;
    VarGroupBox: TGroupBox;
    VarInfoStringGrid: TStringGrid;
    MoveDownVariableButton: TButton;
    MoveUpVariableButton: TButton;
    DeleteVarButton: TButton;
    EditVarButton: TButton;
    AddVariableButton: TButton;
    procedure UnitComboBoxChange(Sender: TObject);
    procedure LabelLineCheckBoxClick(Sender: TObject);
    procedure AutoDetectNameButtonClick(Sender: TObject);
    procedure MoveUpVariableButtonClick(Sender: TObject);
    procedure MoveDownVariableButtonClick(Sender: TObject);
    procedure AddVariableButtonClick(Sender: TObject);
    procedure EditVarButtonClick(Sender: TObject);
    procedure DeleteVarButtonClick(Sender: TObject);
    procedure DoneBtnClick(Sender: TObject);
    procedure VarInfoStringGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UserDataSetSubForm2: TUserDataSetSubForm2;

implementation

uses RereadNamesForm, UserDataPreparationForm;

{$R *.DFM}

procedure TUserDataSetSubForm2.UnitComboBoxChange(Sender: TObject);
begin
   UserDataPreparationForm.UnitComboBoxChange(sender);
end;

procedure TUserDataSetSubForm2.LabelLineCheckBoxClick(Sender: TObject);
begin
   UserDataPreparationForm.LabelLineCheckBoxClick(sender);
end;

procedure TUserDataSetSubForm2.AutoDetectNameButtonClick(Sender: TObject);
begin
   UserDataPreparationForm.AutoDetectNameButtonClick(sender);
end;

procedure TUserDataSetSubForm2.MoveUpVariableButtonClick(Sender: TObject);
begin
   UserDataPreparationForm.MoveUpVariableButtonClick(sender);
end;

procedure TUserDataSetSubForm2.MoveDownVariableButtonClick(Sender: TObject);
begin
   UserDataPreparationForm.MoveDownVariableButtonClick(sender);
end;

procedure TUserDataSetSubForm2.AddVariableButtonClick(Sender: TObject);
begin
   UserDataPreparationForm.AddVariableButtonClick(sender);
end;

procedure TUserDataSetSubForm2.EditVarButtonClick(Sender: TObject);
begin
   UserDataPreparationForm.EditVarButtonClick(sender);
end;

procedure TUserDataSetSubForm2.DeleteVarButtonClick(Sender: TObject);
begin
   UserDataPreparationForm.DeleteVarButtonClick(sender);
end;

procedure TUserDataSetSubForm2.DoneBtnClick(Sender: TObject);
   var found, all_values_ok : boolean;
       x, y, numannual, nummonadic, numnondir, numdir,
       numbadunit, numccode1id, numccode2id, numyearid, numversionid : integer;
begin
   all_values_ok := true;

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

      {Check of # of year, ccode identifiers, and unit definition consistency.}
      numccode1id := 0;
      numccode2id := 0;
      numyearid := 0;
      numversionid := 0;
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
               ((numccode1id <> 1) or (numccode2id <> 1)) then
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
                     ShowMessage ('Variable '+var_info[x].var_name+' requires a complementary variable in the data set because this is a nondirected data set, and because the variable has direction (is ordered dyadic or monadic).  Configuration file will be saved, but it is incorrect.');
                     all_values_ok := false;
                  end;
            end;
         end;

   end;           {with User_data_set_info}


end;

procedure TUserDataSetSubForm2.VarInfoStringGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
   UserDataPreparationForm.VarInfoStringGridSelectCell(sender, ACol, ARow, canSelect);
end;

procedure TUserDataSetSubForm2.HelpBtnClick(Sender: TObject);
begin
     ShowMessage ('This page allows users to define variable characteristics for data submission.  Be sure to define all characteristics and version number.  Refer to documentation for more assistance.')
end;

end.
