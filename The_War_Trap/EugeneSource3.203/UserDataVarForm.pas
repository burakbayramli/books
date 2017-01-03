unit UserDataVarForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Mask, Buttons, EUTypes1;

type

  one_var_data_type_ptr = ^one_var_data_type;

  TVariableInformationForm = class(TForm)
    VarPanel: TPanel;
    VariableNameLabel: TLabel;
    VarDataTypeLabel: TLabel;
    VariableUnitLabel: TLabel;
    ComplementaryVarLabel: TLabel;
    MissingValueLable: TLabel;
    NameMask: TMaskEdit;
    DataTypeCombo: TComboBox;
    VarUnitCombo: TComboBox;
    CompNameEdit: TMaskEdit;
    VarOKBtn: TBitBtn;
    VarCancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    MissingEdit: TEdit;
    procedure HelpBtnClick(Sender: TObject);
    procedure VarCancelBtnClick(Sender: TObject);
    procedure VarOKBtnClick(Sender: TObject);
  private
    { Private declarations }
    StoredOutputPtr : one_var_data_type_ptr;
    datasetunit : dataset_unit_of_analysis_type;
  public
    { Public declarations }
    constructor createWithInfo  (input : one_var_data_type; overalldatasettype : dataset_unit_of_analysis_type; outputptr : one_var_data_type_ptr; Owner : TComponent);
  end;

var
  VariableInformationForm: TVariableInformationForm;

implementation
  {uses UserDataConfigureForm;}
{$R *.DFM}

constructor TVariableInformationForm.createWithInfo (input : one_var_data_type; overalldatasettype : dataset_unit_of_analysis_type; outputptr : one_var_data_type_ptr; Owner : TComponent);
begin
   inherited create (owner);
   StoredOutputPtr := outputptr;
   datasetunit := overalldatasettype;
   NameMask.text := input.var_name;
   DataTypeCombo.items[0] := 'Integer';
   DataTypeCombo.items[1] := 'Real';
   DataTypeCombo.items[2] := 'String';
   case input.var_type of
         varinteger : DataTypeCombo.itemindex := 0;
         varsingle : DataTypeCombo.itemindex := 1;
         varolestr : DataTypeCombo.itemindex := 2
         else ShowMessage ('Program error - var type undefined in input setting.');
      end;  {case;}
   VarUnitCombo.items.clear;
   VarUnitCombo.dropdowncount := 9;
   VarUnitCombo.items.add ('Specify Unit...');                 {0}
   VarUnitCombo.items.add ('Version Identifier');               {1}
   VarUnitCombo.items.add ('Year Identifier');                  {2}
   VarUnitCombo.items.add ('Country Code 1 Identifier');         {3}
   VarUnitCombo.items.add ('Country Code 2 Identifer');             {4}
   VarUnitCombo.items.add ('Annual Data');                             {5}
   VarUnitCombo.items.add ('Monadic (Country Data)');                     {6}
   VarUnitCombo.items.add ('Dyadic, and ordered');                           {7}
   VarUnitCombo.items.add ('Dyadic, but unordered');                            {8}
{Note may want to change ccd1, ccode2 id for country-year and annual data sets.}
   case input.var_unit of
         no_unit_variable : VarUnitCombo.itemindex := 0;
         identifierversion : VarUnitCombo.itemindex := 1;
         identifieryear : VarUnitCombo.itemindex := 2;
         identifierccode1 : VarUnitCombo.itemindex := 3;
         identifierccode2 : VarUnitCombo.itemindex := 4;
         annual : VarUnitCombo.itemindex := 5;
         monadic : VarUnitCombo.itemindex := 6;
         dyadic_ordered : VarUnitCombo.itemindex := 7;
         dyadic_unordered : VarUnitCombo.itemindex := 8
         else ShowMessage ('Program error - var_unit undefined in input setting.');
      end;   {case}

   CompNameEdit.text := input.var_reversed_var;
   if input.var_missing_value = low(longint) then
      MissingEdit.text := '.'
   else
      MissingEdit.text := inttostr(input.var_missing_value);
end;

procedure TVariableInformationForm.HelpBtnClick(Sender: TObject);
begin
   Showmessage ('Specify details of this variable.  See documentation for more details on field values.');
end;

procedure TVariableInformationForm.VarCancelBtnClick(Sender: TObject);
begin
   close;
end;

procedure TVariableInformationForm.VarOKBtnClick(Sender: TObject);
   var all_ok : boolean;
begin
   all_ok := true;
   if VarUnitCombo.itemindex = 0 then
      begin
         ShowMessage ('You must select a variable unit (e.g. annual, directed-dyad, etc.).');
         all_ok := false;
      end;
   {Don't need to check data type, it will default to integer, and there is no bad value}
   {if DataTypeCombo.itemindex = 0 then
      begin
         ShowMessage ('You must select a data type for this variable (integer, string, real).');
         all_ok := false;
      end;   }
      {make them specify complementary var if monadic, ordered dyad in nondir data set}
   if ((VarUnitCombo.itemindex = 6) or (VarUnitCombo.itemindex = 7))
       and ((CompNameEdit.text = '') or (CompNameEdit.text = 'none'))
       and (datasetunit = nondirected_dyad_year) then
      begin
         ShowMessage ('You have specified a variable unit that requires a complementary variable name.  Please enter such a variable name.');
         all_ok := false;
      end;
   if (CompNameEdit.text = '') then CompNameEdit.text := 'none';

      {Don't allow invalid variable types given data set unit}
   if (datasetunit = annual_data) and (VarUnitCombo.itemindex>=6) then
      begin
         ShowMessage ('Annual data sets do not allow monadic or dyadic data for variables.  Please check variable unit.');
         all_ok := false;
      end;
   if (datasetunit = annual_data) and ((VarUnitCombo.itemindex=3) or (VarUnitCombo.itemindex=4)) then
      begin
         ShowMessage ('Annual data sets do not allow country code identifiers.  Please check variable unit.');
         all_ok := false;
      end;
   if (datasetunit = country_year) and (VarUnitCombo.itemindex>=7) then
      begin
         ShowMessage ('Monadic (country-year) data sets do not allow dyadic data for variables.  Please check variable unit.');
         all_ok := false;
      end;
   if (datasetunit = country_year) and (VarUnitCombo.itemindex=4) then
      begin
         ShowMessage ('Monadic (country-year) data sets do not allow a country code identifier for country 2.  Please check variable unit.');
         all_ok := false;
      end;
   if NameMask.text = '' then
      begin
         ShowMessage ('You must enter a valid variable name, 8 characters or less.  The first character must be alphabetic (a-z or A-Z).');
         all_ok := false;
      end;

   if ((MissingEdit.text = '.') or (strtointdef(MissingEdit.text, low(longint)) <> low(longint))) then
      begin
             {it's OK missing value}
      end
      else
      begin
         ShowMessage ('You must enter an integer missing value or a period(.).  Defaulting to ".".');
         MissingEdit.text := '.';
         all_ok := false;
      end;

   if all_ok then
      begin
         StoredOutputPtr^.var_name := NameMask.text;
         case DataTypeCombo.itemindex of
               0 : StoredOutputPtr^.var_type := varinteger;
               1 : StoredOutputPtr^.var_type := varsingle;
               2 : StoredOutputPtr^.var_type := varolestr;
            end;  {case;}
         case VarUnitCombo.itemindex of
               0 : StoredOutputPtr^.var_unit := no_unit_variable;
               1 : StoredOutputPtr^.var_unit := identifierversion;
               2 : StoredOutputPtr^.var_unit := identifieryear;
               3 : StoredOutputPtr^.var_unit := identifierccode1;
               4 : StoredOutputPtr^.var_unit := identifierccode2;
               5 : StoredOutputPtr^.var_unit := annual;
               6 : StoredOutputPtr^.var_unit := monadic;
               7 : StoredOutputPtr^.var_unit := dyadic_ordered;
               8 : StoredOutputPtr^.var_unit := dyadic_unordered;
            end;   {case}
         StoredOutputPtr^.var_reversed_var := CompNameEdit.text;
         if MissingEdit.text = '.' then
            StoredOutputPtr^.var_missing_value := low(longint)
         else StoredOutputPtr^.var_missing_value := strtointdef(MissingEdit.text, low(longint));

         modalresult := mrok;
      end
   else
      modalresult := mrnone;
end;

end.



