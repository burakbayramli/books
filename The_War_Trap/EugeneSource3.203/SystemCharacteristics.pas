unit SystemCharacteristics;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, eutypes1, Buttons;

type
  TSystemCharacteristicsForm = class(TForm)
    SystemVariables: TGroupBox;
    SystemVarsOKButton: TBitBtn;
    StatesInSystemBox: TCheckBox;
    GPsInSystemBox: TCheckBox;
    SysConBox: TCheckBox;
    SysMoveBox: TCheckBox;
    SysMove5Box: TCheckBox;
    SysMoveGPBox: TCheckBox;
    SysMoveGP5Box: TCheckBox;
    procedure SystemVarsOKButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  SystemCharacteristicsForm: TSystemCharacteristicsForm;

implementation

uses PagedOutput;

{$R *.dfm}


procedure TSystemCharacteristicsForm.SystemVarsOKButtonClick(Sender: TObject);

begin
   if StatesInSystemBox.checked
      then include (user_selections.output_format.variables, StatesInSystem)
      else exclude (user_selections.output_format.variables, StatesInSystem);
   if GPsInSystemBox.checked
      then include (user_selections.output_format.variables, GPsInSystem)
      else exclude (user_selections.output_format.variables, GPsInSystem);
   if SysConBox.checked
      then include (user_selections.output_format.variables, SysConcentration)
      else exclude (user_selections.output_format.variables, SysConcentration);
   if SysMoveBox.checked
      then include (user_selections.output_format.variables, SysMovement)
      else exclude (user_selections.output_format.variables, SysMovement);
   if SysMove5Box.checked
      then include (user_selections.output_format.variables, SysMovement5Yr)
      else exclude (user_selections.output_format.variables, SysMovement5Yr);
   if SysMoveGPBox.checked
      then include (user_selections.output_format.variables, SysMoveGP)
      else exclude (user_selections.output_format.variables, SysMoveGP);
   if SysMoveGP5Box.checked
      then include (user_selections.output_format.variables, SysMoveGP5Yr)
      else exclude (user_selections.output_format.variables, SysMoveGP5Yr);

      {Now, if they selected any system vars, mark it on the main variable sheet.}
   if (StatesInSystem in user_selections.output_format.variables)
        or (GPsInSystem in user_selections.output_format.variables)
        or (SysConcentration in user_selections.output_format.variables)
        or (SysMovement in user_selections.output_format.variables)
        or (SysMovement5Yr in user_selections.output_format.variables)
        or (SysMoveGP in user_selections.output_format.variables)
        or (SysMoveGP5Yr in user_selections.output_format.variables) then
      begin
         Output_Options.SystemVarsBox.checked := true;
      end
      else    {there are no system variables}
      begin
         Output_Options.SystemVarsBox.checked := false;
      end;
end;

procedure TSystemCharacteristicsForm.FormShow(Sender: TObject);
begin
   StatesInSystemBox.checked := StatesInSystem in user_selections.output_format.variables;
   GPsInSystemBox.checked    := GPsInSystem in user_selections.output_format.variables;
   SysConBox.checked         := SysConcentration in user_selections.output_format.variables;
   SysMoveBox.checked        := SysMovement in user_selections.output_format.variables;
   SysMove5Box.checked       := SysMovement5Yr in user_selections.output_format.variables;
   SysMoveGPBox.checked      := SysMoveGP in user_selections.output_format.variables;
   SysMoveGP5Box.checked     := SysMoveGP5Yr in user_selections.output_format.variables;

end;

end.
