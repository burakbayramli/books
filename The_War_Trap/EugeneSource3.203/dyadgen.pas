unit dyadgen;

{Unit to assist in generation of dyadic MID files.}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, eutypes1, eutypesmid, StdCtrls;

type
  TDyadicGen = class(TForm)
    InputTitleLabel: TLabel;
    File1Label: TLabel;
    File2Label: TLabel;
    File3Label: TLabel;
    File4Label: TLabel;
    FileDestination: TEdit;
    OutputTitleLabel: TLabel;
    ChangeFileButtonClick: TButton;
    GenerateButton: TButton;
    CancelButton: TButton;
    MID_File_A_Header: TLabel;
    MID_File_B_Header: TLabel;
    MID_File_C_Header: TLabel;
    Incident_header: TLabel;
    ChangeMidAButton: TButton;
    ChangeMidBButton: TButton;
    ChangeMidCButton: TButton;
    ChangeIncidentFileButton: TButton;
    DyadicSaveDialog: TSaveDialog;
    DyadicOpenDialog1: TOpenDialog;
    procedure SetLabels(Sender: TObject);

    procedure ChangeFileButtonClickClick(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);

    procedure ChangeMidAButtonClick(Sender: TObject);
    procedure ChangeMidBButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ChangeIncidentFileButtonClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

uses mdiframe, Setting;

{$R *.dfm}

procedure TDyadicGen.ChangeFileButtonClickClick(Sender: TObject);
var fileok : boolean;
begin
      DyadicSaveDialog.Filter := 'Data Files (*.CSV)|*.CSV|All files (*.*)|*.*';
      DyadicSaveDialog.FileName := FileDestination.Text;
      DyadicSaveDialog.Options := [ofOverwritePrompt, ofHideReadOnly];
      fileok := false;
      user_selections.output_format.overwrite := false;
      if DyadicSaveDialog.execute then
         begin
            if (extractFileExt(DyadicSaveDialog.FileName) = '.') or (extractFileExt(DyadicSaveDialog.FileName) = '') then
               begin    {add .edf}
                  DyadicSaveDialog.FileName := changeFileExt(DyadicSaveDialog.FileName, '.csv');
                  showmessage ('Your output data file must have a 3 character extension (like ".csv"). EUGene has renamed your output file to '+DyadicSaveDialog.FileName);
               end;
            user_selections.output_format.overwrite := true;
            fileok:= true;
            user_selections.output_format.output_file_name := DyadicSaveDialog.FileName;
            FileDestination.text:=DyadicSaveDialog.FileName;
            user_selections.output_format.location := tofile;
            SettingsForm.outputlb.caption:='To File';
            user_selections.output_format.output_set := true;
         end
      else {exited with cancel}
         begin
            user_selections.output_format.overwrite := false;
            user_selections.output_format.output_set := false;
            user_selections.output_format.location := tonone;
            fileOK := false;
         end;

end;

procedure TDyadicGen.GenerateButtonClick(Sender: TObject);
var
  DyadicMID30 : TDyadic_dispute_data_obj_integrated_format30;
  outfilename : TFileName;
begin
    if not (configuration.cow_mid_data_format=disputeformat30) then
       showmessage ('Configuration file not updated to format30 data; this procedure can not be run')
    else
       begin
          try
             self.hide;
             DyadicMID30 := TDyadic_dispute_data_obj_integrated_format30.init(user_selections, configuration, 1992, 2001, none_pre_1992);

             outfilename := FileDestination.text;
             DyadicMID30.output_to_file (outfilename);

             ShowMessage ('Dyadic MID output complete to file '+outfilename);
          finally
             DyadicMID30.free;
          end;
       end;
    Close;
end;

procedure TDyadicGen.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TDyadicGen.ChangeMidAButtonClick(Sender: TObject);
begin
      DyadicOpenDialog1.Filter := 'Data Files (*.CSV)|*.CSV|All files (*.*)|*.*';
      DyadicOpenDialog1.FileName := configuration.cow_mid_case_file_nameA;
      if DyadicOpenDialog1.execute then
         begin
            configuration.cow_mid_case_file_nameA := DyadicOpenDialog1.FileName;
            File1Label.Caption :=DyadicOpenDialog1.FileName;
         end;
end;

procedure TDyadicGen.ChangeMidBButtonClick(Sender: TObject);
begin
      DyadicOpenDialog1.Filter := 'Data Files (*.CSV)|*.CSV|All files (*.*)|*.*';
      DyadicOpenDialog1.FileName := configuration.cow_mid_actor_file_nameB;
      if DyadicOpenDialog1.execute then
         begin
            configuration.cow_mid_actor_file_nameB := DyadicOpenDialog1.FileName;
            File2Label.Caption :=DyadicOpenDialog1.FileName;
         end;
end;

procedure TDyadicGen.Button4Click(Sender: TObject);
begin
      DyadicOpenDialog1.Filter := 'Data Files (*.CSV)|*.CSV|All files (*.*)|*.*';
      DyadicOpenDialog1.FileName := configuration.cow_mid_name_file_nameC;
      if DyadicOpenDialog1.execute then
         begin
            configuration.cow_mid_name_file_nameC := DyadicOpenDialog1.FileName;
            File3Label.Caption :=DyadicOpenDialog1.FileName;
         end;
end;

procedure TDyadicGen.ChangeIncidentFileButtonClick(Sender: TObject);
begin
      DyadicOpenDialog1.Filter := 'Data Files (*.CSV)|*.CSV|All files (*.*)|*.*';
      DyadicOpenDialog1.FileName := configuration.cow_mid_participant_incident_file_name;
      if DyadicOpenDialog1.execute then
         begin
            configuration.cow_mid_participant_incident_file_name := DyadicOpenDialog1.FileName;
            File4Label.Caption :=DyadicOpenDialog1.FileName;
         end;
end;

procedure TDyadicGen.SetLabels(Sender: TObject);
begin
    File1Label.Caption := configuration.cow_mid_case_file_nameA;
    File2Label.Caption := configuration.cow_mid_actor_file_nameB;
    File3Label.Caption := configuration.cow_mid_name_file_nameC;
    File4Label.Caption := configuration.cow_mid_participant_incident_file_name;
    FileDestination.Text := configuration.eugene_directory + 'MIDDyadic_v3.10.csv';
end;

end.
