unit Randy;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, EUTypes1;

type
  TRandysWindow = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Button1: TButton;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    Button3: TButton;
    Button4: TButton;
    VarSeparatorBox: TGroupBox;
    SeparateWithTabs: TRadioButton;
    SeparateWithSpaces: TRadioButton;
    SeparateWithCommas: TRadioButton;
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure SeparateWithTabsClick(Sender: TObject);
    procedure SeparateWithSpacesClick(Sender: TObject);
    procedure SeparateWithCommasClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RandysWindow: TRandysWindow;
  s_output_text_file_name, s_input_text_file_name : TFileName;
  randyseparator : separatortype;

implementation

{$R *.DFM}

uses euprocs1;

procedure TRandysWindow.Button2Click(Sender: TObject);
begin
   close;
end;

procedure TRandysWindow.Button4Click(Sender: TObject);
begin
    OpenDialog1.Title := 'Select file to read for countries/years to create s scores for';
    if OpenDialog1.Execute then
       begin
          Edit2.text := OpenDialog1.filename;
          s_input_text_file_name := OpenDialog1.filename;
       end;
end;

procedure TRandysWindow.Button3Click(Sender: TObject);
begin
      SaveDialog1.Filter :='Output files (*.out)|*.out|Text files (*.txt)|*.txt|Comma-separated files (*.csv)|*.csv|All files (*.*)|*.*';
      SaveDialog1.Title := 'Specify/Select File for Output';
      SaveDialog1.Options := [ofOverwritePrompt, ofHideReadOnly];

      if SaveDialog1.execute then
            begin
               Edit1.text := SaveDialog1.FileName;
               s_output_text_file_name := SaveDialog1.filename;
            end;
end;

procedure TRandysWindow.SeparateWithTabsClick(Sender: TObject);
begin
   randyseparator := tab;
end;

procedure TRandysWindow.SeparateWithSpacesClick(Sender: TObject);
begin
   randyseparator := space;
end;

procedure TRandysWindow.SeparateWithCommasClick(Sender: TObject);
begin
   randyseparator := comma;
end;

procedure TRandysWindow.Button1Click(Sender: TObject);
   var first_proc_year, last_proc_year : year_range;
begin
   first_proc_year := 1816;
   last_proc_year := 2002;

   compute_and_output_s_from_arbitrary_countries_and_years (configuration.cow_alliance_file_name,
      configuration.alliance_seq_file_name, configuration.dyadic_alliance_file_name,
      configuration.cow_system_pct_file_name,
      nation_list, first_proc_year,
      last_proc_year, flat_dyadic, randyseparator, s_output_text_file_name, s_input_text_file_name);

end;

procedure TRandysWindow.FormCreate(Sender: TObject);
begin
   randyseparator := comma;
end;

end.
