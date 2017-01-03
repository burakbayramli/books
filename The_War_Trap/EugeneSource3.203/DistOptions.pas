unit DistOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, EUTypes1;

type
  TDistanceOutputOptions = class(TForm)
    OKBtn: TBitBtn;
    HlpBtn: TBitBtn;
    DistanceVarOptionsPanel: TPanel;
    DistanceVarOptionsLabel: TLabel;
    Cap_to_capbox: TRadioButton;
    Cap_contbox: TRadioButton;
    Cap_cont_wtbox: TRadioButton;
    MinDistBox: TRadioButton;
    procedure OKBtnClick(Sender: TObject);
    procedure Cap_to_capboxClick(Sender: TObject);
    procedure Cap_contboxClick(Sender: TObject);
    procedure Cap_cont_wtboxClick(Sender: TObject);
    procedure HlpBtnClick(Sender: TObject);
    procedure MinDistBoxClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

uses GenrHelp;

{$R *.DFM}

procedure TDistanceOutputOptions.OKBtnClick(Sender: TObject);
begin
   close;
end;

procedure TDistanceOutputOptions.Cap_to_capboxClick(Sender: TObject);
begin
   user_selections.distance_method := capitols;
end;

procedure TDistanceOutputOptions.Cap_contboxClick(Sender: TObject);
begin
   user_selections.distance_method := capitols_contiguity;
end;

procedure TDistanceOutputOptions.Cap_cont_wtboxClick(Sender: TObject);
begin
   user_selections.distance_method := capitols_contiguity_war_trap;
end;

procedure TDistanceOutputOptions.HlpBtnClick(Sender: TObject);
begin
     GenericHelpForm.present (configuration.HelpFiles_DistanceBox_name);
     GenericHelpForm.Showmodal;

end;

procedure TDistanceOutputOptions.MinDistBoxClick(Sender: TObject);
begin
   user_selections.distance_method := minimum;
end;

procedure TDistanceOutputOptions.FormShow(Sender: TObject);
begin
       Cap_to_capbox.checked := (user_selections.distance_method = capitols);
       Cap_contbox.checked := (user_selections.distance_method = capitols_contiguity);
       Cap_cont_wtbox.checked := (user_selections.distance_method = capitols_contiguity_war_trap);
end;

end.
