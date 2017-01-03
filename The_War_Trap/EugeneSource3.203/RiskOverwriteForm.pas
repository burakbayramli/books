unit RiskOverwriteForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TRiskOverwriteDeleteForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn3: TBitBtn;
    FileOverwriteWarningPanel: TPanel;
    RiskFileOverwriteCaption: TLabel;
    StaticText1: TStaticText;
    overwriteBtn: TRadioButton;
    StaticText2: TStaticText;
    BlankFileBtn: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure overwriteBtnClick(Sender: TObject);
    procedure BlankFileBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    deleteRiskFile, OverwriteRiskFile : boolean;
  end;

var
  RiskOverwriteDeleteForm: TRiskOverwriteDeleteForm;

implementation

{$R *.DFM}

procedure TRiskOverwriteDeleteForm.FormCreate(Sender: TObject);
begin
    overwriteBtn.checked := true;
end;

procedure TRiskOverwriteDeleteForm.overwriteBtnClick(Sender: TObject);
begin
    deleteRiskFile := true;
    OverwriteRiskFile := false;
end;

procedure TRiskOverwriteDeleteForm.BlankFileBtnClick(Sender: TObject);
begin
    deleteRiskFile := false;
    OverwriteRiskFile := true;
end;

end.
