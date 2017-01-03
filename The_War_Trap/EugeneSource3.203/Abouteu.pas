unit Abouteu;

{EUGene  Copyright 1997-2002  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

uses cmnprocd, EUtypes1, WinTypes, WinProcs, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    OKButton: TBitBtn;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    Comments: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    VersionLabel: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    procedure OKButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.DFM}

uses ShellApi;

procedure TAboutBox.OKButtonClick(Sender: TObject);
begin
   close;
end;

procedure TAboutBox.FormShow(Sender: TObject);
begin
   VersionLabel.caption := ('Version ' + realtostring(Eugene_version, 5, 3));
   if private_version = true then versionlabel.caption := versionlabel.caption + ' (custom build)';
end;


end.


