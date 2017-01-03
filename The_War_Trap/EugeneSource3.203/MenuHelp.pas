unit MenuHelp;

{EUGene  Copyright 1997, 1998  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls;

type
  Thelpunit = class(TForm)
    CloseBtn: TBitBtn;
    MainMenuHelpPanel: TPanel;
    MainMenuHelpLabel: TLabel;
    HelpMemo: TRichEdit;
    TraceHlp: TRadioButton;
    FileHlp: TRadioButton;
    ReComputeHlp: TRadioButton;
    HelpBtn: TRadioButton;
    UserDataHlp: TRadioButton;
    WindowsButton: TRadioButton;
    OutputHlp: TRadioButton;
    procedure FileHlpClick(Sender: TObject);
    procedure ReComputeHlpClick(Sender: TObject);
    procedure OutputHlpClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure WindowsButtonClick(Sender: TObject);
    procedure TraceHlpClick(Sender: TObject);
    procedure UserDataHlpClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses eutypes1;

{$R *.DFM}

{--------------------------------------------------------------------------}

procedure Thelpunit.FileHlpClick(Sender: TObject);
begin
  with HelpMemo.Lines do
  begin
     LoadFromFile(configuration.HelpFiles_FileMenu_name);
  end;
end;

{-----------------------------------------------------------------------------}

procedure Thelpunit.ReComputeHlpClick(Sender: TObject);
begin
  with HelpMemo.Lines do
  begin
     LoadFromFile(configuration.HelpFiles_RecomputeMenu_name);
  end;
end;

{---------------------------------------------------------------------------------}

procedure Thelpunit.OutputHlpClick(Sender: TObject);
begin
  with HelpMemo.Lines do
  begin
     LoadFromFile(configuration.HelpFiles_OutputMenu_name);
  end;
end;

{------------------------------------------------------------------------}

procedure Thelpunit.TraceHlpClick(Sender: TObject);
begin
  with HelpMemo.Lines do
  begin
     LoadFromFile(configuration.HelpFiles_TraceMenu_name);
  end;
end;

{------------------------------------------------------------------------}

procedure Thelpunit.HelpBtnClick(Sender: TObject);
begin
  with HelpMemo.Lines do
  begin
     LoadFromFile(configuration.HelpFiles_HelpMenu_name);
  end;
end;

procedure Thelpunit.CloseBtnClick(Sender: TObject);
begin
   close;
end;

procedure Thelpunit.WindowsButtonClick(Sender: TObject);
begin
  with HelpMemo.Lines do
  begin
     LoadFromFile(configuration.HelpFiles_OnscreenWindows_name);
  end;

end;


      {  -------------------------------------------------   }

procedure Thelpunit.UserDataHlpClick(Sender: TObject);
begin
  with HelpMemo.Lines do
  begin
     LoadFromFile(configuration.HelpFiles_UserData_name);
  end;
end;

end.
