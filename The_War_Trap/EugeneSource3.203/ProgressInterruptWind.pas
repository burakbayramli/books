unit ProgressInterruptWind;

{EUGene  Copyright 1997-2000  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ComCtrls, abortcheck, ExtCtrls;

type
   TProgForm = class(TForm)
    StopExecution: TBitBtn;
    PauseButton: TBitBtn;
    ProgFormPanel: TPanel;
    ProgFormLabel: TLabel;
    ProgressBar: TProgressBar;
    TimeRemainingLabel: TLabel;
    PctLeftLabel: TLabel;
    procedure BarStart (title : string; timeleft : string; iterations_needed : longint);
    procedure BarInc (title : string; timeleft, pctdone : string);
    procedure StopExecutionClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pausebuttonClick(Sender: TObject);
   private
   end;


implementation

uses mdiframe, EUTypes1;

{$R *.DFM}

procedure TProgForm.BarStart (title : string; timeleft : string; iterations_needed : longint);
  begin
     self.caption := 'Program Status';
     ProgFormLabel.caption := title;
     if (ProgFormLabel.width+ProgFormLabel.left) > self.Width then self.width :=
                             ProgFormLabel.left + ProgFormLabel.width + 80
        else self.width := ProgressBar.width + 80;
     self.height := ProgFormPanel.height + StopExecution.height + 80;
     TimeRemainingLabel.caption := 'Estimated Time Remaining: '+ TimeLeft;
     ProgressBar.position := 0;
     ProgressBar.min := 0;
     if iterations_needed = 0 then    {if called with an unknown # needed, then set up to inc to 1000}
        ProgressBar.max := 5000
     else
        begin
           ProgressBar.max := iterations_needed;
           ProgressBar.step :=  1;
           self.PctLeftLabel.caption := '% Complete: '+inttostr(progressBar.position);
        end;
  end;
{----------------------------------------------------------}

procedure TProgForm.BarInc (title : string; timeleft, pctdone : string);
  begin
     ProgFormLabel.caption := title;
     TimeRemainingLabel.caption := 'Estimated Time Remaining: '+ TimeLeft;
     ProgressBar.StepIt;
     {self.PctLeftLabel.caption := '% Complete: '+inttostr(round(progressBar.position*100/progressbar.max));}
     self.PctLeftLabel.caption := '% Complete: '+pctdone;
  end;

{------------------------------------------------------------}

procedure TProgForm.StopExecutionClick(Sender: TObject);
begin
    try
       VerifyStopForm.ShowModal;
    except

    end;
end;

{--------------------------------------------------------}

procedure TProgForm.FormShow(Sender: TObject);
   var offset : integer;
begin
     {The deleted code would offset the progress form so it remains
      centered under frame}
     {offset := round(0.5*((frame.width) - (self.width)));
     self.setbounds(Frame.left+offset,(frame.top+frame.height),self.width,self.height);}
   stopExecution.enabled := true;
end;


procedure TProgForm.pausebuttonClick(Sender: TObject);
begin
   showmessage ('Pausing.  Push "OK" to resume execution');
end;

end.
