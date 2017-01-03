unit ErrBx;

{EUGene  Copyright 1997, 1998  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

 interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  Dialogs, StdCtrls, sysutils, EUtypes1;

procedure EUGeneError (amessage : string; pause_time : integer; action : error_action_type;
                    var error_log : text);  {this uses the variable/dialog "errordialog".}

type
  TErrorDialog = class(Tform)
    Label1: TLabel;
    IgnoreButton: TBitBtn;
    HelpButton: TBitBtn;
    AbortButton: TBitBtn;
    procedure HaveError (amessage : string);
    procedure AbortButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure IgnoreButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

   uses TraceUnit;
{$R *.DFM}

procedure TErrorDialog.HaveError (amessage : string);
      begin
         label1.caption := amessage;
         self.hide;
         self.showmodal;
         self.BringToFront;
      end;

procedure TErrorDialog.AbortButtonClick(Sender: TObject);
   begin
      halt;    {have to do this halt.  Can't back out of this due to unit stuff.}
      try
         raise EUserInterrupt.create ('Aborting Due to Error.  ');
      except
         self.hide;
         raise;
      end;
   end;

procedure TErrorDialog.HelpButtonClick(Sender: TObject);
   begin
      ShowMessage ('The program has encountered an error.  Press "Ignore" to continue execution.'+
                   'Press "Abort" to stop this run of EUGene.');
   end;

procedure TErrorDialog.IgnoreButtonClick(Sender: TObject);
   begin
      self.hide;
   end;


procedure EUGeneError (amessage : string; pause_time : integer; action : error_action_type;
                 var error_log : text);
   {processes errors, printing messages to screen and error file.
    Also, pauses for pause_time seconds.}
   var
       datetimestring : string;
       ErrMsg : string;
       ErrorDialog: TErrorDialog;

   begin
      try
         ErrorDialog := TErrorDialog.Create(application);
         {first, write the error string/message to the screen, and to error file.}
         if fileexists(configuration.error_file_name) then
            begin
               datetimestring := formatdatetime ('h:m am/pm, mmmm d, yyyy', date+time);
               writeln (error_log, 'Error at '+ datetimestring,'  ');
               writeln (error_log, amessage);
               writeln (error_log, '-------------------------------------------------');
               trace.message ('Error at '+datetimestring);
               trace.message (amessage);

               ErrMsg := '';
               ErrMsg := ErrMsg +'Error at '+datetimestring+':  '+amessage+'.';

               case action of
                  stop : begin
                        ErrMsg := ErrMsg + '  Critical Error - halting processor and program run.';
                        ErrorDialog.IgnoreButton.enabled := false;   {they must abort}
                        trace.message ('Critical Error - halting processor and program run.');
                     end;
                  continue : begin
                        ErrMsg := ErrMsg + '  Non-critical Error - program can continue.';
                        ErrorDialog.IgnoreButton.enabled := true;   {they can continue}
                        trace.message ('Non-critical Error - continuing program.');
                     end;
                  else  trace.message ('Error in "error" procedure - invalid action type passed.');
                end;   {case}
               if pause_time > 0 then
                  begin
                     ErrorDialog.HaveError(ErrMsg);
                  end;
            end;
      finally
         ErrorDialog.Free;
      end;   {finally}
   end;


    {  ------------------------------------------------------------------   }

end.
