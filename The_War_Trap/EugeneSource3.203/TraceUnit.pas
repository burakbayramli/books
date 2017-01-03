unit TraceUnit;

{EUGene  Copyright 1997, 1998  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ProgressInterruptWind;

type    {trace window is a basic window that displays program trace.}
  TTraceWindow = class(TForm)
    OutputMem: TMemo;
    procedure OutputSetSize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


     {The trace object is for tracing entry/exit into procedures/loops}
   Ttrace_obj = class (Tobject)
      constructor init (start_trace_level : integer);
      destructor destroy; override;
      function get_trace_level : integer;
      procedure enter (amessage : string);
      procedure exit (amessage : string);
      procedure tick (title : string; iterations_needed : longint);
      procedure tickdone;
      procedure SetGlobalTrace;   {there are special procedures for the global, initial trace window}
      procedure hideprogress;
      procedure showprogress;
      procedure message (amessage : string);
      private
         trace_level : integer;
         ticking : boolean;
         iterations_done : longint;
         starttime : tdatetime;
         ProgForm : Tprogform;
      end;    {object}

var
  TraceWindow: TTraceWindow;
  trace : Ttrace_obj;   {this is for tracing levels deep in the program.}

implementation
  uses eutypes1, IButils;   {Need to get to the progform as I go in/out of procs}

{$R *.DFM}

procedure TTraceWindow.OutputSetSize(Sender: TObject);
begin
     self.width := round(4 * screen.pixelsPerInch);
     self.height := round (5 * screen.pixelsPerInch);
     self.left := screen.width - self.width;
     self.top := screen.height - self.height;
     self.outputmem.height := self.clientheight;
     self.outputmem.width := self.clientwidth;
     outputmem.left := 0;
     outputmem.top := 0;
end;


      {methods for trace object, that will allow easy tracing in and out of procedures/fns.}

   constructor Ttrace_obj.init (start_trace_level : integer);
      var offsetwidth, offsetheight : integer;
      begin
         trace_level := start_trace_level;
         ticking := false;
         iterations_done := 0;
            {by default setting in form, this is invisible to start}
            {At level 0, main program level, don't want a progress bar}
         ProgForm := TProgForm.create (application.mainform);
         offsetwidth := round(0.5*((application.mainform.width) - (Progform.width))) + 2*trace_level;
         offsetheight := round(0.5*((application.mainform.height) - (Progform.height))) + 2*trace_level;
         if trace_level > 20 then
            begin
               trace.message ('Trace level > 20 - check programming trace.');
               offsetwidth := round(0.5*((application.mainform.width) - (Progform.width))) + 40;
               offsetheight := round(0.5*((application.mainform.height) - (Progform.height))) + 40;
            end;
         {Height and width set within .barstart}   
         ProgForm.setbounds (application.mainform.left+offsetwidth,
                             application.mainform.top+offsetheight,
                             Progform.width,Progform.height);
         ProgForm.caption := 'Initializing trace...';

         if isiconic (application.handle) then
            begin
               {ProgForm.visible := false;}
               {ProgForm.WindowState := wsMinimized;}
            end
         else
            begin
               {ProgForm.visible := true; }
               ProgForm.WindowState := wsNormal;
            end;

         {
            ProgForm.WindowState := wsMinimized;
            case application.mainform.WindowState of
            wsNormal : showmessage ('Normal');
            wsminimized : showmessage ('minimized');
            else  showmessage ('other');
         end;}

         {if application.mainform.WindowState=wsNormal then
            ProgForm.WindowState := wsNormal; }
      end;

   destructor Ttrace_obj.destroy;
      begin
         try
             {don't have to do anything}
            if self <> nil then
               begin
                  if progform <> nil then
                     ProgForm.free;
 //                 inherited destroy;
               end;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;

   function Ttrace_obj.get_trace_level : integer;
      begin
         get_trace_level := trace_level;
      end;

   procedure Ttrace_obj.enter (amessage : string);
             {called when a procedure is entered}
         var x : integer;
             mess : string;
         begin
            try
               inc(trace_level);
               if debug[min(trace_level, max_debug_level)] then
                  begin
                     mess := '';
                     mess := mess + inttostr(trace_level)+': ';
                     if trace_level >= max_debug_level then mess := mess + '+ ';
                     for x := 1 to (trace_level-1) do mess := mess + '  ';
                     mess := mess + amessage;
                     TraceWindow.OutputMem.lines.add (mess);
                  end;
               {Application.ProcessMessages;
               if UserInterrupt then
                  begin
                     raise EUserInterrupt.create ('User Interrupt');
                  end;  }
            except

            end;  {except}
         end;

   procedure Ttrace_obj.exit (amessage : string);
         var x : integer;
             mess : string;
         begin
            try
               if debug[min(trace_level, max_debug_level)] then
                  begin
                     mess := '';
                     mess := mess + inttostr(trace_level)+': ';
                     if trace_level >= max_debug_level then mess := mess + '+ ';
                     for x := 1 to (trace_level-1) do mess := mess + '  ';
                     mess := mess + amessage;
                     TraceWindow.OutputMem.lines.add (mess);
                  end;
               dec(trace_level);
               mess := inttostr(trace_level);
               TraceWindow.OutputMem.lines.add (mess);
               Application.ProcessMessages;
               {ProgForm.hide;}
               if UserInterrupt then
                  begin
                     raise EUserInterrupt.create ('User Interrupt');
                  end;

            except
                on EUserInterrupt do
                   begin
                      {Must pass this exception back out}
                      raise;
                   end;
            end;  {except}

         end;

   procedure Ttrace_obj.tick (title : string; iterations_needed : longint);
      {this creates just a tick mark at regular intervals. }
      var TimeLeft, TimePerIteration : Tdatetime;
      begin
         try
            if not ticking then
               begin
                  if (application.mainform.WindowState <> wsMinimized) then ProgForm.show;
                  ProgForm.barstart (title, 'unknown', iterations_needed);
                  {showprogress;}
                  starttime := time;
                  ticking := true;
                  iterations_done := 0;
               end
            else
               begin
                    inc(iterations_done);
                    if iterations_needed = 0 then {in this case, unknown params for scope of search, so have no time}
                       ProgForm.BarInc (title, 'unknown','?')
                    else
                       begin
                          TimePerIteration := (time-starttime) / iterations_done;
                          TimeLeft := TimePerIteration * (iterations_needed - iterations_done);
                          if TimeLeft < 0 then
                             begin
                                 TimeLeft := 0;
                                 {showMessage ('Inform programmer to check TimeLeft Trace procedure');}
                             end;
                          ProgForm.BarInc (title, FormatDateTime('hh:mm:ss', TimeLeft), inttostr(round(100*(iterations_done/iterations_needed))));
                       end;
               end;
            Application.ProcessMessages;
            if UserInterrupt then
               begin
                  raise EUserInterrupt.create ('User Interrupt');
               end;

         except
             on EUserInterrupt do
                begin
                   {showmessage ('User interrupt raised in trace.tick');}
                   {Must pass this exception back out}
                   raise;
                end;
         end;  {except}

      end;

   procedure Ttrace_obj.tickdone;
      begin
            ticking := false;
            {If you put in this hideprogress, for some reason it screws up the later
             hideprogress.  May be a scope issue - this hide may actually put it to the
             back within the trace object, which may still be on front of other objects.}
            {hideprogress;}
      end;

   procedure Ttrace_obj.SetGlobalTrace;
      begin
            progform.caption := 'Application Trace';
            progform.pausebutton.enabled := false;
            progform.enabled := false;
            progform.sendtoback;
            {Cannot hide the progress form, because it is an mdi child. So can't do
                 progform.hide;
            But can disable it, and move it to back}
      end;

   procedure Ttrace_obj.hideprogress;
      begin
            {progform.sendtoback;}
      end;
   procedure Ttrace_obj.showprogress;
      begin
            {progform.bringtofront;}
      end;

   procedure Ttrace_obj.message (amessage : string);
      {just like the enter/exit proc, but don't permanently increment the level counter.}
      var x : integer;
          mess : string;
      begin
         try
            if debug[trace_level] then
               begin
                  mess := '';
                  mess := mess + inttostr(trace_level)+': ';
                  for x := 1 to trace_level do mess := mess + ('  ');
                  mess := mess + amessage;
                  if TraceWindow.OutputMem.lines.count >= 1000 then
                     begin
                        for x := 100 downto 0 do
                           TraceWindow.OutputMem.lines.delete(x);
                     end;
                  TraceWindow.OutputMem.lines.add (mess);
               end;
            Application.ProcessMessages;
            if UserInterrupt then
               begin
                  raise EUserInterrupt.create ('User Interrupt');
               end;

            except
               on EUserInterrupt do
                   begin
                      {Must pass this exception back out}
                      raise;
                   end;
            end;  {except}

      end;

end.
