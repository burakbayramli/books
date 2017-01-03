unit RiskCombineIntegrated;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, eutypes1, eutypes2, runningwindow, combineyearssubbox;

const
   max_risk_files = 30;

type
  risk_file_range = 0..max_risk_files;

  TCombineRiskFileForm = class(TForm)
    GroupBox1: TGroupBox;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    InputFileListBox: TListBox;
    GroupBox2: TGroupBox;
    FileDisplay: TEdit;
    CombineFilesRunBox: TGroupBox;
    CancelButton: TBitBtn;
    DateRangeListBox: TListBox;
    DoneButton: TBitBtn;
    FileAddButton: TButton;
    clearlistbutton: TButton;
    RiskSecurityGroupBox: TGroupBox;
    RiskFilesRadioButton: TRadioButton;
    SecurityFilesButton: TRadioButton;
    SubsetList: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    NumList: TListBox;
    Label4: TLabel;
    YearSubsetButton: TButton;
    FromYearList: TListBox;
    ToYearList: TListBox;
    RetrieveOldFileListButton: TButton;
    CombineButton: TButton;
    ChangeOutputButton: TButton;
    FileRemoveButton: TButton;
    procedure FileAddButtonClick(Sender: TObject);
    procedure CombineFileInitialize(Sender: TObject);
    procedure ChangeOutputButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure CombineButtonClick(Sender: TObject);
    procedure ShowFileDateButtonClick(Sender: TObject);
    procedure clearlistbuttonClick(Sender: TObject);
    procedure DoneButtonClick(Sender: TObject);
    procedure RiskFilesRadioButtonClick(Sender: TObject);
    procedure SecurityFilesButtonClick(Sender: TObject);
    procedure YearSubsetButtonClick(Sender: TObject);
    procedure RetrieveOldFileListButtonClick(Sender: TObject);
    procedure RemoveFileButtonClick(Sender: TObject);
  private
    procedure itemadd (name : string);
  public
    { Public declarations }
  end;

var
  inputfilelist : array[risk_file_range] of TFileName;
  RiskOutFileName : TFileName;
  combineriskfilenames : TFileName;
  first_year, last_year : year_range;
  Risksecurityfiles : (riskfiles, securityfiles);

implementation

uses RiskCombineRenameFile;

{$R *.DFM}

procedure TCombineRiskFileForm.itemadd (name : string);
   begin
      InputFileListBox.Items.Add(name);
      numList.items.add(inttostr(InputFileListBox.Items.count-1));
      subsetlist.items.add('n');
      DateRangeListBox.items.add('0');
      FromYearList.items.add('0');
      ToYearList.items.add('0');
   end;

procedure TCombineRiskFileForm.CombineFileInitialize(Sender: TObject);
  var x : integer;

   begin

      Risksecurityfiles := riskfiles;
      RiskFilesRadioButton.checked := true;
      DoneButton.enabled := false;
      DateRangeListBox.clear;
      InputFileListBox.clear;
      subsetlist.clear;
      numList.clear;
      fromyearlist.clear;
      toyearlist.clear;
      {Start with Blank list}
     {
         begin
            itemadd('f:\EUGene\RiskCombine\riskgen18161952.dat');
            itemadd('f:\EUGene\RiskCombine\riskgen19461976.dat');
            itemadd('f:\EUGene\RiskCombine\riskgen19771977.dat');
         end;    }

      RiskOutFileName := (configuration.eugene_directory + 'outRiskTEST.dat');
      FileDisplay.Text := RiskOutFileName;

   end;





procedure TCombineRiskFileForm.ChangeOutputButtonClick(Sender: TObject);
   var tempword : string;
   begin
      if risksecurityfiles = riskfiles then tempword := 'Risk' else tempword := 'Security/Alliance';
      SaveDialog1.title := 'Choose File for New, Combined '+tempword+' File';
      SaveDialog1.Filter := 'Data files (*.DAT)|*.DAT|All files (*.*)|*.*';
      SaveDialog1.FileName := RiskOutFileName;
      SaveDialog1.Options := [ofOverwritePrompt, ofHideReadOnly];
      if SaveDialog1.execute then
         begin
            Riskoutfilename := SaveDialog1.FileName;
            FileDisplay.text:=RiskOutFileName;
         end
      else {exited with cancel}
         begin
         end;
   end;


type risk_file_rec_ptr = ^risk_file_record_type_v2;
        sec_file_rec_ptr = ^security_alliance_file_record_type;

procedure get_file_years (afilename : TFileName; var first_year, last_year : year_range);
   var ariskfile : risk_file_type_v2;
       asecfile : security_alliance_file_type;
       {risk_file_rec_new : risk_file_record_type_v2;
       sec_file_rec_new : security_alliance_file_record_type;}
       risk_file_rec_new : risk_file_rec_ptr;
       sec_file_rec_new : sec_file_rec_ptr;

   begin
      new (risk_file_rec_new);
      new (sec_file_rec_new);
      case risksecurityfiles of
         riskfiles : begin
                assign (ariskfile, afilename);
                if fileexists (afilename) then
                   begin
                      reset(ariskfile);
                      {Get first year of new risk file}
                      read (ariskfile, risk_file_rec_new^);
                      first_year := risk_file_rec_new^.year;
                      while not eof (ariskfile) do
                         begin
                            read (ariskfile, risk_file_rec_new^);
                         end;
                      last_year := risk_file_rec_new^.year;
                      closefile(ariskfile);
                   end
                else
                   showmessage ('File '+afilename+ ' not found.  File ignored.  Please clear file list and try again.')
            end;
         Securityfiles : begin
                assign (asecfile, afilename);
                if fileexists (afilename) then
                   begin
                      reset(asecfile);
                      {Get first year of new risk file}
                      read (asecfile, sec_file_rec_new^);
                      first_year := sec_file_rec_new^.year;
                      while not eof (asecfile) do
                         begin
                            read (asecfile, sec_file_rec_new^);
                         end;
                      last_year := sec_file_rec_new^.year;
                      closefile(asecfile);
                   end
                else
                   showmessage ('File '+afilename+ ' not found.  File ignored.  Please clear file list and try again.')
            end;
      end;   {case risk/security}
      dispose (risk_file_rec_new);
      dispose (sec_file_rec_new);

   end;

procedure TCombineRiskFileForm.FileAddButtonClick(Sender: TObject);
   var tempword : string;
        ctr : integer;
        StartYear, EndYear : year_range;
        temp1, temp2 : string;
        Rename : TRiskCombineFileRename;

        UpdatedFile : string;

   begin
      if risksecurityfiles = riskfiles then tempword := 'Risk' else tempword := 'Security/Alliance';
      OpenDialog1.title := 'Choose Additional Input '+tempword+ ' File';
      OpenDialog1.Filter := 'DAT files (*.DAT)|*.DAT|All files (*.*)|*.*';
      with OpenDialog1 do
        Options := Options +
          [ofAllowMultiSelect, ofFileMustExist];
      {if InputFileListBox.Items.count > 0 then
         OpenDialog1.FileName := InputFileListBox.Items.strings[InputFileListBox.Items.count-1]
      else OpenDialog1.FileName := '';   }


      if OpenDialog1.execute then
         begin

            ctr := 0;

            for ctr := 0 to OpenDialog1.Files.Count - 1 do
              itemadd(OpenDialog1.Files[ctr]);

            ctr := 0;

            for ctr := 0 to InputFileListBox.Items.Count - 1 do
              begin
                get_file_years (InputFileListBox.Items.strings[ctr], StartYear, EndYear);

                if (StrPos(PChar(InputFileListBox.Items.strings[ctr]), PChar(InttoStr(StartYear))) = nil) or (StrPos(PChar(InputFileListBox.Items.strings[ctr]), PChar(InttoStr(EndYear))) = nil) then
                  begin

                    Rename := TRiskCombineFileRename.Init(InputFileListBox.Items.strings[ctr], StartYear, EndYear, Addr(UpdatedFile), self);
                    Rename.showmodal;
                    Rename.free;

                    InputFileListBox.Items.strings[ctr] := UpdatedFile;



                  end
              end;
            InputFileListBox.Sorted := true;
            InputFileListBox.Refresh;
            ShowFileDateButtonClick(self);
         end
      else {exited with cancel}
         begin
         end;

   end;
   
procedure TCombineRiskFileForm.CombineButtonClick(Sender: TObject);
   var x : risk_file_range;
       last_output_year : year_range;
       first_file_year, last_file_year : year_range;
       continuing : boolean;
       riskoutfile : risk_file_type_v2;
       secoutfile : security_alliance_file_type;
       {risk_file_rec_new : risk_file_record_type_v2;
       sec_file_rec_new : security_alliance_file_record_type;
       risk_file_rec_new : risk_file_rec_ptr;
       sec_file_rec_new : sec_file_rec_ptr;}

  {combine files here}
    procedure write_file_recs (fromfilename : TFileName;  var toriskfile : risk_file_type_v2;
              var tosecfile : security_alliance_file_type; fromYear, toYear : year_range; var last_output_year : year_range);
                   {comes in with both files open for reading or writing}
       var current_year_existing : year_range;
           {risk_file_existing_rec :  risk_file_record_type_v2;
           sec_file_existing_rec :  security_alliance_file_record_type;}
           risk_file_existing_rec :  risk_file_rec_ptr;
           sec_file_existing_rec :  sec_file_rec_ptr;
           num_lines : integer;
           fromriskfile : risk_file_type_v2;
           fromsecfile : security_alliance_file_type;
       begin
         new (risk_file_existing_rec);
         new (sec_file_existing_rec);
         case risksecurityfiles of
            riskfiles : begin
                assignfile (fromriskfile, fromfilename);
                reset(fromriskfile);
                repeat
                   read (fromriskfile, risk_file_existing_rec^);
                until (risk_file_existing_rec^.year >= FromYear) or (eof(fromriskfile));
                if not (risk_file_existing_rec^.year = FromYear)
                   then showmessage ('Error in write file recs - did not see FromYear');

                {Write any existing file records until ready to use new file}

                if risk_file_existing_rec^.year <= toYear then
                   begin
                      {write this current record}
                      write (toriskfile, risk_file_existing_rec^);
                      {MainForm.riskmemo.lines.strings[num_lines] :=
                         MainForm.riskmemo.lines.strings[num_lines] +
                         (inttostr(risk_file_existing_rec.year));        }
                      last_output_year := risk_file_existing_rec^.year;
                      {Now process the other records, up to target year}
                      {note next line must be <, since read the next record inside loop}
                      while (not eof (fromriskfile)) and (risk_file_existing_rec^.year<toYear) do
                         begin
                            if not eof(fromriskfile) then read (fromriskfile, risk_file_existing_rec^);
                            if risk_file_existing_rec^.year <= toYear then
                               begin
                                  write (toriskfile, risk_file_existing_rec^);
                                  {MainForm.riskmemo.lines.strings[num_lines] :=
                                     MainForm.riskmemo.lines.strings[num_lines] +
                                     ('..' + inttostr(risk_file_existing_rec.year));  }
                                  last_output_year := risk_file_existing_rec^.year;
                               end;
                         end;
                   end;
                closefile(fromriskfile);
              end;          {risk file type}

            Securityfiles : begin
                assignfile (fromsecfile, fromfilename);
                reset(fromsecfile);
                repeat
                   read (fromsecfile, sec_file_existing_rec^);
                until (sec_file_existing_rec^.year >= FromYear) or (eof(fromsecfile));
                if not (sec_file_existing_rec^.year = FromYear)
                   then showmessage ('Error in write file recs - did not see FromYear');

                {Write any existing file records until ready to use new file}

                if sec_file_existing_rec.year <= toYear then
                   begin
                      {write this current record}
                      write (tosecfile, sec_file_existing_rec^);
                      {MainForm.riskmemo.lines.strings[num_lines] :=
                         MainForm.riskmemo.lines.strings[num_lines] +
                         (inttostr(risk_file_existing_rec.year));        }
                      last_output_year := sec_file_existing_rec^.year;
                      {Now process the other records, up to target year}
                      {note next line must be <, since read the next record inside loop}
                      while (not eof (fromsecfile)) and (sec_file_existing_rec^.year<toYear) do
                         begin
                            if not eof(fromsecfile) then read (fromsecfile, sec_file_existing_rec^);
                            if sec_file_existing_rec^.year <= toYear then
                               begin
                                  write (tosecfile, sec_file_existing_rec^);
                                  {MainForm.riskmemo.lines.strings[num_lines] :=
                                     MainForm.riskmemo.lines.strings[num_lines] +
                                     ('..' + inttostr(risk_file_existing_rec.year));  }
                                  last_output_year := sec_file_existing_rec^.year;
                               end;
                         end;
                   end;
                closefile(fromsecfile);
              end;
         end;   {case risk/security}
         dispose (risk_file_existing_rec);
         dispose (sec_file_existing_rec);
       end;                         {proc}

   begin
      if InputFileListBox.Items.count > 0 then
      begin
         case risksecurityfiles of
            riskfiles : begin
                  assignfile (riskoutfile,RiskOutFileName);
                  rewrite (riskoutfile);
               end;
            securityfiles : begin
                  assignfile (secoutfile,RiskOutFileName);
                  rewrite (secoutfile);
               end;
            end;   {case}
         try
            RunningForm.Show;
            self.enabled := false;

            {this will start output with first year of first file}
            get_file_years (InputFileListBox.Items.strings[0], first_file_year, last_file_year);
            first_year := first_file_year;
            last_year := last_file_year;
            continuing := false;

            for x := 0 to InputFileListBox.Items.count-1 do
               if fileexists (InputFileListBox.Items.strings[x]) then
                 begin
                   get_file_years (InputFileListBox.Items.strings[x], first_file_year, last_file_year);
                   if subsetlist.items.strings[x]='y' then
                      begin
                         first_year := strtointdef(FromYearList.items.strings[x], 0);
                         last_year := strtointdef(ToYearList.items.strings[x], 0);
                      end
                   else
                      begin
                         first_year := first_file_year;
                         last_year := last_file_year;
                      end;
                   if continuing then if last_output_year + 1 <> first_year then
                      showmessage ('Check specification for risk file combine - beginning year of input file '+
                         InputFileListBox.Items.strings[x]+ ' is not equal to end of last output + 1;  Last output year ('+InputFileListBox.Items.strings[x-1]+') is '+
                         inttostr(last_output_year)+ ' and first year of new file is '+inttostr(first_year)+'.');
                   {write will right from previous last output year to last_year of this
                    data set, and reset last_output_year to what it writes.}
                   write_file_recs (InputFileListBox.Items.strings[x], riskoutfile, secoutfile,
                                    first_year, last_year, last_output_year);
                   {Now set for next iteration;  just mark that I've done an it.,
                    the next first_year should equal last_output_year + 1}
                   continuing := true;
                 end
               else
                   showmessage ('File '+InputFileListBox.Items.strings[x]+ ' not found.  File ignored.  Please clear file list and try again.');
         except
            self.enabled := true;
            DoneButton.enabled := true;
         end;

         case risksecurityfiles of
            riskfiles : begin
                  closefile (riskoutfile);
               end;
            securityfiles : begin
                  closefile (secoutfile);
               end;
            end;   {case}
         RunningForm.close;
         self.enabled := true;
         DoneButton.enabled := true;
      end;

   end;


procedure TCombineRiskFileForm.ShowFileDateButtonClick(Sender: TObject);
   var x : risk_file_range;
   begin
       if InputFileListBox.Items.count > 0 then
       begin
          DateRangeListBox.clear;
          for x := 0 to InputFileListBox.Items.count-1 do
             if fileexists (InputFileListBox.Items.strings[x]) then
                begin
                   get_file_years (InputFileListBox.Items.strings[x], first_year, last_year);
                   DateRangeListBox.items.add (inttostr(first_year)+' to '+inttostr(last_year));
                end
             else
                showmessage ('File '+InputFileListBox.Items.strings[x]+ ' not found.  File ignored.  Please clear file list and try again.');
       end;
   end;   {showdates}


procedure TCombineRiskFileForm.clearlistbuttonClick(Sender: TObject);
   begin
      InputFileListBox.clear;
      DateRangeListBox.clear;
      subsetlist.clear;
      numList.clear;
      fromyearlist.clear;
      toyearlist.clear;
   end;

procedure TCombineRiskFileForm.CancelButtonClick(Sender: TObject);
   begin
      close;
   end;


procedure TCombineRiskFileForm.DoneButtonClick(Sender: TObject);
   begin
      if Risksecurityfiles = riskfiles then
         {InputFileListBox.Items.SaveToFile(configuration.eugene_directory + combineriskfilenames); }
         InputFileListBox.Items.SaveToFile(configuration.eugene_directory + 'combineriskfilenames.dat');
   end;


procedure TCombineRiskFileForm.RiskFilesRadioButtonClick(Sender: TObject);
begin
   Risksecurityfiles := riskfiles;
   RiskOutFileName := (configuration.eugene_directory + 'outRiskTEST.dat');
   clearlistbuttonClick(self);
end;

procedure TCombineRiskFileForm.SecurityFilesButtonClick(Sender: TObject);
begin
   Risksecurityfiles := securityfiles;
   RiskOutFileName := (configuration.eugene_directory + 'outSecurityTEST.dat');
   clearlistbuttonClick(self);
end;

procedure TCombineRiskFileForm.YearSubsetButtonClick(Sender: TObject);
   var whichitem : integer;
       ThisForm : TCombineRiskFilesYearsForm;
       fromyear, toyear : year_range;
       use : boolean;

     function anitemisselected (var whichitem : integer): boolean;
      var x : integer;
      begin
         result := false;
         whichitem := -1;
         for x := 0 to InputFileListBox.Items.count-1 do
            if InputFileListBox.selected[x] then
               begin
                  whichitem := x;
                  result := true;
               end;
      end;

begin
   if anitemisselected (whichitem) then
      begin
         try
            ThisForm := TCombineRiskFilesYearsForm.createwithitem (whichitem, self);
            ThisForm.showmodal_returns (fromyear, toyear, use);
            fromyearlist.items[whichitem] := inttostr(fromyear);
            toyearlist.items[whichitem] := inttostr(toyear);
            case use of
               true : subsetlist.items[whichitem] := 'y';
               false : subsetlist.items[whichitem] := 'n';
            end;
         finally
            ThisForm.free;
         end;
      end
   else
      showmessage ('You must highlight a file and then hit the "year subset" button');
end;




procedure TCombineRiskFileForm.RetrieveOldFileListButtonClick(
  Sender: TObject);
  var x : integer;
begin
      combineriskfilenames := 'CombinedRiskFileNames.dat';
      if fileexists (configuration.eugene_directory + combineriskfilenames) then
         begin
            InputFileListBox.Items.loadFromFile(configuration.eugene_directory + combineriskfilenames);
            for x := 0 to InputFileListBox.Items.count-1 do
               begin
                  numList.items.add(inttostr(x));
                  subsetlist.items.add('n');
                  DateRangeListBox.items.add('0');
                  FromYearList.items.add('0');
                  ToYearList.items.add('0');
               end;
         end;
end;

procedure TCombineRiskFileForm.RemoveFileButtonClick(Sender: TObject);
var
  i, j, k, l : integer;

begin
  {removes selected file(s) from the list}

 { for k := 0 to InputFileListBox.SelCount do
  begin
    RemoveCount := InputFileListBox.SelCount+1;
    for i := 0 to (InputFileListBox.Items.Count - RemoveCount) do
    begin
      if (InputFileListBox.Selected[i] = true) then
        begin
          {RemoveCount := RemoveCount + 1; }  {
          InputFileListBox.Items.Delete(i);
          DateRangeListBox.Items.Delete(i);
          NumList.Items.Delete(i);
          for j := 0 to (InputFileListBox.Items.Count - 1) do
            NumList.Items[j] := inttostr(j);
          SubsetList.Items.Delete(i);
          FromYearList.Items.Delete(i);
          ToYearList.Items.Delete(i);
        end;
      end;
    end;     }
    i := InputFileListBox.Count;
    j := InputFileListBox.Count - InputFileListBox.SelCount;
    k := 0;
    while (i <> j) do
    begin
      k := 0;
      while k <= (InputFileListBox.Items.Count - 1) do
      begin
        if(InputFileListBox.Selected[k] = true) then
        begin
          i := i - 1;    {
          InputFileListBox.Items.Delete(i);
          DateRangeListBox.Items.Delete(i);
          NumList.Items.Delete(i);
          for l := 0 to (InputFileListBox.Items.Count - 1) do
            NumList.Items[l] := inttostr(l);
          SubsetList.Items.Delete(i);
          FromYearList.Items.Delete(i);
          ToYearList.Items.Delete(i);  }
          NumList.Items[k] := 'x';
        end;
        k := k + 1;
      end;
    end;


    i := InputFileListBox.Count;
    j := InputFileListBox.Count - InputFileListBox.SelCount;
    k := 0;
    while (i <> j) do
    begin
      k := 0;
      while k <= (InputFileListBox.Items.Count - 1) do
      begin
        if(NumList.Items[k] = 'x') then
        begin
          i := i - 1;
          InputFileListBox.Items.Delete(k);
          DateRangeListBox.Items.Delete(k);
          NumList.Items.Delete(k);

          SubsetList.Items.Delete(k);
          FromYearList.Items.Delete(k);
          ToYearList.Items.Delete(k);
       
        end;
        k := k + 1;
      end;
    end;

    for l := 0 to (InputFileListBox.Items.Count - 1) do
      NumList.Items[l] := inttostr(l);


end;

end.
