unit EUTypes4;
  {has war data procedures.}

{EUGene  Copyright 1997, 1998+  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

uses windows, dialogs, forms, eutypes1, TraceUnit, ProgressInterruptWind, cmnprocD,
     sysUtils, FileError, math;

type
   war_entry = record
      WarNo : war_id_range;
      WarName : string;
      YrBeg1, YrEnd1, YrBeg2, YrEnd2 : year_range;
      MonBeg1, MonEnd1, MonBeg2, MonEnd2 : month_range;
      DayBeg1, DayEnd1, DayBeg2, DayEnd2 : day_range;
      Has_second_war_dates : boolean;
      DurationDays : integer;
      BattleDeaths : integer;
      CenSubSy, SubSysEa : boolean;
      MajPowIn, MajPowEa : boolean;
      WestHem, Europe, Africa, MidEast, Asia, Oceania : boolean;
      Edition : 0..4;
      Version : real;
      end;  {war entry}
   war_data_list_type = array of war_entry;

   participant_entry = record
      WarNo : war_id_range;
      StateNum : ccode_range;
      StateAbb : string;
      YrBeg1, YrEnd1, YrBeg2, YrEnd2 : year_range;
      MonBeg1, MonEnd1, MonBeg2, MonEnd2 : month_range;
      DayBeg1, DayEnd1, DayBeg2, DayEnd2 : day_range;
      Has_second_war_dates : boolean;
      DurationDays : integer;
      BattleDeaths : integer;
      Outcome : 1..6;
      Initiate : boolean;
      SysStat : 1..4;
      PrWarPop : integer;
      PrWarArm : integer;
      WestHem, Europe, Africa, MidEast, Asia, Oceania : boolean;
      Version : real;
      end;
   participant_data_list_type = array of participant_entry;

   dyad_war_entry = record
         overall_war_record : war_id_range;
         participant_record1, participant_record2 : war_participant_range;
      end;
   dyad_data_list_type = array[dyadic_war_range] of dyad_war_entry;

   war_index_type = array[war_id_range] of integer;   {points from war # to a record}
   war_participant_index_type = array[war_participant_range] of integer;   {points from war # to the first participant record}

   TWar_data_obj = class(Tobject)
      constructor init (war_file_name, war_participant_file_name : TFileName);
      destructor destroy; override;
      function initialized : boolean;
      function get_war_number (ccode1, ccode2 : ccode_range; year : year_range) : war_id_range;  {rtns war # for accessing other war data}
      function is_war (ccode1, ccode2 : ccode_range; year : year_range) : boolean;
      function is_war_initiation (ccode1, ccode2 : ccode_range; year : year_range) : boolean;  {did A initiate war vs. B this year}
      function is_war_onset (ccode1, ccode2 : ccode_range; year : year_range) : boolean;{a new war began between A and B this year}
      function is_ongoing (ccode1, ccode2 : ccode_range; year : year_range) : boolean; {a war is ongoing between A and B this year}
      function is_war_joiner (ccode1, ccode2 : ccode_range; year : year_range) : boolean;  {is A joiner into war vs. B this year}
      function is_war_originator (ccode1, ccode2 : ccode_range; year : year_range) : boolean; {is A and B both originators in a war between them this year.  exclusive from is_war_joiner.}
    private
      overall_war_data : war_data_list_type;
      war_index : war_index_type;
      participant_data : participant_data_list_type;
      war_participant_index : war_participant_index_type;
      dyadic_data : dyad_data_list_type;
      created : boolean;
   end;   {TWar_data_obj}

implementation

   uses euinoutd, eutypes2;

   constructor TWar_data_obj.init (war_file_name, war_participant_file_name : TFileName);
      var start_mem : longint;
          x, current_rec, temp : integer;
          war_trace : TTrace_obj;
          infile : text;
          num_wars, num_participant_records : integer;

      procedure make_dyadic_wars;
         begin

            {basically put in stuff from make dyadic disputes here is all that's needed.}

            
         end;

      begin
         {NOTE: -999 is missing / NA}

         try    {for try... finally for procedure, with war_trace}
            trace.enter ('Entered War initialization procedure');
            start_mem := memavail;
            war_trace := TTrace_obj.init(trace.get_trace_level);
            war_trace.enter('Initializing main war data ');

            {Step 1:  read overall war data.  }
            try    {try...except for file access loop}
               try   {try...finally for file closing.}
                  {Now, read in data from file}
                  trace.message('Reading Overall War Data');
                  assign (infile, war_file_name);
                  reset (infile);

                  {initialize the overall war data data structures.}
                  setlength (overall_war_data, 0);
                  for x := 0 to top_war_num do war_index[x] := initialized_value;
                  num_wars := 0;

                  {There is currently NO header line to read out of the file}
                  {readln(infile);}
                  while (not (eof (infile))) and (current_rec <= max_wars) do
                     begin  {read a war}
                        {make space on array for the war}
                        inc(num_wars);
                        current_rec := num_wars - 1;
                        setlength(overall_war_data, num_wars);
                        war_trace.tick ('Executing Procedure: Initialize War Data', 100);
                        overall_war_data[current_rec].WarNo := read_csv_int (infile);
                        overall_war_data[current_rec].WarName := read_csv_string (infile);
                        overall_war_data[current_rec].YrBeg1 := read_csv_int (infile);
                        overall_war_data[current_rec].MonBeg1 := read_csv_int (infile);
                        overall_war_data[current_rec].DayBeg1 := read_csv_int (infile);
                        overall_war_data[current_rec].YrEnd1 := read_csv_int (infile);
                        overall_war_data[current_rec].MonEnd1 := read_csv_int (infile);
                        overall_war_data[current_rec].DayEnd1 := read_csv_int (infile);
                        temp := read_csv_int (infile);
                        if temp = -999 then
                           begin
                              {there is no second war date entry.  Read the vars just to skip them.}
                              temp := read_csv_int (infile);
                              temp := read_csv_int (infile);
                              temp := read_csv_int (infile);
                              temp := read_csv_int (infile);
                              temp := read_csv_int (infile);
                              overall_war_data[current_rec].Has_second_war_dates := false;
                           end
                        else
                           begin   {there is a second war date entry}
                              overall_war_data[current_rec].YrBeg2 := temp;
                              overall_war_data[current_rec].MonBeg2 := read_csv_int (infile);
                              overall_war_data[current_rec].DayBeg2 := read_csv_int (infile);
                              overall_war_data[current_rec].YrEnd2 := read_csv_int (infile);
                              overall_war_data[current_rec].MonEnd2 := read_csv_int (infile);
                              overall_war_data[current_rec].DayEnd2 := read_csv_int (infile);
                              overall_war_data[current_rec].Has_second_war_dates := true;
                           end;
                        overall_war_data[current_rec].DurationDays := read_csv_int (infile);
                        overall_war_data[current_rec].BattleDeaths := read_csv_longint (infile);
                        overall_war_data[current_rec].CenSubSy := read_csv_boolean (infile);
                        overall_war_data[current_rec].SubSysEa := read_csv_boolean (infile);
                        overall_war_data[current_rec].MajPowIn := read_csv_boolean (infile);
                        overall_war_data[current_rec].MajPowEa := read_csv_boolean (infile);
                        overall_war_data[current_rec].WestHem := read_csv_boolean (infile);
                        overall_war_data[current_rec].Europe := read_csv_boolean (infile);
                        overall_war_data[current_rec].Africa := read_csv_boolean (infile);
                        overall_war_data[current_rec].MidEast := read_csv_boolean (infile);
                        overall_war_data[current_rec].Oceania := read_csv_boolean (infile);
                        overall_war_data[current_rec].Edition := read_csv_int (infile);
                        overall_war_data[current_rec].Version := read_csv_real (infile);

                        if not (eof (infile)) then
                          readln (infile);      {go to begin of next record}
                       {Just successfully read a war record:  put it on the war index list }
                        war_index[overall_war_data[current_rec].WarNo] := current_rec;
                     end;             {while not eof (infile);}

                  if num_wars >= max_wars then
                     begin
                       ShowMessage ('Stopped reading main war data at record '+inttostr(num_wars)+
                                ';  maximum number of wars is '+inttostr(max_wars));
                   end;
               finally
                  CloseFile (infile);
               end;
            except
               on EUserInterrupt do raise;
               on EInOutError do
                 begin
                    FileErrorBox.maindo ('Error opening file "'+war_file_name+ '"',
                                         'File could not be opened for input.',
                                         'File may be in use by another program, or may be missing.');
                    FileErrorBox.showmodal;
                    raise;
                 end;
            end;

            {Step 2: read participant data.}
            try    {try...except for file access loop}
               try   {try...finally for file closing.}
                  trace.message('Reading Participant War Data');
                  {Now, read in data from file}
                  assign (infile, war_participant_file_name);
                  reset (infile);

                  {initialize the overall war data data structures.}
                  setlength (participant_data, 0);
                  for x := 0 to max_war_participants do war_participant_index[x] := initialized_value;
                  num_participant_records := 0;

                  {There is currently NO header line to read out of the file}
                  {readln(infile);}
                  while (not (eof (infile))) and (current_rec <= max_war_participants) do
                     begin  {read a participant record}
                        {make space on array for the participant rec}
                        inc(num_participant_records);
                        current_rec := num_participant_records - 1;
                        setlength(overall_war_data, num_participant_records);
                        war_trace.tick ('Executing Procedure: Initialize Participant-War Data', 350);

                        participant_data[current_rec].WarNo := read_csv_int (infile);
                        participant_data[current_rec].StateNum := read_csv_int (infile);
                        participant_data[current_rec].StateAbb := read_csv_string (infile);
                        participant_data[current_rec].YrBeg1 := read_csv_int (infile);
                        participant_data[current_rec].MonBeg1 := read_csv_int (infile);
                        participant_data[current_rec].DayBeg1 := read_csv_int (infile);
                        participant_data[current_rec].YrEnd1 := read_csv_int (infile);
                        participant_data[current_rec].MonEnd1 := read_csv_int (infile);
                        participant_data[current_rec].DayEnd1 := read_csv_int (infile);
                        temp := read_csv_int (infile);
                        if temp = -999 then
                           begin
                              {there is no second war date entry.  Read the vars just to skip them.}
                              temp := read_csv_int (infile);
                              temp := read_csv_int (infile);
                              temp := read_csv_int (infile);
                              temp := read_csv_int (infile);
                              temp := read_csv_int (infile);
                              participant_data[current_rec].Has_second_war_dates := false;
                           end
                        else
                           begin   {there is a second war date entry}
                              participant_data[current_rec].YrBeg2 := temp;
                              participant_data[current_rec].MonBeg2 := read_csv_int (infile);
                              participant_data[current_rec].DayBeg2 := read_csv_int (infile);
                              participant_data[current_rec].YrEnd2 := read_csv_int (infile);
                              participant_data[current_rec].MonEnd2 := read_csv_int (infile);
                              participant_data[current_rec].DayEnd2 := read_csv_int (infile);
                              participant_data[current_rec].Has_second_war_dates := true;
                           end;
                        participant_data[current_rec].DurationDays := read_csv_int (infile);
                        participant_data[current_rec].BattleDeaths := read_csv_longint (infile);
                        participant_data[current_rec].Outcome := read_csv_int (infile);
                        participant_data[current_rec].Initiate := read_csv_boolean (infile);
                        participant_data[current_rec].SysStat := read_csv_int (infile);
                        participant_data[current_rec].PrWarPop := read_csv_int (infile);
                        participant_data[current_rec].PrWarArm := read_csv_int (infile);
                        participant_data[current_rec].WestHem := read_csv_boolean (infile);
                        participant_data[current_rec].Europe := read_csv_boolean (infile);
                        participant_data[current_rec].Africa := read_csv_boolean (infile);
                        participant_data[current_rec].MidEast := read_csv_boolean (infile);
                        participant_data[current_rec].Asia := read_csv_boolean (infile);
                        participant_data[current_rec].Oceania := read_csv_boolean (infile);
                        participant_data[current_rec].Version := read_csv_real (infile);

                        if not (eof (infile)) then
                          readln (infile);      {go to begin of next record}
                       {Just successfully read a war participant record:  put it on the war index list
                        if this is the first record seen for this war.}
                        if war_participant_index[participant_data[current_rec].WarNo] = initialized_value then
                           war_participant_index[participant_data[current_rec].WarNo] := current_rec;
                     end;             {while not eof (infile);}

                  if num_participant_records >= max_war_participants then
                     begin
                       ShowMessage ('Stopped reading participant war data at record '+inttostr(num_participant_records)+
                                ';  maximum number of particpant records is '+inttostr(max_war_participants));
                   end;
               finally
                  CloseFile (infile);
               end;
            except
               on EUserInterrupt do raise;
               on EInOutError do
                 begin
                    FileErrorBox.maindo ('Error opening file "'+war_participant_file_name+ '"',
                                         'File could not be opened for input.',
                                         'File may be in use by another program, or may be missing.');
                    FileErrorBox.showmodal;
                    raise;
                 end;
            end;


            {Step 3: make it dyadic.}
            make_dyadic_wars;


         finally
            created := true;
            war_trace.exit('Finished Initializing main war data, including overall, participant, and dyadic data. ');
            war_trace.free;
            trace.message (' required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
            trace.exit('Finished initializing raw war data procedure.');
         end;

      end;  {procedure.}

   destructor TWar_data_obj.destroy;
      begin
         inherited destroy;
      end;

   function TWar_data_obj.initialized : boolean;
      begin
         if created then result := true;
      end;

   function TWar_data_obj.get_war_number (ccode1, ccode2 : ccode_range; year : year_range) : war_id_range;  {rtns war # for accessing other war data}
      begin
      end;

   function TWar_data_obj.is_war (ccode1, ccode2 : ccode_range; year : year_range) : boolean;
      begin
      end;

   function TWar_data_obj.is_war_initiation (ccode1, ccode2 : ccode_range; year : year_range) : boolean;  {did A initiate war vs. B this year}
      begin
      end;

   function TWar_data_obj.is_war_onset (ccode1, ccode2 : ccode_range; year : year_range) : boolean;{a new war began between A and B this year}
      begin
      end;

   function TWar_data_obj.is_ongoing (ccode1, ccode2 : ccode_range; year : year_range) : boolean; {a war is ongoing between A and B this year}
      begin
      end;

   function TWar_data_obj.is_war_joiner (ccode1, ccode2 : ccode_range; year : year_range) : boolean;  {is A joiner into war vs. B this year}
      begin
      end;

   function TWar_data_obj.is_war_originator (ccode1, ccode2 : ccode_range; year : year_range) : boolean; {is A and B both originators in a war between them this year.  exclusive from is_war_joiner.}
      begin
      end;


end.
 