{Unit to create time series of MID activity so can compute BOP etc. on each day.}

unit MidOverTime;

interface

uses EUTypes1, EUTypes2, EUTypesMID, sysutils;

type
   start_end_type = (se_start, se_end);
   side_type = (side_A, side_B);

   actor_time_record = record
         date : Tdatetime;
         StEnd : start_end_type;
         ccode : ccode_range;
         side : side_type;
         idnum : dispute_id_range;
         hostlev : hostlevtype;
      end;  {rec}

   ata_type = array of actor_time_record;

   time_interval_rec = record
         idnum: dispute_id_range;      {MID #}
         interval_start, interval_end : Tdatetime;
         actorsA, actorsB : array of
            record
               ccode: ccode_range;
               hostlev : hostlevtype;
            end;
         {ccode : ccode_range;
         StDay, EndDay: MIDdaytype;
         StMonth, EndMonth: month_range;
         StYear, EndYear: year_range;    }
         {HostLev: hostlevtype;}
      end;      {one_time_rec}
   time_array = array of time_interval_rec;


   TMID_Over_Time_obj = class(Tobject)
      constructor init (configuration : configuration_type); virtual; abstract;
      function initialized : boolean;
      function mem_needed : longint; virtual; abstract;
      destructor destroy; virtual; abstract;
      function get_last_overtime_rec : integer;
      function get_record (overtime_rec_num : integer) : time_interval_rec;
      procedure output_to_file (afilename1, afilename2 : TFileName);

     private
      created : boolean;
      data : time_array;

    end;   {class}


    {Need a subclass to do this reading/using raw COW data (vs. Maoz)}
   TCOWMID_Over_Time_obj = class(TMID_Over_Time_obj)
      constructor init (configuration : configuration_type); override;
      destructor destroy; override;
      function mem_needed : longint; override;
     private
      country_disputes : TCountry_dispute_data_obj;
      overall_disputes : Toverall_dispute_data_obj;
     end;

implementation

   uses dialogs, math, euinoutd, dateutils, TraceUnit, errbx;

   function TMID_Over_Time_obj.initialized : boolean;
      begin
         if created then result := true else result := false;
      end;

   function TMID_Over_Time_obj.get_last_overtime_rec : integer;
      begin
         result := length(data) - 1;    {loop 0 to this for access}
      end;

   function TMID_Over_Time_obj.get_record (overtime_rec_num : integer) : time_interval_rec;
      begin
         if overtime_rec_num > length(data) - 1 then EUGeneError ('Tried to get a record from MIDs over time structure with out of range record #.  Notify programmer.  Fatal error.',1,stop,error_log);
         result := data[overtime_rec_num];
      end;

   procedure TMID_Over_Time_obj.output_to_file (afilename1, afilename2 : TFileName);
      var atimeinterval, x, days : integer;
          outfile : text;
          styear, stmonth, stday, endyear, endmonth, endday : word;
          interval_num, prior_mid_num : integer;

          {Write out in two formats, a listing format, and a format suitable for stata to merge
           and compute capability balances.}
      begin
         try
            rewrite (outfile, afilename1);
            writeln (outfile, 'Start Year,Start Month,Start Day,End Year,End Month,End Day,MIDNum,ActorsA, ActorsB');
            for atimeinterval := 0 to get_last_overtime_rec do
               begin
                  decodedate(data[atimeinterval].interval_start, styear, stmonth, stday);
                  write(outfile, styear, ',',stmonth,',',stday,',');
                  decodedate(data[atimeinterval].interval_end, endyear, endmonth, endday);
                  write(outfile, endyear, ',',endmonth, ',',endday,',');
                  write(outfile, data[atimeinterval].idnum,',');
                  for x := low(data[atimeinterval].actorsA) to high(data[atimeinterval].actorsA) do
                        write (outfile,data[atimeinterval].actorsA[x].ccode,' (A) (',data[atimeinterval].actorsA[x].hostlev,'),');
                  for x := low(data[atimeinterval].actorsB) to (high(data[atimeinterval].actorsB)-1) do
                     write (outfile,data[atimeinterval].actorsB[x].ccode,' (B) (',data[atimeinterval].actorsB[x].hostlev,'),');
                  write (outfile,data[atimeinterval].actorsB[high(data[atimeinterval].actorsB)].ccode, ' (B) (',data[atimeinterval].actorsB[high(data[atimeinterval].actorsB)].hostlev,')');
                  writeln (outfile);
               end;
            close (outfile);
         except
            EUGeneError ('Unable to write to file 1 in outputting MID over time records.  Check file and try again.', 1, continue, error_log);
            raise;
         end;     {except}

         {Now the version for merging capabilities in stata}
         try
            interval_num := 1;
            prior_mid_num := 0;
            rewrite (outfile, afilename2);
            writeln (outfile, 'StartYear,StartMonth,StartDay,EndYear,EndMonth,EndDay,IntervalNumber,IntervalDays,MIDNum,ActorA, ActorAHost, ActorB, ActorBHost');
            for atimeinterval := 0 to get_last_overtime_rec do
               begin
                   {write a separate line for each state on sideA, and sideB}
                  if data[atimeinterval].idnum <> prior_mid_num then
                     begin
                        interval_num := 1;
                        prior_mid_num := data[atimeinterval].idnum;
                     end
                     else inc(interval_num);
                  for x := low(data[atimeinterval].actorsA) to high(data[atimeinterval].actorsA) do
                     begin
                        decodedate(data[atimeinterval].interval_start, styear, stmonth, stday);
                        write(outfile, styear, ',',stmonth,',',stday,',');
                        decodedate(data[atimeinterval].interval_end, endyear, endmonth, endday);
                        write(outfile, endyear, ',',endmonth, ',',endday,',');
                        write(outfile, interval_num,',');
                        days := round (data[atimeinterval].interval_end - data[atimeinterval].interval_start + 1);
                        write(outfile, days,',');
                        write(outfile, data[atimeinterval].idnum,',');
                        write (outfile,data[atimeinterval].actorsA[x].ccode,',');
                        write (outfile,data[atimeinterval].actorsA[x].hostlev,',');
                        write (outfile,'-9, -9');
                        writeln (outfile);
                     end;
                  for x := low(data[atimeinterval].actorsB) to high(data[atimeinterval].actorsB) do
                     begin
                        decodedate(data[atimeinterval].interval_start, styear, stmonth, stday);
                        write(outfile, styear, ',',stmonth,',',stday,',');
                        decodedate(data[atimeinterval].interval_end, endyear, endmonth, endday);
                        write(outfile, endyear, ',',endmonth, ',',endday,',');
                        write(outfile, interval_num,',');
                        days := round(data[atimeinterval].interval_end - data[atimeinterval].interval_start + 1);
                        write(outfile, days,',');
                        write(outfile, data[atimeinterval].idnum,',');
                        write (outfile,'-9, -9,');
                        write (outfile,data[atimeinterval].actorsB[x].ccode,',');
                        write (outfile,data[atimeinterval].actorsB[x].hostlev);
                        writeln (outfile);
                     end;
               end;
            close (outfile);
            ShowMessage ('Finished outputting MIDs over time to output files '+afilename1+' and '+afilename2+'.  '+inttostr(get_last_overtime_rec+1)+' records output.');
         except
            EUGeneError ('Unable to write to file 1 in outputting MID over time records.  Check file and try again.', 1, continue, error_log);
            raise;
         end;     {except}
      end;

   { ---------------------------------------------------------- }

   function TCOWMID_Over_Time_obj.mem_needed : longint;
      begin                 {rough estimate with 2500 mids}
         result := sizeof(country_disputes) + sizeof(overall_disputes) +
                   2500*sizeof(time_interval_rec);
      end;

   { ---------------------------------------------------------- }

   destructor TCOWMID_Over_Time_obj.destroy;
      var x : integer;
      begin
         try
            country_disputes.free;
            overall_disputes.free;
            created := false;
            inherited destroy;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory in MID Over Time object.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;

   { ---------------------------------------------------------- }

   constructor TCOWMID_Over_Time_obj.init (configuration : configuration_type);

      var
         Time_trace : TTrace_obj;
         heapneeded, start_mem : longint;
         country_dispute, x, y, first_of_dispute, last_of_dispute, arec, arec2 : integer;
         current_start_rec, current_end_rec : integer;
         earlier_date, later_date : boolean;
         actor_time_array : ata_type;
         tempactorrec : actor_time_record;
         current_data_record : integer;
         current_date : Tdatetime;
         overall_start_date, overall_end_date : Tdatetime;
         tempday, tempstartday, tempendday : integer;
         processing_end : boolean;

      procedure add_country_rec (var actor_time_array : ata_type; accode : ccode_range; stend : start_end_type; ayear : year_range;
                                 amonth : month_range; aday : MIDdaytype;  side_A_bool_in: boolean;
                                 midnum : integer; hostlev : hostlevtype; overall_start_date, overall_end_date : Tdatetime);
         var changed_day : boolean;
         begin
            setlength(actor_time_array, length(actor_time_array)+1);
            changed_day := false;
            if aday = -9 then {missing day; set to 15th;  Only possible problem with this would be
                               if it made the date after the last day of the MID, or before
                               the first day we really have.
                               Need to set to 1st day of dispute, probably.}
               begin
                   aday := 15;
                   changed_day := true;
               end;
            if tryencodedate (ayear, amonth, aday, actor_time_array[high(actor_time_array)].date) then
               begin
                  {date is OK, but need to make sure it didn't go beyond the overall end or before the
                   overall start, which could happen if the -9 was changed to a problematic date.}
                  if changed_day then
                     begin
                        if actor_time_array[high(actor_time_array)].date < overall_start_date then
                           actor_time_array[high(actor_time_array)].date := overall_start_date;
                        if actor_time_array[high(actor_time_array)].date > overall_end_date then
                           actor_time_array[high(actor_time_array)].date := overall_end_date;
                     end;
               end
               else
                  begin
                     EUGeneError ('Error encoding date in add_country_rec in init for mid over time.  notify programmer.  Probably date error',0, continue,error_log);
                     actor_time_array[length(actor_time_array)-1].date := encodedate (min_year, 1, 1);
                  end;
            actor_time_array[high(actor_time_array)].StEnd := stend;
            actor_time_array[high(actor_time_array)].ccode := accode;
            if side_A_bool_in = true then
               actor_time_array[high(actor_time_array)].side := side_A
               else actor_time_array[high(actor_time_array)].side := side_B;
            actor_time_array[high(actor_time_array)].idnum := midnum;
            actor_time_array[high(actor_time_array)].hostlev := hostlev;
         end;

      procedure add_actor_to_set (rec_num : integer;  aside : side_type; accode : ccode_range; ahostlev : hostlevtype);
         begin
            case aside of
               side_A : begin
                         setlength (data[rec_num].actorsA, length(data[rec_num].actorsA) + 1);
                         data[rec_num].actorsA[high(data[rec_num].actorsA)].ccode := accode;
                         data[rec_num].actorsA[high(data[rec_num].actorsA)].hostlev := ahostlev;
                      end;
               side_B : begin
                         setlength (data[rec_num].actorsB, length(data[rec_num].actorsB) + 1);
                         data[rec_num].actorsB[high(data[rec_num].actorsB)].ccode := accode;
                         data[rec_num].actorsB[high(data[rec_num].actorsB)].hostlev := ahostlev;
                      end;
               end;   {case}
         end;

      procedure remove_actor_from_set (rec_num : integer;  aside : side_type; accode : ccode_range);
         var index, x : integer;
         begin
            case aside of
               side_A : begin
                         {find state}
                         index := 0;
                         while (data[rec_num].actorsA[index].ccode <> accode) and
                                (index <= high(data[rec_num].actorsA)) do inc(index);
                         if index > high(data[rec_num].actorsA) then EUGeneError ('searched for ccode in remove_actor_from_set in proc init, but didnt find it.  program error.',1,stop,error_log)
                            else
                            begin   {if OK, shift all down}
                               for x := index to high(data[rec_num].actorsA)-1 do
                                  data[rec_num].actorsA[x] := data[rec_num].actorsA[x+1];
                               setlength(data[rec_num].actorsA, length(data[rec_num].actorsA)-1);
                            end;
                      end;
               side_B : begin
                         index := 0;
                         while (data[rec_num].actorsB[index].ccode <> accode) and
                                (index <= high(data[rec_num].actorsB)) do inc(index);
                         if index > high(data[rec_num].actorsB) then EUGeneError ('searched for ccode in remove_actor_from_set in proc init, but didnt find it.  program error.',1,stop,error_log)
                            else
                            begin   {if OK, shift all down}
                               for x := index to high(data[rec_num].actorsB)-1 do
                                  data[rec_num].actorsB[x] := data[rec_num].actorsB[x+1];
                               setlength(data[rec_num].actorsB, length(data[rec_num].actorsB)-1);
                            end;
                      end;
               end;   {case}
         end;

                { ---------------------------------- }

      begin   {main proc init for  TCOWMID_Over_Time_obj}
         created := false;
         country_disputes := nil;
         overall_disputes := nil;
         setlength (data, 0);

         try
            Time_trace := ttrace_obj.init (trace.get_trace_level);
            start_mem := memavail;
            heapneeded := mem_needed;
            if debug[4] then
               begin
                  trace.message ('Dyadic Dispute array size calculation');
                  trace.message ('Calc is that '+inttostr(heapneeded)+' needed for all years.');
                  trace.message ('Max avail mem block is '+inttostr(MaxAvail));
               end;
            if MaxAvail <= (heapneeded) then
               begin
                  EUGeneError ('Not enough memory for dyadic dispute array. ', 5, stop, error_log);
               end;

            {First, must read/create country dispute data.   }
            country_disputes := TCountry_dispute_data_obj.init (configuration.cow_mid_actor_file_nameB, configuration.cow_mid_data_format);

            if not(country_disputes.initialized) then
               begin
                  EUGeneError ('Country_dispute initialization not completed succesfully during dyadid dispute init. ',
                                  5, stop, error_log);
               end;

            {Also, now, must read in overall dispute information}
            overall_disputes := Toverall_dispute_data_obj.init (configuration.cow_mid_case_file_nameA,
                                                        configuration.cow_mid_name_file_nameC, configuration.cow_mid_data_format);

            if not(overall_disputes.initialized) then
               begin
                  EUGeneError ('Overall_dispute initialization not completed succesfully during dyadid dispute init. ',
                                  5, stop, error_log);
               end;

            {Go through country-dispute list.  For every dispute, create record of all time periods.}
            country_dispute := 0;    {start with first record}
            while (country_dispute <= country_disputes.get_last_dispnum) do
               begin
                  {trace.message ('doing country dispute # '+inttostr(country_dispute));  }
                  Time_trace.tick ('Executing Procedure: Create over-time disputes ',country_disputes.get_num_country_disputes);
                  first_of_dispute := country_dispute;
                  {This marks first country-disp of this dispute.  Could be multiple countries on each side.
                   Need to mark last of side 1, first of side 2, last of side 2}
                  last_of_dispute :=  first_of_dispute;
                  repeat
                     last_of_dispute := last_of_dispute + 1;
                  until (last_of_dispute > country_disputes.get_last_dispnum) or
                        (country_disputes.get_disputeid(last_of_dispute) <> country_disputes.get_disputeid(first_of_dispute));
                  {Now have hit last record in this dispute.}
                  last_of_dispute := last_of_dispute - 1;    {decrement back to last}
                  if not (first_of_dispute <= last_of_dispute) then
                     begin
                        EUGeneError ('Error in make over-time disputes procedure ' +
                              ': loops within country disputes not set properly.  Notify programmer',
                              5, continue, error_log);
                     end;
                  {Now have the first, last country dispute marked, so can create sub-periods and dyads from all the combos.}

                  {create individual actor records}
                  setlength (actor_time_array, 0);



{****

((24852, se_start, 710, side_B, 1806, 2),
(24852, se_end, 710, side_B, 1806, 2),
(24855, se_start, 811, side_B, 1806, 4),
(24855, se_start, 817, side_A, 1806, 4),
(24855, se_start, 2, side_A, 1806, 4),
(24855, se_end, 817, side_A, 1806, 4),
(25321, se_end, 811, side_B, 1806, 4),
(25321, se_end, 2, side_A, 1806, 4))



Missing start can't be first event!

**** \

When writing records, if it is a missing day record, if the 15th is earlier than overall start date, then
 use overall start date instead of 15th.}

                  {First figure out first, last day for any state in the MID}
                  {This assumes all MID data has valid dates on it.}
                  {Want overall start to be first date without a -9 if there's a record with same month;
                   if no other record in month, then use 15th.}
                  {Do this by first, Take date of first country dispute.Self
                   Then, if next country rec is a more correct start date, then set that.
                     It is more correct if:
                        start is missing, and this is not missing, and is in the same month or earlier.
                        start is missing, and this is missing, and is in an earlier month.
                        start is not missing, and this is not missing, and it is earlier.
                        start is not missing, and this is missing, and is in an earlier month.   }

                  if country_disputes.get_stday(first_of_dispute) = -9 then
                     tempday := 15
                     else tempday := country_disputes.get_stday(first_of_dispute);
                  overall_Start_date := EncodeDate(country_disputes.get_styear(first_of_dispute),
                                        country_disputes.get_stmonth(first_of_dispute),
                                        tempday);
                  current_start_rec := first_of_dispute;

                  for arec := first_of_dispute+1 to last_of_dispute do
                     begin
                        earlier_date := false;
                        {Now see if we want to change the overall start date record because this record is earlier.}
                        {change if: start is missing, and this is not missing, and is in the same month or earlier.}
                        if (country_disputes.get_stday(current_start_rec) = -9) and (country_disputes.get_stday(arec) <> -9) and
                             ( (country_disputes.get_styear(arec)<country_disputes.get_styear(current_start_rec)) or
                               ( (country_disputes.get_styear(arec)=country_disputes.get_styear(current_start_rec)) and (country_disputes.get_stmonth(arec)<=country_disputes.get_stmonth(current_start_rec)) ) ) then
                           earlier_date := true
                        else
                           {change if start is missing, and this is missing, and is in an earlier month.}
                           if (country_disputes.get_stday(current_start_rec) = -9) and (country_disputes.get_stday(arec) = -9) and
                                ( (country_disputes.get_styear(arec)<country_disputes.get_styear(current_start_rec)) or
                                  ( (country_disputes.get_styear(arec)=country_disputes.get_styear(current_start_rec)) and (country_disputes.get_stmonth(arec)<country_disputes.get_stmonth(current_start_rec)) ) ) then
                           earlier_date := true
                        else
                           {change if start is not missing, and this is not missing, and it is earlier.}
                           if (country_disputes.get_stday(current_start_rec) <> -9) and (country_disputes.get_stday(arec) <> -9) and
                              (EncodeDate(country_disputes.get_styear(arec), country_disputes.get_stmonth(arec), country_disputes.get_stday(arec))
                               < EncodeDate(country_disputes.get_styear(current_start_rec), country_disputes.get_stmonth(current_start_rec), country_disputes.get_stday(current_start_rec))) then
                           earlier_date := true
                        else
                           {change if start is not missing, and this is missing, and is in an earlier month.}
                           if (country_disputes.get_stday(current_start_rec) <> -9) and (country_disputes.get_stday(arec) = -9) and
                                ( (country_disputes.get_styear(arec)<country_disputes.get_styear(current_start_rec)) or
                                  ( (country_disputes.get_styear(arec)=country_disputes.get_styear(current_start_rec)) and (country_disputes.get_stmonth(arec)<country_disputes.get_stmonth(current_start_rec)) ) ) then
                           earlier_date := true;

                        if earlier_date = true then
                           begin
                              if country_disputes.get_stday(arec) = -9 then
                                 tempday := 15
                                 else tempday := country_disputes.get_stday(arec);
                              overall_Start_date := EncodeDate(country_disputes.get_styear(arec),
                                          country_disputes.get_stmonth(arec), tempday);
                              current_start_rec := arec;
                           end;
                     end;


                  {Now set end date}
                  {Do this by first, Take end date of first country dispute.Self
                   Then, if next country rec is a more correct start date, then set that.
                     It is more correct if:
                        end is missing, and this is not missing, and is in the same month or later.
                        end is missing, and this is missing, and is in an later month.
                        end is not missing, and this is not missing, and it is later.
                        end is not missing, and this is missing, and is in an later month.   }
                  if country_disputes.get_endday(first_of_dispute) = -9 then
                     tempday := 15
                     else tempday := country_disputes.get_endday(first_of_dispute);
                  overall_end_date := EncodeDate(country_disputes.get_endyear(first_of_dispute),
                                        country_disputes.get_endmonth(first_of_dispute),
                                        tempday);
                  current_end_rec := first_of_dispute;

                  for arec := first_of_dispute+1 to last_of_dispute do
                     begin
                        later_date := false;
                        {Now see if we want to change the overall end date record because this record is later.}
                        {change if: end is missing, and this is not missing, and is in the same month or later.}
                        if (country_disputes.get_endday(current_end_rec) = -9) and (country_disputes.get_endday(arec) <> -9) and
                             ( (country_disputes.get_endyear(arec)>country_disputes.get_endyear(current_end_rec)) or
                               ( (country_disputes.get_endyear(arec)=country_disputes.get_endyear(current_end_rec)) and (country_disputes.get_endmonth(arec)>=country_disputes.get_endmonth(current_end_rec)) ) ) then
                           later_date := true
                        else
                           {end is missing, and this is missing, and is in an later month.}
                           if (country_disputes.get_endday(current_end_rec) = -9) and (country_disputes.get_endday(arec) = -9) and
                                ( (country_disputes.get_endyear(arec)>country_disputes.get_endyear(current_end_rec)) or
                                  ( (country_disputes.get_endyear(arec)=country_disputes.get_endyear(current_end_rec)) and (country_disputes.get_endmonth(arec)>country_disputes.get_endmonth(current_end_rec)) ) ) then
                           later_date := true
                        else
                           {change if end is not missing, and this is not missing, and it is later.}
                           if (country_disputes.get_endday(current_end_rec) <> -9) and (country_disputes.get_endday(arec) <> -9) and
                              (EncodeDate(country_disputes.get_endyear(arec), country_disputes.get_endmonth(arec), country_disputes.get_endday(arec))
                               > EncodeDate(country_disputes.get_endyear(current_end_rec), country_disputes.get_endmonth(current_end_rec), country_disputes.get_endday(current_end_rec))) then
                           later_date := true
                        else
                           {change if end is not missing, and this is missing, and is in an later month. }
                           if (country_disputes.get_endday(current_end_rec) <> -9) and (country_disputes.get_endday(arec) = -9) and
                                ( (country_disputes.get_endyear(arec)>country_disputes.get_endyear(current_end_rec)) or
                                  ( (country_disputes.get_endyear(arec)=country_disputes.get_endyear(current_end_rec)) and (country_disputes.get_endmonth(arec)>country_disputes.get_endmonth(current_end_rec)) ) ) then
                           later_date := true;

                        if later_date = true then
                           begin
                              if country_disputes.get_endday(arec) = -9 then
                                 tempday := 15
                                 else tempday := country_disputes.get_endday(arec);
                              overall_end_date := EncodeDate(country_disputes.get_endyear(arec),
                                          country_disputes.get_endmonth(arec), tempday);
                              current_end_rec := arec;
                           end;
                     end;

                  {need to check to be sure we didn't mess up start and end dates via using -9 --> 15}
                  if overall_Start_date > overall_end_date then
                     begin   {there was a date problem;  check and fix one date.}
                        if country_disputes.get_stday(current_start_rec) = -9 then {changing the start date messed it up}
                           overall_Start_date := overall_end_date
                        else
                           if country_disputes.get_endday(current_end_rec) = -9 then {changing the end date messed it up}
                              overall_end_date := overall_Start_date
                        else
                           EUGeneError ('Found an initial mid start date > end date in proc mid over time init, but neigher had a -9 date.  Problem, notify programmer.',1, stop, error_log);
                     end;

                  {Add a start and end record from thsi country dispute.  But make sure that -9-> -15 won't mess up.}
                  for arec := first_of_dispute to last_of_dispute do
                     begin
                        if country_disputes.get_stday(arec) = -9 then
                           tempstartday := 15
                           else tempstartday := country_disputes.get_stday(arec);
                        if country_disputes.get_endday(arec) = -9 then
                           tempendday := 15
                           else tempendday := country_disputes.get_endday(arec);
                        if encodedate (country_disputes.get_styear(arec),country_disputes.get_stmonth(arec),tempstartday) > encodedate (country_disputes.get_endyear(arec),country_disputes.get_endmonth(arec),tempendday) then
                              {start date seems to be > end date, something messed up with -9s}
                           begin
                              if country_disputes.get_stday(arec) = -9 then  {need to set start to end day}
                                 tempstartday := country_disputes.get_endday(arec);
                              if country_disputes.get_endday(arec) = -9 then
                                 tempendday := country_disputes.get_stday(arec)
                              else EUGeneError ('Programming error, start>end before add_country_rec, but -9s not seen',0,continue, error_log);
                           end;
                        if encodedate (country_disputes.get_styear(arec),country_disputes.get_stmonth(arec),tempstartday) < overall_start_date then
                           tempstartday := country_disputes.get_stday(current_start_rec);
                        if encodedate (country_disputes.get_endyear(arec),country_disputes.get_endmonth(arec),tempendday) > overall_end_date then
                           tempendday := country_disputes.get_endday(current_end_rec);

                        {One total kludge fix for an odd MID case}
                        if (country_disputes.get_disputeid(arec)=4046) and (country_disputes.get_ccode(arec)=343) then
                           tempstartday := 5;

                        {Now can add the records, with modified start, end days if needed.}
                        add_country_rec (actor_time_array, country_disputes.get_ccode(arec), se_start, country_disputes.get_styear(arec),country_disputes.get_stmonth(arec),tempstartday,country_disputes.get_sideA(arec), country_disputes.get_disputeid(arec), country_disputes.get_hostlev(arec), overall_start_date, overall_end_date);
                        add_country_rec (actor_time_array, country_disputes.get_ccode(arec), se_end, country_disputes.get_endyear(arec),country_disputes.get_endmonth(arec),tempendday,country_disputes.get_sideA(arec), country_disputes.get_disputeid(arec), country_disputes.get_hostlev(arec), overall_start_date, overall_end_date);
                     end;

                  {sort actor records}
                  for arec := 0 to high(actor_time_array) - 1 do
                     for arec2 := arec + 1 to high(actor_time_array) do
                        if (actor_time_array[arec].date > actor_time_array[arec2].date) or
                           ((actor_time_array[arec].date = actor_time_array[arec2].date) and
                            ((actor_time_array[arec].stend = se_end) and
                             (actor_time_array[arec2].stend = se_start) ) ) then
                        begin   {switch to be in order by date, then with start records first.}
                           tempactorrec := actor_time_array[arec];
                           actor_time_array[arec] := actor_time_array[arec2];
                           actor_time_array[arec2] := tempactorrec;
                        end;

                  {Now have individual actor recs for this MID.  Turn into final set.}
                  {first, create the first interval record for this MID.  Might be more, but will
                   be at least one.}
                  setlength (data, length(data) + 1);
                  current_data_record := high(data);
                  data[current_data_record].idnum := actor_time_array[0].idnum;
                  data[current_data_record].interval_start := actor_time_array[0].date;
                  data[current_data_record].interval_end := encodedate(max_year,12,31);
                  setlength (data[current_data_record].actorsA, 0);
                  setlength (data[current_data_record].actorsB, 0);
                  add_actor_to_set (current_data_record, actor_time_array[0].side, actor_time_array[0].ccode, actor_time_array[0].hostlev);
                  current_date := data[current_data_record].interval_start;
                  processing_end := false;

                  {The above set up the initial rec.  Now go through the rest and change that or add more.}
                  for arec := 1 to high(actor_time_array) do
                     begin

                        {First, before processing start/end, if this crossed a year boundary,
                         set up year break.}
                        if yearof(actor_time_array[arec].date) > yearof(current_date) then
                           begin
                              for x := yearof(current_date) to yearof(actor_time_array[arec].date)-1 do
                                 begin
                                    {New record will look just like old rec, except starting in next year.}
                                    setlength (data, length(data) + 1);
                                    {start new record in year x + 1; all other values get copied.}
                                    data[high(data)].interval_start := encodedate (x+1,1,1);
                                    data[high(data)].idnum := data[current_data_record].idnum;
                                    data[high(data)].interval_end := data[current_data_record].interval_end;
                                    setlength (data[high(data)].actorsA, length(data[current_data_record].actorsA));
                                    data[high(data)].actorsA := copy(data[current_data_record].actorsA);
                                    setlength (data[high(data)].actorsB, length(data[current_data_record].actorsB));
                                    data[high(data)].actorsB := copy(data[current_data_record].actorsB);
                                    {now, after copying values, reset previous record in year x to end 12/31/x}
                                    data[current_data_record].interval_end := encodedate(x,12,31);
                                    inc(current_data_record);
                                    current_date := data[current_data_record].interval_start;
                                 end;
                           end;

                        case actor_time_array[arec].StEnd of
                           se_start : begin
                              if actor_time_array[arec].date > current_date then
                                 {New start record, new day.
                                  Need to end prior record, add a new rec.}
                                 begin
if high(data) <> current_data_record then
   EUGeneError ('error in curr data rec in end in '+inttostr(current_data_record),1,continue,error_log);
                                    data[current_data_record].interval_end := incday(actor_time_array[arec].date, -1);
                                    {New record will look a lot like old rec, plus 1 state}
                                    setlength (data, length(data) + 1);
                                    inc(current_data_record);
                                    data[current_data_record].idnum := actor_time_array[arec].idnum;
                                    data[current_data_record].interval_start := actor_time_array[arec].date;
                                    data[current_data_record].interval_end := encodedate(max_year,12,31);
                                    setlength (data[current_data_record].actorsA, length(data[current_data_record-1].actorsA));
                                    data[current_data_record].actorsA := copy(data[current_data_record-1].actorsA);
                                    setlength (data[current_data_record].actorsB, length(data[current_data_record-1].actorsB));
                                    data[current_data_record].actorsB := copy(data[current_data_record-1].actorsB);
                                    {now add new state}
                                    add_actor_to_set (current_data_record, actor_time_array[arec].side,
                                                      actor_time_array[arec].ccode, actor_time_array[arec].hostlev);
                                    current_date := data[current_data_record].interval_start;
                                 end
                              else if actor_time_array[arec].date = current_date then
                                 begin  {if date is same as the current interval start, just add the new state}
                                    add_actor_to_set (current_data_record, actor_time_array[arec].side,
                                                      actor_time_array[arec].ccode, actor_time_array[arec].hostlev);
                                 end
                              else
                                 begin
                                    EUGeneError ('Got to a next date less than current date in init mids over time.  Bad. ',1, stop, error_log);
                                 end;
                              processing_end := false;
                              end;  {se_start}

                           se_end : begin   {this could be very last record, or not}
                                 {if a country ends on a new date, I always need to end the prior record.}
                              if (actor_time_array[arec].date > current_date) or
                                 ((actor_time_array[arec].date = current_date) and not (processing_end)) then
                                 begin
if high(data) <> current_data_record then
   EUGeneError ('error in curr data rec in end in '+inttostr(current_data_record),1,continue,error_log);
                                    data[current_data_record].interval_end := actor_time_array[arec].date;
                                    {subtract actor from next rec...}
                                    setlength (data, length(data) + 1);
                                    inc(current_data_record);
                                    data[current_data_record].idnum := actor_time_array[arec].idnum;
                                    data[current_data_record].interval_start := incday(actor_time_array[arec].date, 1);
                                    data[current_data_record].interval_end := encodedate(max_year,12,31);
                                    setlength (data[current_data_record].actorsA, length(data[current_data_record-1].actorsA));
                                    data[current_data_record].actorsA := copy(data[current_data_record-1].actorsA);
                                    setlength (data[current_data_record].actorsB, length(data[current_data_record-1].actorsB));
                                    data[current_data_record].actorsB := copy(data[current_data_record-1].actorsB);
                                    {now subtract the state that ended state}
                                    remove_actor_from_set (current_data_record, actor_time_array[arec].side, actor_time_array[arec].ccode);
                                    current_date := data[current_data_record].interval_start;
                                 end
                              else if (processing_end) then
                                 begin
                                    {If a country is the first to end, whether a
                                     new date, or same date, we need to end the prior record.
                                     But if it's the 2nd to end, then it just drops out of the next record, and no
                                     new record is needed.  But if it ends even right after
                                     someone else started, it needs to end the record before
                                     dropping out.  }
                                    remove_actor_from_set (current_data_record, actor_time_array[arec].side, actor_time_array[arec].ccode);
                                 end
                              else
                                 EUGeneError ('saw a condition on processing se_end in mid over time init that did not match.  Programming error.',1,continue,error_log);
                              processing_end := true;
                              if (length (data[current_data_record].actorsA)=0) and (length(data[current_data_record].actorsB)=0)  then
                                 {MID is down to 0, that should have been last rec.}
                                 begin
                                    setlength(data, length(data) - 1);
                                    if not (arec=(length(actor_time_array) - 1) ) then
                                       EUGeneError ('programming/logic error - reached MID with no actors in remove actors in .init for mids over time, but was not last state in actor-time loop.  Current data record = '+inttostr(current_data_record)+'; country dispute is '+inttostr(country_dispute),1,continue,error_log);
                                 end;
                              end;      { > current_date}
                           end;   {case se_end}

                     end;

             trace.message('processed country dispute '+inttostr(country_dispute));
                  {Now set up for processing of next country dispute}
                  {At the last record, this increment will put country_dispute past num_country_disputes,
                   which will trigger the exit from the loop}
                  country_dispute := last_of_dispute + 1;

               end;    {while country_dispute <= num country disputes}

               time_trace.tickdone;

         finally
            created := true;
            time_trace.free;
         end;
      end;      {proc  TCOWMID_Over_Time_obj.init}

   { ---------------------------------------------------------- }


end.
