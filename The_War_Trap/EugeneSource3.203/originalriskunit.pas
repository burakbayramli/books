unit OriginalRiskUnit;

interface

uses dialogs, sysutils, fileerror, eutypes1, eutypes2, euinoutd;

type

   risk_rec = record
         risk, security, secmaxsum, secminsum : single;
      end;
   risk_record_type = array[region_range] of risk_rec;   {risk based on regional groups of states}
       {for risk, 2 similar types.  Flat array is for outputting data to file.
        Array of Pointer type is for keeping data in memory on a Win16 system (flat too big)}
   risk_ccode_array_type = array[ccode_index_range] of risk_record_type;
   risk_record_ptr = ^risk_record_type;
   risk_ccode_array_ptr_type = array[ccode_index_range] of risk_record_ptr;
   risk_ccode_array = risk_ccode_array_ptr_type;
   risk_ccode_array_ptr = ^risk_ccode_array;
   risk_year_array = array[year_range] of risk_ccode_array_ptr;
   risk_year_array_ptr = ^risk_year_array;
   Trisk_attitude_array_obj = class (Tobject)
      constructor init (eugene_risk_file_name, WTR_risk_file_name : TFileName; year1, year2 : year_range;
                        raw_data_source : risk_in_data_type);
         {init reads in from intermediate, external file.}
      destructor destroy; override;
      function get_first_partition_year : year_range;
      function get_last_partition_year : year_range;
      function initialized : boolean;
      function get_risk (ccode : ccode_range; year : year_range; aregion : region_range) : single;
      function get_security (ccode : ccode_range; year : year_range; aregion : region_range) : single;
      function get_secmax (ccode : ccode_range; year : year_range; aregion : region_range) : single;
      function get_secmin (ccode : ccode_range; year : year_range; aregion : region_range) : single;
      function get_uncertainty (year : year_range; aregion : region_range) : single;
      function get_ccode_array (year : year_range): risk_ccode_array_type;
     private
      data : risk_year_array_ptr;
      first_partition_year, last_partition_year : year_range;
      created : boolean;
      function all_in_range (ccode : ccode_range; ayear : year_range): boolean;
   end;    {risk object}

   risk_file_record_type = packed record
          year : year_range;
          ccode_array : risk_ccode_array_type;
       end;
   risk_file_type = file of risk_file_record_type;



implementation

   uses TraceUnit, errbx;
   
   constructor Trisk_attitude_array_obj.init (eugene_risk_file_name, WTR_risk_file_name : TFileName;
                    year1, year2 : year_range; raw_data_source : risk_in_data_type);
         {init reads in from intermediate, external file.  Either Bdm source or Eugene source.}
      var risk_file : risk_file_type;
          risk_wtr_file : text;
          risk_ccode : risk_ccode_array_ptr;   {array[ccode_index_range] of ptr to rec; }
          risk_ptr_rec : risk_record_ptr;    {ptr to risk_rec_type}
          year_loop : year_range;
          risk_file_rec : risk_file_record_type;
          temp : year_range;
          start_mem, heapneeded : longint;
          region : region_range;
          x : ccode_index_range;
          left, right : longint;
          temp_record : risk_file_record_type;
          left_year, right_year : year_range;
          prevyear, num_read : longint;
          want_year : year_range;
          Risk_trace : TTrace_obj;

                                { --------------------------------- }

         Procedure read_wtr_risk;
               {this is not as sophisticated as other, b/c I expect to not have to use it much}
            var year : year_range;
                ccode : ccode_range;
                year_real, ccode_real, risk_eur, risk_Mid, RiskAsi, RiskAme : single;
            begin
               try
                  try
                     trace.message ('Reading WTR Risk data from external file');
                     assignFile (risk_wtr_file, WTR_risk_file_name);
                     reset (risk_wtr_file);
                     risk_file_rec.year := min_year;

                     {first, find location in file I want}
                     {find the beginning section of what needs to be read and processed}
                     {Just loop through until find proper record}
                     repeat
                        readln (risk_wtr_file, year_real, ccode_real, risk_eur, risk_Mid, RiskAsi, RiskAme);
                        Risk_trace.tick ('Executing Procedure: Searching Risk File ',0);
                        year := trunc(year_real);
                     until (year >= first_partition_year) or eof (risk_wtr_file);
                     Risk_trace.tickdone;

                     {Now save this record}
                     ccode := trunc(ccode_real);
                     if (nation_list.is_a_state (ccode, year)) and (year >= first_partition_year) and
                        (year <= last_partition_year) then
                        begin
                           data^[year]^[ccode_index.index(ccode)]^[europe].risk := risk_eur;
                           data^[year]^[ccode_index.index(ccode)]^[middleEast].risk := risk_Mid;
                           data^[year]^[ccode_index.index(ccode)]^[asia].risk := RiskAsi;
                           data^[year]^[ccode_index.index(ccode)]^[americas].risk := RiskAme;
                        end;

                     {Now repeat reading and saving records until eof}
                     num_read := 0;
                     prevyear := 0;
                     while (not eof (risk_wtr_file)) and (year <= last_partition_year) do
                        begin
                           readln (risk_wtr_file, year_real, ccode_real, risk_eur, risk_Mid, RiskAsi, RiskAme);
                           year := trunc(year_real);
                           ccode := trunc(ccode_real);
                           if (nation_list.is_a_state (ccode, year)) and (year >= first_partition_year) and
                              (year <= last_partition_year) then
                              begin
                                 data^[year]^[ccode_index.index(ccode)]^[europe].risk := risk_eur;
                                 data^[year]^[ccode_index.index(ccode)]^[middleEast].risk := risk_Mid;
                                 data^[year]^[ccode_index.index(ccode)]^[asia].risk := RiskAsi;
                                 data^[year]^[ccode_index.index(ccode)]^[americas].risk := RiskAme;
                              end;
                           Risk_trace.tick ( 'Executing Procedure: Read Risk Data '+inttostr(self.first_partition_year)+
                               ' to '+inttostr(self.last_partition_year),
                               (self.last_partition_year - self.first_partition_year+1));
                           if year <> prevyear then
                              begin
                                 prevyear := year;
                                 inc(num_read);
                              end;
                        end;
                     Risk_trace.tickdone;

                  finally
                     close (risk_wtr_file);
                  end;
               except
               on EUserInterrupt do raise;
               on EInOutError do
                 begin
                    FileErrorBox.maindo ('Error opening file "'+wtr_risk_file_name+ '"',
                                         'File could not be opened for input.',
                                         'File may be in use by another program, or may be missing.');
                    FileErrorBox.showmodal;
                    raise;
                 end;
               end;
            end;     {Proc read WTR risk;}

                                { --------------------------------- }

         Procedure read_eugene_risk;
            var x : integer;
            begin
               try
                  try
                     trace.message ('Reading EUGENE Risk data from external file');
                     assignFile (risk_file, eugene_risk_file_name);
                     reset (risk_file);
                     risk_file_rec.year := min_year;

                     {first, find location in file I want}
                     {find the beginning section of what needs to be read and processed}
                     {binary search for any value of year < first.  Then go on to sequential.}
                     read (risk_file, risk_file_rec);
                     if risk_file_rec.year < first_partition_year then  {not at first record, so search}
                       begin
                          want_year := first_partition_year - 1;  {need to find some record that
                              is at most one year before what I want here.  }
                          {file starts at position 0}
                          left := 0;
                              {find the leftmost and rightmost years}
                          right := FileSize (risk_file)-1;
                          seek (risk_file, left);
                          read (risk_file, temp_record);
                          left_year := temp_record.year;
                          seek (risk_file, right);
                          read (risk_file, temp_record);
                          right_year := temp_record.year;

                          repeat
                                {left and right are file positions.  Guess how far between l and r
                                 the desired year will fall}
                                 {pure binary search would be
                                  seek (tau_file, ((left + right) div 2));  }
                                 {Improve on that by moving closer to where year should be.}
                             seek (risk_file, trunc(left+(right-left) * ((want_year-left_year) /
                                                                (right_year-left_year+1)) ));
                             Risk_trace.tick ( 'Executing Procedure: Searching Risk File ',0);
                             read (risk_file, risk_file_rec);
                             if risk_file_rec.year < want_year then    {want to search in right half of file}
                                begin
                                   left := FilePos(risk_file);
                                   temp_record := risk_file_rec;
                                   left_year := temp_record.year;
                                end
                             else     {value read was correct, or was more than what I want,
                                       so need to search left half of file}
                                begin
                                   right := FilePos(risk_file);
                                   temp_record := risk_file_rec;
                                   right_year := temp_record.year;
                                end;
                          until (risk_file_rec.year = want_year) or (left > right);
                          {when exit here, file is positioned to year just before where I want it}
                       end;
                     Risk_trace.tickdone;

                          {Now am one year before; get to first record of year I want}
                     while (risk_file_rec.year < first_partition_year) and (not eof(risk_file)) do
                        begin
                           read (risk_file, risk_file_rec);
                           Risk_trace.tick ( 'Executing Procedure: Searching Risk File ',0);
                        end;
                     Risk_trace.tickdone;

                     {have one record in memory.  Possibly store it before continuing read}
                     if (risk_file_rec.year >= self.first_partition_year) and
                        (risk_file_rec.year <= self.last_partition_year) then
                        begin
                           for x := min_ccode_index to max_ccode_index do
                              data^[risk_file_rec.year]^[x]^ := risk_file_rec.ccode_array[x];
                        end;

                     num_read := 0;
                     prevyear := 0;
                     while (not eof (risk_file)) and (risk_file_rec.year <= self.last_partition_year) do
                        begin
                           Risk_trace.tick ( 'Executing Procedure: Read Risk Data, '+inttostr(self.first_partition_year)+
                               ' to '+inttostr(self.last_partition_year),
                               (self.last_partition_year - self.first_partition_year+1));
                           read (risk_file, risk_file_rec);
                           if (risk_file_rec.year >= self.first_partition_year) and
                              (risk_file_rec.year <= self.last_partition_year) then
                              begin
                                 for x := min_ccode_index to max_ccode_index do
                                    data^[risk_file_rec.year]^[x]^ := risk_file_rec.ccode_array[x];
                              end;
                           if risk_file_rec.year <> prevyear then
                              begin
                                 prevyear := risk_file_rec.year;
                                 inc(num_read);
                              end;
                        end;   {while not eof}

                  finally
                     CloseFile (risk_file);
                  end;
               except
                  on EUserInterrupt do raise;
                  on EInOutError do
                    begin
                       FileErrorBox.maindo ('Error opening file "'+eugene_risk_file_name+ '"',
                                            'File could not be opened for input.',
                                            'File may be in use by another program, or may be missing.');
                       FileErrorBox.showmodal;
                       raise;
                    end;
               end;
            end;     {Proc read eugene risk;}

                                { --------------------------------- }

      begin          {main init constructor}
         try
            start_mem := memavail;
            if year1 > year2 then switch_year(year1, year2);
            trace.enter('Initializing risk data, '+inttostr(year1)+' to '+inttostr(year2));
            if not(ccode_index.initialized) then
               begin
                  EUGeneError ('Risk array creation called before ccode_index initialized',
                                  5, stop, error_log);
               end;
            if not(nation_list.initialized) then
               begin
                  EUGeneError ('Risk array creation called before nation_list initialized',
                                  5, stop, error_log);
               end;

            self.first_partition_year := year1;
            self.last_partition_year := year2;
            Risk_trace := nil;
            Risk_trace := TTrace_obj.init(trace.get_trace_level);

            {intialize data}
            trace.message ('Initializing Risk arrays');
            heapneeded := TRisk_Attitude_Array_obj_mem_overhead +
                          ((last_partition_year - first_partition_year + 1) *
                            TRisk_attitude_array_obj_mem_per_year );
            if debug[4] then
               begin
                  trace.message ('Risk array size calculation');
                  trace.message ('Risk one year size = '+inttostr(TRisk_attitude_array_obj_mem_per_year));
                  trace.message ('Calc is that '+inttostr(heapneeded)+' needed for all years.');
                  trace.message ('Max avail mem block is '+inttostr(MaxAvail));
               end;
            if MaxAvail <= (heapneeded) then
               begin
                  EUGeneError ('Not enough memory for risk array. ',
                                  5, stop, error_log);
               end;
            new (data);
            for year_loop := min_year to max_year do
               begin
                  if (year_loop >= self.first_partition_year) and (year_loop <= self.last_partition_year) then
                  begin
                     new(risk_ccode);
                     data^[year_loop] := risk_ccode;
                        {in initialization, don't check for if is a state, just
                         initialize to start value}
                     for x := min_ccode_index to max_ccode_index do
                        begin
                           new (risk_ptr_rec);
                           data^[year_loop]^[x] := risk_ptr_rec;
                           for region := europe to globe do
                              begin
                                 data^[year_loop]^[x]^[region].risk := missing_value;
                                 data^[year_loop]^[x]^[region].security := missing_value;
                                 data^[year_loop]^[x]^[region].secmaxsum := missing_value;
                                 data^[year_loop]^[x]^[region].secminsum := missing_value;
                              end;
                        end;
                  end  {if year in range}
                  else data^[year_loop] := nil;
               end;    {for year_loop}

            case raw_data_source of
               risk_WTR : read_wtr_risk;
               risk_EUGENE : read_eugene_risk;
            end;   {case}

            Risk_Trace.tickdone;
            created := true;

         finally
            Risk_Trace.free;
            trace.message ('Risk array required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
            trace.exit ('Finished initializing risk data');
         end;
      end;

      {  -------------------------------------------------   }

   destructor Trisk_attitude_array_obj.destroy;
      var year_loop : year_range;
          x : ccode_index_range;
      begin
         try
            if self <> nil then
            begin
               for year_loop := min_year to max_year do
                 if data^[year_loop] <> nil then
                    begin
                      for x := min_ccode_index to max_ccode_index do
                         begin
                            dispose (data^[year_loop]^[x]);
                            data^[year_loop]^[x] := nil;
                         end;
                       dispose (data^[year_loop]);
                       data^[year_loop] := nil;
                    end;
              if data <> nil then dispose (data);
              created := false;
              inherited destroy;
            end;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;

      {  -------------------------------------------------   }

   function Trisk_attitude_array_obj.get_first_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Risk get called before initialization',
                               5, stop, error_log);
            end
         else
            get_first_partition_year := first_partition_year;
      end;

      {  -------------------------------------------------   }

   function Trisk_attitude_array_obj.get_last_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Risk get called before initialization',
                               5, stop, error_log);
            end
         else
            get_last_partition_year := last_partition_year;
      end;

      {  -------------------------------------------------   }

   function Trisk_attitude_array_obj.initialized : boolean;
      begin
         if self=nil then
               EUGeneError ('Risk get called before initialization',
                               5, stop, error_log)
         else if not(created) then
               EUGeneError ('Risk get called before initialization',
                               5, stop, error_log)
         else if created then initialized := true
         else initialized := false;
      end;

      {  -------------------------------------------------   }

   function Trisk_attitude_array_obj.all_in_range (ccode : ccode_range; ayear : year_range): boolean;
      begin
         all_in_range := true;
         if not(initialized) then
            begin
               EUGeneError ('Get a value from Risk array called before initialization',
                               5, stop, error_log);
               trace.message ('value  set to missing');
               all_in_range := false;
            end
         else
         if not ((ayear >= get_first_partition_year) and (ayear <= get_last_partition_year)) then
            begin
               EUGeneError ('Internal Error in program - called Get a value from Risk array with year outside partition',
                               5, continue, error_log);
               trace.message ('value  set to missing');
               all_in_range := false;
            end
         else
            if not (nation_list.is_a_state(ccode, ayear)) then
               begin
                  trace.message ('ccode/year Error - called Get a value from Risk array for invalid ccode given year'+inttostr(ccode)+' in '+inttostr(ayear));
                  trace.message ('value set to missing');
                  all_in_range := false;
               end;
      end;

      {  -------------------------------------------------   }

   function Trisk_attitude_array_obj.get_risk (ccode : ccode_range; year : year_range;
                                               aregion : region_range) : single;
      begin
         get_risk := missing_value;
         if all_in_range (ccode, year) then
            get_risk := data^[year]^[ccode_index.index(ccode)]^[aregion].risk;
      end;

      {  -------------------------------------------------   }

      function Trisk_attitude_array_obj.get_security (ccode : ccode_range; year : year_range; aregion : region_range) : single;
      begin
         get_security := missing_value;
         if all_in_range (ccode, year) then
            get_security := data^[year]^[ccode_index.index(ccode)]^[aregion].security;
      end;

      {  -------------------------------------------------   }

      function Trisk_attitude_array_obj.get_secmax (ccode : ccode_range; year : year_range; aregion : region_range) : single;
      begin
         get_secmax := missing_value;
         if all_in_range (ccode, year) then
            get_secmax := data^[year]^[ccode_index.index(ccode)]^[aregion].secmaxsum;
      end;

      {  -------------------------------------------------   }

      function Trisk_attitude_array_obj.get_secmin (ccode : ccode_range; year : year_range; aregion : region_range) : single;
      begin
         get_secmin := missing_value;
         if all_in_range (ccode, year) then
            get_secmin := data^[year]^[ccode_index.index(ccode)]^[aregion].secminsum;
      end;

      {  ---------------------------------------------------------   }

      function Trisk_attitude_array_obj.get_uncertainty (year : year_range; aregion : region_range) : single;
         {uncertainty is the variance in risk scores in a given year.
          Note:  These risk scores should apparently be the UNTRANSFORMED risk scores.  }
      var ccode : ccode_range;
          dataArray : array[1..max_countries] of double;
          num_dat, x : integer;
          variance, sum, sumsq, mean : double;
          uppercase_risk : single;
      begin
         get_uncertainty := missing_value;
         num_dat := 0;
         sum := 0;
         for ccode := min_ccode to max_ccode do
            if nation_list.is_a_state (ccode, year) then
               if nation_list.is_involved_in_region (ccode, aregion, year) then
                  begin
                     uppercase_risk := get_risk (ccode, year, aregion);
                     if not (uppercase_risk = missing_value) then
                       begin
                         inc(num_dat);
                         dataArray[num_dat] := uppercase_risk;
                         sum := sum + dataArray[num_dat];
                       end;
                  end;
         if num_dat > 0 then
           begin
               {Use Schmidt 1979:105 formula ]
               {first, mean}{2nd sum squares}
               mean := sum / num_dat;
               sumsq := 0;
               for x := 1 to num_dat do
                  sumsq := sumsq + (dataArray[x]-mean) * (dataArray[x]-mean);
               variance := sumsq / num_dat;
               get_uncertainty := variance;
           end;
      end;

      {  ---------------------------------------------------------   }

function Trisk_attitude_array_obj.get_ccode_array(year : year_range): risk_ccode_array_type;
   var temp_array : risk_ccode_array_type;
       temp_ccode_index : ccode_index_range;
   begin
      for temp_ccode_index := min_ccode_index to max_ccode_index do
         temp_array[temp_ccode_index] := data^[year]^[temp_ccode_index]^;
      get_ccode_array := temp_array;
   end;

      {  ---------------------------------------------------------   }

end.
 