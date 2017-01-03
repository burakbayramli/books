unit euprocs1;

{EUGene  Copyright 1997, 1998, 1999, 2000  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

     {This unit takes as input various input data, and
      performs various calculations to that data.
      1) raw alliance data to dyadic and tau-b form.
      2) raw COW capabilities to %system capabilities
      3) calculation of risk attitude scores
      4) calculate EU values for dyads.  }

{ ------------------------------------------------------------ }

interface
   uses sysutils, math, dialogs, Forms, Controls, Classes, Windows,
           TraceUnit, cmnprocD, eutypes1, eutypes2, euinoutD, FileError, SingleRiskYear;

   function adj_cap (ccode1, ccode2 : ccode_range; year : year_range; const sys_capability_data :
                      Tsys_capability_array_obj; const distance_data: Tdistance_array_obj): single;

   function tau_from_alliance_year (ccode1, ccode2 : ccode_range; ayear : year_range;
            const alliance_data_year : Talliance_year_obj; region : region_type): real;

   procedure compute_and_save_taus (const sequence_alliance_file_name, seq_file_name,
             dyadic_alliance_file_name, tau_file_name : TFileName;
             const nation_list : Tnation_array_obj; first_proc_year,
             last_proc_year : year_range; raw_data_source : alliance_in_data_type);

   procedure compute_and_save_s (const sequence_alliance_file_name, seq_file_name,
             dyadic_alliance_file_name, s_file_name, sys_cap_file_name : TFileName;
             const nation_list : Tnation_array_obj; first_proc_year,
             last_proc_year : year_range; raw_data_source : alliance_in_data_type);

   procedure compute_and_output_s_from_arbitrary_countries_and_years (const sequence_alliance_file_name, seq_file_name,
             dyadic_alliance_file_name, sys_cap_file_name : TFileName;
             const nation_list : Tnation_array_obj; first_proc_year,
             last_proc_year : year_range; raw_data_source : alliance_in_data_type; randyseparator : separatortype;
             s_output_text_file_name, s_input_text_file_name : TFileName);

   procedure compute_and_save_sys_capability (const raw_cap_file_name,
             modified_cap_file_name, sys_cap_file_name: TFileName;
             const nation_list : Tnation_array_obj; const ccode_index : Tccode_index_obj;
             first_proc_year, last_proc_year: year_range);

   procedure compute_and_save_EUWarTrap (sys_cap_file_name, tau_file_name, s_file_name,
          EUWarTrap_Tau_file_name, EUWarTrap_S_file_name, locations_file_name : TFileName;
          const nation_list : Tnation_array_obj; const contiguity_data : Tcontiguity_array_obj;
          first_proc_year, last_proc_year : year_range);

   procedure compute_and_save_risk (const sys_cap_file_name, sequence_alliance_file_name,
             seq_file_name, dyadic_alliance_file_name,
             security_alliance_file_name, risk_file_name_for_compute_output,
             locations_file_name : TFileName; const nation_list : Tnation_array_obj;
             const contiguity_data : Tcontiguity_array_obj;
             first_proc_year, last_proc_year : year_range;
             raw_data_source : alliance_in_data_type; const risk_specs : risk_calculation_info_type);

{ ------------------------------------------------------------ }

implementation

uses mdiframe, errbx;     {mdiframe used for save dialog box}

   function adj_cap (ccode1, ccode2 : ccode_range; year : year_range; const sys_capability_data :
                      Tsys_capability_array_obj; const distance_data: Tdistance_array_obj): single;
        {returns adjusted capabilities of ccode1 fighting at ccode2 in year.}
      function miles_per_day (year:year_range) : integer;
      begin
         if year < 1918 then miles_per_day := 250
         else if year <= 1945 then miles_per_day := 375
         else miles_per_day := 500;
      end;

      begin
         if sys_capability_data.get_syscap (ccode1, year) = missing_value then
            adj_cap := missing_value
         else
            begin
              if user_selections.distance_method = nodiscount then
                 adj_cap := sys_capability_data.get_syscap (ccode1, year)
              else
                 if (distance_data.get_distance (ccode1, ccode2, year) = missing_value) then
                       adj_cap := missing_value
                 else adj_cap := realpower(sys_capability_data.get_syscap (ccode1, year),
                       log10( (distance_data.get_distance (ccode1, ccode2, year) / miles_per_day (year))
                                       + (10 - e) ) );
            end;
      end;   {function}

         { --------------------------------------- }

   function tau_from_table (atable : tau_table_type; N, nrows, ncolumns : integer) : real;
       {N is the number of cases entered in the table}
       {nrows and ncolumns are the number of rows and columns in table, between 1 and 4.
        Tau will only work if table is 2x2 or better.}
      var row, column, start_row, sub_row, start_col, sub_col : 1..4;
          row_sum, col_sum : array[1..4] of longint;
          T1, T2, Nterm : double;
          Sp, Sn : longint;
          Tc, Tr, colsum, rowsum : longint;
          tau1, tau2 : real;
          denominator, denominator2 : real;
         {Formula for tau-b from Hays, 1981: 603-604.
          Tau = (Sp - Sn) / sqrt (F);
          F = ( ( (N*(N-1))/2)-T1 )  *  ( (N*(N-1))/2)-T2 ) )
          t1 = Sum over j of nj(nj-1) / 2, where nj is marginal total of column j.
          t2 = Sum over k of nk(nk-1) / 2, where nk is marginal total of column k.
          }
      begin
         {Start by calculating the frequency / count in each row and column}
       {N := 30;
       nrows:=4;
       ncolumns:=4;
       for row := 1 to 4 do for column := 1 to 4 do atable[row,column]:=0;
       atable [1,4] := 14;
       atable [4,1] := 10;
       atable [4,4] := 6;  }

       {Note this will normally come in with a correct count of nrows, ncols.  Now,
        though, those #s are not recomputed in risk procedure.  So if an error gets
        generated in this proc, check to see if it was called from there.  }
         if (nrows < 2) or (ncolumns < 2) then
               begin     {cannot calculate tau in this case.}
                  trace.message ('Error computing tau. Contingency table NOT 2x2 or greater.  TAU set to 0.');
                  tau_from_table := 0;
               end
         else
         begin
            for row := 1 to 4 do row_sum[row] := atable[row, 1] + atable[row,2] +
                                                 atable[row,3] + atable[row,4];
            for column := 1 to 4 do col_sum[column] := atable[1, column] + atable[2, column] +
                                                       atable[3, column] + atable[4, column];

            {Figure numerator.  Sp is number of positive/concordant pairs, and Sn is
             number of negative/discordant pairs.}
            Sp := 0;
            for start_row := 1 to 3 do
             for start_col := 1 to 3 do
              begin    {from each starting cell in the matrix, count # of pairs below and to right}
                   {Work top left to bottom right}
                 for sub_row := start_row + 1 to 4 do
                    for sub_col := start_col + 1 to 4 do
                       Sp := Sp + (atable[start_row, start_col] * atable[sub_row, sub_col]);
              end;

            Sn := 0;
            for start_row := 1 to 3 do
             for start_col := 4 downto 2 do
              begin    {from each starting cell in the matrix, count # of pairs below and to left}
                   {work from top right to bottom left}
                 for sub_row := start_row + 1 to 4 do
                    for sub_col := start_col - 1 downto 1 do
                       Sn := Sn + (atable[start_row, start_col] * atable[sub_row, sub_col]);
              end;

              {Now compute number of tied pairs based on the marginal values.}
            T1 := 0;
            for column := 1 to 4 do T1 := T1 + (col_sum[column] * (col_sum[column] - 1) );
            T1 := T1 / 2;

            T2 := 0;
            for row := 1 to 4 do T2 := T2 + (row_sum[row] * (row_sum[row] - 1) );
            T2 := T2 / 2;

            {For readability, compute n*n-1 / 2, and denominator separately.}
            Nterm := (N * (N-1)) / 2;
            Denominator := (sqrt ((Nterm - T1) * (Nterm - T2)));

            {Now can compute final tau-b}
         {
         writeln ('nrows is ',nrows);
         writeln ('ncolumns is ',ncolumns);
         writeln ('Sp is ',Sp : 10);
         writeln ('Sn is ',Sn : 10);
         writeln ('T1 is ',T1 : 10:4);
         writeln ('T2 is ',T2 : 10:4);
         writeln ('Nterm is ',Nterm : 10:4);
         writeln ('Numerator is ',(Sp-Sn) : 10);
         writeln ('Denominator is ',denominator: 10:4);
         }

            if (denominator = 0) then
               tau1 := 0
            else tau1 := (Sp - Sn) / denominator;

         {writeln ('Tau # 1 is ', tau1 : 10 : 4);}


         {2nd formula from Bohrstedt and Knoke.  Produces same tau results.}

         {The 2nd formula is commented out because it is identical to the first.  }

         {Ns - Nd / (sqrt(ns+nd+tr) * sqrt(ns+nd+tc))}
         {Ns, Nd are same as Sp, Sn.  Tr, Tc diff procedure}

         {Tc := 0;
         for start_col := 1 to 4 do
            begin
               for start_row := 1 to 3 do
                  begin    {from each starting cell in the matrix, count # of pairs below only}
         {            colsum := 0;
                     for sub_row := start_row + 1 to 4 do  {staying in this column, do sum}
         {               colsum := colsum + atable[sub_row, start_col];
                     Tc := Tc + colsum*atable[start_row, start_col];
                  end;
            end;

         Tr := 0;
         for start_row := 1 to 4 do
            begin
               for start_col := 1 to 3 do
                  begin    {from each starting cell in the matrix, count # of pairs to right only}
         {            rowsum := 0;
                     for sub_col := start_col + 1 to 4 do  {staying in this row, do sum}
         {               rowsum := rowsum + atable[start_row, sub_col];
                     Tr := Tr + rowsum*atable[start_row, start_col];
                  end;
            end;

         {Ns - Nd / (sqrt(ns+nd+tr) * sqrt(ns+nd+tc))}
         {Ns, Nd are same as Sp, Sn.  Tr, Tc diff procedure}

         {denominator2 := (sqrt (Sp + Sn + Tr) * sqrt (Sp + Sn + Tc));
         if (denominator2 = 0) and (N > 0) then
            tau2 := 0
         else tau2 := (Sp - Sn) / denominator2;

         writeln ('Tau # 2 is ', tau2 : 10 : 4);

         }


            tau_from_table := tau1;
         end;    {nrows, ncolumns >= 2}

      end;

         { --------------------------------------- }

   function tau_from_alliance_year (ccode1, ccode2 : ccode_range; ayear : year_range;
            const alliance_data_year : Talliance_year_obj; region : region_type): real;
             {given a year of alliance data, will compute tau for a specified dyad in that year.}
             {this is in a separate procedure and is not within the "compute and save-taus"
              procedure because when figuring risk, this procedure will need to be called
              as part of the computation of the best and worst alliance portfolios.}
         var
             N : num_countries_range;
             nrows, ncolumns : tau_row_column_count_type;
             table : tau_table_type;  {just an array 1..4, 1..4}
             row : 1..4;
         begin
            alliance_data_year.build_tau_table (ccode1, ccode2, ayear, table, N,
                                  nrows, ncolumns, region);
            {Now, compute tau from the table}
            if (nrows < 2) or (ncolumns < 2) then
               begin     {cannot calculate tau in this case; set to missing}
                  if debug[10] then EUGeneError ('Cannot compute tau for '+inttostr(ccode1)+
                     ' vs. '+inttostr(ccode2)+' in '+inttostr(ayear)+
                     '. Contingency table is '+inttostr(nrows)+' x '+inttostr(ncolumns)+'.  TAU set to 0.',
                     0, continue, error_log);
                  tau_from_alliance_year := missing_value;
               end
            else
               tau_from_alliance_year := tau_from_table (table, N, nrows, ncolumns);

            {these commands will print out a few tables for verification of table #s}
            {if (ccode1=651) and (ccode2=625) and (region=globe) and (ayear=1958) then
              begin
                 trace.message ('651 625 1958');
                 for row := 1 to 4 do
                   trace.message (inttostr(table[row,1])+' '+inttostr(table[row,2])+' '+
                                  inttostr(table[row,3])+' '+inttostr(table[row,4])+' ');
                 trace.message ('Tau = '+ realtostring(result));
              end;
            if (ccode1=652) and (ccode2=660) and (region=globe) and (ayear=1949) then
              begin
                 trace.message ('652 660 1949');
                 for row := 1 to 4 do
                   trace.message (inttostr(table[row,1])+' '+inttostr(table[row,2])+' '+
                                  inttostr(table[row,3])+' '+inttostr(table[row,4])+' ');
                 trace.message ('Tau = '+ realtostring(result));
              end;
            if (ccode1=645) and (ccode2=690) and (region=globe) and (ayear=1967) then
              begin
                 trace.message ('645 690 1967');
                 for row := 1 to 4 do
                   trace.message (inttostr(table[row,1])+' '+inttostr(table[row,2])+' '+
                                  inttostr(table[row,3])+' '+inttostr(table[row,4])+' ');
                 trace.message ('Tau = '+ realtostring(result));
              end;
            if (ccode1=645) and (ccode2=690) and (region=globe) and (ayear=1975) then
              begin
                 trace.message ('645 690 1975');
                 for row := 1 to 4 do
                   trace.message (inttostr(table[row,1])+' '+inttostr(table[row,2])+' '+
                                  inttostr(table[row,3])+' '+inttostr(table[row,4])+' ');
                 trace.message ('Tau = '+ realtostring(result));
              end;

            }
         end;

         { --------------------------------------------------------------- }

   procedure preprocess_for_tau_or_s (const configuration : configuration_type; var files_exist : boolean);
      begin
         files_exist := false;
         if user_selections.alliance_data_source = flat_dyadic then
            begin
               if check_file_Exists (configuration.dyadic_alliance_file_name, 'Dyadic Alliance Data') then
                  files_exist := true;
            end
         else  {user_selections.alliance_data_source = sequence Num format then}
            begin
               if check_file_Exists (configuration.cow_alliance_file_name, 'Main COW Alliance Data') and
                  check_file_Exists (configuration.alliance_seq_file_name, 'COW Seq Numbers for Alliance Data') then
                  files_exist := true;
            end
      end;
                  { --------------------- }

   procedure compute_and_save_taus (const sequence_alliance_file_name, seq_file_name,
             dyadic_alliance_file_name, tau_file_name : TFileName;
             const nation_list : Tnation_array_obj; first_proc_year,
             last_proc_year : year_range; raw_data_source : alliance_in_data_type);

      {from alliance_data, create and save a file of tau data}
      var alliance_data : Talliance_array_obj;
          num_partitions, years_per_partition, partition_start_year,
          partition_end_year, partition_loop : integer;
          tau_file : tau_file_type;
          ccode1, ccode2 : ccode_range;
          year : year_range;
          singleccode1, singleccode2, singleyear : single;
          global, regional1, regional2 : single;
          alliance_year_copy : Talliance_year_obj;
          file_exists : boolean;
          temp : integer;
          num_created : longint;
          tautrace : TTrace_obj;

                  { --------------------- }

      procedure calc_tau_partition (first_year, last_year : year_range;
                var num_partitions, years_per_partition : integer);
         var mem_per_year, mem_to_use : longint;
             years_possible, num_years : longint;
         begin
            trace.enter('Entered calc_tau_partition procedure');
            {Calculation will be, how many years of alliance data can fit into
             1/2 of avaialable memory.  This should leave LOTs of memory free for
             miscellaneous variables and functions. }
            {formula for # partitions is trunc (freemem to use) / (mem per partition)}
            num_years := (last_year - first_year + 1);
            mem_to_use := mem_for_procedure - Talliance_array_obj_mem_overhead;
            mem_per_year := Talliance_array_obj_mem_per_year;
            years_possible := trunc ( mem_to_use / mem_per_year);
            years_per_partition := min (years_possible, num_years);
            if years_per_partition < 1 then
               begin
                  EUGeneError ('Error - less than one year per partition calculated in procedure!',
                        5, stop, error_log);
               end;

             {Now, given that many years and calced # of years per partition, can see
              how many partitions are necessary. }
            num_partitions := (num_years div years_per_partition);
            if num_years mod years_per_partition > 0 then inc(num_partitions);
               {div in above calc will round down, so add 1 if > .  }

            {Now, make sure all years per are about even, so don't do 120 years
             in one pass and only 50 in the next.}
            years_per_partition := (num_years div num_partitions);
            if num_years mod num_partitions > 0 then inc(years_per_partition);

            if debug[3] then
            begin
              trace.message ('Information calculated on Tau calculation partition division:');
              trace.message ('Total years for processing: '+inttostr(num_years));
              trace.message ('Memory that can be used: '+inttostr(mem_to_use)+' bytes');
              trace.message ('Memory to be allocated per year: '+inttostr(mem_per_year)+' bytes');
              trace.message ('Years possible per partition: '+inttostr(years_possible));
              trace.message ('Number of partitions necessary: '+inttostr(num_partitions));
              trace.message ('Actual number of years per partition '+inttostr(years_per_partition));
            end;   {if debug[3]}
            if debug[2] then
            trace.message('Will run through ' + inttostr(num_partitions)+' tau partitions, with '+
                     inttostr(years_per_partition)+' year in each partition.');
            trace.exit ('Finished calc_tau_partition procedure');
         end;

                  { --------------------- }

      begin         {main proc compute and save taus}
      alliance_data := nil;
      try
         trace.enter('Entered compute and save taus procedure;  processing all dyads');
         tautrace := Ttrace_obj.init(trace.get_trace_level);
          if first_proc_year > last_proc_year then
            begin
               temp := first_proc_year;
               first_proc_year := last_proc_year;
               last_proc_year := temp;
            end;
         {figure out any necessary partitioning}
         calc_tau_partition (first_proc_year, last_proc_year, num_partitions, years_per_partition);
         partition_start_year := first_proc_year;

         trace.message ('Processing and writing taus to output file');
           {Make sure necessary files exist.}
         preprocess_for_tau_or_s (configuration, file_exists);
         if file_exists then
           begin
             try
                try
                   num_created := 0;
                   assignFile (tau_file, tau_file_name);
                   rewrite (tau_file);
                   for partition_loop := 1 to num_partitions do
                    begin
                     partition_end_year := min((partition_start_year + years_per_partition - 1), last_proc_year);
                     try
                        alliance_data := Talliance_array_obj.init (sequence_alliance_file_name, seq_file_name,
                                         dyadic_alliance_file_name, partition_start_year, partition_end_year,
                                         raw_data_source);
                        For year := partition_start_year to partition_end_year do
                          begin
                             try
                                alliance_data.make_copy_of_alliance_year_array (year, alliance_year_copy);
                                for ccode1 := min_ccode to max_ccode do
                                 if (nation_list.is_a_state (ccode1, year)) then
                                   begin
                                     singleyear := year;
                                     singleccode1 := ccode1;
                                     write (tau_file, singleyear, singleccode1);
                                     for ccode2 := min_ccode to max_ccode do
                                       if (nation_list.is_a_state (ccode2, year)) then
                                          begin
                                             inc(num_created);
                                             tautrace.tick('Executing Procedure: Tau-b Scores, '+inttostr(year),
                                                nation_list.get_dyad_years);
                                             {need to compute global and regional tau.  For global, use just
                                              default record, which has all states in it.}
                                             if ccode1 = ccode2 then
                                                begin
                                                  global := 1;
                                                  regional1 := 1;
                                                  regional2 := 1;
                                                end
                                             else
                                                begin
                                                  global := tau_from_alliance_year
                                                    (ccode1, ccode2, year, alliance_year_copy, globe);
                                                  regional1 := tau_from_alliance_year
                                                    (ccode1, ccode2, year,
                                                    alliance_year_copy, nation_list.get_home_region(ccode1));
                                                  regional2 := tau_from_alliance_year
                                                    (ccode1, ccode2, year,
                                                    alliance_year_copy, nation_list.get_home_region(ccode2));
                                                end;
                                             singleccode2 := ccode2;
                                             {if change write, need to change eutypes1 read.}
                                             write (tau_file, singleccode2, global, regional1, regional2);
                                          end;
                                   end;    {cc1 is a state}
                             finally
                                alliance_year_copy.free;
                             end;
                          end;   {for year}
                     finally
                        alliance_data.free;
                        partition_start_year := partition_end_year + 1;
                     end;
                    end;  {for partition_loop}

                finally
                   tautrace.tickdone;
                   CloseFile (tau_file);
                   ShowMessage ('Computation of Tau values complete!');
                   trace.exit ('Finished computing and saving tau values, all dyads');
                   tautrace.free;
                end;
             except
                on EInOutError do
                   begin
                      FileErrorBox.maindo ('Error opening/writing to output file "'+tau_file_name+ '"',
                                           'File may be in use by another program, may be read-only, ',
                                           'or disk may be full.');
                      FileErrorBox.showmodal;
                      raise;
                   end;
             end;
           end;    {if file_exists}
      except
         on EUserInterrupt do raise;
      end;  {except}

      end;    {procedure compute and save taus}

         { --------------------------------------------------------------- }

   function s_from_alliance_year (ccode1, ccode2 : ccode_range; ayear : year_range;
            const alliance_data_year : Talliance_year_obj; region : region_type;
            weight_s : s_weighting_range; const sys_capability_data : Tsys_capability_array_obj): real;
             {given a year of alliance data, will compute s for a specified dyad in that year.}
             {this is in a separate procedure and is not within the "compute and save-x"
              procedure because when figuring risk, this procedure will need to be called
              as part of the computation of the best and worst alliance portfolios.}
         var
            cc1value, cc2value, partner_ccode : ccode_range;
            dmax : single;
            max_distance, sum_abs_alliance_diff : real;
            {n : num_countries_range;}
            cap : single;
         begin
            s_from_alliance_year := missing_value;  {out of normal range value initially}
            max_distance := 3;  {for alliances, 1..4}
            sum_abs_alliance_diff := 0;
            {n := 0;}

            if weight_s=weighted then
               begin             {NOTE:  for now, the only option is weighting by capabilities}
                  if (ayear<configuration.first_cap_year) or (ayear>configuration.last_cap_year) then
                     begin
                        dmax := 0;
                        sum_abs_alliance_diff := 0;
                        result := missing_value;
                     end
                  else
                     begin
                        dmax := 0;
                        {figure out sum of weights, equals dmax, along with d score.}
                        for partner_ccode := min_ccode to max_ccode do
                           if nation_list.is_a_state (partner_ccode, ayear) then
                              if ((nation_list.is_involved_in_region(partner_ccode, region, ayear)) or
                                  (ccode1=partner_ccode) or (ccode2=partner_ccode)) then
                                 begin
                                    cc1value := alliance_data_year.get_alliance_value(ccode1, partner_ccode);
                                    cc2value := alliance_data_year.get_alliance_value(ccode2, partner_ccode);
                                    if (cc1value >= defense) and (cc1value <=no_alliance) and (cc2value >=defense) and (cc2value <= no_alliance) then
                                   {NOTE:  This is currently not implemented, all states are included!!
                                    As set up in the alliance data, 1 is a defense pact, 2 is neut, 3 is entente,
                                    4 means no alliance but active in system (which I want to count in tau table)
                                    and 0 means no allianc and inactive, which I don't want to count.}
                                       begin
                                          cap := sys_capability_data.get_syscap(partner_ccode, ayear);
                                          if cap <> missing_value then
                                             begin
                                                sum_abs_alliance_diff := sum_abs_alliance_diff +
                                                   (cap * abs(cc1value - cc2value) );
                                                dmax := dmax + cap;
                                             end;
                                          {otherwise, if capability IS missing, must skip it}
                                       end;
                                 end;
                        sum_abs_alliance_diff := sum_abs_alliance_diff / max_distance;
                     end;
               end
            else   {not weighting s}
               begin
                  dmax := 0;
                  for partner_ccode := min_ccode to max_ccode do
                     if nation_list.is_a_state (partner_ccode, ayear) then
                        if ((nation_list.is_involved_in_region(partner_ccode, region, ayear)) or
                            (ccode1=partner_ccode) or (ccode2=partner_ccode)) then
                           begin
                              cc1value := alliance_data_year.get_alliance_value(ccode1, partner_ccode);
                              cc2value := alliance_data_year.get_alliance_value(ccode2, partner_ccode);
                              if (cc1value >= defense) and (cc1value <=no_alliance) and (cc2value >=defense) and (cc2value <= no_alliance) then
                             {NOTE:  This is currently not implemented, all states are included!!
                              As set up in the alliance data, 1 is a defense pact, 2 is neut, 3 is entente,
                              4 means no alliance but active in system (which I want to count in tau table)
                              and 0 means no allianc and inactive, which I don't want to count.}
                                 begin
                                    sum_abs_alliance_diff := sum_abs_alliance_diff +
                                       (abs(cc1value - cc2value));
                                    dmax := dmax + 1;
                                    {inc(n);}
                                 end;
                        end;
                  sum_abs_alliance_diff := sum_abs_alliance_diff / max_distance;
               end;
            {dmax might be 0 if capabilities don't run through alliance years.  It's OK in that case
             because S will be missing anyway.}
            if (dmax<=0) then
               begin
                  result := missing_value;
                  if ( (ayear<configuration.first_cap_year) or (ayear>configuration.last_cap_year)) then
                     begin     {this is the OK situation, we expect it missing.}
                        {do nothing, report no error}
                     end
                  else
                     begin
                        showmessage ('Error in S generation procedure - dmax is 0 or negative for '+inttostr(ccode1)+' '+inttostr(ccode2)+' '+inttostr(ayear)+' region '+inttostr(ord(region)))
                     end;
               end
            else
               begin
                  result := 1 - 2 * (sum_abs_alliance_diff / dmax);
                  if ((result > 1) or (result < -1)) then
                     showmessage ('S out of range for cc1 cc2 year un/wt of '+inttostr(ccode1) + inttostr(ccode2) + inttostr(ayear) );
               end;


         end;    {function s from alliance year}

         { --------------------------------------- }

   procedure compute_and_save_s (const sequence_alliance_file_name, seq_file_name,
             dyadic_alliance_file_name, s_file_name, sys_cap_file_name : TFileName;
             const nation_list : Tnation_array_obj; first_proc_year,
             last_proc_year : year_range; raw_data_source : alliance_in_data_type);

      {from alliance_data, create and save a file of s data}
      var alliance_data : Talliance_array_obj;
          sys_capability_data : Tsys_capability_array_obj;
          num_partitions, years_per_partition, partition_start_year,
          partition_end_year, partition_loop : integer;
          s_file : s_file_type;  {this is an array of single}
          ccode1, ccode2 : ccode_range;
          year : year_range;
          singleccode1, singleccode2, singleyear : single;
          global, regional1, regional2 : single;
          alliance_year_copy : Talliance_year_obj;
          file_exists : boolean;
          temp : integer;
          num_created : longint;
          strace : TTrace_obj;
          weight : s_weighting_range;

                  { --------------------- }

      procedure calc_s_partition (first_year, last_year : year_range;
                var num_partitions, years_per_partition : integer);
         var mem_per_year, mem_to_use : longint;
             years_possible, num_years : longint;
         begin
            trace.enter('Entered calc_s_partition procedure');
            {Calculation will be, how many years of alliance data can fit into
             1/2 of avaialable memory.  This should leave LOTs of memory free for
             miscellaneous variables and functions. }
            {formula for # partitions is trunc (freemem to use) / (mem per partition)}
            num_years := (last_year - first_year + 1);
            mem_to_use := mem_for_procedure - Talliance_array_obj_mem_overhead;
            mem_per_year := Talliance_array_obj_mem_per_year;
            years_possible := trunc ( mem_to_use / mem_per_year);
            years_per_partition := min (years_possible, num_years);
            if years_per_partition < 1 then
               begin
                  EUGeneError ('Error - less than one year per partition calculated in procedure!',
                        5, stop, error_log);
               end;

             {Now, given that many years and calced # of years per partition, can see
              how many partitions are necessary. }
            num_partitions := (num_years div years_per_partition);
            if num_years mod years_per_partition > 0 then inc(num_partitions);
               {div in above calc will round down, so add 1 if > .  }

            {Now, make sure all years per are about even, so don't do 120 years
             in one pass and only 50 in the next.}
            years_per_partition := (num_years div num_partitions);
            if num_years mod num_partitions > 0 then inc(years_per_partition);

            if debug[3] then
            begin
              trace.message ('Information calculated on s calculation partition division:');
              trace.message ('Total years for processing: '+inttostr(num_years));
              trace.message ('Memory that can be used: '+inttostr(mem_to_use)+' bytes');
              trace.message ('Memory to be allocated per year: '+inttostr(mem_per_year)+' bytes');
              trace.message ('Years possible per partition: '+inttostr(years_possible));
              trace.message ('Number of partitions necessary: '+inttostr(num_partitions));
              trace.message ('Actual number of years per partition '+inttostr(years_per_partition));
            end;   {if debug[3]}
            if debug[2] then
            trace.message('Will run through ' + inttostr(num_partitions)+' s partitions, with '+
                     inttostr(years_per_partition)+' year in each partition.');
            trace.exit ('Finished calc_s_partition procedure');
         end;

                  { --------------------- }

      begin         {main proc compute and save s}
      alliance_data := nil;
      sys_capability_data := nil;

      try
         trace.enter('Entered compute and save s procedure;  processing all dyads');
         strace := Ttrace_obj.init(trace.get_trace_level);
          if first_proc_year > last_proc_year then
            begin
               temp := first_proc_year;
               first_proc_year := last_proc_year;
               last_proc_year := temp;
            end;
         {figure out any necessary partitioning}
         calc_s_partition (first_proc_year, last_proc_year, num_partitions, years_per_partition);
         partition_start_year := first_proc_year;

         trace.message ('Processing and writing s to output file');
           {Make sure necessary files exist.}
         preprocess_for_tau_or_s (configuration, file_exists);
         if file_exists then
           begin
             try
                try
                   num_created := 0;
                   assignFile (s_file, s_file_name);
                   rewrite (s_file);
                   for partition_loop := 1 to num_partitions do
                    begin
                     partition_end_year := min((partition_start_year + years_per_partition - 1), last_proc_year);
                     try
                        alliance_data := Talliance_array_obj.init (sequence_alliance_file_name, seq_file_name,
                                         dyadic_alliance_file_name, partition_start_year, partition_end_year,
                                         raw_data_source);
                        sys_capability_data := Tsys_capability_array_obj.init (sys_cap_file_name,
                            partition_start_year, partition_end_year);

                        For year := partition_start_year to partition_end_year do
                          begin
                             try
                                alliance_data.make_copy_of_alliance_year_array (year, alliance_year_copy);
                                for ccode1 := min_ccode to max_ccode do
                                 if (nation_list.is_a_state (ccode1, year)) then
                                   begin
                                     singleyear := year;
                                     singleccode1 := ccode1;
                                     write (s_file, singleyear, singleccode1);
                                     for ccode2 := min_ccode to max_ccode do
                                       if (nation_list.is_a_state (ccode2, year)) then
                                          begin
                                             inc(num_created);
                                             strace.tick('Executing Procedure: S Scores, '+inttostr(year),
                                                nation_list.get_dyad_years);
                                             {need to compute global and regional s.  For global, use just
                                              default record, which has all states in it.  If this changes,
                                              update eutypes1 s.init method.}
                                             singleccode2 := ccode2;
                                             write (s_file, singleccode2);
                                             for weight := unweighted to weighted do
                                             begin
                                                if ccode1 = ccode2 then
                                                   begin
                                                     global := 1;
                                                     regional1 := 1;
                                                     regional2 := 1;
                                                   end
                                                else
                                                   begin
                                                     global := s_from_alliance_year
                                                       (ccode1, ccode2, year, alliance_year_copy, globe, weight, sys_capability_data);
                                                     regional1 := s_from_alliance_year
                                                       (ccode1, ccode2, year,
                                                       alliance_year_copy, nation_list.get_home_region(ccode1), weight, sys_capability_data);
                                                     regional2 := s_from_alliance_year
                                                       (ccode1, ccode2, year,
                                                       alliance_year_copy, nation_list.get_home_region(ccode2), weight, sys_capability_data);
                                                   end;
                                                {if change write, need to change eutypes1 read.}
                                                write (s_file, global, regional1, regional2);
                                             end;

                                          end;
                                   end;    {cc1 is a state}
                             finally
                                alliance_year_copy.free;
                             end;
                          end;   {for year}
                     finally
                        alliance_data.free;
                        partition_start_year := partition_end_year + 1;
                     end;
                    end;  {for partition_loop}

                finally
                   CloseFile (s_file);
                   ShowMessage ('Computation of S values complete!');
                end;
             except
                on EInOutError do
                   begin
                      FileErrorBox.maindo ('Error opening/writing to output file "'+s_file_name+ '"',
                                           'File may be in use by another program, may be read-only, ',
                                           'or disk may be full.');
                      FileErrorBox.showmodal;
                      raise;
                   end;
             end;
           end;    {if file_exists}
         trace.exit ('Finished computing and saving s values, all dyads');
         strace.tickdone;
         strace.free;
      except
         on EUserInterrupt do raise;
      end;  {except}

      end;    {procedure compute and save s}

         { --------------------------------------- }

   procedure compute_and_output_s_from_arbitrary_countries_and_years (const sequence_alliance_file_name, seq_file_name,
             dyadic_alliance_file_name, sys_cap_file_name : TFileName;
             const nation_list : Tnation_array_obj; first_proc_year,
             last_proc_year : year_range; raw_data_source : alliance_in_data_type; randyseparator : separatortype;
             s_output_text_file_name, s_input_text_file_name : TFileName);

      {from alliance_data, create and save a file of tau data}
      var alliance_data : Talliance_array_obj;
          sys_capability_data : Tsys_capability_array_obj;
          num_partitions, years_per_partition, partition_start_year,
          partition_end_year, partition_loop : integer;
          ccode1, ccode2 : ccode_range;
          endyear, endyear1, endyear3, endyear5, outyear : year_range;
          singleccode1, singleccode2, singleyear : single;
          global, regional1, regional2 : single;
          file_exists : boolean;
          temp : integer;
          num_created : longint;
          strace : TTrace_obj;
          weight : s_weighting_range;
          s_output_text_file, s_input_text_file : text;  {this is for flat output}
          outseparator : char;
          iterationsdone : integer;

                  { --------------------- }


   function s_from_arbitrary_countries_and_years (ccode1 : ccode_range; year1 : year_range;
            ccode2 : ccode_range; year2 : year_range;
            const alliance_data : Talliance_array_obj; region : region_type;
            weight_s : s_weighting_range; const sys_capability_data : Tsys_capability_array_obj): real;
             {given a full set of alliance data, will compute s between any country in year 1
              and any country in year 2.  Could be called with "US 1950" and "US 1955" for instance.}
             {Will assume entire span of alliance data is read in}
         var
            cc1value, cc2value, partner_ccode : ccode_range;
            dmax : single;
            max_distance, sum_abs_alliance_diff : real;
            cap : single;
         begin
            result := missing_value;  {out of normal range value initially}
            max_distance := 3;  {for alliances, 1..4}
            sum_abs_alliance_diff := 0;
            {n := 0;}

            {for partner ccodes, they must be a state in both years.}
            {Also, for regional must be involved in region in both years.}
            if weight_s=weighted then
               begin             {NOTE:  for now, the only option is weighting by capabilities}
                  if ((year1<configuration.first_cap_year) or (year2<configuration.first_cap_year) or
                      (year1>configuration.last_cap_year) or (year2>configuration.last_cap_year)) then
                     begin
                        dmax := 0;
                        sum_abs_alliance_diff := 0;
                        result := missing_value;
                     end
                  else
                     begin
                        dmax := 0;
                        {figure out sum of weights, equals dmax, along with d score.}
                        for partner_ccode := min_ccode to max_ccode do
                           if (nation_list.is_a_state (partner_ccode, year1)) and (nation_list.is_a_state (partner_ccode, year2)) then
                              if (((nation_list.is_involved_in_region(partner_ccode, region, year1)) and
                                   (nation_list.is_involved_in_region(partner_ccode, region, year2))) or
                                  (ccode1=partner_ccode) or (ccode2=partner_ccode)) then
                                 begin
                                    cc1value := alliance_data.get_alliance_value(ccode1, partner_ccode, year1);
                                    cc2value := alliance_data.get_alliance_value(ccode2, partner_ccode, year2);
                                    if (cc1value >= defense) and (cc1value <=no_alliance) and (cc2value >=defense) and (cc2value <= no_alliance) then
                                       begin
                                          cap := sys_capability_data.get_syscap(partner_ccode, year1);
                                          if cap <> missing_value then
                                             begin
                                                sum_abs_alliance_diff := sum_abs_alliance_diff +
                                                   (cap * abs(cc1value - cc2value) );
                                                dmax := dmax + cap;
                                             end;
                                          {otherwise, if capability IS missing, must skip it}
                                       end;
                                 end;
                        sum_abs_alliance_diff := sum_abs_alliance_diff / max_distance;
                     end;
               end
            else   {not weighting s}
               begin
                  dmax := 0;
                  for partner_ccode := min_ccode to max_ccode do
                     if (nation_list.is_a_state (partner_ccode, year1)) and (nation_list.is_a_state (partner_ccode, year2)) then
                        if (((nation_list.is_involved_in_region(partner_ccode, region, year1)) and
                             (nation_list.is_involved_in_region(partner_ccode, region, year2))) or
                            (ccode1=partner_ccode) or (ccode2=partner_ccode)) then
                           begin
                              cc1value := alliance_data.get_alliance_value(ccode1, partner_ccode, year1);
                              cc2value := alliance_data.get_alliance_value(ccode2, partner_ccode, year2);
                              if (cc1value >= defense) and (cc1value <=no_alliance) and (cc2value >=defense) and (cc2value <= no_alliance) then
                                 begin
                                    sum_abs_alliance_diff := sum_abs_alliance_diff +
                                       (abs(cc1value - cc2value));
                                    dmax := dmax + 1;
                                    {inc(n);}
                                 end;
                        end;
                  sum_abs_alliance_diff := sum_abs_alliance_diff / max_distance;
               end;
            {dmax might be 0 if capabilities don't run through alliance years.  It's OK in that case
             because S will be missing anyway.}
            if (dmax<=0) then
               begin
                  result := missing_value;
                  if ( (year1<configuration.first_cap_year) or (year2<configuration.first_cap_year) or
                       (year1>configuration.last_cap_year) or (year2>configuration.last_cap_year)) then
                     begin     {this is the OK situation, we expect it missing.}
                        {do nothing, report no error}
                     end
                  else
                     begin
                        showmessage ('Error in S generation procedure - dmax is 0 or negative for '+inttostr(ccode1)+' '+inttostr(ccode2)+' '+inttostr(year1)+' or '+inttostr(year2)+' region '+inttostr(ord(region)))
                     end;
               end
            else
               begin
                  result := 1 - 2 * (sum_abs_alliance_diff / dmax);
                  if ((result > 1) or (result < -1)) then
                     showmessage ('S arbitrary out of range for cc1 cc2 year1 year2 un/wt of '+inttostr(ccode1) + inttostr(ccode2) + inttostr(year1) + inttostr(year2));
               end;

         end;    {function s from alliance year}

                  { --------------------- }

      begin         {main proc compute and save s arbitrary}

       {Need to read entire span of alliance data}

      alliance_data := nil;
      sys_capability_data := nil;
      case randyseparator of
         comma : outseparator := ',';
         space : outseparator := ' ';
         tab : outseparator := chr(9);
         else outseparator := ',';
      end;   {case}

      try
         trace.enter('Entered compute and save s procedure;  processing all dyads');
         strace := Ttrace_obj.init(trace.get_trace_level);
         if first_proc_year > last_proc_year then
            begin
               temp := first_proc_year;
               first_proc_year := last_proc_year;
               last_proc_year := temp;
            end;
         {Not partitioning here, but use those variables.}
         partition_start_year := first_proc_year;
         partition_end_year := last_proc_year;

         trace.message ('Processing and writing s for arbitrary pairings to text output file');
           {Make sure necessary files exist.}
         preprocess_for_tau_or_s (configuration, file_exists);
         if file_exists then
           begin
             try
                try
                   num_created := 0;
                   assignFile (s_output_text_file, s_output_text_file_name);
                   rewrite (s_output_text_file);
                   assignFile (s_input_text_file, s_input_text_file_name);
                   reset (s_input_text_file);

                     try
                        alliance_data := Talliance_array_obj.init (sequence_alliance_file_name, seq_file_name,
                                         dyadic_alliance_file_name, partition_start_year, partition_end_year,
                                         raw_data_source);
                        sys_capability_data := Tsys_capability_array_obj.init (sys_cap_file_name,
                            partition_start_year, partition_end_year);
                        writeln (s_output_text_file, 'ccode1',outseparator,'ccode2',outseparator,'endyear',outseparator,
                           'endyear1',outseparator,'sunglyb1',outseparator,'sunr1yb1',outseparator,'sunr2yb1',outseparator,'swtglyb1',outseparator,'swtr1yb1',outseparator,'swtr2yb1',outseparator,
                           'endyear3',outseparator,'sunglyb3',outseparator,'sunr1yb3',outseparator,'sunr2yb3',outseparator,'swtglyb3',outseparator,'swtr1yb3',outseparator,'swtr2yb3',outseparator,
                           'endyear5',outseparator,'sunglyb5',outseparator,'sunr1yb5',outseparator,'sunr2yb5',outseparator,'swtglyb5',outseparator,'swtr1yb5',outseparator,'swtr2yb5',outseparator);
                        {Skip first line of input file}
                        readln(s_input_text_file);
                        while not eof(s_input_text_file) do
                           begin
                              ccode1 := read_csv_int(s_input_text_file);
                              ccode2 := read_csv_int(s_input_text_file);
                              endyear := read_csv_int(s_input_text_file);
                              endyear1 := read_csv_int(s_input_text_file);
                              endyear3 := read_csv_int(s_input_text_file);
                              endyear5 := read_csv_int(s_input_text_file);
                              readln(s_input_text_file);
                              if ((nation_list.is_a_state (ccode1, endyear)) ) then
                                 begin
                                    inc(num_created);
                                    strace.tick('Executing Procedure: S Scores arbitrary, base year, '+inttostr(endyear),0);

                                    write (s_output_text_file, ccode1, outseparator, ccode2, outseparator, endyear, outseparator);
                                    {Do this 3 times, one for year2, year3, year4}
                                    iterationsdone := 0;
                                    repeat
                                       if iterationsdone = 0 then outyear := endyear1
                                       else if iterationsdone = 1 then outyear := endyear3
                                       else if iterationsdone = 2 then outyear := endyear5;

                                       write (s_output_text_file, outyear, outseparator);

                                       {need to compute global and regional s.  For global, use just
                                        default record, which has all states in it.  If this changes,
                                        update eutypes1 s.init method.}
                                       for weight := unweighted to weighted do
                                       begin
                                          global := s_from_arbitrary_countries_and_years
                                              (ccode1, endyear, ccode1, outyear,
                                               alliance_data, globe, weight, sys_capability_data);
                                          regional1 := s_from_arbitrary_countries_and_years
                                              (ccode1, endyear, ccode1, outyear,
                                               alliance_data, nation_list.get_home_region(ccode1), weight, sys_capability_data);
                                          regional2 := s_from_arbitrary_countries_and_years
                                              (ccode1, endyear, ccode1, outyear,
                                               alliance_data, nation_list.get_home_region(ccode1), weight, sys_capability_data);
                                          {if change write, need to change eutypes1 read.}
                                          if ((nation_list.is_a_state (ccode1, outyear)) ) then
                                             write (s_output_text_file, global:6:4, outseparator, regional1:6:4, outseparator, regional2:6:4, outseparator)
                                          else
                                             write (s_output_text_file, '-9', outseparator, '-9', outseparator, '-9', outseparator);
                                       end;
                                       inc(iterationsdone);
                                    until iterationsdone = 3;
                                    writeln (s_output_text_file);
                                 end;           {if both are states then...}
                           end;   {while not eof...}
                     finally
                        alliance_data.free;
                        partition_start_year := partition_end_year + 1;
                     end;

                finally
                   CloseFile (s_output_text_file);
                   CloseFile (s_input_text_file);
                   ShowMessage ('Computation of S values arbitrary complete!');
                end;
             except
                on EInOutError do
                   begin
                      FileErrorBox.maindo ('Error opening/writing to output file "'+s_output_text_file_name+ '"',
                                           'File may be in use by another program, may be read-only, ',
                                           'or disk may be full.');
                      FileErrorBox.showmodal;
                      raise;
                   end;
             end;
           end;    {if file_exists}
         trace.exit ('Finished computing and writing arbitrary s values, all dyads');
         strace.tickdone;
         strace.free;
      except
         on EUserInterrupt do raise;
      end;  {except}

      end;    {procedure compute and save s}


   { --------------------------------------- }


   procedure compute_and_save_sys_capability (const raw_cap_file_name,
             modified_cap_file_name, sys_cap_file_name: TFileName;
             const nation_list : Tnation_array_obj; const ccode_index : Tccode_index_obj;
             first_proc_year, last_proc_year: year_range);

       {get raw capability data, process it, save to an outfile.  Exit without any data
        used or created left in memory.}
      type year_sum_type = record
                 sum_irst, sum_upop, sum_tpop, sum_energy, sum_milper, sum_milex : longint;
              end;   {record}
      var raw_capability_data : Traw_capability_array_obj;
          sys_cap_file_record : sys_cap_file_record_type;
          ccode : ccode_range;
          sys_cap_file : sys_cap_file_type;
          year_sum : year_sum_type;
          num_partitions, years_per_partition, partition_start_year,
          partition_end_year, partition_loop, year_loop : integer;
          temp : year_range;
          x : ccode_index_range;
          file_exists : boolean;
          num_saved : longint;
          systrace : Ttrace_obj;

                  { --------------------- }

      procedure calc_sys_cap_partition (var num_partitions, years_per_partition : integer;
                year1, year2 : year_range);
         {need to calculate amount of raw capability data that can fit.  }
         var mem_per_year, mem_to_use : longint;
             years_possible, num_years : longint;
         begin
            trace.enter('Entered calc_sys_cap_partition procedure');

           {Calculation will be, how many years of alliance data can fit into
             1/2 of avaialable memory.  This should leave LOTs of memory free for
             miscellaneous variables and functions. }
            {formula for # partitions is trunc (freemem to use) / (mem per partition)}
            {capability can be calculated between first_cap_year and last_cap_year}
            num_years := (year2 - year1 + 1);
            mem_to_use := mem_for_procedure - Traw_capability_array_obj_mem_overhead;
            mem_per_year := Traw_capability_array_obj_mem_per_year;
            years_possible := trunc ( mem_to_use / mem_per_year);
            years_per_partition := min (years_possible, num_years);
            if years_per_partition < 1 then
               begin
                  EUGeneError ('Error - less than one year per partition calculated in procedure!',
                        5, stop, error_log);
               end;

            {Now, given that many years and calced # of years per partition, can see
             how many partitions are necessary. }
            num_partitions := (num_years div years_per_partition);
            if num_years mod years_per_partition > 0 then inc(num_partitions);
               {div in above calc will round down, so add 1 if > .  }

            {Now, make sure all years per are about even, so don't do 120 years
             in one pass and only 50 in the next.}
            years_per_partition := (num_years div num_partitions);
            if num_years mod num_partitions > 0 then inc(years_per_partition);

            if debug[3] then
            begin
              trace.message ('Information calculated on 6 part capability calculation partition division:');
              trace.message ('Total years for processing: '+inttostr(num_years));
              trace.message ('Memory that can be used: '+inttostr(mem_to_use)+' bytes');
              trace.message ('Memory to be allocated per year: '+inttostr(mem_per_year)+' bytes');
              trace.message ('Years possible per partition: '+inttostr(years_possible));
              trace.message ('Number of partitions necessary: '+inttostr(num_partitions));
              trace.message ('Actual number of years per partition '+inttostr(years_per_partition));
            end;   {if debug[3]}
            if debug[2] then
            trace.message('Will run through ' + inttostr(num_partitions)+' 6 part capability computation partitions, ');
            trace.message(' with '+ inttostr(years_per_partition)+' year in each partition.');

            trace.exit ('Finished calc_sys_cap_partition procedure');
         end;

                  { --------------------- }

      procedure compute_year_sum (const raw_cap : Traw_capability_array_obj;
                year : year_range; var year_sum : year_sum_type);
                {adds up capabilities in each area into sum variables}
         var ccode : ccode_range;
         begin
            year_sum.sum_irst := 0;
            year_sum.sum_upop:= 0;
            year_sum.sum_tpop:= 0;
            year_sum.sum_energy:= 0;
            year_sum.sum_milper:= 0;
            year_sum.sum_milex:= 0;

            for ccode := min_ccode to max_ccode do
               if (nation_list.is_a_state (ccode, year)) then
                  begin
                     if not ( (raw_cap.get_irst(ccode, year) = missing_value) or
                              (raw_cap.get_irst(ccode, year) = initialized_value) ) then
                        year_sum.sum_irst := year_sum.sum_irst + raw_cap.get_irst(ccode, year);
                     if not ( (raw_cap.get_upop(ccode, year) = missing_value) or
                              (raw_cap.get_upop(ccode, year) = initialized_value) ) then
                        year_sum.sum_upop := year_sum.sum_upop + raw_cap.get_upop(ccode, year);
                     if not ( (raw_cap.get_tpop(ccode, year) = missing_value) or
                              (raw_cap.get_tpop(ccode, year) = initialized_value) ) then
                        year_sum.sum_tpop := year_sum.sum_tpop + raw_cap.get_tpop(ccode, year);
                     if not ( (raw_cap.get_energy(ccode, year) = missing_value) or
                              (raw_cap.get_energy(ccode, year) = initialized_value) ) then
                        year_sum.sum_energy := year_sum.sum_energy + raw_cap.get_energy(ccode, year);
                     if not ( (raw_cap.get_milper(ccode, year) = missing_value) or
                              (raw_cap.get_milper(ccode, year) = initialized_value) ) then
                        year_sum.sum_milper := year_sum.sum_milper + raw_cap.get_milper(ccode, year);
                     if not ( (raw_cap.get_milex(ccode, year) = missing_value) or
                              (raw_cap.get_milex(ccode, year) = initialized_value) ) then
                        year_sum.sum_milex := year_sum.sum_milex + raw_cap.get_milex(ccode, year);
                  end;

         end;     {proc compute year sum}

                  { --------------------- }

      function computed_sys_cap (year_sum : year_sum_type; year : year_range; ccode : ccode_range;
                             const raw_cap : Traw_capability_array_obj): single;
                  {computes cow 6 part index for ccode in year given the year totals}
         var pct : array[1..6] of single;
             numerator : single;
             denominator : integer;
             loop : integer;
         begin
            for loop := 1 to 6 do
               pct[loop] := missing_value;
            if (nation_list.is_a_state (ccode, year)) then
                  begin
                     if ( (year_sum.sum_irst = 0) or
                          (raw_cap.get_irst(ccode, year) = missing_value) or
                          (raw_cap.get_irst(ccode, year) = initialized_value)) then
                        pct[1] := missing_value
                        else pct[1] := raw_cap.get_irst(ccode, year) / year_sum.sum_irst;
                     if ( (year_sum.sum_upop = 0) or
                          (raw_cap.get_upop(ccode, year) = missing_value) or
                          (raw_cap.get_upop(ccode, year) = initialized_value)) then
                        pct[2] := missing_value
                        else pct[2] := raw_cap.get_upop(ccode, year) / year_sum.sum_upop;
                     if ( (year_sum.sum_tpop = 0) or
                          (raw_cap.get_tpop(ccode, year) = missing_value) or
                          (raw_cap.get_tpop(ccode, year) = initialized_value)) then
                        pct[3] := missing_value
                        else pct[3] := raw_cap.get_tpop (ccode, year) / year_sum.sum_tpop;
                     if ( (year_sum.sum_energy = 0) or
                          (raw_cap.get_energy(ccode, year) = missing_value) or
                          (raw_cap.get_energy(ccode, year) = initialized_value)) then
                        pct[4] := missing_value
                        else pct[4] := raw_cap.get_energy(ccode, year) / year_sum.sum_energy;
                     if ( (year_sum.sum_milper = 0) or
                          (raw_cap.get_milper(ccode, year) = missing_value) or
                          (raw_cap.get_milper(ccode, year) = initialized_value)) then
                        pct[5] := missing_value
                        else pct[5] := raw_cap.get_milper(ccode, year) / year_sum.sum_milper;
                     if ( (year_sum.sum_milex = 0) or
                          (raw_cap.get_milex(ccode, year) = missing_value) or
                          (raw_cap.get_milex(ccode, year) = initialized_value)) then
                        pct[6] := missing_value
                        else pct[6] := raw_cap.get_milex(ccode, year) / year_sum.sum_milex;

                      {take average of capabilities across the 6 cats.  But, if set to not
                       include energy, then just act like its missing and don't add it in.}
                     numerator := 0;
                     denominator := 0;
                     for loop := 1 to 6 do
                        begin
                           if ((pct[loop] < 0) and (pct[loop] <> missing_value)) then
                              begin
                                 EUGeneError ('Error - percent of system capabilities calculated as < 0 for ' +
                                        inttostr(ccode)+' in '+inttostr(year)+'!!',
                                    10, continue, error_log);
                                 trace.message ('Notify Programmer!');
                              end
                           else
                           if ((pct[loop] = missing_value) or
                               ((loop=4) and
                                (user_selections.capability_modifications = no_energy))) then
                              begin
                                 {do nothing, no calculation to perform, either if
                                  missing or if energy and user chose not
                                  to include energy in calc.}
                              end
                           else
                           if pct[loop] <> missing_value then
                              begin
                                 numerator := numerator + pct[loop];
                                 inc(denominator);
                              end;
                        end;
                     if denominator=0 then computed_sys_cap := missing_value else
                        computed_sys_cap := numerator / denominator;
                  end   {if is_a_state}
            else
               computed_sys_cap := missing_value;
         end;     {function computed capability}

         { --------------------------------------- }

   procedure preprocess_for_sys_cap (const configuration : configuration_type; var files_exist : boolean);
      begin
         files_exist := false;
         if check_file_Exists (configuration.cow_raw_cap_file_name,
                           'Raw Capabilities Data') then
            files_exist := true;
      end;
                  { --------------------- }

      begin                          {main compute and save sys capabiltiy}
         raw_capability_data := nil;
         trace.enter('Entered compute_and_save_sys_capability procedure');
         systrace := Ttrace_obj.init(trace.get_trace_level);

         {This procedure assumes that the provided 1st, last proc years are valid.}
         if first_proc_year > last_proc_year then
            begin
               temp := first_proc_year;
               first_proc_year := last_proc_year;
               last_proc_year := temp;
            end;
         calc_sys_cap_partition (num_partitions, years_per_partition,
                                 first_proc_year, last_proc_year);

         partition_start_year := first_proc_year;

            {may want to add some file checks here...}
         preprocess_for_sys_cap (configuration, file_exists);
         if file_exists then
         begin
            trace.message ('Computing and writing % system capability to output file');
            try
               try
                  assignFile (sys_cap_file, sys_cap_file_name);
                  rewrite (sys_cap_file);
                  num_saved := 0;
                  for partition_loop := 1 to num_partitions do
                  begin
                     partition_end_year := min((partition_start_year + years_per_partition - 1), last_proc_year);
                     try
                        raw_capability_data := Traw_capability_array_obj.init
                           (raw_cap_file_name, modified_cap_file_name, partition_start_year, partition_end_year);
                        trace.message ('Partition from '+inttostr(partition_start_year)+' to '+inttostr(partition_end_year));
                        For year_loop := partition_start_year to partition_end_year do
                           begin
                              sys_cap_file_record.year := year_loop;
                              sys_cap_file_record.ccode_from_index_list := ccode_index.return_ccode_list;
                              for x := min_ccode_index to max_ccode_index do
                                 sys_cap_file_record.ccode_array[x] := initialized_value;
                              compute_year_sum (raw_capability_data, year_loop, year_sum);
                              for ccode := min_ccode to max_ccode do
                                begin
                                  if (nation_list.is_a_state (ccode, year_loop )) then
                                     begin
                                       inc(num_saved);
                                       systrace.tick('Executing Procedure: System Capability Data',
                                          nation_list.get_country_years);
                                       sys_cap_file_record.ccode_array[ccode_index.index(ccode)]
                                            := computed_sys_cap (year_sum, year_loop,
                                               ccode, raw_capability_data);
                                    end;
                                end;
                              write(sys_cap_file, sys_cap_file_record);
                           end;   {for partition_start_year to part_last_year}
                        partition_start_year := partition_end_year + 1;
                     finally
                        raw_capability_data.free;
                     end;
                  end;  {for partition_loop}
               finally
                  CloseFile (sys_cap_file);
                  ShowMessage ('Computation of % system capabilities complete!');
               end;   {try... finally block}
            except
                on EUserInterrupt do raise;
                on EInOutError do
                   begin
                      FileErrorBox.maindo ('Error opening/writing to output file "'+sys_cap_file_name+ '"',
                                           'File may be in use by another program, may be read-only, ',
                                           'or disk may be full.');
                      FileErrorBox.showmodal;
                      raise;
                   end;
             end;

         end;    {if file_exists}

         systrace.tickdone;
         systrace.exit ('Finished computing and writing % system capabilities, all dyads');
         systrace.free;
         trace.exit('Finished compute_and_save_sys_capability procedure');
      end;

         { --------------------------------------- }

   function eu_war_trap (const ccodei, ccodej : ccode_range; const year : year_range;
             const tau_data_year : Ttau_year_obj; const s_data_year : Ts_year_obj;
             const sys_capability_data : Tsys_capability_array_obj;
             const distance_data: Tdistance_array_obj) : single;
             {Figures the EU of ccode1 vs. ccode2 following procedures and methods of The
              War Trap.  Called with some big data structures, but one year of tau data.}
      var adj_capij, adj_capii, adj_capji, adj_capjj, adj_capkj, adj_capki : single;
          Uij, Uiki, Uikj, Pi, Pjk, Pik : single;
          EU_bilateral, EU_multilateral_no_uncertainty, EU_multilateral_uncertainty_risk_averse,
             EU_multilateral_uncertainty_risk_acceptant : single;
          ccodek : ccode_range;

      begin
            {main utilities involved, i and j}
         case user_selections.similarity_method of
            use_tau : Uij := tau_data_year.get_tau_value_regional (ccodei, ccodej, relevant_region(ccodei, ccodej, year));
            use_s : Uij := s_data_year.get_s_value_regional (ccodei, ccodej, relevant_region(ccodei, ccodej, year), user_selections.s_weighting);
            else EUGeneError ('Error in setting utility for s/tau option within eu war trap calculation:  similarity_method not use_tau or use_s.', 5, continue, error_log);
         end;

         {There is a problem if capabilities are missing - the equation can't
          be computed.  So check for that.}
         if ((sys_capability_data.get_syscap (ccodei, year) = missing_value) or
             (sys_capability_data.get_syscap (ccodej, year) = missing_value) or
             (Uij = missing_value) ) then
           begin
              EU_war_trap := missing_value;
           end
         else
           begin    {no capabilities are missing, so do full calculations}
            {Figure capabilities adjusted for distance}
            {these formulas are from war trap p. 105}
             adj_capij := adj_cap(ccodei, ccodej, year, sys_capability_data, distance_data);
             adj_capii := adj_cap(ccodei, ccodei, year, sys_capability_data, distance_data);
             adj_capji := adj_cap(ccodej, ccodei, year, sys_capability_data, distance_data);
             adj_capjj := adj_cap(ccodej, ccodej, year, sys_capability_data, distance_data);

            {these formulas are from war trap pp. 105, 108-109.  This is Pi wins, bilateral}
             if (adj_capij + adj_capjj) = 0 then
                Pi := 0.5
             else Pi := adj_capij / (adj_capij + adj_capjj);

                {Can now calc the bilateral war EU, just i vs. j, war trap p. 47}
             EU_bilateral := Pi * (1-Uij) + (1-Pi)*(Uij-1);

             {Now, if a multilateral war}
             EU_multilateral_no_uncertainty := 0;
             {EU_multilateral_uncertainty_risk_averse := 0;     }
             {EU_multilateral_uncertainty_risk_acceptant := 0;  }
             for ccodek := min_ccode to max_ccode do
                  {It appears that this regional conception is what BdM War Trap does.
                   Bruce thinks so, and this seems to be what p. 97 says.
                   Consider as multilateral states that are involved in the relevant
                   region for the dyad ccodei vs. ccodej.  }
                 {if change this selection criteria, check risk procedures to match changes}
                if (nation_list.is_a_state (ccodek, year)) and
                   ((ccodek <> ccodei) and (ccodek <> ccodej)) and
                   (nation_list.is_involved_in_region(ccodek, relevant_region(ccodei, ccodej, year), year)) then
                  begin
                            {for some 3rd party k, ccodek, various elements.  See War trap 56-59}
                    adj_capkj := adj_cap(ccodek, ccodej, year, sys_capability_data, distance_data);
                    adj_capki := adj_cap(ccodek, ccodei, year, sys_capability_data, distance_data);
                            {p. 109}

                    if (adj_capij + max(adj_capkj, adj_capki) + adj_capjj) = 0 then
                       Pik := 0.5
                    else Pik := (adj_capij + max(adj_capkj, adj_capki)) /
                                (adj_capij + max(adj_capkj, adj_capki) + adj_capjj);

                    if (adj_capji + max(adj_capki, adj_capkj) + adj_capii) = 0 then
                       Pjk := 0.5
                    else Pjk := (adj_capji + max(adj_capki, adj_capkj)) /
                                (adj_capji + max(adj_capki, adj_capkj) + adj_capii);
                           {p 58}
                    case user_selections.similarity_method of
                        use_tau : begin
                             Uiki := tau_data_year.get_tau_value_regional (ccodek, ccodei, relevant_region(ccodek, ccodei, year));
                             Uikj := tau_data_year.get_tau_value_regional (ccodek, ccodej, relevant_region(ccodek, ccodej, year));
                            end;
                        use_s : begin
                             Uiki := s_data_year.get_s_value_regional (ccodek, ccodei, relevant_region(ccodek, ccodei, year), user_selections.s_weighting);
                             Uikj := s_data_year.get_s_value_regional (ccodek, ccodej, relevant_region(ccodek, ccodej, year), user_selections.s_weighting);
                            end;
                        else EUGeneError ('Error in setting utility for s/tau option within eu war trap calculation:  similarity_method not use_tau or use_s.', 5, continue, error_log);
                    end;

                    if (Uiki <> missing_value) and (Uikj <> missing_value) and
                          (sys_capability_data.get_syscap (ccodek, year) <> missing_value) then
                       EU_multilateral_no_uncertainty := EU_multilateral_no_uncertainty + ((Pik + Pjk -1) * (Uiki-Uikj));
                    {risk averse actors would only add some countries, namely those that don't like i}
                    {if Uiki < Uikj then
                      EU_multilateral_uncertainty_risk_averse := EU_multilateral_uncertainty_risk_averse +
                            ((Pik + Pjk -1) * (Uiki-Uikj));     }
                    {risk acceptant actors only add some countries, namely those in cat 1,2
                     which is those with a clear preference, ie like one but not the other.}
                    {if ((Uiki > 0) and (Uikj < 0)) or
                       ((Uiki < 0) and (Uikj > 0)) then
                      EU_multilateral_uncertainty_risk_acceptant := EU_multilateral_uncertainty_risk_acceptant +
                            ((Pik + Pjk -1) * (Uiki-Uikj));    }
                  end;

                {Can now calc the total EU, bilateral + multilateral possibility.  p.59.}
                {without uncertainty, have:  }
                {At this point, had at least Eu_bilateral, even if all multilateral were missing}
             EU_war_trap := EU_bilateral + EU_multilateral_no_uncertainty;
         end;       {not missing either capability, or Uij}

      end;   {function eu_war_trap}

         { --------------------------------------- }

   procedure compute_and_save_EUWarTrap (sys_cap_file_name, tau_file_name, s_file_name,
          EUWarTrap_Tau_file_name, EUWarTrap_S_file_name, locations_file_name : TFileName;
          const nation_list : Tnation_array_obj; const contiguity_data : Tcontiguity_array_obj;
          first_proc_year, last_proc_year : year_range);

      var EUWarTrap_file : EUWarTrap_file_type;
          EUWarTrap_record : EUWarTrap_file_record_type;
          sys_capability_data : Tsys_capability_array_obj;
          tau_data : Ttau_array_obj;
          s_data : Ts_array_obj;
          distance_data : Tdistance_array_obj;
          this_year_tau_data : Ttau_year_obj;
          this_year_s_data : TS_year_obj;
          num_partitions, years_per_partition, partition_start_year,
          partition_end_year, partition_loop, year_loop : integer;
          file_exists : boolean;
          ccode1, ccode2 : ccode_range;
          num_created : longint;
          eutrace : Ttrace_obj;

             { --------------------------  }

     procedure preprocess_for_EUWarTrap (const configuration : configuration_type; var files_exist : boolean);
       begin
         files_exist := false;
         if (((check_file_Exists (tau_file_name,
                             'Tau-b Scores')) and (user_selections.similarity_method = use_tau)) or
             ((check_file_Exists (s_file_name,
                             'S Scores')) and (user_selections.similarity_method = use_s))) then
            if check_file_Exists (sys_cap_file_name,
                           '% System Capabilities') then
                   files_exist := true;
       end;

             { --------------------------  }

      procedure calc_EUWarTrap_partition (var num_partitions, years_per_partition : integer;
                year1, year2 : year_range);
         {need to calculate amount of raw capability data that can fit.  }
         var mem_per_year, mem_to_use : longint;
             years_possible, num_years : longint;
         begin
            trace.enter('Entered calc_EUWarTrap_partition procedure');

           {Calculation will be, how many years of alliance data can fit into
             1/2 of avaialable memory.  This should leave LOTs of memory free for
             miscellaneous variables and functions. }
            {formula for # partitions is trunc (freemem to use) / (mem per partition)}
            {capability can be calculated between first_cap_year and last_cap_year}
            num_years := (year2 - year1 + 1);
            mem_to_use := mem_for_procedure - TSys_capability_array_obj_mem_overhead
                          - TTau_array_obj_mem_overhead - Tdistance_array_mem_overhead;
            mem_per_year := TSys_capability_array_obj_mem_per_year;
            case user_selections.similarity_method of
                          use_tau : mem_per_year := mem_per_year + TTau_array_obj_mem_per_year;
                          use_s : mem_per_year := mem_per_year + Ts_array_obj_mem_per_year;
                          else EUGeneError ('Error in setting s/tau option within eu war trap partition calculation:  similarity_method not use_tau or use_s.', 5, continue, error_log);
               end;  {case}

            years_possible := trunc ( mem_to_use / mem_per_year);
            years_per_partition := min (years_possible, num_years);
            if years_per_partition < 1 then
               begin
                  EUGeneError ('Error - less than one year per partition calculated in procedure!  '+
                          'Cannot run EUGENE with so little memory.',
                        5, stop, error_log);
               end;

            {Now, given that many years and calced # of years per partition, can see
             how many partitions are necessary. }
               num_partitions := (num_years div years_per_partition);
               if num_years mod years_per_partition > 0 then inc(num_partitions);
               {div in above calc will round down, so add 1 if > .  }


            if debug[3] then
            begin
              trace.message ('Information calculated on EUWarTrap_partition division:');
              trace.message ('Total years for processing: '+inttostr(num_years));
              trace.message ('Memory that can be used: '+inttostr(mem_to_use)+' bytes');
              trace.message ('Memory to be allocated per year: '+inttostr(mem_per_year)+' bytes');
              trace.message ('Years possible per partition: '+inttostr(years_possible));
              trace.message ('Number of partitions necessary: '+inttostr(num_partitions));
              trace.message ('Actual number of years per partition '+inttostr(years_per_partition));
            end;   {if debug[3]}
            if debug[2] then
            trace.message('Will run through ' + inttostr(num_partitions)+' EU War Trap partitions, ');
            trace.message(' with '+ inttostr(years_per_partition)+' years in each partition.');

            trace.exit ('Finished calc_EUWarTrap_partition procedure');
         end;

             { --------------------------  }

     begin
         distance_data := nil;
         sys_capability_data := nil;
         this_year_tau_data := nil;
         this_year_s_data := nil;
         tau_data := nil;
         s_data := nil;
         trace.enter('Entered compute_and_save_EUWarTrap procedure');
         eutrace :=  Ttrace_obj.init(trace.get_trace_level);
         {This procedure assumes that the provided 1st, last proc years are valid.}
         if first_proc_year > last_proc_year then
            switch_year (first_proc_year, last_proc_year);
         calc_euwarTrap_partition (num_partitions, years_per_partition,
                                 first_proc_year, last_proc_year);

         partition_start_year := first_proc_year;

            {may want to add some file checks here...}
            {check to make sure intermediat files exist}
         preprocess_for_EUWarTrap (configuration, file_exists);
         if file_exists then
         begin
            trace.message ('Computing and writing EU - War Trap version to output file');
            try
               try
                  case user_selections.similarity_method of
                    use_tau : assignFile (EUWarTrap_file, EUWarTrap_tau_file_name);
                    use_s : assignFile (EUWarTrap_file, EUWarTrap_s_file_name);
                    else EUGeneError ('Came into compute_and_save_EUWarTrap procedure without s/tau option set.  NOtify programmer.  Fatal error.  ',2,stop,error_log);
                  end;   {case}
                  rewrite (EUWarTrap_file);
                  num_created := 0;
                  distance_data := Tdistance_array_obj.init (locations_file_name, user_selections, contiguity_data);
                  for partition_loop := 1 to num_partitions do
                  begin
                     {need start + years -1 b/c want to include that 1st year in count.}
                     partition_end_year := min((partition_start_year + years_per_partition - 1), last_proc_year);
                     trace.message ('Partition from '+inttostr(partition_start_year)+' to '+inttostr(partition_end_year));
                     try
                        sys_capability_data := Tsys_capability_array_obj.init
                           (sys_cap_file_name, partition_start_year, partition_end_year);
                        case user_selections.similarity_method of
                          use_tau : tau_data := Ttau_array_obj.init (tau_file_name, partition_start_year, partition_end_year);
                          use_s : s_data := TS_array_obj.init (s_file_name, partition_start_year, partition_end_year);
                          else EUGeneError ('Error in setting s/tau option within eu war trap calculation:  similarity_method not use_tau or use_s.', 5, continue, error_log);
                        end;  {case}

                        For year_loop := partition_start_year to partition_end_year do
                          begin
                            try
                               case user_selections.similarity_method of
                                  use_tau : this_year_tau_data := Ttau_year_obj.init (year_loop, tau_data);
                                  use_s : this_year_s_data := Ts_year_obj.init (year_loop, s_data);
                                  else EUGeneError ('Error in setting s/tau_year within eu war trap calculation:  similarity_method not use_tau or use_s.', 5, continue, error_log);
                               end;

                               for ccode1 := min_ccode to max_ccode do
                                 if (nation_list.is_a_state (ccode1, year_loop)) then
                                    for ccode2 := min_ccode to max_ccode do
                                       if(nation_list.is_a_state (ccode2, year_loop)) then
                                       begin
                                          inc(num_created);
                                          EUWarTrap_record.year := year_loop;
                                          EUWarTrap_record.ccode1 := ccode1;
                                          EUWarTrap_record.ccode2 := ccode2;
                                          eutrace.tick('Executing Procedure: EU (War Trap), '+
                                            inttostr(year_loop), nation_list.get_dyad_years);
                                          EUWarTrap_record.EUWarTrap_rec.EUWarTrap :=
                                             eu_war_trap(ccode1, ccode2, year_loop, this_year_tau_data,
                                             this_year_s_data, sys_capability_data, distance_data);
                                          write(EUWarTrap_file, EUWarTrap_record);
                                       end;
                            finally
                               this_year_tau_data.free;
                               this_year_s_data.free;
                            end;
                          end;
                     finally
                        sys_capability_data.free;
                        tau_data.free;
                        s_data.free;
                        partition_start_year := partition_end_year + 1;
                     end;
                  end;  {for partition_loop}
               finally
                  CloseFile (EUWarTrap_file);
                  distance_data.free;
                  ShowMessage ('Computation of EU War Trap complete!');
               end;   {try... finally block}
            except
               on EUserInterrupt do raise;
               on EInOutError do
                   begin
                      FileErrorBox.maindo ('Error opening/writing to output file for EU War trap.',
                                           'File may be in use by another program, may be read-only, ',
                                           'or disk may be full.');
                      FileErrorBox.showmodal;
                      raise;
                   end;
            end;
         end;    {if file_exists}

         trace.exit ('Finished computing and writing EU War Trap, all dyads');
         eutrace.tickdone;
         eutrace.free;

      end;

         { --------------------------------------- }

   procedure compute_and_save_risk (const sys_cap_file_name, sequence_alliance_file_name,
             seq_file_name, dyadic_alliance_file_name,
             security_alliance_file_name, risk_file_name_for_compute_output,
             locations_file_name : TFileName; const nation_list : Tnation_array_obj;
             const contiguity_data : Tcontiguity_array_obj;
             first_proc_year, last_proc_year : year_range;
             raw_data_source : alliance_in_data_type; const risk_specs : risk_calculation_info_type);
       {note first, last proc year can't be const, even though they are not passed out,
        b/c they may be adjusted inside the proc temporarily.}
       {get system capability and alliance data, process it, save to an outfile.
        Exit without any data used or created left in memory.}

      type
           all_region_ptr = ^all_region;
           all_region = array[europe..globe] of record
               max_found, min_found : boolean;
               best_max, best_min : stored_alliance_list_type;
             end;
           best=array[ccode_index_range] of all_region_ptr;
           best_alliances_found_type = ^best;
           array_set = array[1..max_region_countries] of ccode_range;

      var sys_cap_data : Tsys_capability_array_obj;
          distance_data : Tdistance_array_obj;
          alliance_data : Talliance_array_obj;
          alliance_year_data : Talliance_year_obj;
          stored_risk_alliance_data : Trisk_stored_security_alliance_obj;

          num_partitions, years_per_partition, partition_loop : integer;
          partition_start_year, partition_end_year, year_loop : year_range;
          risk_file : risk_file_type_v2;
          risk_file_rec : ^risk_file_record_type_v2;
          risk_test_file, risk_int_scores_file : text;
          left, right, want_position : longint;
          left_year, right_year : year_range;
          flush_file_buffers : boolean;
          loop_region : region_type;
          files_exist, tempfileok : boolean;
          x,y : ccode_index_range;
          ccode : ccode_range;
          all_regions : all_region_ptr;
          num_created, num_needed : longint;
          Risk_main_trace : Ttrace_obj;
          previous_alliances : best_alliances_found_type;
          do_generation : boolean;
          SingleRiskYearForm: TSingleRiskYearForm;


            {------------------------}

      procedure calc_risk_partition (const first_proc_year, last_proc_year : year_range;
                var num_partitions, years_per_partition :integer);
         var mem_per_year, mem_to_use : longint;
             years_possible, num_years : longint;
         begin
            trace.enter('Entered calc_risk_partition procedure');
            {Calculation will be, how many years of various data can fit into
             1/2 of avaialable memory.  This should leave LOTs of memory free for
             miscellaneous variables and functions. }
            {formula for # partitions is trunc (freemem to use) / (mem per partition)}
            {calculate for years with capability and alliance data.}
            num_years := last_proc_year - first_proc_year + 1;
            mem_to_use := mem_for_procedure - Talliance_array_obj_mem_overhead -
                          Tsys_capability_array_obj_mem_overhead -
                          Tdistance_array_mem_overhead -
                          TEUWarTrap_array_obj_mem_overhead - TSec_array_obj_mem_overhead;
            mem_per_year := Talliance_array_obj_mem_per_year +
                          Tsys_capability_array_obj_mem_per_year + 
                          TEUWarTrap_array_obj_mem_per_year + TSec_array_obj_mem_per_year;
            years_possible := trunc ( mem_to_use / mem_per_year);
            years_per_partition := min (years_possible, num_years);
            if years_per_partition < 1 then
               begin
                  EUGeneError ('Error - less than one year per partition calculated in procedure!',
                        5, stop, error_log);
               end;

            {Now, given that many years and calced # of years per partition, can see
             how many partitions are necessary. }
            num_partitions := (num_years div years_per_partition);
            if num_years mod years_per_partition > 0 then inc(num_partitions);
              {div in above calc will round down, so add 1 if > .  }

            {Now, make sure all years per are about even, so don't do 120 years
             in one pass and only 50 in the next.}
            years_per_partition := (num_years div num_partitions);
            if num_years mod num_partitions > 0 then inc(years_per_partition);

               {div in above calc will round down, so add 1.  }
            if debug[3] then
            begin
              trace.message ('Information calculated on risk calculation partition division:');
              trace.message ('Total years for processing: '+inttostr(num_years));
              trace.message ('Memory that can be used: '+inttostr(mem_to_use)+' bytes');
              trace.message ('Memory to be allocated per year: '+inttostr(mem_per_year)+' bytes');
              trace.message ('Years possible per partition: '+inttostr(years_possible));
              trace.message ('Number of partitions necessary: '+inttostr(num_partitions));
              trace.message ('Actual number of years per partition '+inttostr(years_per_partition));
            end;   {if debug[3]}
            if debug[2] then
            trace.message('Will run through ' + inttostr(num_partitions)+' risk partitions, with '+
                     inttostr(years_per_partition)+' years in each partition.');

            trace.exit ('Finished calc_risk_partition procedure');

         end;  {risk partition}

          {--------------------------------------------------------------------}

     function computed_risk (const ccode : ccode_range; const year : year_range;
               const alliance_year_data_in_computed : Talliance_year_obj; const sys_cap_data : Tsys_capability_array_obj;
               const distance_data : Tdistance_array_obj;
               const aregion : region_type;
               var previous_alliances : best_alliances_found_type;
               var stored_risk_alliance_data : Trisk_stored_security_alliance_obj;
               num_created, num_needed: longint; const risk_specs :
               risk_calculation_info_type) : risk_rec_v2;

      {compute risk, following war trap revisited, for ccode in year re: aregion.}

         type max_min_type = (maximize, minimize);
              {actual_hypothetical_type = (actual, hypothetical);}
              region_count_type = 0..max_region_countries;
              two_region_count_type = 0..max_two_region_countries;
              vary_type = array[region_count_type] of ccode_range;
              EUvary_type = array[two_region_count_type] of ccode_range;
              ccode_vary_type = array[ccode_range] of region_count_type;
         var SumUji, SumUjiMax, SumUjiMin, Sum1, Sum2 : single;
             risk_sub : single;
             vary_ccodes : vary_type;
             EUvary_ccodes : EUvary_type;
             ccode_vary_loc : ccode_vary_type;
             top_vary_ccode_index : region_count_type;
             top_EU_ccode_index : two_region_count_type;
             ccode_loop : ccode_range;
             outstring : string;
             ccode_index_loop_test : integer;
             secSum : single;
             valid_return : boolean;
             risk_sec_data_print_1, risk_sec_data_print_2 : boolean;

         { -------------------------- }

      function security_position_from_alliance_data (const ccode : ccode_range;
         const year : year_range;
          const alliance_year_data_in_sec_calc : Talliance_year_obj; const for_region : region_type;
          const vary_ccodes : vary_type; const ccode_vary_loc : ccode_vary_type;
          const top_vary_ccode_index : region_count_type; const EUvary_ccodes : EUvary_type;
          const top_EU_ccode_index : two_region_count_type; const sys_capability_data :
          Tsys_capability_array_obj; const distance_data: Tdistance_array_obj;
          var valid_return_sec : boolean) : single;
          {given a listing of alliance data, returns the security position}

         var other_ccode_loop : integer;
             PartialSec : single;
             partial_list : array[1..max_region_countries] of single;


            procedure build_tau_table_fast (const ccode1, ccode2 : ccode_range; const ayear : year_range;
                      const alliance_year_for_tau : Talliance_year_obj;
                      var result_table : tau_table_type; var N : num_countries_range;
                      var nrows, ncolumns : tau_row_column_count_type;
                      const for_region : region_type;  const vary_ccodes : vary_type;
                      const top_vary_ccode_index :
                      region_count_type; const EUvary_ccodes : EUvary_type;
                      const top_EU_ccode_index : two_region_count_type);
                         {builds a tau table from this year, for this ccode pair, using states
                          in the specified region (which might be the globe).}
               var row, column : 0..5;   {these could go 1 beyond range of 1..4}
                   first_row, last_row, first_col, last_col : 0..4;
                   cc1value, cc2value : ccode_range;
                   partner_loc : ccode_index_range;
               begin
                  n := 0;
                     {Row, column 1 is defense pact, 2 is nonaggression, 3 is entente, 4 is no alliance}
                  for row := 1 to 4 do for column := 1 to 4 do result_table[row, column] := 0;
                 {NOTE:  This is currently not implemented, all states are included!!
                  As set up in the alliance data, 1 is a defense pact, 2 is neut, 3 is entente,
                  4 means no alliance but active in system (which I want to count in tau table)
                  and 0 means no allianc and inactive, which I don't want to count.}

                  {Want to include in table states that are involved in "for_region".
                   Ignore others.  But, since we may be calculating a dyad where a state is
                   not involved by the formula (since we are doing all dyads), also manually
                   include the two dyadic members as well.  So, table of US vs. Europeans in
                   1816 should have US as a table entry, so as to include US-US alliance.  }
                  {The vary_ccodes list has in it all states that are involved in region,
                   plus ccode.  So it should have ccode1 and ccode2. }
                  {original tau table build command was :
                     for partner_ccode := min_ccode to max_ccode do
                     if nation_list.is_a_state (partner_ccode, ayear) then
                      if ((nation_list.is_involved_in_region(partner_ccode, for_region, ayear)) or
                          (ccode1=partner_ccode) or (ccode2=partner_ccode)) then  }

                  for partner_loc := 1 to top_EU_ccode_index do
                     if  (nation_list.is_involved_in_region(EUvary_ccodes[partner_loc], for_region, ayear)) or
                         (ccode1=EUvary_ccodes[partner_loc]) or (ccode2=EUvary_ccodes[partner_loc]) then
                        begin
                           cc1value := alliance_year_for_tau.get_alliance_value(ccode1, EUvary_ccodes[partner_loc]);
                           cc2value := alliance_year_for_tau.get_alliance_value(ccode2, EUvary_ccodes[partner_loc]);
                           if (cc1value >= 1) and (cc1value <= 4) and (cc2value >= 1) and (cc2value <= 4) then
                              begin
                                 inc(result_table[cc1value, cc2value]);
                                 inc(n);
                              end;
                        end;

                     {Figure out dimensions of table.  If the table is 1xn or nx1, then tau
                      cannot be calculated.  }
                     first_row := 0;
                     row := 1;
                     while (row <= 4) and (first_row=0) do
                       begin
                          column := 1;
                          while (column <= 4) and (first_row=0) do
                             begin
                                if result_table[row,column] > 0 then first_row := row;
                                inc(column);
                             end;
                          inc(row);
                       end;

                     last_row := 0;
                     row := 4;
                     while (row >= 1) and (last_row=0) do
                       begin
                          column := 1;
                          while (column <= 4) and (last_row=0) do
                             begin
                                if result_table[row,column] > 0 then last_row := row;
                                inc(column);
                             end;
                          dec(row);
                       end;

                     first_col := 0;
                     column := 1;
                     while (column <= 4) and (first_col=0) do
                       begin
                          row := 1;
                          while (row <= 4) and (first_col=0) do
                             begin
                                if result_table[row,column] > 0 then first_col := column;
                                inc(row);
                             end;
                          inc(column);
                       end;


                     last_col := 0;
                     column := 4;
                     while (column >= 1) and (last_col=0) do
                       begin
                          row := 1;
                          while (row <= 4) and (last_col=0) do
                             begin
                                if result_table[row,column] > 0 then last_col := column;
                                inc(row);
                             end;
                          dec(column);
                       end;

                     ncolumns := last_col - first_col + 1;
                     nrows := last_row - first_row + 1;

                     if (ncolumns < 2) or (nrows < 2) then
                        begin
                           {Trace.message ('Calc tau table w/ r/c < 2 for '+
                              inttostr(ccode1)+' '+inttostr(ccode2)); }
                           {EUGeneError ('Calculated a tau table with row or column < 2 for dyad '+
                              inttostr(ccode1)+' '+inttostr(ccode2), 3, continue, error_log);  }
                        end;

               end;    {build tau table procedure}


                  { --------------------------------------- }

            function tau_from_alliance_year_fast (const ccode1, ccode2 : ccode_range;
                     const ayear : year_range; const alliance_data_year : Talliance_year_obj;
                     const region : region_type; const vary_ccodes : vary_type;
                     const top_vary_ccode_index : region_count_type; const EUvary_ccodes :
                     EUvary_type; const top_EU_ccode_index : two_region_count_type): real;
                   {given a year of alliance data, will compute tau for a specified dyad in that year.
                    This uses some sped-up methods involving prior selection of states for inclusion
                    in the calcs.}
                  var
                      N : num_countries_range;
                      nrows, ncolumns : tau_row_column_count_type;
                      table : tau_table_type;  {just an array 1..4, 1..4}
                  begin
                     build_tau_table_fast (ccode1, ccode2, ayear, alliance_data_year, table, N,
                                           nrows, ncolumns, region, vary_ccodes, top_vary_ccode_index,
                                           EUvary_ccodes, top_EU_ccode_index);
                     {Now, compute tau from the table}
                     if (nrows < 2) or (ncolumns < 2) then
                        begin     {cannot calculate tau in this case; set to 0}
                           if debug[10] then EUGeneError ('Cannot compute tau for '+inttostr(ccode1)+
                              ' vs. '+inttostr(ccode2)+' in '+inttostr(ayear)+
                              '. Contingency table is '+inttostr(nrows)+' x '+inttostr(ncolumns)+'.  TAU set to 0.',
                              0, continue, error_log);
                           tau_from_alliance_year_fast := missing_value;
                        end
                     else
                        tau_from_alliance_year_fast := tau_from_table (table, N, nrows, ncolumns);
                  end;

         { --------------------------------------------------------------- }

         function EU_War_Trap_calculated (const ccodei, ccodej : ccode_range; const year : year_range;
                   const alliance_data_year_in_EU : Talliance_year_obj; const sys_capability_data :
                   Tsys_capability_array_obj; const distance_data: Tdistance_array_obj;
                   const vary_ccodes : vary_type; const top_vary_ccode_index : region_count_type;
                   const EUvary_ccodes : EUvary_type; const top_EU_ccode_index : two_region_count_type) : single;
                   {Figures the EU of ccode1 vs. ccode2 following procedures and methods of The
                    War Trap.  This version creates taus from scratch, b/c it will take
                    hypothetical alliance structures.}
            var adj_capij, adj_capii, adj_capji, adj_capjj, adj_capkj, adj_capki : single;
                Uij, Uiki, Uikj, Pi, Pjk, Pik : single;
                EU_bilateral, EU_multilateral_no_uncertainty, EU_multilateral_uncertainty_risk_averse,
                   EU_multilateral_uncertainty_risk_acceptant : single;
                ccodek : ccode_range;
                ccodekloop : integer;
                conflict_region : region_type;

            begin
               conflict_region := relevant_region(ccodei, ccodej, year);
               {Uij := tau_from_alliance_year (ccodei, ccodej, year, alliance_data_year_in_EU,
                                              conflict_region);  }
               case user_selections.similarity_method of
                     use_tau : Uij := tau_from_alliance_year_fast (ccodei, ccodej, year, alliance_data_year_in_EU,
                          conflict_region, vary_ccodes, top_vary_ccode_index,
                          EUvary_ccodes, top_EU_ccode_index);
                     use_s : Uij := s_from_alliance_year (ccodei, ccodej, year, alliance_data_year_in_EU,
                             conflict_region, user_selections.s_weighting, sys_capability_data);
                     else EUGeneError ('Error in setting s/tau option within risk EU_War_trap_Calculated proc:  similarity_method not use_tau or use_s.', 5, continue, error_log);
                  end;   {case}
               if ((sys_capability_data.get_syscap (ccodei, year) = missing_value) or
                   (sys_capability_data.get_syscap (ccodej, year) = missing_value) or
                   (Uij = missing_value) ) then
                 begin
                    EU_War_Trap_calculated := missing_value;
                 end
               else
                 begin    {no base capabilities are missing, so do full calculations}
                   if ((user_selections.compute_this = compute_single_risk) and risk_sec_data_print_2) then
                     begin
                       outstring := '    EU('+inttostr(ccodei)+', '+inttostr(ccodej)+'): Bilateral info ';
                       writeln (risk_int_scores_file, outstring);
                       outstring := '      Uij'+ chr(9)+ 'AdjCapij'+ chr(9)+ 'AdjCapjj'+ chr(9)+
                                    'Pi'+ chr(9);
                       writeln (risk_int_scores_file, outstring);
                     end;
                   adj_capij := adj_cap(ccodei, ccodej, year, sys_capability_data, distance_data);
                   adj_capii := adj_cap(ccodei, ccodei, year, sys_capability_data, distance_data);
                   adj_capji := adj_cap(ccodej, ccodei, year, sys_capability_data, distance_data);
                   adj_capjj := adj_cap(ccodej, ccodej, year, sys_capability_data, distance_data);

                   if (adj_capij + adj_capjj) = 0 then
                      Pi := 0.5
                   else Pi := adj_capij / (adj_capij + adj_capjj);
                   EU_bilateral := Pi * (1-Uij) + (1-Pi)*(Uij-1);
                   EU_multilateral_no_uncertainty := 0;

                   if ((user_selections.compute_this = compute_single_risk) and risk_sec_data_print_2) then
                     begin
                       outstring := '    '+realtostring(Uij, 7, 4) + chr(9)+ realtostring(Adj_Capij, 7, 4)+ chr(9)+
                                 realtostring(Adj_Capjj, 7, 4)+ chr(9)+ realtostring(Pi, 6, 4)+ chr(9);
                       writeln (risk_int_scores_file, outstring);
                     end;

                   {The relevant states k are those involved in the region (k,i) or (k,j).
                   The EUvary List has those in either region. }
                   {original code was: if a state, if ccodek <> ccodei) and (ccodek <> ccodej)) and
                    in relevant region then...}
                   if ((user_selections.compute_this = compute_single_risk) and risk_sec_data_print_2) then
                     begin
                       outstring := '    EU('+inttostr(ccodei)+', '+inttostr(ccodej)+
                                 '): Multilateral Info = Third party k contributions to i vs. j';
                       writeln (risk_int_scores_file, outstring);
                       outstring := '    k'+ chr(9)+ 'AdjCapkj'+ chr(9)+ 'AdjCapki'+ chr(9)+
                                          'Pik'+ chr(9)+ 'Pjk'+ chr(9)+ 'Uiki' + chr(9)+ 'Uikj' ;
                       writeln (risk_int_scores_file, outstring);
                     end;
                   for ccodekloop := 1 to top_EU_ccode_index do
                      if ((EUvary_ccodes[ccodekloop] <> ccodei) and (EUvary_ccodes[ccodekloop] <> ccodej)) and
                         (nation_list.is_involved_in_region(EUvary_ccodes[ccodekloop], conflict_region, year)) then
                        begin
                          ccodek := EUvary_ccodes[ccodekloop];
                          adj_capkj := adj_cap(ccodek, ccodej, year, sys_capability_data, distance_data);
                          adj_capki := adj_cap(ccodek, ccodei, year, sys_capability_data, distance_data);
                          if (adj_capij + max(adj_capkj, adj_capki) + adj_capjj)=0 then
                             Pik := 0.5
                          else Pik := (adj_capij + max(adj_capkj, adj_capki)) /
                                 (adj_capij + max(adj_capkj, adj_capki) + adj_capjj);
                          if (adj_capji + max(adj_capki, adj_capkj) + adj_capii)=0 then
                             pjk := 0.5
                          else Pjk := (adj_capji + max(adj_capki, adj_capkj)) /
                                 (adj_capji + max(adj_capki, adj_capkj) + adj_capii);
                          {Uiki := tau_from_alliance_year (ccodek, ccodei, year, alliance_data_year_in_EU,
                                              relevant_region(ccodek, ccodei, year));  }
                          {This calc works using the sub-regional list above.  need Uki, Ukj here, so tau will
                          involve states in region(ki) or region(kj).  But, since k is in either region i or j,
                          the resulting region must be either region i or j.  All states in i and j are on the
                          list, so the tau table can add just them and be correct.}

                          case user_selections.similarity_method of
                                 use_tau : Uiki := tau_from_alliance_year_fast (ccodek, ccodei, year, alliance_data_year_in_EU,
                                     relevant_region(ccodek, ccodei, year), vary_ccodes,
                                     top_vary_ccode_index, EUvary_ccodes, top_EU_ccode_index);
                                 use_s : Uiki := s_from_alliance_year (ccodek, ccodei, year, alliance_data_year_in_EU,
                                         relevant_region(ccodek, ccodei, year), user_selections.s_weighting, sys_capability_data);
                                 else EUGeneError ('Error in setting s/tau option within risk EU_War_trap_Calculated proc:  similarity_method not use_tau or use_s.', 5, continue, error_log);
                              end;   {case}

                          case user_selections.similarity_method of
                                 use_tau : Uikj := tau_from_alliance_year_fast (ccodek, ccodej, year, alliance_data_year_in_EU,
                                     relevant_region(ccodek, ccodej, year), vary_ccodes,
                                     top_vary_ccode_index, EUvary_ccodes, top_EU_ccode_index);
                                 use_s : Uikj := s_from_alliance_year (ccodek, ccodej, year, alliance_data_year_in_EU,
                                         relevant_region(ccodek, ccodej, year), user_selections.s_weighting, sys_capability_data);
                                 else EUGeneError ('Error in setting s/tau option within risk EU_War_trap_Calculated proc:  similarity_method not use_tau or use_s.', 5, continue, error_log);
                              end;   {case}
                          if (Uiki <> missing_value) and (Uikj <> missing_value) and
                                (sys_capability_data.get_syscap (ccodek, year) <> missing_value) then
                             EU_multilateral_no_uncertainty := EU_multilateral_no_uncertainty +
                                                               ((Pik + Pjk -1) * (Uiki-Uikj));
                          if ((user_selections.compute_this = compute_single_risk) and risk_sec_data_print_2) then
                            begin
                              outstring := '    '+inttostr(EUvary_ccodes[ccodekloop])+ chr(9)+ realtostring(Adj_Capkj, 7, 4)+ chr(9)+
                                        realtostring(Adj_Capki, 7, 4)+ chr(9)+ realtostring(Pik, 7, 4)+ chr(9)+
                                        realtostring(Pjk, 7, 4)+ chr(9)+ realtostring(Uiki, 7, 4) + chr(9)+
                                        realtostring(Uikj, 7, 4) ;
                              writeln (risk_int_scores_file, outstring);
                           end;
                        end;            {for ccodeloop}

                   EU_War_Trap_calculated := EU_bilateral + EU_multilateral_no_uncertainty;
                   if ((user_selections.compute_this = compute_single_risk) and risk_sec_data_print_2) then
                      begin
                        outstring := '    EU_bilateral= '+ chr(9)+ realtostring(EU_bilateral, 7, 4)+
                                     chr(9)+ 'EU_multilateral= '+ realtostring(EU_multilateral_no_uncertainty, 7, 4)+
                                     chr(9) + 'Total= '+realtostring(result, 7, 4);
                        writeln (risk_int_scores_file, outstring);
                        writeln (risk_int_scores_file);
                     end;
               end;       {not missing either capability}

            end;   {function eu_war_trap_calculated}

                 {   ----------------------       }

         begin    {security calculated from alliance data}

            Result := 0;
            valid_return_sec := false;

            if ((user_selections.compute_this = compute_single_risk) and risk_sec_data_print_1) then
               begin
                 writeln (risk_int_scores_file);
                 outstring := 'Contributions to security value, ';
                 outstring := outstring + inttostr(ccode)+ ' '+inttostr(year);
                 writeln (risk_int_scores_file, outstring);
               end;

            for other_ccode_loop := 1 to top_vary_ccode_index do
             {sum EU of other states in this region, since risk is computed
              by region.  For global calc, all states will appear as involved.}
               if vary_ccodes[other_ccode_loop] <> ccode then
                  begin
                     PartialSec := EU_War_Trap_calculated(vary_ccodes[other_ccode_loop],
                            ccode, year, alliance_year_data_in_sec_calc, sys_capability_data,
                            distance_data, vary_ccodes, top_vary_ccode_index, EUvary_ccodes,
                            top_EU_ccode_index);
                     if (PartialSec <> missing_value) then
                        begin
                           Result := Result + PartialSec;
                           valid_return_sec := true;
                        end;
                     if ((user_selections.compute_this = compute_single_risk) and risk_sec_data_print_1) then
                        begin
                          partial_list[other_ccode_loop] := partialsec;
                        end;
                  end;
            security_position_from_alliance_data := Result;

            if ((user_selections.compute_this = compute_single_risk) and risk_sec_data_print_1) then
               begin
                 outstring := 'Contributions to security value, ';
                 outstring := outstring + inttostr(ccode)+ ' '+inttostr(year)+ ', summary';
                 writeln (risk_int_scores_file, outstring);
                 outstring := 'Values are EUji, where "Contributor" is j and i is '+ inttostr(ccode);
                 writeln (risk_int_scores_file, outstring);
                 outstring := 'Loop'+ chr(9)+ 'Contributor'+ chr(9)+ 'PartialSec';
                 writeln (risk_int_scores_file, outstring);
                 for other_ccode_loop := 1 to top_vary_ccode_index do
                    begin
                       if vary_ccodes[other_ccode_loop] = ccode then
                          begin
                             outstring := inttostr(other_ccode_loop) + chr(9)+
                                          inttostr(vary_ccodes[other_ccode_loop]) + chr(9) +
                                          'n/a' + chr(9);
                             writeln (risk_int_scores_file, outstring);
                          end
                       else
                          begin
                             outstring := inttostr(other_ccode_loop) + chr(9)+
                                          inttostr(vary_ccodes[other_ccode_loop]) + chr(9) +
                                          realtostring(partial_list[other_ccode_loop], 7, 4) + chr(9);
                             writeln (risk_int_scores_file, outstring);
                          end;
                    end;
                 outstring := 'Final Security: ' + realtostring(result, 7, 4) +chr(9);
                 writeln (risk_int_scores_file, outstring);
                 writeln (risk_int_scores_file);
               end;

         end;     {security calculated from alliance data}

         {------------------------------------------------------------}

         procedure get_random_walk_optimum (var temp_alliance_year_data : Talliance_year_obj;
              var current_alliance_bits : stored_alliance_list_type; var best_security : single;
              current_set, full_set : array_set; current_set_count, full_set_count : integer;
              initial_security : single; max_min_sum : max_min_type);

                   {from a starting point in temp_alliance_year_data, does a random walk to
                    the first optimum it finds.  Returns the security value, bits, and alliance
                    year data set to reflect that optimum.}
             var loop_count : integer;
                 original_alliance_value, new_alliance_value : alliance_value_type;
                 New_security, current_security : single;
                 other_index : ccode_index_range;
                 other_ccode : ccode_range;
                 valid_return_st : boolean;
             begin
                  current_security := initial_security;
                  {This is the main search loop over alternative alliances.  Within this loop,
                   try varying alliances with other states until a better pattern is found.}
                  {Initially, start and new data are both set to original values.}
                  loop_count := 0;
                  repeat   {repeat is until go thru all states with no change in sec.}
                     loop_count := loop_count + 1;

                     {pick a country to change; remove it from the list}
                     other_index := random (current_set_count-1)+1;
                     other_ccode := current_set[other_index];
                     current_set[other_index] := current_set[current_set_count];
                     dec(current_set_count);

                     original_alliance_value :=
                             temp_alliance_year_data.get_alliance_value(ccode, other_ccode);

                     for new_alliance_value := 1 to 4 do
                        if new_alliance_value <> original_alliance_value then
                           begin
                                {Change it}
                              temp_alliance_year_data.set_alliance_value(
                                ccode, other_ccode, new_alliance_value );
                              temp_alliance_year_data.set_alliance_value(
                                other_ccode, ccode, new_alliance_value );

                             {since this hogs the system, check for Windows operations}
                              Application.ProcessMessages;

                            {Now figure out new security position given this change}
                              New_security := security_position_from_alliance_data (ccode, year,
                                             temp_alliance_year_data, aregion,
                                             vary_ccodes, ccode_vary_loc, top_vary_ccode_index,
                                             EUvary_ccodes, top_EU_ccode_index, sys_cap_data,
                                             distance_data, valid_return_st);

                                {If not valid, don't store it, undo it.  }
                              if (valid_return_st)  and
                                 ( ((max_min_sum = maximize) and (New_security > current_security)) or
                                   ((max_min_sum = minimize) and (New_security < current_security))  )
                               then
                                    begin
                                        {keep this change}
                                       current_alliance_bits[ccode_index.index(other_ccode)] :=
                                                        new_alliance_value;
                                       current_security := New_security;
                                       {reset country list to have all members}
                                       current_set := full_set;
                                       current_set_count := full_set_count;
                                    end
                               else     {invalid, or not better}
                                   begin
                                       {undo the change.}
                                       temp_alliance_year_data.set_alliance_value(
                                         ccode, other_ccode, original_alliance_value );
                                       temp_alliance_year_data.set_alliance_value(
                                         other_ccode, ccode, original_alliance_value );
                                       {Don't reset the country list, leave this one out}
                                   end;
                           end;   {for new_alliance_value loop}


                     {continue looping until all countries have been tried without any change.}
                     {the abs means this should work whether I'm maximizing or minimizing the value.}
                     {The count is very high b/c you could potentially be almost fully through the
                      list (top_vary_ccode_index countries) before finding a change and having
                      to reset and do it all over.  Assuming it's purely downhill, top^2/2 is most
                      possible changes.  But, since I check each country even after it's changed,
                      since I cannot assume it won't change again (essentially because pij may
                      change in landscape terms).  }
                  until (current_set_count = 0) or
                        (loop_count > (top_vary_ccode_index*top_vary_ccode_index));
                  if (loop_count > top_vary_ccode_index*top_vary_ccode_index) then
                     begin
                        Trace.message ('Exited risk optimize loop b/c # of changes exceeded states^2 in region,'
                               +' for state '+inttostr(ccode)+' in '+inttostr(year)+' in region '+inttostr(ord(aregion)));
                     end;
                     {Finally, set main return value}
                  best_security := current_security;

             end;    {proc get rand walk optimum}


            {  -------------------------------------------------- }

      function security_position_best_worstGA (const ccode : ccode_range; year : year_range;
            const aregion : region_type; const original_alliance_year_data : Talliance_year_obj;
            const sys_cap_data : Tsys_capability_array_obj;
            const distance_data : Tdistance_array_obj;
            const max_min_sum : max_min_type; const vary_ccodes : vary_type;
            const ccode_vary_loc : ccode_vary_type; const top_vary_ccode_index : region_count_type;
            const EUvary_ccodes : EUvary_type; const top_EU_ccode_index : two_region_count_type;
            var previous_alliances : best_alliances_found_type;
            var stored_risk_alliance_data : Trisk_stored_security_alliance_obj;
            num_created, num_needed: longint;
            const risk_specs : risk_calculation_info_type): single;

            {figure best or worst sec portfolio for ccode _in this region_, and so
             considering other actors _involved in this region_ also.  }

            type ccodeptr = ^ccode_range;
                alliance_year_data_array = array[1..max_in_ga_pop] of Talliance_year_obj;
                data_table_type = array[1..max_in_ga_pop] of single;
            var ustart, New_USum, Usum_change : single;
                current_alliance_bits, original_alliance_bits : stored_alliance_list_type;
                display_alliance_bits : array[1..max_region_countries] of alliance_value_type;
                loop, loop_count : integer;
                tau_update_ccode_loop1, tau_update_ccode_loop2 : region_count_type;
                ccode_index_loop : ccode_index_range;  {this must have range bigger than regiontype}
                outstring, outreal : string;
                optimize_trace : Ttrace_obj;
                {year_loop : year_range;}

                new_alliance_year_data, old_alliance_year_data : alliance_year_data_array;
                security_data, fitness_table : data_table_type;
                pop_element : 1..max_in_ga_pop;
                ally_value : alliance_value_type;
                generations_stable : integer;
                sum_fitness : single;
                valid_return_ga : boolean;
                sec_calc, optimum_random_security_found : single;

                full_set, current_set : array_set;
                full_set_count, current_set_count : integer;

               { -----------------------  }

            procedure mutate;
               var pop_element : 1..max_in_ga_pop;
                   ccode_item_loop : integer;
                   ally_value : 1..4;
               begin
               {mutate those that aren't specified as to keep.}
                  for pop_element := risk_specs.num_from_previous_to_keep + 1 to
                                     risk_specs.num_in_pop do
                     for ccode_item_loop := 1 to top_vary_ccode_index do
                        begin
                           if random < risk_specs.mutate_probability then
                             begin
                               ally_value := random(3)+1;
                               new_alliance_year_data[pop_element].set_alliance_value(
                                   ccode, vary_ccodes[ccode_item_loop], ally_value );
                               new_alliance_year_data[pop_element].set_alliance_value(
                                   vary_ccodes[ccode_item_loop], ccode, ally_value );
                             end;
                        end;
               end;

               { -----------------------  }

               procedure move_to_position (which, num_in_pop : integer;
                         var fitness_table : data_table_type);
                  {just move nth best in fitness table into nth spot from top.
                   Does this by just swapping the pointers to the objects}
                  var best_sort : record
                          id : integer;  value : single;
                        end;
                      temp_alliance : Talliance_year_obj;
                      temp_security, temp_fitness : single;
                      pop_element : integer;
                  begin
                     best_sort.id := which;
                     best_sort.value := fitness_table[which];
                     for pop_element := which+1 to num_in_pop do
                        if fitness_table[pop_element] > best_sort.value then
                           begin
                              best_sort.id := pop_element;
                              best_sort.value := fitness_table[pop_element];
                           end;
                     if best_sort.id <> which then
                        {found a different pattern for this spot.  Need to switch
                         records, and data in security and fitness arrays.}
                       begin
                          temp_alliance := new_alliance_year_data[which];
                          new_alliance_year_data[which] := new_alliance_year_data[best_sort.id];
                          new_alliance_year_data[best_sort.id] := temp_alliance;
                          temp_fitness := fitness_table[which];
                          fitness_table[which] := fitness_table[best_sort.id];
                          fitness_table[best_sort.id] := temp_fitness;
                          temp_security := security_data[which];
                          security_data[which] := security_data[best_sort.id];
                          security_data[best_sort.id] := temp_security;
                       end;
                  end;     {move to position}

               { -----------------------  }

               procedure Make_fitness_table (var fitness_table : data_table_type;
                         var fitness_sum : single; security_data : data_table_type;
                         num_in_pop : integer; scale_factor : single);
                   {need to do a couple of things for scaling.  If minimizing, everything will
                    get *-1.  And, if minimum is < 0, will need to add that value to move up
                    so bottom = 0.  Then, scale by 1.5.}
                  var element : integer;
                      add, min : single;
                  begin
                     {first, if necessary, reverse direction of func I'm minimizing
                      so that it can be maximized}
                     if max_min_sum = minimize then
                        for element := 1 to num_in_pop do
                            fitness_table[element] := security_data[element] * -1
                        else for element := 1 to num_in_pop do
                            fitness_table[element] := security_data[element];

                       {find minimum fitness value for scaling}
                     min := fitness_table[1];
                     for element := 2 to num_in_pop do
                         if fitness_table[element] < min then min := fitness_table[element];

                       {add minimum to fitness to bring it up to 0.  Also scale.}
                     if min < 0 then
                        begin
                           add := (min * -1) + 0.001;   {add .001 to avoid any rounding errors}
                           for element := 1 to num_in_pop do
                              fitness_table[element] := (fitness_table[element] + add) * scale_factor;
                        end
                        else
                           for element := 1 to num_in_pop do
                              fitness_table[element] := fitness_table[element] * scale_factor;

                       {Now, compute sum of fitness}
                     fitness_sum := 0;
                     for element := 1 to num_in_pop do
                         fitness_sum := fitness_sum + fitness_table[element];

                  end;   {make fitness table}

               { -----------------------  }

            procedure CrossoverAndGenerate (num_in_pop : integer;
                      old_alliance_year_data : alliance_year_data_array;
                      var new_alliance_year_data : alliance_year_data_array;
                      fitness_sum : single; var fitness_table : data_table_type );
               var element1, element2, pop_element : integer;
                   selection_table : array[1..max_in_ga_pop] of integer;

               procedure combine (from1, from2, new1, new2 : integer);
                  var split_point, chromosome : integer;
                  begin
                     {random(n) returns 0 to n-1}
                     {New1 = first half(old1) + second half(old2)}
                     {New2 = first half(old2) + second half(old1)}
                     {set first half}
                     if (new1 <= num_in_pop) and (new2 <= num_in_pop) then
                        begin
                           split_point := random(top_vary_ccode_index) + 1;
                           for chromosome := 1 to split_point do
                              begin
                                 new_alliance_year_data[new1].set_alliance_value(
                                       ccode, vary_ccodes[chromosome],
                                    old_alliance_year_data[from1].get_alliance_value(
                                       ccode, vary_ccodes[chromosome]));
                                 new_alliance_year_data[new1].set_alliance_value(
                                       vary_ccodes[chromosome], ccode,
                                    old_alliance_year_data[from1].get_alliance_value(
                                       vary_ccodes[chromosome], ccode));

                                 new_alliance_year_data[new2].set_alliance_value(
                                       ccode, vary_ccodes[chromosome],
                                    old_alliance_year_data[from2].get_alliance_value(
                                       ccode, vary_ccodes[chromosome]));
                                 new_alliance_year_data[new2].set_alliance_value(
                                       vary_ccodes[chromosome], ccode,
                                    old_alliance_year_data[from2].get_alliance_value(
                                       vary_ccodes[chromosome], ccode));
                              end;

                           {Now set 2nd half of the new alliance patterns}
                           for chromosome := split_point to top_vary_ccode_index do
                              begin
                                 new_alliance_year_data[new1].set_alliance_value(
                                       ccode, vary_ccodes[chromosome],
                                    old_alliance_year_data[from2].get_alliance_value(
                                       ccode, vary_ccodes[chromosome]));
                                 new_alliance_year_data[new1].set_alliance_value(
                                       vary_ccodes[chromosome], ccode,
                                    old_alliance_year_data[from2].get_alliance_value(
                                       vary_ccodes[chromosome], ccode));

                                 new_alliance_year_data[new2].set_alliance_value(
                                       ccode, vary_ccodes[chromosome],
                                    old_alliance_year_data[from1].get_alliance_value(
                                       ccode, vary_ccodes[chromosome]));
                                 new_alliance_year_data[new2].set_alliance_value(
                                       vary_ccodes[chromosome], ccode,
                                    old_alliance_year_data[from1].get_alliance_value(
                                       vary_ccodes[chromosome], ccode));
                              end;
                        end
                     else     {one is out of range}
                        begin
                           if (new1 <= num_in_pop) then
                              new_alliance_year_data[new1].copy (old_alliance_year_data[1]);
                           if (new2 <= num_in_pop) then
                              new_alliance_year_data[new2].copy (old_alliance_year_data[1]);
                        end;
                  end;

               procedure BuildSelectionTable;
                  var element, insert, add_at : integer;
                      string_table : array[1..max_in_ga_pop] of integer;
                  begin
                     {for each pattern, calculate the # of expected members of the new pop}
                     {This will be an easy variant on the deterministic sampling scheme discussed
                      in Goldberg (p121).  Pop ends up being a weighted sum of best.}

                     {First, figure out the expected integer # of each pattern}
                     for element := 1 to num_in_pop do
                        string_table[element] := trunc((fitness_table[element]/fitness_sum) *
                                                (num_in_pop-risk_specs.num_from_previous_to_keep));

                     {For each of the original strings, put that many into selection_table}
                     add_at := 0;
                     for element := 1 to num_in_pop do
                        begin
                           for insert := 1 to string_table[element] do
                              begin
                                 inc(add_at);
                                 selection_table[add_at] := element;
                              end;
                        end;
                     {Now, may have some spaces left to fill.  Just pick randomly for them.}
                     if add_at < num_in_pop then
                        for element := add_at + 1 to num_in_pop do
                           selection_table[element] := (random (num_in_pop - 1) +1);
                  end;

               function pick (top : integer) : integer;
                   {returns integer of a parent, random weighted by fitness}
                  begin
                      {random generates 0 to n.  So, need to generate 0 to b-1.
                       But, want to start at a, so need to gen 0 to b-1-a.
                       Then add +1 and +a to increment up to correct range.}
                       {pick := random ((b-1-a))+1+a;}

                     {Use selection table, which keeps list of higher fitness strings
                      for weighted random selection}
                     pick := selection_table[(random (top - 1) + 1)];
                  end;

               begin       {Crossover and Generate}
                  {create new pop in new from old}
                  {First, make the first in new into best from old}
                  {it should be recommended to users to set this at 3, to keep
                   the best from last iteration, the prvious alliance from prior year,
                   and the input value from the saved data set}
                  for pop_element := 1 to risk_specs.num_from_previous_to_keep do
                     begin
                        move_to_position (pop_element,risk_specs.num_in_pop, fitness_table);
                     end;

                  {Now, for others, randomly (weighted) pick parents from old,
                   merge into new.}
                  BuildSelectionTable;
                  pop_element := risk_specs.num_from_previous_to_keep + 1;
                  while pop_element <= num_in_pop do
                     begin
                        element1 := pick (num_in_pop); {element between 1st and last}
                        element2 := pick (num_in_pop);
                        combine (element1, element2, pop_element, pop_element + 1);
                        pop_element := pop_element + 2;
                     end;
               end;

               {  -------------------------  }

            begin                         {main proc sec_position_best_worst_ga}
               optimize_trace := nil;
               try
                  if max_min_sum = maximize then outstring := ('Maximizing (worsening) security (GA), ')
                     else outstring := ('Minimizing (bettering) security (GA), ');
                  outstring := outstring + inttostr(ccode)+',' + inttostr(year)+', '+inttostr(ord(aregion));
                  optimize_trace := Ttrace_obj.init(trace.get_trace_level);
                  optimize_trace.tick (outstring, 0);
                  optimize_trace.tickdone;

                  {The one data structure related to the original underlying data
                   that will be changed here is the alliance year data.  So need to create
                   that each time this procedure is called.}
                  for pop_element := 1 to risk_specs.num_in_pop do
                     begin
                        new_alliance_year_data[pop_element] := nil;
                        new_alliance_year_data[pop_element] := Talliance_year_obj.init;
                        new_alliance_year_data[pop_element].allocate_full(year);
                        new_alliance_year_data[pop_element].copy (original_alliance_year_data);
                        old_alliance_year_data[pop_element] := nil;
                        old_alliance_year_data[pop_element] := Talliance_year_obj.init;
                        old_alliance_year_data[pop_element].allocate_full(year);
                        old_alliance_year_data[pop_element].copy (new_alliance_year_data[pop_element]);
                     end;


                  {In initial population, of starting points I want 1 to be the
                    original pattern.  Leave that in new_alliance_year_data[1].
                  {Want a 2nd starting pattern to be the best found previously, if
                    we've found a previous best for this state.
                   Want a 3rd to be the saved best/worst optinum in security_alliance_data}
                  if (max_min_sum = maximize) then
                     begin
                        {first set based on previous alliance data}
                        if (previous_alliances^[ccode_index.index(ccode)]^[aregion].max_found = true) then
                           begin
                              for tau_update_ccode_loop1 := 1 to top_vary_ccode_index do
                                 if vary_ccodes[tau_update_ccode_loop1] <> ccode then
                                    begin
                                       ally_value := previous_alliances^[ccode_index.index(ccode)]^[aregion].
                                                  best_max[ccode_index.index(vary_ccodes[tau_update_ccode_loop1])];
                                       old_alliance_year_data[2].set_alliance_value(
                                          ccode, vary_ccodes[tau_update_ccode_loop1], ally_value);
                                       old_alliance_year_data[2].set_alliance_value(
                                          vary_ccodes[tau_update_ccode_loop1], ccode, ally_value);
                                    end;
                           end;
                        {now set to values from security _alliance_data}
                        for tau_update_ccode_loop1 := 1 to top_vary_ccode_index do
                           if vary_ccodes[tau_update_ccode_loop1] <> ccode then
                              begin
                                 ally_value := stored_risk_alliance_data.get_best_alliance(ccode, vary_ccodes[tau_update_ccode_loop1], year, aregion);
                                 old_alliance_year_data[3].set_alliance_value(
                                    ccode, vary_ccodes[tau_update_ccode_loop1], ally_value);
                                 old_alliance_year_data[3].set_alliance_value(
                                    vary_ccodes[tau_update_ccode_loop1], ccode, ally_value);
                              end;

                     end
                  else if (max_min_sum = minimize) then
                     begin
                        if (previous_alliances^[ccode_index.index(ccode)]^[aregion].min_found = true) then
                           begin
                              for tau_update_ccode_loop1 := 1 to top_vary_ccode_index do
                                 if vary_ccodes[tau_update_ccode_loop1] <> ccode then
                                    begin
                                       ally_value := previous_alliances^[ccode_index.index(ccode)]^[aregion].
                                                  best_min[ccode_index.index(vary_ccodes[tau_update_ccode_loop1])];
                                       old_alliance_year_data[2].set_alliance_value(
                                          ccode, vary_ccodes[tau_update_ccode_loop1], ally_value);
                                       old_alliance_year_data[2].set_alliance_value(
                                          vary_ccodes[tau_update_ccode_loop1], ccode, ally_value);
                                    end;
                           end;
                        {now set to values from security _alliance_data}
                        for tau_update_ccode_loop1 := 1 to top_vary_ccode_index do
                           if vary_ccodes[tau_update_ccode_loop1] <> ccode then
                              begin
                                 ally_value := stored_risk_alliance_data.get_worst_alliance(ccode, vary_ccodes[tau_update_ccode_loop1], year, aregion);
                                 old_alliance_year_data[3].set_alliance_value(
                                    ccode, vary_ccodes[tau_update_ccode_loop1], ally_value);
                                 old_alliance_year_data[3].set_alliance_value(
                                    vary_ccodes[tau_update_ccode_loop1], ccode, ally_value);
                              end;
                     end
                  else EUGeneError ('Max Min sum has invalid value at beginning of sec-best-worst procedure', 5, stop, error_log);


                  {set rest of pop to a random pattern.
                   1 is original, 2 is from previous_alliances, 3 is from saved best/worst}
                  for pop_element := 4 to risk_specs.num_in_pop do
                     for tau_update_ccode_loop1 := 1 to top_vary_ccode_index do
                        if vary_ccodes[tau_update_ccode_loop1] <> ccode then
                           begin
                              {0 to 3, + 1}
                              ally_value := random(3)+1;
                              old_alliance_year_data[pop_element].set_alliance_value(
                                 ccode, vary_ccodes[tau_update_ccode_loop1], ally_value);
                              old_alliance_year_data[pop_element].set_alliance_value(
                                 vary_ccodes[tau_update_ccode_loop1], ccode, ally_value);
                           end;

                  {Store an array of alliance bits that have original alliance values, not
                   modified for previous loops or anything else.  }
                  for ccode_index_loop := min_ccode_index to max_ccode_index do
                     original_alliance_bits[ccode_index_loop] := 4;
                  for ccode_index_loop := 1 to top_vary_ccode_index do
                     original_alliance_bits[ccode_index.index(vary_ccodes[ccode_index_loop])] :=
                     original_alliance_year_data.get_alliance_value(ccode, vary_ccodes[ccode_index_loop]);


                  {Initialize starting alliance security and fitness scores}
                  for pop_element := 1 to risk_specs.num_in_pop do
                     begin
                        sec_calc :=
                           security_position_from_alliance_data (ccode, year,
                           old_alliance_year_data[pop_element], aregion, vary_ccodes, ccode_vary_loc,
                           top_vary_ccode_index, EUvary_ccodes, top_EU_ccode_index, sys_cap_data,
                           distance_data, valid_return_ga);
                        if valid_return_ga then security_data[pop_element] := sec_calc
                          else {that constructed alliance_year_data element had problems}
                             begin
                                old_alliance_year_data[pop_element].copy (old_alliance_year_data[1]);
                                security_data[pop_element] := security_data[1];
                                if pop_element = 1 then EUGeneError ('Bad array element was #1 in GA - serious error!',
                                   3, continue, error_log);
                             end;
                        application.processMessages;
                     end;
                  Make_fitness_table (fitness_table, sum_fitness, security_data,
                            risk_specs.num_in_pop, 1.5);


                  {This is the main search loop over alternative alliances.  Within this loop,
                   try varying alliances with other states until the best pattern is found.}
                  loop_count := 0;
                  generations_stable := 0;
                  repeat   {repeat is until change in U < tolerance}
                     application.processMessages;
                     optimize_trace.tick (outstring, 0);
                     loop_count := loop_count + 1;

                     {Loop starts with the previous generation's pop in Old_pop}

                     {Always, the best seen so far is in UStart}
                     Ustart := security_data[1];

                     {recombine genes in "old" to get pop in "new".  The proc
                      contains all procedures for keeping the best, reproducing by
                      fitness, and is totally responsibe for creating new alliance data }
                     CrossoverAndGenerate (risk_specs.num_in_pop, old_alliance_year_data,
                                           new_alliance_year_data, sum_fitness, fitness_table);

                     {mutate those that aren't specified as to keep.}
                     Mutate;

                     application.processMessages;

                     {compute new security and then fitness scores}
                     for pop_element := 1 to risk_specs.num_in_pop do
                        begin
                           sec_calc := security_position_from_alliance_data (ccode, year,
                              new_alliance_year_data[pop_element], aregion, vary_ccodes, ccode_vary_loc,
                              top_vary_ccode_index, EUvary_ccodes, top_EU_ccode_index, sys_cap_data,
                              distance_data, valid_return_ga);
                           if valid_return_ga then security_data[pop_element] := sec_calc
                             else {that constructed alliance_year_data element had major problems,
                                   so replace it with best seen so far}
                                begin
                                   new_alliance_year_data[pop_element].copy (new_alliance_year_data[1]);
                                   security_data[pop_element] := security_data[1];
                                   if pop_element = 1 then EUGeneError ('Bad array element was #1 in GA - serious error!',
                                      3, continue, error_log);
                                end;
                           application.processMessages;
                        end;

                     {Need to use an objective function to get fitness from security}
                     {1.5 here is the scale factor for stretching fitness}
                     Make_fitness_table (fitness_table, sum_fitness, security_data,
                            risk_specs.num_in_pop, 1.5);

                     {Find the best in New_alliance_data, make it be in position 1}
                     move_to_position (1, risk_specs.num_in_pop, fitness_table);

                     {check best score vs. previous best}
                     {I know the best is in f_d[1]}
                     {the abs means this should work whether I'm maximizing or minimizing the
                      value.  However, this could pick up on either an increase or decrease
                      in security.  Basically, checking for little change.}
                     Usum_change := abs(Ustart - security_data[1]);
                     if Usum_change < user_selections.risk_calculation_info.risk_search_tolerance then
                        inc(generations_stable)
                     else    {Usum still changing}
                        generations_stable := 0;

                     {Now move the new population in "new" into the "old" structure}
                     for pop_element := 1 to risk_specs.num_in_pop do
                        begin
                           old_alliance_year_data[pop_element].copy (new_alliance_year_data[pop_element]);
                        end;


                     {trace.message (inttostr(ccode)+' '+inttostr(year)+' aregion '+inttostr(ord(aregion))+' loop '+
                                   inttostr(loop_count)+ ' best = ' + realtostring(security_data[1]));}
                     {for pop_element := 1 to num_in_pop do
                         trace.message (inttostr(pop_element)+': '+realtostring(fitness_data[pop_element]));}


                     {continue looping until change is within tolerance levels.}
                  until ( (Usum_change < user_selections.risk_calculation_info.risk_search_tolerance) and
                          (generations_stable >= risk_specs.generations_to_be_stable) ) or
                        (loop_count > 10*top_vary_ccode_index);

                  if (loop_count > 10*top_vary_ccode_index) then
                     begin
                        trace.message ('Exited risk optimize loop b/c loop count exceeded 10x # states in region in GA,'
                               +' for state '+inttostr(ccode)+' in '+inttostr(year)+' in region '+inttostr(ord(aregion)));
                     end;


                  {Want to check and make sure I'm at an optimum to get any last improvement
                   possible.  So use a fast random walk from this last found GA point to
                   find an optimum.  }
                  {First need to construct set of states to check}
                  {create set of all states}
                  for ccode_index_loop := 1 to top_vary_ccode_index do
                     full_set[ccode_index_loop] := vary_ccodes[ccode_index_loop];
                  full_set_count := top_vary_ccode_index;
                  current_set := full_set;
                  current_set_count := full_set_count;

                  {Get optimum from and into new[1], returning potentially changed current_bits,
                   and optimum sec found.  If the proc finds nothing better, these will be unchanged.}
                  get_random_walk_optimum (new_alliance_year_data[1], current_alliance_bits,
                           optimum_random_security_found, current_set, full_set, current_set_count,
                           full_set_count, security_data[1], max_min_sum);
                      {best it can find will be returned in best_this_iteration}

                  if ((max_min_sum = maximize) and (optimum_random_security_found > security_data[1])) or
                     ((max_min_sum = minimize) and (optimum_random_security_found < security_data[1]))
                   then
                     begin   {if better, keep value, and set alliance data}
                        security_position_best_worstGA := optimum_random_security_found;
                        old_alliance_year_data[1].copy (new_alliance_year_data[1]);
                     end
                   else    {not better  }
                     begin
                        security_position_best_worstGA := security_data[1];
                     end;

                  if (max_min_sum = maximize) then
                     trace.message (inttostr(ccode)+' '+inttostr(year)+' aregion '+inttostr(ord(aregion))+
                                ' best = ' + realtostring(result, 7, 4))
                     else trace.message (inttostr(ccode)+' '+inttostr(year)+' aregion '+inttostr(ord(aregion))+
                                ' worst = ' + realtostring(result, 7, 4));

                  {Now save information about the best/worst structure in the
                   Previous structure}
                  for ccode_index_loop := min_ccode_index to max_ccode_index do
                     current_alliance_bits[ccode_index_loop] := 4;
                  for ccode_index_loop := 1 to top_vary_ccode_index do
                     current_alliance_bits[ccode_index.index(vary_ccodes[ccode_index_loop])] :=
                     old_alliance_year_data[1].get_alliance_value(ccode, vary_ccodes[ccode_index_loop]);

                  {Best alliance pattern found remains in alliance_bits. }
                  if (max_min_sum = maximize) then
                     begin
                        previous_alliances^[ccode_index.index(ccode)]^[aregion].max_found := true;
                        previous_alliances^[ccode_index.index(ccode)]^[aregion].best_max := current_alliance_bits;
                     end
                  else if (max_min_sum = minimize) then
                     begin
                        previous_alliances^[ccode_index.index(ccode)]^[aregion].min_found := true;
                        previous_alliances^[ccode_index.index(ccode)]^[aregion].best_min := current_alliance_bits;
                     end
                  else EUGeneError ('Max Min sum has invalid value at end of sec-best-worst procedure', 5, stop, error_log);

                  {Now Store in alliance_security_risk structure}
                  if (max_min_sum = maximize) then
                     begin
                        {have figured out new alliances for this country, this region only}
                        for ccode_index_loop := 1 to top_vary_ccode_index do
                           stored_risk_alliance_data.set_new_best_alliance_data
                              (ccode, vary_ccodes[ccode_index_loop], year, aregion, old_alliance_year_data[1].get_alliance_value(ccode, vary_ccodes[ccode_index_loop]));
                     end
                  else if (max_min_sum = minimize) then
                     begin
                        for ccode_index_loop := 1 to top_vary_ccode_index do
                           stored_risk_alliance_data.set_new_worst_alliance_data
                              (ccode, vary_ccodes[ccode_index_loop], year, aregion, old_alliance_year_data[1].get_alliance_value(ccode, vary_ccodes[ccode_index_loop]));
                     end
                  else EUGeneError ('Max Min sum has invalid value at end of sec-best-worst procedure', 5, stop, error_log);


                  {Could Also create an array of alliance bits just for displaying the alliances with
                   those in the region}
                  {for ccode_index_loop := 1 to max_region_countries do
                     display_alliance_bits[ccode_index_loop] := 4;
                  display_alliance_bits[top_vary_ccode_index+1] := 0;
                  for ccode_index_loop := 1 to top_vary_ccode_index do
                     display_alliance_bits[ccode_index_loop] :=
                        current_alliance_bits[ccode_index.index(vary_ccodes[ccode_index_loop])];  }


                     {Now clean up}

               finally
                  for pop_element := 1 to risk_specs.num_in_pop do
                     begin
                        new_alliance_year_data[pop_element].free;
                        old_alliance_year_data[pop_element].free;
                     end;
                  optimize_trace.tickdone;
                  optimize_trace.free;
               end;   {main finally}

            end;     {sec best worst GA}

         {------------------------------------------------------------}

         function security_position_best_worst_steepest(const ccode : ccode_range; year : year_range;
            const aregion : region_type; const original_alliance_year_data : Talliance_year_obj;
            const sys_cap_data : Tsys_capability_array_obj;
            const distance_data : Tdistance_array_obj;
            const max_min_sum : max_min_type; const vary_ccodes : vary_type;
            const ccode_vary_loc : ccode_vary_type; const top_vary_ccode_index : region_count_type;
            const EUvary_ccodes : EUvary_type; const top_EU_ccode_index : two_region_count_type;
            var previous_alliances : best_alliances_found_type; num_created, num_needed: longint;
            const risk_specs : risk_calculation_info_type): single;

            var ccode_index_loop : ccode_index_range;  {this must have range bigger than regiontype}
                {year_loop : year_range;
                {tau_update_ccode_loop1, tau_update_ccode_loop2 : region_count_type;}
                new_alliance_value : alliance_value_type;
                optimize_trace : Ttrace_obj;
                start_alliance_year_data, temp_alliance_year_data : TAlliance_year_obj;
                ustart, New_USum, Usum_change : single;
                current_alliance_bits, original_alliance_bits : stored_alliance_list_type;
                display_alliance_bits : array[1..max_region_countries] of alliance_value_type;
                loop, loop_count : integer;
                best_change : record
                       Usum : single;
                       ccode : ccode_range;
                       alliance_value : alliance_value_type;
                    end;
                valid_return_st : boolean;
                sec_calc : single;
                other_ccode : ccode_range;

             {The way this procedure works is as follows.  What needs to be found is
              the best or worst alliance portfolio.  We do this by taking a starting
              configuration and checking each "adjacent" configuration to see which
              has the best/worst change in security.  Then, we shift to that alliance,
              and repeat, checking to see which subsequent shift changes security the
              most.  }

            begin   {function security_position_best_worst_steepest}

               optimize_trace := nil;
               try
                  if max_min_sum = maximize then outstring := ('Maximizing (worsening) security (SD), ')
                     else outstring := ('Minimizing (bettering) security by (SD), ');
                  outstring := outstring + inttostr(ccode)+', '+inttostr(year)+', '+inttostr(ord(aregion));
                  optimize_trace := Ttrace_obj.init(trace.get_trace_level);
                  optimize_trace.tick (outstring, 0);
                  optimize_trace.tickdone;

                  start_alliance_year_data := nil;
                  start_alliance_year_data := Talliance_year_obj.init;
                  start_alliance_year_data.allocate_full(year);
                  start_alliance_year_data.copy (original_alliance_year_data);
                  temp_alliance_year_data := nil;
                  temp_alliance_year_data := Talliance_year_obj.init;
                  temp_alliance_year_data.allocate_full(year);
                  temp_alliance_year_data.copy (start_alliance_year_data);

                  {Initialize starting alliance security score}
                  sec_calc := security_position_from_alliance_data (ccode, year,
                            start_alliance_year_data, aregion,
                            vary_ccodes, ccode_vary_loc, top_vary_ccode_index,
                            EUvary_ccodes, top_EU_ccode_index, sys_cap_data,
                            distance_data, valid_return_st);
                  if valid_return_st then UStart := sec_calc
                    else {that constructed alliance_year_data element had problems}
                       begin
                          EUGeneError ('Invalid security return value for actual calculation for '
                                + inttostr(ccode)+' in '+inttostr(year)+' at top of steepest proc. '+
                                 'This is a serious logic flaw.  Contact programmer.', 3, continue, error_log);
                       end;

                  {Create array of alliance bits that have original alliance values, not
                   modified for previous loops or anything else.  This should match
                   what's in the tau tables.}
                  for ccode_index_loop := min_ccode_index to max_ccode_index do
                     original_alliance_bits[ccode_index_loop] := 4;
                  for ccode_index_loop := 1 to top_vary_ccode_index do
                     original_alliance_bits[ccode_index.index(vary_ccodes[ccode_index_loop])] :=
                     start_alliance_year_data.get_alliance_value(ccode, vary_ccodes[ccode_index_loop]);

                  {initialize current_alliance_bits}
                  current_alliance_bits := original_alliance_bits;

                  {Also create an array of alliance bits just for displaying the alliances with
                   those in the region}
                  for ccode_index_loop := 1 to max_region_countries do
                     display_alliance_bits[ccode_index_loop] := 4;
                  display_alliance_bits[top_vary_ccode_index+1] := 0;
                  for ccode_index_loop := 1 to top_vary_ccode_index do
                     display_alliance_bits[ccode_index_loop] :=
                        current_alliance_bits[ccode_index.index(vary_ccodes[ccode_index_loop])];


                  {Starting point will always be the actual configuration.  }


                  {Now set the initial values of the "best" alternative seen.  Begin with current
                   alliance configuration;  initialize Sum(Uji) and alliance pattern to that starting
                   point.  Record as best change is for state to have def pact with self, which is
                   given in all alliances.}
                  best_change.usum := Ustart;
                  best_change.ccode := ccode;
                  best_change.alliance_value := 1;


                  {This is the main search loop over alternative alliances.  Within this loop,
                   try varying alliances with other states until the best pattern is found.}
                  {Initially, start and new data are both set to original values.}
                  loop_count := 0;
                  repeat   {repeat is until change in U < tolerance}
                     loop_count := loop_count + 1;

                     {initialize starting values for this loop}
                     Ustart := best_change.usum;

                     {Search for the alliance switch from current starting point
                      that most increases USum}
                     for ccode_index_loop := 1 to top_vary_ccode_index do
                       if vary_ccodes[ccode_index_loop] <> ccode then
                        begin    {change alliance, recalc EU}
                             {reset alliance pattern to no changes}
                           temp_alliance_year_data.copy (start_alliance_year_data);
                           other_ccode := vary_ccodes[ccode_index_loop];
                           optimize_trace.tick (outstring, 0);
                           {trace.message ('Checking alliance changes '+inttostr(ccode)+' '+
                                          inttostr(vary_ccodes[ccode_index_loop])+'  loop '+
                                          inttostr(loop_count)); }
                           {change alliance with ccode "vary_ccodes[ccode_index_loop]"}
                           {For this other ccode, want to try each of the alliance types}
                           for new_alliance_value := 1 to 4 do
                              if new_alliance_value <>
                                 current_alliance_bits[ccode_index.index(vary_ccodes[ccode_index_loop])] then
                                 begin
                                    {since this hogs the system, check for Windows operations}
                                    Application.ProcessMessages;

                                    temp_alliance_year_data.set_alliance_value(
                                      ccode, other_ccode, new_alliance_value );
                                    temp_alliance_year_data.set_alliance_value(
                                      other_ccode, ccode, new_alliance_value );

                                    {Now figure out new security position given this change}
                                    new_USum := security_position_from_alliance_data (ccode, year,
                                             temp_alliance_year_data, aregion,
                                             vary_ccodes, ccode_vary_loc, top_vary_ccode_index,
                                             EUvary_ccodes, top_EU_ccode_index, sys_cap_data,
                                             distance_data, valid_return_st);
                                   {trace.message ('Saw value '+realtostring(new_Usum)+' with ' +inttostr(other_ccode));}
                                       {for steepest proc, don't need to anything if invalid except not
                                        record it}

                                        {Maximizing means getting the most positive, so most vulnerable for state.
                                         Minimizing means most negative, so most secure for state.
                                         So alliance changes that made new_USum go up (more positive) hurt the
                                         state's security, while alliance changes that made new_USum go down
                                         (more negative) made state more secure, bettering its security.  }
                                      {If not valid, just ignore it.}
                                    if (valid_return_st) then
                                    if ((max_min_sum = maximize) and (new_USum > best_change.Usum)) or
                                       ((max_min_sum = minimize) and (new_USum < best_change.USum))
                                     then
                                          begin
                                             {Record this as the best adjustment so far}
                                             best_change.Usum := new_USum;
                                             best_change.ccode := other_ccode;
                                             best_change.alliance_value := new_alliance_value;
                                          end;
                                 end;   {for new_alliance_value loop}

                        end;               {ccode loop}

                        {Now have calculated the biggest change possible from the starting config.}
                        {Set the starting set of alliances to reflect this change, which is
                         carried into the next iteration.  }
                     start_alliance_year_data.set_alliance_value(
                          ccode, best_change.ccode, best_change.alliance_value );
                     start_alliance_year_data.set_alliance_value(
                          best_change.ccode, ccode, best_change.alliance_value );
                     current_alliance_bits[ccode_index.index(best_change.ccode)] := best_change.alliance_value;
                     Usum_change := Ustart - best_change.usum;

                     if (max_min_sum = maximize) then
                        trace.message (inttostr(ccode)+' '+inttostr(year)+' region '+inttostr(ord(aregion))+' loop '+
                                   inttostr(loop_count)+ ' best = ' + realtostring(best_change.Usum, 7, 4))
                        else trace.message (inttostr(ccode)+' '+inttostr(year)+' region '+inttostr(ord(aregion))+' loop '+
                                   inttostr(loop_count)+ ' worst = ' + realtostring(best_change.Usum, 7, 4));

                     {continue looping until change is within tolerance levels.}
                     {the abs means this should work whether I'm maximizing or minimizing the value.}
                  until (abs(Usum_change) < user_selections.risk_calculation_info.risk_search_tolerance) or
                        (loop_count > 2*top_vary_ccode_index);
                  if (loop_count > 2*top_vary_ccode_index) then
                     begin
                        trace.message('Exited risk optimize loop b/c loop count exceeded 2x # states in region,'
                               +' for state '+inttostr(ccode)+' in '+inttostr(year)+' in region '+inttostr(ord(aregion)));
                     end;

                  security_position_best_worst_steepest := best_change.usum;

                  {Best alliance pattern found remains in alliance_bits.  Store in perma. array}
                  if (max_min_sum = maximize) then
                     begin
                        previous_alliances^[ccode_index.index(ccode)]^[aregion].max_found := true;
                        previous_alliances^[ccode_index.index(ccode)]^[aregion].best_max := current_alliance_bits;
                     end
                  else if (max_min_sum = minimize) then
                     begin
                        previous_alliances^[ccode_index.index(ccode)]^[aregion].min_found := true;
                        previous_alliances^[ccode_index.index(ccode)]^[aregion].best_min := current_alliance_bits;
                     end
                  else EUGeneError ('Max Min sum has invalid value at end of sec-best-worst procedure', 5, stop, error_log);

                  {update array of alliance bits for displaying the alliances with
                   those in the region}
                  for ccode_index_loop := 1 to top_vary_ccode_index do
                     display_alliance_bits[ccode_index_loop] :=
                     current_alliance_bits[ccode_index.index(vary_ccodes[ccode_index_loop])];

               finally
                  start_alliance_year_data.free;
                  temp_alliance_year_data.free;
                  optimize_trace.tickdone;
                  optimize_trace.free;
               end;
            end;          {func sec best worst steepest}

                       {------------------------}

         function security_position_best_worst_random_walk (const ccode : ccode_range; year : year_range;
            const aregion : region_type; const original_alliance_year_data : Talliance_year_obj;
            const sys_cap_data : Tsys_capability_array_obj;
            const distance_data : Tdistance_array_obj;
            const max_min_sum : max_min_type; const vary_ccodes : vary_type;
            const ccode_vary_loc : ccode_vary_type; const top_vary_ccode_index : region_count_type;
            const EUvary_ccodes : EUvary_type; const top_EU_ccode_index : two_region_count_type;
            var previous_alliances : best_alliances_found_type; num_created, num_needed: longint;
            const risk_specs : risk_calculation_info_type): single;

            var ccode_index_loop : ccode_index_range;  {this must have range bigger than regiontype}
                optimize_trace : Ttrace_obj;
                temp_alliance_year_data : TAlliance_year_obj;
                best_random, Best_this_iteration : single;
                current_alliance_bits, best_current_alliance_bits, original_alliance_bits : stored_alliance_list_type;
                display_alliance_bits : array[1..max_region_countries] of alliance_value_type;
                random_walk_loop :  integer;
                {best_change : record
                       Usum : single;
                       ccode : ccode_range;
                       alliance_value : alliance_value_type;
                    end;  }
                valid_return_st : boolean;
                sec_calc : single;
                full_set, current_set : array_set;
                full_set_count, current_set_count : integer;


             {The way this procedure works is as follows.  What needs to be found is
              the best or worst alliance portfolio.  We do this by taking a starting
              configuration and checking each "adjacent" configuration to see which
              has the best/worst change in security.  Then, we shift to that alliance,
              and repeat, checking to see which subsequent shift changes security the
              most.  }

            begin   {function security_position_best_worst_random_walk}
               optimize_trace := nil;
               try
                  if max_min_sum = maximize then outstring := ('Maximizing (worsening) security (RW), ')
                     else outstring := ('Minimizing (bettering) security by (RW), ');
                  outstring := outstring + ', '+inttostr(ccode)+', '+inttostr(year)+', '+inttostr(ord(aregion));
                  optimize_trace := Ttrace_obj.init(trace.get_trace_level);
                  optimize_trace.tick (outstring, 0);
                  optimize_trace.tickdone;

                  temp_alliance_year_data := nil;
                  temp_alliance_year_data := Talliance_year_obj.init;
                  temp_alliance_year_data.allocate_full(year);
                  temp_alliance_year_data.copy (original_alliance_year_data);

                  {Initialize starting alliance security score}
                  sec_calc := security_position_from_alliance_data (ccode, year,
                            temp_alliance_year_data, aregion,
                            vary_ccodes, ccode_vary_loc, top_vary_ccode_index,
                            EUvary_ccodes, top_EU_ccode_index, sys_cap_data,
                            distance_data, valid_return_st);
                  if valid_return_st then best_random := sec_calc
                    else {that constructed alliance_year_data element had problems}
                       begin
                          EUGeneError ('Invalid security return value for actual calculation for '
                                + inttostr(ccode)+' in '+inttostr(year)+' at top of random walk proc. '+
                                 'This is a serious logic flaw.  Contact programmer.', 3, continue, error_log);
                       end;

                  {Create array of alliance bits that have original alliance values, not
                   modified for previous loops or anything else.  This should match
                   what's in the tau tables.}
                  for ccode_index_loop := min_ccode_index to max_ccode_index do
                     original_alliance_bits[ccode_index_loop] := 4;
                  for ccode_index_loop := 1 to top_vary_ccode_index do
                     original_alliance_bits[ccode_index.index(vary_ccodes[ccode_index_loop])] :=
                        temp_alliance_year_data.get_alliance_value(ccode, vary_ccodes[ccode_index_loop]);

                  {Also create an array of alliance bits just for displaying the alliances with
                   those in the region}
                  for ccode_index_loop := 1 to max_region_countries do
                     display_alliance_bits[ccode_index_loop] := 4;
                  display_alliance_bits[top_vary_ccode_index+1] := 0;
                  for ccode_index_loop := 1 to top_vary_ccode_index do
                     display_alliance_bits[ccode_index_loop] :=
                        current_alliance_bits[ccode_index.index(vary_ccodes[ccode_index_loop])];


                  {create set of all states}
                  for ccode_index_loop := 1 to top_vary_ccode_index do
                     full_set[ccode_index_loop] := vary_ccodes[ccode_index_loop];
                  full_set_count := top_vary_ccode_index;

                  {Intialize var that keeps track of best security over the iterations}
                  best_random := sec_calc;

                  for random_walk_loop := 1 to risk_specs.random_risk_iterations do
                     begin
                        optimize_trace.tick (outstring, 0);
                        temp_alliance_year_data.copy (original_alliance_year_data);

                        {initialize current_alliance_bits}
                        current_alliance_bits := original_alliance_bits;

                        {set current states}
                        current_set := full_set;
                        current_set_count := full_set_count;

                        {Find optimum from Starting point of the actual configuration.  }
                        get_random_walk_optimum (temp_alliance_year_data, current_alliance_bits,
                                 best_this_iteration, current_set, full_set, current_set_count,
                                 full_set_count, sec_calc, max_min_sum);
                            {best it can find will be returned in best_this_iteration}

                        if (max_min_sum = maximize) then
                           trace.message (inttostr(ccode)+' '+inttostr(year)+' region '+inttostr(ord(aregion))+' Iter. '+
                                       inttostr(random_walk_loop)+ ' best = ' + realtostring(Best_this_iteration, 7, 4))
                        else trace.message (inttostr(ccode)+' '+inttostr(year)+' region '+inttostr(ord(aregion))+' Iter. '+
                                       inttostr(random_walk_loop)+ ' worst = ' + realtostring(Best_this_iteration, 7, 4));

                        {Now, is this loop better than previous loops?}
                        if ((max_min_sum = maximize) and (Best_this_iteration > best_random)) or
                           ((max_min_sum = minimize) and (Best_this_iteration < best_random))
                         then
                              begin
                                 best_random := Best_this_iteration;
                                 best_current_alliance_bits := current_alliance_bits;
                                 {Best alliance pattern found remains in alliance_bits.  Store in perma. array}
                                 if (max_min_sum = maximize) then
                                    begin
                                       previous_alliances^[ccode_index.index(ccode)]^[aregion].max_found := true;
                                       previous_alliances^[ccode_index.index(ccode)]^[aregion].best_max := current_alliance_bits;
                                    end
                                 else if (max_min_sum = minimize) then
                                    begin
                                       previous_alliances^[ccode_index.index(ccode)]^[aregion].min_found := true;
                                       previous_alliances^[ccode_index.index(ccode)]^[aregion].best_min := current_alliance_bits;
                                    end
                                 else EUGeneError ('Max Min sum has invalid value at end of sec-best-worst procedure', 5, stop,
                                      error_log);
                              end;

                        trace.message ('Random Walk alliance '+inttostr(ccode)+' '+
                            realToString(best_random, 7, 4));

                     end;       { for random loop}

                     {Final security value is whatever the last best seen was. }
                  security_position_best_worst_random_walk := best_random;

                  {update array of alliance bits for displaying the alliances with
                   those in the region}
                  for ccode_index_loop := 1 to top_vary_ccode_index do
                     display_alliance_bits[ccode_index_loop] :=
                     current_alliance_bits[ccode_index.index(vary_ccodes[ccode_index_loop])];

               finally
                     {clean up}
                  temp_alliance_year_data.free;
                  optimize_trace.tickdone;
                  optimize_trace.free;
               end;
            end;          {func sec best worst Random Walk}

                       {------------------------}

         begin   {main proc computed_risk}

            {Need two lists of states to speed up calculations in sub-procs.
            {First, only need to look at states in this region as to who can vary.
             i.e. if target region is europe, only alliances with european states
             can vary.
             But, calculations in EU might involve states outside that region, namely
             states in the region of state i as well.  So need a list of states in
             target region, and states in i's region as well.}
            for top_vary_ccode_index := 0 to max_region_countries do
               vary_ccodes[top_vary_ccode_index] := 0;
            top_vary_ccode_index := 0;
            for ccode_loop := min_ccode to max_ccode do
                  if (nation_list.is_a_state(ccode_loop, year)) and
                     (nation_list.is_involved_in_region(ccode_loop, aregion, year)) or
                     (ccode_loop = ccode) then
                     begin
                        inc(top_vary_ccode_index);
                        vary_ccodes[top_vary_ccode_index] := ccode_loop;
                        ccode_vary_loc[ccode_loop] := top_vary_ccode_index;
                     end;

            for top_EU_ccode_index := 0 to max_two_region_countries do
               EUvary_ccodes[top_EU_ccode_index] := 0;
            top_EU_ccode_index := 0;
            for ccode_loop := min_ccode to max_ccode do
               if (nation_list.is_a_state(ccode_loop, year)
                  ) and
                  ( (nation_list.is_involved_in_region(ccode_loop, aregion, year)) or
                    (nation_list.is_involved_in_region(ccode_loop, nation_list.get_home_region(ccode), year)) or
                    (ccode_loop = ccode)
                  ) then
                     begin
                        inc(top_EU_ccode_index);
                        EUvary_ccodes[top_EU_ccode_index] := ccode_loop;
                     end;


            {First, current security level, which is sum of all other's EU against you.}
            {If this is for risk test, set a flag since I only want certain security
             computations to be printed/saved.}
            if (user_selections.compute_this = compute_single_risk) then
               begin
                 risk_sec_data_print_1 := true;    {for security components}
                 risk_sec_data_print_2 := true;   {for eu components within each sec comp.}
               end
            else
               begin
                 risk_sec_data_print_1 := false;
                 risk_sec_data_print_2 := false;
               end;
            SecSum := security_position_from_alliance_data (ccode, year,
                   alliance_year_data_in_computed, aregion, vary_ccodes, ccode_vary_loc,
                   top_vary_ccode_index, EUvary_ccodes, top_EU_ccode_index, sys_cap_data,
                   distance_data, valid_return);
            risk_sec_data_print_1 := false;
            risk_sec_data_print_2 := false;


            if valid_return then SumUji := SecSum
            else {that constructed alliance_year_data element had problems}
              begin
                 trace.message ('Invalid security return value for actual calculation for '
                       + inttostr(ccode)+' in '+inttostr(year)+' in computed_risk outer proc. '+
                       'Security, risk set to missing');
                 SumUji := missing_value;
              end;

            if SumUji <> missing_value then {don't bother with other computations if can't get actual sec}
            begin
               case risk_specs.method of
                  use_ga: begin
                     {figure state's most vulnerable, max of SumUji}
                     SumUjiMax := security_position_best_worstGA (ccode, year, aregion, alliance_year_data_in_computed,
                            sys_cap_data, distance_data, maximize, vary_ccodes,
                            ccode_vary_loc, top_vary_ccode_index, EUvary_ccodes, top_EU_ccode_index,
                            previous_alliances, stored_risk_alliance_data,
                            num_created, num_needed, user_selections.risk_calculation_info);
                     {figure state's least vulnerable, best security posn, min of SumUji}
                     SumUjiMin := security_position_best_worstGA (ccode, year, aregion, alliance_year_data_in_computed,
                            sys_cap_data, distance_data, minimize, vary_ccodes,
                            ccode_vary_loc, top_vary_ccode_index, EUvary_ccodes, top_EU_ccode_index,
                            previous_alliances, stored_risk_alliance_data,
                            num_created, num_needed,
                            user_selections.risk_calculation_info);
                     end;
                  use_steepest : begin
                     SumUjiMax := security_position_best_worst_steepest (ccode, year, aregion, alliance_year_data_in_computed,
                            sys_cap_data, distance_data, maximize, vary_ccodes,
                            ccode_vary_loc, top_vary_ccode_index, EUvary_ccodes, top_EU_ccode_index,
                            previous_alliances, num_created, num_needed,
                            user_selections.risk_calculation_info);
                     SumUjiMin := security_position_best_worst_steepest (ccode, year, aregion, alliance_year_data_in_computed,
                            sys_cap_data, distance_data, minimize, vary_ccodes,
                            ccode_vary_loc, top_vary_ccode_index, EUvary_ccodes, top_EU_ccode_index,
                            previous_alliances, num_created, num_needed,
                            user_selections.risk_calculation_info);
                     end;
                  use_random_walk : begin
                     SumUjiMax := security_position_best_worst_random_walk (ccode, year,
                               aregion, alliance_year_data_in_computed,
                            sys_cap_data, distance_data, maximize, vary_ccodes,
                            ccode_vary_loc, top_vary_ccode_index, EUvary_ccodes, top_EU_ccode_index,
                            previous_alliances, num_created, num_needed,
                            user_selections.risk_calculation_info);
                     SumUjiMin := security_position_best_worst_random_walk (ccode, year,
                               aregion, alliance_year_data_in_computed,
                            sys_cap_data, distance_data, minimize, vary_ccodes,
                            ccode_vary_loc, top_vary_ccode_index, EUvary_ccodes, top_EU_ccode_index,
                            previous_alliances, num_created, num_needed,
                            user_selections.risk_calculation_info);
                     end
                  else EUGeneError ('Computed risk entered without proper risk method set',3, stop, error_log);
                  end;   {case}
               end       {had actual security}
            else   {at this point, couldn't even compute basic security, so set all to missing}
               begin
                  SumUjiMax := missing_value;
                  SumUjiMin := missing_value;
                  risk_sub := missing_value;
               end;

            if (SumUji <> missing_value) and (SumUjiMax <> missing_value) and (SumUjiMax <> missing_value) then
               begin    {everything appeared to come out with real values, but check a couple
                         of conditions to make sure...}
                  if (SumUjiMax = SumUjiMin) then
                     begin
                        trace.message ('Maximum and minimum security identical for '+
                               inttostr (ccode)+ ' in '+inttostr(year)+ ' in region '+inttostr(ord(aregion)));
                        risk_sub := 0;
                     end
                  else if (SumUji > SumUjiMax) or (SumUji < SumUjiMin) then
                     begin
                        trace.message ('Actual security not between Min, Max for '+inttostr (ccode)+
                              ' in '+inttostr(year)+ ' in region '+inttostr(ord(aregion)));
                        risk_sub := missing_value;
                     end
                  else
                     risk_sub := (2*SumUji - SumUjiMax - SumUjiMin) / (SumUjiMax-SumUjiMin);
               end;

            computed_risk.risk := risk_sub;
            computed_risk.security := SumUji;
            computed_risk.secmaxsum := SumUjiMax;
            computed_risk.secminsum := SumUjiMin;

            {Note - A last calculation would only be necessary for conversion to ri.
             Ri, the main risk scores reported in WTR by BdM, are just the above formula.}
            {lowercase_risk := (1-(risk_sub/3)) / (1+(risk_sub/3));}



            if (user_selections.compute_this = compute_single_risk) then
              begin
               outstring :=  'CCode='+inttostr(ccode)+ ', year='+inttostr(year)+', ';
               writeln (risk_int_scores_file, outstring);
               writeln (risk_int_scores_file);

               writeln (risk_int_scores_file, 'Generating Risk Scores.  Calculation Methods are:  ');
               case risk_specs.method of
                 use_ga : writeln (risk_int_scores_file, '     Genetic Algorithm, '+
                          ' Pop='+inttostr(risk_specs.num_in_pop)+' '+
                          ' Stable Gen='+inttostr(risk_specs.generations_to_be_stable)+
                          ' Cloning='+inttostr(risk_specs.num_from_previous_to_keep)+
                          ' Mutate P='+realtostring(risk_specs.mutate_probability,5,3));
                 use_steepest : writeln (risk_int_scores_file, '     Steepest Descent, Tolerance ='+realtostring(risk_specs.risk_search_tolerance, 5,3));
                 use_random_walk : writeln (risk_int_scores_file, '     Random Walk, Iterations ='+inttostr(risk_specs.random_risk_iterations));
               end;   {case}
               case user_selections.alliance_data_source of
                  flat_dyadic : writeln (risk_int_scores_file, '     Modified Alliance Data, ');
                  {flat_cow_sequence : writeln (risk_int_scores_file, '     COW Alliance Data, ');}
               end;
               case user_selections.distance_method of
                  capitols : writeln (risk_int_scores_file, '     Capitol to Capitol distance only, ');
                  capitols_contiguity : writeln (risk_int_scores_file, '     Capitol to Capitol + contiguity distance, ');
                  capitols_contiguity_war_trap : writeln (risk_int_scores_file, '     Capitol to Capitol + contiguity + War Trap mods, ');
                  nodiscount : writeln (risk_int_scores_file, '     No distance discounting');
               end;
               case user_selections.capability_modifications of
                  COW_only : writeln (risk_int_scores_file, '     Pure COW capability data only.');
                  no_energy : writeln (risk_int_scores_file, '     COW capability data, dropping Energy component.');
                  modified_capability : writeln (risk_int_scores_file, '     COW capability data modified by input file.');
               end;

               write (risk_int_scores_file, 'Other CCodes relevant for risk calculations ('+inttostr(top_vary_ccode_index)+' countries): ',chr(9));
               for ccode_index_loop_test := 1 to top_vary_ccode_index do
                   write (risk_int_scores_file, vary_ccodes[ccode_index_loop_test],chr(9));
               writeln (risk_int_scores_file);
               writeln (risk_int_scores_file);

               writeln (risk_int_scores_file, 'Max Security Found=',SumUjiMax:8:4, ', ','Min Security Found=',SumUjiMin:8:4);
               writeln (risk_int_scores_file, 'Actual Security=', SumUji:8:4, ' Risk=',risk_sub:8:4);

                write (risk_int_scores_file, 'Actual Alliances (real sec): ',chr(9));
                for ccode_index_loop_test := 1 to top_vary_ccode_index do
                   write (risk_int_scores_file, alliance_year_data_in_computed.get_alliance_value
                                (ccode, vary_ccodes[ccode_index_loop_test]), chr(9));
                writeln (risk_int_scores_file);

                write (risk_int_scores_file, 'Maximum Security Alliances (worst sec): ',chr(9));
                for ccode_index_loop_test := 1 to top_vary_ccode_index do
                   write (risk_int_scores_file, previous_alliances^[ccode_index.index(ccode)]^
                         [aregion].best_max[ccode_index.index(vary_ccodes[ccode_index_loop_test])],chr(9));
                writeln (risk_int_scores_file);

                write (risk_int_scores_file, 'Minimum Security Alliances (best sec): ',chr(9));
                for ccode_index_loop_test := 1 to top_vary_ccode_index do
                   write (risk_int_scores_file, previous_alliances^[ccode_index.index(ccode)]^
                         [aregion].best_min[ccode_index.index(vary_ccodes[ccode_index_loop_test])],chr(9));
                writeln (risk_int_scores_file);
                writeln (risk_int_scores_file, ' (For alliance listing, 1=defense, 2=neutrality, 3=entente, 4=no alliance.)');


                writeln (risk_int_scores_file);

              end;        {risk method test and europe}

         end;                  {computed risk}

                       {------------------------}

   procedure preprocess_for_risk (const configuration : configuration_type; var files_exist : boolean);
      var temp_name : TFileName;
      begin
         files_exist := false;
            case user_selections.similarity_method of
              use_tau : temp_name := configuration.EUWarTrap_tau_file_name;
              use_s : temp_name := configuration.EUWarTrap_s_unweighted_file_name;
              else EUGeneError ('Came into compute_and_save_EUWarTrap file check procedure without s/tau option set.  NOtify programmer.  Fatal error.  ',2,stop,error_log);
            end;   {case}
            if check_file_Exists (sys_cap_file_name,
                           '% System Capabilities') then
               if ( {( (raw_data_source=flat_cow_sequence) and
                      (check_file_Exists (sequence_alliance_file_name, 'Raw Alliance Data')) and
                      (check_file_Exists (seq_file_name, 'Alliance Sequencing Data')) ) or   }
                    ( (raw_data_source=flat_dyadic) and
                      (check_file_Exists (dyadic_alliance_file_name, 'Dyadic Alliance Data')) )
                  )   then
                 if check_file_Exists (temp_name,
                           'War Trap version of EU Data') then
                   files_exist := true;
      end;

                  { --------------------- }

   begin   {main procedure compute and save risk}
   try
      distance_data := nil;
      sys_cap_data := nil;
      alliance_data := nil;
      stored_risk_alliance_data := nil;
      previous_alliances := nil;

      try
         trace.enter ('Entered compute_and_save_risk procedure');

         if last_proc_year < first_proc_year then
            switch_year (last_proc_year, first_proc_year);

         trace.message ('Computing and writing risk scores to output file');
         risk_main_trace := Ttrace_obj.init(trace.get_trace_level);

         do_generation := true;

         if user_selections.compute_this = compute_single_risk then
             begin
               SingleRiskYearForm := TSingleRiskYearForm.create(application.mainform);
               SingleRiskYearForm.showmodal;
               if singleRiskYearForm.modalresult = mrOK then
                  begin
                     assign (risk_int_scores_file, SingleRiskYearForm.outfile_name);
                     trace.message ('Optimal single risk pattern Output to '+SingleRiskYearForm.outfile_name);
                     rewrite (risk_int_scores_file);
                     first_proc_year := SingleRiskYearForm.year;
                     last_proc_year := SingleRiskYearForm.year;
                  end
               else
                  begin
                     showmessage ('Single Risk CCode generation cancelled');
                     do_generation := false;
                  end;
             end; {if computing single risk}

         if do_generation = true then
         try
            new (risk_file_rec);  {need this to store info and possibly search risk file}
            assignFile (risk_file, risk_file_name_for_compute_output);
            {May need to advance file to particular spot, depending on what users
             specified outside this procedure.  The thing to do is simply advance to
             the record that matches the first year specified in the procedure}
            {First, if the file is blank or non-existant, just start at beginning.}
            if not (fileExists (risk_file_name_for_compute_output)) then
               rewrite (risk_file)   {does this either if no file, or if it's 0}
            else            {file exists, is it 0?}
               begin
                  reset (risk_file);
                  if (filesize(risk_file)=0) then
                     begin
                        closefile(risk_file);
                        rewrite (risk_file);   {does this either if no file, or if it's 0}
                     end
                  else   {size says file exists with records, so open for writing to the right place}
                     begin
                        try
                           left := 0;
                           seek (risk_file, left);
                           read (risk_file, risk_file_rec^);
                           left_year := risk_file_rec^.year;
                           trace.message ('First year in risk file: '+inttostr(left_year));
                           right := filesize(risk_file) -1;
                           seek (risk_file, right);
                           read (risk_file, risk_file_rec^);
                           right_year := risk_file_rec^.year;
                           trace.message ('Last year in risk file: '+inttostr(right_year));

                           if first_proc_year < left_year then
                              begin
                                 raise EInOutError.create ('Error - attempt to compute/write risk years before earliest year in risk file.  This cannot be done.  ');
                              end
                           else
                           begin
                              {verify year 1 position}
                              want_position := left + (first_proc_year - left_year);
                              {read that record.  It should be the one I want, but check it.  Then reset just before it.  }
                              seek (risk_file, want_position);
                              read (risk_file, risk_file_rec^);
                              if risk_file_rec^.year <> first_proc_year then
                                 begin
                                    raise EInOutError.create ('Year read at target file position when getting ready to write data to risk file was incorrect');
                                 end
                              else    {get to correct position finally}
                                 begin
                                    seek (risk_file, want_position);
                                    trace.message ('Starting with processing '+inttostr(risk_file_rec^.year));
                                    {file now at correct position for writing.}
                                 end;
                           end;
                        except
                           raise EInOutError.create ('There is a problem with the risk files to be modified.  Cannot continue this risk generation procedure.');
                        end;    {except on file opening}
                     end;
               end;
                     {file is now OK, and in right position}

            try
               calc_risk_partition (first_proc_year, last_proc_year,
                                   num_partitions, years_per_partition);
               partition_start_year := first_proc_year;
               num_created := 0;
               num_needed := round ((nation_list.get_country_years
                            * ((last_proc_year-first_proc_year+1)/(configuration.last_risk_year-configuration.first_risk_year+1))
                            *(ord(americas)-ord(europe))));
               distance_data := Tdistance_array_obj.init (locations_file_name, user_selections, contiguity_data);

                {set previous_alliances to none;  set once before beginning any loops.}
               new (previous_alliances);
               for x := min_ccode_index to max_ccode_index do
                  begin
                    new (all_regions);
                    previous_alliances^[x] := all_regions;
                    for loop_region := europe to globe do
                      begin
                        previous_alliances^[x]^[loop_region].max_found := false;
                        previous_alliances^[x]^[loop_region].min_found := false;
                        for y := min_ccode_index to max_ccode_index do
                          begin
                            previous_alliances^[x]^[loop_region].best_max[y] := no_alliance;
                            previous_alliances^[x]^[loop_region].best_min[y] := no_alliance;
                          end;
                      end;
                  end;

               preprocess_for_risk (configuration, files_exist);

               if files_exist then
                 for partition_loop := 1 to num_partitions do
                    begin
                      try
                        partition_end_year := min((partition_start_year + years_per_partition - 1), last_proc_year);
                        sys_cap_data := Tsys_capability_array_obj.init (sys_cap_file_name,
                                     partition_start_year, partition_end_year);
                        alliance_data := Talliance_array_obj.init (sequence_alliance_file_name, seq_file_name,
                                         dyadic_alliance_file_name, partition_start_year, partition_end_year,
                                         raw_data_source);
                        alliance_data.make_alliances_symmetric;
                        stored_risk_alliance_data := Trisk_stored_security_alliance_obj.init(security_alliance_file_name,
                                          partition_start_year, partition_end_year);
                        For year_loop  := partition_start_year to partition_end_year do
                        begin
                           risk_file_rec^.year := year_loop;
                           risk_file_rec^.ccode_from_index_list := ccode_index.return_ccode_list;

                           for x := min_ccode_index to max_ccode_index do
                              for loop_region := europe to globe do
                                 begin
                                    risk_file_rec^.ccode_array[x][loop_region].risk := missing_value;
                                    risk_file_rec^.ccode_array[x][loop_region].security := missing_value;
                                    risk_file_rec^.ccode_array[x][loop_region].secmaxsum := missing_value;
                                    risk_file_rec^.ccode_array[x][loop_region].secminsum := missing_value;
                                 end;
                              {make a copy of the year of alliance data to pass to sub-procs}
                           alliance_data.make_copy_of_alliance_year_array (year_loop, alliance_year_data);

                           if user_selections.compute_this = compute_single_risk then
                            begin
                              writeln (risk_int_scores_file, 'Calculating risk for CCode='+inttostr(SingleRiskYearForm.ccode)+', year='+inttostr(SingleRiskYearForm.year));
                              writeln (risk_int_scores_file);
                              for loop_region := europe to americas do
                                risk_file_rec^.ccode_array[ccode_index.index(SingleRiskYearForm.ccode)][loop_region] :=
                                  computed_risk (SingleRiskYearForm.ccode, SingleRiskYearForm.year, alliance_year_data,
                                  sys_cap_data, distance_data,
                                  Europe, previous_alliances, stored_risk_alliance_data,
                                  num_created, num_needed, risk_specs);
                            end
                           else  {do multiple ccodes, years}
                            begin
                              for ccode := min_ccode to max_ccode do
                               if (nation_list.is_a_state (ccode, year_loop)) then
                                 begin
                                    risk_main_trace.tick('Executing Procedure: Risk Attitude, '+
                                              inttostr(risk_file_rec^.year),
                                              (nation_list.get_country_years*(ord(americas)-ord(europe))));

                                    {this is what I usually want}
                                    {NOTE:  NOT CODED for global risk scores b/c of
                                     region-state array limit.}
                                    for loop_region := europe to americas do
                                       begin
                                          risk_file_rec^.ccode_array[ccode_index.index(ccode)][loop_region] :=
                                           computed_risk (ccode, year_loop, alliance_year_data,
                                           sys_cap_data, distance_data,
                                           loop_region, previous_alliances, stored_risk_alliance_data,
                                           num_created, num_needed, risk_specs);
                                          inc(num_created);
                                       end;
                                 end;       {if is_a_state}

                                 {now write this year of risk data and security data.  }
                              write (risk_file, risk_file_rec^);
                              stored_risk_alliance_data.write_new_data (security_alliance_file_name,
                                          year_loop, year_loop);

                              {$ifdef win32}
                                 flush_file_buffers := FlushFileBuffers(TFileRec(risk_file).Handle);
                                 if flush_file_buffers = false then
                                    showMessage ('Error Flushing file buffer - check risk procedure');
                              {$endif}

                            end;              {not single ccod-year genreation}
                           alliance_year_data.free;
                        end;   {for year_loop := partition_start_year to part_last_year}

                      finally
                        partition_start_year := partition_end_year + 1;
                        risk_main_trace.tickdone;
                        sys_cap_data.free;
                        alliance_data.free;
                        stored_risk_alliance_data.free;
                      end;  {finally}
                    end;  {for partition_loop, files exists}

            finally {after file is OK}
               distance_data.free;
               if previous_alliances <> nil then
                  begin
                     for x := min_ccode_index to max_ccode_index do
                        if previous_alliances^[x] <> nil then dispose (previous_alliances^[x]);
                        dispose (previous_alliances);
                  end;
               risk_main_trace.tickdone;
             end;

         finally    {do generation is true}
            if (risk_file_rec<>nil) then dispose(risk_file_rec);
            CloseFile (risk_file);
         end;

      finally
         if (user_selections.compute_this = compute_single_risk) then
            begin
               if (singleRiskYearForm.modalresult = mrOK) then CloseFile (risk_int_scores_file);
               SingleRiskYearForm.release;
            end;
         ShowMessage ('Computation of risk scores complete!');
         risk_main_trace.exit ('Finished Compute Risk Procedure');
         risk_main_trace.free;
         trace.exit ('Completed compute_and_save_risk procedure');
      end;

   except
      on EUserInterrupt do raise;
      on EInOutError do raise;
   end;

   end;

   { --------------------------------------------------------------- }


end.    {unit}