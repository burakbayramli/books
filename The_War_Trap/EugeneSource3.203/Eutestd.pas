unit eutestd;
     {This is set up to test various procedures and sub elements of the main EUGene program.}

{EUGene  Copyright 1997, 1998  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

   uses sysutils, windows, dialogs, messages, controls,
        CmnProcD, EUinoutD, EUtypes1, EUtypes2, EUMainD, euprocs1,
        EUProcs2, OutWindow;

var FileInfo : TSearchRec;
       strname : pchar;


procedure TestClick;   {can call lots of things defined in this unit.}

        { ---------------------------------------------------}

implementation

uses mdiframe, RiskGenBox, TraceUnit;

        { ---------------------------------------------------}

procedure nation_list_test;
   var ccode : integer;
   begin
      for ccode := min_ccode to max_ccode do
         if nation_list.have_info (ccode) then
         nation_list.show_info (ccode);
   end;

        { ---------------------------------------------------}

procedure check_tau;
   var row, column : 1..4;
       table : tau_table_type;
             N, nrows, ncolumns : integer;
             first_row, last_row, first_col, last_col : 0..4;
   begin
       for row := 1 to 4 do for column := 1 to 4 do table[row, column] := 0;
{       table [1,1] := 0;
       table [1,4] := 10;
       table [4,1] := 13;
       table [4,4] := 5;
 }
       table [1,4] := 13;
       table [4,1] := 10;
       table [4,4] := 5;

       N := 0;
       for row := 1 to 4 do for column := 1 to 4 do N := N + table[row, column];
            {Figure out dimensions of table.  If the table is 1xn or nx1, then tau
             cannot be calculated.  }
            first_row := 0;
            first_col := 0;
            last_row := 0;
            last_col := 0;
            for row := 1 to 4 do
               for column := 1 to 4 do
                  if table[row, column] > 0 then
                     begin
                        if first_row = 0 then first_row := row;
                        if first_col = 0 then first_col := column;
                        if last_row = 0 then last_row := row;
                        if last_col = 0 then last_col := column;
                        if row > last_row then last_row := row;
                        if column > last_col then last_col := column;
                        if row < first_row then first_row := row;
                        if column < first_col then first_col := column;
                     end;
            ncolumns := last_col - first_col + 1;
            nrows := last_row - first_row + 1;


       writeln;
{commented out b/c this function is no longer accessible externally.}
{       writeln ('Tau is ',tau_from_table (table, N,nrows, ncolumns):8:4);
}
   end;

        { ---------------------------------------------------}

   procedure dirtest;
   var winloc : pchar;
       bufsize : integer;
       config_file_name, windir : string;
       initialization_file_name : TFileName;
   begin
      initialization_file_name := 'temp.ini';
      new (winloc);
      bufsize := GetWindowsDirectory (winloc, 255);
        {If 255 was too small, do again with bigger calculated buffer value}
      if bufsize = 0 then bufsize := GetWindowsDirectory (winloc, bufsize);
        {Now, search for and get path to ini file in Windows directory}
      windir := strPas (winloc);
      writeln (winloc);
      writeln (windir);
      config_file_name := strPas (winloc) + '\' + initialization_file_name;
      writeln (config_file_name);
   end;   {proc dirtest}

        { ---------------------------------------------------}

   procedure memtest;
      begin
         writeln ('Mem is ',memavail);
         ccode_index.init('d:\programs\euproj\inputdat\nations.raw');  {Must do this before calling nation_list.init}
         writeln ('Mem is ',memavail);
         nation_list.init ( configuration);

         writeln ('Mem is ',memavail);
      end;

        { ---------------------------------------------------}

  procedure file_test;
     var file_name : TFileName;
     begin
        file_name := 'd:\programs\euproj\eugene.ini';
        if FileExists (file_name) then writeln ('File ', file_name, ' is there')
           else writeln ('File ', file_name, ' is NOT there');
        file_name := 'd:\programs\euproj\eugene.NOT';
        if fileExists (file_name) then writeln ('File ', file_name, ' is there')
           else writeln ('File ', file_name, ' is NOT there');
        file_name := 'd:\programs\euprojeugene.ini';
        if fileExists (file_name) then writeln ('File ', file_name, ' is there')
           else writeln ('File ', file_name, ' is NOT there');
        file_name := 'd:\programs\nondir\eugene.ini';
        if fileExists (file_name) then writeln ('File ', file_name, ' is there')
           else writeln ('File ', file_name, ' is NOT there');

     end;

  procedure distance_check;
     type check_rec = record
        ccode1, ccode2 : ccode_range;
        year : year_range;
        end;
     var check_array:  array[1..20] of check_rec;
         contig : boolean;
         x, ccode1, ccode2, year, count, dist : integer;
         distance_data : Tdistance_array_obj;
     begin
        trace.enter ('Entered distance check procedure');
        distance_data := Tdistance_array_obj.init (configuration.distance_file_name,
                                             user_selections, contiguity_data);

        {print dist for some ccode years}
        with check_array[1] do begin ccode1:=2; ccode2:=2; year:=1816; end;
        with check_array[2] do begin ccode1:=2; ccode2:=70; year:=1900; end;
        with check_array[3] do begin ccode1:=2; ccode2:=20; year:=1950; end;
        with check_array[4] do begin ccode1:=2; ccode2:=200; year:=1816; end;
        with check_array[5] do begin ccode1:=2; ccode2:=700; year:=1930; end;
        with check_array[6] do begin ccode1:=2; ccode2:=70; year:=1930; end;
        with check_array[7] do begin ccode1:=2; ccode2:=70; year:=1950; end;
        with check_array[8] do begin ccode1:=365; ccode2:=255; year:=1816; end;
        with check_array[9] do begin ccode1:=365; ccode2:=255; year:=1940; end;
        with check_array[10] do begin ccode1:=365; ccode2:=255; year:=1943; end;
        with check_array[11] do begin ccode1:=365; ccode2:=265; year:=1956; end;
        with check_array[12] do begin ccode1:=365; ccode2:=740; year:=1880; end;
        with check_array[13] do begin ccode1:=365; ccode2:=740; year:=1985; end;
        with check_array[14] do begin ccode1:=365; ccode2:=710; year:=1890; end;
        with check_array[15] do begin ccode1:=365; ccode2:=710; year:=1950; end;
        with check_array[16] do begin ccode1:=2; ccode2:=740; year:=1967; end;
        with check_array[17] do begin ccode1:=2; ccode2:=325; year:=1972; end;
        with check_array[18] do begin ccode1:=666; ccode2:=645; year:=1991; end;
        with check_array[19] do begin ccode1:=2; ccode2:=255; year:=1930; end;
        with check_array[20] do begin ccode1:=2; ccode2:=265; year:=1970; end;
        for x := 1 to 20 do
           trace.message (inttostr(x)+': '+inttostr(check_array[x].ccode1)+' '+
           inttostr(check_array[x].ccode2)+' '+ inttostr(check_array[x].year)+' '+
            inttostr(distance_data.get_distance(check_array[x].ccode1,check_array[x].ccode2,check_array[x].year))
            );

        {trace.message ('  ');
        trace.message ('  ');
        trace.message (' Missing distances when both states are nations: ');
        for ccode1 := min_ccode to max_ccode do
          if nation_list.have_info(ccode1) then
            for ccode2 := min_ccode to max_ccode do
              if nation_list.have_info(ccode2) then
              for year := min_year to max_year do
                if (nation_list.is_a_state(ccode1, year) and nation_list.is_a_state(ccode2, year)) then
                  begin
                    if distance_data.get_distance(ccode1,ccode2,year) < 0 then
                    trace.message (inttostr(ccode1)+' '+
                      inttostr(ccode2)+' '+ inttostr(year)+' '+
                      inttostr(distance_data.get_distance(ccode1,ccode2,year)));
                    trace.tick (500, 'Checking distances for missing');
                  end;
        trace.tickdone;
        trace.message ('  ');
        trace.message ('  ');                 }
{        trace.message (' Contiguous states:');
        count := 0;
        for ccode1 := min_ccode to max_ccode do
          if nation_list.have_info(ccode1) then
            for ccode2 := min_ccode to max_ccode do
              if nation_list.have_info(ccode2) then
                begin
                year := nation_list.get_endyear1(ccode1);
                if (nation_list.is_a_state(ccode1, year) and nation_list.is_a_state(ccode2, year)) then
                   begin
                     {trace.tick (500, 'Checking for who is contiguous');
                     if contiguity_data.is_contiguous(ccode1,ccode2,year) then
                        begin
                          count := count + 1;
                          trace.message (inttostr(ccode1)+' '+
                            inttostr(ccode2)+' '+ inttostr(year)+' is contiguous');
                          if count mod 100 = 0 then ShowMessage ('pausing');
                        end;
                     trace.tick (500, 'Checking for who is contiguous');  }
{                     contig := contiguity_data.is_contiguous(ccode1,ccode2,year);
                     dist := distance_data.get_distance(ccode1,ccode2,year);
                     if distance_data.get_distance(ccode1,ccode2,year)<100 then
                        begin
                          count := count + 1;
                          trace.message (inttostr(ccode1)+' '+
                            inttostr(ccode2)+' '+ inttostr(year)+' '+
                            inttostr(distance_data.get_distance(ccode1,ccode2,year)));
                         { if count mod 100 = 0 then ShowMessage('pausing');}
 {                       end;
                   end;
                end;
          trace.tickdone;
  }      trace.message ('Done!');
        distance_data.free;
        trace.exit ('Exiting distance check procedure');

     end;



  procedure tau_check;
     var tau_data : Ttau_array_obj;
         x : integer;
     begin
            tau_data := Ttau_array_obj.init (configuration.tau_file_name, 1816, 1822);
            tau_data.free;
{        for x := 1955 to 1980 do
          begin
            tau_data := Ttau_array_obj.init (configuration.tau_file_name, x, x+1);
            tau_data.free;
          end;  }
     end;

procedure pointer_pass_check;
  type testrectype = record
           val1, val2 : integer;
          end;
       ptype = ^testrectype;
       ptest=array[1..5] of ptype;
       ptest_array=^ptest;
       arr_test_type = ptest_array;
  var test_array : arr_test_type;
      x : integer;

   procedure pnt_change (const intestarray : arr_test_type);
       {note:  this demonstrates that the "const" param id makes no differnece
        with pointer structures, the true values are changed even with it.}
      var y : integer;
   begin
      for y := 1 to 5 do
        begin
           intestarray^[y]^.val1 := 2837;
           intestarray^[y]^.val2 := 9832;
        end;
   end;

   begin
      new(test_array);
      for x := 1 to 5 do
        begin
           new(test_array^[x]);
           test_array^[x]^.val1 := 3;
           test_array^[x]^.val2 := 5;
        end;
      trace.message ('before');
      for x := 1 to 5 do
        begin
           trace.message (inttostr(x)+' '+inttostr(test_array^[x]^.val1)+' '+inttostr(test_array^[x]^.val1));
        end;
      pnt_change (test_array);
      trace.message ('after');
      for x := 1 to 5 do
        begin
           trace.message (inttostr(x)+' '+inttostr(test_array^[x]^.val1)+' '+inttostr(test_array^[x]^.val1));
        end;

   end;


   procedure nation_list_out;
      var temp : text;
          x : integer;
     begin
        assign (temp, 'd:\papers\bdmcheck\progdata\nameconvert.sta');
        rewrite (temp);
        for x := min_ccode to max_ccode do
           begin
              if nation_list.is_a_state_between (x, 1816, 1993) then
                 writeln (temp, 'replace ccode1=',x, ' if nation="', nation_list.get_abbrev(x),'"');
           end;
        close (temp);
     end;

   { -------------------------------------------   }

procedure TestClick1;   {can call lots of things defined in this unit.}
   var alliances : Talliance_array_obj;
   begin
     {nation_list_out;}
     {distance_check;    {in eutestd;  delete later}
     {tau_check;      halt;}

     {        compute_and_save_taus (configuration.cow_alliance_file_name,
                configuration.alliance_seq_file_name, configuration.dyadic_alliance_file_name,
                'c:\temp\testtau.out',
                nation_list, 1958, 1958, flat_dyadic);
             compute_and_save_taus (configuration.cow_alliance_file_name,
                configuration.alliance_seq_file_name, configuration.dyadic_alliance_file_name,
                'c:\temp\testtau.out',
                nation_list, 1975, 1975, flat_dyadic);
      }

     {Use this call for testing security computations}
     user_selections.distance_method := nodiscount;
     user_selections.alliance_data_source := flat_dyadic; {NOTE: Taus must be run before}
     user_selections.capability_modifications := cow_only;   {NOTE:  This must be run before!}

     user_selections.risk_calculation_info.random_risk_iterations := 3;

     user_selections.distance_method := capitols_contiguity_war_trap;
     user_selections.alliance_data_source := flat_dyadic;
     user_selections.capability_modifications := cow_only;
     user_selections.risk_calculation_info.method := use_ga;
     user_selections.risk_calculation_info.num_from_previous_to_keep := 2;
     user_selections.risk_calculation_info.generations_to_be_stable := 10;
     user_selections.risk_calculation_info.num_in_pop := 30;
     user_selections.risk_calculation_info.mutate_probability := 0.05;

     {compute_and_save_risk (configuration.cow_system_pct_file_name,
         configuration.cow_alliance_file_name, configuration.alliance_seq_file_name,
         configuration.dyadic_alliance_file_name,
         configuration.EUWarTrap_file_name,
         'd:\programs\euproj\risktemp.dat', configuration.distance_file_name,
         nation_list, contiguity_data, 1850, 1850, true, user_selections.alliance_data_source,
         user_selections.risk_calculation_info);
      }


     {fileBufferTest;}

   end;

   { -------------------------------------------   }


   { -------------------------------------------   }

   procedure testClick;
   begin
      showmessage ('Test procedure currently not set to any procedure.');
      
      {compute_and_save_s (configuration.cow_alliance_file_name,
                           configuration.alliance_seq_file_name, configuration.dyadic_alliance_file_name,
                           'stest.tmp', configuration.cow_system_pct_file_name,
                           nation_list, 1961 ,
                           1961, user_selections.alliance_data_source);}

   end;

   { -------------------------------------------   }


end.               {unit}
