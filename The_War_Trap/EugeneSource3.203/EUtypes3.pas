unit EUtypes3;
  {has polity data procedures, system variables, generic user data}

{EUGene  Copyright 1997, 1998+  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

 interface

uses windows, dialogs, forms, eutypes1, TraceUnit, ProgressInterruptWind, cmnprocD,
     sysUtils, FileError, math;

type
     {if polity missing values change, need to check function polity_missing below which uses
     any value <0 to be missing for calculating regime chg.}
   pol_range3 = missing_value_polity..3;     {-99, -88, -77, -66 are various missing polity codes.}
   pol_range4 = missing_value_polity..4;
   pol_range5 = missing_value_polity..5;
   pol_range7 = missing_value_polity..7;
   pol_range10 = missing_value_polity..10;

   {object for gurr polity data.}
   polity_data_ptr = ^polity_data;
   polity_data = record
         autoc : pol_range10;
         democ : pol_range10;
         xrreg : pol_range3;
         xrcomp : pol_range3;
         xropen : pol_range4;
         mono : pol_range5;
         xconst : pol_range7;
         parreg : pol_range5;
         parcomp : pol_range5;
         cent : pol_range3;
      end;   {record}

   polity_array_ptr = ^polity_array;
   polity_array = array[ccode_index_range] of polity_data_ptr;
   polity_year_array_ptr = ^polity_year_array;
   polity_year_array = array[year_range] of polity_array_ptr;
   Tpolity_array_obj = class (Tobject)
      constructor init (polity3_file_name : TFileName;
               first_proc_year, last_proc_year : year_range);
         {init reads in from raw data.}
      destructor destroy; override;
      function initialized : boolean;
      function get_autoc (ccode: ccode_range; year: year_range) : integer; overload;
      function get_democ (ccode: ccode_range; year: year_range) : integer; overload;
      function get_xrreg (ccode: ccode_range; year: year_range) : integer; overload;
      function get_xrcomp (ccode: ccode_range; year: year_range) : integer; overload;
      function get_xropen (ccode: ccode_range; year: year_range) : integer; overload;
      function get_mono (ccode: ccode_range; year: year_range) : integer; overload;
      function get_xconst (ccode: ccode_range; year: year_range) : integer; overload;
      function get_parreg (ccode: ccode_range; year: year_range) : integer; overload;
      function get_parcomp (ccode: ccode_range; year: year_range) : integer; overload;
      function get_cent (ccode: ccode_range; year: year_range) : integer; overload;
      function get_dem (ccode: ccode_range; year: year_range) : integer; overload;
      function get_autoclg (ccode: ccode_range; year: year_range) : integer; overload;
      function get_democlg (ccode: ccode_range; year: year_range) : integer; overload;
      function get_demlg (ccode: ccode_range; year: year_range) : integer; overload;
      function get_demchg (ccode: ccode_range; year: year_range) : integer; overload;
      function get_autoc (ccode: ccode_range; year: year_range; error_check: boolean) : integer; overload;
      function get_democ (ccode: ccode_range; year: year_range; error_check: boolean) : integer; overload;
      function get_xrreg (ccode: ccode_range; year: year_range; error_check: boolean) : integer; overload;
      function get_xrcomp (ccode: ccode_range; year: year_range; error_check: boolean) : integer; overload;
      function get_xropen (ccode: ccode_range; year: year_range; error_check: boolean) : integer; overload;
      function get_mono (ccode: ccode_range; year: year_range; error_check: boolean) : integer; overload;
      function get_xconst (ccode: ccode_range; year: year_range; error_check: boolean) : integer; overload;
      function get_parreg (ccode: ccode_range; year: year_range; error_check: boolean) : integer; overload;
      function get_parcomp (ccode: ccode_range; year: year_range; error_check: boolean) : integer; overload;
      function get_cent (ccode: ccode_range; year: year_range; error_check: boolean) : integer; overload;
      function get_dem (ccode: ccode_range; year: year_range; error_check: boolean) : integer; overload;
      function get_autoclg (ccode: ccode_range; year: year_range; error_check: boolean) : integer; overload;
      function get_democlg (ccode: ccode_range; year: year_range; error_check: boolean) : integer; overload;
      function get_demlg (ccode: ccode_range; year: year_range; error_check: boolean) : integer; overload;
      function get_demchg (ccode: ccode_range; year: year_range; error_check: boolean) : integer; overload;
     private
      data : polity_year_array_ptr;
      first_partition_year, last_partition_year : year_range;
      created : boolean;
      function all_in_range (accode: ccode_range; ayear : year_range; error_check : boolean): boolean;
      function polity_missing (avalue : integer) : boolean;
   end;                 {polity object}


   Tsystem_variables_obj = class (Tobject)
      constructor init(sys_capability_data : Tsys_capability_array_obj);
         {init constructs the variable arrays for reference.}
      destructor destroy; override;
      function initialized : boolean;
      function get_states (year: year_range) : extended;
      function get_GPs (year: year_range) : extended;
      function get_con (year: year_range) : extended;
      function get_move (year: year_range) : extended;
      function get_move5 (year: year_range) : extended;
      function get_moveGP (year: year_range) : extended;
      function get_moveGP5 (year: year_range) : extended;
     private
      NumStatesArray, NumGPsArray, SysConArray, SysMoveArray, SysMove5Array,
         SysMoveGPArray, SysMoveGP5Array: system_year_matrix;
      created : boolean;
   end;                 {system variables object}


   {These are the objects to actually hold the user selected user data sets.}
   {The generic object is used so that I can declare a single variable holding
    all the user data sets, which may be of different types (dyadic, monadic, etc.).
    For each type, the way the data is stored and the retrieval commands are
    unique.  But, there are some general procedures that are common, like getting
    the start and end year of partitions, etc.}
   {Most calls need the data set number, because reference information about
    each data set is stored in the main configuration.User_data_set_info[] variable.}

   {Fundamental base type for a generic data record.}
   generic_values_record_type = record
         values : array of variant;
      end;

   Tgeneric_user_data_obj = class(Tobject)
    public
      constructor init (Input_data_set_number : integer;
               start_year, end_year : year_range); virtual; abstract;
         {init reads in from intermediate, external file.}
      destructor destroy ; virtual; abstract;
      function get_value (const data_set_num, config_var_num : integer; ccode1, ccode2 : ccode_range;
                          ayear : year_range; const user_selections: user_selection_type) : variant; overload; virtual; abstract;
      function get_value (const data_set_num, config_var_num : integer; ccode1, ccode2 : ccode_range;
                          ayear : year_range; const user_selections: user_selection_type; error_check : boolean) : variant; overload; virtual; abstract;
    private
      first_partition_year, last_partition_year : year_range;
      created : boolean;
      user_var_num_from_config_var_num : TIntegerArray;  {this will index var numbers to the stored array}
      function user_wanted (const data_set_num : integer; const invar_unit : variable_unit_of_analysis_type; const config_var_num : integer) : boolean;
      function initialized : boolean;
      function get_first_partition_year : year_range;
      function get_last_partition_year : year_range;
      procedure read_one_generic_data_line (var generic_file : text;
             var generic_data_rec: generic_values_record_type; var inccode1, inccode2 : ccode_range;
             var inyear : year_range;  const configuration : configuration_type;
             Input_data_set_number : integer; var badline : boolean);

   end;

   {main ccode array for data}
   ccode_year_array = array[ccode_index_range] of generic_values_record_type;
   ccode_year_ptr = ^ccode_year_array;

   {types and object for generic dyadic data.}
   dyad_year_array_type = array[ccode_index_range] of ccode_year_ptr;
   dyad_year_ptr = ^dyad_year_array_type;
   TgenericDyad_array_obj = class (Tgeneric_user_data_obj)
      constructor init (Input_data_set_number : integer;
               start_year, end_year : year_range); override;
         {init reads in from intermediate, external file.}
      destructor destroy ; override;
      function get_value (const data_set_num, config_var_num : integer; ccode1, ccode2 : ccode_range;
                          ayear : year_range; const user_selections: user_selection_type) : variant; overload; override;
      function get_value (const data_set_num, config_var_num : integer; ccode1, ccode2 : ccode_range;
                          ayear : year_range; const user_selections: user_selection_type; error_check : boolean) : variant; overload; override;
     private
      year_array : array[min_year..max_year] of dyad_year_ptr;
      function all_in_range (const indata_set_num, config_var_num : integer; ccode1, ccode2 : ccode_range; ayear : year_range; error_check : boolean): boolean;
      function sub_get (const data_set_num, user_var_num : integer; ccode1, ccode2 : ccode_range;
               ayear : year_range; const user_selections: user_selection_type) : variant;
   end;                                    {generic dyadic object}


   TgenericMonadic_array_obj = class (Tgeneric_user_data_obj)
      constructor init (Input_data_set_number : integer;
               start_year, end_year : year_range); override;
      destructor destroy ; override;
      function get_value (const data_set_num, config_var_num : integer; ccode1, ccode2 : ccode_range;
                          ayear : year_range; const user_selections: user_selection_type) : variant; overload; override;
      function get_value (const data_set_num, config_var_num : integer; ccode1, ccode2 : ccode_range;
                          ayear : year_range; const user_selections: user_selection_type; error_check : boolean) : variant; overload; override;
     private
      year_array : array[min_year..max_year] of ccode_year_ptr;
      function all_in_range (const data_set_num, config_var_num : integer; ccode1 : ccode_range; ayear : year_range; error_check : boolean): boolean;
      function sub_get (user_var_num : integer; ccode1: ccode_range;
               ayear : year_range; const user_selections: user_selection_type) : variant;
   end;                                    {generic monadic data object}


   TgenericAnnual_array_obj = class (Tgeneric_user_data_obj)
      constructor init (Input_data_set_number : integer;
               start_year, end_year : year_range); override;
      destructor destroy ; override;
      function get_value (const data_set_num, config_var_num : integer; ccode1, ccode2 : ccode_range;
                          ayear : year_range; const user_selections: user_selection_type) : variant; overload; override;
      function get_value (const data_set_num, config_var_num : integer; ccode1, ccode2 : ccode_range;
                          ayear : year_range; const user_selections: user_selection_type; error_check : boolean) : variant; overload; override;
     private
      year_array : array[min_year..max_year] of generic_values_record_type;
      function all_in_range (const data_set_num, config_var_num : integer; ayear : year_range; error_check : boolean): boolean;
      function sub_get (user_var_num : integer; 
               ayear : year_range; const user_selections: user_selection_type) : variant;
   end;                                    {generic annual data object}



   TMultiple_user_data_set_Obj = class(Tobject)
      constructor init (const configuration : configuration_type;
               const start_year, end_year : year_range);
      destructor destroy;
{      function get_varname (const data_set_num, var_num: integer) : string;
}      function get_value (const data_set_num, config_var_num : integer; const ccode1, ccode2 : ccode_range;
                          const year : year_range; const user_selections: user_selection_type) : variant; overload;
      function get_value (const data_set_num, config_var_num : integer; const ccode1, ccode2 : ccode_range;
                          const year : year_range; const user_selections: user_selection_type; error_check : boolean) : variant; overload;
     private
      data : array of Tgeneric_user_data_obj;
   end;

   Multiple_user_data_set_type = TMultiple_user_data_set_Obj;


   Function Tgeneric_dyadarray_obj_mem_overhead : longint;
   Function Tgeneric_dyadarray_obj_mem_per_year (var Input_data_set_number : integer;
            const configuration : configuration_type; const user_selections : user_selection_type) : longint;

   Function Tgeneric_monadarray_obj_mem_overhead : longint;
   Function Tgeneric_monadarray_obj_mem_per_year (var Input_data_set_number : integer;
            const configuration : configuration_type; const user_selections : user_selection_type) : longint;

   Function Tgeneric_annualarray_obj_mem_overhead : longint;
   Function Tgeneric_annualarray_obj_mem_per_year (var Input_data_set_number : integer;
            const configuration : configuration_type; const user_selections : user_selection_type) : longint;

   Function Tpolity_array_obj_mem_overhead : longint;
   Function Tpolity_array_obj_mem_per_year : longint;


implementation

   uses eutypes2, euinoutd, Errbx;

    { ------------------------------------------------------------ }

   {Methods for polity data}

   Function Tpolity_array_obj_mem_overhead : longint;
      begin
         Tpolity_array_obj_mem_overhead:= sizeof(Tpolity_array_obj) +
                 + sizeof(polity_year_array_ptr) + sizeof(polity_year_array)
                 + 2*sizeof(year_range) + sizeof(boolean);
      end;
   Function Tpolity_array_obj_mem_per_year : longint;
      begin
          Tpolity_array_obj_mem_per_year := sizeof(polity_array) +
             (max_ccode_index-min_ccode_index+1) * (sizeof(polity_data)+
                                                    sizeof(polity_data_ptr));
      end;

    { ------------------------------------------------------------ }

   function Tpolity_array_obj.polity_missing (avalue : integer) : boolean;
       {this only checks for polity values that are the special missing polity values,
        -66, -77, -88.  -99 and -9 were converted to missing value (-99) during input.}
      begin
         polity_missing := false;
         if (avalue < -10) then polity_missing := true;
      end;

    { ------------------------------------------------------------ }

   constructor Tpolity_array_obj.init (polity3_file_name : TFileName;
            first_proc_year, last_proc_year : year_range);
      var infile : text;
          ccode_loop, ccode_read : ccode_range;
          year_loop, year_read : year_range;
          polity_year_array : polity_array_ptr;
          ccode_year_data : polity_data_ptr;
          index : ccode_index_range;
          aval : longint;
          achar : char;
          temp : year_range;
          start_mem, one_polity_year_mem_needed, all_years_mem_needed, heapneeded : longint;
          record_count : longint;
          polity_trace : TTrace_obj;

      begin
         try
            polity_trace := nil;
            polity_trace := TTrace_obj.init(trace.get_trace_level);
            trace.enter ('Initializing polity data, '+inttostr(first_proc_year)+' to '
                         +inttostr(last_proc_year));
            start_mem := memavail;
            if first_proc_year > last_proc_year then
               switch_year (first_proc_year, last_proc_year);
            self.first_partition_year := first_proc_year;
            self.last_partition_year := last_proc_year;

            {this procedure needs existing ccode index and nation info to work.}
            created := false;
            if not(ccode_index.initialized) then
               begin
                  EUGeneError ('polity array creation called before ccode_index initialized',
                                  5, stop, error_log);
               end;
            if not(nation_list.initialized) then
               begin
                  EUGeneError ('polity array creation called before nation_list initialized',
                                  10, stop, error_log);
               end;

            {Initial matrix construction.  create cells/arrays for countries that are states
             in this time period}
            {First check available memory.}
            one_polity_year_mem_needed := Tpolity_array_obj_mem_per_year;
            all_years_mem_needed := (last_proc_year-first_proc_year+1) *
                                    one_polity_year_mem_needed;
            heapneeded := Tpolity_array_obj_mem_overhead + all_years_mem_needed;
            if debug[4] then
               begin
                  trace.message ('Polity array size calculation');
                  trace.message ('Calc is that '+inttostr(one_polity_year_mem_needed)+' needed per year.');
                  trace.message ('Calc is that '+inttostr((last_partition_year-first_partition_year+1))+' years needed.');
                  trace.message ('Calc is that '+inttostr(all_years_mem_needed)+' needed for all years.');
                  trace.message ('Max avail mem block is '+inttostr(MaxAvail));
               end;
            if MaxAvail <= (heapneeded) then
               begin
                  EUGeneError ('Not enough memory for polity array.',
                                  10, stop, error_log);
               end;

            new (data);
            for year_loop := min_year to max_year do
               begin
                  polity_trace.tick ('Executing Procedure: Initialize Polity Data',(max_year-min_year+1));
                  if (year_loop >= first_proc_year) and (year_loop <= last_proc_year) then
                  begin
                     new(polity_year_array);
                     data^[year_loop] := polity_year_array;
                     for ccode_loop := min_ccode to max_ccode do
                        begin
                           if nation_list.is_a_state_between(ccode_loop, first_proc_year, last_proc_year) then
                                {Note - initialize countries here.  Later, when read, might also
                                 have to add some new if there are some in the data that aren't
                                 in the "is_a_state_between" list based on ccode dates.}
                              begin
                                 new(ccode_year_data);
                                 ccode_year_data^.democ := missing_value_polity;
                                 ccode_year_data^.autoc := missing_value_polity;
                                 ccode_year_data^.xrreg := missing_value_polity;
                                 ccode_year_data^.xrcomp := missing_value_polity;
                                 ccode_year_data^.xropen := missing_value_polity;
                                 ccode_year_data^.mono := missing_value_polity;
                                 ccode_year_data^.xconst := missing_value_polity;
                                 ccode_year_data^.parreg := missing_value_polity;
                                 ccode_year_data^.parcomp := missing_value_polity;
                                 ccode_year_data^.cent := missing_value_polity;
                                 data^[year_loop]^[ccode_index.index(ccode_loop)] := ccode_year_data;
                              end
                           else
                              data^[year_loop]^[ccode_index.index(ccode_loop)] := nil;
                        end;
                  end  {if year in range}
                  else data^[year_loop] := nil;
               end;    {for year_loop}
            if debug[2] then
               trace.message ('Finished polity array initialization;  reading input file.');
            polity_trace.tickdone;

            try
               try
                  {Now, read in data from file}
                  assignFile (infile, polity3_file_name);
                  reset (infile);
                  record_count := 0;
                      {Note:  file may not be sorted, so must go through whole file}
                  while not eof (infile) do
                    begin
                     if not eoln(infile) then    {This cmd should make it skip blank lines.}
                       begin
                           {14000 records in polity 3 up to 1994}
                        polity_trace.tick ('Executing Procedure: Read Polity Data ',14000);
                        inc(record_count);
                            {first value is a record #, so just skip it}
                        aval := read_csv_int (infile);

                        aval := read_csv_int (infile);
                        if (aval <= max_ccode) and (aval >= min_ccode) then
                           ccode_read := aval
                        else
                           EUGeneError ('Error reading ccode from polity - ccode value of '
                                  + inttostr(ccode_read) + ' out of range in record ' + inttostr(record_count),
                                        4, continue, error_log);

                        {Next field is a 2 or 3 character name.  don't want that}
                        {the read_csv_int before read the space.  So now should have 2-3 alphas, then a space}
                        repeat
                           read (infile, achar);
                        until achar = ' ';

                        aval := read_csv_int (infile);
                        if (aval <= max_year) and (aval >= min_year) then
                           year_read := aval
                        else
                           EUGeneError ('Error reading year from polity - year value of '
                                  + inttostr(year_read) + ' out of range in record ' + inttostr(record_count),
                                        4, continue, error_log);

                        {There are some cases where I need to recode Polity ccodes.
                         Polity leaves gmy at ccode 260 after 1989.  COW goes back to 255
                         in 1990+;  also austria-hungary;  also serbia/yugoslavia;  also italy.}
                        if (year_read >= 1990) and (ccode_read = 260) then
                           ccode_read := 255;
                        if (year_read <= 1918) and (ccode_read = 305) then
                           ccode_read := 300;
                        if (year_read <= 1918) and (ccode_read = 310) then
                           ccode_read := 300;
                        if (year_read <= 1915) and (ccode_read = 344) then
                           ccode_read := 345;
                        if (year_read <= 1861) and (ccode_read = 324) then
                           ccode_read := 325;
                        {this will double read for 300 before 1919, but since cc305
                         and cc310 values are equal, this is OK.}

                        {Have ccode and year of this record.  If I want this ccode-year in this pass,
                         read the rest of the record, otherwise skip it. }
                        if ((year_read >= first_proc_year) and (year_read <= last_proc_year)) then
                           begin
                              {Check to be sure I have a record already created for this year-ccode combo.}
                              if data^[year_read]^[ccode_index.index(ccode_read)] = nil then
                                 begin
                                    new(ccode_year_data);
                                    data^[year_read]^[ccode_index.index(ccode_read)] := ccode_year_data;
                                 end;
                              data^[year_read]^[ccode_index.index(ccode_read)]^.autoc := read_csv_int (infile);
                              data^[year_read]^[ccode_index.index(ccode_read)]^.democ := read_csv_int (infile);
                              data^[year_read]^[ccode_index.index(ccode_read)]^.xrreg := read_csv_int (infile);
                              data^[year_read]^[ccode_index.index(ccode_read)]^.xrcomp := read_csv_int (infile);
                              data^[year_read]^[ccode_index.index(ccode_read)]^.xropen := read_csv_int (infile);
                              data^[year_read]^[ccode_index.index(ccode_read)]^.mono := read_csv_int (infile);
                              data^[year_read]^[ccode_index.index(ccode_read)]^.xconst := read_csv_int (infile);
                              data^[year_read]^[ccode_index.index(ccode_read)]^.parreg := read_csv_int (infile);
                              data^[year_read]^[ccode_index.index(ccode_read)]^.parcomp := read_csv_int (infile);
                              {There is something bizarre about the polity 3 input file, it has a #13 embedded
                               at the end of each line, and not a standard eoln character.  So I need a different
                               read for the end of the record.}
                              readln (infile, data^[year_read]^[ccode_index.index(ccode_read)]^.cent);
                              {read (infile, data^[year_read]^[ccode_index.index(ccode_read)]^.cent);
                              if eoln(infile) then readln (infile); }

                              {make sure any -9 or -99 missing value codes are set as -99.
                               Other missing value codes (-77, -88) should be left as is.}
                              if (data^[year_read]^[ccode_index.index(ccode_read)]^.autoc = -9) or
                                 (data^[year_read]^[ccode_index.index(ccode_read)]^.autoc = -99) then
                                 data^[year_read]^[ccode_index.index(ccode_read)]^.autoc := missing_value_polity;
                              if (data^[year_read]^[ccode_index.index(ccode_read)]^.democ = -9) or
                                 (data^[year_read]^[ccode_index.index(ccode_read)]^.democ = -99) then
                                 data^[year_read]^[ccode_index.index(ccode_read)]^.democ := missing_value_polity;
                              if (data^[year_read]^[ccode_index.index(ccode_read)]^.xrreg = -9) or
                                 (data^[year_read]^[ccode_index.index(ccode_read)]^.xrreg = -99) then
                                 data^[year_read]^[ccode_index.index(ccode_read)]^.xrreg := missing_value_polity;
                              if (data^[year_read]^[ccode_index.index(ccode_read)]^.xrcomp = -9) or
                                 (data^[year_read]^[ccode_index.index(ccode_read)]^.xrcomp = -99) then
                                 data^[year_read]^[ccode_index.index(ccode_read)]^.xrcomp := missing_value_polity;
                              if (data^[year_read]^[ccode_index.index(ccode_read)]^.xropen = -9) or
                                 (data^[year_read]^[ccode_index.index(ccode_read)]^.xropen = -99) then
                                 data^[year_read]^[ccode_index.index(ccode_read)]^.xropen := missing_value_polity;
                              if (data^[year_read]^[ccode_index.index(ccode_read)]^.mono = -9) or
                                 (data^[year_read]^[ccode_index.index(ccode_read)]^.mono = -99) then
                                 data^[year_read]^[ccode_index.index(ccode_read)]^.mono := missing_value_polity;
                              if (data^[year_read]^[ccode_index.index(ccode_read)]^.xconst = -9) or
                                 (data^[year_read]^[ccode_index.index(ccode_read)]^.xconst = -99) then
                                 data^[year_read]^[ccode_index.index(ccode_read)]^.xconst := missing_value_polity;
                              if (data^[year_read]^[ccode_index.index(ccode_read)]^.parreg = -9) or
                                 (data^[year_read]^[ccode_index.index(ccode_read)]^.parreg = -99) then
                                 data^[year_read]^[ccode_index.index(ccode_read)]^.parreg := missing_value_polity;
                              if (data^[year_read]^[ccode_index.index(ccode_read)]^.parcomp = -9) or
                                 (data^[year_read]^[ccode_index.index(ccode_read)]^.parcomp = -99) then
                                 data^[year_read]^[ccode_index.index(ccode_read)]^.parcomp := missing_value_polity;
                              if (data^[year_read]^[ccode_index.index(ccode_read)]^.cent = -9) or
                                 (data^[year_read]^[ccode_index.index(ccode_read)]^.cent = -99) then
                                 data^[year_read]^[ccode_index.index(ccode_read)]^.cent := missing_value_polity;
                           end    {year_read in appropriate range}
                        else
                           begin   {not in appropriate partition/year range}
                               {Don't want this record, do nothing but go to next record}
                               readln (infile);
                           end;
                            {Set to read next record, if there is one.}
                       end;   {if not eoln then ...}
                       {Because I have to do a funky read before, no readln is needed here.}
                       {if (not (eof(infile)))  then
                          readln(infile);  }
                    end;     {while not eof infile}
               finally
                  CloseFile (infile);
                  polity_trace.tickdone;
               end;
            except
               on EUserInterrupt do raise;
               on EInOutError do
                 begin
                    FileErrorBox.maindo ('Error opening file "'+polity3_file_name+ '"',
                                         'File could not be opened for input.',
                                         'File may be in use by another program, or may be missing.');
                    FileErrorBox.showmodal;
                    raise;
                 end;
            end;

            created := true;
            trace.message ('polity data required ' +
                           inttostr(round(((start_mem-memavail)/1024))) + ' Kilobytes of memory');
            polity_trace.tickdone;
         finally
            polity_trace.free;
            trace.exit('Finished initializing polity data');
         end;
      end;


    { ------------------------------------------------------------ }

   destructor Tpolity_array_obj.destroy;
      var ccode_loop : ccode_range;
          year_loop : year_range;
      begin
         try
            if self <> nil then
            begin
              if data <> nil then
                begin
                  for year_loop := min_year to max_year do
                   if data^[year_loop] <> nil then
                    begin
                     for ccode_loop := min_ccode to max_ccode do
                      if data^[year_loop]^[ccode_index.index(ccode_loop)] <> nil then
                       begin
                        dispose (data^[year_loop]^[ccode_index.index(ccode_loop)]);
                        data^[year_loop]^[ccode_index.index(ccode_loop)] := nil;
                       end;
                     dispose (data^[year_loop]);
                     data^[year_loop] := nil;
                    end;
                 dispose(data);
                 data := nil;
               end;
               created := false;
               inherited destroy;
            end;   {self <> nil}
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;    {destructor}

    { ------------------------------------------------------------ }

   function Tpolity_array_obj.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then if created=true then initialized := true;
      end;

       { ------------------------------------------------------------ }

   function Tpolity_array_obj.all_in_range (accode: ccode_range; ayear : year_range; error_check: boolean): boolean;
      begin
         all_in_range := true;
         if not(initialized) then
            begin
               all_in_range := false;
               if error_check then
                  begin
                     EUGeneError ('Get a value from polity array called before initialization',
                                     5, continue, error_log);
                     trace.message ('called polity array get before initialization');
                  end;
            end
         else
         if not ((ayear >= first_partition_year) and (ayear <= last_partition_year)) then
            begin
               all_in_range := false;
               if error_check then
                  begin
                     EUGeneError ('Internal Error in program - called Get a value from polity array with year outside partition',
                                     5, continue, error_log);
                     trace.message ('called polity array with year outside partition, '+inttostr(ayear));
                  end;
            end
         else
         if not (nation_list.is_a_state(accode, ayear)) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        trace.message('polity variable get called for ccode that is not a state (' +
                              inttostr(accode)+' in '+inttostr(ayear)+'). ');
                        trace.message ('Polity value set to missing');
                     end;
               end;
      end;

    { ------------------------------------------------------------ }

   function Tpolity_array_obj.get_autoc (ccode: ccode_range; year: year_range) : integer;
      {returns autoc value}
      begin
         get_autoc := missing_value_polity;
         if all_in_range (ccode, year, true) then
            get_autoc := data^[year]^[ccode_index.index(ccode)]^.autoc;
      end;

   function Tpolity_array_obj.get_autoc (ccode: ccode_range; year: year_range; error_check: boolean) : integer;
      begin
         get_autoc := missing_value_polity;
         if all_in_range (ccode, year, error_check) then
            get_autoc := data^[year]^[ccode_index.index(ccode)]^.autoc;
      end;


   function Tpolity_array_obj.get_democ (ccode: ccode_range; year: year_range) : integer;
      begin
         get_democ := missing_value_polity;
         if all_in_range (ccode, year, true) then
            get_democ := data^[year]^[ccode_index.index(ccode)]^.democ;
      end;

   function Tpolity_array_obj.get_democ (ccode: ccode_range; year: year_range; error_check: boolean) : integer;
      begin
         get_democ := missing_value_polity;
         if all_in_range (ccode, year, error_check) then
            get_democ := data^[year]^[ccode_index.index(ccode)]^.democ;
      end;


   function Tpolity_array_obj.get_xrreg (ccode: ccode_range; year: year_range) : integer;
      begin
         get_xrreg := missing_value_polity;
         if all_in_range (ccode, year, true) then
            get_xrreg := data^[year]^[ccode_index.index(ccode)]^.xrreg;
      end;

   function Tpolity_array_obj.get_xrreg (ccode: ccode_range; year: year_range; error_check: boolean) : integer;
      begin
         get_xrreg := missing_value_polity;
         if all_in_range (ccode, year, error_check) then
            get_xrreg := data^[year]^[ccode_index.index(ccode)]^.xrreg;
      end;


   function Tpolity_array_obj.get_xrcomp (ccode: ccode_range; year: year_range) : integer;
      begin
         get_xrcomp := missing_value_polity;
         if all_in_range (ccode, year, true) then
            get_xrcomp := data^[year]^[ccode_index.index(ccode)]^.xrcomp;
      end;

   function Tpolity_array_obj.get_xrcomp (ccode: ccode_range; year: year_range; error_check: boolean) : integer;
      begin
         get_xrcomp := missing_value_polity;
         if all_in_range (ccode, year, error_check) then
            get_xrcomp := data^[year]^[ccode_index.index(ccode)]^.xrcomp;
      end;


   function Tpolity_array_obj.get_xropen (ccode: ccode_range; year: year_range) : integer;
      begin
         get_xropen := missing_value_polity;
         if all_in_range (ccode, year, true) then
            get_xropen := data^[year]^[ccode_index.index(ccode)]^.xropen;
      end;

   function Tpolity_array_obj.get_xropen (ccode: ccode_range; year: year_range; error_check: boolean) : integer;
      begin
         get_xropen := missing_value_polity;
         if all_in_range (ccode, year, error_check) then
            get_xropen := data^[year]^[ccode_index.index(ccode)]^.xropen;
      end;

   function Tpolity_array_obj.get_mono (ccode: ccode_range; year: year_range) : integer;
      begin
         get_mono := missing_value_polity;
         if all_in_range (ccode, year, true) then
            get_mono := data^[year]^[ccode_index.index(ccode)]^.mono;
      end;

   function Tpolity_array_obj.get_mono (ccode: ccode_range; year: year_range; error_check: boolean) : integer;
      begin
         get_mono := missing_value_polity;
         if all_in_range (ccode, year, error_check) then
            get_mono := data^[year]^[ccode_index.index(ccode)]^.mono;
      end;


   function Tpolity_array_obj.get_xconst (ccode: ccode_range; year: year_range) : integer;
      begin
         get_xconst := missing_value_polity;
         if all_in_range (ccode, year, true) then
            get_xconst := data^[year]^[ccode_index.index(ccode)]^.xconst;
      end;

   function Tpolity_array_obj.get_xconst (ccode: ccode_range; year: year_range; error_check: boolean) : integer;
      begin
         get_xconst := missing_value_polity;
         if all_in_range (ccode, year, error_check) then
            get_xconst := data^[year]^[ccode_index.index(ccode)]^.xconst;
      end;


   function Tpolity_array_obj.get_parreg (ccode: ccode_range; year: year_range) : integer;
      begin
         get_parreg := missing_value_polity;
         if all_in_range (ccode, year, true) then
            get_parreg := data^[year]^[ccode_index.index(ccode)]^.parreg;
      end;

   function Tpolity_array_obj.get_parreg (ccode: ccode_range; year: year_range; error_check: boolean) : integer;
      begin
         get_parreg := missing_value_polity;
         if all_in_range (ccode, year, error_check) then
            get_parreg := data^[year]^[ccode_index.index(ccode)]^.parreg;
      end;


   function Tpolity_array_obj.get_parcomp (ccode: ccode_range; year: year_range) : integer;
      begin
         get_parcomp := missing_value_polity;
         if all_in_range (ccode, year, true) then
            get_parcomp := data^[year]^[ccode_index.index(ccode)]^.parcomp;
      end;

   function Tpolity_array_obj.get_parcomp (ccode: ccode_range; year: year_range; error_check: boolean) : integer;
      begin
         get_parcomp := missing_value_polity;
         if all_in_range (ccode, year, error_check) then
            get_parcomp := data^[year]^[ccode_index.index(ccode)]^.parcomp;
      end;


   function Tpolity_array_obj.get_cent (ccode: ccode_range; year: year_range) : integer;
      begin
         get_cent := missing_value_polity;
         if all_in_range (ccode, year, true) then
            get_cent := data^[year]^[ccode_index.index(ccode)]^.cent;
      end;

   function Tpolity_array_obj.get_cent (ccode: ccode_range; year: year_range; error_check: boolean) : integer;
      begin
         get_cent := missing_value_polity;
         if all_in_range (ccode, year, error_check) then
            get_cent := data^[year]^[ccode_index.index(ccode)]^.cent;
      end;


   function Tpolity_array_obj.get_autoclg (ccode: ccode_range; year: year_range) : integer;
      begin
         get_autoclg := missing_value_polity;
         if all_in_range (ccode, year-1, true) then
            get_autoclg := get_autoc(ccode, year-1);
      end;

   function Tpolity_array_obj.get_autoclg (ccode: ccode_range; year: year_range; error_check: boolean) : integer;
      begin
         get_autoclg := missing_value_polity;
         if all_in_range (ccode, year-1, error_check) then
            get_autoclg := get_autoc(ccode, year-1);
      end;


   function Tpolity_array_obj.get_democlg (ccode: ccode_range; year: year_range) : integer;
      begin
         get_democlg := missing_value_polity;
         if all_in_range (ccode, year-1, true) then
            get_democlg := get_democ(ccode, year-1);
      end;

   function Tpolity_array_obj.get_democlg (ccode: ccode_range; year: year_range; error_check: boolean) : integer;
      begin
         get_democlg := missing_value_polity;
         if all_in_range (ccode, year-1, error_check) then
            get_democlg := get_democ(ccode, year-1);
      end;


   function Tpolity_array_obj.get_dem (ccode: ccode_range; year: year_range) : integer;
      begin
         get_dem := missing_value_polity;
         if all_in_range (ccode, year, true) then
         begin
            if not (polity_missing(get_democ(ccode, year))) and not (polity_missing(get_autoc(ccode, year))) and
               (get_democ(ccode, year) <> missing_value_polity) and (get_autoc(ccode, year) <> missing_value_polity) then
              get_dem := get_democ(ccode, year) - get_autoc(ccode, year);
            if (((get_democ(ccode, year) = -66) and (get_autoc(ccode, year) = -66))
             or ((get_democ(ccode, year) = -77) and (get_autoc(ccode, year) = -77))
             or ((get_democ(ccode, year) = -88) and (get_autoc(ccode, year) = -88)))
             then get_dem := get_democ(ccode, year);
         end;
      end;

   function Tpolity_array_obj.get_dem (ccode: ccode_range; year: year_range; error_check: boolean) : integer;
      begin
         get_dem := missing_value_polity;
         if all_in_range (ccode, year, error_check) then
         begin
            if not (polity_missing(get_democ(ccode, year))) and not (polity_missing(get_autoc(ccode, year))) and
               (get_democ(ccode, year) <> missing_value_polity) and (get_autoc(ccode, year) <> missing_value_polity) then
              get_dem := get_democ(ccode, year) - get_autoc(ccode, year);
            if (((get_democ(ccode, year) = -66) and (get_autoc(ccode, year) = -66))
             or ((get_democ(ccode, year) = -77) and (get_autoc(ccode, year) = -77))
             or ((get_democ(ccode, year) = -88) and (get_autoc(ccode, year) = -88)))
             then get_dem := get_democ(ccode, year);
         end;
      end;


   function Tpolity_array_obj.get_demlg (ccode: ccode_range; year: year_range) : integer;
      begin      {don't really need to do range checks here, b/c they will be done inside get_democlg
                  and get_autoclg procedures, for those appropiate years.  Only the results
                  need to be checked here.}
         get_demlg := missing_value_polity;
         if (all_in_range (ccode, year-1, true)) then
         begin
            if ((not (polity_missing(get_democlg(ccode, year)))) and (not (polity_missing(get_autoclg(ccode, year)))))
               and (get_democlg(ccode, year) <> missing_value_polity) and (get_autoclg(ccode, year) <> missing_value_polity) then
                 get_demlg := get_democlg(ccode, year) - get_autoclg(ccode, year);
            if (((get_democlg(ccode, year) = -66) and (get_autoclg(ccode, year) = -66))
             or ((get_democlg(ccode, year) = -77) and (get_autoclg(ccode, year) = -77))
             or ((get_democlg(ccode, year) = -88) and (get_autoclg(ccode, year) = -88)))
             then get_demlg := get_democlg(ccode, year);
         end;
      end;

   function Tpolity_array_obj.get_demlg (ccode: ccode_range; year: year_range; error_check: boolean) : integer;
      begin      {don't really need to do range checks here, b/c they will be done inside get_democlg
                  and get_autoclg procedures, for those appropiate years.  Only the results
                  need to be checked here.}
         get_demlg := missing_value_polity;
         if (all_in_range (ccode, year-1, error_check)) then
         begin
            if ((not (polity_missing(get_democlg(ccode, year)))) and (not (polity_missing(get_autoclg(ccode, year)))))
               and (get_democlg(ccode, year) <> missing_value_polity) and (get_autoclg(ccode, year) <> missing_value_polity) then
                 get_demlg := get_democlg(ccode, year) - get_autoclg(ccode, year);
            if (((get_democlg(ccode, year) = -66) and (get_autoclg(ccode, year) = -66))
             or ((get_democlg(ccode, year) = -77) and (get_autoclg(ccode, year) = -77))
             or ((get_democlg(ccode, year) = -88) and (get_autoclg(ccode, year) = -88)))
             then get_demlg := get_democlg(ccode, year);
         end;
      end;


   function Tpolity_array_obj.get_demchg (ccode: ccode_range; year: year_range) : integer;
      begin
         get_demchg := missing_value_polity;
         if (all_in_range (ccode, year, true)) and (all_in_range (ccode, year-1, true)) then
         if not (polity_missing(get_dem(ccode, year))) and not (polity_missing(get_demlg(ccode, year))) then
            get_demchg := get_dem(ccode, year) - get_demlg(ccode, year);
      end;

   function Tpolity_array_obj.get_demchg (ccode: ccode_range; year: year_range; error_check: boolean) : integer;
      begin
         get_demchg := missing_value_polity;
         if (all_in_range (ccode, year, error_check)) and (all_in_range (ccode, year-1, error_check)) then
         if not (polity_missing(get_dem(ccode, year))) and not (polity_missing(get_demlg(ccode, year))) then
            get_demchg := get_dem(ccode, year) - get_demlg(ccode, year);
      end;

    { ------------------------------------------------------------ }

   constructor Tsystem_variables_obj.init (sys_capability_data : Tsys_capability_array_obj) ;
      var ccode_inc : ccode_range;
          year_inc : year_range;
          totalcap, totalGPcap, smallnation, smallmajor : system_year_matrix;
          consum, movesum, moveGPsum : extended;
          insysrange : boolean;
          current, last, currentGP, lastGP: extended;

      begin
         try
            created := false;
            for year_inc := min_year to max_year do
            begin
               NumStatesArray[year_inc] := 0;
               NumGPsArray[year_inc] := 0;
               SysConArray[year_inc] := 0;
               SysMoveArray[year_inc] := 0;
               SysMove5Array[year_inc] := 0;
               SysMoveGPArray[year_inc] := 0;
               SysMoveGP5Array[year_inc] := 0;
               totalcap[year_inc] := 0;
               totalGPcap[year_inc] := 0;
               smallnation[year_inc] := 0;
               smallmajor[year_inc] := 0;
            end;

            {First we determine which states exist, and which ones are majors.  We sum up the total
             capabilities across the entire system, and the total capabilities of GPs.  We set the
             value of the smallest capability in the whole system (smallnation) and among the GPs
             (smallmajor).  Any year which doesn't have a value after this process is complete is
             set to missing.  }
            for year_inc := min_year to max_year do
            begin
               if (year_inc >= configuration.first_cap_year) and (year_inc <= configuration.last_cap_year)
                  then insysrange := true
                  else insysrange := false;
               for ccode_inc := min_ccode to max_ccode do
               begin
                  if nation_list.is_a_state(ccode_inc,year_inc) then
                  begin
                     NumStatesArray[year_inc] := NumStatesArray[year_inc] + 1;
                     if nation_list.is_a_GP(ccode_inc,year_inc) then NumGPsArray[year_inc] := NumGPsArray[year_inc] + 1;
                     if sys_capability_data.get_syscap(ccode_inc,year_inc,insysrange) <> missing_value then
                     begin
                        totalcap[year_inc] := totalcap[year_inc] + sys_capability_data.get_syscap(ccode_inc,year_inc,insysrange);
                        if nation_list.is_a_GP(ccode_inc,year_inc)
                           then totalGPcap[year_inc] := totalGPcap[year_inc] + sys_capability_data.get_syscap(ccode_inc,year_inc,insysrange);
                     end;
                     if (sys_capability_data.get_syscap(ccode_inc, year_inc,insysrange) < smallnation[year_inc])
                        or (smallnation[year_inc] = 0) then
                           if nation_list.is_a_state(ccode_inc,year_inc) and nation_list.is_a_state(ccode_inc, year_inc - 1)
                              and (sys_capability_data.get_syscap(ccode_inc,year_inc,insysrange) <> missing_value)
                              then smallnation[year_inc] := sys_capability_data.get_syscap(ccode_inc, year_inc,insysrange);
                     if (sys_capability_data.get_syscap(ccode_inc, year_inc,insysrange) < smallmajor[year_inc])
                        or (smallmajor[year_inc] = 0) then
                           if nation_list.is_a_GP(ccode_inc,year_inc) and nation_list.is_a_GP(ccode_inc, year_inc - 1)
                              and (sys_capability_data.get_syscap(ccode_inc,year_inc,insysrange) <> missing_value)
                              then smallmajor[year_inc] := sys_capability_data.get_syscap(ccode_inc, year_inc,insysrange);
                  end;
               end;         {for ccode}
               if NumStatesArray[year_inc] = 0 then NumStatesArray[year_inc] := missing_value;
               if NumGPsArray[year_inc] = 0 then NumGPsArray[year_inc] := missing_value;
            end;        {for year}

            {Now we do the summations that are in the numerator of the CON and MOVE scores (consum
             and movesum).  SysConArray and SysMoveArray are then easily calculated by plugging the
             relevant sum into the formula.}
            for year_inc := min_year to max_year do
            begin
               consum := 0;
               movesum := 0;
               moveGPsum := 0;
               if (year_inc >= configuration.first_cap_year) and (year_inc <= configuration.last_cap_year)
                  then insysrange := true
                  else insysrange := false;
               if insysrange then
               begin

                  for ccode_inc := min_ccode to max_ccode do
                  begin
                     if nation_list.is_a_state(ccode_inc,year_inc) then
                     begin
                        {add to consum}
                        if (totalcap[year_inc] <> 0) and (sys_capability_data.get_syscap(ccode_inc, year_inc, insysrange) <> missing_value) then
                           consum := consum + sqr(sys_capability_data.get_syscap(ccode_inc,year_inc,insysrange));
                        {compute for move only if state in both years.}
                        if (nation_list.is_a_state(ccode_inc,year_inc)
                            and nation_list.is_a_state(ccode_inc, year_inc - 1)) then
                           begin
                              current := 0;
                              last := 0;
                              if ((totalcap[year_inc] <> 0) and (sys_capability_data.get_syscap(ccode_inc, year_inc, insysrange) <> missing_value))
                                 then current := (sys_capability_data.get_syscap(ccode_inc,year_inc,insysrange)) / totalcap[year_inc];
                              if ((totalcap[year_inc - 1] <> 0) and (sys_capability_data.get_syscap(ccode_inc, (year_inc-1), insysrange) <> missing_value))
                                 then last := (sys_capability_data.get_syscap(ccode_inc,(year_inc - 1),insysrange)) / totalcap[year_inc - 1];
                              if ((totalcap[year_inc] <> 0) and (totalcap[year_inc - 1] <> 0) ) then
                                 movesum := movesum + abs( current - last  );
                           end;
                        if (nation_list.is_a_GP(ccode_inc,year_inc)
                            and nation_list.is_a_GP(ccode_inc, year_inc - 1)) then
                           begin
                              currentGP := 0;
                              lastGP := 0;
                              if ((totalGPcap[year_inc] <> 0) and (sys_capability_data.get_syscap(ccode_inc, year_inc, insysrange) <> missing_value))
                                 then currentGP := (sys_capability_data.get_syscap(ccode_inc,year_inc,insysrange)) / totalGPcap[year_inc];
                              if ((totalGPcap[year_inc - 1] <> 0) and (sys_capability_data.get_syscap(ccode_inc, (year_inc-1), insysrange) <> missing_value))
                                 then lastGP := (sys_capability_data.get_syscap(ccode_inc,(year_inc - 1),insysrange)) / totalGPcap[year_inc - 1];
                              if ((totalGPcap[year_inc] <> 0) and (totalGPcap[year_inc - 1] <> 0) ) then
                                 moveGPsum := moveGPsum + abs( currentGP - lastGP  );
                           end;
                     end;
                  end;
                  if NumStatesArray[year_inc] <> missing_value then
                     SysConArray[year_inc] := sqrt( (consum - (1 / NumStatesArray[year_inc]) )
                                                  / (1 -      (1 / NumStatesArray[year_inc]) ) );
                  if smallnation[year_inc] <> 0 then
                     SysMoveArray[year_inc] := movesum / (2 * (1 - smallnation[year_inc]));
                  if smallmajor[year_inc] <> 0 then
                     SysMoveGPArray[year_inc] := moveGPsum / (2 * (1 - smallmajor[year_inc]));
               end
               else  {Not in sys capability range; all CON and MOVE scores will be missing}
               begin
                  SysConArray[year_inc] := missing_value;
                  SysMoveArray[year_inc] := missing_value;
                  SysMoveGPArray[year_inc] := missing_value;
               end;

               {Now we do the five year averages.}
               if insysrange then
                  if (SysMoveArray[year_inc] <> missing_value) and (SysMoveArray[year_inc - 1] <> missing_value) and
                     (SysMoveArray[year_inc - 2] <> missing_value) and (SysMoveArray[year_inc - 3] <> missing_value) and
                     (SysMoveArray[year_inc - 4] <> missing_value)
                     then SysMove5Array[year_inc] := ( SysMoveArray[year_inc] + SysMoveArray[year_inc - 1] +
                                         SysMoveArray[year_inc - 2] + SysMoveArray[year_inc - 3] +
                                         SysMoveArray[year_inc - 4] ) / 5
                     else SysMove5Array[year_inc] := missing_value;
                  if (SysMoveGPArray[year_inc] <> missing_value) and (SysMoveGPArray[year_inc - 1] <> missing_value) and
                     (SysMoveGPArray[year_inc - 2] <> missing_value) and (SysMoveGPArray[year_inc - 3] <> missing_value) and
                     (SysMoveGPArray[year_inc - 4] <> missing_value)
                     then SysMoveGP5Array[year_inc] := ( SysMoveGPArray[year_inc] + SysMoveGPArray[year_inc - 1] +
                                         SysMoveGPArray[year_inc - 2] + SysMoveGPArray[year_inc - 3] +
                                         SysMoveGPArray[year_inc - 4] ) / 5
                     else SysMoveGP5Array[year_inc] := missing_value;
               created := true;
            end;
         finally
         end;
      end;


   destructor Tsystem_variables_obj.destroy;

      begin
         try
            if self <> nil then
            begin
               created := false;
               inherited destroy;
            end;   {self <> nil}
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;    {destructor}

   function Tsystem_variables_obj.initialized : boolean;

      begin
         initialized := false;
         if self <> nil then if created=true then initialized := true;
      end;

   function Tsystem_variables_obj.get_states (year: year_range): extended;

      begin
         get_states := NumStatesArray[year];
      end;

   function Tsystem_variables_obj.get_GPs (year: year_range): extended;

      begin
         get_GPs := NumGPsArray[year];
      end;

   function Tsystem_variables_obj.get_con (year: year_range): extended;

      begin
         get_con := SysConArray[year];
      end;

   function Tsystem_variables_obj.get_move (year: year_range): extended;

      begin
         get_move := SysMoveArray[year];
      end;

   function Tsystem_variables_obj.get_move5 (year: year_range): extended;

      begin
         get_move5 := SysMove5Array[year];
      end;

   function Tsystem_variables_obj.get_moveGP (year: year_range): extended;

      begin
         get_moveGP := SysMoveGPArray[year];
      end;

   function Tsystem_variables_obj.get_moveGP5 (year: year_range): extended;

      begin
         get_moveGP5 := SysMoveGP5Array[year];
      end;


    { ------------------------------------------------------------ }

{   function Tgeneric_user_data_obj.get_value (user_var_num : integer; ccode1, ccode2 : ccode_range;
                          ayear : year_range; user_selections: user_selection_type) : variant;
         begin
            EUGeneError ('program called non-overridden get_value method for generic user data object.  Notify programmer of error.', 4, stop, error_log);
         end;
   function Tgeneric_user_data_obj.get_value (user_var_num : integer; ccode1, ccode2 : ccode_range;
                          ayear : year_range; user_selections: user_selection_type; error_check : boolean) : variant;
         begin
            EUGeneError ('program called non-overridden get_value method for generic user data object.  Notify programmer of error.', 4, stop, error_log);
         end;
}

                   { -------------------------------- }

{Generic user data procedures;  these are not specific to monad, dyad, etc. types}

   function Tgeneric_user_data_obj.user_wanted (const data_set_num : integer; const invar_unit : variable_unit_of_analysis_type; const config_var_num : integer) : boolean;
      {returns true if this configuration variable number is one that the user wants
       in the output data set, or if it's a key identifier variable=ccode1, ccode2, year.}
      {Note:  this will be called before data structures fully initialized, so don't
       check for data structure initialization here.}
      begin
         {if value = -1, then returns false.  If <> -1, then rtns true}
         result := (user_var_num_from_config_var_num[config_var_num] <> -1);
         {also need to check var config type, b/c also need to keep ccode, year identifiers.}
         if (invar_unit = identifierccode1) or
            (invar_unit = identifierccode2) or
            (invar_unit = identifieryear) then result := true;
         {Also need to read reverse vars when appropriate.}
         if result = false then
            if configuration.User_data_set_info.get_data_set_var_reversed_var(data_set_num, config_var_num) <> 'none' then
            result := user_var_num_from_config_var_num[configuration.User_data_set_info.get_data_set_var_number(data_set_num, configuration.User_data_set_info.get_data_set_var_reversed_var(data_set_num, config_var_num))] <> -1;
      end;

   function Tgeneric_user_data_obj.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then if created=true then initialized := true;
      end;

                   { -------------------------------- }

   function Tgeneric_user_data_obj.get_first_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Get first partition year from generic data set called before initialization.  Programming error.', 5, stop, error_log);
            end
         else
            get_first_partition_year := first_partition_year;
      end;

                   { -------------------------------- }

   function Tgeneric_user_data_obj.get_last_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Get last partition year from generic data set called before initialization.  Programming error.', 5, stop, error_log);
            end
         else
            get_last_partition_year := last_partition_year;
      end;

   procedure Tgeneric_user_data_obj.read_one_generic_data_line (var generic_file : text;
             var generic_data_rec: generic_values_record_type; var inccode1, inccode2 : ccode_range;
             var inyear : year_range;  const configuration : configuration_type;
             Input_data_set_number : integer; var badline : boolean);
      var saved_data_position : integer;
          input_var_num : integer;
          invar : string;
          valtosave : variant;
          this_var_unit : variable_unit_of_analysis_type;

      begin
         try
              {read one record}
            badline := false;
            saved_data_position := 0;
              {first loop goes through every var as given in initial config file structure.}
            for input_var_num := 0 to configuration.user_data_set_info.get_data_set_num_vars(Input_data_set_number) - 1 do
            {input_var_num counts which var on the line I'm on; saved counts where in stored array I'm putting it.}
              begin
                 invar := read_csv_string(generic_file);

                 {If user wants this record, or key identifier, then save it.
                  User wanted func will also keep the key identifiers.}
                 this_var_unit := configuration.user_data_set_info.get_data_set_var_unit(Input_data_set_number,input_var_num);
                 if user_wanted(Input_data_set_number,this_var_unit,input_var_num) then
                    begin {finish processing this variable}
                       {Mark missing values special}
                       if ((invar = '') or (invar = '.')) then
                          begin
                             valtosave := configuration.user_data_set_info.get_data_set_var_missing_value(Input_data_set_number,input_var_num);
                             {check to make sure not missing in essential variables}
                             if (this_var_unit = identifierccode1) or
                                (this_var_unit = identifierccode2) or
                                (this_var_unit = identifieryear) then
                                EUGeneError ('User data set not usable - identifier ccode or year is blank.  All data in this data file will be missing, and EUGene run will probably not function appropriately.',2, continue, error_log)
                          end
                       else      {convert to normal output value}
                          case configuration.user_data_set_info.get_data_set_var_type(Input_data_set_number,input_var_num) of
                             varinteger : valtosave := strtoint(invar);
                             varolestr : valtosave := invar;
                             varsingle : valtosave := strtoFloat(invar);
                          end;   {case to convert data string to values.}

                       {Now, depending on what the variable is, save it.}
                       case this_var_unit of
                             identifierccode1 : inccode1 := valtosave;
                             identifierccode2 : inccode2 := valtosave;
                             identifieryear : inyear := valtosave;
                          else   {it is not a special variable type, so normal data record save.}
                             begin
                                generic_data_rec.values[saved_data_position] := valtosave;
                                inc(saved_data_position);
                             end;
                          end;   { case }
                    end;      {if user_wanted}

              end;   {for input var num}
            if saved_data_position-1 <> length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables)-1 then EUGeneError ('Number of elements read from input file not equal to number of elements specified in data set setup.  Data may be off.',3, continue, error_log);
            readln (generic_file);
            {Line of data now read and processed.  }
         except
            EUGeneError ('Error reading a data line in user input file.  Recheck data files.',3, continue, error_log);
            badline := true;
         end;   {except}
      end;     {procedure read_one_generic_data_line}

    { ------------------------------------------------------------ }
{Now procedures for dyadic object}

   constructor TgenericDyad_array_obj.init (Input_data_set_number : integer;
               start_year, end_year : year_range);
         {init reads in from intermediate, external file.}
      var x : integer;
          inccode1, inccode2 : ccode_range;
          inyear, ayear : year_range;
          ccode1, ccode2 : ccode_range;
          generic_file : text;
          generic_trace : Ttrace_obj;
          start_mem, one_year_generic_memory_needed, heapneeded : longint;
          generic_data_year : dyad_year_ptr;
          generic_data_year2 : ccode_year_ptr;
          ccode_index_loop : ccode_index_range;
          generic_data_rec : generic_values_record_type;
          valtosave : variant;
          First_year_possible, last_year_possible, First_year_to_read, last_year_to_read : year_range;
          badline : boolean;
          cases_read : longint;

      begin
         generic_trace := nil;
         try
            try
               start_mem := memavail;
               trace.enter('Initializing generic data, '+inttostr(First_year_to_read)+' to '+inttostr(Last_year_to_read)+
                 ' from generic data set ' + configuration.user_data_set_info.get_data_set_short_name(Input_data_set_number));
               generic_trace := TTrace_obj.init(trace.get_trace_level);

               {I will set up a reference to the var #s because I will not need to keep
                the ccode and year, or variables that users don't want.  They are unnecessary.}
               {In each record, for a dyad that is actually used/assigned to,
                there is actually a different effective number of vars that I
                am keeping data on.  I don't need to keep ccode1, 2, and year
                because they are the indices.  I also don't want to keep other vars
                that the user hasn't requested data on.}
               {Set up index to the array where data will be, that will be accessed with
                the number in the original configuration var list.
                user_var_num_from_config_var_num[3] will give the storage location of configuration variable 3.}
               setlength(user_var_num_from_config_var_num, configuration.user_data_set_info.get_data_set_num_vars(Input_data_set_number));
               {initialize all to - 1}
               for x := 0 to configuration.user_data_set_info.get_data_set_num_vars(Input_data_set_number)-1 do
                  user_var_num_from_config_var_num[x] := -1;   {initially, invalid location.}
               for x := 0 to length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables)-1 do
                  begin
                     user_var_num_from_config_var_num[user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables[x]] := x;
                  end;

               {now, to access some data for config var 3, access the right spot in the array with
                 ccode1, ccode2, year, user_var_num_from_config_var_num(var_number)}


               {Before reading, I now need to check and see if data actually exists
                for the time period in question.  If not, I don't need the data read.
                This wasn't done before because each data set is different. }
               {if the program is calling to initialize data for a range that is not possible
                because of the scope of the input data set, it will initialize it just to nil
                data.  Can tell this by setting the first, last year to read as follows.
                Start_year is always < end_year in this procedure.
                If First > Last, then the data in the data set does not overlap the requested
                initialization range, and so nil data will be created.}
               First_year_possible := configuration.user_data_set_info.get_data_set_first_year_possible(Input_data_set_number);
               Last_year_possible := configuration.user_data_set_info.get_data_set_last_year_possible(Input_data_set_number);

               if ((start_year >= First_year_possible) and
                   (start_year <= Last_year_possible )) or
                  ((end_year >= First_year_possible) and
                   (end_year <= Last_year_possible )) or
                  ((start_year <= First_year_possible) and
                   (end_year >= Last_year_possible)) then
                  begin     {can read data}

                     {set years to be read}
                     First_year_to_read := max (start_year, First_year_possible);
                     Last_year_to_read := min(end_year,Last_year_possible);
                     {Now set the internal partition years.  }
                     self.first_partition_year := First_year_to_read;
                     self.last_partition_year := Last_year_to_read;

                     try
                        {Now initialize the data structure.  Give every legitimate state pair
                         the initialized value.}
                        generic_trace.message ('Initializing generic dyadic structure for '+ configuration.user_data_set_info.get_data_set_short_name(Input_data_set_number));
                        one_year_generic_memory_needed := Tgeneric_dyadarray_obj_mem_per_year(Input_data_set_number, configuration, user_selections);
                        heapneeded := Tgeneric_dyadarray_obj_mem_overhead +
                                      (last_partition_year - first_partition_year + 1) *
                                         one_year_generic_memory_needed;
                        if debug[4] then
                           begin
                              generic_trace.message ('Generic array size calculation');
                              generic_trace.message ('Calc is that '+inttostr(one_year_generic_memory_needed)+' needed per year.');
                              generic_trace.message ('Calc is that '+inttostr(heapneeded)+' needed for all years.');
                              generic_trace.message ('Max avail mem block is '+inttostr(MaxAvail));
                           end;
                        if MaxAvail <= (heapneeded) then
                           begin
                              EUGeneError ('Not enough memory for generic structure. ',
                                              5, stop, error_log);
                           end;
                        for ayear := min_year to max_year do
                           if ( (ayear >= self.first_partition_year) and (ayear <= self.last_partition_year) ) then
                              begin
                                 new (generic_data_year);
                                 year_array[ayear] := generic_data_year;
                                 for ccode_index_loop := min_ccode_index to max_ccode_index do
                                    year_array[ayear]^[ccode_index_loop] := nil;
                                {Depending on whether it should be directed or non-directed dyadic
                                 data, set up blank range for processing.}
                                 case configuration.user_data_set_info.get_data_set_unit(Input_data_set_number) of
                                    nondirected_dyad_year : begin
                                       for ccode1 := min_ccode to max_ccode-1 do
                                          if nation_list.is_a_state (ccode1, ayear) then
                                             begin
                                                generic_trace.tick ('Executing Procedure: Initializing generic Structure',0);
                                                new (generic_data_year2);
                                                year_array[ayear]^[ccode_index.index(ccode1)] := generic_data_year2;
                                                for ccode2 := ccode1 + 1 to max_ccode do
                                                   if nation_list.is_a_state (ccode2, ayear) then
                                                      begin
                                                         {with each record to assign, set up the dynamic array
                                                          Length is # of vars user is reading.}
                                                         SetLength (year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].values,
                                                                    length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables));
                                                         {assign each blank a missing value record}
                                                         for x := 0 to length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables)-1 do
                                                            year_array[ayear]^[ccode_index.index(ccode1)]^
                                                               [ccode_index.index(ccode2)].values[x] :=
                                                                   configuration.user_data_set_info.get_data_set_var_missing_value(Input_data_set_number,user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables[x]);
                                                      end;
                                             end;
                                       end;  {nondirected}
                                    directed_dyad_year : begin
                                       for ccode1 := min_ccode to max_ccode do
                                          if nation_list.is_a_state (ccode1, ayear) then
                                             begin
                                                generic_trace.tick ('Executing Procedure: Initializing directed dyad generic data Structure',0);
                                                new (generic_data_year2);
                                                year_array[ayear]^[ccode_index.index(ccode1)] := generic_data_year2;
                                                for ccode2 := min_ccode to max_ccode do
                                                   if nation_list.is_a_state (ccode2, ayear) then
                                                      begin
                                                         SetLength (year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].values,
                                                                    length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables));
                                                         for x := 0 to length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables)-1 do
                                                            year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].values[x] :=
                                                                   configuration.user_data_set_info.get_data_set_var_missing_value(Input_data_set_number,user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables[x]);
                                                      end;
                                             end;
                                       end;         {directed dyad year}
                                    else
                                       begin
                                          EUGeneError ('Error - data set being processed in read generic dyad data not type directed_dyad or nondirected_dyad.',2,stop,error_log);
                                       end;
                                 end;   {case}
                              end
                           else year_array[ayear] := nil;
                        generic_trace.tickdone;

                        {Now open and read input file}
                        generic_trace.message ('Reading data from external file '+configuration.user_data_set_info.get_data_set_file_name(Input_data_set_number)+
                           ' from ' +inttostr(first_partition_year)+ ' to '+inttostr(last_partition_year));
                        assign (generic_file, configuration.user_data_set_info.get_data_set_file_name(Input_data_set_number));
                        reset(generic_file);

                        {if there is a set of var names / labels, skip it for now.}
                        if configuration.user_data_set_info.get_data_set_label_line(Input_data_set_number) then
                           readln (generic_file);

                        {Need a generic input data rec}
                        SetLength (generic_data_rec.values, length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables));


                        {can't assume user data sets are sorted, so must go through whole thing!}
                        cases_read := 0;
                        while not eof (generic_file) do
                           begin
                              read_one_generic_data_line (generic_file, generic_data_rec, inccode1, inccode2, inyear, configuration, Input_data_set_number, badline);
                              if not (badline) then
                              begin
                                 inc (cases_read);
                                 generic_trace.tick('reading user data file '+configuration.User_data_set_info.get_data_set_full_name(Input_data_set_number),configuration.User_data_set_info.get_data_set_num_cases(Input_data_set_number) );

                                 {Now, if this is for a non-directed input data set, set ccodes so cc1 < cc2}
                                 if configuration.user_data_set_info.get_data_set_unit(Input_data_set_number) = nondirected_dyad_year then
                                    if not (inccode1 <= inccode2) then
                                       begin
                                          {Right now, report an error.}
                                          EUGeneError ('Non-directed data set '+ configuration.user_data_set_info.get_data_set_file_name(Input_data_set_number) + ' had records with ccode1 > ccode2 for codes '+inttostr(inccode1)+' and '+inttostr(inccode2)+'.  This is not allowed.  This record has been skipped.',1,continue, error_log);
                                         {Eventually, would like to switch the dyad to be cc1<cc2.
                                          This involves switching the identity of monadic and directed variables.
                                          Program must check to see that the reverse direction exists [always will now, forced in config file checking],
                                          Reverse var was selected by user [forced this in the user variable processing]
                                          and switch the 2 variables. ]}
                                          {Must switch any variables that pertain to one ccode or the other,
                                           or that are directed in nature.}
                                          {For any directed or monadic variable, switch the values
                                           between it and the reverse.}
                                           for x := 0 to length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables)-1 do
                                              {check this variable, if a monadic or directed-dyadic, switch it with the reverse}
                                              if ((configuration.User_data_set_info.get_data_set_var_unit(Input_data_set_number,user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables[x]) = monadic) or
                                                  (configuration.User_data_set_info.get_data_set_var_unit(Input_data_set_number,user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables[x]) = dyadic_ordered)) then
                                              begin
                                           {Tricky thing will be not switching a second time if I already switched the variable.  }
                                           {Switching not complete!!}
                                              end;
                                           {Also switch ccode identifiers}
                                          {switch_ccode (inccode1, inccode2);}
                                       end;

                                 {Now, if the record is in range, put it in the right place on the stored data array.}
                                 if ( ((inyear >= self.first_partition_year) and (inyear <= self.last_partition_year)) and
                                      (nation_list.is_a_state (inccode1, inyear)) and
                                      (nation_list.is_a_state (inccode2, inyear)) and
                                      (not(configuration.user_data_set_info.get_data_set_unit(Input_data_set_number) = nondirected_dyad_year) or (inccode1 <= inccode2))
                                    ) then
                                    begin
                                     {b/c data is stored as a dynamic array, must copy the
                                      data rather than just assign the record}
                                       for x := 0 to length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables)-1 do
                                          year_array[inyear]^[ccode_index.index(inccode1)]^
                                             [ccode_index.index(inccode2)].values[x] := generic_data_rec.values[x];
                                    end;
                              end;         {if not badline then...}
                           end;    {while not eof do...}

                        {Entire file now read!}
                        created := true;
                        {Check cases and report error if file appears truncated}
                        if cases_read <> configuration.User_data_set_info.get_data_set_num_cases(Input_data_set_number) then
                           EUGeneError ('Number of cases in data file does not match specification in user configuration file for data set '+configuration.User_data_set_info.get_data_set_file_name(Input_data_set_number)+'.  Check data file for possible truncation.',3, continue, error_log);
                     finally    {trying to read data from outside file}
                        CloseFile (generic_file);
                        generic_data_rec.values := nil;
                        generic_trace.tickdone;
                        trace.message (' required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
                        trace.message('Finished initializing generic data to data from user file');
                     end;    {finally}
                  end   {start and end year were such that this data set is in range}

               else   {dates not OK, this should be a bare bones structure only}
                  begin
                        self.first_partition_year := min_year;
                        self.last_partition_year := min_year;
                        for ayear := min_year to max_year do year_array[ayear] := nil;
                        created := true;
                        trace.message('Finished initializing generic data to nil structure');
                  end;
            finally
               generic_trace.free;
               trace.exit ('Finished initialization');
            end;   {outer try finally}

         except
            on EUserInterrupt do raise;
            on EInOutError do
              begin
                 FileErrorBox.maindo ('Error opening or processing file "'+configuration.user_data_set_info.get_data_set_file_name(Input_data_set_number)+ '"',
                                      'File could not be opened for input, or other I/O error.',
                                      'File may be in use by another program, may be missing, or may be incomplete.');
                 FileErrorBox.showmodal;
                 raise;
              end;
            else
              EUGeneError ('Other error in generic data processing - pausing - notify programmer', 5, continue, error_log);
         end;             {try-except}
      end;       {proc init}

                   { -------------------------------- }

   destructor TgenericDyad_array_obj.destroy;
      var ayear : year_range;
          ccode1, ccode2 : ccode_range;
      begin
         try
            if self <> nil then
            begin
               for ayear := min_year to max_year do
                  if year_array[ayear] <> nil then
                     begin
                       for ccode1 := min_ccode to max_ccode do
                         if year_array[ayear]^[ccode_index.index(ccode1)] <> nil then
                            begin
                               for ccode2 := min_ccode to max_ccode do
                                  finalize(year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].values);
                               dispose (year_array[ayear]^[ccode_index.index(ccode1)]);
                               year_array[ayear]^[ccode_index.index(ccode1)] := nil;
                            end;
                       dispose (year_array[ayear]);
                       year_array[ayear] := nil;
                     end;
               created := false;
               inherited destroy;
            end;  {if self <> nil...}
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;

                   { -------------------------------- }

   function TgenericDyad_array_obj.all_in_range (const indata_set_num, config_var_num : integer; ccode1, ccode2 : ccode_range; ayear : year_range; error_check : boolean): boolean;
      begin
         all_in_range := true;
         if not(initialized) then
            begin
               all_in_range := false;
               if error_check then
                  EUGeneError ('Get '+configuration.User_data_set_info.get_data_set_var_name(indata_set_num,config_var_num)+
                         ' from generic data set '+configuration.User_data_set_info.get_data_set_short_name(indata_set_num)+
                         ' called before initialization', 5, stop, error_log);
            end
         else
            {Check to make sure unit is OK.  here, this is dyadic data structure,
             so it will be checking that user requested dyadic output.}
            if not (can_get_var_value_given_output_unit_and_input_data (indata_set_num, config_var_num, user_selections)) then
               begin
                  all_in_range := false;
                  {This will be a common occurence with multiple data sets, so
                   this check is first and does not generate an error message.}
               end
            else
         if not ((ayear >= get_first_partition_year) and (ayear <= get_last_partition_year)) then
            begin
               all_in_range := false;
               if error_check then
                  begin
                  EUGeneError ('Get '+configuration.User_data_set_info.get_data_set_var_name(indata_set_num,config_var_num)+
                        ' from generic data set '+configuration.User_data_set_info.get_data_set_short_name(indata_set_num)+
                        ' called for value outside data range.', 5, stop, error_log);
                  end;
            end
         else
            if not ( (nation_list.is_a_state(ccode1, ayear)) and (nation_list.is_a_state(ccode2, ayear)) ) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        trace.message ('Error in ccode-years: Get '+configuration.User_data_set_info.get_data_set_var_name(indata_set_num,config_var_num)+
                            ' from generic data set '+configuration.User_data_set_info.get_data_set_short_name(indata_set_num)+
                            ' for invalid ccodes given year of '+ inttostr(ccode1)+' in '+inttostr(ayear));
                        trace.message (configuration.User_data_set_info.get_data_set_var_name(indata_set_num,config_var_num) + ' set to missing_value');
                     end;
               end
         else
            if not user_wanted(indata_set_num,configuration.User_data_set_info.get_data_set_var_unit(indata_set_num, config_var_num), config_var_num) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        trace.message ('Error: called Get '+configuration.User_data_set_info.get_data_set_var_name(indata_set_num,config_var_num)+
                            ' from generic data set '+configuration.User_data_set_info.get_data_set_short_name(indata_set_num)+
                            ' for variable user did not select.  ');
                        trace.message (configuration.User_data_set_info.get_data_set_var_name(indata_set_num,config_var_num) + ' set to missing_value');
                     end;
               end

      end;   {all in range}

                   { -------------------------------- }

   function TgenericDyad_array_obj.sub_get (const data_set_num, user_var_num : integer; ccode1, ccode2 : ccode_range;
            ayear : year_range; const user_selections: user_selection_type) : variant;
      var
          reverse_var_config_index, reverse_var_user_index : integer;
      begin
           {Come into this sub function knowing that we can get this data.
            But where to get it depends on type of data set, and requested output unit...}

         sub_get := missing_value;

         case configuration.User_data_set_info.get_data_set_unit(data_set_num) of
            nondirected_dyad_year : begin
                  case user_selections.output_this of
                     output_nondirected_dyads : begin
                        if not (ccode1 <= ccode2) then
                           EUGeneError ('proc to output generic dyadic data called for non-directed dyads, but ccodes are not in sorted order.  Programming error',2,continue,error_log)
                        else
                           sub_get := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].values[user_var_num];
                        end;   {output non_directed}
                     output_directed_dyads, output_directed_dispute_initiation_dyads, output_nondirected_dispute_dyads : begin
                          {here, data set is nondirected, but want directed output.}
                          {what this means is that for some vars, will need to output
                           a version that is changed/flipped with a parallel var.}
                          {This would output data for dependa in place of dependb,
                           dema in place of demb, b/c the direction is reversed,
                           do this if ccode1 > ccode2.}
                          case configuration.User_data_set_info.get_data_set_var_unit(data_set_num, user_selections.user_data_sets_selections[data_set_num].data_set_variables[user_var_num]) of
                             identifierccode1, identifierccode2, identifieryear, identifierversion, dyadic_unordered, annual :
                                {these can just get output normally.}
                                if ccode1 <= ccode2 then   {just output as in data}
                                   sub_get := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].values[user_var_num]
                                   else sub_get := year_array[ayear]^[ccode_index.index(ccode2)]^[ccode_index.index(ccode1)].values[user_var_num];
                             dyadic_ordered, monadic : begin
                                   {if outputting dyadic ordered, need to check order of cc1, cc2, and maybe switch vars for output.}
                                   if ccode1 <= ccode2 then   {just output as in data}
                                      sub_get := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].values[user_var_num]
                                   else
                                      begin     {This is a directed var but nondir input.}
                                         {output the other paired variable, pseudo-ordered.}
                                         {find index of reverse var in the other
                                          ccode2, ccode1 location!}
                                         reverse_var_config_index := configuration.User_data_set_info.get_data_set_var_number(data_set_num,configuration.User_data_set_info.get_data_set_var_reversed_var(data_set_num,user_selections.user_data_sets_selections[data_set_num].data_set_variables[user_var_num]));
                                         reverse_var_user_index := user_var_num_from_config_var_num[reverse_var_config_index];
                                         {output reverse var}
                                         if reverse_var_user_index = -1 then
                                            EUGeneError ('Error in sub_get dyadic function for a request for dyadic-ordered or monadic variable, from a nondirected data set, for directed-dyad output;  reverse variable needed but not found on user list.',3,continue,error_log)
                                            else sub_get := year_array[ayear]^[ccode_index.index(ccode2)]^[ccode_index.index(ccode1)].values[reverse_var_user_index];
                                      end;
                                end;      {directed ordered, monadic}
                             end;   {case var_unit of...}
                       end  {output directed}
                     else
                        begin
                           {This should not happen.  Calls to dyad year when data requested
                            is country-year should be intercepted by outer get function.}
                           EUGeneError ('Get data for user input dyadic data set called when user selected data output procedure not dyadic.  Error - notify programmer.',2, stop, error_log);
                        end;
                  end;        {case output_this of...}
               end;  {nondirected data}

            {Now, data set is directed dyad-year.}
            directed_dyad_year : begin
                  case user_selections.output_this of
                     output_nondirected_dyads : begin
                          {here, data set is directed, but want nondirected output.}
                          {Return the value looked up, the outer procedure call
                           should call this in both directions to get both versions
                           of the variable.}
                          sub_get := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].values[user_var_num];
                        end;   {output non_directed}
                     output_directed_dyads, output_directed_dispute_initiation_dyads, output_nondirected_dispute_dyads : begin
                            {regular directed output}
                          sub_get := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].values[user_var_num];
                       end  {output directed}
                     else
                        begin
                           {This should not happen.  Calls to dyad year when data requested
                            is country-year should be intercepted by outer get function.}
                           EUGeneError ('Get data for user input dyadic data set called when user selected data output procedure not dyadic.  Error - notify programmer.',2, stop, error_log);
                        end;
                  end;        {case output_this of...}
               end;         {directed dyad year}
            else      {Can't ever get here - In dyadic data object, but unit of data set is neither
                       nondirected nor directed.}
               begin
                  EUGeneError ('Get data for user input dyadic data set called when user selected data output procedure not dyadic.  Error - notify programmer.',2, stop, error_log);
               end;
         end;   {case data_set_unit of ...}
      end;    {function sub_get}

   function TgenericDyad_array_obj.get_value (const data_set_num, config_var_num : integer; ccode1, ccode2 : ccode_range;
                                              ayear : year_range; const user_selections: user_selection_type) : variant;
      {get_value takes a configuration data set variable number as input.
       It is converted to the user number before the call to sub_get}
		 begin
         get_value := missing_value;
         if all_in_range (data_set_num, config_var_num, ccode1, ccode2, ayear, true) then
            get_value := sub_get (data_set_num, user_var_num_from_config_var_num[config_var_num], ccode1, ccode2, ayear, user_selections);
      end;

   function TgenericDyad_array_obj.get_value (const data_set_num, config_var_num : integer; ccode1, ccode2 : ccode_range;
                                              ayear : year_range; const user_selections: user_selection_type; error_check : boolean) : variant;
      begin
         get_value := missing_value;
         if all_in_range (data_set_num, config_var_num, ccode1, ccode2, ayear, error_check) then
            get_value := sub_get (data_set_num, user_var_num_from_config_var_num[config_var_num], ccode1, ccode2, ayear, user_selections);
      end;


    { ------------------------------------------------------------ }

{Now procedures for monadic data object}

   constructor TgenericMonadic_array_obj.init (Input_data_set_number : integer;
            start_year, end_year : year_range);
      var x : integer;
          inccode1, inccode2 : ccode_range;
          inyear, ayear : year_range;
          ccode1, ccode2 : ccode_range;
          generic_file : text;
          generic_trace : Ttrace_obj;
          start_mem, one_year_generic_memory_needed, heapneeded : longint;
          generic_data_year : ccode_year_ptr;
          ccode_index_loop : ccode_index_range;
          generic_data_rec : generic_values_record_type;
          First_year_possible, last_year_possible, First_year_to_read, last_year_to_read : year_range;
          badline : boolean;
          cases_read : longint;

      begin
         generic_trace := nil;
         try
            try
               start_mem := memavail;
               trace.enter('Initializing generic monadic data, '+inttostr(First_year_to_read)+' to '+inttostr(Last_year_to_read)+
                 ' from generic data set ' + configuration.user_data_set_info.get_data_set_short_name(Input_data_set_number));
               generic_trace := TTrace_obj.init(trace.get_trace_level);


               {Set up index to the array where data will be, that will be accessed with
                the number in the original configuration var list.
                user_var_num_from_config_var_num[3] will give the storage location of configuration variable 3.}
               setlength(user_var_num_from_config_var_num, configuration.user_data_set_info.get_data_set_num_vars(Input_data_set_number));
               {initialize all to - 1}
               for x := 0 to configuration.user_data_set_info.get_data_set_num_vars(Input_data_set_number)-1 do
                  user_var_num_from_config_var_num[x] := -1;   {initially, invalid location.}
               for x := 0 to length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables)-1 do
                  begin
                     user_var_num_from_config_var_num[user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables[x]] := x;
                  end;


               {Check time period.}
               First_year_possible := configuration.user_data_set_info.get_data_set_first_year_possible(Input_data_set_number);
               Last_year_possible := configuration.user_data_set_info.get_data_set_last_year_possible(Input_data_set_number);

               if ((start_year >= First_year_possible) and
                   (start_year <= Last_year_possible )) or
                  ((end_year >= First_year_possible) and
                   (end_year <= Last_year_possible )) or
                  ((start_year <= First_year_possible) and
                   (end_year >= Last_year_possible)) then
                  begin     {can read data}

                     {set years to be read}
                     First_year_to_read := max (start_year, First_year_possible);
                     Last_year_to_read := min(end_year,Last_year_possible);
                     {Now set the internal partition years.  }
                     self.first_partition_year := First_year_to_read;
                     self.last_partition_year := Last_year_to_read;

                     try
                        {Now initialize the data structure.  Give every legitimate state pair
                         the initialized value.}
                        generic_trace.message ('Initializing generic monadic structure for '+ configuration.user_data_set_info.get_data_set_short_name(Input_data_set_number));
                        one_year_generic_memory_needed := Tgeneric_monadarray_obj_mem_per_year(Input_data_set_number, configuration, user_selections);

                        heapneeded :=  Tgeneric_monadarray_obj_mem_overhead +
                                      (last_partition_year - first_partition_year + 1) *
                                         one_year_generic_memory_needed;
                        if debug[4] then
                           begin
                              generic_trace.message ('Generic array size calculation');
                              generic_trace.message ('Calc is that '+inttostr(one_year_generic_memory_needed)+' needed per year.');
                              generic_trace.message ('Calc is that '+inttostr(heapneeded)+' needed for all years.');
                              generic_trace.message ('Max avail mem block is '+inttostr(MaxAvail));
                           end;
                        if MaxAvail <= (heapneeded) then
                           begin
                              EUGeneError ('Not enough memory for generic monadic structure. ',
                                              5, stop, error_log);
                           end;
                        {initialize blank structure}
                        for ayear := min_year to max_year do
                           if ( (ayear >= self.first_partition_year) and (ayear <= self.last_partition_year) ) then
                              begin
                                 new (generic_data_year);
                                 year_array[ayear] := generic_data_year;
                                 for ccode_index_loop := min_ccode_index to max_ccode_index do
                                    setlength(year_array[ayear]^[ccode_index_loop].values,0);

                                 for ccode1 := min_ccode to max_ccode do
                                    if nation_list.is_a_state (ccode1, ayear) then
                                       begin
                                          generic_trace.tick ('Executing Procedure: Initializing generic Structure',0);
                                          {with each record to assign, set up the dynamic array
                                           Length is # of vars user is reading.}
                                          SetLength (year_array[ayear]^[ccode_index.index(ccode1)].values,
                                                     length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables));
                                          {Now assign each blank a missing value record}
                                          for x := 0 to length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables)-1 do
                                             year_array[ayear]^[ccode_index.index(ccode1)].values[x] :=
                                                    configuration.user_data_set_info.get_data_set_var_missing_value(Input_data_set_number,user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables[x]);
                                       end;
                              end    {ayear in range}
                           else year_array[ayear] := nil;
                        generic_trace.tickdone;

                        {Now open and read input file}
                        generic_trace.message ('Reading data from external file '+configuration.user_data_set_info.get_data_set_file_name(Input_data_set_number)+
                           ' from ' +inttostr(first_partition_year)+ ' to '+inttostr(last_partition_year));
                        assign (generic_file, configuration.user_data_set_info.get_data_set_file_name(Input_data_set_number));
                        reset(generic_file);

                        {if there is a set of var names / labels, skip it for now.}
                        if configuration.user_data_set_info.get_data_set_label_line(Input_data_set_number) then
                           readln (generic_file);

                        {Need a generic input data rec}
                        SetLength (generic_data_rec.values, length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables));


                        {can't assume user data sets are sorted, so must go through whole thing!}
                        cases_read := 0;
                        while not eof (generic_file) do
                           begin
                              read_one_generic_data_line (generic_file, generic_data_rec, inccode1, inccode2, inyear, configuration, Input_data_set_number, badline);
                              if not (badline) then
                              begin
                                 inc (cases_read);
                                 generic_trace.tick('reading user data file '+configuration.User_data_set_info.get_data_set_full_name(Input_data_set_number),configuration.User_data_set_info.get_data_set_num_cases(Input_data_set_number) );

                                 {Now, if the record is in range, put it in the right place on the stored data array.}
                                 if ( ((inyear >= self.first_partition_year) and (inyear <= self.last_partition_year)) and
                                      (nation_list.is_a_state (inccode1, inyear))
                                    ) then
                                    begin
                                     {b/c data is stored as a dynamic array, must copy the
                                      data rather than just assign the record}
                                       for x := 0 to length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables)-1 do
                                          year_array[inyear]^[ccode_index.index(inccode1)].values[x] := generic_data_rec.values[x];
                                    end;
                              end;
                           end;    {while not eof do...}

                        {Entire file now read!}
                        created := true;
                        {Check cases and report error if file appears truncated}
                        if cases_read <> configuration.User_data_set_info.get_data_set_num_cases(Input_data_set_number) then
                           EUGeneError ('Number of cases in data file does not match specification in user configuration file for data set '+configuration.User_data_set_info.get_data_set_file_name(Input_data_set_number)+'.  Check data file for possible truncation.',3, continue, error_log);
                     finally    {trying to read data from outside file}
                        CloseFile (generic_file);
                        generic_data_rec.values := nil;
                        generic_trace.tickdone;
                        trace.message (' required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
                        trace.message('Finished initializing generic data to data from user file');
                     end;    {finally}
                  end   {start and end year were such that this data set is in range}

               else   {dates not OK, this should be a bare bones structure only}
                  begin
                        self.first_partition_year := min_year;
                        self.last_partition_year := min_year;
                        for ayear := min_year to max_year do year_array[ayear] := nil;
                        created := true;
                        trace.message('Finished initializing generic monadic data to nil structure');
                  end;
            finally
               generic_trace.free;
               trace.exit ('Finished initialization');
            end;   {outer try finally}

         except
            on EUserInterrupt do raise;
            on EInOutError do
              begin
                 FileErrorBox.maindo ('Error opening or processing file "'+configuration.user_data_set_info.get_data_set_file_name(Input_data_set_number)+ '"',
                                      'File could not be opened for input, or other I/O error.',
                                      'File may be in use by another program, may be missing, or may be incomplete.');
                 FileErrorBox.showmodal;
                 raise;
              end;
            else
              EUGeneError ('Other error in generic data processing - pausing - notify programmer', 5, continue, error_log);
         end;             {try-except}
      end;       {proc init}

                   { -------------------------------- }

   destructor TgenericMonadic_array_obj.destroy;
      var ayear : year_range;
          ccode1 : ccode_range;
      begin
         try
            if self <> nil then
            begin
               for ayear := min_year to max_year do
                  if year_array[ayear] <> nil then
                     begin
                       for ccode1 := min_ccode_index to max_ccode_index do
                          finalize(year_array[ayear]^[ccode1].values);
                       dispose (year_array[ayear]);
                       year_array[ayear] := nil;
                     end;
               created := false;
               inherited destroy;
            end;  {if self <> nil...}
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;

                   { -------------------------------- }

   function TgenericMonadic_array_obj.sub_get (user_var_num : integer; ccode1 : ccode_range;
            ayear : year_range; const user_selections: user_selection_type) : variant;
      {For getting data from a monadic data set, what is relevant is just the varnum,
      ccode1, and year.  ccode2 is irrelevant.}
      begin
         sub_get := year_array[ayear]^[ccode_index.index(ccode1)].values[user_var_num];
      end;    {function sub_get}

   function TgenericMonadic_array_obj.get_value (const data_set_num, config_var_num : integer; ccode1, ccode2 : ccode_range;
                       ayear : year_range; const user_selections: user_selection_type) : variant;
      {get_value takes a configuration data set variable number as input.
       It is converted to the user number before the call to sub_get}
		 begin
         get_value := missing_value;
         if all_in_range (data_set_num, config_var_num, ccode1, ayear, true) then
            get_value := sub_get (user_var_num_from_config_var_num[config_var_num], ccode1, ayear, user_selections);
      end;

   function TgenericMonadic_array_obj.get_value (const data_set_num, config_var_num : integer; ccode1, ccode2 : ccode_range;
                       ayear : year_range; const user_selections: user_selection_type; error_check : boolean) : variant;
      {get_value takes a configuration data set variable number as input.
       It is converted to the user number before the call to sub_get}
		 begin
         get_value := missing_value;
         if all_in_range (data_set_num, config_var_num, ccode1, ayear, error_check) then
            get_value := sub_get (user_var_num_from_config_var_num[config_var_num], ccode1, ayear, user_selections);
      end;

                   { -------------------------------- }

   function TgenericMonadic_array_obj.all_in_range (const data_set_num, config_var_num : integer; ccode1 : ccode_range; ayear : year_range; error_check : boolean): boolean;
      {only care about ccode1 and year here}
      begin
         all_in_range := true;
         if not(initialized) then
            begin
               all_in_range := false;
               if error_check then
                  EUGeneError ('Get '+configuration.User_data_set_info.get_data_set_var_name(data_set_num,config_var_num)+
                         ' from generic data set '+configuration.User_data_set_info.get_data_set_short_name(data_set_num)+
                         ' called before initialization', 5, stop, error_log);
            end
         else
            if not (can_get_var_value_given_output_unit_and_input_data (data_set_num, config_var_num, user_selections)) then
               begin
                  all_in_range := false;
                  {This will be a common occurence with multiple data sets, so
                   this check is first and does not generate an error message.}
               end
            else
         if not ((ayear >= get_first_partition_year) and (ayear <= get_last_partition_year)) then
            begin
               all_in_range := false;
               if error_check then
                  begin
                  EUGeneError ('Get '+configuration.User_data_set_info.get_data_set_var_name(data_set_num,config_var_num)+
                        ' from generic data set '+configuration.User_data_set_info.get_data_set_short_name(data_set_num)+
                        ' called for value outside data range.', 5, stop, error_log);
                  end;
            end
         else
            if not (nation_list.is_a_state(ccode1, ayear)) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        trace.message ('Error in ccode-years: Get '+configuration.User_data_set_info.get_data_set_var_name(data_set_num,config_var_num)+
                            ' from generic data set '+configuration.User_data_set_info.get_data_set_short_name(data_set_num)+
                            ' for invalid ccodes given year of '+ inttostr(ccode1)+' in '+inttostr(ayear));
                        trace.message (configuration.User_data_set_info.get_data_set_var_name(data_set_num,config_var_num) + ' set to missing_value');
                     end;
               end
         else
            if not user_wanted(data_set_num,configuration.User_data_set_info.get_data_set_var_unit(data_set_num, config_var_num), config_var_num) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        trace.message ('Error: called Get '+configuration.User_data_set_info.get_data_set_var_name(data_set_num,config_var_num)+
                            ' from generic data set '+configuration.User_data_set_info.get_data_set_short_name(data_set_num)+
                            ' for variable user did not select.  ');
                        trace.message (configuration.User_data_set_info.get_data_set_var_name(data_set_num,config_var_num) + ' set to missing_value');
                     end;
               end

      end;   {all in range}


    { ------------------------------------------------------------ }

{Now procedures for annual data object}

   constructor TgenericAnnual_array_obj.init (Input_data_set_number : integer;
            start_year, end_year : year_range);
      var x : integer;
          inccode1, inccode2 : ccode_range;
          inyear, ayear : year_range;
          ccode1, ccode2 : ccode_range;
          generic_file : text;
          generic_trace : Ttrace_obj;
          start_mem, one_year_generic_memory_needed, heapneeded : longint;
          generic_data_year : ccode_year_ptr;
          ccode_index_loop : ccode_index_range;
          generic_data_rec : generic_values_record_type;
          First_year_possible, last_year_possible, First_year_to_read, last_year_to_read : year_range;
          badline : boolean;
          cases_read : longint;

      begin
         generic_trace := nil;
         try
            try
               start_mem := memavail;
               trace.enter('Initializing generic annual data, '+inttostr(First_year_to_read)+' to '+inttostr(Last_year_to_read)+
                 ' from generic data set ' + configuration.user_data_set_info.get_data_set_short_name(Input_data_set_number));
               generic_trace := TTrace_obj.init(trace.get_trace_level);

               {Set up index to the array where data will be, that will be accessed with
                the number in the original configuration var list.
                user_var_num_from_config_var_num[3] will give the storage location of configuration variable 3.}
               setlength(user_var_num_from_config_var_num, configuration.user_data_set_info.get_data_set_num_vars(Input_data_set_number));
               {initialize all to - 1}
               for x := 0 to configuration.user_data_set_info.get_data_set_num_vars(Input_data_set_number)-1 do
                  user_var_num_from_config_var_num[x] := -1;   {initially, invalid location.}
               for x := 0 to length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables)-1 do
                  begin
                     user_var_num_from_config_var_num[user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables[x]] := x;
                  end;

               {Check time period.}
               First_year_possible := configuration.user_data_set_info.get_data_set_first_year_possible(Input_data_set_number);
               Last_year_possible := configuration.user_data_set_info.get_data_set_last_year_possible(Input_data_set_number);

               if ((start_year >= First_year_possible) and
                   (start_year <= Last_year_possible )) or
                  ((end_year >= First_year_possible) and
                   (end_year <= Last_year_possible )) or
                  ((start_year <= First_year_possible) and
                   (end_year >= Last_year_possible)) then
                  begin     {can read data}

                     {set years to be read}
                     First_year_to_read := max (start_year, First_year_possible);
                     Last_year_to_read := min(end_year,Last_year_possible);
                     {Now set the internal partition years.  }
                     self.first_partition_year := First_year_to_read;
                     self.last_partition_year := Last_year_to_read;

                     try
                        {Now initialize the data structure.  Give every legitimate state pair
                         the initialized value.}
                        generic_trace.message ('Initializing generic monadic structure for '+ configuration.user_data_set_info.get_data_set_short_name(Input_data_set_number));
                        one_year_generic_memory_needed := Tgeneric_annualarray_obj_mem_per_year(Input_data_set_number, configuration, user_selections);

                        heapneeded :=  Tgeneric_annualarray_obj_mem_overhead +
                                      (last_partition_year - first_partition_year + 1) *
                                         one_year_generic_memory_needed;
                        if debug[4] then
                           begin
                              generic_trace.message ('Generic array size calculation');
                              generic_trace.message ('Calc is that '+inttostr(one_year_generic_memory_needed)+' needed per year.');
                              generic_trace.message ('Calc is that '+inttostr(heapneeded)+' needed for all years.');
                              generic_trace.message ('Max avail mem block is '+inttostr(MaxAvail));
                           end;
                        if MaxAvail <= (heapneeded) then
                           begin
                              EUGeneError ('Not enough memory for generic monadic structure. ',
                                              5, stop, error_log);
                           end;
                        {initialize blank structure}
                        for ayear := min_year to max_year do
                           if ( (ayear >= self.first_partition_year) and (ayear <= self.last_partition_year) ) then
                              begin
                                 SetLength (year_array[ayear].values,
                                            length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables));
                                 {Now assign each blank a missing value record}
                                 for x := 0 to length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables)-1 do
                                             year_array[ayear].values[x] :=
                                             configuration.user_data_set_info.get_data_set_var_missing_value(Input_data_set_number,user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables[x]);
                              end    {ayear in range}
                           else SetLength (year_array[ayear].values,0);

                        {Now open and read input file}
                        generic_trace.message ('Reading data from external file '+configuration.user_data_set_info.get_data_set_file_name(Input_data_set_number)+
                           ' from ' +inttostr(first_partition_year)+ ' to '+inttostr(last_partition_year));
                        assign (generic_file, configuration.user_data_set_info.get_data_set_file_name(Input_data_set_number));
                        reset(generic_file);

                        {if there is a set of var names / labels, skip it for now.}
                        if configuration.user_data_set_info.get_data_set_label_line(Input_data_set_number) then
                           readln (generic_file);

                        {Need a generic input data rec}
                        SetLength (generic_data_rec.values, length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables));

                        {can't assume user data sets are sorted, so must go through whole thing!}
                        cases_read := 0;
                        while not eof (generic_file) do
                           begin
                              read_one_generic_data_line (generic_file, generic_data_rec, inccode1, inccode2, inyear, configuration, Input_data_set_number, badline);
                              if not (badline) then
                              begin
                                 inc (cases_read);
                                 generic_trace.tick('reading user data file '+configuration.User_data_set_info.get_data_set_full_name(Input_data_set_number),configuration.User_data_set_info.get_data_set_num_cases(Input_data_set_number) );

                                 {Now, if the record is in range, put it in the right place on the stored data array.}
                                 if ((inyear >= self.first_partition_year) and (inyear <= self.last_partition_year)) then
                                    begin
                                     {b/c data is stored as a dynamic array, must copy the
                                      data rather than just assign the record}
                                       for x := 0 to length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables)-1 do
                                          year_array[inyear].values[x] := generic_data_rec.values[x];
                                    end;
                              end;
                           end;    {while not eof do...}

                        {Entire file now read!}
                        created := true;
                        {Check cases and report error if file appears truncated}
                        if cases_read <> configuration.User_data_set_info.get_data_set_num_cases(Input_data_set_number) then
                           EUGeneError ('Number of cases in data file does not match specification in user configuration file for data set '+configuration.User_data_set_info.get_data_set_file_name(Input_data_set_number)+'.  Check data file for possible truncation.',3, continue, error_log);
                     finally    {trying to read data from outside file}
                        CloseFile (generic_file);
                        generic_data_rec.values := nil;
                        generic_trace.tickdone;
                        trace.message (' required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
                        trace.message('Finished initializing generic data to data from user file');
                     end;    {finally}
                  end   {start and end year were such that this data set is in range}

               else   {dates not OK, this should be a bare bones structure only}
                  begin
                        self.first_partition_year := min_year;
                        self.last_partition_year := min_year;
                        for ayear := min_year to max_year do setLength(year_array[ayear].values, 0);
                        created := true;
                        trace.message('Finished initializing generic monadic data to nil structure');
                  end;
            finally
               generic_trace.free;
               trace.exit ('Finished initialization');
            end;   {outer try finally}

         except
            on EUserInterrupt do raise;
            on EInOutError do
              begin
                 FileErrorBox.maindo ('Error opening or processing file "'+configuration.user_data_set_info.get_data_set_file_name(Input_data_set_number)+ '"',
                                      'File could not be opened for input, or other I/O error.',
                                      'File may be in use by another program, may be missing, or may be incomplete.');
                 FileErrorBox.showmodal;
                 raise;
              end;
            else
              EUGeneError ('Other error in generic data processing - pausing - notify programmer', 5, continue, error_log);
         end;             {try-except}
      end;       {proc init}

                   { -------------------------------- }

   destructor TgenericAnnual_array_obj.destroy;
      var ayear : year_range;
      begin
         try
            if self <> nil then
            begin
               for ayear := min_year to max_year do
                  finalize(year_array[ayear].values);
               created := false;
               inherited destroy;
            end;  {if self <> nil...}
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;

                   { -------------------------------- }

   function TgenericAnnual_array_obj.sub_get (user_var_num : integer;
            ayear : year_range; const user_selections: user_selection_type) : variant;
      begin
         sub_get := year_array[ayear].values[user_var_num];
      end;    {function sub_get}

   function TgenericAnnual_array_obj.get_value (const data_set_num, config_var_num : integer; ccode1, ccode2 : ccode_range;
                       ayear : year_range; const user_selections: user_selection_type) : variant;
      {get_value takes a configuration data set variable number as input.
       It is converted to the user number before the call to sub_get}
		 begin
         get_value := missing_value;
         if all_in_range (data_set_num, config_var_num, ayear, true) then
            get_value := sub_get (user_var_num_from_config_var_num[config_var_num], ayear, user_selections);
      end;

   function TgenericAnnual_array_obj.get_value (const data_set_num, config_var_num : integer; ccode1, ccode2 : ccode_range;
                       ayear : year_range; const user_selections: user_selection_type; error_check : boolean) : variant;
      {get_value takes a configuration data set variable number as input.
       It is converted to the user number before the call to sub_get}
		 begin
         get_value := missing_value;
         if all_in_range (data_set_num, config_var_num, ayear, error_check) then
            get_value := sub_get (user_var_num_from_config_var_num[config_var_num], ayear, user_selections);
      end;

                   { -------------------------------- }

   function TgenericAnnual_array_obj.all_in_range (const data_set_num, config_var_num : integer; ayear : year_range; error_check : boolean): boolean;
      {for annual data, don't care about ccodes, just care about year being in range}
      begin
         all_in_range := true;
         if not(initialized) then
            begin
               all_in_range := false;
               if error_check then
                  EUGeneError ('Get '+configuration.User_data_set_info.get_data_set_var_name(data_set_num,config_var_num)+
                         ' from generic data set '+configuration.User_data_set_info.get_data_set_short_name(data_set_num)+
                         ' called before initialization', 5, stop, error_log);
            end
         else
            if not (can_get_var_value_given_output_unit_and_input_data (data_set_num, config_var_num, user_selections)) then
               begin
                  all_in_range := false;
                  {This will be a common occurence with multiple data sets, so
                   this check is first and does not generate an error message.}
               end
            else
         if not ((ayear >= get_first_partition_year) and (ayear <= get_last_partition_year)) then
            begin
               all_in_range := false;
               if error_check then
                  begin
                  EUGeneError ('Get '+configuration.User_data_set_info.get_data_set_var_name(data_set_num,config_var_num)+
                        ' from generic data set '+configuration.User_data_set_info.get_data_set_short_name(data_set_num)+
                        ' called for value outside data range.', 5, stop, error_log);
                  end;
            end
         else
            if not user_wanted(data_set_num,configuration.User_data_set_info.get_data_set_var_unit(data_set_num, config_var_num), config_var_num) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        trace.message ('Error: called Get '+configuration.User_data_set_info.get_data_set_var_name(data_set_num,config_var_num)+
                            ' from generic data set '+configuration.User_data_set_info.get_data_set_short_name(data_set_num)+
                            ' for variable user did not select.  ');
                        trace.message (configuration.User_data_set_info.get_data_set_var_name(data_set_num,config_var_num) + ' set to missing_value');
                     end;
               end
      end;   {all in range}


    { ------------------------------------------------------------ }


   constructor TMultiple_user_data_set_Obj.init (const configuration : configuration_type;
               const start_year, end_year : year_range);
      var x : integer;
          first_year_available, last_year_available : year_range;
      begin
         if not(ccode_index.initialized) then
            begin
               EUGeneError ('Multiple generic user data array creation called before ccode_index initialized', 5, stop, error_log);
               halt;
            end;
         if not(nation_list.initialized) then
            begin
               EUGeneError ('Multiple generic user data array creation called before nation_list initialized', 5, stop, error_log);
               halt;
            end;
         setlength(data,configuration.User_data_set_info.get_num_data_sets);
         for x := 0 to configuration.User_data_set_info.get_num_data_sets-1 do data[x] := nil;
         for x := 0 to configuration.User_data_set_info.get_num_data_sets-1 do
            if length(user_selections.user_data_sets_selections[x].data_set_variables)>0 then
               {this is a data set that must be read.}
               begin
                  case configuration.User_data_set_info.get_data_set_unit(x) of
                  {Note - I must pass in just the user start, end year and the .init
                   procedure will check appropriate dates.}
                    directed_dyad_year : data[x] := TgenericDyad_array_obj.init (x, start_year, end_year);
                    nondirected_dyad_year : data[x] := TgenericDyad_array_obj.init (x, start_year, end_year);
                    country_year : data[x] := TgenericMonadic_array_obj.init (x, start_year, end_year);
                    annual_data : data[x] := TgenericAnnual_array_obj.init (x, start_year, end_year);
                    else EUGeneError ('Init procedure attempted on a data set with inappropriate unit of analysis.  Data not initialized.  Programming error;  program continues but further errors may result.',3,continue, error_log);
                  end;   {case}
               end;
      end;

    { ------------------------------------------------------------ }

   destructor TMultiple_user_data_set_Obj.destroy;
      var x : integer;
      begin
         for x := 0 to length(data)-1 do data[x].free;
         setlength (data,0);
      end;

    { ------------------------------------------------------------ }

   function TMultiple_user_data_set_Obj.get_value (const data_set_num, config_var_num : integer; const ccode1, ccode2 : ccode_range;
                             const year : year_range; const user_selections: user_selection_type) : variant;
      {This function merely passes the call to the appropriate individual data set.
       Those individual functions handle all error checks.}
      begin
         result := data[data_set_num].get_value(data_set_num, config_var_num, ccode1, ccode2, year, user_selections);
      end;

   function TMultiple_user_data_set_Obj.get_value (const data_set_num, config_var_num : integer; const ccode1, ccode2 : ccode_range;
                             const year : year_range; const user_selections: user_selection_type; error_check : boolean) : variant;
      begin
         result := data[data_set_num].get_value(data_set_num, config_var_num, ccode1, ccode2, year, user_selections, error_check);
      end;

        { ------------------------------------------------------------ }

   Function Tgeneric_dyadarray_obj_mem_overhead : longint;
      begin
         result := sizeof(TgenericDyad_array_obj) +
             (max_year-min_year+1)*sizeof(dyad_year_ptr);
     end;

   Function Tgeneric_dyadarray_obj_mem_per_year (var Input_data_set_number : integer;
            const configuration : configuration_type; const user_selections : user_selection_type) : longint;
      {this is the size for a complete record of all variables.  }
      var sizeperrec, sizeforvar, x : integer;
      begin
         sizeperrec := 0;
         for x := 0 to length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables)-1 do
            begin
               if configuration.User_data_set_info.get_data_set_var_type(Input_data_set_number, user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables[x]) = varinteger then sizeforvar := sizeof(integer)
               else if configuration.User_data_set_info.get_data_set_var_type(Input_data_set_number, user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables[x])  = varsingle then sizeforvar := sizeof(integer)
               else if configuration.User_data_set_info.get_data_set_var_type(Input_data_set_number, user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables[x])  = varolestr then sizeforvar := sizeof('8charvar')
               else sizeforvar := 1;
               sizeperrec := sizeperrec + sizeforvar;
            end;
         Tgeneric_dyadarray_obj_mem_per_year := sizeof(dyad_year_array_type) +
            (max_ccode_index-min_ccode_index+1) *
               (sizeof(ccode_year_ptr) +
                (max_ccode_index-min_ccode_index+1)*(sizeof(generic_values_record_type) + sizeperrec)
               );
      end;

         { ------------------------------------------------------------ }

   Function Tgeneric_monadarray_obj_mem_overhead : longint;
      begin
         result := sizeof(TgenericMonadic_array_obj) +
             (max_year-min_year+1)*sizeof(ccode_year_ptr);
     end;

   Function Tgeneric_monadarray_obj_mem_per_year (var Input_data_set_number : integer;
            const configuration : configuration_type; const user_selections : user_selection_type) : longint;
      var sizeperrec, sizeforvar, x : integer;
      begin
         sizeperrec := 0;
         for x := 0 to length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables)-1 do
            begin
               if configuration.User_data_set_info.get_data_set_var_type(Input_data_set_number, user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables[x]) = varinteger then sizeforvar := sizeof(integer)
               else if configuration.User_data_set_info.get_data_set_var_type(Input_data_set_number, user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables[x])  = varsingle then sizeforvar := sizeof(integer)
               else if configuration.User_data_set_info.get_data_set_var_type(Input_data_set_number, user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables[x])  = varolestr then sizeforvar := sizeof('8charvar')
               else sizeforvar := 1;
               sizeperrec := sizeperrec + sizeforvar;
            end;
         result := sizeof(ccode_year_array) +
            (max_ccode_index-min_ccode_index+1)*(sizeof(generic_values_record_type) + sizeperrec);
      end;

        { ------------------------------------------------------------ }

   Function Tgeneric_annualarray_obj_mem_overhead : longint;
      begin
         result := sizeof(TgenericAnnual_array_obj);
     end;

   Function Tgeneric_annualarray_obj_mem_per_year (var Input_data_set_number : integer;
            const configuration : configuration_type; const user_selections : user_selection_type) : longint;
      var sizeperrec, sizeforvar, x : integer;
      begin
         sizeperrec := 0;
         for x := 0 to length (user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables)-1 do
            begin
               if configuration.User_data_set_info.get_data_set_var_type(Input_data_set_number, user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables[x]) = varinteger then sizeforvar := sizeof(integer)
               else if configuration.User_data_set_info.get_data_set_var_type(Input_data_set_number, user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables[x])  = varsingle then sizeforvar := sizeof(integer)
               else if configuration.User_data_set_info.get_data_set_var_type(Input_data_set_number, user_selections.user_data_sets_selections[Input_data_set_number].data_set_variables[x])  = varolestr then sizeforvar := sizeof('8charvar')
               else sizeforvar := 1;
               sizeperrec := sizeperrec + sizeforvar;
            end;
         result := (sizeof(generic_values_record_type) + sizeperrec);
      end;

       { ------------------------------------------------------------ }


end.
