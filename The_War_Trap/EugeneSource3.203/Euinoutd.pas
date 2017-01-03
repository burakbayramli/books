unit euinoutD;

{EUGene  Copyright 1997-2007+  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

     {This unit has procedures for 1) creating appropriate windows, 2) taking user input,
      and 3) opening the appropriate output file.
      Various procedures here will  take as input the user specified options that will
      be passed to the main processing procedure to create actual eu data.}

{ ------------------------------------------------------------ }

interface

   uses eutypes1, eutypes2, eutypes3, EUtypesMID,
        classes, variants, dialogs, controls, sysutils, fileError, strutils;

   function check_file_Exists (a_file_name : TFileName; file_label : string): boolean;
   procedure output_data (var ccode_index : Tccode_index_obj; var nation_list : Tnation_array_obj;
                      var contiguity_data : Tcontiguity_array_obj; var configuration : configuration_type;
                      var user_selections : user_selection_type; selected_output_unit : output_type);

{ ------------------------------------------------------------ }

implementation

uses printers, math, dateutils, TraceUnit, OutWindow, cmnprocd, errbx;

type varnameAndMissingValueArrayType = record
        num_vars : 0..max_user_variables;
        Names : array[0..max_user_variables] of string;
        Missing_value : array[0..max_user_variables] of integer;
    end;   {record}

   polnonpollisttype = record
         polnames, nonpolnames, usernames : array[1..max_predefined_variables] of string;
         numpolnames, numnonpolnames, numusernames : 0..max_predefined_variables;
      end;
   pol_nonpol_user_var_type = (pol, nonpol, user);

   polnonpolvarlabeltype = record
        name : string;
        varlabel : string;
      end;


   function check_file_Exists (a_file_name : TFileName; file_label : string): boolean;
      {returns true if file exists, else prints a message to user that file must be created}
      begin
         if (FileExists (a_file_name)) then
            begin
               {do nothing, file is OK}
               check_file_Exists := true;
            end
         else   {file does not exist}
            begin
               trace.message('File ' + a_file_name + ': ' + file_label +
                             ' not found and must be created.');
               ShowMessage ('Data file not found.  File for ' + file_label +
                            ' must be created before executing your requested procedure.');
               check_file_Exists := false;
            end;
      end;   {func check file}


 { ------------------------------------------------------------ }

   procedure calculate_output_partitions (const user_selections : user_selection_type;
          var num_partitions : integer; var years_per_partition : integer);
			{Compute number of partitions / passes necessary based on free memory}
   var mem_per_year, mem_to_use : longint;
       years_possible, num_years : longint;
   begin
      try
          try
             trace.enter('Entered main output calculate_partitions procedure');
                 {Calculation will be, how many years of various data structures can fit into
                   appropriate area of memory, given user selections for what should be output.
                  {for mem to use, take available block, and subtract the overhead
                   for the data structures.  }
                  {mem_for_procedure is the available memory to use from proc in eutypes1.}
             num_years := (user_selections.last_year - user_selections.first_year + 1);
             mem_to_use := mem_for_procedure;
             mem_per_year := 1;

             {disputes and distance are already read and subtracted at this point}

             if (SystemCapabilities in user_selections.output_format.variables) or
                (AlliancePortfolioWeighted in user_selections.output_format.variables) then
                begin
                   mem_to_use := mem_to_use - TSys_capability_array_obj_mem_overhead;
                   mem_per_year := mem_per_year + TSys_Capability_Array_obj_mem_per_year;
                end;
             if (riskTau in user_selections.output_format.variables) or
                     (riskdetailsTau in user_selections.output_format.variables) or
                     (uncertaintyTau in user_selections.output_format.variables) then
                begin
                   mem_to_use := mem_to_use - TRisk_Attitude_array_obj_mem_overhead;
                   mem_per_year := mem_per_year + TRisk_Attitude_Array_obj_mem_per_year;
                end;
             if (riskS in user_selections.output_format.variables) or
                     (riskdetailsS in user_selections.output_format.variables) or
                     (uncertaintyS in user_selections.output_format.variables) then
                begin
                   mem_to_use := mem_to_use - TRisk_Attitude_array_obj_mem_overhead;
                   mem_per_year := mem_per_year + TRisk_Attitude_Array_obj_mem_per_year;
                end;
             if (tau in user_selections.output_format.variables) or
                (tauwithleader in user_selections.output_format.variables) then
                begin
                   mem_to_use := mem_to_use - TTau_array_obj_mem_overhead;
                   mem_per_year := mem_per_year + TTau_Array_obj_mem_per_year;
                end;
             if (sunweighted in user_selections.output_format.variables) or
                (sweighted in user_selections.output_format.variables) or
                (swithleader in user_selections.output_format.variables) then
                begin
                   mem_to_use := mem_to_use - TS_array_obj_mem_overhead;
                   mem_per_year := mem_per_year + TS_Array_obj_mem_per_year;
                end;
             if (alliance in user_selections.output_format.variables) or
                (AlliancePortfolioUnweighted in user_selections.output_format.variables) or
                (AlliancePortfolioWeighted in user_selections.output_format.variables) then
                begin
                   mem_to_use := mem_to_use - TAlliance_array_obj_mem_overhead;
                   mem_per_year := mem_per_year + TAlliance_Array_obj_mem_per_year;
                end;
             if (polity3 in user_selections.output_format.variables) then
                begin
                   mem_to_use := mem_to_use - Tpolity_array_obj_mem_overhead;
                   mem_per_year := mem_per_year + Tpolity_Array_obj_mem_per_year;
                end;
             if (user_selections.werner_peace_year_adjustment=true) then
                begin
                   mem_to_use := mem_to_use - Twerner_PeaceYears_obj_mem_overhead;
                end;
             if (user_selections.output_this = output_directed_dyads) or (user_selections.output_this = output_nondirected_dyads) or
                (user_selections.output_this = output_directed_dispute_initiation_dyads) or (user_selections.output_this = output_nondirected_dispute_dyads) then
                begin
                   if EUWarTrapTau in user_selections.output_format.variables then
                      begin
                         mem_to_use := mem_to_use - TEUWarTrap_array_obj_mem_overhead;
                         mem_per_year := mem_per_year + TEUWarTrap_Array_obj_mem_per_year;
                      end;
                   if EUWarTrapS in user_selections.output_format.variables then
                      begin
                         mem_to_use := mem_to_use - TEUWarTrap_array_obj_mem_overhead;
                         mem_per_year := mem_per_year + TEUWarTrap_Array_obj_mem_per_year;
                      end;
                   if (EUWarReasonTau in user_selections.output_format.variables) or
                      (EQWarReasonTau in user_selections.output_format.variables) or
                      (EUWarReasonSUnweighted in user_selections.output_format.variables) or
                      (EQWarReasonSUnweighted in user_selections.output_format.variables) or
                      (EUWarReasonSweighted in user_selections.output_format.variables) or
                      (EQWarReasonSweighted in user_selections.output_format.variables)
                      then
                      begin
                         mem_to_use := mem_to_use - TEUWarReason_array_obj_mem_overhead;
                         mem_per_year := mem_per_year + TEUWarReason_Array_obj_mem_per_year;
                      end;
                end;
             if length(user_selections.user_data_sets_selections) > 0 then
                begin
                   mem_to_use := mem_to_use - Tuser_data_mem_overhead;
                   mem_per_year := mem_per_year + Tuser_data_mem_per_year;
                end;

             years_possible := trunc (mem_to_use / mem_per_year);
             years_per_partition := min (years_possible, num_years);

             if years_per_partition < 1 then
                     begin
                        EUGeneError ('Error - not enough memory for output - less than one year per partition calculated in procedure!',
                              5, stop, error_log);
                     end;

                  {Now, given that many years and calced # of years per partition, can see
                   how many partitions are necessary. }
          finally
             if num_years <= years_per_partition then num_partitions := 1
             else num_partitions := (num_years div years_per_partition) + 1;
                     {div in above calc will round down, so add 1.  }
             trace.message ('Information calculated on output partition division:');
             trace.message ('Total years for processing: '+inttostr(num_years));
             trace.message ('Memory that can be used: '+inttostr(mem_to_use)+' bytes');
             trace.message ('Memory to be allocated per year: '+inttostr(mem_per_year)+' bytes');
             trace.message ('Years possible per partition: '+inttostr(years_possible));
             trace.message ('Number of partitions necessary: '+inttostr(num_partitions));
             trace.message ('Actual number of years per partition '+inttostr(years_per_partition));

             trace.exit('Exited output calculate_partitions procedure');
             {showmessage('pause');}
          end;   {finally}

       except
          raise;
       end;   {except}
   end;

{ ------------------------------------------------------------ }


   procedure preprocess_for_output (const configuration : configuration_type; user_selections :
             user_selection_type; var files_exist : boolean);
       {This procedure just checks to make sure intermediate files exist.  If they
        don't, it reports an error, leaving the user to run and create the
        intermediate files.}

    var temp_name : TFileName;
    begin
         files_exist := true;

         if (SystemCapabilities in user_selections.output_format.variables) or
            (AlliancePortfolioWeighted in user_selections.output_format.variables) then
            if not (check_file_Exists (configuration.cow_system_pct_file_name,
                           '% System Capabilities')) then
               files_exist := false;
         if ((riskTau in user_selections.output_format.variables) and
             (user_selections.risk_data_source = risk_EUGENE)) then
            if not (check_file_Exists (configuration.risk_Tau_file_name,
                           'Risk Scores with Tau')) then
               files_exist := false;
         if ((riskTau in user_selections.output_format.variables) and
             (user_selections.risk_data_source = risk_WTR)) then
            if not (check_file_Exists (configuration.risk_wtr_file_name,
                           'Risk Scores with Tau WTR')) then
               files_exist := false;
         if riskS in user_selections.output_format.variables then
            if not (check_file_Exists (configuration.risk_S_unweighted_file_name,
                           'Risk Scores with S')) then
               files_exist := false;
         if ((user_selections.output_this = output_directed_dyads) or (user_selections.output_this = output_nondirected_dyads)) then
            begin
               if tau in user_selections.output_format.variables then
                  if not (check_file_Exists (configuration.tau_file_name,
                                 'Tau-b Scores')) then
                     files_exist := false;
               if ((alliance in user_selections.output_format.variables) or
                   (AlliancePortfolioUnweighted in user_selections.output_format.variables) or
                   (AlliancePortfolioWeighted in user_selections.output_format.variables)) then
                  if user_selections.alliance_data_source = flat_dyadic then
                     begin
                        if not (check_file_Exists (configuration.dyadic_alliance_file_name, 'Alliances')) then
                           files_exist := false;
                     end
                  else
                     EUGeneError ('Alliances selected for output, but alliance_data_source not set to flat_dyadic.  Programming error.  Fatal.',1,stop,error_log);
                  {if user_selections.alliance_data_source = flat_cow_sequence then
                     begin
                        if not (check_file_Exists (configuration.cow_alliance_file_name, 'Alliances')) then
                           files_exist := false;
                     end;  }

               if ((sunweighted in user_selections.output_format.variables) or
                   (sweighted in user_selections.output_format.variables)) then
                  if not (check_file_Exists (configuration.s_file_name,
                                 'S Scores')) then
                     files_exist := false;
               if EUWarTrapTau in user_selections.output_format.variables then
                  if not (check_file_Exists (configuration.EUWarTrap_tau_file_name,
                                 'Expected Utility - War Trap')) then
                     files_exist := false;
               if EUWarTrapS in user_selections.output_format.variables then
                  if not (check_file_Exists (configuration.EUWarTrap_s_unweighted_file_name,
                                 'Expected Utility - War Trap')) then
                     files_exist := false;
               if (EUWarReasonTau in user_selections.output_format.variables) or
                  (EQWarReasonTau in user_selections.output_format.variables) then
                  if not (check_file_Exists (configuration.EUWarReason_Tau_file_name,
                                 'Expected Utility - War and Reason - Tau')) then
                     files_exist := false;
               if (EUWarReasonSUnweighted in user_selections.output_format.variables) or
                  (EQWarReasonSUnweighted in user_selections.output_format.variables) then
                  if not (check_file_Exists (configuration.EUWarReason_S_unweighted_file_name,
                                 'Expected Utility - War and Reason - S unweighted')) then
                     files_exist := false;
               if (EUWarReasonSweighted in user_selections.output_format.variables) or
                  (EQWarReasonSweighted in user_selections.output_format.variables) then
                  if not (check_file_Exists (configuration.EUWarReason_S_weighted_file_name,
                                 'Expected Utility - War and Reason - S weighted')) then
                     files_exist := false;
            end;
      end;    {proc preprocess for output}

{ ------------------------------------------------------------ }

   procedure read_data (const start_year, end_year : year_range; var tau_data : Ttau_array_obj;
          var alliance_data : TAlliance_array_obj; var s_data : Ts_array_obj; var polity3_data : Tpolity_Array_obj;
          var sys_capability_data: Tsys_capability_array_obj; var raw_capability_data: Traw_capability_array_obj;
          var risk_Tau_data : Trisk_attitude_array_obj; var risk_S_data : Trisk_attitude_array_obj;
          var EUWarTrap_Tau_array : TEUWarTrap_array_obj; var EUWarTrap_S_array : TEUWarTrap_array_obj;
          var EUWarReason_Tau_array : TEUWarReason_array_obj;
          var EUWarReason_S_unweighted_array, EUWarReason_S_weighted_array : TEUWarReason_array_obj;
          var minimum_distance_data : Tmindist_array_obj; var werner_peace_years_data : TWernerPeaceYears_obj;
          var ISO_array : TISO_array_obj;
          var User_Data_Sets: Multiple_user_data_set_type;
          const user_selections : user_selection_type; const configuration : configuration_type);
{      var CurrUserDataInfo : user_data_set_selection_ptr;
          Current_User_Data_Set, AUserDataSet, prev_data : ActualUserDataSetPtr;
         {For debugging problems with generic user data structure calls
         temp : variant;
         temp_dyad_array : TgenericDyad_array_obj;
         temp_generic_array : Tgeneric_user_data_obj;}
         var alliance_last_year, alliance_first_year : year_range;

      begin
         {if (ccodes, year, distance, contiguity, powerstatus, region)
              then its already in memory.}
         try
            tau_data := nil;
            alliance_data := nil;
            s_data := nil;
            sys_capability_data := nil;
            raw_capability_data := nil;
            risk_Tau_data := nil;
            risk_S_data := nil;
            EUWarTrap_Tau_array := nil;
            EUWarTrap_S_array := nil;
            EUWarReason_Tau_array := nil;
            EUWarReason_S_unweighted_array := nil;
            EUWarReason_S_weighted_array := nil;
            polity3_data := nil;
            User_Data_Sets := nil;
            werner_peace_years_data := nil;

            if (SystemCapabilities in user_selections.output_format.variables) or
               (AlliancePortfolioWeighted in user_selections.output_format.variables) then
               if ((start_year >= configuration.first_cap_year) and
                   (start_year <= configuration.last_cap_year)) or
                  ((end_year >= configuration.first_cap_year) and
                   (end_year <= configuration.last_cap_year)) or
                  ((start_year <= configuration.first_cap_year) and
                   (end_year >= configuration.last_cap_year)) then
               sys_capability_data := Tsys_capability_array_obj.init
                  (configuration.cow_system_pct_file_name,
                   max(start_year, configuration.first_cap_year),
                   min(end_year, configuration.last_cap_year));



{* note this is reading the modified cap data, do we watn them to have option?  }

            if RawCapabilities in user_selections.output_format.variables then
               if ((start_year >= configuration.first_cap_year) and
                   (start_year <= configuration.last_cap_year)) or
                  ((end_year >= configuration.first_cap_year) and
                   (end_year <= configuration.last_cap_year)) or
                  ((start_year <= configuration.first_cap_year) and
                   (end_year >= configuration.last_cap_year)) then
               raw_capability_data := Traw_capability_array_obj.init
                  (configuration.cow_raw_cap_file_name, configuration.cow_modified_cap_file_name,
                   max(start_year, configuration.first_cap_year),
                   min(end_year, configuration.last_cap_year));


            if (riskTau in user_selections.output_format.variables) or
               (riskdetailsTau in user_selections.output_format.variables) or
               (uncertaintyTau in user_selections.output_format.variables) then
               if (((user_selections.risk_data_source = risk_EUGENE)) and
                   ((start_year >= configuration.first_risk_year) and
                    (start_year <= configuration.last_risk_year)) or
                   ((end_year >= configuration.first_risk_year) and
                    (end_year <= configuration.last_risk_year)) or
                   ((start_year <= configuration.first_risk_year) and
                    (end_year >= configuration.last_risk_year)) )  then
               risk_Tau_data := Trisk_attitude_array_obj.init (configuration.risk_Tau_file_name,
                   configuration.risk_WTR_file_name,
                   max(start_year, configuration.first_risk_year),
                   min(end_year, configuration.last_risk_year), user_selections.risk_data_source);

            if (riskTau in user_selections.output_format.variables) or
               (riskdetailsTau in user_selections.output_format.variables) or
               (uncertaintyTau in user_selections.output_format.variables) then
               if (((user_selections.risk_data_source = risk_WTR) ) and
                   ((start_year >= configuration.first_wtr_risk_year) and
                    (start_year <= configuration.last_wtr_risk_year)) or
                   ((end_year >= configuration.first_wtr_risk_year) and
                    (end_year <= configuration.last_wtr_risk_year)) or
                   ((start_year <= configuration.first_wtr_risk_year) and
                    (end_year >= configuration.last_wtr_risk_year)) )  then
               risk_Tau_data := Trisk_attitude_array_obj.init (configuration.risk_Tau_file_name,
                   configuration.risk_WTR_file_name,
                   max(start_year, configuration.first_risk_year),
                   min(end_year, configuration.last_risk_year), user_selections.risk_data_source);

            if (riskS in user_selections.output_format.variables) or
               (riskdetailsS in user_selections.output_format.variables) or
               (uncertaintyS in user_selections.output_format.variables) then
               if ((user_selections.risk_data_source = risk_EUGENE) and
                   ((start_year >= configuration.first_risk_year) and
                    (start_year <= configuration.last_risk_year)) or
                   ((end_year >= configuration.first_risk_year) and
                    (end_year <= configuration.last_risk_year)) or
                   ((start_year <= configuration.first_risk_year) and
                    (end_year >= configuration.last_risk_year)) )  then
               risk_S_data := Trisk_attitude_array_obj.init (configuration.risk_S_unweighted_file_name,
                   configuration.risk_WTR_file_name,
                   max(start_year, configuration.first_risk_year),
                   min(end_year, configuration.last_risk_year), user_selections.risk_data_source);

            if polity3 in user_selections.output_format.variables then
               if ((start_year >= configuration.first_polity3_year) and
                   (start_year <= configuration.last_polity3_year)) or
                  ((end_year >= configuration.first_polity3_year) and
                   (end_year <= configuration.last_polity3_year)) or
                  ((start_year <= configuration.first_polity3_year) and
                   (end_year >= configuration.last_polity3_year)) then
                      {sometimes want the prior year for lagged vars}
               polity3_data := Tpolity_Array_obj.init
                  (configuration.polity3_file_name,
                   max(start_year-1, configuration.first_polity3_year),
                   min(end_year, configuration.last_polity3_year));

            if (tau in user_selections.output_format.variables) or
               (tauwithleader in user_selections.output_format.variables) then
               if ((start_year >= configuration.first_alliance_year) and
                   (start_year <= configuration.last_alliance_year)) or
                  ((end_year >= configuration.first_alliance_year) and
                   (end_year <= configuration.last_alliance_year)) or
                  ((start_year <= configuration.first_alliance_year) and
                   (end_year >= configuration.last_alliance_year)) then
               tau_data := Ttau_array_obj.init (configuration.tau_file_name,
                   max(start_year, configuration.first_alliance_year),
                   min(end_year, configuration.last_alliance_year));

            {For variables using alliance data directly, need to set dates appropriately
             depending on my, or original COW sequenced, data.}
            case user_selections.alliance_data_source of
               flat_dyadic : begin
                     alliance_first_year := configuration.first_alliance_year;
                     alliance_last_year := configuration.last_alliance_year;
                  end;
               {flat_cow_sequence : begin
                     alliance_first_year := configuration.first_alliance_seq_year;
                     alliance_last_year := configuration.last_alliance_seq_year;
                  end;}
               else
                  EUGeneError ('Tried to set alliance dates in euinoutd, but but alliance_data_source not set to flat_dyadic.  Programming error.  Fatal.',1,stop,error_log);

            end;

            if ((alliance in user_selections.output_format.variables) or
                (AlliancePortfolioUnweighted in user_selections.output_format.variables) or
                (AlliancePortfolioWeighted in user_selections.output_format.variables)) then
               if ((start_year >= alliance_first_year) and
                   (start_year <= alliance_last_year)) or
                  ((end_year >= alliance_first_year) and
                   (end_year <= alliance_last_year)) or
                  ((start_year <= alliance_first_year) and
                   (end_year >= alliance_last_year)) then
               alliance_data := Talliance_array_obj.init (configuration.cow_alliance_file_name,
                          configuration.alliance_seq_file_name, configuration.dyadic_alliance_file_name,
                          max(start_year, alliance_first_year), min(end_year, alliance_last_year),
                          user_selections.alliance_data_source);

            if (sunweighted in user_selections.output_format.variables) or
               (swithleader in user_selections.output_format.variables) then
               if ((start_year >= configuration.first_alliance_year) and
                   (start_year <= configuration.last_alliance_year)) or
                  ((end_year >= configuration.first_alliance_year) and
                   (end_year <= configuration.last_alliance_year)) or
                  ((start_year <= configuration.first_alliance_year) and
                   (end_year >= configuration.last_alliance_year)) then
               s_data := Ts_array_obj.init (configuration.s_file_name,
                   max(start_year, configuration.first_alliance_year),
                   min(end_year, configuration.last_alliance_year));

            if (sweighted in user_selections.output_format.variables) then
               if (((start_year >= configuration.first_alliance_year) and
                    (start_year <= configuration.last_alliance_year)) or
                   ((end_year >= configuration.first_alliance_year) and
                    (end_year <= configuration.last_alliance_year)) or
                  ((start_year <= configuration.first_alliance_year) and
                   (end_year >= configuration.last_alliance_year)) ) and
                  (((start_year >= configuration.first_cap_year) and
                    (start_year <= configuration.last_cap_year)) or
                   ((end_year >= configuration.first_cap_year) and
                    (end_year <= configuration.last_cap_year)) or
                  ((start_year <= configuration.first_cap_year) and
                   (end_year >= configuration.last_cap_year)) ) then
               s_data := Ts_array_obj.init (configuration.s_file_name,
                   maxintvalue([start_year, configuration.first_alliance_year, configuration.first_cap_year]),
                   minintvalue([end_year, configuration.last_alliance_year, configuration.last_cap_year]));

            if (user_selections.output_this = output_directed_dyads) or
               (user_selections.output_this = output_nondirected_dyads) or
               (user_selections.output_this = output_directed_dispute_initiation_dyads) or
               (user_selections.output_this = output_nondirected_dispute_dyads) then
               begin
                  if EUWarTrapTau in user_selections.output_format.variables then
                     if ((start_year >= configuration.first_EU_year_possible) and
                         (start_year <= configuration.last_EU_year_possible)) or
                        ((end_year >= configuration.first_EU_year_possible) and
                         (end_year <= configuration.last_EU_year_possible)) or
                        ((start_year <= configuration.first_EU_year_possible) and
                         (end_year >= configuration.last_EU_year_possible)) then
                     EUWarTrap_Tau_array := TEUWarTrap_array_obj.init (configuration.EUWarTrap_Tau_file_name,
                         max(start_year, configuration.first_eu_year_possible),
                         min(end_year, configuration.last_eu_year_possible));
                  if EUWarTrapS in user_selections.output_format.variables then
                     if ((start_year >= configuration.first_EU_year_possible) and
                         (start_year <= configuration.last_EU_year_possible)) or
                        ((end_year >= configuration.first_EU_year_possible) and
                         (end_year <= configuration.last_EU_year_possible)) or
                        ((start_year <= configuration.first_EU_year_possible) and
                         (end_year >= configuration.last_EU_year_possible)) then
                     EUWarTrap_S_array := TEUWarTrap_array_obj.init (configuration.EUWarTrap_s_unweighted_file_name,
                         max(start_year, configuration.first_eu_year_possible),
                         min(end_year, configuration.last_eu_year_possible));
                  if ((EUWarReasonTau in user_selections.output_format.variables) or
                      (EQWarReasonTau in user_selections.output_format.variables)) then
                     if ((start_year >= configuration.first_EU_year_possible) and
                         (start_year <= configuration.last_EU_year_possible)) or
                        ((end_year >= configuration.first_EU_year_possible) and
                         (end_year <= configuration.last_EU_year_possible)) or
                        ((start_year <= configuration.first_EU_year_possible) and
                         (end_year >= configuration.last_EU_year_possible)) then
                     EUWarReason_Tau_array := TEUWarReason_array_obj.init (configuration.EUWarReason_Tau_file_name,
                         max(start_year, configuration.first_eu_year_possible),
                         min(end_year, configuration.last_eu_year_possible));
                  if ((EUWarReasonSUnweighted in user_selections.output_format.variables) or
                      (EQWarReasonSUnweighted in user_selections.output_format.variables)) then
                     if ((start_year >= configuration.first_EU_year_possible) and
                         (start_year <= configuration.last_EU_year_possible)) or
                        ((end_year >= configuration.first_EU_year_possible) and
                         (end_year <= configuration.last_EU_year_possible)) or
                        ((start_year <= configuration.first_EU_year_possible) and
                         (end_year >= configuration.last_EU_year_possible)) then
                     EUWarReason_S_unweighted_array := TEUWarReason_array_obj.init (configuration.EUWarReason_S_unweighted_file_name,
                         max(start_year, configuration.first_eu_year_possible),
                         min(end_year, configuration.last_eu_year_possible));
                  if ((EUWarReasonSweighted in user_selections.output_format.variables) or
                      (EQWarReasonSweighted in user_selections.output_format.variables)) then
                     if ((start_year >= configuration.first_EU_year_possible) and
                         (start_year <= configuration.last_EU_year_possible)) or
                        ((end_year >= configuration.first_EU_year_possible) and
                         (end_year <= configuration.last_EU_year_possible)) or
                        ((start_year <= configuration.first_EU_year_possible) and
                         (end_year >= configuration.last_EU_year_possible)) then
                     EUWarReason_S_weighted_array := TEUWarReason_array_obj.init (configuration.EUWarReason_S_weighted_file_name,
                         max(start_year, configuration.first_eu_year_possible),
                         min(end_year, configuration.last_eu_year_possible));

                  if (distance in user_selections.output_format.variables) and
                     (user_selections.distance_method = minimum) then
                     if ((start_year >= configuration.first_mindist_year) and
                         (start_year <= configuration.last_mindist_year)) or
                        ((end_year >= configuration.first_mindist_year) and
                         (end_year <= configuration.last_mindist_year)) or
                        ((start_year <= configuration.first_mindist_year) and
                         (end_year >= configuration.last_mindist_year)) then
                     minimum_distance_data := Tmindist_array_obj.init(start_year, end_year, configuration.mindist_file_name);

                  if (user_selections.werner_peace_year_adjustment=true) then
                     werner_peace_years_data := TWernerPeaceYears_obj.init(configuration.werner_peace_years_file_name);
               end;

            if (ISO_code in user_selections.output_format.variables) or (ISO_abb2 in user_selections.output_format.variables)
               or (ISO_abb3 in user_selections.output_format.variables) or (ISO_short in user_selections.output_format.variables)
               or (ISO_full in user_selections.output_format.variables) then
                 ISO_array := TISO_array_obj.init(configuration.ISO_main_file, configuration.ISO_lookup_file);

            if (UserVariables in user_selections.output_format.variables) then
               User_data_sets := TMultiple_user_data_set_Obj.init(configuration,
                         start_year, end_year);
               {Note that check and setting of first possible and last possible
                dates will occur within the procedure.}

         except
            sys_capability_data.free;
            risk_Tau_data.free;
            risk_S_data.free;
            tau_data.free;
            alliance_data.free;
            polity3_data.free;
            EUWarTrap_Tau_array.free;
            EUWarTrap_S_array.free;
            EUWarReason_Tau_array.free;
            EUWarReason_S_unweighted_array.free;
            EUWarReason_S_weighted_array.free;
            minimum_distance_data.free;
            sys_capability_data := nil;
            risk_Tau_data := nil;
            risk_S_data := nil;
            tau_data := nil;
            alliance_data := nil;
            polity3_data := nil;
            EUWarTrap_Tau_array := nil;
            EUWarTrap_S_array := nil;
            EUWarReason_Tau_array := nil;
            EUWarReason_S_unweighted_array := nil;
            EUWarReason_S_weighted_array := nil;
            minimum_distance_data := nil;
            User_data_sets.free;
            raise;
         end;
      end;

{ ------------------------------------------------------------ }

   procedure delete_data (const start_year, end_year : year_range;
          var tau_data : Ttau_array_obj; var alliance_data : Talliance_array_obj;
          var s_data : Ts_array_obj; var polity3_data : Tpolity_Array_obj;
          var sys_capability_data: Tsys_capability_array_obj; var raw_capability_data: Traw_capability_array_obj;
          var risk_Tau_data : Trisk_attitude_array_obj; var risk_S_data : Trisk_attitude_array_obj; var EUWarTrap_Tau_array : TEUWarTrap_array_obj;
          var EUWarTrap_S_array : TEUWarTrap_array_obj;
          var EUWarReason_Tau_array : TEUWarReason_array_obj;
          var EUWarReason_S_unweighted_array, EUWarReason_S_weighted_array : TEUWarReason_array_obj;
          var minimum_distance_data : Tmindist_array_obj;
          var User_Data_Sets: Multiple_user_data_set_type;
          const user_selections : user_selection_type; const configuration : configuration_type);
{      var Current_User_Data_Set, prev_data : ActualUserDataSetPtr;
}      begin
         try
            if (SystemCapabilities in user_selections.output_format.variables) or
               (AlliancePortfolioWeighted in user_selections.output_format.variables) then
               if ((start_year >= configuration.first_cap_year) and
                   (start_year <= configuration.last_cap_year)) or
                  ((end_year >= configuration.first_cap_year) and
                   (end_year <= configuration.last_cap_year)) then
               sys_capability_data.free;
            if RawCapabilities in user_selections.output_format.variables then
               if ((start_year >= configuration.first_cap_year) and
                   (start_year <= configuration.last_cap_year)) or
                  ((end_year >= configuration.first_cap_year) and
                   (end_year <= configuration.last_cap_year)) then
               raw_capability_data.free;
            if (riskTau in user_selections.output_format.variables) or
               (riskdetailsTau in user_selections.output_format.variables) or
               (uncertaintyTau in user_selections.output_format.variables) then
               if ((start_year >= configuration.first_risk_year) and
                   (start_year <= configuration.last_risk_year)) or
                  ((end_year >= configuration.first_risk_year) and
                   (end_year <= configuration.last_risk_year)) then
               risk_Tau_data.free;
            if (riskS in user_selections.output_format.variables) or
               (riskdetailsS in user_selections.output_format.variables) or
               (uncertaintyS in user_selections.output_format.variables) then
               if ((start_year >= configuration.first_risk_year) and
                   (start_year <= configuration.last_risk_year)) or
                  ((end_year >= configuration.first_risk_year) and
                   (end_year <= configuration.last_risk_year)) then
               risk_S_data.free;
            if (tau in user_selections.output_format.variables) or
               (tauwithleader in user_selections.output_format.variables) then
               if ((start_year >= configuration.first_alliance_year) and
                   (start_year <= configuration.last_alliance_year)) or
                  ((end_year >= configuration.first_alliance_year) and
                   (end_year <= configuration.last_alliance_year)) then
               tau_data.free;
            if ((alliance in user_selections.output_format.variables) or
                (AlliancePortfolioUnweighted in user_selections.output_format.variables) or
                (AlliancePortfolioWeighted in user_selections.output_format.variables)) then
               if ((start_year >= configuration.first_alliance_year) and
                   (start_year <= configuration.last_alliance_year)) or
                  ((end_year >= configuration.first_alliance_year) and
                   (end_year <= configuration.last_alliance_year)) then
               alliance_data.free;

            if (((sunweighted in user_selections.output_format.variables) or
                 (swithleader in user_selections.output_format.variables)) and
                  (((start_year >= configuration.first_alliance_year) and
                    (start_year <= configuration.last_alliance_year)) or
                   ((end_year >= configuration.first_alliance_year) and
                    (end_year <= configuration.last_alliance_year))))   or
               ((sweighted in user_selections.output_format.variables) and
                (((start_year >= configuration.first_alliance_year) and
                    (start_year <= configuration.last_alliance_year)) or
                   ((end_year >= configuration.first_alliance_year) and
                    (end_year <= configuration.last_alliance_year))) and
                  (((start_year >= configuration.first_cap_year) and
                    (start_year <= configuration.last_cap_year)) or
                   ((end_year >= configuration.first_cap_year) and
                    (end_year <= configuration.last_cap_year)))) then
               s_data.free;
            if polity3 in user_selections.output_format.variables then
               if ((start_year >= configuration.first_polity3_year) and
                   (start_year <= configuration.last_polity3_year)) or
                  ((end_year >= configuration.first_polity3_year) and
                   (end_year <= configuration.last_polity3_year)) then
               polity3_data.free;
            if (user_selections.output_this = output_directed_dyads) or
               (user_selections.output_this = output_nondirected_dyads) or
               (user_selections.output_this = output_directed_dispute_initiation_dyads) or
               (user_selections.output_this = output_nondirected_dispute_dyads) then
               begin
                  if EUWarTrapTau in user_selections.output_format.variables then
                     if ((start_year >= configuration.first_EU_year_possible) and
                         (start_year <= configuration.last_EU_year_possible)) or
                        ((end_year >= configuration.first_EU_year_possible) and
                         (end_year <= configuration.last_EU_year_possible)) then
                     EUWarTrap_Tau_array.free;
                  if EUWarTrapS in user_selections.output_format.variables then
                     if ((start_year >= configuration.first_EU_year_possible) and
                         (start_year <= configuration.last_EU_year_possible)) or
                        ((end_year >= configuration.first_EU_year_possible) and
                         (end_year <= configuration.last_EU_year_possible)) then
                     EUWarTrap_S_array.free;
                  if EUWarReasonTau in user_selections.output_format.variables then
                     if ((start_year >= configuration.first_EU_year_possible) and
                         (start_year <= configuration.last_EU_year_possible)) or
                        ((end_year >= configuration.first_EU_year_possible) and
                         (end_year <= configuration.last_EU_year_possible)) then
                     EUWarReason_Tau_array.free;
                  if (EUWarReasonSUnweighted in user_selections.output_format.variables) then
                     if ((start_year >= configuration.first_EU_year_possible) and
                         (start_year <= configuration.last_EU_year_possible)) or
                        ((end_year >= configuration.first_EU_year_possible) and
                         (end_year <= configuration.last_EU_year_possible)) then
                     EUWarReason_S_unweighted_array.free;
                  if (EUWarReasonSweighted in user_selections.output_format.variables) then
                     if ((start_year >= configuration.first_EU_year_possible) and
                         (start_year <= configuration.last_EU_year_possible)) or
                        ((end_year >= configuration.first_EU_year_possible) and
                         (end_year <= configuration.last_EU_year_possible)) then
                     EUWarReason_S_weighted_array.free;
                  if (distance in user_selections.output_format.variables) and
                     (user_selections.distance_method = minimum) then
                     if ((start_year >= configuration.first_mindist_year) and
                         (start_year <= configuration.last_mindist_year)) or
                        ((end_year >= configuration.first_mindist_year) and
                         (end_year <= configuration.last_mindist_year)) or
                        ((start_year <= configuration.first_mindist_year) and
                         (end_year >= configuration.last_mindist_year)) then
                     minimum_distance_data.free;

               end;

            if (UserVariables in user_selections.output_format.variables) then
               User_data_sets.free;

         except
            sys_capability_data.free;
            risk_Tau_data.free;
            risk_S_data.free;
            tau_data.free;
            alliance_data.free;
            s_data.free;
            polity3_data.free;
            EUWarTrap_Tau_array.free;
            EUWarTrap_S_array.free;
            EUWarReason_Tau_array.free;
            EUWarReason_S_unweighted_array.free;
            EUWarReason_S_weighted_array.free;
            User_data_sets.free;
            sys_capability_data := nil;
            risk_Tau_data := nil;
            risk_S_data := nil;
            tau_data := nil;
            polity3_data := nil;
            EUWarTrap_Tau_array := nil;
            EUWarTrap_S_array := nil;
            EUWarReason_Tau_array := nil;
            EUWarReason_S_unweighted_array := nil;
            EUWarReason_S_weighted_array := nil;
            raise;
         end;
      end;   {proc delete data}

{ ------------------------------------------------------------ }

   function is_pol_var (varname : string; polnonpolnames : polnonpollisttype) : boolean;
      {returns true if the var in varname is a polity variable}
   var current : integer;
   begin
      is_pol_var := false;
      current := 0;
      repeat
         inc(current);
         if varname = polnonpolnames.polnames[current] then result := true;
      until (result=true) or (current >= polnonpolnames.numpolnames);
   end;

   function is_nonpol_predefined_var (varname : string; polnonpolnames : polnonpollisttype) : boolean;
      {returns true if the var in varname is not a polity variable}
   var current : integer;
   begin
      is_nonpol_predefined_var := false;
      current := 0;
      repeat
         inc(current);
         if varname = polnonpolnames.nonpolnames[current] then result := true;
      until (result=true) or (current >= polnonpolnames.numnonpolnames);
   end;

   procedure set_header (const configuration : configuration_type; const user_selections : user_selection_type;
             var separator, outstring, outtypes : string; var UserVarNamesAndMissingValuesList : varnameAndMissingValueArrayType;
             var polnonpolnames : polnonpollisttype);
     var {CurrUserDataInfo : user_data_set_selection_ptr;
         Current_User_Data_Set : ActualUserDataSetPtr;}
         avar, var_number : integer;
         x, adataset : integer;
         name, name1, name2 : string;
         existing_user_names : Tstrings;
         uservartype : string;
     {If the user wants a header record this procedure creates it.}

     Procedure SetVarNameArraysAndOutString (var outstring, outtypes : string;
                avariableID : output_variable_type;
                MonadIsavailable : boolean; monadnames : array of string;
                DirectedDyadIsAvailable : boolean; directeddyadnames : array of string;
                NonDirectedDyadIsAvailable : boolean; nondirecteddyadnames : array of string;
                typename : string;
                pol_nonpol_user_var : pol_nonpol_user_var_type; var polnonpolnames : polnonpollisttype);
         var x : integer;

         procedure setpolnonpolnames (avarname : string; pol_nonpol_user_var : pol_nonpol_user_var_type;
                                 var polnonpolnames : polnonpollisttype);
            {puts string variable names on a list from which I can tell if they are
             polity or nonpolity variables later, which is needed for outputting
             appropriate missing values for command files.}
            begin
                case pol_nonpol_user_var of
                     pol : begin
                           inc(polnonpolnames.numpolnames);
                           polnonpolnames.polnames[polnonpolnames.numpolnames] := avarname;
                        end;
                     nonpol : begin
                           inc(polnonpolnames.numnonpolnames);
                           polnonpolnames.nonpolnames[polnonpolnames.numnonpolnames] := avarname;
                        end;
                     user : begin
                           inc(polnonpolnames.numusernames);
                           polnonpolnames.usernames[polnonpolnames.numusernames] := avarname;
                        end;
                  end;  {case}
            end;                        {proc}

         begin         {set var name arrays and outstring}
           if (avariableID in user_selections.output_format.variables) then
              begin
                 {The initial separator is for if the string already has varnames in it}
                 if outstring <> '' then outstring := outstring + separator;
                 if outtypes <> '' then outtypes := outtypes + separator;
                 case user_selections.output_this of
                     output_monads: If MonadIsAvailable then
                        begin
                           for x := 0 to High(monadnames) - 1 do
                              begin
                                 outstring := outstring + monadnames[x] + separator;
                                 outtypes := outtypes + typename + separator;
                                 setpolnonpolnames (monadnames[x], pol_nonpol_user_var, polnonpolnames);
                              end;
                           outstring := outstring + monadnames[High(monadnames)];
                           outtypes := outtypes + typename;
                           setpolnonpolnames (monadnames[High(monadnames)], pol_nonpol_user_var, polnonpolnames);
                        end;
                     output_directed_dyads, output_directed_dispute_initiation_dyads : If DirectedDyadIsAvailable then
                        begin
                           for x := 0 to High(directeddyadnames) - 1 do
                              begin
                                 outstring := outstring + directeddyadnames[x] + separator;
                                 outtypes := outtypes + typename + separator;
                                 setpolnonpolnames (directeddyadnames[x], pol_nonpol_user_var, polnonpolnames);
                              end;
                           outstring := outstring + directeddyadnames[High(directeddyadnames)];
                           outtypes := outtypes + typename;
                           setpolnonpolnames (directeddyadnames[High(directeddyadnames)], pol_nonpol_user_var, polnonpolnames);
                        end;
                     output_nondirected_dyads, output_nondirected_dispute_dyads : If NonDirectedDyadIsAvailable then
                        begin
                           for x := 0 to High(nondirecteddyadnames) - 1 do
                              begin
                                 outstring := outstring + nondirecteddyadnames[x] + separator;
                                 outtypes := outtypes + typename + separator;
                                 setpolnonpolnames (nondirecteddyadnames[x], pol_nonpol_user_var, polnonpolnames);
                              end;
                           outstring := outstring + nondirecteddyadnames[High(nondirecteddyadnames)];
                           outtypes := outtypes + typename;
                           setpolnonpolnames (nondirecteddyadnames[High(nondirecteddyadnames)], pol_nonpol_user_var, polnonpolnames);
                        end;
                  end;    {case output_this of }
               end;   {if avariableID in...}

         end;        {proc set name arrays}

     Procedure SetUserVarNameArraysAndOutString (var UserVarNamesAndMissingValuesList : varnameAndMissingValueArrayType;
                var outstring, outtypes : string;
                MonadIsavailable : boolean; monadnames : array of string;
                DirectedDyadIsAvailable : boolean; directeddyadnames : array of string;
                NonDirectedDyadIsAvailable : boolean; nondirecteddyadnames : array of string;
                typename : string; miss_value : integer);
         var x : integer;
         begin
            {this procedure for user variables is a little different.  It doesn't check to see if any
             variable is selected, because it assumes that is done externally.}
               {The initial separator is for if the string already has varnames in it}
               {outtypes seems to be used only for stata command file creation.}
            if outstring <> '' then outstring := outstring + separator;
            if outtypes <> '' then outtypes := outtypes + separator;
            case user_selections.output_this of
                  output_monads: If MonadIsavailable then
                     begin
                        for x := 0 to High(monadnames) - 1 do
                           begin
                              outstring := outstring + monadnames[x] + separator;
                              outtypes := outtypes + typename + separator;
                              UserVarNamesAndMissingValuesList.Names[UserVarNamesAndMissingValuesList.Num_Vars] := monadnames[x];
                              UserVarNamesAndMissingValuesList.Missing_Value[UserVarNamesAndMissingValuesList.Num_Vars] := miss_value;
                              inc(UserVarNamesAndMissingValuesList.Num_Vars);
                           end;
                        outstring := outstring + monadnames[High(monadnames)];
                        outtypes := outtypes + typename;
                        UserVarNamesAndMissingValuesList.Names[UserVarNamesAndMissingValuesList.Num_Vars] := monadnames[High(monadnames)];
                        UserVarNamesAndMissingValuesList.Missing_Value[UserVarNamesAndMissingValuesList.Num_Vars] := miss_value;
                        inc(UserVarNamesAndMissingValuesList.Num_Vars);
                     end;
                  output_directed_dyads, output_directed_dispute_initiation_dyads : If DirectedDyadIsAvailable then
                     begin
                        for x := 0 to High(directeddyadnames) - 1 do
                           begin
                              outstring := outstring + directeddyadnames[x] + separator;
                              outtypes := outtypes + typename + separator;
                              UserVarNamesAndMissingValuesList.Names[UserVarNamesAndMissingValuesList.Num_Vars] := directeddyadnames[x];
                              UserVarNamesAndMissingValuesList.Missing_Value[UserVarNamesAndMissingValuesList.Num_Vars] := miss_value;
                              inc(UserVarNamesAndMissingValuesList.Num_Vars);
                           end;
                        outstring := outstring + directeddyadnames[High(directeddyadnames)];
                        outtypes := outtypes + typename;
                        UserVarNamesAndMissingValuesList.Names[UserVarNamesAndMissingValuesList.Num_Vars] := directeddyadnames[High(directeddyadnames)];
                        UserVarNamesAndMissingValuesList.Missing_Value[UserVarNamesAndMissingValuesList.Num_Vars] := miss_value;
                        inc(UserVarNamesAndMissingValuesList.Num_Vars);
                     end;
                  output_nondirected_dyads, output_nondirected_dispute_dyads: If NonDirectedDyadIsAvailable then
                     begin
                        for x := 0 to High(nondirecteddyadnames) - 1 do
                           begin
                              outstring := outstring + nondirecteddyadnames[x] + separator;
                              outtypes := outtypes + typename + separator;
                              UserVarNamesAndMissingValuesList.Names[UserVarNamesAndMissingValuesList.Num_Vars] := nondirecteddyadnames[x];
                              UserVarNamesAndMissingValuesList.Missing_Value[UserVarNamesAndMissingValuesList.Num_Vars] := miss_value;
                              inc(UserVarNamesAndMissingValuesList.Num_Vars);
                           end;
                        outstring := outstring + nondirecteddyadnames[High(nondirecteddyadnames)];
                        outtypes := outtypes + typename;
                        UserVarNamesAndMissingValuesList.Names[UserVarNamesAndMissingValuesList.Num_Vars] := nondirecteddyadnames[High(nondirecteddyadnames)];
                        UserVarNamesAndMissingValuesList.Missing_Value[UserVarNamesAndMissingValuesList.Num_Vars] := miss_value;
                        inc(UserVarNamesAndMissingValuesList.Num_Vars);
                     end;
               end;    {case output_this of }
         end;        {proc set user name arrays}

     procedure set_unique_names_monadic (const original_name: string; var name1, name2 : string);
        {constructs two new variable names with a 1 or 2 at end, following conventions.
         For example, gdp will become gdpa and gdpb, or gdp1 gdp2.
         NOTE:  These are indexed 1..8, so name[1] is first char of name.}
         begin
            name1 := 'noname1';
            name2 := 'noname2';
            if length(original_name) <= 7 then
               begin
                  name1 := original_name + '1';
                  name2 := original_name + '2';
               end
            else  {length 8+;  need to change last char of original name.}
               begin
                  if ((original_name[8]='1') or (original_name[8]='2')) then
                     begin
                        name1 := copy(original_name,0,7) + 'a';
                        name2 := copy(original_name,0,7) + 'b';
                     end
                  else
                  if ((original_name[8]='a') or (original_name[8]='b') or
                      (original_name[8]='A') or (original_name[8]='B')) then
                     begin
                        name1 := copy(original_name,0,7) + '1';
                        name2 := copy(original_name,0,7) + '2';
                     end
                  else { didn't end in 1, 2, a, b}
                     {Here, need to drop last char of name and add a 1 or 2}
                     begin
                        name1 := copy(original_name,0,7) + '1';
                        name2 := copy(original_name,0,7) + '2';
                     end

               end;
         end;

     procedure set_unique_names_dyadic (const original_name: string; var name1, name2 : string);
        {constructs two new variable names with a 12 or 21 at end, following conventions,
         for 2 dyadic directions.  If <6 chars, just add 12/21.  If >6 and has 1, 2 at end
         then add ab/ba.  if >6 and has a,b, add 1,2.  If has both ab and 12, add XY/YX}
         begin
            name1 := 'noname12';
            name2 := 'noname21';
            if length(original_name) <= 6 then
               begin
                  name1 := original_name + '12';
                  name2 := original_name + '21';
               end
            else  {length 7+}
            if length(original_name) = 7 then
               begin
                  if ((original_name[7]='1') or (original_name[7]='2')) and
                     ((original_name[7]='a') or (original_name[7]='b') or
                      (original_name[7]='A') or (original_name[7]='B')) then
                     begin
                        name1 := copy(original_name,0,6) + 'xy';
                        name2 := copy(original_name,0,6) + 'yx';
                     end
                  else    {has 12 but not ab}
                  if ((original_name[7]='1') or (original_name[7]='2')) then
                     begin
                        name1 := copy(original_name,0,6) + 'ab';
                        name2 := copy(original_name,0,6) + 'ba';
                     end
                  else    {has ab but not 12}
                  if ((original_name[7]='a') or (original_name[7]='b') or
                      (original_name[7]='A') or (original_name[7]='B')) then
                     begin
                        name1 := copy(original_name,0,6) + '12';
                        name2 := copy(original_name,0,6) + '21';
                     end
                  else {need to make just make last 2 chars 12}
                     begin
                        name1 := copy(original_name,0,6) + '12';
                        name2 := copy(original_name,0,6) + '21';
                     end;
               end
            else {length 8}
               begin
                  if ((original_name[7]='1') or (original_name[7]='2') or
                      (original_name[8]='1') or (original_name[8]='2')) and
                     ((original_name[7]='a') or (original_name[7]='b') or
                      (original_name[7]='A') or (original_name[7]='B') or
                      (original_name[8]='a') or (original_name[8]='b') or
                      (original_name[8]='A') or (original_name[8]='B')) then
                     begin
                        name1 := copy(original_name,0,6) + 'xy';
                        name2 := copy(original_name,0,6) + 'yx';
                     end
                  else    {has 12 but not ab}
                  if ((original_name[7]='1') or (original_name[7]='2') or
                      (original_name[8]='1') or (original_name[8]='2')) then
                     begin
                        name1 := copy(original_name,0,6) + 'ab';
                        name2 := copy(original_name,0,6) + 'ba';
                     end
                  else    {has ab but not 12}
                  if ((original_name[7]='a') or (original_name[7]='b') or
                      (original_name[7]='A') or (original_name[7]='B') or
                      (original_name[8]='a') or (original_name[8]='b') or
                      (original_name[8]='A') or (original_name[8]='B')) then
                     begin
                        name1 := copy(original_name,0,6) + '12';
                        name2 := copy(original_name,0,6) + '21';
                     end
                  else {need to make just make last 2 chars 12}
                     begin
                        name1 := copy(original_name,0,6) + '12';
                        name2 := copy(original_name,0,6) + '21';
                     end;

               end;
         end;

     procedure check_existing_names (Existing_user_names : Tstrings; var name : string);
         {checks to see if "name" is on existing list of variables.  If so, changes the name}
         var noinc : boolean;
             existing_unique_value : integer;
             final2out, last2 : string[2];
         begin
            noinc := true;
            if Existing_user_names.count = 0 then
               begin end   {nothing, can't be on list}
            else
               begin
                  if Existing_user_names.indexof(name) <> -1 then
                     {duplicate exists}
                     begin
                        {Can I just add a number, or do I need to increment
                         an existng 2 digit unique number?}
                        if length (name) < 3 then noinc := true
                        else   {check last 2 chars for numeric}
                           last2 := copy (name, length(name)-1, 2);
                           if (last2[1] in ['0'..'9']) and
                              (last2[2] in ['0'..'9']) then
                           noinc := false;
                        if noinc then
                           begin
                              if length(name) > 6 then name := copy (name, 0, 6);
                              name := name + '01';
                           end
                        else   {must increment last 2 digits}
                           begin
                              try
                                 existing_unique_value := strtoint(last2);
                                 str((existing_unique_value+1):2, final2out);
                                 if final2out[1] = ' ' then final2out[1] := '0';
                                 name := copy(name, 0, length(name)-2) + final2out;
                              except
                                 Showmessage ('Programming error - numbers not number in unique var name set up.  This should not occur.  Notify programmer.');
                                 name := name;
                              end;
                           end;
                        {Now check the new name again against the existing list.}
                        check_existing_names (Existing_user_names, name);
                     end;
               end;
         end;

      begin         {main procedure set_header}
         try
            separator := '';
            case user_selections.output_format.separator of
              tab : separator := chr(9);
              space : separator := ' ';
              comma : separator := ',';
            end;   {case}

            {Initialize pol/nonpol array}
            polnonpolnames.numpolnames := 0;
            polnonpolnames.numnonpolnames := 0;
            polnonpolnames.numusernames := 0;
            for x := 1 to max_predefined_variables do
               begin
                  polnonpolnames.polnames[x] := 'none';
                  polnonpolnames.nonpolnames[x] := 'none';
                  polnonpolnames.usernames[x] := 'none';
               end;

            {outstring and outtypes are set with an initial blank column.  Then, look to make sure
             string has changed before adding separators}
            outstring := '';
            outtypes := '';

            {output order of variables in all procedures is ccode(s), year, major(s),
             capability/ies, risk, contiguity, distance, tau, EU war trap, EU war Reason,
             politically relevant.}

            {NOTE:  THE ORDER OF THESE VARIABLE NAMES MUST MATCH THE ORDER OF THE VARIABLE
             OUTPUT IN THE OUTPUT PROCEDURE BELOW.  IF NOT, LABELS WILL NOT CORRESPOND TO
             THE CORRECT VARIABLES.  }

            SetVarNameArraysAndOutString (outstring, outtypes, ccodes, true, ['ccode'], true, ['ccode1', 'ccode2'],
               true, ['ccode1', 'ccode2'], 'int', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, year, true, ['year'], true, ['year'],
               true, ['year'], 'int', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, abbrevs, true, ['abbrev'], true, ['abbrev1', 'abbrev2'],
               true, ['abbrev1', 'abbrev2'], 'str3', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, ISO_code, true, ['ISOcode'], true, ['ISOcode1', 'ISOcode2'],
                true, ['ISOcode1', 'ISOcode2'], 'int', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, ISO_abb2, true, ['ISO2let'], true, ['ISO2let1', 'ISO2let2'],
                true, ['ISO2let1', 'ISO2let2'], 'str3', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, ISO_abb3, true, ['ISO3let'], true, ['ISO3let1', 'ISO3let2'],
                true, ['ISO3let1', 'ISO3let2'], 'str3', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, ISO_short, true, ['ISOShNm'], true, ['ISOShNm1', 'ISOShNm2'],
                true, ['ISOShNm', 'ISOShNm'], 'str40', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, ISO_full, true, ['ISOLgNm'], true, ['ISOLgNm1', 'ISOLgNm2'],
                true, ['ISOLgNm', 'ISOLgNm'], 'str40', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, SystemCapabilities,
               true, ['cap'], true, ['cap_1', 'cap_2'], true, ['cap_1', 'cap_2'], 'float', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, RawCapabilities,
               true, ['milper', 'milex', 'energy', 'irst', 'upop', 'tpop'],
               true, ['milper_1', 'milex_1', 'energy_1', 'irst_1', 'upop_1', 'tpop_1', 'milper_2', 'milex_2', 'energy_2', 'irst_2', 'upop_2', 'tpop_2'],
               true, ['milper_1', 'milex_1', 'energy_1', 'irst_1', 'upop_1', 'tpop_1', 'milper_2', 'milex_2', 'energy_2', 'irst_2', 'upop_2', 'tpop_2'], 'float', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, PowerStatus, true, ['majpow'], true, ['majpow1', 'majpow2'],
               true, ['majpow1', 'majpow2'], 'byte', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, relregion,
               false, ['BADVAR'], true, ['rlregion'], true, ['rlreg12','rlreg21'], 'byte', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, HomeRegion,
               true, ['region1'], true, ['region1', 'region2'], true, ['region1', 'region2'], 'byte', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, MarkPolRelevant,
               false, ['BADVAR'], true, ['pol_rel'], true, ['pol_rel'], 'byte', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, tau,
               false, ['BADVAR'], true, ['tau_regi', 'tau_glob'], true, ['tau_re1', 'tau_re2', 'tau_glob'], 'float', nonpol, polnonpolnames);
               {for directed, regional is relevant region.  For non, regional is for home region of 1, 2.}

            SetVarNameArraysAndOutString (outstring, outtypes, sunweighted,
               false, ['BADVAR'], true, ['s_un_reg', 's_un_glo'], true, ['s_un_re1', 's_un_re2', 's_un_glo'], 'float', nonpol, polnonpolnames);
               {for directed, regional is relevant region.  For non, regional is for home region of 1, 2.}

            SetVarNameArraysAndOutString (outstring, outtypes, sweighted,
               false, ['BADVAR'], true, ['s_wt_reg', 's_wt_glo'], true, ['s_wt_re1', 's_wt_re2', 's_wt_glo'], 'float', nonpol, polnonpolnames);
               {for directed, regional is relevant region.  For non, regional is for home region of 1, 2.}

            SetVarNameArraysAndOutString (outstring, outtypes, AlliancePortfolioUnweighted,
               false, ['BADVAR'],
               true, ['rportu11','rportu12','rportu13','rportu14','rportu21','rportu22','rportu23','rportu24','rportu31','rportu32','rportu33','rportu34','rportu41','rportu42','rportu43','rportu44','rportn','gportu11','gportu12','gportu13','gportu14','gportu21','gportu22','gportu23','gportu24','gportu31','gportu32','gportu33','gportu34','gportu41','gportu42','gportu43','gportu44','gportn'],
               true, ['rportu11','rportu12','rportu13','rportu14','rportu21','rportu22','rportu23','rportu24','rportu31','rportu32','rportu33','rportu34','rportu41','rportu42','rportu43','rportu44','rportn','gportu11','gportu12','gportu13','gportu14','gportu21','gportu22','gportu23','gportu24','gportu31','gportu32','gportu33','gportu34','gportu41','gportu42','gportu43','gportu44','gportn'], 'byte', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, AllianceportfolioWeighted,
               false, ['BADVAR'],
               true, ['rportw11','rportw12','rportw13','rportw14','rportw21','rportw22','rportw23','rportw24','rportw31','rportw32','rportw33','rportw34','rportw41','rportw42','rportw43','rportw44','rpordmax','gportw11','gportw12','gportw13','gportw14','gportw21','gportw22','gportw23','gportw24','gportw31','gportw32','gportw33','gportw34','gportw41','gportw42','gportw43','gportw44','gpordmax'],
               true, ['rportw11','rportw12','rportw13','rportw14','rportw21','rportw22','rportw23','rportw24','rportw31','rportw32','rportw33','rportw34','rportw41','rportw42','rportw43','rportw44','rpordmax','gportw11','gportw12','gportw13','gportw14','gportw21','gportw22','gportw23','gportw24','gportw31','gportw32','gportw33','gportw34','gportw41','gportw42','gportw43','gportw44','gpordmax'], 'float', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, tauWithLeader,
               true, ['tau_lead'], true, ['tau_ld_1', 'tau_ld_2'],
               true, ['tau_ld_1', 'tau_ld_2'], 'float', nonpol, polnonpolnames);
               {if global, just tau.  If regional then 1) in relevant region;  2) in relevant region 1 and leader, then 2 and leader;
                3) in home region of 1, in home region of 2.  }

            SetVarNameArraysAndOutString (outstring, outtypes, sWithLeader,
               true, ['s_lead'], true, ['s_ld_1', 's_ld_2'],
               true, ['s_ld_1', 's_ld_2'], 'float', nonpol, polnonpolnames);
               {if global, just tau.  If regional then 1) in relevant region;  2) in relevant region 1 and leader, then 2 and leader;
                3) in home region of 1, in home region of 2.  }

            SetVarNameArraysAndOutString (outstring, outtypes, alliance,
               false, ['BADVAR'], true, ['alliance'],
               true, ['alliance'], 'byte', nonpol, polnonpolnames);

            if polity3 in user_selections.output_format.variables then
               begin
                  SetVarNameArraysAndOutString (outstring, outtypes, democ,
                     true, ['democ'], true, ['democ1', 'democ2'], true, ['democ1', 'democ2'], 'byte', pol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, autoc,
                     true, ['autoc'], true, ['autoc1', 'autoc2'], true, ['autoc1', 'autoc2'], 'byte', pol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, xrreg,
                     true, ['xrreg'], true, ['xrreg1', 'xrreg2'], true, ['xrreg1', 'xrreg2'], 'byte', pol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, xrcomp,
                     true, ['xrcomp'], true, ['xrcomp1', 'xrcomp2'], true, ['xrcomp1', 'xrcomp2'], 'byte', pol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, xropen,
                     true, ['xropen'], true, ['xropen1', 'xropen2'], true, ['xropen1', 'xropen2'], 'byte', pol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, mono,
                     true, ['mono'], true, ['mono1', 'mono2'], true, ['mono1', 'mono2'], 'byte', pol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, xconst,
                     true, ['xconst'], true, ['xconst1', 'xconst2'], true, ['xconst1', 'xconst2'], 'byte', pol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, parreg,
                     true, ['parreg'], true, ['parreg1', 'parreg2'], true, ['parreg1', 'parreg2'], 'byte', pol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, parcomp,
                     true, ['parcomp'], true, ['parcomp1', 'parcomp2'], true, ['parcomp1', 'parcomp2'], 'byte', pol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, cent,
                     true, ['cent'], true, ['cent1', 'cent2'], true, ['cent1', 'cent2'], 'byte', pol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, dem,
                     true, ['dem'], true, ['dem1', 'dem2'], true, ['dem1', 'dem2'], 'byte', pol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, laggeddem,
                     true, ['democlg','autoclg','demlg'],
                     true, ['democlg1','democlg2','autoclg1','autoclg2','demlg1','demlg2'],
                     true, ['democlg1','democlg2','autoclg1','autoclg2','demlg1','demlg2'], 'byte', pol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, democratization,
                     true, ['demchg'], true, ['demchg1', 'demchg2'], true, ['demchg1', 'demchg2'], 'byte', pol, polnonpolnames);
               end;

            {Now additional variables for dyadic output only, including output with disputes }

            SetVarNameArraysAndOutString (outstring, outtypes, contig,
               false, ['BADVAR'], true, ['contig'], true, ['contig'], 'byte', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, ColonialContig,
               false, ['BADVAR'], true, ['colcont','colony1','colony2'], true, ['colcont','colony1','colony2'], 'byte', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, distance,
               false, ['BADVAR'], true, ['distance'], true, ['distance'], 'float', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, DyadicTime,
               false, ['BADVAR'], true, ['dyaddur'], true, ['dyaddur'], 'int', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, StatesInSystem,
               true, ['numstate'], true, ['numstate'], true, ['numstate'], 'int', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, GPsInSystem,
               true, ['numGPs'], true, ['numGPs'], true, ['numGPs'], 'int', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, SysConcentration,
               true, ['syscon'], true, ['syscon'], true, ['syscon'], 'float', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, SysMovement,
               true, ['sysmove1'], true, ['sysmove1'], true, ['sysmove1'], 'float', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, SysMovement5Yr,
               true, ['sysmove5'], true, ['sysmove5'], true, ['sysmove5'], 'float', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, SysMoveGP,
               true, ['sysmvGP1'], true, ['sysmvGP1'], true, ['sysmvGP1'], 'float', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, SysMoveGP5Yr,
               true, ['sysmvGP5'], true, ['sysmvGP5'], true, ['sysmvGP5'], 'float', nonpol, polnonpolnames);

           SetVarNameArraysAndOutString (outstring, outtypes, EUWarTrapTau,
               false, ['BADVAR'], true, ['euwtT1v2'], true, ['euwtT1v2','euwtT2v1'], 'float', nonpol, polnonpolnames);

           SetVarNameArraysAndOutString (outstring, outtypes, EUWarTrapS,
               false, ['BADVAR'], true, ['euwtS1v2'], true, ['euwtS1v2','euwtS2v1'], 'float', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, riskTau,
               true, ['riskTeur','riskTmid','riskTafr','riskTasi','riskTame','riskTglo'],
               true, ['riskT1', 'riskT2'], true, ['riskT11', 'riskT12', 'riskT21', 'riskT22'], 'float', nonpol, polnonpolnames);
               {various regions;  relevant region;  1 in 1s home region, 1 in 2s, 2 in 1's, 2 in 2's}

            SetVarNameArraysAndOutString (outstring, outtypes, riskS,
               true, ['riskSeur','riskSmid','riskSafr','riskSasi','riskSame','riskSglo'],
               true, ['riskS1', 'riskS2'], true, ['riskS11', 'riskS12', 'riskS21', 'riskS22'], 'float', nonpol, polnonpolnames);
               {various regions;  relevant region;  1 in 1s home region, 1 in 2s, 2 in 1's, 2 in 2's}

            SetVarNameArraysAndOutString (outstring, outtypes, riskdetailsTau, true,
               ['seurTact','seurTmax','seurTmin','smidTact','smidTmax','smidTmin',
                'safrTact','safrTmax','safrTmin','sasiTact','sasiTmax','sasiTmin',
                'sameTact','sameTmax','sameTmin','sgloTact','sgloTmax','sgloTmin'],
               true, ['s1Tact','s1Tmax','s1Tmin','s2Tact','s2Tmax','s2Tmin'],
               true, ['s1Tact1','s1Tmax1','s1Tmin1','s1Tact2','s1Tmax2','s1Tmin2',
                      's2Tact1','s2Tmax1','s2Tmin1','s2Tact2','s2Tmax2','s2Tmin2'], 'float', nonpol, polnonpolnames);
               {various regions;  relevant region;  1 in 1s home region, 1 in 2s, 2 in 1's, 2 in 2's}

            SetVarNameArraysAndOutString (outstring, outtypes, riskdetailsS, true,
               ['seurSact','seurSmax','seurSmin','smidSact','smidSmax','smidSmin',
                'safrSact','safrSmax','safrSmin','sasiSact','sasiSmax','sasiSmin',
                'sameSact','sameSmax','sameSmin','sgloSact','sgloSmax','sgloSmin'],
               true, ['s1Sact','s1Smax','s1Smin','s2Sact','s2Smax','s2Smin'],
               true, ['s1Sact1','s1Smax1','s1Smin1','s1Sact2','s1Smax2','s1Smin2',
                      's2Sact1','s2Smax1','s2Smin1','s2Sact2','s2Smax2','s2Smin2'], 'float', nonpol, polnonpolnames);
               {various regions;  relevant region;  1 in 1s home region, 1 in 2s, 2 in 1's, 2 in 2's}

            SetVarNameArraysAndOutString (outstring, outtypes, uncertaintyTau,
               true, ['uncertT'], true, ['uncertT'], true, ['uncertT1', 'uncertT2'], 'float', nonpol, polnonpolnames);
               {uncert in home region, relevant region, home region 1 home region 2}

            SetVarNameArraysAndOutString (outstring, outtypes, uncertaintyS,
               true, ['uncertS'], true, ['uncertS'], true, ['uncertS1', 'uncertS2'], 'float', nonpol, polnonpolnames);
               {uncert in home region, relevant region, home region 1 home region 2}

            SetVarNameArraysAndOutString (outstring, outtypes, EuWarReasonTau,
               false, ['BADVAR'],
               true, ['wrTu1v1','wrTu1v2','wrTu1vsq','wrTp1win','wrTstk1','wrTu1sq',
                      'wrTu1ac1','wrTu1ac2','wrTu1neg','wrTu1cp1','wrTu1cp2','wrTu1wr1','wrTu1wr2',
                      'wrTu2v2','wrTu2v1','wrTu2vsq','wrTp2win','wrTstk2','wrTu2sq',
                      'wrTu2ac2','wrTu2ac1','wrTu2neg','wrTu2cp2','wrTu2cp1','wrTu2wr2','wrTu2wr1'],
               true, ['wrTu1v1','wrTu1v2','wrTu1vsq','wrTp1win','wrTstk1','wrTu1sq',
                      'wrTu1ac1','wrTu1ac2','wrTu1neg','wrTu1cp1','wrTu1cp2','wrTu1wr1','wrTu1wr2',
                      'wrTu2v2','wrTu2v1','wrTu2vsq','wrTp2win','wrTstk2','wrTu2sq',
                      'wrTu2ac2','wrTu2ac1','wrTu2neg','wrTu2cp2','wrTu2cp1','wrTu2wr2','wrTu2wr1'], 'float', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, EUWarReasonSUnweighted,
               false, ['BADVAR'],
               true, ['wrSu1v1','wrSu1v2','wrSu1vsq','wrSp1win','wrSstk1','wrSu1sq',
                      'wrSu1ac1','wrSu1ac2','wrSu1neg','wrSu1cp1','wrSu1cp2','wrSu1wr1','wrSu1wr2',
                      'wrSu2v2','wrSu2v1','wrSu2vsq','wrSp2win','wrSstk2','wrSu2sq',
                      'wrSu2ac2','wrSu2ac1','wrSu2neg','wrSu2cp2','wrSu2cp1','wrSu2wr2','wrSu2wr1'],
               true, ['wrSu1v1','wrSu1v2','wrSu1vsq','wrSp1win','wrSstk1','wrSu1sq',
                      'wrSu1ac1','wrSu1ac2','wrSu1neg','wrSu1cp1','wrSu1cp2','wrSu1wr1','wrSu1wr2',
                      'wrSu2v2','wrSu2v1','wrSu2vsq','wrSp2win','wrSstk2','wrSu2sq',
                      'wrSu2ac2','wrSu2ac1','wrSu2neg','wrSu2cp2','wrSu2cp1','wrSu2wr2','wrSu2wr1'], 'float', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, EUWarReasonSweighted,
               false, ['BADVAR'],
               true, ['wrWu1v1','wrWu1v2','wrWu1vsq','wrWp1win','wrWstk1','wrWu1sq',
                      'wrWu1ac1','wrWu1ac2','wrWu1neg','wrWu1cp1','wrWu1cp2','wrWu1wr1','wrWu1wr2',
                      'wrWu2v2','wrWu2v1','wrWu2vsq','wrWp2win','wrWstk2','wrWu2sq',
                      'wrWu2ac2','wrWu2ac1','wrWu2neg','wrWu2cp2','wrWu2cp1','wrWu2wr2','wrWu2wr1'],
               true, ['wrWu1v1','wrWu1v2','wrWu1vsq','wrWp1win','wrWstk1','wrWu1sq',
                      'wrWu1ac1','wrWu1ac2','wrWu1neg','wrWu1cp1','wrWu1cp2','wrWu1wr1','wrWu1wr2',
                      'wrWu2v2','wrWu2v1','wrWu2vsq','wrWp2win','wrWstk2','wrWu2sq',
                      'wrWu2ac2','wrWu2ac1','wrWu2neg','wrWu2cp2','wrWu2cp1','wrWu2wr2','wrWu2wr1'], 'float', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, EQWarReasonTau,
               false, ['BADVAR'],
               true, ['eqTsq', 'eqTnego', 'eqTacqa', 'eqTacqb', 'eqTcapa', 'eqTcapb', 'eqTwara', 'eqTwarb'],
               true, ['eqTsq12', 'eqTneg12', 'eqTaca12', 'eqTacb12', 'eqTcpa12', 'eqTcpb12', 'eqTwra12', 'eqTwrb12',
                      'eqTsq21', 'eqTneg21', 'eqTaca21', 'eqTacb21', 'eqTcpa21', 'eqTcpb21', 'eqTwra21', 'eqTwrb21'], 'byte', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, EQWarReasonSUnweighted,
               false, ['BADVAR'],
               true, ['eqSsq', 'eqSnego', 'eqSacqa', 'eqSacqb', 'eqScapa', 'eqScapb', 'eqSwara', 'eqSwarb'],
               true, ['eqSsq12', 'eqSneg12', 'eqSaca12', 'eqSacb12', 'eqScpa12', 'eqScpb12', 'eqSwra12', 'eqSwrb12',
                      'eqSsq21', 'eqSneg21', 'eqSaca21', 'eqSacb21', 'eqScpa21', 'eqScpb21', 'eqSwra21', 'eqSwrb21'], 'byte', nonpol, polnonpolnames);

            SetVarNameArraysAndOutString (outstring, outtypes, EQWarReasonSweighted,
               false, ['BADVAR'],
               true, ['eqWsq', 'eqWnego', 'eqWacqa', 'eqWacqb', 'eqWcapa', 'eqWcapb', 'eqWwara', 'eqWwarb'],
               true, ['eqWsq12', 'eqWneg12', 'eqWaca12', 'eqWacb12', 'eqWcpa12', 'eqWcpb12', 'eqWwra12', 'eqWwrb12',
                      'eqWsq21', 'eqWneg21', 'eqWaca21', 'eqWacb21', 'eqWcpa21', 'eqWcpb21', 'eqWwra21', 'eqWwrb21'], 'byte', nonpol, polnonpolnames);

            if (COW_disputes in user_selections.output_format.variables) then
               begin
                  SetVarNameArraysAndOutString (outstring, outtypes, COW_disputes,
                     false, ['BADVAR'],
                     true, ['cwongo', 'cwongonm', 'cwinit', 'cwinitnm', 'cwdynm', 'cwhost1', 'cwhost2'],
                     true, ['cwongo', 'cwongonm', 'cwmid', 'cwmidnm', 'cwdynm', 'cwhost1', 'cwhost2'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWMIDHostLevDispute,
                     false, ['BADVAR'], true, ['cwhostd'], true, ['cwhostd'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COW_disputes,
                     false, ['BADVAR'],
                     true, ['cwkeynum'],
                     true, ['cwkeynum'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWMIDOriginator,
                     false, ['BADVAR'], true, ['cworig1', 'cworig2'], true, ['cworig1', 'cworig2'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWMarkMIDJoiners,
                     false, ['BADVAR'], true, ['cwjoanyi', 'cwjoanyt','cwjomidi', 'cwjomidt'], true, ['cwjoany', 'cwjomid'], 'byte', nonpol, polnonpolnames);
                     {for directed, this is state 1 joined the initiating side, state 1 joined the target side.
                      For non-directed, this is either state was a joiner, on either side,
                       so basically just its not an originator dyad.}
                  SetVarNameArraysAndOutString (outstring, outtypes, COWMIDName,
                     false, ['BADVAR'], true, ['cwmidnme'], true, ['cwmidnme'], 'str40', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWMIDStart,
                     false, ['BADVAR'],
                     true, ['cwstmo1', 'cwstday1', 'cwstyr1', 'cwstmo2', 'cwstday2', 'cwstyr2'],
                     true, ['cwstmo1', 'cwstday1', 'cwstyr1', 'cwstmo2', 'cwstday2', 'cwstyr2'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutstring (outstring, outtypes, COWMIDEnd,
                     false, ['BADVAR'],
                     true, ['cwendmo1', 'cwenddy1', 'cwendyr1', 'cwendmo2', 'cwenddy2', 'cwendyr2'],
                     true, ['cwendmo1', 'cwenddy1', 'cwendyr1', 'cwendmo2', 'cwenddy2', 'cwendyr2'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWMIDSideA,
                     false, ['BADVAR'], true, ['cwsideA1', 'cwsideA2'], true, ['cwsideA1', 'cwsideA2'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWMIDRevisionist,
                     false, ['BADVAR'], true, ['cwrevis1', 'cwrevis2'], true, ['cwrevis1', 'cwrevis2'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWMIDRevisiontype,
                     false, ['BADVAR'], true, ['cwrevt11', 'cwrevt21', 'cwrevt12', 'cwrevt22'], true, ['cwrevt11', 'cwrevt21', 'cwrevt12', 'cwrevt22'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWMIDFatalityState,
                     false, ['BADVAR'], true, ['cwfatal1', 'cwfatex1', 'cwfatal2', 'cwfatex2'], true, ['cwfatal1', 'cwfatex1', 'cwfatal2', 'cwfatex2'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWMIDHiActState,
                     false, ['BADVAR'], true, ['cwhiact1', 'cwhiact2'], true, ['cwhiact1', 'cwhiact2'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWMIDHiActDispute,
                     false, ['BADVAR'], true, ['cwhiactd'], true, ['cwhiactd'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWMIDOutcome,
                     false, ['BADVAR'], true, ['cwoutcm'], true, ['cwoutcm'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWMIDSettlement,
                     false, ['BADVAR'], true, ['cwsettle'], true, ['cwsettle'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWMIDFatalityDispute,
                     false, ['BADVAR'], true, ['cwfatald'], true, ['cwfatald'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWMIDReciprocated,
                     false, ['BADVAR'], true, ['cwrecip'], true, ['cwrecip'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWMIDNumStates,
                     false, ['BADVAR'], true, ['cwnumst1', 'cwnumst2'], true, ['cwnumst1', 'cwnumst2'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWNumMIDs,
                     false, ['BADVAR'], true, ['cwnmmdnw', 'cwnmmdal'], true, ['cwnmmdnw', 'cwnmmdal'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWpeaceyrs,
                     false, ['BADVAR'], true, ['cwpceyrs'], true, ['cwpceyrs'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWPeaceDays,
                     false, ['BADVAR'], true, ['cwpcedys'], true, ['cwpcedys'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWRole,
                     false, ['BADVAR'], true, ['cowrolea','cowroleb'], true, ['cowrolea','cowroleb'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, COWLinkStatus,
                     false, ['BADVAR'], true, ['cwlink1','cwlink2','cwlink3','cwon2001'], true, ['cwlink1','cwlink2','cwlink3','cwon2001'], 'int', nonpol, polnonpolnames);
               end;


            if (Maoz_dyadic_disputes in user_selections.output_format.variables) then
               begin
                  SetVarNameArraysAndOutString (outstring, outtypes, Maoz_dyadic_disputes,
                     false, ['BADVAR'],
                     true, ['mzongo', 'mzongonm', 'mzinit', 'mzinitnm', 'mzhost1', 'mzhost2'],
                     true, ['mzongo','mzongonm', 'mzmid', 'mzmidnm','mzhost1', 'mzhost2'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozMIDHostLevDispute,
                     false, ['BADVAR'], true, ['mzhostd'], true, ['mzhostd'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, Maoz_dyadic_disputes,
                     false, ['BADVAR'],
                     true, ['mzkeynum'],
                     true, ['mzkeynum'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozMIDOriginator,
                     false, ['BADVAR'], true, ['mzorig1', 'mzorig2'], true, ['mzorig1', 'mzorig2'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozMarkMIDJoiners,
                     false, ['BADVAR'], true, ['mzjoanyi', 'mzjoanyt','mzjomidi', 'mzjomidt'], true, ['mzjoany', 'mzjomid'], 'byte', nonpol, polnonpolnames);
                     {for directed, this is state 1 joined the initiating side, state 1 joined the target side.
                      For non-directed, this is either state was a joiner, on either side,
                       so basically just its not an originator dyad.}
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozMIDName,
                     false, ['BADVAR'], true, ['mzmidnme'], true, ['mzmidnme'], 'str40', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozMIDStart,
                     false, ['BADVAR'],
                     true, ['mzstmo1', 'mzstday1', 'mzstyr1', 'mzstmo2', 'mzstday2', 'mzstyr2'],
                     true, ['mzstmo1', 'mzstday1', 'mzstyr1', 'mzstmo2', 'mzstday2', 'mzstyr2'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutstring (outstring, outtypes, MaozMIDEnd,
                     false, ['BADVAR'],
                     true, ['mzendmo1', 'mzenddy1', 'mzendyr1', 'mzendmo2', 'mzenddy2', 'mzendyr2'],
                     true, ['mzendmo1', 'mzenddy1', 'mzendyr1', 'mzendmo2', 'mzenddy2', 'mzendyr2'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozMIDSideA,
                     false, ['BADVAR'], true, ['mzsideA1', 'mzsideA2'], true, ['mzsideA1', 'mzsideA2'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozMIDRevisionist,
                     false, ['BADVAR'], true, ['mzrevis1', 'mzrevis2'], true, ['mzrevis1', 'mzrevis2'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozMIDRevisiontype,
                     false, ['BADVAR'], true, ['mzrevt11', 'mzrevt21', 'mzrevt12', 'mzrevt22'], true, ['mzrevt11', 'mzrevt21', 'mzrevt12', 'mzrevt22'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozMIDFatalityState,
                     false, ['BADVAR'], true, ['mzfatal1', 'mzfatex1', 'mzfatal2', 'mzfatex2'], true, ['mzfatal1', 'mzfatex1', 'mzfatal2', 'mzfatex2'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozMIDHiActState,
                     false, ['BADVAR'], true, ['mzhiact1', 'mzhiact2'], true, ['mzhiact1', 'mzhiact2'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozMIDHiActDispute,
                     false, ['BADVAR'], true, ['mzhiactd'], true, ['mzhiactd'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozMIDOutcome,
                     false, ['BADVAR'], true, ['mzoutcm'], true, ['mzoutcm'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozMIDSettlement,
                     false, ['BADVAR'], true, ['mzsettle'], true, ['mzsettle'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozMIDFatalityDispute,
                     false, ['BADVAR'], true, ['mzfatald'], true, ['mzfatald'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozMIDReciprocated,
                     false, ['BADVAR'], true, ['mzrecip'], true, ['mzrecip'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozMIDNumStates,
                     false, ['BADVAR'], true, ['mznumst1', 'mznumst2'], true, ['mznumst1', 'mznumst2'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozCOWWar,
                     false, ['BADVAR'], true, ['mzcowwar'], true, ['mzcowwar'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozDurIndx,
                     false, ['BADVAR'], true, ['mzduridx'], true, ['mzduridx'], 'long', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozDurDays,
                     false, ['BADVAR'], true, ['mzdurday'], true, ['mzdurday'], 'long', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozNumMIDs,
                     false, ['BADVAR'], true, ['mznmmdnw', 'mznmmdal'], true, ['mznmmdnw', 'mznmmdal'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, Maozpeaceyrs,
                     false, ['BADVAR'], true, ['mzpceyrs'], true, ['mzpceyrs'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, Maozpeacedys,
                     false, ['BADVAR'], true, ['mzpcedys'], true, ['mzpcedys'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozRole,
                     false, ['BADVAR'], true, ['mzrolea','mzroleb'], true, ['mzrolea','mzroleb'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, MaozLinkStatus,
                     false, ['BADVAR'], true, ['mzlink1','mzlink2','mzlink3','mzon2001'], true, ['mzlink1','mzlink2','mzlink3','mzon2001'], 'int', nonpol, polnonpolnames);
               end;

            {ICB crises}
            if (ICB_crises in user_selections.output_format.variables) then
               begin
                  SetVarNameArraysAndOutString (outstring, outtypes, ICB_crises,
                     false, ['BADVAR'],
                     true, ['icbongo', 'icbongnm', 'crisis', 'crisno'],
                     true, ['icbongo', 'icbongnm', 'crisis', 'crisno'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, ICBCrisisName,
                     false, ['BADVAR'], true, ['crisname'], true, ['crisname'], 'str25', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, ICBCrisisDyadNumber,
                     false, ['BADVAR'], true, ['crdynum'], true, ['crdynum'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, ICBOneSided,
                     false, ['BADVAR'], true, ['oneside'], true, ['oneside'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, ICBDyadicStart,
                     false, ['BADVAR'],
                     true, ['trgyrdy', 'trgmody', 'trgdady'],
                     true, ['trgyrdy', 'trgmody', 'trgdady'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, ICBDyadicEnd,
                     false, ['BADVAR'],
                     true, ['trmyrdy', 'trmmody', 'trmdady'],
                     true, ['trmyrdy', 'trmmody', 'trmdady'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, ICBDurDays,
                     false, ['BADVAR'], true, ['durdays'], true, ['durdays'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, ICBDurYear,
                     false, ['BADVAR'], true, ['duryear'], true, ['duryear'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, ICBStateStart,
                     false, ['BADVAR'],
                     true, ['yrtriga', 'motriga', 'datriga', 'yrtrigb', 'motrigb', 'datrigb'],
                     true, ['yrtriga', 'motriga', 'datriga', 'yrtrigb', 'motrigb', 'datrigb'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, ICBStateEnd,
                     false, ['BADVAR'],
                     true, ['yrterma', 'moterma', 'daterma', 'yrtermb', 'motermb', 'datermb'],
                     true, ['yrterma', 'moterma', 'daterma', 'yrtermb', 'motermb', 'datermb'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, ICBActorSequence,
                     false, ['BADVAR'], true, ['actnuma', 'actnumb'], true, ['actnuma', 'actnumb'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, ICBCOWMember,
                     false, ['BADVAR'], true, ['cowmema', 'cowmemb'], true, ['cowmema', 'cowmemb'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, ICBGWMember,
                     false, ['BADVAR'], true, ['gwmema', 'gwmemb'], true, ['gwmema', 'gwmemb'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, ICBIntraWar,
                     false, ['BADVAR'], true, ['iwca', 'iwcb'], true, ['iwca', 'iwcb'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, ICBJoiner,
                     false, ['BADVAR'], true, ['icbjoina', 'icbjoinb'], true, ['icbjoina', 'icbjoinb'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, ICBNumCrises,
                     false, ['BADVAR'], true, ['icbnmnew', 'icbnmall'], true, ['icbnmnew', 'icbnmall'], 'byte', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, ICBpeaceyrs,
                     false, ['BADVAR'], true, ['icpceyrs'], true, ['icpceyrs'], 'int', nonpol, polnonpolnames);
                  SetVarNameArraysAndOutString (outstring, outtypes, ICBpeacedys,
                     false, ['BADVAR'], true, ['icpcedys'], true, ['icpcedys'], 'int', nonpol, polnonpolnames);


               end;     {if icb_crises in output}


            UserVarNamesAndMissingValuesList.num_vars := 0;
            if UserVariables in user_selections.output_format.variables then
               begin
                  Existing_user_names := Tstringlist.create;
                  Existing_user_names.clear;
                  for x := 0 to max_user_variables do
                     begin
                        UserVarNamesAndMissingValuesList.names[x] := 'noname';
                        UserVarNamesAndMissingValuesList.missing_value[x] := missing_value;
                     end;
                  {go through each data set}
                  for adataset := 0 to high(user_selections.user_data_sets_selections) do
                     if length (user_selections.user_data_sets_selections[adataset].data_set_variables) > 0 then
                           {want to get each var name for this data set}
                        for x := 0 to length(user_selections.user_data_sets_selections[adataset].data_set_variables)-1 do
                           begin
                              {save variable number in another var just for shorthand;  var number in original
                               configuration info is what's stored in the data_set_variables[x] record.}
                              var_number := user_selections.user_data_sets_selections[adataset].data_set_variables[x];
                              {Name is the main variable name;  in a few cases it will get changed to a unique name.}
                              name := configuration.User_data_set_info.get_data_set_var_name(adataset, var_number);
                              check_existing_names (Existing_user_names, name);
                              case configuration.User_data_set_info.get_data_set_var_type(adataset, var_number) of
                                 varinteger : uservartype := 'long';
                                 varsingle : uservartype := 'float';
                                 varolestr : uservartype := 'str32';
                                 else begin
                                    EUGeneError ('User var type did not have an appropriate type in set header, euinoutd proc.  notify programmer',1,continue, error_log);
                                    uservartype := 'float';
                                    end;
                              end; {case}
                              if name <> configuration.User_data_set_info.get_data_set_var_name(adataset, var_number) then
                                 begin
                                    MessageDlg ('User dataset variables with duplicate names selected.  The variable originally named "'+configuration.User_data_set_info.get_data_set_var_name(adataset, var_number)+'" from user data set tab "'+configuration.User_data_set_info.get_data_set_short_name(adataset)+'" has been renamed to "'+name+'".  Please make a note for using the output dataset.', mtInformation, [mbOK], 0);
                                    configuration.User_data_set_info.reset_data_set_var_name(adataset, var_number, name);
                                 end;
                              Existing_user_names.add(name);
                           case configuration.User_data_set_info.get_data_set_var_unit(adataset,var_number) of
                              identifierversion : begin
                                    SetUserVarNameArraysAndOutString (UserVarNamesAndMissingValuesList, outstring, outtypes,
                                       true, [name],
                                       true, [name],
                                       true, [name], uservartype,
                                       configuration.User_data_set_info.get_data_set_var_missing_value(adataset, var_number));
                                 end;
                              annual : begin
                                    SetUserVarNameArraysAndOutString (UserVarNamesAndMissingValuesList, outstring, outtypes,
                                       true, [name],
                                       true, [name],
                                       true, [name], uservartype,
                                       configuration.User_data_set_info.get_data_set_var_missing_value(adataset, var_number));
                                 end;
                              monadic : begin
                                    {for monadic variables, will sometimes output
                                     2 vars to data set, if var came from monadic input data set}
                                    if configuration.User_data_set_info.get_data_set_unit(adataset) = country_year then
                                       begin
                                          set_unique_names_monadic (name, name1, name2);
                                          SetUserVarNameArraysAndOutString (UserVarNamesAndMissingValuesList, outstring, outtypes,
                                             true, [name],
                                             true, [name1, name2],
                                             true, [name1, name2], uservartype,
                                             configuration.User_data_set_info.get_data_set_var_missing_value(adataset, var_number));
                                       end
                                    else
                                    SetUserVarNameArraysAndOutString (UserVarNamesAndMissingValuesList, outstring, outtypes,
                                       true, [name],
                                       true, [name],
                                       true, [name], uservartype,
                                       configuration.User_data_set_info.get_data_set_var_missing_value(adataset, var_number))
                                 end;
                              dyadic_ordered : begin
                                    {For nondirected output, may output 2 variables,
                                     to have both directions.  }
                                    {If data was from nondirected input data set,
                                     then just output the variables straight.}
                                    if configuration.User_data_set_info.get_data_set_unit(adataset) = nondirected_dyad_year then
                                       SetUserVarNameArraysAndOutString (UserVarNamesAndMissingValuesList, outstring, outtypes,
                                          false, ['BADVAR'],
                                          true, [name],
                                          true, [name], uservartype,
                                          configuration.User_data_set_info.get_data_set_var_missing_value(adataset, var_number))
                                    else if configuration.User_data_set_info.get_data_set_unit(adataset) = directed_dyad_year then
                                       begin
                                          {If data was from directed input data set, though,
                                           then output 2 variables, 2 directions.  Then,
                                           Need unique names.}
                                          set_unique_names_dyadic (name, name1, name2);
                                          SetUserVarNameArraysAndOutString (UserVarNamesAndMissingValuesList, outstring, outtypes,
                                             false, ['BADVAR'],
                                             true, [name],
                                             true, [name1, name2], uservartype,
                                             configuration.User_data_set_info.get_data_set_var_missing_value(adataset, var_number));
                                       end
                                    else
                                       EUGeneError ('Error - reached output section in EUinoutD.pas with ordered dyadic variable and not a dyadic output request.  Notify programmer of error.', 3, continue, error_log);
                                 end;   {dyadic_ordered variable}
                              dyadic_unordered : begin
                                    SetUserVarNameArraysAndOutString (UserVarNamesAndMissingValuesList, outstring, outtypes,
                                       false, ['BADVAR'],
                                       true, [name],
                                       true, [name], uservartype,
                                       configuration.User_data_set_info.get_data_set_var_missing_value(adataset, var_number));
                                 end;
                              else EUGeneError ('Variable of inappropriate type seen in output header/names - notify programmer.', 3, stop, error_log);
                              end;   {case}

                           end;
                  Existing_user_names.free;
               end;   {if user_variables in ...}

         finally

         end;   {try;  no finally or except here, could add.  }
      end;    {Procedure}

     { --------------------------   }

   procedure write_header (var outfile:text; user_selections : user_selection_type;
             var outstring : string);
     {If the user wants a header record this procedure writes it.}
      begin
         try
            {Now that outstring is constructed, write it as header}
            if user_selections.output_format.header = true then
               begin
                  if user_selections.output_format.location=toscreen then
                     outputWindow.Screen_Output.lines.add(outstring)
                  else if user_selections.output_format.location=tofile then
                     writeln (outfile, outstring)
                  else if user_selections.output_format.location=toprinter then
                     writeln (outfile, outstring);
               end;

         except
             on EInOutError do
                begin
                   FileErrorBox.maindo ('Error writing to main output file.',
                                        'File may be in use by another program, may be read-only, ',
                                        'or disk may be full.');
                   FileErrorBox.showmodal;
                   raise;
                end;
         end;
     end;

          { ------------------------------------------------------------ }


   procedure output_user_selections (var cmdfile: text; commentchar: char; outstring: string);

     var i: integer;
         contiguity_text, sweight: string;
         checkset : boolean;

     begin
        case user_selections.output_this of
           output_directed_dyads:        writeln (cmdfile, commentchar, '   ', 'Base Format: Directed Dyad');
           output_nondirected_dyads:     writeln (cmdfile, commentchar, '   ', 'Base Format: Non-directed Dyad');
           output_directed_dispute_initiation_dyads:         writeln (cmdfile, commentchar, '   ', 'Base Format: Directed Dispute Dyad');
           output_nondirected_dispute_dyads:         writeln (cmdfile, commentchar, '   ', 'Base Format: Non-directed Dispute Dyad');
           output_monads:                writeln (cmdfile, commentchar, '   ', 'Base Format: Country-Year');
        end;
        writeln (cmdfile, commentchar, '   ', 'Time Span: ', user_selections.first_year, ' to ', user_selections.last_year);

{what is the proper contiguity level variable?  Forms only allow 1-5 to be selected}

        case user_selections.contiguity_level_required of
           1  : contiguity_text := 'direct contiguity level 1 - land only';
           2  : contiguity_text := 'direct contiguity level 2 - land or within 12 miles over water';
           3  : contiguity_text := 'direct contiguity level 3 - land or within 24 miles over water';
           4  : contiguity_text := 'direct contiguity level 4 - land or within 150 miles over water';
           5  : contiguity_text := 'direct contiguity level 5 - land or within 400 miles over water';
           6  : contiguity_text := 'none';
           7  : contiguity_text := 'colonial contiguity level 1 - land only';
           8  : contiguity_text := 'colonial contiguity level 2 - land or within 12 miles over water';
           9  : contiguity_text := 'colonial contiguity level 3 - land or within 24 miles over water';
           10 : contiguity_text := 'colonial contiguity level 4 - land or within 150 miles over water';
           11 : contiguity_text := 'colonial contiguity level 5 - land or within 400 miles over water';
        end;

        if ((user_selections.output_this = output_directed_dyads) or (user_selections.output_this = output_nondirected_dyads)) then
        begin
           case user_selections.dyads_selected of
              all_states:            writeln (cmdfile, commentchar, '   ', 'Selected Subset: All dyads included)');
              all_gp_gp:             writeln (cmdfile, commentchar, '   ', 'Selected Subset: All major power vs. major power dyads included');
              all_gp_any:            writeln (cmdfile, commentchar, '   ', 'Selected Subset: All major power vs. any state dyads included');
              all_contiguous:        writeln (cmdfile, commentchar, '   ', 'Selected Subset: All contiguous dyads (', contiguity_text, ')');
              within_distance:       begin
                                        writeln (cmdfile, commentchar, '   ', 'Selected Subset: All dyads with an intercapitol distance less than ', user_selections.maximum_distance, ' miles');
                                        case user_selections.distance_method of
                                           capitols:                      writeln (cmdfile, commentchar, '   ', '     Distance measured using capitol coordinates only (no contiguity)');
                                           capitols_contiguity:           writeln (cmdfile, commentchar, '   ', '     Distance measured using capitol coordinates and land contiguity');
                                           capitols_contiguity_war_trap:  writeln (cmdfile, commentchar, '   ', '     Distance measured using capitol coordinates and land contiguity, including modifications from The War Trap');
                                        end;
                                     end;
              within_region:         begin
                                        writeln (cmdfile, commentchar, '   ', 'Selected Subset: All dyads entirely contained within the selected regions');
                                        if europe in user_selections.selected_regions     then writeln (cmdfile, commentchar, '   ', '     Europe');
                                        if middleeast in user_selections.selected_regions then writeln (cmdfile, commentchar, '   ', '     Middle East');
                                        if africa in user_selections.selected_regions     then writeln (cmdfile, commentchar, '   ', '     Africa');
                                        if asia in user_selections.selected_regions       then writeln (cmdfile, commentchar, '   ', '     Asia');
                                        if americas in user_selections.selected_regions   then writeln (cmdfile, commentchar, '   ', '     North and South America');
                                        if globe in user_selections.selected_regions      then writeln (cmdfile, commentchar, '   ', '     All regions');
                                     end;
              politically_relevant : writeln (cmdfile, commentchar, '   ', 'Selected Subset: All politically relevant dyads using ', contiguity_text);
              selected_set:          writeln (cmdfile, commentchar, '   ', 'Selected Subset: Dyads between individually specified states');
              user_file_read:        writeln (cmdfile, commentchar, '   ', 'Selected Subset: As listed in user-specified file ', user_selections.user_specified_dyad_list.file_name);
           end;
        end;
        if (user_selections.output_this = output_directed_dispute_initiation_dyads) or (user_selections.output_this = output_nondirected_dispute_dyads) or (user_selections.output_this = output_nondirected_dispute_dyads) then
        begin
           case user_selections.disputes_selected of
              all_disputes:          writeln (cmdfile, commentchar, '   ', 'Selected Subset: One case per dyadic dispute initiation');
              all_dispute_years:     writeln (cmdfile, commentchar, '   ', 'Selected Subset: One case per dyadic dispute year');
           end;
        end;
        if (user_selections.output_this = output_monads) then
        begin
           case user_selections.monads_selected of
              all_states_mono:       writeln (cmdfile, commentchar, '   ', 'Selected Subset: All states included');
              all_gp_mono:           writeln (cmdfile, commentchar, '   ', 'Selected Subset: All major powers included');
              within_region_mono:    begin
                                        writeln (cmdfile, commentchar, '   ', 'Selected Subset: All states within the selected regions');
                                        if europe in user_selections.selected_regions     then writeln (cmdfile, commentchar, '   ', '     Europe');
                                        if middleeast in user_selections.selected_regions then writeln (cmdfile, commentchar, '   ', '     Middle East');
                                        if africa in user_selections.selected_regions     then writeln (cmdfile, commentchar, '   ', '     Africa');
                                        if asia in user_selections.selected_regions       then writeln (cmdfile, commentchar, '   ', '     Asia');
                                        if americas in user_selections.selected_regions   then writeln (cmdfile, commentchar, '   ', '     North and South America');
                                        if globe in user_selections.selected_regions      then writeln (cmdfile, commentchar, '   ', '     All regions');
                                     end;
              selected_set_mono:     writeln (cmdfile, commentchar, '   ', 'Selected Subset: Individually specified states');
           end;
        end;
        if user_selections.sample_info.sampling = false then
           writeln (cmdfile, commentchar, '   ', 'Sampling: None (100% of available data written to file)')
        else
        begin
           if ((user_selections.output_this = output_directed_dyads) or (user_selections.output_this = output_nondirected_dyads)) then
              writeln (cmdfile, commentchar, '   ', 'Sampling: ',(user_selections.sample_info.proportion_non_dispute_dyads * 100),' percent included');
           if (user_selections.output_this = output_directed_dispute_initiation_dyads) or (user_selections.output_this = output_nondirected_dispute_dyads) then
              writeln (cmdfile, commentchar, '   ', 'Sampling: ',(user_selections.sample_info.proportion_dispute_dyads * 100),' percent included');
           if (user_selections.sample_info.use_randseed = true) then
              writeln (cmdfile, commentchar, '   ', '     Random Seed: ',user_selections.sample_info.randseed);
        end;
        writeln (cmdfile, commentchar, '   ', 'Variables included:');

        writeln (cmdfile, commentchar, '   ', '     ', outstring);

        writeln (cmdfile, commentchar, '   ', 'Selected settings for variables with multiple output options:');
        checkset := false;  {If checkset is still false at the end of all these if-then commands, then we have no complex vars}
        if MarkPolRelevant in user_selections.output_format.variables then
        begin
           writeln (cmdfile, commentchar, '   ', '     Political relevance defined using ',contiguity_text);
           checkset := true;
        end;
        if tauWithLeader in user_selections.output_format.variables then
        begin
           if user_selections.tau_leader_calculation_info = regional then
              writeln (cmdfile, commentchar, '   ', '     Tau with System Leader calculated using regional subsystem tau');
           if user_selections.tau_leader_calculation_info = global then
              writeln (cmdfile, commentchar, '   ', '     Tau with System Leader calculated using global system tau');
           checkset := true;
        end;
        if sWithLeader in user_selections.output_format.variables then
        begin
           if user_selections.s_weighting = 0 then sweight := 'unweighted' else sweight := 'weighted';
           if user_selections.s_leader_calculation_info = regional then
              writeln (cmdfile, commentchar, '   ', '     S with System Leader calculated using ',sweight,' regional subsystem S');
           if user_selections.s_leader_calculation_info = global then
              writeln (cmdfile, commentchar, '   ', '     S with System Leader calculated using ',sweight,' global system S');
           checkset := true;
        end;
        if alliance in user_selections.output_format.variables then
        begin
           if user_selections.alliance_data_source = flat_dyadic then
              writeln (cmdfile, commentchar, '   ', '     Alliances taken from COW dyadic alliance file');
           {if user_selections.alliance_data_source = flat_cow_sequence then
              writeln (cmdfile, commentchar, '   ', '     Alliances taken from COW sequenced alliance file');}
           checkset := true;
        end;
        if distance in user_selections.output_format.variables then
        begin
           case user_selections.distance_method of
              capitols:                      writeln (cmdfile, commentchar, '   ', '     Distance variable measured using capitol coordinates only (no contiguity)');
              capitols_contiguity:           writeln (cmdfile, commentchar, '   ', '     Distance variable measured using capitol coordinates and land contiguity');
              capitols_contiguity_war_trap:  writeln (cmdfile, commentchar, '   ', '     Distance variable measured using capitol coordinates and land contiguity, including modifications from The War Trap');
           end;
           checkset := true;
        end;
        if user_selections.werner_peace_year_adjustment = true
           then writeln (cmdfile, commentchar, '   ', '     Peace years calculated using Werner peace year adjustment')
           else writeln (cmdfile, commentchar, '   ', '     Peace years calculated without Werner peace year adjustment');
        if (EQWarReasonTau in user_selections.output_format.variables)
           or (EQWarReasonSUnweighted in user_selections.output_format.variables)
           or (EQWarReasonSweighted in user_selections.output_format.variables)
        then
        begin
           if user_selections.eu_calculation_info.equilibrium_solution = induction
              then writeln (cmdfile, commentchar, '   ', '     IIG Equilibria calculated using backwards induction');
           if user_selections.eu_calculation_info.equilibrium_solution = logical
              then writeln (cmdfile, commentchar, '   ', '     IIG Equilibria calculated using logical conditions');
           checkset := true;
        end;
        if (COW_disputes in user_selections.output_format.variables)
           or (Maoz_dyadic_disputes in user_selections.output_format.variables)
           or (ICB_crises in user_selections.output_format.variables)
           or (COW_wars in user_selections.output_format.variables)
        then
        begin
           case user_selections.conflict_exclusion_selection of
              cds_COWMID  : writeln (cmdfile, commentchar, '   ', '     Conflict exclusions based on COW MID data');
              cds_MaozMID : writeln (cmdfile, commentchar, '   ', '     Conflict exclusions based on Maoz MID data');
              cds_ICB     : writeln (cmdfile, commentchar, '   ', '     Conflict exclusions based on ICB crisis data');
              cds_COWWar  : writeln (cmdfile, commentchar, '   ', '     Conflict exclusions based on COW war data');
           end;
           writeln (cmdfile, commentchar, '   ', '    Dispute characteristics:');
           case user_selections.output_this of
              output_directed_dyads:     begin
                                            if user_selections.dispute_info.UseFirstDispute = true
                                               then writeln (cmdfile, commentchar, '   ', '     When two or more disputes occur in a year, data is from the first dispute')
                                               else writeln (cmdfile, commentchar, '   ', '     When two or more disputes occur in a year, data is from the highest intensity dispute');
                                            if user_selections.dispute_info.OnlyTrueInitiators = true
                                               then writeln (cmdfile, commentchar, '   ', '     Only originators coded as initiators');
                                            if user_selections.dispute_info.JoinersOnInitiatingSideAsInitiators = true
                                               then writeln (cmdfile, commentchar, '   ', '     Originators and all states on initiating side coded as initiators');
                                            if user_selections.dispute_info.AllJoinersAsInitiators = true
                                               then writeln (cmdfile, commentchar, '   ', '     All originators and joiners coded as initiators');
                                            if user_selections.dispute_info.SideAIsInitiator = true
                                               then writeln (cmdfile, commentchar, '   ', '     Initiator defined as "Side A"')
                                               else writeln (cmdfile, commentchar, '   ', '     Initiator defined as "Revisionist State(s)"');
                                            if (user_selections.output_format.printAllOngoing = true)
                                               and (user_selections.output_format.printOngoingifNewDisp = false)
                                               then writeln (cmdfile, commentchar, '   ', '     All dyad-years with an ongoing MID included');
                                            if (user_selections.output_format.printAllOngoing = false)
                                               and (user_selections.output_format.printOngoingifNewDisp = true)
                                               then writeln (cmdfile, commentchar, '   ', '     Dyad-years with an ongoing MID included iff there is a new MID');
                                            if (user_selections.output_format.printAllOngoing = false)
                                               and (user_selections.output_format.printOngoingifNewDisp = false)
                                               then writeln (cmdfile, commentchar, '   ', '     All dyad-years with an ongoing MID dropped');
                                            if user_selections.dispute_info.MarkSubsequentAsInitiation = true
                                               then writeln (cmdfile, commentchar, '   ', '     Ongoing dispute years treated as new initiations')
                                               else writeln (cmdfile, commentchar, '   ', '     Ongoing dispute years not considered new initiations');
                                            if (user_selections.dispute_info.AlwaysIncludeTgtVsInitiator = true)
                                               and (user_selections.dispute_info.IncludeTgtVsInitiatoriffNew = false)
                                               then writeln (cmdfile, commentchar, '   ', '     Target vs. Initiator directed dyads kept if no new MID');
                                            if (user_selections.dispute_info.AlwaysIncludeTgtVsInitiator = false)
                                               and (user_selections.dispute_info.IncludeTgtVsInitiatoriffNew = true)
                                               then writeln (cmdfile, commentchar, '   ', '     Target vs. Initiator directed dyads dropped if no new MID');
                                            if user_selections.dispute_info.DropJoinerDirectedDyads = true
                                               then writeln (cmdfile, commentchar, '   ', '     Joiner Dyads dropped')
                                               else writeln (cmdfile, commentchar, '   ', '     Joiner Dyads included');
                                         end;
              output_nondirected_dyads:  begin
{Check to see if this uses the same vars as the directed version}
                                            if user_selections.dispute_info.UseFirstDispute = true
                                               then writeln (cmdfile, commentchar, '   ', '     When two or more disputes occur in a year, data is from the first dispute')
                                               else writeln (cmdfile, commentchar, '   ', '     When two or more disputes occur in a year, data is from the highest intensity dispute');
                                            if user_selections.dispute_info.OnlyTrueInitiators = true
                                               then writeln (cmdfile, commentchar, '   ', '     Disputes marked for originators only');
                                            if user_selections.dispute_info.AllJoinersAsInitiators = true
                                               then writeln (cmdfile, commentchar, '   ', '     Disputes marked for originators and joiners');
                                            if (user_selections.output_format.printAllOngoing = true)
                                               and (user_selections.output_format.printOngoingifNewDisp = false)
                                               then writeln (cmdfile, commentchar, '   ', '     All dyad-years with an ongoing MID included');
                                            if (user_selections.output_format.printAllOngoing = false)
                                               and (user_selections.output_format.printOngoingifNewDisp = true)
                                               then writeln (cmdfile, commentchar, '   ', '     Dyad-years with an ongoing MID included iff there is a new MID');
                                            if (user_selections.output_format.printAllOngoing = false)
                                               and (user_selections.output_format.printOngoingifNewDisp = false)
                                               then writeln (cmdfile, commentchar, '   ', '     All dyad-years with an ongoing MID dropped');
                                            if user_selections.dispute_info.MarkSubsequentAsInitiation = true
                                               then writeln (cmdfile, commentchar, '   ', '     Ongoing dispute years treated as new initiations')
                                               else writeln (cmdfile, commentchar, '   ', '     Ongoing dispute years not considered new initiations');
                                            if user_selections.dispute_info.DropJoinerDirectedDyads = true
                                               then writeln (cmdfile, commentchar, '   ', '     Joiner Dyads dropped')
                                               else writeln (cmdfile, commentchar, '   ', '     Joiner Dyads included');
                                         end;
              output_directed_dispute_initiation_dyads:      begin
{Check to see if this uses same vars as directed}
                                            if user_selections.dispute_info.UseFirstDispute = true
                                               then writeln (cmdfile, commentchar, '   ', '     When two or more disputes occur in a year, data is from the first dispute')
                                               else writeln (cmdfile, commentchar, '   ', '     When two or more disputes occur in a year, data is from the highest intensity dispute');
                                            if user_selections.dispute_info.OnlyTrueInitiators = true
                                               then writeln (cmdfile, commentchar, '   ', '     Only originators coded as initiators');
                                            if user_selections.dispute_info.JoinersOnInitiatingSideAsInitiators = true
                                               then writeln (cmdfile, commentchar, '   ', '     Originators and all states on initiating side coded as initiators');
                                            if user_selections.dispute_info.SideAIsInitiator = true
                                               then writeln (cmdfile, commentchar, '   ', '     Initiator defined as "Side A"')
                                               else writeln (cmdfile, commentchar, '   ', '     Initiator defined as "Revisionist State(s)"');
                                            if user_selections.dispute_info.MarkSubsequentAsInitiation = true
                                               then writeln (cmdfile, commentchar, '   ', '     Ongoing dispute years treated as new initiations')
                                               else writeln (cmdfile, commentchar, '   ', '     Ongoing dispute years not considered new initiations');
                                            if (user_selections.dispute_info.AlwaysIncludeTgtVsInitiator = true)
                                               and (user_selections.dispute_info.IncludeTgtVsInitiatoriffNew = false)
                                               then writeln (cmdfile, commentchar, '   ', '     Target vs. Initiator directed dyads kept if no new MID');
                                            if (user_selections.dispute_info.AlwaysIncludeTgtVsInitiator = false)
                                               and (user_selections.dispute_info.IncludeTgtVsInitiatoriffNew = true)
                                               then writeln (cmdfile, commentchar, '   ', '     Target vs. Initiator directed dyads dropped if no new MID');
                                            if user_selections.dispute_info.DropJoinerDirectedDyads = true
                                               then writeln (cmdfile, commentchar, '   ', '     Joiner Dyads dropped')
                                               else writeln (cmdfile, commentchar, '   ', '     Joiner Dyads included');
                                         end;

              output_nondirected_dispute_dyads:      begin
{Check to see if this uses same vars as directed}
                                            if user_selections.dispute_info.UseFirstDispute = true
                                               then writeln (cmdfile, commentchar, '   ', '     When two or more disputes occur in a year, data is from the first dispute')
                                               else writeln (cmdfile, commentchar, '   ', '     When two or more disputes occur in a year, data is from the highest intensity dispute');
                                            if user_selections.dispute_info.OnlyTrueInitiators = true
                                               then writeln (cmdfile, commentchar, '   ', '     Only originators coded as initiators');
                                            if user_selections.dispute_info.JoinersOnInitiatingSideAsInitiators = true
                                               then writeln (cmdfile, commentchar, '   ', '     Originators and all states on initiating side coded as initiators');
                                            if user_selections.dispute_info.SideAIsInitiator = true
                                               then writeln (cmdfile, commentchar, '   ', '     Initiator defined as "Side A"')
                                               else writeln (cmdfile, commentchar, '   ', '     Initiator defined as "Revisionist State(s)"');
                                            if user_selections.dispute_info.MarkSubsequentAsInitiation = true
                                               then writeln (cmdfile, commentchar, '   ', '     Ongoing dispute years treated as new initiations')
                                               else writeln (cmdfile, commentchar, '   ', '     Ongoing dispute years not considered new initiations');
                                            if (user_selections.dispute_info.AlwaysIncludeTgtVsInitiator = true)
                                               and (user_selections.dispute_info.IncludeTgtVsInitiatoriffNew = false)
                                               then writeln (cmdfile, commentchar, '   ', '     Target vs. Initiator directed dyads kept if no new MID');
                                            if (user_selections.dispute_info.AlwaysIncludeTgtVsInitiator = false)
                                               and (user_selections.dispute_info.IncludeTgtVsInitiatoriffNew = true)
                                               then writeln (cmdfile, commentchar, '   ', '     Target vs. Initiator directed dyads dropped if no new MID');
                                            if user_selections.dispute_info.DropJoinerDirectedDyads = true
                                               then writeln (cmdfile, commentchar, '   ', '     Joiner Dyads dropped')
                                               else writeln (cmdfile, commentchar, '   ', '     Joiner Dyads included');
                                         end;
              end; {case statement}
              checkset := true;
        end; {if disputes}

        {other? risk calc, etc.}
        if checkset = false then writeln (cmdfile, commentchar, '   ', '     No complex variables selected');

     end;

          { ------------------------------------------------------------ }

   procedure output_bibliography (var cmdfile: text; commentchar: char);

     var bibliofile: text;
         citation: string;
         i: integer;

     begin
         {The bibliography file, bibliography.txt, is taken in alphabetical order.  Each citation is then
          checked against the chosen variables to see if it is needed.}
         assignfile (bibliofile, configuration.bibliography_file_name);
         reset(bibliofile);
         if (COWPeaceYrs in user_selections.output_format.variables) or
            (MaozPeaceYrs in user_selections.output_format.variables) or
            (ICBPeaceYrs in user_selections.output_format.variables)
           then
           begin
             readln (bibliofile, citation);                      {Beck, Katz, Tucker 1997}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else readln (bibliofile);
         readln(bibliofile);                                     {Bennett 1997; not required}
         readln(bibliofile);                                     {Bennett/Stam 1998a; not required}
         readln(bibliofile);                                     {Bennett/Stam 1998b; not required}
         readln(bibliofile);                                     {Bennett/Stam 1997a; not required}
         readln(bibliofile);                                     {Bennett/Stam 1997b; not required}
         readln(bibliofile);                                     {Bennett/Stam 1995; not required}
         if (tau in user_selections.output_format.variables)
           or (uncertaintyTau in user_selections.output_format.variables)
           or (uncertaintyS in user_selections.output_format.variables)
           then
           begin
             readln (bibliofile, citation);                      {Bueno de Mesquita 1975}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else readln (bibliofile);
         readln(bibliofile);                                     {Bueno de Mesquita 1978; not required}
         if (EUWarTrapTau in user_selections.output_format.variables)
           or (EUWarTrapS in user_selections.output_format.variables)
           or (RelRegion in user_selections.output_format.variables)
           or (tau in user_selections.output_format.variables)
           then
           begin
             readln (bibliofile, citation);                      {Bueno de Mesquita 1981}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else readln (bibliofile);
         if (EUWarReasonTau in user_selections.output_format.variables)
           or (EUWarReasonSUnweighted in user_selections.output_format.variables)
           or (EUWarReasonSweighted in user_selections.output_format.variables)
           or (riskTau in user_selections.output_format.variables)
           or (riskS in user_selections.output_format.variables)
           or (riskdetailsTau in user_selections.output_format.variables)
           or (riskdetailsS in user_selections.output_format.variables)
           then
           begin
             readln (bibliofile, citation);                      {Bueno de Mesquita 1985}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else readln (bibliofile);
         readln (bibliofile);                                    {Bueno de Mesquita and Lalman 1988; not required}
         if (EUWarReasonTau in user_selections.output_format.variables)
           or (EUWarReasonSUnweighted in user_selections.output_format.variables)
           or (EUWarReasonSweighted in user_selections.output_format.variables)
           or (EQWarReasonTau in user_selections.output_format.variables)
           or (EQWarReasonSUnweighted in user_selections.output_format.variables)
           or (EQWarReasonSweighted in user_selections.output_format.variables)
           or (uncertaintyTau in user_selections.output_format.variables)
           or (uncertaintyS in user_selections.output_format.variables)
           then
           begin
             readln (bibliofile, citation);                      {Bueno de Mesquita and Lalman 1992}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else readln (bibliofile);
         readln (bibliofile);                                    {Bueno de Mesqita, Lalman, and Rabushka 1985; not required}
         readln (bibliofile);                                    {Fitzpatrick and Modlin 1986; not required}
         if (alliance in user_selections.output_format.variables)
           then
           begin
             readln (bibliofile, citation);                      {Gibler and Sarkees 2002}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else readln (bibliofile);
         readln (bibliofile);                                    {Goldberg 1989; not required}
         readln (bibliofile);                                    {Hays 1981; not required}
         if (ICB_crises in user_selections.output_format.variables)
           then
           begin
             readln (bibliofile, citation);                      {Hewitt nd}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else readln (bibliofile);
         readln (bibliofile);                                    {Holland 1975; not required}
         if (user_selections.risk_data_source = risk_WTR)
           then
           begin
             readln (bibliofile, citation);                      {Horn 1990}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else readln (bibliofile);
         if (polity3 in user_selections.output_format.variables)
           then
           begin
             readln (bibliofile, citation);                      {Jaggers and Gurr 1995}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else readln (bibliofile);
         if (COW_disputes in user_selections.output_format.variables)
           then
           begin
             readln (bibliofile, citation);                      {Jones, Bremer, and Singer 1996}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else readln (bibliofile);
         if (tauWithLeader in user_selections.output_format.variables)
           or (sWithLeader in user_selections.output_format.variables)
           then
           begin
             readln (bibliofile, citation);                      {Lemke and Reed 1998}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else readln (bibliofile);
         if (Maoz_dyadic_disputes in user_selections.output_format.variables)
           then
           begin
             readln (bibliofile, citation);                      {Maoz 1999}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else readln (bibliofile);
         if (MarkPolRelevant in user_selections.output_format.variables)
           then
           begin
             readln (bibliofile, citation);                      {Maoz and Russett 1993}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else readln (bibliofile);
         if (alliance in user_selections.output_format.variables)
           then
           begin
             readln (bibliofile, citation);                      {Reiter 2000}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else
           begin
              readln (bibliofile);
              readln (bibliofile);
           end;
         readln (bibliofile);                                    {Signorino and Ritter 1999; not required}
         readln (bibliofile);                                    {Singer and Small 1966; not required}
         if (SystemCapabilities in user_selections.output_format.variables)
           or (RawCapabilities in user_selections.output_format.variables)
           or (systemchars in user_selections.output_format.variables)
           then
           begin
             readln (bibliofile, citation);                      {Singer, Bremer, and Stuckey 1972}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else readln (bibliofile);
         if (alliance in user_selections.output_format.variables)
           then
           begin
             readln (bibliofile, citation);                      {Small and Singer 1969}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else readln (bibliofile);
         if (distance in user_selections.output_format.variables)
           or (contig in user_selections.output_format.variables)
           or (ColonialContig in user_selections.output_format.variables)
           or (PowerStatus in user_selections.output_format.variables)
           or (HomeRegion in user_selections.output_format.variables)
           then
           begin
             readln (bibliofile, citation);                      {Small and Singer 1982}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else readln (bibliofile);
         if (distance in user_selections.output_format.variables)
           then
           begin
             readln (bibliofile, citation);                      {Times Atlas 1988}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else readln (bibliofile);
         if user_selections.werner_peace_year_adjustment = true
           then
           begin
             readln (bibliofile, citation);                      {Werner 2000}
             writeln (cmdfile, commentchar, ' ', citation);
           end
           else readln (bibliofile);
         if length(user_selections.user_data_sets_selections) > 0 then
         begin
{How can we get "no citation" as output here?}
           for i := 0 to (length(user_selections.user_data_sets_selections) - 1) do
           begin
             if length(user_selections.user_data_sets_selections[i].data_set_variables) > 0 then
             writeln (cmdfile, commentchar, ' ', configuration.User_data_set_info.get_data_set_citation(i));
           end;
         end;

         close (bibliofile);

     end; {procedure output_bibliography}


          { ------------------------------------------------------------ }

   procedure write_command_files (user_selections : user_selection_type;
             num_cases : longint; var separator, outstring, outtypes : string;
             var UserVarNamesAndMissingValuesList : varnameAndMissingValueArrayType;
             polnonpolnames : polnonpollisttype);
     {If the user wants command files, this procedure writes it.
      num_cases is used for LIMDEP to set nobs and in STATA to estimate memory usage.
      Outstring comes into the proc with the list of variable names in it.}

     type varcharset = set of char;

     var tempfilename, cmdfilename, dicfilename, cmdfilepath : TFileName;
          cmdfile, dicfile, lookup : text;
          x, nvar, nchar : integer;
          mvalnonpol : string;  {string representation of the missing value}
          usernames : array[0..max_user_variables] of string;
          num_user_names : integer;
          numvars : integer;
          memneeded : longint;
          typepos, varpos : integer;
          varchars : varcharset;
          commentchar: char;
          LimdepErrorCheck: boolean;
          polnonpolvarlabel : array of polnonpolvarlabeltype;
          found_label : string;
          count : integer;
          {chTemp : char;
          sTemp : string;
          ctr : integer;
          last_char : boolean;  }
          name1, name2, name3, thislabel : string;
          alongstring : string;



   function havelabel(polnonpolnames : polnonpollisttype; name_to_find : string; var found_label : string) : boolean;
      var loop : integer;
      begin
         result := false;
         found_label := '';
         loop := 0;
         repeat
            if polnonpolvarlabel[loop].name = name_to_find then
               begin
                  result := true;
                  found_label :=polnonpolvarlabel[loop].varlabel;
               end;
            inc(loop);
         until (loop > high(polnonpolvarlabel)) or (result = true);
      end;


   begin
         {for command files, create file with same name as output file, but add
          different extension.}

      {First, read variable labels for Stata, other programs if they allow labels}
      {assign labels by name for non-user variables before doing any specific command file}
      assign(lookup, configuration.label_lookup_file);
      SetLineBreakStyle(lookup, tlbsCRLF);   {doesn't really matter}
      reset(lookup);
      {Assumes first line is just label, so delete it:}
      readln (lookup);
      setlength (polnonpolvarlabel, 0);
      count := 0;
      {ctr := 0;
      last_char := false;   }
      while(Eof(lookup) <> true) do
      begin
        {Each line should have 3 entries and a label.  But because of some odd problems with file, changed
         read procedure to use a readln followed by parsing the line.}
        readln (lookup, alongstring);
        {Need to delete any ' or " in the string or label commands won't work right}
        alongstring := StringReplace(alongstring, '"', '', [rfReplaceAll]);
        alongstring := StringReplace(alongstring, chr(39), '', [rfReplaceAll]);
        name1 := leftstr(alongstring, (pos(',',alongstring)-1)  );
        alongstring := rightstr(alongstring, (length(alongstring)-(pos(',',alongstring))));
        name2 := leftstr(alongstring, (pos(',',alongstring)-1)  );
        alongstring := rightstr(alongstring, (length(alongstring)-(pos(',',alongstring))));
        name3 := leftstr(alongstring, (pos(',',alongstring)-1)  );
        alongstring := rightstr(alongstring, (length(alongstring)-(pos(',',alongstring))));
        thislabel := alongstring;

        inc(count);
        setlength(polnonpolvarlabel, count);
        {This procedure will set the name for as many names as there are.  This could mean some duplicates.}
        if name1 <> '' then
           begin
              polnonpolvarlabel[high(polnonpolvarlabel)].name := name1;
              polnonpolvarlabel[high(polnonpolvarlabel)].varlabel := thislabel;
           end;
        if (name2 <> '') and (name2 <> name1) then
           begin
              polnonpolvarlabel[high(polnonpolvarlabel)].name := name2;
              polnonpolvarlabel[high(polnonpolvarlabel)].varlabel := thislabel;
           end;
        if (name3 <> '') and (name3 <> name1) and (name3 <> name2) then
           begin
              polnonpolvarlabel[high(polnonpolvarlabel)].name := name3;
              polnonpolvarlabel[high(polnonpolvarlabel)].varlabel := thislabel;
           end;
        {Now need to advance to next line of input file}
        {readln(lookup);}


      end;       {while eof <> true}
      closefile(lookup);
      {Now have read the variable labels.}

      if ((spss in user_selections.output_format.commandFiles) or
          (limdep in user_selections.output_format.commandFiles) or
          (stata in user_selections.output_format.commandFiles)) and
          (user_selections.output_format.location = tofile) then
      begin
           {first, set missing values for error command, polity, non_polity data}
         str (missing_value:0, mvalnonpol);
           {Polity data will have to be unique, since any value -66 to -99 is missing}

         varchars := ['a'..'z','A'..'Z','0','1'..'9'];

         {Used to do preprocessing of missing values here onto separate
         strings for polity, nonpolity vars.  This is no longer necessary because
         I enter the procedure with lists of polity, nonpolity var names.  Need these
         separate because there are separate missing values for these types of variables.}
         num_user_names := 0;

         if (spss in user_selections.output_format.commandFiles) and
            (user_selections.output_format.location = tofile) then
            begin
               try
                  cmdfilename := user_selections.output_format.output_file_name + '.sps';
                  assignfile (cmdfile, cmdfilename);
                  rewrite (cmdfile);
                  writeln (cmdfile, '* SPSS command file generated by EUGene version '+realtostring(Eugene_version, 5, 2)+' to read data file');
                  writeln (cmdfile, '* "', user_selections.output_format.output_file_name,'"');
                  writeln (cmdfile, '* To execute this command file: ');
                  writeln (cmdfile, '*    1) Run SPSS,');
                  writeln (cmdfile, '*    2) From the SPSS menus, choose "File -- Open", set type to "Syntax(*.sps)", ');
                  writeln (cmdfile, '*       and locate/select file "', cmdfilename,'"."');
                  writeln (cmdfile, '*    3) Once the syntax file is loaded, choose "Run -- All" from the menus. ');
                  writeln (cmdfile);
                  writeln (cmdfile, '* Number of observations in this data set: ', num_cases);
                  writeln (cmdfile);
                  commentchar := '*';
                  writeln (cmdfile, '* Data set specifications:');
                  output_user_selections(cmdfile,commentchar,outstring);
                  writeln (cmdfile);
                  writeln (cmdfile, '* Note:  EUGene draws on datasets originally published elsewhere by other scholars.  When ');
                  writeln (cmdfile, '* presenting analysis using data generated by EUGene, please cite the original data sources ');
                  writeln (cmdfile, '* as well as EUGene (see EUGene documentation, Appendix D) ');
                  writeln (cmdfile, '*');
                  writeln (cmdfile, '* Bibliography for this data set:');
                  output_bibliography(cmdfile,commentchar);
                  writeln (cmdfile);
                  writeln (cmdfile);
                  writeln (cmdfile, 'data list free file="',user_selections.output_format.output_file_name,'"');
                  write (cmdfile, '  / ');
                  {every time there is a separator character, write a space for spss}
                  for x := 1 to length (outstring) do
                     begin
                        if outstring[x] = separator then write(cmdfile, ' ')
                        else write(cmdfile, outstring[x]);
                     end;
                  writeln (cmdfile, '.');
                  writeln (cmdfile);

                  if polnonpolnames.numnonpolnames > 0 then
                     begin
                        write (cmdfile, 'missing values ');
                        for x := 1 to polnonpolnames.numnonpolnames do
                           write (cmdfile, polnonpolnames.nonpolnames[x] + ' ');
                        write (cmdfile, ' (' + mvalnonpol + ').');
                        writeln (cmdfile);
                     end;

                  if polnonpolnames.numpolnames > 0 then
                     begin
                        write (cmdfile, 'missing values ');
                        for x := 1 to polnonpolnames.numpolnames do
                           write (cmdfile, polnonpolnames.polnames[x] + ' ');
                        write (cmdfile, ' (-99 thru -66).');
                        writeln (cmdfile);
                     end;

                  for x := 0 to UserVarNamesAndMissingValuesList.num_vars-1 do
                     begin
                        writeln (cmdfile, 'missing values ' + UserVarNamesAndMissingValuesList.names[x] +
                                 ' ('+ inttostr(UserVarNamesAndMissingValuesList.missing_value[x]) +').');
                     end;
                  writeln (cmdfile);

                  {The next command drops the case that was read if they included variable names}
                  writeln (cmdfile, '* The next command drops the case that was read if the user included a line of variable names');
                  if user_selections.output_this = output_monads then
                     writeln (cmdfile, 'select if not missing (ccode) and not sysmis(ccode).')
                     else writeln (cmdfile, 'select if not missing (ccode1) and not sysmis(ccode1).');


                  writeln (cmdfile);
                  writeln (cmdfile, 'execute.');
                  close (cmdfile);
               except
                  on EInOutError do
                     begin
                        FileErrorBox.maindo ('Error opening or writing output file for SPSS commands.  ',
                                             'File may be in use by another program, may be read-only, ',
                                             'or disk may be full.  Command file NOT created.');
                        FileErrorBox.showmodal;
                     end;
               end       {except}
            end; {spss output}


         if (limdep in user_selections.output_format.commandFiles) and
            (user_selections.output_format.location = tofile) then
            begin
               try
                  LimdepErrorCheck := false;
                  tempfilename := extractFileName(user_selections.output_format.output_file_name);
                  {need to make this an 8 char name for limdep}
                  cmdfilename := '';
                  x := 0;
                  repeat
                     begin
                        inc(x);
                        if tempfilename[x] <> '.' then cmdfilename := cmdfilename + tempfilename[x];
                     end;
                  until ((x >= length (tempfilename)) or
                         (x >= 8) or
                         (tempfilename[x] = '.'));
                  cmdfilepath := extractFilePath(user_selections.output_format.output_file_name);
                  cmdfilename := cmdfilepath + cmdfilename + '.lim';

                  assignfile (cmdfile, cmdfilename);
                  rewrite (cmdfile);
                  writeln (cmdfile, '? LIMDEP command file generated by EUGene '+realtostring(Eugene_version, 5, 2)+' to read data file');
                  writeln (cmdfile, '? "', user_selections.output_format.output_file_name,'"');
                  writeln (cmdfile, '? To execute this command file: ');
                  writeln (cmdfile, '?    1) Run LIMDEP,');
                  writeln (cmdfile, '?    2) change directory to directory "', extractFilePath(user_selections.output_format.output_file_name),'",');
                  writeln (cmdfile, '?    3) enter command "open ; input = ',extractFileName(cmdfilename),'".');
                  writeln (cmdfile);
                  writeln (cmdfile, '? Number of observations in this data set: ', num_cases);
                  writeln (cmdfile);
                  commentchar := '?';
                  writeln (cmdfile, '? Data set specifications:');
                  output_user_selections(cmdfile,commentchar,outstring);
                  writeln (cmdfile);
                  writeln (cmdfile, '? Note:  EUGene draws on datasets originally published elsewhere by other scholars.  When ');
                  writeln (cmdfile, '? presenting analysis using data generated by EUGene, please cite the original data sources ');
                  writeln (cmdfile, '? as well as EUGene (see EUGene documentation, Appendix D) ');
                  writeln (cmdfile, '?');
                  writeln (cmdfile, '? Bibliography for this data set:');
                  output_bibliography(cmdfile,commentchar);
                  writeln (cmdfile);
                  {need to figure out how many variables for limdep}
                  {do this just by counting how many separators are in the data,
                   and add 1 since last one won't have a separator.}
                  nvar := 0;
                  for x := 1 to length (outstring) do
                     if outstring[x] = separator then inc(nvar);
                  inc (nvar);

                     {NOTE:  if user has chosen to put a header at the top of the file,
                      then it will count as a case to limdep.  SO, to ensure all
                      the data cases are actually read, we need to have num_cases + 1}
                     {But also, if they are generating command files without data, we
                      won't have an n of cases.}
                  if (num_cases = 0) then
                     begin
                        writeln (cmdfile, 'read; nvar=',nvar,'; ');
                     end
                  else  {have valid n of cases}
                     if (user_selections.output_format.header = true) then
                        writeln (cmdfile, 'read; nvar=',nvar,'; nobs=',num_cases + 1,';') else
                        writeln (cmdfile, 'read; nvar=',nvar,'; nobs=',num_cases,';');
                  if nvar > 200 then LimdepErrorCheck := true; {LIMDEP v.7 or earlier cannot handle more than 200 vars}
                  writeln (cmdfile, 'file=', user_selections.output_format.output_file_name,';');
                  write (cmdfile, 'names = ');
                  {every time there is a separator character, write a comma for limdep}
                  nchar := 0;
                  for x := 1 to length (outstring) do
                     begin
                        {limdep has trouble with lines > 80 char, so truncate when we see
                         a separator and we are past the 60th column.  Vars are 8 chars,
                         plus 1 for separator, so writeln even with names= gets in before 80.
                         This will be about 8 variables per line.}
                        if outstring[x] = separator then
                           begin
                              write(cmdfile, ', ');
                              nchar := nchar + 2;
                              if nchar >= 60 then
                                 begin
                                    writeln (cmdfile);
                                    nchar := 0;
                                 end;
                           end
                        else
                           begin
                              write(cmdfile, outstring[x]);
                              inc (nchar);
                           end;
                     end;
                  writeln (cmdfile, ' $');
                  writeln (cmdfile);

                  {now need a missing data command.  Need a line for each var.}
                  if polnonpolnames.numnonpolnames > 0 then
                     begin
                        for x := 1 to polnonpolnames.numnonpolnames do
                           begin
                              write (cmdfile, 'recode; ');
                              write (cmdfile, polnonpolnames.nonpolnames[x]);
                              writeln (cmdfile, '; ' + mvalnonpol + ' = -999 $');
                           end;
                     end;

                  writeln (cmdfile);

                  {Now polity vars, code -99}
                  if polnonpolnames.numpolnames > 0 then
                     begin
                        for x := 1 to polnonpolnames.numpolnames do
                           begin
                              write (cmdfile, 'recode; ');
                              write (cmdfile, polnonpolnames.polnames[x]);
                              writeln (cmdfile, '; -99 / -66 = -999 $');
                           end;
                     end;

                  writeln (cmdfile);

                  for x := 0 to UserVarNamesAndMissingValuesList.num_vars-1 do
                     begin
                        writeln (cmdfile, 'recode; ' + UserVarNamesAndMissingValuesList.names[x] +
                                  '; '+ inttostr(UserVarNamesAndMissingValuesList.missing_value[x]) + ' = -999 $');
                     end;

                  writeln (cmdfile, 'skip $');
                  close (cmdfile);
                  if LimdepErrorCheck = true then
                     EUGeneError ('The data set you have generated contains more than 200 variables.  Depending upon the version of LIMDEP you are using, '+
                            'the LIMDEP command file may not run properly.  If you receive an error message when running this command file in LIMDEP, '+
                            'you will need to reduce the number of variables in the set or use a different statistical package for your analysis.', 2, continue, error_log);
               except
                  on EInOutError do
                     begin
                        FileErrorBox.maindo ('Error opening or writing output file for LIMDEP commands.  ',
                                             'File may be in use by another program, may be read-only, ',
                                             'or disk may be full.  Command file NOT created.');
                        FileErrorBox.showmodal;
                     end;
               end       {except}
            end; {limdep output}


         if (stata in user_selections.output_format.commandFiles) and
            (user_selections.output_format.location = tofile) then
            begin
               try
                  {First compute memory required to load this data set in once created,
                   to add a set memory command to the command file.}
                  numvars := polnonpolnames.numnonpolnames + polnonpolnames.numpolnames +
                             polnonpolnames.numusernames + UserVarNamesAndMissingValuesList.Num_Vars;
                  {Stata uses 4 bytes per float and long, only 1 byte per dummy, and 2 per integer.
                   This should overestimate memory required.}
                  memneeded := numvars * num_cases * 4;
                  {Convert memory needed to kilobytes, and add a 10% overhead cushion +2M}
                  memneeded := round ((memneeded/1024)*1.10) + 2000;

                  {Create dictionary file that will have info about var formats}
                  {dicfilename := user_selections.output_format.output_file_name + '.dct';
                  assignfile (dicfile, dicfilename);
                  rewrite(dicfile);
                  writeln(dicfile, 'dictionary {');
                  {Need to write all chars of a type, then all chars of a var, then rtn.}
                  {typepos := 1;
                  varpos := 1;
                  while not (typepos>=length(outtypes)) and not (varpos>=length(outstring)) do
                     begin
                        {Outer while loop will do each variable/entry, essentially}
                        {indent each line}
                  {      write (dicfile,'     ');
                        while (typepos<length(outtypes)) and (outtypes[typepos] <> separator) do
                           begin
                              write (dicfile, outtypes[typepos]);
                              if typepos < length(outtypes) then inc(typepos);
                           end;
                        {check and write last char from string if necessary}
                  {      if (typepos = length(outtypes)) and ((outtypes[typepos] <> separator)) then
                           write (dicfile, outtypes[typepos]);
                        {Now saw the separator, write space and var name}
                  {      write(dicfile, ' ');
                        while (outstring[varpos] <> separator) and (varpos<length(outstring)) do
                           begin
                              write (dicfile, outstring[varpos]);
                              if varpos < length(outstring) then inc(varpos);
                           end;
                        {check and write last char from string if necessary}
                   {     if (varpos = length(outstring)) and ((outstring[varpos] <> separator)) then
                           write (dicfile, outstring[varpos]);
                        {Move on to next line of dicfile}
                    {    writeln(dicfile);

                        {Now get var and typepos to next character for output}
                    {    if not (typepos>=length(outtypes)) then
                           repeat
                              inc(typepos);
                           until (typepos>=length(outtypes)) or (outtypes[typepos] in varchars);
                        if not (varpos>=length(outstring)) then
                           repeat
                              inc(varpos);
                           until (varpos>=length(outstring)) or (outstring[varpos] in varchars);
                     end;     {while not...}
                  {if not ((typepos>=length(outtypes)) and (varpos>=length(outstring))) then
                     EUGeneError ('end of outstring or outtypes found but end of other not found.  Programming type error.  Notify programmer',1,continue, error_log);
                  writeln (dicfile,'}           {');}  {note this command, if set again, needs to say write dicfile'' with a right curly bracket in the quotes.}
                  {closefile (dicfile);          }

                  cmdfilename := user_selections.output_format.output_file_name + '.do';
                  assignfile (cmdfile, cmdfilename);
                  rewrite (cmdfile);
                  writeln (cmdfile, '* STATA command file generated by EUGene '+realtostring(Eugene_version, 5, 2)+' to read data file');
                  writeln (cmdfile, '* "', user_selections.output_format.output_file_name,'"');
                  writeln (cmdfile, '* To execute this command file: ');
                  writeln (cmdfile, '*    1) Run STATA,');
                  writeln (cmdfile, '*    2) "cd" to directory "', extractFilePath(user_selections.output_format.output_file_name),'",');
                  writeln (cmdfile, '*    3) enter command "do ',extractFileName(cmdfilename),'".');
                  writeln (cmdfile);
                  writeln (cmdfile, '* Number of observations in this data set: ', num_cases);
                  writeln (cmdfile);
                  commentchar := '*';
                  writeln (cmdfile, '* Data set specifications:');
                  output_user_selections(cmdfile,commentchar,outstring);
                  writeln (cmdfile);
                  writeln (cmdfile, '* Note:  EUGene draws on datasets originally published elsewhere by other scholars.  When ');
                  writeln (cmdfile, '* presenting analysis using data generated by EUGene, please cite the original data sources ');
                  writeln (cmdfile, '* as well as EUGene (see EUGene documentation, Appendix D) ');
                  writeln (cmdfile, '*');
                  writeln (cmdfile, '* Bibliography for this data set:');
                  output_bibliography(cmdfile,commentchar);
                  writeln (cmdfile);
                  writeln (cmdfile, '#delimit ;');
                  writeln (cmdfile, 'clear;');
                  writeln (cmdfile, 'set memory '+inttostr(memneeded)+'k;');

                  {write (cmdfile, 'capture infile using "'+dicfilename+'", using("'+
                                  user_selections.output_format.output_file_name+'");');  }

                  {write (cmdfile, 'capture infile ');
                  {every time there is a separator character, write a space for stata}
                  {for x := 1 to length (outstring) do
                     begin
                        if outstring[x] = separator then write(cmdfile, ' ')
                        else write(cmdfile, outstring[x]);
                     end;
                  writeln (cmdfile, ' using "',user_selections.output_format.output_file_name,'";');  }


                  write (cmdfile, 'infile ');
                  {write (cmdfile, 'capture infile ');}

                  {every time there is a separator character, write a space for stata}
                  {Need to write all chars of a type, then all chars of a var, then rtn.}
                  typepos := 1;
                  varpos := 1;
                  while not (typepos>=length(outtypes)) and not (varpos>=length(outstring)) do
                     begin
                        {Outer while loop will do each variable/entry, essentially}
                        {indent each line}
                        while (typepos<length(outtypes)) and (outtypes[typepos] <> separator) do
                           begin
                              write (cmdfile, outtypes[typepos]);
                              if typepos < length(outtypes) then inc(typepos);
                           end;
                        {check and write last char from string if necessary}
                        if (typepos = length(outtypes)) and ((outtypes[typepos] <> separator)) then
                           write (cmdfile, outtypes[typepos]);
                        {Now saw the separator, write space and var name}
                        write(cmdfile, ' ');
                        while (outstring[varpos] <> separator) and (varpos<length(outstring)) do
                           begin
                              write (cmdfile, outstring[varpos]);
                              if varpos < length(outstring) then inc(varpos);
                           end;
                        {check and write last char from string if necessary}
                        if (varpos = length(outstring)) and ((outstring[varpos] <> separator)) then
                           write (cmdfile, outstring[varpos]);
                        write(cmdfile, ' ');
                        {Move on to next variable}

                        {Now get var and typepos to next character for output}
                        if not (typepos>=length(outtypes)) then
                           repeat
                              inc(typepos);
                           until (typepos>=length(outtypes)) or (outtypes[typepos] in varchars);
                        if not (varpos>=length(outstring)) then
                           repeat
                              inc(varpos);
                           until (varpos>=length(outstring)) or (outstring[varpos] in varchars);
                     end;     {while not...}
                  if not ((typepos>=length(outtypes)) and (varpos>=length(outstring))) then
                     EUGeneError ('end of outstring or outtypes found but end of other not found.  Programming type error.  Notify programmer',1,continue, error_log);

                  writeln (cmdfile, ' using "',user_selections.output_format.output_file_name,'";');
                  writeln (cmdfile);

                  {The next command drops the case that was read if they included variable names}
                  writeln (cmdfile, '* The next command drops the case that was read ');
                  writeln (cmdfile, '*  if the user included a line of variable names;');
                  writeln (cmdfile);
                  if user_selections.output_this = output_monads then
                     writeln (cmdfile, 'drop if missing(ccode) | missing(year);')
                     else writeln (cmdfile, 'drop if missing(ccode1) | missing(year);');

                  writeln (cmdfile);
                  writeln (cmdfile, '* Interpret missing values; ');
                  if polnonpolnames.numnonpolnames > 0 then
                     begin
                        write (cmdfile, 'mvdecode ');
                        {every time there is a separator character, always write a space for stata}
                        for x := 1 to polnonpolnames.numnonpolnames do
                           write (cmdfile, polnonpolnames.nonpolnames[x] + ' ');
                        writeln (cmdfile, ', mv(' + mvalnonpol + ');');
                     end;

                  if polnonpolnames.numpolnames > 0 then
                     begin
                        write (cmdfile, 'mvdecode ' );
                        for x := 1 to polnonpolnames.numpolnames do
                           write (cmdfile, polnonpolnames.polnames[x] + ' ');
                        writeln (cmdfile, ', mv(-99);');

                        write (cmdfile, 'mvdecode ' );
                        for x := 1 to polnonpolnames.numpolnames do
                           write (cmdfile, polnonpolnames.polnames[x] + ' ');
                        writeln (cmdfile, ', mv(-88);');

                        write (cmdfile, 'mvdecode ' );
                        for x := 1 to polnonpolnames.numpolnames do
                           write (cmdfile, polnonpolnames.polnames[x] + ' ');
                        writeln (cmdfile, ', mv(-77);');

                        write (cmdfile, 'mvdecode ' );
                        for x := 1 to polnonpolnames.numpolnames do
                           write (cmdfile, polnonpolnames.polnames[x] + ' ');
                        writeln (cmdfile, ', mv(-66);');
                     end;

                     {Now user vars}
                  for x := 0 to UserVarNamesAndMissingValuesList.num_vars-1 do
                     begin
                        writeln (cmdfile, 'mvdecode ' + UserVarNamesAndMissingValuesList.names[x] +
                                 ', mv(' + inttostr(UserVarNamesAndMissingValuesList.missing_value[x]) +');');
                     end;

                  writeln (cmdfile);
                  writeln (cmdfile, '* Variable labels where available (see EUGeneDocumentation.doc for full information); ');
                  if polnonpolnames.numpolnames > 0 then
                     begin
                        {For each variable, see if we have the name on the varlabel list, if so, write the label.}
                        for x := 1 to polnonpolnames.numpolnames do
                           if havelabel(polnonpolnames, polnonpolnames.polnames[x], found_label) then
                              writeln (cmdfile, 'label variable '+polnonpolnames.polnames[x] + ' "' + found_label +'";');
                     end;
                  if polnonpolnames.numnonpolnames > 0 then
                     begin
                        {For each variable, see if we have the name on the varlabel list, if so, write the label.}
                        for x := 1 to polnonpolnames.numnonpolnames do
                           if havelabel(polnonpolnames, polnonpolnames.nonpolnames[x], found_label) then
                              writeln (cmdfile, 'label variable '+polnonpolnames.nonpolnames[x] + ' "' + found_label +'";');
                     end;


                  {Add commands to shrink vars to get smallest possible type.}
                  writeln (cmdfile);
                  writeln (cmdfile, 'compress;');

                  close (cmdfile);


               except
                  on EInOutError do
                     begin
                        close (cmdfile);
                        FileErrorBox.maindo ('Error opening or writing output file for STATA commands.  ',
                                             'File may be in use by another program, may be read-only, ',
                                             'or disk may be full.  Command file NOT created.');
                        FileErrorBox.showmodal;
                     end;
               end       {except}
            end; {stata output}
         end;       {there is some command file}

   end;    {procedure print_command_files}

          { ------------------------------------------------------------ }

   procedure output_one_record (var outfile : text; const ccode1, ccode2 : ccode_range;
       const curryear : year_range; const distance_data : Tdistance_array_obj;
       const minimum_distance_data : Tmindist_array_obj;
       const tau_data : Ttau_array_obj; const alliance_data : TAlliance_array_obj; const s_data : Ts_array_obj; const polity3_data : Tpolity_array_obj;
       const sys_capability_data: Tsys_capability_array_obj; const raw_capability_data: Traw_capability_array_obj;
       const risk_Tau_data : Trisk_attitude_array_obj; const risk_S_data : Trisk_attitude_array_obj;
       const EUWarTrap_Tau_array : TEUWarTrap_array_obj; const EUWarTrap_S_array : TEUWarTrap_array_obj;
       const EUWarReason_Tau_array : TEUWarReason_array_obj; const EUWarReason_S_unweighted_array,
             EUWarReason_S_weighted_array : TEUWarReason_array_obj; const werner_peace_years_data : TWernerPeaceYears_obj;
       const ISO_array : TISO_array_obj;
       var User_Data_Sets: Multiple_user_data_set_type;
       const COW_dyadic_dispute_data_21 : TCOWDyadic_dispute_data_obj_format21;
       const COW_dyadic_dispute_data_30 : TDyadic_dispute_data_obj_integrated_format30;
       const Maoz_dispute_data : TDyadic_dispute_data_obj_integrated_format30;
       const ICB_crisis_data : TICBDyadic_dispute_data_obj; const system_variables : Tsystem_variables_obj;
       const configuration : configuration_type; const user_selections : user_selection_type;
       const use_param : dyadic_call_parameter;
       const input_disp_num_for_cow, input_disp_year_num_for_maoz, input_crisis_num_for_icb  : longint);
      var outstring, separator : string;
          region : region_type;
          x, first, last : integer;
          styear : integer;
          joini_anyinyear, joint_anyinyear, joini_thismid, joint_thismid, init, joina, joinb : boolean;
          ongoing_disp_num, initiation_disp_num, joined_disp_num, specific_sub_dispute_num, found_cow_dispute_num, found_ongoing_num : longint;
          ongoing_disp_year_num, initiation_disp_year_num, joined_disp_year_num, specific_sub_dispute_year_num, found_cow_dispute_year_num : longint;
          crisis_num, found_crisis_num : longint;
          leader_ccode : ccode_range;
          polcount : integer;
          temp_first_year, temp_last_year : year_range;
          adataset, user_var_num, configuration_data_var_number : integer;
          outformat_postdecimal, outformat_totaldigits : integer;
          level : integer;
          colony_code1, colony_code2 : entity_range;
          user_output_value : variant;
          similarity_table : similarity_table_type;
          n : num_countries_range;
          dmax : real;
          row, column : not_active..no_alliance;
          alliance_first_year, alliance_last_year : year_range;

      {this will output either a single ccode-year or dyad-year record.  If called with
       use_param=use_dispute, then will look for that specific dispute in the dyadic_dispute
       records for a dyad year.  }
      {if this is called for monadic output, then ccode2 is irrelevant, and is probably
      passed in as 0.}

      temp : variant;

      procedure count_polity_vars (var polcount : integer);   {counts how many polity vars they selected}
         begin
            polcount := 0;
            if democ in user_selections.output_format.variables then inc(polcount);
            if autoc in user_selections.output_format.variables then inc(polcount);
            if xrreg in user_selections.output_format.variables then inc(polcount);
            if xrcomp in user_selections.output_format.variables then inc(polcount);
            if xropen in user_selections.output_format.variables then inc(polcount);
            if mono in user_selections.output_format.variables then inc(polcount);
            if xconst in user_selections.output_format.variables then inc(polcount);
            if parreg in user_selections.output_format.variables then inc(polcount);
            if parcomp in user_selections.output_format.variables then inc(polcount);
            if cent in user_selections.output_format.variables then inc(polcount);
            if dem in user_selections.output_format.variables then inc(polcount);
            if laggeddem in user_selections.output_format.variables then inc(polcount, 3);
            if democratization in user_selections.output_format.variables then inc(polcount);
         end;

     Procedure SetOutputVarData (var outstring : string;
                avariableID : output_variable_type;
                MonadIsavailable : boolean; monaddata : array of variant;
                DirectedDyadIsAvailable : boolean; directeddyaddata : array of variant;
                NonDirectedDyadIsAvailable : boolean; nondirecteddyaddata : array of variant;
                dataFormat1, dataFormat2 : integer;
                current_year, Range_lowyear, Range_HighYear : year_range;
                var_missing_value : integer);
         var x : integer;

         {Each call to SetOutputVarData uses essentially a triad of inputs, after the first
          2 identifier variables.  The triads in order are for data for output in monadic format,
          directed dyadic format, then non directed dyadic format.  Each of the triads has
          first a boolean marking if that kind of data is available, then an array of values
          which are the data to be sent to the output file.}

         Function FormattedString (onenumber : variant; format1, format2 : integer) : string;
            {takes one variant int or real and returns properly formatted string}
            var val : string;
            begin
               case varType (onenumber) of
                  varSmallInt, varInteger : str (onenumber:format1:0, val);
                  varShortInt, varByte, varWord, varLongWord, varInt64 : str (onenumber:format1:0, val);
                  varSingle, varDouble : str (onenumber:format1:format2, val);
                  varBoolean : if onenumber = true then val := '1' else val := '0';
                  varString : val := '"'+onenumber+'"';
                  else begin
                        EUGeneError ('data passed in for output not string, integer, real, or boolean.  Data set to -9.',1, continue, error_log);
                        val := '-9';
                     end;
                  end;              {case vartype of ... }
               FormattedString := val;
            end;    {function}

         begin
               {now put onto output string}
           if (avariableID in user_selections.output_format.variables) then
              begin
                  {The initial separator is for if the string already has data in it}
                  if outstring <> ' ' then outstring := outstring + separator;
                  case user_selections.output_this of
                     output_monads: If MonadIsAvailable then
                        begin
                           {lowyear, highyear are the first years in the data structure}
                           if ((current_year >= Range_lowyear) and (current_year <= Range_HighYear)) then
                              begin
                                 for x := 0 to High(monaddata) - 1 do
                                    outstring := outstring + FormattedString(monaddata[x], dataFormat1, dataFormat2) + separator;
                                 outstring := outstring + FormattedString(monaddata[high(monaddata)], dataFormat1, dataFormat2);
                              end
                           else
                              begin
                                 for x := 0 to High(monaddata) - 1 do
                                    outstring := outstring + FormattedString(var_missing_value,2,0) + separator;
                                 outstring := outstring + FormattedString(var_missing_value,2,0);
                              end
                        end;
                     output_directed_dyads, output_directed_dispute_initiation_dyads : If DirectedDyadIsAvailable then
                        begin
                           if ((current_year >= Range_lowyear) and (current_year <= Range_HighYear)) then
                              begin
                                 for x := 0 to High(directeddyaddata) - 1 do
                                    outstring := outstring + FormattedString(directeddyaddata[x], dataFormat1, dataFormat2) + separator;
                                 outstring := outstring + FormattedString(directeddyaddata[high(directeddyaddata)], dataFormat1, dataFormat2);
                              end
                           else
                              begin
                                 for x := 0 to High(directeddyaddata) - 1 do
                                    outstring := outstring + FormattedString(var_missing_value,2,0) + separator;
                                 outstring := outstring + FormattedString(var_missing_value,2,0);
                              end
                        end;
                     output_nondirected_dyads, output_nondirected_dispute_dyads : If NonDirectedDyadIsAvailable then
                        begin
                           if ((current_year >= Range_lowyear) and (current_year <= Range_HighYear)) then
                              begin
                                 for x := 0 to High(nondirecteddyaddata) - 1 do
                                    outstring := outstring + FormattedString(nondirecteddyaddata[x], dataFormat1, dataFormat2) + separator;
                                 outstring := outstring + FormattedString(nondirecteddyaddata[high(nondirecteddyaddata)], dataFormat1, dataFormat2);
                              end
                           else
                              begin
                                 for x := 0 to High(nondirecteddyaddata) - 1 do
                                    outstring := outstring + FormattedString(var_missing_value,2,0) + separator;
                                 outstring := outstring + FormattedString(var_missing_value,2,0);
                              end
                        end;

                  end;    {case output_this of }
              end;   {if avariable in...}

         end;        {proc set var data}


      begin       {main procedure output_one_record}
         try
            temp := 0;
            separator := '';
            case user_selections.output_format.separator of
              tab : separator := chr(9);
              space : separator := ' ';
              comma : separator := ',';
            end;   {case}

            outstring := ' ';     {indent 1 so data doesn't start in 1st column}

            {Sometimes this procedure will be called for monadic data.  However, the internal
             calls still need a ccode1 and ccode2 in this case because of the generic nature
             of the output procedures.  In this case, ccode2 came in with value 0 (as called in
             "output_monadic_data" procedure.}

            {output order of variables in all procedures is ccode(s), year, major(s),
             capability/ies, risk, contiguity, distance, tau, EU war trap, EU war Reason,
             politically relevant.}

         {Again, each call to SetOutputVarData uses essentially a triad of inputs, after the first
          2 identifier variables.  The triads in order are for data for output in monadic format,
          directed dyadic format, then non directed dyadic format.  Each of the triads has
          first a boolean marking if that kind of data is available, then an array of values
          which are the data to be sent to the output file.}

            if ccodes in user_selections.output_format.variables then
            SetOutputVardata (outstring, ccodes,
               true, [ccode1], true, [ccode1, ccode2], true, [ccode1, ccode2],3,0,
               curryear, configuration.first_any_year, configuration.last_any_year, missing_value);

            if year in user_selections.output_format.variables then
            SetOutputVardata (outstring, year,
               true, [curryear], true, [curryear], true, [curryear],4,0,
               curryear, configuration.first_any_year, configuration.last_any_year, missing_value);

            if abbrevs in user_selections.output_format.variables then
            SetOutputVardata (outstring, abbrevs,
               true, [nation_list.get_abbrev(ccode1)],
               true, [nation_list.get_abbrev(ccode1), nation_list.get_abbrev(ccode2)],
               true, [nation_list.get_abbrev(ccode1), nation_list.get_abbrev(ccode2)],3,0,
               curryear, configuration.first_any_year, configuration.last_any_year, missing_value);

            if ISO_code in user_selections.output_format.variables then
            SetOutputVardata (outstring, ISO_code,
              true, [ISO_array.get_isocode(ccode1)],
              true, [ISO_array.get_isocode(ccode1), ISO_array.get_isocode(ccode2)],
              true, [ISO_array.get_isocode(ccode1), ISO_array.get_isocode(ccode2)],3,0, curryear, configuration.first_any_year, configuration.last_any_year, missing_value);

            if ISO_abb2 in user_selections.output_format.variables then
            SetOutputVardata (outstring, ISO_abb2,
              true, [ISO_array.get_abbrev2(ccode1)],
              true, [ISO_array.get_abbrev2(ccode1), ISO_array.get_abbrev2(ccode2)],
              true, [ISO_array.get_abbrev2(ccode1), ISO_array.get_abbrev2(ccode2)],3,0, curryear, configuration.first_any_year, configuration.last_any_year, missing_value);

            if ISO_abb3 in user_selections.output_format.variables then
            SetOutputVardata (outstring, ISO_abb3,
              true, [ISO_array.get_abbrev3(ccode1)],
              true, [ISO_array.get_abbrev3(ccode1), ISO_array.get_abbrev3(ccode2)],
              true, [ISO_array.get_abbrev3(ccode1), ISO_array.get_abbrev3(ccode2)],3,0, curryear, configuration.first_any_year, configuration.last_any_year, missing_value);

            if ISO_short in user_selections.output_format.variables then
            SetOutputVardata (outstring, ISO_short,
              true, [ISO_array.get_short_iso_name(ccode1)],
              true, [ISO_array.get_short_iso_name(ccode1), ISO_array.get_short_iso_name(ccode2)],
              true, [ISO_array.get_short_iso_name(ccode1), ISO_array.get_short_iso_name(ccode2)],40,0, curryear, configuration.first_any_year, configuration.last_any_year, missing_value);

            if ISO_full in user_selections.output_format.variables then
            SetOutputVardata (outstring, ISO_full,
              true, [ISO_array.get_full_iso_name(ccode1)],
              true, [ISO_array.get_full_iso_name(ccode1), ISO_array.get_full_iso_name(ccode2)],
              true, [ISO_array.get_full_iso_name(ccode1), ISO_array.get_full_iso_name(ccode2)],80,0, curryear, configuration.first_any_year, configuration.last_any_year, missing_value);

            if SystemCapabilities in user_selections.output_format.variables then
            SetOutputVardata (outstring, SystemCapabilities,
               true, [sys_capability_data.get_syscap(ccode1, curryear, dont_stop_on_error)],
               true, [sys_capability_data.get_syscap(ccode1, curryear, dont_stop_on_error), sys_capability_data.get_syscap(ccode2, curryear, dont_stop_on_error)],
               true, [sys_capability_data.get_syscap(ccode1, curryear, dont_stop_on_error), sys_capability_data.get_syscap(ccode2, curryear, dont_stop_on_error)],
               8,6, curryear, configuration.first_cap_year, configuration.last_cap_year, missing_value);

            if RawCapabilities in user_selections.output_format.variables then
            SetOutputVardata (outstring, RawCapabilities,
               true, [raw_capability_data.get_milper(ccode1, curryear, dont_stop_on_error),
               		  raw_capability_data.get_milex(ccode1, curryear, dont_stop_on_error),
               		  raw_capability_data.get_energy(ccode1, curryear, dont_stop_on_error),
                      raw_capability_data.get_irst(ccode1, curryear, dont_stop_on_error),
                      raw_capability_data.get_upop(ccode1, curryear, dont_stop_on_error),
                      raw_capability_data.get_tpop(ccode1, curryear, dont_stop_on_error)],
               true, [raw_capability_data.get_milper(ccode1, curryear, dont_stop_on_error),
               		  raw_capability_data.get_milex(ccode1, curryear, dont_stop_on_error),
               		  raw_capability_data.get_energy(ccode1, curryear, dont_stop_on_error),
                      raw_capability_data.get_irst(ccode1, curryear, dont_stop_on_error),
                      raw_capability_data.get_upop(ccode1, curryear, dont_stop_on_error),
                      raw_capability_data.get_tpop(ccode1, curryear, dont_stop_on_error),
                      raw_capability_data.get_milper(ccode2, curryear, dont_stop_on_error),
               		  raw_capability_data.get_milex(ccode2, curryear, dont_stop_on_error),
               		  raw_capability_data.get_energy(ccode2, curryear, dont_stop_on_error),
                      raw_capability_data.get_irst(ccode2, curryear, dont_stop_on_error),
                      raw_capability_data.get_upop(ccode2, curryear, dont_stop_on_error),
                      raw_capability_data.get_tpop(ccode2, curryear, dont_stop_on_error)],
               true, [raw_capability_data.get_milper(ccode1, curryear, dont_stop_on_error),
               		  raw_capability_data.get_milex(ccode1, curryear, dont_stop_on_error),
               		  raw_capability_data.get_energy(ccode1, curryear, dont_stop_on_error),
                      raw_capability_data.get_irst(ccode1, curryear, dont_stop_on_error),
                      raw_capability_data.get_upop(ccode1, curryear, dont_stop_on_error),
                      raw_capability_data.get_tpop(ccode1, curryear, dont_stop_on_error),
                      raw_capability_data.get_milper(ccode2, curryear, dont_stop_on_error),
               		  raw_capability_data.get_milex(ccode2, curryear, dont_stop_on_error),
               		  raw_capability_data.get_energy(ccode2, curryear, dont_stop_on_error),
                      raw_capability_data.get_irst(ccode2, curryear, dont_stop_on_error),
                      raw_capability_data.get_upop(ccode2, curryear, dont_stop_on_error),
                      raw_capability_data.get_tpop(ccode2, curryear, dont_stop_on_error)],
               8,6, curryear, configuration.first_cap_year, configuration.last_cap_year, missing_value);

            if PowerStatus in user_selections.output_format.variables then
            SetOutputVardata (outstring, PowerStatus,
               true, [nation_list.is_a_gp(ccode1, curryear)],
               true, [nation_list.is_a_gp(ccode1, curryear), nation_list.is_a_gp(ccode2, curryear)],
               true, [nation_list.is_a_gp(ccode1, curryear), nation_list.is_a_gp(ccode2, curryear)],
               1,0, curryear, configuration.first_nation_year, configuration.last_nation_year, missing_value);

            if RelRegion in user_selections.output_format.variables then
            SetOutputVardata (outstring, RelRegion,
               false, [0],
               true, [ord(relevant_region (ccode1, ccode2, curryear))],
               true, [ord(relevant_region (ccode1, ccode2, curryear)), ord(relevant_region (ccode2, ccode1, curryear))],
               1,0, curryear, configuration.first_nation_year, configuration.last_nation_year, missing_value);

            if HomeRegion in user_selections.output_format.variables then
            SetOutputVardata (outstring, HomeRegion,
               true, [ord(nation_list.get_home_region (ccode1))],
               true, [ord(nation_list.get_home_region (ccode1)), ord(nation_list.get_home_region (ccode2))],
               true, [ord(nation_list.get_home_region (ccode1)), ord(nation_list.get_home_region (ccode2))],
               1,0, curryear, configuration.first_nation_year, configuration.last_nation_year, missing_value);

            if MarkPolRelevant in user_selections.output_format.variables then
            SetOutputVardata (outstring, MarkPolRelevant,
               false, [0],
               true, [is_Politically_relevant (ccode1, ccode2, curryear)],
               true, [is_Politically_relevant (ccode1, ccode2, curryear)],
               1,0, curryear, configuration.first_nation_year, configuration.last_nation_year, missing_value);

            if tau in user_selections.output_format.variables then
            SetOutputVardata (outstring, tau,
               false, [0],
               true, [tau_data.get_tau_value_regional(ccode1, ccode2, curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error),
                      tau_data.get_tau_value_global(ccode1, ccode2, curryear, dont_stop_on_error)],
               true, [tau_data.get_tau_value_regional(ccode1, ccode2, curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                      tau_data.get_tau_value_regional(ccode1, ccode2, curryear, nation_list.get_home_region (ccode2), dont_stop_on_error),
                      tau_data.get_tau_value_global(ccode1, ccode2, curryear, dont_stop_on_error)],
               8,6, curryear, configuration.first_alliance_year, configuration.last_alliance_year, missing_value);


            if sunweighted in user_selections.output_format.variables then
            SetOutputVardata (outstring, sunweighted,
               false, [0],
               true, [s_data.get_s_value_regional(ccode1, ccode2, curryear, relevant_region(ccode1, ccode2, curryear), unweighted, dont_stop_on_error),
                      s_data.get_s_value_global(ccode1, ccode2, curryear, unweighted, dont_stop_on_error)],
               true, [s_data.get_s_value_regional(ccode1, ccode2, curryear, nation_list.get_home_region (ccode1), unweighted, dont_stop_on_error),
                      s_data.get_s_value_regional(ccode1, ccode2, curryear, nation_list.get_home_region (ccode2), unweighted, dont_stop_on_error),
                      s_data.get_s_value_global(ccode1, ccode2, curryear, unweighted, dont_stop_on_error)],
               8,6, curryear, configuration.first_alliance_year, configuration.last_alliance_year, missing_value);

            if sweighted in user_selections.output_format.variables then
            SetOutputVardata (outstring, sweighted,
               false, [0],
               true, [s_data.get_s_value_regional(ccode1, ccode2, curryear, relevant_region(ccode1, ccode2, curryear), weighted, dont_stop_on_error),
                      s_data.get_s_value_global(ccode1, ccode2, curryear, weighted, dont_stop_on_error)],
               true, [s_data.get_s_value_regional(ccode1, ccode2, curryear, nation_list.get_home_region (ccode1), weighted, dont_stop_on_error),
                      s_data.get_s_value_regional(ccode1, ccode2, curryear, nation_list.get_home_region (ccode2), weighted, dont_stop_on_error),
                      s_data.get_s_value_global(ccode1, ccode2, curryear, weighted, dont_stop_on_error)],
               8,6, curryear, configuration.first_alliance_year, configuration.last_alliance_year, missing_value);


            {For variables using alliance data directly, need to set dates appropriately
             depending on my, or original COW sequenced, data.}
            case user_selections.alliance_data_source of
               flat_dyadic : begin
                     alliance_first_year := configuration.first_alliance_year;
                     alliance_last_year := configuration.last_alliance_year;
                  end;
               {flat_cow_sequence : begin
                     alliance_first_year := configuration.first_alliance_seq_year;
                     alliance_last_year := configuration.last_alliance_seq_year;
                  end;  }
               else EUGeneError ('Tried to set configuration dates for alliances in euinoutd, but alliance_data_source not set to flat_dyadic.  Programming error.  Fatal.',1,stop,error_log);
            end;


            if AlliancePortfolioUnweighted in user_selections.output_format.variables then
            begin
               {first output regional table.}
               if (curryear >= alliance_first_year) and (curryear <= alliance_last_year) then
               alliance_data.build_similarity_table (ccode1, ccode2, curryear,
                  relevant_region(ccode1, ccode2, curryear),
                  unweighted, sys_capability_data, similarity_table, n, dmax);
               for row := defense to no_alliance do
                  for column := defense to no_alliance do
                     SetOutputVardata (outstring, AlliancePortfolioUnweighted,
                        false, [0], true, [similarity_table[row,column]],
                        true, [similarity_table[row,column]],
                        1,0, curryear, alliance_first_year, alliance_last_year, missing_value);
               SetOutputVardata (outstring, AlliancePortfolioUnweighted,
                  false, [0], true, [n], true, [n],
                  4,0, curryear, alliance_first_year, alliance_last_year, missing_value);
               {Now output global table}
               if (curryear >= alliance_first_year) and (curryear <= alliance_last_year) then
               alliance_data.build_similarity_table (ccode1, ccode2, curryear,
                  globe,
                  unweighted, sys_capability_data, similarity_table, n, dmax);
               for row := defense to no_alliance do
                  for column := defense to no_alliance do
                     SetOutputVardata (outstring, AlliancePortfolioUnweighted,
                        false, [0], true, [similarity_table[row,column]],
                        true, [similarity_table[row,column]],
                        1,0, curryear, alliance_first_year, alliance_last_year, missing_value);
               SetOutputVardata (outstring, AlliancePortfolioUnweighted,
                  false, [0], true, [n], true, [n],
                  4,0, curryear, alliance_first_year, alliance_last_year, missing_value);
            end;

            if AlliancePortfolioWeighted in user_selections.output_format.variables then
            {Alliance port weighted dates depend on both capabilities and alliances.}
            begin
               {first output regional table.}
               if (curryear >= alliance_first_year) and (curryear <= alliance_last_year) then
               alliance_data.build_similarity_table (ccode1, ccode2, curryear,
                  relevant_region(ccode1, ccode2, curryear),
                  weighted, sys_capability_data, similarity_table, n, dmax);
               for row := defense to no_alliance do
                  for column := defense to no_alliance do
                     SetOutputVardata (outstring, AlliancePortfolioWeighted,
                        false, [0], true, [similarity_table[row,column]],
                        true, [similarity_table[row,column]],
                        8,6, curryear, max(alliance_first_year, configuration.first_cap_year), min(alliance_last_year,configuration.last_cap_year), missing_value);
               SetOutputVardata (outstring, AlliancePortfolioWeighted,
                  false, [0], true, [dmax], true, [dmax],
                  8,6, curryear, max(alliance_first_year, configuration.first_cap_year), min(alliance_last_year,configuration.last_cap_year), missing_value);
               {Now output global table}
               if (curryear >= alliance_first_year) and (curryear <= alliance_last_year) then
               alliance_data.build_similarity_table (ccode1, ccode2, curryear,
                  globe,
                  weighted, sys_capability_data, similarity_table, n, dmax);
               for row := defense to no_alliance do
                  for column := defense to no_alliance do
                     SetOutputVardata (outstring, AlliancePortfolioWeighted,
                        false, [0], true, [similarity_table[row,column]],
                        true, [similarity_table[row,column]],
                        8,6, curryear, max(alliance_first_year, configuration.first_cap_year), min(alliance_last_year,configuration.last_cap_year), missing_value);
               SetOutputVardata (outstring, AlliancePortfolioWeighted,
                  false, [0], true, [dmax], true, [dmax],
                  8,6, curryear, max(alliance_first_year, configuration.first_cap_year), min(alliance_last_year,configuration.last_cap_year), missing_value);
            end;

            if curryear <= 1945 then leader_ccode:=200 else leader_ccode:=2;
            if tauWithLeader in user_selections.output_format.variables then
            case user_selections.tau_leader_calculation_info of
               regional : begin
                    SetOutputVardata (outstring, tauWithLeader,
                        true, [tau_data.get_tau_value_regional(ccode1, leader_ccode, curryear,relevant_region(ccode1, leader_ccode, curryear), dont_stop_on_error)],
                        true, [tau_data.get_tau_value_regional(ccode1, leader_ccode, curryear, relevant_region(ccode1, leader_ccode, curryear), dont_stop_on_error),
                               tau_data.get_tau_value_regional(ccode2, leader_ccode, curryear, relevant_region(ccode2, leader_ccode, curryear), dont_stop_on_error)],
                        true, [tau_data.get_tau_value_regional(ccode1, leader_ccode, curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                               tau_data.get_tau_value_regional(ccode2, leader_ccode, curryear, nation_list.get_home_region (ccode2), dont_stop_on_error)],
                        8,6, curryear, configuration.first_alliance_year, configuration.last_alliance_year, missing_value);
                  end;
               global : begin
                    SetOutputVardata (outstring, tauWithLeader,
                        true, [tau_data.get_tau_value_global(ccode1, leader_ccode, curryear, dont_stop_on_error)],
                        true, [tau_data.get_tau_value_global(ccode1, leader_ccode, curryear, dont_stop_on_error),
                               tau_data.get_tau_value_global(ccode2, leader_ccode, curryear, dont_stop_on_error)],
                        true, [tau_data.get_tau_value_global(ccode1, leader_ccode, curryear, dont_stop_on_error),
                               tau_data.get_tau_value_global(ccode2, leader_ccode, curryear, dont_stop_on_error)],
                        8,6, curryear, configuration.first_alliance_year, configuration.last_alliance_year, missing_value);
                  end;
               end;   {case}

            if curryear <= 1945 then leader_ccode:=200 else leader_ccode:=2;
            if sWithLeader in user_selections.output_format.variables then
            case user_selections.s_leader_calculation_info of
               regional : begin
                    SetOutputVardata (outstring, sWithLeader,
                        true, [s_data.get_s_value_regional(ccode1, leader_ccode, curryear, relevant_region(ccode1, leader_ccode, curryear), user_selections.s_weighting, dont_stop_on_error)],
                        true, [s_data.get_s_value_regional(ccode1, leader_ccode, curryear, relevant_region(ccode1, leader_ccode, curryear), user_selections.s_weighting, dont_stop_on_error),
                               s_data.get_s_value_regional(ccode2, leader_ccode, curryear, relevant_region(ccode2, leader_ccode, curryear), user_selections.s_weighting, dont_stop_on_error)],
                        true, [s_data.get_s_value_regional(ccode1, leader_ccode, curryear, nation_list.get_home_region (ccode1), user_selections.s_weighting, dont_stop_on_error),
                               s_data.get_s_value_regional(ccode2, leader_ccode, curryear, nation_list.get_home_region (ccode2), user_selections.s_weighting, dont_stop_on_error)],
                        8,6, curryear, configuration.first_alliance_year, configuration.last_alliance_year, missing_value);
                  end;
               global : begin
                    SetOutputVardata (outstring, sWithLeader,
                        true, [s_data.get_s_value_global(ccode1, leader_ccode, curryear, user_selections.s_weighting, dont_stop_on_error)],
                        true, [s_data.get_s_value_global(ccode1, leader_ccode, curryear, user_selections.s_weighting, dont_stop_on_error),
                               s_data.get_s_value_global(ccode2, leader_ccode, curryear, user_selections.s_weighting, dont_stop_on_error)],
                        true, [s_data.get_s_value_global(ccode1, leader_ccode, curryear, user_selections.s_weighting, dont_stop_on_error),
                               s_data.get_s_value_global(ccode2, leader_ccode, curryear, user_selections.s_weighting, dont_stop_on_error)],
                        8,6, curryear, configuration.first_alliance_year, configuration.last_alliance_year, missing_value);
                  end;
               end;   {case}

            if alliance in user_selections.output_format.variables then
            SetOutputVardata (outstring, alliance,
               false, [0],
               true, [alliance_data.get_alliance_value(ccode1, ccode2, curryear, dont_stop_on_error)],
               true, [alliance_data.get_alliance_value(ccode1, ccode2, curryear, dont_stop_on_error)],
               1,0, curryear, alliance_first_year, alliance_last_year, missing_value);


           {All of the polity variables are controlled by a master polity3 variable
            switch.}
           if polity3 in user_selections.output_format.variables then
              begin

                 if democ in user_selections.output_format.variables then
                 SetOutputVardata (outstring, democ,
                     true, [polity3_data.get_democ(ccode1, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_democ(ccode1, curryear, dont_stop_on_error), polity3_data.get_democ(ccode2, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_democ(ccode1, curryear, dont_stop_on_error), polity3_data.get_democ(ccode2, curryear, dont_stop_on_error)],
                     3,0, curryear, configuration.first_polity3_year, configuration.last_polity3_year, missing_value_polity);

                 if autoc in user_selections.output_format.variables then
                 SetOutputVardata (outstring, autoc,
                     true, [polity3_data.get_autoc(ccode1, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_autoc(ccode1, curryear, dont_stop_on_error), polity3_data.get_autoc(ccode2, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_autoc(ccode1, curryear, dont_stop_on_error), polity3_data.get_autoc(ccode2, curryear, dont_stop_on_error)],
                     3,0, curryear, configuration.first_polity3_year, configuration.last_polity3_year, missing_value_polity);

                 if xrreg in user_selections.output_format.variables then
                 SetOutputVardata (outstring, xrreg,
                     true, [polity3_data.get_xrreg(ccode1, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_xrreg(ccode1, curryear, dont_stop_on_error), polity3_data.get_xrreg(ccode2, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_xrreg(ccode1, curryear, dont_stop_on_error), polity3_data.get_xrreg(ccode2, curryear, dont_stop_on_error)],
                     3,0, curryear, configuration.first_polity3_year, configuration.last_polity3_year, missing_value_polity);

                 if xrcomp in user_selections.output_format.variables then
                 SetOutputVardata (outstring, xrcomp,
                     true, [polity3_data.get_xrcomp(ccode1, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_xrcomp(ccode1, curryear, dont_stop_on_error), polity3_data.get_xrcomp(ccode2, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_xrcomp(ccode1, curryear, dont_stop_on_error), polity3_data.get_xrcomp(ccode2, curryear, dont_stop_on_error)],
                     3,0, curryear, configuration.first_polity3_year, configuration.last_polity3_year, missing_value_polity);

                 if xropen in user_selections.output_format.variables then
                 SetOutputVardata (outstring, xropen,
                     true, [polity3_data.get_xropen(ccode1, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_xropen(ccode1, curryear, dont_stop_on_error), polity3_data.get_xropen(ccode2, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_xropen(ccode1, curryear, dont_stop_on_error), polity3_data.get_xropen(ccode2, curryear, dont_stop_on_error)],
                     3,0, curryear, configuration.first_polity3_year, configuration.last_polity3_year, missing_value_polity);

                 if mono in user_selections.output_format.variables then
                 SetOutputVardata (outstring, mono,
                     true, [polity3_data.get_mono(ccode1, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_mono(ccode1, curryear, dont_stop_on_error), polity3_data.get_mono(ccode2, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_mono(ccode1, curryear, dont_stop_on_error), polity3_data.get_mono(ccode2, curryear, dont_stop_on_error)],
                     3,0, curryear, configuration.first_polity3_year, configuration.last_polity3_year, missing_value_polity);

                 if xconst in user_selections.output_format.variables then
                 SetOutputVardata (outstring, xconst,
                     true, [polity3_data.get_xconst(ccode1, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_xconst(ccode1, curryear, dont_stop_on_error), polity3_data.get_xconst(ccode2, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_xconst(ccode1, curryear, dont_stop_on_error), polity3_data.get_xconst(ccode2, curryear, dont_stop_on_error)],
                     3,0, curryear, configuration.first_polity3_year, configuration.last_polity3_year, missing_value_polity);

                 if parreg in user_selections.output_format.variables then
                 SetOutputVardata (outstring, parreg,
                     true, [polity3_data.get_parreg(ccode1, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_parreg(ccode1, curryear, dont_stop_on_error), polity3_data.get_parreg(ccode2, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_parreg(ccode1, curryear, dont_stop_on_error), polity3_data.get_parreg(ccode2, curryear, dont_stop_on_error)],
                     3,0, curryear, configuration.first_polity3_year, configuration.last_polity3_year, missing_value_polity);

                 if parcomp in user_selections.output_format.variables then
                 SetOutputVardata (outstring, parcomp,
                     true, [polity3_data.get_parcomp(ccode1, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_parcomp(ccode1, curryear, dont_stop_on_error), polity3_data.get_parcomp(ccode2, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_parcomp(ccode1, curryear, dont_stop_on_error), polity3_data.get_parcomp(ccode2, curryear, dont_stop_on_error)],
                     3,0, curryear, configuration.first_polity3_year, configuration.last_polity3_year, missing_value_polity);

                 if cent in user_selections.output_format.variables then
                 SetOutputVardata (outstring, cent,
                     true, [polity3_data.get_cent(ccode1, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_cent(ccode1, curryear, dont_stop_on_error), polity3_data.get_cent(ccode2, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_cent(ccode1, curryear, dont_stop_on_error), polity3_data.get_cent(ccode2, curryear, dont_stop_on_error)],
                     3,0, curryear, configuration.first_polity3_year, configuration.last_polity3_year, missing_value_polity);

                 if dem in user_selections.output_format.variables then
                 SetOutputVardata (outstring, dem,
                     true, [polity3_data.get_dem(ccode1, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_dem(ccode1, curryear, dont_stop_on_error), polity3_data.get_dem(ccode2, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_dem(ccode1, curryear, dont_stop_on_error), polity3_data.get_dem(ccode2, curryear, dont_stop_on_error)],
                     3,0, curryear, configuration.first_polity3_year, configuration.last_polity3_year, missing_value_polity);

                 if laggeddem in user_selections.output_format.variables then
                 SetOutputVardata (outstring, laggeddem,
                     true, [polity3_data.get_democlg(ccode1, curryear, dont_stop_on_error), polity3_data.get_autoclg(ccode1, curryear, dont_stop_on_error), polity3_data.get_demlg(ccode1, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_democlg(ccode1, curryear, dont_stop_on_error), polity3_data.get_democlg(ccode2, curryear, dont_stop_on_error),
                            polity3_data.get_autoclg(ccode1, curryear, dont_stop_on_error), polity3_data.get_autoclg(ccode2, curryear, dont_stop_on_error),
                            polity3_data.get_demlg(ccode1, curryear, dont_stop_on_error), polity3_data.get_demlg(ccode2, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_democlg(ccode1, curryear, dont_stop_on_error), polity3_data.get_democlg(ccode2, curryear, dont_stop_on_error),
                            polity3_data.get_autoclg(ccode1, curryear, dont_stop_on_error), polity3_data.get_autoclg(ccode2, curryear, dont_stop_on_error),
                            polity3_data.get_demlg(ccode1, curryear, dont_stop_on_error), polity3_data.get_demlg(ccode2, curryear, dont_stop_on_error)],
                     3,0, curryear, configuration.first_polity3_year, configuration.last_polity3_year, missing_value_polity);

                 if democratization in user_selections.output_format.variables then
                 SetOutputVardata (outstring, democratization,
                     true, [polity3_data.get_demchg(ccode1, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_demchg(ccode1, curryear, dont_stop_on_error), polity3_data.get_demchg(ccode2, curryear, dont_stop_on_error)],
                     true, [polity3_data.get_demchg(ccode1, curryear, dont_stop_on_error), polity3_data.get_demchg(ccode2, curryear, dont_stop_on_error)],
                     3,0, curryear, configuration.first_polity3_year, configuration.last_polity3_year, missing_value_polity);
              end;           {if polity3 included}


              {Now some items for output that are only printed if user is printing dyads}
              {Note:  the get procedures for these data do not need to take account of having 2
               valid ccodes, because they are only called for valid dyads in the first place.
               But they do need checks for year.}

            if contig in user_selections.output_format.variables then
            SetOutputVardata (outstring, contig,
               false, [0],
               true, [contiguity_data.get_direct_contiguity_level(ccode1, ccode2, curryear)],
               true, [contiguity_data.get_direct_contiguity_level(ccode1, ccode2, curryear)],
               1,0, curryear, configuration.first_contiguity_year, configuration.last_contiguity_year, missing_value);


            if ColonialContig in user_selections.output_format.variables then
            begin
               level := contiguity_data.get_colonial_contiguity_level(ccode1, ccode2, curryear, colony_code1, colony_code2);
               SetOutputVardata (outstring, ColonialContig,
                  false, [0],
                  true, [level, colony_code1, colony_code2],
                  true, [level, colony_code1, colony_code2],
                  1,0, curryear, configuration.first_contiguity_year, configuration.last_contiguity_year, missing_value);
            end;

            if distance in user_selections.output_format.variables then
            if user_selections.distance_method = minimum then
               SetOutputVardata (outstring, distance,
                  false, [0],
                  true, [minimum_distance_data.get_distance(ccode1, ccode2, curryear)],
                  true, [minimum_distance_data.get_distance(ccode1, ccode2, curryear)],
                  10,0, curryear, configuration.first_nation_year, configuration.last_nation_year, missing_value)
            else {a different method, in other structure}
               SetOutputVardata (outstring, distance,
                  false, [0],
                  true, [distance_data.get_distance(ccode1, ccode2, curryear)],
                  true, [distance_data.get_distance(ccode1, ccode2, curryear)],
                  10,0, curryear, configuration.first_nation_year, configuration.last_nation_year, missing_value);


            if DyadicTime in user_selections.output_format.variables then
            SetOutputVardata (outstring, DyadicTime,
               false, [0],
               true, [nation_list.get_dyadic_duration(ccode1, ccode2, curryear)],
               true, [nation_list.get_dyadic_duration(ccode1, ccode2, curryear)],
               1,0, curryear, configuration.first_nation_year, configuration.last_nation_year, missing_value);


            if systemchars in user_selections.output_format.variables then
               begin
                  if StatesInSystem in user_selections.output_format.variables then
                  SetOutputVardata (outstring, StatesInSystem,
                     true, [system_variables.get_states(curryear)],
                     true, [system_variables.get_states(curryear)],
                     true, [system_variables.get_states(curryear)],
                     4,0, curryear, configuration.first_nation_year, configuration.last_nation_year, missing_value);
                  if GPsInSystem in user_selections.output_format.variables then
                  SetOutputVardata (outstring, GPsInSystem,
                     true, [system_variables.get_GPs(curryear)],
                     true, [system_variables.get_GPs(curryear)],
                     true, [system_variables.get_GPs(curryear)],
                     2,0, curryear, configuration.first_nation_year, configuration.last_nation_year, missing_value);
                  if SysConcentration in user_selections.output_format.variables then
                  SetOutputVardata (outstring, SysConcentration,
                     true, [system_variables.get_con(curryear)],
                     true, [system_variables.get_con(curryear)],
                     true, [system_variables.get_con(curryear)],
                     8,6, curryear, configuration.first_nation_year, configuration.last_nation_year, missing_value);
                  if SysMovement in user_selections.output_format.variables then
                  SetOutputVardata (outstring, SysMovement,
                     true, [system_variables.get_move(curryear)],
                     true, [system_variables.get_move(curryear)],
                     true, [system_variables.get_move(curryear)],
                     8,6, curryear, configuration.first_nation_year, configuration.last_nation_year, missing_value);
                  if SysMovement5Yr in user_selections.output_format.variables then
                  SetOutputVardata (outstring, SysMovement5Yr,
                     true, [system_variables.get_move5(curryear)],
                     true, [system_variables.get_move5(curryear)],
                     true, [system_variables.get_move5(curryear)],
                     8,6, curryear, configuration.first_nation_year, configuration.last_nation_year, missing_value);
                  if SysMoveGP in user_selections.output_format.variables then
                  SetOutputVardata (outstring, SysMoveGP,
                     true, [system_variables.get_moveGP(curryear)],
                     true, [system_variables.get_moveGP(curryear)],
                     true, [system_variables.get_moveGP(curryear)],
                     8,6, curryear, configuration.first_nation_year, configuration.last_nation_year, missing_value);
                  if SysMoveGP5Yr in user_selections.output_format.variables then
                  SetOutputVardata (outstring, SysMoveGP5Yr,
                     true, [system_variables.get_moveGP5(curryear)],
                     true, [system_variables.get_moveGP5(curryear)],
                     true, [system_variables.get_moveGP5(curryear)],
                     8,6, curryear, configuration.first_nation_year, configuration.last_nation_year, missing_value);
            end;


            if EUWarTrapTau in user_selections.output_format.variables then
            SetOutputVardata (outstring, EUWarTrapTau,
               false, [0],
               true, [EUWarTrap_Tau_array.get_EUWarTrap(ccode1, ccode2, curryear, dont_stop_on_error)],
               true, [EUWarTrap_Tau_array.get_EUWarTrap(ccode1, ccode2, curryear, dont_stop_on_error), EUWarTrap_Tau_array.get_EUWarTrap(ccode2, ccode1, curryear, dont_stop_on_error)],
               8,6, curryear, configuration.first_nation_year, configuration.last_nation_year, missing_value);

            if EUWarTrapS in user_selections.output_format.variables then
            SetOutputVardata (outstring, EUWarTrapS,
               false, [0],
               true, [EUWarTrap_S_array.get_EUWarTrap(ccode1, ccode2, curryear, dont_stop_on_error)],
               true, [EUWarTrap_S_array.get_EUWarTrap(ccode1, ccode2, curryear, dont_stop_on_error), EUWarTrap_S_array.get_EUWarTrap(ccode2, ccode1, curryear, dont_stop_on_error)],
               8,6, curryear, configuration.first_nation_year, configuration.last_nation_year, missing_value);


           {need to set some years for risk and uncertainty}
           if ((UncertaintyTau in user_selections.output_format.variables) or
               (riskTau in user_selections.output_format.variables) or
               (riskdetailsTau in user_selections.output_format.variables) or
               (riskS in user_selections.output_format.variables) or
               (uncertaintyS in user_selections.output_format.variables) or
               (riskdetailsS in user_selections.output_format.variables) ) then
           begin
              if user_selections.risk_data_source = risk_EUGENE then
                 begin
                    temp_first_year := configuration.first_risk_year;
                    temp_last_year := configuration.last_risk_year;
                 end
              else if user_selections.risk_data_source = risk_WTR then
                 begin
                    temp_first_year := configuration.first_wtr_risk_year;
                    temp_last_year := configuration.last_wtr_risk_year;
                 end
              else
                 begin
                    EUGeneError ('uncertainty chosen but risk source not defined in euinoutd; some data may be missing.',1,continue,error_log);
                    temp_first_year := configuration.first_risk_year;
                    temp_last_year := configuration.last_risk_year;
                 end;
           end;

            if riskTau in user_selections.output_format.variables then
             {monads get each region listed.  directed dyads get 2 risk vals, nondir get 4.}
            SetOutputVardata (outstring, riskTau,
               true, [risk_Tau_data.get_risk(ccode1, curryear, europe, dont_stop_on_error),risk_Tau_data.get_risk(ccode1, curryear, middleeast, dont_stop_on_error),
                      risk_Tau_data.get_risk(ccode1, curryear, africa, dont_stop_on_error),risk_Tau_data.get_risk(ccode1, curryear, asia, dont_stop_on_error),
                      risk_Tau_data.get_risk(ccode1, curryear, americas, dont_stop_on_error),risk_Tau_data.get_risk(ccode1, curryear, globe, dont_stop_on_error)],
               true, [risk_Tau_data.get_risk(ccode1, curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error),
                      risk_Tau_data.get_risk(ccode2, curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error)],
               true, [risk_Tau_data.get_risk(ccode1, curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                      risk_Tau_data.get_risk(ccode1, curryear, nation_list.get_home_region (ccode2), dont_stop_on_error),
                      risk_Tau_data.get_risk(ccode2, curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                      risk_Tau_data.get_risk(ccode2, curryear, nation_list.get_home_region (ccode2), dont_stop_on_error)],
               8,6, curryear, temp_first_year, temp_last_year, missing_value);

            if riskS in user_selections.output_format.variables then
             {monads get each region listed.  directed dyads get 2 risk vals, nondir get 4.}
            SetOutputVardata (outstring, riskS,
               true, [risk_S_data.get_risk(ccode1, curryear, europe, dont_stop_on_error),risk_S_data.get_risk(ccode1, curryear, middleeast, dont_stop_on_error),
                      risk_S_data.get_risk(ccode1, curryear, africa, dont_stop_on_error),risk_S_data.get_risk(ccode1, curryear, asia, dont_stop_on_error),
                      risk_S_data.get_risk(ccode1, curryear, americas, dont_stop_on_error),risk_S_data.get_risk(ccode1, curryear, globe, dont_stop_on_error)],
               true, [risk_S_data.get_risk(ccode1, curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error),
                      risk_S_data.get_risk(ccode2, curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error)],
               true, [risk_S_data.get_risk(ccode1, curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                      risk_S_data.get_risk(ccode1, curryear, nation_list.get_home_region (ccode2), dont_stop_on_error),
                      risk_S_data.get_risk(ccode2, curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                      risk_S_data.get_risk(ccode2, curryear, nation_list.get_home_region (ccode2), dont_stop_on_error)],
               8,6, curryear, temp_first_year, temp_last_year, missing_value);


           if riskdetailsTau in user_selections.output_format.variables then
           SetOutputVardata (outstring, riskdetailsTau,
               true, [risk_Tau_data.get_security(ccode1, curryear, europe, dont_stop_on_error), risk_Tau_data.get_secmax(ccode1, curryear, europe, dont_stop_on_error),risk_Tau_data.get_secmin(ccode1, curryear, europe, dont_stop_on_error),
                      risk_Tau_data.get_security(ccode1, curryear, middleeast, dont_stop_on_error), risk_Tau_data.get_secmax(ccode1, curryear, middleeast, dont_stop_on_error),risk_Tau_data.get_secmin(ccode1, curryear, middleeast, dont_stop_on_error),
                      risk_Tau_data.get_security(ccode1, curryear, africa, dont_stop_on_error),risk_Tau_data.get_secmax(ccode1, curryear, africa, dont_stop_on_error),risk_Tau_data.get_secmin(ccode1, curryear, africa, dont_stop_on_error),
                      risk_Tau_data.get_security(ccode1, curryear, asia, dont_stop_on_error),risk_Tau_data.get_secmax(ccode1, curryear, asia, dont_stop_on_error),risk_Tau_data.get_secmin(ccode1, curryear, asia, dont_stop_on_error),
                      risk_Tau_data.get_security(ccode1, curryear, americas, dont_stop_on_error),risk_Tau_data.get_secmax(ccode1, curryear, americas, dont_stop_on_error),risk_Tau_data.get_secmin(ccode1, curryear, americas, dont_stop_on_error),
                      risk_Tau_data.get_security(ccode1, curryear, globe, dont_stop_on_error),risk_Tau_data.get_secmax(ccode1, curryear, globe, dont_stop_on_error),risk_Tau_data.get_secmin(ccode1, curryear, globe, dont_stop_on_error)],
               true, [risk_Tau_data.get_security(ccode1, curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error),
                      risk_Tau_data.get_secmax(ccode1, curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error),
                      risk_Tau_data.get_secmin(ccode1, curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error),
                      risk_Tau_data.get_security(ccode2, curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error),
                      risk_Tau_data.get_secmax(ccode2, curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error),
                      risk_Tau_data.get_secmin(ccode2, curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error)],
               true, [risk_Tau_data.get_security(ccode1, curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                      risk_Tau_data.get_secmax(ccode1, curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                      risk_Tau_data.get_secmin(ccode1, curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                      risk_Tau_data.get_security(ccode1, curryear, nation_list.get_home_region (ccode2), dont_stop_on_error),
                      risk_Tau_data.get_secmax(ccode1, curryear, nation_list.get_home_region (ccode2), dont_stop_on_error),
                      risk_Tau_data.get_secmin(ccode1, curryear, nation_list.get_home_region (ccode2), dont_stop_on_error),
                      risk_Tau_data.get_security(ccode2, curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                      risk_Tau_data.get_secmax(ccode2, curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                      risk_Tau_data.get_secmin(ccode2, curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                      risk_Tau_data.get_security(ccode2, curryear, nation_list.get_home_region (ccode2), dont_stop_on_error),
                      risk_Tau_data.get_secmax(ccode2, curryear, nation_list.get_home_region (ccode2), dont_stop_on_error),
                      risk_Tau_data.get_secmin(ccode2, curryear, nation_list.get_home_region (ccode2), dont_stop_on_error)],
               8,6, curryear, temp_first_year, temp_last_year, missing_value);

           if riskdetailsS in user_selections.output_format.variables then
           SetOutputVardata (outstring, riskdetailsS,
               true, [risk_S_data.get_security(ccode1, curryear, europe, dont_stop_on_error), risk_S_data.get_secmax(ccode1, curryear, europe, dont_stop_on_error),risk_S_data.get_secmin(ccode1, curryear, europe, dont_stop_on_error),
                      risk_S_data.get_security(ccode1, curryear, middleeast, dont_stop_on_error), risk_S_data.get_secmax(ccode1, curryear, middleeast, dont_stop_on_error),risk_S_data.get_secmin(ccode1, curryear, middleeast, dont_stop_on_error),
                      risk_S_data.get_security(ccode1, curryear, africa, dont_stop_on_error),risk_S_data.get_secmax(ccode1, curryear, africa, dont_stop_on_error),risk_S_data.get_secmin(ccode1, curryear, africa, dont_stop_on_error),
                      risk_S_data.get_security(ccode1, curryear, asia, dont_stop_on_error),risk_S_data.get_secmax(ccode1, curryear, asia, dont_stop_on_error),risk_S_data.get_secmin(ccode1, curryear, asia, dont_stop_on_error),
                      risk_S_data.get_security(ccode1, curryear, americas, dont_stop_on_error),risk_S_data.get_secmax(ccode1, curryear, americas, dont_stop_on_error),risk_S_data.get_secmin(ccode1, curryear, americas, dont_stop_on_error),
                      risk_S_data.get_security(ccode1, curryear, globe, dont_stop_on_error),risk_S_data.get_secmax(ccode1, curryear, globe, dont_stop_on_error),risk_S_data.get_secmin(ccode1, curryear, globe, dont_stop_on_error)],
               true, [risk_S_data.get_security(ccode1, curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error),
                      risk_S_data.get_secmax(ccode1, curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error),
                      risk_S_data.get_secmin(ccode1, curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error),
                      risk_S_data.get_security(ccode2, curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error),
                      risk_S_data.get_secmax(ccode2, curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error),
                      risk_S_data.get_secmin(ccode2, curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error)],
               true, [risk_S_data.get_security(ccode1, curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                      risk_S_data.get_secmax(ccode1, curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                      risk_S_data.get_secmin(ccode1, curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                      risk_S_data.get_security(ccode1, curryear, nation_list.get_home_region (ccode2), dont_stop_on_error),
                      risk_S_data.get_secmax(ccode1, curryear, nation_list.get_home_region (ccode2), dont_stop_on_error),
                      risk_S_data.get_secmin(ccode1, curryear, nation_list.get_home_region (ccode2), dont_stop_on_error),
                      risk_S_data.get_security(ccode2, curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                      risk_S_data.get_secmax(ccode2, curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                      risk_S_data.get_secmin(ccode2, curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                      risk_S_data.get_security(ccode2, curryear, nation_list.get_home_region (ccode2), dont_stop_on_error),
                      risk_S_data.get_secmax(ccode2, curryear, nation_list.get_home_region (ccode2), dont_stop_on_error),
                      risk_S_data.get_secmin(ccode2, curryear, nation_list.get_home_region (ccode2), dont_stop_on_error)],
               8,6, curryear, temp_first_year, temp_last_year, missing_value);

           if UncertaintyTau in user_selections.output_format.variables then
              SetOutputVardata (outstring, UncertaintyTau,
                  true, [risk_Tau_data.get_uncertainty(curryear, nation_list.get_home_region (ccode1), dont_stop_on_error)],
                  true, [risk_Tau_data.get_uncertainty(curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error)],
                  true, [risk_Tau_data.get_uncertainty(curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                         risk_Tau_data.get_uncertainty(curryear, nation_list.get_home_region (ccode2), dont_stop_on_error)],
                  7,4, curryear, temp_first_year, temp_last_year, missing_value);

           if UncertaintyS in user_selections.output_format.variables then
              SetOutputVardata (outstring, UncertaintyS,
                  true, [risk_S_data.get_uncertainty(curryear, nation_list.get_home_region (ccode1), dont_stop_on_error)],
                  true, [risk_S_data.get_uncertainty(curryear, relevant_region(ccode1, ccode2, curryear), dont_stop_on_error)],
                  true, [risk_S_data.get_uncertainty(curryear, nation_list.get_home_region (ccode1), dont_stop_on_error),
                         risk_S_data.get_uncertainty(curryear, nation_list.get_home_region (ccode2), dont_stop_on_error)],
                  7,4, curryear, temp_first_year, temp_last_year, missing_value);


            if EUWarReasonTau in user_selections.output_format.variables then
            SetOutputVardata (outstring, EUWarReasonTau,
               false, [0],
               true, [EUWarReason_Tau_array.get_UtilityAA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UtilityAB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UtilityASQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_ProbWinAB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_StakesA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UiSQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UiAcqi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UiAcqj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UiNego(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UiCapi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UiCapj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UiWari(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UiWarj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UtilityBB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UtilityBA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UtilityBSQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_ProbWinBA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_StakesB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UjSQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UjAcqj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UjAcqi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UjNego(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UjCapj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UjCapi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UjWarj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UjWari(ccode1, ccode2, curryear, user_selections, dont_stop_on_error) ],
               true, [EUWarReason_Tau_array.get_UtilityAA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UtilityAB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UtilityASQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_ProbWinAB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_StakesA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UiSQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UiAcqi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UiAcqj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UiNego(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UiCapi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UiCapj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UiWari(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UiWarj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UtilityBB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UtilityBA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UtilityBSQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_ProbWinBA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_StakesB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UjSQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UjAcqj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UjAcqi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UjNego(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UjCapj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UjCapi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UjWarj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_UjWari(ccode1, ccode2, curryear, user_selections, dont_stop_on_error) ],
               8,6, curryear, configuration.first_EU_year_possible, configuration.last_EU_year_possible, missing_value);

            if EUWarReasonSUnweighted in user_selections.output_format.variables then
            SetOutputVardata (outstring, EUWarReasonSUnweighted,
               false, [0],
               true, [EUWarReason_S_unweighted_array.get_UtilityAA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UtilityAB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UtilityASQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_ProbWinAB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_StakesA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UiSQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UiAcqi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UiAcqj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UiNego(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UiCapi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UiCapj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UiWari(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UiWarj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UtilityBB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UtilityBA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UtilityBSQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_ProbWinBA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_StakesB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UjSQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UjAcqj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UjAcqi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UjNego(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UjCapj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UjCapi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UjWarj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UjWari(ccode1, ccode2, curryear, user_selections, dont_stop_on_error) ],
               true, [EUWarReason_S_unweighted_array.get_UtilityAA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UtilityAB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UtilityASQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_ProbWinAB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_StakesA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UiSQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UiAcqi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UiAcqj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UiNego(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UiCapi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UiCapj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UiWari(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UiWarj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UtilityBB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UtilityBA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UtilityBSQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_ProbWinBA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_StakesB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UjSQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UjAcqj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UjAcqi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UjNego(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UjCapj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UjCapi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UjWarj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_UjWari(ccode1, ccode2, curryear, user_selections, dont_stop_on_error) ],
               8,6, curryear, configuration.first_EU_year_possible, configuration.last_EU_year_possible, missing_value);

            if EUWarReasonSweighted in user_selections.output_format.variables then
            SetOutputVardata (outstring, EUWarReasonSweighted,
               false, [0],
               true, [EUWarReason_S_weighted_array.get_UtilityAA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UtilityAB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UtilityASQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_ProbWinAB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_StakesA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UiSQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UiAcqi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UiAcqj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UiNego(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UiCapi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UiCapj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UiWari(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UiWarj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UtilityBB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UtilityBA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UtilityBSQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_ProbWinBA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_StakesB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UjSQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UjAcqj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UjAcqi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UjNego(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UjCapj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UjCapi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UjWarj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UjWari(ccode1, ccode2, curryear, user_selections, dont_stop_on_error) ],
               true, [EUWarReason_S_weighted_array.get_UtilityAA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UtilityAB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UtilityASQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_ProbWinAB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_StakesA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UiSQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UiAcqi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UiAcqj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UiNego(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UiCapi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UiCapj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UiWari(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UiWarj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UtilityBB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UtilityBA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UtilityBSQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_ProbWinBA(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_StakesB(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UjSQ(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UjAcqj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UjAcqi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UjNego(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UjCapj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UjCapi(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UjWarj(ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_UjWari(ccode1, ccode2, curryear, user_selections, dont_stop_on_error) ],
               8,6, curryear, configuration.first_EU_year_possible, configuration.last_EU_year_possible, missing_value);

            if EQWarReasonTau in user_selections.output_format.variables then
            SetOutputVardata (outstring, EQWarReasonTau,
               false, [0],
               true, [EUWarReason_Tau_array.get_EQ(SQ, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(Nego, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(AcqA, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(AcqB, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(CapA, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(CapB, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(WarA, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(WarB, ccode1, ccode2, curryear, user_selections, dont_stop_on_error)],
               true, [EUWarReason_Tau_array.get_EQ(SQ, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(Nego, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(AcqA, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(AcqB, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(CapA, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(CapB, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(WarA, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(WarB, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                      EUWarReason_Tau_array.get_EQ(SQ, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(Nego, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(AcqA, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(AcqB, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(CapA, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(CapB, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(WarA, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_Tau_array.get_EQ(WarB, ccode2, ccode1, curryear, user_selections, dont_stop_on_error)],
               1,0, curryear, configuration.first_EU_year_possible, configuration.last_EU_year_possible, missing_value);

            if EQWarReasonSUnweighted in user_selections.output_format.variables then
            SetOutputVardata (outstring, EQWarReasonSUnweighted,
               false, [0],
               true, [EUWarReason_S_unweighted_array.get_EQ(SQ, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(Nego, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(AcqA, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(AcqB, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(CapA, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(CapB, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(WarA, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(WarB, ccode1, ccode2, curryear, user_selections, dont_stop_on_error)],
               true, [EUWarReason_S_unweighted_array.get_EQ(SQ, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(Nego, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(AcqA, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(AcqB, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(CapA, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(CapB, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(WarA, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(WarB, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                      EUWarReason_S_unweighted_array.get_EQ(SQ, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(Nego, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(AcqA, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(AcqB, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(CapA, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(CapB, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(WarA, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_unweighted_array.get_EQ(WarB, ccode2, ccode1, curryear, user_selections, dont_stop_on_error)],
               1,0, curryear, configuration.first_EU_year_possible, configuration.last_EU_year_possible, missing_value);

            if EQWarReasonSweighted in user_selections.output_format.variables then
            SetOutputVardata (outstring, EQWarReasonSweighted,
               false, [0],
               true, [EUWarReason_S_weighted_array.get_EQ(SQ, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(Nego, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(AcqA, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(AcqB, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(CapA, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(CapB, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(WarA, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(WarB, ccode1, ccode2, curryear, user_selections, dont_stop_on_error)],
               true, [EUWarReason_S_weighted_array.get_EQ(SQ, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(Nego, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(AcqA, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(AcqB, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(CapA, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(CapB, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(WarA, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(WarB, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(SQ, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(Nego, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(AcqA, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(AcqB, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(CapA, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(CapB, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(WarA, ccode2, ccode1, curryear, user_selections, dont_stop_on_error),
                        EUWarReason_S_weighted_array.get_EQ(WarB, ccode2, ccode1, curryear, user_selections, dont_stop_on_error)],
               1,0, curryear, configuration.first_EU_year_possible, configuration.last_EU_year_possible, missing_value);



            if (COW_disputes in user_selections.output_format.variables) then
            {NOTE:  This section is repeated below for version with Maoz data}
               begin
                  ongoing_disp_num  := null_dispute_number;
                  initiation_disp_num := null_dispute_number;
                  joined_disp_num := null_dispute_number;
                  specific_sub_dispute_num := null_dispute_number;
                  found_cow_dispute_num := null_dispute_number;
                  found_ongoing_num := null_dispute_number;
                  ongoing_disp_year_num := null_dispute_number;
                  initiation_disp_year_num := null_dispute_number;
                  joined_disp_year_num := null_dispute_number;
                  specific_sub_dispute_year_num := null_dispute_number;
                  found_cow_dispute_year_num := null_dispute_number;
                  crisis_num := null_dispute_number;
                  found_crisis_num := null_dispute_number;

                     {First output whether there was an ongoing dispute at the
                      beginning of the year or not.  Always output this.  This will never
                      be checked by dispute #, it is always by ccode year.}
                     {Ongoing_disp_num will have in it the # of the dispute that's ongoing.}
                  SetOutputVardata (outstring, COW_disputes,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.is_ongoing(ccode1, ccode2, curryear, ongoing_disp_num)],
                     true, [COW_dyadic_dispute_data_30.is_ongoing(ccode1, ccode2, curryear, ongoing_disp_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  SetOutputVardata (outstring, COW_disputes,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_MID_num(ongoing_disp_num)],
                     true, [COW_dyadic_dispute_data_30.get_MID_num(ongoing_disp_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  {Now generate and output initiation in its appropriate form.  Always output this.
                   Outputs dummy for initiation, also # of MID initiated.}
                  {Initiation could be one
                   of several things, depending on the combo of chosen output options.
                   Combos involve Originators, SideA vs. Revision, and mark subsequent years.
                   Figure it out first, using main proc from dyadic_dispute type.  The call with
                   use_param parameter will ensure getting either the ccode-year or the disp# form.}
                   {note the new-initiation routine checks for revision/sideA coding.}

                 {This will (or may) return a dispute number in the initiation_disp_num variable, if
                  called with use_ccode_year option.  But since it might be called either for ccode-year
                  or with a specific input dispute, and because I want to keep the returned initiation
                  dispute variable # if called with ccode-year, I want to set the initiation# variable
                  to the value of the input dispute # before making the call.  then, if it's called
                  with use_ccodeyear, this will be replaced.  If called with use_dispute, it won't be.}
                 initiation_disp_num := input_disp_num_for_cow;

                 {** Note:  if this code changes, be sure to update the "sampled" function,
                   which uses these commands to check on whether there is a dispute for
                   stratified sampling purposes.}
                  case user_selections.output_this of
                        output_monads: init := false;   {this will not be output}
                        output_directed_dyads, output_directed_dispute_initiation_dyads :
                           case user_selections.dispute_info.MarkSubsequentAsInitiation of
                                 true: init := COW_dyadic_dispute_data_30.wanted_new_or_continuing_initiation (ccode1, ccode2, curryear, user_selections, initiation_disp_num, use_param);
                                 false: init := COW_dyadic_dispute_data_30.wanted_new_initiation (ccode1, ccode2, curryear, user_selections, initiation_disp_num, use_param);
                              end;
                        output_nondirected_dyads, output_nondirected_dispute_dyads :
                           case user_selections.dispute_info.MarkSubsequentAsInitiation of
                                 true: init := COW_dyadic_dispute_data_30.wanted_new_or_Continuing_NonDir_dispute (ccode1, ccode2, curryear, user_selections, initiation_disp_num, use_param);
                                 false: init := COW_dyadic_dispute_data_30.wanted_new_nondir_dispute (ccode1, ccode2, curryear, user_selections, initiation_disp_num, use_param);
                              end;
                  end;
                  {output initiation dummy and mid numbers}
                  SetOutputVardata (outstring, COW_disputes,
                     false, [0],
                     true, [init, COW_dyadic_dispute_data_30.get_MID_num(initiation_disp_num), COW_dyadic_dispute_data_30.get_dyadic_MID_num(initiation_disp_num)],
                     true, [init, COW_dyadic_dispute_data_30.get_MID_num(initiation_disp_num), COW_dyadic_dispute_data_30.get_dyadic_MID_num(initiation_disp_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);


                  {Now check other dispute info.}
                  {At this point, I can identify the appropriate MID # for this dyad if it was
                   called with ccodeyear. Then, most of everything else will be called with that MIDnum.}
                  found_cow_dispute_num := COW_dyadic_dispute_data_30.relevant_midnum(ccode1, ccode2, curryear, user_selections);

                  {The rest of the procedures will use either the specific dispute number passed
                   in above, if called with Use_dispute, or will find a new dispute number if called
                   with use_ccodeyear.

                   I have the variables input_disp_num, and found_cow_dispute_num.

                   Most calls below can now use the use_dispute call, and one of these variables.
                   Put the correct value for the specific dispute # into a generic holder.
                   Note that this is the internal dyadic dispute number, not the COW dispute number.  }
                  case use_param of
                     use_dispute : specific_sub_dispute_num := input_disp_num_for_cow;
                     use_ccodeyear : specific_sub_dispute_num := found_cow_dispute_num;
                     else EUGeneError ('error in euinout outputting disputes - value of use_param not recognized.  notify programmer.',1,stop,error_log);
                  end;      {case}


                  {Next is hostility level.  Always output this.
                     In the old version, the hostility level call also identified
                     the dispute number of the appropriate dispute for all the other dispute
                     information, if it is called using the use_ccode_year option.
                     This is now changed, since I've identified the appropriate MID # above.

                   {output hostility level.  In the revision of 8/2003, since I now know the
                     MID # I want to output data on, just call the hostlev routine directly and
                     bypass the relevant_hostlev func I used before, b/c it's no longer needed.}
                  SetOutputVardata (outstring, COW_disputes,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_hostlev_state(specific_sub_dispute_num, ccode1), COW_dyadic_dispute_data_30.get_hostlev_state(specific_sub_dispute_num, ccode2)],
                     true, [COW_dyadic_dispute_data_30.get_hostlev_state(specific_sub_dispute_num, ccode1), COW_dyadic_dispute_data_30.get_hostlev_state(specific_sub_dispute_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  {if they want it, host lev in dispute here so it makes sense next to individual states.}
                  if COWMIDHostLevDispute in user_selections.output_format.variables then
                  SetOutputVardata (outstring, COWMIDHostLevDispute,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_MID_hostlev(specific_sub_dispute_num)],
                     true, [COW_dyadic_dispute_data_30.get_MID_hostlev(specific_sub_dispute_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  {Now always output MID number}
                  SetOutputVardata (outstring, COW_disputes,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_MID_num(specific_sub_dispute_num)],
                     true, [COW_dyadic_dispute_data_30.get_MID_num(specific_sub_dispute_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);


                  {originator will report if originator of this dispute}
                  if COWMIDOriginator in user_selections.output_format.variables then
                  SetOutputVardata (outstring, COWMIDOriginator,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_originator(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_originator(specific_sub_dispute_num, ccode2)],
                     true, [COW_dyadic_dispute_data_30.get_originator(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_originator(specific_sub_dispute_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWMarkMIDJoiners in user_selections.output_format.variables then
                     begin
                        {Want to output 2 variable for "joined", one is joined the
                         initiators, one is joined the targets OF ANY DISPUTE IN THIS YEAR.
                         This should be basically like coding the initiators above, but by only
                         marking a "1" if it is a joiner.  Other criteria for sideA/Revision hold.
                         So, mark joiners as states in sideA or revisionist states.  But, if they are
                         ever a joiner, they're always a joiner, so always mark them as a joiner.
                         Note - this might have to be rethought in cases of 2 disputes in a year,
                         where one is a joining and one is an initiation.  But actually that is
                         probably OK, because then they will just be coded as both an initiator
                         and a joiner.}
                        {if this code for joiners changes, check also the "want_in_year" procedure.}

                        {Note:  changed this code on 6/7/2000 to only call using the disp # that
                         was found above by the relevant_hostlev procedure, or the specific
                         disp# passed into the procedure.  This was done because I am now
                         reporting only if a state joined in THIS MID and because the proc
                         called with ccode-year values can change the value of found disp #.}

                        {Note added 10/10/2001.  Apparently I never made this change - output and
                         call continued to return that you're a joiner if you are a joiner in any
                         MID in the year in question.  To fix this, need to change the call to use
                         the use_dispute parameter rather than the general "use_param" which usually
                         meant just use the ccode-year, looking for any joining.

                         To preserve backwards compatibility and add more info,
                         I've now added 2 variants on the joiner variable.
                         The first variant looks for any joining in year, and returns the number of the MID
                         that was joined in joined_disp_num.  The second variant looks at the specific MID
                         # above.   }

                        {first need to set 2 joiner variables for the directed dyad, ccode-year situation.
                        {first, mark joiner_i}
                        case user_selections.dispute_info.SideAIsInitiator of
                         true:      {sideA}
                            joini_anyinyear := COW_dyadic_dispute_data_30.Is_AnyYear_Joined_Initiation (ccode1, ccode2, curryear, joined_disp_num, use_ccodeyear);
                         false:    {Revisionist}
                            joini_anyinyear := COW_dyadic_dispute_data_30.Is_AnyYear_Joined_Revision (ccode1, ccode2, curryear, joined_disp_num, use_ccodeyear);
                        end;   {case sideA is initiator}

                        {now, mark joiner_t  (target)}
                        case user_selections.dispute_info.SideAIsInitiator of
                         true:      {sideA}
                            joint_anyinyear := COW_dyadic_dispute_data_30.Is_AnyYear_Joined_Targets (ccode1, ccode2, curryear, joined_disp_num, use_ccodeyear);
                         false:    {Revisionist}
                            joint_anyinyear := COW_dyadic_dispute_data_30.Is_AnyYear_Joined_SQ (ccode1, ccode2, curryear, joined_disp_num, use_ccodeyear);
                        end;   {case sideA is initiator}

                        {Now set 2 joiner variables for the directed dyad situation, but
                         using the specific MID found by the initial MID procedure.
                        {first, mark joiner_i}
                        case user_selections.dispute_info.SideAIsInitiator of
                         true:      {sideA}
                            joini_thismid := COW_dyadic_dispute_data_30.Is_AnyYear_Joined_Initiation (ccode1, ccode2, curryear, specific_sub_dispute_num, use_dispute);
                         false:    {Revisionist}
                            joini_thismid := COW_dyadic_dispute_data_30.Is_AnyYear_Joined_Revision (ccode1, ccode2, curryear, specific_sub_dispute_num, use_dispute);
                        end;   {case sideA is initiator}

                        {now, mark joiner_t  (target)}
                        case user_selections.dispute_info.SideAIsInitiator of
                         true:      {sideA}
                            joint_thismid := COW_dyadic_dispute_data_30.Is_AnyYear_Joined_Targets (ccode1, ccode2, curryear, specific_sub_dispute_num, use_dispute);
                         false:    {Revisionist}
                            joint_thismid := COW_dyadic_dispute_data_30.Is_AnyYear_Joined_SQ (ccode1, ccode2, curryear, specific_sub_dispute_num, use_dispute);
                        end;   {case sideA is initiator}


                        {for nodirected, it is always just Is_AnyYear_NonDir_Dispute_Joiners, since there
                         is no direction for initiator/target side.  And there is just one output # stating
                         that at least one of them was a joiner.  So a better label might be "latecomer dyad"}

                        SetOutputVardata (outstring, COWMarkMIDJoiners,
                           false, [0],
                           true, [joini_anyinyear, joint_anyinyear, joini_thismid, joint_thismid],
                           true, [COW_dyadic_dispute_data_30.Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2, curryear, joined_disp_num, use_ccodeyear),
                                  COW_dyadic_dispute_data_30.Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2, curryear, specific_sub_dispute_num, use_dispute)],
                           1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                     end;    {joiners in output}


                  {Now for calls from this point on, I can get information for the specific
                  dispute number in specific_sub_dispute_num.  The rest of these outputs are options that
                  the user may or may not have selected for output.}
                  if COWMIDName in user_selections.output_format.variables then
                  SetOutputVardata (outstring, COWMIDName,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_MID_name(specific_sub_dispute_num)],
                     true, [COW_dyadic_dispute_data_30.get_MID_name(specific_sub_dispute_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWMIDStart in user_selections.output_format.variables then
                  SetOutputVardata (outstring, COWMIDStart,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_stmonth(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_stday(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_styear(specific_sub_dispute_num, ccode1),
                            COW_dyadic_dispute_data_30.get_stmonth(specific_sub_dispute_num, ccode2),COW_dyadic_dispute_data_30.get_stday(specific_sub_dispute_num, ccode2),COW_dyadic_dispute_data_30.get_styear(specific_sub_dispute_num, ccode2)],
                     true, [COW_dyadic_dispute_data_30.get_stmonth(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_stday(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_styear(specific_sub_dispute_num, ccode1),
                            COW_dyadic_dispute_data_30.get_stmonth(specific_sub_dispute_num, ccode2),COW_dyadic_dispute_data_30.get_stday(specific_sub_dispute_num, ccode2),COW_dyadic_dispute_data_30.get_styear(specific_sub_dispute_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWMIDEnd in user_selections.output_format.variables then
                  SetOutputVardata (outstring, COWMIDEnd,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_endmonth(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_endday(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_endyear(specific_sub_dispute_num, ccode1),
                            COW_dyadic_dispute_data_30.get_endmonth(specific_sub_dispute_num, ccode2),COW_dyadic_dispute_data_30.get_endday(specific_sub_dispute_num, ccode2),COW_dyadic_dispute_data_30.get_endyear(specific_sub_dispute_num, ccode2)],
                     true, [COW_dyadic_dispute_data_30.get_endmonth(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_endday(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_endyear(specific_sub_dispute_num, ccode1),
                            COW_dyadic_dispute_data_30.get_endmonth(specific_sub_dispute_num, ccode2),COW_dyadic_dispute_data_30.get_endday(specific_sub_dispute_num, ccode2),COW_dyadic_dispute_data_30.get_endyear(specific_sub_dispute_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWMIDSideA in user_selections.output_format.variables then
                  SetOutputVardata (outstring, COWMIDSideA,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_sideA(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_sideA(specific_sub_dispute_num, ccode2)],
                     true, [COW_dyadic_dispute_data_30.get_sideA(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_sideA(specific_sub_dispute_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWMIDRevisionist in user_selections.output_format.variables then
                  SetOutputVardata (outstring, COWMIDRevisionist,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_revisionist(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_revisionist(specific_sub_dispute_num, ccode2)],
                     true, [COW_dyadic_dispute_data_30.get_revisionist(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_revisionist(specific_sub_dispute_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWMIDRevisiontype in user_selections.output_format.variables then
                  SetOutputVardata (outstring, COWMIDRevisiontype,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_revtype(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_revtype2(specific_sub_dispute_num, ccode1),
                            COW_dyadic_dispute_data_30.get_revtype(specific_sub_dispute_num, ccode2),COW_dyadic_dispute_data_30.get_revtype2(specific_sub_dispute_num, ccode2)],
                     true, [COW_dyadic_dispute_data_30.get_revtype(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_revtype2(specific_sub_dispute_num, ccode1),
                            COW_dyadic_dispute_data_30.get_revtype(specific_sub_dispute_num, ccode2),COW_dyadic_dispute_data_30.get_revtype2(specific_sub_dispute_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWMIDFatalityState in user_selections.output_format.variables then
                  SetOutputVardata (outstring, COWMIDFatalityState,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_fatality_state(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_fatality_state_precise(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_fatality_state(specific_sub_dispute_num, ccode2),COW_dyadic_dispute_data_30.get_fatality_state_precise(specific_sub_dispute_num, ccode2)],
                     true, [COW_dyadic_dispute_data_30.get_fatality_state(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_fatality_state_precise(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_fatality_state(specific_sub_dispute_num, ccode2),COW_dyadic_dispute_data_30.get_fatality_state_precise(specific_sub_dispute_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWMIDHiActState in user_selections.output_format.variables then
                  SetOutputVardata (outstring, COWMIDHiActState,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_hiact_state(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_hiact_state(specific_sub_dispute_num, ccode2)],
                     true, [COW_dyadic_dispute_data_30.get_hiact_state(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_hiact_state(specific_sub_dispute_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWMIDHiActDispute in user_selections.output_format.variables then
                  SetOutputVardata (outstring, COWMIDHiActDispute,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_MID_hiact(specific_sub_dispute_num)],
                     true, [COW_dyadic_dispute_data_30.get_MID_hiact(specific_sub_dispute_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWMIDOutcome in user_selections.output_format.variables then
                  SetOutputVardata (outstring, COWMIDOutcome,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_MID_outcome(specific_sub_dispute_num)],
                     true, [COW_dyadic_dispute_data_30.get_MID_outcome(specific_sub_dispute_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWMIDSettlement in user_selections.output_format.variables then
                  SetOutputVardata (outstring, COWMIDSettlement,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_MID_settlement(specific_sub_dispute_num)],
                     true, [COW_dyadic_dispute_data_30.get_MID_settlement(specific_sub_dispute_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWMIDFatalityDispute in user_selections.output_format.variables then
                  SetOutputVardata (outstring, COWMIDFatalityDispute,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_MID_fatality(specific_sub_dispute_num)],
                     true, [COW_dyadic_dispute_data_30.get_MID_fatality(specific_sub_dispute_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWMIDReciprocated in user_selections.output_format.variables then
                  SetOutputVardata (outstring, COWMIDReciprocated,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_MID_reciprocated(specific_sub_dispute_num)],
                     true, [COW_dyadic_dispute_data_30.get_MID_reciprocated(specific_sub_dispute_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWMIDNumStates in user_selections.output_format.variables then
                  SetOutputVardata (outstring, COWMIDNumStates,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_MID_numstates(specific_sub_dispute_num, ccode1), COW_dyadic_dispute_data_30.get_MID_numstates(specific_sub_dispute_num, ccode2)],
                     true, [COW_dyadic_dispute_data_30.get_MID_numstates(specific_sub_dispute_num, ccode1), COW_dyadic_dispute_data_30.get_MID_numstates(specific_sub_dispute_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWNumMIDs in user_selections.output_format.variables then
                  SetOutputVardata (outstring, COWNumMIDs,
                     false, [0],
                     true, [COW_dyadic_dispute_data_30.get_num_new_mids(ccode1, ccode2, curryear), COW_dyadic_dispute_data_30.get_num_total_mids(ccode1, ccode2, curryear)],
                     true, [COW_dyadic_dispute_data_30.get_num_new_mids(ccode1, ccode2, curryear), COW_dyadic_dispute_data_30.get_num_total_mids(ccode1, ccode2, curryear)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWpeaceyrs in user_selections.output_format.variables then
                     SetOutputVardata (outstring, COWpeaceyrs,
                        false, [0],
                        true, [COW_dyadic_dispute_data_30.get_peace_years(ccode1, ccode2, curryear, user_selections, werner_peace_years_data)],
                        true, [COW_dyadic_dispute_data_30.get_peace_years(ccode1, ccode2, curryear, user_selections, werner_peace_years_data)],
                        4,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWPeaceDays in user_selections.output_format.variables then
                     SetOutputVardata (outstring, COWPeaceDays,
                        false, [0],
                        true, [COW_dyadic_dispute_data_30.get_peace_days(ccode1, ccode2, curryear, user_selections, werner_peace_years_data)],
                        true, [COW_dyadic_dispute_data_30.get_peace_days(ccode1, ccode2, curryear, user_selections, werner_peace_years_data)],
                        5,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWRole in user_selections.output_format.variables then
                     SetOutputVardata (outstring, COWRole,
                        false, [0],
                        true, [COW_dyadic_dispute_data_30.get_Role(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_Role(specific_sub_dispute_num, ccode2)],
                        true, [COW_dyadic_dispute_data_30.get_Role(specific_sub_dispute_num, ccode1),COW_dyadic_dispute_data_30.get_Role(specific_sub_dispute_num, ccode2)],
                        4,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if COWLinkStatus in user_selections.output_format.variables then
                     SetOutputVardata (outstring, COWLinkStatus,
                        false, [0],
                        true, [COW_dyadic_dispute_data_30.get_Link1(specific_sub_dispute_num),COW_dyadic_dispute_data_30.get_Link2(specific_sub_dispute_num),COW_dyadic_dispute_data_30.get_Link3(specific_sub_dispute_num),COW_dyadic_dispute_data_30.get_ongoing2001(specific_sub_dispute_num)],
                        true, [COW_dyadic_dispute_data_30.get_Link1(specific_sub_dispute_num),COW_dyadic_dispute_data_30.get_Link2(specific_sub_dispute_num),COW_dyadic_dispute_data_30.get_Link3(specific_sub_dispute_num),COW_dyadic_dispute_data_30.get_ongoing2001(specific_sub_dispute_num)],
                        4,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  {Not outputting Side Dyadic.}

                      end;    {cow_disputes in output}

            if (Maoz_dyadic_disputes in user_selections.output_format.variables) then
               begin
                  ongoing_disp_num  := null_dispute_number;
                  initiation_disp_num := null_dispute_number;
                  joined_disp_num := null_dispute_number;
                  specific_sub_dispute_num := null_dispute_number;
                  found_cow_dispute_num := null_dispute_number;
                  found_ongoing_num := null_dispute_number;
                  ongoing_disp_year_num := null_dispute_number;
                  initiation_disp_year_num := null_dispute_number;
                  joined_disp_year_num := null_dispute_number;
                  specific_sub_dispute_year_num := null_dispute_number;
                  found_cow_dispute_year_num := null_dispute_number;
                  crisis_num := null_dispute_number;
                  found_crisis_num := null_dispute_number;

                  SetOutputVardata (outstring, Maoz_dyadic_disputes,
                     false, [0],
                     true, [Maoz_dispute_data.is_ongoing(ccode1, ccode2, curryear, ongoing_disp_year_num)],
                     true, [Maoz_dispute_data.is_ongoing(ccode1, ccode2, curryear, ongoing_disp_year_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  SetOutputVardata (outstring, Maoz_dyadic_disputes,
                     false, [0],
                     true, [Maoz_dispute_data.get_MID_num(ongoing_disp_year_num)],
                     true, [Maoz_dispute_data.get_MID_num(ongoing_disp_year_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);


                 initiation_disp_year_num := input_disp_year_num_for_maoz;

                 {** Note:  if this code changes, be sure to update the "sampled" function,
                   which uses these commands to check on whether there is a dispute for
                   stratified sampling purposes.}
                  case user_selections.output_this of
                        output_monads: init := false;   {this will not be output}
                        output_directed_dyads, output_directed_dispute_initiation_dyads :
                           case user_selections.dispute_info.MarkSubsequentAsInitiation of
                                 true: init := Maoz_dispute_data.wanted_new_or_continuing_initiation (ccode1, ccode2, curryear, user_selections, initiation_disp_year_num, use_param);
                                 false: init := Maoz_dispute_data.wanted_new_initiation (ccode1, ccode2, curryear, user_selections, initiation_disp_year_num, use_param);
                              end;
                        output_nondirected_dyads, output_nondirected_dispute_dyads :
                           case user_selections.dispute_info.MarkSubsequentAsInitiation of
                                 true: init := Maoz_dispute_data.wanted_new_or_Continuing_NonDir_dispute (ccode1, ccode2, curryear, user_selections, initiation_disp_year_num, use_param);
                                 false: init := Maoz_dispute_data.wanted_new_nondir_dispute (ccode1, ccode2, curryear, user_selections, initiation_disp_year_num, use_param);
                              end;
                  end;
                  {output initiation}
                  SetOutputVardata (outstring, Maoz_dyadic_disputes,
                     false, [0], true, [init, Maoz_dispute_data.get_MID_num(initiation_disp_year_num)], true, [init, Maoz_dispute_data.get_MID_num(initiation_disp_year_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);


                  {At this point, I can identify the appropriate MID # for this dyad if it was
                   called with ccodeyear. Then, most of everything else will be called with that MIDnum.}
                  found_cow_dispute_year_num := Maoz_dispute_data.relevant_midnum(ccode1, ccode2, curryear, user_selections);

                  case use_param of
                     use_dispute : specific_sub_dispute_year_num := input_disp_year_num_for_maoz;
                     use_ccodeyear : specific_sub_dispute_year_num := found_cow_dispute_year_num;
                     else EUGeneError ('error in euinout outputting disputes - value of use_param not recognized.  notify programmer.',1,stop,error_log);
                  end;      {case}

                    {Now output hostility level.  Always output this.  }
                  SetOutputVardata (outstring, Maoz_dyadic_disputes,
                     false, [0],
                     true, [Maoz_dispute_data.get_hostlev_state(specific_sub_dispute_year_num, ccode1), Maoz_dispute_data.get_hostlev_state(specific_sub_dispute_year_num, ccode2)],
                     true, [Maoz_dispute_data.get_hostlev_state(specific_sub_dispute_year_num, ccode1), Maoz_dispute_data.get_hostlev_state(specific_sub_dispute_year_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  {now disp host lev if they want it.}
                  if MaozMIDHostLevDispute in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozMIDHostLevDispute,
                     false, [0],
                     true, [Maoz_dispute_data.get_MID_hostlev(specific_sub_dispute_year_num)],
                     true, [Maoz_dispute_data.get_MID_hostlev(specific_sub_dispute_year_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                     {Now Maoz MID #, now always output this.}
                  SetOutputVardata (outstring, Maoz_dyadic_disputes,
                     false, [0],
                     true, [Maoz_dispute_data.get_MID_num(specific_sub_dispute_year_num)],
                     true, [Maoz_dispute_data.get_MID_num(specific_sub_dispute_year_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozMIDOriginator in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozMIDOriginator,
                     false, [0],
                     true, [Maoz_dispute_data.get_originator(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_originator(specific_sub_dispute_year_num, ccode2)],
                     true, [Maoz_dispute_data.get_originator(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_originator(specific_sub_dispute_year_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozMarkMIDJoiners in user_selections.output_format.variables then
                     begin
                        {first need to set 2 joiner variables for the directed dyad situation.
                        {first, mark joiner_i}
                        case user_selections.dispute_info.SideAIsInitiator of
                         true:      {sideA}
                            joini_anyinyear := Maoz_dispute_data.Is_AnyYear_Joined_Initiation (ccode1, ccode2, curryear, joined_disp_year_num, use_ccodeyear);
                         false:    {Revisionist}
                            joini_anyinyear := Maoz_dispute_data.Is_AnyYear_Joined_Revision (ccode1, ccode2, curryear, joined_disp_year_num, use_ccodeyear);
                        end;   {case sideA is initiator}

                        {now, mark joiner_t  (target)}
                        case user_selections.dispute_info.SideAIsInitiator of
                         true:      {sideA}
                            joint_anyinyear := Maoz_dispute_data.Is_AnyYear_Joined_Targets (ccode1, ccode2, curryear, joined_disp_year_num, use_ccodeyear);
                         false:    {Revisionist}
                            joint_anyinyear := Maoz_dispute_data.Is_AnyYear_Joined_SQ (ccode1, ccode2, curryear, joined_disp_year_num, use_ccodeyear);
                        end;   {case sideA is initiator}

                        {Now set 2 joiner variables for the directed dyad situation, but
                         using the specific MID found by the initial MID procedure.
                        {first, mark joiner_i}
                        case user_selections.dispute_info.SideAIsInitiator of
                         true:      {sideA}
                            joini_thismid := Maoz_dispute_data.Is_AnyYear_Joined_Initiation (ccode1, ccode2, curryear, specific_sub_dispute_year_num, use_dispute);
                         false:    {Revisionist}
                            joini_thismid := Maoz_dispute_data.Is_AnyYear_Joined_Revision (ccode1, ccode2, curryear, specific_sub_dispute_year_num, use_dispute);
                        end;   {case sideA is initiator}

                        {now, mark joiner_t  (target)}
                        case user_selections.dispute_info.SideAIsInitiator of
                         true:      {sideA}
                            joint_thismid := Maoz_dispute_data.Is_AnyYear_Joined_Targets (ccode1, ccode2, curryear, specific_sub_dispute_year_num, use_dispute);
                         false:    {Revisionist}
                            joint_thismid := Maoz_dispute_data.Is_AnyYear_Joined_SQ (ccode1, ccode2, curryear, specific_sub_dispute_year_num, use_dispute);
                        end;   {case sideA is initiator}

                        SetOutputVardata (outstring, MaozMarkMIDJoiners,
                           false, [0],
                           true, [joini_anyinyear, joint_anyinyear, joini_thismid, joint_thismid],
                           true, [Maoz_dispute_data.Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2, curryear, joined_disp_year_num, use_ccodeyear),
                                  Maoz_dispute_data.Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2, curryear, specific_sub_dispute_year_num, use_dispute)],
                           1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                     end;    {joiners in output}


                  {Now for calls from this point on, I can get information for the specific
                  dispute number in specific_sub_dispute_num.  The rest of these outputs are options that
                  the user may or may not have selected for output.}

                  if MaozMIDName in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozMIDName,
                     false, [0],
                     true, [Maoz_dispute_data.get_MID_name(specific_sub_dispute_year_num)],
                     true, [Maoz_dispute_data.get_MID_name(specific_sub_dispute_year_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozMIDStart in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozMIDStart,
                     false, [0],
                     true, [Maoz_dispute_data.get_stmonth(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_stday(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_styear(specific_sub_dispute_year_num, ccode1),
                            Maoz_dispute_data.get_stmonth(specific_sub_dispute_year_num, ccode2),Maoz_dispute_data.get_stday(specific_sub_dispute_year_num, ccode2),Maoz_dispute_data.get_styear(specific_sub_dispute_year_num, ccode2)],
                     true, [Maoz_dispute_data.get_stmonth(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_stday(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_styear(specific_sub_dispute_year_num, ccode1),
                            Maoz_dispute_data.get_stmonth(specific_sub_dispute_year_num, ccode2),Maoz_dispute_data.get_stday(specific_sub_dispute_year_num, ccode2),Maoz_dispute_data.get_styear(specific_sub_dispute_year_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozMIDEnd in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozMIDEnd,
                     false, [0],
                     true, [Maoz_dispute_data.get_endmonth(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_endday(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_endyear(specific_sub_dispute_year_num, ccode1),
                            Maoz_dispute_data.get_endmonth(specific_sub_dispute_year_num, ccode2),Maoz_dispute_data.get_endday(specific_sub_dispute_year_num, ccode2),Maoz_dispute_data.get_endyear(specific_sub_dispute_year_num, ccode2)],
                     true, [Maoz_dispute_data.get_endmonth(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_endday(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_endyear(specific_sub_dispute_year_num, ccode1),
                            Maoz_dispute_data.get_endmonth(specific_sub_dispute_year_num, ccode2),Maoz_dispute_data.get_endday(specific_sub_dispute_year_num, ccode2),Maoz_dispute_data.get_endyear(specific_sub_dispute_year_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozMIDSideA in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozMIDSideA,
                     false, [0],
                     true, [Maoz_dispute_data.get_sideA(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_sideA(specific_sub_dispute_year_num, ccode2)],
                     true, [Maoz_dispute_data.get_sideA(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_sideA(specific_sub_dispute_year_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozMIDRevisionist in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozMIDRevisionist,
                     false, [0],
                     true, [Maoz_dispute_data.get_revisionist(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_revisionist(specific_sub_dispute_year_num, ccode2)],
                     true, [Maoz_dispute_data.get_revisionist(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_revisionist(specific_sub_dispute_year_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozMIDRevisiontype in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozMIDRevisiontype,
                     false, [0],
                     true, [Maoz_dispute_data.get_revtype(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_revtype2(specific_sub_dispute_year_num, ccode1),
                            Maoz_dispute_data.get_revtype(specific_sub_dispute_year_num, ccode2),Maoz_dispute_data.get_revtype2(specific_sub_dispute_year_num, ccode2)],
                     true, [Maoz_dispute_data.get_revtype(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_revtype2(specific_sub_dispute_year_num, ccode1),
                            Maoz_dispute_data.get_revtype(specific_sub_dispute_year_num, ccode2),Maoz_dispute_data.get_revtype2(specific_sub_dispute_year_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozMIDFatalityState in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozMIDFatalityState,
                     false, [0],
                     true, [Maoz_dispute_data.get_fatality_state(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_fatality_state_precise(specific_sub_dispute_year_num, ccode1),
                            Maoz_dispute_data.get_fatality_state(specific_sub_dispute_year_num, ccode2),Maoz_dispute_data.get_fatality_state_precise(specific_sub_dispute_year_num, ccode2)],
                     true, [Maoz_dispute_data.get_fatality_state(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_fatality_state_precise(specific_sub_dispute_year_num, ccode1),
                            Maoz_dispute_data.get_fatality_state(specific_sub_dispute_year_num, ccode2),Maoz_dispute_data.get_fatality_state_precise(specific_sub_dispute_year_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozMIDHiActState in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozMIDHiActState,
                     false, [0],
                     true, [Maoz_dispute_data.get_hiact_state(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_hiact_state(specific_sub_dispute_year_num, ccode2)],
                     true, [Maoz_dispute_data.get_hiact_state(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_hiact_state(specific_sub_dispute_year_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozMIDHiActDispute in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozMIDHiActDispute,
                     false, [0],
                     true, [Maoz_dispute_data.get_MID_hiact(specific_sub_dispute_year_num)],
                     true, [Maoz_dispute_data.get_MID_hiact(specific_sub_dispute_year_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozMIDOutcome in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozMIDOutcome,
                     false, [0],
                     true, [Maoz_dispute_data.get_MID_outcome(specific_sub_dispute_year_num)],
                     true, [Maoz_dispute_data.get_MID_outcome(specific_sub_dispute_year_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozMIDSettlement in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozMIDSettlement,
                     false, [0],
                     true, [Maoz_dispute_data.get_MID_settlement(specific_sub_dispute_year_num)],
                     true, [Maoz_dispute_data.get_MID_settlement(specific_sub_dispute_year_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozMIDFatalityDispute in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozMIDFatalityDispute,
                     false, [0],
                     true, [Maoz_dispute_data.get_MID_fatality(specific_sub_dispute_year_num)],
                     true, [Maoz_dispute_data.get_MID_fatality(specific_sub_dispute_year_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozMIDReciprocated in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozMIDReciprocated,
                     false, [0],
                     true, [Maoz_dispute_data.get_MID_reciprocated(specific_sub_dispute_year_num)],
                     true, [Maoz_dispute_data.get_MID_reciprocated(specific_sub_dispute_year_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozMIDNumStates in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozMIDNumStates,
                     false, [0],
                     true, [Maoz_dispute_data.get_MID_numstates(specific_sub_dispute_year_num, ccode1), Maoz_dispute_data.get_MID_numstates(specific_sub_dispute_year_num, ccode2)],
                     true, [Maoz_dispute_data.get_MID_numstates(specific_sub_dispute_year_num, ccode1), Maoz_dispute_data.get_MID_numstates(specific_sub_dispute_year_num, ccode2)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozCOWWar in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozCOWWar,
                     false, [0],
                     true, [Maoz_dispute_data.get_COWWar(specific_sub_dispute_year_num)],
                     true, [Maoz_dispute_data.get_COWWar(specific_sub_dispute_year_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozDurindx in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozDurindx,
                     false, [0],
                     true, [Maoz_dispute_data.get_Durindx(specific_sub_dispute_year_num)],
                     true, [Maoz_dispute_data.get_Durindx(specific_sub_dispute_year_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozDurDays in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozDurDays,
                     false, [0],
                     true, [Maoz_dispute_data.get_DurDays(specific_sub_dispute_year_num)],
                     true, [Maoz_dispute_data.get_DurDays(specific_sub_dispute_year_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozNumMIDs in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozNumMIDs,
                     false, [0],
                     true, [Maoz_dispute_data.get_num_new_mids(ccode1, ccode2, curryear), Maoz_dispute_data.get_num_total_mids(ccode1, ccode2, curryear)],
                     true, [Maoz_dispute_data.get_num_new_mids(ccode1, ccode2, curryear), Maoz_dispute_data.get_num_total_mids(ccode1, ccode2, curryear)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  {Next variable not available in Maoz Dyadic Data v1.1
                  if MaozReciprocatedDyadic in user_selections.output_format.variables then
                  SetOutputVardata (outstring, MaozReciprocatedDyadic,
                     false, [0],
                     true, [Maoz_dispute_data.get_ReciprocatedDyadic(specific_sub_dispute_year_num)],
                     true, [Maoz_dispute_data.get_ReciprocatedDyadic(specific_sub_dispute_year_num)],
                     1,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);
                  }

                 if Maozpeaceyrs in user_selections.output_format.variables then
                     SetOutputVardata (outstring, Maozpeaceyrs,
                        false, [0],
                        true, [Maoz_dispute_data.get_peace_years(ccode1, ccode2, curryear, user_selections, werner_peace_years_data)],
                        true, [Maoz_dispute_data.get_peace_years(ccode1, ccode2, curryear, user_selections, werner_peace_years_data)],
                        4,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                 if Maozpeacedys in user_selections.output_format.variables then
                     SetOutputVardata (outstring, Maozpeacedys,
                        false, [0],
                        true, [Maoz_dispute_data.get_peace_days(ccode1, ccode2, curryear, user_selections, werner_peace_years_data)],
                        true, [Maoz_dispute_data.get_peace_days(ccode1, ccode2, curryear, user_selections, werner_peace_years_data)],
                        5,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                 if MaozRole in user_selections.output_format.variables then
                     SetOutputVardata (outstring, MaozRole,
                        false, [0],
                        true, [Maoz_dispute_data.get_role(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_role(specific_sub_dispute_year_num, ccode2)],
                        true, [Maoz_dispute_data.get_role(specific_sub_dispute_year_num, ccode1),Maoz_dispute_data.get_role(specific_sub_dispute_year_num, ccode2)],
                        4,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);

                  if MaozLinkStatus in user_selections.output_format.variables then
                     SetOutputVardata (outstring, MaozLinkStatus,
                        false, [0],
                        true, [Maoz_dispute_data.get_Link1(specific_sub_dispute_year_num),Maoz_dispute_data.get_Link2(specific_sub_dispute_year_num),Maoz_dispute_data.get_Link3(specific_sub_dispute_year_num),Maoz_dispute_data.get_ongoing2001(specific_sub_dispute_year_num)],
                        true, [Maoz_dispute_data.get_Link1(specific_sub_dispute_year_num),Maoz_dispute_data.get_Link2(specific_sub_dispute_year_num),Maoz_dispute_data.get_Link3(specific_sub_dispute_year_num),Maoz_dispute_data.get_ongoing2001(specific_sub_dispute_year_num)],
                        4,0, curryear, configuration.first_MID_year, configuration.last_MID_year, missing_value);
               end;     {if maoz mids in output}


            {ICB crises}
            if (ICB_crises in user_selections.output_format.variables) then
               begin
                  ongoing_disp_num  := null_dispute_number;
                  initiation_disp_num := null_dispute_number;
                  joined_disp_num := null_dispute_number;
                  specific_sub_dispute_num := null_dispute_number;
                  found_cow_dispute_num := null_dispute_number;
                  found_ongoing_num := null_dispute_number;
                  ongoing_disp_year_num := null_dispute_number;
                  initiation_disp_year_num := null_dispute_number;
                  joined_disp_year_num := null_dispute_number;
                  specific_sub_dispute_year_num := null_dispute_number;
                  found_cow_dispute_year_num := null_dispute_number;
                  crisis_num := null_dispute_number;
                  found_crisis_num := null_dispute_number;

                     {First output whether there was an ongoing dispute at the
                      beginning of the year or not.  Always output this.  This is always by ccode year.}
                     {if I care, crisis_num will have in it the # of the dispute that's ongoing}
                  SetOutputVardata (outstring, ICB_crises,
                     false, [0],
                     true, [ICB_crisis_data.is_ongoing(ccode1, ccode2, curryear, found_ongoing_num)],
                     true, [ICB_crisis_data.is_ongoing(ccode1, ccode2, curryear, found_ongoing_num)],
                     1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                  SetOutputVardata (outstring, ICB_crises,
                     false, [0],
                     true, [ICB_crisis_data.get_crisis_num(found_ongoing_num)],
                     true, [ICB_crisis_data.get_crisis_num(found_ongoing_num)],
                     1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                  {Now check other dispute info}

                  {First, set dispute number to be a specific dispute number that was
                   passed into the program, if there was one.  Note that this is the internal
                   dyadic crisis number, not the ICB crisis number.  This must be done now
                   because the "get_ongoing" call above may reset the value of crisis_num.
                   Usually, though, there won't be a specific #.}

                  {Now output initiation (onset) in its appropriate form.  Always output this.  }
                   {Initiation could be one
                   of several things, depending on the combo of chosen output options.
                   Combos involve Originators, and mark subsequent years.
                   Figure it out first, using main proc from dyadic_dispute type.  The call with
                   use_param parameter will ensure getting either the ccode-year or the disp# form.}
                   {note the new-initiation routine checks for revision/sideA coding.}

                  {In case we get in this routine called with using specific disp #, set "crisis_num"
                   to that specific disp num.  Still have found_ongoing_num stored.}
                  crisis_num := input_crisis_num_for_icb;
                  case user_selections.output_this of
                        output_monads: init := false;   {this will not be output for monads}
                        {for ICB, output is always nondirected onset}
                        output_directed_dyads, output_directed_dispute_initiation_dyads, output_nondirected_dispute_dyads, output_nondirected_dyads :
                           case user_selections.dispute_info.MarkSubsequentAsInitiation of
                                 true: init := ICB_crisis_data.wanted_new_or_Continuing_NonDir_dispute (ccode1, ccode2, curryear, user_selections, crisis_num, use_param);
                                 false: init := ICB_crisis_data.wanted_new_nondir_dispute (ccode1, ccode2, curryear, user_selections, crisis_num, use_param);
                              end;
                     end;
                  {output initiation}
                  SetOutputVardata (outstring, ICB_crises,
                     false, [0], true, [init], true, [init],
                     1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                  {Always do crisis #}
                  SetOutputVardata (outstring, ICB_crises,
                     false, [0],
                     true, [ICB_crisis_data.get_crisis_num(crisis_num)],
                     true, [ICB_crisis_data.get_crisis_num(crisis_num)],
                     1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);


                  {Here, if there's an initiation, then use that crisis #.  Otherwise,
                   use the ongoing crisis number.}
                  if crisis_num > null_dispute_number then
                     found_crisis_num := crisis_num
                  else if ICB_crisis_data.is_ongoing(ccode1, ccode2, curryear, found_ongoing_num) then
                     found_crisis_num := found_ongoing_num
                  else found_crisis_num := null_dispute_number;

                  {the above func will return the crisis # of the crisis I care about for
                   all the procedures below EXCEPT for the joiner function.  Or, if procedure was called
                   for a specific dispute, then this var will just take on the
                   value that was passed into the proc, because crisis_num will not
                   have changed in any sub-procedure above.}

                  {Now for calls from this point on, I can get information for the specific
                  dispute number in found_crisis_num.  The rest of these outputs are options that
                  the user may or may not have selected for output.}

                  if ICBCrisisName in user_selections.output_format.variables then
                  SetOutputVardata (outstring, ICBCrisisName,
                     false, [0],
                     true, [ICB_crisis_data.get_crisname(found_crisis_num)],
                     true, [ICB_crisis_data.get_crisname(found_crisis_num)],
                     1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                  if ICBCrisisDyadNumber in user_selections.output_format.variables then
                  SetOutputVardata (outstring, ICBCrisisDyadNumber,
                     false, [0],
                     true, [ICB_crisis_data.get_crdynum(found_crisis_num)],
                     true, [ICB_crisis_data.get_crdynum(found_crisis_num)],
                     1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                  if ICBOneSided in user_selections.output_format.variables then
                  SetOutputVardata (outstring, ICBOneSided,
                     false, [0],
                     true, [ICB_crisis_data.get_oneside(found_crisis_num)],
                     true, [ICB_crisis_data.get_oneside(found_crisis_num)],
                     1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                  if ICBDyadicStart in user_selections.output_format.variables then
                  SetOutputVardata (outstring, ICBDyadicStart,
                     false, [0],
                     true, [ICB_crisis_data.get_trgyrdy(found_crisis_num),ICB_crisis_data.get_trgmody(found_crisis_num),ICB_crisis_data.get_trgdady(found_crisis_num)],
                     true, [ICB_crisis_data.get_trgyrdy(found_crisis_num),ICB_crisis_data.get_trgmody(found_crisis_num),ICB_crisis_data.get_trgdady(found_crisis_num)],
                     1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                  if ICBDyadicEnd in user_selections.output_format.variables then
                  SetOutputVardata (outstring, ICBDyadicEnd,
                     false, [0],
                     true, [ICB_crisis_data.get_trmyrdy(found_crisis_num),ICB_crisis_data.get_trmmody(found_crisis_num),ICB_crisis_data.get_trmdady(found_crisis_num)],
                     true, [ICB_crisis_data.get_trmyrdy(found_crisis_num),ICB_crisis_data.get_trmmody(found_crisis_num),ICB_crisis_data.get_trmdady(found_crisis_num)],
                     1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                  if ICBDurDays in user_selections.output_format.variables then
                  SetOutputVardata (outstring, ICBDurDays,
                     false, [0],
                     true, [ICB_crisis_data.get_durdays(found_crisis_num)],
                     true, [ICB_crisis_data.get_durdays(found_crisis_num)],
                     1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                  if ICBDurYear in user_selections.output_format.variables then
                  SetOutputVardata (outstring, ICBDurYear,
                     false, [0],
                     true, [ICB_crisis_data.get_duryear(found_crisis_num)],
                     true, [ICB_crisis_data.get_duryear(found_crisis_num)],
                     1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                  if ICBStateStart in user_selections.output_format.variables then
                  SetOutputVardata (outstring, ICBStateStart,
                     false, [0],
                     true, [ICB_crisis_data.get_yrtriga(found_crisis_num),ICB_crisis_data.get_motriga(found_crisis_num),ICB_crisis_data.get_datriga(found_crisis_num),
                            ICB_crisis_data.get_yrtrigb(found_crisis_num),ICB_crisis_data.get_motrigb(found_crisis_num),ICB_crisis_data.get_datrigb(found_crisis_num)],
                     true, [ICB_crisis_data.get_yrtriga(found_crisis_num),ICB_crisis_data.get_motriga(found_crisis_num),ICB_crisis_data.get_datriga(found_crisis_num),
                            ICB_crisis_data.get_yrtrigb(found_crisis_num),ICB_crisis_data.get_motrigb(found_crisis_num),ICB_crisis_data.get_datrigb(found_crisis_num)],
                     1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                  if ICBStateEnd in user_selections.output_format.variables then
                  SetOutputVardata (outstring, ICBStateEnd,
                     false, [0],
                     true, [ICB_crisis_data.get_yrterma(found_crisis_num),ICB_crisis_data.get_moterma(found_crisis_num),ICB_crisis_data.get_daterma(found_crisis_num),
                            ICB_crisis_data.get_yrtermb(found_crisis_num),ICB_crisis_data.get_motermb(found_crisis_num),ICB_crisis_data.get_datermb(found_crisis_num)],
                     true, [ICB_crisis_data.get_yrterma(found_crisis_num),ICB_crisis_data.get_moterma(found_crisis_num),ICB_crisis_data.get_daterma(found_crisis_num),
                            ICB_crisis_data.get_yrtermb(found_crisis_num),ICB_crisis_data.get_motermb(found_crisis_num),ICB_crisis_data.get_datermb(found_crisis_num)],
                     1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                  if ICBActorSequence in user_selections.output_format.variables then
                  SetOutputVardata (outstring, ICBActorSequence,
                     false, [0],
                     true, [ICB_crisis_data.get_actnuma(found_crisis_num),ICB_crisis_data.get_actnumb(found_crisis_num)],
                     true, [ICB_crisis_data.get_actnuma(found_crisis_num),ICB_crisis_data.get_actnumb(found_crisis_num)],
                     1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                  if ICBCOWMember in user_selections.output_format.variables then
                  SetOutputVardata (outstring, ICBCOWMember,
                     false, [0],
                     true, [ICB_crisis_data.get_cowmema(found_crisis_num),ICB_crisis_data.get_cowmemb(found_crisis_num)],
                     true, [ICB_crisis_data.get_cowmema(found_crisis_num),ICB_crisis_data.get_cowmemb(found_crisis_num)],
                     1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                  if ICBGWMember in user_selections.output_format.variables then
                  SetOutputVardata (outstring, ICBGWMember,
                     false, [0],
                     true, [ICB_crisis_data.get_gwmema(found_crisis_num),ICB_crisis_data.get_gwmemb(found_crisis_num)],
                     true, [ICB_crisis_data.get_gwmema(found_crisis_num),ICB_crisis_data.get_gwmemb(found_crisis_num)],
                     1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                  if ICBIntraWar in user_selections.output_format.variables then
                  SetOutputVardata (outstring, ICBIntraWar,
                     false, [0],
                     true, [ICB_crisis_data.get_iwca(found_crisis_num),ICB_crisis_data.get_iwcb(found_crisis_num)],
                     true, [ICB_crisis_data.get_iwca(found_crisis_num),ICB_crisis_data.get_iwcb(found_crisis_num)],
                     1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                  if ICBJoiner in user_selections.output_format.variables then
                     begin
                       {Want to output 2 variable for "joined", one is a joined, one is b joined.
                        Also want two sets, for joined this crisis, vs. joined any crisis in year.
                        Currently very simple, just outputs whether they joined this crisis.}

                        if found_crisis_num = 0 then
                           begin
                              joina := false;
                              joinb := false;
                           end
                        else
                           begin
                              joina := not(ICB_crisis_data.Is_originator_in_crisis (ccode1, found_crisis_num));
                              joinb := not(ICB_crisis_data.Is_originator_in_crisis (ccode2, found_crisis_num));
                           end;
                        SetOutputVardata (outstring, ICBJoiner,
                           false, [0],
                           true, [joina, joinb],
                           true, [joina, joinb],
                           1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                     end;    {joiners in output}

                  if ICBNumCrises in user_selections.output_format.variables then
                  SetOutputVardata (outstring, ICBNumCrises,
                     false, [0],
                     true, [ICB_crisis_data.get_num_new_crises(ccode1, ccode2, curryear), ICB_crisis_data.get_num_total_crises(ccode1, ccode2, curryear)],
                     true, [ICB_crisis_data.get_num_new_crises(ccode1, ccode2, curryear), ICB_crisis_data.get_num_total_crises(ccode1, ccode2, curryear)],
                     1,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                  if ICBpeaceyrs in user_selections.output_format.variables then
                     SetOutputVardata (outstring, ICBpeaceyrs,
                        false, [0],
                        true, [ICB_Crisis_Data.get_peace_years(ccode1, ccode2, curryear, user_selections, werner_peace_years_data)],
                        true, [ICB_Crisis_Data.get_peace_years(ccode1, ccode2, curryear, user_selections, werner_peace_years_data)],
                        4,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);

                  if ICBpeacedys in user_selections.output_format.variables then
                     SetOutputVardata (outstring, ICBpeacedys,
                        false, [0],
                        true, [ICB_Crisis_Data.get_peace_days(ccode1, ccode2, curryear, user_selections, werner_peace_years_data)],
                        true, [ICB_Crisis_Data.get_peace_days(ccode1, ccode2, curryear, user_selections, werner_peace_years_data)],
                        5,0, curryear, configuration.first_ICB_year, configuration.last_ICB_year, missing_value);
                  end;   {if icb_crises in output}

            {Last output item:  user variables.}
            {User variables could be available for monadic, or dyadic data}

            if UserVariables in user_selections.output_format.variables then
               {go through each data set, for data sets where vars were selected,
                go through each var.  For each var, it may be available for monad
                or dyad, depending on the nature of the *var* itself, not the
                data set.}
               for adataset := 0 to length(user_selections.user_data_sets_selections)-1 do
                  if length (user_selections.user_data_sets_selections[adataset].data_set_variables) > 0 then
                     for user_var_num := 0 to length (user_selections.user_data_sets_selections[adataset].data_set_variables)-1 do {index is 0..n-1}
                        begin
                           {var to output number is user_selections.user_data_sets_selections[adataset].data_set_variables[user_var_num].
                           This number corresponds to the number in the configuration data set
                           description.  Set this in a var just for convenience.}
                           configuration_data_var_number := user_selections.user_data_sets_selections[adataset].data_set_variables[user_var_num];

                           {var info is in configuration.User_data_set_info.get_whatever (adataset,configuration_data_var_number)}
                           {var name is configuration.User_data_set_info.get_data_set_var_name(adataset,configuration_data_var_number)}
                           {name of the corresponding var from the data set is
                             User_Data_Sets.get_var_name(adataset, configuration_data_var_number);}
                           {data for that var will be
                             User_Data_Sets.get_value(adataset, configuration_data_var_number, ccode1, ccode2, curryear); }
                           {missing value is configuration.User_data_set_listing[CurrUserDataInfo.data_set_configuration_number].var_info[configuration_data_var_number].var_missing_value }

                           {Now output that variable}
                           {Need to set output format}
                           case configuration.User_data_set_info.get_data_set_var_type(adataset, configuration_data_var_number) of
                              varinteger:  begin
                                    outformat_totaldigits := 4;
                                    outformat_postdecimal := 0;
                                 end;
                              varsingle: begin
                                    outformat_totaldigits := 10;
                                    outformat_postdecimal := 8;
                                 end;
                              varolestr : begin
                                    outformat_totaldigits := 10;
                                    outformat_postdecimal := 0;
                                 end;
                              end;   {case}

                          {monadic output file (country-year) can't have either kind of dyadic data, but
                           this data should never be available.}
                           case configuration.User_data_set_info.get_data_set_var_unit(adataset,configuration_data_var_number) of
                              identifierversion : begin
                                 {version # can always be output, just one var.}
                                    user_output_value := User_Data_Sets.get_value(adataset, configuration_data_var_number, ccode1, ccode2, curryear, user_selections, dont_stop_on_error);
                                    SetOutputVardata (outstring, UserVariables,
                                       true, [user_output_value],
                                       true, [user_output_value],
                                       true, [user_output_value],
                                       outformat_totaldigits, outformat_postdecimal, curryear,
                                       configuration.User_data_set_info.get_data_set_first_year_possible(adataset),
                                       configuration.User_data_set_info.get_data_set_last_year_possible(adataset),
                                       configuration.User_data_set_info.get_data_set_var_missing_value(adataset, configuration_data_var_number));
                                 end;
                              annual : begin
                                 {if a given variable is annual data, then it can be
                                  output to all 3 types of output data sets.}
                                    {Use generic call for output values, even though some
                                     fields maybe irrelevant if the underlying data set
                                     structure is monadic.  Let the get procedure figure
                                     that out internally.}
                                    {call to get_value must be made with configuration_var_num;
                                     it is converted to the user/stored number internally.}
                                    {The internals of the get_value call will also check for errors,
                                     and matching/appropriate units of analysis.}
                                    user_output_value := User_Data_Sets.get_value(adataset, configuration_data_var_number, ccode1, ccode2, curryear, user_selections, dont_stop_on_error);
                                    SetOutputVardata (outstring, UserVariables,
                                       true, [user_output_value],
                                       true, [user_output_value],
                                       true, [user_output_value],
                                       outformat_totaldigits, outformat_postdecimal, curryear,
                                       configuration.User_data_set_info.get_data_set_first_year_possible(adataset),
                                       configuration.User_data_set_info.get_data_set_last_year_possible(adataset),
                                       configuration.User_data_set_info.get_data_set_var_missing_value(adataset, configuration_data_var_number));

                                 end;
                              monadic : begin
                                 {if a given variable is a monadic type, then it can be
                                  output to all 3 types of output data sets.}
                                 {But for dyadic output, we want to output 2 versions
                                  of the variable to have data on ccode1 and ccode2.}
                                 {Depending on where the variable came from, we
                                  may need to create this.}
                                    if configuration.User_data_set_info.get_data_set_unit(adataset) = country_year then
                                    {need to create and output both directions of the monadic variable.}
                                    SetOutputVardata (outstring, UserVariables,
                                       true, [User_Data_Sets.get_value(adataset, configuration_data_var_number, ccode1, ccode2, curryear, user_selections, dont_stop_on_error)],
                                       true, [User_Data_Sets.get_value(adataset, configuration_data_var_number, ccode1, 0, curryear, user_selections, dont_stop_on_error),
                                              User_Data_Sets.get_value(adataset, configuration_data_var_number, ccode2, 0, curryear, user_selections, dont_stop_on_error)],
                                       true, [User_Data_Sets.get_value(adataset, configuration_data_var_number, ccode1, 0, curryear, user_selections, dont_stop_on_error),
                                              User_Data_Sets.get_value(adataset, configuration_data_var_number, ccode2, 0, curryear, user_selections, dont_stop_on_error)],
                                       outformat_totaldigits, outformat_postdecimal, curryear,
                                       configuration.User_data_set_info.get_data_set_first_year_possible(adataset),
                                       configuration.User_data_set_info.get_data_set_last_year_possible(adataset),
                                       configuration.User_data_set_info.get_data_set_var_missing_value(adataset, configuration_data_var_number))
                                    else    {monadic data came from dyadic input, so assume the user has picked both directions.}
                                    SetOutputVardata (outstring, UserVariables,
                                       true, [User_Data_Sets.get_value(adataset, configuration_data_var_number, ccode1, ccode2, curryear, user_selections, dont_stop_on_error)],
                                       true, [User_Data_Sets.get_value(adataset, configuration_data_var_number, ccode1, ccode2, curryear, user_selections, dont_stop_on_error)],
                                       true, [User_Data_Sets.get_value(adataset, configuration_data_var_number, ccode1, ccode2, curryear, user_selections, dont_stop_on_error)],
                                       outformat_totaldigits, outformat_postdecimal, curryear,
                                       configuration.User_data_set_info.get_data_set_first_year_possible(adataset),
                                       configuration.User_data_set_info.get_data_set_last_year_possible(adataset),
                                       configuration.User_data_set_info.get_data_set_var_missing_value(adataset, configuration_data_var_number));
                                 end;
                              dyadic_ordered : begin
                                 {if a given variable is dyadic and ordered, then it can be
                                  can't be output to monadic.}
                                 {For nondirected output:
                                  If from non-directed input, just output it, because user will have
                                   provided a complementary variable, which gets the other direction
                                   (will be output on the next pass through the variables, since
                                    it's another variable).
                                  If from directed, ... , must output 2 variables here.
                                  Order is monadic, directed dyadic, nondirected dyadic.}
                                    user_output_value := User_Data_Sets.get_value(adataset, configuration_data_var_number, ccode1, ccode2, curryear, user_selections, dont_stop_on_error);
                                    if configuration.User_data_set_info.get_data_set_unit(adataset) = nondirected_dyad_year then
                                       {output just one variable for nondirected.}
                                       SetOutputVardata (outstring, UserVariables,
                                          false, [user_output_value],
                                          true, [user_output_value],
                                          true, [user_output_value],
                                          outformat_totaldigits, outformat_postdecimal, curryear,
                                          configuration.User_data_set_info.get_data_set_first_year_possible(adataset),
                                          configuration.User_data_set_info.get_data_set_last_year_possible(adataset),
                                          configuration.User_data_set_info.get_data_set_var_missing_value(adataset, configuration_data_var_number))
                                    else
                                       if configuration.User_data_set_info.get_data_set_unit(adataset) = directed_dyad_year then
                                       SetOutputVardata (outstring, UserVariables,
                                          false, [user_output_value],
                                          true, [user_output_value],
                                          true, [User_Data_Sets.get_value(adataset, configuration_data_var_number, ccode1, ccode2, curryear, user_selections, dont_stop_on_error),
                                                 User_Data_Sets.get_value(adataset, configuration_data_var_number, ccode2, ccode1, curryear, user_selections, dont_stop_on_error)],
                                          outformat_totaldigits, outformat_postdecimal, curryear,
                                          configuration.User_data_set_info.get_data_set_first_year_possible(adataset),
                                          configuration.User_data_set_info.get_data_set_last_year_possible(adataset),
                                          configuration.User_data_set_info.get_data_set_var_missing_value(adataset, configuration_data_var_number))
                                    else
                                       EUGeneError ('Error in variable output - outputting a dyadic_ordered variable for input data set type not directed or nondirected.  Programming error - notify programmer.',3,continue,error_log);
                                 end;
                              dyadic_unordered : begin
                                 {if a given variable is dyadic and unordered, then it
                                  can't be output to monadic, only}
                                    user_output_value := User_Data_Sets.get_value(adataset, configuration_data_var_number, ccode1, ccode2, curryear, user_selections, dont_stop_on_error);
                                    SetOutputVardata (outstring, UserVariables,
                                       false, [user_output_value],
                                       true, [user_output_value],
                                       true, [user_output_value],
                                       outformat_totaldigits, outformat_postdecimal, curryear,
                                       configuration.User_data_set_info.get_data_set_first_year_possible(adataset),
                                       configuration.User_data_set_info.get_data_set_last_year_possible(adataset),
                                       configuration.User_data_set_info.get_data_set_var_missing_value(adataset, configuration_data_var_number));
                                 end;
                              else EUGeneError ('Variable of inappropriate type seen in output procedure - notify programmer.', 3, stop, error_log);
                              end;   {case}

                        end;    {for a var;  also end of if user_variables.}



         {Now the line of text data is ready, so actually write it to the output location.}
            if user_selections.output_format.location=toscreen then
               outputWindow.Screen_Output.lines.add(outstring)
            else if user_selections.output_format.location=tofile then
               begin
                  {First, check for disk space.  there is a problem in diskfree with large
                   drives, it reports negative bytes.  So also check for < 0.}
                  if (DiskFree(0) > (2 * sizeof(outstring))) or (DiskFree(0) < 0) then
                  begin
                     {limdep can only handle 500 chars per line, so if outputting for limdep,
                      stop at 400 per line}
                     if (limdep in user_selections.output_format.commandFiles) and
                        (user_selections.output_format.location = tofile) and
                        (length (outstring) > 500)then
                        begin
                           first := 1;
                           last := 400;
                           repeat
                              for x := first to last do write (outfile, outstring[x]);
                              x := last;
                              if x < length(outstring) then
                                 repeat
                                    inc(x);
                                    if (x <= length(outstring)) then write (outfile, outstring[x]);
                                 until (outstring[x] = separator) or (x >= length(outstring));
                              writeln (outfile);
                              first := x + 1;
                              last := min (first + 400, length(outstring));
                           until first >= length(outstring);
                        end
                     else writeln (outfile, outstring);
                  end    {disk space OK}
                  else
                     raise (EInOutError.create ('Disk Full:  only '+IntToStr(DiskFree(0)) + ' bytes remaining.'));
                  begin
                  end;     {disk space problem}
               end
            else if user_selections.output_format.location=toprinter then
               writeln (outfile, outstring);
         except
             on EInOutError do
                begin
                   FileErrorBox.maindo ('Error opening/writing to main output file ',
                                        'File may be in use by another program, may be read-only, ',
                                        'or disk may be full.');
                   FileErrorBox.showmodal;
                   raise;
                end;
         end;
     end;


{ ------------------------------------------------------------ }

procedure output_data (var ccode_index : Tccode_index_obj; var nation_list : Tnation_array_obj;
                      var contiguity_data : Tcontiguity_array_obj; var configuration : configuration_type;
                      var user_selections : user_selection_type; selected_output_unit : output_type);
   {This is the unified, multi-unit of analysis output procedure that integrates code from the
    prior three units on dyadic, monadic, dispute-unit.}
   {Using list of variables as specified by user, output those variables to the
    output file}

   var
      {generic / all variables}
      outfile : text;   {this is the main flat output file}
      main_num_partitions, main_years_per_partition, main_partition_loop : integer;
      main_partition_start_year, main_partition_end_year, main_year_loop : year_range;
      Output_trace : Ttrace_obj;
      separator, outstring, outtypes: string;
      files_exist : boolean;
      fileoK : boolean;

      raw_capability_data : Traw_capability_array_obj;
      sys_capability_data : Tsys_capability_array_obj;
      distance_data : Tdistance_array_obj;
      minimum_distance_data : Tmindist_array_obj;
      risk_Tau_data : Trisk_attitude_array_obj;
      risk_S_data : Trisk_attitude_array_obj;
      tau_data : Ttau_array_obj;
      alliance_data : TAlliance_array_obj;
      s_data : Ts_array_obj;
      polity3_data : Tpolity_Array_obj;
      polnonpolnames : polnonpollisttype;
      EUWarTrap_Tau_data : TEUWarTrap_array_obj;
      EUWarTrap_S_data : TEUWarTrap_array_obj;
      EUWarReason_Tau_data : TEUWarReason_array_obj;
      EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data : TEUWarReason_array_obj;
      COW_dyadic_dispute_data_21 : TCOWDyadic_dispute_data_obj_format21;
      COW_dyadic_dispute_data_30 : TDyadic_dispute_data_obj_integrated_format30;
      Maoz_dispute_data : TDyadic_dispute_data_obj_integrated_format30; {used to be Maoz_Dyadic_dispute_data_obj;, but now I'm adding the maoz to the cow, so don't need a separate object.}
      ICB_crisis_data : TICBDyadic_dispute_data_obj;
      {The above to dispute data sets are for variable values.  The final generic set is for
       identifying the cases / dyad-years to be dropped, from any data set.}
      Conflict_Case_Exclusion_data : TGeneric_Dyadic_Conflict_data_obj;
      system_variables : Tsystem_variables_obj;
      werner_peace_years_data : TWernerPeaceYears_obj;
      ISO_array : TISO_array_obj;
      User_Data_Sets : Multiple_user_data_set_type;
      UserVarNamesAndMissingValuesList : varnameAndMissingValueArrayType;
      output_cases_planned, cases_output : longint;

      {Monad variables}
      main_ccode : ccode_range;  {These are just for the #s for dyads in main loop}

      {dyadic variables}
      wanted_dyad_list : Twanted_dyad_list_obj;
      dyads_done: boolean;
      main_ccode1, main_ccode2 : ccode_range;  {These are just for the #s for dyads in main loop}

      {dispute output variables}
      main_dispute_loop : longint;
      corresponding_cow_dispnum, corresponding_maoz_dispnum : longint;

      EndTime, StartTime, ElapsedTime : TDateTime;
      TimeFormat : TFormatSettings;

      {temporary:  x : integer;  }

           {  ---------------   }

   function num_country_years_wanted (const user_selections : user_selection_type) : longint;
      var region_counter: integer;
      begin
         case User_selections.monads_selected of
            all_states_mono :
                begin   {take # years as proportion of total years, multiply by total
                         # country years.  This will give an average country-years
                         expected.}
                   result := round ((user_selections.last_year - user_selections.first_year + 1)
                                / (configuration.last_any_year - configuration.first_any_year+1)
                                * (nation_list.get_country_years) );
                end;
            selected_set_mono :
                begin
                   result := (user_selections.last_year - user_selections.first_year + 1)
                              * user_selections.selected_country_list.num_countries;
                end;
            all_gp_mono :
                  begin    {This is years * avg # of GPs.}
                     result := 6 * (user_selections.last_year - user_selections.first_year + 1)
                  end;
            within_region_mono :
                  begin
                     region_counter := 0;
                     if europe in user_selections.selected_regions then region_counter := region_counter + 1;
                     if middleeast in user_selections.selected_regions then region_counter := region_counter + 1;
                     if africa in user_selections.selected_regions then region_counter := region_counter + 1;
                     if asia in user_selections.selected_regions then region_counter := region_counter + 1;
                     if americas in user_selections.selected_regions then region_counter := region_counter + 1;
                     if globe in user_selections.selected_regions then region_counter := 5;
                     if region_counter = 1 then region_counter := 2;  {Ensures we don't underestimate for a single region}
                     {We arbitrarily use an average of 40 states per region, which is a large overestimate for years < 1914}
                     result := (user_selections.last_year - user_selections.first_year + 1) * 40 * region_counter;
                  end;
            else
                 begin
                   EUGeneError ('Error - mono selection type not specified in function "num_country_years_wanted".  Notify Programmer',
                          5, stop, error_log);
                 end;
         end;   {case}
      end;    {function num_country_years_wanted}

   { ------------------------------------------------------------ }

   function want_in_monad_year (const ccode : ccode_range; const year : year_range;
         const user_selections : user_selection_type; const nation_list : Tnation_array_obj) : boolean;
      var ccodeloop : integer;
          found : boolean;
      begin
         result := false;
                   {for all, they must be states}
         if (nation_list.is_a_state (ccode, year)) then
            begin
              case User_selections.monads_selected of
               all_states_mono :
                begin   {it's a state, so it is OK}
                  result := true
                end;

               selected_set_mono :
                begin
                   found := false;
                   ccodeloop := 1;
                   while (ccodeloop <= user_selections.selected_country_list.num_countries) and
                         not (found) do
                     begin
                       if user_selections.selected_country_list.data[ccodeloop] = ccode then
                         begin
                           result := true;
                           found := true;
                         end;
                       inc(ccodeloop);
                     end;
                end;

               all_gp_mono :
                  begin    {check if both GPs}
                     if (nation_list.is_a_gp (ccode, year)) then
                     result := true;
                  end;

               within_region_mono :
                  begin
                    if nation_list.get_home_region(ccode) in user_selections.selected_regions then
                      result := true;
                    if globe in user_selections.selected_regions then
                      result := true;
                  end;

               else
               begin
                  EUGeneError ('Error - mono selection type not specified in function "want_in_year".  Notify Programmer',
                             5, continue, error_log);
               end;
              end;      {case monad criterion}
            end;    {if both are states}

      end;         {func want_monad_in_year}

   { ------------------------------------------------------------ }

   function want_in_dyad_year (const ccode1, ccode2 : ccode_range; const year : year_range;
         const user_selections : user_selection_type; const nation_list : Tnation_array_obj;
         const conflict_exclusion_data : TGeneric_Dyadic_Conflict_data_obj) : boolean;
      var disp_num_found : longint;
          want_states, want_casesubset, want_ongoing, want_TgtVsInit, want_joining : boolean;
          new_disp_ab, new_disp_ba, is_joiner : boolean;

          {This procedure evaluates a given input ccode-ccode year to see if it is appropriate
           for directed-dyad-year output.  NOT called by dispute-year procedure.}
      begin
         result := false;

         {there are several criteria that must be met to want the dyad-year}
         {Always want to check states, and case subset.  Then, if disputes are wanted, check the others}
         want_states := false;
         want_casesubset := false;
         want_ongoing := false;
         want_TgtVsInit := false;
         want_joining := false;

                   {for all, they must be states}
         if ( (nation_list.is_a_state (ccode1, year)) and
              (nation_list.is_a_state (ccode2, year)) ) then  want_states := true;

         {Since a fundamental criterion for EUGene is that they are both states, only bother
          with other checks if they are both states.}

         if want_states then
         begin
               {check to make sure within the selected subset of states}
            case user_selections.dyads_selected of
               all_states, selected_set:
                begin   {if all, just want it;  if selected, the selection is specified
                         before calling this procedure}
                  want_casesubset := true;
                end;
               all_gp_gp :
                  begin    {check if both GPs}
                     if (nation_list.is_a_gp (ccode1, year)) and
                        (nation_list.is_a_gp (ccode2, year)) then
                     want_casesubset := true;
                  end;
               all_contiguous :
                  begin
                     if contiguity_data.is_directly_contiguous (ccode1, ccode2, year, user_selections.contiguity_level_required) then
                     want_casesubset := true;
                  end;
               all_gp_any :
                  begin
                    if (nation_list.is_a_gp (ccode1, year)) or
                       (nation_list.is_a_gp (ccode2, year)) then
                    want_casesubset := true;
                  end;
               politically_relevant :
                  begin
                    if is_politically_relevant (ccode1, ccode2, year) then
                    want_casesubset := true;
                  end;
               within_distance :
                  {Note: unlike other want_in_year calls, this option has not had irrelevant dyads
                   weeded out in the creation of wanted_dyad_list.}
                  begin
                    if distance_data.get_distance (ccode1, ccode2, year)
                       <= user_selections.maximum_distance then
                    want_casesubset := true;
                  end;
               within_region :
                  {Should be redundant, since wanted_dyad_list has already removed all non-qualifying dyads.}
                  begin
                    if nation_list.get_home_region(ccode1) in user_selections.selected_regions then
                      if nation_list.get_home_region(ccode2) in user_selections.selected_regions then
                      want_casesubset := true;
                    if globe in user_selections.selected_regions then
                    want_casesubset := true;
                  end;
               user_file_read :
                  begin
                    if want_user_dyad_year (user_selections, ccode1, ccode2, year) then
                    want_casesubset := true;
                  end;
               else                    {case}
                  begin
                     EUGeneError ('Error - dyad selection type not seen in function "want_in_year".  Notify Programmer',
                                5, continue, error_log);
                  end;
            end;       {case;  this is a wanted with case subsetting}


            {Now want to check the last three conditions iff user has selected MIDs,
             crises, or wars for exclusions.}
            if user_selections.conflict_exclusion_selection <> cds_none then
               begin

                  {First, create a marker for if there is a new dispute I want to think about}
                  case user_selections.dispute_info.MarkSubsequentAsInitiation of
                     true:
                        case user_selections.output_this of
                           output_directed_dyads : new_disp_ab := conflict_exclusion_data.wanted_new_or_continuing_initiation (ccode1, ccode2, year, user_selections, disp_num_found, use_ccodeyear);
                           output_nondirected_dyads : new_disp_ab := conflict_exclusion_data.wanted_new_or_continuing_nondir_dispute (ccode1, ccode2, year, user_selections, disp_num_found, use_ccodeyear);
                        end;   {case}
                     false:
                        case user_selections.output_this of
                           output_directed_dyads : new_disp_ab := conflict_exclusion_data.wanted_new_initiation (ccode1, ccode2, year, user_selections, disp_num_found, use_ccodeyear);
                           output_nondirected_dyads : new_disp_ab := conflict_exclusion_data.wanted_new_nondir_dispute (ccode1, ccode2, year, user_selections, disp_num_found, use_ccodeyear);
                        end;   {case}
                  end;  {case}


                  {1.  check ongoing disputes}
                  {Want to print this year if not ongoing, or if is ongoing but want ongoing, or
                   if is ongoing and want ongoin if new disp and there is a new disp.}
                  {don't want it if don't want any ongoing, and it's ongoing.
                   Also don't want it if would take new disps, but it's not a new disp I want}
                  if ((not (conflict_exclusion_data.is_ongoing(ccode1, ccode2, year, disp_num_found)))
                       or
                      ((conflict_exclusion_data.is_ongoing(ccode1, ccode2, year, disp_num_found)) and
                       (user_selections.output_format.printAllOngoing=true))
                       or
                      ((conflict_exclusion_data.is_ongoing(ccode1, ccode2, year, disp_num_found)) and
                       (user_selections.output_format.printOngoingIfNewDisp) and
                       (new_disp_ab))
                       {note:  if want to treat ongoing as new initiations, then the include dyad
                        iff new disp will basically include every ongoing year.  }
                     )   then {want this dyad year}
                     want_ongoing := true;


                  {2.  Now specify conditions for reverse (tgt vs. init) dyad;  this is case where
                   what if A inits vs. B, what to do with B vs. A}
                  {First, if outputting non-directed, then don't care about this condition.}
                  case user_selections.output_this of
                     output_nondirected_dyads : want_TgtVsInit := true;
                     output_directed_dyads :
                        begin
                           {Normally want this dyad if its a non-initiation, and opposite had no initiation;
                            or if an initiation (then don't care about reverse, whether it is or isn't an
                            initiation, I want this direction).  Then, depending on user setting
                            for including all, may or may not want it if there is no inititation but other
                            side had initation.}
                           {Note that the "wanted_new_initation" etc. procedure includes checks for
                              true initiators and initiators vs. joiners.  So if we want joiners then
                              this proc will output true for a joiner in the right year.  }
                           case user_selections.dispute_info.MarkSubsequentAsInitiation of
                              true: new_disp_ba := conflict_exclusion_data.wanted_new_or_continuing_initiation (ccode2, ccode1, year, user_selections, disp_num_found, use_ccodeyear);
                              false: new_disp_ba := conflict_exclusion_data.wanted_new_initiation (ccode2, ccode1, year, user_selections, disp_num_found, use_ccodeyear);
                           end;  {case}
                           {new_disp_ab is set previously}

                           {first, if this direction is a new dispute/crisis/war, want it.}
                           if (new_disp_ab) then want_TgtVsInit := true
                           {second, if neither has a disp, want this direction.  }
                              else if (not(new_disp_ab) and not(new_disp_ba)) then want_TgtVsInit := true
                                {Third, ab is not a dispute, but there was a ba dispute, so ab will print
                                 only if user has selected to always include reverse.}
                                else if (not (new_disp_ab)) and (new_disp_ba) then
                                   begin
                                      if (user_selections.dispute_info.AlwaysIncludeTgtVsInitiator = true) then want_TgtVsInit := true;
                                   end
                                   {should never get to the 4th else, it shouldn't be possible logically}
                                  else
                                     EUGeneError ('Error - Want in year, checking for tgt vs. init dyads reaches logically impossible condition.  Notify Programmer of this EXACT message.',
                                               5, continue, error_log);
                        end;   {directed dyads}
                  end;   {case output_this of nondir or directed_dyads}


                  {3.  check if this is a joiner dyad, and if I want to print joiner dyad-years.}
                  {don't want it if don't want any joiners, and it's a joiner.}
                  {Want to print this year if not a joiner,
                   or if is joiner but not dropping joiners.  Drop if any year of a joining.}
                  is_joiner := false;
                  case user_selections.output_this of
                     output_nondirected_dyads :
                         is_joiner := conflict_exclusion_data.Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, disp_num_found, use_ccodeyear);
                     output_directed_dyads :
                        case user_selections.conflict_exclusion_selection of
                           cds_COWMID, cds_MaozMID :
                              case user_selections.dispute_info.SideAIsInitiator of
                               true:      {sideA}
                                  is_joiner := (conflict_exclusion_data.Is_AnyYear_Joined_Initiation (ccode1, ccode2, year, disp_num_found, use_ccodeyear) or
                                                conflict_exclusion_data.Is_AnyYear_Joined_Targets (ccode1, ccode2, year, disp_num_found, use_ccodeyear));
                               false:    {Revisionist}
                                  is_joiner := (conflict_exclusion_data.Is_AnyYear_Joined_Revision (ccode1, ccode2, year, disp_num_found, use_ccodeyear) or
                                                conflict_exclusion_data.Is_AnyYear_Joined_SQ (ccode1, ccode2, year, disp_num_found, use_ccodeyear));
                              end;   {case sideA is initiator}
                           cds_ICB : {ICB doesn't have initiation, so the checks for sideA/revisionist are unncecessary.}
                                  is_joiner := (conflict_exclusion_data.Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, disp_num_found, use_ccodeyear));
                           cds_COWWar : begin end;     {not yet implemented}
                           cds_none : EUGeneError ('Reached place in program shouldnt have - in want_in_year cds_none under 3rd section. notify programmer.',1,stop,error_log);
                        end;      {case exclusion_selection of...}
                  end;    {case output_this of...}

                  if ((not (is_joiner))
                       or
                      ((is_joiner) and (user_selections.dispute_info.DropJoinerDirectedDyads=false))
                     )   then {want to print this dyad year}
                         {this leaves dropping for if is_joiner and DropJoinerDirectedDyads=true.}
                     want_joining := true;

               end   {if exclusions not none}

            else     {no exclusions, so those criteria always true}
               begin
                  want_ongoing := true;
                  want_TgtVsInit := true;
                  want_joining := true;
               end;

            end;       {if want_states overall condition}


         {finally, only if all conditions are satisfied is the dyad-year one we want to include.}
         if (want_states and want_ongoing and want_casesubset and
             want_TgtVsInit and want_joining) = true then
            result := true;

      end;         {func want_in_year}

   { ------------------------------------------------------------ }

   function dyad_sampled (user_selections : user_selection_type; specific_disp_num : longint;
                     ccode1, ccode2 : ccode_range; ayear : year_range; use_param : dyadic_call_parameter;
                     const conflict_exclusion_data : TGeneric_Dyadic_Conflict_data_obj) : boolean;
     {returns true if we want a given data point.  This will be true if we are not sampling,
      or if we are sampling and we meet the right random test.}
     {It is OK here to assume that we valid conflict exclusion data, because check_completeness
      makes them select a type of MID.}
      var disp_num_internal : longint;
          init : boolean;
      begin
         disp_num_internal := specific_disp_num;
         case user_selections.sample_info.sampling of
            true : begin
                   {might want this data point.  First see if it's a dispute.}
                  {first, check and see if there is a dispute this dyad year}
                  if (ayear >= configuration.first_MID_year) and
                     (ayear <= configuration.last_MID_year) then
                     begin
                        case user_selections.output_this of
                           output_directed_dyads : case user_selections.dispute_info.MarkSubsequentAsInitiation of
                                 true: init := conflict_exclusion_data.wanted_new_or_continuing_initiation (ccode1, ccode2, ayear, user_selections, disp_num_internal, use_param);
                                 false: init := conflict_exclusion_data.wanted_new_initiation (ccode1, ccode2, ayear, user_selections, disp_num_internal, use_param);
                              end;
                           output_nondirected_dyads : case user_selections.dispute_info.MarkSubsequentAsInitiation of
                                 true: init := conflict_exclusion_data.wanted_new_or_continuing_NonDir_dispute (ccode1, ccode2, ayear, user_selections, disp_num_internal, use_param);
                                 false: init := conflict_exclusion_data.wanted_new_NonDir_dispute (ccode1, ccode2, ayear, user_selections, disp_num_internal, use_param);
                              end;
                           end;    {case usere .output_this of}
                     end
                     else init := false;
                  if (init) then
                     begin
                         if random <= user_selections.sample_info.proportion_dispute_dyads then
                           result := true
                        else result := false;
                     end
                  else  {this is a non-dispute dyad}
                     begin
                        if random <= user_selections.sample_info.proportion_non_dispute_dyads then
                           result := true
                        else result := false;
                     end;
               end;        {case sampling of true}
            false : result := true;
          end;  {case}
      end;

   { --------------------------------------------------}

   function want_in_dispute_initiation_year (const ccode1, ccode2 : ccode_range; const year : year_range;
         const user_selections : user_selection_type; const nation_list : Tnation_array_obj;
         const input_disp_num : longint;
         const conflict_exclusion_data : TGeneric_Dyadic_Conflict_data_obj) : boolean;
      var is_joiner : boolean;
          disp_num_internal : longint;

       {This procedure evaluates a given input dyadic dispute-year to see if it is appropriate
        for disp-year output.  NOT called by directed-dyad-year procedure.}

      begin
         {Don't have to check all the same things as in the dyad-year check function.
          There is not the same case subsetting option.
          Also don't care about target direction in this procedure,
             b/c it's only initiations we're concerned about.
          Concerns about keep ongoing disputes, or not, handled via user choosing one case
             per dispute initiation, or one case per dispute year.}

          {Although they should be states to be in the COW MID list, there are some cases
           where there are mids with non-system members.  So I do need to check state status.
           Do that after the initial check of whether this is an initiation or not.}

         result := false;
         disp_num_internal := input_disp_num;

         {1. Check new initiations.
             The wanted_new_... functions check whether the user wants joiners
             as initiators, or only true initiators.}
         if ((user_selections.disputes_selected = all_disputes) and
             (conflict_exclusion_data.wanted_new_initiation(ccode1, ccode2, year, user_selections, disp_num_internal, use_dispute) ))
             or
            ((user_selections.disputes_selected = all_dispute_years) and
             (conflict_exclusion_data.wanted_new_or_continuing_initiation(ccode1, ccode2, year, user_selections, disp_num_internal, use_dispute) ))
            then result := true;

         {Include joiners if specified that joiners=initiators.
          Note that the formation of the dyadic dispute list only sets up the
          initiating side vs. the target side as initiations.  So joiners can include
          only those joiners that joined the initiating side;  that is checked
          in the procedures above.}
         {Also need to check if want to drop all joiner dyads, in which case it
          takes precedence and we drop it.}

         {2. Check if this is a joiner dyad, and verify whether I want to print joiner dyad-years.}
         {don't want it if don't want any joiners, and it's a joiner.}
         is_joiner := false;
         case user_selections.dispute_info.SideAIsInitiator of
             true:      {sideA}
                is_joiner := (conflict_exclusion_data.Is_AnyYear_Joined_Initiation (ccode1, ccode2, year, disp_num_internal, use_dispute) or
                              conflict_exclusion_data.Is_AnyYear_Joined_Targets (ccode1, ccode2, year, disp_num_internal, use_dispute));
             false:    {Revisionist}
                is_joiner := (conflict_exclusion_data.Is_AnyYear_Joined_Revision (ccode1, ccode2, year, disp_num_internal, use_dispute) or
                              conflict_exclusion_data.Is_AnyYear_Joined_SQ (ccode1, ccode2, year, disp_num_internal, use_dispute));
            end;   {case sideA is initiator}
         if (is_joiner) and (user_selections.dispute_info.DropJoinerDirectedDyads)
            then {want to drop this dispute year }
            result := false;

         {3.  Finally, ReCheck status as states}
         if not ( (nation_list.is_a_state (ccode1, year)) and
                  (nation_list.is_a_state (ccode2, year)) ) then result := false;

      end;  {func want_in_dispute_initiation_year}


      function want_in_nondirected_dispute_year (const ccode1, ccode2 : ccode_range; const year : year_range;
         const user_selections : user_selection_type; const nation_list : Tnation_array_obj;
         const input_disp_num : longint;
         const conflict_exclusion_data : TGeneric_Dyadic_Conflict_data_obj) : boolean;
      var is_joiner : boolean;
          disp_num_internal : longint;

       {This procedure evaluates a given input dyadic dispute-year to see if it is appropriate
        for nondirected disp-year output.  NOT called by directed-dyad-year procedure.}

      begin
         {Don't have to check all the same things as in the dyad-year check function.
          There is not the same case subsetting option.
          Also don't care about target direction in this procedure,
             b/c it's only initiations we're concerned about.
          Concerns about keep ongoing disputes, or not, handled via user choosing one case
             per dispute initiation, or one case per dispute year.}

          {Although they should be states to be in the COW MID list, there are some cases
           where there are mids with non-system members.  So I do need to check state status.
           Do that after the initial check of whether this is an initiation or not.}

         result := false;
         disp_num_internal := input_disp_num;

         {1. Check new initiations.
             The wanted_new_... functions check whether the user wants joiners
             as initiators, or only true initiators.}
         {if ((user_selections.disputes_selected = all_disputes) and
             (conflict_exclusion_data.wanted_new_initiation(ccode1, ccode2, year, user_selections, disp_num_internal, use_dispute) ))
             or
            ((user_selections.disputes_selected = all_dispute_years) and
             (conflict_exclusion_data.wanted_new_or_continuing_initiation(ccode1, ccode2, year, user_selections, disp_num_internal, use_dispute) )
            then result := true;   }

         if ((user_selections.disputes_selected = all_disputes) and
             (conflict_exclusion_data.wanted_new_nonDir_Dispute(ccode1, ccode2, year, user_selections, disp_num_internal, use_dispute) ))
             or
            ((user_selections.disputes_selected = all_dispute_years) and
             (conflict_exclusion_data.wanted_new_or_continuing_nondir_dispute(ccode1, ccode2, year, user_selections, disp_num_internal, use_dispute) )   )
            then result := true;

         {Include joiners if specified that joiners=initiators.
          Note that the formation of the dyadic dispute list only sets up the
          initiating side vs. the target side as initiations.  So joiners can include
          only those joiners that joined the initiating side;  that is checked
          in the procedures above.}
         {Also need to check if want to drop all joiner dyads, in which case it
          takes precedence and we drop it.}

         {2. Check if this is a joiner dyad, and verify whether I want to print joiner dyad-years.}
         {don't want it if don't want any joiners, and it's a joiner.}
         is_joiner := false;
         case user_selections.dispute_info.SideAIsInitiator of
             true:      {sideA}
                is_joiner := (conflict_exclusion_data.Is_AnyYear_Joined_Initiation (ccode1, ccode2, year, disp_num_internal, use_dispute) or
                              conflict_exclusion_data.Is_AnyYear_Joined_Targets (ccode1, ccode2, year, disp_num_internal, use_dispute));
             false:    {Revisionist}
                is_joiner := (conflict_exclusion_data.Is_AnyYear_Joined_Revision (ccode1, ccode2, year, disp_num_internal, use_dispute) or
                              conflict_exclusion_data.Is_AnyYear_Joined_SQ (ccode1, ccode2, year, disp_num_internal, use_dispute));
            end;   {case sideA is initiator}
         if (is_joiner) and (user_selections.dispute_info.DropJoinerDirectedDyads)
            then {want to drop this dispute year }
            result := false;

         {3.  Finally, ReCheck status as states}
         if not ( (nation_list.is_a_state (ccode1, year)) and
                  (nation_list.is_a_state (ccode2, year)) ) then result := false;

      end;  {func want_in_nondirected_dispute_year}


      { ------------------------------------------------------------ }

   procedure open_output (user_selections: user_selection_type; var outfile: text);
   {This proc assigns the appropriate output file to the var "outfile", and opens
    that file for writing.  }
   {This referst to the print dialog declared in the main window, Frame}
   var OutputPrintDialog: TPrintDialog;

      begin
         trace.enter('Entered open_output procedure');
         {AT this point, this is a new file, or user has said OK to overwrite.}
         if user_selections.output_format.location = tofile then
            begin
               assignFile (outfile, user_selections.output_format.output_file_name);
                  try
                     rewrite (outfile);
                  except
                     on EInOutError do
                       begin
                          FileErrorBox.maindo ('Error opening file "'+user_selections.output_format.output_file_name + '"',
                                               'File could not be opened for output.',
                                               'File may be in use by another program, or is read-only.' );
                          FileErrorBox.showmodal;
                          raise;
                       end;
                  end;     {except}
            end
         else
            if user_selections.output_format.location = toprinter then
            begin
               try
                  OutputPrintDialog:= TPrintDialog.create(outputwindow);
                  OutputPrintDialog.Options := [poHelp, poWarning, poPrintToFile];
                  if OutputPrintDialog.Execute then
                  begin
                     AssignPrn(outfile);
                     try
                        Rewrite(outfile);	{create and open the output file}
                     except
                        on EInOutError do
                          begin
                             FileErrorBox.maindo ('Error accessing printer.',
                                                  'Printer could not be opened for output.',
                                                  'Please check printer' );
                             FileErrorBox.showmodal;
                             raise;
                          end;
                     end;
                  end;
               finally
                  OutputPrintDialog.Free;
               end;
            end
         else
            begin
                  {going to screen}
               OutputWindow.Screen_Output.clear;
               OutputWindow.show;
            end;

         trace.exit ('Finished open output procedure');
      end;       {open output}

 {  -------------------------  }

   procedure close_output (user_selections: user_selection_type; var outfile: text);
   {This proc closes the output file for writing.  Assumes file is open and OK}
      begin
         trace.enter('Entered close output procedure');
         if user_selections.output_format.location = tofile then
            begin
               try
                 CloseFile (outfile)
               except       {in this except, I've potentially raised a new I/O error}
               on EInOutError do
                 begin
                    FileErrorBox.maindo ('Error closing file "'+user_selections.output_format.output_file_name + '"',
                                         'File could not be closed successfully.',
                                         '(File may never have opened properly.)' );
                    FileErrorBox.showmodal;
                    raise;
                 end;
               end;  {inner except}
            end
         else if user_selections.output_format.location = toprinter then
            begin
               try
                  CloseFile(outfile)
               except       {in this except, I've potentially raised a new I/O error}
               on EInOutError do
                 begin
                    FileErrorBox.maindo ('Error accessing printer.',
                                         'Printer could not be closed after output.',
                                         'Please notify programmer of this error message.' );
                    FileErrorBox.showmodal;
                    raise;
                 end;
               end;  {inner except}
            end
         else
               begin
                     {going to screen, do nothing to close the window, user can close it.}
               end;

         trace.exit ('Finished close output procedure');
      end;



   { ------------------------------------------------------------ }

   begin      {main proc output_data}

      try
         trace.enter ('Entered main output procedure - type '+ inttostr(ord(selected_output_unit)));
         StartTime := now;
         raw_capability_data:= nil;
         sys_capability_data:= nil;
         distance_data := nil;
         minimum_distance_data := nil;
         tau_data := nil;
         alliance_data := nil;
         s_data := nil;
         polity3_data := nil;
         risk_Tau_data := nil;
         risk_S_data := nil;
         EUWarTrap_Tau_data := nil;
         EUWarTrap_S_data := nil;
         EUWarReason_Tau_data := nil;
         EUWarReason_S_unweighted_data := nil;
         EUWarReason_S_weighted_data := nil;
         COW_dyadic_dispute_data_21 := nil;
         COW_dyadic_dispute_data_30 := nil;
         Maoz_dispute_data := nil;
         ICB_crisis_data := nil;
         Conflict_Case_Exclusion_data := nil;
         User_Data_Sets := nil;
         system_variables := nil;
         wanted_dyad_list := nil;
         werner_peace_years_data := nil;

         preprocess_for_output (configuration, user_selections, files_exist);
         if files_exist then
         begin

              Output_trace := Ttrace_obj.init(trace.get_trace_level);

              {System level variables only have to be created once, so they are outside "read data" proc.}

              if (systemchars in user_selections.output_format.variables) then
              begin
                 sys_capability_data := Tsys_capability_array_obj.init
                    (configuration.cow_system_pct_file_name,
                     configuration.first_cap_year,
                     configuration.last_cap_year);
                 system_variables := Tsystem_variables_obj.init(sys_capability_data);
              end;

              {There are a couple of structures I need only if it's dyadic output.}
              if ((selected_output_unit = output_directed_dyads) or
                  (selected_output_unit = output_nondirected_dyads) or
                  (selected_output_unit = output_directed_dispute_initiation_dyads) or
                   (selected_output_unit = output_nondirected_dispute_dyads) )
                 then
                 begin
                    {distance array only has to be read once, so it is outside "read data" proc}
                    {But, minimum distance must be read inside each partition}
                    if ((distance in user_selections.output_format.variables) and
                        (user_selections.distance_method <> minimum)) or
                       (user_selections.dyads_selected = within_distance) then
                        distance_data := Tdistance_array_obj.init (configuration.distance_file_name, user_selections, contiguity_data);

                    {dyadic dispute array only has to be read once, so it is outside "read data" proc.}
                     {Must read dyadic disputes, either to output dispute data or to verify ongoing/not years}
                     {Note: COW_dyadic_dispute_data are read for all years, b/c there is no savings for
                      only reading subyears.  Also, dyadic disputes can be used for directed and
                      nondirected output.  Data is directed, but any direction can be recovered.}
                     {If the user has specifically selected Maoz_disputes, then use that, otherwise
                      use base set of dyadic disputes.}
                    {First, always read the v3.0 data.  Then append either the pre-1992 cow or maoz
                     data.  }
                    {if (Maoz_dyadic_disputes in user_selections.output_format.variables) or
                        (COW_disputes in user_selections.output_format.variables) then
                       COW_dyadic_dispute_data_30 := TDyadic_dispute_data_obj_integrated_format30.init (user_selections, configuration,
                                                  configuration.first_MID_year, configuration.last_MID_year);  }
                    if Maoz_dyadic_disputes in user_selections.output_format.variables then
                       begin
                          Maoz_dispute_data := TDyadic_dispute_data_obj_integrated_format30.init (user_selections, configuration,
                                                  configuration.first_MID_year, configuration.last_MID_year, maoz_pre_1992);
{trace.message ('Checking maoz disps after full initialize');
trace.message (inttostr(maoz_dispute_data.get_last_dispnum )+' disputes in structure after full initialize');
for x := 0 to maoz_dispute_data.get_last_dispnum do
   begin
      if (maoz_dispute_data.get_first_year(x) = 1994) and (maoz_dispute_data.get_ccode(x,0)=710) and (maoz_dispute_data.get_ccode(x,1)=2) then trace.message ('Saw requested rec in maoz as record'+inttostr(x));
      if (maoz_dispute_data.get_ccode(x,0)=710) and (maoz_dispute_data.get_ccode(x,1)=2) then trace.message ('Saw rec in maoz as record '+inttostr(x)+' with year '+inttostr(maoz_dispute_data.get_first_year(x)) + ' and end year ' + inttostr(maoz_dispute_data.get_first_year(x) ) );
   end;  }
                       end;
                    if COW_disputes in user_selections.output_format.variables then
                       begin
                          COW_dyadic_dispute_data_30 := TDyadic_dispute_data_obj_integrated_format30.init (user_selections, configuration,
                                                  configuration.first_MID_year, configuration.last_MID_year, cow_pre_1992);
{trace.message('checking cow');
trace.message (inttostr(COW_dyadic_dispute_data_30.get_last_dispnum )+' disputes in structure');
for x := 0 to COW_dyadic_dispute_data_30.get_last_dispnum do
   begin
      if (COW_dyadic_dispute_data_30.get_first_year(x) = 1994) and (COW_dyadic_dispute_data_30.get_ccode(x,0)=710) and (COW_dyadic_dispute_data_30.get_ccode(x,1)=2) then trace.message ('Saw requested rec in cow as record'+inttostr(x));
      if (COW_dyadic_dispute_data_30.get_ccode(x,0)=710) and (COW_dyadic_dispute_data_30.get_ccode(x,1)=2) then trace.message ('Saw rec in cow as record '+inttostr(x)+' with year '+inttostr((COW_dyadic_dispute_data_30.get_first_year(x)) ) );
   end;  }
                       end;
                    if ICB_crises in user_selections.output_format.variables then
                       ICB_crisis_data := TICBDyadic_dispute_data_obj.init (user_selections, configuration,
                                          configuration.first_ICB_year, configuration.last_ICB_year);


                    {for the dyadic output, the user may be selecting particular dyads based
                     on values of conflict.  For this structure, they will have just one
                     "exclusion" data set selected, whether MID, war, ICB...}
                    {Need to set this up differently if they are doing dyad years, or
                     dyad disputes.}

                    {for directed, nondirected dyad years, they might or might not specify
                     an exclusion set.  If they don't, then want in year will not drop any
                     cases through the exclusion criteria.}
                    {for directed_dispute units, we MUST have some data in the Conflict_Case_Exclusion_data
                     structure, because we always output just one set of dispute data.  This
                     is handled in check_completeness by changing the exclusion setting to match the
                     type of dispute data selected.}


                    case user_selections.conflict_exclusion_selection of
                          cds_MaozMID : begin
                                Conflict_Case_Exclusion_data := TDyadic_dispute_data_obj_integrated_format30.init (user_selections, configuration,
                                  configuration.first_MID_year, configuration.last_MID_year, maoz_pre_1992);
                             end;
                          cds_COWMID : begin
                                Conflict_Case_Exclusion_data := TDyadic_dispute_data_obj_integrated_format30.init (user_selections, configuration,
                                   configuration.first_MID_year, configuration.last_MID_year, cow_pre_1992);
                             end;
                          cds_ICB : Conflict_Case_Exclusion_data :=  TICBDyadic_dispute_data_obj.init (user_selections, configuration,
                               configuration.first_ICB_year, configuration.last_ICB_year);
                          cds_COWWar : showmessage ('COW War choice not yet implemented');
                          cds_none : Conflict_Case_Exclusion_data := nil;
                          else EUGeneError ('Type for exclusion in user_selections doesnt match programmed type in euinoutd.  Notify programmer of fatal error.',1,stop, error_log);
                       end;              {case}


                    {wanted_dyad_list is helper list of dyads user wants to output.}
                    if ((selected_output_unit = output_directed_dyads) or
                        (selected_output_unit = output_nondirected_dyads))
                    then
                       begin
                          wanted_dyad_list := Twanted_dyad_list_obj.init;
                          wanted_dyad_list.update (user_selections);           {updated for dir/nondir}
                       end;
                 end;      {if dyadic or dispute output}

            calculate_output_partitions (user_selections, main_num_partitions, main_years_per_partition);
            main_partition_start_year := user_selections.first_year;
            cases_output := 0;

            {calculate estimated number of cases to output, for purposes of displaying a loop count.}
            case selected_output_unit of
               output_directed_dyads, output_nondirected_dyads :
                  output_cases_planned := wanted_dyad_list.get_num_dyad_years;
               output_monads : output_cases_planned := num_country_years_wanted (user_selections);
               output_directed_dispute_initiation_dyads :
                  if Maoz_dyadic_disputes in user_selections.output_format.variables then
                        output_cases_planned := (main_num_partitions * Maoz_dispute_data.get_last_dispnum)
                     else if COW_disputes in user_selections.output_format.variables then
                        output_cases_planned := (main_num_partitions * COW_dyadic_dispute_data_30.get_last_dispnum)
                     else if ICB_Crises in user_selections.output_format.variables then
                        output_cases_planned := (main_num_partitions * ICB_crisis_data.get_last_dispnum);
               output_nondirected_dispute_dyads :
                  if Maoz_dyadic_disputes in user_selections.output_format.variables then
                        output_cases_planned := (main_num_partitions * Maoz_dispute_data.get_last_dispnum)
                     else if COW_disputes in user_selections.output_format.variables then
                        output_cases_planned := (main_num_partitions * COW_dyadic_dispute_data_30.get_last_dispnum)
                     else if ICB_Crises in user_selections.output_format.variables then
                        output_cases_planned := (main_num_partitions * ICB_crisis_data.get_last_dispnum)
               else EUGeneError ('Reached output_data procedure with an output unit not properly specified.',1,stop,error_log);
               end;     {case}


            try
              set_header (configuration, user_selections, separator, outstring, outtypes, UserVarNamesAndMissingValuesList, polnonpolnames);
              if not (CommandFilesOnly in user_selections.output_format.commandfiles) then
              begin
                 try
                    open_output (user_selections, outfile);
                    if (user_selections.output_format.header = true) then
                      write_header (outfile, user_selections, outstring);
                    for main_partition_loop := 1 to main_num_partitions do
                       begin
                          main_partition_end_year := min(main_partition_start_year + main_years_per_partition,
                                                       user_selections.last_year);
                          try  {read data and loop through}
                             Output_trace.tick('Executing Procedure: reading data for output, '+inttostr(main_partition_start_year)+' to '+inttostr(main_partition_end_year), 0);
                             Read_data (main_partition_start_year, main_partition_end_year,
                                      tau_data, alliance_data, s_data, polity3_data, sys_capability_data, raw_capability_data,
                                      risk_Tau_data, risk_S_data, EUWarTrap_Tau_data, EUWarTrap_S_data,
                                      EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                      minimum_distance_data, werner_peace_years_data, ISO_array, User_Data_Sets, user_selections, configuration);

                              case selected_output_unit of
                                 output_directed_dyads, output_nondirected_dyads :  {do dyadic loop}
                                    For main_year_loop := main_partition_start_year to main_partition_end_year do
                                       begin
                                          {For nondirected, the wanted_dyad_list constructed only cc1<cc2 dyads.
                                           For directed, both directsion are there.}
                                          dyads_done := false;
                                          wanted_dyad_list.get_first_dyad (main_ccode1, main_ccode2, dyads_done);
                                          if not(dyads_done) then
                                          repeat
                                             if want_in_dyad_year (main_ccode1, main_ccode2, main_year_loop,
                                                              user_selections, nation_list, Conflict_Case_Exclusion_data) then
                                               if dyad_sampled (user_selections, 0, main_ccode1,
                                                           main_ccode2, main_year_loop, use_ccodeyear, Conflict_Case_Exclusion_data) then
                                                begin
                                                  output_one_record(outfile, main_ccode1, main_ccode2, main_year_loop,
                                                          distance_data, minimum_distance_data, tau_data, alliance_data, s_data, polity3_data, sys_capability_data,
                                                          raw_capability_data, risk_Tau_data, risk_S_data, EUWarTrap_Tau_data, EUWarTrap_S_data,
                                                          EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                                          werner_peace_years_data, ISO_array, User_Data_Sets, COW_dyadic_dispute_data_21, COW_dyadic_dispute_data_30, Maoz_dispute_data,
                                                          ICB_crisis_data, system_variables,
                                                          configuration, user_selections,
                                                          use_ccodeyear, null_dispute_number, null_dispute_number, null_dispute_number);
                                                  Output_trace.tick('Executing Procedure: Write final output ('+inttostr(main_year_loop)+')',
                                                     wanted_dyad_list.get_num_dyad_years);
                                                  inc(cases_output);
                                                end;
                                             wanted_dyad_list.get_next_dyad (main_ccode1, main_ccode2, dyads_done);
                                          until dyads_done;
                                       end;   {for main_year_loop}

                                 output_monads :  {do monadic loop}
                                    For main_year_loop := main_partition_start_year to main_partition_end_year do
                                    begin
                                      for main_ccode := min_ccode to max_ccode do
                                         if want_in_monad_year (main_ccode, main_year_loop, user_selections, nation_list) then
                                            begin
                                               output_one_record(outfile, main_ccode, 0, main_year_loop,
                                                 distance_data, minimum_distance_data, tau_data, alliance_data, s_data, polity3_data,
                                                 sys_capability_data, raw_capability_data,
                                                 risk_Tau_data, risk_S_data,
                                                 EUWarTrap_Tau_data, EUWarTrap_S_data,
                                                 EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                                 werner_peace_years_data, ISO_array, User_Data_Sets,
                                                 COW_dyadic_dispute_data_21, COW_dyadic_dispute_data_30, Maoz_dispute_data,
                                                 ICB_crisis_data, system_variables,
                                                 configuration, user_selections,
                                                 use_ccodeyear, null_dispute_number, null_dispute_number, null_dispute_number);
                                               Output_trace.tick('Executing Procedure: Write final monadic output ('+inttostr(main_year_loop)+')',
                                                  output_cases_planned);
                                               inc(cases_output);
                                             end;
                                    end;   {for main_year_loop}

                                 output_directed_dispute_initiation_dyads : {do dispute initiation dyad loop}
                                    {Do output by looping through all dyadic disputes.  For each dyadic dispute,
                                     output each dyad-year that's in the partition range.}
                                    {dyad year output}

                                    {Now, all dispute data are integrated in one structure, which
                                     was initialized with pre-1992 and post-1992 disputes.  So just
                                     need to call the appropriate structure, but with the same method.}
                                    begin
                                    {Decide which set to loop through based on exclusion setting.}
                                    if user_selections.conflict_exclusion_selection = cds_MaozMID then
                                       {old way was:  if Maoz_dyadic_disputes in user_selections.output_format.variables then}
                                       begin
                                          For main_dispute_loop := 0 to Maoz_dispute_data.get_last_dispnum do
                                             begin   {for every year of the dispute, if it's in range, process it}
                                                Output_trace.tick('Executing Procedure: Write final dispute output Maoz 30 (' +
                                                     inttostr(main_partition_start_year)+' to '+inttostr(main_partition_end_year)+')',
                                                     output_cases_planned);
                                                main_ccode1 := Maoz_dispute_data.get_ccode(main_dispute_loop, 0);
                                                main_ccode2 := Maoz_dispute_data.get_ccode(main_dispute_loop, 1);
                                                {note: this year loop should work whether using the old maoz dispute-year structure,
                                                 or the new one, because get first, last year will either give multiple years, or just 1.}
                                                {old code was
                                                  main_year_loop := Maoz_dispute_data.get_year(main_dispute_loop);  }
                                                {new code is next 2 lines.}
                                                for main_year_loop := Maoz_dispute_data.get_first_year(main_dispute_loop) to
                                                                 Maoz_dispute_data.get_last_year(main_dispute_loop) do
                                                   if ((main_year_loop >= main_partition_start_year) and
                                                       (main_year_loop <= main_partition_end_year)) then
                                                      begin
                                                         if want_in_dispute_initiation_year (main_ccode1, main_ccode2, main_year_loop,
                                                                 user_selections, nation_list, main_dispute_loop, Conflict_Case_Exclusion_data)
                                                         then
                                                            begin
                                                              {in this call, pass main_dispute_loop as the maoz case value.
                                                               COW, ICB values must be set elsewhere.}
                                                              if COW_disputes in user_selections.output_format.variables then
                                                                 corresponding_cow_dispnum := COW_dyadic_dispute_data_30.get_corresponding_cow_dispnum (main_dispute_loop, Maoz_dispute_data);
                                                              output_one_record(outfile, main_ccode1, main_ccode2, main_year_loop,
                                                                   distance_data, minimum_distance_data, tau_data, alliance_data, s_data, polity3_data,
                                                                   sys_capability_data, raw_capability_data,
                                                                   risk_Tau_data, risk_S_data,
                                                                   EUWarTrap_Tau_data, EUWarTrap_S_data,
                                                                   EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                                                   werner_peace_years_data, ISO_array, User_Data_Sets,
                                                                   COW_dyadic_dispute_data_21, COW_dyadic_dispute_data_30, Maoz_dispute_data,
                                                                   ICB_crisis_data, system_variables,
                                                                   configuration, user_selections,
                                                                   use_dispute, corresponding_cow_dispnum, main_dispute_loop, null_dispute_number);
                                                              inc(cases_output);
                                                            end;
                                                      end;
                                             end   {for main dispute loop}
                                       end
                                    else if user_selections.conflict_exclusion_selection = cds_COWMID then
                                       {old way was: else if COW_disputes in user_selections.output_format.variables then {Use COW disputes, not maoz}
                                       begin
                                          For main_dispute_loop := 0 to COW_dyadic_dispute_data_30.get_last_dispnum do   {this indexing is correct, get last is num-1}
                                             begin   {for every year of the dispute, if it's in range, process it}
                                                Output_trace.tick('Executing Procedure: Write final dispute output COW 30 (' +
                                                     inttostr(main_partition_start_year)+' to '+inttostr(main_partition_end_year)+')',
                                                     output_cases_planned);
                                                main_ccode1 := COW_dyadic_dispute_data_30.get_ccode(main_dispute_loop, 0);
                                                main_ccode2 := COW_dyadic_dispute_data_30.get_ccode(main_dispute_loop, 1);
                                                for main_year_loop := COW_dyadic_dispute_data_30.get_first_year(main_dispute_loop) to
                                                                 COW_dyadic_dispute_data_30.get_last_year(main_dispute_loop) do
                                                   if ((main_year_loop >= main_partition_start_year) and
                                                       (main_year_loop <= main_partition_end_year)) then
                                                      begin
                                                         if want_in_dispute_initiation_year (main_ccode1, main_ccode2, main_year_loop,
                                                                 user_selections, nation_list, main_dispute_loop, Conflict_Case_Exclusion_data)
                                                         then
                                                            begin
                                                              {in this call, pass main_dispute_loop as the COW case value.
                                                               Maoz, ICB values must be set elsewhere.}
                                                              if Maoz_dyadic_disputes in user_selections.output_format.variables then
                                                                 corresponding_maoz_dispnum := Maoz_dispute_data.get_corresponding_maoz_dispnum (main_dispute_loop, COW_dyadic_dispute_data_30);
                                                              output_one_record(outfile, main_ccode1, main_ccode2, main_year_loop,
                                                                   distance_data, minimum_distance_data, tau_data, alliance_data, s_data, polity3_data,
                                                                   sys_capability_data, raw_capability_data,
                                                                   risk_Tau_data, risk_S_data,
                                                                   EUWarTrap_Tau_data, EUWarTrap_S_data,
                                                                   EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                                                   werner_peace_years_data, ISO_array, User_Data_Sets,
                                                                   COW_dyadic_dispute_data_21, COW_dyadic_dispute_data_30, Maoz_dispute_data,
                                                                   ICB_crisis_data, system_variables,
                                                                   configuration, user_selections,
                                                                   use_dispute, main_dispute_loop, corresponding_maoz_dispnum, null_dispute_number);
                                                              inc(cases_output);
                                                            end;
                                                      end;
                                             end;   {for main dispute loop}
                                       end   {if cow_disputes}
                                    {can't yet do ICB in this output unit, because it is not directed.}
                                    {else if ICB_crises in user_selections.output_format.variables then {Use COW disputes, not maoz}
                                    {   begin
                                          For main_dispute_loop := 0 to ICB_crisis_data.get_last_dispnum do
                                             begin   {for every year of the dispute, if it's in range, process it}
                                    {            Output_trace.tick('Executing Procedure: Write final dispute output ICB (' +
                                                     inttostr(main_partition_start_year)+' to '+inttostr(main_partition_end_year)+')',
                                                     output_cases_planned);
                                    {            for main_year_loop := ICB_crisis_data.get_first_year(main_dispute_loop) to
                                                                 ICB_crisis_data.get_last_year(main_dispute_loop) do
                                                   if ((main_year_loop >= main_partition_start_year) and
                                                       (main_year_loop <= main_partition_end_year)) then
                                                      begin
                                                         main_ccode1 := ICB_crisis_data.get_ccode(main_dispute_loop, 0);
                                                         main_ccode2 := ICB_crisis_data.get_ccode(main_dispute_loop, 1);
                                                         if want_in_dispute_initiation_year (main_ccode1, main_ccode2, main_year_loop,
                                                                 user_selections, nation_list, main_dispute_loop, Conflict_Case_Exclusion_data)
                                                         then
                                                            begin
                                                              output_one_record(outfile, main_ccode1, main_ccode2, main_year_loop,
                                                                   distance_data, minimum_distance_data, tau_data, alliance_data, s_data, polity3_data,
                                                                   sys_capability_data, raw_capability_data,
                                                                   risk_Tau_data, risk_S_data,
                                                                   EUWarTrap_Tau_data, EUWarTrap_S_data,
                                                                   EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                                                   werner_peace_years_data, User_Data_Sets,
                                                                   COW_dyadic_dispute_data_21, COW_dyadic_dispute_data_30, Maoz_dispute_data,
                                                                   ICB_crisis_data, system_variables,
                                                                   configuration, user_selections, use_dispute, main_dispute_loop);
                                                              inc(cases_output);
                                                            end;
                                                      end;
                                             end;   {for main dispute loop}
                                    {   end   {if icb_crises}
                                    else
                                       EUGeneError ('Tried to output dispute initiation unit, but neither COW nor MAOZ disputes selected as exclusion set for output.  Programming error - notify programmer.',1,stop,error_log);
                                    end;    {case output_directed_dispute_initiation_dyads}







                                         {copy of the directed dispute initiation dyad at the moment}
                                 output_nondirected_dispute_dyads : {do dispute initiation dyad loop}
                                    {Do output by looping through all dyadic disputes.  For each dyadic dispute,
                                     output each dyad-year that's in the partition range.}
                                    {dyad year output}

                                    {Now, all dispute data are integrated in one structure, which
                                     was initialized with pre-1992 and post-1992 disputes.  So just
                                     need to call the appropriate structure, but with the same method.}
                                    begin
                                    {Decide which set to loop through based on exclusion setting.}
                                    if user_selections.conflict_exclusion_selection = cds_MaozMID then
                                       {old way was:  if Maoz_dyadic_disputes in user_selections.output_format.variables then}
                                       begin
                                          For main_dispute_loop := 0 to Maoz_dispute_data.get_last_dispnum do
                                             begin   {for every year of the dispute, if it's in range, process it}
                                                Output_trace.tick('Executing Procedure: Write final dispute output Maoz 30 (' +
                                                     inttostr(main_partition_start_year)+' to '+inttostr(main_partition_end_year)+')',
                                                     output_cases_planned);
                                                main_ccode1 := min(Maoz_dispute_data.get_ccode(main_dispute_loop, 0), Maoz_dispute_data.get_ccode(main_dispute_loop, 1));
                                                main_ccode2 := max(Maoz_dispute_data.get_ccode(main_dispute_loop, 0), Maoz_dispute_data.get_ccode(main_dispute_loop, 1));
                                                for main_year_loop := Maoz_dispute_data.get_first_year(main_dispute_loop) to
                                                                 Maoz_dispute_data.get_last_year(main_dispute_loop) do
                                                   if ((main_year_loop >= main_partition_start_year) and
                                                       (main_year_loop <= main_partition_end_year)) then
                                                      begin

                                                         if want_in_nondirected_dispute_year (main_ccode1, main_ccode2, main_year_loop,
                                                                 user_selections, nation_list, main_dispute_loop, Conflict_Case_Exclusion_data)
                                                         then
                                                            begin
                                                              {in this call, pass main_dispute_loop as the maoz case value.
                                                               COW, ICB values must be set elsewhere.}
                                                              if COW_disputes in user_selections.output_format.variables then
                                                                 corresponding_cow_dispnum := COW_dyadic_dispute_data_30.get_corresponding_cow_dispnum (main_dispute_loop, Maoz_dispute_data);
                                                              output_one_record(outfile, main_ccode1, main_ccode2, main_year_loop,
                                                                   distance_data, minimum_distance_data, tau_data, alliance_data, s_data, polity3_data,
                                                                   sys_capability_data, raw_capability_data,
                                                                   risk_Tau_data, risk_S_data,
                                                                   EUWarTrap_Tau_data, EUWarTrap_S_data,
                                                                   EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                                                   werner_peace_years_data, ISO_array, User_Data_Sets,
                                                                   COW_dyadic_dispute_data_21, COW_dyadic_dispute_data_30, Maoz_dispute_data,
                                                                   ICB_crisis_data, system_variables,
                                                                   configuration, user_selections,
                                                                   use_dispute, corresponding_cow_dispnum, main_dispute_loop, null_dispute_number);
                                                              inc(cases_output);
                                                            end;

                                                      end;
                                             end   {for main dispute loop}
                                       end
                                    else if user_selections.conflict_exclusion_selection = cds_COWMID then
                                       {old way was: else if COW_disputes in user_selections.output_format.variables then {Use COW disputes, not maoz}
                                       begin
                                          For main_dispute_loop := 0 to COW_dyadic_dispute_data_30.get_last_dispnum do
                                             begin   {for every year of the dispute, if it's in range, process it}
                                                Output_trace.tick('Executing Procedure: Write final dispute output COW 30 (' +
                                                     inttostr(main_partition_start_year)+' to '+inttostr(main_partition_end_year)+')',
                                                     output_cases_planned);
                                                {main_ccode1 := COW_dyadic_dispute_data_30.get_ccode(main_dispute_loop, 0);
                                                main_ccode2 := COW_dyadic_dispute_data_30.get_ccode(main_dispute_loop, 1);  }
                                                main_ccode1 := min (COW_dyadic_dispute_data_30.get_ccode(main_dispute_loop, 0), COW_dyadic_dispute_data_30.get_ccode(main_dispute_loop, 1));
                                                main_ccode2 := max (COW_dyadic_dispute_data_30.get_ccode(main_dispute_loop, 0), COW_dyadic_dispute_data_30.get_ccode(main_dispute_loop, 1));
                                                for main_year_loop := COW_dyadic_dispute_data_30.get_first_year(main_dispute_loop) to
                                                                 COW_dyadic_dispute_data_30.get_last_year(main_dispute_loop) do
                                                   if ((main_year_loop >= main_partition_start_year) and
                                                       (main_year_loop <= main_partition_end_year)) then
                                                      begin

                                                         if want_in_nondirected_dispute_year (main_ccode1, main_ccode2, main_year_loop,
                                                                 user_selections, nation_list, main_dispute_loop, Conflict_Case_Exclusion_data)
                                                         then
                                                            begin
                                                              {in this call, pass main_dispute_loop as the COW case value.
                                                               Maoz, ICB values must be set elsewhere.}
                                                              if Maoz_dyadic_disputes in user_selections.output_format.variables then
                                                                 corresponding_maoz_dispnum := Maoz_dispute_data.get_corresponding_maoz_dispnum (main_dispute_loop, COW_dyadic_dispute_data_30);
                                                              output_one_record(outfile, main_ccode1, main_ccode2, main_year_loop,
                                                                   distance_data, minimum_distance_data, tau_data, alliance_data, s_data, polity3_data,
                                                                   sys_capability_data, raw_capability_data,
                                                                   risk_Tau_data, risk_S_data,
                                                                   EUWarTrap_Tau_data, EUWarTrap_S_data,
                                                                   EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                                                   werner_peace_years_data, ISO_array, User_Data_Sets,
                                                                   COW_dyadic_dispute_data_21, COW_dyadic_dispute_data_30, Maoz_dispute_data,
                                                                   ICB_crisis_data, system_variables,
                                                                   configuration, user_selections,
                                                                   use_dispute, main_dispute_loop, corresponding_maoz_dispnum, null_dispute_number);
                                                              inc(cases_output);
                                                            end;


                                                      end;
                                             end;   {for main dispute loop}
                                       end   {if cow_disputes}

                                    else
                                       EUGeneError ('Tried to output nondirected dispute unit, but neither COW nor MAOZ disputes selected as exclusion set for output.  Programming error - notify programmer.',1,stop,error_log);
                                    end    {case output_nondirected_dispute_dyads}


                                 else EUGeneError ('Reached loop section in output_data procedure with an output unit not properly specified.',1,stop,error_log);
                                 end;     {case}

                          finally
                             delete_data (main_partition_start_year, main_partition_end_year, tau_data, alliance_data,
                                       s_data, polity3_data, sys_capability_data, raw_capability_data,
                                       risk_Tau_data, risk_S_data, EUWarTrap_Tau_data, EUWarTrap_S_data,
                                       EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                       minimum_distance_data, User_Data_Sets, user_selections, configuration);                                    main_partition_start_year := main_partition_end_year + 1;
                          end;   {try-finally }

                       end;   {for main_partition_loop}
                 finally
                    close_output (user_selections, outfile);
                 end;
              end;   {not command files only}
              if (user_selections.output_format.commandfiles <> []) then
                 write_command_files (user_selections, cases_output, separator, outstring, outtypes, UserVarNamesAndMissingValuesList, polnonpolnames);
              trace.exit ('Finished main output loop');

            finally
              {Delete structures I created if it's dyadic output.}
              if ((selected_output_unit = output_directed_dyads) or
                  (selected_output_unit = output_nondirected_dyads) or
                  (selected_output_unit = output_directed_dispute_initiation_dyads) or
                  (selected_output_unit = output_nondirected_dispute_dyads) )
                 then
                 begin
                    if ((distance in user_selections.output_format.variables) and
                        (user_selections.distance_method <> minimum)) or
                       (user_selections.dyads_selected = within_distance) then
                          distance_data.free;
                    if (Maoz_dyadic_disputes in user_selections.output_format.variables) or
                        (COW_disputes in user_selections.output_format.variables) then
                       COW_dyadic_dispute_data_30.Free;
                    if ICB_Crises in user_selections.output_format.variables then
                       ICB_crisis_data.free;
                    if ((selected_output_unit = output_directed_dyads) or
                        (selected_output_unit = output_nondirected_dyads))
                    then
                       wanted_dyad_list.free;
                 end;      {if dyadic or dispute output}

              Output_trace.tickdone;
              Output_trace.free;
              EndTime := now;
              ElapsedTime := EndTime - StartTime;
              GetLocaleFormatSettings(0,timeformat);

              if (user_selections.output_format.location = tofile)
              then
                 ShowMessage ('EUGene output complete!  '+inttostr(cases_output)+' cases written to file '+user_selections.output_format.output_file_name+' in time '+inttostr(hourof(ElapsedTime))+':'+inttostr(minuteof(ElapsedTime)) +':'+inttostr(secondof(ElapsedTime)) +'.'+inttostr(millisecondof(ElapsedTime))+'.')  {TimeToStr(ElapsedTime, timeformat)+'.')}
              else
                 ShowMessage ('EUGene output complete!  '+inttostr(cases_output)+' cases written in time '+inttostr(hourof(ElapsedTime))+':'+inttostr(minuteof(ElapsedTime)) +':'+inttostr(secondof(ElapsedTime)) +'.'+inttostr(millisecondof(ElapsedTime))+'.')
            end;   {finally}
         end     {if files_exist}
         else
            begin
               ShowMessage ('Necessary files not found!');
            end;

      except
         on EUserInterrupt do
            begin
               {need the exception still raised outside this procedure}
               raise;
            end;
      end;        {outer try except}

   end;   {main proc output_data}

{ ------------------------------------------------------------ }

end.    {unit euinoutD}
