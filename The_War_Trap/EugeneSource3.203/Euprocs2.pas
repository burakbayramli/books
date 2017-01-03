unit Euprocs2;

{EUGene  Copyright 1997, 1998  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

 interface

uses TraceUnit, sysutils, dialogs, math, cmnprocD, eutypes1, eutypes2,
     euprocs1, euinoutD, FileError, eutypesMID;

procedure compute_and_save_EUWarReason (sys_cap_file_name, tau_file_name, s_file_name,
          risk_tau_file_name, risk_s_file_name, risk_wtr_file_name, locations_file_name, EU_WR_File_Name : TFileName;
          const nation_list : Tnation_array_obj;
          const contiguity_data : Tcontiguity_array_obj;
          first_proc_year, last_proc_year : year_range; user_selections : user_selection_type);


implementation

   uses errbx;

procedure compute_and_save_EUWarReason (sys_cap_file_name, tau_file_name, s_file_name,
          risk_tau_file_name, risk_s_file_name, risk_wtr_file_name, locations_file_name, EU_WR_File_Name : TFileName;
          const nation_list : Tnation_array_obj;
          const contiguity_data : Tcontiguity_array_obj;
          first_proc_year, last_proc_year : year_range; user_selections : user_selection_type);

          {This procedure creates and outputs EU values following user specifications
           for all dyads and years in range.  This essentially replaces the big
           outer loop in the EUMain unit that was there in early program versions.}
   var main_num_partitions, main_years_per_partition, main_partition_loop : integer;
       main_partition_start_year, main_partition_end_year, main_year_loop : year_range;
       tau_data : Ttau_array_obj;
       s_data : Ts_array_obj;
       risk_data : Trisk_attitude_array_obj;
       sys_capability_data : Tsys_capability_array_obj;
       distance_data : Tdistance_array_obj;
       EUWarReason_file : EUWarReason_file_type;
       files_exist : boolean;
       EUWarReason_file_record : EUWarReason_file_record_type;
       year_loop, temp : year_range;
       ccode1, ccode2 : ccode_range;
       EUWRtrace : Ttrace_obj;
       want_dyad : boolean;
       COW_dyadic_dispute_data_30 : TDyadic_dispute_data_obj_integrated_format30;
       Maoz_dispute_data : TDyadic_dispute_data_obj_integrated_format30;
       ICB_crisis_data : TICBDyadic_dispute_data_obj;
       adispnum : longint;

      { ------------------------------------------------------------ }

   procedure preprocess_for_EUWarReason (const configuration : configuration_type;
      var files_exist : boolean; const user_selections : user_selection_type);
       {This procedure just checks to make sure intermediate files exist.  If they
        don't, it reports an error, leaving the user to run and create the
        intermediate files.}
      var temp_name : TFileName;
      begin
         files_exist := false;
         case user_selections.similarity_method of
           use_tau : temp_name := configuration.risk_Tau_file_name;
           use_s : temp_name := configuration.risk_S_unweighted_file_name;
           else EUGeneError ('Came into compute_and save risk file check procedure without s/tau option set.  NOtify programmer.  Fatal error.  ',2,stop,error_log);
         end;  {case}

         if check_file_Exists (configuration.cow_system_pct_file_name,
                           '% System Capabilities') then
            if (((check_file_Exists (configuration.tau_file_name,
                             'Tau-b Scores')) and (user_selections.similarity_method = use_tau)) or
                ((check_file_Exists (configuration.s_file_name,
                             'S Scores')) and (user_selections.similarity_method = use_s))) then
               if check_file_Exists (temp_name, 'Risk Scores') then
                  files_exist := true;

      end;    {proce preprocess for eu}

      { ------------------------------------------------------------ }

   procedure calculate_EUWarReason_partitions (first_proc_year, last_proc_year : year_range;
          var num_partitions, years_per_partition : integer);
			{Compute number of partitions / passes necessary based on free memory}

   var mem_per_year, mem_to_use : longint;
       years_possible, num_years : longint;
   begin
      trace.enter('Entered calculate_EUWarReason_partitions procedure');
           {Calculation will be, how many years of tau, system capability, risk data can fit into
             appropriate area of memory.  This should leave LOTs of memory free for
             miscellaneous variables and functions. }
            {formula for # partitions is trunc (freemem to use) / (mem per partition)}
            {for mem to use, take 1/2 of available block, and subtract the overhead
             for the data structures.  }
       num_years := (last_proc_year - first_proc_year + 1);
       mem_to_use := mem_for_procedure - TTau_array_obj_mem_overhead -
                      Tsys_capability_array_obj_mem_overhead - TRisk_Attitude_array_obj_mem_overhead;
       mem_per_year := Tsys_capability_array_obj_mem_per_year + TRisk_Attitude_array_obj_mem_per_year;
       case user_selections.similarity_method of
                          use_tau : mem_per_year := mem_per_year + TTau_array_obj_mem_per_year;
                          use_s : mem_per_year := mem_per_year + Ts_array_obj_mem_per_year;
                          else EUGeneError ('Error in setting s/tau option within eu war reason partition calculation:  similarity_method not use_tau or use_s.', 5, continue, error_log);
               end;  {case}
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
         trace.message ('Information calculated on main partition division:');
         trace.message ('Total years for processing: '+inttostr(num_years));
         trace.message ('Memory that can be used: '+inttostr(mem_to_use)+' bytes');
         trace.message ('Memory to be allocated per year: '+inttostr(mem_per_year)+' bytes');
         trace.message ('Memory per Tau year: '+inttostr( ((sizeof(tau_year_array_type) +
                       ((max_ccode_index-min_ccode_index+1) * (sizeof(tau_array_2))) )) )
                       +' bytes');
         trace.message ('Memory per 6 part COW Capability year data: '+inttostr(sizeof(syscap_array))+' bytes');
         trace.message ('Memory per year of Risk data: '+inttostr(sizeof(risk_ccode_array_v2))+' bytes');
         trace.message ('Years possible per partition: '+inttostr(years_possible));
         trace.message ('Number of partitions necessary: '+inttostr(num_partitions));
         trace.message ('Actual number of years per partition '+inttostr(years_per_partition));
       end;   {if debug[3]}
       if debug[2] then
         trace.message('Will run through ' + inttostr(num_partitions)+' partitions, with '+
                     inttostr(years_per_partition)+' year in each partition.');

      trace.exit('Exited calculate_partitions procedure');
   end;

{ ------------------------------------------------------------ }

   procedure Read_data_for_EUWarReason (const partition_start_year, partition_end_year : year_range;
          const configuration : configuration_type; var tau_data : Ttau_array_obj;
          var s_data : TS_array_obj;  var sys_capability_data:
          Tsys_capability_array_obj;  var risk_data : Trisk_attitude_array_obj;
          const user_selections : user_selection_type);
					{Read data on taus, system_year_capability, risk attitude,
                     for this partition }
   begin
      tau_data := nil;
      s_data := nil;
      sys_capability_data := nil;
      risk_data := nil;
      try
         trace.enter('Entered procedure to read_data_for_EUWarReason (tau, cap, risk) from '+inttostr(partition_start_year)+
                              ' to '+inttostr(partition_end_year));
         case user_selections.similarity_method of
           use_tau : begin
                 tau_data := Ttau_array_obj.init (tau_file_name, partition_start_year, partition_end_year);
                 tau_data.modifyTauForWarReason (partition_start_year, partition_end_year);
              end;
           use_s : begin
                 s_data := TS_array_obj.init (s_file_name, partition_start_year, partition_end_year);
                 s_data.modifySForWarReason (partition_start_year, partition_end_year);
              end;

           else EUGeneError ('Error in setting s/tau option within eu war trap calculation:  similarity_method not use_tau or use_s.', 5, continue, error_log);
         end;  {case}
         sys_capability_data := Tsys_capability_array_obj.init
                             (configuration.cow_system_pct_file_name, partition_start_year, partition_end_year);

         case user_selections.similarity_method of
           use_tau : risk_data := Trisk_attitude_array_obj.init (risk_Tau_file_name, risk_WTR_file_name,
                             partition_start_year, partition_end_year, user_selections.risk_data_source);
           use_s : risk_data := Trisk_attitude_array_obj.init (risk_S_file_name, risk_WTR_file_name,
                             partition_start_year, partition_end_year, user_selections.risk_data_source);
           else EUGeneError ('Came into read risk files inside compute eu war and reason procedure without s/tau option set.  NOtify programmer.  Fatal error.  ',2,stop,error_log);
         end;   {case}
         trace.exit ('Finished reading this set of input data');
      except
         tau_data.free;
         s_data.free;
         sys_capability_data.free;
         risk_data.free;
         raise;
      end;
   end;

{ ------------------------------------------------------------ }

procedure delete_data_for_EUWarReason (var tau_data : Ttau_array_obj; var s_data : TS_array_obj;
          var sys_capability_data: Tsys_capability_array_obj;  var risk_data : Trisk_attitude_array_obj);
   begin
      try
         trace.enter('Entered delete_data_for_EUWarReason procedure');
      finally   {make sure these all get done}
         tau_data.free;
         s_data.free;
         risk_data.free;
         sys_capability_data.free;
         trace.exit ('Finished deleting this EUWarReason data');
      end;
   end;

{ ------------------------------------------------------------ }

   type U_for_this_outcome_type = (DAA, DAB, ASQ, DBB, DBA, BSQ);

   function utility_war_reason (const ccode1, ccode2 : ccode_range;
            year : year_range; outcome_wanted : U_for_this_outcome_type;
            const aregion : region_type; const tau_data : Ttau_array_obj;
            const s_data : Ts_array_obj;  const risk_data : Trisk_attitude_array_obj;
            const user_selections : user_selection_type) : single;
      {returns utility adjusted by risk.  Gets a region in the call b/c
       of the risk score, which requires a regional ID, and b/c this could
       be called with ccode1 vs. ccode1, or for SQ value.
      If called with ccode2 = 0, then this returns U(SQ).
      If called with ccode1=ccode2, uses different formula than ccode1<>ccode2}
      var lowercase_risk1, uppercase_risk1, Tau12 : single;
          lowercase_risk2, uppercase_risk2, Tau21 : single;
          {Note:  Taus here are taus for the specified region, not necessarily
           the "relevant_region" b/c we need to maintain directionality for EU calcs.
           So, calcs for B for dyad AB are not necessarily the same as B for dyad BA.}

      begin
         {This will use either s or tau as specified by user, but it builds those
          scores into variables that are still named/labeled tau internally.  This is not
          a problem.}

         Tau12 := 0;
         Tau21 := 0;
         uppercase_risk1 := 0;
         uppercase_risk2 := 0;
         {check tau/s symmetric}
         case user_selections.similarity_method of
            use_tau : if tau_data.get_tau_value_regional (ccode1, ccode2, year, aregion) <>
                     tau_data.get_tau_value_regional (ccode2, ccode1, year, aregion) then
                        begin
                           showmessage ('mismatch tau at '+inttostr(ccode1)+inttostr(ccode2)+inttostr(year)+inttostr(ord(aregion)));
                        end;
            use_s : if s_data.get_s_value_regional (ccode1, ccode2, year, aregion, user_selections.s_weighting) <>
                     s_data.get_s_value_regional (ccode2, ccode1, year, aregion, user_selections.s_weighting) then
                        begin
                           showmessage ('mismatch s at '+inttostr(ccode1)+inttostr(ccode2)+inttostr(year)+inttostr(ord(aregion)));
                        end;
            else EUGeneError ('Error in checking s/tau match within eu war + reason calculation:  similarity_method not use_tau or use_s.', 5, continue, error_log);
         end;

         case outcome_wanted of
            DAA, DAB, ASQ : begin
                  uppercase_risk1 := risk_data.get_risk(ccode1, year, aregion);
                  lowercase_risk1 := (1-(uppercase_risk1/3)) / (1+(uppercase_risk1/3));
                  case user_selections.similarity_method of
                     use_tau : Tau12 := tau_data.get_tau_value_regional (ccode1, ccode2, year, aregion);
                     use_s : Tau12 := s_data.get_s_value_regional (ccode1, ccode2, year, aregion, user_selections.s_weighting);
                     else EUGeneError ('Error in getting utility for s/tau option within eu war reason calculation:  similarity_method not use_tau or use_s.', 5, continue, error_log);
                  end;
               end;
            DBB, DBA, BSQ : begin
                  uppercase_risk2 := risk_data.get_risk(ccode2, year, aregion);
                  lowercase_risk2 := (1-(uppercase_risk2/3)) / (1+(uppercase_risk2/3));
                  case user_selections.similarity_method of
                     use_tau : Tau21 := tau_data.get_tau_value_regional (ccode2, ccode1, year, aregion);
                     use_s : Tau21 := s_data.get_s_value_regional (ccode2, ccode1, year, aregion, user_selections.s_weighting);
                     else EUGeneError ('Error in getting utility for s/tau option within eu war reason calculation:  similarity_method not use_tau or use_s.', 5, continue, error_log);
                  end;
               end;
         end;    {case}

         if (Tau12 = missing_value) or (uppercase_risk1 = missing_value) or
            (Tau21 = missing_value) or (uppercase_risk2 = missing_value) then
            begin
               utility_war_reason := missing_value;
            end
         else
         case outcome_wanted of
           DAA : utility_war_reason := 2-4*realpower( ( (2-(1-Tau12))/4 ), lowercase_risk1);
           DAB : utility_war_reason := 2-4*realpower( ( (2-(Tau12-1))/4 ), lowercase_risk1);
           ASQ : utility_war_reason := 2-4*realpower( (2/4), lowercase_risk1);
           DBB : utility_war_reason := 2-4*realpower( ( (2-(1-Tau21))/4 ), lowercase_risk2);
           DBA : utility_war_reason := 2-4*realpower( ( (2-(Tau21-1))/4 ), lowercase_risk2);
           BSQ : utility_war_reason := 2-4*realpower( (2/4), lowercase_risk2);
         end;   {case}

      end;

           { -------------------------------------- }

   procedure compute_one_EUWarReason_dyad (ccodeA, ccodeB : ccode_range; year : year_range;
             const distance_data: Tdistance_array_obj; const tau_data : Ttau_array_obj;
             const s_data : Ts_array_obj;  const sys_capability_data : Tsys_capability_array_obj;
             const risk_data : Trisk_attitude_array_obj;
             var EUWarReason : EUWarReason_record; var user_selections : user_selection_type);

      {This proc takes various stored raw data in memory and processes it to create
       eu and prob(escalate) as in War and Reason, for this dyad-year. }
      var UADA, UADB, UASQ, PAB : single;
          UBDB, UBDA, UBSQ, PBA : single;
          key_UAB : single;
          Uppercase_RiskA, Uppercase_RiskB : single;
          region : region_type;
          PnumA, PDenomA, PNumB, PDenomB, KTauA, KTauB, AUKA_minus_UKB,
             BUKB_minus_UKA : single;
          ccodeK : ccode_range;
          AdjCapKA, AdjCapKB, AdjCapK : single;

      begin
         {There are several elements to EUwar Reason Utility.  First, notation.
         Ui(Di)=i's utility for getting I's demand; Ui(Dj)=i's utility for getting j's
           demand;  Ui(SQ), I's utility for keeping SQ.
         STAKES in a dispute = Ui(Di)-Ui(Dj).  Smaller the diff in alliance portfolios,
           smaller the difference in UiDi-UiDj
         Risk is ri.  Note that we compute Ri, it must be transformed.
         These are defined in W+R pp 293-294.
         For each dyad A vs B, we have:
         UA(DA) = 2-4*( (2-(1-Tau(AB))/4)^rA.
         UA(DB) = 2-4*( (2-(Tau(AB)-1)/4)^rA.
         UA(SQ) = 2-4*(2/4)^rA.
         And Similar for UB(DB)...   }

         {This will use either s or tau as specified by user, but it builds those
          scores into variables that are still named/labeled tau internally.  This is not
          a problem.}

         {Since we are ultimately concerned about EU of A vs. B, codings use
          relevant region A vs B}
         region := relevant_region(ccodeA, ccodeB, year);

         {U for A}
         UADA := utility_war_reason(ccodeA, ccodeB, year, DAA, region, tau_data, s_data, risk_data, user_selections);
         UADB := utility_war_reason(ccodeA, ccodeB, year, DAB, region, tau_data, s_data, risk_data, user_selections);
         UASQ := utility_war_reason(ccodeA, ccodeB, year, ASQ, region, tau_data, s_data, risk_data, user_selections);

         {Also figure out U for B}
         UBDB := utility_war_reason(ccodeA, ccodeB, year, DBB, region, tau_data, s_data, risk_data, user_selections);
         UBDA := utility_war_reason(ccodeA, ccodeB, year, DBA, region, tau_data, s_data, risk_data, user_selections);
         UBSQ := utility_war_reason(ccodeA, ccodeB, year, BSQ, region, tau_data, s_data, risk_data, user_selections);
         {These could come out as -9, if so, they are just missing and will be passed
          out as such.}

         {Now need to estimate the probability of A winning.  }
         {This is a function of summing what all states in the system do.
          Numerator (W+R 296-297) is summed over states that prefer A,
          Denominator is summed over all states.}
         {It is possible to get an extreme, and probably incorrect, result if the capabilities of
          one of the main states A or B is missing.  so verify that they both have capabilities
          and s/taus if this dyad is to be computed.}
         {Risk for A and B are toward the relevant region}
         {Assign missing in a number of circumstances where key data is missing, and for A=B}
         Uppercase_RiskA := risk_data.get_risk(ccodeA, year, region);
         Uppercase_RiskB := risk_data.get_risk(ccodeB, year, region);
         case user_selections.similarity_method of
            use_tau : key_UAB := tau_data.get_tau_value_regional (ccodeA, ccodeB, year, region);
            use_s : key_UAB := s_data.get_s_value_regional (ccodeA, ccodeB, year, region, user_selections.s_weighting);
            else EUGeneError ('Error in getting key utility for s/tau option within compute 1 eu war reason dyad calculation:  similarity_method not use_tau or use_s.', 5, continue, error_log);
         end;

         if ((key_UAB <> missing_value) and
             (Uppercase_RiskA <> missing_value) and (Uppercase_RiskB <> missing_value) and
             (sys_capability_data.get_syscap(ccodeA, year) <> missing_value) and
             (sys_capability_data.get_syscap(ccodeB, year) <> missing_value) and
             (ccodeA <> ccodeB) ) then
            begin
               PnumA := 0;
               PnumB := 0;
               PDenomA := 0;
               PDenomB := 0;
               {loop through all states in the region, including the main initiator (A) and
                target (B).  If we are missing any info on a state that would usually be
                added, just don't add in that state.}
               {This can be easily made global, just remove the check for states in
                the region}
               for ccodeK := min_ccode to max_ccode do
                 {if (nation_list.is_a_state (ccodeK, year)) then}
                 if (nation_list.is_a_state (ccodeK, year)) and
                    ( (nation_list.is_involved_in_region(ccodeK, region, year)) or
                      (ccodeK=ccodeA) or (ccodeK=ccodeB) ) then
                   begin
                      {Whether state contributes to numerator or just to denominator depends
                       on whether K (other_ccode) prefers A or B.}
                      {Note that the variable is called Uppercase_Risk to make clear that
                       this is RA rather than rA from BdMs notation.  rA is the transformed
                       version, and is used in the Utility function calculations.  RA is the
                       untransformed version calculated and saved by the risk procedures.}
                      {Here, use the relevant_region for K and A, or K and B, whatever those
                       are, rather than specifying the region of A vs. B.  This is logically
                       because K is deciding to help A or B depending on K's interests, which
                       may not involve the region A and B are concerned about.}
                      {Use regional only for A, B themselves.}
                      {Note:  in correct notation, we should separate A's perception of K's utility
                       for joining side A or B.  But, operationally, A and B perceive the same.}
                      if (ccodeK = ccodeA) or (ccodeK=ccodeB) then
                         begin
                           case user_selections.similarity_method of
                              use_tau : begin
                                   KTauA := Tau_data.get_tau_value_regional(ccodeK, ccodeA, year, region);
                                   KTauB := Tau_data.get_tau_value_regional(ccodeK, ccodeB, year, region);
                                 end;
                              use_s : begin
                                   KTauA := s_data.get_s_value_regional(ccodeK, ccodeA, year, region, user_selections.s_weighting);
                                   KTauB := s_data.get_s_value_regional(ccodeK, ccodeB, year, region, user_selections.s_weighting);
                                 end;
                              else EUGeneError ('Error in getting utility for s/tau option within eu war reason calculation:  similarity_method not use_tau or use_s.', 5, continue, error_log);
                           end;
                         end
                      else
                         begin
                           case user_selections.similarity_method of
                              use_tau : begin
                                   KTauA := Tau_data.get_tau_value_regional(ccodeK, ccodeA, year,
                                                           relevant_region(ccodeK, ccodeA, year));
                                   KTauB := Tau_data.get_tau_value_regional(ccodeK, ccodeB, year,
                                                           relevant_region(ccodeK, ccodeB, year));
                                 end;
                              use_s : begin
                                   KTauA := s_data.get_s_value_regional(ccodeK, ccodeA, year,
                                                           relevant_region(ccodeK, ccodeA, year), user_selections.s_weighting);
                                   KTauB := s_data.get_s_value_regional(ccodeK, ccodeB, year,
                                                           relevant_region(ccodeK, ccodeB, year), user_selections.s_weighting);
                                 end;
                              else EUGeneError ('Error in getting utility for s/tau option within eu war reason calculation:  similarity_method not use_tau or use_s.', 5, continue, error_log);
                           end;
                         end;

                      if ((KTauA <> missing_value) and (KTauB <> missing_value) and
                          (Uppercase_RiskA <> missing_value)) then
                         AUKA_minus_UKB := ((KTauA - KTauB)/2) *
                                          (realpower(e, Uppercase_RiskA*(KTauA - KTauB)))
                      else AUKA_minus_UKB := missing_value;

                      if ((KTauA <> missing_value) and (KTauB <> missing_value) and
                          (Uppercase_RiskB <> missing_value)) then
                         BUKB_minus_UKA := ((KTauB - KTauA)/2) *
                                          (realpower(e, Uppercase_RiskB*(KTauB - KTauA)))
                      else BUKB_minus_UKA := missing_value;

                      {For distance discounting, Third party states K can contribute their
                       capabilities to either reinforce one state or fight at the other,
                       whichever is shorter.  But, since this is EU of A vs B, so A attacking
                       B, A must always project its capabilities to B, while B always keeps
                       its capabilities at B.}
                      if (ccodeK=ccodeA) then
                         adjCapK := adj_cap(ccodeA, ccodeB, year, sys_capability_data, distance_data)
                      else if (ccodeK=ccodeB) then
                         adjCapK := adj_cap(ccodeB, ccodeB, year, sys_capability_data, distance_data)
                      else     {this is a third party}
                         begin
                            AdjCapKA := adj_cap(ccodeK, ccodeA, year, sys_capability_data, distance_data);
                            AdjCapKB := adj_cap(ccodeK, ccodeB, year, sys_capability_data, distance_data);
                            AdjCapK := max(AdjCapKA, AdjCapKB);
                         end;

                      if (AdjCapK <> missing_value) and (AUKA_minus_UKB <> missing_value) then
                         begin
                            {If K prefers A, goes into Numerator for PA.}
                            if AUKA_minus_UKB > 0 then
                               PnumA := PnumA + (AdjCapK * AUKA_minus_UKB);

                             {Abs value of K's prefs always goes into the denominator.}
                            PDenomA := PDenomA + (AdjCapK * abs(AUKA_minus_UKB));
                         end;
                      if (AdjCapK <> missing_value) and (BUKB_minus_UKA <> missing_value) then
                         begin        {IF STATE prefers B to A, it goes in num, else in denom only}
                            {If K prefers B, goes into Numerator for PB.}
                            if BUKB_minus_UKA > 0 then
                               PnumB := PnumB + (AdjCapK * BUKB_minus_UKA);
                             {Abs value of K's prefs always goes into the denominator.}
                            PDenomB := PDenomB + (AdjCapK * abs(BUKB_minus_UKA));
                         end;

                     {** This is where to output some special info about who prefers A or B, and what the adjusted
                      cap is.}
                      if user_selections.custom_procedure = EUWarReasonComponents then
                         writeln (user_selections.temporary_output_file, ccodeA:3, ', ', ccodeB:3, ', ', ccodeK:3, ', ', year:4, ',',
                                  KTauA:6:5, ' ,', KTauB:6:5, ', ', Uppercase_RiskA:7:6, ', ', Uppercase_RiskB:7:6, ', ', AUKA_minus_UKB:9:8, ',', BUKB_minus_UKA:9:8, ',',
                                  AdjCapKA:6:5, ', ', AdjCapKB:6:5);

                   end;      {for other_ccode}

               if PDenomA = 0 then
                    begin
                       EUGeneError ('Denominator in EU calculation for PAB for '+inttostr(ccodeA)+' vs '+inttostr(ccodeB)+
                          ' in '+inttostr(year)+' was 0.  Setting P to missing value.', 5, continue, error_log);
                       PAB := missing_value;
                    end
               else PAB := PnumA / PDenomA;

               if PDenomB = 0 then     {should never be 0 at this point.}
                    begin
                       EUGeneError ('Denominator in EU calculation for PBA for '+inttostr(ccodeA)+' vs '+inttostr(ccodeB)+
                          ' in '+inttostr(year)+' was 0.  Setting P to missing value.', 5, continue, error_log);
                       PBA := missing_value;
                    end
               else PBA := PnumB / PDenomB;

            end        {not missing either main capability, or main utility}
         else      {am missing some key value}
            begin
               PAB := missing_value;
               PBA := missing_value;
            end;


         {Domestic cost term, p. 297, is A's utility for SQ * its P(win)}
         {This will not be figured here and saved, but is calculated later.}
         { Cost_domestic := UASQ * PAB;}
         {There is also a cost in lost life and property, inverse function of relative
          power.  There is an unestimated alpha, tau, or gamma on these terms
          that BdM cannot separate/measure.  So:  }
         {This will not be figured here and saved, but is calculated later.}
         { Cost_Life_Property := (1-PAB); }


         {Only some of these elements need to be saved permanently.
          When values were missing, these will be passed out as a "missing value", -9.}
         EUWarReason.UtilityAA := UADA;
         EUWarReason.UtilityAB := UADB;
         EUWarReason.UtilityASQ := UASQ;
         EUWarReason.ProbWinAB := PAB;
         EUWarReason.UtilityBB := UBDB;
         EUWarReason.UtilityBA := UBDA;
         EUWarReason.UtilityBSQ := UBSQ;
         EUWarReason.ProbWinBA := PBA;

      end;     {compute and output one EUWarReason dyad}

{ ------------------------------------------------------------ }

   begin    {main procedure compute_and_save_EUWarReason}
      distance_data := nil;
      try
         trace.enter('Entered compute_and_save_EUWarReason procedure');
         EUWRtrace := Ttrace_obj.init(trace.get_trace_level);
         if last_proc_year < first_proc_year then
            switch_year (first_proc_year, last_proc_year);

         preprocess_for_EUWarReason (configuration, files_exist, user_selections);
         if files_exist then
         begin
          try
             try
                assignFile (EUWarReason_file, EU_WR_File_Name);
                rewrite (EUWarReason_file);
                calculate_EUWarReason_partitions (first_proc_year, last_proc_year,
                                    main_num_partitions, main_years_per_partition);
                main_partition_start_year := first_proc_year;
                distance_data := Tdistance_array_obj.init (locations_file_name, user_selections, contiguity_data);

                if user_selections.custom_procedure = EUWarReasonComponents then
                   {to get the components, I just want them for some (joiner) dyads}
                   begin
                      {for now, do this just by reading the cow dyadic dispute data.\ \
                       I could in the future let user pick among the 3, but the procedure doesn't
                       actually get to the i/o window where they make that selection.}
                       COW_dyadic_dispute_data_30 := TDyadic_dispute_data_obj_integrated_format30.init (user_selections, configuration,
                                               configuration.first_MID_year, configuration.last_MID_year, cow_pre_1992);
                          {if Maoz_dyadic_disputes in user_selections.output_format.variables then
                             begin
                                Maoz_dispute_data := TDyadic_dispute_data_obj_integrated_format30.init (user_selections, configuration,
                                                        configuration.first_MID_year, configuration.last_MID_year, maoz_pre_1992);
                             end;
                          if COW_disputes in user_selections.output_format.variables then
                             begin
                                COW_dyadic_dispute_data_30 := TDyadic_dispute_data_obj_integrated_format30.init (user_selections, configuration,
                                                        configuration.first_MID_year, configuration.last_MID_year, cow_pre_1992);
                             end;
                          if ICB_crises in user_selections.output_format.variables then
                             ICB_crisis_data := TICBDyadic_dispute_data_obj.init (user_selections, configuration,
                                                configuration.first_ICB_year, configuration.last_ICB_year);  }
                       for main_partition_loop := 1 to main_num_partitions do
                       begin
                           main_partition_end_year := min((main_partition_start_year + main_years_per_partition - 1),
                                                    last_proc_year);
                           try
                              Read_data_for_EUWarReason (main_partition_start_year, main_partition_end_year, configuration,
                                                   tau_data, s_data, sys_capability_data, risk_data, user_selections);
                              For year_loop := main_partition_start_year to main_partition_end_year do
                                 begin
                                    for ccode1 := min_ccode to max_ccode do
                                      if nation_list.is_a_state (ccode1, year_loop) then
                                        for ccode2 := min_ccode to max_ccode do
                                          if nation_list.is_a_state (ccode2, year_loop) then
                                          {*** Need to add right here something about if it's a dyad I might want,
                                            like a dyad in a dispute.}
                                             begin
                                                  want_dyad := false;
                                                  if (COW_dyadic_dispute_data_30.Is_AnyYear_NonDir_Dispute_Originators(ccode1, ccode2, year_loop, adispnum, use_ccodeyear) or
                                                     COW_dyadic_dispute_data_30.Is_AnyYear_NonDir_Dispute_Joiners(ccode1, ccode2, year_loop, adispnum, use_ccodeyear)) then want_dyad := true;

                                                  {An alternate way to call would probably be the following, but it depends on settings if it gets joiners or not, so use the raw calls above.
                                                     if COW_dyadic_dispute_data_30.wanted_new_or_Continuing_NonDir_Dispute(ccode1, ccode2, year_loop, user_selections, adispnum, use_ccodeyear) then want_dyad := true;}

                                                     {if COW_disputes in user_selections.output_format.variables then
                                                        if COW_dyadic_dispute_data_30.wanted_new_or_Continuing_NonDir_Dispute(ccode1, ccode2, year_loop, user_selections, adispnum, use_ccodeyear) then want_dyad := true
                                                     else if Maoz_dyadic_disputes in user_selections.output_format.variables then
                                                        if Maoz_dispute_data.wanted_new_or_Continuing_NonDir_Dispute(ccode1, ccode2, year_loop, user_selections, adispnum, use_ccodeyear) then want_dyad := true
                                                     else if ICB_crises in user_selections.output_format.variables then
                                                        if ICB_crisis_data.wanted_new_or_Continuing_NonDir_Dispute(ccode1, ccode2, year_loop, user_selections, adispnum, use_ccodeyear) then want_dyad := true;   }
                                                  if want_dyad then
                                                     begin
                                                        EUWarReason_file_record.year := year_loop;
                                                        EUWarReason_file_record.ccode1 := ccode1;
                                                        EUWarReason_file_record.ccode2 := ccode2;
                                                        {Do both directions of the dyads, that I'm sure I can match it up
                                                         with whoever initiates the MID}
                                                        compute_one_EUWarReason_dyad(ccode1, ccode2,
                                                           year_loop, distance_data, tau_data, s_data,
                                                           sys_capability_data, risk_data,
                                                           EUWarReason_file_record.EUWarReason_rec, user_selections);
                                                        compute_one_EUWarReason_dyad(ccode2, ccode1,
                                                           year_loop, distance_data, tau_data, s_data,
                                                           sys_capability_data, risk_data,
                                                           EUWarReason_file_record.EUWarReason_rec, user_selections);
                                                        {Don't actually need to save to output file in this custom procedure:
                                                        write(EUWarReason_file, EUWarReason_file_record);    }
                                                        EUWRtrace.tick('Executing Procedure: EU (War and Reason), '+
                                                             inttostr(year_loop), nation_list.get_dyad_years);
                                                     end;
                                             end;      {check and output this dyad}
                                 end;   {for main_year_loop}
                           finally
                              delete_data_for_EUWarReason (tau_data, s_data, sys_capability_data, risk_data);
                              main_partition_start_year := main_partition_end_year + 1;
                           end;
                       end;   {for main_partition_loop [for if doing custom routine]}
                   end {if custom procedure}
                else
                   begin
                      for main_partition_loop := 1 to main_num_partitions do
                        begin
                           main_partition_end_year := min((main_partition_start_year + main_years_per_partition - 1),
                                                    last_proc_year);
                           try
                              Read_data_for_EUWarReason (main_partition_start_year, main_partition_end_year, configuration,
                                                   tau_data, s_data, sys_capability_data, risk_data, user_selections);
                              For year_loop := main_partition_start_year to main_partition_end_year do
                                 begin
                                    for ccode1 := min_ccode to max_ccode do
                                      if nation_list.is_a_state (ccode1, year_loop) then
                                        for ccode2 := min_ccode to max_ccode do
                                          if nation_list.is_a_state (ccode2, year_loop) then
                                             begin
                                                EUWarReason_file_record.year := year_loop;
                                                EUWarReason_file_record.ccode1 := ccode1;
                                                EUWarReason_file_record.ccode2 := ccode2;
                                                compute_one_EUWarReason_dyad(ccode1, ccode2,
                                                   year_loop, distance_data, tau_data, s_data,
                                                   sys_capability_data, risk_data,
                                                   EUWarReason_file_record.EUWarReason_rec, user_selections);
                                                write(EUWarReason_file, EUWarReason_file_record);
                                                EUWRtrace.tick('Executing Procedure: EU (War and Reason), '+
                                                     inttostr(year_loop), nation_list.get_dyad_years);
                                             end;
                                 end;   {for main_year_loop}
                           finally
                              delete_data_for_EUWarReason (tau_data, s_data, sys_capability_data, risk_data);
                              main_partition_start_year := main_partition_end_year + 1;
                           end;
                        end;   {for main_partition_loop}
                     end;
             finally
                EUWRtrace.tickdone;
                CloseFile(EUWarReason_file);
                distance_data.free;
             end;
          except
             on EUserInterrupt do raise;
             on EInOutError do
                begin
                   FileErrorBox.maindo ('Error opening/writing to output file "'+EU_WR_file_name+ '"',
                                        'File may be in use by another program, may be read-only, ',
                                        'or disk may be full.');
                   FileErrorBox.showmodal;
                   raise;
                end;
          end;

         end;     {if files don't exist, just exit.  }

      finally
          ShowMessage ('Computation of EUWarReason complete!');
          EUWRtrace.free;
          trace.exit ('Finished compute_EUWarReason procedure');
      end;
   end;    {main procedure compute_and_save_EUWarReason}

            { --------------------------------------- }

end.
