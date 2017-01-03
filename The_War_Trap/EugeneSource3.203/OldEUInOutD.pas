unit OldEUInOutD;

   {Old procedures from EUInoutD.pas, before converting to a single main output procedure}

interface

   procedure output_dyadic_data (const user_selections: user_selection_type; const nation_list :
          Tnation_array_obj; const contiguity_data : Tcontiguity_array_obj;
          const configuration : configuration_type);
   procedure output_data_by_dispute (const user_selections: user_selection_type; const nation_list :
          Tnation_array_obj; const contiguity_data : Tcontiguity_array_obj;
          const configuration : configuration_type);
   procedure output_monadic_data (const user_selections: user_selection_type;
             const nation_list : Tnation_array_obj; const configuration : configuration_type);  

implementation

          { ------------------------------------------------------------ }

   procedure output_dyadic_data (const user_selections: user_selection_type; const nation_list :
          Tnation_array_obj; const contiguity_data : Tcontiguity_array_obj;
          const configuration : configuration_type);
      {Using list of variables as specified by user, output those variables to the
       output file as dyads.  For directed and non-directed output.}

   var outfile : text;   {this is the main flat output file}
       wanted_dyad_list : Twanted_dyad_list_obj;
       main_num_partitions, main_years_per_partition, main_partition_loop : integer;
       main_partition_start_year, main_partition_end_year, main_year_loop : year_range;
       dyads_done, files_exist : boolean;
       main_ccode1, main_ccode2 : ccode_range;  {These are just for the #s for dyads in main loop}
       raw_capability_data : Traw_capability_array_obj;
       sys_capability_data : Tsys_capability_array_obj;
       distance_data : Tdistance_array_obj;
       minimum_distance_data : Tmindist_array_obj;
       tau_data : Ttau_array_obj;
       alliance_data : TAlliance_array_obj;
       s_data : Ts_array_obj;
       polity3_data : Tpolity_Array_obj;
       risk_Tau_data : Trisk_attitude_array_obj;
       risk_S_data : Trisk_attitude_array_obj;
       EUWarTrap_Tau_data : TEUWarTrap_array_obj;
       EUWarTrap_S_data : TEUWarTrap_array_obj;
       EUWarReason_Tau_data : TEUWarReason_array_obj;
       EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data : TEUWarReason_array_obj;
       COW_dyadic_disputes : TCOWDyadic_dispute_data_obj;
       Maoz_dispute_data : TMaoz_Dyadic_dispute_data_obj;
       User_Data_Sets : Multiple_user_data_set_type;
       dyads_output : longint;
       fileOK : boolean;
       Output_trace : Ttrace_obj;
       separator, outstring, outtypes: string;
       UserVarNamesAndMissingValuesList : varnameAndMissingValueArrayType;
       polnonpolnames : polnonpollisttype;
       year_index, ccode_index : integer;
       system_variables : Tsystem_variables_obj;

{ ------------------------------------------------------------ }

   function want_in_year (const ccode1, ccode2 : ccode_range; const year : year_range;
         const user_selections : user_selection_type; const nation_list : Tnation_array_obj) : boolean;
   var disp_num : dyadic_dispute_range;
       maoz_disp_num : dyadic_dispute_year_range;
       want_states, want_casesubset, want_ongoing, want_TgtVsInit, want_joining : boolean;
       new_disp_ab, new_disp_ba, is_joiner : boolean;

       {This procedure evaluates a given input ccode-ccode year to see if it is appropriate
        for directed-dyad-year output.  NOT called by dispute-year procedure.}
   begin
      want_in_year := false;

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
                  error ('Error - dyad selection type not seen in function "want_in_year".  Notify Programmer',
                             5, continue, error_log);
               end;
         end;       {case;  this is a wanted with case subsetting}


         {Now want to check the last three conditions iff user has selected MIDs to output}
         if COW_disputes in user_selections.output_format.variables then
            begin

              {first, create a marker for if there is a new dispute I want to think about}
               case user_selections.dispute_info.MarkSubsequentAsInitiation of
                  true:
                     case user_selections.output_this of
                        output_directed_dyads : new_disp_ab := COW_dyadic_disputes.wanted_new_or_continuing_initiation (ccode1, ccode2, year, user_selections, disp_num, use_ccodeyear);
                        output_nondirected_dyads : new_disp_ab := COW_dyadic_disputes.wanted_new_or_continuing_nondir_dispute (ccode1, ccode2, year, user_selections, disp_num, use_ccodeyear);
                     end;   {case}
                  false:
                     case user_selections.output_this of
                        output_directed_dyads : new_disp_ab := COW_dyadic_disputes.wanted_new_initiation (ccode1, ccode2, year, user_selections, disp_num, use_ccodeyear);
                        output_nondirected_dyads : new_disp_ab := COW_dyadic_disputes.wanted_new_nondir_dispute (ccode1, ccode2, year, user_selections, disp_num, use_ccodeyear);
                     end;   {case}
               end;  {case}


               {1.  check ongoing disputes}
               {Want to print this year if not ongoing, or if is ongoing but want ongoing, or
                if is ongoing and want ongoin if new disp and there is a new disp.}
               {don't want it if don't want any ongoing, and it's ongoing.
                Also don't want it if would take new disps, but it's not a new disp I want}
               if ((not (COW_dyadic_disputes.is_ongoing(ccode1, ccode2, year, disp_num)))
                    or
                   ((COW_dyadic_disputes.is_ongoing(ccode1, ccode2, year, disp_num)) and
                    (user_selections.output_format.printAllOngoing=true))
                    or
                   ((COW_dyadic_disputes.is_ongoing(ccode1, ccode2, year, disp_num)) and
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
                           true: new_disp_ba := COW_dyadic_disputes.wanted_new_or_continuing_initiation (ccode2, ccode1, year, user_selections, disp_num, use_ccodeyear);
                           false: new_disp_ba := COW_dyadic_disputes.wanted_new_initiation (ccode2, ccode1, year, user_selections, disp_num, use_ccodeyear);
                        end;  {case}
                        {new_disp_ab is set previously}

                        {first, if this direction is a new dispute, want it.}
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
                                  error ('Error - Want in year, checking for tgt vs. init dyads reaches logically impossible condition.  Notify Programmer of this EXACT message.',
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
                      is_joiner := COW_dyadic_disputes.Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, disp_num, use_ccodeyear);
                  output_directed_dyads :
                     case user_selections.dispute_info.SideAIsInitiator of
                      true:      {sideA}
                         is_joiner := (COW_dyadic_disputes.Is_AnyYear_Joined_Initiation (ccode1, ccode2, year, disp_num, use_ccodeyear) or
                                       COW_dyadic_disputes.Is_AnyYear_Joined_Targets (ccode1, ccode2, year, disp_num, use_ccodeyear));
                      false:    {Revisionist}
                         is_joiner := (COW_dyadic_disputes.Is_AnyYear_Joined_Revision (ccode1, ccode2, year, disp_num, use_ccodeyear) or
                                       COW_dyadic_disputes.Is_AnyYear_Joined_SQ (ccode1, ccode2, year, disp_num, use_ccodeyear));
                     end;   {case sideA is initiator}
               end;    {case output_this of...}

               if ((not (is_joiner))
                    or
                   ((is_joiner) and (user_selections.dispute_info.DropJoinerDirectedDyads=false))
                  )   then {want to print this dyad year}
                      {this leaves dropping for if is_joiner and DropJoinerDirectedDyads=true.}
                  want_joining := true;

            end   {COW MIDs selected, so check all}
         else

         if Maoz_dyadic_disputes in user_selections.output_format.variables then
            {NOTE:  this set of commands is identical to those for the COW dispute data
             above, but use the alternate dyadic dispute data set name.  There ought to be
             a better way to do this with objects, but it adds its own problems to do more
             than this.}
            begin

              {first, create a marker for if there is a new dispute I want to think about}
               case user_selections.dispute_info.MarkSubsequentAsInitiation of
                  true:
                     case user_selections.output_this of
                        output_directed_dyads : new_disp_ab := Maoz_dispute_data.wanted_new_or_continuing_initiation (ccode1, ccode2, year, user_selections, maoz_disp_num, use_ccodeyear);
                        output_nondirected_dyads : new_disp_ab := Maoz_dispute_data.wanted_new_or_continuing_nondir_dispute (ccode1, ccode2, year, user_selections, maoz_disp_num, use_ccodeyear);
                     end;   {case}
                  false:
                     case user_selections.output_this of
                        output_directed_dyads : new_disp_ab := Maoz_dispute_data.wanted_new_initiation (ccode1, ccode2, year, user_selections, maoz_disp_num, use_ccodeyear);
                        output_nondirected_dyads : new_disp_ab := Maoz_dispute_data.wanted_new_nondir_dispute (ccode1, ccode2, year, user_selections, maoz_disp_num, use_ccodeyear);
                     end;   {case}
               end;  {case}


               {1.  check ongoing disputes}
               {Want to print this year if not ongoing, or if is ongoing but want ongoing, or
                if is ongoing and want ongoin if new disp and there is a new disp.}
               {don't want it if don't want any ongoing, and it's ongoing.
                Also don't want it if would take new disps, but it's not a new disp I want}
               if ((not (Maoz_dispute_data.is_ongoing(ccode1, ccode2, year, maoz_disp_num)))
                    or
                   ((Maoz_dispute_data.is_ongoing(ccode1, ccode2, year, maoz_disp_num)) and
                    (user_selections.output_format.printAllOngoing=true))
                    or
                   ((Maoz_dispute_data.is_ongoing(ccode1, ccode2, year, maoz_disp_num)) and
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
                           true: new_disp_ba := Maoz_dispute_data.wanted_new_or_continuing_initiation (ccode2, ccode1, year, user_selections, maoz_disp_num, use_ccodeyear);
                           false: new_disp_ba := Maoz_dispute_data.wanted_new_initiation (ccode2, ccode1, year, user_selections, maoz_disp_num, use_ccodeyear);
                        end;  {case}
                        {new_disp_ab is set previously}

                        {first, if this direction is a new dispute, want it.}
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
                                  error ('Error - Want in year, checking for tgt vs. init dyads reaches logically impossible condition.  Notify Programmer of this EXACT message.',
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
                      is_joiner := Maoz_dispute_data.Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, maoz_disp_num, use_ccodeyear);
                  output_directed_dyads :
                     case user_selections.dispute_info.SideAIsInitiator of
                      true:      {sideA}
                         is_joiner := (Maoz_dispute_data.Is_AnyYear_Joined_Initiation (ccode1, ccode2, year, maoz_disp_num, use_ccodeyear) or
                                       Maoz_dispute_data.Is_AnyYear_Joined_Targets (ccode1, ccode2, year, maoz_disp_num, use_ccodeyear));
                      false:    {Revisionist}
                         is_joiner := (Maoz_dispute_data.Is_AnyYear_Joined_Revision (ccode1, ccode2, year, maoz_disp_num, use_ccodeyear) or
                                       Maoz_dispute_data.Is_AnyYear_Joined_SQ (ccode1, ccode2, year, maoz_disp_num, use_ccodeyear));
                     end;   {case sideA is initiator}
               end;    {case output_this of...}

               if ((not (is_joiner))
                    or
                   ((is_joiner) and (user_selections.dispute_info.DropJoinerDirectedDyads=false))
                  )   then {want to print this dyad year}
                      {this leaves dropping for if is_joiner and DropJoinerDirectedDyads=true.}
                  want_joining := true;

            end   {Maoz MIDs selected, so check all}

         else
            begin      {MIDs not selected, so those criteria always true}
               want_ongoing := true;
               want_TgtVsInit := true;
               want_joining := true;
            end;

         end;       {if want_states overall condition}
         
      {finally, only if all conditions are satisfied is the dyad-year one we want to include.}
      if (want_states and want_ongoing and want_casesubset and
          want_TgtVsInit and want_joining) = true then
         want_in_year := true;

   end;         {func want_in_year}

{ ------------------------------------------------------------ }

   function sampled (user_selections : user_selection_type; specific_disp_num : dyadic_dispute_range;
                     COW_dyadic_disputes : TCOWDyadic_dispute_data_obj; ccode1, ccode2 : ccode_range;
                     ayear : year_range; use_param : dyadic_call_parameter) : boolean;
     {returns true if we want a given data point.  This will be true if we are not sampling,
      or if we are sampling and we meet the right random test.}
      var disp_num : dyadic_dispute_range;
          maoz_disp_num : dyadic_dispute_year_range;
          init : boolean;
      begin
          case user_selections.sample_info.sampling of
            true : begin
                   {might want this data point.  First see if it's a dispute.}
                  {first, check and see if there is a dispute this dyad year}
                  disp_num := specific_disp_num;
                  if (ayear >= configuration.first_MID_year) and
                     (ayear <= configuration.last_MID_year) then
                     begin
                        {need to call differently if Maoz or COW disputes}
                        if Maoz_dyadic_disputes in user_selections.output_format.variables then
                           case user_selections.output_this of
                              output_directed_dyads : case user_selections.dispute_info.MarkSubsequentAsInitiation of
                                    true: init := Maoz_dispute_data.wanted_new_or_continuing_initiation (ccode1, ccode2, ayear, user_selections, maoz_disp_num, use_param);
                                    false: init := Maoz_dispute_data.wanted_new_initiation (ccode1, ccode2, ayear, user_selections, maoz_disp_num, use_param);
                                 end;
                              output_nondirected_dyads : case user_selections.dispute_info.MarkSubsequentAsInitiation of
                                    true: init := Maoz_dispute_data.wanted_new_or_continuing_NonDir_dispute (ccode1, ccode2, ayear, user_selections, maoz_disp_num, use_param);
                                    false: init := Maoz_dispute_data.wanted_new_NonDir_dispute (ccode1, ccode2, ayear, user_selections, maoz_disp_num, use_param);
                                 end;
                              end    {case usere .output_this of}
                        else    {use COW dispute data}
                           case user_selections.output_this of
                              output_directed_dyads : case user_selections.dispute_info.MarkSubsequentAsInitiation of
                                    true: init := COW_dyadic_disputes.wanted_new_or_continuing_initiation (ccode1, ccode2, ayear, user_selections, disp_num, use_param);
                                    false: init := COW_dyadic_disputes.wanted_new_initiation (ccode1, ccode2, ayear, user_selections, disp_num, use_param);
                                 end;
                              output_nondirected_dyads : case user_selections.dispute_info.MarkSubsequentAsInitiation of
                                    true: init := COW_dyadic_disputes.wanted_new_or_continuing_NonDir_dispute (ccode1, ccode2, ayear, user_selections, disp_num, use_param);
                                    false: init := COW_dyadic_disputes.wanted_new_NonDir_dispute (ccode1, ccode2, ayear, user_selections, disp_num, use_param);
                                 end;
                              end;    {case usere .output_this of}

                     end
                     else init := false;
                  if ((init) and (COW_disputes in user_selections.output_format.variables)) then
                     begin
                         if random <= user_selections.sample_info.proportion_dispute_dyads then
                           sampled := true
                        else sampled := false;
                     end
                  else  {this is a non-dispute dyad}
                     begin
                        if random <= user_selections.sample_info.proportion_non_dispute_dyads then
                           sampled := true
                        else sampled := false;
                     end;
               end;        {case sampling of true}
            false : sampled := true;
          end;  {case}
      end;

{ ------------------------------------------------------------ }

   begin   {main proc output_dyadic_data}
      trace.enter ('Entered main dyadic output loop');
      preprocess_for_output (configuration, user_selections, files_exist);
      wanted_dyad_list := nil;
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
      COW_dyadic_disputes := nil;
      Maoz_dispute_data := nil;
      User_Data_Sets := nil;
      system_variables := nil;


      try     {this is try for the except block}
         if files_exist then
         begin
           try
              Output_trace := Ttrace_obj.init(trace.get_trace_level);

                 {distance array only has to be read once, so it is outside "read data" proc}
                 {But, minimum distance must be read inside each partition}
              if ((distance in user_selections.output_format.variables) and
                 (user_selections.distance_method <> minimum)) or
                 (user_selections.dyads_selected = within_distance) then
                  distance_data := Tdistance_array_obj.init (configuration.distance_file_name, user_selections, contiguity_data);

                  {System level variables only have to be created once, so they are outside "read data" proc.}

              if (systemchars in user_selections.output_format.variables) then
              begin
                 sys_capability_data := Tsys_capability_array_obj.init
                    (configuration.cow_system_pct_file_name,
                     configuration.first_cap_year,
                     configuration.last_cap_year);
                 system_variables := Tsystem_variables_obj.init(sys_capability_data);
              end;

                     {wanted_dyad_list is helper list of dyads user wants to output.}
              wanted_dyad_list := Twanted_dyad_list_obj.init;
              wanted_dyad_list.update (user_selections);           {updated for dir/nondir}


                  {dyadic dispute array only has to be read once, so it is outside "read data" proc.}
                  {Must read dyadic disputes, either to output dispute data or to verify ongoing/not years}
                  {Note: COW_dyadic_disputes are read for all years, b/c there is no savings for
                   only reading subyears.  Also, dyadic disputes can be used for directed and
                   nondirected output.  Data is directed, but any direction can be recovered.}
                  {If the user has specifically selected Maoz_disputes, then use that, otherwise
                   use base set of dyadic disputes.}
              if Maoz_dyadic_disputes in user_selections.output_format.variables then
                 Maoz_dispute_data := TMaoz_Dyadic_dispute_data_obj.init (user_selections, configuration,
                      configuration.first_MID_year, configuration.last_MID_year)
              else COW_dyadic_disputes := TCOWDyadic_dispute_data_obj.init (user_selections, configuration,
                      configuration.first_MID_year, configuration.last_MID_year);

              calculate_output_partitions (user_selections, main_num_partitions, main_years_per_partition);
              main_partition_start_year := user_selections.first_year;
              dyads_output := 0;

              try
                 set_header (configuration, user_selections, separator, outstring, outtypes, UserVarNamesAndMissingValuesList, polnonpolnames);
                 if not (CommandFilesOnly in user_selections.output_format.commandfiles) then
                 begin
                    open_output (user_selections, outfile);   {in euinout unit}
                    if (user_selections.output_format.header = true) then
                      write_header (outfile, user_selections, outstring);

                    for main_partition_loop := 1 to main_num_partitions do
                    begin
                        main_partition_end_year := min(main_partition_start_year + main_years_per_partition,
                                                    user_selections.last_year);
                        try
                           Output_trace.tick('Executing Procedure: reading data for output, '+inttostr(main_partition_start_year)+' to '+inttostr(main_partition_end_year), 0);
                           Read_data (main_partition_start_year, main_partition_end_year,
                                      tau_data, alliance_data, s_data, polity3_data, sys_capability_data, raw_capability_data,
                                      risk_Tau_data, risk_S_data, EUWarTrap_Tau_data, EUWarTrap_S_data,
                                      EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                      minimum_distance_data, User_Data_Sets, user_selections, configuration);

                           {dyad year output}
                           For main_year_loop := main_partition_start_year to main_partition_end_year do
                              begin
                                 {For nondirected, the wanted_dyad_list constructed only cc1<cc2 dyads.
                                  For directed, both directsion are there.}
                                 wanted_dyad_list.get_first_dyad (main_ccode1, main_ccode2, dyads_done);
                                 if not(dyads_done) then
                                 repeat
                                    if want_in_year (main_ccode1, main_ccode2, main_year_loop,
                                                     user_selections, nation_list) then
                                      if sampled (user_selections, 0, COW_dyadic_disputes, main_ccode1,
                                                  main_ccode2, main_year_loop, use_ccodeyear) then
                                       begin
                                         output_one_record(outfile, main_ccode1, main_ccode2, main_year_loop,
                                                 distance_data, minimum_distance_data, tau_data, alliance_data, s_data, polity3_data, sys_capability_data,
                                                 raw_capability_data, risk_Tau_data, risk_S_data, EUWarTrap_Tau_data, EUWarTrap_S_data,
                                                 EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                                 User_Data_Sets, COW_dyadic_disputes, Maoz_dispute_data, system_variables,
                                                 configuration, user_selections, use_ccodeyear, 0);
                                         Output_trace.tick('Executing Procedure: Write final output ('+inttostr(main_year_loop)+')',
                                            wanted_dyad_list.get_num_dyad_years);
                                         inc(dyads_output);
                                       end;
                                    wanted_dyad_list.get_next_dyad (main_ccode1, main_ccode2, dyads_done);
                                 until dyads_done;
                              end;   {for main_year_loop}
                        finally
                           delete_data (main_partition_start_year, main_partition_end_year, tau_data, alliance_data,
                                        s_data, polity3_data, sys_capability_data, raw_capability_data,
                                        risk_Tau_data, risk_S_data, EUWarTrap_Tau_data, EUWarTrap_S_data,
                                        EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                        minimum_distance_data, User_Data_Sets, user_selections, configuration);
                           main_partition_start_year := main_partition_end_year + 1;
                        end;  {try finally inner loop}

                    end;   {for main_partition_loop}
                    close_output (user_selections, outfile);
                 end;      {command files only not... then}
                 if (user_selections.output_format.commandfiles <> []) then  {A previous check in the check_completeness proc verified that there were 1+ cmd file types specified.}
                   write_command_files (user_selections, dyads_output, separator, outstring, outtypes, UserVarNamesAndMissingValuesList, polnonpolnames);
              finally
                 Output_trace.tickdone;
                 trace.exit ('Finished main output loop');
                 if (user_selections.output_format.location = tofile)
                 then
                    ShowMessage ('EUGene output complete!  '+inttostr(dyads_output)+' cases written to file '+user_selections.output_format.output_file_name+'.')
                 else
                    ShowMessage ('EUGene output complete!  '+inttostr(dyads_output)+' cases written.');
              end;
           finally
              if (distance in user_selections.output_format.variables) and
                 (user_selections.distance_method <> minimum) then
                 distance_data.free;
              if Maoz_dyadic_disputes in user_selections.output_format.variables then
                 Maoz_dispute_data.free
              else COW_dyadic_disputes.free;
              wanted_dyad_list.free;
              Output_trace.tickdone;
              Output_trace.free;
           end;   {finally}
         end     {if files_exist}
         else
            begin
               ShowMessage ('Necessary files not found!');
            end;
      except   {this except goes with try open_file}
         {don't want to do anything with In/Out error, just exit the procedure.}
         {on EInOutError do
            begin
               raise;
            end;  }
         on EUserInterrupt do
            begin
               {showmessage ('user exception still raised at end of dyadic output');}
               {want the exception still raised outside this procedure}
               raise;
            end;
      end;    {except}

   end;

{ ------------------------------------------------------------ }

   procedure output_data_by_dispute (const user_selections: user_selection_type; const nation_list :
          Tnation_array_obj; const contiguity_data : Tcontiguity_array_obj;
          const configuration : configuration_type);
      {Using list of variables as specified by user, output those variables to the
       output file}

   var outfile : text;   {this is the main flat output file}
       main_num_partitions, main_years_per_partition, main_partition_loop : integer;
       main_partition_start_year, main_partition_end_year, main_year_loop : year_range;
       main_dispute_loop : dyadic_dispute_year_range;
       dyads_done, files_exist : boolean;
       main_ccode1, main_ccode2 : ccode_range;  {These are just for the #s for dyads in main loop}
       raw_capability_data : Traw_capability_array_obj;
       sys_capability_data : Tsys_capability_array_obj;
       distance_data : Tdistance_array_obj;
       minimum_distance_data : Tmindist_array_obj;
       tau_data : Ttau_array_obj;
       alliance_data : TAlliance_array_obj;
       s_data : Ts_array_obj;
       polity3_data : Tpolity_Array_obj;
       risk_Tau_data : Trisk_attitude_array_obj;
       risk_S_data : Trisk_attitude_array_obj;
       EUWarTrap_Tau_data : TEUWarTrap_array_obj;
       EUWarTrap_S_data : TEUWarTrap_array_obj;
       EUWarReason_Tau_data : TEUWarReason_array_obj;
       EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data : TEUWarReason_array_obj;
       COW_dyadic_disputes : TCOWDyadic_dispute_data_obj;
       Maoz_dispute_data : TMaoz_Dyadic_dispute_data_obj;
       User_Data_Sets : Multiple_user_data_set_type;
       fileOK : boolean;
       Output_trace : Ttrace_obj;
       num_dispute_years : longint;
       disputes_output : longint;
       separator, outstring, outtypes: string;
       UserVarNamesAndMissingValuesList : varnameAndMissingValueArrayType;
       polnonpolnames : polnonpollisttype;
       system_variables : Tsystem_variables_obj;

      { --------------------------------------------------}

   function want_in_dispute_year (const ccode1, ccode2 : ccode_range; const year : year_range;
         const user_selections : user_selection_type; const nation_list : Tnation_array_obj;
         const input_disp_num : dyadic_dispute_year_range) : boolean;
      var specific_disp_num : dyadic_dispute_range;
          maoz_disp_num : dyadic_dispute_year_range;
          is_joiner : boolean;

       {This procedure evaluates a given input dyadic dispute-year to see if it is appropriate
        for disp-year output.  NOT called by directed-dyad-year procedure.}

      begin
         {Don't have to check all the same things as in the dyad-year check function.
          There is not the same case subsetting option.
          Also don't care about target direction, b/c it's only initiations we're concerned about.
          Concerns about keep ongoing disputes, or not, handled via user choosing one case
             per dispute initiation, or one case per dispute year.}

          {Although they should be states to be in the COW MID list, there are some cases
           where there are mids with non-system members.  So I do need to check state status.
           Do that after the initial check of whether this is an initiation or not.}

         want_in_dispute_year := false;
         if Maoz_dyadic_disputes in user_selections.output_format.variables then
            maoz_disp_num := input_disp_num else
            specific_disp_num := input_disp_num;
         {this will also include joiners if specified that joiners=initiators.
          But note that the formation of the dyadic dispute list only sets up the
          initiating side vs. the target side as initiations.  So joiners can include
          only those joiners that joined the initiating side}
         if Maoz_dyadic_disputes in user_selections.output_format.variables then
            begin
               if ((user_selections.disputes_selected = all_disputes) and
                   (Maoz_dispute_data.wanted_new_initiation(ccode1, ccode2, year, user_selections, maoz_disp_num, use_dispute) ))
                   or
                  ((user_selections.disputes_selected = all_dispute_years) and
                   (Maoz_dispute_data.wanted_new_or_continuing_initiation(ccode1, ccode2, year, user_selections, maoz_disp_num, use_dispute) ))
                  then want_in_dispute_year := true;
            end
         else     {check COW data structure}
               if ((user_selections.disputes_selected = all_disputes) and
                   (COW_dyadic_disputes.wanted_new_initiation(ccode1, ccode2, year, user_selections, specific_disp_num, use_dispute) ))
                   or
                  ((user_selections.disputes_selected = all_dispute_years) and
                   (COW_dyadic_disputes.wanted_new_or_continuing_initiation(ccode1, ccode2, year, user_selections, specific_disp_num, use_dispute) ))
                  then want_in_dispute_year := true;


          {Check status as states}
         if not ( (nation_list.is_a_state (ccode1, year)) and
                  (nation_list.is_a_state (ccode2, year)) ) then want_in_dispute_year := false;


         {But also need to check if want to drop all joiner dyads, in which case it
          takes precedence and we drop it.}

         {check if this is a joiner dyad, and if I want to print joiner dyad-years.}
         {don't want it if don't want any joiners, and it's a joiner.}
         is_joiner := false;
         if Maoz_dyadic_disputes in user_selections.output_format.variables then
            begin
               case user_selections.dispute_info.SideAIsInitiator of
                   true:      {sideA}
                      is_joiner := (Maoz_dispute_data.Is_AnyYear_Joined_Initiation (ccode1, ccode2, year, maoz_disp_num, use_dispute) or
                                    Maoz_dispute_data.Is_AnyYear_Joined_Targets (ccode1, ccode2, year, maoz_disp_num, use_dispute));
                   false:    {Revisionist}
                      is_joiner := (Maoz_dispute_data.Is_AnyYear_Joined_Revision (ccode1, ccode2, year, maoz_disp_num, use_dispute) or
                                    Maoz_dispute_data.Is_AnyYear_Joined_SQ (ccode1, ccode2, year, maoz_disp_num, use_dispute));
                  end;   {case sideA is initiator}
            end
            else      {use COW dispute structure}
               case user_selections.dispute_info.SideAIsInitiator of
                   true:      {sideA}
                      is_joiner := (COW_dyadic_disputes.Is_AnyYear_Joined_Initiation (ccode1, ccode2, year, specific_disp_num, use_dispute) or
                                    COW_dyadic_disputes.Is_AnyYear_Joined_Targets (ccode1, ccode2, year, specific_disp_num, use_dispute));
                   false:    {Revisionist}
                      is_joiner := (COW_dyadic_disputes.Is_AnyYear_Joined_Revision (ccode1, ccode2, year, specific_disp_num, use_dispute) or
                                    COW_dyadic_disputes.Is_AnyYear_Joined_SQ (ccode1, ccode2, year, specific_disp_num, use_dispute));
                  end;   {case sideA is initiator}
         if (is_joiner) and (user_selections.dispute_info.DropJoinerDirectedDyads)
            then {want to drop this dispute year }
            want_in_dispute_year := false;

      end;  {func want in dispute year}

      { --------------------------------------------------}

   begin   {main proc output_data_by_dispute }
      try
         trace.enter ('Entered main dispute output loop');
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
         COW_dyadic_disputes := nil;
         Maoz_dispute_data := nil;
         User_Data_Sets := nil;
         system_variables := nil;


         preprocess_for_output (configuration, user_selections, files_exist);

         if files_exist then
         begin
           try
              Output_trace := Ttrace_obj.init(trace.get_trace_level);

                 {distance array only has to be read once, so it is outside "read data" proc}
                 {But, minimum distance must be read inside each partition}
              if (distance in user_selections.output_format.variables) and
                 (user_selections.distance_method <> minimum) then
                  distance_data := Tdistance_array_obj.init (configuration.distance_file_name, user_selections, contiguity_data);

                  {dyadic dispute array only has to be read once, so it is outside "read data" proc.}
                  {Must read dyadic disputes, either to output dispute data or to verify ongoing/not years}
                  {Note: COW_dyadic_disputes are read for all years, b/c there is no savings for
                       only reading subyears.}
              if Maoz_dyadic_disputes in user_selections.output_format.variables then
                 Maoz_dispute_data := TMaoz_Dyadic_dispute_data_obj.init (user_selections, configuration,
                      configuration.first_MID_year, configuration.last_MID_year)
              else COW_dyadic_disputes := TCOWDyadic_dispute_data_obj.init (user_selections, configuration,
                      configuration.first_MID_year, configuration.last_MID_year);

                  {System level variables only have to be created once, so they are outside "read data" proc.}

              if (systemchars in user_selections.output_format.variables) then
              begin
                 sys_capability_data := Tsys_capability_array_obj.init
                    (configuration.cow_system_pct_file_name,
                     configuration.first_cap_year,
                     configuration.last_cap_year);
                 system_variables := Tsystem_variables_obj.init(sys_capability_data);
              end;


              calculate_output_partitions (user_selections, main_num_partitions, main_years_per_partition);
              main_partition_start_year := user_selections.first_year;
                 try
                    set_header (configuration, user_selections, separator, outstring, outtypes, UserVarNamesAndMissingValuesList, polnonpolnames);
                    if not (CommandFilesOnly in user_selections.output_format.commandfiles) then
                    begin
                       open_output (user_selections, outfile);   {in euinout unit}
                       if (user_selections.output_format.header = true) then
                         write_header (outfile, user_selections, outstring);

                         {num dispute years is the number of passes that will be made through the data,
                          so will serve as my output counter}
                       if Maoz_dyadic_disputes in user_selections.output_format.variables then
                          num_dispute_years := main_num_partitions * Maoz_dispute_data.get_last_dispnum
                       else num_dispute_years := main_num_partitions * COW_dyadic_disputes.get_last_dispnum;
                       disputes_output := 0;
                       for main_partition_loop := 1 to main_num_partitions do
                         begin
                           main_partition_end_year := min(main_partition_start_year + main_years_per_partition,
                                                       user_selections.last_year);
                           try
                              Output_trace.tick('Executing Procedure: reading data for output, '+inttostr(main_partition_start_year)+' to '+inttostr(main_partition_end_year), 0);
                              Read_data (main_partition_start_year, main_partition_end_year,
                                         tau_data, alliance_data, s_data, polity3_data, sys_capability_data, raw_capability_data,
                                         risk_Tau_data, risk_S_data, EUWarTrap_Tau_data, EUWarTrap_S_data,
                                         EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                         minimum_distance_data, User_Data_Sets, user_selections, configuration);

                              {Do output by looping through all dyadic disputes.  For each dyadic dispute,
                               output each dyad-year that's in the partition range.}
                              {dyad year output}
                              if Maoz_dyadic_disputes in user_selections.output_format.variables then
                                 begin
                                 For main_dispute_loop := 1 to Maoz_dispute_data.get_last_dispnum do
                                    begin   {for every year of the dispute, if it's in range, process it}
                                       Output_trace.tick('Executing Procedure: Write final dispute output Maoz (' +
                                            inttostr(main_partition_start_year)+' to '+inttostr(main_partition_end_year)+')',
                                            num_dispute_years);
                                       main_year_loop := Maoz_dispute_data.get_year(main_dispute_loop);
                                       if  (main_year_loop >= main_partition_start_year) and
                                           (main_year_loop <= main_partition_end_year) then
                                             begin
                                                main_ccode1 := Maoz_dispute_data.get_ccode(main_dispute_loop, 0);
                                                main_ccode2 := Maoz_dispute_data.get_ccode(main_dispute_loop, 1);
                                                if want_in_dispute_year (main_ccode1, main_ccode2, main_year_loop,
                                                        user_selections, nation_list, main_dispute_loop) then
                                                   begin
                                                     output_one_record(outfile, main_ccode1, main_ccode2, main_year_loop,
                                                          distance_data, minimum_distance_data, tau_data, alliance_data, s_data, polity3_data,
                                                          sys_capability_data, raw_capability_data,
                                                          risk_Tau_data, risk_S_data,
                                                          EUWarTrap_Tau_data, EUWarTrap_S_data,
                                                          EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                                          User_Data_Sets, COW_dyadic_disputes, Maoz_dispute_data, system_variables,
                                                          configuration, user_selections, use_dispute, main_dispute_loop);
                                                     inc(disputes_output);
                                                   end;
                                             end;
                                    end;   {for main dispute loop}
                                 end     {if maoz output}
                              else         {cow dyadic dispute output}
                              For main_dispute_loop := 1 to COW_dyadic_disputes.get_last_dispnum do
                                 begin   {for every year of the dispute, if it's in range, process it}
                                    Output_trace.tick('Executing Procedure: Write final dispute output (' +
                                         inttostr(main_partition_start_year)+' to '+inttostr(main_partition_end_year)+')',
                                         num_dispute_years);
                                    for main_year_loop := COW_dyadic_disputes.get_first_year(main_dispute_loop) to
                                                     COW_dyadic_disputes.get_last_year(main_dispute_loop) do
                                       if ((main_year_loop >= main_partition_start_year) and
                                           (main_year_loop <= main_partition_end_year)) then
                                          begin
                                             main_ccode1 := COW_dyadic_disputes.get_ccode(main_dispute_loop, 0);
                                             main_ccode2 := COW_dyadic_disputes.get_ccode(main_dispute_loop, 1);
                                             if want_in_dispute_year (main_ccode1, main_ccode2, main_year_loop,
                                                     user_selections, nation_list, main_dispute_loop) then
                                                begin
                                                  output_one_record(outfile, main_ccode1, main_ccode2, main_year_loop,
                                                       distance_data, minimum_distance_data, tau_data, alliance_data, s_data, polity3_data,
                                                       sys_capability_data, raw_capability_data,
                                                       risk_Tau_data, risk_S_data,
                                                       EUWarTrap_Tau_data, EUWarTrap_S_data,
                                                       EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                                       User_Data_Sets, COW_dyadic_disputes, Maoz_dispute_data, system_variables,
                                                       configuration, user_selections, use_dispute, main_dispute_loop);
                                                  inc(disputes_output);
                                                end;
                                          end;
                                 end;   {for main dispute loop}
                           finally
                              delete_data (main_partition_start_year, main_partition_end_year, tau_data,alliance_data,
                                          s_data, polity3_data, sys_capability_data, raw_capability_data,
                                          risk_Tau_data, risk_S_data, EUWarTrap_Tau_data, EUWarTrap_S_data,
                                          EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                          minimum_distance_data, User_Data_Sets, user_selections, configuration);
                              main_partition_start_year := main_partition_end_year + 1;
                           end;      {try - for main dispute loop}
                         end;   {for main_partition_loop}
                       close_output (user_selections, outfile);
                    end;     {not command files only}
                    if (user_selections.output_format.commandfiles <> []) then
                      write_command_files (user_selections, disputes_output, separator, outstring, outtypes, UserVarNamesAndMissingValuesList, polnonpolnames);
                 finally
                    trace.exit ('Finished main output loop');
                    Output_trace.tickdone;
                    if (user_selections.output_format.location = tofile)
                    then
                       ShowMessage ('EUGene output complete!  '+inttostr(disputes_output)+' cases written to file '+user_selections.output_format.output_file_name+'.')
                    else
                       ShowMessage ('EUGene output complete!  '+inttostr(disputes_output)+' cases written.');
                 end;
           finally
              Output_trace.tickdone;
              Output_trace.free;
              if (distance in user_selections.output_format.variables) and
                 (user_selections.distance_method <> minimum) then
                    distance_data.free;
              if Maoz_dyadic_disputes in user_selections.output_format.variables then
                 Maoz_dispute_data.free
              else COW_dyadic_disputes.free;
           end;   {finally}
         end     {if files_exist}
         else
            begin
               ShowMessage ('Necessary files not found!');
            end;
      except   {this except goes with initial try }
         on EUserInterrupt do
            begin
               {need the exception still raised outside this procedure}
               raise;
            end;
      end;    {except}
   end;                 {proc output_by_dispute}

{ ------------------------------------------------------------ }

   procedure output_monadic_data (const user_selections: user_selection_type;
             const nation_list : Tnation_array_obj; const configuration : configuration_type);
      {Via this, can output country year data for capabilities, risk}
      {Using list of variables as specified by user, output those variables to the
       output file}

   var outfile : text;   {this is the main flat output file}
       main_num_partitions, main_years_per_partition, main_partition_loop : integer;
       main_partition_start_year, main_partition_end_year, main_year_loop : year_range;
       country_years_output, num_country_years_wanted : longint;
       main_ccode : ccode_range;  {These are just for the #s for dyads in main loop}
       raw_capability_data : Traw_capability_array_obj;
       sys_capability_data : Tsys_capability_array_obj;
       risk_Tau_data : Trisk_attitude_array_obj;
       risk_S_data : Trisk_attitude_array_obj;
       distance_data : Tdistance_array_obj;
       minimum_distance_data : Tmindist_array_obj;
       tau_data : Ttau_array_obj;
       alliance_data : TAlliance_array_obj;
       s_data : Ts_array_obj;
       polity3_data : Tpolity_Array_obj;
       EUWarTrap_Tau_data : TEUWarTrap_array_obj;
       EUWarTrap_S_data : TEUWarTrap_array_obj;
       EUWarReason_Tau_data : TEUWarReason_array_obj;
       EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data : TEUWarReason_array_obj;
       COW_dyadic_disputes : TCOWDyadic_dispute_data_obj;
       Maoz_dispute_data : TMaoz_Dyadic_dispute_data_obj;
       User_Data_Sets : Multiple_user_data_set_type;
       files_exist : boolean;
       fileoK : boolean;
       Output_trace : Ttrace_obj;
       separator, outstring, outtypes: string;
       UserVarNamesAndMissingValuesList : varnameAndMissingValueArrayType;
       polnonpolnames : polnonpollisttype;
       system_variables : Tsystem_variables_obj;

       {  -------------------------  }

   function num_wanted (const user_selections : user_selection_type) : longint;

   var region_counter: integer;

   begin
      case User_selections.monads_selected of
            all_states_mono :
             begin   {take # years as proportion of total years, multiply by total
                      # country years.  This will give an average country-years
                      expected.}
                num_wanted := round ((user_selections.last_year - user_selections.first_year + 1)
                             / (configuration.last_any_year - configuration.first_any_year+1)
                             * (nation_list.get_country_years) );
             end;

            selected_set_mono :
             begin
                num_wanted := (user_selections.last_year - user_selections.first_year + 1)
                           * user_selections.selected_country_list.num_countries;
             end;

            all_gp_mono :
               begin    {This is years * avg # of GPs.}
                num_wanted := 6 * (user_selections.last_year - user_selections.first_year + 1)
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
                  num_wanted := (user_selections.last_year - user_selections.first_year + 1) * 40 * region_counter;
               end;

            else
              begin
                error ('Error - mono selection type not specified in function "num_wanted".  Notify Programmer',
                       5, stop, error_log);
              end;
      end;   {case}
   end;    {function num_wanted}

{ ------------------------------------------------------------ }

function want_in_year (const ccode : ccode_range; const year : year_range;
         const user_selections : user_selection_type; const nation_list : Tnation_array_obj) : boolean;
   var ccodeloop : integer;
       found : boolean;
   begin
      want_in_year := false;
                {for all, they must be states}
      if (nation_list.is_a_state (ccode, year)) then
         begin
           case User_selections.monads_selected of
            all_states_mono :
             begin   {already know they are both states, so dyad is OK}
               want_in_year := true
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
                        want_in_year := true;
                        found := true;
                      end;
                    inc(ccodeloop);
                  end;
             end;

            all_gp_mono :
               begin    {check if both GPs}
                  if (nation_list.is_a_gp (ccode, year)) then
                  want_in_year := true;
               end;

            within_region_mono :
               begin
                 if nation_list.get_home_region(ccode) in user_selections.selected_regions then
                   want_in_year := true;
                 if globe in user_selections.selected_regions then
                   want_in_year := true;
               end;

            else
            begin
               error ('Error - mono selection type not specified in function "want_in_year".  Notify Programmer',
                          5, continue, error_log);
            end;
           end;      {case monad criterion}
         end;    {if both are states}

   end;         {func want_in_year}

{ ------------------------------------------------------------ }

   begin   {main proc output monadic}
      try
         trace.enter ('Entered main monadic output loop');
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
         COW_dyadic_disputes := nil;
         Maoz_dispute_data := nil;
         User_Data_Sets := nil;
         system_variables := nil;

         {System level variables only have to be created once, so they are outside "read data" proc.}
         if (systemchars in user_selections.output_format.variables) then
         begin
            sys_capability_data := Tsys_capability_array_obj.init
               (configuration.cow_system_pct_file_name,
                configuration.first_cap_year,
                configuration.last_cap_year);
            system_variables := Tsystem_variables_obj.init(sys_capability_data);
         end;


         preprocess_for_output (configuration, user_selections, files_exist);
         if files_exist then
         begin
            Output_trace := Ttrace_obj.init(trace.get_trace_level);
                  {wanted_dyad_list is helper list of dyads user wants to output.}
            calculate_output_partitions (user_selections, main_num_partitions, main_years_per_partition);
            main_partition_start_year := user_selections.first_year;
            country_years_output := 0;
            num_country_years_wanted := num_wanted (user_selections);
            try
              set_header (configuration, user_selections, separator, outstring, outtypes, UserVarNamesAndMissingValuesList, polnonpolnames);
              if not (CommandFilesOnly in user_selections.output_format.commandfiles) then
              begin
                 open_output (user_selections, outfile);
                 if (user_selections.output_format.header = true) then
                   write_header (outfile, user_selections, outstring);
                 for main_partition_loop := 1 to main_num_partitions do
                    begin
                       main_partition_end_year := min(main_partition_start_year + main_years_per_partition,
                                                    user_selections.last_year);
                       try
                          Output_trace.tick('Executing Procedure: reading data for output, '+inttostr(main_partition_start_year)+' to '+inttostr(main_partition_end_year), 0);
                          Read_data (main_partition_start_year, main_partition_end_year,
                                   tau_data, alliance_data, s_data, polity3_data, sys_capability_data, raw_capability_data,
                                   risk_Tau_data, risk_S_data, EUWarTrap_Tau_data, EUWarTrap_S_data,
                                   EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                   minimum_distance_data, User_Data_Sets, user_selections, configuration);

                          For main_year_loop := main_partition_start_year to main_partition_end_year do
                          begin
                             for main_ccode := min_ccode to max_ccode do
                                if want_in_year (main_ccode, main_year_loop, user_selections, nation_list) then
                                   begin
                                      output_one_record(outfile, main_ccode, 0, main_year_loop,
                                        distance_data, minimum_distance_data, tau_data, alliance_data, s_data, polity3_data,
                                        sys_capability_data, raw_capability_data,
                                        risk_Tau_data, risk_S_data,
                                        EUWarTrap_Tau_data, EUWarTrap_S_data,
                                        EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                        User_Data_Sets, COW_dyadic_disputes, Maoz_dispute_data, system_variables,
                                        configuration, user_selections, use_ccodeyear, 0);
                                      Output_trace.tick('Executing Procedure: Write final output ('+inttostr(main_year_loop)+')',
                                         num_country_years_wanted);
                                      inc(country_years_output);
                                    end;
                          end;   {for main_year_loop}
                       finally
                          delete_data (main_partition_start_year, main_partition_end_year, tau_data, alliance_data,
                                    s_data, polity3_data, sys_capability_data, raw_capability_data,
                                    risk_Tau_data, risk_S_data, EUWarTrap_Tau_data, EUWarTrap_S_data,
                                    EUWarReason_Tau_data, EUWarReason_S_unweighted_data, EUWarReason_S_weighted_data,
                                    minimum_distance_data, User_Data_Sets, user_selections, configuration);
                          main_partition_start_year := main_partition_end_year + 1;
                       end;   {try-finally }
                    end;   {for main_partition_loop}
                 close_output (user_selections, outfile);
              end;   {not command files only}
              if (user_selections.output_format.commandfiles <> []) then
                 write_command_files (user_selections, country_years_output, separator, outstring, outtypes, UserVarNamesAndMissingValuesList, polnonpolnames);
              trace.exit ('Finished main monadic output loop');

            finally
              Output_trace.tickdone;
              Output_trace.free;
              if (user_selections.output_format.location = tofile)
              then
                 ShowMessage ('EUGene output complete!  '+inttostr(country_years_output)+' cases written to file '+user_selections.output_format.output_file_name+'.')
              else
                 ShowMessage ('EUGene output complete!  '+inttostr(country_years_output)+' cases written.');
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
   end;


end.
 