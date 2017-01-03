unit eumaind;

{EUGene  Copyright 1997, 1998-2003+  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

 {Main unit, expected utility calculation program}
{  This program will read COW data and perform expected utility calculations,
   outputting a data set of dyadic expected utility information for a user-selected
   set of dyads.

 Program begun March 1996 by Scott Bennett.

 Main code is executed through the EUGene32 project file; base types are in EUTypes1 unit.
}


{This main unit has the main loop for the user selection of procedures to run.
 It also has initialization procedures and calls to read data.}

interface

uses windows, dialogs, sysutils, filectrl, forms, math, PagedOutput,
     cmnprocD, eutypes1, eutypes2, eutypes3, euprocs1, euinoutD;
   {eutypes1, 2... has type/object/variable definitions, and data object methods.
    euinoutd has procs for window creation, and selection from menus by users.
       It also has final data output and formatting procedures.
    euhelp will eventually have the program's help system.
    euprocs1, 2, etc. has actual main data manipulation procedures
    PagedOutput has initialization routine for user data var boxes}

procedure initialize_data (var configuration : configuration_type;
                           var ccode_index : Tccode_index_obj; var nation_list : Tnation_array_obj;
                           var contiguity_data : Tcontiguity_array_obj; var error_log : text;
                           var debug : debug_array_type;
                           var user_selections : user_selection_type);

procedure close_down (var error_log : text; var nation_list : Tnation_array_obj;
                      var contiguity_data : Tcontiguity_array_obj;
                      var ccode_index : Tccode_index_obj);

implementation

uses TraceUnit;

{ ------------------------------------------------------------ }

procedure set_configuration_entry (env_name, path_name : string; var configuration : configuration_type);
   begin
      {Now process the environment name and path name}
      {Note: many of these paths hang off the main directory.  But in case the
       user reorders the commands, we can't add the main directory path here.
       Wait until after all lines are processed to do that.}
      {if env_name = 'eugene_directory' then configuration.eugene_directory := path_name}
      with configuration do     {because not ordinal, must do with if-then}
         if env_name ='user_data_subdirectory' then user_data_files_subdirectory_name := path_name
         else if env_name ='raw_cow_capabilities_data' then cow_raw_cap_file_name := path_name
         else if env_name ='modified_cow_capabilities_data' then cow_modified_cap_file_name := path_name
         else if env_name ='distance_data' then distance_file_name := path_name
         else if env_name ='colonial_contiguity_data' then colonial_contiguity_file_name := path_name
         else if env_name ='nation_data' then nation_list_file_name := path_name
         else if env_name ='nations_with_different_ccode_data' then same_state_diff_ccode_list_file_name := path_name
         else if env_name ='major_power_data' then major_list_file_name := path_name
         else if env_name ='cow_alliance_data' then cow_alliance_file_name := path_name
         else if env_name ='alliance_seq_data' then alliance_seq_file_name :=path_name
         else if env_name ='dyadic_alliance_data' then dyadic_alliance_file_name := path_name
         else if env_name ='tau_data' then tau_file_name := path_name
         else if env_name ='s_data' then s_file_name := path_name
         else if env_name ='polity3_data' then polity3_file_name := path_name
         else if env_name ='risk_Tau_data' then risk_tau_file_name := path_name
         else if env_name ='risk_S_unweighted_data' then risk_S_unweighted_file_name := path_name
         else if env_name ='WTR_risk_data' then risk_wtr_file_name := path_name
         else if env_name ='syscap_data' then cow_system_pct_file_name := path_name
         else if env_name ='security_alliance_Tau_data' then security_alliance_Tau_file_name := path_name
         else if env_name ='security_alliance_S_unweighted_data' then security_alliance_S_unweighted_file_name := path_name
         else if env_name ='EU_War_and_Reason_Tau_data' then EUWarReason_Tau_file_name := path_name
         else if env_name ='EU_War_and_Reason_S_unweighted_data' then EUWarReason_S_unweighted_file_name := path_name
         else if env_name ='EU_War_and_Reason_S_weighted_data' then EUWarReason_S_weighted_file_name := path_name
         else if env_name ='EU_War_Trap_Tau_data' then EUWarTrap_Tau_file_name := path_name
         else if env_name ='EU_War_Trap_S_unweighted_data' then EUWarTrap_S_unweighted_file_name := path_name
         else if env_name ='MID_Case_Data' then cow_mid_case_file_nameA := path_name
         else if env_name ='MID_Actor_Data' then cow_mid_actor_file_nameB := path_name
         else if env_name ='MID_Name_Data' then cow_mid_name_file_nameC := path_name
         else if env_name ='MID_Participant_Incident_Data' then cow_mid_participant_incident_file_name := path_name
         else if env_name ='MID_Participant_Incident_Data_First_Year' then
            cow_MID_Participant_Incident_Data_First_Year := strtoint(path_name)
         else if env_name ='MID_Format_2_1_Data_Last_year' then
            cow_MID_Format_2_1_Data_Last_year := strtoint(path_name)
         else if env_name ='MID_Data_format' then
            begin
               if path_name = '2.1' then cow_mid_data_format := disputeformat21
               else if path_name = '3.0' then cow_mid_data_format := disputeformat30
               else
                  begin
                     {Note:  Did have a call to the "error" procedure here, but the error_log
                      is not yet open for reading, so that failed.  Just pop up a message for now.}
                     ShowMessage ('Error reading eugene.ini:  value for MID_Data_format not recognized.  Check for up to date installation and eugene.ini file.  Continuing, but errors may result.');
                     cow_mid_data_format := disputeformat30;
                  end;
            end
         else if env_name ='Maoz_Dyadic_MID_Data' then maoz_dyadic_mid_file_name := path_name
         else if env_name ='ICB_Dyadic_Crisis_Data' then ICB_dyadic_file_name := path_name
         else if env_name ='COW_War_Data' then COW_war_file_name := path_name
         else if env_name ='werner_peace_years_file_name' then werner_peace_years_file_name := path_name
         else if env_name ='ISO_main_file' then ISO_main_file := path_name
         else if env_name ='ISO_lookup_file' then ISO_lookup_file :=path_name
         else if env_name ='Browser_lookup_file' then Browser_lookup_file := path_name
         else if env_name ='Label_lookup_file' then Label_lookup_file := path_name
         else if env_name ='Minimum_Distance_Data' then mindist_file_name := path_name
         else if env_name ='EUGene_ftp_site' then EUGene_ftp_site_name := path_name
         else if env_name ='Saved_Settings' then saved_settings_file_name := path_name
         else if env_name ='bibliography' then bibliography_file_name := path_name
         else if env_name ='error_file' then error_file_name := path_name
         else if env_name ='HelpFiles_FileMenu_name' then HelpFiles_FileMenu_name := path_name
         else if env_name ='HelpFiles_RecomputeMenu_name' then HelpFiles_RecomputeMenu_name := path_name
         else if env_name ='HelpFiles_TraceMenu_name' then HelpFiles_TraceMenu_name := path_name
         else if env_name ='HelpFiles_OutputMenu_name' then HelpFiles_OutputMenu_name := path_name
         else if env_name ='HelpFiles_HelpMenu_name' then HelpFiles_HelpMenu_name := path_name
         else if env_name ='HelpFiles_RecalcBox_name' then HelpFiles_RecalcBox_name := path_name
         else if env_name ='HelpFiles_DistanceBox_name' then HelpFiles_DistanceBox_name := path_name
         else if env_name ='HelpFiles_OnscreenWindows_name' then HelpFiles_OnscreenWindows_name := path_name
         else if env_name ='HelpFiles_DestinationPage_name' then HelpFiles_DestinationPage_name := path_name
         else if env_name ='HelpFiles_SamplingPage_name' then HelpFiles_SamplingPage_name := path_name
         else if env_name ='HelpFiles_CasePage_name' then HelpFiles_CasePage_name := path_name
         else if env_name ='HelpFiles_VariablePage_name' then HelpFiles_VariablePage_name := path_name
         else if env_name ='HelpFiles_DisputePage_name' then HelpFiles_DisputePage_name := path_name
         else if env_name ='HelpFiles_ExclusionPage_name' then HelpFiles_ExclusionPage_name := path_name
         else if env_name ='HelpFiles_UserData_name' then HelpFiles_UserData_name := path_name
         else if env_name ='first_capability_year' then first_cap_year := strtoint(path_name)
         else if env_name ='last_capability_year' then last_cap_year := strtoint(path_name)
         else if env_name ='first_alliance_year' then first_alliance_year := strtoint(path_name)
         else if env_name ='last_alliance_year' then last_alliance_year := strtoint(path_name)
         else if env_name ='first_alliance_seq_year' then first_alliance_seq_year := strtoint(path_name)
         else if env_name ='last_alliance_seq_year' then last_alliance_seq_year := strtoint(path_name)
         else if env_name ='first_polity3_year' then first_polity3_year := strtoint(path_name)
         else if env_name ='last_polity3_year' then last_polity3_year := strtoint(path_name)
         else if env_name ='first_nation_year' then first_nation_year := strtoint(path_name)
         else if env_name ='last_nation_year' then last_nation_year := strtoint(path_name)
         else if env_name ='first_MID_year' then first_MID_year := strtoint(path_name)
         else if env_name ='last_MID_year' then last_MID_year := strtoint(path_name)
         else if env_name ='first_ICB_year' then first_ICB_year := strtoint(path_name)
         else if env_name ='last_ICB_year' then last_ICB_year := strtoint(path_name)
         else if env_name ='first_contiguity_year' then first_contiguity_year := strtoint(path_name)
         else if env_name ='last_contiguity_year' then last_contiguity_year := strtoint(path_name)
         else if env_name ='first_mindist_year' then first_mindist_year := strtoint(path_name)
         else if env_name ='last_mindist_year' then last_mindist_year := strtoint(path_name)
         else if env_name ='first_wtr_risk_year' then first_wtr_risk_year := strtoint(path_name)
         else if env_name ='last_wtr_risk_year' then last_wtr_risk_year := strtoint(path_name)
         else
            begin
               {Note:  Did have a call to the "error" procedure here, but the error_log
                is not yet open for reading, so that failed.  Just pop up a message for now.}
               {EUGeneError ('Error - unrecognized environment variable name seen ',1, continue, error_log);}
               showmessage ('Error - unrecognized environment variable name seen, unrecognized name was ' + env_name
                            + '.  Has EUGene.ini file been modified?  If not, notify programmer of apparent incompatible program versions.');
            end;     {not recognized;  ignore it.}
   end;

{ ------------------------------------------------------------ }

procedure initialize_data (var configuration : configuration_type;
                           var ccode_index : Tccode_index_obj; var nation_list : Tnation_array_obj;
                           var contiguity_data : Tcontiguity_array_obj; var error_log : text;
                           var debug : debug_array_type;
                           var user_selections : user_selection_type);
    {1.  read paths and info from configuration file.
     2.  Read generic (necessary always) data input, index array and nations info,
         and distances.
     3.  Initialize menu and input selection variables}

   var config_file : text;
       config_file_name : TFileName;
       bufsize : word;
       winloc : pchar;
       yr, month, day, hour, minute, second, sec100 : word;
       present : tdatetime;
       dlevel : integer;
       x : integer;
       error_log_path, error_log_name : string;

   procedure read_process_config_line (var config_file : text;
                                       var configuration : configuration_type);
          {this will read one line of the configuration file, parse it, and if it's
           an appropriate environment/configuration variable, set it.}
      var env_name, path_name : string;
          achar : char;
      begin
         {Get first character}
         env_name := '';
         path_name := '';
         repeat
            if (not eof(config_file)) and (not eoln(config_file)) then
            read (config_file,achar);
         until ( (achar <> ' ') and (achar <> chr(9))) or
               ( (eof(config_file)) or (eoln(config_file)) ) ;
         if (achar = '[') or (achar = ';') then
            begin
                {it's a comment line, so skip it}
            end
         else
            begin
                {saw a real character, which means this should be an environment variable.}
                {read the rest of the line, and parse it.}
               env_name := achar;
               repeat
                  if (not eof(config_file)) and (not eoln(config_file)) then
                     read (config_file,achar);
                  if ( (achar <> ' ') and (achar <> chr(9)) and (achar <> '=')) then
                     env_name := env_name + achar;
               until ( (achar = ' ') or (achar = chr(9)) or (achar = '=')) or
                     ( (eof(config_file)) or (eoln(config_file)) ) ;

               { Now, look for a " mark to indicate the beginning of the environment value}
               while (achar <> '"') and (not eof(config_file)) and (not eoln(config_file)) do
                  read (config_file, achar);

               if achar = '"' then   {now read the rest of the file name, up to the next "}
               repeat
                  if (not eof(config_file)) and (not eoln(config_file)) then
                     read (config_file,achar);
                  if (achar <> '"') then path_name := path_name + achar;
               until (achar = '"') or (eof(config_file)) or (eoln(config_file)) ;

               set_configuration_entry (env_name, path_name, configuration);

            end;
         if (not eof(config_file)) then readln (config_file);
      end;     {read and process a config file line}

                     { -------------------------------}

   begin         {main procedure initialize_data}
      try
         try
            trace.enter ('Entered initialize_data procedure');
            ccode_index := nil;
            nation_list := nil;
            contiguity_data := nil;

            {Before doing any processing, set up the debug levels and tracing of program}
            {initially, set all debugging to on}
            for dlevel := 0 to max_debug_level do debug[dlevel] := true;
            debug[10] := false;

            {First just initialize/blank out configuration data.}
            configuration.eugene_directory := 'BLANKDir';
            configuration.user_data_files_directory := 'BLANKDir';
            configuration.user_data_files_subdirectory_name := 'BLANKSubDir';
            configuration.error_file_name := 'BLANK';
            configuration.nation_list_file_name := 'BLANK';
            configuration.major_list_file_name := 'BLANK';
            configuration.same_state_diff_ccode_list_file_name := 'BLANK';
            configuration.cow_raw_cap_file_name := 'BLANK';
            configuration.cow_modified_cap_file_name := 'BLANK';
            configuration.cow_system_pct_file_name := 'BLANK';
            configuration.distance_file_name := 'BLANK';
            configuration.colonial_contiguity_file_name := 'BLANK';
            configuration.cow_alliance_file_name := 'BLANK';
            configuration.alliance_seq_file_name := 'BLANK';
            configuration.dyadic_alliance_file_name := 'BLANK';
            configuration.tau_file_name := 'BLANK';
            configuration.s_file_name := 'BLANK';
            configuration.polity3_file_name := 'BLANK';
            configuration.risk_Tau_file_name := 'BLANK';
            configuration.risk_S_unweighted_file_name := 'BLANK';
            configuration.risk_wtr_file_name := 'BLANK';
            configuration.security_alliance_Tau_file_name := 'BLANK';
            configuration.security_alliance_S_unweighted_file_name := 'BLANK';
            configuration.EUWarReason_Tau_file_name := 'BLANK';
            configuration.EUWarReason_S_unweighted_file_name := 'BLANK';
            configuration.EUWarReason_S_weighted_file_name := 'BLANK';
            configuration.EUWarTrap_Tau_file_name := 'BLANK';
            configuration.EUWarTrap_S_unweighted_file_name := 'BLANK';
            configuration.cow_mid_case_file_nameA := 'BLANK';
            configuration.cow_mid_actor_file_nameB := 'BLANK';
            configuration.cow_mid_name_file_nameC := 'BLANK';
            configuration.cow_mid_participant_incident_file_name := 'BLANK';
            configuration.cow_MID_Participant_Incident_Data_First_Year := min_year;
            configuration.cow_MID_Format_2_1_Data_Last_year := min_year;
            configuration.cow_mid_data_format := disputeformat21;
            configuration.maoz_dyadic_mid_file_name := 'BLANK';
            configuration.ICB_dyadic_file_name := 'BLANK';
            configuration.COW_war_file_name := 'BLANK';
            configuration.werner_peace_years_file_name := 'BLANK';
            configuration.ISO_main_file := 'BLANK';
            configuration.ISO_lookup_file := 'BLANK';
            configuration.browser_lookup_file := 'BLANK';
            configuration.label_lookup_file := 'BLANK';
            configuration.mindist_file_name := 'BLANK';
            configuration.EUGene_ftp_site_name := 'BLANK';
            configuration.saved_settings_file_name := 'BLANK';
            configuration.bibliography_file_name := 'BLANK';
            configuration.HelpFiles_FileMenu_name := 'BLANK';
            configuration.HelpFiles_RecomputeMenu_name := 'BLANK';
            configuration.HelpFiles_OutputMenu_name := 'BLANK';
            configuration.HelpFiles_TraceMenu_name := 'BLANK';
            configuration.HelpFiles_HelpMenu_name  := 'BLANK';
            configuration.HelpFiles_RecalcBox_name  := 'BLANK';
            configuration.HelpFiles_DistanceBox_name  := 'BLANK';
            configuration.HelpFiles_OnscreenWindows_name  := 'BLANK';
            configuration.HelpFiles_DestinationPage_name := 'BLANK';
            configuration.HelpFiles_CasePage_name := 'BLANK';
            configuration.HelpFiles_SamplingPage_name := 'BLANK';
            configuration.HelpFiles_VariablePage_name := 'BLANK';
            configuration.HelpFiles_DisputePage_name := 'BLANK';
            configuration.HelpFiles_ExclusionPage_name := 'BLANK';
            configuration.first_cap_year := min_year;
            configuration.last_cap_year := min_year;
            configuration.first_nation_year := min_year;
            configuration.last_nation_year := min_year;
            configuration.first_alliance_year := min_year;
            configuration.last_alliance_year := min_year;
            configuration.first_alliance_seq_year := min_year;
            configuration.last_alliance_seq_year := min_year;
            configuration.first_polity3_year := min_year;
            configuration.last_polity3_year := min_year;
            configuration.first_wtr_risk_year := min_year;
            configuration.last_wtr_risk_year := min_year;
            configuration.first_MID_year := min_year;
            configuration.last_MID_year := min_year;
            configuration.first_ICB_year := min_year;
            configuration.last_ICB_year := min_year;
            configuration.first_contiguity_year := min_year;
            configuration.last_contiguity_year := min_year;
            configuration.first_mindist_year := min_year;
            configuration.last_mindist_year := min_year;


            {Main Eugene directory is directory where the main executable is}
            configuration.eugene_directory := ExtractFilePath(application.exename);
            config_file_name := configuration.eugene_directory + initialization_file_name;

            {User input files will come from a specified subdirectory}

            {Very first version did install of config info into windows directory}
              {First, basic input from configuration file}
              {Start by getting path to windows directory.  }
            {winloc := stralloc (255);
            bufsize := GetWindowsDirectory (winloc, 255);
              {If 255 was too small, do again with bigger calculated buffer value}
            {if bufsize = 0 then bufsize := GetWindowsDirectory (winloc, bufsize);
               {This returns windows directory without a trailing backslash.}
              {Now, search for and get path to ini file in Windows directory}
              {Note:  the name of the initialization file is defined as a const in eutypobj.}
            {config_file_name := strPas (winloc) + '\' + initialization_file_name;}
            {config_file_name := strPas (winloc) + '\' + initialization_file_name;
            strdispose (winloc);  }

            try
               if not (FileExists (config_file_name)) then
                  begin
                     ShowMessage ('Fatal Error - Configuration file not found at location '+config_file_name+'.  ');
                     ShowMessage ('Configuration File or Program must be re-installed to this location.  ');
                     ShowMessage ('EUGene will now exit.');
                     halt;
                  end;

               trace.message('      Reading configuration file...');
               try
                  assignFile (config_file, config_file_name);
                  reset (config_file);
                  repeat
                     read_process_config_line (config_file, configuration);
                  until eof(config_file);
               finally
                  CloseFile (config_file);
               end;
            except
               on EInOutError do
                 begin
                     ShowMessage ('Fatal Error - Cannot open configuration file '+config_file_name+'.  ');
                     ShowMessage ('Configuration File or Program must be re-installed to this location.  ');
                     ShowMessage ('EUGene will now exit.');
                     halt;
                 end;
            end;

            {First thing to do at this point is open the error log,
             in the main program directory.}
            {$ifdef prod_run}
               configuration.error_file_name := 'c:\temp\errorpro.log';
            {$else}
               error_log_path := ExtractFilePath(configuration.error_file_name);
               error_log_name := ExtractFileName(configuration.error_file_name);
               if error_log_path = '' then  {empty main path, so error file goes in eugene installation directory}
                  configuration.error_file_name := configuration.eugene_directory + error_log_name
               else
                  begin
                    {main path not empty, so is on a different drive or in another directory}
                    {just leave the configuration file name the same!}
                    if not directoryexists(error_log_path) then
                       if not createdir(error_log_path) then
                          begin
                             {If can't create the directory as specified by the user, then create
                              file under temp directory.}
                             ShowMessage ('Error log path specified as '+ error_log_path +
                                       ' in eugene.ini, but this path does not exist and could not be created.  ' +
                                       'Please specify a new directory path in eugene.ini and rerun EUGene.');
                             halt;
                          end;
                  end;
            {$endif}
            try
               assignFile (error_log, configuration.error_file_name);
               if FileExists (configuration.error_file_name) then
                  begin
                     append (error_log);
                  end
               else
                  begin
                     rewrite (error_log);
                     write (error_log, 'Cumulative error log for Eugene program.  Log initially created at ');
                     present := now;
                     DecodeDate (present, yr, month, day);
                     DecodeTime (present, hour, minute, second, sec100);
                     writeln (error_log, hour:2,':',minute,' on ',month:2,'/',day:2,'/',yr:4);
                     writeln (error_log);
                  end;
            except
               on EInOutError do
                 begin
                    {Couldn't write anywhere else, now, try to write error log to windows/temp.}
                    try
                       {Start by getting path to windows directory.  }
                       winloc := stralloc (255);
                       bufsize := GetWindowsDirectory (winloc, 255);
                       {If 255 was too small, do again with bigger calculated buffer value}
                       if bufsize = 0 then bufsize := GetWindowsDirectory (winloc, bufsize);
                       {This returns windows directory without a trailing backslash.}
                       error_log_path := strPas (winloc) +'\temp';
                       if not directoryexists(error_log_path) then
                          if not createdir(error_log_path) then
                             begin
                                {in trouble here; can't create or use windows/temp}
                             end;
                       {Now, whether successful or not, try to use windows/temp as log location.
                        If that failed, will raise an exception.}
                       configuration.error_file_name := strPas (winloc) + '\temp\' + error_log_name;
                       assignFile (error_log, configuration.error_file_name);
                       if FileExists (configuration.error_file_name) then append (error_log)
                       else
                           begin
                              rewrite (error_log);
                              write (error_log, 'Cumulative error log for Eugene program.  Log initially created at ');
                              present := now;
                              DecodeDate (present, yr, month, day);
                              DecodeTime (present, hour, minute, second, sec100);
                              writeln (error_log, hour:2,':',minute,' on ',month:2,'/',day:2,'/',yr:4);
                              writeln (error_log);
                           end;
                    except
                       ShowMessage ('Fatal Error - Cannot open error log file '+configuration.error_file_name+'.  Is the file open in another program, or read-only?  ');
                       ShowMessage ('EUGene must be able to write to a log file to execute.  Please identify a writeable log file name and enter it in the "error_file" field within EUGene.ini.  Then rerun EUGene.');
                    end;
                 end;
            end;


            {In the .ini file, only the config file path name may be complete.  Other paths
             must combine that name with the installation directory path to
             get the complete file name.}

               {User input files will come from a specified subdirectory}
            configuration.user_data_files_directory := configuration.eugene_directory +
                                                  configuration.user_data_files_subdirectory_name;

            configuration.nation_list_file_name := configuration.eugene_directory +
                                                      configuration.nation_list_file_name;
            configuration.major_list_file_name := configuration.eugene_directory +
                                                      configuration.major_list_file_name;
            configuration.same_state_diff_ccode_list_file_name := configuration.eugene_directory +
                                                      configuration.same_state_diff_ccode_list_file_name;
            configuration.cow_raw_cap_file_name := configuration.eugene_directory +
                                                      configuration.cow_raw_cap_file_name;
            configuration.cow_modified_cap_file_name := configuration.eugene_directory +
                                                      configuration.cow_modified_cap_file_name;
            configuration.cow_system_pct_file_name := configuration.eugene_directory +
                                                      configuration.cow_system_pct_file_name;
            configuration.distance_file_name := configuration.eugene_directory +
                                                      configuration.distance_file_name;
            configuration.colonial_contiguity_file_name   := configuration.eugene_directory +
                                                configuration.colonial_contiguity_file_name ;
            configuration.cow_alliance_file_name   := configuration.eugene_directory +
                                                configuration.cow_alliance_file_name ;
            configuration.alliance_seq_file_name   := configuration.eugene_directory +
                                                configuration.alliance_seq_file_name ;
            configuration.dyadic_alliance_file_name   := configuration.eugene_directory +
                                                configuration.dyadic_alliance_file_name ;
            configuration.tau_file_name := configuration.eugene_directory +
                                           configuration.tau_file_name;
            configuration.s_file_name := configuration.eugene_directory +
                                           configuration.s_file_name;
            configuration.polity3_file_name := configuration.eugene_directory +
                                           configuration.polity3_file_name;
            configuration.security_alliance_Tau_file_name := configuration.eugene_directory +
                                                configuration.security_alliance_Tau_file_name;
            configuration.security_alliance_S_unweighted_file_name := configuration.eugene_directory +
                                                configuration.security_alliance_S_unweighted_file_name;
            configuration.risk_Tau_file_name := configuration.eugene_directory +
                                                configuration.risk_Tau_file_name;
            configuration.risk_S_unweighted_file_name := configuration.eugene_directory +
                                                configuration.risk_S_unweighted_file_name;
            configuration.risk_wtr_file_name   := configuration.eugene_directory +
                                                configuration.risk_wtr_file_name ;
            configuration.EUWarTrap_Tau_file_name := configuration.eugene_directory +
                                                configuration.EUWarTrap_Tau_file_name;
            configuration.EUWarTrap_s_unweighted_file_name := configuration.eugene_directory +
                                                configuration.EUWarTrap_s_unweighted_file_name;
            configuration.EUWarReason_Tau_file_name := configuration.eugene_directory +
                                                configuration.EUWarReason_Tau_file_name;
            configuration.EUWarReason_S_unweighted_file_name := configuration.eugene_directory +
                                                configuration.EUWarReason_S_unweighted_file_name;
            configuration.EUWarReason_S_weighted_file_name := configuration.eugene_directory +
                                                configuration.EUWarReason_S_weighted_file_name;
            configuration.cow_mid_case_file_nameA := configuration.eugene_directory +
                                                configuration.cow_mid_case_file_nameA;
            configuration.cow_mid_actor_file_nameB := configuration.eugene_directory +
                                                configuration.cow_mid_actor_file_nameB;
            configuration.cow_mid_name_file_nameC := configuration.eugene_directory +
                                                configuration.cow_mid_name_file_nameC;
            configuration.cow_mid_participant_incident_file_name := configuration.eugene_directory +
                                                configuration.cow_mid_participant_incident_file_name;
            configuration.maoz_dyadic_mid_file_name := configuration.eugene_directory +
                                                configuration.maoz_dyadic_mid_file_name;
            configuration.ICB_dyadic_file_name := configuration.eugene_directory +
                                                configuration.ICB_dyadic_file_name;
            configuration.COW_war_file_name := configuration.eugene_directory +
                                                configuration.COW_war_file_name;
            configuration.werner_peace_years_file_name := configuration.eugene_directory +
                                                configuration.werner_peace_years_file_name;
            configuration.ISO_main_file := configuration.eugene_directory + configuration.ISO_main_file;
            configuration.ISO_lookup_file := configuration.eugene_directory + configuration.ISO_lookup_file;
            configuration.browser_lookup_file := configuration.eugene_directory + configuration.browser_lookup_file;
            configuration.label_lookup_file := configuration.eugene_directory + configuration.label_lookup_file;
            configuration.mindist_file_name := configuration.eugene_directory +
                                                configuration.mindist_file_name;
            configuration.saved_settings_file_name := configuration.eugene_directory +
                                                configuration.saved_settings_file_name;
            configuration.bibliography_file_name := configuration.eugene_directory +
                                                configuration.bibliography_file_name;
            configuration.HelpFiles_FileMenu_name  := configuration.eugene_directory +
                                                configuration.HelpFiles_FileMenu_name ;
            configuration.HelpFiles_RecomputeMenu_name  := configuration.eugene_directory +
                                                configuration.HelpFiles_RecomputeMenu_name ;
            configuration.HelpFiles_OutputMenu_name  := configuration.eugene_directory +
                                                configuration.HelpFiles_OutputMenu_name ;
            configuration.HelpFiles_TraceMenu_name  := configuration.eugene_directory +
                                                configuration.HelpFiles_TraceMenu_name ;
            configuration.HelpFiles_HelpMenu_name  := configuration.eugene_directory +
                                                configuration.HelpFiles_HelpMenu_name ;
            configuration.HelpFiles_RecalcBox_name   := configuration.eugene_directory +
                                                configuration.HelpFiles_RecalcBox_name ;
            configuration.HelpFiles_DistanceBox_name   := configuration.eugene_directory +
                                                configuration.HelpFiles_DistanceBox_name ;
            configuration.HelpFiles_OnscreenWindows_name := configuration.eugene_directory +
                                                configuration.HelpFiles_OnscreenWindows_name ;
            configuration.HelpFiles_DestinationPage_name   := configuration.eugene_directory +
                                                configuration.HelpFiles_DestinationPage_name ;
            configuration.HelpFiles_SamplingPage_name   := configuration.eugene_directory +
                                                configuration.HelpFiles_SamplingPage_name ;
            configuration.HelpFiles_CasePage_name   := configuration.eugene_directory +
                                                configuration.HelpFiles_CasePage_name ;
            configuration.HelpFiles_VariablePage_name   := configuration.eugene_directory +
                                                configuration.HelpFiles_VariablePage_name ;
            configuration.HelpFiles_DisputePage_name   := configuration.eugene_directory +
                                                configuration.HelpFiles_DisputePage_name ;
            configuration.HelpFiles_ExclusionPage_name   := configuration.eugene_directory +
                                                configuration.HelpFiles_ExclusionPage_name ;
            configuration.HelpFiles_UserData_name   := configuration.eugene_directory +
                                                configuration.HelpFiles_UserData_name ;{configuration.error_file_name   fixed above}


            trace.message('Finished Reading and processing configuration file.');


            {Now set the first and last year for possible processing, based on the
             config file info about first and last capability, alliance, nation data.}
            configuration.last_eu_year_possible :=
                         minintvalue([configuration.last_cap_year, configuration.last_alliance_year,
                                 configuration.last_nation_year]);
            configuration.first_eu_year_possible :=
                         maxintvalue([configuration.first_cap_year, configuration.first_alliance_year,
                            configuration.first_nation_year]);

            configuration.first_risk_year := max(configuration.first_cap_year, configuration.first_alliance_year);
            configuration.last_risk_year := min(configuration.last_cap_year, configuration.last_alliance_year);

            {a limiting factor is COW nation data.}
            {Note, it will normally be impossible for any data to range outside that of the
             nation year.  But this can serve as a check on input data.}
            {when we check years for output, for runs, etc., will usually use nation_year.}
            configuration.first_any_year := min (configuration.first_nation_year,
               minintvalue([configuration.first_cap_year, configuration.first_alliance_year,
                  configuration.first_MID_year, configuration.first_polity3_year]));
            configuration.last_any_year := max (configuration.last_nation_year,
               maxintvalue([configuration.last_cap_year, configuration.last_alliance_year,
                  configuration.last_MID_year, configuration.last_polity3_year] ));

            {config .first, last wtr risk year read and set without modification.}

            {Read and set user supplied data file information}
            configuration.User_data_set_info := Tuser_data_set_listing_obj.init (configuration.user_data_files_directory);


                 {User selections}
            user_selections.complete := false;

            user_selections.dyads_selected := not_selected;
            user_selections.monads_selected := not_selected_mono;
            user_selections.disputes_selected := not_selected_disputes;

            user_selections.user_specified_dyad_list.file_name := configuration.eugene_directory + 'userdyad.txt';
            user_selections.user_specified_dyad_list.first_dyad := nil;
         
            user_selections.years := all;
            user_selections.first_year := configuration.first_nation_year;
            user_selections.last_year := configuration.last_nation_year;

            user_selections.output_format.header := true;
            user_selections.output_format.printii := false;
            user_selections.output_format.printAllOngoing := false;
            user_selections.output_format.printOngoingIfNewDisp := false;
            user_selections.output_format.output_set := false;
            user_selections.output_format.location := tonone;
            user_selections.output_format.output_file_name := configuration.eugene_directory + 'temp.out';
            user_selections.output_format.overwrite := false;
            user_selections.output_format.separator := tab;
            user_selections.output_format.commandFiles := [];


            {only these initially}
            user_selections.output_format.variables := [ccodes, year];
            {user_selections.output_format.polity3_variables := [];}

            user_selections.selected_country_list.num_countries := 0;
            for x := 1 to max_countries do
               user_selections.selected_country_list.data[x] := 0;

            user_selections.output_this := output_none;
            user_selections.compute_this := compute_none;
            user_selections.distance_method := capitols_contiguity_war_trap;
            user_selections.contiguity_level_required := 1;
            user_selections.maximum_distance := 12500;
            include(user_selections.selected_regions, globe);
            user_selections.alliance_data_source := flat_dyadic;
            user_selections.risk_data_source := risk_EUGENE;
            user_selections.capability_modifications := modified_capability;
            user_selections.werner_peace_year_adjustment := false;
            user_selections.risk_calculation_info.method := use_steepest;
            user_selections.risk_calculation_info.num_in_pop := 50;
            user_selections.risk_calculation_info.mutate_probability := 0.05;
            user_selections.risk_calculation_info.generations_to_be_stable := 8;
            user_selections.risk_calculation_info.num_from_previous_to_keep := 3;
            user_selections.risk_calculation_info.risk_search_tolerance := 0.01;
            user_selections.risk_calculation_info.random_risk_iterations := 1;
            user_selections.similarity_method := use_tau;
            user_selections.s_weighting := unweighted;

            user_selections.eu_calculation_info.adjusted_phi := false;
            user_selections.eu_calculation_info.equilibrium_solution := induction;
            user_selections.eu_calculation_info.game_variant := original;

            user_selections.tau_leader_calculation_info := global;
            user_selections.s_leader_calculation_info := global;

            user_selections.dispute_info.OnlyTrueInitiators := true;
            user_selections.dispute_info.JoinersOnInitiatingSideAsInitiators := false;
            user_selections.dispute_info.AllJoinersAsInitiators := false;
            user_selections.dispute_info.SideAIsInitiator := true;
            user_selections.dispute_info.MarkSubsequentAsInitiation := false;
            {Of the next two, one must always start false and one true b/c they toggle}
            user_selections.dispute_info.UseMostSeriousDispute := true;
            user_selections.dispute_info.UseFirstDispute := false;
            {Of the next two, one must always start false and one true b/c they toggle}
            user_selections.dispute_info.AlwaysIncludeTgtVsInitiator := false;
            user_selections.dispute_info.IncludeTgtVsInitiatoriffNew := true;
            user_selections.conflict_exclusion_selection := cds_none;

            user_selections.dispute_info.DropJoinerDirectedDyads := false;

            user_selections.sample_info.sampling := false;
            user_selections.sample_info.proportion_dispute_dyads := 1.0;
            user_selections.sample_info.proportion_non_dispute_dyads := 1.0;
            user_selections.sample_info.use_randseed := false;
            user_selections.sample_info.randseed := 0;

            user_selections.user_data_sets_selections := nil;
            setlength(user_selections.user_data_sets_selections,configuration.User_data_set_info.get_num_data_sets);

            user_selections.custom_procedure := not_custom;

                {Initialize ccode index, nation data, distance data}
                {Must do ccode index before calling nation_list.init}
            ccode_index := Tccode_index_obj.init(configuration.nation_list_file_name);
            nation_list := Tnation_array_obj.init (configuration);
            nation_list.calculate_n;
            contiguity_data := Tcontiguity_array_obj.init (configuration.colonial_contiguity_file_name);

            Randomize;        {necessary to randomize for risk-generation procedures}

         finally
            trace.exit ('Exiting initialize_data procedure');
         end;
      except
         raise;
      end;    {except}
   end;

{ ------------------------------------------------------------ }

procedure close_down (var error_log : text; var nation_list : Tnation_array_obj;
                      var contiguity_data : Tcontiguity_array_obj;
                      var ccode_index : Tccode_index_obj);
   {This proc should close any open files, and delete any remaining pointer
    data structures on the heap.  All of the EU data should be destroyed
    before reaching here.  So, nation_list, ccode_index left.
    Also error file.}
   begin
      {all of these should be executed, without being in a try, so that none are skipped}
      try
         try
            trace.enter('Entered close_down procedure');
         finally
            contiguity_data.free;
            nation_list.free;
            ccode_index.free;
            configuration.User_data_set_info.free;
            trace.exit ('Finishing close down procedure');
            CloseFile (error_log);
         end;
      except
          {if there's an exception now, don't worry about it, program is over anyway}
      end;
   end;    {proc close_down}

{ ------------------------------------------------------------ }

end.    {unit}
