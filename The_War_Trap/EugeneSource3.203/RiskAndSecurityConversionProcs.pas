unit RiskAndSecurityConversionProcs;

interface
   uses sysutils, windows, dialogs, messages, controls, forms,
        CmnProcD, EUtypes1, EUtypes2,
        OutWindow, originalriskunit, RiskProgrammerWarnings;

procedure riskfileconvert;
procedure risk_data_show;
procedure riskfile_initial_security_alliance_file_create;
procedure security_data_show (asecuritydatafilename : TFileName);

implementation

   uses TraceUnit, mdiframe;

   { -------------------------------------------   }

   procedure riskfileconvert;
    {converts risk data file from that in v1 to v2}
   var risk_data : Trisk_attitude_array_obj;
       new_risk_file : risk_file_type_v2;
       risk_file_record : ^risk_file_record_type_v2;
       year_loop : year_range;
       ccode : ccode_range;
       loop_region: region_type;
       old_ccode_array : risk_ccode_array_type;
       ccode_index_num, alliance_spot : ccode_index_range;
       aregion : region_range;
       alliance_data : Talliance_array_obj;
       risk_convert_trace : TTrace_obj;

{          risk_file_record_type_v2 = packed record
          year : year_range;
          ccode_from_index_list : ccode_index_array;
          ccode_array : risk_ccode_array_type;
       end;                                           }

   begin
      showmessage ('this converts risk data from v1 to v2;  output file set in code.');
      risk_convert_trace := Ttrace_obj.init(trace.get_trace_level);

     { alliance_data := Talliance_array_obj.init (configuration.cow_alliance_file_name,
                    configuration.alliance_seq_file_name, configuration.dyadic_alliance_file_name,
                    configuration.first_any_year, configuration.last_any_year,  flat_dyadic);  }

     {set to convert risk with tau only in new version 1.9+}
      risk_data := Trisk_attitude_array_obj.init (configuration.risk_Tau_file_name,
                configuration.risk_WTR_file_name, configuration.first_any_year,
                configuration.last_any_year, risk_EUGENE);
      assignFile (new_risk_file, 'f:\eugene\riskv2.dat');
      rewrite (new_risk_file);
      new (risk_file_record);
      For year_loop  := configuration.first_any_year to configuration.last_any_year do
         begin
            risk_file_record^.year := year_loop;
            risk_file_record^.ccode_from_index_list := ccode_index.return_ccode_list;
            old_ccode_array := risk_data.get_ccode_array(year_loop);
            for ccode_index_num := min_ccode_index to max_ccode_index do
               for aregion := europe to globe do
                  with risk_file_record^.ccode_array[ccode_index_num][aregion] do
                     begin
                        risk := old_ccode_array[ccode_index_num][aregion].risk;
                        security := old_ccode_array[ccode_index_num][aregion].security;
                        secmaxsum := old_ccode_array[ccode_index_num][aregion].secmaxsum;
                        secminsum := old_ccode_array[ccode_index_num][aregion].secminsum;
                        risk_convert_trace.tick('risk convert, '+inttostr(year_loop),
                                                (configuration.last_any_year-configuration.first_any_year)*max_ccode_index*ord(globe));
{                        for alliance_spot := min_ccode_index to max_ccode_index do
                           begin
                              best_alliance[alliance_spot] := alliance_data.get_alliance_value(ccode_index.ccode(ccode_index_num),ccode_index.ccode(alliance_spot),year_loop);
                              worst_alliance[alliance_spot] := alliance_data.get_alliance_value(ccode_index.ccode(ccode_index_num),ccode_index.ccode(alliance_spot),year_loop);
                           end;  }
                     end;

            write (new_risk_file, risk_file_record^);
         end;
      close (new_risk_file);
      risk_convert_trace.tickdone;
      risk_convert_trace.free;

      risk_data_show;

   end;

   { -------------------------------------------   }

   procedure risk_data_show;
   var
       new_risk_data : Trisk_attitude_array_obj_v2;
       year_loop : year_range;
       ccode : ccode_range;
       loop_region: region_type;
       val1, outstring : string;
        yr1, yr2 : year_range;
   begin
      showmessage ('this shows a few years of risk data;  years and input file set in code.');
      yr1 := 1816;
      yr2 := 1816;

      {configuration.risk_Tau_file_name,}
      new_risk_data := Trisk_attitude_array_obj_v2.init ('d:\eugene\RiskSubsetS18161816.dat',
                configuration.risk_WTR_file_name, yr1,
                yr2, risk_EUGENE);
      {  'd:\eugene\combinedtaurisk.dat'
         'd:\eugene\intermediate data files\riskv2tau.dat'
         'd:\eugene\risksubsettau19851988.dat'   }

      OutputWindow.Screen_Output.clear;
      OutputWindow.show;

      For year_loop  :=  yr1 to yr2 do
         for ccode := min_ccode to max_ccode do
           if (nation_list.is_a_state (ccode, year_loop)) then
             begin
               outstring := inttostr(year_loop) +' '+inttostr(ccode)+' ';
               for loop_region := europe to americas do
                  begin
                     str (new_risk_data.get_risk (ccode, year_loop, loop_region):8:6, val1);
                     outstring := outstring + inttostr(ccode) + ' '+ inttostr(year_loop)+' '+val1 + ' ';
                  end;
               outputWindow.Screen_Output.lines.add(outstring);
             end;
        end;  {proc}


   { -------------------------------------------   }

   procedure riskfile_initial_security_alliance_file_create;
   var sec_file_record : security_alliance_file_record_type;
       new_sec_file : security_alliance_file_type;
       year_loop : year_range;
       ccode : ccode_range;
       loop_region: region_type;
       aregion : region_range;
       alliance_data : Talliance_array_obj;
       sec_convert_trace : TTrace_obj;
       ccode_index_num, alliance_spot : ccode_index_range;
       SecurityFileOverwriteWarning: TSecurityFileOverwriteWarning;


{          risk_file_record_type_v2 = packed record
          year : year_range;
          ccode_from_index_list : ccode_index_array;
          ccode_array : risk_ccode_array_type;
       end;                                           }

      {creates an initial security file where best, worst alliances are set to
       the actual alliance pattern.}

   begin
      {This will overwrite any existing security data file}
      try
         SecurityFileOverwriteWarning := TSecurityFileOverwriteWarning.Create(application);
         SecurityFileOverwriteWarning.overwriteWarningMemo.clear;
         SecurityFileOverwriteWarning.overwriteWarningMemo.text :=
            'This procedure will overwrite any existing security data file named '+
            configuration.security_alliance_Tau_file_name+' with a new '+
            'file of baseline security/alliance information, saved to fiel with Tau name, where the alliances in the file '+
            'are new alliances with the values of the states actual alliance patterns.  '+
            'Do you really want to overwrite any existing security file? ';
         SecurityFileOverwriteWarning.showmodal;
         if SecurityFileOverwriteWarning.modalresult = mrOK then
            begin
               sec_convert_trace := Ttrace_obj.init(trace.get_trace_level);

               alliance_data := Talliance_array_obj.init (configuration.cow_alliance_file_name,
                             configuration.alliance_seq_file_name, configuration.dyadic_alliance_file_name,
                             configuration.first_any_year, configuration.last_any_year, flat_dyadic);

               assignFile (new_sec_file, configuration.security_alliance_Tau_file_name);
               rewrite (new_sec_file);
               For year_loop  := configuration.first_any_year to configuration.last_any_year do
                  begin
                     sec_file_record.year := year_loop;
                     sec_file_record.ccode_from_index_list := ccode_index.return_ccode_list;

                     for ccode_index_num := min_ccode_index to max_ccode_index do
                        for aregion := europe to globe do
                           with sec_file_record.ccode_array[ccode_index_num][aregion] do
                              begin
                                 sec_convert_trace.tick('sec create, '+inttostr(year_loop),
                                                         (configuration.last_any_year-configuration.first_any_year)*max_ccode_index*ord(globe));
                                 for alliance_spot := min_ccode_index to max_ccode_index do
                                    begin
                                       best_alliance[alliance_spot] := initialized_value;
                                       worst_alliance[alliance_spot] := initialized_value;
                                    end;
                                 for alliance_spot := min_ccode_index to max_ccode_index do
                                    begin
                                       best_alliance[alliance_spot] := alliance_data.get_alliance_value(ccode_index.ccode(ccode_index_num),ccode_index.ccode(alliance_spot),year_loop);
                                       worst_alliance[alliance_spot] := alliance_data.get_alliance_value(ccode_index.ccode(ccode_index_num),ccode_index.ccode(alliance_spot),year_loop);
                                    end;
                              end;

                     write (new_sec_file, sec_file_record);
                  end;
               close (new_sec_file);
               sec_convert_trace.tickdone;
               sec_convert_trace.free;

               security_data_show (configuration.security_alliance_Tau_file_name);
            end
         else
            Showmessage ('Cancelling procedure');
      finally;
         SecurityFileOverwriteWarning.Free;
      end;   {finally}
   end;

 { -------------------------------------------   }

   procedure security_data_show (asecuritydatafilename : TFileName);
   var
       sec_data : Trisk_stored_security_alliance_obj;
       year_loop : year_range;
       ccode2, ccode : ccode_range;
       loop_region: region_type;
       val1, outstring : string;
       yr1, yr2 : year_range;
   begin

      showmessage ('this shows a few years of the best security/alliance data, for states vis-a-vis each region;  years and input file set in code.'+
                   '  Code could be changed to show actual, or worst, data.');

      OutputWindow.Screen_Output.clear;
      OutputWindow.show;

      yr1 := 1816;
      yr2 := 1817;
      sec_data := Trisk_stored_security_alliance_obj.init (asecuritydatafilename, yr1, yr2);

      For year_loop  :=  yr1 to yr2 do
         for ccode := min_ccode to max_ccode do
           if (nation_list.is_a_state (ccode, year_loop)) then
             begin
               for loop_region := europe to americas do
                  begin
                     outstring := inttostr(year_loop) +' '+inttostr(ccode)+' ';
                     outstring := outstring + inttostr(ord(loop_region)) +' ';
                     for ccode2 := min_ccode to max_ccode do
                       if (nation_list.is_a_state (ccode2, year_loop)) then
                       begin
                          str (sec_data.get_best_alliance (ccode, ccode2, year_loop, loop_region):1, val1);
                          outstring := outstring + val1 + ' ';
                       end;
                      outputWindow.Screen_Output.lines.add(outstring);
                  end;
             end;
        end;  {proc}

end.
