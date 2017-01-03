
                          {Now that I've set dates from this incidnet, check to see if the end
                           of this incident (just processed) is the same as a state end in MIDB,
                           but if there are more incidents for the state.
                          This is a sign of a multiple entry/exit for the state.  In this case, we must
                          end this dyadic MID record here, and start again at the next incident for the
                          next dyadic MID.  }

 {                         must_split := false;
                          if (country_disputes.get_num_country_recs_for_state(final_dyadic_mid.idnum, final_dyadic_mid.ccodeA) > 1) then
                             for mid_episode := 1 to country_disputes.get_num_country_recs_for_state(final_dyadic_mid.idnum, final_dyadic_mid.ccodeA) do
                                begin
                                   country_disp_num_to_check := Country_Disputes.get_country_disp_num_when_many (final_dyadic_mid.idnum, final_dyadic_mid.ccodeA, mid_episode);
                                   If ((final_dyadic_mid.EndYear = country_disputes.get_endyear(country_disp_num_to_check)) and
                                       (final_dyadic_mid.EndMonth = country_disputes.get_endmonth(country_disp_num_to_check)) and
                                       (final_dyadic_mid.EndDay = country_disputes.get_endday(country_disp_num_to_check)) ) and
                                       (not (x=end_incident_rec) )  then
                                      begin
                                         {This is the case where we need to split the MID.}
{                                         trace.message ('Think we found a multi-entry mid for '+inttostr(final_dyadic_mid.ccodeA)+' vs. '+ inttostr(final_dyadic_mid.ccodeB)+' in MID # '+inttostr(final_dyadic_mid.idnum));
                                         must_split := true;
                                      end;
                                end;
                          if (country_disputes.get_num_country_recs_for_state(final_dyadic_mid.idnum, final_dyadic_mid.ccodeB) > 1) then
                             for mid_episode := 1 to country_disputes.get_num_country_recs_for_state(final_dyadic_mid.idnum, final_dyadic_mid.ccodeB) do
                                begin
                                   country_disp_num_to_check := Country_Disputes.get_country_disp_num_when_many (final_dyadic_mid.idnum, final_dyadic_mid.ccodeB, mid_episode);
                                   If ((final_dyadic_mid.EndYear = country_disputes.get_endyear(country_disp_num_to_check)) and
                                       (final_dyadic_mid.EndMonth = country_disputes.get_endmonth(country_disp_num_to_check)) and
                                       (final_dyadic_mid.EndDay = country_disputes.get_endday(country_disp_num_to_check)) ) and
                                       (not (x=end_incident_rec) )  then
                                      begin
                                         {This is the case where we need to split the MID.}
{                                         trace.message ('Think we found a multi-entry mid for '+inttostr(final_dyadic_mid.ccodeA)+' vs. '+ inttostr(final_dyadic_mid.ccodeB)+' in MID # '+inttostr(final_dyadic_mid.idnum));
                                         must_split := true;
                                      end;
                                end;

{** need to check to see if I need to move another incident up, if there is another one
with the same end date.

** it doesn't seem to be pickin up 2nd to 3rd records.
}
