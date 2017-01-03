unit CodeForUserCheckBoxes;

interface
   {Assume we have main settings tab.  It has several tabs, output settings, etc.  One of these is has
    the "variables" tabs under it;  This TTabsheet is called "VarTab".  Under the VarTab sheet,
    there are sub grouped tabs for different data sets.  These are TPageControls.
    Assume further that one of the sheets on the PageControl is called
    UserVarTab.  We will work from this base.  UserVarTab must have on it a further TTabsheet,
    call it UserVarTabSheet.
    Each Page of the UserVarTabSheet will be a dynamically defined page for a user data set.}

type
  {This will have to be modified, probably.  Previously, just kept the number of the original variable.
   Now, probably need to keep the # of the original data set, and the variable number.}
  {Original:  }
  TVarFormCheckBoxRecord = record
        original_var_num : integer;
        ACheckBox : TCheckBox;
     end;
  {New ???:  }
  TVarFormCheckBoxRecord = record
        original_user_dataset_num : integer;
        original_user_var_num : integer;
        ACheckBox : TCheckBox;
     end;

  TVarFormCheckBoxes = array of TVarFormCheckBoxRecord;


{We might or might not need some structure like this to save information about the tabs.
 This kept information about each of the variables displayed on each tab, if there were
 further subtabs, etc.}
   {specific info about the tab format/setup.}
  individual_tab_info_type = record
         tab_name : string;
         num_vars : integer;
         is_seen : boolean;
         varlist : array of integer;   {each integer points to a variable, the number in the
                                        MAR data interface file.}
         num_subtabs : integer;
         sub_tabs : array of record
            subtab_name : string;
            num_vars : integer;
            varlist : array of integer;   {might have sub-tabs with variables on them.}
            end;
      end;
  Multiple_tab_info_type = record
         num_tabs : integer;
         one_tab_info : array of individual_tab_info_type;
      end;

implementation
   var VarFormCheckBoxes : TVarFormCheckBoxes;  {this holds the user variable check boxes.  From this
        we can check for whether they are checked or not.  NOTE - this might need to go in some
        other unit?  Or maybe not, because the info on whether stuff is checked will then be put into
        the user_selections variable...}



   procedure setUserTabsAndVariablesDuringInitialization (user_selections, info_about_user_data_sets);
   {This procedure probably needs to be run just once during the initial program initialization/setup.
    We will have to have another procedure somewhere to mark particular variables as
    enabled/disabled when they make a set of choices like the unit of analysis.}

      var
               Vars_added, vars_added_on_tab, column, row : integer;

      begin
         {Need to check the indexing on this...}

         {First, get number of user data sets.}
         num_user_data_sets := xxx;   {whereever this comes from.  Looks like: }
         num_user_data_sets := configuration.user_data_set_info.get_num_data_sets;
         {this has already happened at the end of eumaind}

         for tabsheet := 0 to num_user_data_sets - 1 do
         begin
            UserVarTabSheet[tabsheet] := TTabsheet.create(self);  {new tabsheet}
            UserVarTabSheet[tabsheet].name := 'UserVarTab'+inttostr(tabsheet);  {This is an internal Delphi name}

            {make the displayed caption/name below the name of the user data set: }
            UserVarTabSheet[tabsheet].caption := configuration.user_data_set_info.get_data_set_short_name (tabsheet);

            UserVarTabSheet[tabsheet].pagecontrol := UserVarTab;  {This refers to the higher level, I think}
         end;

         {for display to be attractive, we either need a panel of the right type to match
          the other variables, or perhaps even more sub-tabs (if too many variables to display
          on the window.  Assume to start that they will all fit, and we'll change that
          when needed.  Actually, we probably will need to change soon b/c polity has many vars.}

         {create what is under the tabs, either a panel or more sub-tabs.
          Change the code here to match the format, color, etc. of the variable panels.}
         for tabsheet := 0 to num_user_data_sets-1 do
            begin
            {Note- need to figure out # of subtabs needed (if any) at/before this point.
             We might also need some structure to save information about each data set
             and if it needs any subtabs.  That's how I did it in MARGene.  It might or might
             not be necessary here, I'm just not sure.}
               if (Need_just_one_tab_for_this_user_set) then
                  begin
                     apanel := TPanel.create(UserVarTabSheet.Pages[tabsheet]);
                     apanel.name := 'VarPanel'+inttostr(tabsheet);
                     apanel.caption := '';
                     apanel.parent := UserVarTabSheet.Pages[tabsheet];
                     apanel.left := 8;
                     apanel.width := apanel.parent.ClientWidth - 16;
                     apanel.top := 8;
                     apanel.height := apanel.parent.ClientHeight - 16;
                     apanel.BevelInner := bvNone;
                     apanel.BevelOuter := bvLowered;
                  end
               else
               {If necessary, create new subtab sheets.}
                     begin
                        asubpagecontrol := TPageControl.create (UserVarTabSheet.Pages[tabsheet]);
                        asubpagecontrol.name := 'SubPageControl'+inttostr(tabsheet);
                        asubpagecontrol.parent := UserVarTabSheet.Pages[tabsheet];
                        {VarTabs.Pages[tabsheet].Controls[0]  is the pagecontrol. verified.}
                        asubpagecontrol.left := 8;
                        asubpagecontrol.width := asubpagecontrol.parent.ClientWidth - 16;
                        asubpagecontrol.top := 8;
                        asubpagecontrol.height := asubpagecontrol.parent.ClientHeight - 16;
                        for subtab := 1 to [Num_subtabs_needed] do  {Note- need to figure out # of subtabs needed before this point}
                           begin
                              asubtabsheet := TTabsheet.create(asubpagecontrol);
                              asubtabsheet.name := 'SubTabSheet'+inttostr(tabsheet)+inttostr(subtab);
                              asubtabsheet.parent := asubpagecontrol;
                              asubtabsheet.pagecontrol := asubpagecontrol;
                              asubtabsheet.caption := inttostr(subtab);

                              apanel := TPanel.create(asubtabsheet);
                              apanel.name := 'VarPanel'+inttostr(tabsheet)+inttostr(subtab);
                              apanel.caption := '';
                              apanel.parent := asubtabsheet;
                              apanel.left := 8;
                              apanel.width := apanel.parent.ClientWidth - 16;
                              apanel.top := 8;
                              apanel.height := apanel.parent.ClientHeight - 16;
                              apanel.BevelInner := bvNone;
                              apanel.BevelOuter := bvLowered;
                           end;
                     end;
            end;

         
            {That should have set up one tab per user data set.  Now can add variables to each tab.}
         {Main tabs are now set;  add variables}
         SelectionTabs.ActivePage := VariablesTab;
         VariableTab.ActivePage := UserVarTabSheet;
         {This code may not be right for what the .activepage should be attached to.  Basically,
          start by making the first page active.  Not even sure if that's necessary.}
         UserVarTabSheet.ActivePage := UserVarTabSheet.Pages[0];

         {Need to get a count of how many total user variables need check boxes.  Then set up
          a structure in memory to hold those checkboxes.  This will allow us to just loop through
          all the check boxes to look for checks, without worrying about what data set they are in.}
         setlength (VarFormCheckBoxes, total_number_of_user_variables);

         vars_added := 0;
         for tabsheet := 0 to num_user_data_sets-1 do
            begin
               {different code for if just 1 tab, or several subtabs for variables}
               if UserVarTabSheet.pages[tabsheet].pagecount = 0 then
                  begin   {just add all to the main tab}
                     Vars_added_on_tab := 0;
                     column := 0;
                     row := 0;
                        {Note - this code for getting # variables from user data set not right.
                         Also check indexing.}
                     for varloop := 0 to user_data_set[tabsheet].num_variables do
                        begin
                           VarFormCheckBoxes[vars_added].original_user_var_num := varloop;
                           VarFormCheckBoxes[vars_added].original_user_data_set_num := tabsheet;
                           {** Note: the "GenerateForm" here was the name of the main application form, you just need to
                            set the owner of the check box to be some form that will go away when user closes
                            application.}
                           VarFormCheckBoxes[vars_added].ACheckBox := TCheckBox.Create(GenerateForm);
                           VarFormCheckBoxes[vars_added].ACheckBox.name := 'VarBox'+inttostr(original_var_num);
                           VarFormCheckBoxes[vars_added].ACheckBox.parent := UserVarTabSheet.Pages[tabsheet];

                           {Make caption on check box the name of the variable:
                           user_data_set_info.get_data_set_var_name (tabsheet, varloop);}
                           VarFormCheckBoxes[vars_added].ACheckBox.Caption := configuration.user_data_set_info.get_data_set_var_name (tabsheet, varloop);

                           VarFormCheckBoxes[vars_added].ACheckBox.width := 70;
                           VarFormCheckBoxes[vars_added].ACheckBox.left := 18 + column*var_column_width_pixels;
                           VarFormCheckBoxes[vars_added].ACheckBox.top := 18+ row*20;
                           VarFormCheckBoxes[vars_added].ACheckBox.BringToFront;
                           inc(row);
                           if VarFormCheckBoxes[vars_added].ACheckBox.Top + VarFormCheckBoxes[vars_added].ACheckBox.Height >
                              VarFormCheckBoxes[vars_added].ACheckBox.Parent.ClientHeight-32 then
                              begin  {Need a new column for next variable.}
                                 inc(column);
                                 row := 0;
                              end;
                           inc(vars_added);
                           inc(vars_added_on_tab);
                        end;        {for varloop}
                  end          {adding to main tab, no subtabs}
               else     {there are subtabs}
                  begin
                           subtab := 0;
                           Vars_added_on_tab := 0;
                           column := 0;
                           row := 0;
                           for varloop := 0 to user_data_set[tabsheet].num_variables do
                              begin
                                 {Do code similar to that above.  But, before attaching the check box to
                                  a sheet, figure out which of the subtabs it needs to be on.  Probably
                                  something like:
                                     add_a_variable;
                                     if too_far_down, increment column.
                                     if too far over, then 1) increment subtab and 2) reset column to 0.

                                  What's below doesn't yet do that, I haven't changed it from MAR code.
                                  }
                                 VarFormCheckBoxes[vars_added].original_user_var_num := varloop;
                                 VarFormCheckBoxes[vars_added].original_user_data_set_num := tabsheet;
                                 VarFormCheckBoxes[vars_added].ACheckBox := TCheckBox.Create(GenerateForm);
                                 VarFormCheckBoxes[vars_added].ACheckBox.name := 'VarBox'+inttostr(original_var_num);

                                 {The only tricky thing is setting the parent of the check boxes under
                                  a subtab, and figuring out the identity of the parent.  The parent needs to be
                                  set to the correct subtab.  Maybe in the
                                  new setup in EUGene this isn't as hard as I thought it was in MARGene.   }
                                 {The subpage control is always control # 0 off the pagecontrol above. }
                                 {PageControl is : VarTabs.Pages[tabsheet].Controls[0]
                                 So, Tab control should be VarTabs.Pages[tabsheet].Controls[0].pages[subtab]}
                                 {Set the parent to the page control, which is NOT right, it puts
                                  the check boxes under the tab.}
                                 AbovePageControl := UserVarTabSheet.Pages[tabsheet].controls[0] as TPageControl;
                                 VarFormCheckBoxes[vars_added].ACheckBox.parent := AbovePageControl.pages[subtab];

                                 VarFormCheckBoxes[vars_added].ACheckBox.Caption := mar_data_info.get_data_set_var_name(0, original_var_num);
                                 VarFormCheckBoxes[vars_added].ACheckBox.hint := mar_data_info.get_data_set_var_hint(0, original_var_num);
                                 VarFormCheckBoxes[vars_added].ACheckBox.showhint := true;
                                 VarFormCheckBoxes[vars_added].ACheckBox.width := 70;
                                 VarFormCheckBoxes[vars_added].ACheckBox.left := 18 + column*var_column_width_pixels;
                                 VarFormCheckBoxes[vars_added].ACheckBox.top := 18+ row*20;
                                 VarFormCheckBoxes[vars_added].ACheckBox.BringToFront;
                                 inc(row);
                                 if VarFormCheckBoxes[vars_added].ACheckBox.Top + VarFormCheckBoxes[vars_added].ACheckBox.Height >
                                    VarFormCheckBoxes[vars_added].ACheckBox.Parent.ClientHeight-32 then
                                    {Alternative code:  something like, if vars_added_on_tab div # per column then...}
                                    begin  {Need a new column for next variable.}
                                       inc(column);
                                       row := 0;
                                    end;
                                 inc(vars_added);
                                 inc(vars_added_on_tab);
                              end;        {for varloop}
                        end;
                  end;
            end;     {for tabsheet}
         if vars_added <> length (VarFormCheckBoxes) then error ('Programming error - number of check boxes added to form unequal to number loaded from documentation.  Notify programmer of error.',0,stop,error_log);
      end;
      end;   {proc  setUserTabsAndVariables}




   procedure set_vars_as_enabled_given_user_selections;
      {When user selects a unit of analysis, that determines which user variables
       should be available for selection.  Go through each check box, look
       at the user variable referenced, and enable or disable each check box.}
      begin
         for varbox := 0 to length (VarFormCheckBoxes)-1 do
            begin
               {do something here to check the selected unit of analysis under user_selections,
                and compare it to the unit of the particular user variable.  We have info on what
                user variable this is through
                   VarFormCheckBoxes[varbox].original_user_dataset_num     and
                   VarFormCheckBoxes[varbox].original_user_var_num
                }

                if variables_should_be_enabled then
                   VarFormCheckBoxes[varbox].ACheckBox.enabled := true
                   else
                   VarFormCheckBoxes[varbox].ACheckBox.enabled := false;

            end;
      end;




   procedure set_user_vars_when_click_OK_Button;
      begin
         for varbox := 0 to length (VarFormCheckBoxes)-1 do
            begin
               {check both to see if it is an enabled variable choice - so it's appropriate given aggregation
                or other options that might affect this - and selected/checked by the user.}

               {Note - this points out that earlier, we need code somewhere to enable/disable the
                variable check boxes based on user selections for unit of analysis.}

               {first, blank out list of any previously selected user variables.}
               for x := 0 to num_user_data_sets do
               setlength(user_selections.user_data_sets_selections[x].data_set_variables, 0);

               if ((VarFormCheckBoxes[varbox].ACheckBox.enabled) and (VarFormCheckBoxes[varbox].ACheckBox.checked)) then
                  {add this var}
                  begin
                     {I haven't checked the correct code to add the variable to the user
                      data set listing - find the code from under the OK button of the user variable
                      popup.  The first 2 lines below are NOT RIGHT for EUGene.}
                     setlength(user_selections.MAR_variable_selections.data_set_variables, length(user_selections.MAR_variable_selections.data_set_variables)+1);
                     user_selections.MAR_variable_selections.data_set_variables[length(user_selections.MAR_variable_selections.data_set_variables)-1] :=
                        VarFormCheckBoxes[varbox].original_var_num;

                     {It will be something like: }
                     setlength(user_selections.user_data_sets_selections[x].data_set_variables, length(user_selections.user_data_sets_selections[x].data_set_variables)+1);
                     spot_to_add :=length(user_selections.user_data_sets_selections[x].data_set_variables) - 1;
                     user_selections.user_data_sets_selections[VarFormCheckBoxes[varbox].original_user_dataset_num].data_set_variables[spot_to_add] :=
                         VarFormCheckBoxes[varbox].original_user_var_num;
                  end;
            end;

      end;

end.
