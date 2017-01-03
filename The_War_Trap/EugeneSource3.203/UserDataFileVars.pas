unit UserDataFileVars;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, EUTypes1, StdCtrls, ExtCtrls, Buttons, math;

const
  badvarsuffix = ' (N/A)';
type
  TUserDataSelectionForm = class(TForm)
    MainTabControl: TTabControl;
    userDataOKButton: TBitBtn;
    UserDataCancelButton: TBitBtn;
    UserDataHelpButton: TBitBtn;
    UserPanel: TPanel;
    FileNameLocation: TStaticText;
    ShortDataSetName: TStaticText;
    FullDataSetName: TStaticText;
    SrcList: TListBox;
    DstList: TListBox;
    IncludeBtn: TSpeedButton;
    IncAllBtn: TSpeedButton;
    ExcludeBtn: TSpeedButton;
    ExAllBtn: TSpeedButton;
    FromBoxLabel: TLabel;
    VariablesSelectedLabel: TLabel;
    UpdateUserFilesBtn: TButton;
    YearsAvailable: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure MainTabControlChange(Sender: TObject);
    procedure IncludeBtnClick(Sender: TObject);
    procedure IncAllBtnClick(Sender: TObject);
    procedure ExcludeBtnClick(Sender: TObject);
    procedure ExAllBtnClick(Sender: TObject);
    procedure SetButtons(Sender: TObject);
    function GetFirstSelection(List: TCustomListBox): Integer;
    procedure MoveSelected(List: TCustomListBox; DestItems: TStrings);
    procedure SetItem(List: TListBox; Index: Integer);
    procedure FormShow(Sender: TObject);
    procedure SaveTabVariableInfo(Sender: TObject; var AllowChange: Boolean);
    procedure userDataOKButtonClick(Sender: TObject);
    procedure SortList(Items : Tstrings);
    procedure UpdateUserFilesBtnClick(Sender: TObject);
    procedure tabset(Sender: TObject);
    procedure UserDataHelpButtonClick(Sender: TObject);
  private
    BadList : TStringlist;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Saved_Var_Info : user_data_sets_selection_type;

implementation
   uses euinoutd;

{$R *.DFM}

{ ----------------------------------- }

function original_name (varname : string) : string;
   begin
      if length(varname)<= length(badvarsuffix) then
         original_name := varname
      else   {maybe bad}
         if ansipos(badvarsuffix, varname) <> 0 then
         original_name := copy(varname,0,length(varname)-length(badvarsuffix))
           {did do check for substring with function:  copy (varname,length(varname)-length(badvarsuffix),length(badvarsuffix)) = badvarsuffix then {has bad suffix at end}
      else original_name := varname;
   end;

procedure TUserDataSelectionForm.SetButtons;
var
  SrcEmpty, DstEmpty: Boolean;
begin
  SrcEmpty := SrcList.Items.Count = 0;
  DstEmpty := DstList.Items.Count = 0;
  IncludeBtn.Enabled := not SrcEmpty;
  IncAllBtn.Enabled := not SrcEmpty;
  ExcludeBtn.Enabled := not DstEmpty;
  ExAllBtn.Enabled := not DstEmpty;
end;

function TUserDataSelectionForm.GetFirstSelection(List: TCustomListBox): Integer;
begin
  for Result := 0 to List.Items.Count - 1 do
    if List.Selected[Result] then Exit;
  Result := LB_ERR;
end;

procedure TUserDataSelectionForm.MoveSelected(List: TCustomListBox; DestItems: TStrings);
{Moves all selected items}
var
  I, reverse_index: Integer;
begin
  for I := List.Items.Count - 1 downto 0 do
    if List.Selected[I] then
       begin
        {Also Need to check and make sure variable is not on the badlist.
        But, srclist.items has name + '(n/a)', while BadList has name w/o n/a.
        Need to modify name to search on bad list, without ' (N/A)'.}
        if BadList.indexof(original_name(List.items[I])) = -1 then
          begin
            DestItems.AddObject(List.Items[I], List.Items.Objects[I]);
            List.Items.Delete(I);
          end;
       end;

        {if they select a monadic variable in a dyadic data set, also make
         sure they have selected the complementary variable for the other state.}
        {And, if user wants a dir or non-dir data set, and
         the var is from a non-dir data set with reverse var, then force them to
         output the complementary var also.}
        {Do this by going through items (the destination), and looking for reverse
         of the item on the list side.}
        {Only want this when output is nondirected, too, though, or nondir input
         and dir output.}
       {monadic VAR or (nondir input DATASET).  Had tried to set this up so that
        a nondir input dataset must have nondir output data also, but it was
        tricky programming and requiring the user to recognize the foibles of
        a nondir dataset is OK, too.  So, this will set 2 vars in destination for
        all directed vars from nondirected user dataset. }

  for I := DestItems.Count - 1 downto 0 do
     if (configuration.User_data_set_info.get_data_set_var_unit(MainTabControl.tabindex,configuration.User_data_set_info.get_data_set_var_number(MainTabControl.tabindex,DestItems[I])) = monadic) or
        ((configuration.User_data_set_info.get_data_set_unit(MainTabControl.tabindex) = nondirected_dyad_year)
        ) then
     begin
        reverse_index := List.items.indexof(configuration.User_data_set_info.get_data_set_var_reversed_var(MainTabControl.tabindex,configuration.User_data_set_info.get_data_set_var_number(MainTabControl.tabindex,DestItems[I])));
        if reverse_index <> -1 then
        begin
            DestItems.AddObject(List.Items[reverse_index], List.Items.Objects[reverse_index]);
            List.Items.Delete(reverse_index);
        end;
     end;

end;

procedure TUserDataSelectionForm.SetItem(List: TListBox; Index: Integer);
   {make an entry in the string list selected, if in range.}
var
  MaxIndex: Integer;
begin
  with List do
  begin
    SetFocus;
    MaxIndex := List.Items.Count - 1;
    if Index = LB_ERR then Index := 0
       else if Index > MaxIndex then Index := MaxIndex;
    if List.Items.Count > 0 then        {If I can select an item, then do.}
       Selected[Index] := True;
  end;
  SetButtons(self);
end;

{ ----------------------------------- }

procedure TUserDataSelectionForm.SortList(Items : Tstrings);
   var x, y : integer;
       tempitem : string;
   begin
      if Items.count > 1 then
      for x := 0 to Items.count - 2 do
         for y := x + 1 to Items.count-1 do
            begin        {compare and switch these items}
               if configuration.User_data_set_info.get_data_set_var_number(MainTabControl.tabindex,original_name(Items[x])) >
                  configuration.User_data_set_info.get_data_set_var_number(MainTabControl.tabindex,original_name(Items[y])) then
                  begin
                     tempitem := Items[x];
                     Items[x] := Items[y];
                     Items[y] := tempitem;
                  end;
               end;
   end;

procedure TUserDataSelectionForm.IncludeBtnClick(Sender: TObject);
var
  Index: Integer;
  VarNameToSearchFor : string;
begin
  Index := GetFirstSelection(SrcList);
  if Index <> LB_Err then
     begin
        MoveSelected(SrcList, DstList.Items);
        SetItem (SrcList, Index);
        SortList (DstList.Items);
     end;
end;

procedure TUserDataSelectionForm.IncAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := SrcList.Items.Count - 1 downto 0 do
    begin
       SrcList.Selected[I] := True;
       if BadList.indexof(original_name(SrcList.items[I]) ) = -1 then
          begin
             DstList.Items.AddObject(SrcList.Items[I], SrcList.Items.Objects[I]);
             SrcList.Items.delete(I);
          end;
    end;
  {SrcList.Items.Clear;}
  SetItem(SrcList, 0);
  SortList (DstList.items);
end;

procedure TUserDataSelectionForm.ExcludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(DstList);
  if Index <> LB_Err then
     begin
        MoveSelected(DstList, SrcList.Items);
        SetItem(DstList, Index);
     end;
  SortList (SrcList.items);
end;

procedure TUserDataSelectionForm.ExAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to DstList.Items.Count - 1 do
    SrcList.Items.AddObject(DstList.Items[I], DstList.Items.Objects[I]);
  DstList.Items.Clear;
  SetItem(DstList, 0);
  SortList (SrcList.items);
end;

{ ----------------------------------- }

Procedure TUserDataSelectionForm.tabset(Sender: TObject);
   var x : integer;
   begin
      MainTabControl.tabs.clear;
      if configuration.User_data_set_info.get_num_data_sets > 0 then
         begin
            {create one tab per user data set.  Note that tab index # will match
             the configuration.user_data_set_info number.}
            for x := 0 to (configuration.User_data_set_info.get_num_data_sets-1) do
               MainTabControl.tabs.add(configuration.User_data_set_info.get_data_set_short_name(x));
            {Note that setting information on the tab must go in the on change event
             in the tab event handler.  It can't be done here.  }
            userDataOKButton.enabled := true;
         end
      else
         begin
            MainTabControl.tabs.add('No User Data Sets Available');
            userDataOKButton.enabled := false;
         end;
   end;

procedure TUserDataSelectionForm.FormCreate(Sender: TObject);
begin
   BadList := TstringList.create;
   BadList.clear;
   tabset(sender);
   self.height := 5 + MainTabControl.height + 30 + userDataOkButton.height + 30;
   self.width := 10 + MainTabControl.width + 20;
   {Initialize saved variable info record}
   setlength(Saved_Var_Info, configuration.User_data_set_info.get_num_data_sets);
end;

{ ----------------------------------- }

procedure TUserDataSelectionForm.FormShow(Sender: TObject);
begin
   {Now When form is shown, send change message to be sure display is updated.}
   self.MainTabControlChange(sender);
end;

{ ----------------------------------- }

procedure TUserDataSelectionForm.UpdateUserFilesBtnClick(Sender: TObject);
   var adataset : integer;
       files_changed : integer;
       change_allowed : boolean;
begin
   {first save variables from this tab.}
   SaveTabVariableInfo (sender, change_allowed);
   {If data sets on disk changed, reset tabs.}
   {If new added, then if add at end, all saved Info OK.}
   {If deleted, must clear saved info.}
   files_changed := configuration.User_data_set_info.update_changes(configuration.user_data_files_directory);
   if files_changed  = 0 then
      begin end     {no changes, do nothing}
   else if files_changed  = 1 then
      {files added, need to update tabs}
      begin
         tabset(sender);
         self.MainTabControlChange(sender);
         setlength(Saved_Var_Info, configuration.User_data_set_info.get_num_data_sets);
      end
   else if files_changed  = 2 then
      {data files on disk changed in a way that saved var info must be cleared, and tabs set.}
      begin
         for adataset := 0 to MainTabControl.tabs.count-1 do
            begin
               setlength (Saved_Var_Info[adataset].data_set_variables, 0);
            end;   {for adataset}
         tabset(sender);
         self.MainTabControlChange(sender);
         setlength(Saved_Var_Info, configuration.User_data_set_info.get_num_data_sets);
      end
   else
      MessageDlg ('Programming error - result of update files file change not recognized value.  Notify programmer.', mterror, [mbok], 0);
end;

{ ----------------------------------- }

procedure TUserDataSelectionForm.MainTabControlChange(Sender: TObject);
   {This is called right after the new tab is entered.  It should set names, and
    reset selected variables to what user previously specified.}
   var index, x, y : integer;
       listspot : integer;
       varname : string;
begin
   {Note:  can't move this to create procedure, must be in local on change event.  }
   {if there are any data sets, then set caption for this tab.}
   if configuration.User_data_set_info.get_num_data_sets > 0 then
      begin
         FullDataSetName.caption := 'Data Set: '+ configuration.User_data_set_info.get_data_set_full_name(MainTabControl.tabindex);
         ShortDataSetName.caption := 'Short Reference Name: '+ configuration.User_data_set_info.get_data_set_short_name(MainTabControl.tabindex);
         FileNameLocation.caption := 'Data File Name: '+ ExtractFileName(configuration.User_data_set_info.get_data_set_file_name(MainTabControl.tabindex));
         YearsAvailable.caption := 'Years Available: '+inttostr(configuration.User_data_set_info.get_data_set_first_year_possible(MainTabControl.tabindex))
                                      +' to '+inttostr(configuration.User_data_set_info.get_data_set_last_year_possible(MainTabControl.tabindex));
         srcList.items.clear;
         dstList.items.clear;
         BadList.clear;

         { set user selections here.}
         {Add all variables to source list, and note bad if a problem.}
         for index := 0 to configuration.User_data_set_info.get_data_set_num_vars(MainTabControl.tabindex)-1 do
            if ( (configuration.User_data_set_info.get_data_set_var_unit(MainTabControl.tabindex, index) <> identifierccode1) and
                 (configuration.User_data_set_info.get_data_set_var_unit(MainTabControl.tabindex, index) <> identifierccode2) and
                 (configuration.User_data_set_info.get_data_set_var_unit(MainTabControl.tabindex, index) <> identifieryear) ) then
               begin
                  {note if this is a problematic variable given user selections}
                  if not(can_get_var_value_given_output_unit_and_input_data (MainTabControl.tabindex, index, user_selections)) then
                     begin
                        {Put original name on badList.  Then change the name for visible list.}
                        BadList.add(configuration.User_data_set_info.get_data_set_var_name(MainTabControl.tabindex,index));
                        varname := configuration.User_data_set_info.get_data_set_var_name(MainTabControl.tabindex,index)+badvarsuffix;
                     end
                     else varname := configuration.User_data_set_info.get_data_set_var_name(MainTabControl.tabindex,index);
                  {add item to source or destination list.  Put on dst list if already marked.}
                  SrcList.Items.Add (varname);
               end;
         {Now find items on srcList and move to dstlist if necessary}
         {Loop through each variable on user list;  if available and not a bad variable
          because of unit of analysis, move it.  }
         for x := length(Saved_Var_Info[MainTabControl.tabindex].data_set_variables)-1 downto 0 do
            begin
               {Find variable on source list, move to dst list}
               listspot := SrcList.items.indexof(configuration.User_data_set_info.get_data_set_var_name(MainTabControl.tabindex, Saved_Var_Info[MainTabControl.tabindex].data_set_variables[x]));
               if listspot <> -1 then    {var is on source list, always should be true}
                  begin
                      {Need to check to see if have to remove this from user list b/c bad!
                       This will be updated in the save variable info procedure, because
                       if I don't put the bad variable on the destination list here, and user
                       cannot choose it, it will not get on the list that is eventually checked
                       to create user var list!}

                     if BadList.indexof(configuration.User_data_set_info.get_data_set_var_name(MainTabControl.tabindex, Saved_Var_Info[MainTabControl.tabindex].data_set_variables[x])) = -1
                     then   {This should be most vars!}
                        begin
                           DstList.Items.AddObject(SrcList.Items[listspot], SrcList.Items.Objects[listspot]);
                           SrcList.Items.delete(listspot);
                        end
                  end;

            end;       {for x}

         SortList (SrcList.items);
         SortList (DstList.items);
         SetButtons(self);

      end;   {if there are data sets}
end;

{ ----------------------------------- }

procedure TUserDataSelectionForm.SaveTabVariableInfo(Sender: TObject; var AllowChange: Boolean);
   {This is called by the "OnChanging" event, which occurs before a tab is exited.
    This proc saves data from a tab in a variable for later retrieval.  It can do
    this repeatedly, for multiple tabs, and keep the most recent version of each.}
   var x : integer;
begin
   {before switching to the next tab, need to save info about these vars so it can be
    reconstructed if user comes back to this tab, or accessed for the program running.}
   setLength(Saved_Var_Info[MainTabControl.tabindex].data_set_variables, dstList.Items.count);
   for x := 0 to dstList.Items.count-1 do
      Saved_Var_Info[MainTabControl.tabindex].data_set_variables[x] :=
         configuration.User_data_set_info.get_data_set_var_number(MainTabControl.tabindex, dstList.Items[x]);
   AllowChange := true;
end;

{ ----------------------------------- }

procedure TUserDataSelectionForm.userDataOKButtonClick(Sender: TObject);
var auser_data_set : user_data_sets_selection_type;
    x, adataset, curr_var : integer;
    num_data_sets : integer;
    change_allowed : boolean;
begin
   {First, be sure the top tab is updated.  Unclear from Delphi help if
    the onchanging event will operate when this button is pushed, because
    this is a button, not a tab change, and it is the procedure called by
    onchanging that updates the saved variables.}
   SaveTabVariableInfo (sender, change_allowed);

   {Note that the size of the overall user array is set previously, in eumaind,
    to the number of user data sets there are.}

   {Now process my saved data structure.  Go through each tab, if there are vars
    specified, then add them to the user selections list.}
   for adataset := 0 to MainTabControl.tabs.count-1 do
      begin
         setlength (user_selections.user_data_sets_selections[adataset].data_set_variables, length(Saved_Var_Info[adataset].data_set_variables));
         if length(Saved_Var_Info[adataset].data_set_variables) > 0 then
            begin
               for x := 0 to length(Saved_Var_Info[adataset].data_set_variables)-1 do
                  user_selections.user_data_sets_selections[adataset].data_set_variables[x] := Saved_Var_Info[adataset].data_set_variables[x];
            end;
      end;   {for adataset}

end;

procedure TUserDataSelectionForm.UserDataHelpButtonClick(Sender: TObject);
begin
     ShowMessage ('Use this interface to select user dataset variables to include in or exclude from your data.  More information about selecting and updating data may be found in the Eugene documentation.')
end;

end.
