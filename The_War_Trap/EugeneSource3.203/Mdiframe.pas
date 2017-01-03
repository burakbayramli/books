unit mdiframe;

{EUGene  Copyright 1997-2007+  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

uses
  SysUtils, WinTypes, WinProcs, Classes, Graphics,
  Controls, Forms, Dialogs, Menus, StdCtrls, ExtCtrls, Buttons,
  CmnProcD, EUinoutD, EUtypes1, EUtypes2, EUTestD, GABox,
  RiskAndSecurityConversionProcs, riskgenbox, riskcombineintegrated,
  Cntry, Setting, OutWindow, PagedOutput, Abouteu, MenuHelp, YrBox;

type
  TEUGeneMainFrame = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileMenuExit: TMenuItem;
    OutputMenu: TMenuItem;
    CountryYearDataOutputMenuChoice: TMenuItem;
    DirectedDyadYearDataOutputMenuChoice: TMenuItem;
    MainPrintDialog: TPrintDialog;
    HelpMenu: TMenuItem;
    AboutEugene1: TMenuItem;
    Contents1: TMenuItem;
    RecomputeMenu: TMenuItem;
    ExpectedUtilityWarandReasonMenuChoice: TMenuItem;
    RiskAttitudeWarTrapRevisitedMenuChoice: TMenuItem;
    ExpectedUtilityWarTrapMenuChoice: TMenuItem;
    TaubScoresUtilityMenuChoice: TMenuItem;
    PercentSystemCapabilitiesMenuChoice: TMenuItem;
    SaveDialog1: TSaveDialog;
    DisputeDyadDataOutputMenuChoice: TMenuItem;
    TraceMenu: TMenuItem;
    TraceOff: TMenuItem;
    TraceOn: TMenuItem;
    TestMenuItem: TMenuItem;
    SMenuChoice: TMenuItem;
    OpenDialog1: TOpenDialog;
    RiskDataConvert1: TMenuItem;
    RiskDataShow1: TMenuItem;
    SecAllianceFileCreate1: TMenuItem;
    securitydatashow1: TMenuItem;
    RiskSecurityProcs1: TMenuItem;
    RiskAllYears1: TMenuItem;
    RiskSubsetofYears1: TMenuItem;
    RiskAppendNewYears1: TMenuItem;
    RiskSingleDisplayProcedure: TMenuItem;
    NonDirectedDyadYearOutputMenuChoice: TMenuItem;
    CombineRiskFilesMenuItem: TMenuItem;
    showsecuritytaumenu: TMenuItem;
    ShowSecuritySMenu: TMenuItem;
    UserDataMenu: TMenuItem;
    CreateDataConfig: TMenuItem;
    TransferUserDataSets1: TMenuItem;
    CustomProceduresMenu: TMenuItem;
    RunRandysProcedure: TMenuItem;
    LoadSettingsMenu: TMenuItem;
    SaveSettingsMenu: TMenuItem;
    N1: TMenuItem;
    ViewDocumentation1: TMenuItem;
    Website1: TMenuItem;
    N2: TMenuItem;
    UserDataSetProceduresMenuItem: TMenuItem;
    SplitMIDsOvertimeProcedure: TMenuItem;
    DyadicMIDData30MenuItem: TMenuItem;
    N3: TMenuItem;
    ShowOutputWindow1: TMenuItem;
    HypotheticalAllianceData1: TMenuItem;
    MainMenu2: TMainMenu;
    N4: TMenuItem;
    N5: TMenuItem;
    DatasetBrowserMenu: TMenuItem;
    NonDirectedDisputeDyadData: TMenuItem;
    NonDirDisputeOnsetsMenuChoice: TMenuItem;
    NonDirDisputeYearMenuChoice: TMenuItem;
    OutputEUProjectedJoiningInfo: TMenuItem;
    DirDisputeInitiationMenuChoice: TMenuItem;
    DirDisputeYearMenuChoice: TMenuItem;
    procedure EnableMenus;
    procedure DisableMenus;
    procedure PercentSystemCapabilitiesMenuChoiceClick(Sender: TObject);
    procedure SearchTopic1Click(Sender: TObject);
    procedure AboutEugene1Click(Sender: TObject);
    procedure FileMenuExitClick(Sender: TObject);
    procedure CountryYearDataOutputMenuChoiceClick(Sender: TObject);
    procedure DirectedDyadYearDataOutputMenuChoiceClick(Sender: TObject);
    procedure TaubScoresUtilityMenuChoiceClick(Sender: TObject);
    procedure ExpectedUtilityWarTrapMenuChoiceClick(Sender: TObject);
    procedure ExpectedUtilityWarandReasonMenuChoiceClick(Sender: TObject);
    procedure Contents1Click(Sender: TObject);
    procedure DisputeDyadDataOutputMenuChoiceClick(Sender: TObject);
    procedure TraceOnClick(Sender: TObject);
    procedure TraceOffClick(Sender: TObject);
    procedure TestMenuClick(Sender: TObject);
    procedure SMenuChoiceClick(Sender: TObject);
    procedure RiskDataConvert1Click(Sender: TObject);
    procedure RiskDataShow1Click(Sender: TObject);
    procedure SecAllianceFileCreate1Click(Sender: TObject);
    procedure RiskAllYears1Click(Sender: TObject);
    procedure RiskSubsetofYears1Click(Sender: TObject);
    procedure RiskAppendNewYears1Click(Sender: TObject);
    procedure RiskSingleDisplayProcedureClick(Sender: TObject);
    procedure NonDirectedDyadYearOutputMenuChoiceClick(Sender: TObject);
    procedure CombineRiskFilesMenuItemClick(Sender: TObject);
    procedure showsecuritytaumenuClick(Sender: TObject);
    procedure ShowSecuritySMenuClick(Sender: TObject);
    procedure TransferUserDataSets1Click(Sender: TObject);
    procedure CreateDataConfigClick(Sender: TObject);
    procedure RunRandysProcedureClick(Sender: TObject);
    procedure LoadSettingsMenuClick(Sender: TObject);
    procedure SaveSettingsMenuClick(Sender: TObject);
    procedure ViewDocumentation1Click(Sender: TObject);
    procedure Website1Click(Sender: TObject);
    procedure UserDataSetProceduresMenuItemClick(Sender: TObject);
    procedure SplitMIDsOvertimeProcedureClick(Sender: TObject);
    procedure DyadicMIDData30MenuItemClick(Sender: TObject);
    procedure ShowOutputWindow1Click(Sender: TObject);
    procedure HypotheticalAllianceData1Click(Sender: TObject);
    procedure DatasetBrowserMenuClick(Sender: TObject);
    procedure NonDirDisputeOnsetsMenuChoiceClick(Sender: TObject);
    procedure NonDirDisputeYearMenuChoiceClick(Sender: TObject);
    procedure OutputEUProjectedJoiningInfoClick(Sender: TObject);
    procedure DirDisputeInitiationMenuChoiceClick(
      Sender: TObject);
    procedure DirDisputeYearMenuChoiceClick(Sender: TObject);
  private
    { Private declarations }
    procedure output_sub_call (var ccode_index : Tccode_index_obj; var nation_list : Tnation_array_obj;
                      var contiguity_data : Tcontiguity_array_obj; var configuration : configuration_type;
                      var user_selections : user_selection_type; selected_output_unit : output_type);
  public
    var Output_Options: TOutput_Options;
  end;

var EUGeneMainFrame: TEUGeneMainFrame;

implementation

uses Calcbox, euprocs1, EUProcs2, TraceUnit, RiskOverwriteForm,
  FTPConnect, UserDataPreparationForm, ShellApi, Randy, MidOverTime, EUTypesMID, dyadgen,
  allianceoutput, browser, errbx;

{$R *.DFM}

{---------------------------------------------------------------}

procedure TEUGeneMainFrame.SearchTopic1Click(Sender: TObject);
begin
     ShowMessage('Help Topic Info Here');
end;

{-----------------------------------------------------------------}

procedure TEUGeneMainFrame.AboutEugene1Click(Sender: TObject);
var AboutBox : TAboutBox;
begin
   try
     AboutBox := TAboutBox.create(application);
     AboutBox.hide;
     AboutBox.showmodal;
   finally
      AboutBox.free;
   end;
end;

{-------------------------------------------------------------}

procedure TEUGeneMainFrame.FileMenuExitClick(Sender: TObject);
    begin
         Close;
    end;

{-----------------------------------------------------------------------------------------------}

procedure TEUGeneMainFrame.output_sub_call (var ccode_index : Tccode_index_obj; var nation_list : Tnation_array_obj;
                      var contiguity_data : Tcontiguity_array_obj; var configuration : configuration_type;
                      var user_selections : user_selection_type; selected_output_unit : output_type);
   var option_result : integer;
   begin
      try
         user_selections.output_this := selected_output_unit;
         option_result := Output_Options.showmodal;
         {disable the buttons that would allow user to start procedures that might
          interfere with running procedures, then call output procedure.  }
         if option_result = mrOK then
            begin
               try
                  self.DisableMenus;
                  output_data (ccode_index, nation_list, contiguity_data, configuration, user_selections, selected_output_unit)
               finally
                  Self.EnableMenus;
                  trace.hideprogress;  {need to hide this at the end of each procedure}
               end;   {try-finally}
            end;
      except
         on EInOutError do
            begin
            {do nothing but clear the exception, ready for user to try again}
            end;
         on EUserInterrupt do
            begin
               {do nothing but clear the exception, ready for user to try again}
               UserInterrupt := false;
            end;
      end;     {try-except}
   end;

{-----------------------------------------------------------------------}

procedure TEUGeneMainFrame.CountryYearDataOutputMenuChoiceClick(Sender: TObject);
var option_result : integer;
begin
   try
      output_sub_call (ccode_index, nation_list, contiguity_data, configuration, user_selections,
                       output_monads);
   except
      on EInOutError do
         begin
         {do nothing but clear the exception, ready for user to try again}
         end;
      on EUserInterrupt do
         begin
            {do nothing but clear the exception, ready for user to try again}
            UserInterrupt := false;
         end;
   end;     {try-except}
end;

{-----------------------------------------------------------------------}

procedure TEUGeneMainFrame.DirectedDyadYearDataOutputMenuChoiceClick(Sender: TObject);
var option_result : integer;
begin
   try
      output_sub_call (ccode_index, nation_list, contiguity_data, configuration, user_selections,
                       output_directed_dyads);
   except
      on EInOutError do
         begin
         {do nothing but clear the exception, ready for user to try again}
         end;
      on EUserInterrupt do
         begin
            {do nothing but clear the exception, ready for user to try again}
            UserInterrupt := false;
         end;
   end;     {try-except}
end;

{-------------------------------------------------------------------------}

procedure TEUGeneMainFrame.NonDirectedDyadYearOutputMenuChoiceClick(Sender: TObject);
var option_result : integer;
begin
   try
      output_sub_call (ccode_index, nation_list, contiguity_data, configuration, user_selections,
                       output_nondirected_dyads);
   except
      on EInOutError do
         begin
         {do nothing but clear the exception, ready for user to try again}
         end;
      on EUserInterrupt do
         begin
            {do nothing but clear the exception, ready for user to try again}
            UserInterrupt := false;
         end;
   end;     {try-except}
end;

{-------------------------------------------------------------------------}

procedure TEUGeneMainFrame.DisputeDyadDataOutputMenuChoiceClick(Sender: TObject);
var option_result : integer;
begin
   try
      output_sub_call (ccode_index, nation_list, contiguity_data, configuration, user_selections,
                       output_directed_dispute_initiation_dyads);
   except
      on EInOutError do
         begin
         {do nothing but clear the exception, ready for user to try again}
         end;
      on EUserInterrupt do
         begin
            {do nothing but clear the exception, ready for user to try again}
            UserInterrupt := false;
         end;
   end;     {try-except}
end;

{-------------------------------------------------------------------------}

procedure TEUGeneMainFrame.PercentSystemCapabilitiesMenuChoiceClick(Sender: TObject);
var option_result : integer;
begin
   try
      user_selections.compute_this := compute_syscap;
      Calculation_box.showmodal;
      option_result := Calculation_box.modalresult;
      {disable the buttons that would allow user to start procedures that might
       interfere with running procedures, then call procedure.  }
      if option_result = mrOK then
         begin
            try
               self.DisableMenus;
                         {for sys cap, start/end determined by first_cap_year, last_...}
               {Make sure it's ok to overwrite the file, if there is one}
               if FileExists (configuration.cow_system_pct_file_name) then
                  begin
                     if MessageDlg(('There is an existing national capabilities data file.  Overwrite?  '+
                                '(Yes overwrites, No cancels recalculate procedure)'), mtWarning,
                                [mbYes, mbNo], 0) = mrYes then
                        compute_and_save_sys_capability (configuration.cow_raw_cap_file_name,
                               configuration.cow_modified_cap_file_name,
                               configuration.cow_system_pct_file_name, nation_list, ccode_index,
                               configuration.first_cap_year, configuration.last_cap_year);
                  end
                  else
                        compute_and_save_sys_capability (configuration.cow_raw_cap_file_name,
                               configuration.cow_modified_cap_file_name,
                               configuration.cow_system_pct_file_name, nation_list, ccode_index,
                               configuration.first_cap_year, configuration.last_cap_year);

            finally
               self.EnableMenus;
               trace.hideprogress;  {need to hide this at the end of each procedure}
            end;
         end;
   except
      on EInOutError do
         begin
         {do nothing but clear the exception, ready for user to try again}
         end;
      on EUserInterrupt do
         begin
            {do nothing but clear the exception, ready for user to try again}
            UserInterrupt := false;
         end;
   end;  {except}
end;

{-----------------------------------------------------------------------}

procedure TEUGeneMainFrame.TaubScoresUtilityMenuChoiceClick(Sender: TObject);
var option_result : integer;
begin
   try
      user_selections.compute_this := compute_tau;
      Calculation_box.showmodal;
      option_result := Calculation_box.modalresult;
      {disable the buttons that would allow user to start procedures that might
       interfere with running procedures, then call procedure.  }
      if option_result = mrOK then
         begin
            try
               self.DisableMenus;
                         {for taus, start/end year determined by first_alliance_year, last_...}
               {Make sure it's ok to overwrite the file, if there is one}
               if FileExists (configuration.tau_file_name) then
                  begin
                     if MessageDlg(('There is an existing tau data file.  Overwrite?  '+
                                '(Yes overwrites, No cancels recalculate procedure)'), mtWarning,
                                [mbYes, mbNo], 0) = mrYes then
                        compute_and_save_taus (configuration.cow_alliance_file_name,
                           configuration.alliance_seq_file_name, configuration.dyadic_alliance_file_name,
                           configuration.tau_file_name, nation_list, configuration.first_alliance_year,
                           configuration.last_alliance_year, user_selections.alliance_data_source);
                  end
               else
                        compute_and_save_taus (configuration.cow_alliance_file_name,
                           configuration.alliance_seq_file_name, configuration.dyadic_alliance_file_name,
                           configuration.tau_file_name, nation_list, configuration.first_alliance_year,
                           configuration.last_alliance_year, user_selections.alliance_data_source);

            finally
               self.EnableMenus;
               trace.hideprogress;  {need to hide this at the end of each procedure}
            end;
         end;
   except
      on EInOutError do
         begin
         {do nothing but clear the exception, ready for user to try again}
         end;
      on EUserInterrupt do
         begin
            {do nothing but clear the exception, ready for user to try again}
            UserInterrupt := false;
         end;
   end;
end;

{---------------------------------------------------------------------}

procedure TEUGeneMainFrame.SMenuChoiceClick(Sender: TObject);
var option_result : integer;
begin
   try
      user_selections.compute_this := compute_S;
      Calculation_box.showmodal;
      option_result := Calculation_box.modalresult;
      {disable the buttons that would allow user to start procedures that might
       interfere with running procedures, then call procedure.  }
      if option_result = mrOK then
         begin
            try
               self.DisableMenus;
               {Make sure it's ok to overwrite the file, if there is one}
               if FileExists (configuration.s_file_name) then
                  begin
                     if MessageDlg(('There is an existing s data file.  Overwrite?  '+
                                '(Yes overwrites, No cancels recalculate procedure)'), mtWarning,
                                [mbYes, mbNo], 0) = mrYes then
                        compute_and_save_s (configuration.cow_alliance_file_name,
                           configuration.alliance_seq_file_name, configuration.dyadic_alliance_file_name,
                           configuration.s_file_name, configuration.cow_system_pct_file_name,
                           nation_list, configuration.first_alliance_year,
                           configuration.last_alliance_year, user_selections.alliance_data_source);
                  end
               else
                        compute_and_save_s (configuration.cow_alliance_file_name,
                           configuration.alliance_seq_file_name, configuration.dyadic_alliance_file_name,
                           configuration.s_file_name, configuration.cow_system_pct_file_name,
                           nation_list, configuration.first_alliance_year,
                           configuration.last_alliance_year, user_selections.alliance_data_source);

            finally
               self.EnableMenus;
               trace.hideprogress;
            end;
         end;
   except
      on EInOutError do
         begin
         {do nothing but clear the exception, ready for user to try again}
         end;
      on EUserInterrupt do
         begin
            {do nothing but clear the exception, ready for user to try again}
            UserInterrupt := false;
         end;
   end;
end;

{---------------------------------------------------------------------}

procedure TEUGeneMainFrame.ExpectedUtilityWarTrapMenuChoiceClick(Sender: TObject);
var option_result : integer;
    temp_name : TFileName;
begin
   try
      user_selections.compute_this := compute_EUWarTrap;
      Calculation_box.showmodal;
      option_result := Calculation_box.modalresult;
      {disable the buttons that would allow user to start procedures that might
       interfere with running procedures, then call procedure.  }
      if option_result = mrOK then
         begin
            try
               self.DisableMenus;
               {Make sure it's ok to overwrite the file, if there is one}
               case user_selections.similarity_method of
                 use_tau : temp_name := configuration.EUWarTrap_tau_file_name;
                 use_s : temp_name := configuration.EUWarTrap_s_unweighted_file_name;
                 else EUGeneError ('Came into compute_and_save_EUWarTrap file check procedure without s/tau option set.  NOtify programmer.  Fatal error.  ',2,stop,error_log);
               end;   {case}

               if FileExists (temp_name) then
                  begin
                     if MessageDlg(('There is an existing EU War Trap data file.  Overwrite?  '+
                                '(Yes overwrites, No cancels recalculate procedure)'), mtWarning,
                                [mbYes, mbNo], 0) = mrYes then
                        compute_and_save_EUWarTrap (configuration.cow_system_pct_file_name,
                           configuration.tau_file_name, configuration.s_file_name,
                           configuration.EUWarTrap_tau_file_name, configuration.EUWarTrap_s_unweighted_file_name,
                           configuration.distance_file_name, nation_list, contiguity_data,
                           configuration.first_eu_year_possible, configuration.last_eu_year_possible);
                  end
               else
                        compute_and_save_EUWarTrap (configuration.cow_system_pct_file_name,
                           configuration.tau_file_name, configuration.s_file_name,
                           configuration.EUWarTrap_tau_file_name, configuration.EUWarTrap_s_unweighted_file_name,
                           configuration.distance_file_name, nation_list, contiguity_data,
                           configuration.first_eu_year_possible, configuration.last_eu_year_possible);

            finally
               self.EnableMenus;
               trace.hideprogress;  {need to hide this at the end of each procedure}
            end;
         end;
   except
      on EInOutError do
         begin
         {do nothing but clear the exception, ready for user to try again}
         end;
      on EUserInterrupt do
         begin
            {do nothing but clear the exception, ready for user to try again}
            UserInterrupt := false;
         end;
   end;
end;

{----------------------------------------------------------------------}

{---------------------------------------------------------------------}

procedure TEUGeneMainFrame.ExpectedUtilityWarandReasonMenuChoiceClick(Sender: TObject);
var option_result : integer;
    output_file_name : TFileName;
begin
   try
      user_selections.compute_this := compute_EUWarReasonProb;
      Calculation_box.showmodal;
      option_result := Calculation_box.modalresult;
      case user_selections.similarity_method of
         use_tau : output_file_name := configuration.EUWarReason_Tau_file_name;
         use_s : case user_selections.s_weighting of
                  weighted : output_file_name := configuration.EUWarReason_S_weighted_file_name;
                  unweighted : output_file_name := configuration.EUWarReason_S_unweighted_file_name;
                  else EUGeneError ('Problem in mdiframe - sweighting not right value when calling compute EU war reason.  Fatal error - notify programmer.', 5, stop, error_log);
                 end;
         else EUGeneError ('Problem in mdiframe - similarity_method not use_tau or use_s when calling compute EU war reason.  Fatal error - notify programmer.', 5, stop, error_log);
      end;  {case}
      {disable the buttons that would allow user to start procedures that might
       interfere with running procedures, then call procedure.  }
      if option_result = mrOK then
         begin
            try
               self.DisableMenus;
               {Make sure it's ok to overwrite the file, if there is one}
               if FileExists (output_file_name) then
               begin
                  if MessageDlg(('There is an existing War and Reason Utility data file.  Overwrite?  '+
                             '(Yes overwrites, No cancels recalculate procedure)'), mtWarning,
                             [mbYes, mbNo], 0) = mrYes then
                     compute_and_save_EUWarReason (configuration.cow_system_pct_file_name,
                        configuration.tau_file_name, configuration.s_file_name,
                        configuration.risk_tau_file_name, configuration.risk_S_unweighted_file_name,
                        configuration.risk_wtr_file_name,
                        configuration.distance_file_name, output_file_name,
                        nation_list, contiguity_data, configuration.first_eu_year_possible,
                        configuration.last_eu_year_possible, user_selections);
               end
               else
                     compute_and_save_EUWarReason (configuration.cow_system_pct_file_name,
                        configuration.tau_file_name, configuration.s_file_name,
                        configuration.risk_tau_file_name, configuration.risk_S_unweighted_file_name,
                        configuration.risk_wtr_file_name,
                        configuration.distance_file_name, output_file_name,
                        nation_list, contiguity_data, configuration.first_eu_year_possible,
                        configuration.last_eu_year_possible, user_selections);

            finally
               self.EnableMenus;
               trace.hideprogress;  {need to hide this at the end of each procedure}
            end;
         end;
   except
      on EInOutError do
         begin
         {do nothing but clear the exception, ready for user to try again}
         end;
      on EUserInterrupt do
         begin
            {do nothing but clear the exception, ready for user to try again}
            UserInterrupt := false;
         end;
   end;
end;

{----------------------------------------------------------------------}

procedure TEUGeneMainFrame.Contents1Click(Sender: TObject);

var
  MenuHelpForm: Thelpunit;

begin
   try
      try
         MenuHelpForm := Thelpunit.create(application);
         MenuHelpForm.Top := 75;
         MenuHelpForm.Left := 75;
         MenuHelpForm.hide;
         MenuHelpForm.Showmodal;
         except
            on EInOutError do
               begin
               {do nothing but clear the exception, ready for user to try again}
               end;
            on EUserInterrupt do
               begin
                  {do nothing but clear the exception, ready for user to try again}
                  UserInterrupt := false;
               end;
         end;   {except}
      finally
         MenuHelpForm.free;
      end;
end;

{----------------------------------------------------------------------}

procedure TEUGeneMainFrame.TraceOnClick(Sender: TObject);
begin
   TraceOn.checked := true;
   TraceOff.checked := false;
   TraceWindow.show;
end;

procedure TEUGeneMainFrame.TraceOffClick(Sender: TObject);
begin
   TraceOn.checked := false;
   TraceOff.checked := true;
   TraceWindow.hide;
end;

procedure TEUGeneMainFrame.ShowOutputWindow1Click(Sender: TObject);
begin
   outputWindow.show;
end;

{----------------------------------------------------------------------}

procedure TEUGeneMainFrame.TestMenuClick(Sender: TObject);
begin
   testclick;
end;

{----------------------------------------------------------------------}

procedure TEUGeneMainFrame.EnableMenus;
begin
      file1.enabled:=true;
      recomputeMenu.enabled := true;
      outputMenu.enabled := true;
      TraceMenu.enabled := true;
      HelpMenu.enabled := true;
      TestMenuItem.enabled := true;
      RiskSecurityProcs1.enabled := true;
      UserDataMenu.enabled := true;
end;

{----------------------------------------------------------------------}

procedure TEUGeneMainFrame.DisableMenus;
begin
      file1.enabled:=false;
      recomputeMenu.enabled := false;
      outputMenu.enabled := false;
      {TraceMenu.enabled := false;}
      HelpMenu.enabled := false;
      TestMenuItem.enabled := false;
      RiskSecurityProcs1.enabled := false;
      UserDataMenu.enabled := false;
end;

{----------------------------------------------------------------------}

procedure TEUGeneMainFrame.RiskDataConvert1Click(Sender: TObject);
begin
   ShowMessage ('Risk Data has already been converted.  This procedure does not need to be run again.');
   {riskfileconvert; }
end;

procedure TEUGeneMainFrame.RiskDataShow1Click(Sender: TObject);
begin
   risk_data_show;
end;

procedure TEUGeneMainFrame.SecAllianceFileCreate1Click(Sender: TObject);
begin
   ShowMessage ('Initial Security Data has already been converted.  This procedure does not need to be run again.');
   riskfile_initial_security_alliance_file_create;
end;

{----------------------------------------------------------------------}

procedure TEUGeneMainFrame.RiskAllYears1Click(Sender: TObject);
var risk_file : risk_file_type_v2;
    file_ready : boolean;
    temp_risk_file_name, temp_security_alliance_file_name : TFileName;
begin
   try
      user_selections.compute_this := compute_risk;
      Calculation_box.showmodal;
      if Calculation_box.modalresult = mrOK then
         try
            file_ready := false;
            case user_selections.similarity_method of
              use_tau : begin
                    temp_risk_file_name := configuration.risk_Tau_file_name;
                    temp_security_alliance_file_name := configuration.security_alliance_Tau_file_name;
                 end;
              use_s : case user_selections.s_weighting of
                        weighted : begin
                             temp_risk_file_name := configuration.risk_S_weighted_file_name;
                             temp_security_alliance_file_name := configuration.security_alliance_S_weighted_file_name;
                           end;
                        unweighted : begin
                             temp_risk_file_name := configuration.risk_S_unweighted_file_name;
                             temp_security_alliance_file_name := configuration.security_alliance_S_unweighted_file_name;
                           end;
                       else EUGeneError ('Problem in mdiframe - sweighting not right value when calling compute Risk all yrs.  Fatal error - notify programmer.', 5, stop, error_log);
                    end;
              else EUGeneError ('Came into All years risk proc from main menu procedure without s/tau option set.  NOtify programmer.  Fatal error.  ',2,stop,error_log);
            end;   {case}
            assignFile (risk_file, temp_risk_file_name);
            if (fileExists (temp_risk_file_name)) then
              begin
                 reset (risk_file);
                 if (filesize(risk_file)>0) then
                    begin
                       try
                          RiskOverwriteDeleteForm := TRiskOverwriteDeleteForm.create (self);
                          RiskOverwriteDeleteForm.showmodal;
                          if RiskOverwriteDeleteForm.modalresult = mrOK then  {overwrite existing}
                             begin
                                case RiskOverwriteDeleteForm.deleteRiskFile of
                                   true : begin
                                         {Close and blank it with a rewrite.}
                                         closefile(risk_file);
                                         rewrite (risk_file);
                                         closefile(risk_file);
                                      end;
                                   false :begin
                                         {don't need to do anything, just close the open file.
                                          The regular reset and get
                                          to first year procedure in the compute risk proc will be OK}
                                         closefile(risk_file);
                                      end;
                                   end;    {case}
                                file_ready := true;
                             end
                          else
                          {if they canceled the overwrite form, then file_ready will stay false,
                           and the procedure won't execute.  Just close the open file.}
                             closefile(risk_file);
                       finally
                          RiskOverwriteDeleteForm.release;
                       end;    {finally}
                    end
                 else   {file size is 0, it's OK to go on an overwrite inside compute proc}
                    begin
                       closefile(risk_file);
                       file_ready := true;
                    end;
              end   {if file_exists}
            else   {file does not exist, so OK to overwrite}
              file_ready := true;

            if file_ready then
              begin
                 {disable the buttons that would allow user to start procedures that might
                 interfere with running procedures, then call procedure.  }
                 self.DisableMenus;
                  {Here, for risk, start determined by capabilities and alliances,
                   as saved in first, last_risk_year.}
                 {call procedure with appropriate I/O files for s or tau version}
                 compute_and_save_risk (configuration.cow_system_pct_file_name,
                     configuration.cow_alliance_file_name, configuration.alliance_seq_file_name,
                     configuration.dyadic_alliance_file_name,
                     temp_security_alliance_file_name,
                     temp_risk_file_name, configuration.distance_file_name,
                     nation_list, contiguity_data, configuration.first_risk_year,
                     configuration.last_risk_year, user_selections.alliance_data_source,
                     user_selections.risk_calculation_info);
              end;   {if file_ready}
            finally
                 self.EnableMenus;
                 trace.hideprogress;  {need to hide this at the end of each procedure}
              end;
   except
         on EInOutError do
         begin
         {do nothing but clear the exception, ready for user to try again}
         end;
      on EUserInterrupt do
         begin
            {do nothing but clear the exception, ready for user to try again}
            UserInterrupt := false;
         end;
   end;
end;

{----------------------------------------------------------------------}

procedure TEUGeneMainFrame.RiskSubsetofYears1Click(Sender: TObject);
var RiskYearBox : TRiskGenForm;
    RiskOutFileName, temp_security_alliance_file_name : TFileName;
begin
   try
      user_selections.compute_this := compute_risk;
      RiskYearBox := TRiskGenForm.create(self);
      RiskYearBox.showmodal;

      if RiskYearBox.modalresult = MROK then
      begin;
         Calculation_box.showmodal;
         if Calculation_box.modalresult = mrOK then
            begin
               try
                  {disable the buttons that would allow user to start procedures that might
                   interfere with running procedures, then call procedure.  }
                  self.DisableMenus;
                  {Set needed values from Risk sub-year generation form}
                  user_selections.years := subrange;
                  user_selections.first_year := RiskYearBox.year1;
                  user_selections.last_year := RiskYearBox.year2;
                  SettingsForm.label1.caption:='Start Year Selected = ' + inttostr(user_selections.first_year);
                  SettingsForm.label2.caption:='End Year Selected = ' + inttostr(user_selections.last_year);
                  case RiskYearBox.OverwriteRiskRecords of
                     true: case user_selections.similarity_method of
                          use_tau : begin
                                RiskOutFileName := configuration.risk_Tau_file_name;
                                temp_security_alliance_file_name := configuration.security_alliance_Tau_file_name;
                             end;
                          use_s : case user_selections.s_weighting of
                                    weighted : begin
                                         RiskOutFileName := configuration.risk_S_weighted_file_name;
                                         temp_security_alliance_file_name := configuration.security_alliance_S_weighted_file_name;
                                       end;
                                    unweighted : begin
                                         RiskOutFileName := configuration.risk_S_unweighted_file_name;
                                         temp_security_alliance_file_name := configuration.security_alliance_S_unweighted_file_name;
                                       end;
                                   else EUGeneError ('Problem in mdiframe - sweighting not right value when calling compute Risk all yrs.  Fatal error - notify programmer.', 5, stop, error_log);
                                end;
                          else EUGeneError ('Came into risk subset year proc from main menu procedure without s/tau option set.  NOtify programmer.  Fatal error.  ',2,stop,error_log);
                        end;   {case}
                     false: case user_selections.similarity_method of
                              use_tau : begin
                                    RiskOutFileName := configuration.eugene_directory+'RiskSubsetTau' +
                                       inttostr(user_selections.first_year) + inttostr(user_selections.last_year) + '.dat';
                                    temp_security_alliance_file_name := configuration.eugene_directory+'SecurityAllianceSubsetTau' +
                                       inttostr(user_selections.first_year) + inttostr(user_selections.last_year) + '.dat';
                                 end;
                             use_s : case user_selections.s_weighting of
                                       weighted : begin
                                            RiskOutFileName := configuration.eugene_directory+'RiskSubsetSWeighted' +
                                                      inttostr(user_selections.first_year) + inttostr(user_selections.last_year) + '.dat';
                                            temp_security_alliance_file_name := configuration.eugene_directory+'SecurityAllianceSubsetSWeighted' +
                                                      inttostr(user_selections.first_year) + inttostr(user_selections.last_year) + '.dat';
                                          end;
                                       unweighted : begin
                                            RiskOutFileName := configuration.eugene_directory+'RiskSubsetSUnweighted' +
                                                      inttostr(user_selections.first_year) + inttostr(user_selections.last_year) + '.dat';
                                            temp_security_alliance_file_name := configuration.eugene_directory+'SecurityAllianceSubsetSUnweighted' +
                                                      inttostr(user_selections.first_year) + inttostr(user_selections.last_year) + '.dat';
                                          end;
                                      else EUGeneError ('Problem in mdiframe - sweighting not right value when calling compute Risk all yrs.  Fatal error - notify programmer.', 5, stop, error_log);
                                   end;
                              else EUGeneError ('Came into risk subset year proc from main menu procedure without s/tau option set.  NOtify programmer.  Fatal error.  ',2,stop,error_log);
                           end;   {case}
                  end; {case}

                  compute_and_save_risk (configuration.cow_system_pct_file_name,
                     configuration.cow_alliance_file_name, configuration.alliance_seq_file_name,
                     configuration.dyadic_alliance_file_name,
                     temp_security_alliance_file_name,
                     RiskOutFileName, configuration.distance_file_name,
                     nation_list, contiguity_data, user_selections.first_year,
                     user_selections.last_year, user_selections.alliance_data_source,
                     user_selections.risk_calculation_info);
               finally
                  RiskYearBox.release;
                  self.EnableMenus;
                  trace.hideprogress;  {need to hide this at the end of each procedure}

               end;
            end;
      end;   {time result OK}
   except
         on EInOutError do
         begin
         {do nothing but clear the exception, ready for user to try again}
         end;
      on EUserInterrupt do
         begin
            {do nothing but clear the exception, ready for user to try again}
            UserInterrupt := false;
         end;
   end;

end;

{----------------------------------------------------------------------}

procedure TEUGeneMainFrame.RiskAppendNewYears1Click(Sender: TObject);
var temprisk_file : risk_file_type_v2;
    temp_risk_file_record : ^risk_file_record_type_v2;
    temp_risk_file_name, temp_security_alliance_file_name : TFileName;
    right : longint;

begin
   try

      showmessage ('This procedure is currently disabled.');

   if false then
   begin
      showmessage ('NOTE:  This procedure is currently set to only append post 1997 data.  To update before 1997, use the "Subset of Years" risk procedure.');

      user_selections.compute_this := compute_risk;

      Calculation_box.showmodal;
      if Calculation_box.modalresult = mrOK then
         begin
            try
               {Check to see what years are already in risk file}
               case user_selections.similarity_method of
                 use_tau : begin
                       temp_risk_file_name := configuration.risk_Tau_file_name;
                       temp_security_alliance_file_name := configuration.security_alliance_Tau_file_name;
                    end;
                 use_s : case user_selections.s_weighting of
                        weighted : begin
                             temp_risk_file_name := configuration.risk_S_weighted_file_name;
                             temp_security_alliance_file_name := configuration.security_alliance_S_weighted_file_name;
                           end;
                        unweighted : begin
                             temp_risk_file_name := configuration.risk_S_unweighted_file_name;
                             temp_security_alliance_file_name := configuration.security_alliance_S_unweighted_file_name;
                           end;
                       else EUGeneError ('Problem in mdiframe - sweighting not right value when calling compute Risk all yrs.  Fatal error - notify programmer.', 5, stop, error_log);
                    end;
                 else EUGeneError ('Came into All years risk proc from main menu procedure without s/tau option set.  NOtify programmer.  Fatal error.  ',2,stop,error_log);
               end;   {case}
               assignFile (temprisk_file, temp_risk_file_name);
               if (fileExists (temp_risk_file_name)) then
                 begin
                    reset (temprisk_file);
                    if (filesize(temprisk_file)>0) then
                       begin   {file exists, and has records, so you can append}
                          {File is already open here.  Note - must use reset b/c file is typed, not text,
                           so file is not opened read only.}
                          right := filesize(temprisk_file);
                          seek (temprisk_file, right-1);
                          new (temp_risk_file_record);
                          read (temprisk_file, temp_risk_file_record^);
                          trace.message ('Last year in risk file: '+inttostr(temp_risk_file_record^.year));
                          trace.message ('Last year possible: '+inttostr(configuration.last_risk_year));
                          if configuration.last_risk_year <= temp_risk_file_record^.year then
                             showmessage ('Cannot create additional risk records - no additional years of capability/alliance data are available')
                          else
                             if MessageDlg(('Last year of current risk file is '+ inttostr(temp_risk_file_record^.year) +
                                   '.  Append to this file, up to '+ inttostr(configuration.last_risk_year) + '(last year possible)?  [Yes appends, No cancels procedure]'), mtWarning,
                                   [mbYes, mbNo], 0) = mrYes   then
                                begin
                                   {close the file (it will be reopened inside the computer procedure),
                                    get calculation options, then compute}
                                   closefile (temprisk_file);

                                   {NOW do actual computing}
                                    {disable the buttons that would allow user to start procedures that might
                                     interfere with running procedures, then call procedure.  }
                                    self.DisableMenus;
                                    {Set needed values from Risk sub-year generation form}
                                    user_selections.years := subrange;
                                    user_selections.first_year := temp_risk_file_record^.year+1;
                                    user_selections.last_year := configuration.last_risk_year;
                                    SettingsForm.label1.caption:='Start Year Selected = ' + inttostr(user_selections.first_year);
                                    SettingsForm.label2.caption:='End Year Selected = ' + inttostr(user_selections.last_year);

                                    compute_and_save_risk (configuration.cow_system_pct_file_name,
                                       configuration.cow_alliance_file_name, configuration.alliance_seq_file_name,
                                       configuration.dyadic_alliance_file_name,
                                       temp_security_alliance_file_name,
                                       temp_risk_file_name, configuration.distance_file_name,
                                       nation_list, contiguity_data, user_selections.first_year,
                                       user_selections.last_year, user_selections.alliance_data_source,
                                       user_selections.risk_calculation_info);
                                end
                                else
                                begin    {user said no, so close the file.}
                                   closefile (temprisk_file);
                                end;
                       end    {if filesize > 0}
                       else
                       begin
                          showmessage ('Risk file has size 0 - appending not possible');
                          closefile (temprisk_file);
                       end;
                 end   {risk file exists}
               else
                 showmessage ('Risk file does not exist - appending not possible');


            finally
               self.EnableMenus;
               trace.hideprogress;  {need to hide this at the end of each procedure}
            end;
         end;

   end;     {if false, don't execute this, loop}

   except
      on EInOutError do
         begin
         {do nothing but clear the exception, ready for user to try again}
         end;
      on EUserInterrupt do
         begin
            {do nothing but clear the exception, ready for user to try again}
            UserInterrupt := false;
         end;
   end;

end;

{----------------------------------------------------------------------}

procedure TEUGeneMainFrame.RiskSingleDisplayProcedureClick(Sender: TObject);
   {Will not overwrite any risk files, just computes and displays a single ccode-year detail.
    This calls compute_and_save_risk, but that proc checks internally to see if this
    "display only" option was chosen}
var risk_file : risk_file_type_v2;
    temp_risk_file_name, temp_security_alliance_file_name : TFileName;
begin
   try
      user_selections.compute_this := compute_single_risk;
      case user_selections.similarity_method of
        use_tau : begin
              temp_risk_file_name := configuration.risk_Tau_file_name;
              temp_security_alliance_file_name := configuration.security_alliance_Tau_file_name;
           end;
        use_s : case user_selections.s_weighting of
                  weighted : begin
                       temp_risk_file_name := configuration.risk_S_weighted_file_name;
                       temp_security_alliance_file_name := configuration.security_alliance_S_weighted_file_name;
                     end;
                  unweighted : begin
                       temp_risk_file_name := configuration.risk_S_unweighted_file_name;
                       temp_security_alliance_file_name := configuration.security_alliance_S_unweighted_file_name;
                     end;
                 else EUGeneError ('Problem in mdiframe - sweighting not right value when calling compute Risk all yrs.  Fatal error - notify programmer.', 5, stop, error_log);
              end;
        else EUGeneError ('Came into All years risk proc from main menu procedure without s/tau option set.  NOtify programmer.  Fatal error.  ',2,stop,error_log);
      end;   {case}
      assignFile (risk_file, temp_risk_file_name);

      Calculation_box.showmodal;
      if Calculation_box.modalresult = mrOK then
        try
           {disable the buttons that would allow user to start procedures that might
           interfere with running procedures, then call procedure.  }
           self.DisableMenus;
            {Here, for risk, start determined by capabilities and alliances,
             as saved in first, last_risk_year.}
           compute_and_save_risk (configuration.cow_system_pct_file_name,
               configuration.cow_alliance_file_name, configuration.alliance_seq_file_name,
               configuration.dyadic_alliance_file_name,
               temp_security_alliance_file_name,
               temp_risk_file_name, configuration.distance_file_name,
               nation_list, contiguity_data, configuration.first_risk_year,
               configuration.last_risk_year, user_selections.alliance_data_source,
               user_selections.risk_calculation_info);

         finally
           self.EnableMenus;
           trace.hideprogress;  {need to hide this at the end of each procedure}
           RiskOverwriteDeleteForm.release;
         end;
   except
         on EInOutError do
         begin
         {do nothing but clear the exception, ready for user to try again}
         end;
      on EUserInterrupt do
         begin
            {do nothing but clear the exception, ready for user to try again}
            UserInterrupt := false;
         end;
   end;

end;           {risk test procedure}

procedure TEUGeneMainFrame.CombineRiskFilesMenuItemClick(Sender: TObject);
var CombineRiskFileForm : TCombineRiskFileForm;
begin
   try
      CombineRiskFileForm := TCombineRiskFileForm.create(application);
      CombineRiskFileForm.showmodal;
   finally
      CombineRiskFileForm.free;
   end;
end;

procedure TEUGeneMainFrame.showsecuritytaumenuClick(Sender: TObject);
begin
   security_data_show (configuration.security_alliance_Tau_file_name);
end;

procedure TEUGeneMainFrame.ShowSecuritySMenuClick(Sender: TObject);
begin
   security_data_show (configuration.security_alliance_S_unweighted_file_name);
end;

procedure TEUGeneMainFrame.TransferUserDataSets1Click(Sender: TObject);
begin
   Application.CreateForm(TFTP_Form, FTP_Form);
   FTP_Form.showmodal;
   FTP_Form.free;
end;

procedure TEUGeneMainFrame.CreateDataConfigClick(Sender: TObject);
begin
   {might or might not need to transfer with ftp forms, but create in case I do.}
   Application.CreateForm(TFTP_Form, FTP_Form);
   UserDataPrepForm.showmodal;
   FTP_Form.free;
end;

procedure TEUGeneMainFrame.RunRandysProcedureClick(Sender: TObject);
begin
   try
      RandysWindow := TRandysWindow.create(application);
      RandysWindow.showmodal;
   finally
      RandysWindow.free;
   end;

end;

{ ----------------------------------------------  }

procedure TEUGeneMainFrame.SaveSettingsMenuClick(Sender: TObject);
{user_selections : user_selection_type;
   configuration : configuration_type;  }
   var saved_settings_for_output : saved_settings_type;
       saved_settings_file : saved_settings_file_type;
       current_user_dyad : user_dyad_ptr;
       counter2, counter : integer;

begin
   {Now put from user_selections format into format for saved variable for file.}
   with saved_settings_for_output.user_selections do
   begin
      dyads_selected := user_selections.dyads_selected;
      monads_selected := user_selections.monads_selected;
      disputes_selected:= user_selections.disputes_selected;
      selected_country_list := user_selections.selected_country_list;
      first_year := user_selections.first_year;
      last_year := user_selections.last_year;
      years := user_selections.years;
      compute_this   := user_selections.compute_this;
      output_this := user_selections.output_this;
      alliance_data_source := user_selections.alliance_data_source;
      similarity_method := user_selections.similarity_method;
      s_weighting := user_selections.s_weighting;
      risk_data_source := user_selections.risk_data_source;
      risk_calculation_info := user_selections.risk_calculation_info;
      eu_calculation_info:= user_selections.eu_calculation_info;
      tau_leader_calculation_info := user_selections.tau_leader_calculation_info;
      s_leader_calculation_info := user_selections.s_leader_calculation_info;
      distance_method := user_selections.distance_method;
      werner_peace_year_adjustment := user_selections.werner_peace_year_adjustment;
      contiguity_level_required := user_selections.contiguity_level_required;
      maximum_distance := user_selections.maximum_distance;
      selected_regions := user_selections.selected_regions;
      capability_modifications := user_selections.capability_modifications;
      dispute_info := user_selections.dispute_info;
      sample_info := user_selections.sample_info;
      conflict_exclusion_selection := user_selections.conflict_exclusion_selection;
      complete := user_selections.complete;

     {These sections are special because of pointers, indefinite arrays, etc.
         output_format : output_format_type;     ** DONE **
         user_specified_dyad_list : specified_dyad_list_type;
         user_data_sets_selections : user_data_sets_selection_type;   }

      {from output_format}
      output_format.output_set := user_selections.output_format.output_set;
      output_format.header := user_selections.output_format.header;
      output_format.printii := user_selections.output_format.printii;
      output_format.printAllOngoing := user_selections.output_format.printAllOngoing;
      output_format.printOngoingIfNewDisp := user_selections.output_format.printOngoingIfNewDisp;
      output_format.location := user_selections.output_format.location;
      output_format.overwrite := user_selections.output_format.overwrite;
      output_format.output_file_name := user_selections.output_format.output_file_name;
      output_format.variables := user_selections.output_format.variables;
      output_format.separator := user_selections.output_format.separator;
      output_format.commandFiles := user_selections.output_format.commandFiles;

      {from user_specified_dyad_list}
      specified_dyad_list_file_name := user_selections.user_specified_dyad_list.file_name;
      current_user_dyad := user_selections.user_specified_dyad_list.first_dyad;
      num_dyads := 0;
      while current_user_dyad <> nil do
         begin
            inc(num_dyads);
            user_specified_dyads[num_dyads].ccode1 := current_user_dyad^.ccode1;
            user_specified_dyads[num_dyads].ccode2 := current_user_dyad^.ccode2;
            user_specified_dyads[num_dyads].styear := current_user_dyad^.styear;
            user_specified_dyads[num_dyads].endyear := current_user_dyad^.endyear;
            current_user_dyad := current_user_dyad^.next_rec;
         end;
      for counter := num_dyads + 1 to max_saved_user_dyads do
         begin
            user_specified_dyads[counter].ccode1 := min_ccode;
            user_specified_dyads[counter].ccode2 := min_ccode;
            user_specified_dyads[counter].styear := min_year;
            user_specified_dyads[counter].endyear := min_year;
         end;

      {from user_data_sets_selections}
      num_user_data_sets := length (user_selections.user_data_sets_selections);
      for counter := 0 to length (user_selections.user_data_sets_selections)-1 do
         begin
            {Here, set the full name of the original data set that corresponds to this user selected set.
             This will be used to check later when data set info is read in "read settings".}
            user_data_sets_selections[counter].full_name := configuration.User_data_set_info.get_data_set_full_name(counter);

            {Now mark variables in that data set}
            for counter2 := 0 to length (user_selections.user_data_sets_selections[counter].data_set_variables)-1 do
               user_data_sets_selections[counter].data_set_variables[counter2] :=
                  user_selections.user_data_sets_selections[counter].data_set_variables[counter2];
            for counter2 := length (user_selections.user_data_sets_selections[counter].data_set_variables) to
               max_user_variables_per_data_set do
               user_data_sets_selections[counter].data_set_variables[counter2] := -1;
         end;
   end;   {with saved_settings_for_output.user_selections }

   assignfile (saved_settings_file, configuration.saved_settings_file_name);
   rewrite (saved_settings_file);
   write (saved_settings_file, saved_settings_for_output);
   closefile (saved_settings_file);

   showmessage('Settings saved!');


end;

{ ----------------------------------------------  }

procedure TEUGeneMainFrame.LoadSettingsMenuClick(Sender: TObject);
   var saved_settings_for_input : saved_settings_type;
       saved_settings_file : saved_settings_file_type;
       current_user_dyad, first_user_dyad, last_user_dyad : user_dyad_ptr;
       varcount, dyadcounter, input_data_set_var_counter, input_data_set_counter, matched_user_data_set_number, current_user_data_set_number : integer;

begin
   {configuration variable will be set by eumaind initialized procedure.
    Only need to load user settings}

   assignfile (saved_settings_file, configuration.saved_settings_file_name);
   if fileexists(configuration.saved_settings_file_name) then
   begin
      reset (saved_settings_file);
      read (saved_settings_file, saved_settings_for_input);
      closefile (saved_settings_file);

      {Now put from saved variable from file into user_selections format.}
      with user_selections do
      begin
         dyads_selected := saved_settings_for_input.user_selections.dyads_selected;
         monads_selected := saved_settings_for_input.user_selections.monads_selected;
         disputes_selected:= saved_settings_for_input.user_selections.disputes_selected;
         selected_country_list := saved_settings_for_input.user_selections.selected_country_list;
         first_year := saved_settings_for_input.user_selections.first_year;
         last_year := saved_settings_for_input.user_selections.last_year;
         years := saved_settings_for_input.user_selections.years;
         compute_this   := saved_settings_for_input.user_selections.compute_this;
         output_this := saved_settings_for_input.user_selections.output_this;
         alliance_data_source := saved_settings_for_input.user_selections.alliance_data_source;
         similarity_method := saved_settings_for_input.user_selections.similarity_method;
         s_weighting := saved_settings_for_input.user_selections.s_weighting;
         risk_data_source := saved_settings_for_input.user_selections.risk_data_source;
         risk_calculation_info := saved_settings_for_input.user_selections.risk_calculation_info;
         eu_calculation_info:= saved_settings_for_input.user_selections.eu_calculation_info;
         tau_leader_calculation_info := saved_settings_for_input.user_selections.tau_leader_calculation_info;
         s_leader_calculation_info := saved_settings_for_input.user_selections.s_leader_calculation_info;
         distance_method := saved_settings_for_input.user_selections.distance_method;
         werner_peace_year_adjustment := saved_settings_for_input.user_selections.werner_peace_year_adjustment;
         contiguity_level_required := saved_settings_for_input.user_selections.contiguity_level_required;
         maximum_distance := saved_settings_for_input.user_selections.maximum_distance;
         selected_regions := saved_settings_for_input.user_selections.selected_regions;
         capability_modifications := saved_settings_for_input.user_selections.capability_modifications;
         dispute_info := saved_settings_for_input.user_selections.dispute_info;
         sample_info := saved_settings_for_input.user_selections.sample_info;
         conflict_exclusion_selection := saved_settings_for_input.user_selections.conflict_exclusion_selection;
         complete := saved_settings_for_input.user_selections.complete;

        {These sections are special because of pointers, indefinite arrays, etc.
            output_format : output_format_type;     ** DONE **
            user_specified_dyad_list : specified_dyad_list_type;
            user_data_sets_selections : user_data_sets_selection_type;   }

         {from output_format}
         output_format.output_set := saved_settings_for_input.user_selections.output_format.output_set;
         output_format.header := saved_settings_for_input.user_selections.output_format.header;
         output_format.printii := saved_settings_for_input.user_selections.output_format.printii;
         output_format.printAllOngoing := saved_settings_for_input.user_selections.output_format.printAllOngoing;
         output_format.printOngoingIfNewDisp := saved_settings_for_input.user_selections.output_format.printOngoingIfNewDisp;
         output_format.location := saved_settings_for_input.user_selections.output_format.location;
         output_format.overwrite := saved_settings_for_input.user_selections.output_format.overwrite;
         output_format.output_file_name := saved_settings_for_input.user_selections.output_format.output_file_name;
         output_format.variables := saved_settings_for_input.user_selections.output_format.variables;
         output_format.separator := saved_settings_for_input.user_selections.output_format.separator;
         output_format.commandFiles := saved_settings_for_input.user_selections.output_format.commandFiles;

         {from user_specified_dyad_list}
         user_specified_dyad_list.file_name := saved_settings_for_input.user_selections.specified_dyad_list_file_name;

         first_user_dyad := nil;
         last_user_dyad := nil;
         if saved_settings_for_input.user_selections.num_dyads = 0 then
            user_specified_dyad_list.first_dyad := nil else
            begin
               for dyadcounter := 0 to saved_settings_for_input.user_selections.num_dyads do
                  begin
                     new(current_user_dyad);
                     current_user_dyad^.ccode1 := saved_settings_for_input.user_selections.user_specified_dyads[dyadcounter].ccode1;
                     current_user_dyad^.ccode2 := saved_settings_for_input.user_selections.user_specified_dyads[dyadcounter].ccode2;
                     current_user_dyad^.styear := saved_settings_for_input.user_selections.user_specified_dyads[dyadcounter].styear;
                     current_user_dyad^.endyear := saved_settings_for_input.user_selections.user_specified_dyads[dyadcounter].endyear;
                     current_user_dyad^.next_rec := nil;
                     if last_user_dyad <> nil then
                        last_user_dyad^.next_rec := current_user_dyad;
                     if first_user_dyad = nil then first_user_dyad := current_user_dyad;
                     last_user_dyad := current_user_dyad;
                  end;
               user_specified_dyad_list.first_dyad := first_user_dyad;
            end;


         SetLength(user_selections.user_data_sets_selections, saved_settings_for_input.user_selections.num_user_data_sets);

         for input_data_set_counter := 0 to saved_settings_for_input.user_selections.num_user_data_sets-1 do
            begin
               {first check and see if this data set exists in current EUGene run.}
               {Get its number.}
               matched_user_data_set_number := -1;
               for current_user_data_set_number := 0 to configuration.User_data_set_info.get_num_data_sets -1 do
                  if configuration.User_data_set_info.get_data_set_full_name(current_user_data_set_number) = saved_settings_for_input.user_selections.user_data_sets_selections[input_data_set_counter].full_name then
                     matched_user_data_set_number := current_user_data_set_number;
               {if the data set exists, then set variables.  matched_numer is the numer in the current eugene run.}
               if matched_user_data_set_number <> -1 then
                  begin
                     {first count how many legit variables}
                     varcount := 0;
                     for input_data_set_var_counter := 0 to max_user_variables_per_data_set-1 do if
                        saved_settings_for_input.user_selections.user_data_sets_selections[input_data_set_counter].data_set_variables[input_data_set_var_counter] <> -1 then
                        inc(varcount);
                     setlength (user_data_sets_selections[matched_user_data_set_number].data_set_variables, varcount);

                     {now add variables}
                     for input_data_set_var_counter := 0 to varcount-1 do
                        begin
                           user_data_sets_selections[matched_user_data_set_number].data_set_variables[input_data_set_var_counter] :=
                              saved_settings_for_input.user_selections.user_data_sets_selections[input_data_set_counter].data_set_variables[input_data_set_var_counter];
                        end;
                  end;
            end;

      end;   {with user_selections}

      showmessage('Settings loaded!');
   end         {if file exists}
   else showmessage ('Settings file not found.  Settings must be saved before they can be loaded.');
end;


procedure TEUGeneMainFrame.ViewDocumentation1Click(Sender: TObject);
begin
    ShellExecute(Handle, 'open',
      'docs\EUGeneDocumentation.doc',nil,nil, SW_SHOWNORMAL);
end;

procedure TEUGeneMainFrame.Website1Click(Sender: TObject);
begin
    ShellExecute(Handle, 'open', 'http://www.eugenesoftware.org',nil,nil, SW_SHOWNORMAL);
end;

procedure TEUGeneMainFrame.UserDataSetProceduresMenuItemClick(Sender: TObject);
begin
    ShellExecute(Handle, 'open', 'docs\EUGene user dataset procedures.doc',nil,nil, SW_SHOWNORMAL);
end;

procedure TEUGeneMainFrame.SplitMIDsOvertimeProcedureClick(Sender: TObject);
var MIDsOverTime : TMID_Over_Time_obj;
begin
   MIDsOverTime := TCOWMID_Over_Time_obj.init(configuration);
   MIDsOverTime.output_to_file('d:\eugene\MID_over_time_listing.csv',
                               'd:\eugene\MID_over_time_for_merge.csv');
   MIDsOverTime.free;
end;

procedure TEUGeneMainFrame.DyadicMIDData30MenuItemClick(Sender: TObject);
var DyadicGen: TDyadicGen;
begin
    DyadicGen := TDyadicGen.Create(application);
    DyadicGen.Showmodal;
    {Show a window here with input and output file names.}
    {Input name is set in eugene.ini
     Output name is where they will save file.}
{ShowMessage ('new window here if want to enter file names as input.');}
    {if not (configuration.cow_mid_data_format=disputeformat30) then
       showmessage ('Configuration file not updated to format30 data; this procedure can not be run')
    else
       begin
          DyadicMID30 := TDyadic_dispute_data_obj_integrated_format30.init(user_selections, configuration, 1992, 2001, none_pre_1992);

          outfilename := 'd:\eugene\MIDDyadic_v3.02.csv';
          DyadicMID30.output_to_file (outfilename);

          ShowMessage ('Dyadic MID output complete to file '+outfilename);
       end;      }

end;

procedure TEUGeneMainFrame.HypotheticalAllianceData1Click(Sender: TObject);

begin
   try
      AllianceOutputForm := TAllianceOutputForm.create(application);
      AllianceOutputForm.Init(configuration,application);
      AllianceOutputForm.ShowModal;
   finally
      AllianceOutputForm.Free;
   end;
end;

procedure TEUGeneMainFrame.DatasetBrowserMenuClick(Sender: TObject);
var DatasetBrowserForm : TDatasetBrowserForm;
begin
   try
      DatasetBrowserForm := TDatasetBrowserForm.Create(application);
      DatasetBrowserForm.Init(configuration, Sender);
      DatasetBrowserForm.ShowModal;
   finally
      DatasetBrowserForm.free;
   end;
end;


procedure TEUGeneMainFrame.OutputEUProjectedJoiningInfoClick(Sender: TObject);
   {This runs the "compute EU data" routine, but with file name and option set to output data
    to a file as it's going through.}
var option_result : integer;
    output_file_name, temporary_output_file_name : TFileName;
begin
   try
      {Set a location for output of component data}
      temporary_output_file_name := 'EUWarReasonJoiningComponents.csv';
      rewrite (user_selections.temporary_output_file, temporary_output_file_name);
      {Write header}
      writeln (user_selections.temporary_output_file, 'ccodeA, ccodeB, ccodeK, year,'+
               'KTauA, KTauB, Uppercase_RiskA, Uppercase_RiskB, AUKA_minus_UKB, BUKB_minus_UKA,'+
               'AdjCapKA, AdjCapKB');
      user_selections.compute_this := compute_EUWarReasonProb;
      user_selections.custom_procedure := EUWarReasonComponents;
      Calculation_box.showmodal;
      option_result := Calculation_box.modalresult;
      case user_selections.similarity_method of
         use_tau : output_file_name := configuration.EUWarReason_Tau_file_name;
         use_s : case user_selections.s_weighting of
                  weighted : output_file_name := configuration.EUWarReason_S_weighted_file_name+'temp';
                  unweighted : output_file_name := configuration.EUWarReason_S_unweighted_file_name+'temp';
                  else EUGeneError ('Problem in mdiframe - sweighting not right value when calling compute EU war reason.  Fatal error - notify programmer.', 5, stop, error_log);
                 end;
         else EUGeneError ('Problem in mdiframe - similarity_method not use_tau or use_s when calling compute EU war reason.  Fatal error - notify programmer.', 5, stop, error_log);
      end;  {case}
      {disable the buttons that would allow user to start procedures that might
       interfere with running procedures, then call procedure.  }
      if option_result = mrOK then
         begin
            try
               self.DisableMenus;
               {Make sure it's ok to overwrite the file, if there is one}
               if FileExists (output_file_name) then
               begin
                  if MessageDlg(('There is an existing War and Reason Utility data file.  Overwrite?  '+
                             '(Yes overwrites, No cancels recalculate procedure)'), mtWarning,
                             [mbYes, mbNo], 0) = mrYes then
                     compute_and_save_EUWarReason (configuration.cow_system_pct_file_name,
                        configuration.tau_file_name, configuration.s_file_name,
                        configuration.risk_tau_file_name, configuration.risk_S_unweighted_file_name,
                        configuration.risk_wtr_file_name,
                        configuration.distance_file_name, output_file_name,
                        nation_list, contiguity_data, configuration.first_eu_year_possible,
                        configuration.last_eu_year_possible, user_selections);
               end
               else
                     compute_and_save_EUWarReason (configuration.cow_system_pct_file_name,
                        configuration.tau_file_name, configuration.s_file_name,
                        configuration.risk_tau_file_name, configuration.risk_S_unweighted_file_name,
                        configuration.risk_wtr_file_name,
                        configuration.distance_file_name, output_file_name,
                        nation_list, contiguity_data, configuration.first_eu_year_possible,
                        configuration.last_eu_year_possible, user_selections);

            finally
               self.EnableMenus;
               trace.hideprogress;  {need to hide this at the end of each procedure}
            end;
         end;
   except
      on EInOutError do
         begin
         {do nothing but clear the exception, ready for user to try again}
         end;
      on EUserInterrupt do
         begin
            {do nothing but clear the exception, ready for user to try again}
            UserInterrupt := false;
         end;
   end;
   closefile (user_selections.temporary_output_file);

end;


procedure TEUGeneMainFrame.NonDirDisputeOnsetsMenuChoiceClick(Sender: TObject);
begin
   try
      user_selections.disputes_selected := all_disputes;
      output_sub_call (ccode_index, nation_list, contiguity_data, configuration, user_selections,
                       output_nondirected_dispute_dyads);
   except
      on EInOutError do
         begin
         {do nothing but clear the exception, ready for user to try again}
         end;
      on EUserInterrupt do
         begin
            {do nothing but clear the exception, ready for user to try again}
            UserInterrupt := false;
         end;
   end;     {try-except}
end;


procedure TEUGeneMainFrame.NonDirDisputeYearMenuChoiceClick(Sender: TObject);
begin
   try
      user_selections.disputes_selected := all_dispute_years;
      output_sub_call (ccode_index, nation_list, contiguity_data, configuration, user_selections,
                       output_nondirected_dispute_dyads);
   except
      on EInOutError do
         begin
         {do nothing but clear the exception, ready for user to try again}
         end;
      on EUserInterrupt do
         begin
            {do nothing but clear the exception, ready for user to try again}
            UserInterrupt := false;
         end;
   end;     {try-except}
end;


procedure TEUGeneMainFrame.DirDisputeInitiationMenuChoiceClick(Sender: TObject);
begin
   try
      user_selections.disputes_selected := all_disputes;
      output_sub_call (ccode_index, nation_list, contiguity_data, configuration, user_selections,
                       output_directed_dispute_initiation_dyads);
   except
      on EInOutError do
         begin
         {do nothing but clear the exception, ready for user to try again}
         end;
      on EUserInterrupt do
         begin
            {do nothing but clear the exception, ready for user to try again}
            UserInterrupt := false;
         end;
   end;     {try-except}
end;

procedure TEUGeneMainFrame.DirDisputeYearMenuChoiceClick(Sender: TObject);
begin
   try
      user_selections.disputes_selected := all_dispute_years;
      output_sub_call (ccode_index, nation_list, contiguity_data, configuration, user_selections,
                       output_directed_dispute_initiation_dyads);
   except
      on EInOutError do
         begin
         {do nothing but clear the exception, ready for user to try again}
         end;
      on EUserInterrupt do
         begin
            {do nothing but clear the exception, ready for user to try again}
            UserInterrupt := false;
         end;
   end;     {try-except}
end;

end.


