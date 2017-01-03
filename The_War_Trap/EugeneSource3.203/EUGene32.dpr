program EUGene32;

{EUGene  Copyright 1997-2007+ D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

{Version declared in EUTypes1}

{%ToDo 'EUGene32.todo'}

uses
  Forms,
  graphics,
  sysutils,
  dialogs,
  Abouteu in 'Abouteu.pas' {AboutBox},
  Calcbox in 'Calcbox.pas' {Calculation_box},
  Cntry in 'Cntry.pas' {CountryListDlg},
  Contigu in 'Contigu.pas' {contigForm},
  Errbx in 'Errbx.pas' {ErrorDialog},
  Eumaind in 'Eumaind.pas',
  Euinoutd in 'Euinoutd.pas',
  euprocs1 in 'euprocs1.pas',
  Euprocs2 in 'Euprocs2.pas',
  Eutestd in 'Eutestd.pas',
  Eutypes1 in 'Eutypes1.pas',
  Eutypes2 in 'Eutypes2.pas',
  FileError in 'FileError.pas' {FileErrorBox},
  Gabox in 'Gabox.pas' {GAOptionBox},
  GenrHelp in 'GenrHelp.pas' {GenericHelpForm},
  Mdiframe in 'Mdiframe.pas' {EUGeneMainFrame},
  TraceUnit in 'TraceUnit.pas' {TraceWindow},
  MenuHelp in 'MenuHelp.pas' {helpunit},
  PagedOutput in 'PagedOutput.pas' {Output_Options},
  ProgressInterruptWind in 'ProgressInterruptWind.pas' {ProgForm},
  Randbox in 'Randbox.pas' {RandomIterationForm},
  RiskGenBox in 'RiskGenBox.pas' {RiskGenForm},
  OutWindow in 'OutWindow.pas' {outputwindow},
  Setting in 'Setting.pas' {SettingsForm},
  SplashMove in 'SplashMove.pas' {Splashy},
  Steepbox in 'Steepbox.pas' {SteepestSubForm},
  TauOptions in 'TauOptions.pas' {SystemLeaderVariableOptions},
  EUtypes3 in 'EUtypes3.pas',
  cmnprocd in 'cmnprocd.pas',
  abortcheck in 'abortcheck.pas' {VerifyStopForm},
  EUOptions in 'EUOptions.pas' {EU_Options_Window},
  SWeightBox in 'SWeightBox.pas' {SimilarityWeightingForm},
  RiskAndSecurityConversionProcs in 'RiskAndSecurityConversionProcs.pas',
  RiskOverwriteForm in 'RiskOverwriteForm.pas' {RiskOverwriteDeleteForm},
  SingleRiskYear in 'SingleRiskYear.pas' {SingleRiskYearForm},
  RiskProgrammerWarnings in 'RiskProgrammerWarnings.pas' {SecurityFileOverwriteWarning},
  AllianceSource in 'AllianceSource.pas' {AllianceDataSourceForm},
  riskcombineintegrated in 'riskcombineintegrated.pas' {CombineRiskFileForm},
  runningwindow in 'runningwindow.pas' {RunningForm},
  CombineYearsSubbox in 'CombineYearsSubbox.pas' {CombineRiskFilesYearsForm},
  DistOptions in 'DistOptions.pas' {DistanceOutputOptions},
  EUTypesMID in 'EUTypesMID.pas',
  RereadNamesForm in 'RereadNamesForm.pas' {ReadVarNameOkForm},
  UserDataVarForm in 'UserDataVarForm.pas' {VariableInformationForm},
  originalriskunit in 'originalriskunit.pas',
  UserDataPreparationForm in 'UserDataPreparationForm.pas' {UserDataPrepForm},
  UserDataSetSub1 in 'UserDataSetSub1.pas' {UserDataSetSubForm1},
  UserDataSetSub2 in 'UserDataSetSub2.pas' {UserDataSetSubForm2},
  UserDataSetSub3 in 'UserDataSetSub3.pas' {UserDataSetSubForm3},
  FTPConnect in 'FTPConnect.pas' {FTP_Form},
  GetFilesForUpload in 'GetFilesForUpload.pas' {FilesForUploadForm},
  Randy in 'Randy.pas' {RandysWindow},
  SelectRegion in 'SelectRegion.pas' {RegionSelectForm},
  MidOverTime in 'MidOverTime.pas',
  EUTypes4 in 'EUTypes4.pas',
  PeaceYearsOptions in 'PeaceYearsOptions.pas' {PeaceYearsOptionForm},
  dyadgen in 'dyadgen.pas' {DyadicGen},
  AllianceOutput in 'AllianceOutput.pas' {AllianceOutputForm},
  RiskCombineRenameFile in 'RiskCombineRenameFile.pas' {RiskCombineFileRename},
  browser in 'browser.pas' {DatasetBrowserForm};

{VarBrowsePopup in 'VarBrowsePopup.pas' PopupBrowseForm;   }

{$R *.RES}

var mainfont : TFont;

begin

{  Use the following to do different directory!!
Use Application.ExeName to get the name of the exe file, then use
ExtractFilePath to extract the program directory:
   ProgDir := ExtractFilePath(Application.ExeName);
}

   try
     UserInterrupt := false;
     Application.Initialize;
     Application.Title := 'EUGene';
{ShowMessage ('Creating Main Form');}
     Application.CreateForm(TEUGeneMainFrame, EUGeneMainFrame);
  if private_version = true then
        begin
           EUGeneMainFrame.CustomProceduresMenu.enabled := true;
           EUGeneMainFrame.CustomProceduresMenu.Visible := true;
     	   EUGeneMainFrame.RiskSecurityProcs1.enabled := true;
     	   EUGeneMainFrame.RiskSecurityProcs1.Visible := true;
     	   EUGeneMainFrame.TestMenuItem.enabled := true;
     	   EUGeneMainFrame.TestMenuItem.Visible := true;
        end
     else
        begin
           EUGeneMainFrame.CustomProceduresMenu.enabled := false;
           EUGeneMainFrame.CustomProceduresMenu.Visible := false;
           EUGeneMainFrame.RiskSecurityProcs1.enabled := false;
           EUGeneMainFrame.RiskSecurityProcs1.Visible := false;
     	   EUGeneMainFrame.TestMenuItem.enabled := false;
     	   EUGeneMainFrame.TestMenuItem.Visible := false;
        end;
     EUGeneMainFrame.hide;
{ShowMessage ('Main Form Created - setting characteristics.');}

     {Note:  font is an object, so it must be set before being accessed.  It must
      also be set after the creation/initialization of EUGeneMainFrame.}
     mainfont := TFont.create;
     mainfont.assign(EUGeneMainFrame.font);
     mainfont.name := 'MS Sans Serif';
     mainfont.size := 9;

     EUGeneMainFrame.font.assign(mainfont);
     EUGeneMainFrame.PixelsPerInch := screen.pixelsPerInch;
     EUGeneMainFrame.Scaled := True;
     if Screen.PixelsPerInch > 75 then
       EUGeneMainFrame.PixelsPerInch := EUGeneMainFrame.PixelsPerInch + 30;
     EUGeneMainFrame.Top := 10;
     EUGeneMainFrame.Left := 10;

{ShowMessage ('Main Form characteristics set - doing animation and splash.');}
     Splashy := TSplashy.Create(application.mainform);
     Splashy.GlobeAnimation.Filename := ExtractFilePath(application.exename)+'TheEarth.avi';
     Splashy.GlobeAnimation.play(Splashy.GlobeAnimation.StartFrame, Splashy.GlobeAnimation.StopFrame, 0);
     Splashy.SplashScreenVersionLabel.caption := ('Version ' + realtostring(Eugene_version, 6, 3));
     if private_version = true then Splashy.SplashScreenVersionLabel.caption := Splashy.SplashScreenVersionLabel.caption + ' (custom build)';
     Splashy.top := EUGeneMainFrame.top;
     splashy.left := EUGeneMainFrame.left;
     Splashy.height := EUGeneMainFrame.height;
     Splashy.notready;
     Splashy.show;

     {These forms are ones it makes sense to create here}
     SettingsForm := TSettingsForm.Create(application);
     EUGeneMainFrame.clientheight := SettingsForm.height + 5;
     EUGeneMainFrame.clientwidth := SettingsForm.width + 5;

{ShowMessage ('Animation done - doing trace, help, other forms.');}

     TraceWindow := TTraceWindow.create(application);
     TraceWindow.Top := 75;
     TraceWindow.Left := 75;
     TraceWindow.hide;

     GenericHelpForm := TGenericHelpForm.create(application);
     GenericHelpForm.Top := 75;
     GenericHelpForm.Left := 75;
     GenericHelpForm.hide;

     trace := Ttrace_obj.init (0);
     {hide the progress bar from the main, outer trace. Now that the program is initialized,
      don't need to see it, but want to keep it in existence for vars like trace level.}
     trace.SetGlobalTrace;
     {trace.progform.hide?}

     Application.CreateForm(TRegionSelectForm, RegionSelectForm);
     Application.CreateForm(TPeaceYearsOptionForm, PeaceYearsOptionForm);
     Application.CreateForm(TAllianceDataSourceForm, AllianceDataSourceForm);
     Application.CreateForm(TRunningForm, RunningForm);

     Application.CreateForm(TContigForm, ContiguityBox);
     ContiguityBox.Top := 25;
     ContiguityBox.Left := 25;
     ContiguityBox.hide;

     Application.CreateForm(Toutputwindow, outputwindow);  {for dyad output to screen}
     outputwindow.Top := 50;
     outputwindow.Left := 100;
     outputWindow.hide;

{ShowMessage ('First set of forms done.  Doing initialize_data.');  }
     initialize_data (configuration, ccode_index, nation_list, contiguity_data,
                       error_log, debug, user_selections);

     {Do create the main output window here, because we want the options to be preserved
      between switches in the menu choices.}
     {Application.CreateForm(TOutput_Options, Output_Options);}
     EUGeneMainFrame.Output_Options := TOutput_Options.Create(application);
     EUGeneMainFrame.Output_Options.Top := 55;
     EUGeneMainFrame.Output_Options.Left := 75;
     EUGeneMainFrame.Output_options.PixelsPerInch := screen.pixelsPerInch;
     EUGeneMainFrame.Output_options.WindowState := wsNormal;
     EUGeneMainFrame.Output_options.clientheight := EUGeneMainFrame.Output_options.PagedOutput.height + EUGeneMainFrame.Output_options.ButtonGroupBox.height + 30 + 5;
     EUGeneMainFrame.Output_options.clientwidth := EUGeneMainFrame.Output_options.PagedOutput.width + 22;
     EUGeneMainFrame.Output_Options.hide;

{ShowMessage ('Initialization done.  Creating final forms.');}
{Not sure there is a need for the following to be created here}
     Application.CreateForm(TReadVarNameOkForm, ReadVarNameOkForm);
     Application.CreateForm(TVariableInformationForm, VariableInformationForm);
     Application.CreateForm(TFilesForUploadForm, FilesForUploadForm);
     Application.CreateForm(TUserDataSetSubForm1, UserDataSetSubForm1);
     Application.CreateForm(TUserDataSetSubForm2, UserDataSetSubForm2);
     Application.CreateForm(TUserDataSetSubForm3, UserDataSetSubForm3);
     Application.CreateForm(TUserDataPrepForm, UserDataPrepForm);


     Application.CreateForm(TCalculation_box, Calculation_box);
     Calculation_box.Top := 75;
     Calculation_box.Left := 75;
     Calculation_box.close;

     Application.CreateForm(TSimilarityWeightingForm, SimilarityWeightingForm);
     SimilarityWeightingForm.top := Calculation_box.top+calculation_box.height-SimilarityWeightingForm.height;
     SimilarityWeightingForm.Left := Calculation_box.left + (Calculation_box.width div 2);
     SimilarityWeightingForm.close;

     Application.CreateForm(TRandomIterationForm, RandomIterationForm);
     RandomIterationForm.Top := 75;
     RandomIterationForm.Left := 75;
     RandomIterationForm.hide;

     Application.CreateForm(TSteepestSubForm, SteepestSubForm);
     SteepestSubForm.Top := 75;
     SteepestSubForm.Left := 75;
     SteepestSubForm.hide;

     Application.CreateForm(TGAOptionBox, GAOptionBox);
     GAOptionBox.Top := 75;
     GAOptionBox.Left := 75;
     GAOptionBox.hide;


     Application.CreateForm(TFileErrorBox, FileErrorBox);
     FileErrorBox.Top := 75;
     FileErrorBox.Left := 75;
     FileErrorBox.hide;

     Application.CreateForm(TEU_Options_Window, EU_Options_Window);
     EU_Options_Window.Top := 75;
     EU_Options_Window.Left := 75;
     EU_Options_Window.hide;

     Application.CreateForm(TVerifyStopForm, VerifyStopForm);
     VerifyStopForm.Top := 75;
     VerifyStopForm.Left := 75;
     VerifyStopForm.hide;

     Splashy.ready;
     Splashy.GlobeAnimation.play(Splashy.GlobeAnimation.StartFrame, Splashy.GlobeAnimation.StopFrame, 0);
     Splashy.hide;
{Showmessage ('All forms done - about to show modal splash window.'); }
     splashy.showmodal;

     EUGeneMainFrame.enabled := true;
     EUGeneMainFrame.show;
     EUGeneMainFrame.BringToFront;
     EUGeneMainFrame.TraceOffClick (application);

     {showmessage ('total physical = '+inttostr(totalphysicalmem));
      showmessage ('available physicsal ='+ inttostr(availphysicalmem));
      showmessage ('total virtual = '+inttostr(totalvirtualmem));
      showmessage ('available virtual ='+ inttostr(availvirtualmem));  }
     {showmessage ('disk free current = '+inttostr(diskfree(0) div 1024));
     showmessage ('disk free C = '+inttostr(diskfree(3) div 1024));
     showmessage ('disk free D = '+inttostr(diskfree(4) div 1024));
     showmessage ('disk free F = '+inttostr(diskfree(6) div 1024));
     areal := diskfree(6) / 1024;
     showmessage ('disk free F = '+realtostring(areal,20,3));
     showmessage ('disk space C = '+inttostr(disksize(3) div 1024));
     showmessage ('disk space F = '+inttostr(disksize(6) div 1024));   }

{ShowMessage ('All forms done and splash closed - doing application.run.');}

     Application.Run;

   finally
     EUGeneMainFrame.enabled := false;
     close_down (error_log, nation_list, contiguity_data, ccode_index);

     ContiguityBox.free;
     outputwindow.free;
     TraceWindow.free;
     RandomIterationForm.free;
     GAOptionBox.free;
     Calculation_box.free;
     SteepestSubForm.free;
     GenericHelpForm.free;
     FileErrorBox.free;

     EUGeneMainFrame.Output_Options.free;
     Splashy.free;
     SettingsForm.free;
     {free the initial window tracing the basic data reads}
     trace.free;
     {EUGeneMainFrame.free;  this is done automatically on exit}

   end;   {finally}

end.


