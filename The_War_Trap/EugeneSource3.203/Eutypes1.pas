unit Eutypes1;

{EUGene  Copyright 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007+
 D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

    {This unit contains the main type, object, and variable declarations for the EU program,
     and object methods.  }

{ ------------------------------------------------------------ }

interface

uses dialogs, math, cmnprocD, sysUtils, windows, fileCtrl, StdCtrls,
     FileError, TraceUnit, ProgressInterruptWind;

const
   Eugene_Version = 3.203;
   private_version = false;

      {Version 1.0 prepared for distribution 4/28/98 }
      {Version 1.12 first version distributed from Web Site after CMS award received.}

   {Beta versions, 0.9+}
   {0.92 adds dispute processing, new dispute output selections}
   {0.93 adds exception blocks, and exit to main menu instead of halting.}
   {0.94 added globe animation.}
   {0.95 adds change / autodetect in file paths, adds choice of output separators}
   {0.96 adds polity, sampling, set of dyads from user file}
   {0.98 made some small bug fixes and made the help files rich text format.}
   {1.0 is first release, May 1 1998, that date set for CMS award submission.}
   {1.01 has minor screen adjustments, and an adjustment of polity code.
    1.011 and 1.012 are also minor polity missing value code adjustments.
    1.1 adds the option to include or drop joiner dyad-years.
    1.11 removes option to adjust phi [euoptions.pas file], since it seems to
         generate problematic values.  Note that phi code is still embedded in program
         as an option, but user has no where to choose phi for computations.
         [NOTE: phi options added back in v2.2.]
         Also modifies output tab setup to be clearer.
         Also fixes a problem with user-specified random number seeds not being kept
         in memory from run to run.  Updated help files to match new output options.
    1.12 fixed a problem in the "read dyads from user file" procedures, which created
         unintended duplicate dyads when the user listed a dyad twice (with different dates,
         for instance.
         This is the public release version!
    1.13 adds peace years variable.  It also adds an additional option for treating joiners
         as initiators, and clarified what it meant to treat joiners as initiators in doing
         directed dispute dyad data.  It also fixed a small problem where ongoing dispute years
         were printed even when the members were not states (this originally stems from a problem
         with COW data, sincd MID v2.1 does not match the 1994 system membership list perfectly.
         This version also begins using system membership data from the 1997 COW update, which has
         a different format than the previous nations.raw file.  This meant changing all of the
         nation_data.init routines.
    1.14 changes set up so cases are not dropped unless MID variables are selected.  Distribution
         also includes new, correcte latitude/longitude file.  Corrects a bug where values of -1 were
         mistakenly reported for system capabilities when COW capability data set did not have values,
         even though country was indeed a system member.  Corrects a bug where values of -9 were
         outputted for some missing polity data.
    1.15 fixes a miscoding in the peace years variable.  It was resetting to 0 in the year of a MID,
         but should increment to a new year value in that year and reset to 0 the following year.
    1.16 updates reading of Polity III data to correct mismatch between COW and Polity III, where
         COW has italy as ccode 325 always, but polity has 324/325; and where COW has yugoslavia/serbia
         as ccode 345 always, but polity has 344/345.
    1.17 fixes reading of new COW state membership data where some names were being read wrong,
         and where as a result some countries were unavailable for output (e.g. N. Korea).
    1.18 fixes a problem with joiner_i and joiner_t from 1993 on, where the separator between the
         variables was not being output.  It also fixes a problem with creating command files
         for Stata, where the command to set missing values was not correct if the user had
         selected the option of outputting data separated by commas.
    1.19 fixes the dyadic duration calculation so that it resets if a state drops out of the
         system.

    1.90 is the beginning of a major revision/revamping/expansion of the program.
         Added s options.  Added saved risk alliance information to large external file.
         Added options to risk score menu to compute sub-range of years, and to show a
         detail.  Procedure changed to write into existing file.  Several data file
         structures changed, including system capabilities.
         Added non-directed dyads, which required changing i/o procedures and
         adding more mid-data accessing options.

    1.91 begins to add generic data reading procedures, but the main purpose of this
         version is to get in the russett and oneal kant data.  The generic is thus not
         yet totally generic here.
         Fixed some errors in the specific set of dyads output.
    1.92 adds raw capability and alliance output.  Also added options for EU War trap computed
         with S scores, Risk with S Scores, EU and equilibria under war and reason methods.
         Initially, the output options involving s scores are disabled.  Adds additional
         (all remaining) COW MID variables to output.
    1.93 adds Maoz dyadic MID Dispute data.  Seems debugged and OK 6/13/2000.
         Also modified procedure for output of missing values to allow new variables
         to be added more easily internally.
    1.94 brings the risk combine procedure into the main eugene program, rather than
         being separate and stand alone.  It also makes the security alliance data
         into a tau and s version.  Added S with system leader, and updated the
         procedure for counting # of dyad years to be output, and for setting
         memory size in Stata.
    1.95 minor window display changes.  Also fixed a small bug in generation of risk
         scores with s that would cause the program to stop iterating prematurely,
         bug had to do with reading security/alliance data file.  Also fixed a problem
         with output of alliance score missing value, was outputting a -1, now back
         to a regular -9.
    1.96 adds closest colonial contiguity relationship.  Also adds commands to stata and
         spss output to drop the line of labels read, if user wants data for one
         of these programs but also outputs a line of labels.  Adds a button to var
         output page to set distance variable output options from there.
    1.97 adds generic user data.
    1.98 adds upload/download user data sets.  Also corrects a small problem
         with the output of dyads when user chooses to include joiners; problem
         was that dyads where A was an originator and B joined later were
         not included in output when A as joiner was selected.
    1.981 makes some small improvements to user data set and ftp display format and so on.
    1.99 changes the ftp transfer to use the name of a dataset that will have
         to be entered in a file on the website/server.  Clicking this name
         will download the data files associated with that dataset.  Also
         makes version a required variable in user data sets.
    1.991 changes the user file procedures by splitting up the initial window into several
         smaller windows.
    1.992 changes error_log procedure to try to create an error file in windows/temp if it
         can't do it in the location that is (implied) in the eugene.ini file.
    1.993 fixes a problem discovered by Kevin Sweeney that nondirected MID output using the
         Maoz dyadic mid data was wrong.  MIDs were being output for the last time a MID
         had occurred, rather than checking only the current year, in some cases.  So,
         US-Germany 1990 said there was a MID in 1990 (which actually occured in 1945).
         The problem was code in some of the non-directed procedures had (essentially) a
         dispute_year <= current_year rather than dispute_year=current_year.  This came
         from cutting and pasting procedures from COW MID data, where the <= line was
         folowed by a >= line.  Changed all the relevant <= to =.
    1.9931 adds window for Randy Siverson to do funky s computation.  Usually won't
         be available to users.
    1.994 adds Matt Rupert's help on the User variables stuff.
    1.995 was a temporary build version #.
    2.0  is the new version # for the first public release of the many changes dealing
         with user variables!  Beta version ready 12/21/2000.
    2.001 Minor cosmetic changes made Jan 2, 2001.
    2.002 Made a change in computing EU war and reason using to correctly use the
         EU_WarandReason S output file; it was set to use the tau file by mistake.
    2.003 Made 2 minor programming changes.  First, noticed that an initial value was
         not being initialized to 0 in the EUWar and Reason computation procedure.  The variable
         was PDenomB (PdenomA was initialized to 0 twice).  Normally the unitialized variable
         had a default value very close to 0, so setting it to 0 (the correct value, since
         the procedure summed onto this base) led to very few changes.  All new values correlated
         with old values at >0.99;  the EU changes resulting from the procedure change led to a
         change in EU equilibrium predictions in .24% of cases (so proportion of 0.0024).
         Also changed code for EU computation using S (before generating any EU with S scores)
         because code was not adjusting S to be 0.999 instead of 1.0 for EU calculations,
         which it needed to, just like Tau.
    2.010 Made some minor corrections in highlighting of buttons and user data setup procedures.
         Also updated the read_csv_string procedure to properly handle strings marked off
         by either single or double quotes, that contain commas, and double quote delimited
         strings that contain a single quote (e.g. Yemen People's Republic).  This was enough
         of a change to call it v2.01.
    2.011 Corrected changing variable names from gdp to gdp1 (e.g.).  Proc wasn't setting
         names appropriately for 8 character starting names.
    2.012 Hid the "update user data files" button in user var window since it wasn't
         working quite right.  Added creation of a stata dictionary file to allow
         data to be read into memory with appropriate variable size.
    2.013 Modified the data creation procedures to not use a dictionary, but to use infile
         without a dictionary but with variable sizes.
         Made necessary changes to use Maoz dyadid MID data v1.1, which has a couple of
         extra variables, and is missing a couple it used to have.  Did internal EUGene
         recoding to deal with not having Maoz's own reciprocation variable.
    2.014 Modified the labels on MID variables so that if nondirected it says "onset"
         and if directed it says "initiation".  Also modified MID sub-variable window to
         say if COW or Maoz MID data.
    2.020 Added save and load settings functions.  Also corrected a problem with Maoz dispute
         variables in non-directed setting;  In some cases, the "onset" variable would not
         be coded properly in some dyads, even though hostility levels were above 0 for at
         least one state, and the dyad was due to be included due to ongoing, joiners, etc.
         The source was a simple programming error (a call using ccode1, ccode1 was written
         instead of a call using ccode2, ccode1.
    2.021 Turned off user menu when output is happening.  Neglected to do this before.
    2.100 renumbered to 2.1, enough changes have been made to do this.  Also, made it so
         settings file would be in same (writeable) location as error log file.
    2.101
    2.102 fixes an erroneous error message popping up in the user data set definition section.
    2.11 removed the "capture" statement from the stata infile command.  It also forces
         output data files to have a 3 character extension, because this was causing
         problems in stata when users did not specify an extension and stata automatically
         looked for its own raw data extension.

    2.2 adds various options and add ons for APSA paper 2001.  This includes:
      1) 3 variants on the game, solved by backwards induction only;
      2) adds back phi adjustment option;
      3) allows computation of S wtd to be saved in a separate file, and choice to output
         s unweighted or s weighted;
      4) Reads an initial version of minimum distance data.
      5) allows output of alliance portfolio tables, weighted or unweighted.
      6) Fixes a bug whereby contiguity after 1993 was reported as "6" (non-contiguous) rather
      than "-9" for missing.  (Contig data ends in 1993).
      7) Allows output of COW country abbreviations.
    2.21 changes the order for generating EU scores so that CapA, CapB won't come up but
      AcqA and AcqB will instead (as they should by assumption)
    2.22 fixes a problem with outputting data from the game variants with nowar, noforce.
    2.25 fixes bugs and prepares for a new distribution.  Fixes a bug in sequenced alliance
         data which would report some (nonexistent) errors having to do with
         mismatched country codes.  File "seqnums.raw" was updated for this.
         Also corrected errors in the alliance portfolio creation that would mess it up
         if sequenced COW data selected.  Fixed a problem in entering years for a data set in
         the User Data Set upload/creation section, where it would report problem with each character
         a user entered for a date, rather than checking once after everything was entered.
         Fixed a bug in Maoz dyadic dispute data which would incorrectly code joining is
         subsequent years of a dispute (joining in the first year of a MID was OK).
         This version also hides from users the options entered in the APSA paper.  To restore these
         options for using adjusted Phi for equilibrium calculations, and the option for game variants,
         just set the visible property on the EUOptions form to be "true" (currently it's set to false).
         Added global and regional alliance portfolio tables.
         Finally, hid the option to get Gleditsch/Ward Minimum distance data as part of the main output,
         this is now an option only under user data sets.
    2.251 fixes a small bug discovered by Erik Gartzke where the check for whether all
         selections were made properly before recomputing EU scores wasn't processing distance
         selections correctly.  Now OK.
    2.3 changes implementation of ftp to use INDY ftp components rather than nmftp.
         Does some fixes (vartype) for Delphi 6 / XP variable recognition.  Changed
         variable and check box list recognition in the paged output unit so that there
         won't be an error when users switch from dyadic to monadic data sets and leave
         variables selected.
    2.4 adds a number of elements (programmed by Chris Baker), including bibliography/
         citation information, having the command file contain lines for program
         settings, new case subsets (within region and distance), and several system-level variables.
         It also allows additional
         data subset options including dyads with capitals within a specified number
			 of miles of one another, and states within a region (as defined by COW).
         Adds memory checking for user data sets.
         In the same release we will release a new version of the latitude/longitude data that
         corrects some mistaken placements.  This will affect distance between states.  However,
         we have not yet recomputed expected utility values with this new data.
         LIMDEP error message now displayed if user selects too many variables and LIMDEP as an
         output command file.  After initial tests, it also
         fixed the speed of processing dyads given a selection to output dyads within a
         distance range.
    2.41 starts to build in Scott's get disputes over time procedure in a new unit, MIDOverTime.
         Functioning/procedures in this new unit is designed to be independent of other units,
         except that it uses EUTypes1 and EUTypesMID.  Not completed 1-15-03.
    2.42 starts to build in war data set.  Not completed (shell only) 1-22-03.
    2.421 fixes a new problem in the ftp connect window where it was trying to change directory
         and was getting an error.
    2.423 fixes a problem in the user data set routine where it messed up setting unique names
         for the various versions of user data when they were exactly 7 characters long.
    2.424 has Chris's window/structure updates, along with a recheck for integration between
         code that Scott did while Chris was working.  Everything in Source looks correct/matched
         as of 5/14/2003.
    2.425 adds ICB.  Also may have fixed a problem with outputting all dyads where 1 is a major power,
         which may have been wrong.  Rewrote the code; definitely now OK.  Also updated the
         states.csv file to use the 2002 version of the data;  the 2002 version switches the order
         of the ccode and state abbrev in the data set, and so the program code changed slightly.
         Also added the complete version of my split-mids-over-time procedure.
    2.430 is a temporary stopping point where I sent the program back to Randy with his procedure
         enabled.
    2.431 starts to work with setting up MID 3.0 disputes.  Does an update to ftp connect to use
         new INDY ftp component, which changed structure slightly.  Also took out NMftp reference.
    2.432 imports Chris's procedures for user variable check boxes.  Does dyadic MID 3.0
         construction.  Add Number of MIDs and Crises as an option.  Modified dyadic alliance
         data read to use new COW released version (not custom) which has a header line
         and and version # in it, along with being .csv.
    2.433 imports Chris's final procedures pre-departure, including modifying country names
         in the country input box, fixing the bibliography.  Fixed a user variable
         memory computation error (was miscounting number of user variables before computing
         memory needed for stata command file).  Added box for cow/maoz role variable.  Removed
         SeqNum version of alliances as an option (commented out/fixed all refs to flat_cow_seq).
    2.44 Interim stop point to give to Signorino for corrected ATOP data access.
    2.45 adds some code to check for missing location data so program doesn't crash.
         Rearranges some code in generating system variables so it doesn't report errors for
         lagged countries that don't exist in year-1.  Changes Maoz/MID integration code so
         that all variables are transferred to the newly created dyadic structure, with no
         references back to base data.  This allows better portability of calling structures
         between MID 3.0 and maoz data.  Did (hopefully) final checks to be sure MID data was
         working OK, with production versions of the MID files.
    2.50 Candidate for release as v3.0.  Distribute to RAs for testing.
    2.51 fixed 3 minor problems noted by RAs.
    2.99 compiled with same code as v3.0 release, but with custom menu enabled
         so that COW can regenerated dyadic MID data as needed.  Still should build
         interface window for input/output files.
         Also needed to change code to read COW capabilities data b/c format changed.
         Added popup hint on user data files - should change to display full name/file
         above variables.  Do this later.
    3.0 10/6/2003 - release of major version.
    3.01 updates code for reading alliances to read the whole file rather than stopping
         at the first year > partition, because file may not be sorted properly.
         note - version not distributed right away; component file updated only.
    3.02 corrects a bug in nondirected dyad output where some pairs were not being found
        as valid for output because of a simple typo.  Also corrected the nondirected
        dyad MID onset labelling (it was saying initiation).  Finally, added a kludge fix
        (which should be removed later when MID 3.0 data is updated again) in
        EUTypesMID.TCountry_dispute_data_obj.init which fixes the originator coding of MID 4190 and 4197.
        Also removed display of Target vs. Initiator setting from directed dispute dyad output
        choice, since it is irrelevant.
    3.03 adds read of state pairs that have different #s but are really the same state.  Fixes
        an output problem for Maoz MID revision type, where the appropriate number of variable names
        was not being output for non-directed dyads for this one variable.  Modified range of
        state and dyad highest action level for Maoz dispute data set (where it ranged 1..23)
        to match the new MID 3.0 data range of 1.21.  Also, outside of the code, updated contiguity
        data to be correct through 2001.
    3.04 fixes a problem where the Maoz data was being improperly sorted and the 2nd, 3rd, etc.
        years of maoz mids were being improperly handled, resulting in duplicate records in the
        output of the directed-dispute-dyad unit.  This error was introduced in v3.0.
    3.05 fixes/finishes the Split MIDs over time custom procedure.
    3.06 added peacedays function to COW, Maoz, and ICB conflict data
         Added a window for Generating Dyadic MID data so that the user can select all files involved
            (both the output file and all [?] of the input files)
         Added ISO Numbers and Names
    3.07 fixed a bug with selecting sampling;  when sampling is selected, a dispute exclusion set
         must also be specified.  Program now sets an exclusion set.
         Fixed a bug in computing Pi in EU calculations when the denominator of the balance of forces
         expression was 0 (this situation was not being taken into account).
    3.08 Added Hypothetical Alliance Dataset output, previously only internal to EUGene
          Improved remove file procedure in RiskCombineIntegrated
    3.09 Added a Browser for looking at datasets from within EUGene
    3.1  Version number assigned to public release November 2005.
    3.11 Pulls in James' code from 12/2005 with new unit of analysis, variable labels.
         Noted that complete boolean evaluation (compiler option) cannot be on.  Many of the dispute
         routines use a construction that needs short-circuit evaluation.  It would be
         possible to rewrite them all, but somewhat time consuming for little gain.
    3.12 Reworked a bunch of windowing calls and variable constructions in order
         to trace a bug when switching to Delphi 2005.
         Verified January 2007 that all nondirected dyad code was correct; nondirectd dyad dispute data is OK.
         Fixed problems with creating dyadic MID data for states that enter/exit more than once.
         New dyadic MID # created.  Code updated on creating dyadic MIDs to look at MIDB for correct
         hostility levels, and end dates of dyadic MIDs.
    3.20 Number assigned for public release, Sept. 2007.
    3.201 Bug fix, Indy components were not properly integrated, so user data download
         didn't work.  Fixed 10/29/07.
    3.202 Bug fix, EUGene was not requiring complementary variable selection even when
         output required the complementary variable.  This was added.  6/08.
    3.203 Changed interface so entry of subrange years automatically clicks the subrange
         button so user doesn't have to.  Also improved an error message for a particular
         error if certain elements of user data sets are not specified correctly.  7/16/08.
    }

    {Notes:  When adding variables/options:
     Add types, buttons, etc.
     Initialize in paged output.
     Output in euinoutd; must add variable name(s), then output command.
    }

   max_debug_level = 10;

{$ifdef prod_run}
   initialization_file_name = 'eugProd.ini';
{$else}
   initialization_file_name = 'eugene.ini';
{$endif}


   initialized_value = -1;  {When initialized, all arrays, etc. are set to this.}
   missing_value = -9;      {missing data will get this value}
   missing_value_polity = -99;      {missing polity data will get this value}
                        {Note:  This value cannot be changed b/c it is hard coded in
                         the command-file creation unit for the missing value commands}
                        {also note:  in the output procedures, I assume that missing values
                         are type integer.}
   null_dispute_number = initialized_value;  {in theory, initialized_value and null_dispute_number
                                             can be different, and I think I changed all the
                                             references to initialized_value that should now be
                                             using null_dispute_number.  But initially they were
                                             both set to -1, so make them explicitly equal here.}

   stop_on_error = true;     {stop, dont stop on error will be used by output procedures.}
   dont_stop_on_error = false;

   top_nation_num = 1000;
   top_entity_num = 9999;
   max_countries = 250;  {will never have more than 250 countries.  As of 1996, it's < 200}
   max_region_countries = 100;  {will never have more than 100 countries in a region.
                                 This is used in risk generation procedure tau tables.  If
                                 this is enlarged, e.g. to allow calculation of a global
                                 risk structure, structures in risk calc must be changed.}
   max_gp_records = 3;   {maximum of 3 records in the input file of great power dates.}
   max_two_region_countries = 150;   {never more than 150 countries between any 2 regions}
   max_year = 2025;   {will never see a year value greater than this}
   min_year = 1800;   {will never see a year value less than this}
   min_ccode = initialized_value;
   max_ccode = top_nation_num;
   min_entity = initialized_value;
   max_entity = top_entity_num;
   min_ccode_index = initialized_value;
   max_ccode_index = max_countries;  {index of ccodes goes 1 to max_countries.}
   max_in_GA_pop = 100;


   cow_alliance_deck_value = 600;   {COW alliance data file has this deck # to be read in}

   top_dispute_num = 6000;   {Top num in MID 3.0 is 4900 or so}
   max_disputes = 4000;      {Overall disputes;  MID 3.0 has about 2330}
   max_country_disputes = 8000;   {MID 3.0 has about 5600}
   max_dyadic_disputes = 8000;     {from v2.1 MID I compute about 3500 with sideA as initiator; add some for MID 3.0}
   max_dyadic_dispute_years = 6500;  {This is used to read Maoz dyadic dispute data, where dyadic dispute-year is unit of analysis}
   max_participant_incident_records = 6000;      {for COW participant index records; v3.0 has 4800.}
   max_wars = 150;   {currently about 90}
   top_war_num = 500;    {id number of wars; currently (v3.0) top # is 211.}
   max_war_participants = 1000;  {no more than this many war participants;  currently <300 recs}
   max_dyadic_wars = 2000;  {Will create no more than this many dyadic war pairings.}
   max_icb_crisis_number = 1000;   {currently 400}

   longnamestringlength = 50;  {This is used for length of state and city names.}

   e = 2.718281828;

   unweighted = 0;
   weighted = 1;

   {values of alliances}
   not_active = 0;
   defense = 1;
   neutral = 2;
   entente = 3;
   no_alliance = 4;

   max_user_variables_per_data_set = 100;
   max_user_variables = 5000;       {total across all user data sets.}
   max_predefined_variables = 500;  {variables used by / read in EUGene in hard code}
   max_saved_user_dyads = 1000;     {to save user selected dyads}
   max_saved_user_datasets = 50;    {to save selected vars from user data sets}
   eugene_data_file_extension = 'edf';
   eugene_csv_file_extension = 'csv';
   eugene_doc_file_extension = 'rtf';
   eugeneFileListName = 'EUGeneDownloadFileList.csv';

   {maximum Iso number, just a high number to leave some space for future additions in the ISO data}
   max_iso_code = 1000;

type

   EUserInterrupt = class(Exception);

   dummy_type = missing_value..1;

   isocode_range = -9..max_iso_code;

   year_range = min_year..max_year;
   month_range = 0..12;
   day_range = 0..31;
   ccode_range = min_ccode..max_ccode;
   entity_range = min_entity..max_entity;
   ccode_index_range = min_ccode_index..max_ccode_index;   {This will be an index to ccodes, since there are many blanks}
                     {Given a ccode, ccode_index will return an index location}
   num_countries_range = 0..max_countries;
   contiguity_range = 1..11; {cow contig levels are 1=land, 2=12 miles water,
                     3=13-24, 4=25-150, 5=151-400 miles water.  6= mine=not contiguous.
                     7-11 are mine, and are colonial contiguity type paralleling 1, 2, 3, 4, 5,
                     so type 7 is colonial land contiguity.}
   region_type = (none, europe, middleeast, africa, asia, americas, globe);
   region_range = europe..globe;
   region_selection_type = set of region_type;
   full_region_range = none..globe;  {this variant is in calls for get so that I can call without a real region}
   dispute_id_range = 0..top_dispute_num;
   dyadic_dispute_id_range = 0..(top_dispute_num * 100);
   num_disputes_range = 0..max_disputes;
   country_dispute_range = null_dispute_number..max_country_disputes;
   dyadic_dispute_range = 0..max_dyadic_disputes;
   dyadic_dispute_year_range = 0..max_dyadic_dispute_years;
   dispute_data_version_for_read_type = (disputeformat21, disputeformat30);
   dyadic_war_range = 0..max_dyadic_wars;
   war_id_range = 0..top_war_num;
   war_participant_range = 0..max_war_participants;
   s_weighting_range = unweighted..weighted;
   IIG_outcomes = (SQ, AcqA, AcqB, CapA, CapB, Nego, WarA, WarB);

   error_action_type = (stop, continue);
   TIntegerArray = array of integer;

   {Objects and types for generic input data files, all kinds of input data.
    These types define all information that is necessary to read from an
    input file, wheter one type of input unit, or mixed.  The object here
    holds only types, though, the actual data types are defined in EUTypes3.}

   dataset_unit_of_analysis_type = (no_unit_dataset,
        country_year, directed_dyad_year, nondirected_dyad_year, annual_data);

   variable_unit_of_analysis_type = (no_unit_variable, identifierccode1, identifierccode2,
        identifieryear, identifierversion, monadic, dyadic_ordered, dyadic_unordered, annual);
        {note if these are changed or added to, need to change the read generic procs,
         or currently just the set russett/oneal var read.}

   one_var_data_type = record
         var_name : string;
         var_type : integer; {varinteger, varsingle, varolestr are the only processed types}
         var_unit : variable_unit_of_analysis_type;
         var_missing_value : integer;
         var_reversed_var : string;  {name of anonther var}
         {read_var : boolean;     dropped this - selection to keep just in .init, I still need record}
      end;
   data_set_info_record = record
         config_file_name : TFileName;
         file_name : TFileName;
         data_set_full_name : string;
         data_set_short_name : string;
         data_set_citation : string;
         data_set_unit : dataset_unit_of_analysis_type;
         data_set_first_year_possible : year_range;
         data_set_last_year_possible : year_range;
         label_line : boolean;
         num_vars : integer;
         num_cases : longint;
         var_info : array of one_var_data_type;  {holds names of ALL variables and other info in data record}
            {index runs 0..num_vars-1}
      end;   {record}

   user_data_set_list_type = array of data_set_info_record;
      {note: the length of the array will indicate how many user data sets there are.}
            {index runs 0..length-1}

   procedure Read_one_user_data_file_info(user_data_config_filename : TFileName;
             var one_user_data_set_info : data_set_info_record; var baddataset : boolean);
      {This reads one data set configuration file.  Separate procedure because it will
       also be called by the data set configurator program.}

   procedure write_one_user_data_file_info(user_data_config_filename : TFileName;
             one_user_data_set_info : data_set_info_record);
      {This writes one data set configuration file.  Separate procedure because it fits
       here logically, but will normally only be called by the data set configurator program.}

type
   Tuser_data_set_listing_obj = class(Tobject)
   public
      constructor init (data_path: string);
      procedure update (data_path: string);
      destructor destroy;
      function update_changes (data_path : string) : integer;
      function get_num_data_sets: integer;
      function get_config_file_name (data_set_num : integer): TFileName;
      function get_data_set_full_data_record (data_set_num : integer): data_set_info_record;
      function get_data_set_short_name (data_set_num : integer): string;
      function get_data_set_full_name (data_set_num : integer): string;
      function get_data_set_citation (data_set_num : integer): string;
      function get_data_set_file_name (data_set_num : integer): TFileName;
      function get_data_set_first_year_possible (data_set_num : integer): year_range;
      function get_data_set_last_year_possible (data_set_num : integer): year_range;
      function get_data_set_unit (data_set_num : integer): dataset_unit_of_analysis_type;
      function get_data_set_num_vars (data_set_num : integer): integer;
      function get_data_set_num_cases (data_set_num : integer): integer;
      function get_data_set_label_line (data_set_num : integer): boolean;

      function get_data_set_var_name (data_set_num, var_num : integer): string;
      procedure reset_data_set_var_name (data_set_num, var_num : integer; new_name : string);
      function get_data_set_var_type (data_set_num, var_num : integer): integer;
      function get_data_set_var_unit (data_set_num, var_num : integer): variable_unit_of_analysis_type;
      function get_data_set_var_missing_value (data_set_num, var_num : integer): integer;
      function get_data_set_var_reversed_var (data_set_num, var_num : integer): string;
      function get_data_set_var_number (data_set_num : integer; var_name : string): integer;
   private
      user_data_set_info : user_data_set_list_type;
   end;         {user data set listing object}

   configuration_type = record
         {help_files : array [] of TFileName;}

         eugene_directory : string;

         user_data_files_subdirectory_name : string;
         user_data_files_directory : string;
         User_data_set_info : Tuser_data_set_listing_obj;

         nation_list_file_name : TFileName;
         major_list_file_name : TFileName;
         same_state_diff_ccode_list_file_name : TFileName;
         cow_raw_cap_file_name : TFileName;
         cow_modified_cap_file_name : TFileName;
         cow_system_pct_file_name : TFileName;
         distance_file_name : TFileName;
         colonial_contiguity_file_name : TFileName;
         cow_alliance_file_name : TFileName;
         alliance_seq_file_name : TFileName;
         dyadic_alliance_file_name : TFileName;
         cow_mid_case_file_nameA : TFileName;
         cow_mid_actor_file_nameB : TFileName;
         cow_mid_name_file_nameC : TFileName;
         cow_mid_participant_incident_file_name : TFileName;
         cow_MID_Participant_Incident_Data_First_Year : year_range;
         cow_MID_Format_2_1_Data_Last_year : year_range;  {only compute internal cow mids to this year}
         cow_mid_data_format : dispute_data_version_for_read_type;
         maoz_dyadic_mid_file_name : TFileName;
         ICB_dyadic_file_name : TFileName;
         COW_war_file_name : TFileName;
         tau_file_name : TFileName;
         s_file_name : TFileName;
         polity3_file_name : TFileName;
         risk_Tau_file_name : TFileName;
         risk_S_unweighted_file_name : TFileName;
         risk_S_weighted_file_name : TFileName;
         risk_wtr_file_name : TFileName;
         security_alliance_Tau_file_name : TFileName;
         security_alliance_S_unweighted_file_name : TFileName;
         security_alliance_S_weighted_file_name : TFileName;
         EUWarReason_Tau_file_name : TFileName;
         EUWarReason_S_unweighted_file_name : TFileName;
         EUWarReason_S_weighted_file_name : TFileName;
         EUWarTrap_Tau_file_name : TFileName;
         EUWarTrap_S_unweighted_file_name : TFileName;
         mindist_file_name : TFileName;
         EUGene_ftp_site_name : string;
         werner_peace_years_file_name : TFileName;

         ISO_main_file : TFileName;
         ISO_lookup_file : TFileName;

         browser_lookup_file :TFileName;
         label_lookup_file : TFileName;

         error_file_name : TFileName;
         saved_settings_file_name : TFileName;
         bibliography_file_name: TFileName;

         HelpFiles_FileMenu_name : TFileName;
         HelpFiles_RecomputeMenu_name : TFileName;
         HelpFiles_OutputMenu_name : TFileName;
         HelpFiles_HelpMenu_name : TFileName;
         HelpFiles_RecalcBox_name : TFileName;
         HelpFiles_DistanceBox_name : TFileName;
         HelpFiles_TraceMenu_name : TFileName;
         HelpFiles_OnscreenWindows_name : TFileName;
         HelpFiles_DestinationPage_name : TFileName;
         HelpFiles_CasePage_name : TFileName;
         HelpFiles_SamplingPage_name : TFileName;
         HelpFiles_VariablePage_name : TFileName;
         HelpFiles_DisputePage_name : TFileName;
         HelpFiles_ExclusionPage_name : TFileName;
         HelpFiles_UserData_name : TFileName;

         first_cap_year : year_range;
         last_cap_year : year_range;
         first_nation_year : year_range;
         last_nation_year : year_range;
         first_alliance_year : year_range;
         last_alliance_year : year_range;
         first_alliance_seq_year : year_range;
         last_alliance_seq_year : year_range;
         first_eu_year_possible : year_range;
         last_eu_year_possible : year_range;
         first_risk_year : year_range;
         last_risk_year : year_range;
         first_polity3_year : year_range;
         last_polity3_year : year_range;
         first_wtr_risk_year : year_range;
         last_wtr_risk_year : year_range;
         first_MID_year : year_range;
         last_MID_year : year_range;
         first_ICB_year : year_range;
         last_ICB_year : year_range;
         first_contiguity_year : year_range;
         last_contiguity_year : year_range;
         first_mindist_year : year_range;
         last_mindist_year : year_range;

         first_any_year : year_range;
         last_any_year : year_range;
      end;


   {Types for storing user input and selected options}
   {users will choose dyads, years, output file, output format, output variables}
                    {options to be selected from windows interface menu}
   compute_type = (compute_tau, compute_syscap, compute_risk, compute_single_risk,
                   compute_EUWarTrap, compute_EUWarReasonProb, compute_s, compute_EUWarReasonProbSpecial,
                   compute_none);   {EUwarreason special outputs the component data as it computes.}
   output_type = (output_directed_dyads, output_nondirected_dyads, output_monads,
                  output_directed_dispute_initiation_dyads, output_nondirected_dispute_dyads, output_none);
   dyad_selection_type = (all_states, all_gp_gp, all_contiguous, all_gp_any, selected_set,
                          politically_relevant, within_distance, within_region, user_file_read, not_selected);
   mono_selection_type = (all_states_mono, all_gp_mono, within_region_mono, selected_set_mono, not_selected_mono);
   dispute_selection_type = (all_disputes, all_dispute_years, not_selected_disputes);
   alliance_in_data_type = (flat_dyadic{, flat_cow_sequence});
   risk_in_data_type = (risk_WTR, risk_EUGENE);
   stau_type = (use_s, use_tau);
   year_subset_type = (all, subrange, noyears);
   risk_calculation_info_type = record
         method : (use_ga, use_steepest, use_random_walk);
         num_in_pop : 1..max_in_ga_pop;
         mutate_probability : single;
         generations_to_be_stable : integer;
         num_from_previous_to_keep : 0..max_in_ga_pop;
         random_risk_iterations : integer;
         risk_search_tolerance : single;   {when searching for risk, keep looking until this level met}
      end;   {record}
   eu_calculation_info_type = record
         adjusted_phi : boolean;
         equilibrium_solution : (logical, induction);
         game_variant : (original, NoWar, NoForce, Node3Forward);
      end;  {record}
   tau_leader_calculation_info_type = (global, regional);

   output_variable_type = (ccodes, year, abbrevs,
                           tau, tauWithLeader, sunweighted, sweighted, sWithLeader,
                           AlliancePortfolioUnweighted, AlliancePortfolioWeighted,
                           EUWarTrapTau, EUWarTrapS,
                           EUWarReasonTau, EUWarReasonSUnweighted, EUWarReasonSweighted,
                           EQWarReasonTau, EQWarReasonSUnweighted, EQWarReasonSweighted,
                           distance, SystemCapabilities, RawCapabilities, alliance,
                           riskTau, riskS, riskdetailsTau, riskdetailsS, uncertaintyTau, uncertaintyS,
                           MarkPolRelevant, contig, ColonialContig,
                           PowerStatus, RelRegion, HomeRegion, DyadicTime,

                           COW_disputes, COWMIDStart, COWMIDEnd, COWMIDSideA, COWMIDRevisionist, COWMIDRevisiontype,
                           COWMIDFatalityState, COWMIDFatalityDispute, COWMIDHiActState, COWMIDHiActDispute,
                           COWMIDHostLevDispute, COWMIDOriginator,
                           COWMarkMIDJoiners, COWMIDNumber, COWMIDName, COWMIDSettlement, COWMIDOutcome,
                           COWMIDReciprocated, COWMIDNumStates, COWNumMIDs, COWpeaceyrs, COWRole, COWLinkStatus,

                           Maoz_dyadic_disputes, MaozMIDStart, MaozMIDEnd, MaozMIDSideA, MaozMIDRevisionist, MaozMIDRevisiontype,
                           MaozMIDFatalityState, MaozMIDFatalityDispute, MaozMIDHiActState, MaozMIDHiActDispute,
                           MaozMIDHostLevDispute, MaozMIDOriginator,
                           MaozMarkMIDJoiners, MaozMIDNumber, MaozMIDName, MaozMIDSettlement, MaozMIDOutcome,
                           MaozMIDReciprocated, MaozMIDNumStates,
                           MaozCOWWar, MaozDurindx, MaozDurDays, MaozReciprocatedDyadic,
                           MaozNumMIDs, Maozpeaceyrs, Maozpeacedys, MaozRole, MaozLinkStatus,

                           ICB_crises, ICBCrisisNumber, ICBCrisisName, ICBCrisisDyadNumber, ICBDurDays,
                           ICBDurYear, ICBDyadicStart, ICBDyadicEnd,ICBOneSided,
                           ICBStateStart,ICBStateEnd, ICBJoiner, ICBActorSequence,
                           ICBCOWMember,ICBGWMember, ICBIntraWar, ICBNumCrises, ICBpeaceyrs, ICBpeacedys,

                           ISO_code, ISO_abb2, ISO_abb3, ISO_short, ISO_full,

                           polity3,
                           democ, autoc, xrreg, xrcomp, xropen, mono, xconst,
                           parreg, parcomp, cent, dem, laggeddem, democratization, UserVariables,
                           systemchars, StatesInSystem, GPsInSystem, SysConcentration,
                           SysMovement, SysMovement5Yr, SysMoveGP, SysMoveGP5Yr,
                           COW_wars, COW_warnum, COWPeaceDays);

   SeparatorType = (space, comma, tab);
   output_location_type = (tofile, toscreen, toprinter, tonone);
   CommandFileType = (spss, stata, limdep, commandfilesonly);
   CommandFileSet = set of commandFileType;
   output_variable_set = set of output_variable_type;
   {Note: if change output format type, then must change output format for saving.}
   output_format_type = record
         output_set : boolean;
         header : boolean;  {T if write header, F if don't}
         printii : boolean;  {T if want dyads like "200 vs 200", F if don't}
         printAllOngoing : boolean;  {T if want dyads where a dispute was ongoing at start of year}
         printOngoingIfNewDisp : boolean;  {T if want dyads where a dispute was ongoing at start of year only when there is a new dispute}
         location : output_location_type;  {where does output go? }
         overwrite : boolean;            {if going to an existing file, overwrite it?  If, not append}
         output_file_name : TFileName;
         variables : output_variable_set;
         separator : SeparatorType;
         commandFiles : commandFileSet;
      end;           {record}
   output_format_type_for_saving = record
         output_set : boolean;
         header : boolean;
         printii : boolean;
         printAllOngoing : boolean;
         printOngoingIfNewDisp : boolean;
         location : output_location_type;
         overwrite : boolean;
         output_file_name : string[255];
         variables : output_variable_set;
         separator : SeparatorType;
         commandFiles : commandFileSet;
      end;           {record}
   Selected_country_list_type = record
         num_countries : integer;
         data : array[1..max_countries] of ccode_range;
      end;
   distance_type = (capitols, capitols_contiguity, capitols_contiguity_war_trap, minimum, nodiscount);
   modified_capability_type = (COW_only, no_energy, modified_capability);
      {for dispute info, if OnlyTrueInitiators then day 1 participants only get a dispute
       coded, vs. joiners;  SideA... codes that Side A (cow data) is initiator, alternative
       is that revisionist state is coded initiator.}
   dispute_info_type = record
          OnlyTrueInitiators, JoinersOnInitiatingSideAsInitiators, AllJoinersAsInitiators, SideAIsInitiator, MarkSubsequentAsInitiation, AlwaysIncludeTgtVsInitiator,
          IncludeTgtVsInitiatoriffNew, DropJoinerDirectedDyads, UseMostSeriousDispute, UseFirstDispute: boolean;
       end;
   conflict_data_set_type = (cds_COWMID, cds_MaozMID, cds_ICB, cds_COWWar, cds_none);
   sampling_info_type = record
          sampling, use_randseed : boolean;
          proportion_dispute_dyads, proportion_non_dispute_dyads : real;
          randseed : longint;
      end;

   user_dyad_ptr = ^user_dyad_rec;
   user_dyad_rec = record
          ccode1, ccode2 : ccode_range;
          styear, endyear : year_range;
          next_rec : user_dyad_ptr;
      end;   {record}

   specified_dyad_list_type = record
         file_name : TFileName;
         first_dyad : user_dyad_ptr;
      end;


   {These are the types to hold user selections about what user data sets to use.}
   {user_data_set_var_ptr = ^user_data_set_var;
   user_data_set_var = record
         var_num : integer;
         next_var : user_data_set_var_ptr;
      end;  }

   one_user_data_set_selections = record
         {data_set_configuration_number : integer;
         {shouldn't need: num_data_set_variables : 0..max_user_variables_per_data_set;  {this is the num of actual vars}
         data_set_variables : TIntegerArray;  {has the original number of each variable}
      end;   {record}

   user_data_sets_selection_type = array of one_user_data_set_selections;
      {There will be 1 per user data set.  Index refers to data set # in the master list.
       Some will contain 0 variables, indicating no user selections here.}

   one_user_data_set_selections_for_saving = record
         data_set_variables : array[0..max_user_variables_per_data_set] of integer;  {has the original number of each variable}
         full_name : string[100];
      end;   {record}
   user_data_sets_selection_type_for_saving = array[0..max_saved_user_datasets] of one_user_data_set_selections_for_saving;

   custom_procedure_type = (not_custom, EUWarReasonComponents);

   {NOTE: If change/add to user_selection_type, be sure to update user_selection_type_for_saving,
    and associated procedure.}
   user_selection_type = record
         dyads_selected : dyad_selection_type;
         user_specified_dyad_list : specified_dyad_list_type;
         monads_selected : mono_selection_type;
         disputes_selected : dispute_selection_type;
         selected_country_list : selected_country_list_type;
         first_year, last_year : year_range;
         years : year_subset_type;
         output_format : output_format_type;
         compute_this : compute_type;
         output_this : output_type;
         alliance_data_source : alliance_in_data_type;
         similarity_method : stau_type;  {use s or tau in calculating various subsequent values}
         s_weighting : s_weighting_range;
         risk_data_source : risk_in_data_type;
         risk_calculation_info : risk_calculation_info_type;
         eu_calculation_info : eu_calculation_info_type;
         tau_leader_calculation_info, s_leader_calculation_info : tau_leader_calculation_info_type;
         distance_method : distance_type;
         contiguity_level_required : contiguity_range;
         werner_peace_year_adjustment : boolean;
         maximum_distance : integer;
         selected_regions : region_selection_type;
         capability_modifications : modified_capability_type;
         dispute_info : dispute_info_type;
         sample_info : sampling_info_type;
         conflict_exclusion_selection : conflict_data_set_type;
         user_data_sets_selections : user_data_sets_selection_type;
            {original was  user_data_set_selection_ptr, new is user_data_sets_selection_type}
         complete : boolean;   {marks when user has made all necessary choices}
         custom_procedure : custom_procedure_type;
         temporary_output_file : text;  {holds a file name in case a procedure is run sending output to a file.}
      end;   {record}

   user_selection_type_for_saving = record
         dyads_selected : dyad_selection_type;
         monads_selected : mono_selection_type;
         disputes_selected : dispute_selection_type;
         selected_country_list : selected_country_list_type;
         first_year, last_year : year_range;
         years : year_subset_type;
         compute_this : compute_type;
         output_this : output_type;
         alliance_data_source : alliance_in_data_type;
         similarity_method : stau_type;  {use s or tau in calculating various subsequent values}
         s_weighting : s_weighting_range;
         risk_data_source : risk_in_data_type;
         risk_calculation_info : risk_calculation_info_type;
         eu_calculation_info : eu_calculation_info_type;
         tau_leader_calculation_info, s_leader_calculation_info : tau_leader_calculation_info_type;
         distance_method : distance_type;
         contiguity_level_required : contiguity_range;
         werner_peace_year_adjustment : boolean;
         maximum_distance : integer;
         selected_regions : region_selection_type;
         capability_modifications : modified_capability_type;
         dispute_info : dispute_info_type;
         sample_info : sampling_info_type;
         conflict_exclusion_selection : conflict_data_set_type;
         complete : boolean;   {marks when user has made all necessary choices}
         {in original type: output_format : output_format_type;}
         output_format : output_format_type_for_saving;
         {in original type: user_specified_dyad_list : specified_dyad_list_type;}
         specified_dyad_list_file_name : string[255];
         num_dyads : integer;
         user_specified_dyads : array[1..max_saved_user_dyads] of record
               ccode1, ccode2 : ccode_range;
               styear, endyear : year_range;
            end;     {record}

         {in original type: user_data_sets_selections : user_data_sets_selection_type;  }
         num_user_data_sets : integer;
         user_data_sets_selections : user_data_sets_selection_type_for_saving;

      end;   {record}

  {ISO data record}
  iso_record = record
    iso_code : isocode_range;
    iso_abbrev2 : string[2];
    iso_abbrev3 : string[3];
    iso_name_short : string[30];
    iso_name_full : string [60];
    cow_code : ccode_range;
  end;

  iso_lookup_table = record
    iso_code : isocode_range;
    cow_code : ccode_range;
  end;


  iso_array_type = array [isocode_range] of iso_record;

  iso_lookup_type = array [isocode_range] of iso_lookup_table;

  TISO_array_obj = class (Tobject)
      constructor init (ISO_main_file : TFileName; ISO_lookup_file : TFileName);
      destructor destroy; override;
      function initialized : boolean;
      function get_isocode (ccode: ccode_range) : integer;
      function get_ccode_from_isocode (isocode: isocode_range) : integer;
      function get_abbrev2_from_isocode (isocode: isocode_range) : string;
      function get_abbrev2 (ccode: ccode_range) : string;
      function get_abbrev3_from_isocode (isocode: isocode_range) : string;
      function get_abbrev3 (ccode: ccode_range) : string;
      function get_short_iso_name_from_isocode (isocode: isocode_range) : string;
      function get_short_iso_name (ccode: isocode_range) : string;
      function get_full_iso_name_from_isocode (isocode: isocode_range) : string;
      function get_full_iso_name (ccode: ccode_range) : string;
     private
      data : iso_array_type;
      lookup_table : iso_lookup_type;
      created : boolean;
   end;                 {ISO code object}


   function can_get_var_value_given_output_unit_and_input_data (const data_set_num, config_var_num : integer; user_selections : user_selection_type) : boolean;

type

   TVarFormCheckBoxRecord = record
        original_user_dataset_num : integer;
        original_user_var_num : integer;
        complement_user_var_num : integer;
        complement_check_box : integer;    {this will be the number of the complement box in the list.}
        ACheckBox : TCheckBox;
   end;
   TVarFormCheckBoxes = array of TVarFormCheckBoxRecord;

   saved_settings_type = record
      {configuration : configuration_type;}
      user_selections : user_selection_type_for_saving;
   end;
   saved_settings_file_type = file of saved_settings_type;


   index_array = array [ccode_range] of ccode_index_range;
   ccode_index_array = array[ccode_index_range] of ccode_range;
   Tccode_index_obj = class (Tobject)
      {This structure has index pointers for different countries.  Given a ccode, the
       index locates the record (in all the data structures) for that ccode.  I use this
       intermediate structure because there are ~1000 ccodes, but it is a sparse array
       and there are only 200 countries.  I save 4x space by using the index.}
      {takes as input the file name of the nation_list file.  This allows creation of
       the index numbers.}
         constructor init (afilename : TFileName);
         destructor destroy; override;
         function index (accode : ccode_range) : ccode_index_range;
         function initialized : boolean;
         function ccode (anindex : ccode_index_range) : ccode_range;
         function return_ccode_list : ccode_index_array;
         function identical_ccode_lists (another_ccode_index_array: ccode_index_array): boolean;
        private
         index_data : index_array;
         created : boolean;
         ccode_data : ccode_index_array;
      end;

    {object for nation ccodes, names, years of system and GP membership}
    {involved list is for marking involvement of states in regions}
   involved_list = array [region_type] of record
         start_year, end_year : year_range;
      end;
   involved_list_ptr = ^involved_list;
     {Tnationdat_obj is object for one nation/record}
   Tnationdat_obj = class (Tobject)
       constructor init (accode : ccode_range; aabbrev : string; afullname : string;
          astartyear1, aendyear1, astartyear2, aendyear2 : year_range;
          astartmonth1, aendmonth1, astartmonth2, aendmonth2 : month_range;
          astartday1, aendday1, astartday2, aendday2 : day_range;
          aregion : region_type; involvement : involved_list_ptr);
       procedure set_gp_info (which : integer; start_year, end_year : year_range;
                 start_month, end_month : month_range; start_day, end_day : day_range);
       destructor destroy; override;
       function get_ccode : ccode_range;
       function get_abbrev : string;
       function get_fullname : string;
       function get_home_region : region_type;
       function is_involved_in_region (aregion : region_type; ayear : year_range) : boolean;
       function get_startyear1 : year_range;
       function get_endyear1 : year_range;
       function get_startyear2 : year_range;
       function get_endyear2 : year_range;
       function get_gpstartyear (which : integer) : year_range;
       function get_gpendyear (which : integer) : year_range;
      private
       ccode : ccode_range;
       abbrev : string[3];
       fullname : string[longnamestringlength];
       region : region_type;
       involved_in_list : involved_list_ptr;          {has extra-regional involvement}
       startyear1, endyear1, startyear2, endyear2 : year_range;
       startmonth1, endmonth1, startmonth2, endmonth2 : month_range;
       startday1, endday1, startday2, endday2 : day_range;
       gpstartyear : array[1..max_gp_records] of year_range;
       gpendyear : array[1..max_gp_records] of year_range;
       gpstartmonth : array[1..max_gp_records] of month_range;
       gpendmonth : array[1..max_gp_records] of month_range;
       gpstartday : array[1..max_gp_records] of day_range;
       gpendday : array[1..max_gp_records] of day_range;
    end;

      {system matrix types store the systemic variable calculations (such as "number of states
       in the international system") for output.}
   system_year_matrix = array[year_range] of extended;


      {state matrix types are to set up a fast array for accessing whether or not a ccode-year
        is a state, since this is called so many times.}
   state_year_matrix = array[year_range] of boolean;
   state_year_matrix_ptr = ^state_year_matrix;
   state_matrix = array[ccode_range] of state_year_matrix_ptr;
   state_matrix_ptr = ^state_matrix;
   Nationarray_type = array[ccode_index_range] of Tnationdat_obj;
   PNationarray_type = ^Nationarray_type;
     {This is the array of nations}
   {same nation_diff_ccode rec is for when states change ccode but are actually same state.}
   same_nation_diff_ccode_rec = record
         state1, state2 : ccode_range;
         year : year_range;
      end;
   Tnation_array_obj = class (Tobject)
      constructor init (configuration: configuration_type);
         {reads in from raw data }
      destructor destroy; override;
      procedure calculate_n;
      function initialized : boolean;
      function is_a_state (accode : ccode_range; ayear : year_range) : boolean;
      function is_a_state_between (accode : ccode_range; year1, year2 : year_range) : boolean;
      function is_a_gp (accode : ccode_range; ayear : year_range) : boolean;
      function is_a_gp_between (accode : ccode_range; year1, year2 : year_range) : boolean;
      function is_involved_in_region (accode : ccode_range; aregion : region_type; ayear : year_range) : boolean;
      function have_info (accode : ccode_range) : boolean;
      function get_ccode (accode : ccode_range) : ccode_range;
      function get_abbrev (accode : ccode_range) : string;
      function get_fullname (accode : ccode_range) : string;
      function get_home_region (accode : ccode_range) : region_type;
      function get_startyear1 (accode : ccode_range) : year_range;
      function get_endyear1 (accode : ccode_range) : year_range;
      function get_startyear2 (accode : ccode_range) : year_range;
      function get_endyear2 (accode : ccode_range) : year_range;
      function have_2_start_years (accode : ccode_range) : boolean; 
      function get_gpstartyear (accode : ccode_range; which : integer) : year_range;
      function get_gpendyear (accode : ccode_range; which : integer) : year_range;
      function get_country_years : longint;
      function get_dyad_years : longint;
      function get_ndyads : longint;
      procedure show_info (accode : ccode_range);
      function get_ccode_from_fullname (fullname : string): ccode_range;
      function get_ccode_from_abbrev (abbrev : string): ccode_range;
      function get_dyadic_duration (ccode1, ccode2 : ccode_range; ayear : year_range) : integer;
      function country_date_overlap (ccode1, ccode2 : ccode_range): boolean;
      function different_states (ccode1, ccode2 : ccode_range) : boolean;
     private
      data : PNationarray_type;
      state_version : real;
      major_version : real;
      is_a_state_matrix : state_matrix_ptr;
      nations_with_different_ccode_data : array of same_nation_diff_ccode_rec;
      created : boolean;
      ncountry_years, ndyad_years, ndyads : longint;
   end;


   {object for COW raw capabilities data.}
   raw_cap_data_ptr = ^raw_cap_data;
   raw_cap_data = record
         tpop : longint;
         upop : longint;
         irst : longint;
         energy : longint;
         milper : longint;
         milex : longint;
      end;   {record}
   rawcap_array_ptr = ^rawcap_array;
   rawcap_array = array[ccode_index_range] of raw_cap_data_ptr;
   rawcap_year_array_ptr = ^rawcap_year_array;
   rawcap_year_array = array[year_range] of rawcap_array_ptr;
   Traw_capability_array_obj = class (Tobject)
      constructor init (a_file_name, modified_data_file_name : TFileName;
               first_proc_year, last_proc_year : year_range);
      destructor destroy; override;
      function get_first_partition_year : year_range;
      function get_last_partition_year : year_range;
      function initialized : boolean;
      function get_tpop (ccode: ccode_range; year: year_range) : longint; overload;
      function get_upop (ccode: ccode_range; year: year_range) : longint; overload;
      function get_irst (ccode: ccode_range; year: year_range) : longint; overload;
      function get_energy (ccode: ccode_range; year: year_range) : longint; overload;
      function get_milper (ccode: ccode_range; year: year_range) : longint; overload;
      function get_milex (ccode: ccode_range; year: year_range) : longint; overload;
      function get_tpop (ccode: ccode_range; year: year_range; error_check: boolean) : longint; overload;
      function get_upop (ccode: ccode_range; year: year_range; error_check: boolean) : longint; overload;
      function get_irst (ccode: ccode_range; year: year_range; error_check: boolean) : longint; overload;
      function get_energy (ccode: ccode_range; year: year_range; error_check: boolean) : longint; overload;
      function get_milper (ccode: ccode_range; year: year_range; error_check: boolean) : longint; overload;
      function get_milex (ccode: ccode_range; year: year_range; error_check: boolean) : longint; overload;
         {init reads in from raw data.}
     private
      data : rawcap_year_array_ptr;
      first_partition_year, last_partition_year : year_range;
      created : boolean;
      function all_in_range (ccode : ccode_range; year : year_range; error_check: boolean) : boolean;
   end;                 {capability object}



   {object for system capability (6 part cow index 0..1) }
   sys_cap_ccode_array_type = array[ccode_index_range] of single;
   syscap_array_ptr = ^syscap_array;
   syscap_array = sys_cap_ccode_array_type;
   syscap_year_array_ptr = ^syscap_year_array;
   syscap_year_array = array[year_range] of syscap_array_ptr;
   Tsys_capability_array_obj = class (Tobject)
      constructor init (a_file_name : TFileName; year1, year2 : year_range);
         {init reads in from intermediate, external file of system index.}
      destructor destroy; override;
      function get_first_partition_year : year_range;
      function get_last_partition_year : year_range;
      function initialized : boolean;
      function get_syscap (ccode : ccode_range; year : year_range) : single; overload;
      function get_syscap (ccode : ccode_range; year : year_range; error_check: boolean) : single;  overload;
     private
      data : syscap_year_array_ptr;
      first_partition_year, last_partition_year : year_range;
      created : boolean;
      function all_in_range (ccode : ccode_range; year : year_range; error_check: boolean) : boolean;
   end;                 {capability object}


   {Tau table is associated with tau structure, but used in a method
    for alliances, where table is generated.  }
   tau_table_type = array [1..4, 1..4] of 0..max_countries;
   tau_row_column_count_type = 0..4;

   similarity_table_type = array [1..4, 1..4] of real;  {will work for wtd s also}


   {object for alliances.}
   alliance_value_type = initialized_value..no_alliance;  {initialized_value=not a state/blank; 0=not active,
                       4=no alliance, active; 1, 2, 3 COW types Defense, Neutral, Entente}
       {note:  type 0 not used.  4=no alliance, and both are really system members.}
      {note:  some methods rely on alliance_year_array_type being a straight array,
       with no pointers.  If this is ever changed, then the copy procedures
       must be changed.  }
   alliance_year_array_type = array[ccode_index_range, ccode_index_range] of alliance_value_type;
       {a year is 62.5 K}
   alliance_year_ptr = ^alliance_year_array_type;
   Talliance_year_obj_ptr = ^Talliance_year_obj;
   Talliance_year_obj = class (Tobject)
      constructor init;   {does new, blanks data to 0}
      procedure allocate_full (for_year : year_range);
      procedure copy (another_alliance_year : Talliance_year_obj);
      destructor destroy; override;
      procedure set_alliance_value (ccode1, ccode2 : ccode_range; atype : alliance_value_type);
      function get_alliance_value (ccode1, ccode2: ccode_range) : alliance_value_type;
      procedure build_tau_table (ccode1, ccode2 : ccode_range; ayear : year_range;
             var result_table : tau_table_type; var N : num_countries_range;
             var nrows, ncolumns : tau_row_column_count_type;
             for_region : region_type);
    private
      data : alliance_year_ptr;
   end;

   Talliance_array_obj = class (Tobject)
      constructor init (sequence_alliance_file_name, seq_file_name, dyadic_alliance_file_name :
                       TFileName; year1, year2 : year_range; raw_data_source : alliance_in_data_type);
          {reads in from raw data, sequence # format (original cow format) }
      destructor destroy;  override;
      procedure make_alliances_symmetric;
      function initialized : boolean;
      function get_first_partition_year : year_range;
      function get_last_partition_year : year_range;
      function get_alliance_value (ccode1, ccode2 : ccode_range;  ayear : year_range) :
               alliance_value_type; overload;
      function get_alliance_value (ccode1, ccode2 : ccode_range;  ayear : year_range;
               error_check : boolean) : alliance_value_type; overload;
      function get_year_array_ptr (ayear : year_range) : Talliance_year_obj_ptr;
      procedure make_copy_of_alliance_year_array (year : year_range;
                                             var copy : Talliance_year_obj);
      procedure build_similarity_table (ccode1, ccode2 : ccode_range; ayear : year_range;
         for_region : region_type; weight : s_weighting_range; const sys_capability_data : Tsys_capability_array_obj;
         var result_table : similarity_table_type; var n : num_countries_range; var dmax : real);

     private
      year_array : array[min_year..max_year] of Talliance_year_obj;
      first_partition_year, last_partition_year : year_range;
      created : boolean;
      procedure init_from_seq (alliance_file_name, seq_file_name : TFileName;
                                 year1, year2 : year_range);
      procedure init_from_update (dyadic_alliance_file_name : TFileName;
                                    year1, year2 : year_range);
      function all_in_range (ccode1, ccode2 : ccode_range;  ayear : year_range; error_check : boolean) : boolean;

   end;



   {object for tau-bs for dyads}
   tau_array_ptr2 = ^tau_array_2;
   tau_record_type = record
         global : single;
         regional : array[1..2] of single;   {has tau based on global, regional alliance patterns.}
             {regional1 is tau for 1's region, regional[2] is tau for 2's region}
      end;
   tau_array_2 = array[ccode_index_range] of tau_record_type;
   tau_year_ptr = ^tau_year_array_type;
   tau_year_array_type = array[ccode_index_range] of tau_array_ptr2;
   Ttau_array_obj = class (Tobject)
      constructor init (a_file_name : TFileName; year1, year2 : year_range);
         {init reads in from intermediate, external file.}
      destructor destroy ; override;
      procedure modifyTauForWarReason (start_year, end_year : year_range);
      function initialized : boolean;
      function get_first_partition_year : year_range;
      function get_last_partition_year : year_range;
      function get_tau_value_global (ccode1, ccode2 : ccode_range;  ayear : year_range) : single; overload;
   	 function get_tau_value_global (ccode1, ccode2 : ccode_range;  ayear : year_range; error_check : boolean) : single; overload;
      function get_tau_value_regional (ccode1, ccode2 : ccode_range;  ayear : year_range;
                                       which_region : region_type) : single; overload;
      function get_tau_value_regional (ccode1, ccode2 : ccode_range;  ayear : year_range;
            which_region : region_type; error_check : boolean) : single; overload;

     private
      year_array : array[min_year..max_year] of tau_year_ptr;
      first_partition_year, last_partition_year : year_range;
      created : boolean;
      function all_in_range (ccode1, ccode2 : ccode_range;  ayear : year_range;
               					which_region : region_type;  error_check: boolean) : boolean;
   end;                                    {tau object}


   {need an object to hold one year of taus, for doing the risk/hypothetical U calculations}
   Ttau_year_obj = class (Tobject)  {this is one year of tau data, for extracting one piece.}
        constructor init (ayear : year_range; const start_taus: Ttau_array_obj);
        destructor destroy; override;
        function get_tau_value_global (ccode1, ccode2 : ccode_range) : single;
        function get_tau_value_regional (ccode1, ccode2 : ccode_range; which_region : region_type) : single;
        procedure set_tau_value_global (ccode1, ccode2 : ccode_range;  value : single);
        procedure set_tau_value_regional (ccode1, ccode2 : ccode_range; value : single;
                                         which_state : ccode_range);
      private
        data : tau_year_array_type;
      end;    {Ttau_year_obj}








   

   {object for s scores for dyads}
   s_array_ptr2 = ^s_array_2;
   s_record_type = record
         global : array[s_weighting_range] of single;
         regional : array[s_weighting_range] of array[1..2] of single;   {has s based on global, regional alliance patterns.}
             {regional1 is s for 1's region, regional[2] is s for 2's region}
      end;
   s_array_2 = array[ccode_index_range] of s_record_type;
   s_year_ptr = ^s_year_array_type;
   s_year_array_type = array[ccode_index_range] of s_array_ptr2;
   Ts_array_obj = class (Tobject)
      constructor init (a_file_name : TFileName; year1, year2 : year_range);
         {init reads in from intermediate, external file.}
      destructor destroy ; override;
      procedure modifysForWarReason (start_year, end_year : year_range);
      function initialized : boolean;
      function get_first_partition_year : year_range;
      function get_last_partition_year : year_range;
      function get_s_value_global (ccode1, ccode2 : ccode_range;  ayear : year_range; weight : s_weighting_range) : single; overload;
      function get_s_value_global (ccode1, ccode2 : ccode_range;  ayear : year_range; weight : s_weighting_range; error_check: boolean) : single; overload;
      function get_s_value_regional (ccode1, ccode2 : ccode_range;  ayear : year_range;
           which_region : region_type; weight : s_weighting_range) : single; overload;
      function get_s_value_regional (ccode1, ccode2 : ccode_range;  ayear : year_range;
           which_region : region_type; weight : s_weighting_range; error_check: boolean) : single; overload;
     private
      year_array : array[min_year..max_year] of s_year_ptr;
      first_partition_year, last_partition_year : year_range;
      created : boolean;
      function all_in_range (ccode1, ccode2 : ccode_range;  ayear : year_range; error_check : boolean) : boolean;
   end;                                    {s object}


   {need an object to hold one year of s, for doing the risk/hypothetical U calculations}
   Ts_year_obj = class (Tobject)  {this is one year of s data, for extracting one piece.}
        constructor init (ayear : year_range; const start_s: Ts_array_obj);
        destructor destroy; override;
        function get_s_value_global (ccode1, ccode2 : ccode_range; weight : s_weighting_range) : single;
        function get_s_value_regional (ccode1, ccode2 : ccode_range; which_region : region_type; weight : s_weighting_range) : single;
        procedure set_s_value_global (ccode1, ccode2 : ccode_range; weight : s_weighting_range; value : single);
        procedure set_s_value_regional (ccode1, ccode2 : ccode_range; weight : s_weighting_range; value : single;
                                         which_state : ccode_range);
      private
        data : s_year_array_type;
      end;    {Ts_year_obj}



      {object for EUs using War Trap methods for dyads}
   EUWarTrap_record = record
         EUWartrap : single;
      end;
   EUWarTrap_ccode_array_2 = array[ccode_index_range] of EUWarTrap_record;
   EUWarTrap_ccode_array_ptr2 = ^EUWarTrap_ccode_array_2;
   EUWarTrap_ccode_array_type = array[ccode_index_range] of EUWarTrap_ccode_array_ptr2;
   EUWarTrap_ccode_ptr = ^EUWarTrap_ccode_array_type;
   TEUWarTrap_array_obj = class (Tobject)
      constructor init (a_file_name : TFileName; year1, year2 : year_range);
         {init reads in from intermediate, external file.}
      destructor destroy; override;
      function get_first_partition_year : year_range;
      function get_last_partition_year : year_range;
      function initialized : boolean;
      function get_EUWarTrap (ccode1, ccode2 : ccode_range; ayear : year_range) : single; overload;
      function get_EUWarTrap (ccode1, ccode2 : ccode_range; ayear : year_range; error_check : boolean) : single; overload;
     private
      year_array : array[min_year..max_year] of EUWarTrap_ccode_ptr;
      first_partition_year, last_partition_year : year_range;
      created : boolean;
      function all_in_range (ccode1, ccode2 : ccode_range;  ayear : year_range; error_check : boolean) : boolean;
   end;    {EUWarTrap object}



	{ -------------------------  }

   {No methods to complete for these}

    {debug array will hold markers for whether to print/output various tracing/debugging info during execution.}
   debug_array_type = array[0..max_debug_level] of boolean;   {for debugging}

   dyadic_alliance_record_type = packed record
      allynum, ccode1, ccode2, year : integer;
      atype : defense..no_alliance;
   end;

   dyadic_alliance_file_type = file of dyadic_alliance_record_type;


    {Intermediate output files are binary to save space}
    {unused type definitions}
    {tau_file_record_type = record
          ccode1, ccode2 : ccode_range;
          year_array: array[year_range] of tau_record;
       end;    {record}
    {tau_file_type = file of tau_file_record_type;}
    {Changed tau file type to be just single to allow easier search later}
    
    tau_file_type = file of single;    {binary only - recs like year cc1 cc2 taurec cc2 taurec year ...}

    s_file_type = file of single;    {binary only - recs like year cc1 cc2 s cc2 s}


    {note: sys_cap_ccode_array_type = array[ccode_index_range] of single; }
    sys_cap_file_record_type = packed record
          year : year_range;
          ccode_from_index_list : ccode_index_array;  {to be able to verify ccode_indexes are the same}
          ccode_array : sys_cap_ccode_array_type;
       end;
    sys_cap_file_type = file of sys_cap_file_record_type;

    EUWarTrap_file_record_type = packed record
          ccode1, ccode2 : ccode_range;
          year : year_range;
          EUWarTrap_rec : EUWarTrap_record;
       end;
    EUWarTrap_file_type = file of EUWarTrap_file_record_type;


var
   {Main program configuration, interface, and connections to outside world}
   UserInterrupt : boolean;
   configuration : configuration_type;
   error_log : text;
   debug : debug_array_type;

   {User input variable structure}
   user_selections : user_selection_type;
   VarFormCheckBoxes: TVarFormCheckBoxes;

   {Main data structures.  ccodeindex, nation_list are read globally.  Others
    are needed only in various subprocedures, so are not declared here.}
   ccode_index : Tccode_index_obj;
   nation_list : Tnation_array_obj;
   {distance_data : Tdistance_array_obj;   declared inside procs}
   {contiguity_data : Tcontiguity_array_obj; declared in eutypes 2}
   {alliance_data : Talliance_array_obj;    This is only needed inside the preprocessor}
   {tau_data : Ttau_array_obj;}
   {raw_capability_data : Traw_capability_array_obj; }
   {sys_capability_data : Tsys_capability_array_obj;}
   {risk_data : Trisk_attitude_array_obj;}
   {User_data_sets}

    {relevant_region is used by alliance object, but is needed outside it also.}
   function relevant_region (ccode1, ccode2 : ccode_range; ayear : year_range) : region_type;
   procedure switch_ccode (var num1, num2 : ccode_range);
   procedure switch_year (var num1, num2 : year_range);

   function Tccode_index_obj_mem_overhead : longint;
   function Tccode_index_obj_mem_per_year : longint;
   Function Tnation_array_obj_mem_overhead : longint;
   Function Tnation_array_obj_mem_per_year : longint;
   Function Traw_capability_array_obj_mem_overhead : longint;
   Function Traw_capability_array_obj_mem_per_year : longint;
   Function TSys_capability_array_obj_mem_overhead : longint;
   Function TSys_capability_array_obj_mem_per_year : longint;
   function Tdistance_array_mem_overhead : longint;
   function Tcontiguity_array_mem_overhead : longint;
   Function Talliance_array_obj_mem_overhead : longint;
   Function Talliance_array_obj_mem_per_year : longint;
   Function TTau_array_obj_mem_overhead : longint;
   Function TTau_array_obj_mem_per_year : longint;
   Function TS_array_obj_mem_overhead : longint;
   Function TS_array_obj_mem_per_year : longint;
   Function TEUWarTrap_array_obj_mem_overhead : longint;
   Function TEUWarTrap_array_obj_mem_per_year : longint;
   Function Tuser_data_mem_overhead : longint;
   Function Tuser_data_mem_per_year : longint;

   function mem_for_procedure : longint;
   function maxavail : longint;
   function memavail : longint;
   function totalphysicalmem : longint;
   function availphysicalmem : longint;
   function totalvirtualmem : longint;
   function availvirtualmem : longint;

   procedure specified_dyad_list_set (var user_selections: user_selection_type);
   procedure specified_dyad_list_delete (var user_selections: user_selection_type);
   function want_user_dyad_year (user_selections : user_selection_type;
                             ccode1, ccode2 : ccode_range; ayear : year_range) : boolean;

{ ------------------------------------------------------------ }

implementation

uses euinoutD, eutypes2, errbx;
   {euinout used for "error" procedure; eutypes3 used for user data information}

{These do a substitute calculation for memory use on a windows95/windows NT
 32 bit system.  In these systems, memory space is "not an issue" for practical
 purposes according to Delphi help, because these OSs use disk paging to simulate
 additional memory on demand.  But, I still don't want to use disk paging more than
 necessary, so I want to try to stay within physical memory as much as possible.
 At first I thought there was not a Windows API call to get
 exactly what I want.  So I used the total physical
 memory available;  Some of this will be in use by other programs (operating system,
 kernel, etc.) so I took a 50% portion of it as the total memory that can be used.
 The main program itself actually further reduces this, so I should be safe.
 Now, though, it seems I can use the total physical memory available, although I guess
 this can be changed as stuff is swapped in and out of physical memory.  Since most users
 (I am assuming) will run EUGene as the main thing they are running, I will use
 75% of the available physical memory as the memory bounds for the program.
 Also, note that in the procedures, memory used is calculated as change from
 memavail at start to memavail at end.  Because of paging, this might not be a true
 reflection of the memory a structure uses.
 }

function maxavail : longint;
   var mem_info : Tmemorystatus;
   begin
      GlobalMemoryStatus(mem_info);
      maxavail := mem_info.dwavailvirtual;
   end;
function memavail : longint;
   var mem_info : Tmemorystatus;
   begin
      GlobalMemoryStatus(mem_info);
      memavail := mem_info.dwAvailPhys;
   end;
function totalphysicalmem : longint;
   var mem_info : Tmemorystatus;
   begin
      GlobalMemoryStatus(mem_info);
      totalphysicalmem := mem_info.dwTotalPhys;
   end;
function availphysicalmem : longint;
   var mem_info : Tmemorystatus;
   begin
      GlobalMemoryStatus(mem_info);
      availphysicalmem := mem_info.dwAvailPhys;
   end;
function totalvirtualmem : longint;
   var mem_info : Tmemorystatus;
   begin
      GlobalMemoryStatus(mem_info);
      totalvirtualmem := mem_info.dwtotalvirtual;
   end;
function availvirtualmem : longint;
   var mem_info : Tmemorystatus;
   begin
      GlobalMemoryStatus(mem_info);
      availvirtualmem := mem_info.dwavailvirtual;
   end;

function mem_for_procedure : longint;
   var mem_info : Tmemorystatus;
   begin
      GlobalMemoryStatus(mem_info);
      {showmessage ('Total phys memory is '+FormatFloat('#,###" KB"', mem_info.dwTotalPhys / 1024));
      showmessage ('Available phys memory is '+FormatFloat('#,###" KB"', mem_info.dwTotalPhys / 1024));}
      {if mem_info.dwAvailPhys < 1000000 then}
      mem_for_procedure := round(mem_info.dwtotalphys * 0.5);
      {mem_for_procedure := round(mem_info.dwAvailPhys * 0.75);}
   end;



   function relevant_region (ccode1, ccode2 : ccode_range; ayear : year_range) : region_type;
      {this gives the relevant region that states must be involved in if they are
       to be included in the expected utility calculation for cc1 vs. cc2.  This
       includes whether they are looked at to build tau tables, and included as
       possibly assistint ccode1 or ccode2 in the multilateral war EU calculation.
       Also gives the region for the relevant risk score, since it is by region.
       This is from BdM War Trap pp 97-98.  rules are as follows:
         -- If init involved in target's region, then target region used.
         -- But, if init not in target region, then init region used.}
      var region1, region2 : region_type;

      function all_in_range (ccode1, ccode2 : ccode_range; ayear : year_range) : boolean;
         begin
            all_in_range := true;
            if (ccode1=0) or (ccode2=0) then
               all_in_range := false
            else
               if not (nation_list.initialized) then
                  all_in_range := false
            else
               if not (nation_list.is_a_state(ccode1, ayear) and nation_list.is_a_state(ccode2, ayear)) then
                  all_in_range := false;
         end;

      begin  {main func rel region}
         relevant_region := none;
		    if all_in_range (ccode1, ccode2, ayear) then
            begin
               region1 := nation_list.get_home_region(ccode1);
               region2 := nation_list.get_home_region(ccode2);
               if (region1 = region2)
                  then relevant_region := region1
               else  {not same home region;  if A involved in B's region, then want B's region info}
                  if nation_list.is_involved_in_region (ccode1, region2, ayear)
                     then relevant_region := region2
               else  {A not involved in B's region, So, use A's region.  }
                  relevant_region := region1;
            end;
      end;

  { ---------------------------------------------------------------  }

   procedure switch_ccode (var num1, num2 : ccode_range);
     var temp : ccode_range;
     begin
        temp := num2;
        num2 := num1;
        num1 := temp;
     end;

      {  -----------------------   }

   procedure switch_year (var num1, num2 : year_range);
     var temp : year_range;
     begin
        temp := num2;
        num2 := num1;
        num1 := temp;
     end;

   { ----------------------------------------  }

   function  Tccode_index_obj_mem_overhead : longint;
      begin
         Tccode_index_obj_mem_overhead := sizeof (Tccode_index_obj) +
           (max_ccode-min_ccode+1)*sizeof(ccode_index_range) +
           sizeof(boolean);
      end;
   function  Tccode_index_obj_mem_per_year : longint;
         begin
            Tccode_index_obj_mem_per_year := 0;
         end;
   Function Tnation_array_obj_mem_overhead : longint;
      begin
           {main structure, + is_a_state_matrix, + individual records}
         Tnation_array_obj_mem_overhead := sizeof (Tnation_array_obj) +
            sizeof(PNationarray_type) + sizeof (state_matrix_ptr) +
            sizeof(boolean) +
            sizeof(state_matrix) +
            ( (max_ccode - min_ccode + 1) * sizeof(state_year_matrix) ) +
            sizeof(NationArray_type) +
            ( (max_ccode - min_ccode + 1) *
              (sizeof(Tnationdat_obj) + sizeof (involved_list) +
               sizeof(ccode_range) + sizeof(region_type) + 8*sizeof(year_range)
               + (3 + longnamestringlength) ) );
      end;
   Function Tnation_array_obj_mem_per_year : longint;
      begin
         Tnation_array_obj_mem_per_year := 0;
      end;
   Function Traw_capability_array_obj_mem_overhead : longint;
      begin
         Traw_capability_array_obj_mem_overhead:= sizeof(Traw_capability_array_obj) +
                 + sizeof(rawcap_year_array_ptr) + sizeof( rawcap_year_array)
                 + 2*sizeof(year_range) + sizeof(boolean);
      end;
   Function Traw_capability_array_obj_mem_per_year : longint;
      begin
          Traw_capability_array_obj_mem_per_year := sizeof(rawcap_array) +
             (max_ccode_index-min_ccode_index+1) * (sizeof(raw_cap_data)+
                                                    sizeof(raw_cap_data_ptr));
      end;
   Function TSys_capability_array_obj_mem_overhead : longint;
      begin
          TSys_capability_array_obj_mem_overhead := sizeof (Tsys_capability_array_obj) +
             sizeof (syscap_year_array_ptr) + sizeof (syscap_year_array) +
             2*sizeof(year_range) + sizeof(boolean);
      end;
   Function TSys_capability_array_obj_mem_per_year : longint;
      begin
	       TSys_capability_array_obj_mem_per_year := sizeof(syscap_array);
      end;
   Function Tdistance_array_mem_overhead : longint;
   begin
      Tdistance_array_mem_overhead := sizeof(Tdistance_array_obj) +
        sizeof(dist_array_ptr) +
        sizeof(boolean) + sizeof(dist_array) +
        (max_ccode_index-min_ccode_index+1) *
           ( sizeof(dist2_array_type) +
            (max_ccode_index-min_ccode_index+1) * (sizeof(distance_rec)) ) +
        sizeof (location_array) +
        (max_ccode_index-min_ccode_index+1) * round(1.3*sizeof(location_rec));
           {assume 30% of countries have multiple city/location records.
            This is an overestimate.}
   end;
   Function Tcontiguity_array_mem_overhead : longint;
   begin
      {contiguity tougher to figure mem needed, b/c won't create all dyad
       records.  Assume that each state will be contiguous to 10 others for
       purposes of this calculation.}
      Tcontiguity_array_mem_overhead := sizeof(Tcontiguity_array_obj) +
        sizeof(contig_array_ptr) +
        sizeof(boolean) + sizeof(contig_array) +
        (max_ccode_index-min_ccode_index+1) *
           ( sizeof(contig2_array_type) +
            (10) * (sizeof(contigu_rec)) );
   end;
   Function Talliance_array_obj_mem_overhead : longint;
      begin
          Talliance_array_obj_mem_overhead := sizeof( Talliance_array_obj ) +
             (max_year-min_year+1)*sizeof(Talliance_year_obj)+ 2*sizeof(year_range) +
             sizeof(boolean);
      end;
   Function Talliance_array_obj_mem_per_year : longint;
      begin
         Talliance_array_obj_mem_per_year := sizeof(alliance_year_ptr) +
              sizeof (alliance_year_array_type);
      end;
   Function TTau_array_obj_mem_overhead : longint;
      begin
          TTau_array_obj_mem_overhead := sizeof (Ttau_array_obj)
          + (max_year-min_year+1)*sizeof(tau_year_ptr) + 2*sizeof(year_range)
          + sizeof(boolean);
      end;
   Function TTau_array_obj_mem_per_year : longint;
      begin
            TTau_array_obj_mem_per_year := sizeof(tau_year_array_type) +
               (max_ccode_index-min_ccode_index+1) * (sizeof(tau_array_2)) ;
      end;
   Function Ts_array_obj_mem_overhead : longint;
      begin
          Ts_array_obj_mem_overhead := sizeof (Ts_array_obj)
          + (max_year-min_year+1)*sizeof(s_year_ptr) + 2*sizeof(year_range)
          + sizeof(boolean);
      end;
   Function Ts_array_obj_mem_per_year : longint;
      begin
            Ts_array_obj_mem_per_year := sizeof(s_year_array_type) +
               (max_ccode_index-min_ccode_index+1) * (sizeof(s_array_2)) ;
      end;
   Function TEUWarTrap_array_obj_mem_overhead : longint;
       begin
          TEUWarTrap_array_obj_mem_overhead := sizeof(TEUWarTrap_array_obj) +
             (max_year-min_year+1)*sizeof(EUWarTrap_ccode_ptr) + 2*sizeof(year_range) +
             sizeof(boolean);
      end;
   Function TEUWarTrap_array_obj_mem_per_year : longint;
      begin
         TEUWarTrap_array_obj_mem_per_year := sizeof(EUWarTrap_ccode_array_type) +
            (max_ccode_index-min_ccode_index+1) * sizeof(EUWarTrap_ccode_array_2);
      end;
   Function Tuser_data_mem_overhead : longint;
      begin
         Tuser_data_mem_overhead := sizeof(Tuser_data_set_listing_obj);
            { + length(user_data_sets_vars) * (max_year-min_year+1)*sizeof(user_data_var_ptr)}
      end;

   Function Tuser_data_mem_per_year : longint;
   var i,j,x: integer;
      begin
         x := 0;
         for i := 0 to (length(user_selections.user_data_sets_selections) - 1) do
         begin
            if length(user_selections.user_data_sets_selections[i].data_set_variables) > 0 then
            begin
               for j := 0 to (length(user_selections.user_data_sets_selections[i].data_set_variables) - 1) do
               begin
                  case configuration.User_data_set_info.get_data_set_var_type(i,user_selections.user_data_sets_selections[i].data_set_variables[j]) of
                     varinteger : x := x + sizeof (integer);
                     varsingle  : x := x + sizeof (single);
                     varolestr  : x := x + sizeof (string);
                  end;
               end;
            end;
         end;
         Tuser_data_mem_per_year := x;
      end;

   { -------------------------------------------------- }

   {this first function is never called from outside the nation array initialization,
    so it's defined privately to this unit.}
   function sing_small_region (ccode : ccode_range) : region_type;
      begin
         if ccode < 200 then sing_small_region := americas
         else if ccode < 400 then sing_small_region := europe
         else if ccode < 600 then sing_small_region := africa
         else if ccode < 700 then sing_small_region := middleeast
         else sing_small_region := asia;
      end;

   {  -----------------------   methods for Tccode_index }

   constructor Tccode_index_obj.init (afilename : TFileName);
       {reads nation dat file and sets index numbers.}
      var
         infile:text;
         achar, blank:char;
         ccode : ccode_range;
         abbrev : string[3];
         fullname : string[20];
         index_value, namecount : integer;
      begin
         try
            try
               trace.enter ('Initializing ccode_index');
               if MaxAvail < Tccode_index_obj_mem_overhead then
                  EUGeneError ('Not enough memory for main ccode index.  Fatal error.  Halting.',
                         10, stop, error_log);
               for index_value := min_ccode to max_ccode do
                  begin
                     index_data[index_value] := initialized_value;
                  end;
               for index_value := min_ccode_index to max_ccode_index do
                  begin
                     ccode_data[index_value] := initialized_value;
                  end;

               assignFile (infile, afilename);
               reset (infile);
               index_value := 0;

               {In the cow format, the first line is a line of variable labels.  Skip it.}
               readln(infile);

               {Now iterate through the input nation file}
               while not eof (infile) do
               begin
                  {read just the ccode, abbrev to make sure file is OK and get ccode}
                  {variable order is abbrev, ccode, name, styear, stmonth, stday, endyr, endmo, enddy, version}
                  ccode := read_csv_int (infile);
                  abbrev := read_csv_string(infile);

                  if not eof (infile) then readln (infile);
                  if index_data[ccode] <> initialized_value then
                     begin
                        {have seen this country and given it an index already, so do nothing}
                     end
                     else
                     begin
                        {First time seeing this ccode, so set the index array for this ccode}
                        inc(index_value);
                        index_data[ccode] := index_value;
                        ccode_data[index_value] := ccode;
                     end;

               end;
               created := true;
            finally
               CloseFile (infile);
               trace.exit('Completed initializing ccode_index');
            end;
         except
            on EUserInterrupt do raise;
            on EInOutError do
              begin
                 FileErrorBox.maindo ('Error opening file "'+afilename+ '"',
                                      'File could not be opened for input.',
                                      'File may be in use by another program, or may be missing.');
                 FileErrorBox.showmodal;
                 raise;
              end;
         end;
      end;

   destructor Tccode_index_obj.destroy;
      begin
         try
            if self <> nil then inherited destroy;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;

   function Tccode_index_obj.index (accode : ccode_range) : ccode_index_range;
       {looks up the index value for a given ccode}
       {Returns "initialized_value" if a bad ccode given.}
       {Also, since index list initialized with "initialized value", then func
        will return it if a ccode, e.g. 301, is given that is not a ccode that was read.
        This ensures that if other procedures try to initialize some matrix using a non
        existant ccode, then cell "initialized value" will be initialized.  Usually this
        will be cell -1, given the current setting for initialized_value.}
      begin
         index := initialized_value;
         if self = nil then
               begin
                  EUGeneError ('Called ccode_index.index before initialization',
                     5, stop, error_log);
               end else
         if not(initialized) then
               begin
                  EUGeneError ('Called ccode_index.index before initialization',
                     5, stop, error_log);
               end else
         if accode=initialized_value then index := initialized_value
            else index := index_data[accode];
      end;


   function Tccode_index_obj.ccode (anindex : ccode_index_range) : ccode_range;
      {returns the ccode that is in the spot of a particular index #}
      begin
         ccode := initialized_value;
         if self = nil then
               begin
                  EUGeneError ('Called ccode_index.ccode before initialization',
                     5, stop, error_log);
               end else
         if not(initialized) then
               begin
                  EUGeneError ('Called ccode_index.ccode before initialization',
                     5, stop, error_log);
               end else
         if anindex=initialized_value then ccode := initialized_value
            else ccode := ccode_data[anindex];
         {ccode := initialized_value;
         found := false;
         current_ccode := 0;
         repeat
            inc (current_ccode);
         until (current_ccode > max_ccode) or (found);
         if found then ccode := current_ccode;   }
      end;

   function Tccode_index_obj.return_ccode_list : ccode_index_array;
        {just returns the ccode list for use elsewhere}
      begin
         if self = nil then
               begin
                  EUGeneError ('Called ccode_index.return_ccode_list before initialization',
                     5, stop, error_log);
               end else
         if not(initialized) then
               begin
                  EUGeneError ('Called ccode_index.return_ccode_list before initialization',
                     5, stop, error_log);
               end else
         return_ccode_list := ccode_data;
      end;

   function Tccode_index_obj.identical_ccode_lists (another_ccode_index_array : ccode_index_array ): boolean;
        {true if the input list has same ccode index values as existing list}
      var index_value : ccode_index_range;
      begin
         identical_ccode_lists := false;
         if self = nil then
               begin
                  EUGeneError ('Called ccode_index.return_ccode_list before initialization',
                     5, stop, error_log);
               end else
         if not(initialized) then
               begin
                  EUGeneError ('Called ccode_index.return_ccode_list before initialization',
                     5, stop, error_log);
               end else
            begin
               index_value := min_ccode_index;
               while (ccode_data[index_value] = another_ccode_index_array[index_value]) and
                     (index_value < max_ccode_index) do
                  inc(index_value);
               if (index_value = max_ccode_index) and
                  (ccode_data[index_value] = another_ccode_index_array[index_value])
                  then {made it through the list with all values equal}
                     identical_ccode_lists := true;
            end;
      end;

   function Tccode_index_obj.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then if created=true then initialized := true;
      end;


   {  -----------------------   methods for Tnation_obj, the individual nation record  }

   constructor Tnationdat_obj.init (accode : ccode_range; aabbrev : string; afullname : string;
          astartyear1, aendyear1, astartyear2, aendyear2 : year_range;
          astartmonth1, aendmonth1, astartmonth2, aendmonth2 : month_range;
          astartday1, aendday1, astartday2, aendday2 : day_range;
          aregion : region_type; involvement : involved_list_ptr);
      var which_gp_rec : 1..max_gp_records;
          begin
             ccode := accode;
             abbrev := aabbrev;
             fullname := afullname;
             region := aregion;
             involved_in_list := involvement;  {note - memory allocated outside the obj}
             startyear1 := astartyear1;
             endyear1 := aendyear1;
             startmonth1 := astartmonth1;
             endmonth1 := aendmonth1;
             startday1 := astartday1;
             endday1 := aendday1;
             startyear2 := astartyear2;
             endyear2 := aendyear2;
             startmonth2 := astartmonth2;
             endmonth2 := aendmonth2;
             startday2 := astartday2;
             endday2 := aendday2;
             for which_gp_rec := 1 to max_gp_records do
               begin
                  gpstartyear[which_gp_rec] := min_year;
                  gpendyear[which_gp_rec] := min_year;
                  gpstartmonth[which_gp_rec] := 0;
                  gpendmonth[which_gp_rec] := 0;
                  gpstartday[which_gp_rec] := 0;
                  gpendday[which_gp_rec] := 0;
               end;

          end;

   procedure Tnationdat_obj.set_gp_info (which : integer; start_year, end_year : year_range;
                 start_month, end_month : month_range; start_day, end_day : day_range);
      begin
         gpstartyear[which] := start_year;
         gpendyear[which] := end_year;
         gpstartmonth[which] := start_month;
         gpendmonth[which] := end_month;
         gpstartday[which] := start_day;
         gpendday[which] := end_day;
      end;

   destructor Tnationdat_obj.destroy;
      begin
         try
            if self <> nil then
               begin
                  if involved_in_list <> nil then dispose (involved_in_list);
                  involved_in_list := nil;
                  inherited destroy;
               end;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;

   function Tnationdat_obj.get_ccode : ccode_range;
          begin
             get_ccode := ccode;
          end;
   function Tnationdat_obj.get_abbrev : string;
          begin
             get_abbrev := abbrev;
          end;
   function Tnationdat_obj.get_fullname : string;
          begin
             get_fullname := fullname;
          end;
   function Tnationdat_obj.get_home_region : region_type;
          begin
             get_home_region := region;
          end;
   function Tnationdat_obj.is_involved_in_region (aregion : region_type;
                                                  ayear : year_range) : boolean;
        {is involved in region if it is its home region, or if previously set on
         involvement list}
      begin
         if aregion=get_home_region then
            is_involved_in_region := true
         else if involved_in_list = nil then   {not home region, and no list}
            is_involved_in_region := false
         else if ((ayear >= involved_in_list^[aregion].start_year) and
                  (ayear <= involved_in_list^[aregion].end_year)) then
            is_involved_in_region := true
         else
            is_involved_in_region := false;
      end;

   function Tnationdat_obj.get_startyear1 : year_range;
          begin
             get_startyear1 := startyear1;
          end;
   function Tnationdat_obj.get_endyear1 : year_range;
          begin
             get_endyear1 := endyear1;
          end;
   function Tnationdat_obj.get_startyear2 : year_range;
          begin
             get_startyear2 := startyear2;
          end;
   function Tnationdat_obj.get_endyear2 : year_range;
          begin
             get_endyear2 := endyear2;
          end;
   function Tnationdat_obj.get_gpstartyear (which : integer) : year_range;
          begin
             get_gpstartyear := gpstartyear[which];
          end;
   function Tnationdat_obj.get_gpendyear (which : integer) : year_range;
          begin
             get_gpendyear := gpendyear[which];
          end;

   {  -----------------------   methods for Tnation_array  }

   function Tnation_array_obj.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then if created=true then initialized := true;
      end;

   function set_extra_involvement_list (ccode : ccode_range) : involved_list_ptr;
      {this creates a new involved_in_list if necessary, and sets years for a state being
       involved in some other region.}
         var inv_list : involved_list_ptr;
         begin
            if MaxAvail <= sizeof(involved_list_ptr) then
               begin
                  EUGeneError ('Out of memory - cannot allocate involved_list_ptr',
                     5, stop, error_log);
               end;
            try
            case ccode of
               2 : begin   {us}
                        new (inv_list);
                        inv_list^[none].start_year := min_year;
                        inv_list^[none].end_year := min_year;
                        inv_list^[europe].start_year := 1898;
                        inv_list^[europe].end_year := max_year;
                        inv_list^[middleeast].start_year := 1898;
                        inv_list^[middleeast].end_year := max_year;
                        inv_list^[africa].start_year := 1946;
                        inv_list^[africa].end_year := max_year;
                        inv_list^[asia].start_year := min_year;
                        inv_list^[asia].end_year := max_year;
                        inv_list^[americas].start_year := min_year;
                        inv_list^[americas].end_year := max_year;
                     end;
               200 : begin  {UK}
                        new (inv_list);
                        inv_list^[none].start_year := min_year;
                        inv_list^[none].end_year := min_year;
                        inv_list^[europe].start_year := min_year;
                        inv_list^[europe].end_year := max_year;
                        inv_list^[middleeast].start_year := min_year;
                        inv_list^[middleeast].end_year := max_year;
                        inv_list^[africa].start_year := min_year;
                        inv_list^[africa].end_year := max_year;
                        inv_list^[asia].start_year := min_year;
                        inv_list^[asia].end_year := max_year;
                        inv_list^[americas].start_year := min_year;
                        inv_list^[americas].end_year := max_year;
                     end;
               210 : begin    {holland/netherlands}
                        new (inv_list);
                        inv_list^[none].start_year := min_year;
                        inv_list^[none].end_year := min_year;
                        inv_list^[europe].start_year := min_year;
                        inv_list^[europe].end_year := max_year;
                        inv_list^[middleeast].start_year := min_year;
                        inv_list^[middleeast].end_year := min_year;
                        inv_list^[africa].start_year := min_year;
                        inv_list^[africa].end_year := min_year;
                        inv_list^[asia].start_year := min_year;
                        inv_list^[asia].end_year := min_year;
                        inv_list^[americas].start_year := min_year;
                        inv_list^[americas].end_year := max_year;
                     end;
               211 : begin    {belgium}
                        new (inv_list);
                        inv_list^[none].start_year := min_year;
                        inv_list^[none].end_year := min_year;
                        inv_list^[europe].start_year := min_year;
                        inv_list^[europe].end_year := max_year;
                        inv_list^[middleeast].start_year := min_year;
                        inv_list^[middleeast].end_year := min_year;
                        inv_list^[africa].start_year := min_year;
                        inv_list^[africa].end_year := max_year;
                        inv_list^[asia].start_year := min_year;
                        inv_list^[asia].end_year := min_year;
                        inv_list^[americas].start_year := min_year;
                        inv_list^[americas].end_year := min_year;
                     end;
               220 : begin      {france}
                        new (inv_list);
                        inv_list^[none].start_year := min_year;
                        inv_list^[none].end_year := min_year;
                        inv_list^[europe].start_year := min_year;
                        inv_list^[europe].end_year := max_year;
                        inv_list^[middleeast].start_year := min_year;
                        inv_list^[middleeast].end_year := max_year;
                        inv_list^[africa].start_year := min_year;
                        inv_list^[africa].end_year := max_year;
                        inv_list^[asia].start_year := min_year;
                        inv_list^[asia].end_year := max_year;
                        inv_list^[americas].start_year := min_year;
                        inv_list^[americas].end_year := max_year;
                     end;
               230 : begin    {spain}
                        new (inv_list);
                        inv_list^[none].start_year := min_year;
                        inv_list^[none].end_year := min_year;
                        inv_list^[europe].start_year := min_year;
                        inv_list^[europe].end_year := max_year;
                        inv_list^[middleeast].start_year := min_year;
                        inv_list^[middleeast].end_year := 1936;
                        inv_list^[africa].start_year := min_year;
                        inv_list^[africa].end_year := min_year;
                        inv_list^[asia].start_year := min_year;
                        inv_list^[asia].end_year := max_year;
                        inv_list^[americas].start_year := min_year;
                        inv_list^[americas].end_year := max_year;
                     end;
               235 : begin   {portugal}
                        new (inv_list);
                        inv_list^[none].start_year := min_year;
                        inv_list^[none].end_year := min_year;
                        inv_list^[europe].start_year := min_year;
                        inv_list^[europe].end_year := max_year;
                        inv_list^[middleeast].start_year := min_year;
                        inv_list^[middleeast].end_year := min_year;
                        inv_list^[africa].start_year := min_year;
                        inv_list^[africa].end_year := max_year;
                        inv_list^[asia].start_year := min_year;
                        inv_list^[asia].end_year := max_year;
                        inv_list^[americas].start_year := min_year;
                        inv_list^[americas].end_year := max_year;
                     end;
               255 : begin    {Germany}
                        new (inv_list);
                        inv_list^[none].start_year := min_year;
                        inv_list^[none].end_year := min_year;
                        inv_list^[europe].start_year := min_year;
                        inv_list^[europe].end_year := max_year;
                        inv_list^[middleeast].start_year := min_year;
                        inv_list^[middleeast].end_year := max_year;
                        inv_list^[africa].start_year := min_year;
                        inv_list^[africa].end_year := 1945;
                        inv_list^[asia].start_year := min_year;
                        inv_list^[asia].end_year := max_year;
                        inv_list^[americas].start_year := min_year;
                        inv_list^[americas].end_year := min_year;
                     end;
               260 : begin    {W. Germany}
                        new (inv_list);
                        inv_list^[none].start_year := min_year;
                        inv_list^[none].end_year := min_year;
                        inv_list^[europe].start_year := min_year;
                        inv_list^[europe].end_year := max_year;
                        inv_list^[middleeast].start_year := min_year;
                        inv_list^[middleeast].end_year := max_year;
                        inv_list^[africa].start_year := min_year;
                        inv_list^[africa].end_year := min_year;
                        inv_list^[asia].start_year := min_year;
                        inv_list^[asia].end_year := max_year;
                        inv_list^[americas].start_year := min_year;
                        inv_list^[americas].end_year := min_year;
                     end;
               300 : begin    {Austria-Hungary}
                        new (inv_list);
                        inv_list^[none].start_year := min_year;
                        inv_list^[none].end_year := min_year;
                        inv_list^[europe].start_year := min_year;
                        inv_list^[europe].end_year := max_year;
                        inv_list^[middleeast].start_year := min_year;
                        inv_list^[middleeast].end_year := 1918;
                        inv_list^[africa].start_year := min_year;
                        inv_list^[africa].end_year := min_year;
                        inv_list^[asia].start_year := min_year;
                        inv_list^[asia].end_year := min_year;
                        inv_list^[americas].start_year := min_year;
                        inv_list^[americas].end_year := min_year;
                     end;
               325 : begin    {italy}
                        new (inv_list);
                        inv_list^[none].start_year := min_year;
                        inv_list^[none].end_year := min_year;
                        inv_list^[europe].start_year := min_year;
                        inv_list^[europe].end_year := max_year;
                        inv_list^[middleeast].start_year := 1860;
                        inv_list^[middleeast].end_year := 1943;
                        inv_list^[africa].start_year := min_year;
                        inv_list^[africa].end_year := 1943;
                        inv_list^[asia].start_year := min_year;
                        inv_list^[asia].end_year := min_year;
                        inv_list^[americas].start_year := min_year;
                        inv_list^[americas].end_year := min_year;
                     end;
               350 : begin    {greece}
                        new (inv_list);
                        inv_list^[none].start_year := min_year;
                        inv_list^[none].end_year := min_year;
                        inv_list^[europe].start_year := min_year;
                        inv_list^[europe].end_year := max_year;
                        inv_list^[middleeast].start_year := 1828;
                        inv_list^[middleeast].end_year := max_year;
                        inv_list^[africa].start_year := min_year;
                        inv_list^[africa].end_year := min_year;
                        inv_list^[asia].start_year := min_year;
                        inv_list^[asia].end_year := min_year;
                        inv_list^[americas].start_year := min_year;
                        inv_list^[americas].end_year := min_year;
                     end;
               365 : begin                        {russia/ussr}
                        new (inv_list);
                        inv_list^[none].start_year := min_year;
                        inv_list^[none].end_year := min_year;
                        inv_list^[europe].start_year := min_year;
                        inv_list^[europe].end_year := max_year;
                        inv_list^[middleeast].start_year := min_year;
                        inv_list^[middleeast].end_year := max_year;
                        inv_list^[africa].start_year := min_year;
                        inv_list^[africa].end_year := max_year;
                        inv_list^[asia].start_year := min_year;
                        inv_list^[asia].end_year := max_year;
                        inv_list^[americas].start_year := min_year;
                        inv_list^[americas].end_year := max_year;
                           {Bdm p. 96 has Russia listed 1800s, 1945+.  The period
                            1900-1945 dropped in a kludge fix in the set involvement
                            list procedure further down.}
                     end;
               640 : begin   {turkey}
                        new (inv_list);
                        inv_list^[none].start_year := min_year;
                        inv_list^[none].end_year := min_year;
                        inv_list^[europe].start_year := min_year;
                        inv_list^[europe].end_year := max_year;
                        inv_list^[middleeast].start_year := min_year;
                        inv_list^[middleeast].end_year := max_year;
                        inv_list^[africa].start_year := min_year;
                        inv_list^[africa].end_year := min_year;
                        inv_list^[asia].start_year := min_year;
                        inv_list^[asia].end_year := min_year;
                        inv_list^[americas].start_year := min_year;
                        inv_list^[americas].end_year := min_year;
                     end;
               710 : begin   {china}
                        new (inv_list);
                        inv_list^[none].start_year := min_year;
                        inv_list^[none].end_year := min_year;
                        inv_list^[europe].start_year := 1950;
                        inv_list^[europe].end_year := max_year;
                        inv_list^[middleeast].start_year := min_year;
                        inv_list^[middleeast].end_year := min_year;
                        inv_list^[africa].start_year := 1946;
                        inv_list^[africa].end_year := max_year;
                        inv_list^[asia].start_year := min_year;
                        inv_list^[asia].end_year := max_year;
                        inv_list^[americas].start_year := min_year;
                        inv_list^[americas].end_year := min_year;
                     end;
               740 : begin          {japan}
                        new (inv_list);
                        inv_list^[none].start_year := min_year;
                        inv_list^[none].end_year := min_year;
                        inv_list^[europe].start_year := 1895;
                        inv_list^[europe].end_year := 1945;
                        inv_list^[middleeast].start_year := min_year;
                        inv_list^[middleeast].end_year := min_year;
                        inv_list^[africa].start_year := min_year;
                        inv_list^[africa].end_year := min_year;
                        inv_list^[asia].start_year := min_year;
                        inv_list^[asia].end_year := max_year;
                        inv_list^[americas].start_year := min_year;
                        inv_list^[americas].end_year := min_year;
                     end;
               else inv_list := nil;   {state is only involved in own home region}
            end;    {case}

            set_extra_involvement_list := inv_list;
            except
               if inv_list <> nil then dispose (inv_list);
            end;
         end;             {function set_involvement_list}

   constructor Tnation_array_obj.init (configuration: configuration_type);
      {This creates a new data array for nation info, reads the input file,
       and sets countries.  It also initializes the index array. }
      {Nation list in input data must match cow/pss format as of 8/29/98.  }
      var x, y : integer;
         infile : text;
         achar : char;
         namecount : integer;
         previous_ccode, current_ccode : ccode_range;
         have_a_record : boolean;
         abbrev, abbrevnew : string[3];
         fullname, fullnamenew : string[longnamestringlength];
         startyear1, endyear1, startyear2, endyear2 : year_range;
         startmonth1, endmonth1, startmonth2, endmonth2 : month_range;
         startday1, endday1, startday2, endday2 : day_range;
         gpstartyear, gpendyear  : year_range;
         gpstartmonth, gpendmonth : month_range;
         gpstartday, gpendday : day_range;
         which_gp_rec : integer;
         aregion : region_type;
         accode, ccode1, ccode2 : ccode_range;
         ayear : year_range;
         state_year_matrix : state_year_matrix_ptr;
         start_mem : longint;
         heapneeded : longint;
         {tempdat : PNationarray_type;}
         involvement_list : involved_list_ptr;
         centralstart, centralend : year_range;
         nation_trace : ttrace_obj;
         temp_nation_ccode_rec : same_nation_diff_ccode_rec;

      begin
         try
            trace.enter('Initializing nation_array');
            try
               nation_trace := ttrace_obj.init (trace.get_trace_level);
               {Note - this needs to use memavail to calculate memory used, but maxavial
               for largest block to check against.}
               start_mem := memavail;
               if not(ccode_index.initialized) then
                  begin
                     EUGeneError ('Nation array creation called before ccode_index initialized',
                           5, stop, error_log);
                     trace.message('Check program coding [notify programmer].');
                  end;
                  {Mem needed:  main structure, plus involved_list.  But - don't know size of
                   involved_list yet.  }
               heapneeded := Tnation_array_obj_mem_overhead;
                  {note - that memory check includes nation_dat sub objects}
               if debug[4] then
                  begin
                     trace.message ('Nation_list size calculation');
                     trace.message ('Calc is that '+inttostr(heapneeded)+' needed for full structure.');
                     trace.message ('Max avail mem block is '+inttostr(MaxAvail));
                  end;
               if MaxAvail <= (heapneeded) then
                  begin
                     EUGeneError ('Not enough memory for nation_array',
                           5, stop, error_log);
                  end;
               new (data);
               for x:= min_ccode_index to max_ccode_index do
                  begin
                     data^[x] := Tnationdat_obj.init (initialized_value,'xxx','initial',
                              min_year, min_year, min_year, min_year, 0, 0, 0, 0, 0, 0, 0, 0, none, nil);
                  end;

               trace.message('Reading nation data from file');
                      {input file is ccode, abbrev, name, membership years}
               try
                  assignFile (infile, configuration.nation_list_file_name);
                  reset (infile);

                  {In the cow format, the first line is a line of variable labels.  Skip it.}
                  readln(infile);

                  {Now set initial values}
                  previous_ccode := 0;
                  have_a_record := false;
                  abbrev := 'xxx';
                  fullname := 'Name';
                  startyear1 := min_year;
                  endyear1 := min_year;
                  startyear2 := min_year;
                  endyear2 := min_year;
                  startmonth1 := 0;
                  endmonth1 := 0;
                  startday1 := 0;
                  endday1 := 0;
                  startmonth2 := 0;
                  endmonth2 := 0;
                  startday2 := 0;
                  endday2 := 0;
                  {gps initialized automatically in .init procedure}

                  aregion := none;
                  involvement_list := nil;

                  {Now iterate through the input nation file}
                  while not eof (infile) do
                  begin
                     nation_trace.tick('Executing Procedure: Read Nation Data',0);
                     {variable order is abbrev, ccode, name, styear, stmonth, stday, endyr, endmo, enddy, version}

                     {The states 2004 data changed the order of abbreviation and name compared
                      to 2002 version.  So this version of EUGene will only work with 2004+ states file.}

                     {First read abbreviation}
                     abbrevnew := read_csv_string(infile);

                     {then read ccode}
                     current_ccode := read_csv_int (infile);

                     {check if this is new record, or a continuation}
                     if current_ccode <> previous_ccode then
                     begin
                        {This is a new country and record.}
                        {So, start by writing previous record, if I have one}
                        {First, drop the old blank record. Then store the info
                         I gathered on previous_ccode.}
                        if have_a_record then
                        begin
                           data^[ccode_index.index(previous_ccode)].free;
                           involvement_list := set_extra_involvement_list(previous_ccode);
                           data^[ccode_index.index(previous_ccode)] :=
                             Tnationdat_obj.init (previous_ccode, abbrev,
                                fullname, startyear1, endyear1, startyear2, endyear2,
                                startmonth1, endmonth1, startmonth2, endmonth2,
                                startday1, endday1, startday2, endday2,
                                sing_small_region (previous_ccode), involvement_list);
                           have_a_record := false;
                           {set rest of variables to blank values}
                           abbrev := 'xxx';
                           fullname := 'Name';
                           startyear1 := min_year;
                           endyear1 := min_year;
                           startyear2 := min_year;
                           endyear2 := min_year;
                           startmonth1 := 0;
                           endmonth1 := 0;
                           startday1 := 0;
                           endday1 := 0;
                           startmonth2 := 0;
                           endmonth2 := 0;
                           startday2 := 0;
                           endday2 := 0;
                          {gps initialized automatically in .init procedure}
                        end;   {if have a record already}

                        {Now I'm on this record.  Return abbreviation to main storage variable}
                        abbrev := abbrevnew;

                        {Next read fullname.  }
                        fullname := read_csv_string(infile);


                        {Next is set of dates.}
                        startyear1 := read_csv_int (infile);
                        startmonth1 := read_csv_int (infile);
                        startday1 := read_csv_int (infile);
                        endyear1 := read_csv_int (infile);
                        endmonth1 := read_csv_int (infile);
                        endday1 := read_csv_int (infile);

                        {Finally, have version.  Note that this will repeatedly be re-read into
                         the same variable at the level of the nation_data structure, because we
                         don't need 200 copies of it.}
                        state_version := read_csv_real (infile);

                        if not eof (infile) then readln (infile);
                        have_a_record := true;

                        {That completed read of the record.  But, before can set it in the array,
                         need to check to see if there is a second record for this ccode.
                         So don't set it in the array now, but go to next record}
                         {DO set this ccode as previous, though, so can check the next.}
                        previous_ccode := current_ccode;
                     end   {ccode read was NOT a repeat}
                           {Now done reading the first (and maybe only) record}
                     else
                     begin    {current_ccode = previous_ccode}
                        {The ccode read was the same as previous.  So, finish read as 2nd record}
                        {1st double check the abbreviation}
                        if (abbrev <> abbrevnew) then
                           begin
                              EUGeneError ('ERROR reading nations file - ', 5, continue, error_log);
                              trace.message ('3 letter abbrevs do not match for CCode ' + inttostr(current_ccode));
                           end;

                        {Next read fullname.  }
                        fullname := read_csv_string(infile);

                        {Next is set of dates.}
                        startyear2 := read_csv_int (infile);
                        startmonth2 := read_csv_int (infile);
                        startday2 := read_csv_int (infile);
                        endyear2 := read_csv_int (infile);
                        endmonth2 := read_csv_int (infile);
                        endday2 := read_csv_int (infile);

                        state_version := read_csv_real (infile);

                        if not eof (infile) then readln (infile);
                        have_a_record := true;

                     end;   {reading the 2nd record}

                  end;  {while not eof infile}

                  {Now finished whole file.  But, may still have a record not assigned to array}
                  {First drop old blank record.}
                  if have_a_record then
                        begin
                           data^[ccode_index.index(previous_ccode)].free;
                           involvement_list := set_extra_involvement_list(previous_ccode);
                           data^[ccode_index.index(previous_ccode)] :=
                              Tnationdat_obj.init (previous_ccode, abbrev,
                                fullname, startyear1, endyear1, startyear2, endyear2,
                                startmonth1, endmonth1, startmonth2, endmonth2,
                                startday1, endday1, startday2, endday2,
                                sing_small_region (previous_ccode), involvement_list);
                           have_a_record := false;
                        end;
                  created := true;
                  trace.message ('Initial Nation_array required ' +
                                 inttostr(round(((start_mem-memavail)/1024)))+' K');
               finally
                  CloseFile (infile);
                  nation_trace.tickdone;
               end;

            except
               on EUserInterrupt do raise;
               on EInOutError do
                 begin
                    FileErrorBox.maindo ('Error opening file "'+configuration.nation_list_file_name+ '"',
                                         'File could not be opened for input.',
                                         'File may be in use by another program, or may be missing.');
                    FileErrorBox.showmodal;
                    raise;
                 end;
            end;


               {Now read GP status file }
            try
               try
                  assignFile (infile, configuration.major_list_file_name);
                  reset (infile);

                  {In the cow format, the first line is a line of variable labels.  Skip it.}
                  readln(infile);
                  which_gp_rec := 1;
                  previous_ccode := min_ccode;

                  while not eof(infile) do
                  begin
                     nation_trace.tick('Executing Procedure: Read Great Power Data',0);
                     {variable order is abbrev, ccode, name, styear, stmonth, stday, endyr, endmo, enddy, version}

                     {Next read abbreviation}
                     abbrevnew := read_csv_string(infile);

                     {read ccode}
                     current_ccode := read_csv_int (infile);

                     if current_ccode=previous_ccode then
                        which_gp_rec := which_gp_rec + 1
                        else which_gp_rec := 1;

                     gpstartyear := read_csv_int (infile);
                     gpstartmonth := read_csv_int (infile);
                     gpstartday := read_csv_int (infile);
                     gpendyear := read_csv_int (infile);
                     gpendmonth := read_csv_int (infile);
                     gpendday := read_csv_int (infile);

                     major_version := read_csv_real (infile);

                     if not eof (infile) then readln (infile);

                     data^[ccode_index.index(current_ccode)].set_gp_info (which_gp_rec, gpstartyear, gpendyear,
                        gpstartmonth, gpendmonth, gpstartday, gpendday);

                     previous_ccode := current_ccode;
                  end;      {while}

               finally
                  CloseFile (infile);
                  nation_trace.tickdone;
               end;
            except
               on EUserInterrupt do raise;
               on EInOutError do
                 begin
                    FileErrorBox.maindo ('Error opening file "'+configuration.major_list_file_name+ '"',
                                         'File could not be opened for input.',
                                         'File may be in use by another program, or may be missing.');
                    FileErrorBox.showmodal;
                    raise;
                 end;
            end;

            try
               {Now set up the is_a_state matrix for fast access.}
               trace.message('Setting up fast state access matrix.');
               new (is_a_state_matrix);
               for accode := min_ccode to max_ccode do
                  is_a_state_matrix^[accode] := nil;
               for accode := min_ccode to max_ccode do
                  begin
                     nation_trace.tick ( 'Executing Procedure: Create Fast State Access Matrix',max_ccode);
                     new (state_year_matrix);
                     if not (have_info(accode)) then
                        for ayear := min_year to max_year do
                           state_year_matrix^[ayear] := false
                     else
                        for ayear := min_year to max_year do
                          begin
                           if ( ((ayear >= get_startyear1(accode)) and (ayear <= get_endyear1(accode))) or
                                ((ayear >= get_startyear2(accode)) and (ayear <= get_endyear2(accode))) ) then
                             state_year_matrix^[ayear] := true
                           else state_year_matrix^[ayear] := false;
                           {also put in a check for out of bounds years}
                           if (ayear < configuration.first_nation_year) or
                              (ayear > configuration.last_nation_year) then
                              state_year_matrix^[ayear] := false;
                          end;
                     is_a_state_matrix^[accode] := state_year_matrix;
                  end;

            finally
               trace.message('Finished initializing nation_array fast access matrix');
            end;

            {now read the info on states that have changing ccodes.}
            try
               trace.message('Setting up ccode-changing-state-matrix.');
               setlength (nations_with_different_ccode_data,0);

               assignFile (infile, configuration.same_state_diff_ccode_list_file_name);
               reset (infile);

               {The first line is a line of variable labels.  Skip it.}
               readln(infile);
               while not eof(infile) do
                  begin
                     nation_trace.tick('Executing Procedure: Read same ccode data',0);
                     {variable order is state1, state2, year}
                     setlength (nations_with_different_ccode_data, length(nations_with_different_ccode_data)+1);
                     nations_with_different_ccode_data[high(nations_with_different_ccode_data)].state1 := read_csv_int(infile);
                     nations_with_different_ccode_data[high(nations_with_different_ccode_data)].state2 := read_csv_int(infile);
                     nations_with_different_ccode_data[high(nations_with_different_ccode_data)].year := read_csv_int(infile);
                     if nations_with_different_ccode_data[high(nations_with_different_ccode_data)].state1 > nations_with_different_ccode_data[high(nations_with_different_ccode_data)].state2 then
                        begin
                           accode := nations_with_different_ccode_data[high(nations_with_different_ccode_data)].state1;
                           nations_with_different_ccode_data[high(nations_with_different_ccode_data)].state1 := nations_with_different_ccode_data[high(nations_with_different_ccode_data)].state2;
                           nations_with_different_ccode_data[high(nations_with_different_ccode_data)].state2 := accode;
                        end;
                     if not eof (infile) then readln (infile);
                  end;
               {Now sort by ccode1; few enough records to do a simple sort.}
               for x := 0 to high(nations_with_different_ccode_data) - 1 do
                  for y := x+1 to high(nations_with_different_ccode_data) do
                     if nations_with_different_ccode_data[x].state1 > nations_with_different_ccode_data[y].state1 then
                        begin  {swap records}
                           temp_nation_ccode_rec := nations_with_different_ccode_data[x];
                           nations_with_different_ccode_data[x] := nations_with_different_ccode_data[y];
                           nations_with_different_ccode_data[y] := temp_nation_ccode_rec;
                        end;
               nation_trace.tickdone;
            finally
               CloseFile (infile);
               trace.message('Finished initializing states with changing ccodes');
            end;

         finally
            trace.message ('Nation_array and fast access matrix required ' +
                           inttostr(round(((start_mem-memavail)/1024)))+' K');
            trace.exit('Finished initializing nation_array structures');
            nation_trace.free;
         end;
      end;   {constructor}

   destructor Tnation_array_obj.destroy;
      var accode_index : ccode_index_range;
          accode : ccode_range;
      begin
         try
            if self <> nil then              {if self=nil, then done already}
            begin
               {first, dispose of all of the initialized_list records, some of which may
                have been allocated on the heap.}
               if data <> nil then
                  begin
                     for accode_index := min_ccode_index to max_ccode_index do
                        begin
                           if data^[accode_index] <> nil then
                              data^[accode_index].free;
                           data^[accode_index] := nil;
                        end;    {for accode}
                     dispose (data);
                  end;
               data := nil;
               if is_a_state_matrix <> nil then
                  begin
                     for accode := min_ccode to max_ccode do
                       if is_a_state_matrix^[accode] <> nil then
                          begin
                             dispose (is_a_state_matrix^[accode]);
                             is_a_state_matrix^[accode] := nil;
                          end;
                     dispose (is_a_state_matrix);
                     is_a_state_matrix := nil;
                     end;          {if is_a_state_matrix <> nil}
               is_a_state_matrix := nil;
             created := false;
             inherited destroy;
            end;   {if self <> nil}
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;    {proc done. }

   procedure Tnation_array_obj.calculate_n;
      var ccode1, ccode2 : ccode_range;
          ayear : year_range;
      begin
         trace.message('Computing number of dyad-years and nation-years.');
         ndyad_years := 0;
         ncountry_years := 0;
         ndyads := 0;

         {don't want to recompute each run, b/c large!!}
         ndyad_years := 1077682;
         ncountry_years := 11502;
         ndyads := 43264;       {208 states, squared}

         {for ccode1 := min_ccode to max_ccode do
            if self.have_info (ccode1) then
               for ccode2 := min_ccode to max_ccode do
                 begin
                   if (self.have_info (ccode2)) then
                     inc(ndyads);
                   trace.tick (200, 'Computing dyad/state counts',0,0);
                 end;
         for ayear := min_year to max_year do
           for ccode1 := min_ccode to max_ccode do
             if self.is_a_state (ccode1, ayear) then
               begin
                  inc(ncountry_years);
                  for ccode2 := min_ccode to max_ccode do
                     if self.is_a_state (ccode2, ayear) then
                        inc(ndyad_years);
                  trace.tick (200, 'Computing dyad/state counts',0,0);
               end;
         trace.tickdone;   }
      end;

   function Tnation_array_obj.get_ccode (accode : ccode_range) : ccode_range;
      {Note - this should always return the same ccode as it is called with if
      the data is properly read and initialized.}
      begin
         if not(initialized) then
            begin
               get_ccode := initialized_value;
               EUGeneError ('Error - nation_array get_ccode called before initializtion',
                       5, continue, error_log);
            end
         else
            get_ccode := data^[ccode_index.index(accode)].get_ccode;
      end;

   function Tnation_array_obj.get_abbrev (accode : ccode_range) : string;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Error - nation_array get_abbrev called before initializtion',
                       5, continue, error_log);
            end
         else
            get_abbrev := data^[ccode_index.index(accode)].get_abbrev;
      end;
   function Tnation_array_obj.get_fullname (accode : ccode_range) : string;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Error - nation_array get_fullname called before initialization',
                       5, continue, error_log);
            end
         else
         get_fullname := data^[ccode_index.index(accode)].get_fullname;
      end;
   function Tnation_array_obj.get_home_region (accode : ccode_range) : region_type;
      begin
         if accode = 0 then get_home_region := none
         else
            if not(initialized) then
               begin
                  get_home_region := none;
                  EUGeneError ('Error - nation_array get_home_region called before initialization',
                          5, continue, error_log);
               end
         else
            get_home_region := data^[ccode_index.index(accode)].get_home_region;
      end;
   function Tnation_array_obj.is_involved_in_region (accode : ccode_range; aregion : region_type;
                                                     ayear : year_range) : boolean;
      begin
         if aregion = globe then is_involved_in_region := true
         else if not(initialized) then
            begin
               is_involved_in_region := false;
               EUGeneError ('Error - nation_array is_involved_in_region called before initialization',
                       5, continue, error_log);
            end
         else
            is_involved_in_region := data^[ccode_index.index(accode)].is_involved_in_region(aregion, ayear);
         {This is a total kludge fix for Russia.  Right now (8/96) the data set sets up data
          for russia to be involved in the Americas for the whole time period 1816-1992.  BdM
          actually has Russia involved only 1816-1900, and 1945+.  So fix the period when Russia
          really isn't involved.  This is kludged because Russia is the only country that has
          two periods of involvement, the others are all one.}
         if ((accode=365) and (ayear > 1900) and (ayear < 1945)) then is_involved_in_region := false;
      end;
   function Tnation_array_obj.get_startyear1 (accode : ccode_range) : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Error - nation_array get_startyear1 called before initializtion',
                       5, continue, error_log);
            end
         else
         get_startyear1 := data^[ccode_index.index(accode)].get_startyear1;
      end;
   function Tnation_array_obj.get_endyear1 (accode : ccode_range) : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Error - nation_array get_endyear1 called before initializtion',
                       5, continue, error_log);
            end
         else
         get_endyear1 := data^[ccode_index.index(accode)].get_endyear1;
      end;
   function Tnation_array_obj.get_startyear2 (accode : ccode_range) : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Error - nation_array get_startyear2 called before initializtion',
                       5, continue, error_log);
            end
         else
            get_startyear2 := data^[ccode_index.index(accode)].get_startyear2;
      end;
   function Tnation_array_obj.get_endyear2 (accode : ccode_range) : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Error - nation_array get_endyear2 called before initializtion',
                       5, continue, error_log);
            end
         else
         get_endyear2 := data^[ccode_index.index(accode)].get_endyear2;
      end;
   function Tnation_array_obj.get_gpstartyear (accode : ccode_range; which : integer) : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Error - nation_array get_gpstartyear1 called before initializtion',
                       5, continue, error_log);
            end
         else
            get_gpstartyear := data^[ccode_index.index(accode)].get_gpstartyear(which);
      end;
   function Tnation_array_obj.get_gpendyear (accode : ccode_range; which : integer) : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Error - nation_array get_gpendyear1 called before initializtion',
                       5, continue, error_log);
            end
         else
         get_gpendyear := data^[ccode_index.index(accode)].get_gpendyear(which);
      end;

   function Tnation_array_obj.is_a_state (accode : ccode_range; ayear : year_range) : boolean;
      begin
         if accode = 0 then is_a_state := false
         else
            if not(initialized) then
               begin
                  EUGeneError ('Error - nation_array is_a_state called before initialization',
                          5, continue, error_log);
               end
         else
              {Previously, in init constructor, set up a matrix for fast access to
               status as a state or not.  It should have all booleans for direct lookup.}
            is_a_state := is_a_state_matrix^[accode]^[ayear];
      end;        {is a state}

   function Tnation_array_obj.is_a_state_between (accode : ccode_range; year1, year2 : year_range) : boolean;
         {returns true if ccode is a state between the two years given.}
      var temp : year_range;
      begin
         is_a_state_between := false;
         if year1 > year2 then
           begin
             temp := year1;
             year1 := year2;
             year2 := temp;
           end;
         if accode = 0 then
         	 is_a_state_between := false
         else
            if not(initialized) then
               EUGeneError ('Error - nation_array is_a_state_between called before initializtion',
                       5, continue, error_log)
         else
            if not (have_info(accode)) then
               is_a_state_between := false
         else
            if ((get_startyear1(accode) >= year1) and (get_startyear1(accode) <= year2)) or
               ((get_endyear1(accode) >= year1) and (get_endyear1(accode) <= year2)) or
               ((year1 >= get_startyear1(accode)) and (year1 <= get_endyear1(accode))) or
               ((year2 >= get_startyear1(accode)) and (year2 <= get_endyear1(accode))) or
               ((get_startyear2(accode) >= year1) and (get_startyear2(accode) <= year2)) or
               ((get_endyear2(accode) >= year1) and (get_endyear2(accode) <= year2)) or
               ((year1 >= get_startyear2(accode)) and (year1 <= get_endyear2(accode))) or
               ((year2 >= get_startyear2(accode)) and (year2 <= get_endyear2(accode)))
               then is_a_state_between := true;
      end;         {is a state between}

   function Tnation_array_obj.is_a_gp (accode : ccode_range; ayear : year_range) : boolean;
      var which : 1..max_gp_records;
      begin
         is_a_gp := false;
         if accode = 0 then is_a_gp := false
         else
            if not(initialized) then
               begin
                  EUGeneError ('Error - nation_array is_a_gp called before initializtion',
                          5, continue, error_log);
               end
         else
            if not (have_info(accode)) then is_a_gp := false
         else
            begin
               if is_a_state (accode, ayear) then
                  begin
                     for which := 1 to max_gp_records do
                        if ((get_gpstartyear(accode, which) <= ayear) and (get_gpendyear(accode, which) >= ayear))
                        then is_a_gp := true
                  end
               else is_a_gp := false;
            end;
      end;

   function Tnation_array_obj.is_a_gp_between (accode : ccode_range; year1, year2 : year_range) : boolean;
      var which : 1..max_gp_records;
          temp : year_range;
      begin
         {returns true if is a GP anytime between year 1 and year 2}
         is_a_gp_between := false;
         if year1 > year2 then
           begin
             temp := year1;
             year1 := year2;
             year2 := temp;
           end;
         if accode = 0 then is_a_gp_between := false
         else
            if not(initialized) then
               begin
                  EUGeneError ('Error - nation_array is_a_gp called before initializtion',
                          5, continue, error_log);
               end
         else
            if not (have_info(accode)) then is_a_gp_between := false
         else
            begin
               if is_a_state_between (accode, year1, year2) then
                  begin
                     for which := 1 to max_gp_records do
                        if ((get_gpstartyear(accode, which) >= year1) and (get_gpstartyear(accode, which) <= year2)) or
                           ((get_gpendyear(accode, which) >= year1) and (get_gpendyear(accode, which) <= year2)) or
                           ((year1 >= get_gpstartyear(accode, which)) and (year1 <= get_gpendyear(accode, which))) or
                           ((year2 >= get_gpstartyear(accode, which)) and (year2 <= get_gpendyear(accode, which)))
                        then is_a_gp_between := true;
                  end
               else is_a_gp_between := false;
            end;
      end;

   function Tnation_array_obj.have_2_start_years (accode : ccode_range) : boolean;
      begin
         have_2_start_years := false;
         if not(initialized) then
            begin
               EUGeneError ('Error - nation_array have 2 start years called before initializtion',
                       5, continue, error_log);
            end
         else
            if ((get_startyear2(accode) >= configuration.first_nation_year) and
                (get_startyear2(accode) <= configuration.last_nation_year)) then
            have_2_start_years := true;
      end;

   function Tnation_array_obj.have_info (accode : ccode_range) : boolean;
          {returns true if there is a record for this country, that is, we have some nation info.}
      begin
         if not(initialized) then
            begin
               EUGeneError ('Error - nation_array have_info called before initializtion',
                       5, continue, error_log);
            end
         else
            begin
               if ccode_index.index(accode) <> initialized_value then
                  have_info := true
               else have_info := false;
            end;
      end;

   procedure Tnation_array_obj.show_info (accode : ccode_range);
      begin
         if have_info (accode) then
            begin
               writeln ('ccode ',accode,': ',get_ccode(accode),'/', get_abbrev(accode),'/',
                 get_fullname (accode), '/', get_startyear1(accode), ' to ',get_endyear1(accode), ' and ',
                 get_startyear2 (accode), ' to ',get_endyear2(accode));
            end
         else writeln ('ccode ',accode,': Have no info on this ccode');
      end;

  function Tnation_array_obj.get_ccode_from_fullname (fullname : string): ccode_range;
     var ccode : ccode_range;
         found : boolean;
     begin
        result := 0;
        found := false;
        ccode := min_ccode;
        repeat
           if have_info(ccode) then
              if get_fullname(ccode) = fullname then
                 begin
                    result := ccode;
                    found := true;
                 end;
           inc(ccode);
        until (found) or (ccode > max_ccode);
     end;

  function Tnation_array_obj.get_ccode_from_abbrev (abbrev : string): ccode_range;
     var ccode : ccode_range;
         found : boolean;
     begin
        result := 0;
        found := false;
        ccode := min_ccode;
        repeat
           if have_info(ccode) then
              if lowercase(get_abbrev(ccode)) = lowercase(abbrev) then
                 begin
                    result := ccode;
                    found := true;
                 end;
           inc(ccode);
        until (found) or (ccode >= max_ccode);
     end;

  function Tnation_array_obj.get_country_years : longint;
     begin
        get_country_years := ncountry_years;
     end;

  function Tnation_array_obj.get_dyad_years : longint;
     begin
        get_dyad_years := ndyad_years;
     end;

  function Tnation_array_obj.get_ndyads : longint;
     begin
        get_ndyads := ndyads;
     end;

   function Tnation_array_obj.get_dyadic_duration (ccode1, ccode2 : ccode_range; ayear : year_range) : integer;
          {returns duration both members of dyads have been in existence.}
      var Relevant_start_year_cc1, Relevant_start_year_cc2: year_range;
      begin
         get_dyadic_duration := missing_value;
         if not(initialized) then
            begin
               EUGeneError ('Error - nation_array get dyadic duration called before initializtion',
                       5, continue, error_log);
            end
         else
            begin
               if (is_a_state(ccode1, ayear) and is_a_state(ccode2, ayear)) then
                 {am in a period of overlap}
                  begin
                    {Want to count years from the start date that is closest but before
                     the current year.  Because I checked is_a_state, I know there is overlap.}
                    if ((nation_list.get_startyear2(ccode1) <= ayear) and (have_2_start_years (ccode1))) then
                       Relevant_start_year_cc1 := nation_list.get_startyear2(ccode1) else
                       Relevant_start_year_cc1 := nation_list.get_startyear1(ccode1);
                    if ((nation_list.get_startyear2(ccode2) <= ayear) and (have_2_start_years (ccode2))) then
                       Relevant_start_year_cc2 := nation_list.get_startyear2(ccode2) else
                       Relevant_start_year_cc2 := nation_list.get_startyear1(ccode2);
                    get_dyadic_duration := ayear - max (Relevant_start_year_cc1, Relevant_start_year_cc2);
                 end;
            end;
      end;

   function Tnation_array_obj.country_date_overlap (ccode1, ccode2 : ccode_range): boolean;
       {compare the 1, 2 start/end year slots in all possible combos.  But ignore
        the comparison if they are not real slots}
       var overlap11, overlap12, overlap21, overlap22 : boolean;
       begin
          country_date_overlap := false;
          overlap11 := true;
          if (get_endyear1(ccode1) < get_startyear1(ccode2)) or
                          (get_startyear1(ccode1) > get_endyear1(ccode2)) then
                          overlap11 := false;
          overlap12 := true;
          if (get_endyear1(ccode1) < get_startyear2(ccode2)) or
                          (get_startyear1(ccode1) > get_endyear2(ccode2)) or
                          (get_startyear2(ccode2) = min_year) or
                          (get_endyear2(ccode2) = min_year) then
               overlap12 := false;
          overlap21 := true;
          if (get_endyear2(ccode1) < get_startyear1(ccode2)) or
                          (get_startyear2(ccode1) > get_endyear1(ccode2)) or
                          (get_startyear2(ccode1) = min_year) or
                          (get_endyear2(ccode1) = min_year) then
               overlap21 := false;
          overlap22 := true;
          if (get_endyear2(ccode1) < get_startyear2(ccode2)) or
                          (get_startyear2(ccode1) > get_endyear2(ccode2)) or
                          (get_startyear2(ccode1) = min_year) or
                          (get_endyear2(ccode1) = min_year) or
                          (get_startyear2(ccode2) = min_year) or
                          (get_endyear2(ccode2) = min_year) then
               overlap22 := false;
          if overlap11 or overlap12 or overlap21 or overlap22 then
             country_date_overlap := true
       end;

   function Tnation_array_obj.different_states (ccode1, ccode2 : ccode_range) : boolean;
      var done : boolean;
          x : integer;
      begin
         result := true;
         if (ccode1 = ccode2) then result := false
         else  {different ccode #s, but are they on the list of different #s that are actually same state?}
               {if on the list, then different_states also = false.}
            begin
               done := false;
               x := low(nations_with_different_ccode_data);
               {check first to see if ccode to check is in range of what was in the nations_with... list.}
               if (min(ccode1, ccode2) < nations_with_different_ccode_data[low(nations_with_different_ccode_data)].state1) then {it's OK}
                  begin
                     result := true;
                     done := true;
                  end;
               if min(ccode1, ccode2) > nations_with_different_ccode_data[high(nations_with_different_ccode_data)].state1 then {it's OK}
                  begin
                     result := true;
                     done := true;
                  end;
               if not done then
                  repeat
                     if min(ccode1, ccode2) = nations_with_different_ccode_data[x].state1 then
                        if max(ccode1, ccode2) = nations_with_different_ccode_data[x].state2 then
                           begin
                              result := false;
                              done := true;
                           end;
                     inc(x);
                     if (x > high(nations_with_different_ccode_data)) then done := true;
                  until (done);
            end;
      end;

   { -------------------------------------------------------------------------   }

   Procedure Read_one_user_data_file_info(user_data_config_filename : TFileName;
             var one_user_data_set_info : data_set_info_record; var baddataset : boolean);
      var config_file : text;
          num_vars_read, x, y : integer;
          found, badline : boolean;

      procedure read_process_user_config_line (var data_file : text;
                var one_user_data_set_info : data_set_info_record;
                var num_vars_read : integer; var bad_data_line : boolean);
             {this will read one line of the user data set configuration file,
              parse it, and if it's an appropriate user data set configuration
              variable, set it.}
         var env_name, contents : string;
             achar : char;

         procedure set_user_configuration_entry (env_name, contents : string;
                   var one_user_data_set_info: data_set_info_record;
                   var num_vars_read : integer; var bad_data_line : boolean);
            var tempstring : string;
                tempval : integer;
            begin
               bad_data_line := false;
               {Process the configuration entry name and rest of line.  }
               with one_user_data_set_info do     {because not ordinal, must do with if-then}
                  if env_name ='data_file_name' then file_name := configuration.user_data_files_directory+contents
                  else if env_name ='data_set_full_name' then
                     begin
                        if length(contents) > 45 then ShowMessage ('The descriptive name of the user data set described in file '+ user_data_config_filename+' is too long.  Only 45 characters will be kept.');
                        data_set_full_name := copy (contents, 0, 45);
                     end
                  else if env_name ='data_set_short_name' then
                     begin
                        if length(contents) > 18 then ShowMessage ('The short/abbreviated name of the user data set described in file '+ user_data_config_filename+' is too long.  Only 18 characters will be kept.');
                        data_set_short_name := copy (contents, 0, 18);
                     end
                  else if env_name ='data_set_citation' then
                     begin
                        data_set_citation := contents;
                     end
                  else if env_name ='unit_of_analysis' then
                     begin
                        if contents = 'country_year' then data_set_unit := country_year
                        else if contents = 'directed_dyad_year' then data_set_unit := directed_dyad_year
                        else if contents = 'nondirected_dyad_year' then data_set_unit := nondirected_dyad_year
                        else if contents = 'annual_data' then data_set_unit := annual_data
                        else {contents unrecognized}
                           begin
                              data_set_unit := no_unit_dataset;
                              MessageDlg ('Error in input user data configuration file '+ user_data_config_filename+
                                     '; unit of analysis is not recognized.  Continuing run;  further errors may result.  Check user data set configuration file.  Skipping user file.', mtwarning, [mbok], 0);
                              bad_data_line := true;
                           end;
                     end
                  else if env_name ='first_year_possible' then data_set_first_year_possible := strtoint(contents)
                  else if env_name ='last_year_possible' then data_set_last_year_possible := strtoint(contents)
                  else if env_name ='label_line_in_data_file' then
                     begin
                        if (contents = 'true') or (contents = '1') then label_line := true
                        else if (contents = 'false') or (contents = '0') then label_line := false
                        else
                           begin
                              messageDlg ('Error in input user data configuration file '+ user_data_config_filename+
                                     '; label line entry (true/false) is not recognized.  Continuing run;  further errors may result.  Check user data set configuration file.    Skipping user file.', mtwarning, [mbok], 0);
                              bad_data_line := true;
                           end;
                     end
                  else if env_name ='number_of_variables' then
                     begin
                        num_vars := strtoint(contents);
                        if num_vars > max_user_variables_per_data_set then
                           begin
                              MessageDlg ('More variables in input user data set described in '+ user_data_config_filename+
                                     ' than allowed by system.  Only '+inttostr(max_user_variables_per_data_set)+
                                     ' variables allowed;  only this many will be read.  Notify programmer that more variable space must be hardcoded for this user data file.  ', mtwarning, [mbok], 0);
                              setlength(one_user_data_set_info.var_info, max_user_variables_per_data_set);
                           end
                        else setlength(one_user_data_set_info.var_info, num_vars);
                     end
                  else if env_name ='number_of_cases' then
                     num_cases := strtoint(contents)
                  else if env_name ='variable' then
                     begin
                        if not ((num_vars > 0) and (one_user_data_set_info.data_set_unit <> no_unit_dataset)) then
                           begin
                              messageDlg ('Error in input user data configuration file '+ user_data_config_filename+
                                     ', line of variables observed before number of variables or dataset unit of analysis declared.  Recheck user data set specification file.  No more variable data will be read;  further errors may result.    Skipping user file.',mtwarning, [mbok], 0);
                              bad_data_line := true;
                           end
                        else
                           if num_vars > max_user_variables_per_data_set then
                              begin
                                 MessageDlg ('Error in input user data configuration file '+ user_data_config_filename+
                                        ', line of variables observed before number of variables declared.  Recheck user data set specification file.  No more variable data will be read;  further errors may result.  Skipping user file.',mtwarning, [mbok], 0);
                                 bad_data_line := true;
                              end
                        else    {all checks are OK, read variables.}
                           begin
                              inc(num_vars_read);

                              {extract name, then clear string through comma.}
                              one_user_data_set_info.var_info[num_vars_read-1].var_name := lowercase(copy(contents,1,pos(',',contents)-1));
                              delete(contents,1,pos(',',contents));

                              {extract type, then clear string through comma.}
                              tempstring := lowercase(copy(contents,1,pos(',',contents)-1));
                              delete(contents,1,pos(',',contents));
                              if tempstring = 'integer' then one_user_data_set_info.var_info[num_vars_read-1].var_type := varinteger
                                 else if tempstring = 'real' then one_user_data_set_info.var_info[num_vars_read-1].var_type := varsingle
                                 else if tempstring = 'string' then one_user_data_set_info.var_info[num_vars_read-1].var_type := varolestr
                                 else
                                    begin
                                       MessageDlg ('Error reading generic input file '+user_data_config_filename+': var type not varinteger, varsingle, or varolestr.    Skipping user file.', mtwarning, [mbok], 0);
                                       bad_data_line := true;
                                    end;

                              {extract unit for var, then clear string through comma.}
                              tempstring := lowercase(copy(contents,1,pos(',',contents)-1));
                              delete(contents,1,pos(',',contents));
                              if tempstring = 'identifierccode1' then one_user_data_set_info.var_info[num_vars_read-1].var_unit := identifierccode1
                                 else if tempstring = 'identifierccode2' then one_user_data_set_info.var_info[num_vars_read-1].var_unit := identifierccode2
                                 else if tempstring = 'identifieryear' then one_user_data_set_info.var_info[num_vars_read-1].var_unit := identifieryear
                                 else if tempstring = 'no_unit' then one_user_data_set_info.var_info[num_vars_read-1].var_unit := no_unit_variable
                                 else if tempstring = 'monadic' then one_user_data_set_info.var_info[num_vars_read-1].var_unit := monadic
                                 else if tempstring = 'dyadic_ordered' then one_user_data_set_info.var_info[num_vars_read-1].var_unit := dyadic_ordered
                                 else if tempstring = 'dyadic_unordered' then one_user_data_set_info.var_info[num_vars_read-1].var_unit := dyadic_unordered
                                 else if tempstring = 'annual' then one_user_data_set_info.var_info[num_vars_read-1].var_unit := annual
                                 else if tempstring = 'identifierversion' then one_user_data_set_info.var_info[num_vars_read-1].var_unit := identifierversion
                                 else
                                    begin
                                       MessageDlg ('Error reading generic input file '+user_data_config_filename+': unit identifier not correct.    Skipping user file.', mtwarning, [mbok], 0);
                                       bad_data_line := true;
                                    end;
                              {Now check this variable unit for consistency with data set.}
                              {Annual vars can be in any type of data set.
                               monadic vars can be in country_year, directed_dyad_year, nondirected_dyad_year datasets only.
                               dyadic_ordered vars can be in directed or nondir dyad_year datasets only.
                               dyadic_unordered can be in directed or nondirected dyadic only.}
                              case one_user_data_set_info.var_info[num_vars_read-1].var_unit of
                                 identifierccode1 : {can't be in annual dat set}
                                    if one_user_data_set_info.data_set_unit = annual_data then
                                       begin
                                          messageDlg('Error in user data specification input file '+ user_data_config_filename +
                                                  ':  A variable unit of "identifierccode1" was observed in input file for an annual data set.  This is invalid.  Check file and try again.  Continuing, but further errors may result.  Skipping user file.',mtwarning, [mbok], 0);
                                          bad_data_line := true;
                                       end;
                                 identifierccode2 : {can't be in annual or monadic data}
                                    if ((one_user_data_set_info.data_set_unit = annual_data) or (one_user_data_set_info.data_set_unit = country_year)) then
                                       begin
                                          messageDlg('Error in user data specification input file '+ user_data_config_filename +
                                                  ':  A variable unit of "identifierccode1" was observed in input file for an annual data set.  This is invalid.  Check file and try again.  Continuing, but further errors may result.  Skipping user file.',mtwarning, [mbok], 0);
                                          bad_data_line := true;
                                       end;
                                 no_unit_variable :
                                       begin
                                          messageDlg('Error in user data specification input file '+ user_data_config_filename +
                                                  ':  A variable unit of "no_unit" was observed in input file.  Check file and try again.  Continuing, but further errors may result.',mtwarning, [mbok], 0);
                                          bad_data_line := true;
                                       end;
                                 annual : begin end;   {can be in any type of data set}
                                 monadic : {can't be in annual data set only.}
                                    if one_user_data_set_info.data_set_unit = annual_data then
                                       begin
                                          messageDlg('Error in user data specification input file '+ user_data_config_filename +
                                                  ':  A variable unit of "monadic" was observed in input file for an annual data set.  This is invalid.  Check file and try again.  Continuing, but further errors may result.  Skipping user file.',mtwarning, [mbok], 0);
                                          bad_data_line := true;
                                       end;
                                 dyadic_ordered, dyadic_unordered  : {can only be in directed or nondirected dyadic data set, although either could be in either data set type.}
                                    if not ((one_user_data_set_info.data_set_unit = directed_dyad_year) or (one_user_data_set_info.data_set_unit = nondirected_dyad_year)) then
                                       begin
                                          messageDlg('Error in user data specification input file '+ user_data_config_filename +
                                                  ':  A variable unit of "dyadic_ordered" was observed in input file where the unit of analysis is not directed dyads.  This is invalid.  Check file and try again.  Continuing, but further errors may result.  Skipping user file.',mtwarning, [mbok], 0);
                                          bad_data_line := true;
                                       end;
                              end;   {case}

                              {extract var_reversed_var, then clear string through comma.}
                              tempstring := lowercase(copy(contents,1,pos(',',contents)-1));
                              delete(contents,1,pos(',',contents));
                              one_user_data_set_info.var_info[num_vars_read-1].var_reversed_var := tempstring;

                                                   {extract missing value}
                              {note that the last segment read is different.}
                              tempstring := lowercase(copy(contents,1,length(contents)));
                              if tempstring = 'none' then   {No user missing value}
                                 tempstring := '0';    {Internally, will know this is not really a missing value.}
                              tempval := strtoint(tempstring);
                              one_user_data_set_info.var_info[num_vars_read-1].var_missing_value := tempval;
                           end;    {all is OK, read variable info.}
                     end   {variable}

                  else
                     messageDlg('Error - unrecognized user data set configuration variable name seen, unrecognized name was ' + env_name
                                  + '.  Check configuration file for errors.  Attempted processing of this user configuration file will continue.', mtwarning, [mbok], 0);
            end;

         begin  {read process user config line}
            bad_data_line := false;
            {scan for various attributes: data file name, unit, var list, etc.}
            {Get first character}
            env_name := '';
            contents := '';
            repeat
               if (not eof(config_file)) and (not eoln(config_file)) then
               read (config_file,achar);
            until ( (achar <> ' ') and (achar <> chr(9))) or
                  ( (eof(config_file)) or (eoln(config_file)) ) ;
            if (achar = '[') or (achar = ';') or (eof(config_file)) or (eoln(config_file)) then
               begin
                   {it's a comment line, or a blank line, so skip it}
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

                  if achar = '"' then   {now read the rest of the line, up to the next "}
                  repeat
                     if (not eof(config_file)) and (not eoln(config_file)) then
                        read (config_file,achar);
                     if (achar <> '"') then contents := contents + achar;
                  until (achar = '"') or (eof(config_file)) or (eoln(config_file)) ;

                  set_user_configuration_entry (env_name, contents, one_user_data_set_info, num_vars_read, bad_data_line);

               end;
            if (not eof(config_file)) then readln (config_file);
         end;     {read and process a config file line}


      begin   {read user data file info}
         try
            baddataset := false;
            if not (FileExists (user_data_config_filename)) then
               begin
                  messageDlg ('Error - User Data Configuration file '+user_data_config_filename+
                         ' not found.  Programming error - notify programmer.  Skipping user data file.', mterror, [mbok], 0);
                  baddataset := true;
               end;
            try
               {Initialize values, then read from file}
               with one_user_data_set_info do
                  begin
                     config_file_name := 'BLANK';
                     file_name := 'BLANK';
                     data_set_full_name := 'No full name';
                     data_set_short_name := 'No short name';
                     data_set_citation := 'No citation';
                     data_set_unit := no_unit_dataset;
                     data_set_first_year_possible := min_year;
                     data_set_last_year_possible := min_year;
                     label_line := false;
                     num_vars := 0;
                     num_cases := 0;
                     setlength(var_info,0);
                  end;

               assignFile (config_file, user_data_config_filename);
               reset (config_file);
               one_user_data_set_info.config_file_name := user_data_config_filename;
               num_vars_read := 0;
               repeat
                  read_process_user_config_line (config_file, one_user_data_set_info, num_vars_read, badline);
               until eof(config_file);

               if badline then baddataset := true;

               {Now perform checks on the user data specification file.}
               if not(num_vars_read = one_user_data_set_info.num_vars) and
                     (one_user_data_set_info.num_vars > 0) then
                  begin
                     MessageDlg ('Error in user data file configuration file '+
                            user_data_config_filename +'.  Number of actual variable lines does not match number specified in configuration file, or number of variables specified not > 0.  Program continues;  check configuration file for errors.  Skipping user data file.', mtWarning, [mbOK],0);
                     baddataset := true;
                  end;

               if not (one_user_data_set_info.num_cases > 0) then
                  begin
                     MessageDlg ('Error in user data file configuration file '+
                            user_data_config_filename +'.  Number of cases either not specified or not > 0.  Program continues;  check configuration file for errors.  Skipping user data file.',mtWarning, [mbOK],0);
                     baddataset := true;
                  end;

               if not fileexists (one_user_data_set_info.file_name) then
                  begin
                     MessageDlg ('Note:  User data file configuration file '+
                            user_data_config_filename +' specifies a data file ('+one_user_data_set_info.file_name+') that does not exist.  Check configuration file for errors.  This user data file will be skipped.  To prevent this user data set from being read by EUGene, delete the .edf file listed above.',mtinformation, [mbOK],0);
                     baddataset := true;
                  end;

               {Now check reverse variable in non-directed data set.}
               if one_user_data_set_info.data_set_unit = nondirected_dyad_year then
               for x := 0 to one_user_data_set_info.num_vars - 1 do
                  if (one_user_data_set_info.var_info[x].var_unit=dyadic_ordered) or
                     (one_user_data_set_info.var_info[x].var_unit=monadic) then
                  {The reverse variable must exist}
                     begin
                        y := 0;
                        found := false;
                        while not(found) and (y < one_user_data_set_info.num_vars) do
                           begin
                              if one_user_data_set_info.var_info[y].var_name = one_user_data_set_info.var_info[x].var_reversed_var then
                                 found := true;
                                   {OK!  reverse var found}
                              inc(y);
                           end;
                        if not found then
                           begin
                              messageDlg ('Error in dyadic user data file configuration file '+
                                 user_data_config_filename +'.  At least one variable is ordered or monadic, but the data set does not contain a reverse variable.  This may result in further program errors or erroneous output - proceed with caution.    Skipping user data file.',mtWarning, [mbOK], 0);
                              baddataset := true;
                           end;
                     end;

            finally
               CloseFile (config_file);
            end;
         except
            on EInOutError do
               begin
                  messageDlg ('Error - Cannot open user configuration file '+user_data_config_filename+
                         '.    Skipping user data file.', mtError, [mbOK], 0);
                  baddataset := true;
               end;
         end;
      end;    {read user data file}

   procedure write_one_user_data_file_info(user_data_config_filename : TFileName;
             one_user_data_set_info : data_set_info_record);
      var config_file : text;
          outstring : string;
          avar : integer;
          badtype, badvarunit : boolean;
      begin
         try
            assignFile (config_file, user_data_config_filename);
            rewrite (config_file);
            badtype := false;
            badvarunit := false;

            {Write a series of configuration lines}
            writeln (config_file, '[User data set configuration file for EUGene software]');
            writeln (config_file, '[Configuration file '+ExtractFileName(user_data_config_filename)+']');

            writeln (config_file, 'data_file_name = "'+ExtractFileName(one_user_data_set_info.file_name)+'"');
            writeln (config_file, 'data_set_full_name = "'+one_user_data_set_info.data_set_full_name+'"');
            writeln (config_file, 'data_set_short_name = "'+one_user_data_set_info.data_set_short_name+'"');
            writeln (config_file, 'data_set_citation = "'+one_user_data_set_info.data_set_citation+'"');
            case one_user_data_set_info.data_set_unit of
                  annual_data : outstring := 'annual_data';
                  country_year : outstring := 'country_year';
                  directed_dyad_year : outstring := 'directed_dyad_year';
                  nondirected_dyad_year : outstring := 'nondirected_dyad_year';
                  else
                     begin
                        outstring := 'bad_unit';
                        EUGeneError ('Programming error in outputting user data configuration file '+ user_data_config_filename+
                                           '; unit of analysis was not set.  ', 3, continue, error_log);
                     end;
               end;   {case}
            writeln (config_file, 'unit_of_analysis = "'+outstring+'"');
            writeln (config_file, 'first_year_possible = "'+inttostr(one_user_data_set_info.data_set_first_year_possible)+'"');
            writeln (config_file, 'last_year_possible = "'+inttostr(one_user_data_set_info.data_set_last_year_possible)+'"');
            if one_user_data_set_info.label_line = true then
               writeln (config_file, 'label_line_in_data_file = "true"') else
               writeln (config_file, 'label_line_in_data_file = "false"');
            writeln (config_file, 'number_of_variables = "'+inttostr(one_user_data_set_info.num_vars)+'"');
            writeln (config_file, 'number_of_cases = "'+inttostr(one_user_data_set_info.num_cases)+'"');

            for avar := 0 to one_user_data_set_info.num_vars-1 do
               begin
                  write (config_file, 'variable = "');
                  write (config_file, one_user_data_set_info.var_info[avar].var_name+',');
                  case one_user_data_set_info.var_info[avar].var_type of
                        varinteger : outstring := 'integer';
                        varsingle : outstring := 'real';
                        varolestr : outstring := 'string'
                        else
                           begin
                              outstring := 'bad_type';
                              badtype := true;
                           end;
                     end;   {case}
                  write (config_file, outstring+',');
                  case one_user_data_set_info.var_info[avar].var_unit of
                        identifierccode1 : outstring := 'identifierccode1';
                        identifierccode2 : outstring := 'identifierccode2';
                        identifieryear : outstring := 'identifieryear';
                        identifierversion : outstring := 'identifierversion';
                        monadic : outstring := 'monadic';
                        dyadic_ordered : outstring := 'dyadic_ordered';
                        dyadic_unordered : outstring := 'dyadic_unordered';
                        annual : outstring := 'annual'
                        else
                           begin
                              outstring := 'no_unit';
                              badvarunit := true;
                           end;
                     end;   {case}
                  write (config_file, outstring+',');

                  write (config_file, one_user_data_set_info.var_info[avar].var_reversed_var+',');
                  write (config_file, inttostr(one_user_data_set_info.var_info[avar].var_missing_value)+'"');
                  writeln (config_file);
               end;   {for avar}
            if badtype then ShowMessage ('Error writing config file - var type not set to integer, real, string for at least one variable.  Data file may not be read properly.');
            if badvarunit then ShowMessage ('Error writing configuration file - var unit not set appropriately for at least one variable.  Data file may not be read properly.');

            closefile (config_file);
            ShowMessage ('Configuration file successfully saved!');
         except
            ShowMessage ('An error occured while writing configuration file.  Check file specified for output and try again.');
         end;   {except}

      end;    {proc write one user data file}


   constructor Tuser_data_set_listing_obj.init (data_path: string);
         {init just constructs the array.  Update must be called to put names in.  }
      begin
         SetLength (user_data_set_info, 0);
         update (data_path);
      end;

   procedure Tuser_data_set_listing_obj.update (data_path: string);
         {init reads (or rereads) basic data about data sets from user data set directory, then
          from identified external definition files.}
      var File_found_rec: TSearchRec;
          file_result : integer;
          num_user_data_files, final_num_data_sets, user_data_file_storage_location,
          user_data_file_loop : integer;
          baddatasetfile : boolean;
      begin            {update main procedure}
         SetLength (user_data_set_info, 0);
         baddatasetfile := false;
         {First scan user data subdirectory for EUGene data configuration files}
         if DirectoryExists (data_path) then
            begin
               {First count files}
               num_user_data_files := 0;
               file_result := FindFirst(data_path+'\*.'+eugene_data_file_extension, 0, File_found_rec);
               while file_result = 0 do
                  begin
                     inc(num_user_data_files);
                     file_result := FindNext(File_found_rec);
                  end;
               SetLength (user_data_set_info, num_user_data_files);
               final_num_data_sets := num_user_data_files;

               {Files now counted.  Loop through them.}
               user_data_file_storage_location := 0;
               for user_data_file_loop := 0 to num_user_data_files-1 do
                  begin
                     if user_data_file_loop = 0 then
                        file_result := FindFirst(data_path+'\*.'+eugene_data_file_extension, 0, File_found_rec)
                     else
                        file_result := FindNext(File_found_rec);
                     {Now, read each config file for data set information}
                     {Name in File_found_rec.name;}
                     trace.message('      Reading user data set configuration file '+data_path+File_found_rec.name+'...');
                     Read_one_user_data_file_info(data_path+File_found_rec.name, user_data_set_info[user_data_file_storage_location], baddatasetfile);
                     inc (user_data_file_storage_location);
                     if (baddatasetfile) then
                        begin
                           {Now, if this file is bad, then decrease the storage index
                            so that the next file will overwrite it.  }
                           dec (user_data_file_storage_location);
                           dec(final_num_data_sets);
                        end;
                  end;
               SetLength (user_data_set_info, final_num_data_sets);

               {Finally, release memory allocated by find first, find next procedures}
               sysutils.FindClose(File_found_rec);
            end;   {directory exists}
       end;

   destructor Tuser_data_set_listing_obj.destroy;
      var x,y : integer;
      begin
         for x := 0 to length (user_data_set_info) - 1 do
            begin
                for y := 0 to user_data_set_info[x].num_vars - 1 do
                   user_data_set_info[x].var_info := nil;
            end;
         user_data_set_info := nil;
      end;

   function Tuser_data_set_listing_obj.update_changes (data_path : string) : integer;
      {searches path again, checks names of data files.  If no changes,
       returns 0.  If new files, add at end
       and return 1.  If files deleted, or other changes that might invalidate
       any user selections, return 2 so calling proc will reread.  }
      var x : integer;
          files_have_changed, onlist : boolean;
          File_found_rec: TSearchRec;
          file_result : integer;
          num_existing_files, num_user_data_files : integer;
          user_data_file_loop, user_data_file_storage_location : integer;
          final_num_data_sets : integer;
          baddatasetfile : boolean;
      begin
         update_changes := 0;
         if (DirectoryExists (data_path)) then
            begin
               {Go through each in user list, make sure they're still here.}
               files_have_changed := false;
               num_existing_files := length(user_data_set_info);
               for x := 0 to num_existing_files-1 do
                  {if this config and data file still exists, then OK}
                  if (fileexists(get_config_file_name(x))) and
                     (fileexists(get_data_set_file_name(x))) then
                     begin end   {ok}
                  else
                     files_have_changed := true;
               {Also check # files.  If more files, need to add.  If less files,
                need to totally reread.}
               {Scan user data subdirectory for EUGene data configuration files and count.}
               num_user_data_files := 0;
               file_result := FindFirst(data_path+'\*.'+eugene_data_file_extension, 0, File_found_rec);
               while file_result = 0 do
                  begin
                     inc(num_user_data_files);
                     file_result := FindNext(File_found_rec);
                  end;
               {Now have counted and checked names.
                If any names in main list changed, must reread all.  Or, if files
                have been deleted, must reread all.}
               if (num_user_data_files < num_existing_files) or
                  (files_have_changed) then   {must reread files!}
                     begin
                        update_changes := 2;
                        update(data_path);
                     end
               else
                  if (num_user_data_files > num_existing_files) then
                     {have more user files, must add the new file !at end!.}
                     begin
                        update_changes := 1;
                        {Files now counted.  Loop through them.}
                        SetLength (user_data_set_info, num_user_data_files);
                        final_num_data_sets := num_user_data_files;
                        {set new storage location to 1 past existing files, which
                         is the index at the # of files (rather than -1) }
                        user_data_file_storage_location := num_existing_files;
                        for user_data_file_loop := 0 to num_user_data_files-1 do
                           begin
                              if user_data_file_loop = 0 then
                                 file_result := FindFirst(data_path+'\*.'+eugene_data_file_extension, 0, File_found_rec)
                              else
                                 file_result := FindNext(File_found_rec);
                              {If this file is already on list, OK.  If not, add it as new end record.
                               Check if it exists on list.}
                              baddatasetfile := false;
                              onlist := false;
                              for x := 0 to num_existing_files-1 do
                                 if file_found_rec.name = ExtractFileName(get_config_file_name(x)) then
                                    begin
                                       onlist := true;
                                    end;   {ok}
                              if not(onlist) then
                                 {Now need to add at end}
                                 begin
                                    trace.message('      Reading user data set configuration file '+data_path+File_found_rec.name+'...');
                                    Read_one_user_data_file_info(data_path+File_found_rec.name, user_data_set_info[user_data_file_storage_location], baddatasetfile);
                                    inc (user_data_file_storage_location);
                                    if (baddatasetfile) then
                                       begin
                                          {Now, if this file is bad, then decrease the storage index
                                           so that the next file will overwrite it.  }
                                          dec (user_data_file_storage_location);
                                          dec(final_num_data_sets);
                                       end;
                                 end;
                           end;    {for x 1 to user data files on disk}
                        SetLength (user_data_set_info, final_num_data_sets);

                     end;       {add new files at end}
                     {Finally, release memory allocated by find first, find next procedures}
                     sysutils.FindClose(File_found_rec);
            end;   {directory exists}
      end;

   function Tuser_data_set_listing_obj.get_num_data_sets: integer;
      begin
         get_num_data_sets := length(user_data_set_info);
      end;

   function Tuser_data_set_listing_obj.get_config_file_name (data_set_num : integer): TFileName;
      begin
         get_config_file_name := user_data_set_info[data_set_num].config_file_name;
      end;

   {Note that for all gets, the data files are indexed 0..length-1, so calling
    routines need to call with appropriate index number.}

   function Tuser_data_set_listing_obj.get_data_set_full_data_record (data_set_num : integer): data_set_info_record;
      begin
         result := user_data_set_info[data_set_num];
      end;

   function Tuser_data_set_listing_obj.get_data_set_short_name (data_set_num : integer): string;
      begin
         result := user_data_set_info[data_set_num].data_set_short_name;
      end;

   function Tuser_data_set_listing_obj.get_data_set_full_name (data_set_num : integer): string;
      begin
         result := user_data_set_info[data_set_num].data_set_full_name;
      end;

   function Tuser_data_set_listing_obj.get_data_set_citation (data_set_num: integer): string;
      begin
         result := user_data_set_info[data_set_num].data_set_citation;
      end;

   function Tuser_data_set_listing_obj.get_data_set_file_name (data_set_num : integer): TFileName;
      begin
         result := user_data_set_info[data_set_num].file_name;
      end;

   function Tuser_data_set_listing_obj.get_data_set_first_year_possible (data_set_num : integer): year_range;
      begin
         result := user_data_set_info[data_set_num].data_set_first_year_possible;
      end;

   function Tuser_data_set_listing_obj.get_data_set_last_year_possible (data_set_num : integer): year_range;
      begin
         result := user_data_set_info[data_set_num].data_set_last_year_possible;
      end;

   function Tuser_data_set_listing_obj.get_data_set_unit (data_set_num : integer): dataset_unit_of_analysis_type;
      begin
         result := user_data_set_info[data_set_num].data_set_unit;
      end;

   function Tuser_data_set_listing_obj.get_data_set_num_vars (data_set_num : integer): integer;
      begin
         result := user_data_set_info[data_set_num].num_vars;
      end;

   function Tuser_data_set_listing_obj.get_data_set_num_cases (data_set_num : integer): integer;
      begin
         result := user_data_set_info[data_set_num].num_cases;
      end;

   function Tuser_data_set_listing_obj.get_data_set_label_line (data_set_num : integer): boolean;
      begin
         result := user_data_set_info[data_set_num].label_line;
      end;

   function Tuser_data_set_listing_obj.get_data_set_var_name (data_set_num, var_num : integer): string;
      begin
         result := user_data_set_info[data_set_num].var_info[var_num].var_name;
      end;

   procedure Tuser_data_set_listing_obj.reset_data_set_var_name (data_set_num, var_num : integer; new_name : string);
      begin
         user_data_set_info[data_set_num].var_info[var_num].var_name := new_name;
      end;

   function Tuser_data_set_listing_obj.get_data_set_var_type (data_set_num, var_num : integer): integer;
      begin
         result := user_data_set_info[data_set_num].var_info[var_num].var_type;
      end;

   function Tuser_data_set_listing_obj.get_data_set_var_unit (data_set_num, var_num : integer): variable_unit_of_analysis_type;
      begin
         result := user_data_set_info[data_set_num].var_info[var_num].var_unit;
      end;

   function Tuser_data_set_listing_obj.get_data_set_var_missing_value (data_set_num, var_num : integer): integer;
      begin
         result := user_data_set_info[data_set_num].var_info[var_num].var_missing_value;
      end;

   function Tuser_data_set_listing_obj.get_data_set_var_reversed_var (data_set_num, var_num : integer): string;
      begin
         result := user_data_set_info[data_set_num].var_info[var_num].var_reversed_var;
      end;

   function Tuser_data_set_listing_obj.get_data_set_var_number (data_set_num : integer; var_name : string): integer;
      var curr_var : integer;
      begin
         curr_var := 0;
         while (get_data_set_var_name(data_set_num, curr_var) <> var_name) and
               (curr_var < get_data_set_num_vars(data_set_num)-1) do
            inc(curr_var);
         if get_data_set_var_name(data_set_num, curr_var) <> var_name then
            EUGeneError ('Error in generic set up vars - Could not find variable name '+ var_name + ' in configuration information '
               + ' when trying to find number, for data set number '+inttostr(data_set_num)
               + '.  Programming error - notify programmer', 3, stop, error_log)
         else {this is the right number for this variable}
            result := curr_var;
      end;   {get var number}

   { -------------------------------------------------------------------------   }

   function can_get_var_value_given_output_unit_and_input_data (const data_set_num, config_var_num : integer; user_selections : user_selection_type) : boolean;
     {This checks to make sure the combination of input data structure,
      variable choice, and output unit work to be able to output data.
     {This needs to return false if variable is dyadic and user wants monadic data.
      Also false if input data is dyadic and output is monadic.
?      or return false if var is directed dyadic and user wants non-directed data.?}
      begin
         result := false;
         case configuration.User_data_set_info.get_data_set_var_unit(data_set_num,config_var_num) of
            identifierversion : result := true;
            annual : begin
                  case user_selections.output_this of
                        output_directed_dyads, output_directed_dispute_initiation_dyads, output_nondirected_dispute_dyads, output_nondirected_dyads : result := true;
                        output_monads : if (configuration.User_data_set_info.get_data_set_unit(data_set_num) = country_year) or
                                           (configuration.User_data_set_info.get_data_set_unit(data_set_num) = annual_data)
                                        then result := true
                                        else result := false;
                     else EUGeneError ('Output type not recognized in function can_get_var_value_given_output_unit_and_input_data - notify programmer.', 5, stop, error_log);
                     end;   {case output_type of...}
               end;
            monadic : begin
                  case user_selections.output_this of
                        output_directed_dyads, output_directed_dispute_initiation_dyads, output_nondirected_dispute_dyads, output_nondirected_dyads : result := true;
                        output_monads : if configuration.User_data_set_info.get_data_set_unit(data_set_num) = country_year then result := true
                                        else result := false;
                     else EUGeneError ('Output type not recognized in function can_get_var_value_given_output_unit_and_input_data - notify programmer.', 5, stop, error_log);
                     end;   {case output_type of...}
               end;
            dyadic_ordered : begin
                  case user_selections.output_this of
                        output_directed_dyads, output_directed_dispute_initiation_dyads, output_nondirected_dispute_dyads, output_nondirected_dyads : result := true;
                        output_monads : result := false;
                     else EUGeneError ('Output type not recognized in function can_get_var_value_given_output_unit_and_input_data - notify programmer.', 5, stop, error_log);
                     end;   {case output_type of...}
               end;
            dyadic_unordered : begin
                  case user_selections.output_this of
                        output_directed_dyads, output_directed_dispute_initiation_dyads, output_nondirected_dispute_dyads, output_nondirected_dyads : result := true;
                        output_monads : result := false;
                     else EUGeneError ('Output type not recognized in function can_get_var_value_given_output_unit_and_input_data - notify programmer.', 5, stop, error_log);
                     end   {case output_type of...}
                  end;
               else EUGeneError ('Appropriate variable type for output not recognized in function can_get_var_value_given_output_unit_and_input_data - notify programmer.', 5, stop, error_log);
               end;   {case}
      end;


   { -------------------------------------------------------------------------   }


   {Methods for raw capability data}
   constructor Traw_capability_array_obj.init (a_file_name,
               modified_data_file_name : TFileName;
               first_proc_year, last_proc_year : year_range);
         {init reads in capability information from raw data.}
      var infile : text;
          ccode_loop, ccode_read : ccode_range;
          year_loop, year_read : year_range;
          cap_year_array : rawcap_array_ptr;       {array[ccode_index_range] of 6 part record}
          ccode_year_data : raw_cap_data_ptr;
          index : ccode_index_range;
          aval : longint;
          stringval : string;
          temp : year_range;
          start_mem, one_cap_year_mem_needed, all_years_mem_needed, heapneeded : longint;
          data_struct : rawcap_year_array_ptr;
          record_count : longint;
          Raw_trace : TTrace_obj;

      begin
         try
            Raw_trace := nil;
            Raw_trace := TTrace_obj.init(trace.get_trace_level);
            trace.enter ('Initializing raw capability data, '+inttostr(first_proc_year)+' to '
                         +inttostr(last_proc_year));
            start_mem := memavail;
            if first_proc_year > last_proc_year then
               switch_year (first_proc_year, last_proc_year);
            self.first_partition_year := first_proc_year;
            self.last_partition_year := last_proc_year;

            {this procedure needs existing ccode index and nation info to work.}
            created := false;
            if not(ccode_index.initialized) then
               begin
                  EUGeneError ('raw capability  array creation called before ccode_index initialized',
                                  5, stop, error_log);
               end;
            if not(nation_list.initialized) then
               begin
                  EUGeneError ('raw capability  array creation called before nation_list initialized',
                                  10, stop, error_log);
               end;

            {Initial matrix construction.  create cells/arrays for countries that are states
             in this time period}
            {First check available memory.}
            one_cap_year_mem_needed := Traw_capability_array_obj_mem_per_year;
            all_years_mem_needed := (last_proc_year-first_proc_year+1) *
                                    one_cap_year_mem_needed;
            heapneeded := Traw_capability_array_obj_mem_overhead + all_years_mem_needed;
            if debug[4] then
               begin
                  trace.message ('Capability array size calculation');
                  trace.message ('Calc is that '+inttostr(one_cap_year_mem_needed)+' needed per year.');
                  trace.message ('Calc is that '+inttostr((last_partition_year-first_partition_year+1))+' years needed.');
                  trace.message ('Calc is that '+inttostr(all_years_mem_needed)+' needed for all years.');
                  trace.message ('Max avail mem block is '+inttostr(MaxAvail));
               end;
            if MaxAvail <= (heapneeded) then
               begin
                  EUGeneError ('Not enough memory for raw capability array.',
                                  10, stop, error_log);
               end;

            new (data_struct);
            data := data_struct;
            for year_loop := min_year to max_year do
               begin
                  Raw_trace.tick ('Executing Procedure: Initialize Raw Capability Data',(max_year-min_year+1));
                  if (year_loop >= first_proc_year) and (year_loop <= last_proc_year) then
                  begin
                     new(cap_year_array);
                     data^[year_loop] := cap_year_array;
                     for ccode_loop := min_ccode to max_ccode do
                        begin
                           if nation_list.is_a_state_between(ccode_loop, first_proc_year, last_proc_year) then
                                {Note - initialize countries here.  Later, when read, might also
                                 have to add some new if there are some in the data that aren't
                                 in the "is_a_state_between" list based on ccode dates.}
                              begin
                                 new(ccode_year_data);
                                 ccode_year_data^.tpop := initialized_value;
                                 ccode_year_data^.upop := initialized_value;
                                 ccode_year_data^.energy:= initialized_value;
                                 ccode_year_data^.irst := initialized_value;
                                 ccode_year_data^.milper := initialized_value;
                                 ccode_year_data^.milex := initialized_value;
                                 data^[year_loop]^[ccode_index.index(ccode_loop)] := ccode_year_data;
                              end
                           else
                              data^[year_loop]^[ccode_index.index(ccode_loop)] := nil;
                        end;
                  end  {if year in range}
                  else data^[year_loop] := nil;
               end;    {for year_loop}
            if debug[2] then
               trace.message ('Finished raw capability array initialization;  reading input file.');
            Raw_trace.tickdone;

            try
               try
                  {Now, read in data from file}
                  assignFile (infile, a_file_name);
                  reset (infile);
                  record_count := 0;
                  {Skip header line}
                  readln (infile);
                      {Note:  file may not be sorted, so must go through whole file}
                  while not eof (infile) do
                    begin
                     if not eoln(infile) then    {This cmd should make it skip blank lines.}
                       begin
                        Raw_trace.tick ('Executing Procedure: Read Raw Capability Data ',12000);
                        inc(record_count);
                        {state abbrev is first}
                        stringval := read_csv_string (infile);
                        aval := read_csv_int (infile);
                        if (aval <= max_ccode) and (aval >= min_ccode) then
                           ccode_read := aval
                        else
                           EUGeneError ('Error reading ccode from raw capabilities - ccode value of '
                                  + inttostr(ccode_read) + ' out of range in record ' + inttostr(record_count) + '.',
                                        4, continue, error_log);
                        aval := read_csv_int (infile);
                        if (aval <= max_year) and (aval >= min_year) then
                           year_read := aval
                        else
                           EUGeneError ('Error reading year from raw capabilities - year value of '
                                  + inttostr(year_read) + ' out of range in record ' + inttostr(record_count) + '.',
                                        4, continue, error_log);

                        {Have ccode and year of this record.  If I want this ccode-year in this pass,
                         read the rest of the record, otherwise skip it. }
                        if (year_read >= first_proc_year) and (year_read <= last_proc_year) then
                           begin
                              {Check to be sure I have a record already created for this year-ccode combo.}
                              if data^[year_read]^[ccode_index.index(ccode_read)] = nil then
                                 begin
                                    new(ccode_year_data);
                                    data^[year_read]^[ccode_index.index(ccode_read)] := ccode_year_data;
                                 end;
                              {In capabil v 3.0, order has changed to
                                 stateabb	ccode	year	irst	milex	milper	energy	tpop	upop	cinc	version}
                              data^[year_read]^[ccode_index.index(ccode_read)]^.irst := read_csv_longint (infile);
                              data^[year_read]^[ccode_index.index(ccode_read)]^.milex := read_csv_longint (infile);
                              data^[year_read]^[ccode_index.index(ccode_read)]^.milper := read_csv_longint (infile);
                              data^[year_read]^[ccode_index.index(ccode_read)]^.energy := read_csv_longint (infile);
                              data^[year_read]^[ccode_index.index(ccode_read)]^.tpop := read_csv_longint (infile);
                              data^[year_read]^[ccode_index.index(ccode_read)]^.upop := read_csv_longint (infile);

                              {don't bother reading cinc (recomputed here) and version.}

                              if (data^[year_read]^[ccode_index.index(ccode_read)]^.milper = -9) or
                                 (data^[year_read]^[ccode_index.index(ccode_read)]^.milper = -99) then
                                 data^[year_read]^[ccode_index.index(ccode_read)]^.milper := missing_value;
                              if (data^[year_read]^[ccode_index.index(ccode_read)]^.milex = -9) or
                                 (data^[year_read]^[ccode_index.index(ccode_read)]^.milex = -99) then
                                 data^[year_read]^[ccode_index.index(ccode_read)]^.milex := missing_value;
                              if (data^[year_read]^[ccode_index.index(ccode_read)]^.energy = -9) or
                                 (data^[year_read]^[ccode_index.index(ccode_read)]^.energy = -99) then
                                 data^[year_read]^[ccode_index.index(ccode_read)]^.energy := missing_value;
                              if (data^[year_read]^[ccode_index.index(ccode_read)]^.irst = -9) or
                                 (data^[year_read]^[ccode_index.index(ccode_read)]^.irst = -99) then
                                 data^[year_read]^[ccode_index.index(ccode_read)]^.irst := missing_value;
                              if (data^[year_read]^[ccode_index.index(ccode_read)]^.upop = -9) or
                                 (data^[year_read]^[ccode_index.index(ccode_read)]^.upop = -99) then
                                 data^[year_read]^[ccode_index.index(ccode_read)]^.upop := missing_value;
                              if (data^[year_read]^[ccode_index.index(ccode_read)]^.tpop = -9) or
                                 (data^[year_read]^[ccode_index.index(ccode_read)]^.tpop = -99) then
                                 data^[year_read]^[ccode_index.index(ccode_read)]^.tpop := missing_value;
                           end    {year_read in appropriate range}
                        else
                           begin   {not in appropriate partition/year range}
                               {Don't want this record, do nothing}
                           end;
                            {Set to read next record, if there is one.}
                       end;   {if not eoln then ...}
                       if not eof(infile) then readln(infile);
                    end;     {while not eof infile}
               finally
                  CloseFile (infile);
                  Raw_Trace.tickdone;
               end;
            except
               on EUserInterrupt do raise;
               on EInOutError do
                 begin
                    FileErrorBox.maindo ('Error opening file "'+a_file_name+ '"',
                                         'File could not be opened for input.',
                                         'File may be in use by another program, or may be missing.');
                    FileErrorBox.showmodal;
                    raise;
                 end;
            end;

            try
               {Now, read in any modifications to the data from another input file}
               if (user_selections.capability_modifications = modified_capability) then
               begin
                  try
                     assignFile (infile, modified_data_file_name);
                     reset (infile);
                     if not eof(infile) then
                        begin
                           {Skip header line}
                           readln (infile);
                           record_count := 0;
                               {Note:  file may not be sorted, so must go through whole file}
                           while not eof (infile) do
                             begin
                              if not eoln(infile) then    {This cmd should make it skip blank lines.}
                                begin
                                 Raw_trace.tick ('Executing Procedure: Read Modified Capability Data ',0);
                                 inc(record_count);
                                 {state abbrev is first}
                                 stringval := read_csv_string (infile);
                                 aval := read_csv_int (infile);
                                 if (aval <= max_ccode) and (aval >= min_ccode) then
                                    ccode_read := aval
                                 else
                                    EUGeneError ('Error reading ccode from modified raw capabilities - ccode value of '
                                           + inttostr(ccode_read) + ' out of range in record ' + inttostr(record_count) + '.',
                                                 4, continue, error_log);
                                 aval := read_csv_int (infile);
                                 if (aval <= max_year) and (aval >= min_year) then
                                    year_read := aval
                                 else
                                    EUGeneError ('Error reading year from raw capabilities - year value of '
                                           + inttostr(year_read) + ' out of range in record ' + inttostr(record_count) + '.',
                                                 4, continue, error_log);

                                 {Have ccode and year of this record.  If I want this ccode-year in this pass,
                                  read the rest of the record, otherwise skip it. }
                                 if (year_read >= first_proc_year) and (year_read <= last_proc_year) then
                                    begin
                                       {Check to be sure I have a record already created for this year-ccode combo.}
                                       if data^[year_read]^[ccode_index.index(ccode_read)] = nil then
                                          begin
                                             new(ccode_year_data);
                                             data^[year_read]^[ccode_index.index(ccode_read)] := ccode_year_data;
                                          end;
                                       {In capabil v 3.0, order has changed to
                                          stateabb	ccode	year	irst	milex	milper	energy	tpop	upop	cinc	version}
                                       data^[year_read]^[ccode_index.index(ccode_read)]^.irst := read_csv_longint (infile);
                                       data^[year_read]^[ccode_index.index(ccode_read)]^.milex := read_csv_longint (infile);
                                       data^[year_read]^[ccode_index.index(ccode_read)]^.milper := read_csv_longint (infile);
                                       data^[year_read]^[ccode_index.index(ccode_read)]^.energy := read_csv_longint (infile);
                                       data^[year_read]^[ccode_index.index(ccode_read)]^.tpop := read_csv_longint (infile);
                                       data^[year_read]^[ccode_index.index(ccode_read)]^.upop := read_csv_longint (infile);

                                       {don't bother reading cinc (recomputed here) and version.}

                                       {Note:  if add read to end of record, need to change below read eoln command}

                                       if (data^[year_read]^[ccode_index.index(ccode_read)]^.milper = -9) or
                                          (data^[year_read]^[ccode_index.index(ccode_read)]^.milper = -99) then
                                          data^[year_read]^[ccode_index.index(ccode_read)]^.milper := missing_value;
                                       if (data^[year_read]^[ccode_index.index(ccode_read)]^.milex = -9) or
                                          (data^[year_read]^[ccode_index.index(ccode_read)]^.milex = -99) then
                                          data^[year_read]^[ccode_index.index(ccode_read)]^.milex := missing_value;
                                       if (data^[year_read]^[ccode_index.index(ccode_read)]^.energy = -9) or
                                          (data^[year_read]^[ccode_index.index(ccode_read)]^.energy = -99) then
                                          data^[year_read]^[ccode_index.index(ccode_read)]^.energy := missing_value;
                                       if (data^[year_read]^[ccode_index.index(ccode_read)]^.irst = -9) or
                                          (data^[year_read]^[ccode_index.index(ccode_read)]^.irst = -99) then
                                          data^[year_read]^[ccode_index.index(ccode_read)]^.irst := missing_value;
                                       if (data^[year_read]^[ccode_index.index(ccode_read)]^.upop = -9) or
                                          (data^[year_read]^[ccode_index.index(ccode_read)]^.upop = -99) then
                                          data^[year_read]^[ccode_index.index(ccode_read)]^.upop := missing_value;
                                       if (data^[year_read]^[ccode_index.index(ccode_read)]^.tpop = -9) or
                                          (data^[year_read]^[ccode_index.index(ccode_read)]^.tpop = -99) then
                                          data^[year_read]^[ccode_index.index(ccode_read)]^.tpop := missing_value;
                                    end    {year_read in appropriate range}
                                 else
                                    begin   {not in appropriate partition/year range}
                                        {Don't want this record, do nothing}
                                    end;
                                     {Set to read next record, if there is one.}
                                end;   {if not eoln then ...}
                                {I have not read to the end of the record, so do a readln to advance}
                                if not eof(infile) then readln(infile);
                             end;     {while not eof infile}
                        end;   {if not eof infile}
                  finally
                     CloseFile (infile);
                  end;
               end;         {if select modified cap.}
            except
               on EUserInterrupt do raise;
               on EInOutError do
                 begin
                    FileErrorBox.maindo ('Error opening file "'+modified_data_file_name+ '"',
                                         'File could not be opened for input.',
                                         'File may be in use by another program, or may be missing.');
                    FileErrorBox.showmodal;
                    raise;
                 end;
            end;

            created := true;
            trace.message ('Raw capability data required ' +
                           inttostr(round(((start_mem-memavail)/1024))) + ' Kilobytes of memory');
            Raw_trace.tickdone;
         finally
            Raw_trace.free;
            trace.exit('Finished initializing raw capability data');
         end;
      end;


   destructor Traw_capability_array_obj.destroy;
      var ccode_loop : ccode_range;
          year_loop : year_range;
      begin
         try
            if self <> nil then
            begin
              if data <> nil then
                begin
                  for year_loop := min_year to max_year do
                   if data^[year_loop] <> nil then
                    begin
                     for ccode_loop := min_ccode to max_ccode do
                      if data^[year_loop]^[ccode_index.index(ccode_loop)] <> nil then
                       begin
                        dispose (data^[year_loop]^[ccode_index.index(ccode_loop)]);
                        data^[year_loop]^[ccode_index.index(ccode_loop)] := nil;
                       end;
                     dispose (data^[year_loop]);
                     data^[year_loop] := nil;
                    end;
                 dispose(data);
                 data := nil;
               end;
               created := false;
               inherited destroy;
            end;   {self <> nil}
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;    {destructor}

   function Traw_capability_array_obj.get_first_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Raw capability get called before initialization. ',
                               5, stop, error_log);
            end
         else
         get_first_partition_year := first_partition_year;
      end;

   function Traw_capability_array_obj.get_last_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Raw capability get called before initialization. ',
                               5, stop, error_log);
            end
         else
         get_last_partition_year := last_partition_year;
      end;

   function Traw_capability_array_obj.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then if created=true then initialized := true;
      end;

   function Traw_capability_array_obj.all_in_range (ccode : ccode_range; year : year_range; error_check: boolean) : boolean;
      begin
         all_in_range := true;
         if ccode = 0 then
            all_in_range := false
         else
            if not(initialized) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        EUGeneError ('Individual capability component get called before initialization. ', 5, stop, error_log);
                     end;
               end
         else
            if not ((year >= get_first_partition_year) and (year <= get_last_partition_year)) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        EUGeneError ('Individual capability component get called with out-of-bounds year ('+inttostr(year)+'). ',
                                        5, continue, error_log);
                        trace.message ('Capability set to missing');
                     end;
               end
         else
            if not (nation_list.is_a_state(ccode, year)) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        trace.message('Individual capability component get called for ccode that is not a state (' +
                              inttostr(ccode)+' in '+inttostr(year)+'). ');
                        trace.message ('Capability set to missing');
                     end;
               end;
      end;   {func all in range}


   function Traw_capability_array_obj.get_tpop (ccode: ccode_range; year: year_range) : longint;
      {returns total populatino value, or -8 if the year is not a valid state year for the ccode}
      begin
         {no error check param, so check for errors as usual}
         get_tpop := missing_value;
         if all_in_range (ccode, year, true) then
            get_tpop := data^[year]^[ccode_index.index(ccode)].tpop;
         if result = initialized_value then
            begin
               trace.message('tpop capability still at initialized value for ' +
                           inttostr(ccode)+' in '+inttostr(year)+').  Capability set to missing');
               get_tpop := missing_value;
            end;
      end;

   function Traw_capability_array_obj.get_upop (ccode: ccode_range; year: year_range) : longint;
      begin
         {no error check param, so check for errors as usual}
         get_upop := missing_value;
         if all_in_range (ccode, year, true) then
            get_upop := data^[year]^[ccode_index.index(ccode)].upop;
         if result = initialized_value then
            begin
               trace.message('get_upop capability still at initialized value for ' +
                           inttostr(ccode)+' in '+inttostr(year)+').  Capability set to missing');
               get_upop := missing_value;
            end;
      end;

   function Traw_capability_array_obj.get_irst (ccode: ccode_range; year: year_range) : longint;
      begin
         {no error check param, so check for errors as usual}
         get_irst := missing_value;
         if all_in_range (ccode, year, true) then
            get_irst := data^[year]^[ccode_index.index(ccode)].irst;
         if result = initialized_value then
            begin
               trace.message('get_irst capability still at initialized value for ' +
                           inttostr(ccode)+' in '+inttostr(year)+').  Capability set to missing');
               get_irst := missing_value;
            end;
      end;

   function Traw_capability_array_obj.get_energy (ccode: ccode_range; year: year_range) : longint;
      begin
         {no error check param, so check for errors as usual}
         get_energy := missing_value;
         if all_in_range (ccode, year, true) then
            get_energy := data^[year]^[ccode_index.index(ccode)].energy;
         if result = initialized_value then
            begin
               trace.message('get_irst capability still at initialized value for ' +
                           inttostr(ccode)+' in '+inttostr(year)+').  Capability set to missing');
               get_energy := missing_value;
            end;
      end;

   function Traw_capability_array_obj.get_milper (ccode: ccode_range; year: year_range) : longint;
      begin
         {no error check param, so check for errors as usual}
         get_milper := missing_value;
         if all_in_range (ccode, year, true) then
            get_milper := data^[year]^[ccode_index.index(ccode)].milper;
         if result = initialized_value then
            begin
               trace.message('get_milper capability still at initialized value for ' +
                           inttostr(ccode)+' in '+inttostr(year)+').  Capability set to missing');
               get_milper := missing_value;
            end;
      end;

   function Traw_capability_array_obj.get_milex (ccode: ccode_range; year: year_range) : longint;
      begin
         {no error check param, so check for errors as usual}
         get_milex := missing_value;
         if all_in_range (ccode, year, true) then
            get_milex := data^[year]^[ccode_index.index(ccode)].milex;
         if result = initialized_value then
            begin
               trace.message('get_milex capability still at initialized value for ' +
                           inttostr(ccode)+' in '+inttostr(year)+').  Capability set to missing');
               get_milex := missing_value;
            end;
      end;


{Now version called with error check}
   function Traw_capability_array_obj.get_tpop (ccode: ccode_range; year: year_range; error_check: boolean) : longint;
      {returns total populatino value, or -8 if the year is not a valid state year for the ccode}
      begin
         {called with error check param, so call with value of error_check param.}
         get_tpop := missing_value;
         if all_in_range (ccode, year, error_check) then
            get_tpop := data^[year]^[ccode_index.index(ccode)].tpop;
         if result = initialized_value then
            begin
               trace.message('tpop capability still at initialized value for ' +
                           inttostr(ccode)+' in '+inttostr(year)+').  Capability set to missing');
               get_tpop := missing_value;
            end;
      end;

   function Traw_capability_array_obj.get_upop (ccode: ccode_range; year: year_range; error_check: boolean) : longint;
      begin
         {called with error check param, so call with value of error_check param.}
         get_upop := missing_value;
         if all_in_range (ccode, year, error_check) then
            get_upop := data^[year]^[ccode_index.index(ccode)].upop;
         if result = initialized_value then
            begin
               trace.message('get_upop capability still at initialized value for ' +
                           inttostr(ccode)+' in '+inttostr(year)+').  Capability set to missing');
               get_upop := missing_value;
            end;
      end;

   function Traw_capability_array_obj.get_irst (ccode: ccode_range; year: year_range; error_check: boolean) : longint;
      begin
         {called with error check param, so call with value of error_check param.}
         get_irst := missing_value;
         if all_in_range (ccode, year, error_check) then
            get_irst := data^[year]^[ccode_index.index(ccode)].irst;
         if result = initialized_value then
            begin
               trace.message('get_irst capability still at initialized value for ' +
                           inttostr(ccode)+' in '+inttostr(year)+').  Capability set to missing');
               get_irst := missing_value;
            end;
      end;

   function Traw_capability_array_obj.get_energy (ccode: ccode_range; year: year_range; error_check: boolean) : longint;
      begin
         {called with error check param, so call with value of error_check param.}
         get_energy := missing_value;
         if all_in_range (ccode, year, error_check) then
            get_energy := data^[year]^[ccode_index.index(ccode)].energy;
         if result = initialized_value then
            begin
               trace.message('get_irst capability still at initialized value for ' +
                           inttostr(ccode)+' in '+inttostr(year)+').  Capability set to missing');
               get_energy := missing_value;
            end;
      end;

   function Traw_capability_array_obj.get_milper (ccode: ccode_range; year: year_range; error_check: boolean) : longint;
      begin
         {called with error check param, so call with value of error_check param.}
         get_milper := missing_value;
         if all_in_range (ccode, year, error_check) then
            get_milper := data^[year]^[ccode_index.index(ccode)].milper;
         if result = initialized_value then
            begin
               trace.message('get_milper capability still at initialized value for ' +
                           inttostr(ccode)+' in '+inttostr(year)+').  Capability set to missing');
               get_milper := missing_value;
            end;
      end;

   function Traw_capability_array_obj.get_milex (ccode: ccode_range; year: year_range; error_check: boolean) : longint;
      begin
         {called with error check param, so call with value of error_check param.}
         get_milex := missing_value;
         if all_in_range (ccode, year, error_check) then
            get_milex := data^[year]^[ccode_index.index(ccode)].milex;
         if result = initialized_value then
            begin
               trace.message('get_milex capability still at initialized value for ' +
                           inttostr(ccode)+' in '+inttostr(year)+').  Capability set to missing');
               get_milex := missing_value;
            end;
      end;

      {  -------------------------------------------------   }

   {methods for system capabilities object}
   constructor Tsys_capability_array_obj.init (a_file_name : TFileName; year1, year2 : year_range);
      var cap_file : sys_cap_file_type;
          cap_rec : sys_cap_file_record_type;
          year_loop : year_range;
          ccode_loop : ccode_range;
          new_index_number, old_index_number, ccode_index_loop : ccode_index_range;
          current_ccode_list : ccode_index_array;
          ccode_year_data : syscap_array_ptr;  {array[ccode_index_range] of single;}
          temp : integer;
          start_mem, heapneeded : longint;
          Sys_trace : TTrace_obj;

      begin
         try
            try
               start_mem := memavail;
               if year1 > year2 then switch_year (year1, year2);
               trace.enter('Initializing system capability data, '+inttostr(year1)+' to '+inttostr(year2));
               self.first_partition_year := year1;
               self.last_partition_year := year2;

               Sys_trace := nil;
               Sys_trace := TTrace_obj.init(trace.get_trace_level);
               if not(ccode_index.initialized) then
                  begin
                     EUGeneError ('Sys Capability array creation called before ccode_index initialized',
                                     5, stop, error_log);
                  end;
               if not(nation_list.initialized) then
                  begin
                     EUGeneError ('Sys Capability array creation called before nation_list initialized',
                                     5, stop, error_log);
                  end;

                  {initialize file}
               trace.message ('Initializing data arrays');
               heapneeded := TSys_capability_array_obj_mem_overhead +
                               ((self.last_partition_year - self.first_partition_year + 1) *
                             TSys_capability_array_obj_mem_per_year);
               if debug[4] then
                  begin
                     trace.message ('6 part capability array size calculation');
                     trace.message ('Calc is that '+inttostr(TSys_capability_array_obj_mem_per_year)+' needed per year.');
                     trace.message ('Calc is that '+inttostr(heapneeded)+' needed for all years.');
                     trace.message ('Max avail mem block is '+inttostr(MaxAvail));
                  end;
               if MaxAvail <= (heapneeded) then
                  begin
                     EUGeneError ('Not enough memory for system capability. ',
                                     5, stop, error_log);
                  end;
               new (data);
               for year_loop := min_year to max_year do
                  begin
                     if (year_loop >= self.first_partition_year) and (year_loop <= self.last_partition_year) then
                     begin
                        new(ccode_year_data);
                        data^[year_loop] := ccode_year_data;
                        for ccode_loop := min_ccode to max_ccode do
                           data^[year_loop]^[ccode_index.index(ccode_loop)] := initialized_value;
                        for ccode_index_loop := min_ccode_index to max_ccode_index do
                           data^[year_loop]^[ccode_index_loop] := initialized_value;
                     end  {if year in range}
                     else data^[year_loop] := nil;
                  end;    {for year_loop}

               trace.message ('Reading data from external file');
               assignFile (cap_file, a_file_name);
               reset (cap_file);
               cap_rec.year := min_year;   {this is needed only to start the iterations}
               while (not eof (cap_file)) and (cap_rec.year <= self.last_partition_year) do
                  begin
                     Sys_trace.tick ( 'Executing Procedure: Read System Capability Data ',nation_list.get_country_years);
                     read (cap_file, cap_rec);
                     {each record has year, then ccode_index_listing, then array by ccode_index of reals}
                     if (cap_rec.year >= self.first_partition_year) and (cap_rec.year <= self.last_partition_year) then
                        begin
                           if ccode_index.identical_ccode_lists(cap_rec.ccode_from_index_list) then
                              data^[cap_rec.year]^ := cap_rec.ccode_array
                           else
                              begin  {new list not old list, change it}
                                 for old_index_number := min_ccode_index to max_ccode_index do
                                    begin
                                       {cap_rec.ccode_from_index_list[old_index_number]  is a ccode; may be missing value}
                                       {ccode_index.index(cap_rec.ccode_from_index_list[old_index_number])  is the current index of that old ccode}
                                       new_index_number := ccode_index.index(cap_rec.ccode_from_index_list[old_index_number]);
                                       if new_index_number <> initialized_value then
                                          data^[cap_rec.year]^[new_index_number] := cap_rec.ccode_array[old_index_number];
                                    end;
                              end;
                        end;
                  end;   {while not eof}
               created := true;

            finally
               CloseFile (cap_file);
               sys_trace.tickdone;
               Sys_trace.free;
               trace.message ('System Capability required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
               trace.exit('Finished initializing system capability data');
            end;
         except
            on EUserInterrupt do raise;
            on EInOutError do
              begin
                 FileErrorBox.maindo ('Error opening file "'+a_file_name+ '"',
                                      'File could not be opened for input.',
                                      'File may be in use by another program, or may be missing.');
                 FileErrorBox.showmodal;
                 raise;
              end;
         end;
      end;

   destructor Tsys_capability_array_obj.destroy;
      var year_loop : year_range;
      begin
         try
            if self <> nil then
            begin
               for year_loop := min_year to max_year do
                 if data^[year_loop] <> nil then
                    begin
                       dispose (data^[year_loop]);
                       data^[year_loop] := nil;
                    end;
               if data <> nil then dispose (data);
               data := nil;
               created := false;
               inherited destroy;
            end;   {if self <> nil}
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;

   function Tsys_capability_array_obj.get_first_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('6 part system capability get called before initialization. ',
                               5, stop, error_log);
            end
         else
         get_first_partition_year := first_partition_year;
      end;

   function Tsys_capability_array_obj.get_last_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('6 part system capability get called before initialization. ',
                               5, stop, error_log);
            end
         else
         get_last_partition_year := last_partition_year;
      end;

   function Tsys_capability_array_obj.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then if created=true then initialized := true;
      end;

                                    {  -------------}

   function Tsys_capability_array_obj.all_in_range (ccode : ccode_range; year : year_range; error_check: boolean) : boolean;
      begin
         all_in_range := true;
         if ccode = 0 then
            all_in_range := false
         else
            if not(initialized) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        EUGeneError ('6 part system capability get called before initialization. ',
                                  5, stop, error_log);
                     end;
               end
         else
            if not ((year >= get_first_partition_year) and (year <= get_last_partition_year)) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        EUGeneError ('6 part system capability get called with out-of-bounds year ('+inttostr(year)+'). ',
                                        5, continue, error_log);
                        trace.message ('Capability set to missing');
                     end;
               end
         else
            if not (nation_list.is_a_state(ccode, year)) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        trace.message('6 part system capability get called for ccode that is not a state (' +
                              inttostr(ccode)+' in '+inttostr(year)+'). ');
                        trace.message ('Capability set to missing');
                     end;
               end;
      end;
                                    {  -------------}

   function Tsys_capability_array_obj.get_syscap (ccode : ccode_range; year : year_range) : single;
      begin
         {no error check param, so check for errors as usual}
         get_syscap := missing_value;
         if all_in_range (ccode, year, true) then
            get_syscap := data^[year]^[ccode_index.index(ccode)];
         if result = initialized_value then
            begin
               trace.message('6 part system capability still at initialized value for ' +
                           inttostr(ccode)+' in '+inttostr(year)+').  Capability set to missing');
               get_syscap := missing_value;
            end;
      end;

                                    {  -------------}

   function Tsys_capability_array_obj.get_syscap (ccode : ccode_range; year : year_range; error_check: boolean) : single;
      begin
         {called with error check param, so call with value of error_check param.}
         get_syscap := missing_value;
         if all_in_range (ccode, year, error_check) then
            get_syscap := data^[year]^[ccode_index.index(ccode)];
         if result = initialized_value then
            begin
               if error_check then
                  trace.message('6 part system capability still at initialized value for ' +
                           inttostr(ccode)+' in '+inttostr(year)+').  Capability set to missing');
               get_syscap := missing_value;
            end;
      end;



   {  -----------------------   methods for Talliance_year_obj  }

   constructor Talliance_year_obj.init;   {For initial object, just blanks data to nil}
      begin
         data := nil;
      end;    {constructor}

   destructor Talliance_year_obj.destroy;
      begin
         try
            if self <> nil then
               begin
                  if data <> nil then dispose (data);
                  data := nil;
                  if self <> nil then inherited destroy;
               end;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;

   procedure Talliance_year_obj.allocate_full (for_year : year_range);
             {does new, blanks data to -1 or no alliance (4)}
      var country_index1, country_index2 : ccode_index_range;
          country1, country2 : ccode_range;
          heapneeded : longint;
      begin   {initialize all pairs to no alliance, or a -1 if it's not a valid pair}
         try
            heapneeded := sizeof(alliance_year_array_type);
            if MaxAvail <= (heapneeded) then
               begin
                  EUGeneError ('Not enough memory for alliance_year object. ',
                          5, stop, error_log);
               end;
            new (data);
               {first initialize all to be -1}
            for country_index1 := min_ccode_index to max_ccode_index do
               for country_index2 := min_ccode_index to max_ccode_index do
                  begin
                     data^[country_index1, country_index2] := initialized_value;
                  end;

           {Can't do just that, though.  In fact, some
                  need to be set to no alliance b/c COW alliance
                  data does not have a record in the data set for all states, even if they are
                  members of the system.}
            {now set to be no alliance when necessary}
            for country1 := min_ccode to max_ccode do
              if (nation_list.is_a_state(country1, for_year)) then
                for country2 := min_ccode to max_ccode do
                  if (nation_list.is_a_state(country2, for_year)) then
                      begin
                        set_alliance_value(country1, country2, 4);
                      end;
         finally
         end;
      end;    {full allocation procedure}

   procedure Talliance_year_obj.copy (another_alliance_year : Talliance_year_obj);
       {this assumes the structure exists, i.e. .init was called.
        Also assumes that a new(data) has been called, as in the allocate_full proc.}
     begin
           {can do the copy in one step b/c the data structure is just
            a straight array, no pointers.}
           self.data^ := another_alliance_year.data^;
     end;

   procedure Talliance_year_obj.set_alliance_value (ccode1, ccode2 : ccode_range; atype : alliance_value_type);
      begin
         data^[ccode_index.index(ccode1), ccode_index.index(ccode2)] := atype;
      end;    {proc set alliance _value}

   function Talliance_year_obj.get_alliance_value (ccode1, ccode2: ccode_range) : alliance_value_type;
      begin
         get_alliance_value := data^[ccode_index.index(ccode1), ccode_index.index(ccode2)];
      end;   {function get alliance value}

   procedure Talliance_year_obj.build_tau_table (ccode1, ccode2 : ccode_range; ayear : year_range;
             var result_table : tau_table_type; var N : num_countries_range;
             var nrows, ncolumns : tau_row_column_count_type;
             for_region : region_type);
                {builds a tau table from this year, for this ccode pair, using states
                 in the specified region (which might be the globe).}
      var row, column : integer;   {these could go 1 beyond range of 1..4}
          first_row, last_row, first_col, last_col : 0..4;
          cc1value, cc2value, partner_ccode : ccode_range;
      begin
         n := 0;
            {Row, column 1 is defense pact, 2 is nonaggression, 3 is entente, 4 is no alliance}
         for row := defense to no_alliance do
            for column := defense to no_alliance do
               result_table[row, column] := 0;
           {NOTE:  This is currently not implemented, all states are included!!
            As set up in the alliance data, 1 is a defense pact, 2 is neut, 3 is entente,
            4 means no alliance but active in system (which I want to count in tau table)
            and 0 means no allianc and inactive, which I don't want to count.}
         for partner_ccode := min_ccode to max_ccode do
         {Want to include in table states that are involved in "for_region".
          Ignore others.  But, since we may be calculating a dyad where a state is
          not involved by the formula (since we are doing all dyads), also manually
          include the two dyadic members as well.  So, table of US vs. Europeans in
          1816 should have US as a table entry, so as to include US-US alliance.  }
            if nation_list.is_a_state (partner_ccode, ayear) then
             if ((nation_list.is_involved_in_region(partner_ccode, for_region, ayear)) or
                 (ccode1=partner_ccode) or (ccode2=partner_ccode)) then
               begin
                  cc1value := get_alliance_value(ccode1, partner_ccode);
                  cc2value := get_alliance_value(ccode2, partner_ccode);
                  if (cc1value >= defense) and (cc1value <=no_alliance) and (cc2value >=defense) and (cc2value <= no_alliance) then
                     begin
                        inc(result_table[cc1value, cc2value]);
                        inc(n);
                     end;


         {if (ccode1=200) and (ccode2=255) and (for_region=globe) then
           begin
              trace.message ('[ '+inttostr(cc1value)+','+inttostr(cc2value)+' ] '+
                            inttostr(partner_ccode)+' '+nation_list.get_abbrev(partner_ccode));
           end;
         }
               end;

            {Figure out dimensions of table.  If the table is 1xn or nx1, then tau
             cannot be calculated.  }
            first_row := 0;
            row := 1;
            while (row <= 4) and (first_row=0) do
              begin
                 column := 1;
                 while (column <= 4) and (first_row=0) do
                    begin
                       if result_table[row,column] > 0 then first_row := row;
                       inc(column);
                    end;
                 inc(row);
              end;

            last_row := 0;
            row := 4;
            while (row >= 1) and (last_row=0) do
              begin
                 column := 1;
                 while (column <= 4) and (last_row=0) do
                    begin
                       if result_table[row,column] > 0 then last_row := row;
                       inc(column);
                    end;
                 dec(row);
              end;

            first_col := 0;
            column := 1;
            while (column <= 4) and (first_col=0) do
              begin
                 row := 1;
                 while (row <= 4) and (first_col=0) do
                    begin
                       if result_table[row,column] > 0 then first_col := column;
                       inc(row);
                    end;
                 inc(column);
              end;


            last_col := 0;
            column := 4;
            while (column >= 1) and (last_col=0) do
              begin
                 row := 1;
                 while (row <= 4) and (last_col=0) do
                    begin
                       if result_table[row,column] > 0 then last_col := column;
                       inc(row);
                    end;
                 dec(column);
              end;

            ncolumns := last_col - first_col + 1;
            nrows := last_row - first_row + 1;

            if (ncolumns < 2) or (nrows < 2) then
               begin
                  Trace.message ('Calculated a tau table with row or column < 2 for dyad '+
                     inttostr(ccode1)+' '+inttostr(ccode2));
                  {EUGeneError ('Calculated a tau table with row or column < 2 for dyad '+
                     inttostr(ccode1)+' '+inttostr(ccode2), 3, continue, error_log);  }
               end;

      end;    {build tau table procedure}

     {  -----------------------   methods for Talliance_array_obj  }

      constructor Talliance_array_obj.init (sequence_alliance_file_name, seq_file_name,
                  dyadic_alliance_file_name : TFileName; year1, year2 : year_range;
                  raw_data_source : alliance_in_data_type);
      begin
         try
            {if raw_data_source = flat_cow_sequence then
               init_from_seq(sequence_alliance_file_name, seq_file_name, year1, year2)
            else  }
            if raw_data_source = flat_dyadic then
               init_from_update (dyadic_alliance_file_name, year1, year2)
            else
               EUGeneError ('Alliance data init called with improper source code - Fatal error.  ',3, stop, error_log);
         finally
         end;  {finally}

      end;

     {  -----------------------    }

   procedure Talliance_array_obj.init_from_seq (alliance_file_name, seq_file_name : TFileName;
               year1, year2 : year_range);

      type seq_array_type = array[min_ccode_index..max_ccode_index] of ccode_range;  {for a seq #, returns ccode}
      var year_loop, year_read : year_range;
          x, prev_year : integer;
          ccode_read : ccode_range;
          seq_array : seq_array_type;
          alliance_file : text;
          country : ccode_range;
          allyspot : integer;
          rec_count : integer;
          start_mem, heapneeded : longint;
          blank_char : char;
          Ally_trace : Ttrace_obj;

      procedure read_seq_array (var seq_array : seq_array_type; seq_array_file_name : TFileName);
         var x, accode, aseq : integer;
             achar : char;
             aname : string[3];
             infile : text;
             Seq_Trace : Ttrace_obj;
         begin
            try
               try
                  Trace.enter ('Reading Sequence Array from external file');
                  Seq_Trace := nil;
                  Seq_Trace := TTrace_obj.init (trace.get_trace_level);
                   {initialize seq array}
                  for x := min_ccode_index to max_ccode_index do seq_array[x] := initialized_value;
                  assignFile (infile, seq_array_file_name);
                  reset (infile);
                  repeat
                     Seq_Trace.tick ('Executing Procedure: Read Alliance Sequence Values',0);
                     read(infile, accode);
                     {there might be some ccode in seqnums that are not in nations list.}
                     if nation_list.have_info(accode) then  {this is a seq # rec we want}
                     begin
                        repeat
                           read(infile, achar)
                        until (achar >='A') and (achar <='z');
                        aname := '   ';
                        aname[1] := achar;
                        read (infile, aname[2]);
                        read (infile, aname[3]);
                        if aname[3] = chr(9) then aname[3] := ' ';
                        read (infile, aseq);
                        {Some abbreviations changed...}
                        if aname='RUS' then aname := 'USR';
                        if aname = nation_list.get_abbrev(accode) then    {matches correctly}
                           begin
                              if ((aseq >= min_ccode_index) and (aseq <= max_ccode_index)) then
                                  seq_array[aseq] := accode
                              else
                                 EUGeneError ('Read a seq # in seq_num read that was too large. ',
                                     10, continue, error_log);
                           end
                        else
                           begin
                              EUGeneError ('ERROR reading seq num file - ',
                                     10, continue, error_log);
                              trace.message('3 letter abbrevs do not match for CC:'+aname+' '+nation_list.get_abbrev(accode));
                           end;
                     end    {if have_info}
                  else   {have no info on this ccode}
                     begin
                         {This ccode from seq list not on nations list, skip it.}
                        EUGeneError ('   ccode '+inttostr(accode)+' was in seqnum list but not nation list - skipping',
                               3, continue, error_log);
                     end;
                  readln(infile);
               until (eof (infile));
            finally
               Seq_Trace.tickdone;
               Seq_Trace.free;
               CloseFile (infile);
               Trace.exit ('Finished reading sequence array from external file');
            end;
         except
            on EUserInterrupt do raise;
            on EInOutError do
              begin
                 FileErrorBox.maindo ('Error opening file "'+seq_array_file_name+ '"',
                                      'File could not be opened for input.',
                                      'File may be in use by another program, or may be missing.');
                 FileErrorBox.showmodal;
                 raise;
              end;
         end;
      end;   {procedure read_seq_nums}

         { ---------------------------  }

      function ccode_from_seq (aseq : integer) : ccode_range;
         begin
            ccode_from_seq := seq_array[aseq];
         end;

         { ---------------------------  }

      procedure read_alliance_header (var alliance_file : text; var year_read : year_range;
                var ccode_read : ccode_range);
                {this procedure reads the deck #, year, and ccode from the alliance data set.
                 The variables are not separated by space in that data.  The deck, year,
                 and ccode are all 3 characters long.}
         var read_3 : string[3];
             deck_temp, ccode_temp, year_temp : integer;
             achar : char;
             errcode : integer;

         begin
             {first, read 3 character deck #.  There may be initial spaces.}
            read_3 := '   ';
            repeat
               read (alliance_file, achar)
            until achar in ['0'..'9'];
            read_3[1] := achar;
            read (alliance_file, read_3[2]);
            read (alliance_file, read_3[3]);
            val (read_3, deck_temp, errcode);   {don't bother using errcode}
            if deck_temp <> cow_alliance_deck_value then
               EUGeneError ('Error - deck value read in alliance read not correct, saw value of '+inttostr(deck_temp),
                               10, continue, error_log);

               {Now read 3 digit version of year}
            read_3 := '   ';
            read (alliance_file, read_3[1]);
            read (alliance_file, read_3[2]);
            read (alliance_file, read_3[3]);
            val (read_3, year_temp, errcode);
            year_temp := year_temp + 1000;
            if not(year_temp >= min_year) then
               EUGeneError ('Year value read in alliance less than minimum year ',
                               10, continue, error_log);
            if not(year_temp <= max_year) then
               EUGeneError ('Year value read in alliance greater than maximum year',
                               10, continue, error_log);
            year_read := year_temp;

               {Now read 3 digit version of ccode.  1st char may be a space, which is OK}
            read_3 := '   ';
            read (alliance_file, read_3[1]);
            read (alliance_file, read_3[2]);
            read (alliance_file, read_3[3]);
            val (read_3, ccode_temp, errcode);
            if not ((ccode_temp >= min_ccode) and (ccode_temp <= max_ccode)) then
               EUGeneError ('Ccode value read in alliance not in range',
                               10, continue, error_log);
            ccode_read := ccode_temp;

            {Also, there is a blank space after the ccode.  Read it.}
            read(alliance_file, achar);
            if achar <> ' ' then
               EUGeneError ('Blank not seen after reading ccode in alliance read',
                               10, continue, error_log);
         end;    {proc read alliance header}

         { ---------------------------  }

      procedure read_and_process_one_alliance (var alliance_file : text;
                ccode_read, year_read, allyspot : integer);
                {Reads one character [which is the alliance value in the data
                 for the pair in the sequence location], checks it, adds to the main
                 alliance data object}
                {COW data read in "0s" for pairs even when one is not a state.  Leave
                 as initialized value when this is the case.  }

         var achar : char;
             ally_value : integer;
             errcode : integer;
         begin
            if eof (alliance_file) then
                     EUGeneError ('Premature end of alliance file found.  Complete records may not be read in.',
                      3, continue, error_log)
            else read (alliance_file, achar);
            val (achar, ally_value, errcode);
            if errcode <> 0 then
                EUGeneError ('Invalid alliance value read in read alliance, alliance character read was '+achar,
                       3, stop, error_log);


          {If this spot is a pair where one member is not an actual state in
             this year according to Singer and Small list, then want to read the data
             anyway.  }
          {Do Not write into reverse pairing, so have A w/ B, B w/ A.
           This allows for non-symmetric alliances, if ever have them.}

             {check that value is valid}
            if (ally_value < initialized_value) or (ally_value > 4) then
                           begin
                              EUGeneError ('Invalid alliance value read in read alliance, value of '+inttostr(ally_value)+
                               ' for ccodes '+inttostr(ccode_read)+'  '+inttostr(ccode_from_seq(allyspot)),
                               3, continue, error_log);
                              trace.message ('Continuing processing');
                           end;

                  {check to see if already have a value in this spot in the array.
                   If so, this means there are duplicate alliance records in the data file.}
            if (get_alliance_value(ccode_read, ccode_from_seq(allyspot), year_read)
                     >= 1) and  (get_alliance_value(ccode_read, ccode_from_seq(allyspot), year_read)
                     <= 3) then
                           begin
                              EUGeneError ('Error reading alliance file.  Already read a type 1-3 alliance value for ccode '+
                                 inttostr(ccode_read)+' and ccode '+inttostr(ccode_from_seq(allyspot))+
                                 ' in '+inttostr(year_read), 3, continue, error_log);
                              trace.message ('Writing new record over old.');
                           end;

                  {if what's read is a non-alliance, check to see if it should be a
                   0 or a 4}
            if ally_value=0 then
                begin
                       if (nation_list.is_a_state (ccode_read, year_read)) and
                          (nation_list.is_a_state(ccode_from_seq(allyspot), year_read)) then
                         year_array[year_read].set_alliance_value(ccode_read, ccode_from_seq(allyspot), 4)
                end
            else
                year_array[year_read].set_alliance_value(ccode_read, ccode_from_seq(allyspot), ally_value);

         end;   {procedure read_and_process_one_alliance;}

         { -------------------------------------------  }

      begin      {main code for alliance_array_obj .init_from_seq}
         try
            try
               start_mem := memavail;
               if year1 > year2 then
                  switch_year (year1, year2);
               trace.enter ( ('Initializing alliance array, ' + inttostr(year1) + ' to ' + inttostr(year2)) );
               Ally_trace := nil;
               Ally_trace := TTrace_obj.init(trace.get_trace_level);
               created := false;
               self.first_partition_year := year1;
               self.last_partition_year := year2;

               if not(ccode_index.initialized) then
                  begin
                     EUGeneError ('Alliance array creation called before ccode_index initialized',
                                     5, stop, error_log);
                     Ally_trace.message ('Check program coding [notify programmer].  ');
                  end;
               if not(nation_list.initialized) then
                  begin
                     EUGeneError ('Alliance array creation called before nation_list initialized',
                                     5, stop, error_log);
                     Ally_trace.message ('Check program coding [notify programmer].  ');
                  end;

               heapneeded := Talliance_array_obj_mem_overhead +
                             (year2 - year1 + 1) * Talliance_array_obj_mem_per_year;
               if debug[4] then
                  begin
                     Ally_trace.message ('Alliance array size calculation in .init');
                     Ally_trace.message ('Calc is that '+inttostr(heapneeded)+' needed for structure.');
                     Ally_trace.message ('Max avail mem block is '+inttostr(MaxAvail));
                  end;
               if MaxAvail <= (heapneeded) then
                  begin
                     EUGeneError ('Not enough memory for alliance data structure. ',
                                     5, stop, error_log);
                     Ally_trace.message ('Check program coding [notify programmer].  ');
                  end;

               read_seq_array (seq_array, seq_file_name);

               {Initially, the .init constructor just sets the data to nil}
               for year_loop := min_year to max_year do
                  begin
                    year_array[year_loop] := nil;
                    year_array[year_loop] := Talliance_year_obj.init;
                    Ally_trace.tick( 'Initializing alliance array',(max_year-min_year+1));
                  end;
               Ally_trace.tickdone;

               {Some, but not all records will need a full set of data arrays.  This
                is done in the "allocate_full" procedure, not the .init constructor.}
               {initialize the records that will be read from the input file}
               for year_loop := self.first_partition_year to self.last_partition_year do
                  begin
                     Ally_trace.tick('Allocating and initializing full blank alliance space in specified partition',
                                                 (self.last_partition_year-self.first_partition_year+1));
                     year_array[year_loop].allocate_full (year_loop);
                  end;
               Ally_trace.tickdone;

               {The array is now initialized, and can be accessed, so set it to show as initialized}
               created := true;

               {now open and repeatedly read records here, until all relevant records are in}
               assignFile (alliance_file, alliance_file_name);
               reset (alliance_file);

               Ally_trace.message ('Reading alliance data from external sequence # file');
               {First, read headers until finding the correct place in the data given the strt year}
               Repeat
                  Ally_trace.tick ( 'Executing Procedure: Reading Alliance Data',21000 div 4);
                  read_alliance_header (alliance_file, year_read, ccode_read);
                  if not ((year_read >= self.first_partition_year) and
                          (year_read <= self.last_partition_year)) then
                     begin    {not in correct range, so keep going.  Read rest of record}
                              {Each record has 4 lines}
                        if eof (alliance_file) then
                           EUGeneError ('Premature end of alliance file found.  Complete records may not be read in.',
                                     10, continue, error_log)
                        else readln (alliance_file);
                        if eof (alliance_file) then
                           EUGeneError ('Premature end of alliance file found.  Complete records may not be read in.',
                                     10, continue, error_log)
                        else readln (alliance_file);
                        if eof (alliance_file) then
                           EUGeneError ('Premature end of alliance file found.  Complete records may not be read in.',
                                     10, continue, error_log)
                        else readln (alliance_file);
                        if eof (alliance_file) then
                           EUGeneError ('Premature end of alliance file found.  Complete records may not be read in.',
                                     10, continue, error_log)
                        else readln (alliance_file);
                     end;
               until ((year_read >= self.first_partition_year) and (year_read <= self.last_partition_year))
                     or eof(alliance_file);
               Ally_trace.tickdone;

               if eof(alliance_file) then
                  begin
                     EUGeneError ('Hit eof alliance file before reading appropriate year!!',
                                     10, stop, error_log);
                     Ally_trace.message ('Check program coding or data [notify programmer].  ');
                  end
               else
                  begin
                        {Now have Read start of a good record.  Have ccode_read, year_read from that.}
                        {Now Finish reading and processing this record}
                        for allyspot := 1 to 60 do
                           read_and_process_one_alliance (alliance_file,
                                    ccode_read, year_read, allyspot);
                           {done with seqnum 1 to 60;  now move on to next record}
                        if eof (alliance_file) then
                           EUGeneError ('Premature end of alliance file found.  Complete records may not be read in.',
                                     10, continue, error_log)
                        else readln (alliance_file);
                        {subsequent lines have a number of blank spaces starting each line}
                        for x := 1 to 11 do
                           begin
                              read(alliance_file, blank_char);
                              if blank_char <> ' ' then EUGeneError ('Error in alliance file - blank not seen when expected ',
                                                               3, continue, error_log);
                           end;
                        for allyspot := 61 to 120 do
                           read_and_process_one_alliance (alliance_file,
                                    ccode_read, year_read, allyspot);
                        if eof (alliance_file) then
                           EUGeneError ('Premature end of alliance file found.  Complete records may not be read in.',
                                     10, continue, error_log)
                        else readln (alliance_file);
                        for x := 1 to 11 do
                           begin
                              read(alliance_file, blank_char);
                              if blank_char <> ' ' then EUGeneError ('Error in alliance file - blank not seen when expected ',
                                                               3, continue, error_log);
                           end;
                        for allyspot := 121 to 180 do
                           read_and_process_one_alliance (alliance_file,
                                    ccode_read, year_read, allyspot);
                        if eof (alliance_file) then
                           EUGeneError ('Premature end of alliance file found.  Complete records may not be read in.',
                                     10, continue, error_log)
                        else readln (alliance_file);
                        for x := 1 to 11 do
                           begin
                              read(alliance_file, blank_char);
                              if blank_char <> ' ' then EUGeneError ('Error in alliance file - blank not seen when expected ',
                                                               3, continue, error_log);
                           end;
                        for allyspot := 181 to 240 do
                           read_and_process_one_alliance (alliance_file,
                                    ccode_read, year_read, allyspot);
                        if eof (alliance_file) then
                           EUGeneError ('Premature end of alliance file found.  Complete records may not be read in.',
                                     10, continue, error_log)
                        else readln (alliance_file);
                  end;     {Found the correct location in data to start reading, and have read }
                            {the first record.  }

                  {Now read the rest of the data.}
                  {If the data were guaranteed to be sorted by year, could do:
                   while (not(eof(alliance_file))) and (year_read <= last_partition_year) do
                   but they may not be, so read the whole rest of the file.}
               while (not(eof(alliance_file))) do
                  begin        {read and (possibly) save a record}
                        read_alliance_header (alliance_file, year_read, ccode_read);
                        Ally_trace.tick ('Executing Procedure: Reading Alliance Data, '+inttostr(self.first_partition_year)+' to '+
                            inttostr(self.last_partition_year),21000 div 4);
                        if not ((year_read >= self.first_partition_year) and (year_read <= self.last_partition_year)) then
                           begin
                              {date out of range - do not process record, but skip to next}
                              if eof (alliance_file) then
                              EUGeneError ('Premature end of alliance file found.  Complete records may not be read in.',
                                     10, continue, error_log)
                              else readln (alliance_file);
                              if eof (alliance_file) then
                              EUGeneError ('Premature end of alliance file found.  Complete records may not be read in.',
                                     10, continue, error_log)
                              else readln (alliance_file);
                              if eof (alliance_file) then
                              EUGeneError ('Premature end of alliance file found.  Complete records may not be read in.',
                                     10, continue, error_log)
                              else readln (alliance_file);
                              if eof (alliance_file) then
                              EUGeneError ('Premature end of alliance file found.  Complete records may not be read in.',
                                     10, continue, error_log)
                              else readln (alliance_file);
                           end
                        else
                        begin
                              {Now have Read start of a good record.  Have ccode_read, year_read from that.}
                              {Now Finish reading and processing this record}
                              for allyspot := 1 to 60 do
                                 read_and_process_one_alliance (alliance_file,
                                                                ccode_read, year_read, allyspot);
                              if eof (alliance_file) then
                                 EUGeneError ('Premature end of alliance file found.  Complete records may not be read in.',
                                     10, continue, error_log)
                              else readln (alliance_file);
                              for x := 1 to 11 do
                                begin
                                  read(alliance_file, blank_char);
                                  if blank_char <> ' ' then EUGeneError ('Error in alliance file - blank not seen when expected ',
                                                               3, continue, error_log);
                                end;
                              for allyspot := 61 to 120 do
                                 read_and_process_one_alliance (alliance_file,
                                                                ccode_read, year_read, allyspot);
                              if eof (alliance_file) then
                                 EUGeneError ('Premature end of alliance file found.  Complete records may not be read in.',
                                     10, continue, error_log)
                              else readln (alliance_file);
                              for x := 1 to 11 do
                                begin
                                  read(alliance_file, blank_char);
                                  if blank_char <> ' ' then EUGeneError ('Error in alliance file - blank not seen when expected ',
                                                               3, continue, error_log);
                                end;
                              for allyspot := 121 to 180 do
                                 read_and_process_one_alliance (alliance_file,
                                                                ccode_read, year_read, allyspot);
                              if eof (alliance_file) then
                                 EUGeneError ('Premature end of alliance file found.  Complete records may not be read in.',
                                     10, continue, error_log)
                              else readln (alliance_file);
                              for x := 1 to 11 do
                                begin
                                  read(alliance_file, blank_char);
                                  if blank_char <> ' ' then EUGeneError ('Error in alliance file - blank not seen when expected ',
                                                               3, continue, error_log);
                                end;
                              for allyspot := 181 to 240 do
                                 read_and_process_one_alliance (alliance_file,
                                                                ccode_read, year_read, allyspot);
                              if eof (alliance_file) then
                                 EUGeneError ('Premature end of alliance file found.  Complete records may not be read in.',
                                      10, continue, error_log)
                              else readln (alliance_file);
                        end;     {if out of range else }
                  end;    {while not eof and in range, read and save a record of input file}
               Ally_trace.tickdone;

               {Now completed reading data records!}
               Ally_trace.message ('Finished reading alliance data from file');

                    {Now, set all countries to have def pact w/ self}
               for year_loop := self.first_partition_year to self.last_partition_year do
                  begin   {if a state in year, set to defense pact.  }
                     for country := min_ccode to max_ccode do
                        if nation_list.is_a_state (country, year_loop) then
                           begin
                              Ally_trace.tick ('Executing Procedure: Setting Defense Pacts',(max_ccode-min_ccode)*(self.last_partition_year-self.first_partition_year));
                              year_array[year_loop].set_alliance_value(country, country, 1);
                           end;
                  end;
               Ally_trace.tickdone;


               {Previously, checked activity of states to code whether or not they had any alliance activity.
                That was wrong.  But, do want to code states that are actually states
                as being a 4.  If a ccode is not a state, then it should stay as a 0.  This
                is done in the read and process one alliance procedure above.}

            finally
               CloseFile (alliance_file);
               Ally_trace.free;
               trace.message ('Alliance data required ' + inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
               trace.exit ('Finished Initializing alliance array');
            end;
         except
            on EUserInterrupt do raise;
            on EInOutError do
              begin
                 FileErrorBox.maindo ('Error opening file "'+alliance_file_name+ '"',
                                      'File could not be opened for input.',
                                      'File may be in use by another program, or may be missing.');
                 FileErrorBox.showmodal;
                 raise;
              end;
         end;
      end;       {method init for Talliance_array_obj}

               { ---------------------------  }

   procedure Talliance_array_obj.init_from_update (dyadic_alliance_file_name : TFileName;
                                    year1, year2 : year_range);
      var
          {dyadic_alliance_file : dyadic_alliance_file_type;}
          dyadic_alliance_text_file : text;
          dyadic_in_rec, left_record, right_record : dyadic_alliance_record_type;
          Ally_trace : Ttrace_obj;
          ally_value : alliance_value_type;
          start_mem, heapneeded, left, right, count : longint;
          year_loop, want_year, prev_year : year_range;
          country : ccode_range;
          rec_count : integer;

     function read_dyadic_alliance_text_rec (var infile : text) : dyadic_alliance_record_type;
        var arec : dyadic_alliance_record_type;
            file_version : real;
        begin
           try
              arec.ccode1 := read_csv_int (infile);
              arec.ccode2 := read_csv_int (infile);
              arec.year := read_csv_int (infile);
              arec.allynum := read_csv_int (infile);
              arec.atype := read_csv_int (infile);
              file_version := read_csv_real (infile);
              readln(infile);
           except
              EUGeneError ('Error reading a line within the alliance dyadic text file.  Setting record to blank.  Notify programmer of errors.',1,continue,error_log);
              arec.ccode1 := min_ccode;
              arec.ccode2 := min_ccode;
              arec.year := min_year;
              arec.allynum := 0;
              arec.atype := no_alliance;
           end;
           result := arec;
        end;

     begin                           {alliance array init from update}
        try
            try
               start_mem := memavail;
               if year1 > year2 then
                  switch_year (year1, year2);
               trace.enter ( ('Initializing alliance array from dyads, ' + inttostr(year1) + ' to '
                              + inttostr(year2)) );
               Ally_trace := nil;
               Ally_trace := TTrace_obj.init(trace.get_trace_level);
               created := false;
               self.first_partition_year := year1;
               self.last_partition_year := year2;

               if not(ccode_index.initialized) then
                  begin
                     EUGeneError ('Alliance array creation called before ccode_index initialized',
                                     5, stop, error_log);
                     Ally_trace.message ('Check program coding [notify programmer].  ');
                  end;
               if not(nation_list.initialized) then
                  begin
                     EUGeneError ('Alliance array creation called before nation_list initialized',
                                     5, stop, error_log);
                     Ally_trace.message ('Check program coding [notify programmer].  ');
                  end;

               heapneeded := Talliance_array_obj_mem_overhead +
                             (year2 - year1 + 1) * Talliance_array_obj_mem_per_year;
               if debug[4] then
                  begin
                     Ally_trace.message ('Alliance array size calculation in .init');
                     Ally_trace.message ('Calc is that '+inttostr(heapneeded)+' needed for structure.');
                     Ally_trace.message ('Max avail mem block is '+inttostr(MaxAvail));
                  end;
               if MaxAvail <= (heapneeded) then
                  begin
                     EUGeneError ('Not enough memory for alliance data structure. ',
                                     5, stop, error_log);
                     Ally_trace.message ('Check program coding [notify programmer].  ');
                  end;

               {Initially, the .init constructor just sets the data to nil}
               for year_loop := min_year to max_year do
                  begin
                    year_array[year_loop] := nil; 
                    year_array[year_loop] := Talliance_year_obj.init;
                    Ally_trace.tick('Executing Procedure: Initializing Alliance Array,'+inttostr(year_loop),max_year-min_year+1);
                  end;
               Ally_trace.tickdone;

               {Some, but not all records will need a full set of data arrays.  This
                is done in the "allocate_full" procedure, not the .init constructor.}
               {initialize the records that will be read from the input file.
                Note:  this sets all dyads that exist to be a type '4', no alliance.}
               for year_loop := self.first_partition_year to self.last_partition_year do
                  begin
                     Ally_trace.tick('Executing Procedure: Initializing Blank Alliance Array,'+inttostr(year_loop),max_year-min_year+1);
                     year_array[year_loop].allocate_full (year_loop);
                  end;
               Ally_trace.tickdone;

               {Because this version of the alliance data has only dyads with alliances,
               need to set all initially to no alliance, and then update them.}


               {The array is now initialized, and can be accessed, so set it to show as initialized}
               created := true;

               {now open and repeatedly read records here, until all relevant records are in}
               assignFile (dyadic_alliance_text_file, dyadic_alliance_file_name);
               reset (dyadic_alliance_text_file);
               {For COW alliance data starting with v3.03, set this up to use the released
                version of the alliance data file, which has a header line.}
               readln (dyadic_alliance_text_file);

               Ally_trace.message ('Reading alliance data from external dyadic file');
               rec_count := 0;

               dyadic_in_rec := read_dyadic_alliance_text_rec(dyadic_alliance_text_file);
               inc(rec_count);

               {find the beginning section of what needs to be read and processed}
               {have one record in memory.  Possibly store it before continuing read}
               if (dyadic_in_rec.year >= self.first_partition_year) and
                  (dyadic_in_rec.year <= self.last_partition_year) then
                  begin
                        if (get_alliance_value(dyadic_in_rec.ccode1, dyadic_in_rec.ccode2, dyadic_in_rec.year, false) >= 1)
                           and (get_alliance_value(dyadic_in_rec.ccode1, dyadic_in_rec.ccode2, dyadic_in_rec.year, false) <= 3)
                        then ally_value := min(get_alliance_value(dyadic_in_rec.ccode1,
                                dyadic_in_rec.ccode2, dyadic_in_rec.year, false), dyadic_in_rec.atype)
                        else ally_value := dyadic_in_rec.atype;
                        year_array[dyadic_in_rec.year].set_alliance_value(dyadic_in_rec.ccode1,
                              dyadic_in_rec.ccode2, ally_value);
                  end;

               {Old version read records until hit year out of range.  But that assumes
                that the data is sorted properly, which it may not be coming from COW.
                So just read the entire data file.}
               while not eof (dyadic_alliance_text_file) do
                  begin
                     Ally_trace.tick( 'Executing Procedure: Reading Alliance Data', 110000);
                     dyadic_in_rec := read_dyadic_alliance_text_rec(dyadic_alliance_text_file);
                     inc(rec_count);
                     if (dyadic_in_rec.year >= first_partition_year) and
                        (dyadic_in_rec.year <= last_partition_year) then
                     begin
                        {check that value is valid}
                        if (dyadic_in_rec.atype < initialized_value) or (dyadic_in_rec.atype > no_alliance) then
                                 begin
                                    EUGeneError ('Invalid alliance value read in read alliance, value of '+inttostr(dyadic_in_rec.atype)+
                                     ' for ccodes '+inttostr(dyadic_in_rec.ccode1)+'  '+inttostr(dyadic_in_rec.ccode2),
                                     3, continue, error_log);
                                    trace.message ('Continuing processing');
                                 end;

                        {check to see if already have a value in this spot in the array.
                         If so, this means there are duplicate alliance records in the data file.}
                        {And, if so, take the lower value.}
                        {This step will generate an error message in the trace file
                         if there are alliance records in the alliance data for years
                         that the cow nation list does not report the states as states.}
                        if (get_alliance_value(dyadic_in_rec.ccode1, dyadic_in_rec.ccode2, dyadic_in_rec.year, false) >= 1) and
                           (get_alliance_value(dyadic_in_rec.ccode1, dyadic_in_rec.ccode2, dyadic_in_rec.year, false) <= 3)
                           then
                               ally_value := min(get_alliance_value(dyadic_in_rec.ccode1,
                                      dyadic_in_rec.ccode2, dyadic_in_rec.year, false), dyadic_in_rec.atype)
                           else ally_value := dyadic_in_rec.atype;
                        year_array[dyadic_in_rec.year].set_alliance_value(dyadic_in_rec.ccode1,
                              dyadic_in_rec.ccode2, ally_value);
                     end;   {if year in range}

                  end;    {while}

               {Now completed reading data records!}
               Ally_trace.tickdone;

               Ally_trace.message ('Finished reading alliance data from file; '+inttostr(rec_count)+' records read from alliance data file.');


                    {Now, set all countries to have def pact w/ self}
               for year_loop := self.first_partition_year to self.last_partition_year do
                  begin   {if a state in year, set to defense pact.  }
                     for country := min_ccode to max_ccode do
                        if nation_list.is_a_state (country, year_loop) then
                           begin
                              Ally_trace.tick ('Executing Procedure: Setting Defense Pacts ',(max_ccode-min_ccode+1)*(self.last_partition_year-self.first_partition_year));
                              year_array[year_loop].set_alliance_value(country, country, 1);
                           end;
                  end;
               Ally_trace.tickdone;

            finally
               CloseFile (dyadic_alliance_text_file);
               Ally_trace.free;
               trace.message ('Alliance data required ' + inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
               trace.exit ('Finished Initializing alliance array');
            end;  {finally}
        except
            on EUserInterrupt do raise;
            on EInOutError do
              begin
                 FileErrorBox.maindo ('Error opening file "'+dyadic_alliance_file_name+ '"',
                                      'File could not be opened for input.',
                                      'File may be in use by another program, or may be missing.');
                 FileErrorBox.showmodal;
                 raise;
              end;
        end;
     end;

               { ---------------------------  }

   destructor Talliance_array_obj.destroy ;
      var year : integer;
      begin
         try
            if self <> nil then
            begin
               for year := self.first_partition_year to self.last_partition_year do
                  begin
                     year_array[year].free;
                     year_array[year] := nil;
                  end;
               self.first_partition_year := min_year;
               self.last_partition_year := min_year;
               created := false;
               inherited destroy;
            end;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;

               { ---------------------------  }

   procedure Talliance_array_obj.make_alliances_symmetric;
      var country1, country2 : ccode_range;
          replace_value : alliance_value_type;
          year_loop : year_range;
          Ally_trace : Ttrace_obj;
      begin
         try
            Ally_trace := nil;
            Ally_trace := TTrace_obj.init(trace.get_trace_level);
                 {Now, verify / make symmetric}
            for year_loop := self.first_partition_year to self.last_partition_year do
               begin   {if both states in year, check pacts.  }
                  Ally_trace.tick ('Executing Procedure: Checking symmetry',0);
                  for country1 := min_ccode to max_ccode do
                     if nation_list.is_a_state (country1, year_loop) then
                        for country2 := min_ccode to max_ccode do
                           if nation_list.is_a_state (country2, year_loop) then
                           begin
                              if year_array[year_loop].get_alliance_value(country1, country2) <>
                                 year_array[year_loop].get_alliance_value(country2, country1) then
                                 begin
                                    replace_value := min (year_array[year_loop].get_alliance_value(country1, country2),
                                                          year_array[year_loop].get_alliance_value(country2, country1));
                                    year_array[year_loop].set_alliance_value(country1, country2, replace_value);
                                    year_array[year_loop].set_alliance_value(country2, country1, replace_value);
                                    Trace.Message ('Replacing asymmetric alliance of '+inttostr(country1)+
                                                  ' and '+inttostr(country2)+' with '+inttostr(replace_value));
                                 end;
                           end;
               end;
            Ally_trace.tickdone;
         finally
            Ally_trace.free;
         end;
      end;   {procedure make symmetric}

               { ---------------------------  }

   function Talliance_array_obj.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then if created=true then initialized := true;
      end;

               { ---------------------------  }

   function Talliance_array_obj.get_first_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Alliance_array get_first_partition_year called before initialization',
                               10, stop, error_log);
               trace.message ('Check program coding [notify programmer]. ' );
            end
         else
            get_first_partition_year := first_partition_year;
      end;

               { ---------------------------  }

   function Talliance_array_obj.get_last_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Alliance_array get_last_partition_year called before initialization',
                               10, stop, error_log);
               trace.message ('Check program coding [notify programmer].  ');
            end
         else
         get_last_partition_year := last_partition_year;
      end;

               { ---------------------------  }

   function Talliance_array_obj.all_in_range (ccode1, ccode2 : ccode_range;  ayear : year_range; error_check : boolean) : boolean;
      begin
         all_in_range := true;
         if ((ccode1 = 0) or (ccode2 = 0)) then
            all_in_range := false
         else
            if not(initialized) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        EUGeneError ('alliance array get called before initialization. ',
                                  5, stop, error_log);
                     end;
               end
         else
            if not ((ayear >= get_first_partition_year) and (ayear <= get_last_partition_year)) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        EUGeneError ('alliance get called with out-of-bounds year ('+inttostr(ayear)+'). ',
                                        5, continue, error_log);
                        trace.message ('alliance set to missing');
                     end;
               end
         else
            if not ( (nation_list.is_a_state(ccode1, ayear)) and (nation_list.is_a_state(ccode2, ayear))) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        trace.message('alliance get called for ccodes that are not states (' +
                              inttostr(ccode1)+ ' and '+inttostr(ccode2)+' in '+inttostr(ayear)+'). ');
                        trace.message ('Alliance set to missing');
                     end;
               end;
      end;   {func all in range}

   function Talliance_array_obj.get_alliance_value (ccode1, ccode2 : ccode_range;  ayear : year_range) :
               alliance_value_type;
      begin
         get_alliance_value := initialized_value;
         {First check that range is OK}
         if all_in_range (ccode1, ccode2, ayear, true) then
            get_alliance_value := year_array[ayear].get_alliance_value(ccode1, ccode2);
         if result = initialized_value then
            begin
               trace.message('Alliance pair found to still have initialized value for ' +
                  inttostr(ccode1)+ ' ' + inttostr(ccode2)+' in ' + inttostr(ayear) + '.  Alliance set to no alliance.');
               get_alliance_value := no_alliance;
            end;
      end;

   function Talliance_array_obj.get_alliance_value (ccode1, ccode2 : ccode_range;  ayear : year_range;
           error_check : boolean) : alliance_value_type;
      begin
         get_alliance_value := initialized_value;
         {First check that range is OK}
         if all_in_range (ccode1, ccode2, ayear, error_check) then
            get_alliance_value := year_array[ayear].get_alliance_value(ccode1, ccode2);
         if (result = initialized_value) then
            begin
               if (error_check) then
                  trace.message('Alliance pair found to still have initialized value for ' +
                     inttostr(ccode1)+ ' ' + inttostr(ccode2)+' in ' + inttostr(ayear) + '.  Alliance set to no alliance.');
               get_alliance_value := no_alliance;
            end;
      end;

               { ---------------------------  }

   function Talliance_array_obj.get_year_array_ptr (ayear : year_range) : Talliance_year_obj_ptr;
      {This returns a pointer to a specific year structure.  Access the data in the returned
       variable by get_ptr^data^[cc1, cc2].  Note:  This returns a pointer to the actual
       data.  It must be copied if it's to be modified w/o screwing up the whole data!}
      begin
         if (initialized) and (ayear >= first_partition_year) and (ayear <= last_partition_year) then
            get_year_array_ptr := @year_array[ayear]
         else
            begin
               EUGeneError ('Alliance_array get year array called before initialization',
                               10, stop, error_log);
               trace.message ('Check program coding [notify programmer].'  );
            end;
      end;


   procedure Talliance_array_obj.make_copy_of_alliance_year_array (year : year_range;
                                         var copy : Talliance_year_obj);
   {This procedure will copy one year
    of the data from the alliance object.  The copy can then be manipulated.}
      {The var with the object will be declared in the calling procedure}
      begin
         if not(initialized) then
            begin
               EUGeneError ('Alliance_array copy_alliance_year_obj called before initialization',
                               5, continue, error_log);
               trace.message ('Copy is set to initialized values only.  Source object not copied. ' );
            end
         else
            begin
               try
                  copy := Talliance_year_obj.init;  {this just sets to nil}
                  new (copy.data);
                  {copy the full bloc of alliance data.  Can do this in one step
                   bc the year data is just a straight array}
                  copy.data^ := self.year_array[year].data^;
               finally
               end;
            end;

      end;

   procedure Talliance_array_obj.build_similarity_table (ccode1, ccode2 : ccode_range; ayear : year_range;
         for_region : region_type; weight : s_weighting_range; const sys_capability_data : Tsys_capability_array_obj;
         var result_table : similarity_table_type; var n : num_countries_range; var dmax : real);
         {builds similarity table, probably just for output, but could be used elsewhere}
      var row, column : integer;   {these could go 1 beyond range of 1..4}
          cc1value, cc2value, partner_ccode : ccode_range;
          cap : real;
      begin
         {Note: this code should match what's in the alliance year coding section to avoid errors!}
         n := 0;
         dmax := 0;
            {Row, column 1 is defense pact, 2 is nonaggression, 3 is entente, 4 is no alliance}
         for row := defense to no_alliance do
            for column := defense to no_alliance do
               result_table[row, column] := 0;
         for partner_ccode := min_ccode to max_ccode do
         {Want to include in table states that are involved in "for_region".
          Ignore others.  But, since we may be calculating a dyad where a state is
          not involved by the formula (since we are doing all dyads), also manually
          include the two dyadic members as well.  So, table of US vs. Europeans in
          1816 should have US as a table entry, so as to include US-US alliance.  }
            if nation_list.is_a_state (partner_ccode, ayear) then
             if ((nation_list.is_involved_in_region(partner_ccode, for_region, ayear)) or
                 (ccode1=partner_ccode) or (ccode2=partner_ccode)) then
               begin
                  cc1value := get_alliance_value(ccode1, partner_ccode, ayear);
                  cc2value := get_alliance_value(ccode2, partner_ccode, ayear);
                  if (cc1value >= defense) and (cc1value <=no_alliance) and
                     (cc2value >=defense) and (cc2value <= no_alliance) then
                     begin
                        case weight of
                           unweighted : begin
                              result_table[cc1value, cc2value] := result_table[cc1value, cc2value] + 1;
                              inc(n);
                              end;   {unwtd}
                           weighted : begin
                              cap := sys_capability_data.get_syscap(partner_ccode, ayear, false);
                              if cap <> missing_value then
                                 begin
                                    result_table[cc1value, cc2value] := result_table[cc1value, cc2value] + cap;
                                    dmax := dmax + cap;
                                 end;
                              end;   {weighted}
                        end;    {case}
                     end;
               end;

       end;    {build similarity table procedure}

      {  ---------------------------------------------------------   }

         {methods for Tau array object}
   constructor Ttau_array_obj.init (a_file_name : TFileName; year1, year2 : year_range);
        {init reads in from intermediate, external file.}

      var ayear : year_range;
          tau_year : tau_year_ptr;
          tau_array2 : tau_array_ptr2; {array[ccode_index_range] of single;}
          tau_file : file; {This is actually tau_file_type, which is an array of single,
             but is set as untyped here to allow fast search in procedure below.}
          tau_rec : tau_record_type;
          value : single;
          right_value, left_value : single;
          year_read, want_year : year_range;
          ccode1, ccode2 : ccode_range;
          start_mem, one_year_tau_memory_needed, heapneeded : longint;
          ccode_index_loop : ccode_index_range;
          left, right, year_start_pos : longint;
          Tau_trace : Ttrace_obj;

          buffer_array : array[0..((4*max_countries)+2+100)] of single;  {must be > (4*max_countries + 2)}
          buffer_pos, numread : integer;
          years_read : integer;
          prev_year : year_range;

      begin
         Tau_trace := nil;
         try
            try
               start_mem := memavail;
               if year1 > year2 then switch_year (year1, year2);
               trace.enter('Initializing tau data, '+inttostr(year1)+' to '+inttostr(year2));
               Tau_trace := TTrace_obj.init(trace.get_trace_level);
               self.first_partition_year := year1;
               self.last_partition_year := year2;

               if not(ccode_index.initialized) then
                  begin
                     EUGeneError ('Tau array creation called before ccode_index initialized',
                                     5, stop, error_log);
                     halt;
                  end;
               if not(nation_list.initialized) then
                  begin
                     EUGeneError ('Tau array creation called before nation_list initialized',
                                     5, stop, error_log);
                     halt;
                  end;

               {first initialize the data structure.  Give every legitimate state pair
                the initialized value.}
               Tau_trace.message ('Initializing tau structure');
               one_year_tau_memory_needed := Ttau_array_obj_mem_per_year;
               heapneeded := TTau_array_obj_mem_overhead +
                             (last_partition_year - first_partition_year + 1) *
                                one_year_tau_memory_needed;
               if debug[4] then
                  begin
                     Tau_trace.message ('Tau array size calculation');
                     Tau_trace.message ('Calc is that '+inttostr(one_year_tau_memory_needed)+' needed per year.');
                     Tau_trace.message ('Calc is that '+inttostr(heapneeded)+' needed for all years.');
                     Tau_trace.message ('Max avail mem block is '+inttostr(MaxAvail));
                  end;
               if MaxAvail <= (heapneeded) then
                  begin
                     EUGeneError ('Not enough memory for tau structure. ',
                                     5, stop, error_log);
                  end;
               for ayear := min_year to max_year do
                  if ( (ayear >= self.first_partition_year) and (ayear <= self.last_partition_year) ) then
                     begin
                        new (tau_year);
                        year_array[ayear] := tau_year;
                        for ccode_index_loop := min_ccode_index to max_ccode_index do
                           year_array[ayear]^[ccode_index_loop] := nil;
                        for ccode1 := min_ccode to max_ccode do
                           if nation_list.is_a_state (ccode1, ayear) then
                              begin
                                 Tau_trace.tick ('Executing Procedure: Initializing Tau Structure',0);
                                 new (tau_array2);
                                 year_array[ayear]^[ccode_index.index(ccode1)] := tau_array2;
                                 for ccode2 := min_ccode to max_ccode do
                                    if nation_list.is_a_state (ccode2, ayear) then
                                       begin
                                          year_array[ayear]^[ccode_index.index(ccode1)]^
                                                    [ccode_index.index(ccode2)].global := missing_value;
                                          year_array[ayear]^[ccode_index.index(ccode1)]^
                                                    [ccode_index.index(ccode2)].regional[1] := missing_value;
                                          year_array[ayear]^[ccode_index.index(ccode1)]^
                                                    [ccode_index.index(ccode2)].regional[2] := missing_value;
                                       end;
                              end;
                     end
                  else year_array[ayear] := nil;
                  Tau_trace.tickdone;


               {This is set to read binary file, where order of #s is
                   year cc1 cc2 taurec cc2 taurec year cc1 cc2 taurec etc.
                   So, when see a value that is in year range, it marks begin of new rec.}
               {This assumes that taus are in file in year order, which they should be,
                   since they were output by this program.}

               Tau_trace.message ('Reading Taus from external file, '+inttostr(first_partition_year)+
                  ' to '+inttostr(last_partition_year));
               assignFile (tau_file, a_file_name);
               reset (tau_file, sizeof(single));
               blockread (tau_file, value, 1, numread);
                  {find the beginning section of what needs to be read and processed}
                  {binary search for any value of year < first.  Then go on to sequential.}
               if value < first_partition_year then  {not at first record, so search}
                 begin
                    want_year := first_partition_year - 1;  {need to find some record that
                        is at most one year before what I want here.  }
                    {file starts at position 0}
                    left := 0;
                        {find the leftmost and rightmost years}
                    right := FileSize (tau_file);
                    seek (tau_file, left);
                    blockread (tau_file, left_value, 1, numread);
                    seek (tau_file, right-1);
                    repeat
                          Tau_trace.tick ( 'Executing Procedure: Scanning Tau File, '+inttostr(first_partition_year)+' to '
                                +inttostr(last_partition_year),0);
                          blockread (tau_file, right_value, 1, numread);
                          if not ((right_value >= min_year) and (right_value <= max_year)) then
                             seek (tau_file, FilePos(tau_file)-2);
                          {trace.message (inttostr(filePos(tau_file)));  }
                    until ((right_value >= min_year) and (right_value <= max_year));
                    tau_trace.tickdone;

                    repeat
                          {left and right are file positions.  Guess how far between l and r
                           the desired year will fall}
                           {pure binary search would be
                            seek (tau_file, ((left + right) div 2));  }
                           {Improve on that by moving closer to where year should be.}
                       seek (tau_file, trunc(left+(right-left) * ((want_year-left_value) /
                                                          (right_value-left_value+1)) ));
                       {find year value at or before this spot}
                       repeat
                          Tau_trace.tick ('Executing Procedure: Scanning Tau File, '+inttostr(first_partition_year)+' to '
                                +inttostr(last_partition_year),0);
                          blockread (tau_file, value, 1, numread);
                          if not ((value >= min_year) and (value <= max_year)) then
                             seek (tau_file, FilePos(tau_file)-2);
                       until ((value >= min_year) and (value <= max_year));
                       if value < want_year then    {want to search in right half of file}
                          left := FilePos(tau_file) - 1    {point at the year}
                       else     {value read was correct, or was more than what I want,
                                 so need to search left half of file}
                          right := FilePos(tau_file) - 1;  {point at the year}
                        {find the years at the leftmost and rightmost spot}
                       seek (tau_file, left);
                       repeat
                          blockread (tau_file, left_value, 1, numread);
                          if not ((left_value >= min_year) and (left_value <= max_year)) then
                             seek (tau_file, FilePos(tau_file)-2);
                       until ((left_value >= min_year) and (left_value <= max_year));
                       seek (tau_file, right);
                       repeat
                          blockread (tau_file, right_value, 1, numread);
                          if not ((right_value >= min_year) and (right_value <= max_year)) then
                             seek (tau_file, FilePos(tau_file)-2);
                       until ((right_value >= min_year) and (right_value <= max_year));

                    until (value = want_year) or (left > right);
                    {when exit here, file is positioned to year just before where I want it}
                 end;
               Tau_trace.tickdone;

                    {get to first record of year I want}
               while (value < first_partition_year) and (not eof(tau_file)) do
                  begin
                     blockread (tau_file, value, 1, numread);
                     Tau_trace.tick ('Executing Procedure: Scanning Tau File, '+inttostr(first_partition_year)+' to '
                                +inttostr(last_partition_year),0);
                  end;
               Tau_trace.tickdone;
               year_read := trunc(value);

                       {now read records, saving ccodes and tau values}
               years_read := 0;
               prev_year := 1816;
               seek (tau_file, FilePos(tau_file)-1);  {reset to pre-read of year}
               while (not eof(tau_file)) and (year_read <= last_partition_year) do
                 begin
                   year_start_pos := FilePos(tau_file);
                         {this will read more than a full year of values.  Each year has
                          year, ccode1, and an undefined # of 4 singles, ccode2 global regional1 regional2.}
                     {This reads 4*max+2 reals, and stores the # read in "numread".
                      Numread will be < 3*max+2 iff the blockread hit the end of the file.}
                   blockread (tau_file, buffer_array, 4*max_countries+2, numread);
                   buffer_pos := 0;
                   year_read := trunc(buffer_array[buffer_pos]);
                   inc(buffer_pos);
                   ccode1 := trunc(buffer_array[buffer_pos]);
                   inc(buffer_pos);
                   while ((buffer_array[buffer_pos] >= min_ccode) and
                          (buffer_array[buffer_pos] <= max_ccode) and
                          (buffer_pos<numread) ) do
                     begin
                       ccode2 := trunc(buffer_array[buffer_pos]);
                       inc(buffer_pos);
                       {order here important - written in order of global, then regional[1], then r[2]}
                       tau_rec.global := buffer_array[buffer_pos];
                       inc(buffer_pos);
                       tau_rec.regional[1] := buffer_array[buffer_pos];
                       inc(buffer_pos);
                       tau_rec.regional[2] := buffer_array[buffer_pos];
                       inc(buffer_pos);
                       if ((tau_rec.global > 1) or (tau_rec.global < -1) or
                           (tau_rec.regional[1] > 1) or (tau_rec.regional[1] < -1) or
                           (tau_rec.regional[2] > 1) or (tau_rec.regional[2] < -1) ) then
                            EUGeneError ('Tau values read from file are out of range',5, continue, error_log);

                       if (year_read >= first_partition_year) and (year_read <= last_partition_year) then
                         year_array[year_read]^[ccode_index.index(ccode1)]^
                                               [ccode_index.index(ccode2)] := tau_rec;
                     end;     {while}
                   {here, value was out of ccode range, so it must be the next year,
                    or have hit end of buffer.  So, reset the file pointer to be at this
                    location, so that can read a new set of buffer records at the top
                    of the loop.}
                   if (buffer_pos < numread) then seek (tau_file, year_start_pos + buffer_pos )
                     else
                        begin
                          {When buffer_pos >= numread at this point, we have reached the end
                           of the block read.  This will occur only when the block read to/past
                           the end of file.  }
                        end;
                   if year_read > prev_year then
                      begin
                         Tau_trace.tick ('Executing Procedure: Reading Tau File, ' +
                                inttostr(first_partition_year)+' to '+inttostr(last_partition_year),
                                (last_partition_year-first_partition_year+1));
                         inc(years_read);
                         prev_year := year_read;
                      end;

                 end;    {while not eof tau_file}
               Tau_trace.tickdone;
               created := true;
            finally
               CloseFile (tau_file);
               Tau_trace.free;
               trace.message (' required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
               trace.exit('Finished initializing tau data');
            end;    {finally}
         except
            on EUserInterrupt do raise;
            on EInOutError do
              begin
                 FileErrorBox.maindo ('Error opening file "'+a_file_name+ '"',
                                      'File could not be opened for input.',
                                      'File may be in use by another program, or may be missing.');
                 FileErrorBox.showmodal;
                 raise;
              end;
         end;
      end;       {proc init}

   destructor Ttau_array_obj.destroy ;
      var ayear : year_range;
          ccode1 : ccode_range;
      begin
         try
            if self <> nil then
            begin
               for ayear := min_year to max_year do
                  if year_array[ayear] <> nil then
                     begin
                       for ccode1 := min_ccode to max_ccode do
                         if year_array[ayear]^[ccode_index.index(ccode1)] <> nil then
                            begin
                               dispose (year_array[ayear]^[ccode_index.index(ccode1)]);
                               year_array[ayear]^[ccode_index.index(ccode1)] := nil;
                            end;
                       dispose (year_array[ayear]);
                       year_array[ayear] := nil;
                     end;
               created := false;
               inherited destroy;
            end;  {if self <> nil...}
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;                     {destructor }

   procedure Ttau_array_obj.modifyTauForWarReason (start_year, end_year : year_range);
         {in War and Reason, Bdm changes taus of 1.0 to 0.999, see p. 291}
      var ayear : year_range;
          ccode1, ccode2 : ccode_range;
      begin
         for ayear := start_year to end_year do
            for ccode1 := min_ccode to max_ccode do
               if nation_list.is_a_state (ccode1, ayear) then
                  for ccode2 := min_ccode to max_ccode do
                     if nation_list.is_a_state (ccode2, ayear) then
                        if ccode1 <> ccode2 then    {leave cc1 vs cc2 same, at 1}
                        begin
                           if year_array[ayear]^[ccode_index.index(ccode1)]^
                                              [ccode_index.index(ccode2)].global = 1.0 then
                              year_array[ayear]^[ccode_index.index(ccode1)]^
                                              [ccode_index.index(ccode2)].global := 0.999;
                           if year_array[ayear]^[ccode_index.index(ccode1)]^
                                              [ccode_index.index(ccode2)].regional[1] = 1.0 then
                              year_array[ayear]^[ccode_index.index(ccode1)]^
                                              [ccode_index.index(ccode2)].regional[1] := 0.999;
                           if year_array[ayear]^[ccode_index.index(ccode1)]^
                                              [ccode_index.index(ccode2)].regional[2] = 1.0 then
                              year_array[ayear]^[ccode_index.index(ccode1)]^
                                              [ccode_index.index(ccode2)].regional[2] := 0.999;

                        end;
      end;

   function Ttau_array_obj.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then if created=true then initialized := true;
      end;

   function Ttau_array_obj.get_first_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Tau get called before initialization',
                               5, stop, error_log);
            end
         else
            get_first_partition_year := first_partition_year;
      end;

   function Ttau_array_obj.get_last_partition_year : year_range;
      begin
        if not(initialized) then
            begin
               EUGeneError ('Tau get called before initialization',
                               5, stop, error_log);
            end
         else
            get_last_partition_year := last_partition_year;
      end;

   function Ttau_array_obj.all_in_range (ccode1, ccode2 : ccode_range;  ayear : year_range;
            which_region : region_type;  error_check: boolean) : boolean;
      begin
         all_in_range := true;
         if (ccode1=0) or (ccode2=0) or (which_region=none) then
            all_in_range := false
         else
         if not(initialized) then
            begin
               all_in_range := false;
               if error_check then
                  EUGeneError ('Tau get called before initialization',
                                     5, stop, error_log);
            end
         else
         if not ((ayear >= get_first_partition_year) and (ayear <= get_last_partition_year)) then
            begin
               all_in_range := false;
               if error_check then
                  begin
                     EUGeneError ('Internal Error in program - called get tau regional with year outside partition',
                                     5, continue, error_log);
                     trace.message ('Tau set to missing_value');
                  end;
            end
         else
            if not ((nation_list.is_a_state(ccode1, ayear)) and (nation_list.is_a_state(ccode2, ayear)) ) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        trace.message ('Error in ccode-years: called get tau regional for invalid ccodes given ccodes/year'+
                              inttostr(ccode1)+' ' + inttostr(ccode2)+ ' in '+inttostr(ayear));
                        trace.message ('Tau set to missing_value');
                     end;
               end
         else
            if not ((which_region >= europe) and (which_region <= globe)) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        trace.message ('Region Error - called Get a value from get tau for invalid region for ccodes '+inttostr(ccode1)+' '+inttostr(ccode2)+' in '+inttostr(ayear));
                        trace.message ('risk value set to missing');
                     end;
               end;
      end;   {all in range}

   function Ttau_array_obj.get_tau_value_global (ccode1, ccode2 : ccode_range;  ayear : year_range) : single;
      {return value of tau from matrix for this year-pair.}
      {this version of the procedure always does error checking}
      begin
         get_tau_value_global := missing_value;
			 if all_in_range (ccode1, ccode2, ayear, globe, true) then
            begin
               get_tau_value_global := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].global;
            end;
      end;

   function Ttau_array_obj.get_tau_value_global (ccode1, ccode2 : ccode_range;  ayear : year_range; error_check : boolean) : single;
      {return value of tau from matrix for this year-pair.}
      {this version of the procedure MIGHT do error checking}
      begin
         get_tau_value_global := missing_value;
			 if all_in_range (ccode1, ccode2, ayear, globe, error_check) then
            begin
               get_tau_value_global := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].global;
            end;
      end;

   function Ttau_array_obj.get_tau_value_regional (ccode1, ccode2 : ccode_range;  ayear : year_range;
            which_region : region_type) : single;
      {Return value of tau from matrix for this year-pair.
       Region is a region of the world regular code.}
      {With this call want to do regular error checking}
      begin
         get_tau_value_regional := missing_value;
			 if all_in_range (ccode1, ccode2, ayear, which_region, true) then
            begin
               if ccode1=ccode2 then
                  get_tau_value_regional := 1
               else
                  begin
                     {Want to return the tau value for the specified region, which MUST equal the home
                      region of either 1 or 2.}
                     {Note:  the all_in_range procedure checked for valid region, so if this is called
                      with region none or invalid ccode1 it will be OK.}
                     if which_region = nation_list.get_home_region(ccode1) then
                        get_tau_value_regional :=
                           year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].regional[1]
                     else if which_region = nation_list.get_home_region(ccode2) then
                        get_tau_value_regional :=
                           year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].regional[2]
                     else
                        {in this version of the procedure, always check/report this error}
                        begin
                           EUGeneError (('Tried to get tau regional for a region not stored in this record, for cc-cc-year '+
                                 inttostr(ccode1)+' '+inttostr(ccode2)+' '+inttostr(ayear)), 3, continue, error_log);
                        end;
                  end;
            end;
      end;

   function Ttau_array_obj.get_tau_value_regional (ccode1, ccode2 : ccode_range;  ayear : year_range;
            which_region : region_type; error_check : boolean) : single;
      {Return value of tau from matrix for this year-pair.
       Region is a region of the world regular code.}
      {With this call MIGHT or MIGHT NOT want to do regular error checking/reporting}
      begin
         get_tau_value_regional := missing_value;
			 if all_in_range (ccode1, ccode2, ayear, which_region, error_check) then
            begin
               if ccode1=ccode2 then
                  get_tau_value_regional := 1
               else
                  begin
                     {Want to return the tau value for the specified region, which MUST equal the home
                      region of either 1 or 2.}
                     {Note:  the all_in_range procedure checked for valid region, so if this is called
                      with region none or invalid ccode1 it will be OK.}
                     if which_region = nation_list.get_home_region(ccode1) then
                        get_tau_value_regional :=
                           year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].regional[1]
                     else if which_region = nation_list.get_home_region(ccode2) then
                        get_tau_value_regional :=
                           year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].regional[2]
                     else
                        {in this version of the procedure, might check/report this error}
                        begin
                           if error_check then
                              EUGeneError (('Tried to get tau regional for a region not stored in this record, for cc-cc-year '+
                                     inttostr(ccode1)+' '+inttostr(ccode2)+' '+inttostr(ayear)), 3, continue, error_log);
                        end;
                  end;
            end;
      end;

      {  -------------------------------------------------------------   }

   {methods for Ttau_year_obj. }
   constructor Ttau_year_obj.init (ayear : year_range; const start_taus: Ttau_array_obj);
       {creates an object with a specified year of data from the main tau array}
      var ccode1, ccode2 : ccode_range;
          ccode_index_loop : ccode_index_range;
          tau_array2 : tau_array_ptr2;
      begin
         try
             for ccode_index_loop := min_ccode_index to max_ccode_index do
                 data[ccode_index_loop] := nil;
             for ccode1 := min_ccode to max_ccode do
                if nation_list.is_a_state (ccode1, ayear) then
                   begin
                      new (tau_array2);
                      data[ccode_index.index(ccode1)] := tau_array2;
                      for ccode2 := min_ccode to max_ccode do
                         if nation_list.is_a_state (ccode2, ayear) then
                            begin
                               data[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)] :=
                                  start_taus.year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)];

                               {This was before adding a 2nd regional tau.}
                               {data[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].global :=
                                  start_taus.get_tau_value_global (ccode1, ccode2, ayear);
                               data[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].regional :=
                                  start_taus.get_tau_value_regional (ccode1, ccode2, ayear, ccode1); }
                            end;
                   end;
         finally
         end;
      end;

   destructor Ttau_year_obj.destroy;
      var ccode_index_loop : ccode_index_range;
      begin
         try
            if self <> nil then
              begin
                 for ccode_index_loop := min_ccode_index to max_ccode_index do
                   if data[ccode_index_loop] <> nil then
                      begin
                         dispose (data[ccode_index_loop]);
                         data[ccode_index_loop] := nil;
                      end;
                 inherited destroy;
              end;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;

   function Ttau_year_obj.get_tau_value_global (ccode1, ccode2 : ccode_range) : single;
      begin
         get_tau_value_global := data[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].global;
      end;

   function Ttau_year_obj.get_tau_value_regional (ccode1, ccode2 : ccode_range; which_region : region_type) : single;
      begin
         if ccode1=ccode2 then
            get_tau_value_regional := 1
         else
            begin
               {Want to return the tau value for the specified region, which MUST equal the home
                region of either 1 or 2.}
               if which_region = nation_list.get_home_region(ccode1) then
                  get_tau_value_regional := data[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].regional[1]
               else if which_region = nation_list.get_home_region(ccode2) then
                  get_tau_value_regional := data[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].regional[2]
               else
                  begin
                     EUGeneError ('Tried to get tau regional for a region not stored in this record, for cc-cc-year '+
                           inttostr(ccode1)+' '+inttostr(ccode2), 3, continue, error_log);
                  end;
            end;
      end;

   procedure Ttau_year_obj.set_tau_value_global (ccode1, ccode2 : ccode_range;  value : single);
      begin
         data[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].global := value;
      end;

   procedure Ttau_year_obj.set_tau_value_regional (ccode1, ccode2 : ccode_range; value : single;
                                         which_state : ccode_range);
        {want to put this tau in spot 1 or 2 depending on state}
      begin
         if which_state=ccode1 then
            data[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].regional[1] := value
         else if which_state=ccode2 then
            data[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].regional[2] := value
         else
            begin
               EUGeneError ('Tried to set tau regional for mismatched ccode, which_state',3,continue,error_log);
            end;
      end;


      {  ---------------------------------------------------------   }

         {methods for s array object}
   constructor Ts_array_obj.init (a_file_name : TFileName; year1, year2 : year_range);
        {init reads in from intermediate, external file.}

      var ayear : year_range;
          s_year : s_year_ptr;
          s_array2 : s_array_ptr2; {array[ccode_index_range] of single;}
          s_file : file; {s_file_type;}
          s_rec : s_record_type;
          value : single;
          right_value, left_value : single;
          year_read, want_year : year_range;
          ccode1, ccode2 : ccode_range;
          start_mem, one_year_s_memory_needed, heapneeded : longint;
          ccode_index_loop : ccode_index_range;
          left, right, year_start_pos : longint;
          s_trace : Ttrace_obj;

          buffer_array : array[0..2000] of single;  {must be > (7*max_countries + 2)}
          buffer_pos, numread : longint;
          years_read : integer;
          prev_year : year_range;
          weight : s_weighting_range;

      begin
         s_trace := nil;
         try
            try
               start_mem := memavail;
               if year1 > year2 then switch_year (year1, year2);
               trace.enter('Initializing s data, '+inttostr(year1)+' to '+inttostr(year2));
               s_trace := TTrace_obj.init(trace.get_trace_level);
               self.first_partition_year := year1;
               self.last_partition_year := year2;

               if not(ccode_index.initialized) then
                  begin
                     EUGeneError ('s array creation called before ccode_index initialized',
                                     5, stop, error_log);
                     halt;
                  end;
               if not(nation_list.initialized) then
                  begin
                     EUGeneError ('s array creation called before nation_list initialized',
                                     5, stop, error_log);
                     halt;
                  end;

               {first initialize the data structure.  Give every legitimate state pair
                the initialized value.}
               s_trace.message ('Initializing s structure');
               one_year_s_memory_needed := Ts_array_obj_mem_per_year;
               heapneeded := Ts_array_obj_mem_overhead +
                             (last_partition_year - first_partition_year + 1) *
                                one_year_s_memory_needed;
               if debug[4] then
                  begin
                     s_trace.message ('s array size calculation');
                     s_trace.message ('Calc is that '+inttostr(one_year_s_memory_needed)+' needed per year.');
                     s_trace.message ('Calc is that '+inttostr(heapneeded)+' needed for all years.');
                     s_trace.message ('Max avail mem block is '+inttostr(MaxAvail));
                  end;
               if MaxAvail <= (heapneeded) then
                  begin
                     EUGeneError ('Not enough memory for s structure. ',
                                     5, stop, error_log);
                  end;
               for ayear := min_year to max_year do
                  if ( (ayear >= self.first_partition_year) and (ayear <= self.last_partition_year) ) then
                     begin
                        new (s_year);
                        year_array[ayear] := s_year;
                        for ccode_index_loop := min_ccode_index to max_ccode_index do
                           year_array[ayear]^[ccode_index_loop] := nil;
                        for ccode1 := min_ccode to max_ccode do
                           if nation_list.is_a_state (ccode1, ayear) then
                              begin
                                 s_trace.tick ('Executing Procedure: Initializing s Structure',0);
                                 new (s_array2);
                                 year_array[ayear]^[ccode_index.index(ccode1)] := s_array2;
                                 for ccode2 := min_ccode to max_ccode do
                                    if nation_list.is_a_state (ccode2, ayear) then
                                       begin
                                          for weight := unweighted to weighted do
                                          begin
                                             year_array[ayear]^[ccode_index.index(ccode1)]^
                                                       [ccode_index.index(ccode2)].global[weight] := missing_value;
                                             year_array[ayear]^[ccode_index.index(ccode1)]^
                                                       [ccode_index.index(ccode2)].regional[weight][1] := missing_value;
                                             year_array[ayear]^[ccode_index.index(ccode1)]^
                                                       [ccode_index.index(ccode2)].regional[weight][2] := missing_value;
                                          end;
                                       end;
                              end;
                     end
                  else year_array[ayear] := nil;
                  s_trace.tickdone;


               {This is set to read binary file, where order of #s is
                   year cc1 cc2 srec cc2 srec year cc1 cc2 srec etc.
                   So, when see a value that is in year range, it marks begin of new rec.}
               {This assumes that ss are in file in year order, which they should be,
                   since they were output by this program.}

               s_trace.message ('Reading s from external file, '+inttostr(first_partition_year)+
                  ' to '+inttostr(last_partition_year));
               assignFile (s_file, a_file_name);
               reset (s_file, sizeof(single));
               blockread (s_file, value, 1, numread);
                  {find the beginning section of what needs to be read and processed}
                  {binary search for any value of year < first.  Then go on to sequential.}
               if value < first_partition_year then  {not at first record, so search}
                 begin
                    want_year := first_partition_year - 1;  {need to find some record that
                        is at most one year before what I want here.  }
                    {file starts at position 0}
                    left := 0;
                        {find the leftmost and rightmost years}
                    right := FileSize (s_file);
                    seek (s_file, left);
                    blockread (s_file, left_value, 1, numread);
                    seek (s_file, right-1);
                    repeat
                          s_trace.tick ( 'Executing Procedure: Scanning s File, '+inttostr(first_partition_year)+' to '
                                +inttostr(last_partition_year),0);
                          blockread (s_file, right_value, 1, numread);
                          if not ((right_value >= min_year) and (right_value <= max_year)) then
                             seek (s_file, FilePos(s_file)-2);
                          {trace.message (inttostr(filePos(s_file)));  }
                    until ((right_value >= min_year) and (right_value <= max_year));
                    s_trace.tickdone;

                    repeat
                          {left and right are file positions.  Guess how far between l and r
                           the desired year will fall}
                           {pure binary search would be
                            seek (s_file, ((left + right) div 2));  }
                           {Improve on that by moving closer to where year should be.}
                       seek (s_file, trunc(left+(right-left) * ((want_year-left_value) /
                                                          (right_value-left_value+1)) ));
                       {find year value at or before this spot}
                       repeat
                          s_trace.tick ('Executing Procedure: Scanning s File, '+inttostr(first_partition_year)+' to '
                                +inttostr(last_partition_year),0);
                          blockread (s_file, value, 1, numread);
                          if not ((value >= min_year) and (value <= max_year)) then
                             seek (s_file, FilePos(s_file)-2);
                       until ((value >= min_year) and (value <= max_year));
                       if value < want_year then    {want to search in right half of file}
                          left := FilePos(s_file) - 1    {point at the year}
                       else     {value read was correct, or was more than what I want,
                                 so need to search left half of file}
                          right := FilePos(s_file) - 1;  {point at the year}
                        {find the years at the leftmost and rightmost spot}
                       seek (s_file, left);
                       repeat
                          blockread (s_file, left_value, 1, numread);
                          if not ((left_value >= min_year) and (left_value <= max_year)) then
                             seek (s_file, FilePos(s_file)-2);
                       until ((left_value >= min_year) and (left_value <= max_year));
                       seek (s_file, right);
                       repeat
                          blockread (s_file, right_value, 1, numread);
                          if not ((right_value >= min_year) and (right_value <= max_year)) then
                             seek (s_file, FilePos(s_file)-2);
                       until ((right_value >= min_year) and (right_value <= max_year));

                    until (value = want_year) or (left > right);
                    {when exit here, file is positioned to year just before where I want it}
                 end;
               s_trace.tickdone;

                    {get to first record of year I want}
               while (value < first_partition_year) and (not eof(s_file)) do
                  begin
                     blockread (s_file, value, 1, numread);
                     s_trace.tick ('Executing Procedure: Scanning s File, '+inttostr(first_partition_year)+' to '
                                +inttostr(last_partition_year),0);
                  end;
               s_trace.tickdone;
               year_read := trunc(value);

                       {now read records, saving ccodes and s values}
               years_read := 0;
               prev_year := 1816;
               seek (s_file, FilePos(s_file)-1);  {reset to pre-read of year}
               while (not eof(s_file)) and (year_read <= last_partition_year) do
                 begin
                   year_start_pos := FilePos(s_file);
                         {this will read more than a full year of values.  Each year has
                          year, ccode1, then an undefined # of singles, each single set is
                            ccode2 unweighted global regional[1] regional[2] then weighted global r1 r2.}
                     {This reads 7*max+2 reals, and stores the # read in "numread".
                      Numread will be < 7*max+2 iff the blockread hit the end of the file.}
                   blockread (s_file, buffer_array, 7*max_countries+2, numread);
                   buffer_pos := 0;
                   year_read := trunc(buffer_array[buffer_pos]);
                   inc(buffer_pos);
                   ccode1 := trunc(buffer_array[buffer_pos]);
                   inc(buffer_pos);
                   while ((buffer_array[buffer_pos] >= min_ccode) and
                          (buffer_array[buffer_pos] <= max_ccode) and
                          (buffer_pos<numread) ) do
                     begin
                       ccode2 := trunc(buffer_array[buffer_pos]);
                       inc(buffer_pos);
                       {order here important - written in order of global, then regional[1], then r[2]
                        And for all, unweighted, then weighted, as in euprocs1 compute and save s procedure.}
                       for weight := unweighted to weighted do
                       begin
                          s_rec.global[weight] := buffer_array[buffer_pos];
                          inc(buffer_pos);
                          s_rec.regional[weight][1] := buffer_array[buffer_pos];
                          inc(buffer_pos);
                          s_rec.regional[weight][2] := buffer_array[buffer_pos];
                          inc(buffer_pos);
                          if ( ( ((s_rec.global[weight] > 1) or (s_rec.global[weight] < -1)) and (not (s_rec.global[weight] = missing_value))) or
                               ( ((s_rec.regional[weight][1] > 1) or (s_rec.regional[weight][1] < -1)) and (not (s_rec.regional[weight][1] = missing_value))) or
                               ( ((s_rec.regional[weight][2] > 1) or (s_rec.regional[weight][2] < -1)) and (not (s_rec.regional[weight][2] = missing_value))) ) then
                               EUGeneError ('s values read from file are out of range',5, continue, error_log);
                       end;

                       if (year_read >= first_partition_year) and (year_read <= last_partition_year) then
                         year_array[year_read]^[ccode_index.index(ccode1)]^
                                               [ccode_index.index(ccode2)] := s_rec;
                     end;     {while}
                   {here, value was out of ccode range, so it must be the next year,
                    or have hit end of buffer.  So, reset the file pointer to be at this
                    location, so that can read a new set of buffer records at the top
                    of the loop.}
                   if (buffer_pos < numread) then seek (s_file, year_start_pos + buffer_pos )
                     else
                        begin
                          {When buffer_pos >= numread at this point, we have reached the end
                           of the block read.  This will occur only when the block read to/past
                           the end of file.  }
                        end;
                   if year_read > prev_year then
                      begin
                         s_trace.tick ('Executing Procedure: Reading s File, ' +
                                inttostr(first_partition_year)+' to '+inttostr(last_partition_year),
                                (last_partition_year-first_partition_year+1));
                         inc(years_read);
                         prev_year := year_read;
                      end;

                 end;    {while not eof s_file}
               s_trace.tickdone;
               created := true;
            finally
               CloseFile (s_file);
               s_trace.free;
               trace.message (' required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
               trace.exit('Finished initializing s data');
            end;    {finally}
         except
            on EUserInterrupt do raise;
            on EInOutError do
              begin
                 FileErrorBox.maindo ('Error opening file "'+a_file_name+ '"',
                                      'File could not be opened for input.',
                                      'File may be in use by another program, or may be missing.');
                 FileErrorBox.showmodal;
                 raise;
              end;
         end;
      end;       {proc init}

   destructor Ts_array_obj.destroy ;
      var ayear : year_range;
          ccode1 : ccode_range;
      begin
         try
            if self <> nil then
            begin
               for ayear := min_year to max_year do
                  if year_array[ayear] <> nil then
                     begin
                       for ccode1 := min_ccode to max_ccode do
                         if year_array[ayear]^[ccode_index.index(ccode1)] <> nil then
                            begin
                               dispose (year_array[ayear]^[ccode_index.index(ccode1)]);
                               year_array[ayear]^[ccode_index.index(ccode1)] := nil;
                            end;
                       dispose (year_array[ayear]);
                       year_array[ayear] := nil;
                     end;
               created := false;
               inherited destroy;
            end;  {if self <> nil...}
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;                     {destructor }

   procedure Ts_array_obj.modifySForWarReason (start_year, end_year : year_range);
         {in War and Reason, Bdm changes taus of 1.0 to 0.999, see p. 291}
      var ayear : year_range;
          ccode1, ccode2 : ccode_range;
          weight : s_weighting_range;
      begin
         for ayear := start_year to end_year do
            for ccode1 := min_ccode to max_ccode do
               if nation_list.is_a_state (ccode1, ayear) then
                  for ccode2 := min_ccode to max_ccode do
                     if nation_list.is_a_state (ccode2, ayear) then
                        if ccode1 <> ccode2 then    {leave cc1 = cc2 same, at 1}
                        begin
                           for weight := unweighted to weighted do
                           begin
                              if year_array[ayear]^[ccode_index.index(ccode1)]^
                                                 [ccode_index.index(ccode2)].global[weight] = 1.0 then
                                 year_array[ayear]^[ccode_index.index(ccode1)]^
                                                 [ccode_index.index(ccode2)].global[weight] := 0.999;
                              if year_array[ayear]^[ccode_index.index(ccode1)]^
                                                 [ccode_index.index(ccode2)].regional[weight][1] = 1.0 then
                                 year_array[ayear]^[ccode_index.index(ccode1)]^
                                                 [ccode_index.index(ccode2)].regional[weight][1] := 0.999;
                              if year_array[ayear]^[ccode_index.index(ccode1)]^
                                                 [ccode_index.index(ccode2)].regional[weight][2] = 1.0 then
                                 year_array[ayear]^[ccode_index.index(ccode1)]^
                                                 [ccode_index.index(ccode2)].regional[weight][2] := 0.999;
                           end;
                        end;
      end;

   function Ts_array_obj.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then if created=true then initialized := true;
      end;

   function Ts_array_obj.get_first_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('s get called before initialization',
                               5, stop, error_log);
            end
         else
            get_first_partition_year := first_partition_year;
      end;

   function Ts_array_obj.get_last_partition_year : year_range;
      begin
        if not(initialized) then
            begin
               EUGeneError ('s get called before initialization',
                               5, stop, error_log);
            end
         else
            get_last_partition_year := last_partition_year;
      end;

     { ---------------------------------  }

   function Ts_array_obj.all_in_range (ccode1, ccode2 : ccode_range;  ayear : year_range; error_check : boolean) : boolean;
      begin
         all_in_range := true;
         if not(initialized) then
            begin
               all_in_range := false;
               if error_check then
                  begin
                     EUGeneError ('s get called before initialization',
                               5, stop, error_log);
                  end;
            end
         else
         if not ((ayear >= get_first_partition_year) and (ayear <= get_last_partition_year)) then
            begin
               all_in_range := false;
               if error_check then
                  begin
                     EUGeneError ('Internal Error in program - called get s global with year outside partition',
                                     5, continue, error_log);
                     trace.message ('s set to missing_value');
                  end;
            end
         else
            if not ( (nation_list.is_a_state(ccode1, ayear)) and (nation_list.is_a_state(ccode2, ayear)) ) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        trace.message ('Error in ccode-years - called get s (global or regional) for invalid ccodes given year'+
                                      inttostr(ccode1)+' in '+inttostr(ayear));
                        trace.message ('s set to missing_value');
                     end;
               end;
      end;   {func all in range}

     { ---------------------------------  }

   function Ts_array_obj.get_s_value_global (ccode1, ccode2 : ccode_range;  ayear : year_range;
            weight : s_weighting_range) : single;
      {return value of s from matrix for this year-pair.}
      begin
         get_s_value_global := missing_value;
         if ccode1=ccode2 then
            get_s_value_global := 1
         else
         if all_in_range (ccode1, ccode2, ayear, true) then
            get_s_value_global := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].global[weight];
      end;

   function Ts_array_obj.get_s_value_global (ccode1, ccode2 : ccode_range;  ayear : year_range;
            weight : s_weighting_range;  error_check : boolean) : single;
      {return value of s from matrix for this year-pair.}
      begin
         get_s_value_global := missing_value;
         if ccode1=ccode2 then
            get_s_value_global := 1
         else
         if all_in_range (ccode1, ccode2, ayear, error_check) then
            get_s_value_global := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].global[weight];
      end;

     { ---------------------------------  }

   function Ts_array_obj.get_s_value_regional (ccode1, ccode2 : ccode_range;  ayear : year_range;
           which_region : region_type; weight : s_weighting_range) : single;
      {return value of s from matrix for this year-pair.}
      {In this proc always check errors}
      begin
         get_s_value_regional := missing_value;
         if ccode1=ccode2 then
            get_s_value_regional := 1
         else
         if all_in_range (ccode1, ccode2, ayear, true) then
            begin
               {Want to return the s value for the specified region, which MUST equal the home
                region of either 1 or 2.}
               if which_region = nation_list.get_home_region(ccode1) then
                  get_s_value_regional :=
                     year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].regional[weight][1]
               else if which_region = nation_list.get_home_region(ccode2) then
                  get_s_value_regional :=
                     year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].regional[weight][2]
               else
                  begin
                     EUGeneError (('Tried to get s regional for a region not stored in this record, for cc-cc-year '+
                        inttostr(ccode1)+' '+inttostr(ccode2)+' '+inttostr(ayear)), 3, continue, error_log);
                  end;
            end;
      end;

   function Ts_array_obj.get_s_value_regional (ccode1, ccode2 : ccode_range;  ayear : year_range;
           which_region : region_type; weight : s_weighting_range; error_check : boolean) : single;
      {return value of s from matrix for this year-pair.}
      begin
         get_s_value_regional := missing_value;
         if ccode1=ccode2 then
            get_s_value_regional := 1
         else
         if all_in_range (ccode1, ccode2, ayear, error_check) then
            begin
               {Want to return the s value for the specified region, which MUST equal the home
                region of either 1 or 2.}
               if which_region = nation_list.get_home_region(ccode1) then
                  get_s_value_regional :=
                     year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].regional[weight][1]
               else if which_region = nation_list.get_home_region(ccode2) then
                  get_s_value_regional :=
                     year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].regional[weight][2]
               else
                  begin
                     if error_check then
                        EUGeneError (('Tried to get s regional for a region not stored in this record, for cc-cc-year '+
                           inttostr(ccode1)+' '+inttostr(ccode2)+' '+inttostr(ayear)), 3, continue, error_log);
                  end;
            end;
      end;

      {  -------------------------------------------------------------   }

   {methods for Ts_year_obj. }
   constructor Ts_year_obj.init (ayear : year_range; const start_s: Ts_array_obj);
       {creates an object with a specified year of data from the main s array}
      var ccode1, ccode2 : ccode_range;
          ccode_index_loop : ccode_index_range;
          s_array2 : s_array_ptr2;
      begin
         try
             for ccode_index_loop := min_ccode_index to max_ccode_index do
                 data[ccode_index_loop] := nil;
             for ccode1 := min_ccode to max_ccode do
                if nation_list.is_a_state (ccode1, ayear) then
                   begin
                      new (s_array2);
                      data[ccode_index.index(ccode1)] := s_array2;
                      for ccode2 := min_ccode to max_ccode do
                         if nation_list.is_a_state (ccode2, ayear) then
                            begin
                               data[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)] :=
                                  start_s.year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)];

                            end;
                   end;
         finally
         end;
      end;

   destructor Ts_year_obj.destroy;
      var ccode_index_loop : ccode_index_range;
      begin
         try
            if self <> nil then
              begin
                 for ccode_index_loop := min_ccode_index to max_ccode_index do
                   if data[ccode_index_loop] <> nil then
                      begin
                         dispose (data[ccode_index_loop]);
                         data[ccode_index_loop] := nil;
                      end;
                 inherited destroy;
              end;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;

   function Ts_year_obj.get_s_value_global (ccode1, ccode2 : ccode_range; weight : s_weighting_range) : single;
      begin
         get_s_value_global := data[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].global[weight];
      end;

   function Ts_year_obj.get_s_value_regional (ccode1, ccode2 : ccode_range; which_region : region_type; weight : s_weighting_range) : single;
      begin
         if ccode1=ccode2 then
            get_s_value_regional := 1
         else
            begin
               {Want to return the s value for the specified region, which MUST equal the home
                region of either 1 or 2.}
               if which_region = nation_list.get_home_region(ccode1) then
                  get_s_value_regional := data[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].regional[weight][1]
               else if which_region = nation_list.get_home_region(ccode2) then
                  get_s_value_regional := data[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].regional[weight][2]
               else
                  begin
                     EUGeneError ('Tried to get s regional for a region not stored in this record, for cc-cc-year '+
                           inttostr(ccode1)+' '+inttostr(ccode2), 3, continue, error_log);
                  end;
            end;
      end;

   procedure Ts_year_obj.set_s_value_global (ccode1, ccode2 : ccode_range;
             weight : s_weighting_range; value : single);
      begin
         data[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].global[weight] := value;
      end;

   procedure Ts_year_obj.set_s_value_regional (ccode1, ccode2 : ccode_range; weight : s_weighting_range;
                value : single;  which_state : ccode_range);
        {want to put this s in spot 1 or 2 depending on state}
      begin
         if which_state=ccode1 then
            data[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].regional[weight][1] := value
         else if which_state=ccode2 then
            data[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].regional[weight][2] := value
         else
            begin
               EUGeneError ('Tried to set s regional for mismatched ccode, which_state',3,continue,error_log);
            end;
      end;


      {  -------------------------------------------------   }

      {Methods for EUs using War Trap methods for dyads}
   constructor TEUWarTrap_array_obj.init (a_file_name : TFileName; year1, year2 : year_range);
      var ayear : year_range;
          EUWarTrap_year : EUWarTrap_ccode_ptr;
          EUWarTrap_array2 : EUWarTrap_ccode_array_ptr2; {array[ccode_index_range] of single;}
          EUWarTrap_file : EUWarTrap_file_type;
          EUWarTrap_record_read, left_record, right_record : EUWarTrap_file_record_type;
          temp : year_range;
          ccode1, ccode2 : ccode_range;
          start_mem, one_year_EUWarTrap_memory_needed, heapneeded : longint;
          ccode_index_loop : ccode_index_range;
          want_year : year_range;
          left, right : longint;
          prevyear, num_read : longint;
          EU_trace : TTrace_obj;

      begin
         EU_Trace := nil;
         try
            try
               start_mem := memavail;
               if year1 > year2 then switch_year (year1, year2);
               trace.enter('Initializing EUWarTrap data, '+inttostr(year1)+' to '+inttostr(year2));
               self.first_partition_year := year1;
               self.last_partition_year := year2;

               if not(ccode_index.initialized) then
                  begin
                     EUGeneError ('EUWarReason array creation called before ccode_index initialized',
                                     5, stop, error_log);
                     halt;
                  end;
               if not(nation_list.initialized) then
                  begin
                     EUGeneError ('EUWarTrap array creation called before nation_list initialized',
                                     5, stop, error_log);
                     halt;
                  end;

               EU_trace := TTrace_obj.init(trace.get_trace_level);

               {first initialize the data structure.  Give every legitimate state pair
                the initialized value.}
               trace.message ('Initializing EUWarTrap structure');
               one_year_EUWarTrap_memory_needed := TEUWarTrap_array_obj_mem_per_year;
               heapneeded := TEUWarTrap_array_obj_mem_overhead +
                             (self.last_partition_year - self.first_partition_year + 1) *
                              one_year_EUWarTrap_memory_needed;
               if MaxAvail <= (heapneeded) then
                  begin
                     EUGeneError ('Not enough memory for EUWarTrap structure. ',
                                     5, stop, error_log);
                  end;
               for ayear := min_year to max_year do
                  if ( (ayear >= self.first_partition_year) and (ayear <= self.last_partition_year) ) then
                     begin
                        new (EUWarTrap_year);
                        year_array[ayear] := EUWarTrap_year;
                        for ccode_index_loop := min_ccode_index to max_ccode_index do
                           year_array[ayear]^[ccode_index_loop] := nil;
                        for ccode1 := min_ccode to max_ccode do
                           if nation_list.is_a_state (ccode1, ayear) then
                              begin
                                 new (EUWarTrap_array2);
                                 year_array[ayear]^[ccode_index.index(ccode1)] := EUWarTrap_array2;
                                 for ccode2 := min_ccode to max_ccode do
                                    if nation_list.is_a_state (ccode2, ayear) then
                                       begin
                                          year_array[ayear]^[ccode_index.index(ccode1)]^
                                                    [ccode_index.index(ccode2)].EUWarTrap := missing_value;
                                       end;
                              end;
                     end
                  else year_array[ayear] := nil;

               trace.message ('Reading EUWarTrap from external file, '+inttostr(first_partition_year)+
                  ' to '+inttostr(last_partition_year));
               assignFile (EUWarTrap_file, a_file_name);
               reset (EUWarTrap_file);
               read (EUWarTrap_file, EUWarTrap_record_read);


               {find the beginning section of what needs to be read and processed}
               {binary search for any value of year < first.  Then go on to sequential.}

               if EUWarTrap_record_read.year < first_partition_year then  {not at first record, so search}
                 begin
                    want_year := first_partition_year - 1;  {need to find some record that
                        is at most one year before what I want here.  }
                    {file starts at position 0}
                    left := 0;
                        {find the leftmost and rightmost years}
                    right := FileSize (EUWarTrap_file)-1;
                    seek (EUWarTrap_file, left);
                    read (EUWarTrap_file, left_record);
                    seek (EUWarTrap_file, right);
                    read (EUWarTrap_file, right_record);

                    repeat
                          {left and right are file positions.  Guess how far between l and r
                           the desired year will fall}
                           {pure binary search would be
                            seek (tau_file, ((left + right) div 2));  }
                           {Improve on that by moving closer to where year should be.}
                       seek (EUWarTrap_file, trunc(left+(right-left) * ((want_year-left_record.year) /
                                                          (right_record.year-left_record.year+1)) ));
                       EU_trace.tick ( 'Executing Procedure: Scanning EU (War Trap) File ',0);
                       read (EUWarTrap_file, EUWarTrap_record_read);
                       if EUWarTrap_record_read.year < want_year then    {want to search in right half of file}
                          begin
                             left := FilePos(EUWarTrap_file);
                             left_record := EUWarTrap_record_read;
                          end
                       else     {value read was correct, or was more than what I want,
                                 so need to search left half of file}
                          begin
                             right := FilePos(EUWarTrap_file);
                             right_record := EUWarTrap_record_read;
                          end;
                    until (EUWarTrap_record_read.year = want_year) or (left > right);
                    {when exit here, file is positioned to year just before where I want it}
                 end;
               EU_trace.tickdone;

                    {Now am one year before; get to first record of year I want}
               while (EUWarTrap_record_read.year < first_partition_year) and (not eof(EUWarTrap_file)) do
                  begin
                     read (EUWarTrap_file, EUWarTrap_record_read);
                     EU_trace.tick ( 'Executing Procedure: Scanning EU (War Trap) ',0);
                  end;
               EU_trace.tickdone;

               {have one record in memory.  Possibly store it before continuing read}
               if (EUWarTrap_record_read.year >= self.first_partition_year) and
                  (EUWarTrap_record_read.year <= self.last_partition_year) then
                  year_array[EUWarTrap_record_read.year]^[ccode_index.index(EUWarTrap_record_read.ccode1)]^
                            [ccode_index.index(EUWarTrap_record_read.ccode2)] := EUWarTrap_record_read.EUWarTrap_rec;

               num_read := 0;
               prevyear := 0;
               EU_trace.tick ('Executing Procedure: Reading EU (WarTrap) File, '+inttostr(self.first_partition_year)+
                             ' to '+inttostr(self.last_partition_year),
                             (self.last_partition_year-self.first_partition_year+1));
               while (not eof(EUWarTrap_file)) and (EUWarTrap_record_read.year <= self.last_partition_year) do
                  begin
                     read (EUWarTrap_file, EUWarTrap_record_read);
                     if (EUWarTrap_record_read.year >= self.first_partition_year) and
                        (EUWarTrap_record_read.year <= self.last_partition_year) then
                        year_array[EUWarTrap_record_read.year]^[ccode_index.index(EUWarTrap_record_read.ccode1)]^
                                  [ccode_index.index(EUWarTrap_record_read.ccode2)] := EUWarTrap_record_read.EUWarTrap_rec;
                     if EUWarTrap_record_read.year <> prevyear then
                        begin
                          EU_trace.tick ('Executing Procedure: Reading EU (WarTrap) File, '+inttostr(self.first_partition_year)+
                             ' to '+inttostr(self.last_partition_year),
                             (self.last_partition_year-self.first_partition_year+1));
                           prevyear := EUWarTrap_record_read.year;
                           inc(num_read);
                        end;
                 end;    {while not eof}
               EU_trace.tickdone;
               created := true;

            finally
               CloseFile (EUWarTrap_file);
               EU_trace.free;
               trace.message (' required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
               trace.exit('Finished initializing EUWarTrap data');
            end;
         except
            on EUserInterrupt do raise;
            on EInOutError do
              begin
                 FileErrorBox.maindo ('Error opening file "'+a_file_name+ '"',
                                      'File could not be opened for input.',
                                      'File may be in use by another program, or may be missing.');
                 FileErrorBox.showmodal;
                 raise;
              end;
         end;
      end;


   destructor TEUWarTrap_array_obj.destroy;
      var ayear : year_range;
          ccode1 : ccode_range;
      begin
         try
            if self <> nil then
            begin
               for ayear := min_year to max_year do
                  if year_array[ayear] <> nil then
                     begin
                       for ccode1 := min_ccode to max_ccode do
                         if year_array[ayear]^[ccode_index.index(ccode1)] <> nil then
                            begin
                               dispose (year_array[ayear]^[ccode_index.index(ccode1)]);
                               year_array[ayear]^[ccode_index.index(ccode1)] := nil;
                            end;
                        dispose (year_array[ayear]);
                        year_array[ayear] := nil;
                     end;
               created := false;
               inherited destroy;
            end;  {if self <> nil...}
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;                     {destructor }


   function TEUWarTrap_array_obj.get_first_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('EUWarTrap get_partition called before initialization',
                               5, stop, error_log);
            end
         else
            get_first_partition_year := first_partition_year;
      end;

   function TEUWarTrap_array_obj.get_last_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('EUWarTrap get_partition called before initialization',
                               5, stop, error_log);
            end
         else
            get_last_partition_year := last_partition_year;
      end;

   function TEUWarTrap_array_obj.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then if created=true then initialized := true;
      end;

   function TEUWarTrap_array_obj.all_in_range (ccode1, ccode2 : ccode_range; ayear : year_range; error_check : boolean) : boolean;
      begin
         all_in_range := true;
         if not(initialized) then
            begin
               all_in_range := false;
               if error_check then
                  EUGeneError ('Get EUWarTrap value get called before initialization',
                               5, stop, error_log);
            end
         else
         if not ((ayear >= get_first_partition_year) and (ayear <= get_last_partition_year)) then
            begin
               all_in_range := false;
               if error_check then
                  begin
                     EUGeneError ('Internal Error in program - called get EUWarTrap with year outside partition',
                                  5, continue, error_log);
                     trace.message ('EUWarTrap set to missing');
                  end;
            end
         else
            if not ( (nation_list.is_a_state(ccode1, ayear)) and (nation_list.is_a_state(ccode2, ayear)) ) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        trace.message ('Error in ccodes/year list: called get EUWarTrap for invalid ccodes given year'+inttostr(ccode1)+' in '+inttostr(ayear));
                        trace.message ('EUWarTrap set to missing');
                     end;
               end
      end;   {all in range}

   function TEUWarTrap_array_obj.get_EUWarTrap (ccode1, ccode2 : ccode_range; ayear : year_range) : single;
      {return value of EU from matrix for this year-pair.}
      begin
         get_EUWarTrap := missing_value;
         if all_in_range (ccode1, ccode2, ayear, true) then
            get_EUWarTrap := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].EUWarTrap;
      end;

   function TEUWarTrap_array_obj.get_EUWarTrap (ccode1, ccode2 : ccode_range; ayear : year_range; error_check : boolean) : single;
      {return value of EU from matrix for this year-pair.}
      begin
         get_EUWarTrap := missing_value;
         if all_in_range (ccode1, ccode2, ayear, error_check) then
            get_EUWarTrap := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].EUWarTrap;
      end;

      {  -------------------------------------------------   }

procedure specified_dyad_list_set (var user_selections : user_selection_type);
   var indyadfile : text;
       adyadrec, prev_rec : user_dyad_ptr;
       infilename : TFileName;
       indyad_trace : TTrace_obj;
   begin
      try
         try
            try
               {first, do I need to delete an old, existing list?}
               if user_selections.user_specified_dyad_list.first_dyad <> nil then
                  specified_dyad_list_delete (user_selections);
               indyad_trace := TTrace_obj.init(trace.get_trace_level);
               trace.enter ('Reading User Dyad File');
               assignfile (indyadfile, user_selections.user_specified_dyad_list.file_name);
               reset (indyadfile);

               try
                  indyad_trace.tick ('Executing Procedure: Read User Dyad Data',0);
                  if (eoln (indyadfile)) or (eof(indyadfile)) then
                     EUGeneError ('File with list of dyads is empty or has blank first line.  Correct this problem and try again.  This is a fatal error.',2, stop, error_log);
                  new (adyadrec);
                  adyadrec^.next_rec := nil;
{maybe:  if not eoln(indyadfile)     or    if not eof(indyadfile)..... begin   read stuff   end}
                  adyadrec^.ccode1 := read_csv_int (indyadfile);
                  adyadrec^.ccode2 := read_csv_int (indyadfile);
                  adyadrec^.styear := read_csv_int (indyadfile);
                  adyadrec^.endyear := read_csv_int (indyadfile);
                  if not eof(indyadfile) then readln (indyadfile);
                  user_selections.user_specified_dyad_list.first_dyad := adyadrec;
                  prev_rec := user_selections.user_specified_dyad_list.first_dyad;
                  repeat
                     if not eoln (indyadfile) then
                        begin
                           indyad_trace.tick ('Executing Procedure: Read User Dyad Data',0);
                           new (adyadrec);
                           adyadrec^.next_rec := nil;
                           adyadrec^.ccode1 := read_csv_int (indyadfile);
                           adyadrec^.ccode2 := read_csv_int (indyadfile);
                           adyadrec^.styear := read_csv_int (indyadfile);
                           adyadrec^.endyear := read_csv_int (indyadfile);
                           prev_rec^.next_rec := adyadrec;
                           prev_rec := adyadrec;
                        end;
                     if not eof(indyadfile) then readln (indyadfile);
                  until (eof (indyadfile));
               except
                  on EInOutError do
                    begin
                       FileErrorBox.maindo ('Error accessing file "'+user_selections.user_specified_dyad_list.file_name+ '"',
                                            'File read error.',
                                            'Please check file format and try again.');
                       FileErrorBox.showmodal;
                       raise;
                    end;
               end;

            finally
               CloseFile (indyadfile);
               indyad_trace.tickdone;
            end;
         except
            on EUserInterrupt do raise;
            on EInOutError do
              begin
                 FileErrorBox.maindo ('Error opening file "'+user_selections.user_specified_dyad_list.file_name+ '"',
                                      'File could not be opened for input.',
                                      'File may be in use by another program, or may be missing.');
                 FileErrorBox.showmodal;
                 raise;
              end;
         end;
      finally
         indyad_trace.free;
         trace.exit('Finished reading user dyads');
      end;


   end;

procedure specified_dyad_list_delete (var user_selections: user_selection_type);
   var curr_rec, prev_rec : user_dyad_ptr;
   begin
      prev_rec := user_selections.user_specified_dyad_list.first_dyad;
      while (prev_rec <> nil) and (prev_rec^.next_rec <> nil) do
         begin
            curr_rec := prev_rec^.next_rec;
            dispose(prev_rec);
            prev_rec := curr_rec;
         end;
      if prev_rec <> nil then dispose(prev_rec);
   end;

function want_user_dyad_year (user_selections : user_selection_type;
                             ccode1, ccode2 : ccode_range; ayear : year_range) : boolean;
   var curr_ptr : user_dyad_ptr;
       found : boolean;
   begin
      want_user_dyad_year := false;
      found := false;
      {find dyad on list;  it should always be there}
      {Need to look at all dyads on list, b/c a dyad could be listed 2x with different dates}
      curr_ptr := user_selections.user_specified_dyad_list.first_dyad;
      repeat
         repeat
            if curr_ptr <> nil then
               begin
                  if (min(curr_ptr^.ccode1, curr_ptr^.ccode2) = min (ccode1, ccode2)) and
                     (max(curr_ptr^.ccode1, curr_ptr^.ccode2) = max (ccode1, ccode2)) then
                     found := true;
                  if not found then curr_ptr := curr_ptr^.next_rec;
               end;
         until (found) or (curr_ptr = nil);
         if (found) and (curr_ptr <> nil) then
            begin
               {found the first dyad with these ccodes.  But check for additional dyads,
                in case user has the same dyad with two date ranges.}
               if (ayear >= min(curr_ptr^.styear, curr_ptr^.endyear)) and
                  (ayear <= max(curr_ptr^.styear, curr_ptr^.endyear)) then
               want_user_dyad_year := true;
            end;

         {Now move on to next dyad to continue search}
         found := false;
         if curr_ptr <> nil then curr_ptr := curr_ptr^.next_rec;
      until (result = true) or (curr_ptr = nil);

   end;


{ISO stuff}

constructor TISO_array_obj.init (ISO_main_file : TFileName; ISO_lookup_file : TFileName);
var
  ctr : integer;
  infile : text;
  abbrev_2_temp : string [2];
  abbrev_3_temp : string [3];
  entry : integer;

begin
  ctr := 0;
  created := false;

  {initialize the arrays}
  for ctr := 0 to max_iso_code do
    begin
        data[ctr].iso_code := initialized_value;
        data[ctr].cow_code := initialized_value;
        data[ctr].iso_abbrev2 := 'xx';
        data[ctr].iso_abbrev3 := 'xxx';
        data[ctr].iso_name_short := 'None';
        data[ctr].iso_name_full := 'None';
    end;

  for ctr := 0 to max_iso_code do
    begin
      lookup_table[ctr].iso_code := initialized_value;
      lookup_table[ctr].cow_code := initialized_value;
    end;

   {read in the data from the files}

   {do the main ISO file first}
   assignFile(infile, ISO_main_file);
   reset(infile);
   {skip header}
   readln(infile);

   while not eof(infile) do
      begin
      {read in to temp variables until we find the ISO number}
         abbrev_2_temp := read_csv_string(infile);
         abbrev_3_temp := read_csv_string(infile);

       {ISO number}
         entry := read_csv_int(infile);

       {go back to the data we stored before}
         data[entry].iso_abbrev2 := abbrev_2_temp;
         data[entry].iso_abbrev3 := abbrev_3_temp;

         data[entry].iso_code := entry;

         read_csv_string(infile);

        {read in the rest of the data we want}
         data[entry].iso_name_short := read_csv_string(infile);
         data[entry].iso_name_full := read_csv_string(infile);

        {skip to the next line}
         readln(infile);
      end;

      CloseFile(infile);

      {now the COW vs. ISO look up table}

      assignFile(infile, ISO_lookup_file);
      reset(infile);

      {skip header}
      readln(infile);

      while not eof(infile) do
        begin
           entry := read_csv_int(infile);

           lookup_table[entry].cow_code := entry;

           {skip section with country name}
           read_csv_string(infile);

           lookup_table[entry].iso_code := read_csv_int(infile);

           {go to next line}
           readln(infile);

        end;

      CloseFile(infile);

      created := true;

end;

destructor TISO_array_obj.destroy;
  begin
    
  end;

function TISO_array_obj.initialized : boolean;
  begin
     initialized := false;
         if self <> nil then if created=true then initialized := true;
  end;

function TISO_array_obj.get_isocode (ccode: ccode_range) : integer;
  begin
      get_isocode := lookup_table[ccode].iso_code;
  end;

function TISO_array_obj.get_ccode_from_isocode (isocode: isocode_range) : integer;
  begin
    get_ccode_from_isocode := data[isocode].cow_code;
  end;

function TISO_array_obj.get_abbrev2_from_isocode (isocode: isocode_range) : string;
  begin
    get_abbrev2_from_isocode := data[isocode].iso_abbrev2;
  end;

function TISO_array_obj.get_abbrev2 (ccode: ccode_range) : string;
  var
    temp : integer;
  begin
    temp := lookup_table[ccode].iso_code;
    get_abbrev2 := data[temp].iso_abbrev2;
  end;

function TISO_array_obj.get_abbrev3_from_isocode (isocode: isocode_range) : string;
  begin
    get_abbrev3_from_isocode := data[isocode].iso_abbrev3;
  end;

function TISO_array_obj.get_abbrev3 (ccode: ccode_range) : string;
  var
    temp : integer;
  begin
    temp := lookup_table[ccode].iso_code;
    get_abbrev3 := data[temp].iso_abbrev3;
  end;

function TISO_array_obj.get_short_iso_name_from_isocode (isocode: isocode_range) : string;
  begin
    get_short_iso_name_from_isocode := data[isocode].iso_name_short;
  end;

function TISO_array_obj.get_short_iso_name (ccode: isocode_range) : string;
  var
    temp : integer;
  begin
     temp := lookup_table[ccode].iso_code;
     get_short_iso_name := data[temp].iso_name_short;
  end;

function TISO_array_obj.get_full_iso_name_from_isocode (isocode: isocode_range) : string;
  begin
    get_full_iso_name_from_isocode := data[isocode].iso_name_full;
  end;

function TISO_array_obj.get_full_iso_name (ccode: ccode_range) : string;
  var
    temp : integer;
  begin
    temp := lookup_table[ccode].iso_code;
    get_full_iso_name := data[temp].iso_name_full;
  end;

      {  -------------------------------------------------   }

end.                                             {unit eutypes1}

