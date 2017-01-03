unit EUTypesMID;

{EUGene  Copyright 1997, 1998-2007+  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

    {This unit has data types for dispute data;
     has read/manipulation functions and procedures for new MID data.
     Also has ICB data objects.
     Also has Werner Peace Years object.}

    {Procedures include:
        read COW disputes, the new COW MID data version.
        Make the COW disputes dyadic into the standard dyadic version.
        }

    {Note - should go and add checks for "file exists" to avoid hangs if dispute
     file names not initialized.  }

interface

uses sysutils, dialogs, math, eutypes1, eutypes2, cmnprocd, fileError;

{There are three structures/objects for MID data.
 1st is main dispute data.  2nd is country-specific dispute info.
 3rd is the (derived) dyadic data.  The dyadic data is embedded under a generic structure.}

type


    TWernerPeaceYears_obj = class(TObject)
        constructor init (infile_name : TFileName);
        destructor destroy;
        function get_werner_peaceyears (ccode1, ccode2 : ccode_range) : integer;
        function get_werner_peacedays (ccode1, ccode2 : ccode_range) : integer;
        function initialized : boolean;

     private
        data : array[ccode_range, ccode_range] of real;
        created : boolean;
     end;


    MIDdaytype = -9..31;
    hostlevtype = -9..5;
    side_type = 0..1;
    outcometype=-9..9;
    settlementtype=-9..4;
    fatalitytype=-9..6;
    ActionType=-9..23;
    revisiontype=-9..4;
    overall_dispute_rec = record    {record of dispute-specific info}
          idnum: dispute_id_range;
          StDay, EndDay: MIDdaytype;
          StMonth, EndMonth: month_range;
          StYear, EndYear: year_range;
          Outcome: outcometype;
          Settlement: settlementtype;
          Fatality: fatalitytype;
          MaxDur, MinDur: integer;
          HiAct: ActionType;
          HostLev: hostlevtype;
          Reciprocated : boolean;
          NumberA, NumberB: integer;
          Version: real;
          dispute_label: string;
       {new variables for version 3.0 format}
          Fatality_precise : integer;
          Link1, Link2, Link3 : string;  {eventually it will need to be an integer.}
          Ongoing_2001 : boolean;
    end;
    overall_dispute_rec_ptr = ^overall_dispute_rec;
    overall_disparray = array[num_disputes_range] of overall_dispute_rec_ptr;
    overall_disparrayptr = ^overall_disparray;
     {dispute_index_type has index given dispute # to location in dispute list}
    overall_dispute_index_type = array[dispute_id_range] of integer;
    overall_dispute_index_type_ptr = ^overall_dispute_index_type;

    {This object is for information about the overall dispute.}
    Toverall_dispute_data_obj = class(Tobject)
        constructor init (disp_file_name, dispute_labels_infile_name : TFileName;
                    dispute_data_version_for_read : dispute_data_version_for_read_type);
         {init reads ALL disputes in from raw file of dispute info.}
        destructor destroy; override;
        function initialized : boolean;
        function have_info_on_midnum (dispute_number : dispute_id_range) : boolean;
        {The remaining procedures all assume that a valid dispute_number is passed in.}
        function get_styear_dispute (dispute_number : dispute_id_range) : year_range;
          {get should be called with dispute #, internally it will figure out where this
           dispute is in the list.}
        function get_version (dispute_number : dispute_id_range) : real;
        function get_outcome (dispute_number : dispute_id_range) : outcometype;
        function get_settlement (dispute_number : dispute_id_range) : settlementtype;
        function get_fatality_dispute (dispute_number : dispute_id_range) : fatalitytype;
        function get_hiact_dispute (dispute_number : dispute_id_range) : Actiontype;
        function get_hostlev_dispute (dispute_number : dispute_id_range) : hostlevtype;
        function get_reciprocated (dispute_number : dispute_id_range) : boolean;
        function get_numberA (dispute_number : dispute_id_range) : integer;
        function get_numberB (dispute_number : dispute_id_range) : integer;
        function get_dispute_name (dispute_number : dispute_id_range) : string;
        function get_Fatality_precise (dispute_number : dispute_id_range) : integer;
        function get_Link1 (dispute_number : dispute_id_range) : string;
        function get_Link2 (dispute_number : dispute_id_range) : string;
        function get_Link3 (dispute_number : dispute_id_range) : string;
        function get_Ongoing_2001 (dispute_number : dispute_id_range) : boolean;

     private
        data : overall_disparrayptr;
        index : overall_dispute_index_type_ptr;
        num_disputes : num_disputes_range;
        created : boolean;
        procedure check_initialized;
     end;


     {Now object with record of info on each country's participation in disputes}
    country_dispute_rec = record
                  idnum : dispute_id_range;
                  ccode : ccode_range;
                  cabbrev : string[3];
                  sideA : boolean;           {true=yes, is sideA;  false=no}
                  revisionist : boolean;       {0=no, not revisionist;  1=is revisionist}
                  revtype : revisiontype;
                  StDay, EndDay: MIDdaytype;
                  StMonth, EndMonth: month_range;
                  StYear, EndYear : year_range;
                  Fatality: fatalitytype;
                  HiAct: ActionType;
                  HostLev: hostlevtype;
                  Originator: boolean;
                  Version: real;
                  {new variables for version 3.0 format}
                  revtype2 : revisiontype;
                  Fatality_precise : integer;
      end;
    country_dispute_rec_ptr = ^country_dispute_rec;
    country_disparray = array[country_dispute_range] of country_dispute_rec_ptr;
    country_dispute_data_ptr = ^country_disparray;
    TCountry_dispute_data_obj = class(Tobject)
        constructor init (infile_name : TFileName; dispute_data_version_for_read : dispute_data_version_for_read_type);
         {init reads in from raw file of dispute info.}
        destructor destroy; override;
        function initialized : boolean;
        function get_num_country_disputes : country_dispute_range;
        function get_last_dispnum : country_dispute_range;
        function get_ccode (country_dispute_number : country_dispute_range) : ccode_range;
        function get_version (country_dispute_number : country_dispute_range) : real;
        function get_disputeid (country_dispute_number : country_dispute_range) : dispute_id_range;
        function get_styear(dispnum : country_dispute_range) : year_range;
        function get_stmonth(dispnum : country_dispute_range) : month_range;
        function get_stday(dispnum : country_dispute_range) : MIDdaytype;
        function get_endyear(dispnum : country_dispute_range) : year_range;
        function get_endmonth(dispnum : country_dispute_range) : month_range;
        function get_endday(dispnum : country_dispute_range) : MIDdaytype;
        function get_sideA(dispnum : country_dispute_range) : boolean;
        function get_revisionist(dispnum : country_dispute_range) : boolean;
        function get_revtype(dispnum : country_dispute_range) : revisiontype;
        function get_fatality(dispnum : country_dispute_range) : fatalitytype;
        function get_HiAct(dispnum : country_dispute_range) : Actiontype;
        function get_hostlev(dispnum : country_dispute_range) : hostlevtype;
        function get_originator (dispnum : country_dispute_range) : boolean;
        function get_revtype2 (dispnum : country_dispute_range) : revisiontype;
        function get_Fatality_precise (dispnum : country_dispute_range) : integer;
        function date_overlap (disp1, disp2 : country_dispute_range) : boolean;
        function get_country_disp_num (midnum : dispute_id_range; accode : ccode_range) : country_dispute_range;
        function get_num_country_recs_for_state (midnum : dispute_id_range; accode : ccode_range) : integer;
        function get_country_disp_num_when_many (midnum : dispute_id_range; accode : ccode_range; mid_episode_to_find : integer) : integer;
           {most gets should be called with country dispute #, which is sequential}
     private
        data : country_dispute_data_ptr;
        index : overall_dispute_index_type_ptr;     {index will return # of first countrydispute rec,
                                         given input of the dispute #}
        num_country_disputes : country_dispute_range;
        created : boolean;
        procedure check_initialized;
     end;

     {For version 3.0 data at some point might want incident data.  Not doing it for now,
      it's not needed to dyadize the MIDs which is top priority.}

     {Now, for version 3.0, participant-incident data}
    pirecord=record
                  mid_idnum : dispute_id_range;
                  mid_incident_idnum : longint;
                  ccode : ccode_range;
                  cabbrev : string[3];
                  StDay, EndDay: MIDdaytype;
                  StMonth, EndMonth: month_range;
                  StYear, EndYear : year_range;
                  sideA : boolean;           {true=yes, is sideA;  false=no}
                  insideA : boolean;           {incident sideA}
                  Fatality: fatalitytype;
                  fatalityv : integer;
                  Action: ActionType;
                  HostLev: hostlevtype;
                  issue1, issue2 : integer;
                  Version: real;
                  {revisionist and originator can come from participant level.}
       end;   {record}
    pi_data_array = array of pirecord;

    Tparticipant_incident_data_obj = class(Tobject)
        constructor init (infile_name : TFileName);
        destructor destroy; override;
        function initialized : boolean;
        function get_mid_idnum (pi_id : integer) : dispute_id_range;
        function get_mid_incident_idnum (pi_id : integer) : longint;
        function get_ccode (pi_id : integer) : ccode_range;
        function get_cabbrev (pi_id : integer) : string;
        function get_StDay (pi_id : integer): MIDdaytype;
        function get_EndDay (pi_id : integer): MIDdaytype;
        function get_StMonth (pi_id : integer): month_range;
        function get_EndMonth (pi_id : integer): month_range;
        function get_StYear (pi_id : integer): year_range;
        function get_EndYear (pi_id : integer) : year_range;
        function get_sideA (pi_id : integer) : boolean;
        function get_incident_sideA (pi_id : integer) : boolean;
        function get_Fatality (pi_id : integer): fatalitytype;
        function get_fatalityv (pi_id : integer) : integer;
        function get_Action (pi_id : integer): ActionType;
        function get_HostLev (pi_id : integer): hostlevtype;
        function get_issue1 (pi_id : integer) : integer;
        function get_issue2 (pi_id : integer) : integer;
        function get_Version (pi_id : integer): real;

     private
        data : pi_data_array;
        index : overall_dispute_index_type_ptr;  {index will return # of first pi rec, given input of the dispute #}
        num_participant_incident_records : 0..max_participant_incident_records;
        created : boolean;
        procedure check_initialized;
     end;


     {Now, dyadic conflict object and related types}
    dyadic_call_parameter = (use_dispute, use_ccodeyear);
       {this will mark whether to use a ccode-year value, or a dispute value, in calls to some dyadic_dispute procedures.}
    dyad_dispute_main_rec = record
          record1, record2 : country_dispute_range;  {point to country-dispute records}
          others: boolean;       {3rd countries also involved}
      end;
    dyad_dispute_main_rec_ptr = ^dyad_dispute_main_rec;
    dyad_array = array of dyad_dispute_main_rec_ptr;
    dyad_array_ptr = ^dyad_array;
    index_array_type = array[ccode_range, ccode_range] of longint;
    index_array_ptr = ^index_array_type;
    stored_peaceyrs_type = record
          ccode1, ccode2 : ccode_range;
          year : year_range;
          numyears : integer;
       end;

    TGeneric_Dyadic_Conflict_data_obj = class(Tobject)
        stored_peaceyrs : stored_peaceyrs_type;
        constructor init (user_selections : user_selection_type; configuration :
                          configuration_type; year1, year2 : year_range); virtual; abstract;
        destructor destroy; virtual; abstract;
        function initialized : boolean;
        function get_first_partition_year : year_range;
        function get_last_partition_year : year_range;

        {Note:  the procedures within the generic conflict type are needed only for the want_in_year
         routine.  Additional procedures specific to the conflict data sets are within them.}
        function is_ongoing (ccode1, ccode2 : ccode_range; year : year_range;
                 var ongoing_num : longint) : boolean; virtual; abstract;

        function Is_AnyYear_Joined_Initiation (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; virtual; abstract;
        function Is_AnyYear_Joined_targets (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; virtual; abstract;
        function Is_AnyYear_Joined_Revision (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; virtual; abstract;
        function Is_AnyYear_Joined_SQ (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; virtual; abstract;

        function Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2 : ccode_range; year : year_range;
                 var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; virtual; abstract;

        function wanted_new_initiation (const ccode1, ccode2 : ccode_range;
                 const year : year_range; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : boolean; virtual; abstract;
        function wanted_new_or_continuing_initiation (const ccode1, ccode2 : ccode_range;
                 const year : year_range; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : boolean; virtual; abstract;
        function wanted_new_NonDir_Dispute (const ccode1, ccode2 : ccode_range;
                 const year : year_range; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : boolean; virtual; abstract;
        function wanted_new_or_Continuing_NonDir_Dispute (const ccode1, ccode2 : ccode_range;
                 const year : year_range; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : boolean; virtual; abstract;
      private
        first_partition_year, last_partition_year : year_range;
        created : boolean;
        procedure check_initialized;

    end;   {class}


    cow_maoz_1992_type = (none_pre_1992, cow_pre_1992, maoz_pre_1992);

    TCOWDyadic_dispute_data_obj = class(TGeneric_Dyadic_Conflict_data_obj)
        constructor init (user_selections : user_selection_type; configuration :
                          configuration_type; year1, year2 : year_range); overload; virtual; abstract;
        constructor init (user_selections : user_selection_type; configuration :
                          configuration_type; year1, year2 : year_range; use_cow_or_maoz_pre_1992 : cow_maoz_1992_type); overload; virtual; abstract;
        destructor destroy; virtual; abstract;
        function get_last_dispnum : longint;

       {Note:  these functions are called with dyadic dispute #, rather than ccode year.
        They just return info on particular disputes, once a dispute is identified
        by some other function (usually a call with a ccode-year).  }
        function get_ccode (dyadic_dispute_num : longint;
                 which_state : side_type) : ccode_range;  virtual; abstract;
        function get_first_year (dyadic_dispute_num : longint) : year_range;  virtual; abstract;
        function get_last_year (dyadic_dispute_num : longint) : year_range;  virtual; abstract;

        function get_MID_num (dyadic_dispute_num : longint) : dispute_id_range;  virtual; abstract;
        function get_dyadic_MID_num (dyadic_dispute_num : longint) : dyadic_dispute_id_range;  virtual; abstract;
        function get_hostlev_state (dyadic_dispute_num : longint; ccode: ccode_range) : hostlevtype;  virtual; abstract;
        function get_sideA (dyadic_dispute_num : longint; ccode : ccode_range) : boolean;  virtual; abstract;
        function get_revisionist (dyadic_dispute_num : longint; ccode: ccode_range) : boolean;  virtual; abstract;
        function get_originator (dyadic_dispute_num : longint; ccode: ccode_range) : boolean;  virtual; abstract;
        function get_styear (dyadic_dispute_num : longint; ccode: ccode_range) : year_range;  virtual; abstract;
        function get_stmonth (dyadic_dispute_num : longint; ccode: ccode_range) : month_range;   virtual; abstract;
        function get_stday (dyadic_dispute_num : longint; ccode: ccode_range) : MIDdaytype;  virtual; abstract;
        function get_endyear (dyadic_dispute_num : longint; ccode: ccode_range) : year_range;  virtual; abstract;
        function get_endmonth (dyadic_dispute_num : longint; ccode: ccode_range) : month_range;  virtual; abstract;
        function get_endday (dyadic_dispute_num : longint; ccode: ccode_range) : MIDdaytype;  virtual; abstract;
        function get_revtype (dyadic_dispute_num : longint; ccode: ccode_range) : revisiontype;  virtual; abstract;
        function get_fatality_state (dyadic_dispute_num : longint; ccode: ccode_range) : fatalitytype;  virtual; abstract;
        function get_HiAct_state (dyadic_dispute_num : longint; ccode: ccode_range) : Actiontype;  virtual; abstract;
        {The next set applies to MID as a whole.  But some are still adjusted for dyadic-ness.}
        function get_MID_name (dyadnum : longint) : string; virtual; abstract;
        function get_MID_numstates (dyadnum : longint; ccode: ccode_range) : integer; virtual; abstract;
        function get_MID_outcome (dyadnum : longint) : outcometype; virtual; abstract;
        function get_MID_settlement (dyadnum : longint) : settlementtype; virtual; abstract;
        function get_MID_fatality (dyadnum : longint) : fatalitytype; virtual; abstract;
        function get_MID_hiact (dyadnum : longint) : actiontype; virtual; abstract;
        function get_MID_hostlev (dyadnum : longint) : hostlevtype; virtual; abstract;
        function get_MID_reciprocated (dyadnum : longint) : boolean; virtual; abstract;

        {NOTE:  These calls are made with ccode1 ccode2 year.}
        function is_ongoing (ccode1, ccode2 : ccode_range; year : year_range;
                 var ongoing_num : longint) : boolean; override;
        function Is_1stYearOfDispute (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_disp_num : longint) : boolean;
        function Is_sideA (ccode1, ccode2 : ccode_range; year : year_range) : boolean;
        function Is_originator (ccode1, ccode2 : ccode_range; year : year_range) : boolean;

        function Is_1stYear_Initiation (ccode1, ccode2 : ccode_range; year : year_range;
                 var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_AnyYear_Initiation (ccode1, ccode2 : ccode_range; year : year_range;
                 var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_1stYear_Joined_Initiation (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_AnyYear_Joined_Initiation (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; override;
        function Is_1styear_joined_targets (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_AnyYear_Joined_targets (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; override;

        function Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2 : ccode_range; year : year_range;
                 var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2 : ccode_range; year : year_range;
                 var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_AnyYear_NonDir_Dispute_Originators (ccode1, ccode2 : ccode_range; year : year_range;
                 var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2 : ccode_range; year : year_range;
                 var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; override;

        function Is_1stYear_Revision (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_AnyYear_Revision (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_1stYear_Joined_Revision (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_AnyYear_Joined_Revision (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; override;
        function Is_1stYear_Joined_SQ (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_AnyYear_Joined_SQ (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; override;

        function wanted_new_initiation (const ccode1, ccode2 : ccode_range;
                 const year : year_range; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : boolean; override;
        function wanted_new_or_continuing_initiation (const ccode1, ccode2 : ccode_range;
                 const year : year_range; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : boolean; override;

        function wanted_new_NonDir_Dispute (const ccode1, ccode2 : ccode_range;
                 const year : year_range; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : boolean; override;
        function wanted_new_or_Continuing_NonDir_Dispute (const ccode1, ccode2 : ccode_range;
                 const year : year_range; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : boolean; override;

        function relevant_midnum (const ccode1, ccode2 : ccode_range; const year : year_range;
                 const user_selections : user_selection_type) : longint;
        function get_last_dispute_in_year (const ccode1, ccode2 : ccode_range; const year : year_range;
                 const which_state : side_type; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : TDateTime;
        function relevant_hostlev (const ccode1, ccode2 : ccode_range; const year : year_range;
                 const which_state : side_type; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : hostlevtype;
        function get_peace_days (ccode1, ccode2 : ccode_range; year : year_range;
                 user_selections : user_selection_type; werner_peace_years_data : TWernerPeaceYears_obj) : integer;
        function get_peace_years (ccode1, ccode2 : ccode_range; year : year_range;
                 user_selections : user_selection_type; werner_peace_years_data : TWernerPeaceYears_obj) : integer;
        function get_num_new_mids (ccode1, ccode2 : ccode_range; year : year_range) : integer;
        function get_num_total_mids (ccode1, ccode2 : ccode_range; year : year_range) : integer;

     private
        country_disputes : TCountry_dispute_data_obj;
        overall_disputes : Toverall_dispute_data_obj;
        index : index_array_ptr;
        num_dyadic_disputes : longint;   {Arrays now indexed 0..(num_disputes-1) }
     end;

    TCOWDyadic_dispute_data_obj_format21 = class(TCOWDyadic_dispute_data_obj)
        constructor init (user_selections : user_selection_type; configuration :
                          configuration_type; year1, year2 : year_range); override;
        destructor destroy; override;

       {Note:  these functions are called with dyadic dispute #, rather than ccode year.
        They just return info on particular disputes, once a dispute is identified
        by some other function (usually a call with a ccode-year).  }
        function get_ccode (dyadic_dispute_num : longint; which_state : side_type) : ccode_range;  override;
        function get_first_year (dyadic_dispute_num : longint) : year_range; override;
        function get_last_year (dyadic_dispute_num : longint) : year_range; override;
        function get_MID_num (dyadic_dispute_num : longint) : dispute_id_range; override;
        function get_hostlev_state (dyadic_dispute_num : longint; ccode: ccode_range) : hostlevtype; override;
        function get_sideA (dyadic_dispute_num : longint; ccode : ccode_range) : boolean; override;
        function get_revisionist (dyadic_dispute_num : longint; ccode: ccode_range) : boolean; override;
        function get_originator (dyadic_dispute_num : longint; ccode: ccode_range) : boolean; override;
        function get_styear (dyadic_dispute_num : longint; ccode: ccode_range) : year_range; override;
        function get_stmonth (dyadic_dispute_num : longint; ccode: ccode_range) : month_range; override;
        function get_stday (dyadic_dispute_num : longint; ccode: ccode_range) : MIDdaytype; override;
        function get_endyear (dyadic_dispute_num : longint; ccode: ccode_range) : year_range; override;
        function get_endmonth (dyadic_dispute_num : longint; ccode: ccode_range) : month_range; override;
        function get_endday (dyadic_dispute_num : longint; ccode: ccode_range) : MIDdaytype; override;
        function get_revtype (dyadic_dispute_num : longint; ccode: ccode_range) : revisiontype; override;
        function get_fatality_state (dyadic_dispute_num : longint; ccode: ccode_range) : fatalitytype; override;
        function get_HiAct_state (dyadic_dispute_num : longint; ccode: ccode_range) : Actiontype; override;
        function get_MID_name (dyadnum : longint) : string; override;
        function get_MID_numstates (dyadnum : longint; ccode: ccode_range) : integer; override; {not adjusted for dyadic here - just from overall}
        function get_MID_outcome (dyadnum : longint) : outcometype; override; {not adjusted for dyadic here - just from overall}
        function get_MID_settlement (dyadnum : longint) : settlementtype; override; {not adjusted for dyadic here - just from overall}
        function get_MID_fatality (dyadnum : longint) : fatalitytype; override; {not adjusted for dyadic here - just from overall}
        function get_MID_hiact (dyadnum : longint) : actiontype; override;  {not adjusted for dyadic here - just from overall}
        function get_MID_hostlev (dyadnum : longint) : hostlevtype; override;  {not adjusted for dyadic here - just from overall}
        function get_MID_reciprocated (dyadnum : longint) : boolean; override;  {not adjusted for dyadic here - just from overall}
     private
        dyadic_dispute_data : dyad_array_ptr;
     end;

    roletype = 0..4;     {0 indicates blank/missing}
    set_04_type = set of 0..4;


     {Now, Maoz dyadic dispute object and related types}
    maoz_dyadic_dispute_main_rec = record
            idnum : dispute_id_range;  {maoz disno}
            ccodeA, ccodeB : ccode_range;       {maoz stateA, stateB}
            cabbrevA, cabbrevB : string[3];       {maoz namea, nameB}
            StDay, EndDay: MIDdaytype;
            StMonth, EndMonth: month_range;
            StYear, EndYear : year_range;
            year : year_range;            {actual annual record marker}
            Outcome: outcometype;
            Settlement: settlementtype;
            Fatality: fatalitytype;
            HiAct: ActionType;
            HostLev: hostlevtype;
            ReciprocatedMID : boolean;
            NumberA, NumberB: integer;
            StDayA, EndDayA : MIDdaytype;
            StMonthA, EndMonthA : month_range;
            StYearA, EndYearA : year_range;
            SIDEAA : boolean;
            revisionistA : boolean;       {0=no, not revisionist;  1=is revisionist}
            revtypeA : revisiontype;
            FatalityA : fatalitytype;
            HiActA : ActionType;
            HostLevA : hostlevtype;
            OriginatorA : boolean;
            StDayB, EndDayB : MIDdaytype;
            StMonthB, EndMonthB : month_range;
            StYearB, EndYearB : year_range;
            SIDEAB : boolean;
            revisionistB : boolean;       {0=no, not revisionist;  1=is revisionist}
            revtypeB : revisiontype;
            FatalityB : fatalitytype;
            HiActB : ActionType;
            HostLevB : hostlevtype;
            OriginatorB : boolean;
            RoleA, RoleB : roletype;
            COWWar : boolean;
            Durindx : integer;
            DurDays : integer;
            {ReciprocatedDyadic : boolean;       Not available v1.1}
            {NewMID : boolean;                   Not available in v1.1}
       end;   {dyadic record}
    Maoz_dyadic_dispute_main_rec_ptr = ^maoz_dyadic_dispute_main_rec;
    Maoz_dyad_array = array of Maoz_dyadic_dispute_main_rec_ptr;
    Maoz_dyad_array_ptr = ^Maoz_dyad_array;
    maoz_index_array_type = array[ccode_range, ccode_range] of longint;
    maoz_index_array_ptr = ^maoz_index_array_type;
    TMaoz_Dyadic_dispute_data_obj = class(TGeneric_Dyadic_Conflict_data_obj)
        stored_peaceyrs : stored_peaceyrs_type;
        constructor init (user_selections : user_selection_type; configuration :
                          configuration_type; year1, year2 : year_range); override;
        destructor destroy; override;
        function get_last_dispnum : longint;

       {Note:  these functions are called with dyadic dispute #, rather than ccode year.
        They just return info on particular disputes, once a dispute is identified
        by some other function (usually a call with a ccode-year).  }
        {Note that dyadic_dispute_num here is really just a record # in the datastructure.}
        function get_ccode (dyadic_dispute_num : longint; which_state : side_type) : ccode_range;
        function get_year (dyadic_dispute_num : longint) : year_range;  {year of record}
        function get_MID_num (dyadic_dispute_num : longint) : dispute_id_range;

        function get_hostlev_state (dyadic_dispute_num : longint; ccode: ccode_range) : hostlevtype;
        function get_sideA (dyadic_dispute_num : longint; ccode : ccode_range) : boolean;
        function get_revisionist (dyadic_dispute_num : longint; ccode: ccode_range) : boolean;
        function get_originator (dyadic_dispute_num : longint; ccode: ccode_range) : boolean;
        function get_styear (dyadic_dispute_num : longint; ccode: ccode_range) : year_range;
        function get_stmonth (dyadic_dispute_num : longint; ccode: ccode_range) : month_range;
        function get_stday (dyadic_dispute_num : longint; ccode: ccode_range) : MIDdaytype;
        function get_endyear (dyadic_dispute_num : longint; ccode: ccode_range) : year_range;
        function get_endmonth (dyadic_dispute_num : longint; ccode: ccode_range) : month_range;
        function get_endday (dyadic_dispute_num : longint; ccode: ccode_range) : MIDdaytype;
        function get_revtype (dyadic_dispute_num : longint; ccode: ccode_range) : revisiontype;
        function get_fatality_state (dyadic_dispute_num : longint; ccode: ccode_range) : fatalitytype;
        function get_HiAct_state (dyadic_dispute_num : longint; ccode: ccode_range) : Actiontype;
        function get_Role (dyadic_dispute_num : longint; ccode: ccode_range) : Roletype;

        function get_MID_styear (dyadic_dispute_num : longint) : year_range;
        function get_MID_stmonth (dyadic_dispute_num : longint) : month_range;
        function get_MID_stday (dyadic_dispute_num : longint) : MIDdaytype;
        function get_MID_endyear (dyadic_dispute_num : longint) : year_range;
        function get_MID_endmonth (dyadic_dispute_num : longint) : month_range;
        function get_MID_endday (dyadic_dispute_num : longint) : MIDdaytype;

        function get_MID_numstates (dyadic_dispute_num : longint; ccode: ccode_range) : integer;
        function get_MID_outcome (dyadic_dispute_num : longint) : outcometype;
        function get_MID_settlement (dyadic_dispute_num : longint) : settlementtype;
        function get_MID_fatality (dyadic_dispute_num : longint) : fatalitytype;
        function get_MID_hiact (dyadic_dispute_num : longint) : actiontype;
        function get_MID_hostlev (dyadic_dispute_num : longint) : hostlevtype;
        function get_MID_reciprocated (dyadic_dispute_num : longint) : boolean;
        
        function get_COWWar (dyadic_dispute_num : longint) : boolean;
        function get_DurIndx (dyadic_dispute_num : longint) : integer;
        function get_DurDays (dyadic_dispute_num : longint) : integer;
        {function get_ReciprocatedDyadic (dyadic_dispute_num : longint) : boolean; }
        function get_NewMID (dyadic_dispute_num : longint) : boolean;

        {NOTE:  These calls are made with ccode1 ccode2 year.}
        function is_ongoing (ccode1, ccode2 : ccode_range; year : year_range;
                 var ongoing_num : longint) : boolean; override;
        function Is_1stYearOfDispute (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_disp_num : longint) : boolean;
        function Is_sideA (ccode1, ccode2 : ccode_range; year : year_range) : boolean;
        function Is_originator (ccode1, ccode2 : ccode_range; year : year_range) : boolean;

        function Is_1stYear_Initiation (ccode1, ccode2 : ccode_range; year : year_range;
                 var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_AnyYear_Initiation (ccode1, ccode2 : ccode_range; year : year_range;
                 var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_1stYear_Joined_Initiation (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_AnyYear_Joined_Initiation (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; override;
        function Is_1styear_joined_targets (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_AnyYear_Joined_targets (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; override;

        function Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2 : ccode_range; year : year_range;
                 var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2 : ccode_range; year : year_range;
                 var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_AnyYear_NonDir_Dispute_Originators (ccode1, ccode2 : ccode_range; year : year_range;
                 var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2 : ccode_range; year : year_range;
                 var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; override;

        function Is_1stYear_Revision (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_AnyYear_Revision (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_1stYear_Joined_Revision (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_AnyYear_Joined_Revision (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; override;
        function Is_1stYear_Joined_SQ (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_AnyYear_Joined_SQ (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; override;

        function wanted_new_initiation (const ccode1, ccode2 : ccode_range;
                 const year : year_range; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : boolean; override;
        function wanted_new_or_continuing_initiation (const ccode1, ccode2 : ccode_range;
                 const year : year_range; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : boolean; override;

        function wanted_new_NonDir_Dispute (const ccode1, ccode2 : ccode_range;
                 const year : year_range; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : boolean; override;
        function wanted_new_or_Continuing_NonDir_Dispute (const ccode1, ccode2 : ccode_range;
                 const year : year_range; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : boolean; override;

        function relevant_midnum (const ccode1, ccode2 : ccode_range; const year : year_range;
                 const user_selections : user_selection_type) : longint;
        function relevant_hostlev (const ccode1, ccode2 : ccode_range; const year : year_range;
                 const which_state : side_type; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : hostlevtype;
        function get_last_dispute_in_year ( const ccode1, ccode2 : ccode_range; const year : year_range;
                 const which_state : side_type; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : TDateTime;
        function get_peace_days (ccode1, ccode2 : ccode_range; year : year_range;
                 user_selections : user_selection_type; werner_peace_years_data : TWernerPeaceYears_obj) : integer;
        function get_peace_years (ccode1, ccode2 : ccode_range; year : year_range;
                 user_selections : user_selection_type; werner_peace_years_data : TWernerPeaceYears_obj) : integer;
        function get_num_new_mids (ccode1, ccode2 : ccode_range; year : year_range) : integer;
        function get_num_total_mids (ccode1, ccode2 : ccode_range; year : year_range) : integer;

     private
        data : Maoz_dyad_array_ptr;
        index : maoz_index_array_ptr;       {defined above}
        num_dyadic_dispute_years : longint;
     end;


    format_30_dyad_record = record
       {Note that if the format changes, be sure to update the .init procedure,
        and also the "add_cow_dyadic_pre_1992" and "add_maoz_dyadic_pre_1992" procs.
        Also the initialization to blank record within the .init procedure.}
            idnum : dispute_id_range;  {midnumber}
            dyadic_idnum : dyadic_dispute_id_range;  {new dyadic midnumber}
            incident_idnum : integer;  {won't be output, for internal use, shows the incident a dyadic MID was generated from.  Eventually will be collapsed and meaningless.}
            ccodeA, ccodeB : ccode_range;
            nondirected_dyad_number : integer;   {concat of low ccode with high ccode}
            StDay, EndDay: MIDdaytype;        {dates these 2 were involved against each other}
            StMonth, EndMonth: month_range;   {from first day of first incident, to last day of last incident}
            StYear, EndYear : year_range;
            {inexact_start_date, inexact_end_date : boolean;
            internal_start_date, internal_end_date : TDateTime;   }
            {year : year_range;            {actual annual record marker - don't do for now}

            {For the next set, we can't compute dyadic, so reported values post 1992 are
             just from overall or country-MID level data.  But Maoz has his own values.}
            Outcome: outcometype;
            Settlement: settlementtype;
            Fatality: fatalitytype;
            FatalityA, FatalityB : fatalitytype;
            FatalityVA, FatalityVB : integer;

            HiAct: ActionType;      {compute these across incidents}
            HiActA : ActionType;
            HiActB : ActionType;
            HostLev: hostlevtype;
            HostLevA : hostlevtype;
            HostLevB : hostlevtype;

            ReciprocatedMID : boolean;         {dyadic reciprocation.  compute from incidents.}
            NumberA, NumberB: integer;    {number in overall mid.  Not adjusted for v3.0 data, but needed
                                           original values from Maoz.}
            OriginatorA : boolean;        {originator in overall MID.  take from mid-participant data.}
            OriginatorB : boolean;
            revisionistA : boolean;       {dyadic revisionism.  Compute from incidents.}
            RevType_A1, RevType_A2 : revisiontype;
            revisionistB : boolean;
            RevType_B1, RevType_B2 : revisiontype;
            {The issue sets are for internal use}
            issue_setA, issue_setB : set_04_type;

            SideA, SideB : boolean;  {Side A in overall dispute}
            SIDEAA_dyadic : boolean;  {Side A in the *dyadic* dispute.  First to take directed action in dyad.}
            SIDEAB_dyadic : boolean;  {used to compute "role" variables.}

            RoleA, RoleB : roletype;    {can compute at the end?}
            version : real;

            {Also 3 variables from the maoz data set, which get moved in when the
             add_maoz_data proc is used.}
            COWWar : boolean;
            Durindx : integer;
            DurDays : integer;

       end;
    format_30_dyad_record_ptr = ^format_30_dyad_record;
    format_30_dyad_array = array of format_30_dyad_record_ptr;
    format_30_dyad_array_ptr = ^format_30_dyad_array;

    {This structure will use PI data for 1992+, and either construct COW disps or use
     Maoz disps for earlier part, depending on user setting.}
    TDyadic_dispute_data_obj_integrated_format30 = class(TCOWDyadic_dispute_data_obj)
        constructor init (user_selections : user_selection_type; configuration :
                          configuration_type; year1, year2 : year_range;
                          use_cow_or_maoz_pre_1992 : cow_maoz_1992_type); override;
        destructor destroy; override;
        procedure output_to_file (outfilename : TFilename);   {dump the entire data set to a file}

       {Note:  these functions are called with dyadic dispute #, rather than ccode year.
        They just return info on particular disputes, once a dispute is identified
        by some other function (usually a call with a ccode-year).  }
       {Note that these must all be programmed, even if not relevant in new dyadic data,
        because they are in the generic procedure.  We may have some new variables to get as well
        (Issues, e.g.)
        ** problem - we have vars that parallel Maoz more than they parallel the old COW.
        What do we do?  Here, probably just need calls that return missing_value if we can't get
        it in old COW (maybe need parallel functions in old cow data?).  In euinoutd procs,
        we'll just need to have same calls, but in some of the data, it will be missing.}

        function get_ccode (dyadic_dispute_num : longint; which_state : side_type) : ccode_range;  override;
        function get_first_year (dyadic_dispute_num : longint) : year_range; override;
        function get_last_year (dyadic_dispute_num : longint) : year_range; override;

        function get_MID_num (dyadic_dispute_num : longint) : dispute_id_range; override;
        function get_dyadic_MID_num (dyadic_dispute_num : longint) : dyadic_dispute_id_range; override;

        function get_hostlev_state (dyadic_dispute_num : longint; ccode: ccode_range) : hostlevtype; override;
        function get_HiAct_state (dyadic_dispute_num : longint; ccode: ccode_range) : Actiontype; override;
        function get_reciprocated_dyadic (dyadic_dispute_num : longint) : boolean;
        function get_sideA (dyadic_dispute_num : longint; ccode : ccode_range) : boolean; override;
        function get_revisionist (dyadic_dispute_num : longint; ccode: ccode_range) : boolean; override;
        function get_originator (dyadic_dispute_num : longint; ccode: ccode_range) : boolean; override;
        function get_styear (dyadic_dispute_num : longint; ccode: ccode_range) : year_range; override;
        function get_stmonth (dyadic_dispute_num : longint; ccode: ccode_range) : month_range; override;
        function get_stday (dyadic_dispute_num : longint; ccode: ccode_range) : MIDdaytype; override;
        function get_endyear (dyadic_dispute_num : longint; ccode: ccode_range) : year_range; override;
        function get_endmonth (dyadic_dispute_num : longint; ccode: ccode_range) : month_range; override;
        function get_endday (dyadic_dispute_num : longint; ccode: ccode_range) : MIDdaytype; override;
        function get_revtype (dyadic_dispute_num : longint; ccode: ccode_range) : revisiontype; override;
        function get_revtype2 (dyadic_dispute_num : longint; ccode: ccode_range) : revisiontype;
        function get_fatality_state (dyadic_dispute_num : longint; ccode: ccode_range) : fatalitytype; override;
        function get_version (dyadic_dispute_num : longint) : real;

        function get_MID_name (dyadnum : longint) : string; override; {not specifically dyadic}
        function get_MID_numstates (dyadnum : longint; ccode: ccode_range) : integer; override; {maoz only adjusted ; supposedly reflects dyadic in maoz, but will not in mid 3.0  How can it?}
        function get_MID_outcome (dyadnum : longint) : outcometype; override; {maoz only reflects dyadic}
        function get_MID_settlement (dyadnum : longint) : settlementtype; override; {maoz only reflects dyadic}
        function get_MID_fatality (dyadnum : longint) : fatalitytype; override; {maoz only reflects dyadic}
        function get_MID_hiact (dyadnum : longint) : actiontype; override; {adjusted to reflect dyadic, in cow and maoz}
        function get_MID_hostlev (dyadnum : longint) : hostlevtype; override; {adjusted to reflect dyadic, in cow and maoz}
        function get_MID_reciprocated (dyadnum : longint) : boolean; override; {adjusted to reflect dyadic}

        {New routines for dyadic 3.0 data that we couldn't have before.}
        function get_fatality_state_precise (dyadic_dispute_num : longint; ccode: ccode_range) : integer;
        function get_sideA_dyadic (dyadic_dispute_num : longint; ccode: ccode_range) : boolean;
        function get_Role (dyadic_dispute_num : longint; ccode: ccode_range) : Roletype;
        function has_midnum_from_pi_data (mid_num : dispute_id_range) : boolean;
        function get_link1 (dyadic_dispute_num : longint) : string;
        function get_link2 (dyadic_dispute_num : longint) : string;
        function get_link3 (dyadic_dispute_num : longint) : string;
        function get_ongoing2001 (dyadic_dispute_num : longint) : boolean;

        {A few maoz only procedures, since maoz data now read in/integrated with COW in this structure.}
        function get_COWWar (dyadic_dispute_num : longint) : boolean;
        function get_DurIndx (dyadic_dispute_num : longint) : integer;
        function get_DurDays (dyadic_dispute_num : longint) : integer;

        {This should be called only on the maoz data.}
        function get_corresponding_maoz_dispnum (cow_num : longint; var COW_dyadic_dispute_data_30 : TDyadic_dispute_data_obj_integrated_format30) : longint;
        {This should be called only on the cow data.}
        function get_corresponding_cow_dispnum (maoz_num : longint; var Maoz_dispute_data : TDyadic_dispute_data_obj_integrated_format30) : longint;

        function convert_hiact_v21_to_v30 (hiact_21 : actiontype) : actiontype;

     private
        participant_incident_data : Tparticipant_incident_data_obj;
        dyadic_dispute_data : format_30_dyad_array_ptr;
        pi_midnum_list : array[dispute_id_range] of boolean;   {True or False if a midnum in this structure.}
        added_cow_or_maoz : (added_cow, added_maoz, added_none);
        procedure add_cow_dyadic_pre_1992 (user_selections : user_selection_type; configuration :
                          configuration_type; Country_Disputes : TCountry_dispute_data_obj; year1, year2 : year_range;
                          blank_dyad_rec : format_30_dyad_record);
        procedure add_maoz_dyadic_pre_1992 (user_selections : user_selection_type; configuration :
                          configuration_type; year1, year2 : year_range; blank_dyad_rec : format_30_dyad_record);
     end;


     {Now, icb dyadic dispute object and related types}
    icb_dyadic_dispute_main_rec = record
            crisno : longint;   {number of ICB crisis case comes from}
            crdynum : longint;  {unique dyadic crisis #, same across multiple years of same crisis}
            ccodeA, ccodeB : ccode_range;       {hewitt stateA, stateB}
            cabbrevA, cabbrevB : string[3];       {hewitt namea, nameB}
            year : year_range;            {actual annual record marker}
            crisname : string;
            actnuma, actnumb : longint;  {ICB actor-level sequence number}
            oneside : integer; { 0..2; 0 - two sided dyad (both dyad members are considered crisis actors)
                                 1 - one-sided dyad, state A is a non-crisis actor
                                 2 - one-sided dyad, state B is a non-crisis actor }
            cowmema, cowmemb : integer;   {0..4;
                  {0 -	state is a member of interstate system for entire duration of crisis dyad period (if a state terminates on the last day of the crisis period, it is considered an independent state for the entire duration of the crisis dyad period)
                  1 - state started during crisis dyad period
                  2 - state terminated during crisis dyad period (before last day of crisis)
                  3 - state started after termination of crisis dyad period
                  4 - state terminated before initiation of crisis dyad period }
            gwmema, gwmemb : integer;   {0..4;
                  {0 -	state is a member of interstate system for entire duration of crisis dyad period (if a state terminates on the last day of the crisis period, it is considered an independent state for the entire duration of the crisis dyad period)
                  1 - state started during crisis dyad period
                  2 - state terminated during crisis dyad period (before last day of crisis)
                  3 - state started after termination of crisis dyad period
                  4 - state terminated before initiation of crisis dyad period
                  5 - state is a microstate  }
            iwca, iwcb : integer;    {0..10;   {intra-war crisis}
            yrtriga, yrterma, yrtrigb, yrtermb, trgyrdy, trmyrdy : integer;    {-99..max_year;  }
            motriga, moterma, motrigb, motermb, trgmody, trmmody : integer;    {-99..12;  }
            datriga, daterma, datrigb, datermb, trgdady, trmdady : integer;    {-99..99;  }
            durdays : integer; {Dyad duration, in days}
            duryear : integer; {Number of calendar years spanned by dyad}
            ongoing : integer;  {0=onset, 1=ongoing}
            originatora, originatorb : boolean;   {I compute this as an auxiliary function, need it later.
               Records whether a (or b) are originators in the crisis, i.e. involved dyadic crisis day 1.}
            crisis_start : TDateTime;   {for each dyad, keeps the date of the overall crisis beginning.}
       end;   {dyadic record}

    ICB_dyadic_dispute_main_rec_ptr = ^ICB_dyadic_dispute_main_rec;
    ICB_dyad_array = array of ICB_dyadic_dispute_main_rec_ptr;
    ICB_dyad_array_ptr = ^ICB_dyad_array;
    ICB_index_array_type = array[ccode_range, ccode_range] of longint;
    ICB_index_array_ptr = ^ICB_index_array_type;

    TICBDyadic_dispute_data_obj = class(TGeneric_Dyadic_Conflict_data_obj)
        stored_peaceyrs : stored_peaceyrs_type;
        constructor init (user_selections : user_selection_type; configuration :
                          configuration_type; year1, year2 : year_range); override;
        destructor destroy; override;
        function get_last_dispnum : longint;

       {Note:  these functions are called with dyadic dispute #, rather than ccode year.
        They just return info on particular crises, once a crisis record is identified
        by some other function (usually a call with a ccode-year).  }
        function get_ccode (record_num : longint; which_state : side_type) : ccode_range;
        function get_year (record_num : longint) : year_range;
        {get first and last year get the first and last year of the crisis associated with this
         record.  This relies on ICB having the same dyadic dates on all years of a crisis.}
        function get_first_year (record_num : longint) : year_range;
        function get_last_year (record_num : longint) : year_range;

        function get_crisis_num (record_num : longint) : integer;
        function get_crisname (record_num : longint) : string;
        function get_crdynum (record_num : longint) : integer;
        function get_oneside (record_num : longint) : integer;
        function get_durdays (record_num : longint) : integer;
        function get_duryear (record_num : longint) : integer;
        function get_ongoing (record_num : longint) : integer;

        function get_actnuma (record_num : longint) : integer;
        function get_actnumb (record_num : longint) : integer;
        function get_cowmema (record_num : longint) : integer;
        function get_cowmemb (record_num : longint) : integer;
        function get_gwmema (record_num : longint) : integer;
        function get_gwmemb (record_num : longint) : integer;
        function get_iwca (record_num : longint) : integer;
        function get_iwcb (record_num : longint) : integer;
        function get_yrtriga (record_num : longint) : integer;
        function get_yrterma (record_num : longint) : integer;
        function get_yrtrigb (record_num : longint) : integer;
        function get_yrtermb (record_num : longint) : integer;
        function get_trgyrdy (record_num : longint) : integer;
        function get_trmyrdy (record_num : longint) : integer;
        function get_motriga (record_num : longint) : integer;
        function get_moterma (record_num : longint) : integer;
        function get_motrigb (record_num : longint) : integer;
        function get_motermb (record_num : longint) : integer;
        function get_trgmody (record_num : longint) : integer;
        function get_trmmody (record_num : longint) : integer;
        function get_datriga (record_num : longint) : integer;
        function get_daterma (record_num : longint) : integer;
        function get_datrigb (record_num : longint) : integer;
        function get_datermb (record_num : longint) : integer;
        function get_trgdady (record_num : longint) : integer;
        function get_trmdady (record_num : longint) : integer;
        function get_originatora (record_num : longint) : boolean;
        function get_originatorb (record_num : longint) : boolean;

        {NOTE:  These calls are made with ccode1 ccode2 year.}
        function is_ongoing (ccode1, ccode2 : ccode_range; year : year_range;
                 var ongoing_num : longint) : boolean; override;
        function Is_1stYearOfCrisis (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_disp_num : longint; const use_param : dyadic_call_parameter) : boolean;
                 {true if first year of a crisis in this dyad-year; returns record #;
                  or if called with use_dispute, checks if this is the first year of this dyadic crisis.}

        {Returns true if accode is an originator in the crisis, in this dyadic rec.}
        function Is_originator_in_crisis (accode : ccode_range; record_num : longint) : boolean;

        function Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2 : ccode_range; year : year_range;
                 var record_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2 : ccode_range; year : year_range;
                 var record_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_AnyYear_NonDir_Dispute_Originators (ccode1, ccode2 : ccode_range; year : year_range;
                 var record_num : longint; const use_param : dyadic_call_parameter) : boolean;
        function Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2 : ccode_range; year : year_range;
                 var record_num : longint; const use_param : dyadic_call_parameter) : boolean; override;

        {Note: the next 4 should never be called}
        function Is_AnyYear_Joined_Initiation (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; override;
        function Is_AnyYear_Joined_targets (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; override;
        function Is_AnyYear_Joined_Revision (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; override;
        function Is_AnyYear_Joined_SQ (ccode1, ccode2 : ccode_range;
                 year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean; override;

        function wanted_new_NonDir_Dispute (const ccode1, ccode2 : ccode_range;
                 const year : year_range; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : boolean; override;
        function wanted_new_or_Continuing_NonDir_Dispute (const ccode1, ccode2 : ccode_range;
                 const year : year_range; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : boolean; override;

        function wanted_new_initiation (const ccode1, ccode2 : ccode_range;
                 const year : year_range; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : boolean; override;
        function wanted_new_or_continuing_initiation (const ccode1, ccode2 : ccode_range;
                 const year : year_range; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : boolean; override;
        function get_last_dispute_in_year ( const ccode1, ccode2 : ccode_range; const year : year_range;
                 const which_state : side_type; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : TDateTime;
        function get_peace_days (ccode1, ccode2 : ccode_range; year : year_range;
                 user_selections : user_selection_type; werner_peace_years_data : TWernerPeaceYears_obj) : integer;
        function get_peace_years (ccode1, ccode2 : ccode_range; year : year_range;
                 user_selections : user_selection_type; werner_peace_years_data : TWernerPeaceYears_obj) : integer;
        function get_num_new_crises (ccode1, ccode2 : ccode_range; year : year_range) : integer;
        function get_num_total_crises (ccode1, ccode2 : ccode_range; year : year_range) : integer;

     {Nondirected dyad year data.  Crdynum and year give a unique identifier.
      But ICB crisis number (crisis) is the equivalent to MID number.}

     private
        data : ICB_dyad_array_ptr;     {note: data in rows 1..num_dyadic_dispute_years}
        index : ICB_index_array_ptr;       {defined above}
        num_dyadic_dispute_years : longint;

        procedure compute_originators;  {Need to compute this once.}
     end;


    Function TCOWdyadic_dispute_obj_mem_overhead: longint;
    Function TMaoz_dyadic_dispute_obj_mem_overhead : longint;
    Function TICB_dyadic_dispute_obj_mem_overhead : longint;
    Function Twerner_PeaceYears_obj_mem_overhead : longint;

    {  ------------------------------------------------  }

implementation

uses euinoutd, dateutils, TraceUnit, Errbx;

   function earlier_date (ayr: year_range; amon:month_range; aday:MIDdaytype; byr: year_range; bmon:month_range;
                             bday: MIDdaytype) : boolean;
            {returns true if a was strictly earlier than b}
         begin
            earlier_date := false;
            {Go through all the possible comparisons of year, then month, then day.}
            if ayr < byr then earlier_date := true else
            if ayr > byr then earlier_date := false else
            if ayr = byr then
               begin
                  {if missing date, then don't know it's earlier, so return false}
                  if ((amon=-9) or (bmon=-9)) then earlier_date := false else
                  begin
                     if amon < bmon then earlier_date := true else
                     if amon > bmon then earlier_date := false else
                     if amon = bmon then
                        begin
                              {if missing date, then don't know it's earlier, so return false}
                           if ((aday=-9) or (bday=-9)) then earlier_date := false else
                              begin
                                 if aday < bday then earlier_date := true else
                                 if aday > bday then earlier_date := false else
                                 if aday = bday then earlier_date := false;
                              end;      {not -9}
                        end;          {amon=bmon}
                  end;   {ayr=byr}
               end;
         end;

   function which_earlier_date (ayr: year_range; amon:month_range; aday:MIDdaytype; aincidentnum : integer; byr: year_range; bmon:month_range;
                             bday: MIDdaytype; bincidentnum : integer; var identified_because_more_precise : boolean):integer;
                       {returns 1 or 2, depending if 1st or second date set is earlier}
            {returns 1 if a was earlier than b, or 2 if b was earlier than a.  If a date is not
             precise, then returns the # of the lower numbered MID incident.  If within same incident,
             then takes the more precise.}
         begin
            result := 1;
            identified_because_more_precise := false;
            {Go through all the possible comparisons of year, then month, then day.}
            if ayr < byr then result := 1 else
            if ayr > byr then result := 2 else
            if ayr = byr then
               begin
                  {if missing date, then may not know which is earlier. but if one is precise and
                   other is not, return the precise one.  }
                  {This is the old way, that always takes more precise date.  :
                  if ((amon <> -9) and (bmon = -9)) then result := 1
                  else if ((amon = -9) and (bmon <> -9)) then result := 2
                  else if ((amon = -9) and (bmon = -9)) then result := 1
                  else {both are valid month values, so check day.   }

                  if ((amon = -9) or (bmon = -9)) then
                     begin
                        identified_because_more_precise := true;
                        if aincidentnum < bincidentnum then result := 1
                        else if aincidentnum > bincidentnum then result := 2
                        else {they are equal, so take more precise date}
                           begin
                              if ((amon <> -9) and (bmon = -9)) then result := 1
                              else if ((amon = -9) and (bmon <> -9)) then result := 2
                              else if ((amon = -9) and (bmon = -9)) then result := 1
                           end;
                     end
                  else {both are valid month values, so check day.}
                  begin
                     if amon < bmon then result := 1 else
                     if amon > bmon then result := 2 else
                     if amon = bmon then
                        begin
                           {if missing date, then may not know which is earlier. but if one is precise and
                            other is not, return the precise one.  }
                           if ((aday = -9) or (bday = -9)) then
                           begin
                              identified_because_more_precise := true;
                              if aincidentnum < bincidentnum then result := 1
                              else if aincidentnum > bincidentnum then result := 2
                              else {they are equal, so take more precise date}
                                 begin
                                    if ((aday <> -9) and (bday = -9)) then result := 1
                                    else if ((aday = -9) and (bday <> -9)) then result := 2
                                    else if ((aday = -9) and (bday = -9)) then result := 1;
                                 end;

                           end  {not missing}
                           else {both are valid day values, so compare.}
                              begin
                                 if aday < bday then result := 1 else
                                 if aday > bday then result := 2 else
                                 if aday = bday then result := 1;
                              end;      {neither is -9}
                        end;          {amon=bmon}
                  end;   {ayr=byr}
               end;
         end;

    constructor Toverall_dispute_data_obj.init (disp_file_name, dispute_labels_infile_name : TFileName;
                dispute_data_version_for_read : dispute_data_version_for_read_type);
         {init reads in from raw file of dispute info.
          This will read from MID A 210.txt in most cases.}
       var data_struct : overall_disparrayptr;
           tempint, x, dispnum, current_rec : integer;
           start_mem : longint;
           infile, labelfile : text;
           a_rec : overall_dispute_rec_ptr;
           astring : string;
           dispute_trace : TTrace_obj;
           achar : char;

       begin
         dispute_trace := nil;
         try
            try
               start_mem := memavail;
               dispute_trace := TTrace_obj.init(trace.get_trace_level);
               dispute_trace.enter('Initializing main dispute data ');

               {first initialize the data structures.}
               trace.enter ('Initializing overall dispute structure');
               new (data_struct);
               data := data_struct;
               for x := 0 to max_disputes do data^[x] := nil;
               new (index);
               for x := 0 to top_dispute_num do index^[x] := initialized_value;

               {Now, read in data from file}
               try
                  assign (infile, disp_file_name);
                  reset (infile);
                  num_disputes := 0;
                  current_rec := 0;

                  {First thing to do is read the header line out of the file}
                  readln(infile);
                  while (not (eof (infile))) and (current_rec <= max_disputes) do
                     begin  {read a dispute}
                        dispute_trace.tick ('Executing Procedure: Initialize Dispute Data', 800);
                        new(a_rec);
                        a_rec^.idnum := read_csv_int (infile);
                        if a_rec^.idnum > max_dyadic_disputes then
                           EUGeneError ('Dispute ID num read from COW overall MID file is out of range, > longint.  Notify programmer.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.StDay := read_csv_int (infile);
                        a_rec^.StMonth := read_csv_int (infile);
                        a_rec^.StYear := read_csv_int (infile);
                        a_rec^.EndDay := read_csv_int (infile);
                        a_rec^.EndMonth := read_csv_int (infile);
                        a_rec^.EndYear := read_csv_int (infile);
                        a_rec^.Outcome := read_csv_int (infile);
                        a_rec^.Settlement := read_csv_int (infile);
                        a_rec^.Fatality := read_csv_int (infile);
                        case dispute_data_version_for_read of
                           disputeformat21 : begin    {do nothing, there is no precise fatality variable}
                                                a_rec^.Fatality_precise := missing_value;
                                             end;
                           disputeformat30 : begin    {read precise fatalties}
                                                a_rec^.Fatality_precise := read_csv_int (infile);
                                             end
                           else EUGeneError ('Unrecognized dispute format type in reading overall dispute data. Program error - notify programmer',1,stop,error_log);
                        end;     {case}
                        a_rec^.MaxDur := read_csv_int (infile);
                        a_rec^.MinDur := read_csv_int (infile);
                        a_rec^.HiAct := read_csv_int (infile);
                        a_rec^.HostLev := read_csv_int (infile);
                          {in input, -1 means is reciprocated}
                        tempint := read_csv_int (infile);
                        if tempint = -1 then a_rec^.reciprocated := true else a_rec^.reciprocated := false;
                        a_rec^.NumberA := read_csv_int (infile);
                        a_rec^.NumberB := read_csv_int (infile);
                        case dispute_data_version_for_read of
                           disputeformat21 : begin    {do nothing, there is no link variable}
                                                a_rec^.link1 := '';
                                                a_rec^.link2 := '';
                                                a_rec^.link3 := '';
                                             end;
                           disputeformat30 : begin    {read precise fatalties}
                                                a_rec^.link1 := read_csv_string (infile);
                                                a_rec^.link2 := read_csv_string (infile);
                                                a_rec^.link3 := read_csv_string (infile);
                                             end
                           else EUGeneError ('Unrecognized dispute format type in reading overall dispute data. Program error - notify programmer',1,stop,error_log);
                        end;     {case}
                        case dispute_data_version_for_read of
                           disputeformat21 : begin    {do nothing, there is no link variable}
                                                a_rec^.Ongoing_2001 := false;
                                             end;
                           disputeformat30 : begin    {read precise fatalties}
                                                a_rec^.Ongoing_2001 := StrToBool(read_csv_string (infile));
                                             end
                           else EUGeneError ('Unrecognized dispute format type in reading overall dispute data. Program error - notify programmer',1,stop,error_log);
                        end;     {case}

                        a_rec^.Version := read_csv_real (infile);

                        a_rec^.dispute_label := '';

                        if not (eof (infile)) then
                          readln (infile);      {go to begin of next record}

                       {Just successfully read a dispute:  put it on the dispute list }
                        data^[current_rec] := a_rec;

                       {Also put the dispute # on the index list}
                        index^[data^[current_rec]^.idnum] := current_rec;

                       {now inc for next record} 
                        inc(current_rec);
                     end;             {while not eof (infile);}

                  num_disputes := current_rec;
                  if num_disputes >= max_disputes then
                     begin
                       ShowMessage ('Stopped reading main dispute data at record '+inttostr(num_disputes)+
                                ';  maximum number of disputes is '+inttostr(max_disputes));
                   end;
               finally
                  CloseFile (infile);
               end;
            except
               on EUserInterrupt do raise;
               on EInOutError do
                 begin
                    FileErrorBox.maindo ('Error opening file "'+disp_file_name+ '"',
                                         'File could not be opened for input.',
                                         'File may be in use by another program, or may be missing.');
                    FileErrorBox.showmodal;
                    raise;
                 end;
            end;

             {Now read labels for disputes with labels.  First is the dispute #, then a label
              enclosed in double quotes, then the version # (that I'll ignore) }
            {First thing to do is read the header line out of the file}
            try
               try
                  assign (labelfile, dispute_labels_infile_name);
                  reset (labelfile);
                  {First thing to do is read the header line out of the file}
                  readln(labelfile);
                  repeat
                      dispnum := read_csv_int (labelfile);
                      data^[index^[dispnum]]^.dispute_label := read_csv_string (labelfile);
                      {could also read real here for version #}
                      readln (labelfile);
                  until eof (labelfile);
               finally
                  close (labelfile);
                  dispute_trace.tickdone;
               end;
            except
               on EUserInterrupt do raise;
               on EInOutError do
                 begin
                    FileErrorBox.maindo ('Error opening file "'+dispute_labels_infile_name+ '"',
                                         'File could not be opened for input.',
                                         'File may be in use by another program, or may be missing.');
                    FileErrorBox.showmodal;
                    raise;
                 end;
            end;


            created := true;

         finally
            dispute_trace.exit('Finished Initializing main dispute data ');
            dispute_trace.free;
            trace.message (' required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
            trace.exit('Finished initializing raw MID main dispute data');
         end;

       end;   {constructor init}

    destructor Toverall_dispute_data_obj.destroy;
    var x : integer;
       begin
         try
            for x := 1 to max_disputes do
               if data^[x] <> nil then dispose (data^[x]);
            if data <> nil then dispose(data);
            data := nil;
            if index <> nil then dispose(index);
            index := nil;
            created := false;
            inherited destroy;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
       end;

    function Toverall_dispute_data_obj.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then if created=true then initialized := true;
      end;

    function Toverall_dispute_data_obj.have_info_on_midnum (dispute_number : dispute_id_range) : boolean;
       begin
          result := false;
          check_initialized;
          if dispute_number = null_dispute_number then {no dispute} result := false else
             if index^[dispute_number] = null_dispute_number then result := false else
             result := true;
       end;

    procedure Toverall_dispute_data_obj.check_initialized;
          begin
            if not(initialized) then
               begin
                  EUGeneError ('Get a dispute_dat_obj dispute value get called before initialization',
                                  5, stop, error_log);
               end
          end;

    function Toverall_dispute_data_obj.get_styear_dispute (dispute_number : dispute_id_range) : year_range;
          {get should be called with dispute #, internally it will figure out where this
           dispute is in the list.}     {Q:  this is called with MID #}
       begin
          check_initialized;
          if dispute_number = null_dispute_number then {no dispute} result := min_year else
             get_styear_dispute := data^[index^[dispute_number]]^.StYear;
       end;

    function Toverall_dispute_data_obj.get_version (dispute_number : dispute_id_range) : real;
       begin
          check_initialized;
          if dispute_number = null_dispute_number then {no dispute} result := 0 else
             result := data^[index^[dispute_number]]^.version;
       end;

    function Toverall_dispute_data_obj.get_outcome (dispute_number : dispute_id_range) : outcometype;
       begin
          check_initialized;
          if dispute_number = null_dispute_number then {no dispute} result := 0 else
             get_outcome := data^[index^[dispute_number]]^.outcome;
       end;

    function Toverall_dispute_data_obj.get_settlement (dispute_number : dispute_id_range) : settlementtype;
       begin
          check_initialized;
          if dispute_number = null_dispute_number then {no dispute} result := 0 else
             get_settlement := data^[index^[dispute_number]]^.settlement;
       end;

    function Toverall_dispute_data_obj.get_fatality_dispute (dispute_number : dispute_id_range) : fatalitytype;
       begin
          check_initialized;
          if dispute_number = null_dispute_number then {no dispute} result := 0 else
             get_fatality_dispute := data^[index^[dispute_number]]^.fatality;
       end;

    function Toverall_dispute_data_obj.get_hiact_dispute (dispute_number : dispute_id_range) : actiontype;
       begin
          check_initialized;
          if dispute_number = null_dispute_number then {no dispute} result := 0 else
             get_hiact_dispute := data^[index^[dispute_number]]^.hiact;
       end;

    function Toverall_dispute_data_obj.get_hostlev_dispute (dispute_number : dispute_id_range) : hostlevtype;
       begin
          check_initialized;
          if dispute_number = null_dispute_number then {no dispute} result := 0 else
             get_hostlev_dispute := data^[index^[dispute_number]]^.hostlev;
           {Also, want to change coding as a "-9" hostility to a 1, which means
            no militarized response, although you were a target.}
           if (result = -9) then result := 1;
       end;

    function Toverall_dispute_data_obj.get_reciprocated (dispute_number : dispute_id_range) : boolean;
       begin
          check_initialized;
          if dispute_number = null_dispute_number then {no dispute} result := false else
             get_reciprocated := data^[index^[dispute_number]]^.reciprocated;
       end;

    function Toverall_dispute_data_obj.get_numberA (dispute_number : dispute_id_range) : integer;
       begin
          check_initialized;
          if dispute_number = null_dispute_number then {no dispute} result := 0 else
             get_numberA := data^[index^[dispute_number]]^.numberA;
       end;

    function Toverall_dispute_data_obj.get_numberB (dispute_number : dispute_id_range) : integer;
       begin
          check_initialized;
          if dispute_number = null_dispute_number then {no dispute} result := 0 else
             get_numberB := data^[index^[dispute_number]]^.numberB;
       end;

    function Toverall_dispute_data_obj.get_dispute_name (dispute_number : dispute_id_range) : string;
       {An extra check is present for get_dispute_name, because it is also called from the Maoz
        structure, which has some dispute #s that are not in the original COW data with names.}
       begin
          check_initialized;
          if dispute_number = null_dispute_number then {no dispute} result := '' else
             if index^[dispute_number] = null_dispute_number then result := '' else
             result := data^[index^[dispute_number]]^.dispute_label;
       end;

    function Toverall_dispute_data_obj.get_Fatality_precise (dispute_number : dispute_id_range) : integer;
       begin
          check_initialized;
          if dispute_number = null_dispute_number then {no dispute} result := 0 else
             result := data^[index^[dispute_number]]^.Fatality_precise;
       end;

    function Toverall_dispute_data_obj.get_Link1 (dispute_number : dispute_id_range) : string;
       begin
          check_initialized;
          if dispute_number = null_dispute_number then {no dispute} result := '' else
             result := data^[index^[dispute_number]]^.link1;
       end;

    function Toverall_dispute_data_obj.get_Link2 (dispute_number : dispute_id_range) : string;
       begin
          check_initialized;
          if dispute_number = null_dispute_number then {no dispute} result := '' else
             result := data^[index^[dispute_number]]^.link2;
       end;

    function Toverall_dispute_data_obj.get_Link3 (dispute_number : dispute_id_range) : string;
       begin
          check_initialized;
          if dispute_number = null_dispute_number then {no dispute} result := '' else
             result := data^[index^[dispute_number]]^.link3;
       end;

    function Toverall_dispute_data_obj.get_Ongoing_2001 (dispute_number : dispute_id_range) : boolean;
       begin
          check_initialized;
          if dispute_number = null_dispute_number then {no dispute} result := false else
             result := data^[index^[dispute_number]]^.Ongoing_2001;
       end;

{  ------------------------------------------------  }

    constructor TCountry_dispute_data_obj.init (infile_name : TFileName; dispute_data_version_for_read : dispute_data_version_for_read_type);
      {init reads in from raw file of country dispute info.
       This will come from MID_B210.txt in some cases, but updated to check and work for v3.0.}
      var infile:text;
         tempint, current_rec, x : integer;
         achar: char;
         a_rec : country_dispute_rec_ptr;
         country_dispute_trace : TTrace_obj;
         start_mem : longint;

      procedure sort_by_disp_ccode_startdate;
         var x, y, top_country_dispnum : integer;
         done : boolean;
            {sorts the data in order by MIDnum, then by ccode in each, then by start date of countryrec.}

        procedure quicksort_country_disputes_by_midnum (left, right:integer);
                  {Sorts in order low to high}
            var
               base_mid, comp_mid, i, j: integer;
               temp: country_dispute_rec;

            begin
             if right > left then
               begin
                  base_mid := data^[right]^.idnum;
                  i := left - 1;
                  j := right;
                  repeat
                     repeat
                        i := i + 1;
                        comp_mid := data^[i]^.idnum;
                     until comp_mid >= base_mid;
                     repeat
                        j := j - 1;
                        comp_mid := data^[j]^.idnum;
                     until (comp_mid <= base_mid) or (j = left);
                     temp := data^[i]^;
                     data^[i]^ := data^[j]^;
                     data^[j]^ := temp;
                  until j <= i;
                  data^[j]^ := data^[i]^;
                  data^[i]^ := data^[right]^;
                  data^[right]^ := temp;
                  quicksort_country_disputes_by_midnum (left, i-1);
                  quicksort_country_disputes_by_midnum (i+1, right);
               end;
            end;      {procedure quicksort_dyads_by_year}

        procedure quicksort_country_disputes_by_ccode (left, right:integer);
                  {Sorts in order low to high}
            var
               base_value, comp_value, i, j: integer;
               temp: country_dispute_rec;

            begin
             if right > left then
               begin
                  base_value := data^[right]^.ccode;
                  i := left - 1;
                  j := right;
                  repeat
                     repeat
                        i := i + 1;
                        comp_value := data^[i]^.ccode;
                     until comp_value >= base_value;
                     repeat
                        j := j - 1;
                        comp_value := data^[j]^.ccode;
                     until (comp_value <= base_value) or (j = left);
                     temp := data^[i]^;
                     data^[i]^ := data^[j]^;
                     data^[j]^ := temp;
                  until j <= i;
                  data^[j]^ := data^[i]^;
                  data^[i]^ := data^[right]^;
                  data^[right]^ := temp;
                  quicksort_country_disputes_by_ccode (left, i-1);
                  quicksort_country_disputes_by_ccode (i+1, right);
               end;
            end;      {procedure quicksort_country_disputes_by_ccode}

        procedure quicksort_country_disputes_by_startdate (left, right:integer);
                  {Sorts in order low to high}
            var
               base_value, comp_value : record
                  StDay, EndDay: MIDdaytype;
                  StMonth, EndMonth: month_range;
                  StYear, EndYear : year_range;
                  end;
               i, j: integer;
               temp: country_dispute_rec;

            begin
             if right > left then
               begin
                  base_value.StDay := data^[right]^.StDay;
                  base_value.StMonth := data^[right]^.StMonth;
                  base_value.StYear := data^[right]^.StYear;
                  i := left - 1;
                  j := right;
                  repeat
                     repeat
                        i := i + 1;
                        comp_value.StDay := data^[i]^.StDay;
                        comp_value.StMonth := data^[i]^.StMonth;
                        comp_value.StYear := data^[i]^.StYear;
                        {usual check:  comp_value >= base_value;
                         to do this use not(earlier) because earlier_date is strictly <}
                     until not (earlier_date (comp_value.StYear, comp_value.stmonth, comp_value.StDay, base_value.StYear, base_value.StMonth, base_value.stday));
                     repeat
                        j := j - 1;
                        comp_value.StDay := data^[j]^.StDay;
                        comp_value.StMonth := data^[j]^.StMonth;
                        comp_value.StYear := data^[j]^.StYear;
                        {usual check:  (comp_value <= base_value) ;  use not b/c of strictly in earlier func.}
                     until not (earlier_date (base_value.StYear, base_value.StMonth, base_value.stday, comp_value.StYear, comp_value.stmonth, comp_value.StDay)) or (j = left);
                     temp := data^[i]^;
                     data^[i]^ := data^[j]^;
                     data^[j]^ := temp;
                  until j <= i;
                  data^[j]^ := data^[i]^;
                  data^[i]^ := data^[right]^;
                  data^[right]^ := temp;
                  quicksort_country_disputes_by_startdate (left, i-1);
                  quicksort_country_disputes_by_startdate (i+1, right);
               end;
            end;      {procedure quicksort_country_disputes_by_ccode}

            {   -----------------------  }

         begin
            top_country_dispnum := num_country_disputes-1;
            {first sort by midnum}
            quicksort_country_disputes_by_midnum (0, top_country_dispnum);
            {now sort by ccode within each midnum}

            trace.message ('Starting country-disp-rect sort by country within idnum...');
            x := 0;
            y := 0;
            repeat
               done := false;
               repeat
                  y := y + 1;
                  if (y >= top_country_dispnum) then done:= true
                  else if (data^[x]^.idnum) <> (data^[y]^.idnum) then done:=true;
               until done;
               if y = top_country_dispnum then quicksort_country_disputes_by_ccode (x, y) else
                  quicksort_country_disputes_by_ccode (x, y-1);
               x := y;
            until y > top_country_dispnum;

            {That did sorts by midnum, and ccode.  Now do by start date within each mid and ccode.}
            trace.message ('Doing quicksort by styear within mid-id and ccode...');
            x := 0;
            y := 0;
            repeat
               done := false;
               repeat
                  y := y + 1;
                  if (y >= top_country_dispnum) then done:= true
                  else if ((data^[x]^.idnum) <> (data^[y]^.idnum)) or
                          ((data^[x]^.ccode) <> (data^[y]^.ccode)) then done:=true;
               until done;
               if y = top_country_dispnum then quicksort_country_disputes_by_startdate (x, y) else
                  quicksort_country_disputes_by_startdate (x, y-1);
               x := y;
            until y > top_country_dispnum;

            {Don'need any further sorting for now.}
            trace.message ('Finished sorting country dispute recs.');

         end;   {sort country disputes}


      begin                            {main procedure country disputes.init.}
         country_dispute_trace := nil;
         try
            try
               start_mem := memavail;
               trace.enter('Initializing country dispute data ');
               {first, initialize dispute index, and create main data array}
               country_dispute_trace := TTrace_obj.init(trace.get_trace_level);
               trace.message ('Initializing country dispute data structure');

               new(data);
               for x := 0 to max_country_disputes do data^[x] := nil;
               new(index);
               for x := 0 to top_dispute_num do index^[x] := null_dispute_number;
               num_country_disputes := 0;
               current_rec := 0;

               assign (infile, infile_name);
               reset (infile);
               {First line is line of labels.  Read it to skip over it.}
               readln(infile);

               while (not (eof (infile))) and (current_rec <= max_country_disputes) do
                  begin
                     country_dispute_trace.tick ('Executing Procedure: Read Country Disputes',5600);
                     new(a_rec);
                     a_rec^.idnum := read_csv_int (infile);
                     if a_rec^.idnum > max_dyadic_disputes then
                        EUGeneError ('Dispute ID num read from COW Country MID file is out of range, > longint.  Notify programmer.  Program continues, but further errors may result.',1,continue, error_log);

                     {next record is 3 char abbrev.  There will be at least one space before it}
                     a_rec^.cabbrev := '   ';
                     repeat
                        read (infile, achar);
                     until ( (achar <> ' ') and (achar <> chr(9)) and (achar <> '"') );
                     a_rec^.cabbrev[1] := achar;
                     read (infile, a_rec^.cabbrev[2]);
                     read (infile, a_rec^.cabbrev[3]);
                     {3 char abbrev now done;  continue with integers}
                     a_rec^.ccode := read_csv_int (infile);
                     a_rec^.StDay := read_csv_int (infile);
                     a_rec^.StMonth := read_csv_int (infile);
                     a_rec^.StYear := read_csv_int (infile);
                     a_rec^.EndDay := read_csv_int (infile);
                     a_rec^.EndMonth := read_csv_int (infile);
                     a_rec^.EndYear := read_csv_int (infile);
                     a_rec^.sideA := strtobool(read_csv_string(infile));
                     a_rec^.revisionist := strtobool(read_csv_string(infile));
                     a_rec^.revtype := read_csv_int (infile);
                     case dispute_data_version_for_read of
                        disputeformat21 : begin    {do nothing, there is no 2nd issue coded}
                                             a_rec^.revtype2 := missing_value;
                                          end;
                        disputeformat30 : begin    {read precise fatalties}
                                             a_rec^.revtype2 := read_csv_int (infile);
                                          end
                        else EUGeneError ('Unrecognized dispute format type in reading overall dispute data. Program error - notify programmer',1,stop,error_log);
                     end;     {case}
                     a_rec^.Fatality := read_csv_int (infile);
                     case dispute_data_version_for_read of
                        disputeformat21 : begin    {do nothing, there is no precise fatality variable}
                                             a_rec^.Fatality_Precise := missing_value;
                                          end;
                        disputeformat30 : begin    {read precise fatalties}
                                             a_rec^.Fatality_Precise := read_csv_int (infile);
                                          end
                        else EUGeneError ('Unrecognized dispute format type in reading overall dispute data. Program error - notify programmer',1,stop,error_log);
                     end;     {case}
                     a_rec^.HiAct := read_csv_int (infile);
                     a_rec^.HostLev := read_csv_int (infile);
                        {FOR originator variable, version 2.1 data has a -1=yes, 0=no; MID 3.0 has standard 0/1.}
                     case dispute_data_version_for_read of
                        disputeformat21 : begin
                                             tempint := read_csv_int (infile);
                                             if tempint=-1 then a_rec^.Originator := true else a_rec.originator := false;
                                          end;
                        disputeformat30 : begin    {read as boolean}
                                             a_rec^.Originator := strtobool (read_csv_string (infile));
                                          end
                        else EUGeneError ('Unrecognized dispute format type in reading overall dispute data. Program error - notify programmer',1,stop,error_log);
                     end;     {case}
                     a_rec^.Version := read_csv_real (infile);

                    {Earlier Kludge fix for 2 miscoded MIDs in 3.0.  But actually, data looks OK
                     with these originators as true, so this fix commented out 7/2/07.}
                    {if (a_rec^.idnum = 4190) and (a_rec^.ccode=2) then a_rec^.Originator := false;
                    if (a_rec^.idnum = 4197) and (a_rec^.ccode=20) then a_rec^.Originator := false;  }

                    
                    {Although this is the end of the line, It seems I have not
                     read to the end of the record, so do a readln to advance to
                     beginning of next record and line.}
                     if not (eof (infile)) then
                        readln (infile);      {go to begin of next record}

                     {Just successfully read a dispute:  put it on the dispute list}
                     data^[current_rec] := a_rec;

                     inc(current_rec);

                  end;             {while not eof (infile);}

                  num_country_disputes := current_rec;
                  if num_country_disputes >= max_country_disputes then
                     begin
                        ShowMessage ('Stopped reading data at record '+inttostr(num_country_disputes)+
                                 ';  maximum number of country disputes is '+inttostr(max_country_disputes));
                     end;

                  {Sort the data so in order by MIDnum and dates, necessary for later procedures.}
                  sort_by_disp_ccode_startdate;

                  {Now that the data are sorted, put the dispute # on the index list to get the first record for each dispute #}
                  for current_rec := 0 to num_country_disputes - 1 do
                     if index^[data^[current_rec]^.idnum] = null_dispute_number then
                        index^[data^[current_rec]^.idnum] := current_rec;

                  country_dispute_trace.tickdone;
                  created := true;

               finally
                  close (infile);
                  country_dispute_trace.free;
                  if debug[4] then
                     trace.message (' required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
                  trace.exit('Finished initializing country dispute data');
               end;
            except
               on EUserInterrupt do raise;
               on EInOutError do
                 begin
                    FileErrorBox.maindo ('Error opening file "'+infile_name+ '"',
                                         'File could not be opened for input.',
                                         'File may be in use by another program, or may be missing.');
                    FileErrorBox.showmodal;
                    raise;
                 end;
            end;

       end;

    procedure TCountry_dispute_data_obj.check_initialized;
          begin
            if not(initialized) then
               begin
                  EUGeneError ('Get a country_dispute value get called before initialization', 5, stop, error_log);
               end
          end;

    destructor TCountry_dispute_data_obj.destroy;
       var x : integer;
       begin
         try
            if self <> nil then
            begin
               if data <> nil then
               for x := 1 to max_country_disputes do
                  if data^[x] <> nil then dispose (data^[x]);
               if data <> nil then dispose(data);
               data := nil;
               if index <> nil then dispose(index);
               index := nil;
               created := false;
            end;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
       end;

    function TCountry_dispute_data_obj.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then if created=true then initialized := true;
      end;

    function TCountry_dispute_data_obj.get_ccode (country_dispute_number : country_dispute_range) : ccode_range;
       begin
         check_initialized;
         if country_dispute_number = null_dispute_number then {no dispute} result := 0 else
            get_ccode := data^[country_dispute_number]^.ccode;
       end;

    function TCountry_dispute_data_obj.get_version (country_dispute_number : country_dispute_range) : real;
       begin
         check_initialized;
         if country_dispute_number = null_dispute_number then {no dispute} result := 0 else
            result := data^[country_dispute_number]^.version;
       end;

    function TCountry_dispute_data_obj.get_disputeid (country_dispute_number : country_dispute_range) : dispute_id_range;
       begin
         result := 0;
         check_initialized;
         try
         if country_dispute_number = null_dispute_number then {no dispute} result := 0 else
            result := data^[country_dispute_number]^.idnum;
         except
            showmessage ('Error calling dispute ID dispnum'+inttostr(country_dispute_number));
            result:=1;
         end;
       end;

    function TCountry_dispute_data_obj.get_sideA(dispnum : country_dispute_range) : boolean;
       begin
         check_initialized;
            {1=yes, is sideA;  0=no}
         if dispnum = null_dispute_number then {no dispute} result := false else
            get_sideA := data^[dispnum]^.sideA;
       end;

    function TCountry_dispute_data_obj.get_revisionist(dispnum : country_dispute_range) : boolean;
       begin
         check_initialized;
            {0=no, not revisionist;  1=is revisionist}
         if dispnum = null_dispute_number then {no dispute} result := false else
            get_revisionist := data^[dispnum]^.revisionist;
       end;

    function TCountry_dispute_data_obj.get_revtype(dispnum : country_dispute_range) : revisiontype;
       begin
         check_initialized;
         if dispnum = null_dispute_number then {no dispute} result := 0 else
            get_revtype := data^[dispnum]^.revtype;
       end;

    function TCountry_dispute_data_obj.get_originator (dispnum : country_dispute_range) : boolean;
       begin
         check_initialized;
         if dispnum = null_dispute_number then {no dispute} result := false else
            get_originator := data^[dispnum]^.originator;
       end;

    function TCountry_dispute_data_obj.get_revtype2 (dispnum : country_dispute_range) : revisiontype;
       begin
         check_initialized;
         if dispnum = null_dispute_number then {no dispute} result := 0 else
            get_revtype2 := data^[dispnum]^.revtype2;
       end;

    function TCountry_dispute_data_obj.get_Fatality_precise (dispnum : country_dispute_range) : integer;
       begin
         check_initialized;
         if dispnum = null_dispute_number then {no dispute} result := 0 else
            get_Fatality_precise := data^[dispnum]^.Fatality_precise;
       end;

    function TCountry_dispute_data_obj.get_num_country_disputes : country_dispute_range;
       begin
         check_initialized;
         get_num_country_disputes := num_country_disputes;
       end;

    function TCountry_dispute_data_obj.get_last_dispnum : country_dispute_range;
       begin
         check_initialized;
         get_last_dispnum := num_country_disputes - 1;
       end;

    function TCountry_dispute_data_obj.get_endyear(dispnum : country_dispute_range) : year_range;
       begin
         check_initialized;
         if dispnum = null_dispute_number then {no dispute} result := min_year else
            get_endyear := data^[dispnum]^.endyear;
       end;

    function TCountry_dispute_data_obj.get_endmonth(dispnum : country_dispute_range) : month_range;
       begin
         check_initialized;
         if dispnum = null_dispute_number then {no dispute} result := 0 else
            get_endmonth := data^[dispnum]^.endmonth;
       end;

    function TCountry_dispute_data_obj.get_endday(dispnum : country_dispute_range) : MIDdaytype;
       begin
         check_initialized;
         if dispnum = null_dispute_number then {no dispute} result := 0 else
            get_endday := data^[dispnum]^.endday;
       end;

    function TCountry_dispute_data_obj.get_styear(dispnum : country_dispute_range) : year_range;
       begin
         check_initialized;
         if dispnum = null_dispute_number then {no dispute} result := min_year else
            get_styear := data^[dispnum]^.styear;
       end;

    function TCountry_dispute_data_obj.get_stmonth(dispnum : country_dispute_range) : month_range;
       begin
         check_initialized;
         if dispnum = null_dispute_number then {no dispute} result := 0 else
            get_stmonth := data^[dispnum]^.stmonth;
       end;

    function TCountry_dispute_data_obj.get_stday(dispnum : country_dispute_range) : MIDdaytype;
       begin
         check_initialized;
         if dispnum = null_dispute_number then {no dispute} result := 0 else
            get_stday := data^[dispnum]^.stday;
       end;

    function TCountry_dispute_data_obj.get_fatality(dispnum : country_dispute_range) : fatalitytype;
       begin
         check_initialized;
         if dispnum = null_dispute_number then {no dispute} result := 0 else
            get_fatality := data^[dispnum]^.fatality;
       end;

    function TCountry_dispute_data_obj.get_HiAct(dispnum : country_dispute_range) : ActionType;
       begin
         check_initialized;
         if dispnum = null_dispute_number then {no dispute} result := 0 else
            get_HiAct := data^[dispnum]^.hiact;
       end;

    function TCountry_dispute_data_obj.get_hostlev(dispnum : country_dispute_range) : hostlevtype;
       begin
         check_initialized;
         if dispnum = null_dispute_number then {no dispute} result := 0 else
            get_hostlev := data^[dispnum]^.hostlev;
           {Also, want to change coding as a "-9" hostility to a 1, which means
            no militarized response, although you were a target.}
         if (result = -9) then result := 1;
       end;

    function TCountry_dispute_data_obj.date_overlap (disp1, disp2 : country_dispute_range) : boolean;
       {from 2 country dispute #s, returns true if they were in dispute at same time}
       {main function works by seeing if either ended before the other began.  If so, no overlap.}
       begin
         check_initialized;
         if (disp1 = null_dispute_number) or (disp2 = null_dispute_number)  then
            {no dispute} result := false else
         if (earlier_date (get_endyear(disp1), get_endmonth(disp1), get_endday(disp1),
                           get_styear(disp2), get_stmonth(disp2), get_stday(disp2))  or
             earlier_date (get_endyear(disp2), get_endmonth(disp2), get_endday(disp2),
                           get_styear(disp1), get_stmonth(disp1), get_stday(disp1)) )
            then date_overlap := false else
            date_overlap := true;
       end;

    function TCountry_dispute_data_obj.get_country_disp_num (midnum : dispute_id_range; accode : ccode_range) : country_dispute_range;
       {returns the proper country dispute # for this ccode in this MID.}
       {*** what if multiple country recs?  Then it will take the first one chronologically.}
       var spot : integer;
           done : boolean;
       begin
          result := null_dispute_number;
          done := false;
          spot := index^[midnum];  {this is first spot on index for this mid.}
          if spot <> null_dispute_number then
             repeat
                if get_ccode(spot) = accode then
                   begin
                      result := spot;
                      done := true;
                   end;
                inc(spot);
                if (spot > get_last_dispnum) then done:=true else
                   if (get_disputeid(spot)<>midnum) then done:= true;
             until done;
          if result = null_dispute_number then
             EUGeneError ('Programming error - returned null country_disp_num while looking for country dispute for a pi record [MID '+ inttostr(midnum)+ ', cccode '+inttostr(accode)+'] in the make 3.0 dyadic disputes proc.  Notify programmer.  Attempting to continue, but program failure likely.',1,continue,error_log);
       end;

    function TCountry_dispute_data_obj.get_num_country_recs_for_state (midnum : dispute_id_range; accode : ccode_range) : integer;
       {returns the num ber of records in mid b a country has.  Used for breakup up dyadic
        mids if a state comes and goes.}
       var spot : integer;
           done : boolean;
       begin
          result := 0;
          done := false;
          spot := index^[midnum];  {this is first spot on index for this mid.}
          if spot <> null_dispute_number then
             repeat
                if get_ccode(spot) = accode then
                   begin
                      inc(result);
                   end;
                inc(spot);
                if (spot > get_last_dispnum) then done:=true else
                   if (get_disputeid(spot)<>midnum) then done:= true;
             until done;
          if result = 0 then
             EUGeneError ('Programming error - returned 0 count while looking for country dispute in country_disp_data record [MID '+ inttostr(midnum)+ ', cccode '+inttostr(accode)+'] in the make 3.0 dyadic disputes proc.  Notify programmer.  Attempting to continue, but program failure likely.',1,continue,error_log);
       end;


    function TCountry_dispute_data_obj.get_country_disp_num_when_many (midnum : dispute_id_range; accode : ccode_range; mid_episode_to_find : integer) : integer;
       {returns the dispute number of the nth record in mid b for a country.
        If the data is sorted by start year, this will get us the nth epiosde for a country in a dispute.}
       var spot, count : integer;
           done : boolean;
       begin
          result := null_dispute_number;
          done := false;
          spot := index^[midnum];  {this is first spot on index for this mid.}
          count := 0;
          if spot <> null_dispute_number then
             repeat
                if get_ccode(spot) = accode then
                   begin
                      result := spot;
                      inc(count);
                   end;
                inc(spot);
                if (spot > get_last_dispnum) or (count=mid_episode_to_find) then
                      done:=true
                   else if (get_disputeid(spot)<>midnum) then
                      done:= true;
             until done;
          if count<mid_episode_to_find then
             EUGeneError ('Programming error - did not find episode '+inttostr(mid_episode_to_find)+ ' of MID '+inttostr(midnum)+' in func get country disp num when many.',1,continue, error_log);
          if result = null_dispute_number then
             EUGeneError ('Programming error - returned null country_disp_num while looking for country dispute for a pi record [MID '+ inttostr(midnum)+ ', cccode '+inttostr(accode)+'] in the make 3.0 dyadic disputes proc.  Notify programmer.  Attempting to continue, but program failure likely.',1,continue,error_log);
       end;

        {  ------------------------------------------------  }

   constructor Tparticipant_incident_data_obj.init (infile_name : TFileName);
      {init reads in from raw file of country dispute info.
       This will come from MID_B210.txt in most cases.}
      var infile:text;
          x : integer;
          pi_trace : TTrace_obj;
          a_rec : pirecord;
          tempint : integer;
          achar: char;

      begin
         pi_trace := nil;
         try
            try
               trace.enter('Initializing participant-incident dispute data ');
               {first, initialize dispute index, and create main data array}
               pi_trace := TTrace_obj.init(trace.get_trace_level);
               trace.message ('Initializing participant incident data structure');

               setlength(data, 0);
               new(index);
               for x := 0 to top_dispute_num do index^[x] := null_dispute_number;
               assignfile (infile, infile_name);
               reset (infile);
               num_participant_incident_records := 0;

               {First line is line of labels.  Read it to skip over it.}
               readln(infile);

               {Now read records}
               while (not (eof (infile))) and (num_participant_incident_records < max_participant_incident_records) do
                  begin
                     pi_trace.tick ('Executing Procedure: Read Participant-Incident records',5000);
                     {Read into a_rec, then assign values}

                     a_rec.mid_idnum := read_csv_int (infile);
                     if a_rec.mid_idnum > top_dispute_num then
                        EUGeneError ('Dispute ID num read from COW participant incident MID file is out of range, > longint.  Notify programmer.  Program continues, but further errors may result.',1,continue, error_log);
                     a_rec.mid_incident_idnum := read_csv_longint (infile);

                     {next record is 3 char abbrev.  }
                     a_rec.cabbrev := read_csv_string (infile);
                     a_rec.ccode := read_csv_int (infile);
                     a_rec.StDay := read_csv_int (infile);
                     a_rec.StMonth := read_csv_int (infile);
                     a_rec.StYear := read_csv_int (infile);
                     a_rec.EndDay := read_csv_int (infile);
                     a_rec.EndMonth := read_csv_int (infile);
                     a_rec.EndYear := read_csv_int (infile);
                     a_rec.insideA := strtobool(read_csv_string (infile));
                     a_rec.sideA := strtobool(read_csv_string (infile));
                     a_rec.Fatality := read_csv_int (infile);
                     a_rec.fatalityv := read_csv_int (infile);
                     a_rec.action := read_csv_int (infile);
                     a_rec.HostLev := read_csv_int (infile);
                     a_rec.issue1 := read_csv_int (infile);
                     a_rec.issue2 := read_csv_int (infile);
                     a_rec.Version := read_csv_real (infile);

                     if not (eof (infile)) then
                        readln (infile);      {go to begin of next record}

                     {Just successfully read a record:  put it on the pi list}
                     setlength(data, length(data)+1);
                     data[high(data)] := a_rec;

                     {Also put the dispute # on the index list if this is the first record for this disp #}
                     if index^[data[high(data)].mid_idnum] = null_dispute_number then
                        index^[data[high(data)].mid_idnum] := high(data);

                  end;             {while not eof (infile);}

                  num_participant_incident_records := high(data);
                  if num_participant_incident_records >= max_participant_incident_records then
                     begin
                        ShowMessage ('Stopped reading data at record '+inttostr(num_participant_incident_records)+
                                 ';  maximum number of country disputes is '+inttostr(max_participant_incident_records));
                     end;

                  pi_trace.tickdone;
                  created := true;

               finally
                  close (infile);
                  pi_trace.free;
                  trace.exit('Finished initializing participant-incident data');
               end;
            except
               on EUserInterrupt do raise;
               on EInOutError do
                 begin
                    FileErrorBox.maindo ('Error opening file "'+infile_name+ '"',
                                         'File could not be opened for input.',
                                         'File may be in use by another program, or may be missing.');
                    FileErrorBox.showmodal;
                    raise;
                 end;
            end;               {except}
       end;       {constructor}

   destructor Tparticipant_incident_data_obj.destroy;
      var x : integer;
      begin
         try
            if self <> nil then
            begin
               setlength(data,0);
               if index <> nil then dispose(index);
               index := nil;
               created := false;
            end;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
       end;

   function Tparticipant_incident_data_obj.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then if created=true then initialized := true;
      end;

   procedure Tparticipant_incident_data_obj.check_initialized;
          begin
            if not(initialized) then
               begin
                  EUGeneError ('Get a participant_incident_record value get called before initialization', 5, stop, error_log);
               end
          end;

   function Tparticipant_incident_data_obj.get_mid_idnum (pi_id : integer) : dispute_id_range;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].mid_idnum
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
       end;

   function Tparticipant_incident_data_obj.get_mid_incident_idnum (pi_id : integer) : longint;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].mid_incident_idnum
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
       end;

   function Tparticipant_incident_data_obj.get_ccode (pi_id : integer) : ccode_range;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].ccode
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
       end;

   function Tparticipant_incident_data_obj.get_cabbrev (pi_id : integer) : string;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].cabbrev
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
       end;

   function Tparticipant_incident_data_obj.get_StDay (pi_id : integer): MIDdaytype;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].StDay
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
       end;

   function Tparticipant_incident_data_obj.get_EndDay (pi_id : integer): MIDdaytype;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].EndDay
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
       end;

   function Tparticipant_incident_data_obj.get_StMonth (pi_id : integer): month_range;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].StMonth
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
       end;

   function Tparticipant_incident_data_obj.get_EndMonth (pi_id : integer): month_range;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].EndMonth
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
       end;

   function Tparticipant_incident_data_obj.get_StYear (pi_id : integer): year_range;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].StYear
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
       end;

   function Tparticipant_incident_data_obj.get_EndYear (pi_id : integer) : year_range;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].EndYear
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
       end;

   function Tparticipant_incident_data_obj.get_sideA (pi_id : integer) : boolean;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].sideA
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
       end;

   function Tparticipant_incident_data_obj.get_incident_sideA (pi_id : integer) : boolean;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].insideA
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
       end;

   function Tparticipant_incident_data_obj.get_Fatality (pi_id : integer): fatalitytype;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].Fatality
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
       end;

   function Tparticipant_incident_data_obj.get_fatalityv (pi_id : integer) : integer;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].fatalityv
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
       end;

   function Tparticipant_incident_data_obj.get_Action (pi_id : integer): ActionType;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].Action
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
       end;

   function Tparticipant_incident_data_obj.get_HostLev (pi_id : integer): hostlevtype;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].HostLev
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
           {Also, want to change coding as a "-9" hostility to a 1, which means
            no militarized response, although you were a target.}
          if (result = -9) then result := 1;
       end;

   function Tparticipant_incident_data_obj.get_issue1 (pi_id : integer) : integer;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].issue1
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
       end;

   function Tparticipant_incident_data_obj.get_issue2 (pi_id : integer) : integer;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].issue2
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
       end;

   function Tparticipant_incident_data_obj.get_Version (pi_id : integer): real;
       begin
          if (pi_id >= low(data)) and (pi_id <= high(data)) then
             result := data[pi_id].Version
          else EUGeneError ('Called get value from participant id data with out of range index.  Programming error - notify programmer.  Fatal error. ',1,stop,error_log);
       end;

        {  ------------------------------------------------  }


     {Now, general dyadic dispute object}


    function TGeneric_Dyadic_Conflict_data_obj.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then if created=true then initialized := true;
      end;

    procedure TGeneric_Dyadic_Conflict_data_obj.check_initialized;
          begin
            if not(initialized) then
               begin
                  EUGeneError ('Get a dyadic_dispute value get called before initialization',
                                  5, stop, error_log);
               end
          end;

    function TGeneric_Dyadic_Conflict_data_obj.get_first_partition_year : year_range;
      begin
         check_initialized;
         get_first_partition_year := first_partition_year;
      end;

    function TGeneric_Dyadic_Conflict_data_obj.get_last_partition_year : year_range;
      begin
         check_initialized;
         get_last_partition_year := last_partition_year;
      end;

{  --------------------------------------------    }

    {Now various functions that look at multiple conditional inputs
     These are written for the dyadic dispute object, because they apply to either the
     v2.1 format, or the 3.0 format.}

     function TCOWDyadic_dispute_data_obj.get_last_dispnum : longint;
      begin
         check_initialized;
         get_last_dispnum := num_dyadic_disputes - 1;
      end;

     function TCOWDyadic_dispute_data_obj.is_ongoing (ccode1, ccode2 : ccode_range; year : year_range;
                 var ongoing_num : longint) : boolean;
        {returns true if there is a dispute ongoing between cc1, cc2;
         even if 2 vs. 1.  SO, this is nondirected.  Returns -1 as # if no ongoing disp.}
        var disputenum : longint;
            done : boolean;
        begin
           check_initialized;
           is_ongoing := false;
           ongoing_num := null_dispute_number;
           disputenum := index^[ccode1, ccode2];
           done := false;
           if disputenum <> null_dispute_number then
              repeat
                {need to check all disputes starting with the 1st until another dyad is seen,
                  or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                  I've identified if any of them are ongoing at start of this year.}
                 if (get_first_year (disputenum) < year) and (get_last_year (disputenum) >= year) then
                    begin
                       is_ongoing := true;
                       ongoing_num := disputenum;
                    end;
                 inc(disputenum);
                 {this code codes the end condition sequentially in case complete boolean evaluation is on}
                 if (result=true) then done:= true
                    else if (disputenum > get_last_dispnum) then done := true
                    else if (get_first_year(disputenum) > year) then done := true
                    else if (get_ccode(disputenum, 0) <> ccode1) then done := true
                    else if (get_ccode(disputenum, 1) <> ccode2) then done := true;

              until done;
                 {At this point, have examined all records of cc1 vs cc2, and either found that there is
                  a dispute starting in year, or that I went through all the records and there was
                  not anytime a dispute started in year.  }

               {now, if not yet seen, check the reverse dyad to see if 1st year of 2 vs. 1}
           done := false;
           if ongoing_num = null_dispute_number then
           begin
              disputenum := index^[ccode2, ccode1];
              if disputenum <> null_dispute_number then
                 repeat
                    if (get_first_year (disputenum) < year) and (get_last_year (disputenum) >= year) then
                       begin
                          is_ongoing := true;
                          ongoing_num := disputenum;
                       end;
                    inc(disputenum);
                 if (result=true) then done:= true
                    else if (disputenum > get_last_dispnum) then done := true
                    else if (get_first_year(disputenum) > year) then done := true
                    else if (get_ccode(disputenum, 0) <> ccode2) then done := true
                    else if (get_ccode(disputenum, 1) <> ccode1) then done := true;
                 until done;
           end;

        end;

     function TCOWDyadic_dispute_data_obj.Is_1stYearOfDispute (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_disp_num : longint) : boolean;
        {returns true if this is the 1st year of a dispute between cc1, cc2;
         even if 2 vs. 1.  So, this is nondirected.}
        var disputenum : longint;
            none_found : boolean;
        begin
           check_initialized;
           Is_1stYearOfDispute := false;
           found_disp_num := null_dispute_number;
           disputenum := index^[ccode1, ccode2];
           if disputenum <> null_dispute_number then
              repeat
                {need to check all disputes starting with the 1st until another dyad is seen,
                  or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                  I've identified if any of them start in this year.}
                 if (get_first_year (disputenum) = year) then
                    begin
                       Is_1stYearOfDispute := true;
                       found_disp_num := disputenum;
                    end;
                 inc(disputenum);
              until ((result=true) or
                     (disputenum > get_last_dispnum) or
                     (get_first_year(disputenum) > year) or
                     (get_ccode(disputenum, 0) <> ccode1) or
                     (get_ccode(disputenum, 1) <> ccode2));
                 {At this point, have examined all records of cc1 vs cc2, and either found that there is
                  a dispute starting in year, or that I went through all the records and there was
                  not anytime a dispute started in year.  }

               {now, if not yet seen, check the reverse dyad to see if 1st year of 2 vs. 1}
           if found_disp_num = null_dispute_number then
           begin
              disputenum := index^[ccode2, ccode1];
              if disputenum <> null_dispute_number then
                 repeat
                   {need to check all disputes starting with the 1st until another dyad is seen,
                     or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                     I've identified if any of them start in this year.}
                    if (get_first_year (disputenum) = year) then
                       begin
                          Is_1stYearOfDispute := true;
                          found_disp_num := disputenum;
                       end;
                    inc(disputenum);
                 until ((result=true) or
                        (disputenum > get_last_dispnum) or
                        (get_first_year(disputenum) > year) or
                        (get_ccode(disputenum, 0) <> ccode2) or
                        (get_ccode(disputenum, 1) <> ccode1));
           end;
        end;

     function TCOWDyadic_dispute_data_obj.Is_sideA (ccode1, ccode2 : ccode_range; year : year_range) : boolean;
        {returns true if ccode1 is on side A of a dispute against ccode2 this year, even if ongoing.}
        var disputenum : longint;
        begin
           check_initialized;
           Is_sideA := false;
           disputenum := index^[ccode1, ccode2];
           if disputenum <> null_dispute_number then
              repeat
                {need to check all disputes starting with the 1st until another dyad is seen,
                  or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                  I've identified if cc1 is on side A in any of them in this year.}
                 if (get_sideA (disputenum, ccode1) = true) and
                    ((get_first_year(disputenum) <= year) and
                     (get_last_year(disputenum) >= year)) then
                     Is_sideA := true;
                 inc(disputenum);
              until ((result=true) or
                     (disputenum > get_last_dispnum) or
                     (get_first_year(disputenum) > year) or
                     (get_ccode(disputenum, 0) <> ccode1) or
                     (get_ccode(disputenum, 1) <> ccode2));
                 {At this point, have examined all records of cc1 vs cc2, and either found that there is
                  a dispute starting in year, or that I went through all the records and there was
                  not anytime a dispute started in year.  }

                 {Also need to check disputes of cc2 vs. cc1}
           if result <> true then
           begin
              disputenum := index^[ccode2, ccode1];
              if disputenum <> null_dispute_number then
                 repeat
                   {need to check all disputes starting with the 1st until another dyad is seen,
                     or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                     I've identified if cc1 is on side A in any of them in this year.}
                    if (get_sideA (disputenum, ccode1) = true) and
                       ((get_first_year(disputenum) <= year) and
                        (get_last_year(disputenum) >= year)) then
                          Is_sideA := true;
                    inc(disputenum);
                 until ((result=true) or
                        (disputenum > get_last_dispnum) or
                        (get_first_year(disputenum) > year) or
                        (get_ccode(disputenum, 0) <> ccode2) or
                        (get_ccode(disputenum, 1) <> ccode1));
           end;
        end;

     function TCOWDyadic_dispute_data_obj.Is_originator (ccode1, ccode2 : ccode_range; year : year_range) : boolean;
        {returns true if ccode1 is originator in a dispute against ccode2 this year.
         This does not say this isi the origination year, but 1 was an originator in a
         disp that is either new or ongoing in this year.  This is non-directed.}
        var disputenum : longint;
        begin
           check_initialized;
           Is_originator := false;
           disputenum := index^[ccode1, ccode2];
           if disputenum <> null_dispute_number then
              repeat
                 if (get_originator (disputenum, ccode1)) and
                    ((get_first_year(disputenum) <= year) and
                     (get_last_year(disputenum) >= year)) then
                     Is_originator := true;
                 inc(disputenum);
              until ((result=true) or
                     (disputenum > get_last_dispnum) or
                     (get_first_year(disputenum) > year) or
                     (get_ccode(disputenum, 0) <> ccode1) or
                     (get_ccode(disputenum, 1) <> ccode2));

              {also check the reverse dyad, if I havent' id'd the originator already.}
           if result = false then
              begin
                 disputenum := index^[ccode2, ccode1];
                 if disputenum <> null_dispute_number then
                    repeat
                       if (get_originator (disputenum, ccode1)) and
                          ((get_first_year(disputenum) <= year) and
                           (get_last_year(disputenum) >= year)) then
                          Is_originator := true;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_first_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode2) or
                           (get_ccode(disputenum, 1) <> ccode1));
              end;
        end;

     function TCOWDyadic_dispute_data_obj.Is_1stYear_Initiation (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 and 2 are originators,
         and cc1 is on side A in a dispute against ccode2 this year,
         and this is the first year of the dispute.  }
        {This marks what we consider a new true initiation by cc1 vs. cc2}
        {This works if called either with a disp# or a ccode-year}
        var disputenum : longint;
        begin
           check_initialized;
           Is_1stYear_Initiation := false;
           if use_param = use_dispute then
              begin
                 if ((get_first_year (found_dispute_num) = year) and
                     (get_originator (found_dispute_num, ccode1)) and
                     (get_originator (found_dispute_num, ccode2)) and
                     (get_sideA (found_dispute_num, ccode1) = true) ) then
                     Is_1stYear_Initiation := true;
              end
           else     {use ccode-year}
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are true initiations.}
                       {This will stop and identify/return the dispute number of
                         the first initiation between A and B in this year}
                       if ((get_first_year (disputenum) = year) and
                           (get_originator (disputenum, ccode1)) and
                           (get_originator (disputenum, ccode2)) and
                           (get_sideA (disputenum, ccode1) = true) ) then  {sideA is a 1}
                          begin
                             Is_1stYear_Initiation := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_first_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
                       {At this point, have examined all records of cc1 vs cc2, and either found that there is
                        a dispute starting in year, or that I went through all the records and there was
                        not anytime a dispute started in year.  }
              end;
        end;

     function TCOWDyadic_dispute_data_obj.Is_1stYear_Joined_Initiation (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is on initiating side vs. cc2, and this is the 1st year that
         cc1 joined the dispute, BUT either cc1 or cc2 is not an originator.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_1stYear_Joined_Initiation := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_first_year (found_dispute_num) = year) and
                     (not (get_originator (found_dispute_num, ccode1) and
                           get_originator (found_dispute_num, ccode2) ) ) and
                     (get_sideA (found_dispute_num, ccode1) = true) )  then
                       Is_1stYear_Joined_Initiation := true;
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                       if ((get_first_year (disputenum) = year) and
                           (not (get_originator (disputenum, ccode1) and
                                 get_originator (disputenum, ccode2) ) ) and
                           (get_sideA (disputenum, ccode1) = true) ) then
                          begin
                             Is_1stYear_Joined_Initiation := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_first_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
              end;
        end;

     function TCOWDyadic_dispute_data_obj.Is_1stYear_Joined_Targets (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is on target side vs. cc2, and this is the 1st year that
         cc1 joined the dispute, BUT either cc1 or cc2 is not an originator.
         To do this by the ccode-year method, will need to access the
         ccode2 vs. ccode1 dyad, because only one direction
         of most dyads was created.  To see if someone is a target, will need to look
         at the other country first, since we want to check dyads where it is init side.
         TO see if CC1 is a target, then will need to look at dyads of cc2 vs. cc1.  }
        var disputenum : longint;
        begin
           check_initialized;
           result := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_first_year (found_dispute_num) = year) and
                     (not (get_originator (found_dispute_num, ccode1) and
                           get_originator (found_dispute_num, ccode2) ) ) and
                     (get_sideA (found_dispute_num, ccode1) = false) ) then
                       result := true;
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 {start index at first of ccode2 vs. ccode1 dyads/disputes}
                 disputenum := index^[ccode2, ccode1];
                 if disputenum <> null_dispute_number then
                    repeat
                       if ((get_first_year (disputenum) = year) and
                           (not (get_originator (disputenum, ccode1) and
                                 get_originator (disputenum, ccode2) ) ) and
                           (get_sideA (disputenum, ccode1) = false) ) then
                          begin
                             result := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_first_year(disputenum) > year) or
                    {need to reverse ccode2, ccode1 coding here b/c searching in
                     reversed dyad, b/c looking at targets.}
                           (get_ccode(disputenum, 0) <> ccode2) or
                           (get_ccode(disputenum, 1) <> ccode1));
              end;
        end;

     function TCOWDyadic_dispute_data_obj.Is_AnyYear_Initiation (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 and 2 are originators, cc1 initiated vs. cc2,
         BUT this may not be the first year of the dispute.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_AnyYear_Initiation := false;
           if use_param = use_dispute then
              begin
                 if ((get_first_year (found_dispute_num) <= year) and
                     (get_last_year (found_dispute_num) >= year) and
                     (get_originator (found_dispute_num, ccode1)) and
                     (get_originator (found_dispute_num, ccode2)) and
                     (get_sideA (found_dispute_num, ccode1) = true) ) then
                 Is_AnyYear_Initiation := true;
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                       if ((get_first_year (disputenum) <= year) and
                           (get_last_year (disputenum) >= year) and
                           (get_originator (disputenum, ccode1)) and
                           (get_originator (disputenum, ccode2)) and
                           (get_sideA (disputenum, ccode1) = true) ) then
                          begin
                             Is_AnyYear_Initiation := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_first_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
                       {At this point, have examined all records of cc1 vs cc2, and either found that there is
                        a dispute in year where 1 was initiator vs. 2,
                        or that I went through all the records and there was not a dispute in year.}
              end;
        end;

     function TCOWDyadic_dispute_data_obj.Is_AnyYear_Joined_Initiation (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is on initiating side vs. cc2, and this is ANY year of
         the dispute, BUT either cc1 or cc2 is not an originator.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_AnyYear_Joined_Initiation := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_first_year (found_dispute_num) <= year) and
                     (get_last_year (found_dispute_num) >= year) and
                     (not (get_originator (found_dispute_num, ccode1) and
                           get_originator (found_dispute_num, ccode2)   )) and
                     (get_sideA (found_dispute_num, ccode1) = true) ) then
                   Is_AnyYear_Joined_Initiation := true;
                end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                       if ((get_first_year (disputenum) <= year) and
                           (get_last_year (disputenum) >= year) and
                           (not (get_originator (disputenum, ccode1) and
                                 get_originator (disputenum, ccode2)   )) and
                           (get_sideA (disputenum, ccode1) = true) ) then
                          begin
                             Is_AnyYear_Joined_Initiation := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_first_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
              end;
        end;

     function TCOWDyadic_dispute_data_obj.Is_AnyYear_Joined_Targets (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is on target side vs. cc2, and this is ANY year of
         the dispute, BUT either cc1 or cc2 is not an originator.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_AnyYear_Joined_Targets := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_first_year (found_dispute_num) <= year) and
                     (get_last_year (found_dispute_num) >= year) and
                     (not (get_originator (found_dispute_num, ccode1) and
                           get_originator (found_dispute_num, ccode2)   )) and
                     (get_sideA (found_dispute_num, ccode1) = false) ) then
                   Is_AnyYear_Joined_Targets := true;
                end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 {start index at first of ccode2 vs. ccode1 dyads/disputes because
                  looking for joining the target}
                 disputenum := index^[ccode2, ccode1];
                 if disputenum <> null_dispute_number then
                    repeat
                       if ((get_first_year (disputenum) <= year) and
                           (get_last_year (disputenum) >= year) and
                           (not (get_originator (disputenum, ccode1) and
                                 get_originator (disputenum, ccode2)   )) and
                           (get_sideA (disputenum, ccode1) = false) ) then
                          begin
                             Is_AnyYear_Joined_Targets := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_first_year(disputenum) > year) or
                    {need to reverse ccode2, ccode1 coding here b/c searching in
                     reversed dyad, b/c looking at targets}
                           (get_ccode(disputenum, 0) <> ccode2) or
                           (get_ccode(disputenum, 1) <> ccode1));
              end;
        end;

    function TCOWDyadic_dispute_data_obj.Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2 : ccode_range; year : year_range;
              var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 and 2 are originators, in a dispute that started this year.
         Don't care about who is side A or B.  }
        {This marks what we consider a new true, non-directed initiation between cc1 and cc2}
        {This works if called either with a disp# or a ccode-year.}
        var disputenum : longint;
        begin
           check_initialized;
           Is_1stYear_NonDir_Dispute_Originators := false;
           if use_param = use_dispute then
              begin
                 if ((get_first_year (found_dispute_num) = year) and
                     (get_originator (found_dispute_num, ccode1)) and
                     (get_originator (found_dispute_num, ccode2)) ) then
                     Is_1stYear_NonDir_Dispute_Originators := true;
              end
           else     {use ccode-year}
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are initiations.}
                       {This will stop and identify/return the dispute number of
                         the first dispute between A and B in this year}
                       if ((get_first_year (disputenum) = year) and
                           (get_originator (disputenum, ccode1)) and
                           (get_originator (disputenum, ccode2))) then
                          begin
                             Is_1stYear_NonDir_Dispute_Originators := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_first_year(disputenum) > year) or
                           {CODE error found here 10/19/03, had 2nd get_ccode using a 0 not
                            a 1 call param, so it was checking 1st dyad member 2x.}
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
                       {At this point, have examined all records of cc1 vs cc2, and either found that there is
                        a dispute starting in year, or that I went through all the records and there was
                        not anytime a dispute started in year.
                        Also need to check the reverse direction, though.}
                 if result = false then
                    begin
                       disputenum := index^[ccode2, ccode1];
                       if disputenum <> null_dispute_number then
                          repeat
                             if ((get_first_year (disputenum) = year) and
                                 (get_originator (disputenum, ccode1)) and
                                 (get_originator (disputenum, ccode2))) then
                                begin
                                   Is_1stYear_NonDir_Dispute_Originators := true;
                                   found_dispute_num := disputenum;
                                end;
                             inc(disputenum);
                          until ((result=true) or
                                 (disputenum > get_last_dispnum) or
                                 (get_first_year(disputenum) > year) or
                                 (get_ccode(disputenum, 0) <> ccode2) or
                                 (get_ccode(disputenum, 1) <> ccode1));
                    end;
              end;
        end;

    function TCOWDyadic_dispute_data_obj.Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2 : ccode_range; year : year_range;
              var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 and cc2 are in a dispute,
         but at least one is not an originator,
         and this is the 1st year of the dispute.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_1stYear_NonDir_Dispute_Joiners := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_first_year (found_dispute_num) = year) and
                     (not (get_originator (found_dispute_num, ccode1) and
                           get_originator (found_dispute_num, ccode2)))  ) then
                       Is_1stYear_NonDir_Dispute_Joiners := true;
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                       if ((get_first_year (disputenum) = year) and
                           (not (get_originator (disputenum, ccode1) and
                                 get_originator (disputenum, ccode2))))  then
                          begin
                             Is_1stYear_NonDir_Dispute_Joiners := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_first_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
                    {check reverse dyad}
                 if result = false then
                    begin
                       disputenum := index^[ccode2, ccode1];
                       if disputenum <> null_dispute_number then
                          repeat
                             if ((get_first_year (disputenum) = year) and
                                 (not (get_originator (disputenum, ccode1) and
                                       get_originator (disputenum, ccode2))))  then
                                begin
                                   Is_1stYear_NonDir_Dispute_Joiners := true;
                                   found_dispute_num := disputenum;
                                end;
                             inc(disputenum);
                          until ((result=true) or
                                 (disputenum > get_last_dispnum) or
                                 (get_first_year(disputenum) > year) or
                                 (get_ccode(disputenum, 0) <> ccode2) or
                                 (get_ccode(disputenum, 1) <> ccode1));
                    end;
              end;
        end;

    function TCOWDyadic_dispute_data_obj.Is_AnyYear_NonDir_Dispute_Originators (ccode1, ccode2 : ccode_range; year : year_range;
              var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 and 2 are originators, in a dispute that started this year.
         Don't care about who is side A or B.  }
        {This marks what we consider a new true, non-directed initiation between cc1 and cc2}
        {This works if called either with a disp# or a ccode-year}
        var disputenum : longint;
        begin
           check_initialized;
           Is_AnyYear_NonDir_Dispute_Originators := false;
           if use_param = use_dispute then
              begin
                 if ((get_first_year (found_dispute_num) <= year) and
                     (get_last_year (found_dispute_num) >= year) and
                     (get_originator (found_dispute_num, ccode1)) and
                     (get_originator (found_dispute_num, ccode2)) ) then
                     Is_AnyYear_NonDir_Dispute_Originators := true;
              end
           else     {use ccode-year}
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are initiations.}
                       {This will stop and identify/return the dispute number of
                         the first dispute between A and B in this year}
                       if ((get_first_year (disputenum) <= year) and
                           (get_last_year (disputenum) >= year) and
                           (get_originator (disputenum, ccode1)) and
                           (get_originator (disputenum, ccode2))) then
                          begin
                             Is_AnyYear_NonDir_Dispute_Originators := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_first_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
                       {At this point, have examined all records of cc1 vs cc2, and either found that there is
                        a dispute starting in year, or that I went through all the records and there was
                        not anytime a dispute started in year.  }
                 if result = false then
                    begin
                       disputenum := index^[ccode2, ccode1];
                       if disputenum <> null_dispute_number then
                          repeat
                             if ((get_first_year (disputenum) <= year) and
                                 (get_last_year (disputenum) >= year) and
                                 (get_originator (disputenum, ccode1)) and
                                 (get_originator (disputenum, ccode2))) then
                                begin
                                   Is_AnyYear_NonDir_Dispute_Originators := true;
                                   found_dispute_num := disputenum;
                                end;
                             inc(disputenum);
                          until ((result=true) or
                                 (disputenum > get_last_dispnum) or
                                 (get_first_year(disputenum) > year) or
                                 (get_ccode(disputenum, 0) <> ccode2) or
                                 (get_ccode(disputenum, 1) <> ccode1));
                    end;
              end;
        end;

    function TCOWDyadic_dispute_data_obj.Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2 : ccode_range; year : year_range;
              var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 and cc2 in dispute, and this is ANY year of
         the dispute, BUT at least one is not an originator.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_AnyYear_NonDir_Dispute_Joiners := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_first_year (found_dispute_num) <= year) and
                     (get_last_year (found_dispute_num) >= year) and
                     (not (get_originator (found_dispute_num, ccode1) and
                           get_originator (found_dispute_num, ccode2)))) then
                   Is_AnyYear_NonDir_Dispute_Joiners := true;
                end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                       if ((get_first_year (disputenum) <= year) and
                           (get_last_year (disputenum) >= year) and
                           (not (get_originator (disputenum, ccode1) and
                                 get_originator (disputenum, ccode2)))) then
                          begin
                             Is_AnyYear_NonDir_Dispute_Joiners := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_first_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
                 if result = false then
                    begin
                       disputenum := index^[ccode2, ccode1];
                       if disputenum <> null_dispute_number then
                          repeat
                             if ((get_first_year (disputenum) <= year) and
                                 (get_last_year (disputenum) >= year) and
                                 (not (get_originator (disputenum, ccode1) and
                                       get_originator (disputenum, ccode2)))) then
                                begin
                                   Is_AnyYear_NonDir_Dispute_Joiners := true;
                                   found_dispute_num := disputenum;
                                end;
                             inc(disputenum);
                          until ((result=true) or
                                 (disputenum > get_last_dispnum) or
                                 (get_first_year(disputenum) > year) or
                                 (get_ccode(disputenum, 0) <> ccode2) or
                                 (get_ccode(disputenum, 1) <> ccode1));
                    end;
              end;
        end;


    function TCOWDyadic_dispute_data_obj.Is_1stYear_Revision (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is coded revisionist in a dispute against ccode2 this year,
         and this is the first year of the dispute.  }
        {This marks what we consider a new true initiation, but using revisionist, not side A}
        var disputenum : longint;
        begin
           check_initialized;
           Is_1stYear_Revision := false;
           if use_param = use_dispute then
              begin
                  if ((get_first_year (found_dispute_num) = year) and
                      (get_originator (found_dispute_num, ccode1)) and
                      (get_originator (found_dispute_num, ccode2)) and
                      (get_revisionist (found_dispute_num, ccode1) = true))  then
                  Is_1stYear_Revision := true;
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are true initiations.}
                       if ((get_first_year (disputenum) = year) and
                           (get_originator (disputenum, ccode1)) and
                           (get_originator (disputenum, ccode2)) and
                           (get_revisionist (disputenum, ccode1) = true))  then
                          begin
                             Is_1stYear_Revision := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_first_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
              end;
        end;

    function TCOWDyadic_dispute_data_obj.Is_1stYear_Joined_Revision (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is coded revisionist in a dispute against ccode2 this year,
         and this is the first year of the dispute, but ccode1 or ccode2 is a joiner.}
        var disputenum : longint;
        begin
           check_initialized;
           Is_1stYear_Joined_Revision := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_first_year (found_dispute_num) = year) and
                     (not (get_originator (found_dispute_num, ccode1) and
                           get_originator (found_dispute_num, ccode2))) and
                     (get_revisionist (found_dispute_num, ccode1) = true) ) then
                 Is_1stYear_Joined_Revision := true;
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are true initiations.}
                       if ((get_first_year (disputenum) = year) and
                           (not (get_originator (disputenum, ccode1) and
                                 get_originator (disputenum, ccode2) )) and
                           (get_revisionist (disputenum, ccode1) = true) ) then
                          begin
                             Is_1stYear_Joined_Revision := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_first_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
              end;
        end;

    function TCOWDyadic_dispute_data_obj.Is_1stYear_Joined_SQ (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is coded non-revisionist in a dispute against ccode2 this year,
         and this is the first year of the dispute.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_1stYear_Joined_SQ := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_first_year (found_dispute_num) = year) and
                     (not (get_originator (found_dispute_num, ccode1) and
                           get_originator (found_dispute_num, ccode2) )) and
                     (not (get_revisionist (found_dispute_num, ccode1) = true)) ) then
                 Is_1stYear_Joined_SQ := true;
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 {start index at first of ccode2 vs. ccode1 dyads/disputes because
                  looking for joining the target}
                 disputenum := index^[ccode2, ccode1];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are true initiations.}
                       if ((get_first_year (disputenum) = year) and
                           (not (get_originator (disputenum, ccode1) and
                                 get_originator (disputenum, ccode2) )) and
                           (not (get_revisionist (disputenum, ccode1) = true)) ) then
                          begin
                             Is_1stYear_Joined_SQ := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_first_year(disputenum) > year) or
                    {need to reverse ccode2, ccode1 coding here b/c searching in
                     reversed dyad, b/c looking at targets}
                           (get_ccode(disputenum, 0) <> ccode2) or
                           (get_ccode(disputenum, 1) <> ccode1));
              end;
        end;

    function TCOWDyadic_dispute_data_obj.Is_AnyYear_Revision (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is coded revisionist in a dispute against ccode2 this year,
         But this is not necessarily the first year of the dispute.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_AnyYear_Revision := false;
           if use_param = use_dispute then
              begin
                 if ((get_first_year (found_dispute_num) <= year) and
                     (get_last_year (found_dispute_num) >= year) and
                     (get_originator (found_dispute_num, ccode1)) and
                     (get_originator (found_dispute_num, ccode2)) and
                     (get_revisionist (found_dispute_num, ccode1) = true))  then
                  Is_AnyYear_Revision := true;
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are true initiations.}
                       if ((get_first_year (disputenum) <= year) and
                           (get_last_year (disputenum) >= year) and
                           (get_originator (disputenum, ccode1)) and
                           (get_originator (disputenum, ccode2)) and
                           (get_revisionist (disputenum, ccode1) = true))  then
                          begin
                             Is_AnyYear_Revision := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_first_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
              end;
        end;

    function TCOWDyadic_dispute_data_obj.Is_AnyYear_Joined_Revision (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is coded revisionist in a dispute against ccode2 this year,
         and this is any year of the dispute.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_AnyYear_Joined_Revision := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_first_year (found_dispute_num) <= year) and
                     (get_last_year (found_dispute_num) >= year) and
                     (not(get_originator (found_dispute_num, ccode1) and
                          get_originator (found_dispute_num, ccode2))) and
                     (get_revisionist (found_dispute_num, ccode1) = true) ) then
                   Is_AnyYear_Joined_Revision := true;
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are true initiations.}
                       if ((get_first_year (disputenum) <= year) and
                           (get_last_year (disputenum) >= year) and
                           (not(get_originator (disputenum, ccode1) and
                                get_originator (disputenum, ccode2))) and
                           (get_revisionist (disputenum, ccode1) = true) ) then
                          begin
                             Is_AnyYear_Joined_Revision := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_first_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
              end;
        end;

    function TCOWDyadic_dispute_data_obj.Is_AnyYear_Joined_SQ (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is coded non-revisionist in a dispute against ccode2 this year,
         and this is any year of the dispute.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_AnyYear_Joined_SQ := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_first_year (found_dispute_num) <= year) and
                     (get_last_year (found_dispute_num) >= year) and
                     (not(get_originator (found_dispute_num, ccode1) and
                          get_originator (found_dispute_num, ccode2))) and
                     (not(get_revisionist (found_dispute_num, ccode1) = true)) ) then
                   Is_AnyYear_Joined_SQ := true;
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 {start index at first of ccode2 vs. ccode1 dyads/disputes because
                  looking for joining the target}
                 disputenum := index^[ccode2, ccode1];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are true initiations.}
                       if ((get_first_year (disputenum) <= year) and
                           (get_last_year (disputenum) >= year) and
                           (not(get_originator (disputenum, ccode1) and
                                get_originator (disputenum, ccode2))) and
                           (not(get_revisionist (disputenum, ccode1) = true)) ) then
                          begin
                             Is_AnyYear_Joined_SQ := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_first_year(disputenum) > year) or
                    {need to reverse ccode2, ccode1 coding here b/c searching in
                     reversed dyad, b/c looking at targets}
                           (get_ccode(disputenum, 0) <> ccode2) or
                           (get_ccode(disputenum, 1) <> ccode1));
              end;
        end;



     function TCOWDyadic_dispute_data_obj.wanted_new_initiation (const ccode1, ccode2 : ccode_range;
                           const year : year_range; const user_selections : user_selection_type;
                           var dispnum : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if there is a new directed initiation of a type I want in this year,
         meaningn checking sideA vs. Revisionist, and originator or joiner.
         Used by procedured to check printing a dyad year iff a new dispute}
         begin
            check_initialized;
            if user_selections.dispute_info.OnlyTrueInitiators then
               begin   {originators only, not joiners}
                 if user_selections.dispute_info.SideAIsInitiator then  {want originators, sideA}
                     wanted_new_initiation := Is_1stYear_Initiation (ccode1, ccode2, year, dispnum, use_param)
                   else     {originators, Revisionist}
                     wanted_new_initiation := Is_1stYear_Revision (ccode1, ccode2, year, dispnum, use_param);
                 end
              else    {want originators or joiners on initiating side}
            if user_selections.dispute_info.JoinersOnInitiatingSideAsInitiators then
                begin
                   if user_selections.dispute_info.SideAIsInitiator then   {want originators or joiners, sideA}
                       wanted_new_initiation :=
                          (Is_1stYear_Initiation (ccode1, ccode2, year, dispnum, use_param)
                           or Is_1stYear_Joined_Initiation (ccode1, ccode2, year, dispnum, use_param))
                   else   {want originators or joiners on init side, Revisionist}
                       wanted_new_initiation :=
                              (Is_1stYear_Revision (ccode1, ccode2, year, dispnum, use_param)
                               or Is_1stYear_Joined_Revision (ccode1, ccode2, year, dispnum, use_param));
                 end
                else     {want originators or any kind of joiner marked}
            if user_selections.dispute_info.AllJoinersAsInitiators then
                begin
                   {if a initiated vs. B or
                    a is on side A, and either A or B was a joiner; or
                    a is on side B, and either A or B was a joiner.}
                   if user_selections.dispute_info.SideAIsInitiator then   {want originators or joiners, sideA}
                       wanted_new_initiation :=
                          (Is_1stYear_Initiation (ccode1, ccode2, year, dispnum, use_param)
                           or Is_1stYear_Joined_Initiation (ccode1, ccode2, year, dispnum, use_param)
                           or Is_1stYear_Joined_Targets (ccode1, ccode2, year, dispnum, use_param))
                   else   {want originators or any joiners, Revisionist}
                       wanted_new_initiation :=
                              (Is_1stYear_Revision (ccode1, ccode2, year, dispnum, use_param)
                               or Is_1stYear_Joined_Revision (ccode1, ccode2, year, dispnum, use_param)
                               or Is_1stYear_Joined_SQ (ccode1, ccode2, year, dispnum, use_param));
                 end
            else {should never get here!}
               EUGeneError ('Did not have an appropriate initiator identifier in wanted_new_or_ongoing_initiation',
                               5, stop, error_log);

         end;     {function wanted_new_initiation}


     function TCOWDyadic_dispute_data_obj.wanted_new_or_continuing_initiation (const ccode1, ccode2 : ccode_range;
                 const year : year_range; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if there is a new initiation or a continuing dispute,
         of a type I want in this year,
         meaning checking sideA vs. Revisionist, and originator or joiner.
         Used by procedured to check printing a dyad year iff a new dispute}
         begin
            check_initialized;
            if user_selections.dispute_info.OnlyTrueInitiators then   {originators only, not joiners}
               begin
                 if user_selections.dispute_info.SideAIsInitiator then    {originators, sideA}
                       wanted_new_or_continuing_initiation := Is_AnyYear_Initiation (ccode1, ccode2, year, dispnum, use_param)
                   else     {originators, Revisionist}
                       wanted_new_or_continuing_initiation := Is_AnyYear_Revision (ccode1, ccode2, year, dispnum, use_param);
               end   {sideA is initiator}
            else     {want originators or init side joiners}
            if user_selections.dispute_info.JoinersOnInitiatingSideAsInitiators then
               begin
                 if user_selections.dispute_info.SideAIsInitiator then   {originators or joiners, sideA}
                     wanted_new_or_continuing_initiation :=
                              (Is_AnyYear_Initiation (ccode1, ccode2, year, dispnum, use_param)
                               or Is_AnyYear_Joined_Initiation (ccode1, ccode2, year, dispnum, use_param))
                     else   {originators or joiners, Revisionist}
                        wanted_new_or_continuing_initiation :=
                               (Is_AnyYear_Revision (ccode1, ccode2, year, dispnum, use_param)
                                or Is_AnyYear_Joined_Revision (ccode1, ccode2, year, dispnum, use_param));
               end
            else     {want originators or any joiners}
            if user_selections.dispute_info.AllJoinersAsInitiators then
                begin
                 if user_selections.dispute_info.SideAIsInitiator then   {originators or joiners, sideA}
                     wanted_new_or_continuing_initiation :=
                              (Is_AnyYear_Initiation (ccode1, ccode2, year, dispnum, use_param)
                               or Is_AnyYear_Joined_Initiation (ccode1, ccode2, year, dispnum, use_param)
                               or Is_AnyYear_Joined_Targets (ccode1, ccode2, year, dispnum, use_param))
                     else   {originators or joiners, Revisionist}
                        wanted_new_or_continuing_initiation :=
                               (Is_AnyYear_Revision (ccode1, ccode2, year, dispnum, use_param)
                                or Is_AnyYear_Joined_Revision (ccode1, ccode2, year, dispnum, use_param)
                                or Is_AnyYear_Joined_SQ (ccode1, ccode2, year, dispnum, use_param));
               end
            else {should never get here!}
               EUGeneError ('Did not have an appropriate initiator identifier in wanted_new_or_ongoing_initiation',
                               5, stop, error_log);

         end;     {function wanted_new_or_continuing_initiation}


     function TCOWDyadic_dispute_data_obj.wanted_new_NonDir_dispute (const ccode1, ccode2 : ccode_range;
         const year : year_range; const user_selections : user_selection_type;
         var dispnum : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if there is a new non-directed dispute of a type I want in this year,
         meaningn checking originator or joiner.
         Used by procedured to check printing a dyad year iff a new dispute}
         begin
            check_initialized;
            if user_selections.dispute_info.OnlyTrueInitiators then
                {originators only, not joiners}
                  wanted_new_NonDir_dispute := Is_1stYear_NonDir_dispute_originators(ccode1, ccode2, year, dispnum, use_param)
              else    {want originators or joiners }
            if user_selections.dispute_info.AllJoinersAsInitiators then
                   wanted_new_NonDir_dispute := (Is_1stYear_NonDir_dispute_Joiners(ccode1, ccode2, year, dispnum, use_param) or
                       Is_1stYear_NonDir_dispute_originators(ccode1, ccode2, year, dispnum, use_param))
            else {should never get here!}
               EUGeneError ('Did not have an appropriate initiator identifier in wanted_new_nondir_disptue',
                               5, stop, error_log);
         end;     {function wanted_new_dispute}

     function TCOWDyadic_dispute_data_obj.wanted_new_or_Continuing_NonDir_dispute (const ccode1, ccode2 : ccode_range;
                           const year : year_range; const user_selections : user_selection_type;
                           var dispnum : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if there is a new or continuing non-directed dispute of a type I want
         in this year, meaning checking originator or joiner.
         Used by procedures to check printing a dyad year iff a new dispute}
         begin
            check_initialized;
            if user_selections.dispute_info.OnlyTrueInitiators then
                {originators only, not joiners}
                  wanted_new_or_Continuing_NonDir_dispute := Is_AnyYear_NonDir_dispute_originators(ccode1, ccode2, year, dispnum, use_param)
              else    {want originators or joiners }
            if user_selections.dispute_info.AllJoinersAsInitiators then
                   wanted_new_or_Continuing_NonDir_dispute := (Is_AnyYear_NonDir_dispute_Joiners(ccode1, ccode2, year, dispnum, use_param) or
                       Is_AnyYear_NonDir_dispute_originators(ccode1, ccode2, year, dispnum, use_param))
            else {should never get here!}
               EUGeneError ('Did not have an appropriate initiator identifier in wanted_new_nondir_disptue',
                               5, stop, error_log);
         end;     {function wanted_new_dispute}


     function TCOWDyadic_dispute_data_obj.relevant_midnum (const ccode1, ccode2 : ccode_range; const year : year_range;
                 const user_selections : user_selection_type) : longint;
        {returns # of relevant dyadic MidNum for dyad-year.
         0 if no dispute.  For cc1 in dispute with cc2 in year}

        {For directed dyads, rule for where to get the hostility level is as follows.
        {If choose first MID:
            Takes info on the first origination (initiation or first revision, if revisionist selected).
            If no new origination, then first event, including joining.
            If no origination or joining, takes info on ongoing.
        If choose highest hostility level:
            Takes info on MID with highest hostility level for 1, whether origination, joining,
               or ongoing.  If multiple MIDs with same hostility level, takes new initiation first,
               then joining, then ongoing.  Within those categories, takes first date.


        {for nondirected, if choose first MID:
           first see if any new dispute originator this year.
           If multiple, take the first one.
           2nd, take any dispute including joiner.
           3rd, check ongoing disutes, first to see if both originators of an
           ongoing dispute.
           4th, If not ongoing originators, then check ongoing joiners.

         If choose highest host, take the dispute with the highest host for ccode1 *or* 2.}

        var found_dispute_num, function_dispnum, disputenum12, disputenum21 : longint;
            currdisp, currhighestdisp  : longint;
            function_1storigination, function_1stjoining, function_anyorigination, function_anyjoining : longint;
            currhigh1storigination, currhigh1stjoining, currhighanyorigination, currhighanyjoining: boolean;
            curr1storigination, curr1stjoining, curranyorigination, curranyjoining: boolean;
            currhighesthost : hostlevtype;
            {Two helper functions to sequentially set highest hostility dispute #s.}
         procedure set_current_highs (ccode1, ccode2 : ccode_range; year : year_range; currdisp : longint);
            begin
              currhigh1storigination :=
                 ( (user_selections.dispute_info.SideAIsInitiator and
                    Is_1stYear_Initiation (ccode1, ccode2, year, currdisp, use_dispute))
                  or (user_selections.dispute_info.SideAIsInitiator and
                      Is_1stYear_Revision (ccode1, ccode2, year, currdisp, use_dispute)) );
              currhigh1stjoining :=
                 ( (user_selections.dispute_info.SideAIsInitiator and
                    (Is_1stYear_Joined_Initiation (ccode1, ccode2, year, currdisp, use_dispute) or
                     Is_1styear_joined_targets (ccode1, ccode2, year, currdisp, use_dispute)) )
                  or (user_selections.dispute_info.SideAIsInitiator and
                     (Is_1stYear_Joined_Revision (ccode1, ccode2, year, currdisp, use_dispute) or
                      Is_1stYear_Joined_SQ (ccode1, ccode2, year, currdisp, use_dispute)) ) );
              currhighanyorigination :=
                 ( (user_selections.dispute_info.SideAIsInitiator and
                    Is_AnyYear_Initiation (ccode1, ccode2, year, currdisp, use_dispute))
                  or (user_selections.dispute_info.SideAIsInitiator and
                      Is_AnyYear_Revision (ccode1, ccode2, year, currdisp, use_dispute)) );
              currhighanyjoining :=
                 ( (user_selections.dispute_info.SideAIsInitiator and
                    (Is_AnyYear_Joined_Initiation (ccode1, ccode2, year, currdisp, use_dispute) or
                     Is_AnyYear_Joined_targets (ccode1, ccode2, year, currdisp, use_dispute)) )
                  or (user_selections.dispute_info.SideAIsInitiator and
                     (Is_AnyYear_Joined_Revision (ccode1, ccode2, year, currdisp, use_dispute) or
                      Is_AnyYear_Joined_SQ (ccode1, ccode2, year, currdisp, use_dispute)) ) );
            end;
         procedure set_current (ccode1, ccode2 : ccode_range; year : year_range; currdisp : longint);
            begin
              curr1storigination :=
                 ( (user_selections.dispute_info.SideAIsInitiator and
                    Is_1stYear_Initiation (ccode1, ccode2, year, currdisp, use_dispute))
                  or (user_selections.dispute_info.SideAIsInitiator and
                      Is_1stYear_Revision (ccode1, ccode2, year, currdisp, use_dispute)) );
              curr1stjoining :=
                 ( (user_selections.dispute_info.SideAIsInitiator and
                    (Is_1stYear_Joined_Initiation (ccode1, ccode2, year, currdisp, use_dispute) or
                     Is_1styear_joined_targets (ccode1, ccode2, year, currdisp, use_dispute)) )
                  or (user_selections.dispute_info.SideAIsInitiator and
                     (Is_1stYear_Joined_Revision (ccode1, ccode2, year, currdisp, use_dispute) or
                      Is_1stYear_Joined_SQ (ccode1, ccode2, year, currdisp, use_dispute)) ) );
              curranyorigination :=
                 ( (user_selections.dispute_info.SideAIsInitiator and
                    Is_AnyYear_Initiation (ccode1, ccode2, year, currdisp, use_dispute))
                  or (user_selections.dispute_info.SideAIsInitiator and
                      Is_AnyYear_Revision (ccode1, ccode2, year, currdisp, use_dispute)) );
              curranyjoining :=
                 ( (user_selections.dispute_info.SideAIsInitiator and
                    (Is_AnyYear_Joined_Initiation (ccode1, ccode2, year, currdisp, use_dispute) or
                     Is_AnyYear_Joined_targets (ccode1, ccode2, year, currdisp, use_dispute)) )
                  or (user_selections.dispute_info.SideAIsInitiator and
                     (Is_AnyYear_Joined_Revision (ccode1, ccode2, year, currdisp, use_dispute) or
                      Is_AnyYear_Joined_SQ (ccode1, ccode2, year, currdisp, use_dispute)) ) );
            end;
        begin                {main proc}
           result := null_dispute_number;
           check_initialized;
           found_dispute_num := null_dispute_number;
           function_dispnum := null_dispute_number;
           disputenum12 := index^[ccode1, ccode2];
           disputenum21 := index^[ccode2, ccode1];
           currdisp := 0;
           currhighestdisp := null_dispute_number;
           currhighesthost := 0;
               {preliminarily, check and see if any disputes for this dyad}
           if ((disputenum12 = null_dispute_number) and (disputenum21 = null_dispute_number)) then
              found_dispute_num := null_dispute_number
           else
           begin
              if ((user_selections.output_this = output_directed_dyads) or
                  (user_selections.output_this = output_nondirected_dispute_dyads) or
                  (user_selections.output_this = output_directed_dispute_initiation_dyads)) then
              begin
                 if user_selections.dispute_info.UseMostSeriousDispute = true then
                    begin
                       if disputenum12 <> null_dispute_number then   {only check 1 vs. 2 if have a 1vs2 disp entry in data}
                          begin
                             currdisp := disputenum12;
                             if ((year >= get_first_year(currdisp)) and (year <= get_last_year(currdisp))) then
                                begin
                                   currhighestdisp := currdisp;
                                   currhighesthost := get_hostlev_state (currdisp, ccode1);
                                   set_current_highs (ccode1, ccode2, year, currdisp);
                                end;
                             repeat  {check for the event that had highest host level in this year}
                                if ((year >= get_first_year(currdisp)) and (year <= get_last_year(currdisp))) and
                                   (get_hostlev_state (currdisp, ccode1) > currhighesthost) then
                                   begin
                                      currhighestdisp := currdisp;
                                      currhighesthost := get_hostlev_state (currdisp, ccode1);
                                      set_current_highs (ccode1, ccode2, year, currdisp);
                                   end
                                else
                                   if ((year >= get_first_year(currdisp)) and (year <= get_last_year(currdisp))) and
                                      (get_hostlev_state (currdisp, ccode1) = currhighesthost) then
                                      begin
                                         set_current (ccode1, ccode2, year, currdisp);
                                         {Now, saw equal hostlev.  so sequentially take function value from routines generated above}
                                         if currhigh1storigination then
                                            begin
                                               {already have an origination, don't change the value.}
                                            end
                                         else   {not an origination, so only take new record if its a new joining.}
                                         if currhigh1stjoining then
                                            begin
                                               if curr1storigination then
                                                  begin
                                                     currhighestdisp := currdisp;
                                                     currhighesthost := get_hostlev_state (currdisp, ccode1);
                                                     set_current_highs (ccode1, ccode2, year, currdisp);
                                                  end;
                                            end
                                         else
                                         if currhighanyorigination then
                                            begin
                                               if (curr1storigination or curr1stjoining) then
                                                  begin
                                                     currhighestdisp := currdisp;
                                                     currhighesthost := get_hostlev_state (currdisp, ccode1);
                                                     set_current_highs (ccode1, ccode2, year, currdisp);
                                                  end;
                                            end
                                         else
                                         if currhighanyjoining then
                                            begin
                                               if (curr1storigination or curr1stjoining or curranyorigination) then
                                                  begin
                                                     currhighestdisp := currdisp;
                                                     currhighesthost := get_hostlev_state (currdisp, ccode1);
                                                     set_current_highs (ccode1, ccode2, year, currdisp);
                                                  end;
                                            end;
                                      end;
                                inc(currdisp);
                             until ((currdisp > get_last_dispnum) or
                                    (get_first_year(currdisp) > year) or
                                    (get_ccode(currdisp, 0) <> ccode1) or
                                    (get_ccode(currdisp, 1) <> ccode2));
                          end;       {if dispute12 not 0}

                       if disputenum21 <> null_dispute_number then   {only check 1 vs. 2 if have a 1vs2 disp entry in data}
                          begin
                             currdisp := disputenum21;
                             if currhighestdisp = null_dispute_number then      {only reset highest if didn't find one under the 12 direction}
                             if ((year >= get_first_year(currdisp)) and (year <= get_last_year(currdisp))) then
                             begin
                                currhighestdisp := currdisp;
                                {still want to get the hostility levels for ccode 1}
                                currhighesthost := get_hostlev_state (currdisp, ccode1);
                                set_current_highs (ccode1, ccode2, year, currdisp);
                             end;
                             repeat  {check for the event that had highest host level in this year}
                                if ((year >= get_first_year(currdisp)) and (year <= get_last_year(currdisp))) and
                                   (get_hostlev_state (currdisp, ccode1) > currhighesthost) then
                                   begin
                                      currhighestdisp := currdisp;
                                      currhighesthost := get_hostlev_state (currdisp, ccode1);
                                      set_current_highs (ccode1, ccode2, year, currdisp);
                                   end
                                else
                                   if ((year >= get_first_year(currdisp)) and (year <= get_last_year(currdisp))) and
                                      (get_hostlev_state (currdisp, ccode1) = currhighesthost) then
                                      begin
                                         set_current (ccode1, ccode2, year, currdisp);
                                         {Now, saw equal hostlev.  so sequentially take function value from routines generated above}
                                         if currhigh1storigination then
                                            begin
                                               {already have an origination, don't change the value.}
                                            end
                                         else   {not an origination, so only take new record if its a new joining.}
                                         if currhigh1stjoining then
                                            begin
                                               if curr1storigination then
                                                  begin
                                                     currhighestdisp := currdisp;
                                                     currhighesthost := get_hostlev_state (currdisp, ccode1);
                                                     set_current_highs (ccode1, ccode2, year, currdisp);
                                                  end;
                                            end
                                         else
                                         if currhighanyorigination then
                                            begin
                                               if (curr1storigination or curr1stjoining) then
                                                  begin
                                                     currhighestdisp := currdisp;
                                                     currhighesthost := get_hostlev_state (currdisp, ccode1);
                                                     set_current_highs (ccode1, ccode2, year, currdisp);
                                                  end;
                                            end
                                         else
                                         if currhighanyjoining then
                                            begin
                                               if (curr1storigination or curr1stjoining or curranyorigination) then
                                                  begin
                                                     currhighestdisp := currdisp;
                                                     currhighesthost := get_hostlev_state (currdisp, ccode1);
                                                     set_current_highs (ccode1, ccode2, year, currdisp);
                                                  end;
                                            end;
                                      end;
                                inc(currdisp);
                             until ((currdisp > get_last_dispnum) or
                                    (get_first_year(currdisp) > year) or
                                    (get_ccode(currdisp, 0) <> ccode2) or
                                    (get_ccode(currdisp, 1) <> ccode1));
                          end;       {if dispute21 not 0}

                       found_dispute_num := currhighestdisp;
                    end      {if use most serious}
                 else if user_selections.dispute_info.UseFirstDispute = true then
                    begin
                           {first, check for true initiations}
                       if (user_selections.dispute_info.SideAIsInitiator and
                          Is_1stYear_Initiation (ccode1, ccode2, year, function_dispnum, use_ccodeyear))
                          then found_dispute_num := function_dispnum
                       else  {2nd, check for true initiations by revisionist state}
                          if (not(user_selections.dispute_info.SideAIsInitiator) and
                              Is_1stYear_Revision (ccode1, ccode2, year, function_dispnum, use_ccodeyear))
                             then found_dispute_num := function_dispnum
                       else    {not an initiation or revision attempt, but there are disputes,
                                so check them for new dispute, or ongoing.}
                          begin
                             if disputenum12 <> null_dispute_number then   {only check 1 vs. 2 if have a 1vs2 disp entry in data}
                                repeat  {3rd, check for any event that was new in this year}
                                  {need to check disputes to see if anything starts this year}
                                   if (get_first_year(disputenum12) = year) then
                                      {found a dispute that started in this year, take that hostlev}
                                      found_dispute_num := disputenum12;
                                   inc(disputenum12);
                                until ((found_dispute_num<>null_dispute_number) or
                                       (disputenum12 > get_last_dispnum) or
                                       (get_first_year(disputenum12) > year) or
                                       (get_ccode(disputenum12, 0) <> ccode1) or
                                       (get_ccode(disputenum12, 1) <> ccode2));
                             if found_dispute_num = null_dispute_number then   {if not yet found, check reverse dyad, cc2 vs. cc1}
                             if disputenum21 <> null_dispute_number then   {only check 2 vs. 1 if have a 2vs1 disp entry in data}
                                repeat
                                  {need to check disputes to see if anything starts this year}
                                   if (get_first_year(disputenum21) = year) then
                                      {found a dispute that started in this year, take that hostlev}
                                      found_dispute_num := disputenum21;
                                   inc(disputenum21);
                                until ((found_dispute_num<>null_dispute_number) or
                                       (disputenum21 > get_last_dispnum) or
                                       (get_first_year(disputenum21) > year) or
                                       (get_ccode(disputenum21, 0) <> ccode2) or
                                       (get_ccode(disputenum21, 1) <> ccode1));

                                    {4th, still not found, check to see if something was ongoing}
                             if found_dispute_num = null_dispute_number then
                                if is_ongoing (ccode1, ccode2, year, function_dispnum) then
                                   found_dispute_num := function_dispnum;

                             {else at this point, result [=relevant_midnum] is still 0, meaning there
                              was no initiation or ongoing dispute this year.  }
                          end; {not a revision/initiation}

                    end      {directed - if use first}
                 else EUGeneError ('error in proc get relevant midnum, user_selections not appropriately set for usefirstdispute or usemostseriousdispute.  Fatal error - check program.',1,stop,error_log);
              end         {directed}

              else
              if (user_selections.output_this = output_nondirected_dyads) then
                 begin
                    if user_selections.dispute_info.UseMostSeriousDispute = true then
                       begin
                       if disputenum12 <> null_dispute_number then   {only check 1 vs. 2 if have a 1vs2 disp entry in data}
                          begin
                             currdisp := disputenum12;
                             if ((year >= get_first_year(currdisp)) and (year <= get_last_year(currdisp))) then
                                begin
                                   currhighestdisp := currdisp;
                                   currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                end;
                             repeat  {check for the event that had highest host level in this year}
                                if ((year >= get_first_year(currdisp)) and (year <= get_last_year(currdisp))) and
                                   (max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2)) > currhighesthost) then
                                   begin
                                      currhighestdisp := currdisp;
                                      currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                   end
                                else
                                if ((year >= get_first_year(currdisp)) and (year <= get_last_year(currdisp))) and
                                   (max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2)) = currhighesthost) then
                                   begin
                                      if Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, currhighestdisp, use_dispute) then
                                         begin    {do nothing, already originators}
                                         end
                                      else
                                      if Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, currhighestdisp, use_dispute) then
                                         begin
                                            if Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, currdisp, use_dispute) then
                                            begin
                                               currhighestdisp := currdisp;
                                               currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                            end
                                         end
                                      else
                                      if Is_AnyYear_NonDir_Dispute_Originators (ccode1, ccode2, year, function_dispnum, use_ccodeyear) then
                                         begin
                                            if (Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, currdisp, use_dispute) or
                                                Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, currhighestdisp, use_dispute) ) then
                                            begin
                                               currhighestdisp := currdisp;
                                               currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                            end
                                         end
                                      else
                                      if Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, function_dispnum, use_ccodeyear) then
                                         begin
                                            if (Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, currdisp, use_dispute) or
                                                Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, currhighestdisp, use_dispute) or
                                                Is_AnyYear_NonDir_Dispute_Originators (ccode1, ccode2, year, function_dispnum, use_ccodeyear) ) then
                                            begin
                                               currhighestdisp := currdisp;
                                               currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                            end
                                         end;
                                   end;
                                inc(currdisp);
                             until ((currdisp > get_last_dispnum) or
                                    (get_first_year(currdisp) > year) or
                                    (get_ccode(currdisp, 0) <> ccode1) or
                                    (get_ccode(currdisp, 1) <> ccode2));
                          end;       {if dispute12 not 0}
                       if disputenum21 <> null_dispute_number then   {only check 2 vs. 1 if have entry in data}
                          begin
                             currdisp := disputenum21;
                             if currhighestdisp = 0 then
                             if ((year >= get_first_year(currdisp)) and (year <= get_last_year(currdisp))) then
                                begin
                                   currhighestdisp := currdisp;
                                   currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                end;
                             repeat  {check for the event that had highest host level in this year}
                                if ((year >= get_first_year(currdisp)) and (year <= get_last_year(currdisp))) and
                                   (max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2)) > currhighesthost) then
                                   begin
                                      currhighestdisp := currdisp;
                                      currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                   end
                                else
                                if ((year >= get_first_year(currdisp)) and (year <= get_last_year(currdisp))) and
                                   (max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2)) = currhighesthost) then
                                   begin
                                      if Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, currhighestdisp, use_dispute) then
                                         begin    {do nothing, already originators}
                                         end
                                      else
                                      if Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, currhighestdisp, use_dispute) then
                                         begin
                                            if Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, currdisp, use_dispute) then
                                            begin
                                               currhighestdisp := currdisp;
                                               currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                            end
                                         end
                                      else
                                      if Is_AnyYear_NonDir_Dispute_Originators (ccode1, ccode2, year, function_dispnum, use_ccodeyear) then
                                         begin
                                            if (Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, currdisp, use_dispute) or
                                                Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, currhighestdisp, use_dispute) ) then
                                            begin
                                               currhighestdisp := currdisp;
                                               currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                            end
                                         end
                                      else
                                      if Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, function_dispnum, use_ccodeyear) then
                                         begin
                                            if (Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, currdisp, use_dispute) or
                                                Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, currhighestdisp, use_dispute) or
                                                Is_AnyYear_NonDir_Dispute_Originators (ccode1, ccode2, year, function_dispnum, use_ccodeyear) ) then
                                            begin
                                               currhighestdisp := currdisp;
                                               currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                            end
                                         end;
                                   end;
                                inc(currdisp);
                             until ((currdisp > get_last_dispnum) or
                                    (get_first_year(currdisp) > year) or
                                    (get_ccode(currdisp, 0) <> ccode2) or
                                    (get_ccode(currdisp, 1) <> ccode1));
                          end;       {if dispute21 not 0}

                          found_dispute_num := currhighestdisp;
                       end
                    else if user_selections.dispute_info.UseFirstDispute = true then
                       begin
                            {for nondirected, first see if any new dispute originator this year.
                             If multiple, take the first one.
                             2nd, take first year of any dispute including joiners.
                             3rd, check ongoing disutes, first to see if both originators of an
                             ongoing dispute.
                             4th, If not ongoing originators, then check ongoing joiners.}
                          if Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, function_dispnum, use_ccodeyear)
                             then found_dispute_num := function_dispnum
                          else    {not an originator side dispute this year,
                                   so check them for new dispute and joining.}
                             if Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, function_dispnum, use_ccodeyear)
                                then found_dispute_num := function_dispnum
                          else
                                {3rd, still not found, check to see if something was ongoing}
                             if Is_AnyYear_NonDir_Dispute_Originators (ccode1, ccode2, year, function_dispnum, use_ccodeyear)
                                then found_dispute_num := function_dispnum
                          else
                             {4th, still not found, check to see ongoing joiners}
                             if Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, function_dispnum, use_ccodeyear)
                                then found_dispute_num := function_dispnum;
                                {else at this point, result [=relevant_midnum] is still 0, meaning there
                                 was no initiation or ongoing dispute this year.  }
                       end          {use first dispute true}
                    else EUGeneError ('error in proc get relevant midnum nondirected, user_selections not appropriately set for usefirstdispute or usemostseriousdispute.  Fatal error - check program.',1,stop,error_log);
                 end         {nondirected}

              else
                 begin
                    EUGeneError ('Relevant_midnum called but neither dispute-dyad, directed-dyad, or non-directed dyad output selected.  Relevant Hostlev set to 0.',1,continue,error_log);
                    found_dispute_num := null_dispute_number;
                 end;
           end;       {dispute # not 0}

           result := found_dispute_num;

        end;   {proc get relevant midnum}

     function TCOWDyadic_dispute_data_obj.get_last_dispute_in_year ( const ccode1, ccode2 : ccode_range; const year : year_range;
                 const which_state : side_type; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : TDateTime;
      var disputenum12, disputenum21, dispute_temp: longint;
          temp_month : month_range;
          temp_day : MIDdaytype;
          temp_date : TDateTime;

      begin
            Result := 0;
            dispute_temp := 0;
            temp_date := 0;
            
            disputenum12 := index^[ccode1, ccode2];
            disputenum21 := index^[ccode2, ccode1];

            if disputenum12 <> null_dispute_number then   {only check 1 vs. 2 if have a 1vs2 disp entry in data}
              repeat
                if (get_endyear(disputenum12, ccode1) = year) then
                  begin
                    dispute_temp := disputenum12;
                  end;
                inc(disputenum12);
              until ((temp_date<>0) or
                (disputenum12 > get_last_dispnum) or
                (get_endyear(disputenum12, ccode1) > year) or
                (get_ccode(disputenum12, 0) <> ccode1) or
                (get_ccode(disputenum12, 1) <> ccode2));
            if dispute_temp <> 0 then
              begin
                temp_month := get_endmonth(dispute_temp,ccode1);
                temp_day := get_endday(dispute_temp,ccode1);
                if((temp_day > 0) and (temp_day < 32)) then
                  begin
                    if((temp_month = 4) or (temp_month = 6) or (temp_month = 9) or (temp_month = 11)) then
                      if(temp_day = 31) then
                        temp_day := 30;
                    if((temp_month = 2) and (temp_day > 29)) then
                      temp_day := 28;
                    temp_date := EncodeDate(year, temp_month, temp_day);
                  end
                else
                  begin
                    temp_day := 15;
                    temp_date := EncodeDate(year, temp_month, temp_day);
                  end
              end;
            if (temp_date = 0) then
            if disputenum21 <> null_dispute_number then   {only check 2 vs. 1 if have a 2vs1 disp entry in data}
              repeat
              {need to check disputes to see if anything starts this year}
                if (get_endyear(disputenum21,ccode2) = year) then
                  begin
                    dispute_temp := disputenum21;
                  end;
                inc(disputenum21);
              until ((temp_date<>0) or
                (disputenum21 > get_last_dispnum) or
                (get_endyear(disputenum21,ccode2) > year) or
                (get_ccode(disputenum21, 0) <> ccode2) or
                (get_ccode(disputenum21, 1) <> ccode1));
            if dispute_temp <> 0 then
              begin
                temp_month := get_endmonth(dispute_temp, ccode1);
                temp_day := get_endday(dispute_temp, ccode1);
                if((temp_day > 0) and (temp_day < 32)) then
                 begin
                  if((temp_month = 4) or (temp_month = 6) or (temp_month = 9) or (temp_month = 11)) then
                      if(temp_day = 31) then
                        temp_day := 30;
                  if((temp_month = 2) and (temp_day > 29)) then
                      temp_day := 28;
                  temp_date := EncodeDate(year, temp_month, temp_day)
                  end
                else
                  begin
                    temp_day := 15;
                    temp_date := EncodeDate(year, temp_month, temp_day);
                  end
              end;
            Result := temp_date;
      end; {get last dispute in year function}

     function TCOWDyadic_dispute_data_obj.relevant_hostlev (const ccode1, ccode2 : ccode_range; const year : year_range;
                 const which_state : side_type; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : hostlevtype;
        {returns hostlev, 0 if no dispute, of cc1 in dispute with cc2 in year}
        {Which state is 0 for ccode1, 1 for ccode2.}

        {For directed dyads, rule for where to get the hostility level is as follows.
         1st, if want true initiations, and there's a new initiation in this year, use that.
         2nd, if want true revisionists, do the same but with revisions.
         Then, if there is not a true initiation or revision attempt, then
         3rd, take value of 1st new event in this year if there is one, either join, target, whatever.
         4th, if no new dispute, but one ongoing, take the value of the ongoing disp.
         Finally, if nothing, 0.}

        {for nondirected, first see if any new dispute originator this year.
           If multiple, take the first one.
           2nd, take any dispute including joiner.
           3rd, check ongoing disutes, first to see if both originators of an
           ongoing dispute.
           4th, If not ongoing originators, then check ongoing joiners.}

        {returns the dyadic dispute number of the dispute it is reporting the hostility level for.}
        
        var found_dispute_num, disputenum12, disputenum21 : longint;
        begin
           check_initialized;
           relevant_hostlev := 0;
           found_dispute_num := null_dispute_number;
           if use_param = use_dispute then
              begin
                 if which_state = 0 then relevant_hostlev := (get_hostlev_state (dispnum, ccode1))
                 else if which_state = 1 then relevant_hostlev := (get_hostlev_state (dispnum, ccode2));
              end
           else      {search by ccode year}
              begin
                 disputenum12 := index^[ccode1, ccode2];
                 disputenum21 := index^[ccode2, ccode1];
                     {preliminarily, check and see if any disputes for this dyad}
                 if ((disputenum12 = null_dispute_number) and (disputenum21 = null_dispute_number)) then
                    relevant_hostlev := 0
                 else
                 begin
                    if ((user_selections.output_this = output_directed_dyads) or
                    (user_selections.output_this = output_nondirected_dispute_dyads) or
                        (user_selections.output_this = output_directed_dispute_initiation_dyads)) then
                        begin
                              {first, check for true initiations}
                          if (user_selections.dispute_info.SideAIsInitiator and
                              Is_1stYear_Initiation (ccode1, ccode2, year, found_dispute_num, use_param))
                             then
                                begin
                                   if which_state = 0 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode1))
                                   else if which_state = 1 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode2));
                                   dispnum := found_dispute_num;
                                end
                          else  {2nd, check for true initiations by revisionist state}
                          if (not(user_selections.dispute_info.SideAIsInitiator) and
                              Is_1stYear_Revision (ccode1, ccode2, year, found_dispute_num, use_param))
                             then
                                begin
                                   if which_state = 0 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode1))
                                   else if which_state = 1 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode2));
                                   dispnum := found_dispute_num;
                                end
                          else    {not an initiation or revision attempt, but there are disputes,
                                   so check them for new dispute, or ongoing.}
                             begin
                                if disputenum12 <> null_dispute_number then   {only check 1 vs. 2 if have a 1vs2 disp entry in data}
                                   repeat  {3rd, check for any event that was new in this year}
                                     {need to check disputes to see if anything starts this year}
                                      if (get_first_year(disputenum12) = year) then
                                         begin
                                            {found a dispute that started in this year, take that hostlev}
                                            if which_state = 0 then relevant_hostlev := (get_hostlev_state (disputenum12, ccode1))
                                            else if which_state = 1 then relevant_hostlev := (get_hostlev_state (disputenum12, ccode2));
                                            dispnum := disputenum12;
                                         end;
                                      inc(disputenum12);
                                   until ((result>0) or
                                          (disputenum12 > get_last_dispnum) or
                                          (get_first_year(disputenum12) > year) or
                                          (get_ccode(disputenum12, 0) <> ccode1) or
                                          (get_ccode(disputenum12, 1) <> ccode2));
                                if result = 0 then   {if not yet found, check reverse dyad, cc2 vs. cc1}
                                if disputenum21 <> null_dispute_number then   {only check 2 vs. 1 if have a 2vs1 disp entry in data}
                                   repeat
                                     {need to check disputes to see if anything starts this year}
                                      if (get_first_year(disputenum21) = year) then
                                         begin
                                            {found a dispute that started in this year, take that hostlev}
                                            if which_state = 0 then relevant_hostlev := (get_hostlev_state (disputenum21, ccode1))
                                            else if which_state = 1 then relevant_hostlev := (get_hostlev_state (disputenum21, ccode2));
                                            dispnum := disputenum21;
                                         end;
                                      inc(disputenum21);
                                   until ((result>0) or
                                          (disputenum21 > get_last_dispnum) or
                                          (get_first_year(disputenum21) > year) or
                                          (get_ccode(disputenum21, 0) <> ccode2) or
                                          (get_ccode(disputenum21, 1) <> ccode1));

                                       {4th, still not found, check to see if something was ongoing}
                                if result = 0 then
                                   if is_ongoing (ccode1, ccode2, year, found_dispute_num) then
                                      begin
                                         if which_state = 0 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode1))
                                         else if which_state = 1 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode2));
                                         dispnum := found_dispute_num;
                                      end;

                                {else at this point, result [=relevant_hostlev] is still 0, meaning there
                                 was no initiation or ongoing dispute this year.  So, no hostility for
                                 this ccode.}
                             end; {not a revision/initiation}
                        end   {directed disputes, dispute-dyads}
                     else
                     if (user_selections.output_this = output_nondirected_dyads) then
                        begin
                            {for nondirected, first see if any new dispute originator this year.
                             If multiple, take the first one.
                             2nd, take any dispute including joiner.
                             3rd, check ongoing disutes, first to see if both originators of an
                             ongoing dispute.
                             4th, If not ongoing originators, then check ongoing joiners.}
                          if Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, found_dispute_num, use_param) then
                                begin
                                   if which_state = 0 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode1))
                                   else if which_state = 1 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode2));
                                   dispnum := found_dispute_num;
                                end
                          else    {not an originator side dispute this year,
                                   so check them for new dispute and joining.}
                             if Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, found_dispute_num, use_param) then
                                begin
                                   if which_state = 0 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode1))
                                   else if which_state = 1 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode2));
                                   dispnum := found_dispute_num;
                                end
                          else
                                {3rd, still not found, check to see if something was ongoing}
                             if Is_AnyYear_NonDir_Dispute_Originators (ccode1, ccode2, year, found_dispute_num, use_param) then
                                begin
                                   if which_state = 0 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode1))
                                   else if which_state = 1 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode2));
                                   dispnum := found_dispute_num;
                                end
                          else
                             {4th, still not found, check to see ongoing joiners}
                             if Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, found_dispute_num, use_param) then
                                begin
                                   if which_state = 0 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode1))
                                   else if which_state = 1 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode2));
                                   dispnum := found_dispute_num;
                                end;
                                {else at this point, result [=relevant_hostlev] is still 0, meaning there
                                 was no initiation or ongoing dispute this year.  So, no hostility for
                                 this ccode.}
                        end        {if nondirected}
                     else
                        begin
                           EUGeneError ('Relevant_hostlev called but neither dispute-dyad, directed-dyad, or non-directed dyad output selected.  Relevant Hostlev set to 0.',1,continue,error_log);
                           relevant_hostlev := 0;
                        end;
                 end;
              end;     {not param = use_dispute #}

           {Also, want to change coding as a "-9" hostility to a 1, which means
            no militarized response, although you were a target.}
           if (result = -9) then relevant_hostlev := 1;

        end;      {proc relevant hostlev}

      function TCOWDyadic_dispute_data_obj.get_peace_days (ccode1, ccode2 : ccode_range; year : year_range;
                 user_selections : user_selection_type; werner_peace_years_data : TWernerPeaceYears_obj) : integer;

        var adispnum, ongoing_num, werner_adj : longint;
            hostprev1, hostprev2 : hostlevtype;
            last_year, ctr : integer;
            Current_date, End_Date : TDateTime;

          function all_in_range (ccode1, ccode2 : ccode_range;  ayear : year_range; error_check : boolean): boolean;
            begin
              all_in_range := true;
              if (ccode1=0) or (ccode2=0) then
                all_in_range := false
              else
              if not(initialized) then
                begin
                  all_in_range := false;
                  if error_check then
                     EUGeneError ('Get peace days get called before dispute initialization', 5, stop, error_log);
                end
              else
              if not ((ayear >= get_first_partition_year) and (ayear <= get_last_partition_year)) then
                begin
                  all_in_range := false;
                  if error_check then
                        EUGeneError ('Internal Error in program - called get peace days with year outside partition',
                                        5, continue, error_log);
                end
              else
               if not ((nation_list.is_a_state(ccode1, ayear)) and (nation_list.is_a_state(ccode2, ayear)) ) then
                   begin
                     all_in_range := false;
                     if error_check then
                       trace.message ('Error in ccode-years: called get peace days for invalid ccodes given ccodes/year'+
                                inttostr(ccode1)+' ' + inttostr(ccode2)+ ' in '+inttostr(ayear));
                   end;
          end;  {func all in range}

        begin
          Current_date := EncodeDate(year, 1, 1);
          ongoing_num := null_dispute_number;
          ctr := 0;
          End_date := -99999;
          werner_adj := 0;

          if (all_in_range(ccode1, ccode2, year, false)) then  {call with no error check}
            repeat
              if (((year-ctr) = nation_list.get_startyear1(ccode1)) or ((year-ctr) = nation_list.get_startyear2(ccode1)) or
                    ((year-ctr) = nation_list.get_startyear1(ccode2)) or ((year-ctr) = nation_list.get_startyear2(ccode2)) )
                    then End_date := EncodeDate((year-ctr),1,1);
               if ((year-ctr) = 1816) then
                  begin
                    if user_selections.werner_peace_year_adjustment=true then
                          werner_adj := werner_peace_years_data.get_werner_peacedays(ccode1, ccode2);
                    End_date := EncodeDate(1816,1,1);
                  end;

              if(ctr = 0) then
                begin
                  if is_ongoing(ccode1, ccode2, year, ongoing_num) then
                    End_date := Current_date;
                end
              else if(ctr > 0) then
                begin
                  hostprev1 := relevant_hostlev (ccode1, ccode2, (year-ctr), 0, user_selections, adispnum, use_ccodeyear);
                  hostprev2 := relevant_hostlev (ccode1, ccode2, (year-ctr), 1, user_selections, adispnum, use_ccodeyear);
                  if((hostprev1 > 0) or (hostprev2 > 0)) then
                    End_date := get_last_dispute_in_year(ccode1, ccode2, (year-ctr), 1, user_selections, adispnum, use_ccodeyear);
                end;

              ctr := ctr + 1;

              until(End_date <> -99999);
            Result := werner_adj + DaysBetween(Current_date, End_date);
        end;

      function TCOWDyadic_dispute_data_obj.get_peace_years (ccode1, ccode2 : ccode_range; year : year_range;
                               user_selections : user_selection_type; werner_peace_years_data : TWernerPeaceYears_obj) : integer;
         var adispnum: longint;
            hostprev1, hostprev2 : hostlevtype;
            last_year : integer;
           {returns # of years since last dispute.  1816 always gets value 0, and series increments
            from there}

         function all_in_range (ccode1, ccode2 : ccode_range;  ayear : year_range; error_check : boolean): boolean;
         begin
            all_in_range := true;
            if (ccode1=0) or (ccode2=0) then
               all_in_range := false
            else
            if not(initialized) then
               begin
                  all_in_range := false;
                  if error_check then
                     EUGeneError ('Get peace years get called before dispute initialization', 5, stop, error_log);
               end
            else
            if not ((ayear >= get_first_partition_year) and (ayear <= get_last_partition_year)) then
               begin
                  all_in_range := false;
                  if error_check then
                        EUGeneError ('Internal Error in program - called get peace years with year outside partition',
                                        5, continue, error_log);
               end
            else
               if not ((nation_list.is_a_state(ccode1, ayear)) and (nation_list.is_a_state(ccode2, ayear)) ) then
                  begin
                     all_in_range := false;
                     if error_check then
                           trace.message ('Error in ccode-years: called get peace years for invalid ccodes given ccodes/year'+
                                 inttostr(ccode1)+' ' + inttostr(ccode2)+ ' in '+inttostr(ayear));
                  end;

      end;  {func all in range}

        begin                    {main proc get peace years}
           get_peace_years := missing_value;
              {all in range ensures that they are both already states.}
           if (all_in_range(ccode1, ccode2, year, false)) then  {call with no error check}
              begin
                 if (year = 1816) then
                    begin
                       if user_selections.werner_peace_year_adjustment=true then
                          get_peace_years := werner_peace_years_data.get_werner_peaceyears(ccode1, ccode2)
                       else get_peace_years := 0;
                    end
                 else
                    if ((year = nation_list.get_startyear1(ccode1)) or (year = nation_list.get_startyear2(ccode1)) or
                        (year = nation_list.get_startyear1(ccode2)) or (year = nation_list.get_startyear2(ccode2)) )
                    then get_peace_years := 0
                 else     {not 1816, and neither is a new state}
                    begin
                         {need to look at hostility in the previous year;  if there was a MID, then this year
                          is 0}
                       hostprev1 := relevant_hostlev (ccode1, ccode2, (year-1), 0, user_selections, adispnum, use_ccodeyear);
                       hostprev2 := relevant_hostlev (ccode1, ccode2, (year-1), 1, user_selections, adispnum, use_ccodeyear);
                       if ((hostprev1 = missing_value) or (hostprev2 = missing_value)) then get_peace_years := missing_value
                       else
                          if (hostprev1 > 0) or (hostprev2 > 0) then get_peace_years := 0
                       else    {host last year was not missing, and not 0}
                          {There is no hostility last year, so increment last year's value.
                           But, to speed up this calculation, I sometimes (usually) have last year's value in memory already}
                          if ((stored_peaceyrs.ccode1 = ccode1) and (stored_peaceyrs.ccode2 = ccode2) and
                              (stored_peaceyrs.year = year - 1) )
                              then
                                 begin
                                    if stored_peaceyrs.numyears = missing_value then get_peace_years := missing_value
                                    else get_peace_years := (stored_peaceyrs.numyears + 1);
                                 end
                          else    {don't have last year's value in memory, so do manual recursion}
                             begin
                                last_year := get_peace_years (ccode1, ccode2, year-1, user_selections, werner_peace_years_data);
                                if last_year = missing_value then get_peace_years := missing_value else
                                get_peace_years := last_year + 1;
                             end;
                    end;
                 {Now store this value of peaceyears to speed up search next time}
                 stored_peaceyrs.ccode1 := ccode1;
                 stored_peaceyrs.ccode2 := ccode2;
                 stored_peaceyrs.year := year;
                 stored_peaceyrs.numyears := result;
              end;
        end;

      function TCOWDyadic_dispute_data_obj.get_num_new_mids (ccode1, ccode2 : ccode_range; year : year_range) : integer;
        {counts number of new MIDs involving cc1 and cc2 in this year.  NONDIRECTED.
         Counts Even if 2 vs. 1.  SO, this is nondirected.}
        var disputenum : longint;
        begin
           check_initialized;
           result := 0;
           disputenum := index^[ccode1, ccode2];
           if disputenum <> null_dispute_number then
              repeat
                {need to check all disputes starting with the 1st until another dyad is seen,
                  or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                  I've counted all of them.}
                 if (get_first_year (disputenum) = year) and (get_last_year (disputenum) >= year) then
                    inc(result);
                 inc(disputenum);
              until (
                     (disputenum > get_last_dispnum) or
                     (get_first_year(disputenum) > year) or
                     (get_ccode(disputenum, 0) <> ccode1) or
                     (get_ccode(disputenum, 1) <> ccode2));
                 {At this point, have examined all records of cc1 vs cc2}

               {now, check the reverse dyad to see if 1st year of 2 vs. 1}
           disputenum := index^[ccode2, ccode1];
           if disputenum <> null_dispute_number then
              repeat
                 if (get_first_year (disputenum) = year) and (get_last_year (disputenum) >= year) then
                    inc(result);
                 inc(disputenum);
              until (
                     (disputenum > get_last_dispnum) or
                     (get_first_year(disputenum) > year) or
                     (get_ccode(disputenum, 0) <> ccode2) or
                     (get_ccode(disputenum, 1) <> ccode1));
        end;     {proc}

      function TCOWDyadic_dispute_data_obj.get_num_total_mids (ccode1, ccode2 : ccode_range; year : year_range) : integer;
        {counts number of ongoing or new MIDs involving cc1 and cc2.  NONDIRECTED.
         Counts Even if 2 vs. 1.  SO, this is nondirected.}
        var disputenum : longint;
        begin
           check_initialized;
           result := 0;
           disputenum := index^[ccode1, ccode2];
           if disputenum <> null_dispute_number then
              repeat
                {need to check all disputes starting with the 1st until another dyad is seen,
                  or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                  I've counted all of them.}
                 if (get_first_year (disputenum) <= year) and (get_last_year (disputenum) >= year) then
                    inc(result);
                 inc(disputenum);
              until (
                     (disputenum > get_last_dispnum) or
                     (get_first_year(disputenum) > year) or
                     (get_ccode(disputenum, 0) <> ccode1) or
                     (get_ccode(disputenum, 1) <> ccode2));
                 {At this point, have examined all records of cc1 vs cc2}

               {now, check the reverse dyad to see if 1st year of 2 vs. 1}
           disputenum := index^[ccode2, ccode1];
           if disputenum <> null_dispute_number then
              repeat
                 if (get_first_year (disputenum) <= year) and (get_last_year (disputenum) >= year) then
                    inc(result);
                 inc(disputenum);
              until (
                     (disputenum > get_last_dispnum) or
                     (get_first_year(disputenum) > year) or
                     (get_ccode(disputenum, 0) <> ccode2) or
                     (get_ccode(disputenum, 1) <> ccode1));
        end;  {proc}

        {        -----------------------------------------------------   }

  {Now the procedures that are specific to the version 2.1 format data, i.e. where we had to
   construct MIDs from country-year records.

        {  ------------------------------------------------  }

     {Now, COW based specifics for dyadic dispute object}

     {public functions}
     constructor TCOWDyadic_dispute_data_obj_format21.init (user_selections : user_selection_type;
                  configuration : configuration_type; year1, year2 : year_range);
      {init computes dyadic disputes after reading country-dispute data.
       It also sorts the dyadic dispute data by ccode1, ccode2, then start year of dispute.
       It creates records for all on side1 vs. all on side 2, setting up cc1 vs. cc2.}

        var Dyadic_dispute_trace : TTrace_obj;
            heapneeded, start_mem : longint;
            country_dispute, x, y, first_of_dispute, last_of_dispute : integer;
            done, bad_case : boolean;

         procedure quicksort_dyadic_disputes_by_country (left, right:integer; sort_dyad:side_type);
                  {Sorts in order low to high ccode between left and right;
                   Will sort by country 1 or country 2 depending on value of sort_dyad.
                   Note that sort_dyad is not same as sideA.  It just sorts by 1st or 2nd, sideA
                   only has to do with the (previous) creation of dyads.}
            const trace_quicksort = false;
            var
               base_ccode, i, j, comp_ccode : integer;
               temp: dyad_dispute_main_rec;
            begin
             if right > left then
               begin
                  {v needs to be set to value of right case, ccode of country 1 or ccode of country 2}
                  {dyadic_disputes^[right].pointer1 is case in country_disputes of first ccode}
                  base_ccode := self.get_ccode(right, sort_dyad);
                  i := left - 1;
                  j := right;
                  repeat
                     repeat
                        i := i + 1;
                        comp_ccode := self.get_ccode(i, sort_dyad);
                     until comp_ccode >= base_ccode;
                     repeat
                        j := j - 1;
                        comp_ccode := self.get_ccode(j, sort_dyad);
                     until (comp_ccode <= base_ccode) or (j = left);
                     temp := dyadic_dispute_data^[i]^;
                     dyadic_dispute_data^[i]^ := dyadic_dispute_data^[j]^;
                     dyadic_dispute_data^[j]^ := temp;
                  until j <= i;
                  dyadic_dispute_data^[j]^ := dyadic_dispute_data^[i]^;
                  dyadic_dispute_data^[i]^ := dyadic_dispute_data^[right]^;
                  dyadic_dispute_data^[right]^ := temp;
                  quicksort_dyadic_disputes_by_country (left, i-1, sort_dyad);
                  quicksort_dyadic_disputes_by_country (i+1, right, sort_dyad);
               end;
            end;      {procedure quicksort_dyads_by_country}

            {   -----------------------  }

         procedure quicksort_dyadic_disputes_by_first_year (left, right:integer);
                  {Sorts in order low to high}
            var
               base_year, comp_year, i, j: integer;
               temp: dyad_dispute_main_rec;

            begin
             if right > left then
               begin
                  base_year := self.get_first_year (right);
                  i := left - 1;
                  j := right;
                  repeat
                     repeat
                        i := i + 1;
                        comp_year := self.get_first_year (i);
                     until comp_year >= base_year;
                     repeat
                        j := j - 1;
                        comp_year := self.get_first_year (j);
                     until (comp_year <= base_year) or (j = left);
                     temp := dyadic_dispute_data^[i]^;
                     dyadic_dispute_data^[i]^ := dyadic_dispute_data^[j]^;
                     dyadic_dispute_data^[j]^ := temp;
                  until j <= i;
                  dyadic_dispute_data^[j]^ := dyadic_dispute_data^[i]^;
                  dyadic_dispute_data^[i]^ := dyadic_dispute_data^[right]^;
                  dyadic_dispute_data^[right]^ := temp;
                  quicksort_dyadic_disputes_by_first_year (left, i-1);
                  quicksort_dyadic_disputes_by_first_year (i+1, right);
               end;
         end;      {procedure quicksort_dyads_by_first_year}

            {   -----------------------  }

        begin   {main proc dyadic data init}
           country_disputes := nil;
           dyadic_dispute_trace := nil;
           try
               trace.enter('Initializing dyadic dispute data, version 2.1 format,');
               if year1 > year2 then switch_year (year1, year2);
               self.first_partition_year := year1;
               self.last_partition_year := year2;

               start_mem := memavail;
               heapneeded := TCOWdyadic_dispute_obj_mem_overhead;
               if debug[4] then
                  begin
                     trace.message ('Dyadic Dispute array size calculation');
                     trace.message ('Calc is that '+inttostr(heapneeded)+' needed for all years.');
                     trace.message ('Max avail mem block is '+inttostr(MaxAvail));
                  end;
               if MaxAvail <= (heapneeded) then
                     EUGeneError ('Not enough memory for dyadic dispute array. ',
                                     5, stop, error_log);

               {First, must read/create country dispute data.   }
               country_disputes := TCountry_dispute_data_obj.init (configuration.cow_mid_actor_file_nameB, configuration.cow_mid_data_format);
               if not(country_disputes.initialized) then
                     EUGeneError ('Country_dispute initialization not completed succesfully during dyadid dispute init. ',
                                     5, stop, error_log);

               {Also, now, must read in overall dispute information}
               overall_disputes := Toverall_dispute_data_obj.init (configuration.cow_mid_case_file_nameA,
                                                           configuration.cow_mid_name_file_nameC,configuration.cow_mid_data_format);
               if not(overall_disputes.initialized) then
                     EUGeneError ('Overall_dispute initialization not completed succesfully during dyadid dispute init. ',
                                     5, stop, error_log);


               Dyadic_dispute_trace := TTrace_obj.init(trace.get_trace_level);
               trace.message ('Initializing Dyadic dispute data structure');

               new (dyadic_dispute_data);
               setlength (dyadic_dispute_data^, max_dyadic_disputes);
               num_dyadic_disputes := 0;
               for x := 0 to max_dyadic_disputes-1 do dyadic_dispute_data^[x] := nil;

               new (index);
               for x := 1 to top_nation_num do
                  for y := 1 to top_nation_num do
                     index^[x,y] := null_dispute_number;

               {Go through country-dispute list.  For every dispute, create record of all inits}
               { by all tgts.  Put in order, low ccode to high ccode.}
               {Note:  want to order as sideA vs other side.  But, data not nece. sorted by
                sideA then side B.  So first just mark 1st and last dispute, then
                go through full list and pair up.}
               country_dispute := 0;    {start with first record}
               while (country_dispute <= (country_disputes.get_last_dispnum) ) and
                     (num_dyadic_disputes <= max_dyadic_disputes) do
                  begin
                     Dyadic_dispute_trace.tick ('Executing Procedure: Create dyadic disputes ',country_disputes.get_num_country_disputes);
                     first_of_dispute := country_dispute;
                     {This marks first country-disp of this dispute.  Could be multiple countries on each side.
                      Need to mark last of side 1, first of side 2, last of side 2}
                     last_of_dispute :=  first_of_dispute;
                     repeat
                        inc(last_of_dispute);
                     until (last_of_dispute >= country_disputes.get_last_dispnum) or
                           (country_disputes.get_disputeid(last_of_dispute) <> country_disputes.get_disputeid(first_of_dispute));
                     {6/12/06 fixed problem here that occured when complete boolean is on.  Above
                      >= was added (from just >, and dec code changed below}
                     {Now have hit last record in this dispute.}
                     {decrement back to last, if there are more;
                        but if this is the last on the list of disputes, don't decr it.  This assumes
                        the last disp rec is part of dispute before it, it should be [couldn't be a mid
                        otherwise without at least one on the other side.]}
                     if last_of_dispute = country_disputes.get_last_dispnum then
                        begin
                        end
                     else   {else went one beyond the last of this dispute, so go back}
                        dec(last_of_dispute);
                     {   was:  dec(last_of_dispute);  (before fixing to allow complete eval.)}

                     if not (first_of_dispute <= last_of_dispute) then
                        begin
                           EUGeneError ('Error in make dyadic disputes procedure ' +
                                 ': loops within country disputes not set properly.  Notify programmer',
                                 5, continue, error_log);
                        end;

                     {Now have the first, last country dispute marked, so can create dyads from all the combos.}

                        {create dyads for all on side A vs. all on side B, for the dyads that overlap}
                     for x := first_of_dispute to last_of_dispute do
                        for y := first_of_dispute to last_of_dispute do
                        {Possibly make a dyadic dispute from this pair.  Do this if it's side A vs B,
                         and that's how the user wants initiation coded.  Or, do this if it's a revisionist
                         vs. someone on the other side, and the user wants revisionsts as initiators.}

                           if ((user_selections.dispute_info.SideAIsInitiator) and
                               (country_disputes.get_sideA(x) = true) and
                               (country_disputes.get_sideA(y) = false) and
                               (num_dyadic_disputes <= max_dyadic_disputes) and
                               (country_disputes.date_overlap(x, y)) )
                             or
                              ((not(user_selections.dispute_info.SideAIsInitiator)) and
                               (country_disputes.get_revisionist(x)) and
                               (country_disputes.get_sideA(x) <> country_disputes.get_sideA(y)) and
                               (num_dyadic_disputes <= max_dyadic_disputes) and
                               (country_disputes.date_overlap(x, y))) then
                             {check if in time range for initialization of this structure}
                             if (  (   (max (country_disputes.get_styear(x),country_disputes.get_styear(y)) >= year1)
                                    and (max (country_disputes.get_styear(x),country_disputes.get_styear(y)) <= year2)
                                   )  or
                                   (   (min (country_disputes.get_endyear(x),country_disputes.get_endyear(y)) >= year1)
                                    and (min (country_disputes.get_endyear(x),country_disputes.get_endyear(y)) <= year2)
                                   )
                                ) then
                                 {if they overlap, add a dyadic record}
                              begin
                                  {do check of ccodes}
                                 bad_case := false;

                                 if country_disputes.get_ccode(x) = country_disputes.get_ccode(y) then
                                    begin
                                       {There is an occasional (1 case) problem with the COW data, where
                                        a country switches sides from one side to the other of the dispute,
                                        and the last day on one side is the same as the first day on other
                                        side.  This case generates an error.  Do not add this case to
                                        the dyadic data (the country doesn't really dispute itself}
                                       trace.message ('Dropping COW data error where ccode '+inttostr(country_disputes.get_ccode(y))+
                                                ' appears to have a dispute with itself.  ');
                                       {To drop this case, just decrement the counter so that the next dispute
                                        will be assigned to this slot.}
                                       bad_case := true;
                                    end
                                 else      {different ccodes, probably OK record}
                                 if (not(nation_list.is_a_state(country_disputes.get_ccode(x),
                                            country_disputes.get_styear(x))) or
                                     not(nation_list.is_a_state(country_disputes.get_ccode(y),
                                            country_disputes.get_styear(y)))) then
                                    begin
                                       {There is an occasional (1 case) problem with the COW MID data,
                                        where zimbabwe supposedly has a 1965 dispute but isn't a state
                                        until 1966.  If a case is a dispute in a year but the states aren't
                                        really states, drop it.}
                                       trace.message ('Dropping COW MID where ccode '+inttostr(country_disputes.get_ccode(x))+ ' or '+ inttostr(country_disputes.get_ccode(y))+
                                                ' is not a COW state in the first year of the dispute.  ');
                                       {To drop this case, just decrement the counter so that the next dispute
                                        will be assigned to this slot.}
                                       bad_case := true;
                                    end
                                 else      {different ccodes, OK record}
                                    begin
                                       new (dyadic_dispute_data^[num_dyadic_disputes]);
                                       dyadic_dispute_data^[num_dyadic_disputes]^.record1 := x;
                                       dyadic_dispute_data^[num_dyadic_disputes]^.record2 := y;
                                       {previously, coded true initiation here.  But can now tell by using
                                        the .originator field of the individual country dispute records.}

                                       {Mark if it's multilateral}
                                       if (last_of_dispute - first_of_dispute > 0) then
                                             dyadic_dispute_data^[num_dyadic_disputes]^.others := true else
                                             dyadic_dispute_data^[num_dyadic_disputes]^.others := false;
                                       inc(num_dyadic_disputes);
                                    end;

                            end;              {for x, y}

                      {Now set up for processing of next country dispute}
                      {At the last record, this increment will put country_dispute past num_country_disputes,
                       which will trigger the exit from the loop}
                      country_dispute := last_of_dispute + 1;

                  end;    {while country_dispute <= num country disputes and num<max}

               if num_dyadic_disputes >= max_dyadic_disputes then
                    begin
                       EUGeneError ('Stopped adding dyadic disputes because hit maximum of '+ inttostr(max_dyadic_disputes)+' dyadic disputes. '+
                             'Notify programmer.',3,continue,error_log);
                    end;

               {*** this is wrong where it is}
               {num_dyadic_disputes := current_rec;  {indexed from 0 to num_dyadic_disputes - 1}
               Dyadic_dispute_trace.tickdone;
               created := true;

               if debug[4] then
                  trace.message (' required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
               trace.message('Finished reading and creating initial dyadic dispute data');


               {Sort the list of dyadic-disputes by the countries involved and by start date.}
               trace.message ('Finished constructing dyadic disputes.  Now Sorting by country 1.');
               trace.message ('       '+inttostr(num_dyadic_disputes)+ ' dyadic disputes of all types calculated');
               {writeln ('Before sorting, dyadic dispute list is');}
               {list_dyadic_disputes (dyadic_disputes, num_dyadic_disputes-1, country_disputes); }

               {First sort by first country in dyad, state 0}
               quicksort_dyadic_disputes_by_country (0, get_last_dispnum, 0);

               {writeln ('After sorting by country 1, dyadic dispute list is '); }
               { list_dyadic_disputes (dyadic_disputes, num_dyadic_disputes, country_disputes);}

               {That does overall list by first of dyad.  Now do by 2nd country in dyad, within each first}
               trace.message ('Starting sort by country 2...');
               x := 0;
               y := 0;
               repeat
                  done := false;
                  repeat
                     y := y + 1;
                     if (y >= get_last_dispnum) then done:= true
                     else if (self.get_ccode(x, 0) <>
                              self.get_ccode(y, 0)) then done:=true;
                  until done;
                  {now sorting by second country, side 1}
                  if y = get_last_dispnum then quicksort_dyadic_disputes_by_country (x, y, 1) else
                     quicksort_dyadic_disputes_by_country (x, y-1, 1);
                  x := y;
               until y > get_last_dispnum;
               {writeln ('After sorting by country 2, dyadic dispute list is ');}
               {list_dyadic_disputes (dyadic_disputes, num_dyadic_disputes, country_disputes);}

               {That did sort by first and second in dyad.  Now do by year within each dyad}
               trace.message ('Doing quicksort by year...');
               x := 0;
               y := 0;
               repeat
                  done := false;
                  repeat
                     y := y + 1;
                     if (y > get_last_dispnum) then done:= true
                     else if (self.get_ccode(x, 0) <>
                              self.get_ccode(y, 0)) or
                             (self.get_ccode(x, 1) <>
                              self.get_ccode(y, 1)) then done:=true;
                  until done;
                  if y = get_last_dispnum then quicksort_dyadic_disputes_by_first_year (x, y) else
                     quicksort_dyadic_disputes_by_first_year (x, y-1);
                  x := y;
               until y > get_last_dispnum;

               {Don'need any further sorting for now.}
               trace.message ('Finished sorting dyadic disputes.');

               {Now set the index marker to the 1st dispute for each ccode pair.  }
               {do this by running through the sorted dyads, and marking the index to the correct spot
                when the spot has not yet been set.}
               for x := 0 to num_dyadic_disputes-1 do
                  if index^[self.get_ccode(x, 0), self.get_ccode(x, 1)] = null_dispute_number then
                     index^[self.get_ccode(x, 0), self.get_ccode(x, 1)] := x;

               Dyadic_dispute_trace.tickdone;

               stored_peaceyrs.ccode1 := min_ccode;
               stored_peaceyrs.ccode2 := min_ccode;
               stored_peaceyrs.year := min_year;
               stored_peaceyrs.numyears := missing_value;

           finally
               Dyadic_dispute_trace.free;
               trace.exit('Finished initializing, reading, and sorting Dyadic dispute data');
           end;
        end;  {proc init}

            {   -----------------------  }

     destructor TCOWDyadic_dispute_data_obj_format21.destroy;
       var x : integer;
       begin
         try
            country_disputes.free;
            overall_disputes.Free;
            for x := low(dyadic_dispute_data^) to high(dyadic_dispute_data^) do
               if dyadic_dispute_data^[x] <> nil then dispose (dyadic_dispute_data^[x]);
            if dyadic_dispute_data <> nil then dispose(dyadic_dispute_data);
            dyadic_dispute_data := nil;
            created := false;
            inherited destroy;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
       end;

     function TCOWDyadic_dispute_data_obj_format21.get_MID_num (dyadic_dispute_num : longint) : dispute_id_range;
      begin
         if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
            result := country_disputes.get_disputeid(dyadic_dispute_data^[dyadic_dispute_num]^.record1);
      end;

     function TCOWDyadic_dispute_data_obj_format21.get_first_year (dyadic_dispute_num : longint) : year_range;
                   {given number of a dyadic dispute record, returns
                    the first year of overlap between the 2 participants.}
                   {We come into this function knowing that there is overlap.  }
        begin
           check_initialized;
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := min_year else
           result := max (country_disputes.get_styear(dyadic_dispute_data^[dyadic_dispute_num]^.record1),
                          country_disputes.get_styear(dyadic_dispute_data^[dyadic_dispute_num]^.record2));
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_last_year (dyadic_dispute_num : longint) : year_range;
                   {Returns the last year of overlap between the 2 participants.}
                   {We come into this function knowing that there is overlap.  }
        var temp : ccode_range;
        begin
           check_initialized;
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := min_year else
           get_last_year := min (country_disputes.get_endyear(dyadic_dispute_data^[dyadic_dispute_num]^.record1),
                                 country_disputes.get_endyear(dyadic_dispute_data^[dyadic_dispute_num]^.record2));
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_ccode (dyadic_dispute_num : longint; which_state : side_type) : ccode_range;
                   {given number of a dyadic dispute record, returns
                    the ccode of the specified particpant in the dyadic dispute}
        begin
            check_initialized;
            if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
            if which_state = 0 then
               result := country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record1)
            else if which_state=1 then
               result := country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record2);
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_hostlev_state (dyadic_dispute_num : longint; ccode: ccode_range) : hostlevtype;
        {either record1 or record2 are ccode}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record1)  = ccode) then
              get_hostlev_state := (country_disputes.get_hostlev(dyadic_dispute_data^[dyadic_dispute_num]^.record1))
           else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record2) = ccode) then
              get_hostlev_state := (country_disputes.get_hostlev(dyadic_dispute_data^[dyadic_dispute_num]^.record2))
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_originator - ccode not in record.  Notify programmer.');
           {Also, want to change coding as a "-9" hostility to a 1, which means
            no militarized response, although you were a target.}
           if (result = -9) then result := 1;
        end;
        
     function TCOWDyadic_dispute_data_obj_format21.get_sideA (dyadic_dispute_num : longint; ccode: ccode_range) : boolean;
        {either record1 or record2 are ccode}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} get_sideA := false else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record1)  = ccode) then
              get_sideA := (country_disputes.get_sideA(dyadic_dispute_data^[dyadic_dispute_num]^.record1))
           else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record2) = ccode) then
              get_sideA := (country_disputes.get_sideA(dyadic_dispute_data^[dyadic_dispute_num]^.record2))
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_sideAA - ccode not in record.  Notify programmer.');
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_revisionist (dyadic_dispute_num : longint;
              ccode: ccode_range) : boolean;
        {either record1 or record2 are ccode;  true if was revisionist in MID coding.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} get_revisionist := false else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record1)  = ccode) then
              get_revisionist := (country_disputes.get_revisionist(dyadic_dispute_data^[dyadic_dispute_num]^.record1))
           else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record2) = ccode) then
              get_revisionist := (country_disputes.get_revisionist(dyadic_dispute_data^[dyadic_dispute_num]^.record2))
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_revisionist - ccode not in record.  Notify programmer.');
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_originator (dyadic_dispute_num : longint;
              ccode: ccode_range) : boolean;
        {either record1 or record2 are ccode;  true if was originator in MID coding.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} get_originator := false else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record1)  = ccode) then
              get_originator := (country_disputes.get_originator(dyadic_dispute_data^[dyadic_dispute_num]^.record1)=true)
           else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record2) = ccode) then
              get_originator := (country_disputes.get_originator(dyadic_dispute_data^[dyadic_dispute_num]^.record2)=true)
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_originator - ccode not in record.  Notify programmer.');
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_styear (dyadic_dispute_num : longint; ccode: ccode_range) : year_range;
        {either record1 or record2 are ccode;  returns MID coding start year.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} get_styear := min_year else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record1)  = ccode) then
              get_styear := (country_disputes.get_styear(dyadic_dispute_data^[dyadic_dispute_num]^.record1))
           else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record2) = ccode) then
              get_styear := (country_disputes.get_styear(dyadic_dispute_data^[dyadic_dispute_num]^.record2))
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_styear - ccode not in record.  Notify programmer.');
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_stmonth (dyadic_dispute_num : longint; ccode: ccode_range) : month_range;
        {either record1 or record2 are ccode;  returns MID coding start month.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} get_stmonth := 0 else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record1)  = ccode) then
              get_stmonth := (country_disputes.get_stmonth(dyadic_dispute_data^[dyadic_dispute_num]^.record1))
           else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record2) = ccode) then
              get_stmonth := (country_disputes.get_stmonth(dyadic_dispute_data^[dyadic_dispute_num]^.record2))
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_stmonth - ccode not in record.  Notify programmer.');
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_stday (dyadic_dispute_num : longint; ccode: ccode_range) : MIDdaytype;
        {either record1 or record2 are ccode;  returns MID coding start day.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} get_stday := 0 else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record1)  = ccode) then
              get_stday := (country_disputes.get_stday(dyadic_dispute_data^[dyadic_dispute_num]^.record1))
           else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record2) = ccode) then
              get_stday := (country_disputes.get_stday(dyadic_dispute_data^[dyadic_dispute_num]^.record2))
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_stday - ccode not in record.  Notify programmer.');
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_endyear (dyadic_dispute_num : longint; ccode: ccode_range) : year_range;
        {either record1 or record2 are ccode;  returns MID coding end year.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} get_endyear := min_year else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record1)  = ccode) then
              get_endyear := (country_disputes.get_endyear(dyadic_dispute_data^[dyadic_dispute_num]^.record1))
           else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record2) = ccode) then
              get_endyear := (country_disputes.get_endyear(dyadic_dispute_data^[dyadic_dispute_num]^.record2))
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_endyear - ccode not in record.  Notify programmer.');
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_endmonth (dyadic_dispute_num : longint; ccode: ccode_range) : month_range;
        {either record1 or record2 are ccode;  returns MID coding end month.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} get_endmonth := 0 else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record1)  = ccode) then
              get_endmonth := (country_disputes.get_endmonth(dyadic_dispute_data^[dyadic_dispute_num]^.record1))
           else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record2) = ccode) then
              get_endmonth := (country_disputes.get_endmonth(dyadic_dispute_data^[dyadic_dispute_num]^.record2))
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_endmonth - ccode not in record.  Notify programmer.');
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_endday (dyadic_dispute_num : longint; ccode: ccode_range) : MIDdaytype;
        {either record1 or record2 are ccode;  returns MID coding end day.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} get_endday := 0 else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record1)  = ccode) then
              get_endday := (country_disputes.get_endday(dyadic_dispute_data^[dyadic_dispute_num]^.record1))
           else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record2) = ccode) then
              get_endday := (country_disputes.get_endday(dyadic_dispute_data^[dyadic_dispute_num]^.record2))
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_endday - ccode not in record.  Notify programmer.');
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_revtype (dyadic_dispute_num : longint; ccode: ccode_range) : revisiontype;
        {either record1 or record2 are ccode;  returns MID coding of revision type sought.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} get_revtype := 0 else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record1)  = ccode) then
              get_revtype := (country_disputes.get_revtype(dyadic_dispute_data^[dyadic_dispute_num]^.record1))
           else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record2) = ccode) then
              get_revtype := (country_disputes.get_revtype(dyadic_dispute_data^[dyadic_dispute_num]^.record2))
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_revtype - ccode not in record.  Notify programmer.');
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_fatality_state (dyadic_dispute_num : longint; ccode: ccode_range) : fatalitytype;
        {either record1 or record2 are ccode;  returns MID fatality coding.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} get_fatality_state := 0 else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record1)  = ccode) then
              get_fatality_state := (country_disputes.get_fatality(dyadic_dispute_data^[dyadic_dispute_num]^.record1))
           else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record2) = ccode) then
              get_fatality_state := (country_disputes.get_fatality(dyadic_dispute_data^[dyadic_dispute_num]^.record2))
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_fatality - ccode not in record.  Notify programmer.');
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_HiAct_state (dyadic_dispute_num : longint; ccode: ccode_range) : Actiontype;
        {either record1 or record2 are ccode;  returns highest action in MID coding.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} get_HiAct_state := 0 else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record1)  = ccode) then
              get_HiAct_state := (country_disputes.get_HiAct(dyadic_dispute_data^[dyadic_dispute_num]^.record1))
           else
           if (country_disputes.get_ccode(dyadic_dispute_data^[dyadic_dispute_num]^.record2) = ccode) then
              get_HiAct_state := (country_disputes.get_HiAct(dyadic_dispute_data^[dyadic_dispute_num]^.record2))
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_HiAct - ccode not in record.  Notify programmer.');
        end;


     function TCOWDyadic_dispute_data_obj_format21.get_MID_name (dyadnum : longint) : string;
        begin
           if dyadnum = null_dispute_number then {no dispute} result := '' else
           result := overall_disputes.get_dispute_name(get_mid_num(dyadnum));
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_MID_numstates (dyadnum : longint; ccode: ccode_range) : integer;
        begin
           if dyadnum = null_dispute_number then {no dispute} get_MID_numstates := 0 else
           if get_sideA(dyadnum, ccode) = true then
              get_MID_numstates := overall_disputes.get_numberA(get_mid_num(dyadnum))
              else get_MID_numstates := overall_disputes.get_numberB(get_mid_num(dyadnum));
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_MID_outcome (dyadnum : longint) : outcometype;
        begin
           if dyadnum = null_dispute_number then {no dispute} get_MID_outcome := 0 else
           get_MID_outcome := overall_disputes.get_outcome(get_mid_num(dyadnum));
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_MID_settlement (dyadnum : longint) : settlementtype;
        begin
           if dyadnum = null_dispute_number then {no dispute} get_MID_settlement := 0 else
           get_MID_settlement := overall_disputes.get_settlement(get_mid_num(dyadnum));
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_MID_fatality (dyadnum : longint) : fatalitytype;
        begin
           if dyadnum = null_dispute_number then {no dispute} get_MID_fatality := 0 else
           get_MID_fatality := overall_disputes.get_fatality_dispute(get_mid_num(dyadnum));
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_MID_hiact (dyadnum : longint) : actiontype;
        begin
           if dyadnum = null_dispute_number then {no dispute} get_MID_hiact := 0 else
           get_MID_hiact := overall_disputes.get_hiact_dispute(get_mid_num(dyadnum));
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_MID_hostlev (dyadnum : longint) : hostlevtype;
        begin
           if dyadnum = null_dispute_number then {no dispute} get_MID_hostlev := 0 else
           get_MID_hostlev := overall_disputes.get_hostlev_dispute(get_mid_num(dyadnum));
        end;

     function TCOWDyadic_dispute_data_obj_format21.get_MID_reciprocated (dyadnum : longint) : boolean;
        begin
           if dyadnum = null_dispute_number then {no dispute} get_MID_reciprocated := false else
           get_MID_reciprocated := overall_disputes.get_reciprocated(get_mid_num(dyadnum));
        end;

        {  ------------------------------------------------  }

        {Now, format 3.0 dyadic dispute object.}
     constructor TDyadic_dispute_data_obj_integrated_format30.init (user_selections : user_selection_type; configuration :
                       configuration_type; year1, year2 : year_range;
                       use_cow_or_maoz_pre_1992 : cow_maoz_1992_type);

         {sort by nondirected dyad #, then start date, taking into account the sorted/-9 date incident planning.}
        type tdl_array_type = array of format_30_dyad_record;
        var Dyadic_dispute_trace : TTrace_obj;
            heapneeded, start_mem : longint;
            start_incident_rec, end_incident_rec, target_incident_record, temp_end_incident : integer;
            start_mid_rec, end_mid_rec : integer;
            x, y : integer;
            done_mid, done_incident, done_mid_end, new_dyad : boolean;
            temp_dyad_list : tdl_array_type;
            temp_dyad_rec, final_dyadic_mid, blank_dyad_rec : format_30_dyad_record;
            final_dyadic_mid_ptr : format_30_dyad_record_ptr;
            issue_set : set of 0..4;
            country_disp_numA, country_disp_numB, first_of_dispute, last_of_dispute : integer;
            done : boolean;
            which_earlier : integer;
            identified_because_more_precise : boolean;
            MIDB_HostLevA, MIDB_HostLevB : hostlevtype;
            this_ccode : ccode_range;
            x_loop: Integer;
            country_disp_num_to_check: Integer;
            sub_mid_number : integer;
            temp_done : boolean;
            temp_matching_country_recA, temp_matching_country_recB: integer;
            matching_country_rec_array, blank_matching_country_rec_array : array [min_ccode..max_ccode] of integer;
            next_mid_ok: Boolean;
  extension_done: boolean;

        procedure Sort_temp_dyad_list_by_dyad_and_startdate (temp_dyad_list : tdl_array_type);
          {Sorts dyad list by nondir dyad, then by start date.}
        var x, y, first_in_dyad, last_in_dyad : integer;
            done_dyad : boolean;
            id_precise : boolean;
        begin
           for x := low(temp_dyad_list) to high(temp_dyad_list) - 1 do
              for y := x + 1 to high(temp_dyad_list) do
                 if temp_dyad_list[y].nondirected_dyad_number < temp_dyad_list[x].nondirected_dyad_number then
                    begin    {switch}
                       temp_dyad_rec := temp_dyad_list[y];
                       temp_dyad_list[y] := temp_dyad_list[x];
                       temp_dyad_list[x] := temp_dyad_rec;
                    end;
           {Now sort by dates within dyad}
           first_in_dyad := low(temp_dyad_list);
           {Find a dyad, and repeat for all dyads}
           repeat
              {find last in this dyad}
              last_in_dyad := first_in_dyad;
              repeat
                 inc (last_in_dyad);
                 if (last_in_dyad >  high (temp_dyad_list)) then done_dyad := true
                 else if (temp_dyad_list[last_in_dyad].nondirected_dyad_number <> temp_dyad_list[first_in_dyad].nondirected_dyad_number) then done_dyad := true
                 else done_dyad := false;
              until done_dyad;
              dec(last_in_dyad);
              {now sort by date within this dyad.}
              for x := first_in_dyad to last_in_dyad - 1 do
                 for y := x + 1 to last_in_dyad do
                    if which_earlier_date (temp_dyad_list[x].StYear, temp_dyad_list[x].StMonth, temp_dyad_list[x].StDay, temp_dyad_list[x].incident_idnum,
                                           temp_dyad_list[y].StYear, temp_dyad_list[y].StMonth, temp_dyad_list[y].StDay, temp_dyad_list[y].incident_idnum, id_precise) = 2 then
                       begin    {switch}
                          temp_dyad_rec := temp_dyad_list[y];
                          temp_dyad_list[y] := temp_dyad_list[x];
                          temp_dyad_list[x] := temp_dyad_rec;
                       end;
              first_in_dyad := last_in_dyad + 1;
           until last_in_dyad >  high (temp_dyad_list);
        end;    {proc sort}

        function low_issue (which : integer; aset : set_04_type) : revisiontype;
           var ordered_list : array[1..5] of 0..5;
               temp, temp2, temp3 : integer;
            begin
               {If which is 1, returns first lowest of 1-2-3-4-0 in that order.
                If which is 2, returns 2nd lowest.
                If which is 3, returns 3rd lowest.}
               for temp := 1 to 5 do ordered_list[temp] := 5;
               if 1 in aset then ordered_list[1] := 1;
               if 2 in aset then ordered_list[2] := 2;
               if 3 in aset then ordered_list[3] := 3;
               if 4 in aset then ordered_list[4] := 4;
               {if 0 in aset, don't do anything, because the remaining 5's will become 0s.}
               {Now sort the list.}
               for temp := 1 to 4 do
                  for temp2 := temp + 1 to 5 do
                     if (ordered_list[temp2] >= 1) and (ordered_list[temp2] <= 4) and
                        (ordered_list[temp2] < ordered_list[temp]) then   {switch}
                        begin
                           temp3 := ordered_list[temp];
                           ordered_list[temp] := ordered_list[temp2];
                           ordered_list[temp2] := temp3;
                        end;
               {now change 5s to 0s, these are the cases where values were not replaced by 1..4}
               for temp := 1 to 5 do
                  if ordered_list[temp] = 5 then ordered_list[temp] := 0;
               {now, just return spot # 1, 2, 3, etc.}
               result := ordered_list[which];
            end;


         procedure quicksort_dyadic_disputes_by_country (left, right:integer; sort_dyad:side_type);
                  {Sorts in order low to high ccode between left and right;
                   Will sort by country 1 or country 2 depending on value of sort_dyad.
                   Note that sort_dyad is not same as sideA.  It just sorts by 1st or 2nd, sideA
                   only has to do with the (previous) creation of dyads.}
            const trace_quicksort = false;
            var
               base_ccode, i, j, comp_ccode : integer;
               temp: format_30_dyad_record;
            begin
             if right > left then
               begin
                  {v needs to be set to value of right case, ccode of country 1 or ccode of country 2}
                  {dyadic_disputes^[right].pointer1 is case in country_disputes of first ccode}
                  base_ccode := self.get_ccode(right, sort_dyad);
                  i := left - 1;
                  j := right;
                  repeat
                     repeat
                        i := i + 1;
                        comp_ccode := self.get_ccode(i, sort_dyad);
                     until comp_ccode >= base_ccode;
                     repeat
                        j := j - 1;
                        comp_ccode := self.get_ccode(j, sort_dyad);
                     until (comp_ccode <= base_ccode) or (j = left);
                     temp := dyadic_dispute_data^[i]^;
                     dyadic_dispute_data^[i]^ := dyadic_dispute_data^[j]^;
                     dyadic_dispute_data^[j]^ := temp;
                  until j <= i;
                  dyadic_dispute_data^[j]^ := dyadic_dispute_data^[i]^;
                  dyadic_dispute_data^[i]^ := dyadic_dispute_data^[right]^;
                  dyadic_dispute_data^[right]^ := temp;
                  quicksort_dyadic_disputes_by_country (left, i-1, sort_dyad);
                  quicksort_dyadic_disputes_by_country (i+1, right, sort_dyad);
               end;
            end;      {procedure quicksort_dyads_by_country}

            {   -----------------------  }

         procedure quicksort_dyadic_disputes_by_first_year (left, right:integer);
                  {Sorts in order low to high}
            var
               base_year, comp_year, i, j: integer;
               temp: format_30_dyad_record;

            begin
             if right > left then
               begin
                  base_year := self.get_first_year (right);
                  i := left - 1;
                  j := right;
                  repeat
                     repeat
                        i := i + 1;
                        comp_year := self.get_first_year (i);
                     until comp_year >= base_year;
                     repeat
                        j := j - 1;
                        comp_year := self.get_first_year (j);
                     until (comp_year <= base_year) or (j = left);
                     temp := dyadic_dispute_data^[i]^;
                     dyadic_dispute_data^[i]^ := dyadic_dispute_data^[j]^;
                     dyadic_dispute_data^[j]^ := temp;
                  until j <= i;
                  dyadic_dispute_data^[j]^ := dyadic_dispute_data^[i]^;
                  dyadic_dispute_data^[i]^ := dyadic_dispute_data^[right]^;
                  dyadic_dispute_data^[right]^ := temp;
                  quicksort_dyadic_disputes_by_first_year (left, i-1);
                  quicksort_dyadic_disputes_by_first_year (i+1, right);
               end;
            end;      {procedure quicksort_dyadic_disputes_by_first_year}


         function incident_end_matches_MIDB_end_with_multi_recs (dyadic_incident : format_30_dyad_record;
                  var matching_incident_numA, matching_incident_numB : integer) : boolean;
            {returns true if this incident has an end date matching a MID B end record,
             iff there are multiple country recs.  matching_incdent_num gives the number of the record (1st, 2nd, 3rd etc)}
            var mid_episode : integer;
            begin
               {Check to see if the end of this incident is the same as a state end in MIDB,
                but if there are more incidents for the state.
                This is a sign of a multiple entry/exit for the state.  In this case, we must
                end this dyadic MID record here, and start again at the next incident for the
                next dyadic MID.  }
              result := false;
              matching_incident_numA := -1;
              matching_incident_numB := -1;
              if (country_disputes.get_num_country_recs_for_state(dyadic_incident.idnum, dyadic_incident.ccodeA) > 1) then
                 for mid_episode := 1 to country_disputes.get_num_country_recs_for_state(dyadic_incident.idnum, dyadic_incident.ccodeA) do
                    begin
                       country_disp_num_to_check := Country_Disputes.get_country_disp_num_when_many (dyadic_incident.idnum, dyadic_incident.ccodeA, mid_episode);
                       If ((dyadic_incident.EndYear = country_disputes.get_endyear(country_disp_num_to_check)) and
                           (dyadic_incident.EndMonth = country_disputes.get_endmonth(country_disp_num_to_check)) and
                           (dyadic_incident.EndDay = country_disputes.get_endday(country_disp_num_to_check)) )
                           then
                          begin
                             {This is the case where we need to split the MID.}
                             result := true;
                             matching_incident_numA := mid_episode;
                          end;
                    end;
              if (country_disputes.get_num_country_recs_for_state(dyadic_incident.idnum, dyadic_incident.ccodeB) > 1) then
                 for mid_episode := 1 to country_disputes.get_num_country_recs_for_state(dyadic_incident.idnum, dyadic_incident.ccodeB) do
                    begin
                       country_disp_num_to_check := Country_Disputes.get_country_disp_num_when_many (dyadic_incident.idnum, dyadic_incident.ccodeB, mid_episode);
                       If ((dyadic_incident.EndYear = country_disputes.get_endyear(country_disp_num_to_check)) and
                           (dyadic_incident.EndMonth = country_disputes.get_endmonth(country_disp_num_to_check)) and
                           (dyadic_incident.EndDay = country_disputes.get_endday(country_disp_num_to_check)) )
                           then
                          begin
                             {This is the case where we need to split the MID.}
                             result := true;
                             matching_incident_numB := mid_episode;
                          end;
                    end;

            end;  {proc incident_end_matches_MIDB_end}

        begin              {main code for .init for dispute_integrated}
           {Need to read in country_disputes, overall_disputes, and participant incident data.
            These are the raw ingredients.
            Then will compute dyadic disputes differently for the first part and 2nd part
            of the time period.}
           added_cow_or_maoz := added_none;
           country_disputes := nil;
           overall_disputes := nil;
           participant_incident_data := nil;
           dyadic_dispute_trace := nil;
           for x := min_ccode to max_ccode do blank_matching_country_rec_array[x] := -1;
           matching_country_rec_array := blank_matching_country_rec_array;
           with blank_dyad_rec do
              begin
                  idnum := 0;
                  dyadic_idnum := 0;
                  incident_idnum := null_dispute_number;
                  ccodeA := min_ccode;
                  ccodeB := min_ccode;
                  nondirected_dyad_number := initialized_value;
                  StDay := 0;
                  EndDay:= 0;
                  StMonth := 0;
                  EndMonth := 0;
                  StYear := min_year;
                  EndYear := min_year;
                  Fatality := 0;
                  Outcome := 0;
                  Settlement := 0;
                  FatalityA := 0;
                  FatalityB := 0;
                  FatalityVA := 0;
                  FatalityVB := 0;
                  HiAct:= 0;
                  HiActA := 0;
                  HiActB := 0;
                  HostLev := 0;
                  HostLevA := 0;
                  HostLevB := 0;
                  ReciprocatedMID := false;
                  NumberA := 0;
                  NumberB := 0;
                  OriginatorA := false;
                  OriginatorB := false;
                  revisionistA := false;
                  RevType_A1 := 0;
                  RevType_A2 := 0;
                  revisionistB := false;
                  RevType_B1 := 0;
                  RevType_B2 := 0;
                  issue_setA := [];
                  issue_setB := [];
                  SideA := false;
                  SideB := false;
                  SIDEAA_dyadic := false;
                  SIDEAB_dyadic := false;
                  RoleA := 0;
                  RoleB := 0;
                  version := 0;
                  COWWar := false;
                  Durindx := 0;
                  DurDays := 0;
              end;

           final_dyadic_mid := blank_dyad_rec;

           try
              trace.enter('Initializing dyadic dispute data, version 3.0 format,');
              if year1 > year2 then switch_year (year1, year2);
              self.first_partition_year := year1;
              self.last_partition_year := year2;

              start_mem := memavail;
              heapneeded := TCOWdyadic_dispute_obj_mem_overhead;
              if debug[4] then
                  begin
                     trace.message ('Dyadic Dispute array size calculation');
                     trace.message ('Calc is that '+inttostr(heapneeded)+' needed for all years.');
                     trace.message ('Max avail mem block is '+inttostr(MaxAvail));
                  end;
              if MaxAvail <= (heapneeded) then
                     EUGeneError ('Not enough memory for dyadic dispute array. ',
                                     5, stop, error_log);

               {First, must read/create country dispute data. This is MIDB info.  }
              country_disputes := TCountry_dispute_data_obj.init (configuration.cow_mid_actor_file_nameB, configuration.cow_mid_data_format);
              if not(country_disputes.initialized) then
                     EUGeneError ('Country_dispute initialization not completed succesfully during dyadid dispute init. ',
                                     5, stop, error_log);

               {Also, now, must read in overall dispute information.  This is MIDA, MIDC info.}
              overall_disputes := Toverall_dispute_data_obj.init (configuration.cow_mid_case_file_nameA,
                                                           configuration.cow_mid_name_file_nameC, configuration.cow_mid_data_format);
              if not(overall_disputes.initialized) then
                     EUGeneError ('Overall_dispute initialization not completed succesfully during dyadid dispute init. ',
                                     5, stop, error_log);

              participant_incident_data := Tparticipant_incident_data_obj.init(configuration.cow_mid_participant_incident_file_name);
              if not(participant_incident_data.initialized) then
                     EUGeneError ('participant_incident_data initialization not completed succesfully during dyadid dispute init. ',
                                     5, stop, error_log);

              Dyadic_dispute_trace := TTrace_obj.init(trace.get_trace_level);
              trace.message ('Initializing Dyadic dispute data structure');

              {Don't actually initialize full dyadic structure, it's dynamic now}
              new (dyadic_dispute_data);
              setlength (dyadic_dispute_data^, 0);
              {for x := 0 to max_dyadic_disputes-1 do dyadic_dispute_data^[x] := nil; }
              
              num_dyadic_disputes := 0;

              {initialize index}
              new (index);
              for x := 1 to top_nation_num do
                 for y := 1 to top_nation_num do
                    index^[x,y] := null_dispute_number;

              for x := low(dispute_id_range) to high(dispute_id_range) do
                 pi_midnum_list[x] := false;

              {With the raw data, need to compute dyadic disputes, here, from PI data.
               Then will need to check in case it's a war to see if hostility levels need to be overridden.
               Also, overall MID dates may be more precise than PI data, so need to check MIDB state
                 participation dates to see if more precise than PI.
               Also, some MID participations by states are broken into two or more episodes.
                 So if MIDB shows an entry/exit for a state, then its dyadic disputes need to be
                 broken up.  }

              {Now process the participant-incident data to compute MIDs}
              {outer loop processes each mid;  inner goes through each incident.
               Each incident has several participants (but only 1 target).}
              start_mid_rec := 0;
              start_incident_rec := 0;
              repeat
                 done_mid := false;
                 {Find first and last records of this MID}
                 end_mid_rec := start_mid_rec;
                 done_mid_end := false;
                 repeat
                    inc (end_mid_rec);
                    if end_mid_rec > participant_incident_data.num_participant_incident_records then done_mid_end := true
                    else if participant_incident_data.get_mid_idnum(end_mid_rec)<>participant_incident_data.get_mid_idnum(start_mid_rec) then done_mid_end := true
                 until done_mid_end;
                 dec (end_mid_rec);
                 {Now have first and last incident of this MID}

                 setlength(temp_dyad_list, 0);
                 {Now process each incident.}
                 repeat
                    {Find first and last records (participant-incident recs) of this incident}
                    end_incident_rec := start_incident_rec;
                    done_incident := false;
                    repeat
                       inc (end_incident_rec);
                       if (end_incident_rec > participant_incident_data.num_participant_incident_records) then done_incident := true
                       else if (participant_incident_data.get_mid_incident_idnum(end_incident_rec)<>participant_incident_data.get_mid_incident_idnum(start_incident_rec)) then done_incident := true
                       else done_incident := false;
                    until done_incident;
                    dec(end_incident_rec);

                    if start_incident_rec=end_incident_rec then
                       EUGeneError ('Two incidents with same incident num not found, Incident #s are '+inttostr(participant_incident_data.get_mid_incident_idnum(start_incident_rec)) +' and '+ inttostr(participant_incident_data.get_mid_incident_idnum(end_incident_rec+1)) +'.  Notify programmer.', 1, continue, error_log);

                    {create dyadic incident records}
                    {first figure out the target - do this with incident-side-A (insideA) var.}
                    target_incident_record := missing_value;
                    for x := start_incident_rec to end_incident_rec do
                       if participant_incident_data.get_incident_sideA(x)=false then {this is the target record}
                          target_incident_record := x;
                    if target_incident_record = missing_value then
                       EUGeneError ('Incident '+inttostr(x) +' has no target - need to correct program.  Notify programmer.', 1, continue, error_log);

                    {Now process each participant record of this incident, and create all dyadic incidents.}
                    for x := start_incident_rec to end_incident_rec do
                       {If this record is not the target record, create a dyad record of it with the target.
                        Note - this is currently creating directed MID records, SideA vs. SideB.}
                       if x <> target_incident_record then
                          begin
                             temp_dyad_rec := blank_dyad_rec;
                             temp_dyad_rec.idnum := participant_incident_data.get_mid_idnum(x);
                             temp_dyad_rec.incident_idnum := participant_incident_data.get_mid_incident_idnum(x);
                             temp_dyad_rec.ccodeA := participant_incident_data.get_ccode(x);
                             temp_dyad_rec.ccodeB := participant_incident_data.get_ccode(target_incident_record);
                             temp_dyad_rec.nondirected_dyad_number := 1000 * min (participant_incident_data.get_ccode(x), participant_incident_data.get_ccode(target_incident_record))
                                                        + max (participant_incident_data.get_ccode(x), participant_incident_data.get_ccode(target_incident_record));
                             {Within the incident, start date should be the later of the 2 state
                              dates, because there are some where not all states in the incident have
                              the same dates.  Similarly, take earlier of the two precise end dates.}
                             {Take the earliest precise start date, and latest precise end date.}
                             which_earlier := which_earlier_date (participant_incident_data.get_styear(x), participant_incident_data.get_stmonth(x), participant_incident_data.get_stday(x), participant_incident_data.get_mid_incident_idnum(x),
                                              participant_incident_data.get_styear(target_incident_record), participant_incident_data.get_stmonth(target_incident_record), participant_incident_data.get_stday(target_incident_record), participant_incident_data.get_mid_incident_idnum(target_incident_record),
                                              identified_because_more_precise);
                             {If 1st is earlier, take target.
                                 But, if 1st is more precise, then use that.}
                             if identified_because_more_precise then
                                begin
                                   if which_earlier = 1 then
                                      begin
                                         temp_dyad_rec.StDay := participant_incident_data.get_stday(x);
                                         temp_dyad_rec.StMonth := participant_incident_data.get_stmonth(x);
                                         temp_dyad_rec.StYear := participant_incident_data.get_styear(x);
                                      end
                                   else    {which_earlier=2}
                                      begin
                                         temp_dyad_rec.StDay := participant_incident_data.get_stday(target_incident_record);
                                         temp_dyad_rec.StMonth := participant_incident_data.get_stmonth(target_incident_record);
                                         temp_dyad_rec.StYear := participant_incident_data.get_styear(target_incident_record);
                                      end
                                end
                             else   {identified not because of precision, but by start/end date.
                                     so take opposite start record.}
                                begin
                                   if which_earlier = 1 then
                                       {take target rec as start date}
                                      begin
                                         temp_dyad_rec.StDay := participant_incident_data.get_stday(target_incident_record);
                                         temp_dyad_rec.StMonth := participant_incident_data.get_stmonth(target_incident_record);
                                         temp_dyad_rec.StYear := participant_incident_data.get_styear(target_incident_record);
                                      end
                                   else    {which_earlier=2}
                                      begin
                                       {take 1st rec as start date}
                                         temp_dyad_rec.StDay := participant_incident_data.get_stday(x);
                                         temp_dyad_rec.StMonth := participant_incident_data.get_stmonth(x);
                                         temp_dyad_rec.StYear := participant_incident_data.get_styear(x);
                                      end
                                end;
                             {Neither of these is actually used any more.}
                             {temp_dyad_rec.inexact_start_date := (temp_dyad_rec.StDay = -9);
                             {if temp_dyad_rec.inexact_start_date then temp_day := 15 else temp_day := temp_dyad_rec.StDay;
                             temp_dyad_rec.internal_start_date := encodedate (temp_dyad_rec.StYear, temp_dyad_rec.StMonth, temp_day); }

                             {Now end date.  Take first of the 2 end dates.}
                             which_earlier := which_earlier_date (participant_incident_data.get_endyear(x), participant_incident_data.get_endmonth(x), participant_incident_data.get_endday(x), participant_incident_data.get_mid_incident_idnum(x),
                                              participant_incident_data.get_endyear(target_incident_record), participant_incident_data.get_endmonth(target_incident_record), participant_incident_data.get_endday(target_incident_record), participant_incident_data.get_mid_incident_idnum(target_incident_record),
                                              identified_because_more_precise);
                             {If 1st is later, take target.
                                 But, if 1st is more precise, then use that.}
                             if identified_because_more_precise then
                                begin
                                   if which_earlier = 1 then
                                      begin
                                         temp_dyad_rec.EndDay := participant_incident_data.get_endday(x);
                                         temp_dyad_rec.endMonth := participant_incident_data.get_endmonth(x);
                                         temp_dyad_rec.EndYear := participant_incident_data.get_endyear(x);
                                      end
                                   else    {which_earlier=2}
                                      begin
                                         temp_dyad_rec.EndDay := participant_incident_data.get_endday(target_incident_record);
                                         temp_dyad_rec.EndMonth := participant_incident_data.get_endmonth(target_incident_record);
                                         temp_dyad_rec.EndYear := participant_incident_data.get_endyear(target_incident_record);
                                      end
                                end
                             else   {identified not because of precision, but by start/end date.
                                     so take opposite start record.}
                                begin
                                   if which_earlier = 1 then
                                       {take 1st rec as end date}
                                      begin
                                         temp_dyad_rec.EndDay := participant_incident_data.get_endday(x);
                                         temp_dyad_rec.endMonth := participant_incident_data.get_endmonth(x);
                                         temp_dyad_rec.EndYear := participant_incident_data.get_endyear(x);
                                      end
                                   else    {which_earlier=2}
                                      begin
                                       {take 2nd rec as end date}
                                         temp_dyad_rec.EndDay := participant_incident_data.get_endday(target_incident_record);
                                         temp_dyad_rec.EndMonth := participant_incident_data.get_endmonth(target_incident_record);
                                         temp_dyad_rec.EndYear := participant_incident_data.get_endyear(target_incident_record);
                                      end
                                end;

                             {temp_dyad_rec.inexact_end_date := (temp_dyad_rec.EndDay = -9);
                             {if temp_dyad_rec.inexact_end_date then temp_day := 15 else temp_day := temp_dyad_rec.EndDay;
                             temp_dyad_rec.internal_end_date := encodedate (temp_dyad_rec.EndYear, temp_dyad_rec.EndMonth, temp_day);  }

                             temp_dyad_rec.HiActA := participant_incident_data.get_Action(x);
                             temp_dyad_rec.HiActB := participant_incident_data.get_Action(target_incident_record);
                             temp_dyad_rec.HostLevA := participant_incident_data.get_HostLev(x);
                             temp_dyad_rec.HostLevB := participant_incident_data.get_HostLev(target_incident_record);

                             {compute if revisionist, and types of revisions, revtype (issues) here}
                             temp_dyad_rec.revisionistA := (participant_incident_data.get_issue1(x)<>0)
                                                        or (participant_incident_data.get_issue2(x)<>0)
                                                        {or (participant_incident_data.get_issue3(x)<>0)};
                             if participant_incident_data.get_issue1(x)<>-9 then include (temp_dyad_rec.issue_setA, participant_incident_data.get_issue1(x));
                             if participant_incident_data.get_issue2(x)<>-9 then include (temp_dyad_rec.issue_setA, participant_incident_data.get_issue2(x));
                             {if participant_incident_data.get_issue3(x)<>-9 then include (temp_dyad_rec.issue_setA, participant_incident_data.get_issue3(x));}
                             temp_dyad_rec.revisionistB := (participant_incident_data.get_issue1(target_incident_record)<>0)
                                                        or (participant_incident_data.get_issue2(target_incident_record)<>0)
                                                        {or (participant_incident_data.get_issue3(target_incident_record)<>0)};
                             if participant_incident_data.get_issue1(target_incident_record)<>-9 then include (temp_dyad_rec.issue_setB, participant_incident_data.get_issue1(target_incident_record));
                             if participant_incident_data.get_issue2(target_incident_record)<>-9 then include (temp_dyad_rec.issue_setB, participant_incident_data.get_issue2(target_incident_record));
                             temp_dyad_rec.version := participant_incident_data.get_version(x);


                             {Now created a (directed) dyad.  Add it to list.}
                             setlength(temp_dyad_list, length(temp_dyad_list)+1);
                             temp_dyad_list[high(temp_dyad_list)] := temp_dyad_rec;
                             inc(num_dyadic_disputes);
                          end;       {for each record of incident.}
                    {Now processed this incident, and dyadized it.}

                    {Now move on to the next incident}
                    start_incident_rec := end_incident_rec + 1;
                    if (start_incident_rec >  participant_incident_data.num_participant_incident_records) then done_mid := true
                       else if (participant_incident_data.get_mid_idnum(start_incident_rec)<>participant_incident_data.get_mid_idnum(start_mid_rec)) then done_mid := true
                       else done_mid := false;
                 until (done_mid);

                 {At this point, I have dyadized each incident in this MID.
                  Sort the incidents by dyad and by start date, and process them to get just 1 dyadic MID record.
                  Currently, the temp_dyad_list has many records for each dyadic MID.}

                 {create one or more dyadic MIDs here, depending on if MIDB says a state leaves,
                  or if it's continuous across incidents. }
                 {But note:  although usually there will just be 1 dyadic MID per dyad, there are times that
                  states enter/exit a MID multiple times.  We can only know this from the MIDB data.  In those
                  cases, we need multiple dyadic MIDs.}
                 Sort_temp_dyad_list_by_dyad_and_startdate (temp_dyad_list);

                 start_incident_rec := low(temp_dyad_list);

                 {reset the matching country rec array so can get apporpriate record #s.}
                 matching_country_rec_array := blank_matching_country_rec_array;

                 {sub_mid_number gives me the chronological order of sub-mids.}
                 sub_mid_number := 0;

                 repeat    {until (start_incident_rec > high(temp_dyad_list) );}
                    {Have start incident;  now find last dyadized incident for this dyad in this MID}
                    end_incident_rec := start_incident_rec;

                    {In at least one case, the end of the prior dyadic MID had a -9 end date,
                     but there is a subsequent record with an exact date in the same month.  In
                     this kind of case, we want to skip the next exact date, because it's included
                     within the inexact date.  So what that means is that after a -9 end,
                     I want to find the first incident in the next month (or later) and this is really
                     the next start date.}
                    {Really, this should extend the search through that -9 MID, but that may mess
                     up the matching below in terms of what is identified as end dates, so I've
                     left it here because none of the other variables are changed in practice
                     by the couple of cases that exist.}

                    if ((final_dyadic_mid.nondirected_dyad_number = temp_dyad_list[start_incident_rec].nondirected_dyad_number) and
                        (final_dyadic_mid.EndDay = -9)) then  {need to be sure next incident is next month}
                       repeat
                          next_mid_ok := false;
                          if ((temp_dyad_list[start_incident_rec].StYear > final_dyadic_mid.EndYear) or
                              ((temp_dyad_list[start_incident_rec].StYear = final_dyadic_mid.EndYear) and
                               (temp_dyad_list[start_incident_rec].StMonth > final_dyadic_mid.EndMonth)) or
                              (start_incident_rec >= high(temp_dyad_list)) ) then
                             begin
                                {This situation is fine, continue with processing}
                                next_mid_ok := true;
                             end
                             else
                                begin
                                   inc(start_incident_rec);
                                   end_incident_rec := start_incident_rec;
                                end;
                          if (final_dyadic_mid.nondirected_dyad_number <> temp_dyad_list[start_incident_rec].nondirected_dyad_number) then
                             begin
                                next_mid_ok := true;
                                ShowMessage ('Uncertain code is correct in make dyadic mids - searched for next MID past dyad boundary.');
                             end;
                       until (next_mid_ok);

                    {There may also be some other cases where there are subsequent incidents in the
                     MID IP file which had later start dates than those in the dyadic list, but
                     where the end dates are before what we've already found as the final dyadic
                     incident.  These should NOT be made into new dyadic MIDs, so I need to skip them.}

                    {in cases where it doesn't match via MID end date, the matching country rec is 1.}
                    matching_country_rec_array[temp_dyad_list[start_incident_rec].ccodeA] := 1;
                    matching_country_rec_array[temp_dyad_list[start_incident_rec].ccodeB] := 1;
                    {matching_country_recA := 1;
                    matching_country_recB := 1;  }
                    repeat
                       inc (end_incident_rec);
                       if (end_incident_rec > high(temp_dyad_list)) then
                          begin
                             done_incident := true;
                             dec(end_incident_rec);
                          end
                       {check to see if hit next nondir dyad}
                       else if (temp_dyad_list[end_incident_rec].nondirected_dyad_number<>temp_dyad_list[start_incident_rec].nondirected_dyad_number) then
                          begin
                             done_incident := true;
                             dec(end_incident_rec);
                          end
                       {also check to see if end of the start incident is MIDB end date, in which
                        case start incident is the first and last incident for the dyad.}
                       else if (incident_end_matches_MIDB_end_with_multi_recs (temp_dyad_list[start_incident_rec], temp_matching_country_recA, temp_matching_country_recB)) then
                          begin
                             done_incident := true;
                             dec(end_incident_rec);
                             if temp_matching_country_recA <> -1 then matching_country_rec_array[temp_dyad_list[start_incident_rec].ccodeA] := temp_matching_country_recA;
                             if temp_matching_country_recB <> -1 then matching_country_rec_array[temp_dyad_list[start_incident_rec].ccodeB] := temp_matching_country_recB;
                          end
                       {also check to see if end of the next incident (end_incident_rec) is MIDB end date,
                        but not at end of list.  If incident end matches midB end, but not at end of list, then
                        we have a state exit and reentry.}
                       else if ((temp_dyad_list[end_incident_rec].nondirected_dyad_number=temp_dyad_list[start_incident_rec].nondirected_dyad_number)
                                and (incident_end_matches_MIDB_end_with_multi_recs (temp_dyad_list[end_incident_rec], temp_matching_country_recA, temp_matching_country_recB))
                                and (end_incident_rec < high(temp_dyad_list)) )  then
                             begin
                                {We know here there is at least one incident where the incident end date
                                 matches the MIDB partial end date.  But be sure to have the last of
                                 such matching incidents.}
                                done_incident := true;
                                temp_end_incident := end_incident_rec;
                                if temp_matching_country_recA <> -1 then matching_country_rec_array[temp_dyad_list[end_incident_rec].ccodeA] := temp_matching_country_recA;
                                if temp_matching_country_recB <> -1 then matching_country_rec_array[temp_dyad_list[end_incident_rec].ccodeB] := temp_matching_country_recB;
                                repeat
                                   inc(temp_end_incident);
                                   if (temp_end_incident < high(temp_dyad_list)) then
                                      begin
                                         if (incident_end_matches_MIDB_end_with_multi_recs (temp_dyad_list[temp_end_incident], temp_matching_country_recA, temp_matching_country_recB)) then
                                             {continue looking, but since it matched, set matching rec found.}
                                            begin
                                               temp_done := false;
                                               if temp_matching_country_recA <> -1 then matching_country_rec_array[temp_dyad_list[temp_end_incident].ccodeA] := temp_matching_country_recA;
                                               if temp_matching_country_recB <> -1 then matching_country_rec_array[temp_dyad_list[temp_end_incident].ccodeB]  := temp_matching_country_recB;
                                            end
                                         else  {incident date did not match}
                                            begin
                                               temp_done := true;
                                               dec(temp_end_incident);
                                            end;
                                      end
                                      else   {temp incident past end of list}
                                         begin
                                            temp_done := true;
                                            dec(temp_end_incident);
                                         end;

                                until temp_done; {end date of this incident is later than this end date.}
                                   {the temp loop ends up with temp_end_incident one beyond the end_rec
                                    we want, just as before.  So assign tem_incident # back to regular
                                    end_incident_rec.}
                                end_incident_rec := temp_end_incident;

                                {There may also be cases after stopping the search for last incident
                                 because of matching MIDB where there are actually incidents that are
                                 still part of the dyadic MID, because their start date is later than
                                 the start date of the incident where the end matches MIDB, but the
                                 start or end is before the MIDB end, so they are within the MID.
                                 Need to include those.}
                                if end_incident_rec < high(temp_dyad_list) then
                                   begin
                                      extension_done := false;
                                      temp_end_incident := end_incident_rec;
                                      repeat
                                         inc(temp_end_incident);
                                         if (temp_end_incident < high(temp_dyad_list)) then
                                            begin
                                               if ((temp_dyad_list[end_incident_rec].nondirected_dyad_number = temp_dyad_list[temp_end_incident].nondirected_dyad_number) and
                                                   earlier_date(temp_dyad_list[temp_end_incident].StYear, temp_dyad_list[temp_end_incident].StMonth, temp_dyad_list[temp_end_incident].StDay, temp_dyad_list[end_incident_rec].EndYear, temp_dyad_list[end_incident_rec].EndMonth, temp_dyad_list[end_incident_rec].EndDay) ) then
                                                  begin
                                                     {This is the case where we need to extend the end incident, b/c the next incident falls before this end.}
                                                     {Verify that it ends before the MIDB end.}
                                                     if not (earlier_date(temp_dyad_list[temp_end_incident].EndYear, temp_dyad_list[temp_end_incident].EndMonth, temp_dyad_list[temp_end_incident].EndDay, temp_dyad_list[end_incident_rec].EndYear, temp_dyad_list[end_incident_rec].EndMonth, temp_dyad_list[end_incident_rec].EndDay)) then
                                                        EugeneError ('Programming error, end date of incident subsequent to MIDB-located end date is not within range determined by MIDB.  Check code.',1,continue, error_log);
                                                     {Now, this is the case where we want to extend end_incident to include this.
                                                      To do that, just keep going.}
                                                  end
                                               else
                                                  begin
                                                     {here, the case didn't match on dates or dyad, so am done extending.}
                                                     extension_done := true;
                                                     dec(temp_end_incident);
                                                  end;
                                            end
                                         else {temp incident past end of list}
                                            begin
                                               extension_done := true;
                                               dec(temp_end_incident);
                                            end;
                                      until extension_done;
                                         {the temp loop ends up with temp_end_incident one beyond the end_rec
                                          we want, just as before.  So assign tem_incident # back to regular
                                          end_incident_rec.}
                                      end_incident_rec := temp_end_incident;
                                   end; {if end < high}


                             end  {if same dyad and incident end matches MIDB end.}
                       else done_incident := false;
                    until done_incident;
                    {Now have first and last for this dyad.  Sorted by start date.}


                    {Go through each incident for the dyad here, and compile one master
                     dyadic MID for that dyad from the incidents.}

                    repeat  {until x > end_incident_rec;  }

                       {Initially set final info to be the start record.  That will set dates,
                        ccodes, nondirID, MIDnum, initial incident #, actions, hostility, for first incident.
                        If there are multiple entry/exits, then start_incident_rec will be reset (at end of
                        repeat loop).}
                       final_dyadic_mid := temp_dyad_list[start_incident_rec];
                       {For most MIDs, we will only make a pass through here once.  So, incrementing
                        sub_mid_number will make it be a "1" which means it will the first (and only)
                        dyadic MID for this mid number.  Note that sub_mid_number is not necessarily the
                        country_disp_record number for a given dyad, though.  So, we need to keep track of
                        that separately, somehow. }
                       inc(sub_mid_number);
                       final_dyadic_mid.dyadic_idnum := final_dyadic_mid.idnum * 100 + sub_mid_number;

                       {The first incident of each dyad will give us SideA for the dyadic MID.}
                       {since this is the first (temporal) incident, I can set sideA and sideB.}
                       final_dyadic_mid.SIDEAA_dyadic := true;
                       final_dyadic_mid.SIDEAB_dyadic := false;

                       {Set up for figuring out reciprocated MID.  Do this by seeing if there is
                        an incident in the opposite direction.  But also, if any incident is a clash,
                        then it would be reciprocated.}
                       final_dyadic_mid.ReciprocatedMID := false;
                       {check first incident for a clash}
                       if (final_dyadic_mid.HiActA = 17) or (final_dyadic_mid.HiActB = 17) then
                          final_dyadic_mid.ReciprocatedMID := true;

                       {Now repeat through the rest of the dyadic incidents.}
                       x := start_incident_rec+1;
                       if not (x > end_incident_rec) then
                       repeat    {until x > end_incident_rec or must split}
                          {process each other incident to set dates, up to last dyadic incident,
                           or to the point at which  we must split it.  }
                          {Check for earlier start / later end dates.  shouldn't ever have an earlier
                           start date because it's sorted above, but should check all start dates here.
                           Also check end dates, because last incident and last-starting incident won't
                           necessarily have the latest end date..}

                          {Check the start dates, put the earliest start date in the dyadic MID record}
                          if which_earlier_date (final_dyadic_mid.StYear, final_dyadic_mid.StMonth, final_dyadic_mid.StDay, final_dyadic_mid.incident_idnum,
                                                 temp_dyad_list[x].StYear, temp_dyad_list[x].StMonth, temp_dyad_list[x].StDay, temp_dyad_list[x].incident_idnum, identified_because_more_precise)
                                              = 2 then EUGeneError ('Error in creating dyadic MIDs v3.0 format; in dyadized incident list, a later-sorted incident had an earlier start date.  programming error.  Continues, but problem.',1,continue,error_log)
                          else if which_earlier_date (final_dyadic_mid.StYear, final_dyadic_mid.StMonth, final_dyadic_mid.StDay, final_dyadic_mid.incident_idnum,
                                                 temp_dyad_list[x].StYear, temp_dyad_list[x].StMonth, temp_dyad_list[x].StDay, temp_dyad_list[x].incident_idnum, identified_because_more_precise)
                                              = 1 then
                             begin
                                {this is normal - the first MID has the 1st start date, and since MID with index 0 went into the temp
                                 record, then this newer start date is already in.  So do nothing.}
                             end;
                          {Now check the end dates, put the latest end date in the dyadic MID record}
                          if which_earlier_date (final_dyadic_mid.EndYear, final_dyadic_mid.EndMonth, final_dyadic_mid.EndDay, final_dyadic_mid.incident_idnum,
                                                 temp_dyad_list[x].EndYear, temp_dyad_list[x].EndMonth, temp_dyad_list[x].EndDay, temp_dyad_list[x].incident_idnum, identified_because_more_precise)
                                              = 1 then
                             begin
                                {record in final_dyadic_mid has earlier end date than record x, so
                                 x is later.  This will often happen - 2nd record (record x) will have
                                 later end date.  So update the end date}
                                final_dyadic_mid.EndYear := temp_dyad_list[x].EndYear;
                                final_dyadic_mid.EndMonth := temp_dyad_list[x].EndMonth;
                                final_dyadic_mid.EndDay := temp_dyad_list[x].EndDay;
                                {final_dyadic_mid.inexact_end_date := temp_dyad_list[x].inexact_end_date;
                                final_dyadic_mid.internal_end_date := temp_dyad_list[x].internal_end_date; }
                             end
                          else if which_earlier_date (final_dyadic_mid.EndYear, final_dyadic_mid.EndMonth, final_dyadic_mid.EndDay, final_dyadic_mid.incident_idnum,
                                                 temp_dyad_list[x].EndYear, temp_dyad_list[x].EndMonth, temp_dyad_list[x].EndDay, temp_dyad_list[x].incident_idnum, identified_because_more_precise)
                                              = 2 then
                             begin
                                {here, record x is the earlier date;  record in final_dyadic_mid is later,
                                 so do not need to change anything.}
                             end
                          else EUGeneError ('Unrecognized response from which_earlier_date in create mids from pi data.  Notify programmer.',1,continue,error_log);

                          {Check for higher highest action, hostility level.
                           Here, need to be sure I update the proper direction of the dyadic MID.}
                          if (final_dyadic_mid.ccodeA = temp_dyad_list[x].ccodeA) then
                             begin   {check A vs. A, B vs. B}
                                if not (final_dyadic_mid.ccodeB = temp_dyad_list[x].ccodeB) then EUGeneError ('Error in dyadic MID 3.0 creation, saw A=A and not B=B when checking hostlevs.  continuing, but data/programming error.',1,continue,error_log);
                                if temp_dyad_list[x].HiActA > final_dyadic_mid.HiActA then
                                   final_dyadic_mid.HiActA := temp_dyad_list[x].HiActA;
                                if temp_dyad_list[x].HiActB > final_dyadic_mid.HiActB then
                                   final_dyadic_mid.HiActB := temp_dyad_list[x].HiActB;
                                if temp_dyad_list[x].HostLevA > final_dyadic_mid.HostLevA then
                                   final_dyadic_mid.HostLevA := temp_dyad_list[x].HostLevA;
                                if temp_dyad_list[x].HostLevB > final_dyadic_mid.HostLevB then
                                   final_dyadic_mid.HostLevB := temp_dyad_list[x].HostLevB;
                             end
                          else
                             begin   {Check A vs. B, and B vs. A}
                                if not ( (final_dyadic_mid.ccodeB = temp_dyad_list[x].ccodeA) and (final_dyadic_mid.ccodeA = temp_dyad_list[x].ccodeB) ) then
                                   EUGeneError ('Error in dyadic MID 3.0 creation, saw A=A and not B=B when checking hostlevs.  continuing, but data/programming error.',1,continue,error_log);
                                if temp_dyad_list[x].HiActB > final_dyadic_mid.HiActA then
                                   final_dyadic_mid.HiActA := temp_dyad_list[x].HiActB;
                                if temp_dyad_list[x].HiActA > final_dyadic_mid.HiActB then
                                   final_dyadic_mid.HiActB := temp_dyad_list[x].HiActA;
                                if temp_dyad_list[x].HostLevB > final_dyadic_mid.HostLevA then
                                   final_dyadic_mid.HostLevA := temp_dyad_list[x].HostLevB;
                                if temp_dyad_list[x].HostLevA > final_dyadic_mid.HostLevB then
                                   final_dyadic_mid.HostLevB := temp_dyad_list[x].HostLevA;
                             end;

                          {Figure out issue codes.  Again, need to be sure I have sideA, B correct.}
                          if (final_dyadic_mid.ccodeA = temp_dyad_list[x].ccodeA) then
                             begin
                                if not (final_dyadic_mid.ccodeB = temp_dyad_list[x].ccodeB) then EUGeneError ('Error in dyadic MID 3.0 creation, saw A=A and not B=B when checking hostlevs.  continuing, but data/programming error.',1,continue,error_log);
                                final_dyadic_mid.issue_setA := final_dyadic_mid.issue_setA + temp_dyad_list[x].issue_setA;
                                final_dyadic_mid.issue_setB := final_dyadic_mid.issue_setB + temp_dyad_list[x].issue_setB;
                             end
                          else
                             begin   {Check A vs. B, and B vs. A}
                                if not ( (final_dyadic_mid.ccodeB = temp_dyad_list[x].ccodeA) and (final_dyadic_mid.ccodeA = temp_dyad_list[x].ccodeB) ) then
                                   EUGeneError ('Error in dyadic MID 3.0 creation, saw A=A and not B=B when checking hostlevs.  continuing, but data/programming error.',1,continue,error_log);
                                final_dyadic_mid.issue_setA := final_dyadic_mid.issue_setA + temp_dyad_list[x].issue_setB;
                                final_dyadic_mid.issue_setB := final_dyadic_mid.issue_setB + temp_dyad_list[x].issue_setA;
                             end;

                          {check if reciprocated.  If there is any incident in opposite direction,
                           or a clash, then dyadic reciprocation (clash for 1st rec was handled above).}
                          if (final_dyadic_mid.HiActA = 17) or (final_dyadic_mid.HiActB = 17) then
                             final_dyadic_mid.ReciprocatedMID := true;
                          if (temp_dyad_list[x].ccodeA = final_dyadic_mid.ccodeB) then
                             begin
                                if (temp_dyad_list[x].ccodeB = final_dyadic_mid.ccodeA) then
                                   final_dyadic_mid.ReciprocatedMID := true
                                else EUGeneError ('Error in creating dyadic MIDs v3.0 format; in dyadized incident list, examined MID where next in dyad had A=B, but where B<>A.  Logic problem - notify programmer.  Continuing, but programming problem exists.',1,continue,error_log);
                             end;


                          {now move on to the next incident}
                          inc (x);
                          {x will mark the next incident to start at after completing this
                           incident, if splitting was necessary.}

                       until x > end_incident_rec;

                          {at this point, have gone through incident start+1 to either end of list,
                           or to the point where the split is.  x is now one more than what
                           I've processed.}


                       {Now compute other info about the dyadic MID, now that the dates are set.}
                       {compute final top 3 issues for A and B}
                       final_dyadic_mid.RevType_A1 := low_issue(1, final_dyadic_mid.issue_setA);
                       final_dyadic_mid.RevType_A2 := low_issue(2, final_dyadic_mid.issue_setA);
                       final_dyadic_mid.RevType_B1 := low_issue(1, final_dyadic_mid.issue_setB);
                       final_dyadic_mid.RevType_B2 := low_issue(2, final_dyadic_mid.issue_setB);


                       {compute RoleA, RoleB.}
                       {Primary initiator (1) if 1) originator in overall MID, and 2) on Side A.
                        Primary target (3) if 1) originator in overall MID, and 2) on Side B.
                        Initiator side joiner (2) if 1) NOT originator in overall MID, and 2) on Side A.
                        Target side joiner (4) if 1) NOT originator in overall MID, and 2) on Side B.  }


                       country_disp_numA := Country_Disputes.get_country_disp_num_when_many (final_dyadic_mid.idnum, final_dyadic_mid.ccodeA, matching_country_rec_array[final_dyadic_mid.ccodeA]);
                       country_disp_numB := Country_Disputes.get_country_disp_num_when_many (final_dyadic_mid.idnum, final_dyadic_mid.ccodeB, matching_country_rec_array[final_dyadic_mid.ccodeB]);
                       if (Country_Disputes.get_sideA(country_disp_numA)) and (Country_Disputes.get_originator(country_disp_numA)) then
                          final_dyadic_mid.RoleA := 1
                       else
                          if (Country_Disputes.get_sideA(country_disp_numA)) and (not(Country_Disputes.get_originator(country_disp_numA))) then
                             final_dyadic_mid.RoleA := 2
                       else
                          if (not(Country_Disputes.get_sideA(country_disp_numA))) and (Country_Disputes.get_originator(country_disp_numA)) then
                             final_dyadic_mid.RoleA := 3
                       else
                          if (not(Country_Disputes.get_sideA(country_disp_numA))) and (not(Country_Disputes.get_originator(country_disp_numA))) then
                             final_dyadic_mid.RoleA := 4;

                       if (Country_Disputes.get_sideA(country_disp_numB)) and (Country_Disputes.get_originator(country_disp_numB)) then
                          final_dyadic_mid.RoleB := 1
                       else
                          if (Country_Disputes.get_sideA(country_disp_numB)) and (not(Country_Disputes.get_originator(country_disp_numB))) then
                             final_dyadic_mid.RoleB := 2
                       else
                          if (not(Country_Disputes.get_sideA(country_disp_numB))) and (Country_Disputes.get_originator(country_disp_numB)) then
                             final_dyadic_mid.RoleB := 3
                       else
                          if (not(Country_Disputes.get_sideA(country_disp_numB))) and (not(Country_Disputes.get_originator(country_disp_numB))) then
                             final_dyadic_mid.RoleB := 4;

                       {compute overall highest action, hostility level (may not ever output this.)}
                       final_dyadic_mid.HiAct := max(final_dyadic_mid.HiActA, final_dyadic_mid.HiActB);
                       final_dyadic_mid.HostLev := max(final_dyadic_mid.HostLevA, final_dyadic_mid.HostLevB);
                       final_dyadic_mid.Fatality := overall_disputes.get_fatality_dispute(final_dyadic_mid.idnum);
                       final_dyadic_mid.Outcome := overall_disputes.get_outcome(final_dyadic_mid.idnum);
                       final_dyadic_mid.Settlement := overall_disputes.get_settlement(final_dyadic_mid.idnum);
                       final_dyadic_mid.NumberA := overall_disputes.get_numberA(final_dyadic_mid.idnum);
                       final_dyadic_mid.NumberB := overall_disputes.get_numberB(final_dyadic_mid.idnum);

                       {Also set a last few variables that come from the country data, but that I want
                        to have stored in the structure so that I can add maoz values also.}

                       country_disp_numA := Country_Disputes.get_country_disp_num_when_many (final_dyadic_mid.idnum, final_dyadic_mid.ccodeA, matching_country_rec_array[final_dyadic_mid.ccodeA]);
                       final_dyadic_mid.FatalityA := Country_Disputes.get_fatality(country_disp_numA);
                       final_dyadic_mid.FatalityVA := Country_Disputes.get_fatality_precise(country_disp_numA);
                       final_dyadic_mid.OriginatorA := Country_Disputes.get_originator(country_disp_numA);
                       final_dyadic_mid.SideA := Country_Disputes.get_SideA(country_disp_numA);

                       country_disp_numB := Country_Disputes.get_country_disp_num_when_many (final_dyadic_mid.idnum, final_dyadic_mid.ccodeB, matching_country_rec_array[final_dyadic_mid.ccodeB]);
                       final_dyadic_mid.FatalityB := Country_Disputes.get_fatality(country_disp_numB);
                       final_dyadic_mid.FatalityVB := Country_Disputes.get_fatality_precise(country_disp_numB);
                       final_dyadic_mid.OriginatorB := Country_Disputes.get_originator(country_disp_numB);
                       final_dyadic_mid.SideB := Country_Disputes.get_SideA(country_disp_numB);

                       {In this initialization routine, set 3 vars that come from Maoz only}
                       final_dyadic_mid.COWWar := false;
                       final_dyadic_mid.Durindx := 0;
                       final_dyadic_mid.DurDays := 0;


                       {There are also a few exceptions for cases of war, and end dates.
                        1) In cases that go to war, the MID end date has been set to war end date, even
                         though there are not necessarily incidents coding the war.  The dates in the dyadic
                         MID will have been set from last incident date, and so they need to be set to the
                         last MID date in MIDA or MIDB, possibly need to figure out last overlap of war dates,
                         so earlier of the two MIDB dates for the 2 states in the dyad.
                        2) Also, in cases that go to war, the MIDB hostlev is a 5, even though there may not
                         be an incident with level 5.  This is because many level 4 incidents could add up to
                         an overall level 5 MID.  The dyadic mid record needs to be updated so that the
                         dyadic hostlev, and hostlevA and B, are level 5s.
                        3) Also, final MID end date could be different from last incdient end date, e.g.
                         if a later settlement. }

                        {To fix these, what I need is, if both states are 5's in MIDB,
                          then dyadic hostlevs should be 5, and fix end dates. }

                       country_disp_numA := Country_Disputes.get_country_disp_num_when_many (final_dyadic_mid.idnum, final_dyadic_mid.ccodeA, matching_country_rec_array[final_dyadic_mid.ccodeA]);
                       country_disp_numB := Country_Disputes.get_country_disp_num_when_many (final_dyadic_mid.idnum, final_dyadic_mid.ccodeB, matching_country_rec_array[final_dyadic_mid.ccodeB]);
                       MIDB_HostLevA := Country_Disputes.get_hostlev(country_disp_numA);
                       MIDB_HostLevB := Country_Disputes.get_hostlev(country_disp_numB);

                       {Now check the first circumstance I'm concerned about, where it's a war but dyadic MID may not say it.}
                       if (MIDB_HostLevA = 5) and (MIDB_HostLevB = 5) then
                          begin
                             {In this circumstance, the data generated for final dyadic MID based on incident data
                              will be wrong, because overall MID info takes into account war, while other data set
                              does not.}
                             final_dyadic_mid.HostLevA := MIDB_HostLevA;  {this should be a 5 always}
                             final_dyadic_mid.HostLevB := MIDB_HostLevB;  {this should be a 5 always}
                             final_dyadic_mid.HostLev := max(final_dyadic_mid.HostLevA, final_dyadic_mid.HostLevB); {this should be a 5 always}

                             {That set hostility level.  Also need to set end date to the earlier of the two ends
                              from MID B.}
                             if which_earlier_date (country_disputes.get_endyear(country_disp_numA), country_disputes.get_endmonth(country_disp_numA), country_disputes.get_endday(country_disp_numA), country_disp_numA,
                                                    country_disputes.get_endyear(country_disp_numB), country_disputes.get_endmonth(country_disp_numB), country_disputes.get_endday(country_disp_numB), country_disp_numB, identified_because_more_precise)
                                                 = 1 then
                                begin
                                   final_dyadic_mid.EndYear := country_disputes.get_endyear(country_disp_numA);
                                   final_dyadic_mid.EndMonth := country_disputes.get_endmonth(country_disp_numA);
                                   final_dyadic_mid.EndDay := country_disputes.get_endday(country_disp_numA);
                                end
                             else
                                begin
                                   final_dyadic_mid.EndYear := country_disputes.get_endyear(country_disp_numB);
                                   final_dyadic_mid.EndMonth := country_disputes.get_endmonth(country_disp_numB);
                                   final_dyadic_mid.EndDay := country_disputes.get_endday(country_disp_numB);
                                end;

                          end;

                       {What to do if only 1 is hostlev 5 at MIDB level.
                        Should its hostlev be set in the dyadic MID a 5 from miD B too?
                        And end dates changed only for it?   No - this is a case
                        where one was a participant in the overall war, but not both.
                        So in the dyad, the dyadic incidents describe their relationship
                        appropriately.
                       The next code would check the MID B vs. MIDA levels when only is war.  But it's OK for
                        this to be the case, so commented out. }
                       {if ((MIDB_HostLevA = 5) and not (MIDB_HostLevB = 5)) or ((MIDB_HostLevB = 5) and not (MIDB_HostLevA = 5)) then
                          begin
                             trace.message ('MID '+inttostr(final_dyadic_mid.idnum)+', has different final hostlevs where one is war: Overall Hostlev='+inttostr(overall_disputes.get_hostlev_dispute(final_dyadic_mid.idnum))+'; ccode1 '+ inttostr(final_dyadic_mid.ccodeA)+ ' is '+inttostr(MIDB_HostLevA)+' and ccode2 '+ inttostr(final_dyadic_mid.ccodeB) + ' is '+inttostr(MIDB_HostLevB)+';  The dyadic MID hostlevs will be left at: '+ inttostr(final_dyadic_mid.HostLevA) + ' ' + inttostr(final_dyadic_mid.HostLevB) );
                          end;   }

                       {Comment out after checking errors.}
                       {These checks set to show up on private builds only as of
                        9/25/07 for version 3.2, with no
                        problematic dyadic MIDs found using MID v3.10.  }

                       if private_version then
                       begin
                          {First check for MID data errors.  }
                          {First check state A's end dates.}
                           If (final_dyadic_mid.EndYear = country_disputes.get_endyear(country_disp_numA)) and
                              (final_dyadic_mid.EndMonth = country_disputes.get_endmonth(country_disp_numA)) and
                              (final_dyadic_mid.EndDay = country_disputes.get_endday(country_disp_numA)) then
                              begin
                                 {There is no problem, the MIDB end date for country matches the dyadic end date.}
                              end
                           else
                              begin
                                   {The end dates don't match.  }
                                   {Check if there seems to be an error that COW should investigate.
                                    The date in final_dyadic_mid comes from IP data.  If IP data is later than MID date,
                                    then might be a problem.}
                                 if which_earlier_date (country_disputes.get_endyear(country_disp_numA), country_disputes.get_endmonth(country_disp_numA), country_disputes.get_endday(country_disp_numA), country_disp_numA,
                                                        final_dyadic_mid.EndYear, final_dyadic_mid.Endmonth, final_dyadic_mid.EndDay, final_dyadic_mid.incident_idnum, identified_because_more_precise)
                                                     = 1 then
                                    trace.message ('Possible MID end date problem - last IP date is > MIDB end date for ccode '+inttostr(final_dyadic_mid.ccodeA)+' in MID # '+inttostr(final_dyadic_mid.idnum)+'; IP date is '+ inttostr(final_dyadic_mid.EndMonth)+'/'+inttostr(final_dyadic_mid.EndDay)+'/'+inttostr(final_dyadic_mid.EndYear) + ' but MIDB date is '+inttostr(country_disputes.get_endmonth(country_disp_numA))+'/'+inttostr(country_disputes.get_endday(country_disp_numA))+'/'+inttostr(country_disputes.get_endyear(country_disp_numA)));
                              end;
                             {Now check state B's end dates}
                           If (final_dyadic_mid.EndYear = country_disputes.get_endyear(country_disp_numB)) and
                              (final_dyadic_mid.EndMonth = country_disputes.get_endmonth(country_disp_numB)) and
                              (final_dyadic_mid.EndDay = country_disputes.get_endday(country_disp_numB)) then
                              begin
                                  {There is no problem, the MIDB end date for country matches the dyadic end date.}
                              end
                           else
                              begin
                                   {The end dates don't match.  The dyadic end dates may need to be updated.   }
                                   {First, check if there seems to be an error that COW should investigate.}
                               if which_earlier_date (country_disputes.get_endyear(country_disp_numB), country_disputes.get_endmonth(country_disp_numB), country_disputes.get_endday(country_disp_numB), country_disp_numB,
                                                      final_dyadic_mid.EndYear, final_dyadic_mid.Endmonth, final_dyadic_mid.EndDay, final_dyadic_mid.incident_idnum, identified_because_more_precise)
                                                   = 1 then
                                  trace.message ('Possible MID end date problem - last IP date is > MIDB end date for ccode '+inttostr(final_dyadic_mid.ccodeB)+' in MID # '+inttostr(final_dyadic_mid.idnum)+'; IP date is '+ inttostr(final_dyadic_mid.EndMonth)+'/'+inttostr(final_dyadic_mid.EndDay)+'/'+inttostr(final_dyadic_mid.EndYear) + ' but MIDB date is '+inttostr(country_disputes.get_endmonth(country_disp_numB))+'/'+inttostr(country_disputes.get_endday(country_disp_numB))+'/'+inttostr(country_disputes.get_endyear(country_disp_numB)));
                            end;

                           {Now check start dates to see if any MIDs with incidents before MIDB start.}
                           {First check state A}
                            If (final_dyadic_mid.styear = country_disputes.get_styear(country_disp_numA)) and
                               (final_dyadic_mid.stMonth = country_disputes.get_stmonth(country_disp_numA)) and
                               (final_dyadic_mid.stDay = country_disputes.get_stday(country_disp_numA)) then
                               begin
                                      {There is no problem, the MIDB start date for country matches the dyadic end date.}
                               end
                            else
                               begin
                                      {The start dates don't match.  }
                                      {Check if there seems to be an error that COW should investigate. }
                                  if which_earlier_date (country_disputes.get_styear(country_disp_numA), country_disputes.get_stmonth(country_disp_numA), country_disputes.get_stday(country_disp_numA), country_disp_numA,
                                                         final_dyadic_mid.stYear, final_dyadic_mid.stmonth, final_dyadic_mid.stDay, final_dyadic_mid.incident_idnum, identified_because_more_precise)
                                                      = 2 then
                                     trace.message ('Possible MID start date problem - first IP date is < MIDB start date for ccode '+inttostr(final_dyadic_mid.ccodeA)+' in MID # '+inttostr(final_dyadic_mid.idnum)+'; IP date is '+ inttostr(final_dyadic_mid.stMonth)+'/'+inttostr(final_dyadic_mid.stDay)+'/'+inttostr(final_dyadic_mid.stYear) + ' but MIDB date is '+inttostr(country_disputes.get_stmonth(country_disp_numA))+'/'+inttostr(country_disputes.get_stday(country_disp_numA))+'/'+inttostr(country_disputes.get_styear(country_disp_numA)));
                               end;
                                {Also need to check state B's start dates}

                            If (final_dyadic_mid.styear = country_disputes.get_styear(country_disp_numB)) and
                               (final_dyadic_mid.stMonth = country_disputes.get_stmonth(country_disp_numB)) and
                               (final_dyadic_mid.stDay = country_disputes.get_stday(country_disp_numB)) then
                               begin
                                   {There is no problem, the MIDB end date for country matches the dyadic end date.}
                               end
                            else
                               begin
                                    {The end dates don't match.  The dyadic end dates may need to be updated.   }
                                      {First, check if there seems to be an error that COW should investigate.}
                                  if which_earlier_date (country_disputes.get_styear(country_disp_numB), country_disputes.get_stmonth(country_disp_numB), country_disputes.get_stday(country_disp_numB), country_disp_numB,
                                                         final_dyadic_mid.stYear, final_dyadic_mid.stmonth, final_dyadic_mid.stDay, final_dyadic_mid.incident_idnum, identified_because_more_precise)
                                                      = 2 then
                                     trace.message ('Possible MID start date problem - first IP date is < MIDB st date for ccode '+inttostr(final_dyadic_mid.ccodeB)+' in MID # '+inttostr(final_dyadic_mid.idnum)+'; IP date is '+ inttostr(final_dyadic_mid.stMonth)+'/'+inttostr(final_dyadic_mid.stDay)+'/'+inttostr(final_dyadic_mid.stYear) + ' but MIDB date is '+inttostr(country_disputes.get_stmonth(country_disp_numB))+'/'+inttostr(country_disputes.get_stday(country_disp_numB))+'/'+inttostr(country_disputes.get_styear(country_disp_numB)));
                                 end;
                       end;       {if private_version}


                       {Now check end dates.  Looking for cases where end date from incidents is not end date
                        for state in MIDB.  It could be the case that the MID ends later than the last
                        dyadic incident for a state.  If so, want to use the MIDB end date.  }
                       {Check final dyadic mid 1x vs. MID B end date for state A, then MIDB date for state B.}
                       for x_loop := 1 to 2 do
                          begin
                             case x_loop of
                                1: country_disp_num_to_check := country_disp_numA;
                                2: country_disp_num_to_check := country_disp_numB;
                                end;  {case}
                             If (final_dyadic_mid.EndYear = country_disputes.get_endyear(country_disp_num_to_check)) and
                                (final_dyadic_mid.EndMonth = country_disputes.get_endmonth(country_disp_num_to_check)) and
                                (final_dyadic_mid.EndDay = country_disputes.get_endday(country_disp_num_to_check)) then
                                begin
                                   {There is no problem, the MIDB end date for country matches the dyadic end date.}
                                end
                             else
                                begin
                                   {The end dates don't match.  The dyadic end dates may need to be updated.   }
                                   {If it's a bilateral dispute, then clearly dyad end date
                                    the end date in MIDB (it can only apply to those 2 states).  So take the
                                    earlier of ccodeA, ccodeB end dates as the dyadic MID end date.}
                                   if ((final_dyadic_mid.NumberA = 1) or (final_dyadic_mid.NumberB = 1)) then
                                      begin
                                         if which_earlier_date (country_disputes.get_endyear(country_disp_numA), country_disputes.get_endmonth(country_disp_numA), country_disputes.get_endday(country_disp_numA), country_disp_numA,
                                                              country_disputes.get_endyear(country_disp_numB), country_disputes.get_endmonth(country_disp_numB), country_disputes.get_endday(country_disp_numB), country_disp_numB, identified_because_more_precise)
                                                             = 1 then
                                            begin
                                               {trace.message ('Extending dyadic MID end date, dyadic dispute, ccodeA, in for ccode '+inttostr(final_dyadic_mid.ccodeA)+' vs. '+ inttostr(final_dyadic_mid.ccodeB)+' in MID # '+inttostr(final_dyadic_mid.idnum)+'. Old end date is '+ inttostr(final_dyadic_mid.EndMonth)+'/'+inttostr(final_dyadic_mid.EndDay)+'/'+inttostr(final_dyadic_mid.EndYear) +'; new end date is '+inttostr(country_disputes.get_endmonth(country_disp_numA))+'/'+inttostr(country_disputes.get_endday(country_disp_numA))+'/'+inttostr(country_disputes.get_endyear(country_disp_numA)));}
                                               final_dyadic_mid.EndYear := country_disputes.get_endyear(country_disp_numA);
                                               final_dyadic_mid.EndMonth := country_disputes.get_endmonth(country_disp_numA);
                                               final_dyadic_mid.EndDay := country_disputes.get_endday(country_disp_numA);
                                            end
                                         else
                                            begin
                                               {trace.message ('Extending dyadic MID end date, dyadic dispute, ccodeB, in for ccode '+inttostr(final_dyadic_mid.ccodeA)+' vs. '+ inttostr(final_dyadic_mid.ccodeB)+' in MID # '+inttostr(final_dyadic_mid.idnum)+'. Old end date is '+ inttostr(final_dyadic_mid.EndMonth)+'/'+inttostr(final_dyadic_mid.EndDay)+'/'+inttostr(final_dyadic_mid.EndYear) +'; new end date is '+inttostr(country_disputes.get_endmonth(country_disp_numB))+'/'+inttostr(country_disputes.get_endday(country_disp_numB))+'/'+inttostr(country_disputes.get_endyear(country_disp_numB)));}
                                               final_dyadic_mid.EndYear := country_disputes.get_endyear(country_disp_numB);
                                               final_dyadic_mid.EndMonth := country_disputes.get_endmonth(country_disp_numB);
                                               final_dyadic_mid.EndDay := country_disputes.get_endday(country_disp_numB);
                                            end;
                                      end              {if which _date...}
                                   else
                                   {Also, if just one side has one state on it, then end dates in MID B must reflect
                                    that dyad.}
                                   if ((final_dyadic_mid.NumberA = 1) or (final_dyadic_mid.NumberB = 1)) then
                                      begin
                                         if which_earlier_date (country_disputes.get_endyear(country_disp_numA), country_disputes.get_endmonth(country_disp_numA), country_disputes.get_endday(country_disp_numA), country_disp_numA,
                                                                country_disputes.get_endyear(country_disp_numB), country_disputes.get_endmonth(country_disp_numB), country_disputes.get_endday(country_disp_numB), country_disp_numB, identified_because_more_precise)
                                                             = 1 then
                                            begin
                                               {trace.message ('Extending dyadic MID end date, many-on-1 dispute, ccodeA, in for ccode '+inttostr(final_dyadic_mid.ccodeA)+' vs. '+ inttostr(final_dyadic_mid.ccodeB)+' in MID # '+inttostr(final_dyadic_mid.idnum)+'. Old end date is '+ inttostr(final_dyadic_mid.EndMonth)+'/'+inttostr(final_dyadic_mid.EndDay)+'/'+inttostr(final_dyadic_mid.EndYear) +'; new end date is '+inttostr(country_disputes.get_endmonth(country_disp_numA))+'/'+inttostr(country_disputes.get_endday(country_disp_numA))+'/'+inttostr(country_disputes.get_endyear(country_disp_numA)));}
                                               final_dyadic_mid.EndYear := country_disputes.get_endyear(country_disp_numA);
                                               final_dyadic_mid.EndMonth := country_disputes.get_endmonth(country_disp_numA);
                                               final_dyadic_mid.EndDay := country_disputes.get_endday(country_disp_numA);
                                            end
                                         else
                                            begin
                                               {trace.message ('Extending dyadic MID end date, many-on-1 dispute, ccodeB, in for ccode '+inttostr(final_dyadic_mid.ccodeA)+' vs. '+ inttostr(final_dyadic_mid.ccodeB)+' in MID # '+inttostr(final_dyadic_mid.idnum)+'. Old end date is '+ inttostr(final_dyadic_mid.EndMonth)+'/'+inttostr(final_dyadic_mid.EndDay)+'/'+inttostr(final_dyadic_mid.EndYear) +'; new end date is '+inttostr(country_disputes.get_endmonth(country_disp_numB))+'/'+inttostr(country_disputes.get_endday(country_disp_numB))+'/'+inttostr(country_disputes.get_endyear(country_disp_numB)));}
                                               final_dyadic_mid.EndYear := country_disputes.get_endyear(country_disp_numB);
                                               final_dyadic_mid.EndMonth := country_disputes.get_endmonth(country_disp_numB);
                                               final_dyadic_mid.EndDay := country_disputes.get_endday(country_disp_numB);
                                            end;
                                      end
                                   else
                                      begin
                                        {multilateral;  MIDS 3564 and 4339 hit here, but the discrepant end dates
                                         are actually OK because the states in the "mismatching" dyads did leave early.
                                         Keep in the check, but without checking those two MIDs.}
                                        if not ( (final_dyadic_mid.idnum=3564) or (final_dyadic_mid.idnum=4339) ) then
                                           trace.message ('Multilateral dispute with discrepant end dates in MID '+inttostr(final_dyadic_mid.idnum)+', states '+inttostr(final_dyadic_mid.ccodeA)+' and '+inttostr(final_dyadic_mid.ccodeB));
                                      end;

                                end;
                          end;    {for x_loop}


                       {Finally, there are also a few exceptions for cases where there is a precise MID start/end date,
                        but an inexact incident end/start date.  In these cases, we want to use the
                        more precise dates from the MIDB information.  One complication, though,
                        is what if it's multilateral, so we don't know if the precise date applies to the
                        particular dyad where we have uncertainty in the MID IP data.  It turns out that this
                        is not a problem in the actual data.

                        End dates were actually caught above, in the more general routine that checked
                        incident end dates vs. MID end dates.  So here only a check for start dates is needed.
                        (Code to check end dates is there, but all cases are actually caught before this.) }

                       {If IP mid date is imprecise, but MID B data is precise, report it.
                        Imprecise data is indicated by a -9 in start day.}
                        {If only one is more precise...happens often.  Process only if both are precise in MID B}
                       {** need start day, and side B}
                       If ((final_dyadic_mid.Stday = -9) and (country_disputes.get_stday(country_disp_numA) <> -9) and
                           (country_disputes.get_stday(country_disp_numB) <> -9)) then
                          begin
                             {have more precise data.  But can't assume it's related - precise end day for the
                              state in MID B might be totally unrelated to the dyadic interaction. }
                             {trace.message ('Dispute has precise start date for both in MID B but incident does not in MID '+inttostr(final_dyadic_mid.idnum)+', states '+inttostr(final_dyadic_mid.ccodeA)+' and '+inttostr(final_dyadic_mid.ccodeB)+'.  Replacing imprecise MID data with precise date.');}
                             {In all cases where this occurs, an inspection of the data reveals that the start date
                              should become the MID B start date.  Data inspection also shows that the more
                              precise data is the same for ccodeA and B.}
                             final_dyadic_mid.Stday := country_disputes.get_stday(country_disp_numA);
                          end;

                       {There are actually no cases where this happens for MIDB}
                       If ((final_dyadic_mid.EndDay = -9) and (country_disputes.get_endday(country_disp_numA) <> -9) and
                           (country_disputes.get_endday(country_disp_numB) <> -9)) then
                          begin
                             {have more precise data.  But can't assume it's related - precise end day for the
                              state in MID B might be totally unrelated to the dyadic interaction. }
                             {trace.message ('Dispute has precise end date for both in MID B but incident does not in MID '+inttostr(final_dyadic_mid.idnum)+', states '+inttostr(final_dyadic_mid.ccodeA)+' and '+inttostr(final_dyadic_mid.ccodeB)+'.  Replacing imprecise MID data with precise date.');}
                          end;

                       {Now have processed each dyadic incident for that dyadic MID}

                       {Add the final dyadic dispute to the list.}
                       setlength(dyadic_dispute_data^, length(dyadic_dispute_data^)+1);
                       new (final_dyadic_mid_ptr);
                       final_dyadic_mid_ptr^ := final_dyadic_mid;
                       dyadic_dispute_data^[high(dyadic_dispute_data^)] := final_dyadic_mid_ptr;


                       {move on to next incident;  this might be moving on to next MID, or
                        to next dyadic MID if this incident wasn't the last for the dyad.}

                    until x > end_incident_rec;  {if x has advanced past the end, then done with MID}

                    start_incident_rec := end_incident_rec + 1;
                 until (start_incident_rec > high(temp_dyad_list) );
                 {This is the end of the loop going through each dyadic incident for this dyad.



                 {now have set up the final dyadic MIDs for that overall MID, so move on to next MID}
                 start_mid_rec := end_mid_rec + 1;
                 start_incident_rec := end_mid_rec + 1;
              until (start_mid_rec > participant_incident_data.num_participant_incident_records-1);

              num_dyadic_disputes := length(dyadic_dispute_data^);


              {Before going on to the pre-1992 data, mark that these
               MID #s were created from PI data.  }
              for x := low(dyadic_dispute_data^) to high(dyadic_dispute_data^) do
                 pi_midnum_list[self.get_MID_num(x)] := true;


              {Also mark it as initialized, so that I don't have an error calling it from
               itself below.}
              created := true;

              
              {Now add the dyadic dispute data for the period where we don't have new PI data,
               either from Maoz or from COW}
              case use_cow_or_maoz_pre_1992 of
                 cow_pre_1992 : add_cow_dyadic_pre_1992 (user_selections, configuration, Country_Disputes, year1, configuration.cow_MID_Format_2_1_Data_Last_year, blank_dyad_rec);
                 maoz_pre_1992 : add_maoz_dyadic_pre_1992 (user_selections, configuration, year1, year2, blank_dyad_rec);
                 none_pre_1992 : begin   end;   {add no other data}
                 end;     {case}


               {Sort the list of dyadic-disputes by the countries involved and by start date.}
              trace.message ('Finished constructing dyadic disputes in v3.0 read.  Now Sorting by country 1.');
              trace.message ('       '+inttostr(num_dyadic_disputes)+ ' dyadic disputes of all types calculated');


              {writeln ('Before sorting, dyadic dispute list is');}
              {list_dyadic_disputes (dyadic_disputes, num_dyadic_disputes, country_disputes); }

              {First sort by first country in dyad, state 0}
              quicksort_dyadic_disputes_by_country (low(dyadic_dispute_data^), high(dyadic_dispute_data^), 0);

              {writeln ('After sorting by country 1, dyadic dispute list is '); }
              { list_dyadic_disputes (dyadic_disputes, num_dyadic_disputes, country_disputes);}

              {That does overall list by first of dyad.  Now do by 2nd country in dyad, within each first}
              trace.message ('Starting sort by country 2...');
              x := 0;
              y := 0;
              repeat
                  done := false;
                  repeat
                     y := y + 1;
                     if (y >= get_last_dispnum) then done:= true
                     else if (self.get_ccode(x, 0) <>
                              self.get_ccode(y, 0)) then done:=true;
                  until done;
                  {now sorting by second country, side 1}
                  if y = get_last_dispnum then quicksort_dyadic_disputes_by_country (x, y, 1) else
                     quicksort_dyadic_disputes_by_country (x, y-1, 1);
                  x := y;
              {until y > high(dyadic_dispute_data^);}
              until y > get_last_dispnum;
              {writeln ('After sorting by country 2, dyadic dispute list is ');}
              {list_dyadic_disputes (dyadic_disputes, num_dyadic_disputes, country_disputes);}

               {That did sort by first and second in dyad.  Now do by year within each dyad}
              trace.message ('Doing quicksort by year...');
              x := 1;
              y := 1;
              repeat
                  done := false;
                  repeat
                     y := y + 1;
                     {if (y > high(dyadic_dispute_data^)) then done:= true}
                     if (y >= get_last_dispnum) then done:= true
                     else if (self.get_ccode(x, 0) <> self.get_ccode(y, 0)) or
                             (self.get_ccode(x, 1) <> self.get_ccode(y, 1)) then done:=true;
                  until done;
                  if y = get_last_dispnum then quicksort_dyadic_disputes_by_first_year (x, y) else
                     quicksort_dyadic_disputes_by_first_year (x, y-1);
                  {quicksort_dyadic_disputes_by_first_year (x, y-1);}
                  x := y;
              until y > get_last_dispnum;

               {Don'need any further sorting for now.}
               trace.message ('Finished sorting dyadic disputes, v3.0.');

              {Now set the index marker to the 1st dispute for each ccode pair.  }
               {do this by running through the sorted dyads, and marking the index to the correct spot
                when the spot has not yet been set.}
              for x := low(dyadic_dispute_data^) to high(dyadic_dispute_data^) do
                 if index^[self.get_ccode(x, 0), self.get_ccode(x, 1)] = null_dispute_number then
                    index^[self.get_ccode(x, 0), self.get_ccode(x, 1)] := x;

              Dyadic_dispute_trace.tickdone;

              stored_peaceyrs.ccode1 := min_ccode;
              stored_peaceyrs.ccode2 := min_ccode;
              stored_peaceyrs.year := min_year;
              stored_peaceyrs.numyears := missing_value;

           finally
              Dyadic_dispute_trace.free;
              trace.exit('Finished initializing, reading, and sorting v3.0 Dyadic dispute data');
           end;
        end;       {constructor 3.0 dyadic}


     destructor TDyadic_dispute_data_obj_integrated_format30.destroy;
       var x : integer;
       begin
         try
            country_disputes.free;
            overall_disputes.free;
            participant_incident_data.free;
            for x := low(dyadic_dispute_data^) to high(dyadic_dispute_data^) do
               if dyadic_dispute_data^[x] <> nil then dispose (dyadic_dispute_data^[x]);
            setlength(dyadic_dispute_data^,0);
            if dyadic_dispute_data <> nil then dispose(dyadic_dispute_data);
            dyadic_dispute_data := nil;
            created := false;
            inherited destroy;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;   {except}
       end;     {destructor}


     procedure TDyadic_dispute_data_obj_integrated_format30.output_to_file (outfilename : TFileName);
        var outfile : text;
            output_case : integer;
            ccodeA, ccodeB : ccode_range;
            count : integer;
        begin
           try
              try
                 assignfile (outfile, outfilename);
                 rewrite (outfile);
                 count := 0;
                 writeln (outfile, 'DispNum,DyMIDNum,CCodeA,CCodeB,StDay,StMon,StYear,EndDay,EndMon,EndYear,',
                          'HiactA,HiactB,HostlevA,HostlevB,SideAA,SideAB,',
                          'RevstatA,Revtyp1A,Revtyp2A,RevstatB,Revtyp1B,Revtyp2B,',
                          'OrigA,OrigB,FatalA,FatalB,FatalPrA,FatalPrB,RoleA,RoleB,',
                          'Reciproc,SideADyA,SideADyB,Version');

                 for output_case := low(dyadic_dispute_data^) to high(dyadic_dispute_data^) do
                    {output only the version 3.0 cases}
                    if has_midnum_from_pi_data(get_MID_num(output_case)) then
                    begin
                       ccodeA := get_ccode(output_case, 0);
                       ccodeB := get_ccode(output_case, 1);

                       write(outfile, get_MID_num(output_case), ',');
                       write(outfile, get_dyadic_MID_num(output_case), ',');
                       write(outfile, ccodeA, ',');
                       write(outfile, ccodeB, ',');

                       write(outfile, get_stday(output_case, ccodeA), ',');
                       write(outfile, get_stmonth(output_case, ccodeA), ',');
                       write(outfile, get_styear(output_case, ccodeA), ',');
                       if not (get_stday(output_case, ccodeA) = get_stday(output_case,ccodeB)) then EUGeneError ('Error in outputting dyadic disputes 3.0, start dates for the 2 states do not match.  Notify programmer of error.',1,continue,error_log);
                       if not (get_stmonth(output_case, ccodeA) = get_stmonth(output_case,ccodeB)) then EUGeneError ('Error in outputting dyadic disputes 3.0, start dates for the 2 states do not match.  Notify programmer of error.',1,continue,error_log);
                       if not (get_styear(output_case, ccodeA) = get_styear(output_case,ccodeB)) then EUGeneError ('Error in outputting dyadic disputes 3.0, start dates for the 2 states do not match.  Notify programmer of error.',1,continue,error_log);
                       write(outfile, get_endday(output_case, ccodeA), ',');
                       write(outfile, get_endmonth(output_case, ccodeA), ',');
                       write(outfile, get_endyear(output_case, ccodeA), ',');
                       if not (get_endday(output_case, ccodeA) = get_endday(output_case,ccodeB)) then EUGeneError ('Error in outputting dyadic disputes 3.0, start dates for the 2 states do not match.  Notify programmer of error.',1,continue,error_log);
                       if not (get_endmonth(output_case, ccodeA) = get_endmonth(output_case,ccodeB)) then EUGeneError ('Error in outputting dyadic disputes 3.0, start dates for the 2 states do not match.  Notify programmer of error.',1,continue,error_log);
                       if not (get_endyear(output_case, ccodeA) = get_endyear(output_case,ccodeB)) then EUGeneError ('Error in outputting dyadic disputes 3.0, start dates for the 2 states do not match.  Notify programmer of error.',1,continue,error_log);
                       write(outfile, get_HiAct_state(output_case, ccodeA), ',');
                       write(outfile, get_HiAct_state(output_case, ccodeB), ',');
                       write(outfile, get_hostlev_state(output_case, ccodeA), ',');
                       write(outfile, get_hostlev_state(output_case, ccodeB), ',');
                       {SideA is on sideA of original/overall MID}
                       write(outfile, booltostring(get_sideA(output_case, ccodeA)), ',');
                       write(outfile, booltostring(get_sideA(output_case, ccodeB)), ',');
                       write(outfile, booltostring(get_revisionist(output_case, ccodeA)), ',');
                       write(outfile, get_revtype(output_case, ccodeA), ',');
                       write(outfile, get_revtype2(output_case, ccodeA), ',');
                       write(outfile, booltostring(get_revisionist(output_case, ccodeB)), ',');
                       write(outfile, get_revtype(output_case, ccodeB), ',');
                       write(outfile, get_revtype2(output_case, ccodeB), ',');
                       {Note: Originator is Originator in overall MID, NOT dyadic.}
                       write(outfile, booltostring(get_originator(output_case, ccodeA)), ',');
                       write(outfile, booltostring(get_originator(output_case, ccodeB)), ',');
                       {Note: fatility, fatality precise are for overall MID, NOT dyadic.}
                       {Do we want this?}
                       write(outfile, get_fatality_state(output_case, ccodeA), ',');
                       write(outfile, get_fatality_state(output_case, ccodeB), ',');
                       write(outfile, get_fatality_state_precise(output_case, ccodeA), ',');
                       write(outfile, get_fatality_state_precise(output_case, ccodeB), ',');
                       {Note: role is role in overall MID}
                       write(outfile, get_Role(output_case, ccodeA), ',');
                       write(outfile, get_Role(output_case, ccodeB), ',');
                       write(outfile, booltostring(get_reciprocated_dyadic(output_case)), ',');

                       {Note: sideA dyadic is side (first mil action) in dyad.}
                       write(outfile, booltostring(get_sideA_dyadic(output_case, ccodeA)), ',');
                       write(outfile, booltostring(get_sideA_dyadic(output_case, ccodeB)), ',');

                       write(outfile, get_version(output_case):4:2);

                       writeln (outfile);
                       inc(count);
                    end;
              finally
                 closefile(outfile);
                 ShowMessage (inttostr (count) + ' dyadic-MID records written to output file '+outfilename);
              end;    {finally}
           except
              EUGeneError ('Error opening or writing to file '+outfilename+' in procedure write dyad disputes, format 3.0.  Check that file is not open in another program, is not read-only, and that disk is not full, and try again.  Program continues.',1,continue, error_log);
           end;   {except}
        end;

     procedure TDyadic_dispute_data_obj_integrated_format30.add_cow_dyadic_pre_1992 (user_selections : user_selection_type; configuration :
               configuration_type; Country_Disputes : TCountry_dispute_data_obj; year1, year2 : year_range;
               blank_dyad_rec : format_30_dyad_record);
               {Adds data from year1 to year2, computed from cow 21 format data.}
        var cow_country_computed_dyadic_data : TCOWDyadic_dispute_data_obj_format21;
            x : integer;
            final_dyadic_mid : format_30_dyad_record;
            final_dyadic_mid_ptr : format_30_dyad_record_ptr;
            country_disp_num : integer;
        begin
           {Note that I can call the format21 routine even though we have 3.0 data.  This is because
           that routine differs only in that it computes dyad-mids from country-mid data.  It will
           still read in all of 3.0 data, because country_disputes and overall_disputes had their
           read procedures updated for v3.0.}
           cow_country_computed_dyadic_data := TCOWDyadic_dispute_data_obj_format21.init(user_selections, configuration, year1, year2);
           {country record data indexing starts at 1, NOT 0}
           for x := 0 to cow_country_computed_dyadic_data.get_last_dispnum do
              {check and see if we already have a participant-incident data version of the MID.}
              if not self.has_midnum_from_pi_data(cow_country_computed_dyadic_data.get_MID_num(x)) then {add this record}
                 begin   {add a copied record, we don't already have new data on that MID}
                    final_dyadic_mid := blank_dyad_rec;  {blank rec defined in .init routine.}
                    with final_dyadic_mid do
                       begin
                          idnum := cow_country_computed_dyadic_data.get_MID_num(x);
                          dyadic_idnum := (idnum * 100) + 1;
                          incident_idnum := missing_value;
                          ccodeA := cow_country_computed_dyadic_data.get_ccode(x, 0);
                          ccodeB := cow_country_computed_dyadic_data.get_ccode(x, 1);
                          nondirected_dyad_number := (min(ccodeA,ccodeB)*1000)+max(ccodeA,ccodeB);
                          {Need latest start date, and earliest end date}
                          if earlier_date (cow_country_computed_dyadic_data.get_styear(x, ccodeA),
                                           cow_country_computed_dyadic_data.get_stmonth(x, ccodeA),
                                           cow_country_computed_dyadic_data.get_stday(x, ccodeA),
                                           cow_country_computed_dyadic_data.get_styear(x, ccodeB),
                                           cow_country_computed_dyadic_data.get_stmonth(x, ccodeB),
                                           cow_country_computed_dyadic_data.get_stday(x, ccodeB)) then
                             {A has earlier date, so set to later date, which is B!!!}
                             begin
                                StDay := cow_country_computed_dyadic_data.get_stday(x, ccodeB);
                                StMonth := cow_country_computed_dyadic_data.get_stmonth(x, ccodeB);
                                StYear := cow_country_computed_dyadic_data.get_styear(x, ccodeB);
                             end
                             else
                             begin
                                StDay := cow_country_computed_dyadic_data.get_stday(x, ccodeA);
                                StMonth := cow_country_computed_dyadic_data.get_stmonth(x, ccodeA);
                                StYear := cow_country_computed_dyadic_data.get_styear(x, ccodeA);
                             end;
                          if earlier_date (cow_country_computed_dyadic_data.get_endyear(x, ccodeA),
                                           cow_country_computed_dyadic_data.get_endmonth(x, ccodeA),
                                           cow_country_computed_dyadic_data.get_endday(x, ccodeA),
                                           cow_country_computed_dyadic_data.get_endyear(x, ccodeB),
                                           cow_country_computed_dyadic_data.get_endmonth(x, ccodeB),
                                           cow_country_computed_dyadic_data.get_endday(x, ccodeB)) then
                             {A has earlier date, so set the end date to that earlier end date, which is A. !!!}
                             begin
                                endDay := cow_country_computed_dyadic_data.get_endday(x, ccodeA);
                                endMonth := cow_country_computed_dyadic_data.get_endmonth(x, ccodeA);
                                endYear := cow_country_computed_dyadic_data.get_endyear(x, ccodeA);
                             end
                             else
                             begin
                                endDay := cow_country_computed_dyadic_data.get_endday(x, ccodeB);
                                endMonth := cow_country_computed_dyadic_data.get_endmonth(x, ccodeB);
                                endYear := cow_country_computed_dyadic_data.get_endyear(x, ccodeB);
                             end;
                          {inexact_start_date := false;  {set to false;  not computed, and not used outside init procedure.}
                          {inexact_end_date := false;
                          internal_start_date := 0;
                          internal_end_date := 0;}
                          HiAct := max(cow_country_computed_dyadic_data.get_HiAct_state(x,ccodeA), cow_country_computed_dyadic_data.get_HiAct_state(x,ccodeB));
                          HiActA := cow_country_computed_dyadic_data.get_HiAct_state(x,ccodeA);
                          HiActB := cow_country_computed_dyadic_data.get_HiAct_state(x,ccodeB);
                          HostLev := max(cow_country_computed_dyadic_data.get_hostlev_state(x,ccodeA), cow_country_computed_dyadic_data.get_hostlev_state(x,ccodeB));
                          HostLevA := cow_country_computed_dyadic_data.get_hostlev_state(x,ccodeA);
                          HostLevB := cow_country_computed_dyadic_data.get_hostlev_state(x,ccodeB);
                          ReciprocatedMID := (HostLevA>0) and (HostLevB>0);
                          revisionistA := cow_country_computed_dyadic_data.get_revisionist(x,ccodeA);
                          RevType_A1 := cow_country_computed_dyadic_data.get_revtype(x,ccodeA);
                          RevType_A2 := missing_value;
                          revisionistB := cow_country_computed_dyadic_data.get_revisionist(x,ccodeB);
                          RevType_B1 := cow_country_computed_dyadic_data.get_revtype(x,ccodeB);
                          RevType_B2 := missing_value;
                          issue_setA := [];
                          issue_setB := [];
                          SIDEAA_dyadic := false;   {meaningless}
                          SIDEAB_dyadic := false;   {meaningless}

                          {compute RoleA, RoleB.}
                          {Primary initiator (1) if 1) originator in overall MID, and 2) on Side A.
                           Primary target (3) if 1) originator in overall MID, and 2) on Side B.
                           Initiator side joiner (2) if 1) NOT originator in overall MID, and 2) on Side A.
                           Target side joiner (4) if 1) NOT originator in overall MID, and 2) on Side B.  }

                          country_disp_num := Country_Disputes.get_country_disp_num_when_many (idnum, ccodeA, 1);
                          if (Country_Disputes.get_sideA(country_disp_num)) and (Country_Disputes.get_originator(country_disp_num)) then
                             RoleA := 1
                          else
                             if (Country_Disputes.get_sideA(country_disp_num)) and (not(Country_Disputes.get_originator(country_disp_num))) then
                                RoleA := 2
                          else
                             if (not(Country_Disputes.get_sideA(country_disp_num))) and (Country_Disputes.get_originator(country_disp_num)) then
                                RoleA := 3
                          else
                             if (not(Country_Disputes.get_sideA(country_disp_num))) and (not(Country_Disputes.get_originator(country_disp_num))) then
                                RoleA := 4;

                          country_disp_num := Country_Disputes.get_country_disp_num_when_many (idnum, ccodeB, 1);
                          if (Country_Disputes.get_sideA(country_disp_num)) and (Country_Disputes.get_originator(country_disp_num)) then
                             RoleB := 1
                          else
                             if (Country_Disputes.get_sideA(country_disp_num)) and (not(Country_Disputes.get_originator(country_disp_num))) then
                                RoleB := 2
                          else
                             if (not(Country_Disputes.get_sideA(country_disp_num))) and (Country_Disputes.get_originator(country_disp_num)) then
                                RoleB := 3
                          else
                             if (not(Country_Disputes.get_sideA(country_disp_num))) and (not(Country_Disputes.get_originator(country_disp_num))) then
                                RoleB := 4;

                          Fatality := overall_disputes.get_fatality_dispute (final_dyadic_mid.idnum);
                          outcome := overall_disputes.get_outcome (final_dyadic_mid.idnum);
                          settlement := overall_disputes.get_settlement (final_dyadic_mid.idnum);
                          NumberA := overall_disputes.get_numberA(final_dyadic_mid.idnum);
                          NumberB := overall_disputes.get_numberB(final_dyadic_mid.idnum);

                          country_disp_num := Country_Disputes.get_country_disp_num_when_many (idnum, ccodeA, 1);
                          FatalityA := Country_Disputes.get_fatality(country_disp_num);
                          FatalityVA := Country_Disputes.get_fatality_precise(country_disp_num);
                          OriginatorA := Country_Disputes.get_originator(country_disp_num);
                          SideA := Country_Disputes.get_SideA(country_disp_num);

                          country_disp_num := Country_Disputes.get_country_disp_num_when_many (idnum, ccodeB, 1);
                          FatalityB := Country_Disputes.get_fatality(country_disp_num);
                          FatalityVB := Country_Disputes.get_fatality_precise(country_disp_num);
                          OriginatorB := Country_Disputes.get_originator(country_disp_num);
                          SideB := Country_Disputes.get_SideA(country_disp_num);


                          COWWar := false;
                          Durindx := missing_value;
                          DurDays := missing_value;

                          version := 2.1;   {actually, it's 3.0 data, but this will indicated they were computed dyad-MIDs pre 1993.}
                       end;       {with final_dyadic_mid...}

                    {Add the final dyadic dispute to the list.}
                    setlength(dyadic_dispute_data^, length(dyadic_dispute_data^)+1);
                    new (final_dyadic_mid_ptr);
                    final_dyadic_mid_ptr^ := final_dyadic_mid;
                    dyadic_dispute_data^[high(dyadic_dispute_data^)] := final_dyadic_mid_ptr;
                 end;
           num_dyadic_disputes := length(dyadic_dispute_data^);
           added_cow_or_maoz := added_cow;

           cow_country_computed_dyadic_data.free;
        end;

     procedure TDyadic_dispute_data_obj_integrated_format30.add_maoz_dyadic_pre_1992 (user_selections : user_selection_type; configuration :
               configuration_type; year1, year2 : year_range; blank_dyad_rec : format_30_dyad_record);
        {This adds the maoz dyadic MID disputes to the integrated structure.  While doing this, it converts
         the format of the maoz records from 1 case per dispute year to 1 case per dispute.}
        var
            x : integer;
            final_dyadic_mid : format_30_dyad_record;
            final_dyadic_mid_ptr : format_30_dyad_record_ptr;
            maoz_dyadic_dispute_data : TMaoz_Dyadic_dispute_data_obj;
            matching_case_num, case_to_check : integer;
            done_check, add_new, update_old :boolean;
        begin
           if not initialized then EUGeneError ('Tried to add maoz dyadic data before 3.0 data was initialized.  Error.',1,stop,error_log);
           maoz_dyadic_dispute_data := TMaoz_Dyadic_dispute_data_obj.init(user_selections, configuration, year1, year2);

{trace.message ('Checking maoz disps after original structure initialized.');
trace.message (inttostr(maoz_dyadic_dispute_data.get_last_dispnum )+' disputes in structure after initial initialize of maoz only structure');
trace.message (inttostr(length(dyadic_dispute_data^))+' disputes in structure after PI creation, before maoz added, inside add proc. ');
for x := 0 to maoz_dyadic_dispute_data.get_last_dispnum do
   begin
      if (maoz_dyadic_dispute_data.get_year(x) = 1994) and (maoz_dyadic_dispute_data.get_ccode(x,0)=710) and (maoz_dyadic_dispute_data.get_ccode(x,1)=2) then trace.message ('Saw requested rec in maoz as record'+inttostr(x));
      if (maoz_dyadic_dispute_data.get_ccode(x,0)=710) and (maoz_dyadic_dispute_data.get_ccode(x,1)=2) then trace.message ('Saw rec in maoz as record '+inttostr(x)+' with year '+inttostr((maoz_dyadic_dispute_data.get_year(x)) ) );
   end;  }
           for x := 0 to maoz_dyadic_dispute_data.get_last_dispnum do
              {note:  each case is a dispute *year*  }
              if not self.has_midnum_from_pi_data(maoz_dyadic_dispute_data.get_MID_num(x)) then {add this record}
                 begin   {add a copied record, if we don't already have new data on that MID}
                    final_dyadic_mid := blank_dyad_rec;  {blank rec defined in .init routine.}
                    with final_dyadic_mid do
                       begin
                          {Maoz unit is dyad year, so need to check to see if this is a
                           duplicate (another year) record, or a new MID.  }
                          idnum := maoz_dyadic_dispute_data.get_MID_num(x);
                          dyadic_idnum := (idnum * 100) + 1;
                          incident_idnum := missing_value;
                          ccodeA := maoz_dyadic_dispute_data.get_ccode(x, 0);
                          ccodeB := maoz_dyadic_dispute_data.get_ccode(x, 1);
                          nondirected_dyad_number := (min(ccodeA,ccodeB)*1000)+max(ccodeA,ccodeB);
                          StDay := maoz_dyadic_dispute_data.get_MID_stday(x);
                          StMonth := maoz_dyadic_dispute_data.get_MID_stmonth(x);
                          StYear := maoz_dyadic_dispute_data.get_MID_styear(x);
                          endDay := maoz_dyadic_dispute_data.get_MID_EndDay(x);
                          endMonth := maoz_dyadic_dispute_data.get_MID_Endmonth(x);
                          endYear := maoz_dyadic_dispute_data.get_MID_Endyear(x);
                          {inexact_start_date := false;  {set to false;  not computed, and not used outside init procedure.}
                          {inexact_end_date := false;
                          internal_start_date := 0;
                          internal_end_date := 0;}
                          HiAct := convert_hiact_v21_to_v30(maoz_dyadic_dispute_data.get_MID_hiact(x));
                          HiActA := convert_hiact_v21_to_v30(maoz_dyadic_dispute_data.get_HiAct_state(x,ccodeA));
                          HiActB := convert_hiact_v21_to_v30(maoz_dyadic_dispute_data.get_HiAct_state(x,ccodeB));
                          HostLev := maoz_dyadic_dispute_data.get_MID_hostlev(x);
                          HostLevA := maoz_dyadic_dispute_data.get_hostlev_state(x,ccodeA);
                          HostLevB := maoz_dyadic_dispute_data.get_hostlev_state(x,ccodeB);
                          ReciprocatedMID := maoz_dyadic_dispute_data.get_MID_reciprocated(x);
                          revisionistA := maoz_dyadic_dispute_data.get_revisionist(x,ccodeA);
                          RevType_A1 := maoz_dyadic_dispute_data.get_revtype(x,ccodeA);
                          RevType_A2 := missing_value;
                          revisionistB := maoz_dyadic_dispute_data.get_revisionist(x,ccodeB);
                          RevType_B1 := maoz_dyadic_dispute_data.get_revtype(x,ccodeB);
                          RevType_B2 := missing_value;
                          issue_setA := [];
                          issue_setB := [];
                          SIDEAA_dyadic := false;   {meaningless}
                          SIDEAB_dyadic := false;   {meaningless}
                          RoleA := maoz_dyadic_dispute_data.get_Role(x, ccodeA);
                          RoleB := maoz_dyadic_dispute_data.get_Role(x, ccodeB);
                          COWWar := maoz_dyadic_dispute_data.get_COWWar(x);
                          Durindx := maoz_dyadic_dispute_data.get_DurIndx(x);
                          DurDays := maoz_dyadic_dispute_data.get_DurDays(x);

                          {Fatality, fatleva, outcome, and settlement are coded only in the last year of the dispute}
                          Fatality := maoz_dyadic_dispute_data.get_MID_fatality(x);
                          outcome := maoz_dyadic_dispute_data.get_MID_outcome (x);
                          settlement := maoz_dyadic_dispute_data.get_MID_settlement (x);
                          NumberA := maoz_dyadic_dispute_data.get_MID_numstates(x,ccodeA);
                          NumberB := maoz_dyadic_dispute_data.get_MID_numstates(x,ccodeB);

                          FatalityA := maoz_dyadic_dispute_data.get_fatality_state(x, ccodeA);
                          FatalityVA := missing_value;
                          OriginatorA := maoz_dyadic_dispute_data.get_originator(x,ccodeA);
                          SideA := maoz_dyadic_dispute_data.get_SideA(x, ccodeA);

                          FatalityB := maoz_dyadic_dispute_data.get_fatality_state(x, ccodeB);
                          FatalityVB := missing_value;
                          OriginatorB := maoz_dyadic_dispute_data.get_originator(x,ccodeB);
                          SideB := maoz_dyadic_dispute_data.get_SideA(x, ccodeB);

                          version := 1.1;   {Maoz data is v1.1, and that's what's used in this record.}
                       end;       {with final_dyadic_mid...}

                    {problem with the next check - not all the MID-years in a given MID are always together
                     in the Maoz structure.  That data is sorted by ccode1 - ccode2 - year [not start year]
                     so that all years in the same dyad are together.
                     So, this check doesn't always compare the right 2 records,
                     and so multiple records are being added that shouldn't.}
                     {Old code was:
                       {if (final_dyadic_mid.idnum=dyadic_dispute_data^[high(dyadic_dispute_data^)].idnum) and
                       (final_dyadic_mid.ccodeA=dyadic_dispute_data^[high(dyadic_dispute_data^)].ccodeA) and
                       (final_dyadic_mid.ccodeB=dyadic_dispute_data^[high(dyadic_dispute_data^)].ccodeB) then }
                     {for new code, maybe need to loop backward through each MID until a prior start year is found to be
                     sure that checks are made correctly.}

                    add_new := false;
                    update_old := false;
                    matching_case_num := -1;

                       {if this is the 2nd year + of a maoz dispute, then need to check it.}
                    if final_dyadic_mid.stYear = maoz_dyadic_dispute_data.get_year(x) then
                       begin
                          {it's a new dispute, so we can add it as new.}
                          add_new := true;
                          update_old := false;
                       end
                    else    {otherwise check to see if we want to add new, or update.
                             Since I know this is the 2nd year, I should always be updating at this
                             point.}
                       begin
                          done_check := false;
                          case_to_check := high(dyadic_dispute_data^);
                          repeat
                             {check one MID}
                             if ((final_dyadic_mid.idnum=dyadic_dispute_data^[case_to_check].idnum) and
                                 (final_dyadic_mid.ccodeA=dyadic_dispute_data^[case_to_check].ccodeA) and
                                 (final_dyadic_mid.ccodeB=dyadic_dispute_data^[case_to_check].ccodeB)) then
                                begin
                                   update_old := true;
                                   add_new := false;
                                   matching_case_num := case_to_check;
                                   done_check := true;   {can be done with loop, b/c found a match.}
                                end;
                             {Also, check to see if I've looked back far enough.
                              That means going back to another dyad.  So, I'm checking this 2nd year
                              against all cases within the dyad.  I should always find a match, though,
                              if the maoz data is correct.}
                             if ((final_dyadic_mid.ccodeA <> dyadic_dispute_data^[case_to_check].ccodeA) or
                                 (final_dyadic_mid.ccodeB <> dyadic_dispute_data^[case_to_check].ccodeB) ) then
                                done_check := true;
                             dec (case_to_check);
                          until ((done_check) or (case_to_check < low(dyadic_dispute_data^)) or (update_old=true));
                          {I should never exit that loop without having found a match.  So check this.}
                          if done_check and not (update_old = true) then
                             EUGeneError ('programming error - maoz data 2nd year search loop exited without finding match.  Notify programmer of error.',1,continue,error_log);
                       end;
                       
                    {Now, I either know I must add new, or must update an existing record}
                    if update_old then
                       begin
                          if add_new then EUGeneError ('programming error in maoz 2nd year search.  updateold and addnew both true. ',1,continue,error_log);
                          {Since it's a subsequent record, then we want to keep only the values for
                           Fatality, fatleva, outcome, and settlement, and replace those in the
                           current record.}
                          dyadic_dispute_data^[matching_case_num].fatality := final_dyadic_mid.fatality;
                          dyadic_dispute_data^[matching_case_num].outcome := final_dyadic_mid.outcome;
                          dyadic_dispute_data^[matching_case_num].settlement := final_dyadic_mid.settlement;
                          dyadic_dispute_data^[matching_case_num].FatalityA := final_dyadic_mid.FatalityA;
                          dyadic_dispute_data^[matching_case_num].FatalityB := final_dyadic_mid.FatalityB;
                       end
                    else  if add_new then {it's a new dispute}
                       begin
                          if update_old then EUGeneError ('programming error in maoz 2nd year search.  updateold and addnew both true. ',1,continue,error_log);
                          {Add the final dyadic dispute to the list.}
                          setlength(dyadic_dispute_data^, length(dyadic_dispute_data^)+1);
                          new (final_dyadic_mid_ptr);
                          final_dyadic_mid_ptr^ := final_dyadic_mid;
                          dyadic_dispute_data^[high(dyadic_dispute_data^)] := final_dyadic_mid_ptr;
                       end
                    else
                       begin
                          EUGeneError ('programming error in maoz 2nd year search.  neither updateold nor addnew are true. ',1,continue,error_log);                       end;
                 end;
           num_dyadic_disputes := length(dyadic_dispute_data^);
           added_cow_or_maoz := added_maoz;

{trace.message ('Checking created dyadic structure including maoz disps, after creation.');
trace.message (inttostr(length(dyadic_dispute_data^))+' disputes in structure after maoz added to PI recs, inside add procedure.');
for x := 0 to high(dyadic_dispute_data^) do
   begin
      if (dyadic_dispute_data^[x].StYear = 1994) and (dyadic_dispute_data^[x].ccodeA=710) and (dyadic_dispute_data^[x].ccodeB=2) then trace.message ('Saw requested rec in maoz after creation as record'+inttostr(x));
      if (dyadic_dispute_data^[x].ccodeA=710) and (dyadic_dispute_data^[x].ccodeB=2) then trace.message ('Saw 710-2 rec in maoz as record '+inttostr(x)+' with start year '+inttostr(dyadic_dispute_data^[x].StYear)+ ' and end year '+inttostr(dyadic_dispute_data^[x].EndYear) );
   end;
}
           maoz_dyadic_dispute_data.free;
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_ccode (dyadic_dispute_num : longint; which_state : side_type) : ccode_range;
                   {given number of a dyadic dispute record, returns
                    the ccode of the specified particpant in the dyadic dispute}
        begin
           try
              check_initialized;
              if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
              if which_state = 0 then
                 result := dyadic_dispute_data^[dyadic_dispute_num]^.ccodeA
              else if which_state=1 then
                 result := dyadic_dispute_data^[dyadic_dispute_num]^.ccodeB;
           except
              result := 0;
           end;  {except}
        end;

   {The first set of functions are for variables that were recomputed in the version 3.0
    dyadic set up.}
     function TDyadic_dispute_data_obj_integrated_format30.get_MID_num (dyadic_dispute_num : longint) : dispute_id_range;
      begin
         if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
            if dyadic_dispute_data^[dyadic_dispute_num] = nil then result := 0 else
               result := dyadic_dispute_data^[dyadic_dispute_num]^.idnum;
      end;

     function TDyadic_dispute_data_obj_integrated_format30.get_dyadic_MID_num (dyadic_dispute_num : longint) : dyadic_dispute_id_range;
      begin
         if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
            if dyadic_dispute_data^[dyadic_dispute_num] = nil then result := 0 else
               result := dyadic_dispute_data^[dyadic_dispute_num]^.dyadic_idnum;
      end;

     function TDyadic_dispute_data_obj_integrated_format30.get_first_year (dyadic_dispute_num : longint) : year_range;
        begin
           try
              check_initialized;
              if dyadic_dispute_num = null_dispute_number then {no dispute} result := min_year else
              result := dyadic_dispute_data^[dyadic_dispute_num]^.styear;
           except
              showmessage ('error in get first year for disp num '+inttostr(dyadic_dispute_num));
           end;
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_last_year (dyadic_dispute_num : longint) : year_range;
        begin
           check_initialized;
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := min_year else
           result := dyadic_dispute_data^[dyadic_dispute_num]^.endyear;
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_hostlev_state (dyadic_dispute_num : longint; ccode: ccode_range) : hostlevtype;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.HostLevA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.HostLevB)
           else {ccode not found}
              showmessage ('Programming error in dyadic 30.get_hostlev - ccode '+inttostr(ccode)+' in dyadic disp num '+inttostr(dyadic_dispute_num)+ ' not in record.  Notify programmer.');
           {Also, want to change coding as a "-9" hostility to a 1, which means
            no militarized response, although you were a target.}
           if (result = -9) then result := 1;
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_HiAct_state (dyadic_dispute_num : longint; ccode: ccode_range) : actiontype;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.HiActA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.HiActB)
           else {ccode not found}
              showmessage ('Programming error in dyadic 30.get_hiact - ccode not in record.  Notify programmer.');
        end;

    function TDyadic_dispute_data_obj_integrated_format30.get_reciprocated_dyadic (dyadic_dispute_num : longint) : boolean;
       begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := false else
           result := (dyadic_dispute_data^[dyadic_dispute_num]^.ReciprocatedMID);
       end;

     function TDyadic_dispute_data_obj_integrated_format30.get_sideA (dyadic_dispute_num : longint; ccode: ccode_range) : boolean;
        {get sideA for overall dispute here.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := false else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.SideA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.SideB)
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_sideA - ccode not in record.  Notify programmer.');
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_originator (dyadic_dispute_num : longint;
              ccode: ccode_range) : boolean;
        {true if was originator in MID coding.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := false else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.originatorA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.originatorB)
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_originator - ccode not in record.  Notify programmer.');
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_revisionist (dyadic_dispute_num : longint;
              ccode: ccode_range) : boolean;
        {True if was dyadic revisionist in MID coding - adjusted for dyad.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := false else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.revisionistA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.revisionistB)
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_revisionist - ccode not in record.  Notify programmer.');
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_styear (dyadic_dispute_num : longint; ccode: ccode_range) : year_range;
        begin
           check_initialized;
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := min_year else
           result := dyadic_dispute_data^[dyadic_dispute_num]^.styear;
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_stmonth (dyadic_dispute_num : longint; ccode: ccode_range) : month_range;
        begin
           check_initialized;
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           result := dyadic_dispute_data^[dyadic_dispute_num]^.stmonth;
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_stday (dyadic_dispute_num : longint; ccode: ccode_range) : MIDdaytype;
        begin
           check_initialized;
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           result := dyadic_dispute_data^[dyadic_dispute_num]^.stday;
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_endyear (dyadic_dispute_num : longint; ccode: ccode_range) : year_range;
        begin
           check_initialized;
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := min_year else
           result := dyadic_dispute_data^[dyadic_dispute_num]^.endyear;
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_endmonth (dyadic_dispute_num : longint; ccode: ccode_range) : month_range;
        begin
           check_initialized;
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           result := dyadic_dispute_data^[dyadic_dispute_num]^.endmonth;
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_endday (dyadic_dispute_num : longint; ccode: ccode_range) : MIDdaytype;
        begin
           check_initialized;
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           result := dyadic_dispute_data^[dyadic_dispute_num]^.endday;
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_revtype (dyadic_dispute_num : longint; ccode: ccode_range) : revisiontype;
        {returns highest MID coding of revision type sought.}
        begin
           check_initialized;
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.RevType_A1)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.RevType_B1)
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_revisionist - ccode not in record.  Notify programmer.');
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_revtype2 (dyadic_dispute_num : longint; ccode: ccode_range) : revisiontype;
        {returns second highest MID coding of revision type sought.}
        begin
           check_initialized;
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.RevType_A2)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.RevType_B2)
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_revtype2 - ccode not in record.  Notify programmer.');
        end;


     function TDyadic_dispute_data_obj_integrated_format30.get_fatality_state (dyadic_dispute_num : longint; ccode: ccode_range) : fatalitytype;
        {get fatality for state for overall dispute here.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.fatalityA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.fatalityB)
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_fatality_state - ccode not in record.  Notify programmer.');
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_version (dyadic_dispute_num : longint) : real;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
              begin
                 result := (dyadic_dispute_data^[dyadic_dispute_num]^.version);
              end;
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_MID_name (dyadnum : longint) : string;
        begin
           if dyadnum = null_dispute_number then {no dispute} result := '' else
           if overall_disputes.have_info_on_midnum(get_mid_num(dyadnum)) then
              result := overall_disputes.get_dispute_name(get_mid_num(dyadnum));
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_MID_numstates (dyadnum : longint; ccode: ccode_range) : integer;
        {get number of states on either side - from dyadic data set.}
        begin
           if dyadnum = null_dispute_number then {no dispute} result := 0 else
           if (get_ccode(dyadnum, 0) = ccode) then
              result := (dyadic_dispute_data^[dyadnum]^.NumberA)
           else
           if (get_ccode(dyadnum, 1) = ccode) then
              result := (dyadic_dispute_data^[dyadnum]^.NumberB)
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_MID_numstates - ccode not in record.  Notify programmer.');
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_MID_outcome (dyadnum : longint) : outcometype;
        begin
           if dyadnum = null_dispute_number then {no dispute} result := 0 else
           result := (dyadic_dispute_data^[dyadnum]^.outcome);
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_MID_settlement (dyadnum : longint) : settlementtype;
        begin
           if dyadnum = null_dispute_number then {no dispute} result := 0 else
           result := (dyadic_dispute_data^[dyadnum]^.Settlement);
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_MID_fatality (dyadnum : longint) : fatalitytype;
        begin
           if dyadnum = null_dispute_number then {no dispute} result := 0 else
           result := (dyadic_dispute_data^[dyadnum]^.Fatality);
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_MID_hiact (dyadnum : longint) : actiontype;
        begin
           if dyadnum = null_dispute_number then {no dispute} result := 0 else
           result := (dyadic_dispute_data^[dyadnum]^.HiAct);
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_MID_hostlev (dyadnum : longint) : hostlevtype;
        begin
           if dyadnum = null_dispute_number then {no dispute} result := 0 else
           result := (dyadic_dispute_data^[dyadnum]^.HostLev);
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_MID_reciprocated (dyadnum : longint) : boolean;
        begin
           if dyadnum = null_dispute_number then {no dispute} result := false else
           result := (dyadic_dispute_data^[dyadnum]^.ReciprocatedMID);
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_fatality_state_precise (dyadic_dispute_num : longint; ccode: ccode_range) : integer;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.fatalityVA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.fatalityVB)
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_fatality_state_precise - ccode not in record.  Notify programmer.');
        end;

    function TDyadic_dispute_data_obj_integrated_format30.get_sideA_dyadic (dyadic_dispute_num : longint; ccode: ccode_range) : boolean;
        {get sideA for dyadic dispute here.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := false else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.SideAA_dyadic)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.SideAB_dyadic)
           else {ccode not found}
              showmessage ('Programming error in dyadic.get_sideAA - ccode not in record.  Notify programmer.');
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_Role (dyadic_dispute_num : longint; ccode: ccode_range) : Roletype;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.RoleA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (dyadic_dispute_data^[dyadic_dispute_num]^.RoleB)
           else {ccode not found}
              showmessage ('Programming error in COW 3.0 dyadic.get_Role - ccode not in record.  Notify programmer.');
        end;

     function TDyadic_dispute_data_obj_integrated_format30.has_midnum_from_pi_data (mid_num : dispute_id_range) : boolean;
        begin
           result := pi_midnum_list[mid_num];
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_link1 (dyadic_dispute_num : longint) : string;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := '' else
           if overall_disputes.have_info_on_midnum(get_mid_num(dyadic_dispute_num)) then
              result := overall_disputes.get_link1(get_mid_num(dyadic_dispute_num));
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_link2 (dyadic_dispute_num : longint) : string;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := '' else
           if overall_disputes.have_info_on_midnum(get_mid_num(dyadic_dispute_num)) then
              result := overall_disputes.get_link2(get_mid_num(dyadic_dispute_num));
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_link3 (dyadic_dispute_num : longint) : string;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := '' else
           if overall_disputes.have_info_on_midnum(get_mid_num(dyadic_dispute_num)) then
              result := overall_disputes.get_link3(get_mid_num(dyadic_dispute_num));
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_ongoing2001 (dyadic_dispute_num : longint) : boolean;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := false else
           if overall_disputes.have_info_on_midnum(get_mid_num(dyadic_dispute_num)) then
              result := overall_disputes.get_ongoing_2001(get_mid_num(dyadic_dispute_num));
        end;

     {Maoz only funcs}
     function TDyadic_dispute_data_obj_integrated_format30.get_COWWar (dyadic_dispute_num : longint) : boolean;
        begin
           if self.added_cow_or_maoz = added_maoz then
              begin
                 if dyadic_dispute_num = null_dispute_number then {no dispute} result := false else
                 result := dyadic_dispute_data^[dyadic_dispute_num]^.COWWar;
              end
           else
              begin
                 EUGeneError ('Tried to get_cowWar from integrated structure, but structure not initialized as maoz data.  Programming error - notify programmer.  Program continues, but problem.',1,continue,error_log);
                 result := false;
              end;
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_DurIndx (dyadic_dispute_num : longint) : integer;
        begin
           if self.added_cow_or_maoz = added_maoz then
              begin
                 if dyadic_dispute_num = null_dispute_number then {no dispute} result := missing_value else
                 result := dyadic_dispute_data^[dyadic_dispute_num]^.DurIndx;
              end
           else
              begin
                 EUGeneError ('Tried to get_DurIndx from integrated structure, but structure not initialized as maoz data.  Programming error - notify programmer.  Program continues, but problem.',1,continue,error_log);
                 result := missing_value;
              end;
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_DurDays (dyadic_dispute_num : longint) : integer;
        begin
           if self.added_cow_or_maoz = added_maoz then
              begin
                 if dyadic_dispute_num = null_dispute_number then {no dispute} result := missing_value else
                 result := dyadic_dispute_data^[dyadic_dispute_num]^.DurDays;
              end
           else
              begin
                 EUGeneError ('Tried to get_DurDays from integrated structure, but structure not initialized as maoz data.  Programming error - notify programmer.  Program continues, but problem.',1,continue,error_log);
                 result := missing_value;
              end;
        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_corresponding_maoz_dispnum (cow_num : longint; var COW_dyadic_dispute_data_30 : TDyadic_dispute_data_obj_integrated_format30) : longint;
        {given a COW dispute record num, finds the corresponding number to access the maoz data structure.
         Called within the Maoz structure.
         Assumes cc1, 2 are directed, so doesn't look for the reverse direction.}
        var found : boolean;
            mid_num_needed : dispute_id_range;
            ccode1_needed, ccode2_needed : ccode_range;
            disputenum : longint;
        begin
           if (not (self.added_cow_or_maoz = added_maoz)) then EUGeneError ('called get maoz dispnum on a data structure not initialized as maoz data.  Programming error - notify programmer.',1,continue,error_log);
           result := null_dispute_number;
           found := false;
           mid_num_needed := COW_dyadic_dispute_data_30.get_MID_num(cow_num);
           ccode1_needed := COW_dyadic_dispute_data_30.get_ccode(cow_num, 0);
           ccode2_needed := COW_dyadic_dispute_data_30.get_ccode(cow_num, 1);
           disputenum := index^[ccode1_needed, ccode2_needed];
           if disputenum <> null_dispute_number then
              repeat
                {need to check all disputes starting with this until the match is seen}
                 if (self.get_MID_num(disputenum) = mid_num_needed) and
                    (self.get_ccode(disputenum,0) = ccode1_needed) and
                    (self.get_ccode(disputenum,1) = ccode2_needed)
                 then
                    begin
                       found := true;
                       result := disputenum;
                    end;
                 inc(disputenum);
              until ((found=true) or
                     (disputenum > self.get_last_dispnum) or
                     (self.get_ccode(disputenum, 0) <> ccode1_needed) or
                     (self.get_ccode(disputenum, 1) <> ccode2_needed));

        end;

     function TDyadic_dispute_data_obj_integrated_format30.get_corresponding_cow_dispnum (maoz_num : longint; var Maoz_dispute_data : TDyadic_dispute_data_obj_integrated_format30) : longint;
        var found : boolean;
            mid_num_needed : dispute_id_range;
            ccode1_needed, ccode2_needed : ccode_range;
            disputenum : longint;
        begin
           if (not (self.added_cow_or_maoz = added_cow)) then EUGeneError ('called get cow dispnum on a data structure not initialized as cow data.  Programming error - notify programmer.',1,continue,error_log);
           result := null_dispute_number;
           found := false;
           mid_num_needed := Maoz_dispute_data.get_MID_num(maoz_num);
           ccode1_needed := Maoz_dispute_data.get_ccode(maoz_num, 0);
           ccode2_needed := Maoz_dispute_data.get_ccode(maoz_num, 1);
           disputenum := index^[ccode1_needed, ccode2_needed];
           if disputenum <> null_dispute_number then
              repeat
                {need to check all disputes starting with this until the match is seen}
                 if (self.get_MID_num(disputenum) = mid_num_needed) and
                    (self.get_ccode(disputenum,0) = ccode1_needed) and
                    (self.get_ccode(disputenum,1) = ccode2_needed)
                 then
                    begin
                       found := true;
                       result := disputenum;
                    end;
                 inc(disputenum);
              until ((found=true) or
                     (disputenum > self.get_last_dispnum) or
                     (self.get_ccode(disputenum, 0) <> ccode1_needed) or
                     (self.get_ccode(disputenum, 1) <> ccode2_needed));
        end;

        {  ------------------------------------------------  }

     function TDyadic_dispute_data_obj_integrated_format30.convert_hiact_v21_to_v30 (hiact_21 : actiontype) : actiontype;
        {function converts highest action from v2.1 range (1-23) to v3.0 range (0-21).}
        begin
          case hiact_21 of
            -9 : result := -9;
            0 : EUGeneError ('Error in conversion of highest action level for v21 to v30 format.  Input value was '+inttostr(hiact_21)+'. Setting to missing, but notify programmer of error.', 5, continue, error_log);
            1 : result := 0;
            2 : result := 1;
            3 : result := 2;
            4 : result := 3;
            5 : result := 4;
            6 : result := 5;      {???}  {6 never appears as a possible category for converted data.}
            7 : result := 7;
            8 : result := 7;
            9 : result := 7;
            10 : result := 8;
            11 : result := 9;
            12 : result := 10;
            13 : result := 11;
            14 : result := 12;
            15 : result := 13;
            16 : result := 14;
            17 : result := 15;
            18 : result := 16;
            19 : result := 17;
            20 : result := 18;
            21 : result := 19;
            22 : result := 20;
            23 : result := 21;
            else
               begin
                  EUGeneError ('Error in conversion of highest action level for v21 to v30 format.  Input value was '+inttostr(hiact_21)+'. Setting to missing, but notify programmer of error.', 5, continue, error_log);
                  result := -9;
               end;
          end;
        end;

        {  ------------------------------------------------  }


     {Now, MAOZ dyadic dispute object}

     {public functions}
     constructor TMaoz_Dyadic_dispute_data_obj.init (user_selections : user_selection_type;
                  configuration : configuration_type; year1, year2 : year_range);
      {init reads the Maoz dyadic dispute year data.
       It also sorts the dyadic dispute data by ccode1, ccode2, then start year of dispute.}

        var Dyadic_dispute_trace : TTrace_obj;
            heapneeded, start_mem : longint;
            x, y, current_rec, tempint : integer;
            a_rec : Maoz_dyadic_dispute_main_rec_ptr;
            done, bad_case : boolean;
            infile : text;

         procedure quicksort_dyadic_disputes_by_country (left, right:integer; sort_dyad:side_type);
                  {Sorts in order low to high ccode between left and right;
                   Will sort by country 1 or country 2 depending on value of sort_dyad.
                   Note that now, for Maoz dyads, sort_dyad will indicate to sorty by
                   ccodeA or ccodeB.  }
            const trace_quicksort = false;
            var
               base_ccode, i, j, comp_ccode : integer;
               temp: maoz_dyadic_dispute_main_rec;
            begin
             if right > left then
               begin
                  {v needs to be set to value of right case, ccode of country 1 or ccode of country 2}
                  if sort_dyad = 0 then base_ccode := data^[right]^.ccodeA else
                                        base_ccode := data^[right]^.ccodeB;
                  i := left - 1;
                  j := right;
                  repeat
                     repeat
                        i := i + 1;
                        if sort_dyad = 0 then comp_ccode := data^[i]^.ccodeA else
                                              comp_ccode := data^[i]^.ccodeB;
                     until comp_ccode >= base_ccode;
                     repeat
                        j := j - 1;
                        if sort_dyad = 0 then comp_ccode := data^[j]^.ccodeA else
                                              comp_ccode := data^[j]^.ccodeB;
                     until (comp_ccode <= base_ccode) or (j = left);
                     temp := data^[i]^;
                     data^[i]^ := data^[j]^;
                     data^[j]^ := temp;
                  until j <= i;
                  data^[j]^ := data^[i]^;
                  data^[i]^ := data^[right]^;
                  data^[right]^ := temp;
                  quicksort_dyadic_disputes_by_country (left, i-1, sort_dyad);
                  quicksort_dyadic_disputes_by_country (i+1, right, sort_dyad);
               end;
            end;      {procedure quicksort_dyads_by_country}

            {   -----------------------  }

         procedure quicksort_dyadic_disputes_by_year (left, right:integer);
                  {Sorts in order low to high}
            var
               base_year, comp_year, i, j: integer;
               temp: maoz_dyadic_dispute_main_rec;

            begin
             if right > left then
               begin
                  base_year := data^[right]^.year;
                  i := left - 1;
                  j := right;
                  repeat
                     repeat
                        i := i + 1;
                        comp_year := data^[i]^.year;
                     until comp_year >= base_year;
                     repeat
                        j := j - 1;
                        comp_year := data^[j]^.year;
                     until (comp_year <= base_year) or (j = left);
                     temp := data^[i]^;
                     data^[i]^ := data^[j]^;
                     data^[j]^ := temp;
                  until j <= i;
                  data^[j]^ := data^[i]^;
                  data^[i]^ := data^[right]^;
                  data^[right]^ := temp;
                  quicksort_dyadic_disputes_by_year (left, i-1);
                  quicksort_dyadic_disputes_by_year (i+1, right);
               end;
            end;      {procedure quicksort_dyads_by_year}

            {   -----------------------  }

         procedure switch_maoz_order (var some_disp_rec : Maoz_dyadic_dispute_main_rec_ptr);
             {this proc switches the order of A and B in the dispute-year record.  This
              will happen if A is not the initiator or revisionist state, as appropriate}
             var tempint : integer;
                 tempstring : string;
                 tempbool : boolean;
            begin
               tempint := some_disp_rec^.ccodeA;
               some_disp_rec^.ccodeA := some_disp_rec^.ccodeB;
               some_disp_rec^.ccodeB := tempint;

               tempstring := some_disp_rec^.cabbrevA;
               some_disp_rec^.cabbrevA := some_disp_rec^.cabbrevB;
               some_disp_rec^.cabbrevB := tempstring;

               tempint := some_disp_rec^.NumberA;
               some_disp_rec^.NumberA := some_disp_rec^.NumberB;
               some_disp_rec^.NumberB := tempint;

               tempint := some_disp_rec^.StDayA;
               some_disp_rec^.StDayA := some_disp_rec^.StDayB;
               some_disp_rec^.StDayB := tempint;

               tempint := some_disp_rec^.EndDayA;
               some_disp_rec^.EndDayA := some_disp_rec^.EndDayB;
               some_disp_rec^.EndDayB := tempint;

               tempint := some_disp_rec^.StMonthA;
               some_disp_rec^.StMonthA := some_disp_rec^.StMonthB;
               some_disp_rec^.StMonthB := tempint;

               tempint := some_disp_rec^.EndMonthA;
               some_disp_rec^.EndMonthA := some_disp_rec^.EndMonthB;
               some_disp_rec^.EndMonthB := tempint;

               tempint := some_disp_rec^.StYearA;
               some_disp_rec^.StYearA := some_disp_rec^.StYearB;
               some_disp_rec^.StYearB := tempint;

               tempint := some_disp_rec^.EndYearA;
               some_disp_rec^.EndYearA := some_disp_rec^.EndYearB;
               some_disp_rec^.EndYearB := tempint;

               tempbool := some_disp_rec^.SideAA;
               some_disp_rec^.SideAA := some_disp_rec^.SideAB;
               some_disp_rec^.SideAB := tempbool;

               tempbool := some_disp_rec^.revisionistA;
               some_disp_rec^.revisionistA := some_disp_rec^.revisionistB;
               some_disp_rec^.revisionistB := tempbool;

               tempint := some_disp_rec^.revtypeA;
               some_disp_rec^.revtypeA := some_disp_rec^.revtypeB;
               some_disp_rec^.revtypeB := tempint;

               tempint := some_disp_rec^.FatalityA;
               some_disp_rec^.FatalityA := some_disp_rec^.FatalityB;
               some_disp_rec^.FatalityB := tempint;

               tempint := some_disp_rec^.HiActA;
               some_disp_rec^.HiActA := some_disp_rec^.HiActB;
               some_disp_rec^.HiActB := tempint;

               tempint := some_disp_rec^.HostLevA;
               some_disp_rec^.HostLevA := some_disp_rec^.HostLevB;
               some_disp_rec^.HostLevB := tempint;

               tempbool := some_disp_rec^.OriginatorA;
               some_disp_rec^.OriginatorA := some_disp_rec^.OriginatorB;
               some_disp_rec^.OriginatorB := tempbool;

               tempint := some_disp_rec^.RoleA;
               some_disp_rec^.RoleA := some_disp_rec^.RoleB;
               some_disp_rec^.RoleB := tempint;

            end;    {proc switch_maoz_order}

            {   -----------------------  }

        begin   {main proc maoz dyadic data init}
           dyadic_dispute_trace := nil;
           try
               trace.enter('Initializing Maoz dyadic dispute data, ');
               if year1 > year2 then switch_year (year1, year2);
               self.first_partition_year := year1;
               self.last_partition_year := year2;

               start_mem := memavail;
               heapneeded := TMaoz_dyadic_dispute_obj_mem_overhead;
               if debug[4] then
                  begin
                     trace.message ('Dyadic Dispute array size calculation');
                     trace.message ('Calc is that '+inttostr(heapneeded)+' needed for all years.');
                     trace.message ('Max avail mem block is '+inttostr(MaxAvail));
                  end;
               if MaxAvail <= (heapneeded) then
                  begin
                     EUGeneError ('Not enough memory for dyadic dispute array. ',
                                     5, stop, error_log);
                  end;

               Dyadic_dispute_trace := TTrace_obj.init(trace.get_trace_level);
               trace.message ('Initializing Maoz Dyadic dispute data structure');

               new (data);
               setlength (data^, max_dyadic_dispute_years);
               num_dyadic_dispute_years := 0;
               for x := 0 to max_dyadic_dispute_years-1 do data^[x] := nil;

               new (index);
               for x := 1 to top_nation_num do
                  for y := 1 to top_nation_num do
                     index^[x,y] := null_dispute_number;

               {Now read Maoz data from file }

               try
                  assign (infile, configuration.maoz_dyadic_mid_file_name);
                  reset (infile);
                  current_rec := 0;

                  {First line is header, skip it}
                  readln (infile);

                  while (not (eof (infile))) and (current_rec <= max_dyadic_dispute_years) do
                     begin
                        Dyadic_dispute_trace.tick ('Executing Procedure: Read Maoz Dyadic Disputes',max_dyadic_dispute_years);
                        new(a_rec);

                        {Note a couple of differences in variables from v1.0 to v1.1.}
                        {Version 1.1 Variable order:
                        DISNO	STATEA	NAMEA	STATEB	NAMEB	STRTDAY	STRTMNTH	STRTYR	YEAR	ENDDAY	ENDMNTH	ENDYEAR
                        OUTCOME	SETTLMNT	FATLEV
                        MAXDUR	MINDUR
                        HIGHACT	HIHOST	RECIP	NOINIT	NOTARG
                        STDAYA	STMNTHA	STYEARA	ENDDAYA	ENDMNTHA	ENDYEARA	SIDEAA
                        REVSTATA	REVTYPEA	FATALEVA	HIGHMCAA	HIHOSTA	ORIGNATA	STDAYB	STMNTHB	STYEARB
                        ENDDAYB	ENDMNTHB	ENDYEARB	SIDEAB	REVSTATE	REVTYPEB	FATALEVB	HIGHMCAB	HIHOSTB
                        ORIGNATB	ROLEA	ROLEB	WAR	Durindx	Duration        }

                        {Version 1.0 Variable order:
                        DISNO	STATEA	NAMEA	STATEB	NAMEB	STRTDAY	STRTMNTH	STRTYR	YEAR	ENDDAY	ENDMNTH	ENDYEAR
                        OUTCOME	SETTLMNT	FATLEV
                        HIGHACT	HIHOST	RECIP	NOINIT	NOTARG
                        STDAYA	STMNTHA	STYEARA ENDDAYA	ENDMNTHA	ENDYEARA	SIDEAA
                        REVSTATA	REVTYPEA	FATALEVA	HIGHMCAA	HIHOSTA ORIGNATA	STDAYB	STMNTHB	STYEARB
                        ENDDAYB	ENDMNTHB	ENDYEARB	SIDEAB REVSTATE	REVTYPEB	FATALEVB	HIGHMCAB	HIHOSTB
                        ORIGNATB	ROLEA	ROLEB	WAR  Durindx	Duration
                        Reciproc	NEWMID              }

                        a_rec^.idnum := read_csv_int (infile);
                        if a_rec^.idnum > max_dyadic_dispute_years then
                           EUGeneError ('Dispute ID num read from Maoz dyadic MID file is out of range, > longint.  Notify programmer.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.ccodeA := read_csv_int (infile);
                        {next record is 3 char abbrev for A.  }
                        a_rec^.cabbrevA := read_csv_string (infile);
                        a_rec^.ccodeB := read_csv_int (infile);
                        a_rec^.cabbrevB := read_csv_string (infile);

                        a_rec^.StDay := read_csv_int (infile);
                        a_rec^.StMonth := read_csv_int (infile);
                        a_rec^.StYear := read_csv_int (infile);
                        a_rec^.Year := read_csv_int (infile);
                        a_rec^.EndDay := read_csv_int (infile);
                        a_rec^.EndMonth := read_csv_int (infile);
                        a_rec^.EndYear := read_csv_int (infile);

                        a_rec^.outcome := read_csv_int (infile);
                        a_rec^.settlement := read_csv_int (infile);
                        a_rec^.fatality := read_csv_int (infile);

                        {Version 1.1 has 2 undocumented variables, maxdur and mindur, here.
                         Skip them.}
                        tempint := read_csv_int (infile);
                        tempint := read_csv_int (infile);

                        a_rec^.hiact := read_csv_int (infile);
                        a_rec^.hostlev := read_csv_int (infile);
                        tempint := read_csv_int (infile);
                        if tempint=-1 then a_rec^.reciprocatedMID := true else a_rec^.reciprocatedMID := false;
                        a_rec^.NumberA := read_csv_int (infile);
                        a_rec^.NumberB := read_csv_int (infile);

                        a_rec^.StDayA := read_csv_int (infile);
                        a_rec^.StMonthA := read_csv_int (infile);
                        a_rec^.StYearA := read_csv_int (infile);
                        a_rec^.EndDayA := read_csv_int (infile);
                        a_rec^.EndMonthA := read_csv_int (infile);
                        a_rec^.EndYearA := read_csv_int (infile);
                        tempint := read_csv_int (infile);
                        if tempint=1 then
                           a_rec^.SideAA := true else
                           a_rec^.SideAA := false;
                        tempint := read_csv_int (infile);
                        if tempint=1 then a_rec^.revisionistA := true else a_rec^.revisionistA := false;
                        a_rec^.revtypeA := read_csv_int (infile);
                        a_rec^.FatalityA := read_csv_int (infile);
                        a_rec^.HiActA := read_csv_int (infile);
                        a_rec^.HostLevA := read_csv_int (infile);
                           {FOR originator variable, data has a -1=yes, 0=no}
                        tempint := read_csv_int (infile);
                        if tempint=-1 then a_rec^.OriginatorA := true else a_rec^.originatorA := false;

                        a_rec^.StDayB := read_csv_int (infile);
                        a_rec^.StMonthB := read_csv_int (infile);
                        a_rec^.StYearB := read_csv_int (infile);
                        a_rec^.EndDayB := read_csv_int (infile);
                        a_rec^.EndMonthB := read_csv_int (infile);
                        a_rec^.EndYearB := read_csv_int (infile);
                        tempint := read_csv_int (infile);
                        if tempint=1 then a_rec^.SideAB := true else a_rec^.SideAB := false;
                        tempint := read_csv_int (infile);
                        if tempint=1 then a_rec^.revisionistB := true else a_rec^.revisionistB := false;
                        a_rec^.revtypeB := read_csv_int (infile);
                        a_rec^.FatalityB := read_csv_int (infile);
                        a_rec^.HiActB := read_csv_int (infile);
                        a_rec^.HostLevB := read_csv_int (infile);
                           {FOR originator variable, data has a -1=yes, 0=no}
                        tempint := read_csv_int (infile);
                        if tempint=-1 then a_rec^.OriginatorB := true else a_rec^.originatorB := false;

                        a_rec^.roleA := read_csv_int (infile);
                        a_rec^.roleB := read_csv_int (infile);
                        tempint := read_csv_int (infile);
                        if tempint=1 then a_rec^.COWWar := true else a_rec^.COWWar := false;
                        a_rec^.durindx := read_csv_int (infile);
                        a_rec^.durDays := read_csv_int (infile);

                        {The two variables ReciprocatedDyadic and NewMID are not in v1.1,
                         so don't try to read them.}
                        {tempint := read_csv_int (infile);
                        if tempint=1 then a_rec^.ReciprocatedDyadic := true else a_rec^.ReciprocatedDyadic := false;
                        tempint := read_csv_int (infile);
                        if tempint=1 then a_rec^.NewMID := true else a_rec^.NewMID := false; }

                        if (not (eof (infile))) then
                           readln (infile);      {go to begin of next record}

                        {Just successfully read a dispute:  Put it on the dispute list
                         (but then check to see if it occasionally has to be removed)}
                        data^[current_rec] := a_rec;
                        inc(current_rec);

                              {There is an occasional (1 case) problem with the COW MID data,
                               where zimbabwe supposedly has a 1965 dispute but isn't a state
                               until 1966.  This could be a symptom of other problems, though, so do
                               a check, and if a case is listed as a dispute in a year but the states aren't
                               really states according to COW, drop it.}
                        if (not(nation_list.is_a_state(a_rec^.ccodeA, a_rec^.year)) or
                            not(nation_list.is_a_state(a_rec^.ccodeB, a_rec^.year)) ) then
                           begin
                              trace.message ('Dropping COW MID where ccode '+inttostr(a_rec^.ccodeA)+ ' or '+ inttostr(a_rec^.ccodeB)+
                                       ' is not a COW state in the year of the Maoz dyadic dispute, '+inttostr(a_rec^.year));
                              {To drop this case, just decrement the counter so that the next dispute
                               will be assigned to the slot again.}
                              dec(current_rec);
                              data^[current_rec] := nil;
                              bad_case := true;
                              dispose (a_rec);
                           end

                     end;             {while not eof (infile);}

                     num_dyadic_dispute_years := current_rec;
                     if num_dyadic_dispute_years >= max_dyadic_dispute_years then
                        begin
                           ShowMessage ('Stopped reading Maoz dyadic dispute data at record '+inttostr(num_dyadic_dispute_years)+
                                    ';  maximum number of dyadic dispute years is '+inttostr(max_dyadic_dispute_years));
                        end;

                     Dyadic_dispute_trace.tickdone;
                     created := true;

                  {Now have all Maoz data (dyad-year) read into structure.
                   But need to sort data, and put sideA first always.  Every dispute should only
                   have one one sideA and one on side B, So resort to be side A first
                   so that it is directed.
                   BUT - which state is first, and so the initiator, will depend on user
                   selections for revisionist initiators, or side A initiators.
                   Somebody is always on side A.
                   Are there ever cases where neither is revisionist?  Yes.  These are cases that,
                   if the user says they want only revisionist initiators, eventually must not be
                   printed as initiations.}

                   {First, go through and put the appropriate initiator/revisionist as state A.}
                     For current_rec := 0 to num_dyadic_dispute_years-1 do
                        begin
                           {If user wants SideA initiators, make sure Side A is 1st in Dyad.
                            If user wants revisionist, make sure revisionist is first in Dyad.
                            If 2 are coded as revisionist, then make sure the one that is revisionist
                            and on side A is first.  If they want revisionist and neither is revisionist,
                            then make sure Side A is first in dyad.  }
                              if ((user_selections.dispute_info.SideAIsInitiator) and
                                  (data^[current_rec]^.sideAA = true))  then
                                   {then OK}
                                 begin
                                      {do nothing, no switch needed}
                                 end
                              else if ((user_selections.dispute_info.SideAIsInitiator) and
                                  (data^[current_rec]^.sideAA = false)) then
                                    {then switch}
                                 switch_maoz_order (data^[current_rec])
                              else     {they want revisionist, rev in spot 1, B not rev.}
                                 if ((not(user_selections.dispute_info.SideAIsInitiator)) and
                                     (data^[current_rec]^.revisionistA = true) and
                                     (data^[current_rec]^.revisionistB = false))  then
                                     {then OK}
                                 begin
                                      {do nothing, no switch needed}
                                 end
                              else    {they want rev, 2 revs, and Side A not in 1st spot}
                                 if ((not(user_selections.dispute_info.SideAIsInitiator)) and
                                     (data^[current_rec]^.revisionistA = true) and
                                     (data^[current_rec]^.revisionistB = true) and
                                     (data^[current_rec]^.sideAA = false)) then
                                     {then switch}
                                 switch_maoz_order (data^[current_rec])
                              else    {they want rev, 0 revs, and Side A not in 1st spot}
                                 if ((not(user_selections.dispute_info.SideAIsInitiator)) and
                                     (data^[current_rec]^.revisionistA = false) and
                                     (data^[current_rec]^.revisionistB = false) and
                                     (data^[current_rec]^.sideAA = false)) then
                                     {then switch}
                                 switch_maoz_order (data^[current_rec])
                              else  {dyad record is OK}
                                 begin
                                      {do nothing, no switch needed}
                                 end;
                        end;
                   {Now, either the initiator or revisionist is side A.  }

                   {*** Second, could go through and keep only the initiations the user wants, which means
                    dropping records if the user wants revisionists only and the record has no
                    revisionist states.  Not doing this for now.}

                  {Sort the list of dyadic-dispute years by the countries involved and by start date.}
                  trace.message ('Finished constructing Maoz dyadic disputes.  Now Sorting by country 1.');
                  trace.message ('       '+inttostr(num_dyadic_dispute_years)+ ' dyadic dispute years of all types kept.');
                  {writeln ('Before sorting, dyadic dispute list is');}
                  {list_dyadic_disputes (dyadic_disputes, num_dyadic_disputes, country_disputes); }

                  {First sort by first country in dyad, state 0}
                  quicksort_dyadic_disputes_by_country (0, get_last_dispnum, 0);

                  {writeln ('After sorting by country 1, dyadic dispute list is '); }
                  { list_dyadic_disputes (dyadic_disputes, num_dyadic_disputes, country_disputes);}

                  {That does overall list by first of dyad.  Now do by 2nd country in dyad, within each first}
                  trace.message ('Starting sort by country 2...');
                  x := 0;
                  y := 0;
                  repeat
                     done := false;
                     repeat
                        y := y + 1;
                        if (y >= get_last_dispnum) then done:= true
                        else if (data^[x]^.ccodeA) <> (data^[y]^.ccodeA) then done:=true;
                     until done;
                     {now sorting by second country, side 1}
                     if y = get_last_dispnum then quicksort_dyadic_disputes_by_country (x, y, 1) else
                        quicksort_dyadic_disputes_by_country (x, y-1, 1);
                     x := y;
                  until y > get_last_dispnum;
                  {writeln ('After sorting by country 2, dyadic dispute list is ');}
                  {list_dyadic_disputes (dyadic_disputes, num_dyadic_disputes, country_disputes);}

                  {That did sort by first and second in dyad.  Now do by year within each dyad}
                  {Note that this will sort by year, not first year of dispute.  this means if there
                   are multiple dyadic disputes between 2 ccodes in a year, all the 1941 disps will be
                   together, then the 1942s, etc.  }
                  trace.message ('Doing quicksort by year...');
                  x := 0;
                  y := 0;
                  repeat
                     done := false;
                     repeat
                        y := y + 1;
                        if (y >= get_last_dispnum) then done:= true
                        else if ((data^[x]^.ccodeA) <> (data^[y]^.ccodeA)) or
                                ((data^[x]^.ccodeB) <> (data^[y]^.ccodeB)) then done:=true;
                     until done;
                     if y = get_last_dispnum then quicksort_dyadic_disputes_by_year (x, y) else
                        quicksort_dyadic_disputes_by_year (x, y-1);
                     x := y;
                  until y > get_last_dispnum;

                  {Don'need any further sorting for now.}
                  trace.message ('Finished sorting dyadic disputes.');

                  {Now set the index marker to the 1st dispute for each ccode pair.  }
                  {do this by running through the sorted dyads, and marking the index to the correct spot
                   when the spot has not yet been set.}
                  for x := 0 to get_last_dispnum do
                     if index^[data^[x]^.ccodeA, data^[x]^.ccodeB] = null_dispute_number then
                        index^[data^[x]^.ccodeA, data^[x]^.ccodeB] := x;

                  Dyadic_dispute_trace.tickdone;

                  stored_peaceyrs.ccode1 := min_ccode;
                  stored_peaceyrs.ccode2 := min_ccode;
                  stored_peaceyrs.year := min_year;
                  stored_peaceyrs.numyears := missing_value;
               except  {There's been an error, probably a file error}
                  EUGeneError ('Unable to successfully open and read Maoz dyadic MID file.  Please check that files exist and are not open in another program and try again.',3,continue, error_log);
               end;

           finally
               Dyadic_dispute_trace.free;
               close (infile);
               if debug[4] then
                  trace.message (' required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory for maoz dyadic data');
               trace.exit('Finished initializing, reading, and sorting Maoz Dyadic dispute data');
           end;
        end;  {proc init}

            {   -----------------------  }

     destructor TMaoz_Dyadic_dispute_data_obj.destroy;
       var x : integer;
       begin
         try
            for x := 0 to max_dyadic_dispute_years-1 do
               if data^[x] <> nil then dispose (data^[x]);
            if data <> nil then dispose(data);
            data := nil;
            created := false;
            inherited destroy;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
       end;

     function TMaoz_Dyadic_dispute_data_obj.get_last_dispnum : longint;
      begin
         check_initialized;
         get_last_dispnum := num_dyadic_dispute_years - 1;
      end;

     function TMaoz_Dyadic_dispute_data_obj.get_ccode (dyadic_dispute_num : longint; which_state : side_type) : ccode_range;
                   {given number of a dyadic year dispute record, returns
                    the ccode of the specified particpant in the dyadic dispute}
        var temp : ccode_range;
        begin
            check_initialized;
            if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
            if which_state = 0 then
               temp := data^[dyadic_dispute_num]^.ccodeA
            else if which_state=1 then
               temp := data^[dyadic_dispute_num]^.ccodeB;
            get_ccode := temp;
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_year (dyadic_dispute_num : longint) : year_range;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := min_year else
               result := (data^[dyadic_dispute_num]^.Year);
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_MID_num (dyadic_dispute_num : longint) : dispute_id_range;
      begin
         if dyadic_dispute_num = null_dispute_number then {no dispute} get_MID_num := 0 else
            get_MID_num := data^[dyadic_dispute_num]^.idnum;
      end;

     function TMaoz_Dyadic_dispute_data_obj.get_hostlev_state (dyadic_dispute_num : longint; ccode: ccode_range) : hostlevtype;
        {either record1 or record2 are ccode}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (data^[dyadic_dispute_num]^.hostlevA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (data^[dyadic_dispute_num]^.hostlevB)
           else {ccode not found}
              showmessage ('Programming error in Maoz dyadic.get_originator - ccode not in record.  Notify programmer.');
           {Also, want to change coding as a "-9" hostility to a 1, which means
            no militarized response, although you were a target.}
           if (result = -9) then result := 1;
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_sideA (dyadic_dispute_num : longint; ccode: ccode_range) : boolean;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := false else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (data^[dyadic_dispute_num]^.SideAA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (data^[dyadic_dispute_num]^.SideAB)
           else {ccode not found}
              showmessage ('Programming error in Maoz dyadic.get_sideAA - ccode not in record.  Notify programmer.');
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_revisionist (dyadic_dispute_num : longint;
              ccode: ccode_range) : boolean;
        {either record1 or record2 are ccode;  true if was revisionist in MID coding.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := false else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (data^[dyadic_dispute_num]^.RevisionistA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (data^[dyadic_dispute_num]^.RevisionistB)
           else {ccode not found}
              showmessage ('Programming error in Maoz dyadic.get_revisionist - ccode not in record.  Notify programmer.');
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_originator (dyadic_dispute_num : longint;
              ccode: ccode_range) : boolean;
        {either record1 or record2 are ccode;  true if was originator in MID coding.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := false else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (data^[dyadic_dispute_num]^.OriginatorA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (data^[dyadic_dispute_num]^.OriginatorB)
           else {ccode not found}
              showmessage ('Programming error in Maoz dyadic.get_originator - ccode not in record.  Notify programmer.');
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_styear (dyadic_dispute_num : longint; ccode: ccode_range) : year_range;
        {either record1 or record2 are ccode;  returns MID coding start year.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := min_year else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (data^[dyadic_dispute_num]^.StYearA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (data^[dyadic_dispute_num]^.StYearB)
           else {ccode not found}
              showmessage ('Programming error in Maoz dyadic.get_styear - ccode not in record.  Notify programmer.');
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_stmonth (dyadic_dispute_num : longint; ccode: ccode_range) : month_range;
        {either record1 or record2 are ccode;  returns MID coding start month.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (data^[dyadic_dispute_num]^.stMonthA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (data^[dyadic_dispute_num]^.stMonthB)
           else {ccode not found}
              showmessage ('Programming error in Maoz dyadic.get_stmonth - ccode not in record.  Notify programmer.');
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_stday (dyadic_dispute_num : longint; ccode: ccode_range) : MIDdaytype;
        {either record1 or record2 are ccode;  returns MID coding start day.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (data^[dyadic_dispute_num]^.StDayA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (data^[dyadic_dispute_num]^.StDayB)
           else {ccode not found}
              showmessage ('Programming error in Maoz dyadic.get_stday - ccode not in record.  Notify programmer.');
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_endyear (dyadic_dispute_num : longint; ccode: ccode_range) : year_range;
        {either record1 or record2 are ccode;  returns MID coding end year.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := min_year else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (data^[dyadic_dispute_num]^.EndYearA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (data^[dyadic_dispute_num]^.EndYearB)
           else {ccode not found}
              showmessage ('Programming error in Maoz dyadic.get_endyear - ccode not in record.  Notify programmer.');
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_endmonth (dyadic_dispute_num : longint; ccode: ccode_range) : month_range;
        {either record1 or record2 are ccode;  returns MID coding end month.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (data^[dyadic_dispute_num]^.EndMonthA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (data^[dyadic_dispute_num]^.EndMonthB)
           else {ccode not found}
              showmessage ('Programming error in Maoz dyadic.get_endmonth - ccode not in record.  Notify programmer.');
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_endday (dyadic_dispute_num : longint; ccode: ccode_range) : MIDdaytype;
        {either record1 or record2 are ccode;  returns MID coding end day.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (data^[dyadic_dispute_num]^.EndDayA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (data^[dyadic_dispute_num]^.EndDayB)
           else {ccode not found}
              showmessage ('Programming error in Maoz dyadic.get_endday - ccode not in record.  Notify programmer.');
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_revtype (dyadic_dispute_num : longint; ccode: ccode_range) : revisiontype;
        {either record1 or record2 are ccode;  returns MID coding of revision type sought.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (data^[dyadic_dispute_num]^.RevTypeA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (data^[dyadic_dispute_num]^.RevTypeB)
           else {ccode not found}
              showmessage ('Programming error in Maoz dyadic.get_revtype - ccode not in record.  Notify programmer.');
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_fatality_state (dyadic_dispute_num : longint; ccode: ccode_range) : fatalitytype;
        {either record1 or record2 are ccode;  returns MID fatality coding.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (data^[dyadic_dispute_num]^.FatalityA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (data^[dyadic_dispute_num]^.FatalityB)
           else {ccode not found}
              showmessage ('Programming error in Maoz dyadic.get_fatality - ccode not in record.  Notify programmer.');
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_HiAct_state (dyadic_dispute_num : longint; ccode: ccode_range) : Actiontype;
        {either record1 or record2 are ccode;  returns highest action in MID coding.}
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (data^[dyadic_dispute_num]^.HiActA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (data^[dyadic_dispute_num]^.HiActB)
           else {ccode not found}
              showmessage ('Programming error in Maoz dyadic.get_HiAct - ccode not in record.  Notify programmer.');
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_Role (dyadic_dispute_num : longint; ccode: ccode_range) : Roletype;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           if (get_ccode(dyadic_dispute_num, 0) = ccode) then
              result := (data^[dyadic_dispute_num]^.RoleA)
           else
           if (get_ccode(dyadic_dispute_num, 1) = ccode) then
              result := (data^[dyadic_dispute_num]^.RoleB)
           else {ccode not found}
              showmessage ('Programming error in Maoz dyadic.get_Role - ccode not in record.  Notify programmer.');
        end;

     {get first year of dyadic dispute}
     function TMaoz_Dyadic_dispute_data_obj.get_MID_styear (dyadic_dispute_num : longint) : year_range;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := min_year else
              result := (data^[dyadic_dispute_num]^.StYear);
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_MID_stmonth (dyadic_dispute_num : longint) : month_range;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
              result := (data^[dyadic_dispute_num]^.StMonth);
        end;
     function TMaoz_Dyadic_dispute_data_obj.get_MID_stday (dyadic_dispute_num : longint) : MIDdaytype;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
              result := (data^[dyadic_dispute_num]^.StDay);
        end;
     function TMaoz_Dyadic_dispute_data_obj.get_MID_endyear (dyadic_dispute_num : longint) : year_range;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := min_year else
              result := (data^[dyadic_dispute_num]^.EndYear);
        end;
     function TMaoz_Dyadic_dispute_data_obj.get_MID_endmonth (dyadic_dispute_num : longint) : month_range;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
              result := (data^[dyadic_dispute_num]^.EndMonth);
        end;
     function TMaoz_Dyadic_dispute_data_obj.get_MID_endday (dyadic_dispute_num : longint) : MIDdaytype;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
              result := (data^[dyadic_dispute_num]^.EndDay);
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_MID_numstates (dyadic_dispute_num : longint; ccode: ccode_range) : integer;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           if (get_sideA(dyadic_dispute_num, ccode) = true) then
              result := (data^[dyadic_dispute_num]^.numberA)
           else 
              result := (data^[dyadic_dispute_num]^.NumberB);
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_MID_outcome (dyadic_dispute_num : longint) : outcometype;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           result := data^[dyadic_dispute_num]^.outcome;
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_MID_settlement (dyadic_dispute_num : longint) : settlementtype;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           result := data^[dyadic_dispute_num]^.settlement;
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_MID_fatality (dyadic_dispute_num : longint) : fatalitytype;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           result := data^[dyadic_dispute_num]^.Fatality;
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_MID_hiact (dyadic_dispute_num : longint) : actiontype;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           result := data^[dyadic_dispute_num]^.HiAct;
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_MID_hostlev (dyadic_dispute_num : longint) : hostlevtype;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           result := data^[dyadic_dispute_num]^.HostLev;
        end;

     {Note this is the unadjusted mid 2.1 coding of reciprocated}
     function TMaoz_Dyadic_dispute_data_obj.get_MID_reciprocated (dyadic_dispute_num : longint) : boolean;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := false else
           result := data^[dyadic_dispute_num]^.ReciprocatedMid;
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_COWWar (dyadic_dispute_num : longint) : boolean;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := false else
           result := data^[dyadic_dispute_num]^.COWWar;
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_DurIndx (dyadic_dispute_num : longint) : integer;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           result := data^[dyadic_dispute_num]^.DurIndx;
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_DurDays (dyadic_dispute_num : longint) : integer;
        begin
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := 0 else
           result := data^[dyadic_dispute_num]^.DurDays;
        end;

     {Variable no longer available, v1.1 Dyad MID
     function TMaoz_Dyadic_dispute_data_obj.get_ReciprocatedDyadic (dyadic_dispute_num : longint) : boolean;
        begin
           if dyadic_dispute_num = 0 then {no dispute}
     {         result := false else
           result := data^[dyadic_dispute_num]^.ReciprocatedDyadic;
        end;
     }

     {Maoz no longer includes a variable marking NewMID, but need the call in multiple
      later procedures to determine if a MID is new, so have modified the function
      to compute whether it's a new MID.}
     function TMaoz_Dyadic_dispute_data_obj.get_NewMID (dyadic_dispute_num : longint) : boolean;
        begin
           result := false;
           if dyadic_dispute_num = null_dispute_number then {no dispute} result := false else
           {result := data^[dyadic_dispute_num]^.NewMID;}
           if self.get_MID_styear(dyadic_dispute_num) = self.get_year(dyadic_dispute_num) then result := true;
        end;


    {Now various functions that look at multiple conditional inputs}

     function TMaoz_Dyadic_dispute_data_obj.is_ongoing (ccode1, ccode2 : ccode_range; year : year_range;
                 var ongoing_num : longint) : boolean;
        {returns true if there is a dispute ongoing between cc1, cc2;
         even if 2 vs. 1.  SO, this is nondirected.  Rtns # of ongoing disp in ongoing_num.
         NOTE:  This relies on the MAOZ coding being correct, which it appears to be in DYADMID v1.0.}
        var disputenum : longint;
        begin
           check_initialized;
           is_ongoing := false;
           ongoing_num := null_dispute_number;
           disputenum := index^[ccode1, ccode2];       {1st disp-year between cc1, cc2}
           {For Maoz data, the disputes are sorted by  ccode1 ccode2 YEAR, not cc1 cc2 start year.
            So, I can find the first for cc1 and cc2, then look for year before I care about, then see
            if same MID number is continuing in the year I care about.  Also, Maoz has the "newmid" field
            that I can use to see if a mid in a year is new or ongoing.}
           if disputenum <> null_dispute_number then
              repeat
                {need to check all disputes starting with the 1st until have reached the
                 next year, or find one that is ongoing.}
                 if (get_year (disputenum) = year) and (get_newmid (disputenum) = false) then
                    begin
                       is_ongoing := true;
                       ongoing_num := disputenum;
                    end;
                 inc(disputenum);
              until ((result=true) or
                     (disputenum > get_last_dispnum) or
                     (get_year(disputenum) > year) or
                     (get_ccode(disputenum, 0) <> ccode1) or
                     (get_ccode(disputenum, 1) <> ccode2));
                 {At this point, have examined all records of cc1 vs cc2, and either found that there is
                  a dispute starting in year, or that I went through all the records and there was
                  not anytime a dispute started in year.  }

               {now, if not yet seen, check the reverse dyad to see if 1st year of 2 vs. 1}
           if ongoing_num = null_dispute_number then
           begin
              disputenum := index^[ccode2, ccode1];
              if disputenum <> null_dispute_number then
                 repeat
                    if (get_year (disputenum) = year) and (get_newmid (disputenum) = false) then
                       begin
                          is_ongoing := true;
                          ongoing_num := disputenum;
                       end;
                    inc(disputenum);
                 until ((result=true) or
                        (disputenum > get_last_dispnum) or
                        (get_year(disputenum) > year) or
                        (get_ccode(disputenum, 0) <> ccode2) or
                        (get_ccode(disputenum, 1) <> ccode1));
           end;

        end;

     function TMaoz_Dyadic_dispute_data_obj.Is_1stYearOfDispute (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_disp_num : longint) : boolean;
        {returns true if this is the 1st year of a dispute between cc1, cc2;
         even if 2 vs. 1.  So, this is nondirected.
         NOTE:  This relies on the MAOZ coding being correct, which it appears to be in DYADMID v1.0.}
        var disputenum : longint;
            none_found : boolean;
        begin
           check_initialized;
           Is_1stYearOfDispute := false;
           found_disp_num := null_dispute_number;
           disputenum := index^[ccode1, ccode2];
           if disputenum <> null_dispute_number then
              repeat
                {need to check all disputes starting with the 1st until another dyad is seen,
                  or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                  I've identified if any of them start in this year.}
                 if (get_year (disputenum) = year) and (get_newmid(disputenum)=true) then
                    begin
                       Is_1stYearOfDispute := true;
                       found_disp_num := disputenum;
                    end;
                 inc(disputenum);
              until ((result=true) or
                     (disputenum > get_last_dispnum) or
                     (get_year(disputenum) > year) or
                     (get_ccode(disputenum, 0) <> ccode1) or
                     (get_ccode(disputenum, 1) <> ccode2));
                 {At this point, have examined all records of cc1 vs cc2, and either found that there is
                  a dispute starting in year, or that I went through all the records and there was
                  not anytime a dispute started in year.  }

               {now, if not yet seen, check the reverse dyad to see if 1st year of 2 vs. 1}
           if found_disp_num = null_dispute_number then
           begin
              disputenum := index^[ccode2, ccode1];
              if disputenum <> null_dispute_number then
                 repeat
                 if (get_year (disputenum) = year) and (get_newmid(disputenum)=true) then
                       begin
                          Is_1stYearOfDispute := true;
                          found_disp_num := disputenum;
                       end;
                    inc(disputenum);
                 until ((result=true) or
                        (disputenum > get_last_dispnum) or
                        (get_year(disputenum) > year) or
                        (get_ccode(disputenum, 0) <> ccode2) or
                        (get_ccode(disputenum, 1) <> ccode1));
           end;
        end;

     function TMaoz_Dyadic_dispute_data_obj.Is_sideA (ccode1, ccode2 : ccode_range; year : year_range) : boolean;
        {returns true if ccode1 is on side A of a dispute against ccode2 this year, even if ongoing.}
        var disputenum : longint;
        begin
           check_initialized;
           result := false;
           disputenum := index^[ccode1, ccode2];
           if disputenum <> null_dispute_number then
              repeat
                {need to check all disputes starting with the 1st until another dyad is seen,
                  or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                  I've identified if cc1 is on side A in any of them in this year.}
                 if (get_sideA (disputenum, ccode1) = true) and
                    (get_year(disputenum) = year) then
                     result := true;
                 inc(disputenum);
              until ((result=true) or
                     (disputenum > get_last_dispnum) or
                     (get_year(disputenum) > year) or
                     (get_ccode(disputenum, 0) <> ccode1) or
                     (get_ccode(disputenum, 1) <> ccode2));

                 {Also need to check disputes of cc2 vs. cc1}
                 {This should be unnecessary with Maoz data, because it should be sorted
                  so that side A is always first.  But leave it in to double check.}
           if result <> true then
           begin
              disputenum := index^[ccode2, ccode1];
              if disputenum <> null_dispute_number then
                 repeat
                 if (get_sideA (disputenum, ccode1) = true) and
                    (get_year(disputenum) = year) then
                          result := true;
                    inc(disputenum);
                 until ((result=true) or
                        (disputenum > get_last_dispnum) or
                        (get_year(disputenum) > year) or
                        (get_ccode(disputenum, 0) <> ccode2) or
                        (get_ccode(disputenum, 1) <> ccode1));
           end;
        end;

     function TMaoz_Dyadic_dispute_data_obj.Is_originator (ccode1, ccode2 : ccode_range; year : year_range) : boolean;
        {returns true if ccode1 is originator in a dispute against ccode2 this year.
         This does not say this is the origination year, but 1 was an originator in a
         disp that is either new or ongoing in this year.  This is non-directed.}
        var disputenum : longint;
        begin
           check_initialized;
           result := false;
           disputenum := index^[ccode1, ccode2];
           if disputenum <> null_dispute_number then
              repeat
                 if (get_originator (disputenum, ccode1)) and
                    (get_year(disputenum) = year) then
                     result := true;
                 inc(disputenum);
              until ((result=true) or
                     (disputenum > get_last_dispnum) or
                     (get_year(disputenum) > year) or
                     (get_ccode(disputenum, 0) <> ccode1) or
                     (get_ccode(disputenum, 1) <> ccode2));

              {also check the reverse dyad, if I havent' id'd the originator already.}
           if result = false then
              begin
                 disputenum := index^[ccode2, ccode1];
                 if disputenum <> null_dispute_number then
                    repeat
                       if (get_originator (disputenum, ccode1)) and
                          (get_year(disputenum) = year) then
                           result := true;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode2) or
                           (get_ccode(disputenum, 1) <> ccode1));
              end;
        end;

     function TMaoz_Dyadic_dispute_data_obj.Is_1stYear_Initiation (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 and 2 are originators,
         and cc1 is on side A in a dispute against ccode2 this year,
         and this is the first year of the dispute.  }
        {This marks what we consider a new true initiation by cc1 vs. cc2}
        {This works if called either with a disp# or a ccode-year}
        var disputenum : longint;
        begin
           check_initialized;
           result := false;
           if use_param = use_dispute then
              begin
                 if ((get_year (found_dispute_num) = year) and
                     (get_originator (found_dispute_num, ccode1)) and
                     (get_originator (found_dispute_num, ccode2)) and
                     (get_sideA (found_dispute_num, ccode1)) and
                     (get_newmid (found_dispute_num))) then
                     result := true;
              end
           else     {use ccode-year}
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are true initiations.}
                       {This will stop and identify/return the dispute number of
                         the first initiation between A and B in this year}
                       if ((get_year (disputenum) = year) and
                           (get_originator (disputenum, ccode1)) and
                           (get_originator (disputenum, ccode2)) and
                           (get_sideA (disputenum, ccode1)) and
                           (get_newmid (disputenum))) then
                          begin
                             result := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
                       {At this point, have examined all records of cc1 vs cc2, and either found that there is
                        a dispute starting in year, or that I went through all the records and there was
                        not anytime a dispute started in year.  }
              end;
           {double check maoz role variable}
           {The first version is what it should be once Maoz data is fixed.  The 2nd version
            ignores three cases with inconsistencies.}
           {if result=true then if not (get_role(found_dispute_num, ccode1) = 1) then
              showmessage ('Programming error - Maoz role doesnt match in procedure "is 1st year initiation" for '+inttostr(ccode1)+' '+inttostr(ccode2)+' '+inttostr(year));}
           if result=true then if not ((get_role(found_dispute_num, ccode1) = 1) or (get_mid_num(found_dispute_num)=1039)or (get_mid_num(found_dispute_num)=3994)or (get_mid_num(found_dispute_num)=3995)) then
              showmessage ('Programming error - Maoz role doesnt match in procedure "is 1st year initiation" for '+inttostr(ccode1)+' '+inttostr(ccode2)+' '+inttostr(year));
        end;

     function TMaoz_Dyadic_dispute_data_obj.Is_1stYear_Joined_Initiation (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is on initiating side vs. cc2, and this is the 1st year that
         cc1 joined the dispute, BUT either cc1 or cc2 is not an originator.}
        var disputenum : longint;
        begin
           check_initialized;
           result := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_year (found_dispute_num) = year) and
                     (get_newmid (found_dispute_num)) {marks 1st year of MID} and
                     (not (get_originator (found_dispute_num, ccode1) and
                           get_originator (found_dispute_num, ccode2) ) ) and
                     (get_sideA (found_dispute_num, ccode1)) ) then
                       result := true;
                 {double check maoz role variable}
                 if ( (result=true) and (not(get_mid_num(found_dispute_num)=258))) then
                    if not ( (get_role(found_dispute_num, ccode1) = 2) or (get_role(found_dispute_num, ccode1) = 1) )then
                    showmessage ('Programming error - Maoz role doesnt match in procedure "is 1st year joined initiation" for '+inttostr(ccode1)+' '+inttostr(ccode2)+' '+inttostr(year));
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                       if ((get_year (disputenum) = year) and
                           (get_newmid (found_dispute_num)) and
                           (not (get_originator (disputenum, ccode1) and
                                 get_originator (disputenum, ccode2) ) ) and
                           (get_sideA (disputenum, ccode1)) ) then
                          begin
                             result := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
                    {double check maoz role variable}
                    if ((result=true) and (not(get_mid_num(found_dispute_num)=258))) then
                       if not (get_role(found_dispute_num, ccode1) = 2) then
                       showmessage ('Programming error - Maoz role doesnt match in procedure "is 1st year joined initiation" for '+inttostr(ccode1)+' '+inttostr(ccode2)+' '+inttostr(year));
              end;
        end;

     function TMaoz_Dyadic_dispute_data_obj.Is_1stYear_Joined_Targets (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is on target side vs. cc2, and this is the 1st year that
         cc1 joined the dispute, BUT either cc1 or cc2 is not an originator.
         Need to look at directed dyads of 1 vs. 2.

         To do this by the ccode-year method, will need to access the
         ccode2 vs. ccode1 dyad, because only one direction
         of most dyads was created.  To see if someone is a target, will need to look
         at the other country first, since we want to check dyads where it is init side.
         So, to see if cc1 is a target, will need to look at cc2 vs. cc1 dyads.}
        var disputenum : longint;
        begin
           check_initialized;
           result := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_year (found_dispute_num) = year) and
                     (get_newmid (found_dispute_num) = true) and
                     (not (get_originator (found_dispute_num, ccode1) and
                           get_originator (found_dispute_num, ccode2) ) ) and
                     (get_sideA (found_dispute_num, ccode1) = false) ) then
                       result := true;
                 {MID 258 is hard to check b/c italy switches around.}
                 if ((result=true) and (not(get_mid_num(found_dispute_num)=258))) then
                    if not ( (get_role(found_dispute_num, ccode1) = 4) or (get_role(found_dispute_num, ccode1) = 3)) then
                    showmessage ('Programming error - Maoz role doesnt match in procedure "is 1st year joined targets" for '+inttostr(ccode1)+' '+inttostr(ccode2)+' '+inttostr(year));
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode2, ccode1];
                 if disputenum <> null_dispute_number then
                    repeat
                       if ((get_year (disputenum) = year) and
                           (get_newmid (found_dispute_num) = true) and
                           (not (get_originator (disputenum, ccode1) and
                                 get_originator (disputenum, ccode2) ) ) and
                           (get_sideA (disputenum, ccode1) = false) ) then
                          begin
                             result := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                    {need to reverse ccode2, ccode1 coding here b/c searching in
                     reversed dyad, b/c looking at targets}
                           (get_ccode(disputenum, 0) <> ccode2) or
                           (get_ccode(disputenum, 1) <> ccode1));
                    if ((result=true) and not((get_mid_num(found_dispute_num)=258))) then
                       if not (get_role(found_dispute_num, ccode2) = 4) then
                       showmessage ('Programming error - Maoz role doesnt match in procedure "is 1st year joined targets" for '+inttostr(ccode1)+' '+inttostr(ccode2)+' '+inttostr(year));
              end;
           {double check maoz role variable.
            Until 6/20/03, had this checking ccode2.  That doesn't make sense - should be ccode1.}
        end;

     function TMaoz_Dyadic_dispute_data_obj.Is_AnyYear_Initiation (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 and 2 are originators, cc1 initiated vs. cc2,
         BUT this may not be the first year of the dispute.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_AnyYear_Initiation := false;
           if use_param = use_dispute then
              begin
                 if ((get_year (found_dispute_num) = year) and
                     (get_originator (found_dispute_num, ccode1)) and
                     (get_originator (found_dispute_num, ccode2)) and
                     (get_sideA (found_dispute_num, ccode1)) ) then
                 Is_AnyYear_Initiation := true;
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                       if ((get_year (disputenum) = year) and
                           (get_originator (disputenum, ccode1)) and
                           (get_originator (disputenum, ccode2)) and
                           (get_sideA (disputenum, ccode1)) ) then
                          begin
                             Is_AnyYear_Initiation := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
                       {At this point, have examined all records of cc1 vs cc2, and either found that there is
                        a dispute in year where 1 was initiator vs. 2,
                        or that I went through all the records and there was not a dispute in year.}
              end;
        end;

     function TMaoz_Dyadic_dispute_data_obj.Is_AnyYear_Joined_Initiation (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is on initiating side vs. cc2, and this is ANY year of
         the dispute, BUT either cc1 or cc2 is not an originator.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_AnyYear_Joined_Initiation := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_year (found_dispute_num) = year) and
                     (not (get_originator (found_dispute_num, ccode1) and
                           get_originator (found_dispute_num, ccode2)    )) and
                     (get_sideA (found_dispute_num, ccode1)) ) then
                   Is_AnyYear_Joined_Initiation := true;
                end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                       if ((get_year (disputenum) = year) and
                           (not (get_originator (disputenum, ccode1) and
                                 get_originator (disputenum, ccode2)   )) and
                           (get_sideA (disputenum, ccode1) = true) ) then
                          begin
                             Is_AnyYear_Joined_Initiation := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
              end;
        end;

     function TMaoz_Dyadic_dispute_data_obj.Is_AnyYear_Joined_Targets (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is on target side vs. cc2, and this is ANY year of
         the dispute, BUT either cc1 or cc2 is not an originator.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_AnyYear_Joined_Targets := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_year (found_dispute_num) = year) and
                     (not (get_originator (found_dispute_num, ccode1) and
                           get_originator (found_dispute_num, ccode2)   )) and
                     (get_sideA (found_dispute_num, ccode1) = false) ) then
                   Is_AnyYear_Joined_Targets := true;
                end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 {start index at first of ccode2 vs. ccode1 dyads/disputes because
                  looking for joining the target}
                 disputenum := index^[ccode2, ccode1];
                 if disputenum <> null_dispute_number then
                    repeat
                       if ((get_year (disputenum) = year) and
                           (not (get_originator (disputenum, ccode1) and
                                 get_originator (found_dispute_num, ccode2)   )) and
                           (get_sideA (disputenum, ccode1) = false) ) then
                          begin
                             Is_AnyYear_Joined_Targets := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                    {need to reverse ccode2, ccode1 coding here b/c searching in
                     reversed dyad, b/c looking at targets}
                           (get_ccode(disputenum, 0) <> ccode2) or
                           (get_ccode(disputenum, 1) <> ccode1));
              end;
        end;

    function TMaoz_Dyadic_dispute_data_obj.Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2 : ccode_range; year : year_range;
              var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 and 2 are originators, in a dispute that started this year.
         Don't care about who is side A or B.  }
        {This marks what we consider a new true, non-directed initiation between cc1 and cc2}
        {This works if called either with a disp# or a ccode-year.}
        var disputenum : longint;
        begin
           check_initialized;
           Is_1stYear_NonDir_Dispute_Originators := false;
           if use_param = use_dispute then
              begin
                 if ((get_year (found_dispute_num) = year) and
                     (get_newmid (found_dispute_num)) and
                     (get_originator (found_dispute_num, ccode1)) and
                     (get_originator (found_dispute_num, ccode2)) ) then
                     Is_1stYear_NonDir_Dispute_Originators := true;
              end
           else     {use ccode-year}
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are initiations.}
                       {This will stop and identify/return the dispute number of
                         the first dispute between A and B in this year}
                       if ((get_year (disputenum) = year) and
                           (get_newmid (disputenum)) and
                           (get_originator (disputenum, ccode1)) and
                           (get_originator (disputenum, ccode2))) then
                          begin
                             Is_1stYear_NonDir_Dispute_Originators := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
                       {At this point, have examined all records of cc1 vs cc2, and either found that there is
                        a dispute starting in year, or that I went through all the records and there was
                        not anytime a dispute started in year.  }
                       {check cc2 vs. cc1 order also}
                 if result = false then
                    begin
                       disputenum := index^[ccode2, ccode1];
                       if disputenum <> null_dispute_number then
                          repeat
                             if ((get_year (disputenum) = year) and
                                 (get_newmid (disputenum)) and
                                 (get_originator (disputenum, ccode1)) and
                                 (get_originator (disputenum, ccode2))) then
                                begin
                                   Is_1stYear_NonDir_Dispute_Originators := true;
                                   found_dispute_num := disputenum;
                                end;
                             inc(disputenum);
                          until ((result=true) or
                                 (disputenum > get_last_dispnum) or
                                 (get_year(disputenum) > year) or
                                 (get_ccode(disputenum, 0) <> ccode2) or
                                 (get_ccode(disputenum, 1) <> ccode1));
                    end;
              end;
        end;

    function TMaoz_Dyadic_dispute_data_obj.Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2 : ccode_range; year : year_range;
              var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 and cc2 are in a dispute, but one is not an originator,
         and this is the 1st year of the dispute.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_1stYear_NonDir_Dispute_Joiners := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_year (found_dispute_num) = year) and
                     (get_newmid (found_dispute_num)) and
                     (not (get_originator (found_dispute_num, ccode1) and
                           get_originator (found_dispute_num, ccode2)))  ) then
                       Is_1stYear_NonDir_Dispute_Joiners := true;
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                       if ((get_year (disputenum) = year) and
                           (get_newmid (disputenum)) and
                           (not (get_originator (disputenum, ccode1) and
                                 get_originator (disputenum, ccode2))))  then
                          begin
                             Is_1stYear_NonDir_Dispute_Joiners := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
                 if result = false then
                    begin
                       disputenum := index^[ccode2, ccode1];
                       if disputenum <> null_dispute_number then
                          repeat
                             if ((get_year (disputenum) = year) and
                                 (get_newmid (disputenum)) and
                                 (not (get_originator (disputenum, ccode1) and
                                       get_originator (disputenum, ccode2))))  then
                                begin
                                   Is_1stYear_NonDir_Dispute_Joiners := true;
                                   found_dispute_num := disputenum;
                                end;
                             inc(disputenum);
                          until ((result=true) or
                                 (disputenum > get_last_dispnum) or
                                 (get_year(disputenum) > year) or
                                 (get_ccode(disputenum, 0) <> ccode2) or
                                 (get_ccode(disputenum, 1) <> ccode1));
                    end;
              end;
        end;

    function TMaoz_Dyadic_dispute_data_obj.Is_AnyYear_NonDir_Dispute_Originators (ccode1, ccode2 : ccode_range; year : year_range;
              var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 and 2 are originators, in a dispute that might or might
         not have started this year.  Don't care about who is side A or B.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_AnyYear_NonDir_Dispute_Originators := false;
           if use_param = use_dispute then
              begin
                 if ((get_year (found_dispute_num) = year)  and
                     (get_originator (found_dispute_num, ccode1)) and
                     (get_originator (found_dispute_num, ccode2)) ) then
                     Is_AnyYear_NonDir_Dispute_Originators := true;
              end
           else     {use ccode-year}
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are initiations.}
                       {This will stop and identify/return the dispute number of
                         the first dispute between A and B in this year}
                       if ((get_year (disputenum) = year) and
                           (get_originator (disputenum, ccode1)) and
                           (get_originator (disputenum, ccode2))) then
                          begin
                             Is_AnyYear_NonDir_Dispute_Originators := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
                       {At this point, have examined all records of cc1 vs cc2, and either found that there is
                        a dispute starting in year, or that I went through all the records and there was
                        not anytime a dispute started in year.  }
                 if result = false then
                    begin
                       disputenum := index^[ccode2, ccode1];
                       if disputenum <> null_dispute_number then
                          repeat
                             if ((get_year (disputenum) = year) and
                                 (get_originator (disputenum, ccode1)) and
                                 (get_originator (disputenum, ccode2))) then
                                begin
                                   Is_AnyYear_NonDir_Dispute_Originators := true;
                                   found_dispute_num := disputenum;
                                end;
                             inc(disputenum);
                          until ((result=true) or
                                 (disputenum > get_last_dispnum) or
                                 (get_year(disputenum) > year) or
                                 (get_ccode(disputenum, 0) <> ccode2) or
                                 (get_ccode(disputenum, 1) <> ccode1));
                    end;
              end;
        end;

    function TMaoz_Dyadic_dispute_data_obj.Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2 : ccode_range; year : year_range;
              var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 and cc2 in dispute, and this is ANY year of
         the dispute, BUT at least one is not an originator.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_AnyYear_NonDir_Dispute_Joiners := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_year (found_dispute_num) = year) and
                     (not (get_originator (found_dispute_num, ccode1) and
                           get_originator (found_dispute_num, ccode2)))) then
                   Is_AnyYear_NonDir_Dispute_Joiners := true;
                end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                       if ((get_year (disputenum) = year) and
                           (not (get_originator (disputenum, ccode1) and
                                 get_originator (disputenum, ccode2)))) then
                          begin
                             Is_AnyYear_NonDir_Dispute_Joiners := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
                 if result=false then
                    begin
                       disputenum := index^[ccode2, ccode1];
                       if disputenum <> null_dispute_number then
                          repeat
                             if ((get_year (disputenum) = year) and
                                 (not (get_originator (disputenum, ccode1) and
                                       get_originator (disputenum, ccode2)))) then
                                begin
                                   Is_AnyYear_NonDir_Dispute_Joiners := true;
                                   found_dispute_num := disputenum;
                                end;
                             inc(disputenum);
                          until ((result=true) or
                                 (disputenum > get_last_dispnum) or
                                 (get_year(disputenum) > year) or
                                 (get_ccode(disputenum, 0) <> ccode2) or
                                 (get_ccode(disputenum, 1) <> ccode1));
                    end;
              end;
        end;

    function TMaoz_Dyadic_dispute_data_obj.Is_1stYear_Revision (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is coded revisionist in a dispute against ccode2 this year,
         and this is the first year of the dispute.  }
        {This marks what we consider a new true initiation, but using revisionist, not side A}
        var disputenum : longint;
        begin
           check_initialized;
           Is_1stYear_Revision := false;
           if use_param = use_dispute then
              begin
                  if ((get_year (found_dispute_num) = year) and
                      (get_newmid (found_dispute_num)) and
                      (get_originator (found_dispute_num, ccode1)) and
                      (get_originator (found_dispute_num, ccode2)) and
                      (get_revisionist (found_dispute_num, ccode1) = true))  then
                  Is_1stYear_Revision := true;
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are true initiations.}
                       if ((get_year (disputenum) = year) and
                           (get_newmid (disputenum)) and
                           (get_originator (disputenum, ccode1)) and
                           (get_originator (disputenum, ccode2)) and
                           (get_revisionist (disputenum, ccode1) = true))  then
                          begin
                             Is_1stYear_Revision := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
              end;
        end;

    function TMaoz_Dyadic_dispute_data_obj.Is_1stYear_Joined_Revision (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is coded revisionist in a dispute against ccode2 this year,
         and this is the first year of the dispute.  But, ccode1 or ccode2 is a joiner.}
        var disputenum : longint;
        begin
           check_initialized;
           Is_1stYear_Joined_Revision := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_year (found_dispute_num) = year) and
                     (get_newmid (found_dispute_num)) and
                     (not (get_originator (found_dispute_num, ccode1) and
                           get_originator (found_dispute_num, ccode2) ) ) and
                     (get_revisionist (found_dispute_num, ccode1) = true) ) then
                 Is_1stYear_Joined_Revision := true;
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are true initiations.}
                       if ((get_year (disputenum) = year) and
                           (get_newmid (disputenum)) and
                           (not (get_originator (found_dispute_num, ccode1) and
                                 get_originator (found_dispute_num, ccode2) ) ) and
                           (get_revisionist (disputenum, ccode1) = true) ) then
                          begin
                             Is_1stYear_Joined_Revision := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
              end;
        end;

    function TMaoz_Dyadic_dispute_data_obj.Is_1stYear_Joined_SQ (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is coded non-revisionist in a dispute against ccode2 this year,
         and this is the first year of the dispute.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_1stYear_Joined_SQ := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_year (found_dispute_num) = year) and
                     (get_newmid (found_dispute_num)) and
                     (not (get_originator (found_dispute_num, ccode1) and
                           get_originator (found_dispute_num, ccode2) ) ) and
                     (not (get_revisionist (found_dispute_num, ccode1) = true)) ) then
                 Is_1stYear_Joined_SQ := true;
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 {start index at first of ccode2 vs. ccode1 dyads/disputes because
                  looking for joining the target}
                 disputenum := index^[ccode2, ccode1];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are true initiations.}
                       if ((get_year (disputenum) = year) and
                           (get_newmid (disputenum)) and
                           (not (get_originator (found_dispute_num, ccode1) and
                                 get_originator (found_dispute_num, ccode2) )) and
                           (not (get_revisionist (disputenum, ccode1) = true)) ) then
                          begin
                             Is_1stYear_Joined_SQ := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                    {need to reverse ccode2, ccode1 coding here b/c searching in
                     reversed dyad, b/c looking at targets}
                           (get_ccode(disputenum, 0) <> ccode2) or
                           (get_ccode(disputenum, 1) <> ccode1));
              end;
        end;

    function TMaoz_Dyadic_dispute_data_obj.Is_AnyYear_Revision (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is coded revisionist in a dispute against ccode2 this year,
         But this is not necessarily the first year of the dispute.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_AnyYear_Revision := false;
           if use_param = use_dispute then
              begin
                 if ((get_year (found_dispute_num) = year) and
                     (get_originator (found_dispute_num, ccode1)) and
                     (get_originator (found_dispute_num, ccode2)) and
                     (get_revisionist (found_dispute_num, ccode1) = true))  then
                  Is_AnyYear_Revision := true;
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are true initiations.}
                       if ((get_year (disputenum) = year) and
                           (get_originator (disputenum, ccode1)) and
                           (get_originator (disputenum, ccode2)) and
                           (get_revisionist (disputenum, ccode1) = true))  then
                          begin
                             Is_AnyYear_Revision := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
              end;
        end;

    function TMaoz_Dyadic_dispute_data_obj.Is_AnyYear_Joined_Revision (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is coded revisionist in a dispute against ccode2 this year,
         and this is any year of the dispute.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_AnyYear_Joined_Revision := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_year (found_dispute_num) = year) and
                     (not(get_originator (found_dispute_num, ccode1) and
                          get_originator (found_dispute_num, ccode2))) and
                     (get_revisionist (found_dispute_num, ccode1) = true) ) then
                   Is_AnyYear_Joined_Revision := true;
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are true initiations.}
                       if ((get_year (disputenum) = year) and
                           (not(get_originator (found_dispute_num, ccode1) and
                                get_originator (found_dispute_num, ccode2))) and
                           (get_revisionist (disputenum, ccode1) = true) ) then
                          begin
                             Is_AnyYear_Joined_Revision := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
              end;
        end;

    function TMaoz_Dyadic_dispute_data_obj.Is_AnyYear_Joined_SQ (ccode1, ccode2 : ccode_range;
                           year : year_range; var found_dispute_num : longint;
                           const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 is coded non-revisionist in a dispute against ccode2 this year,
         and this is any year of the dispute.  }
        var disputenum : longint;
        begin
           check_initialized;
           Is_AnyYear_Joined_SQ := false;
           if use_param = use_dispute then
              begin
                 if ((not(found_dispute_num=null_dispute_number)) and
                     (get_year (found_dispute_num) = year) and
                     (not(get_originator (found_dispute_num, ccode1) and
                          get_originator (found_dispute_num, ccode2))) and
                     (not(get_revisionist (found_dispute_num, ccode1) = true)) ) then
                   Is_AnyYear_Joined_SQ := true;
              end
           else
              begin
                 found_dispute_num := null_dispute_number;
                 {start index at first of ccode2 vs. ccode1 dyads/disputes because
                  looking for joining the target}
                 disputenum := index^[ccode2, ccode1];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are true initiations.}
                       if ((get_year (disputenum) = year) and
                           (not(get_originator (found_dispute_num, ccode1) and
                                get_originator (found_dispute_num, ccode2))) and
                           (not(get_revisionist (disputenum, ccode1) = true)) ) then
                          begin
                             Is_AnyYear_Joined_SQ := true;
                             found_dispute_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                    {need to reverse ccode2, ccode1 coding here b/c searching in
                     reversed dyad, b/c looking at targets}
                           (get_ccode(disputenum, 0) <> ccode2) or
                           (get_ccode(disputenum, 1) <> ccode1));
              end;
        end;


    {Note:  most of the remaining functions are unchanged from the TCOW version.  The one
     that changed was in relevant hostlev, which does a manual check for a new event
     in a year.  The other procs all call the previous subprocs, which were fixed,
     but the names of which (and calls to which) are the same below.  }

     function TMaoz_Dyadic_dispute_data_obj.wanted_new_initiation (const ccode1, ccode2 : ccode_range;
                           const year : year_range; const user_selections : user_selection_type;
                           var dispnum : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if there is a new directed initiation of a type I want in this year,
         by ccode1 vs. ccode2, meaning checking sideA vs. Revisionist, and originator or joiner.
         Used by procedures to check printing a dyad year iff a new dispute, and to output
         when an initiation occurs.}
         begin
            check_initialized;
            if user_selections.dispute_info.OnlyTrueInitiators then
               begin   {originators only, not joiners}
                 if user_selections.dispute_info.SideAIsInitiator then  {want originators, sideA}
                     wanted_new_initiation := Is_1stYear_Initiation (ccode1, ccode2, year, dispnum, use_param)
                   else     {originators, Revisionist}
                     wanted_new_initiation := Is_1stYear_Revision (ccode1, ccode2, year, dispnum, use_param);
                 end
              else    {want originators or joiners on initiating side}
            if user_selections.dispute_info.JoinersOnInitiatingSideAsInitiators then
                begin
                   if user_selections.dispute_info.SideAIsInitiator then   {want originators or joiners, sideA}
                       wanted_new_initiation :=
                          (Is_1stYear_Initiation (ccode1, ccode2, year, dispnum, use_param)
                           or Is_1stYear_Joined_Initiation (ccode1, ccode2, year, dispnum, use_param))
                   else   {want originators or joiners on init side, Revisionist}
                       wanted_new_initiation :=
                              (Is_1stYear_Revision (ccode1, ccode2, year, dispnum, use_param)
                               or Is_1stYear_Joined_Revision (ccode1, ccode2, year, dispnum, use_param));
                 end
                else     {want originators or any kind of joiner marked}
            if user_selections.dispute_info.AllJoinersAsInitiators then
                begin
                   if user_selections.dispute_info.SideAIsInitiator then   {want originators or joiners, sideA}
                       wanted_new_initiation :=
                          (Is_1stYear_Initiation (ccode1, ccode2, year, dispnum, use_param)
                           or Is_1stYear_Joined_Initiation (ccode1, ccode2, year, dispnum, use_param)
                           or Is_1stYear_Joined_Targets (ccode1, ccode2, year, dispnum, use_param))
                   else   {want originators or any joiners, Revisionist}
                       wanted_new_initiation :=
                              (Is_1stYear_Revision (ccode1, ccode2, year, dispnum, use_param)
                               or Is_1stYear_Joined_Revision (ccode1, ccode2, year, dispnum, use_param)
                               or Is_1stYear_Joined_SQ (ccode1, ccode2, year, dispnum, use_param));
                 end
            else {should never get here!}
               EUGeneError ('Did not have an appropriate initiator identifier in wanted_new_or_ongoing_initiation',
                               5, stop, error_log);

         end;     {function wanted_new_initiation}


     function TMaoz_Dyadic_dispute_data_obj.wanted_new_or_continuing_initiation (const ccode1, ccode2 : ccode_range;
                 const year : year_range; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if there is a new initiation or a continuing dispute,
         of a type I want in this year,
         meaning checking sideA vs. Revisionist, and originator or joiner.
         Used by procedured to check printing a dyad year iff a new dispute}
         begin
            check_initialized;
            if user_selections.dispute_info.OnlyTrueInitiators then   {originators only, not joiners}
               begin
                 if user_selections.dispute_info.SideAIsInitiator then    {originators, sideA}
                       wanted_new_or_continuing_initiation := Is_AnyYear_Initiation (ccode1, ccode2, year, dispnum, use_param)
                   else     {originators, Revisionist}
                       wanted_new_or_continuing_initiation := Is_AnyYear_Revision (ccode1, ccode2, year, dispnum, use_param);
               end   {sideA is initiator}
            else     {want originators or init side joiners}
            if user_selections.dispute_info.JoinersOnInitiatingSideAsInitiators then
               begin
                 if user_selections.dispute_info.SideAIsInitiator then   {originators or joiners, sideA}
                     wanted_new_or_continuing_initiation :=
                              (Is_AnyYear_Initiation (ccode1, ccode2, year, dispnum, use_param)
                               or Is_AnyYear_Joined_Initiation (ccode1, ccode2, year, dispnum, use_param))
                     else   {originators or joiners, Revisionist}
                        wanted_new_or_continuing_initiation :=
                               (Is_AnyYear_Revision (ccode1, ccode2, year, dispnum, use_param)
                                or Is_AnyYear_Joined_Revision (ccode1, ccode2, year, dispnum, use_param));
               end
            else     {want originators or any joiners}
            if user_selections.dispute_info.AllJoinersAsInitiators then
                begin
                 if user_selections.dispute_info.SideAIsInitiator then   {originators or joiners, sideA}
                     wanted_new_or_continuing_initiation :=
                              (Is_AnyYear_Initiation (ccode1, ccode2, year, dispnum, use_param)
                               or Is_AnyYear_Joined_Initiation (ccode1, ccode2, year, dispnum, use_param)
                               or Is_AnyYear_Joined_Targets (ccode1, ccode2, year, dispnum, use_param))
                     else   {originators or joiners, Revisionist}
                        wanted_new_or_continuing_initiation :=
                               (Is_AnyYear_Revision (ccode1, ccode2, year, dispnum, use_param)
                                or Is_AnyYear_Joined_Revision (ccode1, ccode2, year, dispnum, use_param)
                                or Is_AnyYear_Joined_SQ (ccode1, ccode2, year, dispnum, use_param));
               end
            else {should never get here!}
               EUGeneError ('Did not have an appropriate initiator identifier in wanted_new_or_ongoing_initiation',
                               5, stop, error_log);

         end;     {function wanted_new_or_continuing_initiation}


     function TMaoz_Dyadic_dispute_data_obj.wanted_new_NonDir_dispute (const ccode1, ccode2 : ccode_range;
         const year : year_range; const user_selections : user_selection_type;
         var dispnum : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if there is a new non-directed dispute of a type I want in this year,
         meaningn checking originator or joiner.
         Used by procedured to check printing a dyad year iff a new dispute}
         begin
            check_initialized;
            if user_selections.dispute_info.OnlyTrueInitiators then
                {originators only, not joiners}
                  wanted_new_NonDir_dispute := Is_1stYear_NonDir_dispute_originators(ccode1, ccode2, year, dispnum, use_param)
              else    {want originators or joiners }
            if user_selections.dispute_info.AllJoinersAsInitiators then
                   wanted_new_NonDir_dispute := (Is_1stYear_NonDir_dispute_Joiners(ccode1, ccode2, year, dispnum, use_param) or
                       Is_1stYear_NonDir_dispute_originators(ccode1, ccode2, year, dispnum, use_param))
            else {should never get here!}
               EUGeneError ('Did not have an appropriate initiator identifier in wanted_new_nondir_disptue',
                               5, stop, error_log);
         end;     {function wanted_new_dispute}

     function TMaoz_Dyadic_dispute_data_obj.wanted_new_or_Continuing_NonDir_dispute (const ccode1, ccode2 : ccode_range;
                           const year : year_range; const user_selections : user_selection_type;
                           var dispnum : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if there is a new or continuing non-directed dispute of a type I want
         in this year, meaning checking originator or joiner.
         Used by procedures to check printing a dyad year iff a new dispute}
         begin
            check_initialized;
            if user_selections.dispute_info.OnlyTrueInitiators then
                {originators only, not joiners}
                  wanted_new_or_Continuing_NonDir_dispute := Is_AnyYear_NonDir_dispute_originators(ccode1, ccode2, year, dispnum, use_param)
              else    {want originators or joiners }
            if user_selections.dispute_info.AllJoinersAsInitiators then
                   wanted_new_or_Continuing_NonDir_dispute := (Is_AnyYear_NonDir_dispute_Joiners(ccode1, ccode2, year, dispnum, use_param) or
                       Is_AnyYear_NonDir_dispute_originators(ccode1, ccode2, year, dispnum, use_param))
            else {should never get here!}
               EUGeneError ('Did not have an appropriate initiator identifier in wanted_new_nondir_disptue',
                               5, stop, error_log);
         end;     {function wanted_new_dispute}

     function TMaoz_Dyadic_dispute_data_obj.relevant_midnum (const ccode1, ccode2 : ccode_range; const year : year_range;
                 const user_selections : user_selection_type) : longint;

        {returns # of relevant dyadic MidNum for dyad-year.
         0 if no dispute.  For cc1 in dispute with cc2 in year}

        {For directed dyads, rule for where to get the hostility level is as follows.
        {If choose first MID:
            Takes info on the first origination (initiation or first revision, if revisionist selected).
            If no new origination, then first event, including joining.
            If no origination or joining, takes info on ongoing.
        If choose highest hostility level:
            Takes info on MID with highest hostility level for 1, whether origination, joining,
               or ongoing.  If multiple MIDs with same hostility level, takes new initiation first,
               then joining, then ongoing.  Within those categories, takes first date.


        {for nondirected, if choose first MID:
           first see if any new dispute originator this year.
           If multiple, take the first one.
           2nd, take any dispute including joiner.
           3rd, check ongoing disutes, first to see if both originators of an
           ongoing dispute.
           4th, If not ongoing originators, then check ongoing joiners.

         If choose highest host, take the dispute with the highest host for ccode1 *or* 2.}

        var found_dispute_num, function_dispnum, disputenum12, disputenum21 : longint;
            currdisp, currhighestdisp  : longint;
            function_1storigination, function_1stjoining, function_anyorigination, function_anyjoining : longint;
            currhigh1storigination, currhigh1stjoining, currhighanyorigination, currhighanyjoining: boolean;
            curr1storigination, curr1stjoining, curranyorigination, curranyjoining: boolean;
            currhighesthost : hostlevtype;
            {Two helper functions to sequentially set highest hostility dispute #s.}
         procedure set_current_highs (ccode1, ccode2 : ccode_range; year : year_range; currdisp : longint);
            begin
              currhigh1storigination :=
                 ( (user_selections.dispute_info.SideAIsInitiator and
                    Is_1stYear_Initiation (ccode1, ccode2, year, currdisp, use_dispute))
                  or (user_selections.dispute_info.SideAIsInitiator and
                      Is_1stYear_Revision (ccode1, ccode2, year, currdisp, use_dispute)) );
              currhigh1stjoining :=
                 ( (user_selections.dispute_info.SideAIsInitiator and
                    (Is_1stYear_Joined_Initiation (ccode1, ccode2, year, currdisp, use_dispute) or
                     Is_1styear_joined_targets (ccode1, ccode2, year, currdisp, use_dispute)) )
                  or (user_selections.dispute_info.SideAIsInitiator and
                     (Is_1stYear_Joined_Revision (ccode1, ccode2, year, currdisp, use_dispute) or
                      Is_1stYear_Joined_SQ (ccode1, ccode2, year, currdisp, use_dispute)) ) );
              currhighanyorigination :=
                 ( (user_selections.dispute_info.SideAIsInitiator and
                    Is_AnyYear_Initiation (ccode1, ccode2, year, currdisp, use_dispute))
                  or (user_selections.dispute_info.SideAIsInitiator and
                      Is_AnyYear_Revision (ccode1, ccode2, year, currdisp, use_dispute)) );
              currhighanyjoining :=
                 ( (user_selections.dispute_info.SideAIsInitiator and
                    (Is_AnyYear_Joined_Initiation (ccode1, ccode2, year, currdisp, use_dispute) or
                     Is_AnyYear_Joined_targets (ccode1, ccode2, year, currdisp, use_dispute)) )
                  or (user_selections.dispute_info.SideAIsInitiator and
                     (Is_AnyYear_Joined_Revision (ccode1, ccode2, year, currdisp, use_dispute) or
                      Is_AnyYear_Joined_SQ (ccode1, ccode2, year, currdisp, use_dispute)) ) );
            end;
         procedure set_current (ccode1, ccode2 : ccode_range; year : year_range; currdisp : longint);
            begin
              curr1storigination :=
                 ( (user_selections.dispute_info.SideAIsInitiator and
                    Is_1stYear_Initiation (ccode1, ccode2, year, currdisp, use_dispute))
                  or (user_selections.dispute_info.SideAIsInitiator and
                      Is_1stYear_Revision (ccode1, ccode2, year, currdisp, use_dispute)) );
              curr1stjoining :=
                 ( (user_selections.dispute_info.SideAIsInitiator and
                    (Is_1stYear_Joined_Initiation (ccode1, ccode2, year, currdisp, use_dispute) or
                     Is_1styear_joined_targets (ccode1, ccode2, year, currdisp, use_dispute)) )
                  or (user_selections.dispute_info.SideAIsInitiator and
                     (Is_1stYear_Joined_Revision (ccode1, ccode2, year, currdisp, use_dispute) or
                      Is_1stYear_Joined_SQ (ccode1, ccode2, year, currdisp, use_dispute)) ) );
              curranyorigination :=
                 ( (user_selections.dispute_info.SideAIsInitiator and
                    Is_AnyYear_Initiation (ccode1, ccode2, year, currdisp, use_dispute))
                  or (user_selections.dispute_info.SideAIsInitiator and
                      Is_AnyYear_Revision (ccode1, ccode2, year, currdisp, use_dispute)) );
              curranyjoining :=
                 ( (user_selections.dispute_info.SideAIsInitiator and
                    (Is_AnyYear_Joined_Initiation (ccode1, ccode2, year, currdisp, use_dispute) or
                     Is_AnyYear_Joined_targets (ccode1, ccode2, year, currdisp, use_dispute)) )
                  or (user_selections.dispute_info.SideAIsInitiator and
                     (Is_AnyYear_Joined_Revision (ccode1, ccode2, year, currdisp, use_dispute) or
                      Is_AnyYear_Joined_SQ (ccode1, ccode2, year, currdisp, use_dispute)) ) );
            end;
        begin                {main proc}
           result := null_dispute_number;
           check_initialized;
           found_dispute_num := null_dispute_number;
           function_dispnum := null_dispute_number;
           disputenum12 := index^[ccode1, ccode2];
           disputenum21 := index^[ccode2, ccode1];
           currdisp := 0;
           currhighestdisp := null_dispute_number;
           currhighesthost := 0;
               {preliminarily, check and see if any disputes for this dyad}
           if ((disputenum12 = null_dispute_number) and (disputenum21 = null_dispute_number)) then found_dispute_num := null_dispute_number
           else
           begin
              if ((user_selections.output_this = output_directed_dyads) or
                  (user_selections.output_this = output_nondirected_dispute_dyads) or
                  (user_selections.output_this = output_directed_dispute_initiation_dyads)) then
              begin
                 if user_selections.dispute_info.UseMostSeriousDispute = true then
                    begin
                       if disputenum12 <> null_dispute_number then   {only check 1 vs. 2 if have a 1vs2 disp entry in data}
                          begin
                             currdisp := disputenum12;
                             if (year = get_year(currdisp)) then
                                begin
                                   currhighestdisp := currdisp;
                                   currhighesthost := get_hostlev_state (currdisp, ccode1);
                                   set_current_highs (ccode1, ccode2, year, currdisp);
                                end;
                             repeat  {check for the event that had highest host level in this year}
                                if (year = get_year(currdisp)) and
                                   (get_hostlev_state (currdisp, ccode1) > currhighesthost) then
                                   begin
                                      currhighestdisp := currdisp;
                                      currhighesthost := get_hostlev_state (currdisp, ccode1);
                                      set_current_highs (ccode1, ccode2, year, currdisp);
                                   end
                                else
                                   if (year = get_year(currdisp)) and
                                      (get_hostlev_state (currdisp, ccode1) = currhighesthost) then
                                      begin
                                         set_current (ccode1, ccode2, year, currdisp);
                                         {Now, saw equal hostlev.  so sequentially take function value from routines generated above}
                                         if currhigh1storigination then
                                            begin
                                               {already have an origination, don't change the value.}
                                            end
                                         else   {not an origination, so only take new record if its a new joining.}
                                         if currhigh1stjoining then
                                            begin
                                               if curr1storigination then
                                                  begin
                                                     currhighestdisp := currdisp;
                                                     currhighesthost := get_hostlev_state (currdisp, ccode1);
                                                     set_current_highs (ccode1, ccode2, year, currdisp);
                                                  end;
                                            end
                                         else
                                         if currhighanyorigination then
                                            begin
                                               if (curr1storigination or curr1stjoining) then
                                                  begin
                                                     currhighestdisp := currdisp;
                                                     currhighesthost := get_hostlev_state (currdisp, ccode1);
                                                     set_current_highs (ccode1, ccode2, year, currdisp);
                                                  end;
                                            end
                                         else
                                         if currhighanyjoining then
                                            begin
                                               if (curr1storigination or curr1stjoining or curranyorigination) then
                                                  begin
                                                     currhighestdisp := currdisp;
                                                     currhighesthost := get_hostlev_state (currdisp, ccode1);
                                                     set_current_highs (ccode1, ccode2, year, currdisp);
                                                  end;
                                            end;
                                      end;
                                inc(currdisp);
                             until ((currdisp > get_last_dispnum) or
                                    (get_year(currdisp) > year) or
                                    (get_ccode(currdisp, 0) <> ccode1) or
                                    (get_ccode(currdisp, 1) <> ccode2));
                          end;       {if dispute12 not 0}

                       if disputenum21 <> null_dispute_number then   {only check 2 vs. 1 if have a 2vs1 disp entry in data}
                          begin
                             currdisp := disputenum21;
                             if currhighestdisp = null_dispute_number then      {only reset highest if didn't find one under the 12 direction}
                             if (year = get_year(currdisp)) then
                             begin
                                currhighestdisp := currdisp;
                                {still want to get the hostility levels for ccode 1}
                                currhighesthost := get_hostlev_state (currdisp, ccode1);
                                set_current_highs (ccode1, ccode2, year, currdisp);
                             end;
                             repeat  {check for the event that had highest host level in this year}
                                if (year = get_year(currdisp)) and
                                   (get_hostlev_state (currdisp, ccode1) > currhighesthost) then
                                   begin
                                      currhighestdisp := currdisp;
                                      currhighesthost := get_hostlev_state (currdisp, ccode1);
                                      set_current_highs (ccode1, ccode2, year, currdisp);
                                   end
                                else
                                   if (year = get_year(currdisp)) and
                                      (get_hostlev_state (currdisp, ccode1) = currhighesthost) then
                                      begin
                                         set_current (ccode1, ccode2, year, currdisp);
                                         {Now, saw equal hostlev.  so sequentially take function value from routines generated above}
                                         if currhigh1storigination then
                                            begin
                                               {already have an origination, don't change the value.}
                                            end
                                         else   {not an origination, so only take new record if its a new joining.}
                                         if currhigh1stjoining then
                                            begin
                                               if curr1storigination then
                                                  begin
                                                     currhighestdisp := currdisp;
                                                     currhighesthost := get_hostlev_state (currdisp, ccode1);
                                                     set_current_highs (ccode1, ccode2, year, currdisp);
                                                  end;
                                            end
                                         else
                                         if currhighanyorigination then
                                            begin
                                               if (curr1storigination or curr1stjoining) then
                                                  begin
                                                     currhighestdisp := currdisp;
                                                     currhighesthost := get_hostlev_state (currdisp, ccode1);
                                                     set_current_highs (ccode1, ccode2, year, currdisp);
                                                  end;
                                            end
                                         else
                                         if currhighanyjoining then
                                            begin
                                               if (curr1storigination or curr1stjoining or curranyorigination) then
                                                  begin
                                                     currhighestdisp := currdisp;
                                                     currhighesthost := get_hostlev_state (currdisp, ccode1);
                                                     set_current_highs (ccode1, ccode2, year, currdisp);
                                                  end;
                                            end;
                                      end;
                                inc(currdisp);
                             until ((currdisp > get_last_dispnum) or
                                    (get_year(currdisp) > year) or
                                    (get_ccode(currdisp, 0) <> ccode2) or
                                    (get_ccode(currdisp, 1) <> ccode1));
                          end;       {if dispute21 not 0}

                       found_dispute_num := currhighestdisp;
                    end      {if use most serious}
                 else if user_selections.dispute_info.UseFirstDispute = true then
                    begin
                           {first, check for true initiations}
                       if (user_selections.dispute_info.SideAIsInitiator and
                          Is_1stYear_Initiation (ccode1, ccode2, year, function_dispnum, use_ccodeyear))
                          then found_dispute_num := function_dispnum
                       else  {2nd, check for true initiations by revisionist state}
                          if (not(user_selections.dispute_info.SideAIsInitiator) and
                              Is_1stYear_Revision (ccode1, ccode2, year, function_dispnum, use_ccodeyear))
                             then found_dispute_num := function_dispnum
                       else    {not an initiation or revision attempt, but there are disputes,
                                so check them for new dispute, or ongoing.}
                          begin
                             if disputenum12 <> null_dispute_number then   {only check 1 vs. 2 if have a 1vs2 disp entry in data}
                                repeat  {3rd, check for any event that was new in this year}
                                  {need to check disputes to see if anything starts this year}
                                   if (get_year(disputenum12) = year) then
                                      {found a dispute that started in this year, take that hostlev}
                                      found_dispute_num := disputenum12;
                                   inc(disputenum12);
                                until ((found_dispute_num<>null_dispute_number) or
                                       (disputenum12 > get_last_dispnum) or
                                       (get_year(disputenum12) > year) or
                                       (get_ccode(disputenum12, 0) <> ccode1) or
                                       (get_ccode(disputenum12, 1) <> ccode2));
                             if found_dispute_num = null_dispute_number then   {if not yet found, check reverse dyad, cc2 vs. cc1}
                             if disputenum21 <> null_dispute_number then   {only check 2 vs. 1 if have a 2vs1 disp entry in data}
                                repeat
                                  {need to check disputes to see if anything starts this year}
                                   if (get_year(disputenum21) = year) then
                                      {found a dispute that started in this year, take that hostlev}
                                      found_dispute_num := disputenum21;
                                   inc(disputenum21);
                                until ((found_dispute_num<>null_dispute_number) or
                                       (disputenum21 > get_last_dispnum) or
                                       (get_year(disputenum21) > year) or
                                       (get_ccode(disputenum21, 0) <> ccode2) or
                                       (get_ccode(disputenum21, 1) <> ccode1));

                                    {4th, still not found, check to see if something was ongoing}
                             if found_dispute_num = null_dispute_number then
                                if is_ongoing (ccode1, ccode2, year, function_dispnum) then
                                   found_dispute_num := function_dispnum;

                             {else at this point, result [=relevant_midnum] is still 0, meaning there
                              was no initiation or ongoing dispute this year.  }
                          end; {not a revision/initiation}

                    end      {directed - if use first}
                 else EUGeneError ('error in proc get relevant midnum, user_selections not appropriately set for usefirstdispute or usemostseriousdispute.  Fatal error - check program.',1,stop,error_log);
              end         {directed}

              else
              if (user_selections.output_this = output_nondirected_dyads) then
                 begin
                    if user_selections.dispute_info.UseMostSeriousDispute = true then
                       begin
                       if disputenum12 <> initialized_value then   {only check 1 vs. 2 if have a 1vs2 disp entry in data}
                          begin
                             currdisp := disputenum12;
                             if (year = get_year(currdisp))  then
                                begin
                                   currhighestdisp := currdisp;
                                   currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                end;
                             repeat  {check for the event that had highest host level in this year}
                                if (year = get_year(currdisp)) and
                                   (max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2)) > currhighesthost) then
                                   begin
                                      currhighestdisp := currdisp;
                                      currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                   end
                                else
                                if (year = get_year(currdisp)) and
                                   (max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2)) = currhighesthost) then
                                   begin
                                      if Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, currhighestdisp, use_dispute) then
                                         begin    {do nothing, already originators}
                                         end
                                      else
                                      if Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, currhighestdisp, use_dispute) then
                                         begin
                                            if Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, currdisp, use_dispute) then
                                            begin
                                               currhighestdisp := currdisp;
                                               currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                            end
                                         end
                                      else
                                      if Is_AnyYear_NonDir_Dispute_Originators (ccode1, ccode2, year, function_dispnum, use_ccodeyear) then
                                         begin
                                            if (Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, currdisp, use_dispute) or
                                                Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, currhighestdisp, use_dispute) ) then
                                            begin
                                               currhighestdisp := currdisp;
                                               currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                            end
                                         end
                                      else
                                      if Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, function_dispnum, use_ccodeyear) then
                                         begin
                                            if (Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, currdisp, use_dispute) or
                                                Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, currhighestdisp, use_dispute) or
                                                Is_AnyYear_NonDir_Dispute_Originators (ccode1, ccode2, year, function_dispnum, use_ccodeyear) ) then
                                            begin
                                               currhighestdisp := currdisp;
                                               currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                            end
                                         end;
                                   end;
                                inc(currdisp);
                             until ((currdisp > get_last_dispnum) or
                                    (get_year(currdisp) > year) or
                                    (get_ccode(currdisp, 0) <> ccode1) or
                                    (get_ccode(currdisp, 1) <> ccode2));
                          end;       {if dispute12 not 0}
                       if disputenum21 <> null_dispute_number then   {only check 2 vs. 1 if have entry in data}
                          begin
                             currdisp := disputenum21;
                             if currhighestdisp = 0 then
                             if (year = get_year(currdisp)) then
                                begin
                                   currhighestdisp := currdisp;
                                   currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                end;
                             repeat  {check for the event that had highest host level in this year}
                                if (year = get_year(currdisp))  and
                                   (max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2)) > currhighesthost) then
                                   begin
                                      currhighestdisp := currdisp;
                                      currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                   end
                                else
                                if (year = get_year(currdisp)) and
                                   (max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2)) = currhighesthost) then
                                   begin
                                      if Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, currhighestdisp, use_dispute) then
                                         begin    {do nothing, already originators}
                                         end
                                      else
                                      if Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, currhighestdisp, use_dispute) then
                                         begin
                                            if Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, currdisp, use_dispute) then
                                            begin
                                               currhighestdisp := currdisp;
                                               currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                            end
                                         end
                                      else
                                      if Is_AnyYear_NonDir_Dispute_Originators (ccode1, ccode2, year, function_dispnum, use_ccodeyear) then
                                         begin
                                            if (Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, currdisp, use_dispute) or
                                                Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, currhighestdisp, use_dispute) ) then
                                            begin
                                               currhighestdisp := currdisp;
                                               currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                            end
                                         end
                                      else
                                      if Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, function_dispnum, use_ccodeyear) then
                                         begin
                                            if (Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, currdisp, use_dispute) or
                                                Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, currhighestdisp, use_dispute) or
                                                Is_AnyYear_NonDir_Dispute_Originators (ccode1, ccode2, year, function_dispnum, use_ccodeyear) ) then
                                            begin
                                               currhighestdisp := currdisp;
                                               currhighesthost := max(get_hostlev_state (currdisp, ccode1), get_hostlev_state (currdisp, ccode2));
                                            end
                                         end;
                                   end;
                                inc(currdisp);
                             until ((currdisp > get_last_dispnum) or
                                    (get_year(currdisp) > year) or
                                    (get_ccode(currdisp, 0) <> ccode2) or
                                    (get_ccode(currdisp, 1) <> ccode1));
                          end;       {if dispute21 not 0}

                          found_dispute_num := currhighestdisp;
                       end
                    else if user_selections.dispute_info.UseFirstDispute = true then
                       begin
                            {for nondirected, first see if any new dispute originator this year.
                             If multiple, take the first one.
                             2nd, take first year of any dispute including joiners.
                             3rd, check ongoing disutes, first to see if both originators of an
                             ongoing dispute.
                             4th, If not ongoing originators, then check ongoing joiners.}
                          if Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, function_dispnum, use_ccodeyear)
                             then found_dispute_num := function_dispnum
                          else    {not an originator side dispute this year,
                                   so check them for new dispute and joining.}
                             if Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, function_dispnum, use_ccodeyear)
                                then found_dispute_num := function_dispnum
                          else
                                {3rd, still not found, check to see if something was ongoing}
                             if Is_AnyYear_NonDir_Dispute_Originators (ccode1, ccode2, year, function_dispnum, use_ccodeyear)
                                then found_dispute_num := function_dispnum
                          else
                             {4th, still not found, check to see ongoing joiners}
                             if Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, function_dispnum, use_ccodeyear)
                                then found_dispute_num := function_dispnum;
                                {else at this point, result [=relevant_midnum] is still 0, meaning there
                                 was no initiation or ongoing dispute this year.  }
                       end          {use first dispute true}
                    else EUGeneError ('error in proc get relevant midnum nondirected, user_selections not appropriately set for usefirstdispute or usemostseriousdispute.  Fatal error - check program.',1,stop,error_log);
                 end         {nondirected}

              else
                 begin
                    EUGeneError ('relevant_midnum called but neither dispute-dyad, directed-dyad, or non-directed dyad output selected.  Relevant Hostlev set to 0.',1,continue,error_log);
                    found_dispute_num := null_dispute_number;
                 end;
           end;       {dispute # not null_dispute_number}

           result := found_dispute_num;
        end;
        
     function TMaoz_Dyadic_dispute_data_obj.relevant_hostlev (const ccode1, ccode2 : ccode_range; const year : year_range;
                 const which_state : side_type; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : hostlevtype;
        {returns hostlev, 0 if no dispute, of cc1 in dispute with cc2 in year}
        {Which state is 0 for ccode1, 1 for ccode2.}

        {For directed dyads, rule for where to get the hostility level is as follows.
         1st, if want true initiations, and there's a new initiation in this year, use that.
         2nd, if want true revisionists, do the same but with revisions.
         Then, if there is not a true initiation or revision attempt, then
         3rd, take value of 1st new event in this year if there is one, either join, target, whatever.
         4th, if no new dispute, but one ongoing, take the value of the ongoing disp.
         Finally, if nothing, 0.}

        {for nondirected, first see if any new dispute originator this year.
           If multiple, take the first one.
           2nd, take any dispute including joiner.
           3rd, check ongoing disutes, first to see if both originators of an
           ongoing dispute.
           4th, If not ongoing originators, then check ongoing joiners.}

        {returns the dyadic dispute number of the dispute it is reporting the hostility level for.}

        {** Note:  This code is identical to the COW procedure. ** }

        var found_dispute_num, disputenum12, disputenum21 : longint;
        begin
           check_initialized;
           relevant_hostlev := 0;
           found_dispute_num := null_dispute_number;
           if use_param = use_dispute then
              begin
                 if which_state = 0 then relevant_hostlev := (get_hostlev_state (dispnum, ccode1))
                 else if which_state = 1 then relevant_hostlev := (get_hostlev_state (dispnum, ccode2));
              end
           else      {search by ccode year}
              begin
                 disputenum12 := index^[ccode1, ccode2];
                 disputenum21 := index^[ccode2, ccode1];
                     {preliminarily, check and see if any disputes for this dyad}
                 if ((disputenum12 = initialized_value) and (disputenum21 = initialized_value)) then relevant_hostlev := 0
                 else
                 begin
                    if ((user_selections.output_this = output_directed_dyads) or
                        (user_selections.output_this = output_nondirected_dispute_dyads) or
                        (user_selections.output_this = output_directed_dispute_initiation_dyads)) then
                        begin
                              {first, check for true initiations}
                          if (user_selections.dispute_info.SideAIsInitiator and
                              Is_1stYear_Initiation (ccode1, ccode2, year, found_dispute_num, use_param))
                             then
                                begin
                                   if which_state = 0 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode1))
                                   else if which_state = 1 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode2));
                                   dispnum := found_dispute_num;
                                end
                          else  {2nd, check for true initiations by revisionist state}
                          if (not(user_selections.dispute_info.SideAIsInitiator) and
                              Is_1stYear_Revision (ccode1, ccode2, year, found_dispute_num, use_param))
                             then
                                begin
                                   if which_state = 0 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode1))
                                   else if which_state = 1 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode2));
                                   dispnum := found_dispute_num;
                                end
                          else    {not an initiation or revision attempt, but there are disputes,
                                   so check them for new dispute, or ongoing.}
                             begin
                                if disputenum12 <> initialized_value then   {only check 1 vs. 2 if have a 1vs2 disp entry in data}
                                   repeat  {3rd, check for any event that was new in this year}
                                     {need to check disputes to see if anything starts this year}
                                      if (get_year(disputenum12) = year) and get_newmid(disputenum12) then
                                         begin
                                            {found a dispute that started in this year, take that hostlev}
                                            if which_state = 0 then relevant_hostlev := (get_hostlev_state (disputenum12, ccode1))
                                            else if which_state = 1 then relevant_hostlev := (get_hostlev_state (disputenum12, ccode2));
                                            dispnum := disputenum12;
                                         end;
                                      inc(disputenum12);
                                   until ((result>0) or
                                          (disputenum12 > get_last_dispnum) or
                                          (get_year(disputenum12) > year) or
                                          (get_ccode(disputenum12, 0) <> ccode1) or
                                          (get_ccode(disputenum12, 1) <> ccode2));
                                if result = 0 then   {if not yet found, check reverse dyad, cc2 vs. cc1}
                                if disputenum21 <> initialized_value then   {only check 2 vs. 1 if have a 2vs1 disp entry in data}
                                   repeat
                                     {need to check disputes to see if anything starts this year}
                                      if (get_year(disputenum21) = year) and get_newmid(disputenum21) then
                                         begin
                                            {found a dispute that started in this year, take that hostlev}
                                            if which_state = 0 then relevant_hostlev := (get_hostlev_state (disputenum21, ccode1))
                                            else if which_state = 1 then relevant_hostlev := (get_hostlev_state (disputenum21, ccode2));
                                            dispnum := disputenum21;
                                         end;
                                      inc(disputenum21);
                                   until ((result>0) or
                                          (disputenum21 > get_last_dispnum) or
                                          (get_year(disputenum21) > year) or
                                          (get_ccode(disputenum21, 0) <> ccode2) or
                                          (get_ccode(disputenum21, 1) <> ccode1));

                                       {4th, still not found, check to see if something was ongoing}
                                if result = 0 then
                                   if is_ongoing (ccode1, ccode2, year, found_dispute_num) then
                                      begin
                                         if which_state = 0 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode1))
                                         else if which_state = 1 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode2));
                                         dispnum := found_dispute_num;
                                      end;

                                {else at this point, result [=relevant_hostlev] is still 0, meaning there
                                 was no initiation or ongoing dispute this year.  So, no hostility for
                                 this ccode.}
                             end; {not a revision/initiation}
                        end   {directed disputes, dispute-dyads}
                     else   {nondirected output}
                     if (user_selections.output_this = output_nondirected_dyads) then
                        begin
                            {for nondirected, first see if any new dispute originator this year.
                             If multiple, take the first one.
                             2nd, take any dispute including joiner.
                             3rd, check ongoing disutes, first to see if both originators of an
                             ongoing dispute.
                             4th, If not ongoing originators, then check ongoing joiners.}
                          if Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2, year, found_dispute_num, use_param) then
                                begin
                                   if which_state = 0 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode1))
                                   else if which_state = 1 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode2));
                                   dispnum := found_dispute_num;
                                end
                          else    {not an originator side dispute this year,
                                   so check them for new dispute and joining.}
                             if Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, found_dispute_num, use_param) then
                                begin
                                   if which_state = 0 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode1))
                                   else if which_state = 1 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode2));
                                   dispnum := found_dispute_num;
                                end
                          else
                                {3rd, still not found, check to see if something was ongoing}
                             if Is_AnyYear_NonDir_Dispute_Originators (ccode1, ccode2, year, found_dispute_num, use_param) then
                                begin
                                   if which_state = 0 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode1))
                                   else if which_state = 1 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode2));
                                   dispnum := found_dispute_num;
                                end
                          else
                             {4th, still not found, check to see ongoing joiners}
                             if Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2, year, found_dispute_num, use_param) then
                                begin
                                   if which_state = 0 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode1))
                                   else if which_state = 1 then relevant_hostlev := (get_hostlev_state (found_dispute_num, ccode2));
                                   dispnum := found_dispute_num;
                                end;
                                {else at this point, result [=relevant_hostlev] is still 0, meaning there
                                 was no initiation or ongoing dispute this year.  So, no hostility for
                                 this ccode.}
                        end        {if nondirected}
                     else
                        begin
                           EUGeneError ('Relevant_hostlev called but neither dispute-dyad, directed-dyad, or non-directed dyad output selected.  Relevant Hostlev set to 0.',1,continue,error_log);
                           relevant_hostlev := 0;
                        end;
                 end;
              end;     {not param = use_dispute #}

           {Also, want to change coding as a "-9" hostility to a 1, which means
            no militarized response, although you were a target.}
           if (result = -9) then relevant_hostlev := 1;

        end;       {proc relevant_hostlev}

     function TMaoz_Dyadic_dispute_data_obj.get_last_dispute_in_year ( const ccode1, ccode2 : ccode_range; const year : year_range;
                 const which_state : side_type; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : TDateTime;
      var disputenum12, disputenum21, dispute_temp: longint;
          temp_month : month_range;
          temp_day : MIDdaytype;
          temp_date : TDateTime;

      begin
            Result := 0;
            dispute_temp := 0;

            disputenum12 := index^[ccode1, ccode2];
            disputenum21 := index^[ccode2, ccode1];

            if disputenum12 <> null_dispute_number then   {only check 1 vs. 2 if have a 1vs2 disp entry in data}
              repeat
                if (get_endyear(disputenum12, ccode1) = year) then
                  begin
                    dispute_temp := disputenum12;
                  end;
                inc(disputenum12);
              until ((result>0) or
                (disputenum12 > get_last_dispnum) or
                (get_endyear(disputenum12, ccode1) > year) or
                (get_ccode(disputenum12, 0) <> ccode1) or
                (get_ccode(disputenum12, 1) <> ccode2));
            if dispute_temp <> 0 then
              begin
                temp_month := get_endmonth(dispute_temp,ccode1);
                temp_day := get_endday(dispute_temp,ccode1);
                if((temp_day > 0) and (temp_day < 32)) then
                  begin
                    temp_date := EncodeDate(year, temp_month, temp_day);
                  end
                else
                  begin
                    temp_day := 15;
                    temp_date := EncodeDate(year, temp_month, temp_day);
                  end
              end;
            if (temp_date = 0) then
            if disputenum21 <> null_dispute_number then   {only check 2 vs. 1 if have a 2vs1 disp entry in data}
              repeat
              {need to check disputes to see if anything starts this year}
                if (get_endyear(disputenum21,ccode2) = year) then
                  begin
                    dispute_temp := disputenum21;
                  end;
                inc(disputenum21);
              until ((result>0) or
                (disputenum21 > get_last_dispnum) or
                (get_endyear(disputenum21,ccode2) > year) or
                (get_ccode(disputenum21, 0) <> ccode2) or
                (get_ccode(disputenum21, 1) <> ccode1));
            if dispute_temp <> 0 then
              begin
                temp_month := get_endmonth(dispute_temp, ccode1);
                temp_day := get_endday(dispute_temp, ccode1);
                if((temp_day > 0) and (temp_day < 32)) then
                  temp_date := EncodeDate(year, temp_month, temp_day)
                else
                  begin
                    temp_day := 15;
                    temp_date := EncodeDate(year, temp_month, temp_day);
                  end
              end;
            Result := temp_date;
      end; {get last dispute in year function}

      function TMaoz_Dyadic_dispute_data_obj.get_peace_days (ccode1, ccode2 : ccode_range; year : year_range;
                 user_selections : user_selection_type; werner_peace_years_data : TWernerPeaceYears_obj) : integer;

        var adispnum, ongoing_num, werner_adj : longint;
            hostprev1, hostprev2 : hostlevtype;
            last_year, ctr : integer;
            Current_date, End_Date : TDateTime;

          function all_in_range (ccode1, ccode2 : ccode_range;  ayear : year_range; error_check : boolean): boolean;
            begin
              all_in_range := true;
              if (ccode1=0) or (ccode2=0) then
                all_in_range := false
              else
              if not(initialized) then
                begin
                  all_in_range := false;
                  if error_check then
                     EUGeneError ('Get peace days get called before dispute initialization', 5, stop, error_log);
                end
              else
              if not ((ayear >= get_first_partition_year) and (ayear <= get_last_partition_year)) then
                begin
                  all_in_range := false;
                  if error_check then
                        EUGeneError ('Internal Error in program - called get peace days with year outside partition',
                                        5, continue, error_log);
                end
              else
               if not ((nation_list.is_a_state(ccode1, ayear)) and (nation_list.is_a_state(ccode2, ayear)) ) then
                   begin
                     all_in_range := false;
                     if error_check then
                       trace.message ('Error in ccode-years: called get peace days for invalid ccodes given ccodes/year'+
                                inttostr(ccode1)+' ' + inttostr(ccode2)+ ' in '+inttostr(ayear));
                   end;
          end;  {func all in range}

        begin
          Current_date := EncodeDate(year, 1, 1);
          ongoing_num := null_dispute_number;
          ctr := 0;
          End_date := -99999;
          werner_adj := 0;

          if (all_in_range(ccode1, ccode2, year, false)) then  {call with no error check}
            repeat
              if (((year-ctr) = nation_list.get_startyear1(ccode1)) or ((year-ctr) = nation_list.get_startyear2(ccode1)) or
                    ((year-ctr) = nation_list.get_startyear1(ccode2)) or ((year-ctr) = nation_list.get_startyear2(ccode2)) )
                    then End_date := EncodeDate((year-ctr),1,1);
               if ((year-ctr) = 1816) then
                  begin
                    if user_selections.werner_peace_year_adjustment=true then
                          werner_adj := werner_peace_years_data.get_werner_peacedays(ccode1, ccode2);
                    End_date := EncodeDate(1816,1,1);
                  end;

              if(ctr = 0) then
                begin
                  if is_ongoing(ccode1, ccode2, year, ongoing_num) then
                    End_date := Current_date;
                end
              else if(ctr > 0) then
                begin
                  hostprev1 := relevant_hostlev (ccode1, ccode2, (year-ctr), 0, user_selections, adispnum, use_ccodeyear);
                  hostprev2 := relevant_hostlev (ccode1, ccode2, (year-ctr), 1, user_selections, adispnum, use_ccodeyear);
                  if((hostprev1 > 0) or (hostprev2 > 0)) then
                    End_date := get_last_dispute_in_year(ccode1, ccode2, (year-ctr), 1, user_selections, adispnum, use_ccodeyear);
                end;




              ctr := ctr + 1;

              until(End_date <> -99999);
            Result := werner_adj + DaysBetween(Current_date, End_date);
        end;

      function TMaoz_Dyadic_dispute_data_obj.get_peace_years (ccode1, ccode2 : ccode_range; year : year_range;
                               user_selections : user_selection_type; werner_peace_years_data : TWernerPeaceYears_obj) : integer;
         var adispnum: longint;
            hostprev1, hostprev2 : hostlevtype;
            last_year : integer;
           {returns # of years since last dispute.  1816 always gets value 0, and series increments
            from there}

         function all_in_range (ccode1, ccode2 : ccode_range;  ayear : year_range; error_check : boolean): boolean;
         begin
            all_in_range := true;
            if (ccode1=0) or (ccode2=0) then
               all_in_range := false
            else
            if not(initialized) then
               begin
                  all_in_range := false;
                  if error_check then
                     EUGeneError ('Get peace years get called before dispute initialization', 5, stop, error_log);
               end
            else
            if not ((ayear >= get_first_partition_year) and (ayear <= get_last_partition_year)) then
               begin
                  all_in_range := false;
                  if error_check then
                        EUGeneError ('Internal Error in program - called get peace years with year outside partition',
                                        5, continue, error_log);
               end
            else
               if not ((nation_list.is_a_state(ccode1, ayear)) and (nation_list.is_a_state(ccode2, ayear)) ) then
                  begin
                     all_in_range := false;
                     if error_check then
                           trace.message ('Error in ccode-years: called get peace years for invalid ccodes given ccodes/year'+
                                 inttostr(ccode1)+' ' + inttostr(ccode2)+ ' in '+inttostr(ayear));
                  end;
        end;  {func all in range}

        begin                    {main proc get peace years}
           get_peace_years := missing_value;
           if (all_in_range(ccode1, ccode2, year, false)) then  {call with no error check}
              begin
                 if (year = 1816) then
                    begin
                       if user_selections.werner_peace_year_adjustment=true then
                          get_peace_years := werner_peace_years_data.get_werner_peaceyears(ccode1, ccode2)
                       else get_peace_years := 0;
                    end
                 else
                    if ((year = nation_list.get_startyear1(ccode1)) or (year = nation_list.get_startyear2(ccode1)) or
                        (year = nation_list.get_startyear1(ccode2)) or (year = nation_list.get_startyear2(ccode2)) )
                    then get_peace_years := 0
                 else     {not 1816, and neither is a new state}
                    begin
                         {need to look at hostility in the previous year;  if there was a MID,
                          then this year's peace years is 0}
                       hostprev1 := relevant_hostlev (ccode1, ccode2, (year-1), 0, user_selections, adispnum, use_ccodeyear);
                       hostprev2 := relevant_hostlev (ccode1, ccode2, (year-1), 1, user_selections, adispnum, use_ccodeyear);
                       if ((hostprev1 = missing_value) or (hostprev2 = missing_value)) then get_peace_years := missing_value
                       else
                          if (hostprev1 > 0) or (hostprev2 > 0) then get_peace_years := 0
                       else    {host last year was not missing, and not 0}
                          {There is no hostility last year, so increment last year's value.
                           But, to speed up this calculation, I sometimes (usually) have last year's value in memory already}
                          if ((stored_peaceyrs.ccode1 = ccode1) and (stored_peaceyrs.ccode2 = ccode2) and
                              (stored_peaceyrs.year = year - 1) )
                              then
                                 begin
                                    if stored_peaceyrs.numyears = missing_value then get_peace_years := missing_value
                                    else get_peace_years := (stored_peaceyrs.numyears + 1);
                                 end
                          else    {don't have last year's value in memory, so do manual recursion}
                             begin
                                last_year := get_peace_years (ccode1, ccode2, year-1, user_selections, werner_peace_years_data);
                                if last_year = missing_value then get_peace_years := missing_value else
                                get_peace_years := last_year + 1;
                             end;
                    end;
                 {Now store this value of peaceyears to speed up search next time}
                 stored_peaceyrs.ccode1 := ccode1;
                 stored_peaceyrs.ccode2 := ccode2;
                 stored_peaceyrs.year := year;
                 stored_peaceyrs.numyears := result;
              end;
        end;

     function TMaoz_Dyadic_dispute_data_obj.get_num_new_mids (ccode1, ccode2 : ccode_range; year : year_range) : integer;
        {counts number of new MIDs involving cc1 and cc2 in this year.  NONDIRECTED.
         Counts Even if 2 vs. 1.  SO, this is nondirected.}
        var disputenum : longint;
        begin
           check_initialized;
           result := 0;
           disputenum := index^[ccode1, ccode2];       {1st disp-year between cc1, cc2}
           {For Maoz data, the disputes are sorted by  ccode1 ccode2 YEAR, not cc1 cc2 start year.
            So, I can find the first for cc1 and cc2, then look for year before I care about, then see
            if same MID number is continuing in the year I care about.  Also, Maoz has the "newmid" field
            that I can use to see if a mid in a year is new or ongoing.}
           if disputenum <> null_dispute_number then
              repeat
                {need to check all disputes starting with the 1st until have reached the
                 next year, or find one that is ongoing.}
                 if (get_year (disputenum) = year) and (get_newmid (disputenum) = true) then
                    inc(result);
                 inc(disputenum);
              until (
                     (disputenum > get_last_dispnum) or
                     (get_year(disputenum) > year) or
                     (get_ccode(disputenum, 0) <> ccode1) or
                     (get_ccode(disputenum, 1) <> ccode2));

           disputenum := index^[ccode2, ccode1];
           if disputenum <> null_dispute_number then
              repeat
                 if (get_year (disputenum) = year) and (get_newmid (disputenum) = true) then
                    inc(result);
                 inc(disputenum);
              until (
                     (disputenum > get_last_dispnum) or
                     (get_year(disputenum) > year) or
                     (get_ccode(disputenum, 0) <> ccode2) or
                     (get_ccode(disputenum, 1) <> ccode1));

        end;            {proc}


      function TMaoz_Dyadic_dispute_data_obj.get_num_total_mids (ccode1, ccode2 : ccode_range; year : year_range) : integer;
        {counts number of ongoing or new MIDs involving cc1 and cc2.  NONDIRECTED.
         Counts Even if 2 vs. 1.  SO, this is nondirected.}
        var disputenum : longint;
        begin
           check_initialized;
           result := 0;
           disputenum := index^[ccode1, ccode2];       {1st disp-year between cc1, cc2}
           if disputenum <> null_dispute_number then
              repeat
                {need to check all disputes starting with the 1st until have reached the
                 next year, or find one that is ongoing.}
                 if (get_year (disputenum) = year) then
                    inc(result);
                 inc(disputenum);
              until (
                     (disputenum > get_last_dispnum) or
                     (get_year(disputenum) > year) or
                     (get_ccode(disputenum, 0) <> ccode1) or
                     (get_ccode(disputenum, 1) <> ccode2));

           disputenum := index^[ccode2, ccode1];
           if disputenum <> null_dispute_number then
              repeat
                 if (get_year (disputenum) = year) then
                    inc(result);
                 inc(disputenum);
              until (
                     (disputenum > get_last_dispnum) or
                     (get_year(disputenum) > year) or
                     (get_ccode(disputenum, 0) <> ccode2) or
                     (get_ccode(disputenum, 1) <> ccode1));

        end;            {proc}

        {  ------------------------------------------------  }

     {Now, ICB dyadic dispute object}

      {public functions}
      constructor TICBDyadic_dispute_data_obj.init (user_selections : user_selection_type;
                  configuration : configuration_type; year1, year2 : year_range);
      {init reads the ICB dyadic dispute year data.
       It also sorts the dyadic dispute data by ccode1, ccode2, then start year of dispute.}

        var Dyadic_dispute_trace : TTrace_obj;
            heapneeded, start_mem : longint;
            x, y, current_rec, tempint : integer;
            a_rec : ICB_dyadic_dispute_main_rec_ptr;
            done, bad_case : boolean;
            infile : text;

         procedure quicksort_dyadic_disputes_by_country (left, right:integer; sort_dyad:side_type);
                  {Sorts in order low to high ccode between left and right;
                   Will sort by country 1 or country 2 depending on value of sort_dyad.
                   Note that now, for ICB dyads, sort_dyad will indicate to sorty by
                   ccodeA or ccodeB.  }
            const trace_quicksort = false;
            var
               base_ccode, i, j, comp_ccode : integer;
               temp: ICB_dyadic_dispute_main_rec;
            begin
             if right > left then
               begin
                  {v needs to be set to value of right case, ccode of country 1 or ccode of country 2}
                  {dyadic_disputes^[right].pointer1 is case in country_disputes of first ccode}
                  if sort_dyad = 0 then base_ccode := data^[right]^.ccodeA else
                                        base_ccode := data^[right]^.ccodeB;
                  i := left - 1;
                  j := right;
                  repeat
                     repeat
                        i := i + 1;
                        if sort_dyad = 0 then comp_ccode := data^[i]^.ccodeA else
                                              comp_ccode := data^[i]^.ccodeB;
                     until comp_ccode >= base_ccode;
                     repeat
                        j := j - 1;
                        if sort_dyad = 0 then comp_ccode := data^[j]^.ccodeA else
                                              comp_ccode := data^[j]^.ccodeB;
                     until (comp_ccode <= base_ccode) or (j = left);
                     temp := data^[i]^;
                     data^[i]^ := data^[j]^;
                     data^[j]^ := temp;
                  until j <= i;
                  data^[j]^ := data^[i]^;
                  data^[i]^ := data^[right]^;
                  data^[right]^ := temp;
                  quicksort_dyadic_disputes_by_country (left, i-1, sort_dyad);
                  quicksort_dyadic_disputes_by_country (i+1, right, sort_dyad);
               end;
            end;      {procedure quicksort_dyads_by_country}

            {   -----------------------  }

         procedure quicksort_dyadic_disputes_by_year (left, right:integer);
                  {Sorts in order low to high}
            var
               base_year, comp_year, i, j: integer;
               temp: ICB_dyadic_dispute_main_rec;

            begin
             if right > left then
               begin
                  base_year := data^[right]^.year;
                  i := left - 1;
                  j := right;
                  repeat
                     repeat
                        i := i + 1;
                        comp_year := data^[i]^.year;
                     until comp_year >= base_year;
                     repeat
                        j := j - 1;
                        comp_year := data^[j]^.year;
                     until (comp_year <= base_year) or (j = left);
                     temp := data^[i]^;
                     data^[i]^ := data^[j]^;
                     data^[j]^ := temp;
                  until j <= i;
                  data^[j]^ := data^[i]^;
                  data^[i]^ := data^[right]^;
                  data^[right]^ := temp;
                  quicksort_dyadic_disputes_by_year (left, i-1);
                  quicksort_dyadic_disputes_by_year (i+1, right);
               end;
            end;      {procedure quicksort_dyads_by_year}

            {   -----------------------  }

         procedure quicksort_dyadic_disputes_by_icbnum (left, right:integer);
                  {Sorts in order low to high}
            var
               base_crisis, comp_crisis, i, j: integer;
               temp: ICB_dyadic_dispute_main_rec;

            begin
             if right > left then
               begin
                  base_crisis := data^[right]^.crisno;
                  i := left - 1;
                  j := right;
                  repeat
                     repeat
                        i := i + 1;
                        comp_crisis := data^[i]^.crisno;
                     until comp_crisis >= base_crisis;
                     repeat
                        j := j - 1;
                        comp_crisis := data^[j]^.crisno;
                     until (comp_crisis <= base_crisis) or (j = left);
                     temp := data^[i]^;
                     data^[i]^ := data^[j]^;
                     data^[j]^ := temp;
                  until j <= i;
                  data^[j]^ := data^[i]^;
                  data^[i]^ := data^[right]^;
                  data^[right]^ := temp;
                  quicksort_dyadic_disputes_by_icbnum (left, i-1);
                  quicksort_dyadic_disputes_by_icbnum (i+1, right);
               end;
            end;      {procedure quicksort_dyads_ by icb crisis num}

            {   -----------------------  }

        begin   {main proc icb dyadic data init}
           dyadic_dispute_trace := nil;
           try
               trace.enter('Initializing ICB dyadic dispute data, ');
               if year1 > year2 then switch_year (year1, year2);
               self.first_partition_year := year1;
               self.last_partition_year := year2;

               start_mem := memavail;
               heapneeded := TICB_dyadic_dispute_obj_mem_overhead;
               if debug[4] then
                  begin
                     trace.message ('Dyadic Dispute array size calculation');
                     trace.message ('Calc is that '+inttostr(heapneeded)+' needed for all years.');
                     trace.message ('Max avail mem block is '+inttostr(MaxAvail));
                  end;
               if MaxAvail <= (heapneeded) then
                  begin
                     EUGeneError ('Not enough memory for dyadic dispute array. ',
                                     5, stop, error_log);
                  end;

               Dyadic_dispute_trace := TTrace_obj.init(trace.get_trace_level);
               trace.message ('Initializing ICB Dyadic dispute data structure');

               new (data);
               setlength (data^, max_dyadic_dispute_years);
               num_dyadic_dispute_years := 0;
               for x := 0 to max_dyadic_dispute_years-1 do data^[x] := nil;

               new (index);
               for x := 1 to top_nation_num do
                  for y := 1 to top_nation_num do
                     index^[x,y] := null_dispute_number;

               {Now read ICB data from file }

               try
                  assign (infile, configuration.icb_dyadic_file_name);
                  reset (infile);
                  current_rec := 0;

                  {First line is header, skip it}
                  readln (infile);

                  while (not (eof (infile))) and (current_rec <= max_dyadic_dispute_years) do
                     begin
                        Dyadic_dispute_trace.tick ('Executing Procedure: Read ICB Dyadic Disputes',max_dyadic_dispute_years);
                        new(a_rec);

                        a_rec^.crisno := read_csv_int (infile);
                        a_rec^.year := read_csv_int (infile);
                        a_rec^.crisname := read_csv_string (infile);
                        a_rec^.ccodeA := read_csv_int (infile);
                        a_rec^.cabbrevA := read_csv_string (infile);
                        a_rec^.actnumA := read_csv_int (infile);
                        a_rec^.ccodeB := read_csv_int (infile);
                        a_rec^.cabbrevB := read_csv_string (infile);
                        a_rec^.actnumb := read_csv_int (infile);

                        a_rec^.oneside := read_csv_int (infile);
                        if not ((a_rec^.oneside >= 0) and (a_rec^.oneside <= 2)) then
                           EUGeneError ('Oneside variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.cowmema := read_csv_int (infile);
                        if not ((a_rec^.oneside >= 0) and (a_rec^.oneside <= 4)) then
                           EUGeneError ('cowmema variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.cowmemb := read_csv_int (infile);
                        if not ((a_rec^.oneside >= 0) and (a_rec^.oneside <= 4)) then
                           EUGeneError ('cowmemb variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.gwmema := read_csv_int (infile);
                        if not ((a_rec^.oneside >= 0) and (a_rec^.oneside <= 5)) then
                           EUGeneError ('gwmema variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.gwmemb := read_csv_int (infile);
                        if not ((a_rec^.oneside >= 0) and (a_rec^.oneside <= 5)) then
                           EUGeneError ('gwmemb variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.iwca := read_csv_int (infile);
                        if not ((a_rec^.oneside >= 0) and (a_rec^.oneside <= 10)) then
                           EUGeneError ('iwca variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.iwcb := read_csv_int (infile);
                        if not ((a_rec^.oneside >= 0) and (a_rec^.oneside <= 10)) then
                           EUGeneError ('iwcb variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);

                        a_rec^.yrtriga := read_csv_int (infile);
                        if not ((a_rec^.yrtriga >= -99) and (a_rec^.yrtriga <= max_year)) then
                           EUGeneError ('yrtriga variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.motriga := read_csv_int (infile);
                        if not ((a_rec^.motriga >= -99) and (a_rec^.motriga <= 99)) then
                           EUGeneError ('motriga variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.datriga := read_csv_int (infile);
                        if not ((a_rec^.datriga >= -99) and (a_rec^.datriga <= 99)) then
                           EUGeneError ('datriga variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);

                        a_rec^.yrterma := read_csv_int (infile);
                        if not ((a_rec^.yrterma >= -99) and (a_rec^.yrterma <= max_year)) then
                           EUGeneError ('yrterma variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.moterma := read_csv_int (infile);
                        if not ((a_rec^.moterma >= -99) and (a_rec^.moterma <= 99)) then
                           EUGeneError ('moterma variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.daterma := read_csv_int (infile);
                        if not ((a_rec^.daterma >= -99) and (a_rec^.daterma <= 99)) then
                           EUGeneError ('daterma variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);

                        a_rec^.yrtrigb := read_csv_int (infile);
                        if not ((a_rec^.yrtrigb >= -99) and (a_rec^.yrtrigb <= max_year)) then
                           EUGeneError ('yrtrigb variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.motrigb := read_csv_int (infile);
                        if not ((a_rec^.motrigb >= -99) and (a_rec^.motrigb <= 99)) then
                           EUGeneError ('motrigb variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.datrigb := read_csv_int (infile);
                        if not ((a_rec^.datrigb >= -99) and (a_rec^.datrigb <= 99)) then
                           EUGeneError ('datrigb variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);

                        a_rec^.yrtermb := read_csv_int (infile);
                        if not ((a_rec^.yrtermb >= -99) and (a_rec^.yrtermb <= max_year)) then
                           EUGeneError ('yrtermb variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.motermb := read_csv_int (infile);
                        if not ((a_rec^.motermb >= -99) and (a_rec^.motermb <= 99)) then
                           EUGeneError ('motermb variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.datermb := read_csv_int (infile);
                        if not ((a_rec^.datermb >= -99) and (a_rec^.datermb <= 99)) then
                           EUGeneError ('datermb variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);

                        a_rec^.trgyrdy := read_csv_int (infile);
                        if not ((a_rec^.trgyrdy >= -99) and (a_rec^.trgyrdy <= max_year)) then
                           EUGeneError ('trgyrdy variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.trgmody := read_csv_int (infile);
                        if not ((a_rec^.trgmody >= -99) and (a_rec^.trgmody <= 99)) then
                           EUGeneError ('trgmody variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.trgdady := read_csv_int (infile);
                        if not ((a_rec^.trgdady >= -99) and (a_rec^.trgdady <= 99)) then
                           EUGeneError ('trgdady variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);

                        a_rec^.trmyrdy := read_csv_int (infile);
                        if not ((a_rec^.trmyrdy >= -99) and (a_rec^.trmyrdy <= max_year)) then
                           EUGeneError ('trmyrdy variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.trmmody := read_csv_int (infile);
                        if not ((a_rec^.trmmody >= -99) and (a_rec^.trmmody <= 99)) then
                           EUGeneError ('trmmody variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.trmdady := read_csv_int (infile);
                        if not ((a_rec^.trmdady >= -99) and (a_rec^.trmdady <= 99)) then
                           EUGeneError ('trmdady variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);

                        a_rec^.durdays := read_csv_int (infile);
                        a_rec^.duryear := read_csv_int (infile);
                        a_rec^.ongoing := read_csv_int (infile);
                        if not ((a_rec^.ongoing >= 0) and (a_rec^.ongoing <= 1)) then
                           EUGeneError ('ongoing variable read from ICB dyadic file is out of range.  Notify programmer of possible data set or read procedure error.  Program continues, but further errors may result.',1,continue, error_log);
                        a_rec^.crdynum := read_csv_int (infile);
                        if a_rec^.crdynum > max_dyadic_dispute_years then
                           EUGeneError ('Dispute ID num read from ICB dyadic file is out of range, > max_dyadic_dispute_years.  Notify programmer.  Program continues, but further errors may result.',1,continue, error_log);

                        a_rec^.originatora := false;   {will compute this later}
                        a_rec^.originatorb := false;   {will compute this later}

                        if (not (eof (infile))) then
                           readln (infile);      {go to begin of next record}

                        {Just successfully read a crisis:  Put it on the dispute list
                         (but then check to see if it occasionally has to be removed)}
                        data^[current_rec] := a_rec;
                        inc(current_rec);

                              {There is an occasional (1 case) problem with the COW MID data,
                               where zimbabwe supposedly has a 1965 dispute but isn't a state
                               until 1966.  This could be a symptom of other problems, though, so do
                               a check, and if a case is listed as a dispute in a year but the states aren't
                               really states according to COW, drop it.}
                        if (not(nation_list.is_a_state(a_rec^.ccodeA, a_rec^.year)) or
                            not(nation_list.is_a_state(a_rec^.ccodeB, a_rec^.year)) ) then
                           begin
                              trace.message ('Dropping ICB Crisis where ccode '+inttostr(a_rec^.ccodeA)+ ' or '+ inttostr(a_rec^.ccodeB)+
                                       ' is not a COW state in the year of the ICB dyadic dispute, '+inttostr(a_rec^.year));
                              {To drop this case, just decrement the counter so that the next dispute
                               will be assigned to this slot.}
                              dec(current_rec);
                              data^[current_rec] := nil;
                              bad_case := true;
                              dispose (a_rec);
                           end

                     end;             {while not eof (infile);}

                     num_dyadic_dispute_years := current_rec;
                     if num_dyadic_dispute_years >= max_dyadic_dispute_years then
                        begin
                           ShowMessage ('Stopped reading ICB dyadic dispute data at record '+inttostr(num_dyadic_dispute_years)+
                                    ';  maximum number of dyadic dispute years is '+inttostr(max_dyadic_dispute_years));
                        end;

                     Dyadic_dispute_trace.tickdone;
                     created := true;

                  {Now have all ICB data (dyad-year) read into structure.   }

                  {Sort the list of dyadic-dispute years by the countries involved and by start date.}
                  trace.message ('Finished constructing ICB dyadic crises.  Now Sorting by country 1.');
                  trace.message ('       '+inttostr(num_dyadic_dispute_years)+ ' dyadic dispute years of all types kept.');
                  {writeln ('Before sorting, dyadic dispute list is');}
                  {list_dyadic_disputes (dyadic_disputes, num_dyadic_disputes, country_disputes); }

                  {First sort by first country in dyad, state 0}
                  quicksort_dyadic_disputes_by_country (0, num_dyadic_dispute_years-1, 0);

                  {writeln ('After sorting by country 1, dyadic dispute list is '); }
                  { list_dyadic_disputes (dyadic_disputes, num_dyadic_disputes, country_disputes);}

                  {That does overall list by first of dyad.  Now do by 2nd country in dyad, within each first}
                  trace.message ('Starting sort by country 2...');
                  x := 0;
                  y := 0;
                  repeat
                     done := false;
                     repeat
                        y := y + 1;
                        if (y > get_last_dispnum) then done:= true
                        else if (data^[x]^.ccodeA) <> (data^[y]^.ccodeA) then done:=true;
                     until done;
                     {now sorting by second country, side 1}
                     if y = get_last_dispnum then quicksort_dyadic_disputes_by_country (x, y, 1) else
                        quicksort_dyadic_disputes_by_country (x, y-1, 1);
                     x := y;
                  until y > get_last_dispnum;
                  {writeln ('After sorting by country 2, dyadic dispute list is ');}
                  {list_dyadic_disputes (dyadic_disputes, num_dyadic_disputes, country_disputes);}

                  {That did sort by first and second in dyad.  Now do by year within each dyad}
                  {Note that this will sort by year, not first year of dispute.  this means if there
                   are multiple dyadic disputes between 2 ccodes in a year, all the 1941 disps will be
                   together, then the 1942s, etc.  }
                  trace.message ('Doing quicksort by year...');
                  x := 0;
                  y := 0;
                  repeat
                     done := false;
                     repeat
                        y := y + 1;
                        if (y >= get_last_dispnum) then done:= true
                        else if ((data^[x]^.ccodeA) <> (data^[y]^.ccodeA)) or
                                ((data^[x]^.ccodeB) <> (data^[y]^.ccodeB)) then done:=true;
                     until done;
                     if y = get_last_dispnum then quicksort_dyadic_disputes_by_year (x, y) else
                        quicksort_dyadic_disputes_by_year (x, y-1);
                     x := y;
                  until y > get_last_dispnum;

                  {Don'need any further sorting for now.}
                  trace.message ('Finished sorting dyadic disputes.');

                  {Now set the index marker to the 1st dispute for each ccode pair.  }
                  {do this by running through the sorted dyads, and marking the index to the correct spot
                   when the spot has not yet been set.}
                  for x := 0 to num_dyadic_dispute_years-1 do
                     if index^[data^[x]^.ccodeA, data^[x]^.ccodeB] = null_dispute_number then
                        index^[data^[x]^.ccodeA, data^[x]^.ccodeB] := x;

                  stored_peaceyrs.ccode1 := min_ccode;
                  stored_peaceyrs.ccode2 := min_ccode;
                  stored_peaceyrs.year := min_year;
                  stored_peaceyrs.numyears := missing_value;

                  {Also need to compute whether each dyad is originator.  For that,
                   need the data sorted by icb number.}
                  {quicksort_dyadic_disputes_by_icbnum (1, num_dyadic_dispute_years);  }

                  compute_originators;

                  Dyadic_dispute_trace.tickdone;

               except  {There's been an error, probably a file error}
                  EUGeneError ('Unable to successfully open and read ICB dyadic file.  Please check that files exist and are not open in another program and try again.',3,continue, error_log);
               end;

           finally
               Dyadic_dispute_trace.free;
               close (infile);
               if debug[4] then
                  trace.message (' required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory for maoz dyadic data');
               trace.exit('Finished initializing, reading, and sorting ICB Dyadic dispute data');
           end;
        end;  {proc init}

            {   -----------------------  }

      destructor TICBDyadic_dispute_data_obj.destroy;
       var x : integer;
       begin
         try
            for x := 0 to max_dyadic_dispute_years-1 do
               if data^[x] <> nil then dispose (data^[x]);
            if data <> nil then dispose(data);
            data := nil;
            created := false;
            inherited destroy;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
       end;

      function TICBDyadic_dispute_data_obj.get_last_dispnum : longint;
      begin
         check_initialized;
         result := num_dyadic_dispute_years - 1;
      end;

            {   -----------------------  }

      procedure TICBDyadic_dispute_data_obj.compute_originators;
         {compute whether each state is an originator in the crisis of which this dyad is a part.}
        type icbnumoriginator_type = array of record
                  icbnum : integer;
                  state : ccode_range;
               end;
        var icbnum, crisisrecord : longint;
            earliest_date_array : array[0..max_icb_crisis_number] of TDateTime;
            new_date, new_datea, new_dateb : TDateTime;
            tryyr, trymo, tryday : integer;
            icbnumoriginators : icbnumoriginator_type;
            x : integer;
            found : boolean;
            originator_index : array[0..max_icb_crisis_number] of integer;
         begin

         {What I want to know is, is a an originator in this crisis, is b an orginator
         in this crisis (the overall crisis.).  }
         {To do this, get first dyadic trigger date of this overall crisis;
         then get first date A, B are involved in this crisis.
         If either is > than that dyadic date, then it is a joiner, not originator.}
           for icbnum := 1 to max_icb_crisis_number do
              begin
                 earliest_date_array[icbnum] := encodedate (max_year, 12, 31);
                 originator_index[icbnum] := -1;
              end;

           for crisisrecord := 0 to get_last_dispnum do
              begin
                 icbnum := get_crisis_num(crisisrecord);
                 tryyr := get_trgyrdy(crisisrecord);
                 trymo := get_trgmody(crisisrecord);
                 tryday := get_trgdady(crisisrecord);
                 {These always have dates on them, so there shouldn't be a need to check for exceptions.}
                 new_date := encodedate (tryyr, trymo, tryday);
                 if new_date < earliest_date_array[icbnum] then earliest_date_array[icbnum] := new_date;
              end;  {at end of this loop, the earliest dyadic date in each crisis is in the temp array.}


              {Now check the dyadic trigger dates for each dyad against
               the dyadic crisis start date.  If the dyadic trigger date is <= earliest date,
               then mark both states in that dyad as originators.
               If it is a one-sided crisis, then the non-crisis actor is always coded as an originator.
               This is already handled in the dyadic trigger dates.}
              {Problem - what if multiple records for a dyad?  Need to do 2 passes.  First mark all
               originator dyads per icbnum.  Then look up each dyad from a crisis record and mark based
               on if icbnum crisis.}

           setlength(icbnumoriginators, 0);
           for crisisrecord := 0 to get_last_dispnum do
              begin
                 icbnum := get_crisis_num(crisisrecord);
                 tryyr := get_trgyrdy(crisisrecord);
                 trymo := get_trgmody(crisisrecord);
                 tryday := get_trgdady(crisisrecord);
                 {Since using dyadic trigger dates, shouldn't need to process -99 or missing codes.}
                 new_datea := encodedate (tryyr, trymo, tryday);
                 if new_datea <= earliest_date_array[icbnum] then
                    begin
                        {add both states as origingators to the icb.}
                        setlength(icbnumoriginators, length(icbnumoriginators)+1);
                        icbnumoriginators[high(icbnumoriginators)].icbnum := icbnum;
                        icbnumoriginators[high(icbnumoriginators)].state := get_ccode(crisisrecord,0);
                        if originator_index[icbnum] = -1 then originator_index[icbnum] := high(icbnumoriginators);
                        setlength(icbnumoriginators, length(icbnumoriginators)+1);
                        icbnumoriginators[high(icbnumoriginators)].icbnum := icbnum;
                        icbnumoriginators[high(icbnumoriginators)].state := get_ccode(crisisrecord,1);
                        {This next line should be redundant because the index was set above.}
                        if originator_index[icbnum] = -1 then originator_index[icbnum] := high(icbnumoriginators);
                    end;
              end;    {for crisisrecord}


           {Earlier code checked the individual crisis dates, which isn't what we end up wanting.
              {Now check if the dates that each a, b in each dyad got involved are the same as
               the dyadic crisis start date.  If it is a one-sided crisis, then the non-crisis
               actor is always coded as an originator.}
      {     setlength(icbnumoriginators, 0);
           for crisisrecord := 0 to get_last_dispnum do
              begin
                 icbnum := get_crisis_num(crisisrecord);
                 tryyr := get_yrtriga(crisisrecord);
                 trymo := get_motriga(crisisrecord);
                 tryday := get_datriga(crisisrecord);
                 {if -99/-99/-99 then make it date of the other in dyad, so that
                  if this is the 1st dyad of the crisis, the -99 state is always an originator.}
      {           if (tryyr = -99) and (trymo = -99) and (tryday = -99) then
                    begin
                       tryyr := get_yrtrigb(crisisrecord);
                       trymo := get_motrigb(crisisrecord);
                       tryday := get_datrigb(crisisrecord);
                    end;
                 if trymo = 99 then trymo := 12;
                 if tryday = 66 then tryday := 10 else
                 if tryday = 77 then tryday := 20 else
                 if tryday = 88 then tryday := Dayofthemonth(endofamonth(tryyr, trymo)) else  {this should be OK, b/c it will only be a 88, 99 if year and month are otherwise set OK.}
      {           if tryday = 99 then tryday := Dayofthemonth(endofamonth(tryyr, trymo));  {this should be OK, b/c it will only be a 88, 99 if year and month are otherwise set OK.}
      {           new_datea := encodedate (tryyr, trymo, tryday);
                 if new_datea <= earliest_date_array[icbnum] then
                    begin
                        {add to an icb.}
      {                  setlength(icbnumoriginators, length(icbnumoriginators)+1);
                        icbnumoriginators[high(icbnumoriginators)].icbnum := icbnum;
                        icbnumoriginators[high(icbnumoriginators)].state := get_ccode(crisisrecord,0);
                        if originator_index[icbnum] = -1 then originator_index[icbnum] := high(icbnumoriginators);
                    end;

                 tryyr := get_yrtrigb(crisisrecord);
                 trymo := get_motrigb(crisisrecord);
                 tryday := get_datrigb(crisisrecord);
                 if (tryyr = -99) and (trymo = -99) and (tryday = -99) then
                    begin
                       tryyr := get_yrtriga(crisisrecord);
                       trymo := get_motriga(crisisrecord);
                       tryday := get_datriga(crisisrecord);
                    end;
                 if trymo = 99 then trymo := 12;
                 if tryday = 66 then tryday := 10 else
                 if tryday = 77 then tryday := 20 else
                 if tryday = 88 then tryday := Dayofthemonth(endofamonth(tryyr, trymo)) else  {this should be OK, b/c it will only be a 88, 99 if year and month are otherwise set OK.}
      {           if tryday = 99 then tryday := Dayofthemonth(endofamonth(tryyr, trymo));  {this should be OK, b/c it will only be a 88, 99 if year and month are otherwise set OK.}
      {           new_dateb := encodedate (tryyr, trymo, tryday);
                 if new_dateb <= earliest_date_array[icbnum] then
                    begin
                        {add to an icb.}
      {                  setlength(icbnumoriginators, length(icbnumoriginators)+1);
                        icbnumoriginators[high(icbnumoriginators)].icbnum := icbnum;
                        icbnumoriginators[high(icbnumoriginators)].state := get_ccode(crisisrecord,1);
                        if originator_index[icbnum] = -1 then originator_index[icbnum] := high(icbnumoriginators);
                    end;

              end;        {for crisisrecord}

           {Now, process the list of originators to apply them to all records of the relevant crises.}
           for crisisrecord := 0 to get_last_dispnum do
              begin
                 icbnum := get_crisis_num(crisisrecord);
                 {check if a, then b, is on the originator list.  If so, then mark it in dyad record}
                 x := originator_index[icbnum];
                 found := false;
                 while (x <= high(icbnumoriginators)) and not (found) {and (icbnumoriginators[x].icbnum = icbnum)} do
                    begin
                       if (icbnumoriginators[x].icbnum = icbnum) and (icbnumoriginators[x].state = get_ccode(crisisrecord,0)) then found := true;
                       inc(x);
                    end;
                 if found then data^[crisisrecord].originatora := true
                       else data^[crisisrecord].originatora := false;

                 x := originator_index[icbnum];
                 found := false;
                 while (x <= high(icbnumoriginators)) and not (found) {and (icbnumoriginators[x].icbnum = icbnum)} do
                    begin
                       if (icbnumoriginators[x].icbnum = icbnum) and (icbnumoriginators[x].state = get_ccode(crisisrecord,1)) then found := true;
                       inc(x);
                    end;
                 if found then data^[crisisrecord].originatorb := true
                       else data^[crisisrecord].originatorb := false;
              end;

              {test proc:  x := low(icbnumoriginators);
              while (x <= high(icbnumoriginators)) do
                 begin
                    if icbnumoriginators[x].icbnum = 83 then
                       trace.message ('record='+inttostr(x)+';  icbnum='+inttostr(icbnumoriginators[x].icbnum)+';  state='+inttostr(icbnumoriginators[x].state));
                    inc(x);
                 end;      }

        end;


              { -------------------------------------------------------------- }

    {Now various functions that look at multiple conditional inputs}

     function TICBDyadic_dispute_data_obj.is_ongoing (ccode1, ccode2 : ccode_range; year : year_range;
                 var ongoing_num : longint) : boolean;
        {returns true if there is a crisis ongoing between cc1, cc2;
         even if 2 vs. 1.  SO, this is nondirected.  Rtns # of ongoing disp in ongoing_num.
         NOTE:  This relies on the ICB coding being correct.}
        var crisisrecord : longint;
        begin
           check_initialized;
           result := false;
           ongoing_num := null_dispute_number;
           crisisrecord := index^[ccode1, ccode2];       {1st crisis-year between cc1, cc2}
           {ICB dyadic data is nondirected, so cc1 < cc2 always}
           {For ICB data, the disputes are sorted by  ccode1 ccode2 YEAR, not cc1 cc2 start year.
            So, I can find the first for cc1 and cc2, then look for year before I care about, then see
            if same Crisis number is continuing in the year I care about.  }
           if crisisrecord <> null_dispute_number then
              repeat
                {need to check all disputes starting with the 1st until have reached the
                 next year, or find one that is ongoing.}
                 if (get_year (crisisrecord) = year) and (get_ongoing (crisisrecord) = 1) then
                    begin
                       result := true;
                       ongoing_num := crisisrecord;
                    end;
                 inc(crisisrecord);
              until ((result=true) or
                     (crisisrecord > get_last_dispnum) or
                     (get_year(crisisrecord) > year) or
                     (get_ccode(crisisrecord, 0) <> ccode1) or
                     (get_ccode(crisisrecord, 1) <> ccode2));
                 {At this point, have examined all records of cc1 vs cc2, and either found that there is
                  a dispute starting in year, or that I went through all the records and there was
                  not anytime a dispute started in year.  }

        end;

     function TICBDyadic_dispute_data_obj.Is_1stYearOfCrisis (ccode1, ccode2 : ccode_range;
              year : year_range; var found_disp_num : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if this is the 1st year of a crisis between cc1, cc2; nondirected.
         Called either by dispute number, or by ccode-year.}
        var crisisrecord : longint;
            none_found : boolean;
        begin
           check_initialized;
           result := false;
           crisisrecord := index^[ccode1, ccode2];
           if use_param = use_dispute then
              begin
                 if (get_trgyrdy (found_disp_num) = year) then result := true;
              end
           else   {use ccode year}
              begin
                 found_disp_num := null_dispute_number;
                 if crisisrecord <> null_dispute_number then
                    repeat
                      {need to check all crises starting with the 1st until another dyad is seen,
                        or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                        I've identified if any of them start in this year.}
                       {All I need to see in dyadic ICB is if this is the trigger year in this dyad.}
                       if (get_trgyrdy (crisisrecord) = year)  then
                          begin
                             result := true;
                             found_disp_num := crisisrecord;
                          end;
                       inc(crisisrecord);
                    until ((result=true) or
                           (crisisrecord > get_last_dispnum) or
                           (get_year(crisisrecord) > year) or
                           (get_ccode(crisisrecord, 0) <> ccode1) or
                           (get_ccode(crisisrecord, 1) <> ccode2));
                       {At this point, have examined all records of cc1 vs cc2, and either found that there is
                        a dispute starting in year, or that I went through all the records and there was
                        not anytime a dispute started in year.  }
              end;   {use_ccodeyear}

           end;

            {   -----------------------  }

    function TICBDyadic_dispute_data_obj.Is_1stYear_NonDir_Dispute_Originators (ccode1, ccode2 : ccode_range; year : year_range;
              var record_num : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 and 2 are originators, in a dispute that started this year.
         Don't care about who is side A or B.  }
        {This marks what we consider a new true, non-directed crisis between cc1 and cc2}
        {This works if called either with a disp# or a ccode-year.}
        var disputenum : longint;
        begin
           check_initialized;
           result := false;
           if use_param = use_dispute then
              begin
                 if ((Is_1stYearOfCrisis (ccode1, ccode2, year, record_num, use_param)) and
                     (Is_originator_in_crisis (ccode1, record_num)) and
                     (Is_originator_in_crisis (ccode2, record_num)) ) then
                     result := true;
              end
           else     {use ccode-year}
              begin
                 record_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                       {check all crises starting with the 1st until another dyad is seen,
                        or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                        I've identified if any of them are initiations.}
                       {This will stop and identify/return the dispute number of
                         the first dispute between A and B in this year}
                       if ((Is_1stYearOfCrisis (ccode1, ccode2, year, disputenum, use_dispute)) and
                           (Is_originator_in_crisis (ccode1, disputenum)) and
                           (Is_originator_in_crisis (ccode2, disputenum))) then
                          begin
                             result := true;
                             record_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));

              end;
        end;

    function TICBDyadic_dispute_data_obj.Is_1stYear_NonDir_Dispute_Joiners (ccode1, ccode2 : ccode_range; year : year_range;
              var record_num : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 and cc2 are in a dispute, but AT LEAST ONE is not an originator,
         and this is the 1st year of the dispute.  }
        var disputenum : longint;
        begin
           check_initialized;
           result := false;
           if use_param = use_dispute then
              begin
                 if ((not(record_num=null_dispute_number)) and
                     (Is_1stYearOfCrisis (ccode1, ccode2, year, record_num, use_param)) and
                     (not (Is_originator_in_crisis (ccode1, record_num) and
                           Is_originator_in_crisis (ccode2, record_num))) ) then
                     result := true;
              end
           else
              begin
                 record_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                       if ( (Is_1stYearOfCrisis (ccode1, ccode2, year, disputenum, use_dispute)) and
                            (not (Is_originator_in_crisis (ccode1, disputenum) and
                                  Is_originator_in_crisis (ccode2, disputenum)) ) ) then
                          begin
                             result := true;
                             record_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
              end;
        end;

    function TICBDyadic_dispute_data_obj.Is_AnyYear_NonDir_Dispute_Originators (ccode1, ccode2 : ccode_range; year : year_range;
              var record_num : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 and 2 are originators, in a dispute that might or might
         not have started this year.  Don't care about who is side A or B.  }
        var disputenum : longint;
        begin
           check_initialized;
           result := false;
           if use_param = use_dispute then
              begin
                 if ((get_year (record_num) = year)  and
                     (Is_originator_in_crisis (ccode1, record_num)) and
                     (Is_originator_in_crisis (ccode2, record_num)) ) then
                     result := true;
              end
           else     {use ccode-year}
              begin
                 record_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                      {check all disputes starting with the 1st until another dyad is seen,
                       or a later start year, to ensure that even if there are multiple disputes in a dyad/year,
                       I've identified if any of them are initiations.}
                       {This will stop and identify/return the dispute number of
                         the first dispute between A and B in this year}
                       if ((get_year (disputenum) = year) and
                           (Is_originator_in_crisis (ccode1, disputenum)) and
                           (Is_originator_in_crisis (ccode2, disputenum))) then
                          begin
                             result := true;
                             record_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
              end;
        end;

    function TICBDyadic_dispute_data_obj.Is_AnyYear_NonDir_Dispute_Joiners (ccode1, ccode2 : ccode_range; year : year_range;
              var record_num : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if ccode1 and cc2 in dispute, and this is ANY year of
         the dispute, BUT at least one is not an originator.  }
        var disputenum : longint;
        begin
           check_initialized;
           result := false;
           if use_param = use_dispute then
              begin
                 if ((not(record_num=null_dispute_number)) and
                     (get_year (record_num) = year) and
                     (not (Is_originator_in_crisis (ccode1, record_num) and
                           Is_originator_in_crisis (ccode2, record_num)) ) ) then
                     result := true;
                end
           else
              begin
                 record_num := null_dispute_number;
                 disputenum := index^[ccode1, ccode2];
                 if disputenum <> null_dispute_number then
                    repeat
                       if ((get_year (disputenum) = year) and
                           (not (Is_originator_in_crisis (ccode1, disputenum) and
                                 Is_originator_in_crisis (ccode2, disputenum))) ) then
                          begin
                             result := true;
                             record_num := disputenum;
                          end;
                       inc(disputenum);
                    until ((result=true) or
                           (disputenum > get_last_dispnum) or
                           (get_year(disputenum) > year) or
                           (get_ccode(disputenum, 0) <> ccode1) or
                           (get_ccode(disputenum, 1) <> ccode2));
              end;
        end;

            {   -----------------------  }

        {Note: the next should never be called}
     function TICBDyadic_dispute_data_obj.Is_AnyYear_Joined_Initiation (ccode1, ccode2 : ccode_range;
              year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        begin
           EUGeneError ('Function Is_AnyYear_Joined_Revision called for ICB data - this should not occur.  Continuing, but check program.',1,continue,error_log);
           result := false;
        end;

     function TICBDyadic_dispute_data_obj.Is_AnyYear_Joined_targets (ccode1, ccode2 : ccode_range;
              year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        begin
           EUGeneError ('Function Is_AnyYear_Joined_Revision called for ICB data - this should not occur.  Continuing, but check program.',1,continue,error_log);
           result := false;
        end;

     function TICBDyadic_dispute_data_obj.Is_AnyYear_Joined_Revision (ccode1, ccode2 : ccode_range;
              year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        begin
           EUGeneError ('Function Is_AnyYear_Joined_Revision called for ICB data - this should not occur.  Continuing, but check program.',1,continue,error_log);
           result := false;
        end;

     function TICBDyadic_dispute_data_obj.Is_AnyYear_Joined_SQ (ccode1, ccode2 : ccode_range;
              year : year_range; var found_dispute_num : longint; const use_param : dyadic_call_parameter) : boolean;
        begin
           EUGeneError ('Function Is_AnyYear_Joined_SQ called for ICB data - this should not occur.  Continuing, but check program.',1,continue,error_log);
           result := false;
        end;

            {   -----------------------  }

     function TICBDyadic_dispute_data_obj.wanted_new_NonDir_Dispute (const ccode1, ccode2 : ccode_range;
              const year : year_range; const user_selections : user_selection_type;
              var dispnum : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if there is a new non-directed dispute of a type I want in this year,
         meaning checking originator or joiner.
         Used by procedures to check printing a dyad year iff a new dispute}
         begin
            check_initialized;
            if user_selections.dispute_info.OnlyTrueInitiators then
                {originators only, not joiners}
                  result := Is_1stYear_NonDir_dispute_originators(ccode1, ccode2, year, dispnum, use_param)
              else    {want originators or joiners }
            if user_selections.dispute_info.AllJoinersAsInitiators then
                   result := (Is_1stYear_NonDir_dispute_Joiners(ccode1, ccode2, year, dispnum, use_param) or
                       Is_1stYear_NonDir_dispute_originators(ccode1, ccode2, year, dispnum, use_param))
            else {should never get here!}
               EUGeneError ('Did not have an appropriate initiator identifier in wanted_new_nondir_dispute (crisis)',
                               5, stop, error_log);
        end;

     function TICBDyadic_dispute_data_obj.wanted_new_or_Continuing_NonDir_Dispute (const ccode1, ccode2 : ccode_range;
              const year : year_range; const user_selections : user_selection_type;
              var dispnum : longint; const use_param : dyadic_call_parameter) : boolean;
        {returns true if there is a new or continuing non-directed dispute of a type I want
         in this year, meaning checking originator or joiner.
         Used by procedures to check printing a dyad year iff a new dispute}
         begin
            check_initialized;
            if user_selections.dispute_info.OnlyTrueInitiators then
                {originators only, not joiners}
                  result := Is_AnyYear_NonDir_dispute_originators(ccode1, ccode2, year, dispnum, use_param)
              else    {want originators or joiners }
            if user_selections.dispute_info.AllJoinersAsInitiators then
                   result := (Is_AnyYear_NonDir_dispute_Joiners(ccode1, ccode2, year, dispnum, use_param) or
                       Is_AnyYear_NonDir_dispute_originators(ccode1, ccode2, year, dispnum, use_param))
            else {should never get here!}
               EUGeneError ('Did not have an appropriate initiator identifier in wanted_new_nondir_dispute/crisis',
                               5, stop, error_log);
        end;

     function TICBDyadic_dispute_data_obj.wanted_new_initiation (const ccode1, ccode2 : ccode_range;
              const year : year_range; const user_selections : user_selection_type;
              var dispnum : longint; const use_param : dyadic_call_parameter) : boolean;
              {Because ICB is nondirected, we want to use the nondirected value for the directed,
               initiation value.  Just check to make sure the ccodes are in the right direction
               for nondirected.}
        var smaller_ccode, larger_ccode : ccode_range;
        begin
           smaller_ccode := min(ccode1, ccode2);
           larger_ccode := max(ccode1, ccode2);
           result := wanted_new_NonDir_Dispute(smaller_ccode, larger_ccode, year, user_selections, dispnum, use_param);
        end;

     function TICBDyadic_dispute_data_obj.wanted_new_or_continuing_initiation (const ccode1, ccode2 : ccode_range;
              const year : year_range; const user_selections : user_selection_type;
              var dispnum : longint; const use_param : dyadic_call_parameter) : boolean;
        var smaller_ccode, larger_ccode : ccode_range;
        begin
           smaller_ccode := min(ccode1, ccode2);
           larger_ccode := max(ccode1, ccode2);
           result := wanted_new_or_Continuing_NonDir_Dispute(smaller_ccode, larger_ccode, year, user_selections, dispnum, use_param);
        end;

     function TICBDyadic_dispute_data_obj.get_last_dispute_in_year ( const ccode1, ccode2 : ccode_range; const year : year_range;
                 const which_state : side_type; const user_selections : user_selection_type;
                 var dispnum : longint; const use_param : dyadic_call_parameter) : TDateTime;
      var disputenum12, disputenum21, dispute_temp: longint;
          temp_month : month_range;
          temp_day : MIDdaytype;
          temp_date : TDateTime;

      begin
            Result := 0;
            dispute_temp := 0;
            temp_date := 0;
            
            disputenum12 := index^[ccode1, ccode2];
            disputenum21 := index^[ccode2, ccode1];

            if disputenum12 <> null_dispute_number then   {only check 1 vs. 2 if have a 1vs2 disp entry in data}
              repeat
                if (get_trmyrdy(disputenum12) = year) then
                  begin
                    dispute_temp := disputenum12;
                  end;
                inc(disputenum12);
              until ((temp_date<>0) or
                (disputenum12 > get_last_dispnum) or
                (get_trmyrdy(disputenum12) > year) or
                (get_ccode(disputenum12, 0) <> ccode1) or
                (get_ccode(disputenum12, 1) <> ccode2));
            if dispute_temp <> 0 then
              begin
                temp_month := get_trmmody(dispute_temp);
                temp_day := get_trmdady(dispute_temp);
                if((temp_day > 0) and (temp_day < 32)) then
                  begin
                    if(temp_day = 66) then
                      temp_day := 10;
                    if(temp_day = 77) then
                      temp_day := 20;
                    if((temp_day = 88) or (temp_day = 99)) then
                      temp_day := 28;
                     {need to set up a statement to actually determine last day of the month at some point}
                    temp_date := EncodeDate(year, temp_month, temp_day);
                  end
                else
                  begin
                    temp_day := 15;
                    temp_date := EncodeDate(year, temp_month, temp_day);
                  end
              end;
            if (temp_date = 0) then
            if disputenum21 <> null_dispute_number then   {only check 2 vs. 1 if have a 2vs1 disp entry in data}
              repeat
              {need to check disputes to see if anything starts this year}
                if (get_trmyrdy(disputenum21) = year) then
                  begin
                    dispute_temp := disputenum21;
                  end;
                inc(disputenum21);
              until ((temp_date<>0) or
                (disputenum21 > get_last_dispnum) or
                (get_trmyrdy(disputenum21) > year) or
                (get_ccode(disputenum21, 0) <> ccode2) or
                (get_ccode(disputenum21, 1) <> ccode1));
            if dispute_temp <> 0 then
              begin
                temp_month := get_trmmody(dispute_temp);
                temp_day := get_trmdady(dispute_temp);
                if((temp_day > 0) and (temp_day < 32)) then
                 begin
                    if(temp_day = 66) then
                      temp_day := 10;
                    if(temp_day = 77) then
                      temp_day := 20;
                    if((temp_day = 88) or (temp_day = 99)) then
                      temp_day := 28;
                     {need to set up a statement to actually determine last day of the month at some point}
                    temp_date := EncodeDate(year, temp_month, temp_day)
                  end
                else
                  begin
                    temp_day := 15;
                    temp_date := EncodeDate(year, temp_month, temp_day);
                  end
              end;
            Result := temp_date;
      end; {get last dispute in year function}

     function TICBDyadic_dispute_data_obj.get_peace_days (ccode1, ccode2 : ccode_range; year : year_range;
                 user_selections : user_selection_type; werner_peace_years_data : TWernerPeaceYears_obj) : integer;

        var adispnum, ongoing_num, werner_adj : longint;
            crisisprev1, crisisprev2 : boolean;
            last_year, ctr : integer;
            Current_date, End_Date : TDateTime;

          function all_in_range (ccode1, ccode2 : ccode_range;  ayear : year_range; error_check : boolean): boolean;
            begin
              all_in_range := true;
              if (ccode1=0) or (ccode2=0) then
                all_in_range := false
              else
              if not(initialized) then
                begin
                  all_in_range := false;
                  if error_check then
                     EUGeneError ('Get peace days get called before dispute initialization', 5, stop, error_log);
                end
              else
              if not ((ayear >= get_first_partition_year) and (ayear <= get_last_partition_year)) then
                begin
                  all_in_range := false;
                  if error_check then
                        EUGeneError ('Internal Error in program - called get peace days with year outside partition',
                                        5, continue, error_log);
                end
              else
               if not ((nation_list.is_a_state(ccode1, ayear)) and (nation_list.is_a_state(ccode2, ayear)) ) then
                   begin
                     all_in_range := false;
                     if error_check then
                       trace.message ('Error in ccode-years: called get peace days for invalid ccodes given ccodes/year'+
                                inttostr(ccode1)+' ' + inttostr(ccode2)+ ' in '+inttostr(ayear));
                   end;
          end;  {func all in range}

        begin
          Current_date := EncodeDate(year, 1, 1);
          ongoing_num := null_dispute_number;
          ctr := 0;
          End_date := -99999;
          werner_adj := 0;

          if (all_in_range(ccode1, ccode2, year, false)) then  {call with no error check}
            repeat
              if (((year-ctr) = nation_list.get_startyear1(ccode1)) or ((year-ctr) = nation_list.get_startyear2(ccode1)) or
                    ((year-ctr) = nation_list.get_startyear1(ccode2)) or ((year-ctr) = nation_list.get_startyear2(ccode2)) )
                    then End_date := EncodeDate((year-ctr),1,1);
               if ((year-ctr) = 1816) then
                  begin
                    if user_selections.werner_peace_year_adjustment=true then
                          werner_adj := werner_peace_years_data.get_werner_peacedays(ccode1, ccode2);
                    End_date := EncodeDate(1816,1,1);
                  end;

              if(ctr = 0) then
                begin
                  if is_ongoing(ccode1, ccode2, year, ongoing_num) then
                    End_date := Current_date;
                  if is_ongoing(ccode2, ccode1, year, ongoing_num) then
                    End_date := Current_date;
                end
              else if(ctr > 0) then
                begin
                  crisisprev1 := (Is_1stYearOfCrisis(ccode1, ccode2, (year-1), adispnum, use_ccodeyear));
                  if(crisisprev1) then
                    End_date := get_last_dispute_in_year(ccode1, ccode2, (year-ctr), 1, user_selections, adispnum, use_ccodeyear);
                  crisisprev2 := (Is_1stYearOfCrisis(ccode2, ccode1, (year-1), adispnum, use_ccodeyear));
                  if(crisisprev2) then
                    End_date := get_last_dispute_in_year(ccode2, ccode1, (year-ctr), 1, user_selections, adispnum, use_ccodeyear);
                end;

              ctr := ctr + 1;

              until(End_date <> -99999);
            Result := werner_adj + DaysBetween(Current_date, End_date);
        end;

     function TICBDyadic_dispute_data_obj.get_peace_years (ccode1, ccode2 : ccode_range; year : year_range;
                 user_selections : user_selection_type; werner_peace_years_data : TWernerPeaceYears_obj) : integer;
         var acrisisnum: longint;
            crisisprev1, crisisprev2 : boolean;
            last_year : integer;
            temp1, temp2 : integer;

           {returns # of years since last dispute.  1816 always gets value 0, and series increments
            from there}

         function all_in_range (ccode1, ccode2 : ccode_range;  ayear : year_range; error_check : boolean): boolean;
         begin
            all_in_range := true;
            if (ccode1=0) or (ccode2=0) then
               all_in_range := false
            else
            if not(initialized) then
               begin
                  all_in_range := false;
                  if error_check then
                     EUGeneError ('Get peace years get called before ICB crisis initialization', 5, stop, error_log);
               end
            else
            if not ((ayear >= get_first_partition_year) and (ayear <= get_last_partition_year)) then
               begin
                  all_in_range := false;
                  if error_check then
                        EUGeneError ('Internal Error in program - called get peace years with year outside partition',
                                        5, continue, error_log);
               end
            else
               if not ((nation_list.is_a_state(ccode1, ayear)) and (nation_list.is_a_state(ccode2, ayear)) ) then
                  begin
                     all_in_range := false;
                     if error_check then
                           trace.message ('Error in ccode-years: called get peace years for invalid ccodes given ccodes/year'+
                                 inttostr(ccode1)+' ' + inttostr(ccode2)+ ' in '+inttostr(ayear));
                  end;
        end;  {func all in range}

        begin                    {main proc get peace years, for ICB}

           {had to create a second crisisprev variable and compare both of them because
            the peace years for the two countries were different}
           result := missing_value;
           if (all_in_range(ccode1, ccode2, year, false)) then  {call with no error check}
              begin
                 if (year = 1816) then
                    begin
                       if user_selections.werner_peace_year_adjustment=true then
                          get_peace_years := werner_peace_years_data.get_werner_peaceyears(ccode1, ccode2)
                       else get_peace_years := 0;
                    end
                 else
                    if ((year = nation_list.get_startyear1(ccode1)) or (year = nation_list.get_startyear2(ccode1)) or
                        (year = nation_list.get_startyear1(ccode2)) or (year = nation_list.get_startyear2(ccode2)) )
                    then get_peace_years := 0
                 else     {not 1816, and neither is a new state}
                    begin
                         {need to look at hostility in the previous year;  if there was a crisis
                          between the states, then this year's peace years is 0}
                       crisisprev1 := (Is_1stYearOfCrisis(ccode1, ccode2, (year-1), acrisisnum, use_ccodeyear)) or (is_ongoing (ccode1, ccode2, (year-1), acrisisnum));
                       if (crisisprev1) then result := 0;
                        {check the reverse}
                       crisisprev2 := (Is_1stYearOfCrisis(ccode2, ccode1, (year-1), acrisisnum, use_ccodeyear)) or (is_ongoing (ccode2, ccode1, (year-1), acrisisnum));
                       if (crisisprev2) then result := 0;

                       if ((crisisprev1 = False) and (crisisprev2 = False)) then    {had no crisis last year}
                         begin
                          {There is no hostility last year, so increment last year's value.
                           But, to speed up this calculation, I sometimes (usually) have last year's value in memory already}
                          if ((stored_peaceyrs.ccode1 = ccode1) and (stored_peaceyrs.ccode2 = ccode2) and
                              (stored_peaceyrs.year = year - 1) )
                              then
                                 begin
                                    if stored_peaceyrs.numyears = missing_value then result := missing_value
                                    else result := (stored_peaceyrs.numyears + 1);
                                 end
                          else    {don't have last year's value in memory, so do manual recursion}
                             begin
                                last_year := get_peace_years (ccode1, ccode2, year-1, user_selections, werner_peace_years_data);
                                if last_year = missing_value then result := missing_value else
                                result := last_year + 1;
                             end;
                        end;
                    end;
                 {Now store this value of peaceyears to speed up search next time}
                 stored_peaceyrs.ccode1 := ccode1;
                 stored_peaceyrs.ccode2 := ccode2;
                 stored_peaceyrs.year := year;
                 stored_peaceyrs.numyears := result;
              end;
        end;

     function TICBDyadic_dispute_data_obj.get_num_new_crises (ccode1, ccode2 : ccode_range; year : year_range) : integer;
        {counts all new crises between cc1, cc2.  }
        var crisisrecord : longint;
        begin
           check_initialized;
           result := 0;
           crisisrecord := index^[ccode1, ccode2];       {1st crisis-year between cc1, cc2}
           {ICB dyadic data is nondirected, so cc1 < cc2 always}
           {For ICB data, the disputes are sorted by  ccode1 ccode2 YEAR, not cc1 cc2 start year.
            So, I can find the first for cc1 and cc2, then look for year before I care about.}
           if crisisrecord <> null_dispute_number then
              repeat
                {need to check all disputes starting with the 1st until have reached the
                 next year or dyad}
                 if (get_year (crisisrecord) = year) and (get_ongoing (crisisrecord) = 0) then
                    inc(result);
                 inc(crisisrecord);
              until (
                     (crisisrecord > get_last_dispnum) or
                     (get_year(crisisrecord) > year) or
                     (get_ccode(crisisrecord, 0) <> ccode1) or
                     (get_ccode(crisisrecord, 1) <> ccode2));
        end;

     function TICBDyadic_dispute_data_obj.get_num_total_crises (ccode1, ccode2 : ccode_range; year : year_range) : integer;
        {counts all crises (including ongoing) between cc1, cc2.  }
        var crisisrecord : longint;
        begin
           check_initialized;
           result := 0;
           crisisrecord := index^[ccode1, ccode2];       {1st crisis-year between cc1, cc2}
           {ICB dyadic data is nondirected, so cc1 < cc2 always}
           {For ICB data, the disputes are sorted by  ccode1 ccode2 YEAR, not cc1 cc2 start year.
            So, I can find the first for cc1 and cc2, then look for year before I care about.}
           if crisisrecord <> null_dispute_number then
              repeat
                {need to check all disputes starting with the 1st until have reached the
                 next year or dyad}
                 if (get_year (crisisrecord) = year) then
                    inc(result);
                 inc(crisisrecord);
              until (
                     (crisisrecord > get_last_dispnum) or
                     (get_year(crisisrecord) > year) or
                     (get_ccode(crisisrecord, 0) <> ccode1) or
                     (get_ccode(crisisrecord, 1) <> ccode2));
        end;

            {   -----------------------  }

      {private functions called with record # }
      function TICBDyadic_dispute_data_obj.get_ccode (record_num : longint;
                 which_state : side_type) : ccode_range;
                         {given number of a dyadic year dispute record, returns
                    the ccode of the specified particpant in the dyadic dispute}
        var temp : ccode_range;
        begin
            check_initialized;
            if record_num = null_dispute_number then {no dispute} result := 0 else
            if which_state = 0 then
               temp := data^[record_num]^.ccodeA
            else if which_state=1 then
               temp := data^[record_num]^.ccodeB;
            get_ccode := temp;
        end;

      function TICBDyadic_dispute_data_obj.get_year (record_num : longint) : year_range;
        begin
           if record_num = null_dispute_number then {no dispute} result := min_year else
              result := data^[record_num]^.year;
        end;

      function TICBDyadic_dispute_data_obj.get_first_year (record_num : longint) : year_range;
        begin
           if record_num = null_dispute_number then {no dispute} result := min_year else
              result := data^[record_num]^.trgyrdy;
        end;

      function TICBDyadic_dispute_data_obj.get_last_year (record_num : longint) : year_range;
        begin
           if record_num = null_dispute_number then {no dispute} result := min_year else
              result := data^[record_num]^.trmyrdy;
        end;

      function TICBDyadic_dispute_data_obj.get_crisis_num (record_num : longint) : longint;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.crisno;
        end;

      function TICBDyadic_dispute_data_obj.get_crisname (record_num : longint) : string;
        begin
           if record_num = null_dispute_number then {no dispute} result := 'No Crisis' else
              result := data^[record_num]^.crisname;
        end;

      function TICBDyadic_dispute_data_obj.get_crdynum (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.crdynum;
        end;

      function TICBDyadic_dispute_data_obj.get_oneside (record_num : longint) : longint;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.oneside;
        end;

      function TICBDyadic_dispute_data_obj.get_durdays (record_num : longint) : longint;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.durdays;
        end;

      function TICBDyadic_dispute_data_obj.get_duryear (record_num : longint) : longint;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.duryear;
        end;

      function TICBDyadic_dispute_data_obj.get_ongoing (record_num : longint) : longint;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.ongoing;
        end;

      function TICBDyadic_dispute_data_obj.get_actnuma (record_num : longint) : longint;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.actnuma;
        end;

      function TICBDyadic_dispute_data_obj.get_actnumb (record_num : longint) : longint;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.actnumb;
        end;

      function TICBDyadic_dispute_data_obj.get_cowmema (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.cowmema;
        end;

      function TICBDyadic_dispute_data_obj.get_cowmemb (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.cowmemb;
        end;

      function TICBDyadic_dispute_data_obj.get_gwmema (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.gwmema;
        end;

      function TICBDyadic_dispute_data_obj.get_gwmemb (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.gwmemb;
        end;

      function TICBDyadic_dispute_data_obj.get_iwca (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.iwca;
        end;

      function TICBDyadic_dispute_data_obj.get_iwcb (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.iwcb;
        end;

      function TICBDyadic_dispute_data_obj.get_yrtriga (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.yrtriga;
        end;

      function TICBDyadic_dispute_data_obj.get_yrterma (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.yrterma;
        end;

      function TICBDyadic_dispute_data_obj.get_yrtrigb (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.yrtrigb;
        end;

      function TICBDyadic_dispute_data_obj.get_yrtermb (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.yrtermb;
        end;

      function TICBDyadic_dispute_data_obj.get_trgyrdy (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.trgyrdy;
        end;

      function TICBDyadic_dispute_data_obj.get_trmyrdy (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.trmyrdy;
        end;

      function TICBDyadic_dispute_data_obj.get_motriga (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.motriga;
        end;

      function TICBDyadic_dispute_data_obj.get_moterma (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.moterma;
        end;

      function TICBDyadic_dispute_data_obj.get_motrigb (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.motrigb;
        end;

      function TICBDyadic_dispute_data_obj.get_motermb (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.motermb;
        end;

      function TICBDyadic_dispute_data_obj.get_trgmody (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.trgmody;
        end;

      function TICBDyadic_dispute_data_obj.get_trmmody (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.trmmody;
        end;

      function TICBDyadic_dispute_data_obj.get_datriga (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.datriga;
        end;

      function TICBDyadic_dispute_data_obj.get_daterma (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.daterma;
        end;

      function TICBDyadic_dispute_data_obj.get_datrigb (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.datrigb;
        end;

      function TICBDyadic_dispute_data_obj.get_datermb (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.datermb;
        end;

      function TICBDyadic_dispute_data_obj.get_trgdady (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.trgdady;
        end;

      function TICBDyadic_dispute_data_obj.get_trmdady (record_num : longint) : integer;
        begin
           if record_num = null_dispute_number then {no dispute} result := 0 else
              result := data^[record_num]^.trmdady;
        end;

      function TICBDyadic_dispute_data_obj.get_originatora (record_num : longint) : boolean;
        begin
           if record_num = null_dispute_number then {no dispute} result := false else
              result := data^[record_num]^.originatora;
        end;

      function TICBDyadic_dispute_data_obj.get_originatorb (record_num : longint) : boolean;
        begin
           if record_num = null_dispute_number then {no dispute} result := false else
              result := data^[record_num]^.originatorb;
        end;


      function TICBDyadic_dispute_data_obj.Is_originator_in_crisis (accode : ccode_range; record_num : longint) : boolean;  {compute this}
         {true if accode is an originator in the crisis of which this dyad is a part}
        begin
           check_initialized;
           result := false;
           if record_num = null_dispute_number then {no dispute} result := false else
              begin
                 if accode = get_ccode(record_num, 0) then
                    result := get_originatora (record_num)
                 else
                 if accode = get_ccode(record_num, 1) then
                    result := get_originatorb (record_num)
                 else
                    EUGeneError ('In ICB "Is_originator" function, but a ccode does not match either in dyad.  Notify programmer.  Continuing, but output likely wrong.',1,continue,error_log);
              end;
        end;

        {  ------------------------------------------------  }

     Function TCOWdyadic_dispute_obj_mem_overhead : longint;
        begin
           result :=
            sizeof (dyad_array) + max_dyadic_disputes*sizeof(dyad_dispute_main_rec) +
            sizeof(index_array_type)  + sizeof(longint)  + sizeof(boolean) +
            2*sizeof(year_range) + sizeof( TCountry_dispute_data_obj ) +  sizeof(overall_dispute_index_type) +
            sizeof(country_disparray) + max_country_disputes * sizeof(country_dispute_rec) +
            sizeof(country_dispute_range) + sizeof(boolean) + sizeof( TParticipant_incident_data_obj ) ;
        end;

     Function TMaoz_dyadic_dispute_obj_mem_overhead : longint;
        begin
           result :=
            sizeof (TMaoz_Dyadic_dispute_data_obj) + sizeof(index_array_type) +
            max_dyadic_dispute_years * sizeof(maoz_dyadic_dispute_main_rec);
        end;

     Function TICB_dyadic_dispute_obj_mem_overhead : longint;
        begin
           result :=
            sizeof (TICBDyadic_dispute_data_obj) + sizeof(index_array_type) +
            max_dyadic_dispute_years * sizeof(ICB_dyadic_dispute_main_rec);
        end;

        {  ------------------------------------------------  }

     constructor TWernerPeaceYears_obj.init (infile_name : TFileName);
        var x, y : integer;
            peaceyr_value_read : real;
            ccode1, ccode2 : ccode_range;
            year : year_range;
            werner_trace : TTrace_obj;
            infile : text;
            start_mem : longint;
            current_rec : integer;
        begin

         werner_trace := nil;
         try
            try
               start_mem := memavail;
               trace.enter('Initializing werner peace years data ');
               werner_trace := TTrace_obj.init(trace.get_trace_level);
               trace.message ('Initializing Werner Peace Years data structure');

               {initialize data structure}
               for x := min_ccode to max_ccode do
                  for y := min_ccode to max_ccode do
                     data[x,y] := missing_value;

               assign (infile, infile_name);
               reset (infile);
               current_rec := 0;

               {First line is labels}
               readln(infile);
               
               while (not (eof (infile)))  do
                  begin
                     werner_trace.tick ('Executing Procedure: Read Werner Peace years',300);
                     year := read_csv_int (infile);
                     ccode1 := read_csv_int (infile);
                     ccode2 := read_csv_int (infile);
                     peaceyr_value_read := read_csv_real (infile);

                     {Just successfully read a record;  save the info.  }
                     data[min(ccode1, ccode2), max(ccode1, ccode2)] := peaceyr_value_read;
                     inc(current_rec);

                     if not (eof (infile)) then
                        readln (infile);      {go to begin of next record}
                  end;             {while not eof (infile);}

                  werner_trace.tickdone;
                  created := true;

               finally
                  close (infile);
                  werner_trace.free;
                  if debug[4] then
                     trace.message (' required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
                  trace.exit('Finished initializing werner peaceyears data.  '+inttostr(current_rec)+' records read.');
               end;
            except
               on EUserInterrupt do raise;
               on EInOutError do
                 begin
                    FileErrorBox.maindo ('Error opening file "'+infile_name+ '".','File could not be opened for input.  ','File may be in use by another program, or may be missing.');
                    FileErrorBox.showmodal;
                    raise;
                 end;
            end;
        end;

     destructor TWernerPeaceYears_obj.destroy;
        begin
           inherited destroy;
        end;

     function TWernerPeaceYears_obj.get_werner_peaceyears (ccode1, ccode2 : ccode_range) : integer;
        {gets werner's peace years variable.  Rounds because EUGene computes integer peace years counts.}
        begin
           {data is nondirected, so ensure we look up just nondirected}
           if not initialized then
              begin
                 EUGeneError ('Called get_werner_peaceyears before data structure initialized.  Programming error - notify programmer.  Program continues, but returned peaceyears value set to 0.',1,continue,error_log);
                 result := 0;
              end
           else
              begin
                 if data[min(ccode1, ccode2), max(ccode1, ccode2)] = missing_value then
                    result := 0
                 else result := round(data[min(ccode1, ccode2), max(ccode1, ccode2)]);
              end;
        end;

        function TWernerPeaceYears_obj.get_werner_peacedays (ccode1, ccode2 : ccode_range) : integer;
        {gets werner's peace years variable.  multiply by number of days in the years and then round and return}
        begin
           {data is nondirected, so ensure we look up just nondirected}
           if not initialized then
              begin
                 EUGeneError ('Called get_werner_peaceyears before data structure initialized.  Programming error - notify programmer.  Program continues, but returned peaceyears value set to 0.',1,continue,error_log);
                 result := 0;
              end
           else
              begin
                 if data[min(ccode1, ccode2), max(ccode1, ccode2)] = missing_value then
                    result := 0
                 else result := round(data[min(ccode1, ccode2), max(ccode1, ccode2)]*365);
              end;
        end;

     function TWernerPeaceYears_obj.initialized : boolean;
        begin
           result := false;
           if created=true then result := true;
        end;

     function Twerner_PeaceYears_obj_mem_overhead : longint;
        begin
           result := sizeof(TWernerPeaceYears_obj);
        end;

        {  ------------------------------------------------  }


end.   {unit}
