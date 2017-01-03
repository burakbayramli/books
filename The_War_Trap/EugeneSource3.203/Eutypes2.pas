unit eutypes2;

{EUGene  Copyright 1997-2003+  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

 {2nd unit of variable, type, object declarations.  }

interface

uses windows, dialogs, forms, math, eutypes1, TraceUnit, ProgressInterruptWind, cmnprocD,
     sysUtils, FileError, OutWindow, errbx;


   function is_politically_relevant_between (ccode1, ccode2 : ccode_range;
            year1, year2 : year_range) : boolean;

   function is_politically_relevant (ccode1, ccode2 : ccode_range;
            year : year_range) : boolean;

   Function TRisk_Attitude_array_obj_mem_overhead : longint;
   Function TRisk_Attitude_array_obj_mem_per_year : longint;

   Function TSec_array_obj_mem_overhead : longint;
   Function TSec_array_obj_mem_per_year : longint;

   Function TEUWarReason_array_obj_mem_overhead : longint;
   Function TEUWarReason_array_obj_mem_per_year : longint;

type

   {contiguity }
   direct_colonial_range = (direct, colonial);
   contigu_rec_ptr = ^contigu_rec;
   contigu_rec = record
        contiguity_type : contiguity_range;
        direct_colonial_type : direct_colonial_range;
        colony_code : array[1..2] of entity_range;
        start_year, end_year : year_range;
        next_rec : contigu_rec_ptr;
      end;
   contig2_ptr = ^contig2_array_type;
   contig2_array_type = array[ccode_index_range] of contigu_rec_ptr;
   contig_array_ptr = ^contig_array;
   contig_array = array[ccode_index_range] of contig2_ptr;

   Tcontiguity_array_obj = class (Tobject)
      constructor init (afilename : TFileName);
      function is_directly_contiguous (ccode1, ccode2 : ccode_range; year :year_range;
               level_for_contiguity : contiguity_range) : boolean;
      function get_direct_contiguity_level (ccode1, ccode2 : ccode_range; year : year_range) : contiguity_range;
      function get_colonial_contiguity_level (ccode1, ccode2 : ccode_range; year : year_range; var colony1, colony2 : entity_range) : contiguity_range;
      function is_directly_contiguous_ever (ccode1, ccode2 : ccode_range; level_for_contiguity : contiguity_range) : boolean;
      procedure check_direct_contiguity (ccode1, ccode2 : ccode_range;
             year1, year2 : year_range; var continuous, partial, never : boolean;
             level_for_contiguity : contiguity_range);
      destructor destroy; override;
   private
      dyadic_data : contig_array_ptr;
      created : boolean;
   end;


   {object for distances in miles between states, can change over time.}
   location_rec_ptr = ^location_rec;
   location_rec = record
      ccode : ccode_range;
      abbrev : string[3];
      cityname : string[longnamestringlength];
      capitol : boolean;
      {countryname : string[longnamestringlength];}
      start_year, end_year : year_range;
      lat_degrees : 0..90;
      long_degrees : 0..180;
      lat_minutes, long_minutes : 0..60;
      lat_n_s : (north, south);
      long_e_w : (east, west);
      next_rec : location_rec_ptr;
   end;
   location_array = array[ccode_index_range] of location_rec_ptr;
   location_array_ptr = ^location_array;

   distance_rec_ptr = ^distance_rec;
   distance_rec = record
         distance : integer;  {distance in miles}
         start_year, end_year : year_range;
         next_rec : distance_rec_ptr;
      end;
   {Note:  structure too large if just do the following, which would be simple.
      dist_array = array[ccode_index_range, ccode_index_range] of distance_rec_ptr;  }
   dist2_ptr = ^dist2_array_type;
   dist2_array_type = array[ccode_index_range] of distance_rec_ptr;
   dist_array_ptr = ^dist_array;
   dist_array = array[ccode_index_range] of dist2_ptr;
   {will need to access as distance.data^[ccode1]^[ccode2]^.start_year, etc.}
   {size of dynamic needed is (max_ccode_index-min_ccode_index+1)*same*sizeof(distance_rec); }
   Tdistance_array_obj = class (Tobject)
      constructor init (a_file_name : TFileName; user_selections : user_selection_type;
             contiguity_data : Tcontiguity_array_obj);    {reads in from raw data}
                                                         {and figures dyadic dists}
      destructor destroy; override;
      function initialized : boolean;
      function get_distance (ccode1, ccode2 : ccode_range;  ayear : year_range) : integer; overload;
      function get_distance (ccode1, ccode2 : ccode_range;  ayear : year_range; error_check:boolean) : integer; overload;
     private
      locations : location_array_ptr;
      dyadic_data : dist_array_ptr;
      created : boolean;
      function all_in_range (ccode1, ccode2 : ccode_range;
            ayear : year_range; error_check : boolean) : boolean;
   end;   {distance object}


   dist_year_array_type = array[ccode_index_range, ccode_index_range] of integer;
   dist_year_ptr = ^dist_year_array_type;
   Tmindist_array_obj = class (Tobject)
      constructor init (year1, year2 : year_range; a_file_name : TFileName);
                {reads in from raw Gleditsch data}
      destructor destroy; override;
      function initialized : boolean;
      function get_distance (ccode1, ccode2 : ccode_range;  ayear : year_range) : integer; overload;
      function get_distance (ccode1, ccode2 : ccode_range;  ayear : year_range; error_check:boolean) : integer; overload;
      function get_first_partition_year : year_range;
      function get_last_partition_year : year_range;
     private
      year_array : array[min_year..max_year] of dist_year_ptr;
      first_partition_year, last_partition_year : year_range;
      created : boolean;
      function all_in_range (ccode1, ccode2 : ccode_range;
            ayear : year_range; error_check : boolean) : boolean;
   end;   {distance object}


   {The "wanted_list" variable/list will have a starter list of dyads for EU calculation
    and output.  This list will contain just a set of dyads.  Each year will have to be
    further checked for appropriateness.  But even this list will save processing time.}
   dyad_rec_ptr = ^dyad_rec;
   dyad_rec = record
         ccode1, ccode2 : ccode_range;
         next_rec : dyad_rec_ptr;
      end;
   Twanted_dyad_list_obj = class (Tobject)
      constructor init;
      destructor destroy; override;
      procedure update (user_selections: user_selection_type);
      function initialized : boolean;
      procedure get_first_dyad (var ccode1, ccode2 : ccode_range; var complete: boolean);
      procedure get_next_dyad (var ccode1, ccode2 : ccode_range; var complete: boolean);
      function get_num_dyad_years : longint;
      function get_num_dyads : longint;
      {dynamic size needed is sizeof(dyad_rec); }
     private
      data : dyad_rec_ptr;
      current : dyad_rec_ptr;
      created : boolean;
      num_dyads, num_dyad_years : longint;
    end;   {object twanted}


     {Object for risk attitude.  }
   risk_rec_v2 = record
         risk, security, secmaxsum, secminsum : single;
         {best_alliance, worst_alliance : stored_alliance_type;} {don't do - too big}
      end;
   risk_record_type_v2 = array[region_range] of risk_rec_v2;   {risk based on regional groups of states}
       {for risk, 2 similar types.  Flat array is for outputting data to file.
        Array of Pointer type is for keeping data in memory on a Win16 system (flat too big)}
   risk_ccode_array_type_v2 = array[ccode_index_range] of risk_record_type_v2;
   risk_record_ptr_v2 = ^risk_record_type_v2;
   risk_ccode_array_ptr_type_v2 = array[ccode_index_range] of risk_record_ptr_v2;
   risk_ccode_array_v2 = risk_ccode_array_ptr_type_v2;
   risk_ccode_array_ptr_v2 = ^risk_ccode_array_v2;
   risk_year_array_v2 = array[year_range] of risk_ccode_array_ptr_v2;
   risk_year_array_ptr_v2 = ^risk_year_array_v2;
   Trisk_attitude_array_obj_v2 = class (Tobject)
      constructor init (eugene_risk_file_name, WTR_risk_file_name : TFileName;
                        year1, year2 : year_range; raw_data_source : risk_in_data_type);
         {init reads in from intermediate, external file.}
      destructor destroy; override;
      function get_first_partition_year : year_range;
      function get_last_partition_year : year_range;
      function initialized : boolean;
      function get_risk (ccode : ccode_range; year : year_range; aregion : full_region_range) : single; overload;
      function get_risk (ccode : ccode_range; year : year_range; aregion : full_region_range; error_check : boolean) : single; overload;
      function get_security (ccode : ccode_range; year : year_range; aregion : full_region_range; error_check : boolean) : single;
      function get_secmax (ccode : ccode_range; year : year_range; aregion : full_region_range; error_check : boolean) : single;
      function get_secmin (ccode : ccode_range; year : year_range; aregion : full_region_range; error_check : boolean) : single;
      function get_uncertainty (year : year_range; aregion : full_region_range; error_check : boolean) : single;
     private
      data : risk_year_array_ptr_v2;
      first_partition_year, last_partition_year : year_range;
      created : boolean;
      function all_in_range (ccode : ccode_range; ayear : year_range; aregion : region_type; error_check: boolean): boolean;
   end;    {risk object}

   risk_file_record_type_v2 = packed record
          year : year_range;
          ccode_from_index_list : ccode_index_array;  {to be able to verify ccode_indexes are the same}
          ccode_array : risk_ccode_array_type_v2;
       end;
   risk_file_type_v2 = file of risk_file_record_type_v2;

   Trisk_attitude_array_obj = Trisk_attitude_array_obj_v2;




   stored_alliance_list_type = array[ccode_index_range] of alliance_value_type;
   stored_alliance_info_rec = record
         best_alliance, worst_alliance : stored_alliance_list_type;
      end;
   one_country_year_security_alliance_data = array[region_range] of stored_alliance_info_rec;

   sec_alliance_one_country_year_ptr = ^one_country_year_security_alliance_data;
   sec_alliance_ccode_array = array[ccode_index_range] of sec_alliance_one_country_year_ptr;
   sec_alliance_ccode_array_ptr = ^sec_alliance_ccode_array;
   sec_alliance_year_array = array[year_range] of sec_alliance_ccode_array_ptr;
   Trisk_stored_security_alliance_obj = class (Tobject)
      constructor init (security_alliance_file_name : TFileName;
                        year1, year2 : year_range);
      destructor destroy; override;
      procedure write_new_data (security_alliance_file_name : TFileName;
                        year1, year2 : year_range);
      procedure set_new_best_alliance_data
         (ccode1, ccode2: ccode_range; year : year_range; region : region_range; new_alliance_value : alliance_value_type);
      procedure set_new_worst_alliance_data
         (ccode1, ccode2: ccode_range; year : year_range; region : region_range; new_alliance_value : alliance_value_type);
      function get_first_partition_year : year_range;
      function get_last_partition_year : year_range;
      function initialized : boolean;
      function get_best_alliance (ccode1, ccode2: ccode_range; year : year_range; region : full_region_range) : alliance_value_type;
      function get_worst_alliance (ccode1, ccode2: ccode_range; year : year_range; region : full_region_range) : alliance_value_type;
      function get_best_alliance_list (ccode1: ccode_range; year : year_range; region : full_region_range) : stored_alliance_list_type;
      function get_worst_alliance_list (ccode1: ccode_range; year : year_range; region : full_region_range) : stored_alliance_list_type;
     private
      data : sec_alliance_year_array;
      first_partition_year, last_partition_year : year_range;
      created : boolean;
      function all_in_range (ccode1 : ccode_range; ayear : year_range): boolean;
   end;    {stored security object}


   {type to store security alliances in file}
   one_year_security_alliance_data = array[ccode_index_range] of one_country_year_security_alliance_data;
      {this is a version without pointers for file storage}
   security_alliance_file_record_type = packed record
          year : year_range;
          ccode_from_index_list : ccode_index_array;  {to be able to verify ccode_indexes are the same}
          ccode_array : one_year_security_alliance_data;
       end;
   security_alliance_file_type = file of security_alliance_file_record_type;


    {these types are for the internal game tree for solving equilibria}
   nodetype = 1..12;
   UtilityArrayType = array[1..2] of array[IIG_outcomes] of double;

      {object for EUs using War and Reason methods for dyads}
   EUWarReason_record = record
         UtilityAA, UtilityAB, UtilityASQ, ProbWinAB, UtilityBB, UtilityBA,
         UtilityBSQ, ProbWinBA : single;
      end;
   EUWarReason_ccode_array_2 = array[ccode_index_range] of EUWarReason_record;
   EUWarReason_ccode_array_ptr2 = ^EUWarReason_ccode_array_2;
   EUWarReason_ccode_array_type = array[ccode_index_range] of EUWarReason_ccode_array_ptr2;
   EUWarReason_ccode_ptr = ^EUWarReason_ccode_array_type;
   TEUWarReason_array_obj = class (Tobject)
      constructor init (a_file_name : TFileName; year1, year2 : year_range);
         {init reads in from intermediate, external file.}
      destructor destroy; override;
      function get_first_partition_year : year_range;
      function get_last_partition_year : year_range;
      function initialized : boolean;
      function get_UtilityAA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UtilityAB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UtilityASQ (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UtilityBB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UtilityBA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UtilityBSQ (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_ProbWinAB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_ProbWinBA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;

        {The following are calculated from the above;  only the above are actually saved.}
      function get_StakesA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_StakesB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UiSQ (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UiAcqi (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UiAcqj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UiNego (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UiCapi (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UiCapj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UiWari (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UiWarj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UjSQ (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UjAcqi (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UjAcqj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UjNego (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UjCapi (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UjCapj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UjWari (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;
      function get_UjWarj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single; overload;

         {The following are equilibrium outcomes that are computed from the above}
      function get_EQ (EQ: IIG_Outcomes; ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : dummy_type;  overload;
      function get_EQSQ (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : dummy_type;  overload;
      function get_EQNego (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : dummy_type; overload;
      function get_EQAcqA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : dummy_type; overload;
      function get_EQAcqB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : dummy_type; overload;
      function get_EQCapA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : dummy_type; overload;
      function get_EQCapB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : dummy_type; overload;
      function get_EQWarA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : dummy_type; overload;
      function get_EQWarB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : dummy_type; overload;

      function get_UtilityAA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UtilityAB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UtilityASQ (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UtilityBB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UtilityBA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UtilityBSQ (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_ProbWinAB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_ProbWinBA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;

        {The following are calculated from the above;  only the above are actually saved.}
      function get_StakesA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_StakesB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UiSQ (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UiAcqi (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UiAcqj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UiNego (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UiCapi (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UiCapj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UiWari (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UiWarj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UjSQ (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UjAcqi (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UjAcqj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UjNego (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UjCapi (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UjCapj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UjWari (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;
      function get_UjWarj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): single; overload;

         {The following are equilibrium outcomes that are computed from the above}
      function get_EQ (EQ: IIG_Outcomes; ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean) : dummy_type;  overload;
      function get_EQSQ (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean) : dummy_type; overload;
      function get_EQNego (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): dummy_type; overload;
      function get_EQAcqA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): dummy_type; overload;
      function get_EQAcqB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): dummy_type; overload;
      function get_EQCapA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): dummy_type; overload;
      function get_EQCapB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): dummy_type; overload;
      function get_EQWarA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): dummy_type; overload;
      function get_EQWarB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean): dummy_type; overload;

     private
      year_array : array[min_year..max_year] of EUWarReason_ccode_ptr;
      first_partition_year, last_partition_year : year_range;
      created : boolean;
      SolvingUtilityArray : UtilityArrayType;    {this is for internal game equil. solutions}
      function all_in_range (ccode1, ccode2 : ccode_range; ayear : year_range; error_check : boolean): boolean;
      function all_valid (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type): boolean;
      procedure setUtilityArray (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type);
      function solveOriginal (anode : nodetype; U : UtilityArrayType) : IIG_outcomes;
      function solveNoWar (anode : nodetype; U : UtilityArrayType) : IIG_outcomes;
      function solveNoForce (anode : nodetype; U : UtilityArrayType) : IIG_outcomes;
   end;    {EUWarReason object}

    EUWarReason_file_record_type = packed record
          ccode1, ccode2 : ccode_range;
          year : year_range;
          EUWarReason_rec : EUWarReason_record;
       end;
    EUWarReason_file_type = file of EUWarReason_file_record_type;

      { -----------------------------------------------------------------------   }

   var
      contiguity_data : Tcontiguity_array_obj;

      { -----------------------------------------------------------------------   }

   implementation

   uses euinoutd, mdiframe;
   {Mdiframe necessary for reference to "Frame".  There's probably a better way.}

  { ---------------------------------------------------------------  }

   function booltodummy (x : boolean): dummy_type;
     begin
        case x of
          true : booltodummy := 1;
          false : booltodummy := 0;
        end;   {case}
     end;

  { ---------------------------------------------------------------  }
   function is_politically_relevant_between (ccode1, ccode2 : ccode_range;
            year1, year2 : year_range) : boolean;
      begin
        is_politically_relevant_between := false;
        if nation_list.is_a_state_between (ccode1, year1, year2) and
           nation_list.is_a_state_between (ccode2, year1, year2) and
           (nation_list.is_a_gp_between (ccode1, year1, year2) or
            nation_list.is_a_gp_between (ccode2, year1, year2) or
            contiguity_data.is_directly_contiguous_ever (ccode1, ccode2, user_selections.contiguity_level_required)) then
        is_politically_relevant_between := true;
      end;

   function is_politically_relevant (ccode1, ccode2 : ccode_range;
            year : year_range) : boolean;
      begin
        is_politically_relevant := false;
        if nation_list.is_a_state (ccode1, year) and
           nation_list.is_a_state (ccode2, year) and
           (nation_list.is_a_gp (ccode1, year) or
            nation_list.is_a_gp (ccode2, year) or
            contiguity_data.is_directly_contiguous (ccode1, ccode2, year, user_selections.contiguity_level_required)) then
        is_politically_relevant := true;
      end;

      {  ---------------------------------------------------------   }

   {Methods for distance object}
   constructor Tdistance_array_obj.init (a_file_name : TFileName;
               user_selections : user_selection_type; contiguity_data : Tcontiguity_array_obj);
                  {reads locations in from raw data}
                  {data file will be in flat form with ccodes and lat, long.  }
      var start_mem, heapneeded : longint;
          dist_file : text;
          ccode1, ccode2 : ccode_range;
          dist2year : dist2_ptr;
          distrec : distance_rec_ptr;
          locarray : location_array_ptr;
          current_rec, a_location_rec : location_rec_ptr;
          rcount : integer;
          x : ccode_index_range;

          year1, year2, temp, dist_year, rec_start_year, rec_end_year,
            overlap_start, overlap_end : year_range;
          current_dyad_dist_rec, dyad_dist_rec_ptr, following_rec : distance_rec_ptr;
          capitol1, capitol2 : location_rec_ptr;
          new_cap, new_cap2 : boolean;
          distmiles : integer;
          cap1name, cap2name : string;
          continuous, partial, never : boolean;   {for contiguity check}
          dist_table : array[year_range] of integer;   {to hold annual sub-interval distances}
          prev_dist : integer;
          city1, city2 : location_rec_ptr;
          ccmarker : ccode_range;
          dyads_done : longint;
          Dist_trace : TTrace_obj;

                   { --------------------  }

      procedure read_one_rec (var infile : text; var input_location_rec : location_rec_ptr);
         var achar : char;
             namecount : integer;
             boolnum : integer;

         begin
            new (input_location_rec);
            with input_location_rec^ do
            begin
               next_rec := nil;

                {vars to read in order are:  ccode abbrev city Latdegrees LatMinutes ns
                Longdegrees LongMinutes ew YearStrt YearEnd CName}

                {first, ccode}
               read (infile, ccode);

               {Next read abbreviation}
               repeat
                  read(infile, achar);
               until (achar >= 'A') and (achar <= 'z');
               abbrev := '';
               if achar <> chr(9) then abbrev := abbrev+achar;
               read (infile, achar);
               if achar <> chr(9) then abbrev := abbrev+achar;
               read (infile, achar);
               if achar <> chr(9) then abbrev := abbrev+achar;
               if abbrev[3] = chr(9) then abbrev[3] := ' ';

               {Next read cityname.  Read up to a tab.}
               repeat
                  read(infile, achar);
               until (achar >= 'A') and (achar <= 'z');
               cityname := '';
               if achar <> chr(9) then cityname := cityname + achar;
               namecount := 1;
               repeat
                   read(infile, achar);
                   if namecount < longnamestringlength then
                      begin
                        inc(namecount);
                        if achar <> chr(9) then cityname := cityname + achar;
                      end;
               until (achar = chr(9));

               read(infile, boolnum);
               if boolnum=1 then capitol := true else capitol := false;

               {next, LatHours}
               read(infile, Lat_Degrees);

               {next LatMinutes}
               read(infile, Lat_Minutes);

               {next,  ns, but have to do an extra read to skip the tab }
               read(infile, achar);
               if achar=chr(9) then
                  read(infile, achar);
               if (achar='n') or (achar='N') then
                  lat_n_s := north
               else if (achar='s') or (achar='S') then
                  lat_n_s := south
               else begin
                     EUGeneError ('Incorrect n/s Latitude designation seen in input distance file. '+
                            'Notify programmer.  Setting to "n".', 5, continue, error_log);
                     lat_n_s := north
                  end;
               read(infile, achar);
               if achar <> chr(9) then
                  EUGeneError ('Incorrect format after n/s Latitude designation in input distance file. '+
                           'Notify programmer.  Program may fail.', 5, continue, error_log);

               {next LongHours }
               read(infile, Long_Degrees);

               {next LongMinutes }
               read(infile, Long_Minutes);

               {next ew }
               read(infile, achar);
               if achar=chr(9) then
                  read(infile, achar);
               if (achar='e') or (achar='E') then
                  long_e_w := east
               else if (achar='w') or (achar='W') then
                  long_e_w := west
               else begin
                     EUGeneError ('Incorrect e/w Latitude designation seen in input distance file. '+
                            'Notify programmer.  Setting to "e".', 5, continue, error_log);
                     long_e_w := east
                  end;
               read(infile, achar);
               if achar <> chr(9) then
                  EUGeneError ('Incorrect format after e/w Latitude designation in input distance file. '+
                           'Notify programmer.  Program may fail.', 5, continue, error_log);

               {next YearStrt }
               read (infile, start_year);

               {next YearEnd }
               read (infile, end_year);

               {next is country name.  }
               {Don't really need it, so skip it.  }

               {advance to next record in input file}
               readln (infile);

            end;    {with input_rec do ...}
         end;    {procedure read a location rec}

         {   -----------------------   }

      function great_circle_distance (rec1, rec2 : location_rec): integer;
           {great circle distance from 1 to 2, in miles.
            Formula is from Direct Line Distances
            by Gary L. Fitzpatrick and Marilyn J. Modlin, 1986, Metuchen, NJ,
            The Scarecrow Press, Inc. }
         var lat1, lat2, long1, long2, diffLong, CosDist : single;
             dist_degrees : single;
         begin
            {Computes distance between 2 cities given lat, long coordinates in the record}
            {first, convert lat and long to a decimal, and convert n/s e/w to + or - as approp}
            {Note: The formula I have is in degrees.  Delphi computes sin, etc.
             from radians.  So must do conversions in formula.}
            lat1 := rec1.lat_degrees + rec1.lat_minutes/60;
            lat2 := rec2.lat_degrees + rec2.lat_minutes/60;
            long1 := rec1.long_degrees + rec1.long_minutes/60;
            long2 := rec2.long_degrees + rec2.long_minutes/60;
            {for the formula to work, convert longitude values w to be negative.
             Convert latitude values s to be negative.}
            if rec1.lat_n_s = south then lat1 := -1 * abs(lat1);
            if rec2.lat_n_s = south then lat2 := -1 * abs(lat2);
            if rec1.long_e_w = west then long1 := -1 * abs(long1);
            if rec2.long_e_w = west then long2 := -1 * abs(long2);
            DiffLong := long1 - long2;
            CosDist := (   ( Sin(DegrToRad(lat1)) * sin(DegrToRad(lat2)) )
                         + ( cos(DegrToRad(lat1)) * cos(DegrToRad(lat2)) * cos(DegrToRad(DiffLong)) ) );
            dist_degrees := RadToDegr(arccos (CosDist));
            great_circle_distance := round(69.16 * dist_degrees);
         end;

         {   -----------------------   }

   function overlap (city1, city2: location_rec_ptr; var overlap_start, overlap_end :
                     year_range): boolean;
      begin
        {several ways they could overlap.  Key is they do, or Not at all.}
        if (city1^.start_year > city2^.end_year) or (city2^.start_year > city1^.end_year) then
            begin    {no overlap}
              overlap := false;
              overlap_start := min_year;
              overlap_end := min_year;
            end
        else     {here, either city1 encloses city2, or city2 encloses city1}
                 {or, city1 starts before c2 start, but ends while c2 still continues}
                 {or, city2 starts before c1 start, but ends while c1 still continues}
                 {In all of these cases, same formula to figure overlap years can be used.}
            begin
              overlap := true;
              overlap_start := max(city1^.start_year, city2^.start_year);
              overlap_end := min(city1^.end_year, city2^.end_year);
            end;
                 {Unnecessary checks were:  }
            {if ( (city1^.start_year <= city2^.start_year) and (city1^.end_year >= city2^.end_year) ) or
             ( (city2^.start_year <= city1^.start_year) and (city2^.end_year >= city1^.end_year) ) then
            else
            if ( (city1^.start_year <= city2^.start_year) and (city1^.end_year >= city2^.start_year)
               and (city1^.end_year <= city2^.end_year) )  then
            And one had yet to be programmed...}

      end;

               {   -----------------------   }

      begin    {main procedure .init_locations}
         try
            try
               start_mem := memavail;
               trace.enter('Initializing distance data');
               Dist_trace := nil;
               Dist_trace := TTrace_obj.init(trace.get_trace_level);
               if not(ccode_index.initialized) then
                  begin
                     EUGeneError ('Distance array creation called before ccode_index initialized',
                                     5, stop, error_log);
                     trace.message ('Check program coding [notify programmer].  ');
                  end;
               if not(nation_list.initialized) then
                  begin
                     EUGeneError ('Distance array creation called before nation_list initialized',
                                     5, stop, error_log);
                     trace.message ('Check program coding [notify programmer].  ');
                  end;

               heapneeded := Tdistance_array_mem_overhead;
               if MaxAvail <= heapneeded then
                  begin
                     EUGeneError ('Not enough memory for distance array. ',
                                     5, stop, error_log);
                  end;


               new(locarray);
               locations := locarray;
               for x := min_ccode_index to max_ccode_index do
                  locations^[x] := nil;

                  {Now read in input data.}
               trace.message ('Reading locations data file '+ a_file_name);
               try
                  assignFile (dist_file, a_file_name);
                  reset (dist_file);
                  {skip first label line}
                  readln(dist_file);
                  rcount := 0;
                  while not eof(dist_file) do
                     begin
                        Dist_trace.tick ('Executing Procedure: Read Locations Data',0);
                          {this creates and returns a new record}
                        read_one_rec (dist_file, a_location_rec);
                        rcount := rcount + 1;

                         {This could be a multiple record for a ccode, so check that. }
                        if locations^[ccode_index.index(a_location_rec^.ccode)] = nil then
                           {first record for this ccode}
                           locations^[ccode_index.index(a_location_rec^.ccode)] := a_location_rec
                        else
                           begin
                              current_rec := locations^[ccode_index.index(a_location_rec^.ccode)];
                              while current_rec^.next_rec <> nil do
                                 current_rec := current_rec^.next_rec;
                              current_rec^.next_rec := a_location_rec;
                           end;
                     end;
               finally
                  CloseFile (dist_file);
               end;
               Dist_Trace.tickdone;

                  {now initialize complete array}
               trace.message('      Creating initial distance structure');
               heapneeded := Tdistance_array_mem_overhead;
               {actual structure may be larger than this, because some pairs will have multiple
                distances, because of changes over time.  So check for more than the necessary space.}
               if MaxAvail <= (1.5 * heapneeded) then
                  begin
                     EUGeneError ('Not enough memory for distance array (x1.5). ',
                                     5, stop, error_log);
                  end;

               new (dyadic_data);
               dyads_done := 0;
               for ccode1 := min_ccode to max_ccode do
                  begin
                     if nation_list.have_info(ccode1) then
                     begin
                        new (dist2year);
                        dyadic_data^[ccode_index.index(ccode1)] := dist2year;
                        for ccode2 := min_ccode to max_ccode do
                           if nation_list.have_info(ccode2) then
                              begin
                                 new (distrec);
                                 dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)] := distrec;
                                 dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)]^.distance := initialized_value;
                                 dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)]^.start_year := min_year;
                                 dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)]^.end_year := max_year;
                                 dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)]^.next_rec := nil;
                                 {for state with self, set to 0.}
                                 if (ccode1=ccode2) then
                                    dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)]^.distance := 0;
                                 inc(dyads_done);
                                 Dist_trace.tick('Executing Procedure: Initialize Distance Arrays', nation_list.get_ndyads);
                              end
                           else dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)] := nil;
                     end
                     else dyadic_data^[ccode_index.index(ccode1)] := nil;
                  end;
               Dist_trace.tickdone;

                  {Now convert the individual countries to dyadic distance records.}
               dyads_done := 0;
               ccmarker := min_ccode;
               for ccode1 := min_ccode to max_ccode do
                 if nation_list.have_info(ccode1) then
                   for ccode2 := min_ccode to max_ccode do
                     if nation_list.have_info(ccode2) then
                       begin    {need to convert and set up info for this dyad}
                          {if ccmarker <> ccode1 then
                             begin
                                ccmarker := ccode1;
                                trace.message (inttostr(ccode1));
                             end;   }
                          {trace.message (inttostr(ccode1)+' '+inttostr(ccode2));}
                         inc(dyads_done);
                         Dist_trace.tick('Executing Procedure: Compute Distances', nation_list.get_ndyads);
                              {check if same state}
                         if (ccode1=ccode2) then
                            begin
                               dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)]^.distance := 0;
                               dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)]^.start_year := min_year;
                               dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)]^.end_year := max_year;
                               dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)]^.next_rec := nil;
                            end
                         else {different states}
                         if (locations^[ccode_index.index(ccode1)]=nil) or (locations^[ccode_index.index(ccode2)]=nil) then
                            begin
                               current_dyad_dist_rec := dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)];
                               if current_dyad_dist_rec^.distance <> initialized_value then
                               {there are already 1+ records for distance, find the
                               last record, and add a new record}
                               begin
                                 while current_dyad_dist_rec^.next_rec <> nil do
                                   current_dyad_dist_rec := current_dyad_dist_rec^.next_rec;
                                 new (dyad_dist_rec_ptr);
                                 current_dyad_dist_rec^.next_rec := dyad_dist_rec_ptr;
                                 current_dyad_dist_rec := dyad_dist_rec_ptr;
                                 current_dyad_dist_rec^.next_rec := nil;
                               end;
                                {Now can set values for distance and years for this record,
                                 which is now either the only record, or the
                                 new record if one was added}
                               current_dyad_dist_rec^.start_year := min_year;
                               current_dyad_dist_rec^.end_year := max_year;
                               current_dyad_dist_rec^.distance := missing_value;
                            end
                         else   {OK data, and different states - compute distance.}
                            begin
                               {first, figure capitols distance}
                               {find capitol record for each.  This assumes the data has
                                only one city coded as capitol for each time period.}
                               {if nothing coded as capitol, takes the last city}
                              current_dyad_dist_rec := dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)];
                              {want to check all capitol(a) vs. all capitol(b) pairs}
                              {find first capitol of a}
                              capitol1 := locations^[ccode_index.index(ccode1)];
                              while (not (capitol1^.capitol=true)) and (capitol1^.next_rec <> nil) do
                                          capitol1 := capitol1^.next_rec;
                                   {now have 1st a capitol, or last of this location}
                              repeat
                                 {set name so can check whether next city is same/different}
                                 cap1name := capitol1^.cityname;
                                  {Now, find first b capitol}
                                 capitol2 := locations^[ccode_index.index(ccode2)];
                                 while (not (capitol2^.capitol=true)) and (capitol2^.next_rec <> nil) do
                                           capitol2 := capitol2^.next_rec;
                                 repeat
                                     {now, check this capitol(a) to capitol(b) pair}
                                    {First, is there a time overlap between this pair of capitols?
                                     If so, then compute distance and set time period.  If not,
                                     skip it.}
                                    cap2name := capitol2^.cityname;
                                    if (  (capitol1^.start_year >= capitol2^.start_year) and
                                          (capitol1^.start_year <= capitol2^.end_year)) or
                                       (  (capitol1^.end_year >= capitol2^.start_year) and
                                          (capitol1^.end_year <= capitol2^.end_year)) or
                                       (  (capitol2^.start_year >= capitol1^.start_year) and
                                          (capitol2^.start_year <= capitol1^.end_year)) or
                                       (  (capitol2^.end_year >= capitol1^.start_year) and
                                          (capitol2^.end_year <= capitol1^.end_year)) then
                                          {they overlap}
                                      begin
                                        distmiles := great_circle_distance (capitol1^, capitol2^);
                                         {before setting this distance, need to determine whether to set
                                          it in an existing initialized record, or to attach a new
                                          record in sequence.}
                                        if current_dyad_dist_rec^.distance <> initialized_value then
                                       {there are already 1+ records for distance, find the
                                        last record, and add a new record}
                                        begin
                                          while current_dyad_dist_rec^.next_rec <> nil do
                                            current_dyad_dist_rec := current_dyad_dist_rec^.next_rec;
                                          new (dyad_dist_rec_ptr);
                                          current_dyad_dist_rec^.next_rec := dyad_dist_rec_ptr;
                                          current_dyad_dist_rec := dyad_dist_rec_ptr;
                                          current_dyad_dist_rec^.next_rec := nil;
                                        end;
                                         {Now can set values for distance and years for this record,
                                          which is now either the only record, or the
                                          new record if one was added}
                                        current_dyad_dist_rec^.start_year :=
                                          max(capitol1^.start_year, capitol2^.start_year);
                                        current_dyad_dist_rec^.end_year :=
                                          min(capitol1^.end_year, capitol2^.end_year);
                                        current_dyad_dist_rec^.distance := distmiles;
                                      end    {they overlapped}
                                    else
                                      begin
                                          {no overlap, so do not process the pair}
                                      end;

                                     {now find next capitol record for b, if there is one.}
                                    if capitol2^.next_rec <> nil then capitol2 := capitol2^.next_rec;
                                    while (not (capitol2^.capitol=true)) and (capitol2^.next_rec <> nil) do
                                          capitol2 := capitol2^.next_rec;
                                     {if this record now points at a new capitol, mark it and do
                                      the main loop again for distance calc.}
                                    if ((capitol2^.cityname <> cap2name) and (capitol2^.capitol = true)) then
                                       new_cap2 := true else new_cap2 := false;
                                       {repeat until this was the last capitol of b}
                                 until (capitol2^.next_rec = nil) and (new_cap2 = false);
                                 {Now finished this pair.  So,
                                  find next capitol record for A, if there is one.}
                                 if capitol1^.next_rec <> nil then capitol1 := capitol1^.next_rec;
                                 while (not (capitol1^.capitol=true)) and (capitol1^.next_rec <> nil) do
                                          capitol1 := capitol1^.next_rec;
                                 {if this record now points at a new capitol, mark it and do
                                  the main loop again for distance calc.}
                                 if ((capitol1^.cityname <> cap1name) and (capitol1^.capitol = true)) then
                                    new_cap := true else new_cap := false;
                                       {repeat until this was the last capitol of a}
                              until (capitol1^.next_rec = nil) and (new_cap = false);



                              {Now have computed distance capitols to capitols.  Now may
                               want to compute other elements - contiguity, and war trap
                               exceptions or cases where state has multiple cities.}
                              if (user_selections.distance_method = capitols_contiguity) or
                                 (user_selections.distance_method = capitols_contiguity_war_trap) then
                                begin
                                   current_dyad_dist_rec := dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)];
                                   repeat
                                      following_rec := current_dyad_dist_rec^.next_rec;

                                      {trace.message ('Checking contiguity of '+inttostr(ccode1)+' '+inttostr(ccode2));}
                                         {check the contiguity of this dyad}
                                      contiguity_data.check_direct_contiguity (ccode1, ccode2,
                                         current_dyad_dist_rec^.start_year, current_dyad_dist_rec^.end_year,
                                         continuous, partial, never, user_selections.contiguity_level_required);
                                      if never then
                                        begin
                                         {not ever contiguous, so leave distance as capitols}
                                        end
                                      else if continuous then
                                        begin
                                           current_dyad_dist_rec^.distance := 0;
                                        end
                                      else if partial then
                                        begin    {here, need to go through each year and make new records as needed.}
                                           {create a table of distances, by year}
                                           rec_start_year := current_dyad_dist_rec^.start_year;
                                           rec_end_year := current_dyad_dist_rec^.end_year;
                                           for dist_year := rec_start_year to rec_end_year do
                                              if contiguity_data.is_directly_contiguous(ccode1, ccode2, dist_year,
                                                 user_selections.contiguity_level_required) then
                                                 dist_table[dist_year] := 0 else
                                                 dist_table[dist_year] := current_dyad_dist_rec^.distance;
                                           {Now, go through this annual table.  When distances
                                            change from 1 year to the next, create a new
                                            record with the new distance.}
                                           prev_dist := dist_table[rec_start_year];
                                           current_dyad_dist_rec^.end_year := rec_start_year;
                                           current_dyad_dist_rec^.distance := dist_table[rec_start_year];
                                           {current.start_year remains the same.}
                                           for dist_year := rec_start_year to rec_end_year do
                                             begin
                                                if dist_table[dist_year] = prev_dist then
                                                  begin
                                                    {when see another record with some distance,
                                                     increment the time period in question}
                                                    current_dyad_dist_rec^.end_year := dist_year;
                                                  end
                                                else   {different distance, need a new record}
                                                  begin
                                                    {create new record, initialize it.}
                                                    new (dyad_dist_rec_ptr);
                                                    dyad_dist_rec_ptr^.start_year := dist_year;
                                                    dyad_dist_rec_ptr^.end_year := dist_year;
                                                    dyad_dist_rec_ptr^.distance := dist_table[dist_year];
                                                    dyad_dist_rec_ptr^.next_rec := nil;
                                                    {Previous record is complete w/o further changes,
                                                     just make it point to new record as "next".}
                                                    current_dyad_dist_rec^.next_rec := dyad_dist_rec_ptr;
                                                    {now advance to this new record as the current base.}
                                                    current_dyad_dist_rec := dyad_dist_rec_ptr;
                                                  end;
                                                prev_dist := dist_table[dist_year];  {reset before moving on}
                                             end;    {for dist_year}
                                           {now set the final record generated to point
                                            at the next dyad in the list.}
                                           current_dyad_dist_rec^.next_rec := following_rec;
                                        end   {if partial}
                                      else
                                        begin
                                           EUGeneError ('Check_contiguity procedure not right - no value returned.  Notify programmer.'+
                                               '  Non-fatal error - program continues',
                                               5, continue, error_log);
                                        end;
                                      {Now, move the current pointer to start again with
                                       the next record.}
                                      current_dyad_dist_rec := following_rec;
                                   until current_dyad_dist_rec = nil;

                                end;      {if user_selections.distance_method = capitols_contiguity }

                              {Now, do war trap exceptions if necessary}
                              {Thes include using multiple cities, not just capitols.
                               That applies for some countries and periods only.}
                              if user_selections.distance_method = capitols_contiguity_war_trap then
                                begin
                                  {check this dyad to see if it has multiple cities}
                                  city1 := locations^[ccode_index.index(ccode1)];
                                  city2 := locations^[ccode_index.index(ccode2)];
                                  if ((city1^.next_rec = nil) and (city2^.next_rec = nil)) then
                                    begin
                                        {there was only one location/city for each state,
                                         so leave it at the capitol set or contiguous distance}
                                    end
                                  else   {multiple cities, so need to use appropriate pair}
                                    begin
                                        {Here, do exactly the same as with contiguity, but
                                         check to see what the shortest distance in each
                                         year is, and change/add recs when necessary.}
                                      current_dyad_dist_rec := dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)];
                                      repeat     {go through each distance record}
                                        {trace.message ('Before '+inttostr(ccode1)+' '+inttostr(ccode2)+' from ' +
                                         inttostr(current_dyad_dist_rec^.start_year)+' to ' +
                                         inttostr(current_dyad_dist_rec^.end_year)+': '+inttostr(current_dyad_dist_rec^.distance));}
                                        following_rec := current_dyad_dist_rec^.next_rec;
                                        if current_dyad_dist_rec^.distance = 0 then
                                          begin
                                             {need to do nothing, already contiguous}
                                          end
                                        else    {multiple cities, not contig, so check city pairs}
                                          begin
                                             {create dist_table for years in this dyadic record}
                                            rec_start_year := current_dyad_dist_rec^.start_year;
                                            rec_end_year := current_dyad_dist_rec^.end_year;
                                            for dist_year := rec_start_year to rec_end_year do
                                                dist_table[dist_year] := current_dyad_dist_rec^.distance;


                                             {for each pair of cities, compute distance.
                                              For overlapping years of this city pair, see if
                                              new distance is < what's in the table already}
                                            city1 := locations^[ccode_index.index(ccode1)];
                                            repeat    {loop through ccode1's cities}
                                              city2 := locations^[ccode_index.index(ccode2)];
                                              repeat
                                                 {check this pair of city1 vs. city2}
                                                 if overlap (city1, city2, overlap_start, overlap_end) then
                                                   begin
                                                     distmiles := great_circle_distance (city1^, city2^);
                                                     for dist_year := overlap_start to overlap_end do
                                                       if (dist_year >= rec_start_year) and
                                                          (dist_year <= rec_end_year) and
                                                          (distmiles < dist_table[dist_year]) then
                                                          dist_table[dist_year] := distmiles;
                                                   end;
                                                 city2 := city2^.next_rec;
                                              until city2 = nil;
                                              city1 := city1^.next_rec;
                                            until city1 = nil;

                                             {Now, have a table with some perhaps modified values.}
                                             {check and create new records as necessary.  This
                                              section is pretty much like the contiguity
                                              sections above.}
                                             {Now, go through this annual table.  When distances
                                              change from 1 year to the next, create a new
                                              record with the new distance.}
                                            prev_dist := dist_table[rec_start_year];
                                            current_dyad_dist_rec^.end_year := rec_start_year;
                                            current_dyad_dist_rec^.distance := dist_table[rec_start_year];
                                            {current.start_year remains the same.}
                                            for dist_year := rec_start_year to rec_end_year do
                                              begin
                                                if dist_table[dist_year] = prev_dist then
                                                  begin
                                                    {when see another record with some distance,
                                                     increment the time period in question}
                                                    current_dyad_dist_rec^.end_year := dist_year;
                                                  end
                                                else   {different distance, need a new record}
                                                  begin
                                                    {create new record, initialize it.}
                                                    new (dyad_dist_rec_ptr);
                                                    dyad_dist_rec_ptr^.start_year := dist_year;
                                                    dyad_dist_rec_ptr^.end_year := dist_year;
                                                    dyad_dist_rec_ptr^.distance := dist_table[dist_year];
                                                    dyad_dist_rec_ptr^.next_rec := nil;
                                                    {Previous record is complete w/o further changes,
                                                     just make it point to new record as "next".}
                                                    current_dyad_dist_rec^.next_rec := dyad_dist_rec_ptr;
                                                    {now advance to this new record as the current base.}
                                                    current_dyad_dist_rec := dyad_dist_rec_ptr;
                                                  end;
                                                prev_dist := dist_table[dist_year];  {reset before moving on}
                                              end;    {for dist_year}
                                            {now set the final record generated to point
                                            at the next dyad in the list.}
                                            {trace.message ('After '+inttostr(ccode1)+' '+inttostr(ccode2)+' from ' +
                                             inttostr(current_dyad_dist_rec^.start_year)+' to ' +
                                             inttostr(current_dyad_dist_rec^.end_year)+': '+
                                             inttostr(current_dyad_dist_rec^.distance));
                                            current_dyad_dist_rec^.next_rec := following_rec;}
                                          end;    {current distance <> 0}

                                          {Now, move the current pointer to start again with
                                           the next record.}
                                        current_dyad_dist_rec := following_rec;
                                      until current_dyad_dist_rec = nil;
                                    end;   {else... multiple cities}

                                  {Now done with checking multiple cities.  That captured most
                                   War Trap exceptions/changes.  But, War Trap has
                                   2 other exceptions, Japan to US (honolulu) and Japan to
                                   USSR (vladivostok)}
                                   if (ccode1=740) and (ccode2=2) then
                                      begin   {Tokyo to honolulu}
                                         city1 := locations^[ccode_index.index(ccode1)];
                                         while (not (city1^.cityname='Tokyo')) do city1 := city1^.next_rec;
                                         city2 := locations^[ccode_index.index(ccode2)];
                                         while (not (city2^.cityname='Honolulu')) do city2 := city2^.next_rec;
                                         distmiles := great_circle_distance (city1^, city2^);
                                         current_dyad_dist_rec :=
                                            dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)];
                                         repeat   {for each time record for this dyad, set to this new distance}
                                            current_dyad_dist_rec^.distance := distmiles;
                                            current_dyad_dist_rec := current_dyad_dist_rec^.next_rec;
                                         until current_dyad_dist_rec = nil;
                                      end;
                                   if (ccode1=740) and (ccode2=365) then
                                      begin
                                         city1 := locations^[ccode_index.index(ccode1)];
                                         while (not (city1^.cityname='Tokyo')) do city1 := city1^.next_rec;
                                         city2 := locations^[ccode_index.index(ccode2)];
                                         while (not (city2^.cityname='Vladivostok')) do city2 := city2^.next_rec;
                                         distmiles := great_circle_distance (city1^, city2^);
                                         current_dyad_dist_rec :=
                                            dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)];
                                         repeat   {for each time record for this dyad, set to this new distance}
                                            current_dyad_dist_rec^.distance := distmiles;
                                            current_dyad_dist_rec := current_dyad_dist_rec^.next_rec;
                                         until current_dyad_dist_rec = nil;
                                      end;

                                end;   {if user_selections.distance_method = capitols_contiguity_war_trap }

                           end;   {different states}

                       end;      {have info on ccode1, ccode2  }

               Dist_trace.tickdone;
               created := true;

            finally
               Dist_Trace.free;
               trace.exit('Finished initializing dyadic distance data');
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
      end;     {constructor init}

   destructor Tdistance_array_obj.destroy;
      var ccode1, ccode2 : ccode_range;
          prev, temp : distance_rec_ptr;
          x : ccode_index_range;
          current_rec, prev_rec : location_rec_ptr;

      begin
         try
            if self <> nil then
              begin
                 if dyadic_data <> nil then
                   begin
                    for ccode1 := min_ccode to max_ccode do
                      if dyadic_data^[ccode_index.index(ccode1)] <> nil then
                        begin
                          for ccode2 := min_ccode to max_ccode do
                            if dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)] <> nil then
                            begin
                              if dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)]^.next_rec <> nil then
                                 begin
                                   temp := dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)]^.next_rec;
                                   prev := temp;
                                   repeat
                                      temp := temp^.next_rec;
                                      dispose(prev);
                                      prev := temp;
                                   until temp=nil;
                                 end;
                              dispose (dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)]);
                              dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)] := nil;
                            end;  {if 1^2 not nil}
                          dispose (dyadic_data^[ccode_index.index(ccode1)]);
                          dyadic_data^[ccode_index.index(ccode1)] := nil;
                        end;  {if data^1 <> nil}
                    dispose (dyadic_data);
                   end;   {if dyadic_data <> nil}

                 if locations <> nil then
                   begin
                   for x := min_ccode_index to max_ccode_index do
                     if locations^[x] <> nil then
                       begin
                         current_rec := locations^[x];
                         repeat
                            prev_rec := current_rec;
                            current_rec := current_rec^.next_rec;
                            dispose (prev_rec);         {OK b/c we know prev is <> nil}
                         until current_rec = nil;
                         locations^[x] := nil;
                       end;   {if locations^[x] <> nil}
                   dispose(locations);
                 end;           {if locations <> nil}
                 created := false;
                 inherited destroy;

              end;   {if self <> nil then}
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;   {destructor}

   function Tdistance_array_obj.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then
            if created=true then initialized := true;
      end;


   function Tdistance_array_obj.all_in_range (ccode1, ccode2 : ccode_range;
            ayear : year_range; error_check : boolean) : boolean;
      begin
         all_in_range := true;
         if not(initialized) then
            begin
               all_in_range := false;
               if error_check then
                  begin
                     EUGeneError ('Distance "get" called before initialization',
                               5, continue, error_log);
                  end;
            end
         else
         if not (nation_list.is_a_state(ccode1, ayear) and nation_list.is_a_state(ccode2, ayear)) then
            begin
               all_in_range := false;
               if error_check then
                  begin
                     trace.message ('Tried to get distance for ccodes '+inttostr(ccode1) +
                        '-'+inttostr(ccode2)+' in '+inttostr(ayear)+' but not both are states.  '+
                        'Distance set to missing.  ');
                  end;
            end;
      end;   {function all_in_range}


   function Tdistance_array_obj.get_distance (ccode1, ccode2 : ccode_range;
            ayear : year_range) : integer;
      var distrec : distance_rec_ptr;
          finished, found : boolean;
      begin
      {this version always checks and reports errors.}
         get_distance := missing_value;
         if all_in_range (ccode1, ccode2, ayear, true) then
            begin
               finished := false;
               found := false;
               distrec := dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)];
               repeat
                  if (ayear >= distrec^.start_year) and (ayear <= distrec^.end_year) then
                     begin
                        get_distance := distrec^.distance;
                        finished := true;
                        found := true;
                     end;
                  distrec := distrec^.next_rec;
               until (finished = true) or (distrec = nil);

               if not (found) then
                  begin
                     EUGeneError ('Could not find distance for ccodes '+inttostr(ccode1) +
                            '-'+inttostr(ccode2)+' in '+inttostr(ayear)+'.  Set to missing.  Latitude/Longitude file probably not updated to match state list.  Notify programmer.',
                                     5, continue, error_log);
                     get_distance := missing_value;
                  end;

               if result = initialized_value then
                  begin
                     trace.message('distance still at initialized value for ' +
                              inttostr(ccode1)+' and '+inttostr(ccode2)+' in '+inttostr(ayear)+').  Distance set to missing');
                     get_distance := missing_value;
                  end;
               end;

      end;   {method get distance}

      {  ---------------------------------------------------------   }

   function Tdistance_array_obj.get_distance (ccode1, ccode2 : ccode_range;
            ayear : year_range; error_check : boolean) : integer;
      var distrec : distance_rec_ptr;
          finished, found : boolean;
      begin
      {this version always checks and reports errors.}
         get_distance := missing_value;
         if all_in_range (ccode1, ccode2, ayear, error_check) then
            begin
               finished := false;
               found := false;
               distrec := dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)];
               repeat
                  if (ayear >= distrec^.start_year) and (ayear <= distrec^.end_year) then
                     begin
                        get_distance := distrec^.distance;
                        finished := true;
                        found := true;
                     end;
                  distrec := distrec^.next_rec;
               until (finished = true) or (distrec = nil);

               if not (found) then
                  begin
                     EUGeneError ('Could not find distance for ccodes '+inttostr(ccode1) +
                            '-'+inttostr(ccode2)+' in '+inttostr(ayear)+'.  Set to missing.',
                                     5, continue, error_log);
                     get_distance := missing_value;
                  end;

               if result = initialized_value then
                  begin
                     trace.message('distance still at initialized value for ' +
                              inttostr(ccode1)+' and '+inttostr(ccode2)+' in '+inttostr(ayear)+').  Distance set to missing');
                     get_distance := missing_value;
                  end;
               end;

      end;   {method get distance}

      {  ---------------------------------------------------------   }

   {Methods for Gleditsch minimum distance object}
   constructor Tmindist_array_obj.init (year1, year2 : year_range; a_file_name : TFileName);
      type dyadic_in_rec_type = record
         abbrev1, abbrev2 : string;
         ccode1, ccode2 : ccode_range;
         year : year_range;
         distance : integer;
      end;
      var one_dist_year : dist_year_ptr;
          dyadic_in_rec : dyadic_in_rec_type;
          country_index1, country_index2 : ccode_index_range;
          mindist_trace : Ttrace_obj;
          start_mem, heapneeded : longint;
          year_loop : year_range;
          mindist_text_file : text;


      procedure read_mindist_text_rec (var dyadic_in_rec : dyadic_in_rec_type; var mindist_text_file : text);
         var temp_dist, tempstring : string;
             achar : char;
             x : integer;

         procedure fixgwabbrevs (var astring:string);
         begin
            if astring = 'UK' then astring := 'UKG';
            if astring = 'RUS' then astring := 'USR';
         end;

         begin
            tempstring:='';
            repeat
               read(mindist_text_file, achar);
            until (achar <> ' ') and (achar <> chr(9));
            tempstring := tempstring + achar;

            repeat
               read(mindist_text_file, achar);
               if achar <> ' ' then tempstring := tempstring + achar;
            until (achar = ' ') or (achar = chr(9));
            fixgwabbrevs(tempstring);
            dyadic_in_rec.abbrev1 := tempstring;

            {read blank}
            repeat
               read(mindist_text_file, achar);
            until (achar <> ' ') and (achar <> chr(9));

            tempstring := achar;
            repeat
               read(mindist_text_file, achar);
               if achar <> ' ' then tempstring := tempstring + achar;
            until (achar = ' ') or (achar = chr(9));
            fixgwabbrevs(tempstring);
            dyadic_in_rec.abbrev2 := tempstring;

            {read blank}
            repeat
               read(mindist_text_file, achar);
            until (achar <> ' ') and (achar <> chr(9));

            tempstring := achar;
            for x := 2 to 4 do
              begin
                  read(mindist_text_file, achar);
                  tempstring := tempstring + achar;
              end;
            dyadic_in_rec.year := strtoint(tempstring);

            {read blank}
            repeat
               read(mindist_text_file, achar);
            until (achar <> ' ') and (achar <> chr(9));
            temp_dist := achar;

            while not eoln (mindist_text_file) and not eof (mindist_text_file) do
               begin
                  read(mindist_text_file, achar);
                  temp_dist := temp_dist + achar;
               end;
            if temp_dist = '.' then dyadic_in_rec.distance := missing_value else
               dyadic_in_rec.distance := strtoint(temp_dist);

            readln(mindist_text_file);

            dyadic_in_rec.ccode1 := nation_list.get_ccode_from_abbrev(dyadic_in_rec.abbrev1);
            dyadic_in_rec.ccode2 := nation_list.get_ccode_from_abbrev(dyadic_in_rec.abbrev2);

            {They don't put Germany back to 255}
            if (dyadic_in_rec.year >=1991) and (dyadic_in_rec.ccode1 = 260) then
               dyadic_in_rec.ccode1 := 255;
         end;

      begin
        try
            try
               start_mem := memavail;
               if year1 > year2 then
                  switch_year (year1, year2);
               trace.enter ( ('Initializing mindist array from dyads, ' + inttostr(year1) + ' to '
                              + inttostr(year2)) );
               mindist_trace := nil;
               mindist_trace := TTrace_obj.init(trace.get_trace_level);
               created := false;
               self.first_partition_year := year1;
               self.last_partition_year := year2;

               if not(ccode_index.initialized) then
                  begin
                     EUGeneError ('Minimum Distance array creation called before ccode_index initialized',
                                     5, stop, error_log);
                     mindist_trace.message ('Check program coding [notify programmer].  ');
                  end;
               if not(nation_list.initialized) then
                  begin
                     EUGeneError ('Minimum distance array creation called before nation_list initialized',
                                     5, stop, error_log);
                     mindist_trace.message ('Check program coding [notify programmer].  ');
                  end;

               heapneeded := Talliance_array_obj_mem_overhead +
                             (year2 - year1 + 1) * Talliance_array_obj_mem_per_year;
               if debug[4] then
                  begin
                     mindist_trace.message ('Minimum Distance array size calculation in .init');
                     mindist_trace.message ('Calc is that '+inttostr(heapneeded)+' needed for structure.');
                     mindist_trace.message ('Max avail mem block is '+inttostr(MaxAvail));
                  end;
               if MaxAvail <= (heapneeded) then
                  begin
                     EUGeneError ('Not enough memory for minimum distance data structure. ',
                                     5, stop, error_log);
                     mindist_trace.message ('Check program coding [notify programmer].  ');
                  end;

               {Initially, the .init constructor just sets the data to nil}
               for year_loop := min_year to max_year do
                  begin
                    year_array[year_loop] := nil;
                    mindist_trace.tick('Executing Procedure: Initializing min dist Array,'+inttostr(year_loop),max_year-min_year+1);
                  end;
               mindist_trace.tickdone;

               {initialize the records that are in range, which will then be read from the input file. }
               for year_loop := self.first_partition_year to self.last_partition_year do
                  begin
                     mindist_trace.tick('Executing Procedure: Initializing Empty min dist Array,'+inttostr(year_loop),max_year-min_year+1);
                     new (one_dist_year);
                     for country_index1 := min_ccode_index to max_ccode_index do
                        for country_index2 := min_ccode_index to max_ccode_index do
                           begin
                              one_dist_year^[country_index1, country_index2] := initialized_value;
                           end;
                     year_array[year_loop]:= one_dist_year;
                  end;
               mindist_trace.tickdone;

               {The array is now initialized, and can be accessed, so set it to show as initialized}
               created := true;

               {now open and repeatedly read records here, until all relevant records are in}
               assignFile (mindist_text_file, a_file_name);
               reset (mindist_text_file);
               mindist_trace.message ('Reading min dist data from external dyadic file');

               read_mindist_text_rec (dyadic_in_rec, mindist_text_file);

               if (dyadic_in_rec.year >= self.first_partition_year) and
                  (dyadic_in_rec.year <= self.last_partition_year) then
                  begin
                     year_array[dyadic_in_rec.year]^[ccode_index.index(dyadic_in_rec.ccode1),ccode_index.index(dyadic_in_rec.ccode2)] :=
                        dyadic_in_rec.distance;
                  end;

               {now read records until hit year out of range, that I don't want}
               while not eof (mindist_text_file) and (dyadic_in_rec.year <= self.last_partition_year) do
                  begin
                     mindist_trace.tick( 'Executing Procedure: Reading minimum distance Data', 400000);
                     read_mindist_text_rec (dyadic_in_rec, mindist_text_file);
                     if (dyadic_in_rec.year >= first_partition_year) and
                        (dyadic_in_rec.year <= last_partition_year) then
                     begin
                        year_array[dyadic_in_rec.year]^[ccode_index.index(dyadic_in_rec.ccode1),ccode_index.index(dyadic_in_rec.ccode2)] :=
                           dyadic_in_rec.distance;
                     end;
                  end;
               {Now completed reading data records!}
               mindist_trace.tickdone;

               mindist_trace.message ('Finished reading minimum distance data from file');

            finally
               CloseFile (mindist_text_file);
               mindist_trace.free;
               trace.message ('Minimum distance data required ' + inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
               trace.exit ('Finished Initializing mindist array');
            end;  {finally}
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

      end;        {init}

   destructor Tmindist_array_obj.destroy;
      begin
      end;

   function Tmindist_array_obj.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then
            if created=true then initialized := true;
      end;

   function Tmindist_array_obj.all_in_range (ccode1, ccode2 : ccode_range;
            ayear : year_range; error_check : boolean) : boolean;
      begin
         all_in_range := true;
         if not(initialized) then
            begin
               all_in_range := false;
               if error_check then
                  begin
                     EUGeneError ('Distance "get" called before initialization', 5, continue, error_log);
                  end;
            end
         else
            if not ((ayear >= get_first_partition_year) and (ayear <= get_last_partition_year)) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        EUGeneError ('min dist get called with out-of-bounds year ('+inttostr(ayear)+'). ',
                                        5, continue, error_log);
                        trace.message ('distance set to missing');
                     end;
               end
         else
         if not (nation_list.is_a_state(ccode1, ayear) and nation_list.is_a_state(ccode2, ayear)) then
            begin
               all_in_range := false;
               if error_check then
                  begin
                     trace.message ('Tried to get min distance for ccodes '+inttostr(ccode1) +
                        '-'+inttostr(ccode2)+' in '+inttostr(ayear)+' but not both are states.  '+
                        'Distance set to missing.  ');
                  end;
            end;
      end;   {function all_in_range}


   function Tmindist_array_obj.get_distance (ccode1, ccode2 : ccode_range;
            ayear : year_range) : integer;
      var distrec : distance_rec_ptr;
          finished, found : boolean;
      begin
      {this version always checks and reports errors.}
         get_distance := missing_value;
         if all_in_range (ccode1, ccode2, ayear, true) then
            begin
               result := year_array[ayear]^[ccode_index.index(ccode1),ccode_index.index(ccode2)];

                if result = initialized_value then
                  begin
                     trace.message('min distance still at initialized value for ' +
                              inttostr(ccode1)+' and '+inttostr(ccode2)+' in '+inttostr(ayear)+').  Distance set to missing');
                     get_distance := missing_value;
                  end;
               end;
       end;   {method get distance}

      {  ---------------------------------------------------------   }

   function Tmindist_array_obj.get_distance (ccode1, ccode2 : ccode_range;
            ayear : year_range; error_check : boolean) : integer;
      var distrec : distance_rec_ptr;
          finished, found : boolean;
      begin
      {this version always checks and reports errors.}
         get_distance := missing_value;
         if all_in_range (ccode1, ccode2, ayear, error_check) then
            begin
               result := year_array[ayear]^[ccode_index.index(ccode1),ccode_index.index(ccode2)];

               if result = initialized_value then
                  begin
                     trace.message('min distance still at initialized value for ' +
                              inttostr(ccode1)+' and '+inttostr(ccode2)+' in '+inttostr(ayear)+').  Distance set to missing');
                     get_distance := missing_value;
                  end;
               end;

      end;   {method get distance}

   function Tmindist_array_obj.get_first_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Tmindist_array_obj get_first_partition_year called before initialization',
                               10, stop, error_log);
               trace.message ('Check program coding [notify programmer]. ' );
            end
         else
            get_first_partition_year := first_partition_year;
      end;

               { ---------------------------  }

   function Tmindist_array_obj.get_last_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Tmindist_array_obj get_last_partition_year called before initialization',
                               10, stop, error_log);
               trace.message ('Check program coding [notify programmer].  ');
            end
         else
         get_last_partition_year := last_partition_year;
      end;

      {  ---------------------------------------------------------   }

   {has info if they are contiguous, nothing otherwise}

   constructor Tcontiguity_array_obj.init (afilename : TFileName);
      var start_mem, heapneeded : longint;
          contig_file : text;
          x : integer;
          keep_rec, updated : boolean;
          new_rec : contigu_rec_ptr;
          ccode1, ccode2 : ccode_range;
          tempcol : entity_range;
          sub_contig_array : contig2_ptr;
          curr_rec : contigu_rec_ptr;
          rcount : integer;
          Contig_trace : TTrace_obj;

      procedure read_one_rec (var infile : text; var outrec : contigu_rec_ptr;
                var keep : boolean; var ccode1, ccode2 : ccode_range);
          {reads a record from contiguity data set and marks it as to keep
           if it is a valid ccode and is a direct contiguity relationship.}
        var
            tempint : integer;
            tempstring : string;
            start_year, end_year : year_range;
            contig_type : contiguity_range;
            achar : char;
            bad_rec, colonial1_rec, colonial2_rec : boolean;
            colony1, colony2 : entity_range;
            arec : contigu_rec_ptr;

        function string_read (var infile : text; num : integer) : string;
           var outstring : string;
               achar : char;
               x : integer;
           begin
              outstring := '';
              for x := 1 to num do
                 begin
                    read (infile, achar);
                    outstring := outstring + achar;
                 end;
              string_read := outstring;
           end;

        begin   {read_one_rec}
           ccode1 := 0;
           ccode2 := 0;
           bad_rec := false;
           colonial1_rec := false;
           colonial2_rec := false;
           {read some data}
           {1st 3 chars are first ccode}
           tempint := StrToInt (string_read (infile, 3));
           if (tempint >= min_ccode) and (tempint <= max_ccode) then
                ccode1 := tempint
              else bad_rec := true;   {entity out of range}
           {also make sure it's a state}
           if nation_list.have_info(ccode1) = false then bad_rec := true;

           {read a space}
           read (infile, achar);

           {next is colonial possession;  I want next 4 to be blank if I want the
            record as direct contiguity, if not blank then colonial.}
           tempstring := string_read (infile,4);
           if tempstring <> '    ' then
              begin
                 colonial1_rec := true;
                 colony1 := StrToIntDef (tempstring, initialized_value);
              end;

           {read another space}
           read (infile, achar);

           {read contiguity type}
           tempint := StrToInt (string_read (infile, 1));
           if (tempint >= 1) and (tempint <= 5) then contig_type := tempint
             else
               begin
                  EUGeneError ('Error in contiguity read - incorrect contig. type seen.  Set to 5.',
                        5, continue, error_log);
                  contig_type := 5;
               end;

           {read another space}
           read (infile, achar);

           {next is 2nd ccode}
           tempint := StrToInt (string_read (infile, 3));
           if (tempint >= min_ccode) and (tempint <= max_ccode) then
                ccode2 := tempint
              else bad_rec := true;   {entity out of range}
           if nation_list.have_info(ccode2) = false then bad_rec := true;

           {read a space}
           read (infile, achar);

           {next is 2nd colonial possession;  I want next 4 to be blank if I want the
            record, b/c I don't care about colonial contiguity.}
           tempstring := string_read (infile,4);
           if tempstring <> '    ' then
              begin
                 colonial2_rec := true;
                 colony2 := StrToIntDef (tempstring, initialized_value);
              end;

           {read another space}
           read (infile, achar);

           {now read start, end year.}
           tempint := StrToInt (string_read (infile, 4));
           if (tempint <= min_year) then start_year := min_year
             else if (tempint >= max_year) then start_year := max_year
             else start_year := tempint;
           read (infile, achar);
           tempint := StrToInt (string_read (infile, 4));
           if (tempint <= min_year) then end_year := min_year
             else if (tempint >= max_year) then end_year := max_year
             else end_year := tempint;

           if not eof(infile) then readln (infile);

           {if it's bad, do nothing, do not create a new "arec" record.}
           {if it's good, then...}
           if ((bad_rec = false) and (colonial1_rec = false) and (colonial2_rec = false)) then
             {This is a direct contiguity record}
             begin
                new (arec);
                arec^.contiguity_type := contig_type;
                arec^.direct_colonial_type := direct;
                arec^.colony_code[1] := min_entity;
                arec^.colony_code[2] := min_entity;
                arec^.start_year := start_year;
                arec^.end_year := end_year;
                arec^.next_rec := nil;
                keep := true;
             end
           else
           if ((bad_rec = false) and ((colonial1_rec = true) or (colonial2_rec = false))) then
             {This is a colonial contiguity record}
             begin
                new (arec);
                {for colonial records, I want them to be distinguished from direct
                 by the type.}
                arec^.contiguity_type := contig_type;
                arec^.direct_colonial_type := colonial;
                arec^.colony_code[1] := colony1;
                arec^.colony_code[2] := colony2;
                arec^.start_year := start_year;
                arec^.end_year := end_year;
                arec^.next_rec := nil;
                keep := true;
             end
           else    {THIS WAS A BAD REC, so skip it}
             begin
                arec := nil;
                keep := false;
             end;

           outrec := arec;
        end;

      procedure read_one_rec_csv (var infile : text; var outrec : contigu_rec_ptr;
                var keep : boolean; var ccode1, ccode2 : ccode_range);
          {reads a record from contiguity data set set up as .csv and marks it as to keep
           if it is a valid ccode and is a direct contiguity relationship.}
        var
            tempint : integer;
            tempstring : string;
            start_year, end_year : year_range;
            contig_type : contiguity_range;
            achar : char;
            bad_rec, colonial1_rec, colonial2_rec : boolean;
            colony1, colony2 : entity_range;
            arec : contigu_rec_ptr;

        function string_read (var infile : text; num : integer) : string;
           var outstring : string;
               achar : char;
               x : integer;
           begin
              outstring := '';
              for x := 1 to num do
                 begin
                    read (infile, achar);
                    outstring := outstring + achar;
                 end;
              string_read := outstring;
           end;

        begin   {read_one_rec}
           ccode1 := 0;
           ccode2 := 0;
           bad_rec := false;
           colonial1_rec := false;
           colonial2_rec := false;
           {read some data}
           {1st 3 chars are first ccode}
           tempint := read_csv_int(infile);
           if (tempint >= min_ccode) and (tempint <= max_ccode) then
                ccode1 := tempint
              else bad_rec := true;   {entity out of range}
           {also make sure it's a state}
           if nation_list.have_info(ccode1) = false then bad_rec := true;

           {next is colonial possession;  I want next 4 to be blank if I want the
            record as direct contiguity, if not blank then colonial.}
           tempstring := read_csv_string(infile);
           if tempstring <> '' then
              begin
                 colonial1_rec := true;
                 colony1 := StrToIntDef (tempstring, initialized_value);
              end;

           {read contiguity type}
           tempint := read_csv_int(infile);
           if (tempint >= 1) and (tempint <= 5) then contig_type := tempint
             else
               begin
                  EUGeneError ('Error in contiguity read - incorrect contig. type seen.  Set to 5.', 5, continue, error_log);
                  contig_type := 5;
               end;

           {next is 2nd ccode}
           tempint := read_csv_int(infile);
           if (tempint >= min_ccode) and (tempint <= max_ccode) then
                ccode2 := tempint
              else bad_rec := true;   {entity out of range}
           if nation_list.have_info(ccode2) = false then bad_rec := true;

           {next is 2nd colonial possession;  I want next 4 to be blank if I want the
            record, b/c I don't care about colonial contiguity.}
           tempstring := read_csv_string (infile);
           if tempstring <> '' then
              begin
                 colonial2_rec := true;
                 colony2 := StrToIntDef (tempstring, initialized_value);
              end;

           {now read start, end year.}
           tempint := read_csv_int(infile);
           if (tempint <= min_year) then start_year := min_year
             else if (tempint >= max_year) then start_year := max_year
             else start_year := tempint;
           tempint := read_csv_int(infile);
           if (tempint <= min_year) then end_year := min_year
             else if (tempint >= max_year) then end_year := max_year
             else end_year := tempint;

           if not eof(infile) then readln (infile);

           {if it's bad, do nothing, do not create a new "arec" record.}
           {if it's good, then...}
           if ((bad_rec = false) and (colonial1_rec = false) and (colonial2_rec = false)) then
             {This is a direct contiguity record}
             begin
                new (arec);
                arec^.contiguity_type := contig_type;
                arec^.direct_colonial_type := direct;
                arec^.colony_code[1] := min_entity;
                arec^.colony_code[2] := min_entity;
                arec^.start_year := start_year;
                arec^.end_year := end_year;
                arec^.next_rec := nil;
                keep := true;
             end
           else
           if ((bad_rec = false) and ((colonial1_rec = true) or (colonial2_rec = false))) then
             {This is a colonial contiguity record}
             begin
                new (arec);
                {for colonial records, I want them to be distinguished from direct
                 by the type.}
                arec^.contiguity_type := contig_type;
                arec^.direct_colonial_type := colonial;
                arec^.colony_code[1] := colony1;
                arec^.colony_code[2] := colony2;
                arec^.start_year := start_year;
                arec^.end_year := end_year;
                arec^.next_rec := nil;
                keep := true;
             end
           else    {THIS WAS A BAD REC, so skip it}
             begin
                arec := nil;
                keep := false;
             end;

           outrec := arec;
        end;

      begin            {main proc contiguity init}
         try
            try
               created := false;
               start_mem := memavail;
               trace.enter('Initializing contiguity data');
               Contig_trace := nil;
               Contig_trace := TTrace_obj.init(trace.get_trace_level);
               if not(ccode_index.initialized) then
                  begin
                     EUGeneError ('Contiguity array creation called before ccode_index initialized',
                                     5, stop, error_log);
                     trace.message ('Check program coding [notify programmer].  ');
                  end;
               if not(nation_list.initialized) then
                  begin
                     EUGeneError ('Contiguity array creation called before nation_list initialized',
                                     5, stop, error_log);
                     trace.message ('Check program coding [notify programmer].  ');
                  end;

               heapneeded := Tcontiguity_array_mem_overhead;
               if MaxAvail <= heapneeded then
                  begin
                     EUGeneError ('Not enough memory for contiguity array. ',
                                     5, stop, error_log);
                  end;

               AssignFile (contig_file, afilename);
               reset (contig_file);
               {read initial line of labels}
               readln(contig_file);
               rcount := 0;
               new (dyadic_data);
               for x := min_ccode_index to max_ccode_index do
                  dyadic_data^[x] := nil;
               while not (eof (contig_file)) do
                  begin
                     Contig_trace.tick ('Executing Procedure: Read contiguity data', 4200);
                     read_one_rec_csv (contig_file, new_rec, keep_rec, ccode1, ccode2);
                     rcount := rcount + 1;
                        {keep only 1/2 of this array, b/c symmetric}
                     if (keep_rec = true) then
                        begin
                           if ccode2 < ccode1 then   {switch ccode and directional info on colonies}
                              begin
                                 switch_ccode (ccode1, ccode2);
                                 tempcol := new_rec^.colony_code[1];
                                 new_rec^.colony_code[1] := new_rec^.colony_code[2];
                                 new_rec^.colony_code[2] := tempcol;
                              end;
                           {attach data record somewhere}
                           {do we have ccode1?  }
                           if dyadic_data^[ccode_index.index(ccode1)] = nil then
                              begin
                                {create new sub-rec}
                                new (sub_contig_array);
                                for x := min_ccode_index to max_ccode_index do
                                   sub_contig_array^[x] := nil;
                                dyadic_data^[ccode_index.index(ccode1)] := sub_contig_array;
                              end;
                            {Now, either had or created ccode1 record.  Do we have ccode2?}
                           if dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)] = nil then
                              begin
                                 {add first record for this dyad}
                                 dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)] := new_rec;
                              end
                           else    {not nil, add a new record to end of list if it's a new record}
                              begin
                                 curr_rec := dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)];
                                 updated := false;
                                    {is this record the same ccodes, type, and exact years as one I've seen??}
                                    {If it is, then just update values.  Otherwise, add as new record.}
                                 repeat
                                    if (curr_rec^.start_year=new_rec^.start_year) and
                                       (curr_rec^.end_year=new_rec^.end_year) and
                                       (curr_rec^.direct_colonial_type = new_rec^.direct_colonial_type) and
                                       (curr_rec^.colony_code[1]=new_rec^.colony_code[1]) and
                                       (curr_rec^.colony_code[2]=new_rec^.colony_code[2]) then
                                       begin   {if seen before, then update type only.}
                                               {This happens a lot because every dyad is in the data set in 2 directions.}
                                          curr_rec^.contiguity_type := new_rec^.contiguity_type;
                                          dispose (new_rec);
                                          updated := true;
                                       end;
                                    if curr_rec^.next_rec <> nil then curr_rec := curr_rec^.next_rec;
                                 until (updated=true) or (curr_rec^.next_rec = nil);
                                 if not (updated) then {if didn't update, now add the rec as new}
                                    curr_rec^.next_rec := new_rec;
                             end;
                        end
                     else                   {keep_rec is false}
                        begin
                            {didn't want the record;  not relevant, so discard it.}
                        end;
                  end;      {while not begin}
               Contig_trace.tickdone;
               created := true;
            finally
               CloseFile (contig_file);
               Contig_trace.free;
               trace.message ('Contiguity data required ' + inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
               trace.exit('Finished initializing contiguity data');
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

   destructor Tcontiguity_array_obj.destroy;
      var x, y : integer;
          curr_rec, prev_rec : contigu_rec_ptr;
      begin
         try
            if self <> nil then
            begin
               created := false;
               if dyadic_data <> nil then
                  begin
                     for x := min_ccode_index to max_ccode_index do
                       if dyadic_data^[x] <> nil then
                         begin
                           for y := min_ccode_index to max_ccode_index do
                             if dyadic_data^[x]^[y] <> nil then
                               begin
                                 {this will dispose all pointed to records, including [x]^[y]}
                                  curr_rec := dyadic_data^[x]^[y];
                                  repeat
                                     prev_rec := curr_rec;
                                     curr_rec := curr_rec^.next_rec;
                                     dispose (prev_rec);
                                  until curr_rec = nil;
                               end;
                           dispose (dyadic_data^[x]);
                           dyadic_data^[x] := nil;
                         end;
                     dispose (dyadic_data);
                     dyadic_data := nil;
                  end;
                 inherited destroy;
            end;   {self <> nil}
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;

   function Tcontiguity_array_obj.is_directly_contiguous (ccode1, ccode2 : ccode_range;
            year : year_range; level_for_contiguity : contiguity_range) : boolean;
      var curr_rec : contigu_rec_ptr;
          found : boolean;
      begin
         if ccode2 < ccode1 then switch_ccode (ccode1, ccode2);
         is_directly_contiguous := false;
         if dyadic_data^[ccode_index.index(ccode1)] <> nil then
            begin
               if dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)] <> nil then
                  begin
                     curr_rec := dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)];
                     found := false;
                     repeat
                        if (year >= curr_rec^.start_year) and (year <= curr_rec^.end_year) then
                           begin
                                {in cow scheme, lower values are more contiguous, i.e. closer}
                              if curr_rec^.direct_colonial_type = direct then
                                 begin
                                    found := true;
                                    if curr_rec^.contiguity_type <= level_for_contiguity then
                                       is_directly_contiguous := true;
                                 end;
                           end;
                        curr_rec := curr_rec^.next_rec;
                     until (found) or (curr_rec = nil);
                  end;
            end;
      end;

   function Tcontiguity_array_obj.get_direct_contiguity_level (ccode1, ccode2 : ccode_range; year : year_range) : contiguity_range;
      var curr_rec : contigu_rec_ptr;
          found : boolean;
          {Returns level of direct contiguity between 2 states.  If contiguous by colonies,
           returns a 6 (for non-contiguous).}
      begin
         get_direct_contiguity_level := 6;   {start off with not contiguous}
         if ccode2 < ccode1 then switch_ccode (ccode1, ccode2);
         if dyadic_data^[ccode_index.index(ccode1)] <> nil then
            begin
               if dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)] <> nil then
                  begin
                     curr_rec := dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)];
                     found := false;
                     repeat
                        if (year >= curr_rec^.start_year) and (year <= curr_rec^.end_year) then
                           begin
                                {in cow scheme, lower values are more contiguous, i.e. closer}
                                {Here, I only want direct contiguity, which is level 5 or lower}
                              if curr_rec^.direct_colonial_type = direct then
                                 begin
                                    found := true;
                                    get_direct_contiguity_level := curr_rec^.contiguity_type;
                                 end;
                           end;
                        curr_rec := curr_rec^.next_rec;
                     until (found) or (curr_rec = nil);
                  end;
            end;
      end;

   function Tcontiguity_array_obj.get_colonial_contiguity_level (ccode1, ccode2 : ccode_range; year : year_range; var colony1, colony2 : entity_range) : contiguity_range;
      var curr_rec : contigu_rec_ptr;
          found : boolean;
          {Returns level of colonial contiguity between 2 states.  }
      begin
         result := 6;  {this is higher than anything we'll see}
         get_colonial_contiguity_level := 6;   {start off with not contiguous}
         colony1 := 0;
         colony2 := 0;
         {the array I need to search is only 1/2 an array, with ccode1 always
          < ccode2, but can't just switch the ccodes for searching, because the
          colonial entities are attached uniquely to ccode1 or ccode2.}
         {if ccode2 < ccode1 then switch_ccode (ccode1, ccode2);}
         if dyadic_data^[ccode_index.index(min(ccode1, ccode2))] <> nil then
            begin
               if dyadic_data^[ccode_index.index(min(ccode1, ccode2))]^[ccode_index.index(max(ccode1, ccode2))] <> nil then
                  begin
                     curr_rec := dyadic_data^[ccode_index.index(min(ccode1, ccode2))]^[ccode_index.index(max(ccode1, ccode2))];
                     found := false;
                     repeat  {Need to check all records b/c there could be several colonial relationships}
                        if (year >= curr_rec^.start_year) and (year <= curr_rec^.end_year) then
                           begin
                                {in cow scheme, lower values are more contiguous, i.e. closer}
                                {Here, I only want colonial contiguity.
                                 But there could be several colonial relationships.  Keep the lowest
                                 (closest) level.}
                              if curr_rec^.direct_colonial_type = colonial then
                                 if curr_rec^.contiguity_type < result then
                                    begin
                                       found := true;
                                       result := curr_rec^.contiguity_type;
                                       if min(ccode1, ccode2) = ccode1 then
                                          begin
                                             colony1 := curr_rec^.colony_code[1];
                                             colony2 := curr_rec^.colony_code[2];
                                          end
                                       else  {had to look up the reverse, so switch
                                              the colony #s to match original ccode order.}
                                          begin
                                             colony1 := curr_rec^.colony_code[2];
                                             colony2 := curr_rec^.colony_code[1];
                                          end;
                                    end;
                           end;
                        curr_rec := curr_rec^.next_rec;
                     until (curr_rec = nil);
                  end;
            end;
      end;

   function Tcontiguity_array_obj.is_directly_contiguous_ever (ccode1, ccode2 : ccode_range;
            level_for_contiguity : contiguity_range) : boolean;
      var curr_rec : contigu_rec_ptr;
          found : boolean;
      begin
         if ccode2 < ccode1 then switch_ccode (ccode1, ccode2);
         is_directly_contiguous_ever := false;
         if dyadic_data^[ccode_index.index(ccode1)] <> nil then
            begin
               if dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)] <> nil then
                  begin
                     curr_rec := dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)];
                     found := false;
                     repeat
                        if curr_rec^.direct_colonial_type = direct then
                        if curr_rec^.contiguity_type <= level_for_contiguity then
                           begin
                              is_directly_contiguous_ever := true;
                              found := true;
                           end;
                        curr_rec := curr_rec^.next_rec;
                     until (found) or (curr_rec = nil);
                  end;
            end;
      end;

   procedure Tcontiguity_array_obj.check_direct_contiguity (ccode1, ccode2 : ccode_range;
             year1, year2 : year_range; var continuous, partial, never : boolean;
             level_for_contiguity : contiguity_range);
      var found : boolean;
          curr_rec : contigu_rec_ptr;
      {Returns whether these 2 ccodes are directly contiguous on land ever, never, or always.
       This is only used by the distance procedure to reset contiguous states to 0 distance.}
      begin
         continuous := false;
         partial := false;
         never := true;   {default to never true, unless find an overlapping rec.}
         if ccode2 < ccode1 then switch_ccode (ccode1, ccode2);
         if year2 < year1 then switch_year (year1, year2);
         if dyadic_data^[ccode_index.index(ccode1)] = nil then
            begin
               never := true;
            end
         else
            begin
               if dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)] = nil then
                  begin
                     never := true;
                  end
               else
                  begin
                     curr_rec := dyadic_data^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)];
                     {check each contiguity record to see if it spans this period.}
                     found := false;
                     repeat       {check each contiguity rec}
                        {if span is total, and there's some direct contiguity type, then it's continuous}
                        if (curr_rec^.direct_colonial_type = direct) and (curr_rec^.start_year <= year1) and (curr_rec^.end_year >= year2) then
                           begin
                              found := true;
                                {in cow scheme, lower values are more contiguous, i.e. closer}
                                  {if spans whole period, but is not within
                                  OK distance, then leave as never. }
                              if curr_rec^.contiguity_type <= level_for_contiguity then
                                 begin
                                    continuous := true;
                                    never := false;
                                 end
                              else
                                 begin
                                     {here, record found, but not in range.  Leave
                                      as never contiguous}
                                 end;
                           end
                           {if span is partial then partial}
                        else if (curr_rec^.direct_colonial_type = direct) and
                                ((year1 >= curr_rec^.start_year) and (year1 <= curr_rec^.end_year)) or
                                ((year2 >= curr_rec^.start_year) and (year2 <= curr_rec^.end_year)) or
                                ((year1 <= curr_rec^.start_year) and (year2 >= curr_rec^.start_year)) or
                                ((year1 <= curr_rec^.end_year) and (year2 >= curr_rec^.end_year)) then
                           begin
                                {in cow scheme, lower values are more contiguous, i.e. closer}
                              if curr_rec^.contiguity_type <= level_for_contiguity then
                                 begin
                                    found := true;
                                    partial := true;
                                    never := false;
                                 end;
                           end
                        else   {had a record, but there was neither partial nor continuous
                                coverage}
                           begin
                              {Do nothing here, leave as never as the default}
                           end;
                        curr_rec := curr_rec^.next_rec;
                     until (found) or (curr_rec = nil);    {might repeat again if partial}
                  end;
            end;

      end;

      {  ---------------------------------------------------------   }

      {methods for wanted_dyad_list}
      {this is directed, but can use as non directed by taking 1 direction outside}
   constructor Twanted_dyad_list_obj.init;
      begin
         try
            trace.enter('Initializing wanted list');
            data := nil;
            current := nil;   {used for retrieving dyads}
            created := true;
            num_dyad_years := 0;
            num_dyads := 0;
         finally
            trace.exit ('Finished Initializing wanted list');
         end;
      end;

   destructor Twanted_dyad_list_obj.destroy;
      var curr, prev : dyad_rec_ptr;
      begin
         try
            if self <> nil then
               begin
                  curr := data;
                  while curr <> nil do
                     begin
                        prev := curr;
                        curr := curr^.next_rec;
                        dispose (prev);
                     end;
                  created := false;
                  inherited destroy;
               end;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;


   procedure Twanted_dyad_list_obj.update (user_selections : user_selection_type);
       {this proc will use the user selection for the subset of dyads wanted
        and create a "wanted" list.  Creates dyads, not dyad-years}
      var last : dyad_rec_ptr;
          start_mem, heapneeded : longint;
          ccode1, ccode2 : ccode_range;
          ccodeloop1, ccodeloop2 : 0..max_countries;
          current_user_dyad_rec : user_dyad_ptr;
          wanted_trace : Ttrace_obj;
          overlap11, overlap12, overlap21, overlap22 : boolean;

          function on_list (const ccode1, ccode2 : ccode_range) : boolean;
            {use this to check if a cc1 cc2 combo is already on the dyad list}
             var curr_rec : dyad_rec_ptr;
             begin
                on_list := false;
                if data <> nil then
                   begin
                      curr_rec := data;
                      repeat
                         if (curr_rec^.ccode1=ccode1) and (curr_rec^.ccode2=ccode2) then
                            on_list := true;
                         curr_rec := curr_rec^.next_rec;
                      until curr_rec = nil;
                   end;
             end;

          procedure add_new_rec (const ccode1, ccode2 : ccode_range);
              var arec : dyad_rec_ptr;
                  temp_year : year_range;
              begin
                 if MaxAvail <= (heapneeded) then
                    EUGeneError ('Not enough memory for wanted_dyad_list pointer.',
                    10, stop, error_log);
                 new(arec);
                 arec^.ccode1 := ccode1;
                 arec^.ccode2 := ccode2;
                 arec^.next_rec := nil;
                 if data = nil then
                    begin
                       data := arec;
                       last := arec;
                    end
                 else
                    begin
                       last^.next_rec := arec;
                       last := last^.next_rec;
                    end;

                 inc(num_dyads);

                 {To calculate # dyad years, check each year in user selection vs. criteria
                  for both states being members}
                 for temp_year := user_selections.first_year to user_selections.last_year do
                    if ( ( ((temp_year >= nation_list.get_startyear1(ccode1)) and (temp_year <= nation_list.get_endyear1(ccode1))) or ((temp_year >= nation_list.get_startyear2(ccode1)) and (temp_year <= nation_list.get_endyear2(ccode1))) ) and
                         ( ((temp_year >= nation_list.get_startyear1(ccode2)) and (temp_year <= nation_list.get_endyear1(ccode2))) or ((temp_year >= nation_list.get_startyear2(ccode2)) and (temp_year <= nation_list.get_endyear2(ccode2))) ) )
                    then inc(num_dyad_years);

                 {To calculate # dyad years, check each combo of start/end year 1/2.
                  Count amount of overlap in each case, and whether we want to count that overlap.}
{                overlap11 := true;
                if (nation_list.get_endyear1(ccode1) < nation_list.get_startyear1(ccode2)) or
                                (nation_list.get_startyear1(ccode1) > nation_list.get_endyear1(ccode2)) then
                                overlap11 := false;
                overlap12 := true;
                if (nation_list.get_endyear1(ccode1) < nation_list.get_startyear2(ccode2)) or
                                (nation_list.get_startyear1(ccode1) > nation_list.get_endyear2(ccode2)) or
                                (nation_list.get_startyear2(ccode2) = min_year) or
                                (nation_list.get_endyear2(ccode2) = min_year) then
                     overlap12 := false;
                overlap21 := true;
                if (nation_list.get_endyear2(ccode1) < nation_list.get_startyear1(ccode2)) or
                                (nation_list.get_startyear2(ccode1) > nation_list.get_endyear1(ccode2)) or
                                (nation_list.get_startyear2(ccode1) = min_year) or
                                (nation_list.get_endyear2(ccode1) = min_year) then
                     overlap21 := false;
                overlap22 := true;
                if (nation_list.get_endyear2(ccode1) < nation_list.get_startyear2(ccode2)) or
                                (nation_list.get_startyear2(ccode1) > nation_list.get_endyear2(ccode2)) or
                                (nation_list.get_startyear2(ccode1) = min_year) or
                                (nation_list.get_endyear2(ccode1) = min_year) or
                                (nation_list.get_startyear2(ccode2) = min_year) or
                                (nation_list.get_endyear2(ccode2) = min_year) then
                     overlap22 := false;
                if overlap11 then num_dyad_years := num_dyad_years +
                     (min(user_selections.last_year,
                         min(nation_list.get_endyear1(ccode1),nation_list.get_endyear1(ccode2))) -
                      max(user_selections.first_year,
                         max(nation_list.get_startyear1(ccode1),nation_list.get_startyear1(ccode2)))) + 1;
                if overlap12 then num_dyad_years := num_dyad_years +
                     (min(user_selections.last_year,
                         min(nation_list.get_endyear1(ccode1),nation_list.get_endyear2(ccode2))) -
                      max(user_selections.first_year,
                         max(nation_list.get_startyear1(ccode1),nation_list.get_startyear2(ccode2)))) + 1;
                if overlap21 then num_dyad_years := num_dyad_years +
                     (min(user_selections.last_year,
                         min(nation_list.get_endyear2(ccode1),nation_list.get_endyear1(ccode2))) -
                      max(user_selections.first_year,
                         max(nation_list.get_startyear2(ccode1),nation_list.get_startyear1(ccode2)))) + 1;
                if overlap22 then num_dyad_years := num_dyad_years +
                     (min(user_selections.last_year,
                         min(nation_list.get_endyear2(ccode1),nation_list.get_endyear2(ccode2))) -
                      max(user_selections.first_year,
                         max(nation_list.get_startyear2(ccode1),nation_list.get_startyear2(ccode2)))) + 1;

                {old formula that didn't quite work b/c of start, endyear2 problems.}
                {num_dyad_years := num_dyad_years +
                     (min(user_selections.last_year,
                             min(max(nation_list.get_endyear1(ccode1),nation_list.get_endyear2(ccode1)),
                                    max(nation_list.get_endyear1(ccode2),nation_list.get_endyear2(ccode2)))) -
                      max(user_selections.first_year,
                             max(min(nation_list.get_startyear1(ccode1),nation_list.get_startyear2(ccode1)),
                                    min(nation_list.get_startyear1(ccode2),nation_list.get_startyear2(ccode2))))
                      + 1);   }

              end;


      begin         {main wanted list procedure}
         try
            try
               trace.enter('Updating wanted list');
               wanted_trace := TTrace_obj.init (trace.get_trace_level);
               start_mem := memavail;
               if not(ccode_index.initialized) then
                  begin
                     EUGeneError ('Wanted List array creation called before ccode_index initialized',
                                     5, stop, error_log);
                  end;
               if not(nation_list.initialized) then
                  begin
                     EUGeneError ('Wanted List array creation called before nation_list initialized',
                                     10, stop, error_log);
                  end;

               heapneeded := sizeof(dyad_rec);
               {call this before every new in here, since it's quite a dynamic structure.}

               num_dyad_years := 0;
               num_dyads := 0;
               case user_selections.dyads_selected of
                  all_states : begin
                        {here, create wanted_dyads with all states}
                        for ccode1 := min_ccode to max_ccode do
                         if (nation_list.is_a_state_between (ccode1,
                              user_selections.first_year, user_selections.last_year)) then
                          for ccode2 := min_ccode to max_ccode do
                            if nation_list.is_a_state_between (ccode2,
                             user_selections.first_year, user_selections.last_year) then
                             begin
                                wanted_trace.tick ('Executing Procedure: Create Dyad List (All States)', 40000);
                                {check and make sure want this re: i vs. i; make sure they overlap.}
                                if ((nation_list.different_states(ccode1, ccode2)) or (user_selections.output_format.printii=true)) then
                                {if ((ccode1<>ccode2) or (user_selections.output_format.printii=true)) then}
                                  if nation_list.country_date_overlap (ccode1, ccode2) then
                                    begin
                                       {if want directed, then add either direction.  If want
                                        non-directed, add only cc1 < cc2 direction}
                                       case user_selections.output_this of
                                         output_nondirected_dyads : if ccode1 <= ccode2 then add_new_rec (ccode1, ccode2);
                                         output_directed_dyads : add_new_rec (ccode1, ccode2);
                                         else EUGeneError ('Called wanted_dyad_list.update but dyads not selected for output', 1, stop, error_log);
                                         end;   {case}
                                    end;
                             end;
                         wanted_trace.tickdone;
                     end;

                  all_gp_gp : begin
                        {here, create wanted_dyads with gps only}
                        for ccode1 := min_ccode to max_ccode do
                         if (nation_list.is_a_gp_between (ccode1,
                            user_selections.first_year, user_selections.last_year)) then
                          for ccode2 := min_ccode to max_ccode do
                            if nation_list.is_a_gp_between (ccode2,
                             user_selections.first_year, user_selections.last_year) then
                             begin
                                wanted_trace.tick ('Executing Procedure: Create Dyad List (GPs vs. GPs)', 100);
                                if (nation_list.different_states(ccode1, ccode2)) or (user_selections.output_format.printii=true) then
                                  if nation_list.country_date_overlap (ccode1, ccode2) then
                                    begin
                                       {if want directed, then add either direction.  If want
                                        non-directed, add only cc1 < cc2 direction}
                                       case user_selections.output_this of
                                         output_nondirected_dyads : if ccode1 <= ccode2 then add_new_rec (ccode1, ccode2);
                                         output_directed_dyads : add_new_rec (ccode1, ccode2);
                                         else EUGeneError ('Called wanted_dyad_list.update but dyads not selected for output', 1, stop, error_log);
                                         end;   {case}
                                    end;
                             end;
                         wanted_trace.tickdone;
                     end;

                  all_contiguous : begin
                      for ccode1 := min_ccode to max_ccode do
                       if nation_list.is_a_state_between (ccode1,
                           user_selections.first_year, user_selections.last_year) then
                         for ccode2 := min_ccode to max_ccode do
                           if nation_list.is_a_state_between (ccode2,
                              user_selections.first_year, user_selections.last_year) then
                             if contiguity_data.is_directly_contiguous_ever (ccode1, ccode2, user_selections.contiguity_level_required) then
                             begin
                                wanted_trace.tick ( 'Executing Procedure: Create Dyad List (Contiguous States)',0);
                                if (nation_list.different_states(ccode1, ccode2)) or (user_selections.output_format.printii=true) then
                                  if nation_list.country_date_overlap (ccode1, ccode2) then
                                    begin
                                       {if want directed, then add either direction.  If want
                                        non-directed, add only cc1 < cc2 direction}
                                       case user_selections.output_this of
                                         output_nondirected_dyads : if ccode1 <= ccode2 then add_new_rec (ccode1, ccode2);
                                         output_directed_dyads : add_new_rec (ccode1, ccode2);
                                         else EUGeneError ('Called wanted_dyad_list.update but dyads not selected for output', 1, stop, error_log);
                                         end;   {case}
                                    end;
                             end;
                         wanted_trace.tickdone;
                     end;

                  all_gp_any : begin
                        {here, create wanted_dyads with at least one gp}
                        for ccode1 := min_ccode to max_ccode do
                          if nation_list.is_a_state_between (ccode1, user_selections.first_year, user_selections.last_year) then
                            for ccode2 := min_ccode to max_ccode do
                              if nation_list.is_a_state_between (ccode2, user_selections.first_year, user_selections.last_year) then
                              begin       {they are both states.}
                                 {now, is at least one of them a GP?}
                                 if (nation_list.is_a_gp_between (ccode1, user_selections.first_year, user_selections.last_year)
                                     or nation_list.is_a_gp_between (ccode2, user_selections.first_year, user_selections.last_year)
                                     ) then   {both states, one is a GP}
                                  begin
                                      wanted_trace.tick ('Executing Procedure: Create Dyad List (GPs vs All Others',0);
                                      if (nation_list.different_states(ccode1, ccode2)) or (user_selections.output_format.printii=true) then
                                        if nation_list.country_date_overlap (ccode1, ccode2) then
                                          begin
                                             {if want directed, then add either direction.  If want
                                              non-directed, add only cc1 < cc2 direction}
                                             case user_selections.output_this of
                                               output_nondirected_dyads : if ccode1 <= ccode2 then add_new_rec (ccode1, ccode2);
                                               output_directed_dyads : add_new_rec (ccode1, ccode2);
                                               else EUGeneError ('Called wanted_dyad_list.update but dyads not selected for output', 1, stop, error_log);
                                               end;   {case}
                                          end;
                              end;   {they are both states}
                            end;   {all_gp_any case}
                        wanted_trace.tickdone;
                     end;

                  politically_relevant : begin
                        {here, create wanted_dyads with at least one gp OR contiguous}
                        for ccode1 := min_ccode to max_ccode do
                        for ccode2 := min_ccode to max_ccode do
                          if is_politically_relevant_between (ccode1, ccode2,
                               user_selections.first_year, user_selections.last_year) then
                            begin
                                wanted_trace.tick ('Executing Procedure: Create Dyad List (Politically Relevant)',0);
                                if (nation_list.different_states(ccode1, ccode2)) or (user_selections.output_format.printii=true) then
                                  if nation_list.country_date_overlap (ccode1, ccode2) then
                                    begin
                                       {if want directed, then add either direction.  If want
                                        non-directed, add only cc1 < cc2 direction}
                                       case user_selections.output_this of
                                         output_nondirected_dyads : if ccode1 <= ccode2 then add_new_rec (ccode1, ccode2);
                                         output_directed_dyads : add_new_rec (ccode1, ccode2);
                                         else EUGeneError ('Called wanted_dyad_list.update but dyads not selected for output', 1, stop, error_log);
                                         end;   {case}
                                    end;
                            end;
                        wanted_trace.tickdone;
                     end;

                  within_distance : begin
                        {The existing procedure for checking maximum distance in the distance_array object
                         is dyad-year specific.  Adding a new procedure to check whether *any* dyad-year in
                         the user-specified span of years is within the maximum distance allowed would create
                         a potentially redundant, time-consuming process.  Consequently, this case merely
                         repeats the code found under the "all dyads" option, and relies on the distance check
                         in want_in_year to weed out unwanted dyad-years.}
                        for ccode1 := min_ccode to max_ccode do
                         if (nation_list.is_a_state_between (ccode1,
                              user_selections.first_year, user_selections.last_year)) then
                          for ccode2 := min_ccode to max_ccode do
                            if nation_list.is_a_state_between (ccode2,
                             user_selections.first_year, user_selections.last_year) then
                             begin
                                wanted_trace.tick ('Executing Procedure: Create Dyad List (All States)', 40000);
                                {check and make sure want this re: i vs. i; make sure they overlap.}
                                if ((nation_list.different_states(ccode1, ccode2)) or (user_selections.output_format.printii=true)) then
                                  if nation_list.country_date_overlap (ccode1, ccode2) then
                                    begin
                                       {if want directed, then add either direction.  If want
                                        non-directed, add only cc1 < cc2 direction}
                                       case user_selections.output_this of
                                         output_nondirected_dyads : if ccode1 <= ccode2 then add_new_rec (ccode1, ccode2);
                                         output_directed_dyads : add_new_rec (ccode1, ccode2);
                                         else EUGeneError ('Called wanted_dyad_list.update but dyads not selected for output', 1, stop, error_log);
                                         end;   {case}
                                    end;
                             end;
                         wanted_trace.tickdone;
                     end;


                  within_region : begin
                        {here, create wanted_dyads within user-specified regions}
                        for ccode1 := min_ccode to max_ccode do
                        for ccode2 := min_ccode to max_ccode do
                          if {Condition 1: the dyad exists in the specified time span...}
                             (nation_list.is_a_state_between (ccode1,
                              user_selections.first_year, user_selections.last_year) and
                              nation_list.is_a_state_between (ccode2,
                              user_selections.first_year, user_selections.last_year))
                              and (     {...Condition 2: either both states are members of the specified regions...}
                               ((nation_list.get_home_region(ccode1) in user_selections.selected_regions) and
                               (nation_list.get_home_region(ccode2) in user_selections.selected_regions))
                                 or     {...the user specified all regions (the default condition)...}
                               (globe in user_selections.selected_regions)
                               )
                               then
                            begin
                                wanted_trace.tick ('Executing Procedure: Create Dyad List (GPs vs All Others',0);
                                if (nation_list.different_states(ccode1, ccode2)) or (user_selections.output_format.printii=true) then
                                  if nation_list.country_date_overlap (ccode1, ccode2) then
                                    begin
                                       {if want directed, then add either direction.  If want
                                        non-directed, add only cc1 < cc2 direction}
                                       case user_selections.output_this of
                                         output_nondirected_dyads : if ccode1 <= ccode2 then add_new_rec (ccode1, ccode2);
                                         output_directed_dyads : add_new_rec (ccode1, ccode2);
                                         else EUGeneError ('Called wanted_dyad_list.update but dyads not selected for output', 1, stop, error_log);
                                         end;   {case}
                                    end;
                            end;
                        wanted_trace.tickdone;
                     end;

                  selected_set : begin
                        {now, go through selected_country_list to create set of dyads}
                      for ccodeloop1 := 1 to user_selections.selected_country_list.num_countries do
                       if nation_list.is_a_state_between (user_selections.selected_country_list.data[ccodeloop1],
                          user_selections.first_year, user_selections.last_year) then
                        for ccodeloop2 := 1 to user_selections.selected_country_list.num_countries do
                          if nation_list.is_a_state_between (user_selections.selected_country_list.data[ccodeloop2],
                             user_selections.first_year, user_selections.last_year) then
                             begin
                                wanted_trace.tick ('Executing Procedure: Create Dyad List (Selection) ',0);
                                if (user_selections.selected_country_list.data[ccodeloop1] <>
                                    user_selections.selected_country_list.data[ccodeloop2]) or
                                   (user_selections.output_format.printii=true) then
{                                  add_new_rec (user_selections.selected_country_list.data[ccodeloop1],
                                               user_selections.selected_country_list.data[ccodeloop2]);
}                                    begin
                                       {if want directed, then add either direction.  If want
                                        non-directed, add only cc1 < cc2 direction}
                                       case user_selections.output_this of
                                         output_nondirected_dyads : if user_selections.selected_country_list.data[ccodeloop1] <=
                                                                       user_selections.selected_country_list.data[ccodeloop2]
                                            then add_new_rec (user_selections.selected_country_list.data[ccodeloop1], user_selections.selected_country_list.data[ccodeloop2]);
                                         output_directed_dyads : add_new_rec (user_selections.selected_country_list.data[ccodeloop1], user_selections.selected_country_list.data[ccodeloop2]);
                                         else EUGeneError ('Called wanted_dyad_list.update but dyads not selected for output', 1, stop, error_log);
                                         end;   {case}
                                    end;
                             end;
                          wanted_trace.tickdone;
                     end;

                  user_file_read : begin
                         {now process user dyads onto wanted list.  Here, no directed/
                          nondirected check.}
                        current_user_dyad_rec := user_selections.user_specified_dyad_list.first_dyad;
                        while current_user_dyad_rec <> nil do
                           begin
                              wanted_trace.tick ('Executing Procedure: Create Dyad List (User Dyads) ',0);
                              {make sure dyad not already on list in case a duplicate}
                              if not (on_list (current_user_dyad_rec^.ccode1, current_user_dyad_rec^.ccode2)) then
                                 add_new_rec (current_user_dyad_rec^.ccode1, current_user_dyad_rec^.ccode2);
                              current_user_dyad_rec := current_user_dyad_rec^.next_rec;
                           end;
                        wanted_trace.tickdone;
                     end;

                  not_selected : begin
                        showMessage ('Error - called wanted list create with no dyads selected');
                     end;
               end;  {case}
               created := true;
               current := data;    {set current marker to beginning}
            except
               raise;
            end;
         finally
            wanted_trace.tickdone;
            wanted_trace.free;
            trace.message ('Wanted list required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
            trace.exit ('Finished Updating wanted list');
         end;
      end;         {init}

   function Twanted_dyad_list_obj.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then if created=true then initialized := true;
      end;

   procedure Twanted_dyad_list_obj.get_first_dyad (var ccode1, ccode2 : ccode_range; var complete: boolean);
      {It will also return values for dyads as necessary.}
      begin
         if not(initialized) then
            begin
               EUGeneError ('Wanted List get first called before initialization',
                               5, stop, error_log);
            end;
         current := data;
         if current=nil then complete := true
         else
            begin
               ccode1 := current^.ccode1;
               ccode2 := current^.ccode2;
               complete := false;
            end;
      end;

   procedure Twanted_dyad_list_obj.get_next_dyad  (var ccode1, ccode2 : ccode_range; var complete: boolean);
      begin
         if not(initialized) then
            begin
               EUGeneError ('Wanted List get next called before initialization',
                               5, stop, error_log);
            end;
         if current=nil then complete := true
         else
            begin
               current := current^.next_rec;
               if current=nil then complete := true
               else
                  begin
                     ccode1 := current^.ccode1;
                     ccode2 := current^.ccode2;
                     complete := false;
                  end;
            end;
      end;   {proc Wanted_List.get_next_dyad}

   function Twanted_dyad_list_obj.get_num_dyad_years : longint;
      begin
             {this will probably never be < 0, but if they make an odd selection
              of countries and year ranges it could be, if they are countries
              with multiple start, end dates.  I'm just not bothering to change
              the code above since it's not worth the time.  }
         if num_dyad_years < 0 then get_num_dyad_years := 0
         else get_num_dyad_years := num_dyad_years;
      end;

   function Twanted_dyad_list_obj.get_num_dyads : longint;
      begin
         if num_dyads < 0 then get_num_dyads := 0
         else get_num_dyads := num_dyads;
      end;

  
      {  ---------------------------------------------------------   }

    {methods for risk attitude array - new version}
constructor Trisk_attitude_array_obj_v2.init (eugene_risk_file_name, WTR_risk_file_name :
               TFileName; year1, year2 : year_range; raw_data_source : risk_in_data_type);
         {init reads in from intermediate, external file.  Either Bdm source or Eugene source.}
      var risk_file : risk_file_type_v2;
          risk_file_rec : risk_file_record_type_v2;
          temp_record : risk_file_record_type_v2;
          risk_wtr_file : text;
          risk_ccode : risk_ccode_array_ptr_v2;   {array[ccode_index_range] of ptr to rec; }
          risk_ptr_rec : risk_record_ptr_v2;    {ptr to risk_rec_type}
          year_loop : year_range;
          temp : year_range;
          start_mem, heapneeded : longint;
          region : region_range;
          x : ccode_index_range;
          left, right : longint;
          left_year, right_year : year_range;
          prevyear, num_read : longint;
          want_year : year_range;
          Risk_trace : TTrace_obj;

                                { --------------------------------- }

         Procedure read_wtr_risk;
               {this is not as sophisticated as other, b/c I expect to not have to use it much}
            var year : year_range;
                ccode : ccode_range;
                year_real, ccode_real, risk_eur, risk_Mid, RiskAsi, RiskAme : single;
            begin
               try
                  try
                     trace.message ('Reading WTR Risk data from external file');
                     assignFile (risk_wtr_file, WTR_risk_file_name);
                     reset (risk_wtr_file);
                     risk_file_rec.year := min_year;

                     {first, find location in file I want}
                     {find the beginning section of what needs to be read and processed}
                     {Just loop through until find proper record}
                     repeat
                        readln (risk_wtr_file, year_real, ccode_real, risk_eur, risk_Mid, RiskAsi, RiskAme);
                        Risk_trace.tick ('Executing Procedure: Searching Risk File ',0);
                        year := trunc(year_real);
                     until (year >= first_partition_year) or eof (risk_wtr_file);
                     Risk_trace.tickdone;

                     {Now save this record}
                     ccode := trunc(ccode_real);
                     if (nation_list.is_a_state (ccode, year)) and (year >= first_partition_year) and
                        (year <= last_partition_year) then
                        begin
                           data^[year]^[ccode_index.index(ccode)]^[europe].risk := risk_eur;
                           data^[year]^[ccode_index.index(ccode)]^[middleEast].risk := risk_Mid;
                           data^[year]^[ccode_index.index(ccode)]^[asia].risk := RiskAsi;
                           data^[year]^[ccode_index.index(ccode)]^[americas].risk := RiskAme;
                        end;

                     {Now repeat reading and saving records until eof}
                     num_read := 0;
                     prevyear := 0;
                     while (not eof (risk_wtr_file)) and (year <= last_partition_year) do
                        begin
                           readln (risk_wtr_file, year_real, ccode_real, risk_eur, risk_Mid, RiskAsi, RiskAme);
                           year := trunc(year_real);
                           ccode := trunc(ccode_real);
                           if (nation_list.is_a_state (ccode, year)) and (year >= first_partition_year) and
                              (year <= last_partition_year) then
                              begin
                                 data^[year]^[ccode_index.index(ccode)]^[europe].risk := risk_eur;
                                 data^[year]^[ccode_index.index(ccode)]^[middleEast].risk := risk_Mid;
                                 data^[year]^[ccode_index.index(ccode)]^[asia].risk := RiskAsi;
                                 data^[year]^[ccode_index.index(ccode)]^[americas].risk := RiskAme;
                              end;
                           Risk_trace.tick ( 'Executing Procedure: Read Risk Data '+inttostr(self.first_partition_year)+
                               ' to '+inttostr(self.last_partition_year),
                               (self.last_partition_year - self.first_partition_year+1));
                           if year <> prevyear then
                              begin
                                 prevyear := year;
                                 inc(num_read);
                              end;
                        end;
                     Risk_trace.tickdone;

                  finally
                     close (risk_wtr_file);
                  end;
               except
               on EUserInterrupt do raise;
               on EInOutError do
                 begin
                    FileErrorBox.maindo ('Error opening file "'+wtr_risk_file_name+ '"',
                                         'File could not be opened for input.',
                                         'File may be in use by another program, or may be missing.');
                    FileErrorBox.showmodal;
                    raise;
                 end;
               end;
            end;     {Proc read WTR risk;}

                                { --------------------------------- }

         Procedure read_eugene_risk;
            var x : integer;
                old_index_number, new_index_number : ccode_index_range;
            begin
               try
                  try
                     trace.message ('Reading EUGENE Risk data from external file');
                     assignFile (risk_file, eugene_risk_file_name);
                     reset (risk_file);
                     risk_file_rec.year := min_year;

                     {first, find location in file I want}
                     {find the beginning section of what needs to be read and processed}
                     {binary search for any value of year < first.  Then go on to sequential.}
                     read (risk_file, risk_file_rec);
                     if risk_file_rec.year < first_partition_year then  {not at first record, so search}
                       begin
                          want_year := first_partition_year - 1;  {need to find some record that
                              is at most one year before what I want here.  }
                          {file starts at position 0}
                          left := 0;
                              {find the leftmost and rightmost years}
                          right := FileSize (risk_file)-1;
                          seek (risk_file, left);
                          read (risk_file, temp_record);
                          left_year := temp_record.year;
                          seek (risk_file, right);
                          read (risk_file, temp_record);
                          right_year := temp_record.year;

                          repeat
                                {left and right are file positions.  Guess how far between l and r
                                 the desired year will fall}
                                 {pure binary search would be
                                  seek (tau_file, ((left + right) div 2));  }
                                 {Improve on that by moving closer to where year should be.}
                             seek (risk_file, trunc(left+(right-left) * ((want_year-left_year) /
                                                                (right_year-left_year+1)) ));
                             Risk_trace.tick ( 'Executing Procedure: Searching Risk File ',0);
                             read (risk_file, risk_file_rec);
                             if risk_file_rec.year < want_year then    {want to search in right half of file}
                                begin
                                   left := FilePos(risk_file);
                                   temp_record := risk_file_rec;
                                   left_year := temp_record.year;
                                end
                             else     {value read was correct, or was more than what I want,
                                       so need to search left half of file}
                                begin
                                   right := FilePos(risk_file);
                                   temp_record := risk_file_rec;
                                   right_year := temp_record.year;
                                end;
                          until (risk_file_rec.year = want_year) or (left > right);
                          {when exit here, file is positioned to year just before where I want it}
                       end;
                     Risk_trace.tickdone;

                          {Now am one year before; get to first record of year I want}
                     while (risk_file_rec.year < first_partition_year) and (not eof(risk_file)) do
                        begin
                           read (risk_file, risk_file_rec);
                           Risk_trace.tick ( 'Executing Procedure: Searching Risk File ',0);
                        end;
                     Risk_trace.tickdone;

                     {have one record in memory.  Possibly store it before continuing read}
                     if (risk_file_rec.year >= self.first_partition_year) and
                        (risk_file_rec.year <= self.last_partition_year) then
                        begin
                           if ccode_index.identical_ccode_lists(risk_file_rec.ccode_from_index_list) then
                              for x := min_ccode_index to max_ccode_index do
                                 data^[risk_file_rec.year]^[x]^ := risk_file_rec.ccode_array[x]
                           else
                              for old_index_number := min_ccode_index to max_ccode_index do
                                 begin
                                    new_index_number := ccode_index.index(risk_file_rec.ccode_from_index_list[old_index_number]);
                                       if new_index_number <> initialized_value then
                                          data^[risk_file_rec.year]^[new_index_number]^ := risk_file_rec.ccode_array[old_index_number];
                                    end;

                        end;


                     num_read := 0;
                     prevyear := 0;
                     while (not eof (risk_file)) and (risk_file_rec.year <= self.last_partition_year) do
                        begin
                           Risk_trace.tick ( 'Executing Procedure: Read Risk Data, '+inttostr(self.first_partition_year)+
                               ' to '+inttostr(self.last_partition_year),
                               (self.last_partition_year - self.first_partition_year+1));
                           read (risk_file, risk_file_rec);
                           if (risk_file_rec.year >= self.first_partition_year) and
                              (risk_file_rec.year <= self.last_partition_year) then
                              begin
                                 if ccode_index.identical_ccode_lists(risk_file_rec.ccode_from_index_list) then
                                    for x := min_ccode_index to max_ccode_index do
                                       data^[risk_file_rec.year]^[x]^ := risk_file_rec.ccode_array[x]
                                 else
                                    for old_index_number := min_ccode_index to max_ccode_index do
                                       begin
                                          new_index_number := ccode_index.index(risk_file_rec.ccode_from_index_list[old_index_number]);
                                             if new_index_number <> initialized_value then
                                                data^[risk_file_rec.year]^[new_index_number]^ := risk_file_rec.ccode_array[old_index_number];
                                          end;
                              end;
                           if risk_file_rec.year <> prevyear then
                              begin
                                 prevyear := risk_file_rec.year;
                                 inc(num_read);
                              end;
                        end;   {while not eof}

                  finally
                     CloseFile (risk_file);
                  end;
               except
                  on EUserInterrupt do raise;
                  on EInOutError do
                    begin
                       FileErrorBox.maindo ('Error opening file "'+eugene_risk_file_name+ '"',
                                            'File could not be opened for input.',
                                            'File may be in use by another program, or may be missing.');
                       FileErrorBox.showmodal;
                       raise;
                    end;
               end;
            end;     {Proc read eugene risk;}

                                { --------------------------------- }

      begin          {main init constructor}
         try
            start_mem := memavail;
            if year1 > year2 then switch_year(year1, year2);
            trace.enter('Initializing risk data, '+inttostr(year1)+' to '+inttostr(year2));
            if not(ccode_index.initialized) then
               begin
                  EUGeneError ('Risk array creation called before ccode_index initialized',
                                  5, stop, error_log);
               end;
            if not(nation_list.initialized) then
               begin
                  EUGeneError ('Risk array creation called before nation_list initialized',
                                  5, stop, error_log);
               end;

            self.first_partition_year := year1;
            self.last_partition_year := year2;
            Risk_trace := nil;
            Risk_trace := TTrace_obj.init(trace.get_trace_level);

            {intialize data}
            trace.message ('Initializing Risk arrays');
            heapneeded := TRisk_Attitude_Array_obj_mem_overhead +
                          ((last_partition_year - first_partition_year + 1) *
                            TRisk_attitude_array_obj_mem_per_year );
            if debug[4] then
               begin
                  trace.message ('Risk array size calculation');
                  trace.message ('Risk one year size = '+inttostr(TRisk_attitude_array_obj_mem_per_year));
                  trace.message ('Calc is that '+inttostr(heapneeded)+' needed for all years.');
                  trace.message ('Max avail mem block is '+inttostr(MaxAvail));
               end;
            if MaxAvail <= (heapneeded) then
               begin
                  EUGeneError ('Not enough memory for risk array. ',
                                  5, stop, error_log);
               end;
            new (data);
            for year_loop := min_year to max_year do
               begin
                  if (year_loop >= self.first_partition_year) and (year_loop <= self.last_partition_year) then
                  begin
                     new(risk_ccode);
                     data^[year_loop] := risk_ccode;
                        {in initialization, don't check for if is a state, just
                         initialize to start value}
                     for x := min_ccode_index to max_ccode_index do
                        begin
                           new (risk_ptr_rec);
                           data^[year_loop]^[x] := risk_ptr_rec;
                           for region := europe to globe do
                              begin
                                 data^[year_loop]^[x]^[region].risk := missing_value;
                                 data^[year_loop]^[x]^[region].security := missing_value;
                                 data^[year_loop]^[x]^[region].secmaxsum := missing_value;
                                 data^[year_loop]^[x]^[region].secminsum := missing_value;
                              end;
                        end;
                  end  {if year in range}
                  else data^[year_loop] := nil;
               end;    {for year_loop}

            case raw_data_source of
               risk_WTR : read_wtr_risk;
               risk_EUGENE : read_eugene_risk;
            end;   {case}

            Risk_Trace.tickdone;
            created := true;

         finally
            Risk_Trace.free;
            trace.message ('Risk array required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
            trace.exit ('Finished initializing risk data');
         end;
      end;

      {  -------------------------------------------------   }

   destructor Trisk_attitude_array_obj_v2.destroy;
      var year_loop : year_range;
          x : ccode_index_range;
      begin
         try
            if self <> nil then
            begin
               for year_loop := min_year to max_year do
                 if data^[year_loop] <> nil then
                    begin
                      for x := min_ccode_index to max_ccode_index do
                         begin
                            dispose (data^[year_loop]^[x]);
                            data^[year_loop]^[x] := nil;
                         end;
                       dispose (data^[year_loop]);
                       data^[year_loop] := nil;
                    end;
              if data <> nil then dispose (data);
              created := false;
              inherited destroy;
            end;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;
      end;

      {  -------------------------------------------------   }

   function Trisk_attitude_array_obj_v2.get_first_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Risk get called before initialization',
                               5, stop, error_log);
            end
         else
            get_first_partition_year := first_partition_year;
      end;

      {  -------------------------------------------------   }

   function Trisk_attitude_array_obj_v2.get_last_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('Risk get called before initialization',
                               5, stop, error_log);
            end
         else
            get_last_partition_year := last_partition_year;
      end;

      {  -------------------------------------------------   }

   function Trisk_attitude_array_obj_v2.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then if created=true then initialized := true;
      end;

      {  -------------------------------------------------   }

   function Trisk_attitude_array_obj_v2.all_in_range (ccode : ccode_range; ayear : year_range;
            aregion : region_type; error_check: boolean): boolean;
      begin
         all_in_range := true;
         if (ccode=0) or (aregion=none) then
            all_in_range := false
         else
         if not(initialized) then
            begin
               all_in_range := false;
               if error_check then
                  EUGeneError ('Get a value from Risk array called before initialization',
                                     5, stop, error_log);
            end
         else
         if not ((ayear >= get_first_partition_year) and (ayear <= get_last_partition_year)) then
            begin
               all_in_range := false;
               if error_check then
                  begin
                     EUGeneError ('Internal Error in program - called Get a value from Risk array with year outside partition',
                                     5, continue, error_log);
                     trace.message ('risk value set to missing');
                  end;
            end
         else
            if not (nation_list.is_a_state(ccode, ayear)) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        trace.message ('ccode/year Error - called Get a value from Risk array for invalid ccode given year'+inttostr(ccode)+' in '+inttostr(ayear));
                        trace.message ('risk value set to missing');
                     end;
               end
         else
            if not ((aregion >= europe) and (aregion <= globe)) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        trace.message ('Region Error - called Get a value from Risk array for invalid region for ccode '+inttostr(ccode)+' in '+inttostr(ayear));
                        trace.message ('risk value set to missing');
                     end;
               end;
      end;

      {  -------------------------------------------------   }

   function Trisk_attitude_array_obj_v2.get_risk (ccode : ccode_range; year : year_range;
                                        aregion : full_region_range) : single;
      begin
         {if not called with any error_check parameter, then want to check errors as usual.
          So pass into the all_in_range procedure a value of "true" to force error checking, and it
          will stop executing when it hits a problem.}
         get_risk := missing_value;
         if all_in_range (ccode, year, aregion, true) then
            get_risk := data^[year]^[ccode_index.index(ccode)]^[aregion].risk;
      end;

      {  -------------------------------------------------   }

   function Trisk_attitude_array_obj_v2.get_risk (ccode : ccode_range; year : year_range;
                   aregion : full_region_range;  error_check : boolean) : single; 
      begin
         {Here, called with a value to tell it whether to check for errors.  Pass this into the all_in_range
          procedure;  If error_check comes in false, then it
          will not stop executing when it hits a problem, but will just return a missing value}
         get_risk := missing_value;
         if all_in_range (ccode, year, aregion, error_check) then
            get_risk := data^[year]^[ccode_index.index(ccode)]^[aregion].risk;
      end;

      {  -------------------------------------------------   }

   function Trisk_attitude_array_obj_v2.get_security (ccode : ccode_range; year : year_range; aregion : full_region_range;  error_check : boolean) : single;
      begin
         get_security := missing_value;
         if all_in_range (ccode, year, aregion, error_check) then
            get_security := data^[year]^[ccode_index.index(ccode)]^[aregion].security;
      end;

      {  -------------------------------------------------   }

   function Trisk_attitude_array_obj_v2.get_secmax (ccode : ccode_range; year : year_range; aregion : full_region_range;  error_check : boolean) : single;
      begin
         get_secmax := missing_value;
         if all_in_range (ccode, year, aregion, error_check) then
            get_secmax := data^[year]^[ccode_index.index(ccode)]^[aregion].secmaxsum;
      end;

      {  -------------------------------------------------   }

   function Trisk_attitude_array_obj_v2.get_secmin (ccode : ccode_range; year : year_range; aregion : full_region_range;  error_check : boolean) : single;
      begin
         get_secmin := missing_value;
         if all_in_range (ccode, year, aregion, error_check) then
            get_secmin := data^[year]^[ccode_index.index(ccode)]^[aregion].secminsum;
      end;

      {  ---------------------------------------------------------   }

      function Trisk_attitude_array_obj_v2.get_uncertainty (year : year_range; aregion : full_region_range;  error_check : boolean) : single;
         {uncertainty is the variance in risk scores in a given year.
          Note:  These risk scores should apparently be the UNTRANSFORMED risk scores.  }
      var ccode : ccode_range;
          dataArray : array[1..max_countries] of double;
          num_dat, x : integer;
          variance, sum, sumsq, mean : double;
          uppercase_risk : single;
      begin
         get_uncertainty := missing_value;  {this will hold up even if called with region none}
         if aregion <> none then
         begin
            num_dat := 0;
            sum := 0;
            for ccode := min_ccode to max_ccode do
               {Check whether to add in this ccode's risk scores.  Since many countries in loop will be
                invalid, don't stop if an error in the check is encountered.  This avoidance of the
                check will also make the procedure work when called for ccodes and years outside the
                range of the risk data.}
               if all_in_range (ccode, year, aregion, dont_stop_on_error) then
                  begin
                     uppercase_risk := get_risk (ccode, year, aregion);
                     if not (uppercase_risk = missing_value) then
                       begin
                         inc(num_dat);
                         dataArray[num_dat] := uppercase_risk;
                         sum := sum + dataArray[num_dat];
                       end;
                  end;
            if num_dat > 0 then
              begin
                  {Use Schmidt 1979:105 formula ]
                  {first, mean}{2nd sum squares}
                  mean := sum / num_dat;
                  sumsq := 0;
                  for x := 1 to num_dat do
                     sumsq := sumsq + (dataArray[x]-mean) * (dataArray[x]-mean);
                  variance := sumsq / num_dat;
                  get_uncertainty := variance;
              end;
         end;
      end;

      {  ---------------------------------------------------------   }

   Function TRisk_Attitude_array_obj_mem_overhead : longint;
      begin
         TRisk_Attitude_array_obj_mem_overhead := sizeof( Trisk_attitude_array_obj_v2 ) +
            sizeof (risk_year_array_ptr_v2) + sizeof(risk_year_array_v2) + 2*sizeof(year_range) +
            sizeof(boolean);
      end;

   Function TRisk_Attitude_array_obj_mem_per_year : longint;
      begin
         TRisk_Attitude_array_obj_mem_per_year := sizeof (risk_ccode_array_v2) +
            ((max_ccode_index-min_ccode_index+1) *
              (sizeof(risk_record_ptr_v2) + sizeof(risk_record_type_v2)));
      end;

      {  ---------------------------------------------------------   }

   {Methods for stored security information (best and worst alliance data) }

constructor Trisk_stored_security_alliance_obj.init (security_alliance_file_name : TFileName;
                        year1, year2 : year_range);
      var ayear : year_range;
          aregion, region : region_range;
          x, y : ccode_index_range;
          ccodex, ccodey : ccode_range;
          sec_trace : TTrace_obj;
          start_mem, heapneeded : longint;
          sec_alliance : sec_alliance_ccode_array_ptr;
          sec_country_alliance : sec_alliance_one_country_year_ptr;
          left, right : longint;
          left_year, right_year : year_range;
          prevyear, num_read : longint;

   procedure read_existing_records_of_alliance_security_data_from_file (security_alliance_file_name:TFileName);
      var x, y : ccode_index_range;
          aregion : region_range;
          old_index_number1, new_index_number1 : ccode_index_range;
          old_index_number2, new_index_number2 : ccode_index_range;
          sec_file : security_alliance_file_type;
          sec_file_rec, temp_record : ^security_alliance_file_record_type;
          want_year : year_range;
      {This will read any years of data in the partition range from this file.
       If the file ends, it won't read the subsequent (non-existant!) years.
       For this reason, the data initialization must be done right!}

      begin
         try
            assignFile (sec_file, security_alliance_file_name);
            sec_trace.message ('Reading Alliance security Risk data from external file');
            new (sec_file_rec);
            new (temp_record);

            reset (sec_file);
            sec_file_rec^.year := min_year;

            {first, find location in file I want}
            {binary search for any value of year < first.  Then go on to sequential.}
            read (sec_file, sec_file_rec^);
            if sec_file_rec.year < first_partition_year then  {not at first record, so search}
              begin
                 want_year := first_partition_year - 1;  {need to find some record that
                     is at most one year before what I want here.  }
                 {file starts at position 0}
                 left := 0;
                     {find the leftmost and rightmost years}
                 right := FileSize (sec_file)-1;
                 seek (sec_file, left);
                 read (sec_file, temp_record^);
                 left_year := temp_record^.year;
                 seek (sec_file, right);
                 read (sec_file, temp_record^);
                 right_year := temp_record^.year;

                 repeat
                       {left and right are file positions.  Guess how far between l and r
                        the desired year will fall}
                    seek (sec_file, trunc(left+(right-left) * ((want_year-left_year) /
                                                       (right_year-left_year+1)) ));
                    Sec_trace.tick ( 'Executing Procedure: Searching Security File ',0);
                    read (sec_file, sec_file_rec^);
                    if sec_file_rec.year < want_year then    {want to search in right half of file}
                       begin
                          left := FilePos(sec_file);
                          temp_record^ := sec_file_rec^;
                          left_year := temp_record^.year;
                       end
                    else     {value read was correct, or was more than what I want,
                              so need to search left half of file}
                       begin
                          right := FilePos(sec_file);
                          temp_record^ := sec_file_rec^;
                          right_year := temp_record^.year;
                       end;
                 until (sec_file_rec^.year = want_year) or (left > right);
                 {when exit here, file is positioned to year just before where I want it}
              end;
            Sec_trace.tickdone;

                 {Now am one year before; get to first record of year I want}
            while (sec_file_rec.year < first_partition_year) and (not eof(sec_file)) do
               begin
                  read (sec_file, sec_file_rec^);
                  sec_trace.tick ( 'Executing Procedure: Searching security File ',0);
               end;
            sec_trace.tickdone;

            {have one record in memory.  Possibly store it before continuing read}
            if (sec_file_rec^.year >= self.first_partition_year) and
               (sec_file_rec^.year <= self.last_partition_year) then
               begin
                  {If this is a record in range, then save the data.
                   If out of range, skip the data.  }
                  if ccode_index.identical_ccode_lists(sec_file_rec^.ccode_from_index_list) then
                     for x := min_ccode_index to max_ccode_index do
                        data[sec_file_rec^.year]^[x]^ := sec_file_rec^.ccode_array[x]
                  else
                     for old_index_number1 := min_ccode_index to max_ccode_index do
                        begin
                           new_index_number1 := ccode_index.index(sec_file_rec^.ccode_from_index_list[old_index_number1]);
                           if new_index_number1 <> initialized_value then
                              for aregion := europe to globe do
                                 for old_index_number2 := min_ccode_index to max_ccode_index do
                                 begin
                                    new_index_number2 := ccode_index.index(sec_file_rec^.ccode_from_index_list[old_index_number2]);
                                    if new_index_number2 <> initialized_value then
                                       begin
                                          data[sec_file_rec^.year]^[new_index_number1]^[aregion].best_alliance[new_index_number2] :=
                                             sec_file_rec^.ccode_array[old_index_number1][aregion].best_alliance[old_index_number2];
                                          data[sec_file_rec^.year]^[new_index_number1]^[aregion].worst_alliance[new_index_number2] :=
                                             sec_file_rec^.ccode_array[old_index_number1][aregion].worst_alliance[old_index_number2];
                                       end;

                                 end;
                        end;
                     {that puts all the states that were in the old data into the new.}
               end;


            num_read := 0;
            prevyear := 0;
            while (not eof (sec_file)) and (sec_file_rec.year <= self.last_partition_year) do
               begin
                  sec_trace.tick ( 'Executing Procedure: Read sec Data, '+inttostr(self.first_partition_year)+
                      ' to '+inttostr(self.last_partition_year),
                      (self.last_partition_year - self.first_partition_year+1));
                  read (sec_file, sec_file_rec^);
                  if (sec_file_rec^.year >= self.first_partition_year) and
                     (sec_file_rec^.year <= self.last_partition_year) then
                     begin
                        if ccode_index.identical_ccode_lists(sec_file_rec^.ccode_from_index_list) then
                           for x := min_ccode_index to max_ccode_index do
                              data[sec_file_rec^.year]^[x]^ := sec_file_rec^.ccode_array[x]
                        else
                           for old_index_number1 := min_ccode_index to max_ccode_index do
                              begin
                                 new_index_number1 := ccode_index.index(sec_file_rec^.ccode_from_index_list[old_index_number1]);
                                 if new_index_number1 <> initialized_value then
                                    for aregion := europe to globe do
                                       for old_index_number2 := min_ccode_index to max_ccode_index do
                                       begin
                                          new_index_number2 := ccode_index.index(sec_file_rec^.ccode_from_index_list[old_index_number2]);
                                          if new_index_number2 <> initialized_value then
                                             begin
                                                data[sec_file_rec^.year]^[new_index_number1]^[aregion].best_alliance[new_index_number2] :=
                                                   sec_file_rec^.ccode_array[old_index_number1][aregion].best_alliance[old_index_number2];
                                                data[sec_file_rec^.year]^[new_index_number1]^[aregion].worst_alliance[new_index_number2] :=
                                                   sec_file_rec^.ccode_array[old_index_number1][aregion].worst_alliance[old_index_number2];
                                             end;

                                       end;

                                 {outputWindow.Screen_Output.lines.add('old index '+inttostr(old_index_number1) +
                                    ' was ccode '+inttostr(sec_file_rec^.ccode_from_index_list[old_index_number1]) +
                                    ' is new index ' + inttostr(new_index_number1) + ' is new ccode ' + inttostr(ccode_index.ccode(new_index_number1))
                                                                     );}
                              end;
                     end;
                  if sec_file_rec^.year <> prevyear then
                     begin
                        prevyear := sec_file_rec^.year;
                        inc(num_read);
                     end;
               end;   {while not eof}

         finally
            CloseFile (sec_file);
            dispose (sec_file_rec);
            dispose (temp_record);
         end;
      end;   {procedure read alliance sec data from file}

      { --------------------------- }

      begin    {main procedure Stored Security Alliance Obj .init}
         try
            try
               start_mem := memavail;
               if year1 > year2 then switch_year(year1, year2);
               trace.enter('Initializing security alliance data, '+inttostr(year1)+' to '+inttostr(year2));
               if not(ccode_index.initialized) then
                  begin
                     EUGeneError ('Security alliance array creation called before ccode_index initialized',
                                     5, stop, error_log);
                  end;
               if not(nation_list.initialized) then
                  begin
                     EUGeneError ('Risk array creation called before nation_list initialized',
                                     5, stop, error_log);
                  end;

               first_partition_year := year1;
               last_partition_year := year2;

               sec_trace := nil;
               sec_trace := TTrace_obj.init(trace.get_trace_level);
               trace.message ('Initializing Security alliance array');
               heapneeded := TSec_Array_obj_mem_overhead +
                             ((last_partition_year - first_partition_year + 1) *
                               TSec_array_obj_mem_per_year );
               if debug[4] then
                  begin
                     trace.message ('Security alliance array size calculation');
                     trace.message ('Security one year size = '+inttostr(TSec_array_obj_mem_per_year));
                     trace.message ('Calc is that '+inttostr(heapneeded)+' needed for all years.');
                     trace.message ('Max avail mem block is '+inttostr(MaxAvail));
                  end;
               if MaxAvail <= (heapneeded) then
                  begin
                     EUGeneError ('Not enough memory for security array. ', 5, stop, error_log);
                  end;


               {initialize data}
               for ayear := min_year to max_year do
                  begin
                     if (ayear >= self.first_partition_year) and (ayear <= self.last_partition_year) then
                     begin
                        new(sec_alliance);
                        data[ayear] := sec_alliance;
                        {in first part of initialization, don't check for if is a state,
                         just initialize to initialized start value}
                        for x := min_ccode_index to max_ccode_index do
                           begin
                              new (sec_country_alliance);
                              data[ayear]^[x] := sec_country_alliance;
                              for region := europe to globe do
                                 for y := min_ccode_index to max_ccode_index do
                                 begin
                                    data[ayear]^[x]^[region].best_alliance[y] := initialized_value;
                                    data[ayear]^[x]^[region].worst_alliance[y] := initialized_value;
                                 end;
                           end;
                     end  {if year in range}
                        else data[ayear] := nil;
                  end;    {for ayear.  Data now initialized to all -1s.}

               {It is not enough to leave data at -1s.  Must set valid ccode-ccode
                pairs to no alliance, because there might not be a file of external
                security data to read, or if there is, it might only contain some
                years of data.  If only some data are there, the rest of the data
                must be set to no alliance for valid ccode pairs.  }

                  {set security_alliance_data to all no alliance}
               for ayear := min_year to max_year do
                  begin
                     if (ayear >= self.first_partition_year) and (ayear <= self.last_partition_year) then
                     begin
                        {in initialization, don't check for if is a state, just initialize to start value}
                        for ccodex := min_ccode to max_ccode do
                           if nation_list.is_a_state (ccodex, ayear) then
                              for ccodey := min_ccode to max_ccode do
                                 if nation_list.is_a_state (ccodey, ayear) then
                                 for region := europe to globe do
                                    begin
                                       data[ayear]^[ccode_index.index(ccodex)]^[region].best_alliance[ccode_index.index(ccodey)] := no_alliance;
                                       data[ayear]^[ccode_index.index(ccodex)]^[region].worst_alliance[ccode_index.index(ccodey)] := no_alliance;
                                    end;
                     end;  {if year in range}
                  end;    {for ayear.  Data now initialized to no alliance, or read from file.}

               {Now read data if it exists.}
               if fileexists (security_alliance_file_name) then
                  read_existing_records_of_alliance_security_data_from_file (security_alliance_file_name)
                  {The read will overwrite those records with any data from sec file}
               else
                  begin
                    trace.message ('Security alliance data not found - setting best/worst security alliances to all no alliance');
                    {showmessage ('Security alliance data not found - setting best/worst security alliances to all no alliance');}
                  end;


            except    {main outside except}
               on EUserInterrupt do raise;
               on EInOutError do
                 begin
                    FileErrorBox.maindo ('Error opening file "'+security_alliance_file_name+ '"',
                                         'File could not be opened for input.',
                                         'File may be in use by another program, or may be missing.');
                    FileErrorBox.showmodal;
                    raise;
                 end;
            end; {main outside except}

         finally
            Sec_Trace.tickdone;
            sec_Trace.free;
            trace.message ('Security array required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
            trace.exit ('Finished initializing security alliance data');
            created := true;
         end;    {main outside finally}

      end;   {init proc}

      {  -------------------------------------------------   }

   destructor Trisk_stored_security_alliance_obj.destroy;
      var year_loop : year_range;
          x : ccode_index_range;
      begin
         created := false;
         try
            if self <> nil then
            begin
               for year_loop := min_year to max_year do
                 if data[year_loop] <> nil then
                    begin
                      for x := min_ccode_index to max_ccode_index do
                         begin
                            dispose (data[year_loop]^[x]);
                            data[year_loop]^[x] := nil;
                         end;
                       dispose (data[year_loop]);
                       data[year_loop] := nil;
                    end;
              created := false;
              inherited destroy;
            end;
         except
            {If we get here, there has been an error in the deconstruction of the object}
            ShowMessage ('There has been an error freeing up memory due to a program interrupt.'+
                         'To ensure maximum memory availability, you may wish to exit EUGene and re-run it.');
         end;

      end;

      {  -------------------------------------------------   }

   procedure Trisk_stored_security_alliance_obj.write_new_data (security_alliance_file_name : TFileName;
                        year1, year2 : year_range);
      var sec_file : security_alliance_file_type;
          sec_trace : TTrace_obj;
          sec_file_rec, temp_record : ^security_alliance_file_record_type;
          want_year : year_range;
          left, right, want_position : longint;
          left_year, right_year, outyear : year_range;
          x : ccode_index_range;

      begin
         try
            sec_trace := nil;
            sec_trace := TTrace_obj.init(trace.get_trace_level);
            sec_trace.message ('Writing new alliance security Risk data to external file');
            assignFile (sec_file, security_alliance_file_name);
            if fileexists (security_alliance_file_name) then
               begin
                  reset (sec_file);
                  new (sec_file_rec);
                  new (temp_record);

                  {first, find location in file I want}

                     {find the leftmost and rightmost years}
                     {file starts at position 0}
                  left := 0;
                  right := FileSize (sec_file)-1;
                  seek (sec_file, left);
                  read (sec_file, sec_file_rec^);
                  left_year := sec_file_rec^.year;
                  seek (sec_file, right);
                  read (sec_file, temp_record^);
                  right_year := temp_record^.year;

                  {want year 1 position}
                  want_position := left + (year1 - left_year);

                  {Could be appending, or overwriting.
                   If at right of file, then write here.
                   If not, verify writing correct record.}
                  seek (sec_file, want_position);
                  if want_position <= right then  {check record}
                     begin
                        {read that record.  It should be the one I want, but check it.  Then reset just before it.  }
                        read (sec_file, sec_file_rec^);
                        if sec_file_rec^.year <> year1 then
                           EUGeneError ('Year read at target file position when getting ready to write data to security alliance file was incorrect',
                                  1, continue, error_log)
                        else
                           begin
                              {now, reset to position to write this data.  }
                              seek (sec_file, want_position);
                              {Now ready for output}
                           end;
                     end;   {want position <= right}
                  dispose (sec_file_rec);
                  dispose (temp_record);

               end              {file exists}
            else
               begin             {file does not exist, create it for writing.}
                  rewrite (sec_file);
               end;

            {Now, either file exists and I found correct spot in file,
             or I've created a new file for writing.  So now write the data.}
            for outyear := year1 to year2 do
               begin
                  {clear new record}
                  Sec_trace.tick ( 'Executing Procedure: writing new records to security File ',0);
                  new (sec_file_rec);
                  sec_file_rec^.year := outyear;
                  sec_file_rec^.ccode_from_index_list := ccode_index.return_ccode_list;
                  for x := min_ccode_index to max_ccode_index do
                     sec_file_rec^.ccode_array[x] := data[sec_file_rec^.year]^[x]^;

                  write (sec_file, sec_file_rec^);
                  dispose (sec_file_rec);
               end;

            sec_trace.tickdone;

         finally
            CloseFile (sec_file);
            Sec_Trace.tickdone;
            sec_Trace.free;
         end;

      end;

      {  -------------------------------------------------   }

   procedure Trisk_stored_security_alliance_obj.set_new_best_alliance_data
      (ccode1, ccode2: ccode_range; year : year_range; region : region_range; new_alliance_value : alliance_value_type);
        {sets new data for a ccode-ccode-year-region.}
      begin
         if not(initialized) then
               EUGeneError ('Security set called before initialization', 5, stop, error_log)
         else if all_in_range (ccode1, year) and all_in_range (ccode2, year) then
            data[year]^[ccode_index.index(ccode1)]^[region].best_alliance[ccode_index.index(ccode2)] := new_alliance_value
         else
            begin
               EUGeneError ('Tried to set security data for data not in range; setting to initialized_value', 5, continue, error_log);
               data[year]^[ccode_index.index(ccode1)]^[region].best_alliance[ccode_index.index(ccode2)] := initialized_value;
            end;
      end;

      {  -------------------------------------------------   }

   procedure Trisk_stored_security_alliance_obj.set_new_worst_alliance_data
      (ccode1, ccode2: ccode_range; year : year_range; region : region_range; new_alliance_value : alliance_value_type);
        {sets new data for a ccode-ccode-year-region.}
      begin
         if not(initialized) then
               EUGeneError ('Security set called before initialization', 5, stop, error_log)
         else if all_in_range (ccode1, year) and all_in_range (ccode2, year) then
            data[year]^[ccode_index.index(ccode1)]^[region].worst_alliance[ccode_index.index(ccode2)] := new_alliance_value
         else
            begin
               EUGeneError ('Tried to set security data for data not in range; setting to initialized_value', 5, continue, error_log);
               data[year]^[ccode_index.index(ccode1)]^[region].worst_alliance[ccode_index.index(ccode2)] := initialized_value;
            end;
      end;

      {  -------------------------------------------------   }

   function Trisk_stored_security_alliance_obj.get_first_partition_year : year_range;
      begin
         if not(initialized) then
               EUGeneError ('Security get called before initialization', 5, stop, error_log)
         else
            get_first_partition_year := first_partition_year;
      end;

      {  -------------------------------------------------   }

   function Trisk_stored_security_alliance_obj.get_last_partition_year : year_range;
      begin
         if not(initialized) then
               EUGeneError ('Security get called before initialization', 5, stop, error_log)
         else
            get_last_partition_year := last_partition_year;
      end;

      {  -------------------------------------------------   }

   function Trisk_stored_security_alliance_obj.initialized : boolean;
      begin
         if self=nil then
               EUGeneError ('security info called before initialization', 5, stop, error_log)
         else if not(created) then
               EUGeneError ('security info called before initialization', 5, stop, error_log)
         else if created then initialized := true
         else initialized := false;
      end;

      {  -------------------------------------------------   }

   function Trisk_stored_security_alliance_obj.all_in_range (ccode1 : ccode_range; ayear : year_range): boolean;
   {private function}
      begin
         all_in_range := true;
         if not(initialized) then
            begin
               EUGeneError ('Get a value from security array called before initialization',
                               5, stop, error_log);
               trace.message ('value  set to missing');
               all_in_range := false;
            end
         else
         if not ((ayear >= get_first_partition_year) and (ayear <= get_last_partition_year)) then
            begin
               EUGeneError ('Internal Error in program - called Get a value from security array with year outside partition',
                               5, continue, error_log);
               trace.message ('value  set to missing');
               all_in_range := false;
            end
         else
            if not (nation_list.is_a_state(ccode1, ayear)) then
               begin
                  trace.message ('ccode/year Error - called Get a value from security array for invalid ccode given year'+inttostr(ccode1)+' in '+inttostr(ayear));
                  trace.message ('value set to missing');
                  all_in_range := false;
               end;
      end;

      {  -------------------------------------------------   }

   function Trisk_stored_security_alliance_obj.get_best_alliance (ccode1, ccode2: ccode_range; year : year_range; region : full_region_range) : alliance_value_type;
      begin
         get_best_alliance := initialized_value;
         if all_in_range (ccode1, year) and all_in_range (ccode2, year) and (region <> none) then
            get_best_alliance := data[year]^[ccode_index.index(ccode1)]^[region].best_alliance[ccode_index.index(ccode2)];
      end;

   function Trisk_stored_security_alliance_obj.get_worst_alliance (ccode1, ccode2: ccode_range; year : year_range; region : full_region_range) : alliance_value_type;
      begin
         get_worst_alliance := initialized_value;
         if all_in_range (ccode1, year) and all_in_range (ccode2, year) and (region <> none) then
            get_worst_alliance := data[year]^[ccode_index.index(ccode1)]^[region].worst_alliance[ccode_index.index(ccode2)];
      end;

   function Trisk_stored_security_alliance_obj.get_best_alliance_list (ccode1: ccode_range; year : year_range; region : full_region_range) : stored_alliance_list_type;
      var ccode_index_num : ccode_index_range;
      begin
         for ccode_index_num := min_ccode_index to max_ccode_index do
            get_best_alliance_list[ccode_index_num] := initialized_value;
         if all_in_range (ccode1, year) and (region <> none) then
            get_best_alliance_list := data[year]^[ccode_index.index(ccode1)]^[region].best_alliance;
      end;

   function Trisk_stored_security_alliance_obj.get_worst_alliance_list (ccode1: ccode_range; year : year_range; region : full_region_range) : stored_alliance_list_type;
      var ccode_index_num : ccode_index_range;
      begin
         for ccode_index_num := min_ccode_index to max_ccode_index do
            get_worst_alliance_list[ccode_index_num] := initialized_value;
         if all_in_range (ccode1, year) and (region <> none) then
            get_worst_alliance_list := data[year]^[ccode_index.index(ccode1)]^[region].worst_alliance;
      end;

      {  ---------------------------------------------------------   }

   Function TSec_array_obj_mem_overhead : longint;
      begin
         TSec_array_obj_mem_overhead := sizeof( Trisk_stored_security_alliance_obj ) +
            sizeof (sec_alliance_year_array) + 2*sizeof(year_range) +
            sizeof(boolean);
      end;

   Function TSec_array_obj_mem_per_year : longint;
      begin
         TSec_array_obj_mem_per_year := sizeof (sec_alliance_ccode_array) +
            ((max_ccode_index-min_ccode_index+1) *
              (sizeof(sec_alliance_one_country_year_ptr) + sizeof(one_country_year_security_alliance_data)));
      end;

      {  ---------------------------------------------------------   }

      {Methods for EUs using War and reason methods for dyads}

   Function TEUWarReason_array_obj_mem_overhead : longint;
       begin
          TEUWarReason_array_obj_mem_overhead := sizeof(TEUWarReason_array_obj) +
             (max_year-min_year+1)*sizeof(EUWarReason_ccode_ptr) + 2*sizeof(year_range) +
             sizeof(boolean) + sizeof(UtilityArrayType);
            {trace.message ('TEUWarReason_array_obj: '+inttostr(sizeof(TEUWarReason_array_obj)));
            trace.message ('EUWarReason_ccode_ptr: '+inttostr(sizeof(EUWarReason_ccode_ptr)));
            trace.message ('year_range: '+inttostr(sizeof(year_range)));
            trace.message ('boolean: '+inttostr(sizeof(boolean)));
            trace.message ('UtilityArrayType: '+inttostr(sizeof(UtilityArrayType)));
            trace.message ('total: ' + inttostr(result));
            showmessage ('pausing');   }
      end;
   Function TEUWarReason_array_obj_mem_per_year : longint;
       begin
         TEUWarReason_array_obj_mem_per_year := sizeof(EUWarReason_ccode_array_type) +
            (max_ccode_index-min_ccode_index+1) * sizeof(EUWarReason_ccode_array_2);
         {trace.message ('EUWarReason_ccode_array_type: '+inttostr(sizeof(EUWarReason_ccode_array_type)));
         trace.message ('EUWarReason_ccode_array_2: '+inttostr(sizeof(EUWarReason_ccode_array_2)));
         trace.message ('total' + inttostr(result));
         showmessage ('pausing');  }
      end;

      {  ---------------------------------------------------------   }

   constructor TEUWarReason_array_obj.init (a_file_name : TFileName; year1, year2 : year_range);
         {init reads in from intermediate, external file.}
      var ayear : year_range;
          EUWarReason_year : EUWarReason_ccode_ptr;
          EUWarReason_array2 : EUWarReason_ccode_array_ptr2; {array[ccode_index_range] of single;}
          EUWarReason_file : EUWarReason_file_type;
          EUWarReason_record_read, left_record, right_record : EUWarReason_file_record_type;
          want_year : year_range;
          ccode1, ccode2 : ccode_range;
          start_mem, one_year_EUWarReason_memory_needed, heapneeded : longint;
          ccode_index_loop : ccode_index_range;
          left, right : longint;
          num_read, prevyear : longint;
          EU_WarReason_trace : Ttrace_obj;

      begin
         try
            try
               start_mem := memavail;
               if year1 > year2 then
                  switch_year (year1, year2);
               trace.enter('Initializing EUWarReason data, '+inttostr(year1)+' to '+inttostr(year2));
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
                     EUGeneError ('EUWarReason array creation called before nation_list initialized',
                                     5, stop, error_log);
                     halt;
                  end;

               {first initialize the data structure.  Give every legitimate state pair
                the initialized value.}
               EU_WarReason_trace := nil;
               EU_WarReason_trace := TTrace_obj.init(trace.get_trace_level);
               trace.message ('Initializing EUWarReason structure');
               one_year_EUWarReason_memory_needed := TEUWarReason_array_obj_mem_per_year;
               heapneeded := TEUWarReason_array_obj_mem_overhead +
                             (self.last_partition_year - self.first_partition_year + 1) *
                              one_year_EUWarReason_memory_needed;
               if MaxAvail <= (heapneeded) then
                  begin
                     EUGeneError ('Not enough memory for EUWarReason structure. ',
                                     5, stop, error_log);
                  end;
               for ayear := min_year to max_year do
                  if ( (ayear >= self.first_partition_year) and (ayear <= self.last_partition_year) ) then
                     begin
                        new (EUWarReason_year);
                        year_array[ayear] := EUWarReason_year;
                        for ccode_index_loop := min_ccode_index to max_ccode_index do
                           year_array[ayear]^[ccode_index_loop] := nil;
                        for ccode1 := min_ccode to max_ccode do
                           if nation_list.is_a_state (ccode1, ayear) then
                              begin
                                 new (EUWarReason_array2);
                                 year_array[ayear]^[ccode_index.index(ccode1)] := EUWarReason_array2;
                                 for ccode2 := min_ccode to max_ccode do
                                    if nation_list.is_a_state (ccode2, ayear) then
                                       begin
                                          with
                                             year_array[ayear]^[ccode_index.index(ccode1)]^
                                                    [ccode_index.index(ccode2)] do
                                                begin
                                                  UtilityAA := missing_value;
                                                  UtilityAB := missing_value;
                                                  UtilityASQ := missing_value;
                                                  ProbWinAB := missing_value;
                                                  UtilityBB := missing_value;
                                                  UtilityBA := missing_value;
                                                  UtilityBSQ := missing_value;
                                                  ProbWinBA := missing_value;
                                                end;    {with - do}
                                       end;
                              end;
                     end
                  else year_array[ayear] := nil;


               trace.message ('Reading EUWarReason from external file, '+inttostr(first_partition_year)+
                  ' to '+inttostr(last_partition_year));
               assignFile (EUWarReason_file, a_file_name);
               reset (EUWarReason_file);
               read (EUWarReason_file, EUWarReason_record_read);
               {find the beginning section of what needs to be read and processed}
               {binary search for any value of year < first.  Then go on to sequential.}

               if EUWarReason_record_read.year < first_partition_year then  {not at first record, so search}
                 begin
                    want_year := first_partition_year - 1;  {need to find some record that
                        is at most one year before what I want here.  }
                    {file starts at position 0}
                    left := 0;
                        {find the leftmost and rightmost years}
                    right := FileSize (EUWarReason_file)-1;
                    seek (EUWarReason_file, left);
                    read (EUWarReason_file, left_record);
                    seek (EUWarReason_file, right);
                    read (EUWarReason_file, right_record);

                    repeat
                          {left and right are file positions.  Guess how far between l and r
                           the desired year will fall}
                           {pure binary search would be
                            seek (tau_file, ((left + right) div 2));  }
                           {Improve on that by moving closer to where year should be.}
                       if want_year > right_record.year then seek (EUWarReason_file, right) else
                       if want_year < left_record.year then seek (EUWarReason_file, left) else
                       seek (EUWarReason_file, trunc(left+(right-left) * ((want_year-left_record.year) /
                                                          (right_record.year-left_record.year+1)) ));
                       EU_WarReason_trace.tick ('Executing Procedure: Searching EU War Reason File ',0);
                       read (EUWarReason_file, EUWarReason_record_read);
                       if EUWarReason_record_read.year < want_year then    {want to search in right half of file}
                          begin
                             left := FilePos(EUWarReason_file);
                             left_record := EUWarReason_record_read;
                          end
                       else     {value read was correct, or was more than what I want,
                                 so need to search left half of file}
                          begin
                             right := FilePos(EUWarReason_file);
                             right_record := EUWarReason_record_read;
                          end;
                    until (EUWarReason_record_read.year = want_year) or (left > right);
                    {when exit here, file is positioned to year just before where I want it}
                 end;
               EU_WarReason_trace.tickdone;

                    {Now am one year before; get to first record of year I want}
               while (EUWarReason_record_read.year < first_partition_year) and (not eof(EUWarReason_file)) do
                  begin
                     read (EUWarReason_file, EUWarReason_record_read);
                     EU_WarReason_trace.tick ( 'Executing Procedure: Searching EU (War + Reason) File ',0);
                  end;
               EU_WarReason_trace.tickdone;

                  {now read records I need.  First, may have a good record in memory.}
               if (EUWarReason_record_read.year >= self.first_partition_year) and
                  (EUWarReason_record_read.year <= self.last_partition_year) then
                     year_array[EUWarReason_record_read.year]^[ccode_index.index(EUWarReason_record_read.ccode1)]^
                           [ccode_index.index(EUWarReason_record_read.ccode2)] := EUWarReason_record_read.EUWarReason_Rec;
               num_read := 0;
               prevyear := 0;
               while (EUWarReason_record_read.year <= last_partition_year) and (not eof(EUWarReason_file)) do
                  begin
                     read (EUWarReason_file, EUWarReason_record_read);
                     if (EUWarReason_record_read.year >= self.first_partition_year) and
                        (EUWarReason_record_read.year <= self.last_partition_year) then
                       year_array[EUWarReason_record_read.year]^[ccode_index.index(EUWarReason_record_read.ccode1)]^
                           [ccode_index.index(EUWarReason_record_read.ccode2)] := EUWarReason_record_read.EUWarReason_Rec;
                     if EUWarReason_record_read.year <> prevyear then
                       begin
                          EU_WarReason_trace.tick ('Executing Procedure: Read EU (War+Reason) File, '+
                             inttostr(first_partition_year) +' to ' +inttostr(last_partition_year),
                             (last_partition_year-first_partition_year+1));
                          prevyear := EUWarReason_record_read.year;
                          inc(num_read);
                       end;
                  end;

               EU_WarReason_trace.tickdone;
               created := true;

            finally
               CloseFile (EUWarReason_file);
               EU_WarReason_trace.free;
               trace.message (' required '+inttostr(round(((start_mem-memavail)/1024)))+' Kilobytes of memory');
               trace.exit('Finished initializing EUWarReason data');
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

   destructor TEUWarReason_array_obj.destroy;
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

   function TEUWarReason_array_obj.get_first_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('EUWarReason get_partition called before initialization',
                               5, stop, error_log);
               get_first_partition_year := min_year;
            end
         else
            get_first_partition_year := first_partition_year;
      end;

   function TEUWarReason_array_obj.get_last_partition_year : year_range;
      begin
         if not(initialized) then
            begin
               EUGeneError ('EUWarReason get_partition called before initialization',
                               5, stop, error_log);
               get_last_partition_year := min_year;
            end
         else
            get_last_partition_year := last_partition_year;
      end;

   function TEUWarReason_array_obj.initialized : boolean;
      begin
         initialized := false;
         if self <> nil then if created=true then initialized := true;
      end;

   function TEUWarReason_array_obj.all_in_range (ccode1, ccode2 : ccode_range; ayear : year_range; error_check : boolean): boolean;
      begin
         all_in_range := true;
         if not(initialized) then
            begin
               all_in_range := false;
               if error_check then
                  begin
                     EUGeneError ('Get a value from EUWarReason array called before initialization',
                                     5, stop, error_log);
                     trace.message ('value  set to missing');
                  end;
            end
         else
         if not ((ayear >= get_first_partition_year) and (ayear <= get_last_partition_year)) then
            begin
               all_in_range := false;
               if error_check then
                  begin
                     EUGeneError ('Internal Error in program - called Get a value from EUWarReason array with year outside partition',
                                     5, continue, error_log);
                     trace.message ('value  set to missing');
                  end;
            end
         else
            if not ( (nation_list.is_a_state(ccode1, ayear)) and (nation_list.is_a_state(ccode2, ayear)) ) then
               begin
                  all_in_range := false;
                  if error_check then
                     begin
                        trace.message ('ccode/year error in program - called Get a value from EUWarReason array for invalid ccodes given year'+inttostr(ccode1)+' in '+inttostr(ayear));
                        trace.message ('value set to missing');
                     end;
               end;
      end;

   function TEUWarReason_array_obj.all_valid (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : boolean;
      begin
         if ((get_UtilityAA(ccode1, ccode2, ayear, user_selections) = missing_value) or
             (get_UtilityAB(ccode1, ccode2, ayear, user_selections) = missing_value) or
             (get_UtilityBA(ccode1, ccode2, ayear, user_selections) = missing_value) or
             (get_UtilityBB(ccode1, ccode2, ayear, user_selections) = missing_value) or
             (get_UtilityASQ(ccode1, ccode2, ayear, user_selections) = missing_value) or
             (get_UtilityBSQ(ccode1, ccode2, ayear, user_selections) = missing_value) or
             (get_ProbWinAB(ccode1, ccode2, ayear, user_selections) = missing_value) or
             (get_ProbWinBA(ccode1, ccode2, ayear, user_selections) = missing_value) )
            then all_valid := false
         else all_valid := true;
      end;

   function TEUWarReason_array_obj.get_UtilityAA (ccode1, ccode2 : ccode_range; ayear : year_range;
                                                  const user_selections : user_selection_type) : single;
      {return value of Utility of A vs A. So, UA(DA) from matrix for this year-pair.}
      {These are computed as a missing_value if there was a problem with them.}
      begin
         get_UtilityAA := missing_value;
         if all_in_range (ccode1, ccode2, ayear, true) then
               get_UtilityAA := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].UtilityAA;
      end;

   function TEUWarReason_array_obj.get_UtilityAA (ccode1, ccode2 : ccode_range; ayear : year_range;
                                                  const user_selections : user_selection_type; error_check : boolean) : single;
      {return value of Utility of A vs A. So, UA(DA) from matrix for this year-pair.}
      {These are computed as a missing_value if there was a problem with them.}
      begin
         get_UtilityAA := missing_value;
         if all_in_range (ccode1, ccode2, ayear, error_check) then
               get_UtilityAA := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].UtilityAA;
      end;

   function TEUWarReason_array_obj.get_UtilityAB (ccode1, ccode2 : ccode_range; ayear : year_range;
                                                  const user_selections : user_selection_type) : single;
      begin
         get_UtilityAB := missing_value;
         if all_in_range (ccode1, ccode2, ayear, true) then
               get_UtilityAB := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].UtilityAB;
      end;

   function TEUWarReason_array_obj.get_UtilityAB (ccode1, ccode2 : ccode_range; ayear : year_range;
                                                  const user_selections : user_selection_type; error_check : boolean) : single;
      begin
         get_UtilityAB := missing_value;
         if all_in_range (ccode1, ccode2, ayear, error_check) then
               get_UtilityAB := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].UtilityAB;
      end;

   function TEUWarReason_array_obj.get_UtilityASQ (ccode1, ccode2 : ccode_range; ayear : year_range;
                                                  const user_selections : user_selection_type) : single;
      begin
         get_UtilityASQ := missing_value;
         if all_in_range (ccode1, ccode2, ayear, true) then
               get_UtilityASQ := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].UtilityASQ;
      end;

   function TEUWarReason_array_obj.get_UtilityASQ (ccode1, ccode2 : ccode_range; ayear : year_range;
                                                  const user_selections : user_selection_type; error_check : boolean) : single;
      begin
         get_UtilityASQ := missing_value;
         if all_in_range (ccode1, ccode2, ayear, error_check) then
               get_UtilityASQ := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].UtilityASQ;
      end;

   function TEUWarReason_array_obj.get_ProbWinAB (ccode1, ccode2 : ccode_range; ayear : year_range;
                                                 const user_selections : user_selection_type) : single;
      begin
         get_ProbWinAB := missing_value;
         if all_in_range (ccode1, ccode2, ayear, true) then
            get_ProbWinAB := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].ProbWinAB;
      end;

   function TEUWarReason_array_obj.get_ProbWinAB (ccode1, ccode2 : ccode_range; ayear : year_range;
                                                 const user_selections : user_selection_type; error_check : boolean) : single;
      begin
         get_ProbWinAB := missing_value;
         if all_in_range (ccode1, ccode2, ayear, error_check) then
            get_ProbWinAB := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].ProbWinAB;
      end;

   function TEUWarReason_array_obj.get_StakesA (ccode1, ccode2 : ccode_range; ayear : year_range;
                                               const user_selections : user_selection_type) : single;
       {This must be calculated from stored values}
           { StakesA = UADA - UADB; }
    begin
         get_StakesA := missing_value;
         if all_in_range (ccode1, ccode2, ayear, true) then
            if ((get_UtilityAA(ccode1, ccode2, ayear, user_selections) <> missing_value) and
                (get_UtilityAB(ccode1, ccode2, ayear, user_selections) <> missing_value)) then
            get_StakesA := get_UtilityAA(ccode1, ccode2, ayear, user_selections) -
                           get_UtilityAB(ccode1, ccode2, ayear, user_selections);
      end;

   function TEUWarReason_array_obj.get_StakesA (ccode1, ccode2 : ccode_range; ayear : year_range;
                                               const user_selections : user_selection_type; error_check : boolean) : single;
       {This must be calculated from stored values}
           { StakesA = UADA - UADB; }
    begin
         get_StakesA := missing_value;
         if all_in_range (ccode1, ccode2, ayear, error_check) then
            if ((get_UtilityAA(ccode1, ccode2, ayear, user_selections) <> missing_value) and
                (get_UtilityAB(ccode1, ccode2, ayear, user_selections) <> missing_value)) then
            get_StakesA := get_UtilityAA(ccode1, ccode2, ayear, user_selections) -
                           get_UtilityAB(ccode1, ccode2, ayear, user_selections);
      end;

   function TEUWarReason_array_obj.get_UtilityBB (ccode1, ccode2 : ccode_range; ayear : year_range;
                                                 const user_selections : user_selection_type) : single;
      var UBB : single;
      {return value of Utility of B vs B. So, UB(DB) from matrix for this year-pair.}
      {These are computed as a missing_value if there was a problem with them.}
      begin
         if all_in_range (ccode1, ccode2, ayear, true) then
            begin
               get_UtilityBB := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].UtilityBB;
            end;
      end;

   function TEUWarReason_array_obj.get_UtilityBB (ccode1, ccode2 : ccode_range; ayear : year_range;
                                                 const user_selections : user_selection_type; error_check : boolean) : single;
      var UBB : single;
      {return value of Utility of B vs B. So, UB(DB) from matrix for this year-pair.}
      {These are computed as a missing_value if there was a problem with them.}
      begin
         if all_in_range (ccode1, ccode2, ayear, error_check) then
            begin
               get_UtilityBB := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].UtilityBB;
            end;
      end;

   function TEUWarReason_array_obj.get_UtilityBA (ccode1, ccode2 : ccode_range; ayear : year_range;
                                                 const user_selections : user_selection_type) : single;
      begin
         get_UtilityBA := missing_value;
         if all_in_range (ccode1, ccode2, ayear, true) then
            begin
               get_UtilityBA := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].UtilityBA;
            end;
      end;

   function TEUWarReason_array_obj.get_UtilityBA (ccode1, ccode2 : ccode_range; ayear : year_range;
                                                 const user_selections : user_selection_type; error_check : boolean) : single;
      begin
         get_UtilityBA := missing_value;
         if all_in_range (ccode1, ccode2, ayear, error_check) then
            begin
               get_UtilityBA := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].UtilityBA;
            end;
      end;

   function TEUWarReason_array_obj.get_UtilityBSQ (ccode1, ccode2 : ccode_range; ayear : year_range;
                                                  const user_selections : user_selection_type) : single;
      begin
         get_UtilityBSQ := missing_value;
         if all_in_range (ccode1, ccode2, ayear, true) then
            begin
               get_UtilityBSQ := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].UtilityBSQ;
            end;
      end;

   function TEUWarReason_array_obj.get_UtilityBSQ (ccode1, ccode2 : ccode_range; ayear : year_range;
                                                  const user_selections : user_selection_type; error_check : boolean) : single;
      begin
         get_UtilityBSQ := missing_value;
         if all_in_range (ccode1, ccode2, ayear, error_check) then
            begin
               get_UtilityBSQ := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].UtilityBSQ;
            end;
      end;

   function TEUWarReason_array_obj.get_ProbWinBA (ccode1, ccode2 : ccode_range; ayear : year_range;
                                                 const user_selections : user_selection_type) : single;
      begin
         get_ProbWinBA := missing_value;
         if all_in_range (ccode1, ccode2, ayear, true) then
            get_ProbWinBA := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].ProbWinBA;
      end;

   function TEUWarReason_array_obj.get_ProbWinBA (ccode1, ccode2 : ccode_range; ayear : year_range;
                                                 const user_selections : user_selection_type; error_check : boolean) : single;
      begin
         get_ProbWinBA := missing_value;
         if all_in_range (ccode1, ccode2, ayear, error_check) then
            get_ProbWinBA := year_array[ayear]^[ccode_index.index(ccode1)]^[ccode_index.index(ccode2)].ProbWinBA;
      end;

   function TEUWarReason_array_obj.get_StakesB (ccode1, ccode2 : ccode_range; ayear : year_range;
                                               const user_selections : user_selection_type) : single;
       {This must be calculated from stored values}
           { StakesA = UADA - UADB; }
    begin
         get_StakesB := missing_value;
         if all_in_range (ccode1, ccode2, ayear, true) then
            if ((get_UtilityBB(ccode1, ccode2, ayear, user_selections) <> missing_value) and
                (get_UtilityBA(ccode1, ccode2, ayear, user_selections) <> missing_value)) then
            get_StakesB := get_UtilityBB(ccode1, ccode2, ayear, user_selections) -
                           get_UtilityBA(ccode1, ccode2, ayear, user_selections);
      end;

   function TEUWarReason_array_obj.get_StakesB (ccode1, ccode2 : ccode_range; ayear : year_range;
                                               const user_selections : user_selection_type; error_check : boolean) : single;
       {This must be calculated from stored values}
           { StakesA = UADA - UADB; }
    begin
         get_StakesB := missing_value;
         if all_in_range (ccode1, ccode2, ayear, error_check) then
            if ((get_UtilityBB(ccode1, ccode2, ayear, user_selections) <> missing_value) and
                (get_UtilityBA(ccode1, ccode2, ayear, user_selections) <> missing_value)) then
            get_StakesB := get_UtilityBB(ccode1, ccode2, ayear, user_selections) -
                           get_UtilityBA(ccode1, ccode2, ayear, user_selections);
      end;

{ -----------------------------------------------   }

   function TEUWarReason_array_obj.get_UiSQ (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single;
      begin
         get_UiSQ := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UiSQ := get_UtilityASQ(ccode1, ccode2, ayear, user_selections);
      end;

   function TEUWarReason_array_obj.get_UiAcqi (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single;
      begin
         get_UiAcqi := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UiAcqi := get_UtilityAB(ccode1, ccode2, ayear, user_selections);
      end;

   function TEUWarReason_array_obj.get_UiAcqj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single;
      begin
         get_UiAcqj := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UiAcqj := get_UtilityAA(ccode1, ccode2, ayear, user_selections);
      end;

   function TEUWarReason_array_obj.get_UiNego (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single;
      begin
         get_UiNego := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UiNego := (get_ProbWinAB(ccode1, ccode2, ayear, user_selections) *
                           get_UtilityAA(ccode1, ccode2, ayear, user_selections)) +
                          ((1-get_ProbWinAB(ccode1, ccode2, ayear, user_selections)) *
                           get_UtilityAB(ccode1, ccode2, ayear, user_selections)) ;
      end;

   function TEUWarReason_array_obj.get_UiCapi (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single;
      var gamma : single;
         {this is UiDj - 1 * (1-P) }
      begin
         get_UiCapi := missing_value;
         gamma := 1;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UiCapi := get_UtilityAB(ccode1, ccode2, ayear, user_selections) -
                          gamma * (1-get_ProbWinAB (ccode1, ccode2, ayear, user_selections));
      end;

   function TEUWarReason_array_obj.get_UiCapj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single;
      var phi : single;
         {Phi in equations p. 47 is domestic Costs, but notation is unclear.}
         {Drop phi notation, then from bdm, this is UiDi - Ui(SQ)*Pi }
      begin
         get_UiCapj := missing_value;
         case user_selections.eu_calculation_info.adjusted_phi of
             {phi adjustment is to go from -1 to +1 to 0..1.}
           true : phi := (get_UiSQ(ccode1, ccode2, ayear, user_selections) + 1) / 2;
           false : phi := get_UiSQ(ccode1, ccode2, ayear, user_selections);
         end;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UiCapj := get_UtilityAA(ccode1, ccode2, ayear, user_selections) - (phi * get_ProbWinAB (ccode1, ccode2, ayear, user_selections) );
      end;

   function TEUWarReason_array_obj.get_UiWari (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single;
      var phi, alpha : single;
         {Phi in equations p. 47 is get_Cost_Domestic.  By Modified notation, phi=USQ}
      begin
         get_UiWari := missing_value;
         case user_selections.eu_calculation_info.adjusted_phi of
             {phi adjustment is to go from -1 to +1 to 0..1.}
           true : phi := (get_UiSQ(ccode1, ccode2, ayear, user_selections) + 1) / 2;
           false : phi := get_UiSQ(ccode1, ccode2, ayear, user_selections);
         end;
         alpha := 0.5;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UiWari :=   get_ProbWinAB (ccode1, ccode2, ayear, user_selections) *
                            (get_UtilityAA(ccode1, ccode2, ayear, user_selections) -
                             (phi * get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) -
                             (alpha * (1 - get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) ) )
                          + (1-get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) *
                            (get_UtilityAB(ccode1, ccode2, ayear, user_selections) -
                             (phi * get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) -
                             (alpha * (1-get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) ) );
      end;

   function TEUWarReason_array_obj.get_UiWarj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single;
      var phi, tau : single;
         {Phi in equations p. 47 is get_Cost_Domestic}
      begin
         get_UiWarj := missing_value;
         case user_selections.eu_calculation_info.adjusted_phi of
             {phi adjustment is to go from -1 to +1 to 0..1.}
           true : phi := (get_UiSQ(ccode1, ccode2, ayear, user_selections) + 1) / 2;
           false : phi := get_UiSQ(ccode1, ccode2, ayear, user_selections);
         end;
         tau := 1;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UiWarj :=  get_ProbWinAB (ccode1, ccode2, ayear, user_selections) *
                            (get_UtilityAA(ccode1, ccode2, ayear, user_selections) -
                             (phi * get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) -
                             (tau * (1-get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) ) )
                          + (1-get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) *
                            (get_UtilityAB(ccode1, ccode2, ayear, user_selections) -
                             (phi * get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) -
                             (tau * (1-get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) ) );
      end;

      {  ------------------------------      }
     {now overloaded methods}

   function TEUWarReason_array_obj.get_UiSQ (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check:boolean) : single;
      begin
         get_UiSQ := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UiSQ := get_UtilityASQ(ccode1, ccode2, ayear, user_selections);
      end;

   function TEUWarReason_array_obj.get_UiAcqi (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check:boolean) : single;
      begin
         get_UiAcqi := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UiAcqi := get_UtilityAB(ccode1, ccode2, ayear, user_selections);
      end;

   function TEUWarReason_array_obj.get_UiAcqj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check:boolean) : single;
      begin
         get_UiAcqj := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UiAcqj := get_UtilityAA(ccode1, ccode2, ayear, user_selections);
      end;

   function TEUWarReason_array_obj.get_UiNego (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check:boolean) : single;
      begin
         get_UiNego := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UiNego := (get_ProbWinAB(ccode1, ccode2, ayear, user_selections) *
                           get_UtilityAA(ccode1, ccode2, ayear, user_selections)) +
                          ((1-get_ProbWinAB(ccode1, ccode2, ayear, user_selections)) *
                           get_UtilityAB(ccode1, ccode2, ayear, user_selections)) ;
      end;

   function TEUWarReason_array_obj.get_UiCapi (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check:boolean) : single;
      var gamma : single;
      begin
         get_UiCapi := missing_value;
         gamma := 1;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UiCapi := get_UtilityAB(ccode1, ccode2, ayear, user_selections) -
                          gamma * (1-get_ProbWinAB (ccode1, ccode2, ayear, user_selections));
      end;

   function TEUWarReason_array_obj.get_UiCapj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check:boolean) : single;
      var phi : single;
      begin
         get_UiCapj := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               case user_selections.eu_calculation_info.adjusted_phi of
                 true : phi := (get_UiSQ(ccode1, ccode2, ayear, user_selections) + 1) / 2;
                 false : phi := get_UiSQ(ccode1, ccode2, ayear, user_selections);
               end;
               get_UiCapj := get_UtilityAA(ccode1, ccode2, ayear, user_selections) - (phi * get_ProbWinAB (ccode1, ccode2, ayear, user_selections) );
            end;
      end;

   function TEUWarReason_array_obj.get_UiWari (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check:boolean) : single;
      var phi, alpha : single;
      begin
         get_UiWari := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               case user_selections.eu_calculation_info.adjusted_phi of
                 true : phi := (get_UiSQ(ccode1, ccode2, ayear, user_selections) + 1) / 2;
                 false : phi := get_UiSQ(ccode1, ccode2, ayear, user_selections);
               end;
               alpha := 0.5;
               get_UiWari :=   get_ProbWinAB (ccode1, ccode2, ayear, user_selections) *
                               (get_UtilityAA(ccode1, ccode2, ayear, user_selections) -
                                (phi * get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) -
                                (alpha * (1 - get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) ) )
                             + (1-get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) *
                               (get_UtilityAB(ccode1, ccode2, ayear, user_selections) -
                                (phi * get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) -
                                (alpha * (1-get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) ) );
            end;
      end;

   function TEUWarReason_array_obj.get_UiWarj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check:boolean) : single;
      var phi, tau : single;
         {Phi in equations p. 47 is get_Cost_Domestic}
      begin
         get_UiWarj := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               case user_selections.eu_calculation_info.adjusted_phi of
                   {phi adjustment is to go from -1 to +1 to 0..1.}
                 true : phi := (get_UiSQ(ccode1, ccode2, ayear, user_selections) + 1) / 2;
                 false : phi := get_UiSQ(ccode1, ccode2, ayear, user_selections);
               end;
               tau := 1;
               get_UiWarj :=  get_ProbWinAB (ccode1, ccode2, ayear, user_selections) *
                               (get_UtilityAA(ccode1, ccode2, ayear, user_selections) -
                                (phi * get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) -
                                (tau * (1-get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) ) )
                             + (1-get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) *
                               (get_UtilityAB(ccode1, ccode2, ayear, user_selections) -
                                (phi * get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) -
                                (tau * (1-get_ProbWinAB (ccode1, ccode2, ayear, user_selections)) ) );
            end;
      end;

      {  ---------------------------------------------------------   }
      {non overloaded prcs first}

   function TEUWarReason_array_obj.get_UjSQ (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single;
      begin
         get_UjSQ := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UjSQ := get_UtilityBSQ(ccode1, ccode2, ayear, user_selections);
      end;

   function TEUWarReason_array_obj.get_UjAcqi (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single;
      begin
         get_UjAcqi := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UjAcqi := get_UtilityBB(ccode1, ccode2, ayear, user_selections);
      end;

   function TEUWarReason_array_obj.get_UjAcqj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single;
      begin
         get_UjAcqj := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UjAcqj := get_UtilityBA(ccode1, ccode2, ayear, user_selections);
      end;

   function TEUWarReason_array_obj.get_UjNego (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single;
      begin
         get_UjNego := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UjNego := (get_ProbWinBA(ccode1, ccode2, ayear, user_selections) *
                           get_UtilityBB(ccode1, ccode2, ayear, user_selections)) +
                          ((1-get_ProbWinBA(ccode1, ccode2, ayear, user_selections)) *
                           get_UtilityBA(ccode1, ccode2, ayear, user_selections)) ;
      end;

   function TEUWarReason_array_obj.get_UjCapi (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single;
      var phi : single;
      begin
         get_UjCapi := missing_value;
         case user_selections.eu_calculation_info.adjusted_phi of
             {phi adjustment is to go from -1 to +1 to 0..1.}
           true : phi := (get_UiSQ(ccode1, ccode2, ayear, user_selections) + 1) / 2;
           false : phi := get_UiSQ(ccode1, ccode2, ayear, user_selections);
         end;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UjCapi := get_UtilityBB(ccode1, ccode2, ayear, user_selections) - (phi * get_ProbWinBA (ccode1, ccode2, ayear, user_selections) );
            {Note:  even though it looks funny, this is correctly using UtilityBA.  }
      end;

   function TEUWarReason_array_obj.get_UjCapj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single;
      var gamma : single;
      begin
         get_UjCapj := missing_value;
         gamma := 1;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UjCapj := get_UtilityBA(ccode1, ccode2, ayear, user_selections) -
                          gamma * (1-get_ProbWinBA (ccode1, ccode2, ayear, user_selections));
            {Note:  even though it looks funny, this is correctly using UtilityBA.  j's utility
             for a cap by j is based on j's value for i's (A's) preferred posn, UBA.}
      end;

   function TEUWarReason_array_obj.get_UjWari (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single;
      var phi, tau : single;
      begin
         get_UjWari := missing_value;
         case user_selections.eu_calculation_info.adjusted_phi of
             {phi adjustment is to go from -1 to +1 to 0..1.}
           true : phi := (get_UiSQ(ccode1, ccode2, ayear, user_selections) + 1) / 2;
           false : phi := get_UiSQ(ccode1, ccode2, ayear, user_selections);
         end;
         tau := 1;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UjWari :=   get_ProbWinBA (ccode1, ccode2, ayear, user_selections) *
                            (get_UtilityBB(ccode1, ccode2, ayear, user_selections) -
                             (phi * get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) -
                             (tau * (1 - get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) ) )
                          + (1-get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) *
                            (get_UtilityBA(ccode1, ccode2, ayear, user_selections) -
                             (phi * get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) -
                             (tau * (1-get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) ) );
      end;

   function TEUWarReason_array_obj.get_UjWarj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : single;
      var phi, alpha : single;
      begin
         get_UjWarj := missing_value;
         case user_selections.eu_calculation_info.adjusted_phi of
             {phi adjustment is to go from -1 to +1 to 0..1.}
           true : phi := (get_UiSQ(ccode1, ccode2, ayear, user_selections) + 1) / 2;
           false : phi := get_UiSQ(ccode1, ccode2, ayear, user_selections);
         end;
         alpha := 0.5;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UjWarj :=  get_ProbWinBA (ccode1, ccode2, ayear, user_selections) *
                            (get_UtilityBB(ccode1, ccode2, ayear, user_selections) -
                             (phi * get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) -
                             (alpha * (1-get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) ) )
                          + (1-get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) *
                            (get_UtilityBA(ccode1, ccode2, ayear, user_selections) -
                             (phi * get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) -
                             (alpha * (1-get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) ) );
      end;

                  {  ---------------------------------------------------------------   }
     {now overloaded methods}

   function TEUWarReason_array_obj.get_UjSQ (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check:boolean) : single;
      begin
         get_UjSQ := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UjSQ := get_UtilityBSQ(ccode1, ccode2, ayear, user_selections);
      end;

   function TEUWarReason_array_obj.get_UjAcqi (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check:boolean) : single;
      begin
         get_UjAcqi := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UjAcqi := get_UtilityBB(ccode1, ccode2, ayear, user_selections);
      end;

   function TEUWarReason_array_obj.get_UjAcqj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check:boolean) : single;
      begin
         get_UjAcqj := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UjAcqj := get_UtilityBA(ccode1, ccode2, ayear, user_selections);
      end;

   function TEUWarReason_array_obj.get_UjNego (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check:boolean) : single;
      begin
         get_UjNego := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UjNego := (get_ProbWinBA(ccode1, ccode2, ayear, user_selections) *
                           get_UtilityBB(ccode1, ccode2, ayear, user_selections)) +
                          ((1-get_ProbWinBA(ccode1, ccode2, ayear, user_selections)) *
                           get_UtilityBA(ccode1, ccode2, ayear, user_selections)) ;
      end;

   function TEUWarReason_array_obj.get_UjCapi (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check:boolean) : single;
      var phi : single;
      begin
         get_UjCapi := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               case user_selections.eu_calculation_info.adjusted_phi of
                   {phi adjustment is to go from -1 to +1 to 0..1.}
                 true : phi := (get_UiSQ(ccode1, ccode2, ayear, user_selections) + 1) / 2;
                 false : phi := get_UiSQ(ccode1, ccode2, ayear, user_selections);
               end;
                  get_UjCapi := get_UtilityBB(ccode1, ccode2, ayear, user_selections) - (phi * get_ProbWinBA (ccode1, ccode2, ayear, user_selections) );
                  {Note:  even though it looks funny, this is correctly using UtilityBA.  }
				 end;
      end;

   function TEUWarReason_array_obj.get_UjCapj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check:boolean) : single;
      var gamma : single;
      begin
         get_UjCapj := missing_value;
         gamma := 1;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            get_UjCapj := get_UtilityBA(ccode1, ccode2, ayear, user_selections) -
                          gamma * (1-get_ProbWinBA (ccode1, ccode2, ayear, user_selections));
            {Note:  even though it looks funny, this is correctly using UtilityBA.  j's utility
             for a cap by j is based on j's value for i's (A's) preferred posn, UBA.}
      end;

   function TEUWarReason_array_obj.get_UjWari (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check:boolean) : single;
      var phi, tau : single;
      begin
         get_UjWari := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               case user_selections.eu_calculation_info.adjusted_phi of
                   {phi adjustment is to go from -1 to +1 to 0..1.}
                 true : phi := (get_UiSQ(ccode1, ccode2, ayear, user_selections) + 1) / 2;
                 false : phi := get_UiSQ(ccode1, ccode2, ayear, user_selections);
               end;
               tau := 1;
               get_UjWari :=   get_ProbWinBA (ccode1, ccode2, ayear, user_selections) *
                               (get_UtilityBB(ccode1, ccode2, ayear, user_selections) -
                                (phi * get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) -
                                (tau * (1 - get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) ) )
                             + (1-get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) *
                               (get_UtilityBA(ccode1, ccode2, ayear, user_selections) -
                                (phi * get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) -
                                (tau * (1-get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) ) );
				 end;
      end;

   function TEUWarReason_array_obj.get_UjWarj (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check:boolean) : single;
      var phi, alpha : single;
      begin
         get_UjWarj := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               case user_selections.eu_calculation_info.adjusted_phi of
                   {phi adjustment is to go from -1 to +1 to 0..1.}
                 true : phi := (get_UiSQ(ccode1, ccode2, ayear, user_selections) + 1) / 2;
                 false : phi := get_UiSQ(ccode1, ccode2, ayear, user_selections);
               end;
               alpha := 0.5;
               get_UjWarj :=  get_ProbWinBA (ccode1, ccode2, ayear, user_selections) *
                               (get_UtilityBB(ccode1, ccode2, ayear, user_selections) -
                                (phi * get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) -
                                (alpha * (1-get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) ) )
                             + (1-get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) *
                               (get_UtilityBA(ccode1, ccode2, ayear, user_selections) -
                                (phi * get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) -
                                (alpha * (1-get_ProbWinBA (ccode1, ccode2, ayear, user_selections)) ) );
            end;
      end;

                  {  ---------------------------------------------------------------   }

   {Internal solve full model functions if don't want to believe equilibrium equations}

   procedure TEUWarReason_array_obj.setUtilityArray (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type);
   const a=1;
         b=2;
      begin
         SolvingUtilityArray[a,CapA] := get_uicapi(ccode1, ccode2, ayear, user_selections);
         SolvingUtilityArray[a,CapB] := get_uicapj(ccode1, ccode2, ayear, user_selections);
         SolvingUtilityArray[a,AcqA] := get_uiacqi(ccode1, ccode2, ayear, user_selections);
         SolvingUtilityArray[a,AcqB] := get_uiacqj(ccode1, ccode2, ayear, user_selections);
         SolvingUtilityArray[a,WarA] := get_uiwari(ccode1, ccode2, ayear, user_selections);
         SolvingUtilityArray[a,WarB] := get_uiwarj(ccode1, ccode2, ayear, user_selections);
         SolvingUtilityArray[a,Nego] := get_uinego(ccode1, ccode2, ayear, user_selections);
         SolvingUtilityArray[a,SQ] := get_uisq(ccode1, ccode2, ayear, user_selections);
         SolvingUtilityArray[b,CapA] := get_ujcapi(ccode1, ccode2, ayear, user_selections);
         SolvingUtilityArray[b,CapB] := get_ujcapj(ccode1, ccode2, ayear, user_selections);
         SolvingUtilityArray[b,AcqA] := get_ujacqi(ccode1, ccode2, ayear, user_selections);
         SolvingUtilityArray[b,AcqB] := get_ujacqj(ccode1, ccode2, ayear, user_selections);
         SolvingUtilityArray[b,WarA] := get_ujwari(ccode1, ccode2, ayear, user_selections);
         SolvingUtilityArray[b,WarB] := get_ujwarj(ccode1, ccode2, ayear, user_selections);
         SolvingUtilityArray[b,Nego] := get_ujnego(ccode1, ccode2, ayear, user_selections);
         SolvingUtilityArray[b,SQ] := get_ujsq(ccode1, ccode2, ayear, user_selections);
      end;

      function TEUWarReason_array_obj.solveOriginal (anode : nodetype; U : UtilityArrayType) : IIG_outcomes;
      const a=1;
            b=2;
      begin
         case anode of
           1 : if U[a,solveOriginal(2,U)] > U[a,solveOriginal(3,U)] then solveOriginal := solveOriginal(2,U) else solveOriginal := solveOriginal(3,U);
           2 : if U[b,SQ] > U[b,solveOriginal(4,U)] then solveOriginal := SQ else solveOriginal := solveOriginal(4,U);
           { Note:  this was the code that led the game down the wrong branch when values were tied.
             Now, when tied (which shouldn't actually happen by assumption) the game will yield
             the correct Acq outcome.
             3 : if U[b,AcqB] > U[b,solveOriginal(5,U)] then solveOriginal := AcqB else solveOriginal := solveOriginal(5,U); }
           3 : if U[b,solveOriginal(5,U)] > U[b,AcqB] then solveOriginal := solveOriginal(5,U) else solveOriginal := AcqB;
           4 : if U[a,solveOriginal(6,U)] > U[a,AcqA] then solveOriginal := solveOriginal(6,U) else solveOriginal := AcqA;
           5 : if U[a,solveOriginal(9,U)] > U[a,solveOriginal(10,U)] then solveOriginal := solveOriginal(9,U) else solveOriginal := solveOriginal(10,U);
           6 : if U[b,solveOriginal(7,U)] > U[b,solveOriginal(8,U)] then solveOriginal := solveOriginal(7,U) else solveOriginal := solveOriginal(8,U);
           7 : if U[a,Nego] > U[a,solveOriginal(11,U)] then solveOriginal := Nego else solveOriginal := solveOriginal(11,U);
           8 : if U[a,Capa] > U[a,WarB] then solveOriginal := CapA else solveOriginal := WarB;
           9 : if U[b,Nego] > U[b,solveOriginal(12,U)] then solveOriginal := Nego else solveOriginal := solveOriginal(12,U);
           10 : if U[b,CapB] > U[b,WarA] then solveOriginal := CapB else solveOriginal := WarA;
           11 : if U[b,CapB] > U[b,WarA] then solveOriginal := CapB else solveOriginal := WarA;
           12 : if U[a,CapA] > U[a,WarB] then solveOriginal := CapA else solveOriginal := WarB;
         end;   {case}
      end;

      function TEUWarReason_array_obj.solveNoWar (anode : nodetype; U : UtilityArrayType) : IIG_outcomes;
      const a=1;
            b=2;
      begin
         case anode of
           1 : if U[a,solveNoWar(2,U)] > U[a,solveNoWar(3,U)] then solveNoWar := solveNoWar(2,U) else solveNoWar := solveNoWar(3,U);
           2 : if U[b,solveNoWar(4,U)] > U[b,SQ] then solveNoWar := solveNoWar(4,U) else solveNoWar := SQ;
           3 : if U[b,solveNoWar(5,U)] > U[b,AcqB] then solveNoWar := solveNoWar(5,U) else solveNoWar := AcqB;
           4 : if U[a,solveNoWar(6,U)] > U[a,AcqA] then solveNoWar := solveNoWar(6,U) else solveNoWar := AcqA;
           5 : if U[a,solveNoWar(9,U)] > U[a,solveNoWar(10,U)] then solveNoWar := solveNoWar(9,U) else solveNoWar := solveNoWar(10,U);
           6 : if U[b,solveNoWar(7,U)] > U[b,solveNoWar(8,U)] then solveNoWar := solveNoWar(7,U) else solveNoWar := solveNoWar(8,U);
           7 : if U[a,Nego] > U[a,solveNoWar(11,U)] then solveNoWar := Nego else solveNoWar := solveNoWar(11,U);
           8 : solveNoWar := CapA;
           9 : if U[b,Nego] > U[b,solveNoWar(12,U)] then solveNoWar := Nego else solveNoWar := solveNoWar(12,U);
           10 : solveNoWar := CapB;
           11 : solveNoWar := CapB;
           12 : solveNoWar := CapA;
         end;   {case}
      end;

      function TEUWarReason_array_obj.solveNoForce (anode : nodetype; U : UtilityArrayType) : IIG_outcomes;
      const a=1;
            b=2;
      begin
         case anode of
           1 : if U[a,solveNoForce(2,U)] > U[a,solveNoForce(3,U)] then solveNoForce := solveNoForce(2,U) else solveNoForce := solveNoForce(3,U);
           2 : if U[b,solveNoForce(4,U)] > U[b,SQ] then solveNoForce := solveNoForce(4,U) else solveNoForce := SQ;
           3 : if U[b,solveNoForce(5,U)] > U[b,AcqB] then solveNoForce := solveNoForce(5,U) else solveNoForce := AcqB;
           4 : if U[a,solveNoForce(6,U)] > U[a,AcqA] then solveNoForce := solveNoForce(6,U) else solveNoForce := AcqA;
           5 : solveNoForce := solveNoForce(9,U);
           6 : solveNoForce := solveNoForce(7,U);
           7 : solveNoForce := Nego;
           8 : EUGeneError ('Solving no force game reached impossible node - error',1,stop,error_log);
           9 : solveNoForce := Nego;
           10 : EUGeneError ('Solving no force game reached impossible node - error',1,stop,error_log);
           11 : EUGeneError ('Solving no force game reached impossible node - error',1,stop,error_log);
           12 : EUGeneError ('Solving no force game reached impossible node - error',1,stop,error_log);
         end;   {case}
      end;
      {     -----------------------------   }

   function TEUWarReason_array_obj.get_EQ (EQ : IIG_outcomes; ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : dummy_type;
   begin
      get_EQ := missing_value;
      if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
      begin
         get_EQ := 0;
         case user_selections.eu_calculation_info.equilibrium_solution of
           induction :
              begin
                 setUtilityArray(ccode1, ccode2, ayear, user_selections);
                 case user_selections.eu_calculation_info.game_variant of
                    original : get_EQ := booltodummy(solveOriginal(1, SolvingUtilityArray) = EQ);
                    NoWar : get_EQ := booltodummy(solveNoWar(1, SolvingUtilityArray) = EQ);
                    NoForce : get_EQ := booltodummy(solveNoForce(1, SolvingUtilityArray) = EQ);
                    Node3Forward : get_EQ := booltodummy(solveOriginal(3, SolvingUtilityArray) = EQ);
                    else
                       begin
                          EUGeneError ('Error - incorrect model variant seen in solve for EQ.  Notify programmer.',1,continue, error_log);
                          get_eq := missing_value;
                       end;
                 end;   {case game variant of...}
              end;
           logical :
              begin
                 case EQ of
                 SQ: if (  (get_UiSQ(ccode1, ccode2, ayear, user_selections) > get_UiNego(ccode1, ccode2, ayear, user_selections) ) and
                        ( ( (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjWari(ccode1, ccode2, ayear, user_selections) > get_UjCapj(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjNego(ccode1, ccode2, ayear, user_selections) > get_UjCapi(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjSQ(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections) )            )
                          or
                          ( (get_UiNego(ccode1, ccode2, ayear, user_selections) > get_UiCapj(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjCapj(ccode1, ccode2, ayear, user_selections) > get_UjWari(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjNego(ccode1, ccode2, ayear, user_selections) > get_UjCapi(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjSQ(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections) )            )
                          or
                          ( (get_UiWarj(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjWari(ccode1, ccode2, ayear, user_selections) > get_UjCapj(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjSQ(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections) )            )
                          or
                          ( (get_UiNego(ccode1, ccode2, ayear, user_selections) > get_UiCapj(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UiWarj(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjCapj(ccode1, ccode2, ayear, user_selections) > get_UjWari(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjSQ(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections) )            )
                        )
                     ) then get_EQ := 1;
                 AcqA: if ( ( (get_uiacqi(ccode1, ccode2, ayear, user_selections) > get_uiwari(ccode1, ccode2, ayear, user_selections) ) and
                            (get_uicapi(ccode1, ccode2, ayear, user_selections) > get_uiwarj(ccode1, ccode2, ayear, user_selections) ) and
                            (get_ujcapi(ccode1, ccode2, ayear, user_selections) > get_ujnego(ccode1, ccode2, ayear, user_selections) ) and
                            (get_ujwari(ccode1, ccode2, ayear, user_selections) > get_ujacqj(ccode1, ccode2, ayear, user_selections) ) )
                          or
                          ( (get_uicapi(ccode1, ccode2, ayear, user_selections) > get_uiwari(ccode1, ccode2, ayear, user_selections) ) and
                            (get_ujcapi(ccode1, ccode2, ayear, user_selections) > get_ujnego(ccode1, ccode2, ayear, user_selections) ) and
                            ( (get_ujwari(ccode1, ccode2, ayear, user_selections) > get_ujcapj(ccode1, ccode2, ayear, user_selections) ) or
                              ( (get_ujcapj(ccode1, ccode2, ayear, user_selections) > get_ujwari(ccode1, ccode2, ayear, user_selections) ) and
                                (get_uicapi(ccode1, ccode2, ayear, user_selections) > get_uicapj(ccode1, ccode2, ayear, user_selections) )
                              )
                            )
                          )
                        )
                     then get_EQ := 1;
                 AcqB: if (( (get_UjCapj(ccode1, ccode2, ayear, user_selections) > get_UjWari(ccode1, ccode2, ayear, user_selections)) and
                           ( ( (get_UiCapj(ccode1, ccode2, ayear, user_selections) > get_UiNego(ccode1, ccode2, ayear, user_selections)) and
                               (get_UjNego(ccode1, ccode2, ayear, user_selections) > get_UjCapi(ccode1, ccode2, ayear, user_selections)) and
                               (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections)) ) or
                             ( (get_UiCapj(ccode1, ccode2, ayear, user_selections) > get_UiNego(ccode1, ccode2, ayear, user_selections)) and
                               (get_UiWarj(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections)) ) or
                             ( (get_UiCapj(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections)) and
                               (get_UjCapi(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections)) and
                               (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections)) )
                           )
                         )   or
                         ( (get_UjWari(ccode1, ccode2, ayear, user_selections) > get_UjCapj(ccode1, ccode2, ayear, user_selections)) and
                              (get_UiWari(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections)) and
                              (get_UjCapi(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections)) and
                              (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections)) and
                           (get_UjAcqj(ccode1, ccode2, ayear, user_selections) > get_UjWari(ccode1, ccode2, ayear, user_selections))
                         )
                        )
                     then get_EQ := 1;
                 CapA: get_EQ := 0;
                 CapB: get_EQ := 0;
                 Nego: if ((get_EQSQ(ccode1, ccode2, ayear, user_selections) =0) and (get_EQAcqA(ccode1, ccode2, ayear, user_selections) =0) and
                           (get_EQAcqB(ccode1, ccode2, ayear, user_selections) =0) and (get_EQCapA(ccode1, ccode2, ayear, user_selections) =0) and
                           (get_EQCapB(ccode1, ccode2, ayear, user_selections) =0) and (get_EQWarA(ccode1, ccode2, ayear, user_selections) =0) and
                           (get_EQWarB(ccode1, ccode2, ayear, user_selections) =0)) then get_EQ := 1;
                 WarA:  {conditions from WR p. 72}
                        {Actual conditions for predicting WarA involve 4 conditions.  However, only
                         3 can be assessed, I think because CapA > WarB for A is mutually exclusive (by consturction?)
                         from WarA>AcqA for A.  I believe you could substitute the other condition.     }
                     if ((get_UiWari(ccode1, ccode2, ayear, user_selections) > get_UiAcqi(ccode1, ccode2, ayear, user_selections)) and
                         (get_UjCapi(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections)) and
                         (get_UjWari(ccode1, ccode2, ayear, user_selections) > get_UjAcqj(ccode1, ccode2, ayear, user_selections))
                        ) then get_EQ := 1;
                        {Alternative condition if other component is dropped:}
                        {if ((get_UiCapi>get_UiWarj) and
                            (get_UjCapi>get_UjNego) and (get_UjWari>get_UjAcqj) ) get_EQWarA := 1
                         else get_EQWarA := 0;    }
                 WarB: get_EQ := 0;
                 end;       {list of eq}
              end   {logical}
           else begin
                 EUGeneError ('Unrecognized game solution method observed in get EQ procedures, for equilibrium '+inttostr(ord(EQ))+'.  Equilibrium set to 0.  Notify programmer.',0, continue, error_log);
                 get_eq := missing_value;
              end;
         end;       {case}
      end   {all_in_range}
      else get_EQ := missing_value;
   end;    {get_EQ}

   function TEUWarReason_array_obj.get_EQSQ (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : dummy_type;
      begin
         get_EQSQ := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
              if user_selections.eu_calculation_info.equilibrium_solution = induction then
                  begin
                     setUtilityArray(ccode1, ccode2, ayear, user_selections);
                     if solveOriginal(1, SolvingUtilityArray) = SQ
                     then get_EQSQ := 1
                     else get_EQSQ := 0;
                  end
               else
                  begin   {solveOriginal from logical formulas}
                     if (  (get_UiSQ(ccode1, ccode2, ayear, user_selections) > get_UiNego(ccode1, ccode2, ayear, user_selections) ) and
                           ( ( (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjWari(ccode1, ccode2, ayear, user_selections) > get_UjCapj(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjNego(ccode1, ccode2, ayear, user_selections) > get_UjCapi(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjSQ(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections) )            )
                             or
                             ( (get_UiNego(ccode1, ccode2, ayear, user_selections) > get_UiCapj(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjCapj(ccode1, ccode2, ayear, user_selections) > get_UjWari(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjNego(ccode1, ccode2, ayear, user_selections) > get_UjCapi(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjSQ(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections) )            )
                             or
                             ( (get_UiWarj(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjWari(ccode1, ccode2, ayear, user_selections) > get_UjCapj(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjSQ(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections) )            )
                             or
                             ( (get_UiNego(ccode1, ccode2, ayear, user_selections) > get_UiCapj(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UiWarj(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjCapj(ccode1, ccode2, ayear, user_selections) > get_UjWari(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjSQ(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections) )            )
                           )
                        ) then get_EQSQ := 1
                     else get_EQSQ := 0;
                  end;
            end
            else get_EQSQ := missing_value;
      end;

   function TEUWarReason_array_obj.get_EQNego (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : dummy_type;
      begin
         get_EQNego := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               if user_selections.eu_calculation_info.equilibrium_solution = induction then
                  begin
                     setUtilityArray(ccode1, ccode2, ayear, user_selections);
                     if solveOriginal(1, SolvingUtilityArray) = Nego
                     then get_EQNego := 1
                     else get_EQNego := 0;
                  end
               else
                  begin   {solveOriginal from logical formulas}
                     if ((get_EQSQ(ccode1, ccode2, ayear, user_selections) =0) and (get_EQAcqA(ccode1, ccode2, ayear, user_selections) =0) and
                         (get_EQAcqB(ccode1, ccode2, ayear, user_selections) =0) and (get_EQCapA(ccode1, ccode2, ayear, user_selections) =0) and
                         (get_EQCapB(ccode1, ccode2, ayear, user_selections) =0) and (get_EQWarA(ccode1, ccode2, ayear, user_selections) =0) and
                         (get_EQWarB(ccode1, ccode2, ayear, user_selections) =0)) then get_EQNego := 1
                     else get_EQNego := 0;
                  end;
            end
            else get_EQNego := missing_value;
      end;

   function TEUWarReason_array_obj.get_EQAcqA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : dummy_type;
      {conditions from W+R p. 80 and updated conversations with BdM}
    var tempeq1, tempeq2 : dummy_type;
      begin
         get_EQAcqA := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               if user_selections.eu_calculation_info.equilibrium_solution = induction then
                  begin
                     setUtilityArray(ccode1, ccode2, ayear, user_selections);
                     if solveOriginal(1, SolvingUtilityArray) = AcqA
                     then get_EQAcqA := 1
                     else get_EQAcqA := 0;
                  end
               else
                  begin   {solveOriginal from logical formulas}
                     if ( ( (get_uiacqi(ccode1, ccode2, ayear, user_selections) > get_uiwari(ccode1, ccode2, ayear, user_selections) ) and
                            (get_uicapi(ccode1, ccode2, ayear, user_selections) > get_uiwarj(ccode1, ccode2, ayear, user_selections) ) and
                            (get_ujcapi(ccode1, ccode2, ayear, user_selections) > get_ujnego(ccode1, ccode2, ayear, user_selections) ) and
                            (get_ujwari(ccode1, ccode2, ayear, user_selections) > get_ujacqj(ccode1, ccode2, ayear, user_selections) ) )
                          or
                          ( (get_uicapi(ccode1, ccode2, ayear, user_selections) > get_uiwari(ccode1, ccode2, ayear, user_selections) ) and
                            (get_ujcapi(ccode1, ccode2, ayear, user_selections) > get_ujnego(ccode1, ccode2, ayear, user_selections) ) and
                            ( (get_ujwari(ccode1, ccode2, ayear, user_selections) > get_ujcapj(ccode1, ccode2, ayear, user_selections) ) or
                              ( (get_ujcapj(ccode1, ccode2, ayear, user_selections) > get_ujwari(ccode1, ccode2, ayear, user_selections) ) and
                                (get_uicapi(ccode1, ccode2, ayear, user_selections) > get_uicapj(ccode1, ccode2, ayear, user_selections) )
                              )
                            )
                          )
                        )
                     then get_EQAcqA := 1
                     else get_EQAcqA := 0;
                  end;
            end
            else get_EQAcqA := missing_value;
      end;

   function TEUWarReason_array_obj.get_EQAcqB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : dummy_type;
      {conditions from WRp. 81 and figuring out eq from BdM Msgs}
      begin
         get_EQAcqB := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               if user_selections.eu_calculation_info.equilibrium_solution = induction then
                  begin
                     setUtilityArray(ccode1, ccode2, ayear, user_selections);
                     if solveOriginal(1, SolvingUtilityArray) = AcqB
                     then get_EQAcqB := 1
                     else get_EQAcqB := 0;
                  end
               else
                  begin   {solveOriginal from logical formulas}
                     if (( (get_UjCapj(ccode1, ccode2, ayear, user_selections) > get_UjWari(ccode1, ccode2, ayear, user_selections)) and
                           ( ( (get_UiCapj(ccode1, ccode2, ayear, user_selections) > get_UiNego(ccode1, ccode2, ayear, user_selections)) and
                               (get_UjNego(ccode1, ccode2, ayear, user_selections) > get_UjCapi(ccode1, ccode2, ayear, user_selections)) and
                               (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections)) ) or
                             ( (get_UiCapj(ccode1, ccode2, ayear, user_selections) > get_UiNego(ccode1, ccode2, ayear, user_selections)) and
                               (get_UiWarj(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections)) ) or
                             ( (get_UiCapj(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections)) and
                               (get_UjCapi(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections)) and
                               (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections)) )
                           )
                         )   or
                         ( (get_UjWari(ccode1, ccode2, ayear, user_selections) > get_UjCapj(ccode1, ccode2, ayear, user_selections)) and
                              (get_UiWari(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections)) and
                              (get_UjCapi(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections)) and
                              (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections)) and
                           (get_UjAcqj(ccode1, ccode2, ayear, user_selections) > get_UjWari(ccode1, ccode2, ayear, user_selections))
                         )
                        )
                     then get_EQAcqB := 1
                     else get_EQAcqB := 0;
                  end;
            end
            else get_EQAcqB := missing_value;
      end;

   function TEUWarReason_array_obj.get_EQCapA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : dummy_type;
         {international interaction game in W+R never expects Cap by either A or B}
      begin
         get_EQCapA := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               if user_selections.eu_calculation_info.equilibrium_solution = induction then
                  begin
                     setUtilityArray(ccode1, ccode2, ayear, user_selections);
                     if solveOriginal(1, SolvingUtilityArray) = CapA
                     then get_EQCapA := 1
                     else get_EQCapA := 0;
                  end
               else
               get_EQCapA := 0;
            end
            else get_EQCapA := missing_value;
      end;

   function TEUWarReason_array_obj.get_EQCapB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : dummy_type;
         {international interaction game in W+R never expects Cap by either A or B}
      begin
         get_EQCapB := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               if user_selections.eu_calculation_info.equilibrium_solution = induction then
                  begin
                     setUtilityArray(ccode1, ccode2, ayear, user_selections);
                     if solveOriginal(1, SolvingUtilityArray) = CapB
                     then get_EQCapB := 1
                     else get_EQCapB := 0;
                  end
               else
               get_EQCapB := 0;
            end
            else get_EQCapB := missing_value;
      end;

   function TEUWarReason_array_obj.get_EQWarA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : dummy_type;
      begin
         get_EQWarA := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               if user_selections.eu_calculation_info.equilibrium_solution = induction then
                  begin
                     setUtilityArray(ccode1, ccode2, ayear, user_selections);
                     if solveOriginal(1, SolvingUtilityArray) = WarA
                     then get_EQWarA := 1
                     else get_EQWarA := 0;
                  end
               else
                  begin   {solveOriginal from logical formulas}
                       {conditions from WR p. 72}
                      {Actual conditions for predicting WarA involve 4 conditions.  However, only
                       3 can be assessed, I think because CapA > WarB for A is mutually exclusive (by consturction?)
                       from WarA>AcqA for A.  I believe you could substitute the other condition.     }
                     if ((get_UiWari(ccode1, ccode2, ayear, user_selections) > get_UiAcqi(ccode1, ccode2, ayear, user_selections)) and
                         (get_UjCapi(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections)) and
                         (get_UjWari(ccode1, ccode2, ayear, user_selections) > get_UjAcqj(ccode1, ccode2, ayear, user_selections))
                        ) then get_EQWarA := 1
                     else get_EQWarA := 0;

                     {Alternative condition if other component is dropped:}
                     {if ((get_UiCapi>get_UiWarj) and
                         (get_UjCapi>get_UjNego) and (get_UjWari>get_UjAcqj) ) get_EQWarA := 1
                     else get_EQWarA := 0;    }
                  end;
            end
            else get_EQWarA := missing_value;
      end;

   function TEUWarReason_array_obj.get_EQWarB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type) : dummy_type;
         {international interaction game in W+R never expects War started by B}
      begin
         get_EQWarB := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, true) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               if user_selections.eu_calculation_info.equilibrium_solution = induction then
                  begin
                     setUtilityArray(ccode1, ccode2, ayear, user_selections);
                     if solveOriginal(1, SolvingUtilityArray) = WarB
                     then get_EQWarB := 1
                     else get_EQWarB := 0;
                  end
               else
               get_EQWarB := 0;
            end
            else get_EQWarB := missing_value;
      end;

      {     -----------------------------   }
       {Now overloaded methods}

   function TEUWarReason_array_obj.get_EQ (EQ : IIG_outcomes; ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean) : dummy_type;
   begin
      get_EQ := missing_value;
      if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
      begin
         get_EQ := 0;
         case user_selections.eu_calculation_info.equilibrium_solution of
           induction :
              begin
                 setUtilityArray(ccode1, ccode2, ayear, user_selections);
                 case user_selections.eu_calculation_info.game_variant of
                    original : get_EQ := booltodummy(solveOriginal(1, SolvingUtilityArray) = EQ);
                    NoWar : get_EQ := booltodummy(solveNoWar(1, SolvingUtilityArray) = EQ);
                    NoForce : get_EQ := booltodummy(solveNoForce(1, SolvingUtilityArray) = EQ);
                    Node3Forward : get_EQ := booltodummy(solveOriginal(3, SolvingUtilityArray) = EQ);
                    else
                       begin
                          EUGeneError ('Error - incorrect model variant seen in solve for EQ.  Notify programmer.',1,continue, error_log);
                          get_eq := missing_value;
                       end;
                 end;   {case game variant of...}
              end;
           logical :
              begin
                 case EQ of
                 SQ: if (  (get_UiSQ(ccode1, ccode2, ayear, user_selections) > get_UiNego(ccode1, ccode2, ayear, user_selections) ) and
                        ( ( (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjWari(ccode1, ccode2, ayear, user_selections) > get_UjCapj(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjNego(ccode1, ccode2, ayear, user_selections) > get_UjCapi(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjSQ(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections) )            )
                          or
                          ( (get_UiNego(ccode1, ccode2, ayear, user_selections) > get_UiCapj(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjCapj(ccode1, ccode2, ayear, user_selections) > get_UjWari(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjNego(ccode1, ccode2, ayear, user_selections) > get_UjCapi(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjSQ(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections) )            )
                          or
                          ( (get_UiWarj(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjWari(ccode1, ccode2, ayear, user_selections) > get_UjCapj(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjSQ(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections) )            )
                          or
                          ( (get_UiNego(ccode1, ccode2, ayear, user_selections) > get_UiCapj(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UiWarj(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjCapj(ccode1, ccode2, ayear, user_selections) > get_UjWari(ccode1, ccode2, ayear, user_selections) ) and
                            (get_UjSQ(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections) )            )
                        )
                     ) then get_EQ := 1;
                 AcqA: if ( ( (get_uiacqi(ccode1, ccode2, ayear, user_selections) > get_uiwari(ccode1, ccode2, ayear, user_selections) ) and
                            (get_uicapi(ccode1, ccode2, ayear, user_selections) > get_uiwarj(ccode1, ccode2, ayear, user_selections) ) and
                            (get_ujcapi(ccode1, ccode2, ayear, user_selections) > get_ujnego(ccode1, ccode2, ayear, user_selections) ) and
                            (get_ujwari(ccode1, ccode2, ayear, user_selections) > get_ujacqj(ccode1, ccode2, ayear, user_selections) ) )
                          or
                          ( (get_uicapi(ccode1, ccode2, ayear, user_selections) > get_uiwari(ccode1, ccode2, ayear, user_selections) ) and
                            (get_ujcapi(ccode1, ccode2, ayear, user_selections) > get_ujnego(ccode1, ccode2, ayear, user_selections) ) and
                            ( (get_ujwari(ccode1, ccode2, ayear, user_selections) > get_ujcapj(ccode1, ccode2, ayear, user_selections) ) or
                              ( (get_ujcapj(ccode1, ccode2, ayear, user_selections) > get_ujwari(ccode1, ccode2, ayear, user_selections) ) and
                                (get_uicapi(ccode1, ccode2, ayear, user_selections) > get_uicapj(ccode1, ccode2, ayear, user_selections) )
                              )
                            )
                          )
                        )
                     then get_EQ := 1;
                 AcqB: if (( (get_UjCapj(ccode1, ccode2, ayear, user_selections) > get_UjWari(ccode1, ccode2, ayear, user_selections)) and
                           ( ( (get_UiCapj(ccode1, ccode2, ayear, user_selections) > get_UiNego(ccode1, ccode2, ayear, user_selections)) and
                               (get_UjNego(ccode1, ccode2, ayear, user_selections) > get_UjCapi(ccode1, ccode2, ayear, user_selections)) and
                               (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections)) ) or
                             ( (get_UiCapj(ccode1, ccode2, ayear, user_selections) > get_UiNego(ccode1, ccode2, ayear, user_selections)) and
                               (get_UiWarj(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections)) ) or
                             ( (get_UiCapj(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections)) and
                               (get_UjCapi(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections)) and
                               (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections)) )
                           )
                         )   or
                         ( (get_UjWari(ccode1, ccode2, ayear, user_selections) > get_UjCapj(ccode1, ccode2, ayear, user_selections)) and
                              (get_UiWari(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections)) and
                              (get_UjCapi(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections)) and
                              (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections)) and
                           (get_UjAcqj(ccode1, ccode2, ayear, user_selections) > get_UjWari(ccode1, ccode2, ayear, user_selections))
                         )
                        )
                     then get_EQ := 1;
                 CapA: get_EQ := 0;
                 CapB: get_EQ := 0;
                 Nego: if ((get_EQSQ(ccode1, ccode2, ayear, user_selections) =0) and (get_EQAcqA(ccode1, ccode2, ayear, user_selections) =0) and
                           (get_EQAcqB(ccode1, ccode2, ayear, user_selections) =0) and (get_EQCapA(ccode1, ccode2, ayear, user_selections) =0) and
                           (get_EQCapB(ccode1, ccode2, ayear, user_selections) =0) and (get_EQWarA(ccode1, ccode2, ayear, user_selections) =0) and
                           (get_EQWarB(ccode1, ccode2, ayear, user_selections) =0)) then get_EQ := 1;
                 WarA:  {conditions from WR p. 72}
                        {Actual conditions for predicting WarA involve 4 conditions.  However, only
                         3 can be assessed, I think because CapA > WarB for A is mutually exclusive (by consturction?)
                         from WarA>AcqA for A.  I believe you could substitute the other condition.     }
                     if ((get_UiWari(ccode1, ccode2, ayear, user_selections) > get_UiAcqi(ccode1, ccode2, ayear, user_selections)) and
                         (get_UjCapi(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections)) and
                         (get_UjWari(ccode1, ccode2, ayear, user_selections) > get_UjAcqj(ccode1, ccode2, ayear, user_selections))
                        ) then get_EQ := 1;
                        {Alternative condition if other component is dropped:}
                        {if ((get_UiCapi>get_UiWarj) and
                            (get_UjCapi>get_UjNego) and (get_UjWari>get_UjAcqj) ) get_EQWarA := 1
                         else get_EQWarA := 0;    }
                 WarB: get_EQ := 0;
                 end;       {list of eq}
              end   {logical}
           else begin
                 EUGeneError ('Unrecognized game solution method observed in get EQ procedures, for equilibrium '+inttostr(ord(EQ))+'.  Equilibrium set to 0.  Notify programmer.',0, continue, error_log);
                 get_eq := missing_value;
              end;
         end;       {case}
      end   {all_in_range}
      else get_EQ := missing_value;
   end;    {get_EQ}


   function TEUWarReason_array_obj.get_EQSQ (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean) : dummy_type;
      begin
         get_EQSQ := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               if user_selections.eu_calculation_info.equilibrium_solution = induction then
                  begin
                     setUtilityArray(ccode1, ccode2, ayear, user_selections);
                     if solveOriginal(1, SolvingUtilityArray) = SQ
                     then get_EQSQ := 1
                     else get_EQSQ := 0;
                  end
               else
                  begin   {solveOriginal from logical formulas}
                     if (  (get_UiSQ(ccode1, ccode2, ayear, user_selections) > get_UiNego(ccode1, ccode2, ayear, user_selections) ) and
                           ( ( (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjWari(ccode1, ccode2, ayear, user_selections) > get_UjCapj(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjNego(ccode1, ccode2, ayear, user_selections) > get_UjCapi(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjSQ(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections) )            )
                             or
                             ( (get_UiNego(ccode1, ccode2, ayear, user_selections) > get_UiCapj(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjCapj(ccode1, ccode2, ayear, user_selections) > get_UjWari(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjNego(ccode1, ccode2, ayear, user_selections) > get_UjCapi(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjSQ(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections) )            )
                             or
                             ( (get_UiWarj(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjWari(ccode1, ccode2, ayear, user_selections) > get_UjCapj(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjSQ(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections) )            )
                             or
                             ( (get_UiNego(ccode1, ccode2, ayear, user_selections) > get_UiCapj(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UiWarj(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjCapj(ccode1, ccode2, ayear, user_selections) > get_UjWari(ccode1, ccode2, ayear, user_selections) ) and
                               (get_UjSQ(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections) )            )
                           )
                        ) then get_EQSQ := 1
                     else get_EQSQ := 0;
                  end;
            end
            else get_EQSQ := missing_value;
      end;

   function TEUWarReason_array_obj.get_EQNego (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean) : dummy_type;
      begin
         get_EQNego := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               if user_selections.eu_calculation_info.equilibrium_solution = induction then
                  begin
                     setUtilityArray(ccode1, ccode2, ayear, user_selections);
                     if solveOriginal(1, SolvingUtilityArray) = Nego
                     then get_EQNego := 1
                     else get_EQNego := 0;
                  end
               else
                  begin   {solveOriginal from logical formulas}
                     if ((get_EQSQ(ccode1, ccode2, ayear, user_selections) =0) and (get_EQAcqA(ccode1, ccode2, ayear, user_selections) =0) and
                         (get_EQAcqB(ccode1, ccode2, ayear, user_selections) =0) and (get_EQCapA(ccode1, ccode2, ayear, user_selections) =0) and
                         (get_EQCapB(ccode1, ccode2, ayear, user_selections) =0) and (get_EQWarA(ccode1, ccode2, ayear, user_selections) =0) and
                         (get_EQWarB(ccode1, ccode2, ayear, user_selections) =0)) then get_EQNego := 1
                     else get_EQNego := 0;
                  end;
            end
            else get_EQNego := missing_value;
      end;

   function TEUWarReason_array_obj.get_EQAcqA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean) : dummy_type;
      {conditions from W+R p. 80 and updated conversations with BdM}
    var tempeq1, tempeq2 : dummy_type;
      begin
         get_EQAcqA := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               if user_selections.eu_calculation_info.equilibrium_solution = induction then
                  begin
                     setUtilityArray(ccode1, ccode2, ayear, user_selections);
                     if solveOriginal(1, SolvingUtilityArray) = AcqA
                     then get_EQAcqA := 1
                     else get_EQAcqA := 0;
                  end
               else
                  begin   {solveOriginal from logical formulas}
                     if ( ( (get_uiacqi(ccode1, ccode2, ayear, user_selections) > get_uiwari(ccode1, ccode2, ayear, user_selections) ) and
                            (get_uicapi(ccode1, ccode2, ayear, user_selections) > get_uiwarj(ccode1, ccode2, ayear, user_selections) ) and
                            (get_ujcapi(ccode1, ccode2, ayear, user_selections) > get_ujnego(ccode1, ccode2, ayear, user_selections) ) and
                            (get_ujwari(ccode1, ccode2, ayear, user_selections) > get_ujacqj(ccode1, ccode2, ayear, user_selections) ) )
                          or
                          ( (get_uicapi(ccode1, ccode2, ayear, user_selections) > get_uiwari(ccode1, ccode2, ayear, user_selections) ) and
                            (get_ujcapi(ccode1, ccode2, ayear, user_selections) > get_ujnego(ccode1, ccode2, ayear, user_selections) ) and
                            ( (get_ujwari(ccode1, ccode2, ayear, user_selections) > get_ujcapj(ccode1, ccode2, ayear, user_selections) ) or
                              ( (get_ujcapj(ccode1, ccode2, ayear, user_selections) > get_ujwari(ccode1, ccode2, ayear, user_selections) ) and
                                (get_uicapi(ccode1, ccode2, ayear, user_selections) > get_uicapj(ccode1, ccode2, ayear, user_selections) )
                              )
                            )
                          )
                        )
                     then get_EQAcqA := 1
                     else get_EQAcqA := 0;
                  end;
            end
            else get_EQAcqA := missing_value;
      end;

   function TEUWarReason_array_obj.get_EQAcqB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean) : dummy_type;
      {conditions from WRp. 81 and figuring out eq from BdM Msgs}
      begin
         get_EQAcqB := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               if user_selections.eu_calculation_info.equilibrium_solution = induction then
                  begin
                     setUtilityArray(ccode1, ccode2, ayear, user_selections);
                     if solveOriginal(1, SolvingUtilityArray) = AcqB
                     then get_EQAcqB := 1
                     else get_EQAcqB := 0;
                  end
               else
                  begin   {solveOriginal from logical formulas}
                     if (( (get_UjCapj(ccode1, ccode2, ayear, user_selections) > get_UjWari(ccode1, ccode2, ayear, user_selections)) and
                           ( ( (get_UiCapj(ccode1, ccode2, ayear, user_selections) > get_UiNego(ccode1, ccode2, ayear, user_selections)) and
                               (get_UjNego(ccode1, ccode2, ayear, user_selections) > get_UjCapi(ccode1, ccode2, ayear, user_selections)) and
                               (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections)) ) or
                             ( (get_UiCapj(ccode1, ccode2, ayear, user_selections) > get_UiNego(ccode1, ccode2, ayear, user_selections)) and
                               (get_UiWarj(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections)) ) or
                             ( (get_UiCapj(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections)) and
                               (get_UjCapi(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections)) and
                               (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections)) )
                           )
                         )   or
                         ( (get_UjWari(ccode1, ccode2, ayear, user_selections) > get_UjCapj(ccode1, ccode2, ayear, user_selections)) and
                              (get_UiWari(ccode1, ccode2, ayear, user_selections) > get_UiCapi(ccode1, ccode2, ayear, user_selections)) and
                              (get_UjCapi(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections)) and
                              (get_UiCapi(ccode1, ccode2, ayear, user_selections) > get_UiWarj(ccode1, ccode2, ayear, user_selections)) and
                           (get_UjAcqj(ccode1, ccode2, ayear, user_selections) > get_UjWari(ccode1, ccode2, ayear, user_selections))
                         )
                        )
                     then get_EQAcqB := 1
                     else get_EQAcqB := 0;
                  end;
            end
            else get_EQAcqB := missing_value;
      end;

   function TEUWarReason_array_obj.get_EQCapA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean) : dummy_type;
         {international interaction game in W+R never expects Cap by either A or B}
      begin
         get_EQCapA := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               if user_selections.eu_calculation_info.equilibrium_solution = induction then
                  begin
                     setUtilityArray(ccode1, ccode2, ayear, user_selections);
                     if solveOriginal(1, SolvingUtilityArray) = CapA
                     then get_EQCapA := 1
                     else get_EQCapA := 0;
                  end
               else
               get_EQCapA := 0;
            end
            else get_EQCapA := missing_value;
      end;

   function TEUWarReason_array_obj.get_EQCapB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean) : dummy_type;
         {international interaction game in W+R never expects Cap by either A or B}
      begin
         get_EQCapB := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               if user_selections.eu_calculation_info.equilibrium_solution = induction then
                  begin
                     setUtilityArray(ccode1, ccode2, ayear, user_selections);
                     if solveOriginal(1, SolvingUtilityArray) = CapB
                     then get_EQCapB := 1
                     else get_EQCapB := 0;
                  end
               else
               get_EQCapB := 0;
            end
            else get_EQCapB := missing_value;
      end;

   function TEUWarReason_array_obj.get_EQWarA (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean) : dummy_type;
      begin
         get_EQWarA := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               if user_selections.eu_calculation_info.equilibrium_solution = induction then
                  begin
                     setUtilityArray(ccode1, ccode2, ayear, user_selections);
                     if solveOriginal(1, SolvingUtilityArray) = WarA
                     then get_EQWarA := 1
                     else get_EQWarA := 0;
                  end
               else
                  begin   {solveOriginal from logical formulas}
                       {conditions from WR p. 72}
                      {Actual conditions for predicting WarA involve 4 conditions.  However, only
                       3 can be assessed, I think because CapA > WarB for A is mutually exclusive (by consturction?)
                       from WarA>AcqA for A.  I believe you could substitute the other condition.     }
                     if ((get_UiWari(ccode1, ccode2, ayear, user_selections) > get_UiAcqi(ccode1, ccode2, ayear, user_selections)) and
                         (get_UjCapi(ccode1, ccode2, ayear, user_selections) > get_UjNego(ccode1, ccode2, ayear, user_selections)) and
                         (get_UjWari(ccode1, ccode2, ayear, user_selections) > get_UjAcqj(ccode1, ccode2, ayear, user_selections))
                        ) then get_EQWarA := 1
                     else get_EQWarA := 0;

                     {Alternative condition if other component is dropped:}
                     {if ((get_UiCapi>get_UiWarj) and
                         (get_UjCapi>get_UjNego) and (get_UjWari>get_UjAcqj) ) get_EQWarA := 1
                     else get_EQWarA := 0;    }
                  end;
            end
            else get_EQWarA := missing_value;
      end;

   function TEUWarReason_array_obj.get_EQWarB (ccode1, ccode2 : ccode_range; ayear : year_range; const user_selections : user_selection_type; error_check : boolean) : dummy_type;
         {international interaction game in W+R never expects War started by B}
      begin
         get_EQWarB := missing_value;
         if (all_in_range (ccode1, ccode2, ayear, error_check) and all_valid (ccode1, ccode2, ayear, user_selections)) then
            begin
               if user_selections.eu_calculation_info.equilibrium_solution = induction then
                  begin
                     setUtilityArray(ccode1, ccode2, ayear, user_selections);
                     if solveOriginal(1, SolvingUtilityArray) = WarB
                     then get_EQWarB := 1
                     else get_EQWarB := 0;
                  end
               else
               get_EQWarB := 0;
            end
            else get_EQWarB := missing_value;
      end;

      {  ---------------------------------------------------------   }

end.    {unit eutypes2}
