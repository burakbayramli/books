
/* This is a power analysis for monitoring for fry example presented in the notes */



/* Read in the scenarios (i.e. the design specifications) and generate the
   data for use in the power analysis. The data are expected values for
   each observed data point. These are then passed to Proc Mixed for 'analysis'
   as outlined in the Stroup paper */
 
/* This is the monitoring for fry example presented in the notes */

/* The input data are the 
     Alpha level (usually .05 or .10)
     Variance components
        std_site  -> site to site STANDARD DEVIATION
        std_year  -> year to year STANDARD DEVIATION
        std_site_year -> site-year interaction STANDARD DEVIATION
        std_Resid -> within site sub-sampling STANDARD DEVIATION
     Sub-sampling sample sizes.
        We allow for different sample sizes in the treatment-time combinations
        but all sites in that combination must have the same sample size.
        It is possible to generalize this; contact me for details.
        n_TA   -> number of Resids in Treatment-After  combination
        n_TB   -> number of Resids in Treatment-Before combination
        n_CA   -> number of Resids in Control-After    combination
        n_CB   -> number of Resids in Control-Before   combination
     Number of sites
        We allow for a different number of sites for Treatment and Control areaa
        ns_T     -> number of treatment sites
        ns_C     -> number of control sites
     Number of years of monitoring before/after impact
        ny_b
        ny_a
     Marginal Means
        These are used to form the BACI contrast of interest
        mu_TA, mu_TB, mu_CA, mu_CB
 
*/

options nodate noovp orientation=landscape;
title 'Fry example BACI power analysis';
ods pdf file='baci-fry-power-SAS.pdf';


%include '../baci-power.sas'; /* get the baci power macro */

*---part500b;        
proc datasets;   /* delete any existing data set */
   delete all_power;
run;

data scenarios;
   input alpha 
         sdSite sdYear sdSiteYear sdResid
         n_TA n_TB n_CA n_CB
         ns_T  ns_C
         ny_B  ny_A
         mu_TA mu_TB mu_CA mu_CB;
   datalines;
.05   .75 .25 .10 .75   3  3  3  3   3  3   3  2    5.0 5.5 4.5 4.5
.05   .75 .25 .10 .75   6  6  6  6   3  3   3  2    5.0 5.5 4.5 4.5
.05   .75 .25 .10 .75   9  9  9  9   3  3   3  3    5.0 5.5 4.5 4.5
.05   .75 .25 .10 .75   6  6  6  6   3  3   3  4    5.0 5.5 4.5 4.5
run;
      
options mprint;

data _null_;  /* compute the power for the various scenarios */
   set scenarios;
  call execute(
     '%baci_power(alpha='         || alpha ||
     "     , sdSite= "       || sdSite ||
     "     , sdYear= "       || sdYear ||
     "     , sdSiteYear="    || sdSiteYear ||
     "     , sdResid="       || sdResid ||
     "     , n_TA="          || n_ta ||
     "     , n_TB="          || n_tb ||
     "     , n_CA="          || n_CA ||
     "     , n_CB="          || n_CB ||
     "     , ns_T="          || ns_t  ||
     "     , ns_C="          || ns_C  ||
     "     , ny_B="          || ny_B  ||
     "     , ny_A="          || ny_A  ||
     "     , mu_TA="         || mu_TA ||
     "     , mu_TB="         || mu_TB ||
     "     , mu_CA="         || mu_CA ||
     "     , mu_CB="         || mu_CB || ");"   );
*  put "***temp***" temp;
run;
*---part500e;

proc print data=all_power;
   title2 'Compute power';
run;

data scenarios;
   merge scenarios all_power;
run;
 
proc print data=scenarios split="_" label;
   title2 'Estimated power under various scenarios';
run;

proc print data=scenarios split="_" label;
   var   alpha 
         sdSite sdYear sdSiteYear sdResid
         n_TA n_TB n_CA n_CB
         ns_T  ns_C
         ny_B  ny_A
         mu_TA mu_TB mu_CA mu_CB  power;
run;
                 
ods pdf close;


/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;


ods tagsets.mycolorlatex file='baci-fry-power-SAS-500.tex' (notop nobot) stylesheet="sas.sty";
proc print data=scenarios split="_" label noobs;
   var   alpha 
         /* sdSite sdYear sdSiteYear sdResid */
         n_TA n_TB n_CA n_CB
         ns_T  ns_C
         ny_B  ny_A
         mu_TA mu_TB mu_CA mu_CB  power;
ods tagsets.mycolorlatex close;



