/* Compute power for a general BACI design using the method of Stroup */

/* This function computes the power for a BACI design with multiple treatment
  and control sites, multiple years measured before and after treatment is applied, and
  sub-sampling at each year-site combination.
 
  The information we need is:
      Alpha level (usually .05 or .10)
      Variance components (these were obtained from the an analysis of previous data)
         sdSite       -> site to site STANDARD DEVIATION
         sdYear      -> year to year STANDARD DEVIATION
         sdSiteYear -> site-year interaction STANDARD DEVIATION
         sdResid     -> within site sub-sampling STANDARD DEVIATION
      Sub-sampling sample sizes, i.e. the number of sub-samples taken at each site-period combinations.
        We allow for different sample sizes in the treatment-time combinations
         but all sites in that combination must have the same number of samples.
         It is possible to generalize this; contact me for details.
         n_TA   -> number of Resids in Treatment-After  combination
         n_TB   -> number of Resids in Treatment-Before combination
         n_CA   -> number of Resids in Control-After    combination
         n_CB   -> number of Resids in Control-Before   combination
      Number of sites
         We allow for a different number of sites for Treatment and Control areaa
         ns_T     -> number of treatment sites (i.e. downstram of the project)
         ns_C     -> number of control sites (i.e. upstream of the project)
      Number of years of monitoring before/after impact
         ny_B     -> number of years monitoring before project starts
         ny_A     -> number of years monitoring after  project starts
      Marginal Means
         These are used to form the BACI contrast of interest
         mu_TA, mu_TB, mu_CA, mu_CB (i.e mean of Treatment-After,
         Treatment-Before, Control-After, Control_before)
        These are chosen based on the size of impact that may be biologically important
*/ 

%macro baci_power(alpha, 
       sdSite, sdYear, sdSiteYear, sdResid,
             n_TA, n_TB, n_CA, n_CB,  
             ns_T, ns_C,
             ny_B, ny_A,
             mu_TA, mu_TB, mu_CA, mu_CB);

/* compute the variance components needed for Proc Mixed */
%let vcSite    = %sysevalf(&sdSite*&sdSite);
%let vcYear    = %sysevalf(&sdYear*&sdYear);
%let vcSiteYear= %sysevalf(&sdSiteYear*&sdSiteYear);
%let vcResid   = %sysevalf(&sdResid*&sdResid);

/* Generate the fake dataset with the specified means */
data dummy_data;
   alpha = &alpha;
   n_ta = &n_ta;
   n_tb = &n_tb;
   n_ca = &n_ca;
   n_cb = &n_cb;
   ns_t  = &ns_t;
   ns_c  = &ns_c;
   ny_a  = &ny_a;
   ny_b  = &ny_b;
   mu_ta= &mu_ta;
   mu_tb= &mu_tb;
   mu_ca= &mu_ca;
   mu_cb= &mu_cb;

   /* generate the data for the above scenario */
   treatment = 'T';
   do site=1 to ns_t;
      do year=1 to ny_a;
         time = "A";  /* after impact */
         do quadrat=1 to n_TA;
            mu = mu_TA;
            output;
         end;
      end;
      time = "B";  /* before impact */
      do year=1 to ny_b;
         do quadrat=1 to n_TB;
            mu = mu_TB;
            output;
         end;
      end;
   end;

   treatment = 'C';
   do site=1 to ns_c;
      do year=1 to ny_a;
         time = "A";  /* after impact */
         do quadrat=1 to n_CA;
            mu = mu_CA;
            output;
         end;
      end;
      do year=1 to ny_b;
         time = "B";  /* before impact */
         do quadrat=1 to n_CB;
            mu = mu_CB;
            output;
         end;
      end;
   end;
run;

ods listing close; run; /* turn the output off as it is not needed */
ods select none; run;
proc mixed data=dummy_data noprofile;
   title2 'Estimate the non-centrality parameter ';
   class treatment time site year quadrat;
   model mu =  treatment time treatment*time ;  /* the interaction effect is of interest in BACI designs */
   random site(treatment);
   random year(time);
%if &sdSiteYear >0 %then %do;
   random site*year(treatment*time);
%end;
   parms   (&vcSite) (&vcYear)
%if &sdSiteYear >0 %then %do;
           (&vcSiteYear)
%end;
            (&vcResid) / noiter;   /* this is where the estimated  variance components are specified */
   /* save the results to the ods datasets */
   ods output tests3=power_effects;
run;
ods listing; run;   /* turn the output back on */
ods select all; run;

data power_effects;
   set power_effects;
   if effect='treatment*time';    /* only the interaction effect is of interest in BACI designs */
run;

*proc print data=power_effects (obs=20);
*   title3 'some of the power effect from Proc Mixed';
*run;

/* now to compute approximations to the power */
data power;
   set power_effects;
   nc = numdf*Fvalue;  /* approximate non-centrality parameter */ 
   fcrit = finv(1-&alpha, numDF, denDF, 0);  /* estimate critical value */
   power = 1 - probf(fcrit, numdf, dendf, nc);  /* estimated power */
   attrib power label='Power' format=7.2;
   attrib nc    label='Non-centrality' format=7.1;
   attrib fcrit label='F-critical value' format=7.2;
   drop probF ;
run;

proc append base=all_power data=power force;

%mend baci_power;



