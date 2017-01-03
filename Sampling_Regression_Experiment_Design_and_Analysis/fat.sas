/* CRD - completely randomized design - subsampling - pseudoreplication */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;

/* An investigator wish to determine if the fat levels differ among four
   species of fish. She selects three fish of each species, and takes three
   samples of flesh from each fish to determine the fat content.   */

title 'Fat content - Subsampling in a single-factor CRD';
options nodate noovp orientation=landscape;

ods pdf file='fat.pdf';
goptions device=pdf colors=(black) rotate=landscape;

*---part001b; 
data fish;
   infile 'fat.csv' dlm=',' dsd missover firstobs=2;
   input species $ fish sample fat;
run;
*---part001e;

proc print data=fish(obs=10);
   title2 'raw data';
run;


/******************************************* BALANCED CASE ******************************/

proc tabulate data=fish;
   title2 'simple data summary - balanced';
   class species fish sample;
   var fat;
   table fat*(n*f=5.0 mean*f=6.1 std*f=6.1), species*fish;
run;


/*********** APPROACH 1 - use the means over sub-samples *********/
/* Either Proc Mixed or Proc Glm could be used here. I prefer Mixed */
*---part020b;
proc sort data=fish;
   by species fish;
run;

proc means data=fish noprint;  /* compute the average over the sub-samples for each fish */
   by species fish;
   var fat;
   output out=mean_fat mean=mean_fat;
run;
*---part020e;

ods document name=mixed1(write);
*---part030b;
ods graphics on;
proc mixed data=mean_fat plots=all;
   title2 'Analysis using averages - balanced';
   class species;
   model mean_fat=species / ddfm=kr;
   lsmeans species / diff adjust=tukey cl;
   ods output tests3 =Mixed1Tests;
   ods output lsmeans=Mixed1LSmeans;
   ods output diffs  =Mixed1Diffs;
run;
ods graphics off;
*---part030e;
ods document close;


/* Get a joined lines plot */
*---part040b;
%include '../../pdmix800.sas';
%pdmix800(mixed1diffs,mixed1lsmeans,alpha=0.05,sort=yes);
*---part040e;



/************ APPROACH 2 - model all of the data ***************/

/* Do NOT use GLM - it may give incorrect answers              */
/* As noted in the SAS documentation for LSMEANS in GLM
   Note: PROC GLM uses only the information pertaining to 
   expected mean squares when you specify the TEST option in 
   the RANDOM statement and, even then, only in the extra  
   tests produced by the RANDOM statement. Other features in the 
   GLM procedure including the results of the LSMEANS and ESTIMATE 
   statements assume that all effects are fixed, so that all tests 
   and estimability checks for these statements are based on a 
   fixed-effects model, even when you use a RANDOM statement. 
   Therefore, you should use the MIXED procedure to compute tests 
   involving these features that take the random effects into account; 
   see the section PROC GLM versus PROC MIXED for Random-Effects Analysis 
   and Chapter 58, The MIXED Procedure, for more information.  */ 


/* Note that all the tests have the same test-statistic and p-values,
   the estimate of the means are identical along with the estimated
   se, and estimates of differences among species are also identical
   with the approach using averages */

ods document name=mixed2(write);
*---part130b;
ods graphics on; 
proc mixed data=fish plots=all;
   title2 'Analysis using raw data using MIXED - balanced';
   /* Notice in MIXED random effects do NOT appear in model statement */
   /* If you create a unique label for each fish, you could also specify
      the random effect as simply fish.id */
   class species fish sample;
   model fat=species  / ddfm=kr;
   random fish(species);
   lsmeans species / diff cl adjust=tukey;
   ods output tests3 =Mixed2Tests;
   ods output lsmeans=Mixed2LSmeans;
   ods output diffs  =Mixed2Diffs;
   ods output covparms=Mixed2CovParms;
run;
ods graphics off;
*---part130e;
ods document close;


/* Get a joined lines plot */
*---part140b;
%include '../../pdmix800.sas';
%pdmix800(mixed2diffs,mixed2lsmeans,alpha=0.05,sort=yes);
*---part140e;



/******** Now to repeat everything for the UNBALANCED case ******************************************/
data fish2;
   retain count 0;
   set fish;
   count = count + 1;
   if count in (2,5,6,10,11,12) then delete;
run;

proc tabulate data=fish2;
   title2 'simple data summary - UNbalanced';
   class species fish sample;
   var fat;
   table fat*(n*f=5.0 mean*f=6.1 std*f=6.1), species*fish;
run;


/*********** APPROACH 1 - use the means over sub-samples *********/

proc sort data=fish2;
   by species fish;
run;

proc means data=fish2 noprint;  /* compute the average over the sub-samples for each fish */
   by species fish;
   var fat;
   output out=mean_fat2 mean=mean_fat;
run;

ods graphics on;
proc mixed data=mean_fat2 plots=all;
   title2 'Analysis using averages - UNbalanced';
   class species;
   model mean_fat=species / ddfm=kr;
   lsmeans species / diff cl adjust=tukey;
   ods output tests3 =Mixed3Tests;
   ods output lsmeans=Mixed3LSmeans;
   ods output diffs  =Mixed3Diffs;
   ods output covparms=Mixed3CovParms;
run;
ods graphics off;

%include '../../pdmix800.sas';
%pdmix800(mixed3diffs,mixed3lsmeans,alpha=0.05,sort=yes);



/************ APPROACH 2 - model all of the data ***************/
/* Do NOT use GLM - it may give incorrect answers              */
/* As noted in the SAS documentation for LSMEANS in GLM
   Note: PROC GLM uses only the information pertaining to 
   expected mean squares when you specify the TEST option in 
   the RANDOM statement and, even then, only in the extra  
   tests produced by the RANDOM statement. Other features in the 
   GLM procedure—including the results of the LSMEANS and ESTIMATE 
   statements—assume that all effects are fixed, so that all tests 
   and estimability checks for these statements are based on a 
   fixed-effects model, even when you use a RANDOM statement. 
   Therefore, you should use the MIXED procedure to compute tests 
   involving these features that take the random effects into account; 
   see the section PROC GLM versus PROC MIXED for Random-Effects Analysis 
    and Chapter 58, The MIXED Procedure, for more information.  */ 


ods graphics on;  
proc mixed data=fish2 plots=all;
   title2 'Analysis using raw data using MIXED - UNbalanced';
   /* Notice in MIXED random effects do NOT appear in model statement */
   class species fish sample;
   model fat=species  / ddfm=kr;
   random fish(species);
   lsmeans species / pdiff cl adjust=tukey;
   ods output tests3 =Mixed4Tests;
   ods output lsmeans=Mixed4LSmeans;
   ods output diffs  =Mixed4Diffs;
   ods output covparms=Mixed4CovParms;
run;
ods graphics off;

%include '../../pdmix800.sas';
%pdmix800(mixed4diffs,mixed4lsmeans,alpha=0.05,sort=yes);


ods pdf close;



/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=mixed1;
   list /levels=all;
run;

ods tagsets.mycolorlatex file='fishfat-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=fish(obs=10);
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fishfat-SAS-020.tex' (notop nobot);
proc print data=mean_fat(obs=10 drop=_type_ _freq_);
run;
ods tagsets.mycolorlatex close;



ods tagsets.mycolorlatex file='fishfat-SAS-030.tex' (notop nobot);
proc print data=Mixed1Tests noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fishfat-SAS-040.tex' (notop nobot);
proc print data=Mixed1LSmeans noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fishfat-SAS-045a.tex' (notop nobot);
proc print data=Mixed1Diffs(drop=df tvalue probt lower upper) noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fishfat-SAS-045b.tex' (notop nobot);
%pdmix800(mixed1diffs,mixed1lsmeans,alpha=0.05,sort=yes);
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fishfat-SAS-050' reset=index;
proc document name=mixed1;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;




ods tagsets.mycolorlatex file='fishfat-SAS-130.tex' (notop nobot);
proc print data=Mixed2Tests noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fishfat-SAS-135.tex' (notop nobot);
proc print data=Mixed2CovParms noobs label split=" ";
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='fishfat-SAS-140.tex' (notop nobot);
proc print data=Mixed2LSmeans noobs label split=" ";
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fishfat-SAS-145a.tex' (notop nobot);
proc print data=Mixed2Diffs(drop=df tvalue probt lower upper) noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='fishfat-SAS-145b.tex' (notop nobot);
%pdmix800(mixed2diffs,mixed2lsmeans,alpha=0.05,sort=yes);
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='fishfat-SAS-150' reset=index;
proc document name=mixed2;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;


