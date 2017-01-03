/* Estimate the number of grubs using a post-stratified design */
/* 2015-07-04 CJS Update for latest version of SAS */

/* A survey of 20 x 1 m2 plots was taken from a study area of 100 m2. In each quadrat
the number of grubs was measured. At the same time, a post-stratification into
high and low quality habitats was done. It was subsequently determined that there are
30 m2 of high quality and 70 m2 of low quality habitat in the study area. */

title 'Estimate the number of grubs using a post-stratified SRS design';
ods pdf file='post-stratify-SAS.pdf' style=styles.printer;

*---part001b;
data grubs;
   infile 'post-stratify.csv' dlm=',' dsd missover firstobs=2;
   length post_stratum $1.;
   input grubs post_stratum;
run;
*---part001e;

data grubs1; /* make a copy for listing later on */
   set grubs;
run;

data total_survey_units;
   length post_stratum $1.;
   input post_stratum $ _total_; /* must use _total_ as variable name */
   datalines;
h  30 
l  70
;;;;


/* first use a simple non-stratified analysis */
/* Determine the sampling weight */
proc means data=grubs  noprint;
   var grubs;
   output out=n_units n=n;
proc means data=total_survey_units noprint; 
   var _total_;
   output out=surveysize sum=_grand_total_;
data grubs;
   set grubs;
   obs=1;
   set n_units point=obs;
   set surveysize point=obs;
   sampling_weight_overall = _grand_total_ / n;
   drop n _grand_total_;
run;
 
proc print data=grubs;
   title2 'data set used for overall estimate';
run;

title2 'Estimate total number of grubs using no-stratification';
*---part002b;
proc surveymeans data=grubs
                 total=100 /* inflation factors */
                 sum clsum mean clm;
   var grubs;
   weight sampling_weight_overall;
   ods output statistics=poststratifyresultssimple;
run;
*---part002e;



/* Get some summary statistics about each post stratum */
proc sort data=grubs; by post_stratum; run;

*---part003b;
proc tabulate data=grubs;
   class post_stratum;
   var grubs;
   table post_stratum, grubs*(n*f=5.0 mean*f=5.2 std*f=5.2);
run;
*---part003e;



/* Now to so a stratified analysis based on the oberved number of quadrats in each stratum */
/* First determine the sampling weights */
proc sort data=grubs; by post_stratum;
proc means data=grubs  noprint;
   by post_stratum;
   var grubs;
   output out=n_units n=n;
proc sort data=total_survey_units; by post_stratum;
data grubs;
   merge grubs total_survey_units n_units;
   by post_stratum;
   sampling_weight_post_strata = _total_ / n;
run;
proc print data=grubs;
   title2 'dataset used for post-stratified analysis';
run;

title2 'Estimate total number of grubs using a post-stratified SRS';
*---part004b;
proc surveymeans data=grubs
                 total=total_survey_units /* inflation factors for each stratum */
                 sum clsum  mean clm;     /* want to estimate grand totals */
   strata post_stratum /list;    /*  which variable define the strata */
   var    grubs;              /*  which variable to analyze        */
   weight sampling_weight_post_strata;  /*  sampling weight for each obs */
   ods output statistics=poststratifyresults;;
run;
*---part004e;


ods pdf close;



/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=ttest1;
 list /levels=all;
run;


ods tagsets.mylatex file="post-stratify-SAS-data.tex" (notop nobot) /* newfile=table */;
proc print data=grubs1(obs=10);
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file="post-stratify-SAS-stratasum.tex" (notop nobot) /* newfile=table */;
proc tabulate data=grubs;
   class post_stratum;
   var grubs;
   table post_stratum, grubs*(n*f=5.0 mean*f=5.2 std*f=5.2);
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file="post-stratify-SAS-resultssimple.tex" (notop nobot) /* newfile=table */;
proc print data=poststratifyresultssimple noobs label split=" ";
   var varname mean stderr lowerclmean upperclmean sum stddev lowerclsum upperclsum; 
   format mean lowerCLMean stderr upperCLmean 10.2  Sum LowerCLSum stddev UpperCLsum 10.0;
   label LowerCLMean='LCL Mean';
   label UpperCLMean='UCL Mean';
   label stderr     ='SE Mean';
   label LowerCLSum ='LCL Sum';
   label upperCLsum ='UCL Sum';
   label stddev     ='SE sum';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file="post-stratify-SAS-results.tex" (notop nobot) /* newfile=table */;
proc print data=poststratifyresults noobs label split=" ";
   var varname mean stderr lowerclmean upperclmean sum stddev lowerclsum upperclsum; 
   format mean lowerCLMean stderr upperCLmean 10.1  Sum LowerCLSum stddev UpperCLsum 10.0;
   label LowerCLMean='LCL Mean';
   label UpperCLMean='UCL Mean';
   label stderr     ='SE Mean';
   label LowerCLSum ='LCL Sum';
   label upperCLsum ='UCL Sum';
   label stddev     ='SE sum';
run;
ods tagsets.mylatex close;



