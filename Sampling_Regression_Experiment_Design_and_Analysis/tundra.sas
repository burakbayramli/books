/* Estimate the number of tundra swans in Bristol Bay, Alaska */
/* 2015-07-04 Updated to more recent sas code */

/* tagsets.tablesonlylatex used for the LaTex file for the course notes*/
/* *---partxxxa used to delineate the code for LaTex */


title 'Estimate the number of tundra swans';
ods pdf file='tundra.pdf' style=styles.printer;

*---part001b;
data swans;
   infile 'tundra.csv' dlm=',' dsd missover firstobs=2;
   length survey_unit $10 stratum $1;;
   input survey_unit $ stratum $ area num_flocks num_single num_pairs;
   num_swans = num_flocks + num_single + 2*num_pairs;
*---part001e;

*---part002b;
data total_survey_units;
   length stratum $1.;
   input stratum $ _total_; /* must use _total_ as variable name */
   datalines;
h  60
m  68
l  58 
;;;;
*---part002e;

proc sort data=swans; by stratum;
*---part003b;
proc means data=swans noprint;
   by stratum;
   var num_swans;
   output out=n_units n=n;
run;
*---part003e;
proc sort data=total_survey_units; by stratum;
*---part004b;
data swans;
   merge swans total_survey_units n_units;
   by stratum;
   sampling_weight = _total_ / n; 
run;
*---part004e;

proc print data=swans;
   title2 'raw data';
run;
 
proc print data=total_survey_units;
  title2 'number of survey units in each stratum';
run;



*---part005b;
/* first estimate the numbers in each stratum */
proc surveymeans data=swans
                 total=total_survey_units /* inflation factors */
                 sum clsum mean clm;
   by stratum;   /* separate estimates by stratum */
   var num_swans;
   weight sampling_weight;
   ods output statistics=tundraresultssep;
   ods output stratinfo =tundrastratainfo;
run;
*---part005e;


*---part006b;
/* now to estimate the grand total */
proc surveymeans data=swans
                 total=total_survey_units /* inflation factors for each stratum */
                 sum clsum  mean clm;     /* want to estimate grand totals */
   title2 'Estimate total number of swans';
   strata stratum /list;    /*  which variable define the strata */
   var    num_swans;        /*  which variable to analyze        */
   weight sampling_weight;  /*  sampling weight for each obs */
   ods output statistics=tundraresults;
run;
*---part006e;
 




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

ods tagsets.mylatex file="tundra-SAS-results.tex" (notop nobot) /* newfile=table */;
proc print data=tundraresults noobs label split=" ";
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

ods tagsets.mylatex file="tundra-SAS-resultssep.tex" (notop nobot) /* newfile=table */;
proc print data=tundraresultssep noobs label split=" ";
   var stratum varname mean stderr lowerclmean upperclmean sum stddev lowerclsum upperclsum; 
   format mean lowerCLMean stderr upperCLmean 10.1  Sum LowerCLSum stddev UpperCLsum 10.0;
   label LowerCLMean='LCL Mean';
   label UpperCLMean='UCL Mean';
   label stderr     ='SE Mean';
   label LowerCLSum ='LCL Sum';
   label upperCLsum ='UCL Sum';
   label stddev     ='SE sum';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file="tundra-SAS-stratainfo.tex" (notop nobot) /* newfile=table */;
proc print data=tundrastratainfo noobs label split=" ";
run;
ods tagsets.mylatex close;


