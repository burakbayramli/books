/* Example of a stratified sample analysis using SAS */
/* 2015-07-04 Updated to more recent sas code */

/* On each of two days, a sample of vessels were assigned 
   observers who counted the number of sockeye salmon caught
   in that day. On the second day, a new set of vessels was observed. */

/*---partxxx lines are for the LaTex file of my notes and can be ignored */

title 'Number of sockeye caught - example of stratified simple random sampling';
ods pdf file='sockeye.pdf' style=styles.printer;

*---part001b; 
data sockeye;   /* read in the data */
   infile 'sockeye.csv' dlm=',' dsd missover firstobs=2;
   length date $8.;
   input date $ sockeye;
   /* compute the sampling weight. In general,
      these will be different for each stratum */
   if date = '29-Jul' then sampweight = 250/15; 
   if date = '30-Jul' then sampweight = 250/15;  
*---part001e;

*---part002b;
data n_boats;  /* you need to specify the stratum sizes if you want stratum totals */
   length date $8.;
   date = '29-Jul'; _total_=250; output;  /* the stratum sizes must be variable _total_ */
   date = '30-Jul'; _total_=250; output;
run;
*---part002e;

proc print data=sockeye;
   title2 'raw data from the survey';
run;

proc print data=n_boats;
   title2 'number of boats in each stratum';
run;

*---part003b;
proc surveymeans data=sockeye 
   N = n_boats  /* dataset with the stratum population sizes present */
   mean clm     /* average catch/boat along with standard error */
   sum clsum ;  /* request estimates of total */ ;

   strata date / list; /* identify the stratum variable */
   var sockeye;        /* which variable to get estimates for */
   weight sampweight;
   ods output stratainfo=stratainfo;
   ods output statistics=sockeyeresults;
run;
*---part003e;

*---part004b;
proc surveymeans data=sockeye 
   N = n_boats  /* dataset with the stratum population sizes present */
   mean clm     /* average catch/boat along with standard error */
   sum clsum ;  /* request estimates of total */ ;
   by date;
   var sockeye;        /* which variable to get estimates for */
   weight sampweight;
   ods output stratainfo=stratainfosep;
   ods output statistics=sockeyeresultssep;
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

ods tagsets.mylatex file="sockeye-SAS-results.tex" (notop nobot) /* newfile=table */;
proc print data=sockeyeresults noobs label split=" ";
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

ods tagsets.mylatex file="sockeye-SAS-resultssep.tex" (notop nobot) /* newfile=table */;
proc print data=sockeyeresultssep noobs label split=" ";
   var date varname mean stderr lowerclmean upperclmean sum stddev lowerclsum upperclsum; 
   format mean lowerCLMean stderr upperCLmean 10.1  Sum LowerCLSum stddev UpperCLsum 10.0;
   label LowerCLMean='LCL Mean';
   label UpperCLMean='UCL Mean';
   label stderr     ='SE Mean';
   label LowerCLSum ='LCL Sum';
   label upperCLsum ='UCL Sum';
   label stddev     ='SE sum';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file="sockeye-SAS-stratainfo.tex" (notop nobot) /* newfile=table */;
proc print data=stratainfo noobs label split=" ";
run;
ods tagsets.mylatex close;



