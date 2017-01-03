/* 2012-08-27 */
/* Invertebrate data from a simple before/after analysis.

   Two streams were measured at a small hydro electric power project. At each stream
   multiple quadrats were taken on the steam, and the number of invertebrates
   was counted.

   After 5 years, the hydro electric plant started up, and an additional 5 years
   of data were collected.
 
   Is there evidence of a change in abundance after the plant starts?

   Note that this is NOT a BACI design, and is a VERY poor substitute for such.
   The key problem is that changes may have occured over time unrelated
   to the impact, so a "change" may simple be natural. 

   Also, in this analysis we have ignored autocorrelation which can be
   a serious problem in long-term studies. */


/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;

options orientation=landscape nodate noovp;
ods pdf file='invert-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

title 'Invertebrate Before/After Analysis';

*---part001b;
data invert;
   infile 'invert.csv' dlm=',' dsd missover firstobs=2;
   length period $10.;
   input year period stream quadrat count;
run;
*---part001e;
 
proc print data=invert (obs=10);
   title2 'Part of the raw data';
run;

ods document name=summarystat(write);
*---part010b;
proc tabulate data=invert;
   title2 'Preliminary summary statistics';
   class stream year period;
   var count;
   table year*period, stream*count*(n*f=5.0 mean*f=5.1 std*f=5.1);
run;
*---part010e;
ods document close;

/* Plot the log(mean) vs log(std) to see if transformation is needed */
proc sort data=invert;
   by stream year period;
run;
proc means data=invert noprint;
   by stream year period;
   var count;
   output out=mean_count mean=mean_count std=std_count;
run;
data mean_count;
   set mean_count;
   log_mean_count = log(mean_count);
   log_std_count  = log(std_count);
run;

ods document name=varmeanplot(write);
*---part060b;
proc sgplot data=mean_count;
   title2 'Plot of log(std dev) vs log(mean)';
   scatter y=log_std_count x=log_mean_count;
   footnote 'No evidence of transformation needed';
run;
*---part060e;
ods document close;


/* preliminary plot of the trend over time */
/* Find the mean over the quadrats */
proc sort data=invert; by stream year period;
proc means data=invert noprint;
   by stream year period;
   var count;
   output out=mean_invert mean=mean_count;
run;
data invertb;
   merge invert mean_invert;
   by stream year period;
run;

ods document name=trendplot(write);
*---part070b;
proc sgplot data=invertb;
   title2 'Preliminary plot of mean of each stream over time';
   series  y=mean_count x=year / group=stream;
   scatter y=count x=year  / group=stream;
   refline 5.5 / axis=x;
   footnote 'Impact started in year 6';
run;
*---part070e;
ods document close;


/*************************** Analysis of stream 1 first *************************/
data stream1;
   set invert;
   if stream=1;
run;

/* preliminary plot */
/* Find the mean over the quadrats */
proc sort data=stream1; by year;
proc means data=stream1 noprint;
   by year period;
   var count;
   output out=mean_stream1 mean=mean_count;
run;
data stream1b;
   merge stream1 mean_stream1;
   by year;
run;

proc sgplot data=stream1b;
   title2 'Preliminary plot of mean of stream 1 data only over time';
   series  y=mean_count x=year;
   scatter y=count x=year;
   refline 5.5 / axis=x;
   footnote 'Impact started in year 6';
run;

/* analysis on the mean values */
/* This will only be approximate because the number of quadrats is not the same
   in all years */
ods document name=Stream1TtestAverages(write);
*---part100b;
ods graphics on;
proc ttest data=mean_stream1 plots=all;
   title2 'Analysis of stream 1 using the mean over the quadrats';
   var mean_count;
   class period;
   ods output ttests = TtestTest100;
   ods output ConfLimits=TtestCL100;
   ods output Statistics=TtestStat100;
   footnote 'Inference limited to that single stream and cannot be generalized to other streams';
run; 
ods graphics off;
*---part100e;
ods document close;

ods document name=Stream1MixedIndiv(write);
*---part200b; 
/* analysis of the individual quadrats for Stream 1*/
ods graphics on;
proc mixed data=stream1 plots=all;
   title2 'Analysis of stream 1 using the individual quadrats';
   class period year;
   model count = period / ddfm=kr;
   random year(period);
   lsmeans period / diff cl;
   footnote 'Inference limited to that single stream and cannot be generalized to other streams';
   ods output tests3   =Mixed200Test;  /* needed for the pdmix800 */
   ods output lsmeans  =Mixed200Lsmeans;
   ods output diffs    =Mixed200Diffs;
   ods output covparms =Mixed200CovParms;
run;
ods graphics off;
*---part200e;
ods document close;




/*************************** Analysis of ALL streams together *************************/
/* We assume that the same streams are measured in all years */

/* analysis on the mean values */
/* This will only be approximate because the number of quadrats is not the same
   in all years */
ods document name=MixedAvg(write);
*---part300b;
ods graphics on;
proc mixed data=mean_invert plots=all;
   title2 'Analysis of means of quadrat counts';
   class stream year period;
   model mean_count = period / ddfm=kr;
   random year(period) stream;
   lsmeans period / diff cl;
   footnote 'Inference is now for ALL streams in the project';
   ods output tests3   =Mixed300Test;  /* needed for the pdmix800 */
   ods output lsmeans  =Mixed300Lsmeans;
   ods output diffs    =Mixed300Diffs;
   ods output covparms =Mixed300CovParms;
run; 
ods graphics off;
*---part300e;
ods document close;
 
ods document name=MixedIndiv(write);
*---part400b;
/* analysis of the individual quadrats */
ods graphics on;
proc mixed data=invert plots=all nobound;
   title2 'Analysis of all streams using the individual quadrats';
   class period year stream;
   model count = period / ddfm=kr;
   random year(period) stream stream*year(period);
   lsmeans period / diff cl;
   footnote 'Inference is now for ALL streams in the project';
   ods output tests3   =Mixed400Test;  /* needed for the pdmix800 */
   ods output lsmeans  =Mixed400Lsmeans;
   ods output diffs    =Mixed400Diffs;
   ods output covparms =Mixed400CovParms;
run;
ods graphics off;
*---part400e;
ods document close;

 
/* analysis of the individual quadrats with bounded variance componentes */
proc mixed data=invert;
   title2 'Analysis of all streams using the individual quadrats - bounded variance components';
   class period year stream;
   model count = period / ddfm=kr;
   random year(period) stream stream*year(period);
   lsmeans period / diff cl;
   footnote 'Inference is now for ALL streams in the project';
run;

ods pdf close;



/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=Stream1TtestAverages;
   list /levels=all;
run;


ods tagsets.mycolorlatex file='invert-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=invert(obs=10);
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='invert-SAS-010.tex' (notop nobot);
proc document name=summarystat;
   obstitle \Tabulate#1\Report#1\Table#1; /* kill titles */
   obtitle  \Tabulate#1\Report#1\Table#1;
   obfootn  \Tabulate#1\Report#1\Table#1;
   replay   \Tabulate#1\Report#1\Table#1;
run;
ods tagsets.mycolorlatex close;


ods listing;
ods graphics on / imagefmt=png imagename='invert-SAS-060-varmeanplot' reset=index;
proc document name=varmeanplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods listing;
ods graphics on / imagefmt=png imagename='invert-SAS-070-trendplot' reset=index;
proc document name=trendplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;




/* Output from Proc Ttest on test on Stream 1 using Averags */

ods tagsets.mycolorlatex file='invert-SAS-100-teststat.tex' (notop nobot);
proc print data=TtestTest100 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='invert-SAS-100-estdiff.tex' (notop nobot);
proc print data=TtestCL100 noobs label split=" " ;
   where index(lowcase(class),'diff')>0;
   var variable class method variances mean lowerclmean upperclmean;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='invert-SAS-100-stats.tex' (notop nobot);
proc print data=TtestStat100 noobs label split=" " ;
   var variable class n mean stddev stderr lowerclmean upperclmean;
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='invert-SAS-100-ci' reset=index;
proc document name=Stream1TtestAverages;
   replay \Ttest#1\mean_count#1\Interval#1 / dest=listing;
run;
ods graphics off;
ods listing close;




/* Output from Proc Mixed from the analysis on the individual values for stream 1 */
ods tagsets.mycolorlatex file='invert-SAS-200-type3.tex' (notop nobot);
proc document name=Stream1MixedIndiv;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   replay   \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='invert-SAS-200-vc.tex' (notop nobot);
proc print data=Mixed200CovParms noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='invert-SAS-200-LSMperiod.tex' (notop nobot);
proc print data=Mixed200LSmeans noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='invert-SAS-200-LSMperioddiff.tex' (notop nobot);
proc print data=Mixed200Diffs noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='invert-SAS-200-diagnostic' reset=index;
proc document name=Stream1MixedIndiv;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;




/* Output from Proc Mixed from the analysis on the average values for all streams */
ods tagsets.mycolorlatex file='invert-SAS-300-type3.tex' (notop nobot);
proc document name=MixedAvg;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   replay   \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='invert-SAS-300-vc.tex' (notop nobot);
proc print data=Mixed300CovParms noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='invert-SAS-300-LSMperiod.tex' (notop nobot);
proc print data=Mixed300LSmeans noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='invert-SAS-300-LSMperioddiff.tex' (notop nobot);
proc print data=Mixed300Diffs noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='invert-SAS-300-diagnostic' reset=index;
proc document name=MixedAvg;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;





/* Output from Proc Mixed from the analysis on the individual values for all streams */
ods tagsets.mycolorlatex file='invert-SAS-400-type3.tex' (notop nobot);
proc document name=MixedIndiv;
   obstitle \Mixed#1\Tests3#1; /* kill titles */
   obtitle  \Mixed#1\Tests3#1;
   replay   \Mixed#1\Tests3#1;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='invert-SAS-400-vc.tex' (notop nobot);
proc print data=Mixed400CovParms noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='invert-SAS-400-LSMperiod.tex' (notop nobot);
proc print data=Mixed400LSmeans noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='invert-SAS-400-LSMperioddiff.tex' (notop nobot);
proc print data=Mixed400Diffs noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='invert-SAS-400-diagnostic' reset=index;
proc document name=MixedIndiv;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;









 

