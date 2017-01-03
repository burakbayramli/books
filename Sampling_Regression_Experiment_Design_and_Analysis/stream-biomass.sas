/* Regression with pseudo-replication over time. Process and Sampling Error. Stream-biomass example. */
  
/* This example presents fictitious (but realistic) data for measurements
   of biomass at a stream downstream from a large industrial project. 
   In each year, three grab samples were taken of the stream
   bottom, and the biomass (mg) of invertebrates was measured. */
 
/* 2014-08-10 CJS First Edition */
dm 'log'    clear;
dm 'output' clear; 
proc datasets kill; run;
footnote ' ';


title 'Stream Biomass - Regression with pseudo-replication; Process and Sampling Error';
options  noovp orientation=landscape;
ods pdf file='stream-biomass-SAS.pdf' style=styles.printer; 

*---part010b;
data biomass;
   infile 'stream-biomass.csv' dlm=',' dsd missover firstobs=2;
   input year sample biomass;
run;
*---part010e;

proc print data=biomass;
   title2 'raw data';
run;
 
/* plot of the raw data */
/* plot the data to show the replicated points */
ods document name=prelimplot(write);
*---partprelimplotb;
proc sgplot data=biomass;
   title2 'Preliminary data plot';
   scatter  y=biomass x=year / jitter;
   yaxis label='Stream Biomass (g)';
   xaxis label='Year' integer offsetmax=0.05 offsetmin=0.05;
run;
*---partprelimplote;
ods document close;
 
/* Fit the (incorrect) regression line. Proc Reg does NOT allow for process and sampling error */
ods document name=regoutput(write);
*---partregfitb;
ods graphics on;
proc reg data=biomass plot=all;
   title2 'Incorrect regression analysis';
   model biomass = year ; 
   ods output ParameterEstimates=coef;
run;
ods graphics off;
*---partregfite;
ods document close;



/* Dealing with the pseudo-replication. */
/*************************************************************************/
/* Method 1. Do the analysis on the averages.
   This will be exactly correct if the design is balanced, and approximate
   if the number of points at each time point are unequal */
*---partavgpointsb;
proc sort data=biomass; by year; run;
proc means data=biomass noprint;
   by year;
   var biomass;
   output out=meanbiomass mean=meanbiomass;
run;
proc print data=meanbiomass(drop=_type_ _freq_;
   title2 'Average biomass each year';
run;
*---partavgpointse;

*---partregfitavgb;
ods graphics on;
proc reg data=meanbiomass plot=all;
   title2 'Regression analysis on the yearly means';
   model meanbiomass = year / dw dwprob; 
   ods output ParameterEstimates=coefavg;
   ods output dwstatistic=dwstatisticavg;
run;
ods graphics off;
*---partregfitavge;

/*************************************************************************/
/* Method 2. Use a mixed model
   This works regardless if the design is balanced or not. 
   We first need to create copy of the year variable. */

*---partmixedfitb;
data biomass;
   set biomass;
   yearc = year; /* copy of year variable */
run;
ods graphics on;
proc mixed data=biomass plot=all;
   title2 'Regression analysis using Proc Mixed to deal with the pseudo-replication';
   class yearc;
   model biomass = year / s ddfm=kr ;
   random yearc; 
   ods output SolutionF=coefmixed;
   ods output CovParms =covparmsmixed;
run;
ods graphics off;
*---partmixedfite;

/* You can also fit an AR(1) model to the yearly effects */
*---partmixedfitar1b;
ods graphics on;
proc mixed data=biomass plot=all;
   title2 'Regression analysis using Proc Mixed to deal with the pseudo-replication and with AR(1)';
   class yearc;
   model biomass = year / s ddfm=kr;
   random yearc / type=ar(1); 
   ods output SolutionF=coefmixedar1;
   ods output CovParms =covparmsmixedar1;
run;
ods graphics off;
*---partmixedfitar1e;


ods pdf close;



/* create the files to be included in the LaTex document */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=regoutput;
   list /levels=all;
run;


ods listing;
ods graphics on / imagefmt=png imagename='stream-biomass-SAS-prelimplot' reset=index;
proc document name=prelimplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mylatex file='stream-biomass-SAS-010.tex' (notop nobot);
proc print data=biomass(obs=10 drop=yearc) label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='stream-biomass-SAS-regfit.tex' (notop nobot);
proc print data=coef label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='stream-biomass-SAS-avgpoints.tex' (notop nobot);
proc print data=meanbiomass(obs=10 drop=_type_ _freq_) label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='stream-biomass-SAS-regfitavg.tex' (notop nobot);
proc print data=coefavg label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='stream-biomass-SAS-mixedfit.tex' (notop nobot);
proc print data=coefmixed label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='stream-biomass-SAS-mixedfitcovparms.tex' (notop nobot);
proc print data=covparmsmixed label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='stream-biomass-SAS-dwtestavg.tex' (notop nobot);
proc print data=dwstatisticavg label split=" ";
   var label1 nvalue1;
   label label1='Statistic';
   label nvalue1='Value';
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file='stream-biomass-SAS-mixedfitcoefar1.tex' (notop nobot);
proc print data=coefmixedar1 label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='stream-biomass-SAS-mixedfitcovparmsar1.tex' (notop nobot);
proc print data=covparmsmixedar1 label split=" " noobs;
run;
ods tagsets.mylatex close;


