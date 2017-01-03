/* Comparing regressions line with pseudo-replication */

/* 2014-08-14 CJS First edition */

/* Weins and Parker (1985) discuss a variety of design suitable for monitoring environmental 
   impact when little (or no) pre-project data are available. 
   Their ``impact-level-by-time interaction'' design is very similar to the proposal --
   monitor the project and control site to see if the response over time follows a parallel trajectory. 
 
   This example is  (loosely) patterned on monitoring the density of aquatic invertebrates over time
   in the absence of pre-impact data. Two streams were monitored running through a recent clearcut.
   As the clearcut gradually revegetates, the community ecology of the stream changes and so a decline
   in a particular invertebrate over time is envisioned. Around one of the streams, several enhancements
   to speed the regeneration of the forest are tried. Is there evidence that these enhancements change
   the rate at which the invertebrate population changes?
 
   In each of seven years of monitoring, five samples of invertebrates were sampled at 
   each stream. */
dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;
footnote ' ';

options orientation=landscape;
ods pdf file='invertebrates-SAS.pdf' style=styles.printer;
 
title 'Stream invertebrates - pseudo-replication in regression after environmental impact';

*---part-readdatab; 
data inverts;
   infile 'invertebrates.csv' dlm=',' dsd missover firstobs=2;
   length Site $20;
   input year site density sample;
   yearc = year;  /* needed for mixed analysis */ 
run;
*---part-readdatae;

proc print data=inverts(obs=20);
   title2 'part of the raw data';
run;


/* naive analysis ignoring pseudo-replication */
ods document name=prelimplot(write);
*---part-prelimplotb;
ods graphics / reset attrpriority=none;
proc sgplot data=inverts;
   title2 'preliminary plot';
   scatter x=Year y=Density / jitter group=Site ;
   reg     x=Year y=Density / group=site;
   xaxis label='Year' offsetmin=0.05 offsetmax=0.05;
   yaxis label='Density';
   footnote 'Points jittered';
run;
footnote ' ';
*---part-prelimplote;
ods document close;

*---part-regfit-naiveb;
ods graphics on;
proc glm data=inverts plots=all;
   title2 'naive regression analysis';
   class site;
   model Density = Year site year*site/ solution clparm;
   ods output parameterestimates = regnaive_parameterestimates;
   ods output modelanova         = regnaive_modelanova;
run;
*---part-regfit-naivee;



/* analysis on the averages */
*---part-findavgb;
proc sort data=inverts; by year site; run;
proc means data=inverts noprint;
   by year site;
   var Density;
   output out=invertsavg mean=mean_Density;
run;
*---part-findavge;

proc print data=invertsavg;
   title2 'Site-Year averages';
run;


*---part-getdiffsb;
proc transpose data=invertsavg out=invertsavg_trans;
   by year;
   var mean_density;
   id site;
run;

data invertsavg_trans;
   set invertsavg_trans;
   diff = control_site - project_site;
run;
*---part-getdiffse;

proc print data=invertsavg_trans;
   title2 'estimated differences in averages';
run;




/* regression on the differences */
ods graphics on;
proc reg data=invertsavg_trans plots=all;
   title2 'regression of differences of averages';
   model diff = year / clb;
   ods output parameterestimates=regavgdiff_parameterestimates;
run;

ods document name=invertavgdiff_plot(write);
proc sgplot data=invertsavg_trans;
   title2 'Regression of differences of averages';
   scatter x=year y=diff;
   reg     x=year y=diff;
   xaxis label='Year' offsetmin=0.05 offsetmax=0.05;
   yaxis label="Differences of averages within years";
run;
ods document close;


/* Rank correlation of differences with year */
*---part-kendallb;
proc corr data=invertsavg_trans kendall;
   var year diff;
   ods output KendallCorr=kendallcorr;
run;
*---part-kendalle;




 

/* Ancova on average values */
  
proc print data=invertsavg(drop=_type_ _freq_);
   title2 'averaged values';
run;

*---part-ancovaavg-yearcopyb;
data invertsavg;
   set invertsavg;
   yearc = year;
run;
*---part-ancovaavg-yearcopye;



*---part-ancovaavgb;
ods graphics on;
proc mixed data=invertsavg plots=all;
   title2 'ANCOVA using averages';
   class site yearc;;
   model mean_Density = Year Site Year*Site / solution;
   random yearc;
   estimate 'slope 1' year 1 year*site 1 0;
   estimate 'slope 2' year 1 year*site 0 1;
   estimate 'diff slopes'    year*site 1 -1;
   ods output solutionF   = regavg_parameterestimates;
   ods output tests3      = regavg_modelanova;
   ods output covparms    = regavg_varcomp;
   ods output estimates   = regavg_slopes;
run;
*---part-ancovaavge;




*---part-regfit-mixedb;
/* analysis on individual values using MIXED */
/* add a copy of the year variable */
*---part-yearcopy2b;
data inverts;
   set inverts;
   yearc = year;
run;
*---part-yearcopy2e;


*---part-fitmixed-indivb;
ods graphics on;
proc mixed data=inverts plots=all;
   title2 'Fegression using random effects';
   class Site yearc;
   model Density = Year Site Year*Site /ddfm=kr;
   random Yearc Site*Yearc;
   estimate 'slope 1' year 1 year*site 1 0;
   estimate 'slope 2' year 1 year*site 0 1;
   estimate 'diff slopes'    year*site 1 -1;
   ods output solutionf = regmixed_parameterestimates;
   ods output covparms  = regmixed_covparms;
   ods output estimates = regmixed_estimates;
   ods output tests3    = regmixed_anova;
run;
*---part-fitmixed-indive;


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


ods tagsets.mylatex file='invertebrates-SAS-readdata.tex' (notop nobot);
proc print data=inverts(obs=10) label split=" ";
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='invertebrates-SAS-prelimplot' reset=index;
proc document name=prelimplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mylatex file='invertebrates-SAS-regfit-naive.tex' (notop nobot);
proc print data=regnaive_parameterestimates(drop=model dependent) label split=" " noobs;
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file='invertebrates-SAS-avg.tex' (notop nobot);
proc print data=invertsavg(drop=_type_ _freq_) label split=" " noobs;
run;
ods tagsets.mylatex close;



ods listing;
ods graphics on / imagefmt=png imagename='invertebrates-SAS-diffplot' reset=index;
proc document name=invertavgdiff_plot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mylatex file='invertebrates-SAS-diffests.tex' (notop nobot);
proc print data=regavgdiff_parameterestimates(drop=model dependent) label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='invertebrates-SAS-kendall.tex' (notop nobot);
proc print data=kendallcorr label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='invertebrates-SAS-regfit-avg-estimates.tex' (notop nobot);
proc print data=regavg_parameterestimates(drop=model dependent) label split=" " noobs;
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file='invertebrates-SAS-regavg_modelanova.tex' (notop nobot);
proc print data=regavg_modelanova label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='invertebrates-SAS-regavg_varcomp.tex' (notop nobot);
proc print data=regavg_varcomp label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='invertebrates-SAS-regavg_slopes.tex' (notop nobot);
proc print data=regavg_slopes label split=" " noobs;
run;
ods tagsets.mylatex close;





ods tagsets.mylatex file='invertebrates-SAS-regfit-mixed-anova.tex' (notop nobot);
proc print data=regmixed_anova label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='invertebrates-SAS-regmixed_varcomp.tex' (notop nobot);
proc print data=regmixed_covparms label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='invertebrates-SAS-regmixed_slopes.tex' (notop nobot);
proc print data=regmixed_estimates label split=" " noobs;
run;
ods tagsets.mylatex close;


