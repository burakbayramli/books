/* Selenium concentration in fish tissues */

/* 2014-08-12 CJS First edition */

/* This example uses (fictitious) but realistic data based on an environmental review
   of a coal mining project. Coal mining often releases Se into the environment
   and this can accumulate in lakes. A set of 9 lakes (labeled $a$ through $i$) was
   selected in the watershed, and in each lake, a sample of fish (ranging from 1 to 34 fish per lake)
   was sampled, and the concentration of Se in the muscle tissue was measured. */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;
footnote ' ';

options orientation=landscape;
ods pdf file='Se-lake-SAS.pdf' style=styles.printer;
 
title 'Se concentation in Fish in lakes - pseudo-replication in regression';

*---part-readdatab; 
data fish;
   infile 'Se-lake.csv' dlm=',' dsd missover firstobs=2;
   length lake $4;
   input lake log_water_Se log_fish_Se;
run;
*---part-readdatae;

proc print data=fish(obs=20);
   title2 'part of the raw data';
run;


/* naive analysis ignoring pseudo-replication */
ods document name=prelimplot(write);
*---part-prelimplotb;
ods graphics / reset attrpriority=none;
proc sgplot data=fish;
   title2 'preliminary plot';
   scatter x=log_water_Se y=log_fish_se / jitter group=lake ;
   reg     x=log_water_Se y=log_fish_se;
   xaxis label='log(Se concentration in water)' offsetmin=0.05 offsetmax=0.05;
   yaxis label='log(Se concentration in fish)';
   footnote 'Points jittered';
run;
footnote ' ';
*---part-prelimplote;
ods document close;

*---part-regfit-naiveb;
ods graphics on;
proc reg data=fish plots=all;
   title2 'naive regression analysis';
   model log_fish_se = log_water_se / clb;
   ods output parameterestimates = regnaive_parameterestimates;
run;
*---part-regfit-naivee;



/* analysis on the averages */
*---part-findavgb;
proc sort data=fish; by lake; run;
proc means data=fish noprint;
   by lake;
   var log_water_se log_fish_se;
   output out=fishavg mean=mean_log_water_se mean_log_fish_se;
run;
*---part-findavge;
  
proc print data=fishavg(drop=_type_ _freq_);
   title2 'averaged values';
run;

*---part-regfit-avgb;
proc reg data=fishavg;
   title2 'regression using averages';
   model mean_log_fish_se = mean_log_water_se / clb;
   ods output parameterestimates = regavg_parameterestimates;
run;
*---part-regfit-avge;

*---part-regfit-mixedb;
/* analysis on individual values using MIXED */
ods graphics on;
proc mixed data=fish plots=all;
   title2 'regression using random effects';
   class lake;
   model log_fish_se = log_water_se /ddfm=kr solution outp=pred_cond outpm=pred_marg;
   random lake;
   ods output solutionf = regmixed_parameterestimates;
   ods output covparms  = regmixed_covparms;
run;
*---part-regfit-mixede;

proc print data=pred_cond;
  title2 'conditional predictions';
  where lake in ('c','d');
run;

proc print data=pred_marg;
   title2 'marginal predictions';
   where lake in ('c','d');
run;

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


ods tagsets.mylatex file='Se-lake-SAS-readdata.tex' (notop nobot);
proc print data=fish(obs=10) label split=" ";
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='Se-lake-SAS-prelimplot' reset=index;
proc document name=prelimplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mylatex file='Se-lake-SAS-regfit-naive.tex' (notop nobot);
proc print data=regnaive_parameterestimates(drop=model dependent) label split=" " noobs;
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file='Se-lake-SAS-avg.tex' (notop nobot);
proc print data=fishavg(drop=_type_ _freq_) label split=" " noobs;
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file='Se-lake-SAS-regfit-avg-estimates.tex' (notop nobot);
proc print data=regavg_parameterestimates(drop=model dependent) label split=" " noobs;
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file='Se-lake-SAS-regfit-mixed-estimates.tex' (notop nobot);
proc print data=regmixed_parameterestimates label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='Se-lake-SAS-regfit-mixed-covparms.tex' (notop nobot);
proc print data=regmixed_covparms label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='Se-lake-SAS-regfit-mixed-predict-cond.tex' (notop nobot);
proc print data=pred_cond nobs label split=' ';
  title2 'conditional predictions';
  where lake in ('c','d');
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='Se-lake-SAS-regfit-mixed-predict-marg.tex' (notop nobot);
proc print data=pred_marg noobs label split=' ';
   title2 'marginal predictions';
   where lake in ('c','d');
run;
ods tagsets.mylatex close;



