/*  Searching for the time where treatment and control diverge
    Change point problem.

 This example is based on a project by Nate Derstine of 
 Biological Sciences at Simon Fraser University.
 The data are simulated, but illustrative of the process.

 Considered one of the worst invasive pest ants, the electric ant, 
 or little fire ant ({\it Wasmannia auropunctata} (Roger) (Hymenoptera:
 Formicidae)) has negatively impacted both biodiversity and agriculture. 
 Its distribution is nearlypantropical, and greenhouse infestations have 
 been reported as far north as Canada and the United
 Kingdom. Current {\it W. auropunctata} detection methods commonly employ a food item like peanut
 butter. An alternative detection method may be found in pheromone attractants. For W. auropunctata,
 a one-way trap containing an alarm pheromone has been
 successfully used to detect little fire ant populations in macadamia nut orchards. What is the
 longevity of this type of pheromone lure as used in a unique one-way ant
 trap.

 At the beginning of the experiment, 180 control traps and 180 treatment traps were prepared. On each day,
 three traps of each type were randomized to locations in the orchard where the ant species were known to be present.
 24 hours later, the traps were retrieved and the number of ants capture counted and the trap is discarded. */


/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run;

title 'Lifetime of pheromone attraction to ants';
options orientation=landscape;
ods pdf file='ant-SAS.pdf';
goptions device=pdf color=(black) rotate=landscape;

*---part010b;
proc import file='ant.csv' dbms=csv out=ant replace;
run;
*---part010e;

proc print data=ant(obs=8);
   title2 'part of the raw data';
run;


ods document name=plot20(write);
*---part020b;
/* plot of the raw data */
proc sgplot data=ant;
   title2 'plot of the raw counts';
   scatter x=day y=count / group=trt;
   xaxis label='Day';
   yaxis label='Count';
run;
*---part020e;
ods document close;

*---part023b;
/* Do a t-test for each day */
proc sort data=ant; by day; run;
ods select none;
proc ttest data=ant;
   by day;
   class trt;
   var count;
   ods output ttests=ttests;
run;
ods select all; run;
 
data ttests;  /* only select unequal var ttest */
  set ttests;
  if variances = 'Unequal';
run;
*---part023e;   

proc print data=ttests(obs=10);
   title2 'part of the output from the ttests';
run;

ods document name=plot23(write);
proc sgplot data=ttests;
   title2 'P-values of t-tests by day';
   scatter x=day y=probt;
   yaxis label='P-value from t-test' max=0 min=1;
   xaxis label='Day';
   refline .05 / axis=Y;
   format probt 7.2;
run;
ods document close;


*---part030b;
/* compute the mean for each day-trt combination */
proc sort data=ant; by day trt; run;
proc means data=ant noprint;
   by day trt;
   var count;
   output out=mean_count mean=mean_count;
run;
*---part030e;

ods document name=plot30(write);
/* plot the mean counts */
proc sgplot data=mean_count;
   title2 'Plot of the mean daily counts';
   scatter x=day y=mean_count / group=trt;
   xaxis label='Day';
   yaxis label='Mean Count';
run;
ods document close;

*---part032b;
/* Find the ratio of the means */
proc transpose data=mean_count out=trans_mean_count;
   by day;
   var mean_count;
   id trt;
run;
data trans_mean_count;
   set trans_mean_count;
   log_ratio = log(treatment/control);
   attrib log_ratio format=7.2 label='log(Treatment/Control)';
run;
*---part032e;

proc print data=trans_mean_count(obs=10);
   title2 'part of log(ratio) dataset';
run;

ods document name=plot32(write);
proc sgplot data=trans_mean_count;
   title2 'Plot of log(Treatment/Control) means';
   scatter x=day y=log_ratio;
   refline 0 / axis=Y;
   xaxis label='Day';
   yaxis label='log(Treatment Mean/Control Mean)';
run;
ods document close;


ods document name=nlin40(write);
*---part040b;
/* Fit a changepoint model */
proc nlin data=trans_mean_count;
   title2 'Change point model';
   parms CP=45 beta0=1 beta1=.1 beta2=.1;
   if (day < CP) then 
        mean = beta0 + beta1*day;
   else mean = beta0 + beta1*day +beta2*(day-CP);
   model log_ratio = mean;
   output out=model_fit predicted=pred;
   ods output ParameterEstimates=Nlin_est; 
run;
*---part040e;
ods document close;

ods document name=plot40(write);
proc sgplot data=model_fit;
   title2 'Fitted model';
   scatter x=day y=log_ratio;
   series  x=day y=pred;
run;
ods document close;


ods document name=nlin50(write);
*---part050b;
/* Fit a change point model where the slope is forced to be zero after the change*/
proc nlin data=trans_mean_count;
   title2 'Change point model - slope = 0 after change point';
   parms CP=45 beta0=1 beta1=.1 1;
   mean = beta0 + beta1*min(cp,day);
   model log_ratio = mean;
   output out=model_fit0 predicted=pred; 
   ods output ParameterEstimates=Nlin_est0;
run;
*---part050e;
ods document close;

ods document name=plot50(write);
proc sgplot data=model_fit0;
   title2 'Fitted model - slope = 0 after change point';
   scatter x=day y=log_ratio;
   series  x=day y=pred;
run;
ods document close;

ods pdf close;


/* create the files to be included in the LaTex document */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=nlin40;
   list /levels=all;
run;


ods tagsets.mycolorlatex file='ant-SAS-010.tex' (notop nobot);
proc print data=ant(obs=10) label split=" ";
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='ant-SAS-020' reset=index;
proc document name=plot20;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='ant-SAS-023' reset=index;
proc document name=plot23;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='ant-SAS-030' reset=index;
proc document name=plot30;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='ant-SAS-032' reset=index;
proc document name=plot32;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='ant-SAS-040.tex' (notop nobot);
proc print data=Nlin_est label split=" ";
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='ant-SAS-040' reset=index;
proc document name=plot40;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='ant-SAS-050.tex' (notop nobot);
proc print data=Nlin_est0 label split=" ";
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='ant-SAS-050' reset=index;
proc document name=plot50;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;



