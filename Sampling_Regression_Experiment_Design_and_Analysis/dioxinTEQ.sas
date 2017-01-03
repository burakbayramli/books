/* Monitoring Dioxin levels over time */

/* Degradation of dioxin in crabs.

  How fast does dioxin degrade over time? In each year, samples of crabs
  were captured at a site. The crab livers were excised, composited 
  together, and various species of dioxins were measured. These were 
  translated into the World Health Organization (WHO) standardized total #equivalent dose.
*/


/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear
proc datasets kill;


title 'Monitoring Dioxin Levels over Time';
options nodate noovp orientation=landscape;
ods pdf file='dioxinTEQ-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

*---part010b;
data teq;
   infile "dioxinTEQ.csv" dlm=',' dsd missover firstobs=2;
   input site $ year TEQ;
   logTEQ = log(TEQ);   /* compute the log TEQ values */
   attrib logTEQ label='log(TEQ)' format=7.2;
run;
*---part010e;

proc print data=teq;
   title2 'raw data';
run;


/* Create the preliminary plot of the data */

ods document name=initplot(write);
*---part020b;
proc sgplot data=teq;
   title2 'Preliminary data plot';
   scatter y=TEQ x=year /  markerattrs=(symbol=circlefilled);
   yaxis label='TEQ'   offsetmin=.05 offsetmax=.05;
   xaxis label='Year'  offsetmin=.05 offsetmax=.05;
run;
*---part020e;
ods document close;

ods document name=initplot2(write);
*---part021b;
proc sgplot data=teq;
   title2 'Preliminary data plot - after log transform';
   scatter y=logTEQ x=year /  markerattrs=(symbol=circlefilled);
   yaxis label='llog(TEQ)'   offsetmin=.05 offsetmax=.05;
   xaxis label='Year'        offsetmin=.05 offsetmax=.05;
run;
*---part021e;
ods document close;



/* Fit the line to the data, and extract the statistics needed for plotting */
/* NOTE: there is no easy way to get inverse predictions in SAS */

ods document name=regfit(write);
*---part030b;
ods graphics on;
proc reg data=teq plots=all;
   title2 'fit the model';
   model logTEQ = year / all;
   ods output OutputStatistics  =Predictions;
   ods output ParameterEstimates=Estimates;
run;
ods graphics off;
*---part030e;
ods document close;
 
*---part040b;
data predictions;  /* merge the original data set with the predictions */
   merge teq predictions; 
run;
*---part040e;

proc print data=predictions;
   title2 'Predicted values and confidence intervals';
run;

ods document name=ciplot(write);
*---part050b;
proc sgplot data=Predictions;
   title2 'Fitted line and confidence curves for mean and individual values';
   band    x=Year lower=lowerCL     upper=upperCL;
   band    x=Year lower=lowerCLmean upper=upperCLmean / fillattrs=(color=red);
   series  y=PredictedValue X=Year;
   scatter y=logTEQ x=Year /  markerattrs=(symbol=circlefilled);
   yaxis label='logTEQ' offsetmin=.05 offsetmax=.05;
   xaxis label='Year'   offsetmin=.05 offsetmax=.05;
run;
*---part050e;
ods document close;

ods document name=invpredplot(write);
*---part070b;
proc sgplot data=Predictions;
   title2 'Demonstrating how to do inverse predictions at logTEQ=2.302';
   band    x=year lower=lowerCL     upper=upperCL;
   band    x=year lower=lowerCLmean upper=upperCLmean / fillattrs=(color=red);
   series  y=PredictedValue X=Year;
   scatter y=logTEQ x=Year /  markerattrs=(symbol=circlefilled);
   refline 2.302 / axis=y ;
   yaxis label='logTEQ'       offsetmin=.05 offsetmax=.05;
   xaxis label='Year'         offsetmin=.05 offsetmax=.05;
run;
*---part070e;
ods document close;




ods pdf close;


/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=regfit;
   list /levels=all;
run;


ods tagsets.mycolorlatex file='dioxinTEQ-SAS-010.tex' (notop nobot);
proc print data=teq label split=" ";
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='dioxinTEQ-SAS-020' reset=index;
proc document name=initplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='dioxinTEQ-SAS-021' reset=index;
proc document name=initplot2;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mycolorlatex file='dioxinTEQ-SAS-030.tex' (notop nobot);
proc print data=estimates noobs label split=" ";
   var Variable DF Estimate StdErr tValue Probt LowerCl UpperCL; 
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='dioxinTEQ-SAS-040.tex' (notop nobot);
proc print data=Predictions label split=" ";
   var year TEQ logTEQ PredictedValue StdErrMeanPredict LowerCLMean UpperCLMean LowerCL UpperCL;
run;
ods tagsets.mycolorlatex close;


ods listing;
ods graphics on / imagefmt=png imagename='dioxinTEQ-SAS-050' reset=index;
proc document name=ciplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='dioxinTEQ-SAS-050b' reset=index;
proc document name=regfit;
   replay \Reg#1\MODEL1#1\ObswiseStats#1\logTEQ#1\FitPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* Inverse prediction plot */
ods listing;
ods graphics on / imagefmt=png imagename='dioxinTEQ-SAS-070' reset=index;
proc document name=invpredplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* diagnostic plots */

ods listing;
ods graphics on / imagefmt=png imagename='dioxinTEQ-SAS-060' reset=index;
proc document name=regfit;
   replay \Reg#1\MODEL1#1\ObswiseStats#1\logTEQ#1\DiagnosticPlots#1\DiagnosticsPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;;




