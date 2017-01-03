/* Investigate the relationship between yield and the amount of fertilizer */
/* Example of simple linear regression */

/* When I was living in Manitoba, I used to grow 
   tomato plants. One day my daughter saw me planting and asked 
   why I was putting fertilizer with the plants.
   When I responded that it helped the plants grow better, 
  she asked #me if adding more fertilizer made the plants grow better.

  A learning opportunity presented itself.

  We randomly applied different amounts of fertilizer to different plots.
  Over the summer, we carefully watered all plots equally and weighed
  the amount of tomatoes produced.
*/

/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear
proc datasets kill;

title 'Relationship between yield of tomatoes and amount of fertilizer - simple linear regression';

options nodate noovp orientation=landscape;
ods pdf file='fertilizer.pdf';
goptions device=pdf colors=(black) rotate=landscape;

*---part010b;
data tomato;
   infile 'fertilizer.csv' dlm=',' dsd missover firstobs=2;
   input fertilizer yield;
run;;
*---part010e;

proc print data=tomato;
   title2 'raw data';
run;


ods document name=initplot(write);
*---part020b;
proc sgplot data=tomato;
   title2 'Preliminary data plot';
   scatter y=yield x=fertilizer /  markerattrs=(symbol=circlefilled);
   yaxis label='Yield'       offsetmin=.05 offsetmax=.05;
   xaxis label='Fertilizer'  offsetmin=.05 offsetmax=.05;
run;
*---part020e;
ods document close;


/* fit the linear model */
ods document name=regfit(write);
*---part030b;
ods graphics on;
proc reg data=tomato plots=all;
   title2 'fit the model';
   model yield = fertilizer / all;
   ods output OutputStatistics  =Predictions;
   ods output ParameterEstimates=Estimates;
run;
ods graphics off;
*---part030e;
ods document close;
 
*---part040b;
data predictions;  /* merge the original data set with the predictions */
   merge tomato predictions; 
run;
*---part040e;

proc print data=predictions;
   title2 'Predicted values and confidence intervals';
run;


ods document name=ciplot(write);
*---part050b;
proc sgplot data=Predictions;
   title2 'Fitted line and confidence curves for mean and individual values';
   band    x=fertilizer lower=lowerCL     upper=upperCL;
   band    x=fertilizer lower=lowerCLmean upper=upperCLmean / fillattrs=(color=red);
   series  y=PredictedValue X=Fertilizer;
   scatter y=yield x=fertilizer /  markerattrs=(symbol=circlefilled);
   yaxis label='Yield'       offsetmin=.05 offsetmax=.05;
   xaxis label='Fertilizer'  offsetmin=.05 offsetmax=.05;
run;
*---part050e;
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


ods tagsets.mycolorlatex file='fertilizer-SAS-010.tex' (notop nobot);
proc print data=tomato label split=" ";
run;
ods tagsets.mycolorlatex close;

ods listing;
ods graphics on / imagefmt=png imagename='fertilizer-SAS-020' reset=index;
proc document name=initplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='fertilizer-SAS-030.tex' (notop nobot);
proc print data=estimates noobs label split=" ";
   var Variable DF Estimate StdErr tValue Probt LowerCl UpperCL; 
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='fertilizer-SAS-040.tex' (notop nobot);
proc print data=Predictions label split=" ";
   var fertilizer yield PredictedValue StdErrMeanPredict LowerCLMean UpperCLMean LowerCL UpperCL;
run;
ods tagsets.mycolorlatex close;


ods listing;
ods graphics on / imagefmt=png imagename='fertilizer-SAS-050' reset=index;
proc document name=ciplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='fertilizer-SAS-050b' reset=index;
proc document name=regfit;
   replay \Reg#1\MODEL1#1\ObswiseStats#1\yield#1\FitPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* diagnostic plots */

ods listing;
ods graphics on / imagefmt=png imagename='fertilizer-SAS-060' reset=index;
proc document name=regfit;
   replay \Reg#1\MODEL1#1\ObswiseStats#1\yield#1\DiagnosticPlots#1\DiagnosticsPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;;


