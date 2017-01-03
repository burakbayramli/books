n/* Weight length relationship for fish.
   Log-log regression and non-linear least squares .

   A common technique in fisheries management is to investigate the relationship
   between weight and lengths of fish.

   This is expected to a non-linear relationship because as fish get longer, they
   also get wider and thicker. If a fish grew equally in all directions, then the
   weight of a fish should be proportional to the length**3 (why?). However, fish
   do not grow equally in all directions, i.e. a doubling of length is not necessarily
   associated with a doubling of width or thickness. The pattern of association of
   weight with length may reveal information on how fish grow.

   The following example was provided by Randy Zemlak of the British Columbia
   Ministry of Water, Land, and Air Protection.
  A sample of fish was measured at a lake in British Columbia. */



/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear
proc datasets kill;


title 'Weight-length relationship of fish';
options nodate noovp orientation=landscape;
ods pdf file='wtlen-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

*---part010b;
data wtlen;
   infile 'wtlen.csv' dlm=',' dsd missover firstobs=2 ;
   input length weight;
run;
*---part010e;

proc print data=wtlen ;
   title2 'raw data';
run;


*---part011b;
data wtlen2; /* create some plotting points */
   do length = 25 to 50 by 1;
      weight = .;
      output;
   end;
 run;


data wtlen; /* append the plotting points and compute log() transformation */
   set wtlen wtlen2;
   log_length = log(length);
   log_weight = log(weight);
   format log_length log_weight 7.1;
run;
*---part011e;
 
proc sort data=wtlen; by length;
 

ods document name=initplot(write);
*---part020b;
proc sgplot data=wtlen;
   title2 'Preliminary data plot';
   scatter y=weight x=length /  markerattrs=(symbol=circlefilled);
   loess   y=weight x=length;
   yaxis label='Weight'       offsetmin=.05 offsetmax=.05;
   xaxis label='Length'  offsetmin=.05 offsetmax=.05;
run;
*---part020e;
ods document close;

/* fit the linear model */
ods document name=regfit(write);
*---part030b;
ods graphics on;
proc reg data=wtlen plots=all;
   title2 'fit the model using all of the data';
   model log_weight = log_length / all;
   ods output OutputStatistics  =Predictions;
   ods output ParameterEstimates=Estimates;
run;
ods graphics off;
*---part030e;
ods document close;

 
*---part040b;
data predictions;  /* merge the original data set with the predictions */
   merge wtlen predictions; 
run;
*---part040e;

proc print data=predictions;
   title2 'Predicted values and confidence intervals';
run;

ods document name=ciplot(write);
*---part050b;
proc sgplot data=Predictions;
   title2 'Fitted line and confidence curves for mean and individual values';
   band    x=log_length lower=lowerCL     upper=upperCL;
   band    x=log_length lower=lowerCLmean upper=upperCLmean / fillattrs=(color=red);
   series  y=PredictedValue X=log_length;
   scatter y=log_weight x=log_length /  markerattrs=(symbol=circlefilled);
   yaxis label='log_weight'  offsetmin=.05 offsetmax=.05;
   xaxis label='log_length'  offsetmin=.05 offsetmax=.05;
run;
*---part050e;
ods document close;


/* Refit the model after dropping the two largest fish */

*---part110b;
data wtlen3; /* append the plotting points and compute log() transformation */
   set wtlen;
   if length > 40 then delete;
run;
*---part110e;
 
proc sort data=wtlen; by length;
 

ods document name=initplot2(write);
*---part120b;
proc sgplot data=wtlen3;
   title2 'Preliminary data plot - after removing two largest fish';
   scatter y=weight x=length /  markerattrs=(symbol=circlefilled);
   loess   y=weight x=length;
   yaxis label='Weight'       offsetmin=.05 offsetmax=.05;
   xaxis label='Length'  offsetmin=.05 offsetmax=.05;
run;
*---part120e;
ods document close;

/* fit the linear model */
ods document name=regfit2(write);
*---part130b;
ods graphics on;
proc reg data=wtlen3 plots=all;
   title2 'fit the model after two largest fish removed';
   model log_weight = log_length / all;
   ods output OutputStatistics  =Predictions2;
   ods output ParameterEstimates=Estimates2;
run;
ods graphics off;
*---part0130e;
ods document close;

 
*---part140b;
data predictions2;  /* merge the original data set with the predictions */
   merge wtlen3 predictions2; 
run;
*---part140e;

proc print data=predictions2;
   title2 'Predicted values and confidence intervals';
run;

ods document name=ciplot2(write);
*---part150b;
proc sgplot data=Predictions2;
   title2 'Fitted line and confidence curves for mean and individual values - two largest fish removed';
   band    x=log_length lower=lowerCL     upper=upperCL;
   band    x=log_length lower=lowerCLmean upper=upperCLmean / fillattrs=(color=red);
   series  y=PredictedValue X=log_length;
   scatter y=log_weight x=log_length /  markerattrs=(symbol=circlefilled);
   yaxis label='log_weight'  offsetmin=.05 offsetmax=.05;
   xaxis label='log_length'  offsetmin=.05 offsetmax=.05;
run;
*---part150e;
ods document close;




goptions gsfmode=append; 
proc gplot data=wtlen;
   title2 'fitted line on log-scale';
   axis3 label=(a=90 r=0    'log(weight)');
   axis4 label=(            'log(length)');
   plot log_weight * log_length=1 predicted*log_length=2 / vaxis=axis3 haxis=axis4 overlay;
   symbol1 v=plos i=none;
   symbol2 v=none i=rl;
run;
 
proc gplot data=wtlen;
   title2 'fitted line on ordinary scale';
   axis5 label=(a=90 r=0  'Weight');
   axis6 label=(          'Length');
   plot weight*length pred_weight*length=2 / vaxis=axis5 haxis=axis6 overlay;
   symbol1 v=plus i=none;
   symbol2 v=none i=join;
run;

/*---------------- Now for a non-linear least squares fit *****/

ods document name=nlin(write);
*---part300b;
ods graphics on;
proc nlin data=wtlen3 plots=all;
   title2 'non-linear least squares';
   parameters b0=.03  b1=3;
   bounds b0 > 0;
   model weight = b0 * length**b1;
   ods output ParameterEstimates=NlinEstimates;
run;
ods graphics off;
*---part300e;
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


ods tagsets.mycolorlatex file='wtlen-SAS-010.tex' (notop nobot);
proc print data=wtlen(obs=10) label split=" ";
   where weight ^= .;
run;
ods tagsets.mycolorlatex close;


ods listing;
ods graphics on / imagefmt=png imagename='wtlen-SAS-020' reset=index;
proc document name=initplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='wtlen-SAS-030.tex' (notop nobot);
proc print data=estimates noobs label split=" ";
   var Variable DF Estimate StdErr tValue Probt LowerCl UpperCL; 
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='wtlen-SAS-040.tex' (notop nobot);
proc print data=Predictions label split=" ";
   var length log_length weight log_weight PredictedValue StdErrMeanPredict LowerCLMean UpperCLMean LowerCL UpperCL;
run;
ods tagsets.mycolorlatex close;


ods listing;
ods graphics on / imagefmt=png imagename='wtlen-SAS-050' reset=index;
proc document name=ciplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='wtlen-SAS-050b' reset=index;
proc document name=regfit;
   replay \Reg#1\MODEL1#1\ObswiseStats#1\log_weight#1\FitPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* diagnostic plots */

ods listing;
ods graphics on / imagefmt=png imagename='wtlen-SAS-060' reset=index;
proc document name=regfit;
   replay \Reg#1\MODEL1#1\ObswiseStats#1\log_weight#1\DiagnosticPlots#1\DiagnosticsPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;;


 


ods tagsets.mycolorlatex file='wtlen-SAS-110.tex' (notop nobot);
proc print data=wtlen2(obs=10) label split=" ";
   where weight ^= .;
run;
ods tagsets.mycolorlatex close;


ods listing;
ods graphics on / imagefmt=png imagename='wtlen-SAS-120' reset=index;
proc document name=initplot2;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='wtlen-SAS-130.tex' (notop nobot);
proc print data=estimates2 noobs label split=" ";
   var Variable DF Estimate StdErr tValue Probt LowerCl UpperCL; 
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='wtlen-SAS-140.tex' (notop nobot);
proc print data=Predictions2 label split=" ";
   var length log_length weight log_weight PredictedValue StdErrMeanPredict LowerCLMean UpperCLMean LowerCL UpperCL;
run;
ods tagsets.mycolorlatex close;


ods listing;
ods graphics on / imagefmt=png imagename='wtlen-SAS-150' reset=index;
proc document name=ciplot2;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='wtlen-SAS-150b' reset=index;
proc document name=regfit2;
   replay \Reg#1\MODEL1#1\ObswiseStats#1\log_weight#1\FitPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* diagnostic plots */

ods listing;
ods graphics on / imagefmt=png imagename='wtlen-SAS-160' reset=index;
proc document name=regfit2;
   replay \Reg#1\MODEL1#1\ObswiseStats#1\log_weight#1\DiagnosticPlots#1\DiagnosticsPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;;
 
ods tagsets.mycolorlatex file='wtlen-SAS-330.tex' (notop nobot);
proc print data=Nlinestimates noobs label split=" ";
run;
ods tagsets.mycolorlatex close;
