/* Investigate the relationship between mercury levels in blood and the amount of mercury in food eatene */
/* Example of simple linear regression */

/* Can blood mercury be predicted from food blood diets?

  Food blood diets (e.g. number of fish, species of fish, etc) were 
  recorded for a sample of people. Based on estimates of mercury in the 
  food, the mercury in the diet was estimated.  
  A blood sample was also taken from these people 
  and the blood mercury level was also 
  measured. */


/* Lines starting with *---partxxxe and *---parxxxb are used in my Latex file and 
   should be ignored */
/* The tagsets.tablesonlylatex again is used by the Latex Program course notes */
dm 'output' clear;
dm 'log'    clear
proc datasets kill;

title 'Relationship between mercury in blood and in food';
options nodate noovp orientation=landscape;

ods pdf file='mercury.pdf';
goptions device=pdf colors=(black) rotate=landscape;

*---part010b;
data mercury;
   infile 'mercury.csv' dlm=',' dsd missover firstobs=2;
   input intake blood;
   plot_symbol=1;
   if intake = 60 then plot_symbol=2;  /* identify two potential outliers */
   if intake = 600 and blood <200 then plot_symbol=2;
run;
*---part010e;

proc print data=mercury;
   title2 'raw data';
run;


/* Do a regular scatterplot, but use a different symbol for the two plot_symbol groups */
ods document name=initplot(write);
*---part020b;
proc template;
 Define style styles.mystyle;
  Parent=styles.default;
  Style graphdata1 from graphdata1 / MarkerSymbol="CircleFilled"   Color=black Contrastcolor=black;
  Style graphdata2 from graphdata2 / MarkerSymbol="X"              Color=black Contrastcolor=black;
  end;
run;
ods html style=mystyle;
proc sgplot data=mercury;
   title2 'Preliminary data plot';
   scatter y=blood x=intake / group=plot_symbol markerattrs=(size=10px);
   yaxis label='Blood Mercury'   offsetmin=.05 offsetmax=.05;
   xaxis label='Intake Mercury'  offsetmin=.05 offsetmax=.05;
run;
*---part020e;
ods document close;

/* fit the linear model */
ods document name=regfit(write);
*---part030b;
ods graphics on;
proc reg data=mercury plots=all;
   title2 'Fit the model to all of the data';
   model blood = intake / all;
   ods output OutputStatistics  =Predictions;
   ods output ParameterEstimates=Estimates;
run;
ods graphics off;
*---part030e;
ods document close;

*---part040b;
data predictions;  /* merge the original data set with the predictions */
   merge mercury predictions; 
run;
*---part040e;

proc print data=predictions;
   title2 'Predicted values and confidence intervals';
run;

ods document name=ciplot(write);
*---part050b;
ods html style=mystyle;
proc sgplot data=Predictions;
   title2 'Fitted line and confidence curves for mean and individual values';
   band    x=intake lower=lowerCL     upper=upperCL;
   band    x=intake lower=lowerCLmean upper=upperCLmean / fillattrs=(color=red);
   series  y=PredictedValue X=intake;
   scatter y=blood x=intake /  group=plot_symbol markerattrs=(size=10px);
   yaxis label='Blood Mercury'       offsetmin=.05 offsetmax=.05;
   xaxis label='Intake Mercury'  offsetmin=.05 offsetmax=.05;
run;
*---part050e;
ods document close;



/************** Now to refit the data ignoring two outliers *************/
 
*---part110b;
data mercury2;  /* delete the outliers */
   set predictions;
   if plot_symbol ^= 1 then delete;
   if residual > 100 then delete;
   keep blood intake;
run;
*---part110e; 

proc print data=mercury2;
   title2 'raw data after outliers removed';
run;

/* Do a regular scatterplot, but use a different symbol for the two plot_symbol groups */
ods document name=initplot2(write);
*---part120b;
proc sgplot data=mercury2;
   title2 'Preliminary data plot after outliers removed';
   scatter y=blood x=intake /  markerattrs=(symbol=circlefilled);
   yaxis label='Blood Mercury'   offsetmin=.05 offsetmax=.05;
   xaxis label='Intake Mercury'  offsetmin=.05 offsetmax=.05;
run;
*---part120e;
ods document close;

/* fit the linear model */
ods document name=regfit2(write);
*---part130b;
ods graphics on;
proc reg data=mercury2 plots=all;
   title2 'Fit the model after outliers removed';
   model blood = intake / all;
   ods output OutputStatistics  =Predictions2;
   ods output ParameterEstimates=Estimates2;
run;
ods graphics off;
*---part130e;
ods document close;

*---part140b;
data predictions2;  /* merge the original data set with the predictions */
   merge mercury2 predictions2; 
run;
*---part140e;

proc print data=predictions2;
   title2 'Predicted values and confidence intervals after removing outliers';
run;

ods document name=ciplot2(write);
*---part150b;
proc sgplot data=Predictions2;
   title2 'Fitted line and confidence curves for mean and individual values after removing outliers';
   band    x=intake lower=lowerCL     upper=upperCL;
   band    x=intake lower=lowerCLmean upper=upperCLmean / fillattrs=(color=red);
   series  y=PredictedValue X=intake;
   scatter y=blood x=intake /  markerattrs=(symbol=circlefilled);;
   yaxis label='Blood Mercury'       offsetmin=.05 offsetmax=.05;
   xaxis label='Intake Mercury'  offsetmin=.05 offsetmax=.05;
run;
*---part150e;
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


ods tagsets.mycolorlatex file='mercury-SAS-010.tex' (notop nobot);
proc print data=mercury label split=" ";
run;
ods tagsets.mycolorlatex close;

ods listing style=mystyle;
ods graphics on / imagefmt=png imagename='mercury-SAS-020' reset=index;
proc document name=initplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='mercury-SAS-030.tex' (notop nobot);
proc print data=estimates noobs label split=" ";
   var Variable DF Estimate StdErr tValue Probt LowerCl UpperCL; 
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='mercury-SAS-040.tex' (notop nobot);
proc print data=Predictions label split=" ";
   var intake blood PredictedValue StdErrMeanPredict LowerCLMean UpperCLMean LowerCL UpperCL;
run;
ods tagsets.mycolorlatex close;


ods listing;
ods graphics on / imagefmt=png imagename='mercury-SAS-050' reset=index;
ods listing style=mystyle;
proc document name=ciplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='mercury-SAS-050b' reset=index;
proc document name=regfit;
   replay \Reg#1\MODEL1#1\ObswiseStats#1\blood#1\FitPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* diagnostic plots */

ods listing;
ods graphics on / imagefmt=png imagename='mercury-SAS-060' reset=index;
proc document name=regfit;
   replay \Reg#1\MODEL1#1\ObswiseStats#1\blood#1\DiagnosticPlots#1\DiagnosticsPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;;


/* Create graphs and output after outliers removed */
ods tagsets.mycolorlatex file='mercury-SAS-110.tex' (notop nobot);
proc print data=mercury2 label split=" ";
run;
ods tagsets.mycolorlatex close;

ods listing style=mystyle;
ods graphics on / imagefmt=png imagename='mercury-SAS-120' reset=index;
proc document name=initplot2;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mycolorlatex file='mercury-SAS-130.tex' (notop nobot);
proc print data=estimates2 noobs label split=" ";
   var Variable DF Estimate StdErr tValue Probt LowerCl UpperCL; 
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='mercury-SAS-140.tex' (notop nobot);
proc print data=Predictions2 label split=" ";
   var intake blood PredictedValue StdErrMeanPredict LowerCLMean UpperCLMean LowerCL UpperCL;
run;
ods tagsets.mycolorlatex close;


ods listing;
ods graphics on / imagefmt=png imagename='mercury-SAS-150' reset=index;
proc document name=ciplot2;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='mercury-SAS-150b' reset=index;
proc document name=regfit2;
   replay \Reg#1\MODEL1#1\ObswiseStats#1\blood#1\FitPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

/* diagnostic plots */

ods listing;
ods graphics on / imagefmt=png imagename='mercury-SAS-160' reset=index;
proc document name=regfit2;
   replay \Reg#1\MODEL1#1\ObswiseStats#1\blood#1\DiagnosticPlots#1\DiagnosticsPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;;
