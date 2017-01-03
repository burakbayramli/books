/* ANCOVA

An unfortunate byproduct of pulp-and-paper production used to be dioxins - a very
hazardous material. This material was discharged into waterways
with the pulp-and-paper effluent where it bioaccumulated in living
organisms such a crabs. Newer processes have eliminated
this by product, but the dioxins in the organisms takes
a long time to degrade.

Government environmental protection agencies take samples of crabs
from affected areas each year and measure the amount of dioxins
in the tissue. The following example is based on a real study.

Each year, four crabs are captured from two monitoring stations which
are situated quite a distance apart on the same inlet where
the pulp mill was located..
The liver is excised and the livers from all four crabs are
composited together into a single sample.
\footnote{Compositing is a common analytical tool. There is little
loss of useful information induced by the compositing process - the
only loss of information is the among individual-sample variability
which can be used to determine the optimal allocation between
samples within years and the number of years to monitor.}
The dioxins levels
in this composite sample is measured. As there are many different
forms of dioxins with different toxicities, a summary measure, called
the Total Equivalent Dose (TEQ) is computed from the sample.

As seen in the chapter on regression, the appropriate response
variable is $log(TEQ)$.

Is the rate of decline the same for both sites? Did the sites
have the same initial concentration? */

/* 2014-08-10 CJS Update for SAS 9.4 */
dm 'log'    clear;
dm 'output' clear;
proc datasets kill; run;
title; footnote ' '; run;
ods graphics on; run;

options orientation=landscape;
ods pdf file='dioxin2-SAS.pdf' style=styles.printer;

title 'Degradation of dioxin at two sites';

/* read in the data */
*---part010b;
data dioxin2;
   infile 'dioxin2.csv' dlm=',' dsd missover firstobs=2;
   length site $10;
   input site $ year TEQ;
   logTEQ = log(TEQ);
run;
*---part010e;


proc print data=dioxin2;
   title2 'raw data';
run;
 
ods document name=prelimplot(write);
/* Create a preliminary plot of the data */
*---partprelimplotb;
ods graphics / reset attrpriority=none;
proc sgplot data=dioxin2;
   title2 'Preliminary plot of log(TEQ)';
   scatter x=year y=logTEQ / group=site;
   xaxis label="Year" offsetmin=0.05 offsetmax=0.05;
   yaxis label="log(TEQ)";
run;
*---partprelimplote;
ods document close;

/* Fit a model to each group separately */
*---partregfitb;
proc sort data=dioxin2; by site year; run;
ods graphics on;
proc reg data=dioxin2 plots=all;
   title2 'Separate slope fit to each group';
   by site;
   model logTEQ = year / clb;
   ods output ParameterEstimates=regfitcoef;
   ods output fitstatistics     =regfitfitstatistics;
run;
*---partregfite;



ods document name=glmfitnonp(write);
/* Fit the model with non-parallel slopes */
*---partglmfitnonpb;
ods graphics on;
proc glm data=dioxin2 plots=all;
   title2 'Non-parallel slope model';
   class site;   
   model logTEQ = site year site*year / solution clparm;
   estimate 'Site A intercept ' intercept 1 site 1 0;
   estimate 'Site A slope'      year 1 site*year 1 0;
   estimate 'Site B intercept' intercept 1 site 0 1;
   estimate 'Site B slope'      year 1 site*year 0 1;
   ods output ModelAnova=glmfitnonpanova;
   ods output Estimates =glmfitnonpestimates;
run;
*---partglmfitnonpe;
ods document close;

/* Get the individual slopes in one fell swoop */
*---partglmcompslopesb;
proc glm data=dioxin2;
   title2 'Non-parallel slope model with separate estimates of slopes';
   class site;
   model logTEQ = site site*year / noint solution clparm;
   ods output parameterEstimates = compslopes;
run;
*---partglmcompslopese;



ods document name=glmfitp(write);
/* Fit the  model with parallel slopes */
/* unfortunately, in GLM, you can't specify both clm and cli - only one is processed.
   this means that we need to run the model twice to get both the confidence
   interval for the mean (CLM) and prediction intervals for individual responses
   (Cli) and then merge them together. */
*---partglmfitpb;
ods graphics on;
proc glm data=dioxin2 plots=all;
   title2 'Parallel slopes model';
   class site;
   model logTEQ = site year / p  cli clparm;
   estimate 'Site A intercept' intercept 1 site 1 0;
   estimate 'Site B intercept' intercept 1 site 0 1;
   estimate 'common slope' year 1;
   lsmeans site / pdiff cl;
   ods output ModelAnova     =glmfitpanova;
   ods output Estimates      =glmfitpestimates;
   ods output predictedvalues=glmfitppredictedvaluescli;
   ods output lsmeandiffcl   =glmfitplsmeandiffcl;
   ods output predictedinfo  =glmfitppredictedinfo;
run;
*---partglmfitpe;
ods document close;

proc glm data=dioxin2;
   title2 'parallel slope model but now get the CLM';
   class site;
   model logTEQ = site year / clm;
   ods output predictedvalues=glmfitppredictedvaluesclm;
run;


data fittedmodel;   /* join the actual and predicted values together */
   merge dioxin2 glmfitppredictedvaluescli(rename=(lowerCL=lcli upperCL=ucli))
                 glmfitppredictedvaluesclm(rename=(lowerCL=lclm upperCL=uclm));
   drop dependent observed individual biased observation;
   format _numeric_ 6.2;
   format year 4.0;
run;

proc print data=fittedmodel;
   title3 'Estimated predictions and confidence intervals';
run;

ods document name=finalplot(write);
ods graphics / reset attrpriority=none;
proc sgplot data=fittedmodel;
   title2 'Fitted model and confidence intervals';
   band   x=year lower=lclm upper=uclm /group=site fillattrs=(transparency=0.3 ); 
   band   x=year lower=lcli upper=ucli /group=site fillattrs=(transparency=0.9);
   scatter y=logTEQ    x=year / group=site;
   series  y=predicted x=year / group=site;

   xaxis label='Year' offsetmin=0.05 offsetmax=0.05;
   yaxis label='logTEQ';
run;
ods document close;


ods pdf close;


/* create the files to be included in the LaTex document */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=glmfitp;
   list /levels=all;
run;




ods tagsets.mylatex file='dioxin2-SAS-010.tex' (notop nobot);
proc print data=dioxin2(obs=10) label split=" ";
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='dioxin2-SAS-prelimplot' reset=index;
proc document name=prelimplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mylatex file='dioxin2-SAS-regfitcoef.tex' (notop nobot);
proc print data=regfitcoef(drop=model dependent tvalue probt) noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='dioxin2-SAS-regfitfitstatistics.tex' (notop nobot);
proc print data=regfitfitstatistics noobs label split=' ';
   where label1="Root MSE";
   var site label1 nvalue1;
   label label1='Statistic';
   label nvalue1='RMSE';
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file='dioxin2-SAS-glmfitnonpestimates.tex' (notop nobot);
proc print data=glmfitnonpestimates(drop=dependent tvalue probt) noobs label split=' ';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='dioxin2-SAS-glmcompslopes.tex' (notop nobot);
proc print data=compslopes; * noobs label split=' ';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='dioxin2-SAS-glmfitnonpanova.tex' (notop nobot);
proc print data=glmfitnonpanova(drop=dependent) noobs label split=' ';
   where HypothesisType=3;
run;
ods tagsets.mylatex close;



ods tagsets.mylatex file='dioxin2-SAS-glmfitpanova.tex' (notop nobot);
proc print data=glmfitpanova(drop=dependent) noobs label split=' ';
   where HypothesisType=3;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='dioxin2-SAS-glmfitpestimates.tex' (notop nobot);
proc print data=glmfitpestimates(drop=dependent tvalue probt) noobs label split=' ';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='dioxin2-SAS-glmfitplsmeandiffcl.tex' (notop nobot);
proc print data=glmfitplsmeandiffcl(drop=dependent i j) noobs label split=' ';
run;
ods tagsets.mylatex close;


ods listing;
ods graphics on / imagefmt=png imagename='dioxin2-SAS-diagplot' reset=index;
proc document name=glmfitp;
   replay  	\GLM#1\ANOVA#1\logTEQ#1\DiagnosticsPanel#1/ dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mylatex file='dioxin2-SAS-glmfitpdw.tex' (notop nobot);
proc print data=glmfitppredictedinfo noobs label split=' ';
   where index(statistic, "Dur")>0 or index(statistic,"First")>0; 
   var statistic nvalue1;
   label nvalue1='Value';
run;
ods tagsets.mylatex close;



ods tagsets.mylatex file='dioxin2-SAS-predci-values.tex' (notop nobot);
proc print data=fittedmodel(obs=5) label split=" " noobs;
   var year predicted lclm uclm lcli ucli;
   label lclm='Lower 95% MEAN';
   label uclm='Upper 95% MEAN';
   label lcli='Lower 95% INDIV';
   label ucli='Upper 95% INDIV';
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='dioxin2-SAS-finalplot' reset=index;
proc document name=finalplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;



