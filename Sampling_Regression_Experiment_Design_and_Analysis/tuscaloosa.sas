/* ANCOVA Tuscaloosa Temperaturs

/* 2014-08-10 CJS first editions */

/*  Consider a time series of annual average temperatures measured at 
    Tuscaloosa, Alabama from 1901 to 2001. 
    It is well known that shifts in temperature can occur whenever the 
    instrument or location or observer or other characteristics of the station change.

    In this case, this corresponds to the years
    1901-1938 (inclusive); 1940-1956 (inclusive); 1958-1986 (inclusive), and 
    1989-2000 (inclusive). Note that the years 1939, 1957, and 1987 are NOT 
    used because the average temperature in these two years is an 
    amalgam of two different recording #conditions

    Lines starting in *--part001b; and *---part001e; are used
    to bracket portions of the SAS code for inclusion into my
    course notes and are not normally coded. */


dm 'log'    clear;
dm 'output' clear;
proc datasets kill; run;
footnote ' '; title ' '; run;
ods graphics on; run;

options orientation=landscape;
ods pdf file='tuscaloosa-SAS.pdf' style=styles.printer;

title 'Annual Temperature at Tuscaloosa';

/* read in the data */
*---part010b;
data tuscaloosa;
   infile 'tuscaloosa.csv' dlm=',' dsd missover firstobs=2;
   input year avgtemp epoch $;
run;
*---part010e;


proc print data=tuscaloosa(obs=40);
   title2 'Part of raw data';
run;

data tuscaloosa;
   set tuscaloosa;
   if epoch = ' ' then delete;
run;

 
ods document name=prelimplot(write);
/* Create a preliminary plot of the data */
*---partprelimplotb;
ods graphics / reset attrpriority=none;
proc sgplot data=tuscaloosa;
   title2 'Preliminary plot of the average yearly temperature';
   scatter x=year y=avgtemp / group=epoch;
   refline 1939 1957 1987 / axis=x;
   xaxis label="Year" offsetmin=0.05 offsetmax=0.05;
   yaxis label="Average yearly temperature (C)";
run;
*---partprelimplote;
ods document close;

/* Fit a model to each group separately */
*---partregfitb;
proc sort data=tuscaloosa; by epoch year; run;
proc reg data=tuscaloosa plots=all;
   title2 'Separate slope fit to each epoch';
   by epoch;
   model avgtemp = year / clb;
   ods output ParameterEstimates=regfitcoef;
   ods output fitstatistics     =regfitfitstatistics;
run;
*---partregfite;



ods document name=glmfitnonp(write);
/* Fit the model with non-parallel slopes */
*---partglmfitnonpb;
proc glm data=tuscaloosa plots=all;
   title2 'Non-parallel slope model';
   class epoch; 
   model avgtemp = epoch year epoch*year / solution clparm;
   estimate 'Site A intercept ' intercept 1 epoch 1 0;
   estimate 'Site A slope'      year 1 epoch*year 1 0;
   estimate 'Site B intercept' intercept 1 epoch 0 1;
   estimate 'Site B slope'      year 1 epoch*year 0 1;
   ods output ModelAnova=glmfitnonpanova;
   ods output Estimates =glmfitnonpestimates;
run;
*---partglmfitnonpe;
ods document close;

/* Fit the model with non-parallel slopes to get the individual estimates */
/* Get the individual slopes in one fell swoop */

*---partglmcompslopesb;
proc glm data=tuscaloosa;
   title2 'Non-parallel slope model with separate estimates of slopes';
   class epoch;
   model avgtemp = epoch epoch*year / noint solution clparm;
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
proc glm data=tuscaloosa plots=all;
   title2 'Parallel slopes model';
   class epoch;
   model avgtemp = epoch year / p  cli clparm;
   estimate 'Site A intercept' intercept 1 epoch 1 0;
   estimate 'Site B intercept' intercept 1 epoch 0 1;
   estimate 'common slope' year 1;
   lsmeans epoch / pdiff cl;
   ods output ModelAnova     =glmfitpanova;
   ods output Estimates      =glmfitpestimates;
   ods output predictedvalues=glmfitppredictedvaluescli;
   ods output lsmeandiffcl   =glmfitplsmeandiffcl;
   ods output predictedinfo  =glmfitppredictedinfo;
run;
*---partglmfitpe;
ods document close;



proc glm data=tuscaloosa;
   title2 'parallel slope model but now get the CLM';
   class epoch;
   model avgtemp = epoch year / clm;
   ods output predictedvalues=glmfitppredictedvaluesclm;
run;


data fittedmodel;   /* join the actual and predicted values together */
   merge tuscaloosa glmfitppredictedvaluescli(rename=(lowerCL=lcli upperCL=ucli))
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
   band   x=year lower=lclm upper=uclm /group=epoch fillattrs=(transparency=0.3 ); 
   band   x=year lower=lcli upper=ucli /group=epoch fillattrs=(transparency=0.9);
   scatter y=avgtemp    x=year / group=epoch;
   series  y=predicted x=year / group=epoch;
   xaxis label='Year' offsetmin=0.05 offsetmax=0.05;
   yaxis label='avgtemp';
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




ods tagsets.mylatex file='tuscaloosa-SAS-010.tex' (notop nobot);
proc print data=tuscaloosa(obs=10) label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='tuscaloosa-SAS-partdata.tex' (notop nobot);
proc print data=tuscaloosa(obs=10) label split=" ";
   where 1935 < year < 1945;
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='tuscaloosa-SAS-prelimplot' reset=index;
proc document name=prelimplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods tagsets.mylatex file='tuscaloosa-SAS-regfitcoef.tex' (notop nobot);
proc print data=regfitcoef(drop=model dependent tvalue probt) noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='tuscaloosa-SAS-regfitfitstatistics.tex' (notop nobot);
proc print data=regfitfitstatistics noobs label split=' ';
   where label1="Root MSE";
   var epoch label1 nvalue1;
   label label1='Statistic';
   label nvalue1='RMSE';
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file='tuscaloosa-SAS-glmfitnonpestimates.tex' (notop nobot);
proc print data=glmfitnonpestimates(drop=dependent tvalue probt) noobs label split=' ';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='tuscaloosa-SAS-glmcompslopes.tex' (notop nobot);
proc print data=compslopes; * noobs label split=' ';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='tuscaloosa-SAS-glmfitnonpanova.tex' (notop nobot);
proc print data=glmfitnonpanova(drop=dependent) noobs label split=' ';
   where HypothesisType=3;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='tuscaloosa-SAS-glmfitpdw.tex' (notop nobot);
proc print data=glmfitppredictedinfo noobs label split=' ';
   where index(statistic, "Dur")>0 or index(statistic,"First")>0; 
   var statistic nvalue1;
   label nvalue1='Value';
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='tuscaloosa-SAS-nonpplot' reset=index;
proc document name=glmfitnonp;
   replay \GLM#1\ANOVA#1\avgtemp#1\ANCOVAPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;



ods tagsets.mylatex file='tuscaloosa-SAS-glmfitpanova.tex' (notop nobot);
proc print data=glmfitpanova(drop=dependent) noobs label split=' ';
   where HypothesisType=3;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='tuscaloosa-SAS-glmfitpestimates.tex' (notop nobot);
proc print data=glmfitpestimates(drop=dependent tvalue probt) noobs label split=' ';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='tuscaloosa-SAS-glmfitplsmeandiffcl.tex' (notop nobot);
proc print data=glmfitplsmeandiffcl(drop=dependent i j) noobs label split=' ';
run;
ods tagsets.mylatex close;


ods listing;
ods graphics on / imagefmt=png imagename='tuscaloosa-SAS-diagplot' reset=index;
proc document name=glmfitp;
   replay  	\GLM#1\ANOVA#1\avgtemp#1\DiagnosticsPanel#1/ dest=listing;
run;
ods graphics off;
ods listing close;



ods tagsets.mylatex file='tuscaloosa-SAS-predci-values.tex' (notop nobot);
proc print data=fittedmodel(obs=5) label split=" " noobs;
   var year predicted lclm uclm lcli ucli;
   label lclm='Lower 95% MEAN';
   label uclm='Upper 95% MEAN';
   label lcli='Lower 95% INDIV';
   label ucli='Upper 95% INDIV';
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='tuscaloosa-SAS-finalplot' reset=index;
proc document name=finalplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;



