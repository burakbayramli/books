/* Number of mink pelts from Saskatchewan 

This is data series 3707 in the 
   NERC Centre for Population Biology, Imperial College (1999) 
   The Global Population Dynamics Database.
   http://www.sw.ic.ac.uk/cpb/cpb/gpdd.html"  */

/* 2014-08-10 CJS Update for SAS 9.4 */
dm 'log'    clear;
dm 'output' clear;
proc datasets kill; run;
footnote ' ';

options orientation=landscape;
ods pdf file='mink-SAS.pdf' style=styles.printer;

title 'Mink pelts from Saskatchewan';
 
*---part010b;
data mink;
   infile 'mink.csv' firstobs=2 dlm=',' dsd missover;
   input year mink;
   log_mink = log(mink);
   format log_mink  7.1;
run;
*---part010e;

proc print data=mink;
   title2 'raw data';
run;


ods document name=prelimplot(write);
*---partprelimplotb;
proc sgplot data=mink;
   title2 'Time series plot';
   scatter x=year y=log_mink;
   series  x=year y=log_mink;
   reg     x=year y=log_mink;
   xaxis label='Year' offsetmin=0.05 offsetmax=0.05;
   yaxis label='log(Mink)';
run;
*---partprelimplote;
ods document close;


ods document name=regfit(write);
/* Fit the ordinary regression line, get residuals, and plot the residuals */
*---partregfitb;
ods graphics on;
proc reg data=mink plots=all;
   title2 'Ordinary fit ignoring autocorrelation';
   model log_mink = year / r dw dwprob clm cli clb;
   output out=fitmink r=residual uclm=uclm lclm=lclm ucl=ucli lcl=lcli p=predreg;
   ods output OutputStatistics    =regfitOutputStatistics;
   ods output ParameterEstimates  =regfitParameterEstimates;
   ods output dwstatistic         =regfitDWstatistic;
run;
*---partregfite;
ods document close;


data fitmink;
   merge fitmink mink;
   drop model dependent observation depvar;
   lag_resid = lag(residual); /* get the previous residual */
   format _numeric_ 6.1;
   format year 5.0;
run;

proc print data=fitmink;
   title3 'Predicted values and confidence intervals';
run;

ods document name=regfitresid(write);
proc sgplot data=fitmink;
   title2 'Residual plot over time';
   scatter x=year y=residual;
   series  x=year y=residual;
   refline 0 / axis=y;
   xaxis label='Year' offsetmin=0.05 offsetmax=0.05;
   yaxis label='Residuals from simple regression fit';
run;
ods document close;

ods document name=regfitlagresid(write);
proc sgplot data=fitmink;
   title2 'Residual vs lagged(residual)';
   scatter x=lag_resid y=residual;
   xaxis label='lagged(residual)' offsetmin=0.05 offsetmax=0.05;
   yaxis label='Residuals from simple regression fit';
   footnote 'There is clear evidence of autocorrelation';
run;
ods document close;


 
/*******************************************************************************/

/* Use AutoReg to fit the data */
/* Note that if there are missing values, they must be included in the series
   so that AutoReg can compute the autocorrelation among the observations correctly*/
ods document name=autoregfit(write);
*---partautoregfitb;
ods graphics on; 
proc autoreg data=mink plots=all;
   title2 'Using AutoReg to fit the data';
   model log_mink  = year / nlag=1 dw=1 dwprob;
   output out=fitautoreg r=residual uclm=uclm lclm=lclm ucl=ucli lcl=lcli p=predauto pm=predmean;
   ods output ParameterEstimates   =autoregfitParameterEstimates;
   ods output ARparameterEstimates =autoregfitARparameterEstimates;;
   footnote 'Note that SAS gives the negative of the usual autocorrelation coefficient!';
run;  
*---partautoregfite;
ods document close;

data fitautoreg;
   merge fitautoreg mink;
run;

proc print data=fitautoreg;
   title3 'Residuals and fitted values from AutoReg';
run;

ods document name=autoregfitplot(write);
proc sgplot data=fitautoreg;
   title2 'Fitted curve using AUTOREG';
   scatter x=year y=log_mink;
   series  x=year y=log_mink;
   series  x=year y=predmean;
   series  x=year y=predauto/ lineattrs=(pattern=dash color=green);
   xaxis label='Year' offsetmin=0.05 offsetmax=0.05;
   yaxis label='log(Mink)';
run;
ods document close;



/**********************************************************************************/
/****** Also fit an AR(1) process using MIXED *****/
ods graphics on;
proc autoreg data=mink plot=all;
   title2 'Using AutoReg to fit the data using ML';
   model log_mink  = year / nlag=1 method=ml;
   output out=fitautoreg r=residual uclm=uclm lclm=lclm ucl=ucli lcl=lcli p=predauto pm=predmean;
   footnote 'Note that SAS gives the negative of the usual autocorrelation coefficient!';
run;  

*---partmixedfitb;
proc sort data=mink; by year;
ods graphics on;
proc mixed data=mink method=ml plot=all;
   title2 'Fit AR(1) process using MIXED';
   model log_mink= year / solution ddfm=satterth;
   repeated   /subject=intercept type=ar(1);
   ods output solutionF= mixedfitSolutionF;
   ods output covparms = mixedfitCovParms;
   footnote;
run;
*---partmixedfite;

ods pdf close;





/* create the files to be included in the LaTex document */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=autoregfit;
   list /levels=all;
run;


ods listing;
ods graphics on / imagefmt=png imagename='mink-SAS-prelimplot' reset=index;
proc document name=prelimplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mylatex file='mink-SAS-010.tex' (notop nobot);
proc print data=mink(obs=10) label split=" ";
run;
ods tagsets.mylatex close;



ods tagsets.mylatex file='mink-SAS-regfitparameterestimates.tex' (notop nobot);
proc print data=regfitParameterEstimates label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='mink-SAS-regfitdwstatistics.tex' (notop nobot);
proc print data=regfitdwstatistic label split=" ";
   var label1 nvalue1;
   label label1='Statistic';
   label nvalue1='Value';
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='mink-SAS-regfitOutputStatistics.tex' (notop nobot);
proc print data=regfitOutputStatistics label split=" " noobs;
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='mink-SAS-regfitdiagplot' reset=index;
proc document name=regfit;
   replay  	\Reg#1\MODEL1#1\ObswiseStats#1\log_mink#1\DiagnosticPlots#1\DiagnosticsPanel#1/ dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='mink-SAS-regfitresid' reset=index;
proc document name=regfitresid;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='mink-SAS-regfitlagresid' reset=index;
proc document name=regfitlagresid;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;



ods tagsets.mylatex file='mink-SAS-autoregfitparameterestimates.tex' (notop nobot);
proc print data=autoregfitParameterEstimates label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='mink-SAS-autoregfitARparameterEstimates.tex' (notop nobot);
proc print data=autoregfitARparameterEstimates label split=" " noobs;
run;
ods tagsets.mylatex close;

ods listing;
ods graphics on / imagefmt=png imagename='mink-SAS-autoregfitplot' reset=index;
proc document name=autoregfitplot;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;

ods listing;
ods graphics on / imagefmt=png imagename='mink-SAS-autoregdiagplot' reset=index;
proc document name=autoregfit;
   replay  	\Autoreg#1\Model1#1\FitDiagnosticsPlots#1\DiagnosticsPanel#2/ dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mylatex file='mink-SAS-mixedfitSolutionF.tex' (notop nobot);
proc print data=mixedfitSolutionF label split=" " noobs;
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file='mink-SAS-mixedfitCovParms.tex' (notop nobot);
proc print data=mixedfitCovParms label split=" " noobs;
run;
ods tagsets.mylatex close;

