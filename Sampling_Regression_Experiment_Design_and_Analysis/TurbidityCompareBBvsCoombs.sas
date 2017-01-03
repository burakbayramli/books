/* French Creek was monitored for several months at two sites (Barclay Bride and Coombs).
   At the synpotic times, several water quality variables were measured, including
   Turbidity in NTU.
 
   We will compare the Turbidity between Barclay Bridge and Coombs
  
   Lines starting with *---part001b; or *---part001e; bracket the source 
  line for inclusion by LaTex and usually are not coded.
*/

/* Analysis of French Creek Turbidity at Barclay Bay and Coombs using SAS */
dm 'output' clear;
dm 'log'    clear; 
proc datasets kill; run;

title 'Comparing Turbidity at two sites measured same times - example of paired t-test';

options nodate nonumber noovp orientation=landscape;
ods document name=work.output(write);  /* enable selection of output from SAS */
ods pdf file='TurbidityCompareBBvsCoombs.pdf';
goptions device=pdf colors=(black) rotate=landscape;

*---part001b;
data turbidity;
   length SampleTime $10.;
   infile 'TurbidityCompare.csv' dlm=',' dsd missover firstobs=2;
   input SampleTime BB Coombs;
   logBB     = log(BB);
   logCoombs = log(Coombs);
   format logBB logCoombs 7.2;
run;
*---part001e;


proc print data=turbidity;
   title2 'raw data';
run;

 
/****************************** Paired t-test and differences comparison **************/
*---part003b;
data turbidity2;
   set turbidity;
   logratio =logBB-logCoombs;  /* explicitly compute the log-ratio */
run;
*---part003e;
 
/* create a time plot of the logratio */
ods document name=plot1(write);
proc gplot data=turbidity2;
   title2 'Plot of the log-ratio over time';
   axis1 label=(a=90 r=0 'Log ratio (BB/Coombs)') ;
   axis2 label=(         'Sample Time')  offset=(1 cm, 1 cm);
   plot logratio*SampleTime = 1 / vaxis=axis1 haxis=axis2;
   symbol1 v=dot i=join;
run;
ods document close;
   
proc univariate data=turbidity2 plots cibasic;
   title2 'examine the log ratio explicitly';
   var logratio;
   histogram logratio;
   ods output TestsForLocation=TestLogRatio1;
   ods output BasicIntervals  =CIMeanLogRatio1;
run;

ods document name=ttest1(write);
*---part005b;
ods graphics on;
proc ttest data=turbidity2;
   title2 'Examine the log ratio (BB/Coombs)';
   var logratio;
   ods output TTests=TestLogRatio2;
   ods output ConfLimits=CIMeanLogRatio2;
run;
ods graphics off;
*---part005e;
ods document close;

ods document name=ttest2(write)
*---part010b;
ods graphics on;
proc ttest data=turbidity;
   title2 'Paired t-test on log(turbidity) data';
   pair logBB*logCoombs;
run;
ods graphics off;
*---part010e;
ods document close;



/****************************** Now for a modelling approach ***********/
 
/* First stack the data */
*---part021b;
data turbidity3;
   set turbidity;
   length SiteName $10;
   sitename = "BB";     logturbidity = logBB;     output;
   sitename = 'Coombs'; logturbidity = logCoombs; output;
   keep SampleTime SiteName logturbidity;
run;
*---part021e;
 
proc print data=turbidity3(obs=10);
   title2 'part of the transposed raw data';
run;

ods document name=mixed3(write);
*---part025b;
ods graphics on;
proc mixed data=turbidity3 plots=all;
   title2 'Modelling approach as an RCB using MIXED';
   class SampleTime SiteName;
   model logturbidity = SampleTime SiteName/ ddfm=KR;                 
   lsmeans SiteName / diff pdiff;  
   ods output tests3=Test3; 
   ods output lsmeans=lsmeans3;
   ods output diffs=diffs3;
run;
ods graphics off;
*---part025e;
ods document close;

ods pdf close;


/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=plot1;
 list /levels=all;
run;

ods tagsets.mycolorlatex file='TurbidityCompareBBvsCoombs-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=turbidity;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='TurbidityCompareBBvsCoombs-SAS-003.tex' (notop nobot);
proc print data=turbidity2;
run;
ods tagsets.mycolorlatex close;

ods listing;
filename graphout "TurbidityCompareBBvsCoombs-SAS-004.png";
goptions device=png gsfname=graphout;
goptions reset=symbol;
proc document name=plot1;
    replay \Gplot#1\GPLOT#1 / dest=listing;
run;
ods listing close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='TurbidityCompareBBvsCoombs-SAS-004a' reset=index;
proc document name=ttest1;
   replay \Ttest#1\logratio#1\SummaryPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;

ods tagsets.mycolorlatex file='TurbidityCompareBBvsCoombs-SAS-005.tex' (notop nobot);
proc print data=TestLogRatio1 noobs label split=" " ;
   *where index(test,"Student")>0;
run;
proc print data=CIMeanLogRatio1 noobs label split=" " ;
   where index(parameter,"Mean")>0;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='TurbidityCompareBBvsCoombs-SAS-005a.tex' (notop nobot);
proc print data=TestLogRatio2 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;
ods tagsets.mycolorlatex file='TurbidityCompareBBvsCoombs-SAS-005b.tex' (notop nobot);
proc print data=CIMeanLogRatio2 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='TurbidityCompareBBvsCoombs-SAS-006' reset=index;
proc document name=ttest2;
   replay \Ttest#1\logBB_logCoombs#1\AgreementPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;

ods tagsets.mycolorlatex file='TurbidityCompareBBvsCoombs-SAS-021.tex' (notop nobot);
proc print data=turbidity3(obs=10);
   title2 'part of the transposed raw data';
run;
ods tagsets.mycolorlatex close;
title;

ods tagsets.mycolorlatex file='TurbidityCompareBBvsCoombs-SAS-025.tex' (notop nobot);
proc print data=test3 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='TurbidityCompareBBvsCoombs-SAS-026.tex' (notop nobot);
proc print data=lsmeans3 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='TurbidityCompareBBvsCoombs-SAS-027.tex' (notop nobot);
proc print data=diffs3 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='TurbidityCompareBBvsCoombs-SAS-024' reset=index;
proc document name=mixed3;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;
