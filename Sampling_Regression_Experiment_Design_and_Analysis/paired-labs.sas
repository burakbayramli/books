/* Paired t-test - Comparison of impurities from two labs via split-samples */

/* Many medical and other tests are done by independent laboratories. How can
   the regulatory bodies ensure that the work done by these laboratories is of good
   quality?

   A paired design using split samples is often used as a quality control check
   on two laboratories.

   For example, six samples of water are selected, and divided into two parts.
   One half is randomly chosen and sent to Lab 1; the other half is sent to Lab 2.
   Each lab is supposed to measure the impurities in the water.  */


/*  Lines starting with *---part001b; or *---part001e; bracket the source 
  line for inclusion by LaTex and usually are not coded.
*/
dm 'output' clear;
dm 'log'    clear; 
proc datasets kill; run;


title 'Comparison of two labs using split-samples';
options nodate noovp orientation=landscape;
ods pdf file='paired-labs-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

*---part001b;
data labs;
   infile 'paired-labs.csv' dlm=',' dsd missover firstobs=2;
   input sample lab1 lab2;
   diff = lab1 - lab2;
run;
*---part001e;

proc print data=labs;
   title2 'raw data';
run;

/* Plot lab1 vs lab2 */
ods document name=plot1(write);
*---part002b;
proc sgplot data=labs;
   title2 'Plot of Lab1 vs Lab2';
   scatter y=lab1 x=lab2;
   lineparm x=0 y=0 slope=1;
run;
*---part002e;
ods document close;

/*************************************************************************/
/* Analysis 1 - do a t-test on the differences  */
ods document name=ttest1(write);
ods graphics on;
proc ttest data=labs;
   title2 'test if the mean difference is 0';
   var diff;
   ods output TTests=Test1;
   ods output ConfLimits=CIMean1;
run;
ods graphics off;
ods document close;
 

/*************************************************************************/
/* Analysis 2 - do a paired t-test */
ods document name=ttest2(write);
*---part011b;
ods graphics on;
proc ttest data=labs;
   title2 'do a paired t-test';
   paired lab1*lab2;
   ods output Statistics=Stat2;
   ods output TTests=Test2;
   ods output ConfLimits=CIMean2;
run;
ods graphics off;
*---part011e;
ods document close;


/* Analysis 3 - do a generalized linear model.
   Must stack the data and then use GLM */
data stackedlabs;
   set labs;
   lab = 'lab1';
   impurities = lab1;
   output;
   lab = 'lab2';
   impurities = lab2;
   output;
   keep sample lab impurities;
 run;

proc print data=stackedlabs;
   title2 'stacked dataset';
run;

 
ods document name=mixed3(write);
ods graphics on;
proc mixed data=stackedlabs plots=all;
   title3 'analysis using Mixed ';
   class lab sample;
   model impurities = sample lab;  /* sample is the blocking variable */
   lsmeans lab / cl pdiff;
   ods output tests3=Test3; 
   ods output lsmeans=lsmeans3;
   ods output diffs=diffs3;
run;
ods graphics off;
ods document close;
run;

ods pdf close;


/* Now to create the various outputs for my LaTeX files */

/* Create LaTeX files for inclusion by my notes */
%include "../../MyLatexTagset.sas"; run;
title;
footnote;
ods listing;

proc document name=ttest2b;
 list /levels=all;
run;

ods tagsets.mycolorlatex file='paired-labs-SAS-001.tex' (notop nobot) /*stylesheet="sas.sty" */;
proc print data=labs;
run;
ods tagsets.mycolorlatex close;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='paired-labs-SAS-002' reset=index;
proc document name=plot1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;



ods tagsets.mycolorlatex file='paired-labs-SAS-005.tex' (notop nobot);
proc print data=Test1 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='paired-labs-SAS-006.tex' (notop nobot);
proc print data=CIMean1 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;



ods tagsets.mycolorlatex file='paired-labs-SAS-011.tex' (notop nobot);
proc print data=Test2 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='paired-labs-SAS-012.tex' (notop nobot);
proc print data=stat2 noobs label split=' ';
   var variable1 variable2 difference n mean stderr lowerclmean upperclmean;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='paired-labs-SAS-014' reset=index;
proc document name=ttest2;
   replay \Ttest#1\Lab1_Lab2#1\SummaryPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;


ods tagsets.mycolorlatex file='paired-labs-SAS-021.tex' (notop nobot);
proc print data=stackedlabs(obs=10);
   title2 'part of the transposed raw data';
run;
ods tagsets.mycolorlatex close;
title;

ods tagsets.mycolorlatex file='paired-labs-SAS-022.tex' (notop nobot);
proc print data=test3 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='paired-labs-SAS-023.tex' (notop nobot);
proc print data=lsmeans3 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='paired-labs-SAS-024.tex' (notop nobot);
proc print data=diffs3 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='paired-labs-SAS-025' reset=index;
proc document name=mixed3;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;

