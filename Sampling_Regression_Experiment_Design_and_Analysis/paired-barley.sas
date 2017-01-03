/* Paired t-test - Comparison of yield from two farms via paired plantings */

/*  Two varieties of barley are to be compared to see if the mean yield for the second
    variety is different than the mean yield for the first variety.

    Ten farms (from various parts of Manitoba) were selected, and both varieties
    were planted on each farm. */

/*  Lines starting with *---part001b; or *---part001e; bracket the source 
  line for inclusion by LaTex and usually are not coded.
*/
dm 'output' clear;
dm 'log'    clear; 
proc datasets kill; run;


title 'Comparison of two varieties of barley  using paired plantings';
options nodate noovp orientation=landscape;
ods pdf file='paired-barley.pdf';
goptions device=pdf colors=(black) rotate=landscape;

*---part001b;
data farms;
   infile 'paired-barley.csv' dlm=',' dsd firstobs=2;
   input farm V1 V2;
   diff = V1 - V2;
   /* NOTE: The value of 233 on farm 2 is WRONG - it should read 333 */
run; 
*---part001e;

proc print data=farms;
   title2 'raw data - note outlier in Farm 2';
run;

/* Plot v1 vs v2 */
ods document name=plot1(write);
*---part002b;
proc sgplot data=farms;
   title2 'Plot of V1 vs V2';
   scatter y=v1 x=v2;
   lineparm x=0 y=0 slope=1;
run;
*---part002e;
ods document close;

/*************************************************************************/
/* Analysis 1 - do a t-test on the differences  */
ods document name=ttest1(write);
ods graphics on;
proc ttest data=farms;
   title2 'test if the mean difference is 0 - outlier included';
   var diff;
   ods output TTests=Test1;
   ods output ConfLimits=CIMean1;
run;
ods graphics off;
ods document close;

/*************************************************************************/
/* Analysis 2 - do a paired t-test */
ods document name=ttest2(write);
ods graphics on;
proc ttest data=farms;
   title2 'do a paired t-test - outlier included ';
   paired V1*V2;
   ods output TTests=Test2;
   ods output ConfLimits=CIMean2;
run;
ods graphics off;
ods document close;


/*************************************************************************/
/* Analysis 3 - do a generalized linear model.
   Must stack the data and then use GLM */
data stackedfarms;
   set farms;
   variety = 'V1'; yield = V1; output;
   variety = 'V2'; yield = V2; output;
   keep variety farm yield;
run;
 
proc print data=stackedfarms(obs=10);
   title2 'stacked dataset - outlier included';
run;
 
ods document name=mixed3(write);
ods graphics on;
proc mixed data=stackedfarms plots=all;
   title3 'analysis using Mixed - outlier included';
   class variety farm;
   model yield = farm variety;  /* farm is the blocking variable */
   lsmeans variety / cl pdiff;
   ods output tests3=Test3; 
   ods output lsmeans=lsmeans3;
   ods output diffs=diffs3;
run;
ods graphics off;
ods document close;


/*************************************************************************/
/* Now to repeat the analysis after the outlier in Farm 2 is fixed */
*---part010b;
data farms;
  set farms;
  if farm=2 then v1=333;
  diff=v1-v2;
run;
*---part010e;

data stackedfarms;
   set stackedfarms;
   if farm=2 and variety='V1' then yield=333;
run;

proc print data=farms;
   title2 'raw data after outlier is repaired';
run;

proc print data=stackedfarms;
   title2 'stacked raw data after outlier is repaired';
run;

/*************************************************************************/
/* Analysis 1 - do a t-test on the differences  */
ods document name=ttest1b(write);
ods graphics on;
proc ttest data=farms;
   title2 'test if the mean difference is 0 - outlier repaired';
   var diff;
   ods output TTests=Test1b;
   ods output ConfLimits=CIMean1b;
run;
ods graphics off;
ods document close;

/*************************************************************************/
/* Analysis 2 - do a paired t-test */
ods document name=ttest2b(write);
*---part011b;
ods graphics on;
proc ttest data=farms;
   title2 'do a paired t-test - outlier repaired ';
   paired V1*V2;
   ods output Statistics=stat2b;
   ods output TTests=Test2b;
   ods output ConfLimits=CIMean2b;
run;
ods graphics off;
*---part011e;
ods document close;


/*************************************************************************/
/* Analysis 3 - do a generalized linear model.
   Must stack the data and then use GLM */
data stackedfarms;
   set farms;
   variety = 'V1'; yield = V1; output;
   variety = 'V2'; yield = V2; output;
   keep variety farm yield;
run;
 
proc print data=stackedfarms(obs=10);
   title2 'stacked dataset - outlier repaired';
run;
 
ods document name=mixed3b(write);
ods graphics on;
proc mixed data=stackedfarms;
   title3 'analysis using Mixed - outlier included';
   class variety farm;
   model yield = farm variety;  /* farm is the blocking variable */
   lsmeans variety / cl pdiff adjust=tukey;
   ods output tests3=Test3b; 
   ods output lsmeans=lsmeans3b;
   ods output diffs=diffs3b;
run;
ods graphics off;
ods document close;

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

ods tagsets.mycolorlatex file='paired-barley-SAS-001.tex' (notop nobot) stylesheet="sas.sty";
proc print data=farms;
run;
ods tagsets.mycolorlatex close;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='paired-barley-SAS-002' reset=index;
proc document name=plot1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;



ods tagsets.mycolorlatex file='paired-barley-SAS-005.tex' (notop nobot);
proc print data=Test1b noobs label split=" " ;
   *where index(test,"Student")>0;
run;
ods tagsets.mycolorlatex close;
ods tagsets.mycolorlatex file='paired-barley-SAS-006.tex' (notop nobot);
proc print data=CIMean1b noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='paired-barley-SAS-011.tex' (notop nobot);
proc print data=Test2b noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;
ods tagsets.mycolorlatex file='paired-barley-SAS-012.tex' (notop nobot);
proc print data=stat2b noobs label split=' ';
   var variable1 variable2 difference n mean stderr lowerclmean upperclmean;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='paired-barley-SAS-014' reset=index;
proc document name=ttest2b;
   replay \Ttest#1\V1_V2#1\SummaryPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;


ods tagsets.mycolorlatex file='paired-barley-SAS-021.tex' (notop nobot);
proc print data=stackedfarms(obs=10);
   title2 'part of the transposed raw data';
run;
ods tagsets.mycolorlatex close;
title;

ods tagsets.mycolorlatex file='paired-barley-SAS-022.tex' (notop nobot);
proc print data=test3b noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='paired-barley-SAS-023.tex' (notop nobot);
proc print data=lsmeans3b noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='paired-barley-SAS-024.tex' (notop nobot);
proc print data=diffs3b noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='paired-barley-SAS-025' reset=index;
proc document name=mixed3b;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;

