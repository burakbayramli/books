/* Paired t-test - Comparison of lesion counts from paired application to common unit */

/*  A single leaf is taken from 11 different tobacco plants.
    Each leaf is divided in
    half, and given one of two preparations of mosaic virus. We wish to examine if
    there is a difference in the mean number of lesions from the two preparations. */

/*  Lines starting with *---part001b; or *---part001e; bracket the source 
  line for inclusion by LaTex and usually are not coded.
*/
dm 'output' clear;
dm 'log'    clear; 
proc datasets kill; run;


title 'Comparison of two preparations of mosaic virus using applications to common units';
options nodate  noovp orientation=landscape;
ods pdf file='paired-virus-SAS.pdf';
goptions device=pdf colors=(black) rotate=landscape;

*---part001b;
data plants;
   infile 'paired-virus.csv' dlm=',' dsd missover firstobs=2;
   input plant prep1 prep2;
   diff = prep1 - prep2;
run;
*---part001e;

proc print data=plants;
   title2 'raw data - note outlier in plant 6 where prep 1 is inferior to prep 2';
run;


/* Plot lab1 vs lab2 */
ods document name=plot1(write);
*---part002b;
proc sgplot data=plants;
   title2 'Plot of Lab1 vs Lab2';
   scatter y=prep1 x=prep2;
   lineparm x=0 y=0 slope=1;
run;
*---part002e;
ods document close;



/*************************************************************************/
/* Analysis 1 - do a t-test on the differences  */
ods document name=ttest1(write);
ods graphics on;
proc ttest data=plants;
   title2 'test if the mean difference is 0 - outlier included';
   var diff;
   ods output Statistics=Stat1;
   ods output TTests=Test1;
   ods output ConfLimits=CIMean1;
run;
ods graphics off;
ods document close;

proc print data=stat1 noobs label split=' ';
   var variable n mean stderr lowerclmean upperclmean;
run;

/**************************************************************************/
/* Analysis 2 - do a paired t-test */
ods document name=ttest2(write);
*---part011b;
ods graphics on;
proc ttest data=plants dist=normal; /* dist=normal gives the se */
   title2 'do a paired t-test - outlier included ';
   paired prep1*prep2;
   ods output Statistics=Stat2;
   ods output TTests=Test2;
   ods output ConfLimits=CIMean2;
run;
ods graphics off;
*---part011e;
ods document close;

/* Analysis 3 - do a generalized linear model.
   Must stack the data and then use GLM */
data stackedplants;
   set plants;
   prep = 'prep1';
   lesions = prep1;
   output;
   prep = 'prep2';
   lesions = prep2;
   output;
   keep prep plant lesions;
run;
 
proc print data=stackedplants;
   title2 'stacked dataset - outlier included';
run;

ods document name=mixed3(write);
ods graphics on;
proc mixed data=stackedplants plots=all;
   title2 'analysis using MIXED - outlier included';
   class prep plant;
   model lesions = plant prep;  /* plant is the blocking variable */
   lsmeans prep / cl pdiff;
   ods output tests3=Test3; 
   ods output lsmeans=lsmeans3;
   ods output diffs=diffs3;
run;
ods graphics off;
ods document close;
run;


/* Now to repeat the analysis after the outlier in Farm 2 is fixed */
data plants;
  set plants;
  if plant=6 then delete;
run;
data stackedplants;
   set stackedplants;
   if plant=6 then delete;
run;

proc print data=plants;
   title2 'raw data after outlier is removed';
run;
proc print data=stackedplants;
   title2 'stacked raw data after outlier is removed';
run;

/*************************************************************************/
/* Analysis 1 - do a t-test on the differences  */
ods document name=ttest1b(write);
ods graphics on;
proc ttest data=plants;
   title2 'test if the mean difference is 0 - outlier removed';
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
proc ttest data=plants;
   title2 'do a paired t-test - outlier removed ';
   paired prep1*prep2;
   ods output TTests=Test2b;
   ods output ConfLimits=CIMean2b;
run;
ods graphics off;
*---part011e;
ods document close;

/* Analysis 3 - do a generalized linear model.*/
 
ods document name=mixed3b(write);
ods graphics on;
proc mixed data=stackedplants plots=all;
   title2 'analysis using MIXED - outlier removed';
   class prep plant;
   model lesions = plant prep;  /* plant is the blocking variable */
   lsmeans prep / cl pdiff;
   ods output tests3=Test3b; 
   ods output lsmeans=lsmeans3b;
   ods output diffs=diffs3b;
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

proc document name=ttest2;
 list /levels=all;
run;

ods tagsets.mycolorlatex file='paired-virus-SAS-001.tex' (notop nobot) /*stylesheet="sas.sty" */;
proc print data=plants;
run;
ods tagsets.mycolorlatex close;


ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='paired-virus-SAS-002' reset=index;
proc document name=plot1;
   replay \Sgplot#1\SGPlot#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;



ods tagsets.mycolorlatex file='paired-virus-SAS-005.tex' (notop nobot);
proc print data=Test1 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='paired-virus-SAS-006.tex' (notop nobot);
proc print data=CIMean1 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;



ods tagsets.mycolorlatex file='paired-virus-SAS-011.tex' (notop nobot);
proc print data=Test2 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='paired-virus-SAS-012.tex' (notop nobot);
proc print data=stat2 noobs label split=' ';
   var variable1 variable2 difference n mean stderr lowerclmean upperclmean;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='paired-virus-SAS-014' reset=index;
proc document name=ttest2;
   replay \Ttest#1\prep1_prep2#1\SummaryPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;


ods tagsets.mycolorlatex file='paired-virus-SAS-021.tex' (notop nobot);
proc print data=stackedplants(obs=10);
   title2 'part of the transposed raw data';
run;
ods tagsets.mycolorlatex close;
title;

ods tagsets.mycolorlatex file='paired-virus-SAS-022.tex' (notop nobot);
proc print data=test3 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods tagsets.mycolorlatex file='paired-virus-SAS-023.tex' (notop nobot);
proc print data=lsmeans3 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;


ods tagsets.mycolorlatex file='paired-virus-SAS-024.tex' (notop nobot);
proc print data=diffs3 noobs label split=" " ;
run;
ods tagsets.mycolorlatex close;

ods listing;
goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='paired-virus-SAS-025' reset=index;
proc document name=mixed3;
   replay \Mixed#1\ResidualPlots#1\ResidualPanel#1 / dest=listing;
run;
ods graphics off;
ods listing close;
run;

