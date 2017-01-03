/* Example of using SAS to compute a ratio estimator in survey sampling */
/* 2015-07-04 CJS Updated for later version of SAS */
/* ods tagsets.tablesonlylatex  used for LaTEx file */


/* A wildlife biologist has estimated the grouse population
   in a region containing isolated areas (called pockets) of
   bush as follows: She selected 12 pockets of bush at random, and
   attempted to count the numbers of grouse in each of of these.
   (One can assume that the grouse are almost all found in the bush, and for the
   purpose of this question, that the counts were perfectly accurate.)
   The total number of pockets of bush in the region is 248, 
   comprising a total area of 3015 hectares. */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill; run; 

title 'Number of grouse - Ratio estimator';
ods pdf file='grouse-SAS.pdf' style=styles.printer;

*---part001b;
data grouse; 
   infile 'grouse.csv' dlm=',' dsd missover firstobs=2;
   input area grouse;  /* sampling weights not needed */
run;
*---part001e;

proc print data=grouse;
   title2 'raw data';
run;

*---partsimpleb;
proc means data=grouse noprint;
   var grouse;
   output out=sampsize n=n;
run;
data grouse; /* get the survey weights */
   set grouse;
   one=1;
   set sampsize point=one;
   sampweight = 248 / n;
run;

proc surveymeans data=grouse  mean sum clm clsum     N=248;
   title2 'Estimation using a simple expansion estimator estimator';
   var grouse;
   weight sampweight;
   ods output statistics=grousesimpleresults;
run;
*---partsimplee;



ods document name=plot1(write);
*---part002b;
proc sgplot data=grouse;
   title2 'plot to assess assumptions';
   scatter y=grouse x=area;
run;
*---part002e;
ods document close;

*---part003b;
proc surveymeans data=grouse   ratio clm      N=248;
   /* the ratio clm keywords request a ratio estimator and a confidence interval. */
   title2 'Estimation using a ratio estimator';
   var grouse area;
   ratio grouse / area;
   ods output statistics=grouseresults;
   ods output ratio     =grouseratio;   /* extract information so that total can be estimated */
run;
*---part003e;

*---part004b;
data outratio;
   /* compute estimates of the total */
   set grouseratio;
   Est_total = ratio * 3015;
   Se_total  = stderr* 3015;
   UCL_total = uppercl*3015;
   LCL_total = lowercl*3015;
   format est_total se_total ucl_total lcl_total 7.1;
   format ratio stderr lowercl uppercl  7.3;
run;
*---part004e;
 
proc print data=outratio split='_';
   title2 'the computed estimates';
   var ratio stderr lowercl uppercl Est_total Se_total LCL_total UCL_total;
run;


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


ods tagsets.mylatex file="grouse-SAS-data.tex" (notop nobot) /* newfile=table */;
proc print data=grouse;
run;
ods tagset.mylatex close;

ods tagsets.mylatex file="grouse-SAS-simple.tex" (notop nobot) /* newfile=table */;
proc print data=grousesimpleresults noobs label split=" ";
   var varname  mean stderr lowerclmean upperclmean sum stddev lowerclsum upperclsum; 
   format mean lowerCLMean Sum LowerCLSum stderr upperCLmean stddev UpperCLsum 10.3;
   label LowerCLMean='LCL Mean';
   label UpperCLMean='UCL Mean';
   label stderr     ='SE Mean';
   label LowerCLSum ='LCL Sum';
   label upperCLsum ='UCL Sum';
   label stddev     ='SE sum';
run;
ods tagsets.mylatex close;

goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='grouse-SAS-prelim' reset=index;
proc document name=plot1;
   replay \Sgplot#1\SGplot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mylatex file="grouse-SAS-results.tex" (notop nobot) /* newfile=table */;
proc print data=grouseresults noobs label split=" ";
   var varname mean stderr lowerclmean upperclmean ; 
   format mean lowerCLMean stderr upperCLmean 10.1;
   label LowerCLMean='LCL Mean';
   label UpperCLMean='UCL Mean';
   label stderr     ='SE Mean';
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file="grouse-SAS-ratio.tex" (notop nobot) /* newfile=table */;
proc print data=grouseratio noobs label split=" ";
run;
ods tagsets.mylatex close;

ods tagsets.mylatex file="grouse-SAS-total.tex" (notop nobot) /* newfile=table */;
proc print data=outratio split='_';
   var ratio stderr lowercl uppercl Est_total Se_total LCL_total UCL_total;
run;
ods tagsets.mylatex close;




