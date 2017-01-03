/* Example of a ratio estimator in simple random sampling */
/* 2015-07-04 CJS Update for newer version of SAS */

/* Wildlife ecologists interested in measuring the impact of wolf
   predation on moose populations in BC obtained estimates by aerial
   counting of the population size of wolves and moose on 11
   subareas (all roughly equal size) selected as SRSWOR from a total of
   200 subarea in the game management zone.

   In this example, the actual ratio of wolves to moose is of interest. */

/* The tagsets....latex commands are used to create LaTex tables for my output 
   and really are not needed.
   The *---partxxxb and *---partxxxe lines are directives to LaTex  */

title 'Wolf-moose ratio - ratio estimator in SRS design';
ods pdf file='wolf-SAS.pdf' style=styles.printer;

*---part001b; 
data wolf;
   infile 'wolf.csv' dlm=',' dsd missover firstobs=2;
   input subregion wolf moose;
run;
*---part001e;
 
proc print data=wolf;
   title2 'raw data';
   sum wolf moose;
run;

/* Dot plot of the two gender's heights */
ods document name=plot1(write);
*---part002b;
proc sgplot data=wolf;
   title2 'plot to assess assumptions';
   scatter x=wolf y=moose;
run;
*---part002e;
ods document close;

*---part003b;
proc surveymeans data=wolf  ratio clm   N=200;
    title2 'Estimate of wolf to moose  ratio';
    /* ratio clm - request a ratio estimator with confidence intervals */
    /* N=200  specifies total number of units in the population */
    var moose wolf;
    ratio wolf/moose;  /* this statement ask for ratio estimator */
	ods output statistics=wolfresults
    ods output Ratio     =wolfratio;
*---part003e;
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


ods tagsets.mylatex file="wolf-SAS-data.tex" (notop nobot) /* newfile=table */;
proc print data=wolf;
run;
ods tagset.mylatex close;

goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='wolf-SAS-prelim' reset=index;
proc document name=plot1;
   replay \Sgplot#1\SGplot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mylatex file="wolf-SAS-results.tex" (notop nobot) /* newfile=table */;
proc print data=wolfresults noobs label split=" ";
   var varname mean stderr lowerclmean upperclmean ; 
   format mean lowerCLMean stderr upperCLmean 10.1;
   label LowerCLMean='LCL Mean';
   label UpperCLMean='UCL Mean';
   label stderr     ='SE Mean';
run;
ods tagsets.mylatex close;


ods tagsets.mylatex file="wolf-SAS-ratio.tex" (notop nobot) /* newfile=table */;
proc print data=wolfratio noobs label split=" ";
run;
ods tagsets.mylatex close;



