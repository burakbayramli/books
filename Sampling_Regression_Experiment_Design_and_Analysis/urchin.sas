/* Example of a cluster sample */
/* 2015-07-04 Updated for latest version of SAS*/
 
/* This dataset consists of the results from a series
   of transects conducted perpindicular to the shore.
   As divers swam along the transect, they counted
   the number of red sea urchins in 1 m**2 quadrats.

   The variables are (from left to right):
       transect, quadrat, legal size, sublegal sized.

   If a quadrat is not present in this listing, then the count was 0
   for both variables. It does NOT indicate that the quadrat
   was not measured - rather that no urchins were found.

   There was no transect numbered 5, 12, 17, or 19. */

title 'Estimating urchin density - example of cluster analysis';
 
ods pdf file='urchin-SAS.pdf' style=styles.printer;

*---part001b; 
data urchin;
   infile 'urchin.csv' dlm=',' dsd firstobs=2 missover;  /* the first record has the variable names */
   input transect quadrat legal sublegal;
   /* no need to specify sampling weights because transects are an SRS */
run;
*---part001e; 

/***** First check to see if any transects are missing quadrats *************/

*---part002b; 
proc sort data=urchin; by transect; run;
proc means data=urchin noprint;
   by transect;
   var quadrat legal;
   output out=check min=min max=max n=n sum(legal)=tlegal;
run;
*---part002e;
 
data check;
   set check;
   length problem $15.;
   problem = ' ';
   if max ^= n then problem = 'missing quadrat?';
   drop _type_ _freq_;
run;
 
proc print data=check;
   title2 'check to see if any transect is missing quadrats';
run;


ods document name=plot1(write);
*---part003b;
proc sgplot data=check;
   title2 'Plot the relationship between the cluster total and cluster size';
   scatter y=tlegal x=n / datalabel=transect;  /* use the transect number as the plotting character */
run;
*---part003e;
ods document close;


/****************************************************************************/

/* Now for the cluster analysis */

*---part004b;
proc surveymeans data=urchin;   /* do not specify a pop size as fpc is negligble */
   cluster transect;
   var     legal;
   ods output statistics=urchinresults; 
run;
*---part004e;
ods tagsets.tablesonlylatex close;

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


ods tagsets.mylatex file="urchin-SAS-data.tex" (notop nobot) /* newfile=table */;
proc print data=urchin(obs=10);
run;
ods tagset.mylatex close;


goptions device=png colors=(black) rotate=landscape;
ods graphics on / imagefmt=png imagename='urchin-SAS-prelim' reset=index;
proc document name=plot1;
   replay \Sgplot#1\SGplot#1 / dest=listing;
run;
ods graphics off;
ods listing close;


ods tagsets.mylatex file="urchin-SAS-results.tex" (notop nobot) /* newfile=table */;
proc print data=urchinresults noobs label split=" ";
   var varname mean stderr lowerclmean upperclmean ; 
   format mean lowerCLMean stderr upperCLmean 10.3;
   label LowerCLMean='LCL Mean';
   label UpperCLMean='UCL Mean';
   label stderr     ='SE Mean';
run;
ods tagsets.mylatex close;

